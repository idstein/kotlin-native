package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.gen.jvm.KotlinPlatform
import org.jetbrains.kotlin.native.interop.indexer.*

internal class MacroConstantStubBuilder(
        override val context: StubsBuildingContext,
        private val constant: ConstantDef
) : StubElementBuilder {
    override fun build(): List<StubElement> {
        val kotlinName = constant.name
        val declaration = when (constant) {
            is IntegerConstantDef -> {
                val literal = context.tryCreateIntegralStub(constant.type, constant.value) ?: return emptyList()
                val kotlinType = WrapperStubType(context.mirror(constant.type).argType)
                when (context.platform) {
                    KotlinPlatform.NATIVE -> PropertyStub(kotlinName, kotlinType, PropertyStub.Kind.Constant(literal))
                    // No reason to make it const val with backing field on Kotlin/JVM yet:
                    KotlinPlatform.JVM -> {
                        val getter = PropertyAccessor.Getter.SimpleGetter(constant = literal)
                        PropertyStub(kotlinName, kotlinType, PropertyStub.Kind.Val(getter))
                    }
                }
            }
            is FloatingConstantDef -> {
                val literal = context.tryCreateDoubleStub(constant.type, constant.value) ?: return emptyList()
                val kotlinType = WrapperStubType(context.mirror(constant.type).argType)
                val getter = PropertyAccessor.Getter.SimpleGetter(constant = literal)
                PropertyStub(kotlinName, kotlinType, PropertyStub.Kind.Val(getter))
            }
            is StringConstantDef -> {
                val literal = StringConstantStub(constant.value.quoteAsKotlinLiteral())
                val kotlinType = WrapperStubType(KotlinTypes.string)
                val getter = PropertyAccessor.Getter.SimpleGetter(constant = literal)
                PropertyStub(kotlinName, kotlinType, PropertyStub.Kind.Val(getter))
            }
            else -> return emptyList()
        }
        return listOf(declaration)
    }
}

internal class StructStubBuilder(
        override val context: StubsBuildingContext,
        private val decl: StructDecl
) : StubElementBuilder {
    override fun build(): List<StubElement> {
        val platform = context.platform
        val def = decl.def ?: return generateForwardStruct(decl)

        val structAnnotation: AnnotationStub? = if (platform == KotlinPlatform.JVM) {
            if (def.kind == StructDef.Kind.STRUCT && def.fieldsHaveDefaultAlignment()) {
                AnnotationStub.CNaturalStruct(def.members.joinToString { it.name.quoteAsKotlinLiteral() })
            } else {
                null
            }
        } else {
            tryRenderStructOrUnion(def)?.let {
                AnnotationStub.CStruct(it)
            }
        }
        val classifier = context.declarationMapper.getKotlinClassForPointed(decl)

        val fields: List<PropertyStub?> = def.fields.map { field ->
            try {
                assert(field.name.isNotEmpty())
                assert(field.offset % 8 == 0L)
                val offset = field.offset / 8
                val fieldRefType = context.mirror(field.type)
                val unwrappedFieldType = field.type.unwrapTypedefs()
                if (unwrappedFieldType is ArrayType) {
                    val type = (fieldRefType as TypeMirror.ByValue).valueType
                    val annotations = if (platform == KotlinPlatform.JVM) {
                        val length = getArrayLength(unwrappedFieldType)
                        // TODO: @CLength should probably be used on types instead of properties.
                        listOf(AnnotationStub.CLength(length))
                    } else {
                        emptyList()
                    }
                    val kind = PropertyStub.Kind.Val(PropertyAccessor.Getter.ArrayMemberAt(offset))
                    // TODO: Should receiver be added?
                    PropertyStub(field.name.asSimpleName(), WrapperStubType(type), kind, annotations = annotations)
                } else {
                    val pointedType = WrapperStubType(fieldRefType.pointedType)
                    val pointedTypeArgument = TypeArgument(pointedType)
                    if (fieldRefType is TypeMirror.ByValue) {
                        val kind = PropertyStub.Kind.Var(
                                PropertyAccessor.Getter.MemberAt(offset, typeArguments = listOf(pointedTypeArgument)),
                                PropertyAccessor.Setter.MemberAt(offset, typeArguments = listOf(pointedTypeArgument))
                        )
                        PropertyStub(field.name.asSimpleName(), WrapperStubType(fieldRefType.argType), kind)
                    } else {
                        val kind = PropertyStub.Kind.Val(PropertyAccessor.Getter.MemberAt(offset))
                        PropertyStub(field.name.asSimpleName(), pointedType, kind)
                    }
                }
            } catch (e: Throwable) {
                null
            }
        }

        val bitFields: List<PropertyStub> = def.bitFields.map { field ->
            val typeMirror = context.mirror(field.type)
            val typeInfo = typeMirror.info
            val kotlinType = typeMirror.argType
            val rawType = typeInfo.bridgedType
            val signed = field.type.isIntegerTypeSigned()
            // TODO: Type conversion?
            val kind = PropertyStub.Kind.Var(
                    PropertyAccessor.Getter.ReadBits(field.offset, field.size, signed, rawType),
                    PropertyAccessor.Setter.WriteBits(field.offset, field.size)
            )
            PropertyStub(field.name.asSimpleName(), WrapperStubType(kotlinType), kind)
        }

        val superClass = SymbolicStubType("CStructVar")
        val rawPtrConstructorParam = ConstructorParamStub("rawPtr", SymbolicStubType("NativePtr"))
        val superClassInit = SuperClassInit(superClass, listOf(rawPtrConstructorParam))

        // TODO: How we will differ Type and CStructVar.Type?
        val companionSuper = SymbolicStubType("Type")
        val companionSuperInit = SuperClassInit(companionSuper, listOf(IntegralConstantStub(def.size), IntegralConstantStub(def.align.toLong())))
        val companion = ClassStub.Companion(companionSuperInit)

        return listOf(ClassStub.Simple(
                classifier,
                origin = StubOrigin.Struct(decl),
                properties = fields.filterNotNull() + if (platform == KotlinPlatform.NATIVE) bitFields else emptyList(),
                functions = emptyList(),
                modality = ClassStubModality.NONE,
                annotations = listOfNotNull(structAnnotation),
                superClassInit = superClassInit,
                constructorParams = listOf(rawPtrConstructorParam),
                companion = companion
        ))
    }

    private fun getArrayLength(type: ArrayType): Long {
        val unwrappedElementType = type.elemType.unwrapTypedefs()
        val elementLength = if (unwrappedElementType is ArrayType) {
            getArrayLength(unwrappedElementType)
        } else {
            1L
        }

        val elementCount = when (type) {
            is ConstArrayType -> type.length
            is IncompleteArrayType -> 0L
            else -> TODO(type.toString())
        }

        return elementLength * elementCount
    }

    private tailrec fun Type.isIntegerTypeSigned(): Boolean = when (this) {
        is IntegerType -> this.isSigned
        is BoolType -> false
        is EnumType -> this.def.baseType.isIntegerTypeSigned()
        is Typedef -> this.def.aliased.isIntegerTypeSigned()
        else -> error(this)
    }

    /**
     * Produces to [out] the definition of Kotlin class representing the reference to given forward (incomplete) struct.
     */
    private fun generateForwardStruct(s: StructDecl): List<StubElement> = when (context.platform) {
        KotlinPlatform.JVM -> emptyList() // ("class ${s.kotlinName.asSimpleName()}(rawPtr: NativePtr) : COpaque(rawPtr)")
        KotlinPlatform.NATIVE -> emptyList()
    }
}

internal class EnumStubBuilder(
        override val context: StubsBuildingContext,
        private val enumDef: EnumDef
) : StubElementBuilder {
    override fun build(): List<StubElement> {
        if (!context.isStrictEnum(enumDef)) {
            return generateEnumAsConstants(enumDef)
        }

        val baseTypeMirror = context.mirror(enumDef.baseType)
        val baseType = WrapperStubType(baseTypeMirror.argType)

        val clazz = (context.mirror(EnumType(enumDef)) as TypeMirror.ByValue).valueType.classifier

        val enumVariants = enumDef.constants.map {
            val literal = context.tryCreateIntegralStub(enumDef.baseType, it.value)
                    ?: error("Cannot create enum value ${it.value} of type ${enumDef.baseType}")
            EnumEntryStub(it.name.asSimpleName(), literal)
        }

        val qualifier = ConstructorParamStub.Qualifier.VAL(overrides = true)

        val valueParamStub = ConstructorParamStub("value", baseType, qualifier)

        // TODO: It's an interface.
        val superClassInit = SuperClassInit(SymbolicStubType("CEnum"))


        val enum = ClassStub.Enum(clazz, enumVariants,
                origin = StubOrigin.Enum(enumDef),
                constructorParams = listOf(valueParamStub),
                superClassInit = superClassInit
        )
        context.bridgeComponentsBuilder.enumToTypeMirror[enum] = baseTypeMirror

        return listOf(enum)
    }

    /**
     * Produces to [out] the Kotlin definitions for given enum which shouldn't be represented as Kotlin enum.
     */
    private fun generateEnumAsConstants(e: EnumDef): List<StubElement> {
        // TODO: if this enum defines e.g. a type of struct field, then it should be generated inside the struct class
        //  to prevent name clashing

        val results = mutableListOf<StubElement>()

        val constants = e.constants.filter {
            // Macro "overrides" the original enum constant.
            it.name !in context.macroConstantsByName
        }

        val kotlinType: KotlinType

        val baseKotlinType = context.mirror(e.baseType).argType
        if (e.isAnonymous) {
            // TODO: Use StubContainer.
//            if (constants.isNotEmpty()) {
//                out("// ${e.spelling}:")
//            }

            kotlinType = baseKotlinType
        } else {
            val typeMirror = context.mirror(EnumType(e))
            if (typeMirror !is TypeMirror.ByValue) {
                error("unexpected enum type mirror: $typeMirror")
            }

            // Generate as typedef:
            val varTypeName = typeMirror.info.constructPointedType(typeMirror.valueType)
            val varTypeClassifier = typeMirror.pointedType
            val valueTypeClassifier = typeMirror.valueType
            results += TypealiasStub(WrapperStubType(varTypeClassifier), WrapperStubType(varTypeName))
            results += TypealiasStub(WrapperStubType(valueTypeClassifier), WrapperStubType(baseKotlinType))

            kotlinType = typeMirror.valueType
        }

        for (constant in constants) {
            val literal = context.tryCreateIntegralStub(e.baseType, constant.value) ?: continue
            val kind = PropertyStub.Kind.Constant(literal)
            results += PropertyStub(
                    constant.name,
                    WrapperStubType(kotlinType),
                    kind,
                    MemberStubModality.NONE,
                    null
            )
        }
        return results
    }
}

internal class FunctionStubBuilder(
        override val context: StubsBuildingContext,
        private val func: FunctionDecl
) : StubElementBuilder {
    override fun build(): List<StubElement> {
        val platform = context.platform
        val parameters = mutableListOf<FunctionParameterStub>()

        func.parameters.forEachIndexed { index, parameter ->
            val parameterName = parameter.name.let {
                if (it == null || it.isEmpty()) {
                    "arg$index"
                } else {
                    it.asSimpleName()
                }
            }

            val representAsValuesRef = representCFunctionParameterAsValuesRef(parameter.type)
            val origin = StubOrigin.FunctionParameter(parameter)
            parameters += when {
                representCFunctionParameterAsString(func, parameter.type) -> {
                    val annotations = when (platform) {
                        KotlinPlatform.JVM -> emptyList()
                        KotlinPlatform.NATIVE -> listOf(AnnotationStub.CCall.CString)
                    }
                    val type = WrapperStubType(KotlinTypes.string.makeNullable())
                    FunctionParameterStub(parameterName, type, annotations, origin = origin)
                }
                representCFunctionParameterAsWString(func, parameter.type) -> {
                    val annotations = when (platform) {
                        KotlinPlatform.JVM -> emptyList()
                        KotlinPlatform.NATIVE -> listOf(AnnotationStub.CCall.WCString)
                    }
                    val type = WrapperStubType(KotlinTypes.string.makeNullable())
                    FunctionParameterStub(parameterName, type, annotations, origin = origin)
                }
                representAsValuesRef != null -> {
                    FunctionParameterStub(parameterName, WrapperStubType(representAsValuesRef), origin = origin)
                }
                else -> {
                    val mirror = context.mirror(parameter.type)
                    val type = WrapperStubType(mirror.argType)
                    FunctionParameterStub(parameterName, type, origin = origin)
                }
            }
        }

        val returnType = WrapperStubType(if (func.returnsVoid()) {
            KotlinTypes.unit
        } else {
            context.mirror(func.returnType).argType
        })


        val annotations: List<AnnotationStub>
        val mustBeExternal: Boolean
        if (!func.isVararg || platform != KotlinPlatform.NATIVE) {
            annotations = emptyList()
            mustBeExternal = false
        } else {
            val type = WrapperStubType(KotlinTypes.any.makeNullable())
            parameters += FunctionParameterStub("variadicArguments", type, isVararg = true)
            annotations = listOf(AnnotationStub.CCall.Symbol(context.generateNextUniqueId("knifunptr_")))
            mustBeExternal = true
        }
        val functionStub = FunctionStub(
                func.name.asSimpleName(),
                returnType,
                parameters,
                StubOrigin.Function(func),
                annotations,
                mustBeExternal,
                null,
                MemberStubModality.NONE
        )
        return listOf(functionStub)
    }


    private fun FunctionDecl.returnsVoid(): Boolean = this.returnType.unwrapTypedefs() is VoidType

    private fun representCFunctionParameterAsValuesRef(type: Type): KotlinType? {
        val pointeeType = when (type) {
            is PointerType -> type.pointeeType
            is ArrayType -> type.elemType
            else -> return null
        }

        val unwrappedPointeeType = pointeeType.unwrapTypedefs()

        if (unwrappedPointeeType is VoidType) {
            // Represent `void*` as `CValuesRef<*>?`:
            return KotlinTypes.cValuesRef.typeWith(StarProjection).makeNullable()
        }

        if (unwrappedPointeeType is FunctionType) {
            // Don't represent function pointer as `CValuesRef<T>?` currently:
            return null
        }

        if (unwrappedPointeeType is ArrayType) {
            return representCFunctionParameterAsValuesRef(pointeeType)
        }


        return KotlinTypes.cValuesRef.typeWith(context.mirror(pointeeType).pointedType).makeNullable()
    }


    private val platformWStringTypes = setOf("LPCWSTR")

    private val noStringConversion: Set<String>
        get() = context.configuration.noStringConversion

    private fun Type.isAliasOf(names: Set<String>): Boolean {
        var type = this
        while (type is Typedef) {
            if (names.contains(type.def.name)) return true
            type = type.def.aliased
        }
        return false
    }

    private fun representCFunctionParameterAsString(function: FunctionDecl, type: Type): Boolean {
        val unwrappedType = type.unwrapTypedefs()
        return unwrappedType is PointerType && unwrappedType.pointeeIsConst &&
                unwrappedType.pointeeType.unwrapTypedefs() == CharType &&
                !noStringConversion.contains(function.name)
    }

    // We take this approach as generic 'const short*' shall not be used as String.
    private fun representCFunctionParameterAsWString(function: FunctionDecl, type: Type) = type.isAliasOf(platformWStringTypes)
            && !noStringConversion.contains(function.name)
}

internal class GlobalStubBuilder(
        override val context: StubsBuildingContext,
        private val global: GlobalDecl
) : StubElementBuilder {
    override fun build(): List<StubElement> {
        val mirror = context.mirror(global.type)
        val unwrappedType = global.type.unwrapTypedefs()

        val kotlinType: KotlinType
        val kind: PropertyStub.Kind
        if (unwrappedType is ArrayType) {
            kotlinType = (mirror as TypeMirror.ByValue).valueType
            val getter = PropertyAccessor.Getter.SimpleGetter()
            val extra = BridgeGenerationComponents.GlobalGetterBridgeInfo(global.name, mirror.info, isArray = true)
            context.bridgeComponentsBuilder.getterToBridgeInfo[getter] = extra
            kind = PropertyStub.Kind.Val(getter)
        } else {
            when (mirror) {
                is TypeMirror.ByValue -> {
                    kotlinType = mirror.argType
                    val getter = PropertyAccessor.Getter.SimpleGetter()
                    val getterExtra = BridgeGenerationComponents.GlobalGetterBridgeInfo(global.name, mirror.info, isArray = false)
                    context.bridgeComponentsBuilder.getterToBridgeInfo[getter] = getterExtra
                    kind = if (global.isConst) {
                        PropertyStub.Kind.Val(getter)
                    } else {
                        val setter = PropertyAccessor.Setter.SimpleSetter()
                        val setterExtra = BridgeGenerationComponents.GlobalSetterBridgeInfo(global.name, mirror.info)
                        context.bridgeComponentsBuilder.setterToBridgeInfo[setter] = setterExtra
                        PropertyStub.Kind.Var(getter, setter)
                    }
                }
                is TypeMirror.ByRef -> {
                    kotlinType = mirror.pointedType
                    val getter = PropertyAccessor.Getter.InterpretPointed(global.name, WrapperStubType(kotlinType))
                    kind = PropertyStub.Kind.Val(getter)
                }
            }
        }
        return listOf(PropertyStub(global.name, WrapperStubType(kotlinType), kind))
    }
}

internal class TypedefStubBuilder(
        override val context: StubsBuildingContext,
        private val typedefDef: TypedefDef
) : StubElementBuilder {
    override fun build(): List<StubElement> {
        val mirror = context.mirror(Typedef(typedefDef))
        val baseMirror = context.mirror(typedefDef.aliased)

        val varType = mirror.pointedType
        return when (baseMirror) {
            is TypeMirror.ByValue -> {
                val valueType = (mirror as TypeMirror.ByValue).valueType
                val varTypeAliasee = mirror.info.constructPointedType(valueType)
                val valueTypeAliasee = baseMirror.valueType
                listOf(
                        TypealiasStub(WrapperStubType(varType), WrapperStubType(varTypeAliasee)),
                        TypealiasStub(WrapperStubType(valueType), WrapperStubType(valueTypeAliasee))
                )
            }
            is TypeMirror.ByRef -> {
                val varTypeAliasee = baseMirror.pointedType
                listOf(TypealiasStub(WrapperStubType(varType), WrapperStubType(varTypeAliasee)))
            }
        }
    }
}

private class ObjCMethodStubBuilder(
        private val method: ObjCMethod,
        private val container: ObjCContainer,
        private val isDesignatedInitializer: Boolean,
        override val context: StubsBuildingContext
) : StubElementBuilder {
    private val isStret: Boolean
    private val stubReturnType: StubType
    val annotations: List<AnnotationStub>
    private val parameters: List<FunctionParameterStub>
    private val external: Boolean
    private val receiverType: StubType?
    private val name: String = method.kotlinName
    private val origin = StubOrigin.ObjCMethod(method, container)
    private val modality: MemberStubModality

    init {
        val returnType = method.getReturnType(container.classOrProtocol)
        isStret = returnType.isStret(context.configuration.target)
        stubReturnType = if (returnType.unwrapTypedefs() is VoidType) {
            WrapperStubType(KotlinTypes.unit)
        } else {
            WrapperStubType(context.mirror(returnType).argType)
        }
        val methodAnnotation = AnnotationStub.ObjC.Method(
                method.selector.quoteAsKotlinLiteral(),
                method.encoding.quoteAsKotlinLiteral(),
                isStret
        )
        annotations = buildObjCMethodAnnotations(methodAnnotation)
        parameters = method.getParameterStubs(context, forConstructorOrFactory = false)
        external = (container !is ObjCProtocol)
        modality = when (container) {
            is ObjCClassOrProtocol -> {
                if (method.isOverride(container)) {
                    MemberStubModality.OVERRIDE
                } else when (container) {
                    is ObjCClass -> MemberStubModality.OPEN
                    is ObjCProtocol -> MemberStubModality.NONE
                }
            }
            is ObjCCategory -> MemberStubModality.NONE
        }
        receiverType = if (container is ObjCCategory)
            WrapperStubType(context.declarationMapper.getKotlinClassFor(container.clazz, isMeta = method.isClass).type)
        else null
    }

    private fun buildObjCMethodAnnotations(main: AnnotationStub): List<AnnotationStub> = listOfNotNull(
            main,
            AnnotationStub.ObjC.ConsumesReceiver.takeIf { method.nsConsumesSelf },
            AnnotationStub.ObjC.ReturnsRetained.takeIf { method.nsReturnsRetained }
    )

    override fun build(): List<FunctionalStub> {
        val results = mutableListOf<FunctionalStub>()

        results += FunctionStub(name, stubReturnType, parameters, origin, annotations, external, receiverType, modality)

        if (method.isInit) {
            val parameters = method.getParameterStubs(context, forConstructorOrFactory = true)
            when (container) {
                // TODO: should `deprecatedInit` be added?
                is ObjCClass -> {
                    // TODO: consider generating non-designated initializers as factories.
                    val designated = isDesignatedInitializer ||
                            context.configuration.disableDesignatedInitializerChecks

                    val annotations = listOf(AnnotationStub.ObjC.Constructor(method.selector.quoteAsKotlinLiteral(), designated))
                    val constructor = ConstructorStub(parameters, annotations)
                    results += constructor
                }
                is ObjCCategory -> {
                    assert(!method.isClass)
                    val clazz= context.declarationMapper
                            .getKotlinClassFor(container.clazz, isMeta = false).type

                    val factoryAnnotation = AnnotationStub.ObjC.Factory(
                            method.selector.quoteAsKotlinLiteral(),
                            method.encoding.quoteAsKotlinLiteral(),
                            isStret
                    )
                    val annotations = buildObjCMethodAnnotations(factoryAnnotation)

                    val originalReturnType = method.getReturnType(container.clazz)
                    val typeParameter = TypeParameterStub("T", WrapperStubType(clazz))
                    val returnType = if (originalReturnType is ObjCPointer) {
                        typeParameter.getStubType(originalReturnType.isNullable)
                    } else {
                        // This shouldn't happen actually.
                        this.stubReturnType
                    }
                    val receiverType = SymbolicStubType(KotlinTypes.objCClassOf, listOf(typeParameter))
                    val createMethod = FunctionStub(
                            "create",
                            returnType,
                            parameters,
                            receiverType = receiverType,
                            typeParameters = listOf(typeParameter),
                            external = true,
                            origin = StubOrigin.None,
                            annotations = annotations,
                            modality = MemberStubModality.NONE
                    )
                    results += createMethod
                }
                is ObjCProtocol -> {}
            }
        }
        return results
    }
}

private fun ObjCMethod.getParameterStubs(
        stubIrBuilder: StubsBuildingContext,
        forConstructorOrFactory: Boolean
): List<FunctionParameterStub> {
    val names = getKotlinParameterNames(forConstructorOrFactory) // TODO: consider refactoring.
    val result = mutableListOf<FunctionParameterStub>()

    this.parameters.mapIndexedTo(result) { index, it ->
        val kotlinType = stubIrBuilder.mirror(it.type).argType
        val name = names[index]
        val annotations = if (it.nsConsumed) listOf(AnnotationStub.ObjC.Consumed) else emptyList()
        FunctionParameterStub(name, WrapperStubType(kotlinType), isVararg = false, annotations = annotations)
    }
    if (this.isVariadic) {
        result += FunctionParameterStub(
                names.last(),
                WrapperStubType(KotlinTypes.any.makeNullable()),
                isVararg = true,
                annotations = emptyList()
        )
    }
    return result
}

internal abstract class ObjCContainerStubBuilder(
        final override val context: StubsBuildingContext,
        private val container: ObjCClassOrProtocol,
        protected val metaContainerStub: ObjCContainerStubBuilder?
) : StubElementBuilder {
    private val isMeta: Boolean get() = metaContainerStub == null

    private val designatedInitializerSelectors = if (container is ObjCClass && !isMeta) {
        container.getDesignatedInitializerSelectors(mutableSetOf())
    } else {
        emptySet()
    }

    private val methods: List<ObjCMethod>
    private val properties: List<ObjCProperty>

    private val protocolGetter: String?

    init {
        val superMethods = container.inheritedMethods(isMeta)

        // Add all methods declared in the class or protocol:
        var methods = container.declaredMethods(isMeta)

        // Exclude those which are identically declared in super types:
        methods -= superMethods

        // Add some special methods from super types:
        methods += superMethods.filter { it.returnsInstancetype() || it.isInit }

        // Add methods inherited from multiple supertypes that must be defined according to Kotlin rules:
        methods += container.immediateSuperTypes
                .flatMap { superType ->
                    val methodsWithInherited = superType.methodsWithInherited(isMeta).inheritedTo(container, isMeta)
                    // Select only those which are represented as non-abstract in Kotlin:
                    when (superType) {
                        is ObjCClass -> methodsWithInherited
                        is ObjCProtocol -> methodsWithInherited.filter { it.isOptional }
                    }
                }
                .groupBy { it.selector }
                .mapNotNull { (_, inheritedMethods) -> if (inheritedMethods.size > 1) inheritedMethods.first() else null }

        this.methods = methods.distinctBy { it.selector }.toList()

        this.properties = container.properties.filter { property ->
            property.getter.isClass == isMeta &&
                    // Select only properties that don't override anything:
                    superMethods.none { property.getter.replaces(it) || property.setter?.replaces(it) ?: false }
        }
    }

    private val methodToStub = methods.map {
        it to ObjCMethodStubBuilder(it, container, it.selector in designatedInitializerSelectors, context)
    }.toMap()

    private val propertyBuilders = properties.mapNotNull {
        createObjCPropertyBuilder(context, it, container, this.methodToStub)
    }

    val modality = when (container) {
        is ObjCClass -> ClassStubModality.OPEN
        is ObjCProtocol -> ClassStubModality.INTERFACE
    }

    val classifier = context.declarationMapper.getKotlinClassFor(container, isMeta)

    val externalObjCAnnotation = when (container) {
        is ObjCProtocol -> {
            protocolGetter = if (metaContainerStub != null) {
                metaContainerStub.protocolGetter!!
            } else {
                // TODO: handle the case when protocol getter stub can't be compiled.
                context.generateNextUniqueId("kniprot_")
            }
            AnnotationStub.ObjC.ExternalClass(protocolGetter)
        }
        is ObjCClass -> {
            protocolGetter = null
            val binaryName = container.binaryName
            AnnotationStub.ObjC.ExternalClass("", binaryName ?: "")
        }
    }

    val interfaces: List<StubType> by lazy {
        val interfaces = mutableListOf<StubType>()
        if (container is ObjCClass) {
            val baseClass = container.baseClass
            val baseClassifier = if (baseClass != null) {
                context.declarationMapper.getKotlinClassFor(baseClass, isMeta)
            } else {
                if (isMeta) KotlinTypes.objCObjectBaseMeta else KotlinTypes.objCObjectBase
            }
            interfaces += WrapperStubType(baseClassifier.type)
        }
        container.protocols.forEach {
            interfaces += WrapperStubType(context.declarationMapper.getKotlinClassFor(it, isMeta).type)
        }
        if (interfaces.isEmpty()) {
            assert(container is ObjCProtocol)
            val classifier = if (isMeta) KotlinTypes.objCObjectMeta else KotlinTypes.objCObject
            interfaces += WrapperStubType(classifier.type)
        }
        if (!isMeta && container.isProtocolClass()) {
            // TODO: map Protocol type to ObjCProtocol instead.
            interfaces += WrapperStubType(KotlinTypes.objCProtocol.type)
        }
        interfaces
    }

    protected fun buildBody(): Pair<List<PropertyStub>, List<FunctionalStub>> {
        // TODO: add protected constructor if needed
        return Pair(
                propertyBuilders.flatMap { it.build() },
                methodToStub.values.flatMap { it.build() }
        )
    }
}

internal sealed class ObjCClassOrProtocolStubBuilder(
        context: StubsBuildingContext,
        private val container: ObjCClassOrProtocol
) : ObjCContainerStubBuilder(
        context,
        container,
        metaContainerStub = object : ObjCContainerStubBuilder(context, container, metaContainerStub = null) {

            override fun build(): List<StubElement> {
                val (properties, methods) = buildBody()
                val classStub = ClassStub.Simple(
                        super.classifier,
                        properties = properties,
                        functions = methods,
                        origin = StubOrigin.None,
                        modality = super.modality
                )
                return listOf(classStub)
            }
        }
)

internal class ObjCProtocolStubBuilder(
        context: StubsBuildingContext,
        private val protocol: ObjCProtocol
) : ObjCClassOrProtocolStubBuilder(context, protocol), StubElementBuilder {
    override fun build(): List<StubElement> {
        val (properties, methods) = buildBody()
        val classStub = ClassStub.Simple(
                super.classifier,
                properties = properties,
                functions = methods,
                origin = StubOrigin.ObjCProtocol(protocol),
                modality = super.modality
        )
        return listOf(classStub, *metaContainerStub!!.build().toTypedArray())
    }
}

internal class ObjCClassStubBuilder(
        context: StubsBuildingContext,
        private val clazz: ObjCClass
) : ObjCClassOrProtocolStubBuilder(context, clazz), StubElementBuilder {
    override fun build(): List<StubElement> {
        val companionSuper = context.declarationMapper
                .getKotlinClassFor(clazz, isMeta = true)
                .let { SymbolicStubType(it) }

        val objCClassType = KotlinTypes.objCClassOf.typeWith(
                context.declarationMapper.getKotlinClassFor(clazz, isMeta = false).type
        ).let { WrapperStubType(it) }

        val superClassInit = SuperClassInit(companionSuper)
        val companion = ClassStub.Companion(superClassInit, listOf(objCClassType))

        val (properties, methods) = buildBody()

        val classStub = ClassStub.Simple(
                super.classifier,
                origin = StubOrigin.ObjCClass(clazz),
                properties = properties,
                functions = methods,
                modality = super.modality,
                companion = companion
        )
        return listOf(classStub)
    }
}

internal class ObjCCategoryStubBuilder(
        override val context: StubsBuildingContext,
        private val category: ObjCCategory
) : StubElementBuilder {
    private val generatedMembers = context.generatedObjCCategoriesMembers
            .getOrPut(category.clazz, { GeneratedObjCCategoriesMembers() })

    private val methodToBuilder = category.methods.filter { generatedMembers.register(it) }.map {
        it to ObjCMethodStubBuilder(it, category, isDesignatedInitializer = false, context = context)
    }.toMap()

    private val methodBuilders get() = methodToBuilder.values

    private val propertyBuilders = category.properties.filter { generatedMembers.register(it) }.mapNotNull {
        createObjCPropertyBuilder(context, it, category, methodToBuilder)
    }

    override fun build(): List<StubElement> {
        val description = "${category.clazz.name} (${category.name})"
        val startText = "// @interface $description"
        val endText = "// @end; // $description"
        val container = SimpleStubContainer(
                meta = StubContainerMeta(startText, endText),
                functions = methodBuilders.flatMap { it.build() },
                properties = propertyBuilders.flatMap { it.build() }
        )
        return listOf(container)
    }
}

private fun createObjCPropertyBuilder(
        context: StubsBuildingContext,
        property: ObjCProperty,
        container: ObjCContainer,
        methodToStub: Map<ObjCMethod, ObjCMethodStubBuilder>
): ObjCPropertyStubBuilder? {
    // Note: the code below assumes that if the property is generated,
    // then its accessors are also generated as explicit methods.
    val getterStub = methodToStub[property.getter] ?: return null
    val setterStub = property.setter?.let { methodToStub[it] ?: return null }
    return ObjCPropertyStubBuilder(context, property, container, getterStub, setterStub)
}

private class ObjCPropertyStubBuilder(
        override val context: StubsBuildingContext,
        private val property: ObjCProperty,
        private val container: ObjCContainer,
        private val getterBuilder: ObjCMethodStubBuilder,
        private val setterMethod: ObjCMethodStubBuilder?
) : StubElementBuilder {
    override fun build(): List<PropertyStub> {
        val type = property.getType(container.classOrProtocol)
        val kotlinType = context.mirror(type).argType
        val getter = PropertyAccessor.Getter.ExternalGetter(annotations = getterBuilder.annotations)
        val setter = property.setter?.let { PropertyAccessor.Setter.ExternalSetter(annotations = setterMethod!!.annotations) }
        val kind = setter?.let { PropertyStub.Kind.Var(getter, it) } ?: PropertyStub.Kind.Val(getter)
        val modality = if (container is ObjCProtocol) MemberStubModality.FINAL else MemberStubModality.NONE
        val receiver = when (container) {
            is ObjCClassOrProtocol -> null
            is ObjCCategory -> context.declarationMapper
                    .getKotlinClassFor(container.clazz, isMeta = property.getter.isClass)
                    .let { SymbolicStubType(it) }
        }
        return listOf(PropertyStub(property.name, WrapperStubType(kotlinType), kind, modality, receiver))
    }
}