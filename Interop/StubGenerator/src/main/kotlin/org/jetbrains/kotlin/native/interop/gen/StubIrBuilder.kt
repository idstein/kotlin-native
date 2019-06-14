package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.gen.jvm.InteropConfiguration
import org.jetbrains.kotlin.native.interop.gen.jvm.KotlinPlatform
import org.jetbrains.kotlin.native.interop.indexer.*

// TODO: Should it implement [StubContainer]?
class TopLevelContainer(
        override val classes: List<ClassStub>,
        override val functions: List<FunctionalStub>,
        override val properties: List<PropertyStub>,
        override val typealiases: List<TypealiasStub>,
        override val meta: StubContainerMeta,
        val declarationMapper: DeclarationMapper
) : StubContainer {
    override fun accept(visitor: StubIrVisitor) {
        classes.forEach { it.accept(visitor) }
        functions.forEach { it.accept(visitor) }
        properties.forEach { it.accept(visitor) }
        typealiases.forEach { it.accept(visitor) }
    }
}

class StubIrBuilder(
        val configuration: InteropConfiguration,
        val verbose: Boolean = false,
        val platform: KotlinPlatform,
        private val nativeIndex: NativeIndex,
        val imports: Imports
) {

    private val classes = mutableListOf<ClassStub>()
    private val functions = mutableListOf<FunctionalStub>()
    private val globals = mutableListOf<PropertyStub>()
    private val typealiases = mutableListOf<TypealiasStub>()

    private val platformWStringTypes = setOf("LPCWSTR")

    private val noStringConversion: Set<String>
        get() = configuration.noStringConversion

    private fun Type.isAliasOf(names: Set<String>): Boolean {
        var type = this
        while (type is Typedef) {
            if (names.contains(type.def.name)) return true
            type = type.def.aliased
        }
        return false
    }

    // TODO: Use common logging system.
    private fun log(message: String) {
        if (verbose) {
            println(message)
        }
    }

    fun mirror(type: Type): TypeMirror = mirror(declarationMapper, type)

    val declarationMapper = object : DeclarationMapper {
        override fun getKotlinClassForPointed(structDecl: StructDecl): Classifier {
            val baseName = structDecl.kotlinName
            val pkg = when (platform) {
                KotlinPlatform.JVM -> pkgName
                KotlinPlatform.NATIVE -> if (structDecl.def == null) {
                    cnamesStructsPackageName // to be imported as forward declaration.
                } else {
                    getPackageFor(structDecl)
                }
            }
            return Classifier.topLevel(pkg, baseName)
        }

        override fun isMappedToStrict(enumDef: EnumDef): Boolean = enumDef.isStrictEnum

        override fun getKotlinNameForValue(enumDef: EnumDef): String = enumDef.kotlinName

        override fun getPackageFor(declaration: TypeDeclaration): String {
            return imports.getPackage(declaration.location) ?: pkgName
        }

        override val useUnsignedTypes: Boolean
            get() = when (platform) {
                KotlinPlatform.JVM -> false
                KotlinPlatform.NATIVE -> true
            }
    }

    private var theCounter = 0
    fun nextUniqueId() = theCounter++

    private val macroConstantsByName = (nativeIndex.macroConstants + nativeIndex.wrappedMacros).associateBy { it.name }

    val generatedObjCCategoriesMembers = mutableMapOf<ObjCClass, GeneratedObjCCategoriesMembers>()

    fun build(): TopLevelContainer {
        nativeIndex.objCProtocols.forEach { generateStubsForObjCProtocol(it) }
        nativeIndex.objCClasses.forEach { generateStubsForObjCClass(it) }
        nativeIndex.objCCategories.forEach { generateStubsForObjCCategory(it) }
        nativeIndex.typedefs.forEach { generateStubsForTypedef(it) }
        nativeIndex.globals.forEach { generateStubsForGlobal(it) }
        nativeIndex.enums.forEach { generateStubsForEnum(it) }
        nativeIndex.structs.forEach { generateStubsForStruct(it) }
        nativeIndex.functions.forEach { generateStubsForFunction(it) }

        val meta = StubContainerMeta()
        return TopLevelContainer(classes, functions, globals, typealiases, meta, declarationMapper)
    }

    private fun generateStubsForEnum(enumDef: EnumDef) {
        if (!enumDef.isStrictEnum) {
            generateEnumAsConstants(enumDef)
            return
        }

        val baseTypeMirror = mirror(enumDef.baseType)
        val baseKotlinType = baseTypeMirror.argType

        val canonicalsByValue = enumDef.constants
                .groupingBy { it.value }
                .reduce { _, accumulator, element ->
                    if (element.isMoreCanonicalThan(accumulator)) {
                        element
                    } else {
                        accumulator
                    }
                }

        val (canonicalConstants, aliasConstants) = enumDef.constants.partition { canonicalsByValue[it.value] == it }

        val clazz = (mirror(EnumType(enumDef)) as TypeMirror.ByValue).valueType.classifier

        val enumVariants = canonicalConstants.map {
            val literal = tryCreateIntegralStub(enumDef.baseType, it.value)
                    ?: error("Cannot create enum value ${it.value} of type ${enumDef.baseType}")
            EnumVariantStub(it.name.asSimpleName(), literal)
        }

        val valueParamStub = ConstructorParamStub("value", WrapperStubType(baseKotlinType), ConstructorParamStub.Qualifier.VAL(true))

        val superClassInit = SuperClassInit(SymbolicStubType("CEnum"))

        val companionStub = run {
            val properties = aliasConstants.forEach {
                val mainConstant = canonicalsByValue[it.value]!!
//                out("val ${it.name.asSimpleName()} = ${mainConstant.name.asSimpleName()}")
//                PropertyStub(it.name.asSimpleName(), WrapperStubType)
            }
            //out("fun byValue(value: $baseKotlinType) = " +
            //                        "${enumDef.kotlinName.asSimpleName()}.values().find { it.value == value }!!")
            // TODO: Fill companion object.
            ClassStub.Companion()
        }
//        val varClass = run {
//            val cEnumVarClass = SymbolicStubType("CEnumVar")
//            val nativePtrType = SymbolicStubType("NativePtr")
//            val rawPtr = ConstructorParamStub("rawPtr", nativePtrType)
//            val superClassInit = SuperClassInit(cEnumVarClass, listOf(rawPtr))
//            val varClassCompanion = run {
//                val superClassInit = SuperClassInit("Type", listOf(IntegralConstantStub(base)))
//                CompanionStub()
//            }
//            SimpleClassStub()
//        }
//
//        block("enum class ${kotlinFile.declare(clazz)}(override val value: $baseKotlinType) : CEnum") {
//            block("class Var(rawPtr: NativePtr) : CEnumVar(rawPtr)") {
//                val basePointedTypeName = baseTypeMirror.pointedType.render(kotlinFile)
//                out("companion object : Type($basePointedTypeName.size.toInt())")
//                out("var value: ${enumDef.kotlinName.asSimpleName()}")
//                out("    get() = byValue(this.reinterpret<$basePointedTypeName>().value)")
//                out("    set(value) { this.reinterpret<$basePointedTypeName>().value = value.value }")
//            }
//        }
        classes += ClassStub.Enum(clazz, enumVariants,
                origin = StubOrigin.Enum(enumDef),
                companion = companionStub,
                constructorParams = listOf(valueParamStub),
                superClassInit = superClassInit
        )
    }

    /**
     * Produces to [out] the Kotlin definitions for given enum which shouldn't be represented as Kotlin enum.
     *
     * @see isStrictEnum
     */
    private fun generateEnumAsConstants(e: EnumDef) {
        // TODO: if this enum defines e.g. a type of struct field, then it should be generated inside the struct class
        // to prevent name clashing

        val constants = e.constants.filter {
            // Macro "overrides" the original enum constant.
            it.name !in macroConstantsByName
        }

        val kotlinType: KotlinType

        val baseKotlinType = mirror(e.baseType).argType
        if (e.isAnonymous) {
            // TODO: Use StubContainer.
//            if (constants.isNotEmpty()) {
//                out("// ${e.spelling}:")
//            }

            kotlinType = baseKotlinType
        } else {
            val typeMirror = mirror(EnumType(e))
            if (typeMirror !is TypeMirror.ByValue) {
                error("unexpected enum type mirror: $typeMirror")
            }

            // Generate as typedef:
            val varTypeName = typeMirror.info.constructPointedType(typeMirror.valueType)
            val varTypeClassifier = typeMirror.pointedType
            val valueTypeClassifier = typeMirror.valueType
            typealiases += TypealiasStub(WrapperStubType(varTypeClassifier), WrapperStubType(varTypeName))
            typealiases += TypealiasStub(WrapperStubType(valueTypeClassifier), WrapperStubType(baseKotlinType))

            kotlinType = typeMirror.valueType
        }

        for (constant in constants) {
            val literal = tryCreateIntegralStub(e.baseType, constant.value) ?: continue
            val kind = PropertyStub.Kind.Constant(literal)
            globals += PropertyStub(
                    constant.name,
                    WrapperStubType(kotlinType),
                    kind,
                    MemberStubModality.NONE,
                    null
            )
        }
    }

    private fun generateStubsForFunction(func: FunctionDecl) {
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

            parameters += when {
                representCFunctionParameterAsString(func, parameter.type) -> {
                    val annotations = when (platform) {
                        KotlinPlatform.JVM -> emptyList()
                        KotlinPlatform.NATIVE -> listOf(AnnotationStub.CCall.CString)
                    }
                    val type = WrapperStubType(KotlinTypes.string.makeNullable())
                    FunctionParameterStub(parameterName, type, annotations)
                }
                representCFunctionParameterAsWString(func, parameter.type) -> {
                    val annotations = when (platform) {
                        KotlinPlatform.JVM -> emptyList()
                        KotlinPlatform.NATIVE -> listOf(AnnotationStub.CCall.WCString)
                    }
                    val type = WrapperStubType(KotlinTypes.string.makeNullable())
                    FunctionParameterStub(parameterName, type, annotations)
                }
                representAsValuesRef != null -> {
                    FunctionParameterStub(parameterName, WrapperStubType(representAsValuesRef))
                }
                else -> {
                    val mirror = mirror(parameter.type)
                    val type = WrapperStubType(mirror.argType)
                    FunctionParameterStub(parameterName, type)
                }
            }
        }

        val returnType = WrapperStubType(if (func.returnsVoid()) {
            KotlinTypes.unit
        } else {
            mirror(func.returnType).argType
        })


        val annotations: List<AnnotationStub>
        val mustBeExternal: Boolean
        if (!func.isVararg || platform != KotlinPlatform.NATIVE) {
            annotations = emptyList()
            mustBeExternal = false
        } else {
            val type = WrapperStubType(KotlinTypes.any.makeNullable())
            parameters += FunctionParameterStub("variadicArguments", type, isVararg = true)
            annotations = listOf(AnnotationStub.CCall.Symbol("knifunptr_" + pkgName.replace('.', '_') + nextUniqueId()))
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
        functions += functionStub
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


        return KotlinTypes.cValuesRef.typeWith(mirror(pointeeType).pointedType).makeNullable()
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

    private fun generateStubsForStruct(decl: StructDecl) {
        val def = decl.def
        if (def == null) {
            generateForwardStruct(decl)
            return
        }

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
        val classifier = declarationMapper.getKotlinClassForPointed(decl)

        val fields: List<PropertyStub?> = def.fields.map { field ->
            try {
                assert(field.name.isNotEmpty())
                assert(field.offset % 8 == 0L)
                val offset = field.offset / 8
                val fieldRefType = mirror(field.type)
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
                    if (fieldRefType is TypeMirror.ByValue) {
                        val kind = PropertyStub.Kind.Var(
                                PropertyAccessor.Getter.MemberAt(offset, typeParameters = listOf(pointedType)),
                                PropertyAccessor.Setter.MemberAt(offset, typeParameters = listOf(pointedType))
                        )
                        PropertyStub(field.name.asSimpleName(), WrapperStubType(fieldRefType.argType), kind)
                    } else {
                        val kind = PropertyStub.Kind.Val(PropertyAccessor.Getter.MemberAt(offset))
                        PropertyStub(field.name.asSimpleName(), pointedType, kind)
                    }
                }
            } catch (e: Throwable) {
                log("Warning: cannot generate definition for field ${decl.kotlinName}.${field.name}")
                null
            }
        }

        val bitFields: List<PropertyStub> = def.bitFields.map { field ->
            val typeMirror = mirror(field.type)
            val typeInfo = typeMirror.info
            val kotlinType = typeMirror.argType
            val rawType = typeInfo.bridgedType
            val signed = field.type.isIntegerTypeSigned()
            // TODO: Type conversion?
            val kind = PropertyStub.Kind.Var(
                    PropertyAccessor.Getter.ReadBits(field.offset, field.size, signed),
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

        ClassStub.Simple(
                classifier,
                origin = StubOrigin.Struct(decl),
                properties = fields.filterNotNull() + bitFields,
                functions = emptyList(),
                modality = ClassStubModality.NONE,
                annotations = listOfNotNull(structAnnotation),
                superClassInit = superClassInit,
                constructorParams = listOf(rawPtrConstructorParam),
                companion = companion
        )
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
    private fun generateForwardStruct(s: StructDecl) = when (platform) {
        KotlinPlatform.JVM -> ("class ${s.kotlinName.asSimpleName()}(rawPtr: NativePtr) : COpaque(rawPtr)")
        KotlinPlatform.NATIVE -> {}
    }

    private fun generateStubsForTypedef(typedefDef: TypedefDef) {
        val mirror = mirror(Typedef(typedefDef))
        val baseMirror = mirror(typedefDef.aliased)

        val varType = mirror.pointedType
        when (baseMirror) {
            is TypeMirror.ByValue -> {
                val valueType = (mirror as TypeMirror.ByValue).valueType
                val varTypeAliasee = mirror.info.constructPointedType(valueType)
                val valueTypeAliasee = baseMirror.valueType
                this.typealiases += TypealiasStub(WrapperStubType(varType), WrapperStubType(varTypeAliasee))
                this.typealiases += TypealiasStub(WrapperStubType(valueType), WrapperStubType(valueTypeAliasee))
            }
            is TypeMirror.ByRef -> {
                val varTypeAliasee = baseMirror.pointedType
                this.typealiases += TypealiasStub(WrapperStubType(varType), WrapperStubType(varTypeAliasee))
            }
        }
    }

    private fun EnumConstant.isMoreCanonicalThan(other: EnumConstant): Boolean = with(other.name.toLowerCase()) {
        contains("min") || contains("max") ||
                contains("first") || contains("last") ||
                contains("begin") || contains("end")
    }

    private fun generateStubsForGlobal(globalDecl: GlobalDecl) {

    }

    private fun generateStubsForObjCProtocol(objCProtocol: ObjCProtocol) {
    }

    private fun generateStubsForObjCClass(objCClass: ObjCClass) {
    }

    private fun generateStubsForObjCCategory(objCCategory: ObjCCategory) {
    }

    // TODO: make it more robust
    private fun tryCreateIntegralStub(type: Type, value: Long): IntegralConstantStub? {
        val integerType = type.unwrapTypedefs() as? IntegerType ?: return null
        return IntegralConstantStub(value)
    }

    /**
     * Indicates whether this enum should be represented as Kotlin enum.
     */
    val EnumDef.isStrictEnum: Boolean
        // TODO: if an anonymous enum defines e.g. a function return value or struct field type,
        // then it probably should be represented as Kotlin enum
        get() {
            if (this.isAnonymous) {
                return false
            }

            val name = this.kotlinName

            if (name in configuration.strictEnums) {
                return true
            }

            if (name in configuration.nonStrictEnums) {
                return false
            }

            // Let the simple heuristic decide:
            return !this.constants.any { it.isExplicitlyDefined }
        }

    /**
     * The name to be used for this enum in Kotlin
     */
    val EnumDef.kotlinName: String
        get() = if (spelling.startsWith("enum ")) {
            spelling.substringAfter(' ')
        } else {
            assert (!isAnonymous)
            spelling
        }


    private val pkgName: String
        get() = configuration.pkgName

    private val forbiddenStructNames = run {
        val typedefNames = nativeIndex.typedefs.map { it.name }
        typedefNames.toSet()
    }

    private val anonymousStructKotlinNames = mutableMapOf<StructDecl, String>()

    /**
     * The name to be used for this struct in Kotlin
     */
    val StructDecl.kotlinName: String
        get() {
            if (this.isAnonymous) {
                val names = anonymousStructKotlinNames
                return names.getOrPut(this) {
                    "anonymousStruct${names.size + 1}"
                }
            }

            val strippedCName = if (spelling.startsWith("struct ") || spelling.startsWith("union ")) {
                spelling.substringAfter(' ')
            } else {
                spelling
            }

            // TODO: don't mangle struct names because it wouldn't work if the struct
            // is imported into another interop library.
            return if (strippedCName !in forbiddenStructNames) strippedCName else (strippedCName + "Struct")
        }
}

private interface StubElementBuilder {
    fun build(): List<StubElement>
}

private class ObjCMethodBuilder(
        private val method: ObjCMethod,
        private val container: ObjCContainer,
        private val isDesignatedInitializer: Boolean,
        private val stubIrBuilder: StubIrBuilder
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
        isStret = returnType.isStret(stubIrBuilder.configuration.target)
        stubReturnType = if (returnType.unwrapTypedefs() is VoidType) {
            WrapperStubType(KotlinTypes.unit)
        } else {
            WrapperStubType(stubIrBuilder.mirror(returnType).argType)
        }
        val methodAnnotation = AnnotationStub.ObjC.Method(
                method.selector.quoteAsKotlinLiteral(),
                method.encoding.quoteAsKotlinLiteral(),
                isStret
        )
        annotations = buildObjCMethodAnnotations(methodAnnotation)
        parameters = method.getStubParameters(stubIrBuilder, forConstructorOrFactory = false)
        external = (container !is ObjCProtocol)
        modality = when (container) {
            is ObjCClassOrProtocol -> if (method.isOverride(container)) {
                MemberStubModality.OVERRIDE
            } else when (container) {
                is ObjCClass -> MemberStubModality.OPEN
                is ObjCProtocol -> MemberStubModality.NONE
            }
            is ObjCCategory -> MemberStubModality.NONE
        }
        receiverType = if (container is ObjCCategory)
            WrapperStubType(stubIrBuilder.declarationMapper.getKotlinClassFor(container.clazz, isMeta = method.isClass).type)
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
            val parameters = method.getStubParameters(stubIrBuilder, forConstructorOrFactory = true)
            when (container) {
                // TODO: should `deprecatedInit` be added?
                is ObjCClass -> {
                    // TODO: consider generating non-designated initializers as factories.
                    val designated = isDesignatedInitializer ||
                            stubIrBuilder.configuration.disableDesignatedInitializerChecks

                    val annotations = listOf(AnnotationStub.ObjC.Constructor(method.selector.quoteAsKotlinLiteral(), designated))
                    val constructor = ConstructorStub(parameters, annotations)
                    results += constructor
                }
                is ObjCCategory -> {
                    assert(!method.isClass)
                    val clazz= stubIrBuilder.declarationMapper
                            .getKotlinClassFor(container.clazz, isMeta = false).type

                    val factoryAnnonation = AnnotationStub.ObjC.Factory(
                            method.selector.quoteAsKotlinLiteral(),
                            method.encoding.quoteAsKotlinLiteral(),
                            isStret
                    )
                    val annotations = buildObjCMethodAnnotations(factoryAnnonation)

                    val originalReturnType = method.getReturnType(container.clazz)
                    val returnType = if (originalReturnType is ObjCPointer) {
                        TypeParameterStub("T",  nullable = originalReturnType.isNullable)
                    } else {
                        // This shouldn't happen actually.
                        this.stubReturnType
                    }
                    val typeParameter = TypeParameterStub("T", WrapperStubType(clazz))
                    val receiverType = WrapperStubType(KotlinTypes.objCClassOf.type, listOf(typeParameter))
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

private fun ObjCMethod.getStubParameters(
        stubIrBuilder: StubIrBuilder,
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

private abstract class ObjCContainerBuilder(
        stubIrBuilder: StubIrBuilder,
        private val container: ObjCClassOrProtocol,
        protected val metaContainerStub: ObjCContainerBuilder?
) {
    private val isMeta: Boolean get() = metaContainerStub == null

    private val methods: List<ObjCMethod>
    private val properties: List<ObjCProperty>

    val protocolGetter: String?

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

    private val methodBuilders get() = methodToStub.values

    private val designatedInitializerSelectors = if (container is ObjCClass && !isMeta) {
        container.getDesignatedInitializerSelectors(mutableSetOf())
    } else {
        emptySet()
    }

    private val methodToStub = methods.map {
        it to ObjCMethodBuilder(it, container,
                isDesignatedInitializer = it.selector in designatedInitializerSelectors,
                stubIrBuilder = stubIrBuilder
        )
    }.toMap()

    val propertyBuilders = properties.mapNotNull {
        createObjCPropertyBuilder(stubIrBuilder, it, container, this.methodToStub)
    }

    val modality = when (container) {
        is ObjCClass -> ClassStubModality.OPEN
        is ObjCProtocol -> ClassStubModality.INTERFACE
    }

    val classifier = stubIrBuilder.declarationMapper.getKotlinClassFor(container, isMeta)

    val externalObjCAnnotation = when (container) {
        is ObjCProtocol -> {
            protocolGetter = if (metaContainerStub != null) {
                metaContainerStub.protocolGetter!!
            } else {
//                    val nativeBacked = object : NativeBacked {}
                // TODO: handle the case when protocol getter stub can't be compiled.
//                    genProtocolGetter(stubIrBuilder, nativeBacked, container)
                // TODO: Decide, when to generaete Function name
                "kniprot_" + stubIrBuilder.configuration.pkgName.replace('.', '_') + stubIrBuilder.nextUniqueId()
            }
            AnnotationStub.ObjC.ExternalClass(protocolGetter)
        }
        is ObjCClass -> {
            protocolGetter = null
            val binaryName = container.binaryName
            AnnotationStub.ObjC.ExternalClass("", binaryName ?: "")
        }
    }

    val supers = mutableListOf<StubType>()

    init {
        if (container is ObjCClass) {
            val baseClass = container.baseClass
            val baseClassifier = if (baseClass != null) {
                stubIrBuilder.declarationMapper.getKotlinClassFor(baseClass, isMeta)
            } else {
                if (isMeta) KotlinTypes.objCObjectBaseMeta else KotlinTypes.objCObjectBase
            }
            supers += WrapperStubType(baseClassifier.type)
        }
        container.protocols.forEach {
            supers += WrapperStubType(stubIrBuilder.declarationMapper.getKotlinClassFor(it, isMeta).type)
        }
        if (supers.isEmpty()) {
            assert(container is ObjCProtocol)
            val classifier = if (isMeta) KotlinTypes.objCObjectMeta else KotlinTypes.objCObject
            supers += WrapperStubType(classifier.type)
        }
        if (!isMeta && container.isProtocolClass()) {
            // TODO: map Protocol type to ObjCProtocol instead.
            supers += WrapperStubType(KotlinTypes.objCProtocol.type)
        }
    }

    fun buildBody(): Pair<List<PropertyStub>, List<FunctionalStub>> {
        // TODO: add protected constructor if needed
        return Pair(
                propertyBuilders.flatMap { it.build() },
                methodBuilders.flatMap { it.build() }
        )
    }
}

private open class ObjCClassOrProtocolBuilder(
        stubIrBuilder: StubIrBuilder,
        private val container: ObjCClassOrProtocol
) : ObjCContainerBuilder(
        stubIrBuilder,
        container,
        metaContainerStub = object : ObjCContainerBuilder(stubIrBuilder, container, metaContainerStub = null) {}
)

private class ObjCClassBuilder(
        private val stubIrBuilder: StubIrBuilder,
        private val clazz: ObjCClass
) : ObjCClassOrProtocolBuilder(stubIrBuilder, clazz), StubElementBuilder {
    override fun build(): List<StubElement> {
        val companionSuper = stubIrBuilder.declarationMapper
                .getKotlinClassFor(clazz, isMeta = true).type
                .let { WrapperStubType(it) }

        val objCClassType = KotlinTypes.objCClassOf.typeWith(
                stubIrBuilder.declarationMapper.getKotlinClassFor(clazz, isMeta = false).type
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

private class ObjCCategoryBuilder(
        private val stubIrBuilder: StubIrBuilder,
        private val category: ObjCCategory
) : StubElementBuilder {
    private val generatedMembers = stubIrBuilder.generatedObjCCategoriesMembers
            .getOrPut(category.clazz, { GeneratedObjCCategoriesMembers() })

    private val methodToBuilder = category.methods.filter { generatedMembers.register(it) }.map {
        it to ObjCMethodBuilder(it, category, isDesignatedInitializer = false, stubIrBuilder = stubIrBuilder)
    }.toMap()

    private val methodBuilders get() = methodToBuilder.values

    private val propertyBuilders = category.properties.filter { generatedMembers.register(it) }.mapNotNull {
        createObjCPropertyBuilder(stubIrBuilder, it, category, methodToBuilder)
    }

    override fun build(): List<StubElement> =
        methodBuilders.flatMap { it.build() } + propertyBuilders.flatMap { it.build() }
}

private fun createObjCPropertyBuilder(
        stubIrBuilder: StubIrBuilder,
        property: ObjCProperty,
        container: ObjCContainer,
        methodToStub: Map<ObjCMethod, ObjCMethodBuilder>
): ObjCPropertyBuilder? {
    // Note: the code below assumes that if the property is generated,
    // then its accessors are also generated as explicit methods.
    val getterStub = methodToStub[property.getter] ?: return null
    val setterStub = property.setter?.let { methodToStub[it] ?: return null }
    return ObjCPropertyBuilder(stubIrBuilder, property, container, getterStub, setterStub)
}


private class ObjCPropertyBuilder(
        private val stubIrBuilder: StubIrBuilder,
        private val property: ObjCProperty,
        private val container: ObjCContainer,
        private val getterBuilder: ObjCMethodBuilder,
        private val setterMethod: ObjCMethodBuilder?
) : StubElementBuilder {
    override fun build(): List<PropertyStub> {
        val type = property.getType(container.classOrProtocol)
        val kotlinType = stubIrBuilder.mirror(type).argType
        val getter = PropertyAccessor.Getter.ExternalGetter(annotations = getterBuilder.annotations)
        val setter = property.setter?.let { PropertyAccessor.Setter.ExternalSetter(annotations = setterMethod!!.annotations) }
        val kind = setter?.let { PropertyStub.Kind.Var(getter, it) } ?: PropertyStub.Kind.Val(getter)
        val modality = if (container is ObjCProtocol) MemberStubModality.FINAL else MemberStubModality.NONE
        val receiver = when (container) {
            is ObjCClassOrProtocol -> null
            is ObjCCategory -> stubIrBuilder.declarationMapper
                    .getKotlinClassFor(container.clazz, isMeta = property.getter.isClass).type
                    .let { WrapperStubType(it) }
        }
        return listOf(PropertyStub(property.name, WrapperStubType(kotlinType), kind, modality, receiver))
    }
}