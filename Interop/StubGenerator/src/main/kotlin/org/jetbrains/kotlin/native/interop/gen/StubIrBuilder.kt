package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.gen.jvm.InteropConfiguration
import org.jetbrains.kotlin.native.interop.gen.jvm.KotlinPlatform
import org.jetbrains.kotlin.native.interop.indexer.*

class StubIrContainer(
        val classses: List<ClassStub>,
        val functions: List<FunctionalStub>,
        val enums: List<EnumStub>,
        val globals: List<GlobalStub>,
        val typealiases: List<TypealiasStub>
)

class StubIrBuilder(
        val configuration: InteropConfiguration,
        val platform: KotlinPlatform,
        private val nativeIndex: NativeIndex,
        val imports: Imports
) {

    private val classses = mutableListOf<ClassStub>()
    private val functions = mutableListOf<FunctionalStub>()
    private val enums = mutableListOf<EnumStub>()
    private val globals = mutableListOf<GlobalStub>()
    private val typealiases = mutableListOf<TypealiasStub>()

    fun build(): StubIrContainer {
        nativeIndex.objCProtocols.forEach { generateStubsForObjCProtocol(it) }

        return StubIrContainer(classses, functions, enums, globals, typealiases)
    }

    private fun generateStubsForObjCProtocol(objCProtocol: ObjCProtocol) {
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

private class ObjCMethodBuilder(
        private val method: ObjCMethod,
        private val container: ObjCContainer,
        private val isDesignatedInitializer: Boolean,
        private val stubIrBuilder: StubIrBuilder
) {
    private val isStret: Boolean
    private val stubReturnType: StubType
    private val annotations: List<AnnotationStub>
    private val parameters: List<FunctionParameterStub>
    private val external: Boolean
    private val receiverType: StubType?
    private val name: String = method.kotlinName
    private val origin = StubOrigin.ObjCMethod(method, container)
    private val modality: FunctionStubModality

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
                FunctionStubModality.OVERRIDE
            } else when (container) {
                is ObjCClass -> FunctionStubModality.OPEN
                is ObjCProtocol -> FunctionStubModality.NONE
            }
            is ObjCCategory -> FunctionStubModality.NONE
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

    fun build(): List<FunctionalStub> {
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
                    val receiverType = ClassifierStubType(KotlinTypes.objCClassOf, listOf(typeParameter))
                    val createMethod = FunctionStub(
                            "create",
                            returnType,
                            parameters,
                            receiverType = receiverType,
                            typeParameters = listOf(typeParameter),
                            external = true,
                            origin = StubOrigin.None,
                            annotations = annotations,
                            modality = FunctionStubModality.NONE
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