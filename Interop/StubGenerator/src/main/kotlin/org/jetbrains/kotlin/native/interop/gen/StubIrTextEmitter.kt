package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.gen.jvm.InteropConfiguration
import org.jetbrains.kotlin.native.interop.gen.jvm.KotlinPlatform
import org.jetbrains.kotlin.native.interop.indexer.*
import java.lang.IllegalStateException

class StubIrTextEmitter(
        private val configuration: InteropConfiguration,
        private val libName: String,
        private val platform: KotlinPlatform,
        private val stubs: TopLevelContainer,
        private val nativeIndex: NativeIndex,
        private val ktFile: Appendable,
        private val cFile: Appendable,
        private val entryPoint: String?,
        private val imports: Imports
) {

//    private val ktOutput = TODO()
//    private val cOutput = TODO()

    private val pkgName: String
        get() = configuration.pkgName

    private val jvmFileClassName = if (pkgName.isEmpty()) {
        libName
    } else {
        pkgName.substringAfterLast('.')
    }

    val libraryForCStubs = configuration.library.copy(
            includes = mutableListOf<String>().apply {
                add("stdint.h")
                add("string.h")
                if (platform == KotlinPlatform.JVM) {
                    add("jni.h")
                }
                addAll(configuration.library.includes)
            },

            compilerArgs = configuration.library.compilerArgs,

            additionalPreambleLines = configuration.library.additionalPreambleLines +
                    when (configuration.library.language) {
                        Language.C -> emptyList()
                        Language.OBJECTIVE_C -> listOf("void objc_terminate();")
                    }
    )

    private val declarationMapper = object : DeclarationMapper {
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

    private val kotlinFile = object : KotlinFile(pkgName, namesToBeDeclared = computeNamesToBeDeclared()) {
        override val mappingBridgeGenerator: MappingBridgeGenerator
            get() = this@StubIrTextEmitter.mappingBridgeGenerator
    }

    private val simpleBridgeGenerator: SimpleBridgeGenerator =
            SimpleBridgeGeneratorImpl(
                    platform,
                    pkgName,
                    jvmFileClassName,
                    libraryForCStubs,
                    topLevelNativeScope = object : NativeScope {
                        override val mappingBridgeGenerator: MappingBridgeGenerator
                            get() = this@StubIrTextEmitter.mappingBridgeGenerator
                    },
                    topLevelKotlinScope = kotlinFile
            )

    val mappingBridgeGenerator: MappingBridgeGenerator =
            MappingBridgeGeneratorImpl(declarationMapper, simpleBridgeGenerator)

    companion object {
        private val VALID_PACKAGE_NAME_REGEX = "[a-zA-Z0-9_.]+".toRegex()
    }


    private fun computeNamesToBeDeclared(): MutableList<String> {
        return mutableListOf<String>("TODO()")
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

    /**
     * The names that should not be used for struct classes to prevent name clashes
     */
    val forbiddenStructNames = run {
        val typedefNames = nativeIndex.typedefs.map { it.name }
        typedefNames.toSet()
    }

    val anonymousStructKotlinNames = mutableMapOf<StructDecl, String>()

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

    /**
     * The output currently used by the generator.
     * Should append line separator after any usage.
     */
    private var out: (String) -> Unit = {
        throw IllegalStateException()
    }

    fun <R> withOutput(output: (String) -> Unit, action: () -> R): R {
        val oldOut = out
        out = output
        try {
            return action()
        } finally {
            out = oldOut
        }
    }

    fun generateLinesBy(action: () -> Unit): List<String> {
        val result = mutableListOf<String>()
        withOutput({ result.add(it) }, action)
        return result
    }

    fun <R> withOutput(appendable: Appendable, action: () -> R): R {
        return withOutput({ appendable.appendln(it) }, action)
    }

    private fun generateKotlinFragmentBy(block: () -> Unit): KotlinStub {
        val lines = generateLinesBy(block)
        return object : KotlinStub {
            override fun generate(context: StubGenerationContext) = lines.asSequence()
        }
    }

    private fun <R> indent(action: () -> R): R {
        val oldOut = out
        return withOutput({ oldOut("    $it") }, action)
    }

    private fun <R> block(header: String, body: () -> R): R {
        out("$header {")
        val res = indent {
            body()
        }
        out("}")
        return res
    }

    fun emit() {
        withOutput(ktFile) {

        }
        printer.visit(stubs)
    }

    private val printer = object : StubIrVisitor {
        override fun visit(element: StubElement) {
            TODO("not implemented")
        }

        override fun visitContainer(element: StubContainer) {
            TODO("not implemented")
        }

        override fun visitClass(element: SimpleClassStub) {

            block(renderClassHeader(element)) {
            }
        }

        override fun visitCompanion(element: CompanionStub) {
            TODO("not implemented")
        }

        override fun visitTypealias(element: TypealiasStub) {
            TODO("not implemented")
        }

        override fun visitFunction(element: FunctionStub) {
            TODO("not implemented")
        }

        override fun visitProperty(element: PropertyStub) {
            TODO("not implemented")
        }

        override fun visitEnumVariant(enumVariantStub: EnumVariantStub) {
            TODO("not implemented")
        }

        override fun visitConstructor(constructorStub: ConstructorStub) {
            TODO("not implemented")
        }

        override fun visitPropertyAccessor(propertyAccessor: PropertyAccessor) {
            TODO("not implemented")
        }
    }

    private fun renderClassHeader(classStub: SimpleClassStub): String {

        val modality = renderClassStubModality(classStub.modality)
        val className = renderClassifier(classStub.classifier)
        val constructorParams = renderConstructorParams(classStub.constructorParams)
        val superClassInit = classStub.superClassInit?.let { " : " + renderSuperInit(it) } ?: ""

        return "$modality $className$superClassInit"
    }

    private fun renderClassifier(classifier: Classifier): String {
        return kotlinFile.declare(classifier)
    }

    private fun renderClassStubModality(classStubModality: ClassStubModality): String = when (classStubModality) {
        ClassStubModality.INTERFACE -> "interface"
        ClassStubModality.OPEN -> "open class"
        ClassStubModality.ABSTRACT -> "abstract class"
        ClassStubModality.NONE -> "class"
    }

    private fun renderConstructorParams(parameters: List<ConstructorParamStub>): String =
            if (parameters.isEmpty()) {
                ""
            } else {
                parameters.joinToString(prefix = "(", postfix = ")") { renderConstructorParameter(it) }
            }

    private fun renderConstructorParameter(paramStub: ConstructorParamStub): String {
        val prefix = when (paramStub.qualifier) {
            ConstructorParamStub.Qualifier.VAL -> "val "
            ConstructorParamStub.Qualifier.VAR -> "var "
            ConstructorParamStub.Qualifier.NONE -> ""
        }
        return "$prefix${paramStub.name}: ${renderStubType(paramStub.type)}"
    }

    private fun renderSuperInit(superClassInit: SuperClassInit): String {
        val parameters = if (superClassInit.parameters.isEmpty()) {
            ""
        } else {
            superClassInit.parameters.joinToString(prefix = "(", postfix = ")") { renderValueUsage(it) }
        }
        return "${renderStubType(superClassInit.type)}$parameters"
    }

    private fun renderStubType(stubType: StubType): String = when (stubType) {
        is TypeParameterStub -> TODO()
        is WrapperStubType -> TODO()
        is SymbolicStubType -> TODO()
    }

    private fun renderValueUsage(value: ValueStub): String = when (value) {
        is StringConstantStub -> value.value
        is IntegralConstantStub -> value.value.toString()
        is DoubleConstantStub -> value.value.toString()
        is ConstructorParamStub -> value.name
        is FunctionParameterStub -> value.name
    }

    private fun renderAnnotation(annotationStub: AnnotationStub): String = when (annotationStub) {
        AnnotationStub.ObjC.ConsumesReceiver -> "@CCall.ConsumesReceiver"
        AnnotationStub.ObjC.ReturnsRetained -> "@CCall.ReturnsRetained"
        is AnnotationStub.ObjC.Method -> "@ObjCMethod(${annotationStub.selector}, ${annotationStub.encoding}, ${annotationStub.isStret})"
        is AnnotationStub.ObjC.Factory -> "@ObjCFactory(${annotationStub.selector}, ${annotationStub.encoding}, ${annotationStub.isStret})"
        AnnotationStub.ObjC.Consumed -> "@CCall.Consumed"
        is AnnotationStub.ObjC.Constructor -> "@ObjCConstructor(${annotationStub.selector}, ${annotationStub.designated})"
        AnnotationStub.CCall.CString -> "@CCall.CString"
        AnnotationStub.CCall.WCString -> "@CCall.WCString"
        is AnnotationStub.CCall.Symbol -> "@CCall(${annotationStub.symbolName})"
        is AnnotationStub.CStruct -> "@CStruct(${annotationStub.struct})"
        is AnnotationStub.CNaturalStruct -> "@CNaturalStruct(${annotationStub.struct})"
        is AnnotationStub.CLength -> "@CLength(${annotationStub.length})"
    }
}