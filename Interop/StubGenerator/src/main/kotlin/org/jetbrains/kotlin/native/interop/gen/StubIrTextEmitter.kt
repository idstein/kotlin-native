package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.gen.jvm.InteropConfiguration
import org.jetbrains.kotlin.native.interop.gen.jvm.KotlinPlatform
import org.jetbrains.kotlin.native.interop.indexer.*
import org.jetbrains.kotlin.utils.addToStdlib.firstIsInstance
import java.lang.IllegalStateException

class StubIrTextEmitter(
        private val configuration: InteropConfiguration,
        private val libName: String,
        private val platform: KotlinPlatform,
        private val stubs: TopLevelContainer,
        private val ktFile: Appendable,
        private val cFile: Appendable,
        private val entryPoint: String?
) : NativeBacked {

    val fakeNativeBackedStub = object : NativeBacked {}

    private val StubElement.isTopLevel get() = this in stubs.children

    private val pkgName: String
        get() = configuration.pkgName

    private val jvmFileClassName = if (pkgName.isEmpty()) {
        libName
    } else {
        pkgName.substringAfterLast('.')
    }

    private val libraryForCStubs = configuration.library.copy(
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

    private val declarationMapper = stubs.declarationMapper

    private val kotlinFile = object : KotlinFile(pkgName, namesToBeDeclared = stubs.namesToBeDeclared) {
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

    /**
     * The output currently used by the generator.
     * Should append line separator after any usage.
     */
    private var out: (String) -> Unit = {
        throw IllegalStateException()
    }

    private fun <R> withOutput(output: (String) -> Unit, action: () -> R): R {
        val oldOut = out
        out = output
        try {
            return action()
        } finally {
            out = oldOut
        }
    }

    private fun <R> withOutput(appendable: Appendable, action: () -> R): R {
        return withOutput({ appendable.appendln(it) }, action)
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

    private fun generateKotlinFileHeader() {
        if (platform == KotlinPlatform.JVM) {
            out("@file:JvmName(${jvmFileClassName.quoteAsKotlinLiteral()})")
        }
        if (platform == KotlinPlatform.NATIVE) {
            out("@file:kotlinx.cinterop.InteropStubs")
        }

        val suppress = mutableListOf("UNUSED_VARIABLE", "UNUSED_EXPRESSION").apply {
            if (configuration.library.language == Language.OBJECTIVE_C) {
                add("CONFLICTING_OVERLOADS")
                add("RETURN_TYPE_MISMATCH_ON_INHERITANCE")
                add("PROPERTY_TYPE_MISMATCH_ON_INHERITANCE") // Multiple-inheriting property with conflicting types
                add("VAR_TYPE_MISMATCH_ON_INHERITANCE") // Multiple-inheriting mutable property with conflicting types
                add("RETURN_TYPE_MISMATCH_ON_OVERRIDE")
                add("WRONG_MODIFIER_CONTAINING_DECLARATION") // For `final val` in interface.
                add("PARAMETER_NAME_CHANGED_ON_OVERRIDE")
                add("UNUSED_PARAMETER") // For constructors.
                add("MANY_IMPL_MEMBER_NOT_IMPLEMENTED") // Workaround for multiple-inherited properties.
                add("MANY_INTERFACES_MEMBER_NOT_IMPLEMENTED") // Workaround for multiple-inherited properties.
                add("EXTENSION_SHADOWED_BY_MEMBER") // For Objective-C categories represented as extensions.
                add("REDUNDANT_NULLABLE") // This warning appears due to Obj-C typedef nullability incomplete support.
                add("DEPRECATION") // For uncheckedCast.
                add("DEPRECATION_ERROR") // For initializers.
            }
        }

        out("@file:Suppress(${suppress.joinToString { it.quoteAsKotlinLiteral() }})")
        if (pkgName != "") {
            val packageName = pkgName.split(".").joinToString("."){
                if(it.matches(VALID_PACKAGE_NAME_REGEX)){
                    it
                }else{
                    "`$it`"
                }
            }
            out("package $packageName")
            out("")
        }
        if (platform == KotlinPlatform.NATIVE) {
            out("import kotlin.native.SymbolName")
            out("import kotlinx.cinterop.internal.*")
        }
        out("import kotlinx.cinterop.*")

        kotlinFile.buildImports().forEach {
            out(it)
        }

        out("")

        out("// NOTE THIS FILE IS AUTO-GENERATED")
        out("")
    }

    fun emit() {
        withOutput(ktFile) {
            generateKotlinFileHeader()
            printer.visitContainer(stubs)
        }
        val nativeBridges = simpleBridgeGenerator.prepare()

        withOutput(ktFile) {
            nativeBridges.kotlinLines.forEach(out)
            if (platform == KotlinPlatform.JVM) {
                out("private val loadLibrary = System.loadLibrary(\"$libName\")")
            }
        }
    }

    private fun EnumVariantStub.isMoreCanonicalThan(other: EnumVariantStub): Boolean = with(other.name.toLowerCase()) {
        contains("min") || contains("max") ||
                contains("first") || contains("last") ||
                contains("begin") || contains("end")
    }

    private val printer = object : StubIrVisitor {
        override fun visitClass(element: ClassStub) {
            element.annotations.forEach {
                out(renderAnnotation(it))
            }
            block(renderClassHeader(element)) {
                if (element is ClassStub.Enum) {
                    renderEnumBody(element)
                } else {
                    visitContainer(element)
                }
            }
        }

        override fun visitContainer(element: StubContainer) {
            out("")
            element.children.forEach {
                it.accept(this)
                out("")
            }
        }

        override fun visitTypealias(element: TypealiasStub) {
            out("typealias ${renderStubType(element.alias)} = ${renderStubType(element.aliasee)}")
        }

        override fun visitFunction(element: FunctionStub) {
            val modality = renderMemberModality(element.modality)
            val external = if (element.external) "external " else ""
            element.annotations.forEach {
                out(renderAnnotation(it))
            }
            val parameters = element.parameters.joinToString(prefix = "(", postfix = ")") { renderFunctionParameter(it) }
            block("$external${modality}fun ${element.name}$parameters: ${renderStubType(element.returnType)}") {
                renderBridgeBody(element)
            }
        }

        override fun visitProperty(element: PropertyStub) {
            element.annotations.forEach {
                out(renderAnnotation(it))
            }
            val modality = renderMemberModality(element.modality)
            val receiver = if (element.receiverType != null) "${renderStubType(element.receiverType)}." else ""
            when (val kind = element.kind) {
                is PropertyStub.Kind.Constant -> {
                    out("${modality}const val $receiver${element.name}: ${renderStubType(element.type)} = ${renderValueUsage(kind.constant)}")
                }
                is PropertyStub.Kind.Val -> {
                    val shouldWriteInline = kind.getter is PropertyAccessor.Getter.SimpleGetter && kind.getter.constant != null
                    if (shouldWriteInline) {
                        out("${modality}val $receiver${element.name}: ${renderStubType(element.type)}${renderPropertyAccessor(kind.getter)}")
                    } else {
                        out("${modality}val $receiver${element.name}: ${renderStubType(element.type)}")
                        indent {
                            out(renderPropertyAccessor(kind.getter))
                        }
                    }
                }
                is PropertyStub.Kind.Var -> {
                    out("${modality}var $receiver${element.name}: ${renderStubType(element.type)}")
                    indent {
                        out(renderPropertyAccessor(kind.getter))
                        out(renderPropertyAccessor(kind.setter))
                    }
                }
            }
        }

        override fun visitEnumVariant(enumVariantStub: EnumVariantStub) {
        }

        override fun visitConstructor(constructorStub: ConstructorStub) {
        }

        override fun visitPropertyAccessor(propertyAccessor: PropertyAccessor) {
        }

        override fun visitSimpleStubContainer(simpleStubContainer: SimpleStubContainer) {
            out(simpleStubContainer.meta.textAtStart)
            out("")
            simpleStubContainer.classes.forEach { it.accept(this) }
            simpleStubContainer.functions.forEach { it.accept(this) }
            simpleStubContainer.properties.forEach { it.accept(this) }
            simpleStubContainer.typealiases.forEach { it.accept(this) }
            simpleStubContainer.simpleContainers.forEach { it.accept(this) }
            out(simpleStubContainer.meta.textAtEnd)
            out("")
        }
    }

    // TODO: It looks like more logic can be shared between different emitters.
    private fun renderEnumBody(enum: ClassStub.Enum) {
        val canonicalsByValue = enum.variants
                .groupingBy { it.constant.value }
                .reduce { _, accumulator, element ->
                    if (element.isMoreCanonicalThan(accumulator)) {
                        element
                    } else {
                        accumulator
                    }
                }

        val (canonicalConstants, aliasConstants) = enum.variants.partition { canonicalsByValue[it.constant.value] == it }

        canonicalConstants.forEach {
            renderEnumVariant(it)
        }

        val simpleKotlinName = enum.classifier.topLevelName.asSimpleName()
        val baseKotlinType = enum.baseType.kotlinType
        val basePointedTypeName = enum.pointedType.kotlinType.render(kotlinFile)

        out(";")
        block("companion object") {
            aliasConstants.forEach {
                val mainConstant = canonicalsByValue[it.constant.value]!!
                out("val ${it.name.asSimpleName()} = ${mainConstant.name.asSimpleName()}")
            }
            if (aliasConstants.isNotEmpty()) out("")

            out("fun byValue(value: $baseKotlinType) = " +
                    "$simpleKotlinName.values().find { it.value == value }!!")
        }
        out("")
        block("class Var(rawPtr: NativePtr) : CEnumVar(rawPtr)") {
            out("companion object : Type($basePointedTypeName.size.toInt())")
            out("var value: $simpleKotlinName")
            out("    get() = byValue(this.reinterpret<$basePointedTypeName>().value)")
            out("    set(value) { this.reinterpret<$basePointedTypeName>().value = value.value }")
        }
    }

    private fun renderFunctionParameter(parameter: FunctionParameterStub): String {
        val annotations = if (parameter.annotations.isEmpty())
            ""
        else
            parameter.annotations.joinToString { renderAnnotation(it) } + " "
        val vararg = if (parameter.isVararg) "vararg " else ""
        return "$annotations$vararg${parameter.name}: ${renderStubType(parameter.type)}"
    }

    private fun renderMemberModality(modality: MemberStubModality): String = when (modality) {
        MemberStubModality.OVERRIDE -> "override "
        MemberStubModality.OPEN -> "open "
        MemberStubModality.NONE -> ""
        MemberStubModality.FINAL -> "final "
    }

    private fun isCValuesRef(type: StubType): Boolean {
        if (type !is WrapperStubType) return false

        return type.kotlinType is KotlinClassifierType && type.kotlinType.classifier == KotlinTypes.cValuesRef
    }

    private fun renderBridgeBody(function: FunctionStub) {
        assert(function.origin is StubOrigin.Function)
        val origin = function.origin as StubOrigin.Function
        val bodyGenerator = KotlinCodeBuilder(scope = kotlinFile)
        val bridgeArguments = mutableListOf<TypedKotlinValue>()
        var isVararg = false
        function.parameters.forEachIndexed { index, parameter ->
            isVararg = isVararg or parameter.isVararg
            val bridgeArgument = when {
                parameter.annotations.filterIsInstance<AnnotationStub.CCall.CString>().isNotEmpty() -> {
                    bodyGenerator.pushMemScoped()
                    "${parameter.name}?.cstr?.getPointer(memScope)"
                }
                parameter.annotations.filterIsInstance<AnnotationStub.CCall.WCString>().isNotEmpty() -> {
                    bodyGenerator.pushMemScoped()
                    "${parameter.name}?.wcstr?.getPointer(memScope)"
                }
                isCValuesRef(parameter.type) -> {
                    bodyGenerator.pushMemScoped()
                    bodyGenerator.getNativePointer(parameter.name)
                }
                else -> {
                    parameter.name
                }
            }
            // TODO: Better way to pass [Type]?
            bridgeArguments += TypedKotlinValue(origin.function.parameters[index].type, bridgeArgument)
        }
        if (!isVararg || platform != KotlinPlatform.NATIVE) {
            val result = mappingBridgeGenerator.kotlinToNative(
                    bodyGenerator,
                    this,
                    origin.function.returnType,
                    bridgeArguments,
                    independent = false
            ) { nativeValues ->
                "${origin.function.name}(${nativeValues.joinToString()})"
            }
            bodyGenerator.returnResult(result)
        } else {
            val cCallAnnotation = function.annotations.firstIsInstance<AnnotationStub.CCall.Symbol>()
            val cCallSymbolName = cCallAnnotation.symbolName
            simpleBridgeGenerator.insertNativeBridge(
                    this,
                    emptyList(),
                    listOf("extern const void* $cCallSymbolName __asm(${cCallSymbolName.quoteAsKotlinLiteral()});",
                            "extern const void* $cCallSymbolName = &${origin.function.name};")
            )
        }
        bodyGenerator.build().forEach { out(it) }
    }

    private fun renderClassHeader(classStub: ClassStub): String {
        val modality = when (classStub) {
            is ClassStub.Simple -> renderClassStubModality(classStub.modality)
            is ClassStub.Companion -> ""
            is ClassStub.Enum -> "enum class"
        }.let { it }
        val className = when (classStub) {
            is ClassStub.Simple -> declareClassifier(classStub.classifier)
            is ClassStub.Companion -> "companion object"
            is ClassStub.Enum -> declareClassifier(classStub.classifier)
        }
        val constructorParams = when (classStub) {
            is ClassStub.Simple -> renderConstructorParams(classStub.constructorParams)
            is ClassStub.Companion -> ""
            is ClassStub.Enum -> renderConstructorParams(classStub.constructorParams)
        }

        val superClassInit = classStub.superClassInit?.let { " : " + renderSuperInit(it) } ?: ""

        val interfaces = if (classStub.interfaces.isEmpty()) {
            ""
        } else {
            classStub.interfaces.joinToString(prefix = if (superClassInit.isNotEmpty()) ", " else " ") {
                renderStubType(it)
            }
        }

        return "$modality $className$constructorParams$superClassInit$interfaces"
    }

    private fun declareClassifier(classifier: Classifier): String {
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
            is ConstructorParamStub.Qualifier.VAL -> if (paramStub.qualifier.overrides) "override val " else "val "
            is ConstructorParamStub.Qualifier.VAR -> if (paramStub.qualifier.overrides) "override var " else "var "
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
        is TypeParameterStub -> {
            val nullableSymbol = if (stubType.nullable) "?" else ""
            val upperBound = if (stubType.upperBound != null) " : " + renderStubType(stubType.upperBound) else ""
            "${stubType.name}$nullableSymbol$upperBound"
        }
        is WrapperStubType -> stubType.kotlinType.render(kotlinFile)
        is SymbolicStubType -> stubType.name
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

    private fun renderEnumVariant(enumVariantStub: EnumVariantStub): String =
            "${enumVariantStub.name}(${renderValueUsage(enumVariantStub.constant)})"

    private fun renderPropertyAccessor(accessor: PropertyAccessor): String = when (accessor) {
        is PropertyAccessor.Getter.SimpleGetter -> {
            when {
                accessor.constant != null -> " = ${renderValueUsage(accessor.constant)}"
                else -> TODO()
            }
        }
        is PropertyAccessor.Getter.ExternalGetter -> "external get()"

        is PropertyAccessor.Getter.ArrayMemberAt -> "get() = arrayMemberAt(${accessor.offset})"

        is PropertyAccessor.Getter.MemberAt -> {
            if (accessor.typeParameters.isEmpty()) {
                "get() = memberAt(${accessor.offset})"
            } else {
                val typeParameters = accessor.typeParameters.joinToString(prefix = "<", postfix = ">") { renderStubType(it) }
                "get() = memberAt$typeParameters(${accessor.offset}).value"
            }
        }

        is PropertyAccessor.Getter.ReadBits -> {
            "get() = readBits(this.rawPtr, ${accessor.offset}, ${accessor.size}, ${accessor.signed})"
        }

        is PropertyAccessor.Setter.SimpleSetter -> TODO()

        is PropertyAccessor.Setter.MemberAt -> {
            if (accessor.typeParameters.isEmpty()) {
                error("Unexpected memberAt setter without type parameters!")
            } else {
                val typeParameters = accessor.typeParameters.joinToString(prefix = "<", postfix = ">") { renderStubType(it) }
                "set(value) { memberAt$typeParameters(${accessor.offset}).value = value }"
            }
        }

        is PropertyAccessor.Setter.WriteBits -> {
            "set() = writeBits(this.rawPtr, ${accessor.offset}, ${accessor.size}, TODO())"
        }

        is PropertyAccessor.Setter.ExternalSetter -> "external set(TODO)"

        is PropertyAccessor.Getter.BridgedGetter -> {
            val typeInfo = accessor.typeInfo
            val expression = if (accessor.isArray) {
                val getAddressExpression = getGlobalAddressExpression(accessor.cGlobalName)
                typeInfo.argFromBridged(getAddressExpression, kotlinFile, nativeBacked = this) + "!!"
            } else {
                typeInfo.argFromBridged(simpleBridgeGenerator.kotlinToNative(
                        nativeBacked = this,
                        returnType = typeInfo.bridgedType,
                        kotlinValues = emptyList(),
                        independent = false
                ) {
                    typeInfo.cToBridged(expr = accessor.cGlobalName)
                }, kotlinFile, nativeBacked = this)
            }
            "get() = $expression"
        }

        is PropertyAccessor.Getter.InterpretPointed -> {
            val typeParameters = accessor.typeParameters.joinToString(prefix = "<", postfix = ">") { renderStubType(it) }
            val getAddressExpression = getGlobalAddressExpression(accessor.cGlobalName)
            "get() = interpretPointed$typeParameters($getAddressExpression)"
        }

        is PropertyAccessor.Setter.BridgedSetter -> {
            val typeInfo = accessor.typeInfo
            val bridgedValue = BridgeTypedKotlinValue(typeInfo.bridgedType, typeInfo.argToBridged("value"))

            "set(value) { " + simpleBridgeGenerator.kotlinToNative(
                    nativeBacked = fakeNativeBackedStub,
                    returnType = BridgedType.VOID,
                    kotlinValues = listOf(bridgedValue),
                    independent = false
            ) { nativeValues ->
                out("${accessor.cGlobalName} = ${typeInfo.cFromBridged(
                        nativeValues.single(),
                        scope,
                        nativeBacked = fakeNativeBackedStub
                )};")
                ""
            } + " }"
        }
    }

    private val globalAddressExpressions = mutableMapOf<String, KotlinExpression>()

    private fun getGlobalAddressExpression(cGlobalName: String) = globalAddressExpressions.getOrPut(cGlobalName) {
        simpleBridgeGenerator.kotlinToNative(
                nativeBacked = this,
                returnType = BridgedType.NATIVE_PTR,
                kotlinValues = emptyList(),
                independent = false
        ) {
            "&$cGlobalName"
        }
    }
}