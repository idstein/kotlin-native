package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.gen.jvm.InteropConfiguration
import org.jetbrains.kotlin.native.interop.gen.jvm.KotlinPlatform
import org.jetbrains.kotlin.native.interop.indexer.*

/**
 * Additional components that are required to generate bridges.
 */
interface BridgeGenerationComponents {
    class GlobalSetterBridgeInfo(
            val cGlobalName: String,
            val typeInfo: TypeInfo
    )

    class GlobalGetterBridgeInfo(
            val cGlobalName: String,
            val typeInfo: TypeInfo,
            val isArray: Boolean
    )

    val setterToBridgeInfo: Map<PropertyAccessor.Setter.SimpleSetter, GlobalSetterBridgeInfo>

    val getterToBridgeInfo: Map<PropertyAccessor.Getter.SimpleGetter, GlobalGetterBridgeInfo>

    val enumToTypeMirror: Map<ClassStub.Enum, TypeMirror>
}

class BridgeGenerationComponentsBuilder(
        val getterToBridgeInfo: MutableMap<PropertyAccessor.Getter.SimpleGetter, BridgeGenerationComponents.GlobalGetterBridgeInfo> = mutableMapOf(),
        val setterToBridgeInfo: MutableMap<PropertyAccessor.Setter.SimpleSetter, BridgeGenerationComponents.GlobalSetterBridgeInfo> = mutableMapOf(),
        val enumToTypeMirror: MutableMap<ClassStub.Enum, TypeMirror> = mutableMapOf()
) {
    fun build(): BridgeGenerationComponents = object : BridgeGenerationComponents {
        override val getterToBridgeInfo = this@BridgeGenerationComponentsBuilder.getterToBridgeInfo

        override val setterToBridgeInfo = this@BridgeGenerationComponentsBuilder.setterToBridgeInfo

        override val enumToTypeMirror = this@BridgeGenerationComponentsBuilder.enumToTypeMirror
    }
}

/**
 * Common part of all [StubIrBuilder] implementations.
 */
interface StubsBuildingContext {
    val configuration: InteropConfiguration

    fun mirror(type: Type): TypeMirror

    val declarationMapper: DeclarationMapper

    fun generateNextUniqueId(prefix: String): String

    val generatedObjCCategoriesMembers: MutableMap<ObjCClass, GeneratedObjCCategoriesMembers>

    val platform: KotlinPlatform

    fun isStrictEnum(enumDef: EnumDef): Boolean

    val macroConstantsByName: Map<String, MacroDef>

    fun tryCreateIntegralStub(type: Type, value: Long): IntegralConstantStub?

    fun tryCreateDoubleStub(type: Type, value: Double): DoubleConstantStub?

    val bridgeComponentsBuilder: BridgeGenerationComponentsBuilder
}

/**
 *
 */
internal interface StubElementBuilder {
    val context: StubsBuildingContext

    fun build(): List<StubElement>
}

class StubsBuildingContextImpl(
        override val configuration: InteropConfiguration,
        override val platform: KotlinPlatform,
        val imports: Imports,
        private val nativeIndex: NativeIndex
) : StubsBuildingContext {

    private val forbiddenStructNames = run {
        val typedefNames = nativeIndex.typedefs.map { it.name }
        typedefNames.toSet()
    }

    private var theCounter = 0

    override fun generateNextUniqueId(prefix: String) =
            prefix + pkgName.replace('.', '_') + theCounter++

    override fun mirror(type: Type): TypeMirror = mirror(declarationMapper, type)

    /**
     * Indicates whether this enum should be represented as Kotlin enum.
     */

    override fun isStrictEnum(enumDef: EnumDef): Boolean = with(enumDef) {
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

    override val generatedObjCCategoriesMembers = mutableMapOf<ObjCClass, GeneratedObjCCategoriesMembers>()

    override val declarationMapper = object : DeclarationMapper {
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

        override fun isMappedToStrict(enumDef: EnumDef): Boolean = isStrictEnum(enumDef)

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

    override val macroConstantsByName: Map<String, MacroDef> =
            (nativeIndex.macroConstants + nativeIndex.wrappedMacros).associateBy { it.name }

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

    /**
     * Finds all names to be declared for the given type declaration,
     * and adds them to [result].
     *
     * TODO: refactor to compute these names directly from declarations.
     */
    private fun getTypeDeclaringNames(type: Type, result: MutableList<String>) {
        if (type.unwrapTypedefs() == VoidType) {
            return
        }

        val mirror = mirror(type)
        val varClassifier = mirror.pointedType.classifier
        if (varClassifier.pkg == pkgName) {
            result.add(varClassifier.topLevelName)
        }
        when (mirror) {
            is TypeMirror.ByValue -> {
                val valueClassifier = mirror.valueType.classifier
                if (valueClassifier.pkg == pkgName && valueClassifier.topLevelName != varClassifier.topLevelName) {
                    result.add(valueClassifier.topLevelName)
                }
            }
            is TypeMirror.ByRef -> {}
        }
    }

    fun computeNamesToBeDeclared(): MutableList<String> =
            mutableListOf<String>().apply {
                nativeIndex.typedefs.forEach {
                    getTypeDeclaringNames(Typedef(it), this)
                }

                nativeIndex.objCProtocols.forEach {
                    add(it.kotlinClassName(isMeta = false))
                    add(it.kotlinClassName(isMeta = true))
                }

                nativeIndex.objCClasses.forEach {
                    add(it.kotlinClassName(isMeta = false))
                    add(it.kotlinClassName(isMeta = true))
                }

                nativeIndex.structs.forEach {
                    getTypeDeclaringNames(RecordType(it), this)
                }

                nativeIndex.enums.forEach {
                    if (!it.isAnonymous) {
                        getTypeDeclaringNames(EnumType(it), this)
                    }
                }
            }

    // TODO: make it more robust
    override fun tryCreateIntegralStub(type: Type, value: Long): IntegralConstantStub? {
        val integerType = type.unwrapTypedefs() as? IntegerType ?: return null
        return IntegralConstantStub(value)
    }

    override fun tryCreateDoubleStub(type: Type, value: Double): DoubleConstantStub? {
        val unwrappedType = type.unwrapTypedefs() as? FloatingType ?: return null
        return DoubleConstantStub(value)
    }

    override val bridgeComponentsBuilder = BridgeGenerationComponentsBuilder()
}

data class StubIrBuilderResult(
        val stubs: SimpleStubContainer,
        val declarationMapper: DeclarationMapper,
        val namesToBeDeclared: List<String>,
        val bridgeGenerationComponents: BridgeGenerationComponents
)

/**
 * Produces [StubIrBuilderResult] for given [KotlinPlatform] using [InteropConfiguration].
 */
class StubIrBuilder(
        private val configuration: InteropConfiguration,
        platform: KotlinPlatform,
        private val nativeIndex: NativeIndex,
        imports: Imports
) {

    private val classes = mutableListOf<ClassStub>()
    private val functions = mutableListOf<FunctionalStub>()
    private val globals = mutableListOf<PropertyStub>()
    private val typealiases = mutableListOf<TypealiasStub>()
    private val containers = mutableListOf<SimpleStubContainer>()

    private fun addStubs(stubs: List<StubElement>) = stubs.forEach(this::addStub)

    private fun addStub(stub: StubElement) {
        when(stub) {
            is ClassStub -> classes += stub
            is FunctionalStub -> functions += stub
            is PropertyStub -> globals += stub
            is TypealiasStub -> typealiases += stub
            is SimpleStubContainer -> containers += stub
            else -> error("Unexpected stub: $stub")
        }
    }

    private val excludedFunctions: Set<String>
        get() = configuration.excludedFunctions

    private val excludedMacros: Set<String>
        get() = configuration.excludedMacros

    private val buildingContext = StubsBuildingContextImpl(configuration, platform, imports, nativeIndex)

    fun build(): StubIrBuilderResult {
        nativeIndex.objCProtocols.filter { !it.isForwardDeclaration }.forEach { generateStubsForObjCProtocol(it) }
        nativeIndex.objCClasses.filter { !it.isForwardDeclaration && !it.isNSStringSubclass()} .forEach { generateStubsForObjCClass(it) }
        nativeIndex.objCCategories.filter { !it.clazz.isNSStringSubclass() }.forEach { generateStubsForObjCCategory(it) }
        nativeIndex.typedefs.forEach { generateStubsForTypedef(it) }
        nativeIndex.globals.filter { it.name !in excludedFunctions }.forEach { generateStubsForGlobal(it) }
        nativeIndex.enums.forEach { generateStubsForEnum(it) }
        nativeIndex.structs.forEach { generateStubsForStruct(it) }
        nativeIndex.functions.forEach { generateStubsForFunction(it) }
        nativeIndex.macroConstants.filter { it.name !in excludedMacros }.forEach { generateStubsForMacroConstant(it) }
        nativeIndex.wrappedMacros.filter { it.name !in excludedMacros }.forEach { generateStubsForWrappedMacro(it) }

        val meta = StubContainerMeta()
        return StubIrBuilderResult(
                SimpleStubContainer(meta, classes, functions, globals, typealiases, containers),
                buildingContext.declarationMapper,
                buildingContext.computeNamesToBeDeclared(),
                buildingContext.bridgeComponentsBuilder.build()
        )
    }

    private fun generateStubsForWrappedMacro(macro: WrappedMacroDef) {
        generateStubsForGlobal(GlobalDecl(macro.name, macro.type, isConst = true))
    }

    private fun generateStubsForMacroConstant(constant: ConstantDef) {
        addStubs(MacroConstantStubBuilder(buildingContext, constant).build())
    }

    private fun generateStubsForEnum(enumDef: EnumDef) {
        addStubs(EnumStubBuilder(buildingContext, enumDef).build())
    }

    private fun generateStubsForFunction(func: FunctionDecl) {
        addStubs(FunctionStubBuilder(buildingContext, func).build())
    }

    private fun generateStubsForStruct(decl: StructDecl) {
        addStubs(StructStubBuilder(buildingContext, decl).build())
    }

    private fun generateStubsForTypedef(typedefDef: TypedefDef) {
        addStubs(TypedefStubBuilder(buildingContext, typedefDef).build())
    }

    private fun generateStubsForGlobal(global: GlobalDecl) {
        addStubs(GlobalStubBuilder(buildingContext, global).build())
    }

    private fun generateStubsForObjCProtocol(objCProtocol: ObjCProtocol) {
        addStubs(ObjCProtocolStubBuilder(buildingContext, objCProtocol).build())
    }

    private fun generateStubsForObjCClass(objCClass: ObjCClass) {
        addStubs(ObjCClassStubBuilder(buildingContext, objCClass).build())
    }

    private fun generateStubsForObjCCategory(objCCategory: ObjCCategory) {
        addStubs(ObjCCategoryStubBuilder(buildingContext, objCCategory).build())
    }
}