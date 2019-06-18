package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.indexer.*


interface StubContainer : StubElement {
    val meta: StubContainerMeta
    val classes: List<ClassStub>
    val functions: List<FunctionalStub>
    val properties: List<PropertyStub>
    val typealiases: List<TypealiasStub>
    val simpleContainers: List<SimpleStubContainer>
}

/**
 * Meta information about [StubContainer].
 * For example, can be used for comments in textual representation.
 */
class StubContainerMeta(
        val textAtStart: String = "",
        val textAtEnd: String = ""
)


/**
 * Trivial container of IR elements that can be used
 * for module or ObjC category representation.
 */
class SimpleStubContainer(
        override val meta: StubContainerMeta = StubContainerMeta(),
        override val classes: List<ClassStub> = emptyList(),
        override val functions: List<FunctionalStub> = emptyList(),
        override val properties: List<PropertyStub> = emptyList(),
        override val typealiases: List<TypealiasStub> = emptyList(),
        override val simpleContainers: List<SimpleStubContainer> = emptyList()
) : StubContainer {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitSimpleStubContainer(this)
    }
}

val StubContainer.children: List<StubElement>
    get() {
        val classes: List<StubElement> = classes
        val funcs: List<StubElement> = functions
        return classes + funcs + properties + typealiases
    }


sealed class StubType {
    abstract val nullable: Boolean
}

/**
 * Marks that abstract value of such type can be passed as value.
 */
sealed class ValueStub

class TypeParameterStub(
        val name: String,
        val upperBound: StubType? = null
) {
    fun getStubType(nullable: Boolean): StubType =
            SymbolicStubType(name, nullable = nullable)

}

// Add variance if needed
class TypeArgument(val type: StubType)

/**
 * Wrapper over [KotlinType].
 */
class WrapperStubType(
        val kotlinType: KotlinType
) : StubType() {
    override val nullable: Boolean
        get() = when (kotlinType) {
            is KotlinClassifierType -> kotlinType.nullable
            is KotlinFunctionType -> kotlinType.nullable
            else -> error("Unknown KotlinType: $kotlinType")
        }
}

/**
 * Fallback variant for all cases where we cannot refer to specific [KotlinType].
 */
class SymbolicStubType(
        val name: String,
        val typeParameters: List<TypeParameterStub> = emptyList(),
        override val nullable: Boolean = false
) : StubType() {

    constructor(
            classifier: Classifier,
            typeParameters: List<TypeParameterStub> = emptyList(),
            nullable: Boolean = false
    ) : this(classifier.fqName, typeParameters, nullable)
}

/**
 * Represents a source of StubIr element.
 */
sealed class StubOrigin {
    /**
     * Special case when element of IR was generated.
     */
    object None : StubOrigin()

    class ObjCMethod(
            val method: org.jetbrains.kotlin.native.interop.indexer.ObjCMethod,
            val container: ObjCContainer
    ) : StubOrigin()

    class ObjCClass(
            val clazz: org.jetbrains.kotlin.native.interop.indexer.ObjCClass
    ) : StubOrigin()

    class ObjCProtocol(
            val protocol: org.jetbrains.kotlin.native.interop.indexer.ObjCProtocol
    ) : StubOrigin()

    class Enum(val enum: EnumDef) : StubOrigin()

    class Function(val function: FunctionDecl) : StubOrigin()

    class FunctionParameter(val parameter: Parameter) : StubOrigin()

    class Struct(val struct: StructDecl) : StubOrigin()
}


interface StubElement {
    fun accept(visitor: StubIrVisitor)
}

interface StubElementWithOrigin : StubElement {
    val origin: StubOrigin
}

interface AnnotationHolder {
    val annotations: List<AnnotationStub>
}

sealed class AnnotationStub {
    sealed class ObjC : AnnotationStub() {
        object ConsumesReceiver : ObjC()
        object ReturnsRetained : ObjC()
        class Method(val selector: String, val encoding: String, val isStret: Boolean = false) : ObjC()
        class Factory(val selector: String, val encoding: String, val isStret: Boolean = false) : ObjC()
        object Consumed : ObjC()
        class Constructor(val selector: String, val designated: Boolean) : ObjC()
        class ExternalClass(val protocolGetter: String = "", val binaryName: String = "") : ObjC()
    }

    sealed class CCall : AnnotationStub() {
        object CString : CCall()
        object WCString : CCall()
        class Symbol(val symbolName: String) : CCall()
    }

    class CStruct(val struct: String) : AnnotationStub()
    class CNaturalStruct(val struct: String) : AnnotationStub()

    class CLength(val length: Long): AnnotationStub()
}

/**
 * Compile-time known values.
 */
sealed class ConstantStub : ValueStub()
class StringConstantStub(val value: String) : ConstantStub()
class IntegralConstantStub(val value: Long) : ConstantStub()
class DoubleConstantStub(val value: Double) : ConstantStub()


class PropertyStub(
        val name: String,
        val type: StubType,
        val kind: Kind,
        val modality: MemberStubModality = MemberStubModality.NONE,
        val receiverType: StubType? = null,
        override val annotations: List<AnnotationStub> = emptyList()
) : StubElement, AnnotationHolder {
    sealed class Kind {
        class Val(val getter: PropertyAccessor.Getter) : Kind()
        class Var(
                val getter: PropertyAccessor.Getter,
                val setter: PropertyAccessor.Setter
        ) : Kind()

        class Constant(val constant: ConstantStub) : Kind()
    }

    override fun accept(visitor: StubIrVisitor) {
        visitor.visitProperty(this)
    }
}

enum class ClassStubModality {
    INTERFACE, OPEN, ABSTRACT, NONE
}

class ConstructorParamStub(val name: String, val type: StubType, val qualifier: Qualifier = Qualifier.NONE)
    : ValueStub() {
    sealed class Qualifier {
        class VAL(val overrides: Boolean) : Qualifier()
        class VAR(val overrides: Boolean) : Qualifier()
        object NONE : Qualifier()
    }
}

class SuperClassInit(
        val type: StubType,
        val parameters: List<ValueStub> = listOf()
)

sealed class ClassStub : StubElementWithOrigin, StubContainer, AnnotationHolder {

    abstract val superClassInit: SuperClassInit?
    abstract val interfaces: List<StubType>
    abstract val childrenClasses: List<ClassStub>
    abstract val companion : Companion?

    class Simple(
            val classifier: Classifier,
            val modality: ClassStubModality,
            val constructorParams: List<ConstructorParamStub> = emptyList(),
            override val superClassInit: SuperClassInit? = null,
            override val interfaces: List<StubType> = emptyList(),
            override val properties: List<PropertyStub> = emptyList(),
            override val origin: StubOrigin,
            override val annotations: List<AnnotationStub> = emptyList(),
            override val childrenClasses: List<ClassStub> = emptyList(),
            override val companion: Companion? = null,
            override val functions: List<FunctionalStub> = emptyList(),
            override val simpleContainers: List<SimpleStubContainer> = emptyList()
    ) : ClassStub()

    class Companion(
            override val superClassInit: SuperClassInit? = null,
            override val interfaces: List<StubType> = emptyList(),
            override val properties: List<PropertyStub> = emptyList(),
            override val origin: StubOrigin = StubOrigin.None,
            override val annotations: List<AnnotationStub> = emptyList(),
            override val childrenClasses: List<ClassStub> = emptyList(),
            override val functions: List<FunctionalStub> = emptyList(),
            override val simpleContainers: List<SimpleStubContainer> = emptyList()
    ) : ClassStub() {
        override val companion: Companion? = null
    }

    class Enum(
            val classifier: Classifier,
            val variants: List<EnumVariantStub>,
            val constructorParams: List<ConstructorParamStub> = emptyList(),
            override val superClassInit: SuperClassInit? = null,
            override val interfaces: List<StubType> = emptyList(),
            override val properties: List<PropertyStub> = emptyList(),
            override val origin: StubOrigin,
            override val annotations: List<AnnotationStub> = emptyList(),
            override val childrenClasses: List<ClassStub> = emptyList(),
            override val companion: Companion?= null,
            override val functions: List<FunctionalStub> = emptyList(),
            override val simpleContainers: List<SimpleStubContainer> = emptyList()
    ) : ClassStub()

    override val meta: StubContainerMeta = StubContainerMeta()

    override val classes: List<ClassStub>
        get() = childrenClasses + listOfNotNull(companion)

    override fun accept(visitor: StubIrVisitor) {
        visitor.visitClass(this)
    }

    override val typealiases: List<TypealiasStub> = emptyList()
}

class FunctionParameterStub(
        val name: String,
        val type: StubType,
        override val annotations: List<AnnotationStub> = emptyList(),
        val isVararg: Boolean = false,
        val origin: StubOrigin = StubOrigin.None
) : ValueStub(), AnnotationHolder

enum class MemberStubModality {
    OVERRIDE, OPEN, NONE, FINAL
}

interface FunctionalStub : AnnotationHolder, StubElement {
    val parameters: List<FunctionParameterStub>
}

sealed class PropertyAccessor : FunctionalStub {

    sealed class Getter : PropertyAccessor() {
        class SimpleGetter(
                override val annotations: List<AnnotationStub> = emptyList(),
                val constant: ConstantStub? = null
        ) : Getter() {
            override val parameters: List<FunctionParameterStub> = emptyList()
        }

        class ExternalGetter(
                override val annotations: List<AnnotationStub> = emptyList()
        ) : Getter() {
            override val parameters: List<FunctionParameterStub> = emptyList()
        }

        class ArrayMemberAt(
                val offset: Long
        ) : Getter() {
            override val parameters: List<FunctionParameterStub> = emptyList()
            override val annotations: List<AnnotationStub> = emptyList()
        }

        class MemberAt(
                val offset: Long,
                val typeArguments: List<TypeArgument> = emptyList()
        ) : Getter() {
            override val parameters: List<FunctionParameterStub> = emptyList()
            override val annotations: List<AnnotationStub> = emptyList()
        }

        class ReadBits(
                val offset: Long,
                val size: Int,
                val signed: Boolean,
                val rawType: BridgedType
        ) : Getter() {
            override val parameters: List<FunctionParameterStub> = emptyList()
            override val annotations: List<AnnotationStub> = emptyList()
        }

        class InterpretPointed(val cGlobalName:String, pointedType: WrapperStubType) : Getter() {
            override val parameters: List<FunctionParameterStub> = emptyList()
            override val annotations: List<AnnotationStub> = emptyList()
            val typeParameters: List<StubType> = listOf(pointedType)
        }
    }

    sealed class Setter : PropertyAccessor() {
        class SimpleSetter(
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList()
        ) : Setter()

        class ExternalSetter(
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList()
        ) : Setter()

        class MemberAt(
                val offset: Long,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                val typeArguments: List<TypeArgument> = emptyList()
        ) : Setter()

        class WriteBits(
                val offset: Long,
                val size: Int,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList()
        ) : Setter()
    }

    override fun accept(visitor: StubIrVisitor) {
        visitor.visitPropertyAccessor(this)
    }
}

class FunctionStub(
        val name: String,
        val returnType: StubType,
        override val parameters: List<FunctionParameterStub>,
        override val origin: StubOrigin,
        override val annotations: List<AnnotationStub>,
        val external: Boolean = false,
        val receiverType: StubType?,
        val modality: MemberStubModality,
        val typeParameters: List<TypeParameterStub> = emptyList()
) : StubElementWithOrigin, FunctionalStub {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitFunction(this)
    }
}

class ConstructorStub(
        override val parameters: List<FunctionParameterStub>,
        override val annotations: List<AnnotationStub>
) : FunctionalStub {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitConstructor(this)
    }
}

class EnumVariantStub(
        val name: String,
        val constant: IntegralConstantStub
)

class TypealiasStub(
        val alias: StubType,
        val aliasee: StubType
) : StubElement {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitTypealias(this)
    }
}