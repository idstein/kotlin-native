package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.indexer.EnumDef
import org.jetbrains.kotlin.native.interop.indexer.FunctionDecl
import org.jetbrains.kotlin.native.interop.indexer.ObjCContainer
import org.jetbrains.kotlin.native.interop.indexer.StructDecl

class StubContainerMeta(
        val textAtStart: String = "",
        val textAtEnd: String = ""
)

// TODO: Looks like it should be splitted.
// TODO: Add generic sub-containers (ex. for ObjC categories).
interface StubContainer : StubElement {
    val meta: StubContainerMeta
    val classes: List<ClassStub>
    val functions: List<FunctionalStub>
    val properties: List<PropertyStub>
    val typealiases: List<TypealiasStub>
}

val StubContainer.children: List<StubElement>
    get() = classes + functions + properties + typealiases

sealed class StubType

/**
 * Marks that abstract value of such type can be passed value.
 */
sealed class ValueStub

class TypeParameterStub(
        val name: String,
        val upperBound: StubType? = null,
        val nullable: Boolean? = false
) : StubType()

/**
 * Wrapper over [KotlinType].
 */
class WrapperStubType(
        val kotlinType: KotlinType,
        override val typeParameters: List<StubType> = emptyList()
) : StubType(), TypeParametersHolder

/**
 * Fallback variant for all cases where we cannot refer to specific [KotlinType].
 */
class SymbolicStubType(
        // TODO: use fq instead of just name.
        val name: String,
        override val typeParameters: List<StubType> = emptyList()
) : StubType(), TypeParametersHolder

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

    class Enum(val enum: EnumDef) : StubOrigin()

    class Function(val function: FunctionDecl) : StubOrigin()

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

interface TypeParametersHolder {
    val typeParameters: List<StubType>
}

sealed class AnnotationStub {
    sealed class ObjC : AnnotationStub() {
        object ConsumesReceiver : ObjC()
        object ReturnsRetained : ObjC()
        class Method(val selector: String, val encoding: String, val isStret: Boolean = false) : ObjC()
        class Factory(val selector: String, val encoding: String, val isStret: Boolean = false) : ObjC()
        object Consumed : ObjC()
        class Constructor(val selector: String, val designated: Boolean) : ObjC()
        class ExternalClass(val protocolGetter: String = "", val binaryName: String = "")
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
    enum class Qualifier {
        VAL, VAR, NONE
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
            override val functions: List<FunctionalStub> = emptyList()
    ) : ClassStub()

    class Companion(
            override val superClassInit: SuperClassInit? = null,
            override val interfaces: List<StubType> = emptyList(),
            override val properties: List<PropertyStub> = emptyList(),
            override val origin: StubOrigin = StubOrigin.None,
            override val annotations: List<AnnotationStub> = emptyList(),
            override val childrenClasses: List<ClassStub> = emptyList(),
            override val functions: List<FunctionalStub> = emptyList()
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
            override val functions: List<FunctionalStub> = emptyList()
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
        isVararg: Boolean = false
) : ValueStub(), AnnotationHolder

enum class MemberStubModality {
    OVERRIDE, OPEN, NONE, FINAL
}

// TODO: Move here common fields of functions, methods and constructors.
interface FunctionalStub : AnnotationHolder, TypeParametersHolder, StubElement {
    val parameters: List<FunctionParameterStub>
}

// TODO: Clean up hierarchy

sealed class PropertyAccessor() : FunctionalStub {

    sealed class Getter : PropertyAccessor() {
        class SimpleGetter(
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList(),
                // TODO: Unify extenal and value since they are opposite properties.
                val external: Boolean = false,
                val constant: ConstantStub? = null
        ) : Getter() {
            // Ugly test for now
            init {
                assert(external xor (constant != null))
            }
        }

        class ArrayMemberAt(
                val offset: Long,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList()
        ) : Getter()

        class MemberAt(
                val offset: Long,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList()
        ) : Getter()

        class ReadBits(
                val offset: Long,
                val size: Int,
                val signed: Boolean,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList()
        ) : Getter()
    }

    sealed class Setter : PropertyAccessor() {
        class SimpleSetter(
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList(),
                val external: Boolean = false
        ) : Setter()

        class MemberAt(
                val offset: Long,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList()
        ) : Setter()

        class WriteBits(
                val offset: Long,
                val size: Int,
                override val parameters: List<FunctionParameterStub> = emptyList(),
                override val annotations: List<AnnotationStub> = emptyList(),
                override val typeParameters: List<StubType> = emptyList()
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
        override val typeParameters: List<StubType> = emptyList()

) : StubElementWithOrigin,  FunctionalStub {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitFunction(this)
    }
}

class ConstructorStub(
        override val parameters: List<FunctionParameterStub>,
        override val annotations: List<AnnotationStub>,
        override val typeParameters: List<StubType> = emptyList()
) : FunctionalStub {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitConstructor(this)
    }
}

class EnumVariantStub(
        val name: String,
        val constant: ConstantStub
) : StubElement {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitEnumVariant(this)
    }
}

class TypealiasStub(
        val alias: StubType,
        val aliasee: StubType
) : StubElement {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitTypealias(this)
    }
}