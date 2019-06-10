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
interface StubContainer {
    val meta: StubContainerMeta
    val classes: List<ClassStub>
    val functions: List<FunctionalStub>
    val properties: List<PropertyStub>
    val typealiases: List<TypealiasStub>
}

interface StubType {
}

class TypeParameterStub(
        val name: String,
        val upperBound: StubType? = null,
        val nullable: Boolean? = false
) : StubType

/**
 * Wrapper over [KotlinType].
 */
class WrapperStubType(
        val kotlinType: KotlinType,
        override val typeParameters: List<StubType> = emptyList()
) : StubType, TypeParametersHolder

/**
 * Fallback variant for all cases where we cannot refer to specific [KotlinType].
 */
class SymbolicStubType(
        val name: String,
        override val typeParameters: List<StubType> = emptyList()
) : StubType, TypeParametersHolder

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
sealed class ValueStub
class StringValueStub(val value: String) : ValueStub()
class IntegralValueStub(val value: Long) : ValueStub()
class DoubleValueStub(val value: Double) : ValueStub()


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

        class Constant(val value: ValueStub) : Kind()
    }

    override fun accept(visitor: StubIrVisitor) {
        visitor.visitProperty(this)
    }
}

enum class ClassStubModality {
    INTERFACE, OPEN, ABSTRACT, NONE
}

// TODO: Separate into class and interface
open class ClassStub(
        val classifier: Classifier,
        override val origin: StubOrigin,
        override val properties: List<PropertyStub>,
        val methods: List<FunctionalStub>,
        val modality: ClassStubModality,
        // TODO: Split into superClass and interfaces
        val superTypes: List<StubType>,
        val companion : CompanionStub? = null, // TODO: add to classes
        override val classes: List<ClassStub> = emptyList(),
        override val functions: List<FunctionalStub> = emptyList(),
        override val meta: StubContainerMeta = StubContainerMeta(),
        override val annotations: List<AnnotationStub> = emptyList()
) : StubElementWithOrigin, StubContainer, AnnotationHolder {
    override val typealiases: List<TypealiasStub> = emptyList()

    override fun accept(visitor: StubIrVisitor) {
        visitor.visitClass(this)
    }
}

class CompanionStub(
        val superTypes: List<StubType> = emptyList(),
        override val properties: List<PropertyStub> = emptyList(),
        override val functions: List<FunctionalStub> = emptyList(),
        override val meta: StubContainerMeta = StubContainerMeta()
) : StubElement, StubContainer {
    override val classes: List<ClassStub> = emptyList()
    override val typealiases: List<TypealiasStub> = emptyList()

    override fun accept(visitor: StubIrVisitor) {
        visitor.visitCompanion(this)
    }
}

class FunctionParameterStub(
        val name: String,
        val type: StubType,
        override val annotations: List<AnnotationStub> = emptyList(),
        isVararg: Boolean = false
) : AnnotationHolder

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
                val value: ValueStub? = null
        ) : Getter() {
            // Ugly test for now
            init {
                assert(external xor (value != null))
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
        val value: ValueStub
) : StubElement {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitEnumVariant(this)
    }
}

class EnumStub(
        classifier: Classifier,
        val variants: List<EnumVariantStub>,
        origin: StubOrigin,
        properties: List<PropertyStub> = emptyList(),
        methods: List<FunctionalStub> = emptyList(),
        modality: ClassStubModality = ClassStubModality.NONE,
        superTypes: List<StubType> = emptyList(),
        companion: CompanionStub? = null
) : ClassStub(classifier, origin, properties, methods, modality, superTypes, companion)

class TypealiasStub(
        val alias: StubType,
        val aliasee: StubType
) : StubElement {
    override fun accept(visitor: StubIrVisitor) {
        visitor.visitTypealias(this)
    }
}