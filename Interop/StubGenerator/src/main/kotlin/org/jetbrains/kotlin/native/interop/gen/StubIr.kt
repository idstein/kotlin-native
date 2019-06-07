package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.indexer.EnumDef
import org.jetbrains.kotlin.native.interop.indexer.ObjCContainer

class StubContainerMeta(
        val textAtStart: String = "",
        val textAtEnd: String = ""
)

// TODO: Looks like it should be splitted.
interface StubContainer {
    val meta: StubContainerMeta
    val classes: List<ClassStub>
    val functions: List<FunctionalStub>
    val enums: List<EnumStub>
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
        val parameters: List<TypeParameterStub> = emptyList(),
        override val typeParameters: List<TypeParameterStub> = emptyList()
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
}


interface StubElement

interface StubElementWithOrigin : StubElement {
    val origin: StubOrigin
}

interface AnnotationHolder {
    val annotations: List<AnnotationStub>
}

interface TypeParametersHolder {
    val typeParameters: List<TypeParameterStub>
}

sealed class AnnotationStub : StubElement {
    sealed class ObjC : AnnotationStub() {
        object ConsumesReceiver : ObjC()
        object ReturnsRetained : ObjC()
        class Method(val selector: String, val encoding: String, val isStret: Boolean = false) : ObjC()
        class Factory(val selector: String, val encoding: String, val isStret: Boolean = false) : ObjC()
        object Consumed : ObjC()
        class Constructor(val selector: String, val designated: Boolean) : ObjC()
        class ExternalClass(val protocolGetter: String = "", val binaryName: String = "")
    }
}

sealed class ValueStub
class StringValueStub(val value: String) : ValueStub()
class IntegralValueStub(val value: Long) : ValueStub()
class DoubleValueStub(val value: Double) : ValueStub()

class PropertyStub(
        val name: String,
        val type: StubType,
        val kind: Kind,
        val modality: MemberStubModality,
        val receiverType: StubType?
) : StubElement {
    sealed class Kind {
        class Val(val getter: PropertyAccessor.Getter) : Kind()
        class Var(
                val getter: PropertyAccessor.Getter,
                val setter: PropertyAccessor.Setter
        ) : Kind()
        class Constant(val value: ValueStub) : Kind()
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
        val superTypes: List<StubType>,
        val companion : CompanionStub? = null, // TODO: add to classes
        override val classes: List<ClassStub> = emptyList(),
        override val functions: List<FunctionalStub> = emptyList(),
        override val meta: StubContainerMeta = StubContainerMeta()
) : StubElementWithOrigin, StubContainer {
    override val typealiases: List<TypealiasStub> = emptyList()
    override val enums: List<EnumStub> = emptyList()
}

class CompanionStub(
        val superTypes: List<StubType> = emptyList(),
        override val properties: List<PropertyStub> = emptyList(),
        override val functions: List<FunctionalStub> = emptyList(),
        override val meta: StubContainerMeta = StubContainerMeta()
) : StubElement, StubContainer {
    override val classes: List<ClassStub> = emptyList()
    override val typealiases: List<TypealiasStub> = emptyList()
    override val enums: List<EnumStub> = emptyList()
}

class FunctionParameterStub(
        val name: String,
        val type: StubType,
        override val annotations: List<AnnotationStub>,
        isVararg: Boolean = false
): StubElement, AnnotationHolder

enum class MemberStubModality {
    OVERRIDE, OPEN, NONE, FINAL
}

// TODO: Move here common fields of functions, methods and constructors.
interface FunctionalStub : AnnotationHolder, TypeParametersHolder, StubElement {
    val parameters: List<FunctionParameterStub>
}

sealed class PropertyAccessor() : FunctionalStub {
    class Getter(
            override val parameters: List<FunctionParameterStub> = emptyList(),
            override val annotations: List<AnnotationStub> = emptyList(),
            override val typeParameters: List<TypeParameterStub> = emptyList(),
            // TODO: Unify extenal and value since they are opposite properties.
            val external: Boolean = false,
            val value: ValueStub? = null
    ) : PropertyAccessor() {
        // Ugly test for now
        init {
            assert(external xor (value != null))
        }
    }
    class Setter(
            override val parameters: List<FunctionParameterStub> = emptyList(),
            override val annotations: List<AnnotationStub> = emptyList(),
            override val typeParameters: List<TypeParameterStub> = emptyList(),
            val external: Boolean = false
    ) : PropertyAccessor()
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
        override val typeParameters: List<TypeParameterStub> = emptyList()

) : StubElementWithOrigin,  FunctionalStub

class ConstructorStub(
        override val parameters: List<FunctionParameterStub>,
        override val annotations: List<AnnotationStub>,
        override val typeParameters: List<TypeParameterStub> = emptyList()
) : FunctionalStub

class EnumVariantStub(
        val name: String,
        val value: ValueStub
) : StubElement

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
) : StubElement