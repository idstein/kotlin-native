package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.indexer.ObjCContainer

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

class ClassifierStubType(
        val classifier: Classifier,
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

    }
}

class PropertyStub(
        val name: String,
        val type: StubType
) : StubElement

class ClassStub(
        val name: String,
        override val origin: StubOrigin,
        val fields: List<PropertyStub>
) : StubElementWithOrigin

class GlobalStub(
        val name: String,
        override val origin: StubOrigin
) : StubElementWithOrigin

class FunctionParameterStub(
        val name: String,
        val type: StubType,
        override val annotations: List<AnnotationStub>,
        isVararg: Boolean = false
): StubElement, AnnotationHolder

enum class FunctionStubModality {
    OVERRIDE, OPEN, NONE
}

// TODO: Move here common fields of functions, methods and constructors.
interface FunctionalStub : AnnotationHolder, TypeParametersHolder {
    val parameters: List<FunctionParameterStub>
}

class FunctionStub(
        val name: String,
        val returnType: StubType,
        override val parameters: List<FunctionParameterStub>,
        override val origin: StubOrigin,
        override val annotations: List<AnnotationStub>,
        val external: Boolean = false,
        val receiverType: StubType?,
        val modality: FunctionStubModality,
        override val typeParameters: List<TypeParameterStub> = emptyList()

) : StubElementWithOrigin,  FunctionalStub

class ConstructorStub(
        override val parameters: List<FunctionParameterStub>,
        override val annotations: List<AnnotationStub>,
        override val typeParameters: List<TypeParameterStub> = emptyList()
) : FunctionalStub

class EnumElementStub(
        val name: String
) : StubElement

class EnumStub(
        val name: String,
        val elements: List<EnumElementStub>,
        override val origin: StubOrigin
) : StubElementWithOrigin

class TypealiasStub(
        val name: String
) : StubElement