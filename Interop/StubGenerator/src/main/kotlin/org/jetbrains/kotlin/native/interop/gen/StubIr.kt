package org.jetbrains.kotlin.native.interop.gen

interface StubType

/**
 * Represents a source of StubIr element.
 */
interface StubOrigin

/**
 * Special case when element of IR was generated.
 */
object NoneOrigin : StubOrigin

interface StubElement

interface StubElementWithOrigin : StubElement {
    val origin: StubOrigin
}

interface AnnotationHolder {
    val annotations: List<AnnotationStub>
}

class AnnotationStub : StubElement

class ClassStub(
        val name: String,
        override val origin: StubOrigin
) : StubElementWithOrigin

class GlobalStub(
        val name: String,
        override val origin: StubOrigin
) : StubElementWithOrigin

class FunctionParameterStub(
        val name: String,
        val type: StubType
)

class FunctionStub(
        val name: String,
        val returnType: StubType,
        val parameters: List<FunctionParameterStub>,
        override val origin: StubOrigin
) : StubElementWithOrigin

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