package org.jetbrains.kotlin.native.interop.gen

interface StubIrVisitor {

    fun visitContainer(element: StubContainer)

    fun visitClass(element: ClassStub)

    fun visitTypealias(element: TypealiasStub)

    fun visitFunction(element: FunctionStub)

    fun visitProperty(element: PropertyStub)

    fun visitEnumVariant(enumVariantStub: EnumVariantStub)

    fun visitConstructor(constructorStub: ConstructorStub)

    fun visitPropertyAccessor(propertyAccessor: PropertyAccessor)
}