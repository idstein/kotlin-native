package org.jetbrains.kotlin.native.interop.gen

interface StubIrVisitor {

    fun visitClass(element: ClassStub)

    fun visitTypealias(element: TypealiasStub)

    fun visitFunction(element: FunctionStub)

    fun visitProperty(element: PropertyStub)

    fun visitConstructor(constructorStub: ConstructorStub)

    fun visitPropertyAccessor(propertyAccessor: PropertyAccessor)

    fun visitSimpleStubContainer(simpleStubContainer: SimpleStubContainer)
}

interface StubIrVisitorAux<T> {

    fun visitClass(element: ClassStub, aux: T)

    fun visitTypealias(element: TypealiasStub, aux: T)

    fun visitFunction(element: FunctionStub, aux: T)

    fun visitProperty(element: PropertyStub, aux: T)

    fun visitConstructor(constructorStub: ConstructorStub, aux: T)

    fun visitPropertyAccessor(propertyAccessor: PropertyAccessor, aux: T)

    fun visitSimpleStubContainer(simpleStubContainer: SimpleStubContainer, aux: T)
}