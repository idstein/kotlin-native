package org.jetbrains.kotlin.native.interop.gen

class StubIrTextEmitter(
        private val stubs: TopLevelContainer
) {
    fun emit() {

    }

    private val printer = object : StubIrVisitor {
        override fun visit(element: StubElement) {
            TODO("not implemented")
        }

        override fun visitContainer(element: StubContainer) {
            TODO("not implemented")
        }

        override fun visitClass(element: ClassStub) {
            TODO("not implemented")
        }

        override fun visitCompanion(element: CompanionStub) {
            TODO("not implemented")
        }

        override fun visitTypealias(element: TypealiasStub) {
            TODO("not implemented")
        }

        override fun visitFunction(element: FunctionStub) {
            TODO("not implemented")
        }

        override fun visitProperty(element: PropertyStub) {
            TODO("not implemented")
        }

        override fun visitEnumVariant(enumVariantStub: EnumVariantStub) {
            TODO("not implemented")
        }

        override fun visitConstructor(constructorStub: ConstructorStub) {
            TODO("not implemented")
        }

        override fun visitPropertyAccessor(propertyAccessor: PropertyAccessor) {
            TODO("not implemented")
        }
    }
}