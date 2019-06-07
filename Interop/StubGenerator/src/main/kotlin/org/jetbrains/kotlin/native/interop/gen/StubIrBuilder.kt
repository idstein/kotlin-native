package org.jetbrains.kotlin.native.interop.gen

class StubIrContainer(
        val classses: List<ClassStub>,
        val functions: List<FunctionStub>,
        val enums: List<EnumStub>,
        val globals: List<GlobalStub>,
        val typealiases: List<TypealiasStub>
)

class StubIrBuilder() {

    val classses = mutableListOf<ClassStub>()
    val functions = mutableListOf<FunctionStub>()
    val enums = mutableListOf<EnumStub>()
    val globals = mutableListOf<GlobalStub>()
    val typealiases = mutableListOf<TypealiasStub>()

    fun build(): StubIrContainer {


        return StubIrContainer(classses, functions, enums, globals, typealiases)
    }
}