package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.indexer.ObjCProtocol

private val StubOrigin.ObjCMethod.isOptional: Boolean
        get() = container is ObjCProtocol && method.isOptional

fun FunctionStub.isObjCMethodOptional(): Boolean = this.origin is StubOrigin.ObjCMethod &&
        this.origin.isOptional

fun createValueStubFromValue(value: Any): ConstantStub = when (value) {
        is String -> StringConstantStub(value)
        is Long -> IntegralConstantStub(value)
        is Double -> DoubleConstantStub(value)
        else -> error("Unsupported value type: $value")
}