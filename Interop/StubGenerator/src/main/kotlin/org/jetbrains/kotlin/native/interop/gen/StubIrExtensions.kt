package org.jetbrains.kotlin.native.interop.gen

import org.jetbrains.kotlin.native.interop.indexer.ObjCProtocol

private val StubOrigin.ObjCMethod.isOptional: Boolean
        get() = container is ObjCProtocol && method.isOptional

fun FunctionStub.isObjCMethodOptional(): Boolean = this.origin is StubOrigin.ObjCMethod &&
        this.origin.isOptional

fun createValueStubFromValue(value: Any): ValueStub = when (value) {
        is String -> StringValueStub(value)
        is Long -> IntegralValueStub(value)
        is Double -> DoubleValueStub(value)
        else -> error("Unsupported value type: $value")
}