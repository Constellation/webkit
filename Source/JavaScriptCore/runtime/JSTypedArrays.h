/*
 * Copyright (C) 2013 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */

#pragma once

#include "JSGenericTypedArrayView.h"
#include "TypedArrayAdaptors.h"

namespace JSC {

using JSInt8Array = JSGenericTypedArrayView<Int8Adaptor>;
using JSInt16Array = JSGenericTypedArrayView<Int16Adaptor>;
using JSInt32Array = JSGenericTypedArrayView<Int32Adaptor>;
using JSUint8Array = JSGenericTypedArrayView<Uint8Adaptor>;
using JSUint8ClampedArray = JSGenericTypedArrayView<Uint8ClampedAdaptor>;
using JSUint16Array = JSGenericTypedArrayView<Uint16Adaptor>;
using JSUint32Array = JSGenericTypedArrayView<Uint32Adaptor>;
using JSFloat32Array = JSGenericTypedArrayView<Float32Adaptor>;
using JSFloat64Array = JSGenericTypedArrayView<Float64Adaptor>;
using JSBigInt64Array = JSGenericTypedArrayView<BigInt64Adaptor>;
using JSBigUint64Array = JSGenericTypedArrayView<BigUint64Adaptor>;
using JSGrowableSharedInt8Array = JSGenericGrowableSharedTypedArrayView<Int8Adaptor>;
using JSGrowableSharedInt16Array = JSGenericGrowableSharedTypedArrayView<Int16Adaptor>;
using JSGrowableSharedInt32Array = JSGenericGrowableSharedTypedArrayView<Int32Adaptor>;
using JSGrowableSharedUint8Array = JSGenericGrowableSharedTypedArrayView<Uint8Adaptor>;
using JSGrowableSharedUint8ClampedArray = JSGenericGrowableSharedTypedArrayView<Uint8ClampedAdaptor>;
using JSGrowableSharedUint16Array = JSGenericGrowableSharedTypedArrayView<Uint16Adaptor>;
using JSGrowableSharedUint32Array = JSGenericGrowableSharedTypedArrayView<Uint32Adaptor>;
using JSGrowableSharedFloat32Array = JSGenericGrowableSharedTypedArrayView<Float32Adaptor>;
using JSGrowableSharedFloat64Array = JSGenericGrowableSharedTypedArrayView<Float64Adaptor>;
using JSGrowableSharedBigInt64Array = JSGenericGrowableSharedTypedArrayView<BigInt64Adaptor>;
using JSGrowableSharedBigUint64Array = JSGenericGrowableSharedTypedArrayView<BigUint64Adaptor>;

inline bool isGrowableSharedTypedArray(const ClassInfo* classInfo)
{
#define JSC_TYPED_ARRAY_CHECK(type) do { \
    if (classInfo == JSGrowableShared ## type ## Array::info()) \
        return true; \
    } while (0);
    FOR_EACH_TYPED_ARRAY_TYPE_EXCLUDING_DATA_VIEW(JSC_TYPED_ARRAY_CHECK)
#undef JSC_TYPED_ARRAY_CHECK
    return false;
}

JS_EXPORT_PRIVATE JSUint8Array* createUint8TypedArray(JSGlobalObject*, Structure*, RefPtr<ArrayBuffer>&&, unsigned byteOffset, unsigned length);

} // namespace JSC
