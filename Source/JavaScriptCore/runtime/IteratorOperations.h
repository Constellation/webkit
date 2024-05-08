/*
 * Copyright (C) 2015 Yusuke Suzuki <utatane.tea@gmail.com>.
 * Copyright (C) 2016 Apple Inc. All Rights Reserved.
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

#include "IterationModeMetadata.h"
#include "JSArrayIterator.h"
#include "JSCJSValue.h"
#include "JSObjectInlines.h"
#include "JSStringIterator.h"
#include "ThrowScope.h"

namespace JSC {

struct IterationRecord {
    JSValue iterator;
    JSValue nextMethod;
};

JSValue iteratorNext(JSGlobalObject*, IterationRecord, JSValue argument = JSValue());
JS_EXPORT_PRIVATE JSValue iteratorValue(JSGlobalObject*, JSValue iterResult);
bool iteratorComplete(JSGlobalObject*, JSValue iterResult);
JS_EXPORT_PRIVATE JSValue iteratorStep(JSGlobalObject*, IterationRecord);
JS_EXPORT_PRIVATE void iteratorClose(JSGlobalObject*, JSValue iterator);
JS_EXPORT_PRIVATE JSObject* createIteratorResultObject(JSGlobalObject*, JSValue, bool done);

Structure* createIteratorResultObjectStructure(VM&, JSGlobalObject&);

JS_EXPORT_PRIVATE JSValue iteratorMethod(JSGlobalObject*, JSObject*);
JS_EXPORT_PRIVATE IterationRecord iteratorForIterable(JSGlobalObject*, JSCell*, JSValue iteratorMethod);
JS_EXPORT_PRIVATE IterationRecord iteratorForIterable(JSGlobalObject*, JSValue iterable);

JS_EXPORT_PRIVATE JSValue iteratorMethod(JSGlobalObject*, JSObject*);
JS_EXPORT_PRIVATE bool hasIteratorMethod(JSGlobalObject*, JSValue);

JS_EXPORT_PRIVATE IterationMode getIterationMode(VM&, JSGlobalObject*, JSValue iterable);
JS_EXPORT_PRIVATE IterationMode getIterationMode(VM&, JSGlobalObject*, JSValue iterable, JSValue symbolIterator);

template<typename CallBackType>
void forEachInIterable(JSGlobalObject* globalObject, JSValue iterable, const CallBackType& callback)
{
    auto& vm = getVM(globalObject);
    auto scope = DECLARE_THROW_SCOPE(vm);

    switch (getIterationMode(vm, globalObject, iterable)) {
    case IterationMode::FastArray: {
        auto* array = jsCast<JSArray*>(iterable);
        for (unsigned index = 0; index < array->length();) {
            JSValue nextValue = array->getIndex(globalObject, index);
            RETURN_IF_EXCEPTION(scope, void());
            ++index;
            callback(vm, globalObject, nextValue);
            if (UNLIKELY(scope.exception())) {
                scope.release();
                JSArrayIterator* iterator = JSArrayIterator::create(vm, globalObject->arrayIteratorStructure(), array, IterationKind::Values);
                iterator->internalField(JSArrayIterator::Field::Index).setWithoutWriteBarrier(jsNumber(index));
                iteratorClose(globalObject, iterator);
                return;
            }
        }
        return;
    }

    case IterationMode::FastString: {
        auto* stringCell = asString(iterable);
        auto string = stringCell->value(globalObject);
        RETURN_IF_EXCEPTION(scope, void());
        for (unsigned index = 0, length = string.length(); index < length;) {
            auto first = string.characterAt(index);
            JSValue nextValue;
            if (first < 0xD800 || first > 0xDBFF || index + 1 == length) {
                nextValue = jsSingleCharacterString(vm, first);
                ++index;
            } else {
                auto second = string.characterAt(index + 1);
                if (second < 0xDC00 || second > 0xDFFF) {
                    nextValue = jsSingleCharacterString(vm, first);
                    ++index;
                } else {
                    UChar buffer[] { first, second };
                    nextValue = jsNontrivialString(vm, String(std::span { buffer, 2 }));
                    index += 2;
                }
            }

            callback(vm, globalObject, nextValue);
            if (UNLIKELY(scope.exception())) {
                scope.release();
                JSStringIterator* iterator = JSStringIterator::create(vm, globalObject->stringIteratorStructure(), stringCell);
                iterator->internalField(JSStringIterator::Field::Index).setWithoutWriteBarrier(jsNumber(index));
                iteratorClose(globalObject, iterator);
                return;
            }
        }
        return;
    }

    case IterationMode::Generic:
        break;
    }

    IterationRecord iterationRecord = iteratorForIterable(globalObject, iterable);
    RETURN_IF_EXCEPTION(scope, void());
    while (true) {
        JSValue next = iteratorStep(globalObject, iterationRecord);
        if (UNLIKELY(scope.exception()) || next.isFalse())
            return;

        JSValue nextValue = iteratorValue(globalObject, next);
        RETURN_IF_EXCEPTION(scope, void());

        callback(vm, globalObject, nextValue);
        if (UNLIKELY(scope.exception())) {
            scope.release();
            iteratorClose(globalObject, iterationRecord.iterator);
            return;
        }
    }
}

template<typename CallBackType>
void forEachInIterable(JSGlobalObject& globalObject, JSCell* iterable, JSValue iteratorMethod, const CallBackType& callback)
{
    auto& vm = getVM(&globalObject);
    auto scope = DECLARE_THROW_SCOPE(vm);

    switch (getIterationMode(vm, &globalObject, iterable, iteratorMethod)) {
    case IterationMode::FastArray: {
        auto* array = jsCast<JSArray*>(iterable);
        for (unsigned index = 0; index < array->length();) {
            JSValue nextValue = array->getIndex(&globalObject, index);
            RETURN_IF_EXCEPTION(scope, void());
            ++index;
            callback(vm, globalObject, nextValue);
            if (UNLIKELY(scope.exception())) {
                scope.release();
                JSArrayIterator* iterator = JSArrayIterator::create(vm, globalObject.arrayIteratorStructure(), array, IterationKind::Values);
                iterator->internalField(JSArrayIterator::Field::Index).setWithoutWriteBarrier(jsNumber(index));
                iteratorClose(&globalObject, iterator);
                return;
            }
        }
        return;
    }

    case IterationMode::FastString: {
        auto* stringCell = asString(iterable);
        auto string = stringCell->value(&globalObject);
        RETURN_IF_EXCEPTION(scope, void());
        for (unsigned index = 0, length = string.length(); index < length;) {
            auto first = string.characterAt(index);
            JSValue nextValue;
            if (first < 0xD800 || first > 0xDBFF || index + 1 == length) {
                nextValue = jsSingleCharacterString(vm, first);
                ++index;
            } else {
                auto second = string.characterAt(index + 1);
                if (second < 0xDC00 || second > 0xDFFF) {
                    nextValue = jsSingleCharacterString(vm, first);
                    ++index;
                } else {
                    UChar buffer[] { first, second };
                    nextValue = jsNontrivialString(vm, String(std::span { buffer, 2 }));
                    index += 2;
                }
            }

            callback(vm, globalObject, nextValue);
            if (UNLIKELY(scope.exception())) {
                scope.release();
                JSStringIterator* iterator = JSStringIterator::create(vm, globalObject.stringIteratorStructure(), stringCell);
                iterator->internalField(JSStringIterator::Field::Index).setWithoutWriteBarrier(jsNumber(index));
                iteratorClose(&globalObject, iterator);
                return;
            }
        }
        return;
    }

    case IterationMode::Generic:
        break;
    }

    auto iterationRecord = iteratorForIterable(&globalObject, iterable, iteratorMethod);
    RETURN_IF_EXCEPTION(scope, void());
    while (true) {
        JSValue next = iteratorStep(&globalObject, iterationRecord);
        if (UNLIKELY(scope.exception()) || next.isFalse())
            return;

        JSValue nextValue = iteratorValue(&globalObject, next);
        RETURN_IF_EXCEPTION(scope, void());

        callback(vm, globalObject, nextValue);
        if (UNLIKELY(scope.exception())) {
            scope.release();
            iteratorClose(&globalObject, iterationRecord.iterator);
            return;
        }
    }
}

} // namespace JSC
