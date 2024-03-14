/*
 * Copyright (C) 2009-2023 Apple Inc. All rights reserved.
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

#include "CallData.h"
#include "CallLinkInfoBase.h"
#include "JSFunction.h"
#include "Interpreter.h"
#include "VMInlines.h"

namespace JSC {

class JSScope;

class SimpleCall final : public CallLinkInfoBase {
    WTF_MAKE_NONCOPYABLE(SimpleCall);
public:
    SimpleCall()
        : CallLinkInfoBase(CallSiteType::SimpleCall)
    {
    }

    ~SimpleCall()
    {
        clear();
    }

    ALWAYS_INLINE JSValue call(VM& vm, JSObject* function, JSValue thisValue, const ArgList& args, NakedPtr<Exception>& returnedException)
    {
        auto scope = DECLARE_CATCH_SCOPE(vm);
        JSValue result = vm.interpreter.executeSimpleCall(*this, function, thisValue, args);
        if (UNLIKELY(scope.exception())) {
            returnedException = scope.exception();
            scope.clearException();
            return jsUndefined();
        }
        return result;
    }

    void finalizeUnconditionally(VM& vm)
    {
        if (m_globalObject && !vm.heap.isMarked(m_globalObject)) {
            unlinkOrUpgradeImpl(vm, nullptr, nullptr);
            return;
        }
        if (m_codeBlock && !vm.heap.isMarked(m_codeBlock)) {
            unlinkOrUpgradeImpl(vm, nullptr, nullptr);
            return;
        }
    }

    void clear()
    {
        if (isOnList())
            remove();
        m_globalObject = nullptr;
        m_codeBlock = nullptr;
        m_addressForCall = nullptr;
        m_nativeFunction = { };
    }

    void unlinkOrUpgradeImpl(VM&, CodeBlock*, CodeBlock*)
    {
        clear();
    }

    JSGlobalObject* globalObject() const { return m_globalObject; }
    CodeBlock* codeBlock() const { return m_codeBlock; }
    void* addressForCall() const { return m_addressForCall; }
    TaggedNativeFunction nativeFunction() const { return m_nativeFunction; };

private:
    JSGlobalObject* m_globalObject { nullptr };
    CodeBlock* m_codeBlock { nullptr };
    void* m_addressForCall { nullptr };
    TaggedNativeFunction m_nativeFunction { };
    friend class Interpreter;
};

} // namespace JSC
