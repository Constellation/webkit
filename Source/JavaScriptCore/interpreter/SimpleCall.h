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

#include "CallLinkInfoBase.h"
#include "ExceptionHelpers.h"
#include "JSFunction.h"
#include "Interpreter.h"
#include "ProtoCallFrameInlines.h"
#include "VMEntryScope.h"
#include "VMInlines.h"
#include <wtf/ForbidHeapAllocation.h>
#include <wtf/Scope.h>

namespace JSC {

class SimpleCall final : public CallLinkInfoBase {
    WTF_MAKE_NONCOPYABLE(SimpleCall);
public:
    SimpleCall(JSGlobalObject* globalObject, JSObject* function, int argumentCount)
        : CallLinkInfoBase(CallSiteType::SimpleCall)
    {
        VM& vm = globalObject->vm();
        vm.interpreter.prepareForSimpleCall(*this, function);
    }

    ~SimpleCall()
    {
        m_addressForCall = nullptr;
    }

    ALWAYS_INLINE JSValue call(JSGlobalObject* globalObject, JSObject* function, JSValue thisValue, const ArgList& args, NakedPtr<Exception>& returnedException)
    {
        return vm.interpreter.executeSimpleCall(globalObject, *this, function, thisValue, args, returnedException);
    }

    void unlinkOrUpgradeImpl(VM&, CodeBlock*, CodeBlock*)
    {
        if (isOnList())
            remove();
        m_codeBlock = nullptr;
        m_addressForCall = nullptr;
    }

    const CallData& callData() const { return m_callData; }
    CodeBlock* codeBlock() const { return m_codeBlock; }
    void* addressForCall() const { return m_addressForCall; }

private:
    CallData m_callData { };
    CodeBlock* m_codeBlock { nullptr };
    void* m_addressForCall { nullptr };
    friend class Interpreter;
};

} // namespace JSC
