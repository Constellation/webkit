/*
 * Copyright (C) 2015-2016 Apple Inc. All rights reserved.
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

#if ENABLE(B3_JIT)

#include "B3Effects.h"
#include "B3Value.h"

WTF_ALLOW_UNSAFE_BUFFER_USAGE_BEGIN

namespace JSC { namespace B3 {

class JS_EXPORT_PRIVATE CCallValue final : public Value {
public:
    static bool accepts(Kind kind) { return kind == CCall; }

    ~CCallValue() final;

    void appendArgs(const Vector<Value*>&);
    
    Effects effects;

    B3_SPECIALIZE_VALUE_FOR_VARARGS_CHILDREN
    B3_SPECIALIZE_VALUE_FOR_FINAL_SIZE_VARARGS_CHILDREN

private:
    friend class Procedure;
    friend class Value;

    template<typename... Arguments>
    static Opcode opcodeFromConstructor(Arguments...) { return CCall; }

    template<typename... Arguments>
    CCallValue(Type type, Origin origin, Arguments... arguments)
        : Value(CheckedOpcode, CCall, type, VarArgs, origin, static_cast<Value*>(arguments)...)
        , effects(Effects::forCall())
    {
        RELEASE_ASSERT(numChildren() >= 1);
    }

    template<typename... Arguments>
    CCallValue(Type type, Origin origin, const Effects& effects, Arguments... arguments)
        : Value(CheckedOpcode, CCall, type, VarArgs, origin, static_cast<Value*>(arguments)...)
        , effects(effects)
    {
        RELEASE_ASSERT(numChildren() >= 1);
    }
};

} } // namespace JSC::B3

WTF_ALLOW_UNSAFE_BUFFER_USAGE_END

#endif // ENABLE(B3_JIT)
