/*
 * Copyright (C) 2023 Apple Inc. All rights reserved.
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

#include "FPRInfo.h"
#include "GPRInfo.h"

#if ENABLE(WEBASSEMBLY)

namespace JSC {

class JSWebAssemblyInstance;

namespace Wasm {

class Instance;

class WasmProtoCallFrame {
    WTF_FORBID_HEAP_ALLOCATION;
public:
    WasmProtoCallFrame(const Wasm::FunctionSignature* signature, WebAssemblyFunction* callee, JSWebAssemblyInstance* jsInstance, Instance* instance)
        : m_signature(signature)
        , m_callee(callee)
        , m_jsInstance(jsInstance)
        , m_instance(instance)
        , m_stackSizeInBytes(WTF::roundUpToMultipleOf(stackAlignmentBytes(), (std::max(signature->returnCount(), signature->argumentCount()) + CallFrame::headerSizeInRegisters) * sizeof(uint64_t)))
    { }

    void appendGPRArgument(uint64_t value)
    {
        if (m_gprArgumentsCount < GPRInfo::numberOfArgumentRegisters) {
            m_gprArguments[m_gprArgumentsCount++] = value;
            return;
        }
        ++m_gprArgumentsCount;
        m_stackArguments.append(value);
    }

    void appendFPRArgument(uint64_t value)
    {
        if (m_fprArgumentsCount < FPRInfo::numberOfArgumentRegisters) {
            m_fprArguments[m_fprArgumentsCount++] = value;
            return;
        }
        ++m_fprArgumentsCount;
        m_stackArguments.append(value);
    }

    unsigned stackSizeInBytes() const { return m_stackSizeInBytes; }

    void finalize()
    {
        m_stackSlots = m_stackArguments.data();
        m_numberOfStackSlots = m_stackArguments.size();
        m_memoryBase = m_instance->cachedMemory();
        m_boundsCheckingSize = m_instance->cachedBoundsCheckingSize();
    }

    const Wasm::FunctionSignature* m_signature;
    WebAssemblyFunction* m_callee;
    JSWebAssemblyInstance* m_jsInstance;
    Instance* m_instance;
    void* m_memoryBase { nullptr };
    size_t m_boundsCheckingSize { 0 };
    uint64_t* m_stackSlots { nullptr };
    unsigned m_stackSizeInBytes;
    unsigned m_numberOfStackSlots { 0 };
    unsigned m_gprArgumentsCount { 0 };
    unsigned m_fprArgumentsCount { 0 };
    Vector<uint64_t, MarkedArgumentBuffer::inlineCapacity> m_stackArguments;
    uint64_t m_gprArguments[GPRInfo::numberOfArgumentRegisters]; // 32bit is broken.
    uint64_t m_fprArguments[FPRInfo::numberOfArgumentRegisters];
};


} } // namespace JSC::Wasm

#endif // ENABLE(WEBASSEMBLY)
