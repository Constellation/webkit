/*
 * Copyright (C) 2021 Apple Inc. All rights reserved.
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

#include "IsoMemoryAllocatorBase.h"
#include <wtf/Gigacage.h>

#if ENABLE(MALLOC_HEAP_BREAKDOWN)
#include <wtf/DebugHeap.h>
#endif

#if CPU(ADDRESS64) && !ENABLE(STRUCTURE_ID_WITH_SHIFT)
#if !USE(SYSTEM_MALLOC)
#include <bmalloc/BPlatform.h>
#if BUSE(LIBPAS)
#define USE_LIBPAS_STRUCTURE_MEMORY_ALLOCATOR 1
#endif
#endif
#endif

namespace JSC {

#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)

class StructureAlignedMemoryAllocator final : public AlignedMemoryAllocator {
public:
    using Base = AlignedMemoryAllocator;

    StructureAlignedMemoryAllocator(CString);
    ~StructureAlignedMemoryAllocator() final;

    void* tryAllocateAlignedMemory(size_t alignment, size_t size) final;
    void freeAlignedMemory(void*) final;
    void* tryAllocateMemory(size_t) final;
    void freeMemory(void*) final;
    void* tryReallocateMemory(void*, size_t) final;
    void dump(PrintStream&) const final;

    static void initializeStructureAddressSpace();
protected:
};

#else

class StructureAlignedMemoryAllocator final : public IsoMemoryAllocatorBase {
public:
    using Base = IsoMemoryAllocatorBase;

    StructureAlignedMemoryAllocator(CString);
    ~StructureAlignedMemoryAllocator() final;
    
    void dump(PrintStream&) const final;

    void* tryAllocateMemory(size_t) final;
    void freeMemory(void*) final;
    void* tryReallocateMemory(void*, size_t) final;

    static void initializeStructureAddressSpace();

protected:
    void* tryMallocBlock() final;
    void freeBlock(void* block) final;
    void commitBlock(void* block) final;
    void decommitBlock(void* block) final;
};

#endif

} // namespace JSC

