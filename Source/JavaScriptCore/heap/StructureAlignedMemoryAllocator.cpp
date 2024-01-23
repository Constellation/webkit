/*
 * Copyright (C) 2017-2021 Apple Inc. All rights reserved.
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

#include "config.h"
#include "StructureAlignedMemoryAllocator.h"

#include "JSCConfig.h"
#include "MarkedBlock.h"
#include "StructureID.h"

#if CPU(ADDRESS64) && !ENABLE(STRUCTURE_ID_WITH_SHIFT)
#include <wtf/NeverDestroyed.h>
#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)
#include <bmalloc/BPlatform.h>
#include <bmalloc/bmalloc_heap_config.h>
#include <bmalloc/bmalloc_heap_inlines.h>
#include <bmalloc/bmalloc_heap_ref.h>
#include <bmalloc/pas_debug_spectrum.h>
#include <bmalloc/pas_fd_stream.h>
#include <bmalloc/pas_heap_lock.h>
#include <bmalloc/pas_primitive_heap_ref.h>
#include <bmalloc/pas_thread_local_cache.h>
#endif
#endif

#include <wtf/OSAllocator.h>

#if OS(UNIX) && ASSERT_ENABLED
#include <sys/mman.h>
#endif

namespace JSC {

#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)

StructureAlignedMemoryAllocator::StructureAlignedMemoryAllocator(CString)
    : Base()
{
}

#else

StructureAlignedMemoryAllocator::StructureAlignedMemoryAllocator(CString name)
    : Base(name)
{
}

#endif

void StructureAlignedMemoryAllocator::dump(PrintStream& out) const
{
    out.print("Structure(", RawPointer(this), ")");
}

void* StructureAlignedMemoryAllocator::tryAllocateMemory(size_t)
{
    return nullptr;
}

void StructureAlignedMemoryAllocator::freeMemory(void*)
{
    // Structures do not support Precise allocations right now.
    RELEASE_ASSERT_NOT_REACHED();
}

void* StructureAlignedMemoryAllocator::tryReallocateMemory(void*, size_t)
{
    // Structures do not support Precise allocations right now.
    RELEASE_ASSERT_NOT_REACHED();
}

#if CPU(ADDRESS64) && !ENABLE(STRUCTURE_ID_WITH_SHIFT)

#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)
static constexpr bmalloc_type structureType = BMALLOC_TYPE_INITIALIZER(MarkedBlock::blockSize, MarkedBlock::blockSize, "Structure");
static pas_primitive_heap_ref structureHeap = BMALLOC_AUXILIARY_HEAP_REF_INITIALIZER(&structureType);
#endif

class StructureMemoryManager {
public:
    StructureMemoryManager()
    {
        uintptr_t mappedHeapSize = structureHeapAddressSize;
        for (unsigned i = 0; i < 8; ++i) {
            g_jscConfig.startOfStructureHeap = reinterpret_cast<uintptr_t>(OSAllocator::tryReserveUncommittedAligned(mappedHeapSize, structureHeapAddressSize, OSAllocator::FastMallocPages));
            if (g_jscConfig.startOfStructureHeap)
                break;
            mappedHeapSize /= 2;
        }
        g_jscConfig.sizeOfStructureHeap = mappedHeapSize;
        RELEASE_ASSERT(g_jscConfig.startOfStructureHeap && ((g_jscConfig.startOfStructureHeap & ~StructureID::structureIDMask) == g_jscConfig.startOfStructureHeap));

#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)
        bmalloc_force_auxiliary_heap_into_reserved_memory(&structureHeap,  g_jscConfig.startOfStructureHeap, g_jscConfig.startOfStructureHeap + g_jscConfig.sizeOfStructureHeap);
#else
        // Don't use the first page because zero is used as the empty StructureID and the first allocation will conflict.
        m_usedBlocks.set(0);
#endif
    }

    void* tryMallocStructureBlock()
    {
#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)
        return bmalloc_try_allocate_auxiliary_inline(&structureHeap, MarkedBlock::blockSize);
#else
        size_t freeIndex;
        {
            Locker locker(m_lock);
            constexpr size_t startIndex = 0;
            freeIndex = m_usedBlocks.findBit(startIndex, 0);
            ASSERT(freeIndex <= m_usedBlocks.bitCount());
            RELEASE_ASSERT(g_jscConfig.sizeOfStructureHeap <= structureHeapAddressSize);
            if (freeIndex * MarkedBlock::blockSize >= g_jscConfig.sizeOfStructureHeap)
                return nullptr;
            // If we can't find a free block then `freeIndex == m_usedBlocks.bitCount()` and this set will grow the bit vector.
            m_usedBlocks.set(freeIndex);
        }

        auto* block = reinterpret_cast<uint8_t*>(g_jscConfig.startOfStructureHeap) + freeIndex * MarkedBlock::blockSize;
        commitBlock(block);
        return block;
#endif
    }

    void freeStructureBlock(void* blockPtr)
    {
#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)
        bmalloc_deallocate_inline(blockPtr);
#else
        decommitBlock(blockPtr);
        uintptr_t block = reinterpret_cast<uintptr_t>(blockPtr);
        RELEASE_ASSERT(g_jscConfig.startOfStructureHeap <= block && block < g_jscConfig.startOfStructureHeap + g_jscConfig.sizeOfStructureHeap);
        RELEASE_ASSERT(roundUpToMultipleOf<MarkedBlock::blockSize>(block) == block);

        Locker locker(m_lock);
        m_usedBlocks.quickClear((block - g_jscConfig.startOfStructureHeap) / MarkedBlock::blockSize);
#endif
    }

#if !USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)
    static void commitBlock(void* block)
    {
#if OS(UNIX) && ASSERT_ENABLED
        constexpr bool readable = true;
        constexpr bool writable = true;
        OSAllocator::protect(block, MarkedBlock::blockSize, readable, writable);
#else
        constexpr bool writable = true;
        constexpr bool executable = false;
        OSAllocator::commit(block, MarkedBlock::blockSize, writable, executable);
#endif
    }

    static void decommitBlock(void* block)
    {
#if OS(UNIX) && ASSERT_ENABLED
        constexpr bool readable = false;
        constexpr bool writable = false;
        OSAllocator::protect(block, MarkedBlock::blockSize, readable, writable);
#else
        OSAllocator::decommit(block, MarkedBlock::blockSize);
#endif
    }

private:
    Lock m_lock;
    BitVector m_usedBlocks;
#endif
};

static LazyNeverDestroyed<StructureMemoryManager> s_structureMemoryManager;

void StructureAlignedMemoryAllocator::initializeStructureAddressSpace()
{
    static_assert(hasOneBitSet(structureHeapAddressSize));
    s_structureMemoryManager.construct();
}

#if !USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)

void* StructureAlignedMemoryAllocator::tryMallocBlock()
{
    static std::once_flag s_onceFlag;
    std::call_once(s_onceFlag, [] {
        initializeStructureAddressSpace();
    });
    return s_structureMemoryManager->tryMallocStructureBlock();
}

void StructureAlignedMemoryAllocator::freeBlock(void* block)
{
    s_structureMemoryManager->freeStructureBlock(block);
}

void StructureAlignedMemoryAllocator::commitBlock(void* block)
{
    StructureMemoryManager::commitBlock(block);
}

void StructureAlignedMemoryAllocator::decommitBlock(void* block)
{
    StructureMemoryManager::decommitBlock(block);
}

#endif

#else // not CPU(ADDRESS64)

// FIXME: This is the same as IsoAlignedMemoryAllocator maybe we should just use that for 32-bit.

void StructureAlignedMemoryAllocator::initializeStructureAddressSpace()
{
    g_jscConfig.startOfStructureHeap = 0;
    g_jscConfig.sizeOfStructureHeap = UINTPTR_MAX;
}

void* StructureAlignedMemoryAllocator::tryMallocBlock()
{
    return tryFastAlignedMalloc(MarkedBlock::blockSize, MarkedBlock::blockSize);
}

void StructureAlignedMemoryAllocator::freeBlock(void* block)
{
    fastAlignedFree(block);
}

void StructureAlignedMemoryAllocator::commitBlock(void* block)
{
    WTF::fastCommitAlignedMemory(block, MarkedBlock::blockSize);
}

void StructureAlignedMemoryAllocator::decommitBlock(void* block)
{
    WTF::fastDecommitAlignedMemory(block, MarkedBlock::blockSize);
}

#endif // CPU(ADDRESS64)

#if USE(LIBPAS_STRUCTURE_MEMORY_ALLOCATOR)

StructureAlignedMemoryAllocator::~StructureAlignedMemoryAllocator() = default;

void* StructureAlignedMemoryAllocator::tryAllocateAlignedMemory(size_t alignment, size_t size)
{
    RELEASE_ASSERT(alignment == MarkedBlock::blockSize);
    RELEASE_ASSERT(size == MarkedBlock::blockSize);
    static std::once_flag s_onceFlag;
    std::call_once(s_onceFlag, [] {
        initializeStructureAddressSpace();
    });
    return s_structureMemoryManager->tryMallocStructureBlock();
}

void StructureAlignedMemoryAllocator::freeAlignedMemory(void* block)
{
    if (block)
        s_structureMemoryManager->freeStructureBlock(block);
}

#else

StructureAlignedMemoryAllocator::~StructureAlignedMemoryAllocator()
{
    releaseMemoryFromSubclassDestructor();
}

#endif

} // namespace JSC

