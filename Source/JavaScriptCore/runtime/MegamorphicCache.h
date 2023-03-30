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

#include "Structure.h"

namespace JSC {

DECLARE_ALLOCATOR_WITH_HEAP_IDENTIFIER(MegamorphicCache);

class MegamorphicCache {
    WTF_MAKE_FAST_ALLOCATED;
    WTF_MAKE_NONCOPYABLE(MegamorphicCache);
public:
    static constexpr uint32_t size = 1024;
    static_assert(hasOneBitSet(size), "size should be a power of two.");
    static constexpr uint32_t mask = size - 1;

    static constexpr PropertyOffset maxOffset = UINT16_MAX;

    struct Entry {
        static ptrdiff_t offsetOfUid() { return OBJECT_OFFSETOF(Entry, m_uid); }
        static ptrdiff_t offsetOfStructureID() { return OBJECT_OFFSETOF(Entry, m_structureID); }
        static ptrdiff_t offsetOfEpoch() { return OBJECT_OFFSETOF(Entry, m_epoch); }
        static ptrdiff_t offsetOfOffset() { return OBJECT_OFFSETOF(Entry, m_offset); }
        static ptrdiff_t offsetOfHolder() { return OBJECT_OFFSETOF(Entry, m_holder); }

        void initAsMiss(StructureID structureID, UniquedStringImpl* uid, uint16_t epoch)
        {
            m_uid = uid;
            m_structureID = structureID;
            m_epoch = epoch;
            m_offset = 0;
            m_holder = nullptr;
        }

        void initAsHit(StructureID structureID, UniquedStringImpl* uid, uint16_t epoch, JSObject* holder, uint8_t offset, bool ownProperty)
        {
            m_uid = uid;
            m_structureID = structureID;
            m_epoch = epoch;
            m_offset = offset;
            m_holder = (ownProperty) ? JSCell::seenMultipleCalleeObjects() : holder;
        }

        RefPtr<UniquedStringImpl> m_uid;
        StructureID m_structureID { } ;
        uint16_t m_epoch { 0 };
        uint16_t m_offset { 0 };
        JSCell* m_holder { nullptr };
    };

    static ptrdiff_t offsetOfEntries() { return OBJECT_OFFSETOF(MegamorphicCache, m_entries); }
    static ptrdiff_t offsetOfEpoch() { return OBJECT_OFFSETOF(MegamorphicCache, m_epoch); }

    MegamorphicCache() = default;

    // Due to sizeof(Structure), lower several bits are always zero.
    // sizeof(Structure) <= 128. And 128 / 16 = 8 (0b1000). So, 3 + log2(size)
    static constexpr unsigned structureIDHashShift = 3 + 10;

    ALWAYS_INLINE static uint32_t hash(StructureID structureID, UniquedStringImpl* uid)
    {
        return bitwise_cast<uint32_t>(structureID) + uid->hash();
    }

    JS_EXPORT_PRIVATE void age(CollectionScope);

    Entry& tryGet(StructureID structureID, UniquedStringImpl* uid, bool& matched)
    {
        auto& entry = get(structureID, uid);
        matched = entry.m_structureID == structureID && entry.m_uid == uid && entry.m_epoch == m_epoch;
        return entry;
    }

    uint16_t epoch() const { return m_epoch; }

    void bumpEpoch()
    {
        ++m_epoch;
        if (UNLIKELY(!m_epoch))
            clearEntries();
    }

    ALWAYS_INLINE Entry& get(StructureID structureID, UniquedStringImpl* uid)
    {
        uint32_t index = MegamorphicCache::hash(structureID, uid) & mask;
        return m_entries[index];
    }

private:
    JS_EXPORT_PRIVATE void clearEntries();

    std::array<Entry, size> m_entries { };
    uint16_t m_epoch { 0 };
};

ALWAYS_INLINE MegamorphicCache& VM::ensureMegamorphicCache()
{
    if (UNLIKELY(!m_megamorphicCache))
        m_megamorphicCache = makeUnique<MegamorphicCache>();
    return *m_megamorphicCache;
}

} // namespace JSC
