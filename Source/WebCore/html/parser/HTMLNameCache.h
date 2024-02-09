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
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

#include "QualifiedName.h"
#include <wtf/NeverDestroyed.h>
#include <wtf/Vector.h>
#include <wtf/text/AtomString.h>

namespace WebCore {

class HTMLNameCache {
public:
    ALWAYS_INLINE static QualifiedName makeAttributeQualifiedName(std::span<const UChar> string)
    {
        return makeQualifiedName(string);
    }

    ALWAYS_INLINE static QualifiedName makeAttributeQualifiedName(std::span<const LChar> string)
    {
        return makeQualifiedName(string);
    }

    ALWAYS_INLINE static AtomString makeAttributeValue(std::span<const UChar> string)
    {
        return makeAtomString(string);
    }

    ALWAYS_INLINE static AtomString makeAttributeValue(std::span<const LChar> string)
    {
        return makeAtomString(string);
    }

    ALWAYS_INLINE static void clear()
    {
        // FIXME (webkit.org/b/230019): We should try to find more opportunities to clear this cache without hindering this performance optimization.
        atomStringCache().fill({ });
        qualifiedNameCache().fill({ });
    }

private:
    static constexpr auto maxStringLengthForCache = 36;
    static constexpr auto capacity = 512;

    struct AtomStringCacheSlot {
        UChar m_buffer[maxStringLengthForCache];
        UChar m_length { 0 };
        AtomString m_data;
    };

    struct QualifiedNameCacheSlot {
        UChar m_buffer[maxStringLengthForCache];
        UChar m_length { 0 };
        RefPtr<QualifiedName::QualifiedNameImpl> m_data;
    };

    using AtomStringCache = std::array<AtomStringCacheSlot, capacity>;
    using QualifiedNameCache = std::array<QualifiedNameCacheSlot, capacity>;

    template<typename CharacterType>
    ALWAYS_INLINE static AtomString makeAtomString(std::span<const CharacterType> string)
    {
        if (string.empty())
            return emptyAtom();

        auto length = string.size();
        if (length > maxStringLengthForCache)
            return AtomString(string.data(), length);

        auto firstCharacter = string[0];
        auto lastCharacter = string[length - 1];
        auto& slot = atomStringCacheSlot(firstCharacter, lastCharacter, length);
        if (UNLIKELY(slot.m_length != length || !equal(slot.m_buffer, string.data(), length))) {
            AtomString result(string.data(), length);
            if constexpr (sizeof(CharacterType) == 1)
                WTF::copyElements(bitwise_cast<uint16_t*>(slot.m_buffer), bitwise_cast<const uint8_t*>(string.data()), length);
            else
                WTF::copyElements(bitwise_cast<uint16_t*>(slot.m_buffer), bitwise_cast<const uint16_t*>(string.data()), length);
            slot.m_length = length;
            slot.m_data = result;
            return result;
        }

        return slot.m_data;
    }

    template<typename CharacterType>
    ALWAYS_INLINE static QualifiedName makeQualifiedName(std::span<const CharacterType> string)
    {
        if (string.empty())
            return nullQName();

        auto length = string.size();
        if (length > maxStringLengthForCache)
            return QualifiedName(nullAtom(), AtomString(string.data(), length), nullAtom());

        auto firstCharacter = string[0];
        auto lastCharacter = string[length - 1];
        auto& slot = qualifiedNameCacheSlot(firstCharacter, lastCharacter, length);
        if (UNLIKELY(slot.m_length != length || !equal(slot.m_buffer, string.data(), length))) {
            QualifiedName result(nullAtom(), AtomString(string.data(), length), nullAtom());
            if constexpr (sizeof(CharacterType) == 1)
                WTF::copyElements(bitwise_cast<uint16_t*>(slot.m_buffer), bitwise_cast<const uint8_t*>(string.data()), length);
            else
                WTF::copyElements(bitwise_cast<uint16_t*>(slot.m_buffer), bitwise_cast<const uint16_t*>(string.data()), length);
            slot.m_length = length;
            slot.m_data = result.impl();
            return result;
        }

        return *slot.m_data;
    }

    ALWAYS_INLINE static size_t slotIndex(UChar firstCharacter, UChar lastCharacter, UChar length)
    {
        unsigned hash = (firstCharacter << 6) ^ ((lastCharacter << 14) ^ firstCharacter);
        hash += (hash >> 14) + (length << 14);
        hash ^= hash << 14;
        return (hash + (hash >> 6)) % capacity;
    }

    ALWAYS_INLINE static AtomStringCacheSlot& atomStringCacheSlot(UChar firstCharacter, UChar lastCharacter, UChar length)
    {
        auto index = slotIndex(firstCharacter, lastCharacter, length);
        return atomStringCache()[index];
    }

    ALWAYS_INLINE static QualifiedNameCacheSlot& qualifiedNameCacheSlot(UChar firstCharacter, UChar lastCharacter, UChar length)
    {
        auto index = slotIndex(firstCharacter, lastCharacter, length);
        return qualifiedNameCache()[index];
    }

    static AtomStringCache& atomStringCache();
    static QualifiedNameCache& qualifiedNameCache();
};

} // namespace WebCore
