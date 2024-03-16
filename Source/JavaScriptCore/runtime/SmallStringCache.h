/*
 * Copyright (C) 2024 Apple Inc. All rights reserved.
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

#include <wtf/text/AtomStringImpl.h>
#include <wtf/text/StringView.h>

namespace JSC {

class VM;

class SmallStringCache {
public:
    static constexpr unsigned capacity = 512;

    class alignas(alignof(uint64_t)) SmallStringKey {
    public:
        static constexpr unsigned maxStringLengthForCache = 7;
        static constexpr uint8_t invalidKey = 0xff;

        constexpr SmallStringKey() = default;
        explicit SmallStringKey(StringView);

        friend bool operator==(const SmallStringKey&, const SmallStringKey&) = default;
        friend bool operator!=(const SmallStringKey&, const SmallStringKey&) = default;

        bool isValid() const { return m_length != invalidKey; }
        unsigned length() const { return m_length; }
        unsigned hash() const { return WTF::DefaultHash<uint64_t>::hash(*bitwise_cast<const uint64_t*>(this)); }
        const LChar* characters() const { return m_characters; }

    private:
        uint8_t m_length { 0 };
        LChar m_characters[maxStringLengthForCache] { };
    };
    static_assert(sizeof(SmallStringKey) == sizeof(uint64_t));

    struct Entry {
        SmallStringKey m_key { };
        JSString* m_value { nullptr };
    };

    using Cache = std::array<Entry, capacity>;

    template<typename Func>
    JSString* make(VM&, SmallStringKey, const Func&);

    ALWAYS_INLINE void clear()
    {
        m_cache.fill({ });
    }

private:
    Cache m_cache { };
};

} // namespace JSC
