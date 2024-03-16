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

#include "Identifier.h"
#include "SmallStringCache.h"
#include "SmallStrings.h"
#include "VM.h"

namespace JSC {

ALWAYS_INLINE SmallStringCache::SmallStringKey::SmallStringKey(StringView view)
{
    unsigned length = view.length();
    if (length > maxLength) {
        m_length = invalidKey;
        return;
    }

    ASSERT(length != 0);

    if (view.is8Bit()) {
        const auto* characters = view.characters8();
        for (unsigned i = 0; i < length; ++i)
            m_characters[i] = characters[i];
        m_length = length;
        return;
    }

    UChar combined = 0;
    const auto* characters = view.characters16();
    for (unsigned i = 0; i < length; ++i) {
        UChar character = characters[i];
        m_characters[i] = character;
        combined |= character;
    }
    m_length = isLatin1(combined) ? length : invalidKey;
}

template<typename Func>
ALWAYS_INLINE JSString* SmallStringCache::make(VM& vm, SmallStringKey smallString, const Func& func)
{
    ASSERT(smallString.isValid());
    unsigned hash = smallString.hash();
    auto& slot = m_cache[hash % capacity];
    if (slot.m_key == smallString)
        return slot.m_value;

    JSString* result = func(vm, smallString);
    if (LIKELY(result)) {
        slot.m_key = smallString;
        slot.m_value = result;
    }
    return result;
}

} // namespace JSC
