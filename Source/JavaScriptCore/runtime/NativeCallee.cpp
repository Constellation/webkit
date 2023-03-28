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

#include "config.h"
#include "NativeCallee.h"
#include "WasmCallee.h"

namespace JSC {

NativeCallee::NativeCallee(CalleeType calleeType)
    : m_calleeType(calleeType)
    , m_implementationVisibility(ImplementationVisibility::Private)
{
}

template<typename Visitor> constexpr decltype(auto) NativeCallee::visitDerived(Visitor&& visitor)
{
    switch (m_calleeType) {
    case CalleeType::IC:
        return std::invoke(std::forward<Visitor>(visitor), static_cast<ICCallee&>(*this));
    case CalleeType::Wasm:
        return std::invoke(std::forward<Visitor>(visitor), static_cast<Wasm::Callee&>(*this));
    }
    RELEASE_ASSERT_NOT_REACHED();
}

template<typename Visitor> constexpr decltype(auto) NativeCallee::visitDerived(Visitor&& visitor) const
{
    return const_cast<NativeCallee&>(*this).visitDerived([&](auto& value) {
        return std::invoke(std::forward<Visitor>(visitor), std::as_const(value));
    });
}

void NativeCallee::operator delete(NativeCallee* callee, std::destroying_delete_t)
{
    switch (callee->m_calleeType) {
    case CalleeType::IC: {
        auto& derived = static_cast<ICCallee&>(*callee);
        std::destroy_at(&derived);
        std::decay_t<decltype(derived)>::freeAfterDestruction(&derived);
        return;
    }
    case CalleeType::Wasm:
        auto& derived = static_cast<Wasm::Callee&>(*callee);
        delete &derived;
        return;
    }
}

}
