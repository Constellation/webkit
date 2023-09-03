/*
 * Copyright (C) 2013-2021 Apple Inc. All rights reserved.
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

#if ENABLE(JIT)

#include "GPRInfo.h"
#include "JITOperations.h"

namespace JSC {

namespace BaselineJITRegisters {

namespace Call {
    static constexpr JSValueRegs calleeJSR { JSRInfo::jsRegT10 };
    static constexpr GPRReg calleeGPR { GPRInfo::regT0 };
    static constexpr GPRReg callLinkInfoGPR { GPRInfo::regT2 };
}

namespace CallDirectEval {
    namespace SlowPath {
        static constexpr GPRReg calleeFrameGPR { GPRInfo::regT0 };
#if USE(JSVALUE64)
        static constexpr GPRReg scopeGPR { GPRInfo::regT1 };
        static constexpr JSValueRegs thisValueJSR { GPRInfo::regT2 };
#else
        static constexpr GPRReg scopeGPR { GPRInfo::regT1 };
        static constexpr JSValueRegs thisValueJSR { JSRInfo::jsRegT32 };
#endif
    }
}

namespace CheckTraps {
    static constexpr GPRReg bytecodeOffsetGPR { GPRInfo::nonArgGPR0 };
}

namespace Enter {
    static constexpr GPRReg canBeOptimizedGPR { GPRInfo::regT0 };
    static constexpr GPRReg localsToInitGPR { GPRInfo::regT1 };
}

namespace Instanceof {
    using SlowOperation = decltype(operationInstanceOfOptimize);

    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
    static constexpr JSValueRegs valueJSR { preferredArgumentJSR<SlowOperation, 2>() };
    static constexpr JSValueRegs protoJSR { preferredArgumentJSR<SlowOperation, 3>() };

    // Fast path only registers
    namespace FastPath {
        static constexpr GPRReg stubInfoGPR { GPRInfo::argumentGPR1 };
        static_assert(noOverlap(valueJSR, protoJSR, stubInfoGPR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR { preferredArgumentGPR<SlowOperation, 0>() };
        static constexpr GPRReg stubInfoGPR { preferredArgumentGPR<SlowOperation, 1>() };
        static_assert(noOverlap(globalObjectGPR, stubInfoGPR, valueJSR, protoJSR), "Required for call to slow operation");
    }
}

namespace JFalse {
    static constexpr JSValueRegs valueJSR { JSRInfo::jsRegT32 };
}

namespace JTrue {
    static constexpr JSValueRegs valueJSR { JSRInfo::jsRegT32 };
}

namespace Throw {
    using SlowOperation = decltype(operationThrow);

    static constexpr GPRReg globalObjectGPR { preferredArgumentGPR<SlowOperation, 0>() };
    static constexpr JSValueRegs thrownValueJSR { preferredArgumentJSR<SlowOperation, 1>() };
    static constexpr GPRReg bytecodeOffsetGPR { GPRInfo::nonArgGPR0 };
    static_assert(noOverlap(thrownValueJSR, bytecodeOffsetGPR), "Required for call to CTI thunk");
    static_assert(noOverlap(globalObjectGPR, thrownValueJSR), "Required for call to slow operation");
}

namespace ResolveScope {
    static constexpr GPRReg metadataGPR { GPRInfo::regT2 };
    static constexpr GPRReg scopeGPR { GPRInfo::regT0 };
    static constexpr GPRReg bytecodeOffsetGPR { GPRInfo::regT3 };
    static_assert(noOverlap(metadataGPR, scopeGPR, bytecodeOffsetGPR), "Required for call to CTI thunk");
}

namespace GetFromScope {
    static constexpr GPRReg metadataGPR { GPRInfo::regT4 };
    static constexpr GPRReg scopeGPR { GPRInfo::regT2 };
    static constexpr GPRReg bytecodeOffsetGPR { GPRInfo::regT3 };
    static_assert(noOverlap(metadataGPR, scopeGPR, bytecodeOffsetGPR), "Required for call to CTI thunk");
}

namespace PutToScope {
    static constexpr GPRReg bytecodeOffsetGPR { GPRInfo::argumentGPR2 };
}

namespace GetById {
    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
    static constexpr JSValueRegs baseJSR { JSRInfo::returnValueJSR };

    // Fast path only registers
    namespace FastPath {
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT2 };
        static constexpr GPRReg scratchGPR { GPRInfo::regT3 };
        static constexpr JSValueRegs dontClobberJSR { JSRInfo::jsRegT54 };
        static_assert(noOverlap(baseJSR, stubInfoGPR, scratchGPR, dontClobberJSR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR { GPRInfo::regT2 };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT3 };
        static constexpr GPRReg propertyGPR { GPRInfo::regT4 };
        static_assert(noOverlap(baseJSR, stubInfoGPR, propertyGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, globalObjectGPR, stubInfoGPR, propertyGPR), "Required for call to slow operation");
    }
}

namespace GetByIdWithThis {
    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
    static constexpr JSValueRegs baseJSR { JSRInfo::jsRegT10 };
    static constexpr JSValueRegs thisJSR { JSRInfo::jsRegT32 };

    // Fast path only registers
    namespace FastPath {
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT4 };
        static constexpr GPRReg scratchGPR { GPRInfo::regT5 };
        static_assert(noOverlap(baseJSR, thisJSR, stubInfoGPR, scratchGPR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR { GPRInfo::regT4 };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT5 };
        static constexpr GPRReg propertyGPR {
#if USE(JSVALUE64)
            GPRInfo::regT1
#elif USE(JSVALUE32_64)
            GPRInfo::regT6
#endif
        };
        static_assert(noOverlap(baseJSR, thisJSR, stubInfoGPR, propertyGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, thisJSR, globalObjectGPR, stubInfoGPR, propertyGPR), "Required for call to slow operation");
    }
}

namespace GetByVal {
    // Registers used on both Fast and Slow paths
    using SlowOperation = decltype(operationGetByValOptimize);

    static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
    static constexpr JSValueRegs baseJSR { preferredArgumentJSR<SlowOperation, 3>() };
    static constexpr JSValueRegs propertyJSR { preferredArgumentJSR<SlowOperation, 4>() };
    static constexpr GPRReg stubInfoGPR { preferredArgumentGPR<SlowOperation, 1>() };
    static constexpr GPRReg profileGPR { preferredArgumentGPR<SlowOperation, 2>() };
    static constexpr GPRReg globalObjectGPR { preferredArgumentGPR<SlowOperation, 0>() };
    static constexpr GPRReg scratchGPR { globalObjectGPR };
    static_assert(noOverlap(baseJSR, propertyJSR, stubInfoGPR, profileGPR, globalObjectGPR), "Required for DataIC");
}

#if USE(JSVALUE64)
namespace EnumeratorGetByVal {
    // We rely on using the same registers when linking a CodeBlock and initializing registers
    // for a GetByVal StubInfo.
    using GetByVal::resultJSR;
    using GetByVal::baseJSR;
    using GetByVal::propertyJSR;
    using GetByVal::stubInfoGPR;
    using GetByVal::profileGPR;
    using GetByVal::globalObjectGPR;
    using GetByVal::scratchGPR;
    static constexpr GPRReg scratch2 { GPRInfo::regT5 };
    static constexpr GPRReg scratch3 { GPRInfo::regT7 };
    static_assert(noOverlap(baseJSR, propertyJSR, stubInfoGPR, profileGPR, scratch1, scratch2, scratch3));
}
#endif

#if USE(JSVALUE64)
namespace GetByValWithThis {
    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
    static constexpr JSValueRegs baseJSR { GPRInfo::regT0 };
    static constexpr JSValueRegs propertyJSR { GPRInfo::regT1 };
    static constexpr JSValueRegs thisJSR { GPRInfo::regT2 };

    // Fast path only registers
    namespace FastPath {
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT3 };
        static constexpr GPRReg scratchGPR { GPRInfo::regT5 };
        static_assert(noOverlap(baseJSR, propertyJSR, thisJSR, stubInfoGPR, scratchGPR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR { GPRInfo::regT3 };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT4 };
        static constexpr GPRReg profileGPR { GPRInfo::regT5 };
        static_assert(noOverlap(baseJSR, propertyJSR, thisJSR, stubInfoGPR, profileGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, propertyJSR, thisJSR, globalObjectGPR, stubInfoGPR, profileGPR), "Required for call to slow operation");
    }
}
#endif

namespace PutById {
    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs baseJSR { JSRInfo::jsRegT10 };
    static constexpr JSValueRegs valueJSR { JSRInfo::jsRegT32 };

    // Fast path only registers
    namespace FastPath {
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT4 };
        static constexpr GPRReg scratchGPR { GPRInfo::regT5 };
        // Fine to use regT1, which also yields better code size on ARM_THUMB2
        static constexpr GPRReg scratch2GPR { GPRInfo::regT1 };
        static_assert(noOverlap(baseJSR, valueJSR, stubInfoGPR, scratchGPR), "Required for DataIC");
        static_assert(noOverlap(baseJSR.payloadGPR(), valueJSR, stubInfoGPR, scratchGPR, scratch2GPR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR {
#if USE(JSVALUE64)
            GPRInfo::regT1
#elif USE(JSVALUE32_64)
            GPRInfo::regT6
#endif
        };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT4 };
        static constexpr GPRReg propertyGPR { GPRInfo::regT5 };

        static_assert(noOverlap(baseJSR, valueJSR, stubInfoGPR, propertyGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, valueJSR, globalObjectGPR, stubInfoGPR, propertyGPR), "Required for call to slow operation");
    }
}

namespace PutByVal {
    static constexpr JSValueRegs baseJSR { JSRInfo::jsRegT10 };
    static constexpr JSValueRegs propertyJSR { JSRInfo::jsRegT32 };
    static constexpr JSValueRegs valueJSR { JSRInfo::jsRegT54 };
    static constexpr GPRReg profileGPR {
#if USE(JSVALUE64)
        GPRInfo::regT1
#elif USE(JSVALUE32_64)
        GPRInfo::regT6
#endif
    };
    static constexpr GPRReg stubInfoGPR {
#if USE(JSVALUE64)
        GPRInfo::regT3
#elif USE(JSVALUE32_64)
        GPRInfo::regT7
#endif
    };

    static_assert(noOverlap(baseJSR, propertyJSR, valueJSR, profileGPR, stubInfoGPR), "Required for DataIC");

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR {
#if USE(JSVALUE64)
            GPRInfo::regT5
#elif CPU(ARM_THUMB2)
            // We are a bit short on registers on ARM_THUMB2, but we can just about get away with this
            MacroAssemblerARMv7::s_scratchRegister
#else // Other JSVALUE32_64
            GPRInfo::regT8
#endif
        };
        static_assert(noOverlap(baseJSR, propertyJSR, valueJSR, profileGPR, stubInfoGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, propertyJSR, valueJSR, profileGPR, globalObjectGPR, stubInfoGPR), "Required for call to slow operation");
    }
}

#if USE(JSVALUE64)
namespace EnumeratorPutByVal {
    // We rely on using the same registers when linking a CodeBlock and initializing registers
    // for a PutByVal StubInfo.
    static constexpr JSValueRegs baseJSR { PutByVal::baseJSR };
    static constexpr JSValueRegs propertyJSR { PutByVal::propertyJSR };
    static constexpr JSValueRegs valueJSR { PutByVal::valueJSR };
    static constexpr GPRReg profileGPR { PutByVal::profileGPR };
    static constexpr GPRReg stubInfoGPR { PutByVal::stubInfoGPR };
    static constexpr GPRReg scratch1 { GPRInfo::regT5 };
    static_assert(noOverlap(baseJSR, propertyJSR, valueJSR, stubInfoGPR, scratch1));
}
#endif

namespace InById {
    static constexpr JSValueRegs baseJSR { GetById::baseJSR };
    static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
    static constexpr GPRReg stubInfoGPR { GetById::FastPath::stubInfoGPR };
    static constexpr GPRReg scratchGPR { GetById::FastPath::scratchGPR };
}

namespace InByVal {
    using GetByVal::resultJSR;
    using GetByVal::baseJSR;
    using GetByVal::propertyJSR;
    using GetByVal::stubInfoGPR;
    using GetByVal::profileGPR;
    using GetByVal::globalObjectGPR;
    using GetByVal::scratchGPR;
}

namespace DelById {
    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs baseJSR { JSRInfo::jsRegT32 };

    // Fast path only registers
    namespace FastPath {
        static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT4 };
        static_assert(noOverlap(baseJSR, stubInfoGPR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR { GPRInfo::regT0 };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT1 };
        static constexpr GPRReg propertyGPR { GPRInfo::regT4 };
        static constexpr GPRReg ecmaModeGPR { GPRInfo::regT5 };
        static_assert(noOverlap(baseJSR, stubInfoGPR, propertyGPR, ecmaModeGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, globalObjectGPR, stubInfoGPR, propertyGPR, ecmaModeGPR), "Required for call to slow operation");
    }
}

namespace DelByVal {
    // Registers used on both Fast and Slow paths
    static constexpr JSValueRegs baseJSR { JSRInfo::jsRegT32 };
    static constexpr JSValueRegs propertyJSR { JSRInfo::jsRegT10 };

    // Fast path only registers
    namespace FastPath {
        static constexpr JSValueRegs resultJSR { JSRInfo::returnValueJSR };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT4 };
        static_assert(noOverlap(baseJSR, propertyJSR, stubInfoGPR), "Required for DataIC");
    }

    // Slow path only registers
    namespace SlowPath {
        static constexpr GPRReg globalObjectGPR { GPRInfo::regT4 };
        static constexpr GPRReg stubInfoGPR { GPRInfo::regT5 };
        static constexpr GPRReg ecmaModeGPR {
#if USE(JSVALUE64)
            GPRInfo::regT1
#elif USE(JSVALUE32_64)
            GPRInfo::regT6
#endif
        };
        static_assert(noOverlap(baseJSR, propertyJSR, stubInfoGPR, ecmaModeGPR), "Required for call to CTI thunk");
        static_assert(noOverlap(baseJSR, propertyJSR, globalObjectGPR, stubInfoGPR, ecmaModeGPR), "Required for call to slow operation");
    }
}

namespace PrivateBrand {
    using GetByVal::baseJSR;
    using GetByVal::propertyJSR;
    using GetByVal::stubInfoGPR;
    using GetByVal::globalObjectGPR;
    using GetByVal::scratchGPR;
    static_assert(noOverlap(baseJSR, propertyJSR, stubInfoGPR), "Required for DataIC");
}

} // namespace BaselineJITRegisters

} // namespace JSC

#endif // ENABLE(JIT)
