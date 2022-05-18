/*
 * Copyright (C) 2013-2019 Apple Inc. All rights reserved.
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
#include "JITInlineCacheGenerator.h"

#if ENABLE(JIT)

#include "BaselineJITRegisters.h"
#include "CCallHelpers.h"
#include "CacheableIdentifierInlines.h"
#include "CodeBlock.h"
#include "DFGJITCompiler.h"
#include "InlineAccess.h"
#include "JITInlines.h"
#include "LinkBuffer.h"
#include "StructureStubInfo.h"

namespace JSC {

JITInlineCacheGenerator::JITInlineCacheGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo, JITType jitType, CodeOrigin codeOrigin, AccessType accessType)
    : m_jitType(jitType)
{
    if (stubInfos) {
        ASSERT(JITCode::isOptimizingJIT(m_jitType));
        ASSERT_UNUSED(codeBlock, codeBlock);
        m_stubInfo = stubInfos->add(accessType, codeOrigin);
    }
}

void JITInlineCacheGenerator::finalize(
    LinkBuffer& fastPath, LinkBuffer& slowPath, CodeLocationLabel<JITStubRoutinePtrTag> start)
{
    ASSERT(m_stubInfo);
    m_stubInfo->start = start;
    m_stubInfo->doneLocation = fastPath.locationOf<JSInternalPtrTag>(m_done);

    if (!JITCode::useDataIC(m_jitType))
        m_stubInfo->m_slowPathCallLocation = slowPath.locationOf<JSInternalPtrTag>(m_slowPathCall);
    m_stubInfo->slowPathStartLocation = slowPath.locationOf<JITStubRoutinePtrTag>(m_slowPathBegin);
}

#if ENABLE(DFG_JIT)
void JITInlineCacheGenerator::generateDFGDataICFastPath(DFG::JITCompiler& jit, unsigned stubInfoConstant, GPRReg stubInfoGPR)
{
    m_start = jit.label();
    jit.loadConstant(stubInfoConstant, stubInfoGPR);
    jit.farJump(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    m_done = jit.label();
}
#endif

void JITInlineCacheGenerator::generateBaselineDataICFastPath(JIT& jit, unsigned stubInfo, GPRReg stubInfoGPR)
{
    m_start = jit.label();
    RELEASE_ASSERT(JITCode::useDataIC(m_jitType));
    jit.loadConstant(stubInfo, stubInfoGPR);
    jit.farJump(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    m_done = jit.label();
}

JITByIdGenerator::JITByIdGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, AccessType accessType,
    JSValueRegs base, JSValueRegs value)
    : JITInlineCacheGenerator(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, accessType)
    , m_base(base)
    , m_value(value)
{
}

void JITByIdGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    JITInlineCacheGenerator::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

void JITByIdGenerator::generateFastCommon(CCallHelpers& jit, size_t inlineICSize)
{
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else {
        size_t startSize = jit.m_assembler.buffer().codeSize();
        m_slowPathJump = jit.jump();
        size_t jumpSize = jit.m_assembler.buffer().codeSize() - startSize;
        size_t nopsToEmitInBytes = inlineICSize - jumpSize;
        jit.emitNops(nopsToEmitInBytes);
        ASSERT(jit.m_assembler.buffer().codeSize() - startSize == inlineICSize);
    }
    m_done = jit.label();
}

JITGetByIdGenerator::JITGetByIdGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSite, const RegisterSet& usedRegisters,
    CacheableIdentifier propertyName, JSValueRegs base, JSValueRegs value, GPRReg stubInfoGPR, AccessType accessType)
    : JITByIdGenerator(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, accessType, base, value)
    , m_isLengthAccess(codeBlock && propertyName.uid() == codeBlock->vm().propertyNames->length.impl())
{
    RELEASE_ASSERT(base.payloadGPR() != value.tagGPR());
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, accessType, codeOrigin, callSite, usedRegisters, base, value, stubInfoGPR);
    }, stubInfo);
}

static void generateGetByIdInlineAccess(CCallHelpers& jit, GPRReg stubInfoGPR, JSValueRegs baseJSR, GPRReg scratchGPR, JSValueRegs resultJSR)
{
    jit.load32(CCallHelpers::Address(baseJSR.payloadGPR(), JSCell::structureIDOffset()), scratchGPR);
    auto doInlineAccess = jit.branch32(CCallHelpers::Equal, scratchGPR, CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfInlineAccessBaseStructureID()));
    jit.farJump(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    doInlineAccess.link(&jit);
    jit.load32(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfByIdSelfOffset()), scratchGPR);
    jit.loadProperty(baseJSR.payloadGPR(), scratchGPR, resultJSR);
}

void JITGetByIdGenerator::generateFastPath(CCallHelpers& jit, GPRReg scratchGPR)
{
    ASSERT(m_stubInfo);
    if (!JITCode::useDataIC(m_jitType)) {
        generateFastCommon(jit, m_isLengthAccess ? InlineAccess::sizeForLengthAccess() : InlineAccess::sizeForPropertyAccess());
        return;
    }

    ASSERT(scratchGPR != InvalidGPRReg);
    m_start = jit.label();
    jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
    generateGetByIdInlineAccess(jit, m_stubInfo->m_stubInfoGPR, m_base, scratchGPR, m_value);
    m_done = jit.label();
}

void JITGetByIdGenerator::generateBaselineDataICFastPath(JIT& jit, unsigned stubInfo, GPRReg stubInfoGPR)
{
    RELEASE_ASSERT(JITCode::useDataIC(m_jitType));

    m_start = jit.label();

    using BaselineJITRegisters::GetById::baseJSR;
    using BaselineJITRegisters::GetById::resultJSR;
    using BaselineJITRegisters::GetById::FastPath::scratchGPR;

    jit.loadConstant(stubInfo, stubInfoGPR);
    generateGetByIdInlineAccess(jit, stubInfoGPR, baseJSR, scratchGPR, resultJSR);

    m_done = jit.label();
}

#if ENABLE(DFG_JIT)
void JITGetByIdGenerator::generateDFGDataICFastPath(DFG::JITCompiler& jit, unsigned stubInfoConstant, JSValueRegs baseJSR, JSValueRegs resultJSR, GPRReg stubInfoGPR, GPRReg scratchGPR)
{
    m_start = jit.label();
    jit.loadConstant(stubInfoConstant, stubInfoGPR);
    generateGetByIdInlineAccess(jit, stubInfoGPR, baseJSR, scratchGPR, resultJSR);
    m_done = jit.label();
}
#endif

JITGetByIdWithThisGenerator::JITGetByIdWithThisGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSite, const RegisterSet& usedRegisters,
    CacheableIdentifier, JSValueRegs value, JSValueRegs base, JSValueRegs thisRegs, GPRReg stubInfoGPR)
    : JITByIdGenerator(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, AccessType::GetByIdWithThis, base, value)
{
    RELEASE_ASSERT(thisRegs.payloadGPR() != thisRegs.tagGPR());
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, AccessType::GetByIdWithThis, codeOrigin, callSite, usedRegisters, value, base, thisRegs, stubInfoGPR);
    }, stubInfo);
}

void JITGetByIdWithThisGenerator::generateFastPath(CCallHelpers& jit, GPRReg scratchGPR)
{
    ASSERT(m_stubInfo);
    if (!JITCode::useDataIC(m_jitType)) {
        generateFastCommon(jit, InlineAccess::sizeForPropertyAccess());
        return;
    }

    ASSERT(scratchGPR != InvalidGPRReg);
    m_start = jit.label();
    jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
    generateGetByIdInlineAccess(jit, m_stubInfo->m_stubInfoGPR, m_base, scratchGPR, m_value);
    m_done = jit.label();
}

void JITGetByIdWithThisGenerator::generateBaselineDataICFastPath(JIT& jit, unsigned stubInfo, GPRReg stubInfoGPR)
{
    RELEASE_ASSERT(JITCode::useDataIC(m_jitType));

    m_start = jit.label();

    using BaselineJITRegisters::GetByIdWithThis::baseJSR;
    using BaselineJITRegisters::GetByIdWithThis::resultJSR;
    using BaselineJITRegisters::GetByIdWithThis::FastPath::scratchGPR;

    jit.loadConstant(stubInfo, stubInfoGPR);
    generateGetByIdInlineAccess(jit, stubInfoGPR, baseJSR, scratchGPR, resultJSR);

    m_done = jit.label();
}

#if ENABLE(DFG_JIT)
void JITGetByIdWithThisGenerator::generateDFGDataICFastPath(DFG::JITCompiler& jit, unsigned stubInfoConstant, JSValueRegs baseJSR, JSValueRegs resultJSR, GPRReg stubInfoGPR, GPRReg scratchGPR)
{
    m_start = jit.label();
    jit.loadConstant(stubInfoConstant, stubInfoGPR);
    generateGetByIdInlineAccess(jit, stubInfoGPR, baseJSR, scratchGPR, resultJSR);
    m_done = jit.label();
}
#endif

JITPutByIdGenerator::JITPutByIdGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSite, const RegisterSet& usedRegisters, CacheableIdentifier,
    JSValueRegs base, JSValueRegs value, GPRReg stubInfoGPR, GPRReg scratch, 
    ECMAMode ecmaMode, PutKind putKind)
        : JITByIdGenerator(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, AccessType::PutById, base, value)
        , m_ecmaMode(ecmaMode)
        , m_putKind(putKind)
{
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, AccessType::PutById, codeOrigin, callSite, usedRegisters, base, value, stubInfoGPR, scratch, ecmaMode, putKind);
    }, stubInfo);
}

static void generatePutByIdInlineAccess(CCallHelpers& jit, GPRReg stubInfoGPR, JSValueRegs baseJSR, JSValueRegs valueJSR, GPRReg scratchGPR, GPRReg scratch2GPR)
{
    jit.load32(CCallHelpers::Address(baseJSR.payloadGPR(), JSCell::structureIDOffset()), scratchGPR);
    auto doInlineAccess = jit.branch32(CCallHelpers::Equal, scratchGPR, CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfInlineAccessBaseStructureID()));
    jit.farJump(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    doInlineAccess.link(&jit);
    jit.load32(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfByIdSelfOffset()), scratchGPR);
    jit.storeProperty(valueJSR, baseJSR.payloadGPR(), scratchGPR, scratch2GPR);
}

void JITPutByIdGenerator::generateBaselineDataICFastPath(JIT& jit, unsigned stubInfo, GPRReg stubInfoGPR)
{
    RELEASE_ASSERT(JITCode::useDataIC(m_jitType));

    m_start = jit.label();

    jit.loadConstant(stubInfo, stubInfoGPR);

    using BaselineJITRegisters::PutById::baseJSR;
    using BaselineJITRegisters::PutById::valueJSR;
    using BaselineJITRegisters::PutById::FastPath::scratchGPR;
    using BaselineJITRegisters::PutById::FastPath::scratch2GPR;

    generatePutByIdInlineAccess(jit, stubInfoGPR, baseJSR, valueJSR, scratchGPR, scratch2GPR);
    m_done = jit.label();
}

#if ENABLE(DFG_JIT)
void JITPutByIdGenerator::generateDFGDataICFastPath(DFG::JITCompiler& jit, unsigned stubInfoConstant, JSValueRegs baseJSR, JSValueRegs valueJSR, GPRReg stubInfoGPR, GPRReg scratchGPR, GPRReg scratch2GPR)
{
    m_start = jit.label();
    jit.loadConstant(stubInfoConstant, stubInfoGPR);
    generatePutByIdInlineAccess(jit, stubInfoGPR, baseJSR, valueJSR, scratchGPR, scratch2GPR);
    m_done = jit.label();
}
#endif


void JITPutByIdGenerator::generateFastPath(CCallHelpers& jit, GPRReg scratchGPR, GPRReg scratch2GPR)
{
    ASSERT(m_stubInfo);
    if (!JITCode::useDataIC(m_jitType)) {
        generateFastCommon(jit, InlineAccess::sizeForPropertyReplace());
        return;
    }

    ASSERT(scratchGPR != InvalidGPRReg);
    m_start = jit.label();
    jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
    generatePutByIdInlineAccess(jit, m_stubInfo->m_stubInfoGPR, m_base, m_value, scratchGPR, scratch2GPR);
    m_done = jit.label();
}

V_JITOperation_GSsiJJC JITPutByIdGenerator::slowPathFunction()
{
    switch (m_putKind) {
    case PutKind::NotDirect:
        if (m_ecmaMode.isStrict())
            return operationPutByIdStrictOptimize;
        return operationPutByIdNonStrictOptimize;
    case PutKind::Direct:
        if (m_ecmaMode.isStrict())
            return operationPutByIdDirectStrictOptimize;
        return operationPutByIdDirectNonStrictOptimize;
    case PutKind::DirectPrivateFieldDefine:
        ASSERT(m_ecmaMode.isStrict());
        return operationPutByIdDefinePrivateFieldStrictOptimize;
    case PutKind::DirectPrivateFieldSet:
        ASSERT(m_ecmaMode.isStrict());
        return operationPutByIdSetPrivateFieldStrictOptimize;
    }
    // Make win port compiler happy
    RELEASE_ASSERT_NOT_REACHED();
    return nullptr;
}

JITDelByValGenerator::JITDelByValGenerator(CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex, const RegisterSet& usedRegisters, JSValueRegs base, JSValueRegs property, JSValueRegs result, GPRReg stubInfoGPR)
    : Base(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, AccessType::DeleteByVal)
{
    ASSERT(base.payloadGPR() != result.payloadGPR());
#if USE(JSVALUE32_64)
    ASSERT(base.tagGPR() != result.tagGPR());
#endif
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, AccessType::DeleteByVal, codeOrigin, callSiteIndex, usedRegisters, base, property, result, stubInfoGPR);
    }, stubInfo);
}

void JITDelByValGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITDelByValGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

JITDelByIdGenerator::JITDelByIdGenerator(CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex, const RegisterSet& usedRegisters, CacheableIdentifier, JSValueRegs base, JSValueRegs result, GPRReg stubInfoGPR)
    : Base(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, AccessType::DeleteByID)
{
    ASSERT(base.payloadGPR() != result.payloadGPR());
#if USE(JSVALUE32_64)
    ASSERT(base.tagGPR() != result.tagGPR());
#endif
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, AccessType::DeleteByID, codeOrigin, callSiteIndex, usedRegisters, base, result, stubInfoGPR);
    }, stubInfo);
}

void JITDelByIdGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITDelByIdGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

JITInByValGenerator::JITInByValGenerator(CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex, AccessType accessType, const RegisterSet& usedRegisters, JSValueRegs base, JSValueRegs property, JSValueRegs result, GPRReg stubInfoGPR)
    : Base(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, accessType)
{
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, accessType, codeOrigin, callSiteIndex, usedRegisters, base, property, result, stubInfoGPR);
    }, stubInfo);
}

void JITInByValGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITInByValGenerator::finalize(
    LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_start.isSet());
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

JITInByIdGenerator::JITInByIdGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSite, const RegisterSet& usedRegisters,
    CacheableIdentifier propertyName, JSValueRegs base, JSValueRegs value, GPRReg stubInfoGPR)
    : JITByIdGenerator(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, AccessType::InById, base, value)
{
    // FIXME: We are not supporting fast path for "length" property.
    UNUSED_PARAM(propertyName);
    RELEASE_ASSERT(base.payloadGPR() != value.tagGPR());
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, AccessType::InById, codeOrigin, callSite, usedRegisters, base, value, stubInfoGPR);
    }, stubInfo);
}

static void generateInByIdInlineAccess(CCallHelpers& jit, GPRReg stubInfoGPR, JSValueRegs baseJSR, GPRReg scratchGPR, JSValueRegs resultJSR)
{
    jit.load32(CCallHelpers::Address(baseJSR.payloadGPR(), JSCell::structureIDOffset()), scratchGPR);
    auto skipInlineAccess = jit.branch32(CCallHelpers::NotEqual, scratchGPR, CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfInlineAccessBaseStructureID()));
    jit.boxBoolean(true, resultJSR);
    auto finished = jit.jump();

    skipInlineAccess.link(&jit);
    jit.farJump(CCallHelpers::Address(stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);

    finished.link(&jit);
}

void JITInByIdGenerator::generateFastPath(CCallHelpers& jit, GPRReg scratchGPR)
{
    ASSERT(m_stubInfo);
    if (!JITCode::useDataIC(m_jitType)) {
        generateFastCommon(jit, InlineAccess::sizeForPropertyAccess());
        return;
    }

    ASSERT(scratchGPR != InvalidGPRReg);
    m_start = jit.label();
    jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
    generateInByIdInlineAccess(jit, m_stubInfo->m_stubInfoGPR, m_base, scratchGPR, m_value);
    m_done = jit.label();
}

void JITInByIdGenerator::generateBaselineDataICFastPath(JIT& jit, unsigned stubInfo, GPRReg stubInfoGPR)
{
    RELEASE_ASSERT(JITCode::useDataIC(m_jitType));

    m_start = jit.label();

    jit.loadConstant(stubInfo, stubInfoGPR);

    using BaselineJITRegisters::InById::baseJSR;
    using BaselineJITRegisters::InById::resultJSR;
    using BaselineJITRegisters::InById::scratchGPR;

    generateInByIdInlineAccess(jit, stubInfoGPR, baseJSR, scratchGPR, resultJSR);

    m_done = jit.label();
}

#if ENABLE(DFG_JIT)
void JITInByIdGenerator::generateDFGDataICFastPath(DFG::JITCompiler& jit, unsigned stubInfoConstant, JSValueRegs baseJSR, JSValueRegs resultJSR, GPRReg stubInfoGPR, GPRReg scratchGPR)
{
    m_start = jit.label();
    jit.loadConstant(stubInfoConstant, stubInfoGPR);
    generateInByIdInlineAccess(jit, stubInfoGPR, baseJSR, scratchGPR, resultJSR);
    m_done = jit.label();
}
#endif

JITInstanceOfGenerator::JITInstanceOfGenerator(
    CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex,
    const RegisterSet& usedRegisters, GPRReg result, GPRReg value, GPRReg prototype, GPRReg stubInfoGPR,
    bool prototypeIsKnownObject)
    : JITInlineCacheGenerator(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, AccessType::InstanceOf)
{
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, AccessType::InstanceOf, codeOrigin, callSiteIndex, usedRegisters, result, value, prototype, stubInfoGPR, prototypeIsKnownObject);
    }, stubInfo);
}

void JITInstanceOfGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITInstanceOfGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

JITGetByValGenerator::JITGetByValGenerator(CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex, AccessType accessType, const RegisterSet& usedRegisters, JSValueRegs base, JSValueRegs property, JSValueRegs result, GPRReg stubInfoGPR)
    : Base(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, accessType)
    , m_base(base)
    , m_result(result)
{
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, accessType, codeOrigin, callSiteIndex, usedRegisters, base, property, result, stubInfoGPR);
    }, stubInfo);
}

void JITGetByValGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITGetByValGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

JITPutByValGenerator::JITPutByValGenerator(CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex, AccessType accessType, const RegisterSet& usedRegisters, JSValueRegs base, JSValueRegs property, JSValueRegs value, GPRReg arrayProfileGPR, GPRReg stubInfoGPR, PutKind putKind, ECMAMode ecmaMode, PrivateFieldPutKind privateFieldPutKind)
    : Base(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, accessType)
    , m_base(base)
    , m_value(value)
{
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, accessType, codeOrigin, callSiteIndex, usedRegisters, base, property, value, arrayProfileGPR, stubInfoGPR, putKind, ecmaMode, privateFieldPutKind);
    }, stubInfo);
}

void JITPutByValGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITPutByValGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

JITPrivateBrandAccessGenerator::JITPrivateBrandAccessGenerator(CodeBlock* codeBlock, Bag<StructureStubInfo>* stubInfos, CompileTimeStructureStubInfo stubInfo, JITType jitType, CodeOrigin codeOrigin, CallSiteIndex callSiteIndex, AccessType accessType, const RegisterSet& usedRegisters, JSValueRegs base, JSValueRegs brand, GPRReg stubInfoGPR)
    : Base(codeBlock, stubInfos, stubInfo, jitType, codeOrigin, accessType)
{
    ASSERT(accessType == AccessType::CheckPrivateBrand || accessType == AccessType::SetPrivateBrand);
    if (m_stubInfo)
        stubInfo = m_stubInfo;
    std::visit([&](auto* stubInfo) {
        if (stubInfo)
            setUpStubInfo(*stubInfo, accessType, codeOrigin, callSiteIndex, usedRegisters, base, brand, stubInfoGPR);
    }, stubInfo);
}

void JITPrivateBrandAccessGenerator::generateFastPath(CCallHelpers& jit)
{
    ASSERT(m_stubInfo);
    m_start = jit.label();
    if (JITCode::useDataIC(m_jitType)) {
        jit.move(CCallHelpers::TrustedImmPtr(m_stubInfo), m_stubInfo->m_stubInfoGPR);
        jit.farJump(CCallHelpers::Address(m_stubInfo->m_stubInfoGPR, StructureStubInfo::offsetOfCodePtr()), JITStubRoutinePtrTag);
    } else
        m_slowPathJump = jit.patchableJump();
    m_done = jit.label();
}

void JITPrivateBrandAccessGenerator::finalize(LinkBuffer& fastPath, LinkBuffer& slowPath)
{
    ASSERT(m_stubInfo);
    Base::finalize(fastPath, slowPath, fastPath.locationOf<JITStubRoutinePtrTag>(m_start));
    if (JITCode::useDataIC(m_jitType))
        m_stubInfo->m_codePtr = m_stubInfo->slowPathStartLocation;
}

} // namespace JSC

#endif // ENABLE(JIT)

