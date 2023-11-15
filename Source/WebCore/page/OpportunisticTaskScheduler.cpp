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

#include "config.h"
#include "OpportunisticTaskScheduler.h"

#include "CommonVM.h"
#include "GCController.h"
#include "Page.h"
#include <wtf/DataLog.h>
#include <wtf/SystemTracing.h>

namespace WebCore {

OpportunisticTaskScheduler::OpportunisticTaskScheduler(Page& page)
    : m_page(&page)
    , m_runLoopObserver(makeUnique<RunLoopObserver>(RunLoopObserver::WellKnownOrder::PostRenderingUpdate, [weakThis = WeakPtr { this }] {
        if (auto strongThis = weakThis.get())
            strongThis->runLoopObserverFired();
    }, RunLoopObserver::Type::OneShot))
{
}

OpportunisticTaskScheduler::~OpportunisticTaskScheduler() = default;

void OpportunisticTaskScheduler::rescheduleIfNeeded(MonotonicTime deadline)
{
    auto page = checkedPage();
    if (page->isWaitingForLoadToFinish() || !page->isVisibleAndActive())
        return;

    if (!m_mayHavePendingIdleCallbacks && !page->settings().opportunisticSweepingAndGarbageCollectionEnabled())
        return;

    m_runloopCountAfterBeingScheduled = 0;
    m_currentDeadline = deadline;
    m_runLoopObserver->invalidate();
    m_runLoopObserver->schedule();
}

CheckedPtr<Page> OpportunisticTaskScheduler::checkedPage() const
{
    return m_page.get();
}

Ref<ImminentlyScheduledWorkScope> OpportunisticTaskScheduler::makeScheduledWorkScope()
{
    return ImminentlyScheduledWorkScope::create(*this);
}

void OpportunisticTaskScheduler::runLoopObserverFired()
{
    constexpr bool verbose = false;

    if (!m_currentDeadline) {
        return;
    }

    if (UNLIKELY(!m_page)) {
        return;
    }

    auto page = checkedPage();
    if (page->isWaitingForLoadToFinish()) {
        return;
    }

    if (!page->isVisibleAndActive()) {
        return;
    }

    auto currentTime = ApproximateTime::now();
    auto remainingTime = m_currentDeadline.secondsSinceEpoch() - currentTime.secondsSinceEpoch();
    if (remainingTime < 0_ms) {
        return;
    }

    m_runloopCountAfterBeingScheduled++;

    bool shouldRunTask = [&] {
        if (!hasImminentlyScheduledWork())
            return true;

        static constexpr auto fractionOfRenderingIntervalWhenScheduledWorkIsImminent = 0.72;
        if (remainingTime > fractionOfRenderingIntervalWhenScheduledWorkIsImminent * page->preferredRenderingUpdateInterval())
            return true;

        static constexpr auto minimumRunloopCountWhenScheduledWorkIsImminent = 4;
        if (m_runloopCountAfterBeingScheduled > minimumRunloopCountWhenScheduledWorkIsImminent)
            return true;

        dataLogLnIf(verbose, "GAVEUP: task does not get scheduled ", remainingTime, " ", hasImminentlyScheduledWork(), " ", page->preferredRenderingUpdateInterval(), " ", m_runloopCountAfterBeingScheduled);
        return false;
    }();

    if (!shouldRunTask) {
        dataLogLnIf(verbose, "RunLoopObserverInvalidate");
        m_runLoopObserver->invalidate();
        m_runLoopObserver->schedule();
        return;
    }

    TraceScope tracingScope {
        PerformOpportunisticallyScheduledTasksStart,
        PerformOpportunisticallyScheduledTasksEnd,
        static_cast<uint64_t>(remainingTime.microseconds())
    };

    auto deadline = std::exchange(m_currentDeadline, MonotonicTime { });
    if (std::exchange(m_mayHavePendingIdleCallbacks, false)) {
        auto weakPage = m_page;
        page->opportunisticallyRunIdleCallbacks();
        if (UNLIKELY(!weakPage)) {
            dataLogLnIf(verbose, "GAVEUP: page gets destroyed");
            return;
        }
    }

    if (!page->settings().opportunisticSweepingAndGarbageCollectionEnabled()) {
        dataLogLnIf(verbose, "GAVEUP: opportunistic sweep and GC is not enabled");
        return;
    }

    page->performOpportunisticallyScheduledTasks(deadline);
}

ImminentlyScheduledWorkScope::ImminentlyScheduledWorkScope(OpportunisticTaskScheduler& scheduler)
    : m_scheduler(&scheduler)
{
    scheduler.m_imminentlyScheduledWorkCount++;
}

ImminentlyScheduledWorkScope::~ImminentlyScheduledWorkScope()
{
    if (m_scheduler)
        m_scheduler->m_imminentlyScheduledWorkCount--;
}

bool OpportunisticTaskScheduler::SchedulerCoordinator::isBusy() const
{
    bool result = false;
    Page::forEachPage([&](auto& page) {
        if (page.isWaitingForLoadToFinish()) {
            result = true;
            return;
        }
        if (page.opportunisticTaskScheduler().hasImminentlyScheduledWork()) {
            result = true;
            return;
        }
    });
    return result;
}

void OpportunisticTaskScheduler::FullGCActivityCallback::doCollection(JSC::VM& vm)
{
    if (!m_coordinator->isBusy()) {
        m_version = 0;
        m_deferCount = 0;
        Base::doCollection(vm);
        return;
    }

    if (!m_version || m_version != vm.heap.objectSpace().markingVersion()) {
        m_version = vm.heap.objectSpace().markingVersion();
        m_delay = s_decade;
        scheduleTimer(50_ms);
        return;
    }

    if (++m_deferCount < 5) {
        m_delay = s_decade;
        scheduleTimer(50_ms);
        return;
    }

    m_version = 0;
    m_deferCount = 0;
    Base::doCollection(vm);
}

void OpportunisticTaskScheduler::EdenGCActivityCallback::doCollection(JSC::VM& vm)
{
    if (!m_coordinator->isBusy()) {
        m_version = 0;
        m_deferCount = 0;
        Base::doCollection(vm);
        return;
    }

    if (!m_version || m_version != vm.heap.objectSpace().edenVersion()) {
        m_version = vm.heap.objectSpace().edenVersion();
        m_delay = s_decade;
        scheduleTimer(10_ms);
        return;
    }

    if (++m_deferCount < 5) {
        m_delay = s_decade;
        scheduleTimer(10_ms);
        return;
    }

    m_version = 0;
    m_deferCount = 0;
    Base::doCollection(vm);
}

} // namespace WebCore
