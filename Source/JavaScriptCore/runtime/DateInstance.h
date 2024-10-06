/*
 *  Copyright (C) 1999-2000 Harri Porten (porten@kde.org)
 *  Copyright (C) 2008-2023 Apple Inc. All rights reserved.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *
 */

#pragma once

#include "ISO8601.h"
#include "JSObject.h"

namespace JSC {

class DateInstance final : public JSNonFinalObject {
public:
    using Base = JSNonFinalObject;

    template<typename CellType, SubspaceAccess mode>
    static GCClient::IsoSubspace* subspaceFor(VM& vm)
    {
        return &vm.dateInstanceSpace();
    }

    static DateInstance* create(VM& vm, Structure* structure, double date)
    {
        DateInstance* instance = new (NotNull, allocateCell<DateInstance>(vm)) DateInstance(vm, structure);
        instance->finishCreation(vm, date);
        return instance;
    }

    static DateInstance* create(VM& vm, Structure* structure)
    {
        DateInstance* instance = new (NotNull, allocateCell<DateInstance>(vm)) DateInstance(vm, structure);
        instance->finishCreation(vm);
        return instance;
    }

    double internalNumber() const { return m_internalNumber; }
    void setInternalNumber(double value)
    {
        m_internalNumber = value;
        m_cachedGregorianDateTime = { };
        m_cachedGregorianDateTimeUTC = { };
    }

    DECLARE_EXPORT_INFO;

    ISO8601::PlainGregorianDateTime gregorianDateTime(DateCache& cache) const
    {
        if (m_cachedGregorianDateTime)
            return m_cachedGregorianDateTime;
        return calculateGregorianDateTime(cache);
    }

    ISO8601::PlainGregorianDateTime gregorianDateTimeUTC(DateCache& cache) const
    {
        if (m_cachedGregorianDateTimeUTC)
            return m_cachedGregorianDateTimeUTC;
        return calculateGregorianDateTimeUTC(cache);
    }

    inline static Structure* createStructure(VM&, JSGlobalObject*, JSValue);

    static constexpr ptrdiff_t offsetOfInternalNumber() { return OBJECT_OFFSETOF(DateInstance, m_internalNumber); }
    static constexpr ptrdiff_t offsetOfCachedGregorianDateTime() { return OBJECT_OFFSETOF(DateInstance, m_cachedGregorianDateTime); }
    static constexpr ptrdiff_t offsetOfCachedGregorianDateTimeUTC() { return OBJECT_OFFSETOF(DateInstance, m_cachedGregorianDateTimeUTC); }

private:
    JS_EXPORT_PRIVATE DateInstance(VM&, Structure*);

    DECLARE_DEFAULT_FINISH_CREATION;
    JS_EXPORT_PRIVATE void finishCreation(VM&, double);
    JS_EXPORT_PRIVATE ISO8601::PlainGregorianDateTime calculateGregorianDateTime(DateCache&) const;
    JS_EXPORT_PRIVATE ISO8601::PlainGregorianDateTime calculateGregorianDateTimeUTC(DateCache&) const;

    double m_internalNumber { PNaN };
    mutable ISO8601::PlainGregorianDateTime m_cachedGregorianDateTime { };
    mutable ISO8601::PlainGregorianDateTime m_cachedGregorianDateTimeUTC { };
};

} // namespace JSC
