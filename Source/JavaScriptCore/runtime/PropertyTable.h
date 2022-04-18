/*
 *  Copyright (C) 2004-2022 Apple Inc. All rights reserved.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this library; see the file COPYING.LIB.  If not, write to
 *  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301, USA.
 *
 */

#pragma once

#include "JSExportMacros.h"
#include "PropertyOffset.h"
#include "Structure.h"
#include "WriteBarrier.h"
#include <wtf/HashTable.h>
#include <wtf/MathExtras.h>
#include <wtf/StdLibExtras.h>
#include <wtf/Vector.h>
#include <wtf/text/AtomStringImpl.h>


#define DUMP_PROPERTYMAP_STATS 0
#define DUMP_PROPERTYMAP_COLLISIONS 0

#define PROPERTY_MAP_DELETED_ENTRY_KEY ((UniquedStringImpl*)1)

namespace JSC {

DECLARE_ALLOCATOR_WITH_HEAP_IDENTIFIER(PropertyTable);

#if DUMP_PROPERTYMAP_STATS

struct PropertyTableStats {
    std::atomic<unsigned> numFinds;
    std::atomic<unsigned> numCollisions;
    std::atomic<unsigned> numLookups;
    std::atomic<unsigned> numLookupProbing;
    std::atomic<unsigned> numAdds;
    std::atomic<unsigned> numRemoves;
    std::atomic<unsigned> numRehashes;
    std::atomic<unsigned> numReinserts;
};

JS_EXPORT_PRIVATE extern PropertyTableStats* propertyTableStats;

#endif

inline constexpr bool isPowerOf2(unsigned v)
{
    return hasOneBitSet(v);
}

inline constexpr unsigned nextPowerOf2(unsigned v)
{
    // Taken from http://www.cs.utk.edu/~vose/c-stuff/bithacks.html
    // Devised by Sean Anderson, Sepember 14, 2001

    v--;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    v++;

    return v;
}

// compact <-> non-compact PropertyTable
// We need to maintain two things, one is PropertyOffset and one is unsigned index in index buffer of PropertyTable.
// But both are typically small. It is possible that we can get optimized table if both are fit in uint8_t, that's
// compact PropertyTable.
//
// PropertyOffset can be offseted with firstOutOfLineOffset since we can get out-of-line property easily, but this
// offset is small enough (64 currently), so that we can still assume that most of property offsets are < 256.
//
// 1. If property offset gets larger than 255, then we get non-compact PropertyTable. It requires at least 191 (255 - 64) properties.
//    In that case, PropertyTable size should be 256 since it is power-of-two.
// 2. If index gets larger than 255, then we get non-compact PropertyTable. But we are using 0 and 255 for markers. Thus, if we get 253
//    used counts, then we need to change the table.
//
// So, typical scenario is that, once 128th property is added, then we extend the table via rehashing. At that time, we change the
// table from compact to non-compact mode. We could explore the way to keep this more larger, like, up to 192 by using 75% load factor
// for small table (including compact table).
//
//  index-size  table-capacity    compact   v.s. non-compact
//     16             8              80              192
//     32            16             160              384
//     64            32             320              768
//    128            64             640             1536
//    256           128            1280             3072
//    512           256             N/A             6144     // After 512 size, compact PropertyTable does not work. All table gets non-compact.

class PropertyTable final : public JSCell {
public:
    using Base = JSCell;
    static constexpr unsigned StructureFlags = Base::StructureFlags | StructureIsImmortal;

    template<typename CellType, SubspaceAccess>
    static GCClient::IsoSubspace* subspaceFor(VM& vm)
    {
        return &vm.propertyTableSpace();
    }

    static constexpr bool needsDestruction = true;
    static void destroy(JSCell*);
    DECLARE_VISIT_CHILDREN;

    DECLARE_EXPORT_INFO;

    static Structure* createStructure(VM& vm, JSGlobalObject* globalObject, JSValue prototype)
    {
        return Structure::create(vm, globalObject, prototype, TypeInfo(CellType, StructureFlags), info());
    }

    using KeyType = UniquedStringImpl*;
    using ValueType = PropertyTableEntry;

    // Constructor is passed an initial capacity, a PropertyTable to copy, or both.
    static PropertyTable* create(VM&, unsigned initialCapacity);
    static PropertyTable* clone(VM&, const PropertyTable&);
    static PropertyTable* clone(VM&, unsigned initialCapacity, const PropertyTable&);
    ~PropertyTable();

    // Find a value in the table.
    std::tuple<PropertyOffset, unsigned> get(const KeyType&);
    // Add a value to the table
    std::tuple<PropertyOffset, unsigned, bool> WARN_UNUSED_RETURN add(VM&, const ValueType& entry);
    // Remove a value from the table.
    std::tuple<PropertyOffset, unsigned> take(VM&, const KeyType&);
    PropertyOffset updateAttributeIfExists(const KeyType&, unsigned attributes);

    PropertyOffset renumberPropertyOffsets(JSObject*, unsigned inlineCapacity, Vector<JSValue>&);

    void seal();
    void freeze();

    bool isSealed() const;
    bool isFrozen() const;

    // Returns the number of values in the hashtable.
    unsigned size() const;

    // Checks if there are any values in the hashtable.
    bool isEmpty() const;

    // Number of slots in the property storage array in use, included deletedOffsets.
    unsigned propertyStorageSize() const;

    // Used to maintain a list of unused entries in the property storage.
    void clearDeletedOffsets();
    bool hasDeletedOffset();
    PropertyOffset getDeletedOffset();
    void addDeletedOffset(PropertyOffset);
    
    PropertyOffset nextOffset(PropertyOffset inlineCapacity);

    // Copy this PropertyTable, ensuring the copy has at least the capacity provided.
    PropertyTable* copy(VM&, unsigned newCapacity);

#ifndef NDEBUG
    size_t sizeInMemory();
    void checkConsistency();
#endif

    template<typename Functor>
    void forEachProperty(const Functor&) const;

    static constexpr unsigned EmptyEntryIndex = 0;

private:
    PropertyTable(VM&, unsigned initialCapacity);
    PropertyTable(VM&, const PropertyTable&);
    PropertyTable(VM&, unsigned initialCapacity, const PropertyTable&);

    PropertyTable(const PropertyTable&);

    void finishCreation(VM&);

    // Used to insert a value known not to be in the table, and where we know capacity to be available.
    template<typename Index, typename Entry>
    void reinsert(Index*, Entry*, const ValueType& entry);

    // Rehash the table. Used to grow, or to recover deleted slots.
    static bool canBeFitInCompact(const ValueType& entry) { return entry.offset() <= UINT8_MAX; }
    void rehash(VM&, unsigned newCapacity, bool forceNonCompact);

    // The capacity of the table of values is half of the size of the index.
    unsigned tableCapacity() const;

    // We keep an extra deleted slot after the array to make iteration work,
    // and to use for deleted values. Index values into the array are 1-based,
    // so this is tableCapacity() + 1.
    // For example, if m_tableSize is 16, then tableCapacity() is 8 - but the
    // values array is actually 9 long (the 9th used for the deleted value/
    // iteration guard). The 8 valid entries are numbered 1..8, so the
    // deleted index is 9 (0 being reserved for empty).
    unsigned deletedEntryIndex() const;

    // Used in iterator creation/progression.
    template<typename T>
    static T* skipDeletedEntries(T* valuePtr, T* endValuePtr);

    // total number of  used entries in the values array - by either valid entries, or deleted ones.
    unsigned usedCount() const;

    // The size in bytes of data needed for by the table.
    size_t dataSize(bool isCompact);

    // Calculates the appropriate table size (rounds up to a power of two).
    static unsigned sizeForCapacity(unsigned capacity);

    // Check if capacity is available.
    bool canInsert(const ValueType&);

    void remove(VM&, KeyType, unsigned entryIndex, unsigned index);
    std::tuple<unsigned, unsigned, PropertyOffset, uint8_t> find(const KeyType&);

    template<typename Index, typename Entry>
    ALWAYS_INLINE std::tuple<unsigned, unsigned, PropertyOffset, uint8_t> findImpl(const Index*, const Entry*, const KeyType&);

    bool isCompact() const { return m_indexVector & isCompactFlag; }

    template<typename Functor>
    void forEachPropertyMutable(const Functor&);

    // The table of values lies after the hash index.
    CompactPropertyTableEntry* tableFromIndexVector(uint8_t* index)
    {
        return reinterpret_cast_ptr<CompactPropertyTableEntry*>(index + m_indexSize);
    }
    const CompactPropertyTableEntry* tableFromIndexVector(const uint8_t* index) const
    {
        return reinterpret_cast_ptr<const CompactPropertyTableEntry*>(index + m_indexSize);
    }
    PropertyTableEntry* tableFromIndexVector(uint32_t* index)
    {
        return reinterpret_cast_ptr<PropertyTableEntry*>(index + m_indexSize);
    }
    const PropertyTableEntry* tableFromIndexVector(const uint32_t* index) const
    {
        return reinterpret_cast_ptr<const PropertyTableEntry*>(index + m_indexSize);
    }

    CompactPropertyTableEntry* tableEndFromIndexVector(uint8_t* index)
    {
        return tableFromIndexVector(index) + usedCount();
    }
    const CompactPropertyTableEntry* tableEndFromIndexVector(const uint8_t* index) const
    {
        return tableFromIndexVector(index) + usedCount();
    }
    PropertyTableEntry* tableEndFromIndexVector(uint32_t* index)
    {
        return tableFromIndexVector(index) + usedCount();
    }
    const PropertyTableEntry* tableEndFromIndexVector(const uint32_t* index) const
    {
        return tableFromIndexVector(index) + usedCount();
    }

    static constexpr uintptr_t isCompactFlag = 0x1;
    static constexpr uintptr_t indexVectorMask = ~isCompactFlag;

    unsigned m_indexSize;
    unsigned m_indexMask;
    uintptr_t m_indexVector;
    unsigned m_keyCount;
    unsigned m_deletedCount;
    std::unique_ptr<Vector<PropertyOffset>> m_deletedOffsets;

    static constexpr unsigned MinimumTableSize = 16;
    static_assert(MinimumTableSize >= 16, "compact index is uint8_t and we should keep 16 byte aligned entries after this array");
};

template<typename Index, typename Entry>
std::tuple<unsigned, unsigned, PropertyOffset, uint8_t> PropertyTable::findImpl(const Index* indexVector, const Entry* table, const KeyType& key)
{
    unsigned hash = IdentifierRepHash::hash(key);

#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numFinds;
#endif

    while (true) {
        unsigned index = hash & m_indexMask;
        unsigned entryIndex = indexVector[index];
        if (entryIndex == EmptyEntryIndex)
            return std::tuple { entryIndex, index, invalidOffset, 0 };
        const auto& entry = table[entryIndex - 1];
        if (key == entry.key()) {
            ASSERT(!m_deletedOffsets || !m_deletedOffsets->contains(entry.offset()));
            return std::tuple { entryIndex, index, entry.offset(), entry.attributes() };
        }

#if DUMP_PROPERTYMAP_STATS
        ++propertyTableStats->numCollisions;
#endif

#if DUMP_PROPERTYMAP_COLLISIONS
        dataLog("PropertyTable collision for ", key, " (", hash, ")\n");
        dataLog("Collided with ", entry.key(), "(", IdentifierRepHash::hash(entry.key()), ")\n");
#endif

        hash++;
    }
}

inline std::tuple<unsigned, unsigned, PropertyOffset, uint8_t> PropertyTable::find(const KeyType& key)
{
    ASSERT(key);
    ASSERT(key->isAtom() || key->isSymbol());
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag) {
        const auto* index = bitwise_cast<const uint8_t*>(indexVector & indexVectorMask);
        return findImpl(index, tableFromIndexVector(index), key);
    }
    const auto* index = bitwise_cast<const uint32_t*>(indexVector & indexVectorMask);
    return findImpl(index, tableFromIndexVector(index), key);
}

inline std::tuple<PropertyOffset, unsigned> PropertyTable::get(const KeyType& key)
{
    ASSERT(key);
    ASSERT(key->isAtom() || key->isSymbol());
    ASSERT(key != PROPERTY_MAP_DELETED_ENTRY_KEY);

    if (!m_keyCount)
        return std::tuple { invalidOffset, 0 };

    auto [entryIndex, index, offset, attributes] = find(key);
    UNUSED_VARIABLE(entryIndex);
    UNUSED_VARIABLE(index);
    return std::tuple { offset, attributes };
}

inline std::tuple<PropertyOffset, unsigned, bool> WARN_UNUSED_RETURN PropertyTable::add(VM& vm, const ValueType& entry)
{
    ASSERT(!m_deletedOffsets || !m_deletedOffsets->contains(entry.offset()));

    // Look for a value with a matching key already in the array.
    auto [entryIndex, index, offset, attributes] = find(entry.key());
    if (offset != invalidOffset)
        return std::tuple { offset, attributes, false };

#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numAdds;
#endif

    // Ref the key
    entry.key()->ref();

    // ensure capacity is available.
    if (!canInsert(entry)) {
        rehash(vm, m_keyCount + 1, !canBeFitInCompact(entry));
        std::tie(entryIndex, index, offset, attributes) = find(entry.key());
        ASSERT_UNUSED(offset, offset == invalidOffset);
        ASSERT_UNUSED(entryIndex, entryIndex == EmptyEntryIndex);
    }

    // Allocate a slot in the hashtable, and set the index to reference this.
    entryIndex = usedCount() + 1;
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag) {
        auto* vector = bitwise_cast<uint8_t*>(indexVector & indexVectorMask);
        vector[index] = entryIndex;
        tableFromIndexVector(vector)[entryIndex - 1] = entry;
    } else {
        auto* vector = bitwise_cast<uint32_t*>(indexVector & indexVectorMask);
        vector[index] = entryIndex;
        tableFromIndexVector(vector)[entryIndex - 1] = entry;
    }

    ++m_keyCount;
    
    return std::tuple { entry.offset(), entry.attributes(), true };
}

inline void PropertyTable::remove(VM& vm, KeyType key, unsigned entryIndex, unsigned index)
{
#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numRemoves;
#endif

    // Replace this one element with the deleted sentinel. Also clear out
    // the entry so we can iterate all the entries as needed.
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag) {
        auto* vector = bitwise_cast<uint8_t*>(indexVector & indexVectorMask);
        vector[index] = deletedEntryIndex();
        tableFromIndexVector(vector)[entryIndex - 1].setKey(PROPERTY_MAP_DELETED_ENTRY_KEY);
    } else {
        auto* vector = bitwise_cast<uint32_t*>(indexVector & indexVectorMask);
        vector[index] = deletedEntryIndex();
        tableFromIndexVector(vector)[entryIndex - 1].setKey(PROPERTY_MAP_DELETED_ENTRY_KEY);
    }
    key->deref();

    ASSERT(m_keyCount >= 1);
    --m_keyCount;
    ++m_deletedCount;

    if (m_deletedCount * 4 >= m_indexSize)
        rehash(vm, m_keyCount, false);
}

inline std::tuple<PropertyOffset, unsigned> PropertyTable::take(VM& vm, const KeyType& key)
{
    auto [entryIndex, index, offset, attributes] = find(key);
    if (offset != invalidOffset)
        remove(vm, key, entryIndex, index);
    return std::tuple { offset, attributes };
}

inline PropertyOffset PropertyTable::updateAttributeIfExists(const KeyType& key, unsigned attributes)
{
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag) {
        auto* vector = bitwise_cast<uint8_t*>(indexVector & indexVectorMask);
        auto* table = tableFromIndexVector(vector);
        auto [entryIndex, index, offset, oldAttributes] = findImpl(vector, table, key);
        UNUSED_VARIABLE(index);
        UNUSED_VARIABLE(oldAttributes);
        if (offset == invalidOffset)
            return invalidOffset;
        table[entryIndex - 1].setAttributes(attributes);
        return offset;
    } else {
        auto* vector = bitwise_cast<uint32_t*>(indexVector & indexVectorMask);
        auto* table = tableFromIndexVector(vector);
        auto [entryIndex, index, offset, oldAttributes] = findImpl(vector, table, key);
        UNUSED_VARIABLE(index);
        UNUSED_VARIABLE(oldAttributes);
        if (offset == invalidOffset)
            return invalidOffset;
        table[entryIndex - 1].setAttributes(attributes);
        return offset;
    }
}

// returns the number of values in the hashtable.
inline unsigned PropertyTable::size() const
{
    return m_keyCount;
}

inline bool PropertyTable::isEmpty() const
{
    return !m_keyCount;
}

inline unsigned PropertyTable::propertyStorageSize() const
{
    return size() + (m_deletedOffsets ? m_deletedOffsets->size() : 0);
}

inline void PropertyTable::clearDeletedOffsets()
{
    m_deletedOffsets = nullptr;
}

inline bool PropertyTable::hasDeletedOffset()
{
    return m_deletedOffsets && !m_deletedOffsets->isEmpty();
}

inline PropertyOffset PropertyTable::getDeletedOffset()
{
    PropertyOffset offset = m_deletedOffsets->last();
    m_deletedOffsets->removeLast();
    return offset;
}

inline void PropertyTable::addDeletedOffset(PropertyOffset offset)
{
    if (!m_deletedOffsets)
        m_deletedOffsets = makeUnique<Vector<PropertyOffset>>();
    ASSERT(!m_deletedOffsets->contains(offset));
    m_deletedOffsets->append(offset);
}

inline PropertyOffset PropertyTable::nextOffset(PropertyOffset inlineCapacity)
{
    if (hasDeletedOffset())
        return getDeletedOffset();

    return offsetForPropertyNumber(size(), inlineCapacity);
}

inline PropertyTable* PropertyTable::copy(VM& vm, unsigned newCapacity)
{
    ASSERT(newCapacity >= m_keyCount);

    // Fast case; if the new table will be the same m_indexSize as this one, we can memcpy it,
    // save rehashing all keys.
    if (sizeForCapacity(newCapacity) == m_indexSize)
        return PropertyTable::clone(vm, *this);
    return PropertyTable::clone(vm, newCapacity, *this);
}

#ifndef NDEBUG
inline size_t PropertyTable::sizeInMemory()
{
    size_t result = sizeof(PropertyTable) + dataSize(isCompact());
    if (m_deletedOffsets)
        result += (m_deletedOffsets->capacity() * sizeof(PropertyOffset));
    return result;
}
#endif

template<typename Index, typename Entry>
inline void PropertyTable::reinsert(Index* indexVector, Entry* table, const ValueType& entry)
{
#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numReinserts;
#endif

    // Used to insert a value known not to be in the table, and where
    // we know capacity to be available.
    ASSERT(canInsert(entry));
    auto [emptyIndex, index, offset, attributes] = findImpl(indexVector, table, entry.key());
    ASSERT_UNUSED(offset, offset == invalidOffset);
    ASSERT_UNUSED(emptyIndex, emptyIndex == EmptyEntryIndex);

    unsigned entryIndex = usedCount() + 1;
    indexVector[index] = entryIndex;
    table[entryIndex - 1] = entry;

    ++m_keyCount;
}

inline void PropertyTable::rehash(VM& vm, unsigned newCapacity, bool forceNonCompact)
{
#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numRehashes;
#endif

    bool oldIsCompact = isCompact();
    size_t oldDataSize = dataSize(oldIsCompact);
    uintptr_t oldEntryIndices = m_indexVector;
    unsigned oldIndexSize = m_indexSize;
    unsigned oldUsedCount = usedCount();

    m_indexSize = sizeForCapacity(newCapacity);
    m_indexMask = m_indexSize - 1;
    m_keyCount = 0;
    m_deletedCount = 0;

    // Once table gets non-compact, we do not change it back to compact again.
    // This is because some of property offset can be larger than UINT8_MAX already.
    bool isCompact = !forceNonCompact && oldIsCompact && tableCapacity() < UINT8_MAX;
    size_t newDataSize = dataSize(isCompact);
    uintptr_t indexVector = bitwise_cast<uintptr_t>(PropertyTableMalloc::zeroedMalloc(newDataSize));
    m_indexVector = (indexVector | (isCompact ? isCompactFlag : 0));

    if (isCompact) {
        auto* vector = bitwise_cast<uint8_t*>(indexVector);
        auto* table = tableFromIndexVector(vector);

        ASSERT(oldIsCompact);
        const auto* oldIndex = bitwise_cast<const uint8_t*>(oldEntryIndices & indexVectorMask);
        const auto* oldCursor = reinterpret_cast_ptr<const CompactPropertyTableEntry*>(oldIndex + oldIndexSize);
        const auto* oldEnd = oldCursor + oldUsedCount;
        for (; oldCursor != oldEnd; ++oldCursor) {
            if (oldCursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                continue;
            ASSERT(canInsert(*oldCursor));
            reinsert(vector, table, *oldCursor);
        }
    } else {
        auto* vector = bitwise_cast<uint32_t*>(indexVector);
        auto* table = tableFromIndexVector(vector);

        if (oldIsCompact) {
            const auto* oldIndex = bitwise_cast<const uint8_t*>(oldEntryIndices & indexVectorMask);
            const auto* oldCursor = reinterpret_cast_ptr<const CompactPropertyTableEntry*>(oldIndex + oldIndexSize);
            const auto* oldEnd = oldCursor + oldUsedCount;
            for (; oldCursor != oldEnd; ++oldCursor) {
                if (oldCursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                    continue;
                ASSERT(canInsert(*oldCursor));
                reinsert(vector, table, *oldCursor);
            }

        } else {
            const auto* oldIndex = bitwise_cast<const uint32_t*>(oldEntryIndices & indexVectorMask);
            const auto* oldCursor = reinterpret_cast_ptr<const PropertyTableEntry*>(oldIndex + oldIndexSize);
            const auto* oldEnd = oldCursor + oldUsedCount;
            for (; oldCursor != oldEnd; ++oldCursor) {
                if (oldCursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                    continue;
                ASSERT(canInsert(*oldCursor));
                reinsert(vector, table, *oldCursor);
            }
        }
    }

    PropertyTableMalloc::free(bitwise_cast<void*>(oldEntryIndices & indexVectorMask));

    if (oldDataSize < newDataSize)
        vm.heap.reportExtraMemoryAllocated(newDataSize - oldDataSize);
}

inline unsigned PropertyTable::tableCapacity() const { return m_indexSize >> 1; }

inline unsigned PropertyTable::deletedEntryIndex() const { return tableCapacity() + 1; }

template<typename T>
inline T* PropertyTable::skipDeletedEntries(T* valuePtr, T* endValuePtr)
{
    while (valuePtr < endValuePtr && valuePtr->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
        ++valuePtr;
    return valuePtr;
}

inline unsigned PropertyTable::usedCount() const
{
    // Total number of  used entries in the values array - by either valid entries, or deleted ones.
    return m_keyCount + m_deletedCount;
}

inline size_t PropertyTable::dataSize(bool isCompact)
{
    // The size in bytes of data needed for by the table.
    // Ensure that this function can be called concurrently.
    unsigned indexSize = m_indexSize;
    if (isCompact)
        return indexSize * sizeof(uint8_t) + ((indexSize >> 1) + 1) * sizeof(CompactPropertyTableEntry);
    return indexSize * sizeof(uint32_t) + ((indexSize >> 1) + 1) * sizeof(PropertyTableEntry);
}

inline unsigned PropertyTable::sizeForCapacity(unsigned capacity)
{
    if (capacity < MinimumTableSize / 2)
        return MinimumTableSize;
    return nextPowerOf2(capacity + 1) * 2;
}

inline bool PropertyTable::canInsert(const ValueType& entry)
{
    if (usedCount() >= tableCapacity())
        return false;
    if (!isCompact())
        return true;
    return canBeFitInCompact(entry);
}

template<typename Functor>
inline void PropertyTable::forEachProperty(const Functor& functor) const
{
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag) {
        const auto* index = bitwise_cast<const uint8_t*>(indexVector & indexVectorMask);
        const auto* cursor = tableFromIndexVector(index);
        const auto* end = tableEndFromIndexVector(index);
        for (; cursor != end; ++cursor) {
            if (cursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                continue;
            if (functor(*cursor) == IterationStatus::Done)
                return;
        }
    } else {
        const auto* index = bitwise_cast<const uint32_t*>(indexVector & indexVectorMask);
        const auto* cursor = tableFromIndexVector(index);
        const auto* end = tableEndFromIndexVector(index);
        for (; cursor != end; ++cursor) {
            if (cursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                continue;
            if (functor(*cursor) == IterationStatus::Done)
                return;
        }
    }
}

template<typename Functor>
inline void PropertyTable::forEachPropertyMutable(const Functor& functor)
{
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag) {
        auto* index = bitwise_cast<uint8_t*>(indexVector & indexVectorMask);
        auto* cursor = tableFromIndexVector(index);
        auto* end = tableEndFromIndexVector(index);
        for (; cursor != end; ++cursor) {
            if (cursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                continue;
            if (functor(*cursor) == IterationStatus::Done)
                return;
        }
    } else {
        auto* index = bitwise_cast<uint32_t*>(indexVector & indexVectorMask);
        auto* cursor = tableFromIndexVector(index);
        auto* end = tableEndFromIndexVector(index);
        for (; cursor != end; ++cursor) {
            if (cursor->key() == PROPERTY_MAP_DELETED_ENTRY_KEY)
                continue;
            if (functor(*cursor) == IterationStatus::Done)
                return;
        }
    }
}

} // namespace JSC
