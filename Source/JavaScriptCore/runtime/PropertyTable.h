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
    // This is the implementation for 'iterator' and 'const_iterator',
    // used for iterating over the table in insertion order.
    template<typename T>
    class ordered_iterator {
    public:
        ordered_iterator<T>& operator++()
        {
            m_valuePtr = skipDeletedEntries(m_valuePtr + 1, m_endValuePtr);
            return *this;
        }

        bool operator==(const ordered_iterator<T>& other) const
        {
            return m_valuePtr == other.m_valuePtr;
        }

        bool operator!=(const ordered_iterator<T>& other) const
        {
            return m_valuePtr != other.m_valuePtr;
        }

        T& operator*()
        {
            return *m_valuePtr;
        }

        T* operator->()
        {
            return m_valuePtr;
        }

        ordered_iterator(T* valuePtr, T* endValuePtr)
            : m_valuePtr(valuePtr)
            , m_endValuePtr(endValuePtr)
        {
        }

    private:
        T* m_valuePtr;
        T* m_endValuePtr;
    };

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

    // The in order iterator provides overloaded * and -> to access the Value at the current position.
    using iterator = ordered_iterator<ValueType>;
    using const_iterator = ordered_iterator<const ValueType>;

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
    template<typename Index>
    void reinsert(Index*, const ValueType& entry);

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

    // The table of values lies after the hash index.
    ValueType* table();
    const ValueType* table() const;

    ValueType* tableEnd() { return table() + usedCount(); }
    const ValueType* tableEnd() const { return table() + usedCount(); }

    // total number of  used entries in the values array - by either valid entries, or deleted ones.
    unsigned usedCount() const;

    // The size in bytes of data needed for by the table.
    size_t dataSize(bool isCompact);

    // Calculates the appropriate table size (rounds up to a power of two).
    static unsigned sizeForCapacity(unsigned capacity);

    // Check if capacity is available.
    bool canInsert(const ValueType&);

    // The find_iterator is a pair of a pointer to a Value* an the entry in the index.
    // If 'find' does not find an entry then iter.first will be 0, and iter.second will
    // give the point in m_indexVector where an entry should be inserted.
    using find_iterator = std::pair<ValueType*, unsigned>;

    template<typename Index>
    ALWAYS_INLINE ValueType* getImpl(const Index*, const KeyType&);

    void remove(VM&, const find_iterator&);
    find_iterator find(const KeyType&);

    template<typename Index>
    ALWAYS_INLINE find_iterator findImpl(const Index*, const KeyType&);

    bool isCompact() const { return m_indexVector & isCompactFlag; }

    template<typename Functor>
    void forEachPropertyMutable(const Functor&);

    // Ordered iteration methods.
    iterator begin();
    iterator end();
    const_iterator begin() const;
    const_iterator end() const;

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

inline PropertyTable::iterator PropertyTable::begin()
{
    auto* tableEnd = this->tableEnd();
    return iterator(skipDeletedEntries(table(), tableEnd), tableEnd);
}

inline PropertyTable::iterator PropertyTable::end()
{
    auto* tableEnd = this->tableEnd();
    return iterator(tableEnd, tableEnd);
}

inline PropertyTable::const_iterator PropertyTable::begin() const
{
    auto* tableEnd = this->tableEnd();
    return const_iterator(skipDeletedEntries(table(), tableEnd), tableEnd);
}

inline PropertyTable::const_iterator PropertyTable::end() const
{
    auto* tableEnd = this->tableEnd();
    return const_iterator(tableEnd, tableEnd);
}

template<typename Index>
PropertyTable::find_iterator PropertyTable::findImpl(const Index* indexVector, const KeyType& key)
{
    unsigned hash = IdentifierRepHash::hash(key);

#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numFinds;
#endif

    while (true) {
        unsigned entryIndex = indexVector[hash & m_indexMask];
        if (entryIndex == EmptyEntryIndex)
            return std::make_pair((ValueType*)nullptr, hash & m_indexMask);
        if (key == table()[entryIndex - 1].key())
            return std::make_pair(&table()[entryIndex - 1], hash & m_indexMask);

#if DUMP_PROPERTYMAP_STATS
        ++propertyTableStats->numCollisions;
#endif

#if DUMP_PROPERTYMAP_COLLISIONS
        dataLog("PropertyTable collision for ", key, " (", hash, ")\n");
        dataLog("Collided with ", table()[entryIndex - 1].key(), "(", IdentifierRepHash::hash(table()[entryIndex - 1].key()), ")\n");
#endif

        hash++;
    }
}

inline PropertyTable::find_iterator PropertyTable::find(const KeyType& key)
{
    ASSERT(key);
    ASSERT(key->isAtom() || key->isSymbol());
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag)
        return findImpl(bitwise_cast<const uint8_t*>(indexVector & indexVectorMask), key);
    return findImpl(bitwise_cast<const uint32_t*>(indexVector & indexVectorMask), key);
}

inline std::tuple<PropertyOffset, unsigned> PropertyTable::get(const KeyType& key)
{
    ASSERT(key);
    ASSERT(key->isAtom() || key->isSymbol());
    ASSERT(key != PROPERTY_MAP_DELETED_ENTRY_KEY);

    if (!m_keyCount)
        return std::tuple { invalidOffset, 0 };

    uintptr_t indexVector = m_indexVector;
    auto* entry = (indexVector & isCompactFlag) ? getImpl(bitwise_cast<const uint8_t*>(indexVector & indexVectorMask), key) : getImpl(bitwise_cast<const uint32_t*>(indexVector & indexVectorMask), key);
    if (!entry)
        return std::tuple { invalidOffset, 0 };
    return std::tuple { entry->offset(), entry->attributes() };
}

template<typename Index>
inline PropertyTable::ValueType* PropertyTable::getImpl(const Index* indexVector, const KeyType& key)
{
    unsigned hash = IdentifierRepHash::hash(key);

#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numLookups;
#endif

    while (true) {
        unsigned entryIndex = indexVector[hash & m_indexMask];
        if (entryIndex == EmptyEntryIndex)
            return nullptr;
        if (key == table()[entryIndex - 1].key()) {
            ASSERT(!m_deletedOffsets || !m_deletedOffsets->contains(table()[entryIndex - 1].offset()));
            return &table()[entryIndex - 1];
        }

#if DUMP_PROPERTYMAP_STATS
        ++propertyTableStats->numLookupProbing;
#endif

        hash++;
    }
}

inline std::tuple<PropertyOffset, unsigned, bool> WARN_UNUSED_RETURN PropertyTable::add(VM& vm, const ValueType& entry)
{
    ASSERT(!m_deletedOffsets || !m_deletedOffsets->contains(entry.offset()));

    // Look for a value with a matching key already in the array.
    find_iterator iter = find(entry.key());
    if (iter.first)
        return std::tuple { iter.first->offset(), iter.first->attributes(), false };

#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numAdds;
#endif

    // Ref the key
    entry.key()->ref();

    // ensure capacity is available.
    if (!canInsert(entry)) {
        rehash(vm, m_keyCount + 1, !canBeFitInCompact(entry));
        iter = find(entry.key());
        ASSERT(!iter.first);
    }

    // Allocate a slot in the hashtable, and set the index to reference this.
    unsigned entryIndex = usedCount() + 1;
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag)
        bitwise_cast<uint8_t*>(indexVector & indexVectorMask)[iter.second] = entryIndex;
    else
        bitwise_cast<uint32_t*>(indexVector & indexVectorMask)[iter.second] = entryIndex;
    iter.first = &table()[entryIndex - 1];
    *iter.first = entry;

    ++m_keyCount;
    
    return std::tuple { iter.first->offset(), iter.first->attributes(), true };
}

inline void PropertyTable::remove(VM& vm, const find_iterator& iter)
{
    // Removing a key that doesn't exist does nothing!
    if (!iter.first)
        return;

#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numRemoves;
#endif

    // Replace this one element with the deleted sentinel. Also clear out
    // the entry so we can iterate all the entries as needed.
    unsigned entryIndex = deletedEntryIndex();
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag)
        bitwise_cast<uint8_t*>(indexVector & indexVectorMask)[iter.second] = entryIndex;
    else
        bitwise_cast<uint32_t*>(indexVector & indexVectorMask)[iter.second] = entryIndex;
    iter.first->key()->deref();
    iter.first->setKey(PROPERTY_MAP_DELETED_ENTRY_KEY);

    ASSERT(m_keyCount >= 1);
    --m_keyCount;
    ++m_deletedCount;

    if (m_deletedCount * 4 >= m_indexSize)
        rehash(vm, m_keyCount, false);
}

inline std::tuple<PropertyOffset, unsigned> PropertyTable::take(VM& vm, const KeyType& key)
{
    auto iterator = find(key);
    if (!iterator.first)
        return std::tuple { invalidOffset, 0 };
    PropertyOffset offset = iterator.first->offset();
    unsigned attributes = iterator.first->attributes();
    remove(vm, iterator);
    return std::tuple { offset, attributes };
}

inline PropertyOffset PropertyTable::updateAttributeIfExists(const KeyType& key, unsigned attributes)
{
    uintptr_t indexVector = m_indexVector;
    auto* entry = (indexVector & isCompactFlag) ? getImpl(bitwise_cast<const uint8_t*>(indexVector & indexVectorMask), key) : getImpl(bitwise_cast<const uint32_t*>(indexVector & indexVectorMask), key);
    if (!entry)
        return invalidOffset;
    entry->setAttributes(attributes);
    return entry->offset();
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

template<typename Index>
inline void PropertyTable::reinsert(Index* indexVector, const ValueType& entry)
{
#if DUMP_PROPERTYMAP_STATS
    ++propertyTableStats->numReinserts;
#endif

    // Used to insert a value known not to be in the table, and where
    // we know capacity to be available.
    ASSERT(canInsert(entry));
    find_iterator iter = find(entry.key());
    ASSERT(!iter.first);

    unsigned entryIndex = usedCount() + 1;
    indexVector[iter.second] = entryIndex;
    table()[entryIndex - 1] = entry;

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
    iterator iter = this->begin();
    iterator end = this->end();

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
        for (; iter != end; ++iter) {
            ASSERT(canInsert(*iter));
            reinsert(bitwise_cast<uint8_t*>(indexVector), *iter);
        }
    } else {
        for (; iter != end; ++iter) {
            ASSERT(canInsert(*iter));
            reinsert(bitwise_cast<uint32_t*>(indexVector), *iter);
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

inline PropertyTable::ValueType* PropertyTable::table()
{
    // The table of values lies after the hash index.
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag)
        return reinterpret_cast_ptr<ValueType*>((indexVector & indexVectorMask) + (m_indexSize * sizeof(uint8_t)));
    return reinterpret_cast_ptr<ValueType*>((indexVector & indexVectorMask) + (m_indexSize * sizeof(uint32_t)));
}

inline const PropertyTable::ValueType* PropertyTable::table() const
{
    // The table of values lies after the hash index.
    uintptr_t indexVector = m_indexVector;
    if (indexVector & isCompactFlag)
        return reinterpret_cast_ptr<const ValueType*>((indexVector & indexVectorMask) + (m_indexSize * sizeof(uint8_t)));
    return reinterpret_cast_ptr<const ValueType*>((indexVector & indexVectorMask) + (m_indexSize * sizeof(uint32_t)));
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
    return indexSize * (isCompact ? sizeof(uint8_t) : sizeof(unsigned)) + ((indexSize >> 1) + 1) * sizeof(ValueType);
}

inline unsigned PropertyTable::sizeForCapacity(unsigned capacity)
{
    if (capacity < MinimumTableSize / 2)
        return MinimumTableSize;
    return nextPowerOf2(capacity + 1) * 2;
}

inline bool PropertyTable::canInsert(const ValueType& entry)
{
    if (!(usedCount() < tableCapacity()))
        return false;
    if (!isCompact())
        return true;
    return !canBeFitInCompact(entry);
}

template<typename Functor>
inline void PropertyTable::forEachProperty(const Functor& functor) const
{
    for (auto& entry : *this) {
        if (functor(entry) == IterationStatus::Done)
            return;
    }
}

template<typename Functor>
inline void PropertyTable::forEachPropertyMutable(const Functor& functor)
{
    for (auto& entry : *this) {
        if (functor(entry) == IterationStatus::Done)
            return;
    }
}

} // namespace JSC
