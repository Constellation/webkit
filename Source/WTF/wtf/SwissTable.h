/*
 * Copyright (C) 2023 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1.  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 * 2.  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * This HashTable implements SwissTable from Abseil and using some techniques described in F14 table.
 *
 * Copyright 2018 The Abseil Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#pragma once

#include <atomic>
#include <iterator>
#include <mutex>
#include <string.h>
#include <type_traits>
#include <utility>
#include <wtf/Assertions.h>
#include <wtf/DebugHeap.h>
#include <wtf/FastMalloc.h>
#include <wtf/HashMap.h>
#include <wtf/HashSet.h>
#include <wtf/HashTable.h>
#include <wtf/HashTraits.h>
#include <wtf/Lock.h>
#include <wtf/MathExtras.h>
#include <wtf/StdLibExtras.h>
#include <wtf/ValueCheck.h>
#include <wtf/WeakRandomNumber.h>
#if CPU(X86_SSE2)
#include <emmintrin.h>
#include <immintrin.h>
#endif
#if CPU(X86_SSE3)
#include <tmmintrin.h>
#endif
#if CPU(ARM64)
#include <arm_neon.h>
#endif

namespace WTF {

DECLARE_ALLOCATOR_WITH_HEAP_IDENTIFIER(SwissTable);

struct SwissTableSizePolicy {
    static constexpr unsigned maxLoadNumerator = 3;
    static constexpr unsigned maxLoadDenominator = 4;
    static constexpr unsigned minLoad = 6;
};

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
class SwissTable {
public:
    using HashTableType = SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>;
    using iterator = HashTableIterator<HashTableType, Key, Value, Extractor, HashFunctions, Traits, KeyTraits>;
    using const_iterator = HashTableConstIterator<HashTableType, Key, Value, Extractor, HashFunctions, Traits, KeyTraits>;
    using ValueTraits = Traits;
    using KeyType = Key;
    using ValueType = Value;
    using IdentityTranslatorType = IdentityHashTranslator<ValueTraits, HashFunctions>;
    using AddResult = HashTableAddResult<iterator>;

    static constexpr unsigned maxLoadNumerator = SizePolicy::maxLoadNumerator;
    static constexpr unsigned maxLoadDenominator = SizePolicy::maxLoadDenominator;
    static constexpr unsigned minLoad = SizePolicy::minLoad;

    SwissTable() = default;
    ~SwissTable()
    {
        invalidateIterators(this);
        if (m_table)
            deallocateTable(m_table);
    }

    SwissTable(const SwissTable&);
    void swap(SwissTable&);
    SwissTable& operator=(const SwissTable&);

    SwissTable(SwissTable&&);
    SwissTable& operator=(SwissTable&&);

    // When the hash table is empty, just return the same iterator for end as for begin.
    // This is more efficient because we don't have to skip all the empty and deleted
    // buckets, and iterating an empty table is a common case that's worth optimizing.
    iterator begin() { return isEmpty() ? end() : makeIterator(m_table); }
    iterator end() { return makeKnownGoodIterator(m_table + tableSize()); }
    const_iterator begin() const { return isEmpty() ? end() : makeConstIterator(m_table); }
    const_iterator end() const { return makeKnownGoodConstIterator(m_table + tableSize()); }

    iterator random()
    {
        if (isEmpty())
            return end();

        while (true) {
            auto& bucket = m_table[weakRandomNumber<uint32_t>() & tableSizeMask()];
            if (!isEmptyOrDeletedBucket(bucket))
                return makeKnownGoodIterator(&bucket);
        };
    }

    const_iterator random() const { return static_cast<const_iterator>(const_cast<SwissTable*>(this)->random()); }

    unsigned size() const { return keyCount(); }
    unsigned capacity() const { return tableSize(); }
    bool isEmpty() const { return !keyCount(); }

    void reserveInitialCapacity(unsigned keyCount)
    {
        ASSERT(!m_table);
        ASSERT(!tableSize());

        unsigned newTableSize = std::max(minimumTableSize, computeBestTableSize(keyCount));

        m_table = allocateTable(newTableSize);
        setTableSize(newTableSize);
        setTableSizeMask(newTableSize - 1);
        setDeletedCount(0);
        setKeyCount(0);
    }

    AddResult add(const ValueType& value) { return add<IdentityTranslatorType>(Extractor::extract(value), value); }
    AddResult add(ValueType&& value) { return add<IdentityTranslatorType>(Extractor::extract(value), WTFMove(value)); }

    // A special version of add() that finds the object by hashing and comparing
    // with some other type, to avoid the cost of type conversion if the object is already
    // in the table.
    template<typename HashTranslator, typename T, typename Extra> AddResult add(T&& key, Extra&&);
    template<typename HashTranslator, typename T, typename Extra> AddResult addPassingHashCode(T&& key, Extra&&);

    iterator find(const KeyType& key) { return find<IdentityTranslatorType>(key); }
    const_iterator find(const KeyType& key) const { return find<IdentityTranslatorType>(key); }
    bool contains(const KeyType& key) const { return contains<IdentityTranslatorType>(key); }

    template<typename HashTranslator, typename T> iterator find(const T&);
    template<typename HashTranslator, typename T> const_iterator find(const T&) const;
    template<typename HashTranslator, typename T> bool contains(const T&) const;

    void remove(const KeyType&);
    void remove(iterator);
    void removeWithoutEntryConsistencyCheck(iterator);
    void removeWithoutEntryConsistencyCheck(const_iterator);
    template<typename Functor>
    bool removeIf(const Functor&);
    void clear();

    static bool isEmptyBucket(const ValueType& value) { return isHashTraitsEmptyValue<KeyTraits>(Extractor::extract(value)); }
    static bool isReleasedWeakBucket(const ValueType& value) { return isHashTraitsReleasedWeakValue<KeyTraits>(Extractor::extract(value)); }
    static bool isDeletedBucket(const ValueType& value) { return KeyTraits::isDeletedValue(Extractor::extract(value)); }
    static bool isEmptyOrDeletedBucket(const ValueType& value) { return isEmptyBucket(value) || isDeletedBucket(value); }

    ValueType* lookup(const Key& key) { return lookup<IdentityTranslatorType>(key); }
    template<typename HashTranslator, typename T> ValueType* lookup(const T&);
    template<typename HashTranslator, typename T> ValueType* inlineLookup(const T&);

#if ASSERT_ENABLED
    void checkTableConsistency() const;
#else
    static void checkTableConsistency() { }
#endif
    static void internalCheckTableConsistencyExceptSize() { }
    static void internalCheckTableConsistency() { }

private:
    static ValueType* allocateTable(unsigned size);
    static void deallocateTable(ValueType* table);

    typedef std::pair<ValueType*, bool> LookupType;

    LookupType lookupForWriting(const Key& key) { return lookupForWriting<IdentityTranslatorType>(key, IdentityTranslatorType::hash(key)); }
    template<typename HashTranslator, typename T> LookupType lookupForWriting(const T&, unsigned hash);

    template<typename HashTranslator, typename T, typename Extra> void addUniqueForInitialization(T&& key, Extra&&);

    template<typename HashTranslator, typename T> void checkKey(const T&);

    void removeAndInvalidateWithoutEntryConsistencyCheck(ValueType*);
    void removeAndInvalidate(ValueType*);
    void remove(ValueType*);

    static constexpr unsigned computeBestTableSize(unsigned keyCount);

    static constexpr bool shouldExpand(uint64_t keyCount, uint64_t tableSize)
    {
        return keyCount * maxLoadDenominator >= tableSize * maxLoadNumerator;
    }

    bool shouldExpand() const
    {
        unsigned keyCount = this->keyCount();
        unsigned tableSize = this->tableSize();
        return shouldExpand(keyCount, tableSize);
    }

    bool mustRehashInPlace() const { return keyCount() * minLoad < tableSize() * 2; }
    bool shouldShrink() const { return keyCount() * minLoad < tableSize() && tableSize() > minimumTableSize; }
    ValueType* expand(ValueType* entry = nullptr);
    void shrink() { rehash(tableSize() / 2, nullptr); }
    void shrinkToBestSize();

    ValueType* rehash(unsigned newTableSize, ValueType* entry);
    ValueType* reinsert(ValueType&&);

    static void initializeBucket(ValueType& bucket);
    static void deleteBucket(ValueType& bucket) { hashTraitsDeleteBucket<Traits>(bucket); }

    iterator makeIterator(ValueType* pos) { return iterator(this, pos, m_table + tableSize()); }
    const_iterator makeConstIterator(ValueType* pos) const { return const_iterator(this, pos, m_table + tableSize()); }
    iterator makeKnownGoodIterator(ValueType* pos) { return iterator(this, pos, m_table + tableSize(), HashItemKnownGood); }
    const_iterator makeKnownGoodConstIterator(ValueType* pos) const { return const_iterator(this, pos, m_table + tableSize(), HashItemKnownGood); }

#if ASSERT_ENABLED
    void checkTableConsistencyExceptSize() const;
#else
    static void checkTableConsistencyExceptSize() { }
#endif

    static constexpr int tableSizeOffset = -1;
    static constexpr int tableSizeMaskOffset = -2;
    static constexpr int keyCountOffset = -3;
    static constexpr int deletedCountOffset = -4;
    static constexpr unsigned metadataSize = 4 * sizeof(unsigned);

    using Control = uint8_t;
    enum ControlValue : Control {
        Empty   = 0b10000000,
        Deleted = 0b11111111,
    };

    ALWAYS_INLINE unsigned h1(unsigned hash)
    {
        return hash;
    }

    // We extract [17, 24] bits since StringImpl only offers 24 bits.
    inline Control h2(unsigned hash)
    {
        return (hash >> 17) & 0x7F;
    }

    template <class T, unsigned shift>
    class BitMask {
        static_assert(std::is_unsigned<T>::value);

    public:
        using value_type = unsigned;
        using iterator = BitMask;
        using const_iterator = BitMask;

        explicit constexpr BitMask(T mask)
            : m_mask(mask)
        {
        }

        BitMask& operator++()
        {
            m_mask &= (m_mask - 1);
            return *this;
        }

        explicit operator bool() const { return m_mask != 0; }
        unsigned operator*() const
        {
            if (!m_mask)
                return UINT32_MAX;
            return getLSBSet(m_mask) >> shift;
        }
        std::optional<unsigned> lowestBitSet() const
        {
            if (!m_mask)
                return std::nullopt;
            return getLSBSet(m_mask) >> shift;
        }

        std::optional<unsigned> highestBitSet() const
        {
            if (!m_mask)
                return std::nullopt;
            return getMSBSet(m_mask) >> shift;
        }

        BitMask begin() const { return *this; }
        BitMask end() const { return BitMask(0); }

        unsigned trailingZeros() const
        {
            if (!m_mask)
                return UINT32_MAX;
            return ctz(m_mask) >> shift;
        }

        unsigned leadingZeros() const
        {
            if (!m_mask)
                return UINT32_MAX;
            return clz(m_mask) >> shift;
        }

    private:
        friend bool operator==(const BitMask& a, const BitMask& b)
        {
            return a.m_mask == b.m_mask;
        }
        friend bool operator!=(const BitMask& a, const BitMask& b)
        {
            return a.m_mask != b.m_mask;
        }

        T m_mask;
    };


    struct GroupStorage;
#if CPU(X86_SSE2)
    struct Group {
        static constexpr unsigned numberOfEntries = 16;

        explicit Group(GroupStorage* storage)
        {
            m_vector = _mm_loadu_si128(bitwise_cast<const __m128i*>(storage));
        }

        using Mask = BitMask<uint16_t, /* 1 bit */ 0>;
        Mask matchEmptyOrDeleted() const
        {
            return Mask(_mm_movemask_epi8(m_vector));
        }

        Mask match(Control control) const
        {
            auto pattern = _mm_set1_epi8(control);
            return Mask(_mm_movemask_epi8(_mm_cmpeq_epi8(pattern, m_vector)));
        }

        Mask matchEmpty()
        {
#if CPU(X86_SSE3)
            static_assert(static_cast<int8_t>(ControlValue::Empty) == -128);
            return Mask(_mm_movemask_epi8(_mm_sign_epi8(m_vector, m_vector)));
#else
            return match(ControlValue::Empty);
#endif
        }

        // https://github.com/abseil/abseil-cpp/issues/209
        // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87853
        // _mm_cmpgt_epi8 is broken under GCC with -funsigned-char
        inline static __m128i _mm_cmpgt_epi8_fixed(__m128i a, __m128i b)
        {
#if COMPILER(GCC)
            if constexpr (std::is_unsigned_v<char>) {
                const __m128i mask = _mm_set1_epi8(0x80);
                const __m128i diff = _mm_subs_epi8(b, a);
                return _mm_cmpeq_epi8(_mm_and_si128(diff, mask), mask);
            }
#endif
            return _mm_cmpgt_epi8(a, b);
        }

        __m128i m_vector;
    };
#elif CPU(ARM64)
    struct Group {
        static constexpr unsigned numberOfEntries = 16;
        static constexpr uint64_t msbs = 0x8888888888888888ULL;

        explicit Group(GroupStorage* storage)
        {
            m_vector = vld1q_u8(bitwise_cast<const uint8_t*>(storage));
        }

        using Mask = BitMask<uint64_t, /* 4bits */ 2>;
        Mask matchEmptyOrDeleted() const
        {
            auto result = vreinterpretq_u8_s8(vshrq_n_s8(vreinterpretq_s8_u8(m_vector), 7));
            auto reduced = vshrn_n_u16(vreinterpretq_u16_u8(result), 4); // 11111111|00000000 => 11110000
            return Mask(vget_lane_u64(vreinterpret_u64_u8(reduced), 0) & msbs);
        }

        Mask match(Control control) const
        {
            auto pattern = vdupq_n_u8(control); // XXXX => XXXX x8
            auto result = vceqq_u8(pattern, m_vector); // XXXX x8 <=> vector
            auto reduced = vshrn_n_u16(vreinterpretq_u16_u8(result), 4); // 11111111|00000000 => 11110000
            return Mask(vget_lane_u64(vreinterpret_u64_u8(reduced), 0) & msbs);
        }

        Mask matchEmpty()
        {
            return match(ControlValue::Empty);
        }

        uint8x16_t m_vector;
    };
#else
    struct Group {
        static constexpr unsigned numberOfEntries = 8;
        static constexpr uint64_t msbs = 0x8080808080808080ULL;
        static constexpr uint64_t lsbs = 0x0101010101010101ULL;

        explicit Group(GroupStorage* storage)
        {
#if CPU(LITTLE_ENDIAN)
            m_vector = *(bitwise_cast<const uint64_t*>(storage));
#else // !CPU(LITTLE_ENDIAN)
#if COMPILER(GCC_OR_CLANG)
            m_vector = __builtin_bswap64(*(bitwise_cast<const uint64_t*>(storage)));
#elif COMPILER(MSVC)
            m_vector = _byteswap_uint64(*(bitwise_cast<const uint64_t*>(storage)));
#else
#error "Not supported compiler"
#endif
#endif // CPU(LITTLE_ENDIAN)
        }

        using Mask = BitMask<uint64_t, /* 8bits */ 3>;
        Mask matchEmptyOrDeleted() const
        {
            return Mask(m_vector & msbs);
        }

        Mask match(Control control) const
        {
            // For the technique, see:
            // http://graphics.stanford.edu/~seander/bithacks.html##ValueInWord
            // (Determine if a word has a byte equal to n).
            //
            // Caveat: there are false positives but:
            // - they only occur if there is a real match
            // - they never occur on kEmpty, kDeleted, kSentinel
            // - they will be handled gracefully by subsequent checks in code
            //
            // Example:
            //   v = 0x1716151413121110
            //   hash = 0x12
            //   retval = (v - lsbs) & ~v & msbs = 0x0000000080800000
            auto x = m_vector ^ (lsbs * static_cast<uint8_t>(control));
            return Mask((x - lsbs) & ~x & msbs);
        }

        Mask matchEmpty()
        {
            return Mask((m_vector & (~m_vector << 1)) & msbs);
        }

        uint64_t m_vector;
    };
#endif
    struct alignas(Group::numberOfEntries) GroupStorage {
        Control m_controls[Group::numberOfEntries];
    };
    static_assert(sizeof(GroupStorage) == sizeof(Group));

    static constexpr unsigned minimumTableSize = std::max<unsigned>(KeyTraits::minimumTableSize, Group::numberOfEntries);

    unsigned tableSize() const { return m_table ? reinterpret_cast<unsigned*>(m_table)[tableSizeOffset] : 0; }
    void setTableSize(unsigned size) const { ASSERT(m_table); reinterpret_cast<unsigned*>(m_table)[tableSizeOffset] = size; }
    unsigned tableSizeMask() const { ASSERT(m_table); return m_table ? reinterpret_cast<unsigned*>(m_table)[tableSizeMaskOffset] : 0; }
    void setTableSizeMask(unsigned mask) { ASSERT(m_table); reinterpret_cast<unsigned*>(m_table)[tableSizeMaskOffset] = mask; }
    unsigned keyCount() const { return m_table ? reinterpret_cast<unsigned*>(m_table)[keyCountOffset] : 0; }
    void setKeyCount(unsigned count) const { ASSERT(m_table); reinterpret_cast<unsigned*>(m_table)[keyCountOffset] = count; }
    unsigned deletedCount() const { ASSERT(m_table); return reinterpret_cast<unsigned*>(m_table)[deletedCountOffset]; }
    void setDeletedCount(unsigned count) const { ASSERT(m_table); reinterpret_cast<unsigned*>(m_table)[deletedCountOffset] = count; }
    Control* controls() const
    {
        return bitwise_cast<Control*>(groups());
    }
    GroupStorage* groups() const
    {
        unsigned tableSize = this->tableSize();
        return bitwise_cast<GroupStorage*>(bitwise_cast<Control*>(m_table) - metadataSize - tableSize);
    }

    ValueType* m_table { nullptr };
#if CHECK_HASHTABLE_ITERATORS
public:
    // All access to m_iterators should be guarded with m_mutex.
    mutable const_iterator* m_iterators { nullptr };
    // Use std::unique_ptr so HashTable can still be memmove'd or memcpy'ed.
    mutable std::unique_ptr<Lock> m_mutex { makeUnique<Lock>() };
#endif
};

#if !ASSERT_ENABLED

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T>
inline void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::checkKey(const T&)
{
}

#else // ASSERT_ENABLED

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::checkKey(const T& key)
{
    if constexpr (!HashFunctions::safeToCompareToEmptyOrDeleted)
        return;
    ASSERT(!HashTranslator::equal(KeyTraits::emptyValue(), key));
    typename std::aligned_storage<sizeof(ValueType), std::alignment_of<ValueType>::value>::type deletedValueBuffer;
    ValueType* deletedValuePtr = reinterpret_cast_ptr<ValueType*>(&deletedValueBuffer);
    ValueType& deletedValue = *deletedValuePtr;
    Traits::constructDeletedValue(deletedValue);
    ASSERT(!HashTranslator::equal(Extractor::extract(deletedValue), key));
}

#endif // ASSERT_ENABLED

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T>
inline auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::lookup(const T& key) -> ValueType*
{
    return inlineLookup<HashTranslator>(key);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T>
ALWAYS_INLINE auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::inlineLookup(const T& key) -> ValueType*
{
    checkKey<HashTranslator>(key);

    ValueType* table = m_table;
    if (!table)
        return nullptr;

    unsigned h = HashTranslator::hash(key);

    GroupStorage* groups = this->groups();
    unsigned groupHash = h1(h);
    unsigned entryHash = h2(h);
    unsigned groupSize = tableSize() / sizeof(GroupStorage);
    unsigned sizeMask = groupSize - 1;
    unsigned i = groupHash & sizeMask;
    unsigned probeCount = 0;
    while (true) {
        Group group(groups + i);
        for (unsigned position : group.match(entryHash)) {
            ValueType* entry = table + i * Group::numberOfEntries + position;
            ASSERT(!isEmptyBucket(*entry) && !isDeletedBucket(*entry));
            if (LIKELY(HashTranslator::equal(Extractor::extract(*entry), key)))
                return entry;
        }
        if (LIKELY(group.matchEmpty()))
            return nullptr;

        ++probeCount;
        i = (i + probeCount) & sizeMask;
    }
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T>
inline auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::lookupForWriting(const T& key, unsigned h) -> LookupType
{
    ASSERT(m_table);
    checkKey<HashTranslator>(key);

    ValueType* table = m_table;

    GroupStorage* groups = this->groups();
    unsigned groupHash = h1(h);
    unsigned entryHash = h2(h);
    unsigned groupSize = tableSize() / sizeof(GroupStorage);
    unsigned sizeMask = groupSize - 1;
    {
        unsigned i = groupHash & sizeMask;
        unsigned probeCount = 0;
        while (true) {
            Group group(groups + i);
            for (unsigned position : group.match(entryHash)) {
                ValueType* entry = table + i * Group::numberOfEntries + position;
                ASSERT(!isEmptyBucket(*entry) && !isDeletedBucket(*entry));
                if (LIKELY(HashTranslator::equal(Extractor::extract(*entry), key)))
                    return LookupType(entry, true);
            }
            if (LIKELY(group.matchEmpty()))
                break;

            ++probeCount;
            i = (i + probeCount) & sizeMask;
        }
    }
    {
        unsigned i = groupHash & sizeMask;
        unsigned probeCount = 0;
        while (true) {
            Group group(groups + i);
            if (auto position = group.matchEmptyOrDeleted().lowestBitSet())
                return LookupType(table + i * Group::numberOfEntries + position.value(), false);

            ++probeCount;
            i = (i + probeCount) & sizeMask;
        }
    }
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T, typename Extra>
ALWAYS_INLINE void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::addUniqueForInitialization(T&& key, Extra&& extra)
{
    ASSERT(m_table);

    checkKey<HashTranslator>(key);

    ValueType* table = m_table;
    unsigned h = HashTranslator::hash(key);

    ValueType* entry;
    GroupStorage* groups = this->groups();
    unsigned groupHash = h1(h);
    unsigned groupSize = tableSize() / sizeof(GroupStorage);
    unsigned sizeMask = groupSize - 1;
    unsigned i = groupHash & sizeMask;
    unsigned probeCount = 0;
    while (true) {
        Group group(groups + i);
        if (auto position = group.matchEmpty().lowestBitSet()) {
            entry = table + i * Group::numberOfEntries + position.value();
            break;
        }
        ++probeCount;
        i = (i + probeCount) & sizeMask;
    }
    controls()[entry - table] = h2(h);
    HashTranslator::translate(*entry, std::forward<T>(key), std::forward<Extra>(extra));
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::initializeBucket(ValueType& bucket)
{
    HashTableBucketInitializer<Traits::emptyValueIsZero>::template initialize<Traits>(bucket);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T, typename Extra>
ALWAYS_INLINE auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::add(T&& key, Extra&& extra) -> AddResult
{
    checkKey<HashTranslator>(key);

    invalidateIterators(this);

    if (!m_table)
        expand(nullptr);

    internalCheckTableConsistency();

    ASSERT(m_table);

    ValueType* table = m_table;
    unsigned h = HashTranslator::hash(key);

    ValueType* deletedEntry = 0;
    ValueType* entry;
    GroupStorage* groups = this->groups();
    unsigned groupHash = h1(h);
    unsigned entryHash = h2(h);
    unsigned groupSize = tableSize() / sizeof(GroupStorage);
    unsigned sizeMask = groupSize - 1;
    {
        unsigned i = groupHash & sizeMask;
        unsigned probeCount = 0;
        while (true) {
            Group group(groups + i);
            for (unsigned position : group.match(entryHash)) {
                ValueType* entry = table + i * Group::numberOfEntries + position;
                ASSERT(!isEmptyBucket(*entry) && !isDeletedBucket(*entry));
                if (LIKELY(HashTranslator::equal(Extractor::extract(*entry), key)))
                    return AddResult(makeKnownGoodIterator(entry), false);
            }
            if (LIKELY(group.matchEmpty()))
                break;

            ++probeCount;
            i = (i + probeCount) & sizeMask;
        }
    }
    {
        unsigned i = groupHash & sizeMask;
        unsigned probeCount = 0;
        while (true) {
            Group group(groups + i);
            if (auto position = group.matchEmptyOrDeleted().lowestBitSet()) {
                entry = table + i * Group::numberOfEntries + position.value();
                if (isDeletedBucket(*entry))
                    deletedEntry = entry;
                break;
            }

            ++probeCount;
            i = (i + probeCount) & sizeMask;
        }
    }

    if (deletedEntry) {
        initializeBucket(*deletedEntry);
        entry = deletedEntry;
        setDeletedCount(deletedCount() - 1);
    }

    HashTranslator::translate(*entry, std::forward<T>(key), std::forward<Extra>(extra));
    if (auto* controls = this->controls())
        controls[entry - table] = h2(h);
    setKeyCount(keyCount() + 1);

    if (shouldExpand())
        entry = expand(entry);

    internalCheckTableConsistency();

    return AddResult(makeKnownGoodIterator(entry), true);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename HashTranslator, typename T, typename Extra>
inline auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::addPassingHashCode(T&& key, Extra&& extra) -> AddResult
{
    checkKey<HashTranslator>(key);

    invalidateIterators(this);

    if (!m_table)
        expand();

    internalCheckTableConsistency();

    unsigned h = HashTranslator::hash(key);
    LookupType lookupResult = lookupForWriting<HashTranslator>(key, h);

    ValueType* entry = lookupResult.first;
    bool found = lookupResult.second;

    if (found)
        return AddResult(makeKnownGoodIterator(entry), false);

    if (isDeletedBucket(*entry)) {
        initializeBucket(*entry);
        setDeletedCount(deletedCount() - 1);
    }

    HashTranslator::translate(*entry, std::forward<T>(key), std::forward<Extra>(extra), h);
    if (auto* controls = this->controls())
        controls[entry - m_table] = h2(h);
    setKeyCount(keyCount() + 1);

    if (shouldExpand())
        entry = expand(entry);

    internalCheckTableConsistency();

    return AddResult(makeKnownGoodIterator(entry), true);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::reinsert(ValueType&& entry) -> ValueType*
{
    ASSERT(m_table);
    ASSERT(!lookupForWriting(Extractor::extract(entry)).second);
    ASSERT(!isDeletedBucket(*(lookupForWriting(Extractor::extract(entry)).first)));
    const auto& key = Extractor::extract(entry);
    unsigned hash = IdentityTranslatorType::hash(key);
    ValueType* newEntry = lookupForWriting<IdentityTranslatorType>(key, hash).first;
    newEntry->~Value();
    new (NotNull, newEntry) ValueType(WTFMove(entry));
    if (auto* controls = this->controls())
        controls[newEntry - m_table] = h2(hash);

    return newEntry;
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template <typename HashTranslator, typename T>
auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::find(const T& key) -> iterator
{
    if (!m_table)
        return end();

    ValueType* entry = lookup<HashTranslator>(key);
    if (!entry)
        return end();

    return makeKnownGoodIterator(entry);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template <typename HashTranslator, typename T>
auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::find(const T& key) const -> const_iterator
{
    if (!m_table)
        return end();

    ValueType* entry = const_cast<SwissTable*>(this)->lookup<HashTranslator>(key);
    if (!entry)
        return end();

    return makeKnownGoodConstIterator(entry);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template <typename HashTranslator, typename T>
bool SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::contains(const T& key) const
{
    if (!m_table)
        return false;

    return const_cast<SwissTable*>(this)->lookup<HashTranslator>(key);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::removeAndInvalidateWithoutEntryConsistencyCheck(ValueType* pos)
{
    invalidateIterators(this);
    remove(pos);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::removeAndInvalidate(ValueType* pos)
{
    invalidateIterators(this);
    internalCheckTableConsistency();
    remove(pos);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::remove(ValueType* pos)
{
    deleteBucket(*pos);
    if (auto* controls = this->controls())
        controls[pos - m_table] = ControlValue::Deleted;
    setDeletedCount(deletedCount() + 1);
    setKeyCount(keyCount() - 1);

    if (shouldShrink())
        shrink();

    internalCheckTableConsistency();
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::remove(iterator it)
{
    if (it == end())
        return;

    removeAndInvalidate(const_cast<ValueType*>(it.m_iterator.m_position));
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::removeWithoutEntryConsistencyCheck(iterator it)
{
    if (it == end())
        return;

    removeAndInvalidateWithoutEntryConsistencyCheck(const_cast<ValueType*>(it.m_iterator.m_position));
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::removeWithoutEntryConsistencyCheck(const_iterator it)
{
    if (it == end())
        return;

    removeAndInvalidateWithoutEntryConsistencyCheck(const_cast<ValueType*>(it.m_position));
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::remove(const KeyType& key)
{
    remove(find(key));
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
template<typename Functor>
inline bool SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::removeIf(const Functor& functor)
{
    // We must use local copies in case "functor" or "deleteBucket"
    // make a function call, which prevents the compiler from keeping
    // the values in register.
    unsigned removedBucketCount = 0;
    ValueType* table = m_table;

    for (unsigned i = tableSize(); i--;) {
        ValueType& bucket = table[i];
        if (isEmptyOrDeletedBucket(bucket))
            continue;

        if (!functor(bucket))
            continue;

        deleteBucket(bucket);
        if (auto* controls = this->controls())
            controls[i] = ControlValue::Deleted;
        ++removedBucketCount;
    }
    if (removedBucketCount) {
        setDeletedCount(deletedCount() + removedBucketCount);
        setKeyCount(keyCount() - removedBucketCount);
    }

    if (shouldShrink())
        shrinkToBestSize();

    internalCheckTableConsistency();
    return removedBucketCount;
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::allocateTable(unsigned size) -> ValueType*
{
    static_assert(!(metadataSize % alignof(ValueType)));

    size_t allocationSize = metadataSize + size + size * sizeof(ValueType);
    size_t offset = metadataSize + size;

    ValueType* result = nullptr;

    // would use a template member function with explicit specializations here, but
    // gcc doesn't appear to support that
    if constexpr (Traits::emptyValueIsZero)
        result = reinterpret_cast<ValueType*>(static_cast<char*>(SwissTableMalloc::zeroedMalloc(allocationSize)) + offset);
    else {
        result = reinterpret_cast<ValueType*>(static_cast<char*>(SwissTableMalloc::malloc(allocationSize)) + offset);
        for (unsigned i = 0; i < size; i++)
            initializeBucket(result[i]);
    }

    memset(bitwise_cast<Control*>(result) - offset, ControlValue::Empty, size);
    return result;
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::deallocateTable(ValueType* table)
{
    unsigned size = bitwise_cast<unsigned*>(table)[tableSizeOffset];
    for (unsigned i = 0; i < size; ++i) {
        if (!isDeletedBucket(table[i]))
            table[i].~ValueType();
    }
    SwissTableMalloc::free(reinterpret_cast<char*>(table) - metadataSize - size);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::expand(ValueType* entry) -> ValueType*
{
    unsigned newSize;
    unsigned oldSize = tableSize();
    if (!oldSize)
        newSize = minimumTableSize;
    else if (mustRehashInPlace())
        newSize = oldSize;
    else
        newSize = oldSize * 2;

    return rehash(newSize, entry);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
constexpr unsigned SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::computeBestTableSize(unsigned keyCount)
{
    unsigned bestTableSize = WTF::roundUpToPowerOfTwo(keyCount);

    if (shouldExpand(keyCount, bestTableSize))
        bestTableSize *= 2;

    auto aboveThresholdForEagerExpansion = [](double loadFactor, unsigned keyCount, unsigned tableSize)
    {
        // Here is the rationale behind this calculation, using 3/4 load-factor.
        // With maxLoad at 3/4 and minLoad at 1/6, our average load is 11/24.
        // If we are getting half-way between 11/24 and 3/4, we double the size
        // to avoid being too close to loadMax and bring the ratio close to 11/24. This
        // give us a load in the bounds [9/24, 15/24).
        double maxLoadRatio = loadFactor;
        double minLoadRatio = 1.0 / minLoad;
        double averageLoadRatio = (maxLoadRatio + minLoadRatio) / 2;
        double halfWayBetweenAverageAndMaxLoadRatio = (averageLoadRatio + maxLoadRatio) / 2;
        return keyCount >= tableSize * halfWayBetweenAverageAndMaxLoadRatio;
    };

    constexpr double loadFactor = static_cast<double>(maxLoadNumerator) / maxLoadDenominator;
    if (aboveThresholdForEagerExpansion(loadFactor, keyCount, bestTableSize))
        bestTableSize *= 2;
    return std::max(bestTableSize, minimumTableSize);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::shrinkToBestSize()
{
    rehash(std::max(minimumTableSize, computeBestTableSize(keyCount())), nullptr);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::rehash(unsigned newTableSize, ValueType* entry) -> ValueType*
{
    internalCheckTableConsistencyExceptSize();

    unsigned oldTableSize = tableSize();
    ValueType* oldTable = m_table;
    unsigned oldKeyCount = keyCount();
    m_table = allocateTable(newTableSize);
    setTableSize(newTableSize);
    setTableSizeMask(newTableSize - 1);
    setDeletedCount(0);
    setKeyCount(oldKeyCount);

    Value* newEntry = nullptr;
    for (unsigned i = 0; i != oldTableSize; ++i) {
        auto& oldEntry = oldTable[i];
        if (isDeletedBucket(oldEntry)) {
            ASSERT(std::addressof(oldEntry) != entry);
            continue;
        }

        if (isEmptyBucket(oldEntry)) {
            ASSERT(std::addressof(oldEntry) != entry);
            oldTable[i].~ValueType();
            continue;
        }

        if (isReleasedWeakBucket(oldEntry)) {
            ASSERT(std::addressof(oldEntry) != entry);
            oldEntry.~ValueType();
            setKeyCount(keyCount() - 1);
            continue;
        }

        Value* reinsertedEntry = reinsert(WTFMove(oldEntry));
        oldEntry.~ValueType();
        if (std::addressof(oldEntry) == entry) {
            ASSERT(!newEntry);
            newEntry = reinsertedEntry;
        }
    }

    if (oldTable)
        SwissTableMalloc::free(reinterpret_cast<char*>(oldTable) - metadataSize - oldTableSize);

    internalCheckTableConsistency();
    return newEntry;
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::clear()
{
    invalidateIterators(this);
    if (!m_table)
        return;

    deallocateTable(m_table);
    m_table = nullptr;
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::SwissTable(const SwissTable& other)
    : m_table(nullptr)
{
    unsigned otherKeyCount = other.size();
    if (!otherKeyCount)
        return;

    unsigned bestTableSize = computeBestTableSize(otherKeyCount);
    m_table = allocateTable(bestTableSize);
    setTableSize(bestTableSize);
    setTableSizeMask(bestTableSize - 1);
    setKeyCount(otherKeyCount);
    setDeletedCount(0);

    invalidateIterators(this);

    for (const auto& otherValue : other)
        addUniqueForInitialization<IdentityTranslatorType>(Extractor::extract(otherValue), otherValue);

    internalCheckTableConsistency();
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::swap(SwissTable& other)
{
    invalidateIterators(this);
    invalidateIterators(&other);

    std::swap(m_table, other.m_table);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::operator=(const SwissTable& other) -> SwissTable&
{
    SwissTable tmp(other);
    swap(tmp);
    return *this;
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::SwissTable(SwissTable&& other)
{
    invalidateIterators(&other);

    m_table = std::exchange(other.m_table, nullptr);
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
inline auto SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::operator=(SwissTable&& other) -> SwissTable&
{
    SwissTable temp = WTFMove(other);
    swap(temp);
    return *this;
}

#if ASSERT_ENABLED

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::checkTableConsistency() const
{
    checkTableConsistencyExceptSize();
    ASSERT(!m_table || !shouldExpand());
}

template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits, typename SizePolicy>
void SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SizePolicy>::checkTableConsistencyExceptSize() const
{
    if (!m_table)
        return;

    unsigned count = 0;
    unsigned deletedCount = 0;
    unsigned tableSize = this->tableSize();
    Control* controls = this->controls();
    for (unsigned j = 0; j < tableSize; ++j) {
        ValueType* entry = m_table + j;
        if (isEmptyBucket(*entry)) {
            if (controls)
                ASSERT(controls[j] == ControlValue::Empty);
            continue;
        }

        if (isDeletedBucket(*entry)) {
            if (controls)
                ASSERT(controls[j] == ControlValue::Deleted);
            ++deletedCount;
            continue;
        }

        if (controls)
            ASSERT(controls[j] != ControlValue::Empty && controls[j] != ControlValue::Deleted);
        auto& key = Extractor::extract(*entry);
        const_iterator it = find(key);
        ASSERT(entry == it.m_position);
        ++count;

        ValueCheck<Key>::checkConsistency(key);
    }

    ASSERT(count == keyCount());
    ASSERT(deletedCount == this->deletedCount());
    ASSERT(this->tableSize() >= minimumTableSize);
    ASSERT(tableSizeMask());
    ASSERT(this->tableSize() == tableSizeMask() + 1);
}

#endif // ASSERT_ENABLED

struct SwissTableTraits {
    template<typename Key, typename Value, typename Extractor, typename HashFunctions, typename Traits, typename KeyTraits>
    using TableType = SwissTable<Key, Value, Extractor, HashFunctions, Traits, KeyTraits, SwissTableSizePolicy>;
};

template<typename KeyArg, typename MappedArg, typename HashArg = DefaultHash<KeyArg>, typename KeyTraitsArg = HashTraits<KeyArg>, typename MappedTraitsArg = HashTraits<MappedArg>>
using SwissMap = HashMap<KeyArg, MappedArg, HashArg, KeyTraitsArg, MappedTraitsArg, SwissTableTraits>;

template<typename ValueArg, typename HashArg = DefaultHash<ValueArg>, typename TraitsArg = HashTraits<ValueArg>>
using SwissSet = HashSet<ValueArg, HashArg, TraitsArg, SwissTableTraits>;

} // namespace WTF
