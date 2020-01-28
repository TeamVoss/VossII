#pragma once

#include <cstdlib>
#include <exception>
#include <string>

/** Dynamically sized bitset
 *
 * Makes up for the lack of a `std::bitset` equivalent that can have
 * runtime-defined size. `std::vector<bool>` does not support efficient bitwise
 * operations, and is not well-defined apart from "more space-efficient"
 * (leaves a great deal of liberty to the implementation).
 *
 * The size of the bitset must be known at allocation time, and cannot be
 * changed afterwards (apart from copying the bitset to a freshly allocated
 * one).
 */
class DynBitset {
    private:
        typedef long unsigned Word;

    public:
        /// Thrown whenever two `DynBitset`s of mismatched sizes are used
        /// together
        class SizeMismatch : public std::exception {};

        class Reference {
            friend class DynBitset;
            public:
                /// Sets the bit to a given value
                Reference& operator=(bool);
                /// Sets the bit to a given value
                Reference& operator=(const Reference&);

                /// Flips and returns the bit
                inline bool operator~() const {
                    return ~(*this);
                }

                /// Value of the reference
                inline operator bool() const {
                    return (*word) & (1lu << pos);
                }

                /// In-place flips the bit
                Reference& flip() {
                    (*word) ^= (1lu << pos);
                    return *this;
                }

                /// Sets the bit (slightly faster than ` = true`)
                inline void set() {
                    (*word) |= (1lu << pos);
                }

                /// Resets the bit (slightly faster than ` = false`)
                void reset() {
                    (*word) &= ~(1lu << pos);
                }

            private:
                Reference(DynBitset::Word* word, size_t pos)
                    : word(word), pos(pos) {}

                DynBitset::Word* word;
                size_t pos;
        };
        friend class Reference;

        // === Constructors and default operations ===

        /// `size` is expressed in bits. Initializes to zeroes.
        DynBitset(size_t size);
        DynBitset(const DynBitset& oth);
        ~DynBitset();
        void operator=(const DynBitset& oth);
        DynBitset& operator=(DynBitset&& oth) = delete;
    private:
        /// Constructor with pre-set value
        DynBitset(size_t size, Word* words);
        DynBitset(DynBitset&& oth);

    public:
        // === Other methods ===

        inline size_t size() const { return size_; }

        /// Constant bit access operator
        inline bool operator[](size_t pos) const {
            return (data[pos/word_size]) & (1lu << (pos % word_size));
        }

        /// Alterable bit reference
        Reference operator[](size_t pos);

        ///< In-place bitwise and
        DynBitset& operator &=(const DynBitset& oth);

        /// In-place bitwise or
        DynBitset& operator|=(const DynBitset& oth);

        /// In-place bitwise xor
        DynBitset& operator^=(const DynBitset& oth);

        /// In-place bitwise flip
        DynBitset& flip();

        /// Bitwise AND of two `DynBitset`s
        DynBitset operator &(const DynBitset& oth) const;

        /// Bitwise OR of two `DynBitset`s
        DynBitset operator|(const DynBitset& oth) const;

        /// Bitwise XOR of two `DynBitset`s
        DynBitset operator^(const DynBitset& oth) const;

        /// Bitwise flip
        DynBitset operator~() const;

        /// Sets all bits to false
        void reset();

        /// Checks if any bit is true
        bool any() const;

        /// Checks if any bit above the `pos`th (incl.) is true
        bool anyOver(size_t pos) const;

        /// Checks if a single bit is set
        /** Checks whether a single bit is set. If so, returns this bit's
         * position; if no or multiple bits are set, returns -1. */
        int singleBit() const;

        /// Dumps the DynBitset to an hex representation
        std::string dump() const;

    private:
        inline void checkSize(const DynBitset& oth) const {
            if(size_ != oth.size_)
                throw SizeMismatch();
        }

        inline size_t nbWords() const {
            return (size_ + word_size - 1) / word_size;
        }

        /** Aux function for `singleBit`, does the same on a single word.
         * Assumes every bit past `upTo` is false. */
        int whichBit(Word word, int offset = 0) const;

        const size_t size_;
        Word* data;

        constexpr static size_t word_size = sizeof(Word) * 8;
};
