/** Contains a few useful functions to check whether a group is equal to
 * another group (formally). Included for `CircuitGroup`.
 */

#pragma once

#include "circuitTree.h"
#include <vector>
#include <unordered_map>
#include <set>

namespace groupEquality {
    typedef std::vector<std::vector<CircuitTree*> > SigSplit;
    typedef std::unordered_map<sign_t, std::set<CircuitTree*> > SigSplitMapped;

    class TooManyPermutations : public std::exception {};

    class Permutation {
        public:
            typedef std::vector<int> PermElem;

            Permutation(const SigSplit& split);
            const PermElem& operator[](sign_t index) const;

            /// Next "meta-permutation"
            bool next();

        private:
            typedef std::vector<PermElem> PermStruct;

            void mkIdentity(std::vector<int>& vect);
            inline PermStruct::iterator lastIter();

            PermStruct perms;
            PermStruct::iterator nextChange;
    };

    /// Computes k!
    int factorial(int k);

    /// Computes the signature of a wire with given accuracy
    sign_t wireSignature(WireId* wire, int accuracy = -1);

    /** Splits a set of circuits into sets of circuits with the same signatures
     * @param circuits The wires to consider
     * @param splitted A reference to the vector that will be filled
     * @param signatures Will be filled with the list of signatures of each
     *        chunk from `splitted`
     * @param maxPermutations Stop if the number of permutations exceeds this
     *        parameter, and raise TooManyPermutations
     * @param accuracy Level of accuracy of the signature function, or -1 for
     *        the default value
     */
    void splitOnSig(const std::vector<CircuitTree*> circuits,
            SigSplit& splitted,
            std::vector<sign_t>& signatures,
            int maxPermutations = -1,
            int accuracy = -1);

    /** Checks that both `fst` and `snd` have the same keys, and sets of equal
     * sizes for each key. */
    bool equalSizes(
            const SigSplit& fst,
            const SigSplit& snd);

    bool equalWithPermutation(
            const SigSplit& leftSplit, const SigSplit& rightSplit,
            const Permutation& perm);

    bool equal(CircuitGroup* left, CircuitGroup* right);
}
