#pragma once

#include "circuitTree.h"

#include <vector>

class CircuitGroup;

/** Result of a single circuit match */
struct MatchResult {
    /** The parts of the haystack that were matched, in the same oreder as the
     * children of the given needle. */
    std::vector<CircuitTree*> parts;

    /** The inputs matching the needle's inputs, in the same order. */
    std::vector<WireId*> inputs;

    /** The outputs matching the needle's outputs, in the same order. */
    std::vector<WireId*> outputs;
};

/** Finds every match of the components of `needle` in `haystack`, that is,
 * every subgraph of `haystack` formally matching `needle`. The results are
 * always non-overlapping; whenever multiple potential matches overlap, one of
 * them only is arbitrarily picked and returned. */
std::vector<MatchResult> matchSubcircuit(
        CircuitGroup* needle,       ///< Subgroup to find
        CircuitGroup* haystack      ///< Group to be searched in
        );
