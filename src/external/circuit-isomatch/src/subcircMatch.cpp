#include "subcircMatch.h"
#include <map>
#include <set>
#include <unordered_map>
#include <vector>
#include <list>
#include <stdexcept>
#include <algorithm>

#include "circuitGroup.h"
#include "dyn_bitset.h"
#include "logging.h"
#include "debug.h"

using namespace std;

#ifdef DEBUG_FIND
class ImplementationBug: public std::runtime_error {
    public:
        ImplementationBug(const char* what) : runtime_error(what) {};
};
#endif

namespace {

typedef std::vector<DynBitset> PermMatrix;
typedef std::vector<DynBitset> AdjacencyMatr;

struct Vertice {
    Vertice(WireId* w) : type(VertWire), wire(w) {}
    Vertice(CircuitTree* c) : type(VertCirc), circ(c) {}
    enum Type {
        VertWire, VertCirc
    } type;

    union {
        WireId* wire;
        CircuitTree* circ;
    };
};

struct VerticeMapping {
    unordered_map<WireId*, size_t> wireId;
    map<CircuitTree*, size_t> circId;
    vector<Vertice> vertices;
};

struct FullMapping {
    VerticeMapping haystack, needle;
};

/// Recursively finds `needle` in `haystack` filling `results`
void findIn(vector<MatchResult>& results,
        CircuitGroup* needle, CircuitGroup* haystack);

// ========================================================================

sign_t localSign(CircuitTree* circ) {
    return circ->sign(0);
}

class WireFit {
    public:
        /** Add a connection on this wire from a circuit of signature
         * `inSig`, from its `pin`-th pin, which is an input of the circuit
         * iff `in` is true. */
        void connected(sign_t inSig, bool in, int pin) {
            ++conns[ConnType(inSig, in, pin)];
        }

        /** Check whether this wire has the required connections to act as
         * `role` */
        bool fitFor(WireId* role) {
            try {
                return fitness.at(role);
            } catch(const std::out_of_range&) {
                map<ConnType, int> usedConns;

                for(auto circ = role->adjacent_begin();
                        circ != role->adjacent_end(); ++circ)
                {
                    bool isInput = true;
                    int pinPos = 0;
                    for(auto circWire = (*circ)->io_begin();
                            circWire != (*circ)->io_end();
                            ++circWire, ++pinPos)
                    {
                        if(circWire == (*circ)->inp_end()) {
                            pinPos = 0;
                            isInput = false;
                        }
                        if(**circWire == *role)
                            break;
                    }
                    ConnType cConn(localSign(*circ), isInput, pinPos);

                    ++usedConns[cConn];
                    if(usedConns[cConn] > conns[cConn]) {
                        fitness[role] = false;
                        return false;
                    }
                }
                fitness[role] = true;
                return true;
            }
        }

    private:
        struct ConnType {
            ConnType(sign_t inSig, bool in, int pin) :
                inSig(inSig), in(in), pin(pin) {}
            bool operator==(const ConnType& e) {
                return inSig == e.inSig && in == e.in && pin == e.pin;
            }
            bool operator<(const ConnType& e) const {
                return
                    (inSig < e.inSig)
                    || (inSig == e.inSig && in < e.in)
                    || (inSig == e.inSig && in == e.in && pin < e.pin);
            }

            sign_t inSig;
            bool in;
            int pin;
        };

        map<ConnType, int> conns;
        unordered_map<WireId*, bool> fitness;
};

bool isMatchFit(CircuitTree* match, CircuitTree* needleMatch,
        unordered_map<WireId*, WireFit>& wireFit)
{
    FIND_DEBUG(" > Checking fitness\n");
    auto wire = match->io_begin(),
    role = needleMatch->io_begin();

    for(; wire != match->io_end()
            && role != needleMatch->io_end();
            ++wire, ++role)
    {
        if(!wireFit[*wire].fitFor(*role)) {
            FIND_DEBUG("  Not fit\n");
            return false;
        }
    }
    if(wire != match->io_end()
            || role != needleMatch->io_end())
    {
        return false;
    }

    return true;
}

void dumpPerm(const PermMatrix& permMatrix, const FullMapping& mapping) {
#ifndef DEBUG_FIND // UNUSED
    (void)(permMatrix);
    (void)(mapping);
#endif//DEBUG_FIND

#ifdef DEBUG_FIND
    FIND_DEBUG("  ");
    for(const auto& vert: mapping.haystack.vertices)
        FIND_DEBUG(vert.type == Vertice::VertWire ? "W" : "C");
    FIND_DEBUG("\n");
#endif
    for(size_t needleId = 0;
            needleId < mapping.needle.vertices.size(); ++needleId)
    {
        FIND_DEBUG(
                mapping.needle.vertices[needleId].type == Vertice::VertWire ?
                    "W " : "C ");
        for(size_t hayId = 0; hayId < mapping.haystack.vertices.size();
                ++hayId)
        {
            FIND_DEBUG("%c", '0' + permMatrix[needleId][hayId]);
        }
        if(mapping.needle.vertices[needleId].type == Vertice::VertWire)
            FIND_DEBUG("  %s",
                    mapping.needle.vertices[needleId].wire->name().c_str());
        FIND_DEBUG("\n");
    }
}

/// Check whether a supposed match is actually a match or not.
bool isActualMatch(const PermMatrix& perm, const FullMapping& mapping)
{
    /* Here, we must check that the nodes are actually equal to each other.
     * This only applies for circuits; Ullmann's algorithm ensures that wires
     * are correctly mapped when we reach this point.
     */

    FIND_DEBUG(" > Checking a potential solution…\n");

    for(size_t needlePos = 0;
            needlePos < mapping.needle.vertices.size();
            ++needlePos)
    {
        if(mapping.needle.vertices[needlePos].type != Vertice::VertCirc)
            continue; // We don't have to check that one

        int mappedId = perm[needlePos].singleBit();
        if(mappedId < 0) {
#ifdef DEBUG_FIND
            throw ImplementationBug("Non-bijective mapping `isActualMatch`");
#else
            return false; // Bad mapping
#endif
        }

        if(mapping.haystack.vertices[mappedId].type != Vertice::VertCirc) {
#ifdef DEBUG_FIND
            FIND_DEBUG("Pos: %d, vect: %s\n",
                    mappedId, perm[needlePos].dump().c_str());
            throw ImplementationBug("Mapping vertice to wire `isActualMatch`");
#else
            return false;
#endif
        }

        CircuitTree* needlePart = mapping.needle.vertices[needlePos].circ;
        CircuitTree* haystackPart = mapping.haystack.vertices[mappedId].circ;

        if(!needlePart->equals(haystackPart)) {
            // MAYBE TODO: propagate down that it is not a match?
            FIND_DEBUG("  > Not sub-equal\n");
            return false;
        }
    }

    // Everything is fine now!
    FIND_DEBUG("  > Found a match\n");
    return true;
}

void mapVertices(CircuitGroup* group, VerticeMapping& mapping) {
    // MAYBE TODO: So far, a bit dumb. Surely we can do better?
    vector<WireId*> wires = group->wireManager()->wires();
    sort(wires.begin(), wires.end(),
            [](WireId*& e1, WireId*& e2) {
                return e1->connectedCount() > e2->connectedCount();
            });

    for(const auto& wire: wires) {
        mapping.wireId[wire] = mapping.vertices.size();
        mapping.vertices.push_back(Vertice(wire));
    }

    for(const auto& child: group->getChildrenCst()) {
        mapping.circId[child] = mapping.vertices.size();
        mapping.vertices.push_back(Vertice(child));
    }
}

/// Sets bits in the adjacency matrix when circuits are adjacent
void buildAdjacency(CircuitGroup* group,
        const VerticeMapping& mapping,
        AdjacencyMatr& adjacency)
{
    for(const auto& wire: group->wireManager()->wires()) {
        size_t wireId = mapping.wireId.at(wire);
        for(auto circ = wire->adjacent_begin();
                circ != wire->adjacent_end();
                ++circ)
        {
            size_t circId = mapping.circId.at(*circ);
            adjacency[circId][wireId].set();
            adjacency[wireId][circId].set();
        }
    }
}

WireId* mappedWire(WireId* of,
        const FullMapping& mapping,
        const PermMatrix& perm)
{
    size_t needleId = mapping.needle.wireId.at(of);
    int matchId = perm[needleId].singleBit();
#ifdef DEBUG_FIND
    if(matchId < 0)
        throw ImplementationBug("No single corresp `mappedWire`");
    if(mapping.haystack.vertices[matchId].type != Vertice::VertWire)
        throw ImplementationBug("Bad corresp type `mappedWire`");
#endif
    const Vertice& mapped = mapping.haystack.vertices.at(matchId);
    return mapped.wire;
}

/// Create a `MatchResult` based on match maps
MatchResult buildMatchResult(
        const CircuitGroup* fullNeedle,
        const FullMapping& mapping,
        const PermMatrix& perm,
        DynBitset& impliedHay)
{
    MatchResult res;
    for(const auto& needlePart: fullNeedle->getChildrenCst()) {
        size_t needleId = mapping.needle.circId.at(needlePart);
        int matchId = perm[needleId].singleBit();
#ifdef DEBUG_FIND
        if(matchId < 0)
            throw ImplementationBug("No single corresp `buildMatchResult`");
        if(mapping.haystack.vertices[matchId].type != Vertice::VertCirc)
            throw ImplementationBug("Bad corresp type `buildMatchResult`");
#endif
        impliedHay[matchId].set();
        const Vertice& mapped = mapping.haystack.vertices.at(matchId);
        res.parts.push_back(mapped.circ);
    }
    for(const auto& inp: fullNeedle->getInputs())
        res.inputs.push_back(mappedWire(inp->actual(), mapping, perm));
    for(const auto& out: fullNeedle->getOutputs())
        res.outputs.push_back(mappedWire(out->actual(), mapping, perm));
    return res;
}

bool ullmannRefine(PermMatrix& matr,
        const FullMapping& mapping,
        const AdjacencyMatr& hayAdj)
{
    bool changed = true;
    size_t nbNeedle = mapping.needle.vertices.size();
    while(changed) {
        changed = false;
        for(size_t needleId = 0; needleId < nbNeedle; ++needleId) {
            if(!matr[needleId].any())
                return false;

            const Vertice& needleVert = mapping.needle.vertices[needleId];
            for(size_t hayId = 0; hayId < mapping.haystack.vertices.size();
                    ++hayId)
            {
                if(!matr[needleId][hayId])
                    continue;

                if(needleVert.type == Vertice::VertCirc) {
                    const CircuitTree* needle = needleVert.circ;
                    for(auto needleNeigh = needle->io_begin();
                            needleNeigh != needle->io_end();
                            ++needleNeigh)
                    {
                        size_t neighId =
                            mapping.needle.wireId.at(*needleNeigh);
                        if(!(matr[neighId] & hayAdj[hayId]).any()) {
                            changed = true;
                            matr[needleId][hayId].reset();
                            break;
                        }
                    }
                }
                else /* if(needleVert.type == Vertice::VertWire) */ {
                    WireId* needle = needleVert.wire;
                    for(auto needleNeigh = needle->adjacent_begin();
                            needleNeigh != needle->adjacent_end();
                            ++needleNeigh)
                    {
                        size_t neighId =
                            mapping.needle.circId.at(*needleNeigh);
                        if(!(matr[neighId] & hayAdj[hayId]).any()) {
                            changed = true;
                            matr[needleId][hayId].reset();
                            break;
                        }
                    }
                }
            }
        }
    }

    return true;
}

void ullmannFindDepth(size_t depth,
        DynBitset& freeHayVert,
        vector<MatchResult>& results,
        PermMatrix& matr,
        DynBitset& toUnmapHaystack,
        const FullMapping& mapping,
        const AdjacencyMatr& hayAdj,
        const CircuitGroup* fullNeedle)
{
    FIND_DEBUG("> Ullmann: depth %lu/%lu\n", depth,
            mapping.needle.vertices.size());
    if(! (matr[depth] & freeHayVert).any())
        return;

    PermMatrix matrDump = matr; // Copies stuff

    for(size_t hayId = 0; hayId < mapping.haystack.vertices.size(); ++hayId) {
        if(!matr[depth][hayId] || !freeHayVert[hayId])
            continue;

        matr[depth].reset();
        matr[depth][hayId].set();

        DynBitset toUnmapCur(mapping.haystack.vertices.size());
        if(ullmannRefine(matr, mapping, hayAdj)) {
            if(depth == mapping.needle.vertices.size() - 1) {
                if(isActualMatch(matr, mapping)) {
                    results.push_back(buildMatchResult(
                                fullNeedle, mapping, matr, toUnmapCur));
                }
            }
            else {
                freeHayVert[hayId].reset();
                FIND_DEBUG(">> Picking %lu at %lu\n", hayId, depth);
                ullmannFindDepth(depth + 1, freeHayVert, results, matr,
                        toUnmapCur, mapping, hayAdj, fullNeedle);
                freeHayVert[hayId].set();
            }
        }

        toUnmapHaystack |= toUnmapCur;

        if(!matrDump[depth].anyOver(hayId+1))
            break;

        if(toUnmapCur.any()) {
            for(auto& row: matrDump)
                row &= ~toUnmapCur;
        }
        matr = matrDump;
    }
}

void ullmannFind(vector<MatchResult>& results,
        PermMatrix& matr,
        const FullMapping& mapping,
        const AdjacencyMatr& hayAdj,
        const CircuitGroup* fullNeedle,
        const set<CircuitTree*> alreadyImplied)
{
    DynBitset freeHayVert(mapping.haystack.vertices.size());
    freeHayVert.flip(); // Everything's free to begin with
    for(const auto& circ: alreadyImplied)
        freeHayVert[mapping.haystack.circId.at(circ)].reset();
    DynBitset toUnmap(mapping.haystack.vertices.size());
    ullmannFindDepth(0, freeHayVert, results, matr, toUnmap, mapping, hayAdj,
            fullNeedle);
}

void findIn(vector<MatchResult>& results,
        CircuitGroup* needle, CircuitGroup* haystack)
{
    // Circuits that are already part of a match result
    set<CircuitTree*> alreadyImplied;

    // Recurse in hierarchy
    for(auto& child: haystack->getChildrenCst()) {
        if(child->circType() == CircuitTree::CIRC_GROUP) {
            size_t prevMatches = results.size();
            findIn(results, needle, dynamic_cast<CircuitGroup*>(child));
            if(results.size() != prevMatches)
                alreadyImplied.insert(child);
        }
    }

    if(haystack->wireManager()->wires().size()
            < needle->wireManager()->wires().size())
        return;
    if(haystack->getChildrenCst().size() - alreadyImplied.size()
            < needle->getChildrenCst().size())
        return;


    map<CircuitTree*, set<CircuitTree*> > singleMatches;

    // Fill single matches
    {
        unordered_map<sign_t, set<CircuitTree*> > signatures;
        for(auto hayPart : haystack->getChildrenCst())
            signatures[localSign(hayPart)].insert(hayPart);
        for(auto needlePart : needle->getChildrenCst()) {
            singleMatches[needlePart] = signatures[localSign(needlePart)];
        }
    }

    // Fill wire connections -- computes "fitness" for given wire roles
    unordered_map<WireId*, WireFit> wireFit;
    for(const auto& needleMatch: singleMatches) {
        sign_t cSig = localSign(needleMatch.first);
        for(const auto& match: needleMatch.second) {
            int pin = 0;
            for(auto inp = match->inp_begin(); inp != match->inp_end();
                    ++inp)
            {
                wireFit[*inp].connected(cSig, true, pin);
                ++pin;
            }

            pin = 0;
            for(auto out = match->out_begin(); out != match->out_end();
                    ++out)
            {
                wireFit[*out].connected(cSig, false, pin);
                ++pin;
            }
        }
    }

    FIND_DEBUG("=== IN %s ===\n", haystack->name().c_str());

    // Filter out the matches that are not connected as needed
    {
        auto needleMatch = singleMatches.begin();
        while(needleMatch != singleMatches.end()) {
            set<CircuitTree*> nMatches;
            for(const auto& match: needleMatch->second) {
                if(isMatchFit(match, needleMatch->first, wireFit))
                    nMatches.insert(match);
            }
            if(nMatches.size() > 0) {
                singleMatches[needleMatch->first] = nMatches;
                ++needleMatch;
            }
            else // lighten the data structure
                needleMatch = singleMatches.erase(needleMatch);
        }
    }

    // Ensure there is at least enough matches for a full `needle`
    for(const auto& child: needle->getChildrenCst())
        if(singleMatches[child].empty())
            return;

    // Map vertices (ie. wires and circuits) to IDs
    FullMapping mapping;
    mapVertices(needle, mapping.needle);
    mapVertices(haystack, mapping.haystack);

    // Determine haystack's adjacencies
    AdjacencyMatr hayAdj(
            mapping.haystack.vertices.size(),
            DynBitset(mapping.haystack.vertices.size()));
    buildAdjacency(haystack, mapping.haystack, hayAdj);

    // == Ullman's algorithm ==
    // Build the permutation matrix (initially not a permutation
    // The matix is |needle| x |haystack|, and a 1 indicates that we think two
    // vertices could be matches at a given point.
    PermMatrix permMatrix(
            mapping.needle.vertices.size(),
            DynBitset(mapping.haystack.vertices.size()));

    // Setting the possible adjacent circuits (singleMatches)
    for(const auto& match: singleMatches) {
        size_t needleId = mapping.needle.circId[match.first];
        for(const auto& hayPart: match.second) {
            size_t hayId = mapping.haystack.circId[hayPart];
            permMatrix[needleId][hayId].set();
        }
    }

    // Check for dangling wires that can slow down the whole find
    for(const auto& needleWire: needle->wireManager()->wires()) {
        if(needleWire->connectedCirc().size() == 0
                && needleWire->connectedPins().size() == 0)
        {
            LOG_WARNING("Dangling wire %s in needle",
                    needleWire->name().c_str());
        }
    }

    // Setting the possibly adjacent wires (wireFit + degrees)
    // Let's not be clever for now, and (maybe) enhance this part later
    for(const auto& hayWire: haystack->wireManager()->wires()) {
        size_t hayId = mapping.haystack.wireId[hayWire];

        for(const auto& needleWire: needle->wireManager()->wires()) {
            if((hayWire->connectedCirc().size()
                        >= needleWire->connectedCirc().size())
                    && (hayWire->connectedPins().size()
                        >= needleWire->connectedPins().size())
                    && (
                        wireFit.find(hayWire) == wireFit.end()
                        || wireFit[hayWire].fitFor(needleWire)))
                    /* If hayWire is not in wireFit, that means that the
                     * given haystack wire was never connected to anything, in
                     * which case the number of connections already tested are
                     * enough to know whether this is a potential match or not
                     */
            {
                // Fit for this role
                size_t needleId = mapping.needle.wireId[needleWire];
                permMatrix[needleId][hayId].set();
            }
        }
    }

    dumpPerm(permMatrix, mapping); // DEBUG

    // First refining
    if(!ullmannRefine(permMatrix, mapping, hayAdj))
        return;

    // Ullmann's recursion
    ullmannFind(results, permMatrix, mapping, hayAdj, needle, alreadyImplied);
}

}; // namespace

std::vector<MatchResult> matchSubcircuit(CircuitGroup* needle,
        CircuitGroup* haystack)
{
    vector<MatchResult> out;
    findIn(out, needle, haystack);
    return out;
}
