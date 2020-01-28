/** Auxiliary tools for parsing */

#pragma once

#include <vector>
#include <string>
#include <stack>
#include <isomatch.h>  // build it against isomatch

namespace parseTools {
    // Because this is far too ugly to be left wandering in the outside world

template<typename T>
struct ListElem {
    ListElem() : next(NULL) {}
    ListElem(const T& val, ListElem* next=NULL) : next(next), val(val) {}
    ~ListElem() {
        if(next != NULL)
            delete next;
    }

    static ListElem<T>* concat(ListElem<T>* first, ListElem<T>* second) {
        ListElem* end = first;
        if(end == NULL)
            return second;
        while(end->next != NULL)
            end = end->next;
        end->next = second;
        return first;
    }

    /** returns a vector version of this list and de-allocates it. */
    std::vector<T> yield() {
        const std::vector<T>& out =  toVect();
        delete this;
        return out;
    }

    /** returns a vector version of this list */
    std::vector<T> toVect() const {
        std::vector<T> out;
        fillVector(out);
        return out;
    }

    /** Appends the list elements' to `cur` */
    void fillVector(std::vector<T>& cur) const {
        cur.push_back(val);
        if(next != NULL)
            next->fillVector(cur);
    }

    ListElem* next;
    T val;
};

/** Used to return both a wire and a circuit list */
struct ExprConstruction {
    ExprConstruction() : outWire(NULL), gates(NULL) {}
    ExprConstruction(WireId* outWire, ListElem<CircuitTree*>* gates) :
        outWire(outWire), gates(gates) {}
    ExprConstruction(WireId* outWire, CircuitTree* gate) :
        outWire(outWire), gates(new ListElem<CircuitTree*>(gate, NULL)) {}
    WireId* outWire;
    ListElem<CircuitTree*>* gates;
};

/** Builds a CircuitGroup from its description as parsed. */
void makeGroup(CircuitGroup* stub,
        const std::vector<std::string>& inputs,
        const std::vector<std::string>& outputs,
        const std::vector<CircuitTree*>& parts);
}
