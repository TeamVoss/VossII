#include "circuitTree.h"
#include "circuitGroup.h"
#include "debug.h"
#include <cassert>

using namespace std;

size_t CircuitTree::nextCircuitId = 0;

CircuitTree::CircuitTree() :
        curHistoryTime(1), lastAlterationTime(1), // 1: nothing memoized yet
        ancestor_(NULL), circuitId(nextCircuitId)
{
    nextCircuitId++;
}

CircuitTree::~CircuitTree()
{}

sign_t CircuitTree::sign(int level) {
    if(level < (int)memoSig.size()
            && memoSig[level].timestamp >= lastAlterationTime)
        return memoSig[level].sig;

    sign_t signature = computeSignature(level);
    while((int)memoSig.size() <= level) // Create [level] cell
        memoSig.push_back(MemoSign(0, 0)); // 0 is always invalid
    memoSig[level] = MemoSign(curHistoryTime, signature);
    return signature;
}

bool CircuitTree::equals(CircuitTree* oth) {
    if(circType() != oth->circType())
        return false;
    return innerEqual(oth);
}

void CircuitTree::unplug() {
    unplug_common();

    for(auto conn = io_begin(); conn != io_end(); ++conn) {
        (*conn)->disconnect(this);
    }
}

sign_t CircuitTree::computeSignature(int level) {
    // Depends only on the gate's type and contents.
    sign_t inner = innerSignature();

    if(level <= 0)
        return inner;

    // inpSig is the sum of the signatures of order `level - 1` of all
    // directly input-adjacent gates.
    sign_t inpSig = 0;
    for(auto wire = inp_begin(); wire != inp_end(); ++wire) {
        for(auto circ = (*wire)->adjacent_begin();
                circ != (*wire)->adjacent_end(); ++circ)
        {
            inpSig += (*circ)->sign(level-1);
        }
    }

    // Idem with output-adjacent gates
    sign_t outSig = 0;
    for(auto wire = out_begin(); wire != out_end(); ++wire) {
        for(auto circ = (*wire)->adjacent_begin();
                circ != (*wire)->adjacent_end(); ++circ)
        {
            outSig += (*circ)->sign(level-1);
        }
    }

    // Sum of the IO signatures of the connected wires. For more details on
    // what's an IO signature, see `CircuitGroup::ioSigOf`'s docstring.
    sign_t ioSig = 0;
    if(ancestor_ != nullptr) {
        for(auto wire = io_begin(); wire != io_end(); ++wire)
            ioSig += ancestor_->ioSigOf(*wire);
    }

    return inner + ioSig + inpSig - outSig;
}

void CircuitTree::unplug_common() {
    alter();
    if(ancestor_ != nullptr)
        ancestor_->disconnectChild(this);
    ancestor_ = nullptr;
}

void CircuitTree::alter(bool uprec) {
    curHistoryTime++;
    lastAlterationTime = curHistoryTime;
    if(uprec && ancestor_ != nullptr)
        ancestor_->alteredChild();
}
