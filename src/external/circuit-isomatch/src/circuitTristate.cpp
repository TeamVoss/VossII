#include "circuitTristate.h"
#include "signatureConstants.h"
#include "dotPrint.h"

#include <string>
#include <cassert>
using namespace std;

void CircuitTristate::InnerIoIter::operator++() {
    if(ptr == circ->wireInput)
        ptr = circ->wireEnable;
    else if(ptr == circ->wireEnable)
        ptr = circ->wireOutput;
    else if(ptr == circ->wireOutput)
        ptr = NULL;
}

CircuitTristate::CircuitTristate(WireId* from, WireId* to, WireId* enable) :
    wireInput(from), wireOutput(to), wireEnable(enable)
{
    from->connect(this);
    to->connect(this);
    enable->connect(this);
}

sign_t CircuitTristate::innerSignature() const {
    return signatureConstants::opcst_leaftype(
            (circType() << 16) + (1 << 8) + 1);
}

bool CircuitTristate::innerEqual(CircuitTree*) {
    return true; // No inner data
}

size_t CircuitTristate::inputCount() const {
    return 2;
}

size_t CircuitTristate::outputCount() const {
    return 1;
}

WireId* CircuitTristate::nth_input(size_t circId) const {
    if(circId == 0)
        return wireInput;
    else if(circId == 1)
        return wireEnable;
    return nullptr;
}

WireId* CircuitTristate::nth_output(size_t circId) const {
    if(circId == 0)
        return wireOutput;
    return nullptr;
}

void CircuitTristate::toDot(std::basic_ostream<char>& out, int indent) {
    const string thisCirc = string("tri_") + to_string(id());

    dotPrint::indent(out, indent)
        << thisCirc << " "
        << "[shape=triangle, rotate=90]" << '\n';

    dotPrint::inWire(out, thisCirc, wireInput->uniqueName(),
            "headport=w");
    dotPrint::indent(out, indent);
    dotPrint::inWire(out, thisCirc, wireEnable->uniqueName(),
            "headport=n");
    dotPrint::indent(out, indent);
    dotPrint::outWire(out, thisCirc, wireOutput->uniqueName(),
            "headport=e");
}

