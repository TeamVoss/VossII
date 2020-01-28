#include "circuitAssert.h"
#include "signatureConstants.h"
#include "dotPrint.h"

#include <cassert>
#include <string>

using namespace std;

void CircuitAssert::InnerIoIter::operator++() {
    ++ptr;
}

CircuitAssert::CircuitAssert(const std::string& name,
        ExpressionBase* expr) :
    gateName(name), gateExpr(expr)
{
    expr->addRef();
}

CircuitAssert::~CircuitAssert() {
    gateExpr->deleteSelf();
}

void CircuitAssert::addInput(WireId* wire) {
    alter();
    gateInputs.push_back(wire);
}

sign_t CircuitAssert::innerSignature() const {
    return signatureConstants::opcst_leaftype(
            ((circType() << 16) | (gateInputs.size() << 8))
            + gateExpr->sign());
}

bool CircuitAssert::innerEqual(CircuitTree* othTree) {
    const CircuitAssert* oth = dynamic_cast<const CircuitAssert*>(othTree);
    return gateInputs.size() == oth->gateInputs.size()
        && gateExpr->equals(*oth->gateExpr);
}

size_t CircuitAssert::inputCount() const {
    return gateInputs.size();
}

size_t CircuitAssert::outputCount() const {
    return 0;
}

WireId* CircuitAssert::nth_input(size_t circId) const {
    if(circId >= inputCount())
        return nullptr;
    return gateInputs[circId];
}

WireId* CircuitAssert::nth_output(size_t) const {
    return nullptr;
}

void CircuitAssert::toDot(std::basic_ostream<char>& out, int indent) {
    const string thisCirc = string("assert_") + to_string(id());

    dotPrint::indent(out, indent)
        << thisCirc << " "
        << " [label=\"assert\"]" << '\n';

    for(auto input : inputs()) {
        dotPrint::indent(out, indent);
        dotPrint::inWire(out, thisCirc, input->uniqueName());
    }
}

