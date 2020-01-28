#include "circuitGroup.h"
#include "dotPrint.h"
#include "signatureConstants.h"
#include "groupEquality.h"
#include <cstdint>
#include <unordered_map>
#include <algorithm>

#include "debug.h"

using namespace std;

IOPin::IOPin(WireId* formal,
        WireId* actual,
        CircuitGroup* group) :
    _formal(formal), _actual(actual), _group(group)
{}

IOPin::IOPin(std::string formalName, WireId* actual, CircuitGroup* group) :
    _formal(NULL), _formalName(formalName), _actual(actual), _group(group)
{}

void IOPin::connect(WireId* formal) {
    if(_formal != NULL)
        throw IOPin::AlreadyConnected();
    _formal = formal;
    link();
}

void IOPin::link() {
    _formal->connect(this, _actual);
}

void CircuitGroup::InnerIoIter::operator++() {
    innerIncr();
    nextValid(); // Forward to the next valid element.
}

void CircuitGroup::InnerIoIter::nextValid() {
    while(ptr != circ->grpOutputs.end() && (*ptr)->formal() == nullptr)
        innerIncr();
}

void CircuitGroup::InnerIoIter::innerIncr() {
    if(ptr == circ->grpOutputs.end())
        return;
    ++ptr;
    if(ptr == circ->grpInputs.end())
        ptr = circ->grpOutputs.begin();
}

CircuitGroup::CircuitGroup(const std::string& name) :
    CircuitTree(), name_(name), ioSigsTimestamp(0)
{
    wireManager_ = new WireManager();
}

CircuitGroup::CircuitGroup(const std::string& name, WireManager* manager) :
    CircuitTree(), name_(name), wireManager_(manager), ioSigsTimestamp(0)
{}

CircuitGroup::~CircuitGroup() {
    for(auto child: grpChildren)
        delete child;
    for(auto pin: grpInputs)
        delete pin;
    for(auto pin: grpOutputs)
        delete pin;
    delete wireManager_;
}

void CircuitGroup::addChild(CircuitTree* child) {
    alter();

    child->ancestor_ = this; // CircuitGroup is friend of CircuitTree
    if(child->circType() == CIRC_GROUP) {
        CircuitGroup* grp = static_cast<CircuitGroup*>(child);
        for(auto inp : grp->getInputs()) {
            if(inp->formal() == nullptr)
                inp->connect(wireManager_->wire(inp->formalName()));
        }
        for(auto out : grp->getOutputs()) {
            if(out->formal() == nullptr)
                out->connect(wireManager_->wire(out->formalName()));
        }
    }
    grpChildren.push_back(child);
}

void CircuitGroup::addInput(const IOPin& pin) {
    alter();

    IOPin* nPin = new IOPin(pin);
    if(nPin->_formal != nullptr)
        nPin->link();
    grpInputs.push_back(nPin);
}

void CircuitGroup::addInput(const std::string& formal, WireId* actual) {
    addInput(IOPin(formal, actual, this));
}

void CircuitGroup::addOutput(const IOPin& pin) {
    alter();

    IOPin* nPin = new IOPin(pin);
    if(nPin->_formal != nullptr)
        nPin->link();
    grpOutputs.push_back(nPin);
}

void CircuitGroup::addOutput(const std::string& formal, WireId* actual) {
    addOutput(IOPin(formal, actual, this));
}

std::vector<CircuitTree*>& CircuitGroup::getChildren() {
    alter();
    return grpChildren;
}
const std::vector<CircuitTree*>& CircuitGroup::getChildren() const {
    return grpChildren;
}
const std::vector<CircuitTree*>& CircuitGroup::getChildrenCst() const {
    return static_cast<const CircuitGroup*>(this)->getChildren();
}

std::vector<IOPin*>& CircuitGroup::getInputs() {
    alter();
    return grpInputs;
}
const std::vector<IOPin*>& CircuitGroup::getInputs() const {
    return grpInputs;
}

std::vector<IOPin*>& CircuitGroup::getOutputs() {
    alter();
    return grpOutputs;
}
const std::vector<IOPin*>& CircuitGroup::getOutputs() const {
    return grpOutputs;
}

std::vector<MatchResult> CircuitGroup::find(CircuitGroup* needle) {
    return matchSubcircuit(needle, this);
}

size_t CircuitGroup::inputCount() const {
    return grpInputs.size();
}

size_t CircuitGroup::outputCount() const {
    return grpOutputs.size();
}

WireId* CircuitGroup::nth_input(size_t circId) const {
    if(circId >= inputCount())
        return nullptr;
    return grpInputs[circId]->formal();
}

WireId* CircuitGroup::nth_output(size_t circId) const {
    if(circId >= outputCount())
        return nullptr;
    return grpOutputs[circId]->formal();
}

void CircuitGroup::unplug() {
    unplug_common();

    for(const auto& inp: grpInputs) {
        if(inp->formal() != nullptr)
            inp->formal()->disconnect(inp);
    }

    for(const auto& out: grpOutputs) {
        if(out->formal() != nullptr)
            out->formal()->disconnect(out);
    }
}

void CircuitGroup::toDot(std::basic_ostream<char>& out, int indent) {
    const string thisCirc = string("group_") + name_ + to_string(id());

    if(ancestor() == NULL) {
        // Root group
        dotPrint::indent(out, indent)
            << "digraph " << thisCirc << " {\n";
    }
    else {
        dotPrint::indent(out, indent)
            << "subgraph cluster_" << thisCirc << " {\n";
    }
    indent += 2;
    dotPrint::indent(out, indent)
        << "graph[style=filled, splines=curved, label=\"" << name_ << "\"]\n";

    // Wires
    for(auto wire : wireManager()->wires()) {
        dotPrint::indent(out, indent)
            << wire->uniqueName()
            << " [shape=plain, label="
            << wire->uniqueName()
            << "]"
            << '\n';
    }

    // IO pins
    for(auto inPin : grpInputs) {
        if(inPin->formal() != NULL) {
            dotPrint::indent(out, indent)
                << inPin->actual()->uniqueName()
                << " -> "
                << inPin->formal()->uniqueName()
                << " [arrowhead=none]"
                << '\n';
        }
    }
    for(auto outPin : grpOutputs) {
        if(outPin->formal() != NULL)
            dotPrint::indent(out, indent)
                << outPin->actual()->uniqueName()
                << " -> "
                << outPin->formal()->uniqueName()
                << " [arrowhead=none]"
                << '\n';
    }

    // Children
    for(auto child : grpChildren)
        child->toDot(out, indent);

    indent -= 2;
    dotPrint::indent(out, indent)
        << "}\n";
}

sign_t CircuitGroup::ioSigOf(WireId* wire) {
    try {
        if(ioSigsTimestamp < lastAlterationTime)
            computeIoSigs();
        return ioSigs_.at(wire);
    }
    catch(const std::out_of_range& e) {
        return 0;
    }
}

sign_t CircuitGroup::innerSignature() const {
    sign_t subsigs = 0;
    for(auto sub : grpChildren)
        subsigs += sub->sign();
    return signatureConstants::opcst_leaftype(
            ((circType() << 16)
             | (grpInputs.size() << 8)
             | (grpOutputs.size()))
            + subsigs);
}

bool CircuitGroup::innerEqual(CircuitTree* othTree) {
    return groupEquality::equal(this, dynamic_cast<CircuitGroup*>(othTree));
}

/** Computes (base ** exp) % mod through quick exponentiation */
static uint64_t expmod(uint64_t base, uint64_t exp, uint64_t mod) {
    if(exp <= 1)
        return (exp == 1) ? base : 1;
    uint64_t out = expmod((base * base) % mod, exp / 2, mod);
    return (mod & 1) ? ((out * base) % mod) : out;
}

/** Computes the I/O signature of a single wire, given the I/O pins it is
 * connected to. */
static sign_t ioSigOfSet(const unordered_set<size_t>& set) {
    static const uint32_t pinMod = signatureConstants::pinIdMod;
    auto valSig = [](size_t val) { return expmod(2, val, pinMod); };

    sign_t out = 0;
    for(auto& val : set)
        out = (out + valSig(val)) % pinMod;
    return out;
}

void CircuitGroup::computeIoSigs() {
    unordered_map<WireId*, unordered_set<size_t> > inpPinsForWire;
    unordered_map<WireId*, unordered_set<size_t> > outPinsForWire;

    for(size_t inpId = 0; inpId < grpInputs.size(); ++inpId) {
        IOPin* pin = grpInputs[inpId];
        auto& wireSet = inpPinsForWire[pin->actual()];
        wireSet.insert(inpId);
    }
    for(size_t outId = 0; outId < grpOutputs.size(); ++outId) {
        IOPin* pin = grpOutputs[outId];
        auto& wireSet = outPinsForWire[pin->actual()];
        wireSet.insert(outId);
    }

    ioSigs_.clear();
    ioSigsTimestamp = curHistoryTime;
    for(auto& entry : inpPinsForWire)
        ioSigs_[entry.first] = ioSigOfSet(entry.second);
    for(auto& entry : outPinsForWire)
        ioSigs_[entry.first] += (ioSigOfSet(entry.second) << 32);
    // FIXME ough to mix up a bit the two parts.
}

void CircuitGroup::alteredChild() {
    CircuitTree::alter();
    for(auto& child: grpChildren)
        child->alter(false);
}

void CircuitGroup::disconnectChild(CircuitTree* toRemove) {
    auto iter = std::find(grpChildren.begin(), grpChildren.end(), toRemove);
    if(iter == grpChildren.end()) {
        throw NoSuchChild();
    }
    grpChildren.erase(iter);
}
