#include "wireManager.h"
using namespace std;

size_t WireManager::nextId = 0;

WireManager::WireManager() : id_(nextId++)
{}

WireManager::~WireManager() {
    for(auto wire: wireById)
        delete wire;
}

WireId* WireManager::fresh(const std::string& name) {
    if(hasWire(name))
        throw AlreadyDefined(name.c_str());
    wireById.push_back(new WireId(wireById.size(), name, this));
    wireByName[name] = wireById.back();
    return wireById.back();
}

bool WireManager::hasWire(const std::string& name) {
    return wireByName.find(name) != wireByName.end();
}

bool WireManager::hasWire(size_t id) {
    return wireById.size() > id; // `id` is unsigned
}

std::vector<WireId*> WireManager::wires() const {
    vector<WireId*> out;
    for(const auto& wire: allWires()) {
        if(wire->isEndpoint)
            out.push_back(wire);
    }
    return out;
}

WireId* WireManager::wire(const std::string& name, bool dontCreate) {
    if(!hasWire(name)) {
        if(dontCreate)
            throw NotDefined(name.c_str());
        return fresh(name);
    }
    else
        return wireByName[name];
}

WireId* WireManager::wire(size_t id) {
    if(!hasWire(id))
        throw NotDefined("[id]");
    return wireById[id];
}

void WireManager::rename(size_t id, const std::string& newName) {
    if(!hasWire(id))
        throw NotDefined("[id]");

    rename(wireById[id]->name(), newName);
}

void WireManager::rename(const std::string& curName,
        const std::string& newName)
{
    if(!hasWire(curName))
        throw NotDefined(curName.c_str());

    WireId* curWire = wireByName[curName];
    if(hasWire(newName)) {
        WireId* mergeWith = wire(newName);
        wireByName.erase(curName);
        curWire->merge(mergeWith);
    }
    else {
        curWire->rename(newName);
        wireByName.erase(curName);
        wireByName[newName] = curWire;
    }
}
