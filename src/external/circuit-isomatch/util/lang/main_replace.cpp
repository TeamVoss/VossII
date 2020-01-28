#include <cstdio>
#include <iostream>
#include "aux.h"
using namespace std;

int main(int, char** argv) {
    CircuitGroup* circuit = parse(argv[1]);
    CircuitGroup* repl = parse(argv[2]);

    cout << circuit->sign() << endl;

    CircuitGroup* d1 = nullptr;
    for(const auto& child: circuit->getChildrenCst()) {
        if(child->circType() == CircuitTree::CIRC_GROUP) {
            d1 = dynamic_cast<CircuitGroup*>(child);
            break;
        }
    }

    CircuitGroup* d2 = nullptr;
    for(const auto& child: d1->getChildrenCst()) {
        if(child->circType() == CircuitTree::CIRC_GROUP) {
            d2 = dynamic_cast<CircuitGroup*>(child);
            break;
        }
    }

    d2->unplug();
    delete d2;
    d1->addChild(repl);

    cout << circuit->sign() << endl;

    delete circuit;
    return 0;
}

