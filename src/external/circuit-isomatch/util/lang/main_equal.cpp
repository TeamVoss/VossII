#include <cstdio>
#include <iostream>
#include <scramble.h>
#include "aux.h"
using namespace std;

int main(int argc, char** argv) {
    CircuitGroup* circuit1 = parse(argv[1]);
    CircuitGroup* circuit2 = nullptr;
    if(argc > 2)
        circuit2 = parse(argv[2]);
    else
        circuit2 = scrambleCircuit(circuit1);

    cout << circuit1->equals(circuit2)
         << circuit2->equals(circuit1)
         << endl;

    delete circuit1;
    delete circuit2;
    return 0;
}

