//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include <cstdio>
#include <iostream>
#include "aux.h"
using namespace std;

int main(int, char** argv) {
    CircuitGroup* circuit = parse(argv[1]);
    circuit->toDot(cout);

    delete circuit;
    return 0;
}

