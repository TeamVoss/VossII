#include "aux.h"
#include "parser.tab.hpp"

#include <cstdio>
#include <cstdlib>
using namespace std;

CircuitGroup* parse(const char* path) {
    FILE* fPtr = fopen(path, "r"); // Sanity checks? What sanity checks?
    CircuitGroup* circuit = doParse(fPtr);
    fclose(fPtr);

    if(circuit == NULL) {
        fprintf(stderr, "error: parsing failed.\n");
        exit(1);
    }

    return circuit;
}

