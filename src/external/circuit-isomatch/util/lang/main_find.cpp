#include <cstdio>
#include <iostream>
#include "aux.h"
using namespace std;

int main(int argc, char** argv) {
    if(argc != 3) {
        cerr << "Bad arguments. Usage:\n" << argv[0]
             << " [haystack.circ] [needle.circ]" << endl;
        return 1;
    }

    CircuitGroup* haystack = parse(argv[1]);
    CircuitGroup* needle = parse(argv[2]);

    vector<MatchResult> matches = haystack->find(needle);

    cout << matches.size() << " matches" << endl;

    for(const auto& match: matches) {
        static const int SHIFT_LEN = 30;
        const string& ancestor = match.parts[0]->ancestor()->name();
        cout << ancestor
             << string(SHIFT_LEN - ancestor.size(), ' ')
             << match.outputs[0]->name()
             << endl;
    }

    delete haystack;
    delete needle;
    return 0;
}

