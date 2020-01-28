#include "dotPrint.h"
using namespace std;

namespace dotPrint {
    const std::string concatArgs(const std::string& arg1,
            const std::string& arg2)
    {
        if(arg1.empty())
            return arg2;
        if(arg2.empty())
            return arg1;
        return arg1 + string(", ") + arg2;
    }

    std::string fmtArgs(const std::string& args) {
        if(args.empty())
            return args;
        return string("[") + args + ']';
    }

    std::ostream& indent(std::ostream& stream, int indent) {
        for(int i=0; i < indent; i++)
            stream << ' ';
        return stream;
    }

    std::ostream& inWire(std::ostream& stream,
            const std::string& circuit,
            const std::string& wire,
            const std::string& attr)

    {
        stream << wire
               << " -> "
               << circuit
               << " " << fmtArgs(attr)
               << '\n';
        return stream;
    }

    std::ostream& outWire(std::ostream& stream,
            const std::string& circuit,
            const std::string& wire,
            const std::string& attr)
    {
        stream << circuit
               << " -> "
               << wire
               << " " << fmtArgs(concatArgs(
                           "arrowhead=none, arrowtail=normal", attr))
               << '\n';
        return stream;
    }
}
