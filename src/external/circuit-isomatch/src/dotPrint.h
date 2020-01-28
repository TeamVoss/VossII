/** Helper functions to pretty-print to Dot format */

#pragma once

#include <ostream>
#include <string>

namespace dotPrint {
    /** Print `indent` spaces */
    std::ostream& indent(std::ostream& stream, int indent);

    /** Wire entering a circuit */
    std::ostream& inWire(std::ostream& stream,
            const std::string& circuit,
            const std::string& wire,
            const std::string& attr="");

    /** Wire exiting a circuit */
    std::ostream& outWire(std::ostream& stream,
            const std::string& circuit,
            const std::string& wire,
            const std::string& attr="");

};

