#pragma once

#include <isomatch.h>

/** Scrambles `original`, preserving its structure, but destroying a lot
 * of properties (eg. names, ordering, ...).
 *
 * NOTE: `scrambleCircuit` manually allocates a copy of `original`, meaning
 * you'll have to `delete` both of them when you're done.
*/
CircuitGroup* scrambleCircuit(CircuitGroup* original);
