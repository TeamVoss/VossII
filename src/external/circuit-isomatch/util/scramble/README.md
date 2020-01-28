# Scramble

Scrambles a `CircuitGroup`, preserving its structure, but destroying a lot of
properties (eg. names, ordering, ...).

Used for testing purposes.

## Usage

`CircuitGroup* scrambled = scrambleCircuit(original);`

NOTE: `scrambleCircuit` manually allocates a copy of `original`, meaning you'll
have to `delete` both of them when you're done.
