# circuit-isomatch

A library for subgraph isomorphism and pattern-match-and-replace in electronic
circuits.

## Summary and features

This library provides an AST to describe an electronic circuit (`circuit*.h`
files, based on `circuitTree.h`), ordered hierarchically (by "groups" or
"blocks", as if a circuit consisted mostly of integrated circuits
interconnected, themselves composed of integrated circuits, etc. down to the
leaf-level where actual logical gates are present).

Such two circuits can then be checked efficiently for formal equality, calling
the `foo->equals(bar)` method of `CircuitTree`. Note that this method is blind
to the circuit's semantics, but mostly provides efficient graph isomorphism
checking. For instance, `AND` gates are not considered commutative.

Moreover, a circuit can be searched for occurrences of a given sub-circuit,
returning the list of such (non-overlapping) occurrences. These occurrences
will be returned alongside with details about how they are plugged in the
circuit, allowing for an easy "search-and-replace" over a whole circuit. This
internally uses extensively the equality checking, and is mostly performing a
subgraph-isomorphism optimized for electronic circuits.

The library also provides a C interface, which should contain everything
needed to perform the previous operations.

## Compiling

Release (without debugging symbols, optimized):

    make

Debug:

    make debug

In either case, the library is produced as `src/libisomatch.a`.

## Documentation

The code is documented in-line (using Doxygen syntax). The documentation can be
generated as html pages using

    make docs

The files are produced under `docs/html/`.

## `util` folder

* `util/lang` contains a parser for an ad-hoc circuit description language, as
  well as a few test files. This is mostly intended to be an easy way to run
  tests on the library, but can be adapted for simple applications.

* `util/primegen` contains some basic Python code that was used to generate the
  constants internally used to compute numeric signatures.

* `util/scramble` is another library intended for testing, which scrambles a
  circuit into another circuit that should be equal to the original one,
  loosing information and reordering circuit gates on its way.

