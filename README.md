# VossII

## Requirements
# On a stock Ubuntu installation you could do:

`sudo apt install libz-dev libx11-dev libreadline-dev flex bison gawk graphviz doxygen clang`

## Build

```
cd $(VOSSII_DIR)/src
make install
```


Finally, add $(VOSSII_DIR)/bin to your search path

Now typing fl should start fl (yeah!).


will install all the files needed by VossII in `bin/` (relative to where you
cloned VossII).


### Building a relocatable, statically linked binary distribution

Quick and dirty:

```
cd $(VOSSII_DIR)/src
make package
```

Suitable for release:

```
cd $(VOSSII_DIR)/src
make clean_package
```

The `clean_package` target uses Docker to set up a somewhat reproducible
build environment without cluttering up your system,
which means you may need to run it as root.
