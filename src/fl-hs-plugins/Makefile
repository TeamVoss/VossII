.PHONY: all install

BIN_DIR ?= $(HOME)/.bin
DOC_DIR ?= $(BIN_DIR)/../doc
DEFINES = --ghc-options=-DDEFAULT_PLUGIN_DIR=$(PLUGIN_DIR)
PATHS = --bindir=$(BIN_DIR)

all:
	cabal v1-install --only-dependencies
	cabal v1-build $(DEFINES)

install:
	mkdir -p $(BIN_DIR)
	cabal v1-install $(DEFINES) --bindir=$(BIN_DIR) --symlink-bindir=$(BIN_DIR)

doc:
	cabal v1-haddock $(DEFINES)

doc_install: doc
	cp -r dist/doc/html/fl-plugins $(DOC_DIR)

clean:
	cabal v1-clean
