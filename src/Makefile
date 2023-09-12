#
# Top level makefile
#

#
# Current directory
#
# ROOT_DIR := $(shell pwd)
ROOT_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/../)
SRC_DIR := $(ROOT_DIR)/src
REPO_OWNER := `stat --printf "%u:%g" .`
UNAME_S := $(shell uname -s)

#
# Tools (wih flags)
#
ifeq ($(UNAME_S), Darwin)
	YACC   :="`brew --prefix`/opt/bison/bin/bison -y"
	LEX    :="`brew --prefix`/bin/flex -l"
else
	YACC   :="bison -y"
	LEX    :="flex -l"
endif
RANLIB = ranlib
CC     ?= gcc
CX     ?= g++
# CC     = clang
# CX     = clang++

#
# Names of executables
#
FL = fl				            # Name of fl executable

#
# Locations
#
# Installation binary directory
BIN_DIR := $(ROOT_DIR)/bin
# Installation fl library directory
LIB_DIR := $(ROOT_DIR)/vosslib
# Installation fl documentation directory
DOC_DIR := $(ROOT_DIR)/doc
# Minisat installation directory
MINISAT_DIR := $(SRC_DIR)/external/minisat
# Yosys write_pexlif backend source
YOSYS_WRITE_PEXLIF := $(SRC_DIR)/external/yosys/write_pexlif
# Visualization installation directory
VISUALIZATION_DIR := $(SRC_DIR)/external/visualization
# Plugin directory
PLUGIN_DIR := $(LIB_DIR)

#
# Flags
#
FL_OPTIONS = '-D__STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS -Wno-error=clobbered -Wno-error=format-overflow -Wno-error=maybe-uninitialized -Wno-error=builtin-declaration-mismatch'

COMPATIBILITY_FLAG = '-std=gnu99'


TCL_INCLUDE := "`pkg-config --cflags --libs tk`"

#
# Version control
#
VERSION_DATE = `date +'%y/%m/%d'`

#-----------------------------------------------------------------------------
# Debugging version or optimized version?
#-----------------------------------------------------------------------------
DEBUG_FLAG = '-g -DDEBUG'	# Maybe:  -fstack-protector-all
#DEBUG_FLAG = '-g'		#
#OPTIMIZATION_FLAG =  '-O3'	# Maybe: -O4

usage:
	@echo "Basic targets: install, package or clean"

clean_package:
	rm -rf ../bindist_build ; true
	git clone .. ../bindist_build
	chown -R $(REPO_OWNER) ../bindist_build
	make -C ../bindist_build/src package
	cp ../bindist_build/voss.tar.bz2 ../
	rm -rf ../bindist_build

docker_package:
	docker build -t vossii-static-builder ..
	docker run --user $(REPO_OWNER) -v `pwd`/..:/vossroot vossii-static-builder

package: clean
	make install LINKFLAG=-static
	rm -r ../voss ; true
	install -m 755 -d ../voss/
	install -m 755 -d ../voss/bin
	install -m 755 -d ../voss/doc
	install -m 755 -d ../voss/modes
	install -m 755 -d ../voss/modes/emacs
	install -m 755 -d ../voss/modes/vim

	cp -f doc/README_release.md ../voss/README.md

	install -m 755 -t ../voss/bin ../bin/fl
	install -m 755 -t ../voss/bin ../bin/functor
	install -m 755 -t ../voss/bin ../bin/write_pexlif.so
	install -m 755 -t ../voss/bin ../bin/front_end.tcl
	install -m 644 -t ../voss/bin ../bin/prefs.tcl
	install -m 644 -t ../voss/bin ../bin/utils.tcl
	install -m 644 -t ../voss/bin ../bin/builtins.fl
	install -m 644 -t ../voss/bin ../bin/preamble.fl

	install -m 644 -t ../voss/modes/emacs ../modes/emacs/fl-mode.el
	install -m 644 -t ../voss/modes/emacs ../modes/emacs/README.md

	install -m 644 -t ../voss/modes/vim ../modes/vim/fl.vimrc
	install -m 644 -t ../voss/modes/vim ../modes/vim/README

	cp -r ../modes            ../voss/
	cp -r ../vosslib          ../voss/
	cp -r ../IDVII            ../voss/
	cp -r ../ckt_examples     ../voss/
	cp -r ../doc/*            ../voss/doc/

	cd .. && tar -cjf voss.tar.bz2 voss && chown $(REPO_OWNER) voss.tar.bz2
	rm -r ../voss

all: fl functor
	-@echo "Done"

library:
	@make -C lib \
	CC=$(CC) CX=$(CX) \
	DEBUG_FLAG=$(DEBUG_FLAG) OPTIMIZATION_FLAG=$(OPTIMIZATION_FLAG) \
	COMPATIBILITY_FLAG=$(COMPATIBILITY_FLAG) \
	LIB_DIR=$(LIB_DIR) RANLIB=$(RANLIB) TCL_INCLUDE=$(TCL_INCLUDE)

fl: library minisat visualization
	@make -C bin/fl \
	CC=$(CC) CX=$(CX) \
	FL_OPTIONS=$(FL_OPTIONS) DEBUG_FLAG=$(DEBUG_FLAG) \
	OPTIMIZATION_FLAG=$(OPTIMIZATION_FLAG) \
	COMPATIBILITY_FLAG=$(COMPATIBILITY_FLAG) \
	LIB_DIR=$(LIB_DIR) YACC=$(YACC) \
	LINKFLAG=$(LINKFLAG) ROOT_DIR=$(ROOT_DIR) VERSION_DATE=$(VERSION_DATE) \
	MINISAT_DIR=$(MINISAT_DIR) \
	PLUGIN_DIR=$(PLUGIN_DIR) TCL_INCLUDE=$(TCL_INCLUDE)

install_plugin_tools:
	make -C bin/fl abi_version.h
	make -C fl-hs-plugins install doc_install PLUGIN_DIR=$(PLUGIN_DIR) BIN_DIR=$(BIN_DIR) DOC_DIR=$(DOC_DIR)

install_plugin_docs: doc/fl_plugins.pdf
	cp doc/fl_plugins.pdf $(DOC_DIR)/fl_plugins.pdf

doc/fl_plugins.pdf: doc/fl_plugins.md
	pandoc -i doc/fl_plugins.md -o doc/fl_plugins.pdf

functor:
	@cd ./bin/functor; make \
	CC=$(CC) LINKFLAG=$(LINKFLAG) DEBUG_FLAG=$(DEBUG_FLAG) \
	OPTIMIZATION_FLAG=$(OPTIMIZATION_FLAG) \
	COMPATIBILITY_FLAG=$(COMPATIBILITY_FLAG) TCL_INCLUDE=$(TCL_INCLUDE)


ignore_signals:
	@make -C bin/ignore_signals CC=$(CC) CX=$(CX) LINKFLAG=$(LINKFLAG)

install_fixed_address: fl functor
	@-mkdir -p $(BIN_DIR)
	@cp ./bin/fl/fl $(BIN_DIR)/fl.bin
	@chmod ugo+rx $(BIN_DIR)/fl.bin
	@-echo "#!/bin/csh" > $(BIN_DIR)/fl
	@-echo "setenv BIN_DIR $(BIN_DIR)" >> $(BIN_DIR)/fl
	@-cat ./scripts/fl >> $(BIN_DIR)/fl
	@chmod ugo+rx $(BIN_DIR)/fl
	@make functor
	@mv ./bin/functor/functor $(BIN_DIR)/functor
	@chmod ugo+rx $(BIN_DIR)/functor
	@-/bin/rm -rf $(LIB_DIR)
	@-mkdir -p $(LIB_DIR)
	cp -r vosslib/* $(LIB_DIR)
	chmod -R ugo+rX $(LIB_DIR)

install_all: fl functor \
			ignore_signals \
			install_visualization \
			install_write_pexlif
	@-mkdir -p $(BIN_DIR)
	@-unlink $(BIN_DIR)/fl
	@cp ./bin/fl/fl $(BIN_DIR)/fl
	@chmod ugo+rx $(BIN_DIR)/fl
	@cp ./bin/fl/builtins.fl $(BIN_DIR)/builtins.fl
	@chmod ugo+r $(BIN_DIR)/builtins.fl
	@cp ./bin/fl/preamble.fl $(BIN_DIR)/preamble.fl
	@chmod ugo+r $(BIN_DIR)/preamble.fl
	@cp ./bin/fl/front_end.tcl $(BIN_DIR)/front_end.tcl
	@chmod ugo+r $(BIN_DIR)/front_end.tcl
	@cp ./bin/fl/utils.tcl $(BIN_DIR)/utils.tcl
	@chmod ugo+r $(BIN_DIR)/utils.tcl
	@cp ./bin/fl/prefs.tcl $(BIN_DIR)/prefs.tcl
	@chmod ugo+r $(BIN_DIR)/prefs.tcl
	@cp ./bin/ignore_signals/ignore_signals $(BIN_DIR)/ignore_signals
	@chmod ugo+rx $(BIN_DIR)/ignore_signals
	@make functor
	@mv ./bin/functor/functor $(BIN_DIR)/functor
	@chmod ugo+rx $(BIN_DIR)/functor
	@-/bin/rm -rf $(LIB_DIR)
	@-mkdir -p $(LIB_DIR)
	cp -r vosslib/* $(LIB_DIR)
	chmod -R a+rX $(LIB_DIR)
	@cd ../IDVII/src/fl/design; make

install_visualization: visualization
	cp external/visualization/schematic_draw_module.so ../IDVII/

install_write_pexlif: $(YOSYS_WRITE_PEXLIF)/write_pexlif.so
	@-mkdir -p $(BIN_DIR)
	@cp $(YOSYS_WRITE_PEXLIF)/write_pexlif.so $(BIN_DIR)/write_pexlif.so
	@chmod ugo+rx $(BIN_DIR)/write_pexlif.so

install:
	@-make install_all

minisat:
	make -C "$(MINISAT_DIR)" config prefix=$(MINISAT_DIR) CC=$(CC) CXX=$(CX)
	make -C "$(MINISAT_DIR)" install

visualization:
	make -C "$(VISUALIZATION_DIR)" TCL_INCLUDE=$(TCL_INCLUDE) CC=$(CC) CXX=$(CX)

$(YOSYS_WRITE_PEXLIF)/write_pexlif.so: $(YOSYS_WRITE_PEXLIF)/write_pexlif.cc
	make -C $(YOSYS_WRITE_PEXLIF)

clean:
	@-make -C lib clean
	@-make -C bin/fl clean
	@-make -C bin/functor clean
	@-make -C external/minisat clean
	@-make -C external/visualization clean
	@-make -C $(YOSYS_WRITE_PEXLIF) clean
	@-make -C ../IDVII/src/fl/design clean

