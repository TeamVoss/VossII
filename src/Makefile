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

#
# Tools (wih flags)
#
YACC   ='bison -y'
LEX    ='flex -l'
RANLIB = ranlib
CC     = gcc
CX     = g++
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
# Yosys installation directory
YOSYS_DIR := $(SRC_DIR)/external/yosys
# circuit-isomatch installation directory
CKT_ISO_MATCH_DIR := $(SRC_DIR)/external/circuit-isomatch/src
# Visualization installation directory
VISUALIZATION_DIR := $(SRC_DIR)/external/visualization
# Plugin directory
PLUGIN_DIR := $(LIB_DIR)

#
# Flags
#
FL_OPTIONS = '-D__STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS'

COMPATIBILITY_FLAG = '-std=gnu99'

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
	make -C ../bindist_build/src docker_package
	cp ../bindist_build/voss.tar.bz2 ../
	rm -rf ../bindist_build

docker_package:
	docker build -t vossii-static-builder ..
	docker run --user $(REPO_OWNER) -v `pwd`/..:/vossroot vossii-static-builder

package: clean yosys_clean
	make install LINKFLAG=-static
	rm -r ../voss ; true
	install -m 755 -d ../voss/
	install -m 755 -d ../voss/bin
	install -m 755 -d ../voss/doc

	install -m 755 -s -t ../voss/bin 
	install -m 755 -s -t ../voss/bin ../bin/fl
	install -m 755 -s -t ../voss/bin ../bin/functor
	install -m 755 -s -t ../voss/bin ../bin/yosys
	install -m 755    -t ../voss/bin ../bin/front_end.tcl
	install -m 644    -t ../voss/bin ../bin/prefs.tcl
	install -m 644    -t ../voss/bin ../bin/utils.tcl
	install -m 644    -t ../voss/bin ../bin/builtins.fl
	install -m 644    -t ../voss/bin ../bin/preamble.fl

	install -m 644 -t ../voss/doc ../doc/fl_guide.pdf
	install -m 644 -t ../voss/doc ../doc/fl_plugins.md

	cp -r ../doc/hfl_examples ../voss/doc/
	cp -r ../modes            ../voss/
	cp -r ../vosslib          ../voss/
	cp -r ../IDVII            ../voss/

	cd .. && tar -cjf voss.tar.bz2 voss && chown $(REPO_OWNER) voss.tar.bz2
	rm -r ../voss

all: fl functor
	-@echo "Done"

library:
	@make -C lib \
	CC=$(CC) CX=$(CX) \
	DEBUG_FLAG=$(DEBUG_FLAG) OPTIMIZATION_FLAG=$(OPTIMIZATION_FLAG) \
	COMPATIBILITY_FLAG=$(COMPATIBILITY_FLAG) \
	LIB_DIR=$(LIB_DIR) RANLIB=$(RANLIB)

fl: library minisat circuit-isomatch visualization
	@make -C bin/fl \
	CC=$(CC) CX=$(CX) \
	FL_OPTIONS=$(FL_OPTIONS) DEBUG_FLAG=$(DEBUG_FLAG) \
	OPTIMIZATION_FLAG=$(OPTIMIZATION_FLAG) \
	COMPATIBILITY_FLAG=$(COMPATIBILITY_FLAG) \
	LIB_DIR=$(LIB_DIR) YACC=$(YACC) \
	LINKFLAG=$(LINKFLAG) ROOT_DIR=$(ROOT_DIR) VERSION_DATE=$(VERSION_DATE) \
	MINISAT_DIR=$(MINISAT_DIR) \
	CKT_ISO_MATCH_DIR=$(CKT_ISO_MATCH_DIR) \
	PLUGIN_DIR=$(PLUGIN_DIR)

install_plugin_tools:
	make -C bin/fl abi_version.h
	make -C fl-hs-plugins install doc_install PLUGIN_DIR=$(PLUGIN_DIR) BIN_DIR=$(BIN_DIR) DOC_DIR=$(DOC_DIR)

install_plugin_docs:
	cp doc/fl_plugins.md $(DOC_DIR)/fl_plugins.md


functor:
	@cd ./bin/functor; make \
	CC=$(CC) LINKFLAG=$(LINKFLAG) DEBUG_FLAG=$(DEBUG_FLAG) \
	OPTIMIZATION_FLAG=$(OPTIMIZATION_FLAG) \
	COMPATIBILITY_FLAG=$(COMPATIBILITY_FLAG)

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

install_all: install_all_but_yosys install_yosys

install_all_but_yosys: fl functor ignore_signals install_plugin_docs install_visualization
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

install_visualization: visualization
	cp external/visualization/schematic_draw_module.so ../IDVII/

install_yosys: .yosys .yosys_pexlif
	@-mkdir -p $(BIN_DIR)
	@cp $(YOSYS_DIR)/yosys $(BIN_DIR)/yosys
	@chmod ugo+rx $(BIN_DIR)/yosys

install:
	@-touch $(YOSYS_DIR)/yosys
	@-touch .yosys
	@-make install_all

circuit-isomatch:
	make -C "$(CKT_ISO_MATCH_DIR)" release

minisat:
	make -C "$(MINISAT_DIR)" config prefix=$(MINISAT_DIR) CC=$(CC) CXX=$(CX)
	make -C "$(MINISAT_DIR)" install

visualization:
	make -C "$(VISUALIZATION_DIR)"

.yosys:
	make -C "$(YOSYS_DIR)" LDFLAGS=$(LINKFLAG) -f Makefile_Voss config-gcc
	make -C "$(YOSYS_DIR)" LDFLAGS=$(LINKFLAG) -f Makefile_Voss
	touch .yosys

.yosys_pexlif: $(YOSYS_DIR)/backends/pexlif/pexlif.cc
	make -C "$(YOSYS_DIR)" LDFLAGS=$(LINKFLAG) -f Makefile_Voss
	touch .yosys_pexlif

clean:
	@-make -C lib clean
	@-make -C bin/fl clean
	@-make -C bin/functor clean
	@-make -C external/minisat clean
	@-make -C external/visualization clean

yosys_clean:
	@-/bin/rm -f .yosys .yosys_pexlif
