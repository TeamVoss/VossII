CHANGING_LIBS = ../../src/libisomatch.a ../scramble/libscramble.a
LIBPATH = -L../../src -L../scramble
INCLUDE_PATH = -I../../src -I../scramble
C = gcc
CXX = g++
OPTFLAGS ?=
CXXFLAGS = $(INCLUDE_PATH) $(OPTFLAGS) -Wall -Wextra -Werror -std=c++14
CFLAGS = $(INCLUDE_PATH) $(OPTFLAGS) -Wall -Wextra -Werror -std=c11
CLIBS = $(LIBPATH) -lisomatch -lstdc++
CXXLIBS = $(LIBPATH) -lscramble -lisomatch
LEX = flex
YACC = bison
YACCFLAGS =

PARSER = parser.y
MAINS = dot sig equal find
OBJS = aux.o parser.tab.o lexer.yy.o parseTools.o

##########################################################################

BINS = $(addsuffix .bin,$(MAINS))

all: debug

debug: OPTFLAGS += -O0 -g -DDEBUG
debug: do-all

release: OPTFLAGS += -O3
release: do-all

do-all: build test

build: $(BINS)

%.bin: main_%.o $(OBJS) $(CHANGING_LIBS)
	$(CXX) $(CXXFLAGS) -o $@ main_$*.o $(OBJS) $(CXXLIBS)

%.cbin: main_%.o $(CHANGING_LIBS)
	$(C) $(CFLAGS) -o $@ main_$*.o $(CLIBS)

aux.o: aux.cpp parser.tab.hpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

%.o: %.c
	$(C) $(CFLAGS) -o $@ -c $<

%.yy.cpp: %.l $(PARSER:.y=.tab.hpp)
	$(LEX) -o $@ $<

%.tab.cpp: %.y
	$(YACC) $(YACCFLAGS) -d -o $@ $<

%.tab.hpp: %.y
	$(YACC) $(YACCFLAGS) -d -o $(@:.hpp=.cpp) $<

clean:
	rm -rf *.{c,}bin *.o *.yy.{cpp,c} *.tab.{cpp,c,h,hpp}

test: sig.bin dot.bin find.bin capi.cbin equal.bin
	./run_sigtests.py
	./dot.bin circ/processor.circ > /dev/null
	./sig.bin circ/processor.circ > /dev/null
	[ "$$(./find.bin circ/processor.circ circ/mux.circ | head -n 1)" = \
		"73 matches" ]
	./capi.cbin > /dev/null
	./replace.bin circ/simpledeep{,_repl}.circ > /dev/null
	[ "$$(./capi.cbin 2>/dev/null | tail -n 1)" = "2 MUX" ]
	[ "$$(./equal.bin circ/processor.circ)" = "11" ]
	valgrind -q ./dot.bin circ/processor.circ > /dev/null
	valgrind -q ./sig.bin circ/processor.circ > /dev/null
	valgrind -q ./capi.cbin > /dev/null
	valgrind -q ./equal.bin circ/processor.circ > /dev/null
	valgrind -q ./replace.bin circ/simpledeep{,_repl}.circ > /dev/null
	@echo -e "\e[92m>> All tests passed! :)\e[0m"

speed: equal.bin
	time ./equal.bin circ/processor.circ > /dev/null

.PHONY: all build clean test
