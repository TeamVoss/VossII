OPTS ?=
CXX = g++
OPTFLAGS ?=
CXXFLAGS = $(OPTS) $(OPTFLAGS) -Wall -Wextra -Werror -std=c++14
CXXLIBS =
AR = ar
DOXYGEN = doxygen
DOXYGEN_FLAGS =

NAME = isomatch
TARGET = lib$(NAME).a
OBJS = \
	   wireId.o \
	   wireManager.o \
	   dotPrint.o \
	   gateExpression.o \
	   circuitTree.o \
	   circuitGroup.o \
	   circuitTristate.o \
	   circuitDelay.o \
	   circuitAssert.o \
	   circuitComb.o \
	   dyn_bitset.o \
	   groupEquality.o \
	   subcircMatch.o \
	   signatureConstants.o \
	   c_api/isomatch.o

###############################################################################

all: release

debug: OPTFLAGS += -O0 -g
debug: CXXFLAGS += -DDEBUG
debug: do-all

release: OPTFLAGS += -O3
release: do-all

do-all: $(TARGET) docs

$(TARGET): $(OBJS)
	$(AR) rs $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(CXXLIBS) -o $@ -c $<

###############################################################################

docs: Doxyfile
	$(DOXYGEN) $(DOXYGEN_FLAGS)

###############################################################################

clean:
	rm -f *.o
	rm -f c_api/*.o
	rm -f $(TARGET)

###############################################################################

.PHONY: all clean
