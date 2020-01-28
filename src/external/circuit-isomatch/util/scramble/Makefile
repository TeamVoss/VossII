CHANGING_LIBS = ../../src/libisomatch.a
LIBPATH = -L../../src
INCLUDE_PATH = -I../../src
CXX = g++
CXXFLAGS = $(INCLUDE_PATH) -Wall -Wextra -Werror -O0 -g -std=c++14
CXXLIBS = $(LIBPATH) -lisomatch
AR = ar

NAME = scramble
TARGET = lib$(NAME).a
OBJS = scramble.o

###############################################################################

all: $(TARGET)

$(TARGET): $(OBJS) $(CHANGING_LIBS)
	$(AR) rs $@ $(OBJS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

###############################################################################

clean:
	rm -f *.o
	rm -f $(TARGET)

###############################################################################

.PHONY: all clean
