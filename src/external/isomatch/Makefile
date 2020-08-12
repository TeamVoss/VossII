NAME   = isomatch
TARGET = $(NAME)_lib.a

CFLAGS = $(DEBUG_FLAG) \
	$(OPTIMIZATION_FLAG) \
	$(COMPATIBILITY_FLAG) \
	-Wall -Wextra -Werror -Wclobbered

FL_DIR ?= ../../bin/fl/

OBJ = 

.PHONY: release clean

release:

clean:
	rm -f *.o
	rm -f $(TARGET)
