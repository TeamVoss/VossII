
CC = gcc
# CFLAGS = -O3 -fPIC
CFLAGS = -g -fPIC

LIB_FILES = ../../lib/alloc.c \
	    ../../lib/buf.c \
	    ../../lib/hash.c \
	    ../../lib/morestr.c \
	    ../../lib/readrc.c \
	    ../../lib/rec.c \
	    ../../lib/strmgr.c \
	    ../../lib/timer.c \
	    ../../lib/uniq_buf.c \
	    ./list_util.c \
	    ./place_bounds.c \
	    ./place.c \
	    ./sch_util.c

schematic_draw_module.so: schematic_draw_module.c list_util.o place_bounds.o \
			 place.o sch_util.o
	$(CC) -shared $(CFLAGS) schematic_draw_module.c $(LIB_FILES) \
		-I ../../include -I /usr/include/tcl \
		-o schematic_draw_module.so


list_util.o: list_util.h list_util.c
	$(CC) -I ../../include -g -c list_util.c 

place_bounds.o: list_util.h place_bounds.h place_bounds.c
	$(CC) -I ../../include -g -c place_bounds.c

place.o: list_util.h place.h place.c
	$(CC) -I ../../include -g -c place.c

sch_util.o: list_util.h sch_util.h sch_util.c
	$(CC) -I ../../include -g -c sch_util.c 

clean:
	/bin/rm -f *.o *.so
