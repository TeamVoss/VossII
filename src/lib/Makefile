# This makefile makes fl_lib.a from files in this directory
# and includes dependencies to #include files elsewhere
# in the subdirectory.
#
CFLAGS = $(DEBUG_FLAG) $(OPTIMIZATION_FLAG) $(COMPATIBILITY_FLAG) \
	 -Wall -Wextra -Werror

#
#
INC = -I../include

OBJ = alloc.o buf.o hash.o morestr.o readrc.o rec.o strmgr.o timer.o \
      uniq_buf.o generation_hash.o

fl_lib.a: $(OBJ)
	ar r fl_lib.a $?
	$(RANLIB) fl_lib.a

#
# compilation with includes
#
.c.o:
	$(CC) $(CFLAGS) $(INC) -c $<

clean:
	-@/bin/rm -f *.o fl_lib.a
