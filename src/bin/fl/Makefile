# This makefile makes fl from files in this directory
# and includes dependencies to #include files elsewhere
# in the subdirectory.
#


MINISAT_DIR ?=../../external/minisat/
PLUGIN_DIR ?=$(ROOT_DIR)/plugins

CFLAGS = $(DEBUG_FLAG) $(OPTIMIZATION_FLAG) $(FL_OPTIONS) \
	$(COMPATIBILITY_FLAG) \
	-Wall -Wextra -Werror -Wclobbered \
	-DVERSION_DATE='"$(VERSION_DATE)"' \
	-DDEFAULT_PLUGIN_DIR='"$(PLUGIN_DIR)"' -DFL_BINARY=1

# Minisat legacy code workaround
CXXFLAGS = -fpermissive -Wno-error

INC = -I../../include -I$(MINISAT_DIR)/include -DROOT_DIR='"$(ROOT_DIR)"'
LIB = ../../lib/fl_lib.a $(MINISAT_DIR)/lib/libminisat.a \
      $(CKT_ISO_MATCH_DIR)/libisomatch.a
FL_LIB_FLAGS = -ldl

# All headers that affect fl's plugin ABI.
ABI_HEADERS = float.h graph.h plugin.h strings.h typecheck.h ../../include/types.h

OBJ = new_bdd.o error.o arb_prec.o file_ops.o initialize.o symbol_tbl.o \
	y.tab.o lex.yy.o \
	graph.o fl.o typecheck.o emit.o lp.o cache.o compile.o \
	expand_cursor.o io.o remote_tcl.o bexpr.o minisat_ifc.o main.o \
	draw_graph.o debug.o symbol.o strings.o int_ops.o list_ops.o \
	system.o float.o bv.o bev.o fsm.o sha256.o table.o image.o \
	plugin.o plugin_loader.o plugin_checker.o plugin_errlog.o \
	iso.o pexlif.o doc.o

fl: $(OBJ) $(LIB)
	$(CX) $(CFLAGS) $(INC) $(LINKFLAG) -o fl $(OBJ) $(LIB) \
	$(COMPAT_LIB) $(FL_LIB_FLAGS)

#
# compilation with includes
#
.c.o:
	$(CC) $(CFLAGS) $(INC) -c $<

.cc.o:
	$(CX) $(CFLAGS) $(CXXFLAGS) $(INC) -c $<

y.tab.c: language.y fl.h language.h graph.h prefs_ext.h
	@$(YACC) -d -v language.y

lex.yy.c: lexer y.tab.h graph.h fl.h
	@$(LEX) lexer

clean:
	-@touch .q
	-@/bin/rm -f *.o fl y.tab.c y.tab.h y.output lex.yy.c .q
	-@/bin/rm -f prefs_ext.h prefs.h abi_version.h

# gcc -I ../../include/ -M *.c | sed -e 's/\/usr[^ ]*//g' > zz

#
#
# additional dependencies
#
arb_prec.o:	    arb_prec.h fl.h
cache.o:	    cache.h graph.h fl.h
compile.o:	    compile.h graph.h prefs_ext.h fl.h
emit.o:		    emit.h fl.h
error.o:	    error.h fl.h
expand_cursor.o:    expand_cursor.h fl.h
file_ops.o:	    file_ops.h fl.h
fl.o:		    fl.h y.tab.h prefs_ext.h fl.h
graph.o:	    graph.h prefs_ext.h typecheck.h fl.h
initialize.o:	    initialize.h prefs.h fl.h
io.o:		    io.h graph.h fl.h
lp.o:		    lp.h fl.h
memoize.o:	    memoize.h fl.h
new_bdd.o:	    new_bdd.h prefs_ext.h fl.h
remote_tcl.o:	    remote_tcl.h graph.h typecheck.h fl.h
symbol_tbl.o:	    symbol_tbl.h fl.h
typecheck.o:	    typecheck.h graph.h fl.h
bexpr.o:	    bexpr.h graph.h fl.h
draw_graph.o:	    draw_graph.h graph.h fl.h
debug.o:	    debug.h graph.h io.h fl.h
symbol.o:	    symbol.h graph.h typecheck.h fl.h
strings.o:	    strings.h graph.h typecheck.h fl.h
int_ops.o:	    int_ops.h graph.h typecheck.h fl.h
list_ops.o:	    list_ops.h graph.h typecheck.h fl.h
system.o:	    system.h graph.h typecheck.h fl.h
float.o:	    float.h graph.h typecheck.h fl.h
bv.o:		    bv.h graph.h typecheck.h fl.h
bev.o:		    bev.h graph.h typecheck.h fl.h
fsm.o:		    fsm.h pexlif.h graph.h typecheck.h fl.h
doc.o:		    doc.h graph.h typecheck.h fl.h
sha256.o:	    sha256.h graph.h typecheck.h fl.h
image.o:	    image.h graph.h typecheck.h fl.h
plugin.o:	    plugin.h strings.h typecheck.h abi_version.h
plugin_loader.o:    plugin_loader.h plugin.h strings.h typecheck.h abi_version.h plugin_errlog.h
plugin_checker.o:   plugin_loader.h plugin.h strings.h typecheck.h abi_version.h plugin_errlog.h
plugin_errlog.o:    plugin_errlog.h
table.o:	    table.h graph.h typecheck.h fl.h
iso.o:              iso.h fsm.h pexlif.h graph.h fl.h
pexlif.o:           pexlif.h graph.h strings.h fl.h

prefs.h: prefs.tcl
	tclsh make_prefs.tcl

prefs_ext.h: prefs.h

abi_version.h: $(ABI_HEADERS)
	echo '#ifndef FL_ABI_VERSION' > abi_version.h
	echo -n '#define FL_ABI_VERSION "' >> abi_version.h
	cat $(ABI_HEADERS) | sha256sum | awk '{print ($$1 "\"")}' >> abi_version.h
	echo '#endif' >> abi_version.h
