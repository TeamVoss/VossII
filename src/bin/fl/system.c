//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2017			*/
/*									*/
/************************************************************************/
#include <time.h>
#include <stdlib.h>
#include "system.h"
#include "graph.h"
#ifndef __APPLE__
#include <limits.h>
#else
#include <mach-o/dyld.h>
#include <sys/syslimits.h>
#endif

/* ------------- Global variables ------------- */
string binary_location;

/********* Global variables referenced ***********/
extern str_mgr     *stringsp;
extern bool	   gui_mode;
extern string      *fl_args;
extern char	   FailBuf[4096];
extern string      Voss_tmp_dir;

/***** PRIVATE VARIABLES *****/
static char buf [4096];
static char path_buf [PATH_MAX+1];

/* ----- Forward definitions local functions ----- */
static string	get_binary_directory();
static void	load_shared_library(g_ptr redex);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
System_Init()
{
    // Any needed initialization code
}

/* Initialize the binary and library paths.
   Any path given as NULL will be autodetected from the fl binary's location.
   NOTE: the VOSS-LIBRARY/BINARY-DIRECTORY environment variables, if set,
   will always take precedence over both given and autodetected paths.
*/
void
Init_Paths(string bin_path, string lib_path)
{
    if(bin_path == NULL) {
        binary_location = get_binary_directory();
    } else {
        binary_location = bin_path;
    }
    binary_location = wastrsave(stringsp, binary_location);
    setenv("VOSS-BINARY-DIRECTORY", binary_location, 0);

    if(lib_path == NULL) {
        Sprintf(buf, "%s/../vosslib", binary_location);
        setenv("VOSS-LIBRARY-DIRECTORY", wastrsave(stringsp, buf), 0);
    } else {
        setenv("VOSS-LIBRARY-DIRECTORY", lib_path, 0);
    }
}

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
my_exec(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string cmd = GET_STRING(r);
    FILE *fp = popen(cmd, "r");
    if (fp == NULL) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("Failed to execute command '%s'",cmd));
	return;
    }
    g_ptr output = Make_NIL();
    g_ptr tail  = output;
    while (fgets(buf, sizeof (buf), fp) != NULL) {
	buf[strlen(buf)-1] = 0;
	SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(stringsp, buf)));
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    int rc = pclose (fp);
    MAKE_REDEX_PAIR(redex, Make_INT_leaf(rc), output);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_pid(g_ptr redex)
{
    int pid = getpid();
    MAKE_REDEX_INT(redex, pid);
}

static void
noX_mode(g_ptr redex)
{
    if( gui_mode ) {
	MAKE_REDEX_BOOL(redex, B_Zero());
    } else {
	MAKE_REDEX_BOOL(redex, B_One());
    }
}

static void
ARGS(g_ptr redex)
{
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    int i = 0;
    while( fl_args[i] != NULL ) {
	SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(stringsp, fl_args[i])));
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	i++;
    }
    return;
}

static void
etime(g_ptr redex)
{
    g_ptr expr;
    EXTRACT_1_ARG(redex, expr);
    struct timespec start, end;
    clock_gettime(CLOCK_REALTIME, &start);
    INC_REFCNT(expr);
    g_ptr res = force(expr, FALSE);
    clock_gettime(CLOCK_REALTIME, &end);
 
    // time_spent = end - start
    double time_spent = (end.tv_sec - start.tv_sec) +
                        (end.tv_nsec - start.tv_nsec) / 1000000000.0;
    sprintf(buf, "%.9f", time_spent);
    if( is_fail(res) ) {
	MAKE_REDEX_FAILURE(redex, FailBuf);
    } else {
	MAKE_REDEX_PAIR(redex, res, Make_STRING_leaf(wastrsave(stringsp, buf)));
    }
}

static void
wtime(g_ptr redex)
{
    g_ptr expr;
    EXTRACT_1_ARG(redex, expr);
    wtimer_rec timer;
    Start_wtimer(&timer);
    INC_REFCNT(expr);
    g_ptr res = force(expr, FALSE);
    Stop_wtimer(&timer);
    if( is_fail(res) ) {
	MAKE_REDEX_FAILURE(redex, FailBuf);
    } else {
	Sprintf(buf, "%d.%d", Get_wseconds(&timer),
			      Get_wmicroseconds(&timer)/100000);
	MAKE_REDEX_PAIR(redex, res, Make_STRING_leaf(wastrsave(stringsp, buf)));
    }
}

static void
get_stack_trace(g_ptr redex)
{
    g_ptr res = Get_fl_stack_trace();
    OVERWRITE(redex, res);
}

static void
get_cur_eval_cond(g_ptr redex)
{
    MAKE_REDEX_BOOL(redex, Get_cur_eval_cond());
}

static void
get_USER(g_ptr redex)
{
    MAKE_REDEX_STRING(redex, wastrsave(stringsp, getenv("USER")));
}

static void
normalize_file(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    realpath(GET_STRING(r), path_buf);
    MAKE_REDEX_STRING(redex, wastrsave(stringsp, path_buf));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
emit_eval_graph(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr exprs;
    EXTRACT_1_ARG(redex, exprs);
    Sprintf(buf, "%s/eval_graph_emit_XXXXXX", Voss_tmp_dir);
    string dir = wastrsave(stringsp, mkdtemp(buf));
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    int cnt = 0;
    while( !IS_NIL(exprs) ) {
	string file = wastrsave(stringsp, tprintf("%s/%d.tar.gz", dir, cnt));
	if( !Save_graph("_dummy_sig_", file, GET_CONS_HD(exprs)) ) {
	    MAKE_REDEX_FAILURE(redex,
		Fail_pr("Saving element %d in emit_eval_graph failed\n", cnt));
	    return;
	}
	APPEND1(tail, Make_STRING_leaf(file));
	exprs = GET_CONS_TL(exprs);
	cnt++;
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
retrieve_eval_graph(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gfile;
    EXTRACT_1_ARG(redex, gfile);
    string file = GET_STRING(gfile);
    if( !Load_graph("_dummy_sig_", file, redex) ) {
	MAKE_REDEX_FAILURE(redex,
	    Fail_pr("Loading file %s in retrieve_eval_graph failed\n", file));
	return;
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
System_Install_Functions()
{
    // Add builtin functions
    Add_ExtAPI_Function("exec", "1", FALSE,
			GLmake_arrow(GLmake_string(),
			    GLmake_tuple(GLmake_int(),
					 GLmake_list(GLmake_string()))),
			my_exec);

    Add_ExtAPI_Function("noX", "", FALSE, GLmake_bool(), noX_mode);

    Add_ExtAPI_Function("pid", "", FALSE, GLmake_int(), do_pid);

    Add_ExtAPI_Function("ARGS", "", FALSE, GLmake_list(GLmake_string()), ARGS);

    typeExp_ptr tv1 = GLnew_tVar();

    Add_ExtAPI_Function("emit_eval_graph", "2", FALSE,
		GLmake_arrow(GLmake_list(tv1), GLmake_list(GLmake_string())),
		emit_eval_graph);

    Add_ExtAPI_Function("retrieve_eval_graph", "1", FALSE,
		GLmake_arrow(GLmake_string(), tv1),
		retrieve_eval_graph);

    Add_ExtAPI_Function("wtime", "-", FALSE,
		GLmake_arrow(tv1, GLmake_tuple(tv1,GLmake_string())),
		wtime);

    Add_ExtAPI_Function("etime", "-", FALSE,
		GLmake_arrow(tv1, GLmake_tuple(tv1,GLmake_string())),
		etime);

    Add_ExtAPI_Function("load_shared_library", "1", FALSE,
			 GLmake_arrow(GLmake_string(), GLmake_string()),
			 load_shared_library);

    Add_ExtAPI_Function("get_stack_trace", "1", FALSE,
			 GLmake_arrow(GLmake_void(),
				      GLmake_list(GLmake_string())),
			 get_stack_trace);

    Add_ExtAPI_Function("get_cur_eval_cond", "1", FALSE,
			GLmake_arrow(GLmake_void(), GLmake_bool()),
			get_cur_eval_cond);

    Add_ExtAPI_Function("USER", "", FALSE,
			GLmake_string(),
			get_USER);
    Add_ExtAPI_Function("normalize_file", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_string()),
			normalize_file);

}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static string
get_binary_directory()
{
    string tmp;
#ifndef __APPLE__
    Sprintf(buf, "/proc/%d/exe", (int) getpid());
    ssize_t sz = readlink(buf, path_buf, 1000);
    (void) sz;
#else
    uint32_t size = sizeof(path_buf);
    if (_NSGetExecutablePath(path_buf, &size) != 0)
	printf("buffer too small; need size %u\n", size);
    else
	printf("VossII path is %s\n", path_buf);
#endif
    tmp = rindex(path_buf, '/');
    if( tmp == NULL ) {
        Eprintf("readlink did not find full path. WHAT???");
    }
    *tmp = 0;

    int plen = strlen(path_buf);
    if( plen >= 11 && strcmp(path_buf+plen-11, "/src/bin/fl") == 0 ) {
	// Change the binary path to the bin directory if you are running
	// fl from the source directory.
	path_buf[plen-11] = 0;
	strcat(path_buf, "/bin");
    }
    string res = wastrsave(stringsp, path_buf);
    return res;
}

// Code to load yaccfl parsers

#include <dlfcn.h>  // Load last to avoid clashes with other .h definitions

static void
load_shared_library(g_ptr redex)
{
    bool    (*Install_function)();
    g_ptr   glibname;
    string  libname;
    const char *err_msgs = NULL;

    EXTRACT_1_ARG(redex, glibname);
    libname = GET_STRING(glibname);
    

    string shared_lib_name = realpath(tprintf("lib%s.so", libname), path_buf);
    if(  !shared_lib_name ) {
	shared_lib_name =
		realpath(tprintf("%s/lib%s.so",RCBinary_dir,libname),path_buf);
	if( !shared_lib_name ) {
	    string msg = Fail_pr("Cannot find shared library: lib%s.so\n",
					      libname);
	    MAKE_REDEX_FAILURE(redex,msg);
	    return;
	}
    }
    shared_lib_name = wastrsave(stringsp, shared_lib_name);

    void *handle = dlopen(shared_lib_name, RTLD_NOW);

    if( !handle ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("load_shared_library failed: %s\n",
					  dlerror()));
	return;
    }
    dlerror();

    string fun_name = tprintf("Install_%s", libname);
    Install_function = (bool (*)()) dlsym(handle, fun_name);

    err_msgs = dlerror();

    if( err_msgs ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find %s: %s\n",
					  fun_name, err_msgs));
	return;
    }
    dlerror();

    if( Install_function() == FALSE ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Install function %s failed: %s\n",
					  fun_name, FailBuf));
	return;
    }
    
    MAKE_REDEX_STRING(redex, shared_lib_name);
    return;
}

