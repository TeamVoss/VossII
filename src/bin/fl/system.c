//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2017			*/
/*									*/
/************************************************************************/
#include <limits.h>
#include <time.h>
#include "system.h"
#include "graph.h"

/* ------------- Global variables ------------- */
string binary_location;

/********* Global variables referenced ***********/
extern str_mgr     strings;
extern bool	   gui_mode;
extern string      *fl_args;
extern char	   FailBuf[4096];

/***** PRIVATE VARIABLES *****/
static char buf [4096];
static char path_buf [PATH_MAX+1];

/* ----- Forward definitions local functions ----- */
static string get_binary_directory();

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
    binary_location = wastrsave(&strings, binary_location);
    setenv("VOSS-BINARY-DIRECTORY", binary_location, 0);

    if(lib_path == NULL) {
        Sprintf(buf, "%s/../vosslib", binary_location);
        setenv("VOSS-LIBRARY-DIRECTORY", wastrsave(&strings, buf), 0);
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
	SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, buf)));
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
	SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, fl_args[i])));
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
	MAKE_REDEX_PAIR(redex, res, Make_STRING_leaf(wastrsave(&strings, buf)));
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
	MAKE_REDEX_PAIR(redex, res, Make_STRING_leaf(wastrsave(&strings, buf)));
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
    MAKE_REDEX_STRING(redex, wastrsave(&strings, getenv("USER")));
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

    Add_ExtAPI_Function("wtime", "-", FALSE,
		GLmake_arrow(tv1, GLmake_tuple(tv1,GLmake_string())),
		wtime);

    Add_ExtAPI_Function("etime", "-", FALSE,
		GLmake_arrow(tv1, GLmake_tuple(tv1,GLmake_string())),
		etime);

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
}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static string
get_binary_directory()
{
    string tmp;
    Sprintf(buf, "/proc/%d/exe", (int) getpid());
    ssize_t sz = readlink(buf, path_buf, 1000);
    (void) sz;
    tmp = rindex(path_buf, '/');
    if( tmp == NULL ) {
        Eprintf("readlink did not find full path. WHAT???");
    }
    *tmp = 0;
    string res = wastrsave(&strings, path_buf);
    return res;
}
