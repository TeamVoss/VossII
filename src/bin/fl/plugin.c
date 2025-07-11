//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "plugin_loader.h"
#include "plugin.h"
#include "float.h"

extern str_mgr *stringsp;
extern char FailBuf[4096];

/* Return failure, with the given error message.
   Error message should be <1k in size.
 */
static void
fail(bool append_trace, g_ptr redex, const char* fmt, ...)
{
    static char msg[1024];
    va_list arg;
    va_start(arg, fmt);
    vsnprintf(msg, 1024, fmt, arg);
    va_end(arg);
    if(append_trace) {
        Fail_pr("%s", msg);
    } else {
        snprintf(FailBuf, 4096, "%s", msg);
    }
    SET_TYPE(redex, LEAF);
    SET_LEAF_TYPE(redex, PRIM_FN);
    SET_PRIM_FN(redex, P_FAIL);
    SET_FAIL_STRING(redex, wastrsave(stringsp, FailBuf));
}

static void
die(const char* fmt, ...)
{
    va_list args;
    fprintf(stderr, "die() called by plugin\n");
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(-1);
}

static string
wastrsave_strings(string s) { return wastrsave(stringsp, s); }
static void
make_redex_small_int(g_ptr redex, long int i) { MAKE_REDEX_AINT(redex, Arbi_FromInt(i)); }
static void
make_redex_large_int(g_ptr redex, char* i) { MAKE_REDEX_AINT(redex, Arbi_FromString(i, 10)); }
static void
make_redex_string(g_ptr redex, string s) { MAKE_REDEX_STRING(redex, s); }
static void
make_redex_bool(g_ptr redex, formula b) { MAKE_REDEX_BOOL(redex, b); }
static void
make_redex_nil(g_ptr redex) { MAKE_REDEX_NIL(redex); }
static void
make_redex_cons(g_ptr redex, g_ptr hd, g_ptr tl) { MAKE_REDEX_CONS_ND(redex, hd, tl); }
static void
make_redex_void(g_ptr redex) { MAKE_REDEX_VOID(redex); }
static void
make_redex_ext_obj(g_ptr redex, int class, pointer p) { MAKE_REDEX_EXT_OBJ(redex, class, p); }

static g_ptr
get_apply_left(g_ptr redex) { return GET_APPLY_LEFT(redex); }
static g_ptr
get_apply_right(g_ptr redex) { return GET_APPLY_RIGHT(redex); }
static int
is_nil(g_ptr redex) { return IS_NIL(redex); }
static g_ptr
get_head(g_ptr redex) { return force(GET_CONS_HD(redex), FALSE); }
static g_ptr
get_tail(g_ptr redex) { return force(GET_CONS_TL(redex), FALSE); }

static void*
get_int(g_ptr node) { return GET_AINT(node); }
static string
get_string(g_ptr node) { return GET_STRING(node); }
static void*
get_ext_obj(g_ptr node) { return GET_EXT_OBJ(node); }
static formula
get_bool(g_ptr node) { return GET_BOOL(node); }
static double
get_double(g_ptr node) { return ((float_ptr)GET_EXT_OBJ(node))->u.f; }

static typeExp_ptr
make_double() { return Get_Type("float", NULL, TP_DONT_INSERT); }

static g_ptr
new_large_int(char* i) { return Make_AINT_leaf(Arbi_FromString(i, 10)); }
static g_ptr
new_small_int(long int i) { return Make_AINT_leaf(Arbi_FromInt(i)); }

static void
inc_ref_cnt(g_ptr p) { INC_REFCNT(p); }

static g_ptr
new_void() { return void_nd; }

static void
print_node_type(g_ptr p)
{
    char type_buf[100];
    switch(GET_TYPE(p)) {
    case LAMBDA_ND:
        sprintf(type_buf, "a lambda node");
        break;
    case APPLY_ND:
        sprintf(type_buf, "an apply node");
        break;
    case CONS_ND:
        sprintf(type_buf, "a cons node");
        break;
    case LEAF:
        sprintf(type_buf, "a leaf node of type %ld", GET_LEAF_TYPE(p));
        break;
    }
    fprintf(stderr, "Node %lx is %s.\n", (unsigned long)p, type_buf);
}

static string
get_fail_string(g_ptr p) { return GET_FAIL_STRING(p); }

/* fl API accessible to plugins */
static fl_plugin_api_rec plugin_api_rec = {
    .register_types = register_types,
    .register_funs  = register_funs,

    .make_arrow   = GLmake_arrow,
    .make_tuple   = GLmake_tuple,
    .make_list    = GLmake_list,
    .make_void    = GLmake_void,
    .make_bool    = GLmake_bool,
    .make_int     = GLmake_int,
    .make_double  = make_double,
    .make_string  = GLmake_string,

    .die          = die,
    .one          = B_One,
    .zero         = B_Zero,
    .wastrsave    = wastrsave_strings,
    .fail         = fail,
    .dec_ref_cnt  = dec_ref_cnt,
    .inc_ref_cnt  = inc_ref_cnt,
    .get_node     = Get_node,
    .make_apply   = Make_APPL_ND,
    .force        = Eval,
    .gc_protect   = GC_Protect,
    .gc_unprotect = GC_Unprotect,
    .print_node_type = print_node_type,
    .is_fail = is_fail,
    .get_fail_string = get_fail_string,

    .make_redex_small_int = make_redex_small_int,
    .make_redex_large_int = make_redex_large_int,
    .make_redex_double    = make_redex_float,
    .make_redex_string    = make_redex_string,
    .make_redex_nil       = make_redex_nil,
    .make_redex_void      = make_redex_void,
    .make_redex_bool      = make_redex_bool,
    .make_redex_cons      = make_redex_cons,
    .make_redex_ext_obj   = make_redex_ext_obj,

    .new_small_int = new_small_int,
    .new_large_int = new_large_int,
    .new_double    = Make_float,
    .new_bool      = Make_BOOL_leaf,
    .new_string    = Make_STRING_leaf,
    .new_ext_obj   = Make_ext_obj,
    .new_nil       = Make_NIL,
    .new_cons      = Make_CONS_ND,
    .new_void      = new_void,

    .get_apply_left  = get_apply_left,
    .get_apply_right = get_apply_right,
    .is_nil          = is_nil,
    .get_head        = get_head,
    .get_tail        = get_tail,
    .get_int         = get_int,
    .get_double      = get_double,
    .get_string      = get_string,
    .get_bool        = get_bool,
    .get_ext_obj     = get_ext_obj
};

fl_plugin_api plugin_api = &plugin_api_rec;
