//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "graph.h"
#include "prefs_ext.h"
#include "typecheck.h"
#include "symbol.h"
#include "plugin_loader.h"
#include <unistd.h>

/************************************************************************/
/*			Global Variables				*/
/************************************************************************/
extern bool		cephalopode_mode;
extern bool		compile_to_C_flag;
extern bool		transitive_visibility;
extern bool		debug_on;
extern bool		Do_gc_asap;
extern bool		Interrupt_asap;
extern bool		print_warnings;
extern str_mgr		strings;
extern FILE		*odests_fp;
extern FILE		*yyin;
extern old_yyin_ptr	cur_file;
extern symbol_tbl_ptr	symb_tbl;
extern bool		gui_mode;
extern jmp_buf	        *start_envp;

bool			Disable_GC;
bool			do_gc_asap;
bool		        file_load = FALSE;
int		        line_nbr = 1;
g_ptr		        root_node;
g_ptr		        *sp;
char		        *space_const;
buffer		        ext_fun_buf;
buffer		        ext_obj_buf;
char		        FailBuf[4096];
bool		        Abort_ASAP;
bool		        Compute_Every_Node;
bool		        No_top_values;
formula		        Abort_Value;
bool		        landscape;
bool		        return_trace_flag;
g_ptr		        void_nd;
hash_record		Constructor_Table;
hash_record		TypeSignatureTbl;
comment_list_ptr	cur_doc_comments = NULL;
rec_mgr			var_list_rec_mgr;

#if TRACK_FREEING
static string	garbage_collection_started_file;
static int	garbage_collection_started_line;
#endif

#if DBG_TRACE_AND_SAVE
extern int	        debug_id;
extern int	        debug_start_comparing;
static int	        dbg_start_cmp = FALSE;
static int	        force_cnt = 0;
static int	        top_level_print_cnt = 0;
static char	        top_level_print_buf[128];
static char	        top_level_dir[128];
static int	        top_level_traverse_left_cnt = -1;
static int	        START_SAVE_FROM = 99999999;
#endif

#ifdef DEBUG
void		    PR(g_ptr np);
int		    pr_depth;
static bool	    limitedPR(g_ptr node, odests fp, int num, bool strict);
#if TRACE_FUNCTION_CALLS
static int	    call_level = 0;
#endif
#endif /* DEBUG */

bool		    perform_fl_command(string txt);

/************************************************************************/
/*			Local Variables					*/
/************************************************************************/
static int		Call_level = 0;
static g_ptr		free_list = NULL;
static g_ptr		print_nd;
static g_ptr		stack[STACK_SIZE];
static int		allocated = 0;
static int		last_allocated = 0;
static int		gc_threshold = 250000;
static int		find_new_var_cnt = 1;
static int		uniq_quant_cnt = 1;
static buffer		fn_trace_buf;
static rec_mgr		g_rec_mgr;
static rec_mgr		result_rec_mgr;
static rec_mgr		comment_list_rec_mgr;
static rec_mgr		free_rec_mgr;
static hash_record	free_tbl;
static hash_record	gc_protect_tbl;
static rec_mgr		cl_rec_mgr;
static hash_record	pretty_printer_tbl;
static hash_record	hash_comp_tbl;
static char		buf[1024];
static char		*quantifier_name;
static char		*tmp_file_template1;
static g_ptr		return_trace_list = NULL;
static string		s_T;
static string		s_F;
static string		s_colon;
static string		s_seq;
static string		s_fseq;
static string		s_catch;
static string		s_gen_catch;
static string		s_hd;
static string		s_tl;
static string		s_fst;
static string		s_snd;
static string		s_APPLY;
static string		s_BEXPR;
static string		s_BOOL;
static string		s_CONS;
static string		s_INT;
static string		s_LAMBDA;
static string		s_LEAF;
static string		s_NIL;
static string		s_PRIM_FN;
static string		s_EXT_PRIM_FN;
static string		s_STRING;
static string		s_VAR;
static string		s_USERDEF;
static string		s_EXT_OBJ;
static string		s_NONE;
static string		s_SOME;
static string		s_ite;
static string		s_update_ref_var;

static buffer		global_gc_buf;
static formula		current_bdd_cond;
static formula		cur_eval_cond;

static struct prim_fun_name_rec {
				    int	    pfn;
				    string  name;
				    string  user_name;
				} pfn2name [] = {
		    {P_S, "P_S", ""},
		    {P_K, "P_K", ""},
		    {P_I, "P_I", ""},
		    {P_C, "P_C", ""},
		    {P_B, "P_B", ""},
		    {P_Y, "P_Y", ""},
		    {P_HEAD, "P_HEAD", "hd"},
		    {P_TAIL, "P_TAIL", "tl"},
		    {P_CONS, "P_CONS", ":"},
		    {P_PLUS, "P_PLUS", "+"},
		    {P_MINUS, "P_MINUS", "-"},
		    {P_TIMES, "P_TIMES", "*"},
		    {P_DIVIDE, "P_DIVIDE", "/"},
		    {P_COND, "P_COND", ""},
		    {P_LESS, "P_LESS", "<"},
		    {P_LEQ, "P_LEQ", "<="},
		    {P_GREATER, "P_GREATER", ">"},
		    {P_GEQ, "P_GEQ", ">="},
		    {P_EQUAL, "P_EQUAL", "="},
		    {P_NOT_EQUAL, "P_NOT_EQUAL", "!="},
		    {P_VAR, "P_VAR", "variable"},
		    {P_CAT, "P_CAT", "^"},
		    {P_EMPTY, "P_EMPTY", ""},
		    {P_LOAD, "P_LOAD", "load"},
		    {P_LOAD_PLUGIN, "P_LOAD_PLUGIN", "load_plugin"},
		    {P_ERROR, "P_ERROR", "error"},
		    {P_SEQ, "P_SEQ", "seq"},
		    {P_SPRIME, "P_SPRIME", ""},
		    {P_CPRIME, "P_CPRIME", ""},
		    {P_BSTAR, "P_BSTAR", ""},
		    {P_HELP, "P_HELP", "help"},
		    {P_FORALL, "P_FORALL", ""},
		    {P_THEREIS, "P_THEREIS", ""},
		    {P_PRINT, "P_PRINT", ""},
		    {P_IDENTICAL, "P_IDENTICAL", "=="},
		    {P_STRING_HD, "P_STRING_HD", ""},
		    {P_STRING_TL, "P_STRING_TL", ""},
		    {P_FAIL, "P_FAIL", ""},
		    {P_IS_CONS, "P_IS_CONS", ""},
		    {P_CATCH, "P_CATCH", "catch"},
		    {P_SUC, "P_SUC", ""},
		    {P_CONSTR_EQ, "P_CONSTR_EQ", ""},
		    {P_QUANT_FORALL, "P_QUANT_FORALL", ""},
		    {P_QUANT_THEREIS, "P_QUANT_THEREIS", ""},
		    {P_PFAIL, "P_PFAIL", ""},
		    {P_PCATCH, "P_PCATCH", ""},
		    {P_ORD, "P_ORD", "ord"},
		    {P_CHR, "P_CHR", "chr"},
		    {P_TUPLE, "P_TUPLE", ""},
		    {P_FST, "P_FST", "fst"},
		    {P_SND, "P_SND", "snd"},
		    {P_EXPLODE, "P_EXPLODE", "explode"},
		    {P_IMPLODE, "P_IMPLODE", "implode"},
		    {P_DEPENDS, "P_DEPENDS", "depends"},
		    {P_BOOL2STR, "P_BOOL2STR", "bool2str"},
		    {P_INT2STR, "P_INT2STR", "int2str"},
		    {P_TIME, "P_TIME", "time"},
		    {P_BDD_SIZE, "P_BDD_SIZE", "bdd_size"},
		    {P_SUBSTITUTE, "P_SUBSTITUTE", "substitute"},
		    {P_BPRIME, "P_BPRIME", ""},
		    {P_PROFILE, "P_PROFILE", "bdd_profile"},
		    {P_TRACE, "P_TRACE", ""},
		    {P_BDD_SAVE, "P_BDD_SAVE", "bdd_save"},
		    {P_BDD_LOAD, "P_BDD_LOAD", "bdd_load"},
		    {P_BDD_REORDER, "P_BDD_REORDER", "bdd_reorder"},
		    {P_RELPROD_THEREIS, "P_RELPROD_THEREIS", ""},
		    {P_RELPROD_FORALL, "P_RELPROD_FORALL", ""},
		    {P_RVAR, "P_RVAR", "rvariable"},
		    {P_VAR_ORDER, "P_VAR_ORDER", "var_order"},
		    {P_SYSTEM, "P_SYSTEM", "system"},
		    {P_EVAL, "P_EVAL", "eval"},
		    {P_CACHE, "P_CACHE", ""},
		    {P_IS_TUPLE, "P_IS_TUPLE", ""},
		    {P_STRICT_CONS, "P_STRICT_CONS", ""},
		    {P_STRICT_TUPLE, "P_STRICT_TUPLE", ""},
		    {P_FSM_OBJECT, "P_FSM_OBJECT", ""},
		    {P_MK_REF_VAR, "P_MK_REF_VAR", ""},
		    {P_DEREFERENCE, "P_DEREFERENCE", "deref"},
		    {P_UPDATE_RVAR, "P_UPDATE_RVAR", ":="},
		    {P_REF_VAR, "P_REF_VAR", ""},
		    {P_WRITE_TO_FILE, "P_WRITE_TO_FILE", ""},
		    {P_TCL_EVAL, "P_TCL_EVAL", "tcl_eval"},
		    {P_FSEQ, "P_FSEQ", "fseq"},
		    {P_NSEQ, "P_NSEQ", ""},
		    {P_FILEFP, "P_FILEFP", "filter"},
		    {P_FOPEN, "P_FOPEN", "fopen"},
		    {P_FCLOSE, "P_FCLOSE", "fclose"},
		    {P_FFLUSH, "P_FFLUSH", "fflush"},
		    {P_GEN_CATCH, "P_GEN_CATCH", ""},
		    {P_GET_VOSSRC, "P_GET_VOSSRC", "get_vossrc"},
		    {P_UPDATE_VOSSRC, "P_UPDATE_VOSSRC", "update_vossrc"},
		    {P_EXTAPI_FN, "P_EXTAPI_FN", ""},
		    {P_SAVE_GRAPH, "P_SAVE_GRAPH", ""},
		    {P_LOAD_GRAPH, "P_LOAD_GRAPH", ""},
		    {P_EXIT, "P_EXIT", "quit"},
		    {P_THEN, "P_THEN", ""},
		    {P_UNTYPE, "P_UNTYPE", ""},
		    {P_UNQUOTE, "P_UNQUOTE", ""},
		    {P_VOID, "P_VOID", ""},
		    // Empty
};
static char pfn2str_buf[512];

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/
/* -----Function prototypes for local functions-----*/
static g_ptr		mk_constr_nd(string constr_name);
static int		is_large(g_ptr node);
static void 		refcnt_free_nd(g_ptr node);
static g_ptr 		type_constr_rec(int cnt, int arg_cnt, g_ptr constr);
static g_ptr 		abstract_rec(string var, g_ptr node);
static bool  		is_free(string var, g_ptr node);
static bool  		cached_is_free(string var, g_ptr node);
static bool  		is_I(g_ptr e);
static bool  		is_B(g_ptr e);
static bool		is_CONS_fun(g_ptr node);
static bool		is_T_fun(g_ptr node);
static bool		is_F_fun(g_ptr node);
static bool  		check_true_or_false(g_ptr node);
static bool  		print_result_local(g_ptr node, odests fp, bool pr_brack,
				   bool pr_comma, typeExp_ptr type);
static g_ptr 		compile_rec(g_ptr node);
static res_rec		compile(g_ptr node, free_list_ptr vlp);
static void  		sweep();
static void  		push_trace_fn(string name);
static void  		pop_trace_fn();
static void		free_CL_node(cl_ptr nd);
static g_ptr		convert_CL_rec(cl_ptr nd);
static g_ptr		pat_match_rec(g_ptr arg, g_ptr E, int cnt,
				      string *name,int *c);
static g_ptr		get_constr_name(g_ptr node);
static g_ptr		do_explode(string s);
static g_ptr		trarg(g_ptr node, g_ptr E, bool constructor);
static free_list_ptr	create_free_tbl(g_ptr node);
static free_list_ptr	set_union(free_list_ptr s1, free_list_ptr s2);
static free_list_ptr	remove_element(free_list_ptr s, string var);
static free_list_ptr	get_set(g_ptr node);
static string		make_Destructor(g_ptr np);
static void		walk_graph(gmap_info_ptr ip, g_ptr node);
static g_ptr		gen_map_rec(gmap_info_ptr ip, g_ptr node);
static bool		walk2_graph(gmap_info_ptr ip, g_ptr l, g_ptr r);
static g_ptr		gen_map2_rec(gmap_info_ptr ip, g_ptr l,g_ptr r);
static unsigned int	gmap2_hash(pointer np, unsigned int n);
static bool		gmap2_equ(pointer p1, pointer p2);
static g_ptr		g_b_ite(g_ptr l, g_ptr r);
static g_ptr		reduce_list(g_ptr redex, int levels);
static bool		is_pat_fail(g_ptr node);

#ifdef DEBUG
static void		remove_hash();
static void		set_up_hash();
static bool		accessible(g_ptr cur, g_ptr ref);
static bool		cmp_graph_rec(hash_record *dtbl,g_ptr l1, g_ptr l2);
#ifdef CHECK_REF_CNTS
static bool	check_ref_cnt_rec(g_ptr np);
#endif
#endif /* DEBUG */

#if PARANOIA
/* Paranoia tests. */
string
f_GET_LAMBDA_VAR(g_ptr node)
{
    if( !IS_LAMBDA(node) )
	DIE("VIOLATION");
    return( M_GET_LAMBDA_VAR(node) );
}

g_ptr
f_GET_LAMBDA_BODY(g_ptr node)
{
    if( !IS_LAMBDA(node) )
	DIE("VIOLATION");
    return( M_GET_LAMBDA_BODY(node) );
}

g_ptr
f_GET_APPLY_LEFT(g_ptr node)
{
    if( !IS_APPLY(node) )
	DIE("VIOLATION");
    return( M_GET_APPLY_LEFT(node) );
}

g_ptr
f_GET_APPLY_RIGHT(g_ptr node)
{
    if( !IS_APPLY(node) )
	DIE("VIOLATION");
    return( M_GET_APPLY_RIGHT(node) );
}

g_ptr
f_GET_CONS_HD(g_ptr node)
{
    if( !IS_CONS(node) )
	DIE("VIOLATION");
    return( M_GET_CONS_HD(node) );
}

g_ptr
f_GET_CONS_TL(g_ptr node)
{
    if( !IS_CONS(node) ) {
	DIE("VIOLATION");
    }
    return( M_GET_CONS_TL(node) );
}

arbi_T
f_GET_AINT(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_INT(node) )
	DIE("VIOLATION");
    return( M_GET_AINT(node) );
}

string
f_GET_STRING(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_STRING(node) )
	DIE("VIOLATION");
    return( M_GET_STRING(node) );
}

string
f_GET_PRINTF_STRING(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_PRIM_FN(node) )
	DIE("VIOLATION");
    switch( GET_PRIM_FN(node) ) {
	case P_PRINTF:
	case P_FPRINTF:
	case P_SPRINTF:
	case P_EPRINTF:
	case P_SSCANF:
	    break;
	default:
	    DIE("VIOLATION");
    }
    return( M_GET_PRINTF_STRING(node) );
}

string
f_GET_DEBUG_STRING(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_PRIM_FN(node) )
	DIE("VIOLATION");
    if( GET_PRIM_FN(node) != P_DEBUG )
	DIE("VIOLATION");
    return( M_GET_DEBUG_STRING(node) );
}

void
f_SET_APPLY_LEFT(g_ptr np, g_ptr v)
{
    SET_APPLY_LEFT(np,v);
}

void
f_SET_APPLY_RIGHT(g_ptr np, g_ptr v)
{
    SET_APPLY_RIGHT(np,v);
}

string
f_GET_FAIL_STRING(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_PRIM_FN(node) )
	DIE("VIOLATION");
    if( GET_PRIM_FN(node) != P_FAIL )
	DIE("VIOLATION");
    return( M_GET_FAIL_STRING(node) );
}

formula
f_GET_BOOL(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_BOOL(node) )
	DIE("VIOLATION");
    return( M_GET_BOOL(node) );
}

bexpr
f_GET_BEXPR(g_ptr node)
{
    if( !IS_LEAF(node) )
	DIE("VIOLATION");
    if( !IS_BEXPR(node) )
	DIE("VIOLATION");
    return( M_GET_BEXPR(node) );
}

#if DEBUG
void
check_list(g_ptr node)
{
    g_ptr cur;
    if( node == NULL )
	return;
    cur = node;
    while( cur && IS_CONS(cur) )
	cur = GET_CONS_TL(cur);
    if( cur != NULL ) {
	FP(err_fp, "WARNING: WARNING:\n");
	PR(cur);
    }
}
#endif

#endif /* PARANOIA */

/************************************************************************/
/*			Public Functions				*/
/************************************************************************/

VOID
G_Init()
{
    string s;
    Disable_GC = FALSE;
    aligned_new_mgr(&g_rec_mgr, sizeof(g_rec), 8);
    new_mgr(&result_rec_mgr, sizeof(result_rec));
    new_mgr(&cl_rec_mgr, sizeof(cl_rec));
    new_mgr(&var_list_rec_mgr, sizeof(var_list_rec));
    new_mgr(&comment_list_rec_mgr, sizeof(comment_list_rec));
    new_buf(&ext_fun_buf, 100, sizeof(ext_fun_rec));
    new_buf(&ext_obj_buf, 100, sizeof(ext_obj_rec));
    new_buf(&fn_trace_buf, 1000, sizeof(string));
    new_buf(&global_gc_buf, 100, sizeof(g_ptr));
    do_gc_asap = FALSE;
    space_const = wastrsave(&strings, " ");
    quantifier_name = wastrsave(&strings, "q");
    create_hash(&gc_protect_tbl, 100, ptr_hash, ptr_equ);
    create_hash(&Constructor_Table, 100, Ustr_hash, Ustr_equ);
    create_hash(&TypeSignatureTbl, 100, Ustr_hash, Ustr_equ);
    create_hash(&pretty_printer_tbl, 100, Type_hash, Type_eq);
    print_nd = NULL;
    cur_eval_cond = B_One();
    sp = &(stack[STACK_SIZE-1]);
    void_nd = Make_0inp_Primitive(P_VOID);
    SET_REFCNT(void_nd,MAX_REF_CNT);
#ifdef DEBUG
    pr_depth = 5;
#endif
    s = strtemp(RC_TMP_FILE_DIR);
    s = strappend("/FL_BOOL2STR_XXXXXX");
    tmp_file_template1 = wastrsave(&strings, s);
    /* Place strings in unique table */
    s_T = wastrsave(&strings, "T");
    s_F = wastrsave(&strings, "F");
    s_colon = wastrsave(&strings, ":");
    s_seq = wastrsave(&strings, "seq");
    s_fseq = wastrsave(&strings, "fseq");
    s_catch = wastrsave(&strings, "catch");
    s_gen_catch = wastrsave(&strings, "gen_catch");
    s_hd = wastrsave(&strings, "hd");
    s_tl = wastrsave(&strings, "tl");
    s_fst = wastrsave(&strings, "fst");
    s_snd = wastrsave(&strings, "snd");
    s_ite = wastrsave(&strings, "symbolic if-the-else");
    s_update_ref_var = wastrsave(&strings, "symbolic :=");
    //
    s_APPLY       = wastrsave(&strings, "APPLY");
    s_VAR         = wastrsave(&strings, "VAR");
    s_USERDEF     = wastrsave(&strings, "USERDEF");
    s_LAMBDA      = wastrsave(&strings, "LAMBDA");
    s_LEAF        = wastrsave(&strings, "LEAF");
    s_CONS        = wastrsave(&strings, "CONS");
    s_NIL         = wastrsave(&strings, "NIL");
    s_INT         = wastrsave(&strings, "INT");
    s_STRING      = wastrsave(&strings, "STRING");
    s_BOOL        = wastrsave(&strings, "BOOL");
    s_BEXPR       = wastrsave(&strings, "BEXPR");
    s_EXT_OBJ     = wastrsave(&strings, "EXT_OBJ");
    s_PRIM_FN     = wastrsave(&strings, "PRIM_FN");
    s_EXT_PRIM_FN = wastrsave(&strings, "EXT_PRIM_FN");
    s_NONE        = Mk_constructor_name("NONE");
    s_SOME        = Mk_constructor_name("SOME");
}

string
Mk_constructor_name(string constr_name)
{
    string name = strtemp("!,.:");
    name = wastrsave(&strings, strappend(constr_name));
    return name;
}

void
AddComment(string s)
{
    comment_list_ptr clp = (comment_list_ptr) new_rec(&comment_list_rec_mgr);	    clp->comment = wastrsave(&strings, s);
    clp->line = line_nbr;
    clp->next = cur_doc_comments;
    cur_doc_comments = clp;
}

void
PUSH_GLOBAL_GC(g_ptr res)
{
    SET_REFCNT(res, MAX_REF_CNT);
    push_buf(&global_gc_buf, (pointer) &res);
}

void
POP_GLOBAL_GC(int cnt)
{
    for(int i = 0; i < cnt; i++) {
        pop_buf(&global_gc_buf, NULL);
    }
}

bool
GC_Protect(g_ptr g)
{
    SET_REFCNT(g, MAX_REF_CNT);
    if( find_hash(&gc_protect_tbl, (pointer) g) != NULL ) {
	return FALSE;
    } else {
	insert_hash(&gc_protect_tbl, (pointer) g, (pointer) g);
	return TRUE;
    }
}

bool
GC_Unprotect(g_ptr g)
{
    if( find_hash(&gc_protect_tbl, (pointer) g) != NULL ) {
	return FALSE;
    } else {
	delete_hash(&gc_protect_tbl, (pointer) g);
	return TRUE;
    }
}

g_ptr
Make_Failure(string msg)
{
    g_ptr ret = Get_node();
    MAKE_REDEX_FAILURE(ret, msg);
    SET_LINE_NBR(ret,line_nbr);
    MAKE_FORCED(ret);
    return( ret );
}

g_ptr
Make_BOOL_leaf(formula f)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, BOOL);
    SET_BOOL(ret, f);
    SET_LINE_NBR(ret,line_nbr);
    MAKE_FORCED(ret);
    return( ret );
}

g_ptr
Make_BEXPR_leaf(bexpr f)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, BEXPR);
    SET_BEXPR(ret, f);
    SET_LINE_NBR(ret,line_nbr);
    MAKE_FORCED(ret);
    return( ret );
}

g_ptr
Make_AINT_leaf(arbi_T value)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, INT);
    SET_AINT(ret, value);
    SET_LINE_NBR(ret,line_nbr);
    MAKE_FORCED(ret);
    return( ret );
}

g_ptr
Make_INT_leaf(int value)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, INT);
    SET_INT(ret, value);
    SET_LINE_NBR(ret,line_nbr);
    MAKE_FORCED(ret);
    return( ret );
}

g_ptr
Make_STRING_leaf(string s)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, STRING);
    SET_STRING(ret, s);
    SET_LINE_NBR(ret,line_nbr);
    MAKE_FORCED(ret);
    return( ret );
}

g_ptr
Make_VAR_leaf(string s)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, VAR);
    SET_VAR(ret, s);
    SET_LINE_NBR(ret,line_nbr);
    return( ret );
}

g_ptr
Make_USERDEF_leaf(fn_ptr fp)
{
    g_ptr ret = Get_node();
    SET_TYPE(ret, LEAF);
    SET_LEAF_TYPE(ret, USERDEF);
    SET_USERDEF(ret, fp);
    SET_LINE_NBR(ret,line_nbr);
    return( ret );
}

g_ptr
Make_NIL()
{
    g_ptr ret = Make_CONS_ND(NULL, NULL);
    return( ret );
}

g_ptr
Make_SINGLETON(g_ptr leaf)
{
    g_ptr ret = Make_CONS_ND(leaf, Make_NIL());
    return ret;
}

g_ptr
Make_Printf_Primitive(int pr_fn, string pat)
{
    g_ptr res;
    if( pr_fn == P_PRINTF && Printf_arity(pr_fn, pat) == 0 ) {
	// Replace printf "constant_string" with print "constant_string"
	// That will ensure the print(f) is non_lazy
	g_ptr fn_node;
	fn_node = Get_node();
	SET_TYPE(fn_node, LEAF);
	SET_LEAF_TYPE(fn_node, PRIM_FN);
	SET_PRIM_FN(fn_node, P_PRINT);
	res = Make_APPL_ND(fn_node, Make_STRING_leaf(wastrsave(&strings, pat)));
	return( res );
    }
    res = Get_node();
    SET_TYPE(res, LEAF);
    SET_LEAF_TYPE(res, PRIM_FN);
    SET_PRIM_FN(res, pr_fn);
    SET_LINE_NBR(res,line_nbr);
    string spat = wastrsave(&strings, pat);
    SET_PRINTF_STRING(res, spat);
    return( res );
}

g_ptr
Make_Debug_Primitive(string name)
{
    g_ptr fn_node;
    fn_node = Get_node();
    SET_TYPE(fn_node, LEAF);
    SET_LEAF_TYPE(fn_node, PRIM_FN);
    SET_PRIM_FN(fn_node, P_DEBUG);
    SET_LINE_NBR(fn_node,line_nbr);
    string res = wastrsave(&strings, name);
    SET_DEBUG_STRING(fn_node, res);
    return( fn_node );
}

g_ptr
Make_0inp_Primitive(int pr_fn)
{
    g_ptr fn_node;
    fn_node = Get_node();
    SET_TYPE(fn_node, LEAF);
    SET_LEAF_TYPE(fn_node, PRIM_FN);
    SET_PRIM_FN(fn_node, pr_fn);
    SET_LINE_NBR(fn_node,line_nbr);
    return( fn_node );
}

g_ptr
Make_1inp_Primitive(int pr_fn, g_ptr arg1)
{
    g_ptr root, fn_node;
    fn_node = Get_node();
    SET_TYPE(fn_node, LEAF);
    SET_LEAF_TYPE(fn_node, PRIM_FN);
    SET_PRIM_FN(fn_node, pr_fn);
    SET_LINE_NBR(fn_node,line_nbr);
    root    = Get_node();
    SET_TYPE(root, APPLY_ND);
    SET_APPLY_LEFT(root, fn_node);
    SET_APPLY_RIGHT(root, arg1);
    return( root );
}

g_ptr
Make_2inp_Primitive(int pr_fn, g_ptr arg1, g_ptr arg2)
{
    g_ptr root, root2, fn_node;
    fn_node = Get_node();
    SET_TYPE(fn_node, LEAF);
    SET_LEAF_TYPE(fn_node, PRIM_FN);
    SET_PRIM_FN(fn_node, pr_fn);
    SET_LINE_NBR(fn_node,line_nbr);
    root2 = Get_node();
    SET_TYPE(root2, APPLY_ND);
    SET_APPLY_LEFT(root2, fn_node);
    SET_APPLY_RIGHT(root2, arg1);
    root = Get_node();
    SET_TYPE(root, APPLY_ND);
    SET_APPLY_LEFT(root, root2);
    SET_APPLY_RIGHT(root, arg2);
    return( root );
}

g_ptr
Make_3inp_Primitive(int pr_fn, g_ptr arg1, g_ptr arg2, g_ptr arg3)
{
    g_ptr root, root2, root3, fn_node;
    fn_node = Get_node();
    SET_TYPE(fn_node, LEAF);
    SET_LEAF_TYPE(fn_node, PRIM_FN);
    SET_PRIM_FN(fn_node, pr_fn);
    SET_LINE_NBR(fn_node,line_nbr);
    root3 = Get_node();
    SET_TYPE(root3, APPLY_ND);
    SET_APPLY_LEFT(root3, fn_node);
    SET_APPLY_RIGHT(root3, arg1);
    root2 = Get_node();
    SET_TYPE(root2, APPLY_ND);
    SET_APPLY_LEFT(root2, root3);
    SET_APPLY_RIGHT(root2, arg2);
    root = Get_node();
    SET_TYPE(root, APPLY_ND);
    SET_APPLY_LEFT(root, root2);
    SET_APPLY_RIGHT(root, arg3);
    return( root );
}

g_ptr
Make_CONS_ND(g_ptr expr1, g_ptr expr2)
{
    g_ptr root;
    root = Get_node();
    SET_TYPE(root, CONS_ND);
    SET_CONS_HD(root, expr1);
    SET_CONS_TL(root, expr2);
    return( root );
}

g_ptr
Make_APPL_ND(g_ptr expr1, g_ptr expr2)
{
    g_ptr root;
    root = Get_node();
    SET_TYPE(root, APPLY_ND);
    SET_APPLY_LEFT(root, expr1);
    SET_APPLY_RIGHT(root, expr2);
    return( root );
}


g_ptr
Make_Lambda(string var, g_ptr expr)
{
    g_ptr root;
    root = Get_node();
    SET_TYPE(root, LAMBDA_ND);
    SET_LAMBDA_VAR(root, var);
    SET_LAMBDA_BODY(root, expr);
    return( root );
}

g_ptr
Make_PAIR_ND(g_ptr fst, g_ptr snd)
{
    g_ptr root;
    root = Get_node();
    SET_TYPE(root, CONS_ND);
    SET_CONS_HD(root, fst);
    SET_CONS_TL(root, snd);
    return( root );
}

string
Find_new_free_var(g_ptr expr)
{
    do {
	Sprintf(buf, "_q_%d", find_new_var_cnt++);
    } while( is_free(buf, expr) );
    return( wastrsave(&strings, buf) );
}

g_ptr
Make_arglist(int cnt)
{
    char	buf[20];
    string	name;

    if( cnt == 0 )
	return(Make_NIL());
    Sprintf(buf, "_Q_%d", cnt);
    name = wastrsave(&strings, buf);
    return( Make_2inp_Primitive(P_STRICT_TUPLE, Make_VAR_leaf(name),
					 Make_arglist(cnt-1)) );
}

g_ptr
Add_args(int cnt, g_ptr expr)
{
    char	buf[20];
    string	name;

    if( cnt == 0 )
	return(expr);

    Sprintf(buf, "_Q_%d", cnt);
    name = wastrsave(&strings, buf);
    return( Make_Lambda(name, Add_args(cnt-1,expr)) );
}

g_ptr
Remove_Pattern_Matching(g_ptr lhs, g_ptr rhs, string *name, int *cnt)
{
    /* lhs should look like ((.(fn_name arg1) agr2).)argn)	*/
    /* and arg1 is a pattern					*/
    return( pat_match_rec(lhs, rhs, 1, name, cnt) );
}

void
Record_eval_context(eval_ctx_ptr ctx)
{
    ctx->old_sp = sp;
    ctx->old_root_node = root_node;
    ctx->old_start_envp = start_envp;
    ctx->old_call_level = Call_level;
    ctx->old_fn_trace_buf_size = COUNT_BUF(&fn_trace_buf);
    ctx->cur_eval_cond = cur_eval_cond;
}

void
Restore_eval_context(eval_ctx_ptr ctx)
{
    sp = ctx->old_sp;
    root_node = ctx->old_root_node;
    start_envp = ctx->old_start_envp;
    Call_level = ctx->old_call_level;
    int cur_sz = COUNT_BUF(&fn_trace_buf);
    if( cur_sz < ctx->old_fn_trace_buf_size )
	DIE("Bad bug in fn_trace_buf management.... Guru time!");
    if( cur_sz > ctx->old_fn_trace_buf_size ) {
	resize_buf(&fn_trace_buf, ctx->old_fn_trace_buf_size);
    
    }
    cur_eval_cond = ctx->cur_eval_cond;
}

void
Reset_eval_context()
{
    sp = &(stack[STACK_SIZE-1]);
    root_node = void_nd;
    start_envp = NULL;
    Call_level = 0;
    resize_buf(&fn_trace_buf, 0);
    while( cur_file != NULL ) {
	fclose(yyin);
	yyin = Return_to_old_fid();
    }
    file_load = FALSE;
}

void
Install_PrinterFn(result_ptr res)
{
    typeExp_ptr pr_type;
    if( res == NULL )
	return;
    pr_type = PrintConvFn(res->type);
    if( pr_type == NULL ) {
	FP(err_fp, "Type of pretty printer function must be *->string\n");
	return;
    }
    eval_ctx_rec ctx;
    Record_eval_context(&ctx);
    root_node = res->expr;
    jmp_buf start_env;
    start_envp = &start_env;
    if( setjmp(*start_envp) == 0 ) {
	res->expr = force(res->expr, TRUE);
	if( is_fail(res->expr) ) {
	    FP(err_fp, "Failure:    %s", GET_FAIL_STRING(res->expr));
	} else {
	    insert_hash(&pretty_printer_tbl, (pointer)pr_type,
			(pointer)(res->expr));
	}
    } else {
	/* Return from a failure */
	FP(err_fp, "Failure:    %s", FailBuf);
    }
    Restore_eval_context(&ctx);
}

VOID
Print_Result(result_ptr res, odests fp, bool print)
{
    if( res == NULL ) {
	return;
    }
    eval_ctx_rec ctx;
    Record_eval_context(&ctx);
    FailBuf[0] = 0;
    //
    root_node = res->expr;
    PUSH_GLOBAL_GC(root_node);
    SET_REFCNT(root_node, MAX_REF_CNT);
    jmp_buf start_env;
    start_envp = &start_env;
    Call_level = 0;
#ifdef DBG_TRACE_AND_SAVE
    fprintf(stderr, "Print_Result: %d\n", force_cnt);
#endif
#ifdef CHECK_REF_CNTS
    check_ref_cnts(root_node);
#endif
    bool ok = TRUE;
    if( setjmp(*start_envp) == 0 ) {
	/* All ok */
	res->expr = force(res->expr, TRUE);
	if( is_fail(res->expr) ) {
	    ok = FALSE;
	    FP(err_fp, "Failure:    %s", GET_FAIL_STRING(res->expr));
	    FailBuf[0] = 0;
	} else {
            if( print ) {
                print_result_local(res->expr,fp,TRUE,FALSE,res->type);
            }
	}
    } else {
	/* Return from a failure */
	SET_TYPE(res->expr, LEAF);
	SET_LEAF_TYPE(res->expr, PRIM_FN);
	SET_PRIM_FN(res->expr, P_FAIL);
	FP(err_fp, "Failure:    %s", FailBuf);
	FailBuf[0] = 0;
	ok = FALSE;
    }
    POP_GLOBAL_GC(1);
    Restore_eval_context(&ctx);
    if( file_load && !ok ) {
	longjmp(*start_envp,1);
    }
}

g_ptr
TrArg(g_ptr node, g_ptr E, bool constructor)
{
    g_ptr res;
    jmp_buf start_env;
    jmp_buf *old_start_envp = start_envp;
    start_envp = &start_env;
    if( setjmp(*start_envp) == 0 ) {
	/* All ok */
	res = trarg(node, E, constructor);
    } else {
	/* Return from a failure */
	res = NULL;
    }
    start_envp = old_start_envp;
    return res;
}


VOID
Print_Expr(g_ptr node, odests fp)
{
    g_ptr h, t;

    if( node == NULL ) {
	FP(fp, "NIL");
	return;
    }
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    FP(fp, "(");
	    Print_Expr(GET_APPLY_LEFT(node), fp);
	    FP(fp, " ");
	    Print_Expr(GET_APPLY_RIGHT(node), fp);
	    FP(fp, ")");
	    break;
	case LAMBDA_ND:
	    FP(fp, "(lambda %s.", GET_LAMBDA_VAR(node));
	    Print_Expr(GET_LAMBDA_BODY(node), fp);
	    FP(fp, ")");
	    break;
	case CONS_ND:
	    h = GET_CONS_HD(node);
	    t = GET_CONS_TL(node);
	    if( h == t && h == NULL ) {
		FP(fp, "NIL");
		break;
	    }
	    ASSERT(h != NULL && t != NULL );
	    FP(fp, "(");
	    Print_Expr(h, fp);
	    FP(fp, ":");
	    Print_Expr(t, fp);
	    FP(fp, ")");
	    break;
	case LEAF:
	    Print_leaf(node, fp);
	    break;
	default:
	    DIE("Illegal node type");
    }
}



void
inc_refcnt_children(g_ptr node)
{
    if( node == NULL ) return;
    if( GET_TYPE(node) == APPLY_ND) {
	/* An extra access path to LEFT and RIGHT node */
	INC_REFCNT(GET_APPLY_LEFT(node));
	INC_REFCNT(GET_APPLY_RIGHT(node));
    } else
    if( GET_TYPE(node) == CONS_ND) {
	/* An extra access path to LEFT and RIGHT node */
	if( GET_CONS_HD(node) == NULL ) return;
	ASSERT( GET_CONS_TL(node) != NULL);
	INC_REFCNT(GET_CONS_HD(node));
	INC_REFCNT(GET_CONS_TL(node));
    }
}


void
#if TRACK_FREEING
dec_ref_cnt(g_ptr node, string file, int line)
{
    if(node == NULL)
	return;
    node->oldL = node->L;
    node->oldR = node->R;
    node->file = file;
    node->line = line;
#else
dec_ref_cnt(g_ptr node)
{
    (void) node;
#endif
#ifndef NO_REFCNT_GC
    /* Normal operation */
    if(node == NULL)
	return;
    switch( GET_REFCNT(node) ) {
	case 0:
		/* A single reference that now is gone */
		/* Decrement recursively */
		switch( GET_TYPE(node) ) {
		    case APPLY_ND:
			DEC_REF_CNT2(GET_APPLY_LEFT(node));
			DEC_REF_CNT2(GET_APPLY_RIGHT(node));
			break;
		    case CONS_ND:
			DEC_REF_CNT2(GET_CONS_HD(node));
			DEC_REF_CNT2(GET_CONS_TL(node));
			break;
		    case LEAF:
			break;
		    default:
			DIE("Illegal node type");
		}
		refcnt_free_nd(node);
		break;
	case 1:
		SET_REFCNT(node,0);
		break;
	case 2:
		DIE("Illegal reference count value\n");
		break;
	case 3:
		break;
    }
#else
    /* Don't free any nodes through reference counting */
    return;
#endif
}

bool
is_fail(g_ptr node)
{
    return(    GET_TYPE(node) == LEAF
	    && GET_LEAF_TYPE(node) == PRIM_FN
	    && GET_PRIM_FN(node) == P_FAIL);
}

static bool
is_pat_fail(g_ptr node)
{
    return(    GET_TYPE(node) == LEAF
	    && GET_LEAF_TYPE(node) == PRIM_FN
	    && GET_PRIM_FN(node) == P_PFAIL);
}

void
Free_result_ptr(result_ptr rp)
{
    free_rec(&result_rec_mgr, (pointer) rp);
}

result_ptr
Compile(symbol_tbl_ptr stbl, g_ptr onode, typeExp_ptr type, bool delayed)
{
    res_rec	rt;
    g_ptr	super_comb = NULL;

    if( G_check_for_errors(onode, 1000) ) {
	Reset_TypeChecker();
	longjmp(*start_envp,1);
    }

    /* Add context for non-lazy functions and implicit arguments */
    g_ptr node = Add_non_lazy_context_and_find_Userdefs(onode, stbl);
    if( node == NULL ) {
	Reset_TypeChecker();
	longjmp(*start_envp,1);
    }

    // Get all overloaded functions
    impl_arg_ptr implicit_args;
    /* Typecheck */
    if( type == NULL )
	type = TypeCheck(&node, delayed, &implicit_args);

    if( type == NULL ) {
	/* Failed type-checking */
	longjmp(*start_envp,1);
    }

    result_ptr rp = (result_ptr) new_rec(&result_rec_mgr);
    rp->expr_init =  cephalopode_mode? Reflect_expr(node) : NULL;

#ifdef CHECK_REF_CNTS
    check_ref_cnts(node);
#endif

    /* Optimize the graph to ease compilaton */
    node = Optimise(node);
#ifdef CHECK_REF_CNTS
    check_ref_cnts(node);
#endif
    new_mgr(&free_rec_mgr, sizeof(free_list_rec));
    if( is_large(node) < LARGE_CUT_OFF ) {
	create_hash(&free_tbl, 100, ptr_hash, ptr_equ);
	create_free_tbl(node);
	node = compile_rec(node);
	dispose_hash(&free_tbl, NULLFCN);
    } else {
	rt = compile(node, NULL);
	node = rt.res;
    }
    free_mgr(&free_rec_mgr);
#ifdef CHECK_REF_CNTS
    check_ref_cnts(node);
#endif
    rp->expr_comb = cephalopode_mode? Reflect_expr(node) : NULL;

    /* Link in used definitions */
    node = Replace_name(node, stbl);

#ifdef CHECK_REF_CNTS
    check_ref_cnts(node);
#endif
    SET_REFCNT(node,MAX_REF_CNT);	/* Make sure it never gets freed */
    /* Do GC if needed */
    root_node = node;
    if( do_gc_asap ) {
	Garbage_collect();
    }
    rp->expr = node;
    rp->super_comb = super_comb;
    rp->type = type;
    rp->implicit_args = implicit_args;
    return( rp );
}

g_ptr
Execute_fl_code(const string function, ...)
{
    va_list args;
    va_start(args, function);
    
    fn_ptr fp = Find_Function_Def(symb_tbl, function);
    if( fp == NULL ) {
	g_ptr res = Get_node();
	string msg = strtemp("Cannot find function ");
	msg = strappend(function);
	MAKE_REDEX_FAILURE(res, msg);
	return res;
    }
    if( fp->overload ) {
	g_ptr res = Get_node();
	string msg = strtemp("Execute_fl_code cannot use overloaded function ");
	msg = strappend(function);
	MAKE_REDEX_FAILURE(res, msg);
	return res;
    }
    g_ptr expr = fp->expr;
    g_ptr g_arg;
    while( (g_arg = va_arg(args, g_ptr)) != NULL ) {
	expr = Make_APPL_ND(expr, g_arg);
    }
    result_ptr rp = (result_ptr) new_rec(&result_rec_mgr);
    rp->expr = expr;
    rp->type = NULL;
    Print_Result(rp, stdout_fp, FALSE);
    expr = rp->expr;
    Free_result_ptr(rp);
    va_end(args);
    return expr;
}

g_ptr
Make_TYPE(string constr_name, int arg_cnt)
{
    g_ptr res = mk_constr_nd(constr_name);
    res = Add_args(arg_cnt, type_constr_rec(1, arg_cnt, res));
    return( res );
}


cl_ptr
Get_CL_node()
{
    cl_ptr ret;
    ret = (cl_ptr) new_rec(&cl_rec_mgr);
    ret->expr = NULL;
    ret->next = NULL;
    return( ret );
}

g_ptr
Convert_CL(cl_ptr nd, bool list)
{
    cl_ptr start;

    if( list ) {
	/* Append a trailing NIL */
	cl_ptr last = Get_CL_node();
	last->expr = Make_NIL();
	last->next = nd->next;
	last->op   = no_tp;
	nd->next   = last;
	nd = last;
	/* Change combination operator to the list builder */
	for(start = nd->next; start != nd; start = start->next) {
	    start->op = mk_list_tp;
	}
    }
    /* nd points to last element of list */
    /* Cut the list and traverse it 	 */
    start = nd->next;
    nd->next = NULL;
    return( convert_CL_rec(start) );
}


g_ptr
List_reverse(g_ptr cur)
{
    g_ptr first, prev, next;

    if( GET_TYPE(cur) != APPLY_ND )
	return cur;

    first = cur;
    prev = NULL;
    while( GET_TYPE(cur) == APPLY_ND ) {
	next = GET_APPLY_RIGHT(cur);
	SET_APPLY_RIGHT(cur, prev);
	prev = cur;
	cur = next;
    }
    SET_APPLY_RIGHT(first, next);
    return(prev);
}

int
List_length(g_ptr l)
{
    int res = 0;
    while( !IS_NIL(l) ) {
        res++;
        l = GET_CONS_TL(l);
    }
    return res;
}

g_ptr
List_element(g_ptr l, unint index)
{
    while(!IS_NIL(l)) {
        if(index == 0) {
            return GET_CONS_HD(l);
        }
        index--;
        l = GET_CONS_TL(l);
    }
    return NULL;
}

var_list_ptr
Add_Var_to_Var_List(string var, typeExp_ptr type_hint, var_list_ptr vlp)
{
    var_list_ptr new;
    new = (var_list_ptr) new_rec(&var_list_rec_mgr);
    new->next = vlp;
    new->name = var;
    new->type_hint = type_hint;
    return( new );
}

static void
mark_pp_fns(pointer key, pointer data)
{
    g_ptr node;
    (void) key;
    node = (g_ptr) data;
    Mark(node);
    SET_REFCNT(node,MAX_REF_CNT);
}

void
Mark(g_ptr node)
{

    if( node == NULL ) 
	return;
    if( GET_MARK(node) == 1 ) {
	SET_REFCNT(node,MAX_REF_CNT);
	return;
    }

    SET_MARK(node, 1);
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    Mark(GET_APPLY_LEFT(node));
	    Mark(GET_APPLY_RIGHT(node));
	    break;
	case LAMBDA_ND:
	    Mark(GET_LAMBDA_VAR_LEAF(node));
	    Mark(GET_LAMBDA_BODY(node));
	    break;
	case CONS_ND:
	    Mark(GET_CONS_HD(node));
	    Mark(GET_CONS_TL(node));
	    break;
	case LEAF:
	    switch( GET_LEAF_TYPE(node) ) {
		case BOOL:
		    B_Mark(GET_BOOL(node) );
		    break;
		case BEXPR:
		    BE_Mark(GET_BEXPR(node) );
		    break;
		case INT:
		    Arbi_mark(GET_AINT(node));
		    break;
		case PRIM_FN:
		    {
			if( GET_PRIM_FN(node) == P_CACHE )
			    Mark_G_Cache(GET_CACHE_TBL(node));
			else if( GET_PRIM_FN(node) == P_REF_VAR )
			    Mark_RefVar(GET_REF_VAR(node));
			else if( GET_PRIM_FN(node) == P_FSM_OBJECT )
			    DIE("Obsolete fsm object created???");
		    }
		    break;
		case EXT_OBJ:
		    {
			unint class = GET_EXT_OBJ_CLASS(node);
			ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
			op->mark_fn(GET_EXT_OBJ(node));
			break;
		    }
		default:
		    break;
	    }
	    break;
	default:
	    DIE("Cannot happen?");
    }
}

void
#if TRACK_FREEING
Do_garbage_collect(string file, int line)
#else
Do_garbage_collect()
#endif
{
    g_ptr	nd, *ndp;

#if TRACK_FREEING
    garbage_collection_started_file = file;
    garbage_collection_started_line = line;
#endif

    if( Disable_GC ) return;
    if( RCverbose_GC )
	FP(fl_gc_fp, "Start garbage collection.");

    /* Clear all reference counts and marks */
    FOR_REC(&g_rec_mgr, g_ptr, nd) {
	SET_REFCNT(nd, 0);
    }
    Reset_Ref_cnts();

    /* Mark all visible */
    Mark(void_nd);
    SET_REFCNT(void_nd, MAX_REF_CNT);
    B_Mark(cur_eval_cond);
    Mark(root_node);
    Mark(print_nd);
    Mark(return_trace_list);
    Mark_tcl_callbacks();
    FOR_BUF(&global_gc_buf, g_ptr, ndp) {
	Mark(*ndp);
    }
    Mark_symbols();
    scan_hash(&pretty_printer_tbl, mark_pp_fns);
    scan_hash(&gc_protect_tbl, mark_pp_fns);
    // Clean hidden state in list code
    List_GC();

    /* Finally clean out unused nodes */
    Sweep_G_caches();
    Sweep_ext_objs();
    sweep();
    Arbi_sweep();
    B_Clean();
    BE_Clean();
    do_gc_asap = FALSE;
    if( RCverbose_GC )
	FP(fl_gc_fp, "done\n");
}

void
Debug_change_string(char *new_name, g_ptr n)
{
    SET_STRING(n, new_name);
}

void
Print_leaf(g_ptr node, odests fp)
{
    switch( GET_LEAF_TYPE(node) ) {
	case INT:
	    FP(fp, "%s", Arbi_ToString(GET_AINT(node),10));
	    break;
	case STRING:
	    FP(fp, "\"%s\"", GET_STRING(node));
	    break;
	case BOOL:
	    B_Print(fp, GET_BOOL(node), -1);
	    break;
	case BEXPR:
#if COMPACT_BEXPR_PRINT
	    {
		bexpr f = GET_BEXPR(node);
		if( f == BE_One() ) {
		    FP(fp, "bT");
		} else
		if( f == BE_Zero() ) {
		    FP(fp, "bF");
		} else {
		    FP(fp, "bS");
		}
	    }
#else
	    BE_Print(fp, GET_BEXPR(node));
#endif
	    break;
	case PRIM_FN:
	    FP(fp, "%s", Get_pfn_name(node)); break;
	    break;
	case VAR:
	    FP(fp, "V_%s", GET_VAR(node));
	    break;
	case USERDEF: {
	    fn_ptr fn = GET_USERDEF(node);
	    FP(fp, "UD_%s", fn->name);
	    break;
	}
	case EXT_OBJ:
	    FP(fp, "EXT_OBJ %ld %p\n",
		    GET_EXT_OBJ_CLASS(node), GET_EXT_OBJ(node));
	    break;
	default:
	    DIE("Illegal leaf node type");
    }
}

bool
Can_Fail_Pat_Match(g_ptr node)
{
    /* This could be improved by checking whether the patterns
       cover all possible cases. However, this is easier       */
    if( node == NULL )
	return(FALSE);
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
	    if( Can_Fail_Pat_Match(GET_APPLY_LEFT(node)) )
		return( TRUE );
	    return( Can_Fail_Pat_Match(GET_APPLY_RIGHT(node)) );
        case LAMBDA_ND:
	    return( Can_Fail_Pat_Match(GET_LAMBDA_BODY(node)) );
        case CONS_ND:
	    if( Can_Fail_Pat_Match(GET_CONS_HD(node)) )
		return( TRUE );
	    return( Can_Fail_Pat_Match(GET_CONS_TL(node)) );
        case LEAF:
	    if( IS_PRIM_FN(node) && GET_PRIM_FN(node) == P_PFAIL )
		return( TRUE );
	    return(FALSE);
        default:
            DIE("Impossible");
    }
    return FALSE; // Dummy
}

int
GET_INT(g_ptr node)
{
    arbi_T ai;
    int    ret;
    ai = GET_AINT(node);
    ret = (int) (*Arbi_ToInt(ai));
    return ret;
}

void
SET_INT(g_ptr np, int i)
{
    arbi_T ai;
    ai = Arbi_FromInt(i);
    SET_AINT(np, ai);
}

g_ptr
Gen_map2(string parent_op, g_ptr (*fun2)(g_ptr,g_ptr),
	 g_ptr l, g_ptr r, bool read_only)
{
    gmap_info_rec ir;
    new_mgr(&(ir.gmap2_rec_mgr), sizeof(gmap2_rec));
    create_hash(&(ir.gen_map_tbl), 100,  gmap2_hash, gmap2_equ);
    ir.u.leaf_fun2 = fun2;
    ir.read_only = read_only;
    ir.parent_op = parent_op;
    g_ptr	res;
    if( read_only ) {
	if( !walk2_graph(&ir, l, r) ) {
	    res = Make_0inp_Primitive(P_FAIL);
	    SET_FAIL_STRING(res, wastrsave(&strings, FailBuf));
	} else {
	    res = NULL;
	}
    } else {
	res = gen_map2_rec(&ir, l, r);
    }
    dispose_hash(&(ir.gen_map_tbl), NULLFCN);
    free_mgr(&(ir.gmap2_rec_mgr));
    return res;
}

#define IS_NONE(np)	((((np->L) & 0x3b) == 0xb) && GET_STRING(np) == s_NONE)

static bool
walk2_graph(gmap_info_ptr ip, g_ptr l, g_ptr r)
{
  restart_walk2:
    if( l == NULL ) {
	if( r == NULL )
	    return TRUE;
	else
	    goto fail_walk2;
    }
    if( r == NULL )
	goto fail_walk2;
    // Deal with NONE's
    if( IS_NONE(l) ) { l = r; } else if( IS_NONE(r) ) { r = l; }
    // 
    if( GET_TYPE(l) != GET_TYPE(r) ) goto fail_walk2;
    gmap2_rec tmp;
    tmp.l = l;
    tmp.r = r;
    if( find_hash(&(ip->gen_map_tbl), (pointer) &tmp) != NULL ) {
	return TRUE;
    }
    gmap2_ptr pair = (gmap2_ptr) new_rec(&(ip->gmap2_rec_mgr));
    pair->l = l;
    pair->r = r;
    switch( GET_TYPE(l) ) {
	case APPLY_ND:
	    insert_hash(&(ip->gen_map_tbl), (pointer) pair, (pointer) pair);
	    if( !walk2_graph(ip, GET_APPLY_LEFT(l), GET_APPLY_LEFT(r)) )
		goto fail_walk2;
	    if(!walk2_graph(ip, GET_APPLY_RIGHT(l), GET_APPLY_RIGHT(r)))
		goto fail_walk2;
	    return TRUE;
	case CONS_ND:
	    insert_hash(&(ip->gen_map_tbl), (pointer) pair, (pointer) pair);
	    if(!walk2_graph(ip, GET_CONS_HD(l), GET_CONS_HD(r) ) )
		goto fail_walk2;
	    if( !walk2_graph(ip, GET_CONS_TL(l), GET_CONS_TL(r)) )
		goto fail_walk2;
	    return TRUE;
	case LEAF:
	    if( GET_LEAF_TYPE(l) != GET_LEAF_TYPE(r) ) goto fail_walk2;
	    if( IS_REF_VAR(l) ) {
		if( IS_REF_VAR(r) ) {
		    int lref_var = GET_REF_VAR(l);
		    l = Get_RefVar(lref_var);
		    int rref_var = GET_REF_VAR(r);
		    r = Get_RefVar(rref_var);
		    goto restart_walk2;
		} else
		    goto fail_walk2;
	    } else {
		if( IS_REF_VAR(r) ) goto fail_walk2;
	    }
	    if( IS_EXT_OBJ(l) ) {
		unint class = GET_EXT_OBJ_CLASS(l);
		ASSERT( GET_EXT_OBJ_CLASS(r) == class );
		ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
		if( op->gmap2_fn != NULL ) {
		    g_ptr o_l = GET_EXT_OBJ(l);
		    g_ptr o_r = GET_EXT_OBJ(r);
		    op->gmap2_fn(ip, o_l, o_r);
		} else {
		    ip->u.leaf_fun2(l, r);
		}
	    } else {
		ip->u.leaf_fun2(l, r);
	    }
	    insert_hash(&(ip->gen_map_tbl), (pointer) pair, (pointer) pair);
	    return TRUE;
	default:
	    DIE("Should never happen");
    }
  fail_walk2:
    Fail_pr("Structural mismatch in %s", ip->parent_op);
    return FALSE;
}


static string
type2str(int type)
{
    switch (type ) {
	case LAMBDA_ND: return(s_LAMBDA);
	case APPLY_ND: return(s_APPLY);
	case CONS_ND: return(s_CONS);
	case LEAF: return(s_LEAF);
	default: DIE("Illegal type");
    }
}

static string
leaftype2str(int leaftype)
{
    switch (leaftype ) {
	case INT: return s_INT;
	case STRING: return s_STRING;
	case BOOL: return s_BOOL;
	case BEXPR: return s_BEXPR;
	case EXT_OBJ: return s_EXT_OBJ;
	case PRIM_FN: return s_PRIM_FN;
	case VAR: return s_VAR;
	case USERDEF: return s_USERDEF;
	default: DIE("Illegal type");
    }
}

#if 0
#define FAIL_GM2(l,r,msg) {						    \
			res = Make_0inp_Primitive(P_FAIL);		    \
   SET_FAIL_STRING(res,Fail_pr("Structural mismatch in %s\n(%s)\n", ip->parent_op, (msg)));\
   GRl(l); GRl(r);							    \
			return res;					    \
		      }
#else
#define FAIL_GM2(l,r,msg) {						    \
			res = Make_0inp_Primitive(P_FAIL);		    \
   SET_FAIL_STRING(res,Fail_pr("Structural mismatch in %s\n", ip->parent_op));\
			return res;					    \
		      }
#endif

static g_ptr
gen_map2_rec(gmap_info_ptr ip, g_ptr l, g_ptr r)
{
    g_ptr res;
    if( l == NULL ) {
	if( r == NULL )
	    return NULL;
	else {
	    FAIL_GM2(l,r,"l = NULL but r != NULL");
	}
    }
    if( r == NULL ) {
	FAIL_GM2(l,r,"l != NULL but r = NULL");
    }
    // Deal with NONEs
    if( IS_NONE(l) ) { l = r; } else if( IS_NONE(r) ) { r = l; }
    //
    if( GET_TYPE(l) != GET_TYPE(r) ) {
	char msg[100];
	Sprintf(msg, "TYPE(l)=%s but TYPE(r)=%s", type2str(GET_TYPE(l)),
						  type2str(GET_TYPE(r)));
	FAIL_GM2(l,r,msg);
    }
    if( IS_LEAF(l) && (GET_LEAF_TYPE(l) != GET_LEAF_TYPE(r)) ) {
	char msg[100];
	Sprintf(msg, "LEAFTYPE(l)=%s but LEAFTYPE(r)=%s",
		     leaftype2str(GET_LEAF_TYPE(l)),
		     leaftype2str(GET_LEAF_TYPE(r)));
	FAIL_GM2(l,r,msg);
    }
    // Was this combination of l and r already computed
    gmap2_rec tmp;
    tmp.l = l;
    tmp.r = r;
    if( (res = find_hash(&(ip->gen_map_tbl), (pointer) &tmp)) != NULL ) {
	INC_REFCNT(res);
	return( res );
    }
    // Insert place holder for results to deal with loops
    gmap2_ptr pair = (gmap2_ptr) new_rec(&(ip->gmap2_rec_mgr));
    pair->l = l;
    pair->r = r;
    switch( GET_TYPE(l) ) {
	case APPLY_ND:
	    {
		res = Make_APPL_ND(NULL, NULL);
		PUSH_GLOBAL_GC(res);
		insert_hash(&(ip->gen_map_tbl), (pointer) pair, (pointer) res);
		g_ptr left_of_l = GET_APPLY_LEFT(l);
		g_ptr left_of_r = GET_APPLY_LEFT(r);
		g_ptr new_left = gen_map2_rec(ip, left_of_l, left_of_r);
		if( IS_FAILURE(new_left) ) {
		    POP_GLOBAL_GC(1);
		    return new_left;
		}
		PUSH_GLOBAL_GC(new_left);
		g_ptr right_of_l = GET_APPLY_RIGHT(l);
		g_ptr right_of_r = GET_APPLY_RIGHT(r);
		g_ptr new_right = gen_map2_rec(ip, right_of_l,right_of_r);
		POP_GLOBAL_GC(2);
		if( IS_FAILURE(new_right) ) return new_right;
		if( new_left == left_of_l && new_right == right_of_l ) {
		    delete_hash(&(ip->gen_map_tbl), (pointer) pair);
		    insert_hash(&(ip->gen_map_tbl),(pointer) pair,(pointer) l);
		    INC_REFCNT(l);
		    SET_APPLY_LEFT(res,NULL);
		    SET_APPLY_RIGHT(res,NULL);
		    DEC_REF_CNT(res);
		    return( l );
		}
		if( new_left == left_of_r && new_right == right_of_r ) {
		    delete_hash(&(ip->gen_map_tbl), (pointer) pair);
		    insert_hash(&(ip->gen_map_tbl),(pointer) pair,(pointer) r);
		    INC_REFCNT(r);
		    SET_APPLY_LEFT(res,NULL);
		    SET_APPLY_RIGHT(res,NULL);
		    DEC_REF_CNT(res);
		    return( r );
		}
		SET_APPLY_LEFT(res, new_left);
		SET_APPLY_RIGHT(res, new_right);
		return( res );
	    }
	case CONS_ND:
	    {
		if( IS_NIL(l) ) {
		    if( IS_NIL(r) ) {
			INC_REFCNT(l);
			return( l );
		    }
		    FAIL_GM2(l,r,"IS_NIL(l) but IS_CONS(r)");
		}
		if( IS_NIL(r) ) {
		    FAIL_GM2(l,r,"IS_CONS(l) but IS_NIL(r)");
		}
		res = Make_CONS_ND(NULL, NULL);
		PUSH_GLOBAL_GC(res);
		insert_hash(&(ip->gen_map_tbl), (pointer) pair, (pointer) res);
		g_ptr hd_of_l = GET_CONS_HD(l);
		g_ptr hd_of_r = GET_CONS_HD(r);
		g_ptr new_hd = gen_map2_rec(ip, hd_of_l, hd_of_r);
		if( IS_FAILURE(new_hd) ) {
		    POP_GLOBAL_GC(1);
		    return new_hd;
		}
		PUSH_GLOBAL_GC(new_hd);
		g_ptr tl_of_l = GET_CONS_TL(l);
		g_ptr tl_of_r = GET_CONS_TL(r);
		g_ptr new_tl = gen_map2_rec(ip, tl_of_l,tl_of_r);
		POP_GLOBAL_GC(2);
		if( IS_FAILURE(new_tl) ) { return new_tl; }
		if( new_hd == hd_of_l && new_tl == tl_of_l ) {
		    delete_hash(&(ip->gen_map_tbl), (pointer) pair);
		    insert_hash(&(ip->gen_map_tbl),(pointer) pair,(pointer) l);
		    INC_REFCNT(l);
		    SET_CONS_HD(res,NULL);
		    SET_CONS_TL(res,NULL);
		    DEC_REF_CNT(res);
		    return( l );
		}
		if( new_hd == hd_of_r && new_tl == tl_of_r ) {
		    delete_hash(&(ip->gen_map_tbl), (pointer) pair);
		    insert_hash(&(ip->gen_map_tbl),(pointer) pair,(pointer) r);
		    INC_REFCNT(r);
		    SET_CONS_HD(res,NULL);
		    SET_CONS_TL(res,NULL);
		    DEC_REF_CNT(res);
		    return( r );
		}
		SET_CONS_HD(res, new_hd);
		SET_CONS_TL(res, new_tl);
		return( res );
	    }
	case LEAF:
	    {
		if( IS_REF_VAR(l) ) {
		    if( IS_REF_VAR(r) ) {
			int lref_var = GET_REF_VAR(l);
			int rref_var = GET_REF_VAR(r);
			if( lref_var != rref_var ) {
			    res = Make_0inp_Primitive(P_FAIL);
			    SET_FAIL_STRING(res,
				Fail_pr("Different ref vars in gen_map2"));
			    return res;
			}
			g_ptr ll = Get_RefVar(lref_var);
			g_ptr value = gen_map2_rec(ip, ll, ll);
			if( IS_FAILURE(value) ) return value;
			if( value == ll ) {
			    INC_REFCNT(l);
			    return l;
			}
                        Set_RefVar(lref_var,value);
                        INC_REFCNT(value);
			res = l;
			return res;
		    } else {
			FAIL_GM2(l,r,"IS_REF_VAR(l) but NOT IS_REF_VAR(r)");
		    }
		} else {
		    if( IS_REF_VAR(r) ) {
			FAIL_GM2(l,r,"IS_REF_VAR(r) but NOT IS_REF_VAR(l)");
		    }
		}
		if( IS_EXT_OBJ(l) ) {
		    unint class = GET_EXT_OBJ_CLASS(l);
		    ASSERT( GET_EXT_OBJ_CLASS(r) == class );
		    ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
		    if( op->gmap2_fn != NULL ) {
			pointer o_l = GET_EXT_OBJ(l);
			pointer o_r = GET_EXT_OBJ(r);
			pointer res = op->gmap2_fn(ip, o_l, o_r);
			if( o_l == res ) {
			    INC_REFCNT(l);
			    return l;
			}
			if( o_r == res ) {
			    INC_REFCNT(r);
			    return r;
			}
			return( Make_ext_obj(class, res) );
		    }
		}
		res = ip->u.leaf_fun2(l, r);
		if( res == l ) INC_REFCNT(l);
		if( res == r ) INC_REFCNT(r);
		insert_hash(&(ip->gen_map_tbl), (pointer) pair, (pointer) res);
		return res;
	    }
	default:
	    DIE("Should never happen");
    }
}


g_ptr
Gen_map(g_ptr (*fun)(g_ptr), g_ptr node, bool read_only)
{
    gmap_info_rec ir;
    create_hash(&(ir.gen_map_tbl), 100, ptr_hash, ptr_equ);
    ir.u.leaf_fun = fun;
    ir.read_only = read_only;
    ir.parent_op = NULL;
    g_ptr res;
    if( read_only ) {
	walk_graph(&ir, node);
	res = node;
    } else {
	res = gen_map_rec(&ir, node);
    }
    dispose_hash(&(ir.gen_map_tbl), NULLFCN);
    return res;
}

static void
walk_graph(gmap_info_ptr ip, g_ptr node)
{
    g_ptr res;
  restart_walk:
    if( node == NULL )
	return;
    if( (res = (g_ptr) find_hash(&(ip->gen_map_tbl),(pointer) node)) != NULL ) {
	return;
    }
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    insert_hash(&(ip->gen_map_tbl), (pointer) node, (pointer) node);
	    walk_graph(ip, GET_APPLY_LEFT(node));
	    walk_graph(ip, GET_APPLY_RIGHT(node));
	    return;
	case CONS_ND:
	    insert_hash(&(ip->gen_map_tbl), (pointer) node, (pointer) node);
	    walk_graph(ip, GET_CONS_HD(node));
	    walk_graph(ip, GET_CONS_TL(node));
	    return;
	case LEAF:
	    if( IS_PRIM_FN(node) && GET_PRIM_FN(node) == P_REF_VAR ) {
		int ref_var = GET_REF_VAR(node);
		node = Get_RefVar(ref_var);
		goto restart_walk;
	    }
	    if( IS_EXT_OBJ(node) ) {
		unint class = GET_EXT_OBJ_CLASS(node);
		ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
		if( op->gmap_fn != NULL ) {
		    op->gmap_fn(ip, GET_EXT_OBJ(node));
		    return;
		}
	    }
	    ip->u.leaf_fun(node);
	    insert_hash(&(ip->gen_map_tbl), (pointer) node, (pointer) node);
	    return;
	default:
	    DIE("Should never happen");
    }
}

static g_ptr
gen_map_rec(gmap_info_ptr ip, g_ptr node)
{
    g_ptr res;
    if( node == NULL )
	return( NULL );
    if( (res = (g_ptr) find_hash(&(ip->gen_map_tbl),(pointer) node)) != NULL ) {
	INC_REFCNT(res);
	return( res );
    }
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    {
		res = Make_APPL_ND(NULL, NULL);
		PUSH_GLOBAL_GC(res);
		insert_hash(&(ip->gen_map_tbl), (pointer) node, (pointer) res);
		g_ptr old_l = GET_APPLY_LEFT(node);
		g_ptr l = gen_map_rec(ip, old_l);
		SET_APPLY_LEFT(res, l);
		g_ptr old_r = GET_APPLY_RIGHT(node);
		g_ptr r = gen_map_rec(ip, old_r);
		SET_APPLY_RIGHT(res, r);
		POP_GLOBAL_GC(1);
		if( old_l == l && old_r == r ) {
		    delete_hash(&(ip->gen_map_tbl), (pointer) node);
		    insert_hash(&(ip->gen_map_tbl), (pointer) node,
						    (pointer) node);
		    INC_REFCNT(node);
		    SET_APPLY_LEFT(res,NULL);
		    SET_APPLY_RIGHT(res,NULL);
		    DEC_REF_CNT(res);
		    return( node );
		}
		return( res );
	    }
	case CONS_ND:
	    {
		res = Make_CONS_ND(NULL, NULL);
		PUSH_GLOBAL_GC(res);
		insert_hash(&(ip->gen_map_tbl), (pointer) node, (pointer) res);
		g_ptr old_l = GET_CONS_HD(node);
		g_ptr l = gen_map_rec(ip, old_l);
		SET_CONS_HD(res, l);
		g_ptr old_r = GET_CONS_TL(node);
		g_ptr r = gen_map_rec(ip, old_r);
		SET_CONS_TL(res, r);
		POP_GLOBAL_GC(1);
		if( old_l == l && old_r == r ) {
		    delete_hash(&(ip->gen_map_tbl), (pointer) node);
		    insert_hash(&(ip->gen_map_tbl), (pointer) node,
						    (pointer) node);
		    INC_REFCNT(node);
		    SET_CONS_HD(res,NULL);
		    SET_CONS_TL(res,NULL);
		    DEC_REF_CNT(res);
		    return( node );
		}
		return( res );
	    }
	case LEAF: {
		if( IS_PRIM_FN(node) && GET_PRIM_FN(node) == P_REF_VAR ) {
		    int ref_var = GET_REF_VAR(node);
		    node = Get_RefVar(ref_var);
		    g_ptr value = gen_map_rec(ip, node);
		    if( IS_FAILURE(value) ) return value;
		    ref_var = Make_RefVar();
		    Set_RefVar(ref_var,value);
		    INC_REFCNT(value);
		    res = Make_0inp_Primitive(P_REF_VAR);
		    SET_REF_VAR(res, ref_var);
		    insert_hash(&(ip->gen_map_tbl),(pointer)node,(pointer) res);
		    return res;
		}
		if( IS_EXT_OBJ(node) ) {
		    unint class = GET_EXT_OBJ_CLASS(node);
		    ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
		    pointer old = GET_EXT_OBJ(node);
		    if( op->gmap_fn != NULL ) {
			pointer res = op->gmap_fn(ip, old);
			if( old == res ) {
			    INC_REFCNT(node);
			    return node;
			}
			return( Make_ext_obj(class, res) );
		    }
		}
		g_ptr new_node = ip->u.leaf_fun(node);
		if( new_node == node ) {
		    insert_hash(&(ip->gen_map_tbl), (pointer) node,
						    (pointer) node);
		    INC_REFCNT(node);
		    return( node );
		}
		insert_hash(&(ip->gen_map_tbl), (pointer) node,
						(pointer) new_node);
		return( new_node );
	    }
	default:
	    DIE("Should never happen");
    }
    return NULL; // Dummy
}

string
Get_pfn_name(g_ptr np)
{
    int pfn = GET_PRIM_FN(np);
    switch ( pfn ) {
	case P_DEBUG:
	    Sprintf(pfn2str_buf, "<%s>", GET_DEBUG_STRING(np));
	    return pfn2str_buf;
	case P_PRINTF:
	    Sprintf(pfn2str_buf, " (printf \"%s\"\n)", GET_PRINTF_STRING(np));
	    return pfn2str_buf;
	case P_SPRINTF:
	    Sprintf(pfn2str_buf, " (Sprintf \"%s\"\n)", GET_PRINTF_STRING(np));
	    return pfn2str_buf;
	case P_EPRINTF:
	    Sprintf(pfn2str_buf, " (Eprintf \"%s\"\n)", GET_PRINTF_STRING(np));
	    return pfn2str_buf;
	case P_FPRINTF:
	    Sprintf(pfn2str_buf, " (fprintf \"%s\"\n)", GET_PRINTF_STRING(np));
	    return pfn2str_buf;
	case P_SSCANF:
	    Sprintf(pfn2str_buf, " (sscanf \"%s\"\n)", GET_PRINTF_STRING(np));
	    return pfn2str_buf;
	default:
	    break;
    }
    int pfns = sizeof(pfn2name)/sizeof(struct prim_fun_name_rec);
    for(int i = 0; i < pfns; i++) {
	if( pfn2name[i].pfn == pfn ) {
	    return( pfn2name[i].name );
	}
    }
    Sprintf(pfn2str_buf, "Unknown pfn (%d)", pfn);
    return pfn2str_buf;
}

g_ptr
Get_node()
{
    g_ptr ret;
    if( free_list != NULL ) {
	ret = free_list;
	free_list = (g_ptr) GET_RIGHT(ret);
	RESET_G_PTR(ret);
	return( ret );
    }
    allocated++;
    if( allocated - last_allocated > gc_threshold )
	do_gc_asap = 1;
    ret = (g_ptr) new_rec(&g_rec_mgr);
    RESET_G_PTR(ret);
    ASSERT( ((ui) ret)%8 == 0 );
    return( ret );
}

static int
number_of_digits_needed(int n)
{
    if( n == 0 ) return 1;
    int cnt = 0;
    while( n != 0 ) {
	cnt++;
	n = n/10;
    }
    return( cnt );
}

g_ptr
Get_fl_stack_trace()
{
    string *namep;
    g_ptr res = Make_NIL();
    FOR_BUF(&fn_trace_buf, string, namep) {
	g_ptr nm = Make_STRING_leaf(wastrsave(&strings, *namep));
	res = Make_CONS_ND(nm, res);
    }
    return res;
} 

string
Get_stack_trace(int max_entries)
{
    char    ibuf[16];
    string *namep;
    string res;
    if( debug_on ) {
	tstr_ptr ts = new_temp_str_mgr();
	res = gen_strtemp(ts, "Stack trace:\n");
	int cnt = COUNT_BUF(&fn_trace_buf);
	int digits_needed = number_of_digits_needed(cnt);
	int idx = cnt;
	if( cnt < max_entries ) {
	    FUB_ROF(&fn_trace_buf, string, namep) {
		Sprintf(ibuf, "%*d:\t", digits_needed, idx--);
		res = gen_strappend(ts, ibuf);
		res = gen_strappend(ts, *namep);
		res = gen_charappend(ts, '\n');
	    }
	} else {
	    int i = 0;
	    int lower = max_entries/2;
	    int upper = cnt-max_entries/2;
	    bool done_middle = FALSE;
	    FUB_ROF(&fn_trace_buf, string, namep) {
		if( i < lower || i > upper ) {
		    Sprintf(ibuf, "%*d:\t", digits_needed, idx--);
		    res = gen_strappend(ts, ibuf);
		    res = gen_strappend(ts, *namep);
		    res = gen_charappend(ts, '\n');
		} else {
		    idx--;
		    if( !done_middle ) {
			res = gen_charappend(ts, '\n');
			res = gen_charappend(ts, '.');
			res = gen_charappend(ts, '.');
			res = gen_charappend(ts, '.');
			res = gen_charappend(ts, '\n');
			res = gen_charappend(ts, '\n');
			done_middle = TRUE;
		    }
		}
		i++;
	    }
	}
	res = wastrsave(&strings, res);
	free_temp_str_mgr(ts);
	return( res );
    } else {
	return( wastrsave(&strings, "") );
    }
}

g_ptr
Append_string_to_tail(g_ptr tail, string name)
{
    ASSERT(IS_CONS(tail));
    SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, name)));
    g_ptr n_tail = Make_NIL();
    SET_CONS_TL(tail, n_tail);
    return( n_tail );
}

void
Get_Vars(buffer *bp, g_ptr node)
{
    ASSERT( node != NULL );
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    Get_Vars(bp, GET_APPLY_LEFT(node));
	    Get_Vars(bp, GET_APPLY_RIGHT(node));
	    return;
	case CONS_ND:
	    return;
	case LEAF:
	    if( !IS_VAR(node) ) return;
	    if( find_hash(&Constructor_Table, (pointer) GET_VAR(node)) != NULL )
		return;
	    /* Treat : specially unless redefined */
	    if( is_CONS_fun(node) ) { return; }
	    /* Treat T and F specially unless redefined */
	    if( is_T_fun(node) || is_F_fun(node) ) { return; }
	    string name = GET_VAR(node);
	    push_buf(bp, &name);
	    return;
	default:
	   Rprintf("Illegal pattern in function definition\n");
    }
    DIE("Should never occur!");
}

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/

static g_ptr
mk_constr_nd(string constr_name)
{
    string name = Mk_constructor_name(constr_name);
    return( Make_STRING_leaf(name) );
}

static int
is_large(g_ptr node)
{
    int ret;
    if( node == NULL )
	return(0);
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    ret = is_large(GET_APPLY_LEFT(node));
	    if( ret >= LARGE_CUT_OFF )
		return( LARGE_CUT_OFF );
	    return( 1 + ret + is_large(GET_APPLY_RIGHT(node)) );
	case LEAF:
	    return(1);
	case LAMBDA_ND:
	    return( 1 + is_large(GET_LAMBDA_BODY(node)) );
	case CONS_ND:
	    ret = is_large(GET_CONS_HD(node));
	    if( ret > LARGE_CUT_OFF )
		return( LARGE_CUT_OFF );
	    return( 1 + ret + is_large(GET_CONS_HD(node)) );
	default:
	    DIE("Should not happen");
    }
    return 0; // Dummy
}


static void
refcnt_free_nd(g_ptr node)
{
#ifndef DONT_FREE_ND
    /* Normal (default) version */
    node->L = node->R = 0;
    SET_RIGHT(node, free_list);
    free_list = node;
    return;
#else
#if 0
    /* Version that resets the node, but don't put it on free list */
    RESET_G_PTR(node);
    return;
#endif
#if 1
    /* Version that sets the node to an illegal type but does not free it */
    SET_TYPE(node, LEAF);
    SET_LEAF_TYPE(node, PRIM_FN);
    SET_REFCNT(node, MAX_REF_CNT);
    SET_PRIM_FN(node, P_ILLEGAL);
    return;
#endif
#if 0
    /* Version that sets the node to an illegal type but does not free it */
    /* Also checks that no active graph node can access this node */
    set_up_hash();
    ASSERT(node != NULL);
    if( accessible(root_node, node) ) {
	FP(err_fp, "freeing a node still accessible\n");
    }
    remove_hash();
    SET_TYPE(node, LEAF);
    SET_LEAF_TYPE(node, PRIM_FN);
    SET_REFCNT(node, MAX_REF_CNT);
    SET_PRIM_FN(node, P_ILLEGAL);
    return;
#endif
#endif
}



static g_ptr
type_constr_rec(int cnt, int arg_cnt, g_ptr constr)
{
    string name;

    if( cnt > arg_cnt )
	return( constr );
    Sprintf(buf, "_Q_%d", cnt);
    name = wastrsave(&strings, buf);
    return(Make_2inp_Primitive(P_TUPLE,
			       type_constr_rec(cnt+1, arg_cnt, constr),
			       Make_VAR_leaf(name)));
}

static free_list_ptr
get_set(g_ptr node)
{
    return( (free_list_ptr) find_hash(&free_tbl, (pointer) node) );
}

static free_list_ptr
make_set(string var)
{
    free_list_ptr ret;
    ret = (free_list_ptr) new_rec(&free_rec_mgr);
    ret->var = var;
    ret->next = NULL;
    return(ret);
}

static free_list_ptr
set_union(free_list_ptr s1, free_list_ptr s2)
{
    free_list_ptr 	ret;
    int		res;

    if( s1 == NULL )
	return(s2);
    if( s2 == NULL )
	return(s1);
    ret = (free_list_ptr) new_rec(&free_rec_mgr);
    if( (res = strcmp(s1->var, s2->var)) < 0 ) {
	ret->var = s1->var;
	ret->next = set_union(s1->next, s2);
    } else if( res > 0 ) {
	ret->var = s2->var;
	ret->next = set_union(s1, s2->next);
    } else {
	ret->var = s2->var;
	ret->next = set_union(s1->next, s2->next);
    }
    return( ret );
}


static free_list_ptr
remove_element(free_list_ptr s, string var)
{
    free_list_ptr 	ret;

    if(s == NULL )
	return(s);
    if( STREQ(s->var, var) )
	return( s->next );
    ret = (free_list_ptr) new_rec(&free_rec_mgr);
    ret->var = s->var;
    ret->next = remove_element(s->next, var);
    return(ret);
}


static free_list_ptr
create_free_tbl(g_ptr node)
{
    free_list_ptr	ret;

    if( node == NULL ) 
	return( NULL );
    switch( GET_TYPE(node) ) {
	case LEAF:
		if( IS_VAR(node) ) {
		    ret = make_set(GET_VAR(node));
		    insert_hash(&free_tbl, (pointer) node, (pointer) ret);
		    return( ret );
		} else
		    return( NULL );
	case LAMBDA_ND:
		ret = remove_element(create_free_tbl(GET_LAMBDA_BODY(node)),
				     GET_LAMBDA_VAR(node));
		insert_hash(&free_tbl, (pointer) node, (pointer) ret);
		return( ret );
	case APPLY_ND:
		ret = set_union(create_free_tbl(GET_APPLY_LEFT(node)),
				create_free_tbl(GET_APPLY_RIGHT(node)));
		insert_hash(&free_tbl, (pointer) node, (pointer) ret);
		return( ret );
	case CONS_ND:
		ASSERT(GET_CONS_HD(node) == NULL && GET_CONS_TL(node) == NULL);
		return( NULL );
	default:
	    DIE("Should never occur!");
    }
    return NULL; // Dummy
}

static g_ptr
abstract_rec(string var, g_ptr node)
{
    g_ptr 	  e1, e2, cur, ret;
    free_list_ptr fp;

    if( node == NULL ) 
	return(node);
    if( !cached_is_free(var, node) ) {
	/* No free occurence of var below here */
	ret = Make_1inp_Primitive(P_K, node);
	insert_hash(&free_tbl, (pointer) ret, (pointer) get_set(node));
	return(ret);
    }
    switch( GET_TYPE(node) ) {
	case LEAF:
		/* Marked leaf */
		ASSERT( IS_VAR(node) && (STREQ(var, GET_VAR(node))) );
		e1 = Get_node();
		SET_TYPE(e1, LEAF);
		SET_LEAF_TYPE(e1, PRIM_FN);
		SET_PRIM_FN(e1, P_I);
		return( e1 );
	case APPLY_ND:
		/* Marked apply node */
		e1 = GET_APPLY_LEFT(node);
		e2 = GET_APPLY_RIGHT(node);
		if( !cached_is_free(var, e1) ) {
		    /* e1 would become K . . . */
		    /* Only e2 is marked */
		    ASSERT(cached_is_free(var, e2));
		    e2 = abstract_rec(var, e2);
		    if( is_I(e2) )
			return(e1);
		    if( GET_TYPE(e2) == APPLY_ND ) {
			g_ptr r = GET_APPLY_RIGHT(e2);
			cur = GET_APPLY_LEFT(e2);
			if( GET_TYPE(cur) == APPLY_ND ) {
			    g_ptr q = GET_APPLY_RIGHT(cur);
			    cur = GET_APPLY_LEFT(cur);
			    if( is_B(cur) ) {
				ret = Make_1inp_Primitive(P_BSTAR, e1);
				fp = get_set(e1);
				insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
				ret = Make_APPL_ND(ret, q);
				fp = set_union(fp, get_set(q));
				insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
				ret = Make_APPL_ND(ret, r);
				fp = set_union(fp, get_set(r));
				insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
				return(ret);
			    }
			}
		    }
		    ret = Make_1inp_Primitive(P_B, e1);
		    fp = get_set(e1);
		    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
		    ret = Make_APPL_ND(ret, e2);
		    fp = set_union(fp, get_set(e2));
		    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
		    return(ret);
		}
		/* e1 will not be K. . . */
		e1 = abstract_rec(var, e1);
		if( !cached_is_free(var, e2) ) {
		    /* e2 would be K. . . */
		    if( GET_TYPE(e1) == APPLY_ND ) {
			g_ptr q = GET_APPLY_RIGHT(e1);
			cur = GET_APPLY_LEFT(e1);
			if( GET_TYPE(cur) == APPLY_ND ) {
			    g_ptr p = GET_APPLY_RIGHT(cur);
			    cur = GET_APPLY_LEFT(cur);
			    if( is_B(cur) ) {
				ret = Make_1inp_Primitive(P_CPRIME, p);
				fp = get_set(p);
				insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
				ret = Make_APPL_ND(ret, q);
				fp = set_union(fp, get_set(q));
				insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
				ret = Make_APPL_ND(ret, e2);
				fp = set_union(fp, get_set(e2));
				insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
				return(ret);
			    }
			}
		    }
		    ret = Make_1inp_Primitive(P_C, e1);
		    fp = get_set(e1);
		    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
		    ret = Make_APPL_ND(ret, e2);
		    fp = set_union(fp, get_set(e2));
		    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
		    return(ret);
		}
		/* Neither e1 nor e2 would be K. . . */
		e2 = abstract_rec(var, e2);
		if( GET_TYPE(e1) == APPLY_ND ) {
		    g_ptr q = GET_APPLY_RIGHT(e1);
		    cur = GET_APPLY_LEFT(e1);
		    if( GET_TYPE(cur) == APPLY_ND ) {
			g_ptr p = GET_APPLY_RIGHT(cur);
			cur = GET_APPLY_LEFT(cur);
			if( is_B(cur) ) {
			    ret = Make_1inp_Primitive(P_SPRIME, p);
			    fp = get_set(p);
			    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
			    ret = Make_APPL_ND(ret, q);
			    fp = set_union(fp, get_set(q));
			    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
			    ret = Make_APPL_ND(ret, e2);
			    fp = set_union(fp, get_set(e2));
			    insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
			    return(ret);
			}
		    }
		}
		ret = Make_1inp_Primitive(P_S, e1);
		fp = get_set(e1);
		insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
		ret = Make_APPL_ND(ret, e2);
		fp = set_union(fp, get_set(e2));
		insert_hash(&free_tbl,(pointer)ret,(pointer)fp);
		return(ret);
	default:
	    DIE("Should never occur!");
    }
    DIE("Should never occur!"); return NULL; // Dummy
}


static bool
is_free(string var, g_ptr node)
{

    if( node == NULL ) 
	return(FALSE);
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    if( is_free(var, GET_APPLY_LEFT(node)) )
		return( TRUE );
	    return( is_free(var, GET_APPLY_RIGHT(node)) );
	case LAMBDA_ND:
	    if( STREQ(var, GET_LAMBDA_VAR(node)) )
		return( FALSE );
	    else
		return( is_free(var, GET_LAMBDA_BODY(node)) );
	case LEAF:
	    if( IS_VAR(node) && STREQ(var, GET_VAR(node)) )
		return( TRUE );
	    /* Fall through */
	default:
	    return( FALSE );
    }
}

static bool
cached_is_free(string var, g_ptr node)
{
    free_list_ptr res = get_set(node);
    while( res ) {
	if(STREQ(res->var, var))
	    return( TRUE );
	res = res->next;
    }
    return( FALSE );
}


static bool
is_I(g_ptr e)
{
    if( e == NULL )
	return( FALSE );
    else
	return( IS_LEAF(e) && IS_PRIM_FN(e) && (GET_PRIM_FN(e) == P_I));
}

static bool
is_B(g_ptr e)
{
    if( e == NULL )
	return( FALSE );
    else
	return( IS_LEAF(e) && IS_PRIM_FN(e) && (GET_PRIM_FN(e) == P_B));
}

#if DBG_TRACE_AND_SAVE     

static void
do_trace_dbg(int pfn, g_ptr root)
{
    force_cnt++;
    if(debug_id >= 0 && dbg_start_cmp && (force_cnt > debug_start_comparing)) {
	if(debug_id >= 1) {
	    if( pfn == P_EXTAPI_FN ) {
	    fprintf(stderr, "%d P_EXTAPI_FN %s\n", force_cnt,
		Get_ExtAPI_Function_Name(GET_EXTAPI_FN(root)));
	    } else {
		fprintf(stderr, "%d %s\n",
			force_cnt, Get_pfn_name(root));
	    }
	}
#ifdef DO_GRAPH_COMPARISON
	if( (force_cnt % 10) == 0 )
	    fprintf(stderr, "Checking # %d\n", force_cnt);
	if( !Compare_Graphs(debug_id, root_node) ) {
	    fprintf(stderr, "+++++++++ FAILURE +++++++++++++\n");
	    Dbg_stop();
	    exit(-1);
	}
#endif
    }
}
#define PRIM_DO_TRACE_DBG()	do_trace_dbg(pfn, root);
#else
#define PRIM_DO_TRACE_DBG()
#endif

#ifdef CHECK_REF_CNTS
#define DO_TRACE_DBG()	{ check_ref_cnts(root_node); PRIM_DO_TRACE_DBG() }
#else
#define DO_TRACE_DBG()	PRIM_DO_TRACE_DBG()
#endif


g_ptr
traverse_left(g_ptr oroot)
{
    ui      	pfn;
    string  	s;
    g_ptr   	ntmp;
    g_ptr	arg1 = NULL;
    g_ptr	arg2 = NULL;
    g_ptr	arg3 = NULL;
    g_ptr	arg4 = NULL;
    g_ptr	arg5 = NULL;
    g_ptr	redex = NULL;
    formula 	res;
    int		depth = 0;
    g_ptr	root = oroot;

    Call_level++;
    if( COUNT_BUF(&fn_trace_buf) >= (unint) RCcall_limit ) {
	Fail_pr("\n");
	Rprintf("Recursion limit reached (RECURSION-CALL-LIMIT=%d)\n",
		RCcall_limit);
    }
    if( root == NULL ) {
	goto ret;
    }
    if( IS_FORCED(root) ) {
	goto ret;
    }

  start:
#ifdef CHECK_REF_CNTS
    check_ref_cnts(root_node);
#endif
    if( root == NULL ) {
	ASSERT( depth == 0 );
	goto ret;
    }
    CHECK_FOR_INTERRUPT;
    if( do_gc_asap || Do_gc_asap ) {
	Garbage_collect();
    }
    switch( GET_TYPE(root) ) {
	case APPLY_ND:
	    while( GET_TYPE(root) == APPLY_ND ) {
                if( sp == stack )
                    DIE("Out of stack space.\n");
                sp--;
                *sp = root;
                depth++;
                root = GET_APPLY_LEFT(root);
            }
	    goto start;
	case CONS_ND:
	    if( depth != 0 ) {
		DIE("Should not happen!");
	    }
	    goto ret;
	case LEAF:
	    if( GET_LEAF_TYPE(root) != PRIM_FN ) {
		if( IS_VAR(root) )
		    DIE("Should not happen.");
		ASSERT( depth == 0 );
		goto ret;
	    };
	    switch( (pfn = GET_PRIM_FN(root)) ) {
		case P_S:
		    /*						*/
		    /*       @                    @		*/
		    /*      / \                  / \		*/
		    /*     @   X                /   \		*/
		    /*    / \       ==>        @     @		*/
		    /*   @   G                / \   / \		*/
		    /*  / \                  F   \ G   \	*/
		    /* S   F                      \____ X	*/
		    /*						*/
		    if( depth < 3 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    redex = *(sp+2);
		    SET_APPLY_LEFT(redex, Make_APPL_ND(arg1, arg3));
		    INC_REFCNT(arg1);
		    INC_REFCNT(arg3);
		    SET_APPLY_RIGHT(redex, Make_APPL_ND(arg2, arg3));
		    INC_REFCNT(arg2);
		    INC_REFCNT(arg3);
		    goto finish3;


		case P_SPRIME:
		    /*						*/
		    /*         @                  @__           */
		    /*	      /	\                /   \          */
		    /*       @   P              @     @         */
		    /*      / \      ==>       / \   / \ 	*/
		    /*     @   O              M   @ O   |       */
                    /*    / \                    / \    |       */
                    /*   @   N                  N   \   |       */
                    /*  / \                          \  |       */
                    /* S'  M                          \ |       */
                    /*                                 \|       */
		    /*                                  P       */
		    /*						*/
		    if( depth < 4 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    arg4  = GET_APPLY_RIGHT(*(sp+3));
		    redex = *(sp+3);
		    SET_APPLY_LEFT(redex, Make_APPL_ND(arg1,
						     Make_APPL_ND(arg2, arg4)));
		    INC_REFCNT(arg1);
		    INC_REFCNT(arg2);
		    INC_REFCNT(arg4);
		    SET_APPLY_RIGHT(redex, Make_APPL_ND(arg3, arg4));
		    INC_REFCNT(arg3);
		    INC_REFCNT(arg4);
		    goto finish4;

		case P_K:
		    /*						*/
		    /*         @         ==>      x             */
		    /*	      /	\                               */
		    /*       @   y                              */
		    /*      / \                                 */
		    /*     K   x                                */
		    /*						*/
		    /* Evalate before updating since K is a projection fn */
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg1 = traverse_left(arg1);
		    OVERWRITE(redex, arg1);
		    goto finish2;

		case P_NSEQ:
                    /* Dummy SEQ used to introduce dummy dependencies */
                    /* Simply return the second argument */
		    if( depth < 2 )
			goto clean_up;
		    /* Evalate before updating since NSEQ is a projection fn */
		    redex = *(sp+1);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg2 = traverse_left(arg2);
		    OVERWRITE(redex, arg2);
		    goto finish2;

		case P_SEQ:
		case P_FSEQ:
		    /* Evaluate arg1, throw away result, then return arg2 */
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
                    if( pfn == P_SEQ ) {
			push_trace_fn(s_seq);
                        arg1 = traverse_left(GET_APPLY_RIGHT(*sp));
                    } else {
			push_trace_fn(s_fseq);
                        arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
                    }
		    if( is_fail(arg1) ) {
			pop_trace_fn();
			goto arg1_fail2;
		    }
		    /* It now is an identity function for arg2 */
		    arg2 = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
		    OVERWRITE(redex, arg2);
		    pop_trace_fn();
		    goto finish2;

		case P_THEN:
		    /* Evaluate arg1, throw away result, then return */
		    /* (arg2 arg1) i.e., arg2 applied to arg1	     */
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
		    if( is_fail(arg1) ) { goto arg1_fail2; }
		    arg2 = GET_APPLY_RIGHT(*(sp+1));
		    SET_APPLY_LEFT(redex, arg2);
		    INC_REFCNT(arg2);
		    SET_APPLY_RIGHT(redex, arg1);
		    INC_REFCNT(arg1);
		    goto finish2;

		case P_DEBUG:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
#if TRACE_FUNCTION_CALLS
		    {
			int i;
			call_level++;
			for(i = 0; i < call_level; i ++) fprintf(stderr, " ");
			fprintf(stderr,"Entering %s\n", GET_DEBUG_STRING(root));
		    }
#endif
//fprintf(stderr, "DEBUG: ");
		    push_trace_fn(GET_DEBUG_STRING(root));
		    /* It now is an identity function for arg1 */
		    arg1 = traverse_left(GET_APPLY_RIGHT(*sp));
		    OVERWRITE(redex, arg1);
#if TRACE_FUNCTION_CALLS
		    {
			int i;
			for(i = 0; i < call_level; i ++) fprintf(stderr, " ");
			fprintf(stderr, "Leaving %s\n", GET_DEBUG_STRING(root));
			call_level--;
		    }
#endif
//fprintf(stderr, "DEBUG: ");
		    pop_trace_fn();
		    goto finish1;

		case P_CATCH:
		    /* Return E1 if E1 does not fail. Otherwise return E2 */
		    if( depth < 2 )
			goto clean_up;
		    push_trace_fn(s_catch);
		    redex = *(sp+1);
		    arg1 = GET_APPLY_RIGHT(*sp);
		    arg2 = GET_APPLY_RIGHT(*(sp+1));
                    arg1 = force(arg1, FALSE);
                    if( is_fail(arg1) ) {
                        /* Must return E2 */
                        arg2 = traverse_left(arg2);
                        OVERWRITE(redex, arg2);
                    } else {
                        /* Return E1 */
                        OVERWRITE(redex, arg1);
                    }
		    pop_trace_fn();
                    goto finish2;

                case P_GEN_CATCH:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1 = GET_APPLY_RIGHT(*sp);
		    arg2 = GET_APPLY_RIGHT(*(sp+1));
		    push_trace_fn(s_gen_catch);
                    arg1 = force(arg1, FALSE);
                    if( !is_fail(arg1) ) {
                        /* Return E1 */
                        OVERWRITE(redex, arg1);
			pop_trace_fn();
                        goto finish2;
                    } else {
                        string msg = GET_FAIL_STRING(arg1);
                        g_ptr res = Make_APPL_ND(arg2, Make_STRING_leaf(msg));
			INC_REFCNT(arg2);
                        push_buf(&global_gc_buf, (pointer) &res);
                        SET_REFCNT((g_ptr) res, MAX_REF_CNT); 
                        res = force(res, FALSE);
                        pop_buf(&global_gc_buf, (pointer) &res);
                        OVERWRITE(redex, res);
			pop_trace_fn();
                        goto finish2;
                    }

		case P_PCATCH:
		    /* Catch for pattern matching */
		    /* Return E1 if E1 does not fail. Otherwise return E2 */
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1 = traverse_left(GET_APPLY_RIGHT(*sp));
		    arg2 = GET_APPLY_RIGHT(*(sp+1));
		    if( is_pat_fail(arg1) ) {
			/* Must return E2 */
			arg2 = traverse_left(arg2);
			OVERWRITE(redex, arg2);
		    } else {
			/* Return E1 */
			OVERWRITE(redex, arg1);
		    }
		    goto finish2;

		case P_I:
		    /*						*/
		    /*         @         ==>      X             */
		    /*	      /	\                               */
		    /*       I   X                              */
		    /*						*/

		    /* Evalate before updating since I is a projection fn */
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg1 = traverse_left(arg1);
		    OVERWRITE(redex, arg1);
		    goto finish1;

		case P_C:
		    /*						*/
		    /*       @                    @		*/
		    /*      / \                  / \		*/
		    /*     @   y                @   x		*/
		    /*    / \       ==>        / \    		*/
		    /*   @   x                f   y    		*/
		    /*  / \                             	*/
		    /* C   f                             	*/
		    /*						*/
		    if( depth < 3 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    redex = *(sp+2);
		    SET_APPLY_LEFT(redex, Make_APPL_ND(arg1, arg3));
		    INC_REFCNT(arg1);
		    INC_REFCNT(arg3);
		    SET_APPLY_RIGHT(redex, arg2);
		    INC_REFCNT(arg2);
		    goto finish3;

		case P_CPRIME:
                    /*                                          */
                    /*         @                  @             */
                    /*        / \                / \            */
                    /*       @   P              @   O           */
                    /*      / \                / \              */
                    /*     @   O              M   @             */
                    /*    / \       ==>          / \            */
                    /*   @   N                  N   P           */
                    /*  / \                                     */
                    /* C'  M                                    */
                    /*                                          */
		    if( depth < 4 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    arg4  = GET_APPLY_RIGHT(*(sp+3));
		    redex = *(sp+3);
		    SET_APPLY_LEFT(redex, Make_APPL_ND(arg1,
						      Make_APPL_ND(arg2,arg4)));
		    INC_REFCNT(arg1);
		    INC_REFCNT(arg2);
		    INC_REFCNT(arg4);
		    SET_APPLY_RIGHT(redex, arg3);
		    INC_REFCNT(arg3);
		    goto finish4;

		case P_B:
		    /*						*/
		    /*       @                    @		*/
		    /*      / \                  / \		*/
		    /*     @   X                F   @		*/
		    /*    / \       ==>            / \		*/
		    /*   @   G                    G   X		*/
		    /*  / \                             	*/
		    /* B   F                             	*/
		    /*						*/
		    if( depth < 3 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    redex = *(sp+2);
		    SET_APPLY_LEFT(redex, arg1);
		    INC_REFCNT(arg1);
		    SET_APPLY_RIGHT(redex, Make_APPL_ND(arg2, arg3));
		    INC_REFCNT(arg2);
		    INC_REFCNT(arg3);
		    goto finish3;

		case P_BSTAR:
                    /*                                          */
                    /*         @                  @             */
                    /*        / \                / \            */
                    /*       @   x              c   @           */
                    /*      / \                    / \          */
                    /*     @   g                  f   @         */
                    /*    / \       ==>              / \        */
                    /*   @   f                      g   x       */
                    /*  / \                                     */
                    /* B*  c                                    */
                    /*                                          */
		    if( depth < 4 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    arg4  = GET_APPLY_RIGHT(*(sp+3));
		    redex = *(sp+3);
		    SET_APPLY_LEFT(redex, arg1);
                    INC_REFCNT(arg1);
		    SET_APPLY_RIGHT(redex, Make_APPL_ND(arg2,
						      Make_APPL_ND(arg3,arg4)));
                    INC_REFCNT(arg2);
                    INC_REFCNT(arg3);
                    INC_REFCNT(arg4);
		    goto finish4;

		case P_BPRIME:
                    /*                                          */
                    /*         @                  @             */
                    /*        / \                / \            */
                    /*       @   x              /   \           */
                    /*      / \                @     @          */
                    /*     @   g              / \   / \         */
                    /*    / \       ==>      c   f g   x        */
                    /*   @   f                                  */
                    /*  / \                                     */
                    /* B'  c                                    */
                    /*                                          */
		    if( depth < 4 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    arg4  = GET_APPLY_RIGHT(*(sp+3));
		    redex = *(sp+3);
		    SET_APPLY_LEFT(redex, Make_APPL_ND(arg1,arg2));
		    INC_REFCNT(arg1);
		    INC_REFCNT(arg2);
		    SET_APPLY_RIGHT(redex, Make_APPL_ND(arg3, arg4));
		    INC_REFCNT(arg3);
		    INC_REFCNT(arg4);
		    goto finish4;


		case P_Y:
                    /*                                          */
                    /*         @                  @<-+          */
                    /*        / \                / \ |          */
                    /*       Y   f              f   -+          */

		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    SET_APPLY_LEFT(redex, arg1);
		    INC_REFCNT(arg1);
		    SET_APPLY_RIGHT(redex, redex);
		    INC_REFCNT(redex);
		    goto finish1;

		case P_HEAD:
		    /* Evalate before updating since hd is a projection fn */
		    if( depth < 1 )
			goto clean_up;
		    arg1  = GET_APPLY_RIGHT(*sp);
		    push_trace_fn(s_hd);
		    redex = *sp;
		    if( !IS_CONS(arg1) ) {
			arg1 = traverse_left(arg1);
			if( is_fail(arg1) ) {
			    pop_trace_fn();
			    goto arg1_fail1;
			}
		    }
		    arg2 = GET_CONS_HD(arg1);
		    arg2 = traverse_left(arg2);
		    if( arg2 == NULL ) {
			Fail_pr("Cannot compute hd of the empty list");
			pop_trace_fn();
                        goto fail1;
                    }
		    OVERWRITE(redex, arg2);
		    pop_trace_fn();
		    goto finish1;

		case P_FST:
		    /* Evalate before updating since hd is a projection fn */
		    if( depth < 1 )
			goto clean_up;
		    push_trace_fn(s_fst);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    redex = *sp;
		    if( !IS_CONS(arg1) ) {
			arg1 = traverse_left(arg1);
			if( is_fail(arg1) ) {
			    pop_trace_fn();
			    goto arg1_fail1;
			}
		    }
		    arg2 = GET_CONS_HD(arg1);
		    arg2 = traverse_left(arg2);
		    if( arg2 == NULL ) {
			Fail_pr("Cannot compute fst of a pair???");
			pop_trace_fn();
                        goto fail1;
                    }
		    OVERWRITE(redex, arg2);
		    pop_trace_fn();
		    goto finish1;

		case P_TAIL:
		    /* Evalate before updating since tl is a projection fn */
		    if( depth < 1 )
			goto clean_up;
		    push_trace_fn(s_tl);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    redex = *sp;
		    if( !IS_CONS(arg1) ) {
			arg1 = traverse_left(arg1);
			if( is_fail(arg1) ) {
			    pop_trace_fn();
			    goto arg1_fail1;
			}
		    }
		    arg2 = GET_CONS_TL(arg1);
		    arg2 = traverse_left(arg2);
		    if( arg2 == NULL ) {
			Fail_pr("Cannot compute tl of the empty list");
			pop_trace_fn();
                        goto fail1;
                    }
                    OVERWRITE(redex, arg2);
		    pop_trace_fn();
                    goto finish1;

		case P_SND:
		    /* Evalate before updating since tl is a projection fn */
		    if( depth < 1 )
			goto clean_up;
		    push_trace_fn(s_snd);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    redex = *sp;
		    if( !IS_CONS(arg1) ) {
			arg1 = traverse_left(arg1);
			if( is_fail(arg1) ) {
			    pop_trace_fn();
			    goto arg1_fail1;
			}
		    }
		    arg2 = GET_CONS_TL(arg1);
		    arg2 = traverse_left(arg2);
		    /* Note that tl [] is acceptable in HOL! */
		    if( arg2 == NULL ) {
			Fail_pr("Cannot compute tl of the empty list");
			pop_trace_fn();
                        goto fail1;
                    }
                    OVERWRITE(redex, arg2);
		    pop_trace_fn();
                    goto finish1;

                case P_BDD_REORDER:
                    /* reorder nbr_of_times */
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
		    if( is_fail(arg1) )
			goto arg1_fail1;
		    Reorder(GET_INT(arg1));
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, BOOL);
		    SET_BOOL(redex, B_One());
                    goto finish1;

                case P_UNTYPE:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = GET_APPLY_RIGHT(*sp);
                    OVERWRITE(redex, void_nd);
                    goto finish1;

                case P_UNQUOTE:
		    DIE("UNQUOTE encountered at runtime");

                case P_EXIT:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
		    if( is_fail(arg1) )
			goto arg1_fail1;
		    Exit(GET_INT(arg1));
		    /* The next will never happen */
                    OVERWRITE(redex, void_nd);
                    goto finish1;

		case P_CONS:
		case P_TUPLE:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    SET_TYPE(redex, CONS_ND);
		    SET_CONS_HD(redex, arg1);
		    INC_REFCNT(arg1);
		    SET_CONS_TL(redex, arg2);
		    INC_REFCNT(arg2);
		    goto finish2;

		case P_STRICT_CONS:
		case P_STRICT_TUPLE:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
		    SET_TYPE(redex, CONS_ND);
		    SET_CONS_HD(redex, arg1);
		    INC_REFCNT(arg1);
		    SET_CONS_TL(redex, arg2);
		    INC_REFCNT(arg2);
		    goto finish2;

		case P_TIME:
		    {
			timer_rec timer;
			if( depth < 1 )
			    goto clean_up;
			Start_timer(&timer);
			redex = *sp;
			arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
			Stop_timer(&timer);
			if( is_fail(arg1) )
			    goto arg1_fail1;
			Sprintf(buf, "%d.%d", Get_seconds(&timer),
					      Get_microseconds(&timer)/100000);
			SET_TYPE(redex, CONS_ND);
			SET_CONS_HD(redex, arg1);
			INC_REFCNT(arg1);
			SET_CONS_TL(redex,
				    Make_STRING_leaf(wastrsave(&strings,buf)));
			goto finish1;
		    }

		case P_BDD_LOAD:
		    /* bdd_load string -> (bool list) list*/
		    {
			if( depth < 1 )
			    goto clean_up;
			redex = *(sp);
			arg1 = GET_APPLY_RIGHT(*sp);
			arg1 = force(arg1, FALSE);
			if( is_fail(arg1) ) goto arg1_fail1;
			buffer results;
			new_buf(&results, 100, sizeof(formula));
			if( !Load_BDDs(GET_STRING(arg1), &results) ) {
			    free_buf(&results);
			    goto fail1;
			}
			SET_TYPE(redex, CONS_ND);
			SET_CONS_HD(redex, NULL);
			SET_CONS_TL(redex, NULL);
			g_ptr tail = redex;
			formula *bp;
			FOR_BUF(&results, formula, bp) {
			    SET_CONS_HD(tail, Make_BOOL_leaf(*bp));
			    SET_CONS_TL(tail, Make_NIL());
			    tail = GET_CONS_TL(tail);
			}
			free_buf(&results);
			goto finish1;
		    }
		case P_UPDATE_VOSSRC:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1 = GET_APPLY_RIGHT(*sp);
		    arg2 = GET_APPLY_RIGHT(*(sp+1));
		    arg1 = force(arg1, FALSE);
		    if( is_fail(arg1) ) goto arg1_fail2;
		    arg2 = force(arg2, FALSE);
		    if( is_fail(arg2) ) goto arg2_fail2;
                    s = getRCvalue(GET_STRING(arg1));
		    if( *s == 0 ) {
			Fail_pr("No vossrc variable named %s",GET_STRING(arg1));
			goto fail2;
		    }
                    (void) updateRCvalue(GET_STRING(arg1),GET_STRING(arg2));
                    SET_TYPE(redex, LEAF);
                    SET_LEAF_TYPE(redex, STRING);
                    SET_STRING(redex, wastrsave(&strings, s));
		    goto finish2;

		case P_SUC:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, INT);
		    SET_AINT(redex, Arbi_add(GET_AINT(arg1), Arbi_FromInt(1)));
		    goto finish1;

		case P_CONSTR_EQ:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail2;
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail2;
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, BOOL);
		    if(GET_TYPE(arg1) != LEAF || GET_TYPE(arg2) != LEAF ||
		       GET_LEAF_TYPE(arg1) != STRING ||
		       GET_LEAF_TYPE(arg2) != STRING) {
			SET_BOOL(redex, B_Zero());
		    } else {
			if(STREQ(GET_STRING(arg1), GET_STRING(arg2)))
			    SET_BOOL(redex, B_One());
			else
			    SET_BOOL(redex, B_Zero());
		    }
		    goto finish2;

		case P_EQUAL:
		case P_IDENTICAL:
		case P_NOT_EQUAL:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail2;
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail2;
		    res = Is_equal(arg1, arg2, pfn == P_IDENTICAL);
		    if( pfn == P_NOT_EQUAL )
			res = B_Not(res);
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, BOOL);
		    SET_BOOL(redex, res);
		    goto finish2;

		case P_ERROR:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    Fail_pr(" %s\n", GET_STRING(arg1));
		    goto fail1;

		case P_EXPLODE:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    arg2  = do_explode(GET_STRING(arg1));
		    OVERWRITE(redex, arg2);
		    goto finish1;

		case P_IMPLODE:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = force(GET_APPLY_RIGHT(*sp), FALSE);
		    if( is_fail(arg1) )
			goto arg1_fail1;
		    s = strtemp("");
                    ntmp = arg1;
                    while( GET_CONS_HD(ntmp) != NULL ) {
                        s = strappend(GET_STRING(GET_CONS_HD(ntmp)));
                        ntmp = GET_CONS_TL(ntmp);
                    }
		    arg2 = Make_STRING_leaf(wastrsave(&strings, s));
		    OVERWRITE(redex, arg2);
		    goto finish1;

		case P_BDD_SAVE:
		    {
			if( depth < 2 )
			    goto clean_up;
			redex = *(sp+1);
			arg1  = force(GET_APPLY_RIGHT(*sp), FALSE);
			if( is_fail(arg1) )
			    goto arg1_fail2;
			arg2  = force(GET_APPLY_RIGHT(*(sp+1)), FALSE);
			if( is_fail(arg2) )
			    goto arg2_fail2;
			buffer roots;
			new_buf(&roots, 1000, sizeof(formula));
			ntmp = arg2;
			while( GET_CONS_HD(ntmp) != NULL ) {
			    formula b = GET_BOOL(GET_CONS_HD(ntmp));
			    push_buf(&roots, &b);
			    ntmp = GET_CONS_TL(ntmp);
			}
			Save_BDDs(GET_STRING(arg1), &roots);
			free_buf(&roots);
			MAKE_REDEX_BOOL(redex, B_One());
			goto finish2;
		    }
                case P_TCL_EVAL:
                    {
                        if( depth < 1 )
                            goto clean_up;
			if( !gui_mode ) {
			    Fail_pr("tcl_eval not available in -noX mode");
			    goto fail1;
			}
                        redex = *sp;
                        arg1  = force(GET_APPLY_RIGHT(*sp), FALSE);
                        if( is_fail(arg1) )
                            goto arg1_fail1;
                        g_ptr cur = arg1;
			tstr_ptr ts = new_temp_str_mgr();
                        string res = gen_strtemp(ts, "");
                        while( !IS_NIL(cur) ) {
                            string cmd = GET_STRING(GET_CONS_HD(cur));
                            if( !Send_to_tcl(cmd, &res) ) {
                                Fail_pr("Tcl failure: %s", res);
				free_temp_str_mgr(ts);
				goto fail1;
                            }
			    cur = GET_CONS_TL(cur);
                        }
                        SET_TYPE(redex, LEAF);
                        SET_LEAF_TYPE(redex, STRING);
			int len = strlen(res);
			if( *(res+len-1) == '\n' ) {
			    *(res+len-1) = 0;
			}
                        SET_STRING(redex, wastrsave(&strings, res));
			free_temp_str_mgr(ts);
                        goto finish1; 
                    }

		case P_VAR_ORDER:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = force(GET_APPLY_RIGHT(*sp), FALSE);
		    if( is_fail(arg1) )
			goto arg1_fail1;
		    New_ordering();
		    ntmp = arg1;
		    while( GET_CONS_HD(ntmp) != NULL ) {
			Add_ordering_var(GET_STRING(GET_CONS_HD(ntmp)));
			ntmp = GET_CONS_TL(ntmp);
		    }
		    Reorder(1);
		    arg5 = End_ordering();
		    OVERWRITE(redex, arg5);
                    goto finish1;

		case P_FAIL:
		    /* Rewind stack and return failure */
		    if( depth > 0 ) {
			arg1 = root;
			root = *(sp + (depth - 1));
			sp = sp+depth;
			OVERWRITE(root, arg1);
		    }
		    goto ret;

		case P_PFAIL:
		    if( depth < 1 )
			goto clean_up;
		    DIE("P_PFAIL should never be evaluated. Report error.");

		case P_PRINT:
		    if( depth < 1 ) goto clean_up;
                    ASSERT(depth == 1);
                    redex = *sp;
                    arg1 = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) ) { goto arg1_fail1; }
                    Print_line(GET_STRING(arg1), stdout_fp);
                    OVERWRITE(redex, void_nd);
                    goto finish1;

		case P_WRITE_TO_FILE:
		    if( depth < 2 )
			goto clean_up;
		    else {
			redex = *(sp+1);
			arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
			if( is_fail(arg1) ) goto arg1_fail2;
			arg2 = force(GET_APPLY_RIGHT(*(sp+1)), FALSE);
			if( is_fail(arg2) ) goto arg2_fail2;
			odests_fp = fopen(GET_STRING(arg1), "a");
			if( odests_fp == NULL ) {
			    Fail_pr("Cannot open file %s for writing\n",
				    GET_STRING(arg1));
			    goto fail2;
			}
			Print_line(GET_STRING(arg2), FILE_fp);
			fclose(odests_fp);
                        odests_fp = NULL;
			OVERWRITE(redex, void_nd);
			goto finish2;
		    }

		case P_LOAD:
		    if( depth < 2 ) goto clean_up;
		    redex = *(sp+1);
		    arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
		    if( is_fail(arg1) ) goto arg1_fail2;
		    arg2 = force(GET_APPLY_RIGHT(*(sp+1)), FALSE);
		    if( is_fail(arg2) ) goto arg2_fail2;
		    switch( Read_from_file(GET_STRING(arg1),
					   (GET_BOOL(arg2) == B_One()),
					   FALSE) ) {
			case 0:
			    MAKE_REDEX_BOOL(redex, B_One());
			    goto finish2;
			case 1:
			    Fail_pr("Cannot open file %s", GET_STRING(arg1));
			    goto fail2;
			case 2:
			    Fail_append("Loading file %s failed\n",
					GET_STRING(arg1));
			    goto fail2;
			default:
			    DIE("Should not happen");
		    }

		case P_BOOL2STR:
		    if( depth < 2 ) goto clean_up;
		    redex = *(sp+1);
		    arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
		    if( is_fail(arg1) ) goto arg1_fail2;
		    arg2 = force(GET_APPLY_RIGHT(*(sp+1)), FALSE);
		    if( is_fail(arg2) ) goto arg2_fail2;
		    res = GET_BOOL(arg2);
		    odests_fp = fmemopen(buf, 1000, "w");
		    if( odests_fp == NULL ) { DIE("Should never happen"); }
		    B_Print(FILE_fp, res, GET_INT(arg1));
		    fclose(odests_fp);
		    odests_fp = NULL;
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, STRING);
		    SET_STRING(redex, wastrsave(&strings,buf));
		    goto finish2;

		case P_EMPTY:
		case P_SYSTEM:
		case P_IS_CONS:
		case P_IS_TUPLE:
		case P_ORD:
		case P_CHR:
		case P_INT2STR:
		case P_EVAL:
		case P_GET_VOSSRC:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    switch( pfn ) {
			case P_GET_VOSSRC:
                            s = getRCvalue(GET_STRING(arg1));
			    SET_TYPE(redex, LEAF);
                            SET_LEAF_TYPE(redex, STRING);
                            SET_STRING(redex, wastrsave(&strings, s));
                            break;
			case P_ORD:
			    if( strlen(GET_STRING(arg1)) != 1 ) {
                                Fail_pr("ORD needs string of length 1");
				goto fail1;
			    }
			    SET_TYPE(redex, LEAF);
			    SET_LEAF_TYPE(redex, INT);
			    SET_INT(redex, *(GET_STRING(arg1)));
			    break;
                        case P_CHR:
                            if( GET_INT(arg1) < 0 || GET_INT(arg1) > 127 ) {
                                Fail_pr("Range error in CHR (%d)",
					GET_INT(arg1));
                                goto fail1;
                            }
			    SET_TYPE(redex, LEAF);
                            SET_LEAF_TYPE(redex, STRING);
			    buf[0] = GET_INT(arg1);
			    buf[1] = 0;
                            SET_STRING(redex, wastrsave(&strings, buf));
                            break;
			case P_EVAL:
			    if( perform_fl_command(GET_STRING(arg1)) ) {
				MAKE_REDEX_BOOL(redex, B_One());
			    } else {
				MAKE_REDEX_BOOL(redex, B_Zero());
			    }
			    break;
			case P_INT2STR:
			    Sprintf(buf, "%s",Arbi_ToString(GET_AINT(arg1),10));
			    SET_TYPE(redex, LEAF);
			    SET_LEAF_TYPE(redex, STRING);
			    SET_STRING(redex, wastrsave(&strings, buf));
			    break;
			case P_IS_CONS:
			    if( !IS_CONS(arg1) || GET_CONS_HD(arg1) == NULL ) {
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, BOOL);
				SET_BOOL(redex, B_Zero());
			    } else {
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, BOOL);
				SET_BOOL(redex, B_One());
			    }
			    break;
			case P_IS_TUPLE:
			    if( !IS_CONS(arg1) || GET_CONS_HD(arg1) == NULL ) {
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, BOOL);
				SET_BOOL(redex, B_Zero());
			    } else {
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, BOOL);
				SET_BOOL(redex, B_One());
			    }
			    break;
			case P_EMPTY:
			    if( !IS_CONS(arg1) || GET_CONS_HD(arg1) != NULL ) {
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, BOOL);
				SET_BOOL(redex, B_Zero());
			    } else {
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, BOOL);
				SET_BOOL(redex, B_One());
			    }
			    break;
			case P_SYSTEM:
			    {
				int ires;
				ires = system(GET_STRING(arg1));
				SET_TYPE(redex, LEAF);
				SET_LEAF_TYPE(redex, INT);
				SET_INT(redex, ires);
			    }
			    break;
		    }
		    goto finish1;

		case P_FORALL:
		case P_THEREIS:
		    /*						*/
		    /*         @    ==>        @ 		*/
		    /*	      /	\             / \               */
		    /*       @   s           @   \              */
		    /*      / \             / \   \             */
		    /*    !/?   e       QUANT  \   \		*/
		    /*			FORALL	\   \  	   	*/
		    /*		       (THEREIS) |   @  	*/
		    /*		       		 |  / \ 	*/
		    /*		       	         | e   \	*/
		    /*		       	         +------bs	*/
		    /* where bs == B_Var(s)			*/
		    /*		       	         		*/

		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = GET_APPLY_RIGHT(*sp);
		    arg2  = traverse_left(GET_APPLY_RIGHT(redex));
                    if( is_fail(arg2) )
                        goto arg2_fail2;
		    Sprintf(buf, "%%%s%d", GET_STRING(arg2), uniq_quant_cnt++);
		    arg3 = Make_BOOL_leaf(B_Var(wastrsave(&strings, buf)));
		    SET_APPLY_LEFT(redex, Make_1inp_Primitive(
						(pfn == P_FORALL)?
							P_QUANT_FORALL:
							P_QUANT_THEREIS,
						arg3));;
		    INC_REFCNT(arg3);
		    SET_APPLY_RIGHT(redex, Make_APPL_ND(arg1, arg3));
		    INC_REFCNT(arg1);
		    INC_REFCNT(arg3);
		    goto finish2;

		case P_QUANT_FORALL:
		case P_QUANT_THEREIS:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail2;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail2;
		    switch( pfn ) {
			case P_QUANT_FORALL:
			    res = B_forall(B_Depends(GET_BOOL(arg1)),
					   GET_BOOL(arg2));
			    break;
			case P_QUANT_THEREIS:
			    res = B_thereis(B_Depends(GET_BOOL(arg1)),
					    GET_BOOL(arg2));
			    break;
		    }
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, BOOL);
		    SET_BOOL(redex, res);
		    goto finish2;

		case P_RELPROD_THEREIS:
		case P_RELPROD_FORALL:
		    if( depth < 3 )
			goto clean_up;
		    redex = *(sp+2);
		    arg3  = traverse_left(GET_APPLY_RIGHT(*(sp+2)));
                    if( is_fail(arg3) )
                        goto arg3_fail3;
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail3;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail3;
		    Begin_RelProd();
		    res = Rel_prod( B_Depends(GET_BOOL(arg1)),
				    GET_BOOL(arg2),
				    GET_BOOL(arg3),
				    pfn == P_RELPROD_THEREIS );
		    End_RelProd();
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, BOOL);
		    SET_BOOL(redex, res);
		    goto finish3;

		case P_SAVE_GRAPH:
		    if( depth < 3 )
			goto clean_up;
		    redex = *(sp+2);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail3;
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail3;
		    // Do not force the graph to write out!
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    if( Save_graph(GET_STRING(arg1), GET_STRING(arg2), arg3) ) {
			MAKE_REDEX_BOOL(redex, B_One());
			goto finish3;
		    } else {
			goto fail3;
		    }

		case P_LOAD_GRAPH:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail2;
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail2;
		    // Do not force the graph to write out!
		    if( !Load_graph(GET_STRING(arg1), GET_STRING(arg2), redex) )
			goto fail2;
		    goto finish2;

		case P_COND:
		    if( depth < 3 )
			goto clean_up;
		    redex = *(sp+2);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail3;
		    arg2  = GET_APPLY_RIGHT(*(sp+1));
		    arg3  = GET_APPLY_RIGHT(*(sp+2));
		    if( !check_true_or_false(arg1) ) {
			/* Symbolic if-then-else */
			res = cur_eval_cond;
			PUSH_BDD_GC(res);
			if( B_And(res, GET_BOOL(arg1)) == B_Zero() ) {
			    // Effectively an F condition
			    cur_eval_cond = B_And(res, B_Not(GET_BOOL(arg1)));
			    /* Overwrite to remove access to arg2 for g.c. */
			    SET_APPLY_LEFT(redex, void_nd);
			    /* Allow gc to remove this entry */
			    DEC_REF_CNT(*(sp+1));
			    *(sp+1) = void_nd;
			    arg2 = void_nd;
			    arg3 = traverse_left(arg3);
			    OVERWRITE(redex, arg3);
			    cur_eval_cond = res;
			    POP_BDD_GC(1);
			    goto finish3;
			}
			if( B_And(res, B_Not(GET_BOOL(arg1))) == B_Zero() ) {
			    // Effectively a T condition
			    cur_eval_cond = B_And(res, GET_BOOL(arg1));
			    /* Overwrite to remove access to arg3 for g.c. */
			    SET_APPLY_RIGHT(redex, void_nd);
			    DEC_REF_CNT(arg3);
			    arg3 = void_nd;
			    arg2 = traverse_left(arg2);
			    OVERWRITE(redex, arg2);
			    cur_eval_cond = res;
			    POP_BDD_GC(1);
			    goto finish3;
			}
			// Actually both cases can occur
			cur_eval_cond = B_And(res, GET_BOOL(arg1));
			arg2  = force(arg2, FALSE);
			cur_eval_cond = res;
			if( is_fail(arg2) ) {
			    POP_BDD_GC(1);
			    goto arg2_fail3;
			}
			cur_eval_cond = B_And(res, B_Not(GET_BOOL(arg1)));
			arg3  = force(arg3, FALSE);
			cur_eval_cond = res;
			POP_BDD_GC(1);
			if( is_fail(arg3) ) {
			    goto arg3_fail3;
			}
			current_bdd_cond = GET_BOOL(arg1);
			g_ptr res = Gen_map2(s_ite, g_b_ite, arg2, arg3, FALSE);
			OVERWRITE(redex, res)
			goto finish3;
		    }
		    /* Scalar if-then-else */
		    /* Evaluate before updating since COND is a projection fn */
		    if( GET_BOOL(arg1) == B_One() ) {
			/* Overwrite to remove access to arg3 for g.c. */
			SET_APPLY_RIGHT(redex, void_nd);
			DEC_REF_CNT(arg3);
			arg3 = void_nd;
			arg2 = traverse_left(arg2);
			OVERWRITE(redex, arg2);
			goto finish3;
		    } else {
			/* Overwrite to remove access to arg2 for g.c. */
			SET_APPLY_LEFT(redex, void_nd);
			/* Allow gc to remove this entry */
			DEC_REF_CNT(*(sp+1));
			*(sp+1) = void_nd;
			arg2 = void_nd;
			arg3 = traverse_left(arg3);
			OVERWRITE(redex, arg3);
			goto finish3;
		    }


		case P_RVAR:
		    if( depth < 1 )
			goto clean_up;
#if DBG_TRACE_AND_SAVE
		    if( pfn == P_RVAR ) {
			dbg_start_cmp = TRUE;
			if(debug_id >= 1) {
			    fprintf(stderr, "==============================");
			    fprintf(stderr, "==============================\n");
			    fprintf(stderr, "==============================");
			    fprintf(stderr, "==============================\n");
			}
		    }
#endif
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    if( pfn == P_RVAR ) {
			SET_TYPE(redex, LEAF);
			SET_LEAF_TYPE(redex, BOOL);
			SET_BOOL(redex, Rand_Var(GET_STRING(arg1)));
		    } else {
			SET_TYPE(redex, LEAF);
			SET_LEAF_TYPE(redex, BOOL);
			SET_BOOL(redex, B_Var(GET_STRING(arg1)));
		    }
		    goto finish1;

		case P_CAT:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail2;
		    arg2  = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
                    if( is_fail(arg2) )
                        goto arg2_fail2;
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, STRING);
		    s = strtemp(GET_STRING(arg1));
		    s = strappend(GET_STRING(arg2));
		    SET_STRING(redex, wastrsave(&strings, s));
		    goto finish2;

		case P_HELP:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    s = Get_Help(GET_STRING(arg1));
		    if( s == NULL ) {
			Fail_pr("No function named %s defined",
				GET_STRING(arg1));
			goto fail1;
		    } else {
			SET_TYPE(redex, LEAF);
			SET_LEAF_TYPE(redex, STRING);
			SET_STRING(redex, s);
			goto finish1;
		    }

		case P_STRING_HD:
		case P_STRING_TL:
		    if( depth < 1 )
			goto clean_up;
		    redex = *sp;
		    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) )
                        goto arg1_fail1;
		    if( strlen(GET_STRING(arg1)) == 0 ) {
			Fail_pr("Cannot take %s of empty string", 
				(pfn = P_STRING_HD)? "hd" : "tl");
			goto fail1;
		    }
		    SET_TYPE(redex, LEAF);
		    SET_LEAF_TYPE(redex, STRING);
		    if( pfn == P_STRING_HD ) {
			char buf[2], *t;
			t = GET_STRING(arg1);
			buf[0] = *t;
			buf[1] = 0;
			SET_STRING(redex, wastrsave(&strings, buf));
		    } else {
			char *t;
			t = GET_STRING(arg1);
			SET_STRING(redex, wastrsave(&strings, t+1));
		    }
		    goto finish1;

		case P_CACHE:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    /* Force all arguments (note P_STRICT_TUPLE is used!) */
		    arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
		    if( is_fail(arg1) ) {
			goto fail2;
		    }
		    ASSERT( IS_CONS(arg1) );
		    arg3 = arg1;
		    while( !IS_NIL(arg3) ) {
			g_ptr res;
			ASSERT( IS_CONS(arg3) );
			res = GET_CONS_HD(arg3);
			if( is_fail(res) ) {
			    goto fail2;
			}
			arg3 = GET_CONS_TL(arg3);
		    }
#ifdef NO_CACHE_HIT
		    arg2 = NULL;
#else
		    /* Make sure contexts is part of the cache "arguments". */
		    INC_REFCNT(arg1);
		    arg1 =  Make_CONS_ND(Make_BOOL_leaf(cur_eval_cond), arg1);
		    arg2 = Find_in_g_cache(GET_CACHE_TBL(root), arg1);
#endif
		    if( arg2 != NULL ) {
			arg3 = GET_APPLY_RIGHT(*(sp+1));
			OVERWRITE(redex, arg2);
			arg2 = arg3;
		    } else {
			/* Must evaluate expression */
			PUSH_GLOBAL_GC(arg1);
			arg2 = traverse_left(GET_APPLY_RIGHT(*(sp+1)));
			OVERWRITE(redex, arg2);
			Insert_in_g_cache(GET_CACHE_TBL(root), arg1, redex);
			POP_GLOBAL_GC(1);
			INC_REFCNT(arg1);
			INC_REFCNT(redex);
		    }
		    goto finish2;
		case P_MK_REF_VAR:
		    if( depth < 1 )
			goto clean_up;
		    {
			int ref_var;
			redex = *sp;
			arg1 = force(GET_APPLY_RIGHT(*sp), FALSE);
			ref_var = Make_RefVar();
			Set_RefVar(ref_var,arg1);
			INC_REFCNT(arg1);
			SET_TYPE(redex, LEAF);
			SET_LEAF_TYPE(redex, PRIM_FN);
			SET_PRIM_FN(redex, P_REF_VAR);
			SET_REF_VAR(redex, ref_var);
			goto finish1;
		    }

		case P_DEREFERENCE:
		    if( depth < 1 )
			goto clean_up;
		    {
			int ref_var;
			redex = *sp;
			INC_REFCNT(*sp);
			arg1 = traverse_left(GET_APPLY_RIGHT(*sp));
			if( is_fail(arg1) ) goto arg1_fail1;
			ASSERT(IS_LEAF(arg1) && IS_PRIM_FN(arg1) &&
			       GET_PRIM_FN(arg1) == P_REF_VAR);
			ref_var = GET_REF_VAR(arg1);
			root = Get_RefVar(ref_var);
			INC_REFCNT(root);
                        OVERWRITE(redex, root);
                        goto finish1;
		    }

		case P_PRINTF:
		case P_SPRINTF:
		case P_EPRINTF:
		case P_FPRINTF:
                    {
                        if( Printf(&root, &sp, &depth) ) {
			    DO_TRACE_DBG();
                            goto start;
                        } else {
			    DO_TRACE_DBG();
                            goto clean_up;
			}
                    }
		case P_SSCANF:
		    {
                        if( Sscanf(&root, &sp, &depth) ) {
			    DO_TRACE_DBG();
                            goto start;
                        } else {
			    DO_TRACE_DBG();
                            goto clean_up;
			}
                    }
                case P_FOPEN:
                    { if( Fopen(&root, &sp, &depth) ) {
			    DO_TRACE_DBG();
                            goto start;
                        } else {
			    DO_TRACE_DBG();
                            goto clean_up;
			}
                    }

                case P_FFLUSH:
                    { if( Fflush(&root, &sp, &depth) ) {
			    DO_TRACE_DBG();
                            goto start;
                        } else {
			    DO_TRACE_DBG();
                            goto clean_up;
			}
                    }

                case P_FCLOSE:
                    { if( Fclose(&root, &sp, &depth) ) {
			    DO_TRACE_DBG();
                            goto start;
                        } else {
			    DO_TRACE_DBG();
                            goto clean_up;
			}
                    }

		case P_UPDATE_RVAR:
		    if( depth < 2 )
			goto clean_up;
		    redex = *(sp+1);
		    {
			int ref_var;
			ASSERT(depth == 2);
			arg1 = traverse_left(GET_APPLY_RIGHT(*sp));
			ASSERT(IS_REF_VAR(arg1));
			ref_var = GET_REF_VAR(arg1);
			arg2 = force(GET_APPLY_RIGHT(*(sp+1)), FALSE);
			if( cur_eval_cond != B_One() ) {
			    // Conditional assignment
			    // Make it unconditional but with value changed
			    // to be old value when cur_eval_cond if false
			    current_bdd_cond = cur_eval_cond;
			    arg3 = Get_RefVar(ref_var);
			    PUSH_GLOBAL_GC(arg3);
			    arg2 = Gen_map2(s_update_ref_var,
					    g_b_ite, arg2, arg3, FALSE);
			    POP_GLOBAL_GC(1);
			}
			Set_RefVar(ref_var, arg2);
			INC_REFCNT(arg2);
			OVERWRITE(redex, void_nd);
			goto finish2;
		    }
    
		case P_EXTAPI_FN:
		    {
			int id = GET_EXTAPI_FN(root);
			int acnt = Get_ExtAPI_ArgCnt(id);
			if( acnt == 0 ) {
			    redex = root;
			    Get_ExtAPI_Function(id)(redex);
			    DO_TRACE_DBG();
			    goto start;
			}
			redex = *(sp+acnt-1);
			if( depth < acnt ) goto clean_up;
			push_trace_fn(Get_ExtAPI_Function_Name(id));
			s = Get_ExtAPI_Strictness(id);
			int i = 0;
			while( *s ) {
			    switch( *s ) {
				// Leave it as is
				case '-':
				    break;
				// Just make it WHNF
				case '0': {
				    arg1 = GET_APPLY_RIGHT(*(sp+i));
				    arg1 = traverse_left(arg1);
				    if( is_fail(arg1) ) {
					OVERWRITE(redex, arg1);
					sp = sp + acnt;
					depth = depth - acnt;
					root = redex;
					DO_TRACE_DBG();
					pop_trace_fn();
					goto start;
				    }
				    break;
				}
				// Force it completely
				case '1': {
				    arg1 = GET_APPLY_RIGHT(*(sp+i));
				    arg1 = force(arg1, FALSE);
				    if( is_fail(arg1) ) {
					OVERWRITE(redex, arg1);
					sp = sp + acnt;
					depth = depth - acnt;
					root = redex;
					DO_TRACE_DBG();
					pop_trace_fn();
					goto start;
				    }
				    break;
				}
				// Force the list but not the content of list
				case '2': {
				    arg1 = GET_APPLY_RIGHT(*(sp+i));
				    arg1 = reduce_list(arg1,1);
				    if( is_fail(arg1) ) {
					OVERWRITE(redex, arg1);
					sp = sp + acnt;
					depth = depth - acnt;
					root = redex;
					DO_TRACE_DBG();
					pop_trace_fn();
					goto start;
				    }
				    break;
				}
				// Force the list and the list within this
				// list but no further
				case '3': {
				    arg1 = GET_APPLY_RIGHT(*(sp+i));
				    arg1 = reduce_list(arg1,2);
				    if( is_fail(arg1) ) {
					OVERWRITE(redex, arg1);
					sp = sp + acnt;
					depth = depth - acnt;
					root = redex;
					DO_TRACE_DBG();
					pop_trace_fn();
					goto start;
				    }
				    break;
				}

				default:
				    DIE("Illegal strictness (%c)", *s);
				    break;
			    }
			    i++;
			    s++;
			}
			// Now perform the function
			Get_ExtAPI_Function(id)(redex);
			pop_trace_fn();
			sp = sp + acnt;
			depth = depth - acnt;
			root = redex;
			DO_TRACE_DBG();
			goto start;
		    }

		case P_REF_VAR:
		    ASSERT(depth == 0);
		    goto ret;

		case P_VOID:
		    ASSERT(depth == 0);
		    goto ret;

		case P_FILEFP:
		    ASSERT(depth == 0);
		    goto ret;

		case P_LOAD_PLUGIN:
                    if( depth < 1 ) goto clean_up;
                    redex = *sp;
                    arg1  = traverse_left(GET_APPLY_RIGHT(*sp));
                    if( is_fail(arg1) ) goto arg1_fail1;
                    if(!load_plugin(GET_STRING(arg1), &s)) {
                        Fail_pr(wastrsave(&strings, s));
                        goto fail1;
                    }
                    MAKE_REDEX_STRING(redex, wastrsave(&strings, s));
                    goto finish1;

		default:
		    DIE("Illegal primitive function");
	    }
	    DIE("Illegal node type");
	default:
	    DIE("Illegal node type");
    }
    DIE("Should never happen")

    /* Common code sequences */

  finish1:
	DO_TRACE_DBG();
	DEC_REF_CNT(root);
	DEC_REF_CNT(arg1);
        sp = sp + 1; depth = depth - 1;
	root = redex;
	goto start;
  finish2:
	DO_TRACE_DBG();
	DEC_REF_CNT(*sp);
	DEC_REF_CNT(arg2);
        sp = sp + 2; depth = depth - 2;
	root = redex;
	goto start;
  finish3:
	DO_TRACE_DBG();
	DEC_REF_CNT(*(sp+1));
	DEC_REF_CNT(arg3);
        sp = sp + 3; depth = depth - 3;
	root = redex;
	goto start;
  finish4:
	DO_TRACE_DBG();
	DEC_REF_CNT(*(sp+2));
	DEC_REF_CNT(arg4);
        sp = sp + 4; depth = depth - 4;
	root = redex;
	goto start;

  arg1_fail1:
	OVERWRITE(redex, arg1); sp = sp + 1; depth = depth - 1; root = redex;
	DO_TRACE_DBG();
        goto start;
  arg1_fail2:
	OVERWRITE(redex, arg1); sp = sp + 2; depth = depth - 2; root = redex;
	DO_TRACE_DBG();
        goto start;
  arg2_fail2:
	OVERWRITE(redex, arg2); sp = sp + 2; depth = depth - 2; root = redex;
	DO_TRACE_DBG();
        goto start;
  arg1_fail3:
	OVERWRITE(redex, arg1); sp = sp + 3; depth = depth - 3; root = redex;
	DO_TRACE_DBG();
        goto start;
  arg2_fail3:
	OVERWRITE(redex, arg2); sp = sp + 3; depth = depth - 3; root = redex;
	DO_TRACE_DBG();
        goto start;
  arg3_fail3:
	OVERWRITE(redex, arg3); sp = sp + 3; depth = depth - 3; root = redex;
	DO_TRACE_DBG();
        goto start;
  fail1:
        SET_TYPE(redex, LEAF);
        SET_LEAF_TYPE(redex, PRIM_FN);
        SET_PRIM_FN(redex, P_FAIL);
	SET_FAIL_STRING(redex, wastrsave(&strings, FailBuf));
        sp = sp + 1; depth = depth - 1;
        root = redex;
	DO_TRACE_DBG();
        goto start;
  fail2:
        SET_TYPE(redex, LEAF);
        SET_LEAF_TYPE(redex, PRIM_FN);
        SET_PRIM_FN(redex, P_FAIL);
	SET_FAIL_STRING(redex, wastrsave(&strings, FailBuf));
        sp = sp + 2; depth = depth - 2;
        root = redex;
	DO_TRACE_DBG();
        goto start;
  fail3:
        SET_TYPE(redex, LEAF);
        SET_LEAF_TYPE(redex, PRIM_FN);
        SET_PRIM_FN(redex, P_FAIL);
	SET_FAIL_STRING(redex, wastrsave(&strings, FailBuf));
        sp = sp + 3; depth = depth - 3;
        root = redex;
	DO_TRACE_DBG();
        goto start;

  clean_up:
    /* Rewind stack and return root */
    if( depth > 0 ) {
        root = *(sp + (depth - 1));
        sp = sp+depth;
    }
  ret:
    Call_level--;
    return( root );
}

static bool
is_CONS_fun(g_ptr node)
{
    if( !IS_LEAF_VAR(node) )
	return FALSE;
    if( !STREQ(GET_VAR(node),s_colon) )
	return FALSE;
    g_ptr fn = Find_Function(symb_tbl, node);
    if( fn == NULL )
	return FALSE;
    if( IS_APPLY(fn) && IS_DEBUG(GET_APPLY_LEFT(fn)) ) {
	fn = GET_APPLY_RIGHT(fn);
    }
    if( !IS_LEAF(fn) )
	return FALSE;
    if( !IS_PRIM_FN(fn) )
	return FALSE;
    return( GET_PRIM_FN(fn) == P_CONS );
}

static bool
is_T_fun(g_ptr node)
{
    if( !IS_LEAF_VAR(node) )
	return FALSE;
    if( !STREQ(GET_VAR(node),s_T) )
	return FALSE;
    g_ptr fn = Find_Function(symb_tbl, node);
    if( fn == NULL )
	return FALSE;
    if( IS_APPLY(fn) && IS_DEBUG(GET_APPLY_LEFT(fn)) ) {
	fn = GET_APPLY_RIGHT(fn);
    }
    if( !IS_LEAF(fn) )
	return FALSE;
    if( IS_PRIM_FN(fn) && IS_EXTAPI_FN(fn) ) {
	return( STREQ(Get_ExtAPI_Function_Name(GET_EXTAPI_FN(fn)),s_T) );
    }
    if( !IS_BOOL(fn) )
	return FALSE;
    formula f = GET_BOOL(fn);
    return( f == B_One() );
}

static bool
is_F_fun(g_ptr node)
{
    if( !IS_LEAF_VAR(node) )
	return FALSE;
    if( !STREQ(GET_VAR(node),s_F) )
	return FALSE;
    g_ptr fn = Find_Function(symb_tbl, node);
    if( fn == NULL )
	return FALSE;
    if( IS_APPLY(fn) && IS_DEBUG(GET_APPLY_LEFT(fn)) ) {
	fn = GET_APPLY_RIGHT(fn);
    }
    if( IS_PRIM_FN(fn) && IS_EXTAPI_FN(fn) ) {
	return( STREQ(Get_ExtAPI_Function_Name(GET_EXTAPI_FN(fn)),s_F) );
    }
    if( !IS_LEAF(fn) )
	return FALSE;
    if( !IS_BOOL(fn) )
	return FALSE;
    formula f = GET_BOOL(fn);
    return( f == B_Zero() );
}

static bool
check_true_or_false(g_ptr node)
{
    formula f; 
    ASSERT( IS_LEAF(node) && IS_BOOL(node) );
    f = GET_BOOL(node);
    return( (f == B_One()) || (f == B_Zero()) );
}


static unsigned int
graph_hash_rec(g_ptr node, unsigned int n)
{
    pointer tmp;
    unsigned int res = 1;
    unsigned int res1, res2;
  restart_walk:
    if( node == NULL )
	return res;
    if( (tmp = (pointer) find_hash(&hash_comp_tbl,(pointer) node)) != NULL ) {
	res = (res + PTR2UINT(tmp)) % n;
	return res;
    }
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    res1 = graph_hash_rec( GET_APPLY_LEFT(node), n);
	    res2 = graph_hash_rec( GET_APPLY_RIGHT(node), n);
	    res = (res + 1619*res1 + 883*res2) % n;
	    insert_hash(&hash_comp_tbl, (pointer) node, INT2PTR(res));
	    return res;
	case CONS_ND:
	    res1 = graph_hash_rec( GET_CONS_HD(node), n);
	    res2 = graph_hash_rec( GET_CONS_TL(node), n);
	    res = (res + 4877*res1 + 769*res2) % n;
	    insert_hash(&hash_comp_tbl, (pointer) node, INT2PTR(res));
	    return res;
	case LEAF:
	    if( IS_PRIM_FN(node) && GET_PRIM_FN(node) == P_REF_VAR ) {
		int ref_var = GET_REF_VAR(node);
		node = Get_RefVar(ref_var);
		goto restart_walk;
	    }
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    res = (res + Arbi_hash(GET_AINT(node), n)) % n;
		    break;
		case STRING:
		    res = (res + str_hash(GET_STRING(node), n)) % n;
		    break;
		case BOOL:
		    res = (res + (unsigned int) GET_BOOL(node)) % n;
		    break;
		case BEXPR:
		    res = (res + PTR2UINT(GET_BEXPR(node))) % n;
		    break;
		case EXT_OBJ: {
		    unint class = GET_EXT_OBJ_CLASS(node);
		    // This is simplistic.
		    // I should add a hash_fn for all EXT_OBJs to reduce
		    // collisions!
		    res = (res + 17*class) % n;
		    break;
		}
		case PRIM_FN:
		    if( IS_EXTAPI_FN(node) ) {
			res = (res+947*((unsigned int)GET_EXTAPI_FN(node))) % n;
		    } else {
			res = (res+5*((unsigned int)GET_PRIM_FN(node))) % n;
		    }
		    break;
		default:
		    DIE("Unexpected leaf node type in Graph_hash");
	    }
	    insert_hash(&hash_comp_tbl, (pointer) node, INT2PTR(res));
	    return res;
	default:
	    DIE("Should never happen");
    }
}

unsigned int
Graph_hash(pointer np, unsigned int n)
{
    g_ptr g1 = (g_ptr) np;
    create_hash(&hash_comp_tbl, 1000, ptr_hash, ptr_equ);
    unsigned int res = graph_hash_rec(g1, n);
    dispose_hash(&hash_comp_tbl, NULLFCN);
    return res;
}

bool
Graph_equ(pointer p1, pointer p2)
{
    g_ptr g1 = (g_ptr) p1;
    g_ptr g2 = (g_ptr) p2;
    return( Is_equal(g1, g2, TRUE) == B_One() );
}

formula
Is_equal(g_ptr l1, g_ptr l2, bool identical)
{
    formula res;
    arbi_T  ai1, ai2;
    if( l1 == l2 )
        return( B_One() );
    l1 = reduce(l1, FALSE);
    l2 = reduce(l2, FALSE);
    if( l1 == l2 )
        return( B_One() );
    if( l1 == NULL || l2 == NULL )
        return( B_Zero() );
    switch( GET_TYPE(l1) ) {
        case APPLY_ND:
            /* Functions are always deemed unequal */
            return( B_Zero() );
        case CONS_ND:
            if( GET_TYPE( l2 ) != CONS_ND )
                return( B_Zero() );
            res = Is_equal(GET_CONS_HD(l1), GET_CONS_HD(l2), identical);
	    if( res == B_Zero() )
		return(res);
	    PUSH_BDD_GC(res);
            res = B_And(res,
			Is_equal(GET_CONS_TL(l1),GET_CONS_TL(l2),identical));
	    POP_BDD_GC(1);
            return( res );
        case LEAF:
            if( GET_TYPE( l2 ) != LEAF )
                return( B_Zero() );
            if( GET_LEAF_TYPE(l1) != GET_LEAF_TYPE(l2) )
                return( B_Zero() );
            switch( GET_LEAF_TYPE(l1) ) {
                case INT:
		    ai1 = GET_AINT(l1);
		    ai2 = GET_AINT(l2);
		    if( ai1 == ai2 )
			return( B_One() );
		    if( Arbi_cmp(ai1, ai2) == arbi_EQ )
			return( B_One() );
		    return( B_Zero() );
                case STRING:
                    return( (STREQ(GET_STRING(l1), GET_STRING(l2)))?
				B_One(): B_Zero() );
                case BOOL:
		    if( identical )
			if( B_Equal(GET_BOOL(l1), GET_BOOL(l2)) )
			    return( B_One() );
			else
			    return( B_Zero() );
		    else
			return(B_Xnor(GET_BOOL(l1), GET_BOOL(l2)));
                case BEXPR:
		    if( BE_Equal(GET_BEXPR(l1), GET_BEXPR(l2)) )
			return( B_One() );
		    if( identical ) {
			return( B_Zero() );
		     } else {
			Rprintf("Equality over complex bexprs not defined.");
		    }
		    break;
                case EXT_OBJ:
                    {
                        unint class = GET_EXT_OBJ_CLASS(l1);
                        ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
                        ASSERT( class == GET_EXT_OBJ_CLASS(l2) );
                        return( op->eq_fn(GET_EXT_OBJ(l1),
					  GET_EXT_OBJ(l2),
					  identical) );
                    }
		case PRIM_FN:
		    {
			if( IS_VOID(l1) && IS_VOID(l2) ) {
			    return( B_One() );
			} else {
			    return( B_Zero() );
			}
		    }
                default:
                    return( B_Zero() );
            }
	    break;
        default:
            DIE("Fatal error 101. Consult a guru!");
    }
    DIE("Should never occur!"); return B_Zero(); // Dummy
}

#if 1
g_ptr
force(g_ptr node, bool first)
{
    buffer todo_buf;
    if( node == NULL )
	return( node );
    if( IS_FORCED(node) ) {
	return( node );
    }
    new_buf(&todo_buf, 100, sizeof(g_ptr));
    push_buf(&todo_buf, &node);
    while( COUNT_BUF(&todo_buf) != 0 ) {
	g_ptr cur;
	pop_buf(&todo_buf, &cur);
	if( !IS_FORCED(cur) ) {
	    cur = reduce(cur, first);
	    if( is_fail(cur) ) {
		OVERWRITE(node, cur);
		free_buf(&todo_buf);
		return node;
	    }
	    MAKE_FORCED(cur);
	    if( GET_TYPE(cur) == CONS_ND ) {
		if( !IS_NIL(cur) ) {
		    g_ptr t = GET_CONS_TL(cur);
		    push_buf(&todo_buf, &t);
		    g_ptr h = GET_CONS_HD(cur);
		    push_buf(&todo_buf, &h);
		}
	    }
	}
    }
    free_buf(&todo_buf);
    return( node );
}
#else
g_ptr
force(g_ptr node, bool first)
{
    g_ptr res;
    if( node == NULL )
        return( node );
    if( IS_FORCED(node) ) {
        return( node );
    }
    node = reduce(node, first);

    if( GET_TYPE(node) == CONS_ND ) {
        if( GET_CONS_HD(node) == NULL ) {
            ASSERT( GET_CONS_TL(node) == NULL );
            MAKE_FORCED(node);
            return( node );
        }
        res = force(GET_CONS_HD(node), first);
        MAKE_FORCED(res);
        if( is_fail(res) ) {
            OVERWRITE(node, res);
            return(node);
        }
        res = force(GET_CONS_TL(node), first);
        MAKE_FORCED(res);
        if( is_fail(res) ) {
            OVERWRITE(node, res);
        }
    }
    MAKE_FORCED(node);
    return(node);
}
#endif

static g_ptr
reduce_list(g_ptr redex, int levels)
{
    if( redex == NULL )
	return( redex );
    if( IS_FORCED(redex) ) {
	return( redex );
    }
    if( levels <= 0 ) {
	return( redex );
    }
    g_ptr cur = traverse_left(redex);
    if( is_fail(cur) ) { return(redex); }
    while( GET_TYPE(cur) == CONS_ND && !IS_NIL(cur) ) {
	if( levels > 1 ) {
	    g_ptr hd = reduce_list(GET_CONS_HD(cur), levels-1);
	    if( is_fail(hd) ) {
		OVERWRITE(redex, hd);
		return( redex );
	    }
	}
	cur = traverse_left(GET_CONS_TL(cur));
	if( is_fail(cur) ) {
	    OVERWRITE(redex, cur);
	    return( redex );
	}
    }
    return redex;
}

static bool
print_result_local(g_ptr node, odests fp, bool pr_brack, bool pr_comma,
		   typeExp_ptr type)
{
    g_ptr h, t;
    bool  r = TRUE;
    g_ptr conv_fn;

    conv_fn = (g_ptr) find_hash(&pretty_printer_tbl, (pointer) type);
    if( conv_fn != NULL ) {
	print_nd = Make_APPL_ND(conv_fn, node);
	SET_REFCNT(print_nd,MAX_REF_CNT);  /* Make sure it never gets freed */
	print_nd = traverse_left(print_nd);
	ASSERT( GET_TYPE(print_nd) == LEAF );
	if( is_fail(print_nd) ) {
	    FP(fp, "Pretty printer failed: %s",
		    GET_FAIL_STRING(print_nd));
	    return( FALSE );
	} else {
	    string s;
	    s = GET_STRING(print_nd);
	    if( *s != '\0' )
		FP(fp, "%s", s);
	    return( TRUE );
	}
    }
    if( IS_EXT_OBJ(node) ) {
	unint class = GET_EXT_OBJ_CLASS(node);
	ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
	if( op->obj2string != NULL ) {
	    FP(fp, "%s", op->obj2string(GET_EXT_OBJ(node)));
	} else {
	    FP(fp, "-");
	}
	return( TRUE );
    }
    if( !Is_Printable_Type(type) ) {
	if( pr_comma )
	    FP(fp, ",");
	FP(fp, "-");
	return( FALSE );
    }
    switch( GET_TYPE(node) ) {
	case CONS_ND:
	    h = GET_CONS_HD(node);
	    t = GET_CONS_TL(node);
	    if( h == t && h == NULL ) {
		if( pr_brack )
		    FP(fp, "[]");
		break;
	    }
	    if( pr_comma )
		FP(fp, ",");
	    ASSERT(h != NULL && t != NULL );
	    if( Is_List(type) ) {
		if( pr_brack )
		    FP(fp, "[");
		r &= print_result_local(h,fp,TRUE,FALSE,Get_List_type(type));
		r &= print_result_local(t,fp,FALSE,TRUE,type);
		if( pr_brack )
		    FP(fp, "]");
	    } else if( Is_Tuple(type) ) {
		if( pr_brack )
		    FP(fp, "(");
		r &= print_result_local(h, fp, TRUE, FALSE, Get_Fst_Type(type));
		FP(fp,", ");
		r &= print_result_local(t,fp,TRUE,FALSE,Get_Snd_Type(type));
		if( pr_brack )
		    FP(fp, ")");
	    } else {
		if( pr_brack )
		    FP(fp, "(");
		r &= print_result_local(h, fp, TRUE, FALSE, Get_Fst_Type(type));
		if( Is_List(Get_Snd_Type(type)) ) {
		    if( GET_CONS_HD(t) == NULL )
			FP(fp,",");
		    r &= print_result_local(t,fp,TRUE,TRUE,Get_Snd_Type(type));
		} else {
		    r &= print_result_local(t,fp,FALSE,TRUE,Get_Snd_Type(type));
		}
		if( pr_brack )
		    FP(fp, ")");
	    }
	    break;
	case LEAF:
	    if( pr_comma )
		FP(fp, ",");
	    Print_leaf(node, fp);
	    break;
	default:
	    Print_Expr(node, fp);
	    break;
    }
    return( r );
}

static g_ptr
compile_rec(g_ptr node)
{
    if( node == NULL ) 
	return(node);
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    SET_APPLY_LEFT(node, compile_rec(GET_APPLY_LEFT(node)));
	    SET_APPLY_RIGHT(node, compile_rec(GET_APPLY_RIGHT(node)));
	    return(node);
	case LAMBDA_ND:
	    return(abstract_rec(GET_LAMBDA_VAR(node),
			    compile_rec(GET_LAMBDA_BODY(node))));
	default:
	    return(node);
    }
}

static void
sweep()
{
    int   free, used, ref;
    g_ptr nd;
    free_list = NULL;
    free = 0; used = 0; ref = 0;
    FOR_REC(&g_rec_mgr, g_ptr, nd) {
	if( GET_MARK(nd) == 0 ) {
#ifndef DONT_FREE_ND
	    nd->L = 0;
	    SET_RIGHT(nd, (pointer) free_list);
	    free_list = nd;
	    free++;
#else
#if TRACK_FREEING
	    nd->oldL = nd->L;
	    nd->oldR = nd->R;
	    nd->file = garbage_collection_started_file;
	    nd->line = garbage_collection_started_line;
#endif
	    SET_TYPE(nd, LEAF);
	    SET_LEAF_TYPE(nd, PRIM_FN);
	    SET_PRIM_FN(nd, P_ILLEGAL);
	    SET_MARK(nd, 0);
	    used++;
#endif
	} else {
	    used++;
	    if( GET_REFCNT(nd) == 3 ) {
		ref++;
	    } else {
		SET_MARK(nd, 0);
	    }
	}
    }
    if( RCverbose_GC )
	FP(fl_gc_fp, "(Used=%d(Shared=%d) Freed:%d).", used, ref, free);
    last_allocated = used;
}

g_ptr
reduce(g_ptr root, bool first)
{
    g_ptr ret;
    if( first && do_gc_asap )
	Garbage_collect();
    ret = traverse_left(root);
    if( first && do_gc_asap )
	Garbage_collect();
    return( ret );
}

g_ptr
Eval(g_ptr redex)
{
    eval_ctx_rec ctx;
    Record_eval_context(&ctx);
    jmp_buf start_env;
    start_envp = &start_env;
    PUSH_GLOBAL_GC(redex);
    root_node = redex;
    if( setjmp(*start_envp) == 0 ) {
        /* All ok */
        force(redex, FALSE);
    } else {
        /* Return from a failure */
	MAKE_REDEX_FAILURE(redex, FailBuf);
    }
    Restore_eval_context(&ctx);
    POP_GLOBAL_GC(1);
    return redex;
}

static void
push_trace_fn(string name)
{
//fprintf(stderr, "push_trace_fn %s\n", name);
    push_buf(&fn_trace_buf, (pointer) &name);
}

static void
pop_trace_fn()
{
    string dummy;
    pop_buf(&fn_trace_buf, (pointer) &dummy);
//fprintf(stderr, "pop_buf %s\n", dummy);
}


static g_ptr
do_explode(string s)
{
    char buf[2];
    string new;
    if( *s == 0 )
	return( Make_NIL() );
    buf[0] = *s;
    buf[1] = 0;
    s++;
    new = wastrsave(&strings, buf);
    return( Make_CONS_ND(Make_STRING_leaf(new), do_explode(s)) );
}

static void
free_CL_node(cl_ptr nd)
{
    free_rec(&cl_rec_mgr, (pointer) nd);
}

static g_ptr
convert_CL_rec(cl_ptr nd)
{
    g_ptr res;
    if( nd->next != NULL ) {
	res = convert_CL_rec(nd->next);
	if( nd->op == mk_list_tp )
	    res = Make_2inp_Primitive(P_CONS, nd->expr, res);
	else {
	    ASSERT( nd->op == mk_tuple_tp );
	    res = Make_2inp_Primitive(P_TUPLE, nd->expr, res);
	}
	free_CL_node(nd);
	return( res );
    }
    res = nd->expr;
    free_CL_node(nd);
    return( res );
}

static g_ptr
pat_match_rec(g_ptr arg, g_ptr E, int cnt, string *name, int *tot_cnt)
{
    g_ptr res;

    if( GET_TYPE(arg) == APPLY_ND ) {
	char buf[32];
	res = TrArg(GET_APPLY_RIGHT(arg), E, FALSE);
	if( res == NULL )
	    return( NULL );
	Sprintf(buf, "_Q_%d", cnt);
	res = Make_APPL_ND(res, Make_VAR_leaf(wastrsave(&strings, buf)));
	return( pat_match_rec(GET_APPLY_LEFT(arg), res, cnt+1, name, tot_cnt) );
    }
    ASSERT( GET_TYPE(arg) == LEAF );
    ASSERT( GET_LEAF_TYPE(arg) == VAR );
    *name = GET_VAR(arg);
    *tot_cnt = cnt-1;
    return( E );
}

static g_ptr
get_constr_name(g_ptr node)
{
    if( GET_TYPE(node) == APPLY_ND )
	return( get_constr_name(GET_APPLY_LEFT(node)) );
    else
	return( node );
}


static unsigned int
gmap2_hash(pointer np, unsigned int n)
{
    gmap2_ptr p = (gmap2_ptr) np;
    return( (137*((unint) ((lunint) p->l))+ ((unint) ((lunint) p->r))) % n);
}

static bool
gmap2_equ(pointer p1, pointer p2)
{
    gmap2_ptr g1 = (gmap2_ptr) p1;
    gmap2_ptr g2 = (gmap2_ptr) p2;
    return( (g1->l == g2->l) && (g1->r == g2->r) );
}

static g_ptr
trarg(g_ptr node, g_ptr E, bool constructor)
{
    string new;
    g_ptr ret, l1;

    ASSERT( node != NULL );
    switch( GET_TYPE(node) ) {
	case LEAF:
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    /* \x. x = val(node) => E | PFAIL */
		    new = Find_new_free_var(E);
		    ret = Make_Lambda(new,
			    Make_3inp_Primitive(P_COND,
				Make_2inp_Primitive(P_EQUAL,
				    Make_VAR_leaf(new),
				    Make_AINT_leaf(GET_AINT(node))),
				E,
				Make_0inp_Primitive(P_PFAIL)));
		    return(ret);
		case STRING:
		    /* \x. x = val(node) => E | PFAIL */
		    new = Find_new_free_var(E);
		    ret = Make_Lambda(new,
			    Make_3inp_Primitive(P_COND,
				Make_2inp_Primitive(P_CONSTR_EQ,
				    Make_VAR_leaf(new),
				    Make_STRING_leaf(GET_STRING(node))),
				E,
				Make_0inp_Primitive(P_PFAIL)));
		    new = Find_new_free_var(E);
		    return(ret);
		case BOOL:
		    /* \x. x = val(node) => E | PFAIL */
		    new = Find_new_free_var(E);
		    ret = Make_Lambda(new,
			    Make_3inp_Primitive(P_COND,
				Make_2inp_Primitive(P_IDENTICAL,
				    Make_VAR_leaf(new),
				    Make_BOOL_leaf(GET_BOOL(node))),
				E,
				Make_0inp_Primitive(P_PFAIL)));
		    return(ret);
		case BEXPR:
		    Rprintf("Error in definition. Cannot match bexprs.\n");
		    break;
		case VAR:
		    if( find_hash(&Constructor_Table, (pointer)GET_VAR(node))
			!= NULL ) {
			/* A constructor */
			/* \x. x = val(node) => E | PFAIL */
			if( constructor ) {
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(P_COND,
					Make_2inp_Primitive(P_CONSTR_EQ,
					    Make_VAR_leaf(new),
					    mk_constr_nd(GET_VAR(node))),
					E,
					Make_0inp_Primitive(P_PFAIL)));
			} else {
			    string dest_name;
			    dest_name = make_Destructor(node);
			    new = Find_new_free_var(E);
			    ret = Make_APPL_ND(
				    Make_VAR_leaf(dest_name),
				    Make_Lambda(new,
					Make_3inp_Primitive(P_COND,
					    Make_2inp_Primitive(P_CONSTR_EQ,
					       Make_VAR_leaf(new),
					       mk_constr_nd(GET_VAR(node))),
					    E,
					    Make_0inp_Primitive(P_PFAIL))));
			}
		    } else {
			/* Treat T and F specially unless redefined */
			if( is_T_fun(node) ) {
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(P_COND,
					Make_2inp_Primitive(P_IDENTICAL,
					    Make_VAR_leaf(new),
					    Make_BOOL_leaf(B_One())),
					E,
					Make_0inp_Primitive(P_PFAIL)));
			} else if( is_F_fun(node) ) {
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(P_COND,
					Make_2inp_Primitive(P_IDENTICAL,
					    Make_VAR_leaf(new),
					    Make_BOOL_leaf(B_Zero())),
					E,
					Make_0inp_Primitive(P_PFAIL)));
			} else {
			    /* A variable for the function */
			    /* \val(node).E */
			    ret = Make_Lambda(GET_VAR(node), E);
			    MoveTypeHint(node, ret, TRUE);
			}
		    }
		    return(ret);
		case PRIM_FN:
		    Rprintf("Illegal pattern in function definition\n");
		    break;
		case USERDEF:
		    DIE("USERDEFs should not exists before typechecking!");
		default:
		    DIE("Unknown leaf type");
	    }
	    break;
	case CONS_ND:
	    if( GET_CONS_HD(node) != NULL || GET_CONS_TL(node) != NULL )
		Rprintf("Illegal pattern in function definition\n");
	    /* NIL */
	    /* \x. (empty x) => E | PFAIL */
	    new = Find_new_free_var(E);
	    ret = Make_Lambda(new,
		    Make_3inp_Primitive(P_COND,
			Make_1inp_Primitive(P_EMPTY,
			    Make_VAR_leaf(new)),
			E,
			Make_0inp_Primitive(P_PFAIL)));
	    return(ret);
	case APPLY_ND:
	    /* Special cases since FL doesn't implement strings, lists, */
	    /* and numbers using constructors. 				*/
	    l1 = GET_APPLY_LEFT(node);
	    if( GET_TYPE(l1) == APPLY_ND ) {
		g_ptr ll1;
		ll1 = GET_APPLY_LEFT(l1);
	        if(GET_TYPE(ll1) == LEAF && GET_LEAF_TYPE(ll1) == PRIM_FN ) {
		    g_ptr a, b;
		    switch( GET_PRIM_FN(ll1) ) {
			case P_CAT:
			    a = GET_APPLY_RIGHT(node);
			    b = GET_APPLY_RIGHT(l1);
			    /* ((STRING a) b) */
			    /* \x.[(x != "" =>				*/
			    /*  (trarg(a, (trarg(b,E) (str_tl x)))	*/
			    /*			(str_hd x)) | FAIL]	*/
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(P_COND,
					Make_2inp_Primitive(P_NOT_EQUAL,
					    Make_VAR_leaf(new),
					    Make_STRING_leaf(
						      wastrsave(&strings, ""))),
					Make_APPL_ND(
					    trarg(
						a,
						Make_APPL_ND(
						    trarg(b, E, FALSE),
						    Make_1inp_Primitive(
							P_STRING_HD,
							Make_VAR_leaf(new))),
						FALSE),
					    Make_1inp_Primitive(P_STRING_TL,
						Make_VAR_leaf(new))),
				    Make_0inp_Primitive(P_PFAIL)));
			    return(ret);
			case P_CONS:
			    /* ((CONS a) b) 				     */
			    /* \x.[(is_cons x) =>			     */
			    /*  (trarg(a,(trarg(b,E) (tl x))) (hd x)) |      */
			    /*					      PFAIL] */
			    a = GET_APPLY_RIGHT(l1);
			    b = GET_APPLY_RIGHT(node);
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(
					P_COND,
					Make_1inp_Primitive(P_IS_CONS,
							    Make_VAR_leaf(new)),
					Make_APPL_ND(
					    trarg( a,
						   Make_APPL_ND(
						       trarg(b, E, FALSE),
						       Make_1inp_Primitive(
							  P_TAIL,
							  Make_VAR_leaf(new))),
						FALSE),
					    Make_1inp_Primitive(P_HEAD,
						Make_VAR_leaf(new))),
					Make_0inp_Primitive(P_PFAIL)));
			    return( ret );
			case P_TUPLE:
			    /* ((TUPLE a) b) 				     */
			    /* \x.[(is_cons x) =>			     */
			    /*  (trarg(a,(trarg(b,E) (snd x))) (fst x)) |    */
			    /*					      PFAIL] */
			    a = GET_APPLY_RIGHT(l1);
			    b = GET_APPLY_RIGHT(node);
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(
					P_COND,
					Make_1inp_Primitive(P_IS_TUPLE,
							    Make_VAR_leaf(new)),
					Make_APPL_ND(
					    trarg( a,
						   Make_APPL_ND(
						       trarg(b, E, FALSE),
						       Make_1inp_Primitive(
							  P_SND,
							  Make_VAR_leaf(new))),
						FALSE),
					    Make_1inp_Primitive(P_FST,
						Make_VAR_leaf(new))),
					Make_0inp_Primitive(P_PFAIL)));
			    return( ret );
			default:
			    break;
		    }
		}
	    } else {
		if(  GET_TYPE(l1) == LEAF && GET_LEAF_TYPE(l1) == PRIM_FN ) {
		    switch( GET_PRIM_FN(l1) ) {
			case P_SUC:
			    /* (SUC n) */
			    /* \x. x>0 => (trarg(n,E) (x-1)) | PFAIL */
			    new = Find_new_free_var(E);
			    ret = Make_Lambda(new,
				    Make_3inp_Primitive(P_COND,
					Make_2inp_Primitive(P_GREATER,
					    Make_VAR_leaf(new),
					    Make_INT_leaf(0)),
					Make_APPL_ND(
					    trarg(
						GET_APPLY_RIGHT(node),
						E,
						FALSE),
					    Make_2inp_Primitive(P_MINUS,
						Make_VAR_leaf(new),
						Make_INT_leaf(1))),
					Make_0inp_Primitive(P_PFAIL)));
			    return(ret);
			default:
			    break;
		    }
		}
	    }
	    /* Special case for list constructor */
	    if( GET_TYPE(l1) == APPLY_ND ) {
		g_ptr ll1;
		ll1 = GET_APPLY_LEFT(l1);
	        if(is_CONS_fun(ll1) &&
		   !find_hash(&Constructor_Table, (pointer)GET_VAR(ll1)) )
		{
		    g_ptr a, b;
		    /* ((CONS a) b) 				     */
		    /* \x.[(is_cons x) =>			     */
		    /*  (trarg(a,(trarg(b,E) (tl x))) (hd x)) |    */
		    /*					      PFAIL] */
		    a = GET_APPLY_RIGHT(l1);
		    b = GET_APPLY_RIGHT(node);
		    new = Find_new_free_var(E);
		    ret = Make_Lambda(new,
			    Make_3inp_Primitive(
				P_COND,
				Make_1inp_Primitive(P_IS_CONS,
						    Make_VAR_leaf(new)),
				Make_APPL_ND(
				    trarg( a,
					   Make_APPL_ND(
					       trarg(b, E, FALSE),
					       Make_1inp_Primitive(
						  P_TAIL,
						  Make_VAR_leaf(new))),
					FALSE),
				    Make_1inp_Primitive(P_HEAD,
					Make_VAR_leaf(new))),
				Make_0inp_Primitive(P_PFAIL)));
		    return( ret );
		}
	    }
	    /* p q */
	    /* \x.[(is_cons x) =>					*/
	    /*       (trarg(p, (trarg(q,E) (snd x))) (fst x)) | PFAIL]	*/
	    new = Find_new_free_var(E);
	    if( constructor ) {
		ret = Make_Lambda(new,
			Make_3inp_Primitive(P_COND,
			    Make_1inp_Primitive(P_IS_TUPLE, Make_VAR_leaf(new)),
			    Make_APPL_ND(
				trarg(
				    GET_APPLY_LEFT(node),
				    Make_APPL_ND(
					trarg(GET_APPLY_RIGHT(node), E, FALSE),
					Make_1inp_Primitive(P_SND,
					    Make_VAR_leaf(new))),
				    TRUE),
				Make_1inp_Primitive(P_FST,
				    Make_VAR_leaf(new))),
			    Make_0inp_Primitive(P_PFAIL)));
	    } else {
		g_ptr constr_nd;
		string dest_name;
		constr_nd = get_constr_name(GET_APPLY_LEFT(node));
		dest_name = make_Destructor(constr_nd);
		ret = Make_APPL_ND(
			Make_VAR_leaf(dest_name),
			Make_Lambda(new,
			    Make_3inp_Primitive(P_COND,
				Make_1inp_Primitive(P_IS_TUPLE,
						    Make_VAR_leaf(new)),
				Make_APPL_ND(
				    trarg(
					GET_APPLY_LEFT(node),
					Make_APPL_ND(
					    trarg(GET_APPLY_RIGHT(node),
						  E, FALSE),
					    Make_1inp_Primitive(P_SND,
						Make_VAR_leaf(new))),
					TRUE),
				    Make_1inp_Primitive(P_FST,
					Make_VAR_leaf(new))),
				Make_0inp_Primitive(P_PFAIL))));
	    }
	    return( ret );
	    break;
	default:
	   Rprintf("Illegal pattern in function definition\n");
    }
    DIE("Should never occur!"); return NULL; // Dummy
}

static string
make_Destructor(g_ptr np)
{
    if( GET_TYPE(np) != LEAF || GET_LEAF_TYPE(np) != VAR)
	Rprintf("Illegal pattern in function definition");
    string ret = strtemp("__DeStRuCtOr");
    ret = strappend(GET_VAR(np));
    ret = wastrsave(&strings, ret);
    if( Find_Function(symb_tbl, np) == NULL ) {
	Rprintf(
       "Illegal pattern in function definition. %s is not a type constructor\n",
	GET_VAR(np));
    }
    return(ret);
}

bool
G_check_for_errors(g_ptr np, int cnt)
{
    if( np == NULL )
	return(TRUE);
    if( cnt == 0 ) {
	return FALSE;
    }
    switch( GET_TYPE(np) ) {
        case APPLY_ND:
            return( G_check_for_errors(GET_APPLY_LEFT(np), cnt-1) ||
		    G_check_for_errors(GET_APPLY_RIGHT(np), cnt-1) );
        case LAMBDA_ND:
            return( G_check_for_errors(GET_LAMBDA_BODY(np), cnt-1) );
        case CONS_ND:
	    if( GET_CONS_HD(np) == NULL && GET_CONS_TL(np) == NULL )
		return( FALSE );
            return( G_check_for_errors(GET_CONS_HD(np), cnt-1) ||
		    G_check_for_errors(GET_CONS_TL(np), cnt-1) );
        case LEAF:
	    return( FALSE );
            break;
        default:
            DIE("Cannot happen?");
    }
    DIE("Should never occur!"); return FALSE; // Dummy
}


static res_rec
compile(g_ptr node, free_list_ptr vlp)
{
    res_rec		res, res1, res2;
    free_list_ptr	flp, np1, np2, tnp, *prevp, cur;
    free_list_rec	flrec;
    g_ptr		cc;

    if( node == NULL ) {
	res.res = node;
	res.needed = NULL;
	return( res );
    }
    switch( GET_TYPE(node) ) {
	case LEAF:
	    if( IS_VAR(node) ) {
		while( vlp != NULL ) {
		    if( vlp->var == GET_VAR(node) ) {
			flp = (free_list_ptr) new_rec(&free_rec_mgr);
			flp->var = GET_VAR(node);
			flp->next = NULL;
			res.needed = flp;
			res.res = Make_0inp_Primitive(P_I);
			return(res);
		    }
		    vlp = vlp->next;
		}
	    }
	    res.needed = NULL;
	    res.res = node;
	    return(res);
	case APPLY_ND:
	    res1 = compile(GET_APPLY_LEFT(node), vlp);
	    res2 = compile(GET_APPLY_RIGHT(node), vlp);
	    cc = NULL;
	    np1 = res1.needed;
	    np2 = res2.needed;
	    res.needed = NULL;
	    prevp = &(res.needed);
	    cur = vlp;
	    while( cur != NULL ) {
		if(np1 && cur->var == np1->var) {
		    if( np2 && cur->var == np2->var ) {
			/* Argument must be sent to both sides */
			if( cc == NULL ) {
			    cc = Make_0inp_Primitive(P_S);
			} else {
			    cc = Make_1inp_Primitive(P_SPRIME, cc);
			}
			tnp = np2;
			np2 = np2->next;
			free_rec(&free_rec_mgr, (pointer) tnp);
		    } else {
			/* Argument must be sent only to the left */
			if( cc == NULL ) {
			    cc = Make_0inp_Primitive(P_C);
			} else {
			    cc = Make_1inp_Primitive(P_CPRIME, cc);
			}
		    }
		    *prevp = np1;
		    np1 = np1->next;
		    (*prevp)->next = NULL;
		    prevp = &((*prevp)->next);
		} else {
		    if( np2 && cur->var == np2->var ) {
			/* Argument must be sent only to the right */
			if( cc == NULL ) {
			    cc = Make_0inp_Primitive(P_B);
			} else {
			    cc = Make_1inp_Primitive(P_BPRIME, cc);
			}
			*prevp = np2;
			np2 = np2->next;
			(*prevp)->next = NULL;
			prevp = &((*prevp)->next);
		    } else {
			/* Argument is not needed */
		    }
		}
		cur = cur->next;
	    }
	    if( cc != NULL )
		res.res = Make_APPL_ND(Make_APPL_ND(cc, res1.res), res2.res);
	    else
		res.res = Make_APPL_ND(res1.res, res2.res);
	    return(res);

	case LAMBDA_ND:
	    flrec.var = GET_LAMBDA_VAR(node);
	    flrec.next = vlp;
	    res2 = compile(GET_LAMBDA_BODY(node), &flrec);
	    flp = res2.needed;
	    if( flp && flp->var == GET_LAMBDA_VAR(node) ) {
		res2.needed = flp->next;
		free_rec(&free_rec_mgr, (pointer) flp);
		return(res2);
	    }
	    /* Argument not needed */
	    cc = NULL;
	    res1.res = Make_0inp_Primitive(P_K);
	    np2 = res2.needed;
	    res.needed = NULL;
	    prevp = &(res.needed);
	    cur = vlp;
	    while( cur != NULL ) {
		if( np2 && cur->var == np2->var ) {
		    /* Argument must be sent to the right */
		    if( cc == NULL ) {
			cc = Make_0inp_Primitive(P_B);
		    } else {
			cc = Make_1inp_Primitive(P_BPRIME, cc);
		    }
		    *prevp = np2;
		    np2 = np2->next;
		    (*prevp)->next = NULL;
		    prevp = &((*prevp)->next);
		} else {
		    /* Argument is not needed */
		}
		cur = cur->next;
	    }
	    if( cc != NULL )
		res.res = Make_APPL_ND(Make_APPL_ND(cc, res1.res), res2.res);
	    else
		res.res = Make_APPL_ND(res1.res, res2.res);
	    return(res);
	case CONS_ND:
	    ASSERT( GET_CONS_HD(node) == NULL && GET_CONS_TL(node) == NULL);
	    res.needed = NULL;
	    res.res = node;
	    return(res);
	default:
	    DIE("Cannot happen");
    }
    DIE("Should never occur!"); return res;
}



static g_ptr
g_b_ite(g_ptr l, g_ptr r)
{
    g_ptr res;
    // Note: Gen_map2 will only call g_be_ite with the same leaf type!
    ASSERT(IS_LEAF(l));
    if( !IS_BOOL(l) ) {
        if( Is_equal(l, r, TRUE) != B_One() ) {
            res = Make_0inp_Primitive(P_FAIL);
            string m = wastrsave(&strings,
				 "if-then-else over non-matching structures");
            SET_FAIL_STRING(res, m);
            return( res );
        }
        INC_REFCNT(l);
        return l;
    }
    formula lb = GET_BOOL(l);
    formula rb = GET_BOOL(r);
    formula bres = B_Or(B_And(current_bdd_cond, lb),
                        B_And(B_Not(current_bdd_cond),rb));
    if( bres == lb ) {
        INC_REFCNT(l);
        return l;
    }
    if( bres == rb ) {
        INC_REFCNT(r);
        return r;
    }
    res = Make_BOOL_leaf(bres);
    return res;
}


static g_ptr
mkConstr0(string name)
{
    return( mk_constr_nd(name) );
}

static g_ptr
mkConstr1(string name, g_ptr e1)
{
    return( Make_CONS_ND(mk_constr_nd(name), e1) );
}

static g_ptr
mkConstr2(string name, g_ptr e1, g_ptr e2)
{
    return( Make_CONS_ND(Make_CONS_ND(mk_constr_nd(name), e1),e2) );
}

g_ptr
Reflect_expr(g_ptr node)
{

    if( IS_NIL(node)) {
	return( mkConstr0(s_NIL) );
    }
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    if( IS_UNQUOTE(GET_APPLY_LEFT(node)) ) {
		return( GET_APPLY_RIGHT(node) );
	    }
	    return( mkConstr2(s_APPLY,
			       Reflect_expr(GET_APPLY_LEFT(node)),
			       Reflect_expr(GET_APPLY_RIGHT(node))) );
	case LAMBDA_ND:
	    return( mkConstr2(s_LAMBDA,
			       Make_STRING_leaf(GET_LAMBDA_VAR(node)),
			       Reflect_expr(GET_LAMBDA_BODY(node))) );
	case CONS_ND:
	    return( mkConstr2(s_CONS,
			       Reflect_expr(GET_CONS_HD(node)) ,
			       Reflect_expr(GET_CONS_TL(node))) );
	case LEAF:
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    return( mkConstr1(
				s_LEAF,
			        mkConstr1(s_INT,
					   Make_INT_leaf(GET_INT(node)))) );
		case STRING:
		    return( mkConstr1(
				s_LEAF,
			        mkConstr1(
				    s_STRING,
				    Make_STRING_leaf(GET_STRING(node)))) );
		case BOOL:
		    return( mkConstr1(
				s_LEAF,
			        mkConstr1(
				    s_BOOL,
				    Make_BOOL_leaf(GET_BOOL(node)))) );
		case BEXPR:
		    return( mkConstr1(
				s_LEAF,
			        mkConstr1(
				    s_BEXPR,
				    Make_BEXPR_leaf(GET_BEXPR(node)))) );
		case PRIM_FN:
		    {
			int pfn = GET_PRIM_FN(node);
			if( pfn == P_DEBUG ) {
			    string name = GET_DEBUG_STRING(node);
			    return( mkConstr1(
					s_LEAF,
					mkConstr2(
					    s_EXT_PRIM_FN,
					    Make_INT_leaf(pfn),
					    Make_STRING_leaf(name))) );
			}
			if( pfn == P_PRINTF || pfn == P_SPRINTF ||
			    pfn == P_EPRINTF || pfn == P_FPRINTF )
			{
			    string fmt = GET_PRINTF_STRING(node);
			    return( mkConstr1(
					s_LEAF,
					mkConstr2(
					    s_EXT_PRIM_FN,
					    Make_INT_leaf(pfn),
					    Make_STRING_leaf(fmt))) );
			}
			if( pfn == P_EXTAPI_FN ) {
			    string name = Get_ExtAPI_Function_Name(
						GET_EXTAPI_FN(node));
			    return( mkConstr1(
					s_LEAF,
					mkConstr2(
					    s_EXT_PRIM_FN,
					    Make_INT_leaf(pfn),
					    Make_STRING_leaf(name))) );
			}
			return( mkConstr1(
				    s_LEAF,
				    mkConstr1(
					s_PRIM_FN,
					Make_INT_leaf(pfn))) );
		    }
		case VAR:
		    return( mkConstr1(s_VAR,
				       Make_STRING_leaf(GET_VAR(node))) );
		case USERDEF: {
		    // Not clear what to do here....
		    fn_ptr fp = GET_USERDEF(node);
		    return( mkConstr1(
				s_LEAF,
				mkConstr1(
				    s_USERDEF,
				    Make_INT_leaf(fp->id))) );
		}
		default:
		    DIE("Illegal leaf node type");
	    }
	    break;
	default:
	    DIE("Illegal node type");
    }
}

formula
Get_cur_eval_cond()
{
    return cur_eval_cond;
}

/* ============================================================ */
/* 			DEBUGGING STUFF				*/
/* ============================================================ */

#ifdef DEBUG

static bool
is_a_list(g_ptr node)
{
    g_ptr cur;
    if( node == NULL )
	return(FALSE);
    cur = node;
    while( cur && IS_CONS(cur) ) {
	cur = GET_CONS_TL(cur);
    }
    if( cur != NULL ) {
	return(FALSE);
    }
    return(TRUE);
}

bool
cmp_graph(g_ptr l1, g_ptr l2)
{
    hash_record done_tbl;
    create_hash(&done_tbl, 10000, ptr_hash, ptr_equ);
    bool res = cmp_graph_rec(&done_tbl, l1, l2);
    dispose_hash(&done_tbl, NULLFCN);
    return res;
}

static bool
cmp_graph_rec(hash_record *dtbl, g_ptr l1, g_ptr l2)
{
    arbi_T  ai1, ai2;
    if( l1 == l2 )
        return( TRUE );
    if( l1 == NULL || l2 == NULL ) {
	fprintf(stderr, "0cmp_graph failed for %p & %p\n", l1, l2);
        return( FALSE );
    }
    if( GET_TYPE(l1) != GET_TYPE(l2) ) {
	fprintf(stderr, "1cmp_graph failed for %p & %p\n", l1, l2);
	return FALSE;
    }
    if( GET_TYPE(l1) == LEAF && GET_LEAF_TYPE(l1) != GET_LEAF_TYPE(l2) ) {
	fprintf(stderr, "2cmp_graph failed for %p & %p\n", l1, l2);
	return FALSE;
    }
    g_ptr tmp = (g_ptr) find_hash(dtbl, l1);
    if( tmp != NULL ) {
	if( tmp == l2 ) { return TRUE; }
	if( cmp_graph_rec(dtbl,tmp,l2) == TRUE ) { return TRUE; }
	fprintf(stderr, "00cmp_graph failed for %p & %p\n", l1, l2);
	return FALSE;
    }
    insert_hash(dtbl, (pointer) l1, (pointer) l2);
    switch( GET_TYPE(l1) ) {
        case APPLY_ND:
	    if( cmp_graph_rec(dtbl, GET_APPLY_LEFT(l1), GET_APPLY_LEFT(l2)) == FALSE ) {
		fprintf(stderr, "3cmp_graph failed for %p & %p\n", GET_APPLY_LEFT(l1), GET_APPLY_LEFT(l2));
		return FALSE;
	    }
	    if( cmp_graph_rec(dtbl,GET_APPLY_RIGHT(l1), GET_APPLY_RIGHT(l2)) == FALSE) {
		fprintf(stderr, "4cmp_graph failed for %p & %p\n", GET_APPLY_RIGHT(l1), GET_APPLY_RIGHT(l2));
		return FALSE;
	    }
	    return TRUE;
        case CONS_ND:
	    if( cmp_graph_rec(dtbl,GET_CONS_HD(l1), GET_CONS_HD(l2)) == FALSE ) {
		fprintf(stderr, "5cmp_graph failed for %p & %p\n", GET_CONS_HD(l1), GET_CONS_HD(l2));
		return FALSE;
	    }
	    if( cmp_graph_rec(dtbl,GET_CONS_TL(l1), GET_CONS_TL(l2)) == FALSE ) {
		fprintf(stderr, "6cmp_graph failed for %p & %p\n", GET_CONS_TL(l1), GET_CONS_TL(l2));
		return FALSE;
	    }
	    return TRUE;
        case LEAF:
            switch( GET_LEAF_TYPE(l1) ) {
                case INT:
		    ai1 = GET_AINT(l1);
		    ai2 = GET_AINT(l2);
		    if( ai1 == ai2 )
			return( TRUE );
		    if( Arbi_cmp(ai1, ai2) == arbi_EQ )
			return(TRUE);
		    fprintf(stderr, "7cmp_graph failed for %p & %p\n", l1, l2);
		    return(FALSE);
                case STRING:
		    if( STREQ(GET_STRING(l1), GET_STRING(l2)) ) {
			return TRUE;
		    } else {
			fprintf(stderr,"8cmp_graph failed for %p & %p\n",l1,l2);
			return FALSE;
		    }
                case BOOL:
		    if( B_Equal(GET_BOOL(l1), GET_BOOL(l2)) ) {
			return TRUE;
		    } else {
			fprintf(stderr,"9cmp_graph failed for %p & %p\n",l1,l2);
			return FALSE;
		    }
                case BEXPR:
		    if( BE_Equal(GET_BEXPR(l1), GET_BEXPR(l2)) ) {
			return TRUE;
		    } else {
			fprintf(stderr, "10cmp_graph failed for %p & %p\n",l1,l2);
			return FALSE;
		    }
		case PRIM_FN:
		    if( GET_PRIM_FN(l1) == GET_PRIM_FN(l2) ) {
			return TRUE;
		    } else {
			fprintf(stderr, "11cmp_graph failed for %p & %p\n",l1,l2);
			return FALSE;
		    }
                default:
		    fprintf(stderr, "12cmp_graph failed for %p & %p\n",l1,l2);
                    return( FALSE );
            }
        default:
            DIE("Fatal error 101. Consult a guru!");
	    fprintf(stderr, "13cmp_graph failed for %p & %p\n",l1,l2);
	    return FALSE; // Dummy
    }
    DIE("Should never occur!");
    fprintf(stderr, "14cmp_graph failed for %p & %p\n",l1,l2);
    return FALSE; // Dummy
}

static bool
limitedPR(g_ptr node, odests fp, int num, bool strict)
{
    g_ptr h, t;
    bool ret = TRUE;

    if( num == 0 )
	return( FALSE );
    if( node == NULL ) {
	FP(fp, "NIL");
	return FALSE;
    }
    if( strict )
	node = traverse_left(node);
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    FP(fp, "(");
	    if( !limitedPR(GET_APPLY_LEFT(node), fp, num-1, strict) ) {
		ret = FALSE;
		FP(fp, "....");
	    }
	    FP(fp, " ");
	    if( !limitedPR(GET_APPLY_RIGHT(node), fp, num-1, strict) ) {
		ret = FALSE;
		FP(fp, "....");
	    }
	    FP(fp, ")");
	    break;
	case LAMBDA_ND:
	    FP(fp, "(lambda %s.", GET_LAMBDA_VAR(node));
	    if( !limitedPR(GET_LAMBDA_BODY(node), fp, num-1, strict) ) {
		ret = FALSE;
		FP(fp, "....");
	    }
	    FP(fp, ")");
	    break;
	case CONS_ND:
	    num = 10*num;
#if 0
	    if( is_a_list(node) ) {
		bool first = TRUE;
		FP(fp, "[");
		h = GET_CONS_HD(node);
		t = GET_CONS_TL(node);
		while( h != NULL && t != NULL ) {
		    if( !first )
			FP(fp, ", ");
		    first = FALSE;
		    if( num < 0 ) {
			FP(fp, "...");
			ret = FALSE;
			break;
		    }
		    num--;
		    if( !limitedPR(h, fp, num-1, strict) ) {
			ret = FALSE;
			FP(fp, "....");
		    }
		    node = t;
		    h = GET_CONS_HD(node);
		    t = GET_CONS_TL(node);
		}
		FP(fp, "]");
		return(ret);
	    }
#endif
	    h = GET_CONS_HD(node);
	    t = GET_CONS_TL(node);
	    if( h == t && h == NULL ) {
		FP(fp, "NIL");
		break;
	    }
	    ASSERT(h != NULL && t != NULL );
	    FP(fp, "(");
	    if( !limitedPR(h, fp, num-1, strict) ) {
		ret = FALSE;
		FP(fp, "....");
	    }
	    FP(fp, ":");
	    if( !limitedPR(t, fp, num-1, strict) ) {
		ret = FALSE;
		FP(fp, "....");
	    }
	    FP(fp, ")");
	    break;
	case LEAF:
	    Print_leaf(node, fp);
	    break;
	default:
	    DIE("Illegal node type");
    }
    return ret;
}

void
DPR(g_ptr node)
{
    fprintf(stderr, "node: %p ", node);
    if( node == NULL ) {
	fprintf(stderr, "NULL\n");
	return;
    }
    fprintf(stderr, " ref_cnt: %d", (int) GET_REFCNT(node));
    if( IS_FORCED(node) ) {
	fprintf(stderr, " forced");
    } else {
	fprintf(stderr, " unforced");
    }
    switch( GET_TYPE(node) ) {
	case APPLY_ND:
	    fprintf(stderr, " APPLY_ND");
	    fprintf(stderr, " l = %p", GET_APPLY_LEFT(node));
	    fprintf(stderr, " r = %p", GET_APPLY_RIGHT(node));
	    fprintf(stderr, "\n");
	    return;

	case LAMBDA_ND:
	    fprintf(stderr, " LAMBDA_ND");
	    fprintf(stderr, " v = %p", GET_LAMBDA_VAR(node));
	    fprintf(stderr, " b = %p", GET_LAMBDA_BODY(node));
	    fprintf(stderr, "\n");
	    return;

	case CONS_ND:
	    fprintf(stderr, " CONS_ND");
	    fprintf(stderr, " h = %p", GET_CONS_HD(node));
	    fprintf(stderr, " t = %p", GET_CONS_TL(node));
	    fprintf(stderr, "\n");
	    return;

	case LEAF:
	    fprintf(stderr, " LEAF\n");
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    fprintf(stderr, "    INT: %s\n",
			    Arbi_ToString(GET_AINT(node),10));
		    return;
		case STRING:
		    fprintf(stderr, "    STRING: |%s|\n", GET_STRING(node));
		    return;
		case BOOL:
		    {
			fprintf(stderr, "    BOOL:");
			formula f = GET_BOOL(node);
			if( f == B_One() ) {
			    fprintf(stderr, " T\n");
			} else 
			if( f == B_Zero() ) {
			    fprintf(stderr, " F\n");
			} else {
			    fprintf(stderr, " <symbolic>\n");
			}
			return;
		    }
		case BEXPR:
		    {
			fprintf(stderr, "    BEXPR:");
			bexpr f = GET_BEXPR(node);
			if( f == BE_One() ) {
			    fprintf(stderr, " bT\n");
			} else 
			if( f == BE_Zero() ) {
			    fprintf(stderr, " bF\n");
			} else {
			    fprintf(stderr, " <symbolic bexpr>\n");
			}
			return;
		    
		    }
		case PRIM_FN:
		    fprintf(stderr, "    PRIM_FN: %d", (int) GET_PRIM_FN(node));
		    if( GET_PRIM_FN(node) == P_FAIL ) {
			fprintf(stderr, " (P_FAIL \"%s\")\n",
					GET_FAIL_STRING(node));
		    } else {
			fprintf(stderr, " (%s)\n", Get_pfn_name(node));
		    }
		    return;
		case VAR:
		    fprintf(stderr, "    VAR: %s (version %ld)\n",
				    GET_VAR(node), GET_VERSION(node));
		    return;
		case USERDEF: {
		    fn_ptr fp = GET_USERDEF(node);
		    fprintf(stderr, "    USERDEF: %s (version %ld)\n",
				    fp->name, GET_VERSION(node));
		    return;
		}
		case EXT_OBJ:
		    fprintf(stderr, "    EXT_OBJ: %ld %p\n",
			    GET_EXT_OBJ_CLASS(node), GET_EXT_OBJ(node));
		    return;
		default:
		    fprintf(stderr, "    UNKNOWN LEAF TYPE\n");
		    return;
	    }
	default:
	    fprintf(stderr, " UNKNOWN TYPE\n");
	    return;
    }
}

void
PR(g_ptr np)
{
    FILE *old_odests_fp = odests_fp;
    odests_fp = stderr;
    if( limitedPR(np, FILE_fp, pr_depth, FALSE) == FALSE )
	FP(FILE_fp, "....");
    FP(FILE_fp, "\n");
    odests_fp = old_odests_fp;
}

static void
print_free_set(pointer key, pointer data)
{
    g_ptr np;
    free_list_ptr fp;
    np = (g_ptr) key;
    fp = (free_list_ptr) data;
    FP(err_fp, "\n-----------------\n");
    PR(np);
    FP(err_fp, "has free variables: { ");
    while( fp ) {
	FP(err_fp, "%s ", fp->var);
	fp = fp->next;
    }
    FP(err_fp, "}\n");
}

void
PFS()
{
    scan_hash(&free_tbl, print_free_set);
}

int
PRsize(g_ptr node)
{
    if( node == NULL )
	return(0);
    if( IS_LEAF(node) )
	return(1);
    if( IS_CONS(node) )
	return( 1+PRsize(GET_CONS_HD(node))+PRsize(GET_CONS_HD(node)) );
    if( IS_APPLY(node) )
	return( 1+PRsize(GET_APPLY_LEFT(node))+PRsize(GET_APPLY_RIGHT(node)) );
    if( IS_LAMBDA(node) )
	return( 1 + PRsize(GET_LAMBDA_BODY(node)) );
    DIE("Should never occur!"); return 0; // Dummy
}

void
PRl(g_ptr np, int limit)
{
    FILE *old_odests_fp = odests_fp;
    odests_fp = stderr;
    int old_limit = pr_depth;
    pr_depth = limit;
    if( limitedPR(np, FILE_fp, pr_depth, FALSE) == FALSE )
      FP(err_fp, "....");
    FP(FILE_fp, "\n");
    pr_depth = old_limit;
    odests_fp = old_odests_fp;
}


int
Print_Nd_type(g_ptr nd)
{
    switch( GET_TYPE(nd) ) {
	case APPLY_ND:
	    FP(err_fp, "APPLY_ND freed: Left=%ld, Right=%ld\n",
			   (long int) GET_APPLY_LEFT(nd),
                           (long int) GET_APPLY_RIGHT(nd));
	    break;
	case LAMBDA_ND:
	    FP(err_fp, "LAMBDA_ND freed: var=%s, body=%ld\n",
			    GET_LAMBDA_VAR(nd), (long int) GET_LAMBDA_BODY(nd));
	    break;
	case CONS_ND:
	    FP(err_fp, "CONS_ND freed: hd=%ld, tl=%ld\n",
			   (long int) GET_CONS_HD(nd),
                           (long int) GET_CONS_TL(nd));
	    break;
	case LEAF:
	    Print_leaf(nd, err_fp);
	    FP(err_fp, "\n");
	    break;
	default:
	    DIE("Impossible");
    }
    DIE("Should never occur!"); return 0; // Dummy
}


static hash_record visited;

static void
set_up_hash()
{
    create_hash(&visited, 100, ptr_hash, ptr_equ);
}

// To make it look like these functions are needed ....
void Dummy()
{
    remove_hash();
    set_up_hash();
    accessible(NULL, NULL);
    is_a_list(NULL);
    refcnt_free_nd(NULL);
}

static void
remove_hash()
{
    dispose_hash(&visited, NULLFCN);
}

static bool
accessible(g_ptr cur, g_ptr ref)
{
    if( cur == ref )
	return( TRUE );
    if( cur == NULL )
	return( FALSE );
    if( find_hash(&visited, (pointer) cur) != NULL )
	return(FALSE);
    insert_hash(&visited, (pointer) cur, (pointer) cur);
    switch( GET_TYPE(cur) ) {
	case APPLY_ND:
	    if(accessible(GET_APPLY_LEFT(cur), ref))
		return( TRUE );
	    return( accessible(GET_APPLY_RIGHT(cur), ref) );
	case CONS_ND:
	    if(accessible(GET_CONS_HD(cur), ref))
		return( TRUE );
	    return( accessible(GET_CONS_TL(cur), ref) );
	    break;
	case LEAF:
	    return( FALSE );
	    break;
	case LAMBDA_ND:
	    DIE("There shouldn't be any lambda nodes left.\n");
	default:
	    DIE("Impossible");
    }
    DIE("Should never occur!"); return FALSE; // Dummy
}

#ifdef CHECK_REF_CNTS
static int  nbr_ref_cnt_checks = 0;

void
check_ref_cnts(g_ptr np)
{
    nbr_ref_cnt_checks++;
    create_hash(&visited, 1000, ptr_hash, ptr_equ);
    if( check_ref_cnt_rec(np)) {
	extern int start_save_graph_after_ref_cnt_checks;
	if( start_save_graph_after_ref_cnt_checks != -1 &&
	    nbr_ref_cnt_checks > start_save_graph_after_ref_cnt_checks )
	{
	    Save_graph("_dummy", "_last_correct_graph_", np);
	}
    }
    dispose_hash(&visited, NULLFCN);
}

static bool
check_ref_cnt_rec(g_ptr np)
{
    if( np == NULL )
	return( TRUE );
    long int cnt;
    if( (cnt = (long int) find_hash(&visited, (pointer) np)) != 0 ) {
	delete_hash(&visited, (pointer) np);
	cnt++;
	insert_hash(&visited, (pointer) np, (pointer) cnt);
	int cur_cnt = GET_REFCNT(np);
	if( cur_cnt != MAX_REF_CNT && (cur_cnt < (cnt-1)) ) {
	    FP(err_fp, "Incorrect refcount on %p (%d < %d)\n",
			    np, cur_cnt, cnt-1);
	    Dbg_stop();
	    return( FALSE );
	}
	return( TRUE );
    }
    insert_hash(&visited, (pointer) np, (pointer) 1);
    switch( GET_TYPE(np) ) {
	case APPLY_ND:
	    if( !check_ref_cnt_rec(GET_APPLY_LEFT(np)) ) {
		return( FALSE );
	    }
	    return( check_ref_cnt_rec(GET_APPLY_RIGHT(np)) );
	case CONS_ND:
	    if( !check_ref_cnt_rec(GET_CONS_HD(np)) ) {
		return( FALSE );
	    }
	    return( check_ref_cnt_rec(GET_CONS_TL(np)) );
	case LEAF:
	    if( IS_REF_VAR(np) ) {
		return( check_ref_cnt_rec(Get_RefVar(GET_REF_VAR(np))) );
	    } 
	    return( TRUE );
	case LAMBDA_ND:
	    return( check_ref_cnt_rec(GET_LAMBDA_BODY(np)) );
	default:
	    DIE("Impossible");
    }
    DIE("Should never occur!");
}
#endif

#endif /* DEBUG */

