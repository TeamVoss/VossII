//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1990			    *
*                                                                   *
*********************************************************************/
/* graph.h -- header for graph.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct old_yyin_rec     *old_yyin_ptr;
typedef struct cl_rec           *cl_ptr;
typedef struct result_rec       *result_ptr;
typedef struct comment_list_rec *comment_list_ptr;
typedef struct var_list_rec     *var_list_ptr;
typedef unsigned long int       ui;
typedef void			(*eval_fun_tp)(g_ptr);
typedef struct ext_fun_rec	*ext_fun_ptr;
typedef struct bexpr_rec	*bexpr;
typedef struct eval_ctx_rec	*eval_ctx_ptr;
typedef struct eval_ctx_rec {
    g_ptr	*old_sp;
    g_ptr	old_root_node;
    jmp_buf	*old_start_envp;
    int		old_call_level;
    int		old_fn_trace_buf_size;
    formula	cur_eval_cond;
} eval_ctx_rec;
typedef struct gmap_info_rec	*gmap_info_ptr;
typedef struct io_rec		*io_ptr;

// These should be in symbol.h, but there is a cross dependency so they're here!
typedef struct fn_rec           *fn_ptr;
typedef struct symb_tbl_rec     *symbol_tbl_ptr;

/* -------- Function prototypes for exported functions -------- */
g_ptr	     Copy_Graph(g_ptr node);
bool	     G_check_for_errors(g_ptr np, int cnt);
g_ptr	     Get_node();
void	     SET_INT(g_ptr np, int i);
void         PUSH_GLOBAL_GC(g_ptr res);
void         POP_GLOBAL_GC(int cnt);
bool	     GC_Protect(g_ptr g);
bool	     GC_Unprotect(g_ptr g);
int          GET_INT(g_ptr node);
void         inc_refcnt_children(g_ptr node);
#if TRACK_FREEING
void         dec_ref_cnt(g_ptr node, string file, int line);
#else
void         dec_ref_cnt(g_ptr node);
#endif
bool         is_fail(g_ptr node);
g_ptr	     traverse_left(g_ptr oroot);
g_ptr	     reduce(g_ptr root, bool first);
g_ptr        force(g_ptr node, bool first);
VOID         G_Init();
g_ptr	     Make_Named_Arg(string name, g_ptr expr);
bool	     Destr_Named_Arg(g_ptr node, string *namep, g_ptr *exprp);
string	     Mk_constructor_name(string constr_name);
g_ptr	     Make_Failure(string msg);
g_ptr        Make_TYPE(string name, int arg_cnt);
g_ptr        Make_arg_list(int cnt, g_ptr expr);
g_ptr        Make_BOOL_leaf(formula f);
g_ptr	     Make_BEXPR_leaf(bexpr f);
g_ptr        Make_INT_leaf(int value);
g_ptr        Make_AINT_leaf(arbi_T value);
g_ptr        Make_STRING_leaf(string s);
g_ptr        Make_VAR_leaf(string s);
g_ptr	     Make_USERDEF_leaf(fn_ptr fp);
g_ptr        Make_NIL();
g_ptr        Make_SINGLETON(g_ptr leaf);
g_ptr        Make_Printf_Primitive(int pr_fn, string pat);
g_ptr        Make_0inp_Primitive(int pr_fn);
g_ptr        Make_1inp_Primitive(int pr_fn, g_ptr arg1);
g_ptr        Make_2inp_Primitive(int pr_fn, g_ptr arg1, g_ptr arg2);
g_ptr        Make_3inp_Primitive(int pr_fn, g_ptr arg1, g_ptr arg2, g_ptr arg3);
g_ptr	     Make_Debug_Primitive(string name, string file, int line);
g_ptr        Make_CONS_ND(g_ptr expr1, g_ptr expr2);
g_ptr        Make_APPL_ND(g_ptr expr1, g_ptr expr2);
g_ptr        Make_Lambda(string var, g_ptr expr);
g_ptr        Make_PAIR_ND(g_ptr fst, g_ptr snd);
g_ptr        Make_arglist(int cnt);
g_ptr        Add_args(int cnt, g_ptr expr);
g_ptr        Remove_Pattern_Matching(g_ptr lhs,g_ptr rhs,string *name,int *cnt);
g_ptr        TrArg(g_ptr node, g_ptr E, bool constructor);
string       Find_new_free_var(g_ptr expr);
cl_ptr       Get_CL_node();
g_ptr        Convert_CL(cl_ptr nd, bool list);
g_ptr        List_reverse(g_ptr cur);
int          List_length(g_ptr l);
g_ptr        List_element(g_ptr l, unint index);
VOID         Print_Expr(g_ptr node, odests fp);
VOID         Install_PrinterFn(result_ptr res);
VOID         Print_Result(result_ptr res, odests fp, bool print);
void         Mark(g_ptr node);
#if TRACK_FREEING
void         Do_garbage_collect(string file, int line);
#else
void         Do_garbage_collect();
#endif
result_ptr   Compile(symbol_tbl_ptr stbl, g_ptr node, typeExp_ptr type,
		     bool delayed, bool extract_arg_names);
g_ptr	     Execute_fl_code(const string function, ...);
void	     Free_result_ptr(result_ptr rp);
FILE *       Return_to_old_fid();
void         Debug_change_string(char *new_name, g_ptr n);
int	     Read_from_file(string name, bool message, bool remove_after);
var_list_ptr Add_Var_to_Var_List(string var, typeExp_ptr type_hint,
				 var_list_ptr vlp);
void         Print_leaf(g_ptr node, odests fp);
bool         Can_Fail_Pat_Match(g_ptr node);
void         AddComment(string s);
g_ptr	     Gen_map(g_ptr (*fun)(g_ptr), g_ptr node, bool read_only);
g_ptr	     Gen_map2(string parent_op, g_ptr (*fun)(g_ptr,g_ptr),
		      g_ptr l, g_ptr r, bool read_only);
formula	     Is_equal(g_ptr l1, g_ptr l2, bool identical);
string	     Get_pfn_name(g_ptr np, bool verbose_debug);
string	     Get_stack_trace(int max_entries);
g_ptr	     Get_fl_stack_trace();
void	     DPR(g_ptr node);
void	     Record_eval_context(eval_ctx_ptr ctx);
void	     Restore_eval_context(eval_ctx_ptr ctx);
void	     Reset_eval_context();
g_ptr	     Eval(g_ptr redex);
g_ptr	     Append_string_to_tail(g_ptr tail, string name);
void	     Get_Vars(buffer *bp, g_ptr node);
g_ptr	     Reflect_expr(g_ptr node);
g_ptr	     Cephalopde_Reflect_expr(g_ptr node);
formula	     Get_cur_eval_cond();
unsigned int Graph_hash(pointer np, unsigned int n);
bool	     Graph_equ(pointer p1, pointer p2);
void	     Emit_profile_data();

#ifdef DEBUG
int	     PRsize(g_ptr node);
void	     PRl(g_ptr np, int limit);
void	     PR(g_ptr np);
bool	     cmp_graph(g_ptr l1, g_ptr l2);
void	     check_ref_cnts();
#endif

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef GRAPH_H
#define GRAPH_H
#include "fl.h"  /* Global data types and include files               */

/* Memory allocation constants */
#define START_SIZE      65000
#define STACK_SIZE      1000000

/* Compilation cutoff */
#define LARGE_CUT_OFF   100000

/* Cacheing limits */
#define MAX_NBR_ARGS_TO_CACHE      10

typedef struct g_rec {
        ui      L;
        ui      R;
#ifdef TRACK_FREEING
	ui	oldL;
	ui	oldR;
	string	file;
	int	line;
#endif
} g_rec;

#ifdef TRACK_FREEING
#define RESET_G_PTR(np)		{ (np)->L = 0; (np)->R = 0;		\
				  (np)->oldL = 0; (np)->oldR = 0;	\
				  (np)->file = 0; (np)->line = -1; }
#else
#define RESET_G_PTR(np)		{ (np)->L = 0; (np)->R = 0; }
#endif
/* Types of nodes */
#define LAMBDA_ND               0
#define APPLY_ND                1
#define CONS_ND                 2
#define LEAF                    3

#define GET_B_LEFT(np)          (((ui)((np)->L)) & ~0x3)
#define SET_B_LEFT(np,p)        ((np)->L = (ui)((((ui)((np)->L))&0x3) | \
                                                     (((ui) p)&~0x3)))

#define GET_LEFT(np)            (((ui)((np)->L)) & ~0x7)
#define SET_LEFT(np,p)          ((np)->L = (ui)((((ui)((np)->L))&0x7) | \
                                                     (((ui) p)&~0x7)))
#define GET_RIGHT(np)           (((ui)((np)->R)) & ~0x3)
#define SET_RIGHT(np,p)         ((np)->R = (ui)((((ui)((np)->R))&0x3) | \
                                                     (((ui) p)&~0x3)))


#ifndef DONT_USE_FORCING_MARKS
#define IS_FORCED(np)           ((((ui) ((np)->L)) & 0x4) == 0x4)
#define GET_FORCED(np)          (((ui) ((np)->L)) & 0x4)
#define SET_FORCED(np,f)        ((np)->L=(ui)((((ui)((np)->L))& ~0x4)|(f)))
#define MAKE_FORCED(np)         ((np)->L = (ui)(((ui) ((np)->L)) | 0x4))
#define MAKE_UNFORCED(np)       ((np)->L = (ui)(((ui) ((np)->L)) & ~0x4))
#else
#define IS_FORCED(np)           0
#define GET_FORCED(np)          0
#define SET_FORCED(np,f)        
#define MAKE_FORCED(np)         
#define MAKE_UNFORCED(np)       
#endif

#define GET_TYPE(np)            (((ui) ((np)->L)) & 0x3)
#define SET_TYPE(np,tag)        ((np)->L = (ui) ((((ui) ((np)->L))&~0x3)| \
                                                             (((ui)(tag))&0x3)))
#define MAX_REF_CNT		3
#define GET_REFCNT(np)          (((ui) ((np)->R)) & 0x3)
#define SET_REFCNT(np,m)        ((np)->R = (ui)((((ui)((np)->R))&~0x3) | \
                                                        (((ui) m)&0x3)))
/* REF_CNT goes 00 -> 01 -> 11 (and gets stuck at 11) */
#define INC_REFCNT(np)          ((np)->R = (ui)(((((ui)((np)->R))<<1)&0x2)\
                                                       | 0x1 | ((ui)((np)->R))))

#define OVERWRITE(red,np)       {                                       \
                                    ui frc;                             \
                                    ui cr = GET_REFCNT(red);            \
                                    frc = GET_FORCED(red);              \
                                    red->L = (np)->L;                   \
                                    red->R = (np)->R;                   \
                                    SET_REFCNT(red, cr);                \
                                    SET_FORCED(red,frc);                \
                                    inc_refcnt_children(red);           \
                                }

#define GET_MARK(np)            ((((ui) ((np)->R)) & 0x2)>>1)
#define SET_MARK(np,m)          ((np)->R = (ui)((((ui)((np)->R))&~0x2) | \
                                                        ((((ui) m)&0x1)<<1)))

#define IS_LAMBDA(np)           (GET_TYPE(np) == LAMBDA_ND)
#define M_GET_LAMBDA_VAR(np)    ((string) GET_RIGHT(((g_ptr) GET_LEFT(np))))
#define GET_LAMBDA_VAR_LEAF(np) ((g_ptr) GET_LEFT(np))
#define SET_LAMBDA_VAR(np, v)   SET_LEFT(np, Make_VAR_leaf(v))
#define M_GET_LAMBDA_BODY(np)   ((g_ptr) GET_RIGHT(np))
#define SET_LAMBDA_BODY(np, v)  SET_RIGHT(np, (ui) v)

#define IS_APPLY(np)            (GET_TYPE(np) == APPLY_ND)
#define M_GET_APPLY_LEFT(np)    ((g_ptr) GET_LEFT(np))
#define SET_APPLY_LEFT(np, v)   SET_LEFT(np, (ui) v)
#define M_GET_APPLY_RIGHT(np)   ((g_ptr) GET_RIGHT(np))
#define SET_APPLY_RIGHT(np, v)  SET_RIGHT(np, (ui) v)

#define IS_CONS(np)             (GET_TYPE(np) == CONS_ND)
#define NIL                     NULL
#define M_GET_CONS_HD(np)       ((g_ptr) GET_LEFT(np))
#define SET_CONS_HD(np, v)      SET_LEFT(np, (ui) v)
#define M_GET_CONS_TL(np)       ((g_ptr) GET_RIGHT(np))
#define SET_CONS_TL(np, v)      SET_RIGHT(np, (ui) v)
#define IS_NIL(p)               (IS_CONS(p)				       \
                                && (M_GET_CONS_HD(p) == NULL)		       \
                                && (M_GET_CONS_TL(p) == NULL))

#define FOR_CONS(list,li,data)                                                 \
    for(li = list;                                                             \
        !IS_NIL(li)&&(data = M_GET_CONS_HD(li), TRUE);                         \
        li = M_GET_CONS_TL(li))

#define APPEND1(tail,np)                                                       \
    SET_CONS_HD((tail),(np));	                                               \
	SET_CONS_TL((tail), Make_NIL());                                       \
	(tail) = GET_CONS_TL((tail));

#define APPENDL(tail,nlp)                                                      \
    { g_ptr tmp, np; FOR_CONS(nlp, tmp, np) { APPEND1(tail, np); } }

/* Leaf types */
#define INT                     0
#define STRING                  1
#define BOOL                    2
#define BEXPR                   3
#define EXT_OBJ                 4	/* External (C) object */
#define PRIM_FN                 5       /* Includes the combinators */
#define VAR                     6       /* Only used during parsing */
#define USERDEF                 7       /* Only used during compilation */

#define IS_LEAF(np)             (GET_TYPE(np) == LEAF)
#define GET_LEAF_TYPE(np)       ((((ui) GET_LEFT(np))>>3)&0x7)
#define SET_LEAF_TYPE(np,t)     ((np)->L = (ui)((((ui)((np)->L))&~0x38) | \
                                                     ((((ui) t)&0x7)<<3)))

#define IS_INT(np)              (GET_LEAF_TYPE(np) == INT)
#define M_GET_AINT(np)          (((arbi_T) GET_RIGHT(np)))
#define SET_AINT(np,i)          SET_RIGHT(np, (ui) (i))

#define IS_STRING(np)           (GET_LEAF_TYPE(np) == STRING)
#define M_GET_STRING(np)        ((string) GET_RIGHT(np))
#define SET_STRING(np,s)        SET_RIGHT(np, (ui) s)

#define IS_BEXPR(np)            (GET_LEAF_TYPE(np) == BEXPR)
#define M_GET_BEXPR(np)		((bexpr) GET_RIGHT(np))
#define SET_BEXPR(np,s)		 SET_RIGHT(np, (ui) (s))

#define IS_BOOL(np)             (GET_LEAF_TYPE(np) == BOOL)
#define M_GET_BOOL(np)          ((formula) (GET_RIGHT(np)>>2))
#define SET_BOOL(np,s)          SET_RIGHT(np, ((ui) (((ui) s)<<2)))


#define IS_VAR(np)              (GET_LEAF_TYPE(np) == VAR)
#define IS_LEAF_VAR(np)         (IS_LEAF(np) &&  IS_VAR(np))
#define GET_VAR(np)             ((string) GET_RIGHT(np))
#define SET_VAR(np,s)           SET_RIGHT(np, (ui) s)
#define GET_VERSION(np)         ((GET_LEFT(np) >>6)&0x3ff)
#define SET_VERSION(np,f)       ((np)->L = \
                                      (ui)((((ui)((np)->L))&0xffff003f) | \
                                      (((ui) f)<<6)))


#define IS_EXT_OBJ(np)          (IS_LEAF(np) && (GET_LEAF_TYPE(np) == EXT_OBJ))
#define GET_EXT_OBJ(np)         ((pointer) GET_RIGHT(np))
#define SET_EXT_OBJ(np,o)       SET_RIGHT(np,o)

// Use the same SET_VERSION/GET_VERSION as for variables.
#define IS_USERDEF(np)          (IS_LEAF(np) && (GET_LEAF_TYPE(np) == USERDEF))
#define GET_USERDEF(np)         ((pointer) GET_RIGHT(np))
#define SET_USERDEF(np,o)       SET_RIGHT(np,o)

// Maximum 1024 ext_obj classes
#define GET_EXT_OBJ_CLASS(np)	((GET_LEFT(np) >>6)&0x3ff)
#define SET_EXT_OBJ_CLASS(np,c)	((np)->L = \
                                      (ui)((((ui)((np)->L))&~0xffc0) | \
                                      (((ui) c)<<6)))

#define IS_PRIM_FN(np)          (GET_LEAF_TYPE(np) == PRIM_FN)
#define GET_PRIM_FN(np)         ((GET_LEFT(np) >>6)&0x3ff)
#define SET_PRIM_FN(np,f)       ((np)->L = \
                                      (ui)((((ui)((np)->L))&0xffff003f) | \
                                      (((ui) f)<<6)))

#define IS_P_Y(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_Y)
#define IS_P_CACHE(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_CACHE)
#define IS_P_STRICT_ARGS(np)	(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_STRICT_ARGS)
#define IS_P_ERROR(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_ERROR)
#define IS_P_PCATCH(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_PCATCH)
#define IS_REF_VAR(np)          (IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_REF_VAR)
#define IS_FAILURE(np)          (IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_FAIL)

#define IS_VOID(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_VOID)

#define IS_DEBUG(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_DEBUG)

#define IS_NAMED_ARG(np)	(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_NAMED_ARG)

#define IS_MK_REF_VAR(np)	(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_MK_REF_VAR)

#define IS_UNQUOTE(np)		(IS_LEAF(np) &&				    \
				 IS_PRIM_FN(np) &&			    \
				 GET_PRIM_FN(np) == P_UNQUOTE)


#define M_GET_PRINTF_STRING(np) ((string) GET_RIGHT(np))
#define SET_PRINTF_STRING(np,s) SET_RIGHT(np, (ui) s)

#define GET_CACHE_TBL(np)       (((ui) GET_RIGHT(np))>>2)
#define SET_CACHE_TBL(np,s)     SET_RIGHT(np, (((ui) s)<<2))

#define GET_CKT_NBR(np)         (((ui) GET_RIGHT(np))>>2)
#define SET_CKT_NBR(np,s)       SET_RIGHT(np, (((ui) s)<<2))

#define M_GET_FAIL_STRING(np)   ((string) GET_RIGHT(np))
#define SET_FAIL_STRING(np,s)   SET_RIGHT(np, (ui) s)

#define M_GET_DEBUG_STRING(np)   ((string) GET_RIGHT(np))
#define SET_DEBUG_STRING(np,s)   SET_RIGHT(np, (ui) s)

#define GET_FILE_IO_PTR(np)     ((io_ptr) GET_RIGHT(np))
#define SET_FILE_IO_PTR(np,p)   SET_RIGHT(np, (ui) p)

#define GET_REF_VAR(np)         (((ui) GET_RIGHT(np))>>2)
#define SET_REF_VAR(np,s)       SET_RIGHT(np, (((ui) s)<<2))

#define IS_EXTAPI_FN(np)	(GET_PRIM_FN(np) == P_EXTAPI_FN)
#define GET_EXTAPI_FN(np)       ((GET_LEFT(np) >> 16) & 0xffffffff)
#define SET_EXTAPI_FN(np,f)     ((np)->L = \
                                   (ui)((((ui)((np)->L))&0xffff00000000ffff) | \
                                   (((ui) f)<<16)))

#define GET_LINE_NBR(np)        ((int) (GET_LEFT(np) >>48))
#define SET_LINE_NBR(np,ln)     ((np)->L = \
                                      (ui)((((ui)((np)->L))&0xffffffffffff) | \
                                      (((ui) ln)<<48)))

#define GET_LAMBDA_LINE_NBR(np)	    GET_LINE_NBR(GET_LAMBDA_VAR_LEAF(np))
#define SET_LAMBDA_LINE_NBR(np,ln)  SET_LINE_NBR(GET_LAMBDA_VAR_LEAF(np),ln)

/* Primitive functions */
#define P_S		    0       /* Combinators */
#define P_K		    1
#define P_I		    2
#define P_C		    3
#define P_B		    4
#define P_Y		    5
#define P_HEAD		    6       /* Primitive functions */
#define P_TAIL		    7
#define P_CONS		    8
#define P_PLUS		    9
#define P_MINUS		    10
#define P_TIMES		    15
#define P_DIVIDE	    16
#define P_COND		    18
#define P_LESS		    19
#define P_LEQ		    20
#define P_GREATER	    21
#define P_GEQ		    22
#define P_EQUAL		    23
#define P_NOT_EQUAL	    24
#define P_VAR		    25
#define P_CAT		    26
#define P_EMPTY		    27
#define P_LOAD		    28
#define P_ERROR		    29
#define P_SEQ		    30
#define P_SPRIME	    31
#define P_CPRIME	    32
#define P_BSTAR		    33
#define P_HELP		    34
#define P_GET_MATCHING_FNS  35
#define P_FORALL	    36
#define P_THEREIS	    37
#define P_PRINT		    38
#define P_DEBUG		    39
#define P_IDENTICAL	    40
#define P_STRING_HD	    41
#define P_STRING_TL	    42
#define P_FAIL		    43
#define P_IS_CONS	    44
#define P_CATCH		    45
#define P_SUC		    46
#define P_CONSTR_EQ	    47
#define P_QUANT_FORALL	    48
#define P_QUANT_THEREIS	    49
#define P_PFAIL		    50
#define P_PCATCH	    51
#define P_ORD		    52
#define P_CHR		    53
#define P_TUPLE		    54
#define P_FST		    55
#define P_SND		    56
#define P_EXPLODE	    58
#define P_IMPLODE	    59
#define P_DEPENDS	    60
#define P_BOOL2STR	    61
#define P_INT2STR	    62
#define P_TIME		    63
#define P_BDD_SIZE	    64
#define P_SUBSTITUTE	    65	// Not used anymore
#define P_LOAD_EXE	    66	// Not used anymore
#define P_PRINT_STE	    68	// Not used anymore
#define P_BPRIME	    69
#define P_FANIN_LIST	    70	// Not used anymore
#define P_NODES		    71	// Not used anymore
#define P_PROFILE	    72
#define P_GET_NODE_VAL	    73	// Not used anymore
#define P_FANOUT_LIST	    74	// Not used anymore
#define P_GET_DELAYS	    75	// Not used anymore
#define P_TRACE		    76	// Not used anymore
#define P_SAVE_FSM	    77	// Not used anymore
#define P_BDD_SAVE	    78
#define P_BDD_LOAD	    79
#define P_BDD_REORDER	    80
#define P_RELPROD_THEREIS   81
#define P_RELPROD_FORALL    82
#define P_RVAR		    83
#define P_VAR_ORDER	    84
#define P_SYSTEM	    85
#define P_EVAL		    86
#define P_GCD		    87
#define P_LP		    88	// Not used anymore
#define P_CACHE		    89
#define P_IS_TUPLE	    90
#define P_STRICT_CONS	    91
#define P_STRICT_TUPLE	    92
#define P_FSM_OBJECT	    93	// Not used anymore
#define P_IS_STABLE	    94	// Not used anymore
#define P_MK_REF_VAR	    95
#define P_DEREFERENCE	    96
#define P_UPDATE_RVAR	    97
#define P_REF_VAR	    98
#define P_WRITE_TO_FILE	    99
#define P_BVAND		    100
#define P_BVOR		    101
#define P_BVXOR		    102
#define P_BVNOT		    103
#define P_GET_EXCIT	    104	// Not used anymore
#define P_TCL_EVAL	    105
#define P_FSEQ		    106
#define P_NSEQ		    107
#define P_PRINTF	    108
#define P_SPRINTF	    109
#define P_EPRINTF	    110
#define P_FPRINTF	    111
#define P_FILEFP	    112
#define P_FOPEN		    113
#define P_FCLOSE	    114
#define P_FFLUSH	    115
#define P_GEN_CATCH	    116
#define P_GET_VOSSRC	    117
#define P_UPDATE_VOSSRC	    118
#define P_EXTAPI_FN	    119
#define P_SAVE_GRAPH	    120
#define P_LOAD_GRAPH	    121
#define P_EXIT		    122
#define P_THEN		    123
#define P_SSCANF	    124
#define P_UNTYPE	    125
#define P_UNQUOTE	    126
#define P_LOAD_PLUGIN	    127
#define P_STRICT_ARGS	    128
#define P_NAMED_ARG	    129

#define P_VOID          1022

#define P_START_SUPER_COMB 200

#define P_ILLEGAL       1023



#define MAKE_REDEX_APPL_ND(g,l,r)   {					    \
					SET_TYPE((g),APPLY_ND);		    \
					SET_APPLY_LEFT((g),(l));	    \
					SET_APPLY_RIGHT((g),(r));	    \
				    }

#define MAKE_REDEX_CONS_ND(g,h,t)   {					    \
					SET_TYPE((g),CONS_ND);		    \
					SET_CONS_HD((g),(h));	    \
					SET_CONS_TL((g),(t));	    \
				    }

#define MAKE_REDEX_LAMBDA(g,v,b)   {					    \
					SET_TYPE((g),LAMBDA_ND);	    \
					SET_LAMBDA_VAR((g),(v));	    \
					SET_LAMBDA_BODY((g),(b));	    \
				    }

#define MAKE_REDEX_PAIR(g,f,s)   {					    \
					SET_TYPE((g),CONS_ND);		    \
					SET_CONS_HD((g),(f));	    \
					SET_CONS_TL((g),(s));	    \
				    }

#define MAKE_REDEX_NIL(g)	    MAKE_REDEX_CONS_ND(g, NULL, NULL)

#define MAKE_REDEX_VOID(redex)	    { OVERWRITE(redex, void_nd); }

#define MAKE_REDEX_BOOL(redex,b)    {					    \
					SET_TYPE(redex,LEAF);		    \
					SET_LEAF_TYPE(redex,BOOL);	    \
					SET_BOOL(redex,(b));		    \
					MAKE_FORCED(redex);		    \
				    }

#define MAKE_REDEX_BEXPR(redex,b)   {					    \
					SET_TYPE(redex,LEAF);		    \
					SET_LEAF_TYPE(redex,BEXPR);	    \
					SET_BEXPR(redex,(b));		    \
					MAKE_FORCED(redex);		    \
				    }

#define MAKE_REDEX_AINT(redex,a)    {					    \
					SET_TYPE(redex,LEAF);		    \
					SET_LEAF_TYPE(redex,INT);	    \
					SET_AINT(redex,(a));		    \
					MAKE_FORCED(redex);		    \
				    }

#define MAKE_REDEX_INT(redex,i)	    {					    \
					SET_TYPE(redex,LEAF);		    \
					SET_LEAF_TYPE(redex,INT);	    \
					SET_INT(redex,(i));		    \
					MAKE_FORCED(redex);		    \
				    }

#define MAKE_REDEX_STRING(redex,s)  {					    \
					SET_TYPE(redex,LEAF);		    \
					SET_LEAF_TYPE(redex,STRING);	    \
					SET_STRING(redex,(s));		    \
					MAKE_FORCED(redex);		    \
				    }

#define MAKE_REDEX_VAR(redex,s)	    {					    \
					SET_TYPE(redex,LEAF);		    \
					SET_LEAF_TYPE(redex,VAR);	    \
					SET_VAR(redex,(s));		    \
				    }

#define MAKE_REDEX_USERDEF(redex,fp)   {				    \
		    SET_TYPE(redex, LEAF);				    \
		    SET_LEAF_TYPE(redex, USERDEF);			    \
		    SET_USERDEF(redex, (fp));				    \
		    SET_VERSION(redex, 0);				    \
	    }

#define MAKE_REDEX_PRIM_FN(redex,pfn)   {				    \
		    SET_TYPE(redex, LEAF);				    \
		    SET_LEAF_TYPE(redex, PRIM_FN);			    \
		    SET_PRIM_FN(redex, pfn);				    \
	    }

#define MAKE_REDEX_EXTAPI(redex,xfun)   {				    \
		    SET_TYPE(redex, LEAF);				    \
		    SET_LEAF_TYPE(redex, PRIM_FN);			    \
		    SET_PRIM_FN(redex, P_EXTAPI_FN);			    \
		    SET_EXTAPI_FN(redex, xfun);				    \
	    }

#define MAKE_REDEX_EXT_OBJ(redex,class, p)   {				    \
		    SET_TYPE(redex, LEAF);				    \
		    SET_LEAF_TYPE(redex, EXT_OBJ);			    \
		    SET_EXT_OBJ_CLASS(redex, class);			    \
		    SET_EXT_OBJ(redex, p);				    \
	    }

#define MAKE_REDEX_FAILURE(redex,msg)   {				    \
		    SET_TYPE(redex, LEAF);				    \
		    SET_LEAF_TYPE(redex, PRIM_FN);			    \
		    SET_PRIM_FN(redex, P_FAIL);				    \
		    string smsg = wastrsave(&strings, (msg));		    \
		    extern char FailBuf[4096];				    \
		    sprintf(FailBuf, "%s", smsg);			    \
		    SET_FAIL_STRING(redex, smsg);			    \
	    }


#define MAKE_REDEX_REF_VAR(redex,ckt)   {				    \
		    SET_TYPE(redex, LEAF);				    \
		    SET_LEAF_TYPE(redex, PRIM_FN);			    \
		    SET_PRIM_FN(redex, P_REF_VAR);			    \
		    SET_CKT_NBR(redex, ckt);				    \
	    }

#define EXTRACT_1_ARG(np, a1)	a1 = GET_APPLY_RIGHT(np);

#define EXTRACT_2_ARGS(np, a1, a2)  {				    \
					g_ptr t = (np);		    \
					a2 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a1 = GET_APPLY_RIGHT(t);    \
				    }

#define EXTRACT_3_ARGS(np,a1,a2,a3)  {				    \
					g_ptr t = (np);		    \
					a3 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a2 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a1 = GET_APPLY_RIGHT(t);    \
				    }

#define EXTRACT_4_ARGS(np,a1,a2,a3,a4)  {			    \
					g_ptr t = (np);		    \
					a4 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a3 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a2 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a1 = GET_APPLY_RIGHT(t);    \
				    }

#define EXTRACT_5_ARGS(np,a1,a2,a3,a4,a5)  {			    \
					g_ptr t = (np);		    \
					a5 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a4 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a3 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a2 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a1 = GET_APPLY_RIGHT(t);    \
				    }

#define EXTRACT_6_ARGS(np,a1,a2,a3,a4,a5,a6)  {			    \
					g_ptr t = (np);		    \
					a6 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a5 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a4 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a3 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a2 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a1 = GET_APPLY_RIGHT(t);    \
				    }

#define EXTRACT_7_ARGS(np,a1,a2,a3,a4,a5,a6,a7)  {		    \
					g_ptr t = (np);		    \
					a7 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a6 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a5 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a4 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a3 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a2 = GET_APPLY_RIGHT(t);    \
					t = GET_APPLY_LEFT(t);	    \
					a1 = GET_APPLY_RIGHT(t);    \
				    }

typedef enum {mk_tuple_tp, mk_list_tp, no_tp}   constr_tp;

typedef struct cl_rec {
        g_ptr           expr;
        constr_tp       op;
        cl_ptr          next;
} cl_rec;

typedef struct ext_fun_rec {
	string	    name;
        int         arg_cnt;
        string      strict_args;
        typeExp_ptr type;
        eval_fun_tp fun;
} ext_fun_rec;

typedef struct ext_obj_rec *ext_obj_ptr;

typedef struct ext_obj_rec {
	string	    name;
        int         class;
        typeExp_ptr type;
	void	    (*mark_fn)(pointer p);
	void	    (*sweep_fn)();
	void	    (*save_fn)(FILE *fp, pointer p);
	pointer	    (*load_fn)(FILE *fp);
	string	    (*obj2string)(pointer p);
	formula	    (*eq_fn)(pointer a, pointer b, bool identical);
	pointer	    (*gmap_fn)(gmap_info_ptr ip, pointer a);
	pointer	    (*gmap2_fn)(gmap_info_ptr ip, pointer a, pointer b);
	int	    (*sha256_fn)(int *g_cntp, hash_record *g_tblp,
				 SHA256_ptr sha, pointer obj);
} ext_obj_rec;


typedef struct result_rec {
	string		signature;
        g_ptr		expr_init;
        g_ptr		expr_comb;
        g_ptr		expr;
        impl_arg_ptr	implicit_args;
        g_ptr		super_comb;
        typeExp_ptr	type;
	arg_names_ptr	arg_names;
} result_rec;

typedef struct comment_list_rec {
        string              comment;
	int		    line;
        comment_list_ptr    next;
} comment_list_rec;

typedef struct var_list_rec {
        string          name;
	typeExp_ptr	type_hint;
        var_list_ptr    next;
} var_list_rec;

typedef struct free_list_rec *free_list_ptr;
typedef struct free_list_rec {
        string          var;
        free_list_ptr   next;
} free_list_rec;

typedef struct res_rec {
        g_ptr           res;
        free_list_ptr   needed;
} res_rec;

typedef struct gmap2_rec	*gmap2_ptr;
typedef struct gmap2_rec {
    g_ptr	l;
    g_ptr	r;
} gmap2_rec;

typedef struct gmap_info_rec {
	bool		read_only;
	string		parent_op;
	hash_record	gen_map_tbl;
	rec_mgr		gmap2_rec_mgr;
	union {
	    g_ptr	(*leaf_fun)(g_ptr);
	    g_ptr	(*leaf_fun2)(g_ptr,g_ptr);
	}		u;
} gmap_info_rec;

typedef struct profile_data_rec	    *profile_data_ptr;
typedef struct profile_data_rec {
	string		fun_name;
	bool		builtin;
	int		recursion_depth;
	int		invocation_cnt;
	double		time_used;
} profile_data_rec;

typedef struct prof_timing_rec	    *prof_timing_ptr;
typedef struct prof_timing_rec {
    profile_data_ptr	fun_data;
    struct timespec	timepoint;
} prof_timing_rec;

#if defined(PARANOIA) && !defined(FL_PLUGIN)
string		f_GET_LAMBDA_VAR(g_ptr node);
g_ptr		f_GET_LAMBDA_BODY(g_ptr node);
g_ptr		f_GET_APPLY_LEFT(g_ptr node);
g_ptr		f_GET_APPLY_RIGHT(g_ptr node);
g_ptr		f_GET_CONS_HD(g_ptr node);
g_ptr		f_GET_CONS_TL(g_ptr node);
arbi_T		f_GET_AINT(g_ptr node);
string		f_GET_STRING(g_ptr node);
string		f_GET_PRINTF_STRING(g_ptr node);
string		f_GET_DEBUG_STRING(g_ptr node);
string		f_GET_FAIL_STRING(g_ptr node);
formula		f_GET_BOOL(g_ptr node);
bexpr		f_GET_BEXPR(g_ptr node);
#define GET_LAMBDA_VAR(np)      f_GET_LAMBDA_VAR(np)
#define GET_LAMBDA_BODY(np)     f_GET_LAMBDA_BODY(np)
#define GET_APPLY_LEFT(np)      f_GET_APPLY_LEFT(np)
#define GET_APPLY_RIGHT(np)     f_GET_APPLY_RIGHT(np)
#define GET_CONS_HD(np)         f_GET_CONS_HD(np)
#define GET_CONS_TL(np)         f_GET_CONS_TL(np)
#define GET_AINT(np)            f_GET_AINT(np)
#define GET_STRING(np)          f_GET_STRING(np)
#define GET_PRINTF_STRING(np)   f_GET_PRINTF_STRING(np)
#define GET_DEBUG_STRING(np)	f_GET_DEBUG_STRING(np)
#define GET_FAIL_STRING(np)	f_GET_FAIL_STRING(np)
#define GET_BOOL(np)            f_GET_BOOL(np)
#define GET_BEXPR(np)           f_GET_BEXPR(np)

#else

#define GET_LAMBDA_VAR(np)      M_GET_LAMBDA_VAR(np)
#define GET_LAMBDA_BODY(np)     M_GET_LAMBDA_BODY(np)
#define GET_APPLY_LEFT(np)      M_GET_APPLY_LEFT(np)
#define GET_APPLY_RIGHT(np)     M_GET_APPLY_RIGHT(np)
#define GET_CONS_HD(np)         M_GET_CONS_HD(np)
#define GET_CONS_TL(np)         M_GET_CONS_TL(np)
#define GET_AINT(np)            M_GET_AINT(np)
#define GET_STRING(np)          M_GET_STRING(np)
#define GET_PRINTF_STRING(np)   M_GET_PRINTF_STRING(np)
#define GET_DEBUG_STRING(np)    M_GET_DEBUG_STRING(np)
#define GET_FAIL_STRING(np)	M_GET_FAIL_STRING(np)
#define GET_BOOL(np)            M_GET_BOOL(np)
#define GET_BEXPR(np)           M_GET_BEXPR(np)

#endif /* defined(PARANOIA) && !defined(FL_PLUGIN) */

#define GET_FST(np)	    GET_CONS_HD(np)
#define GET_SND(np)	    GET_CONS_TL(np)

#define GET_HD(np)	    GET_CONS_HD(np)
#define GET_TL(np)	    GET_CONS_TL(np)

#endif /* GRAPH_H */
#endif /* EXPORT_FORWARD_DECL */
