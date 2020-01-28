/********************************************************************
*                                                                   *
*     Copyright (C) 1990 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* typecheck.h -- header for typecheck.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct typeList_rec	*typeList_ptr;

/* -------- Function prototypes for exported functions -------- */
void		TC_Init();
bool            Is_Void(typeExp_ptr type);
typeExp_ptr	TypeCheck(g_ptr *ondp, bool delayed, impl_arg_ptr *impl_argsp);
void		Reset_TypeChecker();
typeExp_ptr	Get_common_type(typeExp_ptr default_type, oll_ptr alts);
fn_ptr		Find_Overload_Choice(fn_ptr fn, typeExp_ptr type);
void		MoveTypeHint(g_ptr from, g_ptr to, bool function);
void		TypeHint(g_ptr node, typeExp_ptr type);
typeExp_ptr	GetTypeHint(g_ptr nd);
int		Printf_arity(ui pfn, string pat);
void		Print_Full_Type(fn_ptr fn, odests fp, bool newline, bool reset);
void		Print_Type(typeExp_ptr type, odests fp, bool newline,
			   bool reset);
string		Type2String(typeExp_ptr type);
typeExp_ptr	Get_Type(string name, typeList_ptr tvars, int insert_missing);
typeExp_ptr	Fix_Types(typeExp_ptr *constructor_type,
			  typeExp_ptr concrete_type);
typeExp_ptr	GLmake_arrow(typeExp_ptr from, typeExp_ptr to);
typeExp_ptr	GLmake_tuple(typeExp_ptr fst, typeExp_ptr snd);
typeExp_ptr	GLmake_list(typeExp_ptr type);
typeExp_ptr	GLmake_bool();
typeExp_ptr	GLmake_bexpr();
typeExp_ptr	GLmake_int();
typeExp_ptr	GLmake_string();
typeExp_ptr	GLmake_fsm();
typeExp_ptr	GLmake_fp();
typeExp_ptr	GLmake_void();
typeExp_ptr	GLmake_ref(typeExp_ptr type);
typeExp_ptr	GLmake_tbl(typeExp_ptr key_type, typeExp_ptr data_type);
typeExp_ptr	GLmake_fail();
typeExp_ptr	GLnew_tVar();
typeList_ptr	Append_Type_List(typeList_ptr list, typeExp_ptr type);
int		Length_TypeList(typeList_ptr l);
void		Unify_Constr(typeExp_ptr type, typeList_ptr type_list);
typeExp_ptr	Get_Real_Type(typeExp_ptr type);
bool		Is_Printable_Type(typeExp_ptr type);
typeExp_ptr	PrintConvFn(typeExp_ptr type);
void		New_Type_Abbrev(string name, typeExp_ptr type);
bool		Is_List(typeExp_ptr type);
bool		Is_Tuple(typeExp_ptr type);
typeExp_ptr	Get_List_type(typeExp_ptr type);
typeExp_ptr	Get_Fst_Type(typeExp_ptr type);
typeExp_ptr	Get_Snd_Type(typeExp_ptr type);
unsigned int	Type_hash(pointer tp, unsigned int n);
bool		Type_eq(pointer t1, pointer t2);
bool		Forward_Declare_Ok(typeExp_ptr declare_type,
				   typeExp_ptr actual_type);
bool		Check_no_undef_types(typeExp_ptr type);
bool		Is_monomorphic(typeExp_ptr type);
#ifdef DEBUG
void		PT(typeExp_ptr type);
#endif

#define TP_DONT_INSERT		    0
#define TP_INSERT_PLACE_HOLDER	    1
#define TP_INSERT_FULL_TYPE	    2

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef TYPECHECK_H
#define TYPECHECK_H
#include "fl.h"  /* Global data types and include files               */

typedef struct node_type_rec	*node_type_ptr;
typedef struct node_type_rec {
    g_ptr	    node;
    typeExp_ptr	    type;
    node_type_ptr   l;
    node_type_ptr   r;
    node_type_ptr   parent;
    node_type_ptr   unresolved;
} node_type_rec;

typedef struct node_replace_rec	*node_replace_ptr;
typedef struct node_replace_rec {
    int			    alts_found;
    g_ptr		    old;
    g_ptr		    new;
    node_replace_ptr	    next;
} node_replace_rec;

typedef enum {typeVar_tp, Indirection_tp,
	      bool_tp, bexpr_tp, int_tp, obsolete_fsm_tp, string_tp,
	      fail_tp, void_tp, arrow_tp, tuple_tp, list_tp,
	      ref_tp, tbl_tp, fp_tp, concrete_type_tp}		typeOp_tp;

typedef struct typeList_rec {
	typeExp_ptr	type;
	typeList_ptr	next;
} typeList_rec;

typedef struct typeExp_rec {
	typeOp_tp		typeOp;
	union {
	    int		anon_cnt;
	    string	name;
	} 			u;
	typeList_ptr		typelist;
	typeExp_ptr		alias;
	bool			place_holder;
} typeExp_rec;

typedef struct type_diff_rec {
	typeExp_ptr	    type;
	typeExp_rec	    content;
} type_diff_rec;

typedef struct tc_context_rec	*tc_context_ptr;

typedef struct tc_context_rec {
    buffer	    diff_buf;
    tc_context_ptr  next;
} tc_context_rec;

typedef struct ovl_inf_rec  *ovl_inf_ptr;
typedef struct ovl_inf_rec {
    fn_ptr	    fn;
    ovl_inf_ptr	    next;
} ovl_inf_rec;


// For debugging only....
#if 1
#define BV_SIZE		16
#else
#define BV_SIZE		4
#endif

typedef struct bv_rec	    *bv_ptr;
typedef struct bv_rec {
    int	    bv[BV_SIZE];
} bv_rec;

typedef struct ovl_alt_rec  *ovl_alt_ptr;
typedef struct ovl_alt_rec {
    string	    name;
    bool	    open_overload;
    typeExp_ptr	    type;
    int		    version;
    bv_ptr	    bv;
    bv_ptr	    solution;
    ovl_inf_ptr	    alts;
    ovl_alt_ptr	    next;
} ovl_alt_rec;



#define IS_DEFINED_TYPE(t)  ((t)->place_holder == FALSE)
#define CLEAN_TYPE(t)	    { (t)->place_holder = FALSE; }

#endif /* TYPECHECK_H */
#endif /* EXPORT_FORWARD_DECL */
