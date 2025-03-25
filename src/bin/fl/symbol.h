//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
*********************************************************************/
/* symbol.h -- header for symbol.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct oll_rec		*oll_ptr;
typedef struct arg_names_rec	*arg_names_ptr;
typedef struct name_expr_rec	*name_expr_ptr;

/* -------- Function prototypes for exported functions -------- */
void		Init_symbol();
void		Symbols_Install_Functions();
void		Mark_symbols();
g_ptr		Get_Fun_Expr(fn_ptr fp);
string		Get_tmp_type_string(typeExp_ptr type);
string		Get_Fun_Signature(fn_ptr fp);
typeExp_ptr	Get_Fun_Type(fn_ptr fp);
fn_ptr		Find_Function_Def(symbol_tbl_ptr stbl, string name);
g_ptr		Find_Function(symbol_tbl_ptr stbl, g_ptr node);
bool		Make_non_lazy(string name, symbol_tbl_ptr stbl);
impl_arg_ptr	Gather_overloaded_calls(symbol_tbl_ptr stbl, g_ptr node);
g_ptr		Add_non_lazy_context_and_find_Userdefs(g_ptr node,
						       symbol_tbl_ptr stbl);
symbol_tbl_ptr	Merge_fn_defs(symbol_tbl_ptr stbl1, symbol_tbl_ptr stbl2,
			      bool permissive);
symbol_tbl_ptr	Add_Destructors(string name, typeExp_ptr type,
				symbol_tbl_ptr stbl,
				string file, int start_line);
oll_ptr		Add_To_OverloadList(string name, typeExp_ptr type, oll_ptr l,
				    string file, int start_line);
symbol_tbl_ptr	Make_forward_declare(string name, typeExp_ptr type,
				     symbol_tbl_ptr stbl,
				     string file, int start_line);
symbol_tbl_ptr	New_fn_def(string name, result_ptr res, symbol_tbl_ptr stbl,
			   bool print, string file, int start_line);
symbol_tbl_ptr	InsertOverloadDef(string name, bool open_overload, oll_ptr alts,
				  typeExp_ptr type, symbol_tbl_ptr stbl,
				  string file, int start_line);
symbol_tbl_ptr	AddToOpenOverloadDef(string name, oll_ptr alts,
				     symbol_tbl_ptr stbl,
				     string file, int start_line);
void		Begin_ADT(symbol_tbl_ptr stbl);
bool		Export_Fun(string name, symbol_tbl_ptr stbl);
symbol_tbl_ptr	End_ADT(symbol_tbl_ptr stbl, var_list_ptr vlp);
string		Get_Help(string fun);
g_ptr		Add_ExtAPI_Function(string name, string strictness,
				    bool non_lazy,
				    typeExp_ptr type, eval_fun_tp fun);
typeExp_ptr	Get_ExtAPI_Type(int id);
int		Get_ExtAPI_ArgCnt(int id);
string		Get_ExtAPI_Strictness(int id);
eval_fun_tp	Get_ExtAPI_Function(int id);
string		Get_ExtAPI_Function_Name(int id);
bool		Check_No_Undefined_Types(symbol_tbl_ptr clist);
g_ptr		Replace_name(g_ptr node, symbol_tbl_ptr stbl);
int		Add_ExtAPI_Object(
		    string name,
		    void    (*mark_fn)(pointer p),
		    void    (*sweep_fn)(),
		    void    (*save_fn)(FILE *fp, pointer p),
		    pointer (*load_fn)(FILE *fp),
		    string  (*obj2string)(pointer p),
		    formula (*eq_fn)(pointer a, pointer b, bool identical),
		    unint   (*hash_fn)(pointer a, unint n),
		    pointer (*gmap_fn)(gmap_info_ptr ip, pointer a),
		    pointer (*gmap2_fn)(gmap_info_ptr ip,pointer a,pointer b),
		    int     (*sha256_fn)(int *g_cntp, hash_record *g_tblp,
					 SHA256_ptr sha, pointer obj));
g_ptr		Make_ext_obj(int class, pointer p);
void		Mark_ext_obj(g_ptr np);
void		Save_ext_obj(FILE *fp, g_ptr np);
void		Sweep_ext_objs();
string		Get_ExtAPI_Object_name(int class);
arg_names_ptr	Get_argument_names(g_ptr node);
string		Get_userdef_name(g_ptr node);
void		DBG_check(string msg);


#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef SYMBOL_H
#define SYMBOL_H
#include "fl.h"  /* Global data types and include files               */

typedef struct arg_names_rec {
    string	    name;
    g_ptr	    default_value;
    arg_names_ptr   next;
} arg_names_rec;

typedef struct symb_tbl_rec {
	fn_ptr		def_list;
        hash_record_ptr	tbl_ptr;
} symbol_tbl_rec;

typedef struct oll_rec {
    fn_ptr	fn;
    oll_ptr	next;
} oll_rec;

typedef struct fn_rec {
	int		    id;
        string		    file_name;
	int		    ADT_level;
        int		    start_line_nbr:28;
        bool		    exported:1;
        bool		    visible:1;
        bool		    in_use:1;
        bool		    non_lazy:1;
        int		    end_line_nbr:29;
        bool		    forward:1;
        bool		    overload:1;
        bool		    open_overload:1;
        comment_list_ptr    comments;
        string		    name;
        g_ptr		    expr;
	g_ptr		    expr_init;
	g_ptr		    expr_comb;
        g_ptr		    super_comb;
        typeExp_ptr	    type;
        oll_ptr		    overload_list;
	impl_arg_ptr	    implicit_args;
	bool		    has_default_values;
	arg_names_ptr	    arg_names;
        string		    signature;
        fn_ptr		    next;
} fn_rec;

typedef struct name_expr_rec {
	string		var;
	g_ptr		expr;
} name_expr_rec;

typedef struct arg_info_rec *arg_info_ptr;

typedef struct arg_info_rec {
	g_ptr	    spine;
	string	    arg_name;
	g_ptr	    arg_expr;
} arg_info_rec;

#endif /* SYMBOL_H */
#endif /* EXPORT_FORWARD_DECL */
