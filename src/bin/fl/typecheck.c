//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "typecheck.h"
#include "graph.h"
#include "symbol.h"

#define MAX_OVERLOAD_RESOLUTION_DEPTH	50000

#if 0
#define USE_BDDS_FOR_OR
#endif

/************************************************************************/
/*			Global Variables				*/
/************************************************************************/
extern jmp_buf		*start_envp;
extern str_mgr          strings;
extern FILE		*odests_fp;
extern int		line_nbr;
extern bool		file_load;
extern string		cur_file_name;
extern FILE             *yyin;
extern old_yyin_ptr     cur_file;
extern void		PR(g_ptr np);
extern rec_mgr		name_list_rec_mgr;
extern rec_mgr		impl_arg_rec_mgr;

/************************************************************************/
/*			Local Variables					*/
/************************************************************************/
static bool		type_check_is_active = FALSE;
static node_type_ptr	to_be_resolved;
static bool		ti_dirty;
static rec_mgr		node_replace_rec_mgr;
static rec_mgr		node_type_rec_mgr;
static rec_mgr  	GLtypeExp_rec_mgr;
static rec_mgr  	GLtypeList_rec_mgr;
static rec_mgr  	typeExp_rec_mgr;
static rec_mgr  	typeList_rec_mgr;
static rec_mgr		tc_context_mgr;
static hash_record	typeofTbl;
static hash_record	HintTbl;
static hash_record	HintTblLine;
static hash_record	ConcreteTypeTable;
static hash_record	TypeAbrevTable;
static typeExp_rec	bool_op_rec;
static typeExp_ptr	bool_op;
static typeExp_rec	bexpr_op_rec;
static typeExp_ptr	bexpr_op;
static typeExp_rec	fp_op_rec;
static typeExp_ptr	fp_op;
static typeExp_rec	void_op_rec;
static typeExp_ptr	void_op;
static typeExp_rec	obsolete_fsm_op_rec;
static typeExp_ptr	obsolete_fsm_op;
static typeExp_rec	int_op_rec;
static typeExp_ptr	int_op;
static typeExp_rec	string_op_rec;
static typeExp_ptr	string_op;
static int		anon_cnt;
static string		s_int;
static string		s_string;
static string		s_bexpr;
static string		s_bool;
static string		s_fsm;
static string		s_void;
static string		s_fp;
static string		s_ref;
static string		s_tbl;
static char		type_buf[1024];
static tc_context_ptr	current_tc_context;

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/
/* -----Function prototypes for local functions-----*/
static int		nbr_succ_overload_res(node_type_ptr cur,
					      node_replace_ptr *replsp,
					      bool closed, int level);
g_ptr			make_implicit_call(impl_arg_ptr iargs, fn_ptr fun);
static node_type_ptr	build_node_type_struct(hash_record *lvars_tbl,
					       g_ptr node);
static typeExp_ptr	find_core_type(oll_ptr alt);
static typeExp_ptr	find_common_type_rec(typeExp_ptr t1, typeExp_ptr t2);
static typeList_ptr	find_common_typelist(typeList_ptr l1, typeList_ptr l2);
static typeExp_ptr	get_prim_fun_type(g_ptr node);
static typeExp_ptr	get_typeExp_rec();
static typeExp_ptr	new_tVar();
static typeList_ptr	get_typeList_rec();
static typeExp_ptr	get_real_type(typeExp_ptr ttype);
static typeExp_ptr	make_arrow(typeExp_ptr from, typeExp_ptr to);
static typeExp_ptr	make_tuple(typeExp_ptr fst, typeExp_ptr snd);
static typeExp_ptr	make_list(typeExp_ptr type);
static typeExp_ptr	make_ref(typeExp_ptr type);
static bool		occurs(typeExp_ptr t1, typeExp_ptr t2);
static bool		unify(bool quiet, typeExp_ptr t1, typeExp_ptr t2);
static int		get_insert_star_cnt(typeExp_ptr type);
static void		reset_cnt(typeExp_ptr type);
static void		print_type_rec(typeExp_ptr type, odests fp, int prec);
static void		dbg_print_type_rec(typeExp_ptr type,odests fp,int prec);
static typeExp_ptr	fresh_inst(typeExp_ptr type);
static typeExp_ptr	fresh_inst_rec(typeExp_ptr type);
static typeList_ptr	GLget_typeList_rec();
static typeExp_ptr	GLget_typeExp_rec();
static typeExp_ptr	copy_to_persistent_store(typeExp_ptr type);
static typeExp_ptr	copy_to_persistent_store_rec(typeExp_ptr type);
static typeExp_ptr	get_and_replace_final_type(typeExp_ptr *type,
					typeExp_ptr new_type);
static typeExp_ptr	make_proper(typeExp_ptr ctype, typeExp_ptr cur);
static void		failure(string msg);
static void		check_hint(g_ptr node, typeExp_ptr type);
static typeExp_ptr	new_concrete_type(string name, typeList_ptr tvars,
					  bool place_holder);
static void		annotate_star_cnt(typeExp_ptr type);
static void		push_tc_context();
static void		pop_tc_context(bool restore);
#if 0
static void		dbg_print_node_type_tree(node_type_ptr ntp, int level);
#endif

/************************************************************************/
/*			Public Functions				*/
/************************************************************************/

void
PT(typeExp_ptr type)
{
    dbg_print_type_rec(type, err_fp, 0);
    FP(err_fp, "\n");
}

void
PTN(g_ptr node)
{
    typeExp_ptr type;
    if( (type = (typeExp_ptr) find_hash(&typeofTbl, (pointer) node)) != NULL ) {
	PT(type);
    } else
	FP(err_fp, "Undetermined type\n");
}

void
TC_Init()
{
    new_mgr(&GLtypeExp_rec_mgr, sizeof(typeExp_rec));
    new_mgr(&GLtypeList_rec_mgr, sizeof(typeList_rec));
    create_hash(&ConcreteTypeTable, 100, Ustr_hash, Ustr_equ);
    create_hash(&TypeAbrevTable, 100, Ustr_hash, Ustr_equ);
    new_mgr(&typeExp_rec_mgr, sizeof(typeExp_rec));
    new_mgr(&typeList_rec_mgr, sizeof(typeList_rec));
    create_hash(&typeofTbl, 100, ptr_hash, ptr_equ);
    create_hash(&HintTbl, 100, ptr_hash, ptr_equ);
    create_hash(&HintTblLine, 100, ptr_hash, ptr_equ);
    bool_op = &bool_op_rec;
    bool_op->typeOp = bool_tp;
    bool_op->typelist = NULL;
    bexpr_op = &bexpr_op_rec;
    bexpr_op->typeOp = bexpr_tp;
    bexpr_op->typelist = NULL;
    fp_op = &fp_op_rec;
    fp_op->typeOp = fp_tp;
    fp_op->typelist = NULL;
    void_op = &void_op_rec;
    void_op->typeOp = void_tp;
    void_op->typelist = NULL;
    obsolete_fsm_op = &obsolete_fsm_op_rec;
    obsolete_fsm_op->typeOp = obsolete_fsm_tp;
    obsolete_fsm_op->typelist = NULL;
    int_op = &int_op_rec;
    int_op->typeOp = int_tp;
    int_op->typelist = NULL;
    string_op = &string_op_rec;
    string_op->typeOp = string_tp;
    string_op->typelist = NULL;
    s_int = wastrsave(&strings, "int");
    s_string = wastrsave(&strings, "string");
    s_bexpr = wastrsave(&strings, "bexpr");
    s_bool = wastrsave(&strings, "bool");
    s_fsm = wastrsave(&strings, "obsolete_fsm");
    s_void = wastrsave(&strings, "void");
    s_fp = wastrsave(&strings, "stream");
    s_ref = wastrsave(&strings, "ref");
    s_tbl = wastrsave(&strings, "tbl");
    // 
    new_mgr(&tc_context_mgr, sizeof(tc_context_rec));
    current_tc_context = NULL;
    type_check_is_active = FALSE;
}

typeExp_ptr
TypeCheck(g_ptr *ondp, bool delayed, impl_arg_ptr *impl_argsp)
{
    g_ptr nd = *ondp;
    typeExp_ptr	result;
    jmp_buf	tc_start_env;
    jmp_buf	*old_start_envp = start_envp;
    start_envp = &tc_start_env;
 
    ASSERT(type_check_is_active == FALSE);
    type_check_is_active = TRUE;
 
    new_mgr(&node_type_rec_mgr, sizeof(node_type_rec));
    new_mgr(&node_replace_rec_mgr, sizeof(node_replace_rec));
    push_tc_context();
    hash_record lvars_tbl;
    create_hash(&lvars_tbl, 100, str_hash, str_equ);
    to_be_resolved = NULL;
    if( setjmp(*start_envp) == 0 ) {
	node_type_ptr ntp = build_node_type_struct(&lvars_tbl, nd);
	node_replace_ptr nrp;
	int nbr_solns;
      redo_overload_resolution:
#if 0
	nbr_solns = nbr_succ_overload_res(to_be_resolved, &nrp, !delayed, 0);
#else
	(void) delayed;
	nbr_solns = nbr_succ_overload_res(to_be_resolved, &nrp, TRUE, 0);
#endif
	if( nbr_solns == 0 ) {
	    while( nrp != NULL && nrp->alts_found != 0 ) { nrp = nrp->next; }
	    ASSERT(nrp != NULL);
	    if( file_load )
		FP(err_fp, "===Type error around line %d in file %s\n",
			GET_LINE_NBR(nrp->old),
			cur_file_name);
	    else
		FP(err_fp, "===Type error around line %d\n",
			GET_LINE_NBR(nrp->old));
	    ASSERT( IS_USERDEF(nrp->old) );
	    fn_ptr fn = GET_USERDEF(nrp->old);
	    FP(err_fp, "No match found for overloaded function '%s'\n",
		    fn->name);
	    failure("");
	} else if( nbr_solns == 1 ) {
	    while( nrp != NULL ) {
		*(nrp->old) = *(nrp->new);
		nrp = nrp->next;
	    }
	    dispose_hash(&lvars_tbl, NULLFCN);
	    create_hash(&lvars_tbl, 100, str_hash, str_equ);
	    to_be_resolved = NULL;
	    ntp = build_node_type_struct(&lvars_tbl, nd);
	    if( to_be_resolved != NULL ) {
		goto redo_overload_resolution;
	    }
	    result = ntp->type;
	    *impl_argsp = NULL;
	} else {
	    // Not all overloadings resolved. Make them implicit overloadings
	    int cnt = 1;
	    result = ntp->type;
	    impl_arg_ptr impl_args = NULL;
	    for(node_type_ptr ntp = to_be_resolved; ntp != NULL;
		ntp=ntp->unresolved)
	    {
		fn_ptr fp = GET_USERDEF(ntp->node);
		result = make_arrow(ntp->type, result);
		string vname = tprintf(".impl_arg.%d", cnt);
		vname = wastrsave(&strings, vname);
		MAKE_REDEX_VAR(ntp->node, vname);
		g_ptr body = Get_node();
		*body = *nd;
		MAKE_REDEX_LAMBDA(nd,vname,body);
		impl_arg_ptr ia = (impl_arg_ptr) new_rec(&impl_arg_rec_mgr);
		ia->used = TRUE;
		ia->name = fp->name;
		ia->def = fp;
		ia->type = copy_to_persistent_store(ntp->type);
		ia->next = impl_args;
		impl_args = ia;
		cnt++;
	    }
	    *impl_argsp = impl_args;
	}
    } else {
	*impl_argsp = NULL;
	result = NULL;
    }
    start_envp = old_start_envp;
    if( result != NULL ) {
	result = copy_to_persistent_store(result);
    }
    pop_tc_context(TRUE);
    Reset_TypeChecker();
    free_mgr(&node_type_rec_mgr);
    free_mgr(&node_replace_rec_mgr);
    dispose_hash(&lvars_tbl, NULLFCN);
    type_check_is_active = FALSE;
    *ondp = nd;
    return(result);
}

void
Reset_TypeChecker()
{
    dispose_hash(&typeofTbl, NULLFCN);
    dispose_hash(&HintTbl, NULLFCN);
    dispose_hash(&HintTblLine, NULLFCN);
    free_mgr(&typeList_rec_mgr);
    free_mgr(&typeExp_rec_mgr);
    new_mgr(&typeExp_rec_mgr, sizeof(typeExp_rec));
    new_mgr(&typeList_rec_mgr, sizeof(typeList_rec));
    create_hash(&typeofTbl, 100, ptr_hash, ptr_equ);
    create_hash(&HintTbl, 100, ptr_hash, ptr_equ);
    create_hash(&HintTblLine, 100, ptr_hash, ptr_equ);
}

void
TypeHint(g_ptr node, typeExp_ptr type)
{
    insert_hash(&HintTbl, (pointer) node, (pointer) type);
    insert_hash(&HintTblLine, (pointer) node, INT2PTR(line_nbr));
}

void
MoveTypeHint(g_ptr from, g_ptr to, bool function)
{
    typeExp_ptr res;
    int		line;

    if( (res = (typeExp_ptr) find_hash(&HintTbl, (pointer) from)) != NULL ) {
	if( function ) {
	    res = make_arrow(res, new_tVar());
	}
	insert_hash(&HintTbl, (pointer) to, (pointer) res);
	line = PTR2INT(find_hash(&HintTblLine, (pointer) from));
	insert_hash(&HintTblLine, (pointer) to, INT2PTR(line));
    }
}

typeExp_ptr
GetTypeHint(g_ptr nd)
{
    return( (typeExp_ptr) find_hash(&HintTbl, (pointer) nd) );
}

void
New_Type_Abbrev(string name, typeExp_ptr type)
{
    if( find_hash(&TypeAbrevTable, (pointer) name) != NULL )
	delete_hash(&TypeAbrevTable, (pointer) name);
    if( find_hash(&ConcreteTypeTable, (pointer) name) != NULL )
	delete_hash(&ConcreteTypeTable, (pointer) name);
    insert_hash(&TypeAbrevTable, (pointer) name, (pointer) type);
}

static typeExp_ptr
new_concrete_type(string name, typeList_ptr tvars, bool place_holder)
{
    typeExp_ptr type;
    if( find_hash(&TypeAbrevTable, (pointer) name) != NULL )
	delete_hash(&TypeAbrevTable, (pointer) name);
    if( find_hash(&ConcreteTypeTable, (pointer) name) != NULL ) {
	delete_hash(&ConcreteTypeTable, (pointer) name);
    }
    type = GLget_typeExp_rec();
    type->typeOp = concrete_type_tp;
    type->u.name = name;
    type->alias = NULL;
    type->place_holder = place_holder;
    type->typelist = tvars;
    insert_hash(&ConcreteTypeTable, (pointer) name, (pointer) type);
    return(type);
}

static bool
check_typelists(typeList_ptr l1, typeList_ptr l2)
{
  repeat_len_check:
    if( l1 == NULL && l2 == NULL ) return TRUE;
    if( l1 == NULL ) return FALSE;
    if( l2 == NULL ) return FALSE;
    l1 = l1->next;
    l2 = l2->next;
    goto repeat_len_check;
}

static typeList_ptr
find_common_typelist(typeList_ptr l1, typeList_ptr l2)
{
    if( l1 == NULL ) {
	ASSERT(l2 == NULL);
	return l1;
    }
    typeList_ptr rem = find_common_typelist(l1->next, l2->next);
    typeExp_ptr common = find_common_type_rec(l1->type, l2->type);
    if( rem == l1->next && common == l1->type ) {
	return l1;
    }
    if( rem == l2->next && common == l2->type ) {
	return l2;
    }
    typeList_ptr res = GLget_typeList_rec();
    res->type = common;
    res->next = rem;
    return res;
}

static typeExp_ptr
find_common_type_rec(typeExp_ptr t1, typeExp_ptr t2)
{
    t1 = get_real_type(t1);
    t2 = get_real_type(t2);
    if( Type_eq((pointer) t1, (pointer) t2) ) {
	return t1;
    }
    if( t1->typeOp != t2->typeOp || t1->typeOp != arrow_tp ) {
	if( t1->typeOp == typeVar_tp )
	    return t1;
	if( t2->typeOp == typeVar_tp )
	    return t2;
	return( GLnew_tVar() );
    }
    // Both t1 and t2 are function types
    typeList_ptr tl1 = t1->typelist;
    typeList_ptr tl2 = t2->typelist;
    typeList_ptr tl = find_common_typelist(tl1, tl2);
    if( tl == tl1 ) { return t1; }
    if( tl == tl2 ) { return t2; }
    typeExp_ptr res = GLget_typeExp_rec();
    *res = *t1;
    res->typelist = tl;
    return res;
}

typeExp_ptr
Get_common_type(typeExp_ptr default_type, oll_ptr alts)
{
    typeExp_ptr summary_type;
    if( alts == NULL ) {
	if( default_type == NULL ) {
	    return( GLnew_tVar() );
	} else {
	    return default_type;
	}
    }
    summary_type = default_type;
    if( summary_type == NULL ) {
	summary_type = find_core_type(alts);
    }
    while( alts != NULL ) {
	typeExp_ptr alt_ctp = find_core_type(alts);
	summary_type = find_common_type_rec(summary_type, alt_ctp);
        alts = alts->next;
    }
    return( copy_to_persistent_store(summary_type) );
}


typeExp_ptr
Get_Type(string name, typeList_ptr tvars, int insert_missing)
{
    typeExp_ptr type;
    name = wastrsave(&strings, name);
    type = (typeExp_ptr) find_hash(&ConcreteTypeTable, (pointer) name);
    if( type != NULL ) {
	if( type->typelist != NULL ) {
	    // Must make a copy of type
	    if( !check_typelists(tvars, type->typelist) ) {
		FP(err_fp, "-E-: Inconsistent number of type arguments "
			   "for constructor %s "
			   "in file %s close to line %d\n",
			    name, cur_file_name, line_nbr);               
		return NULL;
	    }
	    ASSERT( tvars != NULL );
	    typeExp_ptr ntype = GLget_typeExp_rec();
	    *ntype = *type;
	    ntype->typelist = tvars;
	    CLEAN_TYPE(ntype);
	    type = ntype;
	}
	if( IS_DEFINED_TYPE(type) ) return( type );
	if( insert_missing == TP_INSERT_FULL_TYPE ) {
	    CLEAN_TYPE(type);
	}
	return( type );
    }
    type = (typeExp_ptr) find_hash(&TypeAbrevTable, (pointer) name);
    if( type != NULL )
	return( type );
    if( STREQ(name, s_int) )
        return( int_op );
    if( STREQ(name, s_string) )
	return( string_op );
    if( STREQ(name, s_bool) )
	return( bool_op );
    if( STREQ(name, s_bexpr) )
	return( bexpr_op );
    if( STREQ(name, s_fsm) )
	return( obsolete_fsm_op );
    if( STREQ(name, s_void) )
	return( void_op );
    if( STREQ(name, s_fp) )
	return( fp_op );
    if( STREQ(name, s_tbl) ) {
	type = GLget_typeExp_rec();
	type->typeOp = tbl_tp;
	type->alias = NULL;
	type->typelist = tvars;
	return( type );
    }
    switch( insert_missing ) {
	case TP_INSERT_FULL_TYPE:
	    return( new_concrete_type(name, tvars, FALSE) );
	case TP_INSERT_PLACE_HOLDER:
	    return( new_concrete_type(name, tvars, TRUE) );
	case TP_DONT_INSERT:
	    break;
	default:
	    DIE("Should never happen");
    }
    return( NULL );
}

typeExp_ptr
Get_Real_Type(typeExp_ptr type)
{
    return( get_real_type(type) );
}

void
Print_Full_Type(fn_ptr fn, odests fp, bool newline, bool reset)
{
    typeExp_ptr	   type = Get_Real_Type(fn->type);
    impl_arg_ptr   np = fn->implicit_args;
    if( np != NULL ) {
	hash_record occurences;
	create_hash(&occurences, 100, str_hash, str_equ);
	for(impl_arg_ptr ia = np; ia != NULL; ia = ia->next) {
	    int cur = 0;
	    if( (cur = PTR2INT(find_hash(&occurences, ia->name))) != 0 ) {
		delete_hash(&occurences, ia->name);
	    }
	    cur++;
	    insert_hash(&occurences, ia->name, INT2PTR(cur));
	}
	char	   sep = '[';
	while( np != NULL ) {
	    string name = np->name;
	    int arity;
	    if( (arity = PTR2INT(find_hash(&occurences, name))) != 0 ) {
		if( arity > 1 ) {
		    FP(fp, "%c%s(%d)", sep, np->name, arity);
		} else {
		    FP(fp, "%c%s", sep, np->name);
		}
		delete_hash(&occurences, name);
		sep = ',';
	    }
	    type = Get_Real_Type(type->typelist->next->type);
	    np = np->next;
	}
	FP(fp, "] ");
	dispose_hash(&occurences, NULLFCN);
    }
    Print_Type(type, fp, newline, reset);
}

void
Print_Type(typeExp_ptr type, odests fp, bool newline, bool reset)
{
    if( reset ) {
	reset_cnt(type);
	anon_cnt = 1;
    }
    print_type_rec(type, fp, 0);
    if( newline )
	FP(fp, "\n");
}

string
Type2String(typeExp_ptr type, bool reset)
{
    if( (odests_fp = fmemopen(type_buf, 1024, "w")) == NULL ) {
        DIE("Should never happen");
    }
    Print_Type(type, FILE_fp, TRUE, reset);
    fclose(odests_fp);
    odests_fp = NULL;
    int len = strlen(type_buf);
    if( len > 1 ) {
	string p = type_buf+len-1;
	while( p > type_buf && (*p == '\n' || *p == ' ') ) {
	    *p = 0;
	    p--;
	}
    }
    return( wastrsave(&strings, type_buf) );
}

typeExp_ptr
Fix_Types(typeExp_ptr *constructor_type, typeExp_ptr concrete_type)
{
    typeExp_ptr old_type, t1;
    old_type = get_and_replace_final_type(constructor_type, concrete_type);
    old_type = make_proper(*constructor_type, string_op);
    t1 = GLnew_tVar();
    old_type = GLmake_arrow(GLmake_arrow(old_type, t1),
			  GLmake_arrow(concrete_type, t1));
    return( old_type );
}

typeExp_ptr
GLmake_arrow(typeExp_ptr from, typeExp_ptr to)
{
    typeList_ptr	fl, al;
    typeExp_ptr 	new;
    new = GLget_typeExp_rec();
    new->typeOp = arrow_tp;
    al = GLget_typeList_rec();
    al->type = get_real_type(to);
    al->next = NULL;
    fl = GLget_typeList_rec();
    fl->type = get_real_type(from);
    fl->next = al;
    new->typelist = fl;
    return( new );
}

typeExp_ptr
GLmake_tuple(typeExp_ptr fst, typeExp_ptr snd)
{
    typeList_ptr	fl, sl;
    typeExp_ptr 	new;

    new = GLget_typeExp_rec();
    new->typeOp = tuple_tp;
    sl = GLget_typeList_rec();
    sl->type = get_real_type(snd);
    sl->next = NULL;
    fl = GLget_typeList_rec();
    fl->type = get_real_type(fst);
    fl->next = sl;
    new->typelist = fl;
    return( new );
}

typeExp_ptr
GLmake_list(typeExp_ptr type)
{
    typeList_ptr	tl;
    typeExp_ptr 	new;
    new = GLget_typeExp_rec();
    new->typeOp = list_tp;
    tl = GLget_typeList_rec();
    tl->type = get_real_type(type);
    tl->next = NULL;
    new->typelist = tl;
    return( new );
}

typeExp_ptr
GLmake_bool()
{
    return( bool_op );
}

typeExp_ptr
GLmake_bexpr()
{
    return( bexpr_op );
}

typeExp_ptr
GLmake_ref(typeExp_ptr type)
{
    typeList_ptr	tl;
    typeExp_ptr 	new;
    new = GLget_typeExp_rec();
    new->typeOp = ref_tp;
    tl = GLget_typeList_rec();
    tl->type = get_real_type(type);
    tl->next = NULL;
    new->typelist = tl;
    return( new );
}

typeExp_ptr
GLmake_tbl(typeExp_ptr key_type, typeExp_ptr data_type)
{
    typeList_ptr	tl1, tl2;
    typeExp_ptr 	new;
    new = GLget_typeExp_rec();
    new->typeOp = tbl_tp;
    tl2 = GLget_typeList_rec();
    tl2->type = get_real_type(data_type);
    tl2->next = NULL;
    tl1 = GLget_typeList_rec();
    tl1->type = get_real_type(key_type);
    tl1->next = tl2;
    new->typelist = tl1;
    return( new );
}

typeExp_ptr
GLmake_int()
{
    return( int_op );
}

typeExp_ptr
GLmake_string()
{
    return( string_op );
}

typeExp_ptr
GLmake_fsm()
{
    return( obsolete_fsm_op );
}

typeExp_ptr
GLmake_void()
{
    return( void_op );
}

typeExp_ptr
GLmake_fp()
{
    return( fp_op );
}

/* Inefficient, but who cares since thses lists will be short */
typeList_ptr
Append_Type_List(typeList_ptr list, typeExp_ptr type)
{
    typeList_ptr cur, new;

    if( type == NULL )
	return( list );
    new = GLget_typeList_rec();
    new->next = NULL;
    new->type = type;
    if( list == NULL )
	return( new );
    for(cur = list; cur->next != NULL; cur = cur->next)
	;
    cur->next = new;
    return( list );
}

int
Length_TypeList(typeList_ptr l)
{
    if( l == NULL )
	return(0);
    return( 1 + Length_TypeList(l->next) );
}

void
Unify_Constr(typeExp_ptr type, typeList_ptr type_list)
{
    while( type_list != NULL ) {
	ASSERT( type->typeOp == arrow_tp );
	ASSERT( type->typelist->type->typeOp == typeVar_tp );
	type->typelist->type = type_list->type;
	type = type->typelist->next->type;
	type_list = type_list->next;
    }
}

bool
Is_Ref(typeExp_ptr type)
{
    return( type->typeOp == ref_tp );
}

bool
Is_Tbl(typeExp_ptr type)
{
    return( type->typeOp == tbl_tp );
}

bool
Is_List(typeExp_ptr type)
{
    return( type->typeOp == list_tp );
}

bool
Is_Tuple(typeExp_ptr type)
{
    return( type->typeOp == tuple_tp );
}

typeExp_ptr
Get_List_type(typeExp_ptr type)
{
    ASSERT( type->typeOp == list_tp );
    return( type->typelist->type );
}

typeExp_ptr
Get_Fst_Type(typeExp_ptr type)
{
    ASSERT( type->typeOp == tuple_tp );
    return( type->typelist->type );
}

typeExp_ptr
Get_Snd_Type(typeExp_ptr type)
{
    ASSERT( type->typeOp == tuple_tp );
    return( type->typelist->next->type );
}

bool
Is_Printable_Type(typeExp_ptr type)
{
    type = get_real_type(type);
    switch( type->typeOp ) {
        case bool_tp:
        case bexpr_tp:
        case int_tp:
        case string_tp:
            return( TRUE );
        case list_tp:
        case tuple_tp:
	    /* Maybe */
            return( TRUE );
        case concrete_type_tp:
        case arrow_tp:
        case typeVar_tp:
        case obsolete_fsm_tp:
        case fp_tp:
        case void_tp:
        case ref_tp:
        case tbl_tp:
            return( FALSE );
        default:
            DIE("Unexpected type operator");
    }
}

typeExp_ptr
PrintConvFn(typeExp_ptr type)
{
    type = get_real_type(type);
    if(type->typeOp == arrow_tp ) {
	typeExp_ptr subtype;
	subtype = get_real_type(type->typelist->next->type);
	if( subtype->typeOp == string_tp )
	    return( get_real_type(type->typelist->type) );
    }
    return( NULL );
}


bool
Forward_Declare_Ok(typeExp_ptr declare_type, typeExp_ptr actual_type)
{
    reset_cnt(declare_type);
    anon_cnt = 1;
    annotate_star_cnt(declare_type);
    reset_cnt(actual_type);
    anon_cnt = 1;
    annotate_star_cnt(actual_type);
    // We could be fancier here and allow the actual type to be
    // more general than declare_type, but I'm not sure it's worth it...
    return( Type_eq(declare_type, actual_type) );
}

bool
Check_no_undef_types(typeExp_ptr type)
{
    typeList_ptr tlp;
    type = get_real_type(type);
    switch( type->typeOp ) {
	case concrete_type_tp:
	    if( type->place_holder ) {
		FP(err_fp, "-E- Unknown type '%s'.\n", type->u.name);
		return FALSE;
	    }
	    break;
	case typeVar_tp:
	case bool_tp:
	case bexpr_tp:
	case int_tp:
	case obsolete_fsm_tp:
	case fp_tp:
	case void_tp:
	case string_tp:
	    break;
        case ref_tp:
        case tbl_tp:
	case arrow_tp:
	case tuple_tp:
	case list_tp:
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		if( !Check_no_undef_types(tlp->type) ) return( FALSE );
	    }
	    break;
	default:
	    DIE("Unexpected type operator");
    }
    return TRUE;
}

bool
Is_monomorphic(typeExp_ptr type)
{
    typeList_ptr tlp;
    type = get_real_type(type);
    switch( type->typeOp ) {
	case typeVar_tp:
	    return( FALSE );
	case bool_tp:
	case bexpr_tp:
	case obsolete_fsm_tp:
	case fp_tp:
	case void_tp:
	case int_tp:
	case string_tp:
	    return(TRUE);
	case concrete_type_tp:
	case arrow_tp:
	case tuple_tp:
	case list_tp:
	case ref_tp:
	case tbl_tp:
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		if( !Is_monomorphic(tlp->type) )
		    return(FALSE);
	    }
	    return(TRUE);
	default:
	    DIE("Unexpected type operator");
    }
}

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/

static typeExp_ptr
get_typeExp_rec()
{
    typeExp_ptr new;
    new           = (typeExp_ptr) new_rec(&typeExp_rec_mgr);
    new->place_holder = FALSE;
    new->alias    = NULL;
    new->typelist = NULL;
    return( new );
}

static typeExp_ptr
new_tVar()
{
    typeExp_ptr 	new;
    new = get_typeExp_rec();
    new->typeOp = typeVar_tp;
    new->u.anon_cnt = -1;
    return( new );
}

static typeList_ptr
get_typeList_rec()
{
    typeList_ptr new;
    new = (typeList_ptr) new_rec(&typeList_rec_mgr);
    return( new );
}

static typeExp_ptr
GLget_typeExp_rec()
{
    typeExp_ptr new;
    new = (typeExp_ptr) new_rec(&GLtypeExp_rec_mgr);
    new->place_holder = FALSE;
    new->alias = NULL;
    new->typelist = NULL;
    return( new );
}

typeExp_ptr
GLnew_tVar()
{
    typeExp_ptr 	new;
    new = GLget_typeExp_rec();
    new->typeOp = typeVar_tp;
    new->u.anon_cnt = -1;
    return( new );
}

static typeList_ptr
GLget_typeList_rec()
{
    typeList_ptr new;
    new = (typeList_ptr) new_rec(&GLtypeList_rec_mgr);
    return( new );
}


static typeExp_ptr
get_real_type(typeExp_ptr ttype)
{
    if( ttype->typeOp == Indirection_tp ) {
	return( get_real_type(ttype->alias) );
    } else
	return( ttype );
}


static typeExp_ptr
make_arrow(typeExp_ptr from, typeExp_ptr to)
{
    typeList_ptr	fl, al;
    typeExp_ptr 	new;
    new = get_typeExp_rec();
    new->typeOp = arrow_tp;
    al = get_typeList_rec();
    al->type = get_real_type(to);
    al->next = NULL;
    fl = get_typeList_rec();
    fl->type = get_real_type(from);
    fl->next = al;
    new->typelist = fl;
    return( new );
}

static typeExp_ptr
make_tuple(typeExp_ptr fst, typeExp_ptr snd)
{
    typeList_ptr	fl, sl;
    typeExp_ptr 	new;

    new = get_typeExp_rec();
    new->typeOp = tuple_tp;
    sl = get_typeList_rec();
    sl->type = get_real_type(snd);
    sl->next = NULL;
    fl = get_typeList_rec();
    fl->type = get_real_type(fst);
    fl->next = sl;
    new->typelist = fl;
    return( new );
}

static typeExp_ptr
make_list(typeExp_ptr type)
{
    typeList_ptr	tl;
    typeExp_ptr 	new;
    new = get_typeExp_rec();
    new->typeOp = list_tp;
    tl = get_typeList_rec();
    tl->type = get_real_type(type);
    tl->next = NULL;
    new->typelist = tl;
    return( new );
}

static typeExp_ptr
make_ref(typeExp_ptr type)
{
    typeList_ptr	tl;
    typeExp_ptr 	new;
    new = get_typeExp_rec();
    new->typeOp = ref_tp;
    tl = get_typeList_rec();
    tl->type = get_real_type(type);
    tl->next = NULL;
    new->typelist = tl;
    return( new );
}

static bool
occurs(typeExp_ptr t1, typeExp_ptr t2)
{
    typeList_ptr tlp;

    t1 = get_real_type(t1);
    t2 = get_real_type(t2);
    ASSERT( t1->typeOp == typeVar_tp );
    if( t1 == t2 )
	return( TRUE );
    if( t2->typeOp == typeVar_tp )
	return( FALSE );
    for(tlp = t2->typelist; tlp != NULL; tlp = tlp->next) {
	if( occurs(t1, tlp->type) )
	    return( TRUE );
    }
    return( FALSE );
}

static void
record_old_value(typeExp_ptr t)
{
    type_diff_rec tr;
    tr.type = t;
    tr.content = *t;
    push_buf(&(current_tc_context->diff_buf), &tr);
}

static void
push_tc_context()
{
    tc_context_ptr tmp = (tc_context_ptr) new_rec(&tc_context_mgr);
    new_buf(&(tmp->diff_buf), 100, sizeof(type_diff_rec));
    tmp->next = current_tc_context;
    current_tc_context = tmp;
}

static void
pop_tc_context(bool restore)
{
    type_diff_rec *tdp;
    if( restore ) {
	FUB_ROF(&(current_tc_context->diff_buf), type_diff_rec, tdp) {
	    *(tdp->type) = tdp->content;
	}
    }
    free_buf(&(current_tc_context->diff_buf));
    tc_context_ptr tmp = current_tc_context;
    current_tc_context = tmp->next;
    free_rec(&tc_context_mgr, tmp);
}

static bool
unify(bool quiet, typeExp_ptr t1, typeExp_ptr t2)
{
    typeList_ptr tlp1, tlp2;

    t1 = get_real_type(t1);
    t2 = get_real_type(t2);
    if( t1->typeOp == typeVar_tp ) {
	if( occurs(t1, t2) ) {
	    if( t1 != t2 ) {
		if( !quiet ) FP(err_fp, "===Infinite type\n");
		return( FALSE );
	    } else
		return( TRUE );
	} else {
	    record_old_value(t1);
	    t1->typeOp = Indirection_tp;
	    t1->alias = t2;
	    ti_dirty = TRUE;
	    return( TRUE );
	}
    }
    if( t2->typeOp == typeVar_tp )
	return( unify(quiet, t2, t1) );
    if( (t1->typeOp != t2->typeOp) ||
	((t1->typeOp == concrete_type_tp) &&
	 ( !STREQ(t1->u.name, t2->u.name))) ) {
	if( !quiet ) {
	    FP(err_fp, "===Type mismatch: ");
	    Print_Type(t1, err_fp, FALSE, TRUE);
	    FP(err_fp, " and ");
	    Print_Type(t2, err_fp, TRUE, TRUE);
	}
	return( FALSE );
    }
    for( tlp1 = t1->typelist, tlp2 = t2->typelist;
	 tlp1 != NULL;
 	 tlp1 = tlp1->next, tlp2 = tlp2->next) {
	if( unify(quiet, tlp1->type, tlp2->type) == FALSE )
	    return( FALSE );
    }
    return( TRUE );
}

static typeExp_ptr
printf_type(ui pfn, string pat)
{
    buffer args_buf;
    new_buf (&args_buf, 100, sizeof(typeExp_ptr));

    PROCESS_PRINTF_PAT(pat,
                    get_types_lbl, /* lbl */
                    {}, /* start_of_pat */
                    {}, /* pat_0 */
                    {}, /* pat_1_to_9 */
                    {}, /* pat_minus */
                    {}, /* pat_percent */
                    { push_buf(&args_buf, &int_op); }, /* pat_star */
                    { push_buf(&args_buf, &int_op); }, /* pat_b */
                    { push_buf(&args_buf, &int_op); }, /* pat_o */
                    { push_buf(&args_buf, &int_op); }, /* pat_x */
                    { push_buf(&args_buf, &int_op); }, /* pat_d */
                    { push_buf(&args_buf, &string_op); }, /* pat_s */
                    { push_buf(&args_buf, &bool_op); }, /* pat_B */
                    {
                        typeExp_ptr stringlist_op = make_list(string_op);
                        push_buf(&args_buf, &stringlist_op);
                    }, /* pat_S */
                    {}, /* pat_error */
                    {}) /* pat_other */

    typeExp_ptr res;
    switch( pfn ) {
        case P_PRINTF:
        case P_FPRINTF:
            res = void_op;
            break;
        case P_SPRINTF:
            res = string_op;
            break;
        case P_EPRINTF:
            res = new_tVar();
            break;
        default:
            DIE("Should never happen");
    }
    typeExp_ptr *tp;
    FUB_ROF(&args_buf, typeExp_ptr, tp) {
        res = make_arrow(*tp, res);
    }
    free_buf(&args_buf);
    if( pfn == P_FPRINTF )
        res = make_arrow(fp_op, res);
    return res;
}

int
Printf_arity(ui pfn, string pat)
{
    int arg_cnt = 0;

    PROCESS_PRINTF_PAT(pat,
                    get_types_lbl, /* lbl */
                    {}, /* start_of_pat */
                    {}, /* pat_0 */
                    {}, /* pat_1_to_9 */
                    {}, /* pat_minus */
                    {}, /* pat_percent */
                    { arg_cnt++; }, /* pat_star */
                    { arg_cnt++; }, /* pat_b */
                    { arg_cnt++; }, /* pat_o */
                    { arg_cnt++; }, /* pat_x */
                    { arg_cnt++; }, /* pat_d */
                    { arg_cnt++; }, /* pat_s */
                    { arg_cnt++; }, /* pat_B */
                    { arg_cnt++; }, /* pat_S */
                    {}, /* pat_error */
                    {}) /* pat_other */

    if( pfn == P_FPRINTF )
	arg_cnt++;
    return arg_cnt;
}

static typeExp_ptr
sscanf_type(string pat)
{
    buffer args_buf;
    buffer res_buf;
    new_buf (&args_buf, 100, sizeof(typeExp_ptr));
    new_buf (&res_buf, 100, sizeof(typeExp_ptr));

    PROCESS_PRINTF_PAT(pat,
                    sget_types_lbl, /* lbl */
                    {}, /* start_of_pat */
                    {}, /* pat_0 */
                    {}, /* pat_1_to_9 */
                    {}, /* pat_minus */
                    {}, /* pat_percent */
                    { push_buf(&args_buf, &int_op); }, /* pat_star */
                    { push_buf(&res_buf, &int_op); }, /* pat_b */
                    { push_buf(&res_buf, &int_op); }, /* pat_o */
                    { push_buf(&res_buf, &int_op); }, /* pat_x */
                    { push_buf(&res_buf, &int_op); }, /* pat_d */
                    { push_buf(&res_buf, &string_op); }, /* pat_s */
                    { 
			/* pat_B */
			FP(err_fp, "%%B not supported in Sscanf");
			failure("");
		    },
                    {
			/* pat_S */
			FP(err_fp, "%%S not supported in Sscanf");
			failure("");
                    },
                    {}, /* pat_error */
                    {}) /* pat_other */


    /* Create return type */
    typeExp_ptr res = NULL;
    int items = COUNT_BUF(&res_buf);
    if( items == 0 ) {
	res = void_op;
    } else {
        int left = items;
        typeExp_ptr cur = NULL;
        typeExp_ptr *tpp;
        FUB_ROF(&res_buf, typeExp_ptr, tpp) {
            left--;
            if( left == 0 ) {
                if( cur == NULL ) {
		    res = *tpp;
                } else {
		    res = make_tuple(*tpp, cur);
                }
            } else {
                cur = (cur == NULL)? *tpp : make_tuple(*tpp, cur);
            }
        }
    }
    typeExp_ptr *tp;
    FUB_ROF(&args_buf, typeExp_ptr, tp) {
        res = make_arrow(*tp, res);
    }
    res = make_arrow(string_op, res);
    free_buf(&args_buf);
    free_buf(&res_buf);
    return res;
}

static bool
mismatch_fun_arg(typeExp_ptr *t1p, typeExp_ptr *t2p)
{
    typeExp_ptr t1 = *t1p;
    typeExp_ptr t2 = *t2p;
    if( t1->typeOp != arrow_tp ) return FALSE;
    if( t2->typeOp != arrow_tp ) return FALSE;
    t1 = get_real_type(t1->typelist->type);
    t2 = get_real_type(t2->typelist->type);
    if( Type_eq(t1,t2) ) return FALSE;
    *t1p = t1;
    *t2p = t2;
    return TRUE;
}

static void
report_failure(node_type_ptr cur, typeExp_ptr type, typeExp_ptr expected_type)
{
    g_ptr node = cur->node;
    if( GET_TYPE(node) == LEAF ) {
	switch( GET_LEAF_TYPE(node) ) {
	  case USERDEF: {
	    if( file_load )
		FP(err_fp,
			"===Type error around line %d in file %s\n",
			GET_LINE_NBR(node),
			cur_file_name);
	    else
		FP(err_fp, "===Type error around line %d\n",
			GET_LINE_NBR(node));
	    FP(err_fp, "Function/variable/constant `");
	    fn_ptr fn = GET_USERDEF(node);
	    FP(err_fp, "'%s' is of type:\n\t", fn->name);
	    Print_Type(get_real_type(type), err_fp, TRUE, TRUE);
	    FP(err_fp, "but its usage requires it to be of type:\n\t");
	    Print_Type(get_real_type(expected_type), err_fp,TRUE,TRUE);
	    failure("");
	    break;
	  }
	  case VAR:
	    if( strncmp("__DeStRuCtOr", GET_VAR(node), 12) == 0 ) {
		if( file_load )
		    FP(err_fp,
			    "===Type error around line %d in file %s\n",
			    GET_LINE_NBR(node),
			    cur_file_name);
		else
		    FP(err_fp, "===Type error around line %d\n",
			    GET_LINE_NBR(node));
		FP(err_fp, "Type error in pattern matching.\n");
		FP(err_fp,
		      "The destructor function to the constructor `%s'",
			((GET_VAR(node))+12));
		FP(err_fp, " is of type:\n\t");
	    } else {
		if( file_load )
		    FP(err_fp,
			    "===Type error around line %d in file %s\n",
			    GET_LINE_NBR(node),
			    cur_file_name);
		else
		    FP(err_fp, "===Type error around line %d\n",
			    GET_LINE_NBR(node));
		FP(err_fp, "Function/variable/constant `");
		Print_leaf(node, err_fp);
		FP(err_fp, "' is of type:\n\t");
	    }
	    Print_Type(get_real_type(type), err_fp, TRUE, TRUE);
	    FP(err_fp, "but its usage requires it to be of type:\n\t");
	    Print_Type(get_real_type(expected_type), err_fp,TRUE,TRUE);
	    failure("");
	    break;
	  case INT:
	  case BOOL:
	  case BEXPR:
	  case STRING:
	    if( file_load )
		FP(err_fp,
			"===Type error around line %d in file %s\n",
			GET_LINE_NBR(node),
			cur_file_name);
	    else
		FP(err_fp, "===Type error around line %d\n",
			GET_LINE_NBR(node));
	    FP(err_fp, "Constant `");
	    Print_leaf(node, err_fp);
	    FP(err_fp, "' is of type:\n\t");
	    Print_Type(get_real_type(type), err_fp, TRUE,TRUE);
	    FP(err_fp,
		    "but its usage requires it to be of type:\n\t");
	    Print_Type(get_real_type(expected_type), err_fp, TRUE,TRUE);
	    failure("");
	    break;
	  case PRIM_FN:
	    if( file_load )
		FP(err_fp,
			"===Type error around line %d in file %s\n",
			GET_LINE_NBR(node),
			cur_file_name);
	    else
		FP(err_fp, "===Type error around line %d\n",
			GET_LINE_NBR(node));
	    FP(err_fp, "Function `");
	    Print_leaf(node, err_fp);
	    FP(err_fp, "' is of type:\n\t");
	    Print_Type(get_real_type(type), err_fp, TRUE,TRUE);
	    FP(err_fp, "but its usage requires it to be of type:\n\t");
	    Print_Type(get_real_type(expected_type), err_fp, TRUE,TRUE);
	    failure("");
	    break;
	  default:
	    DIE("Unknown node type in get_type");
	}
    }
    if( IS_APPLY(node) && IS_DEBUG(GET_APPLY_LEFT(node)) ) {
	g_ptr info = GET_APPLY_LEFT(node);
	if( file_load )
	    FP(err_fp, "===Type error around line %d in file %s\n",
		    GET_LINE_NBR(info), cur_file_name);
	else
	    FP(err_fp, "===Type error around line %d\n",
		    GET_LINE_NBR(info));
	string name = GET_DEBUG_STRING(info);
	name = (*name == '.')? name+1 : name;
	FP(err_fp, "%s is of type:\n\t", name);
	Print_Type(get_real_type(expected_type), err_fp, TRUE,TRUE);
	FP(err_fp, "but its usage requires it to be of type:\n\t");
	Print_Type(get_real_type(type), err_fp, TRUE,TRUE);
	failure("");
    }
    if ( IS_LAMBDA(node) && mismatch_fun_arg(&type, &expected_type) ) {
	g_ptr var = GET_LAMBDA_VAR_LEAF(node);
	if( file_load )
	    FP(err_fp, "===Type error around line %d in file %s\n",
		    GET_LINE_NBR(var), cur_file_name);
	else
	    FP(err_fp, "===Type error around line %d\n",
		    GET_LINE_NBR(var));
	string name = GET_VAR(var);
	name = (*name == '.')? name+1 : name;
	FP(err_fp, "%s is of type:\n\t", name);
	Print_Type(get_real_type(expected_type), err_fp, TRUE,TRUE);
	FP(err_fp, "but its usage requires it to be of type:\n\t");
	Print_Type(get_real_type(type), err_fp, TRUE,TRUE);
	failure("");
    }

    // Try to find a leaf node to get a line number
    while( !IS_LEAF(node) && !IS_NIL(node) ) {
	if( IS_APPLY(node) ) {
	    node = GET_APPLY_LEFT(node);
	} else if( IS_CONS(node) ) {
	    node = GET_CONS_HD(node);
	} else if( IS_LAMBDA(node) ) {
	    node = GET_LAMBDA_BODY(node);
	} else {
	    DIE("Could not happen");
	}
    }
    if( !IS_LEAF(node) ) {
	node = cur->parent->node;
	while( !IS_LEAF(node) && !IS_NIL(node) ) {
	    if( IS_APPLY(node) ) {
		node = GET_APPLY_LEFT(node);
	    } else if( IS_CONS(node) ) {
		node = GET_CONS_HD(node);
	    } else if( IS_LAMBDA(node) ) {
		node = GET_LAMBDA_BODY(node);
	    } else {
		DIE("Could not happen");
	    }
	}
    }
    if( IS_LEAF(node) ) {
	if( file_load )
	    FP(err_fp, "===Type error around line %d in file %s\n",
		       GET_LINE_NBR(node), cur_file_name);
	else
	    FP(err_fp, "===Type error around line %d\n", GET_LINE_NBR(node));
    }
    FP(err_fp, "Inferred type is:\n\t");
    Print_Type(get_real_type(expected_type), err_fp, TRUE,TRUE);
    FP(err_fp, "but its usage requires it to be of type:\n\t");
    Print_Type(get_real_type(type), err_fp, TRUE,TRUE);
    failure("");
}

static typeExp_ptr
find_core_type(oll_ptr alt)
{
    typeExp_ptr type = alt->fn->type;
    impl_arg_ptr nlp = alt->fn->implicit_args;
    while( nlp != NULL ) {
	type = type->typelist->next->type;
	nlp = nlp->next;
    }
    return type;
}


static typeExp_ptr 
get_prim_fun_type(g_ptr node)
{
    typeExp_ptr tVar0, tVar1, tVar2, tVar3;
    typeExp_ptr type;
    int pfn = GET_PRIM_FN(node);
    switch( pfn ) {
	case P_S:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    type = (make_arrow(make_arrow(tVar1,
		    make_arrow(tVar2, tVar0)),
		    make_arrow(make_arrow(tVar1, tVar2),
		    make_arrow(tVar1, tVar0))));
	    break;
	case P_K:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(tVar0,
		    make_arrow(tVar1, tVar0)));
	    break;
	case P_I:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0, tVar0));
	    break;
	case P_C:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    type = (make_arrow(make_arrow(tVar1,
		     make_arrow(tVar2, tVar0)),
		      make_arrow(tVar2,
			make_arrow(tVar1, tVar0))));
	    break;
	case P_B:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    type = (make_arrow(make_arrow(tVar2, tVar0),
		    make_arrow(make_arrow(tVar1, tVar2),
		      make_arrow(tVar1, tVar0))));
	    break;
	case P_BPRIME:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    tVar3 = new_tVar();
	    type = (make_arrow(
		     make_arrow(tVar3, make_arrow(tVar1,tVar0)),
		     make_arrow(
			tVar3,
			make_arrow(make_arrow(tVar2,tVar1),
				   make_arrow(tVar2, tVar0)))));
	    break;
	case P_SPRIME:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    tVar3 = new_tVar();
	    type = (make_arrow(make_arrow(tVar3,
		    make_arrow(tVar2, tVar0)),
		     make_arrow(make_arrow(tVar1, tVar3),
		      make_arrow(make_arrow(tVar1, tVar2),
			make_arrow(tVar1, tVar0)))));
	    break;
	case P_CPRIME:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    tVar3 = new_tVar();
	    type = (make_arrow(make_arrow(tVar3,
		     make_arrow(tVar2, tVar0)),
		      make_arrow(make_arrow(tVar1, tVar3),
		       make_arrow(tVar2,
			make_arrow(tVar1, tVar0)))));
	    break;
	case P_BSTAR:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    tVar2 = new_tVar();
	    tVar3 = new_tVar();
	    type = (make_arrow(make_arrow(tVar3, tVar0),
		    make_arrow(make_arrow(tVar2, tVar3),
		      make_arrow(make_arrow(tVar1, tVar2),
			make_arrow(tVar1, tVar0)))));
	    break;
	case P_Y:
	    tVar0 = new_tVar();
	    type = (make_arrow(
			make_arrow(tVar0, tVar0), tVar0));
	    break;
	case P_FST:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(make_tuple(tVar0,tVar1), tVar0));
	    break;
	case P_SND:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(make_tuple(tVar0,tVar1), tVar1));
	    break;
	case P_HEAD:
	    tVar0 = new_tVar();
	    type = (make_arrow(make_list(tVar0), tVar0));
	    break;
	case P_TAIL:
	    tVar0 = new_tVar();
	    type = (make_arrow(make_list(tVar0),
			make_list(tVar0)));
	    break;
	case P_TUPLE:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar1,
			    make_tuple(tVar0, tVar1))));
	    break;
	case P_CONS:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(make_list(tVar0),
		    make_list(tVar0))));
	    break;
	case P_COND:
	    tVar0 = new_tVar();
	    type = (make_arrow(bool_op, make_arrow(tVar0,
			make_arrow(tVar0, tVar0))));
	    break;
	case P_CAT:
	    type = (make_arrow(string_op,
			make_arrow(string_op, string_op)));
	    break;
	case P_EMPTY:
	    tVar0 = new_tVar();
	    type = (make_arrow(make_list(tVar0), bool_op));
	    break;
	case P_LOAD:
	    type = (make_arrow(string_op,
			       make_arrow(bool_op, bool_op)));
	    break;
	case P_LOAD_PLUGIN:
	    type = (make_arrow(string_op, string_op));
	    break;
	case P_ERROR:
	    tVar0 = new_tVar();
	    type = (make_arrow(string_op, tVar0));
	    break;
	case P_PCATCH:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar0, tVar0)));
	    break;
	case P_CATCH:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar0, tVar0)));
	    break;
	case P_SEQ:
	case P_FSEQ:
	case P_NSEQ:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(tVar1,
			make_arrow(tVar0, tVar0)));
	    break;
	case P_IS_CONS:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0, bool_op));
	    break;
	case P_FORALL:
	    type = (make_arrow(make_arrow(bool_op, bool_op),
			    make_arrow(string_op, bool_op)));
	    break;
	case P_THEREIS:
	    type = (make_arrow(make_arrow(bool_op, bool_op),
		    make_arrow(string_op, bool_op)));
	    break;
	case P_QUANT_FORALL:
	case P_QUANT_THEREIS:
	    type = (make_arrow(bool_op, make_arrow(bool_op,
			bool_op)));
	    break;
	case P_IDENTICAL:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar0, bool_op)));
	    break;
	case P_EQUAL:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar0, bool_op)));
	    break;
	case P_NOT_EQUAL:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar0, bool_op)));
	    break;
	case P_CONSTR_EQ:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar0, bool_op)));
	    break;
	case P_FAIL:
	    type = new_tVar();
	    break;
	case P_PFAIL:
	    tVar0 = new_tVar();
	    type = (tVar0);
	    break;
	case P_PRINT:
	    type = (make_arrow(string_op, void_op));
	    break;
	case P_DEBUG:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0, tVar0));
	    break;
	case P_STRING_HD:
	    type = (make_arrow(string_op, string_op));
	    break;
	case P_STRING_TL:
	    type = (make_arrow(string_op, string_op));
	    break;
	case P_SUC:
	    type = (make_arrow(int_op, int_op));
	    break;
	case P_ORD:
	    type = (make_arrow(string_op, int_op));
	    break;
	case P_CHR:
	    type = (make_arrow(int_op, string_op));
	    break;
	case P_EXPLODE:
	    type = (make_arrow(string_op,make_list(string_op)));
	    break;
	case P_IMPLODE:
	    type = (make_arrow(make_list(string_op),string_op));
	    break;
	case P_BOOL2STR:
	    type = (make_arrow(int_op,
			make_arrow(bool_op,string_op)));
	    break;
	case P_INT2STR:
	    type = (make_arrow(int_op,string_op));
	    break;
	case P_TIME:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			       make_tuple(tVar0, string_op)));
	    break;
	case P_BDD_SAVE:
	    type = (make_arrow(string_op,
		       make_arrow(make_list(bool_op),bool_op)));
	    break;
	case P_BDD_LOAD:
	    type = (make_arrow(string_op, make_list(bool_op)));
	    break;
	case P_BDD_REORDER:
	    type = (make_arrow(int_op, bool_op));
	    break;
	case P_RELPROD_THEREIS:
	case P_RELPROD_FORALL:
	    type = (make_arrow(bool_op,
			make_arrow(bool_op,
			    make_arrow(bool_op, bool_op))));
	    break;
	case P_RVAR:
	    type = (make_arrow(string_op, bool_op));
	    break;
	case P_VAR_ORDER:
	    type = (make_arrow(make_list(string_op), make_list(string_op)));
	    break;
	case P_SYSTEM:
	    type = (make_arrow(string_op, int_op));
	    break;
	case P_EVAL:
	    type = (make_arrow(string_op, bool_op));
	    break;
	case P_CACHE:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type  = (make_arrow(tVar0,make_arrow(tVar1,tVar1)));
	    break;
	case P_STRICT_ARGS:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type  = (make_arrow(tVar0,make_arrow(tVar1,tVar1)));
	    break;
	case P_IS_TUPLE:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0, bool_op));
	    break;
	case P_STRICT_TUPLE:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(tVar1,
			    make_tuple(tVar0, tVar1))));
	    break;
	case P_STRICT_CONS:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,
			make_arrow(make_list(tVar0),
		    make_list(tVar0))));
	    break;
	case P_MK_REF_VAR:
	    tVar0 = new_tVar();
	    type = (make_arrow(tVar0,make_ref(tVar0)));
	    break;
	case P_DEREFERENCE:
	    tVar0 = new_tVar();
	    type = (make_arrow(make_ref(tVar0),tVar0));
	    break;
	case P_UPDATE_RVAR:
	    tVar0 = new_tVar();
	    type = (make_arrow(make_ref(tVar0),
			       make_arrow(tVar0, void_op)));
	    break;
	case P_REF_VAR:
	    tVar0 = new_tVar();
	    type = (make_ref(tVar0));
	    break;
	case P_WRITE_TO_FILE:
	    type = (make_arrow(string_op,
			       make_arrow(string_op, void_op)));
	    break;
	case P_PRINTF:
	case P_FPRINTF:
	case P_SPRINTF:
	case P_EPRINTF:
	    type = printf_type(pfn, GET_PRINTF_STRING(node));
	    break; 
	case P_SSCANF:
	    type = sscanf_type(GET_PRINTF_STRING(node));
	    break; 
	case P_FOPEN:
	    type = make_arrow(string_op,
			      make_arrow(string_op, fp_op));
	    break; 
	case P_FCLOSE:
	    type = make_arrow(fp_op, void_op);
	    break; 
	case P_FFLUSH:
	    type = make_arrow(fp_op, void_op);
	    break; 
	case P_VOID:
	    type = void_op;
	    break;
	case P_FILEFP:
	    type = fp_op;
	    break;
	case P_GEN_CATCH:
	    tVar0 = new_tVar();
	    type = make_arrow(
			tVar0,
			make_arrow(
			    make_arrow(string_op, tVar0),
			    tVar0));
	    break;
	case P_TCL_EVAL:
	    type = make_arrow(make_list(string_op),string_op);
	    break;
	case P_GET_VOSSRC:
	    type = make_arrow(string_op,string_op);
	    break;
	case P_UPDATE_VOSSRC:
	    type = make_arrow(string_op,
			      make_arrow(string_op,string_op));
	    break;
	case P_HELP:
	    type = (make_arrow(string_op, string_op));
	    break;
	case P_EXTAPI_FN:
	    type = Get_ExtAPI_Type( GET_EXTAPI_FN(node) );
	    type = fresh_inst_rec(type);
	    break;
	case P_SAVE_GRAPH:
	    tVar0 = new_tVar();
	    type = (make_arrow(string_op,
			make_arrow(string_op,
			    make_arrow(tVar0, bool_op))));
	    break;
	case P_LOAD_GRAPH:
	    tVar0 = new_tVar();
	    type = (make_arrow(string_op,
			make_arrow(string_op, tVar0)));
	    break;
	case P_EXIT:
	    type = (make_arrow(int_op, void_op));
	    break;
	case P_THEN:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(
			tVar0,
			make_arrow(
			    make_arrow(tVar0,tVar1),
			    tVar1)));
	    break;
	case P_UNTYPE:
	    tVar0 = new_tVar();
	    tVar1 = new_tVar();
	    type = (make_arrow(tVar0, tVar1));
	    break;
	default:
	    fprintf(stderr, "Unexpected primitive function (%d)\n", pfn);
	    DIE("Unexpected primitive function (%d)", pfn);
    }
    return type;
}

static void
check_hint(g_ptr node, typeExp_ptr type)
{
    typeExp_ptr res;
    int		pos;

    if( (res = (typeExp_ptr) find_hash(&HintTbl, (pointer) node)) == NULL )
	return;
    if( !unify(FALSE, type, res) ) {
	pos = PTR2INT(find_hash(&HintTblLine, (pointer) node));
	if( file_load )
	    FP(err_fp,
		    "===Type assertion around line %d in file %s failed\n",
		    pos, cur_file_name);
	else
	    FP(err_fp, "===Type assertion around line %d failed\n", pos);
	failure("");
    }
}

static node_type_ptr
build_node_type_struct(hash_record *lvars_tbl, g_ptr node)
{
    if( node == NULL ) return NULL;
    node_type_ptr res = new_rec(&node_type_rec_mgr);
    res->node = node;
    res->type = NULL;
    res->l = NULL;
    res->r = NULL;
    res->parent = NULL;
    res->unresolved = NULL;
    switch( GET_TYPE(node) ) {
        case APPLY_ND: {
	    res->l = build_node_type_struct(lvars_tbl, GET_APPLY_LEFT(node));
	    res->l->parent = res;
	    res->r = build_node_type_struct(lvars_tbl, GET_APPLY_RIGHT(node));
	    res->r->parent = res;
	    typeExp_ptr fn_tp = get_real_type(res->l->type);
	    typeExp_ptr arg_tp = get_real_type(res->r->type);
	    if( fn_tp->typeOp == typeVar_tp ) {
		typeExp_ptr expected = make_arrow(arg_tp, new_tVar());
		if( !unify(FALSE, fn_tp, expected) ) {
		    report_failure(res, fn_tp, expected);
		}
		fn_tp = get_real_type(fn_tp);
	    } else if( fn_tp->typeOp != arrow_tp ) {
		typeExp_ptr expected = make_arrow(arg_tp, new_tVar());
		report_failure(res->l, fn_tp, expected);
	    }
	    typeExp_ptr expected_arg_tp = fn_tp->typelist->type;
	    if( !unify(FALSE, arg_tp, expected_arg_tp) ) {
		report_failure(res->r, arg_tp, expected_arg_tp);
	    }
	    fn_tp = get_real_type(fn_tp);
	    res->type = fn_tp->typelist->next->type;
	    check_hint(node, res->type);
	    res->type = get_real_type(res->type);
	    return res;
	}
	case CONS_ND: {
	    if( IS_NIL(node) ) {
		res->type = make_list(new_tVar());
		check_hint(node, res->type);
		return res;
	    }
	    res->l = build_node_type_struct(lvars_tbl, GET_CONS_HD(node));
	    res->l->parent = res;
	    res->r = build_node_type_struct(lvars_tbl, GET_CONS_TL(node));
	    res->r->parent = res;
	    typeExp_ptr pot_list_tp = make_list(res->l->type);
	    typeExp_ptr list_tp = res->r->type;
	    if( !unify(FALSE, list_tp, pot_list_tp) ) {
		report_failure(res, list_tp, pot_list_tp);
	    }
	    res->type = get_real_type(list_tp);
	    check_hint(node, res->type);
	    res->type = get_real_type(res->type);
	    return res;
	}
	case LAMBDA_ND: {
	    string name = GET_LAMBDA_VAR(node);
	    node_type_ptr old = find_hash(lvars_tbl, name);
	    if( old != NULL ) { delete_hash(lvars_tbl, name); }
	    node_type_ptr var = new_rec(&node_type_rec_mgr);
	    var->node = GET_LAMBDA_VAR_LEAF(node);
	    var->type = new_tVar();
	    var->l = NULL;
	    var->r = NULL;
	    var->parent = NULL;
	    insert_hash(lvars_tbl, name, var);
	    var->unresolved = NULL;
	    res->l = var;
	    var->parent = res;	// Not right....
	    res->r = build_node_type_struct(lvars_tbl, GET_LAMBDA_BODY(node));
	    res->r->parent = res;
	    typeExp_ptr arg_tp = (res->l->type);
	    typeExp_ptr res_type = res->r->type;
	    res->type = make_arrow(arg_tp, res_type);
	    delete_hash(lvars_tbl, name);
	    if( old != NULL ) {
		insert_hash(lvars_tbl, name, old);
	    }
	    check_hint(node, res->type);
	    res->type = get_real_type(res->type);
	    return res;
	}
	case LEAF:  {
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    res->type = int_op;
		    check_hint(node, res->type);
		    return res;
		case BOOL:
		    res->type = bool_op;
		    check_hint(node, res->type);
		    return res;
		case BEXPR:
		    res->type = bexpr_op;
		    check_hint(node, res->type);
		    return res;
		case STRING:
		    res->type = string_op;
		    check_hint(node, res->type);
		    return res;
		case PRIM_FN:
		    res->type = get_prim_fun_type(node);
		    check_hint(node, res->type);
		    return res;
		    break;
		case VAR: {
		    node_type_ptr var = find_hash(lvars_tbl, GET_VAR(node));
		    ASSERT( var != NULL );
		    check_hint(node, var->type);
		    INC_REFCNT(var->node);
		    return var;
		}
		case USERDEF: {
		    fn_ptr  fp = GET_USERDEF(node);
		    res->type = fresh_inst(fp->type);
		    check_hint(node, res->type);
		    res->type = get_real_type(res->type);
		    if( fp->overload ) {
			res->unresolved = to_be_resolved;
			to_be_resolved = res;
		    }
		    return res;
		}
		case EXT_OBJ:
		    DIE("EXT_OBJ should only occur at evaluation time");
		    break;
		default:
		    DIE("Should never happen");
	    }
	    break;
	  }
	default:
	    DIE("Should never happen");
    }
}

g_ptr
make_implicit_call(impl_arg_ptr iargs, fn_ptr fun)
{
    g_ptr node = Make_USERDEF_leaf(fun);
    while( iargs != NULL ) {
	node = Make_APPL_ND(node, Make_USERDEF_leaf(iargs->def));
	iargs = iargs->next;
    }
    return node;
}

fn_ptr
Find_Overload_Choice(fn_ptr fn, typeExp_ptr type)
{
    if( type == NULL ) {
	return fn;
    }
    push_tc_context();
    if( !unify(TRUE, type, fn->type) ) {
	pop_tc_context(TRUE);
	return NULL;
    }
    pop_tc_context(FALSE);
    if( fn->overload == FALSE ) {
	return fn;
    }
    int matches = 0;
    fn_ptr match_fn = NULL;
    for(oll_ptr ol = fn->overload_list; ol != NULL; ol = ol->next) {
	push_tc_context();
	if( unify(TRUE, type, ol->fn->type) ) {
	    matches++;
	    match_fn = ol->fn;
	}
	pop_tc_context(TRUE);
    }
    if( matches == 1 ) {
	return match_fn;
    }
    return fn;
}

static int
nbr_succ_overload_res(node_type_ptr cur, node_replace_ptr *replsp, bool closed,
		      int level)
{
    if( cur == NULL ) {
	*replsp = NULL;
	return 1;
    }
    if( level > MAX_OVERLOAD_RESOLUTION_DEPTH ) {
	if( file_load )
	    FP(err_fp, "===Type error around line %d in file %s\n",
		    GET_LINE_NBR(cur->node),
		    cur_file_name);
	else
	    FP(err_fp, "===Type error around line %d\n",
		    GET_LINE_NBR(cur->node));
	FP(err_fp, "Cannot resolve overloading. Infinite loop in resolution\n");
	failure("");
    }
    ASSERT(cur->l == NULL);
    ASSERT(cur->r == NULL);
    ASSERT(IS_USERDEF(cur->node));
    fn_ptr fn = GET_USERDEF(cur->node);
    oll_ptr alts = fn->overload_list;
    int cnt = 0;
    typeExp_ptr cur_type = get_real_type(cur->type);
    g_ptr replacement_node = NULL;
    node_replace_ptr rem_replacements = NULL;
    while( alts != NULL ) {
	typeExp_ptr alt_type = fresh_inst(find_core_type(alts));
	push_tc_context();
	if( Type_eq(alt_type, cur_type) || unify(TRUE, alt_type, cur_type) ) {
	    g_ptr new_node =
		make_implicit_call(alts->fn->implicit_args, alts->fn);
	    node_type_ptr orig_to_be_resolved = to_be_resolved;
	    to_be_resolved = to_be_resolved->unresolved;
	    node_type_ptr new_tt = build_node_type_struct(NULL, new_node);
	    if( !unify(TRUE, new_tt->type, cur_type) ) {
		DIE("Should not be possible");
	    }
	    node_replace_ptr nrp;
	    int rem_solns = nbr_succ_overload_res(to_be_resolved, &nrp,closed, level+1);
	    cnt += rem_solns;
	    if( cnt > 1 ) {
		node_replace_ptr np =
			    (node_replace_ptr) new_rec(&node_replace_rec_mgr);
		np->alts_found = (rem_solns > 1) ? 1 : 2;
		np->old = cur->node;
		np->new = cur->node;
		np->next = nrp;
		*replsp = np;
		DEC_REF_CNT(new_node);
		to_be_resolved = orig_to_be_resolved;
		pop_tc_context(TRUE);
		return( cnt );
	    }
	    if( rem_solns == 0 ) {
		DEC_REF_CNT(new_node);
		to_be_resolved = orig_to_be_resolved;
	    } else if( rem_solns == 1 ) {
		replacement_node = new_node;
		rem_replacements = nrp;
		to_be_resolved = orig_to_be_resolved;
	    } else {
		DIE("Cannot occur");
	    }
	} else {
	}
	pop_tc_context(TRUE);
	alts = alts->next;
    }
    if( cnt == 0 && fn->open_overload && !closed ) {
	// Open overloaded operations do not have to be
	// resolved unless we're in closed mode.
	node_replace_ptr np = (node_replace_ptr) new_rec(&node_replace_rec_mgr);
	np->alts_found = 2; // > 1
	np->old = cur->node;
	np->new = cur->node;
	np->next = NULL;
	*replsp = np;
	return( 2 );
    }
    if( cnt == 0 ) {
	*replsp = NULL;
	node_replace_ptr np = (node_replace_ptr) new_rec(&node_replace_rec_mgr);
	np->alts_found = 0;
	np->old = cur->node;
	np->new = cur->node;
	np->next = NULL;
	*replsp = np;
	return 0;
    }
    ASSERT(cnt == 1);
    node_replace_ptr nrp = (node_replace_ptr) new_rec(&node_replace_rec_mgr);
    nrp->alts_found = 1;
    nrp->old = cur->node;
    nrp->new = replacement_node;
    nrp->next = rem_replacements;
    *replsp = nrp;
    return 1;
}


static typeExp_ptr
make_proper(typeExp_ptr ctype, typeExp_ptr cur)
{
    if( ctype->typeOp != arrow_tp )
	return(cur);
    cur = GLmake_tuple(cur, ctype->typelist->type);
    return( make_proper(ctype->typelist->next->type, cur) );
}


static typeExp_ptr
get_and_replace_final_type(typeExp_ptr *type, typeExp_ptr new_type)
{
    typeExp_ptr old_type;
    if( (*type)->typeOp == arrow_tp )
	return( get_and_replace_final_type(
			&((*type)->typelist->next->type),
			new_type) );
    old_type = *type;
    *type = new_type;
    return( old_type );
}

static int
get_insert_star_cnt(typeExp_ptr type)
{
    if( type->u.anon_cnt == -1 )
	type->u.anon_cnt = anon_cnt++;
    return( type->u.anon_cnt );
}

static void
reset_cnt(typeExp_ptr type)
{
    typeList_ptr tlp;

    type = get_real_type(type);
    switch( type->typeOp ) {
	case typeVar_tp:
	    type->u.anon_cnt = -1;
	    break;
	case bool_tp:
	case bexpr_tp:
	case int_tp:
	case obsolete_fsm_tp:
	case fp_tp:
	case void_tp:
	case string_tp:
	    break;
	case concrete_type_tp:
        case ref_tp:
        case tbl_tp:
	case arrow_tp:
	case tuple_tp:
	case list_tp:
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		reset_cnt(tlp->type);
	    }
	    break;
	default:
	    DIE("Unexpected type operator");
    }
}

unsigned int
Type_hash(pointer tp, unsigned int n)
{
    typeExp_ptr type;
    if( tp == 0 )
	return 0;

    type = get_real_type((typeExp_ptr) tp);
    switch( type->typeOp ) {
	case typeVar_tp:
	    return(get_insert_star_cnt(type) % n);
	case bool_tp:
	case bexpr_tp:
	case fp_tp:
	case void_tp:
	case obsolete_fsm_tp:
	case int_tp:
	case string_tp:
	    return( (unint) (((lunint) tp) % n) );
	case concrete_type_tp:
	{
	    unsigned int res = str_hash(type->u.name, n);
	    for(typeList_ptr tl = type->typelist; tl != NULL; tl = tl->next) {
		res = (res*5*Type_hash((pointer) tl->type, n)) % n;
	    }
	    return( res );
	}
	case ref_tp:
	    return( (17*Type_hash((pointer) type->typelist->type, n)) % n);
	case tbl_tp:
	    return(((23*Type_hash((pointer) type->typelist->type, n)) +
		    (491*Type_hash((pointer) type->typelist->next->type, n))
		   ) % n);
	case list_tp:
	    return( (137*Type_hash((pointer) type->typelist->type, n)) % n);
	case arrow_tp:
	    return( (937*Type_hash((pointer) type->typelist->type, n) +
		     Type_hash((pointer) type->typelist->next->type, n)) % n);
	case tuple_tp:
	    return( (1187*Type_hash((pointer) type->typelist->type, n) +
		     Type_hash((pointer) type->typelist->next->type, n)) % n);
	default:
	    DIE("Unexpected type operator");
    }
}

bool
Type_eq(pointer t1, pointer t2)
{
    typeExp_ptr type1, type2;
    if( t1 == t2 )
	return(TRUE);
    if( t1 == NULL || t2 == NULL )
	return(FALSE);
    type1 = get_real_type((typeExp_ptr) t1);
    type2 = get_real_type((typeExp_ptr) t2);
    if( type1->typeOp != type2->typeOp )
	return(FALSE);
    switch( type1->typeOp ) {
	case ref_tp:
	case list_tp:
	    return( Type_eq((pointer) type1->typelist->type,
		            (pointer) type2->typelist->type) );
	case tbl_tp:
	case arrow_tp:
	case tuple_tp:
	    return( Type_eq((pointer) type1->typelist->type,
		            (pointer) type2->typelist->type) &&
		    Type_eq((pointer) type1->typelist->next->type,
			    (pointer) type2->typelist->next->type) );
	case concrete_type_tp:
	{
	    if( !STREQ(type1->u.name, type2->u.name) ) return( FALSE );
	    typeList_ptr tl1 = type1->typelist;
	    typeList_ptr tl2 = type2->typelist;
	    while( tl1 != NULL && tl2 != NULL ) {
		if( !Type_eq((pointer) tl1->type, (pointer) tl2->type) ) {
		    return( FALSE );
		}
		tl1 = tl1->next;
		tl2 = tl2->next;
	    }
	    if( tl1 != NULL || tl2 != NULL ) { return( FALSE ); }
	    return( TRUE );
	}
	case typeVar_tp:
	    return(get_insert_star_cnt(type1) == get_insert_star_cnt(type2));
	case bool_tp:
	case bexpr_tp:
	case fp_tp:
	case void_tp:
	case obsolete_fsm_tp:
	case int_tp:
	case string_tp:
	    return( type1 == type2 );
	default:
	    DIE("Unexpected type operator");
    }
}

bool
Is_Void(typeExp_ptr type)
{
    type = get_real_type(type);
    return( type->typeOp == void_tp );
}

static void
dpt(typeExp_ptr type)
{
    switch( type->typeOp ) {
	case Indirection_tp:
	    fprintf(stderr, "(%p)==>(", type);
	    dpt(type->alias);
	    fprintf(stderr, ")");
	    break;
	case typeVar_tp:
	    fprintf(stderr, "*(%p)", type);
	    break;
	case bool_tp:
	    fprintf(stderr, "bool(%p)", type);
	    break;
	case bexpr_tp:
	    fprintf(stderr, "bexpr(%p)", type);
	    break;
	case fp_tp:
	    fprintf(stderr, "stream(%p)", type);
	    break;
	case void_tp:
	    fprintf(stderr, "void(%p)", type);
	    break;
	case obsolete_fsm_tp:
	    fprintf(stderr, "obsolete_fsm(%p)", type);
	    break;
	case int_tp:
            fprintf(stderr, "int(%p)", type);
	    break;
	case string_tp:
	    fprintf(stderr, "string(%p)", type);
	    break;
	case concrete_type_tp:
	    if( type->typelist == NULL ) {
		fprintf(stderr, "%s(%p)", type->u.name, type);
	    } else {
		char sep = '{';
		for(typeList_ptr t = type->typelist; t != NULL; t = t->next) {
		    fprintf(stderr, "%c", sep);
		    sep = ',';
		    dpt(t->type);
		}
		fprintf(stderr, "} %s(%p)", type->u.name, type);
	    }
	    break;
	case arrow_tp:
	    fprintf(stderr, "(");
	    dpt(type->typelist->type);
	    fprintf(stderr, "->");
	    dpt(type->typelist->next->type);
	    fprintf(stderr, "(%p))", type);
	    break;
	case ref_tp:
	    fprintf(stderr, "(");
	    dpt(type->typelist->type);
	    fprintf(stderr, " ref");
	    fprintf(stderr, ")(%p)", type);
	    break;
	case tbl_tp:
	    fprintf(stderr, "({");
	    dpt(type->typelist->type);
	    fprintf(stderr, ", ");
	    dpt(type->typelist->next->type);
	    fprintf(stderr, "} tbl");
	    fprintf(stderr, ")(%p)", type);
	    break;
	case tuple_tp:
	    fprintf(stderr, "(");
	    dpt(type->typelist->type);
	    fprintf(stderr, "#");
	    dpt(type->typelist->next->type);
	    fprintf(stderr, ")(%p)", type);
	    break;
	case list_tp:
	    fprintf(stderr, "(");
	    dpt(type->typelist->type);
	    fprintf(stderr, " list");
	    fprintf(stderr, ")(%p)", type);
	    break;
	default:
	    DIE("WHAT? Unexpected type operator");
    }
}

void
DPT(typeExp_ptr type)
{
    fprintf(stderr, "Type %p(", type);
    type = get_real_type(type);
    fprintf(stderr, "%p): ", type);
    dpt(type);
    fprintf(stderr, "\n");
}

static void
dbg_print_type_rec(typeExp_ptr type, odests fp, int prec)
{
    type = get_real_type(type);
    switch( type->typeOp ) {
	case typeVar_tp:
	    FP(fp, "*(%p)", type);
	    break;
	case bool_tp:
	    FP(fp, "bool");
	    break;
	case bexpr_tp:
	    FP(fp, "bexpr");
	    break;
	case fp_tp:
	    FP(fp, "stream");
	    break;
	case void_tp:
	    FP(fp, "void");
	    break;
	case obsolete_fsm_tp:
	    FP(fp, "obsolete_fsm");
	    break;
	case int_tp:
            FP(fp, "int");
	    break;
	case string_tp:
	    FP(fp, "string");
	    break;
	case concrete_type_tp:
	    if( type->typelist == NULL ) {
		FP(fp, "%s", type->u.name);
	    } else {
		char sep = '{';
		for(typeList_ptr t = type->typelist; t != NULL; t = t->next) {
		    FP(fp, "%c", sep);
		    sep = ',';
		    dbg_print_type_rec(t->type, fp, prec);
		}
		FP(fp, "} %s", type->u.name);
	    }
	    break;
	case arrow_tp:
	    if( prec > 4 )
		FP(fp, "(");
	    dbg_print_type_rec(type->typelist->type, fp, 5);
	    FP(fp, "->");
	    dbg_print_type_rec(type->typelist->next->type, fp, 4);
	    if( prec > 4 )
		FP(fp, ")");
	    break;
	case ref_tp:
	    if( prec > 1 )
		FP(fp, "(");
	    dbg_print_type_rec(type->typelist->type, fp, 1);
	    FP(fp, " ref");
	    if( prec > 1 )
		FP(fp, ")");
	    break;
	case tbl_tp:
	    if( prec > 1 )
		FP(fp, "(");
	    FP(fp, "{");
	    dbg_print_type_rec(type->typelist->type, fp, 1);
	    FP(fp, ", ");
	    dbg_print_type_rec(type->typelist->next->type, fp, 1);
	    FP(fp, "} tbl");
	    if( prec > 1 )
		FP(fp, ")");
	    break;
	case tuple_tp:
	    if( prec > 2 )
		FP(fp, "(");
	    dbg_print_type_rec(type->typelist->type, fp, 3);
	    FP(fp, "#");
	    dbg_print_type_rec(type->typelist->next->type, fp, 2);
	    if( prec > 2 )
		FP(fp, ")");
	    break;
	case list_tp:
	    if( prec > 0 )
		FP(fp, "(");
	    dbg_print_type_rec(type->typelist->type, fp, 1);
	    FP(fp, " list");
	    if( prec > 0 )
		FP(fp, ")");
	    break;
	default:
	    DIE("Unexpected type operator");
    }
}

static void
print_type_rec(typeExp_ptr type, odests fp, int prec)
{
    int cnt;

    type = get_real_type(type);
    switch( type->typeOp ) {
	case typeVar_tp:
	    cnt = get_insert_star_cnt(type);
	    while( cnt > 0 ) {
		FP(fp, "*");
		cnt--;
	    }
	    break;
	case bool_tp:
	    FP(fp, "bool");
	    break;
	case bexpr_tp:
	    FP(fp, "bexpr");
	    break;
	case fp_tp:
	    FP(fp, "stream");
	    break;
	case void_tp:
	    FP(fp, "void");
	    break;
	case obsolete_fsm_tp:
	    FP(fp, "obsolete_fsm");
	    break;
	case int_tp:
            FP(fp, "int");
	    break;
	case string_tp:
	    FP(fp, "string");
	    break;
	case concrete_type_tp:
	    if( type->typelist == NULL ) {
		FP(fp, "%s", type->u.name);
	    } else {
		char sep = '{';
		for(typeList_ptr t = type->typelist; t != NULL; t = t->next) {
		    FP(fp, "%c", sep);
		    sep = ',';
		    print_type_rec(t->type, fp, prec);
		}
		FP(fp, "} %s", type->u.name);
	    }
	    break;
	case arrow_tp:
	    if( prec > 2 )
		FP(fp, "(");
	    print_type_rec(type->typelist->type, fp, 3);
	    FP(fp, "->");
	    print_type_rec(type->typelist->next->type, fp, 2);
	    if( prec > 2 )
		FP(fp, ")");
	    break;
	case ref_tp:
	    if( prec > 1 )
		FP(fp, "(");
	    print_type_rec(type->typelist->type, fp, 1);
	    FP(fp, " ref");
	    if( prec > 1 )
		FP(fp, ")");
	    break;
	case tbl_tp:
	    if( prec > 1 )
		FP(fp, "(");
	    FP(fp, "{");
	    print_type_rec(type->typelist->type, fp, 1);
	    FP(fp, ", ");
	    print_type_rec(type->typelist->next->type, fp, 1);
	    FP(fp, "} tbl");
	    if( prec > 1 )
		FP(fp, ")");
	    break;
	case tuple_tp:
	    if( prec > 2 )
		FP(fp, "(");
	    print_type_rec(type->typelist->type, fp, 3);
	    FP(fp, "#");
	    print_type_rec(type->typelist->next->type, fp, 3);
	    if( prec > 2 )
		FP(fp, ")");
	    break;
	case list_tp:
	    if( prec > 0 )
		FP(fp, "(");
	    print_type_rec(type->typelist->type, fp, 1);
	    FP(fp, " list");
	    if( prec > 0 )
		FP(fp, ")");
	    break;
	default:
	    DIE("Unexpected type operator");
    }
}

static void
clean_aliases(typeExp_ptr type)
{
    typeList_ptr tlp;

    type = get_real_type(type);
    type->alias = NULL;
    switch( type->typeOp ) {
	case typeVar_tp:
	case bool_tp:
	case bexpr_tp:
	case obsolete_fsm_tp:
	case fp_tp:
	case void_tp:
	case int_tp:
	case string_tp:
	    break;
	case concrete_type_tp:
	case ref_tp:
	case tbl_tp:
	case arrow_tp:
	case tuple_tp:
	case list_tp:
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		clean_aliases(tlp->type);
	    }
	    break;
	default:
	    DIE("Unexpected type operator");
    }
}

static typeExp_ptr
copy_to_persistent_store(typeExp_ptr type)
{
    clean_aliases(type);
    return( copy_to_persistent_store_rec(type) );
}

static typeExp_ptr
copy_to_persistent_store_rec(typeExp_ptr type)
{
    typeExp_ptr	 new;
    typeList_ptr tlp, *prevp;

    type = get_real_type(type);
    if( type->alias != NULL )
	return( type->alias );
    switch( type->typeOp ) {
	case typeVar_tp:
	    new = GLnew_tVar();
	    break;
	case bool_tp:
	    new = bool_op;
	    break;
	case bexpr_tp:
	    new = bexpr_op;
	    break;
	case fp_tp:
	    new = fp_op;
	    break;
	case void_tp:
	    new = void_op;
	    break;
	case obsolete_fsm_tp:
	    new = obsolete_fsm_op;
	    break;
	case int_tp:
	    new = int_op;
	    break;
	case string_tp:
	    new = string_op;
	    break;
	case concrete_type_tp:
	    new = GLget_typeExp_rec();
	    new->typeOp = type->typeOp;
	    new->u.name = type->u.name;
	    new->typelist = NULL;
	    prevp = &(new->typelist);
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		typeList_ptr ntlp;
		ntlp = GLget_typeList_rec();
		*prevp = ntlp;
		ntlp->next = NULL;
		prevp = &(ntlp->next);
		ntlp->type = copy_to_persistent_store_rec(tlp->type);
	    }
	    break;
	case arrow_tp:
	case tuple_tp:
	case list_tp:
	case ref_tp:
	case tbl_tp:
	    new = GLget_typeExp_rec();
	    new->typeOp = type->typeOp;
	    prevp = &(new->typelist);
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		typeList_ptr ntlp;
		ntlp = GLget_typeList_rec();
		*prevp = ntlp;
		ntlp->next = NULL;
		prevp = &(ntlp->next);
		ntlp->type = copy_to_persistent_store_rec(tlp->type);
	    }
	    break;
	default:
	    DIE("Unexpected type operator");
    }
    type->alias = new;
    return(new);
}

static typeExp_ptr
fresh_inst(typeExp_ptr type)
{
    clean_aliases(type);
    return( fresh_inst_rec(type) );
}

static typeExp_ptr
fresh_inst_rec(typeExp_ptr type)
{
    typeExp_ptr	 new;
    typeList_ptr tlp, *prevp;

    type = get_real_type(type);
    if( type->alias != NULL )
	return( type->alias );
    switch( type->typeOp ) {
	case typeVar_tp:
	    new = new_tVar();
	    break;
	case bool_tp:
	    new = bool_op;
	    break;
	case bexpr_tp:
	    new = bexpr_op;
	    break;
	case fp_tp:
	    new = fp_op;
	    break;
	case void_tp:
	    new = void_op;
	    break;
	case obsolete_fsm_tp:
	    new = obsolete_fsm_op;
	    break;
	case int_tp:
	    new = int_op;
	    break;
	case string_tp:
	    new = string_op;
	    break;
	case concrete_type_tp:
	case arrow_tp:
	case tuple_tp:
	case list_tp:
	case ref_tp:
	case tbl_tp:
	    new = get_typeExp_rec();
	    new->typeOp = type->typeOp;
	    if( type->typeOp == concrete_type_tp ) {
		new->u.name = type->u.name;
		new->typelist = NULL;
	    }
	    prevp = &(new->typelist);
	    for(tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
		typeList_ptr ntlp;
		ntlp = get_typeList_rec();
		*prevp = ntlp;
		ntlp->next = NULL;
		prevp = &(ntlp->next);
		ntlp->type = fresh_inst_rec(tlp->type);
	    }
	    break;
	default:
	    DIE("Unexpected type operator");
    }
    type->alias = new;
    return(new);
}

static void
failure(string msg)
{
    if( strcmp(msg, "") != 0 )
	FP(err_fp, "%s\n", msg);
    Emit_prompt("");
    longjmp(*start_envp, 1);
}

static void
annotate_star_cnt(typeExp_ptr type)
{
    type = get_real_type(type);
    if( type->typeOp == typeVar_tp ) {
	get_insert_star_cnt(type);
	return;
    }
    for(typeList_ptr tlp = type->typelist; tlp != NULL; tlp = tlp->next) {
	annotate_star_cnt(tlp->type);
    }
}

#if 0
static void
dbg_print_node_type_tree(node_type_ptr ntp, int level)
{
    if( ntp == NULL ) {
	FP(err_fp, "<NULL>");
	return;
    }
    switch( GET_TYPE(ntp->node) ) {
	case LAMBDA_ND:
	    FP(err_fp, "\n%*s{\\%s. (", level*3,"", GET_LAMBDA_VAR(ntp->node));
	    dbg_print_node_type_tree(ntp->l, level+1);
	    FP(err_fp, ")::");
	    dbg_print_type_rec(ntp->type, err_fp, 0);
	    FP(err_fp, "\n%*s}", level*3, "");
	    break;
	case APPLY_ND:
	    FP(err_fp, "\n%*s{( (", level*3, "");
	    dbg_print_node_type_tree(ntp->l, level+1);
	    FP(err_fp, "\n%*s   )\n%*s    (", level*3,"",level*3,"");
	    dbg_print_node_type_tree(ntp->r,level+1);
	    FP(err_fp, "))::");
	    dbg_print_type_rec(ntp->type, err_fp, 0);
	    FP(err_fp, "\n%*s}", level*3, "");
	    break;
	case CONS_ND:
	    FP(err_fp, "\n%*s{( (", level*3, "");
	    dbg_print_node_type_tree(ntp->l, level+1);
	    FP(err_fp, "\n%*s   ) :\n%*s    (", level*3,"",level*3,"");
	    dbg_print_node_type_tree(ntp->r, level+1);
	    FP(err_fp, "))::");
	    dbg_print_type_rec(ntp->type, err_fp, 0);
	    FP(err_fp, "\n%*s}", level*3, "");
	    break;
	case LEAF:
	    FP(err_fp, "\n%*s{", level*3, "");
	    Print_leaf(ntp->node, err_fp);
	    FP(err_fp, "::");
	    dbg_print_type_rec(ntp->type, err_fp, 0);
	    FP(err_fp, "}");
	    break;
    }
}
#endif
