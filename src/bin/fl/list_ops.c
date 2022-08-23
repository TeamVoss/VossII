//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2017			*/
/*									*/
/************************************************************************/
#include "list_ops.h"
#include "graph.h"

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr	    strings;
extern char	    FailBuf[4096];
extern g_ptr	    void_nd;
extern bool	    Disable_GC;

/***** PRIVATE VARIABLES *****/
static hash_record	assoc_tbls;
static hash_record	rev_assoc_tbls;
static hash_record	el_tbls;
static hash_record	mem_tbls;
static rec_mgr		assoc_tbl_rec_mgr;
static assoc_tbl_ptr	assoc_free_list;
static g_ptr		ai_zero;
static jmp_buf		qsort_env;
static g_ptr		comparison_fun;

/* ----- Forward definitions local functions ----- */
static assoc_tbl_ptr	get_assoc_tbl_record();
static bool		is_shared_list(g_ptr l);
static g_ptr		copy_list(g_ptr l);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
List_ops_Init()
{
    // Any needed initialization code
    new_mgr(&assoc_tbl_rec_mgr, sizeof(assoc_tbl_rec));
    create_hash(&assoc_tbls, 100, ptr_hash, ptr_equ);
    create_hash(&rev_assoc_tbls, 100, ptr_hash, ptr_equ);
    create_hash(&mem_tbls, 100, ptr_hash, ptr_equ);
    create_hash(&el_tbls, 100, ptr_hash, ptr_equ);
    ai_zero = Make_INT_leaf(0);
    PUSH_GLOBAL_GC(ai_zero);
}

void
List_GC()
{
    assoc_free_list = NULL;
    assoc_tbl_ptr atp;
    FOR_REC(&assoc_tbl_rec_mgr, assoc_tbl_ptr, atp) {
	if( !atp->in_use ) {
	    atp->free_list = assoc_free_list;
	    assoc_free_list = atp;
	} else {
	    if( GET_REFCNT(atp->assoc_list) == 0 ) {
		if( find_hash(&assoc_tbls, atp->assoc_list) != NULL )
		    delete_hash(&assoc_tbls, atp->assoc_list);
		if( find_hash(&rev_assoc_tbls, atp->assoc_list) != NULL )
		    delete_hash(&rev_assoc_tbls, atp->assoc_list);
		if( find_hash(&mem_tbls, atp->assoc_list) != NULL )
		    delete_hash(&mem_tbls, atp->assoc_list);
		if( find_hash(&el_tbls, atp->assoc_list) != NULL )
		    delete_hash(&el_tbls, atp->assoc_list);
		atp->assoc_list = NULL;
		atp->next_to_insert = NULL;
		dispose_hash(&(atp->tbl), NULLFCN);
		atp->in_use = FALSE;
		atp->free_list = assoc_free_list;
		assoc_free_list = atp;
	    } else {
		// Make sure association list is not freed by ref counting
		SET_REFCNT(atp->assoc_list,MAX_REF_CNT);
	    }
	}
    }
}

g_ptr
G_append(g_ptr list1, g_ptr list2)
{
    if( IS_NIL(list1) ) { return list2; }
    bool shared = is_shared_list(list1);
    if( shared ) { list1 = copy_list(list1); }
    g_ptr cur = list1;
    g_ptr prev = cur;
    while( !IS_NIL(cur) ) {
	prev = cur;
	cur = GET_CONS_TL(cur);
    }
    SET_CONS_TL(prev,list2);
    return( list1 );
}

int
Graph_cmp(g_ptr n1, g_ptr n2)
{
    int res, tp1,tp2;
  g_cmp_restart:
    if( n1 == n2 ) return(0);
    if( n1 == NULL) return(-1);
    if( n2 == NULL) return(1);
    tp1 = GET_TYPE(n1);
    tp2 = GET_TYPE(n2);
    if( tp1 != tp2 ) { return (tp1-tp2); }
    switch( tp1 ) {
	case LAMBDA_ND:
	    DIE("Should never find LAMBDA_NDs during Graph_cmp");
	case APPLY_ND:
	    {
		res = Graph_cmp(GET_APPLY_LEFT(n1), GET_APPLY_LEFT(n2));
		if( res != 0 ) { return res; }
		n1 = GET_APPLY_RIGHT(n1);
		n2 = GET_APPLY_RIGHT(n2);
		goto g_cmp_restart;
	    }
	case CONS_ND:
	    {
		res = Graph_cmp(GET_CONS_HD(n1), GET_CONS_HD(n2));
		if( res != 0 ) { return res; }
		n1 = GET_CONS_TL(n1);
		n2 = GET_CONS_TL(n2);
		goto g_cmp_restart;
	    }
	case LEAF:
	    {
		tp1 = GET_LEAF_TYPE(n1);
		tp2 = GET_LEAF_TYPE(n2);
		if( tp1 != tp2 ) { return (tp1-tp2); }
		switch( tp1 ) {
		    case INT:
			return( Arbi_cmp(GET_AINT(n1), GET_AINT(n2)) );
		    case STRING:
			return( strcmp(GET_STRING(n1), GET_STRING(n2)) );
		    case BOOL:
			return( ((int) GET_BOOL(n1)) - ((int) GET_BOOL(n2)) );
		    case BEXPR:
			return( PTR2INT(GET_BEXPR(n1))-PTR2INT(GET_BEXPR(n2)) );
		    case PRIM_FN:
                        switch ( GET_PRIM_FN(n1) ) {
                            case P_EXTAPI_FN:
				return( ((int) GET_EXTAPI_FN(n1)) -
					((int) GET_EXTAPI_FN(n2)) );
                            case P_SSCANF:
                            case P_PRINTF:
                            case P_SPRINTF:
                            case P_EPRINTF:
                            case P_FPRINTF:
				return( strcmp(GET_PRINTF_STRING(n1),
					       GET_PRINTF_STRING(n2)) );
                            case P_REF_VAR: {
                                int rv1 = GET_REF_VAR(n1);
				n1 = Get_RefVar(rv1);
                                int rv2 = GET_REF_VAR(n2);
				n2 = Get_RefVar(rv2);
				goto g_cmp_restart;
                            }
                            default:
				return( GET_PRIM_FN(n1) - GET_PRIM_FN(n2) );
                        }
		    case EXT_OBJ:
		    {
			unint class1 = GET_EXT_OBJ_CLASS(n1);
			unint class2 = GET_EXT_OBJ_CLASS(n2);
			if( class1 != class2 ) { return (class1 - class2); }
			return ( PTR2INT(GET_EXT_OBJ(n1)) -
				 PTR2INT(GET_EXT_OBJ(n2)) );
		    }
		    case VAR:
			return( strcmp(GET_VAR(n1),GET_VAR(n2)) );
		    case USERDEF:
		    {
			fn_ptr fn1 = GET_USERDEF(n1);
			fn_ptr fn2 = GET_USERDEF(n2);
			return( strcmp(Get_Fun_Signature(fn1),
				       Get_Fun_Signature(fn2)) );
		    }
		    default:
			DIE("Not possible");
		}
	    }
	default:
	    DIE("Should never happen!");
    }
}


/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/



static int
g_cmp(const void *pi, const void *pj)
{
    g_ptr *i = (g_ptr *) pi;
    g_ptr *j = (g_ptr *) pj;
    INC_REFCNT(*i);
    INC_REFCNT(*j);
    SET_REFCNT(comparison_fun, MAX_REF_CNT);
    g_ptr res = Make_APPL_ND(Make_APPL_ND(comparison_fun, *i), *j);
    res = Eval(res);
    if( is_fail(res) ) { longjmp(qsort_env, 1); }
    return( Arbi_cmp(GET_AINT(res), GET_AINT(ai_zero)) );
}


static void
fl_qsort(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr cmp_fn, list;
    EXTRACT_2_ARGS(redex, cmp_fn, list);
    buffer buf;
    new_buf(&buf, 1000, sizeof(g_ptr));
    while( !IS_NIL(list) ) {
        g_ptr e = GET_CONS_HD(list);
        push_buf(&buf, (pointer) &e);
        list = GET_CONS_TL(list);
    }
    comparison_fun = cmp_fn;
    SET_REFCNT(comparison_fun, MAX_REF_CNT);
    if( setjmp(qsort_env) == 0 ) {
	qsort(START_BUF(&buf), COUNT_BUF(&buf), sizeof(g_ptr), g_cmp);
	MAKE_REDEX_NIL(redex);
	g_ptr res_tail = redex;
	g_ptr *gp;
	FOR_BUF(&buf, g_ptr, gp) {
	    APPEND1(res_tail, *gp);
	    INC_REFCNT(*gp);
	}
    } else {
	MAKE_REDEX_FAILURE(redex, FailBuf);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
assoc(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr key = GET_APPLY_RIGHT(l);
    g_ptr alist = GET_APPLY_RIGHT(redex);
    assoc_tbl_ptr atp;
    atp = (assoc_tbl_ptr) find_hash(&assoc_tbls, (pointer) alist);
    if( atp == NULL ) {
	atp = get_assoc_tbl_record();
	create_hash(&(atp->tbl), 100, G_rec_hash, G_rec_equ);
	atp->assoc_list = alist;
	atp->next_to_insert = alist;
	SET_REFCNT(alist,MAX_REF_CNT);
	insert_hash(&assoc_tbls, (pointer) alist, (pointer) atp);
    }
    g_ptr res = find_hash(&(atp->tbl), (pointer) key);
    if( res != NULL ) {
	OVERWRITE(redex, res);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    g_ptr cur = atp->next_to_insert;
    while( !IS_NIL(cur) ) {
	g_ptr entry = GET_CONS_HD(cur);
	cur = GET_CONS_TL(cur);
	atp->next_to_insert = cur;
	g_ptr lkey = GET_CONS_HD(entry);
	g_ptr v = GET_CONS_TL(entry);
	if( find_hash(&(atp->tbl), (pointer) lkey) == NULL )
	    insert_hash(&(atp->tbl), (pointer) lkey, (pointer) v);
	if( G_rec_equ(key, lkey) ) {
	    OVERWRITE(redex, v);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    // Not found
    MAKE_REDEX_FAILURE(redex, Fail_pr("assoc failed to find element"));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
rev_assoc(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr key = GET_APPLY_RIGHT(l);
    g_ptr alist = GET_APPLY_RIGHT(redex);
    assoc_tbl_ptr atp;
    atp = (assoc_tbl_ptr) find_hash(&rev_assoc_tbls, (pointer) alist);
    if( atp == NULL ) {
	atp = get_assoc_tbl_record();
	create_hash(&(atp->tbl), 100, G_rec_hash, G_rec_equ);
	atp->assoc_list = alist;
	atp->next_to_insert = alist;
	SET_REFCNT(alist,MAX_REF_CNT);
	insert_hash(&rev_assoc_tbls, (pointer) alist, (pointer) atp);
    }
    g_ptr res = find_hash(&(atp->tbl), (pointer) key);
    if( res != NULL ) {
	OVERWRITE(redex, res);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    g_ptr cur = atp->next_to_insert;
    while( !IS_NIL(cur) ) {
	g_ptr entry = GET_CONS_HD(cur);
	cur = GET_CONS_TL(cur);
	atp->next_to_insert = cur;
	g_ptr v = GET_CONS_HD(entry);
	g_ptr lkey = GET_CONS_TL(entry);
	if( find_hash(&(atp->tbl), (pointer) lkey) == NULL )
	    insert_hash(&(atp->tbl), (pointer) lkey, (pointer) v);
	if( G_rec_equ(key, lkey) ) {
	    OVERWRITE(redex, v);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    // Not found
    MAKE_REDEX_FAILURE(redex, Fail_pr("rev_assoc failed to find element"));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
el(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gi = GET_APPLY_RIGHT(l);
    g_ptr alist = GET_APPLY_RIGHT(redex);
    int	idx = GET_INT(gi);
    if( idx <= 0 ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("el with index <= 0 (%d)", idx));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    assoc_tbl_ptr atp;
    atp = (assoc_tbl_ptr) find_hash(&el_tbls, (pointer) alist);
    if( atp == NULL ) {
	atp = get_assoc_tbl_record();
	create_hash(&(atp->tbl), 100, int_hash, int_equ);
	atp->assoc_list = alist;
	atp->next_to_insert = alist;
	SET_REFCNT(alist,MAX_REF_CNT);
	insert_hash(&el_tbls, (pointer) alist, (pointer) atp);
    }
    g_ptr res = find_hash(&(atp->tbl), INT2PTR(idx));
    if( res != NULL ) {
	OVERWRITE(redex, res);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    g_ptr cur = atp->next_to_insert;
    int last_idx = hash_size(&(atp->tbl));
    while( !IS_NIL(cur) ) {
	g_ptr entry = GET_CONS_HD(cur);
	cur = traverse_left(GET_CONS_TL(cur));
	if( is_fail(cur) ) {
	    OVERWRITE(redex, cur);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	atp->next_to_insert = cur;
	last_idx++;
	insert_hash(&(atp->tbl), INT2PTR(last_idx), (pointer) entry);
	if( last_idx == idx ) {
	    entry = traverse_left(entry);
	    OVERWRITE(redex, entry);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    // Not found
    MAKE_REDEX_FAILURE(redex,
		Fail_pr("el %d in list with only %d elements", idx, last_idx));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
sitlist(g_ptr redex)
{
    // sitlist fn l z
    g_ptr fn, list, init;
    EXTRACT_3_ARGS(redex, fn, list, init);
    buffer buf;
    new_buf(&buf, 1000, sizeof(g_ptr));
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	push_buf(&buf, (pointer) &e);
	list = GET_CONS_TL(list);
    }
    g_ptr cur = init;
    INC_REFCNT(init);
    g_ptr *gp;
    FUB_ROF(&buf, g_ptr, gp) {
	g_ptr l = *gp;
	INC_REFCNT(l);
	SET_REFCNT(fn,MAX_REF_CNT);
	cur = Make_APPL_ND(Make_APPL_ND(fn, *gp), cur);
	cur = Eval(cur);
    }
    OVERWRITE(redex, cur);
}

static void
mem(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr key = GET_APPLY_RIGHT(l);
    g_ptr alist = GET_APPLY_RIGHT(redex);
    assoc_tbl_ptr atp;
    atp = (assoc_tbl_ptr) find_hash(&mem_tbls, (pointer) alist);
    if( atp == NULL ) {
	atp = get_assoc_tbl_record();
	create_hash(&(atp->tbl), 100, G_rec_hash, G_rec_equ);
	atp->assoc_list = alist;
	atp->next_to_insert = alist;
	SET_REFCNT(alist,MAX_REF_CNT);
	insert_hash(&mem_tbls, (pointer) alist, (pointer) atp);
    }
    g_ptr res = find_hash(&(atp->tbl), (pointer) key);
    if( res != NULL ) {
	MAKE_REDEX_BOOL(redex, B_One());
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    g_ptr cur = atp->next_to_insert;
    while( !IS_NIL(cur) ) {
	g_ptr entry = GET_CONS_HD(cur);
	cur = GET_CONS_TL(cur);
	atp->next_to_insert = cur;
	if( find_hash(&(atp->tbl), (pointer) entry) == NULL )
	    insert_hash(&(atp->tbl), (pointer) entry, INT2PTR(1));
	if( G_rec_equ(key, entry) ) {
	    MAKE_REDEX_BOOL(redex, B_One());
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    // Not found
    MAKE_REDEX_BOOL(redex, B_Zero());
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
rev(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    buffer buf;
    new_buf(&buf, 100, sizeof(g_ptr));
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	push_buf(&buf, (pointer) &e);
	list = GET_CONS_TL(list);
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    g_ptr *gp;
    FUB_ROF(&buf, g_ptr, gp) {
	SET_CONS_HD(tail, *gp);
	INC_REFCNT(*gp);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    free_buf(&buf);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
length(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    int cnt = 0;
    while( !IS_NIL(list) ) {
	cnt++;
	list = GET_CONS_TL(list);
    }
    MAKE_REDEX_INT(redex, cnt);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
setify(g_ptr redex)
{
    hash_record	    setify_tbl;
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    create_hash(&setify_tbl, 100, G_rec_hash, G_rec_equ);
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	if( find_hash(&setify_tbl, e) == NULL ) {
	    insert_hash(&setify_tbl, e, e);
	    SET_CONS_HD(tail, e);
	    INC_REFCNT(e);
	    SET_CONS_TL(tail, Make_NIL());
	    tail = GET_CONS_TL(tail);
	}
	list = GET_CONS_TL(list);
    }
    dispose_hash(&setify_tbl, NULLFCN);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
butlast(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = r;
    if( IS_NIL(list) ) {
	MAKE_REDEX_FAILURE(redex, "butlast applied to empty list");
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(GET_CONS_TL(list)) ) {
	g_ptr el = GET_CONS_HD(list);
	INC_REFCNT(el);
	SET_CONS_HD(tail, el);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	list = GET_CONS_TL(list);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
butfirstn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gcnt;
    g_ptr list;
    EXTRACT_2_ARGS(redex, gcnt, list);
    int cnt = GET_INT(gcnt);
    int len = 0;
    for(int i = 0; i < cnt; i++) {
	if( IS_NIL(list) ) {
	    string msg = Fail_pr("butfirstn %d on list with only %d elements",
				 cnt, len);
	    MAKE_REDEX_FAILURE(redex, msg);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	list = traverse_left(GET_CONS_TL(list));
	if( is_fail(list) ) {
	    OVERWRITE(redex, list);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	len++;
    }
    OVERWRITE(redex, list);
    if( !IS_NIL(redex) ) {
	INC_REFCNT(GET_CONS_HD(redex));
	INC_REFCNT(GET_CONS_TL(redex));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
lastn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gcnt;
    g_ptr list;
    EXTRACT_2_ARGS(redex, gcnt, list);
    int cnt = GET_INT(gcnt);
    int len = 0;
    for(g_ptr cur = list; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	len++;
    }
    if( cnt > len ) {
	string msg = Fail_pr("lastn %d on list with only %d elements",
			     cnt, len);
	MAKE_REDEX_FAILURE(redex, msg);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    // Now do a butfirstn (len-cnt)
    cnt = len-cnt;
    for(int i = 0; i < cnt; i++) {
	list = GET_CONS_TL(list);
    }
    OVERWRITE(redex, list);
    if( !IS_NIL(redex) ) {
	INC_REFCNT(GET_CONS_HD(redex));
	INC_REFCNT(GET_CONS_TL(redex));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}


static void
butlastn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gcnt;
    g_ptr list;
    EXTRACT_2_ARGS(redex, gcnt, list);
    int cnt = GET_INT(gcnt);
    int len = 0;
    for(g_ptr cur = list; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	len++;
    }
    if( cnt > len ) {
	string msg = Fail_pr("butlastn %d on list with only %d elements",
			     cnt, len);
	MAKE_REDEX_FAILURE(redex, msg);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    cnt = len-cnt;
    // Now do a firstn (len-cnt)
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    for(int i = 0; i < cnt; i++) {
	g_ptr el = GET_CONS_HD(list);
	INC_REFCNT(el);
	SET_CONS_HD(tail, el);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	list = GET_CONS_TL(list);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
firstn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gcnt;
    g_ptr list;
    EXTRACT_2_ARGS(redex, gcnt, list);
    int cnt = GET_INT(gcnt);
    PUSH_GLOBAL_GC(l);
    PUSH_GLOBAL_GC(r);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    int len = 0;
    for(int i = 0; i < cnt; i++) {
	if( IS_NIL(list) ) {
	    string msg = Fail_pr("firstn %d on list with only %d elements",
				 cnt, len);
	    MAKE_REDEX_FAILURE(redex, msg);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    POP_GLOBAL_GC(2);
	    return;
	}
	g_ptr el = GET_CONS_HD(list);
	INC_REFCNT(el);
	SET_CONS_HD(tail, el);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	list = traverse_left(GET_CONS_TL(list));
	if( is_fail(list) ) {
	    OVERWRITE(redex, list);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    POP_GLOBAL_GC(2);
	    return;
	}
	len++;
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    POP_GLOBAL_GC(2);
    return;
}

static void
find_first0(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr pred = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);
    int idx = 0;
    while( !IS_NIL(list) ) {
	idx++;
	g_ptr e = GET_CONS_HD(list);
	g_ptr use = Make_APPL_ND(pred, e);
	INC_REFCNT(pred);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	formula res = GET_BOOL(use);
	if( res == B_One() ) {
	    MAKE_REDEX_INT(redex, idx);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	} else if( res == B_Zero() ) {
	    // Nothing
	} else {
	    // Evaluated to symbolic expression
	    MAKE_REDEX_FAILURE(redex,
		       "find_first0 predicate evaluated to symbolic condition");
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	list = traverse_left(GET_CONS_TL(list));
	if( is_fail(list) ) {
	    OVERWRITE(redex, list);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    MAKE_REDEX_INT(redex, 0);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
find(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr pred = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);
    int idx = 0;
    while( !IS_NIL(list) ) {
	idx++;
	g_ptr e = GET_CONS_HD(list);
	g_ptr use = Make_APPL_ND(pred, e);
	INC_REFCNT(pred);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	formula res = GET_BOOL(use);
	if( res == B_One() ) {
	    OVERWRITE(redex, e);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	} else if( res == B_Zero() ) {
	    // Nothing
	} else {
	    // Evaluated to symbolic expression
	    MAKE_REDEX_FAILURE(redex,
		       "find predicate evaluated to symbolic condition");
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	list = traverse_left(GET_CONS_TL(list));
	if( is_fail(list) ) {
	    OVERWRITE(redex, list);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    MAKE_REDEX_FAILURE(redex,
		       "No element found satisfying the predicate in find");
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
foreach_list_item(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr fun = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	g_ptr use = Make_APPL_ND(fun, e);
	INC_REFCNT(fun);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	list = traverse_left(GET_CONS_TL(list));
	if( is_fail(list) ) {
	    OVERWRITE(redex, list);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
    }
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
find_all(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr pred = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);
    int idx = 0;
    PUSH_GLOBAL_GC(pred);
    PUSH_GLOBAL_GC(list);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(list) ) {
	idx++;
	g_ptr e = GET_CONS_HD(list);
	g_ptr use = Make_APPL_ND(pred, e);
	INC_REFCNT(pred);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    POP_GLOBAL_GC(2);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	formula res = GET_BOOL(use);
	if( res == B_One() ) {
	    APPEND1(tail, Make_INT_leaf(idx));
	} else if( res == B_Zero() ) {
	    // Nothing
	} else {
	    // Evaluated to symbolic expression
	    MAKE_REDEX_FAILURE(redex,
			"find_all predicate evaluated to symbolic condition");
	    POP_GLOBAL_GC(2);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	list = GET_CONS_TL(list);
    }
    POP_GLOBAL_GC(2);
    return;
}

static void
filter(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr pred = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);
    PUSH_GLOBAL_GC(pred);
    PUSH_GLOBAL_GC(list);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	list = GET_CONS_TL(list);
	g_ptr use = Make_APPL_ND(pred, e);
	INC_REFCNT(pred);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    POP_GLOBAL_GC(2);
	    return;
	}
	formula res = GET_BOOL(use);
	if( res == B_One() ) {
	    SET_CONS_HD(tail,e);
	    INC_REFCNT(e);
	    SET_CONS_TL(tail, Make_NIL());
	    tail = GET_CONS_TL(tail);
	} else if( res == B_Zero() ) {
	    // Nothing
	} else {
	    // Evaluated to sym
	    MAKE_REDEX_FAILURE(redex,
			"filter predicate evaluated to symbolic condition");
	    POP_GLOBAL_GC(2);
	    return;
	}
    }
    POP_GLOBAL_GC(2);
    return;
}

static void
split(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr pred = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);

    g_ptr ok = Make_NIL();
    PUSH_GLOBAL_GC(ok);
    g_ptr ok_tail = ok;

    g_ptr not_ok = Make_NIL();
    PUSH_GLOBAL_GC(not_ok);
    g_ptr not_ok_tail = not_ok;

    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	list = GET_CONS_TL(list);
	g_ptr use = Make_APPL_ND(pred, e);
	INC_REFCNT(pred);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    POP_GLOBAL_GC(2);
	    return;
	}
	formula res = GET_BOOL(use);
	if( res == B_One() ) {
	    SET_CONS_HD(ok_tail,e);
	    INC_REFCNT(e);
	    SET_CONS_TL(ok_tail, Make_NIL());
	    ok_tail = GET_CONS_TL(ok_tail);
	} else if( res == B_Zero() ) {
	    SET_CONS_HD(not_ok_tail,e);
	    INC_REFCNT(e);
	    SET_CONS_TL(not_ok_tail, Make_NIL());
	    not_ok_tail = GET_CONS_TL(not_ok_tail);
	} else {
	    // Evaluated to sym
	    MAKE_REDEX_FAILURE(redex,
			"split predicate evaluated to symbolic condition");
	    POP_GLOBAL_GC(2);
	    return;
	}
    }
    MAKE_REDEX_CONS_ND(redex, ok, not_ok);
    POP_GLOBAL_GC(2);
    return;
}


static void
partition(g_ptr redex)
{
    buffer	    partition_results;
    buffer	    partition_results_tail;
    hash_record	    partition_tbl;
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr pred = GET_APPLY_RIGHT(l);
    g_ptr list = GET_APPLY_RIGHT(redex);

//Disable_GC = TRUE;
    int used = 0;
    new_buf(&partition_results, 100, sizeof(g_ptr));
    new_buf(&partition_results_tail, 100, sizeof(g_ptr));
    create_hash(&partition_tbl, 1000, G_rec_hash, G_rec_equ);
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	list = GET_CONS_TL(list);
	g_ptr use = Make_APPL_ND(pred, e);
	INC_REFCNT(pred);
	INC_REFCNT(e);
	use = Eval(use);
	if( is_fail(use) ) {
	    MAKE_REDEX_FAILURE(redex, FailBuf);
	    POP_GLOBAL_GC(COUNT_BUF(&partition_results));
//Disable_GC = FALSE;
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	int idx = PTR2INT(find_hash(&partition_tbl,use));
	if( idx == 0 ) {
	    used++;
	    idx = used;
	    insert_hash(&partition_tbl, use, INT2PTR(idx));
	    g_ptr r = Make_NIL();
	    PUSH_GLOBAL_GC(r);
	    push_buf(&partition_results, &r);
	    push_buf(&partition_results_tail, &r);
	}
	g_ptr tl = *((g_ptr *) M_LOCATE_BUF(&partition_results_tail, idx-1));
	SET_CONS_HD(tl, e);
	INC_REFCNT(e);
	SET_CONS_TL(tl, Make_NIL());
	tl = GET_CONS_TL(tl);
	store_buf(&partition_results_tail,idx-1, &tl);
    }
    POP_GLOBAL_GC(COUNT_BUF(&partition_results));
    g_ptr tail = redex;
    MAKE_REDEX_NIL(redex);
    g_ptr *gp;
    FOR_BUF(&partition_results, g_ptr, gp) {
	SET_CONS_HD(tail, *gp);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    free_buf(&partition_results);
    free_buf(&partition_results_tail);
    dispose_hash(&partition_tbl, NULLFCN);
//Disable_GC = FALSE;
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static bool
is_shared_list(g_ptr l)
{
    while( l != NULL ) {
	if( GET_REFCNT(l) > 0 ) return TRUE;
	l = GET_CONS_TL(l);
    }
    return FALSE;
}

static g_ptr
copy_list(g_ptr l)
{
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    while( !IS_NIL(l) ) {
	g_ptr cur = GET_CONS_HD(l);
	SET_CONS_HD(tail, cur);
	INC_REFCNT(cur);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	l = GET_CONS_TL(l);
    }
    return res;
}

static void
my_union(g_ptr redex)
{
    g_ptr set1, set2;
    EXTRACT_2_ARGS(redex, set1, set2);
    if( IS_NIL(set1) ) {
	OVERWRITE(redex, set2);
	return;
    }
    if( IS_NIL(set2) ) {
	OVERWRITE(redex, set1);
	return;
    }
    hash_record set;
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    create_hash(&set, 100, Graph_hash, Graph_equ);
    while( !IS_NIL(set1) ) {
	g_ptr d = GET_CONS_HD(set1);
	if( find_hash(&set, d) == NULL ) {
	    APPEND1(tail, d);
	    INC_REFCNT(d);
	    insert_hash(&set, d, d);
	}
	set1 = GET_CONS_TL(set1);
    }
    while( !IS_NIL(set2) ) {
	g_ptr d = GET_CONS_HD(set2);
	if( find_hash(&set, d) == NULL ) {
	    APPEND1(tail, d);
	    INC_REFCNT(d);
	    insert_hash(&set, d, d);
	}
	set2 = GET_CONS_TL(set2);
    }
    dispose_hash(&set, NULLFCN);
}

static void
intersect(g_ptr redex)
{
    g_ptr set1, set2;
    EXTRACT_2_ARGS(redex, set1, set2);
    if( IS_NIL(set1) ) {
	OVERWRITE(redex, set1);
	return;
    }
    if( IS_NIL(set2) ) {
	OVERWRITE(redex, set2);
	return;
    }
    hash_record set;
    create_hash(&set, 100, Graph_hash, Graph_equ);
    while( !IS_NIL(set2) ) {
	g_ptr d = GET_CONS_HD(set2);
	if( find_hash(&set, d) == NULL ) {
	    insert_hash(&set, d, d);
	}
	set2 = GET_CONS_TL(set2);
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(set1) ) {
	g_ptr d = GET_CONS_HD(set1);
	if( find_hash(&set, d) != NULL ) {
	    APPEND1(tail, d);
	    INC_REFCNT(d);
	}
	set1 = GET_CONS_TL(set1);
    }
    dispose_hash(&set, NULLFCN);
}

static void
subtract(g_ptr redex)
{
    g_ptr set1, set2;
    EXTRACT_2_ARGS(redex, set1, set2);
    if( IS_NIL(set1) ) {
	OVERWRITE(redex, set1);
	return;
    }
    if( IS_NIL(set2) ) {
	OVERWRITE(redex, set1);
	return;
    }
    hash_record set;
    create_hash(&set, 100, Graph_hash, Graph_equ);
    while( !IS_NIL(set2) ) {
	g_ptr d = GET_CONS_HD(set2);
	if( find_hash(&set, d) == NULL ) {
	    insert_hash(&set, d, d);
	}
	set2 = GET_CONS_TL(set2);
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(set1) ) {
	g_ptr d = GET_CONS_HD(set1);
	if( find_hash(&set, d) == NULL ) {
	    APPEND1(tail, d);
	    INC_REFCNT(d);
	}
	set1 = GET_CONS_TL(set1);
    }
    dispose_hash(&set, NULLFCN);
}

static void
append(g_ptr redex)
{
    g_ptr list1, list2;
    EXTRACT_2_ARGS(redex, list1, list2);
    if( IS_NIL(list1) ) {
	g_ptr l = GET_APPLY_LEFT(redex);
	OVERWRITE(redex, list2);
	DEC_REF_CNT(l);
	return;
    }
    bool shared = is_shared_list(list1);
// A reference counting bug.......
#if 0
    if( shared ) { list1 = copy_list(list1); }
#else
    list1 = copy_list(list1);
#endif
    g_ptr cur = list1;
    g_ptr prev = cur;
    while( !IS_NIL(cur) ) {
	prev = cur;
	cur = GET_CONS_TL(cur);
    }
    SET_CONS_TL(prev,list2);
    if( shared ) {
	DEC_REF_CNT(GET_APPLY_LEFT(redex));
    }
    OVERWRITE(redex, list1);
}

static void
flat(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    // Can be improved to avoiding copying all the sublists if not shared
    while( !IS_NIL(list) ) {
	g_ptr ll = copy_list(GET_CONS_HD(list));
	OVERWRITE(tail, ll);
	while( !IS_NIL(tail) ) {
	    tail = GET_CONS_TL(tail);
	}
	list = GET_CONS_TL(list);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
last(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list;
    EXTRACT_1_ARG(redex, list);
    if( IS_NIL(list) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("last on empty list"));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    g_ptr cur = list;
    g_ptr prev = cur;
    while( !IS_NIL(cur) ) {
	prev = cur;
	cur = GET_CONS_TL(cur);
    }
    g_ptr res = GET_CONS_HD(prev);
    // last is a projection function thus evaluate before overwriting
    res = Eval(res);
    OVERWRITE(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
gather(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list, indices;
    EXTRACT_2_ARGS(redex, list, indices);
    buffer buf;
    new_buf(&buf, 100, sizeof(g_ptr));
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	push_buf(&buf, (pointer) &e);
	list = GET_CONS_TL(list);
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(indices) ) {
	int idx = GET_INT(GET_CONS_HD(indices));
	g_ptr e = *((g_ptr *) M_LOCATE_BUF(&buf, idx-1));
	SET_CONS_HD(tail, e);
	INC_REFCNT(e);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	indices = GET_CONS_TL(indices);
    }
    free_buf(&buf);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
separate(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list, indices;
    EXTRACT_2_ARGS(redex, list, indices);
    buffer buf;
    new_buf(&buf, 100, sizeof(g_ptr));
    while( !IS_NIL(list) ) {
	g_ptr e = GET_CONS_HD(list);
	push_buf(&buf, (pointer) &e);
	list = GET_CONS_TL(list);
    }
    g_ptr sel = Make_NIL();
    g_ptr sel_tl = sel;
    while( !IS_NIL(indices) ) {
	int idx = GET_INT(GET_CONS_HD(indices));
	g_ptr e = *((g_ptr *) M_LOCATE_BUF(&buf, idx-1));
	if( e != NULL ) {
	    SET_CONS_HD(sel_tl, e);
	    INC_REFCNT(e);
	    SET_CONS_TL(sel_tl, Make_NIL());
	    sel_tl = GET_CONS_TL(sel_tl);
	    g_ptr null = NULL;
	    store_buf(&buf,idx-1, &null);
	}
	indices = GET_CONS_TL(indices);
    }
    g_ptr unsel = Make_NIL();
    g_ptr unsel_tl = unsel;
    g_ptr *gp;
    FOR_BUF(&buf, g_ptr, gp) {
	if( *gp != NULL ) {
	    SET_CONS_HD(unsel_tl, *gp);
	    INC_REFCNT(*gp);
	    SET_CONS_TL(unsel_tl, Make_NIL());
	    unsel_tl = GET_CONS_TL(unsel_tl);
	}
    }
    free_buf(&buf);
    MAKE_REDEX_PAIR(redex, sel, unsel);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
gcmp(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr n1, n2;
    EXTRACT_2_ARGS(redex, n1, n2);
    int res = Graph_cmp(n1, n2);
    MAKE_REDEX_INT(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

void
List_ops_Install_Functions()
{
    // Add builtin functions
    typeExp_ptr tv1 = GLnew_tVar();
    typeExp_ptr tv2 = GLnew_tVar();

    Add_ExtAPI_Function("qsort", "11", FALSE,
			GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_arrow(tv1,GLmake_int())),
			    GLmake_arrow(GLmake_list(tv1), GLmake_list(tv1))),
                        fl_qsort);

    Add_ExtAPI_Function("assoc", "11", FALSE,
                        GLmake_arrow(tv1,
			    GLmake_arrow(GLmake_list(GLmake_tuple(tv1, tv2)),
					  tv2)),
                        assoc);

    Add_ExtAPI_Function("rev_assoc", "11", FALSE,
                        GLmake_arrow(tv1,
			    GLmake_arrow(GLmake_list(GLmake_tuple(tv2, tv1)),
					  tv2)),
                        rev_assoc);

    Add_ExtAPI_Function("mem", "11", FALSE,
			GLmake_arrow(tv1,
				     GLmake_arrow(GLmake_list(tv1),
				     GLmake_bool())),
                        mem);

    Add_ExtAPI_Function("el", "10", FALSE,
                        GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_list(tv1), tv1)),
                        el);

    Add_ExtAPI_Function("find_all", "11", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_bool()),
			    GLmake_arrow(GLmake_list(tv1),
					 GLmake_list(GLmake_int()))),
                        find_all);

    Add_ExtAPI_Function("foreach", "10", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_void()),
			    GLmake_arrow(GLmake_list(tv1), GLmake_void())),
                        foreach_list_item);

    Add_ExtAPI_Function("sitlist", "-2-", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1, GLmake_arrow(tv2, tv2)),
			    GLmake_arrow(
				GLmake_list(tv1),
				GLmake_arrow(tv2, tv2))),
                        sitlist);

    Add_ExtAPI_Function("find_first0", "10", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_bool()),
			    GLmake_arrow(GLmake_list(tv1), GLmake_int())),
                        find_first0);

    Add_ExtAPI_Function("find", "10", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_bool()),
			    GLmake_arrow(GLmake_list(tv1), tv1)),
                        find);

    Add_ExtAPI_Function("filter", "11", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_bool()),
			    GLmake_arrow(GLmake_list(tv1), GLmake_list(tv1))),
                        filter);

    Add_ExtAPI_Function("rev", "2", FALSE,
                        GLmake_arrow( GLmake_list(tv1), GLmake_list(tv1)),
                        rev);

    Add_ExtAPI_Function("butlast", "2", FALSE,
                        GLmake_arrow( GLmake_list(tv1), GLmake_list(tv1)),
                        butlast);

    Add_ExtAPI_Function("firstn", "10", FALSE,
                        GLmake_arrow( GLmake_int(),
				      GLmake_arrow(
					GLmake_list(tv1), GLmake_list(tv1))),
                        firstn);

    Add_ExtAPI_Function("butfirstn", "10", FALSE,
                        GLmake_arrow( GLmake_int(),
				      GLmake_arrow(
					GLmake_list(tv1), GLmake_list(tv1))),
                        butfirstn);

    Add_ExtAPI_Function("lastn", "12", FALSE,
                        GLmake_arrow( GLmake_int(),
				      GLmake_arrow(
					GLmake_list(tv1), GLmake_list(tv1))),
                        lastn);

    Add_ExtAPI_Function("butlastn", "12", FALSE,
                        GLmake_arrow( GLmake_int(),
				      GLmake_arrow(
					GLmake_list(tv1), GLmake_list(tv1))),
                        butlastn);

    Add_ExtAPI_Function("length", "2", FALSE,
                        GLmake_arrow( GLmake_list(tv1), GLmake_int()),
                        length);

    Add_ExtAPI_Function("setify", "1", FALSE,
                        GLmake_arrow( GLmake_list(tv1), GLmake_list(tv1)),
                        setify);

    Add_ExtAPI_Function("flat", "3", FALSE,
                        GLmake_arrow( GLmake_list(GLmake_list(tv1)),
				      GLmake_list(tv1)),
                        flat);

    Add_ExtAPI_Function("split", "11", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,GLmake_bool()),
			    GLmake_arrow(GLmake_list(tv1),
					 GLmake_tuple(GLmake_list(tv1),
						      GLmake_list(tv1)))),
                        split);

    Add_ExtAPI_Function("partition", "11", FALSE,
                        GLmake_arrow(
			    GLmake_arrow(tv1,tv2),
			    GLmake_arrow(GLmake_list(tv1),
					 GLmake_list(GLmake_list(tv1)))),
                        partition);


    Add_ExtAPI_Function("append", "2-", FALSE,
                        GLmake_arrow(
			    GLmake_list(tv1),
			    GLmake_arrow(GLmake_list(tv1), GLmake_list(tv1))),
                        append);

    Add_ExtAPI_Function("union", "11", FALSE,
                        GLmake_arrow(
			    GLmake_list(tv1),
			    GLmake_arrow(GLmake_list(tv1), GLmake_list(tv1))),
                        my_union);

    Add_ExtAPI_Function("intersect", "11", FALSE,
                        GLmake_arrow(
			    GLmake_list(tv1),
			    GLmake_arrow(GLmake_list(tv1), GLmake_list(tv1))),
                        intersect);

    Add_ExtAPI_Function("subtract", "11", FALSE,
                        GLmake_arrow(
			    GLmake_list(tv1),
			    GLmake_arrow(GLmake_list(tv1), GLmake_list(tv1))),
                        subtract);

    Add_ExtAPI_Function("last", "2", FALSE,
                        GLmake_arrow(GLmake_list(tv1), tv1),
                        last);

    Add_ExtAPI_Function("gather", "21", FALSE,
                        GLmake_arrow(
			    GLmake_list(tv1),
			    GLmake_arrow(GLmake_list(GLmake_int()),
					 GLmake_list(tv1))),
                        gather);

    Add_ExtAPI_Function("separate", "21", FALSE,
                        GLmake_arrow(
			    GLmake_list(tv1),
			    GLmake_arrow(GLmake_list(GLmake_int()),
					 GLmake_tuple(
					    GLmake_list(tv1),
					    GLmake_list(tv1)))),
                        separate);

    Add_ExtAPI_Function("gcmp", "11", FALSE,
			GLmake_arrow(tv1, GLmake_arrow(tv1, GLmake_int())),
                        gcmp);

}


/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static assoc_tbl_ptr
get_assoc_tbl_record()
{
    assoc_tbl_ptr res;
    if( assoc_free_list != NULL ) {
	res = assoc_free_list;
	assoc_free_list = assoc_free_list->free_list;
	res->in_use = TRUE;
	res->free_list = NULL;
	return res;
    }
    res = (assoc_tbl_ptr) new_rec(&assoc_tbl_rec_mgr);
    res->in_use = TRUE;
    res->free_list = NULL;
    return res;
}
