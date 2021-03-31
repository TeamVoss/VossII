//------------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*									                                          */
/* Original author: Carl-Johan Seger, 2019                                    */
/*									                                          */
/******************************************************************************/
#include "strings.h"
#include "graph.h"
#include "pexlif.h"

/******************************************************************************/
/*                               GLOBAL VARIABLES                             */
/******************************************************************************/

string s_PINST;
string s_P_HIER;
string s_P_LEAF;
string s_W_UPDATE_FN;
string s_W_PHASE_DELAY;
string s_W_X;
string s_W_CONST;
string s_W_NAMED_CONST;
string s_W_VAR;
string s_W_EXPLICIT_VAR;
string s_W_AND;
string s_W_OR;
string s_W_NOT;
string s_W_PRED;
string s_W_EQ;
string s_W_GR;
string s_W_ADD;
string s_W_SUB;
string s_W_MUL;
string s_W_DIV;
string s_W_MOD;
string s_W_SHL;
string s_W_SHR;
string s_W_ASHR;
string s_W_SX;
string s_W_ZX;
string s_W_ITE;
string s_W_SLICE;
string s_W_NAMED_SLICE;
string s_W_UPDATE_NAMED_SLICE;
string s_W_CAT;
string s_W_MEM_READ;
string s_W_MEM_WRITE;
string s_MEM;
//
string s_no_instance;

// Global variables referenced -------------------------------------------------
extern str_mgr strings;

/******************************************************************************/
/*                              PRIVATE VARIABLES                             */
/******************************************************************************/
static char value_list_buf[1024];
// ...
static ustr_mgr        lstrings;
static rec_mgr         vec_mgr;
static rec_mgr         rng_mgr;
static rec_mgr         adj_mgr;
static rec_mgr_ptr     adj_mgr_ptr;
static rec_mgr         vec_tmp_mgr;
static rec_mgr_ptr     vec_tmp_mgr_ptr;
static rec_mgr         vec_tmp_lst_mgr;
static rec_mgr_ptr     vec_tmp_lst_mgr_ptr;
static hash_record     tbl_in;
static hash_record_ptr tbl_in_ptr;
static hash_record     tbl_out;
static hash_record_ptr tbl_out_ptr;

// Forward definitions local functions -----------------------------------------
static void    new_adj_mem();
static void    rem_adj_mem();
static vec_ptr split_vector(string name);
static void    record_vector_signatures(hash_record_ptr tbl, vec_tmp_ptr *tail, unint index, const vec_ptr vec);
static void    mk_adj_tables(vec_tmp_lst_ptr *keys, unint *count, g_ptr p);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

static void
new_adj_mem()
{
    adj_mgr_ptr = &adj_mgr;
    vec_tmp_mgr_ptr = &vec_tmp_mgr;
    vec_tmp_lst_mgr_ptr = &vec_tmp_lst_mgr;
    tbl_in_ptr = &tbl_in;
    tbl_out_ptr = &tbl_out;
    new_mgr(adj_mgr_ptr, sizeof(adj_rec));
    new_mgr(vec_tmp_mgr_ptr, sizeof(vec_tmp_rec));
    new_mgr(vec_tmp_lst_mgr_ptr, sizeof(vec_tmp_lst_rec));
    create_hash(tbl_in_ptr, 100, str_hash, str_equ);
    create_hash(tbl_out_ptr, 100, str_hash, str_equ);
}

static void
rem_adj_mem()
{
    free_mgr(adj_mgr_ptr);
    free_mgr(vec_tmp_mgr_ptr);
    free_mgr(vec_tmp_lst_mgr_ptr);
    dispose_hash(tbl_in_ptr, NULLFCN);
    dispose_hash(tbl_out_ptr, NULLFCN);
    adj_mgr_ptr = NULL;
    vec_tmp_mgr_ptr = NULL;
    vec_tmp_lst_mgr_ptr = NULL;
    tbl_in_ptr = NULL;
    tbl_out_ptr = NULL;
}

static vec_ptr
split_vector(string name)
{
    vec_ptr vp = Split_vector_name(&lstrings, &vec_mgr, &rng_mgr, name);
    for(vec_ptr p = vp; p != NULL; p = p->next) {
        if(p->type == TXT) {
            p->u.name = wastrsave(&strings, p->u.name);
        }
    }
    return vp;
}

#define FORMAL_OF_CONS(fa)                                                     \
    split_vector(GET_STRING(GET_FST(GET_CONS_HD(fa))))

#define ACTUAL_OF_CONS(fa)                                                     \
    split_vector(GET_STRING(GET_CONS_HD(fa)))

#define FOREACH_FORMAL(vec, fa)                                                \
    for( g_ptr li = fa                                                         \
       ; !IS_NIL(li) && (vec = FORMAL_OF_CONS(li), TRUE)                       \
       ; li = GET_CONS_TL(li))

#define FOREACH_ACTUAL(vec, fa)                                                \
    for( g_ptr li = fa                                                         \
        ; !IS_NIL(li)                                                          \
        ; li = GET_CONS_TL(li))                                                \
        for( g_ptr as = GET_SND(GET_CONS_HD(li))                               \
           ; !IS_NIL(as) && (vec = ACTUAL_OF_CONS(as), TRUE)                   \
           ; as = GET_CONS_TL(as))

static void
record_vector_signatures(hash_record_ptr tbl, vec_tmp_ptr *tail, unint index, const vec_ptr vec)
{
    string key = Get_vector_signature(&lstrings, vec);
    // Record signature as we'll need it later again.
    vec_tmp_ptr n = (vec_tmp_ptr) new_rec(vec_tmp_mgr_ptr);
    n->signature = key;
    n->vec = vec;
    n->next = NULL;
    (*tail) = n;
    // Record vec. in tabel.
    adj_ptr bkt = (adj_ptr) find_hash(tbl, key);
    if(bkt == NULL) {
        adj_ptr b = (adj_ptr) new_rec(adj_mgr_ptr);
        b->index = index;
        b->vec = vec;
        b->next = NULL;
        insert_hash(tbl, key, b);
    } else {
        // todo: why not record as head since order does not matter? 
        while(bkt->next != NULL) {
            bkt = bkt->next;
        }
        adj_ptr b = (adj_ptr) new_rec(adj_mgr_ptr);
        b->index = index;
        b->vec = vec;
        b->next = NULL;
        bkt->next = b;
    }
}

static void
mk_adj_tables(vec_tmp_lst_ptr *keys, unint *count, g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        DIE("Bad input");
    }    
    if(is_P_LEAF(cont, &fns)) {
        DIE("Bad input");
    }
    if(is_P_HIER(cont, &children)) {
        vec_ptr vec;
        vec_tmp_ptr key = NULL, *key_tail = &key;
        // Record vector names for parent's formals.
        // todo: count outputs as inputs for the environment and vice versa.
        FOREACH_FORMAL(vec, fa_inps) {
            record_vector_signatures(tbl_out_ptr, key_tail, 0, vec);
            key_tail = &(*key_tail)->next;
        }
        FOREACH_FORMAL(vec, fa_outs) {
            record_vector_signatures(tbl_in_ptr, key_tail, 0, vec);
            key_tail = &(*key_tail)->next;
        }
        vec_tmp_lst_ptr key_lst = (vec_tmp_lst_ptr) new_rec(vec_tmp_lst_mgr_ptr);
        key_lst->vec = key;
        key_lst->next = NULL;
        *keys = key_lst;
        vec_tmp_lst_ptr *key_lst_tail = &key_lst->next;
        // Record vector names for each child's actuals.
        *count = 1;
        g_ptr child, tmp;
        FOR_CONS(children, tmp, child) {
            if(!is_PINST(child, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
                DIE("Bad input");
            }
            vec_tmp_ptr key = NULL, *key_tail = &key;
            FOREACH_ACTUAL(vec, fa_inps) {
                record_vector_signatures(tbl_in_ptr, key_tail, *count, vec);
                key_tail = &(*key_tail)->next;
            }
            FOREACH_ACTUAL(vec, fa_outs) {
                record_vector_signatures(tbl_out_ptr, key_tail, *count, vec);
                key_tail = &(*key_tail)->next;
            }
            vec_tmp_lst_ptr key_lst = (vec_tmp_lst_ptr) new_rec(vec_tmp_lst_mgr_ptr);
            key_lst->vec = key;
            key_lst->next = NULL;
            *key_lst_tail = key_lst;
            key_lst_tail = &key_lst->next;
            *count = *count + 1;
        }
        return;
    }
    DIE("Impossible");
}

/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

string
get_top_name(g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, internals, content;
    string name;
    bool leaf;
    is_PINST(p,&name,&attrs,&leaf,&fa_inps,&fa_outs,&internals,&content);
    return name;
}

int
get_top_size(g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        return -1;
    }
    if(is_P_LEAF(cont, &fns)) {
        return 1;
    }
    if(is_P_HIER(cont, &children)) {
        return 1 + List_length(children);
    }
    return -1;
}

g_ptr
get_top_adjacencies(g_ptr p)
{
    new_adj_mem();
    // /
    unint count;
    adj_ptr bkt;
    vec_tmp_ptr key;
    vec_tmp_lst_ptr keys;
    mk_adj_tables(&keys, &count, p);
    // /
    g_ptr res = Make_NIL(), res_tail = res;
    for(unint i = 0; keys != NULL; i++, keys = keys->next) {
        g_ptr lhs = Make_INT_leaf(i);
        for(key = keys->vec; key != NULL; key = key->next) {
            vec_ptr vec = key->vec;
            string signature = key->signature;
            bkt = (adj_ptr) find_hash(tbl_in_ptr, signature);
            while(bkt != NULL) {
                if(Check_vector_overlap(vec, bkt->vec)) {
                    g_ptr rhs = Make_INT_leaf(bkt->index);
                    g_ptr pair = Make_PAIR_ND(lhs,rhs);
                    APPEND1(res_tail, pair);
                }
                bkt = bkt->next;
            }
            bkt = (adj_ptr) find_hash(tbl_out_ptr, signature);
            while(bkt != NULL) {
                if(Check_vector_overlap(vec, bkt->vec)) {
                    g_ptr rhs = Make_INT_leaf(bkt->index);
                    g_ptr pair = Make_PAIR_ND(rhs,lhs);
                    APPEND1(res_tail, pair);
                }
                bkt = bkt->next;
            }
        }
    }
    // /
    rem_adj_mem();
    return res;
}

string
find_value_list(g_ptr attrs, string name)
{
    sprintf(value_list_buf, "node_values_%s", name);
    while( !IS_NIL(attrs) ) {
	g_ptr key = GET_CONS_HD(GET_CONS_HD(attrs));
	if( strcmp(value_list_buf, GET_STRING(key)) == 0 ) {
	    return( GET_STRING(GET_CONS_TL(GET_CONS_HD(attrs))) );
	}
	attrs = GET_CONS_TL(attrs);
    }
    return NULL;
}

string
find_instance_name(g_ptr attrs)
{
    while( !IS_NIL(attrs) ) {
        g_ptr key = GET_CONS_HD(GET_CONS_HD(attrs));
        if( strcmp(GET_STRING(key), "instance") == 0 ) {
            return( GET_STRING(GET_CONS_TL(GET_CONS_HD(attrs))) );
        }
        attrs = GET_CONS_TL(attrs);
    }
    return s_no_instance;        
}

// Destr. Functions ------------------------------------------------------------

void
destr_PINST(g_ptr node,
	 g_ptr  *namep,
	 g_ptr  *attrsp,
	 g_ptr  *leafp,
	 g_ptr  *fa_inpsp,
	 g_ptr  *fa_outsp,
	 g_ptr  *internalsp,
	 g_ptr  *contentp)
{
    DEST_GET(*contentp)
    DEST_GET(*internalsp)
    DEST_GET(*fa_outsp)
    DEST_GET(*fa_inpsp)
    DEST_GET(*leafp)
    DEST_GET(*attrsp)
    DEST_GET(*namep)
    ASSERT( IS_CONSTRUCTOR(PINST, node) );
}

bool
destr_MEM(g_ptr node, int *a_szp, int *linesp, int *d_szp)
{
    g_ptr a_sz, lines, d_sz;
    EXTRACT(d_sz);
    EXTRACT(lines);
    EXTRACT(a_sz);
    if( !IS_CONSTRUCTOR(MEM, node) ) { return FALSE; }
    *a_szp = GET_INT(a_sz);
    *d_szp = GET_INT(d_sz);
    *linesp = GET_INT(lines);
    return TRUE;
}

// Is. Functions ---------------------------------------------------------------

bool
is_PINST(g_ptr node,
	 string *namep,
	 g_ptr  *attrsp,
	 bool   *leafp,
	 g_ptr  *fa_inpsp,
	 g_ptr  *fa_outsp,
	 g_ptr  *internalsp,
	 g_ptr  *contentp)
{
    g_ptr gname, gleaf;
    EXTRACT(*contentp)
    EXTRACT(*internalsp)
    EXTRACT(*fa_outsp)
    EXTRACT(*fa_inpsp)
    EXTRACT(gleaf)
    EXTRACT(*attrsp)
    EXTRACT(gname)
    if( !IS_CONSTRUCTOR(PINST, node) ) { return FALSE; }
    ASSERT( IS_LEAF(gname) && IS_STRING(gname) );
    *namep = GET_STRING(gname);
    ASSERT( IS_LEAF(gleaf) && IS_BOOL(gleaf) );
    *leafp = (GET_BOOL(gleaf) == B_One())? TRUE : FALSE;
    return TRUE;
}

bool
is_P_HIER(g_ptr node, g_ptr *childrenp)
{
    EXTRACT(*childrenp)
    if( !IS_CONSTRUCTOR(P_HIER, node) ) { return FALSE; }
    return TRUE;
}

bool
is_P_LEAF(g_ptr node, g_ptr *fnsp)
{
    EXTRACT(*fnsp)
    if( !IS_CONSTRUCTOR(P_LEAF, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_UPDATE_FN(g_ptr node, g_ptr *lhsp, g_ptr *rhsp)
{
    EXTRACT(*rhsp)
    EXTRACT(*lhsp)
    if( !IS_CONSTRUCTOR(W_UPDATE_FN, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_PHASE_DELAY(g_ptr node, g_ptr *lhsp, g_ptr *rhsp)
{
    EXTRACT(*rhsp)
    EXTRACT(*lhsp)
    if( !IS_CONSTRUCTOR(W_PHASE_DELAY, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_NOT(g_ptr node, g_ptr *subp)
{
    EXTRACT(*subp);
    if( !IS_CONSTRUCTOR(W_NOT, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_PRED(g_ptr node, string *namep, g_ptr *subp)
{
    g_ptr nm;
    EXTRACT(*subp);
    EXTRACT(nm);
    if( !IS_CONSTRUCTOR(W_PRED, node) ) { return FALSE; }
    ASSERT( IS_LEAF(nm) && IS_STRING(nm) );
    *namep = GET_STRING(nm);
    return TRUE;
}

bool
is_W_X(g_ptr node, int *szp)
{
    g_ptr size;
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_X, node) ) { return FALSE; }
    *szp = GET_INT(size);
    return TRUE;
}

bool
is_W_CONST(g_ptr node, int *szp, arbi_T *valp)
{
    g_ptr value, size;
    EXTRACT(value);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_CONST, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *valp = GET_AINT(value);
    return TRUE;
}

bool
is_W_NAMED_CONST(g_ptr node, string *namep, int *szp, arbi_T *valp)
{
    g_ptr value, size, name;
    EXTRACT(value);
    EXTRACT(size);
    EXTRACT(name);
    if( !IS_CONSTRUCTOR(W_NAMED_CONST, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *valp = GET_AINT(value);
    *namep = GET_STRING(name);
    return TRUE;
}

bool
is_W_VAR(g_ptr node, int *szp, string *basep)
{
    g_ptr base, size;
    EXTRACT(base);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_VAR, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *basep = GET_STRING(base);
    return TRUE;
}

bool
is_W_EXPLICIT_VAR(g_ptr node, int *szp, string *namep)
{
    g_ptr name, size;
    EXTRACT(name);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_EXPLICIT_VAR, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *namep = GET_STRING(name);
    return TRUE;
}

bool
is_W_SX(g_ptr node, int *szp, g_ptr *ep)
{
    g_ptr size;
    EXTRACT(*ep);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_SX, node) ) { return FALSE; }
    *szp = GET_INT(size);
    return TRUE;
}

bool
is_W_ZX(g_ptr node, int *szp, g_ptr *ep)
{
    g_ptr size;
    EXTRACT(*ep);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_ZX, node) ) { return FALSE; }
    *szp = GET_INT(size);
    return TRUE;
}

bool
is_W_ITE(g_ptr node, g_ptr *condp, g_ptr *tp, g_ptr *ep)
{
    EXTRACT(*ep);
    EXTRACT(*tp);
    EXTRACT(*condp);
    if( !IS_CONSTRUCTOR(W_ITE, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_SLICE(g_ptr node, g_ptr *idxlistp, g_ptr *ep)
{
    EXTRACT(*ep);
    EXTRACT(*idxlistp);
    if( !IS_CONSTRUCTOR(W_SLICE, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_NAMED_SLICE(g_ptr node, string *namep, g_ptr *idxlistp, g_ptr *ep)
{
    g_ptr nm;
    EXTRACT(*ep);
    EXTRACT(*idxlistp);
    EXTRACT(nm);
    if( !IS_CONSTRUCTOR(W_NAMED_SLICE, node) ) { return FALSE; }
    *namep = GET_STRING(nm);
    return TRUE;
}

bool
is_W_UPDATE_NAMED_SLICE(g_ptr node, g_ptr *bp, string *namep, g_ptr *idxlistp, g_ptr *ep)
{
    g_ptr nm;
    EXTRACT(*ep);
    EXTRACT(*idxlistp);
    EXTRACT(nm);
    EXTRACT(*bp);
    if( !IS_CONSTRUCTOR(W_UPDATE_NAMED_SLICE, node) ) { return FALSE; }
    *namep = GET_STRING(nm);
    return TRUE;
}

bool
is_W_CAT(g_ptr node, g_ptr *listp)
{
    EXTRACT(*listp);
    if( !IS_CONSTRUCTOR(W_CAT, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_MEM_READ(g_ptr node, int *a_szp, int *linesp, int *d_szp, g_ptr *memp, g_ptr *addrp)
{
    g_ptr info;
    EXTRACT(*addrp);
    EXTRACT(*memp);
    EXTRACT(info);
    if( !IS_CONSTRUCTOR(W_MEM_READ, node) ) { return FALSE; }
    if( !destr_MEM(info, a_szp, linesp, d_szp) ) { return FALSE; }
    return TRUE;
}

bool
is_W_MEM_WRITE(g_ptr node, int *a_szp, int *linesp, int *d_szp, g_ptr *memp, g_ptr *addrp, g_ptr *datap)
{
    g_ptr info;
    EXTRACT(*datap);
    EXTRACT(*addrp);
    EXTRACT(*memp);
    EXTRACT(info);
    if( !IS_CONSTRUCTOR(W_MEM_WRITE, node) ) { return FALSE; }
    if( !destr_MEM(info, a_szp, linesp, d_szp) ) { return FALSE; }
    return TRUE;
}

// Mk. Functions ---------------------------------------------------------------

g_ptr
mk_PINST(g_ptr name, g_ptr attrs, g_ptr leaf, g_ptr fa_inps, g_ptr fa_outs, g_ptr internals, g_ptr content)
{
    g_ptr res = Make_STRING_leaf(s_PINST);
    res = Make_CONS_ND(res, name);
    res = Make_CONS_ND(res, attrs);
    res = Make_CONS_ND(res, leaf);
    res = Make_CONS_ND(res, fa_inps);
    res = Make_CONS_ND(res, fa_outs);
    res = Make_CONS_ND(res, internals);
    res = Make_CONS_ND(res, content);
    return res;
}

g_ptr
mk_P_HIER(g_ptr children)
{
    g_ptr res = Make_STRING_leaf(s_P_HIER);
    res = Make_CONS_ND(res, children);
    return res;
}


g_ptr
mk_P_LEAF(g_ptr fns)
{
    g_ptr res = Make_STRING_leaf(s_P_LEAF);
    res = Make_CONS_ND(res, fns);
    return res;
}

g_ptr
mk_W_UPDATE_FN(g_ptr lhs, g_ptr rhs)
{
    g_ptr res = Make_STRING_leaf(s_W_UPDATE_FN);
    res = Make_CONS_ND(res, lhs);
    res = Make_CONS_ND(res, rhs);
    return res;
}

g_ptr
mk_W_PHASE_DELAY(g_ptr lhs, g_ptr rhs)
{
    g_ptr res = Make_STRING_leaf(s_W_PHASE_DELAY);
    res = Make_CONS_ND(res, lhs);
    res = Make_CONS_ND(res, rhs);
    return res;
}

g_ptr
mk_W_X(g_ptr sz)
{
    g_ptr res = Make_STRING_leaf(s_W_X);
    res = Make_CONS_ND(res, sz);
    return res;
}


g_ptr
mk_W_CONST(g_ptr sz, g_ptr v)
{
    g_ptr res = Make_STRING_leaf(s_W_CONST);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, v);
    return res;
}

g_ptr
mk_W_NAMED_CONST(g_ptr name, g_ptr sz, g_ptr v)
{
    g_ptr res = Make_STRING_leaf(s_W_NAMED_CONST);
    res = Make_CONS_ND(res, name);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, v);
    return res;
}

g_ptr
mk_W_VAR(g_ptr sz, g_ptr base)
{
    g_ptr res = Make_STRING_leaf(s_W_VAR);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, base);
    return res;
}

g_ptr
mk_W_EXPLICIT_VAR(g_ptr sz, g_ptr name)
{
    g_ptr res = Make_STRING_leaf(s_W_EXPLICIT_VAR);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, name);
    return res;
}

g_ptr
mk_W_AND(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_AND);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_OR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_OR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_NOT(g_ptr a)
{
    g_ptr res = Make_STRING_leaf(s_W_NOT);
    res = Make_CONS_ND(res, a);
    return res;
}

g_ptr
mk_W_EQ(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_EQ);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_PRED(g_ptr name, g_ptr cond)
{
    g_ptr res = Make_STRING_leaf(s_W_PRED);
    res = Make_CONS_ND(res, name);
    res = Make_CONS_ND(res, cond);
    return res;
}

g_ptr
mk_W_GR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_GR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_ADD(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_ADD);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SUB(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_SUB);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_MUL(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_MUL);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_DIV(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_DIV);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_MOD(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_MOD);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SHL(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_SHL);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SHR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_SHR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_ASHR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_ASHR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SX(g_ptr sz, g_ptr w)
{
    g_ptr res = Make_STRING_leaf(s_W_SX);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, w);
    return res;
}

g_ptr
mk_W_ZX(g_ptr sz, g_ptr w)
{
    g_ptr res = Make_STRING_leaf(s_W_ZX);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, w);
    return res;
}

g_ptr
mk_W_ITE(g_ptr cond, g_ptr t, g_ptr e)
{
    g_ptr res = Make_STRING_leaf(s_W_ITE);
    res = Make_CONS_ND(res, cond);
    res = Make_CONS_ND(res, t);
    res = Make_CONS_ND(res, e);
    return res;
}

g_ptr
mk_W_SLICE(g_ptr indices, g_ptr w)
{
    g_ptr res = Make_STRING_leaf(s_W_SLICE);
    res = Make_CONS_ND(res, indices);
    res = Make_CONS_ND(res, w);
    return res;
}

g_ptr
mk_W_CAT(g_ptr parts)
{
    g_ptr res = Make_STRING_leaf(s_W_CAT);
    res = Make_CONS_ND(res, parts);
    return res;
}

g_ptr
mk_MEM(g_ptr addr_size, g_ptr lines, g_ptr data_size)
{
    g_ptr res = Make_STRING_leaf(s_MEM);
    res = Make_CONS_ND(res, addr_size);
    res = Make_CONS_ND(res, lines);
    res = Make_CONS_ND(res, data_size);
    return res;
}

g_ptr
mk_W_MEM_READ(g_ptr info, g_ptr mem, g_ptr addr)
{
    g_ptr res = Make_STRING_leaf(s_W_MEM_READ);
    res = Make_CONS_ND(res, info);
    res = Make_CONS_ND(res, mem);
    res = Make_CONS_ND(res, addr);
    return res;
}

g_ptr
mk_W_MEM_WRITE(g_ptr info, g_ptr mem, g_ptr addr, g_ptr data)
{
    g_ptr res = Make_STRING_leaf(s_W_MEM_WRITE);
    res = Make_CONS_ND(res, info);
    res = Make_CONS_ND(res, mem);
    res = Make_CONS_ND(res, addr);
    res = Make_CONS_ND(res, data);
    return res;
}

// ? ---------------------------------------------------------------------------

void
Pexlif_Init()
{
    s_PINST                = Mk_constructor_name("PINST");
    s_P_HIER               = Mk_constructor_name("P_HIER");
    s_P_LEAF               = Mk_constructor_name("P_LEAF");
    s_W_UPDATE_FN          = Mk_constructor_name("W_UPDATE_FN");
    s_W_PHASE_DELAY        = Mk_constructor_name("W_PHASE_DELAY");
    s_W_X                  = Mk_constructor_name("W_X");
    s_W_CONST              = Mk_constructor_name("W_CONST");
    s_W_NAMED_CONST        = Mk_constructor_name("W_NAMED_CONST");
    s_W_VAR                = Mk_constructor_name("W_VAR");
    s_W_EXPLICIT_VAR       = Mk_constructor_name("W_EXPLICIT_VAR");
    s_W_AND                = Mk_constructor_name("W_AND");
    s_W_OR                 = Mk_constructor_name("W_OR");
    s_W_NOT                = Mk_constructor_name("W_NOT");
    s_W_PRED               = Mk_constructor_name("W_PRED");
    s_W_EQ                 = Mk_constructor_name("W_EQ");
    s_W_GR                 = Mk_constructor_name("W_GR");
    s_W_ADD                = Mk_constructor_name("W_ADD");
    s_W_SUB                = Mk_constructor_name("W_SUB");
    s_W_MUL                = Mk_constructor_name("W_MUL");
    s_W_DIV                = Mk_constructor_name("W_DIV");
    s_W_MOD                = Mk_constructor_name("W_MOD");
    s_W_SHL                = Mk_constructor_name("W_SHL");
    s_W_SHR                = Mk_constructor_name("W_SHR");
    s_W_ASHR               = Mk_constructor_name("W_ASHR");
    s_W_SX                 = Mk_constructor_name("W_SX");
    s_W_ZX                 = Mk_constructor_name("W_ZX");
    s_W_ITE                = Mk_constructor_name("W_ITE");
    s_W_SLICE              = Mk_constructor_name("W_SLICE");
    s_W_NAMED_SLICE        = Mk_constructor_name("W_NAMED_SLICE");
    s_W_UPDATE_NAMED_SLICE = Mk_constructor_name("W_UPDATE_NAMED_SLICE");
    s_W_CAT                = Mk_constructor_name("W_CAT");
    s_W_MEM_READ           = Mk_constructor_name("W_MEM_READ");
    s_W_MEM_WRITE          = Mk_constructor_name("W_MEM_WRITE");
    s_MEM                  = Mk_constructor_name("MEM");
    // /
    s_no_instance = wastrsave(&strings, "{}");
}

void
Pexlif_Install_Functions()
{
    new_ustrmgr(&lstrings);
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
}

/******************************************************************************/
/*                               DUMMY FUNCTIONS                              */
/******************************************************************************/

// ...
