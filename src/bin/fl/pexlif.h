//------------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/* Original author: Carl-Johan Seger 2019                                     */
/*                                                                            */
/******************************************************************************/
/* pexlif.h -- header for pexlif.c */
#ifdef EXPORT_FORWARD_DECL

// Function prototypes for public functions ------------------------------------
string get_top_name(g_ptr p);
int    get_top_size(g_ptr p);
g_ptr  get_top_inst(g_ptr p, unint index);
g_ptr  get_top_adjacencies(g_ptr p);
g_ptr  fold_pexlif(g_ptr p, g_ptr ids, string name);
g_ptr  unfold_pexlif(g_ptr p, unint id);
string find_value_list(g_ptr attrs, string name);
string find_instance_name(g_ptr attrs);
// destructors.
void destr_PINST(g_ptr node,g_ptr *namep, g_ptr *attrsp, g_ptr *leafp, g_ptr *fa_inpsp, g_ptr *fa_outsp, g_ptr *internalsp, g_ptr *contentp);
bool destr_MEM(g_ptr node, int *a_szp, int *linesp, int *d_szp);
// constructors.
g_ptr mk_PINST(g_ptr name, g_ptr attrs, g_ptr leaf, g_ptr fa_inps, g_ptr fa_outs, g_ptr internals, g_ptr content);
g_ptr mk_P_HIER(g_ptr children);
g_ptr mk_P_LEAF(g_ptr fns);
g_ptr mk_MEM(g_ptr addr_size, g_ptr lines, g_ptr data_size);
g_ptr mk_W_X(g_ptr sz);
g_ptr mk_W_CONST(g_ptr sz, g_ptr v);
g_ptr mk_W_NAMED_CONST(g_ptr name, g_ptr sz, g_ptr v);
g_ptr mk_W_VAR(g_ptr sz, g_ptr base);
g_ptr mk_W_EXPLICIT_VAR(g_ptr sz, g_ptr name);
g_ptr mk_W_AND(g_ptr a, g_ptr b);
g_ptr mk_W_OR(g_ptr a, g_ptr b);
g_ptr mk_W_NOT(g_ptr a);
g_ptr mk_W_EQ(g_ptr a, g_ptr b);
g_ptr mk_W_PRED(g_ptr name, g_ptr cond);
g_ptr mk_W_GR(g_ptr a, g_ptr b);
g_ptr mk_W_ADD(g_ptr a, g_ptr b);
g_ptr mk_W_SUB(g_ptr a, g_ptr b);
g_ptr mk_W_MUL(g_ptr a, g_ptr b);
g_ptr mk_W_DIV(g_ptr a, g_ptr b);
g_ptr mk_W_MOD(g_ptr a, g_ptr b);
g_ptr mk_W_SHL(g_ptr a, g_ptr b);
g_ptr mk_W_SHR(g_ptr a, g_ptr b);
g_ptr mk_W_ASHR(g_ptr a, g_ptr b);
g_ptr mk_W_SX(g_ptr sz, g_ptr w);
g_ptr mk_W_ZX(g_ptr sz, g_ptr w);
g_ptr mk_W_ITE(g_ptr cond, g_ptr t, g_ptr e);
g_ptr mk_W_SLICE(g_ptr indices, g_ptr w);
g_ptr mk_W_CAT(g_ptr parts);
g_ptr mk_W_IDELAY(g_ptr sz, g_ptr mr, g_ptr Mr, g_ptr mf, g_ptr Mf,
		  g_ptr cur, g_ptr del_inps);
g_ptr mk_W_MEM_READ(g_ptr info, g_ptr mem, g_ptr addr);
g_ptr mk_W_MEM_WRITE(g_ptr info, g_ptr mem, g_ptr addr, g_ptr data);
g_ptr mk_W_UPDATE_FN(g_ptr lhs, g_ptr rhs);
g_ptr mk_W_PHASE_DELAY(g_ptr lhs, g_ptr rhs);
// 'is' tests.
bool is_PINST(g_ptr node, string *namep, g_ptr *attrsp, bool *leafp, g_ptr *fa_inpsp, g_ptr *fa_outsp, g_ptr *internalsp, g_ptr *contentp);
bool is_P_HIER(g_ptr node, g_ptr *childrenp);
bool is_P_LEAF(g_ptr node, g_ptr *fnsp);
bool is_W_UPDATE_FN(g_ptr node, g_ptr *lhsp, g_ptr *rhsp);
bool is_W_PHASE_DELAY(g_ptr node, g_ptr *lhsp, g_ptr *rhsp);
bool is_W_NOT(g_ptr node, g_ptr *subp);
bool is_W_PRED(g_ptr node, string *namep, g_ptr *subp);
bool is_W_X(g_ptr node, int *szp);
bool is_W_CONST(g_ptr node, int *szp, arbi_T *valp);
bool is_W_NAMED_CONST(g_ptr node, string *namep, int *szp, arbi_T *valp);
bool is_W_VAR(g_ptr node, int *szp, string *basep);
bool is_W_EXPLICIT_VAR(g_ptr node, int *szp, string *namep);
bool is_W_SX(g_ptr node, int *szp, g_ptr *ep);
bool is_W_ZX(g_ptr node, int *szp, g_ptr *ep);
bool is_W_ITE(g_ptr node, g_ptr *condp, g_ptr *tp, g_ptr *ep);
bool is_W_SLICE(g_ptr node, g_ptr *idxlistp, g_ptr *ep);
bool is_W_NAMED_SLICE(g_ptr node, string *namep, g_ptr *idxlistp, g_ptr *ep);
bool is_W_UPDATE_NAMED_SLICE(g_ptr node, g_ptr *basep, string *namep, g_ptr *idxlistp, g_ptr *ep);
bool is_W_CAT(g_ptr node, g_ptr *listp);
bool is_W_IDELAY(g_ptr node, int *sz, int *mr, int *Mr, int *mf, int *Mf,
		 g_ptr *curp, g_ptr *del_inps);
bool is_W_MEM_READ(g_ptr node, int *a_szp, int *linesp, int *d_szp, g_ptr *memp, g_ptr *addrp);
bool is_W_MEM_WRITE(g_ptr node, int *a_szp, int *linesp, int *d_szp, g_ptr *memp, g_ptr *addrp, g_ptr *datap);
// ...
void Pexlif_Init();
void Pexlif_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
// Main include file -----------------------------------------------------------
#ifndef PEXLIF_H
#define PEXLIF_H

#include "fl.h"	/* Global data types and include files */

typedef struct adj_bkt_rec *adj_bkt_ptr;
typedef struct adj_bkt_rec {
    unint       index;     // index as found in (parent : children).
    vec_ptr     vec;
    adj_bkt_ptr next;
} adj_bkt_rec;

typedef struct adj_key_rec *adj_key_ptr;
typedef struct adj_key_rec {
    string      name;      // orig. name of vec.
    string      signature;
    bool        input;     // found in 'fa_inps'(T) or 'fa_outs'(F)?
    vec_ptr     vec;
    adj_key_ptr next;
} adj_key_rec;

typedef struct adj_key_list_rec *adj_key_list_ptr;
typedef struct adj_key_list_rec {
    adj_key_ptr      key;
    adj_key_list_ptr next;
} adj_key_list_rec;

typedef struct fa_subst_rec *fa_subst_ptr;
typedef struct fa_subst_rec {
    string          formal;
    sname_list_ptr  actuals;
    hash_record_ptr subst;
} fa_subst_rec;

typedef struct signature_rec {
	string		name;
	string		signature;
	g_ptr		fa_inps;
	g_ptr		fa_outs;
} signature_rec;

#define DEST_GET(n)							       \
	ASSERT( GET_TYPE(node) == CONS_ND );				       \
	(n)  = GET_SND(node);						       \
	node = GET_FST(node);

#define IS_CONSTRUCTOR(name,nd)                                                \
    (IS_LEAF(nd) && IS_STRING(nd) && STREQ(GET_STRING(nd), s_##name))

#define EXTRACT(n)						               \
	if( GET_TYPE(node) != CONS_ND ) { return FALSE; }	               \
	(n)  = GET_SND(node);					               \
	node = GET_FST(node);

#define FORMAL_OF_CONS(fa)                                                     \
    GET_STRING(GET_FST(GET_CONS_HD(fa)))

#define ACTUAL_OF_CONS(fa)                                                     \
    GET_STRING(GET_CONS_HD(fa))

#define INTERNAL_OF_CONS(is)                                                   \
    GET_STRING(GET_CONS_HD(is))

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

#define SCAN_HASH(hp, bpp, k, d)                                               \
    ASSERT((hp)->initialized == HASH_MAGIC_NBR);                               \
    FOR_BUF(&((hp)->table), bucket_ptr, bpp)                                   \
        for( bucket_ptr chp = *bpp                                             \
           ; chp != NULL                                                       \
             && (k = chp->key, TRUE)                                           \
             && (d = chp->data, TRUE)                                          \
           ; chp = chp->next)                                                  \

#endif /* PEXLIF_H */
#endif /* EXPORT_FORWARD_DECL */
