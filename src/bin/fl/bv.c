//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2019			*/
/*									*/
/************************************************************************/
#include "bv.h"
#include "graph.h"

#include <math.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     strings;
extern FILE        *odests_fp;
extern bool     Do_gc_asap;

/***** PRIVATE VARIABLES *****/
static int	    bv_oidx;
static typeExp_ptr  bv_handle_tp;
static rec_mgr	    bv_rec_mgr;
static bv_ptr	    bv_free_list;
static char	    bv_str_buf[4096];
static g_ptr	    Zero;
static g_ptr	    One;

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static g_ptr
trim_bv(g_ptr l)
{
    while (1) {
	if( IS_NIL(l) ) return l;
	g_ptr next = GET_CONS_TL(l);
	if( IS_NIL(next) ) return l;
	if( GET_BOOL(GET_CONS_HD(l)) == GET_BOOL(GET_CONS_HD(next)) ) {
	    l = next;
	} else {
	    return l;
	}
    }
}

static bv_ptr
get_bv_rec(g_ptr l)
{
    bv_ptr res;
    if( bv_free_list != NULL ) {
	res = bv_free_list;
	bv_free_list = bv_free_list->u.next;
    } else {
	res = (bv_ptr) new_rec(&bv_rec_mgr);
    }
    res->flag = 0;
    res->u.l = trim_bv(l);
    return res;
}

static void
mark_bv_fn(pointer p)
{
    bv_ptr fp = (bv_ptr) p;
    fp->flag = 1;
    Mark(fp->u.l);
    return;
}

static void
sweep_bv_fn(void)
{
    bv_ptr fp;
    bv_free_list = NULL;
    FOR_REC(&bv_rec_mgr, bv_ptr, fp) {
	if( fp->flag == 1 ) {
	    fp->flag = 0;
	} else {
	    fp->u.next = bv_free_list;
	    bv_free_list = fp;
	}
    }
}

static int
get_bv_length(g_ptr l)
{
    int len = 0;
    while( !IS_NIL(l) ) {
	len++;
	l = GET_CONS_TL(l);
    }
    return len;
}

static void
save_bv_fn(FILE *fp, pointer p)
{
    bv_ptr bvp = (bv_ptr) p;
    g_ptr l = bvp->u.l;
    fprintf(fp, "%d", get_bv_length(l));
    for(g_ptr np = l; !IS_NIL(np); np = GET_CONS_TL(np)) {
	formula f = GET_BOOL(GET_CONS_HD(np));
	fprintf(fp, "  %d", Save_get_bool_idx(f));
    }
    fputc('\n', fp);	// Add newline for human readability
}

static pointer
load_bv_fn(FILE *fp)
{
    int len;
    if( fscanf(fp, "%d", &len) != 1 ) {
	return NULL;
    }
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    for(int i = 0; i < len; i++) {
	int idx;
	if( fscanf(fp, " %d", &idx) != 1 ) {
	    return NULL;
	}
	formula f = Load_get_bool_from_idx(idx);
	APPEND1(tail, Make_BOOL_leaf(f));
    }
    fgetc(fp);	// Eat new line
    return( (pointer) get_bv_rec(res) );
}

static bool
is_symbolic(formula f)
{
    return( f != B_One() && f != B_Zero() );
}

static string
bv_list2str(g_ptr l, int depth)
{
    if((odests_fp = fmemopen(bv_str_buf, 4096, "w")) == NULL ) {
	DIE("Should never happen");
    }
    char sep = '<';
    for(g_ptr np = l; !IS_NIL(np); np = GET_CONS_TL(np)) {
	formula f = GET_BOOL(GET_CONS_HD(np));
	FP(FILE_fp,"%c", sep);
	sep = ',';
	if( depth == 0 && is_symbolic(f) ) {
	    FP(FILE_fp, "S");
	} else {
	    B_Print(FILE_fp, f,depth);
	}
    }
    FP(FILE_fp, ">");
    fclose(odests_fp);
    odests_fp = NULL;
    return( wastrsave(&strings, bv_str_buf) );
}

static string
bv2str_fn(pointer p)
{
    bv_ptr bp = (bv_ptr) p;
    g_ptr l = bp->u.l;
    return( bv_list2str(l, -1) );
}

static formula
bv_eq_fn(pointer p1, pointer p2, bool identical)
{
    bv_ptr bp1 = (bv_ptr) p1;
    bv_ptr bp2 = (bv_ptr) p2;
    g_ptr   l1 = bp1->u.l;
    g_ptr   l2 = bp2->u.l;
    int	    len1 = get_bv_length(l1);
    int	    len2 = get_bv_length(l2);
    // Make the first list shorter or equal to the second list
    if( len1 > len2 ) {
	int len = len2;
	len2 = len1;
	len1 = len;
	g_ptr l = l2;
	l2 = l1;
	l1 = l;
    }
    formula result = B_One();
    formula b1 = GET_BOOL(GET_CONS_HD(l1));
    while( len2 > len1 ) {
	formula b2 = GET_BOOL(GET_CONS_HD(l2));
	if( identical ) {
	    if( !B_Equal(b1, b2) ) {
		return( B_Zero() );
	    }
	} else {
	    result = B_And(result, B_Xnor(b1, b2));
	    PUSH_BDD_GC(result);
	    if( Do_gc_asap ) Garbage_collect();
	    POP_BDD_GC(1);
	}
	len2--;
	l2 = GET_CONS_TL(l2);
    }
    while( len2 > 0 ) {
	formula b1 = GET_BOOL(GET_CONS_HD(l1));
	formula b2 = GET_BOOL(GET_CONS_HD(l2));
	if( identical ) {
	    if( !B_Equal(b1, b2) ) { return( B_Zero() ); }
	} else {
	    result = B_And(result, B_Xnor(b1, b2));
	    PUSH_BDD_GC(result);
	    if( Do_gc_asap ) Garbage_collect();
	    POP_BDD_GC(1);
	}
	len2--;
	l1 = GET_CONS_TL(l1);
	l2 = GET_CONS_TL(l2);
    }
    return result;
}

static int
bv_sha256_fn(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, pointer a)
{
    bv_ptr bp = (bv_ptr) a;
    int res = *g_cntp;
    *g_cntp = res+1;
    SHA256_printf(sha, "%d=BV\n", res);
    g_ptr  l = bp->u.l;
    SHA256_traverse_graph(g_cntp, g_tblp, sha, l);
    return res;
}

static pointer
bv_gmap_fn(gmap_info_ptr ip, pointer a)
{
    bv_ptr bp = (bv_ptr) a;
    g_ptr  l = bp->u.l;
    if( ip->read_only ) {
	Gen_map(ip->u.leaf_fun, l, ip->read_only);
	return NULL;
    } else {
	g_ptr res_l = Gen_map(ip->u.leaf_fun, l, ip->read_only);
	if( res_l == l ) {
	    return a;
	} else {
	    return( (pointer) get_bv_rec(res_l) );
	}
    }
}

static int
sx2(g_ptr *lp1, g_ptr *lp2)
{
    g_ptr l1 = *lp1;
    g_ptr l2 = *lp2;
    int	    len1 = get_bv_length(l1);
    int	    len2 = get_bv_length(l2);
    while( len1 > len2 ) {
	g_ptr msb = GET_CONS_HD(l2);
	INC_REFCNT(msb);
	l2 = Make_CONS_ND(msb, l2);
	len2++;
    }
    while( len2 > len1 ) {
	g_ptr msb = GET_CONS_HD(l1);
	INC_REFCNT(msb);
	l1 = Make_CONS_ND(msb, l1);
	len1++;
    }
    *lp1 = l1;
    *lp2 = l2;
    return( len2 );
}

static int
zx2(g_ptr *lp1, g_ptr *lp2)
{
    g_ptr l1 = *lp1;
    g_ptr l2 = *lp2;
    int	    len1 = get_bv_length(l1);
    int	    len2 = get_bv_length(l2);
    while( len1 > len2 ) {
	l2 = Make_CONS_ND(Zero, l2);
	len2++;
    }
    while( len2 > len1 ) {
	l1 = Make_CONS_ND(Zero, l1);
	len1++;
    }
    *lp1 = l1;
    *lp2 = l2;
    return( len2 );
}

static pointer
bv_gmap2_fn(gmap_info_ptr ip, pointer a1, pointer a2)
{
    bv_ptr  bp1 = (bv_ptr) a1;
    bv_ptr  bp2 = (bv_ptr) a2;
    g_ptr   l1 = bp1->u.l;
    g_ptr   l2 = bp2->u.l;
    sx2(&l1, &l2);
    if( ip->read_only ) {
	Gen_map2(ip->parent_op, ip->u.leaf_fun2, l1, l2, ip->read_only);
	return NULL;
    } else {
	g_ptr res_l = Gen_map2(ip->parent_op, ip->u.leaf_fun2,
			       l1, l2, ip->read_only);
	if( res_l == l1 ) { return a1; }
	if( res_l == l2 ) { return a2; }
	return( (pointer) get_bv_rec(res_l) );
    }
}

static g_ptr
add_rec(bool neg, g_ptr l1, g_ptr l2, formula *coutp)
{
    if( IS_NIL(l1) ) {
	ASSERT(IS_NIL(l2));
	*coutp = neg? B_One() : B_Zero();
	return( Make_NIL() );
    }
    formula cin;
    g_ptr rem = add_rec(neg, GET_CONS_TL(l1), GET_CONS_TL(l2), &cin);
    PUSH_BDD_GC(cin);
    PUSH_GLOBAL_GC(rem);
    formula a = GET_BOOL(GET_CONS_HD(l1));
    formula b = GET_BOOL(GET_CONS_HD(l2));
    b = neg? B_Not(b) : b;
    formula sum = B_Xor(b, cin);
    PUSH_BDD_GC(sum);
    if( Do_gc_asap ) Garbage_collect();
    POP_BDD_GC(1);
    sum = B_Xor(a, sum);
    PUSH_BDD_GC(sum);
    if( Do_gc_asap ) Garbage_collect();
    POP_BDD_GC(1);
    rem = Make_CONS_ND(Make_BOOL_leaf(sum), rem);
    PUSH_GLOBAL_GC(rem);
    formula ab = B_And(a,b);
    PUSH_BDD_GC(ab);
    if( Do_gc_asap ) Garbage_collect();
    formula ac = B_And(a,cin);
    PUSH_BDD_GC(ac);
    if( Do_gc_asap ) Garbage_collect();
    formula bc = B_And(b,cin);
    PUSH_BDD_GC(bc);
    if( Do_gc_asap ) Garbage_collect();
    formula s1 = B_Or(ac,bc);
    PUSH_BDD_GC(s1);
    if( Do_gc_asap ) Garbage_collect();
    *coutp = B_Or( ab, s1);
    PUSH_BDD_GC(*coutp);
    if( Do_gc_asap ) Garbage_collect();
    POP_BDD_GC(6);
    POP_GLOBAL_GC(2);
    return( rem );
}

static g_ptr
gen_add(bool neg, g_ptr l1, g_ptr l2)
{
    formula cout;
    l1 = Make_CONS_ND(GET_CONS_HD(l1), l1);
    l2 = Make_CONS_ND(GET_CONS_HD(l2), l2);
    PUSH_GLOBAL_GC(l1);
    PUSH_GLOBAL_GC(l2);
    g_ptr res = add_rec(neg, l1, l2, &cout);
    POP_GLOBAL_GC(2);
    return( trim_bv(res) );
}

static formula
is_zero_list(g_ptr l)
{
    formula zero = B_One();
    while( !IS_NIL(l) ) {
	zero = B_And(zero, B_Not(GET_BOOL(GET_CONS_HD(l))));
	l = GET_CONS_TL(l);
    }
    return zero;
}

static g_ptr
negate_bv_list(g_ptr l)
{
    g_ptr zero = Make_CONS_ND(Zero, Make_NIL());
    sx2(&l,&zero);
    PUSH_GLOBAL_GC(zero);
    PUSH_GLOBAL_GC(l);
    g_ptr res = trim_bv(gen_add(TRUE, zero, l));
    PUSH_GLOBAL_GC(res);
    if( Do_gc_asap ) Garbage_collect();
    POP_GLOBAL_GC(3);
    return( res );
}

static g_ptr
decrement_bv_list(g_ptr l)
{
    g_ptr one = Make_CONS_ND(Zero, Make_CONS_ND(One, Make_NIL()));
    sx2(&l,&one);
    PUSH_GLOBAL_GC(one);
    PUSH_GLOBAL_GC(l);
    g_ptr res = trim_bv(gen_add(TRUE, l, one));
    PUSH_GLOBAL_GC(res);
    if( Do_gc_asap ) Garbage_collect();
    POP_GLOBAL_GC(3);
    return( res );
}

static g_ptr
ite_bv_list(formula cond, g_ptr l1, g_ptr l2)
{
    sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    PUSH_GLOBAL_GC(l1);
    PUSH_GLOBAL_GC(l2);
    PUSH_GLOBAL_GC(res);
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1=l1, n2=l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	formula nb = GET_BOOL(GET_CONS_HD(n1));
	formula pb = GET_BOOL(GET_CONS_HD(n2));
	formula b = B_Or(B_And(cond, nb), B_And(B_Not(cond), pb));
	APPEND1(tail, Make_BOOL_leaf(b));
	if( Do_gc_asap ) Garbage_collect();
    }
    POP_GLOBAL_GC(3);
    return( trim_bv(res) );
}

static g_ptr
abs_bv_list(g_ptr l)
{
    PUSH_GLOBAL_GC(l);
    formula neg = GET_BOOL(GET_CONS_HD(l));
    g_ptr l_neg = negate_bv_list(l);
    PUSH_GLOBAL_GC(l_neg);
    g_ptr res = trim_bv(ite_bv_list(neg, l_neg, l));
    PUSH_GLOBAL_GC(res);
    if( Do_gc_asap ) Garbage_collect();
    POP_GLOBAL_GC(3);
    return( res );
}

static g_ptr
mult_bv_list(g_ptr xv, g_ptr yv)
{
    if( IS_NIL(yv) ) {
	return( Make_CONS_ND(Zero, Make_NIL()) );
    }
    g_ptr rem = mult_bv_list(xv, GET_CONS_TL(yv));
    PUSH_GLOBAL_GC(rem);
    if( Do_gc_asap ) Garbage_collect();
    g_ptr prod = Make_NIL();
    PUSH_GLOBAL_GC(prod);
    g_ptr tail = prod;
    APPEND1(tail, Zero);
    formula y = GET_BOOL(GET_CONS_HD(yv));
    for(g_ptr np = xv; !IS_NIL(np); np = GET_CONS_TL(np)) {
	formula x = GET_BOOL(GET_CONS_HD(np));
	formula b = B_And(y,x);
	APPEND1(tail, Make_BOOL_leaf(b));
    }
    for(int i = 0; i < get_bv_length(yv)-1; i++) {
	APPEND1(tail, Zero);
    }
    zx2(&prod, &rem);
    POP_GLOBAL_GC(2);
    PUSH_GLOBAL_GC(rem);
    PUSH_GLOBAL_GC(prod);
    if( Do_gc_asap ) Garbage_collect();
    g_ptr res = trim_bv(gen_add(FALSE, prod, rem));
    POP_GLOBAL_GC(2);
    return( res );
}

static formula
less_rec(g_ptr l1, g_ptr l2)
{
    if( IS_NIL(l1) ) {
	ASSERT(IS_NIL(l2));
	return( B_Zero() );
    }
    formula a = GET_BOOL(GET_CONS_HD(l1));
    formula b = GET_BOOL(GET_CONS_HD(l2));
    formula rem = less_rec(GET_CONS_TL(l1),GET_CONS_TL(l2));
    PUSH_BDD_GC(rem);
    if( Do_gc_asap ) Garbage_collect();
    formula ab = B_Xnor(a,b);
    PUSH_BDD_GC(ab); if( Do_gc_asap ) Garbage_collect();
    formula abrem = B_And(ab,rem);
    PUSH_BDD_GC(abrem); if( Do_gc_asap ) Garbage_collect();
    formula nab = B_And(B_Not(a), b);
    PUSH_BDD_GC(nab); if( Do_gc_asap ) Garbage_collect();
    formula res = B_Or(nab, abrem);
    PUSH_BDD_GC(res); if( Do_gc_asap ) Garbage_collect();
    POP_BDD_GC(5);
    return( res );
}

static formula
less_than_bv_list(g_ptr l1, g_ptr l2)
{
    sx2(&l1, &l2);
    formula neg1 = GET_BOOL(GET_CONS_HD(l1));
    formula neg2 = GET_BOOL(GET_CONS_HD(l2));
    formula raw_res = less_rec(l1, l2);
    PUSH_BDD_GC(raw_res);
    formula res = B_Ite(neg1, B_Ite(neg2, raw_res, B_One()),
	   	        B_Ite(neg2, B_Zero(), raw_res));
    PUSH_BDD_GC(res);
    if( Do_gc_asap ) Garbage_collect();
    POP_BDD_GC(2);
    return( res );
}

static bool
is_scalar_list(g_ptr list)
{
    bool scalar = TRUE;
    for(g_ptr np = list; scalar && !IS_NIL(np); np = GET_CONS_TL(np)) {
	formula f = GET_BOOL(GET_CONS_HD(np));
	if( is_symbolic(f) ) {
	    scalar = FALSE;
	}
    }
    return( scalar );
}

static int
constant_power_of_two(g_ptr bv)
{
    if( !is_scalar_list(bv) ) { return( -1 ); }
    bool found = FALSE;
    while( !found && !IS_NIL(bv) ) {
	if( GET_BOOL(GET_CONS_HD(bv)) == B_One() ) {
	    found = TRUE;
	} 
	bv = GET_CONS_TL(bv);
    }
    if( !found ) { return(-1); }
    int cnt = 0;
    while( !IS_NIL(bv) ) {
	if( GET_BOOL(GET_CONS_HD(bv)) == B_One() ) {
	    return( -1 );
	}
	cnt++;
	bv = GET_CONS_TL(bv);
    }
    return( cnt );
}

static void
fixed_ext_bvdiv(g_ptr av, g_ptr bv, g_ptr *Qp, g_ptr *Rp)
{
    int sz = get_bv_length(av);
    ASSERT( sz == get_bv_length(bv) );
    int cnt = constant_power_of_two(bv);
    if( cnt >= 0 ) {
	// Power of two div/mod
	g_ptr Q = Make_NIL();
	g_ptr Qtail = Q;
	g_ptr R = Make_NIL();
	g_ptr Rtail = R;
	for(int i = 0; i < cnt; i++) {
	    APPEND1(Qtail,Zero);
	}
	for(int i = 0; i < sz-cnt; i++) {
	    APPEND1(Qtail, GET_CONS_HD(av));
	    av = GET_CONS_TL(av);
	}
	for(int i = 0; i < (sz-cnt); i++) {
	    APPEND1(Rtail,Zero);
	}
	for(int i = 0; i < cnt; i++) {
	    APPEND1(Rtail, GET_CONS_HD(av));
	    av = GET_CONS_TL(av);
	}
	*Qp = Q;
	*Rp = R;
	return;
    }
    // General long-hand division
    buffer q;
    buffer r;
    buffer bb;
    buffer sub_buf;
    new_buf(&q, sz, sizeof(formula));
    new_buf(&r, sz, sizeof(formula));
    new_buf(&bb, sz, sizeof(formula));
    new_buf(&sub_buf, sz, sizeof(formula));
    formula zero = B_Zero();
    for(int i = 0; i < sz; i++) {
	formula b = GET_BOOL(GET_CONS_HD(bv));
	bv = GET_CONS_TL(bv);
	push_buf(&bb, (pointer) &b);
	push_buf(&q, (pointer) &zero);
	push_buf(&r, (pointer) &zero);
	push_buf(&sub_buf, (pointer) &zero);
    }
    while( !IS_NIL(av) ) {
	formula a = GET_BOOL(GET_CONS_HD(av));
	for(int i = 1; i < sz; i++) {
	    formula ri = *((formula *) M_LOCATE_BUF(&r, i));
	    store_buf(&r, i-1, (pointer) &ri);
	}
	store_buf(&r, sz-1, (pointer) &a);
	for(int i = 0; i < sz; i++) {
	    formula ri = *((formula *) M_LOCATE_BUF(&r, i));
	    store_buf(&sub_buf, i, (pointer) &ri);
	}
	formula cin = B_One();
	for(int i = sz-1; i >= 0; i--) {
	    formula x = *((formula *) M_LOCATE_BUF(&sub_buf, i));
	    formula y = B_Not(*((formula *) M_LOCATE_BUF(&bb, i)));
	    formula sum = B_Xor(cin, B_Xor(x,y));
	    store_buf(&sub_buf, i, (pointer) &sum);
	    cin = B_Or(B_And(x,y), B_Or(B_And(x,cin), B_And(y,cin)));
	}
	formula geq = B_One();
	for(int i = sz-1; i >= 0; i--) {
	    formula ri = *((formula *) M_LOCATE_BUF(&r, i));
	    formula bi = *((formula *) M_LOCATE_BUF(&bb, i));
	    geq = B_Or(B_And(ri, B_Not(bi)),
		       B_And(B_Xnor(ri,bi), geq));
	}
	for(int i = 0; i < sz-1; i++) {
	    formula ri = *((formula *) M_LOCATE_BUF(&r, i));
	    formula subi = *((formula *) M_LOCATE_BUF(&sub_buf, i));
	    formula qi = *((formula *) M_LOCATE_BUF(&q, i+1));
	    formula Qi = qi;
	    formula Ri = B_Ite(geq, subi, ri);
	    store_buf(&r, i, (pointer) &Ri);
	    store_buf(&q, i, (pointer) &Qi);
	}
	formula ri = *((formula *) M_LOCATE_BUF(&r, sz-1));
	formula subi = *((formula *) M_LOCATE_BUF(&sub_buf, sz-1));
	formula Q0 = geq;
	formula R0 = B_Ite(geq, subi, ri);
	store_buf(&r, sz-1, (pointer) &R0);
	store_buf(&q, sz-1, (pointer) &Q0);
	av = GET_CONS_TL(av);
    }
    g_ptr Q = Make_NIL();
    for(int i = sz-1; i >= 0; i--) {
	formula qi = *((formula *) M_LOCATE_BUF(&q, i));
	Q = Make_CONS_ND(Make_BOOL_leaf(qi), Q);
    }
    g_ptr R = Make_NIL();
    for(int i = sz-1; i >= 0; i--) {
	formula ri = *((formula *) M_LOCATE_BUF(&r, i));
	R = Make_CONS_ND(Make_BOOL_leaf(ri), R);
    }
    *Qp = Q;
    *Rp = R;
    free_buf(&q);
    free_buf(&r);
    free_buf(&bb);
    free_buf(&sub_buf);
    return;
}


/********************************************************/
/*          EXPORTED EXTAPI FUNCTIONS                   */
/********************************************************/

static void
bv_construct(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    INC_REFCNT(list);
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(list));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_destruct(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bv_ptr bp = (bv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    OVERWRITE(redex, list);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv2str(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    int depth = GET_INT(arg1);
    bv_ptr bp = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr list = bp->u.l;
    string res = bv_list2str(list, depth);
    MAKE_REDEX_STRING(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_size(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bv_ptr bp = (bv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    MAKE_REDEX_INT(redex, get_bv_length(list));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_var(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(arg1);
    g_ptr nl = Vec2nodes(name);
    for(g_ptr np = nl; !IS_NIL(np); np = GET_CONS_TL(np)) {
	g_ptr l = GET_CONS_HD(np);
	formula v = B_Var(GET_STRING(l));
	MAKE_REDEX_BOOL(l, v);
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(nl));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static arbi_T
bv_list2int(g_ptr l, arbi_T *ppow)
{
    if( IS_NIL(l) ) {
	*ppow = Arbi_FromInt(1);
	return( Arbi_FromInt(0) );
    }
    arbi_T my_pow;
    arbi_T rem = bv_list2int(GET_CONS_TL(l), &my_pow);
    formula is_zero = ((GET_BOOL(GET_CONS_HD(l))) == B_Zero());
    arbi_T cur = is_zero? Arbi_FromInt(0) : my_pow;
    *ppow = Arbi_mlt(my_pow, Arbi_FromInt(2));
    return( Arbi_add(cur, rem) );
}

static void
bv2int(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bv_ptr bp = (bv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    if( !is_scalar_list(list) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("bv2int of symbolic bitvector"));
	return;
    }
    bool neg = (GET_BOOL(GET_CONS_HD(list)) == B_One());
    g_ptr abs = abs_bv_list(list);
    arbi_T pow;
    arbi_T abs_res = bv_list2int(abs, &pow);
    arbi_T res = neg? Arbi_neg(abs_res) : abs_res;
    MAKE_REDEX_AINT(redex, res)
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
int2bv(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    arbi_T ai = GET_AINT(arg1);
    string s;
    bool is_neg;
    if( Arbi_IsNegative(ai) ) {
	is_neg = TRUE;
	s = Arbi_ToString(Arbi_neg(ai), 2);
	string p = rindex(s, '1');
	p--;
	while( p >= s ) {
	    if( *p == '1' ) {
		*p = '0';
	    } else {
		ASSERT( *p == '0' );
		*p = '1';
	    }
	    p--;
	}
    } else {
	is_neg = FALSE;
	s = Arbi_ToString(ai, 2);
    }
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    APPEND1(tail, (is_neg? One : Zero));
    while( *s ) {
	if( *s == '1' ) {
	    APPEND1(tail, One);
	} else {
	    APPEND1(tail, Zero);
	}
	s++;
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_add(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = gen_add(FALSE, l1, l2);
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_sub(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = gen_add(TRUE, l1, l2);
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_mul(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr av  = a1->u.l;
    g_ptr bv  = a2->u.l;
    int len_av = get_bv_length(av);
    int len_bv = get_bv_length(bv);
    // Make sure bv is the shorter one
    if( len_av < len_bv ) {
	int len_tmp = len_av;
	len_av = len_bv;
	len_bv = len_tmp;
	g_ptr tmp = av;
	av = bv;
	bv = tmp;
    }
    formula neg_a = GET_BOOL(GET_CONS_HD(av));
    g_ptr abs_av = abs_bv_list(av);
    PUSH_GLOBAL_GC(abs_av);
    formula neg_b = GET_BOOL(GET_CONS_HD(bv));
    g_ptr abs_bv = abs_bv_list(bv);
    PUSH_GLOBAL_GC(abs_bv);
    g_ptr abs_res = Make_CONS_ND(Zero,
				 mult_bv_list(abs_av, abs_bv));
    formula neg = B_Xor(neg_a, neg_b);
    PUSH_BDD_GC(neg);
    PUSH_GLOBAL_GC(abs_res);
    g_ptr neg_res = negate_bv_list(abs_res);
    PUSH_GLOBAL_GC(neg_res);
    sx2(&abs_res, &neg_res);
    g_ptr res = ite_bv_list(neg, neg_res, abs_res);
    POP_GLOBAL_GC(4);
    POP_BDD_GC(1);
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
signed_bvdiv(g_ptr dv, g_ptr nv, g_ptr *Q, g_ptr *R)
{
    formula neg_d = GET_BOOL(GET_CONS_HD(dv));
    formula neg_n = GET_BOOL(GET_CONS_HD(nv));
    g_ptr   abs_dv = abs_bv_list(dv);
    PUSH_GLOBAL_GC(abs_dv);
    g_ptr   abs_nv = abs_bv_list(nv);
    PUSH_GLOBAL_GC(abs_nv);
    zx2(&abs_dv, &abs_nv);
    POP_GLOBAL_GC(2);
    PUSH_GLOBAL_GC(abs_dv);
    PUSH_GLOBAL_GC(abs_nv);
    g_ptr Qraw;
    g_ptr Rraw;
    fixed_ext_bvdiv(abs_dv, abs_nv, &Qraw, &Rraw);
    PUSH_GLOBAL_GC(Qraw);
    PUSH_GLOBAL_GC(Rraw);
    formula exact = is_zero_list(Rraw);
    PUSH_BDD_GC(exact);
    formula same_sign = B_Xnor(neg_d, neg_n);
    PUSH_BDD_GC(same_sign);
    g_ptr Q0 = ite_bv_list(same_sign, Qraw, (negate_bv_list(Qraw)));
    PUSH_GLOBAL_GC(Q0);
    *Q = ite_bv_list(B_Or(exact,same_sign), Q0, decrement_bv_list(Q0));
    PUSH_GLOBAL_GC(*Q);
    g_ptr R0 = ite_bv_list(same_sign, Rraw, gen_add(TRUE, Rraw, abs_nv));
    PUSH_GLOBAL_GC(R0);
    *R = ite_bv_list(exact, Rraw, ite_bv_list(neg_d, negate_bv_list(R0), R0));
    POP_BDD_GC(2);
    POP_GLOBAL_GC(7);
} 

static void
bv_ashr(g_ptr redex)
{
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    int cnt = GET_INT(arg2);
    if( cnt < 0 ) {
        MAKE_REDEX_FAILURE(redex,
                           Fail_pr("bv_ashr by negative amount (%d)", cnt));
        return;
    }
    if( cnt == 0 ) {
        OVERWRITE(redex, arg1);
        return;
    }
    bv_ptr a = (bv_ptr) GET_EXT_OBJ(arg1);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr al = a->u.l;
    int len = get_bv_length(al);
    if( cnt >= len ) {
	APPEND1(tail, GET_CONS_HD(al));
    } else {
	for(int i = 0; i < len-cnt; i++) {
	    APPEND1(tail, GET_CONS_HD(al));
	    al = GET_CONS_TL(al);
	}
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
}


static void
bv_div(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr Q;
    g_ptr R;
    signed_bvdiv(a1->u.l, a2->u.l, &Q, &R);
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(Q));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_mod(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr Q;
    g_ptr R;
    signed_bvdiv(a1->u.l, a2->u.l, &Q, &R);
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(R));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_NOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bv_ptr bp = (bv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    for(g_ptr np = list; !IS_NIL(np); np=GET_CONS_TL(np)) {
	formula a = GET_BOOL(GET_CONS_HD(np));
	APPEND1(tail, Make_BOOL_leaf(B_Not(a)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_ZX(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bv_ptr bp = (bv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    g_ptr res = trim_bv(Make_CONS_ND(Zero, list));
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_AND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	formula a = GET_BOOL(GET_CONS_HD(n1));
	formula b = GET_BOOL(GET_CONS_HD(n2));
	APPEND1(tail, Make_BOOL_leaf(B_And(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_OR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	formula a = GET_BOOL(GET_CONS_HD(n1));
	formula b = GET_BOOL(GET_CONS_HD(n2));
	APPEND1(tail, Make_BOOL_leaf(B_Or(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_XOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	formula a = GET_BOOL(GET_CONS_HD(n1));
	formula b = GET_BOOL(GET_CONS_HD(n2));
	APPEND1(tail, Make_BOOL_leaf(B_Xor(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_XNOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	formula a = GET_BOOL(GET_CONS_HD(n1));
	formula b = GET_BOOL(GET_CONS_HD(n2));
	APPEND1(tail, Make_BOOL_leaf(B_Xnor(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bv_oidx, get_bv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_less(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    formula res = less_than_bv_list(l1, l2);
    MAKE_REDEX_BOOL(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_leq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    formula res = B_Not(less_than_bv_list(l2, l1));
    MAKE_REDEX_BOOL(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_greater(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    formula res = less_than_bv_list(l2, l1);
    MAKE_REDEX_BOOL(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bv_geq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bv_ptr a1 = (bv_ptr) GET_EXT_OBJ(arg1);
    bv_ptr a2 = (bv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    formula res = B_Not(less_than_bv_list(l1, l2));
    MAKE_REDEX_BOOL(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Bv_Init()
{
    new_mgr(&bv_rec_mgr, sizeof(bv_rec));
    Zero = Make_BOOL_leaf(B_Zero());
    PUSH_GLOBAL_GC(Zero);
    One = Make_BOOL_leaf(B_One());
    PUSH_GLOBAL_GC(One);
    bv_free_list = NULL;
    bv_oidx  = Add_ExtAPI_Object("bv",
				 mark_bv_fn,
				 sweep_bv_fn,
				 save_bv_fn,
				 load_bv_fn,
				 bv2str_fn,
				 bv_eq_fn,
				 bv_gmap_fn,
				 bv_gmap2_fn,
				 bv_sha256_fn);
    bv_handle_tp  = Get_Type("bv", NULL, TP_INSERT_FULL_TYPE);
}

void
Bv_Install_Functions()
{
    // Add builtin functions

    Add_ExtAPI_Function("list2bv", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_bool()), bv_handle_tp),
			bv_construct);

    Add_ExtAPI_Function("bv2list", "1", FALSE,
			GLmake_arrow(bv_handle_tp, GLmake_list(GLmake_bool())),
			bv_destruct);

    Add_ExtAPI_Function("bv_size", "1", FALSE,
			GLmake_arrow(bv_handle_tp, GLmake_int()),
			bv_size);

    Add_ExtAPI_Function("int2bv", "1", FALSE,
			GLmake_arrow(GLmake_int(), bv_handle_tp),
			int2bv);

    Add_ExtAPI_Function("bv2int", "1", FALSE,
			GLmake_arrow(bv_handle_tp, GLmake_int()),
			bv2int);

    Add_ExtAPI_Function("bv_variable", "1", FALSE,
			GLmake_arrow(GLmake_string(), bv_handle_tp),
			bv_var);

    Add_ExtAPI_Function("bv2str", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(bv_handle_tp,
						  GLmake_string())),
			bv2str);

    Add_ExtAPI_Function("bv_add", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_add);

    Add_ExtAPI_Function("bv_sub", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_sub);

    Add_ExtAPI_Function("bv_mul", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_mul);

    Add_ExtAPI_Function("bv_ashr", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(GLmake_int(), bv_handle_tp)),
			bv_ashr);

    Add_ExtAPI_Function("bv_div", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_div);

    Add_ExtAPI_Function("bv_mod", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_mod);

    Add_ExtAPI_Function("bv_NOT", "1", FALSE,
			 GLmake_arrow(bv_handle_tp, bv_handle_tp),
			bv_NOT);

    Add_ExtAPI_Function("bv_ZX", "1", FALSE,
			 GLmake_arrow(bv_handle_tp, bv_handle_tp),
			bv_ZX);

    Add_ExtAPI_Function("bv_AND", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_AND);

    Add_ExtAPI_Function("bv_OR", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_OR);

    Add_ExtAPI_Function("bv_XOR", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_XOR);

    Add_ExtAPI_Function("bv_XNOR", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp, bv_handle_tp)),
			bv_XNOR);

    Add_ExtAPI_Function("bv_less", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp,
						  GLmake_bool())),
			bv_less);

    Add_ExtAPI_Function("bv_leq", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp,
						  GLmake_bool())),
			bv_leq);

    Add_ExtAPI_Function("bv_greater", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp,
						  GLmake_bool())),
			bv_greater);

    Add_ExtAPI_Function("bv_geq", "11", FALSE,
			GLmake_arrow(bv_handle_tp,
				     GLmake_arrow(bv_handle_tp,
						  GLmake_bool())),
			bv_geq);

}

