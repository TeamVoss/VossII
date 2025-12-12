//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2019			*/
/*									*/
/************************************************************************/
#include "ubv.h"
#include "graph.h"

#include <math.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     *stringsp;
extern FILE        *odests_fp;

/***** PRIVATE VARIABLES *****/
static int	    ubv_oidx;
static typeExp_ptr  ubv_handle_tp;
static rec_mgr	    ubv_rec_mgr;
static ubv_ptr	    ubv_free_list;
static char	    ubv_str_buf[4096];
static g_ptr	    Zero;
static g_ptr	    One;


static int	    sx2(g_ptr *lp1, g_ptr *lp2);

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static g_ptr
trim_ubv(g_ptr l)
{
    while (1) {
	if( IS_NIL(l) ) return l;
	g_ptr next = GET_CONS_TL(l);
	if( IS_NIL(next) ) return l;
	if( GET_UBV(GET_CONS_HD(l)) == GET_UBV(GET_CONS_HD(next)) ) {
	    l = next;
	} else {
	    return l;
	}
    }
}

static ubv_ptr
get_ubv_rec(g_ptr l)
{
    ubv_ptr res;
    if( ubv_free_list != NULL ) {
	res = ubv_free_list;
	ubv_free_list = ubv_free_list->u.next;
    } else {
	res = (ubv_ptr) new_rec(&ubv_rec_mgr);
    }
    res->flag = 0;
    res->u.l = trim_ubv(l);
    return res;
}

static void
mark_ubv_fn(pointer p)
{
    ubv_ptr fp = (ubv_ptr) p;
    fp->flag = 1;
    Mark(fp->u.l);
    return;
}

static void
sweep_ubv_fn(void)
{
    ubv_ptr fp;
    ubv_free_list = NULL;
    FOR_REC(&ubv_rec_mgr, ubv_ptr, fp) {
	if( fp->flag == 1 ) {
	    fp->flag = 0;
	} else {
	    fp->u.next = ubv_free_list;
	    ubv_free_list = fp;
	}
    }
}

static int
get_ubv_length(g_ptr l)
{
    int len = 0;
    while( !IS_NIL(l) ) {
	len++;
	l = GET_CONS_TL(l);
    }
    return len;
}

static void
save_ubv_fn(FILE *fp, pointer p)
{
    ubv_ptr ubvp = (ubv_ptr) p;
    g_ptr l = ubvp->u.l;
    fprintf(fp, "%d", get_ubv_length(l));
    for(g_ptr np = l; !IS_NIL(np); np = GET_CONS_TL(np)) {
	ubv f = GET_UBV(GET_CONS_HD(np));
	fprintf(fp, "  %d", Save_get_ubv_idx(f));
    }
    fputc('\n', fp);	// Add newline for human readability
}

static pointer
load_ubv_fn(FILE *fp)
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
	ubv f = Load_get_ubv_from_idx(idx);
	APPEND1(tail, Make_UBV_leaf(f));
    }
    fgetc(fp);	// Eat new line
    return( (pointer) get_ubv_rec(res) );
}

static bool
is_symbolic(ubv f)
{
    return( f != UBV_One() && f != UBV_Zero() );
}

static string
ubv_list2str(g_ptr l, int depth)
{
    if((odests_fp = fmemopen(ubv_str_buf, 4096, "w")) == NULL ) {
	DIE("Should never happen");
    }
    char sep = '<';
    for(g_ptr np = l; !IS_NIL(np); np = GET_CONS_TL(np)) {
	ubv f = GET_UBV(GET_CONS_HD(np));
	FP(FILE_fp,"%c", sep);
	sep = ',';
	if( depth == 0 && is_symbolic(f) ) {
	    FP(FILE_fp, "S");
	} else {
	    UBV_Print(FILE_fp, f);
	}
    }
    FP(FILE_fp, ">");
    fclose(odests_fp);
    odests_fp = NULL;
    return( wastrsave(stringsp, ubv_str_buf) );
}

static string
ubv2str_fn(pointer p)
{
    ubv_ptr bp = (ubv_ptr) p;
    g_ptr l = bp->u.l;
    return( ubv_list2str(l, -1) );
}

static formula
ubv_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    ubv_ptr bp1 = (ubv_ptr) p1;
    ubv_ptr bp2 = (ubv_ptr) p2;
    g_ptr   l1 = bp1->u.l;
    g_ptr   l2 = bp2->u.l;
    int len = sx2(&l1, &l2);
    while( len > 0 ) {
	ubv b1 = GET_UBV(GET_CONS_HD(l1));
	ubv b2 = GET_UBV(GET_CONS_HD(l2));
	if( !UBV_Equal(b1, b2) ) { return( B_Zero() ); }
	len--;
	l1 = GET_CONS_TL(l1);
	l2 = GET_CONS_TL(l2);
    }
    return( B_One() );
}

static unint
ubv_hash_fn(pointer p, unint n)
{
    ubv_ptr bp = (ubv_ptr) p;
    g_ptr   l = bp->u.l;
    unint res = 7;
    while( !IS_NIL(l) ) {
	res = (3*res+ G_rec_hash(GET_CONS_HD(l), n)) % n;
	l = GET_CONS_TL(l);
    }
    return( res );
}

static int
ubv_sha256_fn(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, pointer a)
{
    ubv_ptr bp = (ubv_ptr) a;
    int res = *g_cntp;
    *g_cntp = res+1;
    SHA256_printf(sha, "%d=UBV\n", res);
    g_ptr  l = bp->u.l;
    SHA256_traverse_graph(g_cntp, g_tblp, sha, l);
    return res;
}

static pointer
ubv_gmap_fn(gmap_info_ptr ip, pointer a)
{
    ubv_ptr bp = (ubv_ptr) a;
    g_ptr  l = bp->u.l;
    if( ip->read_only ) {
	Gen_map(ip->u.leaf_fun, l, ip->read_only);
	return NULL;
    } else {
	g_ptr res_l = Gen_map(ip->u.leaf_fun, l, ip->read_only);
	if( res_l == l ) {
	    return a;
	} else {
	    return( (pointer) get_ubv_rec(res_l) );
	}
    }
}

static int
sx2(g_ptr *lp1, g_ptr *lp2)
{
    g_ptr l1 = *lp1;
    g_ptr l2 = *lp2;
    int	    len1 = get_ubv_length(l1);
    int	    len2 = get_ubv_length(l2);
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
    int	    len1 = get_ubv_length(l1);
    int	    len2 = get_ubv_length(l2);
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
ubv_gmap2_fn(gmap_info_ptr ip, pointer a1, pointer a2)
{
    ubv_ptr  bp1 = (ubv_ptr) a1;
    ubv_ptr  bp2 = (ubv_ptr) a2;
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
	return( (pointer) get_ubv_rec(res_l) );
    }
}

static g_ptr
add_rec(bool neg, g_ptr l1, g_ptr l2, ubv *coutp)
{
    if( IS_NIL(l1) ) {
	ASSERT(IS_NIL(l2));
	*coutp = neg? UBV_One() : UBV_Zero();
	return( Make_NIL() );
    }
    ubv cin;
    g_ptr rem = add_rec(neg, GET_CONS_TL(l1), GET_CONS_TL(l2), &cin);
    ubv a = GET_UBV(GET_CONS_HD(l1));
    ubv b = GET_UBV(GET_CONS_HD(l2));
    b = neg? UBV_Not(b) : b;
    ubv sum = UBV_Xor(a, UBV_Xor(b, cin));
    rem = Make_CONS_ND(Make_UBV_leaf(sum), rem);
    *coutp = UBV_Or( UBV_And(a, b), UBV_Or( UBV_And(a, cin), UBV_And(b, cin)));
    return( rem );
}

static g_ptr
gen_add(bool neg, g_ptr l1, g_ptr l2)
{
    ubv cout;
    l1 = Make_CONS_ND(GET_CONS_HD(l1), l1);
    l2 = Make_CONS_ND(GET_CONS_HD(l2), l2);
    g_ptr res = add_rec(neg, l1, l2, &cout);
    return( trim_ubv(res) );
}

static ubv
is_zero_list(g_ptr l)
{
    ubv zero = UBV_One();
    while( !IS_NIL(l) ) {
	zero = UBV_And(zero, UBV_Not(GET_UBV(GET_CONS_HD(l))));
	l = GET_CONS_TL(l);
    }
    return zero;
}

static g_ptr
negate_ubv_list(g_ptr l)
{
    g_ptr zero = Make_CONS_ND(Zero, Make_NIL());
    sx2(&l,&zero);
    return( trim_ubv(gen_add(TRUE, zero, l)) );
}

static g_ptr
decrement_ubv_list(g_ptr l)
{   
    g_ptr one = Make_CONS_ND(Zero, Make_CONS_ND(One, Make_NIL()));
    sx2(&l,&one);
    return( trim_ubv(gen_add(TRUE, l, one)) );
}   

static g_ptr
ite_ubv_list(ubv cond, g_ptr l1, g_ptr l2)
{
    sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1=l1, n2=l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	ubv nb = GET_UBV(GET_CONS_HD(n1));
	ubv pb = GET_UBV(GET_CONS_HD(n2));
	ubv b = UBV_Or(UBV_And(cond, nb), UBV_And(UBV_Not(cond), pb));
	APPEND1(tail, Make_UBV_leaf(b));
    }
    return( trim_ubv(res) );
}

static g_ptr
abs_ubv_list(g_ptr l)
{
    ubv neg = GET_UBV(GET_CONS_HD(l));
    g_ptr l_neg = negate_ubv_list(l);
    return( trim_ubv(ite_ubv_list(neg, l_neg, l)) );
}

static g_ptr
mult_ubv_list(g_ptr xv, g_ptr yv)
{
    if( IS_NIL(yv) ) {
	return( Make_CONS_ND(Zero, Make_NIL()) );
    }
    g_ptr rem = mult_ubv_list(xv, GET_CONS_TL(yv));
    g_ptr prod = Make_NIL();
    g_ptr tail = prod;
    APPEND1(tail, Zero);
    ubv y = GET_UBV(GET_CONS_HD(yv));
    for(g_ptr np = xv; !IS_NIL(np); np = GET_CONS_TL(np)) {
	ubv x = GET_UBV(GET_CONS_HD(np));
	ubv b = UBV_And(y,x);
	APPEND1(tail, Make_UBV_leaf(b));
    }
    for(int i = 0; i < get_ubv_length(yv)-1; i++) {
	APPEND1(tail, Zero);
    }
    zx2(&prod, &rem);
    return( trim_ubv(gen_add(FALSE, prod, rem)) );
}

static ubv
less_rec(g_ptr l1, g_ptr l2)
{
    if( IS_NIL(l1) ) {
	ASSERT(IS_NIL(l2));
	return( UBV_Zero() );
    }
    ubv a = GET_UBV(GET_CONS_HD(l1));
    ubv b = GET_UBV(GET_CONS_HD(l2));
    return( UBV_Or(UBV_And(UBV_Not(a), b),
		 UBV_And(UBV_Xnor(a,b),
		       less_rec(GET_CONS_TL(l1),GET_CONS_TL(l2)))));
}

static ubv
less_than_ubv_list(g_ptr l1, g_ptr l2)
{
    sx2(&l1, &l2);
    ubv neg1 = GET_UBV(GET_CONS_HD(l1));
    ubv neg2 = GET_UBV(GET_CONS_HD(l2));
    ubv raw_res = less_rec(l1, l2);
    return( UBV_Ite(neg1, UBV_Ite(neg2, raw_res, UBV_One()),
	   	        UBV_Ite(neg2, UBV_Zero(), raw_res)) );

}

static ubv
ubv_eq_list(g_ptr l1, g_ptr l2)
{
    int	len1 = get_ubv_length(l1);
    int	len2 = get_ubv_length(l2);
    // Make the first list shorter or equal to the second list
    if( len1 > len2 ) {
	int len = len2;
	len2 = len1;
	len1 = len;
	g_ptr l = l2;
	l2 = l1;
	l1 = l;
    }
    ubv res = UBV_One();
    ubv b1 = GET_UBV(GET_CONS_HD(l1));
    while( len2 > len1 ) {
	ubv b2 = GET_UBV(GET_CONS_HD(l2));
	res = UBV_And(res, UBV_Xnor(b1, b2));
	len2--;
	l2 = GET_CONS_TL(l2);
    }
    while( len2 > 0 ) {
	ubv b1 = GET_UBV(GET_CONS_HD(l1));
	ubv b2 = GET_UBV(GET_CONS_HD(l2));
	res = UBV_And(res, UBV_Xnor(b1, b2));
	len2--;
	l1 = GET_CONS_TL(l1);
	l2 = GET_CONS_TL(l2);
    }
    return res;
}

static bool
is_scalar_list(g_ptr list)
{
    bool scalar = TRUE;
    for(g_ptr np = list; scalar && !IS_NIL(np); np = GET_CONS_TL(np)) {
	ubv f = GET_UBV(GET_CONS_HD(np));
	if( is_symbolic(f) ) {
	    scalar = FALSE;
	}
    }
    return( scalar );
}

static int
constant_power_of_two(g_ptr ubv)
{
    if( !is_scalar_list(ubv) ) { return( -1 ); }
    bool found = FALSE;
    while( !found && !IS_NIL(ubv) ) {
	if( GET_UBV(GET_CONS_HD(ubv)) == UBV_One() ) {
	    found = TRUE;
	} 
	ubv = GET_CONS_TL(ubv);
    }
    if( !found ) { return(-1); }
    int cnt = 0;
    while( !IS_NIL(ubv) ) {
	if( GET_UBV(GET_CONS_HD(ubv)) == UBV_One() ) {
	    return( -1 );
	}
	cnt++;
	ubv = GET_CONS_TL(ubv);
    }
    return( cnt );
}

static void
fixed_ext_ubvdiv(g_ptr av, g_ptr ubv, g_ptr *Qp, g_ptr *Rp)
{
    int sz = get_ubv_length(av);
    ASSERT( sz == get_ubv_length(ubv) );
    int cnt = constant_power_of_two(ubv);
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
    new_buf(&q, sz, sizeof(ubv));
    new_buf(&r, sz, sizeof(ubv));
    new_buf(&bb, sz, sizeof(ubv));
    new_buf(&sub_buf, sz, sizeof(ubv));
    ubv zero = UBV_Zero();
    for(int i = 0; i < sz; i++) {
	ubv b = GET_UBV(GET_CONS_HD(ubv));
	ubv = GET_CONS_TL(ubv);
	push_buf(&bb, (pointer) &b);
	push_buf(&q, (pointer) &zero);
	push_buf(&r, (pointer) &zero);
	push_buf(&sub_buf, (pointer) &zero);
    }
    while( !IS_NIL(av) ) {
	ubv a = GET_UBV(GET_CONS_HD(av));
	for(int i = 1; i < sz; i++) {
	    ubv ri = *((ubv *) M_LOCATE_BUF(&r, i));
	    store_buf(&r, i-1, (pointer) &ri);
	}
	store_buf(&r, sz-1, (pointer) &a);
	for(int i = 0; i < sz; i++) {
	    ubv ri = *((ubv *) M_LOCATE_BUF(&r, i));
	    store_buf(&sub_buf, i, (pointer) &ri);
	}
	ubv cin = UBV_One();
	for(int i = sz-1; i >= 0; i--) {
	    ubv x = *((ubv *) M_LOCATE_BUF(&sub_buf, i));
	    ubv y = UBV_Not(*((ubv *) M_LOCATE_BUF(&bb, i)));
	    ubv sum = UBV_Xor(cin, UBV_Xor(x,y));
	    store_buf(&sub_buf, i, (pointer) &sum);
	    cin = UBV_Or(UBV_And(x,y), UBV_Or(UBV_And(x,cin), UBV_And(y,cin)));
	}
	ubv geq = UBV_One();
	for(int i = sz-1; i >= 0; i--) {
	    ubv ri = *((ubv *) M_LOCATE_BUF(&r, i));
	    ubv bi = *((ubv *) M_LOCATE_BUF(&bb, i));
	    geq = UBV_Or(UBV_And(ri, UBV_Not(bi)),
		       UBV_And(UBV_Xnor(ri,bi), geq));
	}
	for(int i = 0; i < sz-1; i++) {
	    ubv ri = *((ubv *) M_LOCATE_BUF(&r, i));
	    ubv subi = *((ubv *) M_LOCATE_BUF(&sub_buf, i));
	    ubv qi = *((ubv *) M_LOCATE_BUF(&q, i+1));
	    ubv Qi = qi;
	    ubv Ri = UBV_Ite(geq, subi, ri);
	    store_buf(&r, i, (pointer) &Ri);
	    store_buf(&q, i, (pointer) &Qi);
	}
	ubv ri = *((ubv *) M_LOCATE_BUF(&r, sz-1));
	ubv subi = *((ubv *) M_LOCATE_BUF(&sub_buf, sz-1));
	ubv Q0 = geq;
	ubv R0 = UBV_Ite(geq, subi, ri);
	store_buf(&r, sz-1, (pointer) &R0);
	store_buf(&q, sz-1, (pointer) &Q0);
	av = GET_CONS_TL(av);
    }
    g_ptr Q = Make_NIL();
    for(int i = sz-1; i >= 0; i--) {
	ubv qi = *((ubv *) M_LOCATE_BUF(&q, i));
	Q = Make_CONS_ND(Make_UBV_leaf(qi), Q);
    }
    g_ptr R = Make_NIL();
    for(int i = sz-1; i >= 0; i--) {
	ubv ri = *((ubv *) M_LOCATE_BUF(&r, i));
	R = Make_CONS_ND(Make_UBV_leaf(ri), R);
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
ubv_construct(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    INC_REFCNT(list);
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(list));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_destruct(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    ubv_ptr bp = (ubv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    OVERWRITE(redex, list);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv2str(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    int depth = GET_INT(arg1);
    ubv_ptr bp = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr list = bp->u.l;
    string res = ubv_list2str(list, depth);
    MAKE_REDEX_STRING(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_size(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    ubv_ptr bp = (ubv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    MAKE_REDEX_INT(redex, get_ubv_length(list));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_var(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(arg1);
    g_ptr nl = Vec2nodes(name);
    for(g_ptr np = nl; !IS_NIL(np); np = GET_CONS_TL(np)) {
	g_ptr l = GET_CONS_HD(np);
	ubv v = UBV_Var(GET_STRING(l));
	MAKE_REDEX_UBV(l, v);
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(nl));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static arbi_T
ubv_list2int(g_ptr l, arbi_T *ppow)
{
    if( IS_NIL(l) ) {
	*ppow = Arbi_FromInt(1);
	return( Arbi_FromInt(0) );
    }
    arbi_T my_pow;
    arbi_T rem = ubv_list2int(GET_CONS_TL(l), &my_pow);
    bool is_zero = ((GET_UBV(GET_CONS_HD(l))) == UBV_Zero());
    arbi_T cur = is_zero? Arbi_FromInt(0) : my_pow;
    *ppow = Arbi_mlt(my_pow, Arbi_FromInt(2));
    return( Arbi_add(cur, rem) );
}

static void
ubv2int(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    ubv_ptr bp = (ubv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    if( !is_scalar_list(list) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("ubv2int of symbolic bitvector"));
	return;
    }
    bool neg = (GET_UBV(GET_CONS_HD(list)) == UBV_One());
    g_ptr abs = abs_ubv_list(list);
    arbi_T pow;
    arbi_T abs_res = ubv_list2int(abs, &pow);
    arbi_T res = neg? Arbi_neg(abs_res) : abs_res;
    MAKE_REDEX_AINT(redex, res)
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
int2ubv(g_ptr redex)
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
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_add(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = gen_add(FALSE, l1, l2);
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_sub(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = gen_add(TRUE, l1, l2);
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_mul(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr av  = a1->u.l;
    g_ptr ubv  = a2->u.l;
    int len_av = get_ubv_length(av);
    int len_ubv = get_ubv_length(ubv);
    // Make sure ubv is the shorter one
    if( len_av < len_ubv ) {
	int len_tmp = len_av;
	len_av = len_ubv;
	len_ubv = len_tmp;
	g_ptr tmp = av;
	av = ubv;
	ubv = tmp;
    }
    ubv neg_a = GET_UBV(GET_CONS_HD(av));
    g_ptr abs_av = abs_ubv_list(av);
    ubv neg_b = GET_UBV(GET_CONS_HD(ubv));
    g_ptr abs_ubv = abs_ubv_list(ubv);
    g_ptr abs_res = Make_CONS_ND(Zero,
				 mult_ubv_list(abs_av, abs_ubv));
    ubv neg = UBV_Xor(neg_a, neg_b);
    g_ptr neg_res = negate_ubv_list(abs_res);
    sx2(&abs_res, &neg_res);
    g_ptr res = ite_ubv_list(neg, neg_res, abs_res);
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
signed_ubvdiv(g_ptr dv, g_ptr nv, g_ptr *Q, g_ptr *R)
{
    ubv neg_d = GET_UBV(GET_CONS_HD(dv));
    ubv neg_n = GET_UBV(GET_CONS_HD(nv));
    g_ptr   abs_dv = abs_ubv_list(dv);
    g_ptr   abs_nv = abs_ubv_list(nv);
    zx2(&abs_dv, &abs_nv);
    g_ptr Qraw;
    g_ptr Rraw;
    fixed_ext_ubvdiv(abs_dv, abs_nv, &Qraw, &Rraw);
    ubv exact = is_zero_list(Rraw);
    ubv same_sign = UBV_Xnor(neg_d, neg_n);
    g_ptr Q0 = ite_ubv_list(same_sign, Qraw, (negate_ubv_list(Qraw)));
    *Q = ite_ubv_list(UBV_Or(exact,same_sign), Q0, decrement_ubv_list(Q0));
    g_ptr R0 = ite_ubv_list(same_sign, Rraw, gen_add(TRUE, Rraw, abs_nv));
    *R = ite_ubv_list(exact, Rraw, ite_ubv_list(neg_d, negate_ubv_list(R0),R0));
}

static void
ubv_ashr(g_ptr redex)
{
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    int cnt = GET_INT(arg2);
    if( cnt < 0 ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("ubv_ashr by negative amount (%d)", cnt));
	return;
    }
    if( cnt == 0 ) {
	OVERWRITE(redex, arg1);
	return;
    }
    ubv_ptr a = (ubv_ptr) GET_EXT_OBJ(arg1);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr al = a->u.l;
    int len = get_ubv_length(al);
    for(int i = 0; i < len-cnt; i++) {
	APPEND1(tail, GET_CONS_HD(al));
	al = GET_CONS_TL(al);
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
}

static void
ubv_div(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr Q;
    g_ptr R; 
    signed_ubvdiv(a1->u.l, a2->u.l, &Q, &R);
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(Q));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_mod(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr Q;
    g_ptr R;
    signed_ubvdiv(a1->u.l, a2->u.l, &Q, &R);
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(R));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_NOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    ubv_ptr bp = (ubv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    for(g_ptr np = list; !IS_NIL(np); np=GET_CONS_TL(np)) {
	ubv a = GET_UBV(GET_CONS_HD(np));
	APPEND1(tail, Make_UBV_leaf(UBV_Not(a)));
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_ZX(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    ubv_ptr bp = (ubv_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    g_ptr res = trim_ubv(Make_CONS_ND(Zero, list));
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_AND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	ubv a = GET_UBV(GET_CONS_HD(n1));
	ubv b = GET_UBV(GET_CONS_HD(n2));
	APPEND1(tail, Make_UBV_leaf(UBV_And(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_OR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	ubv a = GET_UBV(GET_CONS_HD(n1));
	ubv b = GET_UBV(GET_CONS_HD(n2));
	APPEND1(tail, Make_UBV_leaf(UBV_Or(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_XOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	ubv a = GET_UBV(GET_CONS_HD(n1));
	ubv b = GET_UBV(GET_CONS_HD(n2));
	APPEND1(tail, Make_UBV_leaf(UBV_Xor(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_XNOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	ubv a = GET_UBV(GET_CONS_HD(n1));
	ubv b = GET_UBV(GET_CONS_HD(n2));
	APPEND1(tail, Make_UBV_leaf(UBV_Xnor(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, ubv_oidx, get_ubv_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_less(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    ubv res = less_than_ubv_list(l1, l2);
    MAKE_REDEX_UBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_leq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    ubv res = UBV_Not(less_than_ubv_list(l2, l1));
    MAKE_REDEX_UBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_greater(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    ubv res = less_than_ubv_list(l2, l1);
    MAKE_REDEX_UBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_geq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    ubv res = UBV_Not(less_than_ubv_list(l1, l2));
    MAKE_REDEX_UBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_eq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    ubv res = ubv_eq_list(l1, l2);
    MAKE_REDEX_UBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
ubv_neq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    ubv_ptr a1 = (ubv_ptr) GET_EXT_OBJ(arg1);
    ubv_ptr a2 = (ubv_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    ubv res = UBV_Not(ubv_eq_list(l1, l2));
    MAKE_REDEX_UBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Ubv_Init()
{
    new_mgr(&ubv_rec_mgr, sizeof(ubv_rec));
    Zero = Make_UBV_leaf(UBV_Zero());
    PUSH_GLOBAL_GC(Zero);
    One = Make_UBV_leaf(UBV_One());
    PUSH_GLOBAL_GC(One);
    ubv_free_list = NULL;
    ubv_oidx  = Add_ExtAPI_Object("ubv",
				 mark_ubv_fn,
				 sweep_ubv_fn,
				 save_ubv_fn,
				 load_ubv_fn,
				 ubv2str_fn,
				 ubv_eq_fn,
				 ubv_hash_fn,
				 ubv_gmap_fn,
				 ubv_gmap2_fn,
				 ubv_sha256_fn);
    ubv_handle_tp  = Get_Type("ubv", NULL, TP_INSERT_FULL_TYPE);
}

void
Ubv_Install_Functions()
{
    // Add builtin functions

    Add_ExtAPI_Function("list2ubv", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_ubv()),ubv_handle_tp),
			ubv_construct);

    Add_ExtAPI_Function("ubv2list", "1", FALSE,
			GLmake_arrow(ubv_handle_tp,GLmake_list(GLmake_ubv())),
			ubv_destruct);

    Add_ExtAPI_Function("ubv_size", "1", FALSE,
			GLmake_arrow(ubv_handle_tp, GLmake_int()),
			ubv_size);

    Add_ExtAPI_Function("int2ubv", "1", FALSE,
			GLmake_arrow(GLmake_int(), ubv_handle_tp),
			int2ubv);

    Add_ExtAPI_Function("ubv2int", "1", FALSE,
			GLmake_arrow(ubv_handle_tp, GLmake_int()),
			ubv2int);

    Add_ExtAPI_Function("ubv_variable", "1", FALSE,
			GLmake_arrow(GLmake_string(), ubv_handle_tp),
			ubv_var);

    Add_ExtAPI_Function("ubv2str", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_string())),
			ubv2str);

    Add_ExtAPI_Function("ubv_add", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_add);

    Add_ExtAPI_Function("ubv_sub", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_sub);

    Add_ExtAPI_Function("ubv_mul", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_mul);

    Add_ExtAPI_Function("ubv_ashr", "11", FALSE,
			GLmake_arrow(
				 ubv_handle_tp,
				 GLmake_arrow(GLmake_int(), ubv_handle_tp)),
			ubv_ashr);

    Add_ExtAPI_Function("ubv_div", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_div);

    Add_ExtAPI_Function("ubv_mod", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_mod);

    Add_ExtAPI_Function("ubv_NOT", "1", FALSE,
			 GLmake_arrow(ubv_handle_tp, ubv_handle_tp),
			ubv_NOT);

    Add_ExtAPI_Function("ubv_ZX", "1", FALSE,
			 GLmake_arrow(ubv_handle_tp, ubv_handle_tp),
			ubv_ZX);

    Add_ExtAPI_Function("ubv_AND", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_AND);

    Add_ExtAPI_Function("ubv_OR", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_OR);

    Add_ExtAPI_Function("ubv_XOR", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_XOR);

    Add_ExtAPI_Function("ubv_XNOR", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				 GLmake_arrow(ubv_handle_tp, ubv_handle_tp)),
			ubv_XNOR);

    Add_ExtAPI_Function("ubv_less", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_ubv())),
			ubv_less);

    Add_ExtAPI_Function("ubv_leq", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_ubv())),
			ubv_leq);

    Add_ExtAPI_Function("ubv_greater", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_ubv())),
			ubv_greater);

    Add_ExtAPI_Function("ubv_geq", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_ubv())),
			ubv_geq);

    Add_ExtAPI_Function("ubv_eq", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_ubv())),
			ubv_eq);

    Add_ExtAPI_Function("ubv_neq", "11", FALSE,
			GLmake_arrow(ubv_handle_tp,
				     GLmake_arrow(ubv_handle_tp,
						  GLmake_ubv())),
			ubv_neq);

}

