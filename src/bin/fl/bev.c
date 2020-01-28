/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 2019			*/
/*									*/
/************************************************************************/
#include "bev.h"
#include "graph.h"

#include <math.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     strings;
extern FILE        *odests_fp;

/***** PRIVATE VARIABLES *****/
static int	    bev_oidx;
static typeExp_ptr  bev_handle_tp;
static rec_mgr	    bev_rec_mgr;
static bev_ptr	    bev_free_list;
static char	    bev_str_buf[4096];
static g_ptr	    Zero;
static g_ptr	    One;

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static g_ptr
trim_bev(g_ptr l)
{
    while (1) {
	if( IS_NIL(l) ) return l;
	g_ptr next = GET_CONS_TL(l);
	if( IS_NIL(next) ) return l;
	if( GET_BEXPR(GET_CONS_HD(l)) == GET_BEXPR(GET_CONS_HD(next)) ) {
	    l = next;
	} else {
	    return l;
	}
    }
}

static bev_ptr
get_bev_rec(g_ptr l)
{
    bev_ptr res;
    if( bev_free_list != NULL ) {
	res = bev_free_list;
	bev_free_list = bev_free_list->u.next;
    } else {
	res = (bev_ptr) new_rec(&bev_rec_mgr);
    }
    res->flag = 0;
    res->u.l = trim_bev(l);
    return res;
}

static void
mark_bev_fn(pointer p)
{
    bev_ptr fp = (bev_ptr) p;
    fp->flag = 1;
    Mark(fp->u.l);
    return;
}

static void
sweep_bev_fn(void)
{
    bev_ptr fp;
    bev_free_list = NULL;
    FOR_REC(&bev_rec_mgr, bev_ptr, fp) {
	if( fp->flag == 1 ) {
	    fp->flag = 0;
	} else {
	    fp->u.next = bev_free_list;
	    bev_free_list = fp;
	}
    }
}

static int
get_bev_length(g_ptr l)
{
    int len = 0;
    while( !IS_NIL(l) ) {
	len++;
	l = GET_CONS_TL(l);
    }
    return len;
}

static void
save_bev_fn(FILE *fp, pointer p)
{
    bev_ptr bevp = (bev_ptr) p;
    g_ptr l = bevp->u.l;
    fprintf(fp, "%d", get_bev_length(l));
    for(g_ptr np = l; !IS_NIL(np); np = GET_CONS_TL(np)) {
	bexpr f = GET_BEXPR(GET_CONS_HD(np));
	fprintf(fp, "  %d", Save_get_bexpr_idx(f));
    }
    fputc('\n', fp);	// Add newline for human readability
}

static pointer
load_bev_fn(FILE *fp)
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
	bexpr f = Load_get_bexpr_from_idx(idx);
	APPEND1(tail, Make_BEXPR_leaf(f));
    }
    fgetc(fp);	// Eat new line
    return( (pointer) get_bev_rec(res) );
}

static bool
is_symbolic(bexpr f)
{
    return( f != BE_One() && f != BE_Zero() );
}

static string
bev_list2str(g_ptr l, int depth)
{
    if((odests_fp = fmemopen(bev_str_buf, 4096, "w")) == NULL ) {
	DIE("Should never happen");
    }
    char sep = '<';
    for(g_ptr np = l; !IS_NIL(np); np = GET_CONS_TL(np)) {
	bexpr f = GET_BEXPR(GET_CONS_HD(np));
	FP(FILE_fp,"%c", sep);
	sep = ',';
	if( depth == 0 && is_symbolic(f) ) {
	    FP(FILE_fp, "S");
	} else {
	    BE_Print(FILE_fp, f);
	}
    }
    FP(FILE_fp, ">");
    fclose(odests_fp);
    odests_fp = NULL;
    return( wastrsave(&strings, bev_str_buf) );
}

static string
bev2str_fn(pointer p)
{
    bev_ptr bp = (bev_ptr) p;
    g_ptr l = bp->u.l;
    return( bev_list2str(l, -1) );
}

static formula
bev_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    bev_ptr bp1 = (bev_ptr) p1;
    bev_ptr bp2 = (bev_ptr) p2;
    g_ptr   l1 = bp1->u.l;
    g_ptr   l2 = bp2->u.l;
    int	    len1 = get_bev_length(l1);
    int	    len2 = get_bev_length(l2);
    // Make the first list shorter or equal to the second list
    if( len1 > len2 ) {
	int len = len2;
	len2 = len1;
	len1 = len;
	g_ptr l = l2;
	l2 = l1;
	l1 = l;
    }
    bexpr b1 = GET_BEXPR(GET_CONS_HD(l1));
    while( len2 > len1 ) {
	bexpr b2 = GET_BEXPR(GET_CONS_HD(l2));
	if( !BE_Equal(b1, b2) ) { return( B_Zero() ); }
	len2--;
	l2 = GET_CONS_TL(l2);
    }
    while( len2 > 0 ) {
	bexpr b1 = GET_BEXPR(GET_CONS_HD(l1));
	bexpr b2 = GET_BEXPR(GET_CONS_HD(l2));
	if( !BE_Equal(b1, b2) ) { return( B_Zero() ); }
	len2--;
	l1 = GET_CONS_TL(l1);
	l2 = GET_CONS_TL(l2);
    }
    return( B_One() );
}

static pointer
bev_gmap_fn(gmap_info_ptr ip, pointer a)
{
    bev_ptr bp = (bev_ptr) a;
    g_ptr  l = bp->u.l;
    if( ip->read_only ) {
	Gen_map(ip->u.leaf_fun, l, ip->read_only);
	return NULL;
    } else {
	g_ptr res_l = Gen_map(ip->u.leaf_fun, l, ip->read_only);
	if( res_l == l ) {
	    return a;
	} else {
	    return( (pointer) get_bev_rec(res_l) );
	}
    }
}

static int
sx2(g_ptr *lp1, g_ptr *lp2)
{
    g_ptr l1 = *lp1;
    g_ptr l2 = *lp2;
    int	    len1 = get_bev_length(l1);
    int	    len2 = get_bev_length(l2);
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
    int	    len1 = get_bev_length(l1);
    int	    len2 = get_bev_length(l2);
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
bev_gmap2_fn(gmap_info_ptr ip, pointer a1, pointer a2)
{
    bev_ptr  bp1 = (bev_ptr) a1;
    bev_ptr  bp2 = (bev_ptr) a2;
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
	return( (pointer) get_bev_rec(res_l) );
    }
}

static g_ptr
add_rec(bool neg, g_ptr l1, g_ptr l2, bexpr *coutp)
{
    if( IS_NIL(l1) ) {
	ASSERT(IS_NIL(l2));
	*coutp = neg? BE_One() : BE_Zero();
	return( Make_NIL() );
    }
    bexpr cin;
    g_ptr rem = add_rec(neg, GET_CONS_TL(l1), GET_CONS_TL(l2), &cin);
    bexpr a = GET_BEXPR(GET_CONS_HD(l1));
    bexpr b = GET_BEXPR(GET_CONS_HD(l2));
    b = neg? BE_Not(b) : b;
    bexpr sum = BE_Xor(a, BE_Xor(b, cin));
    rem = Make_CONS_ND(Make_BEXPR_leaf(sum), rem);
    *coutp = BE_Or( BE_And(a, b), BE_Or( BE_And(a, cin), BE_And(b, cin)));
    return( rem );
}

static g_ptr
gen_add(bool neg, g_ptr l1, g_ptr l2)
{
    bexpr cout;
    l1 = Make_CONS_ND(GET_CONS_HD(l1), l1);
    l2 = Make_CONS_ND(GET_CONS_HD(l2), l2);
    g_ptr res = add_rec(neg, l1, l2, &cout);
    return( trim_bev(res) );
}

static bexpr
is_zero_list(g_ptr l)
{
    bexpr zero = BE_One();
    while( !IS_NIL(l) ) {
	zero = BE_And(zero, BE_Not(GET_BEXPR(GET_CONS_HD(l))));
	l = GET_CONS_TL(l);
    }
    return zero;
}

static g_ptr
negate_bev_list(g_ptr l)
{
    g_ptr zero = Make_CONS_ND(Zero, Make_NIL());
    sx2(&l,&zero);
    return( trim_bev(gen_add(TRUE, zero, l)) );
}

static g_ptr
decrement_bev_list(g_ptr l)
{   
    g_ptr one = Make_CONS_ND(Zero, Make_CONS_ND(One, Make_NIL()));
    sx2(&l,&one);
    return( trim_bev(gen_add(TRUE, l, one)) );
}   

static g_ptr
ite_bev_list(bexpr cond, g_ptr l1, g_ptr l2)
{
    sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1=l1, n2=l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	bexpr nb = GET_BEXPR(GET_CONS_HD(n1));
	bexpr pb = GET_BEXPR(GET_CONS_HD(n2));
	bexpr b = BE_Or(BE_And(cond, nb), BE_And(BE_Not(cond), pb));
	APPEND1(tail, Make_BEXPR_leaf(b));
    }
    return( trim_bev(res) );
}

static g_ptr
abs_bev_list(g_ptr l)
{
    bexpr neg = GET_BEXPR(GET_CONS_HD(l));
    g_ptr l_neg = negate_bev_list(l);
    return( trim_bev(ite_bev_list(neg, l_neg, l)) );
}

static g_ptr
mult_bev_list(g_ptr xv, g_ptr yv)
{
    if( IS_NIL(yv) ) {
	return( Make_CONS_ND(Zero, Make_NIL()) );
    }
    g_ptr rem = mult_bev_list(xv, GET_CONS_TL(yv));
    g_ptr prod = Make_NIL();
    g_ptr tail = prod;
    APPEND1(tail, Zero);
    bexpr y = GET_BEXPR(GET_CONS_HD(yv));
    for(g_ptr np = xv; !IS_NIL(np); np = GET_CONS_TL(np)) {
	bexpr x = GET_BEXPR(GET_CONS_HD(np));
	bexpr b = BE_And(y,x);
	APPEND1(tail, Make_BEXPR_leaf(b));
    }
    for(int i = 0; i < get_bev_length(yv)-1; i++) {
	APPEND1(tail, Zero);
    }
    zx2(&prod, &rem);
    return( trim_bev(gen_add(FALSE, prod, rem)) );
}

static bexpr
less_rec(g_ptr l1, g_ptr l2)
{
    if( IS_NIL(l1) ) {
	ASSERT(IS_NIL(l2));
	return( BE_Zero() );
    }
    bexpr a = GET_BEXPR(GET_CONS_HD(l1));
    bexpr b = GET_BEXPR(GET_CONS_HD(l2));
    return( BE_Or(BE_And(BE_Not(a), b),
		 BE_And(BE_Xnor(a,b),
		       less_rec(GET_CONS_TL(l1),GET_CONS_TL(l2)))));
}

static bexpr
less_than_bev_list(g_ptr l1, g_ptr l2)
{
    sx2(&l1, &l2);
    bexpr neg1 = GET_BEXPR(GET_CONS_HD(l1));
    bexpr neg2 = GET_BEXPR(GET_CONS_HD(l2));
    bexpr raw_res = less_rec(l1, l2);
    return( BE_Ite(neg1, BE_Ite(neg2, raw_res, BE_One()),
	   	        BE_Ite(neg2, BE_Zero(), raw_res)) );

}

static bexpr
bev_eq_list(g_ptr l1, g_ptr l2)
{
    int	len1 = get_bev_length(l1);
    int	len2 = get_bev_length(l2);
    // Make the first list shorter or equal to the second list
    if( len1 > len2 ) {
	int len = len2;
	len2 = len1;
	len1 = len;
	g_ptr l = l2;
	l2 = l1;
	l1 = l;
    }
    bexpr res = BE_One();
    bexpr b1 = GET_BEXPR(GET_CONS_HD(l1));
    while( len2 > len1 ) {
	bexpr b2 = GET_BEXPR(GET_CONS_HD(l2));
	res = BE_And(res, BE_Xnor(b1, b2));
	len2--;
	l2 = GET_CONS_TL(l2);
    }
    while( len2 > 0 ) {
	bexpr b1 = GET_BEXPR(GET_CONS_HD(l1));
	bexpr b2 = GET_BEXPR(GET_CONS_HD(l2));
	res = BE_And(res, BE_Xnor(b1, b2));
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
	bexpr f = GET_BEXPR(GET_CONS_HD(np));
	if( is_symbolic(f) ) {
	    scalar = FALSE;
	}
    }
    return( scalar );
}

static int
constant_power_of_two(g_ptr bev)
{
    if( !is_scalar_list(bev) ) { return( -1 ); }
    bool found = FALSE;
    while( !found && !IS_NIL(bev) ) {
	if( GET_BEXPR(GET_CONS_HD(bev)) == BE_One() ) {
	    found = TRUE;
	} 
	bev = GET_CONS_TL(bev);
    }
    if( !found ) { return(-1); }
    int cnt = 0;
    while( !IS_NIL(bev) ) {
	if( GET_BEXPR(GET_CONS_HD(bev)) == BE_One() ) {
	    return( -1 );
	}
	cnt++;
	bev = GET_CONS_TL(bev);
    }
    return( cnt );
}

static void
fixed_ext_bevdiv(g_ptr av, g_ptr bev, g_ptr *Qp, g_ptr *Rp)
{
    int sz = get_bev_length(av);
    ASSERT( sz == get_bev_length(bev) );
    int cnt = constant_power_of_two(bev);
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
    new_buf(&q, sz, sizeof(bexpr));
    new_buf(&r, sz, sizeof(bexpr));
    new_buf(&bb, sz, sizeof(bexpr));
    new_buf(&sub_buf, sz, sizeof(bexpr));
    bexpr zero = BE_Zero();
    for(int i = 0; i < sz; i++) {
	bexpr b = GET_BEXPR(GET_CONS_HD(bev));
	bev = GET_CONS_TL(bev);
	push_buf(&bb, (pointer) &b);
	push_buf(&q, (pointer) &zero);
	push_buf(&r, (pointer) &zero);
	push_buf(&sub_buf, (pointer) &zero);
    }
    while( !IS_NIL(av) ) {
	bexpr a = GET_BEXPR(GET_CONS_HD(av));
	for(int i = 1; i < sz; i++) {
	    bexpr ri = *((bexpr *) M_LOCATE_BUF(&r, i));
	    store_buf(&r, i-1, (pointer) &ri);
	}
	store_buf(&r, sz-1, (pointer) &a);
	for(int i = 0; i < sz; i++) {
	    bexpr ri = *((bexpr *) M_LOCATE_BUF(&r, i));
	    store_buf(&sub_buf, i, (pointer) &ri);
	}
	bexpr cin = BE_One();
	for(int i = sz-1; i >= 0; i--) {
	    bexpr x = *((bexpr *) M_LOCATE_BUF(&sub_buf, i));
	    bexpr y = BE_Not(*((bexpr *) M_LOCATE_BUF(&bb, i)));
	    bexpr sum = BE_Xor(cin, BE_Xor(x,y));
	    store_buf(&sub_buf, i, (pointer) &sum);
	    cin = BE_Or(BE_And(x,y), BE_Or(BE_And(x,cin), BE_And(y,cin)));
	}
	bexpr geq = BE_One();
	for(int i = sz-1; i >= 0; i--) {
	    bexpr ri = *((bexpr *) M_LOCATE_BUF(&r, i));
	    bexpr bi = *((bexpr *) M_LOCATE_BUF(&bb, i));
	    geq = BE_Or(BE_And(ri, BE_Not(bi)),
		       BE_And(BE_Xnor(ri,bi), geq));
	}
	for(int i = 0; i < sz-1; i++) {
	    bexpr ri = *((bexpr *) M_LOCATE_BUF(&r, i));
	    bexpr subi = *((bexpr *) M_LOCATE_BUF(&sub_buf, i));
	    bexpr qi = *((bexpr *) M_LOCATE_BUF(&q, i+1));
	    bexpr Qi = qi;
	    bexpr Ri = BE_Ite(geq, subi, ri);
	    store_buf(&r, i, (pointer) &Ri);
	    store_buf(&q, i, (pointer) &Qi);
	}
	bexpr ri = *((bexpr *) M_LOCATE_BUF(&r, sz-1));
	bexpr subi = *((bexpr *) M_LOCATE_BUF(&sub_buf, sz-1));
	bexpr Q0 = geq;
	bexpr R0 = BE_Ite(geq, subi, ri);
	store_buf(&r, sz-1, (pointer) &R0);
	store_buf(&q, sz-1, (pointer) &Q0);
	av = GET_CONS_TL(av);
    }
    g_ptr Q = Make_NIL();
    for(int i = sz-1; i >= 0; i--) {
	bexpr qi = *((bexpr *) M_LOCATE_BUF(&q, i));
	Q = Make_CONS_ND(Make_BEXPR_leaf(qi), Q);
    }
    g_ptr R = Make_NIL();
    for(int i = sz-1; i >= 0; i--) {
	bexpr ri = *((bexpr *) M_LOCATE_BUF(&r, i));
	R = Make_CONS_ND(Make_BEXPR_leaf(ri), R);
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
bev_construct(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr list = GET_APPLY_RIGHT(redex);
    INC_REFCNT(list);
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(list));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_destruct(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bev_ptr bp = (bev_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    OVERWRITE(redex, list);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev2str(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    int depth = GET_INT(arg1);
    bev_ptr bp = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr list = bp->u.l;
    string res = bev_list2str(list, depth);
    MAKE_REDEX_STRING(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_size(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bev_ptr bp = (bev_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    MAKE_REDEX_INT(redex, get_bev_length(list));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_var(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(arg1);
    g_ptr nl = Vec2nodes(name);
    for(g_ptr np = nl; !IS_NIL(np); np = GET_CONS_TL(np)) {
	g_ptr l = GET_CONS_HD(np);
	bexpr v = BE_Var(GET_STRING(l));
	MAKE_REDEX_BEXPR(l, v);
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(nl));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static arbi_T
bev_list2int(g_ptr l, arbi_T *ppow)
{
    if( IS_NIL(l) ) {
	*ppow = Arbi_FromInt(1);
	return( Arbi_FromInt(0) );
    }
    arbi_T my_pow;
    arbi_T rem = bev_list2int(GET_CONS_TL(l), &my_pow);
    bool is_zero = ((GET_BEXPR(GET_CONS_HD(l))) == BE_Zero());
    arbi_T cur = is_zero? Arbi_FromInt(0) : my_pow;
    *ppow = Arbi_mlt(my_pow, Arbi_FromInt(2));
    return( Arbi_add(cur, rem) );
}

static void
bev2int(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bev_ptr bp = (bev_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    if( !is_scalar_list(list) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("bev2int of symbolic bitvector"));
	return;
    }
    bool neg = (GET_BEXPR(GET_CONS_HD(list)) == BE_One());
    g_ptr abs = abs_bev_list(list);
    arbi_T pow;
    arbi_T abs_res = bev_list2int(abs, &pow);
    arbi_T res = neg? Arbi_neg(abs_res) : abs_res;
    MAKE_REDEX_AINT(redex, res)
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
int2bev(g_ptr redex)
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
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_add(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = gen_add(FALSE, l1, l2);
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_sub(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = gen_add(TRUE, l1, l2);
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_mul(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr av  = a1->u.l;
    g_ptr bev  = a2->u.l;
    int len_av = get_bev_length(av);
    int len_bev = get_bev_length(bev);
    // Make sure bev is the shorter one
    if( len_av < len_bev ) {
	int len_tmp = len_av;
	len_av = len_bev;
	len_bev = len_tmp;
	g_ptr tmp = av;
	av = bev;
	bev = tmp;
    }
    bexpr neg_a = GET_BEXPR(GET_CONS_HD(av));
    g_ptr abs_av = abs_bev_list(av);
    bexpr neg_b = GET_BEXPR(GET_CONS_HD(bev));
    g_ptr abs_bev = abs_bev_list(bev);
    g_ptr abs_res = Make_CONS_ND(Zero,
				 mult_bev_list(abs_av, abs_bev));
    bexpr neg = BE_Xor(neg_a, neg_b);
    g_ptr neg_res = negate_bev_list(abs_res);
    sx2(&abs_res, &neg_res);
    g_ptr res = ite_bev_list(neg, neg_res, abs_res);
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
signed_bevdiv(g_ptr dv, g_ptr nv, g_ptr *Q, g_ptr *R)
{
    bexpr neg_d = GET_BEXPR(GET_CONS_HD(dv));
    bexpr neg_n = GET_BEXPR(GET_CONS_HD(nv));
    g_ptr   abs_dv = abs_bev_list(dv);
    g_ptr   abs_nv = abs_bev_list(nv);
    zx2(&abs_dv, &abs_nv);
    g_ptr Qraw;
    g_ptr Rraw;
    fixed_ext_bevdiv(abs_dv, abs_nv, &Qraw, &Rraw);
    bexpr exact = is_zero_list(Rraw);
    bexpr same_sign = BE_Xnor(neg_d, neg_n);
    g_ptr Q0 = ite_bev_list(same_sign, Qraw, (negate_bev_list(Qraw)));
    *Q = ite_bev_list(BE_Or(exact,same_sign), Q0, decrement_bev_list(Q0));
    g_ptr R0 = ite_bev_list(same_sign, Rraw, gen_add(TRUE, Rraw, abs_nv));
    *R = ite_bev_list(exact, Rraw, ite_bev_list(neg_d, negate_bev_list(R0),R0));
}

static void
bev_ashr(g_ptr redex)
{
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    int cnt = GET_INT(arg2);
    if( cnt < 0 ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("bev_ashr by negative amount (%d)", cnt));
	return;
    }
    if( cnt == 0 ) {
	OVERWRITE(redex, arg1);
	return;
    }
    bev_ptr a = (bev_ptr) GET_EXT_OBJ(arg1);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr al = a->u.l;
    int len = get_bev_length(al);
    for(int i = 0; i < len-cnt; i++) {
	APPEND1(tail, GET_CONS_HD(al));
	al = GET_CONS_TL(al);
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
}

static void
bev_div(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr Q;
    g_ptr R; 
    signed_bevdiv(a1->u.l, a2->u.l, &Q, &R);
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(Q));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_mod(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr Q;
    g_ptr R;
    signed_bevdiv(a1->u.l, a2->u.l, &Q, &R);
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(R));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_NOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bev_ptr bp = (bev_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    for(g_ptr np = list; !IS_NIL(np); np=GET_CONS_TL(np)) {
	bexpr a = GET_BEXPR(GET_CONS_HD(np));
	APPEND1(tail, Make_BEXPR_leaf(BE_Not(a)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_ZX(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bev_ptr bp = (bev_ptr) GET_EXT_OBJ(GET_APPLY_RIGHT(redex));
    g_ptr list = bp->u.l;
    g_ptr res = trim_bev(Make_CONS_ND(Zero, list));
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_AND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	bexpr a = GET_BEXPR(GET_CONS_HD(n1));
	bexpr b = GET_BEXPR(GET_CONS_HD(n2));
	APPEND1(tail, Make_BEXPR_leaf(BE_And(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_OR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	bexpr a = GET_BEXPR(GET_CONS_HD(n1));
	bexpr b = GET_BEXPR(GET_CONS_HD(n2));
	APPEND1(tail, Make_BEXPR_leaf(BE_Or(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_XOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	bexpr a = GET_BEXPR(GET_CONS_HD(n1));
	bexpr b = GET_BEXPR(GET_CONS_HD(n2));
	APPEND1(tail, Make_BEXPR_leaf(BE_Xor(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_XNOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    (void) sx2(&l1, &l2);
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    g_ptr n1, n2;
    for(n1 = l1, n2 = l2; !IS_NIL(n1); n1=GET_CONS_TL(n1), n2=GET_CONS_TL(n2)) {
	bexpr a = GET_BEXPR(GET_CONS_HD(n1));
	bexpr b = GET_BEXPR(GET_CONS_HD(n2));
	APPEND1(tail, Make_BEXPR_leaf(BE_Xnor(a,b)));
    }
    MAKE_REDEX_EXT_OBJ(redex, bev_oidx, get_bev_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_less(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    bexpr res = less_than_bev_list(l1, l2);
    MAKE_REDEX_BEXPR(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_leq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    bexpr res = BE_Not(less_than_bev_list(l2, l1));
    MAKE_REDEX_BEXPR(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_greater(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    bexpr res = less_than_bev_list(l2, l1);
    MAKE_REDEX_BEXPR(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_geq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    bexpr res = BE_Not(less_than_bev_list(l1, l2));
    MAKE_REDEX_BEXPR(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_eq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    bexpr res = bev_eq_list(l1, l2);
    MAKE_REDEX_BEXPR(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev_neq(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1;
    g_ptr arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    bev_ptr a1 = (bev_ptr) GET_EXT_OBJ(arg1);
    bev_ptr a2 = (bev_ptr) GET_EXT_OBJ(arg2);
    g_ptr l1  = a1->u.l;
    g_ptr l2  = a2->u.l;
    bexpr res = BE_Not(bev_eq_list(l1, l2));
    MAKE_REDEX_BEXPR(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Bev_Init()
{
    new_mgr(&bev_rec_mgr, sizeof(bev_rec));
    Zero = Make_BEXPR_leaf(BE_Zero());
    PUSH_GLOBAL_GC(Zero);
    One = Make_BEXPR_leaf(BE_One());
    PUSH_GLOBAL_GC(One);
    bev_free_list = NULL;
    bev_oidx  = Add_ExtAPI_Object("bev",
				 mark_bev_fn,
				 sweep_bev_fn,
				 save_bev_fn,
				 load_bev_fn,
				 bev2str_fn,
				 bev_eq_fn,
				 bev_gmap_fn,
				 bev_gmap2_fn);
    bev_handle_tp  = Get_Type("bev", NULL, TP_INSERT_FULL_TYPE);
}

void
Bev_Install_Functions()
{
    // Add builtin functions

    Add_ExtAPI_Function("list2bev", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_bexpr()), bev_handle_tp),
			bev_construct);

    Add_ExtAPI_Function("bev2list", "1", FALSE,
			GLmake_arrow(bev_handle_tp, GLmake_list(GLmake_bexpr())),
			bev_destruct);

    Add_ExtAPI_Function("bev_size", "1", FALSE,
			GLmake_arrow(bev_handle_tp, GLmake_int()),
			bev_size);

    Add_ExtAPI_Function("int2bev", "1", FALSE,
			GLmake_arrow(GLmake_int(), bev_handle_tp),
			int2bev);

    Add_ExtAPI_Function("bev2int", "1", FALSE,
			GLmake_arrow(bev_handle_tp, GLmake_int()),
			bev2int);

    Add_ExtAPI_Function("bev_variable", "1", FALSE,
			GLmake_arrow(GLmake_string(), bev_handle_tp),
			bev_var);

    Add_ExtAPI_Function("bev2str", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(bev_handle_tp,
						  GLmake_string())),
			bev2str);

    Add_ExtAPI_Function("bev_add", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_add);

    Add_ExtAPI_Function("bev_sub", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_sub);

    Add_ExtAPI_Function("bev_mul", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_mul);

    Add_ExtAPI_Function("bev_ashr", "11", FALSE,
			GLmake_arrow(
				 bev_handle_tp,
				 GLmake_arrow(GLmake_int(), bev_handle_tp)),
			bev_ashr);

    Add_ExtAPI_Function("bev_div", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_div);

    Add_ExtAPI_Function("bev_mod", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_mod);

    Add_ExtAPI_Function("bev_NOT", "1", FALSE,
			 GLmake_arrow(bev_handle_tp, bev_handle_tp),
			bev_NOT);

    Add_ExtAPI_Function("bev_ZX", "1", FALSE,
			 GLmake_arrow(bev_handle_tp, bev_handle_tp),
			bev_ZX);

    Add_ExtAPI_Function("bev_AND", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_AND);

    Add_ExtAPI_Function("bev_OR", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_OR);

    Add_ExtAPI_Function("bev_XOR", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_XOR);

    Add_ExtAPI_Function("bev_XNOR", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				 GLmake_arrow(bev_handle_tp, bev_handle_tp)),
			bev_XNOR);

    Add_ExtAPI_Function("bev_less", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				     GLmake_arrow(bev_handle_tp,
						  GLmake_bexpr())),
			bev_less);

    Add_ExtAPI_Function("bev_leq", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				     GLmake_arrow(bev_handle_tp,
						  GLmake_bexpr())),
			bev_leq);

    Add_ExtAPI_Function("bev_greater", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				     GLmake_arrow(bev_handle_tp,
						  GLmake_bexpr())),
			bev_greater);

    Add_ExtAPI_Function("bev_geq", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				     GLmake_arrow(bev_handle_tp,
						  GLmake_bexpr())),
			bev_geq);

    Add_ExtAPI_Function("bev_eq", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				     GLmake_arrow(bev_handle_tp,
						  GLmake_bexpr())),
			bev_eq);

    Add_ExtAPI_Function("bev_neq", "11", FALSE,
			GLmake_arrow(bev_handle_tp,
				     GLmake_arrow(bev_handle_tp,
						  GLmake_bexpr())),
			bev_neq);

}

