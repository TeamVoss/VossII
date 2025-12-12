//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2016			*/
/*									*/
/************************************************************************/
#include "bexpr.h"
#include "new_bdd.h"
#include "prefs_ext.h"
#include "graph.h"

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr  *stringsp;
extern char	FailBuf[4096];
extern FILE     *odests_fp;
extern g_ptr	void_nd;
extern bool	Do_gc_asap;

/***** PRIVATE VARIABLES *****/
static rec_mgr	    bexpr_rec_mgr;
static bexpr	    BE_FALSE;
static bexpr	    BE_TRUE;
static hash_record  bexpr_uniq_tbl;
static hash_record  bexpr_sig_tbl;
static bexpr	    free_list = NULL;
static int	    bsize_cnt;
static char	    buf[1024];
static hash_record  depend_tbl;
static hash_record  gl_subst_tbl;
static hash_record  bexpr2bdd_sub_tbl;
static hash_record  res_tbl;
static hash_record  bdd2bexpr_tbl;
static buffer	    bvar_buf;
static buffer	    bvar_sat_buf;
static hash_record  bvar_sat_tbl;
static int	    bexpr_save_cnt;
static hash_record  bexpr_save_tbl;
static string	    s_const_0;
static string	    s_const_1;
static string	    s_variable;
static string	    s_not_variable;
static string	    s_and;
static string	    s_not_and;
static string	    s_gen_bOR;
static string	    s_gen_bAND;
static string	    s_bITE;
static string	    s_bEqual;

static bexpr	    current_bexpr_cond;
static bexpr	    current_bexpr_eq;

/* ----- Forward definitions local functions ----- */
static unsigned int be_hash(pointer p, unsigned int n);
static bool	    be_eq(pointer p1, pointer p2);
static unsigned int be_sig_hash(pointer p, unsigned int n);
static bool	    be_sig_eq(pointer p1, pointer p2);
static void	    mark(bexpr be);
static void	    unmark(bexpr be);
static formula	    be2bdd(bexpr be);
static bexpr	    get_be_node();
static bexpr	    be_find_insert(bexpr p);
static bexpr	    be_and(bexpr f1, bexpr f2);
static void	    be_print(odests fp, bexpr be, int max_to_print);
static void	    be_size(bexpr be);
static g_ptr	    top_be_subst(g_ptr np);
static bexpr	    be_subst(bexpr be);
static g_ptr	    be_record(g_ptr np);
static void	    scan_unmark_be(pointer key, pointer data);
static void	    scan_be_depend(pointer key, pointer data);
static void	    be_depend(bexpr be);
static int	    be_subs_cmp(const void *p1, const void *p2);
static int	    vn_cmp(const void *pi, const void *pj);
static void	    be_sat_depend(bexpr be);
static bool	    check_for_eq_with_SAT(bexpr l, bexpr r,  bexpr c);
static bool	    check_for_satisfaction_with_SAT(bexpr f);
static int	    be_save(FILE *fp, bexpr be);
static g_ptr	    g_be_ite(g_ptr l, g_ptr r);
static g_ptr	    g_be_eq(g_ptr l, g_ptr r);
static ui	    mk_prandom(uint32_t *seedp);
static g_ptr	    g_be_not(g_ptr np);
static g_ptr	    g_be_or(g_ptr l, g_ptr r);
static g_ptr	    g_be_and(g_ptr l, g_ptr r);
static bexpr	    bdd2bexpr(formula f);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

#ifdef DEBUG
int
Dbg_stop()
{
    fprintf(stderr, "-----------Dbg_stop---------\n");
#if 0
    fprintf(stderr, "FailBuf: %s\n", FailBuf);
#endif
    return 0;
}
#endif

/* BE_One -- Return the bexpr bexpr for the constant 1 */
bexpr
BE_One()
{
    return( BE_TRUE );
}

/* BE_Zero -- Return the bexpr bexpr for the constant 0 */
bexpr
BE_Zero()
{
    return( BE_FALSE );
}

/* BE_Var -- Return Boolean bexpr for variable name */
bexpr
BE_Var(string name)
{
    bexpr_rec b;
    b.type = BE_VAR;
    b.l.name = wastrsave(stringsp, name);
    b.height = 1;
    return( be_find_insert(&b) );
}

/* BE_Mark -- Make an external reference to f (at most one reference) */
void
BE_Mark(bexpr f)
{
    mark(f);
}

static void
insert_node_in_signature_tbl(bexpr p)
{
    bexpr cur;
    if( (cur = (bexpr) find_hash(&bexpr_sig_tbl, (pointer) p)) != NULL ) {
	p->same_sig_list = cur->same_sig_list;
	cur->same_sig_list = p;
	return;
    }
    p->same_sig_list = NULL;
    insert_hash(&bexpr_sig_tbl, (pointer) p, (pointer) p);
}
    
/* BE_Clean -- Do a garbage collection */
void
BE_Clean()
{
    bexpr bp;
    mark(BE_FALSE);
    mark(BE_TRUE);
    free_list = NULL;
    dispose_hash(&bexpr_sig_tbl, NULLFCN);
    create_hash(&bexpr_sig_tbl, 100, be_sig_hash, be_sig_eq);
    FOR_REC(&bexpr_rec_mgr, bexpr, bp) {
	if( bp->mark == 1 ) {
	    bp->mark = 0;
	    insert_node_in_signature_tbl(bp);
	} else if( BE_GET_TYPE(bp) == BE_FREE ) {
	    bp->next = free_list;
	    free_list = bp;
	} else {
	    if( find_hash(&bexpr_uniq_tbl, (pointer) bp) != NULL )
		delete_hash(&bexpr_uniq_tbl, (pointer) bp);
	    BE_SET_TYPE(bp, BE_FREE);
	    bp->mark = 0;
	    bp->has_bdd = 0;
	    bp->height = 0;
	    bp->sat_idx = 0;
	    bp->l.be = NULL;
	    bp->r = NULL;
	    bp->same_sig_list = NULL;
	    bp->next = free_list;
	    free_list = bp;
	}
    }
}

/* BE_Not -- Return the complement of a bexpr */
bexpr
BE_Not(bexpr f1)
{
    return( BE_NOT(f1) );
}

bexpr
BE_Nor(bexpr f1, bexpr f2)
{
    return( be_and(BE_NOT(f1), BE_NOT(f2)) );
}

bexpr
BE_Nand(bexpr f1, bexpr f2)
{
    return( BE_NOT(be_and(f1, f2)) );
}

bexpr
BE_And(bexpr f1, bexpr f2)
{
    return( be_and(f1, f2) );
}

bexpr
BE_Ite(bexpr c, bexpr t, bexpr e)
{
    bexpr res = BE_Or(be_and(c,t), be_and(BE_NOT(c),e));
    if( RC_add_redundant_terms ) {
	res = BE_Or(res, be_and(t,e));
    }
    return( res );
}

bexpr
BE_Or(bexpr f1, bexpr f2)
{
    return( BE_NOT(be_and(BE_NOT(f1), BE_NOT(f2))) );
}

bexpr
BE_Xor(bexpr f1, bexpr f2)
{
    return( BE_Or(be_and(BE_NOT(f1), f2), be_and(f1, BE_NOT(f2))) );
}

bexpr
BE_Xnor(bexpr f1, bexpr f2)
{
    return( BE_Or(be_and(BE_NOT(f1), BE_NOT(f2)), be_and(f1, f2)) );
}

// Fast test if f1 != f2.
// Uses the bexpr signatures to avoid a full SAT computation.
bool
BE_NEQ(bexpr f1, bexpr f2)
{
    if( f1 == f2 ) return FALSE;
    if( f1 == BE_NOT(f2) ) return TRUE;
    bexpr p1 = BE_POS(f1);
    bool  n1 = BE_IS_NEG(p1);
    bexpr p2 = BE_POS(f2);
    bool  n2 = BE_IS_NEG(p2);
    bool psame = be_sig_eq((pointer) p1, (pointer) p2);
    if( (n1 == n2) && !psame ) return TRUE;
    return FALSE;
//    return( !check_for_satisfaction_with_SAT(BE_Xor(f1,f2)) );
}

bool
BE_Equal(bexpr f1, bexpr f2)
{
    return( f1 == f2 );
}

/* BE_HL_Print -- Print ternary function given in dual-rail encoding */
void
BE_HL_Print(odests fp, bexpr H, bexpr L)
{
    bexpr o, x;
    if( check_for_satisfaction_with_SAT(BE_And(BE_Not(H), BE_Not(L))) ) { 
	    FP(fp, "***** OVERCONSTRAINED VALUE *****");
	    return;
    }
    o = BE_And(H, BE_Not(L));
    x = BE_And(H, L);
    if( !check_for_satisfaction_with_SAT(o) ) {
	if( !check_for_satisfaction_with_SAT(x) ) {
	    FP(fp, "0");
	    return;
	}
	FP(fp, "X");
	if( !check_for_satisfaction_with_SAT(BE_Not(x)) )
	    return;
	FP(fp, "(");
	BE_Print(fp, x);
	FP(fp, ")");
    } else if( !check_for_satisfaction_with_SAT(BE_Not(o)) )
	FP(fp, "1");
    else {
	BE_Print(fp, o);
	if( !check_for_satisfaction_with_SAT(x) )
	    return;
	FP(fp, " + X");
	if( !check_for_satisfaction_with_SAT(BE_Not(x)) )
	    return;
	FP(fp, "(");
	BE_Print(fp, x);
	FP(fp, ")");
    }
}

/* BE_Print -- Print Boolean bexpr as an expression tree */
void
BE_Print(odests fp, bexpr f)
{
    if( BE_IS_FALSE(f) ) {
	FP(fp, "bF");
	return;
    }
    if( BE_IS_TRUE(f) ) {
	FP(fp, "bT");
	return;
    }
    be_print(fp, f, RCmax_bexpr_print_depth);
}

void
BEP(bexpr f)
{
    BE_Print(err_fp, f);
}


bool
Save_bexprs(string filename, buffer *roots)
{
    FILE *fp;
    if( (fp = fopen(filename, "w")) == NULL ) {
	Fail_pr("Cannot open %s for writing bexprs", filename);
	return( FALSE );
    }
    fprintf(fp, "SAVED_BEXPRS %d\n", COUNT_BUF(roots));

    create_hash(&bexpr_save_tbl, 100, be_hash, be_eq);
    bexpr_save_cnt = 2;	    // 0=FALSE, 1=TRUE, rest are expressions
    // Now save the bexpr body
    buffer res_buf;
    new_buf(&res_buf, COUNT_BUF(roots), sizeof(int));
    bexpr *bp;
    FOR_BUF(roots, bexpr, bp) {
	int r = be_save(fp, *bp);
	push_buf(&res_buf, &r);
    }
    int *ip;
    FOR_BUF(&res_buf, int, ip) {
	fprintf(fp, "r %+d\n", *ip);
    }
    fclose(fp);
    free_buf(&res_buf);
    dispose_hash(&bexpr_save_tbl, NULLFCN);
    return( TRUE );
}

bool
Load_bexprs(string filename, buffer *results)
{
    FILE *fp;
    if( (fp = fopen(filename, "r")) == NULL ) {
	Fail_pr("Cannot open %s for reading bexprs", filename);
	return( FALSE );
    }
    int cnt;
    if( fscanf(fp, "SAVED_BEXPRS %d\n", &cnt) != 1 ) {
	fclose(fp);
	Fail_pr("Illegal file format around line 1 in %s", filename);
        return 1;
    }
    buffer work_buf;
    new_buf(&work_buf, 1000, sizeof(bexpr));
    bexpr f = BE_Zero();
    push_buf(&work_buf, &f);
    f = BE_One();
    push_buf(&work_buf, &f);
    int line_cnt = 2;
    while( !feof(fp) ) {
	int c = fgetc(fp);
	ungetc(c, fp);
	switch( c ) {
	    case 'v':
	    {
		if( fscanf(fp, "v %s\n", buf) != 1 )
		    goto parse_error;
		line_cnt++;
		string name = wastrsave(stringsp, buf);
		bexpr b = BE_Var(name);
		push_buf(&work_buf, (pointer) &b);
		break;
	    }
	    case 'a':
	    {
		char sl, sr;
		int  pl, pr;
		if( fscanf(fp, "a %c%d %c%d\n",&sl,&pl,&sr,&pr) != 4 )
		    goto parse_error;
		line_cnt++;
		bexpr bl = *((bexpr *) M_LOCATE_BUF(&work_buf, pl));
		bexpr bel = (sl == '-')? BE_NOT(bl) : bl;
		bexpr br = *((bexpr *) M_LOCATE_BUF(&work_buf, pr));
		bexpr ber = (sr == '-')? BE_NOT(br) : br;
		bexpr b = BE_And(bel, ber);
		push_buf(&work_buf, &b);
		break;
	    }
	    case 'r':
	    {
		char sr;
		int pr;
		if( fscanf(fp, "r %c%d\n",&sr,&pr) != 2 )
		    goto parse_error;
		line_cnt++;
		bexpr br = *((bexpr *) M_LOCATE_BUF(&work_buf, pr));
		bexpr ber = (sr == '-')? BE_NOT(br) : br;
		push_buf(results, &ber);
		break;
	    }
	    default:
		goto parse_error;
	}
    }
    fclose(fp);
    free_buf(&work_buf);
    return( TRUE );
 
  parse_error:
    free_buf(&work_buf);
    fclose(fp);
    Fail_pr("Illegal format in file %s around line %d", filename, line_cnt);
    return( FALSE );
}


int
SHA256_bexpr(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, bexpr f)
{
    int res;
    if( BE_IS_FALSE(f) ) return -1;
    if( BE_IS_TRUE(f) ) return 1;
    bool neg = BE_IS_NEG(f);
    bexpr bp = BE_POS(f);
    if( (res = PTR2INT(find_hash(g_tblp, bp))) != 0 ) {
        return (neg? -1*res : res);
    }
    res = *g_cntp;
    *g_cntp = res+1;
    res = neg? -1*res : res;
    insert_hash(g_tblp, bp, INT2PTR(res));
    if( BE_IS_VAR(bp) ) {
	SHA256_printf(sha, "%d=V %s\n", res, BE_GET_VAR(bp));
	return res;
    }
    int lres = SHA256_bexpr(g_cntp, g_tblp, sha, BE_GET_LEFT(bp));
    int rres = SHA256_bexpr(g_cntp, g_tblp, sha, BE_GET_RIGHT(bp));
    SHA256_printf(sha, "%d=& %s %d %d\n", res, lres, rres);
    return res;
}


/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
bT(g_ptr redex)
{
    MAKE_REDEX_BEXPR(redex, BE_One());
}

static void
bF(g_ptr redex)
{
    MAKE_REDEX_BEXPR(redex, BE_Zero());
}

static void
bvariable(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BEXPR(redex, BE_Var(GET_STRING(r)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bNOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BEXPR(redex, BE_Not(GET_BEXPR(r)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bAND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    if( BE_IS_FALSE(GET_BEXPR(arg1)) ) {
	MAKE_REDEX_BEXPR(redex, BE_FALSE);
    } else {
	g_ptr arg2 = reduce(GET_APPLY_RIGHT(redex), FALSE);
	MAKE_REDEX_BEXPR(redex, BE_And(GET_BEXPR(arg1),GET_BEXPR(arg2)));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    if( BE_IS_TRUE(GET_BEXPR(arg1)) ) {
	MAKE_REDEX_BEXPR(redex, BE_TRUE);
    } else {
	g_ptr arg2 = reduce(GET_APPLY_RIGHT(redex), FALSE);
	MAKE_REDEX_BEXPR(redex, BE_Or(GET_BEXPR(arg1),GET_BEXPR(arg2)));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bXOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BEXPR(redex, BE_Xor(GET_BEXPR(arg1),GET_BEXPR(arg2)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bXNOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BEXPR(redex, BE_Xnor(GET_BEXPR(arg1),GET_BEXPR(arg2)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bexpr2bdd(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);

    g_ptr sub, bf;
    EXTRACT_2_ARGS(redex, sub, bf);
    create_hash(&bexpr2bdd_sub_tbl, 100, str_hash, str_equ);
    while( !IS_NIL(sub) ) {
        g_ptr pair = GET_CONS_HD(sub);
        string v = GET_STRING(GET_CONS_HD(pair));
        g_ptr e = GET_CONS_TL(pair);
	insert_hash(&bexpr2bdd_sub_tbl, v, e);
        sub = GET_CONS_TL(sub);
    }
    MAKE_REDEX_BOOL(redex, be2bdd(GET_BEXPR(bf)));
    dispose_hash(&bexpr2bdd_sub_tbl, NULLFCN);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bsize(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr cur = r;
    bsize_cnt = 0;
    while( !IS_NIL(cur) ) {
	unmark(GET_BEXPR(GET_CONS_HD(cur)));
	cur = GET_CONS_TL(cur);
    }
    cur = r;
    while( !IS_NIL(cur) ) {
	be_size(GET_BEXPR(GET_CONS_HD(cur)));
	cur = GET_CONS_TL(cur);
    }
    cur = r;
    while( !IS_NIL(cur) ) {
	unmark(GET_BEXPR(GET_CONS_HD(cur)));
	cur = GET_CONS_TL(cur);
    }
    MAKE_REDEX_INT(redex, bsize_cnt);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bsubstitute(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    create_hash(&gl_subst_tbl, 100, str_hash, str_equ);
    while( !IS_NIL(arg1) ) {
	g_ptr pair = GET_CONS_HD(arg1);
	string from = GET_STRING(GET_CONS_HD(pair));
	bexpr to = GET_BEXPR(GET_CONS_TL(pair));
	if( find_hash(&gl_subst_tbl, (pointer) from) != NULL ) {
	    MAKE_REDEX_FAILURE(redex, 
		Fail_pr("Duplicate (%s) in subst. list for bsubstitute", from));
	    return;
	}
	insert_hash(&gl_subst_tbl, (pointer) from, (pointer) to);
	arg1 = GET_CONS_TL(arg1);
    }
    create_hash(&res_tbl, 100, be_hash, be_eq);
    g_ptr res = Gen_map(top_be_subst, arg2, FALSE);
    OVERWRITE(redex, res);
    dispose_hash(&gl_subst_tbl, NULLFCN);
    dispose_hash(&res_tbl, NULLFCN);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static unsigned int
bdd_hash(pointer np, unsigned int n)
{   
    return( ((lunint) np) % n );
}       
        
static bool
bdd_eq(pointer p1, pointer p2)
{       
    return( p1 == p2 );
}

static void
bool2bexpr(g_ptr redex)
{
    g_ptr fs;
    EXTRACT_1_ARG(redex, fs);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    create_hash(&bdd2bexpr_tbl, 100, bdd_hash, bdd_eq);
    for(g_ptr cur = fs; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	formula f = GET_BOOL(GET_CONS_HD(cur));
	bexpr b = bdd2bexpr(f);
	APPEND1(tail, Make_BEXPR_leaf(b));
    }
    DEC_REF_CNT(fs);
    dispose_hash(&bdd2bexpr_tbl, NULLFCN);
}

static void
bexpr2str(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    if( (odests_fp = fmemopen(buf, 1024, "w")) == NULL ) {
        DIE("Should never happen");
    }
    bexpr f = GET_BEXPR(arg2);
    if( BE_IS_FALSE(f) ) {
	FP(FILE_fp, "bF");
    } else if( BE_IS_TRUE(f) ) {
	FP(FILE_fp, "bT");
    } else
	be_print(FILE_fp, f, GET_INT(arg1));
    fclose(odests_fp);
    odests_fp = NULL;
    MAKE_REDEX_STRING(redex, wastrsave(stringsp, buf));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


static void
bdepends(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    create_hash(&depend_tbl, 100, ptr_hash, ptr_equ);
    Gen_map(be_record, arg1, TRUE);
    scan_hash(&depend_tbl, scan_unmark_be);
    new_buf(&bvar_buf, 100, sizeof(string));
    scan_hash(&depend_tbl, scan_be_depend);
    scan_hash(&depend_tbl, scan_unmark_be);
    dispose_hash(&depend_tbl, NULLFCN);
    SET_TYPE(redex, CONS_ND);
    SET_CONS_HD(redex, NULL);
    SET_CONS_TL(redex, NULL);
    g_ptr tail = redex;
    MAKE_FORCED(redex);
    qsort(START_BUF(&bvar_buf), COUNT_BUF(&bvar_buf), sizeof(string), vn_cmp);
    string *sp;
    FOR_BUF(&bvar_buf, string, sp) {
	SET_CONS_HD(tail, Make_STRING_leaf(*sp));
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
	MAKE_FORCED(tail);
    }
    free_buf(&bvar_buf);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bexpr_is_unsat(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr gb, gt;
    EXTRACT_2_ARGS(redex, gb, gt);
    bexpr be = GET_BEXPR(gb);
    if( BE_IS_FALSE(be) ) {
	MAKE_REDEX_BOOL(redex, B_One());
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( BE_IS_TRUE(be) ) {
	MAKE_REDEX_BOOL(redex, B_Zero());
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    bool neg = BE_IS_NEG(be);
    bexpr b = BE_POS(be);
    int a = neg? -1*(b->sat_idx) : b->sat_idx;
    int time_limit = GET_INT(gt);
    int res = Find_model(&a, 1, time_limit);
    if( res <= 0 ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bget_model(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    buffer assumptions;
    new_buf(&assumptions, 100, sizeof(int));
    g_ptr cur = arg1;
    while( !IS_NIL(cur) ) {
	bexpr be = GET_BEXPR(GET_CONS_HD(cur));
	if( BE_IS_FALSE(be) ) {
	    SET_TYPE(redex, CONS_ND);
	    SET_CONS_HD(redex, NULL);
	    SET_CONS_TL(redex, NULL);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	// Ignore constant true assumptions
	if( !BE_IS_TRUE(be) ) {
	    bool neg = BE_IS_NEG(be);
	    bexpr b = BE_POS(be);
	    int a = neg? -1*(b->sat_idx) : b->sat_idx;
	    push_buf(&assumptions, (pointer) &a);
	}
	cur = GET_CONS_TL(cur);
    }
    if( COUNT_BUF(&assumptions) == 0 ) {
	// Dummy model since every model satisfies TRUE
	SET_TYPE(redex, CONS_ND);
	SET_CONS_HD(redex,
		    Make_CONS_ND(
			Make_STRING_leaf(wastrsave(stringsp,"")),
			Make_BEXPR_leaf(BE_One())));
	SET_CONS_TL(redex, Make_NIL());
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }

    int time_limit = GET_INT(arg2);
    int res = Find_model((int *) START_BUF(&assumptions),
			 COUNT_BUF(&assumptions), time_limit);
    free_buf(&assumptions);
    switch( res ) {
	case -1:
	case 0: {
	    SET_TYPE(redex, CONS_ND);
	    SET_CONS_HD(redex, NULL);
	    SET_CONS_TL(redex, NULL);
	    break;
	}
	case 1: {
	    g_ptr cur = arg1;
	    while( !IS_NIL(cur) ) {
		bexpr be = GET_BEXPR(GET_CONS_HD(cur));
		unmark(be);
		cur = GET_CONS_TL(cur);
	    }
	    new_buf(&bvar_sat_buf, 100, sizeof(bexpr));
	    create_hash(&bvar_sat_tbl, 100, str_hash, str_equ);
	    cur = arg1;
	    while( !IS_NIL(cur) ) {
		bexpr be = GET_BEXPR(GET_CONS_HD(cur));
		be_sat_depend(be);
		cur = GET_CONS_TL(cur);
	    }
	    cur = arg1;
	    while( !IS_NIL(cur) ) {
		bexpr be = GET_BEXPR(GET_CONS_HD(cur));
		unmark(be);
		cur = GET_CONS_TL(cur);
	    }
	    SET_TYPE(redex, CONS_ND);
	    SET_CONS_HD(redex, NULL);
	    SET_CONS_TL(redex, NULL);
	    g_ptr tail = redex;
	    bexpr *bep;
	    qsort(START_BUF(&bvar_sat_buf), COUNT_BUF(&bvar_sat_buf),
		  sizeof(bexpr), be_subs_cmp);
	    FOR_BUF(&bvar_sat_buf, bexpr, bep) {
		string name = BE_GET_VAR(*bep);
		int v = Get_Model_Value((*bep)->sat_idx);
		if( v == 0 ) {
		    SET_CONS_HD(tail, Make_CONS_ND(Make_STRING_leaf(name),
						   Make_BEXPR_leaf(BE_Zero())));
		    SET_CONS_TL(tail, Make_NIL());
		    tail = GET_CONS_TL(tail);
		} else if ( v == 1 ) {
		    SET_CONS_HD(tail, Make_CONS_ND(Make_STRING_leaf(name),
						   Make_BEXPR_leaf(BE_One())));
		    SET_CONS_TL(tail, Make_NIL());
		    tail = GET_CONS_TL(tail);
		}
	    }
	    free_buf(&bvar_sat_buf);
	    dispose_hash(&bvar_sat_tbl, NULLFCN);
	    break;
	}
	case 2: {
	    MAKE_REDEX_FAILURE(
		redex,
	        Fail_pr("Timeout/Out of resources in bget_model"));
	    return;
	}
	default:
	    DIE("Should never happen");
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bexpr_save(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    buffer roots;
    new_buf(&roots, 100, sizeof(bexpr));
    g_ptr cur = arg2;
    while( !IS_NIL(cur) ) {
	bexpr be = GET_BEXPR(GET_CONS_HD(cur));
	push_buf(&roots, &be);
	cur = GET_CONS_TL(cur);
    }
    bool res = Save_bexprs(GET_STRING(arg1), &roots);
    free_buf(&roots);
    if( !res ) {
	MAKE_REDEX_FAILURE(redex, wastrsave(stringsp, FailBuf));
	return;
    }
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bexpr_load(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    buffer results;
    new_buf(&results, 100, sizeof(bexpr));
    if( !Load_bexprs(GET_STRING(arg1), &results) ) {
	free_buf(&results);
	MAKE_REDEX_FAILURE(redex, wastrsave(stringsp, FailBuf));
	return;
    }
    SET_TYPE(redex, CONS_ND);
    SET_CONS_HD(redex, NULL);
    SET_CONS_TL(redex, NULL);
    g_ptr tail = redex;
    bexpr *bep;
    FOR_BUF(&results, bexpr, bep) {
	SET_CONS_HD(tail, Make_BEXPR_leaf(*bep));
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    free_buf(&results);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bget_type(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    bexpr be = GET_BEXPR(arg1);
    if( BE_IS_FALSE(be) ) {
	MAKE_REDEX_STRING(redex, s_const_0);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( BE_IS_TRUE(be) ) {
	MAKE_REDEX_STRING(redex, s_const_1);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    bool neg = BE_IS_NEG(be);
    bexpr  b = BE_POS(be);
    switch( b->type ) {
	case BE_VAR:
	    MAKE_REDEX_STRING(redex, neg? s_not_variable : s_variable);
	    break;
	case BE_AND:
	    MAKE_REDEX_STRING(redex, neg? s_not_and : s_and);
	    break;
	case BE_FREE:
	    DIE("Freed bexpr node reached in bget_type");
	default:
	    DIE("Illegal type in bget_type");
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bget_arguments(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    bexpr be = GET_BEXPR(arg1);
    if( BE_IS_CONSTANT(be) ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("bget_arguments on a constant bexpr"));
	return;
    }
    bexpr  b = BE_POS(be);
    if( BE_IS_VAR(b) ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("bget_arguments on a bvariable"));
	return;
    }
    SET_TYPE(redex, CONS_ND);
    SET_CONS_HD(redex, Make_BEXPR_leaf(BE_GET_LEFT(b)));
    SET_CONS_TL(redex, Make_BEXPR_leaf(BE_GET_RIGHT(b)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bget_variable(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    bexpr be = GET_BEXPR(arg1);
    if( BE_IS_CONSTANT(be) ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("bget_variable on a constant bexpr"));
	return;
    }
    bexpr  b = BE_POS(be);
    if( BE_IS_AND(b) ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("bget_variable on an expression "));
	return;
    }
    MAKE_REDEX_STRING(redex, BE_GET_VAR(b));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bget_literal(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    bexpr be = GET_BEXPR(arg1);
    if( BE_IS_CONSTANT(be) ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("bget_sat_literal on a constant bexpr"));
	return;
    }
    bool neg = BE_IS_NEG(be);
    bexpr  b = BE_POS(be);
    MAKE_REDEX_INT(redex, neg? -1*b->sat_idx : b->sat_idx);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
gen_bNOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    g_ptr res = Gen_map(g_be_not, arg1, FALSE);
    OVERWRITE(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
gen_bOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    g_ptr res = Gen_map2(s_gen_bOR, g_be_or, arg1, arg2, FALSE);
    OVERWRITE(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
gen_bAND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    g_ptr res = Gen_map2(s_gen_bAND, g_be_and, arg1, arg2, FALSE);
    OVERWRITE(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bITE(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr ll = GET_APPLY_LEFT(l);
    g_ptr arg1 = GET_APPLY_RIGHT(ll);
    current_bexpr_cond = GET_BEXPR(arg1);
    if( BE_IS_TRUE(current_bexpr_cond) ) {
	g_ptr arg2 = reduce(GET_APPLY_RIGHT(l), FALSE);
	OVERWRITE(redex, arg2);
    } else 
    if( BE_IS_FALSE(current_bexpr_cond) ) {
	g_ptr arg3 = reduce(GET_APPLY_RIGHT(redex), FALSE);
	OVERWRITE(redex, arg3);
    } else {
	g_ptr arg2 = force(GET_APPLY_RIGHT(l), FALSE);
	g_ptr arg3 = force(GET_APPLY_RIGHT(redex), FALSE);
	g_ptr res = Gen_map2(s_bITE, g_be_ite, arg2, arg3, FALSE);
	OVERWRITE(redex, res);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bEqual(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    current_bexpr_eq = BE_TRUE;
    g_ptr res = Gen_map2(s_bEqual, g_be_eq, arg1, arg2, TRUE);
    if( res == NULL ) {
	MAKE_REDEX_BEXPR(redex, current_bexpr_eq);
    } else {
	OVERWRITE(redex, res);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bev2num(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    arbi_T cur = Arbi_FromInt(0);
    while( !IS_NIL(arg1) ) {
	bexpr be = GET_BEXPR(GET_CONS_HD(arg1));
	cur = Arbi_mlt(cur, Arbi_FromInt(2));
	if( be == BE_One() ) {
	    cur = Arbi_add(cur, Arbi_FromInt(1));
	} else if( be != BE_Zero() ) {
	    MAKE_REDEX_FAILURE(redex,Fail_pr("bev2num on symbolic vector"));
	    return;
	}
	arg1 = GET_CONS_TL(arg1);
    }
    MAKE_REDEX_AINT(redex, cur);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
BE_Init()
{
    aligned_new_mgr(&bexpr_rec_mgr, sizeof(bexpr_rec), 8);
    create_hash(&bexpr_uniq_tbl, 100, be_hash, be_eq);
    create_hash(&bexpr_sig_tbl, 100, be_sig_hash, be_sig_eq);
    free_list = NULL;
    // Create constants BE_TRUE and BE_FALSE
    BE_TRUE = get_be_node();
    BE_TRUE->type = BE_ONE;
    BE_TRUE->mark = 0;
    BE_TRUE->height = 0;
    BE_TRUE->sat_idx = 0;
    BE_TRUE->l.be = 0;
    BE_TRUE->next = 0;
    BE_TRUE->r = 0;
    BE_TRUE->has_bdd = 0;
    for(int i = 0; i < NBR_SIGNATURES; i++) {
	BE_TRUE->sig[i] = ~0;
    }
    BE_TRUE->same_sig_list = 0;
    BE_FALSE = BE_NOT(BE_TRUE);

    s_const_0 = wastrsave(stringsp, "const_0");
    s_const_1 = wastrsave(stringsp, "const_1");
    s_variable = wastrsave(stringsp, "variable");
    s_not_variable = wastrsave(stringsp, "not-variable");
    s_and = wastrsave(stringsp, "and");
    s_not_and = wastrsave(stringsp, "not-and");
    s_gen_bOR = wastrsave(stringsp, "gen_bOR");
    s_gen_bAND = wastrsave(stringsp, "gen_bAND");
    s_bITE = wastrsave(stringsp, "bITE");
    s_bEqual = wastrsave(stringsp, "bEqual");
}

static void
bexpr_signature(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    bexpr v = GET_BEXPR(r);
    int i = PTR2INT(v);
    MAKE_REDEX_INT(redex, i);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
Bexpr_Install_Functions()
{
    typeExp_ptr tv1 = GLnew_tVar();

    Add_ExtAPI_Function("bT", "", FALSE, GLmake_bexpr(), bT);

    Add_ExtAPI_Function("bF", "", FALSE, GLmake_bexpr(), bF);

    Add_ExtAPI_Function("bvariable", "1", FALSE,
			GLmake_arrow(GLmake_string(),GLmake_bexpr()),
			bvariable);

    Add_ExtAPI_Function("bexpr_signature", "1", FALSE,
			GLmake_arrow(GLmake_bexpr(),GLmake_int()),
			bexpr_signature);

    Add_ExtAPI_Function("bexpr_is_unsat", "11", FALSE,
			GLmake_arrow(
			    GLmake_bexpr(),
			    GLmake_arrow(GLmake_int(), GLmake_bool())),
			bexpr_is_unsat);

    Add_ExtAPI_Function("bNOT", "1", FALSE,
			GLmake_arrow(GLmake_bexpr(),GLmake_bexpr()),
			bNOT);

    Add_ExtAPI_Function("bAND", "1-", FALSE,
			GLmake_arrow(GLmake_bexpr(),
				      GLmake_arrow(GLmake_bexpr(),
						   GLmake_bexpr())),
			bAND);
    Insert_infix("bAND", 4);

    Add_ExtAPI_Function("bOR", "1-", FALSE,
			GLmake_arrow(GLmake_bexpr(),
				      GLmake_arrow(GLmake_bexpr(),
						   GLmake_bexpr())),
			bOR);
    Insert_infix("bOR", 3);

    Add_ExtAPI_Function("bXOR", "11", FALSE,
			GLmake_arrow(GLmake_bexpr(),
				      GLmake_arrow(GLmake_bexpr(),
						   GLmake_bexpr())),
			bXOR);
    Insert_infix("bXOR", 4);

    Add_ExtAPI_Function("bXNOR", "11", FALSE,
			GLmake_arrow(GLmake_bexpr(),
				      GLmake_arrow(GLmake_bexpr(),
						   GLmake_bexpr())),
			bXNOR);
    Insert_infix("bXNOR", 4);

    Add_ExtAPI_Function("base_bexpr2bdd", "11", FALSE,
			GLmake_arrow(
			    GLmake_list(
			      GLmake_tuple(GLmake_string(),GLmake_bool())),
			      GLmake_arrow( GLmake_bexpr(), GLmake_bool())),
			bexpr2bdd);

    Add_ExtAPI_Function("bsize", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_bexpr()), GLmake_int()),
			bsize);

    Add_ExtAPI_Function("bsubstitute", "11", FALSE,
	    GLmake_arrow(
		GLmake_list(GLmake_tuple(GLmake_string(),GLmake_bexpr())),
		GLmake_arrow(tv1, tv1)),
	    bsubstitute);

    Add_ExtAPI_Function("bexpr2str", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_bexpr(),
						  GLmake_string())),
			bexpr2str);

    Add_ExtAPI_Function("bdepends", "1", FALSE,
			GLmake_arrow(tv1, GLmake_list(GLmake_string())),
			bdepends);

    Add_ExtAPI_Function("bget_model", "11", FALSE,
			GLmake_arrow(
			    GLmake_list(GLmake_bexpr()),
			    GLmake_arrow(
				GLmake_int(),
				 GLmake_list(
				    GLmake_tuple(GLmake_string(),
						 GLmake_bexpr())))),
			bget_model);

    Add_ExtAPI_Function("bexpr_save", "11", FALSE,
	    GLmake_arrow(
		GLmake_string(),
		GLmake_arrow(GLmake_list(GLmake_bexpr()), GLmake_void())),
			bexpr_save);

    Add_ExtAPI_Function("bexpr_load", "1", FALSE,
	    GLmake_arrow(GLmake_string(), GLmake_list(GLmake_bexpr())),
			bexpr_load);

    Add_ExtAPI_Function("bget_type", "1", FALSE,
	    GLmake_arrow(GLmake_bexpr(), GLmake_string()),
			bget_type);

    Add_ExtAPI_Function("bget_arguments", "1", FALSE,
	    GLmake_arrow(GLmake_bexpr(),
			 GLmake_tuple(GLmake_bexpr(), GLmake_bexpr())),
			bget_arguments);

    Add_ExtAPI_Function("bget_variable", "1", FALSE,
	    GLmake_arrow(GLmake_bexpr(), GLmake_string()),
			bget_variable);

    Add_ExtAPI_Function("bget_sat_literal", "1", FALSE,
	    GLmake_arrow(GLmake_bexpr(), GLmake_int()),
			bget_literal);

    Add_ExtAPI_Function("bITE", "1--", FALSE,
	    GLmake_arrow( GLmake_bexpr(),
			  GLmake_arrow(tv1, GLmake_arrow(tv1, tv1))),
			bITE);

    Add_ExtAPI_Function("gen_bNOT", "1", FALSE, GLmake_arrow(tv1,tv1),gen_bNOT);

    Add_ExtAPI_Function("gen_bOR", "11", FALSE,
			GLmake_arrow(tv1, GLmake_arrow(tv1, tv1)),
			gen_bOR);

    Add_ExtAPI_Function("gen_bAND", "11", FALSE,
			GLmake_arrow(tv1, GLmake_arrow(tv1, tv1)),
			gen_bAND);

    Add_ExtAPI_Function("bEqual", "11", FALSE,
			GLmake_arrow(tv1, GLmake_arrow(tv1, GLmake_bexpr())),
			bEqual);

    Add_ExtAPI_Function("bev2num", "1", FALSE,
                        GLmake_arrow(GLmake_list(GLmake_bexpr()), GLmake_int()),
                        bev2num);

    Add_ExtAPI_Function("bool2bexpr", "1", FALSE,
                        GLmake_arrow(GLmake_list(GLmake_bool()),
				     GLmake_list(GLmake_bexpr())),
                        bool2bexpr);


}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static unsigned int
be_hash(pointer p, unsigned int n)
{
    bexpr be = (bexpr) p;
    switch( be->type ) {
	case BE_ONE:
	    return 0;
	case BE_FREE:
	    DIE("Freed bexpr node reached in be_hash");
	case BE_VAR:
	    return( str_hash(BE_GET_VAR(be), n) );
	case BE_AND:
	    return( (241*((ui) BE_GET_LEFT(be)) +
		     503*((ui) BE_GET_RIGHT(be))) % n );
	default:
	    DIE("Illegal type in be_hash");
    }
}

static bool
be_eq(pointer p1, pointer p2)
{
    bexpr b1 = (bexpr) p1;
    bexpr b2 = (bexpr) p2;
    if( BE_GET_TYPE(b1) != BE_GET_TYPE(b2) ) return FALSE;
    if( BE_GET_TYPE(b1) == BE_ONE ) return TRUE;
    if( BE_IS_VAR(b1) ) {
	return( STREQ(BE_GET_VAR(b1), BE_GET_VAR(b2)) );
    } else {
	if( BE_GET_LEFT(b1) != BE_GET_LEFT(b2) ) return FALSE;
	if( BE_GET_RIGHT(b1) != BE_GET_RIGHT(b2) ) return FALSE;
	return TRUE;
    }
}

static unsigned int
be_sig_hash(pointer p, unsigned int n)
{
    bexpr be = (bexpr) p;
    ui res = 0;
    for(int i = 0; i < NBR_SIGNATURES; i++) {
	ui x = be->sig[i];
	x ^=  x >> 12;
	x ^=  x << 25;
	x ^=  x >> 27;
	res ^= x;
    }
    return( res % n );
}

static bool
be_sig_eq(pointer p1, pointer p2)
{
    bexpr b1 = (bexpr) p1;
    bexpr b2 = (bexpr) p2;
    for(int i = 0; i < NBR_SIGNATURES; i++) {
	if( b1->sig[i] != b2->sig[i] ) return FALSE;
    }
    return TRUE;
}


static void
mark(bexpr be)
{
    be = BE_POS(be);
    if( BE_GET_MARK(be) == 1 ) return;
    BE_SET_MARK(be, 1);
    if( be->has_bdd ) { B_Mark(be->bdd); }
    if( BE_GET_TYPE(be) != BE_AND ) return;
    mark( BE_GET_LEFT(be) );
    mark( BE_GET_RIGHT(be) );
}

static void
unmark(bexpr be)
{
    be = BE_POS(be);
    if( BE_GET_MARK(be) == 0 ) return;
    BE_SET_MARK(be, 0);
    if( BE_GET_TYPE(be) != BE_AND ) return;
    unmark( BE_GET_LEFT(be) );
    unmark( BE_GET_RIGHT(be) );
}

static formula
be2bdd(bexpr be)
{
    if( BE_IS_FALSE(be) ) return( B_Zero() );
    if( BE_IS_TRUE(be) ) return( B_One() );
    bool neg = BE_IS_NEG(be);
    bexpr b = BE_POS(be);
    if( b->has_bdd ) {
	return( neg? B_Not(b->bdd) : b->bdd );
    }
    formula res;
    if( BE_IS_VAR(b) )  {
	string v = BE_GET_VAR(b);
	g_ptr old;
	if( (old = find_hash(&bexpr2bdd_sub_tbl, v)) != NULL ) {
	    res = GET_BOOL(old);
	} else {
	    res = B_Var(BE_GET_VAR(b));
	}
    } else {
	res = B_And(be2bdd(BE_GET_LEFT(b)), be2bdd(BE_GET_RIGHT(b)));
	PUSH_BDD_GC(res);
	if( Do_gc_asap ) Garbage_collect();
	POP_BDD_GC(1);
    }
    b->bdd = res;
    b->has_bdd = 1;
    return( neg? B_Not(res) : res );
}

static bexpr
get_be_node()
{
    bexpr bp;
    if( free_list != NULL ) {
	bp = free_list;
	free_list = free_list->next;
    } else {
	bp = (bexpr) new_rec(&bexpr_rec_mgr);
    }
    bp->type = BE_FREE;
    bp->mark = 0;
    bp->has_bdd = 0;
    bp->height = 0;
    bp->sat_idx = 0;
    bp->next = 0;
    bp->l.be = 0;
    bp->r = 0;
    bp->same_sig_list = NULL;
    return( bp );
}

static bexpr
be_find_insert(bexpr p)
{
    bexpr res;
    if( (res = (bexpr) find_hash(&bexpr_uniq_tbl, (pointer) p)) != NULL ) {
	return res;
    }
    switch( p->type ) {
	case BE_VAR:
	    {
		string name = p->l.name;
		res = get_be_node();
		res->type = p->type;
		res->l.name = name;
		res->sat_idx = SAT_Add_Var();
		res->height = p->height;
		string s = name;
		uint32_t seed = 1;
		while( *s ) {
		    seed = 137*seed+((uint32_t) *s);
		    s++;
		}
		if( seed == 0 ) {
		    seed = 34624351;	// Random non-zero
		}
		for(int i = 0; i < NBR_SIGNATURES; i++) {
		    res->sig[i] = mk_prandom(&seed);
		}
		break;
	    }
	case BE_AND:
	    {
		// Compute signature for node
		bexpr l = p->l.be;
		bexpr r = p->r;
		bexpr pl = BE_POS(l);
		bool  nl = BE_IS_NEG(l);
		bexpr pr = BE_POS(r);
		bool  nr = BE_IS_NEG(r);
		int il = nl? -1*(pl->sat_idx) : pl->sat_idx;
		int ir = nr? -1*(pr->sat_idx) : pr->sat_idx;
		uint32_t seed = nl? (nr? 2352 : -32451):(nr? -722521 : 123215);
		for(int i = 0; i < NBR_SIGNATURES; i++) {
		    p->sig[i] = (ui) ((long int) mk_prandom(&seed) +
				      3259*((long int) pl->sig[i]) +
				      (long int) pr->sig[i]);
		}
		// Determine if bexpr already has been created for this function
		bexpr cand = (bexpr) find_hash(&bexpr_sig_tbl, (pointer) p);
		bexpr last = cand;
		while( cand != NULL ) {
		    if( check_for_eq_with_SAT(l, r, cand) ) {
			return cand;
		    }
		    last = cand;
		    cand = cand->same_sig_list;
		}
		// Insert a new node
		res = get_be_node();
		res->type = p->type;
		res->height = p->height;
		res->same_sig_list = NULL;
		for(int i = 0; i < NBR_SIGNATURES; i++) res->sig[i] = p->sig[i];
		if( last != NULL ) {
		    last->same_sig_list = res;
		} else {
		    insert_hash(&bexpr_sig_tbl, (pointer) res, (pointer) res);
		}
		res->sat_idx = SAT_Add_AND_Clauses(il,ir);
		res->l.be = l;
		res->r    = r;
		break;
	    }
	default:
	    DIE("Illegal type encountered in be_find_insert");
    }
    insert_hash(&bexpr_uniq_tbl, (pointer) res, (pointer) res);
    return( res );
}

static int
max(int i, int j)
{
    return( (i>j)? i : j );
}

static bexpr
be_and(bexpr f1, bexpr f2)
{
    if( BE_IS_FALSE(f1) || BE_IS_FALSE(f2) ) {
	return BE_FALSE;
    }
    if( BE_IS_TRUE(f1) ) {
	return( f2 );
    }
    if( BE_IS_TRUE(f2) ) {
	return( f1 );
    }
    if( f1 == f2 ) {
	return( f1 );
    }
    if( f1 == BE_NOT(f2) ) {
	return( BE_FALSE );
    }
    if( RC_reorder_bexpr_ands ) {
	// Make the right-spine as large as possible
	if( f1->height > f2->height ) {
	    bexpr tmp = f1;
	    f1 = f2;
	    f2 = tmp;
	}
    }
    bexpr_rec b;
    b.type = BE_AND;
    b.l.be = f1;
    b.r = f2;
    b.height = max(f1->height, f2->height) + 1;
    return( be_find_insert(&b) );
}

static void
be_print(odests fp, bexpr be, int max_to_print)
{
    if( max_to_print < 0 ) {
	FP(fp, "...");
	return;
    }
    bool neg = BE_IS_NEG(be);
    bexpr b = BE_POS(be);
    if( BE_IS_VAR(b) )  {
	FP(fp, "%s%s", BE_GET_VAR(b), neg? "'" : "");
	return;
    } else {
	FP(fp, "(");
	be_print(fp, BE_GET_LEFT(b), max_to_print-1);
	FP(fp, " & ");
	be_print(fp, BE_GET_RIGHT(b), max_to_print-1);
	FP(fp, ")%s", neg? "'" : "");
    }
}

static void
be_size(bexpr be)
{
    bexpr b = BE_POS(be);
    if( BE_GET_MARK(b) == 1 ) return;
    BE_SET_MARK(b,1);
    if( BE_IS_AND(b) ) {
	bsize_cnt++;
	be_size(BE_GET_LEFT(b));
	be_size(BE_GET_RIGHT(b));
    }
}

static g_ptr
top_be_subst(g_ptr np)
{
    ASSERT(IS_LEAF(np));
    if( !IS_BEXPR(np) ) {
	INC_REFCNT(np);
	return np;
    }
    return( Make_BEXPR_leaf(be_subst(GET_BEXPR(np))) );
}

static bexpr
be_subst(bexpr be)
{
    bexpr res;
    if( BE_IS_CONSTANT(be) ) return be;
    bool neg = BE_IS_NEG(be);
    bexpr b = BE_POS(be);
    if( (res = (bexpr) find_hash(&res_tbl, (pointer) b)) != NULL ) {
	return( neg? BE_NOT(res) : res );
    }
    if( BE_IS_VAR(b) )  {
	res = (bexpr) find_hash(&gl_subst_tbl, (pointer) BE_GET_VAR(b));
	if( res != NULL ) { 
	    res = neg? BE_NOT(res) : res;
	} else {
	    res = be;
	}
	return res;
    } else {
	bexpr old_l = BE_GET_LEFT(b);
	bexpr new_l = be_subst(old_l);
	bexpr old_r = BE_GET_RIGHT(b);
	bexpr new_r = be_subst(old_r);
	if( old_l == new_l && old_r == new_r ) {
	    res = b;
	} else {
	    res = be_and(new_l, new_r);
	}
	insert_hash(&res_tbl, (pointer) b, (pointer) res);
	res = neg? BE_NOT(res) : res;
	return res;
    }
}

static g_ptr
be_record(g_ptr np)
{
    if( GET_LEAF_TYPE(np) != BEXPR ) return np;
    bexpr be = GET_BEXPR(np);
    if( BE_IS_CONSTANT(be) ) return np;
    bexpr b = BE_POS(be);
    if( find_hash(&depend_tbl, (pointer) b) != NULL ) {
	return np;
    }
    insert_hash(&depend_tbl, (pointer) b, (pointer) b);
    return np;
}

static void
scan_unmark_be(pointer key, pointer data)
{
    (void) data;
    bexpr b = (bexpr) key;
    unmark(b);
}

static void
scan_be_depend(pointer key, pointer data)
{
    (void) data;
    bexpr b = (bexpr) key;
    be_depend(b);
}

static void
be_depend(bexpr be)
{
    if( BE_IS_CONSTANT(be) ) return;
    bexpr b = BE_POS(be);
    if(BE_GET_MARK(b) == 1) return;
    BE_SET_MARK(b,1);
    if( BE_IS_VAR(b) )  {
	string name = BE_GET_VAR(b);
	push_buf(&bvar_buf, (pointer) &name);
	return;
    } else {
	be_depend(BE_GET_LEFT(b));
	be_depend(BE_GET_RIGHT(b));
	return;
    }
}

static int
be_subs_cmp(const void *p1, const void *p2)
{
    bexpr *bp1 = (bexpr *) p1;
    bexpr *bp2 = (bexpr *) p2;
    return( node_name_cmp(BE_GET_VAR(*bp1) ,BE_GET_VAR(*bp2)) );
}

static int
vn_cmp(const void *pi, const void *pj)
{
    string *i = (string *) pi;
    string *j = (string *) pj;
    return( strcmp(*i, *j) );
}

static void
be_sat_depend(bexpr be)
{
    if( BE_IS_CONSTANT(be) ) return;
    bexpr b = BE_POS(be);
    if(BE_GET_MARK(b) == 1) return;
    BE_SET_MARK(b,1);
    if( BE_IS_VAR(b) )  {
	string v = BE_GET_VAR(b);
	if( find_hash(&bvar_sat_tbl, (pointer) v) == NULL ) {
	    push_buf(&bvar_sat_buf, (pointer) &b);
	    insert_hash(&bvar_sat_tbl, (pointer) v, (pointer) v);
	}
	return;
    } else {
	be_sat_depend(BE_GET_LEFT(b));
	be_sat_depend(BE_GET_RIGHT(b));
	return;
    }
}

static bool
check_for_satisfaction_with_SAT(bexpr f)
{
    int assumptions[2];
    bool neg_f = BE_IS_NEG(f);
    bexpr p_f  = BE_POS(f);
    assumptions[0] = neg_f? -1*p_f->sat_idx : p_f->sat_idx;
    int res = Find_model(assumptions, 1, 5); 
    if( res <= 0 ) return TRUE;
    return FALSE;
}

static bool
check_for_eq_with_SAT(bexpr l, bexpr r,  bexpr c)
{
    bool neg_l = BE_IS_NEG(l);
    bexpr p_l  = BE_POS(l);
    bool neg_r = BE_IS_NEG(r);
    bexpr p_r  = BE_POS(r);
    bool neg_c = BE_IS_NEG(c);
    bexpr p_c  = BE_POS(c);
    int ll = neg_l? -1*p_l->sat_idx : p_l->sat_idx;
    int lr = neg_r? -1*p_r->sat_idx : p_r->sat_idx;
    int lc = neg_c? -1*p_c->sat_idx : p_c->sat_idx;
    return( SAT_and_is_same(ll, lr, lc) == 1 );
}


static int
be_save(FILE *fp, bexpr be)
{
    int res;
    if( BE_IS_FALSE(be) ) return 0;
    if( BE_IS_TRUE(be) ) return 1;
    bool neg = BE_IS_NEG(be);
    bexpr b = BE_POS(be);
    if( (res = PTR2INT(find_hash(&bexpr_save_tbl, b))) != 0 )
	return (neg? -1*res : res);
    switch( b->type ) {
	case BE_VAR:
	{
	    fprintf(fp, "v %s\n", BE_GET_VAR(b));
	    insert_hash(&bexpr_save_tbl, b, INT2PTR(bexpr_save_cnt));
	    res = bexpr_save_cnt;
	    bexpr_save_cnt++;
	    return( neg? -1*res : res );
	}
	case BE_AND:
	{
	    int il = be_save(fp, BE_GET_LEFT(b));
	    int ir = be_save(fp, BE_GET_RIGHT(b));
	    fprintf(fp, "a %+d %+d\n", il, ir);
	    insert_hash(&bexpr_save_tbl, b, INT2PTR(bexpr_save_cnt));
	    res = bexpr_save_cnt;
	    bexpr_save_cnt++;
	    return( neg? -1*res : res );
	}
	default:
	    DIE("Should not happen");
    }
}

static g_ptr
g_be_not(g_ptr np)
{
    ASSERT(IS_LEAF(np));
    if( !IS_BEXPR(np) ) {
	INC_REFCNT(np);
	return np;
    }
    return( Make_BEXPR_leaf(BE_NOT(GET_BEXPR(np))) );
}

static g_ptr
g_be_or(g_ptr l, g_ptr r)
{
    g_ptr res;
    // Note: Gen_map2 will only call g_be_or with the same leaf type!
    ASSERT(IS_LEAF(l));
    if( !IS_BEXPR(l) ) {
	if( Is_equal(l, r, TRUE) != B_One() ) {
	    res = Make_0inp_Primitive(P_FAIL);
	    string m =
		wastrsave(stringsp, "gen_bOR over non-matching structures");
	    SET_FAIL_STRING(res, m);
	    return( res );
	}
	INC_REFCNT(l);
	return l;
    }
    bexpr lbe = GET_BEXPR(l);
    bexpr rbe = GET_BEXPR(r);
    bexpr bres = BE_Or(lbe, rbe);
    if( bres == lbe ) {
	INC_REFCNT(l);
	return l;
    }
    if( bres == rbe ) {
	INC_REFCNT(r);
	return r;
    }
    res = Make_BEXPR_leaf(bres);
    return res;
}

static g_ptr
g_be_and(g_ptr l, g_ptr r)
{
    g_ptr res;
    // Note: Gen_map2 will only call g_be_or with the same leaf type!
    ASSERT(IS_LEAF(l));
    if( !IS_BEXPR(l) ) {
	if( Is_equal(l, r, TRUE) != B_One() ) {
	    res = Make_0inp_Primitive(P_FAIL);
	    string m =
		wastrsave(stringsp, "gen_bOR over non-matching structures");
	    SET_FAIL_STRING(res, m);
	    return( res );
	}
	INC_REFCNT(l);
	return l;
    }
    bexpr lbe = GET_BEXPR(l);
    bexpr rbe = GET_BEXPR(r);
    bexpr bres = BE_And(lbe, rbe);
    if( bres == lbe ) {
	INC_REFCNT(l);
	return l;
    }
    if( bres == rbe ) {
	INC_REFCNT(r);
	return r;
    }
    res = Make_BEXPR_leaf(bres);
    return res;
}

static g_ptr
g_be_ite(g_ptr l, g_ptr r)
{
    g_ptr res;
    // Note: Gen_map2 will only call g_be_ite with the same leaf type!
    ASSERT(IS_LEAF(l));
    if( !IS_BEXPR(l) ) {
	if( Is_equal(l, r, TRUE) != B_One() ) {
	    res = Make_0inp_Primitive(P_FAIL);
	    string m = wastrsave(stringsp, "bITE over non-matching structures");
	    SET_FAIL_STRING(res, m);
	    return( res );
	}
	INC_REFCNT(l);
	return l;
    }
    bexpr lbe = GET_BEXPR(l);
    bexpr rbe = GET_BEXPR(r);
    bexpr bres = BE_Or(BE_And(current_bexpr_cond, lbe),
		      BE_And(BE_Not(current_bexpr_cond), rbe));
    if( bres == lbe ) {
	INC_REFCNT(l);
	return l;
    }
    if( bres == rbe ) {
	INC_REFCNT(r);
	return r;
    }
    res = Make_BEXPR_leaf(bres);
    return res;
}

static g_ptr
g_be_eq(g_ptr l, g_ptr r)
{
    // Note: Gen_map2 will only call g_be_ite with the same leaf type!
    ASSERT(IS_LEAF(l));
    if( !IS_BEXPR(l) ) {
	if( Is_equal(l,r,TRUE) == B_Zero() ) {
	    current_bexpr_eq = BE_FALSE;
	}
	return NULL;
    }
    bexpr lbe = GET_BEXPR(l);
    bexpr rbe = GET_BEXPR(r);
    bexpr bres = BE_Xnor(lbe,rbe);
    current_bexpr_eq = BE_And(current_bexpr_eq, bres);
    return NULL;
}

static ui
mk_prandom(uint32_t *seedp)
{
    uint32_t seed = *seedp;
    uint32_t r1, r2;
    seed = (uint32_t) (((uint64_t)seed * 48271UL) % 2147483647UL);
    r1 = seed;
    seed = (uint32_t) (((uint64_t)seed * 48271UL) % 2147483647UL);
    r2 = seed;
    *seedp = seed;
    return( ((ui) r1 << 32) | ((ui) r2) );
}

static bexpr
bdd2bexpr(formula f)
{
    if( f == B_One() ) { return BE_TRUE; }
    if( f == B_Zero() ) { return BE_FALSE; }
    bool neg = FALSE;
    if( ISNOT(f) ) {
	f = NOT(f);
	neg = TRUE;
    }
    bexpr res;
    if( (res = find_hash(&bdd2bexpr_tbl, FORMULA2PTR(f))) != NULL ) {
	if( neg ) { res = BE_NOT(res); }
	return res;
    }
    string var;
    formula H, L;
    Get_top_cofactor(f, &var, &H, &L);
    res = BE_Ite(BE_Var(var), bdd2bexpr(H), bdd2bexpr(L));
    insert_hash(&bdd2bexpr_tbl, FORMULA2PTR(f), res);
    if( neg ) { res = BE_NOT(res); }
    return res;
}
