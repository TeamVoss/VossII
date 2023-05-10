//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 1993			*/
/*									*/
/************************************************************************/
#include "new_bdd.h"
#include "prefs_ext.h"
#include "graph.h"

/* ------------- Global variables ------------- */
formula		ZERO;
formula		ONE;
bool		Do_gc_asap;
int             LG_TBL_SIZE;

/********* Global variables referenced ***********/
extern str_mgr  strings;
extern FILE	*v_order_fp;
extern FILE     *odests_fp;
extern g_ptr    void_nd;
extern bool	gui_mode;
extern bool	use_stdout;
extern char	FailBuf[4096];
extern bool	Interrupt_asap;

/***** PRIVATE VARIABLES *****/
static cache_ptr	cache_free_list;
static g_ptr		new_var_order_list = NULL;
static bdd_ptr		MainTbl;		/* Global BDD node table      */
static lunint		sz_MainTbl = 0;		/* Nbr of bins in ClustBuf    */
static formula		b_free_list;		/* Free list of bdd nodes     */
static int		bdd_save_cnt;

static char		draw_cmd[1024];

static lunint		gc_limit;		/* When do garbage collection */

static var_ptr		VarTbl;			/* Variable table	      */
static unint		sz_VarTbl;		/* Nbr of bins in VarTbl      */
static unint		nbr_VarTbl;		/* Nbr of element in VarTbl   */

static hash_record	save_var_tbl;
static hash_record	var_htbl;		/* Variable lookup table      */
static hash_record	rvar_htbl;		/* Rvariable lookup table     */

static lunint		nodes_used;
static unint		unique_cnt;
static unint		bddwidth[MAX_VARS];

static int		draw_node_id;
static bool		use_negated_pointers;

static unsigned long int primes[] = {
                            13,
                            23,
                            59,
                            113,
                            241,
                            503,
                            1019,
                            2039,
                            4091,
                            8179,
                            16369,
                            32749,
                            65521,
                            131063,
                            262139,
                            524269,
                            1048571,
                            2097143,
                            4194287,
                            8388593,
                            16777199,
                            33554393,
                            67108859,
                            134217689,
                            268435399,
                            536870879,
                            1073741789,
                            2147483629,
                            0};

static formula		prod[MAX_PROD_SIZE];
static bool		used_prods[MAX_PROD_SIZE];
static formula		*cur_prod;
static formula		fn_printed;
static int              prods_printed;
extern int              RCmax_prods_to_print;
extern int              RCdynamic_ordering_threshold;
static print_types      print_format;


static rec_mgr		subst_rec_mgr;
static rec_mgr		cache_rec_mgr;
static hash_record	cache_tbl;
static rec_mgr		subst_cache_rec_mgr;
static hash_record	subs_tbl;
static hash_record	bdd_size_tbl;
static hash_record	bdd_save_tbl;
static relprod_cache_ptr relprod_cache;
static int 		relprod_cache_sz;

extern int		RCminimum_reduction;	/* For dynamic var. ordering  */
extern int		RCelasticity;		/* For dynamic var. ordering  */
static bool		quit_early;
static bool		dont_grow_uniq_tbl;
static bool		want_to_grow_uniq_tbl;
static buffer		new_order_tbl;
static hash_record	new_order_htbl;
static bool		user_defined_ordering_flag;
static bool		insist_on_reorder;
static char		buf[1024];
static subst_ptr	bdd_subs;

static buffer           bdd_gc_buf;

/* ----- Forward definitions local functions ----- */
static formula		prim_B_Var(string name);
static formula		find_insert_bdd(var_ptr vp, formula lson, formula rson);
static formula		bdd_step(int fn, formula f1, formula f2);
static formula		quantify(formula f, formula vf, int type);
static void		bdd_tree_print(odests fp, formula f);
static void		bdd_infix_print(odests fp, formula f);
static void		garbage_collect();
static formula		print_prod(odests fp, formula f);
static VOID		sop_print(odests fp, formula f);
static bool		sop_pr_rec(odests fp, formula node,
				   bool look_for_zero, formula f);
string			get_var_name(formula f);
static unsigned int	subst_hash(pointer p, unsigned int n);
static bool		subst_eq(pointer p1, pointer p2);
static unsigned int	bdd_hash(pointer np, unsigned int n);
static bool		bdd_eq(pointer p1, pointer p2);
static void		clean_cache(bool erase);
static lunint		uniq_hash_fn(formula l, formula r, lunint n);
static void		Bdec_ref_cnt(formula f);
static void		re_order();
static void		user_defined_reorder();
static void		do_swap_down(buffer *rev_tablep, unint pos);
static void		swap_down(buffer *rev_tablep, unint pos);
static void		inc_ref_cnt(formula f);
static unint		f2var(formula f);
static formula		f2rson(formula f);
static formula		f2lson(formula f);
static relprod_cache_ptr find_in_relprod_cache(formula v, formula a, formula b);
static int		var_ord_comp(const void *pi, const void *pj);
static void		b_save(FILE *fp, formula f);
static g_ptr		top_b_subst(g_ptr np);
static g_ptr		top_bdd_depends(g_ptr np);
static g_ptr		top_bdd_size(g_ptr np);
static subst_ptr	add_to_subs(subst_ptr start, formula v, formula e);
static formula		b_substitute(formula f, subst_ptr subs);
static bool		do_draw_bdds(formula *bdds, int cnt);
static int		draw_bdd_rec(FILE *fp, hash_record *hp, formula f);
static bool		truth_cover_rec(hash_record *done_tblp,
					buffer *var_bufp, unint idx, formula b,
					arbi_T *resp, string *emsgp);
static int		bdd_get_size_rec(formula f, int *limitp);
static void		restore_mark(formula f);
static cache_ptr	get_new_cache_rec();
static unint		cache_hash(pointer p, unint n);
static bool		cache_equ(pointer p1, pointer p2);
/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

#ifdef DEBUG

bdd_ptr
f_GET_BDDP(formula f)
{
    return( GET_BDDP(f) );
}

int
f_ISNOT(formula f)
{
    return( ISNOT(f) );
}

formula
f_POS(formula f)
{
    return( POS(f) );
}

int
f_BDD_GET_VAR(bdd_ptr f)
{
    return( BDD_GET_VAR(f) );
}

formula
f_GET_LSON(bdd_ptr f)
{
    return( GET_LSON(f) );
}

formula
f_GET_RSON(bdd_ptr f)
{
    return( GET_RSON(f) );
}

void
BP(formula f)
{
    B_Print(err_fp, f, -1);
}


#endif

/* B_One -- Return the Boolean formula for the constant 1 */
formula
B_One()
{
    BDD_RETURN( ONE );
}

/* B_Zero -- Return the Boolean formula for the constant 0 */
formula
B_Zero()
{
    BDD_RETURN( ZERO );
}


/* B_Var -- Return Boolean formula for variable name */
formula
B_Var(string name)
{
    formula 	ret;
    if( (ret = PTR2FORMULA(find_hash(&var_htbl, (pointer) name))) != 0 ) {
	BDD_RETURN(ret);
    }
    int len = strlen(name);
    if( len >= 2 && *(name+len-2) == '_' && *(name+len-1) == 'n' ) {
	/* Make sure base name has been declared */
	string b_name = strtemp(name);
	*(b_name+len-2) = 0;
	b_name = wastrsave(&strings, b_name);
	prim_B_Var(b_name);
	name = wastrsave(&strings, name);
	ret = prim_B_Var(name);
	return( ret );
    } else {
	/* Declare both basename and next-state name */
	name = wastrsave(&strings, name);
	ret = prim_B_Var(name);
	string n_name = strtemp(name);
	n_name = strappend("_n");
	n_name = wastrsave(&strings, n_name);
	prim_B_Var(n_name);
	return ret;
    }
}

/* B_Var -- Return Boolean formula for variable name */
static formula
prim_B_Var(string name)
{
    formula 	ret;
    var_ptr	vp;

    if( (ret = PTR2FORMULA(find_hash(&var_htbl, (pointer) name))) != 0 ) {
	BDD_RETURN(ret);
    }

    if( nbr_VarTbl >= MAX_VARS-1 ) {
	Eprintf("Too many variables (maximum %d)\n", MAX_VARS-1);
    }

    /* Make new variable level */
    if( nbr_VarTbl == sz_VarTbl ) {
	/* Double the buffer size */
	sz_VarTbl = 2*sz_VarTbl;
	VarTbl = (var_ptr) Realloc((pointer) VarTbl, sz_VarTbl*sizeof(var_rec));
    }
    vp = VarTbl + nbr_VarTbl;
    vp->variable = nbr_VarTbl;
    vp->var_name = wastrsave(&strings, name);
    vp->sz_uniq_tbl = 1019;
    vp->nbr_uniq_tbl = 0;
    vp->uniq_tbl = (formula *) Malloc(vp->sz_uniq_tbl*sizeof(formula));
    for(unint i = 0; i < vp->sz_uniq_tbl; i++) {
	*(vp->uniq_tbl+i) = LNULL;
    }
    nbr_VarTbl++;
    ret = find_insert_bdd(vp, ONE, ZERO);
    /* Make sure variables never get removed */
    bdd_ptr bp = GET_BDDP(ret);
    bp->ref_cnt = BDD_MAX_REF_CNT;
    insert_hash(&var_htbl, (pointer) name, FORMULA2PTR(ret));
    ASSERT( POS(ret) != 0 );
    BDD_RETURN( ret );
}

/* Rand_Var -- Return random Boolean value for variable name */
formula
Rand_Var(string name)
{
    formula 	ret;

    if( (ret = PTR2FORMULA(find_hash(&rvar_htbl, (pointer) name))) != 0 )
	BDD_RETURN(ret-1);
    if( rand() < 1073741824 )
	ret = B_Zero();
    else
	ret = B_One();
    insert_hash(&rvar_htbl, (pointer) name, FORMULA2PTR(ret+1));
    BDD_RETURN( ret );
}

/* Return the size of the BDD */
int
Get_bdd_size(formula f, int limit) 
{
    if( f == ZERO || f == ONE)
	return 0;
    int res = bdd_get_size_rec(f, &limit);
    restore_mark(f);
    return res;
}

/* Reset_Ref_cnts -- Prepare for a garbage collection */
void
Reset_Ref_cnts()
{
    if( Do_gc_asap ) {
	bdd_ptr bp = MainTbl;
	bp->mark = TRUE;
	bp->ref_cnt = MAX_REF_CNT;
	for(unint i = 0; i < sz_MainTbl; i++) {
	    bp->mark = 0;
	    bp->ref_cnt = 0;
	    bp++;
	}
    }
}

void
Get_top_cofactor(formula b, string *varp, formula *Hp, formula *Lp)
{
    bdd_ptr bp = GET_BDDP(b);
    var_ptr vp = VarTbl + BDD_GET_VAR(bp);
    *varp = wastrsave(&strings, vp->var_name);
    *Hp = GET_LSON(bp);
    *Lp = GET_RSON(bp);
}

static void
gc_update_ref_cnt(bdd_ptr bp)
{
    if( bp->ref_cnt != BDD_MAX_REF_CNT ) {
	bp->ref_cnt++;
    }
    if( bp->mark ) { return; }
    bp->mark = 1;
    formula tmp = GET_LSON(bp);
    if( !B_IS_CONSTANT(tmp) )
	gc_update_ref_cnt(GET_BDDP(tmp));
    tmp = GET_RSON(bp);
    if( !B_IS_CONSTANT(tmp) )
	gc_update_ref_cnt(GET_BDDP(tmp));
}

/* B_Mark -- Make an external reference to f (at most one reference) */
void
B_Mark(formula f)
{
    bdd_ptr bp;
    if( !Do_gc_asap )
	return;
    if( B_IS_CONSTANT(f) ) return;
    bp = GET_BDDP(f);
    gc_update_ref_cnt(bp);
}

static void
mark_vars(pointer key, pointer data)
{
    (void) key;
    formula f = PTR2FORMULA(data);
    bdd_ptr bp = GET_BDDP(f);
    bp->mark = 1;
    bp->ref_cnt = BDD_MAX_REF_CNT;
}


/* B_Clean -- Do a garbage collection */
void
B_Clean()
{
    if( Do_gc_asap ) {
	formula *fp;
	// Mark all variables
	scan_hash(&var_htbl, mark_vars);
	// Mark all GC protected BDDs
	FOR_BUF(&bdd_gc_buf, formula, fp) {
	    if( *fp != ONE && *fp != ZERO ) {
		bdd_ptr bp = GET_BDDP(*fp);
		gc_update_ref_cnt(bp);
	    }
	}
	bdd_ptr bp = MainTbl;
	bp->mark = TRUE;
	bp->ref_cnt = MAX_REF_CNT;
	// Then do the g.c.
	garbage_collect();
    }
    Do_gc_asap = 0;
}

/* B_New_unique_var -- Return new unique Boolean variable with similar name */
formula
B_New_unique_var(formula f)
{
    char  	buf[64];
    bdd_ptr	bp;
    bp = GET_BDDP(f);
    Sprintf(buf, "%%%s%d", (VarTbl + BDD_GET_VAR(bp))->var_name, unique_cnt++);
    BDD_RETURN( B_Var(buf) );
}

/* B_Not -- Return the complement of a formula */
formula
B_Not(formula f1)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
#endif
    BDD_RETURN(NOT(f1));
}

formula
B_Nor(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    BDD_RETURN( NOT(bdd_step(OR, f1, f2)) );
}

formula
B_Nand(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    BDD_RETURN( NOT(bdd_step(AND, f1, f2)) );
}

formula
B_And(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    BDD_RETURN( bdd_step(AND, f1, f2) );
}

formula
B_Or(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    BDD_RETURN( bdd_step(OR, f1, f2) );
}


formula
B_Xor(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    BDD_RETURN( bdd_step(XOR, f1, f2) );
}

formula
B_Xnor(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    BDD_RETURN( NOT(bdd_step(XOR, f1, f2)) );
}

formula
B_Ite(formula i, formula t, formula e)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(i)->in_use );
    ASSERT( GET_BDDP(t)->in_use );
    ASSERT( GET_BDDP(e)->in_use );
#endif
    formula f1 = B_And(i, t);
    formula f2 = B_And(B_Not(i), e);
    formula res = B_Or(f1, f2);
    BDD_RETURN( res );
}

/* B_Equal -- Compare two Boolean functions */
bool
B_Equal(formula f1, formula f2)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(f1)->in_use );
    ASSERT( GET_BDDP(f2)->in_use );
#endif
    return( f1 == f2 );
}

/* Special ``fast'' versions for next state evaluations */
void
F_not(formula op1, formula op2, formula *res)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(op1)->in_use );
#endif
    (void) op2;
    *res = NOT(op1);
}

void
F_and(formula op1, formula op2, formula *res)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(op1)->in_use );
    ASSERT( GET_BDDP(op2)->in_use );
#endif
    if( op1 == ZERO || op2 == ZERO )
	*res = ZERO;
    else
	if( op1 == ONE )
	    *res = op2;
	else
	    if( op2 == ONE )
		*res = op1;
	    else
		*res = bdd_step(AND, op1, op2);
}

void
F_or(formula op1, formula op2, formula *res)
{
#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(op1)->in_use );
    ASSERT( GET_BDDP(op2)->in_use );
#endif
    if( op1 == ONE || op2 == ONE )
	*res = ONE;
    else
	if( op1 == ZERO )
	    *res = op2;
	else
	    if( op2 == ZERO )
		*res = op1;
	    else
		*res = bdd_step(OR, op1, op2);
}

formula
B_Depends(formula f)
{
    cache_ptr	cp;
    formula	lson, rson, ret;
    var_ptr	vp;
    bdd_ptr	bp;

    /* Find simple identities */
    if( f == ZERO )
	BDD_RETURN( ONE );
    if( f == ONE )
	BDD_RETURN( ONE );

    /* Check cache */
    cache_rec cr;
    cr.fn = DEPENDS;
    cr.u.args.lson = f;
    cr.u.args.rson = f;
    if( (cp = (cache_ptr) find_hash(&cache_tbl, &cr)) != NULL ) {
	BDD_RETURN(cp->res);
    }

    bp = GET_BDDP(f);
    vp = VarTbl + BDD_GET_VAR(bp);
    lson  = GET_LSON(bp);
    rson  = GET_RSON(bp);
    ret = B_And(B_And(B_Depends(lson), B_Depends(rson)),
		find_insert_bdd(vp, ONE, ZERO));

    /* Insert into cache */
    cp = get_new_cache_rec();
    cp->fn = DEPENDS;
    cp->u.args.lson = f;
    cp->u.args.rson = f;
    cp->res = ret;
    insert_hash(&cache_tbl, cp, cp);
    BDD_RETURN( ret );
}

formula
B_forall(formula var, formula f)
{
    BDD_RETURN( quantify(f, var, UNIVERSAL) );
}

formula
B_thereis(formula var, formula f)
{
    BDD_RETURN( quantify(f, var, EXISTENTIAL) );
}

/* B_Print -- Print Boolean formula as an expression tree */
void
B_Print(odests fp, formula f, int max_size)
{
    switch(print_format) {
	case infix_format:
	    {
		int old_size = RCmax_prods_to_print;
		if( max_size != -1 )
		    RCmax_prods_to_print = max_size;
		bdd_infix_print(fp, f);
		RCmax_prods_to_print = old_size;
		break;
	    }
	case sop_format:
	    sop_print(fp, f); break;
	case tree_format:
	    bdd_tree_print(fp, f); break;
	default:
	    Eprintf("Unknown print format");
    }
}

/* HL_Print -- Print ternary function given in dual-rail encoding */
void
HL_Print(odests fp, formula H, formula L)
{
    formula o, x;
    if( B_Or(L, H) != ONE ) {
	    FP(fp, "***** OVERCONSTRAINED VALUE *****");
	    return;
    }
    o = B_And(H, NOT(L));
    x = B_And(H, L);
    if( o == ZERO ) {
	if( x == ZERO ) {
	    FP(fp, "0");
	    return;
	}
	FP(fp, "X");
	if( x == ONE )
	    return;
	FP(fp, "(");
	B_Print(fp, x, -1);
	FP(fp, ")");
    } else if( o == ONE )
	FP(fp, "1");
    else {
	B_Print(fp, o, -1);
	if( x == ZERO )
	    return;
	FP(fp, " + X");
	if( x == ONE )
	    return;
	FP(fp, "(");
	B_Print(fp, x, -1);
	FP(fp, ")");
    }
}

/* HL_Pattern -- Print pattern for ternary value */
void
HL_Pattern(odests fp, formula H, formula L)
{
    if( B_Or(H, L) != ONE ) {
	FP(fp, "illegal");
	return;
    }
    if( H == ZERO ) {
	FP(fp, "L");
	return;
    }
    if( L == ZERO ) {
	FP(fp, "H");
	return;
    }
    if( H == ONE ) {
	if( L == ONE ) {
	    FP(fp, "X");
	    return;
	}
	FP(fp, "XH");
	return;
    }
    if( L == ONE ) {
	FP(fp, "LX");
	return;
    }
    if( B_And(H,L) == ZERO )
	FP(fp, "LH");
    else
	FP(fp, "LXH");
    return;
}

int
B_Width(unint v)
{
    ASSERT(v < nbr_VarTbl);
    for(unint i = 0; i < nbr_VarTbl; i++) {
	if( ORDER(i) == v )
	    return( *(bddwidth + i) );
    }
    DIE("Should not happen.");
}

string
Get_Var_Name(unint v)
{
    ASSERT(v < nbr_VarTbl);
    for(unint i = 0; i < nbr_VarTbl; i++) {
	if( ORDER(i) == v )
	    return( (VarTbl+i)->var_name );
    }
    DIE("Should not happen.");
}

unint
Get_VarCnt()
{
    return(nbr_VarTbl);
}

void
Reset_BDD_Size()
{
    for(unint i = 0;  i < nbr_VarTbl; i++)
	*(bddwidth + i) = 0;
    dispose_hash(&bdd_size_tbl, NULLFCN);
    create_hash(&bdd_size_tbl, 100, bdd_hash, bdd_eq);
}

void
B_Size(formula f)
{
    unint	var;
    bdd_ptr	bp;

    if( f == ZERO || f == ONE)
        return;
    if( find_hash(&bdd_size_tbl, FORMULA2PTR(f)) != NULL )
	return;
    bp = GET_BDDP(f);
    B_Size(POS(GET_LSON(bp)));
    B_Size(POS(GET_RSON(bp)));
    var = BDD_GET_VAR(bp);
    (*(bddwidth + var))++;
    insert_hash(&bdd_size_tbl, FORMULA2PTR(f), FORMULA2PTR(f));
}


bool
Save_BDDs(string filename, buffer *roots)
{
    FILE *fp;
    if( (fp = fopen(filename, "w")) == NULL ) {
	Fail_pr("Cannot open %s for writing BDDs", filename);
	return( FALSE );
    }
    fprintf(fp, "SAVED_BDDS %d\n", COUNT_BUF(roots));
    // Now write out all variables used
    create_hash(&save_var_tbl, 100, str_hash, str_equ);
    Reset_BDD_Size();
    formula	*bp;
    FOR_BUF(roots, formula, bp) {
	B_Size(*bp);
    }
    unint vars = Get_VarCnt();
    unint vcnt = 0;
    for(unint i = 0; i < vars; i++) {
	if( B_Width(i) > 0 ) {
	    string name = Get_Var_Name(i);
	    fprintf(fp, "v %s\n", name);
	    insert_hash(&save_var_tbl, (pointer) name, FORMULA2PTR(vcnt));
	    vcnt++;
	}
    }
    create_hash(&bdd_save_tbl, 100, bdd_hash, bdd_eq);
    bdd_save_cnt = 1;
    FOR_BUF(roots, formula, bp) {
	b_save(fp, *bp);
    }
    FOR_BUF(roots, formula, bp) {
	int i;
	if( ISNOT(*bp) ) {
	    i = (long int) find_hash(&bdd_save_tbl, FORMULA2PTR(POS(*bp)));
	    fprintf(fp, "r -%d\n", i);
	} else {
	    i = (long int) find_hash(&bdd_save_tbl, FORMULA2PTR(*bp));
	    fprintf(fp, "r +%d\n", i);
	}
    }
    fclose(fp);
    dispose_hash(&bdd_save_tbl, NULLFCN);
    return( TRUE );
}

bool
Load_BDDs(string filename, buffer *results)
{
    FILE *fp;
    if( (fp = fopen(filename, "r")) == NULL ) {
	Fail_pr("Cannot open %s for reading BDDs", filename);
	return( FALSE );
    }

    int cnt;
    if( fscanf(fp, "SAVED_BDDS %d\n", &cnt) != 1 ) {
	fclose(fp);
	Fail_pr("Illegal file format around line 1 in %s", filename);
        return 1;
    }
    buffer var_map;
    new_buf(&var_map, 100, sizeof(formula));
    while( fscanf(fp, "v %s\n", buf) == 1 ) {
	string name = wastrsave(&strings, buf);
	formula b = B_Var(name);
	push_buf(&var_map, (pointer) &b);
    }
    buffer work_buf;
    new_buf(&work_buf, 1000, sizeof(formula));
    formula f = B_Zero();
    push_buf(&work_buf, &f);
    unint vidx, l, r;
    char sl, sr;
    while( fscanf(fp, "n %d %c%d %c%d\n", &vidx, &sl, &l, &sr, &r) == 5 ) {
	formula v = *((formula *) M_LOCATE_BUF(&var_map, vidx));
	formula pt = *((formula *) M_LOCATE_BUF(&work_buf, l));
	formula t = (sl == '-') ? B_Not(pt) : pt;
	formula pe = *((formula *) M_LOCATE_BUF(&work_buf, r));
	formula e = (sr == '-') ? B_Not(pe) : pe;
	formula b = B_Ite(v, t, e);
	push_buf(&work_buf, &b);
    }
    while( fscanf(fp, "r %c%d\n", &sr, &r) == 2 ) {
	formula b = *((formula *) M_LOCATE_BUF(&work_buf, r));
	b = (sr == '-')? B_Not(b) : b;
	push_buf(results, &b);
    }
    free_buf(&var_map);
    free_buf(&work_buf);
    fclose(fp);
    return( TRUE );
}

void
Begin_RelProd()
{
    relprod_cache_sz = sz_MainTbl/10;
    relprod_cache = (relprod_cache_ptr)
			Calloc((relprod_cache_sz)*sizeof(relprod_cache_rec));
}

void
End_RelProd()
{
    Free((pointer) relprod_cache);
}

formula
Rel_prod(formula qv, formula a, formula b, bool existential)
{
    relprod_cache_ptr	old;
    formula		ret;
    unint		aindex, bindex, vindex, mindex;

    /* Find simple identities */
    if( a == ZERO || b == ZERO || a == NOT(b) )
	BDD_RETURN( ZERO );
    if( a == ONE || (a == b))
	BDD_RETURN( quantify(b, qv, existential) );
    if( b == ONE )
	BDD_RETURN( quantify(a, qv, existential) );
    if( qv == ONE || qv == ZERO )
	BDD_RETURN( B_And(a,b) );

    /* Look up in cache */
    old = find_in_relprod_cache(qv,a,b);
    if( old->v == qv && old->a == a && old->b == b ) {
	BDD_RETURN(old->result);
    }

    vindex = f2var(qv);
    aindex = f2var(a);
    bindex = f2var(b);
    if( ORDER(aindex) < ORDER(bindex) )
	mindex = aindex;
    else
	mindex = bindex;
    if( ORDER(vindex) < ORDER(mindex) ) {
	/* Quantified variable above formulas */
	ret = Rel_prod(f2lson(qv), a, b, existential);
    } else {
	if( ORDER(vindex) > ORDER(mindex) ) {
	    /* Formula above quantified variable */
	    bdd_ptr	ap,bp;
	    formula 	lson1, lson2, rson1, rson2;
	    var_ptr	vp;

	    ap = GET_BDDP(a);
	    bp = GET_BDDP(b);
	    if( ORDER(aindex) > ORDER(bindex) ) {
		lson1 = a;
		rson1 = a;
		vp = VarTbl + bindex;
	    } else {
		if( ISNOT(a) ) {
		    lson1 = NOT(GET_LSON(ap));
		    rson1 = NOT(GET_RSON(ap));
		} else {
		    lson1 = GET_LSON(ap);
		    rson1 = GET_RSON(ap);
		}
		vp = VarTbl + aindex;
	    }
	    if( ORDER(bindex) > ORDER(aindex) ) {
		lson2 = b;
		rson2 = b;
	    } else {
		if( ISNOT(b) ) {
		    lson2 = NOT(GET_LSON(bp));
		    rson2 = NOT(GET_RSON(bp));
		} else {
		    lson2 = GET_LSON(bp);
		    rson2 = GET_RSON(bp);
		}
	    }

	    lson1 = Rel_prod(qv, lson1, lson2, existential);
	    rson1 = Rel_prod(qv, rson1, rson2, existential);
	    if( lson1 == rson1 )
		ret = lson1;
	    else
		ret = find_insert_bdd(vp, lson1, rson1);
	} else {
	    /* At least one formula at the same level as quantified variable */
	    bdd_ptr	ap,bp;
	    formula 	lson1, lson2, rson1, rson2;

	    ap = GET_BDDP(a);
	    bp = GET_BDDP(b);
	    if( ORDER(aindex) > ORDER(bindex) ) {
		lson1 = a;
		rson1 = a;
	    } else {
		if( ISNOT(a) ) {
		    lson1 = NOT(GET_LSON(ap));
		    rson1 = NOT(GET_RSON(ap));
		} else {
		    lson1 = GET_LSON(ap);
		    rson1 = GET_RSON(ap);
		}
	    }
	    if( ORDER(bindex) > ORDER(aindex) ) {
		lson2 = b;
		rson2 = b;
	    } else {
		if( ISNOT(b) ) {
		    lson2 = NOT(GET_LSON(bp));
		    rson2 = NOT(GET_RSON(bp));
		} else {
		    lson2 = GET_LSON(bp);
		    rson2 = GET_RSON(bp);
		}
	    }
	    lson1 = Rel_prod(f2lson(qv), lson1, lson2, existential);
	    rson1 = Rel_prod(f2lson(qv), rson1, rson2, existential);
	    if( existential )
		ret = bdd_step(OR, lson1, rson1);
	    else
		ret = bdd_step(AND, lson1, rson1);
	}
    }
    /* Insert result in cache */
    old->v = qv;
    old->a = a;
    old->b = b;
    old->result = ret;
    BDD_RETURN( ret );
}
void
New_ordering()
{
    new_buf(&new_order_tbl, nbr_VarTbl, sizeof(int));
    create_hash(&new_order_htbl, nbr_VarTbl, Ustr_hash, Ustr_equ);
    user_defined_ordering_flag = TRUE;
    new_var_order_list = Make_NIL();
    PUSH_GLOBAL_GC(new_var_order_list);
}

static bool
is_next_state_var(string name)
{
    int len = strlen(name);
    return( len > 2 && *(name+len-2) == '_' && *(name+len-1) == 'n' );
}


void
Add_ordering_var(string var)
{
    unint v;
    if( is_next_state_var(var) ) {
	Wprintf("Next-state variable (%s) given ordering location. Ignored!\n");
    } else {
	v = f2var(B_Var(var));
	push_buf(&new_order_tbl, (pointer) &v);
	// Next state variable
	v++;
	push_buf(&new_order_tbl, (pointer) &v);
    }
}

void
Reorder(int times)
{
    unint           o_dyn_var_repetitions;
    unint           o_minsize_for_dyn_ordering;
    bool            o_do_dynamic_var_order;
    bool            o_optimal_dynamic_var_order;

    o_do_dynamic_var_order = RCdo_dynamic_var_order;
    o_optimal_dynamic_var_order = RCoptimal_dynamic_var_order;
    o_dyn_var_repetitions = (unint) RCdyn_var_repetitions;
    o_minsize_for_dyn_ordering = (unint) RCminsize_for_dyn_ordering;
    RCdo_dynamic_var_order = TRUE;
    RCoptimal_dynamic_var_order = TRUE;
    RCdyn_var_repetitions = times;
    insist_on_reorder = TRUE;
    RCminsize_for_dyn_ordering = 0;
    Do_gc_asap = TRUE;
    Garbage_collect();
    insist_on_reorder = FALSE;
    RCdo_dynamic_var_order = o_do_dynamic_var_order;
    RCoptimal_dynamic_var_order = o_optimal_dynamic_var_order;
    RCdyn_var_repetitions = (int) o_dyn_var_repetitions;
    RCminsize_for_dyn_ordering = (int) o_minsize_for_dyn_ordering;
}

g_ptr
End_ordering()
{
    POP_GLOBAL_GC(1);
    return( new_var_order_list );
}


void
B_Init()
{
    bdd_ptr		new;
    unsigned long int	i;
    sz_MainTbl = 1<< LG_TBL_SIZE;
    MainTbl = (bdd_ptr) Malloc(sz_MainTbl*sizeof(bdd_rec));
    b_free_list = LNULL;
    i = sz_MainTbl-1;
    new = (bdd_ptr) (MainTbl + i);
    while( new > MainTbl ) {
	new->next = b_free_list;
#ifdef REF_CNT_DEBUG
	new->in_use = 0;
#endif
	new->mark = 0;
	new->sz_mark = 0;
	new->moving = 0;
	new->ref_cnt = 0;
	b_free_list = i;
	new--;
	i--;
    }
    MainTbl->ref_cnt = BDD_MAX_REF_CNT;		/* Zero is always used */
#ifdef REF_CNT_DEBUG
    MainTbl->in_use = 1;			/* Zero is always used */
#endif
    sz_VarTbl = 64;
    VarTbl = (var_ptr) Calloc(sz_VarTbl*sizeof(var_rec));
    nbr_VarTbl = 0;
    new_mgr(&cache_rec_mgr, sizeof(cache_rec));
    cache_free_list = NULL;
    create_hash(&cache_tbl, sz_MainTbl/10, cache_hash, cache_equ);
    ZERO = 0;
    ONE  = NOT(ZERO);
    create_hash(&var_htbl, sz_VarTbl, Ustr_hash, Ustr_equ);
    create_hash(&rvar_htbl, sz_VarTbl, Ustr_hash, Ustr_equ);
    nodes_used = 0;
    unique_cnt = 0;
    Do_gc_asap = 0;
    gc_limit   = (lunint) sz_MainTbl/2;
    if( strcmp(RCpr_str, "SOP") == 0 || strcmp(RCpr_str, "sop") == 0 )
	print_format = sop_format;
    else if( strcmp(RCpr_str, "INFIX") == 0 || strcmp(RCpr_str, "infix") == 0 )
	print_format = infix_format;
    else if( strcmp(RCpr_str, "TREE") == 0 || strcmp(RCpr_str, "tree") == 0 )
	print_format = tree_format;
    else {
	FP(err_fp, "WARNING: %s is not a valid PRINT-FORMAT\n", RCpr_str);
	FP(err_fp, "         Will use SOP\n");
	print_format = sop_format;
    }
    create_hash(&bdd_size_tbl, 100, bdd_hash, bdd_eq);
    dont_grow_uniq_tbl = FALSE;
    want_to_grow_uniq_tbl = FALSE;
    user_defined_ordering_flag = FALSE;
    new_buf(&bdd_gc_buf, 100, sizeof(formula));
}

void
PUSH_BDD_GC(formula f)
{
    push_buf(&bdd_gc_buf, (pointer) &f);
}

void
POP_BDD_GC(unint cnt)
{
    for(unint i = 0; i < cnt; i++) {
        pop_buf(&bdd_gc_buf, NULL);
    }
}

void
Get_abstract_depends(g_ptr redex, hash_record *abs_tblp, g_ptr obj)
{
    Reset_BDD_Size();
    Gen_map(top_bdd_depends, obj, TRUE);
    bool changed = FALSE;
    unint vars = Get_VarCnt();
    do {
	changed = FALSE;
	for(unint i = 0; i < vars; i++) {
	    if( B_Width(i) > 0 ) {
		string var = Get_Var_Name(i);
		formula	expr;
		if( (expr = PTR2FORMULA(find_hash(abs_tblp, var))) != 0 ) {
		    changed = TRUE;
		    B_Size(expr);
		    delete_hash(abs_tblp, var);
		}
	    }
	}
    } while( changed );
    MAKE_REDEX_NIL(redex);
    g_ptr cur = redex;
    for(unint i = 0; i < vars; i++) {
	if( B_Width(i) > 0 ) {
	    string var = Get_Var_Name(i);
	    int dummy;
	    if( sscanf(var, "_%d", &dummy) != 1 ) {
		SET_CONS_HD(cur, Make_STRING_leaf(Get_Var_Name(i)));
		SET_CONS_TL(cur, Make_NIL());
		cur = GET_CONS_TL(cur);
	    }
	}
    }
}

int
SHA256_bdd(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, formula f)
{
    int res;
    if( f == ZERO ) return -1;
    if( f == ONE ) return 1;
    bool neg = ISNOT(f);
    bdd_ptr bp = GET_BDDP(f);
    if( (res = PTR2INT(find_hash(g_tblp, bp))) != 0 ) {
        return (neg? -1*res : res);
    }
    res = *g_cntp;
    *g_cntp = res+1;
    res = neg? -1*res : res;
    insert_hash(g_tblp, bp, INT2PTR(res));
    int lres = SHA256_bdd(g_cntp, g_tblp, sha, GET_LSON(bp));
    int rres = SHA256_bdd(g_cntp, g_tblp, sha, GET_RSON(bp));
    SHA256_printf(sha, "%d=ITE %s %d %d\n", res, get_var_name(f), lres, rres);
    return res;
}


#if 0

static int signature_target = 0;
static hash_record path_cond_tbl;

static int
mark_relevant(formula f)
{
    if( f == ZERO || f == ONE)
        return 1;
    bdd_ptr bp = GET_BDDP(f);
    int res;
    if( (res = PTR2INT(find_hash(&path_cond_tbl, bp))) != 0 ) {
	return res;
    }
    if( POS(f) == signature_target ) {
	insert_hash(&path_cond_tbl, bp, INT2PTR(2));
	return 2;
    }
    int lres = mark_relevant(GET_LSON(bp));
    int rres = mark_relevant(GET_RSON(bp));
    if( lres == 2 || rres == 2 ) { 
	res = 2;
    } else {
	res = 1;
    }
    insert_hash(&path_cond_tbl, bp, INT2PTR(res));
    return res;
}

static formula
build_path_cond(formula f)
{
    if( f == ZERO || f == ONE)
        return f;
    formula pf = POS(f);
    bdd_ptr bp = GET_BDDP(f);

    if( f == ZERO || f == ONE )
        return;

    
}

formula
Get_Path_Condition(int signature, formula f)
{
    signature_target = POS(signature);
    create_hash(&path_cond_tbl, 100, bdd_hash, bdd_eq);
    mark_relevant(f);
    formula res = build_path_cond(f);

    dispose_hash(&path_cond_tbl, NULLFCN);
}

#endif

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
bddSignature(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    formula v = GET_BOOL(r);
    int i = (int) v;
    MAKE_REDEX_INT(redex, i);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bddT(g_ptr redex)
{
    MAKE_REDEX_BOOL(redex, B_One());
}

static void
bddF(g_ptr redex)
{
    MAKE_REDEX_BOOL(redex, B_Zero());
}

static void
variable(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BOOL(redex, B_Var(GET_STRING(r)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bddNOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BOOL(redex, B_Not(GET_BOOL(r)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bddAND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    if( B_IS_FALSE(GET_BOOL(arg1)) ) {
	MAKE_REDEX_BOOL(redex, B_FALSE);
    } else {
	g_ptr arg2 = reduce(GET_APPLY_RIGHT(redex), FALSE);
	if( IS_FAILURE(arg2) ) {
	    OVERWRITE(redex, arg2);
	    return;
	}
	MAKE_REDEX_BOOL(redex, B_And(GET_BOOL(arg1),GET_BOOL(arg2)));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bddOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    if( B_IS_TRUE(GET_BOOL(arg1)) ) {
	MAKE_REDEX_BOOL(redex, B_TRUE);
    } else {
	g_ptr arg2 = reduce(GET_APPLY_RIGHT(redex), FALSE);
	if( IS_FAILURE(arg2) ) {
	    OVERWRITE(redex, arg2);
	    return;
	}
	MAKE_REDEX_BOOL(redex, B_Or(GET_BOOL(arg1),GET_BOOL(arg2)));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bddXOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BOOL(redex, B_Xor(GET_BOOL(arg1),GET_BOOL(arg2)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bddXNOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_BOOL(redex, B_Xnor(GET_BOOL(arg1),GET_BOOL(arg2)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


static void
bv2num(g_ptr redex)
{   
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    arbi_T cur = Arbi_FromInt(0);
    while( !IS_NIL(arg1) ) {
        formula b = GET_BOOL(GET_CONS_HD(arg1));
        cur = Arbi_mlt(cur, Arbi_FromInt(2));
        if( b == B_One() ) {
            cur = Arbi_add(cur, Arbi_FromInt(1));
        } else if( b != B_Zero() ) {
            MAKE_REDEX_FAILURE(redex,Fail_pr("bv2num on symbolic vector"));
            return;
        }
        arg1 = GET_CONS_TL(arg1);
    }
    MAKE_REDEX_AINT(redex, cur);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
draw_bdds(g_ptr redex)
{   
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    if( !gui_mode ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("draw_bdds not available in -noX mode"));
	return;
    }
    use_negated_pointers = (GET_BOOL(arg1) == B_One());
    buffer bdd_buf;
    new_buf(&bdd_buf, 100, sizeof(formula));
    while( !IS_NIL(arg2) ) {
        formula b = GET_BOOL(GET_CONS_HD(arg2));
	push_buf(&bdd_buf, (pointer) &b);
        arg2 = GET_CONS_TL(arg2);
    }
    if ( do_draw_bdds(START_BUF(&bdd_buf), COUNT_BUF(&bdd_buf)) ) {
	MAKE_REDEX_VOID(redex);
    } else {
	MAKE_REDEX_FAILURE(redex, FailBuf);
    } 
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
top_cofactor(g_ptr redex)
{   
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    formula b = GET_BOOL(arg1);
    if( b == ZERO || b == ONE ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("cofactor not defined for constants T/F"));
	return;
    }
    bdd_ptr bp = GET_BDDP(b);
    var_ptr vp = VarTbl + BDD_GET_VAR(bp);
    string name = wastrsave(&strings, vp->var_name);
    formula L, R;
    if( ISNOT(b) ) {
	L = NOT(GET_LSON(bp));
	R = NOT(GET_RSON(bp));
    } else {
	L = GET_LSON(bp);
	R = GET_RSON(bp);
    }
    MAKE_REDEX_CONS_ND(redex, Make_STRING_leaf(name),
			Make_CONS_ND(Make_BOOL_leaf(L), Make_BOOL_leaf(R)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


static void
depends(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    Reset_BDD_Size();
    Gen_map(top_bdd_depends, arg1, TRUE);
    MAKE_REDEX_NIL(redex);
    g_ptr cur = redex;
    unint vars = Get_VarCnt();
    for(unint i = 0; i < vars; i++) {
	if( B_Width(i) > 0 ) {
	    SET_CONS_HD(cur, Make_STRING_leaf(Get_Var_Name(i)));
	    SET_CONS_TL(cur, Make_NIL());
	    cur = GET_CONS_TL(cur);
	}
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bdd_profile(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    Reset_BDD_Size();
    Gen_map(top_bdd_depends, arg1, TRUE);
    MAKE_REDEX_NIL(redex);
    g_ptr cur = redex;
    unint vars = Get_VarCnt();
    for(unint i = 0; i < vars; i++) {
	if( B_Width(i) > 0 ) {
	    SET_CONS_HD(cur, 
			Make_CONS_ND(Make_STRING_leaf(Get_Var_Name(i)),
				     Make_INT_leaf(B_Width(i))));
	    SET_CONS_TL(cur, Make_NIL());
	    cur = GET_CONS_TL(cur);
	}
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
bdd_size(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    Reset_BDD_Size();
    Gen_map(top_bdd_size, arg1, TRUE);
    unint vars = Get_VarCnt();
    unint size = 0;
    for(unint i = 0; i < vars; i++) {
	int wid = B_Width(i);
	size += wid;
    }
    MAKE_REDEX_INT(redex, size);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
limitedAND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr a, b, gsz;
    EXTRACT_3_ARGS(redex, a, b, gsz);
    int sz = GET_INT(gsz);
    formula res = B_And(GET_BOOL(a), GET_BOOL(b));
    if( Get_bdd_size(res, sz) >= sz ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("limitedAND exceeded max size"));
    } else {
	MAKE_REDEX_BOOL(redex, res);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
limitedOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr a, b, gsz;
    EXTRACT_3_ARGS(redex, a, b, gsz);
    int sz = GET_INT(gsz);
    formula res = B_Or(GET_BOOL(a), GET_BOOL(b));
    if( Get_bdd_size(res, sz) >= sz ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("limitedOR exceeded max size"));
    } else {
	MAKE_REDEX_BOOL(redex, res);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_substitute(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    new_mgr(&subst_rec_mgr, sizeof(subst_rec));
    new_mgr(&subst_cache_rec_mgr, sizeof(subst_cache_rec));
    create_hash(&subs_tbl, 100, subst_hash, subst_eq);
    bdd_subs = NULL;
    while( !IS_NIL(arg1) ) {
        g_ptr pair = GET_CONS_HD(arg1);
        formula v = B_Var(GET_STRING(GET_CONS_HD(pair)));
        formula e = GET_BOOL(GET_CONS_TL(pair));
	bdd_subs = add_to_subs(bdd_subs, v, e);
	arg1 = GET_CONS_TL(arg1);
    }
    g_ptr res = Gen_map(top_b_subst, arg2, FALSE);
    OVERWRITE(redex, res);
    dispose_hash(&subs_tbl, NULLFCN);
    free_mgr(&subst_rec_mgr);
    free_mgr(&subst_cache_rec_mgr);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_truth_cover(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list = GET_APPLY_RIGHT(l);
    formula fun = GET_BOOL(GET_APPLY_RIGHT(redex));
    buffer  var_table;
    new_buf(&var_table, nbr_VarTbl, sizeof(unint));
    hash_record truth_table_done;
    create_hash(&truth_table_done, 100, bdd_hash, bdd_eq);
    while( !IS_NIL(var_list) ) {
        string vname = GET_STRING(GET_CONS_HD(var_list));
	formula v = B_Var(vname);
	bdd_ptr	bp = GET_BDDP(v);
	unint var = BDD_GET_VAR(bp);
	push_buf(&var_table, (pointer) &var);
	var_list = GET_CONS_TL(var_list);
    }

    qsort(START_BUF(&var_table), COUNT_BUF(&var_table), sizeof(unint),
		    var_ord_comp);

    arbi_T res;
    string emsg;
    if( !truth_cover_rec(&truth_table_done, &var_table, 0, fun, &res, &emsg) ) {
	MAKE_REDEX_FAILURE(redex, emsg);
    } else {
	MAKE_REDEX_AINT(redex, res);
    }
    free_buf(&var_table);
    dispose_hash(&truth_table_done, NULLFCN);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static bool
truth_cover_rec(hash_record *done_tblp, buffer *var_bufp, unint idx, formula b,
		arbi_T *resp, string *emsgp)
{
    if( b == ZERO ) {
	*resp = Arbi_FromInt(0);
	return TRUE;
    }
    if( b == ONE ) {
	arbi_T res = Arbi_FromInt(1);
	while( idx < COUNT_BUF(var_bufp) ) {
	    res = Arbi_mlt(res, Arbi_FromInt(2));
	    idx++;
	}
	*resp = res;
	return TRUE;
    }

    bdd_ptr bp = GET_BDDP(b);
    unint next_var = BDD_GET_VAR(bp);
    arbi_T mult = Arbi_FromInt(1);
    if( idx == COUNT_BUF(var_bufp) ) {
	var_ptr vp = VarTbl + next_var;
	*emsgp =
	    Fail_pr("Variable %s not in truth_cover list but f depends on it",
		    vp->var_name);
	return FALSE;
    }
    while( *((unint *) M_LOCATE_BUF(var_bufp, idx)) != next_var ) {
	mult = Arbi_mlt(mult, Arbi_FromInt(2));
	idx++;
	if( idx == COUNT_BUF(var_bufp) ) {
	    var_ptr vp = VarTbl + next_var;
	    *emsgp =
	      Fail_pr("Variable %s not in truth_cover list but f depends on it",
		      vp->var_name);
	    return FALSE;
	}
    }
    arbi_T old_resp = (arbi_T) find_hash(done_tblp, FORMULA2PTR(b));
    if( old_resp != NULL ) {
	*resp = Arbi_mlt(mult, old_resp);
	return TRUE;
    }
    formula L, R;
    if( ISNOT(b) ) {
        L = NOT(GET_LSON(bp));
        R = NOT(GET_RSON(bp));
    } else {
        L = GET_LSON(bp);
        R = GET_RSON(bp);
    }
    arbi_T Lres;
    if( !truth_cover_rec(done_tblp, var_bufp, idx+1, L, &Lres, emsgp) ) {
	return FALSE;
    }
    arbi_T Rres;
    if( !truth_cover_rec(done_tblp, var_bufp, idx+1, R, &Rres, emsgp) ) {
	return FALSE;
    }
    arbi_T sum = Arbi_add(Lres, Rres);
    insert_hash(done_tblp, FORMULA2PTR(b), (pointer) sum);
    *resp = Arbi_mlt(mult, sum);
    return TRUE;
}


void
BDD_Install_Functions()
{

    Add_ExtAPI_Function("bdd_signature", "1", FALSE,
			GLmake_arrow(GLmake_bool(), GLmake_int()),bddSignature);

    Add_ExtAPI_Function("T", "", FALSE, GLmake_bool(), bddT);

    Add_ExtAPI_Function("F", "", FALSE, GLmake_bool(), bddF);

    Add_ExtAPI_Function("variable", "1", FALSE,
			GLmake_arrow(GLmake_string(),GLmake_bool()),
			variable);

    Add_ExtAPI_Function("NOT", "1", FALSE,
			GLmake_arrow(GLmake_bool(),GLmake_bool()),
			bddNOT);

    Add_ExtAPI_Function("AND", "1-", FALSE,
			GLmake_arrow(GLmake_bool(),
				      GLmake_arrow(GLmake_bool(),
						   GLmake_bool())),
			bddAND);
    Insert_infix("AND", 4);

    Add_ExtAPI_Function("OR", "1-", FALSE,
			GLmake_arrow(GLmake_bool(),
				      GLmake_arrow(GLmake_bool(),
						   GLmake_bool())),
			bddOR);
    Insert_infix("OR", 3);

    Add_ExtAPI_Function("XOR", "11", FALSE,
			GLmake_arrow(GLmake_bool(),
				      GLmake_arrow(GLmake_bool(),
						   GLmake_bool())),
			bddXOR);
    Insert_infix("XOR", 4);

    Add_ExtAPI_Function("XNOR", "11", FALSE,
			GLmake_arrow(GLmake_bool(),
				      GLmake_arrow(GLmake_bool(),
						   GLmake_bool())),
			bddXNOR);
    Insert_infix("XNOR", 4);

    Add_ExtAPI_Function("limitedAND", "111", FALSE,
			GLmake_arrow(
			    GLmake_bool(),
			    GLmake_arrow(
			      GLmake_bool(),
			      GLmake_arrow(GLmake_int(),
					   GLmake_bool()))),
			limitedAND);

    Add_ExtAPI_Function("limitedOR", "111", FALSE,
			GLmake_arrow(
			    GLmake_bool(),
			    GLmake_arrow(
			      GLmake_bool(),
			      GLmake_arrow(GLmake_int(),
					   GLmake_bool()))),
			limitedOR);

    Add_ExtAPI_Function("draw_bdds", "11", FALSE,
                        GLmake_arrow(GLmake_bool(),
				     GLmake_arrow(GLmake_list(GLmake_bool()),
						  GLmake_void())),
                        draw_bdds);

    Add_ExtAPI_Function("bv2num", "1", FALSE,
                        GLmake_arrow(GLmake_list(GLmake_bool()), GLmake_int()),
                        bv2num);

    Add_ExtAPI_Function("top_cofactor", "1", FALSE,
                        GLmake_arrow(GLmake_bool(), 
				     GLmake_tuple(GLmake_string(),
						  GLmake_tuple(GLmake_bool(),
							       GLmake_bool()))),
                        top_cofactor);

    typeExp_ptr tv = GLnew_tVar();
    Add_ExtAPI_Function("substitute", "11", FALSE,
            GLmake_arrow(
                GLmake_list(GLmake_tuple(GLmake_string(),GLmake_bool())),
                GLmake_arrow(tv, tv)),
            do_substitute);

    tv = GLnew_tVar();
    Add_ExtAPI_Function("depends", "1", FALSE,
			GLmake_arrow(tv, GLmake_list(GLmake_string())),
			depends);

    tv = GLnew_tVar();
    Add_ExtAPI_Function("profile", "1", FALSE,
			GLmake_arrow(tv,
				     GLmake_list(
					GLmake_tuple(GLmake_string(),
						     GLmake_int()))),
			bdd_profile);

    tv = GLnew_tVar();
    Add_ExtAPI_Function("bdd_size", "1", FALSE,
			GLmake_arrow(tv, GLmake_int()),
			bdd_size);

    Add_ExtAPI_Function("truth_cover", "11", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_arrow(GLmake_bool(),GLmake_int())),
			do_truth_cover);

}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/


static void
grow_MainTbl()
{
    bdd_ptr	new;
    bdd_ptr	otop;
    lunint	osize, i;

    FP(warning_fp, "Growing the main bdd table to %ld nodes.",2*sz_MainTbl);
    osize = sz_MainTbl;
    sz_MainTbl = 2*sz_MainTbl;
    MainTbl = (bdd_ptr) Realloc((pointer) MainTbl, sz_MainTbl*sizeof(bdd_rec));
    b_free_list = LNULL;
    i = sz_MainTbl-1;
    new  = (bdd_ptr) (MainTbl + i);
    otop = (bdd_ptr) (MainTbl + osize);
    while( new >= otop ) {
	new->next = b_free_list;
#ifdef REF_CNT_DEBUG
	new->in_use = 0;
#endif
	new->mark = 0;
	new->sz_mark = 0;
	new->moving = 0;
	new->ref_cnt = 0;
	b_free_list = i;
	new--;
	i--;
    }
    FP(warning_fp, " Done.\n");
}

static void
grow_uniq_tbl(var_ptr vp)
{
    formula	*new_uniq_tbl;
    unint	new_sz_uniq_tbl;
    unint	new_hash;
    unint	i = 1;
    bdd_ptr	bp;

    if( dont_grow_uniq_tbl ) {
	want_to_grow_uniq_tbl = TRUE;
	return;
    }
    while( primes[i] <= vp->sz_uniq_tbl )
	i++;
    new_sz_uniq_tbl = primes[i];
    new_uniq_tbl = (formula *) Malloc(new_sz_uniq_tbl * sizeof(formula));
    for(i = 0; i < new_sz_uniq_tbl; i++) {
	*(new_uniq_tbl+i) = LNULL;
    }
    /* Rehash the old table */
    for(i = 0; i < vp->sz_uniq_tbl; i++) {
	/* For each old hash chain */
	formula cur;
	cur  = *(vp->uniq_tbl + i);
	while( cur != LNULL ) {
	    unint ncur;
	    /* Insert bdd node in new table */
	    bp = GET_BDDP(cur);
	    ncur = GET_NEXT(bp);
	    new_hash = uniq_hash_fn(GET_LSON(bp), GET_RSON(bp),new_sz_uniq_tbl);
	    bp->next = *(new_uniq_tbl+new_hash);
	    *(new_uniq_tbl+new_hash) = cur;
	    cur = ncur;
	}
    }
    Free((pointer) (vp->uniq_tbl));
    vp->uniq_tbl = new_uniq_tbl;
    vp->sz_uniq_tbl = new_sz_uniq_tbl;
}

void
PR_uniq_tbl(var_ptr vp)
{
    unint	i;
    bdd_ptr	bp;
    formula	cur;

    for(i = 0; i < vp->sz_uniq_tbl; i++) {
	/* For each old hash chain */
	cur  = *(vp->uniq_tbl + i);
	if( cur != LNULL ) {
	    FP(err_fp, "\tuniq_tbl[%d]: ", i);
	    while( cur != LNULL ) {
		bp = GET_BDDP(cur);
		FP(err_fp, " (%d = %d %s %d)", cur, GET_LSON(bp),
				vp->var_name, GET_RSON(bp));
		cur = GET_NEXT(bp);
	    }
	    FP(err_fp, "\n");
	}
    }
}

static formula
find_insert_bdd(var_ptr vp, formula lson, formula rson)
{
    unint		hash;
    formula		cur;
    formula		ret;
    unint		neg, var;
    bdd_ptr		bp;

#ifdef REF_CNT_DEBUG
    ASSERT( GET_BDDP(lson)->in_use );
    ASSERT( GET_BDDP(rson)->in_use );
#endif
    CHECK_FOR_INTERRUPT;
    if( lson == rson ) {
	return(lson);
    }
    if( ISNOT(lson) ) {
        neg = 1;
        lson = NOT(lson);
        rson = NOT(rson);
    } else
        neg = 0;

    var  = vp-VarTbl;
    hash = uniq_hash_fn(lson, rson, vp->sz_uniq_tbl);
    cur  = *(vp->uniq_tbl + hash);
    while( cur != LNULL ) {
	bp   = GET_BDDP(cur);
	if(GET_LSON(bp) == lson && GET_RSON(bp) == rson ) {
	    ASSERT(BDD_GET_VAR(bp) == var);
	    /* Node already exists */
	    return( neg? NOT(cur): cur );
	}
	cur = GET_NEXT(bp);
    }
    /* Not in table */
    if( b_free_list == LNULL )
	grow_MainTbl();

    bp = GET_BDDP(b_free_list);
    b_free_list = bp->next;
#ifdef REF_CNT_DEBUG
    bp->in_use = 1;
#endif
    bp->ref_cnt = 0;
    bp->mark = 0;
    bp->sz_mark = 0;
    bp->moving = 0;
    BDD_SET_VAR(bp,var);
    bp->lson = lson;
    inc_ref_cnt(lson);
    bp->rson = rson;
    inc_ref_cnt(rson);
    bp->next = *(vp->uniq_tbl + hash);
    ret = bp - MainTbl;
    *(vp->uniq_tbl + hash) = ret;
    nodes_used++;
    (vp->nbr_uniq_tbl)++;
    if( nodes_used > gc_limit )
	Do_gc_asap = 1;
    if( LOAD_FACTOR * vp->sz_uniq_tbl < vp->nbr_uniq_tbl )
	grow_uniq_tbl(vp);
    return( neg? NOT(ret): ret );
}

static formula
bdd_step(int fn, formula f1, formula f2)
{
    cache_ptr	cp;
    formula	lson1, rson1, lson2, rson2, ret, pf1, pf2;
    unint	f1_var, f2_var;
    var_ptr	vp;
    bdd_ptr	f1p, f2p;

    /* Find simple identities */
    pf1 = POS(f1);
    pf2 = POS(f2);
    if( pf1 == pf2 ) {
	/* f1=f2 or f1=NOT(f2) */
	if( f1 == f2 ) {
	    if( fn == XOR )
		BDD_RETURN( ZERO );
	    BDD_RETURN( f1 );
	} else {
	    if( fn == AND )
		BDD_RETURN( ZERO );
	    BDD_RETURN( ONE );
	}
    }
    if( !pf1 || !pf2 ) {
	/* One of f1 and f2 is ONE or ZERO */
	switch( fn ) {
	    case AND:
		if( f1 == ONE )
		    BDD_RETURN( f2 );
		if( f2 == ONE )
		    BDD_RETURN( f1 );
		ASSERT( f1 == ZERO || f2 == ZERO );
		BDD_RETURN( ZERO );
		break;
	    case OR:
		if( f1 == ZERO )
		    BDD_RETURN( f2 );
		if( f2 == ZERO )
		    BDD_RETURN( f1 );
		ASSERT( f1 == ONE || f2 == ONE );
		BDD_RETURN( ONE );
		break;
	    case XOR:
		if( f1 == ZERO )
		    BDD_RETURN( f2 );
		if( f2 == ZERO )
		    BDD_RETURN( f1 );
		if( f1 == ONE )
		    BDD_RETURN( NOT(f2) );
		ASSERT( f2 == ONE );
		BDD_RETURN( NOT(f1) );
		break;
	    default:
		DIE("Should not happen.");
	}
    }

    /* Check cache */
    cache_rec cr;
    cr.fn = fn;
    cr.u.args.lson = f1;
    cr.u.args.rson = f2;
    if( (cp = (cache_ptr) find_hash(&cache_tbl, &cr)) != NULL ) {
	BDD_RETURN(cp->res);
    }

    f1p = FGET_BDDP(pf1);
#ifdef REF_CNT_DEBUG
    ASSERT( f1p->in_use );
#endif
    f1_var = BDD_GET_VAR(f1p);
    f2p = FGET_BDDP(pf2);
#ifdef REF_CNT_DEBUG
    ASSERT( f2p->in_use );
#endif
    f2_var = BDD_GET_VAR(f2p);

    if( ORDER(f1_var) > ORDER(f2_var) ) {
	lson1 = f1;
	rson1 = f1;
	vp = VarTbl + f2_var;
    } else {
	if( ISNOT(f1) ) {
	    lson1 = NOT(GET_LSON(f1p));
	    rson1 = NOT(GET_RSON(f1p));
	} else {
	    lson1 = GET_LSON(f1p);
	    rson1 = GET_RSON(f1p);
	}
	vp = VarTbl + f1_var;
    }
    if( ORDER(f2_var) > ORDER(f1_var) ) {
	lson2 = f2;
	rson2 = f2;
    } else {
	if( ISNOT(f2) ) {
	    lson2 = NOT(GET_LSON(f2p));
	    rson2 = NOT(GET_RSON(f2p));
	} else {
	    lson2 = GET_LSON(f2p);
	    rson2 = GET_RSON(f2p);
	}
    }
    lson1 = bdd_step(fn, lson1, lson2);
    rson1 = bdd_step(fn, rson1, rson2);
    if( lson1 == rson1 )
	ret = lson1;
    else
	ret = find_insert_bdd(vp, lson1, rson1);

    /* Insert into cache */
    cp = get_new_cache_rec();
    cp->fn = fn;
    cp->u.args.lson = f1;
    cp->u.args.rson = f2;
    cp->res = ret;
    insert_hash(&cache_tbl, cp, cp);
    BDD_RETURN( ret );
}

static formula
quantify(formula f, formula vf, int type)
{
    cache_ptr   cp;
    formula     ret;
    unint       index, cur;
    var_ptr     vp;

    if( vf == ONE || vf == ZERO )
	BDD_RETURN( f );
    if( f == ZERO || f == ONE)
        BDD_RETURN( f );

    /* Check cache */
    cache_rec cr;
    cr.fn = type;
    cr.u.args.lson = f;
    cr.u.args.rson = vf;
    if( (cp = (cache_ptr) find_hash(&cache_tbl, &cr)) != NULL ) {
	BDD_RETURN(cp->res);
    }
    index = f2var(vf);
    cur = f2var(f);

    if( ORDER(cur) > ORDER(index) ) {
	ret = quantify(f, f2lson(vf), type);
    } else {
	if( ORDER(cur) < ORDER(index) ) {
	    vp = VarTbl + cur;
	    if( ISNOT(f) ) {
		ret = find_insert_bdd(vp, quantify(NOT(f2lson(f)),vf, type),
					  quantify(NOT(f2rson(f)),vf, type));
	    } else {
		ret = find_insert_bdd(vp, quantify(f2lson(f), vf, type),
					  quantify(f2rson(f), vf, type));
	    }
	} else {
	    formula lson, rson, new;
	    if( ISNOT(f) ) {
		lson = NOT(f2lson(f));
		rson = NOT(f2rson(f));
	    } else {
		lson = f2lson(f);
		rson = f2rson(f);
	    }
	    new = f2lson(vf);
	    if( type == UNIVERSAL )
		ret = bdd_step(AND, quantify(lson, new, type),
				    quantify(rson, new, type));
	    else
		ret = bdd_step(OR, quantify(lson, new, type),
				   quantify(rson, new, type));
	}
    }
    /* Insert into cache */
    cp = get_new_cache_rec();
    cp->fn = type;
    cp->u.args.lson = f;
    cp->u.args.rson = vf;
    cp->res = ret;
    insert_hash(&cache_tbl, cp, cp);
    BDD_RETURN( ret );
}


static void
bdd_tree_print(odests fp, formula f)
{
    if( f == ONE ) {
	FP(fp, "T");
	return;
    }
    if( f == ZERO ) {
	FP(fp, "F");
	return;
    }
    FP(fp, "(");
    if( ISNOT(f) )
	FP(fp, "*");

    FP(fp, "%s ", get_var_name(f));
    bdd_tree_print(fp, f2lson(f));
    FP(fp, " ");
    bdd_tree_print(fp, f2rson(f));
    FP(fp, ")");
}

static void
bdd_infix_print(odests fp, formula f)
{
    if( f == ONE ) {
	FP(fp, "T");
	return;
    }
    if( f == ZERO ) {
	FP(fp, "F");
	return;
    }
    if( ISNOT(f) )
	FP(fp, " NOT(");
    else
	FP(fp, "(");
    FP(fp, "%s => ", get_var_name(f));
    bdd_infix_print(fp, f2lson(f));
    FP(fp, " | ");
    bdd_infix_print(fp, f2rson(f));
    FP(fp, ")");
}

static void
garbage_collect()
{
    lunint 	i, j;
    lunint	before;
    bdd_ptr     bp;
    bool	erase;
    void	(*old_handler)();

    old_handler = signal(SIGINT, SIG_IGN);
    if( RCverbose_GC ) {
	FP(bdd_gc_fp, "\nStart bdd garbage collection.\n");
	FP(bdd_gc_fp, "Start with: %d bdd nodes in use\n", nodes_used);
    }
    before = nodes_used;

    /* For each variable uniq table */
    for(i = 0; i < nbr_VarTbl; i++) {
	/* For each variable */
	var_ptr	vp;
	vp = VarTbl + i;
	/* For each BDD node in the table  */
	for(j = 0; j < vp->sz_uniq_tbl; j++) {
	    formula	cur, ncur, prev;
	    cur = *(vp->uniq_tbl+j);
	    while( cur != LNULL ) {
		bp = GET_BDDP(cur);
		ncur = GET_NEXT(bp);
		if( bp->mark == 0 ) {
		    *(vp->uniq_tbl+j) = ncur;
#ifdef REF_CNT_DEBUG
		    bp->in_use = 0;
#endif
		    bp->next = b_free_list;
		    b_free_list = cur;
		    nodes_used--;
		    (vp->nbr_uniq_tbl)--;
		    cur = ncur;
		} else
		    break;
	    }
	    if( cur != LNULL ) {
		prev = cur;
		cur = ncur;
	    }
	    while( cur != LNULL ) {
		bp = GET_BDDP(cur);
		ncur = GET_NEXT(bp);
		if( bp->mark == 0 ) {
		    bdd_ptr pbp;
		    pbp = GET_BDDP(prev);
		    pbp->next = ncur;
#ifdef REF_CNT_DEBUG
		    bp->in_use = 0;
#endif
		    bp->next = b_free_list;
		    b_free_list = cur;
		    nodes_used--;
		    (vp->nbr_uniq_tbl)--;
		} else {
		    prev = cur;
		}
		cur = ncur;
	    }
	}
    }

    if(RCdo_dynamic_var_order &&
       (((nodes_used > before/DYN_VAR_RED) &&
	 (nodes_used > (uint) RCdynamic_ordering_threshold))
	|| insist_on_reorder) )
    {
	if( user_defined_ordering_flag )
	    user_defined_reorder();
	else
	    re_order();
	erase = TRUE;	/* Must wipe out cache since nodes may have moved */
	RCdynamic_ordering_threshold = nodes_used + nodes_used/2;
    } else {
	erase = FALSE;
    }

    /* Clean the cache */
    clean_cache(erase);

#if 0
    /* Reset external references */
    for(i = 0; i < sz_MainTbl; i++) {
	bp = MainTbl+i;
	if(bp->mark) {
	    if( bp->ref_cnt < BDD_MAX_REF_CNT )
		(bp->ref_cnt)--;
	    bp->mark = 0;
	}
    }
#endif

    
    static lunint limit1, limit2;
    limit1 = 2*nodes_used;
    limit2 = (lunint) sz_MainTbl/2;
    gc_limit = (limit1 > limit2)? limit1 : limit2;

    if( RCverbose_GC ) {
	FP(bdd_gc_fp, "Finished bdd garbage collection.\n");
	FP(bdd_gc_fp, "Currently: %d bdd nodes in use\n", nodes_used);
    }
    signal(SIGINT, old_handler);
}

static void
clean_cache_rec_fn(pointer key, pointer data)
{
    cache_ptr	cp = (cache_ptr) key;
    (void) data;
    bool	remove = FALSE;
    bdd_ptr	bp = GET_BDDP(cp->u.args.lson);
    if( bp->ref_cnt < 1 )
	remove = TRUE;
    bp = GET_BDDP(cp->u.args.rson);
    if( bp->ref_cnt < 1 )
	remove = TRUE;
    bp = GET_BDDP(cp->res);
    if( bp->ref_cnt < 1 )
	remove = TRUE;
    if( remove ) {
	delete_hash(&cache_tbl, cp);
	cp->u.next = cache_free_list;
	cache_free_list = cp;
    }
}

static void
clean_cache(bool erase)
{
    if( erase ) {
	dispose_hash(&cache_tbl, NULLFCN);
	create_hash(&cache_tbl, sz_MainTbl/10, cache_hash, cache_equ);
    } else {
	scan_hash(&cache_tbl, clean_cache_rec_fn);
    }
}

static formula
print_prod(odests fp, formula f)
{
    formula	 *ffp, ret, check;
    bool	 *up;
    bool 	 first = TRUE;

    /* Check whether term is needed */
    check = ONE;
    for(up = used_prods, ffp = prod; ffp < cur_prod; up++, ffp++) {
	check = B_And(check, *ffp);
	*up = TRUE;
    }
    if( B_Or(NOT(check), fn_printed) == ONE ) {
	return( ZERO );
    }

    /* Print as small term as possible */
    ret = ONE;
    for(up = used_prods, ffp = prod; ffp < cur_prod; up++, ffp++) {
	formula	*tmp, without;
	bool	*up2;
	without = ONE;
	for(up2 = used_prods, tmp = prod; tmp < cur_prod; up2++, tmp++) {
	    if( tmp != ffp && *up2 )
		without = B_And(without, *tmp);
	}
	if( B_Or(NOT(without), f) != ONE ) {
	    if( first ) {
		first = FALSE;
		if( prods_printed )
		    FP(fp, " + ");
	    } else {
		FP(fp, "&");
	    }
	    ret = B_And(ret, *ffp);
	    FP(fp, "%s%s", get_var_name(*ffp), ISNOT(*ffp)? "":"\'");
	} else {
	    *up = FALSE;
	}
    }
    return( ret );
}

static VOID
sop_print(odests fp, formula f)
{
    if( f == ONE ) {
	FP(fp, "T");
	return;
    }
    if( f == ZERO ) {
	FP(fp, "F");
	return;
    }
    prods_printed = 0;
    cur_prod = prod;
    fn_printed = ZERO;
    if( sop_pr_rec(fp, POS(f), ISNOT(f), f) == FALSE )
	FP(fp, " OR ... ");
}

static bool
sop_pr_rec(odests fp, formula node, bool look_for_zero, formula f)
{
    formula	*old;

    if( node == ONE ) {
	if( !look_for_zero ) {
	    fn_printed = B_Or(fn_printed, print_prod(fp, f));
	    prods_printed++;
	}
	return( TRUE );
    }
    if( node == ZERO ) {
	if( look_for_zero ) {
	    fn_printed = B_Or(fn_printed, print_prod(fp, f));
	    prods_printed++;
	}
	return( TRUE );
    }
    if( prods_printed >= RCmax_prods_to_print )
	return( FALSE );

    *cur_prod = B_Var(get_var_name(node));
    cur_prod++;
    old = cur_prod;

    if( !sop_pr_rec(fp, POS(f2lson(node)),
			look_for_zero ^ ISNOT(f2lson(node)), f))
	return( FALSE );
    cur_prod = old;
    *(cur_prod-1) = NOT(*(cur_prod-1));
    if( !sop_pr_rec(fp, POS(f2rson(node)),
			look_for_zero ^ ISNOT(f2rson(node)), f))
	return( FALSE );
    cur_prod = old;
    return( TRUE );
}

string
get_var_name(formula f)
{
    bdd_ptr fp;
    fp = GET_BDDP(f);
    return( (VarTbl+BDD_GET_VAR(fp))->var_name );
}

static int
var_ord_comp(const void *pi, const void *pj)
{
    int *i = (int *) pi;
    int *j = (int *) pj;
    return( (VarTbl+*i)->variable - (VarTbl+*j)->variable );
}

void
reorder_break_handler()
{
    int c;

    if( quit_early ) return;

    if( gui_mode ) {
	if( use_stdout ) FP(err_fp, "\n\n---- Re-ordering interrupted ----\n");
	quit_early = TRUE;
	return;
    } else {
	FP(err_fp, "\n\n---- Re-ordering interrupted ----\n");
	while( 1 ) {
	    FP(err_fp, "\n\tContinue or quit re-ordering [C,Q]?");
	    c = getchar();
	    switch( c ) {
		    case 'q':
		    case 'Q':
			    quit_early = TRUE;
			    return;
		    case 'c':
		    case 'C':
			    return;
		    default:
			    break;
	    }
	}
    }
}

static void
re_order()
{
    unint   i, pos, optimal_size, optimal_location, start_cnt, j, last;
    var_ptr vp;
    buffer  rev_table;
    unint   len, line_len = 0;
    bool    first = TRUE;
    void    (*old_handler)();


    if( RCverbose_dynamic_ordering ) {
	if( gui_mode ) {
            Sprintf(buf, "reorder_start %d %ld", nbr_VarTbl, nodes_used);
            Info_to_tcl(buf);
	} else {
	    FP(bdd_gc_fp, "\nBegin re-ordering of BDD variables\n");
	}
    }
    start_cnt = nodes_used;
    dont_grow_uniq_tbl = TRUE;
    quit_early = FALSE;

    old_handler = signal(SIGINT, reorder_break_handler);

    /* Create a reverse index for the variable ordering */
    /* rev_table maps order[v] -> variable location	*/
    new_buf(&rev_table, nbr_VarTbl, sizeof(int));
    for(i = 0; i < nbr_VarTbl; i++) {
	push_buf(&rev_table, (pointer) &i);
    }
    qsort(START_BUF(&rev_table), nbr_VarTbl, sizeof(int), var_ord_comp);

    for(j = 0; j < (unint) RCdyn_var_repetitions; j++) {
	bool done = FALSE;

	if( quit_early ) break;

	/* Reset the done flags */
	for(i = 0; i < nbr_VarTbl; i++) {
	    vp = VarTbl + i;
	    vp->done = 0;
	}
	/* Force at least one re-ordering */
	last = nodes_used + RCminimum_reduction+2;

	while( !done && !quit_early) {
	    unint  max = 0;
	    done = TRUE;

	    /* Find the widest BDD node left to place */
	    if( (RCoptimal_dynamic_var_order &&
	         (nodes_used >= (unint) RCminsize_for_dyn_ordering))
	       ||
	       (nodes_used > (start_cnt/DYN_VAR_RED) &&
	       (last - nodes_used) > (unint) RCminimum_reduction) ) {
		for(i = 0; i < nbr_VarTbl; i++) {
		    vp = VarTbl + i;
		    if( !vp->done ) {
			if( vp->nbr_uniq_tbl >= max ) {
			    max = vp->nbr_uniq_tbl;
			    pos = (VarTbl+i)->variable;
			    done = FALSE;
			}
		    }
		}
	    }
	    if( done || quit_early )
		break;
	    last = nodes_used;

	    /* Make sure pos points to a current-state variable */
	    pos = 2*(pos/2);


	    /* Find optimal location for variable pos */
	    /* Both regular and next-state variables are done after this */
	    i = *((int *)M_LOCATE_BUF(&rev_table, pos+1));
	    vp = VarTbl + i;
	    vp->done = 1;
	    i = *((int *)M_LOCATE_BUF(&rev_table, pos));
	    vp = VarTbl + i;
	    vp->done = 1;

	    optimal_size = nodes_used;
	    optimal_location = pos;

	    if( RCverbose_dynamic_ordering ) {
		if( gui_mode ) {
		    Sprintf(buf, "reorder_update %ld", nodes_used);
		    Info_to_tcl(buf);
		} else {
		    FP(bdd_gc_fp, "variable %s with width %u\n",
			    vp->var_name, vp->nbr_uniq_tbl);
		    FP(bdd_gc_fp, "Size: %u\n", nodes_used);
		}
	    }

	    /* Sift down */
	    while( (pos < nbr_VarTbl-2) && !quit_early ) {
		do_swap_down(&rev_table, pos);
		pos += 2;
		if( nodes_used > RCelasticity*optimal_size )
		    break;
		if( nodes_used < optimal_size ) {
		    optimal_size = nodes_used;
		    optimal_location = pos;
		}
	    }

	    /* Sift up */
	    while( (pos > 0) && !quit_early ) {
		pos -= 2;
		do_swap_down(&rev_table, pos);
		if( nodes_used > RCelasticity*optimal_size )
		    break;
		if( nodes_used <= optimal_size ) {
		    optimal_size = nodes_used;
		    optimal_location = pos;
		}
	    }

	    /* Sift to final location */
	    if( pos > optimal_location ) {
		while( pos > optimal_location ) {
		    pos -= 2;
		    do_swap_down(&rev_table, pos);
		}
	    } else {
		while( pos < optimal_location ) {
		    do_swap_down(&rev_table, pos);
		    pos += 2;
		}
	    }
	}
    }

    /* Finished re-ordering */
    fprintf(v_order_fp, "var_order [\n\t");
    for(pos = 0; pos < nbr_VarTbl; pos++) {
	i = *((int *)M_LOCATE_BUF(&rev_table, pos));
	vp = VarTbl+i;
	if( !first )
	    fprintf(v_order_fp, ",");
	else
	    first = FALSE;
	line_len++;
	len = strlen(vp->var_name);
	if( len + line_len > 70 ) {
	    fprintf(v_order_fp, "\n\t");
	    line_len = 9;
	}
	line_len = line_len+len+3;
	fprintf(v_order_fp, " \"%s\"", vp->var_name);
    }
    fprintf(v_order_fp, "];\n");
    free_buf(&rev_table);
    dont_grow_uniq_tbl = FALSE;
    signal(SIGINT, old_handler);
    if( RCverbose_dynamic_ordering ) {
	if( gui_mode ) {
	    Sprintf(buf, "reorder_end");
	    Info_to_tcl(buf);
	} else {
	    FP(bdd_gc_fp, "\nCompleted re-ordering of BDD variables\n");
	}
    }
}

static void
user_defined_reorder()
{
    unint   i, pos;
    buffer  rev_table;
    int	    len, line_len = 0;
    bool    first = TRUE;

    dont_grow_uniq_tbl = TRUE;

    /* Create a reverse index for the variable ordering */
    new_buf(&rev_table, nbr_VarTbl, sizeof(int));
    for(i = 0; i < nbr_VarTbl; i++) {
	push_buf(&rev_table, (pointer) &i);
    }
    qsort(START_BUF(&rev_table), nbr_VarTbl, sizeof(int), var_ord_comp);

    for(i = 0; i < COUNT_BUF(&new_order_tbl); i++) {
	unint next;
	next = *((int *)M_LOCATE_BUF(&new_order_tbl,i));
	pos = ORDER(next);
	/* Sift up until node is in position i */
	while( (pos > i) ) {
	    pos -= 2;
	    do_swap_down(&rev_table, pos);
	}
    }

    /* Finished re-ordering */
    g_ptr tail = new_var_order_list;
    fprintf(v_order_fp, "var_order [\n\t");
    for(pos = 0; pos < nbr_VarTbl; pos++) {
	var_ptr vp;
	i = *((int *)M_LOCATE_BUF(&rev_table, pos));
	vp = VarTbl+i;
	if( !is_next_state_var(vp->var_name) ) {
	    if( !first )
		fprintf(v_order_fp, ",");
	    else
		first = FALSE;
	    line_len++;
	    len = strlen(vp->var_name);
	    if( len + line_len > 70 ) {
		fprintf(v_order_fp, "\n\t");
		line_len = 9;
	    }
	    line_len = line_len+len+3;
	    fprintf(v_order_fp, " \"%s\"", vp->var_name);
	    tail = Append_string_to_tail(tail, vp->var_name);
	}
    }
    fprintf(v_order_fp, "];\n");
    free_buf(&rev_table);
    free_buf(&new_order_tbl);
    dispose_hash(&new_order_htbl, NULLFCN);
    dont_grow_uniq_tbl = FALSE;
    user_defined_ordering_flag = FALSE;
}

static void
do_swap_down(buffer *rev_tablep, unint pos)
{
    /* Swap the current+next-state variables together */
    swap_down(rev_tablep, pos+1);
    swap_down(rev_tablep, pos+2);
    swap_down(rev_tablep, pos);
    swap_down(rev_tablep, pos+1);
}

static void
swap_down(buffer *rev_tablep, unint pos)
{
    var_ptr	vp, nvp;
    unint	i, new_var, hash;
    formula 	cur, ncur;
    formula	lson, rson, nlson, nrson;
    bdd_ptr	bp;
    bool	neg;

    unint level = *((int *)M_LOCATE_BUF(rev_tablep, pos));
    unint new_level = *((int *)M_LOCATE_BUF(rev_tablep, pos+1));

    vp = VarTbl + level;
    new_var = new_level;
    nvp = VarTbl+new_var;

    /* Mark the nodes that will be moved */
    for(i = 0; i < vp->sz_uniq_tbl; i++) {
	cur = *(vp->uniq_tbl+i);
	while( cur != LNULL ) {
	    bp = GET_BDDP(cur);
	    ASSERT( bp->ref_cnt > 0 );
	    bp->moving = 1;
	    cur = GET_NEXT(bp);
	}
    }

    for(i = 0; i < vp->sz_uniq_tbl; i++) {
	cur = *(vp->uniq_tbl+i);
	while( cur != LNULL ) {
	    bp = GET_BDDP(cur);
	    ASSERT( bp->ref_cnt > 0 );
	    ncur = GET_NEXT(bp);
	    if( bp->moving ) {
		bp->moving = 0;
		lson = GET_LSON(bp);
		rson = GET_RSON(bp);
		if( ISNOT( rson ) )
		    neg = 1;
		else
		    neg = 0;
		if( POS(lson) != ZERO && (f2var(lson) == new_var) ) {
		    if( POS(rson) != ZERO && (f2var(rson) == new_var) ) {
			/*					*/
			/*         x                  y     	*/
			/*        / \                / \    	*/
			/*       /   \              /   \   	*/
			/*      y     y    ==>     x     x  	*/
			/*     / \   / \          / \   / \ 	*/
			/*    A   B C   D        A   C B   D	*/
			/*					*/
			nlson = find_insert_bdd(vp, f2lson(lson),
						    neg? NOT(f2lson(rson)):
							     f2lson(rson));
			nrson = find_insert_bdd(vp, f2rson(lson),
						    neg? NOT(f2rson(rson)):
							     f2rson(rson));
		    } else {
			/*					*/
			/*         x                  y     	*/
			/*        / \                / \    	*/
			/*       /   \              /   \   	*/
			/*      y     \    ==>     x     x  	*/
			/*     / \     \          / \   / \ 	*/
			/*    A   B     C        A   C B   C	*/
			/*					*/
			nlson = find_insert_bdd(vp, f2lson(lson), rson);
			nrson = find_insert_bdd(vp, f2rson(lson), rson);
		    }
		} else {
		    if( POS(rson) != ZERO && (f2var(rson) == new_var) ) {
			/*					*/
			/*         x                  y     	*/
			/*        / \                / \    	*/
			/*       /   \              /   \   	*/
			/*      /     y    ==>     x     x  	*/
			/*     /     / \          / \   / \ 	*/
			/*    A     B   C        A   B A   C	*/
			/*					*/
			nlson = find_insert_bdd(vp, lson,
						    neg? NOT(f2lson(rson)):
							     f2lson(rson));
			nrson = find_insert_bdd(vp, lson,
						    neg? NOT(f2rson(rson)):
							     f2rson(rson));
		    } else {
			/* Does not depend on new_var		*/
			/*					*/
			/*         x                  x     	*/
			/*        / \                / \    	*/
			/*       /   \              /   \   	*/
			/*      /     \    ==>     /     \  	*/
			/*     /       \          /       \ 	*/
			/*    A         B        A         B	*/
			/*					*/
			goto end_loop;
		    }
		}
		/* Get fresh pointer in case MainTbl was moved */
		bp = GET_BDDP(cur);
		/* Convert node to new_var level */
		BDD_SET_VAR(bp, new_var);
		ASSERT( nlson != nrson );
		bp->lson = nlson;
		bp->rson = nrson;
		hash = uniq_hash_fn(nlson, nrson, nvp->sz_uniq_tbl);
		bp->next = *(nvp->uniq_tbl + hash);
		*(nvp->uniq_tbl + hash) = cur;
		(nvp->nbr_uniq_tbl)++;
		(vp->nbr_uniq_tbl)--;

		/* Remove the node from uniq_tbl at current level */
		if( *(vp->uniq_tbl+i) == cur )
		    *(vp->uniq_tbl+i) = ncur;
		else {
		    formula prev, tcur;
		    tcur = *(vp->uniq_tbl+i);
		    do {
			prev = tcur;
			bp   = GET_BDDP(prev);
			tcur = GET_NEXT(bp);
			ASSERT( tcur != LNULL );
		    } while ( tcur != cur );
		    bp->next = ncur;
		}
		/* Update the reference counters */
		inc_ref_cnt(nlson);
		inc_ref_cnt(nrson);
		Bdec_ref_cnt(lson);
		Bdec_ref_cnt(rson);
	    }
	  end_loop:
	    cur = ncur;
        }
    }
    /* Swap the variable orderings */
    i = vp->variable;
    vp->variable = nvp->variable;
    nvp->variable = i;
    store_buf(rev_tablep, pos, (pointer) &new_level);
    store_buf(rev_tablep, pos+1, (pointer) &level);
}

static unint
f2var(formula f)
{
    bdd_ptr bp;
    bp = GET_BDDP(f);
    return( BDD_GET_VAR(bp) );
}

static formula
f2lson(formula f)
{
    bdd_ptr bp;
    bp = GET_BDDP(f);
    return( GET_LSON(bp) );
}

static formula
f2rson(formula f)
{
    bdd_ptr bp;
    bp = GET_BDDP(f);
    return( GET_RSON(bp) );
}

static void
inc_ref_cnt(formula f)
{
    bdd_ptr bp = GET_BDDP(f);
    if( bp->ref_cnt < BDD_MAX_REF_CNT) {
	(bp->ref_cnt)++;
    }
}

static void
Bdec_ref_cnt(formula f)
{
    formula 	lson, rson, cur, prev;
    var_ptr	vp;
    bdd_ptr	np;
    bdd_ptr	bp;
    unint	hash;

    if( f == ONE || f == ZERO ) 
	return;

    bp = GET_BDDP(f);

    ASSERT( bp->ref_cnt > 0 );
    if( bp->ref_cnt == BDD_MAX_REF_CNT ) {
	return;
    }
    (bp->ref_cnt)--;
    if( bp->ref_cnt > 0 )
	return;

    /* Free the DAG */
    lson = GET_LSON(bp);
    Bdec_ref_cnt(lson);
    rson = GET_RSON(bp);
    Bdec_ref_cnt(rson);

    /* Remove the node */
    vp   = VarTbl + BDD_GET_VAR(bp);
    hash = uniq_hash_fn(lson, rson, vp->sz_uniq_tbl);
    cur  = *(vp->uniq_tbl + hash);
    np   = GET_BDDP(cur);
    if( np == bp ) {
	*(vp->uniq_tbl+hash) = GET_NEXT(bp);
	bp->next = b_free_list;
	b_free_list = cur;
	nodes_used--;
	(vp->nbr_uniq_tbl)--;
	return;
    }
    prev = cur;
    while( cur != LNULL ) {
        np = GET_BDDP(cur);
	if( np == bp ) {
	    bdd_ptr pbp;
	    pbp = GET_BDDP(prev);
	    pbp->next = GET_NEXT(bp);
	    bp->next = b_free_list;
	    b_free_list = cur;
	    nodes_used--;
	    (vp->nbr_uniq_tbl)--;
	    return;
	}
	prev = cur;
	cur = GET_NEXT(np);
    }
    DIE("Should never happen");
}

static unsigned int
subst_hash(pointer p, unsigned int n)
{
    subst_cache_ptr sp;
    sp = (subst_cache_ptr) p;
    return( (((unint) (sp->f)) + ((lunint) (sp->subs))) % n );
}

static bool
subst_eq(pointer p1, pointer p2)
{
    subst_cache_ptr sp1, sp2;
    sp1 = (subst_cache_ptr) p1;
    sp2 = (subst_cache_ptr) p2;
    return( sp1->f == sp2->f && sp1->subs == sp2->subs );
}

static relprod_cache_ptr
find_in_relprod_cache(formula v, formula a, formula b)
{
    unint idx;
    idx = (((unint) v) + ((unint) a) + ((unint) b) ) % relprod_cache_sz;
    return( relprod_cache + idx );
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


static cache_ptr
get_new_cache_rec()
{
    if( cache_free_list != NULL ) {
	cache_ptr res = cache_free_list;
	cache_free_list = cache_free_list->u.next;
	return res;
    }
    cache_ptr res = (cache_ptr) new_rec(&cache_rec_mgr);
    return res;
}

static unint
cache_hash(pointer p, unint n)
{
    cache_ptr	cp = (cache_ptr) p;
    return (((cp->fn << 4)*(cp->u.args.lson)*(cp->u.args.rson)) % n);
}

static bool
cache_equ(pointer p1, pointer p2)
{
    cache_ptr	cp1 = (cache_ptr) p1;
    cache_ptr	cp2 = (cache_ptr) p2;
    return( (cp1->fn == cp2->fn) &&
	    (cp1->u.args.lson == cp2->u.args.lson) &&
	    (cp1->u.args.rson == cp2->u.args.rson) );
}


static lunint
uniq_hash_fn(formula l, formula r, lunint n)
{
    lunint ret;
    ret = ((((l)+1)*((r)+1))>>1) % (n);
    return(ret);
}

static void
b_save(FILE *fp, formula f)
{
    unint	var;
    bdd_ptr	bp;
    formula	pf;
    if( f == ZERO || f == ONE )
        return;
    pf = POS(f);
    if( find_hash(&bdd_save_tbl, FORMULA2PTR(pf)) != NULL )
	return;
    bp = GET_BDDP(f);
    formula l = GET_LSON(bp);
    formula pl = POS(l);
    int il;
    if( pl == ZERO ) {
	il = 0;
    } else {
	b_save(fp, pl);
	il = PTR2FORMULA(find_hash(&bdd_save_tbl, FORMULA2PTR(pl)));
    }

    formula r = GET_RSON(bp);
    formula pr = POS(r);
    int ir;
    if( pr == ZERO ) {
	ir = 0;
    } else {
	b_save(fp, pr);
	ir = PTR2FORMULA(find_hash(&bdd_save_tbl, FORMULA2PTR(pr)));
    }

    var = BDD_GET_VAR(bp);
    fprintf(fp, "n %d",
	    PTR2FORMULA(find_hash(&save_var_tbl, (VarTbl+var)->var_name)));
    if( ISNOT(l) )
	fprintf(fp, " -%d)", il);
    else
	fprintf(fp, " +%d", il);
    if( ISNOT(r) )
	fprintf(fp, " -%d\n", ir);
    else
	fprintf(fp, " +%d\n", ir);
    insert_hash(&bdd_save_tbl, FORMULA2PTR(pf), FORMULA2PTR(bdd_save_cnt));
    bdd_save_cnt++;
}

static g_ptr
top_b_subst(g_ptr np)
{
    ASSERT(IS_LEAF(np));
    if( !IS_BOOL(np) ) return np;
    return( Make_BOOL_leaf(b_substitute(GET_BOOL(np), bdd_subs)) );
}

static g_ptr
top_bdd_depends(g_ptr np)
{
    ASSERT(IS_LEAF(np));
    if( !IS_BOOL(np) ) return np;
    B_Size(GET_BOOL(np));
    return np;
}

static g_ptr
top_bdd_size(g_ptr np)
{
    ASSERT(IS_LEAF(np));
    if( !IS_BOOL(np) ) return np;
    B_Size(GET_BOOL(np));
    return np;
}

static subst_ptr
add_to_subs(subst_ptr start, formula v, formula e)
{
    subst_ptr	new, ret, prev, cur;
    formula	lson, rson;
    unint	vvar, ovar;
    bdd_ptr	bp;

    if( v == ONE || v == ZERO )
        Rprintf("First element in substitution pair is not a variable");
    bp   = GET_BDDP(v);
    vvar = BDD_GET_VAR(bp);
    lson = GET_LSON(bp);
    rson = GET_RSON(bp);
    if( ((lson != ONE) && (lson != ZERO)) || ((rson != ONE) && (rson != ZERO)) )
        Rprintf("First element in substitution pair is an expression");
    new = (subst_ptr) new_rec(&subst_rec_mgr);
    new->var  = v;
    new->expr = e;
    new->next = NULL;
    /* Insert the record so that the variables appear in var_order */
    if( start == NULL )
        return(new);
    bp = GET_BDDP(start->var);
    ovar = BDD_GET_VAR(bp);
    if( ORDER(vvar) < ORDER(ovar) ) {
        new->next = start;
        return( new );
    }
    if( vvar == ovar )
        Rprintf("Repeated occurence in substitution");
    ret = start;
    prev = start;
    cur = start->next;
    while( cur != NULL ) {
	bp = GET_BDDP(cur->var);
	ovar = BDD_GET_VAR(bp);
        if( ORDER(vvar) < ORDER(ovar) )
            break;
        if( vvar == ovar )
            Rprintf("Repeated occurence in substitution");
        prev = cur;
        cur = cur->next;
    }
    new->next = cur;
    prev->next = new;
    return( ret );
}

static formula
b_substitute(formula f, subst_ptr subs)
{
    subst_cache_ptr     old;
    subst_cache_rec     tcache_rec;
    unint		fv, sv;
    formula		ret;
    var_ptr     	vp;
    bdd_ptr		bp;

    if( f == ONE || f == ZERO || subs == NULL )
        return( f );

    tcache_rec.f = f;
    tcache_rec.subs = subs;

    old = (subst_cache_ptr) find_hash(&subs_tbl, (pointer) &tcache_rec);
    if( old != NULL )
        return( old->result );

    old = (subst_cache_ptr) new_rec(&subst_cache_rec_mgr);
    *old = tcache_rec;

    bp = GET_BDDP(subs->var);
    sv = BDD_GET_VAR(bp);
    bp = GET_BDDP(f);
    fv = BDD_GET_VAR(bp);

    if( ORDER(fv) > ORDER(sv) ) {
        formula ret;
        ret = b_substitute(f, subs->next);
        old->result = ret;
        insert_hash(&subs_tbl, (pointer) old, (pointer) old);
        return( ret );
    }

    if( ORDER(fv) < ORDER(sv) ) {
	formula varf, lson, rson;
	vp = VarTbl + fv;
	varf = find_insert_bdd(vp,ONE,ZERO);
        if( ISNOT(f) ) {
	    lson = b_substitute(NOT(f2lson(f)), subs);
	    rson = b_substitute(NOT(f2rson(f)), subs);
        } else {
	    lson = b_substitute(f2lson(f), subs);
	    rson = b_substitute(f2rson(f), subs);
        }
	if((POS(lson) == ZERO || ORDER(f2var(lson)) > ORDER(fv)) &&
	   (POS(rson) == ZERO || ORDER(f2var(rson)) > ORDER(fv)) )
	    ret = find_insert_bdd(vp, lson, rson);
	else
	    ret = B_Or( B_And(lson, varf), B_And(rson, NOT(varf)));
        old->result = ret;
        insert_hash(&subs_tbl, (pointer) old, (pointer) old);
        return( ret );
    }
    /* Make current substitution */
    if( ISNOT(f) ) {
        ret = B_Or(
	       B_And(b_substitute(NOT(f2lson(f)),subs->next),subs->expr),
	       B_And(b_substitute(NOT(f2rson(f)),subs->next),NOT(subs->expr)));
    } else {
        ret = B_Or(B_And(b_substitute(f2lson(f),subs->next), subs->expr),
                   B_And(b_substitute(f2rson(f),subs->next), NOT(subs->expr)));
    }
    old->result = ret;
    insert_hash(&subs_tbl, (pointer) old, (pointer) old);
    return( ret );
}


static bool
do_draw_bdds(formula *bdds, int cnt) 
{
    FILE *fp;
    string filename;
    hash_record draw_tbl;
    //
    if( !Mk_output_file_in_tmp_dir("dot_draw", &fp, &filename) ) {
        Fail_pr("Cannot create dot_draw file. Out of disk space?");
        return FALSE;
    }
    create_hash(&draw_tbl, 100, bdd_hash, bdd_eq);
    //
    fprintf(fp, "digraph G {\n");
    fprintf(fp, "node [shape=circle];\n");
    fprintf(fp, "size = \"8.5,11.0\";\n");
    fprintf(fp, "center = 1;\n");
    fprintf(fp, "margin = 0.5;\n");
    fprintf(fp, "ordering=out;\n");
    draw_node_id = 0;
    for(int i = 0; i < cnt; i++) {
        fprintf(fp, "f%u [shape=box,style=filled,label=\"b%d\"];\n", i, i);
    }
    fprintf(fp, "{rank=same");
    for(int i = 0; i < cnt; i++) {
	fprintf(fp, " f%d", i);
    }
    fprintf(fp, "}\n");
    for(int i = 0; i < cnt; i++) {
	formula b = *(bdds+i);
	if( use_negated_pointers ) {
	    if( ISNOT(b) ) {
		int to = draw_bdd_rec(fp, &draw_tbl, POS(b));
		fprintf(fp, "f%u -> n%u [color=red];\n", i, to);
	    } else {
		int to = draw_bdd_rec(fp, &draw_tbl, b);
		fprintf(fp, "f%u -> n%u [color=blue];\n", i, to);
	    }
	} else {
	    int to = draw_bdd_rec(fp, &draw_tbl, b);
	    fprintf(fp, "f%u -> n%u [color=black];\n", i, to);
	}
    }
    fprintf(fp, "}");
    fclose(fp);
    dispose_hash(&draw_tbl, NULLFCN);
    Sprintf(draw_cmd, "display_dot %s", filename);
    tstr_ptr ts = new_temp_str_mgr();
    string res = strtemp("");
    if( !Send_to_tcl(draw_cmd, &res) ) {
	Fail_pr("display_dot failure: %s", res);
	free_temp_str_mgr(ts);
	return FALSE;
    }
    free_temp_str_mgr(ts);
    return TRUE;
}

static int
draw_bdd_rec(FILE *fp, hash_record *hp, formula f)
{
    int res;
    if( f == ZERO ) {
	draw_node_id++;
        fprintf (fp, "n%u [shape=plaintext, label = \"F\"];\n", draw_node_id);
	return draw_node_id;
    } 
    if( f == ONE ) {
	draw_node_id++;
        fprintf (fp, "n%u [shape=plaintext, label = \"T\"];\n", draw_node_id);
	return draw_node_id;
    } 
    if( (res = PTR2INT(find_hash(hp, FORMULA2PTR(f)))) != 0 ) { return res; }
    bdd_ptr bp = GET_BDDP(POS(f));
    var_ptr vp = VarTbl + BDD_GET_VAR(bp);
    string name = vp->var_name;
    draw_node_id++;
    res = draw_node_id;
    fprintf(fp, "n%u [label = \"%s\", ordering=out];\n", res, name);
    insert_hash(hp, FORMULA2PTR(f), INT2PTR(res));
    formula L = GET_LSON(bp);
    if( use_negated_pointers ) {
	if( ISNOT(L) ) {
	    int to = draw_bdd_rec(fp, hp, POS(L));
	    fprintf(fp, "n%u -> n%u [style=solid, color=red];\n", res, to);
	} else {
	    int to = draw_bdd_rec(fp, hp, L);
	    fprintf(fp, "n%u -> n%u [style=solid, color=blue];\n", res, to);
	}
    } else {
	if( ISNOT(f) ) { L = NOT(L); }
	int to = draw_bdd_rec(fp, hp, L);
	fprintf(fp, "n%u -> n%u [style=solid, color=black];\n", res, to);
    }
    //
    formula R = GET_RSON(bp);
    if( use_negated_pointers ) {
	if( ISNOT(R) ) {
	    int to = draw_bdd_rec(fp, hp, POS(R));
	    fprintf(fp, "n%u -> n%u [style=dashed, color=red];\n", res, to);
	} else {
	    int to = draw_bdd_rec(fp, hp, R);
	    fprintf(fp, "n%u -> n%u [style=dashed, color=blue];\n", res, to);
	}
    } else {
	if( ISNOT(f) ) { R = NOT(R); }
	int to = draw_bdd_rec(fp, hp, R);
	fprintf(fp, "n%u -> n%u [style=dashed, color=black];\n", res, to);
    }
    return res;
}

static int
bdd_get_size_rec(formula f, int *limitp)
{
    if( f == ZERO || f == ONE)
        return 0;
    if( *limitp <= 0 ) return 0;
    bdd_ptr bp = GET_BDDP(f);
    if( bp->sz_mark ) return 0;
    bp->sz_mark = 1;
    (*limitp)--;
    if( *limitp <= 0 ) return 1;
    int lres = bdd_get_size_rec(POS(GET_LSON(bp)), limitp);
    if( *limitp <= 0 ) return( lres+1 );
    int rres = bdd_get_size_rec(POS(GET_RSON(bp)), limitp);
    return(1 + lres + rres );
}

static void
restore_mark(formula f)
{
    if( f == ZERO || f == ONE)
	return;
    bdd_ptr bp = GET_BDDP(f);
    if( !bp->sz_mark ) return;
    bp->sz_mark = 0;
    restore_mark(POS(GET_LSON(bp)));
    restore_mark(POS(GET_RSON(bp)));
}

