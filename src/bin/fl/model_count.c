//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*                                                                      */
/*              Original author: Carl-Johan Seger, 2026                 */
/*                                                                      */
/************************************************************************/
#include "model_count.h"
#include "graph.h"
#include "new_bdd.h"
#include "bv.h"

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern bdd_ptr	    MainTbl;
extern str_mgr	    *stringsp;
extern lunint	    Size_MainTbl;
extern var_ptr	    VarTbl;
extern g_ptr        Zero;
extern g_ptr        One;

/***** PRIVATE VARIABLES *****/
static fp_truth_cov_ptr	    fp_truth_cov_cache;
static int		    fp_truth_cov_cache_sz;
static fp_truth_cov2_ptr    fp_truth_cov2_cache;
static int		    fp_truth_cov2_cache_sz;
static hash_record	    cond_gen_mc_cache_tbl;
static int		    cond_gen_mc_cache_sz = -1;
static hash_record	    gen_mc_cache_tbl;
static int		    gen_mc_cache_sz = -1;
static rec_mgr		    cond_gen_cache_rec_mgr;

/* ----- Forward definitions local functions ----- */
static unsigned int	cond_gen_mc_hash(pointer np, unsigned int n);
static bool		cond_gen_mc_eq(pointer p1, pointer p2);
static bool             fp_truth_cover2_rec(formula vs, formula cond, formula f,
                                            double *resp, string *emsgp);
static fp_truth_cov2_ptr find_in_fp_truth_cov2_cache(formula cond, formula f);

static bool             truth_cover_rec(hash_record *done_tblp,
                                        buffer *var_bufp, unint idx, formula b,
                                        arbi_T *resp, string *emsgp);
static bool
                        fp_truth_cover_rec(formula vs, formula b,
                                           double *resp, string *emsgp);
static g_ptr		find_in_gen_mc_cache(formula f);
static g_ptr		find_in_cond_gen_mc_cache(formula c, formula f);

/********************************************************/
/*                    LOCAL FUNCTIONS                   */
/********************************************************/


static unsigned int
cond_gen_mc_hash(pointer np, unsigned int n)
{
    cond_gen_cache_ptr cp = (cond_gen_cache_ptr) np;
    lunint cpc = (lunint) cp->c;
    lunint cpf = (lunint) cp->f;
    return( (unsigned int) ((cpc*cpf) % (lunint) n) );
}

static bool
cond_gen_mc_eq(pointer p1, pointer p2)
{
    cond_gen_cache_ptr cp1 = (cond_gen_cache_ptr) p1;
    cond_gen_cache_ptr cp2 = (cond_gen_cache_ptr) p2;
    return( (cp1->c == cp2->c) && (cp1->f == cp2->f) );
}

static g_ptr
find_in_cond_gen_mc_cache(formula c, formula f)
{
    ASSERT(cond_gen_mc_cache_sz > 0);
    cond_gen_cache_rec cr;
    cr.c = c;
    cr.f = f;
    g_ptr res = find_hash(&cond_gen_mc_cache_tbl, &cr);
    return res;
}

static void
insert_in_cond_gen_mc_cache(formula c, formula f, g_ptr bvl)
{
    cond_gen_cache_ptr cgcp = new_rec(&cond_gen_cache_rec_mgr);
    cgcp->c = c;
    cgcp->f = f;
    insert_hash(&cond_gen_mc_cache_tbl, cgcp, bvl);
}

static void
mark_cache_entry(pointer key, pointer data)
{
    (void) key;
    g_ptr bvl = (g_ptr) data;
    Mark(bvl);
}

static void
do_truth_cover(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list, gfun;
    EXTRACT_2_ARGS(redex, var_list, gfun);
    formula fun = GET_BOOL(gfun);
    buffer  var_table;
    new_buf(&var_table, 100, sizeof(unint));
    hash_record truth_table_done;
    create_hash(&truth_table_done, 100, Bdd_hash, Bdd_eq);
    while( !IS_NIL(var_list) ) {
        string vname = GET_STRING(GET_CONS_HD(var_list));
        formula v = B_Var(vname);
        bdd_ptr bp = GET_BDDP(v);
        unint var = BDD_GET_VAR(bp);
        push_buf(&var_table, (pointer) &var);
        var_list = GET_CONS_TL(var_list);
    }
    qsort(START_BUF(&var_table), COUNT_BUF(&var_table), sizeof(unint),
                    Var_ord_comp);
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

static void
create_tc_cache()
{
    fp_truth_cov_cache_sz = Size_MainTbl;
    fp_truth_cov_cache = (fp_truth_cov_ptr)Calloc((fp_truth_cov_cache_sz)*
                                                     sizeof(fp_truth_cov_rec));
}

static fp_truth_cov_ptr
find_in_fp_truth_cov_cache(formula f)
{
    unint idx;
    ASSERT(fp_truth_cov_cache_sz > 0);
    idx = (137*((unint) f) ) % fp_truth_cov_cache_sz;
    return( fp_truth_cov_cache + idx );
}

static void
free_tc_cache()
{
    Free((pointer) fp_truth_cov_cache);
    fp_truth_cov_cache_sz = -1;
}

static bool
fp_truth_cover_rec(formula vs, formula b, double *resp, string *emsgp)
{
    if( b == ZERO ) {
        *resp = 0.0;
        return TRUE;
    }
    if( b == ONE ) {
        double res = 1.0;
        while( vs != ZERO ) {
            res = 2.0*res;
            bdd_ptr vsp = GET_BDDP(vs);
            vs = GET_LSON(vsp);
        }
        *resp = res;
        return TRUE;
    }
    bdd_ptr bp = GET_BDDP(b);
    unint next_var = BDD_GET_VAR(bp);
    double mult = 1.0;
    while( (vs != ZERO) && (BDD_GET_VAR(GET_BDDP(vs)) != next_var) ) {
        mult = 2.0*mult;
        vs = GET_LSON(GET_BDDP(vs));
    }
    if( vs == ZERO ) {
        var_ptr vp = VarTbl + next_var;
        *emsgp =
            Fail_pr("Variable %s not in truth_cover list but f depends on it",
                    vp->var_name);
        return FALSE;
    }
    fp_truth_cov_ptr old = find_in_fp_truth_cov_cache(b);
    if( old->f == b ) {
        *resp = mult*old->res;
        return TRUE;
    }
    formula bnot = NOT(b);
    fp_truth_cov_ptr oldnot = find_in_fp_truth_cov_cache(bnot);
    if( oldnot->f == bnot ) {
        double all = 1.0;
        while( (vs != ZERO) ) {
            all = 2.0*all;
            vs = GET_LSON(GET_BDDP(vs));
        }
        *resp = mult*(all-oldnot->res);
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
    double Lres;
    vs = GET_LSON(GET_BDDP(vs));
    if( !fp_truth_cover_rec(vs, L, &Lres, emsgp) ) {
        return FALSE;
    }
    double Rres;
    if( !fp_truth_cover_rec(vs, R, &Rres, emsgp) ) {
        return FALSE;
    }
    double sum = Lres+Rres;
    old->f = b;
    old->res = sum;
    *resp = mult*sum;
    return TRUE;
}

static void
create_tc2_cache()
{   
    create_tc_cache();
    fp_truth_cov2_cache_sz = Size_MainTbl;
    fp_truth_cov2_cache = (fp_truth_cov2_ptr)Calloc((fp_truth_cov2_cache_sz)*
                                                     sizeof(fp_truth_cov2_rec));
}

static fp_truth_cov2_ptr
find_in_fp_truth_cov2_cache(formula cond, formula f)
{
    unint idx;
    ASSERT(fp_truth_cov2_cache_sz > 0);
    idx = (137*((unint) cond) + 487*((unint) f) ) % fp_truth_cov2_cache_sz;
    return( fp_truth_cov2_cache + idx );
}   
    
static void
free_tc2_cache()
{
    free_tc_cache();
    Free((pointer) fp_truth_cov2_cache);
    fp_truth_cov2_cache_sz = -1;
}


static bool
fp_truth_cover2_rec(formula vs, formula cond, formula f,
                    double *resp, string *emsgp)
{
    if( f == ZERO ) {
        *resp = 0.0;
        return TRUE;
    }
    if( cond == ZERO ) {
        *resp = 0.0;
        return TRUE;
    }
    if( f == ONE ) {
        return(fp_truth_cover_rec(vs,cond,resp,emsgp));
    }
    if( cond == ONE ) {
        return(fp_truth_cover_rec(vs,f,resp,emsgp));
    }
    bdd_ptr fp = GET_BDDP(f);
    bdd_ptr cp = GET_BDDP(cond);
    unint fnext_var = BDD_GET_VAR(fp);
    unint cnext_var = BDD_GET_VAR(cp);
    double mult = 1.0;
    unint csvar = BDD_GET_VAR(GET_BDDP(vs));
    while( (vs != ZERO) && (csvar != fnext_var) && (csvar != cnext_var) ) {
        mult = 2.0*mult;
        vs = GET_LSON(GET_BDDP(vs));
        csvar = BDD_GET_VAR(GET_BDDP(vs));
    }
    if( vs == ZERO ) {
        var_ptr vp = VarTbl + fnext_var;
        *emsgp =
            Fail_pr("Variable %s not in truth_cover list but f depends on it",
                    vp->var_name);
        return FALSE;
    }
    // Look up in cache
    fp_truth_cov2_ptr old = find_in_fp_truth_cov2_cache(cond, f);
    if( (old->cond == cond) && (old->f == f) ) {
        *resp = mult*old->res;
        return TRUE;
    }
    // Not in cache
    vs = GET_LSON(GET_BDDP(vs));
    if( (csvar == fnext_var) && (csvar != cnext_var) ) {
        formula L, R;
        if( ISNOT(f) ) {
            L = NOT(GET_LSON(fp));
            R = NOT(GET_RSON(fp));
        } else {
            L = GET_LSON(fp);
            R = GET_RSON(fp);
        }
        double Lres;
        if( !fp_truth_cover2_rec(vs, cond, L, &Lres, emsgp) ) {
            return FALSE;
        }
        double Rres;
        if( !fp_truth_cover2_rec(vs, cond, R, &Rres, emsgp) ) {
            return FALSE;
        }
        double sum = Lres+Rres;
        old->cond = cond;
        old->f = f;
        old->res = sum;
        *resp = mult*sum;
        return TRUE;
    } else {
        if( (csvar != fnext_var) && (csvar == cnext_var) ) {
            formula L, R;
                if( ISNOT(cond) ) {
                L = NOT(GET_LSON(cp));
                R = NOT(GET_RSON(cp));
            } else {
                L = GET_LSON(cp);
                R = GET_RSON(cp);
            }
            double Lres;
            if( !fp_truth_cover2_rec(vs, L, f, &Lres, emsgp) ) {
                return FALSE;
            }
            double Rres;
            if( !fp_truth_cover2_rec(vs, R, f, &Rres, emsgp) ) {
                return FALSE;
            }
            double sum = Lres+Rres;
            old->cond = cond;
            old->f = f;
            old->res = sum;
            *resp = mult*sum;
            return TRUE;
        } else {
            formula L, R;
            if( ISNOT(f) ) {
                L = NOT(GET_LSON(fp));
                R = NOT(GET_RSON(fp));
            } else {
                L = GET_LSON(fp);
                R = GET_RSON(fp);
            }
            formula Lc, Rc;
            if( ISNOT(cond) ) {
                Lc = NOT(GET_LSON(cp));
                Rc = NOT(GET_RSON(cp));
            } else {
                Lc = GET_LSON(cp);
                Rc = GET_RSON(cp);
            }
            double Lres;
            if( !fp_truth_cover2_rec(vs, Lc, L, &Lres, emsgp) ) {
                return FALSE;
            }
            double Rres;
            if( !fp_truth_cover2_rec(vs, Rc, R, &Rres, emsgp) ) {
                return FALSE;
            }
            double sum = Lres+Rres;
            old->cond = cond;
            old->f = f;
            old->res = sum;
            *resp = mult*sum;
            return TRUE;
        }
    }
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

static g_ptr
find_in_gen_mc_cache(formula f)
{
    ASSERT(gen_mc_cache_sz > 0);
    g_ptr res = find_hash(&gen_mc_cache_tbl, FORMULA2PTR(f));
    return res;
}

static void
insert_in_gen_mc_cache(formula f, g_ptr bvl)
{
    insert_hash(&gen_mc_cache_tbl, FORMULA2PTR(f), bvl);
}

static void
do_fp_truth_cover_n(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list, funs;
    EXTRACT_2_ARGS(redex, var_list, funs);
    bool o_do_dynamic_var_order = RCdo_dynamic_var_order;
    RCdo_dynamic_var_order = FALSE;
    create_tc_cache();
    formula vs = ONE;
    while( !IS_NIL(var_list) ) {
        string vname = GET_STRING(GET_CONS_HD(var_list));
        formula v = B_Var(vname);
        vs = B_And(vs, v);
        var_list = GET_CONS_TL(var_list);
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(funs) ) {
        double res;
        string emsg;
        formula fun = GET_BOOL(GET_CONS_HD(funs));
        if( !fp_truth_cover_rec(vs,fun,&res,&emsg)){
            MAKE_REDEX_FAILURE(redex, emsg);
            RCdo_dynamic_var_order = o_do_dynamic_var_order;
            free_tc_cache();
            DEC_REF_CNT(l);
            DEC_REF_CNT(r);
            return;
        } else {
            APPEND1(tail, Make_float_leaf(res));
        }
        funs = GET_CONS_TL(funs);
    }
    RCdo_dynamic_var_order = o_do_dynamic_var_order;
    free_tc_cache();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_fp_truth_cover2_n(g_ptr redex)
{   
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list, g_cond, funs;
    EXTRACT_3_ARGS(redex, var_list, g_cond, funs);
    formula cond = GET_BOOL(g_cond);
    bool o_do_dynamic_var_order = RCdo_dynamic_var_order;
    RCdo_dynamic_var_order = FALSE;
    create_tc2_cache();
    formula vs = ONE;
    while( !IS_NIL(var_list) ) {
        string vname = GET_STRING(GET_CONS_HD(var_list));
        formula v = B_Var(vname);
        vs = B_And(vs, v);
        var_list = GET_CONS_TL(var_list);
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(funs) ) {
        double res;
        string emsg;
        formula fun = GET_BOOL(GET_CONS_HD(funs));
        if( !fp_truth_cover2_rec(vs, cond, fun, &res, &emsg) )
        { 
            MAKE_REDEX_FAILURE(redex, emsg);
            free_tc2_cache();
            RCdo_dynamic_var_order = o_do_dynamic_var_order;
            DEC_REF_CNT(l);
            DEC_REF_CNT(r);
            return;
        } else {
            APPEND1(tail, Make_float_leaf(res)); 
        }
        funs = GET_CONS_TL(funs);
    }
    free_tc2_cache();
    RCdo_dynamic_var_order = o_do_dynamic_var_order;
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void 
do_truth_cover_n(g_ptr redex)
{   
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list, funs;
    EXTRACT_2_ARGS(redex, var_list, funs);
    
    buffer  var_table;
    new_buf(&var_table, 100, sizeof(unint));
    hash_record truth_table_done;
    create_hash(&truth_table_done, 100, Bdd_hash, Bdd_eq);
    while( !IS_NIL(var_list) ) {
        string vname = GET_STRING(GET_CONS_HD(var_list));
        formula v = B_Var(vname);
        bdd_ptr bp = GET_BDDP(v);
        unint var = BDD_GET_VAR(bp);
        push_buf(&var_table, (pointer) &var);
        var_list = GET_CONS_TL(var_list);
    }
    qsort(START_BUF(&var_table), COUNT_BUF(&var_table), sizeof(unint),
                    Var_ord_comp);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(funs) ) {
        arbi_T res;
        string emsg;
        formula fun = GET_BOOL(GET_CONS_HD(funs));
        if( !truth_cover_rec(&truth_table_done,&var_table,0,fun,&res,&emsg) ) {
            MAKE_REDEX_FAILURE(redex, emsg);
            free_buf(&var_table);
            dispose_hash(&truth_table_done, NULLFCN);
            DEC_REF_CNT(l);
            DEC_REF_CNT(r);
            return; 
        } else {
            APPEND1(tail, Make_AINT_leaf(res));
        }
        funs = GET_CONS_TL(funs);
    }
    free_buf(&var_table);
    dispose_hash(&truth_table_done, NULLFCN);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_gen_model_count(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list, funs;
    EXTRACT_2_ARGS(redex, var_list, funs);
    // Turn off Dynamic variable ordering
    bool o_do_dynamic_var_order = RCdo_dynamic_var_order;
    RCdo_dynamic_var_order = FALSE;
    // Determine total BDD size and all variables.
    unint sz;
    formula vs;
    hash_record var_tbl;
    create_hash(&var_tbl, 100, str_hash, str_equ);
    Get_Size_and_Vars(funs, NULL, var_list, &sz, &vs, &var_tbl);
    PUSH_BDD_GC(vs);
    Create_gen_mc_cache(sz);
    // Now compute the gen_model_count for all variables
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(funs) ) {
        formula fun = GET_BOOL(GET_CONS_HD(funs));
        g_ptr res = Gen_model_count_rec(vs, &var_tbl, fun);
        APPEND1(tail, Make_bv(res));
        funs = GET_CONS_TL(funs);
    }
    Free_gen_mc_cache();
    RCdo_dynamic_var_order = o_do_dynamic_var_order;
    dispose_hash(&var_tbl, NULLFCN);
    POP_BDD_GC(1);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_gen_cond_model_count(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr var_list, funs, gcond;
    EXTRACT_3_ARGS(redex, var_list, funs, gcond);
    // Turn off Dynamic variable ordering
    formula cond = GET_BOOL(gcond);
    bool o_do_dynamic_var_order = RCdo_dynamic_var_order;
    RCdo_dynamic_var_order = FALSE;
    // Determine total BDD size and all variables.
    unint sz;
    formula vs;
    hash_record var_tbl;
    create_hash(&var_tbl, 100, str_hash, str_equ);
    Get_Size_and_Vars(funs, gcond, var_list, &sz, &vs, &var_tbl);
    PUSH_BDD_GC(vs);
    Create_gen_mc_cache(sz);
    Create_cond_gen_mc_cache(sz);
    // Now compute the cond_gen_model_count for all variables
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(funs) ) {
        formula fun = GET_BOOL(GET_CONS_HD(funs));
        g_ptr res = Gen_cond_model_count_rec(vs, &var_tbl, cond, fun);
        APPEND1(tail, Make_bv(res));
        funs = GET_CONS_TL(funs);
    }
    Free_cond_gen_mc_cache();
    Free_gen_mc_cache();
    RCdo_dynamic_var_order = o_do_dynamic_var_order;
    dispose_hash(&var_tbl, NULLFCN);
    POP_BDD_GC(1);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

/********************************************************/
/*                    PUBLIC FUNCTIONS                  */
/********************************************************/

void
Create_gen_mc_cache(unint sz)
{
    gen_mc_cache_sz = sz;
    create_hash(&gen_mc_cache_tbl, 2*sz, Bdd_hash, Bdd_eq);
}

void
Free_gen_mc_cache()
{
    dispose_hash(&gen_mc_cache_tbl, NULLFCN);
    gen_mc_cache_sz = -1;
}


void
Create_cond_gen_mc_cache(unint sz)
{
    cond_gen_mc_cache_sz = sz;
    create_hash(&cond_gen_mc_cache_tbl, 2*sz, cond_gen_mc_hash, cond_gen_mc_eq);
    new_mgr(&cond_gen_cache_rec_mgr, sizeof(cond_gen_cache_rec));
}

void
Free_cond_gen_mc_cache()
{
    dispose_hash(&cond_gen_mc_cache_tbl, NULLFCN);
    free_mgr(&cond_gen_cache_rec_mgr);
    cond_gen_mc_cache_sz = -1;
}


void
Model_Count_GC()
{
    if( gen_mc_cache_sz < 0 ) return;
    scan_hash(&gen_mc_cache_tbl, mark_cache_entry);
}


g_ptr
Gen_model_count_rec(formula vs, hash_record *var_tblp, formula fun)
{
    g_ptr res;
    string vtop_var;
    formula vH, vL;
    if( fun == ZERO ) {
        res = Make_CONS_ND(Zero, Make_NIL());
        return res;
    }
    if( fun == ONE ) {
        // Return 2**|vars_left|
        res = Make_CONS_ND(Zero, Make_CONS_ND(One, Make_NIL()));    // 1
        g_ptr tail = GET_CONS_TL(GET_CONS_TL(res));
        while( (vs != ONE) ) {
            Get_top_cofactor(vs, &vtop_var, &vH, &vL);
            if( find_hash(var_tblp, vtop_var) != NULL ) {
                // 2*current
                APPEND1(tail, Make_BOOL_leaf(B_Zero()));
            }
            vs = vH;
        }
        return res;
    }
    unint fun_var = f_BDD_GET_VAR(f_GET_BDDP(fun));
    int mul = 0;
    while( f_BDD_GET_VAR(f_GET_BDDP(vs)) != fun_var ) {
        Get_top_cofactor(vs, &vtop_var, &vH, &vL);
        if( find_hash(var_tblp, vtop_var) != NULL ) {
            mul++;
        }
        vs = vH;
    }
    g_ptr cres = find_in_gen_mc_cache(fun);
    if( cres != NULL ) {
        res = Shift_left(cres, mul);
        return res;
    }
    formula H, L;
    string top_var;
    Get_top_cofactor(fun, &top_var, &H, &L);
    Get_top_cofactor(vs, &vtop_var, &vH, &vL);
    vs = vH;
    g_ptr Hres, Lres;
    Hres = Gen_model_count_rec(vs, var_tblp, H);
    PUSH_GLOBAL_GC(Hres);
    Lres = Gen_model_count_rec(vs, var_tblp, L);
    PUSH_GLOBAL_GC(Lres);
    g_ptr raw_res;
    if( find_hash(var_tblp, top_var) == NULL ) {
        formula v = B_Var(top_var);
        raw_res = Ite_bv_list(v, Hres, Lres);
    } else {
        (void) SX2(&Hres, &Lres);
        raw_res = Add_bv_lists(FALSE, Hres, Lres);
    }
    POP_GLOBAL_GC(2);
    insert_in_gen_mc_cache(fun, raw_res);
    res = Shift_left(raw_res, mul);
    return res;
}

g_ptr
Gen_cond_model_count_rec(formula vs, hash_record *var_tblp,
			 formula c, formula f)
{
    g_ptr res;
    string vtop_var;
    formula vH, vL;
    if( (f == ZERO) || (c == ZERO) ) {
        res = Make_CONS_ND(Zero, Make_NIL());
        return res;
    }
    if( c == ONE ) {
	res = Gen_model_count_rec(vs, var_tblp, f);
	return res;
    }
    if( f == ONE ) {
	res = Gen_model_count_rec(vs, var_tblp, c);
	return res;
    }
    string  cv, fv;
    formula cH, cL, fH, fL;
    Get_top_cofactor(c, &cv, &cH, &cL);
    Get_top_cofactor(f, &fv, &fH, &fL);
    string mv = (Get_BDD_index(f) < Get_BDD_index(c))? fv : cv;
    int mul = 0;

    Get_top_cofactor(vs, &vtop_var, &vH, &vL);
    while( vtop_var != mv ) {
        if( find_hash(var_tblp, vtop_var) != NULL ) {
            mul++;
        }
        vs = vH;
        Get_top_cofactor(vs, &vtop_var, &vH, &vL);
    }
    vs = vH;
    g_ptr cres = find_in_cond_gen_mc_cache(c,f);
    if( cres != NULL ) {
        res = Shift_left(cres, mul);
        return res;
    }
    g_ptr Hres, Lres, raw_res;
    if( (cv == mv) && (fv != mv) ) {
	Hres = Gen_cond_model_count_rec(vs, var_tblp, cH, f);
	Lres = Gen_cond_model_count_rec(vs, var_tblp, cL, f);
    } else
    if( (cv != mv) && (fv == mv) ) {
	Hres = Gen_cond_model_count_rec(vs, var_tblp, c, fH);
	Lres = Gen_cond_model_count_rec(vs, var_tblp, c, fL);
    } else {
	Hres = Gen_cond_model_count_rec(vs, var_tblp, cH, fH);
	Lres = Gen_cond_model_count_rec(vs, var_tblp, cL, fL);
    }
    if( find_hash(var_tblp, mv) == NULL ) {
	// "Free" variable
	formula v = B_Var(mv);
	raw_res = Ite_bv_list(v, Hres, Lres);
    } else {
	(void) SX2(&Hres, &Lres);
	raw_res = Add_bv_lists(FALSE, Hres, Lres);
    }
    insert_in_cond_gen_mc_cache(c, f, raw_res);
    res = Shift_left(raw_res, mul);
    return res;
}

void
Model_count_Install_Functions()
{

    Add_ExtAPI_Function("simple_model_count", "11", FALSE,
                        GLmake_arrow(GLmake_list(GLmake_string()),
                                     GLmake_arrow(GLmake_bool(),GLmake_int())),
                        do_truth_cover);

    Add_ExtAPI_Function("truth_cover_n", "11", FALSE,
                        GLmake_arrow(
                            GLmake_list(GLmake_string()),
                            GLmake_arrow(
                                GLmake_list(GLmake_bool()),
                                GLmake_list(GLmake_int()))),
                        do_truth_cover_n);

    typeExp_ptr float_tp = Get_Type("float", NULL, TP_INSERT_PLACE_HOLDER);
    Add_ExtAPI_Function("fp_truth_cover_n", "11", FALSE,
                        GLmake_arrow(
                            GLmake_list(GLmake_string()),
                            GLmake_arrow(
                                GLmake_list(GLmake_bool()),
                                GLmake_list(float_tp))),
                        do_fp_truth_cover_n);

    Add_ExtAPI_Function("model_count", "111", FALSE,
                        GLmake_arrow(
                            GLmake_list(GLmake_string()),
                            GLmake_arrow(
                              GLmake_bool(),
                              GLmake_arrow(
                                GLmake_list(GLmake_bool()),
                                GLmake_list(float_tp)))),
                        do_fp_truth_cover2_n);

    typeExp_ptr bv_handle_tp = Get_Type("bv", NULL, TP_INSERT_PLACE_HOLDER);
    Add_ExtAPI_Function("gen_model_count", "11", FALSE,
                        GLmake_arrow(
                            GLmake_list(GLmake_string()),
                            GLmake_arrow(
                                GLmake_list(GLmake_bool()),
                                GLmake_list(bv_handle_tp))),
                        do_gen_model_count);

    Add_ExtAPI_Function("gen_cond_model_count", "111", FALSE,
                        GLmake_arrow(
                            GLmake_list(GLmake_string()),
                            GLmake_arrow(
                                GLmake_list(GLmake_bool()),
				GLmake_arrow(
				    GLmake_bool(),
				    GLmake_list(bv_handle_tp)))),
                        do_gen_cond_model_count);

}
