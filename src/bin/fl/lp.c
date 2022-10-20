//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author (C) 1994 Catherine Leung, Mark Greenstreet    *
*     Edited by Carl-Johan Seger, 1995                              *
*     and Sólrún Halla Einarsdóttir, 2018                           *
*                                                                   *
*********************************************************************/
/* lp.c:  decision procedure for linear programs */
#include "lp.h"
#include "graph.h"

/* ------------- Global variables ------------- */
/********* Global variables referenced ***********/
extern str_mgr      strings;
extern char         FailBuf[4096];

/***** PRIVATE VARIABLES *****/
//TODO: Re-implement without these globals?
static	Tableau		*a;
static 	Row		    *cur_row;
static 	Rational	*cur_rat;
static  Rational	rat_zero = NULL;
static  Rational	rat_one = NULL;
static  Rational	rat_MinusOne = NULL;

/* ----- Forward definitions local functions ----- */
static void       free_row(Row *r, int ncol);
static void       free_tableau(Tableau *a);
static void       copy_row(Row *r, Row *rr, int ncol);
static void       copy_unresolved(Tableau *a, Tableau *aa)  ;
static void 		  copy_tableau(Tableau *a, Tableau *aa);
static Tableau * 	lp_copy(Tableau *a);
static Tableau * 	add_row(Tableau *a, Row *d);
static Row * 		  removeRowFromUnresolved(Tableau *a, int i);
static Row * 		  removeRowFromBasic(Tableau *a, int i);
static int 		    in_basis(Tableau *a, int j);
static int * 		  find_basis(Tableau *a);
static Row * 		  diagonalize(Tableau *a, Row *r);
static void 		  pivot(Tableau *a, int i, int j);
static Tableau * 	simplex(Tableau *a, bool *ub, bool noeps);
static int 		    find_non_zero(Tableau *a, int ii);
static Tableau * 	drive_out(Tableau *a);
static Tableau * 	FindBfs(Tableau *a);
static void       bfs(Tableau *a, Rational *s);
static int 		    find_slack(Row *r, Tableau *a);
static bool		    strict_ineq(Tableau *a);
static int		    zero_column(Tableau *a);
static Tableau *	addNE(Tableau *sa);
static int		    is_feasible(Tableau *sa, bool unbounded);
static Tableau *	solveStrictIneq(Tableau *sa);
static void		    negate_row(Row *r, int ncol);
static void		    standard1(Tableau *a);
static void		    standard2(Tableau *a, Row **cv, Tableau *sa);
static void		    standard3(Tableau *sa, Row **cv, int cn);
static void		    standard4(Tableau *a, Tableau *sa);
static Tableau *	eliminate_zeros(Tableau *a);
static Tableau *	lp_StandardTableau(Tableau *a);
static void       rational_init(void);
static void       rational_destroy(void);
static void       rat_free(Rational x);
static int        cmpzero(arbi_T x);
static void       rat_error(char *s);
static Rational		arbi2rat(arbi_T i);
static Rational		rat_return(arbi_T n, arbi_T d);
static Rational		rat_copy(Rational x);
static Rational		rat_neg(Rational x);
static int		    rat_cmp(Rational a, Rational b);
static Rational		rat_add(Rational a, Rational b);
static Rational		rat_sub(Rational a, Rational b);
static Rational		rat_mlt(Rational a, Rational b);
static Rational		rat_inverse(Rational a);
static Rational		rat_div(Rational a, Rational b);
static void		    SetUpLP(int rows, int vars);
static void		    LP_no_cost_fn();
static void		    Add_LP_row(string relation);
static void		    Add_LP_row_elem(arbi_T i);
static bool		    LPfeasible();
static void		    LP_feasible(g_ptr redex);
static int		    lp_feasible(Tableau *a);
static void       LPsol(g_ptr redex);
static void       LP_solution(g_ptr redex);
static Rational *      lp_solution(Tableau *a);
#ifdef DEBUG
static void lp_WriteTableau(odests f, Tableau *a);
static void DBG_write_row(odests f, Row *r, int ncol);
static void DBG_write_number(odests f, Rational r);
static void DBG_write_string(odests f, char *s);
#endif

#define FREE(p)		Free((pointer)(p))
/********************************************************/
/*                    PUBLIC FUNCTIONS                  */
/********************************************************/

void
LP_Init()
{
}

void
LP_Install_Functions()
{
    Add_ExtAPI_Function("LP",				// fl name
                        "1",				// strictness of args
                        FALSE,			// is non-lazy?
                        GLmake_arrow(
                            GLmake_list(
                                GLmake_tuple(
                                    GLmake_list(GLmake_int()),
                                    GLmake_tuple(
                                        GLmake_string(),
                                        GLmake_int()))),
                            GLmake_bool()),   // Type of function
                        LP_feasible           // C function
                       );

    Add_ExtAPI_Function("LPsol",    // fl name
                        "1",        // strictness of args
                        FALSE,      // is non-lazy?
                        GLmake_arrow(
                            GLmake_list(
                                GLmake_tuple(
                                    GLmake_list(GLmake_int()),
                                    GLmake_tuple(
                                        GLmake_string(),
                                        GLmake_int()))),
                            GLmake_list(
                                GLmake_tuple(
                                    GLmake_int(),
                                    GLmake_int()))), // Type of function
                        LP_solution           // C function
                       );
}


/********************************************************/
/*                    LOCAL FUNCTIONS                   */
/********************************************************/

static void
LP_feasible(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    int rows = 0, cols = -1;
    g_ptr arg1 = r;
    /* Count rows and cols */
    g_ptr ntmp = arg1;
    while( GET_CONS_HD(ntmp) != NULL ) {
        g_ptr row, rp;
        int   cur_cols;
        rows++;
        row = GET_CONS_HD(ntmp);
        rp  = GET_CONS_HD(row);
        cur_cols = 0;
        while( GET_CONS_HD(rp) != NULL ) {
            cur_cols++;
            rp = GET_CONS_TL(rp);
        }
        if( cols == -1 )
            cols = cur_cols;
        else if( cols != cur_cols ) {
            MAKE_REDEX_FAILURE(redex,
                               Fail_pr("Inconsistent number of columns in LP"));
            DEC_REF_CNT(l);
            DEC_REF_CNT(r);
            return;
        }
        ntmp = GET_CONS_TL(ntmp);
    }
    if( rows == 0 || cols < 0 ) {
        MAKE_REDEX_FAILURE(redex,
                           Fail_pr("No %s in LP", rows==0?"rows":"columns"));
        DEC_REF_CNT(l);
        DEC_REF_CNT(r);
        return;
    }
    SetUpLP(rows, cols);
    LP_no_cost_fn();
    ntmp = arg1;
    while( GET_CONS_HD(ntmp) != NULL ) {
        g_ptr row, rp;
        row = GET_CONS_HD(ntmp);
        rp  = GET_CONS_TL(row);
        Add_LP_row(GET_STRING(GET_CONS_HD(rp)));
        Add_LP_row_elem(GET_AINT(GET_CONS_TL(rp)));
        rp  = GET_CONS_HD(row);
        while( GET_CONS_HD(rp) != NULL ) {
            Add_LP_row_elem(GET_AINT(GET_CONS_HD(rp)));
            rp = GET_CONS_TL(rp);
        }
        ntmp = GET_CONS_TL(ntmp);
    }
    if( LPfeasible() ) {
        MAKE_REDEX_BOOL(redex, B_One());
    } else {
        MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static bool
LPfeasible()
{
    bool ret;
    ret = lp_feasible(a);
    free_tableau(a);
    rational_destroy();
    return( (ret==1)? TRUE : FALSE );
}

static int
lp_feasible(Tableau *a)
{
    Rational *s;
    if (a->ncol ==0)
        return zero_column(a);

    s = lp_solution(a);
    if (s == NULL) {
        return 0;
    }
    return 1;
}

static void
LP_solution(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    int rows = 0, cols = -1;
    g_ptr arg1 = r;
    /* Count rows and cols */
    g_ptr ntmp = arg1;
    while( GET_CONS_HD(ntmp) != NULL ) {
        g_ptr row, rp;
        int   cur_cols;
        rows++;
        row = GET_CONS_HD(ntmp);
        rp  = GET_CONS_HD(row);
        cur_cols = 0;
        while( GET_CONS_HD(rp) != NULL ) {
            cur_cols++;
            rp = GET_CONS_TL(rp);
        }
        if( cols == -1 )
            cols = cur_cols;
        else if( cols != cur_cols ) {
            MAKE_REDEX_FAILURE(redex,
                               Fail_pr("Inconsistent number of columns in LP"));
            DEC_REF_CNT(l);
            DEC_REF_CNT(r);
            return;
        }
        ntmp = GET_CONS_TL(ntmp);
    }
    if( rows == 0 || cols < 0 ) {
        MAKE_REDEX_FAILURE(redex,
                           Fail_pr("No %s in LP", rows==0?"rows":"columns"));
        DEC_REF_CNT(l);
        DEC_REF_CNT(r);
        return;
    }
    SetUpLP(rows, cols);
    LP_no_cost_fn();
    ntmp = arg1;
    while( GET_CONS_HD(ntmp) != NULL ) {
        g_ptr row, rp;
        row = GET_CONS_HD(ntmp);
        rp  = GET_CONS_TL(row);
        Add_LP_row(GET_STRING(GET_CONS_HD(rp)));
        Add_LP_row_elem(GET_AINT(GET_CONS_TL(rp)));
        rp  = GET_CONS_HD(row);
        while( GET_CONS_HD(rp) != NULL ) {
            Add_LP_row_elem(GET_AINT(GET_CONS_HD(rp)));
            rp = GET_CONS_TL(rp);
        }
        ntmp = GET_CONS_TL(ntmp);
    }
    LPsol(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
LPsol(g_ptr redex)
{
    Rational *r;
    r = lp_solution(a);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;

    while (r != NULL && *r != NULL){
        g_ptr tmp = Make_CONS_ND(
            Make_AINT_leaf((*r)->n),
            Make_AINT_leaf((*r)->d));
        SET_CONS_HD(tail, tmp);
        SET_CONS_TL(tail,Make_NIL());
        tail = GET_CONS_TL(tail);
        r++;
    }
}

/* Return a feasible solution if there is one, otherwise return an empty array. */
static Rational *
lp_solution(Tableau *a)
{
    Tableau *sa;	/* 'a' in standard form */
    Tableau *pa;        /* pointer to the standard form */
    Row *r, *d;         /* r bfs, d diagonalized row */
    Rational *sol;
    int i, spos;
    bool unbounded = FALSE;

    sol = (Rational *)Malloc((a->ncol + 1) * sizeof(Rational));
    sol[a->ncol] = NULL;
    for(i=0; i<a->ncol; i++){
        sol[i] = rat_copy(rat_zero);
    }

    /* determine feasibility of zero column tableau */
    if (a->ncol == 0){
        return NULL;
    }

    /* compute standard form for 'a' */
    sa = lp_StandardTableau(a);

    if (sa == NULL) {
        /* lp_StandardTableau returns NULL
           if there are 0<>0 constraints */
        return NULL;
    }
    if (sa->nrow == 0) {
        /* tableau with zero row is feasible */
        free_tableau(sa);
        sa = NULL;
        return sol;
    }

    /* the LP is feasible iff it has a basic feasible solution */
    pa = sa;
    sa = FindBfs(pa);
    free_tableau(pa);

    /* handle <> not on B2 */
    if (sa) {
        sa = addNE(sa);
    }

    /* handle strict inequalities (ie. > or <) */
    if((sa) && strict_ineq(sa)){
        sa = solveStrictIneq(sa);
    }
    /* solve unresolved list where constraints intersect the bfs
     * represented by sa;
     * try both > and < for each unresolved constraint and see
     * which relation returns a feasible solution if there is one.
     */
    while ((sa != NULL) && (sa->ur > 0)) {
        /* start from the end of unresolved list */
        d = diagonalize(sa, &sa->unresolved[sa->ur]);
        /* try less than */
        d->rel = EQ;
        pa = sa;
        sa = add_row(pa, d);
        FREE(d);
        free_tableau(pa);
        rat_free(sa->rows[0].cols[sa->eps]);
        sa->rows[0].cols[sa->eps] = rat_copy(rat_MinusOne);
        r = removeRowFromUnresolved(sa, sa->ur);
        free_row(r, sa->ncol);
        FREE(r);
        pa = lp_copy(sa);
        sa = simplex(sa, &unbounded, FALSE);
        /* less than yields "feasible" */
        if (is_feasible(sa, unbounded)) {
            free_tableau(pa);
            pa = NULL;
            continue;
        }
        else {
            /* try greater than */
            free_tableau(sa);
            sa = pa;

            /* negate slack variable */
            spos = find_slack(&sa->rows[sa->nrow], sa);
            rat_free(sa->rows[sa->nrow].cols[spos]);
            sa->rows[sa->nrow].cols[spos] = rat_copy(rat_MinusOne);

            /* negate epsilon column */
            rat_free(sa->rows[sa->nrow].cols[sa->eps]);
            sa->rows[sa->nrow].cols[sa->eps] = rat_copy(rat_MinusOne);

            negate_row(&sa->rows[sa->nrow], sa->ncol);
            rat_free(sa->rows[0].cols[sa->eps]);
            sa->rows[0].cols[sa->eps] = rat_copy(rat_MinusOne);
            sa = simplex(sa, &unbounded, FALSE);

            if (!is_feasible(sa, unbounded)) {
                free_tableau(sa);
                sa = NULL;
                break;
            }
        }
    }
    if (sa != NULL) {
        bfs(sa,sol);
        free_tableau(sa);
    }
    else {
        FREE(sol);
        return NULL;
    }
    return sol;
}


static void
SetUpLP(int nrows, int nvars)
{
    int i;
    rational_init();
    a = (Tableau *)Malloc(sizeof(Tableau));
    a->nrow    = nrows;
    a->ncol    = nvars;
    a->rows = (Row *)Malloc((a->nrow + 1) * sizeof(Row));
    a->ur = 0;
    a->unresolved = NULL;
    a->varcols = (int *)Malloc((a->ncol + 1) * sizeof(int));
    for (i = 0; i <= a->ncol; i++){
        a->varcols[i] = i;
    }
    cur_row = (a->rows) - 1;
}

static void
LP_no_cost_fn()
{
    a->rows[0].cols = NULL;
    cur_row++;
}

static void
Add_LP_row(string relation)
{
    cur_row++;
    if( strncmp(relation, "<", 1) == 0 ) {
        if( strcmp(relation, "<=") == 0 )
            cur_row->rel = LE;
        else if( strcmp(relation, "<>") == 0 )
            cur_row->rel = NE;
        else
            cur_row->rel = LT;
    } else if(strncmp(relation, ">", 1) == 0) {
        if( strcmp(relation, ">=") == 0 )
            cur_row->rel = GE;
        else
            cur_row->rel = GT;
    } else if( strcmp(relation, "=") == 0 )
        cur_row->rel = EQ;
    else
        cur_row->rel = COST;
    cur_row->cols = (Rational *)Malloc((a->ncol + 1) * sizeof(Rational));
    cur_rat = cur_row->cols;
}

static void
Add_LP_row_elem(arbi_T i)
{
    *cur_rat = arbi2rat(i);
    cur_rat++;
}

static void
free_row(Row *r, int ncol)
{
    int j;
    for (j=0; j<=ncol; j++)
        rat_free(r->cols[j]);
    FREE(r->cols);
}

static void
free_tableau(Tableau *a)
/* free a tableau */
{
    int i;
    /* free basic tableau */
    for (i= !(a->rows[0].cols); i <= a->nrow; i++)
        free_row(&a->rows[i], a->ncol);
    FREE(a->rows);
    /* free unresolved list */
    for (i=1; i<=a->ur; i++)
        free_row(&a->unresolved[i], a->uc);
    if (a->ur)
        FREE(a->unresolved);
    FREE(a);
}

static void
copy_row(Row *r, Row *rr, int ncol)
{
    int j;
    /* Assumes that memory is already allocated for Row *rr */
    rr->cols = (Rational *)Malloc((ncol + 1) * sizeof(Rational));
    rr->rel = r->rel;
    for (j=0; j <= ncol; j++)
        rr->cols[j] = rat_copy(r->cols[j]);
}

static void
copy_unresolved(Tableau *a, Tableau *aa)
{
    /* Copy unresolved list from a to aa; assume memory is allocated for aa */
    int i;
    aa->uc = a->uc;  aa->ur = a->ur;
    if (a->ur) {
        aa->unresolved = (Row *)Malloc((a->ur + 1) * sizeof(Row));
        aa->unresolved[0].cols = NULL;
        for(i = 1; i <= a->ur; i++)
            copy_row(&a->unresolved[i], &aa->unresolved[i], a->uc);
    }
}

static void
copy_tableau(Tableau *a, Tableau *aa)
{
    /* Copy basic tableau from a to aa (unresolved list not included)
       assume memory is allocated for aa */
    int i;
    aa->nrow = a->nrow;  aa->ncol = a->ncol;
    aa->rows = (Row *)Malloc((a->nrow + 1) * sizeof(Row));
    aa->varcols = (int *)Malloc((a->ncol + 1) * sizeof(int));
    for(i=0;i<=a->ncol;i++){
        aa->varcols[i] = a->varcols[i];
    }
    for (i=!(a->rows[0].cols); i<=a->nrow; i++){
        copy_row(&a->rows[i], &aa->rows[i], a->ncol);
    }
}


static Tableau *
lp_copy(Tableau *a)
{
    Tableau *aa;
    aa = (Tableau *)Malloc(sizeof(Tableau));
    aa->eps = a->eps;
    copy_tableau(a, aa);
    copy_unresolved(a, aa);
    return(aa);
}

static Tableau *
add_row(Tableau *a, Row *d)
{
    Tableau *aa;
    int i;

    /* Add Row d to tableau a;
     * new memory is allocated for new tableau, except memory for row d.
     */
    aa = (Tableau *)Malloc(sizeof(Tableau));
    aa->rows = (Row *)Malloc((a->nrow + 2) *sizeof(Row));
    aa->varcols = (int *)Malloc((a->ncol + 1) * sizeof(int));
    for(i=0; i<= a->ncol; i++){
        aa->varcols[i] = a->varcols[i];
    }
    aa->nrow = a->nrow;  aa->ncol = a->ncol;  aa->eps = a->eps;
    aa->ur = a->ur; aa->uc = a->uc;
    for (i=!(a->rows[0].cols); i<=a->nrow; i++)
        copy_row(&a->rows[i], &aa->rows[i], a->ncol);
    aa->nrow++;
    /* add d to last row */
    aa->rows[aa->nrow].cols = d->cols;
    aa->rows[aa->nrow].rel = d->rel;
    copy_unresolved(a, aa);
    return aa;
}

static Row *
removeRowFromUnresolved(Tableau *a, int i)
{
    Row *r;

    /* Remove the ith-row from the unresolved list;
     * New memory is allocated for the removed unresolved constraint */
    if (a->ur < i)
        DIE("Internal error: removeRowFromUnresolved --- no such constraint");

    r = (Row *)Malloc(sizeof(Row));
    r->cols = a->unresolved[i].cols;
    r->rel = a->unresolved[i].rel;

    if (i != a->ur) {
        a->unresolved[i].cols = a->unresolved[a->ur].cols;
        a->unresolved[i].rel = a->unresolved[a->ur].rel;
    }
    a->ur--;
    if (a->ur == 0)
        FREE(a->unresolved);
    return r;
}


static Row *
removeRowFromBasic(Tableau *a, int i)
{
    Row *r;

    /* Remove the ith-row from the basic tableau;
     * new memory is allocated for the pointer to the removed row  */
    if (a->nrow < i)
        DIE("Internal error: removeRowFromBasic --- no such constraint");
    r = (Row *)Malloc(sizeof(Row));
    r->cols = a->rows[i].cols;  r->rel = a->rows[i].rel;
    a->rows[i] = a->rows[a->nrow];
    a->nrow--;
    return r;
}


static int
in_basis(Tableau *a, int j)
{
    int ii, entry = -1;

    /* read the j-th column in tableau *a, and determine whether it is in the
     * basis;
     * return -1 if column j is not in the basis, and
     * return the row with the 1 entry if column j is in the basis.  */
    for (ii = 0; ii <= a->nrow; ii++) {
        if ((rat_cmp(a->rows[ii].cols[j], rat_one) == EQ) && entry == -1)
            entry = ii;
        else if ((rat_cmp(a->rows[ii].cols[j], rat_zero) != EQ))
            return -1;
    }
    return entry;
}


static int *
find_basis(Tableau *a)
{
    bool    *marked;
    int     *basis;
    int     i, j;         /* row and column indices */

    /* given a tableau, return an array of int with the basis marked by
     * the position of the one and zero when the column is not in the basis.  */
    marked = (bool *)Malloc((a->nrow+1) *sizeof(bool));
    for (i=0; i<=a->nrow; i++)    /* initialize marked */
        marked[i] = FALSE;
    basis = (int *) Malloc((a->ncol+1) * sizeof(int));
    for (j=1; j<=a->ncol; j++) {
        i = in_basis(a, j);
        if ((i > 0) && !marked[i] && (a->eps != j)) {
            basis[j] = i;
            marked[i] = TRUE;
        }
        else
            basis[j] = 0;
    }
    FREE(marked);
    return basis;
}


static Row *
diagonalize(Tableau *a, Row *r)
{
    int *basis;
    int i, j, jj;
    Row *rtmp;
    Rational tmp1, tmp2, multiple;

    /* diagonalize row r according to tableau a;
     * assume r has the a->ncol columns.
     * create new memory for diagonalized row.  */
    rtmp = (Row *)Malloc(sizeof(Row));
    rtmp->cols = (Rational *)Malloc((a->ncol+1) * sizeof(Rational));
    rtmp->rel = r->rel;
    for (j=0; j<=a->ncol; j++)
        rtmp->cols[j] = rat_copy(r->cols[j]);

    basis = find_basis(a);
    for (j=1; j <= a->ncol; j++) {
        if ((i = basis[j]) > 0) {    /* a->rows[i].cols[j] = 1 */
            multiple = rat_copy(rtmp->cols[j]);
            for (jj = 0; jj <= a->ncol; jj++) {
                tmp1        = rat_mlt(a->rows[i].cols[jj], multiple);
                tmp2        = rat_sub(rtmp->cols[jj], tmp1);
                rat_free(tmp1);
                rat_free(rtmp->cols[jj]);
                rtmp->cols[jj] = tmp2;
            }
            rat_free(multiple);
        }
    }
    FREE(basis);
    return rtmp;
}


static void
pivot(Tableau *a, int i, int j)
{
    int ii, jj;
    Rational *r_i, *r_ii;
    Rational tmp, tmp1;
    Rational x;

    r_i = a->rows[i].cols;
    x = rat_copy(r_i[j]);

    /* normalize the pivot row */
    for(jj = 0; jj <= a->ncol; jj++) {
        tmp = rat_div(r_i[jj], x);
        rat_free(r_i[jj]);
        r_i[jj] = tmp;
    }
    rat_free(x);

    /* eliminate column j from all other rows */
    for(ii = 0; ii <= a->nrow; ii++) {
        if(ii != i) {
            r_ii = a->rows[ii].cols;
            x = rat_copy(r_ii[j]);
            for(jj = 0; jj <= a->ncol; jj++) {
                tmp1 = rat_mlt(x, r_i[jj]);
                tmp = rat_sub(r_ii[jj], tmp1);
                rat_free(r_ii[jj]);
                rat_free(tmp1);
                r_ii[jj] = tmp;
            }
            rat_free(x);
        }
    }
}

static Tableau *
simplex(Tableau *a, bool *ub, bool noeps)
{
    bool opt, unbounded, bland;
    int i, ii, j, jj;
    Rational best = NULL, theta_0 = NULL, theta_i = NULL;

    opt = unbounded = bland = FALSE;

    while(!opt && !unbounded) {
        /* find a profitable column */
        jj = 0; best = rat_zero;
        for(j = 1; (j <= a->ncol) && ((jj == 0) || !bland); j++) {
            if(noeps &&(j == a->eps)) continue;
            if(rat_cmp(a->rows[0].cols[j], rat_zero) == LT) {
                if(bland) {
                    jj = j;
                    best = a->rows[0].cols[j];
                }
                else if(rat_cmp(a->rows[0].cols[j], best) == LT) {
                    jj = j;
                    best = a->rows[0].cols[j];
                }
            }
        }
        if(jj == 0) opt = TRUE;
        else {
            /* choose a row for the pivot */
            ii = 0;
            for(i = 1; i <= a->nrow; i++) {
                if(rat_cmp(a->rows[i].cols[jj], rat_zero) == GT) {
                    /* element greater than zero */
                    theta_i = rat_div(a->rows[i].cols[0], a->rows[i].cols[jj]); /* */
                    if((ii == 0) || (rat_cmp(theta_i, theta_0) == LT)) {
                        if (ii > 0)
                            rat_free(theta_0);
                        ii = i;
                        theta_0 = theta_i;
                    }
                    else {
                        rat_free(theta_i);
                        theta_i = NULL;
                    }
                }
            }
            if (theta_0) {
                rat_free(theta_0);
                theta_0 = NULL;
            }

            if(ii == 0) unbounded = TRUE;
            else {
                pivot(a, ii, jj);
            }
        }
    }
    if(ub) *ub = unbounded;
    return(a);
}


static int
find_non_zero(Tableau *a, int ii)
{
    int j;
    /* find a non-artificial variable with non-zero value in row ii.
     * called by drive_out().  */
    /* scan non-artificial columns */
    for (j = 1; j <= a->ncol - a->nrow; j++)
        if ((j != a->eps) && rat_cmp(a->rows[ii].cols[j], rat_zero) != EQ)
            return j;
    return -1;
}


static Tableau *
drive_out(Tableau *a)
{
    int i, ii, jj;
    Row     *r;
    int     *basis;

    /* drive any zero-level artificial variables */
    /* out of the basis of *a			 */
    basis = find_basis(a);
    /* scan columns with artificial variables */
    for (i = a->ncol - a->nrow + 1; i <= a->ncol; i++) {
        if ((ii = basis[i]) > 0) {
            if ((jj = find_non_zero(a, ii)) < 0) {
                /* original matrix contains linearly dependent rows */
                if (rat_cmp(a->rows[ii].cols[0], rat_zero) == EQ) {
                    r = removeRowFromBasic(a, ii);
                    free_row(r, a->ncol);
                    FREE(r);
                } else
                    /* infeasible */
                    return NULL;
            } else
                pivot(a, ii, jj);
        }
    }
    FREE(basis);
    return(a);
}

static Tableau *
FindBfs(Tableau *a)
{
    /* Find a basic feasible solution for the linear program given by
     * tableau *a.  We assume *a is in standard form.  If *a is feasible,
     * we return a tableau corresponding to a feasible solution.
     * We use the two-phase method (see P&S section 2.8).
     * First, we build a tableau with artificial variables.
     * We then solve this tableau.
     * If we find a solution of zero cost:
     *   We drive any zero-level artificial variables out of the basis,
     *   Finally, we make sure that all strict inequalities are satisfiable.
     */
    Tableau *aa;		/* the new tableau */
    int i, j;		/* row and column indices */
    int m;          /* number of rows before driving out */
    bool unbounded;	/* there is an unbounded solution */
    Rational tmp;

    /* Build a tableau with artificial variables */
    aa = (Tableau *)Malloc(sizeof(Tableau));
    aa->nrow = a->nrow - a->ncol;	  /* x >= 0 assumed in the remaining code */
    aa->ncol = a->ncol + aa->nrow;	/* one artificial variable per row */
    aa->eps = a->eps;
    aa->varcols = (int *)Malloc((a->ncol + 1) * sizeof(int));
    for(i=0; i<= a->ncol; i++){
        aa->varcols[i] = a->varcols[i];
    }
    aa->rows = (Row *)Malloc((aa->nrow+1) * sizeof(Row));
    for(i = 0; i <= aa->nrow; i++) {
        aa->rows[i].rel = EQ;
        aa->rows[i].cols = (Rational *)Malloc((aa->ncol + 1)*sizeof(Rational));
    }
    aa->rows[0].rel = COST;

    /* set coeffients */
    for(j = 0; j <= aa->ncol; j++) /* starting point for cost row */
        aa->rows[0].cols[j] = rat_copy(rat_zero);
    for(i = 1; i <= aa->nrow; i++) {
        for(j = 0; j <= a->ncol; j++) {
            /* copy coefficients from *a */
            aa->rows[i].cols[j] = rat_copy(a->rows[i].cols[j]);
            /* and subtract from the cost row */
            tmp = aa->rows[0].cols[j];
            aa->rows[0].cols[j] = rat_sub(tmp, a->rows[i].cols[j]);
            rat_free(tmp);
        }
        for(; j <= aa->ncol; j++) { /* slack variables */
            if(j == a->ncol + i) aa->rows[i].cols[j] = rat_copy(rat_one);
            else aa->rows[i].cols[j] = rat_copy(rat_zero);
        }
    }

    /* copy unresolved list */
    copy_unresolved(a, aa);

    /* solve the linear program with artificial variables */
    aa = simplex(aa, &unbounded, TRUE);
    if(unbounded)
        DIE("Internal error: FindBfs --- messed up LP");

    if(rat_cmp(aa->rows[0].cols[0], rat_zero) != EQ) { /* infeasible */
        free_tableau(aa);
        aa = NULL;
    }
    else {
        /* drive out any remaining artificial variables */
        m = aa->nrow;
        aa = drive_out(aa);

        /* ignore artificial variables: free artificial variables */
        for (i=0; i <= aa->nrow; i++) {
            for (j = aa->ncol-m+1; j <= aa->ncol; j++)
                rat_free(aa->rows[i].cols[j]);
        }
        aa->ncol -=  m;
    }

    return(aa);
}

static void
bfs(Tableau *a, Rational *s)
{
    int i, j, l;        /* row and column indices */
    int *basis, *labels;         /* marked basis, column labels */

    /* Given a tableau return the corresponding feasible solution */
    basis = find_basis(a);
    labels = a->varcols;

    /* the bfs in the original coordinates */
    for (j=1; j<=a->ncol; j++) {
        i = basis[j];
        l = labels[j];
        if(l != 0){
            if (i > 0){
                if(l < 0){
                    s[-1*l - 1] = rat_add(s[-1*l - 1],rat_copy(a->rows[i].cols[0]));
                }
                else{
                    s[l - 1] = rat_add(s[l - 1],rat_copy(a->rows[i].cols[0]));
                }
            }
        }
    }
    FREE(basis);
}


static int
find_slack(Row *r, Tableau *a)
{
    int j;

    /* Look for position of slack variable for row r in a standard tableau a;
     * return -1 if no slack variable in the row.
     * slack variables */
    for (j = a->eps+1; j<=a->ncol; j++) {
        if (rat_cmp(r->cols[j], rat_zero) != EQ)
            return j;
    }
    return -1;
}


static bool
strict_ineq(Tableau *a)
{
    int i;

    /* Return 1 if strict inequalities present in current basic tableau;
     * return 0 otherwise.  */
    if (a->eps <= 0)
        return FALSE;
    for (i=1; i <= a->nrow; i++) {
        if (rat_cmp(a->rows[i].cols[a->eps], rat_zero) != EQ)
            return TRUE;
    }
    return FALSE;
}

static int
zero_column(Tableau *a)
{
    int i;        /* row index */

    for(i=1; i<=a->nrow; i++) {
#if 0
        switch( a->rows[i].rel ) {
        case LT:
            if( rat_cmp(rat_zero, a->rows[i].cols[0]) != LT )
                return( 0 );
            break;
        case LE:
            if( rat_cmp(rat_zero, a->rows[i].cols[0]) == GT )
                return( 0 );
            break;
        case EQ:

            %%%%%%%%
            }
#endif
        if (!(rat_cmp(rat_zero, a->rows[i].cols[0]) & a->rows[i].rel)) {
            return 0;
        }
    }
    return 1;
}


static Tableau *
addNE(Tableau *sa)
{
    int unresolved, spos, i, ii;
    Row *d, *r;
    Tableau *pa;

    /* Examines unresolved constraints, diagonalize each unresolved row,
       and add them to the tableau if the current bfs does not lie in
       the subspace.  */
    unresolved = sa->ur;
    for (ii=1, i=1; i<=unresolved; i++) {
        d = diagonalize(sa, &sa->unresolved[ii]);
        if (rat_cmp(d->cols[0], rat_zero) == EQ) {
            ii++;
            free_row(d, sa->ncol);
            FREE(d);
            continue;
        }
        if (rat_cmp(d->cols[0], rat_zero) == GT) {
            /* strictly less than; keep slack variable */
            d->rel = EQ;
        } else if (rat_cmp(d->cols[0], rat_zero) == LT) {
            /* strictly greater than */
            d->rel = EQ;
            /* negate slack variable */
            spos = find_slack(d, sa);
            rat_free(d->cols[spos]);
            d->cols[spos] = rat_copy(rat_MinusOne);
            /* negate epsilon column */
            rat_free(d->cols[sa->eps]);
            d->cols[sa->eps] = rat_copy(rat_MinusOne);
            negate_row(d, sa->ncol);
        }

        r = removeRowFromUnresolved(sa, ii);
        free_row(r, sa->ncol);
        FREE(r);
        pa = sa;
        sa = add_row(pa, d);
        FREE(d);
        free_tableau(pa);
    }
    return sa;
}

static int
is_feasible(Tableau *sa, bool unbounded)
{
    int i;

    /* epsilon > 0 ==> feasible solution */
    if ((rat_cmp(sa->rows[0].cols[0], rat_zero) == GT) || (unbounded))
        return 1;
    /* epsilon = 0 and bounded ==> infeasible */
    if ((rat_cmp(sa->rows[0].cols[0], rat_zero) == EQ) && (!unbounded))
        return 0;

    /* unbounded and epsilon = 0 */
    for (i=1; i<=sa->nrow; i++) {
        /* can furthur minimize epsilon */
        if (rat_cmp(sa->rows[i].cols[0], rat_zero) == GT)
            return 1;
    }

    return 0;
}

static Tableau *
solveStrictIneq(Tableau *sa)
{
    bool unbounded = FALSE;

    /* call simplex with epsilon cost = -1, and ensure that strict
       inequalities are satified.  */
    rat_free(sa->rows[0].cols[sa->eps]);
    sa->rows[0].cols[sa->eps] = rat_copy(rat_MinusOne);
    sa = simplex(sa, &unbounded, FALSE);

    if (is_feasible(sa, unbounded))
        return sa;
    else {
        free_tableau(sa);
        sa = NULL;
        return sa;
    }
}



/*
 * We export Tableau *lp_StandardTableau(Tableau *a);
 *   Create a new Tableau equivalent to *a but in standard form:
 *	   Ax = b, x >= 0
 *   The equality constraints precede the constraints on each variable in
 *   the ordering of the rows of the tableau produced by this function.
 *   The original variables precede the surplus/slack variables in the
 *   ordering of the columns. The original variables remain in the same
 *   order, except when an unconstrained variable is split.  The + and -
 *   components are adjacent in the standard form tableau.
 *   An extra row (a->rows[a->nrow + 1]) marks split variables
 *   (entries of rat_one indicate the +half of split variables).
 *
 *   This function is idempotent.  Furthermore, if given a tableau in
 *   standard form, the tableau generated will be identical to within
 *   permutation of the rows.
 *
 *   standard1(), standard2(), standard3() and standard4() are helper
 *   functions for standard().
 *
 *   Note: *a may include strict inequalities that can't be represented
 *     in standard form.  Initially, we treat constraints of the form
 *     'x[j] > 0' as 'x[j] >= 0' and ignore constraints of the form
 *     'x[j] <> 0'.  After finding a bfs, to the modified LP, the function
 *     "strict" makes sure all strict inequalities can be satisfied.
 */

static void
negate_row(Row *r, int ncol)
{
    int    j;
    Rational tmp;

    /* Negate all entries of row r (with ncol columns) */
    for (j = 0; j<=ncol; j++) {
        tmp = r->cols[j];
        r->cols[j] = rat_neg(tmp);
        rat_free(tmp);
    }
}

static void
standard1(Tableau *a)
{
    int i;

    /* Negate rows with a->rows[i].cols[0] < 0, and change signs accordingly. */
    for (i=!(a->rows[0].cols); i<= a->nrow; i++) {
        if (rat_cmp(a->rows[i].cols[0], rat_zero) == LT) {
            switch (a->rows[i].rel) {
            case EQ:
            case NE:
                break;
            case GT:
                a->rows[i].rel = LT;
                break;
            case LT:
                a->rows[i].rel = GT;
                break;
            case GE:
                a->rows[i].rel = LE;
                break;
            case LE:
                a->rows[i].rel = GE;
                break;
            default:
                DIE("Unexpected relation");
            }
            negate_row(&a->rows[i], a->ncol);
        }
    }
}

static void
standard2(Tableau *a, Row **cv, Tableau *sa)
{
    int ns;	/* how many surplus/slack variables are needed */
    int nu;	/* how many unconstrained variables in original tableau */
    int drop;	/* how many constraints can be dropped */
    int i;	/* row index */
    Row *r;	/* the current row */
    bool strict; /* a contains strict inequalities */
    int j, jj;	/* column indices */

    /* Figure out how many equality constraints and variables the standard
     * form tableau will have.
     *   a:	  The original tableau.
     *   cv:	*(cv[j]) set to row that constrains x[j] >= 0, if any
     *   sa:  The new tableau; we set sa->nrow, sa->ncol, sa->eps
     */

    /* initialize cv */
    for(j = 0; j <= a->ncol; j++)
        cv[j] = NULL;

    ns = drop = sa->ur = 0;
    nu = a->ncol;
    strict = FALSE;

    for(i = 1; i <= a->nrow; i++) {
        r = a->rows + i;
        switch(r->rel) {
        case EQ:
            break; /* an equality constraint, nothing to do */
        case NE:
            drop++;	/* we ignore <> constraints in the standard form tableau */
            sa->ur++;       /* put <>'s in unresolved list */
            strict = TRUE;  /* will be written as > or < */
            ns++;
            break;
        case GT:
        case LT:
            strict = TRUE;
            ns++; /* slack variable needed */
            break;
        case GE:
            /* if this is a constraint of the form x[j] >= 0,
             * note that x[j] is suitably constrained,
             * otherwise fall through. */
            if(rat_cmp(r->cols[0], rat_zero) == EQ) {
                jj = 0;
                for(j = 1; j <= a->ncol; j++) {
                    if(rat_cmp(r->cols[j], rat_zero) != EQ) {
                        if(!jj) jj = j;
                        else break;
                    }
                }
                if(jj && (j == a->ncol + 1) &&
                   (rat_cmp(r->cols[jj], rat_zero) == GT)) {
                    /* a constraint: x[jj] >= 0 */
                    if(cv[jj] == NULL) nu--;
                    cv[jj] = r;
                    drop++;
                    break;
                }
            }
	    // ??????? CJS ///
            ns++; /* slack variable needed */
	    // ??????? CJS ///
            break;
        case LE:
            ns++; /* slack variable needed */
            break;
        default:
            DIE("Internal error: standard2 --- bad relation");
        }
    }

    sa->ncol = sa->uc = a->ncol + nu + ns;     /* how many variables in sf tableau */
    if (strict) {
        sa->ncol++;
        sa->uc++;
        sa->eps = sa->ncol - ns;
    } else {
        sa->eps = -1;
    }
    sa->nrow = a->nrow - drop + sa->ncol;  /* how many constraints */
}

static void
standard3(Tableau *sa, Row **cv, int cn)
{
    int i;	/* row index */
    Rational *cm;	/* columns in the marker row*/
    int * ls; /* column labels */
    int j, jj, jjj;	/* column indices */

    /* Allocate rows of sf tableau, set row relations, mark split variables.
     *   sa:	Standard form tableau in the making.
     *   cv:	markers for constraints of the form x[j] >= 0
     */

    /* allocate rows, no cost row  */
    sa->rows = (Row *)Malloc((sa->nrow + 2) * sizeof(Row));
    sa->rows[0].cols = NULL;
    for(i = 1; i <= sa->nrow+1; i++)
        sa->rows[i].cols = (Rational *)Malloc((sa->ncol + 1) * sizeof(Rational));

    /* allocate unresolved list */
    if (sa->ur) {
        sa->unresolved = (Row *)Malloc((sa->ur + 1) * sizeof(Row));
        sa->unresolved[0].cols = NULL;
        for(i = 1; i <= sa->ur; i++)
            sa->unresolved[i].cols = (Rational*)Malloc((sa->uc+1)*sizeof(Rational));
    }

    sa->varcols = (int *)Malloc((sa->ncol + 1) * sizeof(int));

    /* mark split variables */
    cm  = sa->rows[sa->nrow + 1].cols;
    ls  = sa->varcols;
    for (j = 1; j <= sa->ncol; j++){ // initialize
        cm[j]  = rat_copy(rat_MinusOne);
        ls[j]  = 0;
    }
    cm[0] = rat_copy(rat_zero);
    ls[0] = 0;
    for(j = jj = jjj = 1; j <= cn; j++, jj++, jjj++) {
        ls[jjj] = j;
        if(cv[j] == NULL){
            cm[jj++] = rat_copy(rat_one);
            jjj++;
            ls[jjj] = -1*j;
        }
        cm[jj] = rat_copy(rat_zero);
    }
    sa->rows[sa->nrow+1].rel = NE;
}

static void
standard4(Tableau *a, Tableau *sa)
{
    int i, ii;		  /* row indices for a and sa */
    int j, jj;		  /* row indices for a and sa */
    Rational *c, *cc, *cm;  /* column arrays for a and sa */
    int slack;		  /* index of next uncommitted slack variable */
    int lastv = 0;	  /* index of last non-slack variable in sa */
    int neq = 0;    /* counter for <> */

    /* copy coefficients from a to sa, splitting unconstrained variables,
     *   introducing slack and surplus variables, and adding constraints
     *   of the form x[j] >= 0 for each variable.
     */

    /* process equality constraints */
    slack = sa->ncol;
    cm = sa->rows[sa->nrow + 1].cols;
    for (i = ii = 1; i <= a->nrow; i++) {
        sa->rows[ii].cols[sa->ncol] = NULL;

        /* fill epsilon if there is one */
        switch(a->rows[i].rel) {
        case EQ:
            if (sa->eps >= 0)
                sa->rows[ii].cols[sa->eps] = rat_copy(rat_zero);
            cc = sa->rows[ii].cols;
            sa->rows[ii].rel = EQ;
            break; /* an equality constraint, just copy it */
        case NE:
            neq++;
            sa->unresolved[neq].cols[sa->eps] = rat_copy(rat_one);
            cc = sa->unresolved[neq].cols;
            sa->unresolved[neq].rel = NE;
            /* continue; * we ignore these for now */
            break;
        case GT:
            sa->rows[ii].cols[sa->eps] = rat_copy(rat_MinusOne);
            cc = sa->rows[ii].cols;
            sa->rows[ii].rel = EQ;
            break;
        case LT:
            sa->rows[ii].cols[sa->eps] = rat_copy(rat_one);
            cc = sa->rows[ii].cols;
            sa->rows[ii].rel = EQ;
            break;
        case GE:
            /* if this is a constraint of the form x[j] >= 0,
             *   ignore it, otherwise copy it, adding a surplus variable */
            if(rat_cmp(a->rows[i].cols[0], rat_zero) == EQ) {
                jj = 0;
                for(j = 1; j <= a->ncol; j++) {
                    if(rat_cmp(a->rows[i].cols[j], rat_zero) != EQ) {
                        if(!jj)
                            jj = j;
                        else
                            break;
                    }
                }
                if(jj && (j == a->ncol + 1) &&
                   (rat_cmp(a->rows[i].cols[jj], rat_zero) == GT)) {
                    /* it's x[jj] >= 0 */
                    continue;	  /* drop it */
                }
            }
            if (sa->eps >= 0)
                sa->rows[ii].cols[sa->eps] = rat_copy(rat_zero);
            cc = sa->rows[ii].cols;
            sa->rows[ii].rel = EQ;
            break;
        case LE:
            if (sa->eps >= 0)
                sa->rows[ii].cols[sa->eps] = rat_copy(rat_zero);
            cc = sa->rows[ii].cols;
            sa->rows[ii].rel = EQ;
            break;
        default:
            DIE("Internal error: standard4.1 --- bad relation");
        }

        /* copy row a->rows[i] to row sa->rows[ii] splitting unconstrained
         *   variables in the process.  If a slack or surplus variable
         *   is needed, use x[slack];
         */
        c = a->rows[i].cols;
        for(j = jj = 0; j <= a->ncol; j++, jj++) {
            cc[jj] = rat_copy(c[j]);
            if(rat_cmp(cm[jj], rat_one) == EQ) { /* split the variable */
                cc[++jj] = rat_neg(c[j]);
            }
        }
        if(sa->eps >= 0)
            jj++;
        lastv = jj-1;
        for(; jj <= sa->ncol; jj++) {
            if(jj == slack) {
                switch(a->rows[i].rel) {
                case NE: cc[slack--] = rat_copy(rat_one); ii--; break;
                case EQ: cc[slack] = rat_copy(rat_zero); break;
                case GT:
                case GE: cc[slack--] = rat_copy(rat_MinusOne); break;
                case LT:
                case LE: cc[slack--] = rat_copy(rat_one); break;
                default: DIE("Internal error: standard4.2---bad relation");
                }
            } else {
                cc[jj] = rat_copy(rat_zero);
            }
        }
        ii++;
    }

    if((ii != sa->nrow + 1 - sa->ncol) || (slack != lastv))
        DIE("Internal error: standard4 --- bookkeeping error");

    /* now set the x[j] >= 0 constraints */
    for(; ii <= sa->nrow; ii++) {
        cc = sa->rows[ii].cols;
        sa->rows[ii].rel = GE;
        for(jj = 0; jj <= sa->ncol; jj++) {
            if(jj == ii - (sa->nrow - sa->ncol))
                cc[jj] = rat_copy(rat_one);
            else
                cc[jj] = rat_copy(rat_zero);
        }
    }

    /* free cm */
    for (j=0; j<=sa->ncol; j++)
        if (sa->rows[sa->nrow+1].cols[j])
            rat_free(sa->rows[sa->nrow+1].cols[j]);
    FREE(sa->rows[sa->nrow+1].cols);
}

static Tableau *
eliminate_zeros(Tableau *a)
{
    int i, j, ii, indx;
    Tableau *aa;

    /* eliminate all zero rows.
     * if relation is =, <=, >=: simply remove the row from the tableau;
     * if relation is >, <, <>:  set a = NULL, ie. problem is infeasible.
     */
    aa = (Tableau *)Malloc(sizeof(Tableau));
    aa->rows = (Row *)Malloc((a->nrow + 1) * sizeof(Row));
    aa->varcols = (int *)Malloc((a->ncol + 1) * sizeof(int));
    for(i=0;i<=a->ncol;i++){
        aa->varcols[i] = a->varcols[i];
    }
    aa->ncol = a->ncol;
    aa->ur = a->ur; 
    aa->uc = a->uc;
    aa->eps = a->eps;
    aa->rows[0].cols = a->rows[0].cols;

    for (i=!(a->rows[0].cols), ii=1; i<=a->nrow; i++) {
        for (j=0; j<=a->ncol; j++) {
            if (rat_cmp(a->rows[i].cols[j], rat_zero) != EQ) {
                break;
            }
        }
        if (j > aa->ncol) {
            /* all zero rows */
            if (a->rows[i].rel & EQ) {
                /* don't copy row */
                continue;
            } else {                 /* infeasible */
                for (indx = !(a->rows[0].cols); indx < ii; indx++)
                    free_row(&aa->rows[indx], a->ncol);
                FREE(aa->rows);
                FREE(aa);
                return NULL;
            }
        } else {                   /* copy row */
            copy_row(&a->rows[i], &aa->rows[ii], a->ncol);
            ii++;
        }
    }
    aa->nrow = ii-1;
    return aa;
}


static Tableau *
lp_StandardTableau(Tableau *a)
{
    Tableau *pa, *sa; /* pa: tableau pointer; sa: tableau in standard form */
    Row **cv;	 /* *(cv[j]) = row that constrains variable j */

    pa = eliminate_zeros(a);
    if (!pa)
        return NULL;
    if (pa->nrow == 0)
        return pa;


    /* allocate sa and cv */
    sa = (Tableau *)Malloc(sizeof(Tableau));
    cv = (Row **)Malloc((a->ncol+1)*sizeof(Row *));

    /* Negate rows with negative cost */
    standard1(pa);

    /* Figure out how many constraints and variables the standard form
     * tableau will have. */
    standard2(pa, cv, sa);

    /* allocate rows of sf tableau, and mark split variables */
    standard3(sa, cv, pa->ncol);

    standard4(pa, sa); /* set the coefficients of sa */

    free_tableau(pa);
    FREE(cv);
    return(sa);
}

/************************************************************************/
/*			Rationals                                                       */
/************************************************************************/


static void
rational_init(void)
{
    rat_zero = (Rational) Malloc(sizeof(struct Rational));
    rat_zero->n = Arbi_FromInt(0);
    rat_zero->d = Arbi_FromInt(1);

    rat_one = (Rational) Malloc(sizeof(struct Rational));
    rat_one->n = Arbi_FromInt(1);
    rat_one->d = Arbi_FromInt(1);

    rat_MinusOne = rat_neg(rat_one);
}

static void
rational_destroy(void)
{
    rat_free(rat_zero);
    rat_free(rat_one);
    rat_free(rat_MinusOne);
}


/* memory allocation routines */
static void
rat_free(Rational x)
{
    if (x->n)
        Arbi_free(x->n);
    if (x->d)
        Arbi_free(x->d);
    FREE(x);
}


/* arithmetic */
static int
cmpzero(arbi_T x)
{
    arbi_T   zero;
    int      flag;

    zero = Arbi_FromInt(0);
    flag = Arbi_cmp(x, zero);
    Arbi_free(zero);
    return flag;
}

static void
rat_error(char *s)
{
    FP(err_fp, "%s", s);
    exit(0);
}

static Rational
arbi2rat(arbi_T i)
{
    Rational r;
    r = (Rational) Malloc(sizeof(struct Rational));
    r->n = Arbi_cpy(i);
    r->d  = Arbi_FromInt(1);
    return r;
}

static Rational
rat_return(arbi_T n, arbi_T d)
{
    Rational r;
    int      nsign, dsign;
    arbi_T   gcd, an, ad, rn;

    nsign = cmpzero(n);
    dsign = cmpzero(d);

    if (dsign == 0)
        rat_error("rat_return: division by zero\n");

    if (nsign == dsign) {
        nsign = 1;  dsign = 1;
    } else {
        nsign = -1; dsign = 1;
    }
    an = Arbi_abs(n);
    ad = Arbi_abs(d);
    r = (Rational) Malloc(sizeof(struct Rational));
    gcd = Arbi_gcd(an, ad);
    rn  = Arbi_div(an, gcd);
    r->d  = Arbi_div(ad, gcd);

    Arbi_free(gcd);
    Arbi_free(an);
    Arbi_free(ad);

    if (nsign < 0) {
        r->n = Arbi_neg(rn);
        Arbi_free(rn);
    } else
        r->n = rn;
    return r;
}

static Rational
rat_copy(Rational x)
{
    return rat_return(x->n, x->d);
}

static Rational
rat_neg(Rational x)
{
    Rational neg;
    arbi_T   xn;

    xn = Arbi_neg(x->n);
    neg = rat_return(xn, x->d);
    Arbi_free(xn);
    return neg;
}

static int
rat_cmp(Rational x, Rational y)
{
    arbi_T xn, yn;
    int    retval;

    xn = Arbi_mlt(x->n, y->d);
    yn = Arbi_mlt(y->n, x->d);
    retval = Arbi_cmp(xn, yn);
    Arbi_free(xn);
    Arbi_free(yn);
    if(retval < 0) return(LT);
    else if(retval == 0) return(EQ);
    else return(GT);
}

static Rational
rat_add(Rational a, Rational b)
{
    Rational sum;
    arbi_T   u, l;
    arbi_T   x, y;

    x = Arbi_mlt(a->d, b->n);
    y = Arbi_mlt(b->d, a->n);
    u = Arbi_add(x, y);
    l = Arbi_mlt(a->d, b->d);
    Arbi_free(x);
    Arbi_free(y);
    sum = rat_return(u, l);
    Arbi_free(u);
    Arbi_free(l);
    return sum;
}

static Rational
rat_sub(Rational a, Rational b)
{
    Rational neg_b, diff;

    neg_b = rat_neg(b);
    diff  = rat_add(a, neg_b);
    rat_free(neg_b);
    return diff;
}

static Rational
rat_mlt(Rational a, Rational b)
{
    Rational prod;
    arbi_T   u, l;

    u = Arbi_mlt(a->n, b->n);
    l = Arbi_mlt(a->d, b->d);
    prod = rat_return(u, l);
    Arbi_free(u);
    Arbi_free(l);
    return prod;
}

static Rational
rat_inverse(Rational x)
{
    return( rat_return(x->d, x->n) );
}

static Rational
rat_div(Rational x, Rational y)
{
    Rational r, inv;

    inv = rat_inverse(y);
    r = rat_mlt(x, inv);
    rat_free(inv);
    return r;
}

#ifdef DEBUG

/************************************************************************/
/*			Print functions for debugging                                   */
/************************************************************************/

#define MAXLINE 76

static char wbuf[MAXLINE+2];
static int  wpos = 0;
static int  indent = 0;

#define INDENT_AMOUNT 2
#define TOTAL_INDENT (indent*INDENT_AMOUNT)


static void
DBG_write_string(odests f, char *s)
{
    int len = strlen(s);

    if(!strcmp(s, "\n")) {
        strcpy(wbuf + wpos, "\n");
        FP(f, "%s", wbuf);
        wpos = 0;
        return;
    }

    while(wpos < TOTAL_INDENT)
        wbuf[wpos++] = ' ';

    if(len + wpos > MAXLINE) {
        /* flush the current buffer */
        strcpy(wbuf + wpos, "\n");
        FP(f, "%s", wbuf);

        for(wpos = 0; wpos < TOTAL_INDENT; wbuf[wpos++] = ' ');
        wbuf[wpos] = '\0';

        /* if s is still too long, print it directly */
        if(len + wpos > MAXLINE) {
            FP(f, "%s%s\n", wbuf, s);
            wpos = 0;
            return;
        }
    }

    /* append s to the buffer */
    strcpy(wbuf + wpos, s);
    wpos += len;
    if(s[len-1] == '\n') {
        FP(f, "%s", wbuf);
        wpos = 0;
    }
}

static void
DBG_write_int(odests f, int i)
{
    char buf[32];

    Sprintf(buf, "%d", i);
    DBG_write_string(f, buf);
}

static void
DBG_write_number(odests f, Rational r)
{
    char *s;

    s = strtemp(Arbi_ToString(r->n, 10));
    s = strappend("/");
    s = strappend(Arbi_ToString(r->d,10));
    DBG_write_string(f, s);
}


static void
DBG_write_relation(odests f, int r)
{
    switch(r) {
    case LT:   DBG_write_string(f, "< ");	break;
    case LE:   DBG_write_string(f, "<=");	break;
    case EQ:   DBG_write_string(f, "= ");	break;
    case GE:   DBG_write_string(f, ">=");	break;
    case GT:   DBG_write_string(f, "> ");	break;
    case NE:   DBG_write_string(f, "<>");	break;
    case COST: DBG_write_string(f, "$");	break;
    default: DIE("Internal error: write_relation --- bad relation");
    }
}

static void
DBG_write_dimension(odests f, Tableau *a)
{
    DBG_write_int(f, a->nrow);
    DBG_write_string(f, ", ");
    DBG_write_int(f, a->ncol);
}

static void
DBG_write_row(odests f, Row *r, int ncol)
{
    int j;

    DBG_write_relation(f, r->rel);
    for(j = 0; j <= ncol; j++) {
        DBG_write_string(f, " ");
        DBG_write_number(f, r->cols[j]);
        if(j < ncol) DBG_write_string(f, ", ");
        else DBG_write_string(f,";\n");
    }
}

static void
DBG_write_rows(odests f, Tableau *a)
{
    int i;

    if(a->rows[0].cols != NULL)
        DBG_write_row(f, a->rows, a->ncol);
    for(i = 1; i <= a->nrow; i++)
        DBG_write_row(f, a->rows + i, a->ncol);
}

static void
lp_WriteTableau(odests f, Tableau *a)
{
    DBG_write_string(f, "{ ");
    DBG_write_dimension(f, a);
    DBG_write_string(f, ";\n");
    indent++;
    DBG_write_rows(f, a);
    indent--;
    DBG_write_string(f, "}\n");
}

void
PRINT_Tableau(Tableau *a)
{
    lp_WriteTableau(err_fp, a);
}

#endif
