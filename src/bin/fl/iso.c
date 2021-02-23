//------------------------------------------------------------------------------
// Copyright 2020 Markus Aronsson
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*      	                                      							  */
/*     Original author: Markus Aronsson, 2020                                 */
/*                                  									      */
/******************************************************************************/
#include "strings.h"
#include "buf.h"
#include "graph.h"
#include "iso.h"

/******************************************************************************/
/*                               GLOBAL VARIABLES                             */
/******************************************************************************/
// ...

// Global variables referenced -------------------------------------------------
extern str_mgr strings;

/******************************************************************************/
/*                              PRIVATE VARIABLES                             */
/******************************************************************************/
static ustr_mgr	lstrings;
static rec_mgr  tbl_mgr;
static rec_mgr  vec_mgr;
static rec_mgr  rng_mgr;
// Isomatch.
static rec_mgr mat_mgr;
static mat_ptr P; // Piece/Needle    adj. matrix.
static mat_ptr G; // Puzzle/Haystack adj. matrix.
static mat_ptr M; // Isomatch.            matrix.

// Forward definitions local functions -----------------------------------------
static vec_ptr   split_vector(string name);
static void      record_vector(hash_record_ptr vec, vec_ptr vs);
static range_ptr append_range(range_ptr r1, range_ptr r2);
// Matrix mgm.
static bool** matrix_allocate(unint R, unint C);
static void   matrix_free(bool **m);
// Adj.
static tbl_ptr mk_adj_tabel(g_ptr pex);
static bool    mk_adj_matrix();
static bool mk_adj_needle(g_ptr p, int size);
static bool mk_adj_haystack(g_ptr p, int size);
// Isomatch.
static bool test_isomorphism_formatting(mat_ptr iso);
static bool test_isomorphism_match(mat_ptr iso, mat_ptr p, mat_ptr g);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

static vec_ptr
split_vector(string name)
{
    vec_ptr vp = Split_vector_name(&lstrings,&vec_mgr,&rng_mgr,name);
    for(vec_ptr p = vp; p != NULL; p = p->next) {
        if(p->type == TXT) {
            p->u.name = wastrsave(&strings, p->u.name);
        }
    }
    return vp;
}

static void
record_vector(hash_record_ptr tbl, vec_ptr vs)
{
    string name;
    for(vec_ptr v = vs; v != NULL; v = v->next) {
        if(v->type == TXT) {
            name = v->u.name;
            insert_check_hash(tbl, &name, NULL);
        } else {
            range_ptr r = v->u.ranges;
            range_ptr o = (range_ptr) find_hash(tbl, &name);
            if (o == NULL) { o = r; }
            else { append_range(o, r); }
        }
    }
}

static range_ptr
append_range(range_ptr r1, range_ptr r2)
{
    if(r1 == NULL) { return r2; }
    range_ptr tmp = r1;
    while(tmp->next != NULL) { tmp = tmp->next; }
    tmp->next = r2;
    return r1;
}

// Matrix mgm. -----------------------------------------------------------------

static bool**
matrix_allocate(unint R, unint C)
{
    bool **m;
    m = Malloc(R*sizeof(bool*));
    m[0] = Calloc(R*C*sizeof(bool));
    for(unint i=1; i<R; i++) {
		m[i] = m[0]+i*C;
    }
    return m;
}

static void
matrix_free(bool **m)
{
    Free((void *)m[0]);
    Free((void *)m);
}

// Adj. matrix -----------------------------------------------------------------

static tbl_ptr
mk_adj_tabel(g_ptr pex)
{
    tbl_ptr node_tbl;
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(pex, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
	    Fail_pr("'mk_adj_matrix' expects a pexlif.");
        return NULL;
    }    
    if(is_P_LEAF(cont, &fns)) {
        Fail_pr("'mk_adj_matrix' expects a hierarchical pexlif.");
        return NULL;
    }
    else if(is_P_HIER(cont, &children)) {
        // Record vector names for parent's formals.
        hash_record parent_tbl_inp, parent_tbl_out;
        create_hash(&parent_tbl_inp, 2, str_hash, str_equ);
        create_hash(&parent_tbl_out, 2, str_hash, str_equ);
        for(g_ptr li = fa_inps; !IS_NIL(li); li = GET_CONS_TL(li)) {
            string fname = GET_STRING(GET_FST(GET_CONS_HD(li)));
            vec_ptr vp = split_vector(fname);
            record_vector(&parent_tbl_inp, vp);
        }
        for(g_ptr lo = fa_outs; !IS_NIL(lo); lo = GET_CONS_TL(lo)) {
            string fname = GET_STRING(GET_FST(GET_CONS_HD(lo)));
            vec_ptr vp = split_vector(fname);
            record_vector(&parent_tbl_out, vp);
        }
        node_tbl = new_rec(&tbl_mgr);
        node_tbl->inps = &parent_tbl_inp;
        node_tbl->outs = &parent_tbl_out;
        node_tbl->next = NULL;
        // Record vector names for each child's actuals.
        g_ptr cattrs, cfa_inps, cfa_outs, cinter, ccont;
        string cname;
        bool cleaf;
        tbl_ptr cur_tbl = node_tbl;
        for(g_ptr cs = children; !IS_NIL(cs); cs = GET_CONS_TL(cs)) {
            g_ptr child = GET_CONS_HD(cs);
            if(!is_PINST(child, &cname, &cattrs, &cleaf, &cfa_inps, &cfa_outs, &cinter, &ccont)) {
                Fail_pr("'mk_adj_matrix' expects all children to be pexlifs.");
                free_rec(&tbl_mgr, node_tbl);
                return NULL;
            }
            hash_record child_tbl_inp, child_tbl_out;
            create_hash(&child_tbl_inp, 2, str_hash, str_equ);
            create_hash(&child_tbl_out, 2, str_hash, str_equ);
            for(g_ptr li = cfa_inps; !IS_NIL(li); li = GET_CONS_TL(li)) {
                for(g_ptr acts = GET_SND(GET_CONS_HD(li)); !IS_NIL(acts); acts = GET_CONS_TL(acts)) {
                    string aname = GET_STRING(GET_CONS_HD(acts));
                    vec_ptr vp = split_vector(aname);
                    record_vector(&child_tbl_inp, vp);
                }
            }
            for(g_ptr lo = cfa_outs; !IS_NIL(lo); lo = GET_CONS_TL(lo)) {
                for(g_ptr acts = GET_SND(GET_CONS_HD(lo)); !IS_NIL(acts); acts = GET_CONS_TL(acts)) {
                    string aname = GET_STRING(GET_CONS_HD(acts));
                    vec_ptr vp = split_vector(aname);
                    record_vector(&child_tbl_out, vp);
                }
            }
            tbl_ptr child_tbl = new_rec(&tbl_mgr);
            child_tbl->inps = &child_tbl_inp;
            child_tbl->outs = &child_tbl_out;
            child_tbl->next = NULL;
            cur_tbl->next = child_tbl;
            cur_tbl = child_tbl;
        }
    }
    return node_tbl;
}

static bool
mk_adj_matrix(mat_ptr adj, g_ptr pex)
{
    tbl_ptr tbl = mk_adj_tabel(pex);
    return FALSE;
}

static bool
mk_adj_needle(g_ptr pex, int size)
{
    P = (mat_ptr) new_rec(&mat_mgr);
    P->mat  = matrix_allocate(size, size);
    P->rows = size;
    P->cols = size;
    //
    if (mk_adj_matrix(P, pex)) { return TRUE; }
    return FALSE;
}

static bool
mk_adj_haystack(g_ptr pex, int size)
{
    G = (mat_ptr) new_rec(&mat_mgr);
	G->mat  = matrix_allocate(size, size);
    G->rows = size;
    G->cols = size;
    //
    if (mk_adj_matrix(G, pex)) { return TRUE; }
    return FALSE;
}

// Solution check --------------------------------------------------------------

static bool
test_isomorphism_formatting(mat_ptr iso)
{
    bool row;
    bool *used = Calloc(iso->cols*sizeof(bool));
    // Only one 'TRUE' allowed per row and column.
    for(unint i=0; i<iso->rows; i++) {
		row = FALSE;
		for(unint j=0; j<iso->cols; j++) {
            if(iso->mat[i][j]) {
				if(row || used[j]) {
					Free((void *)used);
					return FALSE;
                }
                row = TRUE;
                used[j] = TRUE;
            }
        }
    }
    Free((void *)used);
    return TRUE;
}

static bool
test_isomorphism_match(mat_ptr iso, mat_ptr p, mat_ptr g)
{
	bool t1,t2;
    // P == ISO x (ISO x G)^T
	for(unint i=0; i<iso->rows; i++) {
		for(unint j=0; j<iso->rows; j++) {
			t1 = FALSE;
            for(unint k=0; k<iso->cols; k++) {
				t2 = FALSE;
                for(unint l=0; l<iso->cols; l++) {
					if(iso->mat[j][l] && g->mat[l][k]) {
						t2 = TRUE;
                        break;
                    }
                }
                if(iso->mat[i][k] && t2) {
					t1 = TRUE;
                    break;
                }
            }
            if(p->mat[j][i] != t1) {
				return FALSE;
            }
        }
    }
    return TRUE;
}

/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

void
Iso_Init()
{
    new_ustrmgr(&lstrings);
    new_mgr(&tbl_mgr, sizeof(tbl_rec));
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
    new_mgr(&mat_mgr, sizeof(mat_rec));
}

void
Iso_Install_Functions()
{

}

/******************************************************************************/
/*                               DUMMY FUNCTIONS                              */
/******************************************************************************/

void
_DuMMy_iso()
{
    M = NULL;
    append_range(NULL,NULL);
    record_vector(NULL,NULL);
    matrix_free(NULL);
    matrix_allocate(0,0);
    mk_adj_needle(NULL,0);
    mk_adj_haystack(NULL,0);
    test_isomorphism_formatting(NULL);
    test_isomorphism_match(NULL,NULL,NULL);
}

/******************************************************************************/
