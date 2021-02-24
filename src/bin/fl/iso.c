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
static rec_mgr key_mgr;
static rec_mgr bkt_mgr;
static rec_mgr vec_mgr;
static rec_mgr rng_mgr;
static rec_mgr mat_mgr;
// ?
static key_ptr key_lst;
static hash_record_ptr tbl;
// Isomatch.
static mat_ptr P; // Piece/Needle    adj. matrix.
static mat_ptr G; // Puzzle/Haystack adj. matrix.
static mat_ptr M; // Isomatch.            matrix.

// Forward definitions local functions -----------------------------------------
// Matrix mgm.
static void allocate_matrix(mat_ptr m, unint R, unint C);
static void free_matrix(mat_ptr m);
// ?
static void      record_vector(unint i, vec_ptr vs);
static vec_ptr   split_vector(string name);
static vec_ptr   append_vector(vec_ptr v1, vec_ptr v2);
static range_ptr append_range(range_ptr r1, range_ptr r2);
static unint     keys_length();
// Adj.
static void new_adj_mem();
static void rem_adj_mem();
static bool mk_adj_tabel(g_ptr p);
static bool mk_adj_matrix(mat_ptr m, g_ptr p);
static bool mk_adj_needle(g_ptr p, int size);
static bool mk_adj_haystack(g_ptr p, int size);
// Isomatch.
static bool test_isomorphism_formatting(mat_ptr iso);
static bool test_isomorphism_match(mat_ptr iso, mat_ptr p, mat_ptr g);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

// Mem. mgm. -------------------------------------------------------------------

static void
allocate_matrix(mat_ptr m, unint R, unint C)
{
    m = (mat_ptr) new_rec(&mat_mgr);
    m->rows   = R;
    m->cols   = C;
    m->mat    = Malloc(R*sizeof(bool*));
    m->mat[0] = Calloc(R*C*sizeof(bool));
    for(unint i=1; i<R; i++) {
		m->mat[i] = m->mat[0]+i*C;
    }
}

static void
free_matrix(mat_ptr m)
{
    Free((void *)m->mat[0]);
    Free((void *)m->mat);
    free_rec(&mat_mgr, m);
}

// ? ---------------------------------------------------------------------------

static void
record_vector(unint i, vec_ptr vs)
{
    string name;
    while(vs != NULL) {
        if(vs->type == TXT) {
            name = vs->u.name;
            range_ptr r = NULL;
            while(vs != NULL && vs->type == INDEX) {
                r  = append_range(r, vs->u.ranges); // <--
                vs = vs->next;
            }
            bkt_ptr new = new_rec(&bkt_mgr);
            new->label  = i;
            new->range  = r;
            new->next   = NULL;
            bkt_ptr bkt = (bkt_ptr) find_hash(tbl, name);
            if(bkt != NULL) {
                while(bkt->next != NULL) { bkt = bkt->next; }
                bkt->next = new;
            } else {
                insert_hash(tbl, name, new);
            }
        } else {
            Fail_pr("What?!");
        }
        vs = vs->next;
    }
}

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

static vec_ptr
append_vector(vec_ptr v1, vec_ptr v2)
{
    if(v1 == NULL) { return v2; }
    vec_ptr tmp = v1;
    while(tmp->next != NULL) { tmp = tmp->next; }
    tmp->next = v2;
    return v1;
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

static unint
keys_length()
{
    unint len = 0;
    for(key_ptr tmp=key_lst; tmp != NULL; tmp=tmp->next) { len++; }
    return len;
}

// Adj. matrix -----------------------------------------------------------------

static void
new_adj_mem()
{
    new_mgr(&key_mgr, sizeof(key_rec));
    new_mgr(&bkt_mgr, sizeof(bkt_rec));
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
    create_hash(tbl, 100, str_hash, str_equ);
}

static void
rem_adj_mem()
{
    free_mgr(&key_mgr);
    free_mgr(&bkt_mgr);
    free_mgr(&vec_mgr);
    free_mgr(&rng_mgr);
    dispose_hash(tbl, NULLFCN);
}

static bool
mk_adj_tabel(g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
	    Fail_pr("Adj. mat. expects a pexlif.");
        return FALSE;
    }    
    if(is_P_LEAF(cont, &fns)) {
        Fail_pr("Adj. mat. expects a hierarchical pexlif.");
        return FALSE;
    }
    else if(is_P_HIER(cont, &children)) {
        // Record vector names for parent's formals.
        vec_ptr vec, vs = NULL;
        FOREACH_FORMAL(vec, fa_inps) { record_vector(0, vec); }
        FOREACH_FORMAL(vec, fa_outs) { append_vector(vs, vec); }
        key_lst = (key_ptr) new_rec(&key_mgr);
        key_lst->label = 0;
        key_lst->outs = vs;
        key_lst->next = NULL;
        // Record vector names for each child's actuals.
        g_ptr child, tmp;
        key_ptr end = key_lst;
        unint count = 1;
        FOR_CONS(children, tmp, child) {
            if(!is_PINST(child, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
                Fail_pr("Adj. mat. expects all children to be pexlifs.");
                return FALSE;
            }
            vs = NULL;
            FOREACH_ACTUAL(vec, fa_inps) { record_vector(count, vec); }
            FOREACH_ACTUAL(vec, fa_outs) { append_vector(vs, vec); }
            key_ptr new = (key_ptr) new_rec(&key_mgr);
            new->label = count;
            new->outs = vs;
            new->next = NULL;
            end->next = new;
            end = new;
            count++;
        }
    }
    return TRUE;
}

static bool
mk_adj_matrix(mat_ptr m, g_ptr p)
{
    new_adj_mem();
    if(!mk_adj_tabel(p)) {
        rem_adj_mem();
        return FALSE;
    }
    ASSERT(m->cols == keys_length()); // All nodes accounted for.
    ASSERT(m->cols == m->rows);       // Square matrix.
    for(unint i=0; i<m->rows; i++) {
        
    }
    rem_adj_mem();
    return TRUE;
}

static bool
mk_adj_needle(g_ptr p, int size)
{
    allocate_matrix(P, size, size);
    return mk_adj_matrix(P, p);
}

static bool
mk_adj_haystack(g_ptr p, int size)
{
    allocate_matrix(G, size, size);
    return mk_adj_matrix(G, p);
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
    free_matrix(NULL);
    mk_adj_needle(NULL,0);
    mk_adj_haystack(NULL,0);
    test_isomorphism_formatting(NULL);
    test_isomorphism_match(NULL,NULL,NULL);
}

/******************************************************************************/
