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
static rec_mgr  key_lst_mgr;
static rec_mgr  key_mgr;
static rec_mgr  bkt_mgr;
static rec_mgr  vec_mgr;
static rec_mgr  rng_mgr;
static rec_mgr  mat_mgr;
// ?
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
static void      record_vector(unint i, const vec_ptr v);
static vec_ptr   split_vector(string name);
// Adj.
static void new_adj_mem();
static void rem_adj_mem();
static bool mk_adj_tabel(key_lst_ptr k, unint *c, g_ptr p);
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
record_vector(unint i, const vec_ptr v)
{
    string key = Get_vector_signature(&lstrings, v);
    bkt_ptr bkt = find_hash(tbl, key);
    if(bkt == NULL) {
        bkt = new_rec(&bkt_mgr);
        insert_hash(tbl, key, bkt);
    } else {
        while(bkt->next != NULL) { bkt = bkt->next; }
        bkt->next = new_rec(&bkt_mgr);
        bkt = bkt->next;
    }
    bkt->lbl  = i;
    bkt->vec  = v;
    bkt->next = NULL;
}

static vec_ptr
split_vector(string name)
{
    vec_ptr vp = Split_vector_name(&lstrings, &vec_mgr, &rng_mgr, name);
    for(vec_ptr p = vp; p != NULL; p = p->next) {
        if(p->type == TXT) {
            p->u.name = wastrsave(&strings, p->u.name);
        }
    }
    return vp;
}

// Adj. matrix -----------------------------------------------------------------

static void
new_adj_mem()
{
    new_mgr(&key_lst_mgr, sizeof(key_lst));
    new_mgr(&key_mgr, sizeof(key_rec));
    new_mgr(&bkt_mgr, sizeof(bkt_rec));
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
    create_hash(tbl, 100, str_hash, str_equ);
}

static void
rem_adj_mem()
{
    free_mgr(&key_lst_mgr);
    free_mgr(&key_mgr);
    free_mgr(&bkt_mgr);
    free_mgr(&vec_mgr);
    free_mgr(&rng_mgr);
    dispose_hash(tbl, NULLFCN);
}

static bool
mk_adj_tabel(key_lst_ptr keys, unint *count, g_ptr p)
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
    if(is_P_HIER(cont, &children)) {
        key_lst_ptr keys_tail = keys;
        vec_ptr vec;
        // Record vector names for parent's formals.
        FOREACH_FORMAL(vec, fa_inps) {
            record_vector(0, vec);
        }
        key_ptr key = NULL, key_tail;
        FOREACH_FORMAL(vec, fa_outs) {
            if(key == NULL) {
                key      = new_rec(&key_mgr);
                key_tail = key;
            } else {
                key_tail->next = new_rec(&key_mgr);
                key_tail = key_tail->next;
            }
            key_tail->vec  = vec;
            key_tail->next = NULL;
        }
        keys_tail->key  = key;
        keys_tail->next = NULL;
        // Record vector names for each child's actuals.
        g_ptr child, tmp;
        *count = 1;
        FOR_CONS(children, tmp, child) {
            if(!is_PINST(child, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
                Fail_pr("Adj. mat. expects all children to be pexlifs.");
                return FALSE;
            }
            FOREACH_ACTUAL(vec, fa_inps) {
                record_vector(*count, vec);
            }
            key_ptr key = NULL, key_tail;
            FOREACH_ACTUAL(vec, fa_outs) {
                if(key == NULL) {
                    key      = new_rec(&key_mgr);
                    key_tail = key;
                } else {
                    key_tail->next = new_rec(&key_mgr);
                    key_tail = key_tail->next;
                }
                key_tail->vec  = vec;
                key_tail->next = NULL;
            }
            keys_tail->next = new_rec(&key_lst_mgr);
            keys_tail       = keys_tail->next;
            keys_tail->key  = key;
            keys_tail->next = NULL;
            (*count)++;
        }
    }
    Fail_pr("What are you then?!");
    return FALSE;
}

static bool
mk_adj_matrix(mat_ptr m, g_ptr p)
{
    new_adj_mem();
    // Mark adjacencies, if any.
    key_lst_ptr keys = (key_lst_ptr) new_rec(&key_lst_mgr);
    unint length;
    if(mk_adj_tabel(keys, &length, p)) {
        // All nodes accounted for.
        ASSERT(m->cols == length);
        // Square matrix.
        ASSERT(m->cols == m->rows);
        //
        for(int i = 0; keys != NULL; keys = keys->next) {
            for(key_ptr key = keys->key; key != NULL; key = key->next) {
                vec_ptr vec = key->vec;
                string name = Get_vector_signature(&lstrings, vec);
                bkt_ptr bkt = (bkt_ptr) find_hash(tbl, name);
                while(bkt != NULL) {
                    if(Check_vector_overlap(vec, bkt->vec)) {
                        m->mat[i][bkt->lbl] = TRUE;
                    }
                    bkt = bkt->next;
                }
            }
        }
    }
    // Done.
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
    M = M;
}

/******************************************************************************/
