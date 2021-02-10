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
#include "graph.h"
#include "iso.h"

/******************************************************************************/
/*                               GLOBAL VARIABLES                             */
/******************************************************************************/
// ...

// Global variables referenced -------------------------------------------------
// ...

/******************************************************************************/
/*                              PRIVATE VARIABLES                             */
/******************************************************************************/
// Isomatch.
static rec_mgr mat_mgr;
static mat_ptr P; // Piece/Needle    adj. matrix.
static mat_ptr G; // Puzzle/Haystack adj. matrix.
static mat_ptr M; // Isomatch.            matrix.

// Forward definitions local functions -----------------------------------------
// Matrix mgm.
//static bool** matrix_allocate(bool **m, unint R, unint C);
static void matrix_free(bool **m);
// Isomatch.
static void prune(mat_ptr iso, mat_ptr p, mat_ptr g);
static bool test_isomorphism_formatting(mat_ptr iso);
static bool test_isomorphism_match(mat_ptr iso, mat_ptr p, mat_ptr g);
static void recurse(mat_ptr iso, mat_ptr p, mat_ptr g);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

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

// Pruning of sol. space. ------------------------------------------------------

// Thin the possible set of isomorphisms. The general idea is that, if we have
// M(x,y) then any neighbor v of x in P should have a neighbor u in G s.t.
// M(v,u) holds. Otherwise M(x,y) cannot be a solution.
//
// prune(M, P, G)
// do
//   for all (i,j) where M is 1
//     for all neighbors x of vi in P
//       if there is no neighbor y of vj in G s.t. M(x,y)
//         M(i,j) := F
// while M changed
static void
prune(mat_ptr iso, mat_ptr p, mat_ptr g)
{
    bool changed;
    bool neighbor;
    do {
        changed = FALSE;
		for(unint i=0; i<iso->rows; i++) {
            for(unint j=0; j<iso->cols; j++) {
                if(iso->mat[i][j]) {
                    neighbor = FALSE;
					for(unint k=0; k<p->cols; k++) {
						if(p->mat[i][k]) {
							for(unint l=0; l<g->cols; l++) {
                                if(g->mat[j][l]) {
									if(iso->mat[k][l]) {
										neighbor = TRUE;
										break;
									}
                                }
							}
							if(!neighbor) {
								iso->mat[i][j] = FALSE;
								changed = TRUE;
                                break;
							}
						}
                    }
                }
            }
        }
    } while(changed);
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
					if(iso->mat[l][j] && g->mat[k][l]) {
						t2 = TRUE;
                        break;
                    }
                }
                if(iso->mat[i][k] && t2) {
					t1 = TRUE;
                    break;
                }
            }
            if(p->mat[i][j] != t1) {
				return FALSE;
            }
        }
    }
    return TRUE;
}

// Matching algo. --------------------------------------------------------------

// recurse(used_columns, cur_row, G, P, M)
//   if cur_row = num_rows(M)
//     if M is an isomorphism:
//        output yes and end the algorithm
//   M' = M
//   prune(M')
//   for all unused columns c
//     set column c in M' to 1 and other columns to 0
//     mark c as used
//     recurse(used_column, cur_row+1, G, P, M')
//     mark c as unused
//   output no
static void
recurse(mat_ptr iso, mat_ptr p, mat_ptr g)
{
	iso = p;
    p   = g;
    g   = iso;
}

/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

void
Iso_Init()
{
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
    P = NULL;
    G = NULL;
    M = NULL;
    matrix_free(NULL);
    matrix_allocate(0,0);
    prune(NULL,NULL,NULL);
    test_isomorphism_formatting(NULL);
    test_isomorphism_match(NULL,NULL,NULL);
    recurse(NULL,NULL,NULL);
}

/******************************************************************************/
/*                                OLD FUNCTIONS                               */
/******************************************************************************/

/* static iso_mat_ptr */
/* create_iso_mat(fsm_ptr fsm) */
/* { */
/*     unint        _node_count = COUNT_BUF(&(fsm->nodes)); */
/*     adj_mat_rec *_adj_mat; */
/*     hash_record *_node_names; */
/*     // Allocate. ------------------------------------------- */
/*     iso_mat_ptr iso_mat; */
/*     iso_mat = (iso_mat_ptr) new_rec(&iso_mat_mgr); */
/*     // */
/*     _adj_mat = (iso_mat->adj_mat); */
/*     allocate_adjacency_matrix(_adj_mat, _node_count, _node_count); */
/*     // */
/*     _node_names = &(iso_mat->node_names); */
/*     create_hash(_node_names, _node_count, int_hash, int_equ); */
/*     // Initialize. ----------------------------------------- */
/*     unint ix = 0; */
/*     nnode_ptr np; */
/*     FOR_BUF(&(fsm->nodes), nnode_rec, np) { */
/*         unint n_idx = np->idx; */
/* 	if (*get_real_name(np->vec, n_idx-(np->vec->map->from)+1) == '!') { */
/* 	    continue; */
/* 	} */
/*         // Associate nodes with their index in the adj. matrix. */
/*         unint  n_ix; */
/*         unint *n_ix_ptr = (unint*) find_hash(_node_names, INT2PTR(n_idx)); */
/*         if (n_ix_ptr == NULL) { */
/* 	    n_ix = ix++; */
/*             insert_hash(_node_names, INT2PTR(n_idx), INT2PTR(n_ix)); */
/*         } else { */
/* 	    n_ix = *n_ix_ptr; */
/*         } */
/*         // Build adj. matrix. */
/* 	for(idx_list_ptr ilp = np->fanouts; ilp != NULL; ilp = ilp->next) { */
/* 	    ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(&(fsm->composites), ilp->idx); */
/* 	    FOREACH_NODE(t_idx, cp->outs) { */
/*                 unint  t_ix; */
/*                 unint *t_ix_ptr = find_hash(_node_names, INT2PTR(t_idx)); */
/*                 if (t_ix_ptr == NULL) { */
/* 		    t_ix = ix++; */
/*                     insert_hash(_node_names, INT2PTR(t_idx), INT2PTR(t_ix)); */
/*                 } else { */
/* 		    t_ix = *t_ix_ptr; */
/*                 } */
/*                 _adj_mat->mat[n_ix][t_ix] = 1; */
/*             } */
/* 	} */
/*     } */
/*     // */
/*     return iso_mat; */
/* } */

