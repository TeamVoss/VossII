//-------------------------------------------------------------------
// Copyright 2020 Markus Aronsson
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*     Original author: Markus Aronsson, 2020                           */
/*									*/
/************************************************************************/
#include "strings.h"
#include "graph.h"
#include "fsm.h"
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
static rec_mgr     iso_mat_mgr;
static iso_mat_ptr P; // Piece.
static iso_mat_ptr G; // Puzzle.
static rec_mgr     adj_mat_mgr;
static adj_mat_ptr M;
//
static typeExp_ptr  fsm_handle_tp;

// Forward definitions local functions -----------------------------------------
static void recurse();
static void prune();
//
static adj_mat_ptr create_adj_mat();
static unint degree(adj_mat_ptr adj_mat, unint node);
//
static iso_mat_ptr create_iso_mat(fsm_ptr fsm);
static void allocate_adjacency_matrix(adj_mat_ptr adj_mat, unint R, unint C);
static void free_adjacency_matrix(adj_mat_ptr adj_mat);
//
static void initialize_isomatch(fsm_ptr piece, fsm_ptr puzzle);
//
static void test(g_ptr redex);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

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
recurse()
{

}

// prune(...)
// do
//   for all (i,j) where M is 1
//     for all neighbors x of vi in P
//       if there is no neighbor y of vj s.t. M(x,y)=1
//         M(i,j)=0
// while M was changed
static void
prune()
{

}

// Adjacency matrix ------------------------------------------------------------

static adj_mat_ptr
create_adj_mat()
{
    unint _nodes_P = P->adj_mat->rows;
    unint _nodes_G = G->adj_mat->rows;
    // ----------------------------------------
    // Allocate.
    adj_mat_ptr adj_mat;
    adj_mat = (adj_mat_ptr) new_rec(&adj_mat_mgr);
    // todo: not sure about this one...
    allocate_adjacency_matrix(adj_mat, _nodes_P, _nodes_G);
    // ----------------------------------------
    // Initialize.
    for (unint i = 0; i<_nodes_P; i++) {
	for (unint j = 0; j<_nodes_G; j++) {
	    if (degree(P->adj_mat, i) >= degree(G->adj_mat, j)) {
		continue;
            }
            // More checks.
            // Add link.
        }
    }
    //
    return adj_mat;
}

static unint
degree(adj_mat_ptr adj_mat, unint node)
{
    unint *row   = adj_mat->mat[node];
    unint degree = 0;
    for (unint i = 0; i<adj_mat->cols; i++) {
	if (row[i] == 1) {
	    degree++;
        }
    }
    return degree;
}

//static unint
//kind(adj_mat_ptr adj_mat, unint node)
//{
//    return 0;
//}

// Piece and Puzzle matrices ---------------------------------------------------

static iso_mat_ptr
create_iso_mat(fsm_ptr fsm)
{
    unint        _node_count = COUNT_BUF(&(fsm->nodes));
    adj_mat_rec *_adj_mat;
    hash_record *_node_names;
    // Allocate. -------------------------------------------
    iso_mat_ptr iso_mat;
    iso_mat = (iso_mat_ptr) new_rec(&iso_mat_mgr);
    //
    _adj_mat = (iso_mat->adj_mat);
    allocate_adjacency_matrix(_adj_mat, _node_count, _node_count);
    //
    _node_names = &(iso_mat->node_names);
    create_hash(_node_names, _node_count, int_hash, int_equ);
    // Initialize. -----------------------------------------
    unint ix = 0;
    nnode_ptr np;
    FOR_BUF(&(fsm->nodes), nnode_rec, np) {
        unint n_idx = np->idx;
	if (*get_real_name(np->vec, n_idx-(np->vec->map->from)+1) == '!') {
	    continue;
	}
        // Associate nodes with their index in the adj. matrix.
        unint  n_ix;
        unint *n_ix_ptr = (unint*) find_hash(_node_names, INT2PTR(n_idx));
        if (n_ix_ptr == NULL) {
	    n_ix = ix++;
            insert_hash(_node_names, INT2PTR(n_idx), INT2PTR(n_ix));
        } else {
	    n_ix = *n_ix_ptr;
        }
        // Build adj. matrix.
	for(idx_list_ptr ilp = np->fanouts; ilp != NULL; ilp = ilp->next) {
	    ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(&(fsm->composites), ilp->idx);
	    FOREACH_NODE(t_idx, cp->outs) {
                unint  t_ix;
                unint *t_ix_ptr = find_hash(_node_names, INT2PTR(t_idx));
                if (t_ix_ptr == NULL) {
		    t_ix = ix++;
                    insert_hash(_node_names, INT2PTR(t_idx), INT2PTR(t_ix));
                } else {
		    t_ix = *t_ix_ptr;
                }
                _adj_mat->mat[n_ix][t_ix] = 1;
            }
	}
    }
    //
    return iso_mat;
}

static void
allocate_adjacency_matrix(adj_mat_ptr adj_mat, unint R, unint C)
{
    adj_mat->rows   = R;
    adj_mat->cols   = C;
    adj_mat->mat    = Malloc(R*sizeof(int*));
    adj_mat->mat[0] = Calloc(R*C*sizeof(int));
    for(unint i=1; i<R; i++) {
	adj_mat->mat[i] = adj_mat->mat[0]+i*C;
    }
}

static void
free_adjacency_matrix(adj_mat_ptr adj_mat)
{
    Free((void *)adj_mat->mat[0]);
    Free((void *)adj_mat->mat);
}

static void
initialize_isomatch(fsm_ptr piece, fsm_ptr puzzle)
{
    P = create_iso_mat(piece);
    G = create_iso_mat(puzzle);
    create_adj_mat();
}

static void
test(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    MAKE_REDEX_NIL(redex);
    //
    create_iso_mat(fsm);
    //
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

void
Iso_Init()
{
    new_mgr(&iso_mat_mgr, sizeof(iso_mat_rec));
    new_mgr(&adj_mat_mgr, sizeof(adj_mat_rec));
    //
    fsm_handle_tp = Get_Type("fsm", NULL, TP_INSERT_FULL_TYPE);
}

void
Iso_Install_Functions()
{
    Add_ExtAPI_Function("test", "1", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_void()),
			test);    

}

/******************************************************************************/
/*                               DUMMY FUNCTIONS                              */
/******************************************************************************/

void
_DuMMy_iso()
{
    recurse();
    prune();
    //
    create_adj_mat();
    degree(NULL,0);
    //
    create_iso_mat(NULL);
    allocate_adjacency_matrix(NULL,0,0);
    free_adjacency_matrix(NULL);
    initialize_isomatch(NULL,NULL);
    //
    M->rows=0;
}
