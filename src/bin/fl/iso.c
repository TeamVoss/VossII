//-------------------------------------------------------------------
// Copyright 2020 ...
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: ..., 2020      			*/
/*									*/
/************************************************************************/
#include "graph.h"
#include "fsm.h"

#include <stdbool.h>

/* ------------- Global variables ------------- */
// ...

/********* Global variables referenced **********/
// ...

/* ------------ Private variables ------------- */
static bool **M;
static int  M_rows;
static int  M_cols;

/***** Forward definitions local functions ******/
static void recurse();
static void prune();

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

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

static void
create_adjacency_matrix()
{
  for (int i=0; i<M_rows; i++) {
    for (int j=0; j<M_cols; j++) {
      M[i][j] = 0;
    }
  }
  
}

static void
allocate_adjacency_matrix(int rows, int cols)
{
  M_rows = rows;
  M_cols = cols;
  M = Malloc(M_rows*sizeof(bool*));
  M[0] = Malloc(M_rows*M_cols*sizeof(bool));
  for(int i=1; i<M_rows; i++) {
    M[i] = M[0] + i * M_cols;
  }
}

static void
free_adjacency_matrix()
{
  Free((void *)M[0]);
  Free((void *)M);
}

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Iso_Init()
{
  // ...
}

void
Iso_Install_Functions()
{
  // ...
}

// g_ptr r = GET_APPLY_RIGHT(redex);
//   = { #define GET_APPLY_RIGHT(np)     M_GET_APPLY_RIGHT(np) }
// g_ptr r = M_GET_APPLY_RIGHT(redex);
//   = { #define M_GET_APPLY_RIGHT(np)   ((g_ptr) GET_RIGHT(np)) }
// g_ptr r = ((g_ptr) GET_RIGHT(redex));
//   = { #define GET_RIGHT(np)           (((ui)((np)->R)) & ~0x3) }
// g_ptr r = (((ui)((redex)->R)) & ~0x3);

// g_ptr g_fsm;
// EXTRACT_1_ARG(redex, g_fsm);
//   = { #define EXTRACT_1_ARG(np, a1)   a1 = GET_APPLY_RIGHT(np); }
// g_fsm = GET_APPLY_RIGHT(redex);
//   = { ... as before ... }
// g_fsm = (((ui)((redex)->R)) & ~0x3);

// fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
//   = { #define GET_EXT_OBJ(np)         ((pointer) GET_RIGHT(np)) }
// fsm_ptr fsm = ...
