//-------------------------------------------------------------------
// Copyright 2020 Markus Aronsson
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/*     Original author: Markus Aronsson, 2020                                 */
/*                                                                            */
/******************************************************************************/
/* iso.h -- header for iso.c */
#ifdef EXPORT_FORWARD_DECL
// Forward declarations that need to be exported to earlier .h files -----------

// Function prototypes for public functions ------------------------------------
void	    Iso_Init();
void	    Iso_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
// Main include file -----------------------------------------------------------
#ifndef ISO_H
#define ISO_H

#include "fl.h"

typedef struct adj_mat_rec *adj_mat_ptr;
typedef struct adj_mat_rec {
    unint **mat;
    unint rows;
    unint cols;
} adj_mat_rec;

typedef struct iso_mat_rec *iso_mat_ptr;
typedef struct iso_mat_rec {
    adj_mat_ptr adj_mat;    // Adjacency matrix.
    hash_record node_names; // Name table.
} iso_mat_rec;

#endif /* ISO_H */
#endif /* EXPORT_FORWARD_DECL */
