//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
*********************************************************************/
/* list_ops.h -- header for list_ops.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    List_ops_Init();
void	    List_ops_Install_Functions();
void	    List_GC();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef LIST_OPS_H
#define LIST_OPS_H
#include "fl.h"	/* Global data types and include files 		     */

typedef struct assoc_tbl_rec	*assoc_tbl_ptr;

typedef struct assoc_tbl_rec {
    bool	    in_use;
    g_ptr	    assoc_list;
    g_ptr	    next_to_insert;
    hash_record	    tbl;
    assoc_tbl_ptr   free_list;
} assoc_tbl_rec;


#endif /* LIST_OPS_H */
#endif /* EXPORT_FORWARD_DECL */
