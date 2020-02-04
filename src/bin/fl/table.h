//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2019                        *
*                                                                   *
*********************************************************************/
/* table.h -- header for table.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct ilist_rec    *ilist_ptr;

/* ----- Function prototypes for public functions ----- */
void	    Table_Init();
void	    Table_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef TABLE_H
#define TABLE_H
#include "fl.h"	/* Global data types and include files 		     */


typedef struct table_rec    *table_ptr;
typedef struct table_rec {
    hash_record		tbl;
    table_ptr		next;
    unsigned char       flag:1;
} table_rec;


#endif /* TABLE_H */
#endif /* EXPORT_FORWARD_DECL */
