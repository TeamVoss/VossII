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
typedef struct table_rec    *table_ptr;	// ALLOCATE: get_table_rec()


/* ----- Function prototypes for public functions ----- */
void	    Table_Init();
void	    Table_Install_Functions();
table_ptr   get_table_rec();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef TABLE_H
#define TABLE_H
#include "fl.h"	/* Global data types and include files 		     */


typedef struct table_rec {
    hash_record		tbl;	// TYPE: g_ptr -> g_ptr
    table_ptr		next;
    uchar		flag:1;
} table_rec;


#endif /* TABLE_H */
#endif /* EXPORT_FORWARD_DECL */
