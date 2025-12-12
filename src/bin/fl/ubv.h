//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2019                        *
*                                                                   *
*********************************************************************/
/* bev.h -- header for bev.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Ubv_Init();
void	    Ubv_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef BEV_H
#define BEV_H
#include "fl.h" /* Global data types and include files               */

typedef struct ubv_rec    *ubv_ptr;
typedef struct ubv_rec {
    union {
	g_ptr	    l;
	ubv_ptr	    next;
    }			    u;
    unsigned char	    flag:1;
} ubv_rec;

#endif /* BEV_H */
#endif /* EXPORT_FORWARD_DECL */
