//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2019                        *
*                                                                   *
*********************************************************************/
/* bv.h -- header for bv.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct bv_rec    *bv_ptr;

/* ----- Function prototypes for public functions ----- */
void	    Bv_Init();
void	    Bv_Install_Functions();
void	    Bv_GC();
g_ptr	    Ite_bv_list(formula cond, g_ptr l1, g_ptr l2);
g_ptr	    Bv_get_list(bv_ptr bp);
g_ptr	    Aint2bv(arbi_T ai);
void	    MAKE_REDEX_BV(g_ptr redex, g_ptr list);
g_ptr	    Make_bv(g_ptr list);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef BV_H
#define BV_H
#include "fl.h" /* Global data types and include files               */

typedef struct bv_rec {
    union {
	g_ptr	    l;
	bv_ptr	    next;
    }			    u;
    unsigned char	    flag:1;
} bv_rec;


typedef struct gen_mc_rec   *gen_mc_ptr;
typedef struct gen_mc_rec {
    formula	fun;
    g_ptr	result;
} gen_mc_rec;

#endif /* BV_H */
#endif /* EXPORT_FORWARD_DECL */
