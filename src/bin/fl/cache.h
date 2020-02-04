//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1995                        *
*                                                                   *
*********************************************************************/
/* cache.h -- header for cache.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct g_cache_rec	*g_cache_ptr;

/* -------- Function prototypes for exported functions -------- */
void		Init_G_Caches();
void		Cache_ops_Install_Functions();
int		Make_g_cache();
g_ptr		Find_in_g_cache(int cache_nbr, g_ptr argl);
void		Insert_in_g_cache(int cache_nbr, g_ptr argl, g_ptr res);
void		Mark_G_Cache(int cache_tbl);
void		Sweep_G_caches();
int		Make_RefVar();
g_ptr		Get_RefVar(int ref_var);
void		Set_RefVar(int ref_var, g_ptr res);
void		Mark_RefVar(int ref_var);
void		Sweep_RefVars();
unint		G_rec_hash(pointer p, unint size);
bool		G_rec_equ(pointer p1, pointer p2);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef CACHE_H
#define CACHE_H
#include "fl.h"	/* Global data types and include files 		     */

#define MAX_CACHE_LOAD	85	/* Maximum % load before increasing cache */

typedef struct g_cache_rec {
	unint	size;		/* Number of cache lines 		  */
	unint	load;		/* The number of cache-lines used	  */
	bool	mark;		/* For g.c.				  */
	g_ptr	*tbl;		/* Cache table				  */
} g_cache_rec;

#endif /* CACHE_H */
#endif /* EXPORT_FORWARD_DECL */


