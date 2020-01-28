/********************************************************************
*                                                                   *
*     Copyright (C) 2019 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* bv.h -- header for bv.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Bv_Init();
void	    Bv_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef BV_H
#define BV_H
#include "fl.h" /* Global data types and include files               */

typedef struct bv_rec    *bv_ptr;
typedef struct bv_rec {
    union {
	g_ptr	    l;
	bv_ptr	    next;
    }			    u;
    unsigned char	    flag:1;
} bv_rec;

#endif /* BV_H */
#endif /* EXPORT_FORWARD_DECL */
