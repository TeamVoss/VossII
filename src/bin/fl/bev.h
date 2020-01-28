/********************************************************************
*                                                                   *
*     Copyright (C) 2019 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* bev.h -- header for bev.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Bev_Init();
void	    Bev_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef BEV_H
#define BEV_H
#include "fl.h" /* Global data types and include files               */

typedef struct bev_rec    *bev_ptr;
typedef struct bev_rec {
    union {
	g_ptr	    l;
	bev_ptr	    next;
    }			    u;
    unsigned char	    flag:1;
} bev_rec;

#endif /* BEV_H */
#endif /* EXPORT_FORWARD_DECL */
