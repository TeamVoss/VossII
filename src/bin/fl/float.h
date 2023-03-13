//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2018                        *
*                                                                   *
*********************************************************************/
/* float.h -- header for float.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Float_Init();
void	    Float_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef FLOAT_H
#define FLOAT_H
#include "fl.h"
typedef struct float_rec    *float_ptr;
typedef struct float_rec {
    union {
	double	    f;
	float_ptr   next;
    }				u;
    unsigned char		flag:1;
} float_rec;

void make_redex_float(g_ptr redex, double d);
g_ptr Make_float(double d);

#endif /* FLOAT_H */
#endif /* EXPORT_FORWARD_DECL */
