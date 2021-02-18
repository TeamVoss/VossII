//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1990                        *
*                                                                   *
*********************************************************************/
/* initialize.h -- header for initialize.c */

#ifdef EXPORT_FORWARD_DECL

/* -------- Function prototypes for exported functions -------- */
void	Init(void);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef INITIALIZE_H
#define INITIALIZE_H
#include "fl.h"	/* Global data types and include files 		     */

#endif /* INITIALIZE_H */
#endif /* EXPORT_FORWARD_DECL */

