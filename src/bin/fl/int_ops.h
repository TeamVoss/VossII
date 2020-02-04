//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
*********************************************************************/
/* int_ops.h -- header for int_ops.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    Int_ops_Init();
void	    Int_ops_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef INT_OPS_H
#define INT_OPS_H
#include "fl.h"	/* Global data types and include files 		     */


#endif /* INT_OPS_H */
#endif /* EXPORT_FORWARD_DECL */
