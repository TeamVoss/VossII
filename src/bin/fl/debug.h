//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
********************************************************************/
/* debug.h -- Definitions for debug.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	Start_Compare_Fun(int id);
bool	Compare_Graphs(int id, g_ptr g);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef DEBUG_H
#define DEBUG_H
#include "fl.h"	/* Global data types and include files 		     */
#include <unistd.h>

#endif /* DEBUG_H */
#endif /* EXPORT_FORWARD_DECL */
