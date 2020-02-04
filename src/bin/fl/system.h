//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
*********************************************************************/
/* system.h -- header for system.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    System_Init();
void	    System_Install_Functions();
void	    Init_Paths(char* bin_path, char* lib_path);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef SYSTEM_H
#define SYSTEM_H
#include "fl.h"	/* Global data types and include files 		     */


#endif /* SYSTEM_H */
#endif /* EXPORT_FORWARD_DECL */
