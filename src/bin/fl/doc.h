//------------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/* Original author: Carl-Johan Seger 2022                                     */
/*                                                                            */
/******************************************************************************/
/* doc.h -- header for doc.c */
#ifdef EXPORT_FORWARD_DECL

void Doc_Init();
void Doc_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
// Main include file -----------------------------------------------------------
#ifndef DOC_H
#define DOC_H

#include "fl.h"	/* Global data types and include files */

#endif /* DOC_H */
#endif /* EXPORT_FORWARD_DECL */
