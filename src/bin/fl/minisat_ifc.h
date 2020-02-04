//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2016                        *
*                                                                   *
*********************************************************************/
/* minisat_ifc.h -- header for minisat_ifc.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void		TC_Sat_Init();
void		TC_Sat_Free();
int		TC_Sat_NewVar();
int		TC_Sat_Not(int f);
int		TC_Sat_Or(int l, int r);
int		TC_Sat_And(int l, int r);
int		TC_Sat_Model(int cond, int *assumptions, int n);
int		TC_Sat_Get_Model_Value(int var);
//
void		Sat_Init();
int		SAT_Add_Var();
int		SAT_Add_AND_Clauses(int l, int r);
bool		Find_model(int *assumptions, int n, int time_limit);
int		Get_Model_Value(int var);
int		SAT_and_is_same(int l, int r, int c);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef MINISAT_IFC_H
#define MINISAT_IFC_H

#include <errno.h>
#include <signal.h>
#include <zlib.h>

#include "minisat/utils/System.h"
#include "minisat/utils/ParseUtils.h"
#include "minisat/utils/Options.h"
#include "minisat/core/Dimacs.h"
#include "minisat/core/Solver.h"


#endif /* MINISAT_IFC_H */
#endif /* EXPORT_FORWARD_DECL */
