//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1990                        *
*                                                                   *
*********************************************************************/
/* error.h -- header for error.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* -------- Function prototypes for exported functions -------- */
void FP(odests fp, const string format, ...);
void Flush(odests fp);
void DBG_printf(const string fmt, ...);
void ErrMsg(const string fmt, ...);
void Eprintf(const string fmt, ...);
void Rprintf(const string fmt, ...);
void Info_printf(const string fmt, ...);
void Wprintf(const string fmt, ...);
char *Fail_pr(const string fmt, ...);
void Fail_append(const string fmt, ...);
string tprintf(const string fmt, ...);
string tappend(const string fmt, ...);
string gen_tprintf(tstr_ptr tp, const string fmt, ...);
string gen_tappend(tstr_ptr tp, const string fmt, ...);
string make_tcl_safe(string s);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef ERROR_H
#define ERROR_H
#include "fl.h"	/* Global data types and include files 		     */
#include <stdarg.h>

#endif /* ERROR_H */
#endif /* EXPORT_FORWARD_DECL */
