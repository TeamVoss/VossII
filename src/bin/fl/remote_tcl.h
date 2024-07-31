//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1990                        *
*                                                                   *
*********************************************************************/
/* tcl.h -- header for tcl.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */


/* -------- Function prototypes for exported functions -------- */
void        Init_tcl();
bool	    Register_tcl_callback(string name, symbol_tbl_ptr stbl);
void        Tcl_callback_eval(string cmd, int rid, FILE *tcl_fp);
void        Mark_tcl_callbacks();
bool	    Import_tcl_function(string name, typeExp_ptr type);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef TCL_H
#define TCL_H
#include "fl.h"  /* Global data types and include files               */

#define TCL_MAGIC_NUMBER 235821234
typedef struct tcl_callback_rec *tcl_callback_ptr;
typedef struct tcl_callback_rec {
        string          name;
        typeExp_ptr     type;
        g_ptr           fun;
} tcl_callback_rec;


#endif /* TCL_H */
#endif /* EXPORT_FORWARD_DECL */
