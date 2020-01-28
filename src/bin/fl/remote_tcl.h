/********************************************************************
*                                                                   *
*     Copyright (C) 1990 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* tcl.h -- header for tcl.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */


/* -------- Function prototypes for exported functions -------- */
void        Init_tcl();
bool	    Register_tcl_callback(string name, symbol_tbl_ptr stbl);
bool        Tcl_callback_eval(string cmd, string *resp);
void        Mark_tcl_callbacks();

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
