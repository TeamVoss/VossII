/********************************************************************
*                                                                   *
*     Copyright (C) 1990 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* symbol_tbl.h -- header for symbol_tbl.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* -------- Function prototypes for exported functions -------- */
void		Init_symb_tbl(void);
string		wastrsave(str_mgr_ptr smp, string s);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef SYMBOL_TBL_H
#define SYMBOL_TBL_H
#include "fl.h"	/* Global data types and include files 		     */

#endif /* SYMBOL_TBL_H */
#endif /* EXPORT_FORWARD_DECL */
