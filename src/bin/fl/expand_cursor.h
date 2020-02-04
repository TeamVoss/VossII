//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1995                        *
*                                                                   *
*********************************************************************/
/* expand_cursor.h -- header for expand_cursor.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* -------- Function prototypes for exported functions -------- */
void Print_line(string s, odests fp);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef EXPAND_CURSOR_H
#define EXPAND_CURSOR_H
#include "fl.h"  /* Global data types and include files               */

#define LINE_SIZE 	85

typedef struct line_rec	*line_ptr;
typedef struct line_rec {
	char		*line;
	int		allocated;
	int		used;
	line_ptr	up;
	line_ptr	down;
} line_rec;

#endif /* EXPAND_CURSOR_H */
#endif /* EXPORT_FORWARD_DECL */
