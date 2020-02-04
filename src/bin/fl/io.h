//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2016                        *
*                                                                   *
********************************************************************/
/* io.h -- Definitions for io.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

#define PROCESS_PRINTF_PAT(pat,lbl, start_of_pat, pat_0, pat_1_to_9,        \
                           pat_minus, pat_percent, pat_star, pat_b, pat_o,  \
                           pat_x,pat_d,pat_s, pat_B, pat_S, pat_error,      \
                           pat_other)\
{									    \
    const char * s = pat;						    \
    while( *s ) {							    \
	if( *s == '%' ) {						    \
	    start_of_pat						    \
	    s++;							    \
	  lbl :								    \
	    switch( *s ) {						    \
		case '0':						    \
		    pat_0						    \
		    s++;						    \
		    goto lbl;						    \
		case '1':						    \
		case '2':						    \
		case '3':						    \
		case '4':						    \
		case '5':						    \
		case '6':						    \
		case '7':						    \
		case '8':						    \
		case '9':						    \
		    pat_1_to_9						    \
		    s++;						    \
		    goto lbl;						    \
		case '-':						    \
		    pat_minus						    \
		    s++;						    \
		    goto lbl;						    \
		case '*':						    \
		    pat_star						    \
		    s++;						    \
		    goto lbl;						    \
		case '%':						    \
		    pat_percent						    \
		    break;						    \
		case 'b':						    \
		    pat_b						    \
		    break;						    \
		case 'o':						    \
		    pat_o						    \
		    break;						    \
		case 'x':						    \
		    pat_x						    \
		    break;						    \
		case 'd':						    \
		    pat_d						    \
		    break;						    \
		case 's':						    \
		    pat_s						    \
		    break;						    \
		case 'B':						    \
		    pat_B						    \
		    break;						    \
		case 'S':						    \
		    pat_S						    \
		    break;						    \
		default:						    \
		    pat_error						    \
		    break;						    \
	    }								    \
	} else {							    \
	    pat_other							    \
	}								    \
	s++;								    \
    }									    \
}

/* ----- Function prototypes for public functions ----- */
void        Init_io();
bool        Fopen(g_ptr *rootp, g_ptr **spp, int *depthp);
bool        Printf(g_ptr *rootp, g_ptr **spp, int *depthp);
bool	    Sscanf(g_ptr *rootp, g_ptr **spp, int *depthp);
bool        Fflush(g_ptr *rootp, g_ptr **spp, int *depthp);
bool        Fclose(g_ptr *rootp, g_ptr **spp, int *depthp);
bool	    Save_graph(string type_sig, string file_name, g_ptr node);
bool	    Load_graph(string type_sig, string file_name, g_ptr redex);
void	    Io_Install_Functions();
int	    Save_get_bool_idx(formula f);
int	    Save_get_bexpr_idx(bexpr be);
int	    Save_get_string_idx(string s);
int	    Save_get_fsm_idx(int ckt);
formula	    Load_get_bool_from_idx(int idx);
bexpr	    Load_get_bexpr_from_idx(int idx);
string	    Load_get_string_from_idx(int idx);
int	    Load_get_fsm_from_idx(int idx);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef IO_H
#define IO_H
#include "fl.h"	/* Global data types and include files 		     */
#include <sys/stat.h>
#include <sys/types.h>
#include <ctype.h>

typedef struct io_rec {
    char    *name;
    FILE    *fp;
    bool    is_pipe;
    char    *mode;
    bool    writable;
    io_ptr  next;
} io_rec;

#endif /* IO_H */
#endif /* EXPORT_FORWARD_DECL */
