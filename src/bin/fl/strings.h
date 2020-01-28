/********************************************************************
*                                                                   *
*     Copyright (C) 2017 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* strings.h -- header for strings.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct vec_rec	*vec_ptr;

/* ----- Function prototypes for public functions ----- */
void	    Strings_Init();
void	    Strings_Install_Functions();
g_ptr	    Vec2nodes(string name);
vec_ptr	    Split_vector_name(ustr_mgr *str_mgr_ptr,
			      rec_mgr  *vector_mgrp,
			      rec_mgr  *range_mgrp,
			      string vec);
string	    Get_vector_signature(ustr_mgr *str_mgr_ptr, vec_ptr vp);
int	    Get_Vector_Size(string vec);
g_ptr	    Merge_Vectors(g_ptr nds, bool non_contig_vecs);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef STRING_H
#define STRING_H
#include "fl.h"	/* Global data types and include files 		     */

typedef enum {TXT, INDEX}   vec_type;

typedef struct range_rec	*range_ptr;
typedef struct range_rec {
	int	    upper;
	int	    lower;
	range_ptr   next;
} range_rec;

typedef struct vec_rec {
    vec_type		    type;
    union {
	string	    name;
	range_ptr   ranges;
    }			    u;
    vec_ptr		    next;
} vec_rec;

typedef struct sname_list_rec	*sname_list_ptr;
typedef struct sname_list_rec {
    string	    name;
    sname_list_ptr  next;
} sname_list_rec;

typedef struct merge_list_rec	*merge_list_ptr;
typedef struct merge_list_rec {
    vec_ptr	    vec;
    string	    name_signature;
    merge_list_ptr  prev;
    merge_list_ptr  next;
} merge_list_rec;

#endif /* STRING_H */
#endif /* EXPORT_FORWARD_DECL */
