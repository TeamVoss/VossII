//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
*********************************************************************/
/* strings.h -- header for strings.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct range_rec      *range_ptr;
typedef struct vec_rec        *vec_ptr;
typedef struct vec_list_rec   *vec_list_ptr;
typedef struct sname_list_rec *sname_list_ptr;
typedef struct vector_db_rec  *vector_db_ptr;
typedef struct merge_list_rec *merge_list_ptr;

/* ----- Function prototypes for public functions ----- */
void	       Strings_Init();
void	       Strings_Install_Functions();
g_ptr	       Vec2nodes(string name);
vec_ptr	       Split_vector_name(
                   ustr_mgr *string_mgrp, rec_mgr *vector_mgrp,
                   rec_mgr *range_mgrp, string vec);
string         Get_vector_signature(ustr_mgr *string_mgrp, vec_ptr vp);
int	       Get_Vector_Size(string vec);
vec_list_ptr   Expand_vector(
                   rec_mgr *vector_list_mgr, rec_mgr *vector_mgr,
                   rec_mgr *range_mgr, vec_ptr vec);
vec_list_ptr   Merge_Vectors_gen(rec_mgr *vec_list_mgr, vec_list_ptr vecs);
g_ptr	       Merge_Vectors(g_ptr nds, bool non_contig_vecs);
g_ptr	       Extract_Vectors(g_ptr nds, bool non_contig_vecs);
bool           Check_vector_overlap(vec_ptr v1, vec_ptr v2);
bool           Check_range_overlap(range_ptr r1, range_ptr r2);
sname_list_ptr Show_vector(
                   rec_mgr *sname_list_mgrp, vec_ptr vec,
                   bool non_contig_vec);
sname_list_ptr Show_vectors(
                   rec_mgr *sname_list_mgrp, vec_list_ptr vecs,
                   bool non_contig_vecs);
unint          range_hash(pointer k, unint n);
int            range_cmp(pointer k1, pointer k2);
bool           range_equ(pointer k1, pointer k2);
unint          vec_hash(pointer k, unint n);
int            vec_cmp(pointer k1, pointer k2);
bool           vec_equ(pointer k1, pointer k2);
vector_db_ptr  VDB_create();
void	       VDB_destroy(vector_db_ptr vdbp);
void	       VDB_Insert_vector(vector_db_ptr vdbp, string vec);
bool	       VDB_has_name_collision(vector_db_ptr vdbp, string vec,
				      bool basename_only);
vec_ptr	       Copy_vector(rec_mgr *vector_mgr_ptr, rec_mgr *range_mgr_ptr,
			   vec_ptr old);
void           DBG_print_range(range_ptr rp);
void           DBG_print_vec(vec_ptr vp);
void           DBG_print_vec_list(vec_list_ptr vlp);
void           DBG_print_merge_list(merge_list_ptr mlp);
void           DBG_print_sname_list(sname_list_ptr slp);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef STRING_H
#define STRING_H
#include "fl.h"	/* Global data types and include files 		     */

typedef enum {TXT, INDEX} vec_type;

typedef struct range_rec {
	int	       upper;
	int	       lower;
	range_ptr      next;
} range_rec;

typedef struct vec_rec {
    vec_type       type;
    union {
        string	   name;
        range_ptr  ranges;
    }              u;
    vec_ptr	       next;
} vec_rec;

typedef struct vec_list_rec {
    vec_ptr      vec;
    vec_list_ptr next;
} vec_list_rec;

typedef struct sname_list_rec {
    string         name;
    sname_list_ptr next;
} sname_list_rec;

typedef struct merge_list_rec {
    vec_ptr	       vec;
    string         name_signature;
    merge_list_ptr prev;
    merge_list_ptr next;
} merge_list_rec;

typedef struct vector_db_rec	{
	ustr_mgr    ustring_mgr;
	rec_mgr	    vec_rec_mgr;
	rec_mgr	    range_rec_mgr;
	rec_mgr	    vec_list_rec_mgr;
	hash_record sig2vec_list;
} vector_db_rec;


#define DBG_PRINT_RNG(rp)                                                      \
    { bool fst = TRUE;							       \
    for(range_ptr rs = rp; rs != NULL; fst = FALSE, rs = rs->next) {           \
        if(rs->upper == rs->lower) { fprintf(stderr, "%s%d", (fst?"":", "), rs->upper); }     \
        else { fprintf(stderr, "%s%d:%d, ", (fst?"":", "), rs->upper, rs->lower); }             \
    }}

#define DBG_PRINT_VEC(vp)                                                      \
    for(vec_ptr vs = vp; vs != NULL; vs = vs->next) {                          \
        if(vs->type == TXT) { fprintf(stderr, "%s", vs->u.name); }             \
        else { DBG_PRINT_RNG(vs->u.ranges); }                                  \
    }

#define DBG_PRINT_VEC_LIST(vlp)                                                \
    { bool fst = TRUE;							       \
    fprintf(stderr, "[");						       \
    for(vec_list_ptr vls = vlp; vls != NULL; fst=FALSE, vls = vls->next) {     \
        if(!fst) fprintf(stderr, ", ");                                        \
        DBG_PRINT_VEC(vls->vec);                                               \
    }                                                                          \
    fprintf(stderr, "]\n"); }

#define DBG_PRINT_MRG_LIST(mlp)                                                \
    merge_list_ptr mls = mlp;                                                  \
    do {                                                                       \
        DBG_PRINT_VEC(mls->vec);                                               \
        fprintf(stderr, "; ");                                                 \
        mls = mls->next;                                                       \
    } while(mls != NULL && mls != mlp);                                        \
    fprintf(stderr, "\n");

#define DBG_PRINT_STR_LIST(slp)                                                \
    { bool fst = TRUE;							       \
    fprintf(stderr, "[");						       \
    for(sname_list_ptr sls = slp; sls != NULL; fst = FALSE, sls = sls->next) { \
        if(!fst) fprintf(stderr, ", ");                                        \
        fprintf(stderr, "%s", sls->name);                                      \
    }                                                                          \
    fprintf(stderr, "]\n"); }

#define DBG_PRINT_FL_LIST(glp)                                                 \
    for(g_ptr gls = glp; !IS_NIL(gls); gls = M_GET_CONS_TL(gls)) {             \
        fprintf(stderr, "%s; ", GET_STRING(M_GET_CONS_HD(gls)));               \
    }                                                                          \
    fprintf(stderr, "\n");

#endif /* STRING_H */
#endif /* EXPORT_FORWARD_DECL */
