//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
*********************************************************************/
/* voss_strings.h -- header for voss_strings.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct range_rec      *range_ptr;
typedef struct vec_rec        *vec_ptr;
typedef struct vec_list_rec   *vec_list_ptr;
typedef struct sname_list_rec *sname_list_ptr;
typedef struct vector_db_rec  *vector_db_ptr;
typedef struct merge_list_rec *merge_list_ptr;
typedef struct string_op_rec  *string_op_ptr;

/* ----- Function prototypes for public functions ----- */
void	       Strings_Init();
string_op_ptr  Begin_string_ops();
void	       End_string_ops(string_op_ptr sop);
void	       Strings_Install_Functions();
g_ptr	       Vec2nodes(string name);
vec_ptr	       Split_vector_name(string_op_ptr sop, string name);
string	       Get_vector_signature(string_op_ptr sop, vec_ptr vp);
int	       Get_Vector_Size(string vec);
vec_list_ptr   Expand_vector(string_op_ptr sop, vec_ptr vec);
sname_list_ptr Get_expanded_version(string_op_ptr sop, string name);
g_ptr	       Merge_Vectors(g_ptr nds, bool non_contig_vecs);
g_ptr	       Extract_Vectors(g_ptr nds, bool non_contig_vecs);
bool           Check_vector_overlap(vec_ptr v1, vec_ptr v2);
bool           Check_range_overlap(range_ptr r1, range_ptr r2);
sname_list_ptr Show_vector(string_op_ptr sop, vec_ptr vec, bool non_contig_vec);
sname_list_ptr Show_vectors(string_op_ptr sop,
			    vec_list_ptr vecs, bool non_contig_vecs);

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
#if 1
void           DBG_print_range(range_ptr rp);
void           DBG_print_vec(vec_ptr vp);
void           DBG_print_vec_list(vec_list_ptr vlp);
void           DBG_print_merge_list(merge_list_ptr mlp);
void           DBG_print_sname_list(sname_list_ptr slp);
#endif

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef VOSS_STRING_H
#define VOSS_STRING_H
#include "fl.h"	/* Global data types and include files 		     */

typedef enum {TXT, INDEX} vec_type;

typedef struct range_rec {
	int	       upper;
	int	       lower;
	range_ptr      next;
} range_rec;

typedef union  {
        string     name;
        range_ptr  ranges;
    } vec_rec_content;

typedef struct vec_rec {
    vec_type		type;
    vec_rec_content	u;		    // SEL: ->type -> TXT INDEX
    vec_ptr		next;
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

typedef struct string_op_rec {
	bool	    in_use;
	ustr_mgr    tmp_string_mgr;
	rec_mgr	    vector_rec_mgr;	    // TYPE: vec_rec
	rec_mgr	    vec_list_rec_mgr;	    // TYPE: vec_list_rec
	rec_mgr	    range_rec_mgr;	    // TYPE: range_rec
	rec_mgr	    sname_list_rec_mgr;	    // TYPE: sname_list_rec
	rec_mgr	    merge_list_rec_mgr;	    // TYPE: merge_list_rec
} string_op_rec;

typedef struct vector_db_rec	{
	string_op_ptr	sop;
	hash_record	sig2vec_list;	    // TYPE: string -> vec_list_ptr
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

#endif /* VOSS_STRING_H */
#endif /* EXPORT_FORWARD_DECL */
