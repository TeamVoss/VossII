//-------------------------------------------------------------------
// Copyright 2020 Markus Aronsson
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/*     Original author: Markus Aronsson, 2020                                 */
/*                                                                            */
/******************************************************************************/
/* iso.h -- header for iso.c */
#ifdef EXPORT_FORWARD_DECL
// Forward declarations that need to be exported to earlier .h files -----------

// Function prototypes for public functions ------------------------------------
void	    Iso_Init();
void	    Iso_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
// Main include file -----------------------------------------------------------
#ifndef ISO_H
#define ISO_H

#include "fl.h"

// Matrix. -----------------------------
typedef struct mat_rec *mat_ptr;
typedef struct mat_rec {
    int   mark;
    //
    bool  **mat;
    unint rows;
    unint cols;
} mat_rec;

// Adj. matrix construction ------------
typedef struct bkt_rec *bkt_ptr;
typedef struct bkt_rec {
    unint   lbl;
    vec_ptr vec;
    bkt_ptr next;
} bkt_rec;

typedef struct key_rec *key_ptr;
typedef struct key_rec {
    string  lbl;
    vec_ptr vec;
    key_ptr next;
} key_rec;

typedef struct key_lst_rec *key_lst_ptr;
typedef struct key_lst_rec {
    key_ptr     key;
    key_lst_ptr next;
} key_lst_rec;

// Iso. matrix construction ------------
typedef struct sig_rec *sig_ptr;
typedef struct sig_rec {
    string  sha;
    int     fp;
    sig_ptr next;
} sig_rec;

// ? -----------------------------------
typedef struct point_rec *point_ptr;
typedef struct point_rec {
    unint     row;
    unint     col;
    point_ptr next;
} point_rec;

typedef struct search_rec *search_ptr;
typedef struct search_rec {
    // Memory managers.
    rec_mgr    change_mgr;
    buffer     results;
    // Bookkeeping.
    unint      start;
    unint      row;
    unint      *cols;    // A
    bool       *used;    // B
    point_ptr  *changes; // A
    mat_ptr    copy;     // AxB
    // Main matrices.
    mat_ptr    isomatch; // AxB
    mat_ptr    needle;   // AxA
    mat_ptr    haystack; // BxB
} search_rec;

// Short-hands -------------------------

#define FORMAL_OF_CONS(fa)                                                     \
    split_vector(GET_STRING(GET_FST(GET_CONS_HD(fa))))

#define ACTUAL_OF_CONS(fa)                                                     \
    split_vector(GET_STRING(GET_CONS_HD(fa)))

#define FOREACH_FORMAL(vec, fa)                                                \
    for( g_ptr li = fa                                                         \
       ; !IS_NIL(li) && (vec = FORMAL_OF_CONS(li), TRUE)                       \
       ; li = GET_CONS_TL(li))

#define FOREACH_ACTUAL(vec, fa)                                                \
    for( g_ptr li = fa                                                         \
        ; !IS_NIL(li)                                                          \
        ; li = GET_CONS_TL(li))                                                \
        for( g_ptr as = GET_SND(GET_CONS_HD(li))                               \
           ; !IS_NIL(as) && (vec = ACTUAL_OF_CONS(as), TRUE)                   \
           ; as = GET_CONS_TL(as))                                             \

#define FOREACH_KEY(key, keys)                                                 \
    for(unint i = 0; keys != NULL; i++, keys = keys->next)                     \
        for(key = keys->key; key != NULL; key = key->next)    

#endif /* ISO_H */
#endif /* EXPORT_FORWARD_DECL */
