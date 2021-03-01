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

typedef struct key_rec *key_ptr;
typedef struct key_rec {
    vec_ptr vec;
    key_ptr next;
} key_rec;

typedef struct key_lst *key_lst_ptr;
typedef struct key_lst {
    key_ptr     key;
    key_lst_ptr next;
} key_lst;

typedef struct bkt_rec *bkt_ptr;
typedef struct bkt_rec {
    unint   lbl;
    vec_ptr vec;
    bkt_ptr next;
} bkt_rec;

typedef struct mat_rec *mat_ptr;
typedef struct mat_rec {
    bool  **mat;
    unint rows;
    unint cols;
} mat_rec;

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

#endif /* ISO_H */
#endif /* EXPORT_FORWARD_DECL */
