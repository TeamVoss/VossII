//-----------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/*		Original author: Carl-Johan Seger, 2017                               */
/*                                                                            */
/******************************************************************************/
#include "strings.h"
#include "graph.h"
#include <limits.h>
#include <stdlib.h>
#include <fnmatch.h>
#include <ctype.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     strings;
extern jmp_buf     *start_envp;

/***** PRIVATE VARIABLES *****/
static string	    s_TXT;
static string	    s_RANGES;
static char         path_buf[PATH_MAX+1];
static ustr_mgr	    lstrings;
static ustr_mgr	    *lstringsp;
static rec_mgr	    vec_rec_mgr;
static rec_mgr	    *vec_rec_mgrp;
static rec_mgr	    range_rec_mgr;
static rec_mgr	    *range_rec_mgrp;
static rec_mgr	    sname_list_rec_mgr;
static rec_mgr	    *sname_list_rec_mgrp;
static rec_mgr      vec_list_rec_mgr;
static rec_mgr      *vec_list_rec_mgrp;
static rec_mgr	    merge_list_rec_mgr;
static rec_mgr	    *merge_list_rec_mgrp;

/* ----- Forward definitions local functions ----- */
static void           begin_vector_ops();
static void           end_vector_ops();
static vec_ptr        split_name(string name);
static range_ptr      make_range_canonical(range_ptr rp);
static vec_ptr        make_vector_ranges_canonical(vec_ptr vp);
static range_ptr      compress_ranges(range_ptr r1, range_ptr r2);
static void           buffer_vectors_list(
                          vec_list_ptr vs, buffer_ptr vec_buf,
                          bool range_canonical);
static void           buffer_vectors_fl(
                          g_ptr r, buffer_ptr vec_buf, bool range_canonical);
static bool	          same_range(range_ptr r1, range_ptr r2);
static int            vec_name_cmp(vec_ptr v1, vec_ptr v2);
static int            nn_cmp(const void *pi, const void *pj);
static merge_list_ptr gen_extract_vectors(buffer_ptr vec_buf);
static merge_list_ptr gen_merge_vectors(buffer_ptr vec_buf);
static void           record_names_fl(sname_list_ptr nlp, g_ptr redex);
static void           merge_vectors_fl(
                          g_ptr list, g_ptr res, bool non_contig_vecs);
static void           extract_vectors_fl(
                          g_ptr list, g_ptr res, bool non_contig_vecs,
                          bool range_canonical);
static int	          vec_size(vec_ptr vec);
static string         show_non_contig_vector(tstr_ptr tstrings, vec_ptr vp);
static sname_list_ptr show_non_contig_vectors(
                          rec_mgr *sname_list_mgrp, tstr_ptr tstrings,
                          vec_list_ptr vlp);
static sname_list_ptr show_contig_vector(
                          rec_mgr *sname_list_mgrp,
                          tstr_ptr tstrings, vec_ptr vp);
static sname_list_ptr show_contig_vectors(
                          rec_mgr *sname_list_mgrp, tstr_ptr tstrings,
                          vec_list_ptr vp);
static sname_list_ptr show_merge_list(
                          rec_mgr *sname_list_mgrp, tstr_ptr tstrings,
                          merge_list_ptr mlp, bool non_contig_vecs);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

static string		mk_name_signature(vec_ptr vp);

void
Strings_Init()
{
    // Initialization code
    s_TXT = Mk_constructor_name("TXT");
    s_RANGES = Mk_constructor_name("RANGES");
}

vec_ptr
Split_vector_name(ustr_mgr *string_mgrp, 
		  rec_mgr  *vector_mgrp,
		  rec_mgr  *range_mgrp,
		  string vec)
{
    ustr_mgr *tmp_lstringsp = lstringsp;
    rec_mgr *tmp_vec_rec_mgrp = vec_rec_mgrp;
    rec_mgr *tmp_range_rec_mgrp = range_rec_mgrp;
    lstringsp = string_mgrp;
    vec_rec_mgrp = vector_mgrp;
    range_rec_mgrp = range_mgrp;
    vec_ptr res = split_name(vec);
    lstringsp = tmp_lstringsp;
    vec_rec_mgrp = tmp_vec_rec_mgrp;
    range_rec_mgrp = tmp_range_rec_mgrp;
    return res;
}

string
Get_vector_signature(ustr_mgr *string_mgrp, vec_ptr vp)
{
    ustr_mgr *tmp = lstringsp;
    lstringsp = string_mgrp;
    string res = mk_name_signature(vp);
    lstringsp = tmp;
    return res;
}

// todo: contig. or non-contig. vector?
g_ptr
Vec2nodes(string name)
{
    begin_vector_ops();
    tstr_ptr tstrings = new_temp_str_mgr();
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    vec_ptr vp = split_name(name);
    vec_list_ptr vlp =
        Expand_vector(vec_list_rec_mgrp, vec_rec_mgrp, range_rec_mgrp, vp);
    sname_list_ptr nlp =
        show_non_contig_vectors(sname_list_rec_mgrp, tstrings, vlp);
    while( nlp != NULL ) {
        SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, nlp->name)));
        SET_CONS_TL(tail, Make_NIL());
        tail = GET_CONS_TL(tail);
        nlp = nlp->next;
    }
    free_temp_str_mgr(tstrings);
    end_vector_ops();
    return res;
}

int
Get_Vector_Size(string vec)
{
    begin_vector_ops();
    vec_ptr vp = split_name(vec);
    int sz = vec_size(vp);
    end_vector_ops();
    return sz;
}

vec_list_ptr
Expand_vector(rec_mgr *vector_list_mgr,
              rec_mgr *vector_mgr,
              rec_mgr *range_mgr,
              vec_ptr vec)
{
    if(vec == NULL) {
        vec_list_ptr vlp = (vec_list_ptr) new_rec(vector_list_mgr);
        vlp->vec  = NULL;
        vlp->next = NULL;
        return vlp;
    }
    vec_list_ptr rem =
        Expand_vector(vector_list_mgr, vector_mgr, range_mgr, vec->next);
    if(vec->type == TXT) {
        for(vec_list_ptr vlp = rem; vlp != NULL; vlp = vlp->next) {
            vec_ptr v = (vec_ptr) new_rec(vector_mgr);
            v->type = TXT;
            v->u.name = vec->u.name;
            v->next = vlp->vec;
            vlp->vec = v;
        }
        return rem;
    } else {
        vec_list_ptr res = NULL, *res_tl = &res;
        for(range_ptr rp = vec->u.ranges; rp != NULL; rp = rp->next) {
            if(rp->upper >= rp->lower) {
                for(int i = rp->upper; i >= rp->lower; i--) {
                    range_ptr ri = (range_ptr) new_rec(range_mgr);
                    ri->upper = i;
                    ri->lower = i;
                    ri->next = NULL;
                    for(vec_list_ptr vlp = rem; vlp != NULL; vlp = vlp->next) {
                        vec_ptr n = (vec_ptr) new_rec(vector_mgr);
                        n->type = INDEX;
                        n->u.ranges = ri;
                        n->next = vlp->vec;
                        // /
                        vec_list_ptr nl = (vec_list_ptr) new_rec(vector_list_mgr);
                        nl->vec = n;
                        nl->next = NULL;
                        // /
                        *res_tl = nl;
                        res_tl = &(nl->next);
                    }
                }
            } else {
                for(int i = rp->upper; i <= rp->lower; i++) {
                    range_ptr ri = (range_ptr) new_rec(range_mgr);
                    ri->upper = i;
                    ri->lower = i;
                    ri->next = NULL;
                    for(vec_list_ptr vlp = rem; vlp != NULL; vlp = vlp->next) {
                        vec_ptr n = (vec_ptr) new_rec(vector_mgr);
                        n->type = INDEX;
                        n->u.ranges = ri;
                        n->next = vlp->vec;
                        // /
                        vec_list_ptr nl = (vec_list_ptr) new_rec(vector_list_mgr);
                        nl->vec = n;
                        nl->next = NULL;
                        // /
                        *res_tl = nl;
                        res_tl = &(nl->next);
                    }                    
                }
            }
        }
        return res;
    }
    return NULL;
}

g_ptr
Merge_Vectors(g_ptr nds, bool non_contig_vecs)
{
    g_ptr res = Get_node();
    merge_vectors_fl(nds, res, non_contig_vecs);
    return res;
}

vec_list_ptr
Merge_Vectors_gen(rec_mgr *vec_list_mgr, vec_list_ptr vecs)
{
    begin_vector_ops();
    // /
    buffer vec_buf;
    new_buf(&vec_buf, 100, sizeof(vec_ptr));
    buffer_vectors_list(vecs, &vec_buf, FALSE);
    merge_list_ptr mlp = gen_merge_vectors(&vec_buf);
    free_buf(&vec_buf);
    // /
    vec_list_ptr res = NULL, *tail = &res;
    for(; mlp != NULL; mlp = mlp->next) {
        vec_list_ptr vlp = (vec_list_ptr) new_rec(vec_list_mgr);
        vlp->vec  = mlp->vec;
        vlp->next = NULL;
        *tail = vlp;
        tail = &vlp->next;
    }
    // /
    end_vector_ops();
    return res;
}

bool
Check_range_overlap(range_ptr r1, range_ptr r2)
{
    if(r1 == r2) { return TRUE; }
    while(r1 != NULL && r2 != NULL) {
        if(!(r1->lower <= r2->upper && r2->lower <= r1->upper)) {
            return FALSE;
        }
        r1 = r1->next;
        r2 = r2->next;
    }
    return TRUE;
}

bool
Check_vector_overlap(vec_ptr v1, vec_ptr v2)
{
    if(v1 == v2) {
        return TRUE;
    }
    while(v1 != NULL && v2 != NULL) {
        if(v1->type != v2->type) {
            return FALSE;
        }
        if(v1->type == TXT && v1->u.name != v2->u.name) {
            return FALSE;
        }
        if(v1->type == INDEX && !Check_range_overlap(v1->u.ranges, v2->u.ranges)) {
            return FALSE;
        }
        v1 = v1->next;
        v2 = v2->next;
    }
    return TRUE;
}

sname_list_ptr
Show_vector(rec_mgr *sname_list_mgrp, vec_ptr vec, bool non_contig_vec)
{
    tstr_ptr tstrings = new_temp_str_mgr();
    sname_list_ptr names;
    if(non_contig_vec) {
        names = (sname_list_ptr) new_rec(sname_list_mgrp);
        names->name = show_non_contig_vector(tstrings, vec);
        names->next = NULL;
    } else {
        names = show_contig_vector(sname_list_mgrp, tstrings, vec);
    }
    free_temp_str_mgr(tstrings);
    return names;
}

sname_list_ptr
Show_vectors(rec_mgr *sname_list_mgrp, vec_list_ptr vecs, bool non_contig_vecs)
{
    /* if(vecs == NULL) { */
    /*     fprintf(stderr, "Show_vectors for NULL!\n"); */
    /*     return NULL; */
    /* } else { */
    /*     fprintf(stderr, "Show_vectors for "); */
    /*     for(vec_list_ptr vls = vecs; vls != NULL; vls = vls->next) { */
    /*         EMIT_VEC(vls->vec); */
    /*     } */
    /* } */
    // /
    tstr_ptr tstrings = new_temp_str_mgr();
    sname_list_ptr names;
    if(non_contig_vecs) {
        names = show_non_contig_vectors(sname_list_mgrp, tstrings, vecs);
    } else {
        names = show_contig_vectors(sname_list_mgrp, tstrings, vecs);
    }
    free_temp_str_mgr(tstrings);
    /* fprintf(stderr, " => "); */
    /* EMIT_STR_LIST(names); */
    return names;
}

// -----------------------------------------------------------------------------

unint
range_hash(pointer k, unint n)
{
    unint hash = 1;
    for(range_ptr r = (range_ptr) k; r != NULL; r = r->next) {
        hash += ((hash << 5) - hash) + int_hash(&r->upper, n);
        hash += ((hash << 5) - hash) + int_hash(&r->lower, n);
    }
    return (hash % n);
}

int
range_cmp(pointer k1, pointer k2)
{
    range_ptr r1 = (range_ptr) k1;
    range_ptr r2 = (range_ptr) k2;
    while (TRUE) {
        if(r1 == r2) {
            return 0;
        }
        if(r1 == NULL) {
            return -1;
        }
        if(r2 == NULL) {
            return 1;
        }
        if(r1->upper < r2->upper) {
            return -1;
        }
        if(r1->upper > r2->upper) {
            return 1;
        }
        r1 = r1->next;
        r2 = r2->next;
    }
}

bool
range_equ(pointer k1, pointer k2)
{
    return range_cmp(k1, k2) == 0;
}

// -----------------------------------------------------------------------------

// todo: ask Carl about hashing.
unint
vec_hash(pointer k, unint n)
{
    unint hash = 1;
    for(vec_ptr v = (vec_ptr) k; v != NULL; v = v->next) {
        if(v->type == TXT) {
            hash += ((hash << 5) - hash) + str_hash(v->u.name, n);
        } else {
            hash += ((hash << 5) - hash) + range_hash(v->u.ranges, n);
        }
    }
    return (hash % n);
}

int
vec_cmp(pointer k1, pointer k2)
{
    vec_ptr v1 = (vec_ptr) k1;
    vec_ptr v2 = (vec_ptr) k2;
    while(TRUE) {
        if(v1 == v2) {
            return 0;
        }
        if(v1 == NULL) {
            return -1;
        } 
        if(v2 == NULL) {
            return 1;
        }
        if(v1->type == TXT) {
            if(v2->type == TXT) {
                if(v1->u.name == v2->u.name) {
                    return 0;
                }
                return strcmp(v1->u.name, v2->u.name);
            } else {
                return 1;
            }
        }
        if(v2->type == TXT) {
            return -1;
        }
        int cmp = range_cmp(v1->u.ranges, v2->u.ranges);
        if(cmp != 0) {
            return cmp;
        }
        v1 = v1->next;
        v2 = v2->next;
    }
}

bool
vec_equ(pointer k1, pointer k2)
{
    return vec_cmp(k1, k2) == 0;
}

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
file_fullname(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string tmp = realpath(GET_STRING(r), path_buf);
    if( tmp == NULL ) {
        MAKE_REDEX_FAILURE(redex,
                           Fail_pr("Cannot find file %s\n", GET_STRING(r)));
        return;
    }
    MAKE_REDEX_STRING(redex, wastrsave(&strings, tmp));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_strlen(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    MAKE_REDEX_INT(redex, strlen(GET_STRING(r)));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
do_strcmp(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string s1  = GET_STRING(arg1);
    string s2  = GET_STRING(arg2);
    int res = strcmp(s1,s2);
    MAKE_REDEX_INT(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
str_is_prefix(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string pre = GET_STRING(arg1);
    string s   = GET_STRING(arg2);
    if( strncmp(pre, s, strlen(pre)) == 0 ) {
        MAKE_REDEX_BOOL(redex, B_One());
    } else {
        MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
str_is_suffix(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string suf = GET_STRING(arg1);
    string s   = GET_STRING(arg2);
    int lsuf = strlen(suf);
    int ls   = strlen(s);
    if( lsuf <= ls && strcmp(s+(ls-lsuf), suf) == 0 ) {
        MAKE_REDEX_BOOL(redex, B_One());
    } else {
        MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
str_is_substr(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string needle = GET_STRING(arg1);
    string haystack   = GET_STRING(arg2);
    if( strstr(haystack, needle) != NULL ) {
        MAKE_REDEX_BOOL(redex, B_One());
    } else {
        MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
str_match(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string pat = GET_STRING(arg1);
    string s   = GET_STRING(arg2);
    if( fnmatch(pat, s, 0) == 0 ) {
        MAKE_REDEX_BOOL(redex, B_One());
    } else {
        MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
str_split(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1       = GET_APPLY_RIGHT(l);
    g_ptr arg2       = GET_APPLY_RIGHT(redex);
    string s         = GET_STRING(arg1);
    string split_str = GET_STRING(arg2);
    int lsplit = strlen(split_str);
    string tmp = strtemp(s);
    string cur = tmp;
    string next;
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    if( lsplit == 0 ) {
        char buf[2];
        buf[1] = 0;
        while( *s != 0) {
            buf[0] = *s;
            SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, buf)));
            SET_CONS_TL(tail, Make_NIL());
            tail = GET_CONS_TL(tail);
            s++;
        }
    } else {
        while( (next = strstr(cur, split_str)) != NULL ) {
            *next = 0;
            SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, cur)));
            SET_CONS_TL(tail, Make_NIL());
            tail = GET_CONS_TL(tail);
            cur = next+lsplit;
        }
        if( *cur != 0 ) {
            SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, cur)));
            SET_CONS_TL(tail, Make_NIL());
            tail = GET_CONS_TL(tail);
        }
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_split_vector(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    begin_vector_ops();
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    vec_ptr vp = split_name(GET_STRING(r));
    while( vp != NULL ) {
	g_ptr cur;
	if( vp->type == TXT ) {
	    // Text
	    cur = Make_STRING_leaf(s_TXT);
	    cur = Make_CONS_ND(cur,
			      Make_STRING_leaf(wastrsave(&strings,vp->u.name)));
	} else {
	    // Range
	    cur = Make_STRING_leaf(s_RANGES);
	    g_ptr rlist = Make_NIL();
	    g_ptr rtail = rlist;
	    cur = Make_CONS_ND(cur, rlist);
	    range_ptr rp = vp->u.ranges;
	    while(rp != NULL ) {
		g_ptr pair = Make_PAIR_ND(Make_INT_leaf(rp->upper),
					  Make_INT_leaf(rp->lower));
		APPEND1(rtail, pair);
		rp = rp->next;
	    }
	}
	APPEND1(tail, cur);
	vp = vp->next;
    }
    end_vector_ops();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_expand_vectors(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    begin_vector_ops();
    tstr_ptr tstrings = new_temp_str_mgr();
    // /
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    for(g_ptr np = r; !IS_NIL(np); np = GET_CONS_TL(np)) {
        vec_ptr vp = split_name(GET_STRING(GET_CONS_HD(np)));
        vec_list_ptr vlp =
            Expand_vector(vec_list_rec_mgrp, vec_rec_mgrp, range_rec_mgrp, vp);
        sname_list_ptr nlp =
            show_non_contig_vectors(sname_list_rec_mgrp, tstrings, vlp);
        while( nlp != NULL ) {
            SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, nlp->name)));
            SET_CONS_TL(tail, Make_NIL());
            tail = GET_CONS_TL(tail);
            nlp = nlp->next;
        }
    }
    // /
    free_temp_str_mgr(tstrings);
    end_vector_ops();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_expand_vector(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    begin_vector_ops();
    tstr_ptr tstrings = new_temp_str_mgr();
    // /
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    vec_ptr vp = split_name(GET_STRING(r));
    vec_list_ptr vlp =
        Expand_vector(vec_list_rec_mgrp, vec_rec_mgrp, range_rec_mgrp, vp);
    sname_list_ptr nlp =
        show_non_contig_vectors(sname_list_rec_mgrp, tstrings, vlp);
    while( nlp != NULL ) {
        SET_CONS_HD(tail, Make_STRING_leaf(wastrsave(&strings, nlp->name)));
        SET_CONS_TL(tail, Make_NIL());
        tail = GET_CONS_TL(tail);
        nlp = nlp->next;
    }
    // /
    free_temp_str_mgr(tstrings);
    end_vector_ops();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_size(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    int sz = Get_Vector_Size(GET_STRING(r));
    MAKE_REDEX_INT(redex, sz);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_sizes(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    int sz = 0;
    g_ptr nl = r;
    while( !IS_NIL(nl) ) {
	sz += Get_Vector_Size(GET_STRING(GET_CONS_HD(nl)));
	nl = GET_CONS_TL(nl);
    }
    MAKE_REDEX_INT(redex, sz);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_extract_vectors(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    extract_vectors_fl(r, redex, TRUE, TRUE);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
extract_vectors(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    extract_vectors_fl(r, redex, FALSE, TRUE);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
md_merge_vectors(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    merge_vectors_fl(r, redex, TRUE);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
merge_vectors(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    merge_vectors_fl(r, redex, FALSE);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
string_lastn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string s = GET_STRING(GET_APPLY_RIGHT(l));
    int cnt = GET_INT(r);
    string tmp = strtemp(s);
    int len = strlen(s);
    if( len <= cnt ) {
        MAKE_REDEX_STRING(redex, wastrsave(&strings, tmp));
    } else {
        tmp = tmp+len-cnt;
        MAKE_REDEX_STRING(redex, wastrsave(&strings, tmp));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
string_butlastn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string s = GET_STRING(GET_APPLY_RIGHT(l));
    int cnt = GET_INT(r);
    string tmp = strtemp(s);
    int len = strlen(s);
    if( len <= cnt ) {
	MAKE_REDEX_STRING(redex, wastrsave(&strings, ""));
    } else {
	*(tmp+len-cnt) = 0;
	MAKE_REDEX_STRING(redex, wastrsave(&strings, tmp));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
string_strstr(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string haystack = GET_STRING(arg1);
    string needle   = GET_STRING(arg2);
    string loc = strstr(haystack, needle);
    if( loc == NULL ) {
	MAKE_REDEX_INT(redex, (0));
    } else {
	MAKE_REDEX_INT(redex, (1+loc-haystack));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
lift_node_name_cmp(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr g_s1, g_s2;
    EXTRACT_2_ARGS(redex, g_s1, g_s2);
    string s1 = GET_STRING(g_s1);
    string s2 = GET_STRING(g_s2);
    int res = node_name_cmp(s1, s2);
    MAKE_REDEX_INT(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
string_substr(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr g_s, g_f, g_c;
    EXTRACT_3_ARGS(redex, g_s, g_f, g_c);
    string s = GET_STRING(g_s); 
    int loc  = GET_INT(g_f); 
    int cnt  = GET_INT(g_c); 
    if( loc < 1 ) {
	string msg =
	    Fail_pr("Start position for str_substr before start of string (%d)",
		    loc);
	MAKE_REDEX_FAILURE(redex, msg);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    int len  = strlen(s);
    if( loc > len ) {
	MAKE_REDEX_STRING(redex, wastrsave(&strings, ""));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( cnt < 0 ) {
	string res = wastrsave(&strings, s+loc-1);
	MAKE_REDEX_STRING(redex, res);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( (loc + cnt - 1) > len ) {
	string msg = Fail_pr(
      "loc (%d) plus cnt (%d) greater than length of string (%d) in str_substr", 
			loc, cnt, len);
	MAKE_REDEX_FAILURE(redex, msg);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    string tmp = strtemp(s+loc-1);
    *(tmp+cnt) = '\0';
    string res = wastrsave(&strings, tmp);
    MAKE_REDEX_STRING(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
string_trim(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr g_l, g_r, g_s;
    EXTRACT_3_ARGS(redex, g_l, g_r, g_s);
    string s = GET_STRING(g_s); 
    string pref = GET_STRING(g_l);
    string suff = GET_STRING(g_r);
    string lloc = strstr(s, pref);
    if( lloc == NULL ) {
	string msg = Fail_pr("trim: Cannot find prefix (%s) in string (%s)",
			     pref, s);
	MAKE_REDEX_FAILURE(redex, msg);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    s = strtemp(lloc + strlen(pref));
    string cur = s;
    string found = strstr(cur, suff);
    string last = found;
    while( found != NULL ) {
	last = found;
	found = strstr(found+1, suff);
    }
    if( last == NULL ) {
	string msg = Fail_pr("trim: Cannot find suffix (%s) in string (%s)",
			     suff, s);
	MAKE_REDEX_FAILURE(redex, msg);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    *last = 0;
    string res = wastrsave(&strings, s);
    MAKE_REDEX_STRING(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
string_str_cluster(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(l);
    g_ptr arg2 = GET_APPLY_RIGHT(redex);
    string s = GET_STRING(arg1);
    int  sz  = GET_INT(arg2);
    int len  = strlen(s);
    if( (len % sz) != 0 ) {
	MAKE_REDEX_FAILURE(redex,
		Fail_pr("str_cluster: string length not a multiple of %d", sz));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    MAKE_REDEX_NIL(redex);
    string t = strtemp(s);
    g_ptr tail = redex;
    while( *t ) {
	char c = *(t+sz);
	*(t+sz) = 0;
	string item = wastrsave(&strings, t);
	SET_CONS_HD(tail, Make_STRING_leaf(item));
        SET_CONS_TL(tail, Make_NIL());
        tail = GET_CONS_TL(tail);
	t += sz;
	*t = c;
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
Strings_Install_Functions()
{
    // Get handle to vec_info type (defined in preamble.fl)
    typeExp_ptr vec_info_tp = Get_Type("vec_info",NULL,TP_INSERT_PLACE_HOLDER);

    // Add builtin functions
    Add_ExtAPI_Function("file_fullname", "1", FALSE,
			GLmake_arrow(GLmake_string(),GLmake_string()),
			file_fullname);

    Add_ExtAPI_Function("strlen", "1", FALSE,
			GLmake_arrow(GLmake_string(),GLmake_int()),
			do_strlen);

    Add_ExtAPI_Function("strcmp", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_int())),
			do_strcmp);

    Add_ExtAPI_Function("str_is_prefix", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			str_is_prefix);

    Add_ExtAPI_Function("str_is_suffix", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			str_is_suffix);

    Add_ExtAPI_Function("str_is_substr", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			str_is_substr);

    Add_ExtAPI_Function("str_split", "11", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(GLmake_string(),
					 GLmake_list(GLmake_string()))),
			str_split);

    Add_ExtAPI_Function("str_match", "11", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(GLmake_string(),
					 GLmake_bool())),
			str_match);

    Add_ExtAPI_Function("md_split_vector", "1", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_list(vec_info_tp)),
			md_split_vector);

    Add_ExtAPI_Function("md_expand_vector", "1", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_list(GLmake_string())),
			md_expand_vector);


    Add_ExtAPI_Function("md_expand_vectors", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_list(GLmake_string())),
			md_expand_vectors);

    Add_ExtAPI_Function("md_size", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_int()),
			md_size);

    Add_ExtAPI_Function("md_sizes", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_int()),
			md_sizes);

    Add_ExtAPI_Function("md_extract_vectors", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_list(GLmake_string())),
			md_extract_vectors);

    Add_ExtAPI_Function("extract_vectors", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_list(GLmake_string())),
			extract_vectors);

    Add_ExtAPI_Function("md_merge_vectors", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_list(GLmake_string())),
			md_merge_vectors);

    Add_ExtAPI_Function("merge_vectors", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_string()),
				     GLmake_list(GLmake_string())),
			merge_vectors);

    Add_ExtAPI_Function("string_lastn", "11", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(GLmake_int(),
					 GLmake_string())),
			string_lastn);

    Add_ExtAPI_Function("string_butlastn", "11", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(GLmake_int(),
					 GLmake_string())),
			string_butlastn);

    Add_ExtAPI_Function("strstr", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_int())),
			string_strstr);

    Add_ExtAPI_Function("substr", "111", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_int(),
						  GLmake_arrow(
							    GLmake_int(),
							    GLmake_string()))),
			string_substr);

    Add_ExtAPI_Function("trim", "111", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_arrow(
							    GLmake_string(),
							    GLmake_string()))),
			string_trim);

    Add_ExtAPI_Function("str_cluster", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(
					    GLmake_int(),
					    GLmake_list(GLmake_string()))),
			string_str_cluster);

    Add_ExtAPI_Function("node_name_cmp", "11", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(
					    GLmake_string(),
					    GLmake_int())),
			lift_node_name_cmp);
}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static void
begin_vector_ops()
{
    lstringsp = &lstrings;
    vec_rec_mgrp = &vec_rec_mgr;
    range_rec_mgrp = &range_rec_mgr;
    vec_list_rec_mgrp = &vec_list_rec_mgr;
    sname_list_rec_mgrp = &sname_list_rec_mgr;
    merge_list_rec_mgrp = &merge_list_rec_mgr;
    // /
    new_ustrmgr(lstringsp);
    new_mgr(vec_rec_mgrp, sizeof(vec_rec));
    new_mgr(range_rec_mgrp, sizeof(range_rec));
    new_mgr(vec_list_rec_mgrp, sizeof(vec_list_rec));
    new_mgr(sname_list_rec_mgrp, sizeof(sname_list_rec));
    new_mgr(merge_list_rec_mgrp, sizeof(merge_list_rec));
}

static void
end_vector_ops()
{
    free_ustrmgr(lstringsp);
    free_mgr(vec_rec_mgrp);
    free_mgr(range_rec_mgrp);
    free_mgr(vec_list_rec_mgrp);
    free_mgr(sname_list_rec_mgrp);
    free_mgr(merge_list_rec_mgrp);
    // /
    lstringsp = NULL;
    vec_rec_mgrp = NULL;
    range_rec_mgrp = NULL;
    vec_list_rec_mgrp = NULL;
    sname_list_rec_mgrp = NULL;
    merge_list_rec_mgrp = NULL;
}

static range_ptr
merge_indices(range_ptr indices)
{
    range_ptr res = indices;
    range_ptr tail = indices;
    range_ptr cur = indices->next;
    while(cur) {
	if( (tail->upper >= tail->lower) && (cur->upper >= cur->lower) &&
	    ((tail->lower - 1) == cur->upper) )
	{
	    tail->lower = cur->lower;
	    cur = cur->next;
	    tail->next = cur;
	} else
	if( (tail->upper <= tail->lower) && (cur->upper <= cur->lower) &&
	    ((tail->lower + 1) == cur->upper) )
	{
	    tail->lower = cur->lower;
	    cur = cur->next;
	    tail->next = cur;
	} else {
	    tail = cur;
	    cur = cur->next;
	}
    }
    return res;
}

static vec_ptr
split_name(string name)
{
    vec_ptr res = NULL;
    vec_ptr *res_tl_ptr = &res;
    string p = name;
    while( *p ) {
        if( isdigit(*p) ) {
            // Index
            string e = p;
            while( *e && *e != ']' ) e++;
            if( *e == 0 ) { goto illegal_format; }
            char tmp = *e;
            *e = 0;
            range_ptr indices = NULL;
            range_ptr *indices_tl_ptr = &indices;
            while( *p ) {
                int upper, lower;
                if( sscanf(p, "%d:%d", &upper, &lower) == 2 ) {
                    range_ptr range = (range_ptr) new_rec(range_rec_mgrp);
                    range->upper = upper;
                    range->lower = lower;
                    range->next = NULL;
                    *indices_tl_ptr = range;
                    indices_tl_ptr = &(range->next);
                } else if( sscanf(p, "%d", &upper) == 1 ) {
                    range_ptr range = (range_ptr) new_rec(range_rec_mgrp);
                    range->upper = upper;
                    range->lower = upper;
                    range->next = NULL;
                    *indices_tl_ptr = range;
                    indices_tl_ptr = &(range->next);
                } else {
                    goto illegal_format;
                }
                while( *p && *p != ',' ) p++;
                if( *p ) p++;
            }
            vec_ptr n = (vec_ptr) new_rec(vec_rec_mgrp);
            n->type = INDEX;
            n->u.ranges = merge_indices(indices);
            n->next = NULL;
            *res_tl_ptr = n;
            res_tl_ptr = &(n->next);
            *e = tmp;
            p = e;
        } else {
            // String
            if( *p == '\\' ) {
                // An escaped identifier
                string e = p+1;
                while( *e && *e != ' ' ) e++;
                if( *e == 0 ) { goto illegal_format; }
                e++;
                char tmp = *e;
                *e = 0;
                vec_ptr n = (vec_ptr) new_rec(vec_rec_mgrp);
                n->type = TXT;
                n->u.name = uStrsave(lstringsp, p);
                n->next = NULL;
                *res_tl_ptr = n;
                res_tl_ptr = &(n->next);
                *e = tmp;
                p = e;
            } else {
                string e = p;
                while( *e && *e != '[' ) e++;
                if( *e == '[' ) e++;
                char tmp = *e;
                *e = 0;
                vec_ptr n = (vec_ptr) new_rec(vec_rec_mgrp);
                n->type = TXT;
                n->u.name = uStrsave(lstringsp, p);
                n->next = NULL;
                *res_tl_ptr = n;
                res_tl_ptr = &(n->next);
                *e = tmp;
                p = e;
            }
        }
    }
    return res;

  illegal_format:
    res  = (vec_ptr) new_rec(vec_rec_mgrp);
    res->type = TXT;
    res->u.name = uStrsave(lstringsp, name);
    res->next = NULL;
    return res;
}

static range_ptr
make_range_canonical(range_ptr rp)
{
    if( rp == NULL ) { return NULL; }
    if( rp->upper < rp->lower ) { 
        int tmp = rp->upper;
        rp->upper = rp->lower;
        rp->lower = tmp;
    }
    range_ptr rem = make_range_canonical(rp->next);
    if( rem == NULL ) {
        return rp;
    }
    if( rp->lower > rem->upper ) {
        rp->next = rem;
        return rp;
    }
    // Overlap
    if( rp->upper <= rem->upper ) {
        if( rp->lower >= rem->lower ) {
            return rem;
        }
        rem->lower = rp->lower;
        return( make_range_canonical(rem) );
    }
    rem->upper = rp->upper;
    if( rem->lower > rp->lower ) {
        rem->lower = rp->lower;
        return( make_range_canonical(rem) );
    }
    return rem;
}

static vec_ptr
make_vector_ranges_canonical(vec_ptr vp)
{
    for(; vp != NULL; vp = vp->next) {
        if(vp->type == INDEX) {
            vp->u.ranges = make_range_canonical(vp->u.ranges);
        }
    }
    return vp;
}

static range_ptr
compress_ranges(range_ptr r1, range_ptr r2)
{
    if( r1 == NULL ) return r2; 
    if( r2 == NULL ) return r1; 
    // Make sure r1 is the larger range
    if( r1->upper < r2->upper ) {
        range_ptr tmp = r2;
        r2 = r1;
        r1 = tmp;
    }
    if( r1->lower > (r2->upper+1) ) {
        // Non-overlapping
        r1->next = compress_ranges(r1->next, r2);
        return r1;
    }
    if( r1->lower <= r2->lower ) {
        // r1 covers r2. Just ignore r2
        return( compress_ranges(r1, r2->next) );
    }
    r1->lower = r2->lower;
    // Can we merge newly formed range with next range?
    if( r1->next != NULL ) {
        if( r1->lower == (r1->next->upper+1) ) {
            r1->lower = r1->next->lower;
            r1->next = r1->next->next;
        }
    }
    return( compress_ranges(r1, r2->next) );
}

static int
vec_size(vec_ptr vec)
{
    if( vec == NULL ) {
        return 1;
    }
    int rem = vec_size(vec->next);
    if( vec->type == TXT ) {
        return rem;
    } else {
        // Indices
        int sum = 0;
        for(range_ptr rp = vec->u.ranges; rp != NULL; rp = rp->next) {
            sum += abs(rp->upper-rp->lower+1)*rem;
        }
        return sum;
    }
}

static string
mk_name_signature(vec_ptr vp)
{
    string name = strtemp("");
    while(vp != NULL ) {
        if( vp->type == TXT ) {
            name = strappend(vp->u.name);
        } else {
            name = charappend('*');
        }
        vp = vp->next;
    }
    return( uStrsave(lstringsp, name) );
}

// -----------------------------------------------------------------------------

static void
buffer_vectors_list(vec_list_ptr vs, buffer_ptr vec_buf, bool range_canonical)
{
    for(; vs != NULL; vs = vs->next) {
        vec_ptr vp = vs->vec;
        if(range_canonical) {
            // todo: why is 'vp = make_...;' wrong?
            make_vector_ranges_canonical(vp);
        }
        push_buf(vec_buf, &vp);
    }
}

static void
buffer_vectors_fl(g_ptr r, buffer_ptr vec_buf, bool range_canonical)
{
    for(; !IS_NIL(r); r = GET_CONS_TL(r)) {
        vec_ptr vp = split_name(GET_STRING(GET_CONS_HD(r)));
        if(range_canonical) {
            // todo: why is 'vp = make_...;' wrong?
            make_vector_ranges_canonical(vp);
        }
        push_buf(vec_buf, &vp);
    }
}

// -----------------------------------------------------------------------------

// todo: Why the frick doesn't same_range === range_cmp ?!
static bool
same_range(range_ptr r1, range_ptr r2)
{
    while (1) {
        if( r1 == r2 ) { return TRUE; }
        if( r1 == NULL ) { return FALSE; }
        if( r2 == NULL ) { return FALSE; }
        if( r1->upper != r2->upper ) { return FALSE; }
        if( r1->lower != r2->lower ) { return FALSE; }
        r1 = r1->next;
        r2 = r2->next;
    }
}

// todo: This one is different from vec_cmp, but why?
static int
vec_name_cmp(vec_ptr v1, vec_ptr v2)
{
    if(v1 == v2) {
        return 0;
    }
    if(v1 == NULL) {
        return -1;
    }
    if(v2 == NULL) {
        return 1;
    }
    if(v1->type == TXT) {
        if(v2->type == TXT) {
            if(v1->u.name == v2->u.name) {
                return vec_name_cmp(v1->next, v2->next);
            }
            return strcmp(v1->u.name, v2->u.name);
        } else {
            // v2->type == INDEX
            return 1;
        }
    }
    if(v2->type == TXT) {
        // v1->type == INDEX
        // v2->type == TXT
        return -1;
    }
    // v1->type == INDEX
    // v2->type == INDEX
    if(v1->u.ranges->upper == v2->u.ranges->upper) {
        return vec_name_cmp(v1->next, v2->next);
    }
    if(v1->u.ranges->upper > v2->u.ranges->upper) {
        return 1;
    } else {
        return -1;
    }
}

static int
nn_cmp(const void *pi, const void *pj)
{
    vec_ptr vi = *((vec_ptr *) pi);
    vec_ptr vj = *((vec_ptr *) pj);
    //return vec_cmp(vi, vj);
    return( vec_name_cmp(vi, vj) );
}

static merge_list_ptr
gen_extract_vectors(buffer_ptr vec_buf)
{
    // At least one vector in the buffer
    if(vec_buf == NULL || COUNT_BUF(vec_buf) == 0) {
        return NULL;
    }
    // Sort according to merge order
    qsort(START_BUF(vec_buf), COUNT_BUF(vec_buf), sizeof(vec_ptr), nn_cmp);
    vec_ptr *vpp;
    merge_list_ptr  mp = NULL;
    int alts = 0;
    FOR_BUF(vec_buf, vec_ptr, vpp) {
        merge_list_ptr m = new_rec (merge_list_rec_mgrp);
        m->vec = *vpp;
        m->name_signature = mk_name_signature(*vpp);
        if( mp == NULL ) {
            m->next = m;
            m->prev = m;
            mp = m;
        } else {
            m->next = mp;
            m->prev = mp->prev;
            mp->prev->next = m;
            mp->prev = m;
        }
        alts++;
    }
    // Now try to merge adjacent vectors into bigger vectors
    int not_changed = 0;
    do {
        if( alts != 1 ) {
            if( mp->name_signature != mp->next->name_signature ) {
                // Definitely cannot be merged
                not_changed++;
                mp = mp->next;
            } else {
                // Potentially can be merged
                vec_ptr v1 = mp->vec;
                vec_ptr v2 = mp->next->vec;
                while( v1 &&
                       (v1->type == TXT ||
                        //range_equ(v1->u.ranges, v2->u.ranges)) )
                        same_range(v1->u.ranges,v2->u.ranges)) )
                {
                    v1 = v1->next;
                    v2 = v2->next;
                }
                if( v1 == NULL ) {
                    // Two identical vectors
                    mp->next->next->prev = mp;
                    mp->next = mp->next->next;
                    not_changed = 0;
                    alts--;
                } else {
                    // Make sure the rest matches
                    range_ptr r1 = v1->u.ranges;
                    range_ptr r2 = v2->u.ranges;
                    vec_ptr mvp = v1;
                    v1 = v1->next;
                    v2 = v2->next;
                    while( v1 && 
                           (v1->type == TXT ||
                            //range_equ(v1->u.ranges,v2->u.ranges)) )
                            same_range(v1->u.ranges,v2->u.ranges)) )
                    {
                        v1 = v1->next;
                        v2 = v2->next;
                    }
                    if( v1 != NULL ) {
                        not_changed++;
                        mp = mp->next;
                    } else {
                        mvp->u.ranges = compress_ranges(r1, r2);
                        mp->next->next->prev = mp;
                        mp->next = mp->next->next;
                        not_changed = 0;
                        alts--;
                    }
                }
            }
        }
    } while( alts > 1 && (not_changed <= alts) );
    return mp;
}

static merge_list_ptr
gen_merge_vectors(buffer_ptr vec_buf)
{
    // At least one vector in the list
    if(vec_buf == NULL || COUNT_BUF(vec_buf) == 0) {
        return NULL;
    }
    vec_ptr *vpp;
    merge_list_ptr mp = NULL;
    int alts = 0;
    FOR_BUF(vec_buf, vec_ptr, vpp) {
        merge_list_ptr m = new_rec (merge_list_rec_mgrp);
        m->vec = *vpp;
        m->name_signature = mk_name_signature(*vpp);
        if( mp == NULL ) {
            m->next = m;
            m->prev = m;
            mp = m;
        } else {
            m->next = mp;
            m->prev = mp->prev;
            mp->prev->next = m;
            mp->prev = m;
        }
        alts++;
    }
    // Now try to merge adjacent vectors into bigger vectors
    merge_list_ptr mp0 = mp;
    int not_changed = 0;
    while( alts > 1 && not_changed < alts ) {
        if( mp->next == mp0 ) {
            // Never merge the last with the first
            mp = mp0;
        }
        if( mp->name_signature != mp->next->name_signature ) {
            // Definitely cannot be merged
            not_changed++;
            mp = mp->next;
        } else {
            // Potentially can be merged
            vec_ptr v1 = mp->vec;
            vec_ptr v2 = mp->next->vec;
            while( v1 &&
                   (v1->type == TXT ||
                    //range_equ(v1->u.ranges, v2->u.ranges)) )
                    same_range(v1->u.ranges,v2->u.ranges)) )
            {
                v1 = v1->next;
                v2 = v2->next;
            }
            if( v1 == NULL ) {
                // Two identical vectors; don't remove duplicates
                not_changed++;
                mp = mp->next;
            } else {
                // Make sure the rest matches
                range_ptr r1 = v1->u.ranges;
		while( r1->next != NULL ) { r1 = r1->next; }
                range_ptr r2 = v2->u.ranges;
                v1 = v1->next;
                v2 = v2->next;
                while( v1 && 
                       (v1->type == TXT ||
                        same_range(v1->u.ranges,v2->u.ranges)) )
                {
                    v1 = v1->next;
                    v2 = v2->next;
                }
                if( v1 != NULL ) {
		    // Different suffix
                    not_changed++;
                    mp = mp->next;
                } else {
		    // Same prefix and suffix
                    if( (r1->upper >= r1->lower) && (r2->upper >= r2->lower) &&
                        ((r1->lower - 1) == r2->upper) )
                    {
			// Continuous downwards range
                        r1->lower = r2->lower;
			r1->next = r2->next;
                        mp->next->next->prev = mp;
                        mp->next = mp->next->next;
                        not_changed = 0;
                        alts--;
                    } else
		    if( (r1->upper <= r1->lower) && (r2->upper <= r2->lower) &&
                        ((r1->lower + 1) == r2->upper) )
                    {
			// Continuous upwards range
                        r1->lower = r2->lower;
			r1->next = r2->next;
                        mp->next->next->prev = mp;
                        mp->next = mp->next->next;
                        not_changed = 0;
                        alts--;
                    } else {
			r1->next = r2;
                        mp->next->next->prev = mp;
                        mp->next = mp->next->next;
                        not_changed = 0;
                    }
                }
            }
        }
    };
    return mp0;
}

static void
record_names_fl(sname_list_ptr nlp, g_ptr redex)
{
    MAKE_REDEX_NIL(redex);
    for(; nlp != NULL; nlp = nlp->next) {
        APPEND1(redex, Make_STRING_leaf(nlp->name));
    }
}

static void
merge_vectors_fl(g_ptr list, g_ptr res, bool non_contig_vecs)
{
    begin_vector_ops();
    //
    buffer vec_buf;
    new_buf(&vec_buf, 100, sizeof(vec_ptr));
    buffer_vectors_fl(list, &vec_buf, FALSE);
    merge_list_ptr mlp = gen_merge_vectors(&vec_buf);
    free_buf(&vec_buf);
    // /
    tstr_ptr tstrings = new_temp_str_mgr();
    sname_list_ptr nlp =
        show_merge_list(sname_list_rec_mgrp, tstrings, mlp, non_contig_vecs);
    record_names_fl(nlp, res);
    free_temp_str_mgr(tstrings);
    // /
    end_vector_ops();
}

static void
extract_vectors_fl(g_ptr list, g_ptr res, bool non_contig_vecs, bool range_canonical)
{
    begin_vector_ops();
    // /
    buffer vec_buf;
    new_buf(&vec_buf, 100, sizeof(vec_ptr));
    buffer_vectors_fl(list, &vec_buf, range_canonical);
    merge_list_ptr mlp = gen_extract_vectors(&vec_buf);
    free_buf(&vec_buf);
    // /
    tstr_ptr tstrings = new_temp_str_mgr();
    sname_list_ptr nlp =
        show_merge_list(sname_list_rec_mgrp, tstrings, mlp, non_contig_vecs);
    record_names_fl(nlp, res);
    free_temp_str_mgr(tstrings);
    // /
    end_vector_ops();
}

// -----------------------------------------------------------------------------

#define SPRINT_RANGE(tstrings, rp)                                             \
    { char buf[20];                                                            \
      if(rp->upper == rp->lower) { Sprintf(buf, "%d", rp->upper); }            \
      else { Sprintf(buf, "%d:%d", rp->upper, rp->lower); }                    \
      gen_strappend(tstrings, buf);                                            \
    }

static string
show_non_contig_vector(tstr_ptr tstrings, vec_ptr vp)
{
    string str_buf = gen_strtemp(tstrings, "");
    for(; vp != NULL; vp = vp->next) {
        if(vp->type == TXT) {
            gen_strappend(tstrings, vp->u.name);
        } else {
            range_ptr rp = vp->u.ranges;
            if(rp != NULL) {
                SPRINT_RANGE(tstrings, rp);
                for(rp = rp->next; rp != NULL; rp = rp->next) {
                    gen_charappend(tstrings, ',');
                    SPRINT_RANGE(tstrings, rp);
                }
            }
        }
    }
    return wastrsave(&strings, str_buf);
}

static sname_list_ptr
show_non_contig_vectors(
    rec_mgr *sname_list_mgrp, tstr_ptr tstrings, vec_list_ptr vlp)
{
    sname_list_ptr res, *tail_ptr = &res;
    for(; vlp != NULL; vlp = vlp->next) {
        sname_list_ptr slp = (sname_list_ptr) new_rec(sname_list_mgrp);
        slp->name = show_non_contig_vector(tstrings, vlp->vec);
        slp->next = NULL;
        // /
        *tail_ptr = slp;
        tail_ptr = &(slp->next);
    }
    return res;
}

// todo: assumes '_strtemp' has been called, I think.
static sname_list_ptr
show_contig_vector(rec_mgr *sname_list_mgrp, tstr_ptr tstrings, vec_ptr vp)
{
    string str_buf = gen_strappend(tstrings, "");
    for(; vp != NULL; vp = vp->next) {
        if(vp->type == TXT) {
            gen_strappend(tstrings, vp->u.name);
        } else {
            sname_list_ptr res = NULL, *tail_ptr = &res;
	    string end_s = str_buf+strlen(str_buf);
            for(range_ptr rp = vp->u.ranges; rp != NULL; rp = rp->next) {
		*end_s = 0;
		gen_strtemp(tstrings, str_buf);
                SPRINT_RANGE(tstrings, rp);
                sname_list_ptr slp =
                    show_contig_vector(sname_list_mgrp, tstrings, vp->next);
                // /
                *tail_ptr = slp;
                while(slp->next != NULL) { slp = slp->next; }
                tail_ptr = &slp->next;
            }
            return res;
        }
    }
    sname_list_ptr slp = (sname_list_ptr) new_rec(sname_list_mgrp);
    slp->name = wastrsave(&strings, str_buf);
    slp->next = NULL;
    return slp;
}

static sname_list_ptr
show_contig_vectors(rec_mgr *sname_list_mgrp, tstr_ptr tstrings, vec_list_ptr vlp)
{
    sname_list_ptr res = NULL, *tail_ptr = &res;
    for(; vlp != NULL; vlp = vlp->next) {
        gen_strtemp(tstrings, "");
        sname_list_ptr slp =
            show_contig_vector(sname_list_mgrp, tstrings, vlp->vec);
        // /
        *tail_ptr = slp;
        while(slp->next != NULL) { slp = slp->next; }
        tail_ptr = &slp->next;
    }
    return res;
}

static sname_list_ptr
show_merge_list(rec_mgr *sname_list_mgrp, tstr_ptr tstrings, merge_list_ptr mlp, bool non_contig_vecs)
{
    if(mlp == NULL) {
        return NULL;
    }
    // Record 'mlp' vectors in 'vlp'.
    merge_list_ptr start = mlp;
    vec_list_ptr vlp, *tail_ptr = &vlp;
    do {
        vec_list_ptr vlp = (vec_list_ptr) new_rec(vec_list_rec_mgrp);
        vlp->vec = mlp->vec;
        vlp->next = NULL;
        mlp = mlp->next;
        // /
        *tail_ptr = vlp;
        tail_ptr = &(vlp->next);
    } while(mlp != NULL && mlp != start);
    // Show vectors in 'vlp'.
    sname_list_ptr res;
    if(non_contig_vecs) {
        res = show_non_contig_vectors(sname_list_mgrp, tstrings, vlp);
    } else {
        res = show_contig_vectors(sname_list_mgrp, tstrings, vlp);
    }
    return res;
}
