//-----------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/*		Original author: Carl-Johan Seger, 2017                       */
/*                                                                            */
/******************************************************************************/
#include "voss_strings.h"
#include "graph.h"
#include <limits.h>
#include <stdlib.h>
#include <fnmatch.h>
#include <ctype.h>
#include <time.h>
#include <regex.h>

#define NBR_REGEXP_CACHED   10
#define REG_ERR_BUF_SZ	    300

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     strings;
extern jmp_buf     *start_envp;

/***** PRIVATE VARIABLES *****/
//
static string	    reg_cache_expr[NBR_REGEXP_CACHED];
static regex_t	    reg_cache_cexpr[NBR_REGEXP_CACHED];
static uint64_t	    reg_cache_timestamp[NBR_REGEXP_CACHED];
static char	    reg_error_buf[REG_ERR_BUF_SZ];
//
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

static rec_mgr	    vector_db_rec_mgr;

/* ----- Forward definitions local functions ----- */
static void		begin_vector_ops();
static void		end_vector_ops();
static vec_ptr		split_name(string name);
static range_ptr	make_range_canonical(range_ptr rp);
static vec_ptr		make_vector_ranges_canonical(vec_ptr vp);
static range_ptr	compress_ranges(range_ptr r1, range_ptr r2);
static void		buffer_vectors_list(
                          vec_list_ptr vs, buffer_ptr vec_buf,
                          bool range_canonical);
static void		buffer_vectors_fl(
                          g_ptr r, buffer_ptr vec_buf, bool range_canonical);
static bool	          same_range(range_ptr r1, range_ptr r2);
static int		vec_name_cmp(vec_ptr v1, vec_ptr v2);
static int		nn_cmp(const void *pi, const void *pj);
static merge_list_ptr	gen_extract_vectors(buffer_ptr vec_buf);
static merge_list_ptr	gen_merge_vectors(buffer_ptr vec_buf);
static void		record_names_fl(sname_list_ptr nlp, g_ptr redex);
static void		merge_vectors_fl(
                          g_ptr list, g_ptr res, bool non_contig_vecs);
static void		extract_vectors_fl(
                          g_ptr list, g_ptr res, bool non_contig_vecs,
                          bool range_canonical);
static int	          vec_size(vec_ptr vec);
static string		show_non_contig_vector(tstr_ptr tstrings, vec_ptr vp);
static sname_list_ptr	show_non_contig_vectors(
                          rec_mgr *sname_list_mgrp, tstr_ptr tstrings,
                          vec_list_ptr vlp);
static sname_list_ptr	show_contig_vector(
                          rec_mgr *sname_list_mgrp,
                          tstr_ptr tstrings, vec_ptr vp);
static sname_list_ptr	show_contig_vectors(
                          rec_mgr *sname_list_mgrp, tstr_ptr tstrings,
                          vec_list_ptr vp);
static sname_list_ptr	show_merge_list(
                          rec_mgr *sname_list_mgrp, tstr_ptr tstrings,
                          merge_list_ptr mlp, bool non_contig_vecs);

static string		mk_name_signature(vec_ptr vp);
static uint64_t		get_timestamp();
static regex_t		*compile_regexp(string pattern, int flags);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/


void
Strings_Init()
{
    // Initialization code
    s_TXT = Mk_constructor_name("TXT");
    s_RANGES = Mk_constructor_name("RANGES");
    new_mgr(&vector_db_rec_mgr, sizeof(vector_db_rec));
    for(int i = 0; i < NBR_REGEXP_CACHED; i++) {
	reg_cache_expr[i] = NULL;
    }
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

vec_list_ptr
Expand_constant(rec_mgr *vector_list_mgr, rec_mgr *vector_mgr, string s)
{
    if( strncmp(s, "0b", 2) != 0 ) { return NULL; }
    s += 2;
    vec_list_ptr    res = NULL;
    vec_list_ptr    tail;
    while( *s && ((*s == '0') || (*s == '1') || (*s == 'x')) ) {
	vec_ptr v = (vec_ptr) new_rec(vector_mgr);
	v->type = TXT;
	char vbuf[4];
	vbuf[0] = '0';
	vbuf[1] = 'b';
	vbuf[2] = *s;
	vbuf[3] = 0;
	v->u.name = uStrsave(lstringsp, vbuf);
	v->next = NULL;
        vec_list_ptr vlp = (vec_list_ptr) new_rec(vector_list_mgr);
        vlp->vec  = v;
        vlp->next = NULL;
	if( res == NULL ) {
	    res = vlp;
	} else {
	    tail->next = vlp;
	}
	tail = vlp;
	s++;
    }
    if( *s ) { return NULL; }
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
    vec_list_ptr vlp = Expand_constant(vec_list_rec_mgrp, vec_rec_mgrp, name);
    if( vlp == NULL ) {
	vec_ptr vp = split_name(name);
	vlp = Expand_vector(vec_list_rec_mgrp,vec_rec_mgrp,range_rec_mgrp, vp);
    }
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
    if( strncmp(vec, "0b", 2) == 0 ) {
	int len = 0;
	string s = vec+2;
	while( *s && ((*s == '0') || (*s == '1') || (*s == 'x')) ) {
	    len++;
	    s++;
	}
	if( *s == 0 ) { return len; }
	FP(warning_fp, "Get_Vector_Size on invalid constant (%s)", vec);
    }
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

g_ptr
Extract_Vectors(g_ptr nds, bool non_contig_vecs)
{
    g_ptr res = Get_node();
    extract_vectors_fl(nds, res, non_contig_vecs, TRUE);
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
    merge_list_ptr start = mlp;
    do {
        vec_list_ptr vlp = (vec_list_ptr) new_rec(vec_list_mgr);
        vlp->vec  = mlp->vec;
        vlp->next = NULL;
        *tail = vlp;
        tail = &vlp->next;
	mlp = mlp->next;
    } while( mlp != start );
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
        if( v1->type == INDEX
	   &&
	    !Check_range_overlap(v1->u.ranges, v2->u.ranges))
	{
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
    /*         DBG_PRINT_VEC(vls->vec); */
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
    /* DBG_PRINT_STR_LIST(names); */
    return names;
}

// -----------------------------------------------------------------------------

unint
range_hash(pointer k, unint n)
{
    unint hash = 1;
    for(range_ptr r = (range_ptr) k; r != NULL; r = r->next) {
        hash += ((hash << 5) - hash) + (r->upper % n);
        hash += ((hash << 5) - hash) + (r->lower % n);
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
        if(r1->lower < r2->lower) {
            return -1;
        }
        if(r1->lower > r2->lower) {
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

bool
vec_equ(pointer k1, pointer k2)
{
    vec_ptr v1 = (vec_ptr) k1;
    vec_ptr v2 = (vec_ptr) k2;
    while(1) {
	if( v1 == v2 ) return( TRUE );
	if( v1 == NULL ) return( FALSE );
	if( v2 == NULL ) return( FALSE );
	if( v1->type != v2->type ) return( FALSE );
	if( v1->type == TXT ) {
	    if( !STREQ(v1->u.name, v2->u.name) ) return( FALSE );
	} else {
	    range_ptr r1 = v1->u.ranges;
	    range_ptr r2 = v2->u.ranges;
	    while( r1 != NULL ) {
		if( r2 == NULL ) return(FALSE);
		if( r1->upper != r2->upper ) return(FALSE);
		if( r1->lower != r2->lower ) return(FALSE);
		r1 = r1->next;
		r2 = r2->next;
	    }
	    if( r2 != NULL ) return( FALSE );
	}
	v1 = v1->next;
	v2 = v2->next;
    }
}

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
str_reg_match(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_pat, g_s;
    EXTRACT_2_ARGS(redex, g_pat, g_s);
    string pat = GET_STRING(g_pat);
    regex_t *pat_ptr = compile_regexp(pat, REG_EXTENDED);
    if( pat_ptr == NULL ) {
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
        MAKE_REDEX_FAILURE(redex, wastrsave(&strings, reg_error_buf));
        return;
    }
    if( regexec(pat_ptr, GET_STRING(g_s), (size_t) 0, NULL, 0) == 0 ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
str_reg_extract(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_pat, g_s;
    EXTRACT_2_ARGS(redex, g_pat, g_s);
    string pat = GET_STRING(g_pat);
    string str = GET_STRING(g_s);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;

    regex_t *pat_ptr = compile_regexp(pat, REG_EXTENDED);
    if( pat_ptr == NULL ) {
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
        MAKE_REDEX_FAILURE(redex, wastrsave(&strings, reg_error_buf));
        return;
    }
    int patterns = pat_ptr->re_nsub;
    regmatch_t pmatch[patterns+2];
    string tmp = strtemp(str);
    if( regexec(pat_ptr, str, patterns+1, pmatch, 0) == 0 ) {
	for(int i = 0; i <= patterns; i++) {
	    if( pmatch[i].rm_so != -1 ) {
		string sp = tmp + pmatch[i].rm_so;
		string ep = tmp + pmatch[i].rm_eo;
		char c = *ep;
		*ep = 0;
		APPEND1(tail, Make_STRING_leaf(wastrsave(&strings, sp)));
		*ep = c;
	    }
	}
    } else {
	MAKE_REDEX_FAILURE(redex,
	    Fail_pr("str_reg_extract: No match for pattern:%s & string:%s\n", 
		      pat, str) );
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
tcl2list(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string src = GET_STRING(r);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string start = strtemp(src);
    string c = start;
    int level = 0;
    while( *c ) {
	switch( *c ) {
	    case ' ':
		if( level == 0 ) {
		    *c = 0;
		    string element = wastrsave(&strings, start);
		    APPEND1(tail, Make_STRING_leaf(element));
		    c++;
		    start = c;
		} else {
		    c++;
		}
		break;
	    case '\\':
		c += 2;
		break;
	    case '{':
		if( level == 0 ) {
		    level++;
		    c++;
		    start = c;
		} else {
		    level++;
		    c++;
		}
		break;
	    case '}':
		if( level > 1 ) {
		    level--;
		    c++;
		} else {
		    level--;
		    *c = 0;
		    string element = wastrsave(&strings, start);
		    APPEND1(tail, Make_STRING_leaf(element));
		    c++;
		    if( *c == ' ' ) {
			c++;
		    }
		    start = c;
		}
		break;
	    default:
		c++;
		break;
	}
    }
    if( start != c ) {
	string element = wastrsave(&strings, start);
	APPEND1(tail, Make_STRING_leaf(element));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

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
	string name = GET_STRING(GET_CONS_HD(np));
	vec_list_ptr vlp =
	    Expand_constant(vec_list_rec_mgrp, vec_rec_mgrp, name);
	if( vlp == NULL ) {
	    vec_ptr vp = split_name(name);
	    vlp = Expand_vector(vec_list_rec_mgrp, vec_rec_mgrp,
				range_rec_mgrp,vp);
	}
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
    string name = GET_STRING(r);
    vec_list_ptr vlp = Expand_constant(vec_list_rec_mgrp, vec_rec_mgrp, name);
    if( vlp == NULL ) {
	vec_ptr vp = split_name(name);
	vlp = Expand_vector(vec_list_rec_mgrp,vec_rec_mgrp,range_rec_mgrp,vp);
    }
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
    g_ptr g_s;
    EXTRACT_1_ARG(redex, g_s);
    string s = GET_STRING(g_s); 
    string tmp = strtemp(s);
    string f = tmp;
    while( *f && (isspace(*f) || (*f == 10)) ) { f++; }
    if( *f == 0 ) {
	MAKE_REDEX_STRING(redex, wastrsave(&strings, ""));
    } else {
	string t = tmp + strlen(tmp)-1;
	while( t > f && (isspace(*t) || (*t == 10))) { t--; }
	t++;
	*t = 0;
	MAKE_REDEX_STRING(redex, wastrsave(&strings, f));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

static void
string_gen_trim(g_ptr redex)
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

static void
get_vector_signature(g_ptr redex)
{
    g_ptr l    = GET_APPLY_LEFT(redex);
    g_ptr r    = GET_APPLY_RIGHT(redex);
    g_ptr g_s;
    EXTRACT_1_ARG(redex, g_s);
    string s = GET_STRING(g_s);
    begin_vector_ops();
    vec_ptr vp = split_name(s);
    string res = mk_name_signature(vp);
    MAKE_REDEX_STRING(redex, wastrsave(&strings, res));
    end_vector_ops();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
Strings_Install_Functions()
{
    // Get handle to vec_info type (defined in preamble.fl)
    typeExp_ptr vec_info_tp = Get_Type("vec_info",NULL,TP_INSERT_PLACE_HOLDER);

    // Add builtin functions

    Add_ExtAPI_Function("str_reg_match", "11", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(
				GLmake_string(),
				GLmake_bool())),
			str_reg_match);

    Add_ExtAPI_Function("str_reg_extract", "11", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(
				GLmake_string(),
				GLmake_list(GLmake_string()))),
			str_reg_extract);


    Add_ExtAPI_Function("tcl2list", "1", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_list(GLmake_string())),
			tcl2list);

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

    Add_ExtAPI_Function("gen_trim", "111", FALSE,
			GLmake_arrow(GLmake_string(),
				     GLmake_arrow(GLmake_string(),
						  GLmake_arrow(
							    GLmake_string(),
							    GLmake_string()))),
			string_gen_trim);

    Add_ExtAPI_Function("trim", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_string()),
			string_trim);

    Add_ExtAPI_Function("get_vector_signature", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_string()),
			get_vector_signature);

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

static bool
is_valid_index_or_range(string s)
{
    while( 1 ) {
	if( *s == '-' ) {
	    s++;
	}
	if( !isdigit(*s) ) {
	    return( FALSE );
	}
	while( *s && isdigit(*s) ) s++;
	if( *s == ':' ) {
	    s++;
	    if( *s == '-' ) {
		s++;
	    }
	    if( !isdigit(*s) ) {
		return( FALSE );
	    }
	    while( *s && isdigit(*s) ) s++;
	}
	if( *s == ']' ) {
	    return TRUE;
	}
	if( *s != ',' ) {
	    return FALSE;
	} else {
	    s++;
	}
    }
}

static vec_ptr
split_name(string name)
{
    vec_ptr res = NULL;
    vec_ptr *res_tl_ptr = &res;
    string p = name;
    while( *p ) {
        if( is_valid_index_or_range(p) ) {
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
		bool done = FALSE;
		while( !done ) {
		    while( *e && *e != '[' ) {
			e++;
		    }
		    if( *e == '[' ) e++;
		    if( *e == 0 || is_valid_index_or_range(e) ) { done = 1; }
		}
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
    if( rp->next == NULL && rp->upper == -1 && rp->lower == 0 ) {
	// Special treatment of [-1:0] arrays.
	return rp;
    }
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
            sum += (abs(rp->upper-rp->lower)+1)*rem;
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

static string
get_base_name(vec_ptr vp)
{
    string name = strtemp("");
    if( vp->type != TXT ) {
	return( uStrsave(lstringsp, name) );
    }
    name = strtemp(vp->u.name);
    string sq = index(name, '[');
    if( sq != NULL ) {
	*sq = 0;
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
	   int res =  strcmp(v1->u.name, v2->u.name);
	    if( res != 0 ) { return res; }
	    return vec_name_cmp(v1->next, v2->next);
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
    return( vec_name_cmp(vi, vj) );
}


static bool
is_const(merge_list_ptr mp)
{
    vec_ptr vp = mp->vec;
    return( vp && vp->type == TXT && strncmp(vp->u.name, "0b", 2) == 0 );
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
            if( mp->name_signature != mp->next->name_signature || is_const(mp) )
	    {
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
	if( is_const(mp) && is_const(mp->next) ) {
	    // Merge constants
	    string tmp = strtemp(mp->vec->u.name);
	    tmp = strappend(mp->next->vec->u.name+2);
	    mp->vec->u.name = uStrsave(lstringsp, tmp);
	    mp->name_signature = mk_name_signature(mp->vec);
	    mp->next->next->prev = mp;
	    mp->next = mp->next->next;
	    not_changed = 0;
	    alts--;
	} else {
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
			if(    (r1->upper >= r1->lower)
			    && (r2->upper >= r2->lower) &&
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
			if(    (r1->upper <= r1->lower)
			    && (r2->upper <= r2->lower)
			    && ((r1->lower + 1) == r2->upper) )
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


// =================================================

vector_db_ptr
VDB_create()
{
    vector_db_ptr vdbp = (vector_db_ptr) new_rec(&vector_db_rec_mgr);
    new_ustrmgr(&(vdbp->ustring_mgr));
    create_hash(&(vdbp->sig2vec_list), 100, str_hash, str_equ);
    new_mgr(&(vdbp->vec_rec_mgr), sizeof(vec_rec));
    new_mgr(&(vdbp->range_rec_mgr), sizeof(range_rec));
    new_mgr(&(vdbp->vec_list_rec_mgr), sizeof(vec_list_rec));
    return vdbp;
}

void
VDB_destroy(vector_db_ptr vdbp)
{
    free_ustrmgr(&(vdbp->ustring_mgr));
    dispose_hash(&(vdbp->sig2vec_list), NULLFCN);
    free_mgr(&(vdbp->vec_rec_mgr));
    free_mgr(&(vdbp->range_rec_mgr));
    free_mgr(&(vdbp->vec_list_rec_mgr));
    free_rec(&vector_db_rec_mgr, vdbp);
}


void
VDB_Insert_vector(vector_db_ptr vdbp, string vec)
{
    // Store the current active record managers
    ustr_mgr *tmp_lstringsp = lstringsp;
    rec_mgr *tmp_vec_rec_mgrp = vec_rec_mgrp;
    rec_mgr *tmp_range_rec_mgrp = range_rec_mgrp;
    // Change to the new managers
    lstringsp = &(vdbp->ustring_mgr);
    vec_rec_mgrp = &(vdbp->vec_rec_mgr);
    range_rec_mgrp = &(vdbp->range_rec_mgr);
    // Perform all the operations
    vec_ptr vp = split_name(vec);
    string key = get_base_name(vp);
    vec_list_ptr cur_vlp = (vec_list_ptr) find_hash(&(vdbp->sig2vec_list), key);
    vec_list_ptr vlp = new_rec(&(vdbp->vec_list_rec_mgr));
    vlp->vec = vp;
    vlp->next = cur_vlp;
    if( cur_vlp != NULL ) {
	delete_hash(&(vdbp->sig2vec_list), key);
    }
    insert_hash(&(vdbp->sig2vec_list), key, vlp);
    // Now restore the record managers
    lstringsp = tmp_lstringsp;
    vec_rec_mgrp = tmp_vec_rec_mgrp;
    range_rec_mgrp = tmp_range_rec_mgrp;
}

static bool
inside(int lrange, int rrange, int test)
{
    if( lrange <= rrange) {
	return( lrange <= test && test <= rrange );
    } else {
	return( rrange <= test && test <= lrange );
    }
}

static bool
colliding(vec_ptr v1, vec_ptr v2)
{
    if( v1 == v2 ) return TRUE;
    if( v1 == NULL || v2 == NULL ) return FALSE;
    if( v1->type != v2->type ) return FALSE;
    if( v1->type == TXT ) {
	// Make sure a and a[2] are viewed as colliding!
	string name1 = v1->u.name;
	string name2 = v2->u.name;
	int len1 = strlen(name1);
	int len2 = strlen(name2);
	int len = (len1 <= len2)? len1 : len2;
	if( strncmp(name1,name2,len) == 0 ) {	
	    return TRUE;
	}
	return( colliding(v1->next, v2->next) );
    }
    range_ptr r1 = v1->u.ranges;
    range_ptr r2 = v2->u.ranges;
    while( r1 != NULL ) {
	for(range_ptr rt = r2; rt != NULL; rt = rt->next) {
	    if( inside(r1->lower, r1->upper, rt->lower) ||
		inside(r1->lower, r1->upper, rt->upper) )
	    {
		    return( colliding(v1->next, v2->next) );
	    }
	} 
	r1 = r1->next;
    }
    return FALSE;
}

bool
VDB_has_name_collision(vector_db_ptr vdbp, string vec, bool basename_only)
{
    // Store the current active record managers
    ustr_mgr *tmp_lstringsp = lstringsp;
    rec_mgr *tmp_vec_rec_mgrp = vec_rec_mgrp;
    rec_mgr *tmp_range_rec_mgrp = range_rec_mgrp;
    // Change to the new managers
    lstringsp = &(vdbp->ustring_mgr);
    vec_rec_mgrp = &(vdbp->vec_rec_mgr);
    range_rec_mgrp = &(vdbp->range_rec_mgr);
    // Perform the operations
    vec_ptr vp = split_name(vec);
    string key = get_base_name(vp);
    vec_list_ptr cur_vlp = (vec_list_ptr) find_hash(&(vdbp->sig2vec_list), key);
    if( basename_only ) {
	return( cur_vlp != NULL );
    }
    while(cur_vlp != NULL ) {
	if( colliding(vp, cur_vlp->vec) ) {
	    // Now restore the record managers
	    lstringsp = tmp_lstringsp;
	    vec_rec_mgrp = tmp_vec_rec_mgrp;
	    range_rec_mgrp = tmp_range_rec_mgrp;
	    return TRUE;
	}
	cur_vlp = cur_vlp->next;
    }
    // Now restore the record managers
    lstringsp = tmp_lstringsp;
    vec_rec_mgrp = tmp_vec_rec_mgrp;
    range_rec_mgrp = tmp_range_rec_mgrp;
    return FALSE;
}

static range_ptr
copy_range(rec_mgr *range_mgr_ptr, range_ptr old)
{
    if( old == NULL ) return NULL;
    range_ptr res = (range_ptr) new_rec(range_mgr_ptr);
    res->upper = old->upper;
    res->lower = old->lower;
    res->next = copy_range(range_mgr_ptr, old->next);
    return res;
}

vec_ptr
Copy_vector(rec_mgr *vector_mgr_ptr, rec_mgr *range_mgr_ptr, vec_ptr old)
{
    if( old == NULL ) return NULL;
    vec_ptr res = (vec_ptr) new_rec(vector_mgr_ptr);
    res->type = old->type;
    if( old->type == TXT ) {
	res->u.name = old->u.name;
    } else {
	res->u.ranges = copy_range(range_mgr_ptr, old->u.ranges);
    }
    res->next = Copy_vector(vector_mgr_ptr, range_mgr_ptr, old->next);
    return res;
}

#if 1
void DBG_print_range(range_ptr rp) { DBG_PRINT_RNG(rp); }
void DBG_print_vec(vec_ptr vp) { DBG_PRINT_VEC(vp); }
void DBG_print_vec_list(vec_list_ptr vlp) { DBG_PRINT_VEC_LIST(vlp); }
void DBG_print_merge_list(merge_list_ptr mlp) { DBG_PRINT_MRG_LIST(mlp); }
void DBG_print_sname_list(sname_list_ptr slp) { DBG_PRINT_STR_LIST(slp); }
#endif

static uint64_t
get_timestamp() {
    struct timespec tr;
    if( clock_gettime(CLOCK_REALTIME, &tr) ) {
	DIE("Cannot get clock??????");
    }
    return( tr.tv_sec * 1000 + tr.tv_nsec/1000000 );
}

static regex_t *
compile_regexp(string pattern, int flags)
{
    // See if pattern is in cache
    uint64_t	oldest = 0;
    int		oldest_idx = -1;
    int		hole = -1;
    for(int i = 0; i < NBR_REGEXP_CACHED; i++) {
	string old_pat = reg_cache_expr[i];
	if( old_pat != NULL ) {
	    // Cache in use
	    if( STREQ(pattern, old_pat) ) {
		reg_cache_timestamp[i] = get_timestamp();
		return( &(reg_cache_cexpr[i]) );
	    }
	    uint64_t used = reg_cache_timestamp[i];
	    if( oldest == 0 || (used < oldest) ) {
		oldest = used;
		oldest_idx = i;
	    }
	} else {
	    // Empty cache line
	    hole = i;
	}
    }
    // No cached version available.
    int new_idx = -1;
    if( hole == -1 ) {
	new_idx = hole;
    } else {
	regfree(&(reg_cache_cexpr[oldest_idx]));
	new_idx = oldest_idx;
    }
    reg_cache_expr[new_idx] = pattern;
    reg_cache_timestamp[new_idx] = get_timestamp();
    int ret = regcomp(&(reg_cache_cexpr[new_idx]), pattern, flags);
    if( ret == 0 ) {
	return( &(reg_cache_cexpr[new_idx]) );
    }
    // Failed to compile pattern
    reg_cache_expr[new_idx] = NULL;
    regerror(ret, &(reg_cache_cexpr[new_idx]), reg_error_buf, REG_ERR_BUF_SZ);
    return( NULL );
}

