//------------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------
/******************************************************************************/
/*									      */
/* Original author: Carl-Johan Seger, 2022                                    */
/*									      */
/******************************************************************************/
#include <ctype.h>
#include "voss_strings.h"
#include "graph.h"
#include "io.h"

/******************************************************************************/
/*                               GLOBAL VARIABLES                             */
/******************************************************************************/


// Global variables referenced -------------------------------------------------
extern str_mgr	*stringsp;
extern char     FailBuf[4096];
extern g_ptr	void_nd;

/******************************************************************************/
/*                              PRIVATE VARIABLES                             */
/******************************************************************************/

static string s_Nil;
static string s_NilAbove;
static string s_TxtBeside;
static string s_Nest;
static string s_Union;
static string s_Beside;
static string s_Above;
static string s_Empty;


/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

#define EXTRACT(n)						\
        if( GET_TYPE(node) != CONS_ND ) { return FALSE; }	\
        (n)  = GET_SND(node);					\
        node = GET_FST(node);

#define IS_CONSTRUCTOR(name,nd)                                                \
    (IS_LEAF(nd) && IS_STRING(nd) && STREQ(GET_STRING(nd), s_##name))

static bool
is_Nil(g_ptr node)
{
    return( IS_CONSTRUCTOR(Nil,node) );
}

static bool
is_NilAbove(g_ptr node, g_ptr *doc)
{
    EXTRACT(*doc);
    return( IS_CONSTRUCTOR(NilAbove,node) );
}

static bool
is_TxtBeside(g_ptr node, int *len, string *txt, g_ptr *doc)
{
    g_ptr g_txt;
    EXTRACT(*doc);
    EXTRACT(g_txt);
    if( !IS_CONSTRUCTOR(TxtBeside,node) ) return FALSE;
    *len = GET_INT(GET_FST(g_txt));
    *txt = GET_STRING(GET_SND(g_txt));
    return TRUE;
}

static bool
is_Nest(g_ptr node, int *k, g_ptr *doc)
{
    g_ptr g_k;
    EXTRACT(*doc);
    EXTRACT(g_k);
    if( !IS_CONSTRUCTOR(Nest,node) ) return FALSE;
    *k = GET_INT(g_k);
    return TRUE;
}

static bool
is_Union(g_ptr node, g_ptr *doc1, g_ptr *doc2)
{
    EXTRACT(*doc2);
    EXTRACT(*doc1);
    return( IS_CONSTRUCTOR(Union,node) );
}

static bool
is_Beside(g_ptr node, g_ptr *doc1, bool *b, g_ptr *doc2)
{
    g_ptr g_b;
    EXTRACT(*doc2);
    EXTRACT(g_b);
    EXTRACT(*doc1);
    if( !IS_CONSTRUCTOR(Beside,node) ) return FALSE;
    *b = (GET_BOOL(g_b) == B_One())? TRUE : FALSE;
    return TRUE;
}

static bool
is_Above(g_ptr node, g_ptr *doc1, bool *b, g_ptr *doc2)
{
    g_ptr g_b;
    EXTRACT(*doc2);
    EXTRACT(g_b);
    EXTRACT(*doc1);
    if( !IS_CONSTRUCTOR(Above,node) ) return FALSE;
    *b = (GET_BOOL(g_b) == B_One())? TRUE : FALSE;
    return TRUE;
}

static bool
is_Empty(g_ptr node, ...)
{
    return( IS_CONSTRUCTOR(Empty,node) );
}

#undef EXTRACT

static string
get_doc_constructor(g_ptr node)
{
    g_ptr d, p;
    int len, k;
    string txt;
    bool b;

    if( is_Nil(node) ) { return s_Nil; }
    if( is_NilAbove(node, &d) ) { return s_NilAbove; }
    if( is_TxtBeside(node, &len, &txt, &d) ) { return s_TxtBeside; }
    if( is_Nest(node, &k, &d) ) { return s_Nest; }
    if( is_Union(node, &d, &p) ) { return s_Union; }
    if( is_Beside(node, &d, &b, &p) ) { return s_Beside; }
    if( is_Above(node, &d, &b, &p) ) { return s_Above; }
    if( is_Empty(node) ) { return s_Empty; }
    DIE("Should not be possible!");
}

static void
do_pr(io_ptr ip, string s)
{
   if( strcmp(ip->name, "stdout") == 0 ) {
	FP(stdout_fp, "%s", s);
    } else
    if( strcmp(ip->name, "stderr") == 0 ) {
	FP(err_fp, "%s", s);
    } else
    if( strcmp(ip->name, "stdinfo") == 0 ) {
	FP(warning_fp, "%s", s);
    } else {
	fprintf(ip->fp, "%s", s);
    }
}

static bool
doc2print(int k, string end, g_ptr d, io_ptr ip)
{
  start:
    if( is_Nil(d) ) {
	do_pr(ip, end);
	return TRUE;
    }
    g_ptr p;
    if( is_NilAbove(d, &p) ) {
	do_pr(ip, "\n");
	d = p;
	goto start;
    }
    int len;
    string txt;
    if( is_TxtBeside(d, &len, &txt, &p) ) {
	int j = k+len;
	do_pr(ip, tprintf("%*s%s", k, " ", txt));
	k = j;
	d = p;
      startL:
	if( is_Nil(d) ) {
	    do_pr(ip, end);
	    return TRUE;
	}
	if( is_NilAbove(d, &p) ) {
	    do_pr(ip, "\n");
	    d = p;
	    goto start;
	}
	if( is_TxtBeside(d, &len, &txt, &p) ) {
	    int j = k+len;
	    do_pr(ip, txt);
	    d = p;
	    k = j;
	    goto startL;
	}
	int i;
	if( is_Nest(d, &i, &p) ) {
	    d = p;
	    goto startL;
	}
	Fail_pr("Type constructor %s in layL\n", get_doc_constructor(d));
	return( FALSE );
    }
    int i;
    if( is_Nest(d, &i, &p) ) {
	int j = k+i;
	d = p;
	k = j;
	goto start;
    }
    Fail_pr("Type constructor %s in lay\n", get_doc_constructor(d));
    return( FALSE );
}

static void
do_layout(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_k, g_end, d, g_fp;
    EXTRACT_4_ARGS(redex, g_k, g_end, d, g_fp);
    int k = GET_INT(g_k);
    string end = GET_STRING(g_end);
    io_ptr ip = GET_FILE_IO_PTR(g_fp);
    if( !doc2print(k, end, d, ip) ) {
	MAKE_REDEX_FAILURE(redex, wastrsave(stringsp, FailBuf));
	return;
    }
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

void
Doc_Init()
{
    s_Nil	= Mk_constructor_name("Nil");
    s_NilAbove  = Mk_constructor_name("NilAbove");
    s_TxtBeside = Mk_constructor_name("TxtBeside");
    s_Nest	= Mk_constructor_name("Nest");
    s_Union	= Mk_constructor_name("Union");
    s_Beside	= Mk_constructor_name("Beside");
    s_Above	= Mk_constructor_name("Above");
    s_Empty	= Mk_constructor_name("Empty");
}

void
Doc_Install_Functions()
{
    typeExp_ptr doc_tp = Get_Type("doc", NULL, TP_INSERT_PLACE_HOLDER);
    typeExp_ptr stream_tp = Get_Type("stream", NULL, TP_INSERT_PLACE_HOLDER);

    Add_ExtAPI_Function("do_layout", "1111", FALSE,
			GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(
			      GLmake_string(),
			      GLmake_arrow(
				doc_tp,
				GLmake_arrow(
				  stream_tp,
				  GLmake_void())))),
			do_layout);
}

