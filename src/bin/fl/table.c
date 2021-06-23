//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2019			*/
/*									*/
/************************************************************************/
#include "table.h"
#include "graph.h"
#include <math.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern symbol_tbl_ptr	symb_tbl;
extern str_mgr		strings;
extern jmp_buf		*start_envp;
extern char		FailBuf[4096];
extern g_ptr		void_nd;
extern bool		Do_gc_asap;
extern FILE		*odests_fp;

/***** PRIVATE VARIABLES *****/
static int		table_oidx;
static table_ptr	table_free_list = NULL;

/* ----- Forward definitions local functions ----- */
static table_ptr	    get_table_rec();
static g_ptr		    scan_tail;
static rec_mgr		    table_rec_mgr;

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
table_create(g_ptr redex)
{
    g_ptr   g_n;
    EXTRACT_1_ARG(redex, g_n);
    int n = GET_INT(g_n);
    table_ptr tp = get_table_rec();
    create_hash(&(tp->tbl), n, G_rec_hash, G_rec_equ);
    MAKE_REDEX_EXT_OBJ(redex, table_oidx, tp);
}

static void
table_member(g_ptr redex)
{
    g_ptr   g_tbl, g_key;
    EXTRACT_2_ARGS(redex, g_tbl, g_key);
    table_ptr tp = (table_ptr) GET_EXT_OBJ(g_tbl);
    if( find_hash(&(tp->tbl), g_key) != NULL ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
}

static void
table_get(g_ptr redex)
{
    g_ptr   g_tbl, g_key;
    EXTRACT_2_ARGS(redex, g_tbl, g_key);
    table_ptr tp = (table_ptr) GET_EXT_OBJ(g_tbl);
    g_ptr data = (g_ptr) find_hash(&(tp->tbl), g_key);
    if( data == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("tbl_get with key not in table"));
    } else {
	OVERWRITE(redex, data);
    }
}

static void
table_insert(g_ptr redex)
{
    g_ptr   g_tbl, g_key, g_data;
    EXTRACT_3_ARGS(redex, g_tbl, g_key, g_data);
    table_ptr tp = (table_ptr) GET_EXT_OBJ(g_tbl);
    if( find_hash(&(tp->tbl), g_key) != NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("tbl_insert with duplicate key"));
    } else {
	insert_hash(&(tp->tbl), (pointer) g_key, (pointer) g_data);
	SET_REFCNT(g_key, MAX_REF_CNT);
	SET_REFCNT(g_data, MAX_REF_CNT);
	MAKE_REDEX_EXT_OBJ(redex, table_oidx, tp);
    }
}

static void
table_delete(g_ptr redex)
{
    g_ptr   g_tbl, g_key;
    EXTRACT_2_ARGS(redex, g_tbl, g_key);
    table_ptr tp = (table_ptr) GET_EXT_OBJ(g_tbl);
    if( find_hash(&(tp->tbl), g_key) == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("tbl_delete with key not in table"));
    } else {
	delete_hash(&(tp->tbl), (pointer) g_key);
	MAKE_REDEX_EXT_OBJ(redex, table_oidx, tp);
    }
}

static void
table_clear(g_ptr redex)
{
    g_ptr   g_tbl;
    EXTRACT_1_ARG(redex, g_tbl);
    table_ptr tp = (table_ptr) GET_EXT_OBJ(g_tbl);
    int sz = hash_size(&(tp->tbl));
    dispose_hash(&(tp->tbl), NULLFCN);
    create_hash(&(tp->tbl), sz, G_rec_hash, G_rec_equ);
    MAKE_REDEX_EXT_OBJ(redex, table_oidx, tp);
}

static void
scan_fn(pointer pkey, pointer pdata)
{
    g_ptr key = (g_ptr) pkey;
    g_ptr data = (g_ptr) pdata;
    APPEND1(scan_tail, Make_CONS_ND(key, data));
}

static void
table_to_list(g_ptr redex)
{
    g_ptr   g_tbl;
    EXTRACT_1_ARG(redex, g_tbl);
    table_ptr tp = (table_ptr) GET_EXT_OBJ(g_tbl);
    MAKE_REDEX_NIL(redex);
    scan_tail = redex;
    scan_hash(&(tp->tbl), scan_fn);
}

static void
list_to_table(g_ptr redex)
{
    g_ptr   list;
    EXTRACT_1_ARG(redex, list);
    int n = 0;
    for(g_ptr lp = list; !IS_NIL(lp); lp = GET_CONS_TL(lp)) {
	n++;
    }
    table_ptr tp = get_table_rec();
    create_hash(&(tp->tbl), n, G_rec_hash, G_rec_equ);
    for(g_ptr lp = list; !IS_NIL(lp); lp = GET_CONS_TL(lp)) {
	g_ptr key = GET_CONS_HD(lp);
	g_ptr data = GET_CONS_TL(lp);
	if( find_hash(&(tp->tbl), key) != NULL ) {
	    MAKE_REDEX_FAILURE(redex, Fail_pr("list2tbl with duplicate key"));
	    return;
	}
	insert_hash(&(tp->tbl), (pointer) key, (pointer) data);
	SET_REFCNT(key, MAX_REF_CNT);
	SET_REFCNT(data, MAX_REF_CNT);
    }
    MAKE_REDEX_EXT_OBJ(redex, table_oidx, tp);
}


/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static table_ptr
get_table_rec()
{
    table_ptr res;
    if( table_free_list != NULL ) {
        res = table_free_list;
        table_free_list = table_free_list->next;
    } else {
        res = (table_ptr) new_rec(&table_rec_mgr);
    }
    res->flag = 0;
    res->next = NULL;
    return res;
}

static void
scan_mark_entries(pointer pkey, pointer pdata)
{
    g_ptr key = (g_ptr) pkey;
    g_ptr data = (g_ptr) pdata;
    Mark(key);
    Mark(data);
    SET_REFCNT(key, MAX_REF_CNT);
    SET_REFCNT(data, MAX_REF_CNT);
}

static void
table_mark_fn(pointer p)
{
    table_ptr tp = (table_ptr) p;
    tp->flag = 1;
    scan_hash(&(tp->tbl), scan_mark_entries);
    return;
}

static void
table_sweep_fn(void)
{
    table_ptr tp;
    table_free_list = NULL;
    FOR_REC(&table_rec_mgr, table_ptr, tp) {
        if( tp->flag == 1 ) {
            tp->flag = 0;
        } else {
            tp->next = table_free_list;
            table_free_list = tp;
        }
    }
}

static void
table_save_fn(FILE *fp, pointer p)
{
    (void) fp;
    (void) p;
    DIE("table_save not implemented yet");
}

static pointer
table_load_fn(FILE *fp)
{
    (void) fp;
    DIE("table_load not implemented yet");
}

static formula
table_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    table_ptr tp1 = (table_ptr) p1;
    table_ptr tp2 = (table_ptr) p2;
    return( tp1 == tp2 );
}

static int	    *tbl_g_cntp;
static hash_record  *tbl_g_tblp;
static SHA256_ptr   tbl_sha;

static void
do_sha256_comp(pointer pkey, pointer pdata)
{
    g_ptr key = (g_ptr) pkey;
    g_ptr data = (g_ptr) pdata;
    SHA256_traverse_graph(tbl_g_cntp, tbl_g_tblp, tbl_sha, key);
    SHA256_traverse_graph(tbl_g_cntp, tbl_g_tblp, tbl_sha, data);
}

static int
table_sha256_fn(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, pointer a)
{
    table_ptr tp = (table_ptr) a;
    int res = *g_cntp;
    *g_cntp = res+1;
    SHA256_printf(sha, "%d=TBL\n", res);
    tbl_g_cntp = g_cntp;
    tbl_g_tblp = g_tblp;
    tbl_sha = sha;
    scan_hash(&(tp->tbl), do_sha256_comp);
    return res;
}


/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Table_Init()
{
    new_mgr(&table_rec_mgr, sizeof(table_rec));
    //
    table_oidx = Add_ExtAPI_Object("table",
                                   table_mark_fn,
                                   table_sweep_fn,
                                   table_save_fn,
                                   table_load_fn,
                                   NULL,
                                   table_eq_fn,
                                   NULL,
                                   NULL,
				   table_sha256_fn);
    table_free_list = NULL;
}

void
Table_Install_Functions()
{
    typeExp_ptr tv1 = GLnew_tVar();
    typeExp_ptr tv2 = GLnew_tVar();

    Add_ExtAPI_Function("tbl_create", "1", TRUE,
			GLmake_arrow(GLmake_int(), GLmake_tbl(tv1, tv2)),
			table_create);

    Add_ExtAPI_Function("tbl_member", "11", FALSE,
			GLmake_arrow(
			    GLmake_tbl(tv1, tv2),
			    GLmake_arrow(tv1, GLmake_bool())),
			table_member);

    Add_ExtAPI_Function("tbl_get", "11", FALSE,
			GLmake_arrow(GLmake_tbl(tv1, tv2),
				     GLmake_arrow(tv1, tv2)),
			table_get);

    Add_ExtAPI_Function("tbl_insert", "111", FALSE,
			GLmake_arrow(
			    GLmake_tbl(tv1, tv2),
			    GLmake_arrow(
				tv1,
				GLmake_arrow(tv2, GLmake_tbl(tv1, tv2)))),
			table_insert);

    Add_ExtAPI_Function("tbl_delete", "11", FALSE,
			GLmake_arrow(
			    GLmake_tbl(tv1, tv2),
			    GLmake_arrow(tv1, GLmake_tbl(tv1, tv2))),
			table_delete);

    Add_ExtAPI_Function("tbl_clear", "1", FALSE,
			GLmake_arrow(GLmake_tbl(tv1, tv2),GLmake_tbl(tv1, tv2)),
			table_clear);

    Add_ExtAPI_Function("tbl2list", "1", FALSE,
			GLmake_arrow(GLmake_tbl(tv1, tv2),
				     GLmake_list(GLmake_tuple(tv1,tv2))),
			table_to_list);

    Add_ExtAPI_Function("list2tbl", "1", FALSE,
			GLmake_arrow(GLmake_list(GLmake_tuple(tv1,tv2)),
				     GLmake_tbl(tv1, tv2)),
			list_to_table);
}

