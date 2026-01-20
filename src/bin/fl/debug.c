//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************
 *									*
 *	Original author: Carl-Johan Seger 2017      		        *
 *									*
 ************************************************************************/
/* debug.c -- code for comparing the graphs inside two fl runs */
/* Really only for extremely tricky bugs.... */
#if DEBUG
#include "debug.h"
#include "graph.h"

#if DBG_TRACE_AND_SAVE
extern bool dbg_dummy_graph_comparison;
#endif

/**** PRIVATE VARIABLES ****/
static char graph_file_0[64];
static char ready_file_0[64];
static char graph_file_1[64];
static char ready_file_1[64];
static char result_file[64];
static char result_ready_file[64];
static char cmd[128];

/* ----- Forward definitions local functions ----- */
static void run_compare_program();
static void rm_file(string file);
static void push_graph(string file);

/****************************************************************************/
/*                           Main functions                                 */
/****************************************************************************/


void
Start_Compare_Fun(int id)
{
    /* Remove any leftover files */
    Sprintf(graph_file_0, "_GrApH_0");
    Sprintf(ready_file_0, "_GrApH_0_ready");
    Sprintf(graph_file_1, "_GrApH_1");
    Sprintf(ready_file_1, "_GrApH_1_ready");
    Sprintf(result_file, "_GrApH_compare_result");
    Sprintf(result_ready_file, "_GrApH_compare_result_ready");
    if( id == 0 ) {
	if( fork() != 0 ) {
	    /* Parent returns immediately */
	    return;
	} else {
	    /* Child becomes a compare process */
	    run_compare_program();
	}
    }
}

bool
Compare_Graphs(int id, g_ptr g)
{
    FILE *fp;

#if DBG_TRACE_AND_SAVE
    if( dbg_dummy_graph_comparison )
	return TRUE;
#endif

    string graph_file = (id == 0)? graph_file_0 : graph_file_1;
    string ready_file = (id == 0)? ready_file_0 : ready_file_1;
    Save_graph("dummy", graph_file, g);

    fp = fopen(ready_file, "w");
    fputc('1', fp);
    fclose(fp);

    struct timespec sleep_time;
    sleep_time.tv_sec  = 0;
    sleep_time.tv_nsec = 1000000;

    while( (fp = fopen(result_ready_file, "r")) == NULL ) {
	nanosleep(&sleep_time, NULL);
    }
    fclose(fp);
    fp = fopen(result_file, "r");
    int res = fgetc(fp);
    fclose(fp);
    if( res == '1' ) {
	rm_file(ready_file);
	/* Wait for result file to be removed */
	while( (fp = fopen(result_ready_file, "r")) != NULL ) {
	    fclose(fp);
	    nanosleep(&sleep_time, NULL);
	}
	return TRUE;
    } else {
	FP(err_fp, "===== FAILURE COMPARING GRAPHS =====\n");
	return FALSE;
    }
}

g_ptr
DBG_load_graph(string file)
{
    g_ptr g = Get_node();
    Load_graph("dummy", file, g);
    return g;
}

/****************************************************************************/
/*                          Local functions                                 */
/****************************************************************************/

static void
run_compare_program()
{
    FILE *fp;
    struct timespec sleep_time;
    sleep_time.tv_sec  = 0;
    sleep_time.tv_nsec = 1000000;

    while( 1 ) {
	while( fopen(ready_file_0, "r") == NULL ) nanosleep(&sleep_time, NULL);
	g_rec g0;
	Load_graph("dummy", graph_file_0, &g0);
	push_graph(graph_file_0);

	while( fopen(ready_file_1, "r") == NULL ) nanosleep(&sleep_time, NULL);
	g_rec g1;
	Load_graph("dummy", graph_file_1, &g1);
	push_graph(graph_file_1);

	bool ok;
	if( cmp_graph(&g0, &g1) ) {
	    /* Graphs are equal */
	    ok = TRUE;
	    fp = fopen(result_file, "w");
	    fputc('1', fp);
	    fclose(fp);
	} else {
	    /* Graphs differ */
	    ok = FALSE;
	    fprintf(stderr, "====================================\n");
	    fprintf(stderr, "   FAILURE \n");
	    fprintf(stderr, "====================================\n");
	    fp = fopen(result_file, "w");
	    fputc('0', fp);
	    fclose(fp);
	}
	fp = fopen(result_ready_file, "w");
	fputc('1', fp);
	fclose(fp);

	/* Wait for ready_files to be removed */
	while( (fp = fopen(ready_file_0, "r")) != NULL ) {
	    fclose(fp);
	    nanosleep(&sleep_time, NULL);
	}
	while( (fp = fopen(ready_file_1, "r")) != NULL ) {
	    fclose(fp);
	    nanosleep(&sleep_time, NULL);
	}
	if( ok ) {
	    /* Finally remove result file (to let children go on) */
	    rm_file(result_file);
	    rm_file(result_ready_file);
	} else {
	    exit( -1 );
	}
    }
}

static void
rm_file(string file)
{
    Sprintf(cmd, "rm -f %s", file);
    system(cmd);
}

static void
push_graph(string file)
{
    Sprintf(cmd, "cp %s-8 %s-9 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-7 %s-8 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-6 %s-7 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-5 %s-6 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-4 %s-5 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-3 %s-4 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-2 %s-3 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s-1 %s-2 2>/dev/null", file, file);
    system(cmd);
    Sprintf(cmd, "cp %s %s-1 2>/dev/null", file, file);
    system(cmd);
}
#endif
