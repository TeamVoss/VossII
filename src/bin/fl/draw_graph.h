//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2017                        *
*                                                                   *
********************************************************************/
/* draw_graph.h -- Definitions for draw_graph.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
void	Draw_Graph_Install_Functions();


/* ----- Function prototypes for public functions ----- */
bool draw_graphs(g_ptr *nodes, int depth, bool display_address);
void GR(g_ptr node);
void GRl(g_ptr node, int max_depth);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef DRAW_GRAPH_H
#define DRAW_GRAPH_H
#include "fl.h"	/* Global data types and include files 		     */


#endif /* DRAW_GRAPH_H */
#endif /* EXPORT_FORWARD_DECL */
