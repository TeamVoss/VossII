/********************************************************************
*                                                                   *
*     Copyright (C) 1994 Carl-Johan Seger                           *
*                                                                   *
********************************************************************/
/* emit.h -- Definitions for emit.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
VOID   Begin_emit(string file_name);
VOID   End_emit();
VOID   Emit_int(int val);
VOID   Emit_string(string s);
VOID   Emit_node_name(name_list_ptr name_list);
VOID   Emit_and_node(unsigned int result, unsigned int op1, unsigned int op2);
VOID   Emit_or_node(unsigned int result, unsigned int op1, unsigned int op2);
VOID   Emit_not_node(unsigned int result, unsigned int op1);
VOID   Emit_del_node(unsigned int to, unsigned int fromH, unsigned int fromL);
VOID   Emit_free_node(unsigned int node);
VOID   Emit_start();
VOID   Emit_composite();
VOID   Emit_operations();
VOID   Emit_end_marker();
VOID   Emit_delay();
VOID   Emit_states();
VOID   Emit_fanout();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef EMIT_H
#define EMIT_H
#include "fl.h"	/* Global data types and include files 		     */

#define FREE_	0
#define AND	1
#define OR	2
#define NOT	3
#define DEL	4
#define END	5

typedef struct {
	unsigned int	result:29;
	unsigned int	operation:3;
	unsigned int	op1;
	unsigned int	op2;
} fn_exec_rec;

#define BD
#ifdef BD
#define START_MAGIC		0x70920476
#else /* BD */
#define START_MAGIC		0x62912900
#endif /* BD */
#define COMPOSITE_MAGIC		0x92002029
#define OPERATIONS_MAGIC	0x10ab002b
#define DELAY_MAGIC		0x0021e91b
#define STATES_MAGIC		0xa4012b2f
#define FANOUT_MAGIC		0xbf801200

#endif /* EMIT_H */
#endif /* EXPORT_FORWARD_DECL */
