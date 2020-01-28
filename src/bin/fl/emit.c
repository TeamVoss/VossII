/************************************************************************
 *									*
 *	Copyright (C) 1994 Carl-Johan Seger      		        *
 *									*
 ************************************************************************/
/* emit.c -- emit result for use of fl */

#include "emit.h"

#ifdef E_DEBUG
#define DEB_EMIT_PRINTF(stmt)	fprintf stmt
#else /* E_DEBUG */
#define DEB_EMIT_PRINTF(stmt)	/* Empty */
#endif /* E_DEBUG */

/**** PRIVATE VARIABLES ****/

/* ----- Forward definitions local functions ----- */
static VOID	emit_fn_node(unint result, unint op, unint op1, unint op2);

/****************************************************************************/
/*                           Main functions                                 */
/****************************************************************************/

VOID
Begin_emit(string file_name)
{
    if( LL_Open(file_name, 't') != TRUE ) {
	DIE("Could not open file for writing\n");
    }
}

VOID
End_emit()
{
    LL_Close();
}

VOID
Emit_int(int val)
{
    LL_Write((pointer) &val, sizeof(int));
    DEB_EMIT_PRINTF((stderr, "emit_int:%d\n", val));
}

VOID
Emit_string(string s)
{
    /* Make sure length of name (including \0) is a multiple of words */
    int 	slen = strlen(s)+1;
    int 	len = 4*((slen+3)/4);
    int 	zero_int = 0;

    LL_Write((pointer) &len, sizeof(int));
    LL_Write((pointer) s, slen);
    LL_Write((pointer) &zero_int, len-slen);
    DEB_EMIT_PRINTF((stderr, "emit_string:%d %s\n", len, s));
}

VOID
Emit_node_name(name_list_ptr name_list)
{
    name_list_ptr  p;
    int		   cnt = 0;

    /* First find out the nbr of node aliases */
    for(p = name_list; p != NULL; p = p->next ) {
	cnt++;
    }
    Emit_int(cnt);
    /* Emit the list of aliases */
    for(p = name_list; p != NULL; p = p->next ) {
	Emit_string( p->name );
    }
}

VOID
Emit_and_node(unint result, unint op1, unint op2)
{
    emit_fn_node(result, AND, op1, op2);
}

VOID
Emit_or_node(unint result, unint op1, unint op2)
{
    emit_fn_node(result, OR, op1, op2);
}

VOID
Emit_not_node(unint result, unint op1)
{
    emit_fn_node(result, NOT, op1, -1);
}

VOID
Emit_del_node(unint to, unint fromH, unint fromL)
{
    emit_fn_node(to, DEL, fromH, fromL);
}

VOID
Emit_free_node(unint node)
{
    emit_fn_node(node, FREE_, -1, -1);
}

VOID
Emit_start()
{
    unsigned int i = START_MAGIC;
    LL_Write((pointer) &i, sizeof(int));
}

VOID
Emit_composite()
{
    unsigned int i = COMPOSITE_MAGIC;
    LL_Write((pointer) &i, sizeof(int));
}

VOID
Emit_operations()
{
    unsigned int i = OPERATIONS_MAGIC;
    LL_Write((pointer) &i, sizeof(int));
}

VOID
Emit_end_marker()
{
    emit_fn_node(-1, END, -1, -1);
}

VOID
Emit_delay()
{
    unsigned int i = DELAY_MAGIC;
    LL_Write((pointer) &i, sizeof(int));
}

VOID
Emit_states()
{
    unsigned int i = STATES_MAGIC;
    LL_Write((pointer) &i, sizeof(int));
}

VOID
Emit_fanout()
{
    unsigned int i = FANOUT_MAGIC;
    LL_Write((pointer) &i, sizeof(int));
}

/****************************************************************************/
/*                          Local functions                                 */
/****************************************************************************/

static VOID
emit_fn_node(unint result, unint op, unint op1, unint op2)
{
    fn_exec_rec rec;
    rec.result    = result;
    rec.operation = op;
    rec.op1       = op1;
    rec.op2       = op2;
    DEB_EMIT_PRINTF((stderr,"emit_fn: %d <- %s(%d, %d)\n", result,(op==AND)?"and" : (op == OR)? "or" : (op == NOT)? "not" : (op==DEL)? "del" : (op==FREE_)? "free" : "???" ,op1,op2));
    LL_Write((pointer) &rec, sizeof(fn_exec_rec));
}
