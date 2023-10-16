//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************
 *                                                                      *
 *      Original author: Carl-Johan Seger 2017                          *
 *                                                                      *
 ************************************************************************/
/* draw_graph.c -- draw graph using dot */
#include "draw_graph.h"
#include "graph.h"
#include "symbol.h"


extern symbol_tbl_ptr		symb_tbl;
extern g_ptr			void_nd;


/**** PRIVATE VARIABLES ****/

static char draw_cmd[1024];
static int  draw_node_id;

static char *has_type_hint = "+";
static char *has_no_type_hint = "";

/* ----- Forward definitions local functions ----- */
static int	draw_graph_rec(FILE *fp, hash_record *hp, int depth,
			       bool do_addr, g_ptr node);
static string	get_version_name(g_ptr node);

/****************************************************************************/
/*                           Main functions                                 */
/****************************************************************************/


static char tmp[1024];

static string
str_trunc(string s)
{
    if( strlen(s) < 100 ) return s;
    strncpy(tmp, s, 100);
    tmp[100] = 0;
    return tmp;
}

#if TRACK_FREEING
void
oldGR(g_ptr onode)
{
    g_ptr nodes[2];
    g_ptr node = Get_node();
    *node = *onode;
    node->L = node->oldL;
    node->R = node->oldR;
    nodes[0] = node;
    nodes[1] = 0;
    draw_graphs(nodes, 5, FALSE);
}

void
oldGRl(g_ptr onode, int max_depth)
{
    g_ptr nodes[2];
    g_ptr node = Get_node();
    *node = *onode;
    node->L = node->oldL;
    node->R = node->oldR;
    nodes[0] = node;
    nodes[1] = 0;
    draw_graphs(nodes, max_depth, FALSE);
}
#endif

void
GR(g_ptr node)
{
    g_ptr nodes[2];
    nodes[0] = node;
    nodes[1] = 0;
    draw_graphs(nodes, 5, FALSE);
}

void
GRl(g_ptr node, int max_depth)
{
    g_ptr nodes[2];
    nodes[0] = node;
    nodes[1] = 0;
    draw_graphs(nodes, max_depth, FALSE);
}


bool
draw_graphs(g_ptr *nodes, int depth, bool display_address)
{
    FILE *fp;
    string filename;
    hash_record	draw_tbl;

    if( !Mk_output_file_in_tmp_dir("dot_draw", &fp, &filename) ) {
	Fail_pr("Cannot create dot_draw file. Out of disk space?");
	return FALSE;
    }
    create_hash(&draw_tbl, 100, ptr_hash, ptr_equ);

    fprintf(fp, "digraph G {\n");
    fprintf(fp, "node [shape=circle];\n");
    fprintf(fp, "size = \"8.5,11.0\";\n");
    fprintf(fp, "center = 1;\n");
    fprintf(fp, "margin = 0.5;\n");
    fprintf(fp, "ordering=out;\n");
    draw_node_id = 0;
    int cnt = 1;
    while( *nodes != NULL ) {
	int from = draw_node_id++;
	fprintf(fp, "n%u [shape=box,style=filled,label=\"G%d\"];\n", 
		    from, cnt);
	int to = draw_graph_rec(fp, &draw_tbl, depth, display_address, *nodes);
	fprintf(fp, "n%u -> n%u;\n", from, to);
	cnt++;
	nodes++;
    }
    fprintf(fp, "}");
    fclose(fp);
    dispose_hash(&draw_tbl, NULLFCN);
    Sprintf(draw_cmd, "dot -Tpdf %s -o %s.pdf", filename, filename);
    if( system(draw_cmd) != 0 ) {
	Fail_pr("dot failed");
	return FALSE;
    }
    Sprintf(draw_cmd, "xpdf %s.pdf &", filename);
    if( system(draw_cmd) != 0 ) {
	Fail_pr("xpdf failed");
	return FALSE;
    }
    return TRUE;
}



/********************************************************/
/*          EXPORTED EXTAPI FUNCTIONS                   */
/********************************************************/

static void
do_draw_graph(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr depth = GET_APPLY_RIGHT(l);
    g_ptr nd = GET_APPLY_RIGHT(redex);
    GRl(nd, GET_INT(depth));
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

void
Draw_Graph_Install_Functions()
{
    // Add builtin functions
    typeExp_ptr tv1 = GLnew_tVar();

    Add_ExtAPI_Function("draw_graph", "1-", FALSE,
                        GLmake_arrow(GLmake_int(),
				     GLmake_arrow(tv1,GLmake_void())),
                        do_draw_graph);

}



/****************************************************************************/
/*                          Local functions                                 */
/****************************************************************************/

static string
make_string_safe(string s)
{
    string res = strtemp("");
    while(*s) {
	if( *s == '"' ) {
	    res = charappend('\\');
	}
	res = charappend(*s);
	s++;
    }
    res = charappend(*s);
    return res;
}

static int
draw_graph_rec(FILE *fp, hash_record *hp, int depth, bool do_addr, 
		g_ptr node)
{
    int version;
    if( node == NULL ) {
	draw_node_id++;
        fprintf (fp, "n%u [shape=box, label = \"NULL\"];\n", draw_node_id);
	return draw_node_id;
    }

    if( IS_NIL(node) ) {
	draw_node_id++;
        fprintf (fp, "n%u [shape=plaintext, label = \"[]\"];\n", draw_node_id);
	return draw_node_id;
    }

    int ores;
    if( (ores = PTR2INT(find_hash(hp, (pointer) node))) != 0 ) {
	return ores;
    }
    draw_node_id++;
    insert_hash(hp, (pointer) node, INT2PTR(draw_node_id));
    int res = draw_node_id;

    if (depth <= 0) {
        fprintf(fp, "n%u [shape=plaintext, label = \"...\"];\n", res);
        return res;
    }
    depth--;

    string s;
    typeExp_ptr type_hint = GetTypeHint(node);
#ifdef INCL_TYPE_HINT_IN_DRAW
    string th = (type_hint == NULL)? "" : Get_tmp_type_string(type_hint);
#else
    string th = (type_hint == NULL)? has_no_type_hint : has_type_hint;
#endif
    switch( GET_TYPE(node) ) {
	case LAMBDA_ND:
	    {
		if( do_addr )
		    fprintf(fp, "n%u [label = \"\\\\%s,%p\", ordering=out];\n",
				res, th, node);
		else
		    fprintf(fp, "n%u [label = \"\\\\%s\", ordering=out];\n",
				res, th);
		fprintf(fp, "lbl%u [shape=plaintext, label = \"%s(%d)\"];\n",
		   res, GET_LAMBDA_VAR(node), GET_LAMBDA_LINE_NBR(node));
		int l = draw_graph_rec(fp, hp, depth, do_addr,
				       GET_LAMBDA_BODY(node));
		fprintf(fp, "n%u -> lbl%u [color=black];\n", res, res);
		fprintf(fp, "n%u -> n%u [color=black];\n", res, l);
		return res;
	    }
	case APPLY_ND:
	    {
		if( do_addr )
		    fprintf(fp, "n%u [label = \"@%s,%p\", ordering=out];\n",
				res, th, node);
		else
		    fprintf(fp, "n%u [label = \"@%s\", ordering=out];\n",
				res, th);
		int l = draw_graph_rec(fp, hp, depth, do_addr,
				       GET_APPLY_LEFT(node));
		fprintf(fp, "n%u -> n%u [color=red];\n", res, l);
		int r = draw_graph_rec(fp, hp, depth, do_addr,
				       GET_APPLY_RIGHT(node));
		fprintf(fp, "n%u -> n%u [color=blue];\n", res, r);
		return res;
	    }
	case CONS_ND: {
		if( do_addr )
		    fprintf(fp, "n%u [label = \":%s,%p\", ordering=out];\n",
				res, th, node);
		else
		    fprintf(fp, "n%u [label = \":%s\", ordering=out];\n",
				res, th);
		int l = draw_graph_rec(fp, hp, depth, do_addr,
				       GET_CONS_HD(node));
		fprintf(fp, "n%u -> n%u [color=red];\n", res, l);
		int r = draw_graph_rec(fp, hp, depth, do_addr,
				       GET_CONS_TL(node));
		fprintf(fp, "n%u -> n%u [color=blue];\n", res, r);
		return res;
	    }
	case LEAF:
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    s = Arbi_ToString(GET_AINT(node),10);
		    if( do_addr )
			fprintf(fp,
				"n%u [shape=plaintext, label = \"%s%s,%p\"];\n",
				 res, s, th, node);
		    else
			fprintf(fp,
				"n%u [shape=plaintext, label = \"%s%s\"];\n",
				 res, s, th);
		    return res;
		case STRING:
		    s = make_string_safe(GET_STRING(node));
		    if( do_addr )
			fprintf(fp,
		        "n%u [shape=plaintext, label = \"\\\"%s\\\"%s,%p\"];\n",
			   res, s, th, node);
		    else
			fprintf(fp,
			  "n%u [shape=plaintext, label = \"\\\"%s\\\"%s\"];\n",
			   res, s, th);
		    return res;
		case BOOL:
		    {
			char txt[20];
			formula f = GET_BOOL(node);
			if( f == B_One() ) {
			    Sprintf(txt, "T");
			} else 
			if( f == B_Zero() ) {
			    Sprintf(txt, "F");
			} else {
			    Sprintf(txt, "S");
			}
			if( do_addr )
			    fprintf(fp,
			        "n%u [shape=plaintext, label = \"%s%s,%p\"];\n",
				res, txt, th, node);
			else
			    fprintf(fp,
				"n%u [shape=plaintext, label = \"%s%s\"];\n",
				     res, txt, th);
			return res;
		    }
		case BEXPR:
		    {
			char txt[20];
			bexpr f = GET_BEXPR(node);
			if( f == BE_One() ) {
			    Sprintf(txt, "bT");
			} else 
			if( f == BE_Zero() ) {
			    Sprintf(txt, "bF");
			} else {
			    Sprintf(txt, "bS");
			}
			if( do_addr )
			    fprintf(fp,
			        "n%u [shape=plaintext, label = \"%s%s,%p\"];\n",
				res, txt, th, node);
			else
			    fprintf(fp,
				"n%u [shape=plaintext, label = \"%s%s\"];\n",
				 res, txt, th);
			return res;
		    }
		case EXT_OBJ:
		    {
			char txt[1024];
			Sprintf(txt, "<<%s>>", Get_ExtAPI_Object_name(
						    GET_EXT_OBJ_CLASS(node)));
			if( do_addr )
			    fprintf(fp, "n%u [shape=plaintext, %s label "
					"= \"%s%s,%p\"];\n", res,
					"color=red,",
					txt, th, node);
			else
			    fprintf(fp,
				"n%u [shape=plaintext, %s label = \"%s%s\"];\n",
				 res, "color=red,", txt, th);
			return res;
		    }


		case PRIM_FN:
		    {
			char txt[1024];
			bool hili = FALSE;
			switch ( GET_PRIM_FN(node) ) {
			    case P_EXTAPI_FN:
				Sprintf(txt, "[%s]", Get_ExtAPI_Function_Name(
							GET_EXTAPI_FN(node)));
				hili = TRUE;
				break;
			    case P_SSCANF:
				Sprintf(txt, "sscanf \\\"%s\\\"\n)",
					     GET_PRINTF_STRING(node));
				break;
			    case P_PRINTF:
				Sprintf(txt, "printf \\\"%s\\\"\n)",
					     GET_PRINTF_STRING(node));
				break;
			    case P_SPRINTF:
				Sprintf(txt, "srintf \\\"%s\\\"\n)",
					     GET_PRINTF_STRING(node));
				break;
			    case P_EPRINTF:
				Sprintf(txt, "eprintf \\\"%s\\\"\n)",
					     GET_PRINTF_STRING(node));
				break;
			    case P_FPRINTF:
				Sprintf(txt, "fprintf \\\"%s\\\"\n)",
					     GET_PRINTF_STRING(node));
				break;
			    case P_REF_VAR: {
				if( do_addr )
				    fprintf(fp,
					    "n%u [label = \"ref%s,%p\"];\n",
					    res, th, node);
				else
				    fprintf(fp, "n%u [label = \"ref%s\"];\n",
						res, th);
				int ref_var = GET_REF_VAR(node);
				int l = draw_graph_rec(fp, hp, depth, do_addr,
						       Get_RefVar(ref_var));
				fprintf(fp, "n%u -> n%u [color=black];\n",
					    res, l);
				return res;
			    }
			    case P_FAIL:
				Sprintf(txt, "P_FAIL \\\"%s\\\"\n)",
					     str_trunc(GET_FAIL_STRING(node)));
				break;
			    default:
				Sprintf(txt, "%s", Get_pfn_name(node, TRUE));
				break;
			}
			if( do_addr )
			    fprintf(fp, "n%u [shape=plaintext, %s label "
				        "= \"%s%s,%p\"];\n", res,
					(hili? "color=orange," : ""),
					txt, th, node);
			else
			    fprintf(fp,
				"n%u [shape=plaintext, %s label = \"%s%s\"];\n",
				 res, (hili? "color=orange," : ""), txt, th);
			return res;
		    }

		case VAR:
		{
		    s = GET_VAR(node);
#if 0
		    version = GET_VERSION(node);
#else
		    version = GET_LINE_NBR(node);
#endif
		    if( do_addr ) {
			fprintf(fp, "n%u [shape=plaintext, label = ", res);
			fprintf(fp, "\"V\\\"%s\\\"(%d=%s)%s,%p\"];\n", s,
				version, get_version_name(node), th, node);
		    } else {
			fprintf(fp, "n%u [shape=plaintext, label = ", res);
			fprintf(fp, "\"V\\\"%s\\\"(%d=%s)%s\"];\n", s,
				version, get_version_name(node), th);
		    }
		    return res;
		}

		case USERDEF:
		{
		    fn_ptr fn = GET_USERDEF(node);
		    s = fn->name;
		    version = GET_VERSION(node);
		    if( do_addr ) {
			fprintf(fp, "n%u [shape=plaintext, label = ", res);
			fprintf(fp, "\"UD\\\"%s\\\"(%d=%s)%s,%p\"];\n", s,
				version, get_version_name(node), th, node);
		    } else {
			fprintf(fp, "n%u [shape=plaintext, label = ", res);
			fprintf(fp, "\"UD\\\"%s\\\"(%d=%s)%s\"];\n", s,
				version, get_version_name(node), th);
		    }
		    return res;
		}

		default:
		    DIE("Unknown LEAF type");
	    }

	default:
	    DIE("Should never happen");
    }
}

static string
get_version_name(g_ptr node)
{
    int version = GET_VERSION(node);
    if( version == 0 ) return "";
    fn_ptr fn = Find_Function_Def(symb_tbl,GET_VAR(node));
    if( fn == NULL ) return "";
    oll_ptr ol = fn->overload_list;
    if( ol == NULL ) return "";
    int cnt = 1;
    while(cnt != version ) {
	ol = ol->next;
	cnt++;
    }
    return ol->fn->name;
}
