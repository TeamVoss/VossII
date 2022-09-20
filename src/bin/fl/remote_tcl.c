//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "remote_tcl.h"
#include "graph.h"
#include "typecheck.h"
#include <ctype.h>

/************************************************************************/
/*			Global Variables				*/
/************************************************************************/
extern str_mgr  strings;
extern char     FailBuf[4096];
extern jmp_buf  *start_envp;
extern g_ptr    root_node;
extern g_ptr    *sp;
extern void PR(g_ptr np);
extern FILE     *to_tcl_fp;
extern bool     gui_mode;
extern g_ptr	void_nd;

/************************************************************************/
/*			Local Variables					*/
/************************************************************************/
static int	    tcl_initialized = 0;
static buffer	    tcl_callback_buf;
static tstr_ptr	    tmp_str_buf;

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/
static bool     parse_int(string s, int *resp);
static bool     parse_bool(string s, formula *resp);
static bool     get_end_of_item(string s, string *endp);
static bool     tcl_to_g_ptr(string txt, typeExp_ptr arg_type, g_ptr *resp);
static void     g_ptr2tcl(g_ptr np, typeExp_ptr type, FILE *tcl_fp);
static bool     ok_tcl_callback_type(typeExp_ptr type);
static bool	can_be_sent_to_tcl(g_ptr np, typeExp_ptr type);


/************************************************************************/
/*			Public Functions				*/
/************************************************************************/
void
Init_tcl()
{
    tcl_initialized = TCL_MAGIC_NUMBER;
    new_buf(&tcl_callback_buf, 100, sizeof(tcl_callback_rec));
    tmp_str_buf = new_temp_str_mgr();
}

bool
Register_tcl_callback(string name, symbol_tbl_ptr stbl)
{
    if( !gui_mode ) { return TRUE; }
    ASSERT( tcl_initialized == TCL_MAGIC_NUMBER);
    fn_ptr fp = Find_Function_Def(stbl, name);
    if( fp == NULL ) { return FALSE; }
    g_ptr fn = Get_Fun_Expr(fp);
    typeExp_ptr type = Get_Fun_Type(fp);
    tcl_callback_rec new_rec;
    int fn_idx = COUNT_BUF(&tcl_callback_buf);
    if( !ok_tcl_callback_type(type) ) {
        Fail_pr("Not a valid type for tcl callback function %s", name);
        return FALSE;
    }
    new_rec.name = name;
    new_rec.fun  = fn;
    new_rec.type = type;
    push_buf(&tcl_callback_buf, &new_rec);
    fprintf(to_tcl_fp,
	    "proc %s args { eval __basic_fl_callback Fl%d:%s $args; }\n",
            name, fn_idx, name);
    fflush(to_tcl_fp);
    return TRUE;
}

static int
get_callback_fun(string cmd) {
    string s = cmd;
    if( *s != 'F' && (*s && (*(s+1) != 'l')) ) { return( -2 ); }
    s += 2;
    int i = 0;
    while( *s ) {
	switch(*s) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                i = 10*i + (*s-'0');
                s++;
                break;
	    case ' ':
	    case ':':
		return(i);
	    default:
		return(-1);
	}
    }
    return(-1);
}

void
Tcl_callback_eval(string cmd, int rid, FILE *tcl_fp)
{
    ASSERT( tcl_initialized == TCL_MAGIC_NUMBER);
    while( *cmd == ' ' ) cmd++;
    int fun_idx = get_callback_fun(cmd);
    if( fun_idx < 0 ) {
	Fail_pr("Not a valid callback function format (%s). WHAT????", cmd);
	fprintf(tcl_fp,"%d 0%s\n", rid, protect(FailBuf));
	return;
    }
    tstr_ptr tstrings = new_temp_str_mgr();
    tcl_callback_ptr tcp;
    // Find callback function
    tcp = (tcl_callback_ptr) M_LOCATE_BUF(&tcl_callback_buf,fun_idx);
    typeExp_ptr type = tcp->type;
    g_ptr redex = tcp->fun;
    SET_REFCNT(redex, MAX_REF_CNT);
    string p = cmd; 
    while( *p && *p != ' ') p++;
    if( *p ) p++;
    /* Now convert the (string) arguments to graph structures */
    int arg_cnt = 0;
    while( type->typeOp == arrow_tp ) {
        string start = p; 
	if( *start == 0 ) {
            Fail_pr("Argument %d missing for callback function %s",
		    arg_cnt+1, tcp->name);
	    fprintf(tcl_fp,"%d 0%s\n", rid, protect(FailBuf));
            free_temp_str_mgr(tstrings);
	    return;
	}
        while( *p && *p != ' ') p++;
        char tmp = *p;
        *p = 0;
        string arg_s = gen_strtemp(tstrings, unprotect(start));
        *p = tmp;
        if( *p ) p++;
        typeExp_ptr arg_type = type->typelist->type;
        arg_cnt++;
        g_ptr arg = NULL;
        if( !tcl_to_g_ptr(arg_s, arg_type, &arg) ) {
            Fail_pr("Argument %d is of wrong type in tcl function %s",
                    arg_cnt, tcp->name);
	    fprintf(tcl_fp,"%d 0%s\n", rid, protect(FailBuf));
            free_temp_str_mgr(tstrings);
	    return;
        }
        redex = Make_APPL_ND(redex, arg);
	type = Get_Real_Type(type->typelist->next->type);
    }
    free_temp_str_mgr(tstrings);

    // Then evaluate the function
    redex = Eval(redex);
    if( is_fail(redex) ) {
	fprintf(tcl_fp, "%d 0", rid);
	Tcl_printf(tcl_fp, "%s\nIn fl callback function %s\n",
			   FailBuf, tcp->name);
	fprintf(tcl_fp, "\n");
	return;
    } else {
	/* Extract the return type */
	while( type->typeOp == arrow_tp ) {
	    type = Get_Real_Type(type->typelist->next->type);
	}
	if( !can_be_sent_to_tcl(redex, type) ) {
	    fprintf(tcl_fp, "%d 0", rid);
	    Tcl_printf(tcl_fp, "%s\nIn fl callback function %s\n",
			       FailBuf, tcp->name);
	    fprintf(tcl_fp, "\n");
	    return;
	}
	fprintf(tcl_fp, "%d 1", rid);
	g_ptr2tcl(redex, type, tcl_fp);
	fprintf(tcl_fp, "\n");
	return;
    }
}


void
Mark_tcl_callbacks()
{
    ASSERT( tcl_initialized == TCL_MAGIC_NUMBER);
    tcl_callback_ptr tp;
    FOR_BUF(&tcl_callback_buf, tcl_callback_rec, tp) {
        Mark(tp->fun);
        SET_REFCNT(tp->fun, MAX_REF_CNT);
    }
}

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/

static bool
parse_int(string s, int *resp)
{
    int neg = 1;
    if( *s && *s == '-' ) {
        neg = -1;
        s++;
    }
    int i = 0;
    while( *s ) {
        switch( *s ) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                i = 10*i + (*s-'0');
                s++;
                break;
            default:
                return( FALSE );
        }
    }
    *resp = neg * i;
    return(TRUE);
}

static bool
parse_bool(string s, formula *resp)
{
    if( *s == '0' ) {
        *resp = B_Zero();
        return( TRUE );
    } else
    if( *s == '1' ) {
        *resp = B_One();
        return( TRUE );
    } else {
        return FALSE;
    }
}

static bool
get_end_of_item(string txt, string *endp)
{
    string s = txt;
    int curl_cnt = 0;
    while( *s ) {
        switch (*s) {
            case '\\':
                s++;
                if( *s ) s++;
                break;
            case '{':
                curl_cnt++;
                s++;
                break;
            case '}':
                curl_cnt--;
                s++;
		if( curl_cnt == 0 ) {
                    *endp = s;
                    return TRUE;
		}
		break;
            case ' ':
                if( curl_cnt > 0 ) {
                    s++;
                    break;
                } else {
                    *endp = s;
                    return TRUE;
                }
            default:
                s++;
                break;
        }
    }
    if( curl_cnt > 0 ) {
	return FALSE;
    }
    *endp = s;
    return TRUE;
}

static bool
tcl_to_g_ptr(string txt, typeExp_ptr type, g_ptr *resp)
{
    type = Get_Real_Type(type);
    // Trim of surrounding { } pair
    if( *txt == '{' ) {
	string s = txt;
	int curl_cnt = 0;
	bool need_trim = TRUE;
	while( *s ) {
	    switch( *s ) {
		case '{':
		    s++;
		    curl_cnt++;
		    break;
		case '}':
		    s++;
		    curl_cnt--;
		    if( curl_cnt == 0 && *s ) {
			need_trim = FALSE;
		    }
		    break;
		default:
		    s++;
		    break;
	    }
	}
	if( need_trim ) {
	    int len = strlen(txt);
	    *(txt+len-1) = 0;
	    txt++;
	}
    }
    switch( type->typeOp ) {
        case void_tp:
            {
                *resp = void_nd;
                return TRUE;
            }
        case string_tp:
            {
                string res = wastrsave(&strings, txt);
                *resp = Make_STRING_leaf(wastrsave(&strings, res));
                return TRUE;
            }
        case int_tp:
            {
                int res;
                if( !parse_int(txt, &res) ) { return FALSE; }
                *resp = Make_INT_leaf(res);
                return TRUE;
            }
        case bool_tp:
            {
                formula res;
                if( !parse_bool(txt, &res) ) { return FALSE; }
                *resp = Make_BOOL_leaf(res);
                return TRUE;
            }
        case list_tp:
            {
                string cur = txt;
                *resp = Make_NIL();
                g_ptr last = *resp;
                typeExp_ptr ltype = Get_Real_Type(type->typelist->type);
                while( *cur ) {
                    string end;
                    if( !get_end_of_item(cur, &end) ) { return FALSE; }
                    char tmp = *end;
                    *end = 0;
                    g_ptr element;
                    if( !tcl_to_g_ptr(cur, ltype, &element) ) return FALSE;
                    *end = tmp;
                    if( *end ) end++;
                    cur = end;
                    SET_CONS_HD(last,element);
                    SET_CONS_TL(last, Make_NIL());
                    last = GET_CONS_TL(last);
                }
                return TRUE;
            }
        case tuple_tp:
            {
                string cur = txt;
                typeExp_ptr ftype = Get_Real_Type(type->typelist->type);
                typeExp_ptr stype = Get_Real_Type(type->typelist->next->type);
                string end;
                if( !get_end_of_item(cur, &end) ) { return FALSE; }
                char tmp = *end;
                *end = 0;
                g_ptr fst;
                if( !tcl_to_g_ptr(cur, ftype, &fst) ) return FALSE;
                *end = tmp;
                if( *end == 0 ) { return FALSE; }
                cur = end+1;
                if( !get_end_of_item(cur, &end) ) { return FALSE; }
                tmp = *end;
                *end = 0;
                g_ptr snd;
                if( !tcl_to_g_ptr(cur, stype, &snd) ) return FALSE;
                *end = tmp;
                *resp = Make_CONS_ND(fst, snd);
                return TRUE;
            }
        default:
            DIE("Should never happen");
    }
}

static bool
can_be_sent_to_tcl(g_ptr np, typeExp_ptr type)
{
    type = Get_Real_Type(type);
    switch( type->typeOp ) {
        case string_tp: { return TRUE; }
        case void_tp: { return TRUE; }
        case int_tp: { return TRUE; }
        case bool_tp:
            {
                formula f = GET_BOOL(np);
                if( f == B_Zero() ) {
                    return TRUE;
                } else
                if( f == B_One() ) {
                    return TRUE;
                }
                Fail_pr("Cannot return a symbolic expression to tcl");
                return FALSE;
            }
        case list_tp:
            {
                g_ptr li, el;
                typeExp_ptr etype = Get_Real_Type(type->typelist->type);
                FOR_CONS(np, li, el) {
                    if( !can_be_sent_to_tcl(el, etype) ) { return FALSE; }
                }
                return TRUE;
            }
        case tuple_tp:
            {
                typeExp_ptr ftype = Get_Real_Type(type->typelist->type);
                if( !can_be_sent_to_tcl(GET_CONS_HD(np), ftype) ) {
                    return FALSE;
                }
                typeExp_ptr stype = Get_Real_Type(type->typelist->next->type);
                if( !can_be_sent_to_tcl(GET_CONS_TL(np), stype) ) {
                    return FALSE;
                }
                return TRUE;
            }
        default:
            FP(err_fp, "Unexpected type: ");
            Print_Type(type, err_fp, TRUE, TRUE);
            DIE("Should never happen");
    }
}

static void
g_ptr2tcl(g_ptr np, typeExp_ptr type, FILE *tcl_fp)
{
    type = Get_Real_Type(type);
    switch( type->typeOp ) {
        case string_tp:
            {
		Tcl_printf(tcl_fp, "{%s}", GET_STRING(np));
                return;
            }
        case void_tp:
	    {
		Tcl_printf(tcl_fp, "{}");
                return;
	    }
        case int_tp:
            {
		Tcl_printf(tcl_fp, "%s", Arbi_ToString(GET_AINT(np),10));
                return;
            }
        case bool_tp:
            {
                formula f = GET_BOOL(np);
                if( f == B_Zero() ) {
		    Tcl_printf(tcl_fp, "0");
                    return;
                } else
                if( f == B_One() ) {
		    Tcl_printf(tcl_fp, "1");
                    return;
                }
		DIE("Should never happen!");
            }
        case list_tp:
            {
                g_ptr li, el;
                bool first = TRUE;
		Tcl_printf(tcl_fp, "{");
                typeExp_ptr etype = Get_Real_Type(type->typelist->type);
                FOR_CONS(np, li, el) {
                    if( !first ) Tcl_printf(tcl_fp, " ");
                    first = FALSE;
		    
                    g_ptr2tcl(el, etype, tcl_fp);
                }
		Tcl_printf(tcl_fp, "}");
                return;
            }
        case tuple_tp:
            {
                typeExp_ptr ftype = Get_Real_Type(type->typelist->type);
		Tcl_printf(tcl_fp, "{");
                g_ptr2tcl(GET_CONS_HD(np), ftype, tcl_fp);
                typeExp_ptr stype = Get_Real_Type(type->typelist->next->type);
                Tcl_printf(tcl_fp, " ");
                g_ptr2tcl(GET_CONS_TL(np), stype, tcl_fp);
		Tcl_printf(tcl_fp, "}");
		return;
            }
        default:
            DIE("Should never happen");
    }
}

static bool
ok_tcl_callback_type_rec(typeExp_ptr type)
{
    type = Get_Real_Type(type);
    switch( type->typeOp ) {
        case string_tp:
        case int_tp:
        case void_tp:
        case bool_tp:
            return TRUE;
        case list_tp:
        case tuple_tp:
            for(typeList_ptr tlp = type->typelist;
                tlp != NULL; tlp = tlp->next) {
                if( !ok_tcl_callback_type_rec(tlp->type) )
                    return(FALSE);
            }
            return( TRUE );
        default:
            return FALSE;
    }
}

static bool
ok_tcl_callback_type(typeExp_ptr type)
{
    type = Get_Real_Type(type);
    while( type->typeOp == arrow_tp ) {
        typeExp_ptr arg_type = type->typelist->type;
        if( !ok_tcl_callback_type_rec(arg_type) ) { return FALSE; }
        type = Get_Real_Type(type->typelist->next->type);
    }   
    return( ok_tcl_callback_type_rec(type) );
}
