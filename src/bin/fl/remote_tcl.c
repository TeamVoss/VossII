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
extern str_mgr  *stringsp;
extern char     FailBuf[4096];
extern jmp_buf  *start_envp;
extern g_ptr    root_node;
extern g_ptr    *sp;
extern void PR(g_ptr np);
extern FILE     *to_tcl_fp;
extern bool     gui_mode;
extern g_ptr	void_nd;
extern buffer   ext_obj_buf;

/************************************************************************/
/*			Local Variables					*/
/************************************************************************/
static int	    tcl_initialized = 0;
static buffer	    tcl_callback_buf;
static tstr_ptr	    tmp_str_buf;
static hash_record  gptr2tclid;
static buffer	    tclids;

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/
static bool     parse_int(string s, int *resp);
static bool     parse_bool(string s, formula *resp);
static bool     get_end_of_item(string s, string *endp);
static bool     tcl_to_g_ptr(string txt, typeExp_ptr arg_type, g_ptr *resp);
static void     g_ptr2tcl(g_ptr np, typeExp_ptr type, FILE *tcl_fp);
static void	g_ptr2str4tcl(g_ptr np, typeExp_ptr type, tstr_ptr tmp_str_mgr);
static bool     ok_tcl_callback_type(typeExp_ptr type);
static unint	g_ptr_hash(pointer p, unint n);
static bool	g_ptr_equ(pointer p1, pointer p2);

/************************************************************************/
/*			Public Functions				*/
/************************************************************************/
void
Init_tcl()
{
    tcl_initialized = TCL_MAGIC_NUMBER;
    new_buf(&tcl_callback_buf, 100, sizeof(tcl_callback_rec));
    tmp_str_buf = new_temp_str_mgr();
    new_buf(&tclids, 100, sizeof(g_ptr));
    create_hash(&gptr2tclid, 100, g_ptr_hash, g_ptr_equ);
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

static void
call_tcl_fun(g_ptr redex)
{
    // Find the EXTAPI id and collect the arguments
    buffer args;
    new_buf(&args, 100, sizeof(g_ptr));
    g_ptr cur = redex;
    while( IS_APPLY(cur) ) {
	g_ptr arg = GET_APPLY_RIGHT(cur);
	push_buf(&args, &arg);
	cur = GET_APPLY_LEFT(cur);
    }
    //
    ASSERT( IS_LEAF(cur) && IS_PRIM_FN(cur) && IS_EXTAPI_FN(cur) );
    int id = GET_EXTAPI_FN(cur);
    string fun_name  = Get_ExtAPI_Function_Name(id);
    typeExp_ptr type = Get_ExtAPI_Type(id);
    //
    tstr_ptr tmp_str_mgr = new_temp_str_mgr();
    string cmd = gen_tprintf(tmp_str_mgr, "%s", fun_name);
    while( type->typeOp == arrow_tp ) {
	g_ptr arg;
	pop_buf(&args, &arg);
	typeExp_ptr arg_type = type->typelist->type;
	gen_tappend(tmp_str_mgr, " ");
	g_ptr2str4tcl(arg, arg_type, tmp_str_mgr);
	type = Get_Real_Type(type->typelist->next->type);
    }
    string str_res = NULL;
    if( !Send_to_tcl(cmd, &str_res) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Tcl function %s failed", fun_name));
	free_temp_str_mgr(tmp_str_mgr);
	free_buf(&args);
	free(str_res);
	return;
    }
    int len = strlen(str_res);
    if( *(str_res+len-1) == '\n' ) {
	*(str_res+len-1) = 0;
    }
    g_ptr g_res;
    if( !tcl_to_g_ptr(str_res, type, &g_res) ) {
	MAKE_REDEX_FAILURE(redex,
		Fail_pr("Return value for function %s failed", fun_name));
	free_temp_str_mgr(tmp_str_mgr);
	free_buf(&args);
	free(str_res);
	return;
    }
    OVERWRITE(redex, g_res);
    //
    free_temp_str_mgr(tmp_str_mgr);
    free_buf(&args);
    free(str_res);
    return;
}


bool
Import_tcl_function(string name, typeExp_ptr type)
{
    typeExp_ptr ctype = type;
    string strictness = strtemp("");
    while( ctype->typeOp == arrow_tp ) {
	charappend('1');
	ctype = Get_Real_Type(ctype->typelist->next->type);
    }
    Add_ExtAPI_Function(name, strictness, FALSE, type, call_tcl_fun);
    return TRUE;
}


void
Tcl_callback_eval(string cmd, int rid, FILE *tcl_fp)
{
    ASSERT( tcl_initialized == TCL_MAGIC_NUMBER);
    while( *cmd == ' ' ) cmd++;
    int fun_idx = get_callback_fun(cmd);
    if( fun_idx < 0 ) {
	Fail_pr("Not a valid callback function format (%s). WHAT????", cmd);
	fprintf(tcl_fp,"%d 0%s\n", rid, protect(FailBuf, NULL));
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
	    fprintf(tcl_fp,"%d 0%s\n", rid, protect(FailBuf, NULL));
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
	    fprintf(tcl_fp,"%d 0%s\n", rid, protect(FailBuf, NULL));
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
    g_ptr *gpp;
    FOR_BUF(&tclids, g_ptr, gpp) {
	Mark(*gpp);
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
                string res = wastrsave(stringsp, txt);
                *resp = Make_STRING_leaf(wastrsave(stringsp, res));
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
	    {
		if( Type_eq(type, Get_Type("float", NULL, TP_DONT_INSERT)) ) {
		    g_ptr res = Str2float(txt);
		    if( res == NULL ) { return FALSE; }
		    *resp = res;
		    return TRUE;
		}
		unint	obj_id;
		if( sscanf(txt, "_FlObJeCt_%d", &obj_id) != 1 ) {
		    Fail_pr("Not a valid object id (%s)", txt);
		    return FALSE;
		}
		if( obj_id > COUNT_BUF(&tclids) ) {
		    Fail_pr("Not a valid object id (%s)", txt);
		    return FALSE;
		}
		*resp = *((g_ptr *) M_LOCATE_BUF(&tclids, obj_id-1));
		return TRUE;
	    }
    }
}

static void
g_ptr2tcl(g_ptr np, typeExp_ptr type, FILE *tcl_fp)
{
    type = Get_Real_Type(type);
    switch( type->typeOp ) {
        case string_tp:
            {
		string s = GET_STRING(np);
		bool changed;
		string s2 = protect(s, &changed);
		if( changed )
		    Tcl_printf(tcl_fp, "{%s}", s2);
		else
		    Tcl_printf(tcl_fp, "%s", s2);
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
	    {
		int obj_id = PTR2UINT(find_hash(&gptr2tclid, np));
		if( obj_id != 0 ) {
		    Tcl_printf(tcl_fp, "_FlObJeCt_%06d", obj_id);
		    return;
		}
		push_buf(&tclids, &np);
		obj_id = COUNT_BUF(&tclids);
		insert_hash(&gptr2tclid, np, INT2PTR(obj_id));
		Tcl_printf(tcl_fp, "_FlObJeCt_%06d", obj_id);
		return;
	    }
    }
}

static void
g_ptr2str4tcl(g_ptr np, typeExp_ptr type, tstr_ptr tmp_str_mgr)
{
    type = Get_Real_Type(type);
    switch( type->typeOp ) {
        case string_tp:
            {
		string s = GET_STRING(np);
		bool changed;
		string s2 = protect(s, &changed);
		if( changed )
		    gen_tappend(tmp_str_mgr, "{%s}", s2);
		else
		    gen_tappend(tmp_str_mgr, "%s", s2);
                return;
            }
        case void_tp:
	    {
		gen_tappend(tmp_str_mgr, "{}");
                return;
	    }
        case int_tp:
            {
		gen_tappend(tmp_str_mgr, "%s", Arbi_ToString(GET_AINT(np),10));
                return;
            }
        case bool_tp:
            {
                formula f = GET_BOOL(np);
                if( f == B_Zero() ) {
		    gen_tappend(tmp_str_mgr, "0");
                    return;
                } else
                if( f == B_One() ) {
		    gen_tappend(tmp_str_mgr, "1");
                    return;
                }
		DIE("Should never happen!");
            }
        case list_tp:
            {
                g_ptr li, el;
                bool first = TRUE;
		gen_tappend(tmp_str_mgr, "{");
                typeExp_ptr etype = Get_Real_Type(type->typelist->type);
                FOR_CONS(np, li, el) {
                    if( !first ) gen_tappend(tmp_str_mgr, " ");
                    first = FALSE;
                    g_ptr2str4tcl(el, etype, tmp_str_mgr);
                }
		gen_tappend(tmp_str_mgr, "}");
                return;
            }
        case tuple_tp:
            {
                typeExp_ptr ftype = Get_Real_Type(type->typelist->type);
		gen_tappend(tmp_str_mgr, "{");
                g_ptr2str4tcl(GET_CONS_HD(np), ftype, tmp_str_mgr);
                typeExp_ptr stype = Get_Real_Type(type->typelist->next->type);
                gen_tappend(tmp_str_mgr, " ");
                g_ptr2str4tcl(GET_CONS_TL(np), stype, tmp_str_mgr);
		gen_tappend(tmp_str_mgr, "}");
		return;
            }
        default:
	    {
		int obj_id = PTR2UINT(find_hash(&gptr2tclid, np));
		if( obj_id != 0 ) {
		    gen_tappend(tmp_str_mgr, "_FlObJeCt_%06d", obj_id);
		    return;
		}
		push_buf(&tclids, &np);
		obj_id = COUNT_BUF(&tclids);
		insert_hash(&gptr2tclid, np, INT2PTR(obj_id));
		gen_tappend(tmp_str_mgr, "_FlObJeCt_%06d", obj_id);
		return;
	    }
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
	case arrow_tp:
            return FALSE;
        default:
            return TRUE;
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

static unint
g_ptr_hash(pointer p, unint n)
{
    if( n == 0 ) { return 13; }
    g_ptr nd = (g_ptr) p;
    if( nd == NULL ) return( 1 );
    switch( GET_TYPE(nd) ) {
        case CONS_ND:
            return( (931*g_ptr_hash(GET_CONS_HD(nd),n) +
                     137*g_ptr_hash(GET_CONS_TL(nd),n)) % n);
        case APPLY_ND:
            return( (931*(PTR2UINT(GET_APPLY_LEFT(nd))) +
                         (PTR2UINT(GET_APPLY_RIGHT(nd)))) % n );
        case LEAF:
            switch( GET_LEAF_TYPE(nd) ) {
                case INT:
                    return( Arbi_hash(GET_AINT(nd), n) );
                case STRING:
                    return( (PTR2UINT(GET_STRING(nd))) % n );
                case BOOL:
                    return( (((unsigned int) GET_BOOL(nd))) % n );
                case BEXPR:
                    return( (((unsigned long int) GET_BEXPR(nd))) % n );
                case PRIM_FN:
		    // Do not go into reference variables!!!
		    return( (((unsigned int) GET_PRIM_FN(nd))) % n );
                case EXT_OBJ:
                    return( (((unsigned long int)GET_EXT_OBJ(nd))) % n);
                default:
                    DIE("Unexpected cache argument value. Consult guru 3!");
            }
            break;
        default:
            DPR(nd);
            DIE("Unexpected cache argument value. GURU\n");
    }
}

// Almost the same as G_rec_equ except it does not go into reference variables.
static bool
g_ptr_equ(pointer p1, pointer p2)
{
    int         type, ltype;
    arbi_T      ai1, ai2;
    g_ptr n1 = (g_ptr) p1;
    g_ptr n2 = (g_ptr) p2;

  restart_g_rec_equ: 
    if( n1 == n2 )
        return( TRUE );
    if( n1 == NULL || n2 == NULL )
        return( FALSE );
    if( (type = GET_TYPE(n1)) != GET_TYPE(n2))
        return( FALSE );
    switch( type ) {
        case CONS_ND:
            if( !g_ptr_equ(GET_CONS_HD(n1), GET_CONS_HD(n2)) ) {return(FALSE);}
            n1 = GET_CONS_TL(n1);
            n2 = GET_CONS_TL(n2);
            goto restart_g_rec_equ;
        case APPLY_ND:
            return( n1 == n2 );
        case LEAF:
            if( (ltype = GET_LEAF_TYPE(n1)) != GET_LEAF_TYPE(n2) )
                return(FALSE);
            switch( ltype ) {
                case INT:
                    ai1 = GET_AINT(n1);
                    ai2 = GET_AINT(n2);
                    if( ai1 == ai2 )
                        return( TRUE );
                    if( Arbi_cmp(ai1, ai2) == arbi_EQ )
                        return( TRUE );
                    return( FALSE );
                case STRING:
                    return( STREQ(GET_STRING(n1), GET_STRING(n2)) );
                case BOOL:
                    return( B_Equal(GET_BOOL(n1), GET_BOOL(n2)) );
                case BEXPR:
                    return( BE_Equal(GET_BEXPR(n1), GET_BEXPR(n2)) );
                case PRIM_FN:
                    if( GET_PRIM_FN(n1) != GET_PRIM_FN(n2) ) { return FALSE; }
                    switch( GET_PRIM_FN(n1) ) {
                        case P_REF_VAR:
                            n1 = Get_RefVar(GET_REF_VAR(n1));
                            n2 = Get_RefVar(GET_REF_VAR(n2));
			    return( n1 == n2 );
                        case P_PRINTF:
                        case P_FPRINTF:
                        case P_SPRINTF:
                        case P_EPRINTF:
                        case P_SSCANF:
                            return(
                                GET_PRINTF_STRING(n1) == GET_PRINTF_STRING(n2)
                            );
                        case P_EXTAPI_FN:
                            return( GET_EXTAPI_FN(n1) == GET_EXTAPI_FN(n2) );
                        case P_FILEFP:
                            return(GET_FILE_IO_PTR(n1) == GET_FILE_IO_PTR(n2));
                        case P_FAIL:
                            return(
                                M_GET_FAIL_STRING(n1) == M_GET_FAIL_STRING(n2)
                            );
                        case P_DEBUG:
                            return(
                                M_GET_DEBUG_STRING(n1) == M_GET_DEBUG_STRING(n2)
                            );
                        default:
                            return( TRUE );
                    }
                case EXT_OBJ:
                    {
                        unint class = GET_EXT_OBJ_CLASS(n1);
                        ASSERT( class == GET_EXT_OBJ_CLASS(n2) );
                        ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
                        formula eq = op->eq_fn(GET_EXT_OBJ(n1),
                                               GET_EXT_OBJ(n2),
                                               TRUE);
                        if( eq == B_One() ) {
                            return TRUE;
                        } else {
                            return FALSE;
                        }
                    }
                default:
                    DIE("Should never happen!");
            }
            break;
        default:
            DIE("Should never happen!");
    }
}


