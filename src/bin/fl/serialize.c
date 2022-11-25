//-------------------------------------------------------------------
// Copyright 2022 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2022			*/
/*									*/
/************************************************************************/
#include "strings.h"
#include "fsm.h"
#include "table.h"
#include "serialize.h"
#include "graph.h"

#if 0
#define WR_DBG0(type)   { fprintf(stderr, "%*s%s on line %d\n",     \
			  2*(dbg_indent++),"",(type), write_graph_line_nbr); }
#define RD_DBG0(type)   { fprintf(stderr, "%*s%s on line %d\n", \
			  2*(dbg_indent++),"",(type), read_graph_line_nbr); }
#define WR_DBG1(type)   { fprintf(stderr, "%*s%s on line %d\n",     \
			      2*dbg_indent,"",(type), write_graph_line_nbr++); }
#define RD_DBG1(type)   { fprintf(stderr, "%*s%s on line %d\n", \
                              2*dbg_indent,"",(type), read_graph_line_nbr++); }
#define END_DBG(type)   { \
		    fprintf(stderr,"%*s%s\n",2*(--dbg_indent),"",(type)); }

#else
#define WR_DBG0(type)	
#define RD_DBG0(type)	
#define WR_DBG1(type)	
#define RD_DBG1(type)	
#define END_DBG(type)  
#endif

typedef formula pformula;

/* ------------- Global variables -------------- */

/********* Global variables referenced ***********/
extern value_type   current_type;
extern value_type   old_type;
extern int          write_graph_line_nbr;
extern int          read_graph_line_nbr;
extern int          dbg_indent;
extern buffer	    ext_obj_buf;
extern str_mgr      strings;

/***** PRIVATE VARIABLES *****/
static bool unserialize_in_process = FALSE;
static bool serialize_in_process   = FALSE;
static hash_record      objs_saved;	// For writing pointers
static hash_record	pointer_map;    // For reading pointers
static FILE *cur_fp;
static void (*cur_write_hash_key)(FILE *fp, pointer key);
static void (*cur_write_hash_data)(FILE *fp, pointer data);

/* Forward declaration of local functions */
static void		write_ustr_mgr_string(pointer key, pointer data);
static void		write_hash_fun(pointer key, pointer data);
static void		write_g_rec(FILE *fp, g_ptr np);
static void		read_g_rec(FILE *fp, g_ptr np);

/* ----- Forward definitions local functions ----- */
static void	write_hash_fun(pointer key, pointer data);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

unint
pint_hash(pointer p, unint n)
{
    return( ((long unsigned int) p) % n );
}

bool
pint_equ(pointer p1, pointer p2)
{
    return( ((long unsigned int) p1) == ((long unsigned int) p2) );
}


void
Serialize_Begin()
{
    create_hash(&objs_saved, 1000, ptr_hash, ptr_equ);
    serialize_in_process = TRUE;
}

void
Serialize_End()
{
    dispose_hash(&objs_saved, NULLFCN);
    serialize_in_process = FALSE;
}

void
Unserialize_Begin()
{
    create_hash(&pointer_map, 1000, ptr_hash, ptr_equ);
    unserialize_in_process = TRUE;
}

void
Unserialize_End()
{
    dispose_hash(&pointer_map, NULLFCN);
    unserialize_in_process = FALSE;
}

void
Insert_pointer_map(pointer old_ptr, pointer new_ptr)
{
    pointer p = find_hash(&pointer_map, old_ptr);
    if( p == NULL )
	insert_hash(&pointer_map, old_ptr, new_ptr);
    else if( p != new_ptr ) {
	Rprintf("Corrupted file. Pointer mismatch");
    }
}

pointer
Old2new(pointer old)
{
    if( old == NULL ) return NULL;
    return( find_hash(&pointer_map, old) );
}


// ------------------------------------------------------------------
// Code for (un-)serialize primitive types
// ------------------------------------------------------------------

bool
write_pointer(FILE *fp, pointer p)
{
    if( p == NULL ) {
	WR_DBG1("NULL pointer");
	fprintf(fp, "0x0\n");
	return FALSE;
    }
    WR_DBG1("pointer");
    fprintf(fp, "%p\n", p);
    if( find_hash(&objs_saved, p) != NULL ) { return FALSE; }
    insert_hash(&objs_saved, p, INT2PTR(1));
    return TRUE;
}

void
read_pointer(FILE *fp, pointer *pp)
{
    pointer p;
    RD_DBG1("pointer");
    if( fscanf(fp, "%p\n", &p) != 1 )
	Rprintf("Corrupted pointer object");
    *pp = p;
}

void
write_string(FILE *fp, string s)
{
    ASSERT(s != NULL);
    WR_DBG1("string");
    fprintf(fp, "%d\n", Save_get_string_idx(s));
}

void
read_string(FILE *fp, string *sp)
{
    int idx;
    RD_DBG1("string");
    if( fscanf(fp, "%d\n", &idx) != 1 )
	Rprintf("Corrupted string object");
    *sp = Load_get_string_from_idx(idx);
}

void
write_int(FILE *fp, int i)
{
    WR_DBG1("int");
    fprintf(fp, "%d\n", i);
}

void
read_int(FILE *fp, int *ip)
{
    int i;
    RD_DBG1("int");
    if( fscanf(fp, "%d\n", &i) != 1 )
	Rprintf("Corrupted int object");
    *ip = i;
}

void
write_pint(FILE *fp, void *pi)
{
    int i = PTR2INT(pi);
    WR_DBG1("int");
    fprintf(fp, "%d\n", i);
}

void
read_pint(FILE *fp, int *ip)
{
    int i;
    RD_DBG1("int");
    if( fscanf(fp, "%d\n", &i) != 1 )
	Rprintf("Corrupted int object");
    *ip = i;
}

void
write_unint(FILE *fp, unint i)
{
    WR_DBG1("unint");
    fprintf(fp, "%d\n", i);
}

void
read_unint(FILE *fp, unint *ip)
{
    unint i;
    RD_DBG1("unint");
    if( fscanf(fp, "%d\n", &i) != 1 )
	Rprintf("Corrupted unint object");
    *ip = i;
}

void
write_uchar(FILE *fp, uchar i)
{
    WR_DBG1("uchar");
    fprintf(fp, "%c\n", i);
}

void
read_uchar(FILE *fp, uchar *ip)
{
    uchar i;
    RD_DBG1("uchar");
    if( fscanf(fp, "%c\n", &i) != 1 )
	Rprintf("Corrupted uchar object");
    *ip = i;
}

void
write_arbi_T(FILE *fp, arbi_T ai)
{
    WR_DBG1("arbi_T");
    string s = Arbi_ToString(ai, 10);
    write_string(fp, s);
}

void
read_arbi_T(FILE *fp, arbi_T *aip)
{
    string s;
    RD_DBG1("arbi_T");
    read_string(fp, &s);
    *aip = Arbi_FromString(s, 10);
    ASSERT(*aip != NULL );
}

void
write_formula(FILE *fp, formula f)
{
    WR_DBG1("formula");
    int i = Save_get_bool_idx(f);
    write_int(fp, i);
}

void
read_formula(FILE *fp, formula *pf)
{
    int idx;
    RD_DBG1("formula");
    read_int(fp, &idx);
    *pf = Load_get_bool_from_idx(idx);
}

void
write_pformula(FILE *fp, void *pf)
{
    formula f = PTR2INT(pf);
    WR_DBG1("pformula");
    int i = Save_get_bool_idx(f);
    write_int(fp, i);
}

void
read_pformula(FILE *fp, formula *pf)
{
    int idx;
    RD_DBG1("pformula");
    read_int(fp, &idx);
    *pf = Load_get_bool_from_idx(idx);
}

void
write_bexpr(FILE *fp, bexpr f)
{
    WR_DBG1("bexpr");
    int i = Save_get_bexpr_idx(f);
    write_int(fp, i);
}

void
read_bexpr(FILE *fp, bexpr *pf)
{
    int idx;
    RD_DBG1("bexpr");
    read_int(fp, &idx);
    *pf = Load_get_bexpr_from_idx(idx);
}

// ------------------------------------------------------------------
// Code for (un-)serialize a record manager
// ------------------------------------------------------------------

void
write_mgr(FILE *fp, rec_mgr_ptr mp, void (*write_rec)(FILE *fp, pointer p))
{
    WR_DBG0("{ mgr");
    int cnt = MGR_SIZE(mp);
    write_int(fp, cnt);
    pointer p;
    FOR_REC(mp, pointer, p) {
	if( write_pointer(fp, p) ) {
	    write_rec(fp, p);
	}
    }
    END_DBG("} mgr");
}

void
read_mgr(FILE *fp, rec_mgr_ptr mp, void (*read_rec)(FILE *fp, pointer p))
{
    ASSERT(unserialize_in_process);
    int cnt;
    RD_DBG0("{ mgr");
    read_int(fp, &cnt);
    for(int i = 0; i < cnt; i++) {
	pointer op;
	read_pointer(fp, &op);
	ASSERT( op != NULL );
	if( Old2new(op) == NULL ) {
	    pointer np = new_rec(mp);
	    Insert_pointer_map((pointer) op, (pointer) np);
	    read_rec(fp, np);
	}
    }
    END_DBG("} mgr");
}


// ------------------------------------------------------------------
// Code for (un-)serialize a buffer
// ------------------------------------------------------------------

void
write_buf(FILE *fp, buffer_ptr bp, void (*write_item)(FILE *fp, pointer p))
{
    WR_DBG0("{W buf");
    int cnt = COUNT_BUF(bp);
    write_int(fp, cnt);
    for(int i = 0; i < cnt; i++) {
	pointer p = FAST_LOC_BUF(bp, i);
        write_item(fp, p);
    }
    END_DBG("} buf");
}

void
read_buf(FILE *fp, buffer_ptr bp,
		void (*read_item)(FILE *fp, pointer p))
{
    ASSERT(unserialize_in_process);
    int cnt;
    RD_DBG0("{R buf");
    read_int(fp, &cnt);
    resize_buf(bp, cnt);
    for(int i = 0; i < cnt; i++) {
	pointer p = FAST_LOC_BUF(bp, i);
	read_item(fp, p);
    }
    END_DBG("} buf");
}

// ------------------------------------------------------------------
// Code for (un-)serialize a hash table
// ------------------------------------------------------------------
void
write_hash_tbl(FILE *fp, hash_record_ptr hp,
		   void (*write_hash_key)(FILE *fp, pointer key),
		   void (*write_hash_data)(FILE *fp, pointer data))
{
    WR_DBG0("{W hash");
    write_int(fp, hash_size(hp));
    cur_fp = fp;
    cur_write_hash_key = write_hash_key;
    cur_write_hash_data = write_hash_data;
    scan_hash(hp, write_hash_fun);
    END_DBG("} hash");
}

void
read_hash_tbl( FILE *fp,  hash_record_ptr hp, 
			void (*read_hash_key)(FILE *fp, pointer *pp),
			void (*read_hash_data)(FILE *fp, pointer *pp))
{
    int cnt;
    RD_DBG0("{R hash");
    read_int(fp, &cnt);
    for(int i = 0; i < cnt; i++) {
	pointer key;
	read_hash_key(fp, &key);
	pointer data;
	read_hash_data(fp, &data);
	insert_hash(hp, key, data);
    }
    END_DBG("} hash");
}

// ------------------------------------------------------------------
// Code for (un-)serialize a ustr_mgr
// ------------------------------------------------------------------

void
write_ustr_mgr(FILE *fp, ustr_mgr *usmp)
{
    WR_DBG1("ustr_mgr");
    write_int(fp, ustr_mgr_size(usmp));
    cur_fp = fp;
    scan_hash(&(usmp->utbl), write_ustr_mgr_string);
}

void
read_ustr_mgr(FILE *fp,  ustr_mgr *usmp)
{
    int cnt;
    RD_DBG1("ustr_mgr");
    read_int(fp, &cnt);
    for(int i = 0; i < cnt; i++) {
	string s;
	read_string(fp, &s);
	uStrsave(usmp, s);
    }
}

// ------------------------------------------------------------------
// Code for (un-)serialize g_ptr (and g_rec)

void
write_g_ptr(FILE *fp, g_ptr p)
{
    WR_DBG1("{ g_ptr");
    if( write_pointer(fp, (pointer) p) ) {
        write_g_rec(fp, p);
    }
    END_DBG("} g_ptr");
}

void
read_g_ptr(FILE *fp, g_ptr *pp)
{
    pointer oldp, newp;
    RD_DBG1("{ g_ptr");
    read_pointer(fp, &oldp);
    if( oldp == NULL ) {
        *pp = NULL;
        END_DBG("} g_ptr");
        return;
    }
    if( (newp = Old2new(oldp)) != NULL ) {
        *pp = (g_ptr) newp;
    } else {
        newp = (pointer) Get_node();
        Insert_pointer_map(oldp, newp);
        read_g_rec(fp, newp);
        *pp = (g_ptr) newp;
    }
    END_DBG("} g_ptr");
}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

#define TAG_APPLY       'A'
#define TAG_CONS        'C'
#define TAG_NIL         '0'
#define TAG_INT         'I'
#define TAG_STRING      'S'
#define TAG_BOOL        'B'
#define TAG_BEXPR       'E'
#define TAG_FAIL        'F'
#define TAG_REF_VAR     'R'
#define TAG_PRINTF      '!'
#define TAG_EPRINTF     '@'
#define TAG_FPRINTF     '#'
#define TAG_SPRINTF     '$'
#define TAG_SSCANF      '<'
#define TAG_CACHE       '='
#define TAG_EXTAPI      'X'
#define TAG_PRIM_FN     'P'
#define TAG_VAR         'V'
#define TAG_EXTOBJ      'O'
#define TAG_DEBUG	'D'
#define TAG_FILEFP	'f'

#define EMIT(tag)	    fprintf(fp, "%c\n", (tag));

static void
write_g_rec(FILE *fp, g_ptr np)
{
    WR_DBG1("g_rec node");
    switch( GET_TYPE(np) ) {
	case APPLY_ND:
	    {
		EMIT(TAG_APPLY);
		write_g_ptr(fp, GET_APPLY_LEFT(np));
		write_g_ptr(fp, GET_APPLY_RIGHT(np));
		return;
	    }
	case CONS_ND:
	    {
		if( IS_NIL(np) ) {
		    EMIT(TAG_NIL);
		    return;
		} else {
		    EMIT(TAG_CONS);
		    write_g_ptr(fp, GET_CONS_HD(np));
		    write_g_ptr(fp, GET_CONS_TL(np));
		    return;
		}
	    }
	case LEAF:
	    {
		switch( GET_LEAF_TYPE(np) ) {
		    case INT:
			EMIT(TAG_INT);
			write_arbi_T(fp, GET_AINT(np));
			return;
		    case STRING:
			EMIT(TAG_STRING);
			write_string(fp, GET_STRING(np));
			return;
		    case BOOL:
			EMIT(TAG_BOOL);
			write_formula(fp, GET_BOOL(np));
			return;
		    case BEXPR:
			EMIT(TAG_BEXPR);
			write_bexpr(fp, GET_BEXPR(np));
			return;
		    case EXT_OBJ: {
			EMIT(TAG_EXTOBJ);
			int i = GET_EXT_OBJ_CLASS(np);
			write_int(fp, i);
			ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, i);
			ASSERT(op->save_fn != NULL );
			op->save_fn(fp, GET_EXT_OBJ(np));
			return;
		    }
		    case PRIM_FN:
			switch( GET_PRIM_FN(np) ) {
			    case P_FAIL:
				EMIT(TAG_FAIL);
				write_string(fp, GET_FAIL_STRING(np));
				return;
			    case P_DEBUG:
				{
				    EMIT(TAG_DEBUG);
				    write_string(fp, GET_DEBUG_STRING(np));
				    return;
				}
			    case P_FILEFP:
				{
				    EMIT(TAG_FILEFP);
				    io_ptr ip = GET_FILE_IO_PTR(np);
				    write_string(fp, Get_StreamName(ip));
				    return;
				}
			    case P_REF_VAR:
				{
				    EMIT(TAG_REF_VAR);
				    int ref_var = GET_REF_VAR(np);
				    g_ptr root = Get_RefVar(ref_var);
				    write_g_ptr(fp, root);
				    return;
				}
			    case P_SSCANF:
				EMIT(TAG_SSCANF);
				write_string(fp, GET_PRINTF_STRING(np));
				return;
			    case P_PRINTF:
				EMIT(TAG_PRINTF);
				write_string(fp, GET_PRINTF_STRING(np));
				return;
			    case P_FPRINTF:
				EMIT(TAG_FPRINTF);
				write_string(fp, GET_PRINTF_STRING(np));
				return;
			    case P_SPRINTF:
				EMIT(TAG_SPRINTF);
				write_string(fp, GET_PRINTF_STRING(np));
				return;
			    case P_EPRINTF:
				EMIT(TAG_EPRINTF);
				write_string(fp, GET_PRINTF_STRING(np));
				return;
			    case P_CACHE:
				EMIT(TAG_CACHE);
				/* Should we save the content of the cache? */
				return;
			    case P_EXTAPI_FN:
				EMIT(TAG_EXTAPI);
				write_int(fp, GET_EXTAPI_FN(np));
				return;
			    default:
				EMIT(TAG_PRIM_FN);
				write_int(fp, GET_PRIM_FN(np));
				return;
			}
		    case VAR:
			EMIT(TAG_VAR);
			write_string(fp, GET_VAR(np));
			return;
		    default:
			DIE("Unexpected node type");
		}
	    }
	default:
	    DIE("Unexpected node type");
    }
}

static void
read_g_rec(FILE *fp, g_ptr np)
{
    RD_DBG1("g_rec node");
    int type = fgetc(fp);
    fgetc(fp);	// Eat newline
    switch( type ) {
	case TAG_APPLY: {
	    g_ptr l, r;
	    read_g_ptr(fp, &l);
	    read_g_ptr(fp, &r);
	    INC_REFCNT(l);
	    INC_REFCNT(r);
	    MAKE_REDEX_APPL_ND(np,l,r);
	    return;
	}
	case TAG_NIL:
	    MAKE_REDEX_NIL(np);
	    return;
	case TAG_CONS: {
	    g_ptr h, t;
	    read_g_ptr(fp, &h);
	    read_g_ptr(fp, &t);
	    INC_REFCNT(h);
	    INC_REFCNT(t);
	    MAKE_REDEX_CONS_ND(np,h,t);
	    return;
	}
	case TAG_INT: {
	    arbi_T ai;
	    read_arbi_T(fp, &ai);
	    MAKE_REDEX_AINT(np,ai);
	    return;
	}
	case TAG_STRING: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_STRING(np, s);
	    return;
	}
	case TAG_BOOL: {
	    formula f;
	    read_formula(fp, &f);
	    MAKE_REDEX_BOOL(np, f);
	    return;
	}
	case TAG_BEXPR: {
	    bexpr f;
	    read_bexpr(fp, &f);
	    MAKE_REDEX_BEXPR(np, f);
	    return;
	}
	case TAG_EXTOBJ: {
	    int class;
	    read_int(fp, &class);
	    ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
	    ASSERT(op->load_fn != NULL );
	    pointer p = op->load_fn(fp);
	    MAKE_REDEX_EXT_OBJ(np, class, p);
	    return;
	}
	case TAG_FAIL: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_FAILURE(np, s);
	    return;
	}
	case TAG_REF_VAR: {
	    g_ptr c;
	    read_g_ptr(fp, &c);
	    int ref_var = Make_RefVar();
	    Set_RefVar(ref_var, c);
	    INC_REFCNT(c);
	    MAKE_REDEX_PRIM_FN(np, P_REF_VAR);
	    SET_REF_VAR(np, ref_var);
	    return;
	}
	case TAG_SSCANF: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_PRIM_FN(np, P_SSCANF);
	    SET_PRINTF_STRING(np, s);
	    return;
	}
	case TAG_PRINTF: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_PRIM_FN(np, P_PRINTF);
	    SET_PRINTF_STRING(np, s);
	    return;
	}
	case TAG_FPRINTF: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_PRIM_FN(np, P_FPRINTF);
	    SET_PRINTF_STRING(np, s);
	    return;
	}
	case TAG_SPRINTF: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_PRIM_FN(np, P_SPRINTF);
	    SET_PRINTF_STRING(np, s);
	    return;
	}
	case TAG_EPRINTF: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_PRIM_FN(np, P_EPRINTF);
	    SET_PRINTF_STRING(np, s);
	    return;
	}
	case TAG_CACHE: {
	    MAKE_REDEX_PRIM_FN(np, P_CACHE);
	    int ci = Make_g_cache();
	    SET_CACHE_TBL(np, ci);
	    return;
	}
	case TAG_EXTAPI: {
	    int op;
	    read_int(fp, &op);
	    MAKE_REDEX_EXTAPI(np, op);
	    return;
	}
	case TAG_PRIM_FN: {
	    int pfn;
	    read_int(fp, &pfn);
	    MAKE_REDEX_PRIM_FN(np, pfn);
	    return;
	}
	case TAG_DEBUG: {
	    string s;
	    read_string(fp, &s);
	    MAKE_REDEX_PRIM_FN(np, P_DEBUG);
	    SET_DEBUG_STRING(np, s);
	    return;
	}
	case TAG_FILEFP:
	{
	    string name;
	    read_string(fp, &name);
	    MAKE_REDEX_PRIM_FN(np, P_FILEFP);
	    io_ptr ip = Get_OpenStream(name);
	    ASSERT(ip != NULL);
	    SET_FILE_IO_PTR(np, ip);
	    return;
	}
	default:
	    DIE("Unexpected node type");
    }
}

static void
write_ustr_mgr_string(pointer key, pointer data)
{
    string s = (string) key;
    (void) data;
    write_string(cur_fp, s);
}

static void
write_hash_fun(pointer key, pointer data)
{
    cur_write_hash_key(cur_fp, key);
    cur_write_hash_data(cur_fp, data);
}

static unint
g_ptr_hash(pointer p, unint n)
{
    return( G_rec_hash(p, n) );
}

static bool
g_ptr_equ(pointer p1, pointer p2)
{
    return( G_rec_equ(p1, p2) );
}




/********************************************************/
/*  Machine generated template from  scripts/template	*/
/********************************************************/

#include "scripts/template"

