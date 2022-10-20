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

/* ------------- Global variables -------------- */

/********* Global variables referenced ***********/
extern value_type   current_type;
extern value_type   old_type;
extern int          write_graph_line_nbr;
extern int          read_graph_line_nbr;
extern int          dbg_indent;

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

/* ----- Forward definitions local functions ----- */
static void	write_hash_fun(pointer key, pointer data);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

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
    string s = Arbi_ToString(ai, 16);
    write_string(fp, s);
}

void
read_arbi_T(FILE *fp, arbi_T *aip)
{
    string s;
    RD_DBG1("arbi_T");
    read_string(fp, &s);
    *aip = Arbi_FromString(s,16);
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

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

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


/********************************************************/
/*  Machine generated template from  scripts/template	*/
/********************************************************/

#include "scripts/template"

