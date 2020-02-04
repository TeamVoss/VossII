//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 1990			*/
/*									*/
/************************************************************************/
#include "symbol_tbl.h"

/********* External values declared ***********/
int		cur_start;

/********* External values referenced ***********/
extern bool     RCprint_aliases_flag;

/***** PRIVATE VARIABLES *****/
static int		initialized = -1;
static hash_record	string_tbl;

/* ----------- Forward definitions of local functions -------- */

/****************************************************************/
/*     			Public functions			*/
/****************************************************************/
/* Init_Nd_symb_tbl -- Initialize symbol table */
void
Init_symb_tbl()
{
    create_hash(&string_tbl, 10000, str_hash, str_equ);
    initialized = 1;
}

/* Cached and word aligned string save */
string
wastrsave(str_mgr_ptr smp, string s)
{
    string ret;
    ASSERT( initialized == 1 );
    if( (ret = (string) find_hash(&string_tbl, (pointer) s)) != NULL )
	return( ret );
    ret = WAstrsave(smp, s);
    insert_hash(&string_tbl, (pointer) ret, (pointer) ret);
    return( ret );
}
