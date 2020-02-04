//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2018                        *
*                                                                   *
*********************************************************************/
#include "generation_hash.h"

static hash_record *mtbl;

static void	del_fun(pointer key, pointer data);
static void	add_fun(pointer key, pointer data);

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

void
begin_generation(gen_hash_ptr hp)
{
    ASSERT( hp->initialized == GEN_HASH_MAGIC_NBR );
    diff_rec dr;
    create_hash(&(dr.to_del), 100, hp->hash_fn, hp->hash_eq);
    create_hash(&(dr.to_add), 100, hp->hash_fn, hp->hash_eq);
    push_buf(&(hp->diff_rec_buf), &dr);
    buffer *dbp = &(hp->diff_rec_buf);
    hp->cur_diff = (diff_ptr) M_LOCATE_BUF(dbp, COUNT_BUF(dbp)-1);
}

void
create_gen_hash(gen_hash_ptr hp, unint size,
		unint (*hash_fn)(pointer key, unint n),
		bool (*hash_eq)(pointer k1, pointer k2))
{
    ASSERT( hp->initialized != GEN_HASH_MAGIC_NBR );
    create_hash(&(hp->main_tbl), size, hash_fn, hash_eq);
    hp->hash_fn = hash_fn;
    hp->hash_eq = hash_eq;
    new_buf(&(hp->diff_rec_buf), 100, sizeof(diff_rec));
    hp->initialized = GEN_HASH_MAGIC_NBR;
    begin_generation(hp);
}

void
dispose_gen_hash(gen_hash_ptr hp,
		 void (*dispose_fn)(pointer key, pointer data))
{
    ASSERT( hp->initialized == GEN_HASH_MAGIC_NBR );
    dispose_hash(&(hp->main_tbl), dispose_fn);
    diff_ptr	dp;
    FOR_BUF(&(hp->diff_rec_buf), diff_rec, dp) {
	dispose_hash(&(dp->to_del), NULLFCN);
	dispose_hash(&(dp->to_add), NULLFCN);
    }
    free_buf(&(hp->diff_rec_buf));
    hp->initialized = 0;
}

pointer
find_gen_hash(gen_hash_ptr hp, pointer key)
{
    ASSERT( hp->initialized == GEN_HASH_MAGIC_NBR );
    return( find_hash(&(hp->main_tbl), key) );
}

void
insert_gen_hash(gen_hash_ptr hp, pointer key, pointer data)
{
    ASSERT( hp->initialized == GEN_HASH_MAGIC_NBR );
    hash_record *maint = &(hp->main_tbl);
    hash_record *to_del = &(hp->cur_diff->to_del);
    hash_record *to_add = &(hp->cur_diff->to_add);
    bool in_del_tbl = (find_hash(to_del, key) != NULL);
    bool in_add_tbl = (find_hash(to_add, key) != NULL);
    bool in_main_tbl = (find_hash(maint, key) != NULL);
    if( in_del_tbl || in_add_tbl ) {
	// Already recorded initial status of key
	insert_hash(maint, key, data);
    } else {
	// Must record the current status of key
	insert_hash(to_del, key, key);
	if( in_main_tbl ) {
	    insert_hash(to_add, key, find_hash(maint, key));
	}
	insert_hash(maint, key, data);
    }
}

void
delete_gen_hash(gen_hash_ptr hp, pointer key)
{
    ASSERT( hp->initialized == GEN_HASH_MAGIC_NBR );
    hash_record *maint = &(hp->main_tbl);
    hash_record *to_del = &(hp->cur_diff->to_del);
    hash_record *to_add = &(hp->cur_diff->to_add);
    bool in_del_tbl = (find_hash(to_del, key) != NULL);
    bool in_add_tbl = (find_hash(to_add, key) != NULL);
    bool in_main_tbl = (find_hash(maint, key) != NULL);
    if( in_del_tbl || in_add_tbl ) {
	// Already recorded initial status of key
	delete_hash(maint, key);
    } else {
	// Must record the current status of key
	if( in_main_tbl ) {
	    insert_hash(to_add, key, find_hash(maint, key));
	}
	delete_hash(maint, key);
    }
}


void
end_generation(gen_hash_ptr hp)
{
    ASSERT( hp->initialized == GEN_HASH_MAGIC_NBR );
    hash_record *maint = &(hp->main_tbl);
    hash_record *to_del = &(hp->cur_diff->to_del);
    hash_record *to_add = &(hp->cur_diff->to_add);
    // Restore the values in the main_tbl to at the start of the generation
    mtbl = maint;
    scan_hash(to_del, del_fun);
    scan_hash(to_add, add_fun);
    // Free diff buffers
    dispose_hash(to_del, NULLFCN);
    dispose_hash(to_add, NULLFCN);
    buffer *dbp = &(hp->diff_rec_buf);
    pop_buf(dbp, NULL);
    hp->cur_diff = (diff_ptr) M_LOCATE_BUF(dbp, COUNT_BUF(dbp)-1);
}

/************************************************************************/
/*                      Private Functions                               */
/************************************************************************/

static void
del_fun(pointer key, pointer data)
{
    (void) data;
    delete_hash(mtbl, key);
}

static void
add_fun(pointer key, pointer data)
{
    insert_hash(mtbl, key, data);
}
