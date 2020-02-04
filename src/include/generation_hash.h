//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2018                        *
*                                                                   *
*********************************************************************/
/* generation_hash.h -- header for generation_hash.c */

#ifndef _GENERATION_HASH_H
#define _GENERATION_HASH_H
#include <strings.h>
#include "buf.h"
#include "hash.h"

#define GEN_HASH_MAGIC_NBR 59254829

typedef struct diff_rec	{
	hash_record	to_del;
	hash_record	to_add;
} diff_rec, *diff_ptr;


typedef struct gen_hash_rec {
	int	    initialized;
	hash_record main_tbl;
        unint	    (*hash_fn)(pointer key, unint n);
        bool	    (*hash_eq)(pointer k1, pointer k2);
	diff_ptr    cur_diff;
	buffer	    diff_rec_buf;
} gen_hash_record, *gen_hash_ptr;

/* Forward declarations */

void	create_gen_hash(gen_hash_ptr hp, unint size,
			unint (*hash_fn)(pointer key, unint n),
			bool (*hash_eq)(pointer k1, pointer k2));
void	dispose_gen_hash(gen_hash_ptr hp,
			 void (*dispose_fn)(pointer key, pointer data));
pointer	find_gen_hash(gen_hash_ptr hp, pointer key);
void	insert_gen_hash(gen_hash_ptr hp, pointer key, pointer data);
void	delete_gen_hash(gen_hash_ptr hp, pointer key);
void	begin_generation(gen_hash_ptr hp);
void	end_generation(gen_hash_ptr hp);

#endif /* _GENERATION_HASH_H */

