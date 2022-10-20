//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1993                        *
*                                                                   *
*********************************************************************/
/* hash.h -- header for hash.c */

#ifndef _HASH_H
#define _HASH_H
#include <string.h>
#include "types.h"
#include "alloc.h"
#include "buf.h"
#include "rec.h"

#define HASH_MAGIC_NBR	5524491
#define NULLFCN		((void(*)())NULL)

typedef struct bucket_rec *bucket_ptr;
typedef struct bucket_rec {
	pointer		key;
	pointer		data;
	bucket_ptr	next;
} bucket_rec;


typedef struct hash_record_q {
	int	initialized;	/* Helpful for debugging		  */
	bool	autosize;	/* Should the table resize automatically? */
	unint	tbl_size;	/* Size of bucket table			  */
	unint	elements;	/* Nbr of elements in table		  */
	buffer	table;		/* Bucket table 			  */
	rec_mgr	bucket_mgr;	/* Record manager for buckets		  */
				/* User supplied hash & eqality functions */
	unint	(*hash_fn)(pointer key, unint n);
	bool	(*hash_eq)(pointer k1, pointer k2);
} hash_record, *hash_record_ptr;


void		create_hash(hash_record_ptr hp, unint size,
			    unint (*hash_fn)(pointer key, unint n),
			    bool (*hash_eq)(pointer k1, pointer k2));
void		duplicate_hash_struct(hash_record_ptr old_hp,
				      hash_record_ptr new_hp);
void		autosize_hash(hash_record_ptr hp);
void		freeze_size_hash(hash_record_ptr hp);
void		dispose_hash(hash_record_ptr hp,
			     void (*dispose_fn)(pointer key, pointer data));
unint		str_hash(pointer key, unint n);
bool		str_equ(pointer k1, pointer k2);
unint		ptr_hash(pointer key, unint n);
bool		ptr_equ(pointer k1, pointer k2);
unint		int_hash(pointer key, unint n);
bool		int_equ(pointer k1, pointer k2);
pointer		find_hash(hash_record_ptr hp, pointer key);
void		insert_hash(hash_record_ptr hp, pointer key, pointer data);
void		delete_hash(hash_record_ptr hp, pointer key);
void		scan_hash(hash_record_ptr hp,
			  void (*scan_fn)(pointer key, pointer data));
int		hash_size(hash_record_ptr hp);
bool		insert_check_hash(hash_record_ptr hp,pointer key,pointer data);

#endif /* _HASH_H */

