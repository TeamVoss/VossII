//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "hash.h"

/************************************************************************/
/*                      Private Variables                               */
/************************************************************************/

static unint	primes[] = {
                            13,
                            23,
                            59,
                            113,
                            241,
                            503,
                            1019,
                            2039,
                            4091,
                            8179,
                            16369,
                            32749,
                            65521,
                            131063,
                            262139,
                            524269,
                            1048571,
                            2097143,
                            4194287,
                            8388593,
                            16777199,
                            33554393,
                            67108859,
                            134217689,
                            268435399,
                            536870879,
                            1073741789,
                            2147483629,
                            0};


/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

void
create_hash(hash_record_ptr hp, unint size,
	    unint (*hash_fn)(pointer key, unint n),
	    bool (*hash_eq)(pointer k1, pointer k2))
{
    unint i;

    hp->autosize = TRUE;
    i = 0;
    while( primes[i] < size )
	i++;
    size = primes[i];
    hp->tbl_size = size;
    hp->elements = 0;
    new_buf(&(hp->table), size, sizeof(bucket_ptr));
    for(i = 0; i < size; i++) {
	pointer dummy = 0;
	push_buf(&(hp->table), (pointer) &dummy);
    }
    new_mgr(&(hp->bucket_mgr), sizeof(bucket_rec));
    hp->hash_fn = hash_fn;
    hp->hash_eq = hash_eq;
    hp->initialized = HASH_MAGIC_NBR;
}

void
autosize_hash(hash_record_ptr hp)
{
    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    hp->autosize = TRUE;
}

void
freeze_size_hash(hash_record_ptr hp)
{
    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    hp->autosize = FALSE;
}

void
dispose_hash(hash_record_ptr hp, void (*dispose_fn)(pointer key, pointer data) )
{
    bucket_ptr	*curp, chp, tmp;

    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    if( dispose_fn != NULLFCN ) {
	FOR_BUF(&(hp->table), bucket_ptr, curp) {
	    chp = *curp;
	    while( chp != NULL ) {
		tmp = chp;
		chp = chp->next;
		(*dispose_fn)(tmp->key, tmp->data);
		free_rec(&(hp->bucket_mgr), (pointer) tmp);
	    }
	}
    }
    free_mgr(&(hp->bucket_mgr));
    free_buf(&(hp->table));
    hp->initialized = 0;
}

#if 0
#define MAGIC_CONSTANT	997
unint
str_hash(pointer key, unint n)
{
    int		res = 0;
    string	s;

    s = (string) key;
    while( *s ) {
        res = res*MAGIC_CONSTANT + *s;
	s++;
    }
    return ((res < 0) ? -res : res)%n;
}
#else

/* Hashing function for a string */
unint
str_hash(pointer key, unint size)
{
    string  s = (string) key;
    unint   hash = 0;
    while( *s ) {
	hash += *s;
	hash += (hash << 10);
	hash ^= (hash >> 6);
	s++;
    }
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    return (hash % size);
}
#endif

bool
str_equ(pointer k1, pointer k2)
{
    return( (k1 == k2) || (strcmp((string) k1, (string) k2) == 0) );
}


unsigned int 
ptr_hash(pointer np, unsigned int n)
{
    return( ((unint) ((lunint) np)) % n );
}   

bool
ptr_equ(pointer p1, pointer p2)
{   
    return( p1 == p2 );
}

unint
int_hash(pointer p, unint n)
{
    return( ((long unsigned int) p) % n );
}

bool
int_equ(pointer p1, pointer p2)
{
    return( ((long unsigned int) p1) == ((long unsigned int) p2) );
}

pointer
find_hash(hash_record_ptr hp, pointer key)
{
    unint	bucket;
    bucket_ptr 	bp;

    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    bucket = (*(hp->hash_fn))(key, hp->tbl_size);
    bp = *((bucket_ptr *) M_LOCATE_BUF(&(hp->table), bucket));
    while(bp != NULL ) {
	if( ((*(hp->hash_eq))(key, bp->key)) == TRUE )
	    return( bp->data );
	bp = bp->next;
    }
    return( NULL );
}

void
insert_hash(hash_record_ptr hp, pointer key, pointer data)
{
    bucket_ptr	bp, *bpp;
    unint	pos;

    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    if( hp->autosize && hp->elements > 2*hp->tbl_size ) {
	/* Resize the table */
	buffer	new_tbl;
	unint	new_size;
	unint	i = 0;
	
	while( primes[i] < hp->elements )
	    i++;
	new_size = primes[i];
	new_buf(&new_tbl, new_size, sizeof(bucket_ptr));
	for(i = 0; i < new_size; i++) {
	    pointer dummy = 0;
	    push_buf(&new_tbl, (pointer) &dummy);
	}
	FOR_BUF(&(hp->table), bucket_ptr, bpp) {
	    bucket_ptr chp, tmp;
	    chp = *bpp;
	    while( chp != NULL ) {
		unint	new_pos;
		tmp = chp;
		chp = chp->next;
		new_pos = (*(hp->hash_fn))(tmp->key, new_size);
		tmp->next = *((bucket_ptr *) M_LOCATE_BUF(&new_tbl, new_pos));
		store_buf(&new_tbl, new_pos, (pointer) &tmp);
	    }
	}
	free_buf(&(hp->table));
	hp->table = new_tbl;
	hp->tbl_size = new_size;
    }

    pos = (*(hp->hash_fn))(key, hp->tbl_size);
    bp = (bucket_ptr) new_rec(&(hp->bucket_mgr));
    bp->key = key;
    bp->data = data;
    bp->next = *((bucket_ptr *) FAST_LOC_BUF(&(hp->table), pos));
    store_buf(&(hp->table), pos, (pointer) &bp);
    hp->elements++;
}

void
delete_hash(hash_record_ptr hp, pointer key)
{
    bucket_ptr	bp, *prevp;
    unint	pos;

    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    pos = (*(hp->hash_fn))(key, hp->tbl_size);
    prevp = (bucket_ptr *) (M_LOCATE_BUF(&(hp->table), pos));
    bp = *prevp;
    while( (*(hp->hash_eq))(key, bp->key) == FALSE ) {
	prevp = &(bp->next);
	bp = *prevp;
    }
    *prevp = bp->next;
    free_rec(&(hp->bucket_mgr), (pointer) bp);
    hp->elements--;
}

void
scan_hash(hash_record_ptr hp, void (*scan_fn)(pointer key, pointer data))
{
    bucket_ptr	*bpp;

    ASSERT(hp->initialized == HASH_MAGIC_NBR);
    FOR_BUF(&(hp->table), bucket_ptr, bpp) {
	bucket_ptr chp;
	chp = *bpp;
	while( chp != NULL ) {
	    (*scan_fn)(chp->key, chp->data);
	    chp = chp->next;
	}
    }
}

int
hash_size(hash_record_ptr hp)
{
    return(hp->elements);
}

bool
insert_check_hash(hash_record_ptr hp, pointer key, pointer data)
{
    if( find_hash(hp, key) != NULL )
	return(FALSE);
    insert_hash(hp, key, data);
    return(TRUE);
}

