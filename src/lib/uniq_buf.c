//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "uniq_buf.h"

static int  cur_item_size = 0;

static unint	item_hash(pointer key, unint n);
static bool	item_equ(pointer k1, pointer k2);

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

void
new_uniq_buf(uniq_buffer_ptr bp, int n, int size)
{
    new_buf(&(bp->buf), n, size);
    new_mgr(&(bp->recs), size);
    create_hash(&(bp->item2idx), n, item_hash, item_equ);
    bp->item_size   = size;
    bp->items = 0;
    bp->initialized = UNIQ_BUF_MAGIC_NBR;
}

void
free_uniq_buf(uniq_buffer_ptr bp)
{
    free_buf(&(bp->buf));
    free_mgr(&(bp->recs));
    dispose_hash(&(bp->item2idx), NULLFCN);
    bp->initialized = 0;
}

bool
empty_uniq_buf(uniq_buffer_ptr bp)
{
    ASSERT(bp->initialized == UNIQ_BUF_MAGIC_NBR);
    return( bp->items == 0 );
}

#define INT2PTR(p)      ((pointer) ((long int)(p)))
#define PTR2INT(p)      ((int) ((long int)(p)))

bool
push_uniq_buf(uniq_buffer_ptr bp, pointer item)
{
    cur_item_size = bp->item_size;
    if( find_hash(&(bp->item2idx), item) != NULL ) { return FALSE; }
    pointer p = new_rec(&(bp->recs));
    bcopy(item, p, bp->item_size);
    push_buf(&(bp->buf), item);
    bp->items++;
    int idx = bp->items;
    insert_hash(&(bp->item2idx), p, INT2PTR(idx));
    return TRUE;
}

int
get_uniq_buf_index(uniq_buffer_ptr bp, pointer item)
{
    cur_item_size = bp->item_size;
    return( PTR2INT(find_hash(&(bp->item2idx), item))-1 );
}


pointer
locate_uniq_buf(uniq_buffer_ptr bp, int i)
{
    ASSERT(bp->initialized == UNIQ_BUF_MAGIC_NBR);
    return( locate_buf(&(bp->buf), i) );
}

bool
fetch_uniq_buf(uniq_buffer_ptr bp, int i, pointer dest)
{
    ASSERT(bp->initialized == UNIQ_BUF_MAGIC_NBR);
    return( fetch_buf(&(bp->buf), i, dest) );
}

/************************************************************************/
/*                      Private Functions                               */
/************************************************************************/

#define MAGIC_CONSTANT	997

static unint
item_hash(pointer key, unint n)
{
    int		res = 0;
    string	s;
    s = (string) key;
    for(int i = 0; i < cur_item_size; i++) {
        res = res*MAGIC_CONSTANT + *s;
	s++;
    }
    return ((res < 0) ? -res : res)%n;
}
#undef MAGIC_CONSTANT
 
static bool
item_equ(pointer k1, pointer k2)
{
    string	s1, s2;
    s1 = (string) k1;
    s2 = (string) k2;
    for(int i = 0; i < cur_item_size; i++) {
	if( *s1 != *s2 ) return FALSE;
	s1++; s2++;
    }
    return TRUE;
}

