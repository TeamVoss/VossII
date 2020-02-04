//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "buf.h"

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

void
new_buf(buffer_ptr bp, unint n, unint size)
{
    n = (n < 1)?1:n;
    bp->buf_size = 0;
    bp->item_size = size;
    bp->allocated = n*size;
    bp->data = Calloc(n*size);
    bp->initialized = BUF_MAGIC_NBR;

}

void
free_buf(buffer_ptr bp)
{
    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    FREE(bp->data);
    bp->initialized = 0;
}

bool
empty_buf(buffer_ptr bp)
{
    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    return( bp->buf_size == 0 );
}

void
push_buf(buffer_ptr bp, pointer item)
{
    pointer new;

    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    if( bp->item_size * bp->buf_size >= bp->allocated ) {
	/* Must grow buffer */
	new = Calloc(2*bp->buf_size * bp->item_size);
	bcopy(bp->data, new, bp->buf_size*bp->item_size);
	FREE(bp->data);
	bp->data = new;
	bp->allocated = 2*bp->buf_size * bp->item_size;
    }
    bcopy(item,  bp->data + (bp->item_size * bp->buf_size), bp->item_size);
    bp->buf_size++;
}

void
pop_buf(buffer_ptr bp, pointer p)
{
    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    ASSERT(bp->buf_size > 0 );
    if( p != NULL )
        bcopy(bp->data + ((bp->buf_size-1)*bp->item_size), p, bp->item_size);
    bp->buf_size--;
}

void
store_buf(buffer_ptr bp, unint i, pointer item)
{
    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    ASSERT( i < bp->buf_size );
    bcopy(item, bp->data + (i*bp->item_size), bp->item_size);
}

bool
fetch_buf(buffer_ptr bp, unint i, pointer dest)
{
    pointer ret;
    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    ret = M_LOCATE_BUF(bp, i);
    if( ret == NULL )
	return( FALSE );
    bcopy(ret, dest, bp->item_size);
    return( TRUE );
}

void
resize_buf(buffer_ptr bp, unint size)
{
    pointer new;
    ASSERT(bp->initialized == BUF_MAGIC_NBR);
    if( size < bp->buf_size ) {
	/* Shrink the buffer */
	bp->buf_size = size;
	return;
    }
    /* Increase the buffer size. Pad with NULL blocks */
    if( bp->item_size * size >= bp->allocated ) {
        /* Must grow buffer. NULL fill */
        new = Calloc(size * bp->item_size);
        bcopy(bp->data, new, bp->buf_size*bp->item_size);
        FREE(bp->data);
        bp->data = new;
        bp->allocated = size * bp->item_size;
    }
    bzero(bp->data+(bp->buf_size*bp->item_size),
	  (size - bp->buf_size)*bp->item_size);
    bp->buf_size = size;
}

pointer
locate_buf(buffer_ptr bp, unint i)
{
    if( COUNT_BUF(bp) <= i ) {
	resize_buf(bp, i+1);
    }
    return( M_LOCATE_BUF(bp,i) );
}
