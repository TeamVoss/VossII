//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "rec.h"

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/
void
aligned_new_mgr(rec_mgr_ptr mp, unint size, unint alignment)
{
    /* Since records will form free list, must be at least of ptr size */
    size = (size > POINTER_SIZE)? size : POINTER_SIZE;
    mp->rec_size = size;
    mp->alignment = alignment;
    /* Leave room for block pointer */
    if( size > (NAT_BLOCK_SIZE-2*POINTER_SIZE) ) {
	mp->blk_size = size + 2*POINTER_SIZE;	/* Maybe some malloc padding? */
	mp->capacity   = 1;
    } else {
	mp->capacity   = (NAT_BLOCK_SIZE-2*POINTER_SIZE)/size;
	mp->blk_size = mp->capacity*size+2*POINTER_SIZE;
    }
    if( alignment == 1 ) {
        mp->first_blk = mp->last_blk = Malloc(mp->blk_size);
    } else {
        mp->first_blk = mp->last_blk = Alignment_Malloc(mp->blk_size,alignment);
    }
    FWD_BLK_PTR(mp->first_blk) =  NULL;
    BACK_BLK_PTR(mp->first_blk) = NULL;
    mp->free_rec = NULL;
    mp->allocated = 0;
    mp->pure = TRUE;
    mp->initialized = REC_MAGIC_NBR;
}

void
new_mgr(rec_mgr_ptr mp, unint size)
{
    aligned_new_mgr(mp, size, 1);
}

void
free_mgr(rec_mgr_ptr mp)
{
    pointer p, q;
    ASSERT(mp->initialized == REC_MAGIC_NBR);
    p = mp->first_blk;
    while( p != NULL ) {
	q = p;
	p = (pointer) FWD_BLK_PTR(p);
	FREE(q);
    }
    mp->initialized = 0;
}

pointer
new_rec(rec_mgr_ptr mp)
{
    pointer ret;
    pointer new_blk;

    ASSERT(mp->initialized == REC_MAGIC_NBR);
    mp->live++;
    if( mp->free_rec != NULL ) {
	/* Some record on the free list */
	ret = mp->free_rec;
	mp->free_rec = (pointer) *((pointer *) mp->free_rec);
	return( ret );
    }
    if( mp->allocated < mp->capacity ) {
	/* Still room in current block */
	ret = REC_ADDR(mp->last_blk, mp->allocated);
	mp->allocated++;
	return( ret );
    }
    /* No room left. Get new block */
    new_blk = Alignment_Malloc(mp->blk_size, mp->alignment);
    FWD_BLK_PTR(mp->last_blk) = (pointer *) new_blk;
    FWD_BLK_PTR(new_blk) = (pointer *) NULL;
    BACK_BLK_PTR(new_blk) = (pointer *) mp->last_blk;
    mp->last_blk = new_blk;
    mp->allocated = 0;
    ret = REC_ADDR(mp->last_blk, mp->allocated);
    mp->allocated++;
    return( ret );
}

int
ensure_pure_mgr(rec_mgr_ptr mp, char *file, int line)
{
    if( !mp->pure ) {
	fprintf(stderr, "FOR_REC on unpure manager on line %d in \"%s\"\n",
		    line,file);
	fflush(stdout);
	fflush(stderr);
	abort();
    }
    return 1;
}


void
free_rec(rec_mgr_ptr mp, pointer r)
{
    pointer p;
    ASSERT(mp->initialized == REC_MAGIC_NBR);
    ASSERT(mp->allocated > 0);
    ASSERT(mp->live > 0);
    mp->live--;
    if( r == REC_ADDR(mp->last_blk, mp->allocated-1) ) {
	mp->allocated--;
	if( mp->allocated == 0 ) {
	    if( BACK_BLK_PTR(mp->last_blk) == (pointer *) NULL )
		return;
	    /* Free block */
	    p = mp->last_blk;
	    mp->last_blk = (pointer) BACK_BLK_PTR(mp->last_blk);
	    FWD_BLK_PTR(mp->last_blk) = (pointer *) NULL;
	    FREE(p);
	    mp->allocated = mp->capacity;
	}
    } else {
	/* Not the last one. Put it on free list */
	*((pointer *) r) =  mp->free_rec;
	mp->free_rec = r;
	mp->pure = FALSE;
    }
}

bool
owned_by_mgr(rec_mgr_ptr mp, pointer item)
{
    ensure_pure_mgr(mp,__FILE__, __LINE__);
    for(pointer cur_blk = mp->first_blk;
	cur_blk != NULL;
	cur_blk = FWD_BLK_PTR(cur_blk))
    {
	pointer first = cur_blk+2*POINTER_SIZE;
	if( cur_blk == mp->last_blk ) {
	    pointer last = first + mp->allocated * mp->rec_size;
	    if( item >= first && item < last )  return TRUE;
	} else {
	    pointer last = first + mp->capacity * mp->rec_size;
	    if( item >= first && item < last )  return TRUE;
	}
    }
    return FALSE;
}
