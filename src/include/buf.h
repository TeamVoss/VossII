//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1993                        *
*                                                                   *
*********************************************************************/
/* buf.h -- header for buf.c */

#ifndef _BUF_H
#define _BUF_H
#include <strings.h>
#include "types.h"
#include "alloc.h"

#define BUF_MAGIC_NBR   19283746

typedef struct buffer {
	int	initialized;
	unint	buf_size;
	unint	item_size;
	unint	allocated;
	pointer	data;
} buffer, *buffer_ptr;

#define START_BUF(bp)		((bp)->data)
#define COUNT_BUF(bp)		((bp)->buf_size)
#define M_LOCATE_BUF(bp,i)	(((bp)->buf_size >= (unint)(i))?	\
				(bp)->data+(((unint)(i))*(bp)->item_size):NULL)
#define FAST_LOC_BUF(bp,i)	((bp)->data+((((unint)i))*(bp)->item_size))

#define FOR_BUF(bp,t,item)	for((item) = (t *) (bp)->data;		       \
				    (item) < ((t *)(bp)->data)+(bp)->buf_size; \
				    (item)++)

#define FUB_ROF(bp,t,item)	for((item)=((t *)(bp)->data)+(bp)->buf_size-1;\
				    (item)>=(t *)(bp)->data;		       \
				    (item)--)

/* Forward declarations */
void		new_buf(buffer_ptr bp, unint n, unint size);
void		free_buf(buffer_ptr bp);
bool		empty_buf(buffer_ptr bp);
void		push_buf(buffer_ptr bp, pointer item);
void		pop_buf(buffer_ptr bp, pointer p);
void		store_buf(buffer_ptr bp, unint i, pointer item);
pointer		locate_buf(buffer_ptr bp, unint i);
bool		fetch_buf(buffer_ptr bp, unint i, pointer dest);
void		resize_buf(buffer_ptr bp, unint size);

#endif /* _BUF_H */

