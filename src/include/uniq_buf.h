//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2016                        *
*                                                                   *
*********************************************************************/
/* uniq_buf.h -- header for uniq_buf.c */

#ifndef _UNIQ_BUF_H
#define _UNIQ_BUF_H
#include <strings.h>
#include "buf.h"
#include "hash.h"


#define UNIQ_BUF_MAGIC_NBR 98257026

typedef struct uniq_buffer {
	int	    initialized;
	int	    item_size;
	rec_mgr	    recs;
	buffer	    buf;
	int	    items;
	hash_record item2idx;
} uniq_buffer, *uniq_buffer_ptr;

#define COUNT_UNIQ_BUF(bp)	    ((bp)->items)
#define M_LOCATE_UNIQ_BUF(bp,i)	    M_LOCATE_BUF(&((bp)->buf), i)

#define FOR_UNIQ_BUF(bp,t,item)	FOR_BUF(&((bp)->buf),t,item)

/* Forward declarations */
void		new_uniq_buf(uniq_buffer_ptr bp, int n, int size);
void		free_uniq_buf(uniq_buffer_ptr bp);
bool		empty_uniq_buf(uniq_buffer_ptr bp);
bool		push_uniq_buf(uniq_buffer_ptr bp, pointer item);
int		get_uniq_buf_index(uniq_buffer_ptr bp, pointer item);
int		find_insert_uniq_buf(uniq_buffer_ptr bp, pointer item);
pointer		locate_uniq_buf(uniq_buffer_ptr bp, int i);
bool		fetch_uniq_buf(uniq_buffer_ptr bp, int i, pointer dest);
#endif /* _UNIQ_BUF_H */

