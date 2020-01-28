/************************************************************************/
/*	Copyright (C) Carl-Johan Seger 1993				*/
/*	All rights reserved						*/
/************************************************************************/
#ifndef _ALLOC_H
#define _ALLOC_H

#include "types.h"

pointer			Malloc(unsigned long int size);
pointer			Alignment_Malloc(unsigned long int size, int alignment);
pointer			Calloc(unsigned long int size);
pointer			Realloc(pointer oldp, unsigned long int size);
void			Free(pointer p);

#define ALLOC(type)	((type *) Malloc(sizeof( type )))
#define FREE(p)		Free((pointer)(p))

#endif /* _ALLOC_H */
