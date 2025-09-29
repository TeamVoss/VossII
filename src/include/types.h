//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*	General type abbreviations					*/
/*									*/
/*	Original author: Carl-Johan Seger 1993				*/
/*									*/
/************************************************************************/

#ifndef _TYPES_H
#define _TYPES_H
#include <stdio.h>
#include <stdlib.h>

typedef char		    *string;
typedef unsigned int	    machine_word;
typedef unsigned int	    unint;
typedef unsigned long int   lunint;
typedef unsigned int	    *mword_ptr;
typedef void	            *pointer;
typedef unsigned char       *bpointer;
typedef unsigned int	    bool;
typedef unsigned char	    flag;
typedef unsigned char	    uchar;


#define TRUE 1
#define FALSE 0


#define VOID void
#define IGNORE (void)


#define POINTER_SIZE          (sizeof (void *))

#define NULLSTR '\0'

#ifdef DEBUG
#define ASSERT(cond) if( !(cond) ) {                                    \
            fprintf(stderr, "Assertion failed on line %d in \"%s\"\n",  \
                        __LINE__, __FILE__);                            \
            fflush(stdout);                                             \
            fflush(stderr);                                             \
            abort();							\
	}
#else /* DEBUG */
#define ASSERT(cond)
#endif /* DEBUG */


#endif /* _TYPES_H */
