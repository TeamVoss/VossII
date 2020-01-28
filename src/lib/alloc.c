#include "alloc.h"

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

pointer
Malloc(unsigned long int size)
{
    pointer ret;
    if( (ret = (pointer) malloc(size)) == (pointer) NULL ) {
	fprintf(stderr, "Out of memory in malloc\n");
	exit(-1);
    }
    return( ret );
}

// Allocate memory of a given size with a particular alignment.
pointer
Alignment_Malloc (unsigned long int size, int alignment)
{
    if( alignment == 1 ) {
        return ( Malloc(size) );
    }
    ASSERT ((alignment % sizeof (int)) == 0);
    pointer res;
    if( posix_memalign(&res, alignment, size) != 0 ) {
	fprintf(stderr, "Out of memory in posix_memalign: ");
        fprintf(stderr, "Tried to get %ld byes aligned by %d\n",size,alignment);
	exit(-1);
    };

    return res;
}

pointer
Calloc(unsigned long int size)
{
    pointer ret;
    ret = (pointer) calloc(size, 1);
    return(ret);
}

pointer
Realloc(pointer oldp, unsigned long int size)
{
    pointer ret;
    if( (ret = (pointer) realloc(oldp, size)) == (pointer) NULL ) {
	fprintf(stderr,"Out of memory in realloc. Asking for %ld bytes\n",size);
	exit(-1);
    }
    return( ret );
}

void
Free(pointer p)
{
    free(p);
}
