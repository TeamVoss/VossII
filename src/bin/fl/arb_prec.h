/********************************************************************
*                                                                   *
*     Copyright (C) 1994 Mark Greenstreet                           *
*     Edited by Carl-Johan Seger, 1994				    *
*                                                                   *
*********************************************************************/
/* arb_prec.h -- header for arb_prec.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef long int	*arbi_T;

/* ----- Function prototypes for public functions ----- */
/* Conversion routines */
arbi_T		Arbi_FromString(char *s, int base);
char		*Arbi_ToString(arbi_T n, int base);
arbi_T		Arbi_FromInt(long int i);
long int	*Arbi_ToInt(arbi_T x);

/* Arithmetic operations */
arbi_T 		Arbi_cpy(arbi_T x);
arbi_T		Arbi_neg(arbi_T x);
arbi_T		Arbi_abs(arbi_T x);
arbi_T		Arbi_add(arbi_T x, arbi_T y);
arbi_T		Arbi_sub(arbi_T x, arbi_T y);
arbi_T		Arbi_mlt(arbi_T x, arbi_T y);
arbi_T		Arbi_div(arbi_T x, arbi_T y);
arbi_T		Arbi_mod(arbi_T x, arbi_T y);
arbi_T		Arbi_gcd(arbi_T x, arbi_T y);
bool		Arbi_IsNegative(arbi_T x);

/* Bitvector operations (view the integers as unsigned) */
arbi_T		Arbi_bvAND(arbi_T x, arbi_T y);
arbi_T		Arbi_bvOR(arbi_T x, arbi_T y);
arbi_T		Arbi_bvXOR(arbi_T x, arbi_T y);
arbi_T		Arbi_bvNOT(arbi_T x);

/* Comparisons */
int		Arbi_cmp(arbi_T x, arbi_T y);
#define arbi_LESS  -1
#define arbi_EQ     0
#define arbi_GREAT  1

/* Memory allocation and garbage collection */
void		*Arbi_alloc(unsigned nbytes);
void 		Arbi_free(arbi_T n);
void		*Arbi_collect(void *p);
void		Arbi_mark(arbi_T p);
void		Arbi_sweep(void);

/* Reporting exceptions */
char		*Arbi_errmsg(void);
void		Arbi_die(char *source, char *fn, char *msg);

/* Misc */
unsigned int	Arbi_hash (arbi_T value, unsigned int n);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */

#ifndef ARB_PREC_H
#define ARB_PREC_H
#include "fl.h"	/* Global data types and include files 		     */

/* For old-style C, the definitions of the 'long' and 'short' types are
 * machine dependent.  Is this also true for ansi-C?  If so, then the
 * following defintions should be put in #ifdef machine_type blocks.
 */
typedef long int Word;
typedef int HalfWord;
#define WordSize 64
#define HalfSize 32
#define Hmask (~(~((unsigned long) 0) << (unsigned long) HalfSize))
#define s_Word long int
#define s_HalfWord int
#define MinWord (((long) 1) << (WordSize-1))
#define MaxWord (~MinWord)
#define MinHalf (((int) 1) << HalfSize)
#define MaxHalf (~MinHalf)
#define Abs(x) ((x>=0) ? (x) : -(x))

typedef short Length_T;
#define Length(x)	(*((Length_T *)(((char *) (x))-2)))

/* An arbitrary precision integer (henceforth called arb_int) is represented
 * by an array of Words.  Let x be of type arbi_T.  The length of the array
 * is given by ((Length_T *)x)[-1].  (We've got a deal with the allocator
 * that lets us use that space).  Let k be the length of x's array, and let
 * M = 2^WordSize.  Let u = (unsigned long *)x.  Then, the value of x is
 * given by
 *   (M^(k-1) * x[k-1]) + sum_{i=0}^{k-1} u[i] * M^i
 * That is, the most significant digit of x is signed and all of the others
 * are unsigned.
 * Since the number of digits is represented by a short, numbers must have
 * magnitudes less than ~10^315K.  If you're using numbers anywhere near
 * this limit, you should use a different package anyway (e.g. one that
 * uses fast Fermat number transforms for large multiplications).
 */

#define Split(w, l, h) \
        ((l = (w) & Hmask), (h = ((unsigned s_Word)(w) >> HalfSize)))
#define Merge(l, h) (((l) & Hmask) | ((h) << HalfSize))
#define Acc(l, h, w) \
        ((l += (w) & Hmask), (h += (unsigned s_Word)(w) >> HalfSize))
#define Carry(l, h) (h += (l) >> HalfSize)

#define MaxBlock 1000
#define BlockSize ((1 << 16)*sizeof(void *))


/************************************************************************

About storage, garbage collection, etc.:
  This package includes it's own allocation routines in alloc.c.  arbi_alloc
  calls Malloc to get large chunks of memory, which it manages for creating
  arbi_T's.  The size of the chunk is set by the constant BlockSize defined
  in alloc.c.
    Functions that return arbi_T's come in two flavors:
      arbi_name:  returns garbage collected storage.
      arbi__name: returns a pointer to memory that won't be garbage collected.
		      Such blocks can be freed by an explicit call to arbi_free.
  If p is a pointer to a block that is not garbage collectible, calling
  arbi_collect(p) will mark p as collectible.  To do a garbage collection,
  first call arbi_mark for each block allocated by this package that is
  garbage collectible and still in use.  Then call arbi_sweep to reclaim
  unmarked blocks.
    In the current implementation, the allocator will manage at most 64Mbytes
  (not necessarily contiguous) of memory.

About conversion routines:
    arbi_FromString(char *s, int base)
	Convert the string s to an arbi_T.  base tells what base to use
	for the conversion.  If 10 < base <= 36, 'a' - 'z' are used for
	the extra digits in the obvious way.  This is case insensitive
	(i.e. F02C = f02c).
	arbi_FromString fails if s cannot be interpreted as a number in
	the given base, or base < 0, or base > 36.
    arbi_ToString(arbi_T n, int base)
	Convert the arbi_T n to a string.  base tells what base to use.
	If 10 < base <= 36, 'a' - 'z' are used for the extra digits.
	arbi_ToString fails if base < 0 or base > 36.
    arbi_FromInt(long int i)
	Convert the long int i to an arbi_T.
	No failures.
    arbi_ToInt(arbi_T n)
	Returns a pointer to a long int with the same value as the arbi_T n.
	arbi_ToInt fails if n doesn't fit in a long int.

About arithmetic operations:
  All arithmetic routines return their results in a newly allocated piece
  of memory, even if the operation happens to be the identity function
  (such as x+0, x*1, abs(positive_value), x mod 0, etc.).

  arbi_cpy(arbi_T x)
    Make a copy of x (i.e. the identity function).
    No failures.
  arbi_neg(arbi_T x)
    Return the negation of x.
    No failures.
  arbi_abs(arbi_T x)
    Return the absolute value of x.
    No failures.
  arbi_add(arbi_T x, arbi_T y)
    Return x+y.
    No failures.
  arbi_sub(arbi_T x, arbi_T y)
    Return x-y.
    No failures.
  arbi_mlt(arbi_T x, arbi_T y)
    Return x*y.
    No failures.
  arbi_div(arbi_T x, arbi_T y)
    Return x/y.  x = x*(x/y) + (x mod y).  See note on arbi_mod.
    Failure if y is 0.
  arbi_mod(arbi_T x, arbi_T y)
    Return x mod y.
    If y = 0, then x mod y = x.
    Else, x mod y is in the range 0 .. y - sign(y) (i.e. x mod y is zero
    or of the same sign as y).
    No Failures.
  arbi_gcd(arbi_T x, arbi_T y)
    Return the greatest common divisor of x and y.
    gcd(x, y) >= 1.
    No Failures.

About comparisons
    arbi_cmp(arbi_T x, arbi_T y)
      if(x < y)  return(arbi_LESS);
      if(x == y) return(arbi_EQ);
      if(x > y)  return(arbi_GREAT);
      No failures.

About failures:
   If an arithmetic routine is called with bad arguments, it fails.
   Failure is indicated by returning NULL, and setting arbi__errmsg
   to an appropriate string.  The value of this string is returned
   by the function arbi_errmsg(void).  Don't call an arithmetic
   function with a NULL argument where an arbi_T is expected, unless
   you want a core dump.

   If the allocation package detects a corrupted heap, it calls arbi_die
   which prints an error message and exits.

************************************************************************/

#endif /* ARB_PREC_H */
#endif /* EXPORT_FORWARD_DECL */
