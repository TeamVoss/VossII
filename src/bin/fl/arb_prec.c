//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Mark Greenstreet 1994                        *
*     Extensively edited by Carl-Johan Seger, 1994, 2017            *
*                                                                   *
*********************************************************************/
/* arb_prec.c: arbitrary precision integer arithmetic */
#include "arb_prec.h"

/* ------------- Global variables ------------- */
char	*arbi__errmsg;

/********* Global variables referenced ***********/

/***** PRIVATE VARIABLES *****/
static char	*blocks[MaxBlock];
static int	BlockIndex = -1;
static char	*BlockPos;

/* FreeList entries correspond to roughly exponentially growing chunk
 * sizes.  In particular, for i <= 8, FreeList[i], is a list of chunks
 * of i words.  For i >= 8, the chunk size for FreeList[i+4] is twice
 * that for FreeList[i].  This allows a relatively small number of free
 * lists to handle all possible block sizes (up to 2^65 * sizeof(void *))
 * with relatively little waste (an allocated block is at most 1/9 larger
 * than the request).
 */
static void *FreeList[256];

/* ----- Forward definitions local functions ----- */
static arbi_T		allocate(Length_T ndigits);
static arbi_T		arbi__FromInt(long int v);
static arbi_T		arbi__FromString(char *s, int base);
static arbi_T		arbi__abs(arbi_T x);
static arbi_T		arbi__add(arbi_T x, arbi_T y);
static arbi_T		arbi__cpy(arbi_T x);
static arbi_T		arbi__div(arbi_T x, arbi_T y);
static arbi_T		arbi__gcd(arbi_T x, arbi_T y);
static arbi_T		arbi__mlt(arbi_T x, arbi_T y);
static arbi_T		arbi__mod(arbi_T x, arbi_T y);
static arbi_T		arbi__neg(arbi_T x);
static arbi_T		arbi__sub(arbi_T x, arbi_T y);
static char		*arbi__ToString(arbi_T x, int base);
static int		cmp0(arbi_T x);
static int		cmpa(arbi_T x, arbi_T y);
static unsigned int	IndexToNbytes(unsigned int ix);
static unsigned int	NbytesToIndex(unsigned int nb);
static void		*arbi_alloc(unsigned int nbytes);
static void		*arbi_collect(void *p);
static void		AddBlock(void);
static void		arbi_die(char *source, char *fn, char *msg);
static void		arbi_free(void *p);
static void		divmod(arbi_T x, arbi_T y, arbi_T *qr);
static void		ffree(arbi_T x);
static void		trim(arbi_T x);
static char 		*WAMalloc(unint n);
static Word		bv_xor(Word a, Word b);
static Word		bv_or(Word a, Word b);
static Word		bv_and(Word a, Word b);
static Word 		bv_not(Word a, Word b);
static arbi_T 		arbi__bvop(arbi_T x, arbi_T y,  Word (*operation)(Word a, Word b));

/********************************************************/
/*                    PUBLIC FUNCTIONS                  */
/********************************************************/
void
Init_arbi()
{
    for(int i = 0; i < MaxBlock; i++) {
	blocks[i] = NULL;
    }
}

int
Arbi_cmp(arbi_T x, arbi_T y)
{
    Length_T lx, ly, i;

    lx = Length(x); ly = Length(y);
    if(lx > ly) {
	if(x[lx-1] < 0) return(arbi_LESS);
	else  return(arbi_GREAT);
    } else if(lx < ly) {
	if(y[ly-1] >= 0) return(arbi_LESS);
	else return(arbi_GREAT);
    }
    if(x[lx-1] < y[lx-1]) return(arbi_LESS);
    else if(x[lx-1] > y[lx-1]) return(arbi_GREAT);
    for(i = lx-2; i >= 0; i--) {
	if((unsigned long)(x[i]) < (unsigned long)(y[i])) return(arbi_LESS);
	else if((unsigned long)(x[i])>(unsigned long)(y[i])) return(arbi_GREAT);
    }
    return(arbi_EQ);
}

long int *
Arbi_ToInt(arbi_T x)
{
    arbi__errmsg = NULL;
    if(Length(x) == 1) return(x);
    arbi__errmsg = "arbi_ToInt: overflow\n";
    Wprintf("Arbitrary precision to integer conversion failure!!!!\n");
    return(NULL);
}

void
Arbi_mark(arbi_T p)
{
    char *t;

    t = (char *)p;
    if(!(t[-4] & 1))
        arbi_die("alloc", "arbi_mark", "bad chunk");
    t[-4] = 3;
}

void
Arbi_sweep(void)
{
    int i;
    char *t, *y;

    for(i = 0; i <= BlockIndex; i++) {
        y = blocks[i] + BlockSize;
        for(t = blocks[i]+sizeof(void *); t < y;
        	t += IndexToNbytes(t[-3]) + sizeof(void *)) {
            switch(t[-4]) {
                case 0:
		case 2:  /* not collectible, ignore it */
		    break;
                case 1:  /* collectible and unmarked, free it */
                    arbi_free((void *)t);
                    break;
		case 3:  /* collectible and marked, clear the mark */
		    t[-4] = 1;
		    break;
		default:  /* bad tag */
		    arbi_die("alloc", "arbi_sweep", "bad chunk");
	    }
	}
    }
}

arbi_T
Arbi_FromString(char *s, int base)
{
    return(arbi_collect(arbi__FromString(s, base)));
}

char *
Arbi_ToString(arbi_T n, int base)	/* FL doesn't garbage collect strings */
{
    char *ret;
    ret = arbi__ToString(n, base);
    ret = arbi_collect(ret);
    return(ret);
}

arbi_T
Arbi_FromInt(long int i)
{
    return(arbi_collect(arbi__FromInt(i)));
}

arbi_T
Arbi_cpy(arbi_T x)
{
    return(arbi_collect(arbi__cpy(x)));
}

arbi_T
Arbi_neg(arbi_T x)
{
    return(arbi_collect(arbi__neg(x)));
}

arbi_T
Arbi_abs(arbi_T x)
{
    return(arbi_collect(arbi__abs(x)));
}

arbi_T
Arbi_add(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__add(x, y)));
}

arbi_T
Arbi_sub(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__sub(x, y)));
}

arbi_T
Arbi_mlt(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__mlt(x, y)));
}

arbi_T
Arbi_div(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__div(x, y)));
}

arbi_T
Arbi_mod(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__mod(x, y)));
}

arbi_T
Arbi_gcd(arbi_T x, arbi_T y)
{
return(arbi_collect(arbi__gcd(x, y)));
}

void
Arbi_free(arbi_T n)
{
    arbi_free((void*) n);
}

arbi_T
Arbi_bvAND(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__bvop(x, y, bv_and)));
}

arbi_T
Arbi_bvOR(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__bvop(x, y, bv_or)));
}

arbi_T
Arbi_bvXOR(arbi_T x, arbi_T y)
{
    return(arbi_collect(arbi__bvop(x, y, bv_xor)));
}

arbi_T
Arbi_bvNOT(arbi_T x)
{
    return(arbi_collect(arbi__bvop(x, x, bv_not)));
}

bool
Arbi_IsNegative(arbi_T x)
{
    return( cmp0(x) < 0 );
}

unsigned int  
Arbi_hash (arbi_T value, unsigned int n)
{
    unsigned int res = 0;
    for (int i = 0; i < Length (value); i++) {
        res = (res * 367)  + value[i];
    }
    return( res % n );
}

/********************************************************/
/*                    LOCAL FUNCTIONS                   */
/********************************************************/
static arbi_T
allocate(Length_T ndigits)
{
    arbi_T new;

    new = (arbi_T)arbi_alloc((unsigned int) (ndigits * sizeof(Word)));
    Length(new) = ndigits;
    return(new);
}

static void
ffree(arbi_T x)
{
    arbi_free((void *)x);
}

static int
cmp0(arbi_T x)
{
    Length_T lx;
    Word msd;

    lx = Length(x);
    msd = x[lx-1];
    if((msd == 0) && (lx == 1)) return(0);
    else if((msd >= 0)) return(1);
    else return(-1);
}

static void
trim(arbi_T x)
{
    int i;

    for(i = Length(x) - 1; i > 0; i--)
	if(!((x[i-1] < 0) ? (x[i] == -1) : (x[i] == 0)))
	    break;
    Length(x) = i+1;
}

/************************************************************************/
/*									*/
/*  Addition, copy, negation, subtraction, absolute value, comparision	*/	/*									*/
/************************************************************************/

static arbi_T
arbi__add(arbi_T x, arbi_T y)
{
    Length_T lx, ly, lz;
    Word wx = 0;
    Word wy = 0;
    Word wz = 0;
    int i, carry;
    arbi_T z;

    arbi__errmsg = NULL;
    lx = Length(x); ly = Length(y);
    if((lx == 1) && (ly == 1)) {  /* handle this case fast */
    	wx = x[0]; wy = y[0]; wz = wx+wy;
    	if(((wx<0) != (wy<0)) || ((wz<0) == (wx<0))) /* no overflow */
    	    return(arbi__FromInt(wz));
    }
    if(lx < ly) { /* swap them */
        z  = x;  x  = y;  y  = z;
        lz = lx; lx = ly; ly = lz;
    }
    z = allocate(lx+1);
    carry = 0;
    for(i = 0; i < ly; i++) {
    	wx = x[i]; wy = y[i];
    	wz = wx + wy + carry;
	z[i] = wz;
	carry = ((wx<0) && (wy<0)) || (((wx<0) || (wy<0)) && (wz>=0));
    }
    if(lx > ly) {
        /* sign extend y, and continue propagating carries  */
        for(wy = (wy < 0) ? -1 : 0; i < lx; i++) {
	    wx = x[i];
	    wz = wx + wy + carry;
	    z[i] = wz;
	    carry = ((wx<0) && (wy<0)) || (((wx<0) || (wy<0)) && (wz>=0));
	}
    }
    /* check for overflow */
    if(((wx<0) == (wy<0)) && ((wz<0) != (wx<0))) {
	z[lx] = (wz < 0) ? 0 : -1;  /* overflow */
    } else {
	Length(z) = lx;
	trim(z);
    }
    return(z);
}

static Word
bv_xor(Word a, Word b)
{
    return(a ^ b);
}

static Word
bv_or(Word a, Word b)
{
    return(a | b);
}

static Word
bv_and(Word a, Word b)
{
    return(a & b);
}

static Word
bv_not(Word a, Word b)
{
    (void) b;
    return(~a);
}

static arbi_T
arbi__bvop(arbi_T x, arbi_T y,  Word (*operation)(Word a, Word b))
{
    Length_T lx, ly, lz;
    int i, sign;
    arbi_T z;

    arbi__errmsg = NULL;
    lx = Length(x); ly = Length(y);
    if((lx == 1) && (ly == 1)) {  /* handle this case fast */
	return( arbi__FromInt(operation(x[0], y[0])) );
    }
    if(lx < ly) { /* swap them */
        z  = x;  x  = y;  y  = z;
        lz = lx; lx = ly; ly = lz;
    }
    z = allocate(lx);
    for(i = 0; i < ly; i++)
    	z[i] = operation(x[i], y[i]);
    sign = (y[ly-1] < 0)? -1 : 0;
    for(i = ly; i < lx; i++)
	z[i] = operation(x[i], sign);	/* View the numbers as unsigned! */
    Length(z) = lx;
    trim(z);
    return(z);
}

static arbi_T
arbi__cpy(arbi_T x)
{
    Length_T lx;
    arbi_T y;
    int i;

    arbi__errmsg = NULL;
    lx = Length(x);
    y = allocate(lx);
    for(i = 0; i < lx; i++)
        y[i] = x[i];
    return(y);
}

static arbi_T
arbi__neg(arbi_T x)
{
    Length_T lx;
    Word wx, wy;
    arbi_T one, y, z;
    int i;

    arbi__errmsg = NULL;
    lx = Length(x);
    if(lx == 1) {  /* handle this case fast */
    	wx = x[0]; wy = -wx;
    	if((wx<0) != (wy<0)) /* no overflow */
    	    return(arbi__FromInt(wy));
    }

    /* arbi_T is 2's complement, just invert and add one */
    y = allocate(lx+1);
    for(i = 0; i < lx; i++)
        y[i] = ~x[i];
    y[lx] = (x[lx-1] < 0) ? 0 : -1; /* sign extend */
    one = arbi__FromInt(1);
    z = arbi__add(y, one);
    ffree(y); ffree(one);
    return(z);
}

static arbi_T
arbi__abs(arbi_T x)
{
    arbi__errmsg = NULL;
    if(x[Length(x)-1] < 0) return(arbi__neg(x));
    else return(arbi__cpy(x));
}

static arbi_T
arbi__sub(arbi_T x, arbi_T y)
{
    Word wx, wy, wz;
    arbi_T ny, z;

    arbi__errmsg = NULL;
    if((Length(x) == 1) && (Length(y) == 1)) { /* handle this case fast */
	wx = x[0]; wy = y[0]; wz = wx-wy;
        if(((wx<0) == (wy<0)) || ((wx<0) == (wz<0))) /* no overflow */
            return(arbi__FromInt(wz));
    }
    ny = arbi__neg(y);
    z = arbi__add(x, ny);
    ffree(ny);
    return(z);
}

/************************************************************************/
/*									*/
/*		multiplication, division, mod, gcd			*/
/*									*/
/************************************************************************/

static arbi_T
arbi__mlt(arbi_T x, arbi_T y)
{
    Length_T lx, ly, lp, i;
    unsigned s_Word p0, p1, p2, p3;
    arbi_T p, ax, ay, ap;

    arbi__errmsg = NULL;
    lx = Length(x); ly = Length(y);
    if((lx == 1) && (ly == 1)) { /* handle this case fast */
	Word wx, wy, wp;
	wx = x[0]; wy = y[0];
        if(!wx) return(arbi__FromInt(0));
        if(!wy) return(arbi__FromInt(0));
	wp = wx*wy;
	if(wy == wp/wx && wx == wp/wy ) /* no overflow? */
	    return(arbi__FromInt(wp));
    }

    lp = lx + ly; ap = allocate(lx + ly);
    ax = (cmp0(x) < 0) ? arbi__neg(x) : x;
    ay = (cmp0(y) < 0) ? arbi__neg(y) : y;
    p0 = p1 = 0;
    for(i = 0; i < lp; i++) {
	Length_T jmax, kmax, j;
	p2 = p3 = 0;
	jmax = (i < lx) ? i : (lx-1);
	kmax = (i < ly) ? i : (ly-1);
	for(j = i-kmax; j <= jmax; j++) {
	    Length_T k;
	    unsigned s_Word pp, xx, yy;
	    unsigned s_HalfWord xl, yl, xh, yh;
	    k = i-j;
	    xx = ax[j]; Split(xx, xl, xh);
	    yy = ay[k]; Split(yy, yl, yh);
	    pp = ((unsigned s_Word) xl)*((unsigned s_Word) yl); Acc(p0, p1, pp);
	    pp = ((unsigned s_Word) xl)*((unsigned s_Word) yh); Acc(p1, p2, pp);
	    pp = ((unsigned s_Word) xh)*((unsigned s_Word) yl); Acc(p1, p2, pp);
	    pp = ((unsigned s_Word) xh)*((unsigned s_Word) yh); Acc(p2, p3, pp);
	}
	Carry(p0, p1); Carry(p1, p2);
	ap[i] = Merge(p0, p1);
	p0 = p2; p1 = p3;
    }
    
    if(ax != x) ffree(ax);
    if(ay != y) ffree(ay);
    if((ax != x) != (ay != y)) {
	p = arbi__neg(ap);
	ffree(ap);
    } else p = ap;
    trim(p);
    return(p);
}


static int
cmpa(arbi_T x, arbi_T y)
{
    arbi_T ax, ay;
    int d;

    ax = arbi__abs(x); ay = arbi__abs(y);
    d = Arbi_cmp(ax, ay);
    ffree(ax); ffree(ay);
    return(d);
}

/* divmod(x, y, qr): qr[0] <- x DIV y, qr[1] <- x MOD y
 *
 *  The algorithm successively guesses refinements, g, to the quotient
 *  and replaces the current approximation of the quotient, q, with q+g,
 *  and replaces the current approximation of the remainder, r, with r - g*y.
 *
 *  q = 0;
 *  j = Length(x) - Length(y);
 *  r = x DIV (1 << j*WordSize);
 *
 *  while(TRUE) { (* invariant:  x = (q*y + r)*M + (x MOD M)  *)
 *		  (* where M = (1 << j*WordSize) *)
 *      if(abs(r) < abs(y)/2) {
 *	    if(j == 0) { (* we're done *)
 *		(* make sure r is between 0 and y - sign(y) *)
 *		if(((r < 0) != (y < 0)) && (cmp0(r) != 0)) {
 *		    q = q-1;
 *		    r = r+y;
 *		}
 *		qr[0] = q;
 *		qr[1] = r;
 *		return;
 *	    }
 *	    (* else shift *)
 *	    if(abs(r) > abs(y)/2) {
 *		q -= sign(y);
 *		r += abs(y);
 *	    } else if(r > abs(y)/2) 
 *	    j--;
 *	    q = (1 << WordSize)*q;
 *	    r = (1 << WordSize)*r + x[j];
 *	} else {
 *	    g = guess(x, y);
 *	    q = q + g;
 *	    r = r - g*y;
 *	}
 *  }
 */

static void
divmod(arbi_T x, arbi_T y, arbi_T *qr)
{
    Length_T lx, ly, i, j, lr;
    arbi_T q, r;
    Word yy, yh, yl, z;
    long ys;

    lx = Length(x); ly = Length(y);
    if(lx < ly) { /* handle the degenerate cases */
 	if(((cmp0(x) < 0) == (cmp0(y) < 0)) || (cmp0(x) == 0)) {
	    qr[0] = arbi__FromInt(0);
	    qr[1] = arbi__cpy(x);
	} else {
	    qr[0] = arbi__FromInt(-1);
	    qr[1] = arbi__add(x, y);
	}
	return;
    }

    q = arbi__FromInt(0);
    j = lx - ly;

    /* r = x DIV (1 << j*WordSize); */
    r = allocate(ly); lr = ly;
    for(i = 0; i < ly; i++)
        r[i] = x[j+i];

    /* useful quantities for estimating the quotient:
     *  yy, the most significant digit of y;
     *  ys, how many bits yy must be shifted to the left so that
     *		2^(WordSize-3) <= abs(yy << ys) < 2^(WordSize-2)
     *  let yz = yy << ys, M = 2^(HalfSize-1);
     *  yh = yz / M, 2^(HalfSize-2) <= abs(yh) < 2^(HalfSize-1)
     *  yl = yz % M, 0 <= yl < M-1
     */
    yy = y[ly-1];
    if(yy == 0) {
    	ys = WordSize-(long)2;
    	z = ((unsigned s_Word)(y[ly-2])) >> 2;
    } else if(yy == -1) {
	ys = WordSize-(long)3;
	z = (((y[ly-2]) >> 1) | ((long)1 << (WordSize-1))) >> 2;
    } else {
	for(ys = (long)-1; (yy<0) == ((yy << (ys+(long)2))<0); ys++);
	if(ys < 0) z = yy >> -ys;
	else if((ly == 1) || (ys == 0)) z = yy << ys;
	else z = (yy << ys) | (    (unsigned s_Word)(y[ly-2])
				>> (unsigned long)(WordSize-ys));
    }
    yh = z >> (HalfSize-1);
    yl = z & (Hmask >> 1);

    while(TRUE) {
    Length_T lq;
    arbi_T g, t, u;
    Word rr, gg, gh, gl;
    long rs, gs;
	lr = Length(r);
        if(cmpa(y, r) > 0) {
	    if(j == 0) { /* we're done */
		/* make sure r is between 0 and y - sign(y) */
		if(((cmp0(r) < 0) != (cmp0(y) < 0)) && (cmp0(r) != 0)) {
		    u = arbi__FromInt(1);
		    t = arbi__sub(q, u); ffree(q); ffree(u); q = t;
		    t = arbi__add(r, y), ffree(r); r = t;
		}
		qr[0] = q;
		qr[1] = r;
 		return;
	    } else { /* shift */
 		j--;
		/* q = (1 << WordSize)*q */
		lq = Length(q);
		t = allocate(lq+1);
		t[0] = 0;
		for(i = 0; i < lq; i++)
		    t[i+1] = q[i];
		ffree(q); q = t; trim(q);

		/* r = (1 << WordSize)*r + x[j]; */
		u = allocate(lr+1);
		u[0] = x[j];
		for(i = 0; i < lr; i++)
		    u[i+1] = r[i];
		ffree(r); r = u; trim(r);
	    }
	} else {
	    /* guess a refinement to the divisor */ 
	    /*  rr, the most significant digit of y;
	     *  rs, how many bits rr must be shifted to the left so that
	     *		2^(WordSize-4) <= abs(rr << rs) < 2^(WordSize-3)
	     *  z = rr << rs;
	     */
	    rr = r[lr-1];
	    if(rr == 0) { /* lr > 1, because Abs(r) > Abs(y) */
		rs = WordSize-(long)3;
		z = ((unsigned s_Word)(r[lr-2])) >> 3;
	    } else if(rr == -1) {
		rs = WordSize-(long)3;
		z = (((r[lr-2]) >> 1) | ((long)1 << (WordSize-1))) >> 2;
	    } else {
		for(rs = (long)-2; (rr<0) == ((rr << (rs+(long)3))<0); rs++);
		if(rs < 0) z = rr >> -rs;
		else if((lr == 1) || (rs == 0)) z = rr << rs;
		else z = (rr << rs) | (    ((unsigned s_Word)(r[lr-2]))
					>> (unsigned long)(WordSize-rs));
	    }
	    gh = z/yh;	/* 2^(HalfSize-3) <= abs(gh) < 2^(HalfSize-1) */
	    gl = (((z % yh)<<(HalfSize-1)) - yl*gh)/yh;
	    		/* abs(gl) <= 2^(HalfSize+1) */
	    gg = gl + (gh << (HalfSize-1));
	    /* gg is an estimate of (rr << (rs + WordSize-2 - ys)) / yy */
	    gs = (WordSize-(long)2) - WordSize*(long)(lr - ly) + (rs - ys);

	    if(gs < 0) { /* shift gg to the left -gs places */
	        z = gg << -gs;
	        if(((z < 0) != (gg < 0)) || (Abs(z) < Abs(gg))) {
	            if(gg < 0) z = MinWord;
	            else z = MaxWord;
		}
	    } else if(gs == 0) z = gg;
	    else { /* shift gg to the right gs places, and round */
	        z = (gg >> gs) + ((gg >> (gs-(long)1)) & 1);
	        if(z == 0) {
	            if((rr < 0) == (yy < 0)) {
			z = 1;
		    } else {
			z = (long)-1;
		    }
		}
	    }
	    g = arbi__FromInt(z);

	    /* r = r - g*y; */
	    t = arbi__mlt(g, y); u = arbi__sub(r, t);
	    ffree(t); ffree(r); r = u;

	    /* q = q + g; */
	    t = arbi__add(q, g); ffree(q); q = t;

	    ffree(g);
	}
    }
}

static arbi_T
arbi__div(arbi_T x, arbi_T y)
{
    Word wx, wy;
    Length_T lx, ly;
    arbi_T a[2];

    arbi__errmsg = NULL;
    lx = Length(x); ly = Length(y);
    if(ly == 1) {
	wy = y[0];
        if(wy == 0) {
            arbi__errmsg = "attempt to divide by zero";
            return(NULL);
	} else if(wy == 1) return(arbi__cpy(x));
	else if(wy == -1) return(arbi__neg(x));
	wx = x[0];
	if((lx == 1) && (wx != MinWord) && (wy != MinWord)) {
	    if((wx >= 0) && (wy >= 0)) { return(arbi__FromInt(wx/wy)); }
	    if((wx <  0) && (wy >= 0)) {
	        if((-wx % wy) == 0) {
		    return(arbi__FromInt(-(-wx/wy)));
		} else {
		    return(arbi__FromInt(-(-wx/wy)-1));
		}
	    }
	    if((wx >= 0) && (wy <  0)) {
	        if((wx % -wy) == 0) {
		    return(arbi__FromInt(-(wx/-wy)));
		} else {
		    return(arbi__FromInt(-(wx/-wy)-1));
		}
	    }
	    if((wx <  0) && (wy <  0)) {
	        if((-wx % -wy) == 0) {
		    return(arbi__FromInt(-wx/-wy));
		} else {
		    return(arbi__FromInt(-wx/-wy));
		}
	    }
	}
    }
    divmod(x, y, a);
    ffree(a[1]);
    return(a[0]);
}

static arbi_T
arbi__mod(arbi_T x, arbi_T y)
{
    Word wx, wy;
    Length_T lx, ly;
    arbi_T a[2];

    arbi__errmsg = NULL;
    lx = Length(x); ly = Length(y);
    if(ly == 1) {
	wy = y[0];
        if(wy == 0)
	    return(arbi__cpy(x));
	else if((wy == 1) ||  (wy == -1))
	    return(arbi__FromInt(0));
	wx = x[0];
	if((lx == 1) && (wx != MinWord) && (wy != MinWord)) {
	    if((wx >= 0) && (wy >= 0)) return(arbi__FromInt(wx%wy));
	    if((wx % wy) == 0) return(arbi__FromInt(0));
	    if((wx <  0) && (wy >= 0))
	        return(arbi__FromInt(wy-(-wx%wy)));
	    if((wx >= 0) && (wy <  0))
	        return(arbi__FromInt(wy+(wx%-wy)));
	    if((wx <  0) && (wy <  0))
	        return(arbi__FromInt(-(-wx%-wy)));
	}
    }
    divmod(x, y, a);
    ffree(a[0]);
    return(a[1]);
}

static arbi_T
arbi__gcd(arbi_T x, arbi_T y)
{
    arbi_T ax, ay, t;
    Word wx, wy;

    arbi__errmsg = NULL;
    ax = arbi__abs(x);
    ay = arbi__abs(y);
    if(Arbi_cmp(ax, ay) < 0) {
        t = ax; ax = ay; ay = t;
    }
    while(cmp0(ay) != 0) {
        if(Length(ax) == 1) {
            wx = ax[0]; wy = ay[0];
            ffree(ax); ffree(ay);
            while(wy) {
                wx = wx % wy;
                if(wx == 0) return(arbi__FromInt(wy));
                wy = wy % wx;
	    }
	    return(arbi__FromInt(wx));
        } else {
            t = arbi__mod(ax, ay);
            ffree(ax); ax = ay; ay = t;
	}
    }
    ffree(ay);
    return(ax);
}

/************************************************************************/
/*									*/
/*	conversion routines:  int <-> arbi_T, char * <-> arbi_T		*/
/*									*/
/************************************************************************/

static arbi_T
arbi__FromInt(long int v)
{
    arbi_T x;

    arbi__errmsg = NULL;
    x = allocate(1);
    *x = v;
    return(x);
}

static arbi_T
arbi__FromString(char *s, int base)
{
    char *t;
    arbi_T b, x, y, z;
    int j;
    static int convert[256];

    arbi__errmsg = NULL;
    if(!*convert) { /* first time, initialize the convert array */
        for(j = 0; j < 256; j++)
            convert[j] = 1000;
	for(j = '0'; j <= '9'; j++)
	    convert[j] = j - '0';
	for(j = 'a'; j <= 'z'; j++)
	    convert[j] = (j - 'a') + 10;
	for(j = 'A'; j <= 'Z'; j++)
	    convert[j] = (j - 'A') + 10;
    }

    /* convert the string */
    if((base < 2) || base > 36) {
	arbi__errmsg = "arbi_FromString: invalid base";
        return(NULL);
    }
    x = arbi__FromInt(0);
    b = arbi__FromInt(base);
    for(t = s; *t; t++) {
	if(convert[(unint) (*t)] >= base) {
	    arbi__errmsg = "arbi_FromString: character not a valid digit";
	    return(NULL);
	}
	y = arbi__mlt(x, b);
	ffree(x);
	z = arbi__FromInt(convert[(unint) (*t)]);
	x = arbi__add(y, z);
	ffree(y); ffree(z);
    }
    ffree(b);
    return(x);
}

static char *
arbi__ToString(arbi_T x, int base)
{
    arbi_T a[2], r, b;
    char bf[40], *buf, *t;
    int i, lgb4, m, lx, rz;
    Word w, b4, bb;
    static char convert[37];

    arbi__errmsg = NULL;
    if(!*convert) { /* first time, initialize the convert array */
        for(i = 0; i < 10; i++)
            convert[i] = '0' + i;
        for(i = 0; i < 26; i++)
            convert[i+10] = 'a' + i;
    }

    lx = Length(x);
    if((lx == 1)  &&  (base == 10 || base ==16)) {
        if(base == 10) { Sprintf(bf, "%ld", x[0]); }
        else { Sprintf(bf, "%lx", x[0]); }
        buf = (char *)arbi_alloc((unsigned int) (strlen(bf) + 1));
        return(strcpy(buf, bf));
    }
    if( (lx == 1) && (x[0] == 0) ) {
        buf = (char *)arbi_alloc(2);
	buf[0] = '0';
	buf[1] = 0;
        return(buf);
    }

    if((base < 2) || base > 36) {
        arbi__errmsg = "arbi_ToString: invalid base";
        return(NULL);
    }

    /* get an upper bound on how many digits we'll print */
    b4 = base*base*base*base;
    for(lgb4 = 4; b4 >> (lgb4+1); lgb4++);
    w = x[lx-1];
    if(w < 0) { w = -w; }
    if( w < 0 ) {
        m = WordSize;
    } else {
        for(m = 0; w >> m; m++);
    }
    m = (4*(m + WordSize*(lx-1)) + lgb4 - 1)/lgb4 + 1;
    if(x[lx-1] < 0) m++;
    buf = (char *)arbi_alloc((unsigned int) m);
    t = buf + m;
    *--t = '\0';

    bb = b4; w = bb * base;
    for(m = 4; w/base == bb; m++) {
        bb = w;
        w *= base;
    }

    b = arbi__FromInt(bb);
    r = arbi__abs(x);
    while(cmp0(r)) {
	divmod(r, b, a);
	ffree(r);
	r = a[0];
	w = a[1][0];
	rz = cmp0(r);
	for(i = 0; (i < m) && (rz || (w != 0)); i++) {
	    *--t = convert[w % base];
	    w /= base;
	}
	ffree(a[1]);
    }
    ffree(r); ffree(b);
    if(x[lx-1] < 0)
        *--t = '-';
    if(t != buf) memmove(buf, t, strlen(t)+1);
    return(buf);
}

/* ------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------ */
/* alloc.c: memory allocation for arbitrary precision integer and rational
 *		arithmetic package
 */

/* Here's the scoop on allocation headers:
 *   When a chunk of memory is allocated, we return a pointer, p, that
 *   points to at least nbytes of memory.  The header precedes p.  Let
 *   p be of type char *, then p[-1] and p[-2] are unused by the allocator
 *   (but the arithmetic routines use this space).  p[-3] gives the size
 *   of the chunk, and p[-4] gives allocation status as follows.  Let
 *   x = p[-4]
 *	x & 1	: collectible:  if this bit is set, then this chunk
 *		  can be garbage collected; otherwise, it must be
 *		  explicitly freed.
 *	x & 2	: If this chunk is collectible, this is the mark bit,
 *		  set during the mark phase of a gc; otherwise, this
 *		  bit is set to indicate that the chunk is in use, and
 *		  cleared when the block is freed.
 *   An invariant maintained by the allocator is
 *	(x == 0) == the chunk is on a free list
 *
 *   This code assumes sizeof(void *) >= 4, and sizeof(char) = 1.
 */


static unsigned int
NbytesToIndex(unsigned int nb)
{
    unsigned int nw, e;

    /* convert size from bytes to words, sizeof(word) = sizeof(void *) */
    nw = (nb + sizeof(void *) - 1) / sizeof(void *);
    if(nw == 0)
        nw = 1;

    /* figure out the corresponding free table index */
    if(nw <= 8) return(nw);
    for(e = 0; (nw >> e) >= 8; e++);
    return(((nw >> e) & 3) + ((e+1) << 2) + ((nw & ~(~((unsigned long) 0) << e)) != 0));
}

static unsigned int
IndexToNbytes(unsigned int ix)
{
    unsigned int nw;
    if(ix <= 8) nw = ix;
    else nw = (sizeof(void *)+(ix & 3)) << ((ix >> 2)-1);
    return(nw*sizeof(void *));
}

static void
AddBlock(void)
{
    int i;

    if(BlockIndex >= MaxBlock)
	arbi_die("alloc", "AddBlock", "out of blocks");
    for(i = 0; i <= BlockIndex; i++) {
        if(blocks[i] == NULL) {
            blocks[i] = (char *)WAMalloc(BlockSize);
            if(blocks[i] == NULL)
                arbi_die("alloc", "AddBlock", "out of memory");
	}
    }
}

static void *
arbi_alloc(unsigned int nbytes)
{
    int indx;
    void *p;
    char *t;
    unsigned int space_left, nb;

    /* First, try to get the chunk from the appropriate free list. */
    indx = NbytesToIndex(nbytes);
    p = (void *)(FreeList[indx]);
    if(p != NULL) {
        FreeList[indx] = *((void **)p);
        t = (char *)p;
        t[-4] = 2;
        return(p);
    }

    /* Is the chunk bigger than the blocks we grab from Malloc? */
    nb = IndexToNbytes(indx); /* nbytes <= nb <= 10/9 * nbytes */
    if(nb >= BlockSize) {
        /* I don't want to deal with this yet */
        arbi_die("alloc", "arbi_alloc", "nbytes too big");
    }

    /* Is the current block is big enough to satisfy the current request? */
    if(BlockIndex < 0) space_left = 0;
    else space_left = BlockSize - (BlockPos - (blocks[BlockIndex]));
    if(space_left < nb + sizeof(void *)) { /* allocate another block */
        ++BlockIndex;
        AddBlock();
	BlockPos = blocks[BlockIndex] + sizeof(void *);
    }

    /* Allocate the chunk from the current block. */
    t = BlockPos;
    t[-3] = indx;
    t[-4] = 2;
    BlockPos += nb + sizeof(void *);
    BlockPos[-4] = 0;
    BlockPos[-3] = NbytesToIndex(BlockSize);
    return((void *)t);
}

static void
arbi_free(void *p)
{
    char *t;

    t = (char *)p;
    if(!((t[-4] == 1) || t[-4] == 2))
        arbi_die("alloc", "arbi_free", "bad chunk");
    t[-4] = 0;
    *((void **)p) = FreeList[(int)(t[-3])];
    FreeList[(int) (t[-3])] = p;
}

static void *
arbi_collect(void *p)
{
    char *t;

    if(p == NULL)
	return(p);
    t = (char *)p;
    if(t[-4] != 2)
        arbi_die("alloc", "arbi_collect", "bad chunk");
    t[-4] = 1;
    return(p);
}

/* ------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------ */
static void
arbi_die(char *source, char *fn, char *msg)
{
    Eprintf(
      "INTERNAL ERROR in arb. precision arithmetic package\n%s (in %s.c): %s\n",
       fn, source, msg
    );
}

static char *
WAMalloc(unint n)
{
    return( (char *) Malloc(n) );
}

