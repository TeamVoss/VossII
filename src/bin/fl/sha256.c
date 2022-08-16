//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*                                                                      */
/* Derived from public domain SHA-256 implementation (see below)	*/
/*                                                                      */
/*									*/
/* SHA-256								*/
/* Implementation derived from LibTomCrypt (Tom St Denis)		*/
/*									*/
/* LibTomCrypt is a library that provides various cryptographic		*/
/* algorithms in a highly modular and flexible manner.			*/
/*									*/
/* The library is free for all purposes without any express		*/
/* guarantee it works.							*/
/*									*/
/* Tom St Denis, tomstdenis@gmail.com, http://libtomcrypt.org		*/
/*									*/
/************************************************************************/
#include "sha256.h"
#include "graph.h"
#include "symbol.h"

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr	    strings;
extern buffer	    ext_obj_buf;
extern jmp_buf      *start_envp;
extern char	    FailBuf[4096];

/***** PRIVATE VARIABLES *****/
static int	    initialized = 0;
static rec_mgr	    sha256_rec_mgr;

#define SHA256_FINALCOUNT_SIZE  8
#define SHA256_K_SIZE	        64
static const unsigned long K[SHA256_K_SIZE] = {
    0x428a2f98UL, 0x71374491UL, 0xb5c0fbcfUL, 0xe9b5dba5UL, 0x3956c25bUL,
    0x59f111f1UL, 0x923f82a4UL, 0xab1c5ed5UL, 0xd807aa98UL, 0x12835b01UL,
    0x243185beUL, 0x550c7dc3UL, 0x72be5d74UL, 0x80deb1feUL, 0x9bdc06a7UL,
    0xc19bf174UL, 0xe49b69c1UL, 0xefbe4786UL, 0x0fc19dc6UL, 0x240ca1ccUL,
    0x2de92c6fUL, 0x4a7484aaUL, 0x5cb0a9dcUL, 0x76f988daUL, 0x983e5152UL,
    0xa831c66dUL, 0xb00327c8UL, 0xbf597fc7UL, 0xc6e00bf3UL, 0xd5a79147UL,
    0x06ca6351UL, 0x14292967UL, 0x27b70a85UL, 0x2e1b2138UL, 0x4d2c6dfcUL,
    0x53380d13UL, 0x650a7354UL, 0x766a0abbUL, 0x81c2c92eUL, 0x92722c85UL,
    0xa2bfe8a1UL, 0xa81a664bUL, 0xc24b8b70UL, 0xc76c51a3UL, 0xd192e819UL,
    0xd6990624UL, 0xf40e3585UL, 0x106aa070UL, 0x19a4c116UL, 0x1e376c08UL,
    0x2748774cUL, 0x34b0bcb5UL, 0x391c0cb3UL, 0x4ed8aa4aUL, 0x5b9cca4fUL,
    0x682e6ff3UL, 0x748f82eeUL, 0x78a5636fUL, 0x84c87814UL, 0x8cc70208UL,
    0x90befffaUL, 0xa4506cebUL, 0xbef9a3f7UL, 0xc67178f2UL
};

/********************************************************/
/*                    LOCAL FUNCTIONS                   */
/********************************************************/

static void
put_bigendian( void *target, unsigned long long value, size_t bytes )
{
    unsigned char *b = target;
    int i;
    for (i = bytes-1; i >= 0; i--) {
        b[i] = value & 0xff;
        value >>= 8;
    }
}
    
static unsigned long long
get_bigendian( const void *target, size_t bytes )
{
    const unsigned char *b = target;
    unsigned long long result = 0;
    unint i;
    for(i = 0; i < bytes; i++) {
        result = 256 * result + (b[i] & 0xff);
    }
    return result;
}

/* Various logical functions */

#define Ch(x,y,z)       (z ^ (x & (y ^ z)))
#define Maj(x,y,z)      (((x | y) & z) | (x & y)) 

static unsigned long
Sigma0(unsigned long x)
{
    unsigned long mx = x&0xFFFFFFFFUL;
    return (
	(((mx>>2)|(mx<<30))^((mx>>13)|(mx<<19))^ ((mx>>22)|(mx<<10))
	) & 0xFFFFFFFFUL
    );
}

static unsigned long
Sigma1(unsigned long x)
{
    unsigned long mx = x&0xFFFFFFFFUL;
    return (
	(((mx>>6)|(mx<<26))^((mx>>11)|(mx<<21))^ ((mx>>25)|(mx<<7))
	) & 0xFFFFFFFFUL
    );
}

static unsigned long
Gamma0(unsigned long x)
{
    unsigned long mx = x&0xFFFFFFFFUL;
    return (
	(((mx>>7)|(mx<<25))^((mx>>18)|(mx<<14))^(mx>>3)
	) & 0xFFFFFFFFUL
    );
}

static unsigned long
Gamma1(unsigned long x)
{
    unsigned long mx = x&0xFFFFFFFFUL;
    return (
	(((mx>>17)|(mx<<15))^((mx>>19)|(mx<<13))^(mx>>10)
	) & 0xFFFFFFFFUL
    );
}

static void
sha256_compress (SHA256_ptr ctx, const void *buf)
{
    unsigned long S0, S1, S2, S3, S4, S5, S6, S7, W[SHA256_K_SIZE], t0, t1, t;
    int i;
    const unsigned char *p;

    /* copy state into S */
    S0 = ctx->h[0];
    S1 = ctx->h[1];
    S2 = ctx->h[2];
    S3 = ctx->h[3];
    S4 = ctx->h[4];
    S5 = ctx->h[5];
    S6 = ctx->h[6];
    S7 = ctx->h[7];

    /*
     * We've been asked to perform the hash computation on this 512-bit string.
     * SHA256 interprets that as an array of 16 bigendian 32 bit numbers; copy
     * it, and convert it into 16 unsigned long's of the CPU's native format
     */
    p = buf;
    for (i=0; i<16; i++) {
        W[i] = get_bigendian( p, 4 );
        p += 4;
    }

    /* fill W[16..63] */
    for (i = 16; i < SHA256_K_SIZE; i++) {
        W[i] = Gamma1(W[i - 2]) + W[i - 7] + Gamma0(W[i - 15]) + W[i - 16];
    }        

    /* Compress */
#define RND(a,b,c,d,e,f,g,h,i)                         \
     t0 = h + Sigma1(e) + Ch(e, f, g) + K[i] + W[i];   \
     t1 = Sigma0(a) + Maj(a, b, c);                    \
     d += t0;                                          \
     h  = t0 + t1;

     for (i = 0; i < SHA256_K_SIZE; ++i) {
         RND(S0,S1,S2,S3,S4,S5,S6,S7,i);
         t = S7; S7 = S6; S6 = S5; S5 = S4; 
         S4 = S3; S3 = S2; S2 = S1; S1 = S0; S0 = t;
     }
#undef RND     
 
    /* feedback */
    ctx->h[0] += S0;
    ctx->h[1] += S1;
    ctx->h[2] += S2;
    ctx->h[3] += S3;
    ctx->h[4] += S4;
    ctx->h[5] += S5;
    ctx->h[6] += S6;
    ctx->h[7] += S7;
}

static void
SHA256_init (SHA256_ptr ctx)
{
    ctx->Nl = 0;
    ctx->Nh = 0;
    ctx->num = 0;
    ctx->h[0] = 0x6A09E667UL;
    ctx->h[1] = 0xBB67AE85UL;
    ctx->h[2] = 0x3C6EF372UL;
    ctx->h[3] = 0xA54FF53AUL;
    ctx->h[4] = 0x510E527FUL;
    ctx->h[5] = 0x9B05688CUL;
    ctx->h[6] = 0x1F83D9ABUL;
    ctx->h[7] = 0x5BE0CD19UL;
}

static void
SHA256_update (SHA256_ptr ctx, const void *src, unsigned int count)
{
    unsigned new_count = (ctx->Nl + (count << 3)) & 0xffffffff;
    if (new_count < ctx->Nl) {
        ctx->Nh += 1;
    }
    ctx->Nl = new_count;

    while (count) {
        unsigned int this_step = 64 - ctx->num;
        if (this_step > count) this_step = count;
        memcpy( ctx->data + ctx->num, src, this_step);

        if (this_step + ctx->num < 64) {
            ctx->num += this_step;
            break;
        }

        src = (const unsigned char *)src + this_step;
        count -= this_step;
        ctx->num = 0;

        sha256_compress( ctx, ctx->data );
    }
}

/*
 * Add padding and return the message digest.
 */
static void
SHA256_final (unsigned char *digest, SHA256_ptr ctx)
{
    unsigned int i;
    unsigned char finalcount[SHA256_FINALCOUNT_SIZE];

    put_bigendian( &finalcount[0], ctx->Nh, 4 );
    put_bigendian( &finalcount[4], ctx->Nl, 4 );

    SHA256_update(ctx, "\200", 1);

    if (ctx->num > 56) {
        SHA256_update(ctx, "\0\0\0\0\0\0\0\0", 8);
    }
    memset( ctx->data + ctx->num, 0, 56 - ctx->num );
    ctx->num = 56;
    /* Should cause a sha256_compress() */
    SHA256_update(ctx, finalcount, SHA256_FINALCOUNT_SIZE);

    /*
     * The final state is an array of unsigned long's; place them as a series
     * of bigendian 4-byte words onto the output
     */ 
    for (i=0; i<8; i++) {
        put_bigendian( digest + 4*i, ctx->h[i], 4 );
    }
}

static void
sha256_signature(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    SHA256_ptr sha = Begin_SHA256();
    hash_record  g_tbl;
    create_hash(&g_tbl, 1000, ptr_hash, ptr_equ);
    int	g_cnt = 1;
    jmp_buf	tc_start_env;
    jmp_buf	*old_start_envp = start_envp;
    start_envp = &tc_start_env;
    if( setjmp(*start_envp) == 0 ) {
	SHA256_traverse_graph(&g_cnt, &g_tbl, sha, r);
	string sig = Get_SHA256_hash(sha);
	MAKE_REDEX_STRING(redex, sig);
    } else {
	MAKE_REDEX_FAILURE(redex, FailBuf);
    }
    End_SHA256(sha);
    start_envp = old_start_envp;
    dispose_hash(&g_tbl, NULLFCN);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

/********************************************************/
/*                    PUBLIC FUNCTIONS                  */
/********************************************************/

void
Init_SHA256()
{
    new_mgr(&sha256_rec_mgr, sizeof(SHA256_rec));
    initialized = 1;
}

void
SHA256_Install_Functions()
{
    typeExp_ptr tv = GLnew_tVar();

    Add_ExtAPI_Function("sha256_signature", "1", FALSE,
			GLmake_arrow(tv, GLmake_string()),
                        sha256_signature);
}

SHA256_ptr
Begin_SHA256()
{
    ASSERT( initialized == 1 );
    SHA256_ptr ctx = new_rec(&sha256_rec_mgr);
    ctx->in_use = TRUE;
    SHA256_init (ctx);
    return ctx;
}

void
End_SHA256(SHA256_ptr ctx)
{
    ctx->in_use = FALSE;
    free_rec(&sha256_rec_mgr, ctx);
}

string
Get_SHA256_signature(g_ptr node)
{
    SHA256_ptr sha = Begin_SHA256();
    hash_record  g_tbl;
    create_hash(&g_tbl, 1000, ptr_hash, ptr_equ);
    int	g_cnt = 1;
    jmp_buf	tc_start_env;
    jmp_buf	*old_start_envp = start_envp;
    start_envp = &tc_start_env;
    string sig;
    if( setjmp(*start_envp) == 0 ) {
	SHA256_traverse_graph(&g_cnt, &g_tbl, sha, node);
	sig = Get_SHA256_hash(sha);
    } else {
	char buf[10];
	sprintf(buf, "%p", node);
	sig = wastrsave(&strings, buf);
    }
    End_SHA256(sha);
    start_envp = old_start_envp;
    dispose_hash(&g_tbl, NULLFCN);
    return sig;
}



#define SHA_BUF_SIZE	    4096
static char fbuf[SHA_BUF_SIZE];

static void
do_printf(SHA256_ptr ctx, const string format, va_list arg)
{
    vsnprintf(fbuf, SHA_BUF_SIZE, format, arg);
    SHA256_update(ctx, fbuf, (unsigned int) strlen(fbuf));
}


void
SHA256_printf(SHA256_ptr ctx, const string format, ...)
{
    ASSERT( initialized == 1 );
    ASSERT( ctx->in_use == 1 );
    va_list arg;
    va_start(arg, format);
    do_printf(ctx, format, arg);
    va_end(arg);
}

string
Get_SHA256_hash(SHA256_ptr ctx)
{
    ASSERT( initialized == 1 );
    ASSERT( ctx->in_use == 1 );
    unsigned char res[33];
    res[32] = 0;
    SHA256_final(res, ctx);
    ctx->in_use = FALSE;
    string sig = strtemp("");
    for(int i = 0; i < 32; i++) {
	char buf[10];
        Sprintf(buf, "%x%x", res[i]/16, res[i]%16);
	strappend(buf);
    }
    return( wastrsave(&strings, sig) );
}

int
SHA256_traverse_graph(int *g_cntp, hash_record *g_tblp,
		      SHA256_ptr sha, g_ptr node)
{
    int res, lres, rres;
    string s;
    if( node == NULL ) return 1;
    if( (res = PTR2INT(find_hash(g_tblp, node))) != 0 ) {
	return res;
    }
    res = *g_cntp;
    *g_cntp = res+1;
    insert_hash(g_tblp, node, INT2PTR(res));
    if( IS_NIL(node) ) {
	SHA256_printf(sha, "%d=[]\n", res);
	return res;
    }
    switch( GET_TYPE(node) ) {
	case LAMBDA_ND: 
	    rres = SHA256_traverse_graph(g_cntp, g_tblp, sha,
					 GET_LAMBDA_BODY(node));
	    SHA256_printf(sha, "%d=\\%s.%d\n", res, GET_LAMBDA_VAR(node), rres);
	    return res;
	case APPLY_ND:
	    lres = SHA256_traverse_graph(g_cntp, g_tblp, sha,
					 GET_APPLY_LEFT(node));
	    rres = SHA256_traverse_graph(g_cntp, g_tblp, sha,
					 GET_APPLY_RIGHT(node));
	    SHA256_printf(sha, "%d=%d@%d\n", res, lres, rres);
	    return res;
	case CONS_ND: 
	    lres = SHA256_traverse_graph(g_cntp,g_tblp,sha, GET_CONS_HD(node));
	    rres = SHA256_traverse_graph(g_cntp,g_tblp,sha, GET_CONS_TL(node));
	    SHA256_printf(sha, "%d=%d:%d\n", res, lres, rres);
	    return res;
	case LEAF:
	    switch( GET_LEAF_TYPE(node) ) {
		case INT:
		    s = Arbi_ToString(GET_AINT(node),10);
		    SHA256_printf(sha, "%d=I%s\n", res, s);
		    return res;
		case STRING:
		    s = GET_STRING(node);
		    SHA256_printf(sha, "%d=\"%s\"\n", res, s);
		    return res;
		case BOOL:
		    lres = SHA256_bdd(g_cntp, g_tblp, sha, GET_BOOL(node));
		    SHA256_printf(sha, "%d=B %d\n", res, lres);
		    return res;
		case BEXPR:
		    lres = SHA256_bexpr(g_cntp, g_tblp, sha, GET_BEXPR(node));
		    SHA256_printf(sha, "%d=BE %d\n", res, lres);
		    return res;
		case PRIM_FN:
		    {
			switch ( GET_PRIM_FN(node) ) {
			    case P_EXTAPI_FN:
				SHA256_printf(sha, "%d=EFN(%s)\n",
						res,
						Get_ExtAPI_Function_Name(
							GET_EXTAPI_FN(node)));
				return res;
			    case P_SSCANF:
				SHA256_printf(sha, "%d=sscanf \\\"%s\\\"\n)",
						   res,GET_PRINTF_STRING(node));
				return res;
			    case P_PRINTF:
				SHA256_printf(sha, "%d=printf \\\"%s\\\"\n)",
						   res,GET_PRINTF_STRING(node));
				return res;
			    case P_SPRINTF:
				SHA256_printf(sha, "%d=sprintf \\\"%s\\\"\n)",
						   res,GET_PRINTF_STRING(node));
				return res;
			    case P_EPRINTF:
				SHA256_printf(sha, "%d=eprintf \\\"%s\\\"\n)",
						   res,GET_PRINTF_STRING(node));
				return res;
			    case P_FPRINTF:
				SHA256_printf(sha, "%d=fprintf \\\"%s\\\"\n)",
						   res,GET_PRINTF_STRING(node));
				return res;
			    case P_REF_VAR: {
				int r = GET_REF_VAR(node);
				lres = SHA256_traverse_graph(g_cntp, g_tblp,
							     sha,Get_RefVar(r));
				SHA256_printf(sha, "%d=REF %d\n", res, r);
				return res;
			    }
			    default:
				SHA256_printf(sha, "%d=PFN %s\n", res, 
						Get_pfn_name(node, FALSE));
				return res;
			}
		    }
		case EXT_OBJ:
		    {
                        unint class = GET_EXT_OBJ_CLASS(node);
                        ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
			if( op->sha256_fn != NULL ) {
			    op->sha256_fn(g_cntp,g_tblp,sha,GET_EXT_OBJ(node));
			    SHA256_printf(sha, "%d=EO\n", res);
			} else {
			    Wprintf("SHA256 signature for %s is not accurate\n",
				    Get_ExtAPI_Object_name(class));
			    SHA256_printf(sha, "%d=EOp %p\n",
						res, GET_EXT_OBJ(node));
			}
			return res;
		    }
		case VAR:
		    s = GET_VAR(node);
		    SHA256_printf(sha, "%d=VAR %s\n", res, s);
		    return res;
		case USERDEF:
		{
		    fn_ptr fn = GET_USERDEF(node);
		    SHA256_printf(sha, "%d=UD %s\n", res, fn->signature);
		    return res;
		}
		default:
		    DIE("Unknown LEAF type");
	    }
	default:
	    DIE("Should never happen");
    }
}
