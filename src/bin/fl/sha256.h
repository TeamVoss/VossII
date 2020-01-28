/********************************************************************
*                                                                   *
*********************************************************************/
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct SHA256_rec	*SHA256_ptr;

/* ----- Function prototypes for public functions ----- */
void	    Init_SHA256();
SHA256_ptr  Begin_SHA256();
void	    SHA_printf(SHA256_ptr ctx, const string format, ...);
string	    Get_SHA256_hash(SHA256_ptr ctx);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef SHA256_H
#define SHA256_H
#include "fl.h" /* Global data types and include files               */

/* Length of a SHA256 hash */
#define SHA256_LEN	32

/* SHA256 context. */
typedef struct SHA256_rec {
    bool		in_use;	    // Active or not
    unsigned long int	h[8];	    // state; this is in the CPU native format
    unsigned long	Nl;	    // number of bits processed so far
    unsigned long	Nh;	    // number of bits processed so far
    unsigned		num;	    // number of bytes within the below buffer
    unsigned char	data[64];   // Input buffer in byte vector format
} SHA256_rec;

#endif /* SHA256_H */
#endif /* EXPORT_FORWARD_DECL */
