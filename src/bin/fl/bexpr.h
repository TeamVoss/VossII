/********************************************************************
*                                                                   *
*     Copyright (C) 2016 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* bexpr.h -- header for bexpr.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void	    BE_Init();
void	    Bexpr_Install_Functions();
bexpr	    BE_One();
bexpr	    BE_Zero();
bexpr	    BE_Var(string name);
void	    BE_Mark(bexpr f);
void	    BE_Clean();
bexpr	    BE_Not(bexpr f1);
bexpr	    BE_Nor(bexpr f1, bexpr f2);
bexpr	    BE_Nand(bexpr f1, bexpr f2);
bexpr	    BE_And(bexpr f1, bexpr f2);
bexpr	    BE_Ite(bexpr c, bexpr t, bexpr e);
bexpr	    BE_Or(bexpr f1, bexpr f2);
bexpr	    BE_Xor(bexpr f1, bexpr f2);
bexpr	    BE_Xnor(bexpr f1, bexpr f2);
bool	    BE_Equal(bexpr f1, bexpr f2);
bool	    BE_NEQ(bexpr f1, bexpr f2);
void	    BE_Print(odests fp, bexpr f);
void	    BE_HL_Print(odests fp, bexpr H, bexpr L);
void	    Reset_BE_Subs();
subst_ptr   Add_to_BE_Subs(subst_ptr start, bexpr v, bexpr e);
bexpr	    BE_Substitute(bexpr f, subst_ptr subs);
bool	    Save_bexprs(string filename, buffer *roots);
bool	    Load_bexprs(string filename, buffer *results);
#ifdef DEBUG
int	    Dbg_stop();
#endif /* DEBUG */

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef BEXPR_H
#define BEXPR_H
#include "fl.h"	/* Global data types and include files 		     */

typedef enum {
    BE_FREE  = 0,
    BE_VAR  = 1,
    BE_AND  = 2,
    BE_ONE = 3
} be_type;

#define NBR_SIGNATURES	4

typedef struct bexpr_rec {
    be_type	    type:2;
    ui		    mark:1;
    ui		    has_bdd:1;
    ui		    height:28;
    int		    sat_idx:32;
    formula	    bdd;
    bexpr	    next;
    union {
	bexpr	    be;
	string	    name;
	  }	    l;
    bexpr	    r;
    ui		    sig[NBR_SIGNATURES];
    bexpr	    same_sig_list;
} bexpr_rec;


#define BE_GET_MARK(p)	    ((p)->mark)
#define BE_SET_MARK(p,v)    (p)->mark = (v)
#define BE_GET_TYPE(p)	    ((p)->type)
#define BE_SET_TYPE(p,t)    (p)->type = (t)
#define BE_IS_VAR(p)	    ((p)->type == BE_VAR)
#define BE_IS_AND(p)	    ((p)->type == BE_AND)
#define BE_NOT(p)	    ((bexpr) (((ui)(p)) ^ 0x4))
#define BE_POS(p)	    ((bexpr) (((ui)(p)) & ~0x4))
#define BE_IS_NEG(p)	    ((((ui)(p)) & 0x4) != 0)
#define BE_GET_VAR(p)	    ((p)->l.name)
#define BE_SET_VAR(p,v)	    (p)->l.name = (v)

#define BE_IS_TRUE(p)	    ((p) == BE_TRUE)
#define BE_IS_FALSE(p)	    ((p) == BE_FALSE)
#define BE_IS_CONSTANT(p)   (BE_IS_TRUE(p) || BE_IS_FALSE(p))

#define BE_GET_LEFT(p)	    ((p)->l.be)
#define BE_SET_LEFT(p,c)    (p)->l.be = (c)
#define BE_GET_RIGHT(p)	    ((p)->r)
#define BE_SET_RIGHT(p,c)   (p)->r = (c)

#endif /* BEXPR_H */
#endif /* EXPORT_FORWARD_DECL */
