//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1993                        *
*                                                                   *
*********************************************************************/
/* new_bdd.h -- header for new_bdd.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef unint				formula;
extern formula 				ZERO, ONE;
typedef struct subst_rec		*subst_ptr;
typedef struct relprod_cache_rec	*relprod_cache_ptr;
typedef struct bdd_rec			*bdd_ptr;

#define PTR2FORMULA(p) ((formula) ((lunint)(p)))
#define FORMULA2PTR(p) ((pointer) ((lunint)(p)))


#if 0
#define REF_CNT_DEBUG	1
#endif

/* ----- Function prototypes for public functions ----- */

#ifdef DEBUG
string		get_var_name(formula f);
bdd_ptr		f_GET_BDDP(formula f);
int		f_ISNOT(formula f);
formula		f_POS(formula f);
int		f_BDD_GET_VAR(bdd_ptr f);
formula		f_GET_LSON(bdd_ptr f);
formula		f_GET_RSON(bdd_ptr f);
#endif

void		B_Init(void);
void		PUSH_BDD_GC(formula f);
void		POP_BDD_GC(unint cnt);
void		Get_top_cofactor(formula b, string *varp,
				 formula *Hp, formula *Lp);
void		BDD_Install_Functions();
formula		B_One(void);
formula 	B_Zero(void);
formula 	B_Var(char *);
formula 	Rand_Var(char *);
formula 	B_New_unique_var(formula);
formula 	B_Not(formula);
formula 	B_Nor(formula, formula);
formula 	B_Nand(formula, formula);
formula 	B_And(formula, formula);
formula 	B_Or(formula, formula);
formula 	B_Xor(formula, formula);
formula 	B_Xnor(formula, formula);
formula		B_Ite(formula i, formula t, formula e);
void		F_not(formula op1, formula op2, formula *res);
void		F_and(formula op1, formula op2, formula *res);
void		F_or(formula op1, formula op2, formula *res);
formula		B_forall_last(formula, formula);
formula		B_forall(formula, formula);
formula		B_thereis(formula, formula);
formula		B_Depends(formula);
string		B_DependencyList(formula);
int 		B_Equal(formula, formula);
void 		B_Print(odests, formula, int);
void		BP(formula f);
void 		HL_Print(odests, formula, formula);
void		HL_Pattern(odests fp, formula H, formula L);
void		B_Mark(formula);
void		Reset_Ref_cnts();
void 		B_Clean(void);
void		BDD_Sanity(formula f1);
string  	Get_Var_Name(unint i);
unint     	Get_VarCnt();
int		B_Width(unint i);
void    	Reset_BDD_Size();
void    	B_Size(formula f);
void		End_RelProd();
void		Begin_RelProd();
formula		Rel_prod(formula qv, formula a, formula b, bool existential);
void		New_ordering();
void		Add_ordering_var(string var);
void            Reorder(int times);
bool		Save_BDDs(string filename, buffer *roots);
bool		Load_BDDs(string filename, buffer *results);
g_ptr		End_ordering();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef NEW_BDD_H
#define NEW_BDD_H
#include "fl.h"	/* Global data types and include files 		     */


typedef struct var_rec		*var_ptr;

/* Bdd package parameters: Bdd node size = 4 words */
#define AND			1
#define OR			2
#define XOR			3
#define UNIVERSAL		4
#define EXISTENTIAL		5
#define DEPENDS			6

#define CACHE_OP_SZ		3
#define LG_BDD_SIZE1		32
#define LG_BDD_SIZE		(LG_BDD_SIZE1-1)
#define VAR_SZ			18
#ifdef REF_CNT_DEBUG
#define REF_CNT_SIZE		(32-VAR_SZ-3)
#else
#define REF_CNT_SIZE		(32-VAR_SZ-2)
#endif
#define MAX_RECS		((ui) (((ui) 1) << ((ui) LG_BDD_SIZE)))
#define MAX_VARS		(1 << VAR_SZ)
#define BDD_MAX_REF_CNT		((1 << REF_CNT_SIZE)-1)
#define LOAD_FACTOR		4			/* Avg chain = 4     */
#define MIN_PERCENT_GC_INCR	50
#define CACHE_LOAD_FACTOR	8
#define DYN_VAR_RED		2

typedef struct bdd_rec {
	/* Word 1 */
#ifdef REF_CNT_DEBUG
	unint		in_use:1;
#endif
	unint		mark:1;
	unint		moving:1;		/* For the swap algorithm */
	unint		ref_cnt:REF_CNT_SIZE;
	unint		var:VAR_SZ;
	/* Word 2 */
	unint		next;
	/* Word 3 */
	unint		lson:LG_BDD_SIZE;
	/* Word 4 */
	unint		rson:LG_BDD_SIZE1;
} bdd_rec;

typedef struct var_rec {
	unint		done:1;		/* Variable number		 */
	unint		variable:31;	/* Variable number		 */
	string		var_name;	/* Variable name		 */
	formula		*uniq_tbl;	/* Hash table for unique table	 */
	unint		sz_uniq_tbl;	/* Size of unique table		 */
	unint		nbr_uniq_tbl;	/* Elements in unique table	 */
} var_rec;




typedef struct {
	formula	lson;
	formula	rson;
	unint	fn:CACHE_OP_SZ;		/* NOR/XOR/UNIV/EXIST/DEPENDS */
	formula	res:LG_BDD_SIZE1;
} cache_rec, *cache_ptr;

typedef enum {sop_format, infix_format, tree_format}	print_types;
#define MAX_PROD_SIZE	4000	/* Max # characters to print in SOP */

/* ------------------------  Macros ------------ ------------  */
#define LNULL				(MAX_RECS-1)
#define ISNOT(bid)			((bid) & (1<<LG_BDD_SIZE))
#define NOT(bid)			((bid) ^ (1<<LG_BDD_SIZE))
#define POS(bid)			((bid) & (((ui)(1<<LG_BDD_SIZE))-1))

#if 0
#define GET_BDDP(f)			((POS(f)>sz_MainTbl)?NULL:(MainTbl+(POS(f))))
#define FGET_BDDP(f)			(((f)>sz_MainTbl)?NULL:(MainTbl+((f))))
#else 
#define GET_BDDP(f)			(MainTbl+(POS(f)))
#define FGET_BDDP(f)			(MainTbl+((f)))
#endif
#define BDD_GET_VAR(bp)			((bp)->var)
#define BDD_SET_VAR(bp,v)		(bp)->var = (v);
#define GET_LSON(bp)			((bp)->lson)
#define GET_RSON(bp)			((bp)->rson)
#define GET_NEXT(bp)			((bp)->next)

#define ORDER(var)			((VarTbl+(var))->variable)

#define CACHE_FN(f,g,op)	(((((op)<<4)*(f)*(g))>>1) % sz_Cache)
#define REHASH(h,f,g,op)	((((h)+((f)^(g)))>>1) % sz_Cache)

#ifdef  DEBUGF
#define D_PR_F(st,expr)	{					\
				fprintf(stderr,"%s ", st);	\
				bdd_tree_print(stderr,expr);	\
				fprintf(stderr, "\n");		\
			}
#define BDD_RETURN(expr)    {						\
				formula t = expr;			\
				fprintf(stderr, "---(%d)", __LINE__);	\
				D_PR_F("------->",t);			\
				return(t);				\
			    }

#define ENTERING_FN(st,f,g) {						     \
				fprintf(stderr, "vvvvvvv %s vvvvvvv\n", st); \
				D_PR_F("Arg1: ", f1);			     \
				D_PR_F("Arg2: ", f2);			     \
			    }


#else /*DEBUGF */
#define D_PR_F(st,expr)
#define BDD_RETURN(expr)	{formula t = expr; return(t);}
#define ENTERING_FN(st,f,g)
#endif /* DEBUGF */

typedef struct subst_rec {
        formula         var;
        formula         expr;
        subst_ptr       next;
} subst_rec;

typedef struct subst_cache_rec {
        formula         f;
        subst_ptr       subs;
        formula         result;
} subst_cache_rec, *subst_cache_ptr;


typedef struct relprod_cache_rec {
        formula         v;
        formula         a;
        formula         b;
        formula         result;
} relprod_cache_rec;

#define B_TRUE              (ONE)
#define B_FALSE             (ZERO)
#define B_IS_TRUE(p)        ((p) == ONE)
#define B_IS_FALSE(p)       ((p) == ZERO)
#define B_IS_CONSTANT(p)    (B_IS_TRUE(p) || B_IS_FALSE(p))


#endif /* NEW_BDD_H */
#endif /* EXPORT_FORWARD_DECL */
