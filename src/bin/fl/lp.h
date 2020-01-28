/********************************************************************
*                                                                   *
*     Copyright (C) 1995 Carl-Johan Seger                           *
*                                                                   *
*********************************************************************/
/* lp.h -- header for lp.c */

#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* -------- Function prototypes for exported functions -------- */
void	    LP_Init();
void	    LP_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef LP_H
#define LP_H
#include "fl.h"	/* Global data types and include files 		     */

typedef struct Rational{
    arbi_T n;              /* numerator   */
    arbi_T d;              /* denominator */
} *Rational;

typedef struct {
    Rational *cols;	/* the columns of this row */
    int rel;		    /* the relation of this row */
} Row;

typedef struct {
    int nrow, ncol;  /* number of rows and columns (m and n)*/
    int eps;         /* column with epsilon for strict inequalities */
    Row *rows;       /* a tableau is an array of rows 		    */
    int ur, uc;      /* ur rows unresolved, uc columns unresolved   */
    Row *unresolved; /* list of unresolved constraints		    */
    int *varcols;    /* column numbers corresponding to decision variables */
} Tableau;

#define COST		0x10
#define arbi_LESS 	-1
#define arbi_EQ    	0
#define arbi_GREAT 	1
#define EQ      	1
#define LT      	2
#define GT      	4
#define GE      	(GT | EQ)
#define LE      	(LT | EQ)
#define NE      	(LT | GT)

#endif /* LP_H */
#endif /* EXPORT_FORWARD_DECL */

