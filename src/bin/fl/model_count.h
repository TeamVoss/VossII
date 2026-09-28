//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2026                        *
*                                                                   *
*********************************************************************/
/* model_count.h -- header for model_count.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */

/* ----- Function prototypes for public functions ----- */
void		    Model_count_Install_Functions();
void		    Model_Count_GC();
void		    Create_gen_mc_cache(unint sz);
void		    Free_gen_mc_cache();
void		    Create_cond_gen_mc_cache(unint sz);
void		    Free_cond_gen_mc_cache();
g_ptr		    Gen_model_count_rec(formula vs, hash_record *var_tblp,
					formula fun);
g_ptr		    Gen_cond_model_count_rec(formula vs, hash_record *var_tblp,
					     formula c, formula f);


#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef MODEL_COUNT_H
#define MODEL_COUNT_H
#include "fl.h" /* Global data types and include files               */

typedef struct fp_truth_cov_rec    *fp_truth_cov_ptr;
typedef struct fp_truth_cov_rec {
        formula         f;      
        double          res;    
} fp_truth_cov_rec;             
                            
typedef struct fp_truth_cov2_rec    *fp_truth_cov2_ptr;
typedef struct fp_truth_cov2_rec {
        formula         cond;   
        formula         f;      
        double          res;    
} fp_truth_cov2_rec;        


typedef struct cond_gen_cache_rec   *cond_gen_cache_ptr;
typedef struct cond_gen_cache_rec {
    formula	c;
    formula	f;
} cond_gen_cache_rec;

#endif /* MODEL_COUNT_H */
#endif /* EXPORT_FORWARD_DECL */

