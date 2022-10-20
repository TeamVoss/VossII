//-------------------------------------------------------------------
// Copyright 2022 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2022                        *
*                                                                   *
*********************************************************************/
/* serialize.h -- header for serialize.c */
#ifdef EXPORT_FORWARD_DECL

/* ----- Function prototypes for public functions ----- */
void	Serialize_Begin();
void	Serialize_End();
void	Unserialize_Begin();
void	Unserialize_End();
void	Insert_pointer_map(pointer old_ptr, pointer new_ptr);
pointer	Old2new(pointer old);

bool	write_pointer(FILE *fp, pointer p);
void	read_pointer(FILE *fp, pointer *pp);
void	write_string(FILE *fp, string s);
void	read_string(FILE *fp, string *sp);
void	write_int(FILE *fp, int i);
void	read_int(FILE *fp, int *ip);
void	write_unint(FILE *fp, unint i);
void	read_unint(FILE *fp, unint *ip);
void	write_uchar(FILE *fp, uchar i);
void	read_uchar(FILE *fp, uchar *ip);
void	write_arbi_T(FILE *fp, arbi_T ai);
void	read_arbi_T(FILE *fp, arbi_T *aip);
void	write_mgr(FILE *fp, rec_mgr_ptr mp,
		  void (*write_rec)(FILE *fp, pointer p));
void	read_mgr(FILE *fp, rec_mgr_ptr mp,
		 void (*read_rec)(FILE *fp, pointer p));
void	write_buf(FILE *fp, buffer_ptr bp,
		  void (*write_item)(FILE *fp, pointer p));
void	read_buf(FILE *fp, buffer_ptr bp,
		 void (*read_item)(FILE *fp, pointer p));
void	write_hash_tbl(FILE *fp, hash_record_ptr hp,
		       void (*write_hash_key)(FILE *fp, pointer key),
		       void (*write_hash_data)(FILE *fp, pointer data));
void	read_hash_tbl(FILE *fp,  hash_record_ptr hp, 
		      void (*read_hash_key)(FILE *fp, pointer *pp),
		      void (*read_hash_data)(FILE *fp, pointer *pp));
void	write_ustr_mgr(FILE *fp, ustr_mgr *usmp);
void	write_ustr_mgr(FILE *fp, ustr_mgr *usmp);


// Extracted from scripts/template
void write_vstate_ptr(FILE *fp, vstate_ptr p);
void read_vstate_ptr(FILE *fp, vstate_ptr *pp);
void write_idx_list_ptr(FILE *fp, idx_list_ptr p);
void read_idx_list_ptr(FILE *fp, idx_list_ptr *pp);
void write_ste_ptr(FILE *fp, ste_ptr p);
void read_ste_ptr(FILE *fp, ste_ptr *pp);
void write_fsm_ptr(FILE *fp, fsm_ptr p);
void read_fsm_ptr(FILE *fp, fsm_ptr *pp);
void write_ilist_ptr(FILE *fp, ilist_ptr p);
void read_ilist_ptr(FILE *fp, ilist_ptr *pp);

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef SERIALIZE_H
#define SERIALIZE_H
#include "fl.h"	/* Global data types and include files 		     */

#endif /* SERIALIZE_H */
#endif /* EXPORT_FORWARD_DECL */
