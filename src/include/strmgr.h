//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1993			    *
*                                                                   *
*********************************************************************/
/* strmgr.h -- header for strmgr.c */

#ifndef _STRMGR_H
#define _STRMGR_H
#include <string.h>
#include "types.h"
#include "rec.h"
#include "hash.h"

#define STRMGR_MAGIC_NBR	12349876
#define STR_BLOCK_SIZE	32750	/* Leave some room for malloc's header */

typedef struct str_mgr {
	int		initialized;
	rec_mgr		block_mgr;
	pointer		cur_blk;
	int		used;
} str_mgr, *str_mgr_ptr;

typedef struct ustr_mgr {
	str_mgr		smgr;
	hash_record	utbl;
} ustr_mgr, *ustr_mgr_ptr;

typedef struct tstr_rec  *tstr_ptr;
typedef struct tstr_rec {
        int             initialized;
        char            tmp_buf[STR_BLOCK_SIZE];
        char            *cur_tmp;
        int             tmp_cnt;
} tstr_rec;

void            Init_strmgr();
void		new_strmgr(str_mgr_ptr smp);
void		free_strmgr(str_mgr_ptr smp);
void		new_ustrmgr(ustr_mgr_ptr smp);
void		free_ustrmgr(ustr_mgr_ptr smp);
int		ustr_mgr_size(ustr_mgr_ptr smp);
string		Strsave(str_mgr_ptr smp, string s);
string		uStrsave(ustr_mgr_ptr smp, string s);
string		WAstrsave(str_mgr_ptr smp, string s);
string		strtemp(string s);
string		strappend(string s);
string		charappend(char c);
string		gettoken(string *s, string delims);
int		node_name_cmp(const char *s1, const char *s2);
tstr_ptr        new_temp_str_mgr();
void            free_temp_str_mgr(tstr_ptr tp);
string          gen_strtemp(tstr_ptr tp, string s);
string          gen_strappend(tstr_ptr tp, string s);
string		gen_strprepend(tstr_ptr tp, string s);
string          gen_charappend(tstr_ptr tp, char c);

#endif /* _STRMGR_H */
