//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "strmgr.h"
#include <ctype.h>
/************************************************************************/
/*                      Local variables                                 */
/************************************************************************/
static char	tmp_buf[STR_BLOCK_SIZE+1];
static char	*cur_tmp;
static int	tmp_cnt;
static int	initialized = 0;
static rec_mgr  tstr_rec_mgr;

static bool	occur(char c, string s);
void	string_too_long_warning();

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/
void
Init_strmgr()
{
    /* Reset the temp string area too */
    strtemp("");
    new_mgr(&tstr_rec_mgr, sizeof(tstr_rec));
    initialized = STRMGR_MAGIC_NBR;
}

void
new_strmgr(str_mgr_ptr smp)
{
    smp->used = 0;
    new_mgr(&(smp->block_mgr), STR_BLOCK_SIZE+1);
    smp->cur_blk = new_rec(&(smp->block_mgr));
    smp->initialized = STRMGR_MAGIC_NBR;
}

void
free_strmgr(str_mgr_ptr smp)
{
    ASSERT(smp->initialized == STRMGR_MAGIC_NBR);
    free_mgr(&(smp->block_mgr));
    smp->used	     = 0;
    smp->initialized = 0;
}

string
Strsave(str_mgr_ptr smp, string s)
{
    int		len;
    string	ret;

    ASSERT(smp->initialized == STRMGR_MAGIC_NBR);
    len = strlen(s);
    if( len > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	len = STR_BLOCK_SIZE;
    }
    if(smp->used + len >= STR_BLOCK_SIZE) {
	/* Need a new block */
	smp->used = 0;
	smp->cur_blk = new_rec(&(smp->block_mgr));
    }
    ret = (string) (smp->cur_blk + smp->used);
    bcopy(s, smp->cur_blk + smp->used, len);
    smp->used = smp->used+len;
    *((char *) (smp->cur_blk+smp->used)) = NULLSTR;
    smp->used++;
    return(ret);
}

string
WAstrsave(str_mgr_ptr smp, string s)
{
    string	ret;
    ASSERT(smp->initialized == STRMGR_MAGIC_NBR);
    ret = Strsave(smp, s);
    /* Make word aligned */
    smp->used = smp->used + (((smp->used % 4)>0)? (4-(smp->used % 4)):0);
    return(ret);
}

void
new_ustrmgr(ustr_mgr_ptr smp)
{
    new_strmgr(&(smp->smgr));
    create_hash(&(smp->utbl), 100, str_hash, str_equ);
}

void
free_ustrmgr(ustr_mgr_ptr smp)
{
    dispose_hash(&(smp->utbl), NULLFCN);
    free_strmgr(&(smp->smgr));
}

string
uStrsave(ustr_mgr_ptr smp, string s)
{
    string ret;
    if( (ret = (string) find_hash(&(smp->utbl), (pointer) s)) != NULL )
        return( ret );
    ret = Strsave(&(smp->smgr), s);
    insert_hash(&(smp->utbl), (pointer) ret, (pointer) ret);
    return( ret );
}

int
ustr_mgr_size(ustr_mgr_ptr smp)
{
    return( hash_size(&(smp->utbl)) );
}

tstr_ptr
new_temp_str_mgr()
{
    tstr_ptr tp;
    tp = (tstr_ptr) new_rec(&tstr_rec_mgr);
    tp->initialized = STRMGR_MAGIC_NBR;
    tp->tmp_buf[0] = 0;
    tp->cur_tmp = tp->tmp_buf;
    tp->tmp_cnt = 0;
    return tp;
}

void
free_temp_str_mgr(tstr_ptr tp)
{
    free_rec(&tstr_rec_mgr, (pointer) tp);
}

string
gen_strtemp(tstr_ptr tp, string s)
{
    ASSERT(tp->initialized == STRMGR_MAGIC_NBR);
    int len;
    len = strlen(s);
    if( len > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	len = STR_BLOCK_SIZE;
    }
    bcopy(s, tp->tmp_buf, len);
    tp->tmp_buf[len] = 0;
    tp->cur_tmp = &(tp->tmp_buf[len]);
    tp->tmp_cnt = len;
    return(tp->tmp_buf);
}

string
gen_strappend(tstr_ptr tp, string s)
{
    ASSERT(tp->initialized == STRMGR_MAGIC_NBR);
    int len;
    len = strlen(s);
    if( len + tp->tmp_cnt > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	len = STR_BLOCK_SIZE - tp->tmp_cnt;
    }
    bcopy(s, tp->cur_tmp, len);
    tp->cur_tmp = tp->cur_tmp+len;
    *(tp->cur_tmp) = 0;
    tp->tmp_cnt = tp->tmp_cnt + len;
    return(tp->tmp_buf);
}

string
gen_strprepend(tstr_ptr tp, string s)
{
    ASSERT(tp->initialized == STRMGR_MAGIC_NBR);
    int len;
    len = strlen(s);
    if( len + tp->tmp_cnt > STR_BLOCK_SIZE ) {
	// If prepending will overflow the string, make it a no-op.
	string_too_long_warning();
	return(tp->tmp_buf);
    }
    memmove(tp->tmp_buf+len,tp->tmp_buf, tp->tmp_cnt+1);
    memmove(tp->tmp_buf, s, len);
    tp->cur_tmp += len;
    tp->tmp_cnt += len;
    return(tp->tmp_buf);
}

string
gen_charappend(tstr_ptr tp, char c)
{
    ASSERT(tp->initialized == STRMGR_MAGIC_NBR);
    if( 1 + tp->tmp_cnt > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	return(tmp_buf);
    }
    *(tp->cur_tmp) = c;
    tp->cur_tmp++;
    *(tp->cur_tmp) = 0;
    tp->tmp_cnt++;
    return(tp->tmp_buf);
}

string
strtemp(string s)
{
    int len;

    len = strlen(s);
    if( len > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	len = STR_BLOCK_SIZE;
    }
    bcopy(s, tmp_buf, len);
    tmp_buf[len] = NULLSTR;
    cur_tmp = &(tmp_buf[len]);
    tmp_cnt = len;
    initialized = STRMGR_MAGIC_NBR;
    return(tmp_buf);
}

string
strappend(string s)
{
    int len;

    ASSERT(initialized == STRMGR_MAGIC_NBR);
    len = strlen(s);
    if( len + tmp_cnt > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	len = STR_BLOCK_SIZE - tmp_cnt;
    }
    bcopy(s, cur_tmp, len);
    cur_tmp = cur_tmp+len;
    *cur_tmp = NULLSTR;
    tmp_cnt = tmp_cnt + len;
    return(tmp_buf);
}

string
charappend(char c)
{
    ASSERT(initialized == STRMGR_MAGIC_NBR);
    if( 1 + tmp_cnt > STR_BLOCK_SIZE ) {
	string_too_long_warning();
	return(tmp_buf);
    }
    *cur_tmp = c;
    cur_tmp++;
    *cur_tmp = NULLSTR;
    tmp_cnt++;
    return(tmp_buf);
}

string
gettoken(string *s, string delims)
{
    string ret;
    ret = strtemp("");
    while( **s && (**s == ' ' || **s == '\t' || occur(**s, delims)) )
	(*s)++;
    while( **s && **s != ' ' && **s != '\t' && !occur(**s, delims) ) {
	ret = charappend(**s);
	(*s)++;
    }
     return( ret );
}

// Function to compare vector nodes so that
//  a[31] > a[30]
//  a[31] > a[3]
//  a[30] < abc[31]
//      a < b
// . . .
int
node_name_cmp(const char *s1, const char *s2)
{
    while( *s1 && *s2 ) {
	if( isdigit(*s1) && isdigit(*s2) ) {
	    int i1 = atoi(s1);
	    int i2 = atoi(s2);
	    if( i1 != i2 ) { return( i1-i2 ); }
	    while( *s1 && isdigit(*s1) ) s1++;
	    while( *s2 && isdigit(*s2) ) s2++;
	} else {
	    if( *s1 == *s2 ) {
		s1++;
		s2++;
	    } else {
		return( strcmp(s1,s2) );
	    }
	}
    }
    if( *s1 ) {
	// !*s2
	return 1;
    }
    if( *s2 ) {
	// !*s1
	return -1;
    }
    return 0;
}


/************************************************************************/
/*                      Local Functions                                 */
/************************************************************************/

static bool
occur(char c, string s)
{
    while( *s ) {
	if( c == *s )
	    return( TRUE );
	s++;
    }
    return( FALSE );
}

void
string_too_long_warning()
{
    fprintf(stderr, "WARNING: String too long. Truncated\n");
}
