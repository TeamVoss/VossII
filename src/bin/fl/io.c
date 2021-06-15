//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************
 *									*
 *	Original author:  2016 Carl-Johan Seger      		        *
 *									*
 ************************************************************************/
/* io.c -- printf/scanf and file operations */
#include "io.h"
#include "graph.h"

extern g_ptr    void_nd;
extern str_mgr  strings;
extern char     FailBuf[4096];
extern bool	debug_on;
extern string	Voss_tmp_dir;
extern int	RCmax_stack_trace_entries;
extern buffer	ext_obj_buf;
extern FILE     *odests_fp;


/**** PRIVATE VARIABLES ****/
static rec_mgr		io_rec_mgr;
static io_ptr		open_ios;
static char		bdd_pbuf[4096];
static char		buf[4096];
static uniq_buffer	bexpr_buf;
static uniq_buffer	bool_buf;
#if 0
static uniq_buffer	fsm_buf;
#endif
static uniq_buffer	string_buf;
static hash_record	g_save_tbl;
static int		g_save_cnt;
static buffer		bool_results;
static buffer		bexpr_results;
#if 0
static buffer		fsm_results;
#endif
static buffer		str_results;

/* ----- Forward definitions local functions ----- */
static void	neg_too_big(int i, int pfn, g_ptr redex);
static void     make_redex_failure(g_ptr redex);
static bool     check_arg(g_ptr *rootp, g_ptr **spp, int *depthp,
                         int n, g_ptr redex, g_ptr arg);
static bool     prs(int pfn, int i, g_ptr redex,
                    char *s, bool ljust, bool zfill, int size);
static bool     force_arg_and_check(g_ptr redex, g_ptr arg);
static bool     print_list(int pfn, g_ptr l, int i,g_ptr redex,
                           bool ljust, bool zfill, int size);
static bool	can_be_saved(g_ptr np);
static int	save_graph_rec(FILE *fp, g_ptr np);

/****************************************************************************/
/*                           Main functions                                 */
/****************************************************************************/

/*                                                                  */
/* Function to open a file or a pipeline to a command.              */
/* mode is one of:                                                  */
/*                                                                  */
/*      r   Open the file for reading from the start                */
/*                                                                  */
/*      r+  Open the file for reading and writing from the start    */
/*                                                                  */
/*      w   Open the file for writing from the start.               */
/*          If the file did not exist, it is created.               */
/*          If the file did exist, it is truncated.                 */
/*                                                                  */
/*      w+  Open the file for reading and writing from the start.   */
/*                                                                  */
/*      a   Open the file for writing appending to the end.         */
/*                                                                  */
/*      a+  Open the file for reading and writing appending to end. */
/*                                                                  */
/*      |r  Open a read pipe to a command                           */
/*                                                                  */
/*      |w  Open a write pipe to a command                          */
/*                                                                  */
/* Stdin, stdout, and stderr cannot be opened (or closed)           */

bool
Fopen(g_ptr *rootp, g_ptr **spp, int *depthp)
{
    if( *depthp < 2 ) return FALSE;
    g_ptr redex = *(*spp+1);
    g_ptr arg1 = force(GET_APPLY_RIGHT(**spp), FALSE);
    if( !check_arg(rootp, spp, depthp, 2, redex, arg1) ) return( TRUE );
    g_ptr arg2 = force(GET_APPLY_RIGHT(*(*spp+1)), FALSE);
    if( !check_arg(rootp, spp, depthp, 2, redex, arg2) ) return( TRUE );

    string name = GET_STRING(arg1);
    string mode = GET_STRING(arg2);

    for(io_ptr ip = open_ios; ip != NULL; ip = ip->next) {
        if( STREQ(name, ip->name) ) {
	    Fail_pr("File %s already opened", name);
	    g_ptr l = GET_APPLY_LEFT(redex);
	    g_ptr r = GET_APPLY_RIGHT(redex);
            make_redex_failure(redex);
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    *spp = *spp + 2;
	    *depthp = *depthp - 2;
	    *rootp = redex;
            return( TRUE );
        }
    }
    FILE *fp;
    bool is_pipe;
    bool writable;
    if( strcmp(name, "stdin") == 0 ) {
        fp = stdin;
        is_pipe = FALSE;
        writable = FALSE;
        mode = wastrsave(&strings, "r");
    } else
    if( strcmp(name, "stdout") == 0 ) {
        fp = stdout;
        is_pipe = FALSE;
        writable = TRUE;
        mode = wastrsave(&strings, "w");
    } else
    if( strcmp(name, "stderr") == 0 ) {
        fp = stderr;
        is_pipe = FALSE;
        writable = TRUE;
        mode = wastrsave(&strings, "w");
    } else
    if( *mode == '|' ) {
        fp = popen(name, (mode+1));
        if( *(mode+1) == 'w' )
            writable = TRUE;
        else
            writable = FALSE;
        is_pipe = TRUE;
    } else {
        fp = fopen(name, mode);
        if( *mode == 'w' || *mode == 'a' )
            writable = TRUE;
        else
            writable = FALSE;
        is_pipe = FALSE;
    }
    if( fp == NULL ) {
        Fail_pr("Cannot open %s in mode %s", name, mode);
	g_ptr l = GET_APPLY_LEFT(redex);
	g_ptr r = GET_APPLY_RIGHT(redex);
        make_redex_failure(redex);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	*spp = *spp + 2;
	*depthp = *depthp - 2;
	*rootp = redex;
        return( TRUE );
    }

    io_ptr ip;
    ip = (io_ptr) new_rec(&io_rec_mgr);
    ip->name = name;
    ip->fp = fp;
    ip->is_pipe = is_pipe;
    ip->mode = mode;
    ip->writable = writable;
    ip->next = open_ios;
    open_ios = ip;
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    SET_TYPE(redex, LEAF);
    SET_LEAF_TYPE(redex, PRIM_FN);
    SET_PRIM_FN(redex, P_FILEFP);
    SET_FILE_IO_PTR(redex, ip);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    *spp = *spp+2;
    *depthp -= 2;
    *rootp = redex;
    return TRUE;
}

bool
Fflush(g_ptr *rootp, g_ptr **spp, int *depthp)
{
    if( *depthp < 1 ) return FALSE;
    g_ptr redex = **spp;
    g_ptr arg1 = force(GET_APPLY_RIGHT(**spp), FALSE);
    if( !check_arg(rootp, spp, depthp, 1, redex, arg1) ) return( TRUE );
    io_ptr ip = GET_FILE_IO_PTR(arg1);
    fflush(ip->fp);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    OVERWRITE(redex, void_nd);
    DEC_REF_CNT( l );
    DEC_REF_CNT( r );
    *spp = *spp+1;
    *depthp -= 1;
    *rootp = redex;
    return( TRUE );
}

bool
Fclose(g_ptr *rootp, g_ptr **spp, int *depthp)
{
    if( *depthp < 1 ) return FALSE;
    g_ptr redex = **spp;
    g_ptr arg1 = force(GET_APPLY_RIGHT(**spp), FALSE);
    if( !check_arg(rootp, spp, depthp, 1, redex, arg1) ) return( TRUE );
    io_ptr ip = GET_FILE_IO_PTR(arg1);
    if( strcmp(ip->name, "stdin") == 0 ) {
	g_ptr l = GET_APPLY_LEFT(redex);
	g_ptr r = GET_APPLY_RIGHT(redex);
	Fail_pr("Cannot close stdin.");
        make_redex_failure(redex);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	*spp = *spp + 1;
	*depthp = *depthp - 1;
	*rootp = redex;
        return( TRUE );
    } else 
    if( strcmp(ip->name, "stdout") == 0 ) {
	g_ptr l = GET_APPLY_LEFT(redex);
	g_ptr r = GET_APPLY_RIGHT(redex);
	Fail_pr("Cannot close stdout.");
        make_redex_failure(redex);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	*spp = *spp + 1;
	*depthp = *depthp - 1;
	*rootp = redex;
        return( TRUE );
    } else
    if( strcmp(ip->name, "stderr") == 0 ) {
	g_ptr l = GET_APPLY_LEFT(redex);
	g_ptr r = GET_APPLY_RIGHT(redex);
	Fail_pr("Cannot close stderr.");
        make_redex_failure(redex);
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	*spp = *spp + 1;
	*depthp = *depthp - 1;
	*rootp = redex;
        return( TRUE );
    }

    io_ptr *prev_io = &open_ios;
    io_ptr cur = open_ios;
    while( cur && !STREQ(cur->name, ip->name) ) {
        prev_io = &(cur->next);
        cur = cur->next;
    }
    if( cur == NULL ) {
	g_ptr l = GET_APPLY_LEFT(redex);
	g_ptr r = GET_APPLY_RIGHT(redex);
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Trying to close %s more than once!",
				    ip->name));
	DEC_REF_CNT( l );
	DEC_REF_CNT( r );
	*spp = *spp+1;
	*depthp -= 1;
	*rootp = redex;
	return( TRUE );
    }
    *prev_io = cur->next;
    if( ip->is_pipe ) {
        pclose(ip->fp);
    } else {
        fclose(ip->fp);
    }
#if 0
    // This does not really work since there can still be 
    // FILE_IP_PTRs with this ip.
    // For the time being, I will simply not free the record....
    // free_rec(&io_rec_mgr, (pointer) ip);
#endif
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    OVERWRITE(redex, void_nd);
    DEC_REF_CNT( l );
    DEC_REF_CNT( r );
    *spp = *spp+1;
    *depthp -= 1;
    *rootp = redex;
    return( TRUE );
}

static bool
in_range(char c, int max)
{
    if( max < 10 ) {
	int ci = c - '0';
	return( 0 <= ci && ci <= max );
    } else {
	ASSERT( max == 15 );
	int ci = c - '0';
	int ca = c - 'a';
	int cA = c - 'A';
	if( 0 <= ci && ci <= 10 ) return TRUE;
	if( 0 <= ca && ca <= 6 ) return TRUE;
	if( 0 <= cA && cA <= 6 ) return TRUE;
	return FALSE;
    }
}

bool
Sscanf(g_ptr *rootp, g_ptr **spp, int *depthp)
{
    /* First count the number of argument needed (defined by the pattern) */
    /* For sscanf,only * patterns add arguments				  */
    string fmt = GET_PRINTF_STRING(*rootp);
    int args = 1;   // String to scan
    PROCESS_PRINTF_PAT(fmt,
		    sscanf_cnt_args_lbl2, /* lbl */
		    {}, /* start_of_pat */
		    {}, /* pat_0 */
		    {}, /* pat_1_to_9 */
		    {}, /* pat_minus */
		    {}, /* pat_percent */
		    {args++;}, /* pat_star */
		    {}, /* pat_b */
		    {}, /* pat_o */
		    {}, /* pat_x */
                    {}, /* pat_d */
		    {}, /* pat_s */
		    {}, /* pat_B */
		    {}, /* pat_S */
		    {}, /* pat_error */
		    {}) /* pat_other */

    if( *depthp < args ) { return FALSE; }

    g_ptr *sp = *spp;
    g_ptr redex = *(sp+args-1);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);

    /* Now force all the arguments */
    for(int i = 0; i < args; i++) {
	if( !force_arg_and_check(redex, *(sp+i)) )
	    goto init_end_sfn;
    }

    /* Now process the sscanf command             */
    string input = GET_STRING(GET_APPLY_RIGHT(*sp));
    string inp = strtemp(input);
    buffer scanf_result_buf;
    new_buf(&scanf_result_buf, 10, sizeof(g_ptr));
    int size = 0;
    int arg_cnt = 1;
    PROCESS_PRINTF_PAT(fmt,
		    sscanf_args_lbl, /* lbl */
		    {
                        /* start_of_pat */
                        size   = 0;
                    },
		    {
                        /* pat_0 */
                        size = 10 * size + (*s - '0');
                    },
		    {
                        /* pat_1_to_9 */
                        size = 10 * size + (*s - '0');
                    },
		    {
                        /* pat_minus */
                    },
		    {
                        /* pat_percent */
			if( *inp != '%' ) 
			    goto sscanf_failure;
			inp++;
                    },
		    {
                        /* pat_star */
                        size = GET_INT(GET_APPLY_RIGHT(*(sp+arg_cnt)));
                        arg_cnt++;
                    },
		    {
                        /* pat_b */
			string start = inp;
			char tmp;
			size = (size == 0)? (int) strlen(inp) : size;
			int neg = 1;
			if( *inp == '-' ) {
			    neg = -1;
			    inp++;
			    start = inp;
			}
			while( size > 0 && in_range(*inp, 1) ) {
			    inp++;
			    size--;
			}
			tmp = *inp;
			*inp = 0;
			arbi_T ires = Arbi_FromString(start, 2);
			if( neg == -1 ) {
			    ires = Arbi_mlt(Arbi_FromInt(-1), ires);
			}
			g_ptr res = Make_AINT_leaf(ires);
			*inp = tmp;
			size = 0;
			push_buf(&scanf_result_buf, &res);
                    },
		    {
                        /* pat_o */
			string start = inp;
			char tmp;
			size = (size == 0)? (int) strlen(inp) : size;
			int neg = 1;
			if( *inp == '-' ) {
			    neg = -1;
			    inp++;
			    start = inp;
			}
			while( size > 0 && in_range(*inp, 7) ) {
			    inp++;
			    size--;
			}
			tmp = *inp;
			*inp = 0;
			arbi_T ires = Arbi_FromString(start, 8);
			if( neg == -1 ) {
			    ires = Arbi_mlt(Arbi_FromInt(-1), ires);
			}
			g_ptr res = Make_AINT_leaf(ires);
			*inp = tmp;
			size = 0;
			push_buf(&scanf_result_buf, &res);
                    },
		    {
                        /* pat_x */
			string start = inp;
			char tmp;
			size = (size == 0)? (int) strlen(inp) : size;
			int neg = 1;
			if( *inp == '-' ) {
			    neg = -1;
			    inp++;
			    start = inp;
			}
			while( size > 0 && in_range(*inp, 15) ) {
			    inp++;
			    size--;
			}
			tmp = *inp;
			*inp = 0;
			arbi_T ires = Arbi_FromString(start, 16);
			if( neg == -1 ) {
			    ires = Arbi_mlt(Arbi_FromInt(-1), ires);
			}
			g_ptr res = Make_AINT_leaf(ires);
			*inp = tmp;
			size = 0;
			push_buf(&scanf_result_buf, &res);
                    },
                    {
                        /* pat_d */
			string start = inp;
			char tmp;
			size = (size == 0)? (int) strlen(inp) : size;
			int neg = 1;
			if( *inp == '-' ) {
			    neg = -1;
			    inp++;
			    start = inp;
			}
			while( size > 0 && in_range(*inp, 9) ) {
			    inp++;
			    size--;
			}
			tmp = *inp;
			*inp = 0;
			arbi_T ires = Arbi_FromString(start, 10);
			if( neg == -1 ) {
			    ires = Arbi_mlt(Arbi_FromInt(-1), ires);
			}
			g_ptr res = Make_AINT_leaf(ires);
			*inp = tmp;
			size = 0;
			push_buf(&scanf_result_buf, &res);
                    },
		    {
                        /* pat_s */
			string start = inp;
			char tmp;
			size = (size == 0)? (int) strlen(inp) : size;
			while( size > 0 && !isspace(*inp) ) {
			    inp++;
			    size--;
			}
			tmp = *inp;
			*inp = 0;
			g_ptr res =Make_STRING_leaf(wastrsave(&strings, start));
			*inp = tmp;
			size = 0;
			push_buf(&scanf_result_buf, &res);
                    },
		    {
                        /* pat_B */
			MAKE_REDEX_FAILURE(redex,
			    Fail_pr("-E- sscanf does not support %%B"));
			goto end_sfn;
                    },
		    {
                        /* pat_S */
			MAKE_REDEX_FAILURE(redex,
			    Fail_pr("-E- sscanf does not support %%S"));
			goto end_sfn;
                    },
		    {}, /* pat_error */
		    {
                        /* pat_other */
			if( *inp != *s )
			    goto sscanf_failure;
			inp++;
                    }
            )

    /* Create return tuple */
    int items = COUNT_BUF(&scanf_result_buf);
    if( items == 0 ) {
	MAKE_REDEX_VOID(redex);
    } else {
	int left = items;
	g_ptr cur = NULL;
	g_ptr *rpp;
	FUB_ROF(&scanf_result_buf, g_ptr, rpp) {
	    left--;
	    if( left == 0 ) {
		if( cur == NULL ) {
		    OVERWRITE(redex, *rpp);
		} else {
		    MAKE_REDEX_CONS_ND(redex,*rpp,cur);
		}
	    } else {
		cur = (cur == NULL)? *rpp : Make_CONS_ND(*rpp, cur);
	    }
	}
    }
    goto end_sfn;


  sscanf_failure:
    MAKE_REDEX_FAILURE(redex, Fail_pr("-E- sscanf input mismatch"));

  end_sfn:
    free_buf(&scanf_result_buf);
  init_end_sfn:
    if( args > 0 ) {
        DEC_REF_CNT(l);
        DEC_REF_CNT(r);
        *spp = *spp+args;
        *depthp -= args;
    }
    *rootp = redex;
    return( TRUE );
}

bool
Printf(g_ptr *rootp, g_ptr **spp, int *depthp)
{
    /* First count the number of argument needed (defined by the pattern) */
    int pfn = GET_PRIM_FN(*rootp);
    string fmt = GET_PRINTF_STRING(*rootp);
    int args = (pfn == P_FPRINTF)? 1 : 0;
    PROCESS_PRINTF_PAT(fmt,
		    cnt_args_lbl, /* lbl */
		    {}, /* start_of_pat */
		    {}, /* pat_0 */
		    {}, /* pat_1_to_9 */
		    {}, /* pat_minus */
		    {}, /* pat_percent */
		    {args++;}, /* pat_star */
		    {args++;}, /* pat_b */
		    {args++;}, /* pat_o */
		    {args++;}, /* pat_x */
                    {args++;}, /* pat_d */
		    {args++;}, /* pat_s */
		    {args++;}, /* pat_B */
		    {args++;}, /* pat_S */
		    {}, /* pat_error */
		    {}) /* pat_other */

    if( *depthp < args ) { return FALSE; }

    g_ptr redex, l, r, *sp;
    sp = *spp;
    if( args == 0 ) {
        redex = *rootp;
        l = r = NULL;
    } else {
        redex = *(sp+args-1);
        l = GET_APPLY_LEFT(redex);
        r = GET_APPLY_RIGHT(redex);
    }

    /* Now force all the arguments */
    int i;
    for(i = 0; i < args; i++) {
        if( !force_arg_and_check(redex, *(sp+i)) )
            goto end_pfn;
    }

    /* Now process the print command             */
    bool ljust, zfill;
    int size;
    string res = strtemp("");
    i = (pfn == P_FPRINTF)? 1 : 0;
    PROCESS_PRINTF_PAT(fmt,
		    print_args_lbl, /* lbl */
		    {
                        /* start_of_pat */
                        size   = 0;
                        zfill = FALSE;
                        ljust = FALSE;
                    },
		    {
                        /* pat_0 */
                        if (size == 0) {
                            zfill = TRUE;
                        }
                        size = 10 * size + (*s - '0');
                    },
		    {
                        /* pat_1_to_9 */
                        size = 10 * size + (*s - '0');
                    },
		    {
                        /* pat_minus */
                        ljust = TRUE;
                    },
		    {
                        /* pat_percent */
                        res = charappend('%');
                    },
		    {
                        /* pat_star */
                        size = GET_INT(GET_APPLY_RIGHT(*(sp+i)));
                        i++;
                    },
		    {
                        /* pat_b */
                        arbi_T v = GET_AINT(GET_APPLY_RIGHT(*(sp+i)));
			if( size != 0 ) {
			    // If fixed size, always zero fill and don't ljust
			    zfill = TRUE;
			    ljust = FALSE;
			}
			if( Arbi_IsNegative(v) ) {
			    v = Arbi_neg(v);
			    string tmp = res+strlen(res);
			    if( !zfill ) {
				charappend('0');
			    }
			    if( !prs(pfn, i, redex, Arbi_ToString(v,2),
				     ljust, zfill, size) )
				goto end_pfn;
			    string last = rindex(tmp, '1');
			    if( last != NULL ) {
				for(string s = tmp; s < last; s++) {
				    if( *s == '1' )
					*s = '0';
				    else
					*s = '1';
				}
			    }
			} else {
			    if(!zfill && Arbi_cmp(v,Arbi_FromInt(0))!=arbi_EQ) {
				charappend('0');
				if( size == 1 ) {
				    neg_too_big(i, pfn, redex);
				    goto end_pfn;
				}
				if( size > 0 ) size--;
			    }
			    if( !prs(pfn, i, redex, Arbi_ToString(v,2),
				     ljust, zfill, size) )
				goto end_pfn;
			}
                        i++;
                    },
		    {
                        /* pat_o */
                        arbi_T v = GET_AINT(GET_APPLY_RIGHT(*(sp+i)));
                        bool neg = FALSE;
                        string tmp = res+strlen(res);
                        if( Arbi_IsNegative(v) ) {
                            v = Arbi_neg(v);
                            neg = TRUE; 
                        }
                        if( neg && (size == 0 || ljust) ) {
                            charappend('-');
                            if( size == 1 ) {
                                neg_too_big(i, pfn, redex);
                                goto end_pfn;
                            }
                            if( size > 0 ) {
                                size--;
                            }    
                            neg = FALSE;
                        }
                        if( !prs(pfn, i, redex, Arbi_ToString(v,8),
                                 ljust, zfill, size) )
                            goto end_pfn;
                        if( neg ) {
                            string s = tmp;
                            if( *s != '0' ) {
                                while( *s == ' ' ) s++;
                                s--; 
                            }
                            if( s < tmp ) {
                                neg_too_big(i, pfn, redex);
                                goto end_pfn;
                            }
                            *s = '-';
                        }
                        i++;
                    },
		    {
                        /* pat_x */
                        arbi_T v = GET_AINT(GET_APPLY_RIGHT(*(sp+i)));
			bool neg = FALSE;
			string tmp = res+strlen(res);
			if( Arbi_IsNegative(v) ) {
			    v = Arbi_neg(v);
			    neg = TRUE;
			}
			if( neg && (size == 0 || ljust) ) {
			    charappend('-');
			    if( size == 1 ) {
				neg_too_big(i, pfn, redex);
				goto end_pfn;
			    }
			    if( size > 0 ) {
				size--;
			    }
			    neg = FALSE;
			}
                        if( !prs(pfn, i, redex, Arbi_ToString(v,16),
                                 ljust, zfill, size) )
                            goto end_pfn;
			if( neg ) {
			    string s = tmp;
			    if( *s != '0' ) {
				while( *s == ' ' ) s++;
				s--;
			    }
			    if( s < tmp ) {
				neg_too_big(i, pfn, redex);
				goto end_pfn;
			    }
			    *s = '-';
			}
                        i++;
                    },
                    {
                        /* pat_d */
                        arbi_T v = GET_AINT(GET_APPLY_RIGHT(*(sp+i)));
			bool neg = FALSE;
			string tmp = res+strlen(res);
			if( Arbi_IsNegative(v) ) {
			    v = Arbi_neg(v);
			    neg = TRUE;
			}
			if( neg && (size == 0 || ljust) ) {
			    charappend('-');
			    if( size == 1 ) {
				neg_too_big(i, pfn, redex);
				goto end_pfn;
			    }
			    if( size > 0 ) {
				size--;
			    }
			    neg = FALSE;
			}
                        if( !prs(pfn, i, redex, Arbi_ToString(v,10),
                                 ljust, zfill, size) )
                            goto end_pfn;
			if( neg ) {
			    string s = tmp;
			    if( *s != '0' ) {
				while( *s == ' ' ) s++;
				s--;
			    }
			    if( s < tmp ) {
				neg_too_big(i, pfn, redex);
				goto end_pfn;
			    }
			    *s = '-';
			}
                        i++;
                    },
		    {
                        /* pat_s */
                        if( !prs(pfn, i, redex,
                                 GET_STRING(GET_APPLY_RIGHT(*(sp+i))),
                                 ljust, zfill, size) )
                            goto end_pfn;
                        i++;
                    },
		    {
                        /* pat_B */
			if((odests_fp = fmemopen(bdd_pbuf,4096,"w")) == NULL ) {
			    DIE("Should never happen");
			}
			B_Print(FILE_fp, GET_BOOL(GET_APPLY_RIGHT(*(sp+i))),-1);
			fclose(odests_fp);
			odests_fp = NULL;
                        if( !prs(pfn, i, redex, bdd_pbuf, ljust, zfill, size) )
                            goto end_pfn;
                        i++;
                    },
		    {
                        /* pat_S */
                        g_ptr l = GET_APPLY_RIGHT(*(sp+i));
                        if( !print_list(pfn, l, i,redex, ljust, zfill, size) )
                            goto end_pfn;
                        i++;
                    },
		    {}, /* pat_error */
		    {
                        /* pat_other */
                        charappend(*s);
                    }
            )


    switch(pfn) {
        case P_PRINTF: {
            FP(stdout_fp, "%s", res);
            OVERWRITE(redex, void_nd);
            break;
        }
        case P_SPRINTF: {
            SET_TYPE(redex, LEAF);
            SET_LEAF_TYPE(redex, STRING);
            SET_STRING(redex, wastrsave(&strings, res));
            break;
        }
        case P_EPRINTF: {
	    Fail_pr("%s", res);
            make_redex_failure(redex);
            break;
        }
        case P_FPRINTF: {
            io_ptr ip = GET_FILE_IO_PTR(GET_APPLY_RIGHT(*sp));
            if( !ip->writable ) {
		g_ptr l = GET_APPLY_LEFT(redex);
		g_ptr r = GET_APPLY_RIGHT(redex);
                Fail_pr("File %s is not open for writing.", ip->name);
                make_redex_failure(redex);
		DEC_REF_CNT(l);
		DEC_REF_CNT(r);
		*spp = *spp + args;
		*depthp = *depthp - args;
		*rootp = redex;
                return( TRUE );
            }
            if( strcmp(ip->name, "stdout") == 0 ) {
                FP(stdout_fp, "%s", res);
            } else
            if( strcmp(ip->name, "stderr") == 0 ) {
                FP(err_fp, "%s", res);
            } else {
                fprintf(ip->fp, "%s", res);
                fflush(ip->fp);
            }
            OVERWRITE(redex, void_nd);
            break;
        }
        default:
            DIE("Should not happend");
            break;
    }


  end_pfn:
    if( args > 0 ) {
        DEC_REF_CNT(l);
        DEC_REF_CNT(r);
        *spp = *spp+args;
        *depthp -= args;
    }
    *rootp = redex;
    return( TRUE );
}

formula
Load_get_bool_from_idx(int idx)
{
    return( *((formula *) FAST_LOC_BUF(&bool_results, idx)) );
}

bexpr
Load_get_bexpr_from_idx(int idx)
{
    return( *((bexpr *) FAST_LOC_BUF(&bexpr_results, idx)) );
}

string
Load_get_string_from_idx(int idx)
{
    return( *((string *) FAST_LOC_BUF(&str_results, idx)) );
}

#if 0
int
Load_get_fsm_from_idx(int idx)
{
    return( *((int *) FAST_LOC_BUF(&fsm_results, idx)) );
}
#endif

int
Save_get_bool_idx(formula f)
{
    return( get_uniq_buf_index(&bool_buf, (pointer) &f) );
}

int
Save_get_bexpr_idx(bexpr be)
{
    return( get_uniq_buf_index(&bexpr_buf, (pointer) &be) );
}

int
Save_get_string_idx(string s)
{
    return( get_uniq_buf_index(&string_buf, (pointer) &s) );
}

#if 0
int
Save_get_fsm_idx(int ckt)
{
    return( get_uniq_buf_index(&fsm_buf, (pointer) &ckt) );
}
#endif

static g_ptr
record_bexpr(g_ptr leaf_node)
{
    if( GET_LEAF_TYPE(leaf_node) == BEXPR ) {
	bexpr be = GET_BEXPR(leaf_node);
	push_uniq_buf(&bexpr_buf, (pointer) &be);
    }
    return NULL;
}

static g_ptr
record_bool(g_ptr leaf_node)
{
    if( GET_LEAF_TYPE(leaf_node) == BOOL ) {
	formula b = GET_BOOL(leaf_node);
	push_uniq_buf(&bool_buf, (pointer) &b);
    }
    return NULL;
}


static g_ptr
record_string(g_ptr leaf_node)
{
    ASSERT(IS_LEAF(leaf_node));
    if( IS_STRING(leaf_node) ) {
	string s = GET_STRING(leaf_node);
	push_uniq_buf(&string_buf, (pointer) &s);
    } else if( IS_PRIM_FN(leaf_node) ) {
	switch( GET_PRIM_FN(leaf_node) ) {
	    string s;
	    case P_FAIL:
		s = GET_FAIL_STRING(leaf_node);
		push_uniq_buf(&string_buf, (pointer) &s);
		break;
	    case P_PRINTF:
	    case P_FPRINTF:
	    case P_EPRINTF:
	    case P_SPRINTF:
		s = GET_PRINTF_STRING(leaf_node);
		push_uniq_buf(&string_buf, (pointer) &s);
		break;
	    default:
		break;
	}
    } else if( IS_VAR(leaf_node) ) {
	string s = GET_VAR(leaf_node);
	push_uniq_buf(&string_buf, (pointer) &s);
    }
    return NULL;
}

#define TAG_APPLY	'A'
#define TAG_CONS	'C'
#define TAG_NIL		'0'
#define TAG_INT		'I'
#define TAG_STRING	'S'
#define TAG_BOOL	'B'
#define TAG_BEXPR	'E'
#define TAG_FAIL	'F'
#define TAG_REF_VAR	'R'
#define TAG_PRINTF	'!'
#define TAG_EPRINTF	'@'
#define TAG_FPRINTF	'#'
#define TAG_SPRINTF	'$'
#define TAG_CACHE	'='
#define TAG_EXTAPI	'X'
#define TAG_PRIM_FN	'P'
#define TAG_VAR		'V'
#define TAG_EXTOBJ	'O'

bool
Save_graph(string type_sig, string file_name, g_ptr node)
{
    Sprintf(buf, "%s/tmp_save_XXXXXX", Voss_tmp_dir);
    string dir = wastrsave(&strings, mkdtemp(buf));
    Sprintf(buf, "%s/signature", dir);
    string sig_file_name = wastrsave(&strings, buf);
    Sprintf(buf, "%s/bexprs", dir);
    string bexpr_file_name = wastrsave(&strings, buf);
    Sprintf(buf, "%s/bools", dir);
    string bool_file_name = wastrsave(&strings, buf);
    new_uniq_buf(&bexpr_buf, 100, sizeof(bexpr));
    new_uniq_buf(&bool_buf, 100, sizeof(formula));
#if 0
    new_uniq_buf(&fsm_buf, 100, sizeof(int));
#endif
    new_uniq_buf(&string_buf, 100, sizeof(string));
    // Bexpr saving
    Gen_map(record_bexpr, node, TRUE);
    Save_bexprs(bexpr_file_name, &(bexpr_buf.buf));
    // BDD saving
    Gen_map(record_bool, node, TRUE);
    Save_BDDs(bool_file_name, &(bool_buf.buf));
    // String saving
    Gen_map(record_string, node, TRUE);
    Sprintf(buf, "%s/strings", dir);
    FILE *fp = fopen(buf, "w");
    if( fp == NULL ) {
	DIE("Failed to save graph %s\n%s\n", buf, strerror(errno));
    }
    string *sp;
    FOR_UNIQ_BUF(&string_buf, string, sp) {
	fprintf(fp, "%d %s\n", (int) strlen(*sp), *sp);
    }
    fclose(fp);
    // Graph saving
    Sprintf(buf, "%s/graph", dir);
    fp = fopen(buf, "w");
    if( fp == NULL ) {
	DIE("Failed to save graph to %s\n%s\n", buf, strerror(errno));
    }
    create_hash(&g_save_tbl, 1000, ptr_hash, ptr_equ);
    if( !can_be_saved(node) ) {
	dispose_hash(&g_save_tbl, NULLFCN);
	return( FALSE );
    }
    dispose_hash(&g_save_tbl, NULLFCN);
    create_hash(&g_save_tbl, 1000, ptr_hash, ptr_equ);
    g_save_cnt = 0;
    save_graph_rec(fp, node);
    dispose_hash(&g_save_tbl, NULLFCN);
    fclose(fp);
    // Type signature saving
    if( (fp = fopen(sig_file_name, "w")) == NULL ) {
	DIE("Failed to open type signature file %s for writing\n%s\n",
	    sig_file_name, strerror(errno));
    }
    fprintf(fp, "%d\n", g_save_cnt);
    fprintf(fp, "%s\n", type_sig);
    fclose(fp);
    free_uniq_buf(&bexpr_buf);
    free_uniq_buf(&bool_buf);
#if 0
    free_uniq_buf(&fsm_buf);
#endif
    free_uniq_buf(&string_buf);
    // Tar up the files
#if 0
#else
    Sprintf(buf,
	    "tar -c -z -C %s -f %s signature bexprs bools strings graph",
	    dir, file_name);
#endif
    if( system(buf) != 0 ) {
	Fail_pr("Command '%s' failed????", buf);
	return FALSE;
    }
    Sprintf(buf, "/bin/rm -rf %s", dir);
    int i  = system(buf);
    (void) i;
    return TRUE;
}

#if 1
#define RD_FAIL(msg) {							    \
		fprintf(stderr, "Load_graph failure %s around line %d\n",   \
				(msg), line);				    \
		goto rd_fail;						    \
    }
#else
#define RD_FAIL(msg) goto rd_fail;
#endif

bool
Load_graph(string type_sig, string file_name, g_ptr redex)
{
    Sprintf(buf, "%s/tmp_load_XXXXXX", Voss_tmp_dir);
    string dir = wastrsave(&strings, mkdtemp(buf));
    // Make files
    Sprintf(buf, "%s/signature", dir);
    string sig_file_name = wastrsave(&strings, buf);
    Sprintf(buf, "%s/bexprs", dir);
    string bexpr_file_name = wastrsave(&strings, buf);
    Sprintf(buf, "%s/bools", dir);
    string bool_file_name = wastrsave(&strings, buf);
    Sprintf(buf, "%s/strings", dir);
    string str_file_name = wastrsave(&strings, buf);
    Sprintf(buf, "%s/graph", dir);
    string graph_file_name = wastrsave(&strings, buf);
    // ------------- Untar file into tmp directory ------------------
    Sprintf(buf, " tar -C %s -x -z -f %s", dir, file_name);
    if( system(buf) != 0 ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to read %s", file_name));
	return(FALSE);
    }
    // ------------- Read and parse signature ------------------
    FILE *fp;
    if( (fp = fopen(sig_file_name, "r")) == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to open %s", sig_file_name));
	return(FALSE);
    }
    int g_node_cnt;
    if( fscanf(fp, "%d\n", &g_node_cnt) != 1 ) {
	fclose(fp);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Syntax error at line 1 in %s",
					  sig_file_name));
	return(FALSE);
    }
    string tmp = strtemp("");
    tmp = fgets(tmp, STR_BLOCK_SIZE-1, fp);
    fclose(fp);
    if( strncmp(tmp, type_sig, strlen(type_sig)) != 0 ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Type signature mismatch in %s",
					  file_name));
	return(FALSE);
    }
    // ------------- Read bools ------------------
    new_buf(&bool_results, 100, sizeof(formula));
    if( !Load_BDDs(bool_file_name, &bool_results) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to read BDDs in %s",
					  file_name));
	free_buf(&bool_results);
	return(FALSE);
    }
    // ------------- Read bexprs ------------------
    new_buf(&bexpr_results, 100, sizeof(bexpr));
    if( !Load_bexprs(bexpr_file_name, &bexpr_results) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to read bexprs in %s",
					  file_name));
	free_buf(&bool_results);
	free_buf(&bexpr_results);
	return(FALSE);
    }
    // ------------- Read strings ------------------
    new_buf(&str_results, 100, sizeof(string));
    if( (fp = fopen(str_file_name, "r")) == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to read exe file %s", buf));
	free_buf(&bool_results);
	free_buf(&bexpr_results);
#if 0
	free_buf(&fsm_results);
#endif
	free_buf(&str_results);
	return( FALSE );
    }
    while( !feof(fp) ) {
	int sz = 0;
	int c;
	while( (c = fgetc(fp)) != EOF && isdigit(c) ) {
	    sz = 10*sz+c-'0';
	}
	string r = strtemp("");
	for(int i = 0; i < sz; i++) {
	    if( (c = fgetc(fp)) == EOF ) {
		MAKE_REDEX_FAILURE(redex,
				   Fail_pr("Syntax error in %s on line %d",
					   str_file_name, i+1));
		free_buf(&bool_results);
		free_buf(&bexpr_results);
#if 0
		free_buf(&fsm_results);
#endif
		free_buf(&str_results);
		return( FALSE );
	    }
	    charappend(c);
	}
	r = wastrsave(&strings, r);
	push_buf(&str_results, &r);
	// Absorb newline
	fgetc(fp);
    }
    fclose(fp);
    // ------------- Read graph ------------------
    buffer gbuf;
    new_buf(&gbuf, g_node_cnt, sizeof(g_ptr));
    for(int i = 0; i < g_node_cnt; i++) {
	g_ptr np = Get_node();
	push_buf(&gbuf,&np);
    }
    if( (fp = fopen(graph_file_name, "r")) == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot open %s", graph_file_name));
	free_buf(&bool_results);
	free_buf(&bexpr_results);
#if 0
	free_buf(&fsm_results);
#endif
	free_buf(&str_results);
	free_buf(&gbuf);
	return( FALSE );
    }
    int line = 0;
    for(line = 0; line < g_node_cnt; line++) {
        int c = fgetc(fp);
        ungetc(c, fp);
        switch( c ) {
		int ig, il, ir;
		g_ptr g, l, r;
		string s;
		bexpr be;
		formula b;
		pointer p;
		ext_obj_ptr op;
	    case TAG_APPLY:
		if( fscanf(fp,"A %d %d %d\n", &ig, &il, &ir) != 3 )
		    RD_FAIL("TAG_APPLY");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		l = *((g_ptr *) FAST_LOC_BUF(&gbuf, il));
		r = *((g_ptr *) FAST_LOC_BUF(&gbuf, ir));
		MAKE_REDEX_APPL_ND(g, l, r);
		INC_REFCNT(l);
		INC_REFCNT(r);
		break;
	    case TAG_NIL:
		if( fscanf(fp,"0 %d\n", &ig) != 1 )
		    RD_FAIL("TAG_NIL");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		MAKE_REDEX_CONS_ND(g, NULL, NULL);
		break;
	    case TAG_CONS:
		if( fscanf(fp,"C %d %d %d\n", &ig, &il, &ir) != 3 )
		    RD_FAIL("TAG_CONS");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		l = *((g_ptr *) FAST_LOC_BUF(&gbuf, il));
		r = *((g_ptr *) FAST_LOC_BUF(&gbuf, ir));
		MAKE_REDEX_CONS_ND(g, l, r);
		INC_REFCNT(l);
		INC_REFCNT(r);
		break;
	    case TAG_INT:
		{
		    int sz;
		    if( fscanf(fp,"I %d %d ", &ig, &sz) != 2 )
			RD_FAIL("TAG_INT LENGTH");
		    g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		    string tmp = strtemp("");
		    char *p = tmp;
		    for(int i = 0; i < sz; i++) {
			if( (c = fgetc(fp)) == EOF )
			    RD_FAIL("TAG_INT DIGIT");
			*p = c;
			p++;
		    }
		    fgetc(fp);	// Eat newline
		    *p = 0;
		    MAKE_REDEX_AINT(g, Arbi_FromString(tmp, 10));
		    break;
		}
	    case TAG_STRING:
		if( fscanf(fp,"S %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_STRING");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_STRING(g, s);
		break;
	    case TAG_BOOL:
		if( fscanf(fp,"B %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_BOOL");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		b = *((formula *) FAST_LOC_BUF(&bool_results, il));
		MAKE_REDEX_BOOL(g, b);
		break;
	    case TAG_BEXPR:
		if( fscanf(fp,"E %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_BEXPR");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		be = *((bexpr *) FAST_LOC_BUF(&bexpr_results, il));
		MAKE_REDEX_BEXPR(g, be);
		break;
	    case TAG_FAIL:
		if( fscanf(fp,"F %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_FAIL");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_FAILURE(g, s);
		break;
	    case TAG_REF_VAR:
		{
		    if( fscanf(fp,"R %d %d\n", &ig, &il) != 2 )
			RD_FAIL("TAG_REF_VAR");
		    g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		    l = *((g_ptr *) FAST_LOC_BUF(&gbuf, il));
		    int ref_var = Make_RefVar();
		    Set_RefVar(ref_var,l);
		    INC_REFCNT(l);
		    MAKE_REDEX_PRIM_FN(g, P_REF_VAR);
		    SET_REF_VAR(g, ref_var);
		    break;
		}
	    case TAG_PRINTF:
		if( fscanf(fp,"! %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_PRINTF");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_PRIM_FN(g, P_PRINTF);
		SET_PRINTF_STRING(g, s);
		break;
	    case TAG_EPRINTF:
		if( fscanf(fp,"@ %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_EPRINTF");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_PRIM_FN(g, P_EPRINTF);
		SET_PRINTF_STRING(g, s);
		break;
	    case TAG_FPRINTF:
		if( fscanf(fp,"# %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_FPRINTF");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_PRIM_FN(g, P_FPRINTF);
		SET_PRINTF_STRING(g, s);
		break;
	    case TAG_SPRINTF:
		if( fscanf(fp,"$ %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_SPRINTF");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_PRIM_FN(g, P_SPRINTF);
		SET_PRINTF_STRING(g, s);
		break;
	    case TAG_CACHE:
		{
		    if( fscanf(fp,"= %d\n", &ig) != 1 )
			RD_FAIL("TAG_CACHE");
		    g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		    MAKE_REDEX_PRIM_FN(g, P_CACHE);
		    int ci = Make_g_cache();
		    SET_CACHE_TBL(g, ci);
		    break;
		}
	    case TAG_EXTAPI:
		if( fscanf(fp,"X %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_EXTAPI");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		MAKE_REDEX_EXTAPI(g, il);
		break;
	    case TAG_PRIM_FN:
		if( fscanf(fp,"P %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_PRIM_FN");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		MAKE_REDEX_PRIM_FN(g, il);
		break;
	    case TAG_VAR:
		if( fscanf(fp,"V %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_VAR");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		s = *((string *) FAST_LOC_BUF(&str_results, il));
		MAKE_REDEX_VAR(g, s);
		break;
	    case TAG_EXTOBJ:
		if( fscanf(fp,"O %d %d\n", &ig, &il) != 2 )
		    RD_FAIL("TAG_EXTOBJ");
		g = *((g_ptr *) FAST_LOC_BUF(&gbuf, ig));
		op = M_LOCATE_BUF(&ext_obj_buf, il);
		p = op->load_fn(fp);
		if( p == NULL ) {
		    fprintf(stderr,
			    "Load_graph failure for %s object around line %d\n",
			     op->name, line);
		    goto rd_fail;
		}
		MAKE_REDEX_EXT_OBJ(g, il, p);
		break;
	    default:
		RD_FAIL("UNKNOWN_TAG");
	}
    }
    g_ptr result = *((g_ptr *) FAST_LOC_BUF(&gbuf,0));
    OVERWRITE(redex, result);
    free_buf(&bool_results);
    free_buf(&bexpr_results);
#if 0
    free_buf(&fsm_results);
#endif
    free_buf(&str_results);
    free_buf(&gbuf);
    Sprintf(buf, "/bin/rm -rf %s", dir);
    int i = system(buf);
    (void) i;
    return( TRUE );

  rd_fail:
    MAKE_REDEX_FAILURE(redex,
	Fail_pr("Syntax error at line %d in %s", line+1, graph_file_name));
    free_buf(&bool_results);
    free_buf(&bexpr_results);
#if 0
    free_buf(&fsm_results);
#endif
    free_buf(&str_results);
    free_buf(&gbuf);
    Sprintf(buf, "/bin/rm -rf %s", dir);
    {
    int i = system(buf);
    (void) i;
    }
    return( FALSE );
}

#undef RD_FAIL

void
Init_io()
{
    aligned_new_mgr(&io_rec_mgr, sizeof(io_rec), 8);
    open_ios = NULL;
}

static void
c_mktemp(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string base = GET_STRING(r);
    string name;
    FILE *fp;
    if( !Mk_output_file_in_tmp_dir(base, &fp, &name) ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to create temporary file"));
	return;
    }
    io_ptr ip;
    ip = (io_ptr) new_rec(&io_rec_mgr);
    ip->name = name;
    ip->fp = fp;
    ip->is_pipe = FALSE;
    ip->mode = wastrsave(&strings, "w");
    ip->writable = TRUE;
    ip->next = open_ios;
    open_ios = ip;
    SET_TYPE(redex, LEAF);
    SET_LEAF_TYPE(redex, PRIM_FN);
    SET_PRIM_FN(redex, P_FILEFP);
    SET_FILE_IO_PTR(redex, ip);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
c_mktempd(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string base = GET_STRING(r);
    char buf[1024];
    if( *base == '/' ) {
	Sprintf(buf, "%s_XXXXXX", base);
    } else {
	Sprintf(buf, "%s/%s_XXXXXX", Voss_tmp_dir, base);
    }
    string res = mkdtemp(buf);
    if( res == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Failed to create temporary directory"));
	return;
    }
    MAKE_REDEX_STRING(redex, wastrsave(&strings, res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
file_exists(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string file = GET_STRING(r);
    if( access(file, F_OK) < 0 ) {
	MAKE_REDEX_BOOL(redex, B_Zero());
    } else {
	MAKE_REDEX_BOOL(redex, B_One());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
stream2filename(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    io_ptr ip = GET_FILE_IO_PTR(r);
    MAKE_REDEX_STRING(redex, ip->name);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Feof(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    io_ptr ip = GET_FILE_IO_PTR(r);
    if( feof(ip->fp) ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Fgets(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    io_ptr ip = GET_FILE_IO_PTR(r);

    if( index(ip->mode, 'r') == NULL && (index(ip->mode, '+') == NULL) ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("fgets on file not open for reading"));
    } else {
	string tmp = strtemp("");
	tmp = fgets(tmp, STR_BLOCK_SIZE-1, ip->fp);
	if( tmp != NULL ) {
	    MAKE_REDEX_STRING(redex, wastrsave(&strings, tmp));
	} else {
	    MAKE_REDEX_STRING(redex, wastrsave(&strings, ""));
	}
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Fget(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    io_ptr ip = GET_FILE_IO_PTR(r);
    if((index(ip->mode, 'r') == NULL) && (index(ip->mode, '+') == NULL)) {
        MAKE_REDEX_FAILURE(redex,
            Fail_pr("fgetdelim on file not open for reading"));
    } else {
        string tmp = strtemp("");
        size_t size;
        if(getdelim(&tmp, &size, '\0', ip->fp) != -1) {
            MAKE_REDEX_STRING(redex, wastrsave(&strings, tmp));
        } else {
            MAKE_REDEX_STRING(redex, wastrsave(&strings, ""));
        }
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
Io_Install_Functions()
{

    Add_ExtAPI_Function("mktemp", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_fp()),
			c_mktemp);

    Add_ExtAPI_Function("mktempd", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_string()),
			c_mktempd);

    Add_ExtAPI_Function("file_exists", "1", FALSE,
			GLmake_arrow(GLmake_string(), GLmake_bool()),
			file_exists);

    Add_ExtAPI_Function("stream2filename", "1", FALSE, 
			GLmake_arrow(GLmake_fp(), GLmake_string()),
			stream2filename);

    Add_ExtAPI_Function("feof", "1", FALSE, 
			GLmake_arrow(GLmake_fp(), GLmake_bool()),
			Feof);

    Add_ExtAPI_Function("fgets", "1", FALSE, 
			GLmake_arrow(GLmake_fp(), GLmake_string()),
			Fgets);

    Add_ExtAPI_Function("fget", "1", FALSE,
            GLmake_arrow(GLmake_fp(), GLmake_string()),
            Fget);

}


/****************************************************************************/
/*                          Local functions                                 */
/****************************************************************************/
static bool
check_arg(g_ptr *rootp, g_ptr **spp, int *depthp, int n, g_ptr redex, g_ptr arg)
{
    if( !is_fail(arg) ) return( TRUE );
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    OVERWRITE(redex, arg);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    *spp = *spp + n;
    *depthp = *depthp - n;
    *rootp = redex;
    return FALSE;
}

static bool
force_arg_and_check(g_ptr redex, g_ptr arg)
{
    g_ptr res = GET_APPLY_RIGHT(arg);
    res = force(res, FALSE);
    if( is_fail(res) ) {
        OVERWRITE(redex, res)
        return( FALSE );
    } else {
        return( TRUE );
    }
}


static void
make_redex_failure(g_ptr redex)
{
    SET_TYPE(redex, LEAF);
    SET_LEAF_TYPE(redex, PRIM_FN);
    SET_PRIM_FN(redex, P_FAIL);
    SET_FAIL_STRING(redex, wastrsave(&strings, FailBuf));
}

#define PFN2NAME(pfn) ((pfn == P_PRINTF)? "printf" : \
                       (pfn == P_SPRINTF)? "Sprintf" : \
                       (pfn == P_EPRINTF)? "eprintf" : \
                       (pfn == P_FPRINTF)? "fprintf" : \
                       "Unknown printf function???")

static void
neg_too_big(int i, int pfn, g_ptr redex)
{
    Fail_pr("Argument %d to %s is negative and does not fit in width\n",
	    i, PFN2NAME(pfn));
    make_redex_failure(redex);
}

static bool
prs(int pfn, int i, g_ptr redex, char *s, bool ljust, bool zfill, int size)
{
    int len = strlen(s);
    if( size > 0 ) {
        if( len > size ) {
            Fail_pr("Argument %d to %s (%s) does not fit in width %d\n",
		    i, PFN2NAME(pfn), s, size);
            make_redex_failure(redex);
            return( FALSE );
        }
    } else {
        size = len;
    }
    if( ljust )
        strappend(s);
    int fill = size-len;
    char c = (!ljust && zfill)? '0' : ' ';
    while( fill-- > 0 )
        charappend(c);
    if( !ljust )
        strappend(s);
    return( TRUE );
}

#undef PFN2NAME

static bool
print_list(int pfn, g_ptr l,int i,g_ptr redex,bool ljust, bool zfill, int size)
{
    if( IS_NIL(l) ) {
        strappend("[]");
    } else {
        char sep = '[';
        while( !IS_NIL(l) ) {
            charappend(sep);
            if( sep == ',' )
                charappend(' ');
            sep = ',';
            if( !prs(pfn,i,redex,GET_STRING(GET_CONS_HD(l)),
                     ljust, zfill, size) )
                return( FALSE );
            l = GET_CONS_TL(l);
        }
        charappend(']');
    }
    return( TRUE );
}

#define STR_EMIT(tag,s)					    \
    fprintf(fp, "%c %d %s\n", tag, res, (s));
#define INT_EMIT(tag,i)					    \
    fprintf(fp, "%c %d %d\n", tag, res, (i));
#define LONG_EMIT(tag,i)					    \
    fprintf(fp, "%c %d %ld\n", tag, res, (i));


static bool
can_be_saved(g_ptr np)
{
    if( find_hash(&g_save_tbl, (pointer) np) != NULL ) return( TRUE );
    insert_hash(&g_save_tbl, (pointer) np, (pointer) np);
  restart:
    switch( GET_TYPE(np) ) {
	case APPLY_ND:
	    {
		if( !can_be_saved(GET_APPLY_LEFT(np)) ) return( FALSE );
		np = GET_APPLY_RIGHT(np);
		goto restart;
	    }
	case CONS_ND:
	    {
		if( IS_NIL(np) ) return( TRUE );
		if( !can_be_saved(GET_CONS_HD(np)) ) return( FALSE );
		np = GET_CONS_TL(np);
		goto restart;
	    }
	case LEAF:
	    {
		int	i;
		ext_obj_ptr op;
		switch( GET_LEAF_TYPE(np) ) {
		    case INT:
			return( TRUE );
		    case STRING:
			return( TRUE );
		    case BOOL:
			return( TRUE );
		    case BEXPR:
			return( TRUE );
		    case EXT_OBJ:
			i = GET_EXT_OBJ_CLASS(np);
			op = M_LOCATE_BUF(&ext_obj_buf, i);
			if( op->save_fn == NULL ) {
			    Fail_pr("Cannot save type containing %s", op->name);
			    return( FALSE );
			} else {
			    return( TRUE );
			}

		    case PRIM_FN:
			switch( GET_PRIM_FN(np) ) {
			    case P_FAIL:
				return( TRUE );
			    case P_REF_VAR:
				{
				    int ref_var = GET_REF_VAR(np);
				    np = Get_RefVar(ref_var);
				    goto restart;
				}
			    case P_PRINTF:
				return( TRUE );
			    case P_FPRINTF:
				return( TRUE );
			    case P_SPRINTF:
				return( TRUE );
			    case P_EPRINTF:
				return( TRUE );
			    case P_CACHE:
				return( TRUE );
			    case P_EXTAPI_FN:
				return( TRUE );
			    default:
				return( TRUE );
			}
		    case VAR:
			return( TRUE );
		    default:
			DIE("Unexpected node type");
		}
	    }
	default:
	    DIE("Unexpected node type");
    }
}

static int
save_graph_rec(FILE *fp, g_ptr np)
{
    int res = g_save_cnt;
    // Optimization: Only put nodes with REFCNT > 0 into hash table
    // since they are the only ones that are shared...
    if( GET_REFCNT(np) != 0 ) {
	int ores;
	if( (ores = PTR2INT(find_hash(&g_save_tbl, (pointer) np))) != 0 ) {
	    return ores;
	}
	insert_hash(&g_save_tbl, (pointer) np, INT2PTR(res));
	g_save_cnt++;
    } else {
	g_save_cnt++;
    }
    switch( GET_TYPE(np) ) {
	case APPLY_ND:
	    {
		int l = save_graph_rec(fp, GET_APPLY_LEFT(np));
		int r = save_graph_rec(fp, GET_APPLY_RIGHT(np));
		fprintf(fp, "%c %d %d %d\n", TAG_APPLY, res, l, r);
		return( res );
	    }
	case CONS_ND:
	    {
		if( IS_NIL(np) ) {
		    fprintf(fp, "%c %d\n", TAG_NIL, res);
		    return( res );
		} else {
		    int l = save_graph_rec(fp, GET_CONS_HD(np));
		    int r = save_graph_rec(fp, GET_CONS_TL(np));
		    fprintf(fp, "%c %d %d %d\n", TAG_CONS, res, l, r);
		    return( res );
		}
	    }
	case LEAF:
	    {
		formula f;
		bexpr   b;
		int	i;
		string  s;
		ext_obj_ptr op;
		switch( GET_LEAF_TYPE(np) ) {
		    case INT:
			s = Arbi_ToString(GET_AINT(np), 10);
			fprintf(fp, "%c %d %d %s\n", TAG_INT, res,
				    (int) strlen(s), s);
			return( res );
		    case STRING:
			s = GET_STRING(np);
			INT_EMIT(TAG_STRING,get_uniq_buf_index(&string_buf,&s));
			return( res );
		    case BOOL:
			f = GET_BOOL(np);
			INT_EMIT(TAG_BOOL,get_uniq_buf_index(&bool_buf,&f));
			return( res );
		    case BEXPR:
			b = GET_BEXPR(np);
			INT_EMIT(TAG_BEXPR, get_uniq_buf_index(&bexpr_buf, &b));
			return( res );
		    case EXT_OBJ:
			i = GET_EXT_OBJ_CLASS(np);
			INT_EMIT(TAG_EXTOBJ, i);
			op = M_LOCATE_BUF(&ext_obj_buf, i);
			ASSERT(op->save_fn != NULL );
			op->save_fn(fp, GET_EXT_OBJ(np));
			return( res );
		    case PRIM_FN:
			switch( GET_PRIM_FN(np) ) {
			    case P_FAIL:
				s = GET_FAIL_STRING(np);
				INT_EMIT(TAG_FAIL,
					 get_uniq_buf_index(&string_buf, &s));
				return( res );
			    case P_REF_VAR:
				{
				    int ref_var = GET_REF_VAR(np);
				    g_ptr root = Get_RefVar(ref_var);
				    int ch = save_graph_rec(fp, root);
				    INT_EMIT(TAG_REF_VAR, ch);
				    return( res );
				}
			    case P_PRINTF:
				s = GET_PRINTF_STRING(np);
				INT_EMIT(TAG_PRINTF,
					get_uniq_buf_index(&string_buf,&s));
				return( res );
			    case P_FPRINTF:
				s = GET_PRINTF_STRING(np);
				INT_EMIT(TAG_FPRINTF,
					get_uniq_buf_index(&string_buf,&s));
				return( res );
			    case P_SPRINTF:
				s = GET_PRINTF_STRING(np);
				INT_EMIT(TAG_SPRINTF,
					get_uniq_buf_index(&string_buf,&s));
				return( res );
			    case P_EPRINTF:
				s = GET_PRINTF_STRING(np);
				INT_EMIT(TAG_EPRINTF,
					get_uniq_buf_index(&string_buf,&s));
				return( res );
			    case P_CACHE:
				fprintf(fp, "%c %d\n", TAG_CACHE, res);
				return( res );
			    case P_EXTAPI_FN:
				LONG_EMIT(TAG_EXTAPI, GET_EXTAPI_FN(np));
				return( res );
			    default:
				LONG_EMIT(TAG_PRIM_FN, GET_PRIM_FN(np));
				return( res );
			}
		    case VAR:
			STR_EMIT(TAG_VAR, GET_VAR(np));
			return( res );
		    default:
			DIE("Unexpected node type");
		}
	    }
	default:
	    DIE("Unexpected node type");
    }
}

#undef STR_EMIT
#undef INT_EMIT
