//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#ifndef FL_H
#define FL_H
/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 1990                        *
*                                                                   *
*********************************************************************/
/* fl.h -- Main structures for fl */

#if 0
#define Sprintf(...)	{ fprintf(stderr, "Line %d in %s ", __LINE__,__FILE__); sprintf(__VA_ARGS__); fprintf(stderr, "ok\n"); }
#else
#define Sprintf(...)	sprintf(__VA_ARGS__);
#endif

/********************************************************************/
/*	    Version control (debug, paranoia, etc.)		    */
/********************************************************************/
#define PARANOIA 1		 // Turn on checking before doing graph ops.
#if 0
#define TRACK_FREEING 1		 // Keep the old value & location of freed node
#define NO_REFCNT_GC	1	 // Turn off reference counting g.c.
#define CHECK_REF_CNTS 1	 // Check every reachable nodes reference count
#define VERBOSE_DEC_REF_CNT 1	 // Verbose decrementation of reference counts
#define DONT_FREE_ND 1		 // Never free a node
#define NO_CACHE_HIT	1	 // Turn all memo tables into no-ops (miss only)
#define TRACE_FUNCTION_CALLS 1	 // Trace (user named) function calls
#define DO_GRAPH_COMPARISON 1	 // Compare two processes graphs at run time
#define DBG_TRACE_AND_SAVE 1	 // Save intemediate graphs & ops performed
#define COMPACT_BEXPR_PRINT 1	 // Print bexprs in compressed form
#define INCL_TYPE_HINT_IN_DRAW 1 // Add any type hints in draw_graph nodes
#define DONT_USE_FORCING_MARKS 1 // Don't mark nodes "forced"
#endif

#if TRACK_FREEING
#define Garbage_collect()	Do_garbage_collect(__FILE__, __LINE__)
#else
#define Garbage_collect()	Do_garbage_collect()
#endif

#if TRACK_FREEING
#define PRIM_DEC_REF_CNT(np)	dec_ref_cnt((np), __FILE__,__LINE__);
#define PRIM_DEC_REF_CNT2(np)	dec_ref_cnt((np), file, line);
#else
#define PRIM_DEC_REF_CNT(np)	dec_ref_cnt(np);
#define PRIM_DEC_REF_CNT2(np)	dec_ref_cnt(np);
#endif

#if VERBOSE_DEC_REF_CNT
#define DEC_REF_CNT(nd)	    DPR((nd)); PRIM_DEC_REF_CNT((nd));
#define DEC_REF_CNT2(nd)    DPR((nd)); PRIM_DEC_REF_CNT2((nd));
#else
#define DEC_REF_CNT(nd)	    PRIM_DEC_REF_CNT((nd));
#define DEC_REF_CNT2(nd)    PRIM_DEC_REF_CNT2((nd));
#endif

/********************************************************************/

/* All the standard include files that are used in fl */
#include <stdio.h>
#include <strings.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdarg.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <time.h>

#if __GNUC__
#    define STACK_BASED
#endif

#include "types.h"
#include "alloc.h"
#include "morestr.h"
#include "rec.h"
#include "buf.h"
#include "hash.h"
#include "strmgr.h"
#include "readrc.h"
#include "timer.h"
#include "uniq_buf.h"

#define IGNORE_RETURN(f)    ((void) (f));

#if DBG_TRACE_AND_SAVE
#define DBG_TST(...)							    \
	Sprintf(dbg_tst_buf, __VA_ARGS__);				    \
	dbg_tst_line_cnt++;						    \
	if( do_write ) fputs(dbg_tst_buf, dbg_fp); else {		    \
	    fgets(dbg_rd_buf, 100, dbg_fp);				    \
	    if( strcmp(dbg_tst_buf, dbg_rd_buf) != 0 ) {		    \
		fprintf(stderr,"BAD_FAIL around line %d\n",dbg_tst_line_cnt); \
		fprintf(stderr,"with dbg_gr_save_cnt=%d\n", dbg_gr_save_cnt); \
		fprintf(stderr, "|%s| != |%s|\n", dbg_tst_buf,dbg_rd_buf);  \
		Dbg_stop();						    \
		exit(-1);						    \
	    }								    \
	}

#else
#define DBG_TST(...)
#endif


#define CHECK_FOR_INTERRUPT if( Interrupt_asap ) Serve_Interrupt();
#define VOID	void
#define public	/* nothing */

#define PTR2INT(p)      ((int) ((long int)(p)))
#define INT2PTR(p)      ((pointer) ((long int)(p)))
#define PTR2UINT(p)     ((int) ((lunint)(p)))
#define UINT2PTR(p)     ((pointer) ((lunint)(p)))

#define EXPORT_FORWARD_DECL
/* First fl.h's forward declarations */

/* Destinations for output */
typedef enum {FILE_fp,
              stdout_fp,
              sim_fp,
              err_fp,
              warning_fp,
              fl_gc_fp,
              bdd_gc_fp} odests;

/* Type definitions that are needed at a higher level than the include files */
typedef struct typeExp_rec	*typeExp_ptr;

/* Simulation status types */
typedef enum {Stable, Unstable, Abort}  step_ret_vals;


string		Get_DIR(string fullname);
FILE*		Tryopen(string name, string mode, string *full_name);
void            Emit_prompt(const char *pre);
unsigned int	Ustr_hash(pointer np, unsigned int n);
bool		Ustr_equ(pointer p1, pointer p2);
void            Set_default_break_handler();
void            Exit(int status);
string          protect(string txt);
string          unprotect(string txt);
void            Info_to_tcl(string cmd);
bool            Send_to_tcl(string cmd, string *resp);
void            Serve_Interrupt();
bool		Mk_output_file_in_tmp_dir(string prefix, FILE **fpp,
					  string *filename);
void		Tcl_printf (FILE *tcl_fp, const string format, ...);

/* Some forward declarations that are needed early.... */
typedef struct g_rec            *g_ptr;
typedef struct name_list_rec	*name_list_ptr;
typedef struct impl_arg_rec	*impl_arg_ptr;

/* Then include the forward declarations in local .h files */
#include	"prefs_ext.h"
#include	"error.h"
#include	"symbol_tbl.h"
#include	"file_ops.h"
#include	"arb_prec.h"
#include	"sha256.h"
#include	"new_bdd.h"
#include	"initialize.h"
#include	"graph.h"
#include	"symbol.h"
#include	"typecheck.h"
#include	"language.h"
#include	"emit.h"
#include	"lp.h"
#include	"cache.h"
#include	"compile.h"
#include	"expand_cursor.h"
#include	"io.h"
#include	"remote_tcl.h"
#include	"bexpr.h"
#include	"minisat_ifc.h"
#include	"draw_graph.h"
#include	"debug.h"
#include	"voss_strings.h"
#include	"system.h"
#include	"int_ops.h"
#include	"list_ops.h"
#include	"float.h"
#include	"bv.h"
#include        "bev.h"
#include        "fsm.h"
#include        "table.h"
#include        "image.h"
#include        "plugin_loader.h"
#include        "iso.h"
#include        "pexlif.h"
#include        "doc.h"
#include        "serialize.h"

/* Include function prototypes */
#undef EXPORT_FORWARD_DECL

typedef struct name_list_rec {		/* List of valid node names	*/
	bool		    used;
    string          name;           /* Valid name for this node */
    name_list_ptr   next;
} name_list_rec;

typedef struct impl_arg_rec {
	bool		    used;
    string		    name;   	    /* Valid name for this node */
	fn_ptr		    def;
	typeExp_ptr	    type;	        /* General type for this node */
    impl_arg_ptr	next;
} impl_arg_rec;

/*  ========= Generally useful macros ============ */

/* Macros for manipulating flags in an attribute field */
#define FLAG(flag, field)	(((field) & flag) != 0)
#define SET_FLAG(flag, field)	((field) |= flag)
#define RESET_FLAG(flag, field)	((field) &= ~flag)

/* Macros to get and set specific Boolean functions */

#define GET_VAL(idx)	(*((formula *) M_LOCATE_BUF(&ValBuf, (idx))))
#define SET_VAL(idx,f)	{						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,(idx),(pointer)&t);   	    \
			}
#define GET_OLD_H(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start))))
#define SET_OLD_H(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start),(pointer)&t);   \
			}
#define GET_OLD_L(np)	(*((formula *) M_LOCATE_BUF(&ValBuf,\
						    ((np)->start+(unint)1))))
#define SET_OLD_L(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)1),(pointer)&t); \
			}
#define GET_NEW_H(np)	(*((formula *) M_LOCATE_BUF(&ValBuf,		    \
						    ((np)->start+(unint)2))))
#define SET_NEW_H(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,				    \
				      ((np)->start+(unint)2),(pointer)&t); \
			}
#define GET_NEW_L(np)	(*((formula *) M_LOCATE_BUF(&ValBuf,		    \
						    ((np)->start+(unint)3))))
#define SET_NEW_L(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,				    \
				      ((np)->start+(unint)3),(pointer)&t); \
			}
#define GET_ASS_H(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)4))))
#define SET_ASS_H(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)4),(pointer)&t); \
			}
#define GET_ASS_L(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)5))))
#define SET_ASS_L(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)5),(pointer)&t); \
			}
#define GET_CHK_H(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)6))))
#define SET_CHK_H(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)6),(pointer)&t); \
			}
#define GET_CHK_L(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)7))))
#define SET_CHK_L(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)7),(pointer)&t); \
			}

#define GET_INV_H(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)8))))
#define SET_INV_H(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)8),(pointer)&t); \
			}
#define GET_INV_L(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)9))))
#define SET_INV_L(np,f) {						    \
			    formula t=(f);				    \
			    store_buf(&ValBuf,((np)->start+(unint)9),(pointer)&t); \
			}

#define GET_WEAK_H(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)10))))
#define SET_WEAK_H(np,f) {						     \
			    formula t=(f);				     \
			    store_buf(&ValBuf,((np)->start+(unint)10),(pointer)&t); \
			}
#define GET_WEAK_L(np)	(*((formula *) M_LOCATE_BUF(&ValBuf, ((np)->start+(unint)11))))
#define SET_WEAK_L(np,f) {						     \
			    formula t=(f);				     \
			    store_buf(&ValBuf,((np)->start+(unint)11),(pointer)&t); \
			}

#define NBR_H_DELS(np)	( MAX((np)->H_up, (np)->H_down) )
#define NBR_L_DELS(np)	( MAX((np)->L_up, (np)->L_down) )


#define GET_DEL_H(np,i)	(*((formula *)M_LOCATE_BUF(&ValBuf,((np)->start+(unint)12+i))))
#define SET_DEL_H(np,i,f)   {						       \
				formula t=(f);				       \
				store_buf(&ValBuf,		               \
					  ((np)->start+(unint)12+i),		       \
					  (pointer)&t); 		       \
			    }

#define GET_DEL_L(np,i)							   \
	  (*((formula*)M_LOCATE_BUF(&ValBuf,((np)->start+NBR_H_DELS(np)+(unint)13+i))))

#define SET_DEL_L(np,i,f)   {						       \
				formula t=(f);				       \
				store_buf(&ValBuf,			       \
					  ((np)->start+NBR_H_DELS(np)+(unint)13+i),   \
					  (pointer)&t);			       \
			    }



/* Perform an AND/OR and replace old value */
#define AND_IN(old, f) { (old) = B_And((old), (f)); }

#define OR_IN(old, f)  { (old) = B_Or((old), (f)); }

/* Update trajectory, check, etc. formulae */
#define UPDATE_ASSERT_OK(h,l)	  AND_IN(Assertion_OK, B_Or((h),(l)));
#define UPDATE_CHECK_OK(h,l)	  AND_IN(Check_OK, B_Or((h),(l)));

#define UPDATE_TRAJ(H, L)	  AND_IN(ValidTrajectory, B_Or((H),(L)));
#define UPDATE_CHK(vH,vL,cH,cL)	{					       \
			  AND_IN(CheckTrajectory, B_Or(B_Not(vH), cH));\
			  AND_IN(CheckTrajectory, B_Or(B_Not(vL), cL));\
				}

/* Print out the (fatal) error message and exit */
#define DIE(...) while (1) {					             \
	fprintf(stderr, "Fatal error (line %d in %s):", __LINE__, __FILE__); \
        FP(err_fp, "Fatal error (line %d in %s): ", __LINE__, __FILE__);     \
	FP(err_fp, __VA_ARGS__);					     \
	fprintf(stderr, __VA_ARGS__);					     \
	if( RCadd_debug_info ) {					     \
	    FP(err_fp, "\n%s",						     \
		       Get_stack_trace(RCmax_stack_trace_entries));	     \
	    fprintf(stderr, "\n%s",					     \
		       Get_stack_trace(RCmax_stack_trace_entries));	     \
	}								     \
	exit(-1);							     \
};

/* Return the smallest(largest) of two values */
#define MIN(a, b)		(((a) < (b))? a : b)
#define MAX(a, b)		(((a) < (b))? b : a)

#define STREQ(a,b)		((a) == (b))

#endif /* FL_H */
