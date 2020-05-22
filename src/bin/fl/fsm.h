//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/********************************************************************
*                                                                   *
*     Original author: Carl-Johan Seger 2019                        *
*                                                                   *
*********************************************************************/
/* fsm.h -- header for fsm.c */
#ifdef EXPORT_FORWARD_DECL
/* --- Forward declarations that need to be exported to earlier .h files --- */
typedef struct ilist_rec    *ilist_ptr;

/* ----- Function prototypes for public functions ----- */
void	    Fsm_Init();
void	    Fsm_Install_Functions();

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef FSM_H
#define FSM_H
#include "fl.h"	/* Global data types and include files 		     */

typedef struct ilist_rec {
    int		from;
    int		to;
    int		size;
    ilist_ptr	next;
} ilist_rec;

typedef union {
    formula	f;
    bexpr	bp;
    arbi_T	ai;
		    }	gbv;

typedef struct vec_info_rec *vec_info_ptr;
typedef struct vec_info_rec {
    string	    local_name;
    string	    hierarchy;
    vec_ptr	    declaration;
    string	    signature;
    int		    size:31;
    unint	    transient:1;
    ilist_ptr	    map;
    string	    value_list;
    vec_info_ptr    next;
} vec_info_rec;

typedef struct vis_io_rec   *vis_io_ptr;
typedef struct vis_io_rec {
    string	f_vec;
    ilist_ptr	acts;
    vis_io_ptr	next;
} vis_io_rec;

typedef struct vis_rec	*vis_ptr;
typedef struct vis_rec {
    int		    draw_level;
    int		    id;
    string	    pfn;	// Draw function
    vis_io_ptr	    fa_inps;
    vis_io_ptr	    fa_outs;
} vis_rec;

typedef struct vis_list_rec *vis_list_ptr;
typedef struct vis_list_rec {
    vis_ptr	    vp;
    vis_list_ptr    next;
} vis_list_rec;


// FSM datastructure
typedef struct fsm_rec  *fsm_ptr;
typedef struct fsm_rec {
    int		mark;		// Used for g.c.
    fsm_ptr	next;		// Only used for free list
    // Real content
    string	sha256_sig;
    string	top_name;
    int		ranks;
    hash_record all_name_tbl;
    rec_mgr     vec_info_rec_mgr;
    rec_mgr     ilist_rec_mgr;
    rec_mgr	vec_rec_mgr;
    rec_mgr	range_rec_mgr;
    rec_mgr	idx_list_rec_mgr;
    buffer      nodes;
    buffer      composites;
    buffer	top_inps;
    buffer	top_outs;
    rec_mgr	vis_io_rec_mgr;
    rec_mgr	vis_rec_mgr;
    rec_mgr	vis_list_rec_mgr;
} fsm_rec;


typedef struct ncomp_rec *ncomp_ptr;

typedef void (*wl_op)(ncomp_ptr);

typedef struct idx_list_rec *idx_list_ptr;
typedef struct idx_list_rec {
    int		    idx;
    idx_list_ptr    next;
} idx_list_rec;

typedef struct mem_data_rec     *mem_data_ptr;
typedef struct mem_data_rec {
    int         addr_size;
    int         lines;
    int         data_size;
} mem_data_rec;

// Individual composites
typedef struct ncomp_rec {
        unint				size;
	unint				rank:30;
	unint				phase_delay:1;
	unint				flag:1;	// General purpose bit
	wl_op				op;
        union {
	    arbi_T       value;
	    string       name;
	    int          extension_size;
	    mem_data_rec mem;
	    idx_list_ptr idx_list;
        }				arg;
	ilist_ptr			inps;
	ilist_ptr			outs;
} ncomp_rec;

typedef struct nnode_rec    *nnode_ptr;
typedef struct nnode_rec {
    vec_info_ptr    vec;
    unint	    idx:27;	    // I think 134 million nodes are enough....
    unint	    has_phase_event:1;
    unint	    has_weak:1;
    unint	    has_ant:1;
    unint	    has_cons:1;
    unint	    has_trace:1;
    int		    composite:30;
    unint	    is_top_input:1;
    unint	    is_top_output:1;
    idx_list_ptr    fanouts;
    vis_list_ptr    draw_info;
} nnode_rec;


typedef enum {use_bdds, use_bexprs, use_ints}  value_type;

typedef enum {
		start_weak  = 0,
		start_ant   = 1,
		start_cons  = 2,
		start_trace = 3,
		end_weak    = 4,
		end_ant	    = 5,
		end_cons    = 6,
		end_trace   = 7
}	    event_type;

typedef struct event_rec    *event_ptr;
typedef struct event_rec {
    event_type	    type;
    int		    nd_idx;
    int		    time;
    gbv		    H;
    gbv		    L;
} event_rec;

typedef struct weak_rec  *weak_ptr;
typedef struct weak_rec {
    gbv	    when;
    int	    nd_idx;
    int	    from;
    int	    to;
} weak_rec;

typedef struct ant_rec  *ant_ptr;
typedef struct ant_rec {
    gbv	    when;
    int	    nd_idx;
    gbv	    value;
    int	    from;
    int	    to;
} ant_rec;


typedef struct trace_event_rec    *trace_event_ptr;
typedef struct trace_event_rec {
    int		    time;
    gbv		    H;
    gbv		    L;
    trace_event_ptr next;
} trace_event_rec;

typedef struct trace_rec    *trace_ptr;
typedef struct trace_rec {
    int		    nd_idx;
    trace_event_ptr events;
} trace_rec;


typedef struct ste_rec	    *ste_ptr;
typedef struct ste_rec {
    int		mark;		// For g.c.
    ste_ptr	next;		// Only used for free list
    // Real content
    fsm_ptr	fsm;
    value_type	type;
    bool	active;		// Currently being simulated
    int		max_time;	// Time to which it is run
    gbv         validTrajectory;
    gbv         checkTrajectory;
    gbv         assertion_OK;
    gbv         check_OK;
    hash_record	trace_tbl;
    rec_mgr	trace_event_rec_mgr;
    rec_mgr	trace_rec_mgr;
} ste_rec;

// Simulation state
typedef struct state_rec    *state_ptr;
typedef struct state_rec {
    buffer	values;
} state_rec;

typedef struct sch_rec		*sch_ptr;
typedef struct sch_list_rec	*sch_list_ptr;

typedef struct sch_list_rec {
    sch_ptr	    sch;
    sch_list_ptr    next;
} sch_list_rec;

typedef struct sch_rec {
    string	    vec;
    string	    pfn;
    sch_list_ptr    children;
} sch_rec;

typedef struct sch_inst_rec *sch_inst_ptr;
typedef struct sch_inst_rec {
    sch_ptr	    sch;
    hash_record	    done;
    buffer	    anon_buf;
    sch_inst_ptr    next;
} sch_inst_rec;


typedef struct vstate_rec	*vstate_ptr;
typedef struct vstate_rec {
    int		mark;		// For g.c.
    vstate_ptr	next;		// Only used for free list
    // Real content
    fsm_ptr	    fsm;	// Circuit
    hash_record	    stop_nds;
    hash_record	    ifc_nds;
    rec_mgr	    sch_rec_mgr;
    rec_mgr	    sch_list_rec_mgr;
    rec_mgr	    sch_inst_rec_mgr;
    //
    sch_inst_ptr    old_versions;
    sch_ptr	    sch;	// Draw tree
    hash_record	    done;
    buffer	    anon_buf;
} vstate_rec;

typedef struct node_comp_pair_rec   *node_comp_pair_ptr;
typedef struct node_comp_pair_rec {
    nnode_ptr	np;
    int		comp_idx;
} node_comp_pair_rec;

#endif /* FSM_H */
#endif /* EXPORT_FORWARD_DECL */
