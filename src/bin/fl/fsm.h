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
typedef struct vec_info_rec *vec_info_ptr;
typedef struct sch_rec	    *sch_ptr;
typedef struct fsm_rec	    *fsm_ptr;	    // ALLOCATE: get_fsm_rec()
typedef struct vstate_rec   *vstate_ptr;    // ALLOCATE: get_vstate_rec()
typedef struct ste_rec	    *ste_ptr;	    // ALLOCATE: get_ste_rec()
typedef struct idx_list_rec *idx_list_ptr;

/* ----- Function prototypes for public functions ----- */
string get_real_name(vec_info_ptr ip, int idx);
//
void	    Fsm_Init();
void	    Fsm_Install_Functions();
void	    write_ncomp_rec(FILE *fp, pointer p);
void	    read_ncomp_rec(FILE *fp, pointer p);
fsm_ptr	    get_fsm_rec();
ste_ptr	    get_ste_rec();
vstate_ptr  get_vstate_rec();
unint       ilist_ptr_hash(pointer key, unint size);
bool        ilist_ptr_equ(pointer k1, pointer k2);

#ifdef DEBUG
void	    dbg_print_ilist(string msg, ilist_ptr ip);
void	    dbg_print_sch_rec(sch_ptr sch, int indent);
#endif

#else /* EXPORT_FORWARD_DECL */
/* ----------------------- Main include file ------------------------------- */
#ifndef FSM_H
#define FSM_H
#include "fl.h"	/* Global data types and include files 		     */

typedef struct event_rec    *event_ptr;

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
} gbv;

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

typedef struct attr_list_rec	*attr_list_ptr;
typedef struct attr_list_rec {
    string	    name;
    string	    value;
    attr_list_ptr   next;
} attr_list_rec;

typedef struct vis_rec	*vis_ptr;
typedef struct vis_rec {
    int		    draw_level;
    int		    id;
    int		    pinst_cnt;
    string	    pfn;	// Draw function
    vis_io_ptr	    fa_inps;
    vis_io_ptr	    fa_outs;
    attr_list_ptr   attrs;
} vis_rec;

typedef struct vis_list_rec *vis_list_ptr;
typedef struct vis_list_rec {
    vis_ptr	    vp;
    vis_list_ptr    next;
} vis_list_rec;


// FSM datastructure
typedef struct fsm_rec {
    int		mark;		// Used for g.c.
    fsm_ptr	next;		// Only used for free list
    string	sha256_sig;
    string	top_name;
    int		ranks;
    hash_record all_name_tbl;	    // TYPE: string -> vec_info_ptr
    rec_mgr     vec_info_rec_mgr;   // TYPE: vec_info_rec
    rec_mgr     ilist_rec_mgr;	    // TYPE: ilist_rec
    rec_mgr	vec_rec_mgr;	    // TYPE: vec_rec
    rec_mgr	range_rec_mgr;	    // TYPE: range_rec
    rec_mgr	idx_list_rec_mgr;   // TYPE: idx_list_rec
    buffer      nodes;		    // TYPE: nnode_rec
    buffer      composites;	    // TYPE: ncomp_rec
    buffer	top_inps;	    // TYPE: string
    buffer	top_outs;	    // TYPE: string
    buffer	props;		    // TYPE: string
    rec_mgr	vis_io_rec_mgr;	    // TYPE: vis_io_rec
    rec_mgr	vis_rec_mgr;	    // TYPE: vis_rec
    rec_mgr	vis_list_rec_mgr;   // TYPE: vis_list_rec
    rec_mgr	attr_list_rec_mgr;  // TYPE: attr_list_rec
} fsm_rec;

typedef struct ncomp_rec *ncomp_ptr;

typedef void (*wl_op)(ncomp_ptr);

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

typedef union {
	    arbi_T       value;
	    string       name;
	    int          extension_size;
	    mem_data_rec mem;
	    idx_list_ptr idx_list;
} ncomp_args;

// Individual composites
typedef struct ncomp_rec {
        unint				size;
	unint				rank:29;
	unint				phase_delay:1;
	unint				no_weakening:1;
	unint				flag:1;	// General purpose bit
	wl_op				op;
	ncomp_args			arg;
	ilist_ptr			inps;
	ilist_ptr			outs;
} ncomp_rec;

typedef struct event_list_rec	*event_list_ptr;
typedef struct event_list_rec {
    event_ptr	    ep;
    event_list_ptr  next;
} event_list_rec;

typedef struct nnode_rec    *nnode_ptr;
typedef struct nnode_rec {
    vec_info_ptr    vec;
    unint	    idx:26;	    // I think 134 million nodes are enough....
    unint	    has_phase_event:1;
    unint	    has_ant_or_weak_change:1;
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

typedef struct event_rec {
    int		    event_id;
    event_type	    type;
    int		    nd_idx;
    int		    time;
    gbv		    H;	// SEL: current_type -> use_bdds use_bexprs use_ints
    gbv		    L;	// SEL: current_type -> use_bdds use_bexprs use_ints
} event_rec;

typedef struct trace_event_rec    *trace_event_ptr;
typedef struct trace_event_rec {
    int		    time;
    gbv		    H;	// SEL: current_type -> use_bdds use_bexprs use_ints
    gbv		    L;	// SEL: current_type -> use_bdds use_bexprs use_ints
    trace_event_ptr next;
} trace_event_rec;

typedef struct trace_rec    *trace_ptr;
typedef struct trace_rec {
    int		    nd_idx;
    trace_event_ptr events;
} trace_rec;


typedef struct ste_rec {
    int		mark;		// For g.c.
    ste_ptr	next;		// Only used for free list
    fsm_ptr	fsm;
    value_type	type;
    bool	active;		// Currently being simulated
    int		cur_time;
    bool	abort_ASAP;
    int		max_time;	 // Time to which it is run
    gbv         validTrajectory; // SEL: ->type -> use_bdds use_bexprs use_ints
    gbv         checkTrajectory; // SEL: ->type -> use_bdds use_bexprs use_ints
    gbv         assertion_OK;	 // SEL: ->type -> use_bdds use_bexprs use_ints
    gbv         check_OK;	 // SEL: ->type -> use_bdds use_bexprs use_ints
    hash_record	trace_tbl;		// TYPE: pint -> trace_ptr
    hash_record	active_weak_tbl;	// TYPE: pint -> event_list_ptr
    hash_record	active_ant_tbl;		// TYPE: pint -> event_list_ptr
    hash_record	active_cons_tbl;	// TYPE: pint -> event_list_ptr
    rec_mgr	trace_event_rec_mgr;	// TYPE: trace_event_rec
    rec_mgr	trace_rec_mgr;		// TYPE: trace_rec
    rec_mgr	event_rec_mgr;		// TYPE: event_rec
    buffer	event_buf;		// TYPE: event_ptr
    buffer	weakening_buf;		// TYPE: pformula
    rec_mgr	event_list_rec_mgr;	// TYPE: event_list_rec
} ste_rec;

// Simulation state
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
    hash_record	    done;		// TYPE: ilist_ptr -> string
    buffer	    anon_buf;		// TYPE: ilist_ptr
    sch_inst_ptr    next;
} sch_inst_rec;


typedef struct vstate_rec {
    int		mark;		// For g.c.
    vstate_ptr	next;		// Only used for free list
    fsm_ptr	    fsm;	// Circuit
    hash_record	    stop_nds;		// TYPE: pint -> pint
    hash_record	    ifc_nds;		// TYPE: pint -> pint
    rec_mgr	    sch_rec_mgr;	// TYPE: sch_rec
    rec_mgr	    sch_list_rec_mgr;	// TYPE: sch_list_rec
    rec_mgr	    sch_inst_rec_mgr;	// TYPE: sch_inst_rec
    sch_inst_ptr    old_versions;
    sch_ptr	    sch;	// Draw tree
    hash_record	    done;		// TYPE: ilist_ptr -> string
    buffer	    anon_buf;		// TYPE: ilist_ptr
} vstate_rec;

typedef struct node_comp_pair_rec   *node_comp_pair_ptr;
typedef struct node_comp_pair_rec {
    nnode_ptr	np;
    int		comp_idx;
} node_comp_pair_rec;

#define FOREACH_NODE(i, il) \
    for(ilist_ptr _l = il; _l != NULL; _l = _l->next) \
    for(int i = _l->from; abs(i-_l->from) < _l->size; (_l->from>_l->to)?i--:i++)

#endif /* FSM_H */
#endif /* EXPORT_FORWARD_DECL */
