//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*									*/
/*		Original author: Carl-Johan Seger, 2019			*/
/*									*/
/************************************************************************/
#include "voss_strings.h"	// Need the vector/node name data structures
#include "fsm.h"
#include "graph.h"
#include "pexlif.h"

/* ------------- Global variables -------------- */
value_type		current_type;
value_type		old_type;

/********* Global variables referenced ***********/
extern symbol_tbl_ptr	symb_tbl;
extern str_mgr		*stringsp;
extern jmp_buf		*start_envp;
extern bool		gui_mode;
extern bool		use_stdout;
extern char		FailBuf[4096];
extern bool		RCverbose_fsm_print;
extern bool		RC_print_warnings_toplevel_fa_mismatch;
extern bool		RCnotify_traj_failures;
extern bool		RCnotify_check_failures;
extern bool		RCnotify_OK_A_failures;
extern bool		RCnotify_OK_C_failures;
extern int		RCmax_nbr_errors;
extern int		RCStep_limit;
extern bool		RCprint_failures;
extern bool		RCverbose_ste_run;
extern bool		RCprint_time;
extern int		RCprint_time_mod;
extern bool		RCaccurate_ite_comp;
extern bool		RCaccurate_hierachy_visualization;
extern g_ptr		void_nd;
extern bool		Do_gc_asap;
extern FILE		*odests_fp;
//
extern string s_PINST;
extern string s_P_HIER;
extern string s_P_LEAF;
extern string s_W_UPDATE_FN;
extern string s_W_PHASE_DELAY;
extern string s_W_X;
extern string s_W_CONST;
extern string s_W_NAMED_CONST;
extern string s_W_VAR;
extern string s_W_EXPLICIT_VAR;
extern string s_W_AND;
extern string s_W_LAT_LEQ;
extern string s_W_OR;
extern string s_W_NOT;
extern string s_W_PRED;
extern string s_W_EQ;
extern string s_W_GR;
extern string s_W_ADD;
extern string s_W_SUB;
extern string s_W_MUL;
extern string s_W_DIV;
extern string s_W_MOD;
extern string s_W_SHL;
extern string s_W_SHR;
extern string s_W_ASHR;
extern string s_W_SX;
extern string s_W_ZX;
extern string s_W_ITE;
extern string s_W_SLICE;
extern string s_W_NAMED_SLICE;
extern string s_W_UPDATE_NAMED_SLICE;
extern string s_W_CAT;
extern string s_W_IDELAY;
extern string s_W_MEM_READ;
extern string s_W_MEM_WRITE;
extern string s_MEM;
//
extern string s_no_instance;

/***** PRIVATE VARIABLES *****/
static string_op_ptr	sop;
static bool	    print_failures;
static int	    event_id_cnt = 0;
static int	    undeclared_node_cnt;
static bool         quit_simulation_early;
static int	    nbr_errors_reported;
static jmp_buf	    node_map_jmp_env;
static jmp_buf	    event_jmp_env;
static hash_record  *new_tbl;
static int	    sch2tcl_cnt = 0;
static string	    s_use_bdds;
static string	    s_use_bexprs;
static string	    s_use_ints;
static string	    s_DummyOut;
static string	    s_draw_repeat;
static string	    s_ES;

static string	    s_SCH_INT;
static string	    s_SCH_LEAF;
static int	    visualization_id;
static rec_mgr	    fsm_rec_mgr;
static int          fsm_oidx;
static typeExp_ptr  fsm_handle_tp;
static fsm_ptr	    fsm_free_list = NULL;
//
static rec_mgr	    ste_rec_mgr;
static int          ste_oidx;
static typeExp_ptr  ste_handle_tp;
static ste_ptr	    ste_free_list = NULL;
//
static rec_mgr	    vstate_rec_mgr;
static int          vstate_oidx;
static typeExp_ptr  vstate_handle_tp;
static vstate_ptr   vstate_free_list = NULL;
//
static rec_mgr	    node_comp_pair_rec_mgr;
static hash_record  node_comp_pair_tbl;
//
static hash_record  *all_name_tblp;
static rec_mgr      *vec_info_rec_mgrp;
static rec_mgr      *ilist_rec_mgrp;
static rec_mgr      *vec_rec_mgrp;	// %%%%%%%%%%%%%%%%%%%%
static rec_mgr      *range_rec_mgrp;	// %%%%%%%%%%%%%%%%%%%%
static rec_mgr      *idx_list_rec_mgrp;
static rec_mgr	    *vis_io_rec_mgrp;
static rec_mgr	    *vis_rec_mgrp;
static rec_mgr	    *vis_list_rec_mgrp;
static rec_mgr	    *attr_list_rec_mgrp;
static buffer	    attr_buf;
static buffer	    *nodesp;
static buffer	    *compositesp;
static buffer	    *top_inpsp;
static buffer	    *top_outsp;
static buffer	    *propsp;
static hash_record  *old_all_name_tblp;
static rec_mgr      *old_vec_info_rec_mgrp;
static rec_mgr      *old_ilist_rec_mgrp;
static rec_mgr      *old_vec_rec_mgrp;
static rec_mgr      *old_range_rec_mgrp;
static rec_mgr      *old_idx_list_rec_mgrp;
static rec_mgr	    *old_vis_io_rec_mgrp;
static rec_mgr	    *old_vis_rec_mgrp;
static rec_mgr	    *old_vis_list_rec_mgrp;
static rec_mgr	    *old_attr_list_rec_mgrp;
static buffer	    *old_nodesp;
static buffer	    *old_compositesp;
static buffer	    *old_top_inpsp;
static buffer	    *old_top_outsp;
static buffer	    *old_propsp;
static ilist_ptr    idx_map_result;
static ilist_ptr    im_cur;
static hash_record  idx_list_uniq_tbl;
static hash_record  bb_tbl;;
static buffer	    *sim_wheel_bufp;
//
static hash_record  *active_weak_tblp;
static hash_record  *old_active_weak_tblp;
static hash_record  *active_ant_tblp;
static hash_record  *old_active_ant_tblp;
static hash_record  *active_cons_tblp;
static hash_record  *old_active_cons_tblp;
static hash_record  *trace_tblp;
static hash_record  *old_trace_tblp;
static rec_mgr	    *event_list_rec_mgrp;
static rec_mgr	    *old_event_list_rec_mgrp;
static rec_mgr	    *trace_event_rec_mgrp;
static rec_mgr	    *old_trace_event_rec_mgrp;
static rec_mgr	    *trace_rec_mgrp;
static rec_mgr	    *old_trace_rec_mgrp;
static rec_mgr	    *event_rec_mgrp;
static rec_mgr	    *old_event_rec_mgrp;
static buffer	    *weakening_bufp;
static buffer	    *event_bufp;
static buffer	    *old_weakening_bufp;
static buffer	    *old_event_bufp;
//
static gbv	    *weak_buf;
static gbv	    *ant_buf;
static gbv	    *cons_buf;
static gbv	    *cur_buf;
static gbv	    *next_buf;
//
static string	    s_fsm;
static string	    s_ste;

static void	    (*cHL_Print)(odests fp, gbv a, gbv b);
static void	    (*c_Print)(odests fp, gbv a, int size);
static gbv	    (*c_NOT)(gbv a);
static gbv	    (*c_AND)(gbv a, gbv b);
static gbv	    (*c_OR)(gbv a, gbv b);
static bool	    (*c_NEQ)(gbv a, gbv b);
static bool	    (*c_isX)(gbv H, gbv L);
static bool	    (*c_isT)(gbv H, gbv L);
static gbv	    c_ZERO;
static gbv	    c_ONE;
static buffer	    inps_buf;
static buffer	    outs_buf;
static buffer	    tmps_buf;
static gbv	    *gbv_inps;
static gbv	    *gbv_outs;
static gbv	    *gbv_tmps;

static char	    ihier_buf[4096];
static char	    tmp_name_buf[4096];
static char	    buf[4096];
static int	    temporary_node_cnt = 0;
static int	    max_rank = 1;

static int	    BDD_size_limit;
static bool	    information_flow_weakening;

static string state_holding_names [] = {
	"draw_adff",
	"draw_adffe",
	"draw_ah_latch",
	"draw_al_latch",
	"draw_dff",
	"draw_dffe",
	"draw_dffsr",
	"draw_dffsre",
	"draw_elastic_re_ff",
	"draw_ff_fe",
	"draw_ff_fe_reset",
	"draw_ff_fe_reset_set",
	"draw_ff_fe_set",
	"draw_ff_fe_with_en",
	"draw_ff_fe_with_en_reset",
	"draw_ff_fe_with_en_reset_set",
	"draw_ff_fe_with_en_set",
	"draw_ff_re",
	"draw_ff_re_reset",
	"draw_ff_re_reset2",
	"draw_ff_re_reset_set",
	"draw_ff_re_set",
	"draw_ff_re_with_en",
	"draw_ff_re_with_en_reset",
	"draw_ff_re_with_en_reset_set",
	"draw_ff_re_with_en_set",
	"draw_master_slave",
	"draw_neg_d_latch",
	"draw_pos_d_latch",
	"draw_pos_d_latchbig",
	"draw_pos_d_latchen",
	"draw_pos_d_latch_reset",
	"draw_sdff",
	"draw_sdffce",
	"draw_sdffe"
};
static hash_record  state_holding_tbl;

/* ----- Forward definitions local functions ----- */
static void           save_fsm_fn(FILE *fp, pointer p);
static pointer        load_fsm_fn(FILE *fp);
static void           save_ste_fn(FILE *fp, pointer p);
static pointer        load_ste_fn(FILE *fp);
static void           save_vstate_fn(FILE *fp, pointer p);
static pointer        load_vstate_fn(FILE *fp);
static unint          ni_pair_hash(pointer key, unint size);
static bool           ni_pair_equ(pointer k1, pointer k2);
static void           cp_done(pointer key, pointer data);
static sch_list_ptr   copy_sch_list(vstate_ptr vp, sch_list_ptr sl);
static sch_ptr        copy_sch(vstate_ptr vp, sch_ptr sch);
static void           push_undo_point(vstate_ptr vp);
static void           pop_undo_point(vstate_ptr vp);
static vstate_ptr     mk_vstate(fsm_ptr fsm);
static g_ptr          ilist2nds(ilist_ptr il);
static vec_ptr        split_vector_name(string_op_ptr sop, string name);
static string         get_vector_signature(vec_ptr vp);
static fsm_ptr        create_fsm();
static gbv            GET_GBV(g_ptr nd);
static void           SET_GBV(g_ptr nd, gbv value);
static void           MAKE_REDEX_GBV(g_ptr redex, gbv value);
static g_ptr          Make_GBV_leaf(gbv value);
static void           extract_five_tuple(g_ptr nd, gbv *whenp, string *namep,
                                         gbv *val, int *fromp, int *top);
static void           extract_four_tuple(g_ptr nd, gbv *whenp, string *namep,
                                         int *fromp, int *top);
static void           extract_tripple(g_ptr nd, string *namep, int *fromp,
                                      int *top);
static int            event_cmp(const void *p1, const void *p2);
static bool           create_event_buffer(ste_ptr ste, buffer *ebufp, g_ptr wl,
                                          g_ptr ant, g_ptr cons, g_ptr trl,
                                          bool abort_ASAP);
static void           process_event(buffer *event_bufp, int time);
static void           record_trace(int idx, int time, gbv H, gbv L);
static ste_ptr        create_ste(fsm_ptr fsm, value_type type);
static void           push_ste(ste_ptr ste);
static void           pop_ste();
static void           push_fsm_env(fsm_ptr fsm);
static void           pop_fsm_env();
static void           push_fsm(fsm_ptr fsm);
static void           pop_fsm();
static void           mark_fsm_fn(pointer p);
static void           gbv_mark(gbv value);
static void           mark_trace_entries(pointer key, pointer data);
static void           mark_ste_fn(pointer p);
static void           mark_vstate_fn(pointer p);
static void           sweep_vstate_fn(void);
static void           sweep_fsm_fn(void);
static void           sweep_ste_fn(void);
static formula        ste_eq_fn(pointer p1, pointer p2, bool identical);
static unint	      ste_hash_fn(pointer p, unint n);
static pointer        ste_gmap_fn(gmap_info_ptr ip, pointer a);
static pointer        ste_gmap2_fn(gmap_info_ptr ip, pointer a, pointer b);
static unint          idx_list_hash(pointer key, unint n);
static bool           idx_list_equ(pointer k1, pointer k2);
static string         op2str(ncomp_ptr cp);
static void           print_nodes(odests fp, fsm_ptr fsm);
static void           print_composites(odests fp);
static string         anon2real(vstate_ptr vp, string aname);
static void           print_sch_tree(vstate_ptr vp, int indent, sch_ptr sch);
static string         vstate2str_fn(pointer p);
static string         sch2tcl(g_ptr *tlp, sch_ptr sch);
static string         fsm2str_fn(pointer p);
static string         compute_sha256_signature(fsm_ptr fsm);
static formula        fsm_eq_fn(pointer p1, pointer p2, bool identical);
static unint	      fsm_hash_fn(pointer p, unint n);
static string         ste2str_fn(pointer p);
static int            get_wexpr_size(g_ptr we);
static ilist_ptr      ilist_copy(ilist_ptr ip);
static ilist_ptr      make_input_arg(g_ptr we, int sz, hash_record *vtblp,
                                     string hier, bool pdel);
static idx_list_ptr   find_insert_idx(idx_list_ptr ip);
static void           add_fanout(nnode_ptr np, int comp_idx);
static void           add_fanouts(ilist_ptr inps, int comp_idx);
static bool           compile_expr(hash_record *vtblp, string hier,
                                   ilist_ptr outs, g_ptr we, bool pdel);
static string         mk_vec_name(string base, int sz);
static ilist_ptr      get_lhs_indices(hash_record *vtblp, string hier, g_ptr e);
static void           report_source_locations(odests dfp);
static ilist_ptr      declare_vector(hash_record *vtblp, string hier,
                                     string name, bool transient,
                                     ilist_ptr act_map, string value_list);
static int            find_node_index(vec_ptr decl, vec_ptr vp, int start);
static string         idx2name(int idx);
static bool           idx_is_user_defined(int idx);
static bool           ilist_is_user_defined(ilist_ptr il);
static ilist_ptr      vec2indices(string name);
static int            name2idx(string name);
static ilist_ptr      map_vector(hash_record *vtblp, string hier, string name,
                                 bool ignore_missing);
static int            vec_size(vec_ptr vec);
static int            get_stride(vec_ptr vp);
static bool           inside(int i, int upper, int lower);
static int            find_index_from_end(int i, range_ptr rp);
static bool           is_full_range(vec_ptr v1, vec_ptr v2);
static int            ilist_sel(ilist_ptr ip, int idx);
static void           map_node(vec_ptr decl, vec_ptr ivec, ilist_ptr map,
                               int cur);
static int            compute_ilist_length(ilist_ptr l);
static ilist_ptr      ilist_append(ilist_ptr l1, ilist_ptr l2);
static ilist_ptr      append_range(ilist_ptr l, int i_from, int i_to);
static ilist_ptr      translate_range(ilist_ptr map, int from, int to);
static void           geq_fn(int size, gbv *av, gbv *bv, gbv *resv) ;
static void           sub_fn(int size, gbv *av, gbv *bv, gbv *resv) ;
static void           ITE_fn(int size, gbv cH, gbv cL, gbv *av, gbv *bv,
                             gbv *resv);
static void           shift_left_by_1(int size, gbv *vp, gbv iH, gbv iL,
                                      gbv *resp);
static void           sshl_fun(int size, gbv *vp, int cnt, gbv *resp);
static void           sshr_fun(int size, gbv *vp, int cnt, gbv *resp);
static void           sashr_fun(int size, gbv *vp, int cnt, gbv *resp);
static void           add_op_todo(ncomp_ptr cp);
static void           do_phase(ste_ptr ste);
static int            do_combinational(ste_ptr ste);
static int            do_wl_op(ste_ptr ste, ncomp_ptr op);
static void           op_X(ncomp_ptr op);
static void           op_CONST(ncomp_ptr op);
static void           op_VAR(ncomp_ptr op);
static void           op_LAT_LEQ(ncomp_ptr op);
static void           op_AND(ncomp_ptr op);
static void           op_OR(ncomp_ptr op);
static void           op_NOT(ncomp_ptr op);
static void           op_EQ(ncomp_ptr op);
static void           op_GR(ncomp_ptr op);
static void           op_ADD(ncomp_ptr op);
static void           op_SUB(ncomp_ptr op);
static void           op_MUL(ncomp_ptr op);
static void           op_ITE(ncomp_ptr op);
static void           reset_tmps(int cnt);
static void           op_DIV(ncomp_ptr op);
static void           op_MOD(ncomp_ptr op);
static void           op_SHL(ncomp_ptr op);
static void           op_SHR(ncomp_ptr op);
static void           op_ASHR(ncomp_ptr op);
static void           op_SX(ncomp_ptr op);
static void           op_ZX(ncomp_ptr op);
static void           op_SLICE(ncomp_ptr op);
static void           op_UPDATE_SLICE(ncomp_ptr op);
static void           op_WIRE(ncomp_ptr op);
static void           op_MEM_READ(ncomp_ptr op);
static void           op_MEM_WRITE(ncomp_ptr op);
static void	      op_IDELAY(ncomp_ptr op);
static bool           traverse_pexlif(hash_record *parent_tblp, g_ptr p,
                                      string hier, bool top_level,
                                      int draw_level, int inst_cnt,
                                      int max_depth);
static bool           no_fanout(ncomp_ptr cp);
static int            assign_rank(int depth, ncomp_ptr cp);
static int            rank_order();
static void           bexpr_c_Print(odests fp, gbv a, int size);
static bool	      bexpr_c_isX(gbv H, gbv L);
static bool	      bexpr_c_isT(gbv H, gbv L);
static bool	      BDD_c_isX(gbv H, gbv L);
static bool	      BDD_c_isT(gbv H, gbv L);
static void           BDD_c_Print(odests fp, gbv a, int size);
static gbv            bexpr_c_NOT(gbv a);
static gbv            BDD_c_NOT(gbv a);
static gbv            bexpr_c_AND(gbv a, gbv b);
static gbv            BDD_c_AND(gbv a, gbv b);
static gbv            bexpr_c_OR(gbv a, gbv b);
static gbv            BDD_c_OR(gbv a, gbv b);
static gbv            BDD_c_limited_AND(gbv a, gbv b);
static gbv            BDD_c_limited_OR(gbv a, gbv b);
static bool           bexpr_c_NEQ(gbv a, gbv b);
static bool           BDD_c_NEQ(gbv a, gbv b);
static bool           c_EQ(gbv a, gbv b);
static void           switch_to_bexprs();
static void           switch_to_ints();
static void           BDD_cHL_Print(odests fp, gbv H, gbv L);
static void           bexpr_cHL_Print(odests fp, gbv H, gbv L);
static void           switch_to_BDDs();
static int            update_node(ste_ptr ste, int idx, gbv Hnew, gbv Lnew);
static gbv *          allocate_value_buf(int sz, gbv H, gbv L);
static bool           initialize(ste_ptr ste, bool trace_all);
static sch_ptr        mk_repeat(vstate_ptr vp, char *anon);
static bool           has_ifc_nodes(vstate_ptr vp, ilist_ptr il,
                                    ilist_ptr *silp, ilist_ptr *rilp);
static bool           has_stop_nodes(vstate_ptr vp, ilist_ptr il,
                                     ilist_ptr *silp, ilist_ptr *rilp);
static void           expand_fanin(vstate_ptr vp, hash_record *exp, sch_ptr sch,
                                   string aname, int levels, bool expand,
                                   int draw_level);
static void           build_limit_tbl(vstate_ptr vp, hash_record *limit_tblp,
                                      string sn, sch_ptr sch);
static sch_ptr        limited_draw_fanin(vstate_ptr vsp, ilist_ptr il,
                                         hash_record *limit_tblp,
                                         int draw_level);
static bool           same_ilist(ilist_ptr il1, ilist_ptr il2);
static bool           all_outs(ilist_ptr il, vis_io_ptr vp);
static sch_ptr        draw_fanin(vstate_ptr vsp, ilist_ptr il, int levels,
                                 int anon_cnt, int draw_level);
static string         mk_fresh_anon_name(g_ptr internals, g_ptr fa_inps,
					 g_ptr fa_outs, int *cur_cntp);
static string         mk_vector_name(string base, int size);
static string         create_constant(int sz, int *ccnt, g_ptr ints,
				      g_ptr fa_inps, g_ptr fa_outs, string cnst,
                                      buffer *chbufp);
static bool           is_binary_wexpr(g_ptr node, wl_op *opp, g_ptr *ap,
                                      g_ptr *bp);
static bool           is_relation_wexpr(g_ptr node, wl_op *opp, g_ptr *ap,
                                        g_ptr *bp);
static g_ptr          mk_list1(g_ptr el);
static g_ptr          mk_pair(g_ptr fst, g_ptr snd);
static string         create_merge_component(int sz, int *ccnt, g_ptr *intsp,
					     g_ptr fa_inps, g_ptr fa_outs,
                                             g_ptr acts, int len,
                                             buffer *chbufp);
static g_ptr          clean_pexlif_ios(g_ptr node);
static int            fsm_sha256_fn(int *g_cntp, hash_record *g_tblp,
                                    SHA256_ptr sha, pointer a);
static event_list_ptr add_active_event(hash_record *tblp, event_ptr ep);
static event_list_ptr delete_active_event(hash_record *tblp, event_ptr ep);
static void           allocate_ste_buffers(fsm_ptr fsm);
static void           free_ste_buffers();
static void           simulation_break_handler();
static void           gSTE(g_ptr redex, value_type type);
static void           base_print_ilist(ilist_ptr il);
static void           fl_clean_pexlif_ios(g_ptr redex);
static fsm_ptr        gen_pexlif2fsm(g_ptr p, g_ptr black_box_list,
                                     int max_depth);
static int            nn_cmp(const void *pi, const void *pj);
static bool           is_draw_fub(vis_ptr vp);
static vis_ptr        get_vis_info_at_level(vis_list_ptr vlp, int level);
static string         value_type2string(value_type type);
static void           gen_limited_get_trace(g_ptr redex, value_type type);
static void           gen_get_trace(g_ptr redex, value_type type);
static void           gen_get_trace_val(g_ptr redex, value_type type);
static void           gen_get_ste_result(g_ptr redex, value_type type);
static void	      get_nonX_entries(g_ptr redex);

static void           dbg_print_ils(pointer key, pointer data);
static void           dbg_print_stop_nds(pointer key, pointer data);

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Fsm_Init()
{
    new_mgr(&fsm_rec_mgr, sizeof(fsm_rec));
    new_mgr(&ste_rec_mgr, sizeof(ste_rec));
    new_mgr(&vstate_rec_mgr, sizeof(vstate_rec));
    s_draw_repeat =   wastrsave(stringsp, "draw_repeat_nd");

    s_ES =	    wastrsave(stringsp, "");
    s_use_bdds =    wastrsave(stringsp, "bdd");
    s_use_bexprs =  wastrsave(stringsp, "bexpr");
    s_use_ints =    wastrsave(stringsp, "int");
    s_DummyOut =    wastrsave(stringsp, "DummyOut");
    s_fsm =	    wastrsave(stringsp, "fsm");
    s_ste =	    wastrsave(stringsp, "ste");
    s_SCH_INT =	    Mk_constructor_name("SCH_INT");
    s_SCH_LEAF =    Mk_constructor_name("SCH_LEAF");
    //
    fsm_oidx  = Add_ExtAPI_Object("fsm",
                                  mark_fsm_fn,
                                  sweep_fsm_fn,
                                  save_fsm_fn,
                                  load_fsm_fn,
                                  fsm2str_fn,
                                  fsm_eq_fn,
                                  fsm_hash_fn,
                                  NULL,
                                  NULL,
				  fsm_sha256_fn);
    fsm_handle_tp  = Get_Type("fsm", NULL, TP_INSERT_FULL_TYPE);

    ste_oidx = Add_ExtAPI_Object("ste",
                                  mark_ste_fn,
                                  sweep_ste_fn,
                                  save_ste_fn,
                                  load_ste_fn,
                                  ste2str_fn,
                                  ste_eq_fn,
                                  ste_hash_fn,
                                  ste_gmap_fn,
                                  ste_gmap2_fn,
				  NULL);
    ste_handle_tp  = Get_Type("ste", NULL, TP_INSERT_FULL_TYPE);

    vstate_oidx = Add_ExtAPI_Object("vstate",
                                  mark_vstate_fn,
                                  sweep_vstate_fn,
                                  save_vstate_fn,
                                  load_vstate_fn,
                                  vstate2str_fn,
                                  NULL, //vstate_eq_fn,
                                  NULL, //vstate_hash_fn,
                                  NULL, //vstate_gmap_fn,
                                  NULL, //vstate_gmap2_fn
				  NULL);
    vstate_handle_tp  = Get_Type("vis", NULL, TP_INSERT_FULL_TYPE);

    new_buf(&inps_buf, 1000, sizeof(gbv));
    new_buf(&outs_buf, 1000, sizeof(gbv));
    new_buf(&tmps_buf, 1000, sizeof(gbv));

    create_hash(&state_holding_tbl, 50, str_hash, str_equ);
    string *p = state_holding_names;
    for (unsigned int i = 0; i < sizeof(state_holding_names) / sizeof(string) ; i++) {
	string n = wastrsave(stringsp, *p);
	insert_hash(&state_holding_tbl, n, n);
	p++;
    }
}

string
get_real_name(vec_info_ptr ip, int oidx)
{
    vec_ptr decl = ip->declaration;
    string res = strtemp(ip->hierarchy);
    int idx = oidx;
    while( decl != NULL ) {
	if( decl->type != INDEX ) {
	    strappend(decl->u.name);
	    decl = decl->next;
	} else {
	    int stride = get_stride(decl->next);
	    range_ptr r = decl->u.ranges;
	    int cur_chunk = (abs(r->upper-r->lower)+1)*stride;
	    while( idx >= cur_chunk ) {
		idx -= cur_chunk;
		r = r->next;
		if( r == NULL ) {
		    Rprintf("Illegal index in get_real_name (%d)\n", oidx);
		}
		cur_chunk = (abs(r->upper-r->lower)+1)*stride;
	    }
	    int offset = idx / stride;
	    if( r->upper > r->lower ) {
		Sprintf(buf, "%d", r->upper-offset);
	    } else {
		Sprintf(buf, "%d", r->upper+offset);
	    }
	    strappend(buf);
	    idx = idx - offset*stride;
	    decl = decl->next;
	}
    }
    return res;
}

void
write_ncomp_rec(FILE *fp, pointer p)
{
    ncomp_ptr vp = (ncomp_ptr) p;
    write_unint(fp, vp->size);
    write_unint(fp, vp->rank);
    write_unint(fp, vp->phase_delay);
    write_unint(fp, vp->no_weakening);
    write_unint(fp, vp->flag);
    wl_op op = vp->op;
    if( op == op_X ) {
	write_int(fp, 1);
    }
    else if( op == op_CONST ) {
	write_int(fp, 2);
	write_arbi_T(fp, vp->arg.value);
    }
    else if( op == op_VAR ) {
	write_int(fp, 3);
    }
    else if( op == op_AND ) {
	write_int(fp, 4);
    }
    else if( op == op_OR ) {
	write_int(fp, 5);
    }
    else if( op == op_NOT ) {
	write_int(fp, 6);
    }
    else if( op == op_EQ ) {
	write_int(fp, 7);
	write_int(fp, vp->arg.extension_size);
    }
    else if( op == op_GR ) {
	write_int(fp, 8);
	write_int(fp, vp->arg.extension_size);
    }
    else if( op == op_ADD ) {
	write_int(fp, 9);
    }
    else if( op == op_SUB ) {
	write_int(fp, 10);
    }
    else if( op == op_MUL ) {
	write_int(fp, 11);
    }
    else if( op == op_ITE ) {
	write_int(fp, 12);
    }
    else if( op == op_DIV ) {
	write_int(fp, 13);
    }
    else if( op == op_MOD ) {
	write_int(fp, 14);
    }
    else if( op == op_SHL ) {
	write_int(fp, 15);
    }
    else if( op == op_SHR ) {
	write_int(fp, 16);
    }
    else if( op == op_ASHR ) {
	write_int(fp, 17);
    }
    else if( op == op_SX ) {
	write_int(fp, 18);
	write_int(fp, vp->arg.extension_size);
    }
    else if( op == op_ZX ) {
	write_int(fp, 19);
	write_int(fp, vp->arg.extension_size);
    }
    else if( op == op_SLICE ) {
	write_int(fp, 20);
	write_idx_list_ptr(fp, vp->arg.idx_list);
    }
    else if( op == op_WIRE ) {
	write_int(fp, 21);
    }
    else if( op == op_MEM_READ ) {
	write_int(fp, 22);
	write_int(fp, vp->arg.mem.addr_size);
	write_int(fp, vp->arg.mem.data_size);
	write_int(fp, vp->arg.mem.lines);
    }
    else if( op == op_MEM_WRITE ) {
	write_int(fp, 23);
	write_int(fp, vp->arg.mem.addr_size);
	write_int(fp, vp->arg.mem.data_size);
	write_int(fp, vp->arg.mem.lines);
    }
    else if( op == op_LAT_LEQ ) {
	write_int(fp, 24);
    }
    else if( op == op_UPDATE_SLICE ) {
	write_int(fp, 25);
	write_idx_list_ptr(fp, vp->arg.idx_list);
    }
    else {
	DIE("Should not happen!");
    }
    write_ilist_ptr(fp, vp->inps);
    write_ilist_ptr(fp, vp->outs);
}

void
read_ncomp_rec(FILE *fp, pointer p)
{
    ncomp_rec *vp = (ncomp_rec *) p;
    read_unint(fp, &(vp->size));
    { unint i; read_unint(fp, &i); vp->rank = i; }
    { unint i; read_unint(fp, &i); vp->phase_delay = i; }
    { unint i; read_unint(fp, &i); vp->no_weakening = i; }
    { unint i; read_unint(fp, &i); vp->flag = i; }
    int type;
    read_int(fp, &type);
    switch( type ) {
	case 1: vp->op = op_X; break;
	case 2: {
	    vp->op = op_CONST;
	    read_arbi_T(fp, &(vp->arg.value));
	    break;
	}
	case 3: vp->op = op_VAR; break;
	case 4: vp->op = op_AND; break;
	case 5: vp->op = op_OR; break;
	case 6: vp->op = op_NOT; break;
	case 7: {
		    vp->op = op_EQ;
		    read_int(fp, &(vp->arg.extension_size));
		    break;
		}
	case 8: 
		{
		    vp->op = op_GR;
		    read_int(fp, &(vp->arg.extension_size));
		    break;
		}
	case 9: vp->op = op_ADD; break;
	case 10: vp->op = op_SUB; break;
	case 11: vp->op = op_MUL; break;
	case 12: vp->op = op_ITE; break;
	case 13: vp->op = op_DIV; break;
	case 14: vp->op = op_MOD; break;
	case 15: vp->op = op_SHL; break;
	case 16: vp->op = op_SHR; break;
	case 17: vp->op = op_ASHR; break;
	case 18: {
	    vp->op = op_SX;
	    read_int(fp, &(vp->arg.extension_size));
	    break;
	}
	case 19: {
	    vp->op = op_ZX;
	    read_int(fp, &(vp->arg.extension_size));
	    break;
	}
	case 20: {
	    vp->op = op_SLICE;
	    read_idx_list_ptr(fp, &(vp->arg.idx_list));
	    break;
	}
	case 21: vp->op = op_WIRE; break;
	case 22: {
	    vp->op = op_MEM_READ;
	    read_int(fp, &(vp->arg.mem.addr_size));
	    read_int(fp, &(vp->arg.mem.data_size));
	    read_int(fp, &(vp->arg.mem.lines));
	    break;
	}
	case 23: {
	    vp->op = op_MEM_WRITE;
	    read_int(fp, &(vp->arg.mem.addr_size));
	    read_int(fp, &(vp->arg.mem.data_size));
	    read_int(fp, &(vp->arg.mem.lines));
	    break;
	}
	case 24:
	    vp->op = op_LAT_LEQ; break;
	case 25:
	    vp->op = op_UPDATE_SLICE;
	    read_idx_list_ptr(fp, &(vp->arg.idx_list));
	    break;
	default:
	    DIE("Should never happen!");
    }
    read_ilist_ptr(fp, &(vp->inps));
    read_ilist_ptr(fp, &(vp->outs));
}


fsm_ptr
get_fsm_rec()
{
    fsm_ptr fsm;
    if( fsm_free_list != NULL ) {
	fsm = fsm_free_list;
	fsm_free_list = fsm->next;
	fsm->next = NULL;
    } else {
	fsm = (fsm_ptr) new_rec(&fsm_rec_mgr);
    }
    return fsm;
}

ste_ptr
get_ste_rec()
{
    ste_ptr ste;
    if( ste_free_list != NULL ) {
	ste = ste_free_list;
	ste_free_list = ste->next;
	ste->next = NULL;
    } else {
	ste = (ste_ptr) new_rec(&ste_rec_mgr);
    }
    return ste;
}

vstate_ptr
get_vstate_rec()
{
    vstate_ptr vp;
    if( vstate_free_list != NULL ) {
	vp = vstate_free_list;
	vp->next = NULL;
	vstate_free_list = vstate_free_list->next;
    } else {
	vp = new_rec(&vstate_rec_mgr);
	vp->mark = 0;
	vp->next = NULL;
    }
    return vp;
}

unint
ilist_ptr_hash(pointer key, unint size)
{
    ilist_ptr ip = (ilist_ptr) key;
    unint cur = 13;
    while(ip != NULL) {
	cur = (cur + 19*ip->from + 97*ip->to) % size;
	ip = ip->next;
    }
    return cur;
}

bool
ilist_ptr_equ(pointer k1, pointer k2)
{
    ilist_ptr ip1 = (ilist_ptr) k1;
    ilist_ptr ip2 = (ilist_ptr) k2;
    while( ip1 != NULL && ip2 != NULL ) {
	if( ip1->from != ip2->from ) return FALSE;
	if( ip1->to != ip2->to ) return FALSE;
	ip1 = ip1->next;
	ip2 = ip2->next;
    }
    if( ip1 != NULL || ip2 != NULL ) return FALSE;
    return TRUE;
}

/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
newSTE(g_ptr redex)
{
    if( setjmp(event_jmp_env) == 0 ) { 
	gSTE(redex, use_bdds);
    } else {
	MAKE_REDEX_FAILURE(redex, FailBuf);
    }
}

static void
bSTE(g_ptr redex)
{
    gSTE(redex, use_bexprs);
}

static void
toplevel_pexlif2fsm(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr p = GET_APPLY_RIGHT(redex);
    fsm_ptr fsm = gen_pexlif2fsm(p, NULL, 1);
    if( fsm != NULL ) {
        MAKE_REDEX_EXT_OBJ(redex, fsm_oidx, fsm);
    } else {
        MAKE_REDEX_FAILURE(redex, FailBuf);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
pexlif2fsm(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr p = GET_APPLY_RIGHT(redex);
    fsm_ptr fsm = gen_pexlif2fsm(p, NULL, -1);
    if( fsm != NULL ) {
        MAKE_REDEX_EXT_OBJ(redex, fsm_oidx, fsm);
    } else {
        MAKE_REDEX_FAILURE(redex, FailBuf);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
pexlif2fsm2(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr p, bb_list;
    EXTRACT_2_ARGS(redex, p, bb_list);
    fsm_ptr fsm = gen_pexlif2fsm(p, bb_list, -1);
    if( fsm != NULL ) {
        MAKE_REDEX_EXT_OBJ(redex, fsm_oidx, fsm);
    } else {
        MAKE_REDEX_FAILURE(redex, FailBuf);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
nodes(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    buffer res_buf;
    new_buf(&res_buf, COUNT_BUF(&(fsm->nodes)), sizeof(string));
    nnode_ptr np;
    FOR_BUF(&(fsm->nodes), nnode_rec, np) {
	int start = np->vec->map->from;
	string nname = get_real_name(np->vec, np->idx-start);
	if( *nname != '!' ) {
	    nname = wastrsave(stringsp, nname);
	    int idx = name2idx(nname);
	    if( idx >= 0 ) { 
		push_buf(&res_buf, &nname);
	    } else {
		FP(err_fp, "WARNING: %s not in circuit!\n", nname);
	    }
	}
    }
    qsort(START_BUF(&res_buf), COUNT_BUF(&res_buf), sizeof(string), nn_cmp);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string *spp;
    FOR_BUF(&res_buf, string, spp) { APPEND1(tail, Make_STRING_leaf(*spp)); }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
assertions(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string *spp;
    FOR_BUF(propsp, string, spp) { APPEND1(tail, Make_STRING_leaf(*spp)); }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
edges(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    nnode_ptr np;
    FOR_BUF(&(fsm->nodes), nnode_rec, np) {
        int start = np->vec->map->from;
        string sname = get_real_name(np->vec, np->idx-start);
        if(*sname != '!') {
	    g_ptr from = Make_STRING_leaf(wastrsave(stringsp, sname));
	    for(idx_list_ptr ilp = np->fanouts; ilp != NULL; ilp = ilp->next) {
                ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, ilp->idx);
	        FOREACH_NODE(nd, cp->outs) {
                    string tname = idx2name(nd);
		    g_ptr to = Make_STRING_leaf(wastrsave(stringsp, tname));
                    g_ptr pair = Make_PAIR_ND(from, to);
                    APPEND1(tail, pair);
	        }
	    }
        }
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
vectors(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    buffer res_buf;
    new_buf(&res_buf, 1000, sizeof(string));
    vec_info_ptr vip;
    FOR_REC(vec_info_rec_mgrp, vec_info_ptr, vip) {
	if( !vip->transient ) {
	    string vec = strtemp(vip->hierarchy);
	    vec = strappend(vip->local_name);
	    if( *vec != '!' ) {
		vec = wastrsave(stringsp, vec);
		push_buf(&res_buf, &vec);
	    }
	}
    }
    qsort(START_BUF(&res_buf), COUNT_BUF(&res_buf), sizeof(string), nn_cmp);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string *spp;
    string cur = "";
    FOR_BUF(&res_buf, string, spp) {
	if( strcmp(*spp, cur) != 0 ) {
	    APPEND1(tail, Make_STRING_leaf(*spp));
	    cur = *spp;
	}
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
inputs(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string *sp;
    FOR_BUF(top_inpsp, string, sp) {
	APPEND1(tail, Make_STRING_leaf(*sp));
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
outputs(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm;
    EXTRACT_1_ARG(redex, g_fsm);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string *sp;
    FOR_BUF(top_outsp, string, sp) {
	APPEND1(tail, Make_STRING_leaf(*sp));
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
visualization_nodes(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_level;
    EXTRACT_2_ARGS(redex, g_fsm, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    nnode_ptr np;
    FOR_BUF(&(fsm->nodes), nnode_rec, np) {
	if( get_vis_info_at_level(np->draw_info, GET_INT(g_level)) != NULL ) {
	    int start = np->vec->map->from;
	    string nname = get_real_name(np->vec, np->idx-start);
	    if( *nname != '!' ) {
		g_ptr s = Make_STRING_leaf(wastrsave(stringsp, nname));
		APPEND1(tail, s);
	    }
	}
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_latch_type(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
        pop_fsm();
        MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
        return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    string res = s_ES;
    vis_list_ptr vp = np->draw_info;
    while( vp != NULL ) {
	string s;
	if( (s = find_hash(&state_holding_tbl, vp->vp->pfn)) != NULL ) {
	    res = s;
	    break;
	}
	vp = vp->next;
    }
    MAKE_REDEX_STRING(redex, res);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
is_latch(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
        pop_fsm();
        MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
        return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    string res = s_ES;
    vis_list_ptr vp = np->draw_info;
    while( vp != NULL ) {
	string s;
	if( (s = find_hash(&state_holding_tbl, vp->vp->pfn)) != NULL ) {
	    res = s;
	    break;
	}
	vp = vp->next;
    }
    if( res == s_ES ) {
	MAKE_REDEX_BOOL(redex, B_Zero());
    } else {
	MAKE_REDEX_BOOL(redex, B_One());
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
is_input(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
        pop_fsm();
        MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
        return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    if( np->is_top_input ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
is_phase_delay(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
        pop_fsm();
        MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
        return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    int composite = np->composite;
    if( composite >= 0 ) {
        ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, composite);
        if( cp->phase_delay ) {
            MAKE_REDEX_BOOL(redex, B_One());
        } else {
            MAKE_REDEX_BOOL(redex, B_Zero());
        }
    } else {
        MAKE_REDEX_BOOL(redex, B_Zero());
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_visualization_id(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node, g_level;
    EXTRACT_3_ARGS(redex, g_fsm, g_node, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vis_ptr vp = get_vis_info_at_level(np->draw_info, GET_INT(g_level));
    if( vp == NULL ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Node %s has no drawing information", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_INT(redex, vp->id);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_visualization_pinst_cnt(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node, g_level;
    EXTRACT_3_ARGS(redex, g_fsm, g_node, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vis_ptr vp = get_vis_info_at_level(np->draw_info, GET_INT(g_level));
    if( vp == NULL ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Node %s has no drawing information", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_INT(redex, vp->pinst_cnt);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_visualization_pfn(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node, g_level;
    EXTRACT_3_ARGS(redex, g_fsm, g_node, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vis_ptr vp = get_vis_info_at_level(np->draw_info, GET_INT(g_level));
    if( vp == NULL ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Node %s has no drawing information", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_STRING(redex, vp->pfn);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_visualization_attributes(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node, g_level;
    EXTRACT_3_ARGS(redex, g_fsm, g_node, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vis_ptr vp = get_vis_info_at_level(np->draw_info, GET_INT(g_level));
    if( vp == NULL ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Node %s has no drawing information", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    attr_list_ptr ap = vp->attrs;
    while( ap != NULL ) {
	g_ptr nm = Make_STRING_leaf(wastrsave(stringsp, ap->name));
	g_ptr vl = Make_STRING_leaf(wastrsave(stringsp, ap->value));
	APPEND1(tail, Make_PAIR_ND(nm, vl));
	ap = ap->next;
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_visualization_fanins(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node, g_level;
    EXTRACT_3_ARGS(redex, g_fsm, g_node, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vis_ptr vp = get_vis_info_at_level(np->draw_info, GET_INT(g_level));
    if( vp == NULL ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Node %s has no drawing information", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    for(vis_io_ptr vip = vp->fa_inps; vip != NULL; vip = vip->next) {
	g_ptr pair = Make_CONS_ND(Make_STRING_leaf(vip->f_vec),
				  ilist2nds(vip->acts));
	SET_CONS_HD(tail, pair);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_visualization_outputs(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node, g_level;
    EXTRACT_3_ARGS(redex, g_fsm, g_node, g_level);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vis_ptr vp = get_vis_info_at_level(np->draw_info, GET_INT(g_level));
    if( vp == NULL ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Node %s has no drawing information", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    for(vis_io_ptr vip = vp->fa_outs; vip != NULL; vip = vip->next) {
	g_ptr pair = Make_CONS_ND(Make_STRING_leaf(vip->f_vec),
				  ilist2nds(vip->acts));
	SET_CONS_HD(tail, pair);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
fanin(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, FailBuf);
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    int composite = np->composite;
    if( composite >= 0 ) {
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, composite);
	FOREACH_NODE(nd, cp->inps) {
	    g_ptr s = Make_STRING_leaf(wastrsave(stringsp, idx2name(nd)));
	    APPEND1(tail, s);
	}
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
excitation_function(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    MAKE_REDEX_NIL(redex);
    int composite = np->composite;
    if( composite < 0 ) {
	MAKE_REDEX_STRING(redex, wastrsave(stringsp, "Constant or input"));
    } else {
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, composite);
	MAKE_REDEX_STRING(redex, wastrsave(stringsp, op2str(cp)));
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
fanin_dfs(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr g_fsm, pred, nodes;
    EXTRACT_3_ARGS(redex, g_fsm, pred, nodes);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    buffer todo_buf;
    new_buf(&todo_buf, 100, sizeof(unint));
    hash_record done_tbl;
    create_hash(&done_tbl, 100, int_hash, int_equ);
    int cnt = 0;
    while( !IS_NIL(nodes) ) {
	string node = GET_STRING(GET_CONS_HD(nodes));
	int idx = name2idx(node);
	if( idx < 0 ) {
	    pop_fsm();
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	    End_string_ops(sop);
	    return;
	}
	push_buf(&todo_buf, &idx);
	cnt++;
	nodes = GET_CONS_TL(nodes);
    }
    MAKE_REDEX_NIL(redex);
    PUSH_GLOBAL_GC(redex);
    g_ptr tail = redex;
    // Process nodes in bfs order....
    while( COUNT_BUF(&todo_buf) > 0 ) {
	unint nd_idx;
	pop_buf(&todo_buf, &nd_idx);
	cnt--;
	if( find_hash(&done_tbl, INT2PTR(nd_idx)) == NULL ) {
	    insert_hash(&done_tbl, INT2PTR(nd_idx), INT2PTR(1));
	    string name = wastrsave(stringsp, idx2name(nd_idx));
	    APPEND1(tail, Make_STRING_leaf(name));
	    bool keep_going = TRUE;
	    if( cnt < 0 ) {
		// Only apply predicate on real fanin nodes
		g_ptr stop = Make_APPL_ND(pred, Make_STRING_leaf(name));
		INC_REFCNT(pred);
		Eval(stop);
		if( is_fail(stop) ) {
		    MAKE_REDEX_FAILURE(redex, FailBuf);
		    return;
		}
		if( GET_BOOL(stop) == B_One() ) { keep_going = FALSE; }
	    }
	    if( keep_going ) {
		nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd_idx);
		int composite = np->composite;
		if( composite >= 0 ) {
		    ncomp_ptr cp =
			(ncomp_ptr) M_LOCATE_BUF(compositesp, composite);
		    FOREACH_NODE(nd, cp->inps) {
			push_buf(&todo_buf, &nd);
		    }
		}
	    }
	}
    }
    POP_GLOBAL_GC(1);
    free_buf(&todo_buf);
    dispose_hash(&done_tbl, NULLFCN);
    End_string_ops(sop);
}

static void
fanout(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    for(idx_list_ptr ilp = np->fanouts; ilp != NULL; ilp = ilp->next) {
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, ilp->idx);
	FOREACH_NODE(nd, cp->outs) {
	    g_ptr s = Make_STRING_leaf(wastrsave(stringsp, idx2name(nd)));
	    APPEND1(tail, s);
	}
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
fanout_dfs(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr g_fsm, pred, nodes;
    EXTRACT_3_ARGS(redex, g_fsm, pred, nodes);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    buffer todo_buf;
    new_buf(&todo_buf, 100, sizeof(unint));
    hash_record done_tbl;
    create_hash(&done_tbl, 100, int_hash, int_equ);
    int cnt = 0;
    while( !IS_NIL(nodes) ) {
	string node = GET_STRING(GET_CONS_HD(nodes));
	unint idx = name2idx(node);
	push_buf(&todo_buf, &idx);
	cnt++;
	nodes = GET_CONS_TL(nodes);
    }
    MAKE_REDEX_NIL(redex);
    PUSH_GLOBAL_GC(redex);
    g_ptr tail = redex;
    // Process nodes in bfs order....
    while( COUNT_BUF(&todo_buf) > 0 ) {
	unint nd_idx;
	pop_buf(&todo_buf, &nd_idx);
	cnt--;
	if( find_hash(&done_tbl, INT2PTR(nd_idx)) == NULL ) {
	    insert_hash(&done_tbl, INT2PTR(nd_idx), INT2PTR(1));
	    string name = wastrsave(stringsp, idx2name(nd_idx));
	    APPEND1(tail, Make_STRING_leaf(name));
	    bool keep_going = TRUE;
	    if( cnt < 0 ) {
		// Only apply predicate on real fanout nodes
		g_ptr stop = Make_APPL_ND(pred, Make_STRING_leaf(name));
		INC_REFCNT(pred);
		Eval(stop);
		if( is_fail(stop) ) {
		    MAKE_REDEX_FAILURE(redex, FailBuf);
		    return;
		}
		if( GET_BOOL(stop) == B_One() ) { keep_going = FALSE; }
	    }
	    if( keep_going ) {
		nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd_idx);
		for(idx_list_ptr ip = np->fanouts; ip != NULL; ip = ip->next) {
		    ncomp_ptr cp = (ncomp_ptr)M_LOCATE_BUF(compositesp,ip->idx);
		    FOREACH_NODE(nd, cp->outs) {
			push_buf(&todo_buf, &nd);
		    }
		}
	    }
	}
    }
    POP_GLOBAL_GC(1);
    free_buf(&todo_buf);
    dispose_hash(&done_tbl, NULLFCN);
    End_string_ops(sop);
}

static void
node2vector(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    string vec = strtemp(np->vec->hierarchy);
    vec = strappend(np->vec->local_name);
    vec = wastrsave(stringsp, vec);
    MAKE_REDEX_STRING(redex, vec);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
node2value_list(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	End_string_ops(sop);
	return;
    }
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    string vlist = np->vec->value_list;
    MAKE_REDEX_NIL(redex);
    if( vlist != NULL ) {
	g_ptr tail = redex;
	string s = strtemp(vlist);
	s++;
	while( *s == '{' ) {
	    s++;
	    string space = index(s, ' ');
	    *space = '\0';
	    int code = atoi(s);
	    s = space+1;
	    string end = index(s,'}');
	    *end = '\0';
	    string name = wastrsave(stringsp, s);
	    g_ptr pair = Make_CONS_ND(Make_INT_leaf(code),
				      Make_STRING_leaf(name));
	    APPEND1(tail, pair)
	    s = end+1;
	}
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
emit_fsm(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_file;
    EXTRACT_2_ARGS(redex, g_fsm, g_file);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string file = GET_STRING(g_file);
    push_fsm(fsm);
    if( (odests_fp = fopen(file, "w")) == NULL ) {
	MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Cannot open file %s for writing", file));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
    }

    FP(FILE_fp, "FSM for %s with signature:\n%s\nRanks: %d\n",
		  fsm->top_name, fsm->sha256_sig, fsm->ranks);
    FP(FILE_fp, " %d nodes:\n", COUNT_BUF(&(fsm->nodes)));
    print_nodes(FILE_fp, fsm);
    FP(FILE_fp, " %d composites:\n", COUNT_BUF(&(fsm->composites)));
    print_composites(FILE_fp);
    fclose(odests_fp);
    odests_fp = NULL;
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
basename(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_fsm();
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	return;
    }
    string bname = idx2name(idx);
    MAKE_REDEX_STRING(redex, wastrsave(stringsp, bname));
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
is_node(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, g_node;
    EXTRACT_2_ARGS(redex, g_fsm, g_node);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    string node = GET_STRING(g_node);
    push_fsm(fsm);
    int idx = name2idx(node);
    if( idx >= 0 ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
ste2fsm(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste;
    EXTRACT_1_ARG(redex, g_ste);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    MAKE_REDEX_EXT_OBJ(redex, fsm_oidx, ste->fsm);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_weak_expressions(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste;
    EXTRACT_1_ARG(redex, g_ste);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    if( ste->type != use_bdds ) {
	string msg = Fail_pr("get_weak_expressions require ste from STE run");
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    push_ste(ste);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    formula *fp;
    int idx = 1;
    FOR_BUF(weakening_bufp, formula, fp) {
	char nm[20];
	sprintf(nm, "_%d", idx);
	idx++;
	string name = wastrsave(stringsp, nm);
	g_ptr pair = Make_PAIR_ND(Make_STRING_leaf(name), Make_BOOL_leaf(*fp));
	APPEND1(tail, pair);
    }
    pop_ste();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_abstract_depends(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste, obj;
    EXTRACT_2_ARGS(redex, g_ste, obj);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    if( ste->type != use_bdds ) {
	string msg = Fail_pr("get_weak_expressions require ste from STE run");
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    push_ste(ste);
    hash_record abs_tbl;
    create_hash(&abs_tbl, COUNT_BUF(weakening_bufp), str_hash, str_equ);
    formula *fp;
    int idx = 1;
    FOR_BUF(weakening_bufp, formula, fp) {
	char nm[20];
	sprintf(nm, "_%d", idx);
	idx++;
	string name = wastrsave(stringsp, nm);
	insert_hash(&abs_tbl, name, FORMULA2PTR(*fp));
    }
    Get_abstract_depends(redex, &abs_tbl, obj);
    dispose_hash(&abs_tbl, NULLFCN);
    pop_ste();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_trace(g_ptr redex)
{
    gen_get_trace(redex, use_bdds);
}

static void
get_btrace(g_ptr redex)
{
    gen_get_trace(redex, use_bexprs);
}

static void
limited_get_trace(g_ptr redex)
{
    gen_limited_get_trace(redex, use_bdds);
}

static void
limited_get_btrace(g_ptr redex)
{
    gen_limited_get_trace(redex, use_bexprs);
}

static void
get_trace_val(g_ptr redex)
{
    gen_get_trace_val(redex, use_bdds);
}

static void
get_btrace_val(g_ptr redex)
{
    gen_get_trace_val(redex, use_bexprs);
}

static void
visualization_set_stop_nodes(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate, vecs;
    EXTRACT_2_ARGS(redex, g_vstate, vecs);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    dispose_hash(&(vp->stop_nds), NULLFCN);
    create_hash(&(vp->stop_nds), 100, int_hash, int_equ);
    while( !IS_NIL(vecs) ) {
	string vec = GET_STRING(GET_CONS_HD(vecs));
	g_ptr nl = Vec2nodes(vec);
	for(g_ptr cur = nl; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	    string nd = GET_STRING(GET_CONS_HD(cur));
	    int idx = name2idx(nd);
	    if( idx < 0 ) {
		FP(warning_fp, "Cannot find node %s in fsm. Ignored.", nd);
	    } else {
		if( find_hash(&(vp->stop_nds), INT2PTR(idx)) == NULL ) {
		    insert_hash(&(vp->stop_nds), INT2PTR(idx), INT2PTR(1));
		}
	    }
	}
	vecs = GET_CONS_TL(vecs);
    }
    MAKE_REDEX_VOID(redex);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_ste_type(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste;
    EXTRACT_1_ARG(redex, g_ste);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    string type = value_type2string(ste->type);
    MAKE_REDEX_STRING(redex, type);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_ste_result(g_ptr redex)
{
    gen_get_ste_result(redex, use_bdds);
}

static void
bget_ste_result(g_ptr redex)
{
    gen_get_ste_result(redex, use_bexprs);
}

static void
get_ste_maxtime(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste;
    EXTRACT_1_ARG(redex, g_ste);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    MAKE_REDEX_INT(redex, ste->max_time);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
visualization_vecs2tags(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate, g_ignore_missing, vecs;
    EXTRACT_3_ARGS(redex, g_vstate, g_ignore_missing, vecs);
    bool ignore_missing = (GET_BOOL(g_ignore_missing) == B_One());
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    hash_record	idx_tbl;
    create_hash(&idx_tbl, 100, int_hash, int_equ);
    ilist_ptr full_list = NULL;
    while( !IS_NIL(vecs) ) {
	string vec = GET_STRING(GET_CONS_HD(vecs));
	ilist_ptr il = vec2indices(vec);
	if( il == NULL ) {
	    if( !ignore_missing ) {
		pop_fsm();
		MAKE_REDEX_FAILURE(redex,Fail_pr("Cannot find vector %s", vec));
		End_string_ops(sop);
		return;
	    }
	} else {
	    FOREACH_NODE(nd, il) {
		insert_hash(&idx_tbl, INT2PTR(nd), INT2PTR(1));
	    }
	}
	full_list = ilist_append(full_list, il);
	vecs = GET_CONS_TL(vecs);
    }
    MAKE_REDEX_PAIR(redex, Make_NIL(), Make_NIL());
    g_ptr ftail = GET_CONS_HD(redex);
    g_ptr ptail = GET_CONS_TL(redex);

    ilist_ptr	*ipp;
    int anon = 0;
    FOR_BUF(&(vp->anon_buf), ilist_ptr, ipp) {
	bool exact_found = FALSE;
	bool used = FALSE;
	if( ilist_ptr_equ(*ipp, full_list) ) {
	    exact_found = TRUE;
	    used = TRUE;
	} else {
	    int nds = 0;
	    int matches = 0;
	    FOREACH_NODE(nd, *ipp) {
		nds++;
		if( find_hash(&idx_tbl, INT2PTR(nd)) != NULL ) {
		    matches++;
		}
	    }
	    if( matches > 0 ) {
		used = TRUE;
		if( nds == matches )
		    exact_found = TRUE;
	    }
	}
	if( used ) {
	    Sprintf(buf, "an%06d", anon);
	    string an = wastrsave(stringsp, buf);
	    if( exact_found ) {
		SET_CONS_HD(ftail, Make_STRING_leaf(an));
		SET_CONS_TL(ftail, Make_NIL());
		ftail = GET_CONS_TL(ftail);
	    } else {
		SET_CONS_HD(ptail, Make_STRING_leaf(an));
		SET_CONS_TL(ptail, Make_NIL());
		ptail = GET_CONS_TL(ptail);
	    }
	}
	anon++;
    }
#if 0
    if( !exact_found ) {
	int anon_cnt = COUNT_BUF(&(vp->anon_buf));
	push_buf(&(vp->anon_buf), &full_list);
	Sprintf(buf, "an%06d", anon_cnt);
	string anon = wastrsave(stringsp, buf);
	SET_CONS_HD(ftail, Make_STRING_leaf(anon));
	SET_CONS_TL(ftail, Make_NIL());
	ftail = GET_CONS_TL(ftail);
    }
#endif
    dispose_hash(&idx_tbl, NULLFCN);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
visualization_anon2real(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate, g_aname;
    EXTRACT_2_ARGS(redex, g_vstate, g_aname);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    string aname = GET_STRING(g_aname);
    if( *aname == 'a' && *(aname+1) == 'n' ) {
        unint idx = atoi(aname+2);
	if( idx >= COUNT_BUF(&(vp->anon_buf)) ) {
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Anon node %d out of range",idx));
	    pop_fsm();
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
        ilist_ptr ip = *((ilist_ptr *) M_LOCATE_BUF(&(vp->anon_buf), idx));
        g_ptr nds = ilist2nds(ip);
        g_ptr res = Merge_Vectors(nds, TRUE);
	OVERWRITE(redex, res);
    } else {
	MAKE_REDEX_CONS_ND(redex, Make_STRING_leaf(aname), Make_NIL());
    }
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
visualisation2tcl(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate;
    EXTRACT_1_ARG(redex, g_vstate);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    MAKE_REDEX_CONS_ND(redex, NULL, Make_NIL());
    g_ptr pgm = GET_CONS_TL(redex);
    sch2tcl_cnt = 0;
    string final = sch2tcl(&pgm, vp->sch);
    SET_CONS_HD(redex, Make_STRING_leaf(final));
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

// visualize_fanin fsm stop_list ifc_vecs levels vecs draw_level
static void
visualize_fanin(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_fsm, stop_vecs, ifc_vecs, g_levels, vecs, g_draw_level;
    EXTRACT_6_ARGS(redex,g_fsm,stop_vecs,ifc_vecs,g_levels,vecs,g_draw_level);
    int levels = GET_INT(g_levels);
    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    vstate_ptr vp = mk_vstate(fsm);
    while( !IS_NIL(stop_vecs) ) {
	string vec = GET_STRING(GET_CONS_HD(stop_vecs));
	ilist_ptr il = vec2indices(vec);
	if( il == NULL ) {
	    pop_fsm();
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find vector %s", vec));
	    End_string_ops(sop);
	    return;
	}
	FOREACH_NODE(nd, il) {
	    if( find_hash(&(vp->stop_nds), INT2PTR(nd)) == NULL ) {
		insert_hash(&(vp->stop_nds), INT2PTR(nd), INT2PTR(1));
	    }
	}
	stop_vecs = GET_CONS_TL(stop_vecs);
    }
    while( !IS_NIL(ifc_vecs) ) {
	string vec = GET_STRING(GET_CONS_HD(ifc_vecs));
	ilist_ptr il = vec2indices(vec);
	if( il == NULL ) {
	    pop_fsm();
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find vector %s", vec));
	    End_string_ops(sop);
	    return;
	}
	FOREACH_NODE(nd, il) {
	    if( find_hash(&(vp->ifc_nds), INT2PTR(nd)) == NULL ) {
		insert_hash(&(vp->ifc_nds), INT2PTR(nd), INT2PTR(1));
	    }
	}
	ifc_vecs = GET_CONS_TL(ifc_vecs);
    }

    // Will be the draw_output node
    sch_ptr res = (sch_ptr) new_rec(&(vp->sch_rec_mgr));
    res->vec = NULL;
    res->pfn = NULL;
    res->children = NULL;
    tstr_ptr tmp_strs = new_temp_str_mgr();
    string pfn = gen_strtemp(tmp_strs, "draw_output {");
    while( !IS_NIL(vecs) ) {
	string vec = GET_STRING(GET_CONS_HD(vecs));
	ilist_ptr il = vec2indices(vec);
	if( il == NULL ) {
	    pop_fsm();
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find vector %s", vec));
	    End_string_ops(sop);
	    return;
	}
	pfn = gen_strappend(tmp_strs, " {");
	pfn = gen_strappend(tmp_strs, "}");
	sch_ptr fanin = draw_fanin(vp,il,levels,-1,GET_INT(g_draw_level));
	if( fanin == NULL ) {
	    pop_fsm();
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Failed to draw %s", vec));
	    End_string_ops(sop);
	    return;
	}
	sch_list_ptr sl = (sch_list_ptr) new_rec(&(vp->sch_list_rec_mgr));
	sl->sch = fanin;
	sl->next = NULL;
	if( res->children == NULL ) {
	    res->children = sl;
	} else {
	    sch_list_ptr tsl = res->children;
	    while( tsl->next != NULL ) tsl = tsl->next;
	    tsl->next = sl;
	}
	vecs = GET_CONS_TL(vecs);
    }
    pfn = gen_strappend(tmp_strs, "}");
    res->pfn = wastrsave(stringsp, pfn);
    res->vec = s_DummyOut;
    free_temp_str_mgr(tmp_strs);
    vp->sch = res;
    MAKE_REDEX_EXT_OBJ(redex, vstate_oidx, vp);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

// visualize_expand_fanin vfsm anon levels
static void
visualize_expand_fanin(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate, g_aname, g_levels, g_draw_level;
    EXTRACT_4_ARGS(redex, g_vstate, g_aname, g_levels, g_draw_level);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    string aname = GET_STRING(g_aname);
    int levels = GET_INT(g_levels);
    push_undo_point(vp);
    hash_record expanded;
    create_hash(&expanded, 100, str_hash, str_equ);
    expand_fanin(vp, &expanded, vp->sch, aname, levels, FALSE,
		 GET_INT(g_draw_level));
    dispose_hash(&expanded, NULLFCN);
    MAKE_REDEX_EXT_OBJ(redex, vstate_oidx, vp);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}


// visualize_hide_fanin vfsm anon
static void
visualize_hide_fanin(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate, g_aname, g_draw_level;
    EXTRACT_3_ARGS(redex, g_vstate, g_aname, g_draw_level);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    string aname = GET_STRING(g_aname);
    push_undo_point(vp);
    // Build a limit_tbl with all nodes except the node to hide and the
    // incomplete nodes (since we want to stop on them anyways).
    hash_record limit_tbl;
    create_hash(&limit_tbl, 100, ilist_ptr_hash, ilist_ptr_equ);
    build_limit_tbl(vp, &limit_tbl, aname, vp->sch);
    // Put all outputs in a buffer
    sch_ptr sch = vp->sch;
    buffer outputs;
    new_buf(&outputs, 100, sizeof(ilist_ptr));
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	string an = sl->sch->vec;
	int idx = atoi(an+2);
	ilist_ptr il = *((ilist_ptr *) M_LOCATE_BUF(&(vp->anon_buf), idx));
	push_buf(&outputs, &il);
    }
    // Now remove all drawing information
    dispose_hash(&(vp->done), NULLFCN);
    create_hash(&(vp->done), 100, ilist_ptr_hash, ilist_ptr_equ);
    free_buf(&(vp->anon_buf));
    new_buf(&(vp->anon_buf), 100, sizeof(ilist_ptr));
    // Now do a limited fanin for all the outputs
    int ocnt = 0;
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	ilist_ptr il = *((ilist_ptr *)M_LOCATE_BUF(&outputs, ocnt));
	sl->sch = limited_draw_fanin(vp, il, &limit_tbl, GET_INT(g_draw_level));
	ocnt++;
    }
    dispose_hash(&limit_tbl, NULLFCN);
    MAKE_REDEX_EXT_OBJ(redex, vstate_oidx, vp);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
visualize_undo(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate;
    EXTRACT_1_ARG(redex, g_vstate);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    push_fsm(vp->fsm);
    pop_undo_point(vp);
    MAKE_REDEX_EXT_OBJ(redex, vstate_oidx, vp);
    pop_fsm();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
visualize_get_shown_anons(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_vstate;
    EXTRACT_1_ARG(redex, g_vstate);
    vstate_ptr vp = (vstate_ptr) GET_EXT_OBJ(g_vstate);
    MAKE_REDEX_NIL(redex);
    g_ptr tl = redex;
    for(unint i = 0; i < COUNT_BUF(&(vp->anon_buf)); i++) {
	Sprintf(buf, "an%06d", i);
	APPEND1(tl, Make_STRING_leaf(wastrsave(stringsp, buf)));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
Fsm_Install_Functions()
{
    // Add builtin functions
    typeExp_ptr	pexlif_tp = Get_Type("pexlif", NULL, TP_INSERT_PLACE_HOLDER);

    Add_ExtAPI_Function("dbg_clean_pexlif_ios", "1", FALSE,
			GLmake_arrow(pexlif_tp, pexlif_tp),
			fl_clean_pexlif_ios);

    Add_ExtAPI_Function("toplevel_pexlif2fsm", "1", FALSE,
			GLmake_arrow(pexlif_tp, fsm_handle_tp),
			toplevel_pexlif2fsm);

    Add_ExtAPI_Function("pexlif2fsm", "1", FALSE,
			GLmake_arrow(pexlif_tp, fsm_handle_tp),
			pexlif2fsm);

    Add_ExtAPI_Function("pexlif2fsm2", "11", FALSE,
			GLmake_arrow(
			    pexlif_tp, 
			    GLmake_arrow(
				GLmake_list(GLmake_string()),
				fsm_handle_tp)),
			pexlif2fsm2);

    typeExp_ptr weak_tp = GLmake_list(
			    GLmake_tuple(
				GLmake_bool(),
				GLmake_tuple(
				    GLmake_string(),
				    GLmake_tuple(GLmake_int(), GLmake_int()))));

    typeExp_ptr be_weak_tp =
			GLmake_list(
			    GLmake_tuple(
				GLmake_bexpr(),
				GLmake_tuple(
				    GLmake_string(),
				    GLmake_tuple(GLmake_int(), GLmake_int()))));

    typeExp_ptr ant_tp = GLmake_list(
			    GLmake_tuple(
			      GLmake_bool(),
			      GLmake_tuple(
				GLmake_string(),
				GLmake_tuple(
				  GLmake_bool(),
				  GLmake_tuple(GLmake_int(), GLmake_int())))));

    typeExp_ptr be_ant_tp =
			GLmake_list(
			    GLmake_tuple(
			      GLmake_bexpr(),
			      GLmake_tuple(
				GLmake_string(),
				GLmake_tuple(
				  GLmake_bexpr(),
				  GLmake_tuple(GLmake_int(), GLmake_int())))));

    typeExp_ptr tr_tp = GLmake_list(
			    GLmake_tuple(
			      GLmake_string(),
			      GLmake_tuple(GLmake_int(), GLmake_int())));


    Add_ExtAPI_Function("STE", "1111111", FALSE,
			GLmake_arrow(
			  GLmake_string(),
			  GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
			      weak_tp,
			      GLmake_arrow(
				ant_tp,
				GLmake_arrow(
				  ant_tp,
				  GLmake_arrow(
				    tr_tp,
				    GLmake_arrow(
				      GLmake_string(),
				      ste_handle_tp))))))),
			newSTE);

    Add_ExtAPI_Function("bSTE", "1111111", FALSE,
			GLmake_arrow(
			  GLmake_string(),
			  GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
			      be_weak_tp,
			      GLmake_arrow(
				be_ant_tp,
				GLmake_arrow(
				  be_ant_tp,
				  GLmake_arrow(
				    tr_tp,
				    GLmake_arrow(
				      GLmake_string(),
				      ste_handle_tp))))))),
			bSTE);

    Add_ExtAPI_Function("ste2fsm", "1", FALSE,
			GLmake_arrow(ste_handle_tp, fsm_handle_tp),
			ste2fsm);

    Add_ExtAPI_Function("nodes", "1", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_list(GLmake_string())),
			nodes);

    Add_ExtAPI_Function("assertions", "1", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_list(GLmake_string())),
			assertions);

    Add_ExtAPI_Function("edges", "1", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_list(
				GLmake_tuple(
				    GLmake_string(),
				    GLmake_string()))),
			edges);    

    Add_ExtAPI_Function("vectors", "1", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_list(GLmake_string())),
			vectors);

    Add_ExtAPI_Function("inputs", "1", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_list(GLmake_string())),
			inputs);

    Add_ExtAPI_Function("outputs", "1", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_list(GLmake_string())),
			outputs);

    Add_ExtAPI_Function("visualization_nodes", "11", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(GLmake_int(),
					 GLmake_list(GLmake_string()))),
			visualization_nodes);

    Add_ExtAPI_Function("is_phase_delay", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			is_phase_delay);

    Add_ExtAPI_Function("is_latch", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			is_latch);

    Add_ExtAPI_Function("is_input", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			is_input);

    Add_ExtAPI_Function("get_latch_type", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_string())),
			get_latch_type);

    Add_ExtAPI_Function("get_visualization_id", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(GLmake_int(),
					     GLmake_int()))),
			get_visualization_id);

    Add_ExtAPI_Function("get_visualization_pinst_cnt", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(GLmake_int(),
					     GLmake_int()))),
			get_visualization_pinst_cnt);

    Add_ExtAPI_Function("get_visualization_pfn", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(GLmake_int(), GLmake_string()))),
			get_visualization_pfn);

    Add_ExtAPI_Function("get_visualization_attributes", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(
				    GLmake_int(),
				    GLmake_list(
					GLmake_tuple(GLmake_string(),
						     GLmake_string()))))),
			get_visualization_attributes);

    Add_ExtAPI_Function("get_visualization_fanins", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(
				    GLmake_int(),
				    GLmake_list(
					GLmake_tuple(
					    GLmake_string(),
					    GLmake_list(
						GLmake_string())))))),
			get_visualization_fanins);

    Add_ExtAPI_Function("get_visualization_outputs", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(
				    GLmake_int(),
				    GLmake_list(
					GLmake_tuple(
					    GLmake_string(),
					    GLmake_list(
						GLmake_string())))))),
			get_visualization_outputs);

    Add_ExtAPI_Function("fanin", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				 GLmake_arrow(GLmake_string(),
					      GLmake_list(GLmake_string()))),
			fanin);

    Add_ExtAPI_Function("excitation_function", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				 GLmake_arrow(GLmake_string(),
					      GLmake_string())),
			excitation_function);

    Add_ExtAPI_Function("fanin_dfs", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_arrow(GLmake_string(), GLmake_bool()),
				GLmake_arrow(
				    GLmake_list(GLmake_string()),
				    GLmake_list(GLmake_string())))),
			fanin_dfs);

    Add_ExtAPI_Function("fanout_dfs", "111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
				GLmake_arrow(GLmake_string(), GLmake_bool()),
				GLmake_arrow(
				    GLmake_list(GLmake_string()),
				    GLmake_list(GLmake_string())))),
			fanout_dfs);

    Add_ExtAPI_Function("fanout", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				 GLmake_arrow(GLmake_string(),
					      GLmake_list(GLmake_string()))),
			fanout);

    Add_ExtAPI_Function("node2vector", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_string())),
			node2vector);

    Add_ExtAPI_Function("node2value_list", "11", FALSE,
			GLmake_arrow(
			  fsm_handle_tp,
			  GLmake_arrow(
			    GLmake_string(),
			    GLmake_list(
			      GLmake_tuple(
				GLmake_int(), GLmake_string())))),
			node2value_list);

    Add_ExtAPI_Function("dbg_emit_fsm", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_void())),
			emit_fsm);

    Add_ExtAPI_Function("get_ste_type", "1", FALSE,
			GLmake_arrow(ste_handle_tp, GLmake_string()),
			get_ste_type);

    Add_ExtAPI_Function("get_ste_result", "11", FALSE,
			GLmake_arrow(ste_handle_tp, 
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			get_ste_result);

    Add_ExtAPI_Function("bget_ste_result", "11", FALSE,
			GLmake_arrow(ste_handle_tp, 
				     GLmake_arrow(GLmake_string(),
						  GLmake_bexpr())),
			bget_ste_result);

    Add_ExtAPI_Function("get_ste_maxtime", "1", FALSE,
			GLmake_arrow(ste_handle_tp, GLmake_int()),
			get_ste_maxtime);

    Add_ExtAPI_Function("basename", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_string())),
			basename);

    Add_ExtAPI_Function("is_node", "11", FALSE,
			GLmake_arrow(fsm_handle_tp,
				     GLmake_arrow(GLmake_string(),
						  GLmake_bool())),
			is_node);

    Add_ExtAPI_Function("get_weak_expressions", "1", FALSE,
			 GLmake_arrow(ste_handle_tp,
			    GLmake_list(
				GLmake_tuple(GLmake_string(), GLmake_bool()))),
			 get_weak_expressions);


    Add_ExtAPI_Function("get_abstract_depends", "11", FALSE,
			 GLmake_arrow(
			    ste_handle_tp,
			    GLmake_arrow(GLnew_tVar(),
					 GLmake_list(GLmake_string()))),
			 get_abstract_depends);

    Add_ExtAPI_Function("get_nonX_entries", "111", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_list(GLmake_string()),
			    GLmake_arrow(
				GLmake_int(),
				GLmake_list(GLmake_string())))),
			get_nonX_entries);
			
    Add_ExtAPI_Function("get_trace", "11", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_string(),
			    GLmake_list(
			      GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(
				   GLmake_bool(),
				   GLmake_bool()))))),
			get_trace);

    Add_ExtAPI_Function("bget_trace", "11", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_string(),
			    GLmake_list(
			      GLmake_tuple(
				GLmake_int(),
				GLmake_tuple(
				   GLmake_bexpr(),
				   GLmake_bexpr()))))),
			get_btrace);


    Add_ExtAPI_Function("limited_get_trace", "1111", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(
			      GLmake_int(),
			      GLmake_arrow(
				GLmake_string(),
				GLmake_list(
				  GLmake_tuple(
				    GLmake_int(),
				    GLmake_tuple(
				       GLmake_bool(),
				       GLmake_bool()))))))),
			limited_get_trace);

    Add_ExtAPI_Function("limited_bget_trace", "1111", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_int(),
			    GLmake_arrow(
			      GLmake_int(),
			      GLmake_arrow(
				GLmake_string(),
				GLmake_list(
				  GLmake_tuple(
				    GLmake_int(),
				    GLmake_tuple(
				       GLmake_bexpr(),
				       GLmake_bexpr()))))))),
			limited_get_btrace);

    Add_ExtAPI_Function("get_trace_val", "111", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_string(),
			      GLmake_arrow(
				GLmake_int(),
				GLmake_tuple(
				  GLmake_bool(),
				  GLmake_bool())))),
			get_trace_val);

    Add_ExtAPI_Function("bget_trace_val", "111", FALSE,
			GLmake_arrow(
			  ste_handle_tp,
			  GLmake_arrow(
			    GLmake_string(),
			      GLmake_arrow(
				GLmake_int(),
				GLmake_tuple(
				  GLmake_bexpr(),
				  GLmake_bexpr())))),
			get_btrace_val);

    Add_ExtAPI_Function("visualize_fanin", "111111", FALSE,
			GLmake_arrow(
			    fsm_handle_tp,
			    GLmake_arrow(
			      GLmake_list(GLmake_string()),
			      GLmake_arrow(
			        GLmake_list(GLmake_string()),
			        GLmake_arrow(
				   GLmake_int(),
				    GLmake_arrow(
					GLmake_list(GLmake_string()),
					GLmake_arrow(GLmake_int(),
						     vstate_handle_tp)))))),
			visualize_fanin);

    Add_ExtAPI_Function("visualization_anon2real", "11", FALSE,
			GLmake_arrow(vstate_handle_tp,
				     GLmake_arrow(
					GLmake_string(),
					GLmake_list(GLmake_string()))),
			visualization_anon2real);

    Add_ExtAPI_Function("visualization_vecs2tags", "111", FALSE,
			GLmake_arrow(
			    vstate_handle_tp,
			    GLmake_arrow(
				GLmake_bool(),
				GLmake_arrow(
				    GLmake_list(GLmake_string()),
				    GLmake_tuple(
					     GLmake_list(GLmake_string()),
					     GLmake_list(GLmake_string()))))),
			visualization_vecs2tags);

    Add_ExtAPI_Function("visualisation2tcl", "1", FALSE,
			GLmake_arrow(
			    vstate_handle_tp,
			    GLmake_tuple(
				GLmake_string(),
				GLmake_list(GLmake_string()))),
			visualisation2tcl);

    Add_ExtAPI_Function("visualization_set_stop_nodes", "11", FALSE,
			GLmake_arrow(
			    vstate_handle_tp,
			    GLmake_arrow(
				GLmake_list(GLmake_string()),
				GLmake_void())),
			visualization_set_stop_nodes);

    Add_ExtAPI_Function("visualize_undo", "1", FALSE,
			GLmake_arrow(vstate_handle_tp, vstate_handle_tp),
			visualize_undo);

    Add_ExtAPI_Function("visualize_get_shown_anons", "1", FALSE,
			GLmake_arrow(vstate_handle_tp,
				     GLmake_list(GLmake_string())),
			visualize_get_shown_anons);

    Add_ExtAPI_Function("visualize_expand_fanin", "1111", FALSE,
                        GLmake_arrow(
                            vstate_handle_tp,
                            GLmake_arrow(
                                GLmake_string(),
                                GLmake_arrow(
				    GLmake_int(),
				    GLmake_arrow(
					GLmake_int(),
					vstate_handle_tp)))),
                        visualize_expand_fanin);

    Add_ExtAPI_Function("visualize_hide_fanin", "111", FALSE,
			GLmake_arrow(
			    vstate_handle_tp,
			    GLmake_arrow(GLmake_string(),
					 GLmake_arrow(GLmake_int(),
						      vstate_handle_tp))),
			visualize_hide_fanin);

}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static void
save_fsm_fn(FILE *fp, pointer p)
{
    fsm_ptr fsm = (fsm_ptr) p;
    write_fsm_ptr(fp, fsm);
}

static pointer
load_fsm_fn(FILE *fp)
{
    fsm_ptr fsm;
    read_fsm_ptr(fp, &fsm);
    return( (pointer) fsm);
}

static void
save_ste_fn(FILE *fp, pointer p)
{
    ste_ptr ste = (ste_ptr) p;
    write_ste_ptr(fp, ste);
}

static pointer
load_ste_fn(FILE *fp)
{
    ste_ptr ste;
    read_ste_ptr(fp, &ste);
    return( (pointer) ste);
}

static void
save_vstate_fn(FILE *fp, pointer p)
{
    vstate_ptr vstate = (vstate_ptr) p;
    write_vstate_ptr(fp, vstate);
}

static pointer
load_vstate_fn(FILE *fp)
{
    vstate_ptr vstate;
    read_vstate_ptr(fp, &vstate);
    return( (pointer) vstate);
}

static unint
ni_pair_hash(pointer key, unint size)
{
    node_comp_pair_ptr ncp = (node_comp_pair_ptr) key;
    return((((lunint) (ncp->np)) + 117*((lunint) (ncp->comp_idx))) % size);
}

static bool
ni_pair_equ(pointer k1, pointer k2)
{
    node_comp_pair_ptr ncp1 = (node_comp_pair_ptr) k1;
    node_comp_pair_ptr ncp2 = (node_comp_pair_ptr) k2;
    return( (ncp1->np == ncp2->np) && (ncp1->comp_idx == ncp2->comp_idx) );
}


static void
cp_done(pointer key, pointer data)
{
    insert_hash(new_tbl, key, data);
}

static sch_list_ptr
copy_sch_list(vstate_ptr vp, sch_list_ptr sl)
{
    if( sl == NULL ) return NULL;
    sch_list_ptr nsl = (sch_list_ptr) new_rec(&(vp->sch_list_rec_mgr));
    nsl->sch = copy_sch(vp, sl->sch);
    nsl->next = copy_sch_list(vp, sl->next);
    return nsl;
}

static sch_ptr
copy_sch(vstate_ptr vp, sch_ptr sch)
{
    sch_ptr nsch = (sch_ptr) new_rec(&(vp->sch_rec_mgr));
    nsch->vec = sch->vec;
    nsch->pfn = sch->pfn;
    nsch->children = copy_sch_list(vp, sch->children);
    return nsch;
}

static void
push_undo_point(vstate_ptr vp)
{
    sch_inst_ptr copy = (sch_inst_ptr) new_rec(&(vp->sch_inst_rec_mgr));
    copy->sch = copy_sch(vp, vp->sch);
    unint sz = hash_size(&(vp->done));
    create_hash(&(copy->done), sz, ilist_ptr_hash, ilist_ptr_equ);
    new_tbl = &(copy->done);
    scan_hash(&(vp->done), cp_done);
    new_buf(&(copy->anon_buf), COUNT_BUF(&(vp->anon_buf)), sizeof(ilist_ptr));
    ilist_ptr	*ipp;
    FOR_BUF(&(vp->anon_buf), ilist_ptr, ipp) {
	push_buf(&(copy->anon_buf), ipp);
    }
    copy->next = vp->old_versions;
    vp->old_versions = copy;
}

static void
pop_undo_point(vstate_ptr vp)
{
    if( vp->old_versions == NULL ) return;
    vp->sch = vp->old_versions->sch;
    unint sz = hash_size(&(vp->old_versions->done));
    dispose_hash(&(vp->done), NULLFCN);
    create_hash(&(vp->done), sz, ilist_ptr_hash, ilist_ptr_equ);
    new_tbl = &(vp->done);
    scan_hash(&(vp->old_versions->done), cp_done);
    free_buf(&(vp->anon_buf));
    new_buf(&(vp->anon_buf), 100, sizeof(ilist_ptr));
    ilist_ptr	*ipp;
    FOR_BUF(&(vp->old_versions->anon_buf), ilist_ptr, ipp) {
	push_buf(&(vp->anon_buf), ipp);
    }
    vp->old_versions = vp->old_versions->next;
}


static vstate_ptr
mk_vstate(fsm_ptr fsm)
{
    vstate_ptr vp = get_vstate_rec();
    vp->fsm = fsm;
    create_hash(&(vp->done), 100, ilist_ptr_hash, ilist_ptr_equ);
    create_hash(&(vp->stop_nds), 100, int_hash, int_equ);
    create_hash(&(vp->ifc_nds), 100, int_hash, int_equ);
    new_mgr(&(vp->sch_rec_mgr), sizeof(sch_rec)); 
    new_mgr(&(vp->sch_list_rec_mgr), sizeof(sch_list_rec)); 
    new_buf(&(vp->anon_buf), 100, sizeof(ilist_ptr));
    new_mgr(&(vp->sch_inst_rec_mgr), sizeof(sch_inst_rec)); 
    vp->old_versions = NULL;
    vp->sch = NULL;
    return vp;
}

static g_ptr
ilist2nds(ilist_ptr il)
{
    g_ptr res = Make_NIL();
    g_ptr tail = res;
    FOREACH_NODE(nd, il) {
	g_ptr s = Make_STRING_leaf(wastrsave(stringsp, idx2name(nd)));
	SET_CONS_HD(tail, s);
	SET_CONS_TL(tail, Make_NIL());
	tail = GET_CONS_TL(tail);
    }
    return(res);
}


// Wrapper function to store strings permanently
static vec_ptr
split_vector_name(string_op_ptr sop, string name)
{
    vec_ptr vp = Split_vector_name(sop,name);
    // Make vector list permanent
    vec_ptr res = NULL;
    vec_ptr cur = NULL;
    for(vec_ptr p = vp; p != NULL; p = p->next) {
	vec_ptr tmp = (vec_ptr) new_rec(vec_rec_mgrp);
	tmp->type = p->type;
	tmp->next = NULL;
	if( cur == NULL ) {
	    res = tmp;
	    cur = tmp;
	} else {
	    cur->next = tmp;
	    cur = tmp;
	}
	if( p->type == TXT ) {
	    tmp->u.name = wastrsave(stringsp, p->u.name);
	} else {
	    range_ptr cur_range = NULL;
	    for(range_ptr rp = p->u.ranges; rp != NULL; rp = rp->next) {
		range_ptr tr = (range_ptr) new_rec(range_rec_mgrp);
		tr->next = NULL;
		tr->upper = rp->upper;
		tr->lower = rp->lower;
		if( cur_range == NULL ) {
		    tmp->u.ranges = tr;
		    cur_range = tr;
		} else {
		    cur_range->next = tr;
		    cur_range = tr;
		}
	    }
	}
    }
    return res;
}

// Wrapper function to store strings permanently
static string
get_vector_signature(vec_ptr vp)
{
    string sig = Get_vector_signature(sop, vp);
    return( wastrsave(stringsp, sig) );
}

static fsm_ptr
create_fsm()
{
    hash_record	*_all_name_tblp;
    rec_mgr     *_vec_info_rec_mgrp;
    rec_mgr     *_ilist_rec_mgrp;
    rec_mgr     *_vec_rec_mgrp;
    rec_mgr     *_range_rec_mgrp;
    rec_mgr     *_idx_list_rec_mgrp;
    buffer	*_nodesp;
    buffer	*_compositesp;
    buffer	*_top_inpsp;
    buffer	*_top_outsp;
    buffer	*_propsp;
    rec_mgr	*_vis_io_rec_mgrp;
    rec_mgr	*_vis_rec_mgrp;
    rec_mgr	*_vis_list_rec_mgrp;
    rec_mgr	*_attr_list_rec_mgrp;
    //
    fsm_ptr fsm = get_fsm_rec();
    fsm->next = NULL;
    fsm->mark = 1;
    //
    _all_name_tblp = &(fsm->all_name_tbl);
    create_hash(_all_name_tblp, 100, str_hash, str_equ);
    //
    _vec_info_rec_mgrp = &(fsm->vec_info_rec_mgr);
    new_mgr(_vec_info_rec_mgrp, sizeof(vec_info_rec));
    //
    _ilist_rec_mgrp = &(fsm->ilist_rec_mgr);
    new_mgr(_ilist_rec_mgrp, sizeof(ilist_rec));
    //
    _vec_rec_mgrp = &(fsm->vec_rec_mgr);
    new_mgr(_vec_rec_mgrp, sizeof(vec_rec));
    //
    _range_rec_mgrp = &(fsm->range_rec_mgr);
    new_mgr(_range_rec_mgrp, sizeof(range_rec));
    //
    _idx_list_rec_mgrp = &(fsm->idx_list_rec_mgr);
    new_mgr(_idx_list_rec_mgrp, sizeof(idx_list_rec));
    //
    _nodesp = &(fsm->nodes);
    new_buf(_nodesp, 100, sizeof(nnode_rec));
    //
    _compositesp = &(fsm->composites);
    new_buf(_compositesp, 100, sizeof(ncomp_rec));
    //
    _top_inpsp = &(fsm->top_inps);
    new_buf(_top_inpsp, 100, sizeof(string));
    //
    _top_outsp = &(fsm->top_outs);
    new_buf(_top_outsp, 100, sizeof(string));
    //
    _propsp = &(fsm->props);
    new_buf(_propsp, 100, sizeof(string));
    //
    _vis_io_rec_mgrp = &(fsm->vis_io_rec_mgr);
    new_mgr(_vis_io_rec_mgrp, sizeof(vis_io_rec));
    //
    _vis_rec_mgrp = &(fsm->vis_rec_mgr);
    new_mgr(_vis_rec_mgrp, sizeof(vis_rec));
    //
    _vis_list_rec_mgrp = &(fsm->vis_list_rec_mgr);
    new_mgr(_vis_list_rec_mgrp, sizeof(vis_list_rec));
    //
    _attr_list_rec_mgrp = &(fsm->attr_list_rec_mgr);
    new_mgr(_attr_list_rec_mgrp, sizeof(attr_list_rec));
    //
    return fsm;
}

static gbv
GET_GBV(g_ptr nd)
{
    gbv res;
    switch( current_type ) {
	case use_bdds:
	    res.f = GET_BOOL(nd);
	    break;
	case use_bexprs:
	    res.bp = GET_BEXPR(nd);
	    break;
	case use_ints:
	    res.ai = GET_AINT(nd);
	    break;
	default:
	    DIE("Should not happen");
    }
    return res;
}

static void
SET_GBV(g_ptr nd, gbv value)
{
    formula f;
    bexpr bp;
    arbi_T ai;
    switch( current_type ) {
	case use_bdds:
	    f = value.f;
	    SET_BOOL(nd, f);
	    break;
	case use_bexprs:
	    bp = value.bp;
	    SET_BEXPR(nd, bp);
	    break;
	case use_ints:
	    ai = value.ai;
	    SET_AINT(nd, ai);
	    break;
	default:
	    DIE("Should not happen");
    }
}

static void
MAKE_REDEX_GBV(g_ptr redex, gbv value)
{
    formula f;
    bexpr bp;
    arbi_T ai;
    switch( current_type ) {
	case use_bdds:
	    f = value.f;
	    MAKE_REDEX_BOOL(redex, f);
	    break;
	case use_bexprs:
	    bp = value.bp;
	    MAKE_REDEX_BEXPR(redex, bp);
	    break;
	case use_ints:
	    ai = value.ai;
	    MAKE_REDEX_AINT(redex, ai);
	    break;
	default:
	    DIE("Should not happen");
    }
}

static g_ptr
Make_GBV_leaf(gbv value)
{
    formula f;
    bexpr bp;
    arbi_T ai;
    switch( current_type ) {
	case use_bdds:
	    f = value.f;
	    return( Make_BOOL_leaf(f) );
	    break;
	case use_bexprs:
	    bp = value.bp;
	    return( Make_BEXPR_leaf(bp) );
	    break;
	case use_ints:
	    ai = value.ai;
	    return( Make_AINT_leaf(ai) );
	    break;
	default:
	    DIE("Should not happen");
    }
}

static void
extract_five_tuple(g_ptr nd, gbv *whenp, string *namep, gbv *val,
		   int *fromp, int *top)
{
    *whenp = GET_GBV(GET_FST(nd));
    nd = GET_SND(nd);
    *namep = GET_STRING(GET_FST(nd));
    nd = GET_SND(nd);
    *val = GET_GBV(GET_FST(nd));
    nd = GET_SND(nd);
    *fromp = GET_INT(GET_FST(nd));
    *top = GET_INT(GET_SND(nd));
} 

static void
extract_four_tuple(g_ptr nd, gbv *whenp, string *namep, int *fromp, int *top)
{
    *whenp = GET_GBV(GET_FST(nd));
    nd = GET_SND(nd);
    *namep = GET_STRING(GET_FST(nd));
    nd = GET_SND(nd);
    *fromp = GET_INT(GET_FST(nd));
    *top = GET_INT(GET_SND(nd));
} 

static void
extract_tripple(g_ptr nd, string *namep, int *fromp, int *top)
{
    *namep = GET_STRING(GET_FST(nd));
    nd = GET_SND(nd);
    *fromp = GET_INT(GET_FST(nd));
    *top = GET_INT(GET_SND(nd));
} 

static int
event_cmp(const void *p1, const void *p2)
{
    event_ptr e1 = *((event_ptr *) p1);
    event_ptr e2 = *((event_ptr *) p2);
    if( e2->time == e1->time ) {
	if( e1->nd_idx == e2->nd_idx ) {
	    return( e1->type - e2->type );
	} else {
	    return( e2->nd_idx - e1->nd_idx );
	}
    } else {
	return( e2->time - e1->time );
    }
}

static bool
create_event_buffer(ste_ptr ste, buffer *ebufp,
		    g_ptr wl, g_ptr ant, g_ptr cons, g_ptr trl, bool abort_ASAP)
{
    event_id_cnt = 0;
    for(g_ptr cur = wl; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	g_ptr t = GET_CONS_HD(cur);
	gbv when;
	string node;
	int from, to;
	extract_four_tuple(t, &when, &node, &from, &to);
	int nd_idx = name2idx(node);
	if(nd_idx < 0) {
	    Fail_pr("Cannot find node %s in weakening list\n", node);
	    longjmp(event_jmp_env, 1);
	}
	event_ptr ep = new_rec(event_rec_mgrp);
	int event_id = event_id_cnt++;
	ep->event_id = event_id;
	ep->type = start_weak;
	ep->nd_idx = nd_idx;
	ep->time = from;
	ep->H = when;
	ep->L = when;
	push_buf(ebufp, &ep);
	ep = new_rec(event_rec_mgrp);
	ep->event_id = event_id;
	ep->type = end_weak;
	ep->nd_idx = nd_idx;
	ep->time = to;
	ep->H = when;
	ep->L = when;
	push_buf(ebufp, &ep);
    }
    for(g_ptr cur = ant; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	g_ptr t = GET_CONS_HD(cur);
	gbv when, val;
	string node;
	int from, to;
	extract_five_tuple(t, &when, &node, &val, &from, &to);
	int nd_idx = name2idx(node);
	if(nd_idx < 0) {
	    Fail_pr("Cannot find node %s in antecedent\n", node);
	    longjmp(event_jmp_env, 1);
	}
	event_ptr ep = new_rec(event_rec_mgrp);
	int event_id = event_id_cnt++;
	ep->event_id = event_id;
	ep->type = start_ant;
	ep->nd_idx = nd_idx;
	ep->time = from;
	ep->H = c_OR(c_NOT(when), val);
	ep->L = c_OR(c_NOT(when), c_NOT(val));
	gbv okA = c_OR(ep->H, ep->L);
	if( RCnotify_OK_A_failures &&
	    (nbr_errors_reported < RCmax_nbr_errors) &&
	    c_NEQ(okA,c_ONE) )
	{
	    FP(warning_fp, "Warning: Antecedent failure at time %d", from);
	    FP(warning_fp, " on node %s\n", idx2name(nd_idx));
	    FP(warning_fp, "  The OK_A value is:");
	    c_Print(warning_fp, okA, -1);
	    FP(warning_fp, "\n");
	    if( abort_ASAP ) {
		ste->assertion_OK = c_AND(ste->assertion_OK, okA);
		return FALSE;
	    }
	}
	ste->assertion_OK = c_AND(ste->assertion_OK, okA);
	push_buf(ebufp, &ep);
	ep = new_rec(event_rec_mgrp);
	ep->event_id = event_id;
	ep->type = end_ant;
	ep->nd_idx = nd_idx;
	ep->time = to;
	ep->H = c_ONE;
	ep->L = c_ONE;
	push_buf(ebufp, &ep);
    }
    for(g_ptr cur = cons; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	g_ptr t = GET_CONS_HD(cur);
	gbv when, val;
	string node;
	int from, to;
	extract_five_tuple(t, &when, &node, &val, &from, &to);
	int nd_idx = name2idx(node);
	if(nd_idx < 0) {
	    Fail_pr("Cannot find node %s in consequent\n", node);
	    longjmp(event_jmp_env, 1);
	}
	event_ptr ep = new_rec(event_rec_mgrp);
	int event_id = event_id_cnt++;
	ep->event_id = event_id;
	ep->type = start_cons;
	ep->nd_idx = nd_idx;
	ep->time = from;
	ep->H = c_OR(c_NOT(when), val);
	ep->L = c_OR(c_NOT(when), c_NOT(val));
	gbv okC = c_OR(ep->H, ep->L);
	if( RCnotify_OK_C_failures &&
	    (nbr_errors_reported < RCmax_nbr_errors) &&
	    c_NEQ(okC,c_ONE) )
	{
	    FP(warning_fp, "Warning: Consequent failure at time %d", from);
	    FP(warning_fp, " on node %s\n", idx2name(nd_idx));
	    FP(warning_fp, "  The OK_C value is:");
	    c_Print(warning_fp, okC, -1);
	    FP(warning_fp, "\n");
	    if( abort_ASAP ) {
		ste->check_OK = c_AND(ste->check_OK, okC);
		return FALSE;
	    }
	}
	ste->check_OK = c_AND(ste->check_OK, okC);
	push_buf(ebufp, &ep);
	ep = new_rec(event_rec_mgrp);
	ep->event_id = event_id;
	ep->type = end_cons;
	ep->nd_idx = nd_idx;
	ep->time = to;
	ep->H = c_ONE;
	ep->L = c_ONE;
	push_buf(ebufp, &ep);
    }
    for(g_ptr cur = trl; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	g_ptr t = GET_CONS_HD(cur);
	string node;
	int from, to;
	extract_tripple(t, &node, &from, &to);
	int nd_idx = name2idx(node);
	if(nd_idx < 0) {
	    Fail_pr("Cannot find node %s in trace list\n", node);
	    longjmp(event_jmp_env, 1);
	}
	event_ptr ep = new_rec(event_rec_mgrp);
	int event_id = event_id_cnt++;
	ep->event_id = event_id;
	ep->type = start_trace;
	ep->nd_idx = nd_idx;
	ep->time = from;
	ep->H = c_ZERO;	// Don't care
	ep->L = c_ZERO;	// Don't care
	push_buf(ebufp, &ep);
	ep = new_rec(event_rec_mgrp);
	ep->event_id = event_id;
	ep->type = end_trace;
	ep->nd_idx = nd_idx;
	ep->time = to;
	ep->H = c_ZERO;
	ep->L = c_ZERO;
	push_buf(ebufp, &ep);
    }
    // Sort in decreasing time, node index, and start/end so that
    // start_events on a node comes before end events on the same node & time
    qsort(START_BUF(ebufp), COUNT_BUF(ebufp), sizeof(event_ptr), event_cmp);
    return TRUE;
}

static void
process_event(buffer *event_bufp, int time)
{
    if( empty_buf(event_bufp) ) { return; }
    bool done = FALSE;
    while ( !done ) {
	int sz = COUNT_BUF(event_bufp);
	if( sz == 0 ) return;
	event_ptr *epp = (event_ptr*) M_LOCATE_BUF(event_bufp, sz-1);
	event_ptr ep = *epp;
	if( ep->time > time ) {
	    done = TRUE;
	} else {
	    int idx = ep->nd_idx;
	    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
	    np->has_ant_or_weak_change = FALSE;
	    if( ep->type == start_trace ) {
		np->has_trace = TRUE;
		gbv H = *(cur_buf + 2*idx);
		gbv L = *(cur_buf + 2*idx + 1);
		record_trace(idx, time, H, L);
	    } else if( ep->type == end_trace ) {
		np->has_trace = FALSE;
		record_trace(idx, time, c_ONE, c_ONE);
	    } else {
		gbv *buf;
		event_list_ptr	cur_active = NULL;
		switch( ep->type ) {
		    case start_weak:
			np->has_weak = TRUE;
			np->has_ant_or_weak_change = TRUE;
			if( c_EQ(ep->H, c_ZERO) && c_EQ(ep->L, c_ZERO) ) {
			    if( np->composite >= 0 ) {
				ncomp_ptr c;
				c = (ncomp_ptr) M_LOCATE_BUF(compositesp,
							     np->composite);
				c->no_weakening = TRUE;
			    }
			}
			cur_active = add_active_event(active_weak_tblp, ep);
			buf = weak_buf;
			break;
		    case end_weak:
			np->has_weak = FALSE;
			np->has_ant_or_weak_change = TRUE;
			if( c_EQ(ep->H, c_ZERO) && c_EQ(ep->L, c_ZERO) ) {
			    if( np->composite >= 0 ) {
				ncomp_ptr c;
				c = (ncomp_ptr) M_LOCATE_BUF(compositesp,
							     np->composite);
				c->no_weakening = FALSE;
			    }
			}
			cur_active = delete_active_event(active_weak_tblp, ep);
			buf = weak_buf;
			break;
		    case start_ant:
			np->has_ant = TRUE;
			np->has_ant_or_weak_change = TRUE;
			cur_active = add_active_event(active_ant_tblp, ep);
			buf = ant_buf;
			break;
		    case end_ant:
			np->has_ant = FALSE;
			np->has_ant_or_weak_change = TRUE;
			cur_active = delete_active_event(active_ant_tblp, ep);
			buf = ant_buf;
			break;
		    case start_cons:
			np->has_cons = TRUE;
			cur_active = add_active_event(active_cons_tblp, ep);
			buf = cons_buf;
			break;
		    case end_cons:
			np->has_cons = FALSE;
			cur_active = delete_active_event(active_cons_tblp, ep);
			buf = cons_buf;
			break;
		    default:
			DIE("Should not happen");
			break;
		}
		gbv H = c_ONE;
		gbv L = c_ONE;
		while( cur_active ) {
		    H = c_AND(H, cur_active->ep->H);
		    L = c_AND(L, cur_active->ep->L);
		    cur_active = cur_active->next;
		}
		*(buf + 2*idx)   = H;
		*(buf + 2*idx+1) = L;
	    }
	    pop_buf(event_bufp, NULL);
	}
    }
}

static void
record_trace(int idx, int time, gbv H, gbv L)
{
    trace_ptr tp = (trace_ptr) find_hash(trace_tblp, INT2PTR(idx));
    if( tp == NULL ) {
	tp = (trace_ptr) new_rec(trace_rec_mgrp);
	tp->nd_idx = idx;
	tp->events = NULL;
	insert_hash(trace_tblp, INT2PTR(idx), (pointer) tp);
    }
    // Only record if it changed
    if( tp->events && c_EQ(tp->events->H, H) && c_EQ(tp->events->L, L) ) {
	return;
    }
    if( RCverbose_ste_run ) {
	FP(sim_fp, "Node %s {%d} at time %d changes to: ", idx2name(idx),
		   idx, time);
	cHL_Print(sim_fp, H, L);
	FP(sim_fp, "\n");
    }
    trace_event_ptr te = tp->events;
    if( te && te->time == time ) {
	// Don't record glitches but only the final value at this time
	te->H = H;
	te->L = L;
    } else {
	te = (trace_event_ptr) new_rec(trace_event_rec_mgrp);
	te->time = time;
	te->H = H;
	te->L = L;
	te->next = tp->events;
	tp->events = te;
    }
}

static ste_ptr
create_ste(fsm_ptr fsm, value_type type)
{
    ste_ptr ste = get_ste_rec();
    ste->mark = 1;
    ste->max_time = 0;
    ste->next = NULL;
    ste->fsm = fsm;
    ste->type = type;
    hash_record *_active_weak_tblp = &(ste->active_weak_tbl);
    create_hash(_active_weak_tblp, 100, int_hash, int_equ);
    hash_record *_active_ant_tblp = &(ste->active_ant_tbl);
    create_hash(_active_ant_tblp, 100, int_hash, int_equ);
    hash_record *_active_cons_tblp = &(ste->active_cons_tbl);
    create_hash(_active_cons_tblp, 100, int_hash, int_equ);
    hash_record *_trace_tblp = &(ste->trace_tbl);
    create_hash(_trace_tblp, 100, int_hash, int_equ);
    rec_mgr     *_trace_event_rec_mgrp = &(ste->trace_event_rec_mgr);
    new_mgr(_trace_event_rec_mgrp, sizeof(trace_event_rec));
    rec_mgr     *_trace_rec_mgrp = &(ste->trace_rec_mgr);
    new_mgr(_trace_rec_mgrp, sizeof(trace_rec));
    rec_mgr     *_event_rec_mgrp = &(ste->event_rec_mgr);
    new_mgr(_event_rec_mgrp, sizeof(event_rec));
    rec_mgr     *_event_list_rec_mgr = &(ste->event_list_rec_mgr);
    new_mgr(_event_list_rec_mgr, sizeof(event_list_rec));
    buffer     *_weakening_bufp = &(ste->weakening_buf);
    new_buf(_weakening_bufp, 100, sizeof(formula));
    buffer     *_event_bufp = &(ste->event_buf);
    new_buf(_event_bufp, 1000, sizeof(event_ptr));
    push_ste(ste);
    ste->validTrajectory = c_ONE;
    ste->checkTrajectory = c_ONE;
    ste->assertion_OK = c_ONE;
    ste->check_OK = c_ONE;
    return ste;
}

static void
push_ste(ste_ptr ste)
{
    old_type = current_type;
    old_active_weak_tblp = active_weak_tblp;
    old_active_ant_tblp = active_ant_tblp;
    old_active_cons_tblp = active_cons_tblp;
    old_trace_tblp = trace_tblp;
    old_trace_event_rec_mgrp = trace_event_rec_mgrp;
    old_event_list_rec_mgrp = event_list_rec_mgrp;
    old_trace_rec_mgrp = trace_rec_mgrp;
    old_event_rec_mgrp = event_rec_mgrp;
    old_weakening_bufp = weakening_bufp;
    old_event_bufp = event_bufp;
    current_type = ste->type;
    switch( current_type ) {
	case use_bdds:
	    switch_to_BDDs();
	    break;
	case use_bexprs:
	    switch_to_bexprs();
	    break;
	case use_ints:
	    switch_to_ints();
	    break;
	default:
	    break;
    }
    active_weak_tblp = &(ste->active_weak_tbl);
    active_ant_tblp  = &(ste->active_ant_tbl);
    active_cons_tblp = &(ste->active_cons_tbl);
    trace_tblp = &(ste->trace_tbl);
    trace_event_rec_mgrp = &(ste->trace_event_rec_mgr);
    event_list_rec_mgrp = &(ste->event_list_rec_mgr);
    trace_rec_mgrp = &(ste->trace_rec_mgr);
    event_rec_mgrp = &(ste->event_rec_mgr);
    weakening_bufp = &(ste->weakening_buf);
    event_bufp = &(ste->event_buf);
}

static void
pop_ste()
{
    current_type = old_type;
    switch( current_type ) {
	case use_bdds:
	    switch_to_BDDs();
	    break;
	case use_bexprs:
	    switch_to_bexprs();
	    break;
	case use_ints:
	    switch_to_ints();
	    break;
	default:
	    break;
    }
    active_weak_tblp = old_active_weak_tblp;
    active_ant_tblp  = old_active_ant_tblp;
    active_cons_tblp = old_active_cons_tblp;
    trace_tblp = old_trace_tblp;
    trace_event_rec_mgrp = old_trace_event_rec_mgrp;
    event_list_rec_mgrp = old_event_list_rec_mgrp;
    trace_rec_mgrp = old_trace_rec_mgrp;
    event_rec_mgrp = old_event_rec_mgrp;
    weakening_bufp = old_weakening_bufp;
    event_bufp = old_event_bufp;
}

static void
push_fsm_env(fsm_ptr fsm)
{
    old_all_name_tblp = all_name_tblp;
    old_vec_info_rec_mgrp = vec_info_rec_mgrp;
    old_ilist_rec_mgrp = ilist_rec_mgrp;
    old_vec_rec_mgrp = vec_rec_mgrp;
    old_range_rec_mgrp = range_rec_mgrp;
    old_idx_list_rec_mgrp = idx_list_rec_mgrp;
    old_nodesp = nodesp;
    old_compositesp = compositesp;
    old_top_inpsp = top_inpsp;
    old_top_outsp = top_outsp;
    old_propsp = propsp;
    old_vis_io_rec_mgrp = vis_io_rec_mgrp;
    old_vis_rec_mgrp = vis_rec_mgrp;
    old_vis_list_rec_mgrp = vis_list_rec_mgrp;
    old_attr_list_rec_mgrp = attr_list_rec_mgrp;
    all_name_tblp = &(fsm->all_name_tbl);
    vec_info_rec_mgrp = &(fsm->vec_info_rec_mgr);
    ilist_rec_mgrp = &(fsm->ilist_rec_mgr);
    vec_rec_mgrp = &(fsm->vec_rec_mgr);
    range_rec_mgrp = &(fsm->range_rec_mgr);
    idx_list_rec_mgrp = &(fsm->idx_list_rec_mgr);
    nodesp = &(fsm->nodes);
    compositesp = &(fsm->composites);
    top_inpsp = &(fsm->top_inps);
    top_outsp = &(fsm->top_outs);
    propsp = &(fsm->props);
    vis_io_rec_mgrp = &(fsm->vis_io_rec_mgr);
    vis_rec_mgrp = &(fsm->vis_rec_mgr);
    vis_list_rec_mgrp = &(fsm->vis_list_rec_mgr);
    attr_list_rec_mgrp = &(fsm->attr_list_rec_mgr);
}

static void
pop_fsm_env()
{
    all_name_tblp = old_all_name_tblp;
    vec_info_rec_mgrp = old_vec_info_rec_mgrp;
    ilist_rec_mgrp = old_ilist_rec_mgrp;
    vec_rec_mgrp = old_vec_rec_mgrp;
    range_rec_mgrp = old_range_rec_mgrp;
    idx_list_rec_mgrp = old_idx_list_rec_mgrp;
    nodesp = old_nodesp;
    compositesp = old_compositesp;
    top_inpsp = old_top_inpsp;
    top_outsp = old_top_outsp;
    propsp = old_propsp;
}

static void
push_fsm(fsm_ptr fsm)
{
    push_fsm_env(fsm);
}

static void
pop_fsm()
{
    pop_fsm_env();
}

static void
mark_fsm_fn(pointer p)
{
    fsm_ptr fsm = (fsm_ptr) p;
    if( fsm->mark == 2 ) { return; }
    fsm->mark = 2;
    ncomp_ptr	cp;
    FOR_BUF(&(fsm->composites), ncomp_rec, cp) {
	wl_op op = cp->op;
	if( op == op_CONST ) {
	    Arbi_mark(cp->arg.value);
	}
    }
}

static void
gbv_mark(gbv value)
{
    formula f;
    bexpr bp;
    switch( current_type ) {
        case use_bdds:
            f = value.f;
            B_Mark(f);
            break;
        case use_bexprs:
            bp = value.bp;
            BE_Mark(bp);
            break; 
        case use_ints:
            break;      
        default:
            DIE("Should not happen");
    }
}

static void
mark_trace_entries(pointer key, pointer data)
{
    (void) key;
    trace_ptr tp = (trace_ptr) data;
    for(trace_event_ptr te = tp->events; te != NULL; te = te->next) {
	gbv_mark(te->H);
	gbv_mark(te->L);
    }
}

static void
mark_ste_fn(pointer p)
{
    ste_ptr ste = (ste_ptr) p;
    if( ste->mark == 2 ) { return; }
    push_ste(ste);
    push_fsm_env(ste->fsm);
    ste->mark = 2;
    mark_fsm_fn((pointer) (ste->fsm));
    gbv_mark(ste->validTrajectory);
    gbv_mark(ste->checkTrajectory);
    gbv_mark(ste->assertion_OK);
    gbv_mark(ste->check_OK);
    scan_hash(&(ste->trace_tbl), mark_trace_entries);
    if( ste->active ) {
	int nds  = COUNT_BUF(&(ste->fsm->nodes));
	for(int i = 0; i < nds; i++) {
	    gbv_mark(*(weak_buf+2*i));
	    gbv_mark(*(weak_buf+2*i+1));
	    gbv_mark(*(ant_buf+2*i));
	    gbv_mark(*(ant_buf+2*i+1));
	    gbv_mark(*(cons_buf+2*i));
	    gbv_mark(*(cons_buf+2*i+1));
	    gbv_mark(*(cur_buf+2*i));
	    gbv_mark(*(cur_buf+2*i+1));
	    gbv_mark(*(next_buf+2*i));
	    gbv_mark(*(next_buf+2*i+1));
	}
    }
    formula *fp;
    FOR_BUF(weakening_bufp, formula, fp) {
	B_Mark(*fp);
    }
    event_ptr *epp;
    FOR_BUF(event_bufp, event_ptr, epp) {
	gbv_mark((*epp)->H);
	gbv_mark((*epp)->L);
    }
    pop_fsm_env();
    pop_ste();
}

static void
mark_vstate_fn(pointer p)
{
    vstate_ptr vp = (vstate_ptr) p;
    if( vp->mark == 2 ) { return; }
    vp->mark = 2;
    mark_fsm_fn((pointer) (vp->fsm));
}

static void
sweep_vstate_fn(void)
{
    vstate_ptr vp;
    FOR_REC(&vstate_rec_mgr, vstate_ptr, vp) {
	switch( vp->mark ) {
	    case 0:
		break;
	    case 1:
		dispose_hash(&(vp->done), NULLFCN);
		dispose_hash(&(vp->stop_nds), NULLFCN);
		dispose_hash(&(vp->ifc_nds), NULLFCN);
		free_mgr(&(vp->sch_rec_mgr));
		vp->next = vstate_free_list;
		vp->fsm = NULL;
		vstate_free_list = vp;
		vp->mark = 0;
		break;
	    case 2:
		vp->mark = 1;
		break;
	    default:
		DIE("Should not happen");
	}
    }
}

static void
sweep_fsm_fn(void)
{
    fsm_ptr fsm;
    FOR_REC(&fsm_rec_mgr, fsm_ptr, fsm) {
	switch( fsm->mark ) {
	    case 0:
		break;
	    case 1:
		dispose_hash(&(fsm->all_name_tbl), NULLFCN);
		free_mgr(&(fsm->vec_info_rec_mgr));
		free_mgr(&(fsm->ilist_rec_mgr));
		free_mgr(&(fsm->vec_rec_mgr));
		free_mgr(&(fsm->range_rec_mgr));
		free_mgr(&(fsm->idx_list_rec_mgr));
		free_buf(&(fsm->nodes));
		free_buf(&(fsm->composites));
		free_mgr(&(fsm->vis_io_rec_mgr));
		free_mgr(&(fsm->vis_rec_mgr));
		free_mgr(&(fsm->vis_list_rec_mgr));
		free_mgr(&(fsm->attr_list_rec_mgr));
		fsm->next = fsm_free_list;
		fsm_free_list = fsm;
		fsm->mark = 0;
		break;
	    case 2:
		fsm->mark = 1;
		break;
	    default:
		DIE("Should not happen");
	}
    }
}

static void
sweep_ste_fn(void)
{
    ste_ptr ste;
    FOR_REC(&ste_rec_mgr, ste_ptr, ste) {
	switch( ste->mark ) {
	    case 0:
		break;
	    case 1:
		dispose_hash(&(ste->active_weak_tbl), NULLFCN);
		dispose_hash(&(ste->active_ant_tbl), NULLFCN);
		dispose_hash(&(ste->active_cons_tbl), NULLFCN);
		dispose_hash(&(ste->trace_tbl), NULLFCN);
		free_mgr(&(ste->trace_event_rec_mgr));
		free_mgr(&(ste->event_list_rec_mgr));
		free_mgr(&(ste->trace_rec_mgr));
		free_mgr(&(ste->event_rec_mgr));
		free_buf(&(ste->event_buf));
		free_buf(&(ste->weakening_buf));
		ste->next = ste_free_list;
		ste_free_list = ste;
		ste->mark = 0;
		break;
	    case 2:
		ste->mark = 1;
		break;
	    default:
		DIE("Should not happen");
	}
    }
}

static unint
ste_hash_fn(pointer p, unint n)
{
    return( PTR2UINT(p) % n );
}

static formula
ste_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    ste_ptr ste1 = (ste_ptr) p1;
    ste_ptr ste2 = (ste_ptr) p2;
    return( ste1 == ste2 );
}

static pointer
ste_gmap_fn(gmap_info_ptr ip, pointer a)
{
//%%%%%%%%%%%%
    (void) ip;
    (void) a;
    return(a);
}

static pointer
ste_gmap2_fn(gmap_info_ptr ip, pointer a, pointer b)
{
//%%%%%%%%%%%%
    (void) ip;
    (void) a;
    (void) b;
    DIE("Not implemented yet");
}

static unint
idx_list_hash(pointer key, unint n)
{
    idx_list_ptr l = (idx_list_ptr) key;
    return ((((lunint) (l->idx)) + 191*((lunint) (l->next))) % n);
}

static bool
idx_list_equ(pointer k1, pointer k2)
{
    idx_list_ptr l1 = (idx_list_ptr) k1;
    idx_list_ptr l2 = (idx_list_ptr) k2;
    if( l1->idx != l2->idx ) return FALSE;
    return( l1->next == l2->next );
}

static string
op2str(ncomp_ptr cp)
{
    wl_op op = cp->op;
    if( op == op_X ) return( strtemp("op_X") );
    else if( op == op_CONST ) {
	strtemp("op_CONST ");
	return( strappend(Arbi_ToString(cp->arg.value, 10)) );
    }
    else if( op == op_VAR ) return( strtemp("op_VAR") );
    else if( op == op_AND ) return( strtemp("op_AND") );
    else if( op == op_LAT_LEQ ) return( strtemp("op_LAT_LEQ") );
    else if( op == op_OR ) return( strtemp("op_OR") );
    else if( op == op_NOT ) return( strtemp("op_NOT") );
    else if( op == op_EQ ) return( strtemp("op_EQ") );
    else if( op == op_GR ) return( strtemp("op_GR") );
    else if( op == op_ADD ) return( strtemp("op_ADD") );
    else if( op == op_SUB ) return( strtemp("op_SUB") );
    else if( op == op_MUL ) return( strtemp("op_MUL") );
    else if( op == op_ITE ) return( strtemp("op_ITE") );
    else if( op == op_DIV ) return( strtemp("op_DIV") );
    else if( op == op_MOD ) return( strtemp("op_MOD") );
    else if( op == op_SHL ) return( strtemp("op_SHL") );
    else if( op == op_SHR ) return( strtemp("op_SHR") );
    else if( op == op_ASHR ) return( strtemp("op_ASHR") );
    else if( op == op_SX ) {
	Sprintf(buf, "op_SX %d", cp->arg.extension_size);
	return( strtemp(buf) );
    }
    else if( op == op_ZX ) {
	Sprintf(buf, "op_ZX %d", cp->arg.extension_size);
	return( strtemp(buf) );
    }
    else if( op == op_SLICE ) {
	string ret = strtemp("op_SLICE [");
	idx_list_ptr ip = cp->arg.idx_list;
	char sep = ' '; 
	while( ip != NULL ) {
	    Sprintf(buf, "%c%d", sep, ip->idx);
	    sep = ',';
	    strappend(buf);
	    ip = ip->next;
	}
	strappend("]");
	return ret;
    }
    else if( op == op_UPDATE_SLICE ) {
	string ret = strtemp("op_UPDATE_SLICE [");
	idx_list_ptr ip = cp->arg.idx_list;
	char sep = ' '; 
	while( ip != NULL ) {
	    Sprintf(buf, "%c%d", sep, ip->idx);
	    sep = ',';
	    strappend(buf);
	    ip = ip->next;
	}
	strappend("]");
	return ret;
    }
    else if( op == op_WIRE ) return( strtemp("op_WIRE") );
    else if( op == op_MEM_READ ) {
	Sprintf(buf, "op_MEM_READ %d %d %d", cp->arg.mem.addr_size,
					     cp->arg.mem.data_size,
					     cp->arg.mem.lines);
	return( strtemp(buf) );
    }
    else if( op == op_MEM_WRITE ) {
	Sprintf(buf, "op_MEM_WRITE %d %d %d", cp->arg.mem.addr_size,
					      cp->arg.mem.data_size,
					      cp->arg.mem.lines);
	return( strtemp(buf) );
    } else if( op == op_IDELAY ) {
	Sprintf(buf, "op_IDELAY %d %d %d %d", cp->arg.del.min_rise,
					      cp->arg.del.max_rise,
					      cp->arg.del.min_fall,
					      cp->arg.del.max_fall);
	return( strtemp(buf) );

    } else {
	return( strtemp("UNKNOWN op????") );
    }
}

static void
print_nodes(odests fp, fsm_ptr fsm)
{
    nnode_ptr np;
    FOR_BUF(&(fsm->nodes), nnode_rec, np) {
	int start = np->vec->map->from;
	string nname = get_real_name(np->vec, np->idx-start);
	FP(fp, "  %d %s composite:%d fanouts: [",
		      np->idx, nname, np->composite);
	char sep = ' ';
	for(idx_list_ptr ip = np->fanouts; ip != NULL; ip = ip->next) {
	    FP(fp, "%c %d", sep, ip->idx);
	    sep = ',';
	}
	FP(fp, "]\n");
    }
}

static void
print_composites(odests fp)
{
    ncomp_ptr	cp;
    int cnt = 0;
    FOR_BUF(compositesp, ncomp_rec, cp) {
	FP(fp, " %3d %10s sz:%d rank:%d pdel:%d ",
		cnt++, op2str(cp),cp->size,cp->rank,cp->phase_delay);
	FP(fp, "inps: ");
	char *sep = strtemp("[");
	for(ilist_ptr ip = cp->inps; ip != NULL; ip = ip->next) {
	    if( ip->size == 1 ) {
		FP(fp, "%s%d", sep, ip->from);
	    } else {
		FP(fp, "%s[%d:%d]", sep, ip->from, ip->to);
	    }
	    strtemp(",");
	}
	if( cp->inps != NULL ) FP(fp, "] ");
	FP(fp, "outs: ");
	strtemp("[");
	for(ilist_ptr ip = cp->outs; ip != NULL; ip = ip->next) {
	    if( ip->size == 1 ) {
		FP(fp, "%s%d", sep, ip->from);
	    } else {
		FP(fp, "%s[%d:%d]", sep, ip->from, ip->to);
	    }
	    strtemp(",");
	}
	if( cp->outs != NULL ) FP(fp, "]");
	FP(fp, "\n");
    }
}

static string
anon2real(vstate_ptr vp, string aname)
{
    if( *aname == 'a' && *(aname+1) == 'n' ) {
	int idx = atoi(aname+2);
	ilist_ptr ip = *((ilist_ptr *) M_LOCATE_BUF(&(vp->anon_buf), idx));
	g_ptr nds = ilist2nds(ip);
	g_ptr vecs = Merge_Vectors(nds, TRUE);
	string res = strtemp("");
	bool first = TRUE;
	while( !IS_NIL(vecs) ) {
	    if( !first )
		charappend(' ');
	    first = FALSE;
	    strappend(GET_STRING(GET_CONS_HD(vecs)));
	    vecs = GET_CONS_TL(vecs);
	}
	return( res );
    } else {
	return aname;
    }
}

static void
print_sch_tree(vstate_ptr vp, int indent, sch_ptr sch)
{
    if( sch == NULL ) {
	FP(err_fp, "???NULL???");
	return;
    }
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	print_sch_tree(vp, indent+2, sl->sch);
    }
    string vec = anon2real(vp, sch->vec);
    FP(err_fp,"%*s%s <-- %s", indent, "", vec, sch->pfn);
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	string vec = anon2real(vp, sl->sch->vec);
	FP(err_fp," %s", vec);
    }
    FP(err_fp,"\n");
}

static string
vstate2str_fn(pointer p)
{
    vstate_ptr vp = (vstate_ptr) p;
    push_fsm(vp->fsm);
    FP(err_fp, "\n");
    ilist_ptr	*ipp;
    int anon = 0;
    FOR_BUF(&(vp->anon_buf), ilist_ptr, ipp) {
	FP(err_fp, "an%06d: ", anon);
	anon++;
	g_ptr nds = ilist2nds(*ipp);
	g_ptr vecs = Merge_Vectors(nds, TRUE);
	bool first = TRUE;
	while( !IS_NIL(vecs) ) {
	    if( first )
		FP(err_fp, "%s", GET_STRING(GET_CONS_HD(vecs)));
	    else
		FP(err_fp, " %s", GET_STRING(GET_CONS_HD(vecs)));
	    first = FALSE;
	    vecs = GET_CONS_TL(vecs);
	}
	FP(err_fp, "\n");
    } 
    print_sch_tree(vp, 1, vp->sch);
    pop_fsm();
    return( wastrsave(stringsp, "") );
}


static string
sch2tcl(g_ptr *tlp, sch_ptr sch)
{
    if( sch == NULL ) {
	DIE("Should never happen");
    }
    if( sch->children == NULL ) {
	Sprintf(buf, "tr_%d", sch2tcl_cnt);
	sch2tcl_cnt++;
	string res = wastrsave(stringsp, buf);
	//
	string cmd = strtemp("set ");
	strappend(res);
	strappend(" [add_sch_object LEAF {");
	strappend(sch->vec);
	strappend("} {");
	strappend(sch->pfn);
	strappend("} {}]");
	cmd = wastrsave(stringsp, cmd);
	g_ptr nd = Make_STRING_leaf(cmd);
	SET_CONS_HD(*tlp, nd);
	SET_CONS_TL(*tlp, Make_NIL());
	*tlp = GET_CONS_TL(*tlp);
	return( res );
    }
    buffer child_names;
    new_buf(&child_names, 100, sizeof(string));
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	string ch = sch2tcl(tlp, sl->sch);
	push_buf(&child_names, (pointer) &ch);
    }
    Sprintf(buf, "tr_%d", sch2tcl_cnt);
    sch2tcl_cnt++;
    string res = wastrsave(stringsp, buf);
    string cmd = strtemp("set ");
    strappend(res);
    strappend(" [add_sch_object NODE {");
    strappend(sch->vec);
    strappend("} {");
    strappend(sch->pfn);
    strappend("} [list");
    string *sp;
    FOR_BUF(&child_names, string, sp) {
	strappend(" $");
	strappend(*sp);
    }
    strappend("]]");
    cmd = wastrsave(stringsp, cmd);
    g_ptr nd = Make_STRING_leaf(cmd);
    SET_CONS_HD(*tlp, nd);
    SET_CONS_TL(*tlp, Make_NIL());
    *tlp = GET_CONS_TL(*tlp);
    free_buf(&child_names);
    return res;
}

static string
fsm2str_fn(pointer p)
{
    fsm_ptr fsm = (fsm_ptr) p;
    push_fsm(fsm);
    if( RCverbose_fsm_print ) {
	FP(stdout_fp, "FSM for %s with signature:\n%s\nRanks: %d\n",
		      fsm->top_name, fsm->sha256_sig, fsm->ranks);
	FP(stdout_fp, " %d nodes:\n", COUNT_BUF(&(fsm->nodes)));
	print_nodes(stdout_fp, fsm);
	FP(stdout_fp, " %d composites:\n", COUNT_BUF(&(fsm->composites)));
	print_composites(stdout_fp);
    } else {
	FP(stdout_fp, "FSM for %s with %d nodes and %d composites\n",
		      fsm->top_name, COUNT_BUF(&(fsm->nodes))-4,
		      COUNT_BUF(&(fsm->composites)));
    }
    pop_fsm();
    return s_fsm;
}


static string
compute_sha256_signature(fsm_ptr fsm)
{
    SHA256_ptr sha = Begin_SHA256();
    SHA256_printf(sha, "%s %d\n", fsm->top_name, fsm->ranks);
    // Include all information about the nodes
    nnode_ptr np;
    FOR_BUF(&(fsm->nodes), nnode_rec, np) {
	int start = np->vec->map->from;
	vec_info_ptr vp = np->vec;
	SHA256_printf(sha, "  %d %s %s %d composite:%d fanouts: [",
			np->idx, vp->hierarchy, vp->local_name,
			np->idx-start+1, np->composite);
	char sep = ' ';
	for(idx_list_ptr ip = np->fanouts; ip != NULL; ip = ip->next) {
	    SHA256_printf(sha, "%c %d", sep, ip->idx);
	    sep = ',';
	}
	SHA256_printf(sha, "]\n");
    }
    // and the same for all composites
    ncomp_ptr   cp;
    int cnt = 0;
    FOR_BUF(compositesp, ncomp_rec, cp) {
        SHA256_printf(sha, " %3d %s sz:%d rank:%d pdel:%d ",
                cnt++, op2str(cp),cp->size,cp->rank,cp->phase_delay);
        SHA256_printf(sha, "inps: ");
        char *sep = strtemp("[");
        for(ilist_ptr ip = cp->inps; ip != NULL; ip = ip->next) {
            if( ip->size == 1 ) {
                SHA256_printf(sha, "%s%d", sep, ip->from);
            } else {
                SHA256_printf(sha, "%s[%d:%d]", sep, ip->from, ip->to);
            }
            strtemp(",");
        }
        if( cp->inps != NULL ) SHA256_printf(sha, "] ");
        SHA256_printf(sha, "outs: ");
        strtemp("["); 
        for(ilist_ptr ip = cp->outs; ip != NULL; ip = ip->next) {
            if( ip->size == 1 ) {
                SHA256_printf(sha, "%s%d", sep, ip->from);
            } else {
                SHA256_printf(sha, "%s[%d:%d]", sep, ip->from, ip->to);
            }
            strtemp(",");
        }
        if( cp->outs != NULL ) SHA256_printf(sha, "]");
        SHA256_printf(sha, "\n");
    }
    string res = Get_SHA256_hash(sha);
    End_SHA256(sha);
    return( res );
}


static unint
fsm_hash_fn(pointer p, unint n)
{
    fsm_ptr fsm = (fsm_ptr) p;
    return( str_hash(fsm->sha256_sig, n) );
}

static formula
fsm_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    fsm_ptr fsm1 = (fsm_ptr) p1;
    fsm_ptr fsm2 = (fsm_ptr) p2;
    if( fsm1->sha256_sig == fsm2->sha256_sig )
	return( B_One() );
    else
	return( B_Zero() );
}

static string
ste2str_fn(pointer p)
{
    ste_ptr ste = (ste_ptr) p;
    FP(stdout_fp, "STE result for circuit %s simulated to time %d\n",
		   ste->fsm->top_name, ste->max_time);
    return s_ste;
}

static int
get_wexpr_size(g_ptr we)
{
    int sz, mr, Mr, mf, Mf;
    arbi_T ai;
    string s;
    g_ptr  b, c, l, r, cur;
    wl_op  op;
  size_start:
    if( is_W_X(we, &sz) ) { return sz; }
    if( is_W_CONST(we,&sz,&ai) ) { return sz; }
    if( is_W_NAMED_CONST(we,&s,&sz,&ai) ) { return sz; }
    if( is_W_VAR(we,&sz, &s) ) { return sz; }
    if( is_W_EXPLICIT_VAR(we,&sz, &s) ) { return sz; }
    if( is_binary_wexpr(we, &op, &l, &r) ) { we = l; goto size_start; }
    if( is_relation_wexpr(we, &op, &l, &r) ) { return 1; }
    if( is_W_NOT(we, &l) ) { we = l; goto size_start; }
    if( is_W_PRED(we, &s, &l) ) { return 1; }
    if( is_W_SX(we, &sz, &l) ) { return sz; }
    if( is_W_ZX(we, &sz, &l) ) { return sz; }
    if( is_W_ITE(we, &c, &l, &r) ) { we = l; goto size_start; }
    if( is_W_SLICE(we, &l, &r) ) {
	sz = 0;
	while( !IS_NIL(l) ) {
	    sz++;
	    l = GET_CONS_TL(l);
	}
	return sz;
    }
    if( is_W_NAMED_SLICE(we, &s, &l, &r) ) {
	sz = 0;
	while( !IS_NIL(l) ) {
	    sz++;
	    l = GET_CONS_TL(l);
	}
	return sz;
    }
    if( is_W_UPDATE_NAMED_SLICE(we, &b, &s, &l, &r) ) {
	we = b;
	goto size_start;
    }
    if( is_W_IDELAY(we, &sz, &mr, &Mr, &mf, &Mf, &cur, &l) ) {
	return sz;
    }
    if( is_W_CAT(we, &l) ) {
	sz = 0;
	while( !IS_NIL(l) ) {
	    sz += get_wexpr_size(GET_CONS_HD(l));
	    l = GET_CONS_TL(l);
	}
	return sz;
    }
    int d_sz, a_sz, lines;
    if( is_W_MEM_READ(we, &d_sz, &lines, &a_sz, &l, &r) ) { return d_sz; }
    if( is_W_MEM_WRITE(we, &d_sz, &lines, &a_sz, &l, &r, &c) ) {
	we = l;
	goto size_start;
    }
    DIE("Should never happen");
}

static ilist_ptr
ilist_copy(ilist_ptr ip)
{
    if( ip == NULL ) return NULL;
    ilist_ptr copy = (ilist_ptr) new_rec(ilist_rec_mgrp);
    copy->from = ip->from;
    copy->to = ip->to;
    copy->size = ip->size;
    copy->next = ilist_copy(ip->next);
    return copy;
}

static ilist_ptr
make_input_arg(g_ptr we, int sz, hash_record *vtblp, string hier, bool pdel)
{
    string base;
    ilist_ptr inps;
    if( is_W_VAR(we, &sz, &base) ) {
	string vname = mk_vec_name(base, sz);
	inps = map_vector(vtblp, hier, vname, FALSE);
    } else if( is_W_EXPLICIT_VAR(we, &sz, &base) ) {
	inps = map_vector(vtblp, hier, base, FALSE);
    } else {
	// Make a temporary vector
	Sprintf(buf, "__tmp%d", ++temporary_node_cnt);
	string tname = mk_vec_name(buf, sz);
	tname = wastrsave(stringsp, tname);
	inps = declare_vector(vtblp, hier, tname, FALSE, NULL, s_ES);
	if( !compile_expr(vtblp, hier, inps, we, pdel) ) {
	    return NULL;
	}
    }
    return( ilist_copy(inps) );
}

static idx_list_ptr
find_insert_idx(idx_list_ptr ip)
{
    idx_list_ptr res;
    res = (idx_list_ptr) find_hash(&idx_list_uniq_tbl, (pointer) ip);
    if( res == NULL ) {
	res = (idx_list_ptr) new_rec(idx_list_rec_mgrp);
	res->idx = ip->idx;
	res->next = ip->next;
	insert_hash(&idx_list_uniq_tbl, (pointer) res, (pointer) res);
    }
    return res;
}

static void
add_fanout(nnode_ptr np, int comp_idx)
{
    node_comp_pair_rec nc;
    nc.np = np;
    nc.comp_idx = comp_idx;
    if( find_hash(&node_comp_pair_tbl, &nc) != NULL ) return;
    idx_list_rec ir;
    ir.idx = comp_idx;
    ir.next = np->fanouts;
    np->fanouts = find_insert_idx(&ir);
    node_comp_pair_ptr ncp = new_rec(&node_comp_pair_rec_mgr);
    ncp->np = np;
    ncp->comp_idx = comp_idx;
    insert_hash(&node_comp_pair_tbl, ncp, ncp);
}

static void
add_fanouts(ilist_ptr inps, int comp_idx)
{
    FOREACH_NODE(nd, inps) {
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	add_fanout(np, comp_idx);
    }
}

static bool
compile_expr(hash_record *vtblp, string hier, ilist_ptr outs, g_ptr we,
	     bool pdel)
{
    arbi_T value;
    string name;
    int sz, mr, Mr, mf, Mf;
    string base;
    wl_op op;
    g_ptr b, l, r;
    g_ptr cond, cur_state;
    int comp_idx = COUNT_BUF(compositesp);
    int osz = 0;
    FOREACH_NODE(nd, outs) {
	osz++;
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	np->composite = comp_idx;
    }
    ncomp_rec cr;
    cr.rank = 0;
    cr.flag = 0;
    cr.phase_delay = pdel;
    cr.outs = outs;
    if( is_W_VAR(we, &sz, &base) ) {
	cr.op = op_WIRE;
	cr.size = sz;
	string vname = mk_vec_name(base, sz);
	ilist_ptr inps = map_vector(vtblp, hier, vname, FALSE);
	cr.inps = inps;
	add_fanouts(inps, comp_idx);
	push_buf(compositesp, (pointer) &cr);
	return TRUE;
    }
    if( is_W_EXPLICIT_VAR(we, &sz, &name) ) {
	cr.op = op_WIRE;
	cr.size = sz;
	ilist_ptr inps = map_vector(vtblp, hier, name, FALSE);
	cr.inps = inps;
	add_fanouts(inps, comp_idx);
	push_buf(compositesp, (pointer) &cr);
	return TRUE;
    }
    if( is_W_X(we, &sz) ) {
	cr.op = op_X;
	cr.size = sz;
	cr.inps = NULL;
	push_buf(compositesp, (pointer) &cr);
	return TRUE;
    }
    if( is_W_CONST(we,&sz,&value) || is_W_NAMED_CONST(we,&name,&sz,&value) ) {
	ASSERT(value != NULL);
	cr.op = op_CONST;
	cr.size = sz;
	cr.inps = NULL;
	cr.arg.value = value;
	push_buf(compositesp, (pointer) &cr);
	return TRUE;
    }
    if( is_binary_wexpr(we, &op, &l, &r) ) {
	cr.op = op;
	int sz = compute_ilist_length(outs);
	cr.size = sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr linps = make_input_arg(l, sz, vtblp, hier, FALSE);
	ilist_ptr rinps = make_input_arg(r, sz, vtblp, hier, FALSE);
	ilist_ptr inps = linps;
	if( linps == NULL ) {
	    inps = linps = rinps;
	} else {
	    while( linps->next != NULL ) { linps = linps->next; }
	    linps->next = rinps;
	}
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_relation_wexpr(we, &op, &l, &r) ) {
	cr.op = op;
	int sz = 1;
	cr.size = sz;
	int inp_sz = get_wexpr_size(l);
	cr.arg.extension_size = inp_sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr linps = make_input_arg(l, inp_sz, vtblp, hier, FALSE);
	ilist_ptr rinps = make_input_arg(r, inp_sz, vtblp, hier, FALSE);
	ilist_ptr inps = linps;
	if( linps == NULL ) {
	    inps = linps = rinps;
	} else {
	    while( linps->next != NULL ) { linps = linps->next; }
	    linps->next = rinps;
	}
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_W_NOT(we, &l) ) {
	cr.op = op_NOT;
	int sz = compute_ilist_length(outs);
	cr.size = sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr inps = make_input_arg(l, sz, vtblp, hier, FALSE);
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_W_PRED(we, &name, &l) ) {
	if( !compile_expr(vtblp, hier, outs, l, FALSE) ) {
	    return FALSE;
	}
	return TRUE;
    }
    if( is_W_SX(we, &sz, &l) ) {
	cr.op = op_SX;
	cr.size = sz;
	int inp_sz = get_wexpr_size(l);
	cr.arg.extension_size = inp_sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr inps = make_input_arg(l, inp_sz, vtblp, hier, FALSE);
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_W_ZX(we, &sz, &l) ) {
	cr.op = op_ZX;
	cr.size = sz;
	int inp_sz = get_wexpr_size(l);
	cr.arg.extension_size = inp_sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr inps = make_input_arg(l, inp_sz, vtblp, hier, FALSE);
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_W_ITE(we, &cond, &l, &r) ) {
	cr.op = op_ITE;
	int sz = compute_ilist_length(outs);
	cr.size = sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr cinps = make_input_arg(cond, 1, vtblp, hier, FALSE);
	ilist_ptr linps = make_input_arg(l, sz, vtblp, hier, FALSE);
	ilist_ptr rinps = make_input_arg(r, sz, vtblp, hier, FALSE);
	ilist_ptr inps = cinps;
	while(cinps->next != NULL) { cinps = cinps->next; }
	cinps->next = linps;
	while(cinps->next != NULL) { cinps = cinps->next; }
	cinps->next = rinps;
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_W_SLICE(we, &l, &r) || is_W_NAMED_SLICE(we, &name, &l, &r) ) {
	cr.op = op_SLICE;
	int sz = compute_ilist_length(outs);
	int sel_sz = 0;
	for(g_ptr cur = l; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
            sel_sz++;
        }   
	if( sz != sel_sz ) {
	    FP(err_fp, "Slice operation has inconsistent output size");
	    FP(err_fp, " (%d!=%d)\n", sz, sel_sz);
	    g_ptr pp = Make_VAR_leaf(wastrsave(stringsp, "Pwexpr"));
	    pp = Find_Function(symb_tbl, pp);
	    g_ptr res = Make_APPL_ND(pp, we);
	    INC_REFCNT(we);
	    Eval(res);
	    FP(err_fp, "in expression:\n%s\n", GET_STRING(res));
	    report_source_locations(err_fp);
	    Rprintf("");
	}
	cr.size = sz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	int inp_sz = get_wexpr_size(r);
	ilist_ptr inps = make_input_arg(r, inp_sz, vtblp, hier, FALSE);
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	if( inp_sz != compute_ilist_length(inps) ) {
	    FP(err_fp, "Slice operation has inconsistent input size (%d!=%d)\n",
		    inp_sz, compute_ilist_length(inps));
	    report_source_locations(err_fp);
	    Rprintf("");
	}
	add_fanouts(inps, comp_idx);
	idx_list_ptr idx_list = NULL;
	idx_list_ptr *prevp;
	prevp = &idx_list;
	while( !IS_NIL(l) ) {
	    int idx = GET_INT(GET_CONS_HD(l));
	    idx_list_ptr cur = (idx_list_ptr) new_rec(idx_list_rec_mgrp);
	    cur->idx = inp_sz-idx-1;
	    cur->next = NULL;
	    *prevp = cur;
	    prevp = &(cur->next);
	    l = GET_CONS_TL(l);
	}
	cp->arg.idx_list = idx_list;
	return( TRUE );
    }
    if( is_W_UPDATE_NAMED_SLICE(we, &b, &name, &l, &r) ) {
	cr.op = op_UPDATE_SLICE;
	int osz = compute_ilist_length(outs);
	int sel_sz = 0;
	for(g_ptr cur = l; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
            sel_sz++;
        }
	int base_sz = get_wexpr_size(b);
	if( base_sz != osz ) {
	    FP(err_fp, "Update-slice with invalid base size (%d!=%d)\n",
		    base_sz, osz);
	    report_source_locations(err_fp);
	    Rprintf("");
	}
	int inp_sz = get_wexpr_size(r);
	if( inp_sz != sel_sz ) {
	    FP(err_fp, "Update-slice with invalid input size (%d!=%d)\n",
		    inp_sz, sel_sz);
	    report_source_locations(err_fp);
	    Rprintf("");
	}
	// Must push the incomplete record on its correct place!
	cr.size = osz;
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr linps = make_input_arg(b, osz, vtblp, hier, FALSE);
	ilist_ptr rinps = make_input_arg(r, inp_sz, vtblp, hier, FALSE);
        ilist_ptr inps = linps;
        if( linps == NULL ) {
            inps = linps = rinps;
        } else {
            while( linps->next != NULL ) { linps = linps->next; }
            linps->next = rinps;
        }
        ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
        cp->inps = inps;
        add_fanouts(inps, comp_idx);
	idx_list_ptr idx_list = NULL;
	idx_list_ptr *prevp;
	prevp = &idx_list;
	while( !IS_NIL(l) ) {
	    int idx = GET_INT(GET_CONS_HD(l));
	    idx_list_ptr cur = (idx_list_ptr) new_rec(idx_list_rec_mgrp);
	    cur->idx = osz-idx-1;
	    cur->next = NULL;
	    *prevp = cur;
	    prevp = &(cur->next);
	    l = GET_CONS_TL(l);
	}
	cp->arg.idx_list = idx_list;
        return( TRUE );
    }
    if( is_W_IDELAY(we, &sz, &mr, &Mr, &mf, &Mf, &cur_state, &l) ) {
	cr.op = op_IDELAY;
	cr.size = sz;
	cr.arg.del.min_rise = mr;
	cr.arg.del.max_rise = Mr;
	cr.arg.del.min_fall = mf;
	cr.arg.del.max_fall = Mf;
	push_buf(compositesp, (pointer) &cr);
	int osz = get_wexpr_size(cur_state);
	if( osz != sz ) {
	    FP(err_fp, "W_IDELAY with invalid current state size (%d!=%d)\n",
		       osz, sz);
	    report_source_locations(err_fp);
	    Rprintf("");
	}
	ilist_ptr inps = make_input_arg(cur_state, sz, vtblp, hier, FALSE);
	ilist_ptr cur = inps;
	while( !IS_NIL(l) ) {
	    g_ptr e = GET_CONS_HD(l);
	    int osz = get_wexpr_size(e);
	    if( osz != sz ) {
		FP(err_fp, "W_IDELAY with invalid input size (%d!=%d)\n",
			   osz, sz);
		report_source_locations(err_fp);
		Rprintf("");
	    }
	    ilist_ptr tmp = make_input_arg(e, sz, vtblp, hier, FALSE);
	    while( cur->next != NULL ) { cur = cur->next; }
	    cur->next = tmp;
	    l = GET_CONS_TL(l);
	}
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    if( is_W_CAT(we, &l) ) {
	cr.op = op_WIRE;
	cr.size = osz;
	// Must push the incomplete record on its correct place!
	push_buf(compositesp, (pointer) &cr);
	//
	ilist_ptr inps = NULL;
	ilist_ptr cur = inps;
	while( !IS_NIL(l) ) {
	    g_ptr e = GET_CONS_HD(l);
	    int sz = get_wexpr_size(e);
	    ilist_ptr tmp = make_input_arg(e, sz, vtblp, hier, FALSE);
	    if( cur == NULL ) {
		inps = tmp;
		cur = inps;
	    } else {
		while( cur->next != NULL ) { cur = cur->next; }
		cur->next = tmp;
	    }
	    l = GET_CONS_TL(l);
	}
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    int d_sz, a_sz, lines;
    g_ptr mem, addr;
    if( is_W_MEM_READ(we, &a_sz, &lines, &d_sz, &mem, &addr) ) {
	cr.op = op_MEM_READ;
	cr.size = d_sz;
	cr.arg.mem.addr_size = a_sz;
	cr.arg.mem.lines = lines;
	cr.arg.mem.data_size = d_sz;
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr inps = make_input_arg(addr, a_sz, vtblp, hier, FALSE);
	ilist_ptr cur = inps;
	while( cur->next != NULL ) cur = cur->next;
	int mem_sz = lines * d_sz;
	cur->next = make_input_arg(mem, mem_sz, vtblp, hier, FALSE);
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    g_ptr data;
    if( is_W_MEM_WRITE(we, &a_sz, &lines, &d_sz, &mem, &addr, &data) ) {
	cr.op = op_MEM_WRITE;
	int mem_sz = lines * d_sz;
	cr.size = mem_sz;
	cr.arg.mem.addr_size = a_sz;
	cr.arg.mem.lines = lines;
	cr.arg.mem.data_size = d_sz;
	push_buf(compositesp, (pointer) &cr);
	ilist_ptr inps = make_input_arg(addr, a_sz, vtblp, hier, FALSE);
	ilist_ptr cur = inps;
	while( cur->next != NULL ) cur = cur->next;
	cur->next = make_input_arg(data, d_sz, vtblp, hier, FALSE);
	while( cur->next != NULL ) cur = cur->next;
	cur->next = make_input_arg(mem, mem_sz, vtblp, hier, FALSE);
	ncomp_ptr cp = (ncomp_ptr) M_LOCATE_BUF(compositesp, comp_idx);
	cp->inps = inps;
	add_fanouts(inps, comp_idx);
	return( TRUE );
    }
    DIE("Should never happen");
}



static string
mk_vec_name(string base, int sz)
{
    if( sz == 1 ) return base;
    Sprintf(tmp_name_buf, "%s[%d:0]", base, sz-1);
    return tmp_name_buf;
}


static ilist_ptr
get_lhs_indices(hash_record *vtblp, string hier, g_ptr e)
{
    int sz;
    string base;
    g_ptr idx_list, sub_expr, cat_list;
    if( is_W_VAR(e, &sz, &base) ) {
	// Variable
	string vname = mk_vec_name(base, sz);
	ilist_ptr res = map_vector(vtblp, hier, vname, FALSE);
	return res;
    } else if( is_W_EXPLICIT_VAR(e, &sz, &base) ) {
	// Variable
	ilist_ptr res = map_vector(vtblp, hier, base, FALSE);
	return res;
    } else if( is_W_SLICE(e, &idx_list, &sub_expr) ||
	       is_W_NAMED_SLICE(e, &base, &idx_list, &sub_expr) ) {
	// Slice
	ilist_ptr vlist = get_lhs_indices(vtblp, hier, sub_expr);
	int len = compute_ilist_length(vlist);
	ilist_ptr res = NULL;
	for(g_ptr cur = idx_list; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	    int idx = GET_INT(GET_CONS_HD(cur));
	    int loc = len-idx-1;
	    ilist_ptr tmp = translate_range(vlist, loc, loc);
	    res = ilist_append(res, tmp);
	}
	return res;
    } else if( is_W_CAT(e, &cat_list) ) {
	ilist_ptr res = NULL;
	for(g_ptr cur = cat_list; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	    ilist_ptr tmp = get_lhs_indices(vtblp, hier, GET_CONS_HD(cur));
	    res = ilist_append(res, tmp);
	}
	return res;
    } else {
	Fail_pr("Illegal lhs expression in assignment");
	longjmp(*start_envp, 1);
    }
}

static void
report_source_locations(odests dfp)
{
    FP(dfp,"While processing:\n");
    g_ptr *gpp;
    int indent = 2;
    FUB_ROF(&attr_buf, g_ptr, gpp) {
	g_ptr attrs = *gpp;
	for(g_ptr l = attrs; !IS_NIL(l); l = GET_CONS_TL(l)) {
	    string key = GET_STRING(GET_CONS_HD(GET_CONS_HD(l)));
	    string val = GET_STRING(GET_CONS_TL(GET_CONS_HD(l)));
	    if( strcmp(key, "module") == 0 && strcmp(val, "_WrApPeR_") != 0) {
		FP(dfp, "%*s%s\n", indent, "", val);
		indent += 4;
	    } else if( strcmp(key, "src") == 0 ) {
		FP(dfp, " in %s\n", val);
	    } else if( strcmp(key, "fl_src") == 0 ) {
		FP(dfp, " in %s\n", val);
	    }
	}
    }
}

static ilist_ptr
declare_vector(hash_record *vtblp, string hier, string name,
		bool transient, ilist_ptr act_map, string value_list)
{
    vec_info_ptr ip;
    ip = (vec_info_ptr) new_rec(vec_info_rec_mgrp);
    ip->next = NULL;
    ip->value_list = value_list;
    ip->transient = transient;
    ip->local_name = name;
    vec_ptr vp = split_vector_name(sop,name);
    ip->declaration = vp;
    string sig = get_vector_signature(vp);
    ip->signature = sig;
    ip->hierarchy = wastrsave(stringsp, hier);
    int sz = vec_size(vp);
    ip->size = sz;
    if( transient ) {
	int asz = compute_ilist_length(act_map);
	if( sz != asz ) {
	    FP(err_fp,"\nLength mismatch between %s%s and ", hier, name);
	    base_print_ilist(act_map);
	    FP(err_fp," (%d!=%d)\n", sz, asz);
	    report_source_locations(err_fp);
	    Rprintf("");
	}
	ip->map = act_map;
    } else {
	ilist_ptr map = (ilist_ptr) new_rec(ilist_rec_mgrp);
	int vector_cnt = COUNT_BUF(nodesp);
	map->from = vector_cnt;
	map->to = vector_cnt+sz-1;
	map->next = NULL;
	map->size = sz;
	ip->map = map;
	for(int i = 0; i < sz; i++) {
	    nnode_rec nr;
	    nr.vec = ip;
	    nr.idx = vector_cnt;
	    nr.has_phase_event = FALSE;
	    nr.has_weak = FALSE;
	    nr.has_ant = FALSE;
	    nr.has_cons = FALSE;
	    nr.has_trace = FALSE;
	    nr.is_top_input = FALSE;
	    nr.is_top_output = FALSE;
	    vector_cnt++;
	    nr.composite = -1;
	    nr.fanouts = NULL;
	    nr.draw_info = NULL;
	    push_buf(nodesp, (pointer) &nr);
	}
    }
    vec_info_ptr oip = (vec_info_ptr) find_hash(vtblp, sig);
    if( oip == NULL ) {
	insert_hash(vtblp, sig, ip);
    } else {
	while( oip->next ) { oip = oip->next; }
	oip->next = ip;
    }
    string full_vname = strtemp(hier);
    strappend(sig);
    full_vname = wastrsave(stringsp, full_vname);

    vec_info_ptr cip = (vec_info_ptr) new_rec(vec_info_rec_mgrp);
    *cip = *ip;
    oip = find_hash(all_name_tblp, full_vname);
    if( oip == NULL ) {
	insert_hash(all_name_tblp, full_vname, cip);
    } else {


	while( oip->next ) { oip = oip->next; }
	oip->next = cip;


    }
    return( ip->map );
}

static int
find_node_index(vec_ptr decl, vec_ptr vp, int start)
{
    int res = start;
    while( 1 ) {
	while( vp && vp->type == TXT ) {
	    vp = vp->next;
	    decl = decl->next;
	}
	if(vp == NULL) {
	    ASSERT(decl == NULL);
	    return res;
	}
	int stride = get_stride(decl->next);
	range_ptr r = decl->u.ranges;
	while( r != NULL && !inside(vp->u.ranges->upper, r->upper, r->lower) ) {
	    res += (abs(r->upper-r->lower)+1)*stride;
	    r = r->next;
	}
	if( r == NULL) { return -1; }
	res += abs(r->upper - vp->u.ranges->upper)*stride;
	vp = vp->next;
	decl = decl->next;
    }
}

static string
idx2name(int idx)
{
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    int start = np->vec->map->from;
    string nname = get_real_name(np->vec, np->idx-start);
    return nname;
}

static bool
idx_is_user_defined(int idx)
{
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    vec_ptr decl = np->vec->declaration;
    while( decl != NULL ) {
	if( decl->type != INDEX ) {
	    string n = decl->u.name;
	    if( *n == '_' && *(n+1) == '$' ) return FALSE;
	}
	decl = decl->next;
    }
    return TRUE;
}

static bool
ilist_is_user_defined(ilist_ptr il)
{
    FOREACH_NODE(nd, il) {
	if( nd <= 3 ) return( FALSE );
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	if( np->is_top_input ) return( FALSE );
	if( !np->draw_info ) return( FALSE );
	if( !idx_is_user_defined(nd) ) return( FALSE );
    }
    return TRUE;
}

static ilist_ptr
vec2indices(string name)
{
   vec_ptr vp0 = split_vector_name(sop, name);
    string sig = get_vector_signature(vp0);
    vec_info_ptr ip = (vec_info_ptr) find_hash(all_name_tblp, sig);
    while( ip != NULL ) {
	vec_ptr dp = ip->declaration;
	// Ignore the hierarchy prefix
	vec_ptr vp = vp0->next;
	dp = dp->next;
	//
	if( is_full_range(dp, vp) ) {
	    return( ilist_copy(ip->map) );
	}
	idx_map_result = NULL;
	im_cur = NULL;
	if( setjmp(node_map_jmp_env) == 0 ) {
	    map_node(dp, vp, ip->map, 0);
	    return( idx_map_result );
	} 
	ip = ip->next;
    }
    Fail_pr("Cannot find vector %s", name);
    return NULL;
}

static int
name2idx(string name)
{
    vec_ptr vp = split_vector_name(sop, name);
    string sig = get_vector_signature(vp);
    vec_info_ptr ip = (vec_info_ptr) find_hash(all_name_tblp, sig);
    while( ip != NULL ) {
	int res = find_node_index(ip->declaration, vp, ip->map->from);
	if( res >= 0 ) return res;
	ip = ip->next;
    }
    Fail_pr("Cannot find node named '%s'", name);
    return -1;
}

static ilist_ptr
map_vector(hash_record *vtblp, string hier, string name, bool ignore_missing)
{
    if( strncmp("0b", name, 2) == 0 ) {
        // A constant
        ilist_ptr res = NULL;
        for(string s = name+strlen(name)-1; s >= name+2; s--) {
            ilist_ptr ip = (ilist_ptr) new_rec(ilist_rec_mgrp);
            ip->next = res;
            ip->size = 1;
            if( *s == '0' ) { ip->from = ip->to = 0; }
            else if( *s == '1' ) { ip->from = ip->to = 1; }
            else if( *s == 'x' ) { ip->from = ip->to = 2; }
            else {
                Fail_pr("Illegal constant (%s)", name);
                return NULL;
            }
            res = ip;
        }
        return res;
    }
    vec_ptr vp = split_vector_name(sop, name);
    string sig = get_vector_signature(vp);
    vec_info_ptr oip = (vec_info_ptr) find_hash(vtblp,sig);
    if( oip == NULL ) {
	bool is_assertion = ( strncmp(name, "assert__", 8) == 0 );
        if( !ignore_missing && !is_assertion ) {
            FP(warning_fp, "Undeclared signal %s in %s.\n", name, hier);
            report_source_locations(warning_fp);
        }
	string new_name;
	if( is_assertion ) {
	    new_name = name;
	} else {
	    sprintf(tmp_name_buf, "_$$%d_%s", undeclared_node_cnt, name);
	    undeclared_node_cnt++;
	    new_name = wastrsave(stringsp, tmp_name_buf);
	}
        if( !ignore_missing && !is_assertion ) {
            FP(warning_fp, "Replaced %s with %s\n\n", name, new_name);
        }
        name = new_name;
        declare_vector(vtblp, hier, name, FALSE, NULL, s_ES);
        vp = split_vector_name(sop, name);
        sig = get_vector_signature(vp);
        oip = (vec_info_ptr) find_hash(vtblp,sig);
    }
    vec_info_ptr ip = oip;
    FailBuf[0] = 0;
    while( ip != NULL ) {
        vec_ptr dp = ip->declaration;
        idx_map_result = NULL;
        if( is_full_range(dp, vp) ) {
            ilist_ptr res = ilist_copy(ip->map);
            return( res );
        }
        im_cur = NULL;
        if( setjmp(node_map_jmp_env) == 0 ) {
            map_node(dp, vp, ip->map, 0);
            return( idx_map_result );
        } else {
            ip = ip->next;
        }
    }

    vec_list_ptr vl = Expand_vector(sop, vp);
    ilist_ptr final_idx_map_result = NULL;
    ilist_ptr *next_list = &final_idx_map_result; 

    while( vl != NULL ) {
	vec_ptr vp = vl->vec;
	ip = oip;
	idx_map_result = NULL;
	while( ip != NULL && idx_map_result == NULL ) {
	    vec_ptr dp = ip->declaration;
	    if( is_full_range(dp, vp) ) {
		idx_map_result = ilist_copy(ip->map);
	    } else {
		im_cur = NULL;
		if( setjmp(node_map_jmp_env) == 0 ) {
		    map_node(dp, vp, ip->map, 0);
		} else {
		    idx_map_result = NULL;
		}
	    ip = ip->next;
	    }
	}
	if( idx_map_result == NULL ) {
            FP(err_fp, "Cannot map ");
	    while( vp != NULL ) {
		if( vp->type == TXT ) {
		    FP(err_fp, "%s", vp->u.name);
		} else {
		    FP(err_fp, "%d", vp->u.ranges->upper);
		}
		vp = vp->next;
	    }
            FP(err_fp, "\n%s\n", FailBuf);
            report_source_locations(err_fp);
            Rprintf("");
	}
	*next_list = idx_map_result;
	next_list = &(idx_map_result->next);
	vl = vl->next;
    }
    return( final_idx_map_result );
}

static int
vec_size(vec_ptr vec)
{
    if( vec == NULL ) {
	return 1;
    }
    int rem = vec_size(vec->next);
    if( vec->type == TXT ) {
	return rem;
    } else {
	// Indices
	int sum = 0;
	for(range_ptr rp = vec->u.ranges; rp != NULL; rp = rp->next) {
	    sum += (abs(rp->upper-rp->lower)+1)*rem;
	}
	return sum;
    }
}

static int
get_stride(vec_ptr vp)
{
    int res = 1;
    while( vp != NULL ) {
	if( vp->type == INDEX ) {
	    int sum = 0;
	    for(range_ptr rp = vp->u.ranges; rp != NULL; rp = rp->next) {
		sum += abs(rp->upper-rp->lower+1);
	    }
	    res = res * sum;
	}
	vp = vp->next;
    }
    return( res );
}

static bool
inside(int i, int upper, int lower)
{
    if( upper < lower && upper <= i && i <= lower ) { return TRUE; }
    if( upper >= lower && upper >= i && i >= lower ) { return TRUE; }
    return FALSE;
}

static int
find_index_from_end(int i, range_ptr rp)
{
    // Find the range containing i
    int idx = 0;
    while( rp != NULL && !inside(i, rp->upper, rp->lower) ) {
	idx += abs(rp->upper-rp->lower)+1;
	rp = rp->next;
    }
    if( rp == NULL ) {
	Fail_pr("Index %d not within the declared range", i);
	longjmp(node_map_jmp_env, 1);
    }
    idx = idx + abs(i-rp->upper);
    return idx;
}

static bool
is_full_range(vec_ptr v1, vec_ptr v2)
{
  full_range_restart:
    if( v1 == NULL ) {
	ASSERT(v2 == NULL );
	return TRUE;
    }
    if( v1->type == TXT ) {
	ASSERT(v2->type == TXT);
	if( !STREQ(v1->u.name, v2->u.name) ) return FALSE;
	v1 = v1->next;
	v2 = v2->next;
	goto full_range_restart;
    }
    range_ptr r1 = v1->u.ranges;
    range_ptr r2 = v2->u.ranges;
    while( r1 != NULL ) {
	if( r2 == NULL ) return FALSE;
	if( r1->upper != r2->upper ) return FALSE;
	if( r1->lower != r2->lower ) return FALSE;
	r1 = r1->next;
	r2 = r2->next;
    }
    if( r2 != NULL ) return FALSE;
    v1 = v1->next;
    v2 = v2->next;
    goto full_range_restart;
}

static int
ilist_sel(ilist_ptr ip, int idx)
{
    while( ip->size <= idx ) {
	idx -= ip->size;
	ip = ip->next;
    }
    if( ip->from >= ip->to ) {
	return( ip->from-idx );
    } else {
	return( ip->from+idx );
    }
}

static void
map_node(vec_ptr decl, vec_ptr ivec, ilist_ptr map, int cur)
{
    vec_ptr vec = ivec;
    while( vec != NULL && vec->type == TXT ) {
	ASSERT(decl != NULL && decl->type == TXT);
	ASSERT( STREQ(decl->u.name, vec->u.name) );
	vec = vec->next;
	decl = decl->next;
    }
    if( vec == NULL ) {
	ASSERT(decl == NULL);
	int rcur = ilist_sel(map, cur);
	if( idx_map_result == NULL ) {
	    idx_map_result = (ilist_ptr) new_rec(ilist_rec_mgrp);
	    im_cur = idx_map_result;
	    im_cur->next = NULL;
	    im_cur->from = rcur;
	    im_cur->to = rcur;
	    im_cur->size = 1;
	    return;
	}
	if( im_cur->from >= im_cur->to && (rcur == (im_cur->to-1)) ) {
	    im_cur->to = rcur;
	    (im_cur->size)++;
	    return;
	}
	if( im_cur->from <= im_cur->to && (rcur == (im_cur->to+1)) ) {
	    im_cur->to = rcur;
	    (im_cur->size)++;
	    return;
	}
	ilist_ptr tmp = (ilist_ptr) new_rec(ilist_rec_mgrp);
	im_cur->next = tmp;
	im_cur = im_cur->next;
	im_cur->next = NULL;
	im_cur->from = rcur;
	im_cur->to = rcur;
	im_cur->size = 1;
	return;
    }
    // Indices
    ASSERT(decl != NULL && decl->type == INDEX);
    int stride = get_stride(decl->next);
    for(range_ptr rp = vec->u.ranges; rp != NULL; rp = rp->next) {
	if( rp->upper >= rp->lower ) {
	    for(int i = rp->upper; i >= rp->lower; i--) {
		int idx = find_index_from_end(i, decl->u.ranges);
		map_node(decl->next, vec->next, map, cur+(stride*idx));
	    }
	} else {
	    for(int i = rp->upper; i <= rp->lower; i++) {
		int idx = find_index_from_end(i, decl->u.ranges);
		map_node(decl->next, vec->next, map, cur+(stride*idx));
	    }
	}
    }
}

static int
compute_ilist_length(ilist_ptr l)
{
    int sz = 0;
    while( l != NULL ) {
	sz = sz + l->size;
	l = l->next;
    }
    return sz;
}

static ilist_ptr
ilist_append(ilist_ptr l1, ilist_ptr l2)
{
    if( l1 == NULL ) return l2;
    ilist_ptr cur = l1;
    while( cur->next != NULL ) {
	cur = cur->next;
    }
    cur->next = l2;
    return l1;
}

static ilist_ptr
append_range(ilist_ptr l, int i_from, int i_to)
{
    if( l == NULL ) {
	ilist_ptr res = (ilist_ptr) new_rec(ilist_rec_mgrp);
	res->next = NULL;
	res->from = i_from;
	res->to   = i_to;
	res->size = abs(i_to - i_from) + 1;
	return( res );
    }
    ilist_ptr cur = l;
    while( cur->next != NULL ) { cur = cur->next; }
    // Can we extend the last range?
    bool cur_single = cur->from == cur->to;
    bool cur_down   = cur->from >  cur->to;
    bool cur_up     = cur->from <  cur->to;
    bool new_single = i_from == i_to;
    bool new_down   = i_from >  i_to;
    bool new_up     = i_from <  i_to;
    if(    (cur_single || cur_up)
	&& ((cur->to+1) == i_from)
	&& (new_up || new_single) )
    {
	cur->to = i_to;
	cur->size = abs(cur->to - cur->from) + 1;
	return l;
    }
    if(    (cur_single || cur_down)
	&& ((cur->to-1) == i_from)
	&& (new_down || new_single) )
    {
	cur->to = i_to;
	cur->size = abs(cur->to - cur->from) + 1;
	return l;
    }
    ilist_ptr res = (ilist_ptr) new_rec(ilist_rec_mgrp);
    res->next = NULL;
    res->from = i_from;
    res->to   = i_to;
    res->size = abs(i_to - i_from) + 1;
    cur->next = res;
    return( l );
}

static ilist_ptr
translate_range(ilist_ptr map, int from, int to)
{
    ASSERT( from <= to );
    ilist_ptr result = NULL;
    int cur = 0;
    int size = to-from+1;
    while( map != NULL ) {
	int next = cur + map->size;
	if( next > from ) break;
	cur = next;
	map = map->next;
    }
    ASSERT( map != NULL );
    while( map != NULL ) {
	int direction = (map->from > map->to)? -1 : 1;
	int start = map->from + (from-cur)*direction;
	int remain = map->size - abs(start - map->from);
	if( remain >= size ) {
	    int end = start + (size-1)*direction;
	    result = append_range(result, start, end);
	    return( result );
	}
	int end = map->to;
	result = append_range(result, start, end);
	int used = abs(end-start)+1;
	from = from+used;
	size = size-used;
	cur = cur + map->size;
	map = map->next;
    }
    DIE("Should never happen!");
}

#define MSB(sz)			    0
#define LSB(sz)			    ((sz)-1)
#define IS_LSB(sz,i)		    (((int) (i)) == (sz-1))
#define IS_MSB(sz,i)		    ((i) == 0)
#define NEXT_TO_LSB(sz,i)	    ((i)+1)
#define NEXT_TO_MSB(sz,i)	    ((i)-1)
#define CNT_TOWARDS_LSB(sz,i,cnt)   ((i)+(cnt))
#define CNT_TOWARDS_MSB(sz,i,cnt)   ((i)-(cnt))
#define OUTSIDE(sz,i)		    (((i) < 0) || ((i) >= (sz)))
#define FROM_LSB_TO_MSB(sz,i)	    for(int i = sz-1; i >= 0; i--) 
#define FROM_MSB_TO_LSB(sz,i)	    for(unint i = 0; i < (unint) sz; i++) 

static void
geq_fn(int size, gbv *av, gbv *bv, gbv *resv) 
{
    gbv geH = c_ONE;
    gbv geL = c_ZERO;
    FROM_LSB_TO_MSB(size, i) {
	gbv aH = *(av+2*i);
	gbv aL = *(av+2*i+1);
	gbv bH = *(bv+2*i);
	gbv bL = *(bv+2*i+1);
	geH = c_OR(c_AND(aH,bL),c_AND(c_OR(aH,bL),geH));
	geL = c_AND(c_OR(aL,bH),c_OR(c_AND(aL,bH),geL));
    }
    *resv   = geH;
    *(resv+1) = geL;
    return;
}

static void
sub_fn(int size, gbv *av, gbv *bv, gbv *resv) 
{
    gbv difH;
    gbv difL;
    gbv cinH = c_ONE;
    gbv cinL = c_ZERO;
    FROM_LSB_TO_MSB(size, i) {
	gbv aH = *(av+2*i);
	gbv aL = *(av+2*i+1);
	gbv bH = *(bv+2*i+1);
	gbv bL = *(bv+2*i);
        difH = c_OR(
                c_AND(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL),
                c_AND(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH));
        difL = c_AND(
                c_OR(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH),
                c_OR(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL));
        cinH = c_OR(c_OR(c_AND(aH,bH),c_AND(aH,cinH)),
                      c_AND(bH,cinH));
        cinL = c_AND(c_AND(c_OR(aL,bL),c_OR(aL,cinL)),
                       c_OR(bL,cinL));
	*(resv+2*i)   = difH;
	*(resv+2*i+1) = difL;
    }
}

static void
ITE_fn(int size, gbv cH, gbv cL, gbv *av, gbv *bv,
       gbv *resv)
{
    FROM_MSB_TO_LSB(size, i) {
	gbv aH  = *(av+2*i);
	gbv aL  = *(av+2*i+1);
	gbv bH  = *(bv+2*i);
	gbv bL  = *(bv+2*i+1);
	*(resv+2*i)   = c_OR(c_OR(c_AND(aH,bH),c_AND(cH,aH)),
			       c_AND(cL,bH));
	*(resv+2*i+1) = c_AND(c_AND(c_OR(aL,bL),c_OR(cL,aL)),
			       c_OR(cH,bL));
    }
}

static void
shift_left_by_1(int size, gbv *vp, gbv iH, gbv iL,
		gbv *resp)
{
    for(int i = 1; i < size; i++) {
	*(resp+2*(i-1)) = *(vp+2*i);
	*(resp+2*(i-1)+1) = *(vp+2*i+1);
    }
    *(resp+2*(size-1)) = iH;
    *(resp+2*(size-1)+1) = iL;
}

static void
sshl_fun(int size, gbv *vp, int cnt, gbv *resp)
{
    FROM_MSB_TO_LSB(size, i) {
	int idx = CNT_TOWARDS_LSB(size, i, cnt);
	if( OUTSIDE(size, idx) ) {
	    *(resp+2*i)   = c_ZERO;
	    *(resp+2*i+1) = c_ONE;
	} else {
	    *(resp+2*i)   = *(vp+2*idx);
	    *(resp+2*i+1) = *(vp+2*idx+1);
	}
    }
}

static void
sshr_fun(int size, gbv *vp, int cnt, gbv *resp)
{
    FROM_LSB_TO_MSB(size, i) {
	int idx = CNT_TOWARDS_MSB(size, i, cnt);
	if( OUTSIDE(size, idx) ) {
	    *(resp+2*i)   = c_ZERO;
	    *(resp+2*i+1) = c_ONE;
	} else {
	    *(resp+2*i)   = *(vp+2*idx);
	    *(resp+2*i+1) = *(vp+2*idx+1);
	}
    }
    
}

static void
sashr_fun(int size, gbv *vp, int cnt, gbv *resp)
{
    FROM_LSB_TO_MSB(size, i) {
	int idx = CNT_TOWARDS_MSB(size, i, cnt);
	if( OUTSIDE(size, idx) ) {
	    *(resp+2*i)   = *(vp+2*MSB(size));
	    *(resp+2*i+1) = *(vp+2*MSB(size)+1);
	} else {
	    *(resp+2*i)   = *(vp+2*idx);
	    *(resp+2*i+1) = *(vp+2*idx+1);
	}
    }
    
}

static void
add_op_todo(ncomp_ptr cp)
{
    if( cp->flag ) { return; }
    cp->flag = TRUE;
    buffer *bp;
    bp = (buffer *) M_LOCATE_BUF(sim_wheel_bufp, cp->rank);
    push_buf(bp, &cp);
}

static void
do_phase(ste_ptr ste)
{
    nnode_ptr np;
    int idx = 0;
    (ste->cur_time)++;
    FOR_BUF(nodesp, nnode_rec, np) {
	gbv newH, newL;
	if( idx >3 && np->composite == -1 ) {
	    // Input
	    newH = newL = c_ONE;
	    update_node(ste, idx, newH, newL);
	} else if( np->has_phase_event ) {
	    np->has_phase_event = FALSE;
	    newH = *(next_buf+2*idx);
	    newL = *(next_buf+2*idx+1);
	    update_node(ste, idx, newH, newL);
	}
	idx++;
    }
    (ste->cur_time)--;
}

static int
do_combinational(ste_ptr ste)
{
    bool empty = FALSE;
    int iterations = RCStep_limit;
    int todo = 0;
    while( !empty ) {
	int rank = COUNT_BUF(sim_wheel_bufp) - 1;
	empty = TRUE;
	if( --iterations < 0 ) {
	    FP(err_fp,"Combinational relaxtion failed. 0-delay loop likely.\n");
	    return -1;
	}
//FP(err_fp, "\n\niteration: %d\n", iterations);
	do {
	    buffer *bp = (buffer *) M_LOCATE_BUF(sim_wheel_bufp, rank);
	    if( COUNT_BUF(bp) > 0 ) {
		empty = FALSE;
		while( COUNT_BUF(bp) > 0 ) {
		    ncomp_ptr cp;
		    pop_buf(bp, &cp);
//FP(err_fp, "Computing the result for output: "); DBG_print_ilist(cp->outs);
		    cp->flag = FALSE;
		    if( quit_simulation_early ) return -1;
		    todo += do_wl_op(ste, cp);
		}
	    }
	    rank--;
	} while ( rank >= 0 );
    }
    return( todo );
}

//
// Convention for value buffer
//
//	0:	ZERO
//	1:	TRUE
//	2:	a[2].H
//	3:	a[2].L
//	4:	a[1].H
//	5:	a[1].L
//	6:	a[0].H
//	7:	a[0].L
//	8:	b[2].H
//	9:	b[2].L
//	...
//

#define INP_H(i) (*(gbv_inps+2*(i)))
#define INP_L(i) (*(gbv_inps+2*(i)+1))

#define OUT_H(i) (*(gbv_outs+2*(i)))
#define OUT_L(i) (*(gbv_outs+2*(i)+1))

#define TMP_H(i) (*(gbv_tmps+2*(i)))
#define TMP_L(i) (*(gbv_tmps+2*(i)+1))

//
static int
do_wl_op(ste_ptr ste, ncomp_ptr op)
{
    gbv one = c_ONE;
    resize_buf(&inps_buf, 0);
    FOREACH_NODE(idx, op->inps) {
	gbv H = *(cur_buf+2*idx);
	push_buf(&inps_buf, (pointer) &H);
	gbv L = *(cur_buf+2*idx+1);
	push_buf(&inps_buf, (pointer) &L);
    }
    gbv_inps = START_BUF(&inps_buf);
    //
    resize_buf(&outs_buf, 0);
    FOREACH_NODE(idx, op->outs) {
	push_buf(&outs_buf, (pointer) &one);
	push_buf(&outs_buf, (pointer) &one);
    }
    gbv_outs = START_BUF(&outs_buf);
    //
    // Perform the operation
    if( BDD_size_limit >= 0 && !op->no_weakening ) {
	    // Use dynamic weakening
	    gbv (*old_c_AND)(gbv a, gbv b);
	    gbv (*old_c_OR)(gbv a, gbv b);
	    old_c_AND = c_AND;
	    old_c_OR  = c_OR;
	    c_AND = BDD_c_limited_AND;
	    c_OR = BDD_c_limited_OR;
	    op->op(op);
	    c_AND     = old_c_AND;
	    c_OR      = old_c_OR;
    } else {
	op->op(op);
    }

    int additional = 0;
    // Now process the outputs (fanouts, phase delays, etc.)
    if( op->phase_delay ) {
	int i = 0;
	FOREACH_NODE(idx, op->outs) {
	    gbv newH = *(gbv_outs+i); i++;
	    *(next_buf+2*idx) = newH;
	    gbv newL = *(gbv_outs+i); i++;
	    *(next_buf+2*idx+1) = newL;
	    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
	    np->has_phase_event = TRUE;
	    additional++;
	}
    } else {
	int i = 0;
	FOREACH_NODE(idx, op->outs) {
	    gbv newH = *(gbv_outs+i); i++;
	    gbv newL = *(gbv_outs+i); i++;
	    int cnt = update_node(ste, idx, newH, newL);
	    additional += cnt;
	}
    }
    if( Do_gc_asap ) Garbage_collect();
    return additional;
}


static void
op_X(ncomp_ptr op)
{
    for(unint i = 0; i < op->size; i++) {
	OUT_H(i) = c_ONE;
	OUT_L(i) = c_ONE;
    }
}

static void
op_CONST(ncomp_ptr op)
{
    arbi_T v = op->arg.value;
    arbi_T ptr = Arbi_FromInt(1);
    arbi_T zero = Arbi_FromInt(0);
    arbi_T two = Arbi_FromInt(2);
    FROM_LSB_TO_MSB(op->size, i) {
	if( Arbi_cmp(Arbi_bvAND(v, ptr), zero) != arbi_EQ ) {
	    // 1
	    OUT_H(i) = c_ONE;
	    OUT_L(i) = c_ZERO;
	} else {
	    // 0
	    OUT_H(i) = c_ZERO;
	    OUT_L(i) = c_ONE;
	}
	ptr = Arbi_mlt(ptr, two);
    }
}

static void
op_VAR(ncomp_ptr op)
{
    FROM_MSB_TO_LSB(op->size,i) {
	OUT_H(i) = INP_H(i);
	OUT_L(i) = INP_L(i);
    }
}

static void
op_LAT_LEQ(ncomp_ptr op)
{
    FROM_MSB_TO_LSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	// NOTE: The only operation that is not monotonic!!!!
	// Should only be used in verification checkers!
	OUT_H(i) = c_AND(c_OR(bH, c_NOT(aH)), c_OR(bL, c_NOT(aL)));
	OUT_L(i) = c_NOT(OUT_H(i));
    }
}

static void
op_AND(ncomp_ptr op)
{
    FROM_MSB_TO_LSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	OUT_H(i) = c_AND(aH, bH);
	OUT_L(i) = c_OR(aL, bL);
    }
}

static void
op_IDELAY(ncomp_ptr op)
{
    int sz = op->size;
    ui mr = op->arg.del.min_rise;
    ui Mr = op->arg.del.max_rise;
    ui mf = op->arg.del.min_fall;
    ui Mf = op->arg.del.max_fall;
    FROM_MSB_TO_LSB(op->size,i) {
	// Process the H computation
	// 0->1 needs mr
	// 1->0 needs Mf
	gbv to1 = c_ONE;
	for(ui idx = 0; idx < mr; idx++) {
	    to1 = c_AND(to1, INP_H((1+idx)*sz+i));
	}
	gbv to0 = c_ZERO;
	for(ui idx = 0; idx < Mf; idx++) {
	    to0 = c_OR(to0, INP_H((1+idx)*sz+i));
	}
	gbv cur = INP_H(i);
	OUT_H(i) = c_OR(to1, c_AND(cur, to0));
	// Process the L computation
	// 0->1 needs mf
	// 1->0 needs MR
	to1 = c_ONE;
	for(ui idx = 0; idx < mf; idx++) {
	    to1 = c_AND(to1, INP_L((1+idx)*sz+i));
	}
	to0 = c_ZERO;
	for(ui idx = 0; idx < Mr; idx++) {
	    to0 = c_OR(to0, INP_L((1+idx)*sz+i));
	}
	cur = INP_L(i);
	OUT_L(i) = c_OR(to1, c_AND(cur, to0));
    }
}

static void
op_OR(ncomp_ptr op)
{
    FROM_MSB_TO_LSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	OUT_H(i) = c_OR(aH, bH);
	OUT_L(i) = c_AND(aL, bL);
    }
}

static void
op_NOT(ncomp_ptr op)
{
    FROM_MSB_TO_LSB(op->size,i) {
	OUT_H(i) = INP_L(i);
	OUT_L(i) = INP_H(i);
    }
}

static void
op_EQ(ncomp_ptr op)
{
    gbv eqH = c_ONE;
    gbv eqL = c_ZERO;
    unint sz = op->arg.extension_size;
    FROM_MSB_TO_LSB(sz,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	gbv bH = INP_H(i+sz);
	gbv bL = INP_L(i+sz);
	eqH = c_AND(c_OR(c_AND(aH,bH),c_AND(aL,bL)),eqH);
	eqL = c_OR(c_AND(c_OR(aL,bL),c_OR(aH,bH)),eqL);
    }
    OUT_H(0) = eqH;
    OUT_L(0) = eqL;
}

static void
op_GR(ncomp_ptr op)
{

    gbv grH = c_ZERO;
    gbv grL = c_ONE;
    unint sz = op->arg.extension_size;
    FROM_LSB_TO_MSB(sz,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	gbv bH = INP_H(i+sz);
	gbv bL = INP_L(i+sz);
	grH = c_OR(c_AND(aH,bL),c_AND(c_OR(aH,bL),grH));
	grL = c_AND(c_OR(aL,bH),c_OR(c_AND(aL,bH),grL));
    }
    OUT_H(0) = grH;
    OUT_L(0) = grL;
}

static void
op_ADD(ncomp_ptr op)
{
    gbv sumH;
    gbv sumL;
    gbv cinH = c_ZERO;
    gbv cinL = c_ONE;
    FROM_LSB_TO_MSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	sumH = c_OR(
		c_AND(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL),
		c_AND(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH));
	sumL = c_AND(
		c_OR(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH),
		c_OR(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL));
	cinH = c_OR(c_OR(c_AND(aH,bH),c_AND(aH,cinH)),
		      c_AND(bH,cinH));
	cinL = c_AND(c_AND(c_OR(aL,bL),c_OR(aL,cinL)),
		       c_OR(bL,cinL));
	OUT_H(i) = sumH;
	OUT_L(i) = sumL;
    }
}

static void
op_SUB(ncomp_ptr op)
{
    gbv difH;
    gbv difL;
    // Carry in = 1
    gbv cinH = c_ONE;
    gbv cinL = c_ZERO;
    FROM_LSB_TO_MSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	// One's complement of input 2
	gbv bH = INP_L(i+op->size);
	gbv bL = INP_H(i+op->size);
	// Otherwise standard addition
        difH = c_OR(
                c_AND(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL),
                c_AND(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH));
        difL = c_AND(
                c_OR(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH),
                c_OR(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL));
        cinH = c_OR(c_OR(c_AND(aH,bH),c_AND(aH,cinH)),
                      c_AND(bH,cinH));
        cinL = c_AND(c_AND(c_OR(aL,bL),c_OR(aL,cinL)),
                       c_OR(bL,cinL));
	OUT_H(i) = difH;
	OUT_L(i) = difL;
    }
}

static void
op_MUL(ncomp_ptr op)
{
    FROM_MSB_TO_LSB(op->size,i) {
	OUT_H(i) = c_ZERO;
	OUT_L(i) = c_ONE;
    }
    FROM_LSB_TO_MSB(op->size,i) {
	int shift = op->size-i-1;
	gbv biH = INP_H(i+op->size);
	gbv biL = INP_L(i+op->size);
	gbv cinH = c_ZERO;
	gbv cinL = c_ONE;
	FROM_LSB_TO_MSB(op->size,j) {
	    gbv aH, aL, bH, bL, sumH, sumL;
	    int pos = op->size-j-1;
	    if( pos < shift ) {
		aH = c_ZERO;
		aL = c_ONE;
	    } else {
		aH = c_AND(biH, INP_H(j+shift));
		aL =  c_OR(biL, INP_L(j+shift));
	    }
	    bH = OUT_H(j);
	    bL = OUT_L(j);
	    sumH = c_OR(
		    c_AND(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL),
		    c_AND(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH));
	    sumL = c_AND(
		    c_OR(c_AND(c_OR(aL,bH),c_OR(aH,bL)),cinH),
		    c_OR(c_OR(c_AND(aH,bL),c_AND(aL,bH)),cinL));
	    cinH = c_OR(c_OR(c_AND(aH,bH),c_AND(aH,cinH)),
			  c_AND(bH,cinH));
	    cinL = c_AND(c_AND(c_OR(aL,bL),c_OR(aL,cinL)),
			   c_OR(bL,cinL));
	    OUT_H(j) = sumH;
	    OUT_L(j) = sumL;
	}
    }
}

static void
op_ITE(ncomp_ptr op)
{
    gbv cH = INP_H(0);
    gbv cL = INP_L(0);
    FROM_MSB_TO_LSB(op->size,i) {
	gbv aH = INP_H(1+i);
	gbv aL = INP_L(1+i);
	gbv bH = INP_H(1+i+op->size);
	gbv bL = INP_L(1+i+op->size);
	if( RCaccurate_ite_comp ) {
	    OUT_H(i) = c_OR(c_OR(c_AND(aH,bH),c_AND(cH,aH)),
			      c_AND(cL,bH));
	    OUT_L(i) = c_AND(c_AND(c_OR(aL,bL),c_OR(cL,aL)),
			       c_OR(cH,bL));
	} else {
	    OUT_H(i) = c_OR(c_AND(cH,aH), c_AND(cL,bH));
	    OUT_L(i) = c_AND(c_OR(cL,aL), c_OR(cH,bL));
	}
    }
}

static void
reset_tmps(int cnt)
{
    resize_buf(&tmps_buf, 2*cnt);
    gbv_tmps = FAST_LOC_BUF(&tmps_buf, 0);
    for(int i = 0; i < 2*cnt; i++) {
	*(gbv_tmps+i) = c_ONE;
    }
}

static void
dbg_info(string msg, int size, gbv *res)
{
    FP(err_fp, "%s: ", msg);
    for(int i = 0; i < size; i++) {
	gbv H = *(res+2*i);
	gbv L = *(res+2*i+1);
	cHL_Print(err_fp, H, L);
    }
    FP(err_fp, "\n");
}

static void
op_DIV(ncomp_ptr op)
{
    reset_tmps(4*op->size+1);
    gbv *bp   = &(INP_H(op->size));
    gbv *Q    = &(TMP_H(0));
    gbv *R    = &(TMP_H(op->size));
    gbv *R2 = &(TMP_H(2*op->size));
    gbv *sub = &(TMP_H(3*op->size));
    gbv *cond = &(TMP_H(4*op->size));
    FROM_MSB_TO_LSB(op->size,i) {
	*(Q+2*i) = c_ZERO;
	*(Q+2*i+1) = c_ONE;
	*(R+2*i) = c_ZERO;
	*(R+2*i+1) = c_ONE;
    }
    FROM_MSB_TO_LSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	shift_left_by_1(op->size, R, aH, aL, R2);
	sub_fn(op->size, R2, bp, sub);
	geq_fn(op->size, R2, bp, cond);
	ITE_fn(op->size, *cond, *(cond+1), sub, R2, R);
	shift_left_by_1(op->size, Q, *cond, *(cond+1), Q);
    }
    FROM_MSB_TO_LSB(op->size,i) {
	OUT_H(i) = *(Q+2*i);
	OUT_L(i) = *(Q+2*i+1);
    }
    reset_tmps(0);
}

static void
op_MOD(ncomp_ptr op)
{
    reset_tmps(4*op->size+1);
    gbv *bp   = &(INP_H(op->size));
    gbv *Q    = &(TMP_H(0));
    gbv *R    = &(TMP_H(op->size));
    gbv *R2 = &(TMP_H(2*op->size));
    gbv *sub = &(TMP_H(3*op->size));
    gbv *cond = &(TMP_H(4*op->size));
    FROM_MSB_TO_LSB(op->size,i) {
	*(Q+2*i) = c_ZERO;
	*(Q+2*i+1) = c_ONE;
	*(R+2*i) = c_ZERO;
	*(R+2*i+1) = c_ONE;
    }
    FROM_MSB_TO_LSB(op->size,i) {
	gbv aH = INP_H(i);
	gbv aL = INP_L(i);
	shift_left_by_1(op->size, R, aH, aL, R2);
	sub_fn(op->size, R2, bp, sub);
	geq_fn(op->size, R2, bp, cond);
	ITE_fn(op->size, *cond, *(cond+1), sub, R2, R);
	shift_left_by_1(op->size, Q, *cond, *(cond+1), Q);
    }
    FROM_MSB_TO_LSB(op->size,i) {
	OUT_H(i) = *(R+2*i);
	OUT_L(i) = *(R+2*i+1);
    }
    reset_tmps(0);
}

static void
op_SHL(ncomp_ptr op)
{
    reset_tmps(2*op->size);
    gbv *tmp  = &(TMP_H(0));
    gbv *cur  = &(TMP_H(op->size));
    FROM_MSB_TO_LSB(op->size, i) {
	*(cur+2*i)   = INP_H(i);
	*(cur+2*i+1) = INP_L(i);
    }
    unint shift = 1;
    FROM_LSB_TO_MSB(op->size, i) {
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	sshl_fun(op->size, cur, shift, tmp);
	ITE_fn(op->size, bH, bL, tmp, cur, cur);
	if( shift >= op->size ) break;
	shift *= 2;
    }
    FROM_LSB_TO_MSB(op->size, i) {
	OUT_H(i) = *(cur+2*i);
	OUT_L(i) = *(cur+2*i+1);
    }
    reset_tmps(0);
}

static void
op_SHR(ncomp_ptr op)
{
    reset_tmps(2*op->size);
    gbv *tmp  = &(TMP_H(0));
    gbv *cur  = &(TMP_H(op->size));
    FROM_MSB_TO_LSB(op->size, i) {
	*(cur+2*i)   = INP_H(i);
	*(cur+2*i+1) = INP_L(i);
    }
    unint shift = 1;
    FROM_LSB_TO_MSB(op->size, i) {
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	sshr_fun(op->size, cur, shift, tmp);
	ITE_fn(op->size, bH, bL, tmp, cur, cur);
	if( shift >= op->size ) break;
	shift *= 2;
    }
    FROM_LSB_TO_MSB(op->size, i) {
	OUT_H(i) = *(cur+2*i);
	OUT_L(i) = *(cur+2*i+1);
    }
    reset_tmps(0);
}

static void
op_ASHR(ncomp_ptr op)
{
    reset_tmps(2*op->size);
    gbv *tmp  = &(TMP_H(0));
    gbv *cur  = &(TMP_H(op->size));
    FROM_MSB_TO_LSB(op->size, i) {
	*(cur+2*i)   = INP_H(i);
	*(cur+2*i+1) = INP_L(i);
    }
    unint shift = 1;
    // Not very efficient....
    // If the shift amount is >= #bits, we could be more efficient.
    FROM_LSB_TO_MSB(op->size, i) {
	gbv bH = INP_H(i+op->size);
	gbv bL = INP_L(i+op->size);
	sashr_fun(op->size, cur, shift, tmp);
	ITE_fn(op->size, bH, bL, tmp, cur, cur);
	shift *= 2;
    }
    FROM_LSB_TO_MSB(op->size, i) {
	OUT_H(i) = *(cur+2*i);
	OUT_L(i) = *(cur+2*i+1);
    }
    reset_tmps(0);
}

static void
op_SX(ncomp_ptr op)
{
    int isz = op->arg.extension_size;
    gbv msbH = INP_H(0);
    gbv msbL = INP_L(0);
    int cnt = isz-1;
    FROM_LSB_TO_MSB(op->size, i) {
	if( cnt >= 0) {
	    OUT_H(i) = INP_H(cnt);
	    OUT_L(i) = INP_L(cnt);
	} else {
	    OUT_H(i) = msbH;
	    OUT_L(i) = msbL;
	}
	cnt--;
    }
}

static void
op_ZX(ncomp_ptr op)
{
    int isz = op->arg.extension_size;
    int cnt = isz-1;
    FROM_LSB_TO_MSB(op->size, i) {
	if( cnt >= 0) {
	    OUT_H(i) = INP_H(cnt);
	    OUT_L(i) = INP_L(cnt);
	} else {
	    OUT_H(i) = c_ZERO;
	    OUT_L(i) = c_ONE;
	}
	cnt--;
    }
}

static void
op_SLICE(ncomp_ptr op)
{
    idx_list_ptr il = op->arg.idx_list;
    FROM_MSB_TO_LSB(op->size, i) {
	int ti = il->idx;
	OUT_H(i) = INP_H(ti);
	OUT_L(i) = INP_L(ti);
	il = il->next;
    }
}

static void
op_UPDATE_SLICE(ncomp_ptr op)
{
    // First the default values
    FROM_MSB_TO_LSB(op->size, i) {
	OUT_H(i) = INP_H(i);
	OUT_L(i) = INP_L(i);
    }
    // Then replace selected bits
    idx_list_ptr il = op->arg.idx_list;
    int idx = 0;
    while( il != NULL ) {
	int ti = il->idx;
	OUT_H(ti) = INP_H(idx+op->size);
	OUT_L(ti) = INP_L(idx+op->size);
	il = il->next;
	idx++;
    }
}

static void
op_WIRE(ncomp_ptr op)
{
    FROM_LSB_TO_MSB(op->size, i) {
	OUT_H(i) = INP_H(i);
	OUT_L(i) = INP_L(i);
    }
}

static void
op_MEM_READ(ncomp_ptr op)
{
    // Inputs:  addr memory
    //
    int asz = op->arg.mem.addr_size;
    int dsz = op->arg.mem.data_size;
    int lines = op->arg.mem.lines;
    //
    FROM_LSB_TO_MSB(op->size, i) {
	OUT_H(i) = c_ONE;
	OUT_L(i) = c_ONE;
    }
    for(int line = 0; line < lines; line++) {
	gbv selH;
	gbv selL;
	selH = c_ONE;
	selL = c_ZERO;
	int l = line;
	FROM_LSB_TO_MSB(asz, bit) {
	    if( (l % 2) == 1 ) {
		// 1
		selH = c_AND(selH, INP_H(bit));
		selL = c_OR(selL, INP_L(bit));
	    } else {
		// 0
		selH = c_AND(selH, INP_L(bit));
		selL = c_OR(selL, INP_H(bit));
	    }
	    l = l/2;
	}
	FROM_LSB_TO_MSB(dsz, i) {
	    int base = (lines-line-1)*dsz;
	    gbv newH = INP_H(asz+base+i);
	    gbv newL = INP_L(asz+base+i);
	    gbv oldH = OUT_H(i);
	    gbv oldL = OUT_L(i);
	    gbv resH = c_OR(c_OR(c_AND(newH,oldH),c_AND(selH,newH)),
			    c_AND(selL,oldH));
	    gbv resL = c_AND(c_AND(c_OR(newL,oldL),c_OR(selL,newL)),
			     c_OR(selH,oldL));
	    OUT_H(i) = resH;
	    OUT_L(i) = resL;
	}
    }
}

static void
op_MEM_WRITE(ncomp_ptr op)
{
    // Inputs:  addr data memory
    //
    int asz = op->arg.mem.addr_size;
    int dsz = op->arg.mem.data_size;
    int lines = op->arg.mem.lines;
    //
    for(int line = 0; line < lines; line++) {
	gbv selH;
	gbv selL;
	selH = c_ONE;
	selL = c_ZERO;
	int l = line;
	FROM_LSB_TO_MSB(asz, bit) {
	    if( (l % 2) == 1 ) {
		// 1
		selH = c_AND(selH, INP_H(bit));
		selL = c_OR(selL, INP_L(bit));
	    } else {
		// 0
		selH = c_AND(selH, INP_L(bit));
		selL = c_OR(selL, INP_H(bit));
	    }
	    l = l/2;
	}
	FROM_LSB_TO_MSB(dsz, i) {
	    int base = (lines-line-1)*dsz;
	    gbv oldH = INP_H(asz+dsz+base+i);
	    gbv oldL = INP_L(asz+dsz+base+i);
	    gbv newH = INP_H(asz+i);
	    gbv newL = INP_L(asz+i);
	    gbv resH = c_OR(c_OR(c_AND(newH,oldH),c_AND(selH,newH)),
			    c_AND(selL,oldH));
	    gbv resL = c_AND(c_AND(c_OR(newL,oldL),c_OR(selL,newL)),
			     c_OR(selH,oldL));
	    OUT_H(base+i) = resH;
	    OUT_L(base+i) = resL;
	}
    }
}

static bool
traverse_pexlif(hash_record *parent_tblp, g_ptr p, string hier,
                bool top_level, int draw_level, int inst_cnt, int max_depth)
{
    hash_record vinfo_tbl;
    create_hash(&vinfo_tbl, 100, str_hash, str_equ);
    g_ptr attrs, fa_inps, fa_outs, internals, content;
    string name;
    bool leaf;
    if( !is_PINST(p,&name,&attrs,&leaf,&fa_inps,&fa_outs,&internals,&content)) {
        Fail_pr("Not a PINST where expected");
        return FALSE;
    }
    attrs = Make_CONS_ND(
		 Make_CONS_ND(Make_STRING_leaf(wastrsave(stringsp,"module")),
			      Make_STRING_leaf(name)), attrs);
    push_buf(&attr_buf, &attrs);
    // Declare new nodes (internal)
    for(g_ptr l = internals; !IS_NIL(l); l = GET_CONS_TL(l)) {
        string name = GET_STRING(GET_CONS_HD(l));
	if( *name != '!' && strstr(name, "assert__") != NULL ) {
	    string prop_name = strtemp(hier);
	    prop_name = wastrsave(stringsp, strappend(name));
	    push_buf(propsp, &prop_name);
	}
        string value_list = find_value_list(attrs, name);
        declare_vector(&vinfo_tbl, hier, name, FALSE, NULL, value_list);
    }
    vis_ptr vp = NULL;
    if( !top_level && 
	(RCaccurate_hierachy_visualization ||
	 (leaf || (strstr(name,"draw_") != NULL))))
    {
        vp = (vis_ptr) new_rec(vis_rec_mgrp);
        vp->draw_level = draw_level;
        vp->attrs = NULL;
	attr_list_ptr  a_tail = NULL;
	while( !IS_NIL(attrs) ) {
	    attr_list_ptr ap = new_rec(attr_list_rec_mgrp);
	    ap->name = GET_STRING(GET_FST(GET_CONS_HD(attrs)));
	    ap->value = GET_STRING(GET_SND(GET_CONS_HD(attrs)));
	    ap->next = NULL;
	    if( a_tail == NULL ) {
		vp->attrs = ap;
	    } else {
		a_tail->next = ap;
	    }
	    a_tail = ap;
	    attrs = GET_CONS_TL(attrs);
	}
        draw_level++;
        vp->id = ++visualization_id;
        vp->pinst_cnt = inst_cnt;
	vp->pfn = name;
        vp->fa_inps = NULL;
        vp->fa_outs = NULL;
    }
    int nbr_inputs = 0;
    if( top_level ) {
        // Declare new nodes top-level inputs and outputs
        for(g_ptr l = fa_inps; !IS_NIL(l); l = GET_CONS_TL(l)) {
            nbr_inputs++;
            g_ptr pair = GET_CONS_HD(l);
            string fname = GET_STRING(GET_FST(pair));
            if( RC_print_warnings_toplevel_fa_mismatch &&
		(List_length(GET_SND(pair)) != 1 ||
                 !STREQ(fname, GET_STRING(GET_CONS_HD(GET_SND(pair))))) )
	    {
                FP(warning_fp,
                   "Actual != formal (%s) for top-level pexlif input. ", fname);
                FP(warning_fp, "Actual ignored\n");
            }
            push_buf(top_inpsp, &fname);
            string value_list = find_value_list(attrs, fname);
            ilist_ptr il = declare_vector(&vinfo_tbl, hier, fname,
                                          FALSE, NULL, value_list);
            FOREACH_NODE(nd, il) {
                nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
                np->is_top_input = TRUE;
            }
        }
        for(g_ptr l = fa_outs; !IS_NIL(l); l = GET_CONS_TL(l)) {
            g_ptr pair = GET_CONS_HD(l);
            string fname = GET_STRING(GET_FST(pair));
            if( RC_print_warnings_toplevel_fa_mismatch &&
		(List_length(GET_SND(pair)) != 1 ||
                 !STREQ(fname, GET_STRING(GET_CONS_HD(GET_SND(pair))))) )
	    {
                FP(warning_fp,
                   "Actual != formal (%s) for top-level pexlif output. ",fname);
                FP(warning_fp, "Actual ignored\n");
            }
            push_buf(top_outsp, &fname);
            string value_list = find_value_list(attrs, fname);
            ilist_ptr il = declare_vector(&vinfo_tbl, hier, fname,
                                          FALSE, NULL, value_list);
            FOREACH_NODE(nd, il) {
                nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
                np->is_top_output = TRUE;
            }
        }
    } else {
        // Declare inputs using the parent table to get the actuals mapping
        for(g_ptr l = fa_inps; !IS_NIL(l); l = GET_CONS_TL(l)) {
            nbr_inputs++;
            g_ptr pair = GET_CONS_HD(l);
            string fname = GET_STRING(GET_FST(pair));
            g_ptr acts = GET_SND(pair);
            ilist_ptr act_list = NULL;
            while( !IS_NIL(acts) ) {
                string actual = GET_STRING(GET_CONS_HD(acts));
                ilist_ptr tmp = map_vector(parent_tblp, hier, actual, FALSE);
                if( tmp == NULL ) {
                    string phier = strtemp(hier);
                    string last = rindex(phier, '/');
                    if( last != NULL ) { *last = 0; }
                    phier = wastrsave(stringsp, phier);
                    Wprintf("Signal %s not declared in %s.", actual, phier);
                    tmp = declare_vector(&vinfo_tbl, hier, actual,
                                         FALSE, NULL, s_ES);
                }
                act_list = ilist_append(act_list, tmp);
                acts = GET_CONS_TL(acts);
            }
            declare_vector(&vinfo_tbl, hier, fname, TRUE, act_list, s_ES);
            if( vp != NULL ) {
                vis_io_ptr vio = (vis_io_ptr) new_rec(vis_io_rec_mgrp);
                vio->f_vec = fname;
                vio->acts = act_list;
                vio->next = NULL;
                if( vp->fa_inps == NULL ) {
                    vp->fa_inps = vio;
                } else {
                    // Quadratic code, but number of inputs is quite small for
                    // draw_ objects
                    vis_io_ptr rp = vp->fa_inps;
                    while( rp->next != NULL ) rp = rp->next;	
                    rp->next = vio;
                }
            }
        }
        // Declare outputs using the parent table to get the actuals mapping
        for(g_ptr l = fa_outs; !IS_NIL(l); l = GET_CONS_TL(l)) {
            g_ptr pair = GET_CONS_HD(l);
            string fname = GET_STRING(GET_FST(pair));
            g_ptr acts = GET_SND(pair);
            ilist_ptr act_list = NULL;
            while( !IS_NIL(acts) ) {
                string actual = GET_STRING(GET_CONS_HD(acts));
#if 0
                ilist_ptr tmp = map_vector(parent_tblp, hier, actual, TRUE);
#else
                ilist_ptr tmp = map_vector(parent_tblp, hier, actual, FALSE);
#endif
                if( tmp == NULL ) {
                    string phier = strtemp(hier);
                    string last = rindex(phier, '/');
                    if( last != NULL ) { *last = '0'; }
                    phier = wastrsave(stringsp, phier);
                    Wprintf("Signal %s not declared in %s", actual, phier);
                    tmp = declare_vector(&vinfo_tbl, hier, actual,
                                         FALSE, NULL, s_ES);
                }
                act_list = ilist_append(act_list, tmp);
                acts = GET_CONS_TL(acts);
            }
            declare_vector(&vinfo_tbl, hier, fname, TRUE, act_list, s_ES);
            if( vp != NULL ) {
                vis_io_ptr vio = (vis_io_ptr) new_rec(vis_io_rec_mgrp);
                vio->f_vec = fname;
                vio->acts = act_list;
                vio->next = NULL;
                if( vp->fa_outs == NULL ) {
                    vp->fa_outs = vio;
                } else {
                    // Quadratic code, but number of inputs is quite small for
                    // draw_ objects
                    vis_io_ptr rp = vp->fa_outs;
                    while( rp->next != NULL ) rp = rp->next;	
                    rp->next = vio;
                }
                FOREACH_NODE(nd, act_list) {
                    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
                    vis_list_ptr vlp = new_rec(vis_list_rec_mgrp);
                    vlp->vp = vp;
                    vlp->next = np->draw_info;
                    np->draw_info = vlp;
                }
            }
        }
    }

    if( vp != NULL ) {
        string match, pfn;
	tstr_ptr sm = new_temp_str_mgr();
        if( (match = strstr(vp->pfn, "draw_hfl ")) != NULL ) {
            // Replace draw_hfl txt with draw_hfl_code nbr_inps inst txt
            int len = match-vp->pfn+8;
            string res = strtemp(vp->pfn);
            *(res+len) = 0;
	    if( (strlen(res)+20+strlen(res+len+1)) > 4000 ) {
		sprintf(buf, "%s_code %d %d {...}", res, inst_cnt, nbr_inputs);
	    } else {
		sprintf(buf, "%s_code %d %d %s",
			     res, nbr_inputs, inst_cnt, res+len+1);
	    }
            pfn = gen_strappend(sm, buf);
        } else if( (match = strstr(vp->pfn, "draw_hier ")) != NULL ) {
            // Replace draw_hier txt with draw_fub txt inst inputs outputs
            int len = match-vp->pfn;
            gen_strtemp(sm, vp->pfn);
	    gen_strtruncate(sm, len);
            gen_strappend(sm, "draw_fub ");
	    gen_strappend(sm, match+10);
            string iname = find_instance_name(attrs);
            if( iname == s_no_instance ) {
                gen_strappend(sm, " ");
                gen_strappend(sm, hier);
                gen_strappend(sm, " ");
            } else {
                gen_strappend(sm, " {");
                gen_strappend(sm, hier);
                gen_strappend(sm, ": ");
                gen_strappend(sm, iname);
                gen_strappend(sm, "} ");
            }
	    gen_tappend(sm, " %d ", inst_cnt);
            gen_strappend(sm, "{ ");
            for(g_ptr l = fa_inps; !IS_NIL(l); l = GET_CONS_TL(l)) {
                g_ptr pair = GET_CONS_HD(l);
                string fname = GET_STRING(GET_FST(pair));
                gen_strappend(sm, "{{");
                gen_strappend(sm, fname);
                // Don't need the actuals for inputs
                gen_strappend(sm, "} {");
                gen_strappend(sm, "}} ");
            }
            gen_strappend(sm, "} { ");
            for(vis_io_ptr vlp = vp->fa_outs; vlp != NULL; vlp = vlp->next) {
                gen_strappend(sm, "{{");
                gen_strappend(sm, vlp->f_vec);
                gen_strappend(sm, "} {");
                g_ptr nds = ilist2nds(vlp->acts);
                g_ptr avecs = Merge_Vectors(nds, TRUE);
                for(g_ptr ap = avecs; !IS_NIL(ap); ap = GET_CONS_TL(ap)) {
                    string actual = GET_STRING(GET_CONS_HD(ap));
                    gen_strappend(sm, "{");
                    gen_strappend(sm, actual);
                    gen_strappend(sm, "} ");
                }
                DEC_REF_CNT(nds);
                DEC_REF_CNT(avecs);
                gen_strappend(sm, "}} ");
            }
            pfn = gen_strappend(sm, "}");
        } else if( (RCaccurate_hierachy_visualization &&
		   strncmp(vp->pfn, "draw_", 5) != 0 ))
	{
            // Replace txt with draw_fub txt inst inputs outputs
            gen_strtemp(sm, "draw_fub ");
	    gen_strappend(sm, vp->pfn);
	    gen_strappend(sm, " ");
	    gen_strappend(sm, hier);
	    gen_strappend(sm, " ");
	    gen_tappend(sm, " %d ", inst_cnt);
            gen_strappend(sm, "{ ");
            for(g_ptr l = fa_inps; !IS_NIL(l); l = GET_CONS_TL(l)) {
                g_ptr pair = GET_CONS_HD(l);
                string fname = GET_STRING(GET_FST(pair));
                gen_strappend(sm, "{{");
                gen_strappend(sm, fname);
                // Don't need the actuals for inputs
                gen_strappend(sm, "} {");
                gen_strappend(sm, "}} ");
            }
            gen_strappend(sm, "} { ");
            for(vis_io_ptr vlp = vp->fa_outs; vlp != NULL; vlp = vlp->next) {
                gen_strappend(sm, "{{");
                gen_strappend(sm, vlp->f_vec);
                gen_strappend(sm, "} {");
                g_ptr nds = ilist2nds(vlp->acts);
                g_ptr avecs = Merge_Vectors(nds, TRUE);
                for(g_ptr ap = avecs; !IS_NIL(ap); ap = GET_CONS_TL(ap)) {
                    string actual = GET_STRING(GET_CONS_HD(ap));
                    gen_strappend(sm, "{");
                    gen_strappend(sm, actual);
                    gen_strappend(sm, "} ");
                }
                DEC_REF_CNT(nds);
                DEC_REF_CNT(avecs);
                gen_strappend(sm, "}} ");
            }
            pfn = gen_strappend(sm, "}");
	} else {
            pfn = gen_strtemp(sm, vp->pfn);
	}
	// Finally prepend "add_inst <inst_nbr> " to pfn
	sprintf(buf, "add_inst %d ", inst_cnt);
	pfn = gen_strprepend(sm, buf);
	vp->pfn = wastrsave(stringsp, pfn);
	free_temp_str_mgr(sm);
    }

    g_ptr children, fns;
    if( is_P_HIER(content, &children) ) {
	// Only process children for non black-box instances
	if( max_depth != 0 && find_hash(&bb_tbl, ihier_buf) == NULL ) {
	    // Hierarchy
	    int inst = 1;
	    int len = strlen(ihier_buf);
	    char *ep = &(ihier_buf[len]);
	    for(g_ptr cl = children; !IS_NIL(cl); cl = GET_CONS_TL(cl)) {
		Sprintf(ep, "i%d/", inst++);
		if( !traverse_pexlif(&vinfo_tbl, GET_CONS_HD(cl), ihier_buf,
				     FALSE, draw_level, inst-1, max_depth-1)) {
		    return FALSE;
		}
	    }
	}
    } else if( is_P_LEAF(content, &fns) ) {
        // Leaf update function
        temporary_node_cnt = 0;
        for(g_ptr cl = fns; !IS_NIL(cl); cl = GET_CONS_TL(cl)) {
            g_ptr fn = GET_CONS_HD(cl);
            g_ptr lhs, rhs;
            if( is_W_UPDATE_FN(fn, &lhs, &rhs) ) {
                ilist_ptr lhs_indices = get_lhs_indices(&vinfo_tbl, hier, lhs);
                compile_expr(&vinfo_tbl, hier, lhs_indices, rhs, FALSE);
            } else if( is_W_PHASE_DELAY(fn, &lhs, &rhs) ) {
                ilist_ptr lhs_indices = get_lhs_indices(&vinfo_tbl, hier, lhs);
                compile_expr(&vinfo_tbl, hier, lhs_indices, rhs, TRUE);
            } else {
                DIE("Should not be possible!");
            }
        }
    } else {
        DIE("Should not be possible!");
    }
    dispose_hash(&vinfo_tbl, NULLFCN);
    pop_buf(&attr_buf, NULL);
    return TRUE;
}

static bool
no_fanout(ncomp_ptr cp)
{
    FOREACH_NODE(nd, cp->outs) {
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	if( np->fanouts != NULL )
	    return FALSE;
    }
    return TRUE;
}

static int
assign_rank(int depth, ncomp_ptr cp)
{
    if( depth > 1000 ) { return 1; }
    if( cp->rank != 0 ) return cp->rank;
    int my_rank = 1;
    FOREACH_NODE(nd, cp->outs) {
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	for(idx_list_ptr ip = np->fanouts; ip != NULL; ip = ip->next) {
	    ncomp_ptr fo = (ncomp_ptr) M_LOCATE_BUF(compositesp, ip->idx);
	    int fo_rank = assign_rank(depth+1, fo);
	    my_rank = (fo_rank>my_rank)? fo_rank : my_rank;
	}
    }
    my_rank++;
    if( my_rank > max_rank ) { max_rank = my_rank; }
    cp->rank = my_rank;
    return my_rank;
}

static int
rank_order()
{
    ncomp_ptr	cp;
    FOR_BUF(compositesp, ncomp_rec, cp) {
	if( cp->phase_delay || no_fanout(cp) ) {
	    cp->rank = 1;
	} else {
	    cp->rank = 0;
	}
    }
    max_rank = 1;
    FOR_BUF(compositesp, ncomp_rec, cp) {
	assign_rank(1, cp);
    }
    return( max_rank );
}

static void
bexpr_c_Print(odests fp, gbv a, int size)
{
    (void) size;
    bexpr f = a.bp;
    BE_Print(fp, f);
}

static void
BDD_c_Print(odests fp, gbv a, int size)
{
    formula f = a.f;
    B_Print(fp, f, size);
}

static gbv
bexpr_c_NOT(gbv a)
{
    bexpr f = a.bp;
    gbv res;
    res.bp = BE_Not(f);
    return( res );
}

static gbv
BDD_c_NOT(gbv a)
{
    formula f = a.f;
    gbv res;
    res.f = B_Not(f);
    return( res );
}

static gbv
bexpr_c_AND(gbv a, gbv b)
{
    bexpr f = a.bp;
    bexpr g = b.bp;
    gbv res;
    res.bp = BE_And(f,g);
    return( res );
}

static gbv
BDD_c_AND(gbv a, gbv b)
{
    formula f = a.f;
    formula g = b.f;
    gbv res;
    res.f = B_And(f,g);
    return( res );
}

static gbv
bexpr_c_OR(gbv a, gbv b)
{
    bexpr f = a.bp;
    bexpr g = b.bp;
    gbv res;
    res.bp = BE_Or(f,g);
    return( res );
}

static gbv
BDD_c_OR(gbv a, gbv b)
{
    formula f = a.f;
    formula g = b.f;
    gbv res;
    res.f = B_Or(f,g);
    return( res );
}

static gbv
BDD_c_limited_AND(gbv a, gbv b)
{
    formula f = a.f;
    formula g = b.f;
    gbv res;
    formula r = B_And(f,g);
    if( Get_bdd_size(r, BDD_size_limit) >= BDD_size_limit ) {
	if( information_flow_weakening ) {
	    int idx = COUNT_BUF(weakening_bufp);
	    push_buf(weakening_bufp, &r);
	    char nm[20];
	    sprintf(nm, "_%d", idx+1);
	    string name = wastrsave(stringsp, nm);
	    res.f = B_Var(name);
	} else {
	    // Dynamic weakening
	    res.f = B_One();
	}
    } else {
	res.f = r;
    }
    return( res );
}

static gbv
BDD_c_limited_OR(gbv a, gbv b)
{
    formula f = a.f;
    formula g = b.f;
    gbv res;
    formula r = B_Or(f,g);
    if( Get_bdd_size(r, BDD_size_limit) >= BDD_size_limit ) {
	if( information_flow_weakening ) {
	    int idx = COUNT_BUF(weakening_bufp);
	    push_buf(weakening_bufp, &r);
	    char nm[20];
	    sprintf(nm, "_%d", idx+1);
	    string name = wastrsave(stringsp, nm);
	    res.f = B_Var(name);
	} else {
	    // Dynamic weakening
	    res.f = B_One();
	}
    } else {
	res.f = r;
    }
    return( res );
}

static bool
bexpr_c_isT(gbv H, gbv L)
{
    bexpr h = H.bp;
    bexpr l = L.bp;
    return( (h == BE_One()) && (l == BE_Zero()) );
}

static bool
bexpr_c_isX(gbv H, gbv L)
{
    bexpr h = H.bp;
    bexpr l = L.bp;
    return( (h == BE_One()) && (l == BE_One()) );
}

static bool
BDD_c_isT(gbv H, gbv L)
{
    formula h = H.f;
    formula l = L.f;
    return( (h == B_One()) && (l == B_Zero()) );
}

static bool
BDD_c_isX(gbv H, gbv L)
{
    formula h = H.f;
    formula l = L.f;
    return( (h == B_One()) && (l == B_One()) );
}

static bool
bexpr_c_NEQ(gbv a, gbv b)
{
    bexpr f = a.bp;
    bexpr g = b.bp;
    return( BE_NEQ(f, g) );
}

static bool
BDD_c_NEQ(gbv a, gbv b)
{
    return( a.f != b.f );
}

static bool
c_EQ(gbv a, gbv b)
{
    return( !c_NEQ(a, b) );
}

static void
switch_to_bexprs()
{
    cHL_Print = bexpr_cHL_Print;
    c_Print = bexpr_c_Print;
    c_NOT = bexpr_c_NOT;
    c_AND = bexpr_c_AND;
    c_OR = bexpr_c_OR;
    c_NEQ = bexpr_c_NEQ;
    c_ZERO.bp = BE_Zero();
    c_ONE.bp  = BE_One();
    c_isT = bexpr_c_isT;
    c_isX = bexpr_c_isX;
}

static void
switch_to_ints()
{
    DIE("Not implemented yet!");
}

static void
BDD_cHL_Print(odests fp, gbv H, gbv L)
{
    formula Hb = H.f;
    formula Lb = L.f;
    HL_Print(fp, Hb, Lb);
}

static void
bexpr_cHL_Print(odests fp, gbv H, gbv L)
{
    bexpr Hb = H.bp;
    bexpr Lb = L.bp;
    BE_HL_Print(fp, Hb, Lb);
}

static void
switch_to_BDDs()
{
    cHL_Print = BDD_cHL_Print;
    c_Print = BDD_c_Print;
    c_NOT = BDD_c_NOT;
    c_AND = BDD_c_AND;
    c_OR = BDD_c_OR;
    c_NEQ = BDD_c_NEQ;
    c_ZERO.f = B_Zero();
    c_ONE.f  = B_One();
    c_isT = BDD_c_isT;
    c_isX = BDD_c_isX;
}

static int
update_node(ste_ptr ste, int idx, gbv Hnew, gbv Lnew)
{
    int todo = 0;
    gbv Hcur = *(cur_buf+2*idx);
    gbv Lcur = *(cur_buf+2*idx+1);
    nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, idx);
    if( np->has_weak ) {
	gbv Hw = *(weak_buf+2*idx);
	gbv Lw = *(weak_buf+2*idx+1);
	Hnew = c_OR(Hnew, Hw); 
	Lnew = c_OR(Lnew, Lw); 
    }
    if( np->has_ant ) {
	gbv aH = *(ant_buf+2*idx);
	gbv aL = *(ant_buf+2*idx+1);
	if( RCnotify_traj_failures &&
	    (nbr_errors_reported < RCmax_nbr_errors) )
	{
	    gbv abort = c_NOT(c_OR(c_AND(aH, Hnew), c_AND(aL, Lnew)));
	    if( c_NEQ(abort, c_ZERO) ) {
		FP(warning_fp,
		  "Warning: Antecedent failure at time %d on node %s\n",
		  ste->cur_time, idx2name(np->idx));
		if( print_failures ) {
		    FP(warning_fp, "Current value:");
		    cHL_Print(warning_fp, Hnew, Lnew);
		    FP(warning_fp, "\nAsserted value:");
		    cHL_Print(warning_fp, aH, aL);
		    FP(warning_fp, "\n\n");
		}
		nbr_errors_reported++;
		if( ste->abort_ASAP ) {
		    Hnew = c_AND(Hnew, aH); 
		    Lnew = c_AND(Lnew, aL); 
		    gbv v = ste->validTrajectory;
		    v = c_AND(v, c_OR(Hnew, Lnew));
		    ste->validTrajectory = v;
		    ste->max_time = ste->cur_time;
		    ste->active = FALSE;
		    if( gui_mode ) {
			Sprintf(buf, "simulation_end");
			Info_to_tcl(buf);
		    }
		    pop_fsm();
		    kill(getpid(), SIGINT);
		    return 0;
		}
	    }
	}
	Hnew = c_AND(Hnew, aH); 
	Lnew = c_AND(Lnew, aL); 
	gbv v = ste->validTrajectory;
	v = c_AND(v, c_OR(Hnew, Lnew));
	ste->validTrajectory = v;
    }
    *(cur_buf+2*idx) = Hnew;
    *(cur_buf+2*idx+1) = Lnew;
    if( c_NEQ(Hnew,Hcur) || c_NEQ(Lnew,Lcur) ) {
//FP(err_fp, "----> Node %s changed at time %d to ", idx2name(np->idx), ste->cur_time); cHL_Print(err_fp, Hnew, Lnew); FP(err_fp, "\n");
	if( np->fanouts != NULL ) {
	    for(idx_list_ptr il = np->fanouts; il != NULL; il = il->next) {
		ncomp_ptr c;
		c = (ncomp_ptr) M_LOCATE_BUF(compositesp,il->idx);
		todo++;
		add_op_todo(c);
	    }
	}
    }
    return todo;
}

static gbv *
allocate_value_buf(int sz, gbv H, gbv L)
{
    gbv *res = Malloc(2*sz*sizeof(gbv));
    for(int i = 0; i < sz; i++) {
	*(res+2*i) = H;
	*(res+2*i+1) = L;
    }
    return res;
}

static bool
initialize(ste_ptr ste, bool trace_all)
{
    nnode_ptr np;
    FOR_BUF(nodesp, nnode_rec, np) {
	np->has_phase_event = FALSE;
	np->has_weak = FALSE;
	np->has_ant = FALSE;
	np->has_cons = FALSE;
	np->has_trace = FALSE;
    }
    ncomp_ptr cp;
    FOR_BUF(compositesp, ncomp_rec, cp) {
	cp->flag = FALSE;
	add_op_todo(cp);
    }
    int iterations = RCStep_limit;
    // Now do constant propagation
    int todo = 1;
    while( todo > 0 ) {
	if( --iterations < 0 ) {
	    FP(sim_fp, "Circuit did not stabilize in %d steps!\n",RCStep_limit);
	    // %%%%%%%%%%%%% FIX %%%%%%%%%%%%%%%%
	    //FP(sim_fp, "Oscillating nodes set to X\n");
	    return FALSE;
	}
	todo = do_combinational(ste);
	int idx = 0;
	FOR_BUF(nodesp, nnode_rec, np) {
	    if( trace_all || np->has_trace ) {
		gbv curH = *(cur_buf+2*idx);
		gbv curL = *(cur_buf+2*idx+1);
		record_trace(idx, 0, curH, curL);
	    }
	    idx++;
	}
	if( todo < 0 ) return FALSE;
	if( todo > 0 ) 
	    do_phase(ste);
    }
    return( TRUE );
}

static sch_ptr
mk_repeat(vstate_ptr vp, char *anon)
{
    sch_ptr res = (sch_ptr) new_rec(&(vp->sch_rec_mgr));
    res->vec = anon;
    res->pfn = s_draw_repeat;
    res->children = NULL;
    return res;
}

static bool
has_ifc_nodes(vstate_ptr vp, ilist_ptr il, ilist_ptr *silp, ilist_ptr *rilp)
{
    int cnt = 0;
    int ifc_nds = 0;
    FOREACH_NODE(nd, il) {
	if( find_hash(&(vp->ifc_nds), INT2PTR(nd)) != NULL ) {
	    ifc_nds++;
	}
	cnt++;
    }
    if( ifc_nds == 0 ) { return FALSE; }
    if( ifc_nds == cnt ) {
	*rilp = NULL;
	*silp = il;
	return TRUE;
    }
    // Go through the list again and collect different indices. 
    *silp = NULL;
    *rilp = NULL;
    FOREACH_NODE(nd, il) {
	if( find_hash(&(vp->ifc_nds), INT2PTR(nd)) != NULL ) {
	    *silp = append_range(*silp, nd, nd);
	} else {
	    *rilp = append_range(*rilp, nd, nd);
	}
    }
    return TRUE;
}

static bool
has_stop_nodes(vstate_ptr vp, ilist_ptr il, ilist_ptr *silp, ilist_ptr *rilp)
{
    int cnt = 0;
    int stop_nds = 0;
    FOREACH_NODE(nd, il) {
	if( find_hash(&(vp->stop_nds), INT2PTR(nd)) != NULL ) {
	    stop_nds++;
	}
	cnt++;
    }
    if( stop_nds == 0 ) { return FALSE; }
    if( stop_nds == cnt ) {
	*rilp = NULL;
	*silp = il;
	return TRUE;
    }
    // Go through the list again and collect different indices. 
    *silp = NULL;
    *rilp = NULL;
    FOREACH_NODE(nd, il) {
	if( find_hash(&(vp->stop_nds), INT2PTR(nd)) != NULL ) {
	    *silp = append_range(*silp, nd, nd);
	} else {
	    *rilp = append_range(*rilp, nd, nd);
	}
    }
    return TRUE;
}

static void
expand_fanin(vstate_ptr vp, hash_record *exp, sch_ptr sch,
	     string aname, int levels, bool expand, int draw_level)
{
    if( find_hash(exp, sch->vec) != NULL ) {
	// Already expanded
	return;
    }
    if( STREQ(sch->vec,aname) ) {
	expand = TRUE;
    }
    if( expand && strstr(sch->pfn, "draw_incomplete") != NULL ) {
	string nname = sch->vec;
	insert_hash(exp, nname, INT2PTR(1));
	int idx = atoi(nname+2);
	ilist_ptr il = *((ilist_ptr *) M_LOCATE_BUF(&(vp->anon_buf), idx));
	delete_hash(&(vp->done), il);
	sch_ptr subtree = draw_fanin(vp, il, levels, idx, draw_level);
	*sch = *subtree;
	return;
    }
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	expand_fanin(vp, exp, sl->sch, aname, levels, expand, draw_level);
    }
}

static void
build_limit_tbl(vstate_ptr vp, hash_record *limit_tblp, string sn, sch_ptr sch)
{
    string an = sch->vec;
    bool is_incomplete = strstr(sch->pfn, "draw_incomplete") != NULL;
    bool is_repeat = strstr(sch->pfn, "draw_repeat_nd") != NULL;
    if( !STREQ(an,sn) && !is_incomplete && !is_repeat ) {
	int idx = atoi(an+2);
	ilist_ptr il = *((ilist_ptr *) M_LOCATE_BUF(&(vp->anon_buf), idx));
	insert_hash(limit_tblp, il, INT2PTR(1));
    }
    for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	build_limit_tbl(vp, limit_tblp, sn, sl->sch);
    }
}

static sch_ptr
limited_draw_fanin(vstate_ptr vsp, ilist_ptr il, hash_record *limit_tblp,
		   int draw_level)
{
    string anon;

    // Is it aldready drawn
    if( (anon = (string) find_hash(&(vsp->done), il)) != NULL ) {
	return( mk_repeat(vsp, anon) );
    }
    // Insert this node since we will draw it.
    int anon_cnt = COUNT_BUF(&(vsp->anon_buf));

    push_buf(&(vsp->anon_buf), &il);
    Sprintf(buf, "an%06d", anon_cnt);
    anon = wastrsave(stringsp, buf);
    insert_hash(&(vsp->done), il, anon);
    ilist_ptr stop_ilist, rem_ilist;
    if( has_stop_nodes(vsp, il, &stop_ilist, &rem_ilist) ) {
	if( rem_ilist == NULL ) {
	    // Just a stop node
	    string pfn = strtemp("draw_stop_symbol {}");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return res;
	}
	// Some of the nodes are stop nodes, but not all
	// Must create a draw_concat 2 symbol and
	// call draw_fanin recursively.
	string pfn = strtemp("draw_concat 2");
	pfn = wastrsave(stringsp, pfn);
	sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	res->vec = anon;
	res->pfn = pfn;
	//
	sch_ptr s_child =
	    limited_draw_fanin(vsp,stop_ilist,limit_tblp,draw_level);
	sch_list_ptr s_sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	s_sl->sch = s_child;
	//
	sch_ptr r_child =
	    limited_draw_fanin(vsp, rem_ilist, limit_tblp, draw_level);
	sch_list_ptr r_sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	r_sl->sch = r_child;
	//
	s_sl->next = r_sl;
	r_sl->next = NULL;
	res->children = s_sl;
	return res;
    }

    ilist_ptr ifc_ilist;
    if( has_ifc_nodes(vsp, il, &ifc_ilist, &rem_ilist) ) {
	if( rem_ilist == NULL ) {
	    // Just an interface node
	    g_ptr nds = ilist2nds(il);
	    g_ptr vecs = Merge_Vectors(nds, TRUE);
	    string pfn = strtemp("");
	    strappend("draw_ifc_input ");
	    char sep = '{';
	    while( !IS_NIL(vecs) ) {
		charappend(sep);
		sep = ' ';
		charappend('{');
		strappend(GET_STRING(GET_CONS_HD(vecs)));
		charappend('}');
		vecs = GET_CONS_TL(vecs);
	    }
	    strappend(" }");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return(res );
	}
    }
    if( find_hash(limit_tblp, il) == NULL ) {
	// An incomplete node  draw_incomplete
	g_ptr nds = ilist2nds(il);
	g_ptr vecs = Merge_Vectors(nds, TRUE);
	string pfn = strtemp("");
	strappend("draw_incomplete ");
	char sep = '{';
	while( !IS_NIL(vecs) ) {
	    charappend(sep);
	    sep = ' ';
	    charappend('{');
	    strappend(GET_STRING(GET_CONS_HD(vecs)));
	    charappend('}');
	    vecs = GET_CONS_TL(vecs);
	}
	strappend("}");
	pfn = wastrsave(stringsp, pfn);
	sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	res->vec = anon;
	res->pfn = pfn;
	res->children = NULL;
	return res;
    }

    vis_ptr cur_info = NULL;
    int current_type = -99;
    // -3 == dangling signal
    // -2 == constant 0/1/x
    // -1 == is_top_input
    // >= 0  draw_routine
    ilist_ptr	cur_list = NULL;
    sch_list_ptr cur_children = NULL;
    FOREACH_NODE(nd, il) {
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	vis_ptr vp = get_vis_info_at_level(np->draw_info, draw_level);
	int new_type;
	if( vp ) {
	    cur_info = vp;
	    new_type = cur_info->id;
	} else if( nd <= 3 ) {
	    new_type = -2;
	} else if( np->is_top_input ) {
	    new_type = -1;
	} else {
	    new_type = -3;
	}
	if( current_type == -99 || current_type == new_type ) {
	    cur_list = append_range(cur_list, nd, nd);
	    current_type = new_type;
	} else {
	    sch_ptr child =
		limited_draw_fanin(vsp, cur_list, limit_tblp, draw_level);
	    sch_list_ptr sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	    sl->sch = child;
	    sl->next = NULL;
	    if( cur_children == NULL ) {
		cur_children = sl;
	    } else {
		sch_list_ptr tsl = cur_children;
		while( tsl->next != NULL ) tsl =  tsl->next;
		tsl->next = sl;
	    }
	    current_type = new_type;
	    cur_list = append_range(NULL, nd, nd);
	}
    }
    if( cur_children == NULL ) {
	// All elements of the same type.
	if( current_type == -2 ) {
	    // A constant.
	    // Should not happen after clean_pexlif_ios
	    DIE("Not expected");
	} else if( current_type == -1 ) {
	    // A top-level input
	    g_ptr nds = ilist2nds(il);
	    g_ptr vecs = Merge_Vectors(nds, TRUE);
	    string pfn = strtemp("");
	    strappend("draw_input ");
	    char sep = '{';
	    while( !IS_NIL(vecs) ) {
		charappend(sep);
		sep = ' ';
		charappend('{');
		strappend(GET_STRING(GET_CONS_HD(vecs)));
		charappend('}');
		vecs = GET_CONS_TL(vecs);
	    }
	    strappend("}");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return res;
	} else if( current_type == -3 ) {
	    // A dangling signal
	    g_ptr nds = ilist2nds(il);
	    g_ptr vecs = Merge_Vectors(nds, TRUE);
	    string pfn = strtemp("");
	    strappend("draw_dangling_input ");
	    char sep = '{';
	    while( !IS_NIL(vecs) ) {
		charappend(sep);
		sep = ' ';
		charappend('{');
		strappend(GET_STRING(GET_CONS_HD(vecs)));
		charappend('}');
		vecs = GET_CONS_TL(vecs);
	    }
	    strappend(" }");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return res;
	} else {
	    // A drawing routine
            if( !all_outs(il, cur_info->fa_outs) ) {
                ilist_ptr new_il = NULL;
                for(vis_io_ptr vp=cur_info->fa_outs; vp != NULL; vp=vp->next) {
                    new_il = ilist_append(new_il, ilist_copy(vp->acts));
                }
                string pfn = strtemp("draw_split");
                pfn = wastrsave(stringsp, pfn);
                sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
                res->vec = anon;
                res->pfn = pfn;
                //
                sch_ptr s_child =
		    limited_draw_fanin(vsp, new_il, limit_tblp, draw_level);
                sch_list_ptr s_sl =
                        (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
                s_sl->sch = s_child;
		s_sl->next = NULL;
                res->children = s_sl;
                return res;
            }

	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = cur_info->pfn;
	    res->children = NULL;
	    for(vis_io_ptr vi = cur_info->fa_inps; vi != NULL; vi = vi->next) {
		sch_ptr child =
		    limited_draw_fanin(vsp, vi->acts, limit_tblp, draw_level);
		sch_list_ptr sl=(sch_list_ptr)new_rec(&(vsp->sch_list_rec_mgr));
		sl->sch = child;
		sl->next = NULL;
		if( res->children == NULL ) {
		    res->children = sl;
		} else {
		    sch_list_ptr tsl = res->children;
		    while( tsl->next != NULL ) tsl =  tsl->next;
		    tsl->next = sl;
		}
	    }
	    return res;
	}
    } else {
	// Mixed type. Add a catenate drawing
	sch_ptr child = limited_draw_fanin(vsp, cur_list,limit_tblp,draw_level);
	sch_list_ptr sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	sl->sch = child;
	sl->next = NULL;
	if( cur_children == NULL ) {
	    cur_children = sl;
	} else {
	    sch_list_ptr tsl = cur_children;
	    while( tsl->next != NULL ) tsl =  tsl->next;
	    tsl->next = sl;
	}
	sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	res->vec = anon;
	string pfn = strtemp("draw_concat ");
	char buf[20];
	int len = 0;
	for(sch_list_ptr sl = cur_children; sl != NULL; sl = sl->next) {
	    len++;
	}
	Sprintf(buf, "%d", len);
	strappend(buf);
	res->pfn = wastrsave(stringsp, pfn);
	res->children = cur_children;
	return res;
    }
}

static bool
same_ilist(ilist_ptr il1, ilist_ptr il2)
{
    while( il1 != NULL ) {
	if( il2 == NULL ) return FALSE;
	if( il1->from != il2->from ) return FALSE;
	if( il1->to != il2->to ) return FALSE;
	il1 = il1->next;
	il2 = il2->next;
    }
    if( il2 != NULL ) return FALSE;
    return TRUE;
}

static bool
all_outs(ilist_ptr il, vis_io_ptr vp)
{
    if( vp != NULL && vp->next == NULL && same_ilist(il, vp->acts ) ) {
	return TRUE;
    }
    hash_record used;
    create_hash(&used, 100, int_hash, int_equ);
    FOREACH_NODE(nd, il) {
	if( find_hash(&used, INT2PTR(nd)) == NULL )
	    insert_hash(&used, INT2PTR(nd), INT2PTR(1));
    }
    while( vp != NULL ) {
	FOREACH_NODE(nd, vp->acts) {
	    if( find_hash(&used, INT2PTR(nd)) == NULL ) {
		dispose_hash(&used, NULLFCN);
		return FALSE;
	    }
	    delete_hash(&used, INT2PTR(nd));
	}
	vp = vp->next;
    }
    bool res = FALSE;
    if( hash_size(&used) == 0 ) {
	res = TRUE;
    }
    dispose_hash(&used, NULLFCN);
    return res;
}


static sch_ptr
draw_fanin(vstate_ptr vsp, ilist_ptr il, int levels, int anon_cnt,
	   int draw_level)
{
    string anon;


    // Is it aldready drawn
    if( (anon = (string) find_hash(&(vsp->done), il)) != NULL ) {
	return(mk_repeat(vsp, anon) );
    }
    // Insert this node since we will draw it.
    if( anon_cnt < 0 ) {
	anon_cnt = COUNT_BUF(&(vsp->anon_buf));
    }
    push_buf(&(vsp->anon_buf), &il);
    Sprintf(buf, "an%06d", anon_cnt);
    anon = wastrsave(stringsp, buf);
    insert_hash(&(vsp->done), il, anon);
    ilist_ptr stop_ilist, rem_ilist;

    if( has_stop_nodes(vsp, il, &stop_ilist, &rem_ilist) ) {
	if( rem_ilist == NULL ) {
	    // Just a stop node
	    string pfn = strtemp("draw_stop_symbol {}");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return(res );
	}
	// Some of the nodes are stop nodes, but not all
	// Must create a draw_concat 2 symbol and
	// call draw_fanin recursively.
	string pfn = strtemp("draw_concat 2");
	pfn = wastrsave(stringsp, pfn);
	sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	res->vec = anon;
	res->pfn = pfn;
	//
	sch_ptr s_child = draw_fanin(vsp, stop_ilist, levels, -1, draw_level);
	sch_list_ptr s_sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	s_sl->sch = s_child;
	//
	sch_ptr r_child = draw_fanin(vsp, rem_ilist, levels, -1, draw_level);
	sch_list_ptr r_sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	r_sl->sch = r_child;
	//
	s_sl->next = r_sl;
	r_sl->next = NULL;
	res->children = s_sl;
	return( res );
    }

    ilist_ptr ifc_ilist;
    if( has_ifc_nodes(vsp, il, &ifc_ilist, &rem_ilist) ) {
	if( rem_ilist == NULL ) {
	    // Just an interface node
	    g_ptr nds = ilist2nds(il);
	    g_ptr vecs = Merge_Vectors(nds, TRUE);
	    string pfn = strtemp("");
	    strappend("draw_ifc_input ");
	    char sep = '{';
	    while( !IS_NIL(vecs) ) {
		charappend(sep);
		sep = ' ';
		charappend('{');
		strappend(GET_STRING(GET_CONS_HD(vecs)));
		charappend('}');
		vecs = GET_CONS_TL(vecs);
	    }
	    strappend(" }");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return(res );
	}
    }

    if( levels <= 0 && (levels < -10 || ilist_is_user_defined(il)) ) {
	// An incomplete node  draw_incomplete
	g_ptr nds = ilist2nds(il);
	g_ptr vecs = Merge_Vectors(nds, TRUE);
	string pfn = strtemp("");
	strappend("draw_incomplete ");
	char sep = '{';
	while( !IS_NIL(vecs) ) {
	    charappend(sep);
	    sep = ' ';
	    charappend('{');
	    strappend(GET_STRING(GET_CONS_HD(vecs)));
	    charappend('}');
	    vecs = GET_CONS_TL(vecs);
	}
	strappend("}");
	pfn = wastrsave(stringsp, pfn);
	sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	res->vec = anon;
	res->pfn = pfn;
	res->children = NULL;
	return( res );
    }
    vis_ptr cur_info = NULL;
    int current_type = -99;
    // -3 == dangling signal
    // -2 == constant 0/1/x
    // -1 == is_top_input
    // >= 0  draw_routine
    ilist_ptr	cur_list = NULL;
    sch_list_ptr cur_children = NULL;
    FOREACH_NODE(nd, il) {
	nnode_ptr np = (nnode_ptr) M_LOCATE_BUF(nodesp, nd);
	vis_ptr vp = get_vis_info_at_level(np->draw_info,draw_level);
	int new_type;
	if( vp ) {
	    cur_info = vp;
	    new_type = cur_info->id;
	} else if( nd <= 3 ) {
	    new_type = -2;
	} else if( np->is_top_input ) {
	    new_type = -1;
	} else {
	    new_type = -3;
	}
	if( current_type == -99 || current_type == new_type ) {
	    cur_list = append_range(cur_list, nd, nd);
	    current_type = new_type;
	} else {
	    sch_ptr child = draw_fanin(vsp, cur_list, levels, -1, draw_level);
	    sch_list_ptr sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	    sl->sch = child;
	    sl->next = NULL;
	    if( cur_children == NULL ) {
		cur_children = sl;
	    } else {
		sch_list_ptr tsl = cur_children;
		while( tsl->next != NULL ) tsl =  tsl->next;
		tsl->next = sl;
	    }
	    current_type = new_type;
	    cur_list = append_range(NULL, nd, nd);
	}
    }
    if( cur_children == NULL ) {
	// All elements of the same type.
	if( current_type == -2 ) {
	    // A constant.
	    // Should not happen after clean_pexlif_ios
	    DIE("Not expected");
	} else if( current_type == -1 ) {
	    // A top-level input
	    g_ptr nds = ilist2nds(il);
	    g_ptr vecs = Merge_Vectors(nds, TRUE);
	    string pfn = strtemp("");
	    strappend("draw_input ");
	    char sep = '{';
	    while( !IS_NIL(vecs) ) {
		charappend(sep);
		sep = ' ';
		charappend('{');
		strappend(GET_STRING(GET_CONS_HD(vecs)));
		charappend('}');
		vecs = GET_CONS_TL(vecs);
	    }
	    strappend("}");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return( res );
	} else if( current_type == -3 ) {
	    // A dangling signal
	    g_ptr nds = ilist2nds(il);
	    g_ptr vecs = Merge_Vectors(nds, TRUE);
	    string pfn = strtemp("");
	    strappend("draw_dangling_input ");
	    char sep = '{';
	    while( !IS_NIL(vecs) ) {
		charappend(sep);
		sep = ' ';
		charappend('{');
		strappend(GET_STRING(GET_CONS_HD(vecs)));
		charappend('}');
		vecs = GET_CONS_TL(vecs);
	    }
	    strappend(" }");
	    pfn = wastrsave(stringsp, pfn);
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = pfn;
	    res->children = NULL;
	    return( res );
	} else {
	    // A drawing routine
	    // Do we use all outputs?
	    if( !all_outs(il, cur_info->fa_outs) ) {
		ilist_ptr new_il = NULL;
		for(vis_io_ptr vp=cur_info->fa_outs; vp != NULL; vp=vp->next) {
		    new_il = ilist_append(new_il, ilist_copy(vp->acts));
		}
		string pfn = strtemp("draw_split");
		pfn = wastrsave(stringsp, pfn);
		sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
		res->vec = anon;
		res->pfn = pfn;
		//
		sch_ptr s_child = draw_fanin(vsp,new_il,levels,-1,draw_level);
		sch_list_ptr s_sl =
			(sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
		s_sl->sch = s_child;
		s_sl->next = NULL;
		res->children = s_sl;
		return( res );
	    }
	    sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	    res->vec = anon;
	    res->pfn = cur_info->pfn;
	    res->children = NULL;
	    for(vis_io_ptr vi = cur_info->fa_inps; vi != NULL; vi = vi->next) {
		sch_ptr child = draw_fanin(vsp,vi->acts,levels-1,-1,draw_level);
		sch_list_ptr sl =(sch_list_ptr)new_rec(&(vsp->sch_list_rec_mgr));
		sl->sch = child;
		sl->next = NULL;
		if( res->children == NULL ) {
		    res->children = sl;
		} else {
		    sch_list_ptr tsl = res->children;
		    while( tsl->next != NULL ) tsl =  tsl->next;
		    tsl->next = sl;
		}
	    }
	    return( res );
	}
    } else {
	// Mixed type. Add a catenate drawing
	sch_ptr child = draw_fanin(vsp, cur_list, levels, -1,draw_level);
	sch_list_ptr sl = (sch_list_ptr) new_rec(&(vsp->sch_list_rec_mgr));
	sl->sch = child;
	sl->next = NULL;
	if( cur_children == NULL ) {
	    cur_children = sl;
	} else {
	    sch_list_ptr tsl = cur_children;
	    while( tsl->next != NULL ) tsl =  tsl->next;
	    tsl->next = sl;
	}
	sch_ptr res = (sch_ptr) new_rec(&(vsp->sch_rec_mgr));
	res->vec = anon;
	string pfn = strtemp("draw_concat ");
	char buf[20];
	int len = 0;
	for(sch_list_ptr sl = cur_children; sl != NULL; sl = sl->next) {
	    len++;
	}
	Sprintf(buf, "%d", len);
	strappend(buf);
	res->pfn = wastrsave(stringsp, pfn);
	res->children = cur_children;
	return( res );
    }
}

static string
mk_fresh_anon_name(g_ptr internals, g_ptr fa_inps, g_ptr fa_outs, int *cur_cntp)
{
    bool found = TRUE;
    int cur_cnt = *cur_cntp;
    while( found ) {
	cur_cnt++;
	Sprintf(buf, "_$%d", cur_cnt);
	int len = strlen(buf);
	found = FALSE;
	for(g_ptr cur = internals; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	    if( strncmp(GET_STRING(GET_CONS_HD(cur)), buf, len) == 0 ) {
		found = TRUE;
		break;
	    }
	}
	if( !found ) {
	    for(g_ptr cur = fa_inps; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
		if( strncmp(GET_STRING(GET_FST(GET_CONS_HD(cur))), buf, len)
		    == 0 )
		{
		    found = TRUE;
		    break;
		}
	    }
	}
	if( !found ) {
	    for(g_ptr cur = fa_outs; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
		if( strncmp(GET_STRING(GET_FST(GET_CONS_HD(cur))), buf, len)
		    == 0 )
		{
		    found = TRUE;
		    break;
		}
	    }
	}

    }
    *cur_cntp = cur_cnt;
    return( wastrsave(stringsp, buf) );
}

static string
mk_vector_name(string base, int size)
{
    if( size == 1 ) {
	    return( wastrsave(stringsp, base) );
    } else {
	string res = strtemp(base);
	char buf[20];
	Sprintf(buf, "[%d:0]", size-1);
	strappend(buf);
	return( wastrsave(stringsp, res) );
    }
}

static string
create_constant(int sz, int *ccnt, g_ptr ints, g_ptr fa_inps, g_ptr fa_outs,
		string cnst, buffer *chbufp)
{
    ASSERT( *cnst == '0' && *(cnst+1) == 'b' );
    ASSERT( sz == (int) strlen(cnst)-2 );
    string base = mk_fresh_anon_name(ints, fa_inps, fa_outs, ccnt);
    string out  = mk_vector_name(base, sz);
    string tmp = NULL;
    if( index(cnst, 'z') != NULL || index(cnst, 'Z') != NULL ) {
	FP(warning_fp, "z-values not supported. Treated as x!\n");
	tmp = (string) malloc((strlen(cnst)+1));
	strcpy(tmp,cnst);
	string p = tmp;
	while( *p ) {
	    if( *p == 'z' || *p == 'Z' ) {
		*p = 'X';
	    }
	    p++;
	}
	cnst = tmp;
    }
    if( index(cnst, 'x') != NULL || index(cnst, 'X') != NULL ) {
	// A 0,1,x constant
	g_ptr leaf = Make_NIL();
	g_ptr tail = leaf;
	int i = sz-1;
	string s = cnst+2;
	while( *s ) {
	    switch( *s ) {
		case 'x':
		case 'X': {
		    // 
		    g_ptr indices = Make_CONS_ND(Make_INT_leaf(i), Make_NIL());
		    g_ptr var = mk_W_VAR(Make_INT_leaf(sz),
					 Make_STRING_leaf(base));
		    g_ptr lhs = mk_W_SLICE(indices, var);
		    g_ptr rhs = mk_W_X(Make_INT_leaf(1));
		    SET_CONS_HD(tail, mk_W_UPDATE_FN(lhs, rhs));
		    break;
		}
		case '0': {
		    // 
		    g_ptr indices = Make_CONS_ND(Make_INT_leaf(i), Make_NIL());
		    g_ptr var = mk_W_VAR(Make_INT_leaf(sz),
					 Make_STRING_leaf(base));
		    g_ptr lhs = mk_W_SLICE(indices, var);
		    g_ptr rhs = mk_W_CONST(Make_INT_leaf(1), Make_INT_leaf(0));
		    SET_CONS_HD(tail, mk_W_UPDATE_FN(lhs, rhs));
		    break;
		}
		case '1': {
		    // 
		    g_ptr indices = Make_CONS_ND(Make_INT_leaf(i), Make_NIL());
		    g_ptr var = mk_W_VAR(Make_INT_leaf(sz),
					 Make_STRING_leaf(base));
		    g_ptr lhs = mk_W_SLICE(indices, var);
		    g_ptr rhs = mk_W_CONST(Make_INT_leaf(1), Make_INT_leaf(1));
		    SET_CONS_HD(tail, mk_W_UPDATE_FN(lhs, rhs));
		    break;
		}
		default:
		    FP(err_fp, "Unsupported constant %s", cnst);
		    report_source_locations(err_fp);
		    Rprintf("");
		    break;
	    }
	    SET_CONS_TL(tail, Make_NIL());
	    tail = GET_CONS_TL(tail);
	    s++;
	    i--;
	}
	string pfn = strtemp("");
	strappend("draw_constant ");
	strappend( cnst );
	pfn = wastrsave(stringsp, pfn);
	g_ptr res = mk_PINST(
			Make_STRING_leaf(pfn), 
			Make_NIL(),
			Make_BOOL_leaf(B_One()),
			Make_NIL(),
			mk_list1(mk_pair(Make_STRING_leaf(out),
					 mk_list1(Make_STRING_leaf(out)))),
			Make_NIL(),
			mk_P_LEAF(leaf));
	push_buf(chbufp, &res);
	if( tmp != NULL ) free(tmp);
	return out;
    } else {
	// A 0,1 constant
	arbi_T v = Arbi_FromString(cnst+2, 2);
	string pfn = strtemp("");
	strappend("draw_constant 0x");
	strappend( Arbi_ToString(v, 16) );
	pfn = wastrsave(stringsp, pfn);
	// PINST pfn T [] [] [(t,[t])] [] (P_LEAF (W_UPDATE_FN t e))
	g_ptr res = mk_PINST(
			    Make_STRING_leaf(pfn), 
			    Make_NIL(),
			    Make_BOOL_leaf(B_One()),
			    Make_NIL(),
			    mk_list1(mk_pair(Make_STRING_leaf(out),
					     mk_list1(Make_STRING_leaf(out)))),
			    Make_NIL(),
			    mk_P_LEAF(mk_list1(
					mk_W_UPDATE_FN(
					    mk_W_VAR(Make_INT_leaf(sz),
						     Make_STRING_leaf(base)),
					    mk_W_CONST(Make_INT_leaf(sz),
						       Make_AINT_leaf(v))))));

	push_buf(chbufp, &res);
	if( tmp != NULL ) free(tmp);
	return out;
    }
}

static bool
is_binary_wexpr(g_ptr node, wl_op *opp, g_ptr *ap, g_ptr *bp)
{
    EXTRACT(*bp);
    EXTRACT(*ap);
    if( !IS_LEAF(node) || !IS_STRING(node) ) { return FALSE; } 
    string tp = GET_STRING(node);
    if( STREQ(tp, s_W_AND) ) {
		*opp = op_AND;
		return TRUE;
    }
    if( STREQ(tp, s_W_OR) ) {
		*opp = op_OR;
		return TRUE;
    }
    if( STREQ(tp, s_W_ADD) ) {
		*opp = op_ADD;
		return TRUE;
    }
    if( STREQ(tp, s_W_SUB) ) {
		*opp = op_SUB;
		return TRUE;
    }
    if( STREQ(tp, s_W_MUL) ) {
		*opp = op_MUL;
		return TRUE;
    }
    if( STREQ(tp, s_W_DIV) ) {
		*opp = op_DIV;
		return TRUE;
    }
    if( STREQ(tp, s_W_MOD) ) {
		*opp = op_MOD;
		return TRUE;
    }
    if( STREQ(tp, s_W_SHL) ) {
		*opp = op_SHL;
		return TRUE;
    }
    if( STREQ(tp, s_W_SHR) ) {
		*opp = op_SHR;
		return TRUE;
    }
    if( STREQ(tp, s_W_ASHR) ) {
		*opp = op_ASHR;
		return TRUE;
    }
    if( STREQ(tp, s_W_LAT_LEQ) ) {
		*opp = op_LAT_LEQ;
		return TRUE;
    }
    return FALSE;
}

static bool
is_relation_wexpr(g_ptr node, wl_op *opp, g_ptr *ap, g_ptr *bp)
{
    EXTRACT(*bp);
    EXTRACT(*ap);
    if( !IS_LEAF(node) || !IS_STRING(node) ) { return FALSE; } 
    string tp = GET_STRING(node);
    if( STREQ(tp, s_W_EQ) ) {
		*opp = op_EQ;
		return TRUE;
    }
    if( STREQ(tp, s_W_GR) ) {
		*opp = op_GR;
		return TRUE;
    }
    return FALSE;
}

static g_ptr
mk_list1(g_ptr el)
{
    return( Make_CONS_ND(el, Make_NIL()) );
}

static g_ptr
mk_pair(g_ptr fst, g_ptr snd)
{
    return( Make_CONS_ND(fst, snd) );
}

static bool
all_same(string msb, g_ptr al)
{
    while( !IS_NIL(al) ) {
	if( !STREQ(msb, GET_STRING(GET_CONS_HD(al))) ) { return FALSE; }
	al = GET_CONS_TL(al);
    }
    return( TRUE );
}

static string
create_merge_component(int sz, int *ccnt, g_ptr *intsp,
		       g_ptr fa_inps, g_ptr fa_outs,
		       g_ptr acts, int len, buffer *chbufp)
{
    string base = mk_fresh_anon_name(*intsp, fa_inps, fa_outs, ccnt);
    string out  = mk_vector_name(base, sz);
    *intsp = Make_CONS_ND(Make_STRING_leaf(out), *intsp);
    string msb = GET_STRING(GET_CONS_HD(acts));
    if( (Get_Vector_Size(msb) == 1) && ((len == 1) || all_same(msb, acts)) ) {
	g_ptr al = acts;
	g_ptr base_inp = al;
	int cnt = 0;
	while( !IS_NIL(al) && STREQ(msb, GET_STRING(GET_CONS_HD(al))) ) {
	    base_inp = al;
	    al = GET_CONS_TL(al);
	    cnt++;
	}
	if( cnt > 1 ) {
	    // SX
	    if( cnt == len ) {
		// SX of single bit
		string pfn = wastrsave(stringsp, "draw_unary_arithm {SX}");
		g_ptr c_ints = Make_NIL();
		g_ptr c_outs =
			mk_list1(mk_pair(Make_STRING_leaf(out),
					 mk_list1(Make_STRING_leaf(out))));
		string i1 = wastrsave(stringsp, "i1");
		g_ptr c_inps =
			mk_list1(mk_pair(Make_STRING_leaf(i1),
					 mk_list1(Make_STRING_leaf(msb))));
		g_ptr rhs = mk_W_SX(Make_INT_leaf(sz),
				    mk_W_VAR(Make_INT_leaf(1),
					     Make_STRING_leaf(i1)));
		g_ptr lhs = mk_W_VAR(Make_INT_leaf(sz), Make_STRING_leaf(base));
		g_ptr c_content = mk_P_LEAF(mk_list1(mk_W_UPDATE_FN(lhs, rhs)));
		g_ptr res = mk_PINST(Make_STRING_leaf(pfn),
				     Make_NIL(),
				     Make_BOOL_leaf(B_One()),
				     c_inps,
				     c_outs,
				     c_ints,
				     c_content);
		push_buf(chbufp, &res);
		return( out );
	    } else {
		// SX of vector
		string pfn = wastrsave(stringsp, "draw_unary_arithm {SX}");
		g_ptr c_ints = Make_NIL();
		g_ptr c_outs = mk_list1(mk_pair(Make_STRING_leaf(out),
					      mk_list1(Make_STRING_leaf(out))));
		char iv[30];
		sprintf(iv, "i1[%d:0]", sz-cnt);
		string i1 = wastrsave(stringsp, iv);
		string w1 = wastrsave(stringsp, "i1");
		g_ptr c_inps =
			mk_list1(mk_pair(Make_STRING_leaf(i1), base_inp));
		g_ptr rhs = mk_W_SX(Make_INT_leaf(sz),
				    mk_W_VAR(Make_INT_leaf(sz-cnt+1),
					     Make_STRING_leaf(w1)));
		g_ptr lhs = mk_W_VAR(Make_INT_leaf(sz), Make_STRING_leaf(base));
		g_ptr c_content = mk_P_LEAF(mk_list1(mk_W_UPDATE_FN(lhs, rhs)));
		g_ptr res = mk_PINST(Make_STRING_leaf(pfn),
				     Make_NIL(),
				     Make_BOOL_leaf(B_One()),
				     c_inps,
				     c_outs,
				     c_ints,
				     c_content);
		push_buf(chbufp, &res);
		return( out );
	    }
	}
    }
    string pfn = strtemp("draw_concat ");
    Sprintf(buf, "%d", len);
    strappend(buf);
    pfn = wastrsave(stringsp, pfn);
    g_ptr c_ints = Make_NIL();
    g_ptr c_outs = mk_list1(mk_pair(Make_STRING_leaf(out),
				    mk_list1(Make_STRING_leaf(out))));
    int inp_cnt = 1;
    g_ptr cats = Make_NIL();
    g_ptr tail_cats = cats;
    g_ptr c_inps = Make_NIL();
    g_ptr tail_inps = c_inps;
    while( !IS_NIL(acts) ) {
	string act = GET_STRING(GET_CONS_HD(acts));
	int csz;
	if( *act == '0' ) {
	    csz = strlen(act)-2;
	    string t = create_constant(csz, ccnt, *intsp, fa_inps, fa_outs,
				       act, chbufp);
	    *intsp = Make_CONS_ND(Make_STRING_leaf(t), *intsp);
	    Sprintf(buf, "i%d", inp_cnt);
	    g_ptr f = Make_STRING_leaf(mk_vector_name(buf, csz));
	    g_ptr fa = mk_pair(f, mk_list1(Make_STRING_leaf(t)));
	    APPEND1(tail_inps, fa);
	    g_ptr part = mk_W_VAR(Make_INT_leaf(csz),
				  Make_STRING_leaf(wastrsave(stringsp, buf)));
	    APPEND1(tail_cats, part);
	} else {
	    csz = Get_Vector_Size(act);
	    Sprintf(buf, "i%d", inp_cnt);
	    g_ptr f = Make_STRING_leaf(mk_vector_name(buf, csz));
	    g_ptr fa = mk_pair(f, mk_list1(Make_STRING_leaf(act)));
	    APPEND1(tail_inps, fa);
	    g_ptr part = mk_W_VAR(Make_INT_leaf(csz),
				  Make_STRING_leaf(wastrsave(stringsp, buf)));
	    APPEND1(tail_cats, part);
	}
	inp_cnt++;
	acts = GET_CONS_TL(acts);
    }
    c_ints = Make_NIL();
    g_ptr rhs = mk_W_CAT(cats);
    g_ptr lhs = mk_W_VAR(Make_INT_leaf(sz), Make_STRING_leaf(base));
    g_ptr c_content = mk_P_LEAF(mk_list1(mk_W_UPDATE_FN(lhs, rhs)));
    g_ptr res = mk_PINST(Make_STRING_leaf(pfn),
			 Make_NIL(),
			 Make_BOOL_leaf(B_One()),
			 c_inps,
			 c_outs,
			 c_ints,
			 c_content);
    push_buf(chbufp, &res);
    return out;
}


static g_ptr
clean_pexlif_ios(g_ptr node)
{
    g_ptr   name, leaf, attrs, inps, outs, ints, content;
    destr_PINST(node,&name,&attrs,&leaf,&inps,&outs,&ints,&content);
    g_ptr children;
    if( !is_P_HIER(content, &children) ) {
	return node;
    }
    // Do it recursively
    g_ptr new_children = Make_NIL();
    g_ptr new_children_tl = new_children;
    for(g_ptr cur = children; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	g_ptr old = GET_CONS_HD(cur);
	g_ptr new = clean_pexlif_ios(old);
	APPEND1(new_children_tl, new)
    }
    // Now separate out split inputs, constant inputs etc.
    int cur_cnt = 0;
    buffer ch_buf;
    new_buf(&ch_buf, 100, sizeof(g_ptr));
    for(g_ptr cur = new_children; !IS_NIL(cur); cur = GET_CONS_TL(cur)) {
	g_ptr p = GET_CONS_HD(cur);
	g_ptr cnm, cleaf, cattrs, cinps, couts, cints, ccontent;
	destr_PINST(p,&cnm,&cattrs,&cleaf,&cinps,&couts,&cints,&ccontent);
	g_ptr new_cinps = Make_NIL();
	g_ptr new_cinps_tl = new_cinps;
	for(g_ptr inp = cinps; !IS_NIL(inp); inp = GET_CONS_TL(inp)) {
	    g_ptr pair = GET_CONS_HD(inp);
	    g_ptr new_pair = Get_node();
	    *new_pair = *pair;
	    APPEND1(new_cinps_tl, new_pair);
	    string fname = GET_STRING(GET_CONS_HD(pair));
	    g_ptr acts = GET_CONS_TL(pair);
	    if( IS_NIL(acts) ) {
		FP(err_fp, "Actuals to argument %s is empty inside %s\n",
			fname, GET_STRING(cnm));
		report_source_locations(err_fp);
		Rprintf("");
	    }
	    int len = List_length(acts);
	    if( len > 1 ) {
		int sz = Get_Vector_Size(fname);
		string t = create_merge_component(sz, &cur_cnt, &ints, inps,
						  outs, acts, len, &ch_buf);
		g_ptr new_acts = Make_CONS_ND(Make_STRING_leaf(t), Make_NIL());
		SET_CONS_TL(new_pair, new_acts);
	    } else {
		string act = GET_STRING(GET_CONS_HD(acts));
		if( *act == '0' ) {
		    // Constant
		    int sz = Get_Vector_Size(fname);
		    string t = create_constant(sz, &cur_cnt, ints, inps, outs,
					       act, &ch_buf);
		    ints = Make_CONS_ND(Make_STRING_leaf(t), ints);
		    g_ptr nacts = Make_CONS_ND(Make_STRING_leaf(t), Make_NIL());
		    SET_CONS_TL(new_pair, nacts);
		}
	    }
	}
	SET_CONS_HD(cur, 
		    mk_PINST(cnm,cattrs,cleaf,new_cinps,couts,cints,ccontent));
    }
    g_ptr *gpp;
    FOR_BUF(&ch_buf, g_ptr, gpp) {
	APPEND1(new_children_tl, *gpp)
    }
    free_buf(&ch_buf);
    g_ptr new_content = mk_P_HIER(new_children);
    g_ptr res = mk_PINST(name, attrs, leaf, inps, outs, ints, new_content);
    return res;
}

static int
fsm_sha256_fn(int *g_cntp, hash_record *g_tblp, SHA256_ptr sha, pointer a)
{
    (void) g_tblp;
    fsm_ptr fsm = (fsm_ptr) a;
    int res = *g_cntp;
    *g_cntp = res+1;
    SHA256_printf(sha, "%d=FSM %s\n", res, fsm->sha256_sig);
    return res;     
}


// Dummy function to make these function appear to be used.
void
_DuMMy_fsm()
{
    SET_GBV(NULL, c_ZERO);
    clean_pexlif_ios(NULL);
    dbg_print_stop_nds(NULL, NULL);
    dbg_print_ils(NULL, NULL);
    dbg_info("", 0, NULL);
}

static event_list_ptr
add_active_event(hash_record *tblp, event_ptr ep)
{
    int nd_idx = ep->nd_idx;
    event_list_ptr new_elp = (event_list_ptr) new_rec(event_list_rec_mgrp);
    new_elp->ep = ep;
    event_list_ptr  elp = (event_list_ptr) find_hash(tblp, INT2PTR(nd_idx));
    new_elp->next = elp;
    if( elp != NULL ) {
	delete_hash(tblp, INT2PTR(nd_idx));
    }
    insert_hash(tblp, INT2PTR(nd_idx), new_elp);
    return new_elp;
}

static event_list_ptr
delete_active_event(hash_record *tblp, event_ptr ep)
{
    int nd_idx = ep->nd_idx;
    event_list_ptr  elp = (event_list_ptr) find_hash(tblp, INT2PTR(nd_idx));
    ASSERT(elp != NULL);
    int event_id = ep->event_id;
    if( elp->ep->event_id == event_id ) {
	// First in list
	delete_hash(tblp, INT2PTR(nd_idx));
	event_list_ptr next = elp->next;
	if( next != NULL ) {
	    insert_hash(tblp, INT2PTR(nd_idx), next);
	}
	free_rec(event_list_rec_mgrp, elp);
	return next;
    }
    event_list_ptr start = elp;
    event_list_ptr prev = elp;
    elp = elp->next;
    while( elp && elp->ep->event_id != event_id) {
	prev = elp;
	elp = elp->next;
    }
    ASSERT(elp != NULL);
    prev->next = elp->next;
    free_rec(event_list_rec_mgrp, elp);
    return start;
}

static void
allocate_ste_buffers(fsm_ptr fsm)
{
    int nds  = COUNT_BUF(&(fsm->nodes));
    weak_buf = allocate_value_buf(nds, c_ZERO, c_ZERO);
    ant_buf  = allocate_value_buf(nds, c_ONE, c_ONE);
    cons_buf = allocate_value_buf(nds, c_ONE, c_ONE);
    cur_buf  = allocate_value_buf(nds, c_ONE, c_ONE);
    next_buf = allocate_value_buf(nds, c_ONE, c_ONE);
    // Set the initial values correctly for built-in constants
    *(cur_buf+0) = c_ZERO; *(cur_buf+1) = c_ONE;    // !0
    *(cur_buf+2) = c_ONE;  *(cur_buf+3) = c_ZERO;   // !1
    *(cur_buf+4) = c_ONE;  *(cur_buf+5) = c_ONE;    // !X (bottom)
    *(cur_buf+6) = c_ZERO; *(cur_buf+7) = c_ZERO;   // !T (top)
}

static void
free_ste_buffers()
{
    Free(weak_buf);
    Free(ant_buf);
    Free(cons_buf);
    Free(cur_buf);
    Free(next_buf);
    weak_buf = NULL;
    ant_buf = NULL;
    cons_buf = NULL;
    cur_buf = NULL;
    next_buf = NULL;
}

static void
simulation_break_handler()
{
    if( quit_simulation_early ) return;
    if( !gui_mode || use_stdout ) {
        FP(err_fp, "\n\n---- Simulation interrupted ----\n");
    }
    quit_simulation_early = TRUE;
}

static bool
stop_node_active(int idx)
{
    if( idx < 0 ) return FALSE;
    gbv Hcur = *(cur_buf+2*idx);
    gbv Lcur = *(cur_buf+2*idx+1);
    return( c_isT(Hcur, Lcur) );
}

static void
gSTE(g_ptr redex, value_type type)
{
    void    (*old_handler)();

    sop = Begin_string_ops();
    // STE opts fsm wl ant cons trl
    g_ptr g_opts, g_fsm, wl, ant, cons, trl, g_stop_node;
    nbr_errors_reported = 0;
    EXTRACT_7_ARGS(redex, g_opts, g_fsm, wl, ant, cons, trl, g_stop_node);
    string stop_node = GET_STRING(g_stop_node);
    string opts = GET_STRING(g_opts);
    bool trace_all = FALSE;
    print_failures = RCprint_failures;
    if( strstr(opts, "-s") != NULL )
	print_failures = FALSE;
    if( strstr(opts, "-e") != NULL )
	trace_all = TRUE;
    bool abort_ASAP = FALSE;
    if( strstr(opts, "-a") != NULL )
	abort_ASAP = TRUE;
    int abort_time = 99999999;
    //
    string mt = strstr(opts, "-m");
    if( mt != NULL ) {
	if( sscanf(mt, "-m %d", &abort_time) != 1 ) {
	    FP(warning_fp, "Unrecognized flag in STE call (");
	    FP(warning_fp, "-m should be followed by time). Ignored.\n");
	}
    }
    //
    BDD_size_limit = -1;
    string dw = strstr(opts, "-w");
    if( dw != NULL ) {
	if( type != use_bdds ) {
	    FP(err_fp, "Weakening (-w) currently only avaliable in BDD based ");
	    FP(err_fp, "STE. Flag ignored\n");
	} else if( sscanf(dw, "-w %d", &BDD_size_limit) != 1 ) {
	    FP(warning_fp, "Unrecognized flag in STE call (-w should be ");
	    FP(warning_fp,"followed by BDD size limit). Ignored.\n");
	}
    }
    information_flow_weakening = FALSE;
    if( strstr(opts, "-ifw") != NULL ) {
	if( type != use_bdds ) {
	    FP(err_fp, "Information flow weakening (-ifw) only avaliable ");
	    FP(err_fp, "in BDD based STE.  Flag ignored\n");
	} else {
	    information_flow_weakening = TRUE;
	}
    }


    fsm_ptr fsm = (fsm_ptr) GET_EXT_OBJ(g_fsm);
    push_fsm(fsm);
    ste_ptr ste = create_ste(fsm, type);
    ste->abort_ASAP = abort_ASAP;
    int stop_idx = -1;
    if( !STREQ(stop_node, s_ES) ) {
	stop_idx = name2idx(stop_node);
	if( stop_idx < 0 ) {
	    pop_ste();
	    pop_fsm();
	    End_string_ops(sop);
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", stop_node));
	    return;
	}
    }

    old_handler = signal(SIGINT, simulation_break_handler);
    quit_simulation_early = FALSE;

    //
    // Now translate weakenings, ant, cons and trl into events
    //
    bool ok = create_event_buffer(ste,event_bufp,wl,ant,cons,trl,abort_ASAP);
    //
    // Allocate value buffers
    //
    allocate_ste_buffers(fsm);
    //
    // Create event wheel buffers
    //
    buffer sim_wheel_buf;
    sim_wheel_bufp = &sim_wheel_buf;
    new_buf(sim_wheel_bufp, fsm->ranks, sizeof(buffer));
    for(int i = 0; i <= fsm->ranks; i++) {
	buffer b;
	new_buf(&b, 100, sizeof(ncomp_ptr));
	push_buf(sim_wheel_bufp, &b);
    }
    //
    // Make sure g.c. can see the object
    //
    MAKE_REDEX_EXT_OBJ(redex, ste_oidx, ste);
    if( !ok || quit_simulation_early ) {
	ste->max_time = 0;
	pop_fsm();
	End_string_ops(sop);
	signal(SIGINT, old_handler);
	return;
    }
    ste->active = TRUE;
    //
    // Perform constant propagation
    //
    bool old_RCverbose_ste_run = RCverbose_ste_run;
    RCverbose_ste_run = FALSE;
    if( !initialize(ste, trace_all) || quit_simulation_early ) {
	if( quit_simulation_early ) {
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Simulation interrupted"));
	} else {
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Initialization failed"));
	}
	free_ste_buffers();
	ste->max_time = 0;
	ste->active = FALSE;
	pop_fsm();
	End_string_ops(sop);
	signal(SIGINT, old_handler);
	return;
    }
    RCverbose_ste_run = old_RCverbose_ste_run;
 
    // Is there anything to run?
    if( COUNT_BUF(event_bufp) == 0 ) {
	MAKE_REDEX_EXT_OBJ(redex, ste_oidx, ste);
	ste->max_time = 0;
	free_ste_buffers();
	ste->active = FALSE;
	End_string_ops(sop);
	pop_fsm();
	signal(SIGINT, old_handler);
	return;
    }
    //
    // If yes, run the real STE algorithm
    //
    event_ptr *epp = M_LOCATE_BUF(event_bufp, COUNT_BUF(event_bufp)-1);
    event_ptr ep = *epp;
    int min_time = ep->time;
    epp = FAST_LOC_BUF(event_bufp, 0);
    ep = *epp;
    int max_time = ep->time+1;

    if( RCprint_time ) {
	if( gui_mode ) {
            Sprintf(buf, "simulation_start %d %d", min_time, max_time);
            Info_to_tcl(buf);
	} else {
	    FP(bdd_gc_fp, "\nStart simulation:\n");
	}
    }
    for(int t = min_time; t <= max_time; t++) {
	ste->cur_time = t;
	if( t > abort_time || quit_simulation_early ) {
	    if( gui_mode ) {
		Sprintf(buf, "simulation_end");
		Info_to_tcl(buf);
	    }
	    FP(warning_fp, "Simulation interrupted at time %d\n", abort_time);
	    ste->active = FALSE;
	    signal(SIGINT, old_handler);
	    return;
	}
	if( RCprint_time ) {
	    if( gui_mode ) {
		Sprintf(buf, "simulation_update %d", t);
		Info_to_tcl(buf);
	    } else {
		if( (t % RCprint_time_mod) == 0 )
		    FP(sim_fp, "Time: %d\n", t);
	    }
	}
	if ( stop_node_active(stop_idx) ) {
	    ste->max_time = t;
	    free_ste_buffers();
	    ste->active = FALSE;
	    if( gui_mode ) {
		Sprintf(buf, "simulation_end");
		Info_to_tcl(buf);
	    }
	    signal(SIGINT, old_handler);
	    End_string_ops(sop);
	    pop_fsm();
	    return;
	}

	process_event(event_bufp, t);
	nnode_ptr np;
	// Put nodes whose weak/ant changed on evaluation list.
	int idx = 0;
	FOR_BUF(nodesp, nnode_rec, np) {
	    if( np->has_ant_or_weak_change ) {
		if( np->composite >= 0 ) {
		    ncomp_ptr c;
		    c = (ncomp_ptr) M_LOCATE_BUF(compositesp,np->composite);
		    add_op_todo(c);
		} else if( np->composite == -1 ) {
		    // Input node
		    update_node(ste, idx, c_ONE, c_ONE);
		}
		np->has_ant_or_weak_change = FALSE;
	    }
	    idx++;
	}
	do_combinational(ste);
	// Do consequents and traces
	idx = 0;
	FOR_BUF(nodesp, nnode_rec, np) {
	    if( np->has_cons ) {
		gbv curH = *(cur_buf+2*idx);
		gbv curL = *(cur_buf+2*idx+1);
		gbv chkH = *(cons_buf+2*idx);
		gbv chkL = *(cons_buf+2*idx+1);
		if( (type != use_bexprs) && RCnotify_check_failures &&
		    (nbr_errors_reported < RCmax_nbr_errors) )
		{
		    gbv ok = c_AND(c_OR(chkH,c_NOT(curH)),
				   c_OR(chkL,c_NOT(curL)));
		    if( c_NEQ(ok, c_ONE) ) {
			FP(err_fp, "Warning: Consequent failure at time %d", t);
			FP(err_fp, " on node %s\n", idx2name(idx));
			if( print_failures ) {
			    FP(err_fp, "Current value:");
			    cHL_Print(err_fp, curH, curL);
			    FP(err_fp, "\nExpected value:");
			    cHL_Print(err_fp, chkH, chkL);
			    /* Print a counter example */
			    gbv bad = c_AND(
					c_OR(c_AND(chkH,c_NOT(curH)),
					     c_AND(c_NOT(chkH),curH)),
					c_OR(c_AND(chkL,c_NOT(curL)),
					     c_AND(c_NOT(chkL),curL)));
			    if( c_NEQ(bad,c_ZERO) ) {
				FP(err_fp, "\nStrong disagreement when: ");
				c_Print(err_fp, bad, -1);
			    } else {
				FP(err_fp, "\nWeak disagreement when:");
				gbv bad = c_OR(c_AND(c_NOT(chkH), curH),
					       c_AND(c_NOT(chkL), curL));
				c_Print(err_fp, bad, -1);
			    }
			    FP(err_fp, "\n");
			}
			nbr_errors_reported++;
			FP(err_fp, "\n");
			if( abort_ASAP ) {
			    gbv v = ste->checkTrajectory;
			    v = c_AND(v, c_OR(c_NOT(curH), chkH));
			    v = c_AND(v, c_OR(c_NOT(curL), chkL));
			    ste->checkTrajectory = v;
			    if( gui_mode ) {
				Sprintf(buf, "simulation_end");
				Info_to_tcl(buf);
			    }
			    ste->active = FALSE;
			    ste->max_time = t;
			    signal(SIGINT, old_handler);
			    return;
			}
		    }
		}
		gbv v = ste->checkTrajectory;
		v = c_AND(v, c_OR(c_NOT(curH), chkH));
		v = c_AND(v, c_OR(c_NOT(curL), chkL));
		ste->checkTrajectory = v;
	    }
	    if( trace_all || np->has_trace ) {
		gbv curH = *(cur_buf+2*idx);
		gbv curL = *(cur_buf+2*idx+1);
		record_trace(idx, t, curH, curL);
	    }
	    idx++;
	}
	do_phase(ste);
	if( Do_gc_asap )
	    Garbage_collect();
    }
    ste->max_time = max_time;
    free_ste_buffers();
    ste->active = FALSE;
    if( gui_mode ) {
	Sprintf(buf, "simulation_end");
	Info_to_tcl(buf);
    }
    signal(SIGINT, old_handler);
    End_string_ops(sop);
    pop_fsm();
}

void
DBG_print_vec_ptr(vec_ptr vp) 
{
    for(vec_ptr p = vp; p != NULL; p = p->next) {
	if( p->type == TXT ) {
	    FP(err_fp, "%s", p->u.name);
	} else {
	    range_ptr r = p->u.ranges;
	    while( r != NULL ) {
		FP(err_fp, "<%d-%d>", r->upper, r->lower);
		r = r->next;
	    }
	}
    }
    FP(err_fp, "\n");
}

void
dbg_dump_buf(string msg, gbv *buf)
{
    FP(err_fp, "\n-----%s------\n ", msg);
    for(unint i = 0; i < COUNT_BUF(nodesp); i++) {
	FP(err_fp, "Node %s: ", idx2name(i));
	gbv H = *(buf+2*i);
	gbv L = *(buf+2*i+1);
	cHL_Print(err_fp, H, L);
	FP(err_fp, "\n");
    }
}

void
dbg_print_ilist(string msg, ilist_ptr ip)
{
    int cnt = 0;
    fprintf(stderr, "%s\n", msg);
    while( ip != NULL && cnt < 20 ) {
	fprintf(stderr, "    [%p] %d-->%d (%d)\n", ip,ip->from,ip->to,ip->size);
	cnt++;
	ip = ip->next;
    }
    if( cnt >= 20 ) {
	fprintf(stderr, "Too long list...\n");
    }
}

static void
base_print_ilist(ilist_ptr il)
{
    g_ptr nds = ilist2nds(il);
    g_ptr res = Merge_Vectors(nds, TRUE);
    while( !IS_NIL(res) ) {
	FP(err_fp, " %s", GET_STRING(GET_CONS_HD(res)));
	res = GET_CONS_TL(res);
    }
}

void
DBG_print_vec_info_ptr(vec_info_ptr vp)
{
    FP(err_fp, " vec_info_ptr:");
    while( vp != NULL ) {
	FP(err_fp, " %s", vp->local_name);
	vp = vp->next;
    }
    FP(err_fp, "\n");
}

void
DBG_print_ilist(ilist_ptr il)
{
    FP(err_fp, " ilist:");
    if( il == NULL ) { FP(err_fp, "<NULL>"); return; }
    base_print_ilist(il);
    FP(err_fp, "\n");
}

void
DBG_print_vis_io_ptr(vis_io_ptr vp)
{
    FP(err_fp, " vis_io_ptr:");
    while( vp ) {
	FP(err_fp, " %s [", vp->f_vec);
	base_print_ilist(vp->acts);
	FP(err_fp, "]");
	vp = vp->next;
    }
    FP(err_fp, "\n");
}

void
DBG_print_ints(g_ptr il)
{
    while( !IS_NIL(il) ) {
	FP(err_fp, " %s", GET_STRING(GET_CONS_HD(il)));
	il = GET_CONS_TL(il);
    }
    FP(err_fp, "\n");
}

void
DBG_pexlif(g_ptr pexlif)
{
    string pr_fn = wastrsave(stringsp, "pretty_pexlif");
    g_ptr fn = Make_VAR_leaf(pr_fn);
    fn = Find_Function(symb_tbl, fn);
    g_ptr res = Make_APPL_ND(fn, pexlif);
    INC_REFCNT(pexlif);
    Eval(res);
    Flush(stdout_fp);
}

void
dbg_print_sch_rec(sch_ptr sch, int indent)
{
    if( sch->children == NULL ) {
	FP(err_fp, "%*s(LEAF %s %s)\n", indent, "", sch->vec, sch->pfn);
    } else {
	FP(err_fp, "%*s(NODE %s %s\n", indent, "", sch->vec, sch->pfn);
	for(sch_list_ptr sl = sch->children; sl != NULL; sl = sl->next) {
	    dbg_print_sch_rec(sl->sch, indent+3);
	}
	FP(err_fp, "%*s)\n", indent, "");
    }
}

void
DBG_print_sch(string title, sch_ptr sch)
{
    FP(err_fp, "===================================================\n");
    FP(err_fp, "%s\n", title);
    FP(err_fp, "===================================================\n");
    dbg_print_sch_rec(sch, 0);
}

static void
fl_clean_pexlif_ios(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr p = GET_APPLY_RIGHT(redex);
    p = clean_pexlif_ios(p);
    OVERWRITE(redex, p);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static fsm_ptr
gen_pexlif2fsm(g_ptr p, g_ptr black_box_list, int max_depth)
{
    sop = Begin_string_ops();
    p = clean_pexlif_ios(p);
    //
    fsm_ptr fsm = create_fsm();
    push_fsm(fsm);
    new_buf(&attr_buf, 100, sizeof(g_ptr));
    //
    visualization_id = 0;
    undeclared_node_cnt = 0;
    new_mgr(&node_comp_pair_rec_mgr, sizeof(node_comp_pair_rec));
    create_hash(&node_comp_pair_tbl, 1000, ni_pair_hash, ni_pair_equ);
    create_hash(&idx_list_uniq_tbl, 100, idx_list_hash, idx_list_equ);
    create_hash(&bb_tbl, 1000, str_hash, str_equ);
    hash_record parent_tbl;
    create_hash(&parent_tbl, 2, str_hash, str_equ);
    if( black_box_list != NULL ) {
	for(g_ptr bbl = black_box_list; !IS_NIL(bbl); bbl = GET_CONS_TL(bbl)) {
	    string bb_inst = GET_STRING(GET_CONS_HD(bbl));
	    insert_hash(&bb_tbl, bb_inst, bb_inst);
	}
    }
    // Default nodes
    // 0    == constant 0
    // 1    == constant 1
    // 2    == constant X
    // 3    == constant top
    declare_vector(&parent_tbl, "", wastrsave(stringsp, "!0"),FALSE,NULL,s_ES);
    declare_vector(&parent_tbl, "", wastrsave(stringsp, "!1"),FALSE,NULL,s_ES);
    declare_vector(&parent_tbl, "", wastrsave(stringsp, "!X"),FALSE,NULL,s_ES);
    declare_vector(&parent_tbl, "", wastrsave(stringsp, "!T"),FALSE,NULL,s_ES);
    ihier_buf[0] = 0;
    if( traverse_pexlif(&parent_tbl, p, "", TRUE, 0, 1, max_depth) ) {
        fsm->top_name = get_top_name(p);
        fsm->ranks = rank_order();
        fsm->sha256_sig = compute_sha256_signature(fsm);
    } else {
	fsm = NULL;
    }
    qsort(START_BUF(propsp), COUNT_BUF(propsp), sizeof(string), nn_cmp);
    free_mgr(&node_comp_pair_rec_mgr);
    dispose_hash(&node_comp_pair_tbl, NULLFCN);
    dispose_hash(&parent_tbl, NULLFCN);
    dispose_hash(&idx_list_uniq_tbl, NULLFCN);
    dispose_hash(&bb_tbl, NULLFCN);
    free_buf(&attr_buf);
    pop_fsm();
    End_string_ops(sop);
    return fsm;
}


static int
nn_cmp(const void *pi, const void *pj)
{
    string *i = (string *) pi;
    string *j = (string *) pj;
    return( strcmp(*i, *j) );
}

static bool
is_draw_fub(vis_ptr vp)
{
    return( strstr(vp->pfn, "draw_fub ") != NULL );
}

static vis_ptr
get_vis_info_at_level(vis_list_ptr vlp, int level)
{
    if( level < 0 ) {
	// Find the lowest, non-fub version
	int least = 9999999;
	vis_ptr res = NULL;
	for( vis_list_ptr cp = vlp; cp != NULL; cp = cp->next ) {
	    if( !is_draw_fub(cp->vp) ) {
		if( cp->vp->draw_level < least ) {
		    least = cp->vp->draw_level;
		    res = cp->vp;
		}
	    }
	}
	return res;
    } else {
	while( vlp && vlp->vp->draw_level != level ) { vlp = vlp->next; }
	if( vlp == NULL ) return NULL;
	return vlp->vp;
    }
}

static string
value_type2string(value_type type)
{
    switch( type ) {
	case use_bdds:
	    return s_use_bdds;
	case use_bexprs:
	    return s_use_bexprs;
	case use_ints:
	    return s_use_ints;
	default:
	    DIE("Impossible");
    }
    DIE("Impossible");
}

static void
gen_get_trace(g_ptr redex, value_type type)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste, g_node;
    EXTRACT_2_ARGS(redex, g_ste, g_node);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    if( ste->type != type ) {
	string msg = Fail_pr("Asking for %s values when STE run created %ss",
			      value_type2string(type),
			      value_type2string(ste->type));
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    string node = GET_STRING(g_node);
    push_ste(ste);
    push_fsm(ste->fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_ste();
	pop_fsm();
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	return;
    }
    trace_ptr tp = (trace_ptr) find_hash(trace_tblp, INT2PTR(idx));
    if( tp == NULL ) {
	pop_ste();
	pop_fsm();
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not traced", node));
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    for(trace_event_ptr te = tp->events; te != NULL; te = te->next) {
	g_ptr event = Make_CONS_ND(Make_INT_leaf(te->time),
				   Make_CONS_ND(Make_GBV_leaf(te->H),
						Make_GBV_leaf(te->L)));
	APPEND1(tail, event);
    }
    pop_fsm();
    pop_ste();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
gen_limited_get_trace(g_ptr redex, value_type type)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste, g_node, g_min, g_max;
    EXTRACT_4_ARGS(redex, g_ste, g_min, g_max, g_node);
    int t_min = GET_INT(g_min);
    int t_max = GET_INT(g_max);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    if( ste->type != type ) {
	string msg = Fail_pr("Asking for %s values when STE run created %ss",
			      value_type2string(type),
			      value_type2string(ste->type));
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    string node = GET_STRING(g_node);
    push_ste(ste);
    push_fsm(ste->fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_ste();
	pop_fsm();
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in fsm", node));
	return;
    }
    trace_ptr tp = (trace_ptr) find_hash(trace_tblp, INT2PTR(idx));
    if( tp == NULL ) {
	pop_ste();
	pop_fsm();
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not traced", node));
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    bool first = TRUE;
    for(trace_event_ptr te = tp->events; te != NULL; te = te->next) {
	if( te->time > t_max ) { continue; }
	if( first && te->time < t_max ) {
	    g_ptr event = Make_CONS_ND(Make_INT_leaf(t_max),
				   Make_CONS_ND(Make_GBV_leaf(c_ONE),
						Make_GBV_leaf(c_ONE)));
	    APPEND1(tail, event);
	}
	first = FALSE;
	if( te->time < t_min ) {
	    g_ptr event = Make_CONS_ND(Make_INT_leaf(t_min),
				   Make_CONS_ND(Make_GBV_leaf(te->H),
						Make_GBV_leaf(te->L)));
	    APPEND1(tail, event);
	    break;
	}
	g_ptr event = Make_CONS_ND(Make_INT_leaf(te->time),
				   Make_CONS_ND(Make_GBV_leaf(te->H),
						Make_GBV_leaf(te->L)));
	APPEND1(tail, event);
    }
    pop_fsm();
    pop_ste();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
get_nonX_entries(g_ptr redex)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste, g_nodes, g_time;
    EXTRACT_3_ARGS(redex, g_ste, g_nodes, g_time);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    push_ste(ste);
    push_fsm(ste->fsm);
    int	time = GET_INT(g_time);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    while( !IS_NIL(g_nodes) ) {
	string node = GET_STRING(GET_CONS_HD(g_nodes));
	int idx = name2idx(node);
	if( idx < 0 ) {
	    pop_ste();
	    pop_fsm();
	    End_string_ops(sop);
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in ste-fsm", node));
	    return;
	}
	trace_ptr tp = (trace_ptr) find_hash(trace_tblp, INT2PTR(idx));
	if( tp == NULL ) {
	    pop_ste();
	    pop_fsm();
	    End_string_ops(sop);
	    MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not traced", node));
	    return;
	}
	for(trace_event_ptr te = tp->events; te != NULL; te = te->next) {
	    if( te->time <= time ) {
		if( !c_isX(te->H, te->L) ) {
		    APPEND1(tail, Make_STRING_leaf(node));
		}
		break;
	    }
	}
	g_nodes = GET_CONS_TL(g_nodes);
    }
    pop_fsm();
    pop_ste();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
gen_get_trace_val(g_ptr redex, value_type type)
{
    sop = Begin_string_ops();
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste, g_node, g_time;
    EXTRACT_3_ARGS(redex, g_ste, g_node, g_time);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    if( ste->type != type ) {
	string msg = Fail_pr("Asking for %s values when STE run created %ss",
			      value_type2string(type),
			      value_type2string(ste->type));
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, msg);
	return;
    }
    string node = GET_STRING(g_node);
    int	time = GET_INT(g_time);
    push_ste(ste);
    push_fsm(ste->fsm);
    int idx = name2idx(node);
    if( idx < 0 ) {
	pop_ste();
	pop_fsm();
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not in ste-fsm", node));
	return;
    }
    trace_ptr tp = (trace_ptr) find_hash(trace_tblp, INT2PTR(idx));
    if( tp == NULL ) {
	pop_ste();
	pop_fsm();
	End_string_ops(sop);
	MAKE_REDEX_FAILURE(redex, Fail_pr("Node %s not traced", node));
	return;
    }
    bool found = FALSE;
    for(trace_event_ptr te = tp->events; te != NULL; te = te->next) {
	if( te->time <= time ) {
	    found = TRUE;
	    MAKE_REDEX_CONS_ND(redex,Make_GBV_leaf(te->H),Make_GBV_leaf(te->L));
	    break;
	}
    }
    if( !found ) {
	MAKE_REDEX_CONS_ND(redex,Make_GBV_leaf(c_ONE),Make_GBV_leaf(c_ONE));
    }
    pop_fsm();
    pop_ste();
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    End_string_ops(sop);
}

static void
gen_get_ste_result(g_ptr redex, value_type type)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_ste, g_flag;
    EXTRACT_2_ARGS(redex, g_ste, g_flag);
    ste_ptr ste = (ste_ptr) GET_EXT_OBJ(g_ste);
    if( ste->type != type ) {   
        string msg = Fail_pr("Asking for %s value when STE run created %ss",
                              value_type2string(type),
                              value_type2string(ste->type));
        MAKE_REDEX_FAILURE(redex, msg);
        return;
    }
    string flag = GET_STRING(g_flag);
    gbv res;
    if( strcmp(flag, "antOK") == 0 ) {
	res = ste->assertion_OK;
    } else if( strcmp(flag, "trajOK") == 0 ) {
	res = ste->validTrajectory;
    } else if( strcmp(flag, "consOK") == 0 ) {
	res = ste->check_OK;
    } else if( strcmp(flag, "checkOK") == 0 ) {
	res = ste->checkTrajectory;
    } else if( strcmp(flag, "implies") == 0 ) {
	// Implication only
	res = c_OR(c_NOT(ste->assertion_OK),
                   c_OR(c_NOT(ste->validTrajectory),
                        c_AND(ste->checkTrajectory, ste->check_OK)));
    } else if( strcmp(flag, "strong") == 0 || strcmp(flag, "") == 0 ) {
	// All (normal)
	res = c_AND(ste->assertion_OK,
                   c_AND(ste->validTrajectory,
                        c_AND(ste->checkTrajectory, ste->check_OK)));
    } else {
        MAKE_REDEX_FAILURE(redex,
			   Fail_pr("Unknown type (%s) in (b)get_ste_result",
				    flag));
        return;
    }
    MAKE_REDEX_GBV(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
dbg_print_ils(pointer key, pointer data)
{
    (void) data;
    ilist_ptr il = (ilist_ptr) key;
    g_ptr nds = ilist2nds(il);
    g_ptr vecs = Merge_Vectors(nds, TRUE);
    string res = strtemp("");
    bool first = TRUE;
    while( !IS_NIL(vecs) ) {
	if( !first )
	    charappend(' ');
	first = FALSE;
	strappend(GET_STRING(GET_CONS_HD(vecs)));
	vecs = GET_CONS_TL(vecs);
    }
    FP(err_fp, "%s\n", res);
}

static void
dbg_print_stop_nds(pointer key, pointer data)
{
    (void) data;
    int id = PTR2INT(key);
    FP(err_fp, " %s", idx2name(id));
}
