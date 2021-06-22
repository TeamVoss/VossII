//------------------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*									                                          */
/* Original author: Carl-Johan Seger, 2019                                    */
/*									                                          */
/******************************************************************************/
#include "strings.h"
#include "graph.h"
#include "pexlif.h"

/******************************************************************************/
/*                               GLOBAL VARIABLES                             */
/******************************************************************************/

string s_PINST;
string s_P_HIER;
string s_P_LEAF;
string s_W_UPDATE_FN;
string s_W_PHASE_DELAY;
string s_W_X;
string s_W_CONST;
string s_W_NAMED_CONST;
string s_W_VAR;
string s_W_EXPLICIT_VAR;
string s_W_AND;
string s_W_OR;
string s_W_NOT;
string s_W_PRED;
string s_W_EQ;
string s_W_GR;
string s_W_ADD;
string s_W_SUB;
string s_W_MUL;
string s_W_DIV;
string s_W_MOD;
string s_W_SHL;
string s_W_SHR;
string s_W_ASHR;
string s_W_SX;
string s_W_ZX;
string s_W_ITE;
string s_W_SLICE;
string s_W_NAMED_SLICE;
string s_W_UPDATE_NAMED_SLICE;
string s_W_CAT;
string s_W_MEM_READ;
string s_W_MEM_WRITE;
string s_MEM;
// /
string s_no_instance;

// Global variables referenced -------------------------------------------------
extern str_mgr strings;

/******************************************************************************/
/*                              PRIVATE VARIABLES                             */
/******************************************************************************/
static char value_list_buf[1024];
// Global managers.
static ustr_mgr        lstrings;
static rec_mgr         vec_mgr;
static rec_mgr         rng_mgr;
// Folding/Un-folding construction.
static rec_mgr         vector_list_mgr;
static rec_mgr_ptr     vector_list_mgr_ptr;
static rec_mgr         vector_mgr;
static rec_mgr_ptr     vector_mgr_ptr;
static rec_mgr         range_mgr;
static rec_mgr_ptr     range_mgr_ptr;
static rec_mgr         sname_list_mgr;
static rec_mgr_ptr     sname_list_mgr_ptr;
static rec_mgr         fa_subst_mgr;
static rec_mgr_ptr     fa_subst_mgr_ptr;
// Adj-tbl construction.
static rec_mgr         adj_bkt_mgr;
static rec_mgr_ptr     adj_bkt_mgr_ptr;
static rec_mgr         adj_key_mgr;
static rec_mgr_ptr     adj_key_mgr_ptr;
static rec_mgr         adj_key_list_mgr;
static rec_mgr_ptr     adj_key_list_mgr_ptr;
static hash_record     inputs_tbl;
static hash_record_ptr inputs_tbl_ptr;
static hash_record     outputs_tbl;
static hash_record_ptr outputs_tbl_ptr;

// Debugging -------------------------------------------------------------------
#define DEBUG_PEX 0
#define debug_print(fmt, ...)                                                  \
        do { if (DEBUG_PEX) fprintf(stderr, "%s:%d:%s: " fmt, __FILE__,        \
                                __LINE__, __func__, __VA_ARGS__); } while (0)
#define debug_append(fmt, ...)                                                 \
        do { if (DEBUG_PEX) fprintf(stderr, "" fmt, __VA_ARGS__); } while (0)

// Forward definitions local functions -----------------------------------------
static void           new_adj_mem();
static void           rem_adj_mem();
static vec_ptr        split_vector(string name);
static void           record_vector_signatures(
                          adj_key_ptr *tail, unint index, string name,
                          bool input);
static void           mk_adj_tables(
                          adj_key_list_ptr *keys, unint *count, g_ptr p);
static void           new_fold_mem();
static void           rem_fold_mem();
static void           mk_formal_actuals_substitution(
                          hash_record_ptr tbl, g_ptr fa);
static sname_list_ptr subst_formal(hash_record_ptr tbl, g_ptr f);
static g_ptr          subst_fa_list(hash_record_ptr tbl, g_ptr fa_list);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

static void
new_adj_mem()
{
    adj_bkt_mgr_ptr = &adj_bkt_mgr;
    adj_key_mgr_ptr = &adj_key_mgr;
    adj_key_list_mgr_ptr = &adj_key_list_mgr;
    inputs_tbl_ptr = &inputs_tbl;
    outputs_tbl_ptr = &outputs_tbl;
    new_mgr(adj_bkt_mgr_ptr, sizeof(adj_bkt_rec));
    new_mgr(adj_key_mgr_ptr, sizeof(adj_key_rec));
    new_mgr(adj_key_list_mgr_ptr, sizeof(adj_key_list_rec));
    create_hash(inputs_tbl_ptr, 100, str_hash, str_equ);
    create_hash(outputs_tbl_ptr, 100, str_hash, str_equ);
}

static void
rem_adj_mem()
{
    free_mgr(adj_bkt_mgr_ptr);
    free_mgr(adj_key_mgr_ptr);
    free_mgr(adj_key_list_mgr_ptr);
    dispose_hash(inputs_tbl_ptr, NULLFCN);
    dispose_hash(outputs_tbl_ptr, NULLFCN);
    adj_bkt_mgr_ptr = NULL;
    adj_key_mgr_ptr = NULL;
    adj_key_list_mgr_ptr = NULL;
    inputs_tbl_ptr = NULL;
    outputs_tbl_ptr = NULL;
}

static vec_ptr
split_vector(string name)
{
    vec_ptr vp = Split_vector_name(&lstrings, &vec_mgr, &rng_mgr, name);
    for(vec_ptr p = vp; p != NULL; p = p->next) {
        if(p->type == TXT) {
            p->u.name = wastrsave(&strings, p->u.name);
        }
    }
    return vp;
}

static void
record_vector_signatures(adj_key_ptr *tail, unint index, string name, bool input)
{
    vec_ptr vec = split_vector(name);
    string key = Get_vector_signature(&lstrings, vec);
    // Record signature as we'll need it later again.
    adj_key_ptr n = (adj_key_ptr) new_rec(adj_key_mgr_ptr);
    n->name = name;
    n->signature = key;
    n->input = input;
    n->vec = vec;
    n->next = NULL;
    (*tail) = n;
    // Record vec. in tabel.
    hash_record_ptr tbl;
    if(input) {
        tbl = inputs_tbl_ptr;
    } else {
        tbl = outputs_tbl_ptr;
    }
    adj_bkt_ptr bkt = (adj_bkt_ptr) find_hash(tbl, key);
    if(bkt == NULL) {
        adj_bkt_ptr b = (adj_bkt_ptr) new_rec(adj_bkt_mgr_ptr);
        b->index = index;
        b->vec = vec;
        b->next = NULL;
        insert_hash(tbl, key, b);
    } else {
        // todo: why not record as head since order does not matter? 
        while(bkt->next != NULL) {
            bkt = bkt->next;
        }
        adj_bkt_ptr b = (adj_bkt_ptr) new_rec(adj_bkt_mgr_ptr);
        b->index = index;
        b->vec = vec;
        b->next = NULL;
        bkt->next = b;
    }
}

static void
mk_adj_tables(adj_key_list_ptr *keys, unint *count, g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        DIE("Bad input");
    }    
    if(is_P_LEAF(cont, &fns)) {
        DIE("Bad input");
    }
    if(is_P_HIER(cont, &children)) {
        string vec;
        adj_key_ptr key = NULL, *key_tail = &key;
        // Record vector names for parent's formals.
        // todo: count outputs as inputs for the environment and vice versa.
        FOREACH_FORMAL(vec, fa_inps) {
            record_vector_signatures(key_tail, 0, vec, FALSE);
            key_tail = &(*key_tail)->next;
        }
        FOREACH_FORMAL(vec, fa_outs) {
            record_vector_signatures(key_tail, 0, vec, TRUE);
            key_tail = &(*key_tail)->next;
        }
        adj_key_list_ptr key_lst = (adj_key_list_ptr) new_rec(adj_key_list_mgr_ptr);
        key_lst->key  = key;
        key_lst->next = NULL;
        *keys = key_lst;
        adj_key_list_ptr *key_lst_tail = &key_lst->next;
        // Record vector names for each child's actuals.
        *count = 1;
        g_ptr child, tmp;
        FOR_CONS(children, tmp, child) {
            if(!is_PINST(child, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
                DIE("Bad input");
            }
            adj_key_ptr key = NULL, *key_tail = &key;
            FOREACH_ACTUAL(vec, fa_inps) {
                record_vector_signatures(key_tail, *count, vec, TRUE);
                key_tail = &(*key_tail)->next;
            }
            FOREACH_ACTUAL(vec, fa_outs) {
                record_vector_signatures(key_tail, *count, vec, FALSE);
                key_tail = &(*key_tail)->next;
            }
            adj_key_list_ptr key_lst = (adj_key_list_ptr) new_rec(adj_key_list_mgr_ptr);
            key_lst->key  = key;
            key_lst->next = NULL;
            *key_lst_tail = key_lst;
            key_lst_tail = &key_lst->next;
            *count = *count + 1;
        }
        return;
    }
    DIE("Impossible");
}

// -----------------------------------------------------------------------------

static void
new_fold_mem()
{
    vector_list_mgr_ptr = &vector_list_mgr;
    vector_mgr_ptr = &vector_mgr;
    range_mgr_ptr = &range_mgr;
    sname_list_mgr_ptr = &sname_list_mgr;
    fa_subst_mgr_ptr = &fa_subst_mgr;
    new_mgr(vector_list_mgr_ptr, sizeof(vec_list_rec));
    new_mgr(vector_mgr_ptr, sizeof(vec_rec));
    new_mgr(range_mgr_ptr, sizeof(range_rec));
    new_mgr(sname_list_mgr_ptr, sizeof(sname_list_rec));
    new_mgr(fa_subst_mgr_ptr, sizeof(fa_subst_rec));
}

static void
rem_fold_mem()
{
    free_mgr(vector_list_mgr_ptr);
    free_mgr(vector_mgr_ptr);
    free_mgr(range_mgr_ptr);
    free_mgr(sname_list_mgr_ptr);
    free_mgr(fa_subst_mgr_ptr);
    vector_list_mgr_ptr = NULL;
    vector_mgr_ptr = NULL;
    range_mgr_ptr = NULL;
    sname_list_mgr_ptr = NULL;
    fa_subst_mgr_ptr = NULL;
}

static void
mk_formal_actuals_substitution(hash_record_ptr tbl, g_ptr fa)
{
    if(tbl == NULL) {
        DIE("Passed a null hash-table.");
    }
    if(IS_NIL(fa)) {
        return;
    }
    g_ptr li, lj, pair, actual;
    FOR_CONS(fa, li, pair) {
        g_ptr formal = GET_FST(pair);
        g_ptr actuals = GET_SND(pair);
        /* fprintf(stderr, "Building subst for %s.\n", GET_STRING(formal)); */
        // Expand each actual and collect its vectors.
        unint size = 0;
        vec_list_ptr act_exp = NULL, *tail = &act_exp;
        FOR_CONS(actuals, lj, actual) {
            vec_ptr vec =
                Split_vector_name(&lstrings, vector_mgr_ptr, range_mgr_ptr,
                                  GET_STRING(actual));
            vec_list_ptr exp =
                Expand_vector(vector_list_mgr_ptr, vector_mgr_ptr,
                              range_mgr_ptr, vec);
            /* fprintf(stderr, "> Expanding %s => ", GET_STRING(actual)); */
            /* EMIT_VEC_LIST(exp); */
            // /
            *tail = exp;
            while(exp->next != NULL) { exp = exp->next; size++; }
            tail = &exp->next;
        }
        /* fprintf(stderr, "> Final exp. list: "); */
        /* EMIT_VEC_LIST(act_exp); */
        sname_list_ptr x = Show_vectors(sname_list_mgr_ptr, act_exp, FALSE);
        /* fprintf(stderr, "mk_subst: %s => ", GET_STRING(formal)); */
        /* EMIT_STR_LIST(x); */
        // Expand formal and build a mapping of each vector to its corr. actual.
        string form_str = GET_STRING(formal);
        vec_ptr form_vec =
            Split_vector_name(&lstrings, vector_mgr_ptr, range_mgr_ptr,
                              form_str);
        vec_list_ptr form_exp =
            Expand_vector(vector_list_mgr_ptr, vector_mgr_ptr, range_mgr_ptr,
                          form_vec);
        hash_record subst;
        create_hash(&subst, size, vec_hash, vec_equ);
        while(act_exp != NULL) {
            insert_hash(&subst, form_exp->vec, act_exp->vec);
            act_exp  = act_exp->next;
            form_exp = form_exp->next;
        }
        // Record orig. formal/actual for quick lookup and exp. subst.
        fa_subst_ptr sub = (fa_subst_ptr) new_rec(fa_subst_mgr_ptr);
        sub->formal  = form_str;
        sub->actuals = x;
        sub->subst   = &subst;
        string form_sig = Get_vector_signature(&lstrings, form_vec);
        insert_hash(tbl, form_sig, sub);
    }
}

// todo: are vectors contig. or not?
static sname_list_ptr
subst_formal(hash_record_ptr tbl, g_ptr f)
{
    if(tbl == NULL) {
        DIE("Bad input");
    }
    if(!IS_STRING(f)) {
        DIE("Bad input");
    }
    string  form_str = GET_STRING(f);
    vec_ptr form_vec =
        Split_vector_name(&lstrings, vector_mgr_ptr, range_mgr_ptr, form_str);
    string  form_sig = Get_vector_signature(&lstrings, form_vec);
    fa_subst_ptr bkt = (fa_subst_ptr) find_hash(tbl, form_sig);
    /* fprintf(stderr, "> subst formal: %s\n", form_str); */
    if(bkt == NULL) {
        /* fprintf(stderr, "found no match!\n"); */
        return NULL;
    }
    if(str_equ(form_str, bkt->formal)) {
        sname_list_ptr m = bkt->actuals;
        /* fprintf(stderr, "found identical match: "); */
        /* EMIT_STR_LIST(m); */
        return m;
    }
    vec_list_ptr form_exp =
        Expand_vector(vector_list_mgr_ptr, vector_mgr_ptr, range_mgr_ptr,
                      form_vec);
    vec_list_ptr sub_exp  = NULL, *tail = &sub_exp;
    for(; form_exp != NULL; form_exp = form_exp->next) {
        vec_ptr key = form_exp->vec;
        vec_ptr val = find_hash(bkt->subst, key);
        if(val == NULL) {
            DIE("Bad index");
        }
        vec_list_ptr vlp = (vec_list_ptr) new_rec(vector_list_mgr_ptr);
        vlp->vec  = val;
        vlp->next = NULL;
        // /
        *tail = vlp;
        tail = &(vlp->next);
    }
    vec_list_ptr sub = Merge_Vectors_gen(vector_list_mgr_ptr, sub_exp);
    sname_list_ptr names = Show_vectors(sname_list_mgr_ptr, sub, FALSE);
    return names;
}

static g_ptr
subst_fa_list(hash_record_ptr tbl, g_ptr fa_list)
{
    g_ptr i, j, pair, actual;
    g_ptr res = Make_NIL(), res_tail = res;
    FOR_CONS(fa_list, i, pair) {
        g_ptr formal  = GET_FST(pair);
        g_ptr actuals = GET_SND(pair);
        g_ptr as = Make_NIL(), as_tail = as;
        FOR_CONS(actuals, j, actual) {
            sname_list_ptr names = subst_formal(tbl, actual);
            for(; names != NULL; names = names->next) {
                APPEND1(as_tail, Make_STRING_leaf(names->name));
            }
        }
        INC_REFCNT(formal);
        APPEND1(res_tail, Make_PAIR_ND(formal, as));
    }
    return res;
}

/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

string
get_top_name(g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, internals, content;
    string name;
    bool leaf;
    is_PINST(p,&name,&attrs,&leaf,&fa_inps,&fa_outs,&internals,&content);
    return name;
}

int
get_top_size(g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        return -1;
    }
    if(is_P_LEAF(cont, &fns)) {
        return 1;
    }
    if(is_P_HIER(cont, &children)) {
        return 1 + List_length(children);
    }
    return -1;
}

g_ptr
get_top_inst(g_ptr p, unint inst)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        return NULL;
    }
    if(is_P_LEAF(cont, &fns)) {
        if(inst == 0) {
            return p;
        } else {
            return NULL;
        }
    }
    if(is_P_HIER(cont, &children)) {
        if(inst == 0) {
            return p;
        } else if(inst < (unint) List_length(children)) {
            return List_element(p, inst-1);
        } else {
            return NULL;
        }
    }
    return NULL;
}

g_ptr
get_top_adjacencies(g_ptr p)
{
    debug_print("%p\n", (void *) p);
    new_adj_mem();
    // /
    unint count;
    adj_key_list_ptr vchild;
    mk_adj_tables(&vchild, &count, p);
    g_ptr res = Make_NIL(), res_tail = res;
    for(unint i = 0; vchild != NULL; i++, vchild = vchild->next) {
        g_ptr index = Make_INT_leaf(i);
        for(adj_key_ptr vfa = vchild->key; vfa != NULL; vfa = vfa->next) {
            vec_ptr vec = vfa->vec;
            string sig = vfa->signature;
            if(vfa->input) {
                for(adj_bkt_ptr adj = find_hash(outputs_tbl_ptr, sig);
                    adj != NULL;
                    adj = adj->next)
                {
                    if(Check_vector_overlap(vec, adj->vec)) {
                        APPEND1(res_tail,
                            Make_PAIR_ND(
                                Make_INT_leaf(adj->index),
                                index));
                        // /
                        debug_print("%s: %u <- %u\n", sig, i, adj->index);
                    }
                }
            } else {
                for(adj_bkt_ptr adj = find_hash(inputs_tbl_ptr, sig);
                    adj != NULL;
                    adj = adj->next)
                {
                    if(Check_vector_overlap(vec, adj->vec)) {
                        APPEND1(res_tail,
                            Make_PAIR_ND(
                                index,
                                Make_INT_leaf(adj->index)));
                        // /
                        debug_print("%s: %u -> %u\n", sig, i, adj->index);
                    }
                }
            }
        }
    }
    // /
    rem_adj_mem();
    return res;
}

// -----------------------------------------------------------------------------

#define IS_IN(ix, set)                                                         \
    (find_hash(&(set), (ix)) != NULL)

// note: keep non-SHA/FP attrs?
// note: SHA/FP added in FL wrapper.
g_ptr
fold_pexlif(g_ptr p, g_ptr ids, string name)
{
    new_fold_mem();
    new_adj_mem();
    // /
    // Hash-set for inputs/outputs/internals.
    hash_record inps_set, outs_set, ints_set;
    create_hash(&inps_set, 10, str_hash, str_equ);
    create_hash(&outs_set, 10, str_hash, str_equ);
    create_hash(&ints_set, 10, str_hash, str_equ);
    // Hash-set for 'ids' under fold.
    hash_record ids_set;
    create_hash(&ids_set, List_length(ids), int_hash, int_equ);
    for(; !IS_NIL(ids); ids = M_GET_CONS_TL(ids)) {
        insert_check_hash(&ids_set,
            INT2PTR(GET_INT(M_GET_CONS_HD(ids))),
            INT2PTR(TRUE));
    }
    // Record inter-p connections, skipping root (first child ID is '1').
    unint count, ix = 1;
    adj_key_list_ptr vchild;
    mk_adj_tables(&vchild, &count, p);
    for(vchild = vchild->next; vchild != NULL; vchild = vchild->next, ix++) {
        if(!IS_IN(INT2PTR(ix), ids_set)) {
            continue;
        }
        for(adj_key_ptr vfa = vchild->key; vfa != NULL; vfa = vfa->next) {
            vec_ptr vec = vfa->vec;
            string sig  = vfa->signature;
            string act  = vfa->name;
            hash_record_ptr fa, tbl;
            // reg. as input? check drivers, and vice versa.
            if(vfa->input) {
                tbl = outputs_tbl_ptr;
                fa  = &inps_set;
            } else {
                tbl = inputs_tbl_ptr;
                fa  = &outs_set;
            }
            for(adj_bkt_ptr adj = find_hash(tbl, sig); adj != NULL; adj = adj->next) {
                if(Check_vector_overlap(vec, adj->vec)) {
                    if(IS_IN(INT2PTR(adj->index), ids_set)) {
                        insert_check_hash(&ints_set, act, act);
                    } else {
                        insert_check_hash(fa, act, act);
                    }
                }
            }
        }
    }
    // Pick apart 'p' to get at its orig. (old) components.
    g_ptr old_name, old_leaf, old_attrs, old_fa_inps, old_fa_outs, old_inter,
          old_cont, old_children;
    destr_PINST(
        p, &old_name, &old_attrs, &old_leaf, &old_fa_inps, &old_fa_outs,
        &old_inter, &old_cont
    );
    is_P_HIER(old_cont, &old_children);
    // Children are split by 'ids' into new and folded.
    ix = 1;
    g_ptr fold_children = Make_NIL(), fold_children_tail = fold_children;
    g_ptr new_children  = Make_NIL(), new_children_tail = new_children;
    g_ptr li, item;
    FOR_CONS(old_children, li, item) {
        INC_REFCNT(item);
        if(IS_IN(INT2PTR(ix), ids_set)) {
            APPEND1(fold_children_tail, item);
        } else {
            APPEND1(new_children_tail, item);
        }
        ix++;
    }
    // Inputs and outputs doesn't change for new pexlif, but its internals might
    // move into folded pexlif if mentioned by ints set.
    g_ptr new_inter = Make_NIL(), new_inter_tail = new_inter;
    FOR_CONS(old_inter, li, item) {
        if(!IS_IN(GET_STRING(item), ints_set)) {
            INC_REFCNT(item);
            APPEND1(new_inter_tail, item);
        }
    }
    // Inputs/outputs/internals for fold are given by inps/outs/ints sets.
    // Note that any internal could be used as inp/outp elsewhere.
    g_ptr fold_fa_inps = Make_NIL(), fold_fa_inps_tail = fold_fa_inps;
    g_ptr fold_fa_outs = Make_NIL(), fold_fa_outs_tail = fold_fa_outs;
    g_ptr fold_inter   = Make_NIL(), fold_inter_tail   = fold_inter;
    g_ptr bk, bv;
    bucket_ptr *bpp;
    SCAN_HASH(&inps_set, bpp, bk, bv) {
        g_ptr act = Make_STRING_leaf((string) bv);
        g_ptr fa  = Make_PAIR_ND(act, Make_CONS_ND(act, Make_NIL()));
        INC_REFCNT(act);
        APPEND1(fold_fa_inps_tail, fa);
    }
    SCAN_HASH(&outs_set, bpp, bk, bv) {
        g_ptr act = Make_STRING_leaf((string) bv);
        g_ptr fa  = Make_PAIR_ND(act, Make_CONS_ND(act, Make_NIL()));
        INC_REFCNT(act);
        APPEND1(fold_fa_outs_tail, fa);
    }
    SCAN_HASH(&ints_set, bpp, bk, bv) {
        if(!IS_IN((string) bk, inps_set) && !IS_IN((string) bk, outs_set)) {
            g_ptr act = Make_STRING_leaf((string) bv);
            APPEND1(fold_inter_tail, act);
        }
    }
    // Finally, construct new pexlifs.
    INC_REFCNT(old_leaf);
    g_ptr fold_pinst = mk_PINST(
        Make_STRING_leaf(name), Make_NIL(), old_leaf, fold_fa_inps,
        fold_fa_outs, fold_inter, mk_P_HIER(fold_children)
    );
    INC_REFCNT(old_name);
    INC_REFCNT(old_leaf);
    INC_REFCNT(old_fa_inps);
    INC_REFCNT(old_fa_outs);
    g_ptr new_pinst = mk_PINST(
        old_name, Make_NIL(), old_leaf, old_fa_inps, old_fa_outs, new_inter,
        mk_P_HIER(Make_CONS_ND(fold_pinst, new_children))
    );
    // /
    rem_fold_mem();
    rem_adj_mem();
    return new_pinst;
}

g_ptr
unfold_pexlif(g_ptr p, unint id, string prefix)
{
    new_fold_mem();
    new_adj_mem();
    // /
    // Pick apart 'p' to get at its orig. (old) components.
    g_ptr old_name, old_leaf, old_attrs, old_fa_inps, old_fa_outs, old_inter,
          old_cont, old_children;
    destr_PINST(
        p, &old_name, &old_attrs, &old_leaf, &old_fa_inps, &old_fa_outs,
        &old_inter, &old_cont
    );
    is_P_HIER(old_cont, &old_children);
    // Pick out the to-be unfolded child and collect other children.
    unint ix = 1;
    g_ptr orig_children = Make_NIL(), orig_children_tail = orig_children;
    g_ptr unfolded;
    g_ptr li, item;
    FOR_CONS(old_children, li, item) {
        if(id == ix) {
            unfolded = item;
        } else {
            INC_REFCNT(item);
            APPEND1(orig_children_tail, item);
        }
        ix++;
    }
    // Pick apart 'unfolded' (un) to get at its components.
    g_ptr un_name, un_leaf, un_attrs, un_fa_inps, un_fa_outs, un_inter,
          un_cont, un_children;
    destr_PINST(
        unfolded, &un_name, &un_attrs, &un_leaf, &un_fa_inps, &un_fa_outs,
        &un_inter, &un_cont
    );
    is_P_HIER(un_cont, &un_children);
    // Build formal->actuals subst. for input/output/internals. Note that we can
    // build new list of internals while constructing the inter. subst.
    hash_record sub;
    create_hash(&sub, 100, str_hash, str_equ);
    if(IS_NIL(un_fa_inps) || IS_NIL(un_fa_outs)) {
        DIE("Pexlif to unfold has empty inputs/outputs.");
    }
    mk_formal_actuals_substitution(&sub, un_fa_inps);
    mk_formal_actuals_substitution(&sub, un_fa_outs);
    g_ptr new_inter = Make_NIL(), new_inter_tail = new_inter;
    g_ptr inter_sub = Make_NIL(), inter_sub_tail = inter_sub;
    // /
    tstr_ptr ts = new_temp_str_mgr();
    FOR_CONS(un_inter, li, item) {
        string old = GET_STRING(item);
        string buf = gen_strtemp(ts, prefix);
        gen_strappend(ts, old);
        string new = wastrsave(&strings, buf);
        g_ptr newg = Make_STRING_leaf(new);
        INC_REFCNT(newg);
        APPEND1(new_inter_tail, newg);
        APPEND1(inter_sub_tail,
                Make_PAIR_ND(Make_STRING_leaf(old), Make_SINGLETON(newg)));
    }
    free_temp_str_mgr(ts);
    mk_formal_actuals_substitution(&sub, inter_sub);
    APPENDL(new_inter_tail, old_inter);
    // Apply subst. to each grandchild's actuals.
    g_ptr new_children = Make_NIL(), new_children_tail = new_children;
    FOR_CONS(un_children, li, item) {
        g_ptr g_name, g_leaf, g_attrs, g_fa_inps, g_fa_outs, g_inter, g_cont;
        destr_PINST(item, &g_name, &g_attrs, &g_leaf, &g_fa_inps, &g_fa_outs,
                    &g_inter, &g_cont);
        g_ptr n_fa_inps = subst_fa_list(&sub, g_fa_inps);
        g_ptr n_fa_outs = subst_fa_list(&sub, g_fa_outs);
        INC_REFCNT(g_name);
        INC_REFCNT(g_leaf);
        INC_REFCNT(g_attrs);
        INC_REFCNT(g_inter);
        INC_REFCNT(g_cont);
        g_ptr n_pinst =
            mk_PINST(g_name, g_attrs, g_leaf, n_fa_inps, n_fa_outs, g_inter,
                     g_cont);
        APPEND1(new_children_tail, n_pinst);
    }
    APPENDL(new_children_tail, orig_children);
    // Finally, construct new pexlif.
    INC_REFCNT(old_name);
    INC_REFCNT(old_leaf);
    INC_REFCNT(old_fa_inps);
    INC_REFCNT(old_fa_outs);
    g_ptr new_pinst =
        mk_PINST(old_name, Make_NIL(), old_leaf, old_fa_inps, old_fa_outs,
                 new_inter, mk_P_HIER(new_children));
    // /
    rem_fold_mem();
    rem_adj_mem();
    return new_pinst;
}

string
find_value_list(g_ptr attrs, string name)
{
    sprintf(value_list_buf, "node_values_%s", name);
    while( !IS_NIL(attrs) ) {
	g_ptr key = GET_CONS_HD(GET_CONS_HD(attrs));
	if( strcmp(value_list_buf, GET_STRING(key)) == 0 ) {
	    return( GET_STRING(GET_CONS_TL(GET_CONS_HD(attrs))) );
	}
	attrs = GET_CONS_TL(attrs);
    }
    return NULL;
}

string
find_instance_name(g_ptr attrs)
{
    while( !IS_NIL(attrs) ) {
        g_ptr key = GET_CONS_HD(GET_CONS_HD(attrs));
        if( strcmp(GET_STRING(key), "instance") == 0 ) {
            return( GET_STRING(GET_CONS_TL(GET_CONS_HD(attrs))) );
        }
        attrs = GET_CONS_TL(attrs);
    }
    return s_no_instance;        
}

// Destr. Functions ------------------------------------------------------------

void
destr_PINST(g_ptr node,
	 g_ptr  *namep,
	 g_ptr  *attrsp,
	 g_ptr  *leafp,
	 g_ptr  *fa_inpsp,
	 g_ptr  *fa_outsp,
	 g_ptr  *internalsp,
	 g_ptr  *contentp)
{
    DEST_GET(*contentp)
    DEST_GET(*internalsp)
    DEST_GET(*fa_outsp)
    DEST_GET(*fa_inpsp)
    DEST_GET(*leafp)
    DEST_GET(*attrsp)
    DEST_GET(*namep)
    ASSERT( IS_CONSTRUCTOR(PINST, node) );
}

bool
destr_MEM(g_ptr node, int *a_szp, int *linesp, int *d_szp)
{
    g_ptr a_sz, lines, d_sz;
    EXTRACT(d_sz);
    EXTRACT(lines);
    EXTRACT(a_sz);
    if( !IS_CONSTRUCTOR(MEM, node) ) { return FALSE; }
    *a_szp = GET_INT(a_sz);
    *d_szp = GET_INT(d_sz);
    *linesp = GET_INT(lines);
    return TRUE;
}

// Is. Functions ---------------------------------------------------------------

bool
is_PINST(g_ptr node,
	 string *namep,
	 g_ptr  *attrsp,
	 bool   *leafp,
	 g_ptr  *fa_inpsp,
	 g_ptr  *fa_outsp,
	 g_ptr  *internalsp,
	 g_ptr  *contentp)
{
    g_ptr gname, gleaf;
    EXTRACT(*contentp)
    EXTRACT(*internalsp)
    EXTRACT(*fa_outsp)
    EXTRACT(*fa_inpsp)
    EXTRACT(gleaf)
    EXTRACT(*attrsp)
    EXTRACT(gname)
    if( !IS_CONSTRUCTOR(PINST, node) ) { return FALSE; }
    ASSERT( IS_LEAF(gname) && IS_STRING(gname) );
    *namep = GET_STRING(gname);
    ASSERT( IS_LEAF(gleaf) && IS_BOOL(gleaf) );
    *leafp = (GET_BOOL(gleaf) == B_One())? TRUE : FALSE;
    return TRUE;
}

bool
is_P_HIER(g_ptr node, g_ptr *childrenp)
{
    EXTRACT(*childrenp)
    if( !IS_CONSTRUCTOR(P_HIER, node) ) { return FALSE; }
    return TRUE;
}

bool
is_P_LEAF(g_ptr node, g_ptr *fnsp)
{
    EXTRACT(*fnsp)
    if( !IS_CONSTRUCTOR(P_LEAF, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_UPDATE_FN(g_ptr node, g_ptr *lhsp, g_ptr *rhsp)
{
    EXTRACT(*rhsp)
    EXTRACT(*lhsp)
    if( !IS_CONSTRUCTOR(W_UPDATE_FN, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_PHASE_DELAY(g_ptr node, g_ptr *lhsp, g_ptr *rhsp)
{
    EXTRACT(*rhsp)
    EXTRACT(*lhsp)
    if( !IS_CONSTRUCTOR(W_PHASE_DELAY, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_NOT(g_ptr node, g_ptr *subp)
{
    EXTRACT(*subp);
    if( !IS_CONSTRUCTOR(W_NOT, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_PRED(g_ptr node, string *namep, g_ptr *subp)
{
    g_ptr nm;
    EXTRACT(*subp);
    EXTRACT(nm);
    if( !IS_CONSTRUCTOR(W_PRED, node) ) { return FALSE; }
    ASSERT( IS_LEAF(nm) && IS_STRING(nm) );
    *namep = GET_STRING(nm);
    return TRUE;
}

bool
is_W_X(g_ptr node, int *szp)
{
    g_ptr size;
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_X, node) ) { return FALSE; }
    *szp = GET_INT(size);
    return TRUE;
}

bool
is_W_CONST(g_ptr node, int *szp, arbi_T *valp)
{
    g_ptr value, size;
    EXTRACT(value);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_CONST, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *valp = GET_AINT(value);
    return TRUE;
}

bool
is_W_NAMED_CONST(g_ptr node, string *namep, int *szp, arbi_T *valp)
{
    g_ptr value, size, name;
    EXTRACT(value);
    EXTRACT(size);
    EXTRACT(name);
    if( !IS_CONSTRUCTOR(W_NAMED_CONST, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *valp = GET_AINT(value);
    *namep = GET_STRING(name);
    return TRUE;
}

bool
is_W_VAR(g_ptr node, int *szp, string *basep)
{
    g_ptr base, size;
    EXTRACT(base);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_VAR, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *basep = GET_STRING(base);
    return TRUE;
}

bool
is_W_EXPLICIT_VAR(g_ptr node, int *szp, string *namep)
{
    g_ptr name, size;
    EXTRACT(name);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_EXPLICIT_VAR, node) ) { return FALSE; }
    *szp = GET_INT(size);
    *namep = GET_STRING(name);
    return TRUE;
}

bool
is_W_SX(g_ptr node, int *szp, g_ptr *ep)
{
    g_ptr size;
    EXTRACT(*ep);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_SX, node) ) { return FALSE; }
    *szp = GET_INT(size);
    return TRUE;
}

bool
is_W_ZX(g_ptr node, int *szp, g_ptr *ep)
{
    g_ptr size;
    EXTRACT(*ep);
    EXTRACT(size);
    if( !IS_CONSTRUCTOR(W_ZX, node) ) { return FALSE; }
    *szp = GET_INT(size);
    return TRUE;
}

bool
is_W_ITE(g_ptr node, g_ptr *condp, g_ptr *tp, g_ptr *ep)
{
    EXTRACT(*ep);
    EXTRACT(*tp);
    EXTRACT(*condp);
    if( !IS_CONSTRUCTOR(W_ITE, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_SLICE(g_ptr node, g_ptr *idxlistp, g_ptr *ep)
{
    EXTRACT(*ep);
    EXTRACT(*idxlistp);
    if( !IS_CONSTRUCTOR(W_SLICE, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_NAMED_SLICE(g_ptr node, string *namep, g_ptr *idxlistp, g_ptr *ep)
{
    g_ptr nm;
    EXTRACT(*ep);
    EXTRACT(*idxlistp);
    EXTRACT(nm);
    if( !IS_CONSTRUCTOR(W_NAMED_SLICE, node) ) { return FALSE; }
    *namep = GET_STRING(nm);
    return TRUE;
}

bool
is_W_UPDATE_NAMED_SLICE(g_ptr node, g_ptr *bp, string *namep, g_ptr *idxlistp, g_ptr *ep)
{
    g_ptr nm;
    EXTRACT(*ep);
    EXTRACT(*idxlistp);
    EXTRACT(nm);
    EXTRACT(*bp);
    if( !IS_CONSTRUCTOR(W_UPDATE_NAMED_SLICE, node) ) { return FALSE; }
    *namep = GET_STRING(nm);
    return TRUE;
}

bool
is_W_CAT(g_ptr node, g_ptr *listp)
{
    EXTRACT(*listp);
    if( !IS_CONSTRUCTOR(W_CAT, node) ) { return FALSE; }
    return TRUE;
}

bool
is_W_MEM_READ(g_ptr node, int *a_szp, int *linesp, int *d_szp, g_ptr *memp, g_ptr *addrp)
{
    g_ptr info;
    EXTRACT(*addrp);
    EXTRACT(*memp);
    EXTRACT(info);
    if( !IS_CONSTRUCTOR(W_MEM_READ, node) ) { return FALSE; }
    if( !destr_MEM(info, a_szp, linesp, d_szp) ) { return FALSE; }
    return TRUE;
}

bool
is_W_MEM_WRITE(g_ptr node, int *a_szp, int *linesp, int *d_szp, g_ptr *memp, g_ptr *addrp, g_ptr *datap)
{
    g_ptr info;
    EXTRACT(*datap);
    EXTRACT(*addrp);
    EXTRACT(*memp);
    EXTRACT(info);
    if( !IS_CONSTRUCTOR(W_MEM_WRITE, node) ) { return FALSE; }
    if( !destr_MEM(info, a_szp, linesp, d_szp) ) { return FALSE; }
    return TRUE;
}

// Mk. Functions ---------------------------------------------------------------

g_ptr
mk_PINST(g_ptr name, g_ptr attrs, g_ptr leaf, g_ptr fa_inps, g_ptr fa_outs, g_ptr internals, g_ptr content)
{
    g_ptr res = Make_STRING_leaf(s_PINST);
    res = Make_CONS_ND(res, name);
    res = Make_CONS_ND(res, attrs);
    res = Make_CONS_ND(res, leaf);
    res = Make_CONS_ND(res, fa_inps);
    res = Make_CONS_ND(res, fa_outs);
    res = Make_CONS_ND(res, internals);
    res = Make_CONS_ND(res, content);
    return res;
}

g_ptr
mk_P_HIER(g_ptr children)
{
    g_ptr res = Make_STRING_leaf(s_P_HIER);
    res = Make_CONS_ND(res, children);
    return res;
}


g_ptr
mk_P_LEAF(g_ptr fns)
{
    g_ptr res = Make_STRING_leaf(s_P_LEAF);
    res = Make_CONS_ND(res, fns);
    return res;
}

g_ptr
mk_W_UPDATE_FN(g_ptr lhs, g_ptr rhs)
{
    g_ptr res = Make_STRING_leaf(s_W_UPDATE_FN);
    res = Make_CONS_ND(res, lhs);
    res = Make_CONS_ND(res, rhs);
    return res;
}

g_ptr
mk_W_PHASE_DELAY(g_ptr lhs, g_ptr rhs)
{
    g_ptr res = Make_STRING_leaf(s_W_PHASE_DELAY);
    res = Make_CONS_ND(res, lhs);
    res = Make_CONS_ND(res, rhs);
    return res;
}

g_ptr
mk_W_X(g_ptr sz)
{
    g_ptr res = Make_STRING_leaf(s_W_X);
    res = Make_CONS_ND(res, sz);
    return res;
}


g_ptr
mk_W_CONST(g_ptr sz, g_ptr v)
{
    g_ptr res = Make_STRING_leaf(s_W_CONST);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, v);
    return res;
}

g_ptr
mk_W_NAMED_CONST(g_ptr name, g_ptr sz, g_ptr v)
{
    g_ptr res = Make_STRING_leaf(s_W_NAMED_CONST);
    res = Make_CONS_ND(res, name);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, v);
    return res;
}

g_ptr
mk_W_VAR(g_ptr sz, g_ptr base)
{
    g_ptr res = Make_STRING_leaf(s_W_VAR);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, base);
    return res;
}

g_ptr
mk_W_EXPLICIT_VAR(g_ptr sz, g_ptr name)
{
    g_ptr res = Make_STRING_leaf(s_W_EXPLICIT_VAR);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, name);
    return res;
}

g_ptr
mk_W_AND(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_AND);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_OR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_OR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_NOT(g_ptr a)
{
    g_ptr res = Make_STRING_leaf(s_W_NOT);
    res = Make_CONS_ND(res, a);
    return res;
}

g_ptr
mk_W_EQ(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_EQ);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_PRED(g_ptr name, g_ptr cond)
{
    g_ptr res = Make_STRING_leaf(s_W_PRED);
    res = Make_CONS_ND(res, name);
    res = Make_CONS_ND(res, cond);
    return res;
}

g_ptr
mk_W_GR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_GR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_ADD(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_ADD);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SUB(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_SUB);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_MUL(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_MUL);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_DIV(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_DIV);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_MOD(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_MOD);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SHL(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_SHL);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SHR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_SHR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_ASHR(g_ptr a, g_ptr b)
{
    g_ptr res = Make_STRING_leaf(s_W_ASHR);
    res = Make_CONS_ND(res, a);
    res = Make_CONS_ND(res, b);
    return res;
}

g_ptr
mk_W_SX(g_ptr sz, g_ptr w)
{
    g_ptr res = Make_STRING_leaf(s_W_SX);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, w);
    return res;
}

g_ptr
mk_W_ZX(g_ptr sz, g_ptr w)
{
    g_ptr res = Make_STRING_leaf(s_W_ZX);
    res = Make_CONS_ND(res, sz);
    res = Make_CONS_ND(res, w);
    return res;
}

g_ptr
mk_W_ITE(g_ptr cond, g_ptr t, g_ptr e)
{
    g_ptr res = Make_STRING_leaf(s_W_ITE);
    res = Make_CONS_ND(res, cond);
    res = Make_CONS_ND(res, t);
    res = Make_CONS_ND(res, e);
    return res;
}

g_ptr
mk_W_SLICE(g_ptr indices, g_ptr w)
{
    g_ptr res = Make_STRING_leaf(s_W_SLICE);
    res = Make_CONS_ND(res, indices);
    res = Make_CONS_ND(res, w);
    return res;
}

g_ptr
mk_W_CAT(g_ptr parts)
{
    g_ptr res = Make_STRING_leaf(s_W_CAT);
    res = Make_CONS_ND(res, parts);
    return res;
}

g_ptr
mk_MEM(g_ptr addr_size, g_ptr lines, g_ptr data_size)
{
    g_ptr res = Make_STRING_leaf(s_MEM);
    res = Make_CONS_ND(res, addr_size);
    res = Make_CONS_ND(res, lines);
    res = Make_CONS_ND(res, data_size);
    return res;
}

g_ptr
mk_W_MEM_READ(g_ptr info, g_ptr mem, g_ptr addr)
{
    g_ptr res = Make_STRING_leaf(s_W_MEM_READ);
    res = Make_CONS_ND(res, info);
    res = Make_CONS_ND(res, mem);
    res = Make_CONS_ND(res, addr);
    return res;
}

g_ptr
mk_W_MEM_WRITE(g_ptr info, g_ptr mem, g_ptr addr, g_ptr data)
{
    g_ptr res = Make_STRING_leaf(s_W_MEM_WRITE);
    res = Make_CONS_ND(res, info);
    res = Make_CONS_ND(res, mem);
    res = Make_CONS_ND(res, addr);
    res = Make_CONS_ND(res, data);
    return res;
}

// ? ---------------------------------------------------------------------------

static void
fold_pexlif_fn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_pex, g_ids, g_name;
    // /
    EXTRACT_3_ARGS(redex, g_pex, g_ids, g_name);
    g_ptr fold = fold_pexlif(g_pex, g_ids, GET_STRING(g_name));
    OVERWRITE(redex, fold);
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
unfold_pexlif_fn(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_pex, g_id, g_prefix;
    // /
    EXTRACT_3_ARGS(redex, g_pex, g_id, g_prefix);
    g_ptr unfold = unfold_pexlif(g_pex, GET_INT(g_id), GET_STRING(g_prefix));
    OVERWRITE(redex, unfold);
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

void
Pexlif_Init()
{
    s_PINST                = Mk_constructor_name("PINST");
    s_P_HIER               = Mk_constructor_name("P_HIER");
    s_P_LEAF               = Mk_constructor_name("P_LEAF");
    s_W_UPDATE_FN          = Mk_constructor_name("W_UPDATE_FN");
    s_W_PHASE_DELAY        = Mk_constructor_name("W_PHASE_DELAY");
    s_W_X                  = Mk_constructor_name("W_X");
    s_W_CONST              = Mk_constructor_name("W_CONST");
    s_W_NAMED_CONST        = Mk_constructor_name("W_NAMED_CONST");
    s_W_VAR                = Mk_constructor_name("W_VAR");
    s_W_EXPLICIT_VAR       = Mk_constructor_name("W_EXPLICIT_VAR");
    s_W_AND                = Mk_constructor_name("W_AND");
    s_W_OR                 = Mk_constructor_name("W_OR");
    s_W_NOT                = Mk_constructor_name("W_NOT");
    s_W_PRED               = Mk_constructor_name("W_PRED");
    s_W_EQ                 = Mk_constructor_name("W_EQ");
    s_W_GR                 = Mk_constructor_name("W_GR");
    s_W_ADD                = Mk_constructor_name("W_ADD");
    s_W_SUB                = Mk_constructor_name("W_SUB");
    s_W_MUL                = Mk_constructor_name("W_MUL");
    s_W_DIV                = Mk_constructor_name("W_DIV");
    s_W_MOD                = Mk_constructor_name("W_MOD");
    s_W_SHL                = Mk_constructor_name("W_SHL");
    s_W_SHR                = Mk_constructor_name("W_SHR");
    s_W_ASHR               = Mk_constructor_name("W_ASHR");
    s_W_SX                 = Mk_constructor_name("W_SX");
    s_W_ZX                 = Mk_constructor_name("W_ZX");
    s_W_ITE                = Mk_constructor_name("W_ITE");
    s_W_SLICE              = Mk_constructor_name("W_SLICE");
    s_W_NAMED_SLICE        = Mk_constructor_name("W_NAMED_SLICE");
    s_W_UPDATE_NAMED_SLICE = Mk_constructor_name("W_UPDATE_NAMED_SLICE");
    s_W_CAT                = Mk_constructor_name("W_CAT");
    s_W_MEM_READ           = Mk_constructor_name("W_MEM_READ");
    s_W_MEM_WRITE          = Mk_constructor_name("W_MEM_WRITE");
    s_MEM                  = Mk_constructor_name("MEM");
    // /
    s_no_instance = wastrsave(&strings, "{}");
    // /
    new_ustrmgr(&lstrings);
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
}

void
Pexlif_Install_Functions()
{
    typeExp_ptr pexlif_tp = Get_Type("pexlif", NULL, TP_INSERT_PLACE_HOLDER);
    Add_ExtAPI_Function(
          "fold_pexlif"
        , "111"
        , FALSE
        , GLmake_arrow(
              pexlif_tp
            , GLmake_arrow(
                  GLmake_list(GLmake_int())
                , GLmake_arrow(
                      GLmake_string()
                    , pexlif_tp)))
        , fold_pexlif_fn
    );
    Add_ExtAPI_Function(
          "unfold_pexlif"
        , "111"
        , FALSE
        , GLmake_arrow(
              pexlif_tp
            , GLmake_arrow(
                  GLmake_int()
                , GLmake_arrow(
                      GLmake_string()
                    , pexlif_tp)))
        , unfold_pexlif_fn
    );
}

/******************************************************************************/
/*                               DUMMY FUNCTIONS                              */
/******************************************************************************/
