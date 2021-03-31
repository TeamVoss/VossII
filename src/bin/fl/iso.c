//------------------------------------------------------------------------------
// Copyright 2020 Markus Aronsson
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/*     Original author: Markus Aronsson, 2020                                 */
/*                                                                            */
/******************************************************************************/
#include <time.h> // for testing...
#include "strings.h"
#include "buf.h"
#include "graph.h"
#include "iso.h"

/******************************************************************************/
/*                               GLOBAL VARIABLES                             */
/******************************************************************************/
// ...

// Global variables referenced -------------------------------------------------
extern str_mgr strings;
extern g_ptr void_nd;

/******************************************************************************/
/*                              PRIVATE VARIABLES                             */
/******************************************************************************/
static ustr_mgr        lstrings;
static rec_mgr         vec_mgr;
static rec_mgr         rng_mgr;
static rec_mgr         mat_mgr;
// Adj. mat. construction.
static rec_mgr         bkt_mgr;
static rec_mgr_ptr     bkt_mgr_ptr;
static rec_mgr         key_mgr;
static rec_mgr_ptr     key_mgr_ptr;
static rec_mgr         key_lst_mgr;
static rec_mgr_ptr     key_lst_mgr_ptr;
static hash_record     tbl_in;
static hash_record_ptr tbl_in_ptr;
static hash_record     tbl_out;
static hash_record_ptr tbl_out_ptr;
// Iso. mat. construction.
static rec_mgr         sig_mgr;
static rec_mgr_ptr     sig_mgr_ptr;
// Iso matching.
static rec_mgr         updates_mgr;
// Strict iso. matching.
static buffer          results_buf;
static buffer_ptr      results_buf_ptr;
// Lazy iso. matching.
static rec_mgr         search_mgr;
static rec_mgr         result_mgr;
// Types.
static int             mat_oidx;
static typeExp_ptr     mat_tp;
static int             search_oidx;
static typeExp_ptr     search_tp;

// Debugging -------------------------------------------------------------------
#define DEBUG_ISO 0
#define debug_print(fmt, ...)                                                  \
        do { if (DEBUG_ISO) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,      \
                                __LINE__, __func__, __VA_ARGS__); } while (0)

// Forward definitions local functions -----------------------------------------
// Matrix mgm.
static void allocate_matrix(mat_ptr *m, unint R, unint C);
static void free_matrix(mat_ptr m);
static void read_matrix(mat_ptr m, g_ptr redex);
// Matrix/Vector/Pexlif utils.
static vec_ptr split_vector(string name);
static void    trim_matrix_head(mat_ptr m);
static int     pexlif_size(g_ptr p);
// Adj. mat. construciton.
static void record_vector(hash_record_ptr tbl_ptr, key_ptr *tail, unint i, const vec_ptr v);
static bool mk_adj_table(key_lst_ptr *k, unint *c, g_ptr p);
static bool mk_adj_matrix(mat_ptr m, unint s, key_lst_ptr k);
static bool mk_adj(mat_ptr m, g_ptr p);
// Iso. mat. construction.
static bool mk_iso_list(sig_ptr *s, g_ptr p);
static bool mk_iso_matrix(mat_ptr m, sig_ptr p, sig_ptr g);
static bool mk_iso(mat_ptr m, g_ptr p, g_ptr g);
// Solution checking.
static bool is_adjacent(const mat_ptr m, int i, int j);
static bool is_single(const mat_ptr iso);
static bool is_isomorphism(const mat_ptr iso, const mat_ptr p, const mat_ptr g);
static bool is_match(const mat_ptr iso, const mat_ptr p, const mat_ptr g);
// Utils for iso. algo.
static void trim(mat_ptr iso, mat_ptr p, mat_ptr g, unint r, unint c, updates_ptr *ps);
static inline void undo_trim(mat_ptr iso, updates_ptr ps);
static inline void pick(mat_ptr iso, mat_ptr cpy, unint r, unint c);
static inline void undo_pick(mat_ptr iso, mat_ptr cpy, unint r);
// Strict isomatching.
static void strict_search(mat_ptr iso, mat_ptr p, mat_ptr g, mat_ptr c, unint row, bool *used);
static void strict_isomatch(mat_ptr iso, mat_ptr p, mat_ptr g, unint r);
// Lazy isomatching.
static search_ptr create_search(mat_ptr iso, mat_ptr p, mat_ptr g, unint start);
static void free_search(search_ptr s);
static mat_ptr lazy_search(search_ptr s);
static mat_ptr lazy_isomatch(search_ptr s);

/******************************************************************************/
/*                                LOCAL FUNCTIONS                             */
/******************************************************************************/

// Mem. mgm. -------------------------------------------------------------------

static void
allocate_matrix(mat_ptr *m, unint R, unint C)
{
    ASSERT(*m != NULL);
    // /
    mat_ptr n = (mat_ptr) new_rec(&mat_mgr);
    n->mark   = 1;
    n->rows   = R;
    n->cols   = C;
    n->mat    = Malloc(R*sizeof(bool*));
    n->mat[0] = Calloc(R*C*sizeof(bool));
    for(unint i=1; i<R; i++) {
        n->mat[i] = n->mat[0]+i*C;
    }
    *m = n;
}

static void
free_matrix(mat_ptr m)
{
    ASSERT(m != NULL);
    // /
    Free((pointer) m->mat[0]);
    Free((pointer) m->mat);
    free_rec(&mat_mgr, (pointer) m);
}

static void
read_matrix(mat_ptr m, g_ptr redex)
{
    g_ptr lr, lc, row, col;
    int i, j;
    // /
    i = 0;
    FOR_CONS(redex, lr, row) {
        j = 0;
        FOR_CONS(row, lc, col) {
            m->mat[i][j] = GET_BOOL(col);
            j++;
        }
        i++;
    }
}

static void
print_matrix(mat_ptr m)
{
    for(unint r = 0; r < m->rows; r++) {
        for(unint c = 0; c < m->cols; c++) {
            fprintf(stderr, " %s", m->mat[r][c] ? "T" : "_");
        }
        fprintf(stderr, "\n");
    }
}

// ? ---------------------------------------------------------------------------

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
trim_matrix_head(mat_ptr m)
{
    for(unint i=0; i<m->rows; i++) {
        m->mat[i][0] = FALSE;
    }
    for(unint i=1; i<m->cols; i++) {
        m->mat[0][i] = FALSE;
    }    
}

static int
pexlif_size(g_ptr p)
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

// Adj. matrix -----------------------------------------------------------------

static void
new_adj_mem()
{
    bkt_mgr_ptr = &bkt_mgr;
    key_mgr_ptr = &key_mgr;
    key_lst_mgr_ptr = &key_lst_mgr;
    tbl_in_ptr = &tbl_in;
    tbl_out_ptr = &tbl_out;
    new_mgr(bkt_mgr_ptr, sizeof(bkt_rec));
    new_mgr(key_mgr_ptr, sizeof(key_rec));
    new_mgr(key_lst_mgr_ptr, sizeof(key_lst_rec));
    create_hash(tbl_in_ptr, 100, str_hash, str_equ);
    create_hash(tbl_out_ptr, 100, str_hash, str_equ);
}

static void
rem_adj_mem()
{
    free_mgr(bkt_mgr_ptr);
    free_mgr(key_mgr_ptr);
    free_mgr(key_lst_mgr_ptr);
    dispose_hash(tbl_in_ptr, NULLFCN);
    dispose_hash(tbl_out_ptr, NULLFCN);
    bkt_mgr_ptr = NULL;
    key_mgr_ptr = NULL;
    key_lst_mgr_ptr = NULL;
    tbl_in_ptr = NULL;
    tbl_out_ptr = NULL;
}

static void
record_vector(hash_record_ptr tbl_ptr, key_ptr *tail, unint i, const vec_ptr v)
{
    string key = Get_vector_signature(&lstrings, v);
    // Record in keys.
    key_ptr n = (key_ptr) new_rec(key_mgr_ptr);
    n->lbl  = key;
    n->vec  = v;
    n->next = NULL;
    (*tail) = n;
    // Record in table.
    bkt_ptr bkt = find_hash(tbl_ptr, key);
    if(bkt == NULL) {
        bkt_ptr b = (bkt_ptr) new_rec(bkt_mgr_ptr);
        b->lbl  = i;
        b->vec  = v;
        b->next = NULL;
        insert_hash(tbl_ptr, key, b);
    } else {
        while(bkt->next != NULL) {
            bkt = bkt->next;
        }
        bkt_ptr b = (bkt_ptr) new_rec(bkt_mgr_ptr);
        b->lbl  = i;
        b->vec  = v;
        b->next = NULL;
        bkt->next = b;
    }
}

static bool
mk_adj_table(key_lst_ptr *keys, unint *count, g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        Fail_pr("mk_adj: expected a pexlif.");
        return FALSE;
    }    
    if(is_P_LEAF(cont, &fns)) {
        Fail_pr("mk_adj: expected a hierarchical pexlif.");
        return FALSE;
    }
    if(is_P_HIER(cont, &children)) {
        vec_ptr vec;
        key_ptr key = NULL, *key_tail = &key;
        // Record vector names for parent's formals.
        // TODO: count outputs as inputs for the environment and vice versa.
        FOREACH_FORMAL(vec, fa_inps) {
            record_vector(tbl_in_ptr, key_tail, 0, vec);
            key_tail = &(*key_tail)->next;
        }
        FOREACH_FORMAL(vec, fa_outs) {
            record_vector(tbl_out_ptr, key_tail, 0, vec);
            key_tail = &(*key_tail)->next;
        }
        key_lst_ptr key_lst = (key_lst_ptr) new_rec(key_lst_mgr_ptr);
        key_lst->key = key;
        key_lst->next = NULL;
        *keys = key_lst;
        *count = 1;
        key_lst_ptr *key_lst_tail = &key_lst->next;
        // Record vector names for each child's actuals.
        g_ptr child, tmp;
        FOR_CONS(children, tmp, child) {
            if(!is_PINST(child, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
                Fail_pr("mk_adj: expected all children to be pexlifs.");
                return FALSE;
            }
            key_ptr key = NULL, *key_tail = &key;
            FOREACH_ACTUAL(vec, fa_inps) {
                record_vector(tbl_in_ptr, key_tail, *count, vec);
                key_tail = &(*key_tail)->next;
            }
            FOREACH_ACTUAL(vec, fa_outs) {
                record_vector(tbl_out_ptr, key_tail, *count, vec);
                key_tail = &(*key_tail)->next;
            }
            key_lst_ptr key_lst = (key_lst_ptr) new_rec(key_lst_mgr_ptr);
            key_lst->key = key;
            key_lst->next = NULL;
            *key_lst_tail = key_lst;
            key_lst_tail = &key_lst->next;
            *count = *count + 1;
        }
        return TRUE;
    }
    Fail_pr("What?!");
    return FALSE;
}

static bool
mk_adj_matrix(mat_ptr m, unint size, key_lst_ptr keys)
{
    ASSERT(m->cols == size);
    ASSERT(m->cols == m->rows);
    // /
    key_ptr key;
    bkt_ptr bkt;
    FOREACH_KEY(key, keys) {
        string name = key->lbl;
        vec_ptr vec = key->vec;
        // Search inputs.
        bkt = (bkt_ptr) find_hash(tbl_in_ptr, name);
        while(bkt != NULL) {
            if(bkt->lbl != i && Check_vector_overlap(vec, bkt->vec)) {
                m->mat[i][bkt->lbl] = TRUE;
            }
            bkt = bkt->next;
        }
        // Search outputs.
        bkt = (bkt_ptr) find_hash(tbl_out_ptr, name);
        while(bkt != NULL) {
            if(bkt->lbl != i && Check_vector_overlap(vec, bkt->vec)) {
                m->mat[i][bkt->lbl] = TRUE;
            }
            bkt = bkt->next;
        }
    }
    return TRUE;
}

static bool
mk_adj(mat_ptr m, g_ptr p)
{
    new_adj_mem();
    // /
    key_lst_ptr keys;
    unint length;
    bool fail = FALSE;
    if(!fail && !mk_adj_table(&keys, &length, p)) { fail = TRUE; }
    if(!fail && !mk_adj_matrix(m, length, keys))  { fail = TRUE; }
    // /
    rem_adj_mem();
    return !fail;
}

// Iso. matrix -----------------------------------------------------------------

static void
new_iso_mem()
{
    sig_mgr_ptr = &sig_mgr;
    new_mgr(sig_mgr_ptr, sizeof(sig_rec));
}

static void
rem_iso_mem()
{
    free_mgr(sig_mgr_ptr);
    sig_mgr_ptr = NULL;
}

static bool
mk_iso_list(sig_ptr *sigs, g_ptr p)
{
    g_ptr attrs, fa_inps, fa_outs, inter, cont, children, fns;
    string name;
    bool leaf;
    if(!is_PINST(p, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
        Fail_pr("mk_iso: expected a pexlif.");
        return FALSE;
    }    
    if(is_P_LEAF(cont, &fns)) {
        Fail_pr("mk_iso: expected a hierarchical pexlif.");
        return FALSE;
    }
    if(is_P_HIER(cont, &children)) {
        string sha = NULL;
        int fp = -1;
        // Record fp/sha attributes for parent.
        // Note: The fp/sha attributes are likely in a fixed order that we could
        //   test for, and only then falling back on the search below.
        for(g_ptr l = attrs; !IS_NIL(l); l = GET_CONS_TL(l)) {
            string key = GET_STRING(GET_CONS_HD(GET_CONS_HD(l)));
            string val = GET_STRING(GET_CONS_TL(GET_CONS_HD(l)));
            if(strcmp(key, "signature") == 0) {
                sha = val;
            } else if(strcmp(key, "fingerprint") == 0) {
                fp = atoi(val);
            }
        }
        if(fp == -1 || sha == NULL) {
            Fail_pr("mk_iso: pexlif lacks fp/sha attributes.");
            return FALSE;
        }
        sig_ptr sig = (sig_ptr) new_rec(&sig_mgr);
        sig->sha = sha;
        sig->fp = fp;
        sig->next = NULL;
        *sigs = sig;
        sig_ptr *sig_tail = &sig->next;
        // Record fp/sha attributes for children.
        g_ptr child, tmp;
        FOR_CONS(children, tmp, child) {
            if(!is_PINST(child, &name, &attrs, &leaf, &fa_inps, &fa_outs, &inter, &cont)) {
                Fail_pr("mk_adj: expected all children to be pexlifs.");
                return FALSE;
            }
            string sha = NULL;
            int fp = -1;
            for(g_ptr l = attrs; !IS_NIL(l); l = GET_CONS_TL(l)) {
                string key = GET_STRING(GET_CONS_HD(GET_CONS_HD(l)));
                string val = GET_STRING(GET_CONS_TL(GET_CONS_HD(l)));
                if(strcmp(key, "signature") == 0) {
                    sha = val;
                } else if(strcmp(key, "fingerprint") == 0) {
                    fp = atoi(val);
                }
            }
            if(fp == -1 || sha == NULL) {
                Fail_pr("mk_iso: pexlif lacks fp/sha attributes.");
                return FALSE;
            }
            sig_ptr sig = (sig_ptr) new_rec(&sig_mgr);
            sig->sha = sha;
            sig->fp = fp;
            sig->next = NULL;
            *sig_tail = sig;
            sig_tail = &sig->next;
        }
        return TRUE;
    }
    Fail_pr("What?!");
    return FALSE;
}

static bool
mk_iso_matrix(mat_ptr m, sig_ptr p, sig_ptr g)
{
    // assert: length(p) = rows(m)
    // assert: length(p) = cols(m)
    // /
    int i = 0;
    for(sig_ptr r = p; r != NULL; r = r->next) {
        int j = 0;
        for(sig_ptr c = g; c != NULL; c = c->next) {
            if(r->fp == c->fp || strcmp(r->sha, c->sha) == 0) {
                m->mat[i][j] = TRUE;
            }
            j++;
        }
        i++;
    }
    return TRUE;
}

static bool
mk_iso(mat_ptr m, g_ptr p, g_ptr g)
{
    new_iso_mem();
    // /
    sig_ptr p_sigs, g_sigs;
    bool fail = FALSE;
    if(!fail && !mk_iso_list(&p_sigs, p)) {
        fail = TRUE;
    }
    if(!fail && !mk_iso_list(&g_sigs, g)) {
        fail = TRUE;
    }
    if(!fail && !mk_iso_matrix(m, p_sigs, g_sigs)) {
        fail = TRUE;
    }
    // /
    rem_iso_mem();
    return !fail;
}

// Solution check --------------------------------------------------------------

static bool
is_adjacent(const mat_ptr m, int i, int j)
{
    ASSERT(i >= 0 && i < (int) m->rows);
    ASSERT(j >= 0 && j < (int) m->cols);
    return m->mat[i][j];
}

static bool
is_single(const mat_ptr iso)
{
    bool row;
    bool *used = Calloc(iso->cols*sizeof(bool));
    // Only one 'TRUE' allowed per row and column.
    for(unint i=0; i<iso->rows; i++) {
        row = FALSE;
        for(unint j=0; j<iso->cols; j++) {
            if(iso->mat[i][j]) {
                if(row || used[j]) {
                    Free((void *)used);
                    return FALSE;
                }
                row = TRUE;
                used[j] = TRUE;
            }
        }
    }
    Free((void *)used);
    return TRUE;
}

static bool
is_isomorphism(const mat_ptr iso, const mat_ptr p, const mat_ptr g)
{
    // P : AxA, G : BxB, ISO : AxB
    ASSERT(p->rows   == p->cols);
    ASSERT(g->rows   == g->cols);
    ASSERT(iso->rows == p->rows);
    ASSERT(iso->cols == g->cols);
    // Short-hands.
    unint GC = g->cols, IR = iso->rows, IC = iso->cols;
    bool *P = p->mat[0], *G = g->mat[0], *I = iso->mat[0];
    // P == ISO x (ISO x G)^T
    bool t1,t2;
    for(unint i=0; i<IR; i++) {
        for(unint j=0; j<IR; j++) {
            t1 = FALSE;
            for(unint k=0; k<IC; k++) {
                t2 = FALSE;
                for(unint l=0; l<IC; l++) {
                    if(I[l + (IC * j)] && G[k + (GC * l)]) {
                        t2 = TRUE;
                        break;
                    }
                }
                if(I[k + (IC * i)] && t2) {
                    t1 = TRUE;
                    break;
                }
            }
            // If P is connected at this index, G should be as well.
            // That is, G is at least as connected as P. For a stricter
            // connectivity we could use equality of t1 and P instead.
            if(P[i * IR + j] && !t1) {
                return FALSE;
            }
        }
    }
    return TRUE;
}

static bool
is_match(const mat_ptr iso, const mat_ptr p, const mat_ptr g)
{
    return (is_single(iso) && is_isomorphism(iso, p, g));
}

// Trimming of solution space --------------------------------------------------

/* Trim impossible matches given a match of 'P(r)' to 'G(c)': If 'P(r)' is
 * matched against 'G(c)', then no 'P(x)' adj. to 'P(r)' can be isomorphic to
 * some 'G(y)' that is not adj. to 'G(c)'.
 */
static void
trim(mat_ptr iso, mat_ptr p, mat_ptr g, unint r, unint c, updates_ptr *ps)
{
    ASSERT(r < iso->rows);
    ASSERT(c < iso->cols);
    ASSERT(iso->rows == p->rows);
    ASSERT(iso->cols == g->rows);
    // Note: swapped i&r (same for j&c), not sure if actually better perf.
    // Note: starts at r+1 since every row above r should have been picked and
    //       r is not adjacent to itself.
    updates_ptr head  = NULL;
    updates_ptr *tail = &head;
    for(unint i = r+1; i<iso->rows; i++) {
        if(p->mat[r][i]) {
            for(unint j = 0; j<iso->cols; j++) {
                if(!g->mat[c][j] && iso->mat[i][j]) {
                    iso->mat[i][j] = FALSE;
                    // Record update
                    updates_ptr new = (updates_ptr) new_rec(&updates_mgr);
                    new->row  = i;
                    new->col  = j;
                    new->next = NULL;
                    *tail = new;
                    tail  = &new->next;
                }
            }
        }
    }
    *ps = head;    
}

static inline void
undo_trim(mat_ptr iso, updates_ptr ps)
{
    updates_ptr p;
    while(ps != NULL) {
        iso->mat[ps->row][ps->col] = TRUE;
        // Record removed.
        p  = ps;
        ps = ps->next;
        free_rec(&updates_mgr, (pointer) p);
    }
}

static inline void
pick(mat_ptr iso, mat_ptr cpy, unint r, unint c)
{
    for(unint j = 0; j < iso->cols; j++) {
        cpy->mat[r][j] = iso->mat[r][j];
        iso->mat[r][j] = 0;
    }
    iso->mat[r][c] = 1;
}

static inline void
undo_pick(mat_ptr iso, mat_ptr cpy, unint r)
{
    for(unint j = 0; j < iso->cols; j++) {
        iso->mat[r][j] = cpy->mat[r][j];
    }
}

// -----------------------------------------------------------------------------
// Strict searching.

static inline void
new_strict_search_mem(mat_ptr *copy, bool **used, unint rows, unint cols)
{
    allocate_matrix(copy, rows, cols);
    *used = Calloc(cols*sizeof(bool));
    results_buf_ptr = &results_buf;
    new_buf(results_buf_ptr, 1, sizeof(mat_rec));
}

static inline void
rem_strict_search_mem(mat_ptr copy, bool *used)
{
    free_matrix((pointer) copy);
    Free((pointer) used);
    free_buf(results_buf_ptr);
    results_buf_ptr = NULL;
}

static void
strict_search(mat_ptr iso, mat_ptr p, mat_ptr g, mat_ptr c, unint row, bool *used)
{
    if(iso->rows == row) {
        if(is_match(iso, p, g)) {
            push_buf(results_buf_ptr, (pointer) iso);
            return;
        } else {
            return;
        }
    }
    updates_ptr points;
    for(unint col = 0; col < iso->cols; col++) {
        if(used[col] || !iso->mat[row][col]) {
            continue;
        }
        trim(iso, p, g, row, col, &points);
        pick(iso, c, row, col);
        used[col] = TRUE;
        // /
        strict_search(iso, p, g, c, row + 1, used);
        // /
        undo_pick(iso, c, row);
        undo_trim(iso, points);
        used[col] = FALSE;
    }
}

static void
strict_isomatch(mat_ptr iso, mat_ptr p, mat_ptr g, unint row)
{
    bool *used;
    mat_ptr copy;
    new_strict_search_mem(&copy, &used, p->rows, g->rows);
    // /
    strict_search(iso, p, g, copy, row, used);
    debug_print("Found %u solutions\n", results_buf_ptr->buf_size);
    // /
    rem_strict_search_mem(copy, used);
}

// -----------------------------------------------------------------------------
// Lazy searching.

static search_ptr
create_search(mat_ptr iso, mat_ptr p, mat_ptr g, unint start)
{
    ASSERT(iso->rows == p->rows);
    ASSERT(iso->cols == g->cols);
    ASSERT(p->rows   == p->cols);
    ASSERT(g->rows   == g->cols);
    // /
    search_ptr s = (search_ptr) new_rec(&search_mgr);
    s->mark      = 1;
    s->start     = start;
    s->row       = start;
    s->cols      = Calloc(iso->rows*sizeof(unint));
    s->set       = Calloc((iso->rows+1)*sizeof(bool));
    s->used      = Calloc(iso->cols*sizeof(bool));
    s->changes   = Calloc(iso->rows*sizeof(updates_ptr));
    s->isomatch  = iso;
    s->needle    = p;
    s->haystack  = g;
    // /
    mat_ptr copy;    
    allocate_matrix(&copy, iso->rows, iso->cols);
    s->copy = copy;
    // /
    return s;
}

static void
free_search(search_ptr s)
{
    Free((pointer) s->cols);
    Free((pointer) s->set);
    Free((pointer) s->used);
    Free((pointer) s->changes);
    free_matrix((pointer) s->copy);
    free_matrix((pointer) s->isomatch);
    free_matrix((pointer) s->needle);
    free_matrix((pointer) s->haystack);
    free_rec(&search_mgr, (pointer) s);
}

static mat_ptr
lazy_search(search_ptr s)
{
    // Short-hands.
    mat_ptr M = s->isomatch;
    mat_ptr N = s->copy;
    mat_ptr P = s->needle;
    mat_ptr G = s->haystack;
    unint   R = M->rows;
    unint   C = M->cols;
    // Main loop, called until solution is found or if search runs out of
    // potential matches.
    loop: {
        if(s->row == R) {
            if(!s->set[R] && is_match(M, P, G)) {
                s->set[R] = TRUE;
                return M;
            } else {
                s->set[R] = FALSE;
                s->row = R - 1;
                goto loop;
            }
        }
        unint start;
        if(s->set[s->row]) {
            // Undo.
            unint c = s->cols[s->row], r = s->row;
            undo_pick(M, N, r);
            undo_trim(M, s->changes[r]);
            s->used[c] = FALSE;
            // Prep. for next col.
            s->set[r] = FALSE;
            start = s->cols[r] + 1;
        } else {
            start = 0;
        }
        unint next = start;
        while(next < C && (s->used[next] || !M->mat[s->row][next])) { next++; }
        if(next > start && next < C) {
            // Do.
            unint c = next, r = s->row;
            trim(M, P, G, r, c, &s->changes[r]);
            pick(M, N, r, c);
            s->used[c] = TRUE;
            // Prep. for next row.
            s->cols[r] = c;
            s->set[r] = TRUE;
            s->row = r + 1;
            goto loop;
        } else {
            if(s->row == 0) {
                return NULL;
            } else {
                s->row = s->row - 1;
                goto loop;
            }
        }
    }
    // Something went wrong...
    DIE("Impossible");
}

static mat_ptr
lazy_isomatch(search_ptr s)
{
    // Used to do more, could remove now.
    return lazy_search(s);
}

/******************************************************************************/
/*                               PUBLIC FUNCTIONS                             */
/******************************************************************************/

static void
mark_mat_fn(pointer p)
{
    mat_ptr m = (mat_ptr) p;
    m->mark = 2;
}

static void
sweep_mat_fn()
{
    mat_ptr m;
    FOR_REC(&mat_mgr, mat_ptr, m) {
        switch(m->mark) {
        case 0:
            break;
        case 1:
            free_matrix(m);
            m->mark = 0;
            break;
        case 2:
            m->mark = 1;
            break;
        default:
            DIE("Should not happen");
        }
    }
}

//------------------------------------------------------------------------------

static void
mark_search_fn(pointer p)
{
    search_ptr s = (search_ptr) p;
    s->mark = 2;
}

static void
sweep_search_fn()
{
    search_ptr s;
    FOR_REC(&search_mgr, search_ptr, s) {
        switch(s->mark) {
        case 0:
            break;
        case 1:
            free_search(s);
            s->mark = 0;
            break;
        case 2:
            s->mark = 1;
            break;
        default:
            DIE("Should not happen");
        }
    }
}

//------------------------------------------------------------------------------

static void
strict_isomatch_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_p, g_g, g_box;
    EXTRACT_3_ARGS(redex, g_p, g_g, g_box);
    bool box = GET_BOOL(g_box);
    // /
    int p_size = pexlif_size(g_p);
    int g_size = pexlif_size(g_g);
    if(p_size < 2) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: malformed needle."));
    } else if(g_size < 2) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: malformed haystack."));
    } else {
        mat_ptr adj_p, adj_g, iso;
        allocate_matrix(&adj_p, p_size, p_size);
        allocate_matrix(&adj_g, g_size, g_size);
        allocate_matrix(&iso,   p_size, g_size);
        bool fail = FALSE;
        if(!fail && !mk_adj(adj_p, g_p)) {
            MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: invalid needle."));
            fail = TRUE;
        }
        if(!fail && !mk_adj(adj_g, g_g)) {
            MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: invalid haystack."));
            fail = TRUE;
        }
        if(!fail && !mk_iso(iso, g_p, g_g)) {
            MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: invalid isomorphisms."));
            fail = TRUE;
        }
        if(!fail) {
            clock_t start, end;
            double cpu_time_used;
            start = clock();
            // /
            unint start_row;
            if(box) {
                trim_matrix_head(adj_p);
                trim_matrix_head(iso);
                start_row = 1;
            } else {
                start_row = 0;
            }
            strict_isomatch(iso, adj_p, adj_g, start_row);
            free_matrix(adj_p);
            free_matrix(adj_g);
            free_matrix(iso);
            // /
            end = clock();
            cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
            debug_print("Time spent: %fs\n", cpu_time_used);
            MAKE_REDEX_VOID(redex);
        }
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
internal_search_create_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_needle, g_haystack, g_trim;
    EXTRACT_3_ARGS(redex, g_needle, g_haystack, g_trim);
    // /
    unint n_rows = pexlif_size(g_needle);
    unint h_rows = pexlif_size(g_haystack);
    if(n_rows <= 1) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("Expected a hier. pexlif as needle."));
    } else if(h_rows <= 1) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("Expected a hier. pexlif as haystack."));
    } else if(!IS_BOOL(g_trim)) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("Expected a boolean."));
    } else {
        bool trim = GET_BOOL(g_trim);
        debug_print("trim: %s\n", trim ? "T" : "F");
        debug_print("needle rows: %u\n", n_rows);
        debug_print("haystack rows: %u\n", h_rows);
        // /
        mat_ptr needle, haystack, iso;
        allocate_matrix(&needle,   n_rows, n_rows);
        allocate_matrix(&haystack, h_rows, h_rows);
        allocate_matrix(&iso,      n_rows, h_rows);
        if(!mk_adj(needle, g_needle) ||
           !mk_adj(haystack, g_haystack) ||
           !mk_iso(iso, g_needle, g_haystack))
        {
            MAKE_REDEX_FAILURE(redex, Fail_pr("Invalid needle/haystack."));
            free_matrix((pointer) needle);
            free_matrix((pointer) haystack);
            free_matrix((pointer) iso);
        } else {
            unint start;
            if(trim) {
                trim_matrix_head(needle);
                trim_matrix_head(iso);
                start = 1;
            } else {
                start = 0;
            }
            search_ptr s = create_search(iso, needle, haystack, start);
            MAKE_REDEX_EXT_OBJ(redex, search_oidx, s);
        }
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
internal_search_step_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_search;
    EXTRACT_1_ARG(redex, g_search);
    // /
    search_ptr search = (search_ptr) GET_EXT_OBJ(g_search);
    mat_ptr res = lazy_isomatch(search);
    if(res == NULL) {
        MAKE_REDEX_NIL(redex);
        free_search(search);
    } else {
        MAKE_REDEX_NIL(redex);
        g_ptr tail = redex;
        for(unint i = 0; i < res->rows; i++) {
            unint j = 0;
            while(j < res->cols && !res->mat[i][j]) { j++; }
            g_ptr nd = Make_PAIR_ND(Make_INT_leaf(i), Make_INT_leaf(j));
            APPEND1(tail, nd);
        }
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

//------------------------------------------------------------------------------

void
Iso_Init()
{
    // Adj. mat.
    mat_oidx = Add_ExtAPI_Object(
        "bmat"
      , mark_mat_fn
      , sweep_mat_fn
      , NULL //save_?_fn
      , NULL //load_?_fn
      , NULL //?2string_fn
      , NULL //?_eq_fn
      , NULL
      , NULL
      , NULL //?2sha256_fn
    );
    mat_tp = Get_Type("bmat", NULL, TP_INSERT_FULL_TYPE);
    // Search. state.
    search_oidx = Add_ExtAPI_Object(
        "internal_search_state"
      , mark_search_fn
      , sweep_search_fn
      , NULL
      , NULL
      , NULL
      , NULL
      , NULL
      , NULL
      , NULL
    );
    search_tp = Get_Type("internal_search_state", NULL, TP_INSERT_FULL_TYPE);
    // Init. generics.
    new_ustrmgr(&lstrings);
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
    new_mgr(&mat_mgr, sizeof(mat_rec));
    new_mgr(&updates_mgr, sizeof(updates_rec));
    new_mgr(&search_mgr, sizeof(search_rec));
    new_mgr(&result_mgr, sizeof(result_rec));
}

void
Iso_Install_Functions()
{
    typeExp_ptr pexlif_tp = Get_Type("pexlif", NULL, TP_INSERT_PLACE_HOLDER);
    typeExp_ptr step_tp = GLmake_list(GLmake_tuple(GLmake_int(), GLmake_int()));
    Add_ExtAPI_Function(
          "sisomatch"
        , "111"
        , TRUE
          // pex->pex->bool->void
        , GLmake_arrow(
              pexlif_tp
            , GLmake_arrow(
                pexlif_tp
              , GLmake_arrow(
                  GLmake_bool()
                , GLmake_void())))
        , strict_isomatch_fn
    );
    Add_ExtAPI_Function(
          "internal_search_create"
        , "111"
        , TRUE
          // pex->pex->bool->search
        , GLmake_arrow(
              pexlif_tp
            , GLmake_arrow(
                pexlif_tp
              , GLmake_arrow(
                  GLmake_bool()
                , search_tp)))
        , internal_search_create_fn
    );
    Add_ExtAPI_Function(
          "internal_search_step"
        , "1"
        , TRUE
          // search->step
        , GLmake_arrow(
              search_tp
            , step_tp)
        , internal_search_step_fn
    );
}

/******************************************************************************/
/*                               DUMMY FUNCTIONS                              */
/******************************************************************************/

void
_DuMMy_iso()
{
    read_matrix(NULL, NULL);
    print_matrix(NULL);
    is_adjacent(NULL, 0, 0);
    lazy_isomatch(NULL);
}

/******************************************************************************/
