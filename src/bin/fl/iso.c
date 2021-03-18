//------------------------------------------------------------------------------
// Copyright 2020 Markus Aronsson
// SPDX-License-Identifier: Apache-2.0
//------------------------------------------------------------------------------

/******************************************************************************/
/*                                                                            */
/*     Original author: Markus Aronsson, 2020                                 */
/*                                                                            */
/******************************************************************************/
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
// Iso. matching.
static rec_mgr         point_mgr;
static rec_mgr_ptr     point_mgr_ptr;
static buffer          res_buf;
static buffer_ptr      res_buf_ptr;
static bool            *used;
static mat_rec         copy;
static mat_ptr         copy_ptr;
static mat_ptr         needle;
static mat_ptr         haystack;
// Types.
static int             mat_oidx;
static typeExp_ptr     mat_tp;
// Debugging.
#define DEBUG_ISO 0
#define debug_print(fmt, ...)                                                  \
        do { if (DEBUG_ISO) fprintf(stderr, "%s:%d:%s(): " fmt, __FILE__,      \
                                __LINE__, __func__, __VA_ARGS__); } while (0)

// Forward definitions local functions -----------------------------------------
// Matrix mgm.
static void allocate_matrix(mat_ptr *m, unint R, unint C);
static void free_matrix(mat_ptr m);
static void read_matrix(mat_ptr m, g_ptr redex);
// ?
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
static bool test_adjacent(const mat_ptr m, int i, int j);
static bool test_isomorphism_formatting(const mat_ptr iso);
static bool test_isomorphism_match(const mat_ptr iso, const mat_ptr p, const mat_ptr g);
// Isomatching algo.
static void trim(mat_ptr iso, unint r, unint c, point_ptr *ps);
static void pick(mat_ptr iso, unint r, unint c);
static void fill_points(mat_ptr iso, point_ptr ps);
static void fill_shadow(mat_ptr iso, unint r);
static void recurse(mat_ptr iso, unint row);
static void isomatch(mat_ptr iso, mat_ptr p, mat_ptr g);

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
            fprintf(stderr, " %s", m->mat[r][c] ? "_" : "T");
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
        debug_print("(%d) search parents\n", 1);        
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
        debug_print("(%d) search children\n", 2);        
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
            if(r->fp == c->fp || strcmp(r->sha, c->sha)) {
                m->mat[i][j] = TRUE;
                if(m->mat[i][j]) {
                    fprintf(stderr, "true!\n");
                }
            }
            j++;
        }
        i++;
    }
    fprintf(stderr, "mat (iso?):\n");
    print_matrix(m);
    // /
    return TRUE;
}

static bool
mk_iso(mat_ptr m, g_ptr p, g_ptr g)
{
    new_iso_mem();
    // /
    sig_ptr p_sigs, g_sigs;
    bool fail = FALSE;
    debug_print("(%d) mk_iso_list(p)\n", 1);
    if(!fail && !mk_iso_list(&p_sigs, p)) {
        fail = TRUE;
    }
    debug_print("(%d) mk_iso_list(g)\n", 2);
    if(!fail && !mk_iso_list(&g_sigs, g)) {
        fail = TRUE;
    }
    debug_print("(%d) mk_iso_matrix\n", 3);
    if(!fail && !mk_iso_matrix(m, p_sigs, g_sigs)) {
        fail = TRUE;
    }
    debug_print("(%d) done\n", 4);
    // /
    rem_iso_mem();
    return !fail;
}

// Solution check --------------------------------------------------------------

static bool
test_adjacent(const mat_ptr m, int i, int j)
{
    ASSERT(i >= 0 && i < (int) m->rows);
    ASSERT(j >= 0 && j < (int) m->cols);
    return m->mat[i][j];
}

static bool
test_isomorphism_formatting(const mat_ptr iso)
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
test_isomorphism_match(const mat_ptr iso, const mat_ptr p, const mat_ptr g)
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

// Iso. search -----------------------------------------------------------------

static void
new_rec_mem(mat_ptr p, mat_ptr g)
{
    used = Calloc(g->rows*sizeof(bool));
    copy_ptr = &copy;
    allocate_matrix(&copy_ptr, p->rows, g->rows);
    needle = p;
    haystack = g;
    // /
    point_mgr_ptr = &point_mgr;
    res_buf_ptr = &res_buf;
    new_mgr(point_mgr_ptr, sizeof(point_rec));
    new_buf(res_buf_ptr, 1, sizeof(mat_rec));
}

static void
rem_rec_mem()
{
    Free((pointer) used);
    free_matrix((pointer) copy_ptr);
    copy_ptr = NULL;
    needle = NULL;
    haystack = NULL;
    // /
    free_buf(res_buf_ptr);
    free_mgr(point_mgr_ptr);
    point_mgr_ptr = NULL;
    res_buf_ptr = NULL;
}

/* Trim impossible matches given a match of 'P(r)' to 'G(c)': If 'P(r)' is
 * matched against 'G(c)', then no 'P(x)' adj. to 'P(r)' can be isomorphic to
 * some 'G(y)' that is not adj. to 'G(c)'.
 */
static void
trim(mat_ptr iso, unint r, unint c, point_ptr *ps)
{
    ASSERT(r < iso->rows);
    ASSERT(c < iso->cols);
    ASSERT(iso->rows == needle->rows);
    ASSERT(iso->cols == haystack->rows);
    // /
    point_ptr head = NULL, *tail = &head;
    // Note: swapped i->r for r->i (same for j->c), equal but better caching,
    //   might change if adj. def. is updated.
    // Note: starts at r+1 since every row above r should have been picked and
    //   r is not adjacent to itself.
    for(unint i = r+1; i<iso->rows; i++) {
        if(needle->mat[r][i]) {
            for(unint j = 0; j<iso->cols; j++) {
                if(!haystack->mat[c][j] && iso->mat[i][j]) {
                    iso->mat[i][j] = FALSE;
                    // Record update as a record in rec.
                    point_ptr new = (point_ptr) new_rec(point_mgr_ptr);
                    new->row  = i;
                    new->col  = j;
                    new->next = NULL;
                    *tail = new;
                    tail = &new->next;
                }
            }
        }
    }
    *ps = head;
}

static void
pick(mat_ptr iso, unint r, unint c)
{
    ASSERT(iso->rows == copy_ptr->rows);
    ASSERT(iso->cols == copy_ptr->cols);
    // /
    for(unint j = 0; j < iso->cols; j++) {
        copy_ptr->mat[r][j] = iso->mat[r][j];
        iso->mat[r][j] = 0;
    }
    iso->mat[r][c] = 1;
}

static void
fill_points(mat_ptr iso, point_ptr ps)
{
    point_ptr p;
    while(ps != NULL) {
        iso->mat[ps->row][ps->col] = TRUE;
        p = ps;
        ps = ps->next;
        free_rec(point_mgr_ptr, (pointer) p);
    }
}

static void
fill_shadow(mat_ptr iso, unint r)
{
    for(unint j = 0; j < iso->cols; j++) {
        iso->mat[r][j] = copy_ptr->mat[r][j];
    }
}

static void
recurse(mat_ptr iso, unint row)
{
    if(iso->rows == row) {
        fprintf(stderr, "solution?\n");
        print_matrix(iso);
        if(test_isomorphism_match(iso, needle, haystack)) {
            push_buf(res_buf_ptr, (pointer) iso);
            Exit(-1);
            return;
        } else {
            return;
        }
    }
    point_ptr points;
    for(unint col = 0; col < iso->cols; col++) {
        if(used[col] || !iso->mat[row][col]) {
            continue;
        }
        trim(iso, row, col, &points);
        pick(iso, row, col);
        used[col] = TRUE;
        // /
        recurse(iso, row + 1);
        // /
        fill_shadow(iso, row);
        fill_points(iso, points);
        used[col] = FALSE;
    }
}

static void
isomatch(mat_ptr iso, mat_ptr p, mat_ptr g)
{
    fprintf(stderr, "iso:\n");
    print_matrix(iso);
    fprintf(stderr, "needle:\n");
    print_matrix(p);
    fprintf(stderr, "haystack:\n");
    print_matrix(g);
    // /
    new_rec_mem(p, g);
    recurse(iso, 0);
    rem_rec_mem();
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
pex2adj_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_pex, g_size;
    EXTRACT_2_ARGS(redex, g_pex, g_size);
    int size = GET_INT(g_size);
    // /
    if(size <= 0) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("pex2adj: pexlif must have a positive size."));
    }
    mat_ptr adj;
    allocate_matrix(&adj, size, size);
    if(!mk_adj(adj, g_pex)) {
        free_matrix(adj);
        MAKE_REDEX_FAILURE(redex, Fail_pr("pex2adj: pexlif couldn't be converted."));
    } else {
        MAKE_REDEX_EXT_OBJ(redex, mat_oidx, adj);
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
pex2iso_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_p, g_g, g_rows, g_cols;
    EXTRACT_4_ARGS(redex, g_p, g_g, g_rows, g_cols);
    int rows = GET_INT(g_rows);
    int cols = GET_INT(g_cols);
    // /
    if (rows <= 0 || cols <= 0) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("pex2iso: pexlif must have a positive size."));
    }
    mat_ptr iso;
    allocate_matrix(&iso, rows, cols);
    if(!mk_iso(iso, g_p, g_g)) {
        free_matrix(iso);
        MAKE_REDEX_FAILURE(redex, Fail_pr("pex2iso: pexlif couldn't be converted."));
    } else {
        MAKE_REDEX_EXT_OBJ(redex, mat_oidx, iso);
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
trim_adj_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_adj;
    EXTRACT_1_ARG(redex, g_adj);
    mat_ptr adj = (mat_ptr) GET_EXT_OBJ(g_adj);
    // /
    if(adj->rows <= 0 || adj->cols <= 0) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("trim_adj: empty adj."));
    } else {
        trim_matrix_head(adj);
        MAKE_REDEX_VOID(redex);
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
is_adjacent_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_adj, g_row, g_col;
    EXTRACT_3_ARGS(redex, g_adj, g_row, g_col);
    mat_ptr adj = (mat_ptr) GET_EXT_OBJ(g_adj);
    int row = (unint) GET_INT(g_row);
    int col = (unint) GET_INT(g_col);
    // /
    if(row <= 0 || col <= 0) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("is_adjacent: index negative."));
    } else if ((unint) row-1 > adj->rows || (unint) col-1 > adj->cols) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("is_adjacent: index out of range."));
    } else {
        if(test_adjacent(adj, (unint) row-1, (unint) col-1)) {
            MAKE_REDEX_BOOL(redex, B_One());
        } else {
            MAKE_REDEX_BOOL(redex, B_Zero());
        }
    }
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
is_isomorphism_fn(g_ptr redex)
{
    debug_print("%p\n", (void *) redex);
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_p, g_g, g_iso;
    EXTRACT_3_ARGS(redex, g_p, g_g, g_iso);
    mat_ptr p = (mat_ptr) GET_EXT_OBJ(g_p);
    mat_ptr g = (mat_ptr) GET_EXT_OBJ(g_g);
    // /
    int rows = List_length(g_iso);
    int cols = List_length(GET_CONS_HD(g_iso));
    mat_ptr iso;
    allocate_matrix(&iso, p->rows, g->cols);
    read_matrix(iso, g_iso);
    if(rows <= 0 || cols <= 0) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("is_isomorphism: negative index."));
    } else if((unint) rows != p->rows || (unint) cols != g->cols) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("is_isomorphism: mismatch in iso. size."));
    } else if(p->rows != p->cols || g->rows != g->cols) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("is_isomorphism: mismatch in adj. sizes."));
    } else if(iso->rows != p->rows || iso->cols != g->cols) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("is_isomorphism: iso. size doesn't match adj. sizes."));
    } else {
        if(test_isomorphism_formatting(iso) && test_isomorphism_match(iso, p, g)) {
            MAKE_REDEX_BOOL(redex, B_One());
        } else {
            MAKE_REDEX_BOOL(redex, B_Zero());
        }
    }
    free_matrix(iso);
    // /
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
isomatch_fn(g_ptr redex)
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
        debug_print("(%d) allocate\n", 1);
        mat_ptr adj_p, adj_g, iso;
        allocate_matrix(&adj_p, p_size, p_size);
        allocate_matrix(&adj_g, g_size, g_size);
        allocate_matrix(&iso,   p_size, g_size);
        debug_print("(%d) mk_adj(p)\n", 2);
        bool fail = FALSE;
        if(!fail && !mk_adj(adj_p, g_p)) {
            MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: invalid needle."));
            fail = TRUE;
        }
        debug_print("(%d) mk_adj(g)\n", 3);
        if(!fail && !mk_adj(adj_g, g_g)) {
            MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: invalid haystack."));
            fail = TRUE;
        }
        debug_print("(%d) mk_iso\n", 4);
        if(!fail && !mk_iso(iso, g_p, g_g)) {
            MAKE_REDEX_FAILURE(redex, Fail_pr("isomatch: invalid isomorphisms."));
            fail = TRUE;
        }
        debug_print("(%d) isomatch\n", 5);
        if(!fail) {
            if(box) {
                trim_matrix_head(adj_p);
            }
            isomatch(iso, adj_p, adj_g);
            MAKE_REDEX_VOID(redex);
        }
        debug_print("(%d) done\n", 6);
        // /
        free_matrix(adj_p);
        free_matrix(adj_g);
        free_matrix(iso);
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
    // Init. generics.
    new_ustrmgr(&lstrings);
    new_mgr(&vec_mgr, sizeof(vec_rec));
    new_mgr(&rng_mgr, sizeof(range_rec));
    new_mgr(&mat_mgr, sizeof(mat_rec));
}

void
Iso_Install_Functions()
{
    typeExp_ptr pexlif_tp = Get_Type("pexlif", NULL, TP_INSERT_PLACE_HOLDER);
    Add_ExtAPI_Function(
          "pex2adj"
        , "11"
        , TRUE
          // pex->int->adj
        , GLmake_arrow(
              pexlif_tp
            , GLmake_arrow(
                GLmake_int()
              , mat_tp))
        , pex2adj_fn
    );
    Add_ExtAPI_Function(
          "pex2iso"
        , "1111"
        , TRUE
          // pex->pex->int->int->iso
        , GLmake_arrow(
              pexlif_tp
            , GLmake_arrow(
                pexlif_tp
              , GLmake_arrow(
                  GLmake_int()
                , GLmake_arrow(
                    GLmake_int()
                  , mat_tp))))
        , pex2iso_fn
    );
    Add_ExtAPI_Function(
          "trim_adj"
        , "1"
        , TRUE
          // adj->void
        , GLmake_arrow(
              mat_tp
            , GLmake_void())
        , trim_adj_fn
    );
    Add_ExtAPI_Function(
          "is_adjacent"
        , "111"
        , FALSE
          // adj->int->int->bool
        , GLmake_arrow(
              mat_tp
            , GLmake_arrow(
                GLmake_int()
              , GLmake_arrow(
                  GLmake_int()
                , GLmake_bool())))
        , is_adjacent_fn
    );
    Add_ExtAPI_Function(
          "is_isomorphism"
        , "111"
        , FALSE
          // adj->adj->((bool list) list)->int->int->bool
        , GLmake_arrow(
            mat_tp
          , GLmake_arrow(
              mat_tp
            , GLmake_arrow(
                GLmake_list(GLmake_list(GLmake_bool()))
              , GLmake_bool())))
        , is_isomorphism_fn
    );
    // /
    Add_ExtAPI_Function(
          "isomatch"
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
        , isomatch_fn
    );

}

/******************************************************************************/
/*                               DUMMY FUNCTIONS                              */
/******************************************************************************/

void
_DuMMy_iso()
{
    free_matrix(NULL);
    test_isomorphism_formatting(NULL);
    test_isomorphism_match(NULL,NULL,NULL);
    isomatch(NULL,NULL,NULL);
}

/******************************************************************************/
