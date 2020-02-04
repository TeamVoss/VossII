//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/************************************************************************/
/*		Original author: Carl-Johan Seger, 1995			*/
/************************************************************************/
#include "cache.h"
#include "graph.h"

/***** REFERENCED GLOBAL VARIABLES *****/
extern g_ptr    void_nd;
extern buffer	ext_obj_buf;

/***** PRIVATE VARIABLES *****/
static buffer		g_cache_tbl;
static int		g_cache_cnt;
static buffer		ref_var_tbl;
static int		ref_var_cnt;

static unint    primes[] = {
                            13,
                            23,
                            59,
                            113,
                            241,
                            503,
                            1019,
                            2039,
                            4091,
                            8179,
                            16369,
                            32749,
                            65521,
                            131063,
                            262139,
                            524269,
                            1048571,
                            2097143,
                            4194287,
                            8388593,
                            16777199,
                            33554393,
                            67108859,
                            134217689,
                            268435399,
                            536870879,
                            1073741789,
                            2147483629,
                            0};


/* Forward declaration */
static bool             cache_eq(g_ptr p1, g_ptr p2);
static unint		main_cache_hash(g_ptr argl, unint size);
static unsigned int     cache_hash(g_ptr np, unsigned int n);
static int		new_prime(unint cur_size);

/************************************************************************/
/*			GLOBAL FUNCTIONS				*/
/************************************************************************/

void
Init_G_Caches()
{
    g_cache_cnt = 0;
    ref_var_cnt = 0;
    new_buf(&g_cache_tbl, 100, sizeof(g_cache_rec));
    new_buf(&ref_var_tbl, 10, sizeof(g_ptr));
}

int
Make_g_cache()
{
    g_cache_rec gcr;
    gcr.size = 0;
    gcr.load = 0;
    gcr.mark = FALSE;
    gcr.tbl  = NULL;
    push_buf(&g_cache_tbl, (pointer) &gcr);
    g_cache_cnt++;
    return( g_cache_cnt-1 );
}

g_ptr
Find_in_g_cache(int cache_tbl, g_ptr argl)
{
    g_cache_ptr gp;
    ui		hash;
    g_ptr	*recp;

    gp = (g_cache_ptr) locate_buf(&g_cache_tbl, cache_tbl);
    if( gp->load == 0 )
	return( NULL );
    hash  = main_cache_hash(argl, gp->size);
    recp  = gp->tbl + 2*hash;
    if( cache_eq(*recp, argl) ) {
	return( *(recp+1) );
    } else {
	return( NULL );
    }
}

void
Insert_in_g_cache(int cache_tbl, g_ptr argl, g_ptr res)
{
    g_cache_ptr gp;
    unint	hash, i;
    g_ptr	*recp;

    if( res == NULL )
	return;

    gp = (g_cache_ptr) locate_buf(&g_cache_tbl, cache_tbl);

    /* Check whether cache size should be increased */
    if( gp->size == 0 || ((100*(gp->load))/(gp->size) > MAX_CACHE_LOAD) ) {
	/* Increase the size of the cache */
	g_ptr	*old_tbl, *old_recp;
	unint	old_size;

	old_size = gp->size;
	old_tbl  = gp->tbl;
	gp->size = new_prime(old_size+1);
	gp->load = 0;
	gp->tbl  = (g_ptr *) Calloc(2*(gp->size)*sizeof(g_ptr));
	old_recp = old_tbl;
	for(i = 0; i < old_size; i++) {
	    if( *old_recp != NULL ) {
		hash = main_cache_hash(*old_recp, gp->size);
		recp = gp->tbl + 2*hash;
		if( *recp == NULL )
		    gp->load++;
		*recp     = *old_recp;
		*(recp+1) = *(old_recp+1);
	    }
	    old_recp += 2;
	}
 	if( old_tbl != NULL )
	    Free((pointer) old_tbl);
    }

    hash  = main_cache_hash(argl, gp->size);
    recp  = gp->tbl + 2*hash;
    if( *recp == NULL )
	gp->load++;
    *recp     = argl;
    *(recp+1) = res;
}


void
Mark_G_Cache(int cache_tbl)
{
    g_cache_ptr gp;
    g_ptr	*recp;
    unint 	i;

    gp = (g_cache_ptr) locate_buf(&g_cache_tbl, cache_tbl);
    gp->mark = TRUE;
    recp = gp->tbl;
    if( recp != NULL ) {
	for(i = 0; i < gp->size; i++) {
	    if( *recp != NULL ) {
		Mark(*recp);
		Mark(*(recp+1));
	    }
	    recp += 2;
	}
    }
}

void
Sweep_G_caches()
{
    g_cache_ptr	gp;
    FOR_BUF(&g_cache_tbl, g_cache_rec, gp) {
	if( gp->mark == FALSE) {
	    /* Empty the cache */
	    gp->size = 0;
	    gp->load = 0;
	    Free((pointer) (gp->tbl));
	    gp->tbl = NULL;
	}
	gp->mark = FALSE;
    }
}

static void
clean_clets(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_cache_ptr	gp;
    FOR_BUF(&g_cache_tbl, g_cache_rec, gp) {
	if( gp->load != 0) {
	    /* Empty the cache */
	    gp->size = 0;
	    gp->load = 0;
	    Free((pointer) (gp->tbl));
	    gp->tbl = NULL;
	}
    }
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
    return;
}

void
Cache_ops_Install_Functions()
{
    // Add builtin functions
    Add_ExtAPI_Function("clean_clets", "1", FALSE,
                        GLmake_arrow(GLmake_void(),GLmake_void()),
                        clean_clets);
}



int
Make_RefVar()
{
    g_ptr node;
    node = NULL;
    push_buf(&ref_var_tbl, (pointer) &node);
    ref_var_cnt++;
    return( ref_var_cnt-1 );
}

g_ptr
Get_RefVar(int ref_var)
{
    g_ptr node;
    node = *((g_ptr *) locate_buf(&ref_var_tbl, ref_var));
    return(node);
}

void
Set_RefVar(int ref_var, g_ptr res)
{
    store_buf(&ref_var_tbl, ref_var, (pointer) &res);
}

void
Mark_RefVar(int ref_var)
{
    g_ptr node;
    node = *((g_ptr *) locate_buf(&ref_var_tbl, ref_var));
    Mark(node);
}

void
Sweep_RefVars()
{
    g_ptr *np;
    FOR_BUF(&ref_var_tbl, g_ptr, np) {
	if( *np != NULL ) {
	    if( GET_MARK(*np) == 0 )
		*np = NULL;
	}
    }
}

static unint
g_rec_hash_rec(pointer p, unint n, unint sz)
{
    if( sz == 0 ) { return 13; }
    g_ptr nd = (g_ptr) p;
  repeat_g_rec_hash:
    if( nd == NULL ) return( 1 );
    switch( GET_TYPE(nd) ) {
	case CONS_ND:
	    return( (931*g_rec_hash_rec(GET_CONS_HD(nd),n,sz-1) +
		     137*g_rec_hash_rec(GET_CONS_TL(nd),n,sz-1)) % n);
	case APPLY_ND:
	    return( (931*(PTR2UINT(GET_APPLY_LEFT(nd))) +
		         (PTR2UINT(GET_APPLY_RIGHT(nd)))) % n );
	case LEAF:
	    switch( GET_LEAF_TYPE(nd) ) {
                case INT:
		    return( Arbi_hash(GET_AINT(nd), n) );
                case STRING:
		    return( (PTR2UINT(GET_STRING(nd))) % n );
                case BOOL:
		    return( (((unsigned int) GET_BOOL(nd))) % n );
                case BEXPR:
		    return( (((unsigned long int) GET_BEXPR(nd))) % n );
		case PRIM_FN:
		    if( !IS_REF_VAR(nd) )
			return( (((unsigned int) GET_PRIM_FN(nd))) % n );
		    nd = Get_RefVar(GET_REF_VAR(nd));
		    goto repeat_g_rec_hash;
		case EXT_OBJ:
		    return( (((unsigned long int)GET_EXT_OBJ(nd))) % n);
                default:
                    DIE("Unexpected cache argument value. Consult guru 3!");
	    }
	    break;
	default:
	    DIE("Unexpected cache argument value. GURU\n");
    }
}

unint
G_rec_hash(pointer p, unint n)
{
    unint res = g_rec_hash_rec(p, n, 10);
    return res;
}

bool
G_rec_equ(pointer p1, pointer p2)
{
    int		type, ltype;
    arbi_T	ai1, ai2;
    g_ptr n1 = (g_ptr) p1;
    g_ptr n2 = (g_ptr) p2;

  restart_g_rec_equ:
    if( n1 == n2 )
	return( TRUE );
    if( n1 == NULL || n2 == NULL )
	return( FALSE );
    if( (type = GET_TYPE(n1)) != GET_TYPE(n2))
	return( FALSE );
    switch( type ) {
	case CONS_ND:
	    if( !G_rec_equ(GET_CONS_HD(n1), GET_CONS_HD(n2)) ) {return(FALSE);}
	    n1 = GET_CONS_TL(n1);
	    n2 = GET_CONS_TL(n2);
	    goto restart_g_rec_equ;
	case APPLY_ND:
	    return( n1 == n2 );
	case LEAF:
	    if( (ltype = GET_LEAF_TYPE(n1)) != GET_LEAF_TYPE(n2) )
		return(FALSE);
	    switch( ltype ) {
                case INT:
                    ai1 = GET_AINT(n1);
                    ai2 = GET_AINT(n2);
                    if( ai1 == ai2 )
                        return( TRUE );
                    if( Arbi_cmp(ai1, ai2) == arbi_EQ )
                        return( TRUE );
                    return( FALSE );
                case STRING:
                    return( STREQ(GET_STRING(n1), GET_STRING(n2)) );
                case BOOL:
		    return( B_Equal(GET_BOOL(n1), GET_BOOL(n2)) );
                case BEXPR:
		    return( BE_Equal(GET_BEXPR(n1), GET_BEXPR(n2)) );
		case PRIM_FN:
		    if( GET_PRIM_FN(n1) != GET_PRIM_FN(n2) ) { return FALSE; }
		    switch( GET_PRIM_FN(n1) ) {
			case P_REF_VAR:
			    n1 = Get_RefVar(GET_REF_VAR(n1));
			    n2 = Get_RefVar(GET_REF_VAR(n2));
			    goto restart_g_rec_equ;
			case P_PRINTF:
			case P_FPRINTF:
			case P_SPRINTF:
			case P_EPRINTF:
			case P_SSCANF:
			    return(
				GET_PRINTF_STRING(n1) == GET_PRINTF_STRING(n2)
			    );
			case P_EXTAPI_FN:
			    return( GET_EXTAPI_FN(n1) == GET_EXTAPI_FN(n2) );
			case P_FILEFP:
			    return(GET_FILE_IO_PTR(n1) == GET_FILE_IO_PTR(n2));
			case P_FAIL:
			    return(
				M_GET_FAIL_STRING(n1) == M_GET_FAIL_STRING(n2)
			    );
			case P_DEBUG:
			    return(
				M_GET_DEBUG_STRING(n1) == M_GET_DEBUG_STRING(n2)
			    );
			default:
			    return( TRUE );
		    }
		case EXT_OBJ:
		    {
			unint class = GET_EXT_OBJ_CLASS(n1);
			ASSERT( class == GET_EXT_OBJ_CLASS(n2) );
			ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
			formula eq = op->eq_fn(GET_EXT_OBJ(n1),
					       GET_EXT_OBJ(n2),
					       TRUE);
			if( eq == B_One() ) {
			    return TRUE;
			} else {
			    return FALSE;
			}
		    }
                default:
                    DIE("Should never happen!");
	    }
	    break;
	default:
	    DIE("Should never happen!");
    }
}

/************************************************************************/
/*			PRIVATE FUNCTIONS				*/
/************************************************************************/

static bool
cache_eq(g_ptr n1, g_ptr n2)
{
    int		type, ltype;
    arbi_T	ai1, ai2;

  restart_cache_eq:
    if( n1 == n2 )
	return( TRUE );
    if( n1 == NULL || n2 == NULL )
	return( FALSE );
    if( (type = GET_TYPE(n1)) != GET_TYPE(n2))
	return( FALSE );
    switch( type ) {
	case CONS_ND:
	    if( cache_eq(GET_CONS_HD(n1), GET_CONS_HD(n2)) )
		return( cache_eq(GET_CONS_TL(n1), GET_CONS_TL(n2)) );
	    else
		return( FALSE);
	case APPLY_ND:
	    return( n1 == n2 );
	case LEAF:
	    if( (ltype = GET_LEAF_TYPE(n1)) != GET_LEAF_TYPE(n2) )
		return(FALSE);
	    switch( ltype ) {
                case INT:
                    ai1 = GET_AINT(n1);
                    ai2 = GET_AINT(n2);
                    if( ai1 == ai2 )
                        return( TRUE );
                    if( Arbi_cmp(ai1, ai2) == arbi_EQ )
                        return( TRUE );
                    return( FALSE );
                case STRING:
                    return( STREQ(GET_STRING(n1), GET_STRING(n2)) );
                case BOOL:
		    return( B_Equal(GET_BOOL(n1), GET_BOOL(n2)) );
                case BEXPR:
		    return( BE_Equal(GET_BEXPR(n1), GET_BEXPR(n2)) );
		case PRIM_FN:
		    if( GET_PRIM_FN(n1) != GET_PRIM_FN(n2) ) { return FALSE; }
		    switch( GET_PRIM_FN(n1) ) {
			case P_REF_VAR:
			    n1 = Get_RefVar(GET_REF_VAR(n1));
			    n2 = Get_RefVar(GET_REF_VAR(n2));
			    goto restart_cache_eq;
			case P_PRINTF:
			case P_FPRINTF:
			case P_SPRINTF:
			case P_EPRINTF:
			case P_SSCANF:
			    return(
				GET_PRINTF_STRING(n1) == GET_PRINTF_STRING(n2)
			    );
			case P_EXTAPI_FN:
			    return( GET_EXTAPI_FN(n1) == GET_EXTAPI_FN(n2) );
			case P_FILEFP:
			    return(GET_FILE_IO_PTR(n1) == GET_FILE_IO_PTR(n2));
			case P_FAIL:
			    return(
				M_GET_FAIL_STRING(n1) == M_GET_FAIL_STRING(n2)
			    );
			case P_DEBUG:
			    return(
				M_GET_DEBUG_STRING(n1) == M_GET_DEBUG_STRING(n2)
			    );
			default:
			    return( TRUE );
		    }
		case EXT_OBJ:
		    {
			unint class = GET_EXT_OBJ_CLASS(n1);
			ASSERT( class == GET_EXT_OBJ_CLASS(n2) );
			ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
			formula eq = op->eq_fn(GET_EXT_OBJ(n1),
					       GET_EXT_OBJ(n2),
					       TRUE);
			if( eq == B_One() ) {
			    return TRUE;
			} else {
			    return FALSE;
			}
		    }
                default:
                    DIE("Unexpected cache argument value. Consult guru 1!");
	    }
	    break;
	default:
	    DIE("Unexpected cache argument value. Consult guru 2!");
    }
}

static unint
main_cache_hash(g_ptr argl, unint size)
{
    unint sum;
    sum = 0;
    while( argl != NULL ) {
	sum = 931*sum + cache_hash(GET_CONS_HD(argl), size << 4);
	argl = GET_CONS_TL(argl);
    }
    sum = sum % size;
    return(sum);
}

static unsigned int
cache_hash(g_ptr nd, unint n)
{
    if( nd == NULL )
	return( 0 );

    switch( GET_TYPE(nd) ) {
	case CONS_ND:
	    return( (931*(PTR2UINT(GET_CONS_HD(nd))) +
		         (PTR2UINT(GET_CONS_TL(nd)))) % n );
	case APPLY_ND:
	    return( (931*(PTR2UINT(GET_APPLY_LEFT(nd))) +
		         (PTR2UINT(GET_APPLY_RIGHT(nd)))) % n );
	case LEAF:
	    switch( GET_LEAF_TYPE(nd) ) {
                case INT:
		    return( Arbi_hash(GET_AINT(nd), n) );
                case STRING:
		    return( (PTR2UINT(GET_STRING(nd)))%n );
                case BOOL:
		    return( ((unsigned int) GET_BOOL(nd)) % n );
                case BEXPR:
		    return( ((unsigned long int) GET_BEXPR(nd)) % n );
		case PRIM_FN:
		    return( ((unsigned int) GET_PRIM_FN(nd)) % n );
		case EXT_OBJ:
		    return( ((unsigned long int) GET_EXT_OBJ(nd)) % n );
                default:
                    DIE("Unexpected cache argument value. Consult guru 3!");
	    }
	    break;
	default:
	    DIE("Unexpected cache argument value. GURU\n");
    }
}

static int
new_prime(unint cur_size)
{
    unint i;
    i = 0;
    while( primes[i] <= cur_size )
        i++;
    return( primes[i] );
}
