/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 1995			*/
/*									*/
/************************************************************************/
#include "memoize.h"

/***** REFERENCED GLOBAL VARIABLES *****/

/***** PRIVATE VARIABLES *****/
static hash_record	memo_tbl;

/* Forward declaration */
static bool             memo_eq(pointer p1, pointer p2);
static unsigned int     memo_hash(pointer np, unsigned int n);

/************************************************************************/
/*			GLOBAL FUNCTIONS				*/
/************************************************************************/

void
Init_Memo()
{
    create_hash(&memo_tbl, 100, memo_hash, memo_eq);
}

g_ptr
Mk_unique(g_ptr nd)
{
}


/************************************************************************/
/*			PRIVATE FUNCTIONS				*/
/************************************************************************/
static bool
memo_eq(pointer p1, pointer p2)
{
    g_ptr	n1,n2;
    int		type, ltype;
    arbi_T	ai1, ai2;

    n1 = (g_ptr) p1;
    n2 = (g_ptr) p2;
    if( n1 == n2 )
	return( TRUE );
    if( n1 == NULL || n2 == NULL )
	return( FALSE );
    if( (type = GET_TYPE(n1)) != GET_TYPE(n2))
	return( FALSE );
    switch( type ) {
	case CONS_ND:
	    return( GET_CONS_HD(n1) == GET_CONS_HD(n2) &&
		    GET_CONS_TL(n1) == GET_CONS_TL(n2) );
	case APPLY_ND:
	    return( GET_APPLY_LEFT(n1) == GET_APPLY_LEFT(n2) &&
		    GET_APPLY_RIGHT(n1) == GET_APPLY_RIGHT(n2) );
	case LEAF:
	    if( (ltype = GET_LEAF_TYPE(n1)) != GET_LEAF_TYPE(n2) )
		return(FALSE);
	    switch( ltype ) {
                case INT:
                    ai1 = GET_AINT(n1);
                    ai2 = GET_AINT(n1);
                    if( ai1 == ai2 )
                        return( TRUE );
                    if( Arbi_cmp(ai1, ai2) == arbi_EQ )
                        return( TRUE );
                    return( FALSE );
                case STRING:
                    return( STREQ(GET_STRING(n1), GET_STRING(n2)) );
                case BOOL:
		    return( B_Equal(GET_BOOL(l1), GET_BOOL(l2)) );
		case PRIM_FN:
		    return( GET_PRIM_FN(n1) == GET_PRIM_FN(n2) );
                default:
                    DIE("Unexpected memoize value. Consult guru!");
	    }
	    break;
	default:
	    DIE("Unexpected memoize value. Consult guru!");
    }
}


static unsigned int
memo_hash(pointer np, unsigned int n)
{
    g_ptr	nd;
    long int *	lp;

    nd = (g_ptr) np;

    if( nd == NULL )
	return( 0 );

    switch( GET_TYPE(nd) ) {
	case CONS_ND:
	    return( (((unsigned int) GET_CONS_HD(nd)) +
		     ((unsigned int) GET_CONS_TL(nd))) % n );
	case APPLY_ND:
	    return( (((unsigned int) GET_APPLY_LEFT(nd)) +
		     ((unsigned int) GET_APPLY_RIGHT(nd))) % n );
	case LEAF:
	    switch( GET_LEAF_TYPE(nd) ) {
                case INT:
		    lp = Arbi_ToInt(Arbi_mod(GET_AINT(nd), Arbi_FromInt(n)));
		    return( (unsigned int)(*lp) );
                case STRING:
		    return( str_hash(GET_STRING(nd), n) );
                case BOOL:
		    return( ((unsigned int) GET_BOOL(nd)) % n );
                case BEXPR:
		    return( ((unsigned int) GET_BEXPR(nd)) % n );
		case PRIM_FN:
		    return( ((unsigned int) GET_PRIM_FN(nd)) % n );
                default:
                    DIE("Unexpected memoize value. Consult guru!");
	    }
	    break;
	default:
	    DIE("Unexpected memoize value. Consult guru!");
    }
}
