#include "compile.h"
#include "graph.h"
#include "prefs_ext.h"
/************************************************************************/
/*			Global Variables				*/
/************************************************************************/
extern bool		compile_to_C_flag;
extern str_mgr  	strings;

/************************************************************************/
/*			Local Variables					*/
/************************************************************************/
static int		uniq_name_cnt = 0;
static char		buf[20];

/************************************************************************/
/*			Local Functions					*/
/************************************************************************/
/* -----Function prototypes for local functions-----*/
static g_ptr		mk_uniq_names(g_ptr node);
static g_ptr		remove_aliases(g_ptr node);
static g_ptr		lift_lets_and_letrecs(g_ptr node);
static g_ptr		substitute(string old, g_ptr new_var, g_ptr node);
static int		is_let_or_letrec(g_ptr node, string *namep,
					 g_ptr *exprp, g_ptr *inexprp);
/************************************************************************/
/*			Public Functions				*/
/************************************************************************/

#ifdef CHECK_REF_CNTS
#define Check_Ref_Cnts(node)	check_ref_cnts(node)
#else
#define Check_Ref_Cnts(node)	
#endif

g_ptr
Optimise(g_ptr node)
{
    Check_Ref_Cnts(node);

    uniq_name_cnt = 0;
    node = mk_uniq_names(node);

    Check_Ref_Cnts(node);

    node = remove_aliases(node);

    Check_Ref_Cnts(node);

    node = lift_lets_and_letrecs(node);
    Check_Ref_Cnts(node);

    return(node);
}


/************************************************************************/
/*			Local Functions					*/
/************************************************************************/

static g_ptr
mk_uniq_names(g_ptr node)
{
    g_ptr	E, F, res;
    string	var, var2;
    int		line;

    if( node == NULL )
        return(node);
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
            E = mk_uniq_names(GET_APPLY_LEFT(node));
            F = mk_uniq_names(GET_APPLY_RIGHT(node));
	    if( E == GET_APPLY_LEFT(node) && F == GET_APPLY_RIGHT(node) )
		return( node );
            res = Make_APPL_ND(E, F);
	    MoveTypeHint(node, res, FALSE);
	    return( res );
        case LAMBDA_ND:
	    Sprintf(buf, "_ZZ'%d", uniq_name_cnt++);
	    var2 = wastrsave(&strings, buf);
	    var = GET_LAMBDA_VAR(node);
	    line = GET_LAMBDA_LINE_NBR(node);
	    /* Name capture so rename lambda variable */
	    res = Make_Lambda(var2, substitute(var, Make_VAR_leaf(var2),
					mk_uniq_names(GET_LAMBDA_BODY(node))));
	    SET_LAMBDA_LINE_NBR(res, line);
	    MoveTypeHint(node, res, FALSE);
	    return res;
        case CONS_ND:
            ASSERT(GET_CONS_HD(node) == NULL && GET_CONS_TL(node) == NULL);
            return( node );
        case LEAF:
            return( node );
        default:
            DIE("Impossible");
    }
}

static g_ptr
remove_aliases(g_ptr node)
{
    string	var;
    g_ptr	E, F, res;
    int		line;

    if( node == NULL )
        return(node);
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
	    if( is_let_or_letrec(node, &var, &E, &F) == 1 ) {
		if( IS_LEAF(E) && IS_VAR(E) ) {
		    /* LET var = var' in F */
		    return( substitute(var, E, remove_aliases(F)) );
		}
	    }
            E = remove_aliases(GET_APPLY_LEFT(node));
            F = remove_aliases(GET_APPLY_RIGHT(node));
	    res = Make_APPL_ND(E, F);
	    MoveTypeHint(node, res, FALSE);
	    return res;
        case LAMBDA_ND:
	    var = GET_LAMBDA_VAR(node);
	    line = GET_LAMBDA_LINE_NBR(node);
            res = Make_Lambda(GET_LAMBDA_VAR(node),
			      remove_aliases(GET_LAMBDA_BODY(node)));
	    SET_LAMBDA_LINE_NBR(res, line);
	    MoveTypeHint(node, res, FALSE);
	    return res;
        case CONS_ND:
	    ASSERT(GET_CONS_HD(node) == NULL && GET_CONS_TL(node) == NULL);
	    return( node );
        case LEAF:
	    return( node );
        default:
            DIE("Impossible");
    }
}


static g_ptr
lift_lets_and_letrecs(g_ptr node)
{
    string	var;
    g_ptr	E, F, lson, rson, res;
    int		line;

    if( node == NULL )
        return(node);
    switch( GET_TYPE(node) ) {
	    case APPLY_ND:
	    rson = lift_lets_and_letrecs(GET_APPLY_RIGHT(node));
	    lson = GET_APPLY_LEFT(node);
	    if( !IS_APPLY(lson) ) {
		if( rson == GET_APPLY_RIGHT(node) )
		    return( node );
		res = Make_APPL_ND(lson, rson);
		MoveTypeHint(node, res, FALSE);
		return res;
	    }
	    lson = lift_lets_and_letrecs(lson);
	    switch( is_let_or_letrec(node, &var, &E, &F) ) {
		case 0:
		    /* Application node */
		    return( Make_APPL_ND(lson, rson) );
		case 1:
		    /* LET var = E in F */
		    F = lift_lets_and_letrecs(Make_APPL_ND(F, rson));
		    return( Make_APPL_ND(Make_Lambda(var,F),E) );
		case 2:
		    /* LETREC var = E in F */
		    F = lift_lets_and_letrecs(Make_APPL_ND(F, rson));
		    return( Make_APPL_ND(Make_Lambda(var,F),
					 Make_1inp_Primitive(P_Y,
					    Make_Lambda(var,E))) );
		default:
		    DIE("Illegal return value from is_let_or_letrec");
	    }
	    break;
        case LAMBDA_ND:
	    E = lift_lets_and_letrecs(GET_LAMBDA_BODY(node));
	    if( E == GET_LAMBDA_BODY(node) )
		return( node );
	    var = GET_LAMBDA_VAR(node);
	    line = GET_LAMBDA_LINE_NBR(node);
	    res = Make_Lambda(var,E);
	    SET_LAMBDA_LINE_NBR(res, line);
            return( res );
        case CONS_ND:
	    ASSERT(GET_CONS_HD(node) == NULL && GET_CONS_TL(node) == NULL);
	    return( node );
        case LEAF:
	    return( node );
        default:
            DIE("Impossible");
    }
}

static g_ptr
substitute(string old, g_ptr new_var, g_ptr node)
{
    g_ptr	E, F, res;
    string	var, var2;
    int		line;

    if( node == NULL )
        return(node);
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
            E = substitute(old, new_var, GET_APPLY_LEFT(node));
            F = substitute(old, new_var, GET_APPLY_RIGHT(node));
	    if( E == GET_APPLY_LEFT(node) && F == GET_APPLY_RIGHT(node) )
		return( node );
	    res = Make_APPL_ND(E, F);
	    MoveTypeHint(node, res, FALSE);
            return( res );
        case LAMBDA_ND:
	    var = GET_LAMBDA_VAR(node);
	    line = GET_LAMBDA_LINE_NBR(node);
	    if( STREQ(var,old) )
		return( node );
	    if( !STREQ(var, GET_VAR(new_var)) ) {
		E = substitute(old, new_var, GET_LAMBDA_BODY(node));
		if( E == GET_LAMBDA_BODY(node) )
		    return( node );
		res = Make_Lambda(var,E);
		SET_LAMBDA_LINE_NBR(res, line);
		MoveTypeHint(node, res, FALSE);
		return res;
	    }
	    /* Name capture so rename lambda variable */
	    Sprintf(buf, "_Z'%d", uniq_name_cnt++);
	    var2 = wastrsave(&strings, buf);
	    E = substitute(var, Make_VAR_leaf(var2), GET_LAMBDA_BODY(node));
	    E = substitute(old, new_var, E);
	    res = Make_Lambda(var2, E);
	    SET_LAMBDA_LINE_NBR(res, line);
	    MoveTypeHint(node, res, FALSE);
	    return res;
        case CONS_ND:
            ASSERT(GET_CONS_HD(node) == NULL && GET_CONS_TL(node) == NULL);
            return( node );
        case LEAF:
	    if( IS_VAR(node) && STREQ(GET_VAR(node), old) ) {
		res = Make_VAR_leaf(GET_VAR(new_var));
		SET_VERSION(res, GET_VERSION(new_var));
		MoveTypeHint(node, res, FALSE);
		return( res );
	    }
            return( node );
        default:
            DIE("Impossible");
    }
}

static int
is_let_or_letrec(g_ptr node, string *namep, g_ptr *exprp, g_ptr *inexprp)
{
    g_ptr t1, t2, t3;
    ASSERT( IS_APPLY(node) );
    t1 = GET_APPLY_LEFT(node);
    if( IS_LAMBDA(t1) ) {
	*namep = GET_LAMBDA_VAR(t1);
	*inexprp = GET_LAMBDA_BODY(t1);
	t2 = GET_APPLY_RIGHT(node);
	*exprp = t2;
	if( IS_APPLY(t2) ) {
	    t3 = GET_APPLY_LEFT(t2);
            if(IS_LEAF(t3) && IS_PRIM_FN(t3) && GET_PRIM_FN(t3)==P_Y) {
		t3 = GET_APPLY_RIGHT(t2);
		*exprp = GET_LAMBDA_BODY(t3);
		return(2);
	    }
	}
	/* LET f = E in F */
	return(1);
    }
    return( 0 );
}

