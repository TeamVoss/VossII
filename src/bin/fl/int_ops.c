/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 2017			*/
/*									*/
/************************************************************************/
#include "int_ops.h"
#include "graph.h"

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr	    strings;
extern g_ptr	    void_nd;

/***** PRIVATE VARIABLES *****/


/* ----- Forward definitions local functions ----- */

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Int_ops_Init()
{
    // Any needed initialization code
}


/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
do_srandom(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    srandom(GET_INT(r));
    MAKE_REDEX_VOID(redex);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

#define NBR_BITS_PER_CHUNK  31
static void
do_random2(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    int bits = GET_INT(r);
    arbi_T res = Arbi_FromInt(0);
    int chunk = 1 << NBR_BITS_PER_CHUNK;
    arbi_T ws = Arbi_FromInt(chunk);
    while( bits > NBR_BITS_PER_CHUNK ) {
	res = Arbi_mlt(res, ws);
	res = Arbi_add(res, Arbi_FromInt(random() & (chunk-1)));
	bits -= NBR_BITS_PER_CHUNK;
    }
    res = Arbi_mlt(res, Arbi_FromInt(1 << bits));
    res = Arbi_add(res, Arbi_FromInt(random() & ((1 << bits)-1)));
    MAKE_REDEX_AINT(redex, res);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}
#undef NBR_BITS_PER_CHUNK

static void
iPLUS(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_add(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iUNARY_MINUS(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_sub(Arbi_FromInt(0), ai));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iMINUS(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_sub(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iTIMES(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_mlt(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iDIVIDE(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    if( Arbi_cmp(ai2, Arbi_FromInt(0)) == arbi_EQ ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("Division by zero"));
    } else {
	MAKE_REDEX_AINT(redex, Arbi_div(ai1, ai2));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iMOD(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    if( Arbi_cmp(ai2, Arbi_FromInt(0)) == arbi_EQ ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("Mod of zero"));
    } else {
	MAKE_REDEX_AINT(redex, Arbi_mod(ai1, ai2));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iGCD(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_gcd(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iNOT(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_bvNOT(ai1));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iAND(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_bvAND(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_bvOR(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iXOR(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    MAKE_REDEX_AINT(redex, Arbi_bvXOR(ai1, ai2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iLESS(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    int cmp_res = Arbi_cmp(ai1, ai2);
    if( cmp_res == arbi_LESS ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iLEQ(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    int cmp_res = Arbi_cmp(ai1, ai2);
    if( cmp_res == arbi_LESS || cmp_res == arbi_EQ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iGREATER(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    int cmp_res = Arbi_cmp(ai1, ai2);
    if( cmp_res == arbi_GREAT ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
iGEQ(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    arbi_T ai1 = GET_AINT(GET_APPLY_RIGHT(l));
    arbi_T ai2 = GET_AINT(r);
    int cmp_res = Arbi_cmp(ai1, ai2);
    if( cmp_res == arbi_GREAT || cmp_res == arbi_EQ) {
	MAKE_REDEX_BOOL(redex, B_One());
    } else {
	MAKE_REDEX_BOOL(redex, B_Zero());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}




void
Int_ops_Install_Functions()
{
    // Add builtin functions
    Add_ExtAPI_Function("srandom", "1", FALSE,
			GLmake_arrow(GLmake_int(),GLmake_void()),
			do_srandom);

    Add_ExtAPI_Function("random2", "1", FALSE,
			GLmake_arrow(GLmake_int(),GLmake_int()),
			do_random2);

    Add_ExtAPI_Function("+", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iPLUS);
    Insert_infix("+", 7);

    Add_ExtAPI_Function("!unary_minus!", "1", FALSE,
			GLmake_arrow(GLmake_int(), GLmake_int()),
			iUNARY_MINUS);

    Add_ExtAPI_Function("-", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iMINUS);
    Insert_infix_unary("-", 7, "!unary_minus!");

    Add_ExtAPI_Function("*", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iTIMES);
    Insert_infix("*", 8);

    Add_ExtAPI_Function("/", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iDIVIDE);
    Insert_infix("/", 8);

    Add_ExtAPI_Function("%", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iMOD);
    Insert_infix("%", 8);

    Add_ExtAPI_Function("gcd", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iGCD);

    Add_ExtAPI_Function("intAND", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iAND);
    Insert_infix("intAND", 4);

    Add_ExtAPI_Function("intNOT", "11", FALSE,
			 GLmake_arrow(GLmake_int(), GLmake_int()),
			iNOT);

    Add_ExtAPI_Function("intOR", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iOR);
    Insert_infix("intOR", 3);

    Add_ExtAPI_Function("intXOR", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_int())),
			iXOR);
    Insert_infix("intXOR", 4);

    Add_ExtAPI_Function("<", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_bool())),
			iLESS);
    Insert_infix("<", 5);

    Add_ExtAPI_Function("<=", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_bool())),
			iLEQ);
    Insert_infix("<=", 5);

    Add_ExtAPI_Function(">", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_bool())),
			iGREATER);
    Insert_infix(">", 5);

    Add_ExtAPI_Function(">=", "11", FALSE,
			GLmake_arrow(GLmake_int(),
				     GLmake_arrow(GLmake_int(), GLmake_bool())),
			iGEQ);
    Insert_infix(">=", 5);


}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

