/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 2017			*/
/*									*/
/************************************************************************/
#include "signature.h"
#include "graph.h"

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     strings;

/***** PRIVATE VARIABLES *****/

static int ckt_oidx;
static int wire_oidx;
static int expr_oidx;

static typeExp_ptr  ckt_handle_tp;
static typeExp_ptr  wire_handle_tp;
static typeExp_ptr  expr_handle_tp;

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Signature_Init()
{

    ckt_oidx  =
       Add_ExtAPI_Object("ckt_handle",NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
    wire_oidx =
       Add_ExtAPI_Object("wire_handle",NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
    expr_oidx =
       Add_ExtAPI_Object("expr_handle",NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

    ckt_handle_tp  = Get_Type("ckt_handle", NULL, TP_INSERT_FULL_TYPE);
    wire_handle_tp = Get_Type("wire_handle", NULL, TP_INSERT_FULL_TYPE);
    expr_handle_tp = Get_Type("expr_handle", NULL, TP_INSERT_FULL_TYPE);

}


/********************************************************/
/*	    EXPORTED EXTAPI FUNCTIONS    		*/
/********************************************************/

static void
Build_expr_var(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    int idx = GET_INT(r);
    MAKE_REDEX_EXT_OBJ(redex, expr_oidx, build_expr_var(idx));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
gen_build_expr_binop(enum isom_expr_binop op, g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1, arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2)
    expr_handle a1 = (expr_handle) GET_EXT_OBJ(arg1);
    expr_handle a2 = (expr_handle) GET_EXT_OBJ(arg2);
    MAKE_REDEX_EXT_OBJ(redex, expr_oidx, build_expr_binop(op, a1, a2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
build_expr_and(g_ptr redex) { gen_build_expr_binop(BAnd, redex); }

static void
build_expr_or(g_ptr redex) { gen_build_expr_binop(BOr, redex); }

static void
build_expr_xor(g_ptr redex) { gen_build_expr_binop(BXor, redex); }

static void
build_expr_add(g_ptr redex) { gen_build_expr_binop(BAdd, redex); }

static void
build_expr_sub(g_ptr redex) { gen_build_expr_binop(BSub, redex); } 

static void
build_expr_mul(g_ptr redex) { gen_build_expr_binop(BMul, redex); } 

static void
build_expr_div(g_ptr redex) { gen_build_expr_binop(BDiv, redex); } 

static void
build_expr_mod(g_ptr redex) { gen_build_expr_binop(BMod, redex); } 

static void
build_expr_lsr(g_ptr redex) { gen_build_expr_binop(BLsr, redex); } 

static void
build_expr_lsl(g_ptr redex) { gen_build_expr_binop(BLsl, redex); } 

static void
build_expr_asr(g_ptr redex) { gen_build_expr_binop(BAsr, redex); } 

static void
build_expr_not(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    expr_handle a = (expr_handle) GET_EXT_OBJ(r);
    MAKE_REDEX_EXT_OBJ(redex, expr_oidx, build_expr_unop(UNot, a));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


static void
Build_expr_slice(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1, arg2, arg3;
    EXTRACT_3_ARGS(redex, arg1, arg2, arg3);
    expr_handle e = (expr_handle) GET_EXT_OBJ(arg1);
    unsigned beg = GET_INT(arg2);
    unsigned end = GET_INT(arg3);
    MAKE_REDEX_EXT_OBJ(redex, expr_oidx, build_expr_slice(e, beg, end));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Build_expr_merge(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1, arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2);
    expr_handle a1 = (expr_handle) GET_EXT_OBJ(arg1);
    expr_handle a2 = (expr_handle) GET_EXT_OBJ(arg2);
    MAKE_REDEX_EXT_OBJ(redex, expr_oidx, build_expr_merge(a1, a2));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Build_comb(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr name, inps, outs;
    EXTRACT_3_ARGS(redex, name, inps, outs);
    circuit_handle ckt = build_group(GET_STRING(name));
    circuit_handle comb = build_comb(ckt);
    for(g_ptr cur = inps; !IS_NIL(cur); cur = GET_TL(cur)) {
	g_ptr p = GET_HD(cur);
	string f_name = GET_STRING(GET_FST(p));
	string a_name = GET_STRING(GET_SND(p));
	build_group_add_input(ckt, a_name, f_name);
	build_comb_add_input(comb, f_name);
    }
    for(g_ptr cur = outs; !IS_NIL(cur); cur = GET_TL(cur)) {
	g_ptr t = GET_HD(cur);
	string f_name = GET_STRING(GET_FST(t));
	string a_name = GET_STRING(GET_FST(GET_SND(t)));
	expr_handle e = (expr_handle) GET_EXT_OBJ(GET_SND(GET_SND(t)));
	build_group_add_output(ckt, a_name, f_name);
	build_comb_add_output(comb, f_name, e);
    }
    MAKE_REDEX_EXT_OBJ(redex, ckt_oidx, ckt);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Build_delay(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr name, inp, out;
    EXTRACT_3_ARGS(redex, name, inp, out);
    circuit_handle ckt = build_group(GET_STRING(name));
    string i_name = GET_STRING(inp);
    string o_name = GET_STRING(out);
    build_delay(ckt, i_name, o_name);
    MAKE_REDEX_EXT_OBJ(redex, ckt_oidx, ckt);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Build_hierarchy(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr name, inps, outs, subfubs;
    EXTRACT_4_ARGS(redex, name, inps, outs, subfubs);
    circuit_handle ckt = build_group(GET_STRING(name));
    for(g_ptr cur = inps; !IS_NIL(cur); cur = GET_TL(cur)) {
	g_ptr p = GET_HD(cur);
	string f_name = GET_STRING(GET_FST(p));
	string a_name = GET_STRING(GET_SND(p));
	build_group_add_input(ckt, a_name, f_name);
    }
    for(g_ptr cur = outs; !IS_NIL(cur); cur = GET_TL(cur)) {
	g_ptr p = GET_HD(cur);
	string f_name = GET_STRING(GET_FST(p));
	string a_name = GET_STRING(GET_SND(p));
	build_group_add_output(ckt, a_name, f_name);
    }
    for(g_ptr cur = subfubs; !IS_NIL(cur); cur = GET_TL(cur)) {
	build_group_add_child(ckt, (circuit_handle) GET_EXT_OBJ(GET_HD(cur)));
    }
    MAKE_REDEX_EXT_OBJ(redex, ckt_oidx, ckt);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
Compute_Signature(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr c;
    EXTRACT_1_ARG(redex, c);
    circuit_handle ckt = (circuit_handle) GET_EXT_OBJ(c);
//    freeze_circuit(ckt);
    string ssig = wastrsave(&strings, tprintf("%ld", (ui) sign(ckt)));
//    free_circuit(ckt);
    MAKE_REDEX_STRING(redex, ssig);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}



void
Signature_Install_Functions()
{
#if 1
    if(  0 ) {
	Build_expr_var(NULL);
	build_expr_and(NULL);
	build_expr_or(NULL);
	build_expr_xor(NULL);
	build_expr_add(NULL);
	build_expr_sub(NULL);
	build_expr_mul(NULL);
	build_expr_div(NULL);
	build_expr_mod(NULL);
	build_expr_lsr(NULL);
	build_expr_lsl(NULL);
	build_expr_asr(NULL);
	build_expr_not(NULL);
	Build_expr_slice(NULL);
	Build_expr_merge(NULL);
	Build_comb(NULL);
	Build_delay(NULL);
	Build_hierarchy(NULL);
	Compute_Signature(NULL);
    }
#else
    // Add builtin functions
    Add_ExtAPI_Function("build_expr_var", "1", FALSE,
			GLmake_arrow(GLmake_int(), expr_handle_tp),
			Build_expr_var);

    Add_ExtAPI_Function("build_expr_and", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_and);

    Add_ExtAPI_Function("build_expr_or", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_or);

    Add_ExtAPI_Function("build_expr_xor", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_xor);

    Add_ExtAPI_Function("build_expr_add", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_add);

    Add_ExtAPI_Function("build_expr_sub", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_sub);

    Add_ExtAPI_Function("build_expr_mul", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_mul);

    Add_ExtAPI_Function("build_expr_div", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_div);

    Add_ExtAPI_Function("build_expr_mod", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_mod);

    Add_ExtAPI_Function("build_expr_lsr", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_lsr);

    Add_ExtAPI_Function("build_expr_lsl", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_lsl);

    Add_ExtAPI_Function("build_expr_asr", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			build_expr_asr);

    Add_ExtAPI_Function("build_expr_not", "1", FALSE,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp),
			build_expr_not);

    Add_ExtAPI_Function("build_expr_slice", "111", FALSE,
			    GLmake_arrow(expr_handle_tp,
				GLmake_arrow(GLmake_int(),
				    GLmake_arrow(GLmake_int(),
						 expr_handle_tp))),
			Build_expr_slice);

    Add_ExtAPI_Function("build_expr_merge", "11", FALSE,
			GLmake_arrow(expr_handle_tp,
			    GLmake_arrow(expr_handle_tp,
					    expr_handle_tp)),
			Build_expr_merge);

    Add_ExtAPI_Function("build_comb", "111", FALSE,
			GLmake_arrow(GLmake_string(),
			    GLmake_arrow(GLmake_list(GLmake_tuple(
							GLmake_string(),
							GLmake_string())),
				GLmake_arrow(
				    GLmake_list(
					GLmake_tuple(GLmake_string(),
					    GLmake_tuple(
						GLmake_string(),
						expr_handle_tp))),
				    ckt_handle_tp))),
			Build_comb);

    Add_ExtAPI_Function("build_delay", "111", FALSE,
			GLmake_arrow(GLmake_string(),
			    GLmake_arrow(GLmake_string(),
				GLmake_arrow(GLmake_string(), ckt_handle_tp))),
			Build_delay);

    Add_ExtAPI_Function("build_hierarchy", "1111", FALSE,
			GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(
				GLmake_list(
				    GLmake_tuple(GLmake_string(),
						 GLmake_string())),
				GLmake_arrow(
				    GLmake_list(
					GLmake_tuple(GLmake_string(),
						     GLmake_string())),
				    GLmake_arrow(
					GLmake_list(ckt_handle_tp),
					ckt_handle_tp)))),
			Build_hierarchy);

    Add_ExtAPI_Function("get_signature", "1", FALSE,
			GLmake_arrow(ckt_handle_tp,GLmake_string()),
			Compute_Signature);


#endif
}

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

