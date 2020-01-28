/************************************************************************/
/*									*/
/*		Copyright: Carl-Johan Seger, 2018			*/
/*									*/
/************************************************************************/
#include "float.h"
#include "graph.h"

#include <math.h>

/* ------------- Global variables ------------- */

/********* Global variables referenced ***********/
extern str_mgr     strings;

/***** PRIVATE VARIABLES *****/

static int float_oidx;
static typeExp_ptr  float_handle_tp;

static rec_mgr	    float_rec_mgr;
static float_ptr    free_list;

/********************************************************/
/*                    LOCAL FUNCTIONS    		*/
/********************************************************/

static float_ptr
get_float_rec(double v)
{
    if( free_list != NULL ) {
	float_ptr res = free_list;
	free_list = free_list->u.next;
	res->flag = 0;
	res->u.f = v;
	return res;
    } else {
	float_ptr res = (float_ptr) new_rec(&float_rec_mgr);
	res->flag = 0;
	res->u.f = v;
	return res;
    }
}

static bool
double2arbi(double d, arbi_T *resp)
{
    int exp;
    switch( fpclassify(d) ) {
	case FP_NAN:
	    return FALSE;
	case FP_INFINITE:
	    return FALSE;
	case FP_ZERO:
	case FP_SUBNORMAL:
	    *resp = Arbi_FromInt(0);
	    return( TRUE );
	case FP_NORMAL:
	    {
		bool negative = FALSE;
		if( d < 0 ) {
		    negative = TRUE;
		    d = -1 * d;
		}
		double mant = frexp(d, &exp);
		*resp = Arbi_FromInt(0);
		for(int i = 0; i < exp; i++) {
		    *resp = Arbi_mlt(*resp, Arbi_FromInt(2));
		    mant = 2*mant;
		    if( mant >= 1 ) {
			*resp = Arbi_add(*resp, Arbi_FromInt(1));
			mant = mant-1;
		    }
		}
		if( negative ) {
		    *resp = Arbi_mlt(*resp, Arbi_FromInt(-1));
		}
		return TRUE;
	    }
	default:
	    DIE("Should never happen");
    }
}

static void
mark_float_fn(pointer p)
{
    float_ptr fp = (float_ptr) p;
    fp->flag = 1;
    return;
}

static void
sweep_float_fn(void)
{
    float_ptr fp;
    free_list = NULL;
    FOR_REC(&float_rec_mgr, float_ptr, fp) {
	if( fp->flag == 1 ) {
	    fp->flag = 0;
	} else {
	    fp->u.next = free_list;
	    free_list = fp;
	}
    }
}

static void
save_float_fn(FILE *fp, pointer p)
{
    float_ptr frp = (float_ptr) p;
    union {
	double		d;
	unsigned char	bytes[sizeof(double)];
    } u;
    u.d = frp->u.f;
    for(unint i = 0; i < (unint) sizeof(double); i++) {
	fprintf(fp, "%02x", u.bytes[i]);
    }
    fputc('\n', fp);	// Add newline for human readability
}

static pointer
load_float_fn(FILE *fp)
{
    union {
	double		d;
	unsigned char	bytes[sizeof(double)];
    } u;
    for(unint i = 0; i < (unint) sizeof(double); i++) {
	int v;
	if( fscanf(fp, "%2x", &v) != 1 ) {
	    return NULL;
	}
	u.bytes[i] = v;
    }
    fgetc(fp);	// Eat new line
    float_ptr res = get_float_rec(u.d);
    return( (pointer) res );
}


static string
float2str_fn(pointer p)
{
    char buf[20];
    float_ptr fp = (float_ptr) p;
    Sprintf(buf, "%g", fp->u.f);
    return( wastrsave(&strings, buf) );
}

static formula
float_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    float_ptr	fp1 = (float_ptr) p1;
    float_ptr	fp2 = (float_ptr) p2;
    if( fp1->u.f == fp2->u.f ) {
	return( B_One() );
    } else {
	return( B_Zero() );
    }
}

static void
str2float(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    string s = GET_STRING(arg1);
    double d;
    if( sscanf(s, "%lf", &d) != 1 ) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot convert '%s' to a float", s));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    MAKE_REDEX_EXT_OBJ(redex, float_oidx, get_float_rec(d));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
int2float(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1 = GET_APPLY_RIGHT(redex);
    string s = Arbi_ToString(GET_AINT(arg1),10);
    double d;
    if( sscanf(s, "%lf", &d) != 1 ) {
        MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot convert '%s' to a float", s));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    MAKE_REDEX_EXT_OBJ(redex, float_oidx, get_float_rec(d));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
float2str(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg = GET_APPLY_RIGHT(redex);
    float_ptr a = (float_ptr) GET_EXT_OBJ(arg);
    MAKE_REDEX_STRING(redex, float2str_fn((pointer) a));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
gen_double_binop(double (*binop)(double a1, double a2), g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1, arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2)
    float_ptr a1 = (float_ptr) GET_EXT_OBJ(arg1);
    float_ptr a2 = (float_ptr) GET_EXT_OBJ(arg2);
    double res = binop(a1->u.f, a2->u.f);
    MAKE_REDEX_EXT_OBJ(redex, float_oidx, get_float_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static double fadd(double a1, double a2) { return( a1 + a2 ); }
static double fsub(double a1, double a2) { return( a1 - a2 ); }
static double fmul(double a1, double a2) { return( a1 * a2 ); }
static double fdiv(double a1, double a2) { return( a1 / a2 ); }

static void float_add(g_ptr redex) { gen_double_binop(fadd, redex); }
static void float_sub(g_ptr redex) { gen_double_binop(fsub, redex); }
static void float_mul(g_ptr redex) { gen_double_binop(fmul, redex); }
static void float_div(g_ptr redex) { gen_double_binop(fdiv, redex); }
static void fpow(g_ptr redex)	   { gen_double_binop(pow, redex); }

static void
gen_double_unop(double (*unop)(double a), g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg;
    EXTRACT_1_ARG(redex, arg)
    float_ptr a = (float_ptr) GET_EXT_OBJ(arg);
    double res = unop(a->u.f);
    MAKE_REDEX_EXT_OBJ(redex, float_oidx, get_float_rec(res));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void fsqrt(g_ptr redex)  { gen_double_unop(sqrt, redex); }
static void fexp(g_ptr redex)   { gen_double_unop(exp, redex); }
static void flog(g_ptr redex)   { gen_double_unop(log, redex); }
static void flog10(g_ptr redex) { gen_double_unop(log10, redex); }
static void fsin(g_ptr redex)   { gen_double_unop(sin, redex); }
static void fcos(g_ptr redex)   { gen_double_unop(cos, redex); }
static void ftan(g_ptr redex)   { gen_double_unop(tan, redex); }
static void fasin(g_ptr redex)  { gen_double_unop(asin, redex); }
static void facos(g_ptr redex)  { gen_double_unop(acos, redex); }
static void fatan(g_ptr redex)  { gen_double_unop(atan, redex); }

static void
gen_double_relop(bool (*relop)(double a1, double a2), g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg1, arg2;
    EXTRACT_2_ARGS(redex, arg1, arg2)
    float_ptr a1 = (float_ptr) GET_EXT_OBJ(arg1);
    float_ptr a2 = (float_ptr) GET_EXT_OBJ(arg2);
    bool res = relop(a1->u.f, a2->u.f);
    MAKE_REDEX_BOOL(redex, (res? B_One() : B_Zero()));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static bool fgt(double a1, double a2) { return( a1 > a2 ); }
static bool fge(double a1, double a2) { return( a1 >= a2 ); }
static bool feq(double a1, double a2) { return( a1 == a2 ); }
static bool fne(double a1, double a2) { return( a1 != a2 ); }
static bool flt(double a1, double a2) { return( a1 < a2 ); }
static bool fle(double a1, double a2) { return( a1 <= a2 ); }

static void float_gt(g_ptr redex) { gen_double_relop(fgt, redex); }
static void float_ge(g_ptr redex) { gen_double_relop(fge, redex); }
static void float_eq(g_ptr redex) { gen_double_relop(feq, redex); }
static void float_ne(g_ptr redex) { gen_double_relop(fne, redex); }
static void float_lt(g_ptr redex) { gen_double_relop(flt, redex); }
static void float_le(g_ptr redex) { gen_double_relop(fle, redex); }


static void
gen_convert_to_int(double (*cnvop)(double a), g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr arg;
    EXTRACT_1_ARG(redex, arg)
    float_ptr a = (float_ptr) GET_EXT_OBJ(arg);
    double d = cnvop(a->u.f);
    arbi_T ares;
    if( double2arbi(d, &ares) ) {
	MAKE_REDEX_AINT(redex, ares);
    } else {
	char buf[20];
	Sprintf(buf, "%g", d);
        MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot convert '%s' to an int",buf));
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void f_floor(g_ptr redex) { gen_convert_to_int(floor, redex); }
static void f_ceil(g_ptr redex) { gen_convert_to_int(ceil, redex); }
static void f_round(g_ptr redex) { gen_convert_to_int(round, redex); }

/********************************************************/
/*                    PUBLIC FUNCTIONS    		*/
/********************************************************/

void
Float_Init()
{
    new_mgr(&float_rec_mgr, sizeof(float_rec));
    free_list = NULL;
    float_oidx  = Add_ExtAPI_Object("float_handle",
				    mark_float_fn,
				    sweep_float_fn,
				    save_float_fn,
				    load_float_fn,
				    float2str_fn,
				    float_eq_fn,
				    NULL,
				    NULL);
    float_handle_tp  = Get_Type("float", NULL, TP_INSERT_FULL_TYPE);
}

void
Float_Install_Functions()
{
    // Add builtin functions

    Add_ExtAPI_Function("float2str", "1", FALSE,
			GLmake_arrow(float_handle_tp, GLmake_string()),
			float2str);

    Add_ExtAPI_Function("str2float", "1", FALSE,
			GLmake_arrow(GLmake_string(), float_handle_tp),
			str2float);

    Add_ExtAPI_Function("int2float", "1", FALSE,
			GLmake_arrow(GLmake_int(), float_handle_tp),
			int2float);

    Add_ExtAPI_Function("fadd", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    float_handle_tp)),
			float_add);

    Add_ExtAPI_Function("fsub", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    float_handle_tp)),
			float_sub);

    Add_ExtAPI_Function("fmul", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    float_handle_tp)),
			float_mul);

    Add_ExtAPI_Function("fdiv", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    float_handle_tp)),
			float_div);

    Add_ExtAPI_Function("pow", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    float_handle_tp)),
			fpow);

    Add_ExtAPI_Function("fgt", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    GLmake_bool())),
			float_gt);

    Add_ExtAPI_Function("fge", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    GLmake_bool())),
			float_ge);

    Add_ExtAPI_Function("feq", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    GLmake_bool())),
			float_eq);

    Add_ExtAPI_Function("fne", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    GLmake_bool())),
			float_ne);

    Add_ExtAPI_Function("flt", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    GLmake_bool())),
			float_lt);

    Add_ExtAPI_Function("fle", "11", FALSE,
			GLmake_arrow(float_handle_tp,
			    GLmake_arrow(float_handle_tp,
					    GLmake_bool())),
			float_le);

    Add_ExtAPI_Function("floor", "1", FALSE,
			GLmake_arrow(float_handle_tp, GLmake_int()),
			f_floor);

    Add_ExtAPI_Function("ceil", "1", FALSE,
			GLmake_arrow(float_handle_tp, GLmake_int()),
			f_ceil);

    Add_ExtAPI_Function("round", "1", FALSE,
			GLmake_arrow(float_handle_tp, GLmake_int()),
			f_round);

    Add_ExtAPI_Function("sqrt", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			fsqrt);

    Add_ExtAPI_Function("exp", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			fexp);

    Add_ExtAPI_Function("log", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			flog);

    Add_ExtAPI_Function("log10", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			flog10);

    Add_ExtAPI_Function("sin", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			fsin);

    Add_ExtAPI_Function("cos", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			fcos);

    Add_ExtAPI_Function("tan", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			ftan);

    Add_ExtAPI_Function("asin", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			fasin);

    Add_ExtAPI_Function("acos", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			facos);

    Add_ExtAPI_Function("atan", "1", FALSE,
			GLmake_arrow(float_handle_tp, float_handle_tp),
			fatan);
}

void
make_redex_float(g_ptr redex, double d)
{
    MAKE_REDEX_EXT_OBJ(redex, float_oidx, get_float_rec(d));
}

g_ptr
Make_float(double d)
{
    return Make_ext_obj(float_oidx, (pointer)get_float_rec(d));
}
