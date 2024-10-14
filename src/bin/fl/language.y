%{
#include "fl.h"
#include "language.h"
#include "graph.h"
#include "prefs_ext.h"

int Dbg_en = 0;

// #define YYMAXDEPTH 10000	/* To make the parser a bit more robust */

/* --------------- External references --------------- */
extern bool	file_load;
extern bool	RCadd_debug_info;
extern char	*space_const;
extern char	*prompt;
extern str_mgr	strings;
extern int	line_nbr;
extern string	cur_file_name;
extern bool	gui_mode;

#if 1
extern int	dbg_gr_save_cnt;
#endif

extern bool	do_write;
extern FILE	*dbg_fp;

int yylex (void *);

void Exit(int);

/* --------------- Global variables --------------- */
extern symbol_tbl_ptr	symb_tbl;

/* --------------- Local variables --------------- */
static rec_mgr		tvarl_rec_mgr;
static string		it;
static string		eq_string;
static hash_record      Infix_Table;
static hash_record      Infix_Unary_Table;
static hash_record      Binder_Acc_Table;
static hash_record	type_var_tbl;
static FILE		*tmp_fp;
static char             tmp_buf[1024];
static string		s_str2float;
static string		s_crossprod;
static string		s_prefix_0;
static string		s_prefix_1;
static string		s_infix_0;
static string		s_infix_1;
static string		s_infix_2;
static string		s_infix_3;
static string		s_infix_4;
static string		s_infix_5;
static string		s_infix_6;
static string		s_infix_7;
static string		s_infix_8;
static string		s_infix_9;
static string		s_infix_unary_0;
static string		s_infix_unary_1;
static string		s_infix_unary_2;
static string		s_infix_unary_3;
static string		s_infix_unary_4;
static string		s_infix_unary_5;
static string		s_infix_unary_6;
static string		s_infix_unary_7;
static string		s_infix_unary_8;
static string		s_infix_unary_9;
static string		s_infixr_0;
static string		s_infixr_1;
static string		s_infixr_2;
static string		s_infixr_3;
static string		s_infixr_4;
static string		s_infixr_5;
static string		s_infixr_6;
static string		s_infixr_7;
static string		s_infixr_8;
static string		s_infixr_9;
static string		s_postfix;
static string		s_binder;
static string		s_nonfix;
static string		s_then_binder;
static string		s_else_binder;
static string		s_binder_with_acc;
static string		s_sz_binder_with_acc;
static string		s_free_binder;

static struct function_rec {
        string  name;
        int     prim_fn;
	int	infix;
	int	infixr;
	bool	postfix;
        bool	binder;
        bool    non_lazy; } functions[] = {
            /* name      prim_function    infix infixr postfix binder non_lazy*/
            {"bool2str", P_BOOL2STR, 		-1, -1, FALSE,FALSE,FALSE},
            {"bdd_load", P_BDD_LOAD, 		-1, -1, FALSE,FALSE,FALSE},
            {"bdd_reorder", P_BDD_REORDER,	-1, -1, FALSE,FALSE,FALSE},
            {"bdd_save", P_BDD_SAVE, 		-1, -1, FALSE,FALSE,FALSE},
            {"chr", P_CHR,			-1, -1, FALSE,FALSE,FALSE},
            {"deref", P_DEREFERENCE,		-1, -1, FALSE,FALSE,TRUE},
            {"error", P_ERROR,			-1, -1, FALSE,FALSE,FALSE},
            {"eval", P_EVAL,			-1, -1, FALSE,FALSE,FALSE},
            {"empty", P_EMPTY,			-1, -1, FALSE,FALSE,FALSE},
            {"explode", P_EXPLODE,		-1, -1, FALSE,FALSE,FALSE},
            {"exit", P_EXIT,			-1, -1, FALSE,FALSE,FALSE},
            {"fopen", P_FOPEN,                  -1, -1, FALSE,FALSE,TRUE},
            {"fclose", P_FCLOSE,                -1, -1, FALSE,FALSE,TRUE},
            {"fflush", P_FFLUSH,                -1, -1, FALSE,FALSE,TRUE},
            {"fst", P_FST,			-1, -1, FALSE,FALSE,FALSE},
            {"get_vossrc", P_GET_VOSSRC,	-1, -1, FALSE,FALSE,FALSE},
            {"hd", P_HEAD,			-1, -1, FALSE,FALSE,FALSE},
            {"help", P_HELP,			-1, -1, FALSE,FALSE,FALSE},
            {"implode", P_IMPLODE,		-1, -1, FALSE,FALSE,FALSE},
            {"int2str", P_INT2STR,		-1, -1, FALSE,FALSE,FALSE},
            {"_load", P_LOAD,			-1, -1, FALSE,FALSE,FALSE},
            {"load_plugin", P_LOAD_PLUGIN,	-1, -1, FALSE,FALSE,FALSE},
            {"ord", P_ORD, 			-1, -1, FALSE,FALSE,FALSE},
            {"print", P_PRINT,			-1, -1, FALSE,FALSE,TRUE},
            {"rvariable", P_RVAR,		-1, -1, FALSE,FALSE,FALSE},
            {"relprod_thereis", P_RELPROD_THEREIS,-1,-1,FALSE,FALSE,FALSE},
            {"relprod_forall", P_RELPROD_FORALL, -1, -1,FALSE,FALSE,FALSE},
            {"quant_forall", P_QUANT_FORALL,	-1, -1, FALSE,FALSE,FALSE},
            {"quant_thereis", P_QUANT_THEREIS,	-1, -1, FALSE,FALSE,FALSE},
            {"snd", P_SND,			-1, -1, FALSE,FALSE,FALSE},
            {"save", P_WRITE_TO_FILE,		-1, -1, FALSE,FALSE,FALSE},
            {"string_hd", P_STRING_HD,		-1, -1, FALSE,FALSE,FALSE},
            {"string_tl", P_STRING_TL,		-1, -1, FALSE,FALSE,FALSE},
            {"system", P_SYSTEM,		-1, -1, FALSE,FALSE,FALSE},
            {"SUC", P_SUC,			-1, -1, FALSE,FALSE,FALSE},
            {"tcl_eval", P_TCL_EVAL,		-1, -1, FALSE,FALSE,TRUE},
            {"tl", P_TAIL,			-1, -1, FALSE,FALSE,FALSE},
            {"time", P_TIME,			-1, -1, FALSE,FALSE,FALSE},
            {"update_vossrc", P_UPDATE_VOSSRC,	-1, -1, FALSE,FALSE,FALSE},
            {"var_order", P_VAR_ORDER,		-1, -1, FALSE,FALSE,FALSE},
            {"!", P_FORALL,			-1, -1, FALSE,TRUE, FALSE},
            {"?", P_THEREIS,			-1, -1, FALSE,TRUE, FALSE},
            {":", P_CONS,			-1,  9, FALSE,FALSE,FALSE},
            {"^", P_CAT,			 9, -1, FALSE,FALSE,FALSE},
            {"=", P_EQUAL,			 5, -1, FALSE,FALSE,FALSE},
            {"==", P_IDENTICAL,			 5, -1, FALSE,FALSE,FALSE},
            {":=", P_UPDATE_RVAR,		 1, -1, FALSE,FALSE,TRUE},
            {"!=", P_NOT_EQUAL,			 5, -1, FALSE,FALSE,FALSE},
            {"catch", P_CATCH,			 2, -1, FALSE,FALSE,FALSE},
            {"gen_catch", P_GEN_CATCH,		 2, -1, FALSE,FALSE,FALSE},
            {"fseq", P_FSEQ,			 1, -1, FALSE,FALSE,FALSE},
            {"seq", P_SEQ,			 1, -1, FALSE,FALSE,FALSE}
};

static int nbr_functions = sizeof(functions)/sizeof(struct function_rec);

static char buf[1024];

/* --------------- Local functions --------------- */
static tvar_list_ptr	get_tvar_list_rec();
static tvar_list_ptr	cons_name(string name, tvar_list_ptr l);
static int		yyerror(const char *msg);
static void		print_fixity(pointer key, pointer data);
static typeExp_ptr	find_typevar(string name, bool insert);
static g_ptr		make_project_fun(string name, g_ptr pexpr);
static string		get_base_name(string name);

/* --------------- Local macros --------------- */

extern char dbg_tst_buf[101];
extern char dbg_rd_buf[101];
extern int dbg_tst_line_cnt;


%}

%pure-parser

%define parse.lac full
%define parse.error verbose

%start pgm

%union  {
	    struct {
		string	file;
		int	line;
	    }			    loc_t;
	    string		    str_t;
	    int			    int_t;
	    arbi_T		    arbi_t;
	    struct {
		bool		ok;
		string		file;
		int		line;
		string		var;
		g_ptr		expr;
		typeExp_ptr	type;
		int		cnt;
	    }			    decl_t;
	    struct {
		bool		ok;
		symbol_tbl_ptr	fns;
	    }			    decl_list;
	    struct {
		bool		ok;
		oll_ptr		fns;
	    }			    overloads_t;
	    g_ptr		    expr_t;
	    struct {
	        string	file;
	        int	line;
	    	bool strict;
	    	bool cached;
	    	bool recursive;
	    } let_t;
	    struct {
	    	g_ptr expr;
	    	bool     ok;
	    } fn_arg_expr_t;
	    cl_ptr		    cl_t;
	    struct {
		bool	    ok;
		typeExp_ptr type;
	    }			   type_t;
	    struct {
		bool		ok;
		typeList_ptr	tl;
	    }			    tpLptr_t;
	    struct {
		string		name;
		typeExp_ptr	type;
	    }			    varl_itemp_t;
	    var_list_ptr	    varlp_t;
	    struct {
		bool		ok;
		tvar_list_ptr	name_list;
	    }			    tvar_list_t;
	    struct {
		bool		ok;
		string		name;
	    }			    name_t;
	    struct {
		bool		ok;
		string		name;
		typeExp_ptr	type;
	    }			    type_name_t;
	    struct {
		bool		ok;
		string		name;
		typeExp_ptr	type;
	    }			    opt_typed_var_t;
        }

%token <loc_t>	QUALIFIED_BY
%token <loc_t>	BEGIN_ADT
%token <loc_t>	BINDER
%token <loc_t>	NAMED_BINDER
%token <loc_t>	FREE_BINDER
%token <loc_t>	BINDER_WITH_ACC
%token <loc_t>	SZ_BINDER_WITH_ACC
%token <loc_t>	CLEAR_FIXITIES
%token <loc_t>	S_LET
%token <loc_t>	S_LETREC
%token <loc_t>	C_LET
%token <loc_t>	C_LETREC
%token <loc_t>	END_ADT
%token <loc_t>	EXPORT
%token <loc_t>	EXPORT_TO_TCL
%token <loc_t>	IMPORT_FROM_TCL
%token <loc_t>	FORWARD_DECLARE
%token <loc_t>	INFIX
%token <loc_t>	INFIX_UNARY
%token <loc_t>	INFIXR
%token <loc_t>	THEN
%token <loc_t>	IN
%token <loc_t>	INSTALL_PFN
%token <loc_t>	LET
%token <loc_t>	LETREC
%token <loc_t>	LETTYPE
%token <loc_t>	AND_LETTYPE
%token <loc_t>	LIST
%token <loc_t>	TYPE_ABREV
%token <loc_t>	NON_LAZY
%token <loc_t>	NONFIX
%token <loc_t>	ADD_OPEN_OVERLOAD
%token <loc_t>	OVERLOAD
%token <loc_t>	OPEN_OVERLOAD
%token <loc_t>	POSTFIX
%token <loc_t>	PREFIX
%token <loc_t>	PRINT_FIXITIES
%token <loc_t>	REF
%token <loc_t>	VAL


%token		LAMBDA OR IMPLIES
%token		TRUET FALSET NILT STRINGCMD CONJ STARS 
%token		LPAR RPAR DOT SEMICOLON COMMA
%token		LBRACK RBRACK THEN_COND ELSE_COND LOAD
%token 		CATENATE CONS BVARIABLE
%token 		EQUAL
%token 		SEQ FORALL THEREIS PRINT DEPENDS
%token 		QUANT_FORALL QUANT_THEREIS FAIL CATCH
%token 		CROSSPROD STE
%token 		EXPLODE IMPLODE FN_TYPE
%token 		BOOL2STR INT2STR TIMET
%token 		LOAD_EXE MAKE_EXE PRINT_STE LCURL RCURL TYPE_SEP
%token 		CAST_LCURL
%token 		PRINTFT FPRINTFT SPRINTFT EPRINTFT SSCANFT
%token 		IF_THEN_ELSE_BINDER THEN_BINDER ELSE_BINDER UNARY_MINUS

%token		BEGIN_QUOTE END_QUOTE UNQUOTE

%token <str_t>	DEC_NUMBER BIN_NUMBER HEX_NUMBER FLOAT_NUMBER
%token <str_t>	VART STRINGT
%token <str_t>	POSTFIX_VAR BINDER_VAR BINDER_WITH_ACC_VAR
%token <str_t>	SZ_BINDER_WITH_ACC_VAR
%token <str_t>	FREE_BINDER_VAR
%token <str_t>	INFIX_VAR_0 INFIX_VAR_1 INFIX_VAR_2 INFIX_VAR_3 INFIX_VAR_4 
%token <str_t>	INFIX_VAR_5 INFIX_VAR_6 INFIX_VAR_7 INFIX_VAR_8 INFIX_VAR_9 
%token <str_t>	INFIXR_VAR_0 INFIXR_VAR_1 INFIXR_VAR_2 INFIXR_VAR_3
%token <str_t>	INFIXR_VAR_4 INFIXR_VAR_5 INFIXR_VAR_6 INFIXR_VAR_7
%token <str_t>	INFIXR_VAR_8 INFIXR_VAR_9 
%token <str_t>	PREFIX_VAR_0 PREFIX_VAR_1
%token <str_t>	INFIX_VAR_UNARY_0
%token <str_t>	INFIX_VAR_UNARY_1
%token <str_t>	INFIX_VAR_UNARY_2
%token <str_t>	INFIX_VAR_UNARY_3
%token <str_t>	INFIX_VAR_UNARY_4
%token <str_t>	INFIX_VAR_UNARY_5
%token <str_t>	INFIX_VAR_UNARY_6
%token <str_t>	INFIX_VAR_UNARY_7
%token <str_t>	INFIX_VAR_UNARY_8
%token <str_t>	INFIX_VAR_UNARY_9
%token <str_t>	THEN_COND_VAR ELSE_COND_VAR

%type  <decl_list>	    type_decl new_type_decl type_expr type
%type  <overloads_t>	    overload_list
%type  <expr_t>		    top_expr expr expr05 expr1 expr2 expr_list arg_expr
%type  <cl_t>		    rev_expr_list
%type  <decl_t>		    decl fn_defs fn_def lhs_expr_list
%type  <str_t>		    var_or_infix
%type  <type_name_t>	    type_name
%type  <tpLptr_t>	    type_list
%type  <tpLptr_t>	    targ_list
%type  <type_t>		    simple_type type_literal
%type  <varlp_t>	    var_list
%type  <varl_itemp_t>	    var_list_item
%type  <opt_typed_var_t>    opt_typed_var;
%type  <tvar_list_t>	    tvarlist;
%type  <let_t>		    let_top;

%right	CROSSPROD
%right	FN_TYPE
%left	REF
%left	LIST
%left 	IN
%left 	THEN
%left	DOT
%right	THEN_COND ELSE_COND
%right	THEN_COND_VAR ELSE_COND_VAR
%left	CATCH
%left	SEQ
%left	FSEQ
%right	CONS
%right	COMMA
%right	DUMMY_CONS
%left	INFIX_VAR_0
%left	INFIX_VAR_UNARY_0
%right	INFIXR_VAR_0
%left	INFIX_VAR_1 
%left	INFIX_VAR_UNARY_1
%right	INFIXR_VAR_1
%left	INFIX_VAR_2 EQUAL
%left	INFIX_VAR_UNARY_2
%right	INFIXR_VAR_2
%left	INFIX_VAR_3 OR CATENATE
%left	INFIX_VAR_UNARY_3
%right	INFIXR_VAR_3
%left	INFIX_VAR_4
%left	INFIX_VAR_UNARY_4
%right	INFIXR_VAR_4
%left	INFIX_VAR_5 
%left	INFIX_VAR_UNARY_5
%right	INFIXR_VAR_5
%left	INFIX_VAR_6 
%left	INFIX_VAR_UNARY_6
%right	INFIXR_VAR_6
%left	INFIX_VAR_7
%left	INFIX_VAR_UNARY_7
%right	INFIXR_VAR_7
%left	INFIX_VAR_8
%left	INFIX_VAR_UNARY_8
%right	INFIXR_VAR_8
%left	INFIX_VAR_9
%left	INFIX_VAR_UNARY_9
%right	INFIXR_VAR_9
%left 	PRINT EXPLODE IMPLODE LOAD BVARIABLE
	STE FAIL
	QUANT_FORALL QUANT_THEREIS BOOL2STR INT2STR TIMET
	LOAD_EXE MAKE_EXE PRINT_STE
%left	POSTFIX_VAR
%left	DUMMY_LAST
%left	PREFIX_VAR_0
%left	PREFIX_VAR_1
%left	UNARY_MINUS
%left	FREE_BINDER_VAR

%%
pgm		: pgm SEMICOLON stmt0
                {
		    dispose_hash(&type_var_tbl, NULLFCN);
		    create_hash(&type_var_tbl, 100, str_hash, str_equ);
                }
                | stmt0
                {
                }
;

stmt0		: stmt
		{ Emit_prompt(""); }
		| /* Empty */
		{ }
;

stmt		: expr
		{
                    result_ptr res;
                    res = Compile(symb_tbl, $1, NULL, FALSE, FALSE);
                    if( res != NULL ) {
                        if( Is_Void(res->type) ) {
                            Print_Result(res, stdout_fp, FALSE);
                        } else {
                            Print_Result(res, stdout_fp, TRUE);
                            FP(stdout_fp, "\n");
                            symb_tbl = New_fn_def(it, res, symb_tbl, !file_load,
						  cur_file_name, line_nbr);
                        }
			Free_result_ptr(res);
                    }
		}
		| INSTALL_PFN expr
		{
		    result_ptr res = Compile(symb_tbl, $2, NULL, TRUE, FALSE);
		    if( res != NULL ) {
			Install_PrinterFn(res);
			Free_result_ptr(res);
		    }
		    if( !file_load ) FP(stdout_fp, "\n");
		}
		| decl
		{
		    if( $1.ok ) {
			result_ptr res = Compile(symb_tbl, $1.expr,
						 NULL, TRUE, TRUE);
			if( res != NULL ) {
			    symb_tbl = New_fn_def($1.var, res, symb_tbl,
						 !file_load,$1.file,$1.line);
			    Free_result_ptr(res);
			}
		    }
		}
                | VAL arg_expr EQUAL expr
                {
                    result_ptr res;
                    res = Compile(symb_tbl, $4, NULL, TRUE, FALSE);
		    if( res != NULL ) {
			symb_tbl = New_fn_def(it, res, symb_tbl, FALSE,
					      cur_file_name, line_nbr);
			buffer var_buf;
			new_buf(&var_buf, 100, sizeof(string));
			Get_Vars(&var_buf, $2);
			string *sp;
			FOR_BUF(&var_buf, string, sp) {
			    g_ptr e = make_project_fun(*sp, $2);
			    if( e == NULL ) break;
			    res = Compile(symb_tbl, e, NULL, TRUE, FALSE);
			    if( res == NULL ) break;
			    symb_tbl = New_fn_def(*sp, res, symb_tbl,
						  !file_load,
						  cur_file_name, line_nbr);
			}
			free_buf(&var_buf);
		    }
                }
		| ADD_OPEN_OVERLOAD var_or_infix overload_list
		{
		    if( $3.ok ) {
			symb_tbl = AddToOpenOverloadDef($2, $3.fns, symb_tbl,
						    cur_file_name, line_nbr);
		    }
		}
		| OPEN_OVERLOAD opt_typed_var overload_list
		{
		    if( $2.ok && $3.ok ) {
			symb_tbl = InsertOverloadDef($2.name, TRUE,
						     $3.fns, $2.type,
						    symb_tbl,
						    cur_file_name, line_nbr);
		    }
		}
		| OVERLOAD opt_typed_var overload_list
		{
		    if( $2.ok && $3.ok ) {
			symb_tbl = InsertOverloadDef($2.name,FALSE,
						     $3.fns, $2.type,
						     symb_tbl,
						     cur_file_name, line_nbr);
		    }
		}
		| CAST_LCURL simple_type RCURL decl
		{
		    if( $2.ok && $4.ok ) {
			result_ptr res;
			res = Compile(symb_tbl, $4.expr, $2.type, TRUE, TRUE);
			if( res != NULL ) {
			    symb_tbl = New_fn_def($4.var, res, symb_tbl,
						  !file_load, $4.file, $4.line);
			    Free_result_ptr(res);
			}
		    }
		}
		| type_decl
		{
		    if( $1.ok ) {
			symb_tbl = Merge_fn_defs(symb_tbl, $1.fns, TRUE);
		    }
		}
		| IF_THEN_ELSE_BINDER var_or_infix var_or_infix
		{
		    Insert_if_then_else($2, $3);
		}
		| THEN_BINDER var_or_infix
		{
		    Insert_then_binder($2);
		}
		| ELSE_BINDER var_or_infix
		{
		    Insert_else_binder($2);
		}
		| POSTFIX var_or_infix
		{
		    Insert_postfix($2);
		}
		| NONFIX var_or_infix
		{
		    Remove_Infix($2);
		}
                | EXPORT var_or_infix
		{
		    if( !Export_Fun($2, symb_tbl) ) {
                        FP(err_fp, "-E-: Cannot export symbol %s\n", $2);
		    }
		}
                | EXPORT_TO_TCL var_or_infix
                {
                    if( !Register_tcl_callback($2, symb_tbl) ) {
                        FP(err_fp, "-E-: Cannot find symbol %s\n", $2);
                    }
                }
		| IMPORT_FROM_TCL LCURL var_or_infix TYPE_SEP simple_type RCURL
		{
		    if( $5.ok ) {
			if( !Import_tcl_function($3, $5.type) ) {
			    FP(err_fp, "-E-: Cannot import function %s\n", $3);
			}
		    }
		}
		| FORWARD_DECLARE LCURL var_or_infix TYPE_SEP simple_type RCURL
		{
		    if( $5.ok ) {
			symb_tbl = Make_forward_declare($3,$5.type,symb_tbl,
					cur_file_name, line_nbr);
		    }
		}
                | NON_LAZY var_or_infix
                {
                    if( !Make_non_lazy($2, symb_tbl) ) {
                        FP(err_fp, "-E-: Cannot find symbol %s\n", $2);
                    }
                }
		| CLEAR_FIXITIES
		{
		    dispose_hash(&Infix_Table, NULLFCN);
		    dispose_hash(&Infix_Unary_Table, NULLFCN);
		    dispose_hash(&Binder_Acc_Table, NULLFCN);
		    create_hash(&Infix_Table, 100, str_hash, str_equ);
		    create_hash(&Infix_Unary_Table, 100, str_hash, str_equ);
		    create_hash(&Binder_Acc_Table, 100, str_hash, str_equ);
		}
		| PRINT_FIXITIES
		{
                    Sprintf(tmp_buf, "%s/FL_PR_FIXITIES_XXXXXX",
                            RC_TMP_FILE_DIR);
                    int fd = mkstemp(tmp_buf);
                    if( fd < 0 ) {
			Wprintf("WARNING: Cannot create tmp file %s\n",
                                tmp_buf);
                    } else {
                        tmp_fp = fdopen(fd, "w");
			scan_hash(&Infix_Table, print_fixity);
			fclose(tmp_fp);
			string  cmd = strtemp("sort -o ");
			cmd = strappend(tmp_buf);
			cmd = strappend(" ");
			cmd = strappend(tmp_buf);
                        if( system(cmd) != 0 ) {
                            FP(err_fp, "-W- Sort of fixities failed\n");
                        } 
                        FP(stdout_fp,
                          "\nBinders, infix, prefix, and postfix functions:\n");
                        tmp_fp = fopen(tmp_buf, "r");
                        while( fgets(tmp_buf, 1000, tmp_fp) != NULL ) {
                            FP(stdout_fp, "%s", tmp_buf);
                        }
                        fclose(tmp_fp);
			unlink(tmp_buf);
		    }
		}
		| PREFIX DEC_NUMBER var_or_infix
		{
		    int i = atoi($2);
		    if( i < 0 || i > 1 ) {
			FP(err_fp, "Too large/small precedence\n");
                    } else
			Insert_prefix($3, i);
		}
		| BINDER var_or_infix
		{
		    Insert_binder($2);
		}
		| FREE_BINDER var_or_infix
		{
		    Insert_free_binder($2);
		}
		| BINDER_WITH_ACC var_or_infix var_or_infix
		{
		    Insert_binder_with_acc($2,$3);
		}
		| SZ_BINDER_WITH_ACC var_or_infix var_or_infix
		{
		    Insert_size_binder_with_acc($2,$3);
		}
		| INFIX DEC_NUMBER var_or_infix
		{
		    int i = atoi($2);
		    if( i < 0 || i > 9 ) {
			FP(err_fp, "Too large/small precedence\n");
		    } else
			Insert_infix($3, i);
		}
		| INFIX_UNARY DEC_NUMBER var_or_infix var_or_infix
		{
		    int i = atoi($2);
		    if( i < 0 || i > 9 ) {
			FP(err_fp, "Too large/small precedence\n");
		    } else
			Insert_infix_unary($3, i, $4);
		}
		| INFIXR DEC_NUMBER var_or_infix
		{
		    int i = atoi($2);
		    if( i < 0 || i > 9 ) {
			FP(err_fp, "Too large/small precedence\n");
		    } else
			Insert_infixr($3, i);
		}
		| BEGIN_ADT
		{
		    Begin_ADT(symb_tbl);
		}
		| END_ADT var_list
		{
		    symb_tbl = End_ADT(symb_tbl, $2);
		}
		| END_ADT
		{
		    symb_tbl = End_ADT(symb_tbl, NULL);
		}
		;

opt_typed_var	: var_or_infix
		{
		    $$.ok = TRUE;
		    $$.name = $1;
		    $$.type = NULL;
		}
		| LCURL var_or_infix TYPE_SEP simple_type RCURL
		{
		    if( $4.ok ) {
			$$.ok = TRUE;
			$$.name = $2;
			$$.type = $4.type;
		    } else {
			$$.ok = FALSE;
		    }
		}
		;

var_or_infix	: VART { $$ = $1; }
		| CROSSPROD { $$ = s_crossprod; }
		| INFIX_VAR_0 { $$ = $1; }
		| INFIX_VAR_1 { $$ = $1; }
		| INFIX_VAR_2 { $$ = $1; }
		| INFIX_VAR_3 { $$ = $1; }
		| INFIX_VAR_4 { $$ = $1; }
		| INFIX_VAR_5 { $$ = $1; }
		| INFIX_VAR_6 { $$ = $1; }
		| INFIX_VAR_7 { $$ = $1; }
		| INFIX_VAR_UNARY_0 { $$ = $1; }
		| INFIX_VAR_UNARY_1 { $$ = $1; }
		| INFIX_VAR_UNARY_2 { $$ = $1; }
		| INFIX_VAR_UNARY_3 { $$ = $1; }
		| INFIX_VAR_UNARY_4 { $$ = $1; }
		| INFIX_VAR_UNARY_5 { $$ = $1; }
		| INFIX_VAR_UNARY_6 { $$ = $1; }
		| INFIX_VAR_UNARY_7 { $$ = $1; }
		| INFIX_VAR_UNARY_8 { $$ = $1; }
		| INFIX_VAR_UNARY_9 { $$ = $1; }
		| INFIX_VAR_8 { $$ = $1; }
		| INFIX_VAR_9 { $$ = $1; }
		| INFIXR_VAR_0 { $$ = $1; }
		| INFIXR_VAR_1 { $$ = $1; }
		| INFIXR_VAR_2 { $$ = $1; }
		| INFIXR_VAR_3 { $$ = $1; }
		| INFIXR_VAR_4 { $$ = $1; }
		| INFIXR_VAR_5 { $$ = $1; }
		| INFIXR_VAR_6 { $$ = $1; }
		| INFIXR_VAR_7 { $$ = $1; }
		| INFIXR_VAR_8 { $$ = $1; }
		| INFIXR_VAR_9 { $$ = $1; }
		| POSTFIX_VAR { $$ = $1; }
		| BINDER_VAR { $$ = $1; }
		| FREE_BINDER_VAR { $$ = $1; }
		| BINDER_WITH_ACC_VAR { $$ = $1; }
		| SZ_BINDER_WITH_ACC_VAR { $$ = $1; }
		| EQUAL { $$ = eq_string; }
		| PREFIX_VAR_0 { $$ = $1; }
		| PREFIX_VAR_1 { $$ = $1; }
		| THEN_COND_VAR { $$ = $1; }
		| ELSE_COND_VAR { $$ = $1; }
		;

overload_list	: overload_list opt_typed_var
		{
		    if( $1.ok && $2.ok ) {
			$$.fns = Add_To_OverloadList($2.name,
						     $2.type,
						     $1.fns, 
						     cur_file_name,
						     line_nbr);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| /* Empty */
		{
		    $$.fns = NULL;
		    $$.ok = TRUE;
		}
		;

var_list	: var_list var_list_item
		{
		    $$ = Add_Var_to_Var_List($2.name, $2.type, $1);
		}
		| var_list_item
		{
		    $$ = Add_Var_to_Var_List($1.name, $1.type, NULL);
		}
		;

var_list_item	: var_or_infix
		{
		    $$.name = $1;
		    $$.type = NULL;
		}
		| STRINGT
		{
		    $$.name = $1;
		    $$.type = NULL;
		}
		| LCURL var_or_infix TYPE_SEP simple_type RCURL
		{
		    $$.name = $2;
		    if( $4.ok ) {
			$$.type = $4.type;
		    } else {
			$$.type = NULL;
		    }
		}
		| LCURL STRINGT TYPE_SEP simple_type RCURL
		{
		    $$.name = $2;
		    if( $4.ok ) {
			$$.type = $4.type;
		    } else {
			$$.type = NULL;
		    }
		}
		;

decl		: let_top fn_defs
		{
		    g_ptr res;
		    if( $2.ok ) {
			$$.file = $1.file;
    			$$.line = $1.line;
    			$$.var  = $2.var;
			res = $2.expr;
    			if( RCadd_debug_info ) {
    			    g_ptr dbg = Make_Debug_Primitive($2.var,
					$1.file, $1.line);
			    res = Make_APPL_ND(dbg, res);
    			}
			if ($1.cached && $2.cnt > 0) {
				int	cache_nbr;
			        g_ptr tmp;
			        cache_nbr = Make_g_cache();
			        tmp = Make_0inp_Primitive(P_CACHE);
			        SET_CACHE_TBL(tmp, cache_nbr);
			        res = Make_APPL_ND(
				        Make_APPL_ND(tmp, Make_arglist($2.cnt)),
				        res);
		        }
			if($1.strict && $2.cnt > 0 ) {
			   g_ptr tmp = Make_0inp_Primitive(P_STRICT_ARGS);
			   res = Make_APPL_ND(
			   	    Make_APPL_ND(tmp, Make_arglist($2.cnt)),
			   	    res);
			}
			res = Add_args($2.cnt, res);
			if ($1.recursive) {
			    res = Make_1inp_Primitive(P_Y,
                               Make_Lambda($2.var, res));
			}
			$$.expr = res;
    			if( $2.type != NULL ) { TypeHint($$.expr,$2.type); }
    			$$.ok = TRUE;
		    } else {
			    $$.ok = FALSE;
		    }
		}
		;

fn_defs		: fn_def CONJ fn_defs
		{
		    if( $1.ok && $3.ok ) {
			if( $1.var != $3.var ) {
			    FP(err_fp, "-E-: Inconsistent function names in "
				       "pattern matching (%s != %s) in "
				       "file %s close to line %d\n",
					$1.var, $3.var,cur_file_name,line_nbr);
			    $$.ok = FALSE;
			} else if( $1.cnt != $3.cnt ) {
			    FP(err_fp, "-E-: Inconsistent number of arguments "
				       "to function `%s' in file "
				       "%s close to line %d\n",
					$1.var, cur_file_name, line_nbr);
			    $$.ok = FALSE;
			} else {
			    $$.var  = $1.var;
			    $$.expr = Make_2inp_Primitive(
					P_PCATCH, $1.expr, $3.expr);
			    $$.cnt  = $1.cnt;
			    $$.type = $1.type;
			    $$.ok = TRUE;
			}
		    } else {
			$$.ok = FALSE;
		    }
		}
		| fn_def
		{
		    if( $1.ok ) {
			$$.var  = $1.var;
			Sprintf(buf, "No pattern matched for %s\n", $1.var);
			if( Can_Fail_Pat_Match($1.expr) ) {
			    $$.expr = Make_2inp_Primitive(P_PCATCH, $1.expr,
					  Make_1inp_Primitive(P_ERROR, 
						Make_STRING_leaf(
						     wastrsave(&strings,buf))));
			} else {
			    $$.expr = $1.expr;
			}
			$$.cnt  = $1.cnt;
			$$.type = $1.type;
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		;

fn_def		: var_or_infix lhs_expr_list
		{
		    if( $2.ok ) {
			$$.var  = $1;
			$$.expr = $2.expr;
			$$.cnt  = $2.cnt;
			$$.type = NULL;
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| LCURL var_or_infix TYPE_SEP simple_type RCURL lhs_expr_list
		{
		    if( $4.ok && $6.ok ) {
			$$.ok = TRUE;
			$$.var  = $2;
			$$.expr = $6.expr;
			$$.type = $4.type;
			$$.cnt  = $6.cnt;
		    } else {
			$$.ok = FALSE;
		    }
		}
		;
lhs_expr_list	: arg_expr lhs_expr_list
		{
		    if( $1 != NULL && $2.ok ) {
			g_ptr res;
			res = TrArg($1, $2.expr, FALSE);
			if( res != NULL ) {
			    Sprintf(buf, "_Q_%d", $2.cnt+1);
			    $$.expr = Make_APPL_ND(res,
					   Make_VAR_leaf(
						wastrsave(&strings, buf)));
			    $$.cnt = $2.cnt+1;
			    $$.ok = TRUE;
			} else {
			    $$.expr = NULL;
			    $$.ok = FALSE;
			}
		    } else {
			$$.ok = FALSE;
		    }
		}
		| EQUAL top_expr
		{
		    if( $2 != NULL ) {
			$$.expr = $2;
			$$.cnt = 0;
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| QUALIFIED_BY LPAR top_expr RPAR EQUAL top_expr
		{
		    if( $3 != NULL && $6 != NULL ) {
			$$.expr = Make_3inp_Primitive(
				     P_COND,$3,$6,Make_0inp_Primitive(P_PFAIL));
			$$.cnt = 0;
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		;


type_decl	: new_type_decl
		{
		    if( $1.ok && Check_No_Undefined_Types($1.fns) ) {
			$$.ok = TRUE;
			$$.fns = $1.fns;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| TYPE_ABREV var_or_infix EQUAL simple_type
		{
		    if( $4.ok ) {
			New_Type_Abbrev($2, $4.type);
			$$.ok = FALSE;  // Not an error, but nothing to do....
		    } else {
			$$.ok = FALSE;
		    }
		}
		;

new_type_decl	: new_type_decl AND_LETTYPE type_name EQUAL type_expr 
		{
		    if( $1.ok && $3.ok && $5.ok ) {
			$$.ok = TRUE;
			$$.fns = Merge_fn_defs(
				    Add_Destructors($3.name, $3.type, $5.fns,
						    $2.file, $2.line),
				    $1.fns, TRUE);
		    } else {
			$$.ok = FALSE;
		    }
			
		}
		| LETTYPE type_name EQUAL type_expr 
		{
		    if( $4.ok && $2.ok ) {
			$$.ok = TRUE;
			$$.fns = Merge_fn_defs(
				    Add_Destructors($2.name, $2.type, $4.fns,
						    $1.file, $1.line),
				    NULL, TRUE);
		    } else {
			$$.ok = FALSE;
		    }
		}
		;


type_name	: var_or_infix 
		{
		    $$.name = $1;
		    $$.type = Get_Type($1, NULL, TP_INSERT_FULL_TYPE);
		    if( $$.type != NULL ) $$.ok = TRUE; else $$.ok = FALSE;
		}
		| INFIX_VAR_8 VART var_or_infix
		{
		    if( strcmp($1, "*") != 0 ) {
			FP(err_fp, "-E-: Unknown type name %s%s in "
				   "file %s close to line %d\n",
				   $1, $2, cur_file_name, line_nbr);
			$$.ok = FALSE;
		    } else {
			typeList_ptr targs = NULL;
			typeExp_ptr type_var = find_typevar($2, TRUE);
			if( type_var == NULL ) {
			    $$.ok = FALSE;
			} else {
			    targs = Append_Type_List(targs, type_var);

			}
			string type_name = $3;
			$$.name = type_name;
			$$.type = Get_Type(type_name,targs,TP_INSERT_FULL_TYPE);
			if( $$.type != NULL ) $$.ok = TRUE; else $$.ok = FALSE;
		    }
		}
		| LCURL tvarlist RCURL var_or_infix
		{
		    if( !$2.ok ) {
			$$.ok = FALSE;
		    } else {
			typeList_ptr targs = NULL;
			tvar_list_ptr tlp = $2.name_list;
			bool ok = TRUE;
			while( ok && tlp != NULL ) {
			    typeExp_ptr type_var = find_typevar(tlp->var, TRUE);
			    if( type_var == NULL ) {
				ok = FALSE;
			    } else {
				targs = Append_Type_List(targs, type_var);
				tlp = tlp->next;
			    }
			}
			if( !ok ) {
			    $$.ok = FALSE;
			} else {
			    string type_name = $4;
			    $$.name = type_name;
			    $$.type =
				Get_Type(type_name, targs, TP_INSERT_FULL_TYPE);
			    if( $$.type != NULL ) $$.ok=TRUE; else $$.ok=FALSE;
			}
		    }
		}
		;

tvarlist	: INFIX_VAR_8 VART COMMA tvarlist
		{
		    if( !$4.ok ) {
			$$.ok = FALSE;
		    } else {
			if( strcmp($1, "*") != 0 ) {
			    FP(err_fp, "-E-: Unknown type name %s%s in "
				       "file %s close to line %d\n",
				       $1, $2, cur_file_name, line_nbr);
			    $$.ok = FALSE;
			} else {
			    $$.name_list = cons_name($2, $4.name_list);
			    $$.ok = TRUE;
			}
		    }
		}
		| INFIX_VAR_8 VART
		{
		    if( strcmp($1, "*") != 0 ) {
			FP(err_fp, "-E-: Unknown type name %s%s in "
				   "file %s close to line %d\n",
				   $1, $2, cur_file_name, line_nbr);
			$$.ok = FALSE;
		    } else {
			$$.name_list = cons_name($2,NULL);
			$$.ok = TRUE;
		    }
		}
		;

type_expr	: type_expr ELSE_COND type
		{
		    if( $1.ok && $3.ok ) {
			$$.fns = Merge_fn_defs($1.fns, $3.fns, FALSE);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| type
		{
		    $$ = $1;
		}
		;
type		: var_or_infix type_list
		{
		    if( $2.ok ) {
			g_ptr e = Make_TYPE($1, Length_TypeList($2.tl));
			result_ptr tmp_res = Compile(NULL, e, NULL,FALSE,FALSE);
			if( tmp_res != NULL ) {
			    Unify_Constr(tmp_res->type, $2.tl);
			    // Mk: let VAR a1 a2 . . . a$1 =
			    //      "VAR",a1,a2,. . .,a$2;
			    $$.fns = New_fn_def($1, tmp_res, NULL, FALSE,
						cur_file_name, line_nbr);
			    Free_result_ptr(tmp_res);
			    $$.ok = TRUE;
			} else {
			    $$.ok = FALSE;
			}
		    } else {
			$$.ok = FALSE;
		    }
		}
		;

type_list	: type_list type_literal
		{
		    if( $1.ok && $2.ok ) {
			$$.tl = Append_Type_List($1.tl, $2.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| type_list LPAR simple_type RPAR
		{
		    if( $1.ok && $3.ok ) {
			$$.tl = Append_Type_List($1.tl, $3.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| type_list LCURL var_or_infix TYPE_SEP simple_type RCURL
		{
		    if( $1.ok && $5.ok ) {
			$$.tl = Append_Type_List($1.tl, $5.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| /* Empty */
		{
		    $$.tl= NULL;
		    $$.ok = TRUE;
		}
		;


targ_list	: targ_list COMMA simple_type
		{
		    if( !$1.ok || !$3.ok ) {
			$$.ok = FALSE;
		    } else {
			$$.tl = Append_Type_List($1.tl, $3.type);
			$$.ok = TRUE;
		    }
		}
		|   simple_type
		{
		    if( !$1.ok ) {
			$$.ok = FALSE;
		    } else {
			$$.tl = Append_Type_List(NULL, $1.type);
			$$.ok = TRUE;
		    }
		}
		;

simple_type	: LCURL targ_list RCURL var_or_infix
		{
		    if( !$2.ok ) {
			$$.ok = FALSE;
		    } else {
			$$.type = Get_Type($4, $2.tl, TP_INSERT_PLACE_HOLDER);
			if( $$.type != NULL ) $$.ok=TRUE; else $$.ok=FALSE;
		    }
		}
		| simple_type FN_TYPE simple_type
		{
		    if( $1.ok && $3.ok ) {
			$$.type = GLmake_arrow($1.type, $3.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| simple_type CROSSPROD simple_type
		{
		    if( $1.ok && $3.ok ) {
			$$.type = GLmake_tuple($1.type, $3.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| simple_type REF
		{
		    if( $1.ok ) {
			$$.type = GLmake_ref($1.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| simple_type LIST
		{
		    if( $1.ok ) {
			$$.type = GLmake_list($1.type);
			$$.ok = TRUE;
		    } else {
			$$.ok = FALSE;
		    }
		}
		| LPAR simple_type RPAR
		{
		    $$ = $2;
		}
		| type_literal
		{
		    $$ = $1;
		}
		;

type_literal	: VART
		{
		    $$.type = Get_Type($1, NULL, TP_INSERT_PLACE_HOLDER);
		    if( $$.type != NULL ) $$.ok=TRUE; else $$.ok=FALSE;
		}
		| INFIX_VAR_8 VART
		{
		    if( strcmp($1, "*") != 0 ) {
			FP(err_fp, "-E-: Unknown type name %s%s in "
				   "file %s close to line %d\n",
				   $1, $2, cur_file_name, line_nbr);
			$$.ok = FALSE;
		    } else {
			$$.type = find_typevar($2, TRUE);
			// SHOULD THIS BE FALSE???? ^^^^^
			if( $$.type != NULL ) {
			    $$.ok = TRUE;
			} else {
			    $$.ok = FALSE;
			}
		    }
		}
		;

top_expr	: expr
		{
		    $$ = $1;
		}
		;

expr		: LCURL expr TYPE_SEP simple_type RCURL
		{
		    if( $4.ok ) {
			$$ = $2;
			TypeHint($2,$4.type);
		    } else {
			$$ = $2;
		    }
		}

		| decl IN expr
		{
		    /* return: (\($1->lhs).$3)($1->rhs) */
		    if( $1.ok )
			$$ = Make_APPL_ND(Make_Lambda($1.var, $3), $1.expr);
		    else
			$$ = NULL;
		}
                | VAL arg_expr EQUAL expr IN expr
                {
                    g_ptr res;
                    res = TrArg($2, $6, FALSE);
                    if( res != NULL ) {
                        Sprintf(buf, "_Q_1");
                        res = Make_APPL_ND(res,
                                       Make_VAR_leaf(wastrsave(&strings, buf)));
                        Sprintf(buf, "No pattern matched for val expression\n");
			if( Can_Fail_Pat_Match(res) ) {
			    res = Make_2inp_Primitive(P_PCATCH, res,
                                      Make_1inp_Primitive(P_ERROR, 
                                            Make_STRING_leaf(
                                                     wastrsave(&strings,buf))));
			}
                        res = Add_args(1, res);
                        $$ = Make_APPL_ND(res, $4);
                    } else {
                        $$ = NULL;
                    }
                }
		| decl THEN expr
		{
		    /* return: (\($1->lhs).$3)($1->rhs) */
		    if( $1.ok ) {
			$$ = Make_2inp_Primitive(P_THEN, $1.expr, 
						 Make_Lambda($1.var, $3));
                    } else
			$$ = NULL;
		}
                | VAL arg_expr EQUAL expr THEN expr
                {
                    g_ptr res;
                    res = TrArg($2, $6, FALSE);
                    if( res != NULL ) {
                        Sprintf(buf, "_Q_1");
                        res = Make_APPL_ND(res,
                                       Make_VAR_leaf(wastrsave(&strings, buf)));
                        Sprintf(buf, "No pattern matched for val expression\n");
			if( Can_Fail_Pat_Match(res) ) {
			    res = Make_2inp_Primitive(P_PCATCH, res,
                                      Make_1inp_Primitive(P_ERROR, 
                                            Make_STRING_leaf(
                                                     wastrsave(&strings,buf))));
			}
                        res = Add_args(1, res);
			$$ = Make_2inp_Primitive(P_THEN, $4, res);
                    } else {
                        $$ = NULL;
                    }
                }
		| LAMBDA arg_expr DOT expr
		{
                    g_ptr res;
                    res = TrArg($2, $4, FALSE);
                    if( res != NULL ) {
                        Sprintf(buf, "_Q_1");
                        res = Make_APPL_ND(res,
                                       Make_VAR_leaf(wastrsave(&strings, buf)));
                        Sprintf(buf,
				"No pattern matched for lambda expression\n");
			if( Can_Fail_Pat_Match(res) ) {
			    res = Make_2inp_Primitive(P_PCATCH, res,
                                      Make_1inp_Primitive(P_ERROR, 
                                            Make_STRING_leaf(
                                                     wastrsave(&strings,buf))));
			}
                        $$ = Add_args(1, res);
                    } else {
                        $$ = NULL;
                    }
		}
		| SZ_BINDER_WITH_ACC_VAR arg_expr var_list DOT expr
		{
		    var_list_ptr vp;
		    Sprintf(buf, "_sz_bind_cnt_");
		    string scnt = wastrsave(&strings, buf);
		    g_ptr e = $5;
		    string acc = find_hash(&Binder_Acc_Table, (pointer) $1);
		    for(vp = $3; vp != NULL; vp = vp->next) {
			g_ptr fn = Make_Lambda(get_base_name(vp->name),
					       Make_Lambda(acc, e)); 
			if( vp->type_hint != NULL ) {
			    TypeHint(fn, GLmake_arrow(vp->type_hint,
						      GLnew_tVar()));
			}
			e = Make_APPL_ND(
				Make_APPL_ND(
				    Make_APPL_ND(
					Make_APPL_ND(Make_VAR_leaf($1),
						     Make_VAR_leaf(scnt)),
					fn),
				    Make_STRING_leaf(vp->name)),
				Make_VAR_leaf(acc));
		    }
		    $$ = Make_APPL_ND(Make_Lambda(scnt, e), $2);
		}
		| BINDER_WITH_ACC_VAR var_list DOT expr
		{
		    var_list_ptr vp;
		    g_ptr e = $4;
		    string acc = find_hash(&Binder_Acc_Table, (pointer) $1);
		    for(vp = $2; vp != NULL; vp = vp->next) {
			g_ptr fn = Make_Lambda(get_base_name(vp->name),
					       Make_Lambda(acc, e)); 
			if( vp->type_hint != NULL ) {
			    TypeHint(fn, GLmake_arrow(vp->type_hint,
						      GLnew_tVar()));
			}
			e = Make_APPL_ND(
				Make_APPL_ND(
				    Make_APPL_ND(Make_VAR_leaf($1), fn),
				    Make_STRING_leaf(vp->name)),
				Make_VAR_leaf(acc));
		    }
		    $$ = e;
		}
		| BINDER_VAR var_list DOT expr
		{
		    var_list_ptr vp;
		    g_ptr e = $4;
		    for(vp = $2; vp != NULL; vp = vp->next) {
			e = Make_APPL_ND(
				Make_APPL_ND(Make_VAR_leaf($1),
					     Make_Lambda(vp->name, e)),
				Make_STRING_leaf(vp->name));
		    }
		    $$ = e;
		}
		| expr THEN_COND expr ELSE_COND expr
		{
		    /* return: cond $1 $3 $5 */
		    $$ = Make_3inp_Primitive(P_COND, $1, $3, $5);
		}
		| expr THEN_COND_VAR expr ELSE_COND_VAR expr
		{
		    /* c then_fn t else_fn e  -->  then_fn c (else_fn t e) */
		    $$ = Make_APPL_ND(
			    Make_APPL_ND(Make_VAR_leaf($2), $1),
			    Make_APPL_ND(
				Make_APPL_ND(Make_VAR_leaf($4), $3),
				$5));
		}
		| expr POSTFIX_VAR
		{ $$ = Make_APPL_ND(Make_VAR_leaf($2), $1); }
		| expr CROSSPROD expr
		{ 
		    $$ = Make_APPL_ND(
			    Make_APPL_ND(Make_VAR_leaf(s_crossprod), $1),
			    $3);
		}
		| expr INFIX_VAR_0 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_1 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_2 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_3 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_4 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_5 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_6 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_7 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_0 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_1 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_2 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_3 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_4 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_5 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_6 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_7 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_8 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_UNARY_9 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_8 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIX_VAR_9 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_0 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_1 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_2 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_3 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_4 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_5 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_6 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_7 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_8 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr INFIXR_VAR_9 expr
		{ $$ = Make_APPL_ND(Make_APPL_ND(Make_VAR_leaf($2), $1), $3); }
		| expr COMMA expr
		{ $$ = Make_2inp_Primitive(P_TUPLE, $1, $3); }
                | expr EQUAL expr
		{ $$ = Make_APPL_ND(
			    Make_APPL_ND(Make_VAR_leaf(eq_string),$1),
			    $3);
		}
		| REF arg_expr
		{ $$ = Make_1inp_Primitive(P_MK_REF_VAR,$2); }
		| expr05
		{ $$ = $1; }
		;
expr05		: expr05 arg_expr %prec DUMMY_LAST
		{ $$ = Make_APPL_ND($1, $2); }
		| expr05 PREFIX_VAR_0 arg_expr
		{ $$ = Make_APPL_ND($1, Make_APPL_ND(Make_VAR_leaf($2), $3)); }
		| PREFIX_VAR_0 arg_expr
		{ $$ = Make_APPL_ND(Make_VAR_leaf($1), $2); }
		| expr05 PREFIX_VAR_1 arg_expr
		{ $$ = Make_APPL_ND($1, Make_APPL_ND(Make_VAR_leaf($2), $3)); }
		| PREFIX_VAR_1 arg_expr
		{ $$ = Make_APPL_ND(Make_VAR_leaf($1), $2); }
		| INFIX_VAR_UNARY_0 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_1 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_2 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_3 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_4 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_5 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_6 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_7 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_8 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| INFIX_VAR_UNARY_9 arg_expr %prec UNARY_MINUS
		{ $$ = Make_APPL_ND(Make_VAR_leaf(Get_Unary($1)), $2); }
		| expr1
		{
		    $$ = $1;
		}
		;

arg_expr	: expr1
		{
		    $$ = $1;
		}
		| LCURL expr1 TYPE_SEP simple_type RCURL
		{
		    if( $2 != NULL && $4.ok ) {
			$$ = $2;
			TypeHint($2,$4.type);
		    } else {
			$$ = $2;
		    }
		}
		;

expr1		: LBRACK expr_list RBRACK
                {
                    $$ = $2;
                }
		| LBRACK RBRACK
		{
		    $$ = Make_NIL();
		}
		| LPAR RPAR
		{
		    $$ = Make_0inp_Primitive(P_VOID);
		}
		| LPAR expr RPAR
		{
		    $$ = $2;
		}
		| BEGIN_QUOTE expr END_QUOTE
		{
		    g_ptr term = Reflect_expr($2);
		    TypeHint(term, Get_Type("term", NULL, FALSE));
		    $$ = term;
		}
		| expr2
		{
		    $$ = $1;
		}
		;

expr_list	: rev_expr_list
		{
		    $$ = Convert_CL($1, TRUE);
		}
		;

rev_expr_list	: rev_expr_list COMMA expr %prec DUMMY_CONS
		{
		    $$ = Get_CL_node();
		    $$->expr = $3;
		    $$->op   = mk_tuple_tp;
		    $$->next = $1->next;
		    $1->next = $$;
		}
		| expr %prec DUMMY_CONS
		{
		    $$ = Get_CL_node();
		    $$->expr = $1;
		    $$->op   = mk_tuple_tp;
		    $$->next = $$;
		}
		;

expr2		: VART
		{
		    $$ = Make_VAR_leaf($1);
		}
		| VART FN_TYPE expr1
		{
		    $$ = Make_Named_Arg($1, $3);
		}
                | PRINTFT STRINGT
                { 
		    $$ = Make_Printf_Primitive(P_PRINTF, $2);
                }
                | SPRINTFT STRINGT
                { 
		    $$ = Make_Printf_Primitive(P_SPRINTF, $2);
                }
                | EPRINTFT STRINGT
                { 
		    $$ = Make_Printf_Primitive(P_EPRINTF, $2);
                }
                | FPRINTFT expr1 STRINGT
                { 
		    $$ = Make_APPL_ND(Make_Printf_Primitive(P_FPRINTF, $3), $2);
                }
                | SSCANFT STRINGT
                { 
		    $$ = Make_Printf_Primitive(P_SSCANF, $2);
                }
		| BIN_NUMBER
		{
		    $$ = Make_AINT_leaf(Arbi_FromString($1,2));
		}
		| HEX_NUMBER
		{
		    $$ = Make_AINT_leaf(Arbi_FromString($1,16));
		}
		| DEC_NUMBER
		{
		    $$ = Make_AINT_leaf(Arbi_FromString($1,10));
		}
		| FLOAT_NUMBER
		{
		    $$ = Make_APPL_ND(Make_VAR_leaf(s_str2float),
				      Make_STRING_leaf($1));
		}
		| TRUET
		{
		    $$ = Make_BOOL_leaf(B_One());
		}
		| FALSET
		{
		    $$ = Make_BOOL_leaf(B_Zero());
		}
		| NILT
		{
		    $$ = Make_NIL();
		}
		| STRINGT
		{
		    $$ = Make_STRING_leaf($1);
		}
		| UNQUOTE var_or_infix
		{
		    $$ = Make_1inp_Primitive(P_UNQUOTE, Make_VAR_leaf($2));
		}
		| FREE_BINDER_VAR var_or_infix
		{
		    $$ = Make_APPL_ND(Make_VAR_leaf($1), Make_STRING_leaf($2));
		}
		| FREE_BINDER_VAR BIN_NUMBER
		{
		    string tmp = strtemp("0b");
		    tmp = strappend($2);
		    tmp = wastrsave(&strings, tmp);
		    $$ = Make_APPL_ND(Make_VAR_leaf($1), Make_STRING_leaf(tmp));
		}
		| FREE_BINDER_VAR DEC_NUMBER
		{
		    $$ = Make_APPL_ND(Make_VAR_leaf($1), Make_STRING_leaf($2));
		}
		| FREE_BINDER_VAR FLOAT_NUMBER
		{
		    $$ = Make_APPL_ND(Make_VAR_leaf($1), Make_STRING_leaf($2));
		}
		| FREE_BINDER_VAR HEX_NUMBER
		{
		    string tmp = strtemp("0x");
		    tmp = strappend($2);
		    tmp = wastrsave(&strings, tmp);
		    $$ = Make_APPL_ND(Make_VAR_leaf($1), Make_STRING_leaf(tmp));
		}
		;

let_top :	LET  
		{
		    $$.file = $1.file;
		    $$.line = $1.line;
		    $$.recursive = FALSE;
		    $$.strict = FALSE;
		    $$.cached = FALSE;
		}
		| LETREC
		{
		    $$.file = $1.file;
		    $$.line = $1.line;
		    $$.recursive = TRUE ;
		    $$.strict = FALSE;
		    $$.cached = FALSE;
		}
		| S_LET    
		{
		    $$.file = $1.file;
		    $$.line = $1.line;
		    $$.recursive = FALSE;
		    $$.strict = TRUE ;
		    $$.cached = FALSE;
		}
		| S_LETREC 
		{
		    $$.file = $1.file;
		    $$.line = $1.line;
		    $$.recursive = TRUE ;
		    $$.strict = TRUE ;
		    $$.cached = FALSE;
		}
		| C_LET    
		{
		    $$.file = $1.file;
		    $$.line = $1.line;
		    $$.recursive = FALSE;
		    $$.strict = FALSE;
		    $$.cached = TRUE ;
		}
		| C_LETREC 
		{
		    $$.file = $1.file;
		    $$.line = $1.line;
		    $$.recursive = TRUE ;
		    $$.strict = FALSE;
		    $$.cached = TRUE ;
		}
		;
%%


static int
yyerror(const char *msg)
{
    unint		  i;
    extern int	  line_nbr;
    extern string cur_file_name;
    extern char	  prev_line[1001];
    extern int	  last_inp;
    int slen = strlen("syntax error,");
    if( strncmp("syntax error,", msg, slen) == 0 ) {
	msg = msg+slen;
    }
    FP(err_fp, "#### Syntax error in file %s, close to line %d: %s\n",
	    cur_file_name, line_nbr, msg);
    FP(err_fp, "%s\n", prev_line);
    for(i = 0; i < strlen(prev_line)-last_inp; i++) {
	if( prev_line[i] == '\t' )
	    FP(err_fp, "\t");
	else
	    FP(err_fp, " ");
    }
    FP(err_fp, "^\n");
    prev_line[0] = '\0';
    Emit_prompt("");
    extern jmp_buf *start_envp;
    longjmp(*start_envp,1);
}


void
Parse_Init()
{
    create_hash(&Infix_Table, 100, str_hash, str_equ);
    create_hash(&Infix_Unary_Table, 100, str_hash, str_equ);
    create_hash(&Binder_Acc_Table, 100, str_hash, str_equ);
    create_hash(&type_var_tbl, 100, str_hash, str_equ);
    new_mgr(&tvarl_rec_mgr, sizeof(tvar_list_rec));
    it	              = wastrsave(&strings, "it");
    eq_string         = wastrsave(&strings, "=");
    s_crossprod       = wastrsave(&strings, "#");
    s_str2float       = wastrsave(&strings, "str2float");
    s_prefix_0        = wastrsave(&strings, "prefix 0");
    s_prefix_1        = wastrsave(&strings, "prefix 1");
    s_infix_0         = wastrsave(&strings, "infix 0");
    s_infix_1         = wastrsave(&strings, "infix 1");
    s_infix_2         = wastrsave(&strings, "infix 2");
    s_infix_3         = wastrsave(&strings, "infix 3");
    s_infix_4         = wastrsave(&strings, "infix 4");
    s_infix_5         = wastrsave(&strings, "infix 5");
    s_infix_6         = wastrsave(&strings, "infix 6");
    s_infix_7         = wastrsave(&strings, "infix 7");
    s_infix_8         = wastrsave(&strings, "infix 8");
    s_infix_9         = wastrsave(&strings, "infix 9");
    s_infixr_0        = wastrsave(&strings, "infixr 0");
    s_infixr_1        = wastrsave(&strings, "infixr 1");
    s_infixr_2        = wastrsave(&strings, "infixr 2");
    s_infixr_3        = wastrsave(&strings, "infixr 3");
    s_infixr_4        = wastrsave(&strings, "infixr 4");
    s_infixr_5        = wastrsave(&strings, "infixr 5");
    s_infixr_6        = wastrsave(&strings, "infixr 6");
    s_infixr_7        = wastrsave(&strings, "infixr 7");
    s_infixr_8        = wastrsave(&strings, "infixr 8");
    s_infixr_9        = wastrsave(&strings, "infixr 9");
    s_infix_unary_0   = wastrsave(&strings, "infix_unary 0");
    s_infix_unary_1   = wastrsave(&strings, "infix_unary 1");
    s_infix_unary_2   = wastrsave(&strings, "infix_unary 2");
    s_infix_unary_3   = wastrsave(&strings, "infix_unary 3");
    s_infix_unary_4   = wastrsave(&strings, "infix_unary 4");
    s_infix_unary_5   = wastrsave(&strings, "infix_unary 5");
    s_infix_unary_6   = wastrsave(&strings, "infix_unary 6");
    s_infix_unary_7   = wastrsave(&strings, "infix_unary 7");
    s_infix_unary_8   = wastrsave(&strings, "infix_unary 8");
    s_infix_unary_9   = wastrsave(&strings, "infix_unary 9");
    s_postfix         = wastrsave(&strings, "postfix");
    s_binder          = wastrsave(&strings, "binder");
    s_nonfix          = wastrsave(&strings, "nonfix");
    s_then_binder     = wastrsave(&strings, "then_binder");
    s_else_binder     = wastrsave(&strings, "else_binder");
    s_binder_with_acc = wastrsave(&strings, "binder_with_accumulator");
    s_sz_binder_with_acc = wastrsave(&strings, "size_binder_with_accumulator");
    s_free_binder     = wastrsave(&strings, "free_binder");
}

void
Install_BuiltIns()
{
    int		i;
    for(i = 0; i < nbr_functions; i++) {
        string name = wastrsave(&strings, functions[i].name);
	g_ptr fn = Make_0inp_Primitive(functions[i].prim_fn);
	result_ptr res = Compile(NULL, fn, NULL, FALSE,FALSE);
	symb_tbl = New_fn_def(name, res, symb_tbl, FALSE, NULL, -1);
	Free_result_ptr(res);
	if( functions[i].infix > 0 )
	    Insert_infix(name, functions[i].infix);
	if( functions[i].infixr > 0 )
	    Insert_infixr(name, functions[i].infixr);
	if( functions[i].postfix )
	    Insert_postfix(name);
	if( functions[i].binder )
	    Insert_binder(name);
        if( functions[i].non_lazy ) {
            if( !Make_non_lazy(name, symb_tbl) ) {
                FP(err_fp, "FAILED to make %s non_lazy????\n", name);
            }
        }
    }
}

int
Get_Infix(string s)
{
    pointer r;
    r = find_hash(&Infix_Table, (pointer) s);
    if( r == NULL )
	return(VART);
    return( PTR2INT(r) );
}

void
Remove_Infix(string s)
{
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
}

void
Insert_if_then_else(string then_fun, string else_fun)
{
    then_fun = wastrsave(&strings, then_fun);
    if( Get_Infix(then_fun) != VART ) {
	delete_hash(&Infix_Table, (pointer) then_fun);
    }
    insert_hash(&Infix_Table, (pointer) then_fun, (pointer) THEN_COND_VAR);
    else_fun = wastrsave(&strings, else_fun);
    if( Get_Infix(else_fun) != VART ) {
	delete_hash(&Infix_Table, (pointer) else_fun);
    }
    insert_hash(&Infix_Table, (pointer) else_fun, (pointer) ELSE_COND_VAR);
}

void
Insert_then_binder(string fun)
{
    fun = wastrsave(&strings, fun);
    if( Get_Infix(fun) != VART ) {
	delete_hash(&Infix_Table, (pointer) fun);
    }
    insert_hash(&Infix_Table, (pointer) fun, (pointer) THEN_COND_VAR);
}

void
Insert_else_binder(string fun)
{
    fun = wastrsave(&strings, fun);
    if( Get_Infix(fun) != VART ) {
	delete_hash(&Infix_Table, (pointer) fun);
    }
    insert_hash(&Infix_Table, (pointer) fun, (pointer) ELSE_COND_VAR);
}

void
Insert_binder_with_acc(string name, string acc_name)
{
    name = wastrsave(&strings, name);
    acc_name = wastrsave(&strings, acc_name);
    if( Get_Infix(name) != VART ) {
	delete_hash(&Infix_Table, (pointer) name);
    }
    insert_hash(&Infix_Table, (pointer) name, (pointer) BINDER_WITH_ACC_VAR);
    if( find_hash(&Binder_Acc_Table, (pointer) name) != NULL )
	delete_hash(&Binder_Acc_Table, (pointer) name);
    insert_hash(&Binder_Acc_Table, (pointer) name, (pointer) acc_name);
}

void
Insert_size_binder_with_acc(string name, string acc_name)
{
    name = wastrsave(&strings, name);
    acc_name = wastrsave(&strings, acc_name);
    if( Get_Infix(name) != VART ) {
	delete_hash(&Infix_Table, (pointer) name);
    }
    insert_hash(&Infix_Table, (pointer) name, (pointer) SZ_BINDER_WITH_ACC_VAR);
    if( find_hash(&Binder_Acc_Table, (pointer) name) != NULL )
	delete_hash(&Binder_Acc_Table, (pointer) name);
    insert_hash(&Binder_Acc_Table, (pointer) name, (pointer) acc_name);
}

void
Insert_infix(string s, int precedence)
{
    s = wastrsave(&strings, s);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    switch(precedence) {
	case 0:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_0); break;
	case 1:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_1); break;
	case 2:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_2); break;
	case 3:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_3); break;
	case 4:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_4); break;
	case 5:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_5); break;
	case 6:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_6); break;
	case 7:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_7); break;
	case 8:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_8); break;
	case 9:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_9); break;
	default:
	    DIE("Insert_infix with illegal precedence");
    }
}

string
Get_Unary(string s)
{
    string ufn = find_hash(&Infix_Unary_Table, (pointer) s);
    if( ufn == NULL) { DIE("Should not be possible"); }
    return ufn;
}

void
Insert_infix_unary(string s, int precedence, string unary_function)
{
    s = wastrsave(&strings, s);
    unary_function = wastrsave(&strings, unary_function);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    if( find_hash(&Infix_Unary_Table, (pointer) s) != NULL ) {
	delete_hash(&Infix_Unary_Table, (pointer) s);
    }
    switch(precedence) {
	case 0:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_0);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 1:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_1);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 2:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_2);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 3:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_3);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 4:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_4);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 5:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_5);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 6:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_6);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 7:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_7);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 8:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_8);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	case 9:
	    insert_hash(&Infix_Table, (pointer) s, (pointer)INFIX_VAR_UNARY_9);
	    insert_hash(&Infix_Unary_Table, (pointer) s, unary_function);
	    break;
	default:
	    DIE("Insert_infix_unary with illegal precedence");
    }
}


void
Insert_prefix(string s, int precedence)
{
    s = wastrsave(&strings, s);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    switch(precedence) {
	case 0:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)PREFIX_VAR_0); break;
	case 1:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)PREFIX_VAR_1); break;
    }
}

void
Insert_infixr(string s, int precedence)
{
    s = wastrsave(&strings, s);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    switch(precedence) {
	case 0:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_0); break;
	case 1:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_1); break;
	case 2:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_2); break;
	case 3:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_3); break;
	case 4:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_4); break;
	case 5:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_5); break;
	case 6:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_6); break;
	case 7:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_7); break;
	case 8:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_8); break;
	case 9:
	    insert_hash(&Infix_Table, (pointer)s, (pointer)INFIXR_VAR_9); break;
    }
}

void
Insert_postfix(string s)
{
    s = wastrsave(&strings, s);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    insert_hash(&Infix_Table, (pointer) s, (pointer) POSTFIX_VAR);
}

void
Insert_binder(string s)
{
    s = wastrsave(&strings, s);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    insert_hash(&Infix_Table, (pointer) s, (pointer) BINDER_VAR);
}

void
Insert_free_binder(string s)
{
    s = wastrsave(&strings, s);
    if( Get_Infix(s) != VART ) {
	delete_hash(&Infix_Table, (pointer) s);
    }
    insert_hash(&Infix_Table, (pointer) s, (pointer) FREE_BINDER_VAR);
}

static tvar_list_ptr
get_tvar_list_rec()
{
    return( (tvar_list_ptr) new_rec(&tvarl_rec_mgr) );
}

string
Get_Fixity(string name) 
{
    int fix = Get_Infix(name);
    if( strcmp(name,"#") == 0 ) { return s_infixr_0; }
    switch( fix ) {
	case PREFIX_VAR_0:	    return s_prefix_0;
	case PREFIX_VAR_1:	    return s_prefix_1;
	case INFIX_VAR_0:	    return s_infix_0;
	case INFIX_VAR_1:	    return s_infix_1;
	case INFIX_VAR_2:	    return s_infix_2;
	case INFIX_VAR_3:	    return s_infix_3;
	case INFIX_VAR_4:	    return s_infix_4;
	case INFIX_VAR_5:	    return s_infix_5;
	case INFIX_VAR_6:	    return s_infix_6;
	case INFIX_VAR_7:	    return s_infix_7;
	case INFIX_VAR_8:	    return s_infix_8;
	case INFIX_VAR_9:	    return s_infix_9;
	case INFIXR_VAR_0:	    return s_infixr_0;
	case INFIXR_VAR_1:	    return s_infixr_1;
	case INFIXR_VAR_2:	    return s_infixr_2;
	case INFIXR_VAR_3:	    return s_infixr_3;
	case INFIXR_VAR_4:	    return s_infixr_4;
	case INFIXR_VAR_5:	    return s_infixr_5;
	case INFIXR_VAR_6:	    return s_infixr_6;
	case INFIXR_VAR_7:	    return s_infixr_7;
	case INFIXR_VAR_8:	    return s_infixr_8;
	case INFIXR_VAR_9:	    return s_infixr_9;
	case INFIX_VAR_UNARY_0:	    return s_infix_unary_0;
	case INFIX_VAR_UNARY_1:	    return s_infix_unary_1;
	case INFIX_VAR_UNARY_2:	    return s_infix_unary_2;
	case INFIX_VAR_UNARY_3:	    return s_infix_unary_3;
	case INFIX_VAR_UNARY_4:	    return s_infix_unary_4;
	case INFIX_VAR_UNARY_5:	    return s_infix_unary_5;
	case INFIX_VAR_UNARY_6:	    return s_infix_unary_6;
	case INFIX_VAR_UNARY_7:	    return s_infix_unary_7;
	case INFIX_VAR_UNARY_8:	    return s_infix_unary_8;
	case INFIX_VAR_UNARY_9:	    return s_infix_unary_9;
	case POSTFIX_VAR:	    return s_postfix;
	case BINDER_VAR:	    return s_binder;
	case THEN_COND_VAR:	    return s_then_binder;
	case ELSE_COND_VAR:	    return s_else_binder;
	case BINDER_WITH_ACC_VAR:   return s_binder_with_acc;
	case SZ_BINDER_WITH_ACC_VAR:  return s_sz_binder_with_acc;
	case FREE_BINDER_VAR:	    return s_free_binder;
	default:		    return s_nonfix;
    }
}

static void
print_fixity(pointer key, pointer data)
{
    switch( PTR2INT(data) ) {
	case PREFIX_VAR_0:
	    fprintf(tmp_fp, "prefix 0 %s;\n", (string) key); break;
	case PREFIX_VAR_1:
	    fprintf(tmp_fp, "prefix 1 %s;\n", (string) key); break;
	case INFIX_VAR_0:
	    fprintf(tmp_fp, "infix 0 %s;\n", (string) key); break;
	case INFIX_VAR_1:
	    fprintf(tmp_fp, "infix 1 %s;\n", (string) key); break;
	case INFIX_VAR_2:
	    fprintf(tmp_fp, "infix 2 %s;\n", (string) key); break;
	case INFIX_VAR_3:
	    fprintf(tmp_fp, "infix 3 %s;\n", (string) key); break;
	case INFIX_VAR_4:
	    fprintf(tmp_fp, "infix 4 %s;\n", (string) key); break;
	case INFIX_VAR_5:
	    fprintf(tmp_fp, "infix 5 %s;\n", (string) key); break;
	case INFIX_VAR_6:
	    fprintf(tmp_fp, "infix 6 %s;\n", (string) key); break;
	case INFIX_VAR_7:
	    fprintf(tmp_fp, "infix 7 %s;\n", (string) key); break;
	case INFIX_VAR_8:
	    fprintf(tmp_fp, "infix 8 %s;\n", (string) key); break;
	case INFIX_VAR_9:
	    fprintf(tmp_fp, "infix 9 %s;\n", (string) key); break;
	case INFIXR_VAR_0:
	    fprintf(tmp_fp, "infixr 0 %s;\n", (string) key); break;
	case INFIXR_VAR_1:
	    fprintf(tmp_fp, "infixr 1 %s;\n", (string) key); break;
	case INFIXR_VAR_2:
	    fprintf(tmp_fp, "infixr 2 %s;\n", (string) key); break;
	case INFIXR_VAR_3:
	    fprintf(tmp_fp, "infixr 3 %s;\n", (string) key); break;
	case INFIXR_VAR_4:
	    fprintf(tmp_fp, "infixr 4 %s;\n", (string) key); break;
	case INFIXR_VAR_5:
	    fprintf(tmp_fp, "infixr 5 %s;\n", (string) key); break;
	case INFIXR_VAR_6:
	    fprintf(tmp_fp, "infixr 6 %s;\n", (string) key); break;
	case INFIXR_VAR_7:
	    fprintf(tmp_fp, "infixr 7 %s;\n", (string) key); break;
	case INFIXR_VAR_8:
	    fprintf(tmp_fp, "infixr 8 %s;\n", (string) key); break;
	case INFIXR_VAR_9:
	    fprintf(tmp_fp, "infixr 9 %s;\n", (string) key); break;
	case POSTFIX_VAR:
	    fprintf(tmp_fp, "postfix %s;\n", (string) key); break;
	case BINDER_VAR:
	    fprintf(tmp_fp, "binder %s;\n", (string) key); break;
	case THEN_COND_VAR:
	    fprintf(tmp_fp, "then_cond %s;\n", (string) key); break;
	case ELSE_COND_VAR:
	    fprintf(tmp_fp, "else_cond %s;\n", (string) key); break;
	case INFIX_VAR_UNARY_0:
	    fprintf(tmp_fp, "infix_unary 0 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_1:
	    fprintf(tmp_fp, "infix_unary 1 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_2:
	    fprintf(tmp_fp, "infix_unary 2 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_3:
	    fprintf(tmp_fp, "infix_unary 3 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_4:
	    fprintf(tmp_fp, "infix_unary 4 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_5:
	    fprintf(tmp_fp, "infix_unary 5 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_6:
	    fprintf(tmp_fp, "infix_unary 6 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_7:
	    fprintf(tmp_fp, "infix_unary 7 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_8:
	    fprintf(tmp_fp, "infix_unary 8 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	case INFIX_VAR_UNARY_9:
	    fprintf(tmp_fp, "infix_unary 9 %s %s;\n", (string) key,
		    Get_Unary(key));
	    break;
	default:
	    fprintf(stderr, "Unknown fixity (%d) of %s\n",
			    PTR2INT(data), (string) key);
    }
}

static tvar_list_ptr
cons_name(string name, tvar_list_ptr l)
{
    tvar_list_ptr res = get_tvar_list_rec();
    res->var = name;
    res->next = l;
    return res;
}

static typeExp_ptr 
find_typevar(string name, bool insert)
{
    typeExp_ptr res;
    if( (res = find_hash(&type_var_tbl, (pointer) name)) != NULL ) {
	return res;
    }
    if( !insert ) { return NULL; }
    res = GLnew_tVar();
    insert_hash(&type_var_tbl, (pointer) name, (pointer) res);
    return res;
}


static g_ptr
make_project_fun(string name, g_ptr pexpr)
{
    g_ptr def_expr = Make_VAR_leaf(it);
    g_ptr var_expr = Make_VAR_leaf(name);
    g_ptr res;
    res = TrArg(pexpr, var_expr, FALSE);
    if( res != NULL ) {
	Sprintf(buf, "_Q_1");
	res = Make_APPL_ND(res,
		       Make_VAR_leaf(wastrsave(&strings, buf)));
	Sprintf(buf, "No pattern matched for val expression\n");
	if( Can_Fail_Pat_Match(res) ) {
	    res = Make_2inp_Primitive(P_PCATCH, res,
		      Make_1inp_Primitive(P_ERROR, 
			    Make_STRING_leaf(
				     wastrsave(&strings,buf))));
	}
	res = Add_args(1, res);
	return( Make_APPL_ND(res, def_expr) );
    } else {
	return( NULL );
    }
}

static string
get_base_name(string name)
{
    string tmp = index(name, '<');
    if( tmp == NULL ) {
	return name;
    }
    string base = strtemp(name);
    *(base+(tmp-name)) = 0;
    return( wastrsave(&strings, base) );
}
