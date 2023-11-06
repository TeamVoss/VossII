//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include "symbol.h"
#include "graph.h"
#include "typecheck.h"
#include <fnmatch.h>

/************************************************************************/
/*                      Global Variables Referenced                     */
/************************************************************************/
extern bool             cephalopode_mode;
extern str_mgr          strings;
extern hash_record      Constructor_Table;
extern hash_record      TypeSignatureTbl;
extern int              line_nbr;
extern string           cur_file_name;
extern char             FailBuf[4096];
extern comment_list_ptr cur_doc_comments;
extern FILE             *odests_fp;
extern buffer           ext_fun_buf;
extern buffer           ext_obj_buf;
extern rec_mgr          var_list_rec_mgr;
extern bool             file_load;
extern jmp_buf          *start_envp;

/************************************************************************/
/*                      Global Variables Declared                       */
/************************************************************************/
symbol_tbl_ptr          symb_tbl = NULL;
rec_mgr                 name_list_rec_mgr;
rec_mgr                 impl_arg_rec_mgr;


/************************************************************************/
/*                      Local Variables                                 */
/************************************************************************/
static buffer       ADT_buf;
static int          ADT_level = 1;
static rec_mgr      fn_rec_mgr;
static int	    fn_rec_id_cnt = 0;
static rec_mgr      hash_table_mgr;
static rec_mgr      oll_rec_mgr;
static rec_mgr      symb_tbl_mgr;
static rec_mgr      arg_names_rec_mgr;
static char         buf[1024];
static string       s_star;
static string       s_CELL;
static string       s_dummy;
static string       s_empty;
static char         help_buf[4096];
static char         type_buf[4096];
static hash_record  bound_var_tbl;
static int          overloaded_calls;
static jmp_buf      context_env;
static int          builtins_len;

// -------------------------------------------------------------------------

static char DIR_help[] = "\n"
"Function: DIR\n"
"\n"
"Built-in\n"
"\n"
"Arguments:\n"
"\n"
"Return type: string\n"
"\n"
"Fixity: nonfix\n"
"\n"
"Description:\n"
"\n"
"Contains the name of the directory that the file currently\n"
"being read is stored in. The string will always end with a slash (/).\n"
"\n"
"Common use is to load (DIR^\"name_of_local_file\"); from a master file.\n"
"Then only the location of the master file needs to be given and all\n"
"files it needs can be loaded relative to this location.\n"
;

// -------------------------------------------------------------------------

static char eprintf_help[] = "\n"
"Function: eprintf\n"
"\n"
"Built-in\n"
"\n"
"Arguments:\n"
"  format :: Explicit string.\n"
"  ...   \n"
"Type of remaining arguments determined by format.\n"
"\n"
"Fixity: nonfix\n"
"\n"
"Description:\n"
"\n"
"A printf like function that uses a format string and arguments to\n"
"create a suitable error messgae followed by raising an exception.\n"
"\n"
"The format string is composed of zero  or more  directives: ordinary\n"
"characters (not %), which are copied unchanged to the output stream;\n"
"and conversion specifications, each of which results in fetching zero\n"
"or more subsequent arguments. Each conversion specification is\n"
"introduced by the character %, and ends with a conversion specifier.\n"
"In between there may be (in this order) zero or more flags, and an\n"
"optional length modifier.\n"
"\n"
"The arguments must correspond properly (after type promotion) with the\n"
"conversion specifier. The arguments are used in the order given, \n"
"where each '*' and each conversion specifier asks for the next argument.\n"
"\n"
"Flag characters:\n"
"----------------\n"
"The character %% is followed by zero or more of the following flags:\n"
"\n"
"    0   The value should be zero padded.\n"
"    -   The value should be left justified.\n"
"\n"
"Field width:\n"
"------------\n"
"\n"
"An optional decimal digit string (with nonzero first digit) specifying\n"
"a field width. If the converted value has fewer characters\n"
"than the field width, it will be padded with spaces on the left (or\n"
"right, if the left-adjustment flag has been given). Instead of a deci‐\n"
"mal digit string one may write \"*\"  to specify that the field width is\n"
"given in the next argument.  A negative field  width is taken as a '-'\n"
"flag followed by a positive field width.\n"
"If the resulting value does not fit in the width given, an exception is\n"
"raised.\n"
"\n"
"Conversion specifiers:\n"
"----------------------\n"
"    b   the integer argument will be converted to a binary number\n"
"    o   the integer argument will be converted to an octal number\n"
"    d   the integer argument will be converted to a signed decimal number\n"
"    x   the integer argument will be converted to a hexadecimal number\n"
"    s   the string argument will be copied out\n"
"    S   the list of strings argument will be copied out enclosed in [] and\n"
"        each string separated by a comma.\n"
"    B   the BDD (bool) argument will be converted to a string and copied to\n"
"        the output\n"
;

// -------------------------------------------------------------------------

static char fprintf_help[] = "\n"
"Function: fprintf\n"
"\n"
"Built-in\n"
"\n"
"Arguments:\n"
"      fp :: stream\n"
"  format :: Explicit string.\n"
"  ...   \n"
"Type of remaining arguments determined by format.\n"
"\n"
"Return type: void\n"
"\n"
"Fixity: nonfix\n"
"\n"
"Description:\n"
"\n"
"A printf like function that uses a format string and arguments to\n"
"write to the stream fp.\n"
"\n"
"The format string is composed of zero  or more  directives: ordinary\n"
"characters (not %), which are copied unchanged to the output stream;\n"
"and conversion specifications, each of which results in fetching zero\n"
"or more subsequent arguments. Each conversion specification is\n"
"introduced by the character %, and ends with a conversion specifier.\n"
"In between there may be (in this order) zero or more flags, and an\n"
"optional length modifier.\n"
"\n"
"The arguments must correspond properly (after type promotion) with the\n"
"conversion specifier. The arguments are used in the order given, \n"
"where each '*' and each conversion specifier asks for the next argument.\n"
"\n"
"Flag characters:\n"
"----------------\n"
"The character %% is followed by zero or more of the following flags:\n"
"\n"
"    0   The value should be zero padded.\n"
"    -   The value should be left justified.\n"
"\n"
"Field width:\n"
"------------\n"
"\n"
"An optional decimal digit string (with nonzero first digit) specifying\n"
"a field width. If the converted value has fewer characters\n"
"than the field width, it will be padded with spaces on the left (or\n"
"right, if the left-adjustment flag has been given). Instead of a deci‐\n"
"mal digit string one may write \"*\"  to specify that the field width is\n"
"given in the next argument.  A negative field  width is taken as a '-'\n"
"flag followed by a positive field width.\n"
"If the resulting value does not fit in the width given, an exception is\n"
"raised.\n"
"\n"
"Conversion specifiers:\n"
"----------------------\n"
"    b   the integer argument will be converted to a binary number\n"
"    o   the integer argument will be converted to an octal number\n"
"    d   the integer argument will be converted to a signed decimal number\n"
"    x   the integer argument will be converted to a hexadecimal number\n"
"    s   the string argument will be copied out\n"
"    S   the list of strings argument will be copied out enclosed in [] and\n"
"        each string separated by a comma.\n"
"    B   the BDD (bool) argument will be converted to a string and copied to\n"
"        the output\n"
;


// -------------------------------------------------------------------------

static char printf_help[] = "\n"
"Function: printf\n"
"\n"
"Built-in\n"
"\n"
"Arguments:\n"
"  format :: Explicit string.\n"
"  ...   \n"
"Type of remaining arguments determined by format.\n"
"\n"
"Return type: void\n"
"\n"
"Fixity: nonfix\n"
"\n"
"Description:\n"
"\n"
"A function that uses a format string and arguments to write to stdout.\n"
"\n"
"The format string is composed of zero  or more  directives: ordinary\n"
"characters (not %), which are copied unchanged to the output stream;\n"
"and conversion specifications, each of which results in fetching zero\n"
"or more subsequent arguments. Each conversion specification is\n"
"introduced by the character %, and ends with a conversion specifier.\n"
"In between there may be (in this order) zero or more flags, and an\n"
"optional length modifier.\n"
"\n"
"The arguments must correspond properly (after type promotion) with the\n"
"conversion specifier. The arguments are used in the order given, \n"
"where each '*' and each conversion specifier asks for the next argument.\n"
"\n"
"Flag characters:\n"
"----------------\n"
"The character %% is followed by zero or more of the following flags:\n"
"\n"
"    0   The value should be zero padded.\n"
"    -   The value should be left justified.\n"
"\n"
"Field width:\n"
"------------\n"
"\n"
"An optional decimal digit string (with nonzero first digit) specifying\n"
"a field width. If the converted value has fewer characters\n"
"than the field width, it will be padded with spaces on the left (or\n"
"right, if the left-adjustment flag has been given). Instead of a deci‐\n"
"mal digit string one may write \"*\"  to specify that the field width is\n"
"given in the next argument.  A negative field  width is taken as a '-'\n"
"flag followed by a positive field width.\n"
"If the resulting value does not fit in the width given, an exception is\n"
"raised.\n"
"\n"
"Conversion specifiers:\n"
"----------------------\n"
"    b   the integer argument will be converted to a binary number\n"
"    o   the integer argument will be converted to an octal number\n"
"    d   the integer argument will be converted to a signed decimal number\n"
"    x   the integer argument will be converted to a hexadecimal number\n"
"    s   the string argument will be copied out\n"
"    S   the list of strings argument will be copied out enclosed in [] and\n"
"        each string separated by a comma.\n"
"    B   the BDD (bool) argument will be converted to a string and copied to\n"
"        the output\n"
;


// -------------------------------------------------------------------------

static char sprintf_help[] = "\n"
"Function: sprintf\n"
"\n"
"Built-in\n"
"\n"
"Arguments:\n"
"  format :: Explicit string.\n"
"  ...   \n"
"Type of remaining arguments determined by format.\n"
"\n"
"Return type: string\n"
"\n"
"Fixity: nonfix\n"
"\n"
"Description:\n"
"\n"
"A printf like function that uses a format string and arguments to\n"
"create a string.\n"
"\n"
"The format string is composed of zero  or more  directives: ordinary\n"
"characters (not %), which are copied unchanged to the output stream;\n"
"and conversion specifications, each of which results in fetching zero\n"
"or more subsequent arguments. Each conversion specification is\n"
"introduced by the character %, and ends with a conversion specifier.\n"
"In between there may be (in this order) zero or more flags, and an\n"
"optional length modifier.\n"
"\n"
"The arguments must correspond properly (after type promotion) with the\n"
"conversion specifier. The arguments are used in the order given, \n"
"where each '*' and each conversion specifier asks for the next argument.\n"
"\n"
"Flag characters:\n"
"----------------\n"
"The character %% is followed by zero or more of the following flags:\n"
"\n"
"    0   The value should be zero padded.\n"
"    -   The value should be left justified.\n"
"\n"
"Field width:\n"
"------------\n"
"\n"
"An optional decimal digit string (with nonzero first digit) specifying\n"
"a field width. If the converted value has fewer characters\n"
"than the field width, it will be padded with spaces on the left (or\n"
"right, if the left-adjustment flag has been given). Instead of a deci‐\n"
"mal digit string one may write \"*\"  to specify that the field width is\n"
"given in the next argument.  A negative field  width is taken as a '-'\n"
"flag followed by a positive field width.\n"
"If the resulting value does not fit in the width given, an exception is\n"
"raised.\n"
"\n"
"Conversion specifiers:\n"
"----------------------\n"
"    b   the integer argument will be converted to a binary number\n"
"    o   the integer argument will be converted to an octal number\n"
"    d   the integer argument will be converted to a signed decimal number\n"
"    x   the integer argument will be converted to a hexadecimal number\n"
"    s   the string argument will be copied out\n"
"    S   the list of strings argument will be copied out enclosed in [] and\n"
"        each string separated by a comma.\n"
"    B   the BDD (bool) argument will be converted to a string and copied to\n"
"        the output\n"
;

// -------------------------------------------------------------------------

static char sscanf_help[] = "\n"
"Function: sscanf\n"
"\n"
"Built-in\n"
"\n"
"Arguments:\n"
"  format :: Explicit string.\n"
"       s :: string\n"
"\n"
"Return type:\n"
"    Determined by format.\n"
"\n"
"Fixity: nonfix\n"
"\n"
"Description:\n"
"\n"
"Scans the input string s according to the format and extracts the\n"
"corresponding elements.\n"
"The return type is a tuple of all conversion types.\n"
"\n"
"The format string is composed of zero  or more  directives: ordinary\n"
"characters (not %), which must match the input string, and conversion\n"
"specifications, each of which results in converting part of the input.\n"
"\n"
"Flag characters:\n"
"----------------\n"
"The character %% is followed by zero or more of the following flags:\n"
"\n"
"    0   The value should be zero padded.\n"
"    -   The value should be left justified.\n"
"\n"
"Field width:\n"
"------------\n"
"\n"
"An optional decimal digit string (with nonzero first digit) specifying\n"
"a field width. If the converted value has fewer characters\n"
"than the field width, it will be padded with spaces on the left (or\n"
"right, if the left-adjustment flag has been given). Instead of a deci‐\n"
"mal digit string one may write \"*\"  to specify that the field width is\n"
"given in the next argument.  A negative field  width is taken as a '-'\n"
"flag followed by a positive field width.\n"
"If the resulting value does not fit in the width given, an exception is\n"
"raised.\n"
"\n"
"Conversion specifiers:\n"
"----------------------\n"
"    b   a binary number is read\n"
"    o   an octal number is read\n"
"    d   a decimal number is read\n"
"    x   a hexadecimal number is read\n"
"    s   a string is read\n"
;

/************************************************************************/
/*                      Local Functions                                 */
/************************************************************************/
static symbol_tbl_ptr   create_empty_symb_tbl();
static g_ptr            add_non_lazy_context_rec(buffer *ctxt,
                                                 symbol_tbl_ptr stbl,
                                                 g_ptr node);
static bool             is_non_lazy(symbol_tbl_ptr stbl, string name);
static bool             check_match(string pat, string s);
static bool             check_type_match(string arg_pat, string res_pat,
                                         typeExp_ptr type);
static g_ptr            cond_mk_app(g_ptr old, g_ptr E, g_ptr F);
static g_ptr            cond_mk_cons(g_ptr old, g_ptr E, g_ptr F);
static impl_arg_ptr     get_overloaded_calls_rec(impl_arg_ptr l,
                                                 symbol_tbl_ptr stbl,
                                                 g_ptr node);
static void             update_stbl(symbol_tbl_ptr stbl, fn_ptr fp);
static bool             built_in(string filename);
static bool             already_defined(symbol_tbl_ptr stbl, fn_ptr fp);
static g_ptr            ignore_P_Y(g_ptr node);
static g_ptr            ignore_P_DEBUG(g_ptr node);
static g_ptr		ignore_P_NAMED_ARG(g_ptr node);
static bool             get_named_arg(g_ptr node, name_expr_rec *namep,
				      g_ptr *nextp);
static g_ptr            ignore_simple_P_PCATCH(g_ptr onode);
static g_ptr            ignore_P_CACHE(g_ptr onode);
static g_ptr            ignore_P_STRICT_ARGS(g_ptr onode);
static bool		is_P_NAMED_ARG(g_ptr node, g_ptr *E, g_ptr *varp,
				       g_ptr *def_val);
static bool		get_HFL_arg_name(g_ptr node, name_expr_rec *nepp,
					 g_ptr *next_nodep);
static fn_ptr		get_fn_rec();

/************************************************************************/
/*                      Public Functions                                */
/************************************************************************/

void
Init_symbol()
{
    new_buf(&ADT_buf, 10, sizeof(fn_ptr));
    aligned_new_mgr(&fn_rec_mgr, sizeof(fn_rec), 8);
    new_mgr(&oll_rec_mgr, sizeof(oll_rec));
    new_mgr(&hash_table_mgr, sizeof(hash_record));
    new_mgr(&symb_tbl_mgr, sizeof(symbol_tbl_rec));
    new_mgr(&name_list_rec_mgr, sizeof(name_list_rec));
    new_mgr(&impl_arg_rec_mgr, sizeof(impl_arg_rec));
    new_mgr(&arg_names_rec_mgr, sizeof(arg_names_rec));
    symb_tbl = create_empty_symb_tbl();
    s_star = wastrsave(&strings, "*");
    s_CELL = wastrsave(&strings, "CELL");
    s_dummy = wastrsave(&strings, "//DuMmY");
    s_empty = wastrsave(&strings, "");
    fn_ptr dummy = (fn_ptr) get_fn_rec();
    dummy->ADT_level = ADT_level;
    dummy->name = s_dummy;
    dummy->next      = symb_tbl->def_list;
    symb_tbl->def_list = dummy;
    update_stbl(symb_tbl, dummy);
    dummy->visible        = TRUE;
    dummy->in_use         = TRUE;
    dummy->file_name      = wastrsave(&strings, "_dummy_");;
    dummy->start_line_nbr = 1;
    dummy->end_line_nbr   = 1;
    dummy->comments       = NULL;
    dummy->arg_names      = NULL;
    dummy->non_lazy       = FALSE;
    dummy->forward        = FALSE;
    dummy->expr           = NULL;
    dummy->expr_init      = NULL;
    dummy->expr_comb      = NULL;
    dummy->signature      = NULL;
    dummy->super_comb     = NULL;
    dummy->overload       = FALSE;
    dummy->open_overload  = FALSE;
    dummy->overload_list  = NULL;
    dummy->type           = NULL;
    dummy->implicit_args  = NULL;
    builtins_len = strlen("builtins.fl");
}

void
Mark_symbols()
{
    fn_ptr      fp;
    // Make sure all overload versions in_use are marked as in_use
    FOR_REC(&fn_rec_mgr, fn_ptr, fp) {
        if( fp->in_use && fp->overload_list != NULL ) {
            for(oll_ptr ol = fp->overload_list; ol != NULL; ol = ol->next) {
                ol->fn->in_use = TRUE;
            }
        }
    }
    FOR_REC(&fn_rec_mgr, fn_ptr, fp) {
        if( fp->in_use ) {
	    Mark(fp->expr);
	    Mark(fp->expr_init);
	    Mark(fp->expr_comb);
            Mark(fp->super_comb);
	    if( fp->has_default_values ) {
		arg_names_ptr ap = fp->arg_names;
		while( ap != NULL ) {
		    Mark(ap->default_value);
		    ap = ap->next;
		}
	    }
        } else {
            fp->expr = NULL;
            fp->expr_init = NULL;
            fp->expr_comb = NULL;
            fp->type = NULL;
            fp->implicit_args = NULL;
            fp->overload = FALSE;
            fp->open_overload = FALSE;
            fp->overload_list = NULL;
	    fp->has_default_values  = FALSE;
	    arg_names_ptr ap = fp->arg_names;
	    while( ap != NULL ) {
		arg_names_ptr tmp = ap;
		ap = ap->next;
		free_rec(&arg_names_rec_mgr, tmp);
	    }
	    fp->arg_names = NULL;
        }
    }
}

g_ptr
Get_Fun_Expr(fn_ptr fp)
{
    if( fp == NULL ) { return NULL; }
    return( fp->expr );
}

typeExp_ptr
Get_Fun_Type(fn_ptr fp)
{
    if( fp == NULL ) { return NULL; }
    return( fp->type );
}

string
Get_Fun_Signature(fn_ptr fp)
{
    if( fp == NULL ) { return NULL; }
    return( fp->signature );
}


impl_arg_ptr
Gather_overloaded_calls(symbol_tbl_ptr stbl, g_ptr node)
{
    overloaded_calls = 0;
    create_hash(&bound_var_tbl, 100, str_hash, str_equ);
    impl_arg_ptr res = get_overloaded_calls_rec(NULL, stbl, node);
    dispose_hash(&bound_var_tbl, NULLFCN);
    return(res);
}

static g_ptr
top_add_non_lazy_context(buffer *contextp, symbol_tbl_ptr stbl, g_ptr *nodep)
{
    g_ptr res = NULL;
    if( setjmp(context_env) == 0 ) {
        res = add_non_lazy_context_rec(contextp, stbl, *nodep);
    }
    return res;
}

g_ptr
Add_non_lazy_context_and_find_Userdefs(g_ptr node, symbol_tbl_ptr stbl)
{
    buffer context;
    new_buf (&context, 100, sizeof(string));
    create_hash(&bound_var_tbl, 100, str_hash, str_equ);
    node = top_add_non_lazy_context(&context, stbl, &node);
    dispose_hash(&bound_var_tbl, NULLFCN);
    free_buf (&context); 
    return(node);
}

bool
Make_non_lazy(string name, symbol_tbl_ptr stbl)
{
    fn_ptr fp = Find_Function_Def(stbl, name);
    if( fp == NULL )  { return FALSE; }
    fp->non_lazy = TRUE;
    return( TRUE );
}

symbol_tbl_ptr 
Merge_fn_defs(symbol_tbl_ptr stbl1, symbol_tbl_ptr stbl2, bool permissive)
{
    if( stbl2 == NULL || stbl2->def_list == NULL ) { return( stbl1 ); }
    fn_ptr fp = stbl1->def_list;
    while( fp->next != NULL ) { fp = fp->next; }
    fp->next = stbl2->def_list;
    fp = fp->next;
    while( fp != NULL ) {
        if( !permissive && already_defined(stbl1, fp) ) {
            Rprintf("Duplicated type constructor (%s)", fp->name);
        }
        if( fp->visible ) update_stbl(stbl1, fp);
        fp = fp->next;
    }
    free_rec(&hash_table_mgr, (pointer) stbl2->tbl_ptr);
    free_rec(&symb_tbl_mgr, (pointer) stbl2);
    return( stbl1 );
}

symbol_tbl_ptr
Add_Destructors(string name, typeExp_ptr new_type,
                symbol_tbl_ptr stbl, string file, int start_line)
{
    fn_ptr      fnp, newfnp, tmp;
    string      dest_name;

    tstr_ptr tmp_strings = new_temp_str_mgr();
    gen_strtemp(tmp_strings, name);
    gen_strappend(tmp_strings, "= ");

    newfnp = NULL;
    char sep = ' ';
    fn_ptr fnl = stbl->def_list;
    for(fnp = fnl; fnp != NULL; fnp = fnp->next) {
        // Make type "signature"
        gen_charappend(tmp_strings, sep);
        sep = '|';
        gen_strappend(tmp_strings, " ");
        gen_strappend(tmp_strings, fnp->name);
        gen_strappend(tmp_strings, "::");
        gen_strappend(tmp_strings, Get_tmp_type_string(fnp->type));
        gen_strappend(tmp_strings, " ");
        // Create destructor function
        dest_name          = strtemp("__DeStRuCtOr");
        dest_name          = strappend(fnp->name);
        dest_name          = wastrsave(&strings, dest_name);
        tmp                = (fn_ptr) get_fn_rec();
        tmp->ADT_level     = ADT_level;
        tmp->visible       = TRUE;
	tmp->in_use        = TRUE;
        tmp->non_lazy      = FALSE;
        tmp->forward       = FALSE;
        tmp->name          = dest_name;
        tmp->file_name      = file;
        tmp->start_line_nbr = start_line;
        tmp->end_line_nbr   = line_nbr;
        tmp->expr          = Make_0inp_Primitive(P_I);
	tmp->signature	   = Get_SHA256_signature(tmp->expr);
        tmp->expr_init     = cephalopode_mode? Cephalopde_Reflect_expr(tmp->expr) : NULL;
        tmp->expr_comb     = cephalopode_mode? Cephalopde_Reflect_expr(tmp->expr) : NULL;
        tmp->super_comb    = Make_0inp_Primitive(P_I);
        tmp->type          = Fix_Types(&(fnp->type), new_type);
        tmp->overload      = FALSE;
        tmp->open_overload = FALSE;
        tmp->overload_list = NULL;
        tmp->implicit_args = NULL;
        tmp->comments      = NULL;
        tmp->arg_names     = NULL;
        tmp->next          = newfnp;
        newfnp             = tmp;
        insert_hash(&Constructor_Table, (pointer) (fnp->name),
                                        (pointer) (fnp->name));
    }
    string type_sig = wastrsave(&strings, gen_strappend(tmp_strings, ""));
    if( find_hash(&TypeSignatureTbl, (pointer) name) != NULL )
        delete_hash(&TypeSignatureTbl, (pointer) name);
    insert_hash(&TypeSignatureTbl, (pointer) name, type_sig);
    free_temp_str_mgr(tmp_strings);
    if( Is_monomorphic(new_type) ) {
        // Save function
        Sprintf(buf, "write_%s", name);
        string save_fun_name = wastrsave(&strings, buf);
        g_ptr save_expr = Make_1inp_Primitive(P_SAVE_GRAPH,
                                              Make_STRING_leaf(type_sig));
        typeExp_ptr save_type = GLmake_arrow(GLmake_string(),
                                         GLmake_arrow(new_type, GLmake_bool()));
        fn_ptr  save_fun;
        save_fun = (fn_ptr) get_fn_rec();
        save_fun->visible        = TRUE;
	save_fun->in_use         = TRUE;
        save_fun->ADT_level      = ADT_level;
        save_fun->non_lazy       = FALSE;
        save_fun->forward        = FALSE;
        save_fun->name           = save_fun_name;
        save_fun->file_name      = file;
        save_fun->start_line_nbr = start_line;
        save_fun->end_line_nbr   = line_nbr;
        save_fun->expr           = save_expr;
	save_fun->signature	 = Get_SHA256_signature(save_expr);
        save_fun->expr_init      = cephalopode_mode?
					Cephalopde_Reflect_expr(save_fun->expr) : NULL;
        save_fun->expr_comb      = cephalopode_mode?
					Cephalopde_Reflect_expr(save_fun->expr) : NULL;
        save_fun->super_comb     = save_expr;    // ????
        save_fun->type           = save_type;
        save_fun->overload           = FALSE;
        save_fun->open_overload  = FALSE;
        save_fun->overload_list  = NULL;
        save_fun->implicit_args  = NULL;
        save_fun->comments       = NULL;
        save_fun->arg_names      = NULL;
        save_fun->next           = newfnp;
        newfnp               = save_fun;
        // Load function
        Sprintf(buf, "read_%s", name);
        string load_fun_name = wastrsave(&strings, buf);
        g_ptr load_expr = Make_1inp_Primitive(P_LOAD_GRAPH,
                                              Make_STRING_leaf(type_sig));
        typeExp_ptr load_type = GLmake_arrow(GLmake_string(), new_type);
        fn_ptr  load_fun;
        load_fun = (fn_ptr) get_fn_rec();
        load_fun->ADT_level      = ADT_level;
        load_fun->visible        = TRUE;
	load_fun->in_use         = TRUE;
        load_fun->non_lazy       = FALSE;
        load_fun->forward        = FALSE;
        load_fun->name           = load_fun_name;
        load_fun->file_name      = file;
        load_fun->start_line_nbr = start_line;
        load_fun->end_line_nbr   = line_nbr;
        load_fun->expr           = load_expr;
	load_fun->signature	 = Get_SHA256_signature(load_expr);
        load_fun->expr_init      = cephalopode_mode?
					Cephalopde_Reflect_expr(load_fun->expr) : NULL;
        load_fun->expr_comb      = cephalopode_mode?
					Cephalopde_Reflect_expr(load_fun->expr) : NULL;
        load_fun->super_comb     = load_expr;    // ????
        load_fun->type           = load_type;
        load_fun->overload           = FALSE;
        load_fun->open_overload  = FALSE;
        load_fun->overload_list  = NULL;
        load_fun->implicit_args  = NULL;
        load_fun->next           = newfnp;
        load_fun->comments       = NULL;
        load_fun->arg_names      = NULL;
        newfnp               = load_fun;
    }
    //
    while( fnl->next != NULL ) { fnl = fnl->next; }
    fnl->next = newfnp;
    while( newfnp != NULL ) {
        update_stbl(stbl, newfnp);
        newfnp = newfnp->next;
    }
    return(stbl);
}

oll_ptr
Add_To_OverloadList(string name, typeExp_ptr type, oll_ptr l,
                    string file, int start_line)
{
    fn_ptr  fn;
    fn = Find_Function_Def(symb_tbl, name);
    if( fn == NULL || !(fn->visible) ) {
        FP(err_fp, "=== Function %s is not defined ", name);
        if( file_load )     
            FP(err_fp, "around line %d in file %s\n", start_line, file);
        else
            FP(err_fp, "around line %d\n", start_line);
        Emit_prompt("");
        longjmp(*start_envp, 1);
    }
    fn = Find_Overload_Choice(fn, type);
    if( fn == NULL ) {
        FP(err_fp, "=== No function %s of type %s\n", name,
						      Type2String(type, TRUE));
        if( file_load )     
            FP(err_fp, "around line %d in file %s\n", start_line, file);
        else
            FP(err_fp, "around line %d\n", start_line);
        Emit_prompt("");
        longjmp(*start_envp, 1);
    }
    oll_ptr ret;
    ret = (oll_ptr) new_rec(&oll_rec_mgr);
    ret->fn = fn;
    ret->next = l;
    return ret;
}


symbol_tbl_ptr
InsertOverloadDef(string name, bool open_overload, oll_ptr alts,
                  typeExp_ptr type, symbol_tbl_ptr stbl,
                  string file, int start_line)
{
    fn_ptr      ret;
    ret                 = (fn_ptr) get_fn_rec();
    ret->ADT_level      = ADT_level;
    ret->name           = name;
    ret->file_name      = file;
    ret->start_line_nbr = start_line;
    ret->end_line_nbr   = line_nbr;
    ret->overload       = TRUE;
    ret->open_overload  = open_overload;
    ret->overload_list  = alts;
    ret->comments       = NULL;
    ret->arg_names      = NULL;
    ret->visible        = TRUE;
    ret->in_use         = TRUE;
    ret->non_lazy       = FALSE;
    ret->forward        = FALSE;
    ret->expr           = NULL;
    ret->signature      = NULL;
    ret->expr_init      = NULL;
    ret->expr_comb      = NULL;
    ret->type           = Get_common_type(type, alts);
    ret->super_comb     = NULL;
    ret->implicit_args  = NULL;
    ret->next           = stbl->def_list;
    stbl->def_list      = ret;
    update_stbl(stbl, ret);
    return( stbl );
}

symbol_tbl_ptr
AddToOpenOverloadDef(string name, oll_ptr alts, symbol_tbl_ptr stbl,
                     string file, int start_line)
{
    (void) file;
    (void) start_line;
    // Find the open_overload to which alts should be added
    fn_ptr cur = Find_Function_Def(stbl, name);
    if( cur == NULL || cur->open_overload == FALSE ) {
        FP(err_fp, "%s was not declared as an open_overload\n", name);
        return stbl;
    }
    oll_ptr fp = cur->overload_list;
    if( fp == NULL ) {
        cur->overload_list = alts;
        return stbl;
    }
    // We probably should check for overlapping overloads....
    while( fp->next != NULL ) { fp = fp->next; }
    fp->next = alts;
    return stbl;
}

symbol_tbl_ptr
Make_forward_declare(string name, typeExp_ptr type, symbol_tbl_ptr stbl,
                     string file, int start_line)
{
    g_ptr err = Make_0inp_Primitive(P_FAIL);
    Fail_pr("%s is forward_declared but used before it is defined\n", name);
    SET_FAIL_STRING(err, wastrsave(&strings, FailBuf));
    int ref_var = Make_RefVar();
    Set_RefVar(ref_var, err);
    g_ptr rv = Make_0inp_Primitive(P_REF_VAR);
    SET_REF_VAR(rv, ref_var);
    g_ptr expr = Make_1inp_Primitive(P_DEREFERENCE, rv);
    fn_ptr ret = (fn_ptr) get_fn_rec();
    ret->ADT_level      = ADT_level;
    ret->forward        = TRUE;
    ret->visible        = TRUE;
    ret->in_use         = TRUE;
    ret->file_name      = file;
    ret->start_line_nbr = start_line;
    ret->end_line_nbr   = line_nbr;
    ret->comments       = NULL;
    ret->arg_names      = NULL;
    ret->non_lazy       = FALSE;
    ret->name           = name;
    ret->expr           = expr;
    ret->signature	= Get_SHA256_signature(expr);
    ret->expr_init      = cephalopode_mode? Cephalopde_Reflect_expr(ret->expr) : NULL;
    ret->expr_comb      = cephalopode_mode? Cephalopde_Reflect_expr(ret->expr) : NULL;
    ret->super_comb     = err;
    ret->overload       = FALSE;
    ret->open_overload  = FALSE;
    ret->overload_list  = NULL;
    ret->type           = type;
    ret->implicit_args  = NULL;
    ret->next           = stbl->def_list;
    stbl->def_list	= ret;
    update_stbl(stbl, ret);
    // Prepare to pick up new comments
    cur_doc_comments = NULL;
    return stbl;
}

symbol_tbl_ptr
New_fn_def(string name, result_ptr res, symbol_tbl_ptr stbl, bool print,
           string file, int start_line, arg_names_ptr arg_names)
{
    fn_ptr      ret;

    /* Don't store failed definitions */
    if( res == NULL )
        return( stbl );
    //
    if( stbl == NULL ) { stbl = create_empty_symb_tbl(); }
    if( cur_doc_comments != NULL ) {
        // If there are comments, reverse the list (to get it in right order)
        comment_list_ptr clp = cur_doc_comments;
        comment_list_ptr prev = NULL;
	// Ignore comments inside the function
	while( clp->next != NULL && clp->line > start_line )
	    clp = clp->next;
        while( clp->next != NULL ) {
            comment_list_ptr tmp = clp->next;
            clp->next = prev;
            prev = clp;
            clp = tmp;
        }
        clp->next = prev;
        cur_doc_comments = clp;
    }
    // Is this the definition of a forward_declare?
    fn_ptr cur = Find_Function_Def(stbl, name);
    if( cur && cur->forward ) {
        if( !Forward_Declare_Ok(cur->type, res->type) ) {
            ErrMsg("-E- Type mismatch for forward_declare of %s\n",
                   name);
            FP(err_fp, "Declared type: ");
            Print_Type(cur->type, err_fp, TRUE, TRUE);
            FP(err_fp, "Actual type:   ");
            Print_Type(res->type, err_fp, TRUE, FALSE);
            return stbl;
        }
        g_ptr expr = cur->expr;
        if( !IS_APPLY(expr) ) {
            ErrMsg("-E- forward_declare evaluated before defined");
            return stbl;
        }
        expr = GET_APPLY_RIGHT(expr);
        ASSERT(IS_REF_VAR(expr));
        int ref_var = GET_REF_VAR(expr);
        Set_RefVar(ref_var, res->expr);
        ret = cur;
    } else {
        ret = (fn_ptr) get_fn_rec();
        ret->ADT_level = ADT_level;
        ret->name      = name;
        ret->next      = stbl->def_list;
        stbl->def_list = ret;
        update_stbl(stbl, ret);
    }
    //
    ret->visible            = TRUE;
    ret->in_use             = TRUE;
    ret->file_name          = file;
    ret->start_line_nbr     = start_line;
    ret->end_line_nbr       = line_nbr;
    ret->comments           = cur_doc_comments;
    // Handle argument names and default values (if any)
    ret->arg_names          = arg_names;
    ret->has_default_values = FALSE;
    while(arg_names != NULL) {
	if( arg_names->default_value != NULL ) {
	    ret->has_default_values = TRUE;
	    break;
	}
	arg_names = arg_names->next;
    }
    // Prepare to pick up new comments
    cur_doc_comments = NULL;
    ret->non_lazy           = FALSE;
    ret->forward            = FALSE;
    ret->name               = name;
    ret->expr_init          = res->expr_init;
    ret->expr_comb          = res->expr_comb;
    ret->signature	    = res->signature;
    ret->expr               = res->expr;
    ret->super_comb         = res->super_comb;
    ret->overload           = FALSE;
    ret->open_overload      = FALSE;
    ret->overload_list      = NULL;
    ret->type               = res->type;
    ret->implicit_args      = res->implicit_args;
    if( print ) {
        FP(stdout_fp, "%s::", name);
        Print_Full_Type(ret, stdout_fp, TRUE, TRUE);
    }
    return stbl;
}

void
Begin_ADT(symbol_tbl_ptr stbl)
{
    push_buf(&ADT_buf, (pointer) &(stbl->def_list));
    ADT_level++;
}

symbol_tbl_ptr
End_ADT(symbol_tbl_ptr stbl, var_list_ptr vlp)
{
    fn_ptr       last;
    var_list_ptr vp;
    if( empty_buf(&ADT_buf) ) {
        FP(err_fp,
                "Syntax error: end_abstype without matching begin_abstype\n");
        while( vlp != NULL ) {
            vp = vlp;
            vlp = vlp->next;
            free_rec(&var_list_rec_mgr, (pointer) vp);
        }
        ADT_level = 1;
        return stbl;
    }
    ADT_level--;
    pop_buf(&ADT_buf, (pointer) &last);
    hash_record keep_tbl;
    create_hash(&keep_tbl, 100, str_hash, str_equ);
    while( vlp != NULL ) {
        vp = vlp;
	if( find_hash(stbl->tbl_ptr, vlp->name) == NULL ) {
	    FP(err_fp,
	     "Error: end_abstype exports %s which is not defined\n", vlp->name);
	    while( vlp != NULL ) {
		vp = vlp;
		vlp = vlp->next;
		free_rec(&var_list_rec_mgr, (pointer) vp);
	    }
	    ADT_level = 1;
	    return stbl;
	} else {
	    if( find_hash(&keep_tbl, (pointer) vp->name) == NULL ) {
		insert_hash(&keep_tbl, (pointer) vp->name, (pointer) vp->name);
	    }
	}
        vlp = vp->next;
        free_rec(&var_list_rec_mgr, (pointer) vp);
    }
    dispose_hash(stbl->tbl_ptr, NULLFCN);
    create_hash(stbl->tbl_ptr, 100, str_hash, str_equ);
    fn_ptr flp = stbl->def_list;

    while( flp != last ) {
        if( find_hash(&keep_tbl, (pointer) flp->name) != NULL ) {
            flp->visible = TRUE;
            flp->ADT_level = ADT_level;
            insert_hash(stbl->tbl_ptr, (pointer) flp->name, (pointer) flp);
	    // Only keep the last definition of a function inside ADT
	    delete_hash(&keep_tbl, (pointer) flp->name);
        } else {
	    flp->visible = FALSE;
	}
        /* Type constructor status is bounded inside ADT */
        if( find_hash( &Constructor_Table, (pointer) flp->name ) != NULL )
            delete_hash(&Constructor_Table, (pointer) flp->name);
        flp = flp->next;
    }
    dispose_hash(&keep_tbl, NULLFCN);

    // Now add all the non-hidden visble functions back into the hash symb. tbl.
    while( flp != NULL ) {
	if( flp->visible ) {
	    string name = flp->name;
	    if( find_hash(stbl->tbl_ptr, (pointer) name) == NULL) {
                insert_hash(stbl->tbl_ptr, (pointer) name, (pointer) flp);
	    }
	}
	flp = flp->next;
    }
    return(stbl);
}

fn_ptr
Find_Function_Def(symbol_tbl_ptr stbl, string name)
{
    ASSERT( stbl->tbl_ptr != NULL );
    return( (fn_ptr) find_hash(stbl->tbl_ptr, name) );
}

g_ptr
Find_Function(symbol_tbl_ptr stbl, g_ptr node)
{
    ASSERT( IS_LEAF(node) && IS_VAR(node) );
    fn_ptr fp = Find_Function_Def(stbl, GET_VAR(node));
    if( fp == NULL ) return NULL;
    if( fp->overload ) {
        int cnt;
        oll_ptr up = fp->overload_list;
        cnt = GET_VERSION(node);
        if( cnt == 0 ) { return NULL; }
        while( cnt > 1 ) {
            cnt--;
            up = up->next;
        }
        INC_REFCNT(up->fn->expr);
        return(up->fn->expr);
    } else {
        INC_REFCNT(fp->expr);
        return(fp->expr);
    }
}

g_ptr
Replace_name(g_ptr node, symbol_tbl_ptr stbl)
{
    if( node == NULL )
        return(node);
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
            SET_APPLY_LEFT(node, Replace_name(GET_APPLY_LEFT(node), stbl));
            SET_APPLY_RIGHT(node, Replace_name(GET_APPLY_RIGHT(node), stbl));
            return(node);
        case LEAF:
            if( IS_USERDEF(node) ) {
                fn_ptr fn = GET_USERDEF(node);
                g_ptr res = fn->expr;
                SET_REFCNT(res, MAX_REF_CNT);
                return( res );
            }
            /* Fall through */
        default:
            return( node );
        case LAMBDA_ND:
            DIE("Lambdas should have been compiled already\n");
    }
    DIE("Should never occur!"); return NULL; // Dummy
}

string
Get_userdef_name(g_ptr node)
{
    fn_ptr fn = GET_USERDEF(node);
    return( fn->name );
}


static string
do_special_help(string name)
{
    string res = NULL;
    if( strcmp(name, "DIR") == 0 ) res = DIR_help;
    if( strcmp(name, "eprintf") == 0 ) res = eprintf_help;
    if( strcmp(name, "fprintf") == 0 ) res = fprintf_help;
    if( strcmp(name, "printf") == 0 ) res = printf_help;
    if( strcmp(name, "sprintf") == 0 ) res = sprintf_help;
    if( strcmp(name, "sscanf") == 0 ) res = sscanf_help;
    if( res == NULL ) return NULL;
    return( wastrsave(&strings, res) );
}


string
Get_Help(string fun)
{
    fn_ptr fp = Find_Function_Def(symb_tbl, fun);
    if( fp == NULL ) {
        return( do_special_help(fun) );
    }
    if( (odests_fp = fmemopen(help_buf, 4096, "w")) == NULL ) {
        DIE("Should never happen");
    }
    FP(FILE_fp, "Function: %s\n\n", fun);
    if( fp->file_name != NULL && !built_in(fp->file_name) ) {
        FP(FILE_fp, "File:  %s\nStart: %d\nEnd:   %d\n\n",
                    fp->file_name, fp->start_line_nbr, fp->end_line_nbr);
    } else {
        FP(FILE_fp, "Built-in\n\n");
    }
    if( fp->overload ) {
        FP(FILE_fp, "is the overloading of:\n");
        oll_ptr ofn = fp->overload_list;
        while( ofn != NULL ) {
            FP(FILE_fp, "%s :: ", ofn->fn->name);
            Print_Full_Type(ofn->fn, FILE_fp, TRUE, TRUE);
            ofn = ofn->next;
        }
        FP(FILE_fp, "\nFixity: %s\n", Get_Fixity(fun));
        if( fp->non_lazy ) FP(FILE_fp, "Non-lazy: Yes\n");
    } else {
        typeExp_ptr type = fp->type;
        if( fp->implicit_args ) {
            FP(FILE_fp, "Implicit dependencies:");
            impl_arg_ptr np = fp->implicit_args;
            hash_record occurences;
            create_hash(&occurences, 100, str_hash, str_equ);
            for(impl_arg_ptr ia = np; ia != NULL; ia = ia->next) {
                int cur = 0;
                if( (cur = PTR2INT(find_hash(&occurences, ia->name))) != 0 ) {
                    delete_hash(&occurences, ia->name);
                }
                cur++;
                insert_hash(&occurences, ia->name, INT2PTR(cur));
            }
            int indent = strlen("Implicit dependencies:");
            int loc = indent;
            char sep = ' ';
            while( np != NULL ) {
                string name = np->name;
                int arity;
                if( (arity = PTR2INT(find_hash(&occurences, name))) != 0 ) {
                    int len = strlen(np->name)+1;
                    if( (loc+len) > 70 ) {
                        FP(FILE_fp, "%c\n", sep);
                        sep = ' ';
                        FP(FILE_fp, "%*s", indent, " ");
                        loc = indent;
                    }
                    if( arity > 1 ) {
                        FP(FILE_fp, "%c%s(%d)", sep, np->name, arity);
                        len += 3;
                    } else {
                        FP(FILE_fp, "%c%s", sep, np->name);
                    }
                    delete_hash(&occurences, name);
                    loc += len;
                    sep = ',';
                }
                type = Get_Real_Type(type->typelist->next->type);
                np = np->next;
            }
            FP(FILE_fp, "\n");
        }
        int arg = 1;
        arg_names_ptr ap = fp->arg_names;
        int sz = 1;
        while(ap != NULL ) {
            int l = strlen(ap->name);
            if( l > sz ) sz = l;
            ap = ap->next;
        }
        ap = fp->arg_names;
        FP(FILE_fp, "Arguments:\n");
        if( ap != NULL ) { sz += 2; }
        while( type->typeOp == arrow_tp ) {
            if( ap != NULL ) {
                FP(FILE_fp, "%*s: ", sz, ap->name);
                ap = ap->next;
            } else {
                FP(FILE_fp, "  arg. %d: ", arg);
            }
            typeExp_ptr arg_type = type->typelist->type;
            Print_Type(arg_type, FILE_fp, TRUE, (arg == 1));
            type = Get_Real_Type(type->typelist->next->type);
            arg++;
        }
        FP(FILE_fp, "\nReturn type: ");
        Print_Type(type, FILE_fp, TRUE, (arg == 1));
        FP(FILE_fp, "\nFixity: %s\n", Get_Fixity(fun));
        if( fp->non_lazy ) FP(FILE_fp, "Non-lazy: Yes\n");
        FP(FILE_fp, "\n");
    }
    comment_list_ptr clp = fp->comments;
    if( clp != NULL ) 
        FP(FILE_fp, "Description:\n\n");
    while( clp != NULL ) {
        if( clp->comment != NULL )
            FP(FILE_fp, "%s\n", clp->comment);
        clp = clp->next;
    }
    fclose(odests_fp);
    odests_fp = NULL;
    return( wastrsave(&strings, help_buf) );
}

static formula
default_eq_fn(pointer p1, pointer p2, bool identical)
{
    (void) identical;
    if( p1 == p2 ) {
        return( B_One() );
    } else {
        return( B_Zero() );
    }
}

int
Add_ExtAPI_Object(
            string name,
            void    (*mark_fn)(pointer p),
            void    (*sweep_fn)(),
            void    (*save_fn)(FILE *fp, pointer p),
            pointer (*load_fn)(FILE *fp),
            string  (*obj2string)(pointer p),
            formula (*eq_fn)(pointer a, pointer b, bool identical),
            pointer (*gmap_fn)(gmap_info_ptr ip, pointer a),
            pointer (*gmap2_fn)(gmap_info_ptr ip, pointer a, pointer b),
	    int	    (*sha256_fn)(int *g_cntp, hash_record *g_tblp,
				 SHA256_ptr sha, pointer obj) )
{
    name = wastrsave(&strings, name);
    ext_obj_rec obj;
    obj.name = name;
    obj.class = COUNT_BUF(&ext_obj_buf);
    obj.type = Get_Type(name, NULL, TP_INSERT_FULL_TYPE);
    obj.mark_fn = mark_fn;
    obj.sweep_fn = sweep_fn;
    obj.save_fn = save_fn;
    obj.load_fn = load_fn;
    obj.obj2string = obj2string;
    if( eq_fn == NULL ) {
        obj.eq_fn = default_eq_fn;
    } else {
        obj.eq_fn = eq_fn;
    }
    obj.gmap_fn = gmap_fn;
    obj.gmap2_fn = gmap2_fn;
    obj.sha256_fn = sha256_fn;
    push_buf(&ext_obj_buf, (pointer) &obj);
    return(obj.class);
}

string
Get_ExtAPI_Object_name(int class)
{
    ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
    return(op->name);
}

g_ptr
Make_ext_obj(int class, pointer p)
{
    g_ptr root;         
    root = Get_node();  
    SET_TYPE(root, LEAF);
    SET_LEAF_TYPE(root, EXT_OBJ);
    SET_EXT_OBJ_CLASS(root, class);
    SET_EXT_OBJ(root, p);
    return( root );
}

void
Mark_ext_obj(g_ptr np)
{
    ASSERT(IS_EXT_OBJ(np));
    int class = GET_EXT_OBJ_CLASS(np);
    ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
    if( op->mark_fn != NULL ) {
        op->mark_fn(GET_EXT_OBJ(np));
    }
}

void
Save_ext_obj(FILE *fp, g_ptr np)
{
    ASSERT(IS_EXT_OBJ(np));
    int class = GET_EXT_OBJ_CLASS(np);
    ext_obj_ptr op = M_LOCATE_BUF(&ext_obj_buf, class);
    if( op->save_fn != NULL ) {
        op->save_fn(fp, GET_EXT_OBJ(np));
    }
}

void
Sweep_ext_objs()
{
    ext_obj_ptr op;
    FOR_BUF(&ext_obj_buf, ext_obj_rec, op) {
        if( op->sweep_fn != NULL ) {
            op->sweep_fn();
        }
    }
}


void
Add_ExtAPI_Function(string name, string strictness,
                    bool non_lazy, typeExp_ptr type, eval_fun_tp fun)
{
    name = wastrsave(&strings, name);
    ext_fun_rec efr;
    efr.name = name;
    efr.arg_cnt = strlen(strictness);
    efr.strict_args = wastrsave(&strings, strictness);
    efr.type = type;
    efr.fun = fun;
    push_buf(&ext_fun_buf, (pointer) &efr);
    int id = COUNT_BUF(&ext_fun_buf)-1;

    g_ptr expr = Get_node();
    SET_TYPE(expr, LEAF);
    SET_LEAF_TYPE(expr,PRIM_FN);
    SET_PRIM_FN(expr, P_EXTAPI_FN);
    SET_EXTAPI_FN(expr, id);
    fn_ptr ret = (fn_ptr) get_fn_rec();
    ret->ADT_level      = ADT_level;
    ret->visible        = TRUE;
    ret->in_use         = TRUE;
    ret->file_name      = wastrsave(&strings, "builtin");
    ret->start_line_nbr = 0;
    ret->end_line_nbr   = 0;
    ret->comments       = NULL;
    ret->arg_names      = NULL;
    ret->non_lazy       = non_lazy;
    ret->forward        = FALSE;
    ret->name           = name;
    ret->expr           = expr;
    ret->signature	= Get_SHA256_signature(expr);
    ret->expr_init      = cephalopode_mode? Cephalopde_Reflect_expr(ret->expr) : NULL;
    ret->expr_comb      = cephalopode_mode? Cephalopde_Reflect_expr(ret->expr) : NULL;
    ret->super_comb     = NULL;
    ret->overload       = FALSE;
    ret->open_overload  = FALSE;
    ret->overload_list  = NULL;
    ret->type           = type;
    ret->implicit_args  = NULL;
    ret->next           = symb_tbl->def_list;
    symb_tbl->def_list  = ret;
    update_stbl(symb_tbl, ret);
}


typeExp_ptr
Get_ExtAPI_Type(int id)
{
    ext_fun_ptr efp = (ext_fun_ptr) M_LOCATE_BUF(&ext_fun_buf, id);
    return( efp->type );
}

int
Get_ExtAPI_ArgCnt(int id)
{
    ext_fun_ptr efp = (ext_fun_ptr) M_LOCATE_BUF(&ext_fun_buf, id);
    return( efp->arg_cnt );
}

string
Get_ExtAPI_Strictness(int id)
{
    ext_fun_ptr efp = (ext_fun_ptr) M_LOCATE_BUF(&ext_fun_buf, id);
    return( efp->strict_args );
}

eval_fun_tp
Get_ExtAPI_Function(int id)
{
    ext_fun_ptr efp = (ext_fun_ptr) M_LOCATE_BUF(&ext_fun_buf, id);
    return( efp->fun );
}

string
Get_ExtAPI_Function_Name(int id)
{
    ext_fun_ptr efp = (ext_fun_ptr) M_LOCATE_BUF(&ext_fun_buf, id);
    return( efp->name );
}

bool
Check_No_Undefined_Types(symbol_tbl_ptr clist)
{
    fn_ptr cur = clist->def_list;
    while( cur != NULL ) {
        string name = cur->name;
        if( strncmp("__DeStRuCtOr", name, strlen("__DeStRuCtOr")) != 0 ) {
            if( !Check_no_undef_types(cur->type) )
                return FALSE;
        }
        cur = cur->next;
    }
    if( !file_load ) {
        cur = clist->def_list;
        while( cur != NULL ) {
            string name = cur->name;
            if( strncmp("__DeStRuCtOr", name, strlen("__DeStRuCtOr")) != 0 ) {
                FP(stdout_fp, "%s::", name);
                Print_Type(cur->type, stdout_fp, TRUE, TRUE);
            }
            cur = cur->next;
        }
    }
    return TRUE;
}

string
Get_tmp_type_string(typeExp_ptr type)
{
    if( (odests_fp = fmemopen(type_buf, 4096, "w")) == NULL ) {
        DIE("Should never happen");
    }
    Print_Type(type, FILE_fp, FALSE, TRUE);
    fclose(odests_fp);
    odests_fp = NULL;
    return( type_buf );
}

#ifdef DEBUG
void
PrintAllFuns(symbol_tbl_ptr stbl)
{
    fn_ptr fp = stbl->def_list;
    while( fp != NULL ) {
        fprintf(stderr, "%s (%s)\n", fp->name, (fp->visible?"Vis":"Invis"));
        fp = fp->next;
    }
}

#endif

/********************************************************/
/*          EXPORTED EXTAPI FUNCTIONS                   */
/********************************************************/

static void
get_args(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(r);
    fn_ptr fp = Find_Function_Def(symb_tbl, name);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find function %s", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->overload ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s is overloaded", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->implicit_args ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s has implicit arguments", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    typeExp_ptr type = fp->type;
    arg_names_ptr ap = fp->arg_names;
    bool reset = TRUE;
    while( type->typeOp == arrow_tp ) {
	typeExp_ptr arg_type = type->typelist->type;
	if( ap != NULL ) {
	    g_ptr p = Make_PAIR_ND(
			    Make_STRING_leaf(ap->name),
			    Make_STRING_leaf(Type2String(arg_type, reset)));
	    reset = FALSE;
	    APPEND1(tail,p);
	    ap = ap->next;
	} else {
	    MAKE_REDEX_FAILURE(redex, Fail_pr("%s has unnamed argument", name));
	    DEC_REF_CNT(l);
	    DEC_REF_CNT(r);
	    return;
	}
	type = Get_Real_Type(type->typelist->next->type);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_arity(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(r);
    fn_ptr fp = Find_Function_Def(symb_tbl, name);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find function %s", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->overload ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s is overloaded", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->implicit_args ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s has implicit arguments", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    typeExp_ptr type = fp->type;
    int cnt = 0;
    while( type->typeOp == arrow_tp ) {
	cnt++;
	type = Get_Real_Type(type->typelist->next->type);
    }
    MAKE_REDEX_INT(redex, cnt);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_return_type(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(r);
    fn_ptr fp = Find_Function_Def(symb_tbl, name);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find function %s", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->overload ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s is overloaded", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->implicit_args ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s has implicit arguments", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    typeExp_ptr type = fp->type;
    while( type->typeOp == arrow_tp ) {
	type = Get_Real_Type(type->typelist->next->type);
    }
    MAKE_REDEX_STRING(redex, Type2String(type, TRUE));
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_symbol_signature(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string name = GET_STRING(r);
    fn_ptr fp = Find_Function_Def(symb_tbl, name);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find function %s", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    if( fp->overload ) {
	MAKE_REDEX_FAILURE(redex,Fail_pr("%s is overloaded", name));
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    MAKE_REDEX_STRING(redex, fp->signature);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}



static void
is_defined(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    if( Find_Function_Def(symb_tbl, GET_STRING(r)) == NULL ) {
        MAKE_REDEX_BOOL(redex, B_Zero());
    } else {
        MAKE_REDEX_BOOL(redex, B_One());
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_definition(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    if( !cephalopode_mode ) {
	MAKE_REDEX_FAILURE(redex, "get_definition only available in -C mode");
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    string name = GET_STRING(r);
    fn_ptr fp = Find_Function_Def(symb_tbl, name);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find any function named '%s'",
					  name));
    } else {
	if( fp->overload ) {
	    MAKE_REDEX_FAILURE(redex,
			       Fail_pr("Function %s is overloaded", name));
	} else  {
	    OVERWRITE(redex, fp->expr_init);
	}
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_combinator_expression(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    if( !cephalopode_mode ) {
	MAKE_REDEX_FAILURE(redex,
			"get_combinator_expression only available in -C mode");
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    string name = GET_STRING(r);
    fn_ptr fp = Find_Function_Def(symb_tbl, name);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find any function named '%s'",
					  name));
    } else {
	if( fp->overload ) {
	    MAKE_REDEX_FAILURE(redex,
			       Fail_pr("Function %s is overloaded", name));
	} else  {
	    OVERWRITE(redex, fp->expr_comb);
	}
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_userdef_combinator_expression(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    if( !cephalopode_mode ) {
	MAKE_REDEX_FAILURE(redex,
		"get_userdef_combinator_expression only available in -C mode");
	DEC_REF_CNT(l);
	DEC_REF_CNT(r);
	return;
    }
    int idx = GET_INT(r);
    fn_ptr fp = (fn_ptr) REC_EL(&fn_rec_mgr, idx);
    if( fp == NULL ) {
	MAKE_REDEX_FAILURE(redex, Fail_pr("Cannot find USERDEF %d", idx));
    } else {
	MAKE_REDEX_PAIR(redex,Make_STRING_leaf(fp->name),
			      fp->expr_comb);
    }
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static void
get_fixity(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    string var = GET_STRING(r);
    string fixity = Get_Fixity(var);
    MAKE_REDEX_STRING(redex, fixity);
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}

static int
fn_cmp(const void *pi, const void *pj)
{
    string *i = (string *) pi;
    string *j = (string *) pj;
    return( strcmp(*i, *j) );
}


static void
get_matching_functions(g_ptr redex)
{
    g_ptr l = GET_APPLY_LEFT(redex);
    g_ptr r = GET_APPLY_RIGHT(redex);
    g_ptr g_name_pat, g_file_pat, g_arg_type_pat, g_res_type_pat, g_max_cnt;
    EXTRACT_5_ARGS(redex, g_name_pat, g_file_pat, g_arg_type_pat,
			  g_res_type_pat, g_max_cnt);
    string name_pat = GET_STRING(g_name_pat);
    string file_pat = GET_STRING(g_file_pat);
    string arg_type_pat = GET_STRING(g_arg_type_pat);
    string res_type_pat = GET_STRING(g_res_type_pat);
    int	   max_cnt = GET_INT(g_max_cnt);

    hash_record done;
    create_hash(&done, 100, str_hash, str_equ);
    fn_ptr list = symb_tbl->def_list;
    buffer  rbuf;
    new_buf(&rbuf, 100, sizeof(string));
    while( list != NULL ) {
        if( !list->visible )
            goto do_next;
	if( find_hash(symb_tbl->tbl_ptr, (pointer) list->name) == NULL )
            goto do_next;
        if( STREQ(s_dummy, list->name) )
            goto do_next;
        if( strncmp(list->name, "__DeStRuCtOr", 12) == 0 )
            goto do_next;
        if( !check_match(name_pat, list->name) )
            goto do_next;
        if( !check_match(file_pat, list->file_name) )
            goto do_next;
        if( check_type_match(arg_type_pat, res_type_pat, list->type) ) {
            if( find_hash(&done, list->name) == NULL ) {
		push_buf(&rbuf, (pointer) &(list->name));
                insert_hash(&done, list->name, list->name);
            }
            goto do_next;
        }
        if( list->overload_list != NULL ) {
            oll_ptr cur = list->overload_list;
            bool found = FALSE;
            while( !found && cur != NULL ) {
                if( cur->fn->visible &&
                    check_type_match(arg_type_pat, res_type_pat, cur->fn->type)
                  )
                {
                    found = TRUE;
                    if( find_hash(&done, list->name) == NULL ) {
			push_buf(&rbuf, (pointer) &(list->name));
                        insert_hash(&done, list->name, list->name);
                    }
                }
                cur = cur->next;
            }
        }
      do_next:
        list = list->next;
    }
    qsort(START_BUF(&rbuf), COUNT_BUF(&rbuf), sizeof(string), fn_cmp);
    MAKE_REDEX_NIL(redex);
    g_ptr tail = redex;
    string  *namep;
    FOR_BUF(&rbuf, string, namep) {
	if( max_cnt > 0 ) {
	    APPEND1(tail, Make_STRING_leaf(*namep));
	}
	max_cnt--;
    }
    if( max_cnt <= 0 ) {
	APPEND1(tail, Make_STRING_leaf(wastrsave(&strings, "...")));
    }
    free_buf (&rbuf); 
    DEC_REF_CNT(l);
    DEC_REF_CNT(r);
}


void
Symbols_Install_Functions()
{
    typeExp_ptr term = Get_Type("term", NULL, TP_INSERT_PLACE_HOLDER);

    Add_ExtAPI_Function("get_args", "1", TRUE,
                        GLmake_arrow(
			    GLmake_string(),
			    GLmake_list(GLmake_tuple(
					    GLmake_string(),
					    GLmake_string()))),
                        get_args);

    Add_ExtAPI_Function("get_arity", "1", TRUE,
                        GLmake_arrow(GLmake_string(), GLmake_int()),
                        get_arity);

    Add_ExtAPI_Function("get_return_type", "1", TRUE,
                        GLmake_arrow(GLmake_string(), GLmake_string()),
                        get_return_type);
    Add_ExtAPI_Function("get_symbol_signature", "1", TRUE,
                        GLmake_arrow(GLmake_string(), GLmake_string()),
                        get_symbol_signature);

    Add_ExtAPI_Function("get_definition", "1", TRUE,
                        GLmake_arrow(GLmake_string(), term),
                        get_definition);
    Add_ExtAPI_Function("get_combinator_expression", "1", TRUE,
                        GLmake_arrow(GLmake_string(), term),
                        get_combinator_expression);
    Add_ExtAPI_Function("get_userdef_combinator_expression", "1", TRUE,
                        GLmake_arrow(GLmake_int(),
				     GLmake_tuple(GLmake_string(), term)),
                        get_userdef_combinator_expression);

    Add_ExtAPI_Function("is_defined", "1", TRUE,
                        GLmake_arrow(GLmake_string(),GLmake_bool()),
                        is_defined);
    Add_ExtAPI_Function("get_fixity", "1", TRUE,
                        GLmake_arrow(GLmake_string(),GLmake_string()),
                        get_fixity);
    Add_ExtAPI_Function("get_matching_functions", "11111", TRUE,
                        GLmake_arrow(
			  GLmake_string(),
			  GLmake_arrow(
			    GLmake_string(),
			    GLmake_arrow(
			      GLmake_string(),
			      GLmake_arrow(
				GLmake_string(),
				GLmake_arrow(
				  GLmake_int(),
				  GLmake_list(GLmake_string())))))),
                        get_matching_functions);

}

arg_names_ptr
Get_argument_names(g_ptr onode)
{
    buffer args;
    name_expr_rec  *nep;
    arg_names_ptr res = NULL;
    g_ptr node = onode;
    if( node == NULL ) { return NULL; }
    node = ignore_P_Y(node);
    if( node == NULL ) { return NULL; }
    int arg_cnt = 0;
    while( IS_LAMBDA(node) ) {
        arg_cnt++;
        node = GET_LAMBDA_BODY(node);
    }
    if( arg_cnt == 0 ) {
	node = ignore_P_DEBUG(node);
	node = ignore_P_NAMED_ARG(node);
	new_buf(&args, 10, sizeof(name_expr_rec));
	name_expr_rec arg;
	g_ptr next;
	while( get_HFL_arg_name(node, &arg, &next) ) {
	    push_buf(&args, (pointer) &arg);
	    node = next;
	}
	if( COUNT_BUF(&args) == 0 ) {
            free_buf (&args); 
            return NULL;
	}
	goto make_arg_list;
    }
    if( node == NULL ) return NULL;
    node = ignore_P_NAMED_ARG(node);
    node = ignore_P_CACHE(node);
    node = ignore_P_STRICT_ARGS(node);
    node = ignore_P_DEBUG(node);
    if( node == NULL ) return NULL;
    node = ignore_simple_P_PCATCH(node);
    if( node == NULL ) return NULL;
    new_buf(&args, 10, sizeof(name_expr_rec));
    for(int i = 0; i < arg_cnt; i++) {
        name_expr_rec arg;
        g_ptr next;
        if( !get_named_arg(node, &arg, &next) ) {
            free_buf (&args); 
            return NULL;
        }
        push_buf(&args, (pointer) &arg);
        node = next;
    }
  make_arg_list:
    FUB_ROF(&args, name_expr_rec, nep) {
        arg_names_ptr t = (arg_names_ptr) new_rec(&arg_names_rec_mgr);
        t->name = nep->var;
	t->default_value = nep->expr;
        t->next = res;
        res = t;
    }
    free_buf (&args); 
    return res;
}


/************************************************************************/
/*                      Local Functions                                 */
/************************************************************************/

static symbol_tbl_ptr
create_empty_symb_tbl()
{
    symbol_tbl_ptr stbl = 
    stbl = (symbol_tbl_ptr) new_rec(&symb_tbl_mgr);
    stbl->def_list = NULL;
    hash_record_ptr tbl = (hash_record_ptr) new_rec(&hash_table_mgr);
    create_hash(tbl, 100, str_hash, str_equ);
    stbl->tbl_ptr = tbl;
    return stbl;
}

static bool
is_non_lazy(symbol_tbl_ptr stbl, string name)
{
    fn_ptr fp = Find_Function_Def(stbl, name);
    ASSERT( fp != NULL );
    return( fp->non_lazy );
}

static void
add_binding(string name)
{
    int cur = PTR2INT(find_hash(&bound_var_tbl, (pointer) name));
    if( cur == 0 ) {
        insert_hash(&bound_var_tbl, (pointer) name, INT2PTR(2));
    } else {
        cur++;
        delete_hash(&bound_var_tbl, (pointer) name);
        insert_hash(&bound_var_tbl, (pointer) name, INT2PTR(cur));
    }
}

static void
remove_binding(string name)
{
    int cur = PTR2INT(find_hash(&bound_var_tbl, (pointer) name));
    ASSERT( cur != 0 );
    cur--;
    delete_hash(&bound_var_tbl, (pointer) name);
    insert_hash(&bound_var_tbl, (pointer) name, INT2PTR(cur));
}

static bool
is_bound(string name)
{
    int cur = PTR2INT(find_hash(&bound_var_tbl, (pointer) name));
    return( cur > 1 );
}


static impl_arg_ptr
get_overloaded_calls_rec(impl_arg_ptr l, symbol_tbl_ptr stbl, g_ptr node)
{
    if( node == NULL ) { return l; }
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
            l = get_overloaded_calls_rec(l, stbl, GET_APPLY_LEFT(node));
            l = get_overloaded_calls_rec(l, stbl, GET_APPLY_RIGHT(node));
            return( l );
        case CONS_ND:
            l = get_overloaded_calls_rec(l, stbl, GET_CONS_HD(node));
            l = get_overloaded_calls_rec(l, stbl, GET_CONS_TL(node));
            return( l );
        case LAMBDA_ND:
            add_binding(GET_LAMBDA_VAR(node));
            l = get_overloaded_calls_rec(l, stbl, GET_LAMBDA_BODY(node));
            remove_binding(GET_LAMBDA_VAR(node));
            return( l );
        case LEAF:
            if( !IS_VAR(node) ) { return( l ); }
            string name = GET_VAR(node);
            if( is_bound(name) ) return( l );
            fn_ptr fp = Find_Function_Def(stbl, name);
            if( !fp->overload ) return( l );
            // Is the overloading resolved already?
            if( GET_VERSION(node) != 0 ) return( l );
            overloaded_calls++;
            string new_name = tprintf(".impl_arg.%d", overloaded_calls);
            new_name = wastrsave(&strings, new_name);
            SET_VAR(node, new_name);
            impl_arg_ptr np = (impl_arg_ptr) new_rec(&impl_arg_rec_mgr);
            np->name = name;
            np->type = fp->type;
            np->def = fp;
            np->next = l;
            return( np );
        default:
            DIE("Impossible");
    }
    return l;    /* Dummy */
}

static void
report_error_loc(g_ptr node, string type)
{
    if( file_load )
	FP(err_fp, "=== %s error around line %d in file %s\n", type,
		GET_LINE_NBR(node), cur_file_name);
    else
	FP(err_fp, "=== %s error around line %d\n", type, GET_LINE_NBR(node));
}

/* Add dummy dependencies on the lambda bound vars for non_lazy function    */
/* When a contiguous sequence of lambdas occurs, only use the last variable */ 
/* Also handle named arguments and default values			    */
/* %%%%%%%%%%%%%%%%%%%%%%%%% */
static g_ptr
add_non_lazy_context_rec(buffer *ctxt, symbol_tbl_ptr stbl, g_ptr node)
{
    g_ptr	    E, F, Fn, ctx_nd, res;
    string	    name, new_name, *sp;
    int		    line;
    buffer	    arg_info_buf;
    hash_record	    named_arg_tbl;
    arg_info_ptr    aip;
    if( node == NULL ) { return NULL; }
    switch( GET_TYPE(node) ) {
        case APPLY_ND:
	    new_buf(&arg_info_buf, 100, sizeof(arg_info_rec));
	    bool has_named_arg = FALSE;
	    while( GET_TYPE(node) == APPLY_ND ) {
		F = GET_APPLY_RIGHT(node);
		arg_info_rec	info;
		if( Destr_Named_Arg(F, &name, &Fn) ) {
		    has_named_arg = TRUE;
		    info.spine = node;
		    info.arg_name = name;
		    info.arg_expr = add_non_lazy_context_rec(ctxt, stbl, Fn);
		    INC_REFCNT(info.arg_expr);
		} else {
		    info.spine = node;
		    info.arg_name = s_empty;
		    info.arg_expr = add_non_lazy_context_rec(ctxt, stbl, F);
		}
		push_buf(&arg_info_buf, &info);
		node = GET_APPLY_LEFT(node);
	    }
	    if( GET_TYPE(node) != LEAF ) {
		node = add_non_lazy_context_rec(ctxt, stbl, node);
		goto wrap_up;
	    }
            if( !IS_VAR(node) ) {
		if( has_named_arg ) { goto err_named_arg; }
                if( !IS_PRIM_FN(node) ) goto wrap_up;
                int pfn = GET_PRIM_FN(node);
                if( (pfn != P_PRINTF) && (pfn != P_SPRINTF) &&
                    (pfn != P_EPRINTF) && (pfn != P_FPRINTF) &&
                    (pfn != P_PRINT) && (pfn != P_MK_REF_VAR) )
                {
                    goto wrap_up;
                }
                // Printf like functions are non-lazy
                if( COUNT_BUF(ctxt) == 0 ) { goto wrap_up; }
                ctx_nd = Make_NIL();
                FOR_BUF(ctxt, string, sp) {
                    ctx_nd =
                        Make_2inp_Primitive(P_CONS,
                            Make_1inp_Primitive(P_UNTYPE, Make_VAR_leaf(*sp)),
                            ctx_nd);
                }
		node = Make_2inp_Primitive(P_NSEQ, ctx_nd, node);
		goto wrap_up;
            }
            name = GET_VAR(node);
            if( is_bound(name) ) {
		if( has_named_arg ) { goto err_named_arg; }
                // Rename bound variables to ensure no name capture happens
                // when implicit arguments for overloaded identifiers are added
                string new_name = wastrsave(&strings, tprintf(".%s", name));
                SET_VAR(node, new_name);
		goto wrap_up;
            }
            fn_ptr fp = Find_Function_Def(stbl, name);
            if( fp == NULL ) {
		report_error_loc(node, "Type");
                FP(err_fp, "Unidentified identifier \"%s\"\n", GET_VAR(node));
		free_buf(&arg_info_buf);
                longjmp(context_env, 1);
            }
            // Add implicit arguments
            impl_arg_ptr np = fp->implicit_args;
            int v_line_nbr = GET_LINE_NBR(node);
            MAKE_REDEX_USERDEF(node, fp);
            SET_LINE_NBR(node, v_line_nbr);
            while( np != NULL ) {
                g_ptr ifn = Make_USERDEF_leaf(np->def);
                SET_LINE_NBR(ifn, v_line_nbr);
                node = Make_APPL_ND(node, ifn);
                np = np->next;
            }
	    // Add non-lazy dependencies if needed
	    if( COUNT_BUF(ctxt) != 0 && is_non_lazy(stbl, name) ) {
		ctx_nd = Make_NIL();
		FOR_BUF(ctxt, string, sp) {
		    ctx_nd = Make_2inp_Primitive(
				    P_CONS,
				    Make_1inp_Primitive(P_UNTYPE,
							Make_VAR_leaf(*sp)),
				    ctx_nd);
		}
		node = Make_2inp_Primitive(P_NSEQ, ctx_nd, node);
	    }
	    // Deal with named arguments
	    create_hash(&named_arg_tbl, COUNT_BUF(&arg_info_buf),
			Ustr_hash, Ustr_equ); 
	    arg_names_ptr ap = fp->arg_names;
	    if( has_named_arg ) {
		if( ap == NULL ) {
		    report_error_loc(node, "Syntax");
		    FP(err_fp, "Function %s has no named arguments\n", name);
		    free_buf(&arg_info_buf);
		    longjmp(context_env, 1);
		}
		FOR_BUF(&arg_info_buf, arg_info_rec, aip) {
		    if( !STREQ(aip->arg_name, s_empty) ) {
			if( find_hash(&named_arg_tbl, aip->arg_name) != NULL ) {
			    report_error_loc(node, "Syntax");
			    FP(err_fp,
			       "Named argument %s occurs more than once\n",
			       aip->arg_name);
			    free_buf(&arg_info_buf);
			    dispose_hash(&named_arg_tbl, NULLFCN);
			    longjmp(context_env, 1);
			}
			insert_hash(&named_arg_tbl,aip->arg_name,aip->arg_expr);
		    }
		}
	    }
	    // Now process positional arguments
	    bool all_done = TRUE;
	    FUB_ROF(&arg_info_buf, arg_info_rec, aip) {
		if( STREQ(aip->arg_name, s_empty) ) {
		    node = cond_mk_app(aip->spine, node, aip->arg_expr);
		    ap = (ap != NULL)? ap->next : NULL;
		} else {
		    all_done = FALSE;
		    break;
		}
	    }
	    if( all_done && !has_named_arg & !fp->has_default_values ) {
		free_buf(&arg_info_buf);
		dispose_hash(&named_arg_tbl, NULLFCN);
		return( node );
	    }
	    // Now process named arguments and defaults
	    while( ap != NULL ) {
		F = find_hash(&named_arg_tbl, ap->name);
		if( F == NULL ) {
		    F = ap->default_value;
		}
		if( F == NULL ) {
		    report_error_loc(node, "Syntax");
		    FP(err_fp,"%s is not given value for argument %s\n",
			      name, ap->name);
		    free_buf(&arg_info_buf);
		    dispose_hash(&named_arg_tbl, NULLFCN);
		    longjmp(context_env, 1);
		}
		F = add_non_lazy_context_rec(ctxt, stbl, F);
		node = Make_APPL_ND(node, F);
		ap = (ap != NULL)? ap->next : NULL;
	    }
	    free_buf(&arg_info_buf);
	    dispose_hash(&named_arg_tbl, NULLFCN);
	    return( node );
        case CONS_ND:
            E = add_non_lazy_context_rec(ctxt, stbl, GET_CONS_HD(node));
            F = add_non_lazy_context_rec(ctxt, stbl, GET_CONS_TL(node));
            return( cond_mk_cons(node, E, F) );
        case LAMBDA_ND:
            name = GET_LAMBDA_VAR(node);
            // Rename lambda variables to ensure no name capture happens
            // when implicit arguments for overloaded identifiers are added
            new_name = wastrsave(&strings, tprintf(".%s", name));
            line = GET_LAMBDA_LINE_NBR(node);
            add_binding(name);
	    bool innermost = !IS_LAMBDA(GET_LAMBDA_BODY(node));
	    if( innermost ) { push_buf(ctxt, &new_name); }
	    g_ptr body = GET_LAMBDA_BODY(node);
	    g_ptr En, var, def_val;
	    if( is_P_NAMED_ARG(body, &En, &var, &def_val) ) {
		E = add_non_lazy_context_rec(ctxt, stbl, En);
		def_val = add_non_lazy_context_rec(ctxt, stbl, def_val);
		E = Make_3inp_Primitive(P_NAMED_ARG, E, Make_VAR_leaf(new_name),
						     def_val);
	    } else {
		E = add_non_lazy_context_rec(ctxt, stbl, body);
	    }
	    if( innermost ) { pop_buf(ctxt, NULL); }
	    remove_binding(name);
	    res = Make_Lambda(new_name, E);
	    SET_LAMBDA_LINE_NBR(res, line);
	    MoveTypeHint(node, res, FALSE);
	    return res;
        case LEAF:
            if( !IS_VAR(node) ) {
                if( !IS_PRIM_FN(node) ) return( node );
                int pfn = GET_PRIM_FN(node);
                if( (pfn != P_PRINTF) && (pfn != P_SPRINTF) &&
                    (pfn != P_EPRINTF) && (pfn != P_FPRINTF) &&
                    (pfn != P_PRINT) && (pfn != P_MK_REF_VAR) )
                {
                    return(node);
                }
                // Printf like functions are non-lazy
                if( COUNT_BUF(ctxt) == 0 ) { return( node ); }
                ctx_nd = Make_NIL();
                FOR_BUF(ctxt, string, sp) {
                    ctx_nd =
                        Make_2inp_Primitive(P_CONS,
                            Make_1inp_Primitive(P_UNTYPE, Make_VAR_leaf(*sp)),
                            ctx_nd);
                }
                return( Make_2inp_Primitive(P_NSEQ, ctx_nd, node) );
            }
            name = GET_VAR(node);
            if( is_bound(name) ) {
                // Rename bound variables to ensure no name capture happens
                // when implicit arguments for overloaded identifiers are added
                string new_name = wastrsave(&strings, tprintf(".%s", name));
                SET_VAR(node, new_name);
                return( node );
            }
            fp = Find_Function_Def(stbl, name);
            if( fp == NULL ) {
                if( file_load )
                    FP(err_fp, "===Type error around line %d in file %s\n",
                            GET_LINE_NBR(node), cur_file_name);
                else
                    FP(err_fp, "===Type error around line %d\n",
                            GET_LINE_NBR(node));
                FP(err_fp, "Unidentified identifier \"%s\"\n", GET_VAR(node));
                longjmp(context_env, 1);
            }
            // Add implicit arguments
            np = fp->implicit_args;
            v_line_nbr = GET_LINE_NBR(node);
            MAKE_REDEX_USERDEF(node, fp);
            SET_LINE_NBR(node, v_line_nbr);
            while( np != NULL ) {
                g_ptr ifn = Make_USERDEF_leaf(np->def);
                SET_LINE_NBR(ifn, v_line_nbr);
                node = Make_APPL_ND(node, ifn);
                np = np->next;
            }
	    if( COUNT_BUF(ctxt) != 0 && is_non_lazy(stbl, name) ) {
		ctx_nd = Make_NIL();
		FOR_BUF(ctxt, string, sp) {
		    ctx_nd = Make_2inp_Primitive(
				    P_CONS,
				    Make_1inp_Primitive(P_UNTYPE,
							Make_VAR_leaf(*sp)),
				    ctx_nd);
		}
		node = Make_2inp_Primitive(P_NSEQ, ctx_nd, node);
	    }
	    // Add any remaining default arguments
	    if( fp->has_default_values ) {
		ap = fp->arg_names;
		while( ap != NULL ) {
		    F = ap->default_value;
		    if( F == NULL ) {
			report_error_loc(node, "Syntax");
			FP(err_fp,"%s is not given value for argument %s\n",
				  name, ap->name);
			longjmp(context_env, 1);
		    }
		    F = add_non_lazy_context_rec(ctxt, stbl, F);
		    node = Make_APPL_ND(node, F);
		    ap = ap->next;
		}
	    }
	    return( node );
        default:
            DIE("Impossible");
    }
    return NULL;    /* Dummy */
  wrap_up:
    FUB_ROF(&arg_info_buf, arg_info_rec, aip) {
	node = cond_mk_app(aip->spine, node, aip->arg_expr);
    }
    free_buf(&arg_info_buf);
    return( node );

  err_named_arg:
    report_error_loc(node, "Syntax");
    FP(err_fp, "Named argument in calling ", GET_VAR(node));
    Print_leaf(node, err_fp);
    FP(err_fp, "\n");
    free_buf(&arg_info_buf);
    longjmp(context_env, 1);

}

static bool
check_match(string pat, string s)
{
    if( STREQ(pat, s_star) ) return TRUE;
    if( s == NULL ) return FALSE;
    if( fnmatch(pat, s, 0) == 0 ) return TRUE;
    return FALSE;
}

static bool
check_type_match(string arg_pat, string res_pat, typeExp_ptr type)
{
    if( STREQ(arg_pat, s_star) ) {
        if( STREQ(res_pat, s_star) ) { return TRUE; }
        while( type->typeOp == arrow_tp ) {
            type = Get_Real_Type(type->typelist->next->type);
        }
        typeExp_ptr ttp = Get_Type(res_pat, NULL, TP_DONT_INSERT);
        if( ttp != NULL ) {
            return( Type_eq(type,ttp) );
        }
        return( fnmatch(res_pat, Get_tmp_type_string(type), 0) == 0 );
    } else {
        typeExp_ptr ttp = Get_Type(arg_pat, NULL, TP_DONT_INSERT);
        bool match = FALSE;
        while( type->typeOp == arrow_tp ) {
            if( !match ) {
                typeExp_ptr arg_type = type->typelist->type;
                if( ttp != NULL ) {
                    if( Type_eq(ttp, arg_type) ) {
                        match = TRUE;
                    }
                } else {
                    if(fnmatch(arg_pat, Get_tmp_type_string(arg_type),0) == 0) {
                        match = TRUE;
                    }
                }
            }
            type = Get_Real_Type(type->typelist->next->type);
        }
        if( !match ) { return FALSE; }
        if( STREQ(res_pat, s_star) ) { return TRUE; }
        ttp = Get_Type(res_pat, NULL, TP_DONT_INSERT);
        if( ttp != NULL ) {
            return( Type_eq(type,ttp) );
        }
        return( fnmatch(res_pat, Get_tmp_type_string(type), 0) == 0 );
    }
}

static g_ptr
cond_mk_app(g_ptr old, g_ptr E, g_ptr F)
{
    g_ptr old_E, old_F, res;
    old_E = GET_APPLY_LEFT(old);
    old_F = GET_APPLY_RIGHT(old);
    if( old_E == E ) {
        if( old_F == F ) {
            INC_REFCNT(old);
            return old;
        } else {
            INC_REFCNT(old_E);
            res = Make_APPL_ND(old_E, F);
            MoveTypeHint(old, res, FALSE);
            return res;
        }
    } else {
        if( old_F == F ) {
            INC_REFCNT(old_F);
            res = Make_APPL_ND(E, old_F);
            MoveTypeHint(old, res, FALSE);
            return res;
        } else {
            res = Make_APPL_ND(E, F);
            MoveTypeHint(old, res, FALSE);
            return res;
        }
    }
}

static g_ptr
cond_mk_cons(g_ptr old, g_ptr E, g_ptr F)
{
    g_ptr old_E, old_F, res;
    old_E = GET_CONS_HD(old);
    old_F = GET_CONS_TL(old);

    if( E == NULL || F == NULL || old_E == NULL || old_F == NULL ) {
        ASSERT(E == NULL);
        ASSERT(old_E == NULL);
        ASSERT(F == NULL);
        ASSERT(old_F == NULL);
        INC_REFCNT(old);
        return old;
    }
    if( old_E == E ) {
        if( old_F == F ) {
            INC_REFCNT(old);
            return old;
        } else {
            INC_REFCNT(old_E);
            res = Make_CONS_ND(old_E, F);
            MoveTypeHint(old, res, FALSE);
            return res;
        }
    } else {
        if( old_F == F ) {
            INC_REFCNT(old_F);
            res = Make_CONS_ND(E, old_F);
            MoveTypeHint(old, res, FALSE);
            return res;
        } else {
            res = Make_CONS_ND(E, F);
            MoveTypeHint(old, res, FALSE);
            return res;
        }
    }
}

static bool
already_defined(symbol_tbl_ptr stbl, fn_ptr fp)
{
    string name = fp->name;
    return( find_hash(stbl->tbl_ptr,(pointer) name) != NULL );
}

static void
update_stbl(symbol_tbl_ptr stbl, fn_ptr fp)
{
    string name = fp->name;
    fn_ptr last = (fn_ptr) find_hash(stbl->tbl_ptr,(pointer) name);
    if( last == fp ) { return; }
    if(last != NULL && last != fp ) {
        delete_hash(stbl->tbl_ptr, (pointer) name);
    }
    insert_hash(stbl->tbl_ptr, (pointer) name, (pointer) fp);
}

static bool
built_in(string filename)
{
    int len = strlen(filename);
    if( len < builtins_len ) { return FALSE; }
    return( strcmp(filename + (len-builtins_len), "builtins.fl") == 0 );
}

static g_ptr
ignore_P_Y(g_ptr node)
{
    if( !IS_APPLY(node) ) return node;
    if( !IS_P_Y(GET_APPLY_LEFT(node)) ) return node;
    node = GET_APPLY_RIGHT(node);
    ASSERT( IS_LAMBDA(node) );
    node = GET_LAMBDA_BODY(node);
    return node;
}


static bool
is_P_NAMED_ARG(g_ptr node, g_ptr *E, g_ptr *var, g_ptr *default_val)
{
    if( !IS_APPLY(node) ) return FALSE;
    g_ptr l = GET_APPLY_LEFT(node);
    if( !IS_APPLY(l) ) return FALSE;
    g_ptr ll = GET_APPLY_LEFT(l);
    if( !IS_APPLY(ll) ) return FALSE;
    g_ptr lll = GET_APPLY_LEFT(ll);
    if( !IS_NAMED_ARG(lll) ) return FALSE;
    *default_val = GET_APPLY_RIGHT(node);
    *var = GET_APPLY_RIGHT(l);
    *E = GET_APPLY_RIGHT(ll);
    return TRUE;
}

static g_ptr
ignore_P_NAMED_ARG(g_ptr node)
{
    g_ptr E, var, def_val;
    if( is_P_NAMED_ARG(node, &E, &var, &def_val) ) {
	return E;
    }
    return node;
}

static g_ptr
ignore_P_DEBUG(g_ptr node)
{
    if( !IS_APPLY(node) ) return node;
    if( !IS_DEBUG(GET_APPLY_LEFT(node)) ) return node;
    node = GET_APPLY_RIGHT(node);
    return node;
}

static bool
get_named_arg(g_ptr node, name_expr_ptr name_expp, g_ptr *next_nodep)
{
    if( !IS_APPLY(node) ) return FALSE;
    if( !IS_LEAF_VAR(GET_APPLY_RIGHT(node)) ) return FALSE;
    node = GET_APPLY_LEFT(node);
    if( !IS_LAMBDA(node) ) return FALSE;
    name_expp->var = GET_LAMBDA_VAR(node);
    g_ptr body = GET_LAMBDA_BODY(node);
    g_ptr E, var, default_val;
    if( is_P_NAMED_ARG(body, &E, &var, &default_val) ) {
	name_expp->expr = default_val;
	*next_nodep = E;
    } else {
	name_expp->expr = NULL;
	*next_nodep = body;
    }
    return TRUE;
}

static bool
get_HFL_arg_name(g_ptr node, name_expr_rec *nepp, g_ptr *next_nodep)
{
    if( !IS_APPLY(node) ) return FALSE;
    if( !IS_LEAF_VAR(GET_APPLY_RIGHT(node)) ) return FALSE;
    if( !STREQ(GET_VAR(GET_APPLY_RIGHT(node)), s_CELL) ) return FALSE;
    node = GET_APPLY_LEFT(node);
    if( !IS_APPLY(node) ) return FALSE;
    if( !IS_STRING(GET_APPLY_RIGHT(node)) ) return FALSE;
    string name = GET_STRING(GET_APPLY_RIGHT(node)); 
    nepp->var = name;
    nepp->expr = NULL;
    node = GET_APPLY_LEFT(node);
    if( !IS_LEAF_VAR(GET_APPLY_LEFT(node)) ) return FALSE;
    node = GET_APPLY_RIGHT(node);
    if( !IS_LAMBDA(node) ) return FALSE;
    if( !STREQ(GET_LAMBDA_VAR(node), name) ) return FALSE;
    node = GET_LAMBDA_BODY(node);
    if( !IS_LAMBDA(node) ) return FALSE;
    if( !STREQ(GET_LAMBDA_VAR(node), s_CELL) ) return FALSE;
    node = GET_LAMBDA_BODY(node);
    *next_nodep = node;
    return TRUE;
}

static g_ptr
ignore_simple_P_PCATCH(g_ptr onode)
{
    g_ptr node = onode;
    if( !IS_APPLY(node) ) return onode;
    if( !IS_APPLY(GET_APPLY_RIGHT(node)) ) return onode;
    if( !IS_P_ERROR(GET_APPLY_LEFT(GET_APPLY_RIGHT(node))) ) return onode;
    node = GET_APPLY_LEFT(node);
    if( !IS_APPLY(node) ) return onode;
    if( !IS_P_PCATCH(GET_APPLY_LEFT(node)) ) return onode;
    node = GET_APPLY_RIGHT(node);
    return node;
}

static g_ptr
ignore_P_CACHE(g_ptr onode)
{
    g_ptr node = onode;
    if( !IS_APPLY(node) ) return onode;
    if( !IS_APPLY(GET_APPLY_LEFT(node)) ) return onode;
    if( !IS_P_CACHE(GET_APPLY_LEFT(GET_APPLY_LEFT(node))) ) return onode;
    node = GET_APPLY_RIGHT(node);
    return node;
}

static g_ptr
ignore_P_STRICT_ARGS(g_ptr onode)
{
    g_ptr node = onode;
    if( !IS_APPLY(node) ) return onode;
    node = GET_APPLY_LEFT(node);
    if( !IS_APPLY(node) ) return onode;
    node = GET_APPLY_LEFT(node);
    if( !IS_P_STRICT_ARGS(node) ) return onode;
    node = GET_APPLY_RIGHT(onode);
    return node;
}

static fn_ptr
get_fn_rec()
{
    fn_ptr res = (fn_ptr) new_rec(&fn_rec_mgr);
    res->id = fn_rec_id_cnt;
    fn_rec_id_cnt++;
    return res;
}
