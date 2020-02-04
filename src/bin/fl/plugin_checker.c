//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/* Sanity checks for plugins loaded by plugin_loader.c. */
#include "plugin.h"
#include "plugin_errlog.h"

/* How many function arguments does the given type have? */
static int
get_num_args(typeExp_ptr t)
{
    if(t->typeOp == arrow_tp) {
        return 1 + get_num_args(t->typelist->next->type);
    }
    return 0;
}

/* Check that the strictness annotation and type agree about
   the number of arguments.
*/
static void
check_arg_strictness(fl_plugin_fun f, plugin_errlog log)
{
    int num_strict = strlen(f->strictness);
    int num_args = get_num_args(f->type);
    if(num_strict != num_args) {
        const char* fmt = "'%s' has %d args, but strictness is declared for %d.";
        log_plugin_error(log, fmt, f->name, num_args, num_strict);
    }
}

/* Check that the function has an associated function pointer. */
static void
check_fun_ptr_not_null(fl_plugin_fun f, plugin_errlog log)
{
    if(f->fun_ptr == NULL) {
        log_plugin_error(log, "'%s' has no function pointer.", f->name);
    }
}

/* Check that the function's precedence is in range. */
static void
check_fun_precedence_in_range(fl_plugin_fun f, plugin_errlog log) {
    int out_of_range = 0;
    switch(f->fixity) {
    case PREFIX:
        out_of_range = (f->precedence < 0 || f->precedence > 1);
        break;
    case INFIXL:
    case INFIXR:
        out_of_range = (f->precedence < 0 || f->precedence > 9);
        break;
    case NO_FIXITY:
    case POSTFIX:
        /* These don't have any precedence. */
        break;
    }
    if(out_of_range) {
        const char* fmt = "'%s' has out of range precedence: %d.";
        log_plugin_error(log, fmt, f->name, f->precedence);
    }
}

/* Check that the function's fixity declaration makes sense. */
static void
check_fun_fixity(fl_plugin_fun f, plugin_errlog log)
{
    switch(f->fixity) {
    case NO_FIXITY:
        /* Nothing to check here; no fixity always makes sense. */
        break;
    case PREFIX:
    case POSTFIX:
        if(get_num_args(f->type) < 1) {
          const char* fmt = "postfix function '%s' needs at least one argument.";
          log_plugin_error(log, fmt, f->name);
        }
        break;
    case INFIXL:
    case INFIXR:
        if(get_num_args(f->type) < 2) {
          const char* fmt = "infix function '%s' needs at least two arguments.";
          log_plugin_error(log, fmt, f->name);
        }
        break;
    default:
        log_plugin_error(log, "'%s' has invalid fixity: %d.", f->name, f->fixity);
    }
}

/* Check that the function has a name. */
static void
check_fun_has_name(fl_plugin_fun f, plugin_errlog log)
{
    if(f->name == NULL || !strcmp(f->name, "")) {
        log_plugin_error(log, "symbol with no name exported.");
    }
}

/* Check that the function has a type. */
static void
check_fun_has_type(fl_plugin_fun f, plugin_errlog log)
{
    if(f->type == NULL) {
        log_plugin_error(log, "'%s' has no type.", f->name);
    }
}

/* Check that the function has strictness signature. */
static void
check_fun_has_strictness(fl_plugin_fun f, plugin_errlog log)
{
    if(f->strictness == NULL || strnlen(f->strictness, 100) >= 100) {
        log_plugin_error(log, "'%s' has no strictness signature.", f->name);
    }
}

/* Check that the function exists at all. */
static void
check_fun_not_null(fl_plugin_fun f, plugin_errlog log)
{
    if(f == NULL) {
        log_plugin_error(log, "NULL function entry exported.");
    }
}

/* Sanity check the given plugin function. */
static void
check_plugin_fun(fl_plugin_fun f, plugin_errlog log)
{
    if(f == NULL) {
        log_plugin_error(log, "plugin metadata is NULL.");
        return;
    }
    check_fun_not_null(f, log);
    check_fun_ptr_not_null(f, log);
    check_fun_has_name(f, log);
    check_fun_has_type(f, log);
    check_fun_has_strictness(f, log);
    check_fun_fixity(f, log);
    check_fun_precedence_in_range(f, log);
    check_arg_strictness(f, log);
}

/* Check that the plugin's ABI version matches that of the fl binary. */
static void
check_abi_version(fl_plugin p, plugin_errlog log)
{
    if(p->abi_version == NULL || !strcmp(p->abi_version, "")) {
        log_plugin_error(log, "plugin has no ABI version.");
        return;
    }
    if(strcmp(FL_ABI_VERSION, p->abi_version) != 0) {
        const char* fmt = "plugin requires ABI version '%s', but fl provides '%s'.";
        log_plugin_error(log, fmt, p->abi_version, FL_ABI_VERSION);
    }
}

/* Check that the plugin has a name. */
static void
check_plugin_name(fl_plugin p, plugin_errlog log)
{
    if(p->name == NULL || !strcmp(p->name, "")) {
        log_plugin_error(log, "plugin has no name.");
    }
}

/* Check whether the given plugin exports anything. */
static void
check_exports_anything(fl_plugin p, plugin_errlog log)
{
    if(p->num_funs <= 0) {
        log_plugin_error(log, "plugin does not export any functionality.");
    }
}

static void
check_exports_nonnegative_types(fl_plugin p, plugin_errlog log)
{
    if(p->num_types < 0) {
        const char* fmt = "plugin exports negative number of types: %d.";
        log_plugin_error(log, fmt, p->num_types);
    }
}

/* Report the given callback missing for the given type and return failure. */
static void
missing_callback(char* type, char* callback, plugin_errlog log)
{
    static const char *fmt = "type '%s' has null function pointer for callback '%s'.";
    log_plugin_error(log, fmt, type, callback);
}

/* Check that all type fields are present. */
static void
check_plugin_type(fl_plugin_type t, plugin_errlog log)
{
    if(t->name == NULL || !strcmp(t->name, "")) {
        log_plugin_error(log, "type has no name.");
        return;
    }
    if(Get_Type(t->name, NULL, TP_DONT_INSERT)) {
        log_plugin_error(log, "type '%s' is alredy defined.", t->name);
    }
    if(t->mark == NULL)       {missing_callback(t->name, "mark", log);}
    if(t->sweep == NULL)      {missing_callback(t->name, "sweep", log);}
    if(t->save == NULL)       {missing_callback(t->name, "save", log);}
    if(t->load == NULL)       {missing_callback(t->name, "load", log);}
    if(t->obj2string == NULL) {missing_callback(t->name, "obj2string", log);}
    if(t->equals == NULL)     {missing_callback(t->name, "equals", log);}
    if(t->gmap == NULL)       {missing_callback(t->name, "gmap", log);}
    if(t->gmap2 == NULL)      {missing_callback(t->name, "gmap2", log);}
}

/* Sanity check the given plugin. */
void
check_plugin(fl_plugin p, plugin_errlog log)
{
    check_plugin_name(p, log);
    check_abi_version(p, log);
    check_exports_anything(p, log);
    check_exports_nonnegative_types(p, log);
}

/* Sanity check the given types. */
void
check_plugin_types(int num_types, fl_plugin_types types, plugin_errlog log)
{
  if(num_types > 0 && types == NULL) {
      const char* fmt = "plugin claims to export %d types, but types is null.";
      log_plugin_error(log, fmt, num_types);
      return;
  }
  for(int i = 0; i < num_types; i++) {
      check_plugin_type(&types[i], log);
  }
}

/* Sanity check the given functions. */
void
check_plugin_funs(int num_funs, fl_plugin_funs funs, plugin_errlog log)
{
    if(num_funs > 0 && funs == NULL) {
        const char* fmt = "plugin claims to export %d functions, but funs is null.";
        log_plugin_error(log, fmt, num_funs);
        return;
    }
    for(int i = 0; i < num_funs; i++) {
        check_plugin_fun(&funs[i], log);
    }
}
