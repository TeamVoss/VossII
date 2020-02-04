//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/* Data types common to fl and its plugins.
 * Note that when included by plugins, graph manipulation macros always have
 * "paranoia checks" turned off, as said checks are implemented as functions
 * which are not easily made available to plugins without having to build
 * against the rest of the plugin source.
 */
#ifndef __PLUGIN_H
#define __PLUGIN_H

#ifndef FL_BINARY
#define FL_PLUGIN 1
#endif

#include "abi_version.h"
#include "types.h"
#include "strings.h"
#include "typecheck.h"

g_ptr void_nd;
#include "graph.h"

/* Functions made available to fl plugins */
typedef struct fl_plugin_api_rec {
  /* Registering types/funs.
     Returns 0 on success. Plugin must abort load immediatly if return value
     is nonzero.
  */
  int (*register_types)(int, fl_plugin_types);
  int (*register_funs)(int, fl_plugin_funs);

  /* Type language */
  typeExp_ptr (*make_arrow)(typeExp_ptr from, typeExp_ptr to);
  typeExp_ptr (*make_tuple)(typeExp_ptr fst, typeExp_ptr snd);
  typeExp_ptr (*make_list)(typeExp_ptr type);
  typeExp_ptr (*make_void)();
  typeExp_ptr (*make_bool)();
  typeExp_ptr (*make_int)();
  typeExp_ptr (*make_double)();
  typeExp_ptr (*make_string)();

  /* Kill the interpreter, with the given error message. */
  void (*die)(const char* fmt, ...);


  /* Working with redexes */

  /* Save a string to the string manager.
     Do this with any strings you plan on passing back into fl.
  */
  string (*wastrsave)(string s);

  /* Return failure, with the given error message. */
  void (*fail)(bool append_trace, g_ptr redex, const char* fmt, ...);

  /* Decrement the reference count of the given node. */
#if TRACK_FREEING
  void (*dec_ref_cnt)(g_ptr node, string file, int line);
#else
  void (*dec_ref_cnt)(g_ptr node);
#endif


  /* Increment the reference count of the given node. */
  void (*inc_ref_cnt)(g_ptr node);

  /* Set the given redex to the given value for int, string, etc. respectively. */
  void (*make_redex_small_int)(g_ptr node, long int i);
  void (*make_redex_large_int)(g_ptr node, char* i);
  void (*make_redex_double)(g_ptr node, double d);
  void (*make_redex_string)(g_ptr node, string s);
  void (*make_redex_nil)(g_ptr node);
  void (*make_redex_void)(g_ptr node);
  void (*make_redex_bool)(g_ptr node, formula b);
  void (*make_redex_cons)(g_ptr node, g_ptr head, g_ptr tail);
  void (*make_redex_ext_obj)(g_ptr node, int class, pointer p);

  /* Create a new, garbage-collected, node for the given value. */
  g_ptr (*new_small_int)(long int i);
  g_ptr (*new_large_int)(char* i);
  g_ptr (*new_double)(double d);
  g_ptr (*new_bool)(formula b);
  g_ptr (*new_string)(string s);
  g_ptr (*new_ext_obj)(int class, pointer obj);
  g_ptr (*new_nil)();
  g_ptr (*new_cons)(g_ptr hd, g_ptr tl);
  g_ptr (*new_void)();

  /* Get the left/right node of a function application respectively. */
  g_ptr (*get_apply_left)(g_ptr redex);
  g_ptr (*get_apply_right)(g_ptr redex);

  /* Is the given redex a NIL node? */
  int (*is_nil)(g_ptr redex);

  /* Get the value of the given node as an arbitrary precision integer. */
  void* (*get_int)(g_ptr node);

  /* Get the value of the given node as a double precision floating point number. */
  double (*get_double)(g_ptr node);

  /* Get head of a list node. */
  g_ptr (*get_head)(g_ptr node);

  /* Get the tail of a list node. */
  g_ptr (*get_tail)(g_ptr node);

  /* Get the value of the given node as a boolean. */
  formula (*get_bool)(g_ptr node);

  /* Get the value of the given node as a string. */
  string (*get_string)(g_ptr node);

  /* Get the value of the given node as a pointer to an external object. */
  void* (*get_ext_obj)(g_ptr node);

  /* Apply the given lambda node to the given argument node. */
  g_ptr (*make_apply)(g_ptr f, g_ptr x);

  /* The ONE/TRUE constant formula. */
  formula (*one)();

  /* The ZERO/FALSE constant formula. */
  formula (*zero)();

  /* Allocate a new, garbage-collected node. */
  g_ptr (*get_node)();

  /* Evaluate the given node. */
  g_ptr (*force)(g_ptr node);

  /* Protect/unprotect the given node from being garbage collected.
     Returns true if the object's GC protection status was changed; false if
     it was already protected/unprotected.
   */
  bool (*gc_protect)(g_ptr node);
  bool (*gc_unprotect)(g_ptr node);

  /* Is the given node a failure or not? */
  bool (*is_fail)(g_ptr node);

  /* Get the failure string of the given node, if it is a failure.
     Behaviour is undefined if it isn't.
   */
  string (*get_fail_string)(g_ptr node);

  /* Prints the node type of the given node to stderr.
     Also prints the leaf type, if the node is a leaf.
   */
  void (*print_node_type)(g_ptr node);
} fl_plugin_api_rec;
typedef fl_plugin_api_rec* fl_plugin_api;

/* The fixity of an fl function. */
typedef enum { NO_FIXITY, PREFIX, INFIXL, INFIXR, POSTFIX } fl_fun_fixity;


/* Structure representing a single function exported to fl from an fl plugin. */
typedef struct fl_plugin_fun_rec {
  /* Name of the function. Needs to be a valid fl identifier. */
  char* name;

  /* Strictness signature of the function's arguments.
   * Consists of a string of ones and zeroes, where a one denotes a strict
   * argument and a zero a lazy one.
   * For instance, a function with two strict arguments would have
   * the strictness signatures "11".
   */
  char* strictness;

  /* Is the function non-lazy? */
  bool non_lazy;

  /* The fixity of the function. */
  fl_fun_fixity fixity;

  /* Precedence of the function.
     Valid values are 0-9 for infix functions and 0-1 for prefix.
     Ignored if fixity is POSTFIX or NO_FIXITY.
  */
  int precedence;

  /* fl type of the exported function. */
  typeExp_ptr type;

  /* Pointer to the code implementing the export's functionality. */
  void (*fun_ptr)(g_ptr);
} fl_plugin_fun_rec;
typedef fl_plugin_fun_rec* fl_plugin_fun;
typedef fl_plugin_fun_rec* fl_plugin_funs;


/* Structure representing a single type exported to fl from an fl plugin. */
typedef struct fl_plugin_type_rec {
  /* Name of the type. */
  char* name;

  /* Hooks to be called on garbage collection. */
  void (*mark)(pointer p);
  void (*sweep)();

  /* Functions for saving/loading values of the type. */
  void (*save)(FILE* fp, pointer p);
  pointer (*load)(FILE* fp);

  /* Convert the given object into some string representation. */
  string (*obj2string)(pointer p);

  /* Equality comparison of two objects of this type.
     equals((a && b), b, 0) == a, but most types will likely just return
     either true or false.
   */
  formula (*equals)(pointer a, pointer b, bool identical);

  pointer (*gmap)(gmap_info_ptr ip, pointer a);
  pointer (*gmap2)(gmap_info_ptr ip, pointer a, pointer b);

  /* OUT: the class id of the type; use with make_redex_ext_obj, for instance. */
  int class;

  /* OUT: the type expression of the type */
  typeExp_ptr type;
} fl_plugin_type_rec;
typedef fl_plugin_type_rec* fl_plugin_type;
typedef fl_plugin_type_rec* fl_plugin_types;


/* Metadata for an fl plugin. */
typedef struct fl_plugin_rec {
  /* Name of plugin. Currently mainly for diagnostic messages. */
  char* name;

  /* Version of the plugin. */
  int version;

  /* Required ABI version. */
  char* abi_version;

  /* Number of exported functions. */
  int num_funs;

  /* Number of exported types. */
  int num_types;
} fl_plugin_rec;
typedef fl_plugin_rec* fl_plugin;

#endif
