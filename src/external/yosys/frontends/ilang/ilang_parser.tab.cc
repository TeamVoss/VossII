/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Substitute the type names.  */
#define YYSTYPE         RTLIL_FRONTEND_ILANG_YYSTYPE
/* Substitute the variable and function names.  */
#define yyparse         rtlil_frontend_ilang_yyparse
#define yylex           rtlil_frontend_ilang_yylex
#define yyerror         rtlil_frontend_ilang_yyerror
#define yydebug         rtlil_frontend_ilang_yydebug
#define yynerrs         rtlil_frontend_ilang_yynerrs

#define yylval          rtlil_frontend_ilang_yylval
#define yychar          rtlil_frontend_ilang_yychar

/* Copy the first part of user declarations.  */
#line 25 "frontends/ilang/ilang_parser.y" /* yacc.c:339  */

#include <list>
#include "frontends/ilang/ilang_frontend.h"
YOSYS_NAMESPACE_BEGIN
namespace ILANG_FRONTEND {
	std::istream *lexin;
	RTLIL::Design *current_design;
	RTLIL::Module *current_module;
	RTLIL::Wire *current_wire;
	RTLIL::Memory *current_memory;
	RTLIL::Cell *current_cell;
	RTLIL::Process *current_process;
	std::vector<std::vector<RTLIL::SwitchRule*>*> switch_stack;
	std::vector<RTLIL::CaseRule*> case_stack;
	dict<RTLIL::IdString, RTLIL::Const> attrbuf;
	bool flag_nooverwrite, flag_overwrite, flag_lib;
	bool delete_current_module;
}
using namespace ILANG_FRONTEND;
YOSYS_NAMESPACE_END
USING_YOSYS_NAMESPACE

#line 98 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "ilang_parser.tab.hh".  */
#ifndef YY_RTLIL_FRONTEND_ILANG_YY_FRONTENDS_ILANG_ILANG_PARSER_TAB_HH_INCLUDED
# define YY_RTLIL_FRONTEND_ILANG_YY_FRONTENDS_ILANG_ILANG_PARSER_TAB_HH_INCLUDED
/* Debug traces.  */
#ifndef RTLIL_FRONTEND_ILANG_YYDEBUG
# if defined YYDEBUG
#if YYDEBUG
#   define RTLIL_FRONTEND_ILANG_YYDEBUG 1
#  else
#   define RTLIL_FRONTEND_ILANG_YYDEBUG 0
#  endif
# else /* ! defined YYDEBUG */
#  define RTLIL_FRONTEND_ILANG_YYDEBUG 1
# endif /* ! defined YYDEBUG */
#endif  /* ! defined RTLIL_FRONTEND_ILANG_YYDEBUG */
#if RTLIL_FRONTEND_ILANG_YYDEBUG
extern int rtlil_frontend_ilang_yydebug;
#endif
/* "%code requires" blocks.  */
#line 53 "frontends/ilang/ilang_parser.y" /* yacc.c:355  */

#include <string>
#include <vector>
#include "frontends/ilang/ilang_frontend.h"

#line 142 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:355  */

/* Token type.  */
#ifndef RTLIL_FRONTEND_ILANG_YYTOKENTYPE
# define RTLIL_FRONTEND_ILANG_YYTOKENTYPE
  enum rtlil_frontend_ilang_yytokentype
  {
    TOK_ID = 258,
    TOK_VALUE = 259,
    TOK_STRING = 260,
    TOK_INT = 261,
    TOK_AUTOIDX = 262,
    TOK_MODULE = 263,
    TOK_WIRE = 264,
    TOK_WIDTH = 265,
    TOK_INPUT = 266,
    TOK_OUTPUT = 267,
    TOK_INOUT = 268,
    TOK_CELL = 269,
    TOK_CONNECT = 270,
    TOK_SWITCH = 271,
    TOK_CASE = 272,
    TOK_ASSIGN = 273,
    TOK_SYNC = 274,
    TOK_LOW = 275,
    TOK_HIGH = 276,
    TOK_POSEDGE = 277,
    TOK_NEGEDGE = 278,
    TOK_EDGE = 279,
    TOK_ALWAYS = 280,
    TOK_GLOBAL = 281,
    TOK_INIT = 282,
    TOK_UPDATE = 283,
    TOK_PROCESS = 284,
    TOK_END = 285,
    TOK_INVALID = 286,
    TOK_EOL = 287,
    TOK_OFFSET = 288,
    TOK_PARAMETER = 289,
    TOK_ATTRIBUTE = 290,
    TOK_MEMORY = 291,
    TOK_SIZE = 292,
    TOK_SIGNED = 293,
    TOK_REAL = 294,
    TOK_UPTO = 295
  };
#endif

/* Value type.  */
#if ! defined RTLIL_FRONTEND_ILANG_YYSTYPE && ! defined RTLIL_FRONTEND_ILANG_YYSTYPE_IS_DECLARED

union RTLIL_FRONTEND_ILANG_YYSTYPE
{
#line 59 "frontends/ilang/ilang_parser.y" /* yacc.c:355  */

	char *string;
	int integer;
	YOSYS_NAMESPACE_PREFIX RTLIL::Const *data;
	YOSYS_NAMESPACE_PREFIX RTLIL::SigSpec *sigspec;
	std::vector<YOSYS_NAMESPACE_PREFIX RTLIL::SigSpec> *rsigspec;

#line 203 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:355  */
};

typedef union RTLIL_FRONTEND_ILANG_YYSTYPE RTLIL_FRONTEND_ILANG_YYSTYPE;
# define RTLIL_FRONTEND_ILANG_YYSTYPE_IS_TRIVIAL 1
# define RTLIL_FRONTEND_ILANG_YYSTYPE_IS_DECLARED 1
#endif


extern RTLIL_FRONTEND_ILANG_YYSTYPE rtlil_frontend_ilang_yylval;

int rtlil_frontend_ilang_yyparse (void);

#endif /* !YY_RTLIL_FRONTEND_ILANG_YY_FRONTENDS_ILANG_ILANG_PARSER_TAB_HH_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 220 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined RTLIL_FRONTEND_ILANG_YYSTYPE_IS_TRIVIAL && RTLIL_FRONTEND_ILANG_YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   153

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  47
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  46
/* YYNRULES -- Number of rules.  */
#define YYNRULES  96
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  173

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   295

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    41,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    44,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    42,     2,    43,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    45,     2,    46,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40
};

#if RTLIL_FRONTEND_ILANG_YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    86,    86,    86,    94,    97,    97,   100,   101,   102,
     102,   106,   130,   106,   142,   142,   146,   146,   146,   146,
     146,   146,   146,   146,   149,   155,   162,   169,   174,   174,
     186,   189,   192,   195,   198,   201,   206,   211,   215,   219,
     219,   232,   235,   238,   240,   244,   244,   255,   260,   266,
     272,   278,   282,   282,   298,   298,   307,   309,   312,   312,
     322,   326,   330,   333,   337,   338,   339,   339,   343,   352,
     352,   359,   359,   365,   365,   371,   371,   376,   380,   381,
     382,   383,   384,   387,   391,   395,   426,   429,   435,   439,
     445,   451,   457,   462,   466,   470,   478
};
#endif

#if RTLIL_FRONTEND_ILANG_YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TOK_ID", "TOK_VALUE", "TOK_STRING",
  "TOK_INT", "TOK_AUTOIDX", "TOK_MODULE", "TOK_WIRE", "TOK_WIDTH",
  "TOK_INPUT", "TOK_OUTPUT", "TOK_INOUT", "TOK_CELL", "TOK_CONNECT",
  "TOK_SWITCH", "TOK_CASE", "TOK_ASSIGN", "TOK_SYNC", "TOK_LOW",
  "TOK_HIGH", "TOK_POSEDGE", "TOK_NEGEDGE", "TOK_EDGE", "TOK_ALWAYS",
  "TOK_GLOBAL", "TOK_INIT", "TOK_UPDATE", "TOK_PROCESS", "TOK_END",
  "TOK_INVALID", "TOK_EOL", "TOK_OFFSET", "TOK_PARAMETER", "TOK_ATTRIBUTE",
  "TOK_MEMORY", "TOK_SIZE", "TOK_SIGNED", "TOK_REAL", "TOK_UPTO", "','",
  "'['", "']'", "':'", "'{'", "'}'", "$accept", "input", "$@1", "EOL",
  "optional_eol", "design", "module", "$@2", "$@3", "module_body",
  "module_stmt", "param_stmt", "param_defval_stmt", "attr_stmt",
  "autoidx_stmt", "wire_stmt", "$@4", "wire_options", "memory_stmt", "$@5",
  "memory_options", "cell_stmt", "$@6", "cell_body", "proc_stmt", "$@7",
  "switch_stmt", "$@8", "attr_list", "switch_body", "$@9", "compare_list",
  "case_body", "assign_stmt", "sync_list", "$@10", "$@11", "$@12", "$@13",
  "sync_type", "update_list", "constant", "sigspec",
  "sigspec_list_reversed", "sigspec_list", "conn_stmt", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,    44,    91,    93,    58,   123,   125
};
# endif

#define YYPACT_NINF -71

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-71)))

#define YYTABLE_NINF -6

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
     -71,    19,    -7,   -71,   -71,   -71,    -3,    17,    30,    42,
     -71,   -71,   -71,   -71,   -71,    24,   -71,    26,   -71,   -71,
     -71,   -71,   -71,    29,   -71,   -71,    75,   -71,    48,    12,
      60,   -71,    64,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,   -71,    66,   -71,   -71,   -71,     7,   -71,
     -71,    24,   -71,    43,   -71,    12,    33,    68,    38,   -71,
     -71,   -71,   -71,    31,   -71,    -4,    69,    76,    79,    80,
     -71,   -71,   -71,    38,   -71,    28,   -71,   -71,   -71,   -71,
      85,    86,    88,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,    89,     8,   -71,   -71,   -71,   -71,    32,    54,
      12,    12,   -71,   -71,   -71,     1,    95,   -71,     0,   -71,
      38,     7,   111,   -71,    12,   -71,    24,    97,    98,   -71,
      38,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,    12,
     -71,    38,   -71,    24,    24,   -71,   -71,   -71,   -71,   -71,
      38,   -71,   -71,   -71,   -71,    67,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,    -8,    87,    87,    87,   -71,   -71,   -71,
      12,    87,    12,   -71,     7,    62,    38,    38,    12,   -71,
     -71,    38,     8
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       6,     0,     2,     1,     5,    10,     3,     0,     0,     0,
       7,     8,     9,     6,     6,     0,    27,     0,    11,    85,
      87,    86,     6,     4,    15,    26,     0,    28,     0,     0,
       0,    12,     0,    39,    14,    16,    17,    18,    19,    20,
      21,    22,    23,    38,     0,    89,    94,    88,     0,     6,
       6,     6,    44,     0,     6,    95,     0,     0,     6,    52,
      13,    24,     6,     0,     6,     0,     0,     0,     0,     0,
      33,    32,    45,    93,    92,     0,    96,    67,    25,     6,
       0,     0,     0,    29,    30,    31,    35,    36,    37,    34,
      51,    90,     0,    77,    40,    41,    43,    42,     0,     0,
       0,     0,    64,    65,    66,     0,     0,     6,     0,    91,
       6,     0,     0,     6,     0,    46,     0,     0,     0,    54,
       6,    78,    79,    80,    81,    82,     6,     6,     6,     0,
      53,     6,     6,     0,     0,    56,    68,    71,    73,    75,
       6,    50,    47,     6,     6,    60,    84,    84,    84,    69,
      48,    49,    57,     0,    72,    74,    76,    84,    58,     6,
       0,    70,    63,    55,     0,     6,    61,     6,     0,    67,
      83,    62,    59
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -71,   -71,   -71,   -14,   108,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,    -5,   -71,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,   -53,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
     -70,    -9,   -41,   -71,   -71,   -71
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     5,    16,    17,     6,    10,    24,    50,    26,
      34,    35,    36,   102,    12,    38,    43,    53,    39,    52,
      63,    40,    90,    98,    41,    77,   103,   135,   145,   153,
     162,   165,    93,   104,   105,   157,   146,   147,   148,   129,
     154,    47,    48,    55,    56,    42
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      18,    11,    84,   116,     7,     8,    22,    58,    25,   158,
      45,    19,    20,    21,    73,    45,    19,    20,    21,     3,
     112,    37,   159,    13,   100,     4,   101,    85,    19,    20,
      21,   113,     9,    14,    79,    59,    60,    61,   117,   118,
      72,    80,    62,     9,    76,    15,    64,   106,    78,    57,
      83,    44,    46,    65,    66,    67,    68,    46,    23,   110,
     111,    -5,   107,    49,    81,    94,   108,    51,    82,    54,
     120,    91,    92,   131,    75,    86,    69,   155,   156,    74,
      57,    70,    87,    71,    27,    88,    89,   161,   140,    28,
      29,    95,    96,   115,    97,    99,   119,   109,   114,   130,
     133,   134,     9,   168,    30,    31,   136,   132,     2,    32,
       9,    33,   137,   138,   139,   160,   172,   141,   142,   164,
       0,   166,     0,   167,   143,   144,   149,   171,     0,   150,
     151,   121,   122,   123,   124,   125,   126,   127,   128,     0,
     152,     0,     0,     0,     0,   163,     0,     0,     0,     0,
       0,   169,     0,   170
};

static const yytype_int16 yycheck[] =
{
      14,     6,     6,     3,     7,     8,    15,    48,    22,    17,
       3,     4,     5,     6,    55,     3,     4,     5,     6,     0,
      19,    26,    30,     6,    16,    32,    18,    31,     4,     5,
       6,    30,    35,     3,     3,    49,    50,    51,    38,    39,
      54,    10,    51,    35,    58,     3,     3,    15,    62,    42,
      64,     3,    45,    10,    11,    12,    13,    45,    32,   100,
     101,    32,    30,     3,    33,    79,    34,     3,    37,     3,
     111,    43,    44,   114,     6,     6,    33,   147,   148,    46,
      42,    38,     6,    40,     9,     6,     6,   157,   129,    14,
      15,     6,     6,   107,     6,     6,   110,    43,     3,   113,
       3,     3,    35,    41,    29,    30,   120,   116,     0,    34,
      35,    36,   126,   127,   128,    28,   169,   131,   132,   160,
      -1,   162,    -1,   164,   133,   134,   140,   168,    -1,   143,
     144,    20,    21,    22,    23,    24,    25,    26,    27,    -1,
     145,    -1,    -1,    -1,    -1,   159,    -1,    -1,    -1,    -1,
      -1,   165,    -1,   167
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    48,    51,     0,    32,    49,    52,     7,     8,    35,
      53,    60,    61,     6,     3,     3,    50,    51,    50,     4,
       5,     6,    88,    32,    54,    50,    56,     9,    14,    15,
      29,    30,    34,    36,    57,    58,    59,    60,    62,    65,
      68,    71,    92,    63,     3,     3,    45,    88,    89,     3,
      55,     3,    66,    64,     3,    90,    91,    42,    89,    50,
      50,    50,    88,    67,     3,    10,    11,    12,    13,    33,
      38,    40,    50,    89,    46,     6,    50,    72,    50,     3,
      10,    33,    37,    50,     6,    31,     6,     6,     6,     6,
      69,    43,    44,    79,    50,     6,     6,     6,    70,     6,
      16,    18,    60,    73,    80,    81,    15,    30,    34,    43,
      89,    89,    19,    30,     3,    50,     3,    38,    39,    50,
      89,    20,    21,    22,    23,    24,    25,    26,    27,    86,
      50,    89,    88,     3,     3,    74,    50,    50,    50,    50,
      89,    50,    50,    88,    88,    75,    83,    84,    85,    50,
      50,    50,    60,    76,    87,    87,    87,    82,    17,    30,
      28,    87,    77,    50,    89,    78,    89,    89,    41,    50,
      50,    89,    79
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    47,    49,    48,    50,    51,    51,    52,    52,    52,
      52,    54,    55,    53,    56,    56,    57,    57,    57,    57,
      57,    57,    57,    57,    58,    59,    60,    61,    63,    62,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    66,
      65,    67,    67,    67,    67,    69,    68,    70,    70,    70,
      70,    70,    72,    71,    74,    73,    75,    75,    77,    76,
      76,    78,    78,    78,    79,    79,    79,    79,    80,    82,
      81,    83,    81,    84,    81,    85,    81,    81,    86,    86,
      86,    86,    86,    87,    87,    88,    88,    88,    89,    89,
      89,    89,    89,    90,    90,    91,    92
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     3,     2,     2,     0,     2,     2,     2,
       0,     0,     0,     8,     2,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     4,     4,     3,     0,     5,
       3,     3,     2,     2,     3,     3,     3,     3,     0,     0,
       5,     3,     3,     3,     0,     0,     8,     5,     6,     6,
       5,     0,     0,     8,     0,     8,     0,     2,     0,     6,
       0,     1,     3,     0,     2,     2,     2,     0,     4,     0,
       7,     0,     6,     0,     6,     0,     6,     0,     1,     1,
       1,     1,     1,     5,     0,     1,     1,     1,     1,     1,
       4,     6,     3,     2,     0,     1,     4
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if RTLIL_FRONTEND_ILANG_YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !RTLIL_FRONTEND_ILANG_YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !RTLIL_FRONTEND_ILANG_YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 86 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		attrbuf.clear();
	}
#line 1426 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 3:
#line 88 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (attrbuf.size() != 0)
			rtlil_frontend_ilang_yyerror("dangling attribute");
	}
#line 1435 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 11:
#line 106 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		delete_current_module = false;
		if (current_design->has((yyvsp[-1].string))) {
			RTLIL::Module *existing_mod = current_design->module((yyvsp[-1].string));
			if (!flag_overwrite && (flag_lib || (attrbuf.count(ID::blackbox) && attrbuf.at(ID::blackbox).as_bool()))) {
				log("Ignoring blackbox re-definition of module %s.\n", (yyvsp[-1].string));
				delete_current_module = true;
			} else if (!flag_nooverwrite && !flag_overwrite && !existing_mod->get_bool_attribute(ID::blackbox)) {
				rtlil_frontend_ilang_yyerror(stringf("ilang error: redefinition of module %s.", (yyvsp[-1].string)).c_str());
			} else if (flag_nooverwrite) {
				log("Ignoring re-definition of module %s.\n", (yyvsp[-1].string));
				delete_current_module = true;
			} else {
				log("Replacing existing%s module %s.\n", existing_mod->get_bool_attribute(ID::blackbox) ? " blackbox" : "", (yyvsp[-1].string));
				current_design->remove(existing_mod);
			}
		}
		current_module = new RTLIL::Module;
		current_module->name = (yyvsp[-1].string);
		current_module->attributes = attrbuf;
		if (!delete_current_module)
			current_design->add(current_module);
		attrbuf.clear();
		free((yyvsp[-1].string));
	}
#line 1465 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 12:
#line 130 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (attrbuf.size() != 0)
			rtlil_frontend_ilang_yyerror("dangling attribute");
		current_module->fixup_ports();
		if (delete_current_module)
			delete current_module;
		else if (flag_lib)
			current_module->makeblackbox();
		current_module = nullptr;
	}
#line 1480 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 24:
#line 149 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_module->avail_parameters((yyvsp[-1].string));
		free((yyvsp[-1].string));
	}
#line 1489 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 25:
#line 155 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_module->avail_parameters((yyvsp[-2].string));
		current_module->parameter_default_values[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		free((yyvsp[-2].string));
	}
#line 1499 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 26:
#line 162 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		attrbuf[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		delete (yyvsp[-1].data);
		free((yyvsp[-2].string));
	}
#line 1509 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 27:
#line 169 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		autoidx = max(autoidx, (yyvsp[-1].integer));
	}
#line 1517 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 28:
#line 174 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire = current_module->addWire("$__ilang_frontend_tmp__");
		current_wire->attributes = attrbuf;
		attrbuf.clear();
	}
#line 1527 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 29:
#line 178 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (current_module->wire((yyvsp[-1].string)) != nullptr)
			rtlil_frontend_ilang_yyerror(stringf("ilang error: redefinition of wire %s.", (yyvsp[-1].string)).c_str());
		current_module->rename(current_wire, (yyvsp[-1].string));
		free((yyvsp[-1].string));
	}
#line 1538 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 30:
#line 186 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->width = (yyvsp[0].integer);
	}
#line 1546 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 31:
#line 189 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		rtlil_frontend_ilang_yyerror("ilang error: invalid wire width");
	}
#line 1554 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 32:
#line 192 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->upto = true;
	}
#line 1562 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 33:
#line 195 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->is_signed = true;
	}
#line 1570 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 34:
#line 198 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->start_offset = (yyvsp[0].integer);
	}
#line 1578 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 35:
#line 201 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->port_id = (yyvsp[0].integer);
		current_wire->port_input = true;
		current_wire->port_output = false;
	}
#line 1588 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 36:
#line 206 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->port_id = (yyvsp[0].integer);
		current_wire->port_input = false;
		current_wire->port_output = true;
	}
#line 1598 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 37:
#line 211 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_wire->port_id = (yyvsp[0].integer);
		current_wire->port_input = true;
		current_wire->port_output = true;
	}
#line 1608 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 39:
#line 219 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_memory = new RTLIL::Memory;
		current_memory->attributes = attrbuf;
		attrbuf.clear();
	}
#line 1618 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 40:
#line 223 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (current_module->memories.count((yyvsp[-1].string)) != 0)
			rtlil_frontend_ilang_yyerror(stringf("ilang error: redefinition of memory %s.", (yyvsp[-1].string)).c_str());
		current_memory->name = (yyvsp[-1].string);
		current_module->memories[(yyvsp[-1].string)] = current_memory;
		free((yyvsp[-1].string));
	}
#line 1630 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 41:
#line 232 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_memory->width = (yyvsp[0].integer);
	}
#line 1638 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 42:
#line 235 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_memory->size = (yyvsp[0].integer);
	}
#line 1646 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 43:
#line 238 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_memory->start_offset = (yyvsp[0].integer);
	}
#line 1654 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 45:
#line 244 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (current_module->cell((yyvsp[-1].string)) != nullptr)
			rtlil_frontend_ilang_yyerror(stringf("ilang error: redefinition of cell %s.", (yyvsp[-1].string)).c_str());
		current_cell = current_module->addCell((yyvsp[-1].string), (yyvsp[-2].string));
		current_cell->attributes = attrbuf;
		attrbuf.clear();
		free((yyvsp[-2].string));
		free((yyvsp[-1].string));
	}
#line 1668 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 47:
#line 255 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_cell->parameters[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		free((yyvsp[-2].string));
		delete (yyvsp[-1].data);
	}
#line 1678 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 48:
#line 260 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_cell->parameters[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		current_cell->parameters[(yyvsp[-2].string)].flags |= RTLIL::CONST_FLAG_SIGNED;
		free((yyvsp[-2].string));
		delete (yyvsp[-1].data);
	}
#line 1689 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 49:
#line 266 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_cell->parameters[(yyvsp[-2].string)] = *(yyvsp[-1].data);
		current_cell->parameters[(yyvsp[-2].string)].flags |= RTLIL::CONST_FLAG_REAL;
		free((yyvsp[-2].string));
		delete (yyvsp[-1].data);
	}
#line 1700 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 50:
#line 272 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (current_cell->hasPort((yyvsp[-2].string)))
			rtlil_frontend_ilang_yyerror(stringf("ilang error: redefinition of cell port %s.", (yyvsp[-2].string)).c_str());
		current_cell->setPort((yyvsp[-2].string), *(yyvsp[-1].sigspec));
		delete (yyvsp[-1].sigspec);
		free((yyvsp[-2].string));
	}
#line 1712 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 52:
#line 282 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (current_module->processes.count((yyvsp[-1].string)) != 0)
			rtlil_frontend_ilang_yyerror(stringf("ilang error: redefinition of process %s.", (yyvsp[-1].string)).c_str());
		current_process = new RTLIL::Process;
		current_process->name = (yyvsp[-1].string);
		current_process->attributes = attrbuf;
		current_module->processes[(yyvsp[-1].string)] = current_process;
		switch_stack.clear();
		switch_stack.push_back(&current_process->root_case.switches);
		case_stack.clear();
		case_stack.push_back(&current_process->root_case);
		attrbuf.clear();
		free((yyvsp[-1].string));
	}
#line 1731 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 54:
#line 298 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		RTLIL::SwitchRule *rule = new RTLIL::SwitchRule;
		rule->signal = *(yyvsp[-1].sigspec);
		rule->attributes = attrbuf;
		switch_stack.back()->push_back(rule);
		attrbuf.clear();
		delete (yyvsp[-1].sigspec);
	}
#line 1744 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 58:
#line 312 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		RTLIL::CaseRule *rule = new RTLIL::CaseRule;
		rule->attributes = attrbuf;
		switch_stack.back()->back()->cases.push_back(rule);
		switch_stack.push_back(&rule->switches);
		case_stack.push_back(rule);
		attrbuf.clear();
	}
#line 1757 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 59:
#line 319 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		switch_stack.pop_back();
		case_stack.pop_back();
	}
#line 1766 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 61:
#line 326 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		case_stack.back()->compare.push_back(*(yyvsp[0].sigspec));
		delete (yyvsp[0].sigspec);
	}
#line 1775 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 62:
#line 330 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		case_stack.back()->compare.push_back(*(yyvsp[0].sigspec));
		delete (yyvsp[0].sigspec);
	}
#line 1784 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 68:
#line 343 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (attrbuf.size() != 0)
			rtlil_frontend_ilang_yyerror("dangling attribute");
		case_stack.back()->actions.push_back(RTLIL::SigSig(*(yyvsp[-2].sigspec), *(yyvsp[-1].sigspec)));
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].sigspec);
	}
#line 1796 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 69:
#line 352 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType((yyvsp[-2].integer));
		rule->signal = *(yyvsp[-1].sigspec);
		current_process->syncs.push_back(rule);
		delete (yyvsp[-1].sigspec);
	}
#line 1808 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 71:
#line 359 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType::STa;
		rule->signal = RTLIL::SigSpec();
		current_process->syncs.push_back(rule);
	}
#line 1819 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 73:
#line 365 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType::STg;
		rule->signal = RTLIL::SigSpec();
		current_process->syncs.push_back(rule);
	}
#line 1830 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 75:
#line 371 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		RTLIL::SyncRule *rule = new RTLIL::SyncRule;
		rule->type = RTLIL::SyncType::STi;
		rule->signal = RTLIL::SigSpec();
		current_process->syncs.push_back(rule);
	}
#line 1841 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 78:
#line 380 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    { (yyval.integer) = RTLIL::ST0; }
#line 1847 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 79:
#line 381 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    { (yyval.integer) = RTLIL::ST1; }
#line 1853 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 80:
#line 382 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    { (yyval.integer) = RTLIL::STp; }
#line 1859 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 81:
#line 383 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    { (yyval.integer) = RTLIL::STn; }
#line 1865 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 82:
#line 384 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    { (yyval.integer) = RTLIL::STe; }
#line 1871 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 83:
#line 387 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		current_process->syncs.back()->actions.push_back(RTLIL::SigSig(*(yyvsp[-2].sigspec), *(yyvsp[-1].sigspec)));
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].sigspec);
	}
#line 1881 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 85:
#line 395 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		char *ep;
		int width = strtol((yyvsp[0].string), &ep, 10);
		std::list<RTLIL::State> bits;
		while (*(++ep) != 0) {
			RTLIL::State bit = RTLIL::Sx;
			switch (*ep) {
			case '0': bit = RTLIL::S0; break;
			case '1': bit = RTLIL::S1; break;
			case 'x': bit = RTLIL::Sx; break;
			case 'z': bit = RTLIL::Sz; break;
			case '-': bit = RTLIL::Sa; break;
			case 'm': bit = RTLIL::Sm; break;
			}
			bits.push_front(bit);
		}
		if (bits.size() == 0)
			bits.push_back(RTLIL::Sx);
		while ((int)bits.size() < width) {
			RTLIL::State bit = bits.back();
			if (bit == RTLIL::S1)
				bit = RTLIL::S0;
			bits.push_back(bit);
		}
		while ((int)bits.size() > width)
			bits.pop_back();
		(yyval.data) = new RTLIL::Const;
		for (auto it = bits.begin(); it != bits.end(); it++)
			(yyval.data)->bits.push_back(*it);
		free((yyvsp[0].string));
	}
#line 1917 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 86:
#line 426 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.data) = new RTLIL::Const((yyvsp[0].integer), 32);
	}
#line 1925 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 87:
#line 429 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.data) = new RTLIL::Const((yyvsp[0].string));
		free((yyvsp[0].string));
	}
#line 1934 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 88:
#line 435 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.sigspec) = new RTLIL::SigSpec(*(yyvsp[0].data));
		delete (yyvsp[0].data);
	}
#line 1943 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 89:
#line 439 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (current_module->wire((yyvsp[0].string)) == nullptr)
			rtlil_frontend_ilang_yyerror(stringf("ilang error: wire %s not found", (yyvsp[0].string)).c_str());
		(yyval.sigspec) = new RTLIL::SigSpec(current_module->wire((yyvsp[0].string)));
		free((yyvsp[0].string));
	}
#line 1954 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 90:
#line 445 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-1].integer) >= (yyvsp[-3].sigspec)->size() || (yyvsp[-1].integer) < 0)
			rtlil_frontend_ilang_yyerror("bit index out of range");
		(yyval.sigspec) = new RTLIL::SigSpec((yyvsp[-3].sigspec)->extract((yyvsp[-1].integer)));
		delete (yyvsp[-3].sigspec);
	}
#line 1965 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 91:
#line 451 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if ((yyvsp[-3].integer) >= (yyvsp[-5].sigspec)->size() || (yyvsp[-3].integer) < 0 || (yyvsp[-3].integer) < (yyvsp[-1].integer))
			rtlil_frontend_ilang_yyerror("invalid slice");
		(yyval.sigspec) = new RTLIL::SigSpec((yyvsp[-5].sigspec)->extract((yyvsp[-1].integer), (yyvsp[-3].integer) - (yyvsp[-1].integer) + 1));
		delete (yyvsp[-5].sigspec);
	}
#line 1976 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 92:
#line 457 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.sigspec) = (yyvsp[-1].sigspec);
	}
#line 1984 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 93:
#line 462 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.rsigspec)->push_back(*(yyvsp[0].sigspec));
		delete (yyvsp[0].sigspec);
	}
#line 1993 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 94:
#line 466 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.rsigspec) = new std::vector<RTLIL::SigSpec>;
	}
#line 2001 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 95:
#line 470 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		(yyval.sigspec) = new RTLIL::SigSpec;
		for (auto it = (yyvsp[0].rsigspec)->rbegin(); it != (yyvsp[0].rsigspec)->rend(); it++)
			(yyval.sigspec)->append(*it);
		delete (yyvsp[0].rsigspec);
	}
#line 2012 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;

  case 96:
#line 478 "frontends/ilang/ilang_parser.y" /* yacc.c:1646  */
    {
		if (attrbuf.size() != 0)
			rtlil_frontend_ilang_yyerror("dangling attribute");
		current_module->connect(*(yyvsp[-2].sigspec), *(yyvsp[-1].sigspec));
		delete (yyvsp[-2].sigspec);
		delete (yyvsp[-1].sigspec);
	}
#line 2024 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
    break;


#line 2028 "frontends/ilang/ilang_parser.tab.cc" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
