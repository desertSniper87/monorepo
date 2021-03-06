/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "grammar.y" /* yacc.c:339  */

#include <stdio.h>
#include "SymbolTable.h"
#define YYSTYPE SymbolInfo*
#include "bits/stdc++.h"
#include "iostream"

FILE *logout;
FILE *tokenout;
FILE *parseLog;

SymbolTable parser_table;
stringstream ss;
extern FILE *yyin;
extern int line_count;
int tempCount = 0, labelCount = 0;

extern "C"
{
    extern int yylex(void);
    extern int yyparse(void);
}
extern char yytext[];
extern int column;
extern ofstream assembly;

//ofstream ir_code_stream; 

void yyerror(char *s)
{
	fprintf(stderr,"At Line %d, ERROR-> %s\n",line_count,s);
	return;
}

void adele(){
	assembly<< "Hello from the other side"<< endl;
}


#line 106 "grammar.tab.c" /* yacc.c:339  */

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
   by #include "grammar.tab.h".  */
#ifndef YY_YY_GRAMMAR_TAB_H_INCLUDED
# define YY_YY_GRAMMAR_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    SEMICOLON = 258,
    INT = 259,
    FLOAT = 260,
    CHAR = 261,
    COMMA = 262,
    TYPEDEF = 263,
    ID = 264,
    LTHIRD = 265,
    CONST = 266,
    RTHIRD = 267,
    FOR = 268,
    LPAREN = 269,
    RPAREN = 270,
    IF = 271,
    STATIC = 272,
    ELSE = 273,
    WHILE = 274,
    PRINTLN = 275,
    RETURN = 276,
    ASSIGNOP = 277,
    LOGICOP = 278,
    REGISTER = 279,
    RELOP = 280,
    ADDOP = 281,
    MULOP = 282,
    NOT = 283,
    INCOP = 284,
    DECOP = 285,
    CONST_CHAR = 286,
    CONST_INT = 287,
    CONST_FLOAT = 288,
    MAIN = 289,
    TYPE_NAME = 290,
    VOLATILE = 291,
    AUTO = 292,
    VOID = 293,
    SHORT = 294,
    LONG = 295,
    DOUBLE = 296,
    SIGNED = 297,
    UNSIGNED = 298,
    EXTERN = 299,
    ENUM = 300,
    IDENTIFIER = 301,
    STRUCT = 302,
    UNION = 303,
    HEADER = 304,
    NUMBER = 305,
    OR_OP = 306,
    AND_OP = 307,
    EQ_OP = 308,
    GE_OP = 309,
    LEFT_OP = 310,
    LE_OP = 311,
    NE_OP = 312,
    RIGHT_OP = 313
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_GRAMMAR_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 216 "grammar.tab.c" /* yacc.c:358  */

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
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

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
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   925

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  80
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  60
/* YYNRULES -- Number of rules.  */
#define YYNRULES  182
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  303

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   313

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    74,    66,     2,
      75,    76,    72,    70,    62,    71,     2,    73,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    67,    61,
      68,    63,    69,    77,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    78,     2,    79,    65,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    59,    64,    60,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    51,    51,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    66,    67,    71,    73,    74,    75,    76,
      81,    82,    86,    87,    91,    92,    93,    94,    98,    99,
     103,   104,   105,   106,   107,   108,   112,   113,   117,   118,
     122,   123,   124,   125,   126,   127,   128,   129,   132,   133,
     137,   141,   142,   146,   147,   151,   152,   156,   157,   161,
     162,   166,   167,   171,   172,   173,   177,   178,   183,   184,
     185,   186,   187,   192,   196,   197,   201,   202,   207,   208,
     211,   212,   215,   216,   219,   220,   221,   225,   226,   230,
     231,   232,   236,   237,   241,   242,   243,   247,   248,   252,
     253,   254,   255,   256,   261,   262,   263,   266,   267,   268,
     269,   270,   271,   272,   277,   278,   282,   283,   284,   288,
     292,   293,   297,   298,   299,   303,   304,   305,   306,   310,
     311,   315,   316,   321,   322,   323,   324,   330,   331,   332,
     336,   337,   342,   343,   347,   348,   353,   357,   358,   362,
     363,   367,   368,   372,   373,   374,   378,   379,   383,   384,
     385,   389,   390,   391,   392,   393,   394,   395,   396,   397,
     401,   402,   403,   404,   408,   409,   413,   414,   415,   416,
     417,   418,   419
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SEMICOLON", "INT", "FLOAT", "CHAR",
  "COMMA", "TYPEDEF", "ID", "LTHIRD", "CONST", "RTHIRD", "FOR", "LPAREN",
  "RPAREN", "IF", "STATIC", "ELSE", "WHILE", "PRINTLN", "RETURN",
  "ASSIGNOP", "LOGICOP", "REGISTER", "RELOP", "ADDOP", "MULOP", "NOT",
  "INCOP", "DECOP", "CONST_CHAR", "CONST_INT", "CONST_FLOAT", "MAIN",
  "TYPE_NAME", "VOLATILE", "AUTO", "VOID", "SHORT", "LONG", "DOUBLE",
  "SIGNED", "UNSIGNED", "EXTERN", "ENUM", "IDENTIFIER", "STRUCT", "UNION",
  "HEADER", "NUMBER", "OR_OP", "AND_OP", "EQ_OP", "GE_OP", "LEFT_OP",
  "LE_OP", "NE_OP", "RIGHT_OP", "'{'", "'}'", "';'", "','", "'='", "'|'",
  "'^'", "'&'", "':'", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "'('", "')'", "'?'", "'['", "']'", "$accept", "Program",
  "type_specifier", "compound_statement", "declaration_list",
  "statement_list", "declaration", "declaration_specifiers",
  "init_declarator_list", "init_declarator", "statement",
  "expression_statement", "struct_declaration", "struct_declaration_list",
  "variable", "expression", "logic_expression", "rel_expression",
  "type_qualifier", "initializer", "initializer_list",
  "storage_class_specifier", "assignment_operator",
  "inclusive_or_expression", "exclusive_or_expression",
  "type_qualifier_list", "simple_expression", "term", "unary_expression",
  "and_expression", "equality_expression", "struct_or_union",
  "struct_declarator", "identifier_list", "relational_expression",
  "shift_expression", "factor", "parameter_list", "parameter_declaration",
  "parameter_type_list", "logical_and_expression", "additive_expression",
  "multiplicative_expression", "cast_expression", "type_name",
  "specifier_qualifier_list", "struct_or_union_specifier",
  "struct_declarator_list", "enumerator_list", "logical_or_expression",
  "constant_expression", "conditional_expression", "assignment_expression",
  "enum_specifier", "enumerator", "abstract_declarator",
  "direct_abstract_declarator", "pointer", "declarator",
  "direct_declarator", YY_NULLPTR
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
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   123,
     125,    59,    44,    61,   124,    94,    38,    58,    60,    62,
      43,    45,    42,    47,    37,    40,    41,    63,    91,    93
};
# endif

#define YYPACT_NINF -219

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-219)))

#define YYTABLE_NINF -83

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      31,    25,    65,    73,  -219,    68,    71,   404,  -219,  -219,
    -219,  -219,  -219,  -219,   679,  -219,    96,   753,   104,  -219,
     128,   139,   753,  -219,   495,   495,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,   -21,
    -219,  -219,  -219,   877,   863,  -219,   344,   540,  -219,   -15,
    -219,  -219,   126,    17,  -219,   135,   863,   863,   103,   110,
      18,   163,   311,   143,   -12,    -9,   131,   155,   152,   142,
     134,    62,  -219,  -219,   -19,  -219,  -219,  -219,   761,   630,
     204,     8,   753,   753,   220,    24,  -219,  -219,  -219,   177,
     201,   877,   877,   195,    45,  -219,   266,  -219,   575,  -219,
    -219,  -219,  -219,  -219,    -6,    99,   180,  -219,   -24,   214,
     156,   495,  -219,   753,   495,  -219,  -219,   753,   753,   495,
     495,   495,  -219,   753,   753,   753,   753,   219,   877,   753,
     753,   753,   753,   753,   753,  -219,  -219,   753,   753,   753,
     753,   753,   753,   753,   753,   271,    16,   630,   753,  -219,
      27,    37,   265,  -219,   201,   221,    49,  -219,  -219,  -219,
     753,   268,   170,  -219,   160,   162,   276,  -219,  -219,    -6,
    -219,   211,  -219,    99,   156,   665,   449,   639,  -219,  -219,
     226,  -219,  -219,   110,  -219,   143,   267,   163,  -219,  -219,
     -12,   131,   131,   877,  -219,   802,   105,   155,   155,   155,
     155,   134,   134,   103,    62,    62,  -219,  -219,  -219,   142,
      64,  -219,  -219,   753,   610,   610,   292,    92,   753,  -219,
     201,  -219,  -219,    50,   234,  -219,   223,   225,  -219,   238,
    -219,   494,   650,   160,   270,  -219,  -219,  -219,  -219,   665,
    -219,  -219,  -219,  -219,   -29,   242,  -219,   240,   818,  -219,
    -219,   753,  -219,   190,   253,   753,    59,   303,  -219,  -219,
    -219,  -219,  -219,   222,  -219,    69,  -219,   863,  -219,  -219,
    -219,  -219,   246,  -219,   245,   313,  -219,   188,   281,  -219,
    -219,  -219,  -219,  -219,  -219,   105,   753,  -219,   610,   610,
    -219,  -219,  -219,  -219,  -219,   745,  -219,  -219,  -219,  -219,
    -219,  -219,  -219
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     0,    15,     0,     2,    48,
       6,     8,     4,    68,    26,    61,     0,     0,     0,    70,
       0,     0,     0,    72,     0,     0,   111,   109,   110,    14,
      62,    71,     3,     5,     7,     9,    10,    11,    69,     0,
      92,    93,    19,     0,    32,    41,     0,     0,    20,     0,
      22,    40,   107,     0,    55,    57,    34,    30,   120,    74,
      59,    80,   129,    76,    87,     0,    89,    99,    86,   144,
     104,   122,   125,    12,   147,   151,   149,    13,     0,     0,
      53,     0,     0,     0,     0,     0,   107,    84,    85,   155,
       0,   134,   136,     0,   131,    33,     0,    17,     0,    21,
      16,    23,   176,    28,   170,     0,     0,    36,     0,    38,
     175,     0,    49,     0,     0,    35,    31,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,   139,     0,     0,
       0,     0,     0,     0,     0,   112,   113,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   108,
       0,     0,     0,    47,     0,   156,     0,   142,   133,   135,
       0,     0,     0,   132,   159,   158,    24,    18,    78,   171,
     172,     0,    29,     0,   174,     0,     0,     0,    56,    82,
     129,   150,    58,    75,   129,    77,    60,    81,    83,   152,
      88,    90,    91,     0,    51,     0,     0,   103,   102,   100,
     101,   105,   106,   121,   123,   124,   126,   127,   128,   145,
       0,    27,    54,     0,    15,    15,     0,     0,     0,   153,
       0,   130,   166,   118,   119,   114,     0,     0,   162,     0,
     146,     0,     0,   160,     0,    79,   173,   177,    37,     0,
      39,    63,    97,   182,     0,     0,   179,     0,     0,   138,
      52,     0,   140,     0,    94,     0,     0,    43,    45,    46,
     154,   157,   143,     0,   117,   158,   116,     0,   167,   161,
     163,   168,     0,   164,     0,     0,    66,     0,     0,   181,
     180,   178,   137,    95,    50,     0,     0,   148,    15,    15,
     115,   169,   165,    25,    64,     0,    98,   141,    96,    42,
      44,    65,    67
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -219,  -219,   -30,   322,  -219,   283,   284,   132,  -219,   158,
     -45,   -65,  -179,   140,     2,    12,   224,   218,   -36,  -218,
    -219,  -219,  -219,   202,   228,  -219,   235,   236,    -7,   241,
     217,  -219,    57,  -219,    98,    90,  -219,  -219,    95,  -164,
     247,   120,   117,    51,  -219,    11,  -219,  -219,   212,  -219,
    -113,   -89,  -112,  -219,   147,   -91,  -157,   -68,   -99,  -104
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    44,    45,    46,    47,    48,   223,   106,   107,
      50,    51,   194,   195,    86,    53,    54,    55,    56,   240,
     277,    57,   123,    58,    59,   169,    60,    61,   184,    63,
      64,    65,   252,   244,    66,    67,    68,   224,   225,   226,
      69,    70,    71,    72,    93,   196,    73,   253,   156,    74,
     229,    75,    76,    77,   157,   227,   164,   108,   109,   110
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      62,   181,   101,   163,   174,    15,   171,    92,   233,    52,
      62,   189,   245,    91,   147,    62,   250,    87,    88,    52,
     112,   276,   102,   149,    52,    89,   165,   153,   212,    81,
      30,   102,   143,   278,    85,     1,   170,   127,    90,    62,
      62,   125,   214,   119,   120,   126,   103,   279,    52,    52,
     128,   105,   215,   101,    94,    92,    92,   104,   144,     3,
     105,    91,    91,   241,   247,     4,   104,   272,   168,   250,
     113,    62,    62,   230,   288,    62,    62,   302,   113,   113,
      52,    52,   213,     6,    52,    52,   113,     5,   230,   113,
     146,    62,    92,   165,   150,   151,   102,   254,    91,   113,
      52,   236,   158,   159,   179,   261,   180,   179,   233,   219,
      79,   220,   179,   179,   188,   102,   180,   104,    82,   274,
     161,   113,   104,   162,   266,   263,   113,   241,   162,   230,
       7,   255,   264,   235,   140,   141,   142,    62,   283,    49,
      62,    62,    83,   230,   263,   102,    52,   162,   111,    52,
      52,   102,   260,    84,   220,   265,   210,    92,   114,    92,
     146,   174,   230,    91,   171,    91,   287,   117,   180,   257,
     258,   104,   251,   298,   105,   118,    95,   104,    49,    80,
     105,   135,   136,   241,    17,   129,   254,   130,   115,   116,
     121,   206,   207,   208,   137,   265,    24,   230,    25,   131,
     132,    26,    27,    28,   138,   139,    62,    62,    62,   124,
     133,   221,    92,   134,   148,    52,    52,    52,    91,   197,
     198,   199,   200,   191,   192,   256,    10,    11,    12,   152,
      13,   176,   180,    15,   177,   231,   154,   161,   232,    19,
     162,   172,   173,   299,   300,    43,    23,   155,   294,   228,
     295,   284,   285,   201,   202,   204,   205,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,   102,    40,
      41,   160,    10,    11,    12,   166,    13,   175,   193,    15,
     216,    62,    62,   211,   218,    19,   234,   237,   180,   122,
      52,    52,    23,   120,   104,   259,   267,   263,   222,   268,
     162,   269,   275,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,   -82,    40,    41,   270,   280,   281,
     286,   289,   291,   -82,   292,   293,   -82,   296,     8,    98,
      99,   238,   182,   248,   -82,   178,   -82,   -82,   -82,   203,
     104,   190,   297,   161,   222,   183,   162,     9,    10,    11,
      12,    96,    13,    80,   186,    15,   187,    16,    17,   185,
      18,    19,   290,    20,    21,    22,   217,   262,    23,     0,
      24,     0,    25,   -82,   122,    26,    27,    28,   -82,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     209,    40,    41,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     7,    97,     0,     0,     9,    10,    11,
      12,     0,    13,    14,     0,    15,     0,    16,    17,    43,
      18,    19,     0,    20,    21,    22,     0,     0,    23,     0,
      24,     0,    25,     0,     0,    26,    27,    28,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    10,    11,    12,     0,    13,     0,     0,
      15,     0,     0,     7,    42,     0,    19,     0,     0,     0,
       0,     0,     0,    23,     0,     0,     0,     0,     0,    43,
       0,     0,     0,     0,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,   242,    40,    41,    10,    11,
      12,     0,    13,     0,    80,    15,     0,     0,     0,    17,
       0,    19,     0,     0,     0,     0,     0,     0,    23,     0,
       0,    24,     0,    25,     0,   243,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,     9,     0,     0,     0,     0,     0,    80,
       0,     0,     0,    16,    17,     0,    18,     0,     0,    20,
      21,    22,     0,     0,     0,     0,    24,     0,    25,     0,
     271,    26,    27,    28,     0,     0,     0,     0,     9,     0,
       0,     0,     0,     0,    80,     0,     0,     0,    16,    17,
       0,    18,     0,     0,    20,    21,    22,     0,     0,     7,
     100,    24,     0,    25,     0,     0,    26,    27,    28,     0,
       0,     0,     0,     9,     0,    43,     0,     0,     0,    80,
       0,     0,     0,    16,    17,     0,    18,     0,     0,    20,
      21,    22,     0,     9,     7,   167,    24,     0,    25,    80,
       0,    26,    27,    28,    17,     0,     0,     0,    80,     0,
      43,     0,     0,    17,     0,     0,    24,     0,    25,    80,
       0,    26,    27,    28,    17,    24,     0,    25,     0,     7,
      26,    27,    28,     0,    80,     0,    24,     0,    25,    17,
       0,    26,    27,    28,     0,    43,     0,     0,     0,    78,
       0,    24,     0,    25,     0,     0,    26,    27,    28,     0,
       0,   -53,   -53,     0,   -53,    43,   -53,     0,   -53,   -53,
       0,     0,     0,     0,    43,     0,     0,     0,   246,     0,
       0,     0,     0,     0,   239,    43,     0,     0,     0,   273,
     -53,   -53,   -53,   -53,   -53,   -53,   -53,   -53,     0,     0,
      43,   -53,   -53,   -53,   -53,   -53,     0,   -53,   -53,   -53,
     -53,   -53,   -53,   -53,    80,     0,   -53,     0,     0,    17,
       0,     0,    80,     0,     0,     0,     0,    17,     0,     0,
      80,    24,     0,    25,     0,    17,    26,    27,    28,    24,
       0,    25,     0,     0,    26,    27,    28,    24,     0,    25,
       0,     0,    26,   145,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   239,   301,    10,    11,    12,     0,
       0,     0,     0,    15,     0,     0,     0,     0,     0,     0,
      43,     0,    10,    11,    12,     0,     0,     0,    43,    15,
       0,     0,     0,     0,     0,     0,    43,    29,    30,     0,
      32,    33,    34,    35,    36,    37,     0,    39,     0,    40,
      41,     0,     0,    29,    30,     0,    32,    33,    34,    35,
      36,    37,   249,    39,     0,    40,    41,    10,    11,    12,
       0,    13,     0,     0,    15,     0,     0,     0,   282,     0,
      19,    10,    11,    12,     0,     0,     0,    23,    15,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    29,    30,     0,    32,    33,    34,    35,    36,
      37,     0,    39,     0,    40,    41
};

static const yytype_int16 yycheck[] =
{
       7,   113,    47,    94,   108,    11,   105,    43,   165,     7,
      17,   123,   176,    43,    79,    22,   195,    24,    25,    17,
       3,   239,    46,    15,    22,    46,    94,     3,    12,    17,
      36,    46,    51,    62,    22,     4,   104,    46,    59,    46,
      47,    53,    15,    25,    26,    57,    61,    76,    46,    47,
      59,    75,    15,    98,    43,    91,    92,    72,    77,    34,
      75,    91,    92,   175,   177,     0,    72,   231,   104,   248,
      62,    78,    79,   162,    15,    82,    83,   295,    62,    62,
      78,    79,   147,    15,    82,    83,    62,    14,   177,    62,
      78,    98,   128,   161,    82,    83,    46,   196,   128,    62,
      98,   169,    91,    92,   111,   218,   113,   114,   265,    60,
      14,    62,   119,   120,   121,    46,   123,    72,    14,   232,
      75,    62,    72,    78,   223,    75,    62,   239,    78,   218,
      59,    67,   223,   169,    72,    73,    74,   144,   251,     7,
     147,   148,    14,   232,    75,    46,   144,    78,    22,   147,
     148,    46,    60,    14,    62,   223,   144,   193,    23,   195,
     148,   265,   251,   193,   263,   195,   255,    64,   175,   214,
     215,    72,    67,   286,    75,    65,    44,    72,    46,     9,
      75,    29,    30,   295,    14,    54,   285,    56,    56,    57,
      27,   140,   141,   142,    52,   263,    26,   286,    28,    68,
      69,    31,    32,    33,    70,    71,   213,   214,   215,    66,
      55,   160,   248,    58,    10,   213,   214,   215,   248,   129,
     130,   131,   132,   125,   126,   213,     4,     5,     6,     9,
       8,    75,   239,    11,    78,    75,    59,    75,    78,    17,
      78,    61,    62,   288,   289,    75,    24,    46,    60,    79,
      62,    61,    62,   133,   134,   138,   139,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    76,     4,     5,     6,     9,     8,    63,    59,    11,
      15,   288,   289,    12,    63,    17,    10,    76,   295,    63,
     288,   289,    24,    26,    72,     3,    62,    75,    76,    76,
      78,    76,    32,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     3,    47,    48,    79,    76,    79,
      67,    18,    76,    12,    79,    12,    15,    46,     6,    46,
      46,   173,   114,   193,    23,   111,    25,    26,    27,   137,
      72,   124,   285,    75,    76,   117,    78,     3,     4,     5,
       6,     7,     8,     9,   119,    11,   120,    13,    14,   118,
      16,    17,   267,    19,    20,    21,   154,   220,    24,    -1,
      26,    -1,    28,    62,    63,    31,    32,    33,    67,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     143,    47,    48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    60,    -1,    -1,     3,     4,     5,
       6,    -1,     8,     9,    -1,    11,    -1,    13,    14,    75,
      16,    17,    -1,    19,    20,    21,    -1,    -1,    24,    -1,
      26,    -1,    28,    -1,    -1,    31,    32,    33,    -1,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    48,     4,     5,     6,    -1,     8,    -1,    -1,
      11,    -1,    -1,    59,    60,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,    -1,    75,
      -1,    -1,    -1,    -1,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,     4,     5,
       6,    -1,     8,    -1,     9,    11,    -1,    -1,    -1,    14,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    26,    -1,    28,    -1,    76,    31,    32,    33,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    48,     3,    -1,    -1,    -1,    -1,    -1,     9,
      -1,    -1,    -1,    13,    14,    -1,    16,    -1,    -1,    19,
      20,    21,    -1,    -1,    -1,    -1,    26,    -1,    28,    -1,
      76,    31,    32,    33,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,     9,    -1,    -1,    -1,    13,    14,
      -1,    16,    -1,    -1,    19,    20,    21,    -1,    -1,    59,
      60,    26,    -1,    28,    -1,    -1,    31,    32,    33,    -1,
      -1,    -1,    -1,     3,    -1,    75,    -1,    -1,    -1,     9,
      -1,    -1,    -1,    13,    14,    -1,    16,    -1,    -1,    19,
      20,    21,    -1,     3,    59,    60,    26,    -1,    28,     9,
      -1,    31,    32,    33,    14,    -1,    -1,    -1,     9,    -1,
      75,    -1,    -1,    14,    -1,    -1,    26,    -1,    28,     9,
      -1,    31,    32,    33,    14,    26,    -1,    28,    -1,    59,
      31,    32,    33,    -1,     9,    -1,    26,    -1,    28,    14,
      -1,    31,    32,    33,    -1,    75,    -1,    -1,    -1,    10,
      -1,    26,    -1,    28,    -1,    -1,    31,    32,    33,    -1,
      -1,    22,    23,    -1,    25,    75,    27,    -1,    29,    30,
      -1,    -1,    -1,    -1,    75,    -1,    -1,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    59,    75,    -1,    -1,    -1,    79,
      51,    52,    53,    54,    55,    56,    57,    58,    -1,    -1,
      75,    62,    63,    64,    65,    66,    -1,    68,    69,    70,
      71,    72,    73,    74,     9,    -1,    77,    -1,    -1,    14,
      -1,    -1,     9,    -1,    -1,    -1,    -1,    14,    -1,    -1,
       9,    26,    -1,    28,    -1,    14,    31,    32,    33,    26,
      -1,    28,    -1,    -1,    31,    32,    33,    26,    -1,    28,
      -1,    -1,    31,    32,    33,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    59,    60,     4,     5,     6,    -1,
      -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      75,    -1,     4,     5,     6,    -1,    -1,    -1,    75,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    75,    35,    36,    -1,
      38,    39,    40,    41,    42,    43,    -1,    45,    -1,    47,
      48,    -1,    -1,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    60,    45,    -1,    47,    48,     4,     5,     6,
      -1,     8,    -1,    -1,    11,    -1,    -1,    -1,    60,    -1,
      17,     4,     5,     6,    -1,    -1,    -1,    24,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      47,    48,    35,    36,    -1,    38,    39,    40,    41,    42,
      43,    -1,    45,    -1,    47,    48
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,    81,    34,     0,    14,    15,    59,    83,     3,
       4,     5,     6,     8,     9,    11,    13,    14,    16,    17,
      19,    20,    21,    24,    26,    28,    31,    32,    33,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      47,    48,    60,    75,    82,    83,    84,    85,    86,    87,
      90,    91,    94,    95,    96,    97,    98,   101,   103,   104,
     106,   107,   108,   109,   110,   111,   114,   115,   116,   120,
     121,   122,   123,   126,   129,   131,   132,   133,    10,    14,
       9,    95,    14,    14,    14,    95,    94,   108,   108,    46,
      59,    82,    98,   124,   125,    87,     7,    60,    85,    86,
      60,    90,    46,    61,    72,    75,    88,    89,   137,   138,
     139,    22,     3,    62,    23,    87,    87,    64,    65,    25,
      26,    27,    63,   102,    66,    53,    57,    46,    59,    54,
      56,    68,    69,    55,    58,    29,    30,    52,    70,    71,
      72,    73,    74,    51,    77,    32,    95,    91,    10,    15,
      95,    95,     9,     3,    59,    46,   128,   134,   125,   125,
      76,    75,    78,   135,   136,   137,     9,    60,    98,   105,
     137,   138,    61,    62,   139,    63,    75,    78,    96,   108,
     108,   132,    97,   104,   108,   109,   106,   107,   108,   132,
     110,   114,   114,    59,    92,    93,   125,   115,   115,   115,
     115,   121,   121,   103,   122,   122,   123,   123,   123,   120,
      95,    12,    12,    91,    15,    15,    15,   128,    63,    60,
      62,   123,    76,    87,   117,   118,   119,   135,    79,   130,
     131,    75,    78,   136,    10,    98,   137,    76,    89,    59,
      99,   132,    46,    76,   113,   119,    79,   130,    93,    60,
      92,    67,   112,   127,   138,    67,    95,    90,    90,     3,
      60,   130,   134,    75,   135,   137,   138,    62,    76,    76,
      79,    76,   119,    79,   130,    32,    99,   100,    62,    76,
      76,    79,    60,   130,    61,    62,    67,   131,    15,    18,
     118,    76,    79,    12,    60,    62,    46,   112,   130,    90,
      90,    60,    99
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    80,    81,    82,    82,    82,    82,    82,    82,    82,
      82,    82,    82,    82,    82,    83,    83,    83,    83,    83,
      84,    84,    85,    85,    84,    84,    84,    84,    86,    86,
      87,    87,    87,    87,    87,    87,    88,    88,    89,    89,
      90,    90,    90,    90,    90,    90,    90,    90,    91,    91,
      92,    93,    93,    94,    94,    95,    95,    96,    96,    97,
      97,    98,    98,    99,    99,    99,   100,   100,   101,   101,
     101,   101,   101,   102,   103,   103,   104,   104,   105,   105,
     106,   106,   107,   107,   108,   108,   108,   109,   109,   110,
     110,   110,   111,   111,   112,   112,   112,   113,   113,   114,
     114,   114,   114,   114,   115,   115,   115,   116,   116,   116,
     116,   116,   116,   116,   117,   117,   118,   118,   118,   119,
     120,   120,   121,   121,   121,   122,   122,   122,   122,   123,
     123,   124,   124,   125,   125,   125,   125,   126,   126,   126,
     127,   127,   128,   128,   129,   129,   130,   131,   131,    95,
      95,   132,   132,   133,   133,   133,   134,   134,   135,   135,
     135,   136,   136,   136,   136,   136,   136,   136,   136,   136,
     137,   137,   137,   137,   138,   138,   139,   139,   139,   139,
     139,   139,   139
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     5,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     3,     3,     4,     2,
       1,     2,     1,     2,     3,     6,     1,     4,     2,     3,
       1,     2,     1,     2,     1,     2,     1,     3,     1,     3,
       1,     1,     7,     5,     7,     5,     5,     3,     1,     2,
       3,     1,     2,     1,     4,     1,     3,     1,     3,     1,
       3,     1,     1,     1,     3,     4,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     3,     1,     3,     1,     2,
       1,     3,     1,     3,     2,     2,     1,     1,     3,     1,
       3,     3,     1,     1,     1,     2,     3,     1,     3,     1,
       3,     3,     3,     3,     1,     3,     3,     1,     3,     1,
       1,     1,     2,     2,     1,     3,     2,     2,     1,     1,
       1,     3,     1,     3,     3,     1,     3,     3,     3,     1,
       4,     1,     2,     2,     1,     2,     1,     5,     4,     2,
       1,     3,     1,     3,     1,     3,     1,     1,     5,     1,
       3,     1,     3,     4,     5,     2,     1,     3,     1,     1,
       2,     3,     2,     3,     3,     4,     2,     3,     3,     4,
       1,     2,     2,     3,     2,     1,     1,     3,     4,     3,
       4,     4,     3
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
#if YYDEBUG

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
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


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
      
#line 1659 "grammar.tab.c" /* yacc.c:1646  */
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
#line 422 "grammar.y" /* yacc.c:1906  */


int main(int argc,char *argv[]){
	logout= fopen("log.txt","w");
	tokenout= fopen("token.txt","w");
	parseLog = fopen("parseLog", "w");
	//assembly= fopen("assembly.asm", "W");
	ofstream assembly;
	assembly.open("assembly.asm");
	yylex();
	fclose(tokenout);
	fclose(logout);
	yyparse();
	fclose(parseLog);

	printf ("\nTotal line Count: %d\n", line_count);
	parser_table.dump();
	
	return 0;
}
