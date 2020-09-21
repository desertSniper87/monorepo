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
#line 1 "parser.y" /* yacc.c:339  */

#include <stdio.h>
#include "Symbol_table.h"
#define YYSTYPE Symbol_info*
#include "bits/stdc++.h"
#include "iostream"

#include "stdlib.h"
#include "string.h"

using namespace std;

FILE *logout;
FILE *tokenout;
FILE *parseLog;
FILE *asmout;

Symbol_table parser_table;
stringstream ss;
extern FILE *yyin;
extern int line_count;

int tempCount = 0, labelCount = 0;

extern int yylex(void);
extern int yyparse(void);

/*extern int error; //TODO: Error reporting*/

extern "C"
{
    //Kill me
}
extern char yytext[];
extern int column;

extern YYSTYPE yylval;

char *newLabel()
{
	char *lb= new char[4];
	strcpy(lb,"L");
	char b[3];
	sprintf(b,"%d", labelCount);
	labelCount++;
	strcat(lb,b);
	return lb;
}

char *newTemp()
{
	char *t= new char[4];
	strcpy(t,"t");
	char b[3];
	sprintf(b,"%d", tempCount);
	tempCount++;
	strcat(t,b);
	return t;
}

string getLabel()	//put type = "b" for byte
{
    stringstream ss;
    ss << "label" << labelCount++;
    return ss.str();
}

string getTemp(string type = "w")	//put type = "b" for byte
{
    stringstream ss;
    return ss.str();
}


//ofstream ir_code_stream; 

void yyerror(char *s)
{
    fprintf(stderr,"At Line %d, ERROR-> %s\n",line_count,s);
    fprintf(parseLog,"At Line %d, ERROR-> %s\n",line_count,s);
    return;
}

void adele(){
    cout << "Hello from the other side"<< endl;
}


#line 155 "y.tab.cpp" /* yacc.c:339  */

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
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.hpp".  */
#ifndef YY_YY_Y_TAB_HPP_INCLUDED
# define YY_YY_Y_TAB_HPP_INCLUDED
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
    LCURL = 263,
    RCURL = 264,
    ID = 265,
    LSQBRAC = 266,
    RSQBRAC = 267,
    FOR = 268,
    LPAREN = 269,
    RPAREN = 270,
    IF = 271,
    ELSE = 272,
    WHILE = 273,
    PRINTLN = 274,
    RETURN = 275,
    ASSIGNOP = 276,
    LOGICOP = 277,
    RELOP = 278,
    ADDOP = 279,
    MULOP = 280,
    NOT = 281,
    INCOP = 282,
    DECOP = 283,
    CONST_INT = 284,
    CONST_FLOAT = 285,
    CONST_CHAR = 286,
    VOID = 287
  };
#endif
/* Tokens.  */
#define SEMICOLON 258
#define INT 259
#define FLOAT 260
#define CHAR 261
#define COMMA 262
#define LCURL 263
#define RCURL 264
#define ID 265
#define LSQBRAC 266
#define RSQBRAC 267
#define FOR 268
#define LPAREN 269
#define RPAREN 270
#define IF 271
#define ELSE 272
#define WHILE 273
#define PRINTLN 274
#define RETURN 275
#define ASSIGNOP 276
#define LOGICOP 277
#define RELOP 278
#define ADDOP 279
#define MULOP 280
#define NOT 281
#define INCOP 282
#define DECOP 283
#define CONST_INT 284
#define CONST_FLOAT 285
#define CONST_CHAR 286
#define VOID 287

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_HPP_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 270 "y.tab.cpp" /* yacc.c:358  */

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
#define YYFINAL  11
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   164

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  33
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  23
/* YYNRULES -- Number of rules.  */
#define YYNRULES  64
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  116

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   287

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
      25,    26,    27,    28,    29,    30,    31,    32
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   101,   101,   107,   113,   120,   125,   130,   137,   143,
     173,   177,   181,   185,   189,   193,   196,   201,   209,   216,
     221,   226,   234,   240,   245,   250,   256,   262,   271,   276,
     281,   286,   318,   331,   349,   367,   373,   380,   385,   392,
     397,   409,   415,   436,   441,   492,   498,   538,   543,   574,
     579,   616,   622,   631,   638,   653,   664,   669,   674,   679,
     684,   701,   719,   725,   730
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SEMICOLON", "INT", "FLOAT", "CHAR",
  "COMMA", "LCURL", "RCURL", "ID", "LSQBRAC", "RSQBRAC", "FOR", "LPAREN",
  "RPAREN", "IF", "ELSE", "WHILE", "PRINTLN", "RETURN", "ASSIGNOP",
  "LOGICOP", "RELOP", "ADDOP", "MULOP", "NOT", "INCOP", "DECOP",
  "CONST_INT", "CONST_FLOAT", "CONST_CHAR", "VOID", "$accept", "start",
  "program", "unit", "func_declaration", "func_definition",
  "parameter_list", "compound_statement", "var_declaration",
  "type_specifier", "declaration_list", "statements", "statement",
  "expression_statement", "variable", "expression", "logic_expression",
  "rel_expression", "simple_expression", "term", "unary_expression",
  "factor", "argument_list", YY_NULLPTR
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
     285,   286,   287
};
# endif

#define YYPACT_NINF -58

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-58)))

#define YYTABLE_NINF -25

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
       5,   -58,   -58,   -58,    15,     5,   -58,   -58,   -58,   -58,
      17,   -58,   -58,     6,    48,    16,     5,   -58,    59,    46,
     -58,     7,    69,    74,   -58,     5,    10,    35,    58,    79,
     -58,    62,   -58,    78,   -58,   -58,   -58,    49,    84,   133,
      85,    89,    90,   133,   133,   133,   -58,   -58,   -58,   -58,
     -58,    99,    92,   -58,   -58,    86,   112,   -58,    95,    50,
      94,   -58,   -58,   -58,   133,   133,     2,   105,   133,   133,
     118,   126,    56,   -58,   -58,   120,   -58,   -58,   133,   -58,
     -58,   -58,   133,   133,   133,   133,   121,   -58,    41,     2,
     -58,   119,   124,   129,   -58,   -58,   -58,   113,    94,   -58,
     -58,   133,   -58,   133,   122,   122,   142,   -58,   134,   138,
     -58,   -58,   122,   122,   -58,   -58
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    19,    20,    21,     0,     2,     4,     6,     7,     5,
       0,     1,     3,    24,     0,     0,    15,    18,     0,     0,
      13,     0,    14,    22,    25,     0,     0,    12,     0,    11,
       8,     0,     9,     0,    10,    37,    17,    39,     0,     0,
       0,     0,     0,     0,     0,     0,    57,    58,    59,    30,
      28,     0,     0,    26,    29,    54,     0,    41,    43,    45,
      47,    49,    53,    23,     0,    64,     0,     0,     0,     0,
       0,     0,    54,    51,    52,    24,    16,    27,     0,    60,
      61,    38,     0,     0,     0,     0,     0,    63,     0,     0,
      56,     0,     0,     0,    36,    42,    44,    46,    48,    50,
      40,     0,    55,     0,     0,     0,     0,    62,     0,    32,
      34,    35,     0,     0,    31,    33
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -58,   -58,   -58,    34,   -58,   -58,   -58,   130,    19,    52,
     -58,   -58,   -51,   -55,   -42,   -39,   -57,    68,    75,    76,
     -38,   -58,   -58
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,     5,     6,     7,     8,    21,    49,    50,    51,
      14,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    88
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      67,    77,    72,    72,    71,    35,    73,    74,    87,     1,
       2,    89,    37,    30,    25,    11,    39,    15,    31,     9,
      16,    95,    26,    72,     9,    86,    44,    13,    45,    91,
      92,    46,    47,    48,   103,     9,    72,     3,   -24,    12,
      72,    72,    72,    72,   107,    19,    15,    99,   101,    16,
      20,    17,    10,   109,   110,    18,   102,    10,    24,    72,
      64,   114,   115,    65,   108,    35,     1,     2,    22,    23,
      31,    36,    37,    83,    84,    38,    39,    29,    40,    27,
      41,    42,    43,    79,    80,    28,    44,    33,    45,    34,
      63,    46,    47,    48,     3,    35,     1,     2,    66,    68,
      31,    76,    37,    69,    70,    38,    39,    78,    40,    75,
      41,    42,    43,    79,    80,    81,    44,    82,    45,    85,
      90,    46,    47,    48,     3,    35,     1,     2,    93,    94,
      31,    15,    37,   100,   104,    38,    39,    84,    40,   105,
      41,    42,    43,    37,   106,   111,    44,    39,    45,   112,
      96,    46,    47,    48,     3,   113,    32,    44,    97,    45,
      98,     0,    46,    47,    48
};

static const yytype_int8 yycheck[] =
{
      39,    52,    44,    45,    43,     3,    44,    45,    65,     4,
       5,    66,    10,     3,     7,     0,    14,    11,     8,     0,
      14,    78,    15,    65,     5,    64,    24,    10,    26,    68,
      69,    29,    30,    31,    89,    16,    78,    32,     3,     5,
      82,    83,    84,    85,   101,    29,    11,    85,     7,    14,
      16,     3,     0,   104,   105,     7,    15,     5,    12,   101,
      11,   112,   113,    14,   103,     3,     4,     5,    16,    10,
       8,     9,    10,    23,    24,    13,    14,    25,    16,    10,
      18,    19,    20,    27,    28,    11,    24,    29,    26,    10,
      12,    29,    30,    31,    32,     3,     4,     5,    14,    14,
       8,     9,    10,    14,    14,    13,    14,    21,    16,    10,
      18,    19,    20,    27,    28,     3,    24,    22,    26,    25,
      15,    29,    30,    31,    32,     3,     4,     5,    10,     3,
       8,    11,    10,    12,    15,    13,    14,    24,    16,    15,
      18,    19,    20,    10,    15,     3,    24,    14,    26,    15,
      82,    29,    30,    31,    32,    17,    26,    24,    83,    26,
      84,    -1,    29,    30,    31
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,     5,    32,    34,    35,    36,    37,    38,    41,
      42,     0,    36,    10,    43,    11,    14,     3,     7,    29,
      36,    39,    42,    10,    12,     7,    15,    10,    11,    42,
       3,     8,    40,    29,    10,     3,     9,    10,    13,    14,
      16,    18,    19,    20,    24,    26,    29,    30,    31,    40,
      41,    42,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    12,    11,    14,    14,    48,    14,    14,
      14,    48,    47,    53,    53,    10,     9,    45,    21,    27,
      28,     3,    22,    23,    24,    25,    48,    49,    55,    46,
      15,    48,    48,    10,     3,    49,    50,    51,    52,    53,
      12,     7,    15,    46,    15,    15,    15,    49,    48,    45,
      45,     3,    15,    17,    45,    45
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    33,    34,    35,    35,    36,    36,    36,    37,    38,
      39,    39,    39,    39,    39,    39,    40,    40,    41,    42,
      42,    42,    43,    43,    43,    43,    44,    44,    45,    45,
      45,    45,    45,    45,    45,    45,    45,    46,    46,    47,
      47,    48,    48,    49,    49,    50,    50,    51,    51,    52,
      52,    53,    53,    53,    54,    54,    54,    54,    54,    54,
      54,    54,    55,    55,    55
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     1,     6,     6,
       4,     3,     2,     1,     1,     0,     3,     2,     3,     1,
       1,     1,     3,     6,     1,     4,     1,     2,     1,     1,
       1,     7,     5,     7,     5,     5,     3,     1,     2,     1,
       4,     1,     3,     1,     3,     1,     3,     1,     3,     1,
       3,     2,     2,     1,     1,     4,     3,     1,     1,     1,
       2,     2,     3,     1,     0
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
        case 2:
#line 102 "parser.y" /* yacc.c:1646  */
    {
            fprintf(asmout, ((yyvsp[0])->code).c_str());
      }
#line 1439 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 3:
#line 108 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = (yyvsp[-1]);
            (yyval)->code += (yyvsp[0])->code;
            fprintf(parseLog, "GRAMMER RULE: program -> program unit\n"); 
        }
#line 1449 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 4:
#line 114 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = (yyvsp[0]);
            fprintf(parseLog, "GRAMMER RULE: program -> unit\n"); 
        }
#line 1458 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 5:
#line 121 "parser.y" /* yacc.c:1646  */
    {
            (yyval)  = (yyvsp[0]);
            fprintf(parseLog, "GRAMMER RULE: unit -> var_declaration\n"); 
     }
#line 1467 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 6:
#line 126 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = (yyvsp[0]);
            fprintf(parseLog, "GRAMMER RULE: unit -> func_declaration\n"); 
     }
#line 1476 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 7:
#line 131 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = (yyvsp[0]);
            fprintf(parseLog, "GRAMMER RULE: unit -> func_definition\n"); 
     }
#line 1485 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 8:
#line 138 "parser.y" /* yacc.c:1646  */
    {
                     fprintf(parseLog, "GRAMMER RULE: func_declaration -> type_specifier ID LPAREN parameter_list RPAREN SEMICOLON  \n"); 
                 }
#line 1493 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 9:
#line 144 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=new Symbol_info();
                    // Need to work on this

                    (yyval)->code+="\n;function definition";
                    (yyval)->code+="\nPROC "+(yyvsp[-4])->symbol+"\n\n";

                    if((yyvsp[-4])->symbol!="main")
                    {
                        (yyval)->code+="PUSH AX\n";
                        (yyval)->code+="PUSH BX\n";
                        (yyval)->code+="PUSH CX\n";
                        (yyval)->code+="PUSH DX\n";
                    }

                    (yyval)->code += (yyvsp[0])->code ;

                    if((yyvsp[-4])->symbol!="main") {
                        (yyval)->code+="POP DX\n";
                        (yyval)->code+="POP CX\n";
                        (yyval)->code+="POP BX\n";
                        (yyval)->code+="POP AX\n";
                    }

                    //Source of problem
                    fprintf(parseLog, "GRAMMER RULE: func_definition -> type_specifier ID LPAREN parameter_list RPAREN compound_statement  \n"); 
                 }
#line 1525 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 10:
#line 174 "parser.y" /* yacc.c:1646  */
    {
                    fprintf(parseLog, "GRAMMER RULE: parameter_list  -> parameter_list COMMA type_specifier ID  \n"); 
                }
#line 1533 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 11:
#line 178 "parser.y" /* yacc.c:1646  */
    {
                  fprintf(parseLog, "GRAMMER RULE: parameter_list -> parameter_list COMMA type_specifier	   \n"); 
                }
#line 1541 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 12:
#line 182 "parser.y" /* yacc.c:1646  */
    {
                    fprintf(parseLog, "GRAMMER RULE: parameter_list -> type_specifier ID  \n"); 
                }
#line 1549 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 13:
#line 186 "parser.y" /* yacc.c:1646  */
    {
                    fprintf(parseLog, "GRAMMER RULE: parameter_list -> unit  \n"); 
                }
#line 1557 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 14:
#line 190 "parser.y" /* yacc.c:1646  */
    {
                  fprintf(parseLog, "GRAMMER RULE: parameter_list -> type_specifier  \n"); 
                }
#line 1565 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 16:
#line 197 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = (yyvsp[-1]);
                    fprintf(parseLog, "GRAMMER RULE: compound_statement -> LCURL statements RCURL  \n"); 
                 }
#line 1574 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 17:
#line 202 "parser.y" /* yacc.c:1646  */
    {
                     fprintf(parseLog, "GRAMMER RULE: compound_statement -> LCURL RCURL  \n"); 
                     (yyval) = new Symbol_info("compound_statement", "dummy");
                     //TODO Make new Symbol_info
                 }
#line 1584 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 18:
#line 210 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = (yyvsp[-1]);
                    fprintf(parseLog, "GRAMMER RULE: var_declaration -> type_specifier declaration_list SEMICOLON\n"); 
                }
#line 1593 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 19:
#line 217 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = new Symbol_info("int", "KEYWORD");
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> INT\n"); 
                }
#line 1602 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 20:
#line 222 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = new Symbol_info("float", "KEYWORD");
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> FLOAT\n"); 
                }
#line 1611 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 21:
#line 227 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = new Symbol_info("void", "KEYWORD");
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> VOID\n"); 
                }
#line 1620 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 22:
#line 235 "parser.y" /* yacc.c:1646  */
    {
                    // These are the source of all problems
                     //TODO something in line 289
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> declaration_list COMMA ID \n"); 
                 }
#line 1630 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 23:
#line 241 "parser.y" /* yacc.c:1646  */
    {
                     //TODO
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> declaration_list COMMA ID LSQBRAC CONST_INT RSQBRAC \n"); 
                 }
#line 1639 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 24:
#line 246 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = new Symbol_info((yyvsp[0]));
                    fprintf(parseLog, "GRAMMER RULE: declaration_list -> ID \n"); 
                 }
#line 1648 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 25:
#line 251 "parser.y" /* yacc.c:1646  */
    {
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> ID LSQBRAC CONST_INT RSQBRAC \n"); 
                 }
#line 1656 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 26:
#line 257 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = (yyvsp[0]);
                     fprintf(parseLog, "GRAMMER RULE: statements -> statement  \n"); 

                 }
#line 1666 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 27:
#line 263 "parser.y" /* yacc.c:1646  */
    {
                     fprintf(parseLog, "GRAMMER RULE: statements -> statements statement  \n"); 
                     (yyval) = (yyvsp[-1]);
                     (yyval)->code += (yyvsp[0])->code;
                     delete (yyvsp[0]);
                 }
#line 1677 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 28:
#line 272 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=(yyvsp[0]);
                    fprintf(parseLog, "GRAMMER RULE: statement -> var_declaration  \n"); 
                 }
#line 1686 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 29:
#line 277 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = (yyvsp[0]);
                     fprintf(parseLog, "GRAMMER RULE: statement -> expression_statement  \n"); 
                 }
#line 1695 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 30:
#line 282 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = new Symbol_info((yyvsp[0]));
                     fprintf(parseLog, "GRAMMER RULE: statement -> compound_statement  \n"); 
                 }
#line 1704 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 31:
#line 287 "parser.y" /* yacc.c:1646  */
    {
					/*
                        for (i=0;i>5;i++) print(x)
						$3's code at first, which is already done by assigning $$=$3
						create two labels and append one of them in $$->code
						compare $4's symbol with 0
						if equal jump to 2nd label
						append $7's code
						append $5's code
						append the second label in the code
					*/
                     (yyval) = (yyvsp[-4]);
                     (yyval)->code += "\n;Forloop";
                     char *label = newLabel();
                     char *endlabel = newLabel();

                     (yyval)->code += "\n"+ string(label)+ ":\n";
                     (yyval)->code += ";For loop - test expression\n";
                     (yyval)->code += (yyvsp[-3])->code;
                     (yyval)->code += "CMP "+ (yyvsp[-3])->symbol+ " ,0\n";
                     (yyval)->code += "JE "+ string(endlabel)+ "\n";
                     (yyval)->code += ";For loop - body\n";
                     (yyval)->code += (yyvsp[0])->code;
                     (yyval)->code += ";For loop - increment\n";
                     (yyval)->code += (yyvsp[-2])->code;
                     (yyval)->code += "JMP "+ string(label)+ "\n";
                     (yyval)->code += "\n"+ string(endlabel)+ ":\n";

                     fprintf(parseLog, "GRAMMER RULE: statement -> FOR LPAREN expression_statement expression_statement expression RPAREN statement  \n"); 
                     // TODO Some code in Line 96 of the template
                 }
#line 1740 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 32:
#line 319 "parser.y" /* yacc.c:1646  */
    {
                    fprintf(parseLog, "GRAMMER RULE: statement -> IF LPAREN expression RPAREN statement  \n"); 
					(yyval)=(yyvsp[-2]);
					
                    (yyval)->code += "\n;If statement\n";
					char *label=newLabel();
					(yyval)->code+="MOV AX, "+(yyvsp[-2])->getSymbol()+"\n";
					(yyval)->code+="CMP AX, 0\n";
					(yyval)->code+="JE "+string(label)+"\n";
					(yyval)->code+=(yyvsp[0])->code;
					(yyval)->code+=string(label)+":\n";
                 }
#line 1757 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 33:
#line 332 "parser.y" /* yacc.c:1646  */
    {
                    fprintf(parseLog, "GRAMMER RULE: statement -> IF LPAREN expression RPAREN statement ELSE statement  \n"); 
					(yyval)=(yyvsp[-4]);
					
                    (yyval)->code += "\n;If-Else statement\n";
					char *label=newLabel();
					char *label_if=newLabel();
					char *label_else=newLabel();
					(yyval)->code+="MOV AX, "+(yyvsp[-4])->getSymbol()+"\n";
					(yyval)->code+="CMP AX, 0\n";
					(yyval)->code+="JE "+string(label_else)+"\n";
					(yyval)->code+=(yyvsp[-2])->code;     
					(yyval)->code+="JE "+string(label_if)+"\n";
					(yyval)->code+=string(label_else)+":\n";
					(yyval)->code+=(yyvsp[0])->code;     
					(yyval)->code+=string(label_if)+":\n";
                 }
#line 1779 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 34:
#line 350 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = new Symbol_info();
                     (yyval)->code += "\n;While Loop";
                     char *label = newLabel();
                     char *endlabel = newLabel();

                     (yyval)->code += "\n"+ string(label)+ ":\n";
                     (yyval)->code += ";While loop - Test expression\n";
                     (yyval)->code += (yyvsp[-2])->code;
                     (yyval)->code += "CMP "+ (yyvsp[-2])->symbol+ " ,0\n";
                     (yyval)->code += "JE "+ string(endlabel)+ "\n";
                     (yyval)->code += ";While loop - body\n";
                     (yyval)->code += (yyvsp[0])->code;
                     (yyval)->code += "JMP "+ string(label)+ "\n";
                     (yyval)->code += "\n"+ string(endlabel)+ ":\n";
                    fprintf(parseLog, "GRAMMER RULE: statement -> WHILE LPAREN expression RPAREN statement  \n"); 
                 }
#line 1801 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 35:
#line 368 "parser.y" /* yacc.c:1646  */
    {
                    //TODO 
                    (yyval)->code += "\n;Print Variable " + (yyvsp[-2])->symbol;
                    fprintf(parseLog, "GRAMMER RULE: statement -> PRINTLN LPAREN ID RPAREN SEMICOLON  \n"); 
                 }
#line 1811 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 36:
#line 374 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = (yyvsp[-2]);
                    fprintf(parseLog, "GRAMMER RULE: statement -> RETURN expression SEMICOLON  \n"); 
                 }
#line 1820 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 37:
#line 381 "parser.y" /* yacc.c:1646  */
    {
                            (yyval) = new Symbol_info(";", "SEMICOLON");
                            fprintf(parseLog, "GRAMMER RULE: expression_statement -> SEMICOLON  \n"); 
                        }
#line 1829 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 38:
#line 386 "parser.y" /* yacc.c:1646  */
    {
                            (yyval) = (yyvsp[-1]);
                            fprintf(parseLog, "GRAMMER RULE: expression_statement -> expression SEMICOLON   \n"); 
                        }
#line 1838 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 39:
#line 393 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)= new Symbol_info((yyvsp[0]));
                    fprintf(parseLog, "GRAMMER RULE: variable -> ID 		  \n"); 
                 }
#line 1847 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 40:
#line 398 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)= new Symbol_info((yyvsp[-3]));
                    (yyval)->setType("array");

                    (yyval)->code=(yyvsp[-1])->code+"MOV BX, " +(yyvsp[-1])->getSymbol() +"\nADD BX, BX\n";
                    
                    delete (yyvsp[-1]);
                    fprintf(parseLog, "GRAMMER RULE: variable -> ID LSQBRAC expression RSQBRAC   \n"); 
                 }
#line 1861 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 41:
#line 410 "parser.y" /* yacc.c:1646  */
    {
               (yyval) = (yyvsp[0]);
               (yyval)->code += ";Logical Expression\n";
               fprintf(parseLog, "GRAMMER RULE: expression -> logic_expression	  \n"); 
           }
#line 1871 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 42:
#line 416 "parser.y" /* yacc.c:1646  */
    {
				(yyval)=(yyvsp[-2]);
				(yyval)->code+="\n;Assignment Operation\n";
				(yyval)->code+=(yyvsp[0])->code;
				(yyval)->code+="MOV AX, "+(yyvsp[0])->getSymbol()+"\n";
				if((yyval)->getType()=="notarray"){ 
					(yyval)->code+= "MOV "+(yyvsp[-2])->getSymbol()+", AX\n";
				}

				else{
					(yyval)->code+= "MOV  "+ (yyvsp[-2])->getSymbol()+"[BX], AX\n";

				}
				delete (yyvsp[0]);
                /*printf("Exiting e->v a l\n");*/
                
                fprintf(parseLog, "GRAMMER RULE: expression -> variable ASSIGNOP logic_expression 	  \n"); 
           }
#line 1894 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 43:
#line 437 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = (yyvsp[0]);
                    fprintf(parseLog, "GRAMMER RULE: logic_expression -> rel_expression 	  \n"); 
                 }
#line 1903 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 44:
#line 442 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=new Symbol_info((yyvsp[-2]));
                    /*$$ = new Symbol_info();*/
                    (yyval)->code+=(yyvsp[0])->code;
                    (yyval)->code+="\n;Doing Logical opeation between two relational expressions\n";
					
                    char *tr = newTemp();
                    char *f = newLabel();
                    char *t = newLabel();

					if((yyvsp[-1])->getSymbol()=="&&"){
						/* 
						Check whether both operands value is 1. If both are one set value of a temporary variable to 1
						otherwise 0
						*/
                        //TODO Priority 1
                        (yyval)->code += "\n;Doing AND operation\n";
                        
                        /*this is full of errors y'all*/
                        (yyval)->code += "CMP " + (yyvsp[-2])->symbol + ", 1\n";
                        (yyval)->code += "JNE " + string(f) + "\n";
                        (yyval)->code += "CMP " + (yyvsp[0])->symbol + ", 1\n";
                        (yyval)->code += "JNE " + string(f) + "\n";
                        (yyval)->code += "MOV " + string(tr) + " ,1\n";
                        (yyval)->code += "JMP " + string(t) + " \n";
                        (yyval)->code +=  string(f) + ": \n";
                        (yyval)->code += "MOV " + string(tr) + " ,0\n";
                        (yyval)->code += string(t) + " :\n";

					}
					else if((yyvsp[-1])->getSymbol()=="||"){
                        (yyval)->code += "\n;Doing OR operation\n";

                        (yyval)->code += "CMP " + (yyvsp[-2])->symbol + ", 1\n";
                        (yyval)->code += "JE " + string(t) + "\n";
                        (yyval)->code += "CMP " + (yyvsp[0])->symbol + ", 1\n";
                        (yyval)->code += "JE " + string(t) + "\n";
                        (yyval)->code += "MOV " + string(tr) + " ,0\n";
                        (yyval)->code += "JMP " + string(f) + " \n";
                        (yyval)->code += string(t) + " :\n";
                        (yyval)->code += "MOV " + string(t) + " ,1\n";
                        (yyval)->code +=  string(f) + ": \n";
						
					}
					delete (yyvsp[0]);
                    (yyval)->symbol = string(t);
                    fprintf(parseLog, "GRAMMER RULE: logic_expression -> rel_expression LOGICOP rel_expression 	  \n"); 
                 }
#line 1956 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 45:
#line 493 "parser.y" /* yacc.c:1646  */
    {
                     /*$$=$1;*/
                     (yyval)=new Symbol_info((yyvsp[0]));
                     fprintf(parseLog, "GRAMMER RULE: rel_expression -> simple_expression   \n"); 
                 }
#line 1966 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 46:
#line 499 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=(yyvsp[-2]);
                    (yyval)->code+=(yyvsp[0])->code;
                    (yyval)->code+="MOV AX, " + (yyvsp[-2])->getSymbol()+"\n";
                    (yyval)->code+="CMP AX, " + (yyvsp[0])->getSymbol()+"\n";
                    char *temp=newTemp();
                    char *label1=newLabel();
                    char *label2=newLabel();
                    if((yyvsp[-1])->getSymbol()=="<"){
                        (yyval)->code+="JL " + string(label1)+"\n";
                    }
                    else if((yyvsp[-1])->getSymbol()=="<="){
                    //TODO
                        (yyval)->code+="JLE " + string(label1)+"\n";
                    }
                    else if((yyvsp[-1])->getSymbol()==">"){
                        (yyval)->code+="JG " + string(label1)+"\n";
                    }
                    else if((yyvsp[-1])->getSymbol()==">="){
                        (yyval)->code+="JGE " + string(label1)+"\n";
                    }
                    else if((yyvsp[-1])->getSymbol()=="=="){
                        (yyval)->code+="JE " + string(label1)+"\n";
                    }
                    else{
                        (yyval)->code+="JNE " + string(label1)+"\n";
                    }
                    
                    (yyval)->code+="MOV "+string(temp) +", 0\n";
                    (yyval)->code+="JMP "+string(label2) +"\n";
                    (yyval)->code+=string(label1)+":\nMOV "+string(temp)+", 1\n";
                    (yyval)->code+=string(label2)+":\n";
                    (yyval)->setSymbol(temp);
                    delete (yyvsp[0]);

                    fprintf(parseLog, "GRAMMER RULE: rel_expression -> simple_expression RELOP simple_expression	  \n"); 
                 }
#line 2008 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 47:
#line 539 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = (yyvsp[0]);
                     fprintf(parseLog, "GRAMMER RULE: simple_expression -> term   \n"); 
                 }
#line 2017 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 48:
#line 544 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=new Symbol_info((yyvsp[-2]));
                    (yyval)->code+=(yyvsp[0])->code;
                    char *t = newTemp();
                    
                    // MOVe one of the operands to a register, perform addition or subtraction with the other operand and MOVe the result in a temporary variable  
                    
                    if((yyvsp[-1])->getSymbol()=="+"){
                        (yyval)->code += "\n;ADDING THINGS\n";
                        (yyval)->code+="MOV AX,"+(yyvsp[-2])->symbol+"\n";
                        (yyval)->code+="MOV BX,"+(yyvsp[0])->symbol+"\n";
                        (yyval)->code+="ADD AX,BX\n";
                        (yyval)->code+="MOV "+string(t)+",AX\n";
                        (yyval)->code+="\n";
                    }
                    else{
                        (yyval)->code += "\n;SUBBING THINGS\n";
                        (yyval)->code+="MOV AX,"+(yyvsp[-2])->symbol+"\n";
                        (yyval)->code+="MOV BX,"+(yyvsp[0])->symbol+"\n";
                        (yyval)->code+="SUB AX,BX\n";
                        (yyval)->code+="MOV "+string(t)+",AX\n";
                        (yyval)->code+="\n";
                    
                    }
                    delete (yyvsp[0]);

                    fprintf(parseLog, "GRAMMER RULE: simple_expression -> simple_expression ADDOP term   \n"); 
                 }
#line 2050 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 49:
#line 575 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = (yyvsp[0]);
                    fprintf(parseLog, "GRAMMER RULE:  term ->	unary_expression  \n"); 
                 }
#line 2059 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 50:
#line 580 "parser.y" /* yacc.c:1646  */
    {
                 //TODO
                    (yyval)=(yyvsp[-2]);
                    (yyval)->code += (yyvsp[0])->code;
                    (yyval)->code += "MOV AX, "+ (yyvsp[-2])->getSymbol()+"\n";
                    (yyval)->code += "MOV BX, "+ (yyvsp[0])->getSymbol() +"\n";
                    char *temp=newTemp();
                    if((yyvsp[-1])->getSymbol()=="*"){
                        (yyval)->code += "\n;Multiplication\n";
                        (yyval)->code += "MUL BX\n";
                        (yyval)->code += "MOV "+ string(temp) + ", AX\n\n";
                    }
                    else if((yyvsp[-1])->getSymbol()=="/"){
                        // TODO
                        // Division
                        // clear dx, perform 'div BX' and MOV AX to temp
                        (yyval)->code += "\n;Division\n";
                        (yyval)->code += "POP DX\n";
                        (yyval)->code += "DIV BX\n";
                        (yyval)->code += "MOV "+ string(temp) + ", AX\n\n";
                    }
                    else{
                        // % or MOD
                        // clear dx, perform 'div BX' and MOV dx to temp
                        (yyval)->code += "\n;Modulo\n";
                        (yyval)->code += "POP DX\n";
                        (yyval)->code += "DIV BX\n";
                        (yyval)->code += "MOV "+ string(temp) + ", DX\n\n";
                    }
                    (yyval)->setSymbol(temp);
                    delete (yyvsp[0]);
                    
                    fprintf(parseLog, "GRAMMER RULE: term -> term MULOP unary_expression  \n"); 
                 }
#line 2098 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 51:
#line 617 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=new Symbol_info((yyvsp[0]));
                    //TODO Perform NEG operation if the symbol of ADDOP is '-'
                    fprintf(parseLog, "GRAMMER RULE: unary_expression -> ADDOP unary_expression    \n"); 
                 }
#line 2108 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 52:
#line 623 "parser.y" /* yacc.c:1646  */
    {
                    (yyval)=new Symbol_info((yyvsp[0]));
                    char *temp=newTemp();
                    (yyval)->code="MOV AX, " + (yyvsp[0])->getSymbol() + "\n";
                    (yyval)->code+="not AX\n";
                    (yyval)->code+="MOV "+string(temp)+", AX";
                    fprintf(parseLog, "GRAMMER RULE: unary_expression -> NOT unary_expression   \n"); 
                 }
#line 2121 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 53:
#line 632 "parser.y" /* yacc.c:1646  */
    {
                    (yyval) = (yyvsp[0]);
                    fprintf(parseLog, "GRAMMER RULE: unary_expression -> factor   \n"); 
                 }
#line 2130 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 54:
#line 639 "parser.y" /* yacc.c:1646  */
    {
			(yyval)= (yyvsp[0]);
			if((yyval)->getType()=="notarray"){
			    //TODO Do something
			}
			
			else{
				char *temp= newTemp();
				(yyval)->code+="MOV AX, " + (yyvsp[0])->getSymbol() + "[BX]\n";
				(yyval)->code+= "MOV " + string(temp) + ", AX\n";
				(yyval)->setSymbol(temp);
            }
            fprintf(parseLog, "GRAMMER RULE:  factor  -> variable   \n"); 
        }
#line 2149 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 55:
#line 654 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = (yyvsp[-1]);

            char *temp= newTemp();
            (yyval)->code+="MOV AX, " + (yyvsp[-3])->getSymbol() + "[BX]\n";
            (yyval)->code+= "MOV " + string(temp) + ", AX\n";
            (yyval)->setSymbol(temp);

            fprintf(parseLog, "GRAMMER RULE: factor-> ID LPAREN argument_list RPAREN  \n"); 
        }
#line 2164 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 56:
#line 665 "parser.y" /* yacc.c:1646  */
    {
           (yyval) = (yyvsp[-1]); 
           fprintf(parseLog, "GRAMMER RULE: factor -> LPAREN expression RPAREN  \n"); 
        }
#line 2173 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 57:
#line 670 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = new Symbol_info((yyvsp[0]));
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_INT   \n"); 
        }
#line 2182 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 58:
#line 675 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = new Symbol_info((yyvsp[0]));
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_FLOAT  \n"); 
        }
#line 2191 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 59:
#line 680 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = new Symbol_info((yyvsp[0]));
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_CHAR  \n"); 
        }
#line 2200 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 60:
#line 685 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = new Symbol_info((yyvsp[-1]));
            (yyval)->code += "\n;Increment\n";
            char *t = newTemp();
            if ((yyvsp[-1])->getType()=="notarray") {
            // TODO Perform increment
                char *temp = newTemp();
                (yyval)->code+="ADD BX,2\n";
                (yyval)->code+="MOV "+string(temp)+","+(yyvsp[-1])->symbol+"[BX]\n";
            }
            else {
                (yyval)->code+= "INC " + (yyvsp[-1])->symbol + "\n";
            }
            (yyval)->symbol = string(t);
            fprintf(parseLog, "GRAMMER RULE: factor -> variable INCOP   \n"); 
        }
#line 2221 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 61:
#line 702 "parser.y" /* yacc.c:1646  */
    {
            (yyval) = new Symbol_info((yyvsp[-1]));
            (yyval)->code += "\n;Decrement\n";
            char *t = newTemp();
            if ((yyvsp[-1])->getType()=="notarray") {
                char *temp = newTemp();
                (yyval)->code+="SUB BX,2\n";
                (yyval)->code+="MOV "+string(temp)+","+(yyvsp[-1])->symbol+"[BX]\n";
            }
            else {
                (yyval)->code+= "DEC " + (yyvsp[-1])->symbol + "\n";
            }
            (yyval)->symbol = string(t);
            fprintf(parseLog, "GRAMMER RULE: factor -> variable DECOP  \n"); 
        }
#line 2241 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 62:
#line 720 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = (yyvsp[-2]);
                     (yyval)->code += (yyvsp[-1])->code;
                     fprintf(parseLog, "GRAMMER RULE: argument_list -> argument_list COMMA logic_expression  \n"); 
                 }
#line 2251 "y.tab.cpp" /* yacc.c:1646  */
    break;

  case 63:
#line 726 "parser.y" /* yacc.c:1646  */
    {
                     (yyval) = (yyvsp[0]);
                     fprintf(parseLog, "GRAMMER RULE: argument_list -> logic_expression  \n"); 
                 }
#line 2260 "y.tab.cpp" /* yacc.c:1646  */
    break;


#line 2264 "y.tab.cpp" /* yacc.c:1646  */
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
#line 734 "parser.y" /* yacc.c:1906  */


int main(int argc,char *argv[]){
    adele();
    tokenout= fopen("token.txt","w");
    parseLog = fopen("log.txt", "w");
    asmout = fopen("code.asm", "w");

    string c = "";

    c += ".MODEL SMALL\n\n";
    c += ".STACK 100H\n\n";
    c += ".data\n";
    c += "x dw 0\n";
    c += "y dw 0\n";
    c += "z dw 0\n";
    c += "a dw 0\n";
    c += "b dw 0\n";
    c += "c dw 0\n";
    c += "t0 dw 0\n";
    c += "t1 dw 0\n";
    c += "t2 dw 0\n";
    c += "t3 dw 0\n";
    c += "t4 dw 0\n";
    c += "t5 dw 0\n";
    c += "t6 dw 0\n";
    c += "t7 dw 0\n";
    c += "t8 dw 0\n";
    c += "t9 dw 0\n";
    c += "t10 dw 0\n";
    c += "t11 dw 0\n";
    c += "t12 dw 0\n";
    c += "t13 dw 0\n";
    c += "t14 dw 0\n";
    c += "t15 dw 0\n";
    c += "t16 dw 0\n\n\n";
    c += ".CODE\n";

    fprintf(asmout, c.c_str());

    fprintf(parseLog, "Program start: Line Count: 1\n");
    yyparse();
    fclose(tokenout);
    fclose(parseLog);
    fclose(asmout);
    printf ("\nTotal line Count: %d\n", line_count);
    /*parser_table.print(logout);*/
    

return 0;
}
