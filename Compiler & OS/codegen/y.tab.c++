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

extern int yylex(void);
extern int yyparse(void);
string assembly_codes = "";
string varDec = "";

extern "C"
{
	//Kill me
}
extern char yytext[];
extern int column;
ofstream assembly;

string getTemp(string type = "w")	//put type = "b" for byte
{
	stringstream ss;
	varDec+= ss.str()+" d"+type[0]+" 0\n";
	return ss.str();
}

string getLabel()	//put type = "b" for byte
{
	stringstream ss;
	ss << "label" << labelCount++;
	return ss.str();
}


//ofstream ir_code_stream; 

void yyerror(char *s)
{
	fprintf(stderr,"At Line %d, ERROR-> %s\n",line_count,s);
	return;
}

void adele(){
	cout << "Hello from the other side"<< endl;
}


#line 125 "y.tab.c++" /* yacc.c:339  */

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
   by #include "y.tab.h++".  */
#ifndef YY_YY_Y_TAB_H__INCLUDED
# define YY_YY_Y_TAB_H__INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
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
    LCURL = 264,
    RCURL = 265,
    ID = 266,
    LTHIRD = 267,
    CONST = 268,
    RTHIRD = 269,
    FOR = 270,
    LBRAKET = 271,
    RBRAKET = 272,
    IF = 273,
    STATIC = 274,
    ELSE = 275,
    WHILE = 276,
    PRINTLN = 277,
    RETURN = 278,
    ASSIGNOP = 279,
    LOGICOP = 280,
    REGISTER = 281,
    RELOP = 282,
    ADDOP = 283,
    MULOP = 284,
    NOT = 285,
    INCOP = 286,
    DECOP = 287,
    CONST_CHAR = 288,
    CONST_INT = 289,
    CONST_FLOAT = 290,
    MAIN = 291,
    TYPE_NAME = 292,
    VOLATILE = 293,
    AUTO = 294,
    VOID = 295,
    SHORT = 296,
    LONG = 297,
    DOUBLE = 298,
    SIGNED = 299,
    UNSIGNED = 300,
    EXTERN = 301,
    ENUM = 302,
    IDENTIFIER = 303,
    STRUCT = 304,
    UNION = 305,
    HEADER = 306,
    NUMBER = 307,
    STRING = 308,
    OR_OP = 309,
    AND_OP = 310,
    EQ_OP = 311,
    NE_OP = 312,
    LEFT_OP = 313,
    GE_OP = 314,
    LE_OP = 315,
    RIGHT_OP = 316
  };
#endif
/* Tokens.  */
#define SEMICOLON 258
#define INT 259
#define FLOAT 260
#define CHAR 261
#define COMMA 262
#define TYPEDEF 263
#define LCURL 264
#define RCURL 265
#define ID 266
#define LTHIRD 267
#define CONST 268
#define RTHIRD 269
#define FOR 270
#define LBRAKET 271
#define RBRAKET 272
#define IF 273
#define STATIC 274
#define ELSE 275
#define WHILE 276
#define PRINTLN 277
#define RETURN 278
#define ASSIGNOP 279
#define LOGICOP 280
#define REGISTER 281
#define RELOP 282
#define ADDOP 283
#define MULOP 284
#define NOT 285
#define INCOP 286
#define DECOP 287
#define CONST_CHAR 288
#define CONST_INT 289
#define CONST_FLOAT 290
#define MAIN 291
#define TYPE_NAME 292
#define VOLATILE 293
#define AUTO 294
#define VOID 295
#define SHORT 296
#define LONG 297
#define DOUBLE 298
#define SIGNED 299
#define UNSIGNED 300
#define EXTERN 301
#define ENUM 302
#define IDENTIFIER 303
#define STRUCT 304
#define UNION 305
#define HEADER 306
#define NUMBER 307
#define STRING 308
#define OR_OP 309
#define AND_OP 310
#define EQ_OP 311
#define NE_OP 312
#define LEFT_OP 313
#define GE_OP 314
#define LE_OP 315
#define RIGHT_OP 316

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H__INCLUDED  */

/* Copy the second part of user declarations.  */

#line 298 "y.tab.c++" /* yacc.c:358  */

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
#define YYLAST   888

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  83
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  60
/* YYNRULES -- Number of rules.  */
#define YYNRULES  181
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  302

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    77,    69,     2,
      78,    79,    75,    73,    65,    74,     2,    76,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    70,    64,
      71,    66,    72,    80,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    81,     2,    82,    68,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    62,    67,    63,     2,     2,     2,     2,
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
      55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    70,    70,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   106,   108,   109,   110,   111,
     116,   117,   121,   122,   127,   128,   129,   130,   134,   135,
     139,   140,   141,   142,   143,   144,   148,   149,   153,   154,
     158,   159,   160,   161,   162,   163,   164,   168,   169,   173,
     177,   178,   182,   183,   187,   188,   192,   193,   197,   198,
     202,   203,   207,   208,   209,   213,   214,   219,   220,   221,
     222,   223,   228,   232,   233,   237,   238,   243,   244,   247,
     248,   251,   252,   255,   256,   257,   261,   262,   266,   267,
     268,   272,   273,   277,   278,   279,   283,   284,   288,   289,
     290,   291,   292,   297,   298,   299,   302,   303,   304,   305,
     306,   307,   308,   313,   314,   318,   319,   320,   324,   328,
     329,   333,   334,   335,   339,   340,   341,   342,   346,   347,
     351,   352,   357,   358,   359,   360,   366,   367,   368,   372,
     373,   378,   379,   383,   384,   389,   393,   394,   398,   399,
     403,   404,   408,   409,   410,   414,   415,   419,   420,   421,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   437,
     438,   439,   440,   444,   445,   449,   450,   451,   452,   453,
     454,   455
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "SEMICOLON", "INT", "FLOAT", "CHAR",
  "COMMA", "TYPEDEF", "LCURL", "RCURL", "ID", "LTHIRD", "CONST", "RTHIRD",
  "FOR", "LBRAKET", "RBRAKET", "IF", "STATIC", "ELSE", "WHILE", "PRINTLN",
  "RETURN", "ASSIGNOP", "LOGICOP", "REGISTER", "RELOP", "ADDOP", "MULOP",
  "NOT", "INCOP", "DECOP", "CONST_CHAR", "CONST_INT", "CONST_FLOAT",
  "MAIN", "TYPE_NAME", "VOLATILE", "AUTO", "VOID", "SHORT", "LONG",
  "DOUBLE", "SIGNED", "UNSIGNED", "EXTERN", "ENUM", "IDENTIFIER", "STRUCT",
  "UNION", "HEADER", "NUMBER", "STRING", "OR_OP", "AND_OP", "EQ_OP",
  "NE_OP", "LEFT_OP", "GE_OP", "LE_OP", "RIGHT_OP", "'{'", "'}'", "';'",
  "','", "'='", "'|'", "'^'", "'&'", "':'", "'<'", "'>'", "'+'", "'-'",
  "'*'", "'/'", "'%'", "'('", "')'", "'?'", "'['", "']'", "$accept",
  "Program", "type_specifier", "compound_statement", "declaration_list",
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
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   123,   125,    59,    44,    61,   124,    94,    38,
      58,    60,    62,    43,    45,    42,    47,    37,    40,    41,
      63,    91,    93
};
# endif

#define YYPACT_NINF -218

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-218)))

#define YYTABLE_NINF -82

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      24,     7,   125,    53,  -218,    68,    75,   397,  -218,  -218,
    -218,  -218,  -218,  -218,   645,  -218,   115,   651,   137,  -218,
     145,   166,   651,  -218,   771,   771,  -218,  -218,  -218,  -218,
    -218,  -218,  -218,  -218,  -218,  -218,  -218,  -218,  -218,   -26,
    -218,  -218,  -218,   838,   822,   348,   539,  -218,   -22,  -218,
    -218,   148,    22,  -218,   168,   822,   822,   129,   130,    91,
     200,   314,   165,    86,   -16,   132,   -17,   134,   189,   139,
     133,  -218,  -218,   -23,  -218,  -218,  -218,   720,   167,   238,
      33,   651,   651,   265,    27,  -218,  -218,  -218,   223,   243,
     838,   838,   207,   158,  -218,   281,  -218,   560,  -218,  -218,
    -218,  -218,  -218,    20,    98,   182,  -218,    76,   227,    48,
     771,  -218,   651,   771,  -218,  -218,   651,   651,   771,   771,
     771,  -218,   651,   651,   651,   651,   232,   838,   651,   651,
     651,   651,   651,   651,  -218,  -218,   651,   651,   651,   651,
     651,   651,   651,   651,   282,    13,   167,   651,  -218,    42,
      69,   285,  -218,   243,   239,   155,  -218,  -218,  -218,   651,
     269,   443,  -218,    66,    96,   286,  -218,  -218,    20,  -218,
     225,  -218,    98,    48,   712,   444,   576,  -218,  -218,   254,
    -218,  -218,   130,  -218,   165,   293,   200,  -218,  -218,    86,
     132,   132,   838,  -218,   728,    52,   -17,   -17,   -17,   -17,
     139,   139,   129,   133,   133,  -218,  -218,  -218,   189,   -25,
    -218,  -218,   651,   597,   597,   319,   156,   651,  -218,   243,
    -218,  -218,   -13,   258,  -218,   245,   246,  -218,   244,  -218,
     491,   613,    66,   295,  -218,  -218,  -218,  -218,   712,  -218,
    -218,  -218,  -218,   -28,   248,  -218,   250,   775,  -218,  -218,
     651,  -218,   188,   260,   651,    85,   313,  -218,  -218,  -218,
    -218,  -218,   222,  -218,    60,  -218,   822,  -218,  -218,  -218,
    -218,   255,  -218,   253,   322,  -218,   175,   289,  -218,  -218,
    -218,  -218,  -218,  -218,    52,   651,  -218,   597,   597,  -218,
    -218,  -218,  -218,  -218,   221,  -218,  -218,  -218,  -218,  -218,
    -218,  -218
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     0,    15,     0,     2,    47,
       6,     8,     4,    67,    26,    60,     0,     0,     0,    69,
       0,     0,     0,    71,     0,     0,   110,   108,   109,    14,
      61,    70,     3,     5,     7,     9,    10,    11,    68,     0,
      91,    92,    19,     0,    32,     0,     0,    20,     0,    22,
      40,   106,     0,    54,    56,    34,    30,   119,    73,    58,
      79,   128,    75,    86,     0,    88,    98,    85,   143,   103,
     121,   124,    12,   146,   150,   148,    13,     0,     0,    52,
       0,     0,     0,     0,     0,   106,    83,    84,   154,     0,
     133,   135,     0,   130,    33,     0,    17,     0,    21,    16,
      23,   175,    28,   169,     0,     0,    36,     0,    38,   174,
       0,    48,     0,     0,    35,    31,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,   138,     0,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,   107,     0,
       0,     0,    46,     0,   155,     0,   141,   132,   134,     0,
       0,     0,   131,   158,   157,    24,    18,    77,   170,   171,
       0,    29,     0,   173,     0,     0,     0,    55,    81,   128,
     149,    57,    74,   128,    76,    59,    80,    82,   151,    87,
      89,    90,     0,    50,     0,     0,   102,   101,    99,   100,
     104,   105,   120,   122,   123,   125,   126,   127,   144,     0,
      27,    53,     0,     0,     0,     0,     0,     0,   152,     0,
     129,   165,   117,   118,   113,     0,     0,   161,     0,   145,
       0,     0,   159,     0,    78,   172,   176,    37,     0,    39,
      62,    96,   181,     0,     0,   178,     0,     0,   137,    51,
       0,   139,     0,    93,     0,     0,    42,    44,    45,   153,
     156,   142,     0,   116,   157,   115,     0,   166,   160,   162,
     167,     0,   163,     0,     0,    65,     0,     0,   180,   179,
     177,   136,    94,    49,     0,     0,   147,     0,     0,   114,
     168,   164,    25,    63,     0,    97,   140,    95,    41,    43,
      64,    66
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -218,  -218,   -30,  -218,  -218,   300,   301,    65,  -218,   177,
     -45,   -58,  -171,   146,     2,    12,   230,   247,   -36,  -217,
    -218,  -218,  -218,   226,   241,  -218,   240,   249,    -7,   256,
     242,  -218,    88,  -218,   154,    58,  -218,  -218,   109,  -164,
     235,    90,   120,    40,  -218,    61,  -218,  -218,   251,  -218,
    -168,   -60,  -110,  -218,   180,   -89,  -150,   -87,   -99,  -104
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    44,     8,    45,    46,    47,   222,   105,   106,
      49,    50,   193,   194,    85,    52,    53,    54,    55,   239,
     276,    56,   122,    57,    58,   168,    59,    60,   183,    62,
      63,    64,   251,   243,    65,    66,    67,   223,   224,   225,
      68,    69,    70,    71,    92,   195,    72,   252,   155,    73,
     228,    74,    75,    76,   156,   226,   163,   107,   108,   109
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      61,   100,   180,   173,   162,   170,   164,    91,   246,    51,
      61,   244,   188,    90,   232,    61,   169,    86,    87,    51,
     146,   275,    88,   249,    51,   111,   101,   211,     1,    80,
     152,   142,   126,    15,    84,   101,    89,   277,    61,    61,
     112,   132,   102,     3,   133,   254,   127,    51,    51,   260,
     148,   278,   100,   103,    91,    91,   104,   143,    30,   213,
      90,    90,   103,   273,   240,   262,   271,   167,   161,     5,
      61,    61,    48,   164,    61,    61,   249,   301,   112,    51,
      51,   235,   282,    51,    51,     6,   214,   112,   212,   145,
      61,    91,   112,   149,   150,   103,   253,    90,   112,    51,
     101,   229,   287,   178,    93,   179,   178,   112,   101,    94,
      48,   178,   178,   187,   232,   179,   229,   297,   118,   119,
     114,   115,   250,   265,   101,     4,   175,   103,   240,   176,
     104,    78,   234,   263,   112,   264,    61,     7,   262,    61,
      61,   161,   124,   125,   230,    51,   101,   231,    51,    51,
     112,   157,   158,    81,   104,   209,    91,   229,    91,   145,
     173,    82,    90,   170,    90,   134,   135,   179,   256,   257,
       9,   229,   110,   103,   160,   264,   104,   161,    79,   205,
     206,   207,    83,    17,   240,   253,   196,   197,   198,   199,
     229,   128,   129,   113,   286,    24,   116,    25,   117,   220,
      26,    27,    28,   130,   131,    61,    61,    61,   139,   140,
     141,    91,   137,   138,    51,    51,    51,    90,   218,   259,
     219,   219,   200,   201,   255,   229,    10,    11,    12,   120,
      13,   179,    79,   103,   123,    15,   160,    17,   293,   161,
     294,    19,   298,   299,   136,    43,   171,   172,    23,    24,
     147,    25,   283,   284,    26,    27,    28,   203,   204,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     101,    40,    41,    10,    11,    12,   151,    13,   190,   191,
      61,    61,    15,   238,   300,   153,   159,   179,    19,    51,
      51,   154,   165,   174,   192,    23,   210,   103,   233,    43,
     262,   221,   215,   161,   236,   217,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,   -81,    40,    41,
     121,   119,   258,   266,   267,   268,   269,   279,   -81,   274,
     285,   -81,   280,   288,   290,   291,   292,   295,   247,   -81,
     177,   -81,   -81,   -81,   103,    97,    98,   160,   221,   237,
     161,     9,    10,    11,    12,    95,    13,   182,   185,    79,
     181,    15,   202,    16,    17,   189,    18,    19,   186,    20,
      21,    22,   296,   184,    23,   289,    24,   208,    25,   -81,
     121,    26,    27,    28,   -81,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,   261,
       9,    10,    11,    12,   216,    13,     0,     0,    14,     0,
      15,    96,    16,    17,     0,    18,    19,     0,    20,    21,
      22,     0,     0,    23,     0,    24,    43,    25,     0,     0,
      26,    27,    28,     0,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    10,    11,
      12,     0,    13,     0,    79,     0,     0,    15,     0,    17,
      42,     0,     0,    19,     0,     0,     0,     0,     0,     0,
      23,    24,     0,    25,     0,    43,    26,    27,    28,     0,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,   241,    40,    41,    10,    11,    12,     0,    13,
       0,     0,     0,     0,    15,     0,     0,     0,     0,     0,
      19,     0,     0,     0,     0,     0,     0,    23,     0,     0,
       0,    43,     0,   242,     0,   227,     0,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,     9,     0,     0,     0,     0,     0,     0,     0,
      79,     0,     0,     0,    16,    17,     0,    18,     0,     0,
      20,    21,    22,     9,     0,     0,     0,    24,     0,    25,
     270,    79,    26,    27,    28,    16,    17,     0,    18,     0,
       0,    20,    21,    22,     0,     0,     0,    79,    24,     0,
      25,     0,    17,    26,    27,    28,     0,     0,     0,     0,
       9,     0,    99,     0,    24,     0,    25,     0,    79,    26,
      27,    28,    16,    17,     0,    18,     0,    43,    20,    21,
      22,     0,     0,   166,    79,    24,     0,    25,     0,    17,
      26,    27,    28,     0,     0,     0,     0,     0,    43,     0,
       0,    24,     0,    25,     0,     0,    26,    27,    28,     0,
       0,     0,     0,     0,    43,     0,     0,    77,   245,     0,
       0,     0,    79,     0,     0,     0,     0,    17,     0,   -52,
     -52,     0,   -52,     0,   -52,    43,   -52,   -52,     0,    24,
       0,    25,     0,     0,    26,    27,    28,     0,     0,     0,
       0,    43,     0,     0,     0,   272,     0,     0,     0,   -52,
     -52,   -52,   -52,   -52,   -52,   -52,   -52,     0,     0,     0,
     -52,   -52,   -52,   -52,   -52,     0,   -52,   -52,   -52,   -52,
     -52,   -52,   -52,    79,     0,   -52,     0,     0,    17,    43,
       0,    79,    10,    11,    12,     0,    17,     0,     0,     0,
      24,    15,    25,     0,     0,    26,    27,    28,    24,     0,
      25,     0,     0,    26,   144,    28,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    29,    30,     0,    32,    33,
      34,    35,    36,    37,   238,    39,     0,    40,    41,    10,
      11,    12,    79,     0,     0,     0,     0,    17,    15,     0,
      43,   248,     0,     0,     0,     0,     0,     0,    43,    24,
       0,    25,     0,     0,    26,    27,    28,     0,     0,     0,
       0,     0,    29,    30,     0,    32,    33,    34,    35,    36,
      37,     0,    39,     0,    40,    41,    10,    11,    12,     0,
      13,     0,     0,     0,     0,    15,     0,     0,   281,     0,
       0,    19,    10,    11,    12,     0,     0,     0,    23,     0,
       0,    15,     0,     0,     0,     0,     0,     0,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,     0,     0,    29,    30,     0,    32,    33,
      34,    35,    36,    37,     0,    39,     0,    40,    41
};

static const yytype_int16 yycheck[] =
{
       7,    46,   112,   107,    93,   104,    93,    43,   176,     7,
      17,   175,   122,    43,   164,    22,   103,    24,    25,    17,
      78,   238,    48,   194,    22,     3,    48,    14,     4,    17,
       3,    54,    48,    13,    22,    48,    62,    65,    45,    46,
      65,    58,    64,    36,    61,    70,    62,    45,    46,   217,
      17,    79,    97,    75,    90,    91,    78,    80,    38,    17,
      90,    91,    75,   231,   174,    78,   230,   103,    81,    16,
      77,    78,     7,   160,    81,    82,   247,   294,    65,    77,
      78,   168,   250,    81,    82,    17,    17,    65,   146,    77,
      97,   127,    65,    81,    82,    75,   195,   127,    65,    97,
      48,   161,    17,   110,    43,   112,   113,    65,    48,    44,
      45,   118,   119,   120,   264,   122,   176,   285,    27,    28,
      55,    56,    70,   222,    48,     0,    78,    75,   238,    81,
      78,    16,   168,   222,    65,   222,   143,    62,    78,   146,
     147,    81,    56,    57,    78,   143,    48,    81,   146,   147,
      65,    90,    91,    16,    78,   143,   192,   217,   194,   147,
     264,    16,   192,   262,   194,    31,    32,   174,   213,   214,
       3,   231,    24,    75,    78,   262,    78,    81,    11,   139,
     140,   141,    16,    16,   294,   284,   128,   129,   130,   131,
     250,    59,    60,    25,   254,    28,    67,    30,    68,   159,
      33,    34,    35,    71,    72,   212,   213,   214,    75,    76,
      77,   247,    73,    74,   212,   213,   214,   247,    63,    63,
      65,    65,   132,   133,   212,   285,     4,     5,     6,    29,
       8,   238,    11,    75,    69,    13,    78,    16,    63,    81,
      65,    19,   287,   288,    55,    78,    64,    65,    26,    28,
      12,    30,    64,    65,    33,    34,    35,   137,   138,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,     4,     5,     6,    11,     8,   124,   125,
     287,   288,    13,    62,    63,    62,    79,   294,    19,   287,
     288,    48,    11,    66,    62,    26,    14,    75,    12,    78,
      78,    79,    17,    81,    79,    66,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,     3,    49,    50,
      66,    28,     3,    65,    79,    79,    82,    79,    14,    34,
      70,    17,    82,    20,    79,    82,    14,    48,   192,    25,
     110,    27,    28,    29,    75,    45,    45,    78,    79,   172,
      81,     3,     4,     5,     6,     7,     8,   116,   118,    11,
     113,    13,   136,    15,    16,   123,    18,    19,   119,    21,
      22,    23,   284,   117,    26,   266,    28,   142,    30,    65,
      66,    33,    34,    35,    70,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,   219,
       3,     4,     5,     6,   153,     8,    -1,    -1,    11,    -1,
      13,    63,    15,    16,    -1,    18,    19,    -1,    21,    22,
      23,    -1,    -1,    26,    -1,    28,    78,    30,    -1,    -1,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,     4,     5,
       6,    -1,     8,    -1,    11,    -1,    -1,    13,    -1,    16,
      63,    -1,    -1,    19,    -1,    -1,    -1,    -1,    -1,    -1,
      26,    28,    -1,    30,    -1,    78,    33,    34,    35,    -1,
      -1,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,     4,     5,     6,    -1,     8,
      -1,    -1,    -1,    -1,    13,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,
      -1,    78,    -1,    79,    -1,    82,    -1,    -1,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      11,    -1,    -1,    -1,    15,    16,    -1,    18,    -1,    -1,
      21,    22,    23,     3,    -1,    -1,    -1,    28,    -1,    30,
      79,    11,    33,    34,    35,    15,    16,    -1,    18,    -1,
      -1,    21,    22,    23,    -1,    -1,    -1,    11,    28,    -1,
      30,    -1,    16,    33,    34,    35,    -1,    -1,    -1,    -1,
       3,    -1,    63,    -1,    28,    -1,    30,    -1,    11,    33,
      34,    35,    15,    16,    -1,    18,    -1,    78,    21,    22,
      23,    -1,    -1,    63,    11,    28,    -1,    30,    -1,    16,
      33,    34,    35,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    28,    -1,    30,    -1,    -1,    33,    34,    35,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    12,    82,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    16,    -1,    24,
      25,    -1,    27,    -1,    29,    78,    31,    32,    -1,    28,
      -1,    30,    -1,    -1,    33,    34,    35,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    82,    -1,    -1,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    -1,    -1,    -1,
      65,    66,    67,    68,    69,    -1,    71,    72,    73,    74,
      75,    76,    77,    11,    -1,    80,    -1,    -1,    16,    78,
      -1,    11,     4,     5,     6,    -1,    16,    -1,    -1,    -1,
      28,    13,    30,    -1,    -1,    33,    34,    35,    28,    -1,
      30,    -1,    -1,    33,    34,    35,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    45,    62,    47,    -1,    49,    50,     4,
       5,     6,    11,    -1,    -1,    -1,    -1,    16,    13,    -1,
      78,    63,    -1,    -1,    -1,    -1,    -1,    -1,    78,    28,
      -1,    30,    -1,    -1,    33,    34,    35,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      45,    -1,    47,    -1,    49,    50,     4,     5,     6,    -1,
       8,    -1,    -1,    -1,    -1,    13,    -1,    -1,    63,    -1,
      -1,    19,     4,     5,     6,    -1,    -1,    -1,    26,    -1,
      -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    45,    -1,    47,    -1,    49,    50
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,    84,    36,     0,    16,    17,    62,    86,     3,
       4,     5,     6,     8,    11,    13,    15,    16,    18,    19,
      21,    22,    23,    26,    28,    30,    33,    34,    35,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      49,    50,    63,    78,    85,    87,    88,    89,    90,    93,
      94,    97,    98,    99,   100,   101,   104,   106,   107,   109,
     110,   111,   112,   113,   114,   117,   118,   119,   123,   124,
     125,   126,   129,   132,   134,   135,   136,    12,    16,    11,
      98,    16,    16,    16,    98,    97,   111,   111,    48,    62,
      85,   101,   127,   128,    90,     7,    63,    88,    89,    63,
      93,    48,    64,    75,    78,    91,    92,   140,   141,   142,
      24,     3,    65,    25,    90,    90,    67,    68,    27,    28,
      29,    66,   105,    69,    56,    57,    48,    62,    59,    60,
      71,    72,    58,    61,    31,    32,    55,    73,    74,    75,
      76,    77,    54,    80,    34,    98,    94,    12,    17,    98,
      98,    11,     3,    62,    48,   131,   137,   128,   128,    79,
      78,    81,   138,   139,   140,    11,    63,   101,   108,   140,
     141,    64,    65,   142,    66,    78,    81,    99,   111,   111,
     135,   100,   107,   111,   112,   109,   110,   111,   135,   113,
     117,   117,    62,    95,    96,   128,   118,   118,   118,   118,
     124,   124,   106,   125,   125,   126,   126,   126,   123,    98,
      14,    14,    94,    17,    17,    17,   131,    66,    63,    65,
     126,    79,    90,   120,   121,   122,   138,    82,   133,   134,
      78,    81,   139,    12,   101,   140,    79,    92,    62,   102,
     135,    48,    79,   116,   122,    82,   133,    96,    63,    95,
      70,   115,   130,   141,    70,    98,    93,    93,     3,    63,
     133,   137,    78,   138,   140,   141,    65,    79,    79,    82,
      79,   122,    82,   133,    34,   102,   103,    65,    79,    79,
      82,    63,   133,    64,    65,    70,   134,    17,    20,   121,
      79,    82,    14,    63,    65,    48,   115,   133,    93,    93,
      63,   102
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    83,    84,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    86,    86,    86,    86,    86,
      87,    87,    88,    88,    87,    87,    87,    87,    89,    89,
      90,    90,    90,    90,    90,    90,    91,    91,    92,    92,
      93,    93,    93,    93,    93,    93,    93,    94,    94,    95,
      96,    96,    97,    97,    98,    98,    99,    99,   100,   100,
     101,   101,   102,   102,   102,   103,   103,   104,   104,   104,
     104,   104,   105,   106,   106,   107,   107,   108,   108,   109,
     109,   110,   110,   111,   111,   111,   112,   112,   113,   113,
     113,   114,   114,   115,   115,   115,   116,   116,   117,   117,
     117,   117,   117,   118,   118,   118,   119,   119,   119,   119,
     119,   119,   119,   120,   120,   121,   121,   121,   122,   123,
     123,   124,   124,   124,   125,   125,   125,   125,   126,   126,
     127,   127,   128,   128,   128,   128,   129,   129,   129,   130,
     130,   131,   131,   132,   132,   133,   134,   134,    98,    98,
     135,   135,   136,   136,   136,   137,   137,   138,   138,   138,
     139,   139,   139,   139,   139,   139,   139,   139,   139,   140,
     140,   140,   140,   141,   141,   142,   142,   142,   142,   142,
     142,   142
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     5,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     3,     3,     4,     2,
       1,     2,     1,     2,     3,     6,     1,     4,     2,     3,
       1,     2,     1,     2,     1,     2,     1,     3,     1,     3,
       1,     7,     5,     7,     5,     5,     3,     1,     2,     3,
       1,     2,     1,     4,     1,     3,     1,     3,     1,     3,
       1,     1,     1,     3,     4,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     3,     1,     2,     1,
       3,     1,     3,     2,     2,     1,     1,     3,     1,     3,
       3,     1,     1,     1,     2,     3,     1,     3,     1,     3,
       3,     3,     3,     1,     3,     3,     1,     3,     1,     1,
       1,     2,     2,     1,     3,     2,     2,     1,     1,     1,
       3,     1,     3,     3,     1,     3,     3,     3,     1,     4,
       1,     2,     2,     1,     2,     1,     5,     4,     2,     1,
       3,     1,     3,     1,     3,     1,     1,     5,     1,     3,
       1,     3,     4,     5,     2,     1,     3,     1,     1,     2,
       3,     2,     3,     3,     4,     2,     3,     3,     4,     1,
       2,     2,     3,     2,     1,     1,     3,     4,     3,     4,
       4,     3
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
#line 71 "grammar.y" /* yacc.c:1646  */
    {
            	
            	fprintf ( parseLog, "PROGRAM ID '(' identifier_list ')' SEMICOLON declarations subprogram_declarations compound_statement\n");
            	assembly_codes = ".MODEL SMALL\n\n.STACK 100H\n";
            	getTemp();
            	getTemp();
            	assembly << assembly_codes<< "\n";
            	assembly << varDec;
            	assembly << "\n.CODE\n\nMAIN PROC\nMOV AX,@DATA\nMOV DS,AX\n";
            	
            	assembly << assembly_codes << "\n";
            	assembly << "\t\nmain endp\n";
            	assembly << "\n\n;PRINT FUNC\n";
            	assembly << "print proc  \n\tmov bp, sp\n\tmov ax, [bp+2]\n\tcmp ax, 0\n\tje return_print\n\t\n\tmov dx, 0\n\tmov bx, 10\n\tdiv bx\n\t\n\t;recalling\n\tpush dx\n\tpush ax\n\tcall print\n\t\n\t;printing\n\tpop dx\n\tadd dl, '0'\n\tmov ah, 2h\n\tint 21h\n\t\n\treturn_print:\n\t\tret 2\t\nprint endp\n";
//            	assembly << "\nEND MAIN\n";
            }
#line 1751 "y.tab.c++" /* yacc.c:1646  */
    break;


#line 1755 "y.tab.c++" /* yacc.c:1646  */
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
#line 458 "grammar.y" /* yacc.c:1906  */


int main(int argc,char *argv[]){
	adele();
	logout= fopen("log.txt","w");
	tokenout= fopen("token.txt","w");
	parseLog = fopen("parseLog", "w");
	//assembly= fopen("assembly.asm", "W");
	assembly.open("assembly.asm");
	fclose(tokenout);
	fclose(logout);
	yyparse();
	fclose(parseLog);
	printf ("\nTotal line Count: %d\n", line_count);
	parser_table.dump();
	
	return 0;
}
