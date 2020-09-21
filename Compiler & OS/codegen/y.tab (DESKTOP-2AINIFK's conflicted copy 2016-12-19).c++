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
    LBRAKET = 264,
    RBRAKET = 265,
    ID = 266,
    LTHIRD = 267,
    CONST = 268,
    RTHIRD = 269,
    FOR = 270,
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
    STRING = 306,
    OR_OP = 307,
    AND_OP = 308,
    EQ_OP = 309,
    NE_OP = 310,
    LEFT_OP = 311,
    GE_OP = 312,
    LE_OP = 313,
    RIGHT_OP = 314
  };
#endif
/* Tokens.  */
#define SEMICOLON 258
#define INT 259
#define FLOAT 260
#define CHAR 261
#define COMMA 262
#define TYPEDEF 263
#define LBRAKET 264
#define RBRAKET 265
#define ID 266
#define LTHIRD 267
#define CONST 268
#define RTHIRD 269
#define FOR 270
#define IF 271
#define STATIC 272
#define ELSE 273
#define WHILE 274
#define PRINTLN 275
#define RETURN 276
#define ASSIGNOP 277
#define LOGICOP 278
#define REGISTER 279
#define RELOP 280
#define ADDOP 281
#define MULOP 282
#define NOT 283
#define INCOP 284
#define DECOP 285
#define CONST_CHAR 286
#define CONST_INT 287
#define CONST_FLOAT 288
#define MAIN 289
#define TYPE_NAME 290
#define VOLATILE 291
#define AUTO 292
#define VOID 293
#define SHORT 294
#define LONG 295
#define DOUBLE 296
#define SIGNED 297
#define UNSIGNED 298
#define EXTERN 299
#define ENUM 300
#define IDENTIFIER 301
#define STRUCT 302
#define UNION 303
#define HEADER 304
#define NUMBER 305
#define STRING 306
#define OR_OP 307
#define AND_OP 308
#define EQ_OP 309
#define NE_OP 310
#define LEFT_OP 311
#define GE_OP 312
#define LE_OP 313
#define RIGHT_OP 314

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

#line 294 "y.tab.c++" /* yacc.c:358  */

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
#define YYLAST   856

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  81
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  60
/* YYNRULES -- Number of rules.  */
#define YYNRULES  181
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  302

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   314

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    75,    67,     2,
      76,    77,    73,    71,    63,    72,     2,    74,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    68,    62,
      69,    64,    70,    78,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    79,     2,    80,    66,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    60,    65,    61,     2,     2,     2,     2,
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
      55,    56,    57,    58,    59
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
  "COMMA", "TYPEDEF", "LBRAKET", "RBRAKET", "ID", "LTHIRD", "CONST",
  "RTHIRD", "FOR", "IF", "STATIC", "ELSE", "WHILE", "PRINTLN", "RETURN",
  "ASSIGNOP", "LOGICOP", "REGISTER", "RELOP", "ADDOP", "MULOP", "NOT",
  "INCOP", "DECOP", "CONST_CHAR", "CONST_INT", "CONST_FLOAT", "MAIN",
  "TYPE_NAME", "VOLATILE", "AUTO", "VOID", "SHORT", "LONG", "DOUBLE",
  "SIGNED", "UNSIGNED", "EXTERN", "ENUM", "IDENTIFIER", "STRUCT", "UNION",
  "HEADER", "NUMBER", "STRING", "OR_OP", "AND_OP", "EQ_OP", "NE_OP",
  "LEFT_OP", "GE_OP", "LE_OP", "RIGHT_OP", "'{'", "'}'", "';'", "','",
  "'='", "'|'", "'^'", "'&'", "':'", "'<'", "'>'", "'+'", "'-'", "'*'",
  "'/'", "'%'", "'('", "')'", "'?'", "'['", "']'", "$accept", "Program",
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
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     123,   125,    59,    44,    61,   124,    94,    38,    58,    60,
      62,    43,    45,    42,    47,    37,    40,    41,    63,    91,
      93
};
# endif

#define YYPACT_NINF -219

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-219)))

#define YYTABLE_NINF -82

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      21,     3,    44,    45,  -219,    67,    50,   384,  -219,  -219,
    -219,  -219,  -219,  -219,   621,   659,  -219,   117,   134,  -219,
     145,   149,   621,  -219,   430,   430,  -219,  -219,  -219,  -219,
    -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,  -219,   -19,
    -219,  -219,  -219,   808,   794,   337,   517,  -219,   164,  -219,
    -219,    95,    26,  -219,   140,   794,   794,   112,   104,   210,
     172,   303,   158,   216,    49,   176,    60,   249,   240,   211,
     201,  -219,  -219,   -20,  -219,  -219,  -219,   206,    16,   634,
      31,   621,   621,   218,    30,  -219,  -219,  -219,   256,   274,
     808,   808,   244,   151,  -219,   311,  -219,   543,  -219,  -219,
    -219,  -219,  -219,    -8,   -23,   223,  -219,    62,   259,    96,
     430,  -219,   621,   430,  -219,  -219,   621,   621,   430,   430,
     430,  -219,   621,   621,   621,   621,   264,   808,   621,   621,
     621,   621,   621,   621,  -219,  -219,   621,   621,   621,   621,
     621,   621,   621,   621,   621,  -219,   313,    37,    31,    25,
      33,   315,  -219,   274,   267,   124,  -219,  -219,  -219,   621,
     260,   116,  -219,   165,   193,   320,  -219,  -219,    -8,  -219,
     257,  -219,   -23,    96,   162,   429,   583,  -219,  -219,   271,
    -219,  -219,   104,  -219,   158,   312,   172,  -219,  -219,   216,
     176,   176,   808,  -219,   734,   106,    60,    60,    60,    60,
     211,   211,   112,   201,   201,  -219,  -219,  -219,   240,    66,
    -219,  -219,   621,   570,   570,   344,   128,   621,  -219,   274,
    -219,  -219,   105,   286,  -219,   278,   282,  -219,   280,  -219,
     474,   596,   165,   319,  -219,  -219,  -219,  -219,   162,  -219,
    -219,  -219,  -219,    51,   285,  -219,   284,   749,  -219,  -219,
     621,  -219,   247,   318,   621,    35,   365,  -219,  -219,  -219,
    -219,  -219,   215,  -219,    77,  -219,   794,  -219,  -219,  -219,
    -219,   314,  -219,   316,   380,  -219,   150,   356,  -219,  -219,
    -219,  -219,  -219,  -219,   106,   621,  -219,   570,   570,  -219,
    -219,  -219,  -219,  -219,   609,  -219,  -219,  -219,  -219,  -219,
    -219,  -219
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     0,    15,     0,     2,    47,
       6,     8,     4,    67,     0,    26,    60,     0,     0,    69,
       0,     0,     0,    71,     0,     0,   110,   108,   109,    14,
      61,    70,     3,     5,     7,     9,    10,    11,    68,     0,
      91,    92,    19,     0,    32,     0,     0,    20,     0,    22,
      40,   106,     0,    54,    56,    34,    30,   119,    73,    58,
      79,   128,    75,    86,     0,    88,    98,    85,   143,   103,
     121,   124,    12,   146,   150,   148,    13,    52,     0,     0,
       0,     0,     0,     0,     0,   106,    83,    84,   154,     0,
     133,   135,     0,   130,    33,     0,    17,     0,    21,    16,
      23,   175,    28,   169,     0,     0,    36,     0,    38,   174,
       0,    48,     0,     0,    35,    31,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,   138,     0,     0,     0,
       0,     0,     0,     0,   111,   112,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   107,   108,     0,     0,     0,
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
    -219,  -219,   -30,  -219,  -219,   361,   362,   153,  -219,   237,
     -45,   -68,  -145,   219,     2,   -12,   304,   305,   -35,  -218,
    -219,  -219,  -219,   294,   322,  -219,   325,   317,    -7,   323,
     321,  -219,   163,  -219,   187,    73,  -219,  -219,   183,  -139,
     306,   182,   181,   108,  -219,    75,  -219,  -219,   297,  -219,
    -146,  -130,  -108,  -219,   232,   -87,  -142,   -82,  -101,   -88
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
      61,   100,    78,   170,   180,    16,   162,    61,    91,    51,
      84,   164,   148,    90,   188,    61,    51,    86,    87,   173,
     275,   169,   232,   101,    51,     1,   145,    88,    30,   111,
     246,   229,   142,   152,     9,   213,   244,     3,    61,    61,
      14,    89,    77,   214,     4,   287,   229,    51,    51,   249,
     103,   211,   100,   104,     5,    91,    91,    24,   143,    25,
      90,    90,    26,    27,    28,   103,   240,   147,   167,   149,
     150,   260,    61,    61,    61,    61,   301,     6,   164,   112,
     212,    51,    51,    51,    51,   273,   235,   229,   112,   112,
      61,   271,    91,   112,   253,   126,   112,    90,   112,    51,
     112,   229,   249,   178,   282,   179,   178,    43,   101,   127,
       7,   178,   178,   187,   277,   179,   132,   110,    93,   133,
     229,   265,   232,   101,   286,    14,    80,    77,   278,   112,
     240,   209,   147,   234,   254,   263,    61,    61,   104,   297,
     264,    61,    24,    81,    25,    51,    51,    26,    27,    28,
      51,   101,   101,   262,    82,   229,   161,    91,    83,    91,
      48,   170,    90,   113,    90,   157,   158,   179,   256,   257,
     117,    14,   175,    77,   250,   176,   173,   116,   103,   103,
     264,   262,   104,   253,   161,   218,   240,   219,    24,   259,
      25,   219,    43,    26,    27,    28,   227,    94,    48,   120,
     255,   196,   197,   198,   199,    61,    61,    61,   114,   115,
     101,   293,    91,   294,    51,    51,    51,    90,   144,    10,
      11,    12,   238,    13,   103,   123,   102,   160,    16,   151,
     161,   179,    19,   128,   129,   118,   119,   103,    43,    23,
     104,   230,   298,   299,   231,   130,   131,   205,   206,   207,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,   101,    40,    41,    10,    11,    12,   220,    13,   160,
     124,   125,   161,    16,   139,   140,   141,    19,   134,   135,
      61,    61,   137,   138,    23,   171,   172,   179,   103,    51,
      51,   262,   221,   136,   161,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,   -81,    40,    41,   283,
     284,   190,   191,   -81,   200,   201,   153,   -81,   203,   204,
     154,   159,   165,   174,   192,   215,   -81,   210,   -81,   -81,
     -81,   217,   233,   103,   236,   121,   160,   221,   119,   161,
       9,    10,    11,    12,    95,    13,    14,   258,    77,   266,
      16,   274,    17,    18,    19,   267,    20,    21,    22,   268,
     269,    23,   279,    24,   280,    25,   -81,   121,    26,    27,
      28,   -81,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,   288,    40,    41,   285,     9,    10,    11,
      12,   290,    13,    14,   292,    15,   291,    16,    96,    17,
      18,    19,   295,    20,    21,    22,    97,    98,    23,   237,
      24,   247,    25,    43,   177,    26,    27,    28,   181,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
     202,    40,    41,    10,    11,    12,   186,    13,   182,    14,
     184,    77,    16,   185,   189,    42,    19,   296,   208,   289,
     216,   261,     0,    23,     0,     0,    24,     0,    25,     0,
      43,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,   241,    40,    41,    10,    11,
      12,     0,    13,     0,     0,     0,     0,    16,     0,     0,
       0,    19,     0,     0,     0,     0,     0,     0,    23,     0,
       0,     0,     0,     0,     0,     0,   242,     0,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       9,    40,    41,     0,     0,     0,    14,     0,    77,     0,
       0,     0,    17,    18,     0,     0,    20,    21,    22,     0,
       0,     0,     0,    24,     0,    25,     9,     0,    26,    27,
      28,   270,    14,     0,    77,     0,     0,     0,    17,    18,
       0,     0,    20,    21,    22,     0,     0,     0,     0,    24,
       0,    25,     0,     9,    26,    27,    28,     0,    99,    14,
       0,    77,     0,     0,     0,    17,    18,     0,     0,    20,
      21,    22,    14,    43,    77,     0,    24,     0,    25,     0,
       0,    26,    27,    28,   166,    14,     0,    77,     0,    24,
       0,    25,     0,     0,    26,    27,    28,     0,    14,    43,
      77,     0,    24,     0,    25,     0,     0,    26,    27,    28,
      14,     0,    77,     0,     0,    24,     0,    25,     0,     0,
      26,    27,    28,    14,     0,    77,    43,    24,     0,    25,
       0,     0,    26,    27,    28,     0,     0,     0,     0,    43,
      24,     0,    25,   245,     0,    26,   146,    28,     0,   238,
     300,    79,    43,     0,     0,     0,   272,     0,     0,     0,
       0,   -52,   -52,     0,   -52,    43,   -52,     0,   -52,   -52,
       0,     0,     0,     0,     0,     0,     0,    43,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      43,   -52,   -52,   -52,   -52,   -52,   -52,   -52,   -52,     0,
       0,     0,   -52,   -52,   -52,   -52,   -52,     0,   -52,   -52,
     -52,   -52,   -52,   -52,   -52,     0,     0,   -52,    10,    11,
      12,     0,     0,     0,     0,     0,     0,    16,     0,     0,
       0,     0,     0,    10,    11,    12,     0,     0,     0,     0,
       0,     0,    16,     0,     0,     0,     0,     0,     0,    29,
      30,     0,    32,    33,    34,    35,    36,    37,     0,    39,
       0,    40,    41,     0,    29,    30,     0,    32,    33,    34,
      35,    36,    37,     0,    39,   248,    40,    41,    10,    11,
      12,     0,    13,     0,     0,     0,     0,    16,     0,     0,
     281,    19,    10,    11,    12,     0,     0,     0,    23,     0,
       0,    16,     0,     0,     0,     0,     0,     0,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    29,    30,     0,    32,    33,    34,    35,
      36,    37,     0,    39,     0,    40,    41
};

static const yytype_int16 yycheck[] =
{
       7,    46,    14,   104,   112,    13,    93,    14,    43,     7,
      22,    93,    80,    43,   122,    22,    14,    24,    25,   107,
     238,   103,   164,    46,    22,     4,    10,    46,    36,     3,
     176,   161,    52,     3,     3,    10,   175,    34,    45,    46,
       9,    60,    11,    10,     0,    10,   176,    45,    46,   194,
      73,    14,    97,    76,     9,    90,    91,    26,    78,    28,
      90,    91,    31,    32,    33,    73,   174,    79,   103,    81,
      82,   217,    79,    80,    81,    82,   294,    10,   160,    63,
     148,    79,    80,    81,    82,   231,   168,   217,    63,    63,
      97,   230,   127,    63,   195,    46,    63,   127,    63,    97,
      63,   231,   247,   110,   250,   112,   113,    76,    46,    60,
      60,   118,   119,   120,    63,   122,    56,    22,    43,    59,
     250,   222,   264,    46,   254,     9,     9,    11,    77,    63,
     238,   143,   144,   168,    68,   222,   143,   144,    76,   285,
     222,   148,    26,     9,    28,   143,   144,    31,    32,    33,
     148,    46,    46,    76,     9,   285,    79,   192,     9,   194,
       7,   262,   192,    23,   194,    90,    91,   174,   213,   214,
      66,     9,    76,    11,    68,    79,   264,    65,    73,    73,
     262,    76,    76,   284,    79,    61,   294,    63,    26,    61,
      28,    63,    76,    31,    32,    33,    80,    44,    45,    27,
     212,   128,   129,   130,   131,   212,   213,   214,    55,    56,
      46,    61,   247,    63,   212,   213,   214,   247,    12,     4,
       5,     6,    60,     8,    73,    67,    62,    76,    13,    11,
      79,   238,    17,    57,    58,    25,    26,    73,    76,    24,
      76,    76,   287,   288,    79,    69,    70,   139,   140,   141,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,     4,     5,     6,   159,     8,    76,
      54,    55,    79,    13,    73,    74,    75,    17,    29,    30,
     287,   288,    71,    72,    24,    62,    63,   294,    73,   287,
     288,    76,    77,    53,    79,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     3,    47,    48,    62,
      63,   124,   125,    10,   132,   133,    60,    14,   137,   138,
      46,    77,    11,    64,    60,    10,    23,    14,    25,    26,
      27,    64,    12,    73,    77,    64,    76,    77,    26,    79,
       3,     4,     5,     6,     7,     8,     9,     3,    11,    63,
      13,    32,    15,    16,    17,    77,    19,    20,    21,    77,
      80,    24,    77,    26,    80,    28,    63,    64,    31,    32,
      33,    68,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    18,    47,    48,    68,     3,     4,     5,
       6,    77,     8,     9,    14,    11,    80,    13,    61,    15,
      16,    17,    46,    19,    20,    21,    45,    45,    24,   172,
      26,   192,    28,    76,   110,    31,    32,    33,   113,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     136,    47,    48,     4,     5,     6,   119,     8,   116,     9,
     117,    11,    13,   118,   123,    61,    17,   284,   142,   266,
     153,   219,    -1,    24,    -1,    -1,    26,    -1,    28,    -1,
      76,    31,    32,    33,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,     4,     5,
       6,    -1,     8,    -1,    -1,    -1,    -1,    13,    -1,    -1,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    24,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    77,    -1,    -1,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       3,    47,    48,    -1,    -1,    -1,     9,    -1,    11,    -1,
      -1,    -1,    15,    16,    -1,    -1,    19,    20,    21,    -1,
      -1,    -1,    -1,    26,    -1,    28,     3,    -1,    31,    32,
      33,    77,     9,    -1,    11,    -1,    -1,    -1,    15,    16,
      -1,    -1,    19,    20,    21,    -1,    -1,    -1,    -1,    26,
      -1,    28,    -1,     3,    31,    32,    33,    -1,    61,     9,
      -1,    11,    -1,    -1,    -1,    15,    16,    -1,    -1,    19,
      20,    21,     9,    76,    11,    -1,    26,    -1,    28,    -1,
      -1,    31,    32,    33,    61,     9,    -1,    11,    -1,    26,
      -1,    28,    -1,    -1,    31,    32,    33,    -1,     9,    76,
      11,    -1,    26,    -1,    28,    -1,    -1,    31,    32,    33,
       9,    -1,    11,    -1,    -1,    26,    -1,    28,    -1,    -1,
      31,    32,    33,     9,    -1,    11,    76,    26,    -1,    28,
      -1,    -1,    31,    32,    33,    -1,    -1,    -1,    -1,    76,
      26,    -1,    28,    80,    -1,    31,    32,    33,    -1,    60,
      61,    12,    76,    -1,    -1,    -1,    80,    -1,    -1,    -1,
      -1,    22,    23,    -1,    25,    76,    27,    -1,    29,    30,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    52,    53,    54,    55,    56,    57,    58,    59,    -1,
      -1,    -1,    63,    64,    65,    66,    67,    -1,    69,    70,
      71,    72,    73,    74,    75,    -1,    -1,    78,     4,     5,
       6,    -1,    -1,    -1,    -1,    -1,    -1,    13,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,    35,
      36,    -1,    38,    39,    40,    41,    42,    43,    -1,    45,
      -1,    47,    48,    -1,    35,    36,    -1,    38,    39,    40,
      41,    42,    43,    -1,    45,    61,    47,    48,     4,     5,
       6,    -1,     8,    -1,    -1,    -1,    -1,    13,    -1,    -1,
      61,    17,     4,     5,     6,    -1,    -1,    -1,    24,    -1,
      -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    47,    48,    35,    36,    -1,    38,    39,    40,    41,
      42,    43,    -1,    45,    -1,    47,    48
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     4,    82,    34,     0,     9,    10,    60,    84,     3,
       4,     5,     6,     8,     9,    11,    13,    15,    16,    17,
      19,    20,    21,    24,    26,    28,    31,    32,    33,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      47,    48,    61,    76,    83,    85,    86,    87,    88,    91,
      92,    95,    96,    97,    98,    99,   102,   104,   105,   107,
     108,   109,   110,   111,   112,   115,   116,   117,   121,   122,
     123,   124,   127,   130,   132,   133,   134,    11,    96,    12,
       9,     9,     9,     9,    96,    95,   109,   109,    46,    60,
      83,    99,   125,   126,    88,     7,    61,    86,    87,    61,
      91,    46,    62,    73,    76,    89,    90,   138,   139,   140,
      22,     3,    63,    23,    88,    88,    65,    66,    25,    26,
      27,    64,   103,    67,    54,    55,    46,    60,    57,    58,
      69,    70,    56,    59,    29,    30,    53,    71,    72,    73,
      74,    75,    52,    78,    12,    10,    32,    96,    92,    96,
      96,    11,     3,    60,    46,   129,   135,   126,   126,    77,
      76,    79,   136,   137,   138,    11,    61,    99,   106,   138,
     139,    62,    63,   140,    64,    76,    79,    97,   109,   109,
     133,    98,   105,   109,   110,   107,   108,   109,   133,   111,
     115,   115,    60,    93,    94,   126,   116,   116,   116,   116,
     122,   122,   104,   123,   123,   124,   124,   124,   121,    96,
      14,    14,    92,    10,    10,    10,   129,    64,    61,    63,
     124,    77,    88,   118,   119,   120,   136,    80,   131,   132,
      76,    79,   137,    12,    99,   138,    77,    90,    60,   100,
     133,    46,    77,   114,   120,    80,   131,    94,    61,    93,
      68,   113,   128,   139,    68,    96,    91,    91,     3,    61,
     131,   135,    76,   136,   138,   139,    63,    77,    77,    80,
      77,   120,    80,   131,    32,   100,   101,    63,    77,    77,
      80,    61,   131,    62,    63,    68,   132,    10,    18,   119,
      77,    80,    14,    61,    63,    46,   113,   131,    91,    91,
      61,   100
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    81,    82,    83,    83,    83,    83,    83,    83,    83,
      83,    83,    83,    83,    83,    84,    84,    84,    84,    84,
      85,    85,    86,    86,    85,    85,    85,    85,    87,    87,
      88,    88,    88,    88,    88,    88,    89,    89,    90,    90,
      91,    91,    91,    91,    91,    91,    91,    92,    92,    93,
      94,    94,    95,    95,    96,    96,    97,    97,    98,    98,
      99,    99,   100,   100,   100,   101,   101,   102,   102,   102,
     102,   102,   103,   104,   104,   105,   105,   106,   106,   107,
     107,   108,   108,   109,   109,   109,   110,   110,   111,   111,
     111,   112,   112,   113,   113,   113,   114,   114,   115,   115,
     115,   115,   115,   116,   116,   116,   117,   117,   117,   117,
     117,   117,   117,   118,   118,   119,   119,   119,   120,   121,
     121,   122,   122,   122,   123,   123,   123,   123,   124,   124,
     125,   125,   126,   126,   126,   126,   127,   127,   127,   128,
     128,   129,   129,   130,   130,   131,   132,   132,    96,    96,
     133,   133,   134,   134,   134,   135,   135,   136,   136,   136,
     137,   137,   137,   137,   137,   137,   137,   137,   137,   138,
     138,   138,   138,   139,   139,   140,   140,   140,   140,   140,
     140,   140
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
#line 1741 "y.tab.c++" /* yacc.c:1646  */
    break;


#line 1745 "y.tab.c++" /* yacc.c:1646  */
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
