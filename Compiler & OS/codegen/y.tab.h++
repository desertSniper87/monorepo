/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
