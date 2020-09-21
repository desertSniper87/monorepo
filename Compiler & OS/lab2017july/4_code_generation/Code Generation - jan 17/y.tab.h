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

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
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
    IF = 258,
    ELSE = 259,
    FOR = 260,
    WHILE = 261,
    DO = 262,
    INT = 263,
    FLOAT = 264,
    DOUBLE = 265,
    CHAR = 266,
    RETURN = 267,
    VOID = 268,
    BREAK = 269,
    SWITCH = 270,
    CASE = 271,
    DEFAULT = 272,
    CONTINUE = 273,
    ADDOP = 274,
    MULOP = 275,
    ASSIGNOP = 276,
    RELOP = 277,
    LOGICOP = 278,
    SEMICOLON = 279,
    COMMA = 280,
    LPAREN = 281,
    RPAREN = 282,
    LCURL = 283,
    RCURL = 284,
    LTHIRD = 285,
    RTHIRD = 286,
    INCOP = 287,
    DECOP = 288,
    CONST_INT = 289,
    CONST_FLOAT = 290,
    ID = 291,
    NOT = 292,
    PRINTLN = 293,
    MAIN = 294,
    THEN = 295
  };
#endif
/* Tokens.  */
#define IF 258
#define ELSE 259
#define FOR 260
#define WHILE 261
#define DO 262
#define INT 263
#define FLOAT 264
#define DOUBLE 265
#define CHAR 266
#define RETURN 267
#define VOID 268
#define BREAK 269
#define SWITCH 270
#define CASE 271
#define DEFAULT 272
#define CONTINUE 273
#define ADDOP 274
#define MULOP 275
#define ASSIGNOP 276
#define RELOP 277
#define LOGICOP 278
#define SEMICOLON 279
#define COMMA 280
#define LPAREN 281
#define RPAREN 282
#define LCURL 283
#define RCURL 284
#define LTHIRD 285
#define RTHIRD 286
#define INCOP 287
#define DECOP 288
#define CONST_INT 289
#define CONST_FLOAT 290
#define ID 291
#define NOT 292
#define PRINTLN 293
#define MAIN 294
#define THEN 295

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
