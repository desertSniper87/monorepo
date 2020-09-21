/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

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
    PROGRAM = 258,
    THEN = 259,
    REAL = 260,
    IF = 261,
    DO = 262,
    VAR = 263,
    NOT = 264,
    WHILE = 265,
    OF = 266,
    END = 267,
    FUNCTION = 268,
    ARRAY = 269,
    BEGINNING = 270,
    PROCEDURE = 271,
    WRITE = 272,
    ELSE = 273,
    INTEGER = 274,
    LETTER = 275,
    DIGIT = 276,
    UNDERSCORE = 277,
    NUM = 278,
    ILL_NUM = 279,
    ID = 280,
    ILL_ID = 281,
    VALID_STRING = 282,
    ILL_STRING = 283,
    INCOMPLETE_STRING = 284,
    VALID_COMMENT = 285,
    ILL_COMMENT = 286,
    INCOMPLETE_COMMENT = 287,
    RELOP = 288,
    ADDOP = 289,
    MULOP = 290,
    ASSIGNOP = 291,
    BRACKET1 = 292,
    BRACKET2 = 293,
    PAREN1 = 294,
    PAREN2 = 295,
    DOTDOT = 296,
    COMMA = 297,
    SEMICOLON = 298,
    COLON = 299,
    DOT = 300,
    UNRECO = 301,
    AND = 302,
    OR = 303,
    IFX = 304
  };
#endif
/* Tokens.  */
#define PROGRAM 258
#define THEN 259
#define REAL 260
#define IF 261
#define DO 262
#define VAR 263
#define NOT 264
#define WHILE 265
#define OF 266
#define END 267
#define FUNCTION 268
#define ARRAY 269
#define BEGINNING 270
#define PROCEDURE 271
#define WRITE 272
#define ELSE 273
#define INTEGER 274
#define LETTER 275
#define DIGIT 276
#define UNDERSCORE 277
#define NUM 278
#define ILL_NUM 279
#define ID 280
#define ILL_ID 281
#define VALID_STRING 282
#define ILL_STRING 283
#define INCOMPLETE_STRING 284
#define VALID_COMMENT 285
#define ILL_COMMENT 286
#define INCOMPLETE_COMMENT 287
#define RELOP 288
#define ADDOP 289
#define MULOP 290
#define ASSIGNOP 291
#define BRACKET1 292
#define BRACKET2 293
#define PAREN1 294
#define PAREN2 295
#define DOTDOT 296
#define COMMA 297
#define SEMICOLON 298
#define COLON 299
#define DOT 300
#define UNRECO 301
#define AND 302
#define OR 303
#define IFX 304

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
