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
/* Tokens.  */
#define SEMICOLON 258
#define INT 259
#define FLOAT 260
#define CHAR 261
#define COMMA 262
#define TYPEDEF 263
#define ID 264
#define LTHIRD 265
#define CONST 266
#define RTHIRD 267
#define FOR 268
#define LPAREN 269
#define RPAREN 270
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
#define OR_OP 306
#define AND_OP 307
#define EQ_OP 308
#define GE_OP 309
#define LEFT_OP 310
#define LE_OP 311
#define NE_OP 312
#define RIGHT_OP 313

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
