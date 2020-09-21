
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
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


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ID = 258,
     SEMICOLON = 259,
     NUM = 260,
     ASSIGNOP = 261,
     IF = 262,
     ELSE = 263,
     RELOP = 264,
     ADDOP1 = 265,
     ADDOP2 = 266,
     MULOP1 = 267,
     MULOP2 = 268,
     PAREN1 = 269,
     PAREN2 = 270,
     BRACKET1 = 271,
     BRACKET2 = 272,
     OR = 273,
     AND = 274,
     CURLEY1 = 275,
     CURLEY2 = 276,
     MULOP = 277,
     IFX = 278
   };
#endif
/* Tokens.  */
#define ID 258
#define SEMICOLON 259
#define NUM 260
#define ASSIGNOP 261
#define IF 262
#define ELSE 263
#define RELOP 264
#define ADDOP1 265
#define ADDOP2 266
#define MULOP1 267
#define MULOP2 268
#define PAREN1 269
#define PAREN2 270
#define BRACKET1 271
#define BRACKET2 272
#define OR 273
#define AND 274
#define CURLEY1 275
#define CURLEY2 276
#define MULOP 277
#define IFX 278




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;


