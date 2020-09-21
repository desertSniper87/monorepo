%{
#include <iostream>
#include <stdio.h>
#include "Symbol_table.h"
#include "y.tab.hpp"
#include <string.h>
#include <stdlib.h>

using namespace std;


/*extern "C"*/
/*{*/
    /*extern int yylex(void);*/
/*}*/

int line_count = 1;
Symbol_table table;

extern FILE *parseLog;
extern FILE *tokenout;
extern FILE *parseLog;
/*parselog = fopen("log.txt", w)*/

/*extern YYSTYPE yylval;*/
void yyerror(char *); 

int hash_buckets = 7;

void insert(const char* token, char* yytext) {
	//printf("%s: %s ", token, yytext);
	/*printf("%s ", token);*/
	table.insert(token, yytext, hash_buckets);
}
%}        

newLine			\n
delim 			[  \t]
digit			[0-9]
unsigned		{digit}+
exponent		[eE][+-]?{unsigned}
number			{digit}*(\.{digit}+)
letter			[A-Za-z]
/*keyword         program|if|not|end|begin|else|then|do|while|function|Procedure|integer|real|oh|array|write|include|int|char|float|string|printf*/
prnt_stmt       [printf](\.|\n)*{semicolon}
sin_comment		\/\/.*
mul_comment_err	"/**"[^"**/"]+
mul_comment		"/*"(([^*]|(("*"+)[^*/]))*)("*"+)"/"
string			\"((\\.|[^"\n]))+\"
err_str_nl_2    ^\"(\\.|[^"])*[\n]+
err_str_nl      \"(\\.|[^"])*\"
empty_char      \'\'
err_sp_char     \'[a-zA-Z]+[ ]+[a-zA-Z]+\'
char			\'{letter}\'
err_multi_char	\'((\\.|[^'\n])-[ ]+)+\'
err_char_nl     ^\'(\\.|[^'])*\n
id				(_|{letter})({letter}|{digit}|_)*
addop			[\+-]|or
mulop			[\*\/\%]|div|mod|and
relop			==|<>|<|<=|>=|>|!=
logicop         \&\&|\|\||!
assignop		=
dotdot			\.\.
semicolon       ;
hash			#

err_dotdot      {number}\.{unsigned}{exponent}?
err_id 			{digit}+{id}

%%

"{"				{return LCURL;}

"}"				{return RCURL;}

"("				{
    /*printf("found LPAREN\n");*/
    fprintf(parseLog,"\nLine no %d: Token <LPAREN> lexeme <%s> found\n",line_count,yytext);
    return LPAREN;}

")"				{
    /*printf("found RPAREN\n");*/
    fprintf(parseLog,"\nLine no %d: Token <RPAREN> lexeme <%s> found\n",line_count,yytext);
    return RPAREN;}

"++"				{
    /*printf("found INCOP\n");*/
    fprintf(parseLog,"\nLine no %d: Token <INCOP> lexeme <%s> found\n",line_count,yytext);
    return INCOP;}

"--"				{
    /*printf("found DECOP\n");*/
    fprintf(parseLog,"\nLine no %d: Token <DECOP> lexeme <%s> found\n",line_count,yytext);
    return DECOP;}

"["				{return LSQBRAC;}

"]"				{return RSQBRAC;}

","             {
    /*printf("Found COMMA\n");*/
    return COMMA;}


"char"			{ return  CHAR; }

"else"			{ return  ELSE; }

"float"			{ return  FLOAT; }

"for"			{ return  FOR; }

"if"			{ return  IF; }

"int"			{
    /*printf("found INT\n");*/
    return  INT; }

"return"		{ return  RETURN; }

"void"			{ return  VOID; }

"while"			{ return  WHILE; }

"println"       { return  PRINTLN; }

{newLine} {
	line_count++;
}

"\n\n" {
    line_count = line_count + 2;
    table.print(parseLog);
}

{string} {
	insert("STR",yytext);
	/*printf("str: %s\n", yytext);*/
	/*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"\n<STRING, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d:Token <STRING> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"STRING");
    yylval = s;

    /*return STRING;*/
}


{err_str_nl} {
    fprintf(tokenout,"\n<STRING, %s>\n",yytext);
    fprintf(parseLog,"\n ERROR string with newline found\nLine no %d:Token <STRING> lexeme <%s> found\n",line_count,yytext);
}

{char} {
	insert("CHAR",yytext);
    /*printf("str: %s\n", yytext);*/
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"\n<CHAR, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <CHAR> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"CHAR");
    yylval = s;
}

{err_multi_char} {
	insert("CHAR",yytext);
    /*printf("str: %s\n", yytext);*/
    /*printf("line no: %d\n",line_count);*/
    fprintf(parseLog,"\nWARNING: Multi character constant found\nLine no %d: Token <CHAR> lexeme <%s> found\n",line_count,yytext);
    fprintf(tokenout,"\n<CHAR, %s>\n",yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"CHAR");
    yylval = s;

}

{err_char_nl} {
    /*printf("str: %s\n", yytext);*/
    /*printf("line no: %d\n",line_count);*/
    fprintf(parseLog,"\nERROR: Character with newline found\nLine no %d: Token <CHAR> lexeme <%s> found\n",line_count,yytext);
}

{empty_char} {
    /*printf("str: %s\n", yytext);*/
    /*printf("line no: %d\n",line_count);*/
    fprintf(parseLog,"\nERROR: Empty character found\nLine no %d: Token <CHAR> lexeme <%s> found\n",line_count,yytext);
}

	/*insert("KEYWORD",yytext);*/
	/*printf("keyword");*/
	/*fprintf(tokenout,"\n<KEYWORD, %s>\n",yytext);*/
	/*fprintf(parseLog,"\nLine no %d: Token <KEYWORD> lexeme <%s> found\n",line_count,yytext);*/
    /*return KEYWORD;*/
	

	/*insert("header",yytext);*/
    /*fprintf(tokenout,"\n<HEADER %s>\n",yytext);*/
    /*fprintf(parseLog,"\nLine no %d: Token <HEADER> lexeme <%s> found\n",line_count,yytext);*/
    /*[>return HEADER;<]*/


{id} {
	insert("ID",yytext);
    printf("id: %s\n", yytext); 
    fprintf(tokenout,"<ID, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <ID> lexeme <%s> found\n",line_count,yytext);
    printf("found ID.\n");

    Symbol_info *s= new  Symbol_info(yytext, (char *)"ID");
    yylval = s;

    return ID;
}

{sin_comment} {
    /*printf("\nSingleline comment found: %s\n",yytext);*/
    fprintf(parseLog,"\nLine no %d: SINGLELINECOMMENT <%s> found\n",line_count,yytext);
}

{mul_comment} {
    /*printf("\nMultiline comment found: %s\n",yytext);*/
    fprintf(parseLog,"\nLine no %d: MULTILINECOMMENT <%s> found\n",line_count,yytext);
}

{mul_comment_err} {
    /*printf("\nMultiline comment found: %s\n",yytext);*/
    fprintf(parseLog,"\nLine no %d: Unterminated MULTILINECOMMENT <%s> found\n",line_count,yytext);
}

{number} {
	insert("NUMBER",yytext);
    /*printf("number:%s\n",yytext);*/
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"<NUMBER, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <NUMBER> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"CONST_FLOAT");
    yylval = s;

    return CONST_FLOAT;
}

{unsigned} {
	insert("UNSIGNED",yytext);
    /*printf("unsigned:%s\n",yytext);*/
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"<UNSIGNED, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <UNSIGNED> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"CONST_INT");
    yylval = s;

    return CONST_INT;
}

{addop} {
    insert("ADDOP", yytext);
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"\n<ADDOP, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <ADDOP> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"ADDOP");
    yylval = s;

    return ADDOP;
}

{mulop} {
    insert("MULOP", yytext);
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"\n<MULOP, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <MULOP> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"MULOP");
    yylval = s;

    return MULOP;
}

{relop} {
    /*relop is relational operator*/
    insert("RELOP", yytext);
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"\n<RELOP, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <RELOP> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"RELOP");
    yylval = s;

    return RELOP;
}

{logicop} {
    insert("LOGICOP", yytext);
    /*printf("line no: %d\n",line_count);*/
    fprintf(tokenout,"\n<LOGICOP, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <LOGICOP> lexeme <%s> found\n",line_count,yytext);

    Symbol_info *s= new  Symbol_info(yytext, (char *)"LOGICOP");
    yylval = s;

    return LOGICOP;
}

{assignop} {
    insert("ASSIGNOP", yytext);
    /*printf("line no: %d\n",line_count);*/
    /*printf("ASSIGNOP found\n");*/
    fprintf(tokenout,"\n<ASSIGNOP, %s>\n",yytext);
    fprintf(parseLog,"\nLine no %d: Token <ASSIGNOP> lexeme <%s> found\n",line_count,yytext);
    return ASSIGNOP;
}


{dotdot} {
	insert("DOTDOT", yytext);
    /*printf("line no: %d\n",line_count);*/
}	
{hash}  {
	insert("#",yytext);
}

{delim}+ {}

{err_dotdot} {
    /*printf("Illegal usage of decimal\n");*/
    fprintf(parseLog,"\nLine no %d: Illegal usage of decimal <%s> found\n",line_count,yytext);
}

{err_id} {
	/*printf("Illegal id\n");*/
	/*fprintf(parseLog,"\nLine no %d: Illegal usage of id <%s> found\n",line_count,yytext);*/
}

    /*{prnt_stmt} {*/
	/*printf("Illegal id\n");*/
	/*fprintf(parseLog,"\nLine no %d: Printf statement<%s> found\n",line_count,yytext);*/
    /*return PRINTLN;*/
    /*}*/

{semicolon} {
    /*printf("found Semicolon");*/
    return SEMICOLON;
}

%%

int yywrap(void)    {
    return 1;
}
