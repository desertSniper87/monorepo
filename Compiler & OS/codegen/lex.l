%{
#include <stdio.h>
#include "SymbolTable.h"
#include "y.tab.h++"

//extern "C"
//{
//   extern int yylex(void);
//   extern int yyparse(void);
//}

int line_count = 1;
SymbolTable table;
//static int yywrap(void);

void insert(const char* token, char* yytext) {
	////printf ("%s: %s ", token, yytext);
	//printf ("%s ", token);
	SymbolInfo symbolInfo(yytext, token);
	table.insert(symbolInfo, line_count);
}
%}

newLine			\n
delim 			[  \t]
digit			[0-9]
unsigned		{digit}+
exponent		[eE][+-]?{unsigned}
number			[-+]?({unsigned}|{unsigned}\.{unsigned}?|{unsigned}?\.{unsigned}){exponent}?
letter			[A-Za-z]

id				(_|{letter})({letter}|{digit}|_)*
header			{id}\.h
sin_comment		\/\/.*
mul_comment		"/*"(([^*]|(("*"+)[^*/]))*)("*"+)"/"
string			\"(\\.|[^"])*\"
char			\'(\\.|[^'])*\'
addop			[\+-]|or
mulop			[\*\/]|div|mod|and
orop			||
leop			<=
geop			>=
eqop			==
neop			!=
assignop		=
dotdot			\.\.
brace			[\[\]\(\)]
other			[,;:]
hash			#
err_dotdot		[0-9]+\.([0-9]+\.){1,}
err_id 			{digit}+{id}





%%

"{"				{return LCURL;}

"}"				{return RCURL;}

"("				{return LBRAKET;}

")"				{return RBRAKET;}

"char"			{ return  CHAR; }

"const"			{ return  CONST; }

"double"		{ return  DOUBLE; }

"else"			{ return  ELSE; }

"enum"			{ return  ENUM; }

"extern"		{ return  EXTERN; }

"float"			{ return  FLOAT; }

"for"			{ return  FOR; }

"if"			{ return  IF; }

"int"			{ return  INT; }

"long"			{ return  LONG; }

"return"		{ return  RETURN; }

"short"			{ return  SHORT; }

"signed"		{ return  SIGNED; }

"static"		{ return  STATIC; }

"struct"		{ return  STRUCT; }

"typedef"		{ return  TYPEDEF; }

"union"			{ return  UNION; }

"unsigned"		{ return  UNSIGNED; }

"void"			{ return  VOID; }

"volatile"		{ return  VOLATILE; }

"while"			{ return  WHILE; }

"main"			{ return MAIN; }


{newLine} {
	line_count++;
}

{string} {
	//printf ("str: %s\n", yytext);
	//printf ("line no: %d\n",line_count);
	//fprintf(tokenout,"\n<STRING, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: STRING <%s> found\n",line_count,yytext);
	return STRING;
}

{char} {
	//printf ("str: %s\n", yytext);
	//printf ("line no: %d\n",line_count);
	//fprintf(tokenout,"\n<CHAR, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: CHAR <%s> found\n",line_count,yytext);
	return CHAR;
}

{header} {
	insert("header",yytext);
	//fprintf(tokenout,"\n<HEADER %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: HEADER <%s> found\n",line_count,yytext);

}

{id} {
	SymbolInfo symbolInfo(yytext, "ID");
	table.insert(symbolInfo, line_count);
	//printf ("id: %s\n", yytext); 
	//fprintf(tokenout,"<ID, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: ID <%s> found\n",line_count,yytext);
	return ID;
}

{sin_comment} {
	//printf ("\nSingleline comment found: %s\n",yytext);
	//fprintf(logout,"\nLine no %d: SINGLELINECOMMENTP <%s> found\n",line_count,yytext);
}

{mul_comment} {
	//printf ("\nMultiline comment found: %s\n",yytext);
	//fprintf(logout,"\nLine no %d: MULTILINECOMMENT <%s> found\n",line_count,yytext);
}

{number} {
	//printf ("number:%s\n",yytext);
	//printf ("line no: %d\n",line_count);
	SymbolInfo symbolInfo(yytext, "NUMBER");
	table.insert(symbolInfo, line_count);
	//fprintf(tokenout,"<NUMBER, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: NUMBER <%s> found\n",line_count,yytext);
	return NUMBER;
	
}

{addop} {
	insert("ADDOP", yytext);
	//printf ("line no: %d\n",line_count);
	//fprintf(tokenout,"\n<ADDOP, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: ADDOP <%s> found\n",line_count,yytext);
	return ADDOP;
}

{mulop} {
	insert("MULOP", yytext);
	//printf ("line no: %d\n",line_count);
	//fprintf(tokenout,"\n<MULOP, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: MULOP <%s> found\n",line_count,yytext);
	return MULOP;
}

{assignop} {
	insert("ASSIGNOP", yytext);
	//printf ("line no: %d\n",line_count);
	//fprintf(tokenout,"\n<ASSIGNOP, %s>\n",yytext);
	//fprintf(logout,"\nLine no %d: ASSIGNOP <%s> found\n",line_count,yytext);
	return ASSIGNOP;
}


{dotdot} {
	insert("DOTDOT", yytext);
	//printf ("line no: %d\n",line_count);
}	
{hash}  {
	insert("#",yytext);
}

{delim}+ {}

{err_dotdot} {
	//printf ("Illegal usage of decimal\n");
	//fprintf(logout,"\nLine no %d: Illegal usage of decimal <%s> found\n",line_count,yytext);
}

{err_id} {
	//printf ("Illegal id\n");
	//fprintf(logout,"\nLine no %d: Illegal usage of id <%s> found\n",line_count,yytext);
}



%%


