%{
#include "myheader.h"
#include "y.tab.h"
extern YYSTYPE yylval;

using namespace std;

extern SymbolTable st;

int count=0;

%}

WS [ \t]+
NEWLINE [\n]
ASSIGNOP =
BRACKET1 [\[]
BRACKET2 [\]]
CURLEY1 [\{]
CURLEY2 [\}]
PAREN1 [\(]
PAREN2 [\)]
SEMICOLON ;
MULOP1 [*]
MULOP2 [/]
ADDOP1 [+]
ADDOP2 [-]
LESS <(>|=)?
EQUAL (==)|(!=)
OR \|\|
AND \&\&
GREAT >(=)?
RELOP ({LESS}|{EQUAL}|{GREAT}) 
DIGIT [0-9]
NUM {DIGIT}+(\.{DIGIT}+)?(E[+-]?{DIGIT}+(\.{DIGIT}+)?)?
LETTER [A-Za-z]
UNDERSCORE [_]
ID ({LETTER}|{UNDERSCORE})+({LETTER}|{DIGIT}|{UNDERSCORE})*

%%

{WS} {}
{NEWLINE} {
	count++;
}
{OR} {
	//st.insert(yytext,"OR");
	//yylval=(YYSTYPE)st.uplook(yytext);
	return OR;
	//cout<<"OR"<<endl;
}
{AND} {
	//st.insert(yytext,"AND");
	//yylval=(YYSTYPE)st.uplook(yytext);
	return AND;
	//cout<<"AND"<<endl;
}
if {
	return IF;
}
else {
	//fprintf(ofp1,"< %s >",yytext);
	//fprintf(ofp2,"\nLine < %d >: Keyword < %s > is found.\n",count+1,yytext);
	return ELSE;
}
{ID}  {
	st.insert(yytext,"ID");
	yylval=(YYSTYPE)st.uplook(yytext);
	return ID;
}
{NUM} {
	st.insert(yytext,"NUM");
	yylval=(YYSTYPE)st.uplook(yytext);
	return NUM;
}
{RELOP} {
	st.insert(yytext,"RELOP");
	yylval=(YYSTYPE)st.uplook(yytext);
	return RELOP;
}
{ADDOP1} {
	st.insert(yytext,"ADDOP");
	yylval=(YYSTYPE)st.uplook(yytext);
	return ADDOP1;
}
{ADDOP2} {
	st.insert(yytext,"ADDOP");
	yylval=(YYSTYPE)st.uplook(yytext);
	return ADDOP2;
}
{MULOP1} {
	st.insert(yytext,"MULOP");
	yylval=(YYSTYPE)st.uplook(yytext);
	return MULOP1;
}
{MULOP2} {
	st.insert(yytext,"MULOP");
	yylval=(YYSTYPE)st.uplook(yytext);
	return MULOP2;
}
{ASSIGNOP} {
	st.insert(yytext,"ASSIGNOP");
	yylval=(YYSTYPE)st.uplook(yytext);
	return ASSIGNOP;
}
{BRACKET1} {
	st.insert(yytext,"BRACKET");
	yylval=(YYSTYPE)st.uplook(yytext);
	return BRACKET1;
}
{CURLEY1} {
	st.insert(yytext,"CURLEY");
	yylval=(YYSTYPE)st.uplook(yytext);
	return CURLEY1;
}
{PAREN1} {
	st.insert(yytext,"PAREN");
	yylval=(YYSTYPE)st.uplook(yytext);
	return PAREN1;
}
{BRACKET2} {
	st.insert(yytext,"BRACKET");
	yylval=(YYSTYPE)st.uplook(yytext);
	return BRACKET2;
}
{PAREN2} {
	st.insert(yytext,"PAREN");
	yylval=(YYSTYPE)st.uplook(yytext);
	return PAREN2;
}
{CURLEY2} {
	st.insert(yytext,"CURLEY");
	yylval=(YYSTYPE)st.uplook(yytext);
	return CURLEY2;
}
{SEMICOLON} {
	st.insert(yytext,"SEMICOLON");
	yylval=(YYSTYPE)st.uplook(yytext);
	return SEMICOLON;
}

%%

/*int main(int argc,char *argv[])
{
	/*if(argc!=4)
	{
		printf("Usage: ./<programname> <input_file> <output_file1> <output_file2(assembly)>\n");
		printf("Try Again\n");
		exit(1);
	}

	if((ifp=fopen(argv[1],"r"))==NULL)
	{
		printf("Cannot Open Input File.\n");
		exit(1);
	}
	
	yyin=ifp;

	if((ofp1=fopen(argv[2],"w"))==NULL)
	{
		printf("Cannot Open Output File1.\n");
		exit(1);
	}

	if((ofp2=fopen(argv[3],"w"))==NULL)
	{
		printf("Cannot Open Output File2.\n");
		exit(1);
	}*/
	//freopen("in.txt","r",stdin);

	//yylex();
	//fprintf(ofp2,"\nInput file contains %d lines in total",count+1);
	//st.dump();
	//return 0;
//}


