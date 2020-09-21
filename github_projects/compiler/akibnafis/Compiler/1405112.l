%option noyywrap
%option yylineno

%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "y.tab.h"
#include "1405112symboltable.h"

FILE *logout;
FILE *tokenout;

int line_count=1;
int error=0;
symbolInfo *s;
extern symbolTable *sTable;
extern scopeTable *table;

%}

WHITESPACE [ \t\f\r\v]+ 
LETTER [a-zA-Z]
DIGIT [0-9]
NEWLINE \n
FLOAT {DIGIT}*((\.)?{DIGIT}+(E[+-]?{DIGIT}+)?)
ID 	(_|{LETTER})+(_|{LETTER}|{DIGIT})*

%%

{NEWLINE} 	{line_count++;}
"if"		{return IF;}		
"else"		{return ELSE;}
"for"   	{return FOR;}
"while" 	{return WHILE;}
"do"    	{return DO;}
"break" 	{return BREAK;}	
"int"   	{return INT;}
"CHAR"  	{return CHAR;}
"float" 	{return FLOAT;}
"double"	{return DOUBLE;}
"void"  	{return VOID;}
"return"	{return RETURN;}
"switch"	{return SWITCH;}
"case"  	{return CASE;}
"default"	{return DEFAULT;}
"continue"	{return CONTINUE;}
{DIGIT}+ 	{
				yylval=new symbolInfo(yytext,"int",table->tableNo,atoi(yytext),0.0);
				return CONST_INT;
			}
{FLOAT} 	{
				yylval=new symbolInfo(yytext,"float",table->tableNo,0,atof(yytext),0);
				return CONST_FLOAT;
			}

"+"		|

"-"			{
				yylval=new symbolInfo(yytext,"ADDOP");
				return ADDOP;
			}
"*"|"/"|"%" {
				yylval=new symbolInfo(yytext,"MULOP");
				return MULOP;
			}
"++"	    {
				return INCOP;
			}
"--"	   	{
				return DECOP;
		    }
"<" | "<=" |
">" | ">=" |
"=="		{
				yylval=new symbolInfo(yytext,"RELOP");
				return RELOP;
			}
"="			{return ASSIGNOP;}
"!"			{return NOT;}
"("			{return LPAREN;}
")"			{return RPAREN;}
"{"			{return LCURL;}
"}"			{return RCURL;}
"["			{return LTHIRD;}
"]"			{return RTHIRD;}
","			{return COMMA;}
";"			{return SEMICOLON;}

\"((.)*(\\\n)?)*\"		{
				char strl[(yyleng-2)+1];
				int i=1;
				int k=line_count;
				while(i<yyleng-1)
				{
					if(yytext[i]=='\n')
						line_count++;
					strl[i-1]=yytext[i];
					i++;
				}
				strl[i-1]='\0';
				fprintf(logout,"Line no %d: TOKEN <STRING> Lexeme %s found\n",k,yytext);
				return STRING;
			}
{ID}        {
				symbolInfo *s= sTable->lookup_syminfo(yytext);
				if(s==NULL)
				{					
					s= new symbolInfo(yytext, "ID",table->tableNo);
				}
				yylval = (YYSTYPE)s;
							
				return ID;
			}

"&&"|"||"	{
				yylval=new symbolInfo(yytext,"LOGICOP");
				return LOGICOP;
			}
\/\/((.)*(\\\n)?)*	{
					int i=0;
					while(i<yyleng)
					{
						if(yytext[i]=='\n')
							line_count++;
						i++;
					}
			}

\/\*((.)*(\n)*)*\*\/		{
					fprintf(logout,"Line no %d: TOKEN <COMMENT> Lexeme %s found\n",line_count,yytext);
					int i=0;
					while(i<yyleng)
					{
						if(yytext[i]=='\n')
							line_count++;
						i++;
					}
				}

"println"	{return PRINTLN;}
"0k"[0-7]+  {
				fprintf(logout,"Line no %d: TOKEN <CONST_INT> Lexeme %s found\n",line_count,yytext);
				char decimal[10];
				int i=0;
				int d=0;
				int p=1;
				int sum=0;
				for(i=yyleng-1;i>1;i--)
				{
					d =yytext[i]-48;
					d=d*p;
					sum=sum+d;
					p=p*8;
				}
				for(i=9;sum!=0;i--)
				{
					decimal[i]=sum%10;
					sum=sum/10;
				}

			}
"0k"[^0-7]+	{
				fprintf(logout,"Line no %d: Not an Octal number Lexeme %s found\n",line_count,yytext);
			}



{DIGIT}*((\.{DIGIT}*){2,})	{
						error++;
						fprintf(logout,"Line no %d: %s too many decimal point found\n",line_count,yytext);

					}
{DIGIT}+(E){FLOAT}	{
						error++;
						fprintf(logout,"Line no %d: %s ill formed number\n",line_count,yytext);

					}
({DIGIT}+{LETTER}+)	{
						error++;
						fprintf(logout,"Line no %d: %s Invalid suffix on neumeric constant or Invalid prefix on identifier \n",line_count,yytext);
					}
"'"(.){2,}"'"		{
						error++;
						fprintf(logout,"Line no %d: %s multi character constant error\n",line_count,yytext);
					}
("'"[^'\n]*)|("'\\'")			{
						error++;
					}
"''"				{
						error++;
						fprintf(logout,"Line no %d: %s empty character\n",line_count,yytext);
					}
\"([^"\\\n]*("\\\n")?)*	{
						error++;
						fprintf(logout,"Line no %d: %s unfinished string\n",line_count,yytext);
						int i=0;
						while(i<yyleng)
						{
							if(yytext[i]=='\n')
								line_count++;
							i++;
						}
					}
\/\*([^\/\*])*		{
						error++;
						fprintf(logout,"Line no %d: %s unfinished comment\n",line_count,yytext);
						int i=0;
						while(i<yyleng)
						{
							if(yytext[i]=='\n')
								line_count++;
							i++;
						}
					}

{WHITESPACE} 	{
					printf("whitespace found\n");
				}




