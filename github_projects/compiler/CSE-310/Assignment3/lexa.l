%{
#include "SymbolTable.h"
#include "y.tab.h"

using namespace std;

//void yyerror(char*);
extern YYSTYPE yylval;

int lineCount = 1;
extern SymbolTable st;

extern FILE *fp, *parseLog;

extern bool inserted;
%}

NEWLINE \r?\n
WS [ \t\v\f\r]+

PROGRAM "program"
THEN "then"
REAL "real"
IF "if"
DO "do"
VAR "var"
NOT "not"
WHILE "while"
OF "of"
END "end"
FUNCTION "function"
ARRAY "array"
BEGINNING "begin"
PROCEDURE "procedure"
WRITE "write"
ELSE "else"
INTEGER "integer"

LETTER [a-zA-Z]
DIGIT [0-9]
UNDERSCORE _
NUM {DIGIT}+(\.{DIGIT}+)?(E[+-]?{DIGIT}+(\.{DIGIT}+)?)?

ILL_NUM1 (\.|\.{3,})({DIGIT}+)?(E[+-]?{DIGIT}+({DIGIT}+)?)?
ILL_NUM2 ({NUM})\.{NUM}(\.|{NUM})*
ILL_NUM {ILL_NUM1}|{ILL_NUM2}

ID ({LETTER}|{UNDERSCORE})+({LETTER}|{UNDERSCORE}|{DIGIT})*
ILL_ID {DIGIT}+{ID}

MSEND \\\\(\r)?{NEWLINE}
VALID_STRING \"([^"\n]|{MSEND}|"\\\"")*\"
ILL_STRING \"([^"]|"\\\"")*\"
INCOMPLETE_STRING \"([^"\n]|"\\\"")*\n

VALID_COMMENT \{([^{}]*)\}
ILL_COMMENT \{[^{}]*\{[^}]*\}
INCOMPLETE_COMMENT \{([^}\n]*)\n

RELOP ([<>]?=?)|<>
ADDOP \+|\-|or
MULOP \*|\/|div|mod|and
ASSIGNOP :=
BRACKET1 [\[]
BRACKET2 [\]]
PAREN1 [(]
PAREN2 [)]
DOTDOT \.\.
COMMA ,
SEMICOLON ;
COLON :
DOT \.
UNRECO [@~]+
%%


{WS}        {
                //Do Nothing
            }
{NEWLINE}   {
                lineCount += 1;
//                cout << "OLSDJFOLSJIDF\n";
//{KEYWORD}	{
//				fprintf ( parseLog, "Line %d: Keyword \"%s\" is found.\n", lineCount, yytext);
//				fprintf ( tokf, "< %s >", yytext);
//			}
            }
            
{PROGRAM}	{
			return PROGRAM;
		}

{THEN}	{
			return THEN;
		}

{REAL}	{
			return REAL;
		}

{IF}	{
			return IF;
		}

{DO}	{
			return DO;
		}

{VAR}	{
//			cout << "var found at line: " << lineCount << endl;
			return VAR;
		}

{NOT}	{
			return NOT;
		}

{WHILE}	{
			return WHILE;
		}

{OF}	{
			return OF;
		}

{END}	{
			return END;
		}

{FUNCTION}	{
			return FUNCTION;
		}

{ARRAY}	{
			return ARRAY;
		}

{BEGINNING}	{
			return BEGINNING;
		}

{PROCEDURE}	{
			return PROCEDURE;
		}

{WRITE}	{
			return WRITE;
		}

{ELSE}	{
			return ELSE;
		}

{INTEGER}	{
			return INTEGER;
		}

{NUM}	{
			if (st.insert("NUM",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"NUM\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return NUM;
		}

{RELOP}	{
			if (st.insert("RELOP",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"RELOP\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return RELOP;
		}

{ADDOP}	{
			if (st.insert("ADDOP",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"ADDOP\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return ADDOP;
		}

{MULOP}	{
			if (st.insert("MULOP",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"MULOP\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return MULOP;
		}

{ASSIGNOP}	{
			if (st.insert("ASSIGNOP",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"ASSIGNOP\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return ASSIGNOP;
		}

{BRACKET1}	{
			if (st.insert("BRACKET1",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"BRACKET1\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return BRACKET1;
		}

{BRACKET2}	{
			if (st.insert("BRACKET2",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"BRACKET2\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return BRACKET2;
		}

{PAREN1}	{
			if (st.insert("PAREN1",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"PAREN1\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return PAREN1;
		}

{PAREN2}	{
			if (st.insert("PAREN2",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"PAREN2\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return PAREN2;
		}

{DOTDOT}	{
			if (st.insert("DOTDOT",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"DOTDOT\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return DOTDOT;
		}

{COMMA}	{
			if (st.insert("COMMA",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"COMMA\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return COMMA;
		}

{SEMICOLON}	{
			if (st.insert("SEMICOLON",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"SEMICOLON\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return SEMICOLON;
		}

{COLON}	{
			if (st.insert("COLON",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"COLON\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return COLON;
		}

{DOT}	{
			if (st.insert("DOT",yytext) == true )	{
				fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"DOT\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
			}
			else	inserted = false;
			yylval=(YYSTYPE)st.uplook(yytext);
			return DOT;
		}
{ID}        {
//                cout<<"IDENTIFIER "<< yytext << endl;
				if (st.insert("ID",yytext) == true )	{
					fprintf ( parseLog, "At line: %d, lexeme: \"%s\" of type \"ID\" has been inserted in symbol table.\n", lineCount, yytext );
				inserted = true;
				}
				else	inserted = false;
				yylval=(YYSTYPE)st.uplook(yytext);
				return ID;
//				fprintf(parseLog, "Line %d: \"%s\" is found.\n", lineCount, yytext);
//                bool flag = if (st.insert("ID", yytext);
//                if (!flag)	fprintf(parseLog, "\"%s\" already exists.\n", yytext);
//                fprintf (parseLog, "\nSymbol Table: \n-------------\n");
//                st.dump(parseLog);
//                fprintf(parseLog, "\n");
//                fprintf(tokf, "< ID, %s >", yytext);
//				return ID;
            }
{VALID_STRING}	{
				int nl = 0, i = 0;
				char *str = new char [strlen(yytext)+1];
				yytext++;
				while(*yytext != '\0')	{
					if ( yytext[0] == '\\' )	{
						if ( yytext[1] == '\\' )	{
							yytext++;
							if(yytext[1] == '\r' && yytext[2] == '\n' )	{
								yytext+= 2;
								nl++;
							}
							else if (yytext[1] == '\n')	{
								yytext++;
								nl++;
							}
						}
						else if (yytext[1] == 'n')	{
							yytext++;
							*yytext = '\n';
						}
						else if (yytext[1] == 't')	{
							yytext++;
							*yytext = '\t';
						}
						else if (yytext[1] == 'v')	{
							yytext++;
							*yytext = '\v';
						}
						else if (yytext[1] == 'f')	{
							yytext++;
							*yytext = '\f';
						}
						else if (yytext[1] == 'r')	{
							yytext++;
							*yytext = '\r';
						}
						else if (yytext[1] == '\"')	{
							yytext++;
							*yytext = '\"';
						}
						else if (yytext[1] == '\'')	{
							yytext++;
							*yytext = '\'';
						}
					}
					str[i] = *yytext;
					yytext++;
					i++;
//					cout << "PPP: " << i << endl;
				}
				str[i-1] = '\0';	//hence last double quotation mark(") of yytext will be ignored
				fprintf ( parseLog, "Line %d: String \"%s\" is found.\n", lineCount, str);
				lineCount+= nl;
				return VALID_STRING;
			}
{ILL_STRING}	{
				fprintf(stderr, "Line %d: ERROR... Ill-formed string is found.\n", lineCount);
				while ( *yytext!='\0')	{
					if(yytext[0]=='\n')	lineCount++;
					yytext++;
				}
//				return ILL_STRING;
			}			
{INCOMPLETE_STRING}	{
				fprintf(stderr, "Line %d: ERROR... Incomplete string is found.\n", lineCount);
				int l = strlen(yytext);
				if(yytext[l-1]=='\n')	lineCount++;
//				return INCOMPLETE_STRING;
			}	
{VALID_COMMENT}	{
				fprintf ( parseLog, "Line %d: Comment %s is found.\n", lineCount, yytext);
				while(*yytext != '\0')	{
					if(*yytext == '\n')	lineCount++;
					yytext++;
				}
			}
{ILL_COMMENT}	{
				fprintf(stderr, "Line %d: ERROR... Ill-formed comment is found.\n", lineCount);
				int l = strlen(yytext);while(*yytext != '\0')	{
					if(*yytext == '\n')	lineCount++;
					yytext++;
				}
//				return ILL_COMMENT;
			}
{INCOMPLETE_COMMENT}	{
				fprintf(stderr, "Line %d: ERROR... Incomplete comment is found.\n", lineCount);
				int l = strlen(yytext);
				if(yytext[l-1]=='\n')	lineCount++;
//				return INCOMPLETE_COMMENT;
			}
{ILL_ID}			{
				fprintf(stderr, "Line %d: ERROR... Ill-formed identifier \"%s\" is found.\n", lineCount, yytext);
//				return ILL_ID;
			}
{ILL_NUM}			{
				fprintf(stderr, "Line %d: ERROR... Ill-formed number \"%s\" is found.\n", lineCount, yytext);
//				return ILL_NUM;
			}
{UNRECO}	{
				fprintf(stderr, "Line %d: ERROR... Unrecognised charcters %s are found.\n", lineCount, yytext);
//				return UNRECO;
			}
[^ \t\v\f\r\n]	{
				fprintf(stderr, "Line %d: ERROR... Unknown lexical error is found.\n", lineCount);
			}
%%

//int main(int argc, char **argv)
//{
//	fp = fopen(argv[1], "r");
//	tokf = fopen(argv[2], "w");
//	parseLog = fopen(argv[3], "w");
//	yyin = fp;
//    yylex();
//    fprintf(parseLog, "Total line: %d\n", --lineCount );
////    cout << lineCount<<endl;
//    fclose(fp);
//    fclose(tokf);
//    fclose(parseLog);
//    return 0;
//}

//int yywrap()
//{
//	return 1;
//}
