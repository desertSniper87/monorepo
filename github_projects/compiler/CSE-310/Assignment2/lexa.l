%{
#include "SymbolTable.h"

using namespace std;

int lineCount = 1;
SymbolTable st;

FILE *fin, *logp, *tokf;
%}

NEWLINE \r?\n
WS [ \t\v\f\r]+
KEYWORD "program"|"then"|"real"|"if"|"do"|"var"|"not"|"while"|"of"|"end"|"function"|"array"|"begin"|"procedure"|"write"|"else"|"integer"
LETTER [a-zA-Z]
DIGIT [0-9]
UNDERSCORE _
NUM {DIGIT}+(\.{DIGIT}+)?(E[+-]?{DIGIT}+(\.{DIGIT}+)?)?

ILL_NUM1 (\.|\.{3,})({DIGIT}+)?(E[+-]?{DIGIT}+({DIGIT}+)?)?
ILL_NUM2 ({NUM})\.{NUM}(\.|{NUM})*
ILL_NUM {ILL_NUM1}|{ILL_NUM2}

ID ({LETTER}|{UNDERSCORE})+({LETTER}|{UNDERSCORE}|{DIGIT})*
ILL_ID {DIGIT}+{ID}

MSEND \\\\{NEWLINE}
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
BRACKET [\[\]]
PAREN [()]
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
            }
            
{KEYWORD}	{
				fprintf ( logp, "Line %d: Keyword \"%s\" is found.\n", lineCount, yytext);
				fprintf ( tokf, "< %s >", yytext);
			}
{ID}        {
//                cout<<"IDENTIFIER"<<endl;
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("ID", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< ID, %s >", yytext);
            }
{NUM}       {
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("NUM", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< NUM, %s >", yytext);
            }
{RELOP}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("RELOP", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< RELOP, %s >", yytext);
            }
{ADDOP}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("ADDOP", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< ADDOP, %s >", yytext);
            }
{MULOP}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("MULOP", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< MULOP, %s >", yytext);
            }
{ASSIGNOP}	{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("ASSIGNOP", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< ASSIGNOP, %s >", yytext);
            }
{BRACKET}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("BRACKET", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< BRACKET, %s >", yytext);
            }
{PAREN}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("PAREN", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< PAREN, %s >", yytext);
            }
{DOTDOT}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("DOTDOT", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< DOTDOT, %s >", yytext);
            }
{COMMA}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("COMMA", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< COMMA, %s >", yytext);
            }
{SEMICOLON}	{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("SEMICOLON", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< SEMICOLON, %s >", yytext);
            }
{COLON}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("COLON", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< COLON, %s >", yytext);
            }
{DOT}		{
				fprintf(logp, "Line %d: \"%s\" is found.\n", lineCount, yytext);
                bool flag = st.insert("DOT", yytext);
                if (!flag)	fprintf(logp, "\"%s\" already exists.\n", yytext);
                fprintf (logp, "\nSymbol Table: \n-------------\n");
                st.dump(logp);
                fprintf(logp, "\n");
                fprintf(tokf, "< DOT, %s >", yytext);
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
				fprintf ( logp, "Line %d: String \"%s\" is found.\n", lineCount, str);
				lineCount+= nl;
			}
{ILL_STRING}	{
				fprintf(logp, "Line %d: ERROR... Ill-formed string is found.\n", lineCount);
				while ( *yytext!='\0')	{
					if(yytext[0]=='\n')	lineCount++;
					yytext++;
				}
			}			
{INCOMPLETE_STRING}	{
				fprintf(logp, "Line %d: ERROR... Incomplete string is found.\n", lineCount);
				int l = strlen(yytext);
				if(yytext[l-1]=='\n')	lineCount++;
			}	
{VALID_COMMENT}	{
				fprintf ( logp, "Line %d: Comment %s is found.\n", lineCount, yytext);
				while(*yytext != '\0')	{
					if(*yytext == '\n')	lineCount++;
					yytext++;
				}
			}
{ILL_COMMENT}	{
				fprintf(logp, "Line %d: ERROR... Ill-formed comment is found.\n", lineCount);
				int l = strlen(yytext);while(*yytext != '\0')	{
					if(*yytext == '\n')	lineCount++;
					yytext++;
				}
			}
{INCOMPLETE_COMMENT}	{
				fprintf(logp, "Line %d: ERROR... Incomplete comment is found.\n", lineCount);
				int l = strlen(yytext);
				if(yytext[l-1]=='\n')	lineCount++;

			}
{ILL_ID}			{
				fprintf(logp, "Line %d: ERROR... Ill-formed identifier \"%s\" is found.\n", lineCount, yytext);
			}
{ILL_NUM}			{
				fprintf(logp, "Line %d: ERROR... Ill-formed number \"%s\" is found.\n", lineCount, yytext);
			}
{UNRECO}	{
				fprintf(logp, "Line %d: ERROR... Unrecognised charcters %s are found.\n", lineCount, yytext);
			}
[^ \t\v\f\r\n]	{
				fprintf(logp, "Line %d: ERROR... Unknown lexical error is found.\n", lineCount);
			}
%%

int main(int argc, char **argv)
{
	fin = fopen(argv[1], "r");
	tokf = fopen(argv[2], "w");
	logp = fopen(argv[3], "w");
	yyin = fin;
    yylex();
    fprintf(logp, "Total line: %d\n", --lineCount );
//    cout << lineCount<<endl;
    fclose(fin);
    fclose(tokf);
    fclose(logp);
    return 0;
}
