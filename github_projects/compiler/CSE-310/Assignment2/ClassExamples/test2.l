%{
#include <iostream>
#include <cstdio>
#include <cstring>

using namespace std;

int lineCount = 0;
%}

WS [ \t\v\f\r]+
NEWLINE [\n]
LETTER [a-zA-Z]
DIGIT [0-9]
UNDERSCORE [_]
NUM {DIGIT}+(\.{DIGIT}+)?(E[+-]?{DIGIT}+(\.{DIGIT}+)?)?
ID ({LETTER}|{UNDERSCORE})+({LETTER}|{UNDERSCORE}|{DIGIT})*

%%
{WS}        {
                //Do Nothing
            }
{NEWLINE}   {
                lineCount += 1;
                cout<<endl;
            }
"+"         {
                cout<<"PLUS"<<endl;
            }
"-"         {
                cout<<"MINUS"<<endl;
            }
"*"         {
                cout<<"MULTIPLICATION"<<endl;
            }
"/"         {
                cout<<"DIVISION"<<endl;
            }
"="         {
                cout<<"ASSIGNMENT"<<endl;
            }
{ID}        {
                cout<<"IDENTIFIER"<<endl;
            }
{NUM}       {
                cout<<"NUMBER"<<endl;
            }
.           {
                cout<<"ERROR"<<endl;
            }
%%

int main(int argc, char **argv)
{
    yylex();
    cout<<lineCount<<endl;
    return 0;
}
