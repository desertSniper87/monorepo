%{
#include <iostream>
#include <cstdio>
#include <cstring>

using namespace std;

int chars = 0;
int words = 0;
int lines = 0;
%}

%%

\n          {
                lines += 1;
                chars += 1;
            }
[^ \t\n\r\f\v]+ {
                words += 1;
                chars += strlen(yytext);
}
.           {
                chars += 1;
            }

%%

int main(int argc, char **argv)
{
    FILE *ifp = fopen(argv[1], "r");
    yyin = ifp;
    yylex();
    cout<<"Total line count: "<<lines<<endl;
    cout<<"Total word count: "<<words<<endl;
    cout<<"Total char count: "<<chars<<endl;
    fclose(ifp);
    return 0;
}
