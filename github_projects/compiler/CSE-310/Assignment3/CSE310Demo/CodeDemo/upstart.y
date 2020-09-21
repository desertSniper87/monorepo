%{
#include "myheader.h"
#define YYSTYPE SymbolInfo*

using namespace std;
int yyparse(void);
int yylex(void);
extern FILE *yyin;
extern int count;
int tempcount=0;
int levelcount=0;
set<string> datalist;
set<string>::iterator dlist;

class mypair{
public:
	string s;
	int v;
};
bool operator<(mypair mp1,mypair mp2)
{
 	return mp1.s<mp2.s;
}

set<mypair> myarraylist;
set<mypair>::iterator alist;

FILE *fp;

void yyerror(char *s)
{
	fprintf(stdout,"At Line %d, ERROR-> %s\n",count+1,s);
	return;
}

void getTemp(char *cp)
{
	strcpy(cp,"temp");
	char *t=new char[40];
	sprintf(t,"%d",tempcount);
	strcat(cp,t);
	tempcount++;
	delete [] t;
}

void getLabel(char *cp)
{
	strcpy(cp,"level");
	char *t=new char[40];
	sprintf(t,"%d",levelcount);
	strcat(cp,t);
	levelcount++;
	delete [] t;
}

SymbolTable st(200);
/*

*/
%}

%token ID SEMICOLON NUM ASSIGNOP IF ELSE RELOP ADDOP1 ADDOP2 MULOP1 MULOP2 PAREN1 PAREN2 BRACKET1 BRACKET2 OR AND CURLEY1 CURLEY2

%left ADDOP1
%left ADDOP2
%left MULOP
%left MULOP1
%left MULOP2

%nonassoc IFX
%nonassoc ELSE


%%
p:  s
    {
        printf("p -> s\n");
        cout<<$1->code<<"END."<<endl;
    }
    ;
s:  IF PAREN1 b PAREN2 s %prec IFX
    {
        printf("s -> if (b) s\n");
        char *temp=new char[50];
        getTemp(temp);
        st.insert(temp,"temp");
        SymbolInfo *n=st.uplook(temp);
        SymbolInfo *b=$3;
        n->code = b->code + b->labelTrue + ":\n" + $5->code + "\n" + b->labelFalse +":\n";
        $$ = n;
        //cout<<n->code<<endl;
    }
    |
    IF PAREN1 b PAREN2 s ELSE s
    {
        printf("s -> if ( b ) s else s\n");
        char *temp=new char[50];
        getTemp(temp);
        st.insert(temp,"temp");
        SymbolInfo *n=st.uplook(temp);
        SymbolInfo *b=$3;
        char *tempLabel=new char[50];
        getLabel(tempLabel);
        n->code = b->code + b->labelTrue + ":\n" + $5->code + "\ngoto " + tempLabel + "\n" + b->labelFalse +":\n" + $7->code + "\n" + tempLabel +":\n";
        $$ = n;
        //cout<<n->code<<endl;
    }
    |
    ID ASSIGNOP NUM SEMICOLON
    {
        printf("s -> id = num;\n");
        char *temp=new char[50];
        getTemp(temp);
        st.insert(temp,"temp");
        SymbolInfo *n=st.uplook(temp);
        string id = $1->symbol;
        string num = $3->symbol;
        n->code += id + " = " + num +"\n";
        $$ = n;
        //cout<<n->code<<endl;
    }
    ;
b:  CURLEY1 b OR b CURLEY2
    {
        printf("b -> { b or b }\n");
        string b1True = $2->labelTrue;
        string b1False = $2->labelFalse;
        string b2True = $4->labelTrue;
        string b2False = $4->labelFalse;
        char *temp=new char[50];
        getTemp(temp);
        st.insert(temp,"temp");
        SymbolInfo *n=st.uplook(temp);
        string b1 = $2->symbol;
        string b2 = $4->symbol;
        n->labelFalse=b2False;
        n->labelTrue=b1True+":\n"+b2True;
        n->code+=$2->code+b1False+":\n"+$4->code+"\n";
        $$ = n;
        //cout<<n->code<<endl;
    }
    |
    BRACKET1 b AND b BRACKET2
    {
        printf(" b -> [ b and b ]\n");
        string b1True = $2->labelTrue;
        string b1False = $2->labelFalse;
        string b2True = $4->labelTrue;
        string b2False = $4->labelFalse;
        char *temp=new char[50];
        getTemp(temp);
        st.insert(temp,"temp");
        SymbolInfo *n=st.uplook(temp);
        string b1 = $2->symbol;
        string b2 = $4->symbol;
        n->labelTrue=b2True;
        n->labelFalse=b1False+":\n"+b2False;
        n->code+=$2->code+b1True+":\n"+$4->code+"\n";
        $$ = n;
        //cout<<n->code<<endl;
        /*
        */
    }
    |
    PAREN1 ID RELOP NUM PAREN2
    {
        printf("b -> ( id RELOP num )\n");
        char *temp=new char[50];
        getTemp(temp);
        st.insert(temp,"temp");
        SymbolInfo *n=st.uplook(temp);
        string id1 = $2->symbol;
        string op = $3->symbol;
        string id2 = $4->symbol;
        char *levelTrue=new char[50];
        getLabel(levelTrue);
        n->labelTrue = levelTrue;
        char *levelFalse=new char[50];
        getLabel(levelFalse);
        n->labelFalse = levelFalse;
        n->code+=$2->code;
        n->code+=$4->code;
        n->code+="\nif "+ id1 + op + id2 +" goto " + levelTrue + "\n" + "goto " + levelFalse + "\n";
        $$ = n;
        //cout<<n->code<<endl;
    }
    ;
%%
int main(int argc,char *argv[])
{
	if(argc!=2)
	{
		printf("Usage: ./<programname> <input_file>\n");
		printf("Try Again\n");
		exit(1);
	}

	if((fp=fopen(argv[1],"r"))==NULL)
	{
		printf("Cannot Open Input File.\n");
		exit(1);
	}
	
	yyin=fp;
	yyparse();
	return 0;
}

