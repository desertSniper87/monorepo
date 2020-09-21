%{
#include<bits/stdc++.h>
#include "SymbolTable.h"
#define YYSTYPE SymbolInfo*

using namespace std;

int yyparse(void);
int yylex(void);
extern FILE *yyin;
extern int lineCount;
int tempCount, labelCount;

FILE * parseLog;
//int tempcount=0;
//int levelcount=0;
//set<string> datalist;
//set<string>::iterator dlist;

//class mypair{
//public:
//	string s;
//	int v;
//};
//bool operator<(mypair mp1,mypair mp2)
//{
// 	return mp1.s<mp2.s;
//}

//set<mypair> myarraylist;
//set<mypair>::iterator alist;

FILE *fp;

void yyerror(char *s)
{
	fprintf(stdout,"%s\n",lineCount+1,s);
	return;
}

void getTemp(char *cp)
{
	sprintf(cp, "temp%d", tempCount++);
}

void getLabel(char *cp)
{
	sprintf(cp, "label%d", labelCount++);
}

SymbolTable st(200);
/*

*/
%}

%token PROGRAM THEN REAL IF DO VAR NOT WHILE OF END FUNCTION ARRAY BEGINNING PROCEDURE WRITE ELSE INTEGER LETTER DIGIT UNDERSCORE NUM ILL_NUM ID ILL_ID VALID_STRING ILL_STRING INCOMPLETE_STRING VALID_COMMENT ILL_COMMENT INCOMPLETE_COMMENT RELOP ADDOP MULOP ASSIGNOP BRACKET1 BRACKET2 PAREN1 PAREN2 DOTDOT COMMA SEMICOLON COLON DOT UNRECO AND OR

%left ADDOP
//%left ADDOP2
%left MULOP
//%left MULOP1
//%left MULOP2

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
//b:  CURLEY1 b OR b CURLEY2
//    {
//        printf("b -> { b or b }\n");
//        string b1True = $2->labelTrue;
//        string b1False = $2->labelFalse;
//        string b2True = $4->labelTrue;
//        string b2False = $4->labelFalse;
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        string b1 = $2->symbol;
//        string b2 = $4->symbol;
//        n->labelFalse=b2False;
//        n->labelTrue=b1True+":\n"+b2True;
//        n->code+=$2->code+b1False+":\n"+$4->code+"\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//    }
//    |
b:    BRACKET1 b AND b BRACKET2
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
//        n->labelTrue = levelTrue;
        char *levelFalse=new char[50];
        getLabel(levelFalse);
//        n->labelFalse = levelFalse;
//        n->code+=$2->code;
//        n->code+=$4->code;
//        n->code+="\nif "+ id1 + op + id2 +" goto " + levelTrue + "\n" + "goto " + levelFalse + "\n";
        $$ = n;
        //cout<<n->code<<endl;
    }
    ;
//program:    PROGRAM ID PAREN1 identifier_list PAREN2 SEMICOLON declarations subprogram_declarations compound_statement
//            {
//            	cout << "Program poggam popopo: " << endl;
//            }
//            ;
//identifier_list:    ID
//            {
//            }
//            |
//            identifier_list COMMA ID
//            {
//            }
//            ;
//            /*declarations → declarations VAR identifier_list COLON type
//SEMICOLON | ε
//*/
//declarations: 	       
//            {
//            }
//            |
//            declarations VAR identifier_list COLON type SEMICOLON
//            {
//            }
//            ;
//type:       standard_type
//            {
//            }
//            |
//            ARRAY BRACKET1 NUM DOTDOT NUM BRACKET2 OF standard_type
//            {
//            }
//            ;
//standard_type:  INTEGER
//            {
//            }
//            |
//            REAL
//            {
//            }
//            ;
//subprogram_declarations:	           
//            {
//            }
//            |
//            subprogram_declarations subprogram_declaration SEMICOLON
//            {
//            }
//            ;
//subprogram_declaration:	subprogram_head declarations compound_statement
//            {
//            }
//            ;
//subprogram_head:    FUNCTION ID arguments COLON standard_type SEMICOLON
//            {
//            }
//            |
//            PROCEDURE ID arguments SEMICOLON
//            {
//            }
//            ;
//arguments:	        
//            {
//            }
//            |
//            PAREN1 parameter_list PAREN2
//            {
//            }
//            ;
//parameter_list:	identifier_list COLON type
//            {
//            }
//            |
//            parameter_list SEMICOLON identifier_list COLON type
//            {
//            }
//            ;
//compound_statement:	BEGINNING optional_statements END
//            {
//            }
//            ;
//optional_statements:	      
//            {
//            }
//            |
//            statement_list
//            {
//            }
//            ;
//statement_list: statement
//            {
//            }
//            |
//            statement_list SEMICOLON statement
//            {
//            }
//            ;
//statement:  variable ASSIGNOP expression
//            {
//            }
//            |
//            procedure_statement
//            {
//            }
//            |
//            compound_statement
//            {
//            }
//            |
//            IF expression THEN statement %prec IFX
//            {
//            }
//            |
//            IF expression THEN statement ELSE statement
//            {
//            }
//            |
//            WHILE expression DO statement
//            {
//            }
//            |
//            WRITE PAREN1 ID PAREN2
//            {
//            }
//            ;
//variable:   ID
//            {
//            }
//            |
//            ID BRACKET1 expression BRACKET2
//            {
//            }
//            ;
//procedure_statement:    ID
//            {
//            }
//            |
//            ID PAREN1 expression_list PAREN2
//            {
//            }
//            ;
//expression_list:	expression
//            {
//            }
//            |
//            expression_list COMMA expression
//            {
//            }
//            ;
//expression: simple_expression
//            {
//            }
//            |
//            simple_expression RELOP simple_expression
//            {
//            }
//            ;
//simple_expression:  term
//            {
//            }
//            |
//            ADDOP term
//            {
//            }
//            |
//            simple_expression ADDOP term
//            {
//            }
//            ;
//term:       factor
//            {
//            }
//            |
//            term MULOP factor
//            {}
//            ;
//factor:     ID
//            {
//            }
//            |
//            ID PAREN1 expression_list PAREN2
//            {
//            }
//            |
//            NUM
//            {
//            }
//            |
//            PAREN1 expression PAREN2
//            {
//            }
//            |
//            NOT factor
//            {
//            }
//            ;
//sign:       ADDOP1
//            {
//            }
//            |
//            ADDOP2
//            {
//            }
//            ;
//p:  s
//    {
//        printf("p -> s\n");
//        cout<<$1->code<<"END."<<endl;
//    }
//    ;
//s:  IF PAREN1 b PAREN2 s %prec IFX
//    {
//        printf("s -> if (b) s\n");
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        SymbolInfo *b=$3;
//        n->code = b->code + b->labelTrue + ":\n" + $5->code + "\n" + b->labelFalse +":\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//    }
//    |
//    IF PAREN1 b PAREN2 s ELSE s
//    {
//        printf("s -> if ( b ) s else s\n");
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        SymbolInfo *b=$3;
//        char *tempLabel=new char[50];
//        getLabel(tempLabel);
//        n->code = b->code + b->labelTrue + ":\n" + $5->code + "\ngoto " + tempLabel + "\n" + b->labelFalse +":\n" + $7->code + "\n" + tempLabel +":\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//    }
//    |
//    ID ASSIGNOP NUM SEMICOLON
//    {
//        printf("s -> id = num;\n");
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        string id = $1->symbol;
//        string num = $3->symbol;
//        n->code += id + " = " + num +"\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//    }
//    ;
//b:  CURLEY1 b OR b CURLEY2
//    {
//        printf("b -> { b or b }\n");
//        string b1True = $2->labelTrue;
//        string b1False = $2->labelFalse;
//        string b2True = $4->labelTrue;
//        string b2False = $4->labelFalse;
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        string b1 = $2->symbol;
//        string b2 = $4->symbol;
//        n->labelFalse=b2False;
//        n->labelTrue=b1True+":\n"+b2True;
//        n->code+=$2->code+b1False+":\n"+$4->code+"\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//    }
//    |
//    BRACKET1 b AND b BRACKET2
//    {
//        printf(" b -> [ b and b ]\n");
//        string b1True = $2->labelTrue;
//        string b1False = $2->labelFalse;
//        string b2True = $4->labelTrue;
//        string b2False = $4->labelFalse;
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        string b1 = $2->symbol;
//        string b2 = $4->symbol;
//        n->labelTrue=b2True;
//        n->labelFalse=b1False+":\n"+b2False;
//        n->code+=$2->code+b1True+":\n"+$4->code+"\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//        /*
//        */
//    }
//    |
//    PAREN1 ID RELOP NUM PAREN2
//    {
//        printf("b -> ( id RELOP num )\n");
//        char *temp=new char[50];
//        getTemp(temp);
//        st.insert(temp,"temp");
//        SymbolInfo *n=st.uplook(temp);
//        string id1 = $2->symbol;
//        string op = $3->symbol;
//        string id2 = $4->symbol;
//        char *levelTrue=new char[50];
//        getLabel(levelTrue);
//        n->labelTrue = levelTrue;
//        char *levelFalse=new char[50];
//        getLabel(levelFalse);
//        n->labelFalse = levelFalse;
//        n->code+=$2->code;
//        n->code+=$4->code;
//        n->code+="\nif "+ id1 + op + id2 +" goto " + levelTrue + "\n" + "goto " + levelFalse + "\n";
//        $$ = n;
//        //cout<<n->code<<endl;
//    }
//    ;
%%
int main(int argc, char **argv)
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
	parseLog = fopen("ParserLog", "w");
	tempCount = 0;
	labelCount = 0;
	yyin = fp;
	cout << "wioeurioweuriouriouer\n";
    yyparse();
    fprintf(parseLog, "Total line: %d\n", --lineCount );
//    cout << lineCount<<endl;
    fclose(fp);
//    fclose(tokf);
    fclose(parseLog);
    return 0;
}

