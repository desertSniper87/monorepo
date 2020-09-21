%{
#include "SymbolTable.h"
#define YYSTYPE SymbolInfo*
#include <bits/stdc++.h>

using namespace std;
	
SymbolTable st;
stringstream ss;
int yyparse(void);
int yylex(void);
extern FILE *yyin;
extern int lineCount;
int tempCount = 0, labelCount = 0;
//set<string> datalist;
//set<string>::iterator dlist;
//class mypair{
//public:
//	string s;
//	int v;
//};

FILE *fp, *parseLog;

void yyerror(char *s)
{
	fprintf(stderr,"At Line %d, ERROR-> %s\n",lineCount,s);
	return;
}

bool declareZone = true;
bool inserted = false;
string varDec = "", codes = "";

ofstream icfile;

string getTemp(string type = "w")	//put type = "b" for byte
{
	stringstream ss;
	ss << "temp" << tempCount++;
	varDec+= ss.str()+" d"+type[0]+" 0\n";
	return ss.str();
}

string getLabel()	//put type = "b" for byte
{
	stringstream ss;
	ss << "label" << labelCount++;
	return ss.str();
}

%}

%token PROGRAM THEN REAL IF DO VAR NOT WHILE OF END FUNCTION ARRAY BEGINNING PROCEDURE WRITE ELSE INTEGER LETTER DIGIT UNDERSCORE NUM ILL_NUM ID ILL_ID VALID_STRING ILL_STRING INCOMPLETE_STRING VALID_COMMENT ILL_COMMENT INCOMPLETE_COMMENT RELOP ADDOP MULOP ASSIGNOP BRACKET1 BRACKET2 PAREN1 PAREN2 DOTDOT COMMA SEMICOLON COLON DOT UNRECO AND OR

%left ADDOP

%left MULOP


%nonassoc IFX
%nonassoc ELSE


%%


program:    PROGRAM ID PAREN1 identifier_list PAREN2 SEMICOLON declarations subprogram_declarations compound_statement
            {
            	
            	fprintf ( parseLog, "program → PROGRAM ID '(' identifier_list ')' SEMICOLON declarations subprogram_declarations compound_statement\n");
            	codes = ".MODEL SMALL\n\n.STACK 100H\n";
            	getTemp();
            	getTemp();
            	icfile << codes<< "\n";
            	icfile << varDec;
            	icfile << "\n.CODE\n\nMAIN PROC\nMOV AX,@DATA\nMOV DS,AX\n";
            	
            	icfile << $9->code << "\n";
            	icfile << "\t\nmain endp\n";
            	icfile << "\n\n;PRINT FUNC\n";
            	icfile << "print proc  \n\tmov bp, sp\n\tmov ax, [bp+2]\n\tcmp ax, 0\n\tje return_print\n\t\n\tmov dx, 0\n\tmov bx, 10\n\tdiv bx\n\t\n\t;recalling\n\tpush dx\n\tpush ax\n\tcall print\n\t\n\t;printing\n\tpop dx\n\tadd dl, '0'\n\tmov ah, 2h\n\tint 21h\n\t\n\treturn_print:\n\t\tret 2\t\nprint endp\n";
//            	icfile << "\nEND MAIN\n";
            }
//            |
//            statement
//            {
//            	cout << "statement program\n";
//            }
            ;
identifier_list:    ID
            {
            	fprintf ( parseLog, "identifier_list → ID\n");
//            	SymbolInfo *id("temp", "ASM", 0);
           		if ( declareZone )
           			varDec+= $1->symbol+" dw 0\n";
            }
            |
            identifier_list COMMA ID
            {
	            fprintf ( parseLog, "identifier_list → identifier_list COMMA ID\n");
	            if ( declareZone)	{
	            	varDec+= $3->symbol+" dw 0\n";
	            }
//	            cout << $1->symbol << " " << $3->symbol << "\n";
            }
            ;
            /*declarations → declarations VAR identifier_list COLON type
SEMICOLON | ε
*/
declarations: 	       
            {
            	fprintf ( parseLog, "declaration → ε\n");
            	declareZone = true;
            	varDec.clear();
            	varDec = ".data\n";
            }
            |
            declarations VAR identifier_list COLON type SEMICOLON
            {
            	fprintf ( parseLog, "declarations → declarations VAR identifier_list COLON type SEMICOLON\n");
//            	string pp = $1->code + ", "+$3->code;
//            	cout << $2->symbol << endl;
            }
            ;
type:       standard_type
            {
            	fprintf ( parseLog, "type → standard_type\n");
           		$$ = $1;
            }
            |
            ARRAY BRACKET1 NUM DOTDOT NUM BRACKET2 OF standard_type
            {
            	fprintf ( parseLog, "type → ARRAY '[' NUM DOTDOT NUM ']' OF standard_type\n");
            }
            ;
standard_type:  INTEGER
            {
            	fprintf ( parseLog, "standard_type → INTEGER\n");
           		$$ = $1;
            }
            |
            REAL
            {
            	fprintf ( parseLog, "standard_type → REAL\n");
           		$$ = $1;
            }
            ;
subprogram_declarations:	           
            {
            	fprintf ( parseLog, "subprogram_declarations → ε");
            	declareZone = false;
//            	cout << varDec << endl;
            }
            |
            subprogram_declarations subprogram_declaration SEMICOLON
            {
            	fprintf ( parseLog, "subprogram_declarations → subprogram_declarations subprogram_declaration SEMICOLON\n");
            }
            ;
subprogram_declaration:	subprogram_head declarations compound_statement
            {
            	fprintf ( parseLog, "subprogram_declaration → subprogram_head declarations compound_statement\n");
            }
            ;
subprogram_head:    FUNCTION ID arguments COLON standard_type SEMICOLON
            {
            	fprintf ( parseLog, "subprogram_head → FUNCTION ID arguments COLON standard_type SEMICOLON\n");
            }
            |
            PROCEDURE ID arguments SEMICOLON
            {
            	fprintf ( parseLog, "subprogram_head → PROCEDURE ID arguments SEMICOLON\n");
            }
            ;
arguments:	        
            {
            	fprintf ( parseLog, "arguments → ε\n");
            }
            |
            PAREN1 parameter_list PAREN2
            {
            	fprintf ( parseLog, "arguments → '(' parameter_list ')'\n");
            }
            ;
parameter_list:	identifier_list COLON type
            {
            	fprintf ( parseLog, "parameter_list → identifier_list COLON type\n");
            }
            |
            parameter_list SEMICOLON identifier_list COLON type
            {
            	fprintf ( parseLog, "parameter_list → parameter_list SEMICOLON identifier_list COLON type\n");
            }
            ;
compound_statement:	BEGINNING optional_statements END
            {
            	fprintf ( parseLog, "compound_statement → BEGIN optional_statements END\n");
            	$$ = $2;
//            	$$->code = "\n"+$2->code;
//            	fprintf ( parseLog, "-----qwpoiiouioert\n%s\n\n------nlkmnvklsjfh\n", $$->code.c_str());
            }
            ;
optional_statements:	      
            {
            	fprintf ( parseLog, "optional_statements → ε\n");
            }
            |
            statement_list
            {
            	fprintf ( parseLog, "optional_statements → statement_list\n" );
           		$$ = $1;	//dekhte hobe
//           		cout << "statement_list:==>\n"<< $1->code << "\n ENDED"<<endl;
            }
            ;
statement_list: statement
            {
            	fprintf ( parseLog, "statement_list → statement\n");
            	$$ = $1;
//           		$$->code = $1->code;
            }
            |
            statement_list SEMICOLON statement
            {
            	fprintf ( parseLog, "statement_list → statement_list SEMICOLON statement\n");
            	$$->code+= "\n"+$3->code;
//            	$$->code+= $1->code+"\n"+$3->code+"\n";
//            	$$->code = $3->code;
            }
            ;
statement:  variable ASSIGNOP expression
            {
            	fprintf ( parseLog, "statement → variable ASSIGNOP expression\n");
            	if ($3->type=="NUM")	$1->value = $3->value;
            	fprintf ( parseLog, "Assign on line: %d\n", lineCount);
            	
            	SymbolInfo *siTemp = new SymbolInfo("Instr", "ASM");
            	siTemp->code = $3->code;
            	siTemp->code+= "MOV ax, "+$3->symbol+"\nmov " + $1->symbol + ", ax\n";
//            	ss.clear();
//            	ss << "MOV ax, " << $3->symbol << "\nmov " << $1->symbol << ", ax" << "\n";
//            	siTemp->code = ss.str();
            	$$ = siTemp;
            	st.dump(parseLog);
            }
            |
            procedure_statement
            {
            	fprintf ( parseLog, "statement → procedure_statement\n");
           		$$ = $1;
            }
            |
            compound_statement
            {
            	fprintf ( parseLog, "statement → compound_statement\n");
           		$$ = $1;
            }
            |
            IF expression THEN statement %prec IFX
            {
	            fprintf ( parseLog, "statement → IF expression THEN statement\n");
	            string tempLabel = getLabel();
            	SymbolInfo *tmpsi = new SymbolInfo(tempLabel, "ASM_Label");
            	string &tempCode = tmpsi->code;
//            	tempCode = "\n;IF exp THEN stat\n";
            	tempCode+= $2->code;//+"\n"+$3->code;
            	tempCode+= "jmp "+tempLabel+"\n";
            	tempCode+= $2->labelTrue+":\n";
            	tempCode+= $4->code;
            	tempCode+= tempLabel+":\n";
            	$$ = tmpsi;
            }
            |
            IF expression THEN statement ELSE statement
            {
	            fprintf ( parseLog, "statement → IF expression THEN statement ELSE statement\n");
	            string tempLabel = getLabel();
            	SymbolInfo *tmpsi = new SymbolInfo(tempLabel, "ASM_Label");
            	string &tempCode = tmpsi->code;
//            	tempCode = "\n;IF exp THEN stat\n";
            	tempCode+= $2->code;//+"\n"+$3->code;
            	tempCode+= $6->code;
            	tempCode+= "jmp "+tempLabel+"\n";
            	tempCode+= $2->labelTrue+":\n";
            	tempCode+= $4->code;
//            	cout << "oiwuoiuwert\n" << $4->code << "\noiuweoruiwe\n";
            	tempCode+= tempLabel+":\n";
            	$$ = tmpsi;
            }
            |
            WHILE expression DO statement
            {
            	fprintf ( parseLog, "statement → WHILE expression DO statement\n");
            	string loopLabel = getLabel();
            	string endLoop = getLabel();
            	SymbolInfo *tmpsi = new SymbolInfo(loopLabel, "ASM_Label");
            	string &tempCode = tmpsi->code;
            	
            	tempCode = loopLabel+": \n";
            	tempCode+= $2->code+"\n";
            	tempCode+= "jmp "+endLoop+":\n";
            	tempCode+= $2->labelTrue+":\n";
            	tempCode+= $4->code+"\n";
            	tempCode+= "jmp "+loopLabel+"\n";
            	tempCode+= endLoop+":\n";
            	$$ = tmpsi;
            }
            |
            WRITE PAREN1 ID PAREN2
            {
            	fprintf ( parseLog, "statement → write '(' ID ')'\n");
            	string tempLabel = getLabel();
            	SymbolInfo *tmpsi = new SymbolInfo(tempLabel, "ASM_Label");
            	string &tempCode = tmpsi->code;
            	
            	tempCode = "\n;PRINTING\nPUSH "+ $3->symbol +"\n";
            	tempCode+= "CALL PRINT\n";
            	$$ = tmpsi;
            }
            ;
variable:   ID
            {
	            fprintf ( parseLog, "variable → ID\n");
           		$$ = $1;
            }
            |
            ID BRACKET1 expression BRACKET2
            {
            	fprintf ( parseLog, "variable → ID '[' expression ']'\n");
            }
            ;
procedure_statement:    ID
            {
           		fprintf ( parseLog, "procedure_statement → ID\n" );
           		$$ = $1;
            }
            |
            ID PAREN1 expression_list PAREN2
            {
           		fprintf ( parseLog, "procedure_statement → ID '(' expression_list ')'\n" );            	
            }
            ;
expression_list:	expression
            {
            	fprintf ( parseLog, "expression_list → expression\n");
            	$$ = $1;
            }
            |
            expression_list COMMA expression
            {
            	fprintf ( parseLog, "expression_list → expression_list COMMA expression\n");
            	$$->code+= $1->code;
            }
            ;
expression: simple_expression
            {
            	fprintf ( parseLog, "expression → simple_expression\n");
            	$$ = $1;
//            	cout << "POIQRET:\n" << $1->code << "\nopioriweutert\n";
            }
            |
            simple_expression RELOP simple_expression
            {
            	fprintf ( parseLog, "expression → simple_expression RELOP simple_expression\n");
            	string tempVal = getLabel();
            	SymbolInfo *tmpsi = new SymbolInfo(tempVal, "ASM");
            	tmpsi->code = $1->code+"\n"+$3->code;
            	string &tempCode = tmpsi->code;
            	
            	tmpsi->labelTrue = getLabel();
            	tmpsi->labelFalse = getLabel();
            	
            	if ( $2->symbol == "<" )	{
            		tempCode+= "\n;Less than relation operator\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
	            	tempCode+= "CMP ax, bx\n";
	            	tempCode+= "jl "+tmpsi->labelTrue+"\n";
	            	tempCode+= tmpsi->labelFalse+":\n";
            	}
            	else if ( $2->symbol == ">" )	{
            		tempCode+= "\n;Less than relation operator\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
	            	tempCode+= "CMP ax, bx\n";
	            	tempCode+= "jg "+tmpsi->labelTrue+"\n";
	            	tempCode+= tmpsi->labelFalse+":\n";
            	}
            	else if ( $2->symbol == ">=" )	{
            		tempCode+= "\n;Less than relation operator\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
	            	tempCode+= "CMP ax, bx\n";
	            	tempCode+= "jge "+tmpsi->labelTrue+"\n";
	            	tempCode+= tmpsi->labelFalse+":\n";
            	}
            	else if ( $2->symbol == "<=" )	{
            		tempCode+= "\n;Less than relation operator\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
	            	tempCode+= "CMP ax, bx\n";
	            	tempCode+= "jle "+tmpsi->labelTrue+"\n";
	            	tempCode+= tmpsi->labelFalse+":\n";
            	}
            	else	{
            		tempCode+= "\n;Less than relation operator\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
	            	tempCode+= "CMP ax, bx\n";
	            	tempCode+= "jne "+tmpsi->labelTrue+"\n";
	            	tempCode+= tmpsi->labelFalse+":\n";
            	}
//            	tempCode+= "MOV "+ tempVal+", ax\n\n";
            	$$ = tmpsi;
            }
            ;
simple_expression:  term
            {
            	fprintf ( parseLog, "simple_expression → term\n");
            	$$ = $1;
            }
            |
            ADDOP term
            {
            	fprintf ( parseLog, "simple_expression → sign term\n");
            	if ( $1->symbol != "+" && $1->symbol != "-" )	yyerror(" only '+' or '-' sign can be used here.");
            	string tempVal = getTemp();
            	SymbolInfo *tmpsi = new SymbolInfo(tempVal, "ASM");
            	tmpsi->code = "\n"+ $2->code;
            	string &tempCode = tmpsi->code;
            	
            	if ( $1->symbol == "-")	{
            		tempCode+= "\nmov ax, "+$2->symbol + "\nneg ax\n";
            		tempCode+= "MOV "+$2->symbol+", ax\n\n";
            		$2->value*= -1;
            	}
//            	cout << "mmoiwer\n" << $2->code << "\noiuwroi\n";
            	$$ = tmpsi;
            }
            |
            simple_expression ADDOP term
            {
            	fprintf ( parseLog, "simple_expression → simple_expression ADDOP term\n");
            	string tempVal = getTemp();
            	SymbolInfo *tmpsi = new SymbolInfo(tempVal, "ASM");
            	tmpsi->code = $1->code+"\n"+$3->code;
            	string &tempCode = tmpsi->code;
            	
            	
            	if ( $2->symbol == "+" )	{
            		tempCode+= "\n;Adding\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = $1->value+$3->value;
            		tempCode+= "ADD ax, bx\n";
            	}
            	else if ( $2->symbol == "-")	{
            		tempCode+= "\n;SUBTRACTING\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = $1->value-$3->value;
            		tempCode+= "SUB ax, bx\n";
            	}
            	else	{
            		tempCode+= "\n;BITWISE OR\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = ((int)$1->value)|((int)$3->value);
            		tempCode+= "OR ax, bx\n";
            	}
            	tempCode+= "MOV "+ tempVal+", ax\n\n";
            	$$ = tmpsi;
//            	cout << "----ADDDDDDDDING\n" << $$->code << "\n___ADDING ENDED" << endl;
//            	$$->print();
            }
            ;
term:       factor
            {
            	fprintf ( parseLog, "term → factor\n");
            	$$ = $1;
            }
            |
            term MULOP factor
            {
            	fprintf ( parseLog, "term → term MULOP factor\n");
            	string tempVal = getTemp();
            	SymbolInfo *tmpsi = new SymbolInfo(tempVal, "ASM");
            	tmpsi->code = $1->code+"\n"+$3->code;
            	string &tempCode = tmpsi->code;
            	
            	
            	if ( $2->symbol == "*" )	{
//            		if ( $1->symbol == "(")	{
//            			cout << "oiwueroiuweriot::==>\t\t";
//            			 $1->print();
//            			 cout << endl;
//            		}
            		tempCode+= "\n;MULTIPLYING\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = $1->value * $3->value;
            		tempCode+= "MUL bx\n";
            		tempCode+= "MOV "+tempVal+", ax\n";
            	}
            	else if ( $2->symbol == "/")	{
            		tempCode+= "\n;DIVIDING\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = $1->value / $3->value;
            		tempCode+= "MOV dx, 0\nDIV bx\n";
            		tempCode+= "MOV "+tempVal+", ax\n";
            	}
            	else if ( $2->symbol == "mod")	{
            		tempCode+= "\n;REMINDER\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = (int)$1->value % (int)$3->value;
            		tempCode+= "MOV dx, 0\nDIV bx\n";
            		tempCode+= "MOV "+tempVal+", dx\n";
            	}
            	else	{
            		tempCode+= "\n;BITWISE AND\n";
		        	tempCode+= "MOV ax, "+$1->symbol+"\n";
	            	tempCode+= "MOV bx, "+$3->symbol+"\n";
            		tmpsi->value = ((int)$1->value)&((int)$3->value);
            		tempCode+= "AND ax, bx\n";
		        	tempCode+= "MOV "+ tempVal+", ax\n";
            	}
            	tempCode+= "\n";
            	$$ = tmpsi;
            }
            ;
factor:     ID
            {
            	fprintf ( parseLog, "factor → ID\n");
            	$$ = $1;
            }
            |
            ID PAREN1 expression_list PAREN2
            {
            	fprintf ( parseLog, "factor → ID '(' expression_list ')'\n");
            }
            |
            NUM
            {
            	fprintf ( parseLog, "factor → NUM\n");
            	$$ = $1;
            }
            |
            PAREN1 expression PAREN2
            {
            	fprintf ( parseLog, "factor → '(' expression ')'\n");
            	$$ = $2;
            }
            |
            NOT factor
            {
            	fprintf ( parseLog, "factor → NOT factor\n");
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
	parseLog = fopen("parseLog", "w");
	if((fp=fopen(argv[1],"r"))==NULL)
	{
		printf("Cannot Open Input File.\n");
		exit(1);
	}
	
	yyin=fp;
	icfile.open("icfile.asm");
	yyparse();
	fclose(fp);
	fclose(parseLog);
	return 0;
}

