%{
#include <stdio.h>
#include "Symbol_table.h"
#define YYSTYPE Symbol_info*
#include "bits/stdc++.h"
#include "iostream"

#include "stdlib.h"
#include "string.h"

using namespace std;

FILE *logout;
FILE *tokenout;
FILE *parseLog;
FILE *asmout;

Symbol_table parser_table;
stringstream ss;
extern FILE *yyin;
extern int line_count;

int tempCount = 0, labelCount = 0;

extern int yylex(void);
extern int yyparse(void);

/*extern int error; //TODO: Error reporting*/

extern "C"
{
    //Kill me
}
extern char yytext[];
extern int column;

extern YYSTYPE yylval;

char *newLabel()
{
	char *lb= new char[4];
	strcpy(lb,"L");
	char b[3];
	sprintf(b,"%d", labelCount);
	labelCount++;
	strcat(lb,b);
	return lb;
}

char *newTemp()
{
	char *t= new char[4];
	strcpy(t,"t");
	char b[3];
	sprintf(b,"%d", tempCount);
	tempCount++;
	strcat(t,b);
	return t;
}

string getLabel()	//put type = "b" for byte
{
    stringstream ss;
    ss << "label" << labelCount++;
    return ss.str();
}

string getTemp(string type = "w")	//put type = "b" for byte
{
    stringstream ss;
    return ss.str();
}


//ofstream ir_code_stream; 

void yyerror(char *s)
{
    fprintf(stderr,"At Line %d, ERROR-> %s\n",line_count,s);
    fprintf(parseLog,"At Line %d, ERROR-> %s\n",line_count,s);
    return;
}

void adele(){
    cout << "Hello from the other side"<< endl;
}

%}
%error-verbose

%token SEMICOLON INT FLOAT CHAR COMMA LCURL RCURL 
%token ID LSQBRAC RSQBRAC FOR LPAREN RPAREN IF 
%token ELSE WHILE PRINTLN RETURN ASSIGNOP LOGICOP 
%token RELOP ADDOP MULOP NOT INCOP DECOP 
%token CONST_INT CONST_FLOAT CONST_CHAR
%token VOID

%nonassoc ELSE

%%
start : program
      {
            fprintf(asmout, ($1->code).c_str());
      }
    ;

program : program unit 
        {
            $$ = $1;
            $$->code += $2->code;
            fprintf(parseLog, "GRAMMER RULE: program -> program unit\n"); 
        }
        |   unit
        {
            $$ = $1;
            fprintf(parseLog, "GRAMMER RULE: program -> unit\n"); 
        }
    ;

unit : var_declaration
     {
            $$  = $1;
            fprintf(parseLog, "GRAMMER RULE: unit -> var_declaration\n"); 
     }
     | func_declaration
     {
            $$ = $1;
            fprintf(parseLog, "GRAMMER RULE: unit -> func_declaration\n"); 
     }
     | func_definition
     {
            $$ = $1;
            fprintf(parseLog, "GRAMMER RULE: unit -> func_definition\n"); 
     }
     ;

func_declaration : type_specifier ID LPAREN parameter_list RPAREN SEMICOLON
                 {
                     fprintf(parseLog, "GRAMMER RULE: func_declaration -> type_specifier ID LPAREN parameter_list RPAREN SEMICOLON  \n"); 
                 }
                 ;

func_definition : type_specifier ID LPAREN parameter_list RPAREN compound_statement
                 {
                    $$=new Symbol_info();
                    // Need to work on this

                    $$->code+="\n;function definition";
                    $$->code+="\nPROC "+$2->symbol+"\n\n";

                    if($2->symbol!="main")
                    {
                        $$->code+="PUSH AX\n";
                        $$->code+="PUSH BX\n";
                        $$->code+="PUSH CX\n";
                        $$->code+="PUSH DX\n";
                    }

                    $$->code += $6->code ;

                    if($2->symbol!="main") {
                        $$->code+="POP DX\n";
                        $$->code+="POP CX\n";
                        $$->code+="POP BX\n";
                        $$->code+="POP AX\n";
                    }

                    //Source of problem
                    fprintf(parseLog, "GRAMMER RULE: func_definition -> type_specifier ID LPAREN parameter_list RPAREN compound_statement  \n"); 
                 }
                ;

parameter_list  : parameter_list COMMA type_specifier ID
                {
                    fprintf(parseLog, "GRAMMER RULE: parameter_list  -> parameter_list COMMA type_specifier ID  \n"); 
                }
                | parameter_list COMMA type_specifier	 
                {
                  fprintf(parseLog, "GRAMMER RULE: parameter_list -> parameter_list COMMA type_specifier	   \n"); 
                }
                | type_specifier ID
                {
                    fprintf(parseLog, "GRAMMER RULE: parameter_list -> type_specifier ID  \n"); 
                }
                | unit
                {
                    fprintf(parseLog, "GRAMMER RULE: parameter_list -> unit  \n"); 
                }
                | type_specifier
                {
                  fprintf(parseLog, "GRAMMER RULE: parameter_list -> type_specifier  \n"); 
                }
                |
                ;

compound_statement : LCURL statements RCURL
                 {
                    $$ = $2;
                    fprintf(parseLog, "GRAMMER RULE: compound_statement -> LCURL statements RCURL  \n"); 
                 }
                   | LCURL RCURL
                 {
                     fprintf(parseLog, "GRAMMER RULE: compound_statement -> LCURL RCURL  \n"); 
                     $$ = new Symbol_info("compound_statement", "dummy");
                     //TODO Make new Symbol_info
                 }
            ;

var_declaration : type_specifier declaration_list SEMICOLON
                {
                    $$ = $2;
                    fprintf(parseLog, "GRAMMER RULE: var_declaration -> type_specifier declaration_list SEMICOLON\n"); 
                }
                ;

type_specifier	: INT
                {
                    $$ = new Symbol_info("int", "KEYWORD");
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> INT\n"); 
                }
                | FLOAT
                {
                    $$ = new Symbol_info("float", "KEYWORD");
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> FLOAT\n"); 
                }
                | VOID
                {
                    $$ = new Symbol_info("void", "KEYWORD");
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> VOID\n"); 
                }
                ;


declaration_list : declaration_list COMMA ID
                 {
                    // These are the source of all problems
                     //TODO something in line 289
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> declaration_list COMMA ID \n"); 
                 }
                 | declaration_list COMMA ID LSQBRAC CONST_INT RSQBRAC
                 {
                     //TODO
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> declaration_list COMMA ID LSQBRAC CONST_INT RSQBRAC \n"); 
                 }
                 | ID
                 {
                    $$ = new Symbol_info($1);
                    fprintf(parseLog, "GRAMMER RULE: declaration_list -> ID \n"); 
                 }
                 | ID LSQBRAC CONST_INT RSQBRAC
                 {
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> ID LSQBRAC CONST_INT RSQBRAC \n"); 
                 }
                 ;

statements : statement
                 {
                     $$ = $1;
                     fprintf(parseLog, "GRAMMER RULE: statements -> statement  \n"); 

                 }
                 | statements statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statements -> statements statement  \n"); 
                     $$ = $1;
                     $$->code += $2->code;
                     delete $2;
                 }
                 ;

statement : var_declaration
                 {
                    $$=$1;
                    fprintf(parseLog, "GRAMMER RULE: statement -> var_declaration  \n"); 
                 }
                 | expression_statement
                 {
                     $$ = $1;
                     fprintf(parseLog, "GRAMMER RULE: statement -> expression_statement  \n"); 
                 }
                 | compound_statement
                 {
                     $$ = new Symbol_info($1);
                     fprintf(parseLog, "GRAMMER RULE: statement -> compound_statement  \n"); 
                 }
                 | FOR LPAREN expression_statement expression_statement expression RPAREN statement
                 {
					/*
                        for (i=0;i>5;i++) print(x)
						$3's code at first, which is already done by assigning $$=$3
						create two labels and append one of them in $$->code
						compare $4's symbol with 0
						if equal jump to 2nd label
						append $7's code
						append $5's code
						append the second label in the code
					*/
                     $$ = $3;
                     $$->code += "\n;Forloop";
                     char *label = newLabel();
                     char *endlabel = newLabel();

                     $$->code += "\n"+ string(label)+ ":\n";
                     $$->code += ";For loop - test expression\n";
                     $$->code += $4->code;
                     $$->code += "CMP "+ $4->symbol+ " ,0\n";
                     $$->code += "JE "+ string(endlabel)+ "\n";
                     $$->code += ";For loop - body\n";
                     $$->code += $7->code;
                     $$->code += ";For loop - increment\n";
                     $$->code += $5->code;
                     $$->code += "JMP "+ string(label)+ "\n";
                     $$->code += "\n"+ string(endlabel)+ ":\n";

                     fprintf(parseLog, "GRAMMER RULE: statement -> FOR LPAREN expression_statement expression_statement expression RPAREN statement  \n"); 
                     // TODO Some code in Line 96 of the template
                 }
                 | IF LPAREN expression RPAREN statement
                 {
                    fprintf(parseLog, "GRAMMER RULE: statement -> IF LPAREN expression RPAREN statement  \n"); 
					$$=$3;
					
                    $$->code += "\n;If statement\n";
					char *label=newLabel();
					$$->code+="MOV AX, "+$3->getSymbol()+"\n";
					$$->code+="CMP AX, 0\n";
					$$->code+="JE "+string(label)+"\n";
					$$->code+=$5->code;
					$$->code+=string(label)+":\n";
                 }
                 | IF LPAREN expression RPAREN statement ELSE statement
                 {
                    fprintf(parseLog, "GRAMMER RULE: statement -> IF LPAREN expression RPAREN statement ELSE statement  \n"); 
					$$=$3;
					
                    $$->code += "\n;If-Else statement\n";
					char *label=newLabel();
					char *label_if=newLabel();
					char *label_else=newLabel();
					$$->code+="MOV AX, "+$3->getSymbol()+"\n";
					$$->code+="CMP AX, 0\n";
					$$->code+="JE "+string(label_else)+"\n";
					$$->code+=$5->code;     
					$$->code+="JE "+string(label_if)+"\n";
					$$->code+=string(label_else)+":\n";
					$$->code+=$7->code;     
					$$->code+=string(label_if)+":\n";
                 }
                 | WHILE LPAREN expression RPAREN statement
                 {
                     $$ = new Symbol_info();
                     $$->code += "\n;While Loop";
                     char *label = newLabel();
                     char *endlabel = newLabel();

                     $$->code += "\n"+ string(label)+ ":\n";
                     $$->code += ";While loop - Test expression\n";
                     $$->code += $3->code;
                     $$->code += "CMP "+ $3->symbol+ " ,0\n";
                     $$->code += "JE "+ string(endlabel)+ "\n";
                     $$->code += ";While loop - body\n";
                     $$->code += $5->code;
                     $$->code += "JMP "+ string(label)+ "\n";
                     $$->code += "\n"+ string(endlabel)+ ":\n";
                    fprintf(parseLog, "GRAMMER RULE: statement -> WHILE LPAREN expression RPAREN statement  \n"); 
                 }
                 | PRINTLN LPAREN ID RPAREN SEMICOLON
                 {
                    //TODO 
                    $$->code += "\n;Print Variable " + $3->symbol;
                    fprintf(parseLog, "GRAMMER RULE: statement -> PRINTLN LPAREN ID RPAREN SEMICOLON  \n"); 
                 }
                 | RETURN expression SEMICOLON
                 {
                    $$ = $1;
                    fprintf(parseLog, "GRAMMER RULE: statement -> RETURN expression SEMICOLON  \n"); 
                 }
                 ;

expression_statement 	: SEMICOLON			
                        {
                            $$ = new Symbol_info(";", "SEMICOLON");
                            fprintf(parseLog, "GRAMMER RULE: expression_statement -> SEMICOLON  \n"); 
                        }
                        | expression SEMICOLON 
                        {
                            $$ = $1;
                            fprintf(parseLog, "GRAMMER RULE: expression_statement -> expression SEMICOLON   \n"); 
                        }
                        ;

variable : ID 		
                 {
                    $$= new Symbol_info($1);
                    fprintf(parseLog, "GRAMMER RULE: variable -> ID 		  \n"); 
                 }
                 | ID LSQBRAC expression RSQBRAC 
                 {
                    $$= new Symbol_info($1);
                    $$->setType("array");

                    $$->code=$3->code+"MOV BX, " +$3->getSymbol() +"\nADD BX, BX\n";
                    
                    delete $3;
                    fprintf(parseLog, "GRAMMER RULE: variable -> ID LSQBRAC expression RSQBRAC   \n"); 
                 }
     ;

expression : logic_expression	
           {
               $$ = $1;
               $$->code += ";Logical Expression\n";
               fprintf(parseLog, "GRAMMER RULE: expression -> logic_expression	  \n"); 
           }
           | variable ASSIGNOP logic_expression 	
           {
				$$=$1;
				$$->code+="\n;Assignment Operation\n";
				$$->code+=$3->code;
				$$->code+="MOV AX, "+$3->getSymbol()+"\n";
				if($$->getType()=="notarray"){ 
					$$->code+= "MOV "+$1->getSymbol()+", AX\n";
				}

				else{
					$$->code+= "MOV  "+ $1->getSymbol()+"[BX], AX\n";

				}
				delete $3;
                
                fprintf(parseLog, "GRAMMER RULE: expression -> variable ASSIGNOP logic_expression 	  \n"); 
           }
       ;

logic_expression : rel_expression 	
                 {
                    $$ = $1;
                    fprintf(parseLog, "GRAMMER RULE: logic_expression -> rel_expression 	  \n"); 
                 }
                 | rel_expression LOGICOP rel_expression 	
                 {
                    $$=new Symbol_info($1);
                    /*$$ = new Symbol_info();*/
                    $$->code+=$3->code;
                    $$->code+="\n;Doing Logical opeation between two relational expressions\n";
					
                    char *tr = newTemp();
                    char *f = newLabel();
                    char *t = newLabel();

					if($2->getSymbol()=="&&"){
						/* 
						Check whether both operands value is 1. If both are one set value of a temporary variable to 1
						otherwise 0
						*/
                        //TODO Priority 1
                        $$->code += "\n;Doing AND operation\n";
                        
                        /*this is full of errors y'all*/
                        $$->code += "CMP " + $1->symbol + ", 1\n";
                        $$->code += "JNE " + string(f) + "\n";
                        $$->code += "CMP " + $3->symbol + ", 1\n";
                        $$->code += "JNE " + string(f) + "\n";
                        $$->code += "MOV " + string(tr) + " ,1\n";
                        $$->code += "JMP " + string(t) + " \n";
                        $$->code +=  string(f) + ": \n";
                        $$->code += "MOV " + string(tr) + " ,0\n";
                        $$->code += string(t) + " :\n";

					}
					else if($2->getSymbol()=="||"){
                        $$->code += "\n;Doing OR operation\n";

                        $$->code += "CMP " + $1->symbol + ", 1\n";
                        $$->code += "JE " + string(t) + "\n";
                        $$->code += "CMP " + $3->symbol + ", 1\n";
                        $$->code += "JE " + string(t) + "\n";
                        $$->code += "MOV " + string(tr) + " ,0\n";
                        $$->code += "JMP " + string(f) + " \n";
                        $$->code += string(t) + " :\n";
                        $$->code += "MOV " + string(t) + " ,1\n";
                        $$->code +=  string(f) + ": \n";
						
					}
					delete $3;
                    $$->symbol = string(t);
                    fprintf(parseLog, "GRAMMER RULE: logic_expression -> rel_expression LOGICOP rel_expression 	  \n"); 
                 }
         ;

rel_expression	: simple_expression 
                 {
                     /*$$=$1;*/
                     $$=new Symbol_info($1);
                     fprintf(parseLog, "GRAMMER RULE: rel_expression -> simple_expression   \n"); 
                 }
               | simple_expression RELOP simple_expression	
                 {
                    $$=$1;
                    $$->code+=$3->code;
                    $$->code+="MOV AX, " + $1->getSymbol()+"\n";
                    $$->code+="CMP AX, " + $3->getSymbol()+"\n";
                    char *temp=newTemp();
                    char *label1=newLabel();
                    char *label2=newLabel();
                    if($2->getSymbol()=="<"){
                        $$->code+="JL " + string(label1)+"\n";
                    }
                    else if($2->getSymbol()=="<="){
                    //TODO
                        $$->code+="JLE " + string(label1)+"\n";
                    }
                    else if($2->getSymbol()==">"){
                        $$->code+="JG " + string(label1)+"\n";
                    }
                    else if($2->getSymbol()==">="){
                        $$->code+="JGE " + string(label1)+"\n";
                    }
                    else if($2->getSymbol()=="=="){
                        $$->code+="JE " + string(label1)+"\n";
                    }
                    else{
                        $$->code+="JNE " + string(label1)+"\n";
                    }
                    
                    $$->code+="MOV "+string(temp) +", 0\n";
                    $$->code+="JMP "+string(label2) +"\n";
                    $$->code+=string(label1)+":\nMOV "+string(temp)+", 1\n";
                    $$->code+=string(label2)+":\n";
                    $$->setSymbol(temp);
                    delete $3;

                    fprintf(parseLog, "GRAMMER RULE: rel_expression -> simple_expression RELOP simple_expression	  \n"); 
                 }
        ;

simple_expression : term 
                 {
                     $$ = $1;
                     fprintf(parseLog, "GRAMMER RULE: simple_expression -> term   \n"); 
                 }
                  | simple_expression ADDOP term 
                 {
                    $$=new Symbol_info($1);
                    $$->code+=$3->code;
                    char *t = newTemp();
                    
                    // MOVe one of the operands to a register, perform addition or subtraction with the other operand and MOVe the result in a temporary variable  
                    
                    if($2->getSymbol()=="+"){
                        $$->code += "\n;ADDING THINGS\n";
                        $$->code+="MOV AX,"+$1->symbol+"\n";
                        $$->code+="MOV BX,"+$3->symbol+"\n";
                        $$->code+="ADD AX,BX\n";
                        $$->code+="MOV "+string(t)+",AX\n";
                        $$->code+="\n";
                    }
                    else{
                        $$->code += "\n;SUBBING THINGS\n";
                        $$->code+="MOV AX,"+$1->symbol+"\n";
                        $$->code+="MOV BX,"+$3->symbol+"\n";
                        $$->code+="SUB AX,BX\n";
                        $$->code+="MOV "+string(t)+",AX\n";
                        $$->code+="\n";
                    
                    }
                    delete $3;

                    fprintf(parseLog, "GRAMMER RULE: simple_expression -> simple_expression ADDOP term   \n"); 
                 }
          ;

term :	unary_expression
                 {
                    $$ = $1;
                    fprintf(parseLog, "GRAMMER RULE:  term ->	unary_expression  \n"); 
                 }
     |  term MULOP unary_expression
                 {
                 //TODO
                    $$=$1;
                    $$->code += $3->code;
                    $$->code += "MOV AX, "+ $1->getSymbol()+"\n";
                    $$->code += "MOV BX, "+ $3->getSymbol() +"\n";
                    char *temp=newTemp();
                    if($2->getSymbol()=="*"){
                        $$->code += "\n;Multiplication\n";
                        $$->code += "MUL BX\n";
                        $$->code += "MOV "+ string(temp) + ", AX\n\n";
                    }
                    else if($2->getSymbol()=="/"){
                        // TODO
                        // Division
                        // clear dx, perform 'div BX' and MOV AX to temp
                        $$->code += "\n;Division\n";
                        $$->code += "POP DX\n";
                        $$->code += "DIV BX\n";
                        $$->code += "MOV "+ string(temp) + ", AX\n\n";
                    }
                    else{
                        // % or MOD
                        // clear dx, perform 'div BX' and MOV dx to temp
                        $$->code += "\n;Modulo\n";
                        $$->code += "POP DX\n";
                        $$->code += "DIV BX\n";
                        $$->code += "MOV "+ string(temp) + ", DX\n\n";
                    }
                    $$->setSymbol(temp);
                    delete $3;
                    
                    fprintf(parseLog, "GRAMMER RULE: term -> term MULOP unary_expression  \n"); 
                 }
     ;

unary_expression : ADDOP unary_expression  
                 {
                    $$=new Symbol_info($2);
                    //TODO Perform NEG operation if the symbol of ADDOP is '-'
                    fprintf(parseLog, "GRAMMER RULE: unary_expression -> ADDOP unary_expression    \n"); 
                 }
                 | NOT unary_expression 
                 {
                    $$=new Symbol_info($2);
                    char *temp=newTemp();
                    $$->code="MOV AX, " + $2->getSymbol() + "\n";
                    $$->code+="not AX\n";
                    $$->code+="MOV "+string(temp)+", AX";
                    fprintf(parseLog, "GRAMMER RULE: unary_expression -> NOT unary_expression   \n"); 
                 }
                 | factor 
                 {
                    $$ = $1;
                    fprintf(parseLog, "GRAMMER RULE: unary_expression -> factor   \n"); 
                 }
                 ;

factor	: variable 
        {
			$$= $1;
			if($$->getType()=="notarray"){
			    //TODO Do something
			}
			
			else{
				char *temp= newTemp();
				$$->code+="MOV AX, " + $1->getSymbol() + "[BX]\n";
				$$->code+= "MOV " + string(temp) + ", AX\n";
				$$->setSymbol(temp);
            }
            fprintf(parseLog, "GRAMMER RULE:  factor  -> variable   \n"); 
        }
        | ID LPAREN argument_list RPAREN
        {
            $$ = $3;

            char *temp= newTemp();
            $$->code+="MOV AX, " + $1->getSymbol() + "[BX]\n";
            $$->code+= "MOV " + string(temp) + ", AX\n";
            $$->setSymbol(temp);

            fprintf(parseLog, "GRAMMER RULE: factor-> ID LPAREN argument_list RPAREN  \n"); 
        }
        | LPAREN expression RPAREN
        {
           $$ = $2; 
           fprintf(parseLog, "GRAMMER RULE: factor -> LPAREN expression RPAREN  \n"); 
        }
        | CONST_INT 
        {
            $$ = new Symbol_info($1);
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_INT   \n"); 
        }
        | CONST_FLOAT
        {
            $$ = new Symbol_info($1);
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_FLOAT  \n"); 
        }
        | CONST_CHAR
        {
            $$ = new Symbol_info($1);
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_CHAR  \n"); 
        }
        | variable INCOP 
        {
            $$ = new Symbol_info($1);
            $$->code += "\n;Increment\n";
            char *t = newTemp();
            if ($1->getType()=="notarray") {
            // TODO Perform increment
                char *temp = newTemp();
                $$->code+="ADD BX,2\n";
                $$->code+="MOV "+string(temp)+","+$1->symbol+"[BX]\n";
            }
            else {
                $$->code+= "INC " + $1->symbol + "\n";
            }
            $$->symbol = string(t);
            fprintf(parseLog, "GRAMMER RULE: factor -> variable INCOP   \n"); 
        }
        | variable DECOP
        {
            $$ = new Symbol_info($1);
            $$->code += "\n;Decrement\n";
            char *t = newTemp();
            if ($1->getType()=="notarray") {
                char *temp = newTemp();
                $$->code+="SUB BX,2\n";
                $$->code+="MOV "+string(temp)+","+$1->symbol+"[BX]\n";
            }
            else {
                $$->code+= "DEC " + $1->symbol + "\n";
            }
            $$->symbol = string(t);
            fprintf(parseLog, "GRAMMER RULE: factor -> variable DECOP  \n"); 
        }
    ;

argument_list : argument_list COMMA logic_expression
                 {
                     $$ = $1;
                     $$->code += $2->code;
                     fprintf(parseLog, "GRAMMER RULE: argument_list -> argument_list COMMA logic_expression  \n"); 
                 }
              | logic_expression
                 {
                     $$ = $1;
                     fprintf(parseLog, "GRAMMER RULE: argument_list -> logic_expression  \n"); 
                 }
          |
          ;


%%

int main(int argc,char *argv[]){
    adele();
    tokenout= fopen("token.txt","w");
    parseLog = fopen("log.txt", "w");
    asmout = fopen("code.asm", "w");

    string c = "";

    c += ".MODEL SMALL\n\n";
    c += ".STACK 100H\n\n";
    c += ".data\n";
    c += "x dw 0\n";
    c += "y dw 0\n";
    c += "z dw 0\n";
    c += "a dw 0\n";
    c += "b dw 0\n";
    c += "c dw 0\n";
    c += "t0 dw 0\n";
    c += "t1 dw 0\n";
    c += "t2 dw 0\n";
    c += "t3 dw 0\n";
    c += "t4 dw 0\n";
    c += "t5 dw 0\n";
    c += "t6 dw 0\n";
    c += "t7 dw 0\n";
    c += "t8 dw 0\n";
    c += "t9 dw 0\n";
    c += "t10 dw 0\n";
    c += "t11 dw 0\n";
    c += "t12 dw 0\n";
    c += "t13 dw 0\n";
    c += "t14 dw 0\n";
    c += "t15 dw 0\n";
    c += "t16 dw 0\n\n\n";
    c += ".CODE\n";

    fprintf(asmout, c.c_str());

    fprintf(parseLog, "Program start: Line Count: 1\n");
    yyparse();
    fclose(tokenout);
    fclose(parseLog);
    fclose(asmout);
    printf ("\nTotal line Count: %d\n", line_count);
    /*parser_table.print(logout);*/
    

return 0;
}
