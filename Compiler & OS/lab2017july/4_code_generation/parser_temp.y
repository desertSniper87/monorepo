%{
#include <stdio.h>
#include "Symbol_table.h"
#define YYSTYPE Symbol_info*
#include "bits/stdc++.h"
#include "iostream"

FILE *logout;
FILE *tokenout;
FILE *parseLog;

Symbol_table parser_table;
stringstream ss;
extern FILE *yyin;
extern int line_count;
int tempCount = 0, labelCount = 0;

extern int yylex(void);
extern int yyparse(void);

extern "C"
{
    //Kill me
}
extern char yytext[];
extern int column;
ofstream assembly;

string getTemp(string type = "w")	//put type = "b" for byte
{
    stringstream ss;
    return ss.str();
}

string getLabel()	//put type = "b" for byte
{
    stringstream ss;
    ss << "label" << labelCount++;
    return ss.str();
}


//ofstream ir_code_stream; 

void yyerror(char *s)
{
    fprintf(stderr,"At Line %d, ERROR-> %s\n",line_count,s);
    return;
}

void adele(){
    cout << "Hello from the other side"<< endl;
}

%}

%token SEMICOLON INT FLOAT CHAR COMMA LCURL RCURL 
%token ID LSQBRAC RSQBRAC FOR LPAREN RPAREN IF 
%token ELSE WHILE PRINTLN RETURN ASSIGNOP LOGICOP 
%token RELOP ADDOP MULOP NOT INCOP DECOP 
%token CONST_INT CONST_FLOAT CONST_CHAR
%token VOID

%%
start : program;

program : program unit 
	| unit
	;
	
unit : var_declaration
     | func_declaration
     | func_definition
     ;
     
func_declaration : type_specifier ID LPAREN parameter_list RPAREN SEMICOLON
		 ;
		 
func_definition : type_specifier ID LPAREN parameter_list RPAREN compound_statement
 		 ;
 		 
parameter_list  : parameter_list COMMA type_specifier ID
		| parameter_list COMMA type_specifier	 
 		| type_specifier ID
 		| type_specifier
 		|
 		;
 		
compound_statement : LCURL statements RCURL
 		    | LCURL RCURL
 		    ;
 		    
var_declaration : type_specifier declaration_list SEMICOLON
 		 ;
 		 
type_specifier	: INT
 		| FLOAT
 		| VOID
 		;
 		
declaration_list : declaration_list COMMA ID
 		  | declaration_list COMMA ID LSQBRAC CONST_INT RSQBRAC
 		  | ID
 		  | ID LSQBRAC CONST_INT RSQBRAC
 		  ;
 		  
statements : statement
	   | statements statement
	   ;
	   
statement : var_declaration
	  | expression_statement
	  | compound_statement
	  | FOR LPAREN expression_statement expression_statement expression RPAREN statement
	  | IF LPAREN expression RPAREN statement
	  | IF LPAREN expression RPAREN statement ELSE statement
	  | WHILE LPAREN expression RPAREN statement
	  | PRINTLN LPAREN ID RPAREN SEMICOLON
	  | RETURN expression SEMICOLON
	  ;
	  
expression_statement 	: SEMICOLON			
			| expression SEMICOLON 
			;
	  
variable : ID 		
	 | ID LSQBRAC expression RSQBRAC 
	 ;
	 
 expression : logic_expression	
	   | variable ASSIGNOP logic_expression 	
	   ;
			
logic_expression : rel_expression 	
		 | rel_expression LOGICOP rel_expression 	
		 ;
			
rel_expression	: simple_expression 
		| simple_expression RELOP simple_expression	
		;
				
simple_expression : term 
		  | simple_expression ADDOP term 
		  ;
					
term :	unary_expression
     |  term MULOP unary_expression
     ;

unary_expression : ADDOP unary_expression  
		 | NOT unary_expression 
		 | factor 
		 ;
	
factor	: variable 
	| ID LPAREN argument_list RPAREN
	| LPAREN expression RPAREN
	| CONST_INT 
	| CONST_FLOAT
	| CONST_CHAR
	| variable INCOP 
	| variable DECOP
	;
	
argument_list : argument_list COMMA logic_expression
	      | logic_expression
	      |
	      ;
 
%%

int main(int argc,char *argv[]){
    adele();
    tokenout= fopen("token.txt","w");
    parseLog = fopen("log.txt", "w");
    fprintf(parseLog, "Program start: Line Count: 1\n");
    yyparse();
    fclose(tokenout);
    fclose(parseLog);
    printf ("\nTotal line Count: %d\n", line_count);
    /*parser_table.print(logout);*/

return 0;
}
