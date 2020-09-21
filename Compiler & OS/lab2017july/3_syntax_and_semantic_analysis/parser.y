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
    fprintf(parseLog,"At Line %d, ERROR-> %s\n",line_count,s);
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
        {
            fprintf(parseLog, "GRAMMER RULE: program -> program unit\n"); 
        }
        |   unit
        {
            fprintf(parseLog, "GRAMMER RULE: program -> unit\n"); 
        }
    ;

unit : var_declaration
     {
            fprintf(parseLog, "GRAMMER RULE: unit -> var_declaration\n"); 
     }
     | func_declaration
     {
            fprintf(parseLog, "GRAMMER RULE: unit -> func_declaration\n"); 
     }
     | func_definition
     {
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
                     fprintf(parseLog, "GRAMMER RULE: compound_statement -> LCURL statements RCURL  \n"); 
                 }
                   | LCURL RCURL
                 {
                     fprintf(parseLog, "GRAMMER RULE: compound_statement -> LCURL RCURL  \n"); 
                 }
            ;

var_declaration : type_specifier declaration_list SEMICOLON
                {
                    fprintf(parseLog, "GRAMMER RULE: var_declaration -> type_specifier declaration_list SEMICOLON\n"); 
                }
                ;

type_specifier	: INT
                {
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> INT\n"); 
                }
                | FLOAT
                {
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> FLOAT\n"); 
                }
                | VOID
                {
                    fprintf(parseLog, "GRAMMER RULE: type_specifier -> VOID\n"); 
                }
                ;

declaration_list : declaration_list COMMA ID
                 {
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> declaration_list COMMA ID \n"); 
                 }
                 | declaration_list COMMA ID LSQBRAC CONST_INT RSQBRAC
                 {
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> declaration_list COMMA ID LSQBRAC CONST_INT RSQBRAC \n"); 
                 }
                 | ID
                 {
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> ID \n"); 
                 }
                 | ID LSQBRAC CONST_INT RSQBRAC
                 {
                     fprintf(parseLog, "GRAMMER RULE: declaration_list -> ID LSQBRAC CONST_INT RSQBRAC \n"); 
                 }
                 ;

statements : statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statements -> statement  \n"); 
                 }
                 | statements statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statements -> statements statement  \n"); 
                 }
                 ;

statement : var_declaration
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> var_declaration  \n"); 
                 }
                 | expression_statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> expression_statement  \n"); 
                 }
                 | compound_statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> compound_statement  \n"); 
                 }
                 | FOR LPAREN expression_statement expression_statement expression RPAREN statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> FOR LPAREN expression_statement expression_statement expression RPAREN statement  \n"); 
                 }
                 | IF LPAREN expression RPAREN statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> IF LPAREN expression RPAREN statement  \n"); 
                 }
                 | IF LPAREN expression RPAREN statement ELSE statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> IF LPAREN expression RPAREN statement ELSE statement  \n"); 
                 }
                 | WHILE LPAREN expression RPAREN statement
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> WHILE LPAREN expression RPAREN statement  \n"); 
                 }
                 | PRINTLN LPAREN ID RPAREN SEMICOLON
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> PRINTLN LPAREN ID RPAREN SEMICOLON  \n"); 
                 }
                 | RETURN expression SEMICOLON
                 {
                     fprintf(parseLog, "GRAMMER RULE: statement -> RETURN expression SEMICOLON  \n"); 
                 }
                 ;

expression_statement 	: SEMICOLON			
                        {
                            fprintf(parseLog, "GRAMMER RULE: expression_statement -> SEMICOLON  \n"); 
                        }
                        | expression SEMICOLON 
                        {
                            fprintf(parseLog, "GRAMMER RULE: expression_statement -> expression SEMICOLON   \n"); 
                        }
                        ;

variable : ID 		
                 {
                     fprintf(parseLog, "GRAMMER RULE: variable -> ID 		  \n"); 
                 }
         | ID LSQBRAC expression RSQBRAC 
                 {
                     fprintf(parseLog, "GRAMMER RULE: variable -> ID LSQBRAC expression RSQBRAC   \n"); 
                 }
     ;

expression : logic_expression	
                 {
                     fprintf(parseLog, "GRAMMER RULE: expression -> logic_expression	  \n"); 
                 }
               | variable ASSIGNOP logic_expression 	
                 {
                     fprintf(parseLog, "GRAMMER RULE: expression -> variable ASSIGNOP logic_expression 	  \n"); 
                 }
       ;

logic_expression : rel_expression 	
                 {
                     fprintf(parseLog, "GRAMMER RULE: logic_expression -> rel_expression 	  \n"); 
                 }
                 | rel_expression LOGICOP rel_expression 	
                 {
                     fprintf(parseLog, "GRAMMER RULE: logic_expression -> rel_expression LOGICOP rel_expression 	  \n"); 
                 }
         ;

rel_expression	: simple_expression 
                 {
                     fprintf(parseLog, "GRAMMER RULE: rel_expression -> simple_expression   \n"); 
                 }
               | simple_expression RELOP simple_expression	
                 {
                     fprintf(parseLog, "GRAMMER RULE: rel_expression -> simple_expression RELOP simple_expression	  \n"); 
                 }
        ;

simple_expression : term 
                 {
                     fprintf(parseLog, "GRAMMER RULE: simple_expression -> term   \n"); 
                 }
                  | simple_expression ADDOP term 
                 {
                     fprintf(parseLog, "GRAMMER RULE: simple_expression -> simple_expression ADDOP term   \n"); 
                 }
          ;

term :	unary_expression
                 {
                     fprintf(parseLog, "GRAMMER RULE:  term ->	unary_expression  \n"); 
                 }
     |  term MULOP unary_expression
                 {
                     fprintf(parseLog, "GRAMMER RULE: term -> term MULOP unary_expression  \n"); 
                 }
     ;

unary_expression : ADDOP unary_expression  
                 {
                     fprintf(parseLog, "GRAMMER RULE: unary_expression -> ADDOP unary_expression    \n"); 
                 }
                 | NOT unary_expression 
                 {
                     fprintf(parseLog, "GRAMMER RULE: unary_expression -> NOT unary_expression   \n"); 
                 }
                 | factor 
                 {
                     fprintf(parseLog, "GRAMMER RULE: unary_expression -> factor   \n"); 
                 }
                 ;

factor	: variable 
        {
        fprintf(parseLog, "GRAMMER RULE:  factor  -> variable   \n"); 
        }
        | ID LPAREN argument_list RPAREN
        {
            fprintf(parseLog, "GRAMMER RULE: factor-> ID LPAREN argument_list RPAREN  \n"); 
        }
        | LPAREN expression RPAREN
        {
            fprintf(parseLog, "GRAMMER RULE: factor -> LPAREN expression RPAREN  \n"); 
        }
        | CONST_INT 
        {
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_INT   \n"); 
        }
        | CONST_FLOAT
        {
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_FLOAT  \n"); 
        }
        | CONST_CHAR
        {
            fprintf(parseLog, "GRAMMER RULE: factor -> CONST_CHAR  \n"); 
        }
        | variable INCOP 
        {
            fprintf(parseLog, "GRAMMER RULE: factor -> variable INCOP   \n"); 
        }
    | variable DECOP
                 {
                     fprintf(parseLog, "GRAMMER RULE: factor -> variable DECOP  \n"); 
                 }
    ;

argument_list : argument_list COMMA logic_expression
                 {
                     fprintf(parseLog, "GRAMMER RULE: argument_list -> argument_list COMMA logic_expression  \n"); 
                 }
              | logic_expression
                 {
                     fprintf(parseLog, "GRAMMER RULE: argument_list -> logic_expression  \n"); 
                 }
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
