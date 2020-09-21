%{
#include <stdio.h>
#include "SymbolTable.h"
#define YYSTYPE SymbolInfo*
#include "bits/stdc++.h"
#include "iostream"

FILE *logout;
FILE *tokenout;
FILE *parseLog;

SymbolTable parser_table;
stringstream ss;
extern FILE *yyin;
extern int line_count;
int tempCount = 0, labelCount = 0;

extern int yylex(void);
extern int yyparse(void);
string assembly_codes = "";
string varDec = "";

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
	varDec+= ss.str()+" d"+type[0]+" 0\n";
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

%token SEMICOLON INT FLOAT CHAR COMMA TYPEDEF LCURL RCURL
%token ID LTHIRD CONST RTHIRD FOR LBRAKET RBRAKET IF STATIC
%token ELSE WHILE PRINTLN RETURN ASSIGNOP LOGICOP REGISTER
%token RELOP ADDOP MULOP NOT INCOP DECOP CONST_CHAR
%token CONST_INT CONST_FLOAT MAIN TYPE_NAME VOLATILE AUTO
%token VOID SHORT LONG DOUBLE SIGNED UNSIGNED EXTERN
%token ENUM IDENTIFIER STRUCT UNION HEADER NUMBER STRING
%token OR_OP AND_OP EQ_OP NE_OP LEFT_OP GE_OP LE_OP RIGHT_OP

%%
Program : INT MAIN LBRAKET RBRAKET compound_statement
{
            	
            	fprintf ( parseLog, "PROGRAM ID '(' identifier_list ')' SEMICOLON declarations subprogram_declarations compound_statement\n");
            	assembly_codes = ".MODEL SMALL\n\n.STACK 100H\n";
            	getTemp();
            	getTemp();
            	assembly << assembly_codes<< "\n";
            	assembly << varDec;
            	assembly << "\n.CODE\n\nMAIN PROC\nMOV AX,@DATA\nMOV DS,AX\n";
            	
            	assembly << assembly_codes << "\n";
            	assembly << "\t\nmain endp\n";
            	assembly << "\n\n;PRINT FUNC\n";
            	assembly << "print proc  \n\tmov bp, sp\n\tmov ax, [bp+2]\n\tcmp ax, 0\n\tje return_print\n\t\n\tmov dx, 0\n\tmov bx, 10\n\tdiv bx\n\t\n\t;recalling\n\tpush dx\n\tpush ax\n\tcall print\n\t\n\t;printing\n\tpop dx\n\tadd dl, '0'\n\tmov ah, 2h\n\tint 21h\n\t\n\treturn_print:\n\t\tret 2\t\nprint endp\n";
//            	assembly << "\nEND MAIN\n";
            }
	;


type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| struct_or_union_specifier
	| enum_specifier
	| TYPE_NAME
	;


compound_statement 
		   : 
		   | '{' statement_list '}'
		   | '{' declaration_list '}'
		   | '{' declaration_list statement_list '}'
		   | '{' '}'
		   ;


declaration_list
	: declaration
	| declaration_list declaration
	;

statement_list
	: statement
	| statement_list statement
	;

			
declaration_list 
		 : declaration_list COMMA ID 
		 | declaration_list COMMA ID LTHIRD CONST_INT RTHIRD
		 | ID 
		 | ID LTHIRD CONST_INT RTHIRD 
		 ;

declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	;

declaration_specifiers
	: storage_class_specifier
	| storage_class_specifier declaration_specifiers
	| type_specifier
	| type_specifier declaration_specifiers
	| type_qualifier
	| type_qualifier declaration_specifiers
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator
	| declarator '=' initializer
	;


statement  : expression_statement 
	   | FOR LBRAKET expression_statement expression_statement expression RBRAKET statement
	   | IF LBRAKET expression RBRAKET statement
	   | IF LBRAKET expression RBRAKET statement ELSE statement
	   | WHILE LBRAKET expression RBRAKET statement 
	   | PRINTLN LBRAKET ID RBRAKET SEMICOLON 
	   | RETURN expression SEMICOLON 
	   ;
		
expression_statement	
			: SEMICOLON			
			| expression SEMICOLON 
			;

struct_declaration
	: specifier_qualifier_list struct_declarator_list ';'
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;
						
variable
	 : ID 		
	 | ID LTHIRD expression RTHIRD 
	 ;
			
expression
	   : logic_expression	
	   | variable ASSIGNOP logic_expression 	
	   ;
			
logic_expression 
		 : rel_expression 	
		 | rel_expression LOGICOP rel_expression 	
		 ;
			
rel_expression	
		: simple_expression 
		| simple_expression RELOP simple_expression	
		;

type_qualifier
	: CONST
	| VOLATILE
	;

initializer
	: assignment_expression
	| '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	;

initializer_list
	: initializer
	| initializer_list ',' initializer
	;


storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	;


assignment_operator
	: '='
	;

inclusive_or_expression
	: exclusive_or_expression
	| inclusive_or_expression '|' exclusive_or_expression
	;

exclusive_or_expression
	: and_expression
	| exclusive_or_expression '^' and_expression
	;


type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
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

and_expression
	: equality_expression
	| and_expression '&' equality_expression
	;

equality_expression
	: relational_expression
	| equality_expression EQ_OP relational_expression
	| equality_expression NE_OP relational_expression
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declarator
	: declarator
	| ':' constant_expression
	| declarator ':' constant_expression
	;
	
identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

relational_expression
	: shift_expression
	| relational_expression '<' shift_expression
	| relational_expression '>' shift_expression
	| relational_expression LE_OP shift_expression
	| relational_expression GE_OP shift_expression
	;


shift_expression
	: additive_expression
	| shift_expression LEFT_OP additive_expression
	| shift_expression RIGHT_OP additive_expression
	;

factor	: variable 
	| LBRAKET expression RBRAKET 
	| CONST_INT 
	| CONST_FLOAT
	| CONST_CHAR
	| factor INCOP 
	| factor DECOP
	;


parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

parameter_type_list
	: parameter_list
	;

logical_and_expression
	: inclusive_or_expression
	| logical_and_expression AND_OP inclusive_or_expression
	;

additive_expression
	: multiplicative_expression
	| additive_expression '+' multiplicative_expression
	| additive_expression '-' multiplicative_expression
	;

multiplicative_expression
	: cast_expression
	| multiplicative_expression '*' cast_expression
	| multiplicative_expression '/' cast_expression
	| multiplicative_expression '%' cast_expression
	;

cast_expression
	: unary_expression
	| '(' type_name ')' cast_expression
	;

type_name
	: specifier_qualifier_list
	| specifier_qualifier_list abstract_declarator
	;


specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;



struct_or_union_specifier
	: struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	| struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;


enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

logical_or_expression
	: logical_and_expression
	| logical_or_expression OR_OP logical_and_expression
	;


constant_expression
	: conditional_expression
	;

conditional_expression
	: logical_or_expression
	| logical_or_expression '?' expression ':' conditional_expression
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

assignment_expression
	: conditional_expression
	| unary_expression assignment_operator assignment_expression
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER 
	;

enumerator
	: IDENTIFIER
	| IDENTIFIER '=' constant_expression
	;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' constant_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' constant_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

pointer
	: '*'
	| '*' type_qualifier_list
	| '*' pointer
	| '*' type_qualifier_list pointer
	;

declarator
	: pointer direct_declarator
	| direct_declarator
	;

direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' constant_expression ']'
	| direct_declarator '[' ']'
	| direct_declarator '(' parameter_type_list ')'
	| direct_declarator '(' identifier_list ')'
	| direct_declarator '(' ')'
	;

%%

int main(int argc,char *argv[]){
	adele();
	logout= fopen("log.txt","w");
	tokenout= fopen("token.txt","w");
	parseLog = fopen("parseLog", "w");
	//assembly= fopen("assembly.asm", "W");
	assembly.open("assembly.asm");
	fclose(tokenout);
	fclose(logout);
	yyparse();
	fclose(parseLog);
	printf ("\nTotal line Count: %d\n", line_count);
	parser_table.dump();
	
	return 0;
}
