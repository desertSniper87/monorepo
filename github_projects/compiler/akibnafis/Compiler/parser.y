%{
#include<iostream>
#include<stdio.h>
#include<cstdlib>
#include<string.h>
#include<cstring>
#include<cmath>
#include<fstream>
#include <vector>
#include "1405112symboltable.h"

#define YYSTYPE symbolInfo*
#define YYDEBUG 1

using namespace std;


int yyparse(void);
int yylex(void);
extern int line_count;
extern int error;
FILE *temp;
extern FILE *yyin;
extern FILE *logout;
extern FILE *tokenout;
FILE *code;
FILE *errorout;
FILE *fp;
symbolTable *sTable=new symbolTable(7);
scopeTable *table=sTable->curr;
scopeTable *parentTable;
string variable_type;
string t;
int labelCount=0;
int tempCount=0;
char *label;

symbolInfo *tempSymbolinfo;
symbolInfo *parameterList=new symbolInfo();
vector<string> variableList;

string typeMatch(symbolInfo *s1,symbolInfo *s2)
{
	if(s1->type==s2->type)
		return s1->type;
	else if(s1->type=="float" && s1->type=="int")
		return s1->type;
	else
		return "nomatch";
}

void yyerror(const char *s)
{
	fprintf(errorout,"Line no:%d %s\n",line_count,s);
}
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

%}

%token IF ELSE FOR WHILE DO BREAK INT CHAR FLOAT DOUBLE VOID RETURN SWITCH CASE DEFAULT CONTINUE 
CONST_INT CONST_FLOAT ADDOP MULOP INCOP DECOP
ASSIGNOP RELOP LOGICOP NOT LPAREN RPAREN LCURL RCURL LTHIRD RTHIRD COMMA SEMICOLON CONST_CHAR STRING ID MAIN PRINTLN 

%left '+' '-'
%left '*' '/'
%error-verbose
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%

start : program{

			fprintf(tokenout,"Line no : %d start : program\n",line_count);
			fprintf(logout,"Total Error : %d\n",error);
			fprintf(logout,"Total Line Count : %d\n",line_count);
			fprintf(code,"%s",$1->code.c_str());

	  }
	  ;

program : program unit
			{
				fprintf(tokenout,"Line no : %d program : program unit\n",line_count);
				$$=new symbolInfo($1);
				$$->code+=$2->code;
			}
		| unit
			{
				fprintf(tokenout,"Line no : %d program : unit\n",line_count);
				$$=new symbolInfo($1);
				
			}

		;
	
unit : var_declaration
		{
			fprintf(tokenout,"Line no : %d unit: var_declaration\n",line_count);
			$$=new symbolInfo($1);
		}
     | func_declaration
     	{
     		fprintf(tokenout,"Line no : %d unit :func_declaration\n",line_count);
     		$$=new symbolInfo($1);
     	}
     | func_definition
     	{
     		fprintf(tokenout,"Line no : %d unit :func_definition\n",line_count);
     		$$=new symbolInfo($1);
     	}
     ;     
func_declaration : type_specifier ID LPAREN parameter_list RPAREN SEMICOLON
					{
						parentTable=table->parent;
						parentTable->insert($2->name,"function");
						fprintf(tokenout,"Line no : %d func_declaration : type_specifier ID LPAREN parameter_list RPAREN SEMICOLON value %s\n",line_count,$2->name.c_str());
						variableList.push_back($2->name);
						sTable->PrintCurrentScopeTable(logout);
						sTable->exitScope();
						table=sTable->curr;
						delete $2;
						//delete $1;
						//delete $4;
					}
				 ;
		 
func_definition : type_specifier ID LPAREN parameter_list RPAREN compound_statement
					{
						if(sTable->lookup_syminfo($2->name)==NULL)
						{
							parentTable=table->parent;
							parentTable->insert($2->name,"function");
						}
						printf("Line no : %d func_definition : type_specifier ID LPAREN parameter_list RPAREN compound_statement %s\n ",line_count,$2->name.c_str());
						$$=new symbolInfo();
						$$->code+="PROC "+$2->name+"\n";
						if($2->name=="main")
						{

						}
						if($2->name!="main")
						{
							$$->code+="PUSH AX\n";
							$$->code+="PUSH BX\n";
							$$->code+="PUSH CX\n";
							$$->code+="PUSH DX\n";
						}
						$$->code+=$6->code;
						if($2->name!="main")
						{
							$$->code+="POP DX\n";
							$$->code+="POP CX\n";
							$$->code+="POP BX\n";
							$$->code+="POP AX\n";
						}
						$$->code+="END "+$2->name+"\n";
				  		fprintf(tokenout,"Line no : %d func_definition : type_specifier ID LPAREN parameter_list RPAREN compound_statement",line_count);
				  		sTable->PrintCurrentScopeTable(logout);
				  		sTable->exitScope();
						table=sTable->curr;
				  	}
				;
 		 
parameter_list  : parameter_list COMMA type_specifier ID 
					{
						symbolInfo *x=parameterList;
 						while(x->next)
 						{
 							x=x->next;
 						}
 						x->next=new symbolInfo($4->name,variable_type);
						table->insert($4->name,variable_type);
						fprintf(tokenout,"Line No : %d parameter_list  : parameter_list COMMA type_specifier ID\n",line_count);
						//delete $4;
					}

				| parameter_list COMMA type_specifier	
					{
						symbolInfo *x=parameterList;
 						while(x->next)
 						{
 							x=x->next;
 						}
 						x->next=new symbolInfo("type",variable_type);
						fprintf(tokenout,"Line No : %d parameter_list  : parameter_list COMMA type_specifier\n",line_count);
					}

 				| type_specifier ID 	
 					{
 						symbolInfo *x=parameterList;
 						while(x->next)
 						{
 							x=x->next;
 						}
 						x->next=new symbolInfo($2->name,variable_type);
 						sTable->enterScope();
 						table=sTable->curr;
						table->insert($2->name,variable_type);

 						fprintf(tokenout,"Line No : %d parameter_list  : type_specifier ID \n",line_count);
 						//delete $2;here it is needed to be used double paramter
 					}


 				| type_specifier 	
 					{
 						symbolInfo *x=parameterList;
 						while(x->next)
 						{
 							x=x->next;
 						}
 						x->next=new symbolInfo("type",variable_type);
 						sTable->enterScope();
 						table=sTable->curr;
 						fprintf(tokenout,"Line No : %d parameter_list  : type_specifier \n",line_count);
 					}

 				|
 					{
 						parameterList->next=NULL;
 						$$=new symbolInfo();
 						sTable->enterScope();
 						table=sTable->curr;
 						$$=new symbolInfo();
 						fprintf(tokenout,"Line No : %d parameter_list  : VOID \n",line_count);
 					}
 				;
 		
compound_statement : LCURL statements RCURL 									{
							fprintf(tokenout,"Line No : %d compound_statement : LCURL statements RCURL\n",line_count);
							$$=$2;
						}

 		    	   | LCURL RCURL 
 		    	   		{
 		    	   			fprintf(tokenout,"Line No : %d compound_statement : LCURL RCURL\n",line_count);
 		    	   			$$=new symbolInfo("compound_statement","dummy");
 		    	   		}

 		    	   ;
 		    
var_declaration : type_specifier declaration_list SEMICOLON
					{
						(tokenout,"Line No : %d var_declaration : type_specifier declaration_list\n",line_count);
					}
 		 		;
 		 
type_specifier	: INT 	
					{
						fprintf(tokenout,"Line No : %d type_specifier : INT  \n",line_count);
						variable_type="int";
					}

 				| FLOAT 
 					{
 						fprintf(tokenout,"Line No : %d type_specifier : FLOAT  found\n",line_count);
 						variable_type="float";
 					}

 				| VOID  
 					{
 						fprintf(tokenout,"Line No : %d type_specifier : VOID  found\n",line_count);
 						variable_type="VOID";
 					}

 				;
 		
declaration_list : declaration_list COMMA ID
					{
						fprintf(tokenout,"Line No : %d declaration_list : declaration_list COMMA ID\n value is %s \n",line_count,$3->name.c_str());
						$3=table->insert($3->name,variable_type,table->tableNo);
						variableList.push_back($3->address);
						table->print(logout);
						//delete($3);
					}

 		  		 | declaration_list COMMA ID LTHIRD CONST_INT RTHIRD
 		  		 	{
 		  		 		fprintf(tokenout,"Line No : %d declaration_list : declaration_list COMMA ID LTHIRD CONST_INT RTHIRD\nvalue is %s \n",line_count,$3->name.c_str());
 		  		 		$3->aSize=$5->iValue;
 		  		 		$3=table->insert($3->name,variable_type,table->tableNo,$3->iValue,$3->fValue,$3->aSize);
 		  		 		variableList.push_back($3->address);
 		  		 		table->print(logout);
 		  		 		//delete($5);
 		  		 		//delete($3);
 		  		 	}

 		  		 | ID 
 		  		 	{
 		  		 		fprintf(tokenout,"Line No :  %d declaration_list : ID \nvalue is %s \n",line_count,$1->name.c_str());
 		  		 		$1=table->insert($1->name,variable_type,table->tableNo);
 		  		 		variableList.push_back($1->address);
 		  		 		table->print(logout);
 		  		 		//delete($1);
 		  		 	}

 		         | ID LTHIRD CONST_INT RTHIRD 
 		         	{
 		         		fprintf(tokenout,"Line No:  %d declaration_list : ID LTHIRD CONST_INT RTHIRD \nvalue is %s \n",line_count,$1->name.c_str());
 		         		$1->aSize=$3->iValue;

 		  		 		printf("size is %d\n",$1->aSize);
 		         		$1=table->insert($1->name,variable_type,table->tableNo,$1->iValue,$1->fValue,$1->aSize);
 		         		variableList.push_back($1->address);
 		         		table->print(logout);
 		         		//delete($3);
 		         		//delete($1);
 		         	}
 		  		 ;
 		  
statements : statement 
				{
					fprintf(tokenout,"Line No:  %d statements : statement\n",line_count);
					$$=$1;
				}

	   	   | statements statement 
	   	   		{
	   	   			fprintf(tokenout,"Line No : %d statements : statements statement\n",line_count);
	   	   			$$=$1;
	   	   			$$->code=$1->code+$2->code;
	   	   			delete $2;
	   	   		}
	   	   ;
	   
statement : var_declaration 	
				{
					fprintf(tokenout,"Line No : %d statement : var_declaration \n",line_count);
					//add variable to global array;
					$$=$1;
				}

	  	  | expression_statement 	
	  	  		{
	  	  			fprintf(tokenout,"Line No : %d statement : expression_statement \n",line_count);
	  	  			$$=$1;
	  	  		}

	  	  | compound_statement 	
	  	  		{
	  	  			fprintf(tokenout,"Line No : %d statement : compound_statement \n",line_count);
	  	  			$$=$1;
	  	  		}

	   	  | FOR LPAREN expression_statement expression_statement expression RPAREN statement 	
	   	  		{
	   	  			fprintf(tokenout,"Line No : %d statement : FOR LPAREN expression_statement expression_statement expression RPAREN statement \n",line_count);
	   	  			$$=$3;
	   	  			char *labelFOR=newLabel();
	   	  			char *labelEND=newLabel();
	   	  			$$->code+=string(labelFOR)+":\n";
	   	  			$$->code+=$4->code;
	   	  			$$->code+="CMP "+$4->name+",0\n";
	   	  			$$->code+="JE "+string(labelEND)+"\n";
	   	  			$$->code+=$7->code;
	   	  			$$->code+=$5->code;
	   	  			$$->code+="JMP "+string(labelFOR)+"\n";
	   	  			$$->code+=string(labelEND)+":\n";
	   	  			$$->name="for";
	   	  		} 

	  	  | IF LPAREN expression RPAREN statement %prec LOWER_THAN_ELSE			  
	  	  		{
	  	  			fprintf(tokenout,"Line No : %d statement : IF LPAREN expression RPAREN statement LOWER_THAN_ELSE \n",line_count);
	  	  			$$=$3;
	  	  			label=newLabel();
	  	  			$$->code+="MOV AX, "+$3->name+"\n";
	  	  			$$->code+="CMP AX, 0\n";
	  	  			$$->code+="JE "+string(label)+"\n";
	  	  			$$->code+=$5->code;
	  	  			$$->code+=string(label)+":	\n";
	  	  			$$->name="IF";
	  	  		}

	  	  | IF LPAREN expression RPAREN statement ELSE statement 	
	  	  		{
	  	  			fprintf(tokenout,"Line No  : %d statement : IF LPAREN expression RPAREN statement ELSE statement fuck \n",line_count);
	  	  			$$=$3;
	  	  			char *labelIf = newLabel();
	  	  			char *labelElse = newLabel();
	  	  			$$->code+="MOV AX, "+$3->name+"\n";
	  	  			$$->code+="CMP AX, 0\n";
	  	  			$$->code+="JE "+string(labelElse)+ "\n";
	  	  			$$->code+=$5->code;
	  	  			$$->code+="JMP "+string(labelIf)+"\n";
	  	  			$$->code+=string(labelElse)+": \n";
	  	  			$$->code+=$7->code;
	  	  			$$->code+=string(labelIf)+": \n";
	  	  			$$->name="IF_ELSE";
	  	  		}

	  	  | WHILE LPAREN expression RPAREN statement 	
	  	  		{
	  	  			fprintf(tokenout,"Line No : %d statement : WHILE LPAREN expression RPAREN statement \n",line_count);
	  	  			$$=new symbolInfo();
	  	  			char *labelWHILE=newLabel();
	  	  			char *labelEND=newLabel();
	  	  			$$->code+=""+string(labelWHILE)+":\n";
	  	  			$$->code+=$3->code;
	  	  			$$->code+="CMP "+$3->name+",0\n";
	  	  			$$->code+="JE "+string(labelEND)+"\n";
	  	  			$$->code+=$5->code;
	  	  			$$->code+="JMP "+string(labelWHILE)+"\n";
	  	  			$$->code+=string(labelEND)+":\n";
	  	  		}

	  	  | PRINTLN LPAREN ID RPAREN SEMICOLON   	   {
	  	  			fprintf(tokenout,"Line No : %d statement : PRINTLN LPAREN ID RPAREN SEMICOLON statement \n",line_count);
	  	  			$$=new symbolInfo("println","nonterminal");
	  	  		}

	  	  | RETURN expression SEMICOLON 			  {
	  	  			fprintf(tokenout,"Line No : %d statement : RETURN expression SEMICOLON \n",line_count);
	  	  			$$=$1;
	  	  		}

	  	  ;
	  
expression_statement : SEMICOLON   
						{
							fprintf(tokenout,"Line no : %d expression_statement:SEMICOLON\n",line_count);
							$$=new symbolInfo(";","SEMICOLON");
							$$->code="";
						}		
					 | expression SEMICOLON 	   
					 	{
					 		fprintf(tokenout,"Line no : %d expression_statement:expression SEMICOLON\n",line_count);
					 		$$=new symbolInfo($1);
					 	}
					 ;
	  
variable : ID 	
			{
				$$=new symbolInfo($1);
				fprintf(tokenout,"Line no : %d variable : ID\n",line_count);
			}

	 	 | ID LTHIRD expression RTHIRD  
	 	 	{
	 	 		$$=new symbolInfo($1);
	 	 		$$->code=$3->code+"MOV BX, "+$3->name+"\nADD BX, BX\n";
	 	 		if($3->iValue<$$->aSize&&$3->iValue>=0)
	 	 			{
	 	 				$$->index=$3->iValue;
	 	 				$$->iValue=$$->iara[$3->iValue];
	 	 			}
	 	 		else
	 	 			yyerror("array index out of bound\n");
	 	 		fprintf(tokenout,"Line no : %d variable: ID LTHIRD expression RTHIRD \n",line_count);
	 	 		delete $3;
	 	 	}
	 	 ;
	 
expression : logic_expression	
				{
					$$=new symbolInfo($1);
					fprintf(tokenout,"Line no : %d expression : logic_expression\n",line_count);
				}

	   		| variable ASSIGNOP logic_expression 	
	   			{
	   				fprintf(tokenout,"Line no : %d expression : variable ASSIGNOP logic_expression \n",line_count);
	   				$$=$1;
	   				$$->code=$3->code+$1->code;
	   				$$->code+="MOV AX, "+$3->name+"\n";
	   				if($$->aSize==0)
	   				{
	   					$$->code+="MOV "+$1->name+",AX\n";
	   				}
	   				else
	   				{
	   					$$->code+="MOV "+$1->name+"[BX],AX\n";
	   				}
	   				delete $3;
	   			}
	   		;
			
logic_expression : rel_expression 	
					{
						$$=new symbolInfo($1);
						if($1->iValue!=0)
							$$->iValue=$1->iValue;
						else
							$$->iValue=0;
						fprintf(tokenout,"Line no : %d logic_expression : rel_expression \n",line_count);
					}

		 		 | rel_expression LOGICOP rel_expression 	
		 		 	{
		 		 		$$=new symbolInfo($1);
		 		 		$$->code+=$3->code;

		 		 		char *te=newTemp();
		 		 		char *fal=newLabel();
		 		 		char *tr=newLabel();
		 		 		if($2->name=="&&")
		 		 		{
		 		 			$$->code+="CMP "+$1->name+",1\n";
		 		 			$$->code+="JNE "+string(fal)+"\n";
		 		 			$$->code+="CMP "+$3->name+",1\n";
		 		 			$$->code+="JNE "+string(fal)+"\n";
		 		 			$$->code+="MOV "+string(te)+",1\n";
		 		 			$$->code+="JMP "+string(tr) +"\n";
		 		 			$$->code+=string(fal)+":\n";
		 		 			$$->code+="MOV "+string(te)+",0\n";
		 		 			$$->code+=string(tr)+":\n";
		 		 		}
		 		 		else
		 		 		{
		 		 			$$->code+="CMP "+$1->name+",1\n";
		 		 			$$->code+="JE "+string(tr) +"\n";
		 		 			$$->code+="CMP "+$3->name+",1\n";
		 		 			$$->code+="JE "+string(tr) +"\n";
		 		 			$$->code+="MOV "+string(te)+",0\n";
		 		 			$$->code+="JMP "+string(fal) +"\n";
		 		 			$$->code+=string(tr)+":\n";
		 		 			$$->code+="MOV "+string(te)+",1\n";
		 		 			$$->code+=string(fal)+":\n";

		 		 		}
		 		 		$$->name=string(te);
		 		 		fprintf(tokenout,"Line no : %d logic_expression : rel_expression LOGICOP rel_expression \n",line_count);
		 		 		delete $3;
		 		 	}

		 		 ;
			
rel_expression	: simple_expression 	
					{
						$$=new symbolInfo($1);
						fprintf(tokenout,"Line no : %d rel_expression	: simple_expression \n",line_count);
					}
				| simple_expression RELOP simple_expression	
					{
						$$=new symbolInfo($1);
						$$->code+=$3->code;
						$$->code="MOV AX, "+$1->name+"\n";
						$$->code+="CMP AX, "+$3->name+"\n";
						char *te=newTemp();
						char *label1=newLabel();
						char *label2=newLabel();

						if($2->name=="<")
		 		 		{
		 		 			$$->code+="JL "+string(label1)+"\n";
		 		 			if($1->iValue<$3->iValue)
		 		 				$$->iValue=1;
		 		 			else
		 		 				$$->iValue=0;
		 		 		}
		 		 		else if($2->name=="<=")
		 		 		{
		 		 			$$->code+="JLE "+string(label1)+"\n";
		 		 		}
		 		 		else if($2->name==">")
		 		 		{
		 		 			$$->code+="JG "+string(label1)+"\n";
		 		 		}
		 		 		else if($2->name==">=")
		 		 		{
		 		 			$$->code+="JGE "+string(label1)+"\n";
		 		 		}
		 		 		else 
		 		 		{
		 		 			$$->code+="JE "+string(label1)+"\n";
		 		 		}
		 		 		$$->code+="MOV "+string(te)+",0\n";
		 		 		$$->code+="JMP "+string(label2)+"\n";
		 		 		$$->code+=string(label1)+":\nMOV "+string(te)+", 1\n";
		 		 		$$->code+=string(label2)+":\n";
		 		 		$$->name=string(te);
		 		 		delete $3;
						fprintf(tokenout,"Line no : %d rel_expression	: simple_expression RELOP simple_expression \n",line_count);
					}

				;
				
simple_expression : term 	
					{

						$$=new symbolInfo($1);
						fprintf(tokenout,"Line no : %d simple_expression : term\n",line_count);
					}

		  		  | simple_expression ADDOP term  
		  		  	{
		  		  		$$=new symbolInfo($1);
		  		  		$$->code+=$3->code;
		  		  		char *te=newTemp();

		  		  		if($2->name=="+")
		  		  		{
		  		  			t=typeMatch($1,$3);
		  		  			if(t=="int")
		  		  			{
		  		  				$$->code+="MOV AX,"+$1->name+"\n";
		  		  				$$->code+="MOV BX,"+$3->name+"\n";
		  		  				$$->code+="ADD AX,BX\n";
		  		  				$$->code+="MOV "+string(te)+",AX\n";

		  		  			}
		  		  			else
		  		  			{
		  		  				yyerror("no floating point operation please\n");
		  		  			}
		  		  		}
		  		  		else 
		  		  		{
		  		  			t=typeMatch($1,$3);
		  		  			if(t=="int")
		  		  			{
		  		  				$$->code+="MOV AX,"+$1->name+"\n";
		  		  				$$->code+="MOV BX,"+$3->name+"\n";
		  		  				$$->code+="SUB AX,BX\n";
		  		  				$$->code+="MOV "+string(te)+",AX\n";

		  		  			}
		  		  			else
		  		  			{
		  		  				yyerror("no floating point operation please\n");
		  		  			}
		  		  		}
		  		  		$$->name=string(te);
		  		  		delete $3;	
		  		  		fprintf(tokenout,"Line no : %d simple_expression : simple_expression ADDOP term\n",line_count);
		  		  	}
		  		  ;
					
term :	unary_expression	
		{
			$$=new symbolInfo($1);

			fprintf(tokenout,"Line no : %d term :unary_expression\n",line_count);
		}
     |  term MULOP unary_expression 
     	{
     		$$=new symbolInfo($1);
     		$$->code+=$3->code;
     		$$->code+="MOV AX, "+$1->name+"\n";
     		$$->code+="MOV BX, "+$3->name+"\n";
     		char *t1=newTemp();
     		if($2->name=="*")
     		{
     			$$->code+="MUL BX\n";
     			$$->code+="MOV "+string(t1)+" AX\n";
     			//$$->code+="MOV "+string(t1)+"R, AX\n";
     			$$->iValue=$1->iValue*$3->iValue;
     		}
     		else if($2->name=="/")
     		{
     			$$->code+="DIV BX\n";
     			$$->code+="MOV "+string(t1)+", AL\n";
     			$$->iValue=$1->iValue/$3->iValue;
     		}
     		else
     		{
     			$$->iValue=$1->iValue%$3->iValue;
     			$$->code+="DIV BX\n";
     			$$->code+="MOV "+string(t1)+", AH\n";
     		}
     		$$->name=string(t1);
     		fprintf(tokenout,"Line no : %d term :term MULOP unary_expression\n",line_count);
     		delete $3;
     	}
     ;

unary_expression : ADDOP unary_expression  									{
						fprintf(tokenout,"Line no:%d unary_expression : ADDOP unary_expression \n",line_count);
						$$=new symbolInfo($2);
						if($1->name=="-")
						{
							char *te=newTemp();
							$$->code+="MOV AX, "+$2->name+"\n";
							$$->code+="NEG AX\n";
							$$->code+="MOV "+string(te)+ ", AX\n";
							$$->name=string(te);
						}
					}

		 		 | NOT unary_expression  
		 		 	{
		 		 		fprintf(tokenout,"Line no : %d unary_expression : NOT unary_expression \n",line_count);
		 		 		$$=new symbolInfo($2);
		 		 		char *te=newTemp();
		 		 		$$->code="MOV AX, "+$2->name+"\n";
		 		 		$$->code+="NOT AX\n";
		 		 		$$->code+="MOV "+string(te)+",AX\n";
		 		 		$$->name=string(te);
		 		 	}

		 		 | factor 
		 		 	{
		 		 		$$=new symbolInfo($1);
		 		 		fprintf(tokenout,"Line no : %d unary_expression : factor \n",line_count);
		 		 	}
		 		 ;
	
factor	: variable 
			{
				$$=new symbolInfo($1);
				if($$->aSize==0)
				{

				}
				else
				{
					char *te=newTemp();
					$$->code+="MOV AX, "+$1->name+"[BX]\n";
					$$->code+="MOV "+string(te)+",AX\n";
					$$->name=string(te);
				}				
				fprintf(tokenout,"Line no : %d factor	: variable \n",line_count);
			}

		| ID LPAREN argument_list RPAREN			
			{
				if(sTable->lookup_syminfo($1->name))
				{
					$$=new symbolInfo($1);

				}
				else
				{
					yyerror("function not declared or defined in this scope");
				}	
				fprintf(tokenout,"Line no : %d factor	: ID LPAREN argument_list RPAREN \n",line_count);
			}

		| LPAREN expression RPAREN	
			{
				$$=new symbolInfo($2);
				fprintf(tokenout,"Line no : %d factor	: LPAREN expression RPAREN \n",line_count);
			}

		| CONST_INT 	
			{
				$$=new symbolInfo($1);
				fprintf(tokenout,"Line no : %d factor	: CONST_INT \n",line_count);
			}

		| CONST_FLOAT	
			{
				$$=new symbolInfo($1);
				fprintf(tokenout,"Line no : %d factor	: CONST_FLOAT \n",line_count);
			}


		| variable INCOP 
			{
				$$=new symbolInfo($1);
				char *te=newTemp();
				if($$->aSize>0)
				{
					$$->code+=$1->code;
					$$->code+="ADD BX,2\n";
					$$->code+="MOV "+string(te)+","+$1->name+"[BX]\n";
				}
				else
					$$->code+="INC "+$1->name+"\n";
				$$->name=string(te);
				fprintf(tokenout,"Line no : %d factor	: variable INCOP \n",line_count);
			}

		| variable DECOP 
			{
				$$=new symbolInfo($1);
				char *te=newTemp();
				if($$->aSize>0)
				{
					$$->code+=$1->code;
					$$->code+="SUB BX,2\n";
					$$->code+="MOV "+string(te)+","+$1->name+"[BX]\n";
				}
				else
					$$->code+="DEC "+$1->name+"\n";
				$$->name=string(te);
				fprintf(tokenout,"Line no : %d factor	: variable DECOP \n",line_count);
			}

	;	
argument_list : arguments

				{
					$$=new symbolInfo($1);
					fprintf(tokenout,"Line no : %d argument_list : arguments\n",line_count);
				}
			  | {
			  		$$=NULL;
			  		fprintf(tokenout,"Line no : %d argument_list : null\n",line_count);
			    }
arguments : arguments COMMA logic_expression
 			{
 				symbolInfo* t=$1;
				while(t->next)
				{t=t->next;}
				t->next=$3;
				$$=new symbolInfo($1);
 			}
          |	logic_expression 
            {
              	$$=new symbolInfo($1);
              	fprintf(tokenout,"Line no : %d logic_expression \n",line_count);
            }
%%
int main(int argc,char *argv[])
{

	if((fp=fopen(argv[1],"r"))==NULL)
	{
		printf("Cannot Open Input File.\n");
		exit(1);
	}

	logout= fopen("log.txt","w");
	temp=fopen("temp.txt","w");
	errorout=fopen("error.txt","w");
	fclose(logout);
	tokenout= fopen("parser.txt","w");
	fclose(tokenout);
	
	logout= fopen("log.txt","a");
	tokenout= fopen("parser.txt","a");
	code = fopen("code.asm","w");


	yyin=fp;
	yyparse();
	

	fclose(temp);
	fclose(errorout);
	fclose(logout);
	fclose(tokenout);
	
	return 0;
}