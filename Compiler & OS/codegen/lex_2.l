%{
#include <stdio.h>
#include "SymbolTable.h"

//extern "C"
//{
 //   extern int yylex(void);
 //   extern int yyparse(void);
//}

int line_count = 1;
SymbolTable table;

void insert(const char* token, char* yytext) {
	////printf ("%s: %s ", token, yytext);
	//printf ("%s ", token);
	SymbolInfo symbolInfo(yytext, token);
	table.insert(symbolInfo, line_count);
}
%}

intsuffix							([uU][lL]?)|([lL][uU]?)
fracconst							([0-9]*\.[0-9]+)|([0-9]+\.)
exppart								[eE][-+]?[0-9]+
floatsuffix							[fFlL]
chartext							([^'])|(\\.)
stringtext							([^"])|(\\.)

%%



"\n"								{ ++line_number; }
[\t\f\v\r ]+						{ /* Ignore whitespace. */ }
"/*"								{ skip_comment(); }
"{"									{ return '{'; }
"<%"								{ return '{'; }
"}"									{ return '}'; }
"%>"								{ return '}'; }
"["									{ return '['; }
"<:"								{ return '['; }
"]"									{ return ']'; }
":>"								{ return ']'; }
"("									{ return '('; }
")"									{ return ')'; }
";"									{ return ';'; }
":"									{ return ':'; }
"..."								{ return ELLIPSIS; }
"?"									{ return '?'; }
"::"								{ return COLONCOLON; }
"."									{ return '.'; }
".*"								{ return DOTSTAR; }
"+"									{ return '+'; }
"-"									{ return '-'; }
"*"									{ return '*'; }
"/"									{ return '/'; }
"%"									{ return '%'; }
"^"									{ return '^'; }
"xor"								{ return '^'; }
"&"									{ return '&'; }
"bitand"							{ return '&'; }
"|"									{ return '|'; }
"bitor"								{ return '|'; }
"~"									{ return '~'; }
"compl"								{ return '~'; }
"!"									{ return '!'; }
"not"								{ return '!'; }
"="									{ return '='; }
"<"									{ return '<'; }
">"									{ return '>'; }
"+="								{ return ADDEQ; }
"-="								{ return SUBEQ; }
"*="								{ return MULEQ; }
"/="								{ return DIVEQ; }
"%="								{ return MODEQ; }
"^="								{ return XOREQ; }
"xor_eq"							{ return XOREQ; }
"&="								{ return ANDEQ; }
"and_eq"							{ return ANDEQ; }
"|="								{ return OREQ; }
"or_eq"								{ return OREQ; }
"<<"								{ return SL; }
">>"								{ return SR; }
"<<="								{ return SLEQ; }
">>="								{ return SREQ; }
"=="								{ return EQ; }
"!="								{ return NOTEQ; }
"not_eq"							{ return NOTEQ; }
"<="								{ return LTEQ; }
">="								{ return GTEQ; }
"&&"								{ return ANDAND; }
"and"								{ return ANDAND; }
"||"								{ return OROR; }
"or"								{ return OROR; }
"++"								{ return PLUSPLUS; }
"--"								{ return MINUSMINUS; }
","									{ return ','; }
"->*"								{ return ARROWSTAR; }
"->"								{ return ARROW; }
"asm"								{ return ASM; }
"auto"								{ return AUTO; }
"bool"								{ return BOOL; }
"break"								{ return BREAK; }
"case"								{ return CASE; }
"catch"								{ return CATCH; }
"char"								{ return CHAR; }
"class"								{ return CLASS; }
"const"								{ return CONST; }
"const_cast"							{ return CONST_CAST; }
"continue"							{ return CONTINUE; }
"default"							{ return DEFAULT; }
"delete"							{ return DELETE; }
"do"								{ return DO; }
"double"							{ return DOUBLE; }
"dynamic_cast"							{ return DYNAMIC_CAST; }
"else"								{ return ELSE; }
"enum"								{ return ENUM; }
"explicit"							{ return EXPLICIT; }
"export"							{ return EXPORT; }
"extern"							{ return EXTERN; }
"false"								{ return FALSE; }
"float"								{ return FLOAT; }
"for"								{ return FOR; }
"friend"							{ return FRIEND; }
"goto"								{ return GOTO; }
"if"								{ return IF; }
"inline"							{ return INLINE; }
"int"								{ return INT; }
"long"								{ return LONG; }
"mutable"							{ return MUTABLE; }
"namespace"							{ return NAMESPACE; }
"new"								{ return NEW; }
"operator"							{ return OPERATOR; }
"private"							{ return PRIVATE; }
"protected"							{ return PROTECTED; }
"public"							{ return PUBLIC; }
"register"							{ return REGISTER; }
"reinterpret_cast"						{ return REINTERPRET_CAST; }
"return"							{ return RETURN; }
"short"								{ return SHORT; }
"signed"							{ return SIGNED; }
"sizeof"							{ return SIZEOF; }
"static"							{ return STATIC; }
"static_cast"							{ return STATIC_CAST; }
"struct"							{ return STRUCT; }
"switch"							{ return SWITCH; }
"template"							{ return TEMPLATE; }
"this"								{ return THIS; }
"throw"								{ return THROW; }
"true"								{ return TRUE; }
"try"								{ return TRY; }
"typedef"							{ return TYPEDEF; }
"typeid"							{ return TYPEID; }
"typename"							{ return TYPENAME; }
"union"								{ return UNION; }
"unsigned"							{ return UNSIGNED; }
"using"								{ return USING; }
"vrtual"							{ return VIRTUAL; }
"void"								{ return VOID; }
"volatile"							{ return VOLATILE; }
"wchar_t"							{ return WCHAR_T; }
"while"								{ return WHILE; }
[a-zA-Z_][a-zA-Z_0-9]*				{ return check_identifier(yytext); }
"0"[xX][0-9a-fA-F]+{intsuffix}?		{ return INTEGER; }
"0"[0-7]+{intsuffix}?				{ return INTEGER; }
[0-9]+{intsuffix}?					{ return INTEGER; }
{fracconst}{exppart}?{floatsuffix}?	{ return FLOATING; }
[0-9]+{exppart}{floatsuffix}?		{ return FLOATING; }
"'"{chartext}*"'"					{ return CHARACTER; }
"L'"{chartext}*"'"					{ return CHARACTER; }

"\""{stringtext}*"\""			    { return STRING; }
"L\""{stringtext}*"\""			    { return STRING; }

.					                { fprintf(stderr, "%d: unexpected character `%c'\n", line_number, yytext[0]); }

%%