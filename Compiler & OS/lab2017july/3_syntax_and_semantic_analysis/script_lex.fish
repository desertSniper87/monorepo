#!/usr/bin/fish 

lex -o lex.yy.cpp lex.l
g++ -c  Symbol_table.cpp lex.yy.cpp 
g++ -o lex.out lex.yy.o -lfl
chmod u+x lex.out
