#!/usr/bin/env bash
lex -o lex.yy.cpp lex.l
g++ -c  SymbolTable.cpp lex.yy.cpp
g++ -o lex.out lex.yy.o SymbolTable.o -lfl
chmod u+x lex.out
./lex.out < in.c