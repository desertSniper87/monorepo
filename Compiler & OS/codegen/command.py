#!/bin/python

import os

print("Analyzing grammer.y...")
os.system("bison -d -y -o y.tab.c++ grammar.y")
print("Creating parser object from y.tab.c...")
os.system("g++ -fpermissive -w -c -o parser.o y.tab.c++")
print("Lexing lex.l...")
os.system("flex -o lex.yy.cpp lex.l")
print("Compiling lex.yy.cpp...")
os.system("g++ -c -w -o lexer.o lex.yy.cpp") 
print("Compiling SymbolTable.cpp...")
os.system("g++ -c -w -o SymbolTable.o SymbolTable.cpp") 
#os.system("g++ -o lex.out lex.yy.o SymbolTable.o -lfl")
#os.system("chmod u+x lex.out")
#os.system("./lex.out < in.c")
print("Creating compiler")
# os.system("g++ -o compiler lexer.o parser.o SymbolTable.o -L /usr/lib -lfl -ly")
os.system("g++ -o compiler lexer.o parser.o SymbolTable.o -lfl -ly")
print("Inputting file...")
os.system("./compiler < in3.c")
print("Done... Removing files")
os.system("rm compiler lex.yy.cpp parser.o SymbolTable.o ")


