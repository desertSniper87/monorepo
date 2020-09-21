##CSE-310
The source code for CSE-310 lab(Compiler Sessional) that I flunked in 3rd year.

#How to run
###1. Symbol table creation
```bash
g++ main.cpp
chmod +x a.out
./a.out
```
###2. Lexical Analysis
```bash
#!/bin/bash

lex -o lex.yy.cpp lex.l
g++ -c  main.cpp lex.yy.cpp 
g++ -o lex.out lex.yy.o -lfl
chmod u+x lex.out
./lex.out < input1.txt
```
###3. Syntax and semantic analysis
Note: Use a 32 bit Linux environment
```bash
#!/usr/bin/bash

bison -d -y -o y.tab.cpp parser.y --debug --verbose
g++ -fpermissive -w -c -o parser.o y.tab.cpp
flex -o lex.yy.cpp lex.l
g++ -c -w -o lexer.o lex.yy.cpp
g++ -c -w -o main.o Symbol_table.cpp
g++ -o compiler lexer.o parser.o main.o -L /usr/lib -lfl -ly
./compiler < sample_input_1.c

```
###4. Code Generation
Same as before. Install emu8086 using wine and feed asm file.
