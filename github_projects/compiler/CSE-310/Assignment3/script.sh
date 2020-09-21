#!/usr/bin/env bash
#bison -d -y parsea.y
#echo 1
#g++ -w -c -o parsea.o y.tab.c
#echo 2
#flex lexa.l
#echo 3
g++ -fpermissive -w -c -o test.o lex.yy.c
echo 4
g++ -o demoCompiler test.o parsea.o -lfl -ly
echo 5
./demoCompiler f.pas
