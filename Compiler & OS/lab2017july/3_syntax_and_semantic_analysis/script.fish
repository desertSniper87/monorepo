#!/usr/bin/fish 

echo "Analyzing grammer.y..."
bison -d -y -o y.tab.cpp parser.y --debug --verbose
#bison -d -y -o y.tab.cpp parser_temp.y --debug --verbose
echo "Creating parser object from y.tab.cpp..."
g++ -fpermissive -w -c -o parser.o y.tab.cpp
echo "Lexing lex.l..."
flex -o lex.yy.cpp lex.l
echo "Compiling lex.yy.cpp..."
g++ -c -w -o lexer.o lex.yy.cpp
echo "Compiling Symbol_table.cpp..."
g++ -c -w -o main.o Symbol_table.cpp
echo "Creating compiler"
g++ -o compiler lexer.o parser.o main.o -L /usr/lib -lfl -ly
echo "Inputting file..."
./compiler < sample_input_1.c
#echo "Done... Removing files"
#rm compiler lex.yy.cpp parser.o main.o 
