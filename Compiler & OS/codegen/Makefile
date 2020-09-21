YACC=bison
LEX=flex

all: compiler

.c.o:
	g++ -c $<

compiler: grammar.o lex.o
	g++ -o compiler grammar.o lex.o

grammar.c grammar.h: grammar.y
	$(YACC) -dt grammar.y
	mv -f grammar.tab.c grammar.c
	mv -f grammar.tab.h grammar.h

lexer.c: lexer.l
	$(LEX) -t lex.l >lex.c

lexer.o: grammar.h

clean:
	rm -f compiler *.o
	rm -f lex.c grammar.c grammar.h
