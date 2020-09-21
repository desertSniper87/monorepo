yacc -d upstart.y
echo 1;
g++ -c -o upstart.o y.tab.c
echo 2;
lex -o startup.c startup.l
echo 3;
g++ -c -o startup.o startup.c
echo 4;
g++ -o 1 startup.o upstart.o -ll -ly
echo 5;
#./1 in.txt out.txt out.asm
# %~!@#$%^&*()
./1 myin.txt
