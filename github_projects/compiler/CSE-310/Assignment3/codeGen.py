
#keys = "LETTER DIGIT UNDERSCORE NUM RELOP ADDOP MULOP ASSIGNOP BRACKET1 BRACKET2 PAREN1 PAREN2 DOTDOT COMMA SEMICOLON COLON DOT".split()	#input().split()
#keys = input().split()
keyCode = open("KeyWordCode", "w")
#print (keys)

#for key in keys:
#	temp = '''{%s}\t{
#	\t\tst.insert("%s",yytext);
#	\t\tyylval=(YYSTYPE)st.uplook(yytext);
#	\t\treturn %s;
#	\t}\n\n'''%(key,key,key)
##	print (temp)
#	keyCode.write(temp)	

while True:
	temp = input().split()
	if(len(temp)==0):	break
	keyCode.write(temp[0]+' '+temp[1].lower()+'\n')
#inp = '"PROGRAM"|"THEN"|"REAL"|"IF"|"DO"|"VAR"|"NOT"|"WHILE"|"OF"|"END"|"FUNCTION"|"ARRAY"|"BEGIN"|"PROCEDURE"|"WRITE"|"ELSE"|"INTEGER"'
#keys=inp.replace('"','').split('|')

#for key in keys:
#	temp = '%s "%s"\n'%(key,key)
#	print(temp)
#	keyCode.write(temp)
keyCode.close()
