b -> ( id RELOP num )
b -> ( id RELOP num )
b -> ( id RELOP num )
 b -> [ b and b ]
b -> { b or b }
s -> id = num;
s -> id = num;
s -> if ( b ) s else s
p -> s

if x<200 goto level0
goto level1
level1:

if x>100 goto level2
goto level3
level2:

if x!=150 goto level4
goto level5


level0:
level4:
x = 123

goto level6
level3:
level5:
x = 150

level6:
END.
