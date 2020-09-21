.MODEL SMALL

.STACK 100H

.data
x dw 0
y dw 0
z dw 0
a dw 0
b dw 0
c dw 0
t0 dw 0
t1 dw 0
t2 dw 0
t3 dw 0
t4 dw 0
t5 dw 0
t6 dw 0
t7 dw 0
t8 dw 0
t9 dw 0
t10 dw 0
t11 dw 0
t12 dw 0
t13 dw 0
t14 dw 0
t15 dw 0
t16 dw 0


.CODE

;function definition
PROC main


;Assignment Operation

;ADDING THINGS
MOV AX,2
MOV BX,3
ADD AX,BX
MOV t0,AX

;Logical Expression
MOV AX, 1
MOV BX, 2

;Multiplication
MUL BX
MOV t1, AX

MOV AX, t1
MOV BX, 3

;Modulo
POP DX
DIV BX
MOV t2, DX

MOV AX, t2
MOV  a[BX], AX

;Assignment Operation
MOV AX, 1
CMP AX, 5
JL L0
MOV t3, 0
JMP L1
L0:
MOV t3, 1
L1:
MOV AX, t3
MOV  b[BX], AX
;Logical Expression
MOV BX, 0
ADD BX, BX

;Assignment Operation
MOV AX, 2
MOV  c[BX], AX
MOV AX, a[BX]
MOV t4, AX
MOV AX, b[BX]
MOV t5, AX

;Doing Logical opeation between two relational expressions

;Doing AND operation
CMP t4, 1
JNE L2
CMP t5, 1
JNE L2
MOV t6 ,1
JMP L3 
L2: 
MOV t6 ,0
L3 :
;Logical Expression

;If-Else statement
MOV AX, L3
CMP AX, 0
JE L6
;Logical Expression
MOV BX, 0
ADD BX, BX

;Increment
INC c
;Logical Expression
JE L5
L6:
;Logical Expression
MOV BX, 1
ADD BX, BX

;Assignment Operation
;Logical Expression
MOV BX, 0
ADD BX, BX
MOV AX, c[BX]
MOV t8, AX
MOV AX, t8
MOV  c[BX], AX
L5:

;Print Variable a
;Print Variable b