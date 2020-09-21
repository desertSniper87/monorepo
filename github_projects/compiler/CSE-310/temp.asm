.MODEL SMALL

.STACK 100H

.data
x dw 0
y dw 0
q dw 0
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

MAIN PROC
MOV AX,@DATA
MOV DS,AX
MOV ax, 4
mov x, ax



;Adding
MOV ax, 123
MOV bx, 432
ADD ax, bx
MOV t0, ax

MOV ax, t0
mov x, ax



mov ax, x
neg ax
MOV x, ax

MOV ax, t1
mov x, ax

MOV ax, 7
mov y, ax



;Adding
MOV ax, 3
MOV bx, 2
ADD ax, bx
MOV t2, ax



;Adding
MOV ax, t2
MOV bx, 5
ADD ax, bx
MOV t3, ax



;Adding
MOV ax, t3
MOV bx, 6
ADD ax, bx
MOV t4, ax



;SUBTRACTING
MOV ax, t4
MOV bx, 2
SUB ax, bx
MOV t5, ax




;MULTIPLYING
MOV ax, 1
MOV bx, 5
MUL bx
MOV t6, ax


;SUBTRACTING
MOV ax, t5
MOV bx, t6
SUB ax, bx
MOV t7, ax

MOV ax, t7
mov x, ax



;BITWISE AND
MOV ax, 0
MOV bx, 4
AND ax, bx
MOV t8, ax



;MULTIPLYING
MOV ax, t8
MOV bx, 4
MUL bx
MOV t9, ax




;REMINDER
MOV ax, 1
MOV bx, 5
MOV dx, 0
DIV bx
MOV t10, dx


;Adding
MOV ax, t9
MOV bx, t10
ADD ax, bx
MOV t11, ax

MOV ax, t11
mov y, ax



;Less than relation operator
MOV ax, x
MOV bx, 3
CMP ax, bx
jg label1
label2:


;Adding
MOV ax, x
MOV bx, 4
ADD ax, bx
MOV t13, ax

MOV ax, t13
mov x, ax
jmp label3
label1:


;Adding
MOV ax, x
MOV bx, 5
ADD ax, bx
MOV t12, ax

MOV ax, t12
mov x, ax
label3:


;PRINTING
PUSH x
CALL PRINT

label8: 


;Less than relation operator
MOV ax, x
MOV bx, y
CMP ax, bx
jl label6
label7:

jmp label9:
label6:
MOV ax, x
mov c, ax



;Adding
MOV ax, x
MOV bx, 1
ADD ax, bx
MOV t14, ax

MOV ax, t14
mov x, ax

jmp label8
label9:

	
main endp


;PRINT FUNC
print proc  
	mov bp, sp
	mov ax, [bp+2]
	cmp ax, 0
	je return_print
	
	mov dx, 0
	mov bx, 10
	div bx
	
	;recalling
	push dx
	push ax
	call print
	
	;printing
	pop dx
	add dl, '0'
	mov ah, 2h
	int 21h
	
	return_print:
		ret 2	
print endp
