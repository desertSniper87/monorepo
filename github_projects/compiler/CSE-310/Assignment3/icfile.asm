.MODEL SMALL

.STACK 100H

.data
x dw 0
y dw 0
q dw 0
c dw 0
temp0 dw 0
temp1 dw 0
temp2 dw 0
temp3 dw 0
temp4 dw 0
temp5 dw 0
temp6 dw 0
temp7 dw 0
temp8 dw 0
temp9 dw 0
temp10 dw 0
temp11 dw 0
temp12 dw 0
temp13 dw 0
temp14 dw 0
temp15 dw 0
temp16 dw 0

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
MOV temp0, ax

MOV ax, temp0
mov x, ax



mov ax, x
neg ax
MOV x, ax

MOV ax, temp1
mov x, ax

MOV ax, 7
mov y, ax



;Adding
MOV ax, 3
MOV bx, 2
ADD ax, bx
MOV temp2, ax



;Adding
MOV ax, temp2
MOV bx, 5
ADD ax, bx
MOV temp3, ax



;Adding
MOV ax, temp3
MOV bx, 6
ADD ax, bx
MOV temp4, ax



;SUBTRACTING
MOV ax, temp4
MOV bx, 2
SUB ax, bx
MOV temp5, ax




;MULTIPLYING
MOV ax, 1
MOV bx, 5
MUL bx
MOV temp6, ax


;SUBTRACTING
MOV ax, temp5
MOV bx, temp6
SUB ax, bx
MOV temp7, ax

MOV ax, temp7
mov x, ax



;BITWISE AND
MOV ax, 0
MOV bx, 4
AND ax, bx
MOV temp8, ax



;MULTIPLYING
MOV ax, temp8
MOV bx, 4
MUL bx
MOV temp9, ax




;REMINDER
MOV ax, 1
MOV bx, 5
MOV dx, 0
DIV bx
MOV temp10, dx


;Adding
MOV ax, temp9
MOV bx, temp10
ADD ax, bx
MOV temp11, ax

MOV ax, temp11
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
MOV temp13, ax

MOV ax, temp13
mov x, ax
jmp label3
label1:


;Adding
MOV ax, x
MOV bx, 5
ADD ax, bx
MOV temp12, ax

MOV ax, temp12
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
MOV temp14, ax

MOV ax, temp14
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
