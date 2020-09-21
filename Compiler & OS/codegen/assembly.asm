.MODEL SMALL

.STACK 100H

 dw 0
 dw 0

.CODE

MAIN PROC
MOV AX,@DATA
MOV DS,AX
.MODEL SMALL

.STACK 100H

	
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
