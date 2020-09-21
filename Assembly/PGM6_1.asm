.MODEL SMALL
.STACK 100H
.CODE
MAIN PROC
    
    MOV DL , 0
    MOV AX , 256
    MOV AH , 2  
    
    PRINT_LOOP:
    
    INC DL 
    INT 21H
    DEC AX
    JNZ PRINT_LOOP
    
    MOV AH , 4CH
    INT 21H
    
    MAIN ENDP
END MAIN
    
    
    
    