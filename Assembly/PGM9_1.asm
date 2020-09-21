.MODEL SMALL
.STACK 100H
.CODE

OUTDEC PROC
    PUSH AX
    PUSH BX
    PUSH CX
    PUSH DX
    
    ;input
    
    OR AX , AX
    JGE @END_IF1
    
    ;then
    
    PUSH AX
    MOV DL , '-'
    MOV AH , 2
    INT 21H
    POP AX
    NEG AX
    
@END_IF1:

    XOR CX , CX
    MOV BX , 10H
    
@REPEAT1:
    
    XOR DX , DX
    DIV BX
    PUSH DX
    INC CX
    
    OR AX , AX
    JNE @REPEAT1
    
    MOV AH , 2
    
@PRINT_LOOP:
    
    POP DX
    OR DL , 30H
    INT 21H
    LOOP @PRINT_LOOP
    
    POP AX
    POP BX
    POP CX
    POP DX
    
ENDP OUTDEC

  
    
    
    
    