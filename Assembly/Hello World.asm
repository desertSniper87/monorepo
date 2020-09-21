.MODEL SMALL

.DATA
MSG DB 'Hello World!$'

.CODE 

MAIN PROC
    
    MOV AX , DATA
    LEA DX , MSG
    
    MOV AH , 9
    INT 21H
    
    MAIN ENDP

            
    
    