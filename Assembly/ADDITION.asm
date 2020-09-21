.STACK 100H
.DATA 

NUM1 DB 0H
NUM2 DB 0H
MSG1 DB 'PLEASE ENTER NUMBER 1$'
MSG2 DB 'PLEASE ENTER NUMBER 2$'

.CODE 
MAIN PROC                       
    
    MOV AX , @DATA
    MOV DS , AX
    
    LEA AX , MSG1
    MOV AH , 9
    INT 21H
    
    MOV AH , 2
    INT 21H
    MOV NUM1 , DL  
    
    LEA AX , MSG2
    MOV AH , 9
    INT 21H      
                  
    MOV AH , 2
    INT 21H
    MOV NUM2 , DL  
                             
    MOV AL , NUM1
    ADD AL , NUM2
    MOV DL , AL
    
    MOV AH , 9 
    INT 21H         
    
    MAIN ENDP
END MAIN

    
    
    