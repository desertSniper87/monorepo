
; You may customize this and other start-up templates; 
; The location of this template is c:\emu8086\inc\0_com_template.txt

.MODEL SMALL
.CODE

org 100h 

MAIN PROC
    
    ;COUT<< '?';
    
    MOV AH , 2
    MOV DL , '?'
    INT 21H
    
    ;INT I; CIN>> I;
    
    MOV AH, 1
    INT 21H
    MOV BL , AL     
    
    ;COUT>> '\N';
    
    MOV AH , 2
    MOV DL , 0DH
    INT 21H
    MOV AH , 2
    MOV DL , 0AH
    INT 21H
    
    ;COUT<< I;
    
    MOV AH , 2
    MOV DL , BL
    INT 21H

    
; add your code here

ret




