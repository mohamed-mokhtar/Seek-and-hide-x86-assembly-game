
PRINT_CHAR MACRO CHAR      
     MOV AH,2              
     MOV DL,CHAR           
     INT 21H               
PRINT_CHAR ENDM            

.MODEL SMALL
.STACK 64
.DATA

;------------------------------------------------|           
NAME_MSG DB 'ENTER YOUR NAMES:  $'
P1       DB 'P1: $'
P2       DB 'P2: $' 
NAME1    DB 15,?,15 DUP('$') 
NAME2    DB 15,?,15 DUP('$')                                                 
;------------------------------------------------|                                                    
;------------------------------------------------| 
MSG1 DB 'PRESS (P) TO PLAY $'     ;----|         |
MSG2 DB 'PRESS (C) TO CHAT $'     ;MENU|         |
MSG3 DB 'PRESS ESC TO EXIT $'     ;----|         |
;------------------------------------------------| 
BYE_MSG DB 'GOOD BYE XD $'  
P1_WIN_MSG DB 'Player One WIN  $'  
P2_WIN_MSG DB 'Player Two WIN  $'
;-------------------------------------------------    
COUNT DB ?  
SPACE DB 80 DUP(' '),'$'    ;FOR CLEAR
Y_POS DB ?
;-------------------------------------------------
GAMENOTES_P1 DB 'PLAYER_1 BLACK : HIDE - ','$'
GAMENOTES_P2 DB 'PLAYER_2 WHITE : SEEK - ','$'

;------------------------------------------------|
TIME DB 60          ; GAME DURATION  180 SEC !! |
OLDTIME DB ?         ; GAME TIME TEMP VAR        |
;------------------------------------------------|


;------------------------------------------------|          
P1_X DB ?            ; PLAYER_1 POS              |
P1_Y DB ?            ;                           |
;------------------------------------------------|
P2_X DB ?            ; PLAYER_2 POS              |
P2_Y DB ?            ;                           |
;------------------------------------------------|
                                               
; GAPS POSITION FOR ALLOWING MOVING OR NOT
             ;1       2        3         4        5         6        7        
GAPS DB 1,30,64, 7,47,73, 18,50,77, 4,27,64, 35,45,78, 2,12,62, 7,30,60

TEMP    DB  ? ; FOR MUTLIPLE USING LIKE AUXLURAY VARIABLE
COUNTER DB  0 ; FOR LOOPS USEING       

  ; y co-ordient  4    6    8   10   12  14
COINS_POSITION DB 60 , 70 , 40 , 7 , 32, 22 
SAVED_COINS_POS DB 60 , 70 , 40 , 7 , 32, 22 
 
                  ; 6 coins


WALL db 80 DUP(0B1H),'$'  ; WALL STRING VARIABLE  
                                     
.CODE
MAIN PROC FAR
MOV AX,@DATA        ; MOVING DATA SEGMENT INTO DS REGISTER
MOV DS,AX           ;
                                                       


MOV CH,32    ; hide the cursor `s symbol '_'
MOV AH,1
INT 10H
;---------------------- THE PLAYER NAMES !! ----------------------    

CALL CLEAR_SCREEN
        MOV AH,2
        MOV DX,0720H       ;MOV CURSUR    
        INT 10H
        MOV AH,9
        MOV DX,OFFSET NAME_MSG    ;DISPLAY MSG
        INT 21H  
        MOV AH,2
        MOV DX,0B20H
        INT 10H 
        MOV AH,9
        MOV DX,OFFSET P1    ;DISPLAY MSG PLAYER1
        INT 21H      
        
        MOV AH,0AH
        MOV DX,OFFSET NAME1
        INT 21H
        
        MOV AH,2
        MOV DX,0F20H
        INT 10H 
        MOV AH,9
        MOV DX,OFFSET P2    ;DISPLAY MSG PLAYER2
        INT 21H  
        
        MOV AH,0AH
        MOV DX,OFFSET NAME2
        INT 21H                                            

;---------------------- THE MNUE HERE !!!!! ----------------------     

MNUE:  

        MOV TIME,80 ;rest Time
        MOV AH,2
        MOV DX,0720H           
        INT 10H
        
        MOV AH,9
        MOV DX,OFFSET MSG1    ;DISPLAY MSG1
        INT 21H    
        
        MOV AH,2
        MOV DX,0B20H
        INT 10H 
        
        MOV AH,9
        MOV DX,OFFSET MSG2    ;DISPLAY MSG2
        INT 21H 
        
        MOV AH,2
        MOV DX,0F20H
        INT 10H
                  
        MOV AH,9
        MOV DX,OFFSET MSG3    ;DISPLAY MSG3
        INT 21H 
;-------------------- MSGES DISPLAYED ON THE SCREEN -------------- 
        MOV AH,2
        MOV DX,0000H          ;MOV CURSUR BACK TO 00
        INT 10H
              
C_A:    MOV AH,1 
        INT 16H
        JZ C_A                ;C_A = CHECK AGAIN  
        
        MOV AH,1
        INT 16H
        
        CMP AL,'P'            ;GO TO GAME IF P
        JE LETS_PLAY
        CMP AL,'p'
        JE LETS_PLAY
        CMP AL,'C'            ;EXIT NOW TILL WE MAKE CHAT
        JE EXIT
        CMP AL,'c'
        JE EXIT
        CMP AH,1              ;THE REAL EXIT
        JE EXIT 
        
        CALL CLEARBUFFER              
        
        JMP C_A
                  
;---------------------- THE END OF MNUE !!! ----------------------    

LETS_PLAY:   
CALL CLEAR_SCREEN

MOV DI,OFFSET SAVED_COINS_POS

MOV SI,OFFSET COINS_POSITION   

MOV BX,0
GET_COINS_POISITIONS_BACK:
MOV AL,DI[BX]
MOV SI[BX],AL
INC BX
CMP BX,6
JNZ GET_COINS_POISITIONS_BACK


MOV AH, 9
MOV DX, OFFSET GAMENOTES_P1
INT 21H 

MOV AH, 9
MOV DX, OFFSET NAME1+2
INT 21H  

MOV AH,2
MOV DX,0100H
INT 10H     

MOV AH, 9    
MOV DX, OFFSET GAMENOTES_P2
INT 21H  

MOV AH, 9
MOV DX, OFFSET NAME2+2
INT 21H

AND AX,0H
AND DX,0H
NEWGAME_ENTRY_POINT:                                                                    
        MOV BX,07H  ; COUNTER FOR THE DRAWING LOOP ' 7 TIMES '
        MOV AH,2    ; INT PARAMATER
        MOV DX,0300H; STARTING POSITION OF THE MAP
        MOV TEMP,DH ; USING TEMPLATE VAR TO KEEP DH 
                    ;BECAUSE @ WRITING STRING IT`S CHANGED

DRAW_MAP_LOOP:
        MOV AH,2    ; AH=2 FOR INT PARAMATER
        INT 10H     ; APPLYING MOVING THE CURSOR
         
        MOV AH, 9   ; AH=9 FOR STRING PRINT INT
        MOV DX, OFFSET WALL
        INT 21H     ; APPLYING THE STRING PRINT INT
  
        MOV DL,0H   ; MOVE X TO 0 
        MOV DH,TEMP ;
        ADD DH,02H  ; ADDING 2 TO Y
        MOV TEMP,DH ;
        DEC BX      ; DEC COUNTER
        CMP BX,0H   ; CHECKING
        JNZ DRAW_MAP_LOOP
        

;DRAWING GAPS    
        MOV DX,00H     ; CLEARING DX
        MOV BX,0       ; STARTING FROM [ 0 ]
        MOV DH,03      ; INITIAL Y POSITION FIRST WALL BRICK
        MOV CX,7       ; 7 ITERATIONS FOR 7 WALLS
        MOV AH,2       ; INTERUPT PARAMTER
     ALL_GAPS_LOOP:    ; O(7) OUTER LOOP HAS INNER LOOP O(3) 
        MOV COUNTER,0
        EACH_LINE_GAPS:
        
        MOV DL,GAPS[BX] ; MOVING FIRST GAP POSITION TO DL ' X ' CO-ORDINATE
        INT 10H              ; INTERUPT FOR APPLYING MOVING CURSOR
        MOV DL,' '           ; MOVING DL SPACE FOR THE GAP SHAPE
        INT 21H              ; PRINTNG ONE GAP
        MOV DL,GAPS[BX] ; TAKE AGAIN FIRST GAP`S POSITION
        INC DL               ; TO DRAW ANOTHER ONE NEXT TO IT 
        INT 10H              ; TO LOOK A SUITABLE PATH
        MOV  DL,' '          ; MOVING GAP SHAPE INTO DL
        INT  21H             ; PRINITNG ANOTHER SPACE
        INC  COUNTER         ; INC COUNTER FOR THE INNER LOOP
        INC  BX              ; INC THE GAPS ARRAY POISITIONS INDEX LOOPER BX
        CMP  COUNTER,3       ; INNER LOOP CONDITION
        JNZ  EACH_LINE_GAPS  ;
        ADD  DH,02H          ; ADDING 2 IN Y CO-ORDINATE TO JUMP INTO THE NEXT LINE
     LOOP ALL_GAPS_LOOP
        
        MOV BX,0        ; STARTING INDEX = 0 ,ARRAY LOOPER
        MOV CX,6        ; 6 LINES (OUTERLOOP) ,  3 ASTRISK (INEERLOOP) 
        MOV DX,0400H    ; STARTING POSITION
   
     ALL_COINS_LOOP:    ; PRINTING COINS LOOP
        MOV COUNTER,0   ; COUNTER FOR INNER LOOP
        EACH_LINE_COINS:
        MOV DL,COINS_POSITION[BX]
        INT 10H         ; MOVING CURSOR INTERUPT  
        MOV DL,'*'      ; * IS THE COIN ICON
        INT 21H         ; PRITING INTERPUT
        INC BX          ; ARRAY LOOPER INCERMENT
        INC COUNTER     ; INNER LOOP INCEREMNT
        CMP COUNTER,1   ; INNER LOOP CONDITION
        JNZ EACH_LINE_COINS
     ADD DH,2           ; ADDING 2 IN Y CO-ORDINATE TO JUMP INTO THE NEXT LINE
     LOOP ALL_COINS_LOOP
        
; STARTING DRAWING THE PLAYER        
AND AX,0H
AND BX,0H
AND CX,1H
AND DX,0H
; MAKING ALL REGISTERS 0 FOR NEW USAGE
MOV P1_X,3EH ;0AH ; 10 X
MOV P1_Y,02H ; 02 Y  INITIAL POISITON


MOV SI,OFFSET P1_X
MOV AH,2
MOV DX,[SI]
INT 10H    ; CHANGING CURSROS TO ITS CORESPONDING X AND Y OF PLAYER1 S POS
MOV AH,2   ; INTERUPT PARAMTER
MOV DL,01H ; SMILE NON-FILLED SHAPE :)   THE PLAYER WHOS IS GOING TO HIDE
INT 21H    ; DRAWING 


MOV P2_X,3EH ; 62 X    3EH
MOV P2_Y,10H ; 18 Y  INITIAL POISITON


MOV SI,OFFSET P2_X
MOV AH,2
MOV DX,[SI]
INT 10H     ; CHANGING CURSROS TO ITS CORESPONDING X AND Y OF PLAYER1 S POS
MOV AH,2    ; INTERUPT PARAMTER
MOV DL,02H  ; SMILE FILLED SHAPE :)   THE PLAYER WHOS IS GOING TO SEEK
INT 21H     ; DRAWING 



;;;;;;;;;;;;;;;Storing time to CMP with current time to know time changed or Not ?
mov ah,2ch
int 21h  
mov oldTime,dh ;Sec only
;___________           
GAMELOOP:   ;
AND AX,0H   ; 
AND BX,0H   ; CLEARING REGISETERS
AND CX,0H   ; 
AND DX,0H 
;time postion on the screen 
mov ah,2
mov dx,00078D
int 10h


;new time 
mov ah,2ch
int 21h   
cmp dh,oldTime ;CMP oldTime with CurrentTime
je noChange    ;Jump to Display Timer
dec Time       ;1 sec passed dec timer
cmp Time,0     ;Check time over
jle HideWin  ;time over Hiding player Win  
noChange:      ;Display Time
MOV al, Time 
mov ah,0  
aam 
or ax, 3030h
mov bx,ax
mov ah,2
mov dl,bh
int 21h  
mov dl,bl
int 21h  

mov ah,2ch
int 21h  
mov oldTime,dh  
;;;;;;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;Checking Seeking player win (the two players are in the same position)    
mov al,P1_X
mov ah,P2_X
mov bl,P1_Y
mov bh,P2_Y
Cmp al,ah  
jne checkinput 
Cmp bl,bh  
jne checkinput 
jmp SeekWin




checkinput:





AND AX,0H   ; 
AND BX,0H   ; CLEARING REGISETERS
AND CX,0H   ; 
AND DX,0H 



 
            ;________
MOV AH,1             ;  CHECKING INPUT
INT 16H              ;        W
                     ;
JZ GAMELOOP          ;     A  S  D
CMP AL,'D'           ;     PLAYER1  ; HIDING   
                     ;
                     ;        ^
JZ MOVE_P1_RIGHT     ;        |
CMP AL,'d'           ;     <- - ->
JZ MOVE_P1_RIGHT     ;     PLAYER2  ; SEEKING
CMP AL,'A'           ;
JZ MOVE_P1_LEFT      ;
CMP AL,'a'           ;
JZ MOVE_P1_LEFT      ;
CMP AL,'W'           ;
JZ MOVE_P1_UP        ;
CMP AL,'w'           ;
JZ MOVE_P1_UP        ;
CMP AL,'S'           ;
JZ MOVE_P1_DOWN      ;
CMP AL,'s'           ;
JZ MOVE_P1_DOWN      ;
                     ;
                     ;
                     ;
CMP AH,77            ; RIGHT ARROW
JZ MOVE_P2_RIGHT     ;
                     ;
CMP AH,75            ;
JZ MOVE_P2_LEFT      ; LEFT  ARROW
                     ;
CMP AH,72            ; UP    ARROW
JZ MOVE_P2_UP        ;
CMP AH,80            ;
JZ MOVE_P2_DOWN      ; DOWN  ARROW
                     ;
CMP AH,1             ;THE REAL EXIT
JE EXIT              ;
                     ;
CALL CLEARBUFFER     ; CLEARING BUFFER

JMP GAMELOOP
  
        
         
                                  
;MOVING LOGIC FOR THE TWO PLAYERS CODE .

MOVE_P1_RIGHT:
CALL CLEARBUFFER
CMP P1_X,79              ; THE LEFT OF THE SCREEN IS NOT ALLOWED
JZ GAMELOOP  
;CHECKING COIN EXISTANCE
JMP R1
R1: ; PLAYER 1 RIGHT
MOV AH,2
MOV SI,OFFSET P1_X
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,' '               ; SPACING THE OLD POSITION
INT 21H

INC P1_X

MOV DI,OFFSET COINS_POSITION

MOV CL,P1_Y

CMP CL,4
JZ CHECK_COIN_0_SUB
CMP CL,6
JZ CHECK_COIN_1_SUB
CMP CL,8
JZ CHECK_COIN_2_SUB
CMP CL,10
JZ CHECK_COIN_3_SUB
CMP CL,12
JZ CHECK_COIN_4_SUB
CMP CL,14
JZ CHECK_COIN_5_SUB


MOV AH,2
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,01H              ; DRAW THE PLAYER IN THE NEW LOCATION
INT 21H                 ; ASCI OF 01 IS A SMILE
JMP GAMELOOP    



MOVE_P1_LEFT:
CALL CLEARBUFFER
CMP P1_X,0            ; CANT MOVE LEFT IF YOU ARE IN THE RIGHT OF THE SCREEN
JZ GAMELOOP 
;CHECKING COIN EXISTANCE
JMP L1

L1:
MOV AH,2
MOV SI,OFFSET P1_X
MOV DX,[SI]           ; MOVING CURSOR
INT 10H

MOV AH,2
MOV DL,' '
INT 21H               ; FREE THE OLD LOCATION WITH ' ' SPACE

DEC P1_X              ; DECREMENT THE 'Y' OF PLAYER 1 DUE TO MOVING LEFT

MOV DI,OFFSET COINS_POSITION

MOV CL,P1_Y

CMP CL,4
JZ CHECK_COIN_0_SUB
CMP CL,6
JZ CHECK_COIN_1_SUB
CMP CL,8
JZ CHECK_COIN_2_SUB
CMP CL,10
JZ CHECK_COIN_3_SUB
CMP CL,12
JZ CHECK_COIN_4_SUB
CMP CL,14
JZ CHECK_COIN_5_SUB

CONT_AFTER_COINS_CHECKING:

MOV AH,2
MOV DX,[SI]           ; MOVING CURSOR TO THE NEW LOCATION
INT 10H

MOV AH,2
MOV DL,01H            ; DRAWING THE PLAYER IN THE NEW LOCATION
INT 21H
JMP GAMELOOP    


CHECK_COIN_0_SUB:
MOV CH,P1_X
CMP CH,[DI]
JZ  SUBTIME_0
JMP CONT_AFTER_COINS_CHECKING

CHECK_COIN_1_SUB:
MOV CH,P1_X
CMP CH,[DI+1]
JZ  SUBTIME_1
JMP CONT_AFTER_COINS_CHECKING

CHECK_COIN_2_SUB:
MOV CH,P1_X
CMP CH,[DI+2]               
JZ  SUBTIME_2
JMP CONT_AFTER_COINS_CHECKING

CHECK_COIN_3_SUB:
MOV CH,P1_X
CMP CH,[DI+3]               
JZ  SUBTIME_3
JMP CONT_AFTER_COINS_CHECKING

CHECK_COIN_4_SUB:
MOV CH,P1_X
CMP CH,[DI+4]               
JZ  SUBTIME_4
JMP CONT_AFTER_COINS_CHECKING

CHECK_COIN_5_SUB:
MOV CH,P1_X
CMP CH,[DI+5]               
JZ  SUBTIME_5
JMP CONT_AFTER_COINS_CHECKING


SUBTIME_0:
SUB TIME,3
MOV [DI],90
JMP CONT_AFTER_COINS_CHECKING

SUBTIME_1:
SUB TIME,3
MOV [DI+1],90
JMP CONT_AFTER_COINS_CHECKING

SUBTIME_2:
SUB TIME,3
MOV [DI+2],90
JMP CONT_AFTER_COINS_CHECKING

SUBTIME_3:
SUB TIME,3
MOV [DI+3],90
JMP CONT_AFTER_COINS_CHECKING

SUBTIME_4:
SUB TIME,3
MOV [DI+4],90
JMP CONT_AFTER_COINS_CHECKING

SUBTIME_5:
SUB TIME,3
MOV [DI+5],90
JMP CONT_AFTER_COINS_CHECKING


MOVE_P1_DOWN:
CALL CLEARBUFFER
CMP P1_Y,16 ; LAST POSITION IN THE MAP 10H
JZ GAMELOOP  
;-> Y OF P1 IN AL +1 THEN * 3 THEN TAKE IT IN BX BX=(P1_Y+1)*3
MOV AL,P1_Y ; KEEPING P1_Y INTO AL TO CHECK IF IT S A GAP OR NOT
SHR AL,1    ; DIVIDE AL OVER 2
SUB AL,1    ; SUB AL,1
MOV TEMP,AL ; MOVING AL INTO TEMP
ADD AL,TEMP ; MUL : AL=AL*3
ADD AL,TEMP ;       AL=AL+AL+AL

MOV AH,0    ; CLEAR THE HIGH BYTE 
MOV BX,AX   ; MOVING THE FIRST INDEX OF THE COORESPONDING 3 GAPS  
            ; IN THE BELOW LINE
 
MOV DL,GAPS[BX] ; TAKING THE VALUE OF THE GAP TO GET THE NEIGHBOUR GAP
MOV CL,GAPS[BX]
ADD CL,1        ; THE SIDE GAP OF EACH ELEMENT OF GAP ARRAY|__|
                ; WE KEEP IN THE ARRAY JUST ONE GAP POISITION
CMP P1_X,DL     ; THE NEXT ONE IS +1
JZ D1           ; D-1 = LABEL OF MOVING DOWN - PLAYER 1
CMP P1_X,CL 
JZ D1

MOV DL,GAPS[BX+1]
MOV CL,GAPS[BX+1]
ADD CL,1
CMP P1_X,DL 
JZ D1
CMP P1_X,CL 
JZ D1


MOV DL,GAPS[BX+2]
MOV CL,GAPS[BX+2]
ADD CL,1
CMP P1_X,DL 
JZ D1
CMP P1_X,CL 
JZ D1          

JMP GAMELOOP 

D1:
MOV AH,2
MOV SI,OFFSET P1_X
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,' '
INT 21H

ADD P1_Y,2H
MOV AH,2
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,01H
INT 21H

JMP GAMELOOP

;------------------ UP

MOVE_P1_UP:
CALL CLEARBUFFER
CMP P1_Y,2 ; HIGHEST TOP`S POSITION IN THE MAP 2H
JZ GAMELOOP  
;-> (P1_Y / 2) - 2 * 3 -> TIHS THE FIRST INDEX OF THE GAPS IN THE UPPER LINE
MOV AL,P1_Y ; KEEPING P1_Y INTO AL TO CHECK IF IT S A GAP OR NOT
SHR AL,1    ; DIVIDE AL OVER 2
SUB AL,2    ; SUB AL,2
MOV TEMP,AL ; MOVING AL INTO TEMP
ADD AL,TEMP ; MUL : AL=AL*3
ADD AL,TEMP ;       AL=AL+AL+AL

MOV AH,0    ; CLEAR THE HIGH BYTE 
MOV BX,AX   ; MOVING THE FIRST INDEX OF THE COORESPONDING 3 GAPS  
            ; IN THE BELOW LINE
 
MOV DL,GAPS[BX] ; TAKING THE VALUE OF THE GAP TO GET THE NEIGHBOUR GAP
MOV CL,GAPS[BX]
ADD CL,1        ; THE SIDE GAP OF EACH ELEMENT OF GAP ARRAY|__|
                ; WE KEEP IN THE ARRAY JUST ONE GAP POISITION
CMP P1_X,DL     ; THE NEXT ONE IS +1
JZ U1           ; D-1 = LABEL OF MOVING DOWN - PLAYER 1
CMP P1_X,CL 
JZ U1

MOV DL,GAPS[BX+1]
MOV CL,GAPS[BX+1]    ; SECOND GAP            
ADD CL,1                                     
CMP P1_X,DL                                  
JZ U1                                        
CMP P1_X,CL                                  
JZ U1                                        
                                             
                                             
MOV DL,GAPS[BX+2]                            
MOV CL,GAPS[BX+2]                            
ADD CL,1             ; THIRD GAP             
CMP P1_X,DL                                  
JZ U1                                        
CMP P1_X,CL                                  
JZ U1                                        
                                             
JMP GAMELOOP                                 
                                             
U1:                                          
MOV AH,2             ; MOVING APPROVED       
MOV SI,OFFSET P1_X
MOV DX,[SI]
INT 10H

MOV AH,2             ; FREE THE OLD PLACE WITH SPACE
MOV DL,' '
INT 21H

SUB P1_Y,2H
MOV AH,2              
MOV DX,[SI]          ; MOVING THE CURSOR
INT 10H

MOV AH,2
MOV DL,01H           ; DRAWING IN THE NEW LOCATION
INT 21H

JMP GAMELOOP



;------------------



MOVE_P2_RIGHT:
CALL CLEARBUFFER
CMP P2_X,79
JZ GAMELOOP  
;
JMP R2
R2: ; PLAYER 1 RIGHT
MOV AH,2
MOV SI,OFFSET P2_X          ; GETTING THE CURRENT LOCATION
MOV DX,[SI]
INT 10H

MOV AH,2                    ; FREE THE OLD LOCATION
MOV DL,' '
INT 21H

INC P2_X


MOV DI,OFFSET COINS_POSITION

MOV CL,P2_Y

CMP CL,4
JZ CHECK_COIN_0_ADD
CMP CL,6
JZ CHECK_COIN_1_ADD
CMP CL,8
JZ CHECK_COIN_2_ADD
CMP CL,10
JZ CHECK_COIN_3_ADD
CMP CL,12
JZ CHECK_COIN_4_ADD
CMP CL,14
JZ CHECK_COIN_5_ADD

CONT_AFTER_COINS_CHECKING_2:


MOV AH,2
MOV DX,[SI]                 ; MOVING THE CURSOR TO THE NEW LOCATION
INT 10H

MOV AH,2
MOV DL,02H
INT 21H                     ; MOVING INTO THE NEW LOCATION
JMP GAMELOOP    



CHECK_COIN_0_ADD:
MOV CH,P2_X
CMP CH,[DI]
JZ  ADDTIME_0
JMP CONT_AFTER_COINS_CHECKING_2

CHECK_COIN_1_ADD:
MOV CH,P2_X
CMP CH,[DI+1]
JZ  ADDTIME_1
JMP CONT_AFTER_COINS_CHECKING_2

CHECK_COIN_2_ADD:
MOV CH,P2_X
CMP CH,[DI+2]               
JZ  ADDTIME_2
JMP CONT_AFTER_COINS_CHECKING_2

CHECK_COIN_3_ADD:
MOV CH,P2_X
CMP CH,[DI+3]               
JZ  ADDTIME_3
JMP CONT_AFTER_COINS_CHECKING_2

CHECK_COIN_4_ADD:
MOV CH,P2_X
CMP CH,[DI+4]               
JZ  ADDTIME_4
JMP CONT_AFTER_COINS_CHECKING_2

CHECK_COIN_5_ADD:
MOV CH,P2_X
CMP CH,[DI+5]               
JZ  ADDTIME_5
JMP CONT_AFTER_COINS_CHECKING_2


ADDTIME_0:
ADD TIME,3
MOV [DI],90
JMP CONT_AFTER_COINS_CHECKING_2

ADDTIME_1:
ADD TIME,3
MOV [DI+1],90
JMP CONT_AFTER_COINS_CHECKING_2

ADDTIME_2:
ADD TIME,3
MOV [DI+2],90
JMP CONT_AFTER_COINS_CHECKING_2

ADDTIME_3:
ADD TIME,3
MOV [DI+3],90
JMP CONT_AFTER_COINS_CHECKING_2

ADDTIME_4:
ADD TIME,3
MOV [DI+4],90
JMP CONT_AFTER_COINS_CHECKING_2

ADDTIME_5:
ADD TIME,3
MOV [DI+5],90
JMP CONT_AFTER_COINS_CHECKING_2




MOVE_P2_LEFT:
CALL CLEARBUFFER            ; SAME ALGORITHM WITH THE PLAYER 1
CMP P2_X,0
JZ GAMELOOP 
;
JMP L2

L2:
MOV AH,2
MOV SI,OFFSET P2_X
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,' '
INT 21H

DEC P2_X 


MOV DI,OFFSET COINS_POSITION

MOV CL,P2_Y

CMP CL,4
JZ CHECK_COIN_0_ADD
CMP CL,6
JZ CHECK_COIN_1_ADD
CMP CL,8
JZ CHECK_COIN_2_ADD
CMP CL,10
JZ CHECK_COIN_3_ADD
CMP CL,12
JZ CHECK_COIN_4_ADD
CMP CL,14
JZ CHECK_COIN_5_ADD



MOV AH,2 
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,02H
INT 21H
JMP GAMELOOP    
;--------------P2 DOWN ------
MOVE_P2_DOWN:
CALL CLEARBUFFER
CMP P2_Y,16 ; LAST POSITION IN THE MAP 10H
JZ GAMELOOP  
;-> Y OF P2 IN AL +1 THEN * 3 THEN TAKE IT IN BX BX=(P2_Y+1)*3
MOV AL,P2_Y ; KEEPING P2_Y INTO AL TO CHECK IF IT S A GAP OR NOT
SHR AL,1    ; DIVIDE AL OVER 2
SUB AL,1    ; SUB AL,1
MOV TEMP,AL ; MOVING AL INTO TEMP
ADD AL,TEMP ; MUL : AL=AL*3
ADD AL,TEMP ;       AL=AL+AL+AL

MOV AH,0    ; CLEAR THE HIGH BYTE 
MOV BX,AX   ; MOVING THE FIRST INDEX OF THE COORESPONDING 3 GAPS  
            ; IN THE BELOW LINE
 
MOV DL,GAPS[BX] ; TAKING THE VALUE OF THE GAP TO GET THE NEIGHBOUR GAP
MOV CL,GAPS[BX]
ADD CL,1        ; THE SIDE GAP OF EACH ELEMENT OF GAP ARRAY|__|
                ; WE KEEP IN THE ARRAY JUST ONE GAP POISITION
CMP P2_X,DL     ; THE NEXT ONE IS +1
JZ D2           ; D-1 = LABEL OF MOVING DOWN - PLAYER 1
CMP P2_X,CL 
JZ D2

MOV DL,GAPS[BX+1]    ; SECOND GAP
MOV CL,GAPS[BX+1]
ADD CL,1
CMP P2_X,DL 
JZ D2
CMP P2_X,CL 
JZ D2


MOV DL,GAPS[BX+2]
MOV CL,GAPS[BX+2]    ; THIRD GAP
ADD CL,1
CMP P2_X,DL 
JZ D2
CMP P2_X,CL 
JZ D2          

JMP GAMELOOP 

D2:                  ; MOVING APPROVED
MOV AH,2
MOV SI,OFFSET P2_X
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,' '           ; REMOVE THE PRVIOUS PLACE WITH SPACE
INT 21H

ADD P2_Y,2H
MOV AH,2
MOV DX,[SI]
INT 10H

MOV AH,2
MOV DL,02H           ; DRAW THE PLAYER IN THE NEW PLACE
INT 21H

JMP GAMELOOP


;--------------P2 UP --------

MOVE_P2_UP:
CALL CLEARBUFFER
CMP P2_Y,2 ; HIGHEST TOP`S POSITION IN THE MAP 2H
JZ GAMELOOP  
;-> (P2_Y / 2) - 2 * 3 -> TIHS THE FIRST INDEX OF THE GAPS IN THE UPPER LINE
MOV AL,P2_Y ; KEEPING P2_Y INTO AL TO CHECK IF IT S A GAP OR NOT
SHR AL,1    ; DIVIDE AL OVER 2
SUB AL,2    ; SUB AL,2
MOV TEMP,AL ; MOVING AL INTO TEMP
ADD AL,TEMP ; MUL : AL=AL*3
ADD AL,TEMP ;       AL=AL+AL+AL

MOV AH,0    ; CLEAR THE HIGH BYTE 
MOV BX,AX   ; MOVING THE FIRST INDEX OF THE COORESPONDING 3 GAPS  
            ; IN THE BELOW LINE
 
MOV DL,GAPS[BX] ; TAKING THE VALUE OF THE GAP TO GET THE NEIGHBOUR GAP
MOV CL,GAPS[BX]
ADD CL,1        ; THE SIDE GAP OF EACH ELEMENT OF GAP ARRAY|__|
                ; WE KEEP IN THE ARRAY JUST ONE GAP POISITION
CMP P2_X,DL     ; THE NEXT ONE IS +1
JZ U2           ; D-1 = LABEL OF MOVING DOWN - PLAYER 1
CMP P2_X,CL 
JZ U2

MOV DL,GAPS[BX+1]      ; SECOND GAP
MOV CL,GAPS[BX+1]
ADD CL,1
CMP P2_X,DL 
JZ U2
CMP P2_X,CL 
JZ U2


MOV DL,GAPS[BX+2]      ; THIRD GAP
MOV CL,GAPS[BX+2]
ADD CL,1
CMP P2_X,DL 
JZ U2
CMP P2_X,CL 
JZ U2          

JMP GAMELOOP 

U2:                    ; UP IS APPROVED
MOV AH,2
MOV SI,OFFSET P2_X
MOV DX,[SI]            ;
INT 10H

MOV AH,2
MOV DL,' '             ; REMOVING THE OLD PLACE WITH SPACE
INT 21H

SUB P2_Y,2H
MOV AH,2
MOV DX,[SI]
INT 10H

MOV AH,2 
MOV DL,02H
INT 21H               ; DRAWING THE PLAYER IN THE NEW PLACE

JMP GAMELOOP

;----------------------------
JMP GAMELOOP   

HideWin: 
       CALL CLEAR_SCREEN
               
        MOV AH,2 
        MOV AL,0
        MOV DH,02h
        MOV DL,20H                      ;MOVING CURSUR TO 00 00
        INT 10H 
        
        MOV AH,9
        MOV DX,OFFSET P1_WIN_MSG    ;DISPLAY MSG1
        INT 21H    


        jmp  MNUE   
        
        
SeekWin: 
       CALL CLEAR_SCREEN
               
        MOV AH,2 
        MOV AL,0
        MOV DH,02h
        MOV DL,20H                      ;MOVING CURSUR TO 00 00
        INT 10H 
        
        MOV AH,9
        MOV DX,OFFSET P2_WIN_MSG    ;DISPLAY MSG1
        INT 21H   

        jmp  MNUE  





EXIT:    
        CALL CLEAR_SCREEN
        
        MOV AH,2
        MOV DX,0A20H
        INT 10H 
        
        MOV AH,9
        MOV DX,OFFSET BYE_MSG    ;DISPLAY MSG1
        INT 21H 
             
    HLT
    MAIN ENDP
; A PROCEDURE FOR DRAWING A BRICK OF WALL OF THE MAZE RUNNER
;       ITS ASCII CODE IS '0B1' HEXA TO BE LIKE A WALL BRICK 
DRAWBRICK    PROC
        
            MOV CX,80    
        MOV AH,2
        MOV DL,0B1h
        DRAWBRICKLOOP: 
        INT 21H    
        LOOP DRAWBRICKLOOP
        RET 
DRAWBRICK    ENDP

; A PROCEDURE FOR CLEARING THE BUFFER AFTER EACH INPUT 
CLEARBUFFER         PROC 
                    ;CLEARING BUFFER DATA FOR EACH INPUT
                    mov ah,0h
                    int 16h
                    RET
CLEARBUFFER         ENDP  

CLEAR_SCREEN        PROC  
                 
         
mov ah,9 ;Display
mov bh,0 ;Page 0
mov al,20h ;Letter D
mov cx,0ffffh ;5 times
mov bl,0fh ;Green (A) on white(F) background
int 10h             
              
        RET
CLEAR_SCREEN        ENDP    


END MAIN            
; WHAT IS FIXED
; 1. PLAYERS MOVING
; 2. MAP DRAWING
; 3. COINS DRAWING
;
;
;
; TODO LIST
; 1. CHECKING THE COINS COLLASION  *
; 2. CHECKING THE PLAYERS COLLASION.
; 3. MAKING THE TIMER IT HASNT BEEN WORKING WELL IN DOS BOX.
; 4. THE MENU , STATUS BAR   
; 5. First screen at each terminal. 
;    TAKING THE USER `S NAME AND ASK FOR PRESS ENTER 
;    .. IDK IS IT REQUIRED IN PHASE ONE OR NOT
; 6. THE WINING AND LOSING CONDITIONS. 
; 7. TEST
;
;-----------------------------------------------
; DOCTOR `S POST  
; For semester students:
; Phase 1 Grading Criteria:
; 2 Game graphics
; 1 Game animations
; 1 Main menu
; 2 Status bar and Score update
; 2 Code organization and comments    ******************
; 2 Everything working correctly (over all point of view)

