    processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Include required files with VCS register memory mapping and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "vcs.h"
    include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from memory address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80

JetXPos         byte         ; player0 x-position
JetYPos         byte         ; player0 y-position
BomberXPos      byte         ; player1 x-position
BomberYPos      byte         ; player1 y-position
Score           byte         ; 2-digit socre stored as BCD
Timer           byte         ; 2-digit timer stores as BCD
Temp            byte         ; auxiliar variable to store tempor score
OnesDigitOffset word         ; lookup table offset for the score Ones digit
TensDigitOffset word         ; lookup table offset for the score Tens digit
JetSpritePtr    word         ; pointer to player0 sprite lookup table
JetColorPtr     word         ; pointer to player0 color lookup table
BomberSpritePtr word         ; pointer to player1 sprite lookup table
BomberColorPtr  word         ; pointer to player1 color lookup table
JetAnimOffset   byte         ; palyer0 sprite configura animacao 
Random          byte         ; Random gera um numero aleatorio para posiçao do inimigo
ScoreSprite     byte         ; store the sprite bit pattern fot the score
TimerSprite     byte         ; store the sprite bit patter for the Timer
TerrainColor    byte         ; store the color of the terrain
RiverColor      byte         ; store the color of the river

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9             ; player1 sprite height (# rows in lookup table)
DIGITS_HEIGHT = 5             ; scoreboard digiti height  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code at memory address $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg Code
    org $F000

Reset:
    CLEAN_START              ; call macro to reset memory and registers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #68
    sta JetXPos              ; JetXPos = 68
    lda #10
    sta JetYPos              ; JetYPos = 10
    lda #62
    sta BomberXPos           ; BomberXPos = 62
    lda #83
    sta BomberYPos           ; BomberYPos = 83
    lda #%11010100
    sta Random               ; Random = $D4
    lda #4
    sta Score                ; Score = 0
    lda #8
    sta Timer                ; Timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #<JetSprite
    sta JetSpritePtr         ; lo-byte pointer for jet sprite lookup table
    lda #>JetSprite
    sta JetSpritePtr+1       ; hi-byte pointer for jet sprite lookup table

    lda #<JetColor
    sta JetColorPtr          ; lo-byte pointer for jet color lookup table
    lda #>JetColor
    sta JetColorPtr+1        ; hi-byte pointer for jet color lookup table

    lda #<BomberSprite
    sta BomberSpritePtr      ; lo-byte pointer for enemy sprite lookup table
    lda #>BomberSprite
    sta BomberSpritePtr+1    ; hi-byte pointer for enemy sprite lookup table

    lda #<BomberColor
    sta BomberColorPtr       ; lo-byte pointer for enemy color lookup table
    lda #>BomberColor
    sta BomberColorPtr+1     ; hi-byte pointer for enemy color lookup table

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start the main display loop and frame rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
    lda #2
    sta VBLANK               ; turn on VBLANK
    sta VSYNC                ; turn on VSYNC
    REPEAT 3
        sta WSYNC            ; display 3 recommended lines of VSYNC
    REPEND
    lda #0
    sta VSYNC                ; turn off VSYNC
    REPEAT 31
        sta WSYNC            ; display the 37 recommended lines of VBLANK
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calcula e executa a tarefa dentro do VBlank
;; importante: o tempo que vai levar (clock CPU) para realizas as intruçoes abaixo
;; deve ser considerado e descontado dentro do loop REPEAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos           ; jump (goto)

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos

    jsr CalculationDigitOffset  ; calcula o digital scoreboard tabela

    sta WSYNC
    sta HMOVE       

    lda #0
    sta VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines - 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0                ; limpa o TIA register antes de cada frame
    sta COLUBK            ; reset TIA register before displaying the score 
    sta PF0               ; reset TIA register before displaying the score    
    sta PF1               ; reset TIA register before displaying the score
    sta PF2               ; reset TIA register before displaying the score
    sta GRP0              ; reset TIA register before displaying the score
    sta GRP1              ; reset TIA register before displaying the score
    sta CTRLPF
    
    lda #$1E
    sta COLUPF            ; set the scoreboard playfield color with yellow
    ldx #DIGITS_HEIGHT    ; start X conuter with 5 (height of digits)
                          ; 5 pq cada numero do score é composto por 5 linhs de byte, 
    ;a tabela de sprites dos numeros, esta na sequencao de 00,11,22,33,etc
    ;entao para pegar o numero 3, multiplica 5x5= decima quinta linha
.ScoreDigitLoop:
    ldy TensDigitOffset   ; get the tens digit offset for the score
    lda Digits,Y          ; load the bit pattern from lookup table
    and #$F0              ; mask/remove the graphics for the one digit
    sta ScoreSprite       ; save the score tens digit pattern in the variable

    ldy OnesDigitOffset   ; get the ones digit offset for the score
    lda Digits,Y          ; load the bit pattern from lookup table
    and #$0F
    ora ScoreSprite       ; merge it with the saved tens digit sprite
    sta ScoreSprite       ; and save it
    sta WSYNC
    sta PF1               ; update the playfiedl to display the Score sprite

    ldy TensDigitOffset+1 ; get the left digit offset for the timer
    lda Digits,Y            
    and #$F0
    sta TimerSprite       ;save the timer digits in a variable

    ldy OnesDigitOffset+1 ;get the one digit offset for the timer
    lda Digits,Y 
    and #$0F
    ora TimerSprite
    sta TimerSprite

    jsr Sleep12Cyles      ; waste some cycles

    sta PF1               ; update the playfiesl for timer display

    ldy ScoreSprite       ; preload for the next scanline
    sta WSYNC

    sty PF1
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1   ; increment all digits for the next line of data

    jsr Sleep12Cyles        ; waste some cycles

    dex                     ; X--
    sta PF1
    bne .ScoreDigitLoop     ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC               ; padding do Score
    sta WSYNC
    sta WSYNC
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 96 visible scanlines of our main game (because 2-line kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda TerrainColor
    sta COLUPF               ; set playfield/grass color 

    lda RiverColor
    sta COLUBK               ; set background/river color 
    
    lda #%00000001
    sta CTRLPF               ; enable playfield reflection
    lda #$F0
    sta PF0                  ; setting PF0 bit pattern
    lda #$FC
    sta PF1                  ; setting PF1 bit pattern
    lda #0
    sta PF2                  ; setting PF2 bit pattern

    ldx #85                 ; X counts the number of remaining scanlines 
                            ; 96 total da area (usando 2 linhas kernel)
                            ;84 (96 - linha do display 20 = 86)
.GameLineLoop:
.AreWeInsideJetSprite:       ; check if should render sprite player0
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set
    sbc JetYPos              ; subtract sprite Y coordinate
    cmp JET_HEIGHT           ; are we inside the sprite height bounds?
    bcc .DrawSpriteP0        ; if result < SpriteHeight, call subroutine
    lda #0                   ; else, set lookup index to 0
.DrawSpriteP0:
    clc                      ; clears carry flag before addition
    adc JetAnimOffset        ; jumps to correct sprite frame in memory
    tay                      ; load Y so we can work with pointer
    lda (JetSpritePtr),Y     ; load player bitmap slice of data
    sta WSYNC                ; wait for next scanline
    sta GRP0                 ; set graphics for player 0
    lda (JetColorPtr),Y      ; load player color from lookup table
    sta COLUP0               ; set color for player 0 slice

.AreWeInsideBomberSprite:    ; check if should render sprite player1
    txa                      ; transfer X to A
    sec                      ; make sure carry flag is set
    sbc BomberYPos           ; subtract sprite Y coordinate
    cmp BOMBER_HEIGHT        ; are we inside the sprite height bounds?
    bcc .DrawSpriteP1        ; if result < SpriteHeight, call subroutine
    lda #0                   ; else, set index to 0
.DrawSpriteP1:
    tay
    lda #%0000101
    sta NUSIZ1               ; stretch player1 sprite
    lda (BomberSpritePtr),Y  ; load player bitmap slice of data
    sta WSYNC                ; wait for next scanline
    sta GRP1                 ; set graphics for player 0
    lda (BomberColorPtr),Y   ; load player color from lookup table
    sta COLUP1               ; set color for player 0 slice

    dex                      ; X--
    bne .GameLineLoop        ; repeat next main game scanline while X != 0

    lda #0
    sta JetAnimOffset          ; reset jet animation
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK again
    REPEAT 30
        sta WSYNC            ; display 30 recommended lines of VBlank Overscan
    REPEND
    lda #0
    sta VBLANK               ; turn off VBLANK


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processa entrada jpoustick do player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Joystick checamos posicao (cima,baixo,esq e dir) uma por uma se foram pressionada

CheckP0Up:
    lda #%00010000              ; player0 joystick pra cima
    bit SWCHA                   ; compara os bits do registrador SWACH com 00001000
    bne CheckP0Down             ; se bit pattern nao for igual, vai para o proximo teste
    inc JetYPos
    lda #0                    ; 0
    sta JetAnimOffset           ; reset srpite animation offset para primeiro frame

CheckP0Down:
    lda #%00100000              ;player joystick baixo
    bit SWCHA
    bne CheckP0Left
    dec JetYPos
    lda #0                    ; 0
    sta JetAnimOffset           ; reset srpite animation offset para primeiro frame

CheckP0Left:
    lda #%01000000              ;player joystick left
    bit SWCHA
    bne CheckP0Right
    dec JetXPos
    lda JET_HEIGHT              ; 9
    sta JetAnimOffset           ; set animation offset para segundo frame

CheckP0Right:
    lda #%10000000              ;player joystick direita
    bit SWCHA
    bne EndInputCheck
    inc JetXPos
    lda JET_HEIGHT              ; 9
    sta JetAnimOffset           ; set animation offset para segundo frame

EndInputCheck:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atualizaçao da posicao do inimigo plaeyr1 proximo frame da tela
;; corrigir a quando o bomber desce para a tela e demora para voltar ao topo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateBomberPosition:
    lda BomberYPos   ; guarda a posicao atual do YPOS, para depois comparar
    clc                 ; clear acumulador
    cmp #0            ; compara de A(Ypos) = 0
    bmi .ResetBomberPosition    ; bmi(compara valor e negatio) se for < 0 entao reset position para o topo
    dec BomberYPos              ; se nao é zzero decrementa a posiçao
    jmp EndPositionUpdate
.ResetBomberPosition
    jsr GetRandomBomberPos      ; chama a subrotina para gerar uma posiçao alealtoria do inimigo (bomber)

EndPositionUpdate:   ; fallback for positopn to random number

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cheque por colisao de objeto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000  ; CXPPMM bit 7 detecta colisao entre p0 p1
    bit CXPPMM      ; cheque se o bit 7 do register CXPPMM e igual register A
    bne .CollisionP0P1  ;if collision P0 and P1 happened, game over
    jmp CheckCollisionP0PF      ; se nao, pula para o proximo cheque
.CollisionP0P1:
    jsr GameOver        ; call gameover subroutine

CheckCollisionP0PF:
    lda #%10000000      ;CXP0PF bit 7 detectado, P0 e PF colindiram
    bit CXP0FB 
    bne .CollisionP0PF      ; if conllision P0 e PF
    jmp EndCollisionCheck   ; se nao pula para o final cheque        

.CollisionP0PF:
    jsr GameOver

EndCollisionCheck:      ; fallback
    sta CXCLR           ; clear register colission

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop back to start a brand new frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame           ; continue to display the next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subrotina para configurar posicao horizontal de um objeto com ajuste fino
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A é a posiçao coordenada x do alvo, em pixel de nosso objeto
;; Y e o tipo de objeto (0: player 0, 1:player1, 2:missel0, 3:missel1, 4:bola)

SetObjectXPos subroutine
    sta WSYNC           ; inicia um nova scanline
    sec                 ; ativa set carry para realizar subtração
.Div15Loop
    sbc #15             ; subtrai 15 do acumulador A
    bcs .Div15Loop      ; fique no loop ate o carr flag estiver limpo,
    eor #7              ; pega o resto da divisao e descobre qual o ajsute fino -8 a 7
    asl                 ; instruçoes ASL move bit para esquerda, 
    asl                 ; para transformar o byte em 4 bit
    asl                 ; 
    asl                  
    sta HMP0,y 
    sta RESP0,y
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GameOver Subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
    lda #$30        
    sta TerrainColor            ; set terrain collor to red
    sta RiverColor              ; set river color to red
    lda #0
    sta Score                   ; score = 0
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subrotina usando LFSR Linear feedback Shift Register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gerer LFSR aleatorio numero e 
;; Divide o numero por 4, para limitar o tamanho do resultado e ficar dentro da
;; campo do jogo (rio), depois adicionar + 30 pos pra direita para compensar
;; a parte da grama da esquerda do jogo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random
    lsr             ; divide o valor por 4 usando bitshift (movendo para direita)
    lsr
    sta BomberXPos   ; save o numero aleatorioa na var bomberxpos
    lda #30
    adc BomberXPos  ; adc 30 posiscao + BomberXPOS para compensar(pular) a grama
    sta BomberXPos
    lda #96
    sta BomberYPos  ; configura o Y-Posicao 

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scorteboard digits to be displayd on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert the high and low nibbles of the variable score and timer
;; into offset of digits lookup tables so the value can be displayed
;; each digit has a height of 5 bytes in the lookup table
;;
;; for the low nibble we need multiple by 5
;;  - we can use left shift to perfome multiplication by 2
;;  - for any number N, the value of N*5 = (n*2*2)+N each operation shift left multiplica 2
;;   
;; for the upper nibble, since its already times 16, we need to divide it
;; and then multiply by 5:
;;   - we can use right shifts to perfom division by 2
;;   - for any number N, the value of (n/16)*5 = (n/2/2)+(n/2/2/2/2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculationDigitOffset subroutine
    ldx #1          ; x register is the loop counter
.PrepareScoreLoop   ; this will loop twice, first X=1, and then X=0

    lda Score,X             ; load A with timer (x=1) or Score (X=0)
    and #$0F                ;remove de tens digits by maksinh 4 bits 00001111 (usa 4 ultimos bits)
    sta Temp                ; save the value of A int temp
    asl                     ; shift left (it is now N*2)
    asl                     ; shift left (it is now N*4)
    adc Temp                ; add the value saved in temp (+N)
    sta OnesDigitOffset,X   ; save A in OneDigitOffset+1 or OneDigitOffSet

    lda Score,X     ; load a with Timer (X=1) or Socre (X=0)
    and #$F0        ; remove the one digit by masking 4 bits 11110000
    lsr             ; shift right (it is now N/2)
    lsr             ; shift right (it is now N/4)
    sta Temp        ; save the value of A into Temp
    lsr             ; shift right (it is now N/8)
    lsr             ; shift right (it is now N/16)
    adc Temp        ; add value saved in Temp (N/16 + N/4)
    sta TensDigitOffset,X  ; store A in TensDigitOffset+1 or TensDigitOffset

    dex             ; X--
    bpl .PrepareScoreLoop ; while x >= ), loop to pass a secondd time
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cyles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #



JetSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

JetSpriteTurn:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #

BomberSprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###

JetColor:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$BA
    .byte #$0E
    .byte #$08

JetColorTurn:
    .byte #$00
    .byte #$FE
    .byte #$0C
    .byte #$0E
    .byte #$0E
    .byte #$04
    .byte #$0E
    .byte #$0E
    .byte #$08

BomberColor:
    .byte #$00
    .byte #$32
    .byte #$32
    .byte #$0E
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40
    .byte #$40

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size with exactly 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC                ; move to position $FFFC
    word Reset               ; write 2 bytes with the program reset address
    word Reset               ; write 2 bytes with the interruption vector
