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
JetSpritePtr    word         ; pointer to player0 sprite lookup table
JetColorPtr     word         ; pointer to player0 color lookup table
BomberSpritePtr word         ; pointer to player1 sprite lookup table
BomberColorPtr  word         ; pointer to player1 color lookup table
JetAnimOffset   byte         ; palyer0 sprite configura animacao 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
JET_HEIGHT = 9               ; player0 sprite height (# rows in lookup table)
BOMBER_HEIGHT = 9            ; player1 sprite height (# rows in lookup table)

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
    lda #10
    sta JetYPos              ; JetYPos = 10
    lda #0
    sta JetXPos              ; JetXPos = 60
    lda #83
    sta BomberYPos           ; BomberYPos = 83
    lda #54
    sta BomberXPos           ; BomberXPos = 54

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
;; calcula e executa a tarefa dentro do pre-Vnlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda JetXPos
    ldy #0
    jsr SetObjectXPos           ; jump (goto)

    lda BomberXPos
    ldy #1
    jsr SetObjectXPos

    sta WSYNC
    sta HMOVE                  
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
    REPEAT 37
        sta WSYNC            ; display the 37 recommended lines of VBLANK
    REPEND
    sta VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the 192 visible scanlines of our main game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameVisibleLine:
    lda #$84
    sta COLUBK               ; set background/river color to blue
    lda #$C2
    sta COLUPF               ; set playfield/grass color to green
    lda #%00000001
    sta CTRLPF               ; enable playfield reflection
    lda #$F0
    sta PF0                  ; setting PF0 bit pattern
    lda #$FC
    sta PF1                  ; setting PF1 bit pattern
    lda #0
    sta PF2                  ; setting PF2 bit pattern

    ldx #96                 ; X counts the number of remaining scanlines
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
    lda #96
    sta BomberYPos

EndPositionUpdate:   ; fallback for positopn to random number



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
;; Declare ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
