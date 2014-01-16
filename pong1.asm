  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate  .rs 1  ; .rs 1 means reserve one byte of space
ballx      .rs 1  ; ball horizontal position
bally      .rs 1  ; ball vertical position
ballup     .rs 1  ; 1 = ball moving up
balldown   .rs 1  ; 1 = ball moving down
ballleft   .rs 1  ; 1 = ball moving left
ballright  .rs 1  ; 1 = ball moving right
ballspeedx .rs 1  ; ball horizontal speed per frame
ballspeedy .rs 1  ; ball vertical speed per frame
paddle1ytop   .rs 1  ; player 1 paddle top vertical position
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
score1     .rs 1  ; score 1s digit
score10		.rs 1 ; scores 10s digit
score100    .rs 1 ; score 100s digit


;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
  
RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $E0
LEFTWALL       = $04
  
;PADDLE1X       = $08  ; horizontal position for paddles, doesnt move
PADDLE1X       = $1F
PADDLE1XFRONT  = $1F  ; horizontal position for paddles, doesnt move
PADDLE2X       = $F0
PADDLESPEED	   = $06

;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; MACROS
;;;;;;;;;;;;;;;;;;

; resetPPU 
; affects A
; Resets the PPU and sets the address to XXYY
resetPPU .macro
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #\1
  STA $2006             ; write the high byte of $3F00 address
  LDA #\2
  STA $2006             ; write the low byte of $3F00 address
  .endm

; drawBackgroundString
; affects A, X
; Draws a byte string starting at \1 of \2 bytes onto the background starting at \3\4
drawBackgroundString .macro
  resetPPU \3, \4
  LDX #$00
  
drawLoop\@:
  LDA \1, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to P to LoadPalettesLoop if compare was Not Equal to zeroPU
  INX                   ; X = X + 1
  CPX \2              ; / copy X bytes
  BNE drawLoop\@  ; Branch

  .endm

  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


LoadPalettes:
  resetPPU $3F, $00
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                        ; if compare was equal to 32, keep going down


GraphicsInit:
  JSR ClearBackground
  JSR DrawScore
  JSR DrawTitle


;;:Set starting game state
  LDA #STATETITLE
  STA gamestate


              
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000

  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI
  
 

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  LDA gamestate
  CMP #STATEPLAYING
  BNE ScoreDone
  JSR DrawScore
ScoreDone:

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
    
  ;;;all graphics updates done by here, run game engine


  JSR ReadController1  ;;get the current button data for player 1
  
GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle    ;;game is displaying title screen
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver  ;;game is displaying ending screen
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying   ;;game is playing
GameEngineDone:  
  
  JSR UpdateSprites  ;;set ball/paddle sprites from positions

  RTI             ; return from interrupt
 
 
 
 
;;;;;;;;
 
EngineTitle:
  ;;if start button pressed
  LDA #%00010000
  AND buttons1
  BEQ GameEngineDone ; if it was 0, do nothing
  
  ;;  turn screen off
  LDA #%00000000  
  STA $2001
  
  ;;  load game screen
  JSR ClearBackground
  
  ;;  set starting paddle/ball position
  ;;;Set some initial ball stats
  LDA #$01
  STA balldown
  STA ballright
  LDA #$00
  STA ballup
  STA ballleft
  
  LDA #$50
  STA bally
  
  LDA #$80
  STA ballx
  
  LDA #$02
  STA ballspeedx
  STA ballspeedy
  
  LDA #$00
  STA score1
  
  LDA #100
  STA paddle1ytop
  
  ;;  go to Playing State
  LDA #STATEPLAYING
  STA gamestate
  ;;  turn screen on
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  LDA #%00010000
  AND buttons1
  BEQ GameEngineDone ; if it was 0, do nothing
  
  ;;  turn screen off
  LDA #%00000000  
  STA $2001
  
  ;;  load title screen
  JSR ClearBackground
  JSR DrawTitle
  ;;  go to Title State
  LDA #STATETITLE
  STA gamestate
  ;;  turn screen on 
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  
  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:

MoveBallRight:
  LDA ballright
  BEQ MoveBallRightDone   ;;if ballright=0, skip this section

  LDA ballx
  CLC
  ADC ballspeedx        ;;ballx position = ballx + ballspeedx
  STA ballx

  LDA ballx
  CMP #RIGHTWALL
  BCC MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
  LDA #$00
  STA ballright
  LDA #$01
  STA ballleft         ;;bounce, ball now moving left
  ;;in real game, give point to player 1, reset ball
MoveBallRightDone:


MoveBallLeft:
  LDA ballleft
  BEQ MoveBallLeftDone   ;;if ballleft=0, skip this section

  LDA ballx
  SEC
  SBC ballspeedx        ;;ballx position = ballx - ballspeedx
  STA ballx

  LDA ballx
  CMP #LEFTWALL
  BCS MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
  ;; you lose, transition to game over screen
  ;;  turn screen off
  LDA #%00000000  
  STA $2001
  
    ; gamestate to gameover
  LDA #STATEGAMEOVER
  STA gamestate
  
  ;; draw game over
  drawBackgroundString str_GameOver, #$09, $21, $AA
  
  ;;  turn screen on
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  
  ;; no more engine updates needed
  JMP GameEngineDone
MoveBallLeftDone:


MoveBallUp:
  LDA ballup
  BEQ MoveBallUpDone   ;;if ballup=0, skip this section

  LDA bally
  SEC
  SBC ballspeedy        ;;bally position = bally - ballspeedy
  STA bally

  LDA bally
  CMP #TOPWALL
  BCS MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
  LDA #$01
  STA balldown
  LDA #$00
  STA ballup         ;;bounce, ball now moving down
MoveBallUpDone:


MoveBallDown:
  LDA balldown
  BEQ MoveBallDownDone   ;;if ballup=0, skip this section

  LDA bally
  CLC
  ADC ballspeedy        ;;bally position = bally + ballspeedy
  STA bally

  LDA bally
  CMP #BOTTOMWALL
  BCC MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
  LDA #$00
  STA balldown
  LDA #$01
  STA ballup         ;;bounce, ball now moving down
MoveBallDownDone:

MovePaddleUp:
  ;;if up button pressed
  LDA #%00001000
  AND buttons1
  BEQ MovePaddleUpDone ; if it was 0, do nothing
  ;;  if paddle top > top wall
  ;;    move paddle top and bottom up
  LDA paddle1ytop
  SEC
  SBC #PADDLESPEED
  STA paddle1ytop  
  
  LDA paddle1ytop
MovePaddleUpDone:

MovePaddleDown:
  ;;if down button pressed
  LDA #%00000100
  AND buttons1
  BEQ MovePaddleDownDone ; if it was 0, do nothing
  ;;  if paddle bottom < bottom wall
  ;;    move paddle top and bottom down
  LDA paddle1ytop
  CLC
  ADC #PADDLESPEED        ;;bally position = bally + ballspeedy
  STA paddle1ytop
  
MovePaddleDownDone:
  
CheckPaddleCollision:
  ;;if ball x < paddle1x
  LDA ballx
  CMP #PADDLE1XFRONT
  BCS CheckPaddleCollisionDone      ;;if ball x > left wall, still on screen, skip next section

  ;;  if ball y < paddle y top - over the paddle
  LDA bally
  CMP paddle1ytop
  BCC CheckPaddleCollisionDone      
  
  ;;    if ball y > paddle y bottom
  CLC
  LDA paddle1ytop
  ADC #$20 ; bottom of the paddle
  CMP bally
  BCC CheckPaddleCollisionDone      ;;if ball y < bottom wall, still on screen, skip next section
  ;;      bounce, ball now moving right
  LDA #$01
  STA ballright
  LDA #$00
  STA ballleft         ;;bounce, ball now moving right
  
  ;; get a point for bouncing the ball
  JSR IncScore
  
CheckPaddleCollisionDone:

  JMP GameEngineDone
 
 
 
 
UpdateSprites:
  LDA bally  ;;update all ball sprite info
  STA $0200
  
  LDA #$75
  STA $0201
  
  LDA #$00
  STA $0202
  
  LDA ballx
  STA $0203
  
  ;;update paddle sprites
  LDA paddle1ytop
  STA $0204
  LDA #$61
  STA $0205
  LDA #$00
  STA $0206
  LDA #PADDLE1X
  STA $0207
  
  LDA paddle1ytop
  CLC
  ADC #$08
  STA $0208
  LDA #$50
  STA $0209
  LDA #%10000000
  STA $020A
  LDA #PADDLE1X
  STA $020B
  
  LDA paddle1ytop
  CLC
  ADC #$10
  STA $020C
  LDA #$50
  STA $020D
  LDA #%10000000
  STA $020E
  LDA #PADDLE1X
  STA $020F
  
  LDA paddle1ytop
  CLC
  ADC #$18
  STA $0210
  LDA #$61
  STA $0211
  LDA #%10000000
  STA $0212
  LDA #PADDLE1X
  STA $0213
  
  RTS
  

DrawScore:
  ;;draw score on screen using background tiles
  resetPPU $20, $42
  
  ; draw score offset from 0 tile (which is $00, so this is easy)
  LDA score100
  STA $2007
  LDA score10
  STA $2007
  LDA score1
  STA $2007

  RTS
 
DrawTitle:
  drawBackgroundString str_Welcome, #$0F, $21, $08
  drawBackgroundString str_PressStart, #$0B, $21, $AA ; call for now, might want to make start flash
  RTS
  
ClearBackground:
  resetPPU $20, $00
  LDY #$00
  
LoadBackgroundOuterLoop:
  LDX #$00				; start out at 0
LoadBackgroundLoop:
  LDA #$24
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$20            ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down
  INY
  CPY #$1D
  BNE LoadBackgroundOuterLoop
  
  RTS
 
ReadController1:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016
  LDX #$08
ReadController1Loop:
  LDA $4016
  LSR A            ; bit0 -> Carry
  ROL buttons1     ; bit0 <- Carry
  DEX
  BNE ReadController1Loop
  RTS
  
IncScore:
IncOnes:
  LDA score1     ; load the lowest digit of the number
  CLC 
  ADC #$01          ; add one
  STA score1
  CMP #$0A          ; check if it overflowed, now equals 10
  BNE IncDone       ; if there was no overflow, all done
IncTens:
  LDA #$00
  STA score1     ; wrap digit to 0
  LDA score10     ; load the next digit
  CLC 
  ADC #$01          ; add one, the carry from previous digit
  STA score10
  CMP #$0A          ; check if it overflowed, now equals 10
  BNE IncDone       ; if there was no overflow, all done
IncHundreds:
  LDA #$00
  STA score10     ; wrap digit to 0
  LDA score100 ; load the next digit
  CLC 
  ADC #$01          ; add one, the carry from previous digit
  STA score100
IncDone:
  RTS

        
;;;;;;;;;;;;;;  
  
  
  
  .bank 1
  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

;strings
str_Welcome:
  .db $20, $0E, $15, $0C, $18, $16, $0E, $24 ; "Welcome "
  .db $1D, $18, $24; "to "
  .db $19, $18, $17, $10; "pong"
  
str_PressStart:
  .db $19, $1B, $0E, $1C, $1C, $24 ; "press "
  .db $1C, $1D, $0A, $1B, $1D ; "start"
  
str_GameOver:
  .db $10, $0A, $16, $0E, $24 ; "game "
  .db $18, $1F, $0E, $1B ; "over"  

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1