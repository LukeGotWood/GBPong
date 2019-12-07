INCLUDE "hardware.inc"
INClUDE "util.asm"
INCLUDE "dma.asm"

INCLUDE "Paddle.asm"
INCLUDE "Ball.asm"
INCLUDE "Score.asm"

; -------- INTERRUPT VECTORS --------
; specific memory addresses are called when a hardware interrupt triggers


; Vertical-blank triggers each time the screen finishes drawing. Video-RAM
; (VRAM) is only available during VBLANK. So this is when updating OAM /
; sprites is executed.
SECTION "VBlank", ROM0[$0040]
    jp _HRAM    ; Jump to the start of DMA Routine

; LCDC interrupts are LCD-specific interrupts (not including vblank) such as
; interrupting when the gameboy draws a specific horizontal line on-screen
SECTION "LCDC", ROM0[$0048]
    reti

; Timer interrupt is triggered when the timer, rTIMA, ($FF05) overflows.
; rDIV, rTIMA, rTMA, rTAC all control the timer.
SECTION "Timer", ROM0[$0050]
    reti

; Serial interrupt occurs after the gameboy transfers a byte through the
; gameboy link cable.
SECTION "Serial", ROM0[$0058]
    reti

; Joypad interrupt occurs after a button has been pressed. Usually we don't
; enable this, and instead poll the joypad state each vblank
SECTION "Joypad", ROM0[$0060]
    reti

; -------- END INTERRUPT VECTORS --------

; -------- HEADER --------

SECTION "Header", ROM0[$0100]

EntryPoint:
    di          ; Disable interrupts
    jp Start    ; Jump to code start

; RGBASM will fix this later
rept $150 - $104
    db 0
endr

; -------- END HEADER --------

; -------- MAIN --------

SECTION "Game Code", ROM0

Start:
    ld SP, $FFFF        ; Set stack pointer to the top of HRAM

    ClearRAM            ; ClearRAM MACRO

    DMA_COPY            ; Copy the DMA Routine to HRAM

    ld a, IEF_VBLANK    ; Load VBlank mask into A
    ld [rIE], a         ; Set VBlank interrupt flag
    ei                  ; Enable interrupts

; -------- WaitVBlank --------

.waitVBlank
    ld a, [rLY]         ; Load LCDC Y-Coordinate into A
    cp a, SCRN_Y        ; rLY - SCRN_Y
    jr c, .waitVBlank   ; if rLY < SCRN_Y then jump to .waitVBlank

; -------- END WaitVBlank --------

; -------- Initial Configuration --------

    xor a           ; (ld A, 0)
    ld [rLCDC], a   ; Load A into LCDC register

    ClearScreen     ; ClearScreen MACRO

    CopyData _VRAM, PADDLE, PADDLEEND                       ; CopyData MACRO

    CopyData (_VRAM + (PADDLEEND - PADDLE)), BALL, BALLEND  ; CopyData MACRO

    CopyData $9010, SCORE, SCOREEND                         ; CopyData MACRO

    ld a, %00011011     ; Load A with colour pallet settings
    ld [rBGP], a        ; Load BG colour pallet with A

    ld a, %00011011     ; Load A with colour pallet settings
    ld [rOBP0], a       ; Load OBJ0 colour pallet with A

    xor a               ; (ld a, 0)
    ld [rSCY], a        ; Load BG scroll Y with A
    ld [rSCX], a        ; Load BG scroll X with A

    ld [rNR52], a       ; Load sound enable with A

    or LCDCF_ON         ; Set LCD enabled bit in A
    or LCDCF_BGON       ; Set BG enabled bit in A
    or LCDCF_OBJ8       ; Set OBJ height 8 bit in A
    or LCDCF_OBJON      ; Set OBJ on bit in A
    ld [rLCDC], a       ; Load LCDC with A (settings)

    ld a, $01           ; (ld a, 0)
    ld [$9824], a       ; Position left score on screen
    ld [$9830], a       ; Position right score on screen

    ; -------- Paddle_Left --------
    Spr_getY 0                      ; (ld hl, _RAM) HL points to sprite's Y
	ld	[hl], ((SCRN_Y / 2) + 8)	; Set Y to middle-screen
	inc	hl		                    ; HL points to sprite's X
	ld	[hl], 8	                    ; Set X to left-screen
	inc	hl		                    ; HL points to sprite's tile (from BG map)
	ld	[hl], $00	                ; Set Tile to the (R) graphic
	inc	hl		                    ; HL points to sprite's flags
	ld	[hl], 0		                ; Set all flags to 0. X,Y-flip, palette, etc.
    ; -------- END Paddle_Left --------

    ; -------- Paddle_Right --------
    Spr_getY 1                      ; (ld hl, _RAM + 4) HL points to sprite's Y
	ld	[hl], ((SCRN_Y / 2) + 8)	; Set Y to middle-screen
	inc	hl		                    ; HL points to sprite's X
	ld	[hl], SCRN_X                ; Set X to right-screen
	inc	hl		                    ; HL points to sprite's tile (from BG map)
	ld	[hl], $00	                ; Set Tile to the (R) graphic
	inc	hl		                    ; HL points to sprite's flags
	ld	[hl], OAMF_XFLIP            ; Set all flags to 0. X,Y-flip, palette, etc.

    xor a                           ; (ld a, 0)
    ld [_RAM + 160], a              ; Load A into Paddle direction

    ; -------- END Paddle_Right --------

    ; -------- Ball --------
    Spr_getY 2                      ; (ld hl, _RAM + 8) HL points to sprite's Y
	ld	[hl], ((SCRN_Y / 2) + 8)	; Set Y to middle-screen
	inc	hl		                    ; HL points to sprite's X
	ld	[hl], ((SCRN_X / 2))        ; Set X to right-screen
	inc	hl		                    ; HL points to sprite's tile (from BG map)
	ld	[hl], $01	                ; Set Tile to the (R) graphic
	inc	hl		                    ; HL points to sprite's flags
	ld	[hl], 0                     ; Set all flags to 0. X,Y-flip, palette, etc.

    ld a, $02                       ; Set initial direction of ball                 #TODO: Random start direction
    ld [_RAM + 161], a              ; Load A into ball direction

    ; Ball Direction Value
    ; $00 - East | North
    ; $01 - East | South
    ; $02 - West | North
    ; $03 - West | South

    ; -------- END Ball --------

; -------- END Initial Configuration --------

; -------- Main Loop --------

loop:
    halt            ; Halts CPU until interrupt triggers
    nop             ; No Operation

    ; -------- Paddle_Left Movement --------
    ld a, $20       ; Mask to pull bit 4 low (read the D pad)           #TODO: Replace hard coded value with EQU
    ld [_HW], a     ; Pull bit 4 low
    ld a, [_HW]     ; Read the value of the inputs
    ld a, [_HW]     ; Read again to avoid debounce

    cpl             ; (A = ~A)
    and $0F         ; Remove top 4 bits

    swap a          ; Move the lower 4 bits to the upper 4 bits
    ld b, a         ; Save the buttons states to b

    ld a, $10       ; Mask to pull bit 4 low (read the buttons pad)     #TODO: Replace hard coded value with EQU
    ld [_HW], a     ; Pull bit 4 low
    ld a, [_HW]     ; Read the value of the inputs
    ld a, [_HW]     ; Read again to avoid debounce

    cpl             ; (A = ~A)
    and $0F         ; Remove top 4 bits

    or b            ; Combine with the button states
    cp a, $0        ; Check if value is zero

    push af         ; Save the joypad state
    and PADF_UP     ; If up then set NZ flag

    jr z, .JOY_DOWN     ; If z flag then skip JOY_UP

    ; -------- JOY_UP --------
    Spr_getY 0      ; Spr_getY MACRO
    ld a, [hl]      ; Get current Y
    cp a, (16)      ; (Y - N)
    jr z, .JOY_DOWN ; If Y is N, ignore press

    dec a           ; Move Paddle_Left upwards
    ld [hl], a      ; Write new Y to sprite sheet

.JOY_DOWN
    pop af          ; Load the joypad state
    and PADF_DOWN   ; If Right then set NZ flag

    jr z, .JOY_END  ; If z flag then skip to JOY_END

    ; -------- JOY_DOWN --------
    Spr_getY 0          ; Spr_getY MACRO
    ld a, [hl]          ; Get current Y 
    cp a, (SCRN_Y + 8)  ; (Y - (SCRN_Y + N))
    jr z, .JOY_END      ; If Y is (SCRN_Y + N), ignore press

    inc a               ; Move Paddle_Left downwards
    ld [hl], a          ; Write new Y to sprite sheet

.JOY_END
    ; -------- END Paddle_Left Movement --------

    ; -------- Paddle_Right Movement --------
    Spr_getY 1          ; Spr_getY MACRO
    ld a, [hl]          ; Load Paddle_right's Y into A
    Spr_getY_P 2        ; Spr_getY MACRO
    cp [hl]             ; (Paddle_right's Y - Ball's Y)
    jr z, .MOVE_END     ; If Paddle_right and ball in same Y, continue loop
    jr c, .MOVE_DOWN    ; If ball below Paddle_right, move down

    ; -------- MOVE_UP --------
    Spr_getY 1          ; Spr_getY MACRO
    ld a, [hl]          ; Load Paddle_Right's Y into A
    dec a               ; Move Paddle_Right downwards
    ld [hl], a          ; Write new Y to sprite sheet

    jr .MOVE_END        ; Continue loop

.MOVE_DOWN
    ; -------- MOVE_DOWN --------
    Spr_getY 1          ; Spr_getY MACRO
    ld a, [hl]          ; Load Paddle_Right's Y into A
    inc a               ; Move Paddle_Right upwards
    ld [hl], a          ; Write new Y to sprite sheet

.MOVE_END
    ; -------- END Paddle_Right Movement --------

    ; -------- Ball Movement --------

    ; -------- NORTH/SOUTH --------
    ld a, [(_RAM + 161)]    ; Load ball's direction into A
    and $01                 ; Mask to get Y direction

    jr nz, .BALL_SOUTH      ; If Z, Move ball north

    ; -------- BALL_NORTH --------
    Spr_getY 2              ; Spr_getY MACRO
    ld a, [hl]              ; Load ball's Y into A
    cp a, 16                ; (Y - N)
    jr z, .BALL_YFLIP       ; If Z, change Y direction

    dec a                   ; Move ball north
    ld [hl], a              ; Load new Y into sprite sheet
    jr .BALL_EW             ; Continue to check X direction

.BALL_SOUTH
    ; -------- BALL_SOUTH --------
    Spr_getY 2              ; Spr_getY MACRO
    ld a, [hl]        ; Load ball's Y into A
    cp a, (SCRN_Y + 8)      ; (Y - (SCRN_Y + N)))
    jr z, .BALL_YFLIP       ; If Z, change Y direction

    inc a                   ; Move ball north
    ld [hl], a        ; Load new Y into sprite sheet
    jr .BALL_EW             ; Continue to check X direction

.BALL_YFLIP
    ld a, [(_RAM + 161)]    ; Load ball's direction into A
    xor a, $01              ; Flip Y direction
    ld [(_RAM + 161)], a    ; Load A into ball's direction

.BALL_EW
    ; -------- EAST/WEST --------
    ld a, [(_RAM + 161)]    ; Load ball's direction into A
    and $02                 ; Mask to get X direction

    jr nz, .BALL_WEST       ; If Z, Move ball east

    ; -------- BALL_EAST --------
    Spr_getX 2              ; Spr_getX MACRO
    ld a, [hl]              ; Load ball's X into A
    cp a, SCRN_X            ; (X - N)
    jr z, .BALL_XCHECK      ; If Z, change X direction

    inc a                   ; Move ball north
    ld [hl], a              ; Load new X into sprite sheet
    jr .BALL_END            ; Continue loop

.BALL_WEST
    ; -------- BALL_WEST --------
    Spr_getX 2              ; Spr_getX MACRO
    ld a, [hl]              ; Load ball's X into A
    cp a, 8                 ; (X - N)
    jr z, .BALL_XCHECK      ; If Z, change X direction

    dec a                   ; Move ball west
    ld [hl], a              ; Load new X into sprite sheet
    jr .BALL_END            ; Continue to check X direction

.BALL_XCHECK
    ld a, [(_RAM + 161)]    ; Load ball's direction into A
    and $02                 ; Mask to get X direction
    jr nz, .BALL_LPADDLE    ; If Z, ball is moving east

    ; -------- BALL_RPADDLE --------
    
    ; -------- CHECK_TOP --------
    Spr_getY 1          ; Spr_getY MACRO
    ld a, [hl]          ; Load Paddle_Right's Y into A
    ld b, a             ; Load A into B

    Spr_getY 2          ; Spr_getY MACRO
    ld a, [hl]          ; Load ball's Y into A

    sub a, 10           ; Subtract offsets from ball's Y
    cp a, b             ; (A - B)
    jr c, .BALL_END     ; If C, ball is above top

    ; -------- CHECK_BOTTOM --------
    Spr_getY 1          ; Spr_getY MACRO
    ld a, [hl]          ; Load Paddle_Right's Y into A
    ld b, a             ; Load A into B

    Spr_getY 2          ; Spr_getY MACRO
    ld a, [hl]          ; Load ball's Y into A

    sub a, 6            ; Subtract offsets from ball's Y
    cp a, b             ; (A - B)
    jr c, .BALL_XFLIP   ; If C, ball is above bottom
    jr .BALL_END

.BALL_XFLIP
    xor $02                 ; Flip X direction
    ld [(_RAM + 161)], a    ; Load A into ball's X direction
    jr .BALL_END            ; Goto BALL_END

.BALL_LPADDLE
    ; -------- BALL_LPADDLE --------
    xor $02                 ; Flip X direction
    ld [(_RAM + 161)], a    ; Load A into ball's X direction

.BALL_END

    ; -------- END Ball Movement --------

    jp loop

.lockup         
    jr .lockup      ; Should never be reached

; -------- END Main Loop --------
; -------- END Main --------