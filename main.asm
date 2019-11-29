INCLUDE "hardware.inc"
INClUDE "util.asm"
INCLUDE "dma.asm"

INCLUDE "Paddle.asm"
INCLUDE "Ball.asm"

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
    cp a, SCRN_Y           ; rLY - SCRN_Y
    jr c, .waitVBlank   ; if rLY < SCRN_Y then jump to .waitVBlank

; -------- END WaitVBlank --------

; -------- Initial Configuration --------

    xor a                               ; (ld A, 0)
    ld [rLCDC], a                       ; Load A into LCDC register

    ClearScreen                         ; ClearScreen MACRO

    CopyData _VRAM, PADDLE, PADDLEEND   ; CopyData MACRO

    CopyData (_VRAM + (PADDLEEND - PADDLE)), BALL, BALLEND      ; CopyData MACRO

    ld a, %00011011     ; Load A with colour pallet settings
    ld [rBGP], a        ; Load BG colour pallet with A

    ld a, %00011011
    ld [rOBP0], a       ; Load OBJ0 colour pallet with A

    xor a               ; (ld a, 0)
    ld [rSCY], a        ; Load scroll Y with A
    ld [rSCX], a        ; Load scroll X with A

    xor a               ; (ld a, 0)
    ld [rNR52], a       ; Load sound enable with A

    xor a               ; (ld a, 0)
    or LCDCF_ON         ; Set LCD enabled bit in A
    or LCDCF_BGON       ; Set BG enabled bit in A
    or LCDCF_OBJ8       ; Set OBJ height 8 bit in A
    or LCDCF_OBJON      ; Set OBJ on bit in A
    ld [rLCDC], a       ; Load LCDC with A (settings)

    ; -------- Paddle_Left --------
    ld	hl, _RAM	                ; HL points to sprite's Y
	ld	[hl], (SCRN_Y / 2) + 8	    ; Set Y to middle-screen
	inc	hl		                    ; HL points to sprite's X
	ld	[hl], 8	                    ; Set X to left-screen
	inc	hl		                    ; HL points to sprite's tile (from BG map)
	ld	[hl], $00	                ; Set Tile to the (R) graphic
	inc	hl		                    ; HL points to sprite's flags
	ld	[hl], 0		                ; Set all flags to 0. X,Y-flip, palette, etc.
    ; -------- END Paddle_Left --------

    ; -------- Paddle_Right --------
    ld	hl, (_RAM + 4)	            ; HL points to sprite's Y
	ld	[hl], ((SCRN_Y / 2) + 8)	; Set Y to middle-screen
	inc	hl		                    ; HL points to sprite's X
	ld	[hl], SCRN_X                ; Set X to right-screen
	inc	hl		                    ; HL points to sprite's tile (from BG map)
	ld	[hl], $00	                ; Set Tile to the (R) graphic
	inc	hl		                    ; HL points to sprite's flags
	ld	[hl], OAMF_XFLIP            ; Set all flags to 0. X,Y-flip, palette, etc.

    xor a                           ; (ld a, 0)
    ld [(_RAM + 160)], a            ; Load A into Paddle direction

    ; -------- END Paddle_Right --------

    ; -------- Ball --------
    ld	hl, (_RAM + 8)	            ; HL points to sprite's Y
	ld	[hl], ((SCRN_Y / 2) + 8)	; Set Y to middle-screen
	inc	hl		                    ; HL points to sprite's X
	ld	[hl], ((SCRN_X / 2))        ; Set X to right-screen
	inc	hl		                    ; HL points to sprite's tile (from BG map)
	ld	[hl], $01	                ; Set Tile to the (R) graphic
	inc	hl		                    ; HL points to sprite's flags
	ld	[hl], 0                     ; Set all flags to 0. X,Y-flip, palette, etc.

    ld a, $02                       ; Set initial direction of ball                 #TODO: Random start direction
    ld [(_RAM + 161)], a            ; Load A into ball direction

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
    ld a, [_RAM]    ; Get current Y
    cp a, (16)      ; (Y - N)
    jr z, .JOY_DOWN ; If Y is N, ignore press

    dec a           ; Move Paddle_Left upwards
    ld [_RAM], a    ; Write new Y to sprite sheet

.JOY_DOWN
    pop af          ; Load the joypad state
    and PADF_DOWN   ; If Right then set NZ flag

    jr z, .JOY_END  ; If z flag then skip to JOY_END

    ; -------- JOY_DOWN --------
    ld a, [_RAM]        ; Get current Y 
    cp a, (SCRN_Y + 8)  ; (Y - (SCRN_Y + N))
    jr z, .JOY_END      ; If Y is (SCRN_Y + N), ignore press

    inc a               ; Move Paddle_Left downwards
    ld [_RAM], a        ; Write new Y to sprite sheet

.JOY_END
    ; -------- END Paddle_Left Movement --------

    ; -------- Paddle_Right Movement --------

    ld a, [(_RAM + 160)]    ; Load Paddle_Right's direction into A
    and 1                   ; If 0, set flag
    jr z, .MOVE_DOWN        ; If 0, move downwards

    ; -------- MOVE_UP --------
    ld a, [_RAM + 4]        ; Load Paddle_Right's Y into A
    cp a, 16                ; (Y - N)
    jr z, .MOVE_FLIP        ; If Y is N, flip direction

    dec a                   ; Move Paddle_Right downwards
    ld [_RAM + 4], a        ; Write new Y to sprite sheet

    jr .MOVE_END            ; Continue loop

.MOVE_DOWN
    ; -------- MOVE_DOWN --------
    ld a, [_RAM + 4]        ; Load Paddle_Right's Y into A
    cp a, (SCRN_Y + 8)      ; (Y - (SCRN_Y + N))
    jr z, .MOVE_FLIP        ; If Y is (SCRN_Y + N), flip direction

    inc a                   ; Move Paddle_Right upwards
    ld [_RAM + 4], a        ; Write new Y to sprite sheet

    jr .MOVE_END            ; Continue loop

.MOVE_FLIP
    ; -------- MOVE_FLIP --------
    ld a,  [(_RAM + 160)]   ; Load Paddle_Right's direction into A
    xor 1                   ; Toggle direction
    ld [(_RAM + 160)], a    ; Load new direction into RAM

.MOVE_END
    ; -------- END Paddle_Right Movement --------

    ; -------- Ball Movement --------

    ; -------- NORTH/SOUTH --------
    ld a, [(_RAM + 161)]    ; Load ball's direction into A
    and $01                 ; Mask to get Y direction

    jr nz, .BALL_SOUTH      ; If Z, Move ball north

    ; -------- BALL_NORTH --------
    ld a, [_RAM + 8]        ; Load ball's Y into A
    cp a, 16                ; (Y - N)
    jr z, .BALL_YFLIP       ; If Z, change Y direction

    dec a                   ; Move ball north
    ld [_RAM + 8], a        ; Load new Y into sprite sheet
    jr .BALL_EW            ; Continue to check X direction

.BALL_SOUTH
    ; -------- BALL_SOUTH --------
    ld a, [_RAM + 8]        ; Load ball's Y into A
    cp a, (SCRN_Y + 8)      ; (Y - (SCRN_Y + N)))
    jr z, .BALL_YFLIP       ; If Z, change Y direction

    inc a                   ; Move ball north
    ld [_RAM + 8], a        ; Load new Y into sprite sheet
    jr .BALL_EW            ; Continue to check X direction

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
    ld a, [_RAM + (8 + 1)]  ; Load ball's X into A
    cp a, SCRN_X            ; (X - N)
    jr z, .BALL_XFLIP       ; If Z, change X direction

    inc a                   ; Move ball north
    ld [_RAM + (8 + 1)], a  ; Load new X into sprite sheet
    jr .BALL_END            ; Continue loop

.BALL_WEST
    ; -------- BALL_WEST --------
    ld a, [_RAM + (8 + 1)]  ; Load ball's X into A
    cp a, 8                 ; (X - N)
    jr z, .BALL_XFLIP       ; If Z, change X direction

    dec a                   ; Move ball west
    ld [_RAM + 8 + 1], a    ; Load new X into sprite sheet
    jr .BALL_END            ; Continue to check X direction

.BALL_XFLIP
    ld a, [(_RAM + 161)]    ; Load ball's direction into A
    xor a, $02              ; Flip Y direction
    ld [(_RAM + 161)], a    ; Load A into ball's direction

.BALL_END

    ; -------- END Ball Movement --------

    jp loop

.lockup         
    jr .lockup      ; Should never be reached

; -------- END Main Loop --------

; -------- END Main --------