INCLUDE "hardware.inc"

_RAM_END EQU $DFFF
_VRAM_END EQU $9FF0
_BG_MAP EQU $9000

; Writes 0s to RAM
ClearRAM\@: MACRO

    ld hl, _RAM             ; Load HL with pointer to RAM
    ld bc, _RAM_END - _RAM  ; Load BC with length of RAM

.clearRAMLoop\@
    xor a                   ; (ld a, $00)
    ld [hl], a              ; Set RAM address to 0
    inc hl                  ; Load HL with next RAM address
    dec bc                  ; Decrement length of RAM (count)
    ld a, c                 ; Load A with 8 LSBs of count
    or b                    ; OR count 8 LSBs with its 8 MSBs 
    jr nz, .clearRAMLoop\@  ; If the result is not zero (BC ~= 0), continue loop
ENDM

; Writes 0s to RAM & preserves registers
ClearRAM_P: MACRO
    push hl     ; Preserve HL register
    push bc     ; Preserve BC register

    ClearRAM    ; ClearRAM MACRO

    pop bc      ; Restore BC register
    pop hl      ; Restore HL register
ENDM

; Writes 0s to BG tile data & map
ClearScreen\@: MACRO

    ld hl, _SCRN0               ; Load HL with pointer to the BG tile data
    ld de, _BG_MAP              ; Load DE with pointer to the BG tile map
    ld bc, _VRAM_END - _SCRN0   ; Load BC with length of BG tile data + BG tile map banks

.clearScreenLoop\@
    ld a, d                     ; Load A with 8 MSBs of _BG_MAP
    ld [hl], a                  ; Load BG tile data with A
    ld a, e                     ; Load A with 8 LSBs of _BG_MAP
    ld [hl], a                  ; Load BG tile data with A
    inc hl                      ; Load HL with next VRAM address
    dec bc                      ; Decrement length of banks (count)
    ld a, c                     ; Load A with 8 LSBs of count
    or b                        ; OR count 8 LSBs with its 8 MSBs
    jr nz, .clearScreenLoop\@   ; If the result is not zero (BC ~= 0), continue loop
ENDM

; Writes 0s to BG tile data & map & preserves registers
ClearScreen_P\@: MACRO
    push hl     ; Preserve HL register
    push de     ; Preserve DE register
    push bc     ; Preserve BC register

    ClearScreen ; ClearScreen MACRO

    pop bc      ; Restore BC register
    pop de      ; Restore DE register
    pop hl      ; Restore HL register
ENDM

; Copys data from src address to dst address
CopyData\@: MACRO

    ld hl, \1               ; Load HL with pointer to dst address
    ld de, \2               ; Load DE with pointer to the start of src data
    ld bc, \3 - \2          ; Load BC with length of src data

.copyDataLoop\@
    ld a, [de]              ; Load src data into A
    ld [hli], a             ; Load A into dst address and move to next address
    inc de                  ; Move to next src data address
    dec bc                  ; Decrement length of src data (count)
    ld a, b                 ; Load A with 8 LSBs of count
    or c                    ; OR count 8 LSBs with its 8 MSBs
    jr nz, .copyDataLoop\@  ; If the result is not zero (BC ~= 0), continue loop
ENDM

; Copys data from src address to dst address & preserves registers
CopyData_P\@: MACRO
    push hl     ; Preserve HL register
    push de     ; Preserve DE register
    push bc     ; Preserve BC register

    CopyData    ; CopyData MACRO

    pop bc      ; Restore BC register
    pop de      ; Restore DE register
    pop hl      ; Restore HL register
ENDM