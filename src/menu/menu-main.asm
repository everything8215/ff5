; +-------------------------------------------------------------------------+
; |                                                                         |
; |                             FINAL FANTASY V                             |
; |                                                                         |
; +-------------------------------------------------------------------------+
; | file: menu/menu-main.asm                                                |
; |                                                                         |
; | description: menu code                                                  |
; +-------------------------------------------------------------------------+

.p816

.include "macros.inc"
.include "hardware.inc"
.include "const.inc"

.export ExecMenu_ext, UpdateJoypad_ext

.segment "menu_code"

; ---------------------------------------------------------------------------

; c2/a000
ExecMenu_ext:
        bra ExecMenu

UpdateJoypad_ext:
        jsr     $ff7d
        rtl

_c2a006:
        bra     _a00a

_c2a008:
        bra     _a00f

_a00a:  jsr     $ff56
        bra     _a012
_a00f:  jsr     $ff68
_a012:  jsr     $fe5b
        rtl

; ---------------------------------------------------------------------------

ExecMenu:
@a016:  jsr     $a11b
        stz     $39
        lda     $34
        and     #7
        asl
        tax
        lda     $c0e600,x
        sta     $c7
        shorta
        jmp     ($01c7)

; ---------------------------------------------------------------------------

CommonReturn:
@a02d:  jsr     $b2bd
        shorta
        rtl

; ---------------------------------------------------------------------------

; C2/A11B

InitMenu:
@a11b:  longai
        lda     #$0100
        tcd

        rts

; ---------------------------------------------------------------------------
