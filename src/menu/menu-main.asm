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

.export ExecMenu_ext, UpdateJoypad_ext, _c2a006, _c2a008

.segment "menu_code"

; ---------------------------------------------------------------------------

; c2/a000
ExecMenu_ext:
        bra ExecMenu

UpdateJoypad_ext:
        jsr     _c2ff7d
        rtl

_c2a006:
        bra     _a00a

_c2a008:
        bra     _a00f

_a00a:  jsr     _c2ff56
        bra     _a012
_a00f:  jsr     _c2ff68
_a012:  jsr     _c2fe5b
        rtl

; ---------------------------------------------------------------------------

ExecMenu:

        stz     $0139
        lda     $0134
        bne     :+
        lda     #$f0                    ; hack to force use a tent
        sta     $0139
:       rtl

; @a016:  jsr     InitMenu
;         stz     $39
;         lda     $34
;         and     #7
;         asl
;         tax
;         lda     $c0e600,x
;         sta     $c7
;         shorta
;         jmp     ($01c7)

; ---------------------------------------------------------------------------

CommonReturn:
; @a02d:  jsr     $b2bd
;         shorta
;         rtl

; ---------------------------------------------------------------------------

; c2/a11b
InitMenu:
; @a11b:  longai
;         lda     #$0100
;         tcd

        rts

; ---------------------------------------------------------------------------

; [ update joypad input ]

_c2fe5b:
        php
        longa
        pha
        php
        phx
        phy
        phb
        phd
        shorta
        longi
        lda     #$00
        pha
        plb
        pea     $0100
        pld
        lda     #$01
@fe72:  bit     $4212
        bne     @fe72
        ldy     #$0000
        lda     $4d
        beq     @fe97
        lda     $0974
        and     #$80
        beq     @fe97       ; branch if single controller
        lda     $010d
        longa
        and     #$0003
        tax
        lda     $097c,x     ; character assigned to controller
        and     #$00ff
        beq     @fe97
        iny
@fe97:  longa
        sty     $12
        tya
        asl
        tax
        lda     $4218,x     ; joypad register
        sta     $06
        and     #$000f
        beq     @feaa
        stz     $06
@feaa:  lda     $14,x
        sta     $0e
        jsr     _c2fed0
        lda     $12
        asl
        tax
        lda     $0e
        sta     $14,x
        pld                 ; pull previous direct page
        lda     $010a
        sta     $00
        lda     $0108
        sta     $02
        lda     $0106
        sta     $04
        plb
        ply
        plx
        plp
        pla
        plp
        rts

; ---------------------------------------------------------------------------

; [  ]

_c2fed0:
@fed0:  .a16
        lda     $0974
        and     #$0040
        bne     @fede       ; branch if custom button config
        lda     $06
        sta     $08
        bra     @fefb
@fede:  lda     $06
        and     #$1f0f
        sta     $08
        ldx     #$0000
@fee8:  lda     $06
        and     f:_c0e7b8,x
        beq     @fef4
        lda     $26,x
        tsb     $08
@fef4:  inx2
        cpx     #$000e
        bne     @fee8
@fefb:  lda     $0e
        eor     #$ffff
        and     $08
        sta     $0a
        lda     $08
        and     $0e
        shorta
        bit     $010c
        longa
        bmi     @ff14
        and     #$7f7f
@ff14:  sta     $10
        lda     $08
        sta     $0e
        ldx     #$0000
        ldy     $12
@ff1f:  longa
        lda     f:_c0e7c6,x
        bit     $0a
        bne     @ff43       ; branch if already in repeat mode
        and     $10
        beq     @ff45
        shorta
        lda     $011a,y     ; decrement delay counter
        dec
        beq     @ff37
        bpl     @ff45
@ff37:  longa
        lda     f:_c0e7c6,x
        tsb     $0a         ; set repeat mode
        lda     $19
        bra     @ff45
@ff43:  lda     $18
@ff45:  shorta
        sta     $011a,y
        iny2                ; next button
        inx2
        cpx     #$000c
        bne     @ff1f
        longa
        rts

; ---------------------------------------------------------------------------

; [ ??? (battle) ]

_c2ff56:
        php
        longa
        pha
        lda     #$0310      ; delay = 16 frames, rate = 3 frames
        sta     f:$000118     ; repeat settings
        sta     f:$00014d     ; allow multipler controllers
        pla
        plp
        rts

; ---------------------------------------------------------------------------

; [ ??? (field) ]

_c2ff68:
        php
        longa
        pha
        lda     #$0416      ; delay = 22 frames, rate = 4 frames
        sta     f:$000118     ; repeat settings
        lda     #$0000
        sta     f:$00014d     ; single controller
        pla
        plp
        rts

; ---------------------------------------------------------------------------

; [ update joypad config ]

_c2ff7d:
        phb
        phd
        pha
        phx
        phy
        php
        pea     $0000
        plb
        plb
        longai
        pea     $0100
        pld
        lda     #$0416      ; delay = 22 frames, rate = 4 frames
        sta     $18
        stz     $4d         ; single controller
        ldy     #$0000
        tyx
@ff99:  lda     $0975,y     ; joypad config
        jsr     _c2ffc2
        sta     $26,x
        iny
        inx2
        cpy     #$0007
        bne     @ff99
        stz     $0e
        stz     $14
        stz     $16
        lda     #$0101
        ldx     #$000c
@ffb5:  dex2
        sta     $1a,x
        bne     @ffb5
        plp
        ply
        plx
        pla
        pld
        plb
        rts

; ---------------------------------------------------------------------------

; [  ]

_c2ffc2:
@ffc2:  .a16
        phx
        and     #$00fc
        xba
        ldx     #$0000
@ffca:  asl
        bcs     @ffd7
        inx2
        cpx     #$000c
        bne     @ffca
        ldx     #$0000
@ffd7:  lda     f:_c0e7b8,x
        plx
        rts

; ---------------------------------------------------------------------------

; todo: move to the correct segment
_c0e7b8:
        .word   $0080,$8000,$0040,$4000,$0020,$0010,$2000

_c0e7c6:
        .word   $0080,$8000,$0800,$0400,$0200,$0100

; ---------------------------------------------------------------------------
