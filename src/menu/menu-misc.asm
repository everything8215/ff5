; +-------------------------------------------------------------------------+
; |                                                                         |
; |                             FINAL FANTASY V                             |
; |                                                                         |
; +-------------------------------------------------------------------------+
; | file: menu/menu-misc.asm                                                |
; |                                                                         |
; | description:                                                            |
; +-------------------------------------------------------------------------+

; ---------------------------------------------------------------------------

; []

_d0dc2a:
_dc2a:  phb
        lda #$00
        pha
        plb
        lda $7ef9a1
        beq @dca0
        cmp #$01
        beq @dc5f
        lda #$41
        sta $4330
        lda #$26        ; window position
        sta $4331
        ldx #$dd81
        stx $4332
        lda #$d0
        sta $4334
        lda #$7e
        sta $4337
        lda $7ebc84
        ora #$08
        sta $7ebc84
        bra @dca0
@dc5f:  lda #$40
        sta $4360
        lda #$31
        sta $4361
        lda $7edbd3
        beq @dc74
        ldx #$dd9f
        bra @dc77
@dc74:  ldx #$ddac
@dc77:  stx $4362
        lda #$d0
        sta $4364
        lda #$7e
        sta $4367
        bra @dca0
        lda #$40
        sta $4360
        lda #$00
        sta $4361
        ldx #$dd7a
        stx $4362
        lda #$d0
        sta $4364
        lda #$7e
        sta $4367
@dca0:  plb
        stz $f9a1
        rtl

; ---------------------------------------------------------------------------

; [ init battle hdma ]

_d0dca5:
_dca5:  phb
        lda #$00
        pha
        plb
        lda #$43        ; 2-address, write twice, indirect
        sta $4300
        sta $4310
        sta $4320
        lda #$0d        ; dma channel 0: bg1 scroll
        sta $4301
        lda #$0f        ; dma channel 1: bg2 scroll
        sta $4311
        lda #$11        ; dma channel 2: bg3 scroll
        sta $4321
        ldx #$a897
        stx $4302
        ldx #$a8b0
        stx $4312
        ldx #$a930
        stx $4322
        lda #$7e
        sta $4304
        sta $4314
        sta $4324
        sta $4307
        sta $4317
        sta $4327
        lda $7edbd3
        beq @dd0a
        lda #$40
        sta $4320
        lda #$32        ; dma channel 2: fixed color
        sta $4321
        ldx #$dd7a
        sta $4322
        lda #$d0
        sta $4324
        lda #$7e
        sta $4327
@dd0a:  lda #$40
        sta $4430
        lda #$05        ; dma channel 3: bg mode
        sta $4331
        lda $7edbd3
        beq @dd1f
        ldx #$dd92
        bra @dd22
@dd1f:  ldx #$dd88
@dd22:  stx $4332
        lda #$d0
        sta $4334
        lda #$7e
        sta $4337
        lda #$40
        sta $4360
        lda #$00        ; dma channel 6: screen brightness
        sta $4361
        ldx #$dd7a
        stx $4362
        lda #$d0
        sta $4364
        lda #$7e
        sta $4367
        lda #$40
        sta $4370
        lda #$08        ; dma channel 7: bg2 base address
        sta $4371
        lda $7edbd3
        beq @dd5e
        ldx #$ddc0
        bra @dd61
@dd5e:  ldx #$ddb6
@dd61:  stx $4372
        lda #$d0
        sta $4374
        lda #$7e
        sta $4377
        lda $7ebc84     ; enable hdma channels 2,3,4,6,7
        ora #$ce
        sta $7ebc84
        plb
        rtl

; ---------------------------------------------------------------------------

; hdma tables

; fixed color / screen brightness
_d0dd7a:
        .byte $f0,$55,$ba
        .byte $f0,$c5,$ba
        .byte $80

; window position
_d0dd81:
        .byte $f0,$f0,$f9
        .byte $f0,$d0,$fa
        .byte $80

; bg mode
_d0dd88:
        .byte $50,$82,$bc
        .byte $50,$82,$bc
        .byte $40,$83,$bc
        .byte $00

; bg mode
_d0dd92:
        .byte $20,$83,$bc
        .byte $50,$82,$bc
        .byte $50,$82,$bc
        .byte $20,$83,$bc
        .byte $00

; color math
_d0dd9f:
        .byte $20,$86,$bc
        .byte $50,$86,$bc
        .byte $50,$86,$bc
        .byte $20,$86,$bc
        .byte $00

; color math
_d0ddac:
        .byte $50,$86,$bc
        .byte $50,$86,$bc
        .byte $40,$87,$bc
        .byte $00

; bg2 base address
_d0ddb6:
        .byte $50,$c2,$db
        .byte $50,$c2,$db
        .byte $40,$c3,$db
        .byte $00

; bg2 base address
_d0ddc0:
        .byte $20,$c3,$db
        .byte $50,$c2,$db
        .byte $50,$c2,$db
        .byte $20,$c3,$db
        .byte $00

_d0ddcd:
        .word $1000
        .word $1000
        .word $1000
        .word $8f00
        .word $8000
        .word $0000

_d0ddd9:
        .byte $c0

_d0ddda:
        .byte $0d,$80,$fd,$40,$fe,$02,$40,$01,$ff
        .byte $fd,$80,$fe,$0f,$40,$fe,$01,$ff
        .byte $fd,$80,$fe,$08,$60,$0b,$60,$01,$ff
        .byte $0e,$60,$fd,$b0,$fe,$01,$ff
        .byte $fe,$09,$60,$0a,$60,$01,$ff
        .byte $fd,$80,$fe,$08,$40,$0b,$40,$03,$50,$01,$62,$0f,$20,$fd,$90,$fe,$01,$28,$10,$28,$fd,$28,$fe,$ff

; ---------------------------------------------------------------------------

; [ copy data to vram (channel 5) ]

;    A: source bank
;   +X: source address
;   +Y: destination address (vram)
; +$70: size

_d0de1a:
_de1a:  phb
        pha
        lda #$00
        pha
        plb
        pla
        sty $2116
        stx $4352
        sta $4354
        lda #$01
        sta $4350
        lda #$18
        sta $4351
        ldx $70
        stx $4355
        lda #$20
        sta $420b
        plb
        rtl

; ---------------------------------------------------------------------------

; [ copy data to vram (channel 4) ]

;    A: source bank
;   +X: source address
;   +Y: destination address (vram)
; +$88: size

_d0de40:
_de40:  phb
        pha
        lda #$00
        pha
        plb
        pla
        sty $2116
        stx $4342
        sta $4344
        lda #$01
        sta $4340
        lda #$18
        sta $4341
        ldx $88
        stx $4345
        lda #$10
        sta $420b
        plb
        rtl

; ---------------------------------------------------------------------------

; [ copy color palettes to vram ]

_d0de66:
_de66:  phb
        lda #$00
        pha
        plb
        sta $2121
        ldx #$2202
        stx $4340
        ldx #$7e09      ; color palettes
        stx $4342
        lda #$7e
        sta $4344
        ldx #$0200
        stx $4345
        lda #$10
        sta $420b
        plb
        rtl

; ---------------------------------------------------------------------------

; [  ]

_d0de8c:
_de8c:  lda $bc75
        bne @ded0
        phb
        lda #$00
        pha
        plb
        ldx #$0000
        stx $2102
        ldx #$0400
        stx $4340
        ldx #$0200
        stx $4342
        lda #$00
        sta $4344
        sta $4347
        ldx #$0220
        stx $4345
        lda #$10
        sta $420b
        lda $7ecd46
        bpl @decf
        lda $7ecd45
        sta $2102
        lda $7ecd46
        sta $2103
@decf:  plb
@ded0:  rtl

; ---------------------------------------------------------------------------

; [ clear vram ]

; +x: vram address
; +y: size

_d0ded1:
_ded1:  phb
        lda #$00
        pha
        plb
        stx $2116
        ldx #$def8      ; D0/DEF8 (16-bit constant zero)
        stx $4352
        lda #$09
        sta $4350
        lda #$18
        sta $4351
        lda #$d0
        sta $4354
        sty $4355
        lda #$20
        sta $420b
        plb
        rtl

; ---------------------------------------------------------------------------

; [ validate inventory ]

_d0ef78:
_ef78:  tdc
        tax
@ef7a:  lda $0640,x     ; item id
        bne @ef82
        stz $0740,x
@ef82:  lda $0740,x     ; item quantity
        bne @ef8a
        stz $0640,x
@ef8a:  inx
        cpx #$0100
        bne @ef7a
        rtl

; ---------------------------------------------------------------------------
