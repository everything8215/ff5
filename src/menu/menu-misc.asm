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