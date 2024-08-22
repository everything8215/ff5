; +-------------------------------------------------------------------------+
; |                                                                         |
; |                             FINAL FANTASY V                             |
; |                                                                         |
; +-------------------------------------------------------------------------+
; | file: cutscene/cutscene-main.asm                                        |
; |                                                                         |
; | description: code for decompression and loading cinematics              |
; +-------------------------------------------------------------------------+

.p816

.include "macros.inc"
.include "hardware.inc"
.include "const.inc"

.export ShowCutscene_ext, Decomp_ext

; ---------------------------------------------------------------------------

.segment "cutscene_code"

; ---------------------------------------------------------------------------

; [ show cutscene ]

ShowCutscene_ext:
        bra     ShowCutscene

; ---------------------------------------------------------------------------

; [ decompress ]

Decomp_ext:
        php
        phb
        phd
        jsr     InitDecomp
        jsr     Decomp
        pld
        plb
        plp
        rtl

; ---------------------------------------------------------------------------

; [ show cutscene ]

; A: cutscene id

; c3/000f
ShowCutscene:
        .a8
        .i16
        rtl
        ; pha
        jsr     InitDecomp
        sei
        lda     #$80
        sta     hINIDISP
        lda     #$00
        sta     hNMITIMEN
        lda     $c7
        sta     hHDMAEN
        lda     #$c3
        sta     $d2
        ldx     #$7e4d                  ; c3/7e4d
        stx     $d0
        lda     #$7f
        sta     $d5
        ldx     #$8000                  ; 7f/8000
        stx     $d3
        jsr     Decomp
        pla
        jml     $7f8000                 ;

; ---------------------------------------------------------------------------

; [ init decompression ]

; c3/003d
InitDecomp:
        .a8
        .i16
        lda     #0
        pha
        plb
        longa
        lda     #$0420                  ; direct page is at $0420
        tcd
        shorta
        ldx     #0
        stx     $c7
        lda     #$ff
        sta     $c6
        rts

; ---------------------------------------------------------------------------

; [ decompress ]

; ++$04f0/$d0: source
; ++$04f3/$d3: destination

; c3/0053
Decomp:
@0053:  .a8
        .i16
        phb
        lda     #$7f
        pha
        plb
        lda     #$7f
        sta     $c9
        longa
        lda     #$07ff                  ; buffer mask
        sta     $de
        lda     $d0
        cmp     #$ffff
        bne     @008a
        shorta
        lda     [$d0]
        longa
        and     $c6
        sta     $ca
        inc     $d0
        shorta
        inc     $d2
        lda     [$d0]
        longa
        and     $c6
        xba
        ora     $ca
        dec
        sta     $ca
        inc     $d0
        bra     @00a3
@008a:  lda     [$d0]                   ; data length
        dec
        sta     $ca
        inc     $d0
        bne     @0099
        shorta
        inc     $d2
        longa
@0099:  inc     $d0
        bne     @00a3
        shorta
        inc     $d2
        longa
@00a3:  ldx     #$07de
        stx     $cc                     ; set buffer pointer
        ldx     $c7
        txa
        tay
@00ac:  sta     $f7ff,x                 ; clear buffer
        inx2
        cpx     $cc
        bne     @00ac
        stz     $ce
@00b7:  lsr     $ce
        lda     $ce
        and     #$0080
        bne     @00d6                   ; branch if still working on a chunk
        shorta
        lda     [$d0]                   ; header byte
        longa
        inc     $d0
        bne     @00d0
        shorta
        inc     $d2
        longa
@00d0:  and     $c6
        ora     $c8
        sta     $ce                     ; header byte
@00d6:  lda     $ce
        and     #1
        beq     @010b                   ; branch if compressed
        shorta
        lda     [$d0]                   ; data byte
        longa
        inc     $d0
        bne     @00ed
        shorta
        inc     $d2
        longa
@00ed:  shorta
        sta     [$d3],y                 ; copy to destination
        longa
        cpy     $ca
        bne     @00fd
@00f7:  lda     $c7                     ; end of data
        shorta
        plb
        rts
@00fd:  iny
        ldx     $cc
        sta     $f7ff,x                 ; copy to buffer
        txa
        inc
        and     $de                     ; buffer mask
        sta     $cc
        bra     @00b7
@010b:  shorta
        lda     [$d0]                   ; first byte
        xba
        longa
        inc     $d0
        bne     @011a
        shorta
        inc     $d2
@011a:  shorta
        lda     [$d0]                   ; second byte
        sta     $da
        lsr5
        xba
        longa
        sta     $d8
        lda     $da
        and     #$001f
        inc3
        sta     $da                     ; string length
        stz     $dc                     ; clear byte counter
        inc     $d0
        bne     @0140
        shorta
        inc     $d2
        longa
@0140:  lda     $dc                     ; byte counter
        cmp     $da
        beq     @016d                   ; branch when counter reaches the string length
        lda     $d8
        clc
        adc     $dc
        and     $de                     ; buffer mask
        tax
        shorta
        lda     $f7ff,x
        sta     [$d3],y                 ; copy to destination
        longa
        cpy     $ca
        beq     @00f7
        iny
        ldx     $cc
        sta     $f7ff,x                 ; copy to buffer
        inc     $cc
        lda     $cc
        and     $de
        sta     $cc
        inc     $dc
        bra     @0140
@016d:  brl     @00b7
