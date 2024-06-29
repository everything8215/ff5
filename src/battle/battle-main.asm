; +-------------------------------------------------------------------------+
; |                                                                         |
; |                             FINAL FANTASY V                             |
; |                                                                         |
; +-------------------------------------------------------------------------+
; | file: battle/battle-main.asm                                            |
; |                                                                         |
; | description: battle program                                             |
; +-------------------------------------------------------------------------+

.p816

; ===========================================================================

.include "macros.inc"
.include "hardware.inc"
.include "const.inc"
.include "battle_ram.inc"

.export ExecBattle_ext

; ===========================================================================

.segment "battle_code"

; ==============================[ battle main ]==============================

.proc ExecBattle_ext

_0000:  jmp     ExecBattle

.endproc

; ---------------------------------------------------------------------------

; [ execute battle ]

.proc ExecBattle

_0003:  php
        longai
        phb
        phd
        pha
        phx
        phy
        lda     #0
        shorta
        longi
        jsr     $0053
        longa
        clc
        lda     $09c0
        adc     #1                      ; increment battle count
        bcc     _0023
        lda     #$ffff
_0023:  sta     $09c0
        shorta0
        stz     $7cd8
_002c:  jsr     $4ce0
        lda     #0
        sta     f:hINIDISP
        sta     f:hHDMAEN
        sta     f:hMDMAEN
        sta     f:hNMITIMEN
        lda     $7cd8
        bne     _002c
        sei
        jsr     $0053
        longai
        ply
        plx
        pla
        pld
        plb
        plp
        rtl

.endproc

; ---------------------------------------------------------------------------

.proc SetupRegisters

_0053:  lda #$00
        pha
        plb
        sta $4200
        ldx #$0000
        phx
        pld
        lda #$80
        sta $2100
        lda #$7e
        pha
        plb
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CallC1

_0069:  jsl Dispatch_C1
        rts

.endproc

; ---------------------------------------------------------------------------

.proc MusicChange

_006E:  inc wMusicChanged
        sta MusicData
        jsl MusicChange_C4
        stz wMusicChanged

.endproc

; ---------------------------------------------------------------------------

;generates a random number between X and A, inclusive
.proc Random_X_A

_007C:  sep #$10
        stx $3c
        cpx #$ff
        bne +
        bra _Finish
+       cmp #00
        beq _Finish
+       cmp $3c
        beq _Finish
        pha
        tdc
        tax
        lda RNGPointer
        eor #01
        sta RNGPointer
        tax
        lda $3a,X
        tax
        pla
        sec
        sbc $3c
        cmp #$ff
        bne +
        lda ROMRNG,X
        bra _Finish
+
        inc
        sta Divisor
        stz Divisor+1
        lda ROMRNG,X
        tax
        stx Dividend
        rep #$10
        jsr Division
        sep #$10
        clc
        lda Remainder
        adc $3c
_Finish:
        pha
        lda RNGPointer
        tax
        inc $3a,X
        rep #$10
        pla
        rts

.endproc

; ---------------------------------------------------------------------------

.proc Multiply_16bit

_00D2:  rep #$20
        ldx #$0010
        stz $2e
        stz $30
-
        ror $2c
        bcc +
        clc
        lda $_20a3
        adc $30
        sta $30
+
        ror $30
        ror $2e
        dex
        bne -
        tdc
        sep #$20
        rts

.endproc

; ---------------------------------------------------------------------------

.proc Multiply_8bit

_00F1:  lda $24
        sta $004202
        lda $25
        sta $004203
        rep #$20
        nop
        nop
        nop
        lda $004216
        sta $26
        tdc
        sep #$20
        rts

.endproc

; ---------------------------------------------------------------------------

;Divide $7CB7 by $7CB9: result in $7CBB, remainder in $7CBD)
;16 bit
.proc Division

_010C   rep #$20
        stz Quotient
        stz Remainder
        lda Dividend
        beq _Finish
        lda Divisor
        beq _Finish
        clc 
        ldx #$0010
-                                                                               
        rol Dividend
        rol Remainder
        sec 
        lda Remainder
        sbc Divisor
        sta Remainder
        bcs +
        lda Remainder
        adc Divisor
        sta Remainder
        clc
+                                                                       
        rol Quotient
        dex 
        bne -
_Finish                                                                                 ;:                              
        tdc 
        sep #$20
        rts

.endproc

; ---------------------------------------------------------------------------


.if !_StaticMode
.org $C20148
;A block of JIS japenese text is here
; since this isn't the encoding the game uses for text 
; it's likely a hidden programmer message and isn't used in-game
	.byte $20,$20,$20,$20,$20,$BA,$C9,$20					;C2/0148: .byte $20,$20,$20,$20,$20,$BA,$C9,$20
	.byte $BB,$B8,$CB,$DD,$20,$A6,$20,$BB					;C2/0150: .byte $BB,$B8,$CB,$DD,$20,$A6,$20,$BB
	.byte $B2,$B1,$B2,$C5,$D9,$20,$B5,$C4					;C2/0158: .byte $B2,$B1,$B2,$C5,$D9,$20,$B5,$C4
	.byte $B3,$C4,$20,$CB,$B8,$DE,$C1,$20					;C2/0160: .byte $B3,$C4,$20,$CB,$B8,$DE,$C1,$20
	.byte $C0,$B6,$C9,$D8,$20,$C6,$20,$BB					;C2/0168: .byte $C0,$B6,$C9,$D8,$20,$C6,$20,$BB
	.byte $BB,$B9,$DE,$D9,$20,$20,$20,$C8					;C2/0170: .byte $BB,$B9,$DE,$D9,$20,$20,$20,$C8
	.byte $B6,$DE,$DC,$B8,$CA,$DE,$20,$BF					;C2/0178: .byte $B6,$DE,$DC,$B8,$CA,$DE,$20,$BF
	.byte $C9,$20,$C0,$CF,$BC,$B2,$20,$C9					;C2/0180: .byte $C9,$20,$C0,$CF,$BC,$B2,$20,$C9
	.byte $20,$D4,$BD,$D7,$B6,$20,$C5,$D7					;C2/0188: .byte $20,$D4,$BD,$D7,$B6,$20,$C5,$D7
	.byte $DD,$20,$BA,$C4,$20,$A6,$20,$20					;C2/0190: .byte $DD,$20,$BA,$C4,$20,$A6,$20,$20
	.byte $20,$31,$39,$39,$32,$20,$32,$2E					;C2/0198: .byte $20,$31,$39,$39,$32,$20,$32,$2E
	.byte $31,$33,$20,$CB,$B8,$DE,$C1,$20					;C2/01A0: .byte $31,$33,$20,$CB,$B8,$DE,$C1,$20
	.byte $B6,$C2,$CB,$BB,$20,$20,$20,$20					;C2/01A8: .byte $B6,$C2,$CB,$BB,$20,$20,$20,$20
	.byte $20									;C2/01B0: .byte $20

;kono sakuhin wo saiainaru otouto higuchi takanori ni sasageru
;negawakuba sono tamashii no yasuraka naran koto wo
;1992 2.13 higuchi katsuhisa

;Translation (though note that I don't speak Japanese):

;I devote this work to my beloved younger brother Takanori Higuchi
;I pray that his soul rests in peace
;1992 2.13 Katsuhisa Higuchi

;Katsuhisa Higuchi is one of the FF5 battle programmers

.endif

; ---------------------------------------------------------------------------

;Multiplication by 256, 128, 64, 32, 16, 8, 4 or 2 via shifts
.proc ShiftMultiply

_01B1:
.256 
        asl
.128
        asl
.64
        asl
.32
        asl
.16
        asl
.8
        asl
.4
        asl
.2
        asl
        rts

.endproc

; ---------------------------------------------------------------------------

;Division by 256, 128, 64, 32, 16, 8, 4 or 2 via shifts
.proc ShiftDivide

_01BA:
.256
        lsr
.128
        lsr
.64
        lsr
.32
        lsr
.16
        lsr
.8
        lsr
.4
        lsr
.2
        lsr
        rts

.endproc

; ---------------------------------------------------------------------------

;Count number of set bits in 8-bit accumulator, store in X
.proc CountSetBits

_01C3:
        ldx #$0000
        ldy #$0008
-       asl
        bcc +
        inx
+       dey
        bne -
        rts

.endproc


; ---------------------------------------------------------------------------

;Uses a rom table to clear a single bit in A determined by X
.proc ClearBit_X

_01D1:
        and ROMBitUnset,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Uses a rom table to set a single bit in A determined by X
.proc SetBit_X

_01D6:
        ora ROMBitSet,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Uses a rom table to select a single bit from A determined by X
.proc SelectBit_X

_01DB:
        and ROMBitSet,X
        rts

.endproc

; ---------------------------------------------------------------------------

;X = X + 128
;Primarily used to iterate through character structures with that size
.proc NextCharOffset

_01E0:
        rep #$20
        clc
        txa
        adc #$0080
        tax
        tdc
        sep #$20
        rts

.endproc

; ---------------------------------------------------------------------------

;$32 = A * 128
;CalculateCharOffset(A): X = $32 = attacker/target offset A*128
;this one isn't actually used during attack type routines, but it's used in other combat code
.proc CalculateCharOffset

_01EC:
        rep #$20
        jsr ShiftMultiply_128
        tax
        tdc
        sep #$20
        stx AttackerOffset
        rts

.endproc

; ---------------------------------------------------------------------------

;Y = $34 = A * 650
;offset into CharSpells struct for current character
.proc CalculateSpellOffset

_01F8:
        asl
        tax
        rep #$20
        lda ROMTimes650w,X
        sta SpellOffset
        tay
        tdc
        sep #$20
        rts

.endproc

; ---------------------------------------------------------------------------

;Get Timer Offset from ROM (A: Participant index): Y = $36 = timer offset
.proc GetTimerOffset

_0207:
        phx
        asl
        tax
        rep #$20
        lda ROMTimes11w,X	;from ROM
        sta TimerOffset
        tay
        tdc
        sep #$20
        plx
        rts

.endproc