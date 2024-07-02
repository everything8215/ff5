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
.reloc

.import InitSound_ext, ExecBtlGfx_ext
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

_0069:  jsl ExecBtlGfx_ext
        rts

.endproc

; ---------------------------------------------------------------------------

.proc MusicChange

_006E:  inc wMusicChanged
        sta MusicData
        jsl InitSound_ext
        stz wMusicChanged

.endproc

; ---------------------------------------------------------------------------

;generates a random number between X and A, inclusive
.proc Random_X_A

_007C:  sep #$10
        stx $3c
        cpx #$ff
        bne :+
        bra _Finish
:       cmp #00
        beq _Finish
        cmp $3c
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
        bne :+
        lda ROMRNG,X
        bra _Finish
:
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

_00D2:  longa
        ldx #$0010
        stz $2e
        stz $30
:
        ror $2c
        bcc :+
        clc
        lda $2a
        adc $30
        sta $30
:
        ror $30
        ror $2e
        dex
        bne :--
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

.proc Multiply_8bit

_00F1:  lda $24
        sta $004202
        lda $25
        sta $004203
        longa
        nop
        nop
        nop
        lda $004216
        sta $26
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Divide $7CB7 by $7CB9: result in $7CBB, remainder in $7CBD)
;16 bit
.proc Division

_010C:  longa
        stz Quotient
        stz Remainder
        lda Dividend
        beq _Finish
        lda Divisor
        beq _Finish
        clc 
        ldx #$0010
:                                                                               
        rol Dividend
        rol Remainder
        sec 
        lda Remainder
        sbc Divisor
        sta Remainder
        bcs :+
        lda Remainder
        adc Divisor
        sta Remainder
        clc
:                                                                       
        rol Quotient
        dex 
        bne :--
_Finish:
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------


;.if !_StaticMode
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

;.endif

; ---------------------------------------------------------------------------

;Multiplication by 256, 128, 64, 32, 16, 8, 4 or 2 via shifts
.proc ShiftMultiply

_01B1:
_256:
        asl
_128:
        asl
_64:
        asl
_32:
        asl
_16:
        asl
_8:
        asl
_4:
        asl
_2:
        asl
        rts

.endproc

; ---------------------------------------------------------------------------

;Division by 256, 128, 64, 32, 16, 8, 4 or 2 via shifts
.proc ShiftDivide

_01BA:
_256:
        lsr
_128:
        lsr
_64:
        lsr
_32:
        lsr
_16:
        lsr
_8:
        lsr
_4:
        lsr
_2:
        lsr
        rts

.endproc

; ---------------------------------------------------------------------------

;Count number of set bits in 8-bit accumulator, store in X
.proc CountSetBits

_01C3:
        ldx #$0000
        ldy #$0008
:       asl
        bcc :+
        inx
:       dey
        bne :--
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
        longa
        clc
        txa
        adc #$0080
        tax
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;$32 = A * 128
;CalculateCharOffset(A): X = $32 = attacker/target offset A*128
;this one isn't actually used during attack type routines, but it's used in other combat code
.proc CalculateCharOffset

_01EC:
        longa
        jsr ShiftMultiply::_128
        tax
        shorta0
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
        longa
        lda ROMTimes650w,X
        sta SpellOffset
        tay
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Get Timer Offset from ROM (A: Participant index): Y = $36 = timer offset
.proc GetTimerOffset

_0207:
        phx
        asl
        tax
        longa
        lda ROMTimes11w,X	;from ROM
        sta TimerOffset
        tay
        shorta0
        plx
        rts

.endproc

; ---------------------------------------------------------------------------

;Wipes structures related to pending animations, message boxes, and damage displays
.proc WipeDisplayStructures

_0218:
        ldx #$0090
WipeAnimBlocks:		;wipes Anim structure and blocking information after it, $3BCC-$3C5B
        stz ActionAnim::Flags,X
        dex
        bpl WipeAnimBlocks
        txa 		;A now $FF
        ldx #$037F
WipeGFXQueueDamage:	;wipes GFXQueue structure and DisplayDamage after it with $FF, $384C-$3BCB
        sta !GFXQueue,X
        dex
        bpl WipeGFXQueueDamage
        ldx #$005F
WipeMessagesTimers:	;wipes Message Boxes and Timer structures after them with $FF, $3C5F-$3E8D
        sta MessageBoxes,X
        dex
        bpl WipeMessagesTimers
        tdc
        ldx #$000F
WipeReflectCounters:	;wipes $7B49-7B58
        stz CounterReflecteeTable,X
        dex
        bpl WipeReflectCounters
        ldx #$002F
WipeMessageBoxData:	;wipes numbers used for message boxes, $3CBF-$3CEE
        stz MessageBoxData,X
        dex
        bpl WipeMessageBoxData
        rts

.endproc

; ---------------------------------------------------------------------------

;Recalculate Stats+Level w/Song(X: Character Offset)
.proc CopyStatsWithBonuses

_0248:
        phx
        phy
        tdc
        tay
AddCopyStats:			;adds/copies Str/Agi/Vit/Mag
        clc
        lda CharStruct::EquippedStr,X
        adc CharStruct::BonusStr,X
        cmp #$64		;100
        bcc :+
        lda #$63   		;cap at 99
:	sta Strength,Y
        inx
        iny
        cpy #$0004
        bne AddCopyStats
        ply
        plx
        clc
        lda CharStruct::Level,X
        adc CharStruct::BonusLevel,X
        cmp #$64		;100
        bcc :+
        lda #$63   		;cap at 99
:	sta Level
        rts

.endproc

; ---------------------------------------------------------------------------


;Params: 	A = character index
;		$3C5D (TempDisplayDamage) = displayed damage
.proc CopyDisplayDamage

_0276:
        tax
        lda ROMCombatantReorder,X	;party members after monsters
        asl
        tax
        lda TempDisplayDamage
        sta DisplayDamage,X
        lda TempDisplayDamage+1
        sta DisplayDamage+1,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc WipeActionData

_028A:
        stz wTargetIndex	;**optimize: wastes a byte
        ldx #$0133
:	stz $79F9,X	;clears memory $79F9 - $7B2C
        dex
        bpl :-
        txa 		;now $FF
        ldx #$0010
:	sta $7B2D,X	;sets memory $7B2D - $7B3D to $FF
        dex
        bpl :-
        tdc
        rts

.endproc

; ---------------------------------------------------------------------------

;(Random number from 0..99)
.proc Random_0_99

_02A2:
        tdc
        tax
        lda #$63
        jmp Random_X_A

.endproc

; ---------------------------------------------------------------------------

;combines MonsterTargets and PartyTargets into a combined target bitmask PPPPMMMM MMMM0000 to match normal index order
.proc BuildTargetBitmask

_02A9:
        lda MonsterTargets
        pha
        and #$F0
        lsr
        lsr
        lsr
        lsr
        ora PartyTargets
        sta TempTargetBitmask
        pla
        and #$0F
        asl
        asl
        asl
        asl
        sta TempTargetBitmask+1
        rts

.endproc
; ---------------------------------------------------------------------------

;returns with number of targets minus 1 in A and $2620 (TempTargetting) 
;notably, 0 if single target
.proc CheckMultiTarget

_02C2:
        lda MonsterTargets
        ora PartyTargets
        jsr CountSetBits
        dex
        txa
        sta TempTargetting
        rts

.endproc

; ---------------------------------------------------------------------------

.proc RemoveInactiveTargets

_02CF:
        stz NoValidTargets
        tdc
        tax
        stx $0E
Loop:
        lda ActiveParticipants,X
        beq Inactive
        ldy $0E
        lda CharStruct::Status1,Y
        and #$C0	;dead/stone
        bne Inactive
        lda CharStruct::Status4,Y
        and #$81	;erased/hiding
        beq Next
Inactive:
        phx
        cpx #$0004	;monster check
        bcs Monster
        lda PartyTargets
        jsr ClearBit_X
        sta PartyTargets
        bra NextPLX
Monster:
        txa
        sec
        sbc #$04	;monster index
        tax
        lda MonsterTargets
        jsr ClearBit_X
        sta MonsterTargets
NextPLX:
        plx
Next:
        longa
        clc
        lda $0E
        adc #$0080	;next CharStruct offset
        sta $0E
        shorta0
        inx
        cpx #$000C	;12 battle participants
        bne Loop
        lda PartyTargets
        ora MonsterTargets
        bne Ret
        inc NoValidTargets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Copies magic data for spell in A from ROM to AttackInfo (offset Y)
.proc CopyROMMagicInfo

_0324:
        longa
        jsr ShiftMultiply::_8	;Size of Magic Data
        tax
        shorta0
        stz $3D
CopyFirst5:                                                                                                            
        lda !ROMMagicInfo,X
        sta !AttackInfo,Y
        inx
        iny
        inc $3D
        lda $3D
        cmp #$05
        bne CopyFirst5
        iny 			;AttackInfo has 4 bytes that
        iny 			;don't apply to magic
        iny
        iny
CopyLast3:
        lda !ROMMagicInfo,X
        sta !AttackInfo,Y
        inx
        iny
        inc $3D
        lda $3D
        cmp #$08
        bne CopyLast3
        rts

.endproc

; ---------------------------------------------------------------------------

;Advances the current message box offets to the next set of messgae boxes
;(for multi-commands like x-magic)
.proc NextMessageBoxSet

_0356:
        clc
        lda MessageBoxOffset
        adc #$18		;+24, next message box set
        sta MessageBoxOffset
        clc
        lda MessageBoxDataOffset
        adc #$0C		;+12, next message box data set
        sta MessageBoxDataOffset
        rts

.endproc

; ---------------------------------------------------------------------------

;(Y:index into in-battle inventory)
;(Returns A: bitmask depending on equipment type and some character properties)
; format seems to be 2 bits per character, 00 for usable and 10 for not
.proc GetItemUsableY
.reloc ; TODO: don't know why this is needed

_0369:
        lda Temp,Y
        bpl :+
        lda #$AA		;usable for none
        jmp Ret
:	and #$40		;Consumable
        beq Equipment
        lda InventoryFlags,Y
        and #$20
        beq RetZero
        lda #$AA		;usable for none
        bra Ret
RetZero:								
        lda #$00		;usable for all
        bra Ret
Equipment:		;**optimize: this whole section is basically a copy of the GetItemUsableA $455E subroutine		
        lda Temp,Y
        asl
        asl
        tax
        tdc
        tay
        									;:					
:	lda !ROMItemEquippable,X
        sta !TempEquippable,Y
        inx
        iny
        cpy #$0004
        bne :-
        									;.					
        tdc
        tax
        tay
        lda #$AA
        sta $0E
        									;:					
DetermineEquippableLoop:
        lda CharEquippable::Weapons,X
        and TempEquippable::Weapons,Y
        bne Match
        lda CharEquippable::Weapons+1,X
        and TempEquippable::Weapons+1,Y
        bne Match
        lda CharEquippable::Armor,X
        and TempEquippable::Armor,Y
        bne Match
        lda CharEquippable::Armor+1,X
        and TempEquippable::Armor+1,Y
        beq NextChar
Match:
        txa
        lsr
        lsr
        bne Check1
        									;.					
        lda $0E
        and #$7F		;clear first character bit
        sta $0E
        bra NextChar
Check1:								
        cmp #$01
        bne Check2
        lda $0E
        and #$DF		;clear second character bit
        sta $0E
        bra NextChar
Check2:								
        cmp #$02
        bne Other
        lda $0E
        and #$F7         	;clear third character bit
        sta $0E
        bra NextChar
Other:								
        lda $0E
        and #$FD		;clear fourth character bit
        sta $0E
NextChar:
        inx
        inx
        inx
        inx
        cpx #$0010			;4 bytes * 4 characters
        bne DetermineEquippableLoop
        lda $0E
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;(sets up equipment type, targetting, and usable bytes for one item in battle inventory)
;(A: item#)
;(Y: inventory offset to write data about item)
;Returns with EquipmentType low bytes in Temp ($2620), $40 there if it's a consumable, $80 if unusable
;TODO: find a better way to access into the D1 item data because I didnt know how the translation from asar to ca65 would work
.proc SetupInventoryInfo

_03FA:
        beq ItemZero
        cmp #$E0
        bcs Consumable		;>$E0 is consumable
        cmp #$80
        bcc Weapon		;<$80 is a weapon
Armor:				;otherwise it's armor
        sec
        sbc #$80		;remove the armor offset
        longa
        asl
        asl
        sta $0E
        asl
        clc
        adc $0E			;armor *12 (size of equipment struct)
        tax
        shorta0
        lda ROMArmor,X
        and #$08		;target enemy?
        beq ItemZero
        lda ROMArmor+2,X
        and #$3F		;mask to equip info
        sta Temp,Y
        lda #$5A
        sta InventoryFlags,Y
        bra Ret
ItemZero:	;or armor targettng bit 08h
        lda #$80		;not usable
        sta Temp,Y
        lda #$5A
        sta InventoryFlags,Y
        bra Ret
Weapon:
        longa
        asl
        asl
        sta $0E
        asl
        clc
        adc $0E         	;weapon *12
        tax
        shorta0
        lda ROMWeapons+4,X
        and #$80
        jsr ShiftDivide::_32	;shift to 04h bit
        sta InventoryFlags,Y
        lda ROMWeapons,X
        sta InventoryTargetting,Y
        lda ROMWeapons+2,X
        pha
        and #$C0		;flag bits from equipment type(? and throwable)
        ora #$1A		;set some more bits (??)
        ora InventoryFlags,Y	;keep existing bits (double grip)
        sta InventoryFlags,Y
        pla
        and #$3F		;mask to just equip info
        sta Temp,Y
        bra Ret
Consumable:								
        sec
        sbc #$E0
        longa
        asl
        asl
        asl
        tax
        shorta0
        lda ROMConsumables::Targetting,X
        sta InventoryTargetting,Y
        lda ROMConsumables::Misc,X
        sta InventoryFlags,Y
        lda #$40		;consumable
        sta Temp,Y
Ret: 	RTS

.endproc

; ---------------------------------------------------------------------------

;Utility routine used for the +% HP/MP passives on level up
;inputs: 
;$2E: 4 byte multiply result (value * percentage)
;$08: 2 byte base value
;$0A: 2 byte cap (generally 999 or 9999)
;output:
;$08: 2 byte value (value*percentage/100)+Base, capped
.proc ApplyPercentage

_0491:
        ldx #$000F
:	stz $0E,X		;clear $0E-1D
        dex
        bpl :-
        ldx #$0064		;100
        stx $12
        ldx $2E			;previous multiply result (low bytes)
        stx $0E
        lda $30			;(high bytes)
        sta $10
;32 bit division routine
;Dividend: 	$0E-11
;Divisor: 	$12-15
;Quotient: 	$16-19
;Remainder: 	$1A-1C
;**optimize: make this a general purpose subroutine, duplicate at $57FE
        longa
        clc
        ldx #$0020
:	rol $0E
        rol $10
        rol $1A
        rol $1C
        sec
        lda $1A
        sbc $12
        sta $1A
        lda $1C
        sbc $14
        sta $1C
        bcs :+
        lda $1A
        adc $12
        sta $1A
        lda $1C
        adc $14
        sta $1C
        clc
:	rol $16
        rol $18
        dex
        bne :--
        shorta0
;division ends here
        clc
        lda $16		;quotient, input/100
        adc $08		;base value
        sta $08		;adjusted value
        lda $17		;high byte of above
        adc $09
        sta $09
        sec 		;checks against 9999
        lda $08
        sbc $0A		;9999 low byte
        lda $09
        sbc $0B		;9999 high byte
        bcc Ret
        lda $0A		;caps at 9999
        sta $08
        lda $0B
        sta $09
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;used by defend and guard
.proc NoActionAbility

_04FB:
        jsr $16E1 ;GFXCmdAbilityAnim
        lda ProcSequence
        tax
        stz AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        inc UnknownReaction
        jmp $1750 ;FinishCommandNullTargets
;Note about commands:
;Most of FF5 uses a command value between $01-$56, which matches the slots on the combat menu
;this is then mapped to command table values $00-$34 via a rom table
;most of the time the difference is just -1, but all magic types map to $2B 
;everything after $2B shifts down accordingly

.endproc

; ---------------------------------------------------------------------------

.proc CopyAbilityInfo

_16AA:
        pha
        jsr SelectCurrentProcSequence
        pla
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        stz $0A
:	LDA !ROMAbilityInfo,X
        sta !AttackInfo,Y
        inx
        iny
        inc $0A
        lda $0A
        cmp #$05     ;copy first 5 bytes
        bne :-
        iny          ;skip 4 on destination
        iny
        iny
        iny
:	LDA !ROMAbilityInfo,X
        sta !AttackInfo,Y
        inx
        iny
        inc $0A
        lda $0A
        cmp #$08      ;copy remaining 3 bytes
        bne :-
        rts

.endproc

; ---------------------------------------------------------------------------

;Displays an ability or command animation
;creates Action $00,FC,01,<A>,00
.proc GFXCmdAbilityAnim

_16E1:
        pha
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01	;ability/command anim
        sta GFXQueue::Type,X
        pla
        sta GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        rts

.endproc
; ---------------------------------------------------------------------------

;Displays an Attack name from String Table 1
;creates Action $00,FC,04,01,<A>
.proc GFXCmdAttackNameA

_16FA:
        sta Temp+1
        lda #$01
        sta Temp
        jmp GFXCmdAttackNameFromTemp

.endproc

; ---------------------------------------------------------------------------

.proc MagicAtkTypeSingleTarget

_1705:
        lda ProcSequence
        tax
        ldy $0C
        lda AttackInfo::MagicAtkType,Y
        and #$7F
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc MagicAtkTypeMultiTarget

_171A:
        lda ProcSequence
        tax
        ldy $0C
        lda AttackInfo::MagicAtkType,Y
        and #$7F
        sta AtkType,X
        lda TempTargetting
        inc 	;unconditional, so always considered multitarget
        sta MultiTarget,X
        lda #$80	;multi target
        sta TargetType,X
        rts

.endproc

; ---------------------------------------------------------------------------

;copies command targetting to final locations and advances ProcSequence
.proc FinishCommand

_1735:
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        sta TargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        sta TargetBitmask+1,X
        inc ProcSequence
        rts

.endproc

; ---------------------------------------------------------------------------

;wipes command targetting and advances ProcSequence
.proc FinishCommandNullTargets

_1750:
        lda ProcSequence
        asl
        tax
        stz CommandTargetBitmask,X
        stz CommandTargetBitmask+1,X
        inc ProcSequence
        rts

.endproc

; ---------------------------------------------------------------------------

.proc GetTargets

_175F:
        ldx AttackerOffset
        lda CharStruct::PartyTargets,X
        sta PartyTargets
        lda CharStruct::MonsterTargets,X
        sta MonsterTargets
        rts

.endproc
; ---------------------------------------------------------------------------

.proc HandleATBMenu

_176C:
        lda MenuData::MenuOpen
        bne MenuOpen
        jmp MenuClosed
MenuOpen:	;checks if current display info for status/mp matches what's in CharStruct
        lda DisplayInfo::CurrentChar
        sta CurrentChar
        jsr CalculateCharOffset
        longa
        lda CharStruct::Status1,X	;includes status 2
        cmp DisplayInfo::Status1
        bne Differs
        lda CharStruct::Status3,X	;includes status 4
        cmp DisplayInfo::Status3
        bne Differs
        lda CharStruct::CurMP,X
        cmp DisplayInfo::CurMP
        bne Differs
        shorta0
        bra Matches
Differs:	;disable commands as needed, and update displayinfo for menu
        shorta0
        jsr CheckDisablingStatus
        bne Disabled
        jsr DisableCommandsMagic
        jsr ApplyBerserkStatus
        bne Disabled
        lda #$05		;C1 routine
        jsr CallC1
        lda #$06		;C1 routine
        jsr CallC1
        longa
        ldx AttackerOffset
        lda CharStruct::Status1,X
        sta DisplayInfo::Status1
        lda CharStruct::Status3,X
        sta DisplayInfo::Status3
        lda CharStruct::CurMP,X
        sta DisplayInfo::CurMP
        shorta0
        bra Matches
Disabled:	;if character has become disabled while their menu is open, close the menu
        lda DisplayInfo::CurrentChar
        sta MenuCurrentChar
        lda GearChanged
        beq :+
        stz GearChanged
        jsr ReplaceHands
        jsr ApplyGear
:	LDA DisplayInfo::CurrentChar
        sta MenuCurrentChar
        lda MenuDataC1::MenuOpen
        beq WaitMenu
        lda #$01	;C1 routine: close menu
        jsr CallC1
WaitMenu:
        lda MenuDataC1::MenuOpen
        bne WaitMenu	;ends up 0 eventually? via interrupts?
        lda #$FF
        sta DisplayInfo::CurrentChar
        rts
Matches:	;data either already matched or has been updated
        lda ControllingA
        beq Ret
        lda DisplayInfo::CurrentChar
        tax
        lda ControlTarget,X
        beq :+
        tax
        lda ActiveParticipants,X
        bne Ret
:	LDA DisplayInfo::CurrentChar
        sta MenuCurrentChar
        lda MenuDataC1::MenuOpen
        beq WaitMenu2
        lda #$01	;C1 routine: close menu
        jsr CallC1
WaitMenu2:
        lda MenuDataC1::MenuOpen
        bne WaitMenu2	;ends up 0 eventually? via interrupts?
        lda #$80
        sta MenuData::ActionFlag
        stz MenuData::Command
        stz MenuData::MonsterTargets
        stz MenuData::PartyTargets
        stz MenuData::SelectedItem
        stz MenuData::SecondActionFlag
        stz MenuData::SecondCommand
        stz MenuData::SecondMonsterTargets
        stz MenuData::SecondPartyTargets
        stz MenuData::SecondSelectedItem
        bra MenuClosed
Ret:	rts
MenuClosed:								;
        lda DisplayInfo::CurrentChar
        cmp #$FF
        beq NoCurrentChar
        jmp ProcessMenuCommand
NoCurrentChar:
        lda ATBReadyQueue
        cmp #$FF
        bne NextReadyATB
        rts		;no one else in queue either
NextReadyATB:	;there's a character in the queue with ATB ready
        pha
        tdc
        tax
AdvanceQueue:	;advances all the queue elements up by one, there's a terminator $FF in the 5th slot
        lda ATBReadyQueue+1,X
        sta ATBReadyQueue,X
        inx
        cpx #$0004
        bne AdvanceQueue
        dec ATBReadyCount
        pla
        sta MenuCurrentChar
        sta DisplayInfo::CurrentChar
        jsr CalculateCharOffset
        lda QuickTurns
        beq DontStopTime
        lda DisplayInfo::CurrentChar
        cmp QuickCharIndex
        beq DontStopTime
        jsr GetTimerOffset  	;sets Y to Timer offset
        lda CurrentTimer::ATB,Y
        bne FinishEarly	;check if frozen char's ATB is ready
        lda #$01		;increase ATB to 1 (no longer ready)
        sta CurrentTimer::ATB,Y
        sta EnableTimer::ATB,Y
        bra FinishEarly
DontStopTime:
        jsr CheckDisablingStatus
        beq NotDisabled
FinishEarly:
        lda #$FF
        sta DisplayInfo::CurrentChar
        rts
NotDisabled:	;character's turn has just come up
        stz MenuCurrentChar+1
        jsr ApplyBerserkStatus
        bne FinishEarly
        jsr DisableCommandsMagic
        lda #$01
        sta ATBWaiting
        lda ATBWaitTime
        sta ATBWaitLeft
        longa
        ldx AttackerOffset
        lda CharStruct::Status1,X	;includes 2
        sta DisplayInfo::Status1
        lda CharStruct::Status3,X	;includes 4
        sta DisplayInfo::Status3
        lda CharStruct::CurMP,X
        sta DisplayInfo::CurMP
        stz CharStruct::CmdStatus,X	;also damagemod
        shorta0
        jsr CheckControlTargetActive
        lda #$01
        sta FleeTickerActive	;can't start running until first atb
        lda EncounterInfo::IntroFX
        bpl NoCredits
        stz MenuData::MenuOpen
        rts
NoCredits:
        lda DisplayInfo::CurrentChar
        jsr GetTimerOffset	;Y = Timer offset
        lda EnableTimer::ATB,Y
        beq TimerOff
        lda #$FF
        sta DisplayInfo::CurrentChar
        jmp NoCurrentChar
TimerOff:
        tdc
        jsr CallC1 	;C1 routine $00: open menu
WaitMenu3:
        lda MenuDataC1::MenuOpen
        beq WaitMenu3
        rts
ProcessMenuCommand:
        stz ATBWaiting
        jsr ProcessMenuCommandData
        lda #$FF	;no current char
        sta DisplayInfo::CurrentChar
        rts

.endproc
; ---------------------------------------------------------------------------


;Sets ControllingA and B variables to 1 if control target is valid and active, 0 otherwise
;logic is a bit strange but doesn't seem like they can ever be set to different values
.proc CheckControlTargetActive

_190B:
        stz ControllingA
        lda DisplayInfo::CurrentChar
        tax
        lda ControlTarget,X
        beq Finish
        tay
        lda ActiveParticipants,Y
        beq Finish
        lda #$01
        sta ControllingA
Finish:	STA ControllingB
        rts

.endproc
; ---------------------------------------------------------------------------

;copies command data from MenuData struct into CharStruct, and performs any other necessary processing
;also handles gear changes, removing control when needed, consuming items when used, and action delays
.proc ProcessMenuCommandData

_1926:
        lda EncounterInfo::IntroFX
        bpl :+		;check for credits demo
        jsr SetupCreditsDemo
:	LDA DisplayInfo::CurrentChar
        sta CurrentChar
        lda GearChanged
        beq :+
        stz GearChanged
        jsr ReplaceHands
        jsr ApplyGear
:	LDA DisplayInfo::CurrentChar
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        and #$C0	;dead/stone
        bne ClearControl
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78	;sleep/para/charm/berserk
        bne ClearControl
        lda CharStruct::Status3,X
        and #$10	;stop
        bne ClearControl
        lda CharStruct::Status4,X
        and #$80	;erased
        beq :+
ClearControl:
        lda DisplayInfo::CurrentChar
        tax
        stz ControlTarget,X
        bra ClearMenuData
:	LDA DisplayInfo::CurrentChar
        cmp MenuData::CurrentChar
        beq :+
        lda EncounterInfo::IntroFX
        bmi :+		;branch if credits fight
        lda #$0D	;C1 Routine
        jsr CallC1
WaitForever:
        bra WaitForever	;infinite loop?
:	LDA DisplayInfo::CurrentChar
        tax
        lda ControlTarget,X
        beq NoControlTarget
        tay
        lda ActiveParticipants,Y
        beq ClearMenuData
        inc ControlCommand,X
        sec
        lda ControlTarget,X
        sbc #$04
        sta $0E		;monster index of control target
        tay
        lda DisplayInfo::CurrentChar
        tax
        clc
        lda ROMTimes20,X	;size of CharControl struct
        adc MenuData::SelectedItem	;action 0-3
        tax
        lda CharControl::Actions,X
        sta MonsterControlActions,Y
        sec
        lda $0E
        asl
        tax
        lda MenuData::PartyTargets
        sta ForcedTarget::Party,X
        lda MenuData::MonsterTargets
        sta ForcedTarget::Monster,X
ClearMenuData:
        LDA #$80
        STA MenuData::ActionFlag
        STZ MenuData::Command
        STZ MenuData::MonsterTargets
        STZ MenuData::PartyTargets
        STZ MenuData::SelectedItem
        STZ MenuData::SecondActionFlag
        STZ MenuData::SecondCommand
        STZ MenuData::SecondMonsterTargets
        STZ MenuData::SecondPartyTargets
        STZ MenuData::SecondSelectedItem
        BRA CopyCommands
NoControlTarget:
        lda ControllingB
        bne ClearMenuData	;controlling with no target
CopyCommands:
        lda DisplayInfo::CurrentChar
        tax
        stx $2A
        ldx #$028A   	;650, size of CharSpells struct
        stx $2C
        jsr Multiply_16bit	;not using the rom *650 table?
        longa
        clc
        lda $2E		;CurrentChar * 650
        adc #$2D34   	;CharSpells struct location
        sta TempSpellOffset
        shorta0
        ldx AttackerOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$18	;charm/berserk
        bne CheckCommand
        lda MenuData::Command
        sta CharStruct::Command,X
        lda MenuData::MonsterTargets
        sta CharStruct::MonsterTargets,X
        lda MenuData::PartyTargets
        sta CharStruct::PartyTargets,X
        lda MenuData::SelectedItem
        sta CharStruct::SelectedItem,X
        lda MenuData::ActionFlag
        sta CharStruct::ActionFlag,X
        and #$20	;magic
        beq NotXMagic
        lda MenuData::SelectedItem
        tay
        lda (TempSpellOffset),Y
        sta CharStruct::SelectedItem,X
        lda MenuData::ActionFlag
        and #$08	;x-magic
        beq NotXMagic
        lda MenuData::SecondCommand
        sta CharStruct::SecondCommand,X
        lda MenuData::SecondMonsterTargets
        sta CharStruct::SecondMonsterTargets,X
        lda MenuData::SecondPartyTargets
        sta CharStruct::SecondPartyTargets,X
        lda MenuData::SecondSelectedItem
        tay
        lda (TempSpellOffset),Y
        sta CharStruct::SecondSelectedItem,X
        lda MenuData::SecondActionFlag
        sta CharStruct::SecondActionFlag,X
        bra CheckCommand
NotXMagic:
        stz CharStruct::SecondCommand,X
        stz CharStruct::SecondMonsterTargets,X
        stz CharStruct::SecondMonsterTargets,X	;**bug: PartyTargets
        stz CharStruct::SecondSelectedItem,X
        stz CharStruct::SecondActionFlag,X
CheckCommand:
        lda MenuData::Command
        sta $24
        lda #$08
        sta $25
        jsr Multiply_8bit
        ldx $26		;command * 8
        ldy AttackerOffset
        lda ROMAbilityInfo::CmdStatus,X
        sta CharStruct::CmdStatus,Y
        lda ROMAbilityInfo::DamageMod,X
        sta CharStruct::DamageMod,Y
        lda MenuData::Command
        cmp #$2C	;first magic command
        bcc NotMagicCommand
        cmp #$4E	;after last magic command
        bcs NotMagicCommand
        lda CharStruct::ActionFlag,Y
        ora #$01     	;costs MP
        sta CharStruct::ActionFlag,Y
NotMagicCommand:
        lda MenuData::Command
        tax
        lda ROMCommandDelay,X
        bmi CalculateDelay
        pha
        lda MenuData::Command
        cmp #$11	;throw
        beq Item
        cmp #$20	;drink
        beq Item
        cmp #$1F	;mix
        bne NotItem
Mix:	
        lda MenuData::SecondSelectedItem
        pha
        tax
        lda InventoryItems,X
        ldx AttackerOffset
        sta CharStruct::SecondSelectedItem,X
        pla
        jsr ConsumeItem
Item:
        lda MenuData::SelectedItem
        pha
        tax
        lda InventoryItems,X
        ldx AttackerOffset
        sta CharStruct::SelectedItem,X
        pla
        jsr ConsumeItem
NotItem:
        pla
        jmp Finish
CalculateDelay:	
        lda MenuData::ActionFlag
        and #$08	;XMagic
        beq :+
        jmp MagicDelay
:	LDA MenuData::ActionFlag
        and #$40	;Item
        bne ItemDelay
        lda MenuData::ActionFlag
        and #$20	;Magic
        beq :+
        jmp MagicDelay
:	LDA MenuData::ActionFlag
        and #$10	;Weapon used as item
        beq WeaponAttackDelay
        jmp WeaponUseDelay
WeaponAttackDelay:	;despite the calculation, I don't think any weapons have delay values
        stz $0E
        lda DisplayInfo::CurrentChar
        sta $24
        lda #$54     ;84, size of GearStats struct
        sta $25
        jsr Multiply_8bit
        ldy $26
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        beq :+
        lda RHWeapon::Targetting,Y
        and #$03	;delay bits (delay/10)
        tax
        lda ROMTimes10,X
        sta $0E		;attack delay
:	LDX AttackerOffset
        lda CharStruct::LHWeapon,X
        beq :+
        lda !LHWeapon,Y
        and #$03	;delay bits (delay/10)
        tax
        clc
        lda ROMTimes10,X
        adc $0E		;add other weapon's delay
        sta $0E
:	LDA $0E		;attack delay
        jmp Finish
ItemDelay: 
        lda MenuData::SelectedItem
        tax
        lda InventoryItems,X
        ldx AttackerOffset
        sta CharStruct::SelectedItem,X
        sec
        sbc #$E0	;consumable item offset
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        lda ROMConsumables::Misc,X
        and #$08
        bne :+
        lda MenuData::SelectedItem
        jsr ConsumeItem
:	LDA ROMConsumables::Targetting,X
        and #$03	;delay bits (delay/10)
        tax
        lda ROMTimes10,X
        bra Finish
MagicDelay:	
        stz $0E
        lda MenuData::SelectedItem
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        and #$03	;delay bits (delay/10)
        tax
        lda ROMTimes10,X
        sta $0E		;attack delay
        lda MenuData::ActionFlag
        and #$08	;X-Magic
        beq FinishMagic
        lda MenuData::SecondSelectedItem
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        and #$03	;delay bits (delay/10)
        tax
        clc
        lda ROMTimes10,X
        adc $0E		;add other spell's delay
        sta $0E
FinishMagic:
        lda $0E		;attack delay
        bra Finish
WeaponUseDelay:
        lda DisplayInfo::CurrentChar
        sta $24
        lda #$54     	;84, size of GearStats struct
        sta $25
        jsr Multiply_8bit
        ldy $26
        lda MenuData::SelectedItem
        beq :+
        longa
        tya
        clc
        adc #$000C	;shifts offset from RHWeapon to LHWeapon
        tay
        shorta0
:	LDA RHWeapon::ItemMagic,Y	;could be LHWeapon
        and #$7F	;weapon magic to cast
        beq Finish
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        and #$03	;delay bits (delay/10)
        tax
        lda ROMTimes10,X
Finish:		
        pha
        lda DisplayInfo::CurrentChar
        jsr GetTimerOffset	;Y and $36 = timer offset
        ldx AttackerOffset
        pla
        jsr HasteSlowMod	;adjusts delay
        sta CurrentTimer::ATB,Y	;time until action fires
        lda #$41		;flag indicating a queued action
        sta EnableTimer::ATB,Y
        lda #$80		;physical/other
        sta MenuData::ActionFlag
        stz MenuData::Command
        stz MenuData::CurrentChar
        stz MenuData::MonsterTargets
        stz MenuData::PartyTargets
        stz MenuData::SelectedItem
        stz MenuData+7
        stz MenuData::SecondActionFlag
        stz MenuData::SecondCommand
        stz MenuData+10
        stz MenuData::SecondMonsterTargets
        stz MenuData::SecondPartyTargets
        stz MenuData::SecondSelectedItem
        rts

.endproc 	;**optimize: save space with a clear loop or function
; ---------------------------------------------------------------------------

;subtracts 1 from item quantity of item in A
;blanks out inventory slot if qty is now 0
.proc ConsumeItem

_1C36:
        tax
        lda InventoryQuantities,X
        dec
        sta InventoryQuantities,X
        bne Ret
        stz InventoryItems,X
        stz InventoryTargetting,X
        lda #$5A
        sta InventoryFlags,X
        lda #$AA
        sta InventoryUsable,X
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;initializes some values when a battle during the credits happens
;this range is used by C1 graphics code but unsure what it does
.proc SetupCreditsDemo

_1C51:
        lda #$80	;physical/other
        sta MenuData::ActionFlag
        sta MenuData::MonsterTargets
        lda #$54	;job-specific animation (credits)
        sta MenuData::Command
        stz MenuData::PartyTargets
        stz MenuData::SelectedItem
        stz MenuData::SecondActionFlag
        stz MenuData::SecondCommand
        stz MenuData::SecondMonsterTargets
        stz MenuData::SecondPartyTargets
        stz MenuData::SecondSelectedItem
        rts

.endproc
; ---------------------------------------------------------------------------

;returns >0 if character has a status that prevents them from taking Action
.proc CheckDisablingStatus

_1C74:
        ldx AttackerOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$C2   	;dead/stone/zombie
        bne Ret
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78   	;sleep/para/Charm/Berserk
        bne Ret
        lda CharStruct::Status3,X
        and #$10   	;stop
        bne Ret
        lda CharStruct::Status4,X
        and #$84   	;erased/singing
        bne Ret
        tdc
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Make Berserk ability have Berserk Status
.proc ApplyBerserkStatus

_1C9A:
        ldx AttackerOffset
        lda CharStruct::Passives2,X
        and #$08   	;berserk
        beq Finish
        lda EncounterInfo::IntroFX
        bpl NotCredits
Finish:	TDC
        rts
        								;
NotCredits:
        lda CharStruct::AlwaysStatus2,X
        ora #$08   	;berserk
        sta CharStruct::AlwaysStatus2,X
        rts

.endproc
; ---------------------------------------------------------------------------


;Disables Magic and Commands when Status or MP prevents their use
.proc DisableCommandsMagic

_1CB3:
        tdc
        tax
        stx $16
        lda Void
        and #$40     	;void
        beq :+
        ldx #$0080
        stx $16		;disables magic
:	LDA DisplayInfo::CurrentChar
        jsr CalculateSpellOffset	;sets Y
        longa
        tdc
        sta $12
        sta $14
        ldx AttackerOffset
        lda CharStruct::CurMP,X
        sta $0E		;current mp
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        sta $22		;status 3/4
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        sta $10		;status 1/2
        and #$0400	;mute
        beq :+
        lda #$0080
        sta $12		;disables magic
:	LDA CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$0020	;toad
        beq :+
        lda #$0080
        sta $14		;disables magic
:	tdc
        tax
DisableSpells:
        lda CharSpells::Flags,Y
        and #$0001	;skip mp/status checks
        bne NextSpell
        lda CharSpells::MP,Y
        and #$00FF	;clear high part since it's an 8 bit field
        cmp $0E		;current mp
        beq CheckStatus
        bcc CheckStatus
        lda CharSpells::Flags,Y
        ora #$0080
        sta CharSpells::Flags,Y
        bra NextSpell
CheckStatus:
        lda CharSpells::Flags,Y
        and #$FF7F	;clear bit 80h in flags, disabled bit?
        sta CharSpells::Flags,Y
        lda CharSpells::ID,Y
        and #$00FF
        cmp #$0080	;blue magic
        bcs NextSpell
        lda CharSpells::Flags,Y
        ora $12		;from mute
        ora $14		;from toad
        ora $16		;from void
        sta CharSpells::Flags,Y
        lda $16
        bne NextSpell
        lda $12
        bne NextSpell
        lda $14
        beq NextSpell
        lda CharSpells::ID,Y
        and #$00FF
        cmp #$0029	;toad spell
        bne NextSpell
        lda CharSpells::Flags,Y
        and #$FF7F	;re-enable toad spell if toad status
        sta CharSpells::Flags,Y
NextSpell:
        iny
        inx
        cpx #$0082	;130 spell slots
        bne DisableSpells
        shorta0
        lda DisplayInfo::CurrentChar
        sta $24
        lda #$14	;20, size of CharCommands struct
        sta $25
        jsr Multiply_8bit
        tdc
        tax
        stx $0E
        ldy $26
        longa
DisableCommands:
        lda CharCommands::ID,Y
        and #$00FF
        sta $12		;command id
        beq DisableCommand
        asl
        tax
        lda ROMStatusDisableCommands,X
        and $10		;status 1/2
        bne DisableCommand
        lda $12		;command id
        cmp #$0026	;show command
        beq EnableCommand
        lda $22		;status 3/4
        and #$0100	;hidden
        beq EnableCommand
DisableCommand:
        lda CharCommands::Flags,Y
        ora #$0080	;disabled
        sta CharCommands::Flags,Y
        bra NextCommand
EnableCommand:
        lda CharCommands::Flags,Y
        and #$FF7F	;enabled
        sta CharCommands::Flags,Y
NextCommand:
        iny
        inc $0E		;character index
        lda $0E
        cmp #$0004	;4 commands per character
        bne DisableCommands
        shorta0
        rts

.endproc
; ---------------------------------------------------------------------------