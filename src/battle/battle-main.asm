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



; ===========================================================================
;                       Beginning section of commands
;Note about commands:
;Most of FF5 uses a command value between $01-$56, which matches the slots on the combat menu
;this is then mapped to command table values $00-$34 via a rom table
;most of the time the difference is just -1, but all magic types map to $2B 
;everything after $2B shifts down accordingly
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


.endproc

;Command $01 (Other) and $24 (Dummy01)
.proc CommandTable00

_0511:
CommandTable23:
        lda AttackerIndex
        cmp #$04		;monster check
        bcc Party
Monster:	
        sec
        sbc #$04		;now monster index
        asl
        tax
        lda ROMTimes100w,X	;*100
        sta $0E
        lda ROMTimes100w+1,X
        sta $0F
        tdc
        tay
        ldx $0E
CopyGFXQueue:		;copy 100 bytes from monster ai to GFXQueue
        lda !MonsterAIScript,X
        sta !GFXQueue,Y
        inx
        iny
        cpy #$0064
        bne CopyGFXQueue
        bra Continue
Party:
        jsr $98FA ;FindOpenGFXQueueSlot   ;next slot in X
        stz GFXQueue::Flag,X
        stz GFXQueue::Cmd,X
        stz GFXQueue::Type,X
        stz GFXQueue::Data1,X
        stz GFXQueue::Data2,X
Continue:
        lda ProcSequence
        tax
        lda #$7E
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        inc UnknownReaction
        lda ProcSequence
        asl
        tax
        lda #$80		;first party member
        sta CommandTargetBitmask,X
        stz CommandTargetBitmask+1,X
        inc ProcSequence
        rts

.endproc

; ---------------------------------------------------------------------------

;Command $02 (Item) and $20 (Drink)
.proc ItemCommand

_0570:
CommandTable01:
CommandTable1F:
        stz SelectedItem
        jsr $9923 ;SelectCurrentProcSequence	;$0C = ProcSequence*12
        jsr $175F ;GetTargets
        ldx AttackerOffset
        lda CharStruct::ActionFlag,X
        and #$10	;weapon used as item
        beq :+
        jmp WeaponItem
:       lda CharStruct::SelectedItem,X
        cmp #$EF	;magic lamp
        bne ConsumableItem
        jsr $0767 ;PrepMagicLamp
        jmp $0E43 ;ConjureCommand_MagicLamp	;finishes up in Conjure routine
ConsumableItem:	
        sta TempSpell
        sec
        sbc #$E0	;now consumable item index
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        ldy $0C		;ProcSequence*12
        stz $0A
:       lda !ROMConsumables,X
        sta !AttackInfo,Y
        inx
        iny
        inc $0A
        lda $0A
        cmp #$05      	;copy 5 bytes
        bne :-
        iny           	;skip 4 in Attackinfo
        iny
        iny
        iny
:	    lda !ROMConsumables,X
        sta !AttackInfo,Y
        inx
        iny
        inc $0A
        lda $0A
        cmp #$08      	;copy remaining 3 bytes
        bne :-
        jsr CheckMultiTarget
        bne TargetOK
        ldy $0C		;ProcSequence*12
        lda AttackInfo::MagicAtkType,Y
        bpl CheckRetarget
        lda ProcSequence
        tax
        inc HitsInactive,X	;can hit dead targets
        bra TargetOK
CheckRetarget:
        jsr CheckRetarget
TargetOK:
        jsr BuildTargetBitmask
        lda TempSpell
        sta Temp+1
        lda #$04
        sta Temp
        jsr $992F ;GFXCmdAttackNameFromTemp
        jsr $98FA ;FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$09		;command 9: item use
        sta GFXQueue::Type,X
        lda TempSpell
        sta GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        ldy $0C		;ProcSequence*12
        lda ProcSequence
        tax
        lda AttackInfo::MagicAtkType,Y
        and #$7F
        sta AtkType,X
        lda TempTargetting
        sta MultiTarget,X
        beq Single
        inc MultiTarget,X	;now proper number of targets if >1
        lda #$80
Single:	STA TargetType,X
        jsr $1735 ;FinishCommand
        jsr $98E3 ;GFXCmdDamageNumbers
        rts

.endproc

; ---------------------------------------------------------------------------

.proc WeaponItem

_0632:
        stz TempHand
        lda AttackerIndex
        tax
        lda ROMTimes84,X	;size of one character's gear offset
        tay
        ldx AttackerOffset
        lda CharStruct::SelectedItem,X
        bne Left
        lda CharStruct::RHWeapon,X
        sta CharStruct::SelectedItem,X
        sta SelectedItem
        tax
        lda RHWeapon::ItemMagic,Y
        bra ItemReady
Left:	INC TempHand
        lda CharStruct::LHWeapon,X
        sta CharStruct::SelectedItem,X
        sta SelectedItem
        tax
        lda LHWeapon::ItemMagic,Y
ItemReady:
        sta TempItemMagic
        and #$7F
        txy 		;Y is now selected item
        pha
        pha
        lda ProcSequence
        tax
        pla
        sta AtkType,X
        tya
        sta Temp+1	;selected item
        lda #$04
        sta Temp
        jsr $992F ;GFXCmdAttackNameFromTemp
        lda ProcSequence
        tax
        lda AtkType,X
        ldy $0C		;ProcSequence*12
        jsr CopyROMMagicInfo
        jsr CheckMultiTarget
        bne TargetOK
        ldy $0C
        lda AttackInfo::MagicAtkType,Y
        bpl DontRetarget
        lda ProcSequence
        tax
        inc HitsInactive,X
        bra TargetOK
DontRetarget:
        jsr $4AFE ;CheckRetarget
TargetOK:
        jsr BuildTargetBitmask
        jsr $98FA ;FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$0A	;command 10: weapon used as item
        sta GFXQueue::Type,X
        pla
        sta GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        lda ProcSequence
        tax
        lda AtkType,X
        bpl :+
        tdc
        bra :++
:	ldy $0C
        lda AttackInfo::MagicAtkType,Y
        and #$7F
:	sta AtkType,X
        lda TempTargetting
        sta MultiTarget,X
        beq :+
        inc MultiTarget,X
        lda #$80
:	sta TargetType,X
        jsr $1735 ;FinishCommand
        jsr $98E3 ;GFXCmdDamageNumbers
        lda TempItemMagic
        bpl Ret
BreakOnUse:	;80h indicates item should now break
        lda AttackerIndex
        jsr ShiftMultiply::_4
        sta $0E
        asl
        clc
        adc $0E
        tay 		;Attacker index *12
        lda TempHand
        bne Left2
        tdc
        sta HandItems::ID,Y
        sta HandItems::Level,Y
        sta HandItems::MP,Y
        lda #$38
        sta HandItems::Targetting,Y
        lda #$5A
        sta HandItems::Flags,Y
        lda #$AA
        sta HandItems::Usable,Y
        bra DoneHandItems
Left2:	tdc
        sta HandItems::ID+1,Y
        sta HandItems::Level+1,Y
        sta HandItems::MP+1,Y
        lda #$38
        sta HandItems::Targetting+1,Y
        lda #$5A
        sta HandItems::Flags+1,Y
        lda #$AA
        sta HandItems::Usable+1,Y
DoneHandItems:
        lda MessageBoxOffset
        tay
        lda #$50	;item shattered message
        sta MessageBoxes,Y
        lda MessageBoxOffset
        sta $0E
        asl
        clc
        adc $0E
        tax 		;message box index *3
        lda SelectedItem
        sta MessageBoxData,X
        stz MessageBoxData+1,X
        stz MessageBoxData+2,X
        jsr $994C ;GFXCmdMessage
        lda DisplayInfo::CurrentChar
        pha
        lda AttackerIndex
        sta DisplayInfo::CurrentChar
        sta CurrentChar
        jsr $9F3A ;ReplaceHands
        pla
        sta DisplayInfo::CurrentChar
        jsr $9A6F ;ApplyGear
Ret:	rts

.endproc

; ---------------------------------------------------------------------------


.proc PrepMagicLamp

_0767:
        lda BattleData::MagicLamp
        cmp #$0D	;<13, normal
        bcc Continue
        cmp #$20	;>=32, egg chop
        bcs EggChop
        lda #$0D	;chocobo
        bra Continue
EggChop:	
        lda #$0E	;egg chop
Continue:
        tax
        lda ROMMagicLamp,X
        sta TempSpell
        stz TempIsEffect
        clc
        lda BattleData::MagicLamp
        adc #$01
        bcc :+
        lda #$FF	;cap at 255 uses
:	sta BattleData::MagicLamp
        rts

.endproc
; ---------------------------------------------------------------------------

;Command $03 (Row)
.proc CommandTable02

_0791:
        lda #$03	;row ability
        jsr CopyAbilityInfo
        inc UnknownReaction
        lda #$02
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jmp FinishCommandNullTargets

.endproc

; ---------------------------------------------------------------------------

;Command $04 (Defend)
.proc CommandTable03

_07A4:
        lda #$03
        jmp NoActionAbility

.endproc

; ---------------------------------------------------------------------------

;Command $06 (Guard)
.proc CommandTable05

_07A9:
        lda #$05
        jmp NoActionAbility

.endproc

; ---------------------------------------------------------------------------

;Command $07 (Kick)
.proc CommandTable06

_07AE:
        lda #$07	;kick ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        lda #$07	;kick ability name
        jsr GFXCmdAttackNameA
        lda #$06	;kick anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeMultiTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $08 (BuildUp/Focus)
.proc CommandTable07

_07CF:
        lda #$08	;ability name
        jsr GFXCmdAttackNameA
        lda #$07	;ability anim
        jsr GFXCmdAbilityAnim
        lda ProcSequence
        tax
        stz AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommandNullTargets
        ldx AttackerOffset
        lda #$4E	;Command, maps to CommandTable2C/SimpleFight
        sta CharStruct::Command,X
        lda #$10	;double M
        sta CharStruct::DamageMod,X
        lda #$4E
        tax
        lda ROMCommandDelay,X
        pha
        lda AttackerIndex
        jsr GetTimerOffset
        ldx AttackerOffset
        pla
        jsr HasteSlowMod
        sta CurrentTimer::ATB,Y
        lda #$41	;waiting for delayed action
        sta EnableTimer::ATB,Y
        inc DelayedFight
        rts

.endproc

; ---------------------------------------------------------------------------

;Command $4E (Simple Fight)  
;No procs, used by Capture/BuildUp/etc.
.proc SimpleFight

_0814:
CommandTable2C:
        jsr GetTargets
        jsr CheckRetarget
        ldx AttackerOffset
        lda PartyTargets
        sta CharStruct::PartyTargets,X
        lda MonsterTargets
        sta CharStruct::MonsterTargets,X
        jsr BuildTargetBitmask
        lda AttackerIndex
        tax
        lda ROMTimes84,X	;size of combined gear stats struct
        tax
        stx $0E			;gear stats offset
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        bne RH
        jmp LH
RH:
        jsr SelectCurrentProcSequence
        sty $14			;AttackInfo Offset
        stz $12			;loop index
        ldx $0E			;gear stats offset
:	lda !RHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        ldx $0E			;gear stats offset
        lda RHWeapon::Properties,X
        and #$04		;magic sword ok
        bne __MSword
        tdc
        bra __DoneMSword
__MSword:
        ldx AttackerOffset
        lda CharStruct::MSwordAnim,X
        and #$7F		;clear high bit for right hand
__DoneMSword:
        pha
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01		;ability/command animation
        sta GFXQueue::Type,X
        lda #$04		;fight
        sta GFXQueue::Data1,X
        pla 			;magic sword anim
        sta GFXQueue::Data2,X
        ldx $0E
        lda RHWeapon::AtkType,X
        pha
        lda ProcSequence
        tax
        pla
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommand
        jsr GFXCmdDamageNumbers
LH:
        ldx AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        rts 			;no weapons (not even fists)
:	jsr SelectCurrentProcSequence
        sty $12			;AttackInfo Offset
        stz $14			;loop index
        ldx $0E			;gear stats offset
:	lda !LHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $14
        lda $14
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        ldx $0E			;gear stats offset
        lda ProcSequence
        tay
        lda LHWeapon::AtkType,X
        sta AtkType,Y
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01		;ability/command animation
        sta GFXQueue::Type,X
        lda #$04		;fight
        sta GFXQueue::Data1,X
        stx $08			;GFXQueue offset
        ldx $0E			;gear stats offset
        lda LHWeapon::Properties,X
        and #$04		;magic sword ok
        bne __MSword2
        lda #$80		;left hand
        bra __DoneMSword2
__MSword2:
        ldx AttackerOffset
        lda CharStruct::MSwordAnim,X
        ora #$80		;left hand
__DoneMSword2:
        ldx $08			;GFXQueue offset
        sta GFXQueue::Data2,X	;magic sword anim
        lda ProcSequence
        tax
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommand
        jsr GFXCmdDamageNumbers
        rts

.endproc

; ---------------------------------------------------------------------------

;Command $09 (Mantra)
.proc CommandTable08

_090B:
         lda #$09	;mantra ability
         jsr CopyAbilityInfo
         lda #$09	;ability name
         jsr GFXCmdAttackNameA
         lda #$08	;ability anim
         jsr GFXCmdAbilityAnim
         jsr MagicAtkTypeSingleTarget
         jsr FinishCommandNullTargets
         lda ProcSequence	;a second effect?
         tax
         stz AtkType,X		;no attack type
         stz MultiTarget,X
         stz TargetType,X
         jsr FinishCommandNullTargets
         jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $0A (Escape/Flee)
.proc CommandTable09

_0933:
         lda #$0A	;escape ability
         jsr CopyAbilityInfo
         inc UnknownReaction
         lda #$0A	;ability name
         jsr GFXCmdAttackNameA
         lda #$09	;ability anim
         jsr GFXCmdAbilityAnim
         jsr MagicAtkTypeSingleTarget
         jsr FinishCommandNullTargets
         jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $0B (Steal)
.proc CommandTable0A

_094E:
         lda #$0B	;steal ability
         jsr CopyAbilityInfo
         jsr GetTargets
         jsr CheckRetarget
         jsr BuildTargetBitmask
         lda #$0B	;name
         jsr GFXCmdAttackNameA
         lda #$0A	;anim
         jsr GFXCmdAbilityAnim
         jsr MagicAtkTypeSingleTarget
         jsr FinishCommand
         jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $0C (Capture/Mug)
.proc CommandTable0B

_096F:
         stz ProcSequence	;overwriting entire sequence
         stz NextGFXQueueSlot
         lda #$0C		;ability name
         jsr GFXCmdAttackNameA
         jsr SimpleFight
         lda #$0B		;steal ability
         jsr CopyAbilityInfo
         lda #$0A		;steal anim
         jsr GFXCmdAbilityAnim
         jsr MagicAtkTypeSingleTarget
         jsr FinishCommand
         jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $0D (Jump)
.proc CommandTable0C

_0990:
JumpCommand:
         lda #$0D	;jump ability name
         jsr GFXCmdAttackNameA
Anim:			;routine is called here by command $52
         lda #$0C	;jump ability anim (launch)
         jsr GFXCmdAbilityAnim
         lda ProcSequence
         tax
         stz AtkType,X
         stz MultiTarget,X
         stz TargetType,X
         jsr FinishCommandNullTargets
         inc UnknownReaction
         ldx AttackerOffset
         lda #$4F	;jump landing command, maps to CommandTable2D
         sta CharStruct::Command,X
         lda #$10	;jumping
         sta CharStruct::CmdStatus,X
         lda #$80	;auto hit
         sta CharStruct::DamageMod,X
         lda #$4F	;jump landing command
         tax
         lda ROMCommandDelay,X
         pha
         lda AttackerIndex
         jsr GetTimerOffset
         ldx AttackerOffset
         pla
         jsr HasteSlowMod
         sta CurrentTimer::ATB,Y
         lda #$41	;queued action
         sta EnableTimer::ATB,Y
         inc DelayedFight
         rts

.endproc

; ---------------------------------------------------------------------------

;Command $4F (landing from Jump)
.proc CommandTable2D

_09DD:
        lda #$2D	;jump landing anim
        jsr GFXCmdAbilityAnim
        jsr GetTargets
        jsr CheckRetarget
        ldx AttackerOffset
        lda PartyTargets
        sta CharStruct::PartyTargets,X
        lda MonsterTargets
        sta CharStruct::MonsterTargets,X
        jsr BuildTargetBitmask
        lda AttackerIndex
        tax
        lda ROMTimes84,X	;size of combined gearstats struct
        tax
        stx $0E			;gear stats offset
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        bne RH
        jmp LH
RH:	jsr SelectCurrentProcSequence
        stz $12
        ldx $0E
:	lda !RHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        ldx $0E			;gear stats offset
        lda RHWeapon::AtkType,X
        pha
        lda ProcSequence
        tax
        pla
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommand
        jsr GFXCmdDamageNumbers
LH:	LDX AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        jmp Finish
:	jsr SelectCurrentProcSequence
        stz $12
        ldx $0E			;gear stats offset
:	lda !LHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        ldx $0E			;gear stats offset
        lda ProcSequence
        tay
        lda LHWeapon::AtkType,X
        sta AtkType,Y
        lda ProcSequence
        tax
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommand
        jsr GFXCmdDamageNumbers
Finish:	INC UnknownReaction
        rts

.endproc
; ---------------------------------------------------------------------------

;Command $50 (forced Jump landing, via Interceptor Rocket)
.proc CommandTable2E

_0A7D:
        lda #$50		;jump intercepted
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        stz MonsterTargets
        lda AttackerIndex
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets	;target self
        jsr BuildTargetBitmask
        inc UnknownReaction
        lda #$2E		;jump intercepted anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jmp FinishCommand

.endproc

; ---------------------------------------------------------------------------

;Command $0E (Dragon Sword)
.proc CommandTable0D

_0AA4:
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        lda #$0E		;ability name
        jsr GFXCmdAttackNameA
        lda #$0D		;ability anim
        jsr GFXCmdAbilityAnim
        jsr SelectCurrentProcSequence
        lda #$71		;dragon sword hp drain
        jsr CopyROMMagicInfo
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jsr GFXCmdDamageNumbers
        jsr SelectCurrentProcSequence
        lda #$72		;dragon sword mp drain
        jsr CopyROMMagicInfo
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $0F (Smoke)
.proc CommandTable0E

_0ADC:
        lda #$0F		;smoke ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        lda #$0F		;ability name
        jsr GFXCmdAttackNameA
        lda #$0E		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeMultiTarget
        jmp FinishCommand

.endproc

; ---------------------------------------------------------------------------

;Command $10 (Image)
.proc CommandTable0F

_0AFA:
        lda #$10		;image ability
        jsr CopyAbilityInfo
        lda #$10		;ability name
        jsr GFXCmdAttackNameA
        lda #$0F		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jmp FinishCommandNullTargets

.endproc

; ---------------------------------------------------------------------------

;Command $11 (Throw)
;**optimize: use SandwormBattle instead of checking encounter
.proc CommandTable10

_0B0F:
        ldx AttackerOffset
        lda CharStruct::SelectedItem,X
        bpl Weapon		;otherwise, a scroll
        tdc
        tax
        stz $0E			;target bits
TargetActiveMonsters:
        lda ActiveParticipants+4,X
        beq Next
        lda $0E
        jsr SetBit_X  		;add as target if active
        sta $0E
Next:	INX
        cpx #$0008		;8 monsters
        bne TargetActiveMonsters
        ldx AttackerOffset
        lda $0E			;target bits
        sta CharStruct::MonsterTargets,X
        lda EncounterIndex+1
        beq ItemFlag
        lda EncounterIndex
        cmp #$BF		;sandworm fight
        bne ItemFlag
        lda $0E
        and #$FC		;exclude "real" sandworm
        sta CharStruct::MonsterTargets,X
ItemFlag:
        lda #$40		;item
        sta CharStruct::ActionFlag,X
        jmp ItemCommand
Weapon:
        lda #$11		;throw ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$11		;ability name
        jsr GFXCmdAttackNameA
        lda #$10		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

.proc SwordSlapCommand

_0B6F:
;Command $12 (Sword Slap)
;sets a variable which is never checked then issues a regular fight command
CommandTable11:
        lda #$12		;ability name
        jsr GFXCmdAttackNameA
        inc SwordSlap		;not checked anywhere
        jmp FightCommand

.endproc

; ---------------------------------------------------------------------------

;Command $13 (Gil Toss)
.proc CommandTable12

_0B7A:
        lda #$13		;gil toss ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        lda #$13		;ability name
        jsr GFXCmdAttackNameA
        lda #$12		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeMultiTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $14 (Slash)
.proc CommandTable13

_0B9B:
        lda #$14	;slash ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        inc UnknownReaction
        lda #$14	;ability name
        jsr GFXCmdAttackNameA
        lda #$13	;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeMultiTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $15 (Animals)
;**optimize: rewrite to remove repeated BRA's 
.proc CommandTable14

_0BBF:
        tdc
        tax
        lda Level
        jsr Random_X_A		;0..Level
        bne :+
        tdc 		;0 mystidian rabbit
        bra Chosen
:	CMP #$05
        bcs :+
        lda #$01	;<5 squirrel
        bra Chosen
:	CMP #$0A
        bcs :+
        lda #$02	;<10 bee swarm
        bra Chosen
:	CMP #$14
        bcs :+
        lda #$03	;<20 nightingale
        bra Chosen
:	CMP #$1E
        bcs :+
        lda #$04	;<30 momonga
        bra Chosen
:	CMP #$28
        bcs :+
        lda #$05	;<40 falcon
        bra Chosen
:	CMP #$32
        bcs :+
        lda #$06	;<50 skunk
        bra Chosen
:	CMP #$3C
        bcs :+
        lda #$07	;<60 wild boar
        bra Chosen
:	LDA #$08	;otherwise unicorn
Chosen:
        sta TempSpell
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        tdc
        tay
:	LDA !ROMEffectInfo,X
        sta Temp,Y
        inx
        iny
        cpy #$0008	;copy 8 bytes magic info
        bne :-
        stz PartyTargets
        stz MonsterTargets
        lda Temp	;targetting byte in magic info
        bne Targetting
        lda AttackerIndex
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets	;default to targetting self
        bra TargetSet
Targetting:
        and #$08		;target enemy
        bne TargetEnemy
        lda #$F0
        sta PartyTargets	;entire party
        bra TargetSet
TargetEnemy:
        lda Temp
        and #$40		;hits all targets
        bne TargetAll
        tdc
        tax
        lda #$07
        jsr Random_X_A    	;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
        bra TargetSet
TargetAll:
        lda #$FF
        sta MonsterTargets
TargetSet:
        stz TempAttachedSpell	;CastSpell routine params
        stz TempSkipNaming
        lda #$01		;animals are effect magic
        sta TempIsEffect
        jmp CastSpell

.endproc

; ---------------------------------------------------------------------------

;Command $16 (Aim)
.proc CommandTable15

_0C67:
        lda #$16		;ability name
        jsr GFXCmdAttackNameA
        jmp FightCommand

.endproc

; ---------------------------------------------------------------------------

;Command $17 (X-Fight)
;**optimize: 	lots to trim in the targetting code
;		could also use BuildTargetBitmask instead of duplicating all its code here	
.proc CommandTable16

_0C6F:
        lda #$17		;ability name
        jsr GFXCmdAttackNameA
        stz $22			;index for attack loop
AttackLoop:
        tdc
        tax
        lda #$07
        jsr Random_X_A 		;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        ldx AttackerOffset
        sta CharStruct::MonsterTargets,X
        stz CharStruct::PartyTargets,X
        ldx AttackerOffset
        lda CharStruct::MonsterTargets,X
        sta MonsterTargets
        lda CharStruct::PartyTargets,X
        sta PartyTargets
        jsr CheckRetarget
        ldx AttackerOffset
        lda PartyTargets
        sta CharStruct::PartyTargets,X
        lda MonsterTargets
        sta CharStruct::MonsterTargets,X
        pha
        and #$F0
        lsr
        lsr
        lsr
        lsr
        ora CharStruct::PartyTargets,X
        sta TempTargetBitmask
        pla
        and #$0F
        asl
        asl
        asl
        asl
        sta TempTargetBitmask+1
        lda AttackerIndex
        tax
        lda ROMTimes84,X	;combined size of gearstats structs
        tax
        stx $0E			;gearstats offset
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        bne RH
        jmp LH
RH:	JSR SelectCurrentProcSequence
        sty $14			;AttackInfo offset
        stz $12
        ldx $0E			;gearstats offset
:	LDA !RHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01		;ability/command anim
        sta GFXQueue::Type,X
        lda #$04		;fight
        sta GFXQueue::Data1,X
        stz GFXQueue::Data2,X	;right hand, no msword anim
        ldx $0E			;gearstats offset
        lda RHWeapon::AtkType,X
        pha
        lda ProcSequence
        tax
        pla
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        inc ProcSequence
        jsr GFXCmdDamageNumbers
LH:	LDX AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        jmp Finish
:	JSR SelectCurrentProcSequence
        sty $12
        stz $14
        ldx $0E		;gearstats offset
:	LDA !LHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $14
        lda $14
        cmp #$0C	;copy 12 bytes weapon data
        bne :-
        ldx $0E		;gearstats offset
        lda ProcSequence
        tay
        lda LHWeapon::AtkType,X
        sta AtkType,Y
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01		;ability/command anim
        sta GFXQueue::Type,X
        lda #$04		;fight
        sta GFXQueue::Data1,X
        lda #$80		;left hand, no msword anim
        sta GFXQueue::Data2,X
        lda ProcSequence
        tax
        stz MultiTarget,X
        stz TargetType,X
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        inc ProcSequence
        jsr GFXCmdDamageNumbers
Finish:	inc $22			;attack loop index
        lda $22
        cmp #$04		;4 attacks
        beq Ret
        jmp AttackLoop
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Command $19 (Observe)
.proc CommandTable18

_0DA2:
        lda #$19		;observe ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$19		;ability name
        jsr GFXCmdAttackNameA
        lda #$18		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $1A (Analyze)
.proc CommandTable19

_0DC3:
        lda #$1A		;observe ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$1A		;ability name
        jsr GFXCmdAttackNameA
        lda #$19		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $18 (Conjure)
.proc CommandTable17

_0DE4:
ConjureCommand:
        lda MagicBits+10	;2nd byte of summons
        and #$FE		;last bit is a song
        ora MagicBits+9		;1st byte of summons
        bne PickRandomSummon
;no summons known
        lda #$18
        jsr GFXCmdAttackNameA
        lda MessageBoxOffset
        tax
        lda #$1D		;message
        sta MessageBoxes,X
        lda ProcSequence
        tax
        lda #$7E		;always miss
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        lda #$0D		;ability animation
        jsr GFXCmdAbilityAnim
        jsr FinishCommandNullTargets
        jsr GFXCmdMessage
        jmp Ret
PickRandomSummon:
        tdc
        tax
        stx $0E
        lda #$0E
        jsr Random_X_A    	;0..14
        clc
        adc #$48		;offset of first summon
        sta TempSpell
        stz TempIsEffect
        lsr
        ror $0E
        lsr
        ror $0E
        lsr
        ror $0E
        tay 			;MagicBits offset
        lda $0E
        jsr ShiftDivide::_32
        tax 			;MagicBits spell
        lda MagicBits,Y
        jsr SelectBit_X
        beq PickRandomSummon	;don't know this one, try again
MagicLamp:			;Magic Lamp use jumps in here
        stz PartyTargets
        stz MonsterTargets
        lda TempSpell
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        tdc
        tay
:	    LDA !ROMMagicInfo,X
        sta Temp,Y
        inx
        iny
        cpy #$0008		;copy 8 bytes magic data
        bne :-
        lda Temp		;targetting
        bne FindTargets
;no targetting data, target self
        lda AttackerIndex
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets
        bra TargetSet
FindTargets:
        and #$40		;hits all
        bne TargetAll
        lda Temp		;targetting
        and #$08		;enemy by default
        bne SingleEnemy
SingleAlly:			;hardcoded for phoenix, targets first dead ally
        tdc
        tax
        tay
:	    lDA CharStruct::Status1,X
        and #$80		;dead
        bne DeadAlly
        jsr NextCharOffset
        iny
        cpy #$0004		;4 chars
        bne :-
        lda #$80		;defaults to first member if none dead
        bra SetAlly
DeadAlly:
        tyx
        tdc
        jsr SetBit_X
SetAlly:	
        sta PartyTargets	;target single dead ally
        bra TargetSet
SingleEnemy:
        tdc
        tax
        lda #$07
        jsr Random_X_A 		;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
        bra TargetSet
TargetAll:
        lda Temp		;targetting
        and #$08		;enemy by default
        bne AllEnemy
        lda #$F0
        sta PartyTargets	;all allies
        bra TargetSet
AllEnemy:
        lda #$FF
        sta MonsterTargets	;all enemies
TargetSet:
        stz TempAttachedSpell	;params for CastSpell
        stz TempSkipNaming
        jsr CastSpell
        lda TempAttachedSpell
        beq Ret
        lda TempAttachedSpell	;second spell, for phoenix summon
        sta TempSpell
        stz TempIsEffect
        lda TempMonsterTargets
        sta MonsterTargets
        lda TempPartyTargets
        sta PartyTargets
        inc TempSkipNaming	;2nd spell has no label and diff anim
        jsr CastSpell
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Command $1B (Tame)
.proc CommandTable1A

_0EE0:
        lda #$1B		;tame ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$1B		;ability name
        jsr GFXCmdAttackNameA
        lda #$1A		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jmp FinishCommand

.endproc

; ---------------------------------------------------------------------------

;Command $1C (Control)
.proc CommandTable1B

_0EFE:
        lda #$1C		;control ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$1C		;ability name
        jsr GFXCmdAttackNameA
        lda #$1B		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $1D (Catch)
.proc CommandTable1C

_0F1F:
        lda #$1D		;catch ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$1D		;ability name
        jsr GFXCmdAttackNameA
        lda #$1C		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $1E (Release)
.proc CommandTable1D

_0F40:
        ldx AttackerOffset
        lda CharStruct::CaughtMonster,X
        sta ReleasedMonsterID
        pha
        lda #$FF		;no monster caught
        sta CharStruct::CaughtMonster,X
        pla
        tax
        lda ROMMonsterReleaseActions,X
        sta TempSpell
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        tdc
        tay
:  	    LDA !ROMMagicInfo,X
        sta !TempMagicInfo,Y
        inx
        iny
        cpy #$0008		;8 bytes magic data
        bne :-
        jsr SelectCurrentProcSequence
        tdc
        tax
:	    LDA !TempMagicInfo,X
        sta !AttackInfo,Y
        inx
        iny
        cpx #$0005		;copy first 5 bytes
        bne :-
        iny 			;increment dest pointer by 4
        iny
        iny
        iny
:	    LDA !TempMagicInfo,X
        sta !AttackInfo,Y
        inx
        iny
        cpx #$0008		;then copy remaining 3 bytes
        bne :-
        stz MonsterTargets
        stz PartyTargets
        lda TempMagicInfo::Targetting
        bne Targetting
        lda AttackerIndex
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets	;default to attacker if no targetting
        bra TargetSet
Targetting:
        and #$40		;all targets
        bne TargetAll
        lda TempMagicInfo::Targetting
        and #$08		;enemy by default
        bne TargetEnemy
TargetParty:
        tdc
        tax
        lda #$03
        jsr Random_X_A		;0..3 random party
        cmp AttackerIndex
        beq TargetParty	;pick again if attacker chosen
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets
        bra TargetSet
TargetEnemy:
        tdc
        tax
        lda #$07
        jsr Random_X_A	      	;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
        bra TargetSet
TargetAll:
        lda TempMagicInfo::Targetting
        and #$08		;enemy by default
        bne TargetAllEnemy
        lda #$F0
        sta PartyTargets
        bra TargetSet
TargetAllEnemy:
        lda #$FF
        sta MonsterTargets
TargetSet:
        jsr CheckMultiTarget
        bne Multi
        lda TempMagicInfo::AtkType
        bpl CheckRetarget
        lda ProcSequence
        tax
        inc HitsInactive,X
        bra TargetOK
CheckRetarget:
        jsr CheckRetarget
        bra TargetOK
Multi:
        jsr RemoveInactiveTargets
        jsr CheckMultiTarget
TargetOK:
        jsr BuildTargetBitmask
        lda TempSpell
        sta Temp+1		;attack id
        stz Temp		;string table 0
        jsr GFXCmdAttackNameFromTemp
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Data2,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$00		;attack animation
        sta GFXQueue::Type,X
        lda Temp+1		;attack id
        sta GFXQueue::Data1,X
        lda ProcSequence
        tax
        lda TempMagicInfo::AtkType
        and #$7F		;remove flag bit
        sta AtkType,X
        lda TempTargetting	;number of targets minus 1
        sta MultiTarget,X
        beq :+
        inc MultiTarget,X	;number of targets (but 1 target -> 0)
        lda #$80		;multi target
:	    STA TargetType,X
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
        jsr GFXCmdDamageNumbers
        lda AttackerIndex
        sta $24
        lda #$14
        sta $25
        jsr Multiply_8bit     	;Index * 20, size of CharCommands
        tdc
        tay
        ldx $26			;CharCommands offset
FindCmd:
        lda CharCommands::ID,X
        cmp #$1E		;release
        beq Found
        inx
        iny
        cpy #$0004		;4 command slots
        bne FindCmd
        beq CopyStats
Found:
        lda #$1D		;catch
        sta CharCommands::ID,X
        lda #$28		;target selectable + enemy default
        sta CharCommands::Targetting,X
        inx
        iny
        bra FindCmd		;keep going, could have multiple copies
CopyStats:	;backs up attacker's stats so it can load monster stats instead, they will be restored later
        ldx AttackerOffset
        lda CharStruct::Level,X
        sta SavedCharStats::Level
        lda CharStruct::MonsterAttack,X
        sta SavedCharStats::MonsterAttack
        lda CharStruct::MonsterM,X
        sta SavedCharStats::MonsterM
        lda CharStruct::EquippedMag,X
        sta SavedCharStats::EquippedMag
        lda CharStruct::CharRow,X
        sta SavedCharStats::CharRow
        tdc
        tay
CopyStatus:
        lda CharStruct::Status1,X
        sta SavedCharStats::Status1,Y
        stz CharStruct::Status1,X	;clear status for released mon
        inx
        iny
        cpy #$0009		;9 bytes of status/passives
        bne CopyStatus
        tdc
        tay
        ldx AttackerOffset
CopyMSword:
        lda CharStruct::MSwordElemental1,X
        sta SavedCharStats::MSwordElemental1,Y
        stz CharStruct::MSwordElemental1,X	;clear all msword
        inx
        iny
        cpy #$0006		;6 bytes msword elements/status
        bne CopyMSword
        tdc
        tay
        ldx AttackerOffset
CopyMisc:
        lda CharStruct::AlwaysStatus1,X
        sta SavedCharStats::AlwaysStatus1,Y
        stz CharStruct::AlwaysStatus1,X
        inx
        iny
        cpy #$000B		;11 bytes always status/bonuses/etc
        bne CopyMisc
        lda ReleasedMonsterID
        longa
        jsr ShiftMultiply::_32
        tax
        shorta0
        ldy AttackerOffset
        lda CharStruct::CharRow,Y
        and #$7F		;always front row
        sta CharStruct::CharRow,Y
        lda ROMMonsterStats::Level,X
        sta CharStruct::Level,Y
        lda ROMMonsterStats::AttackPower,X
        sta CharStruct::MonsterAttack,Y
        lda ROMMonsterStats::AttackMult,X
        sta CharStruct::MonsterM,Y
        lda ROMMonsterStats::MagicPower,X
        sta CharStruct::EquippedMag,Y
        lda #$01
        sta WasMonsterReleased	;causes stats to be restored later
        rts

.endproc

; ---------------------------------------------------------------------------

;Command $1F(Combine/Mix)
.proc CommandTable1E

_1125:
        ldx AttackerOffset
        lda CharStruct::MonsterTargets,X
        sta MonsterTargets
        lda CharStruct::PartyTargets,X
        sta PartyTargets
        lda CharStruct::SelectedItem,X
        sec
        sbc #$E0	;remove consumable item offset from item id
        tax
        stx $0E		;first item consumable index
        ldx AttackerOffset
        lda CharStruct::SecondSelectedItem,X
        sec
        sbc #$E0	;remove consumable item offset from item id
        sta $24
        lda #$0C
        sta $25
        jsr Multiply_8bit    	;item*12
        longa
        clc
        lda $26			;second item * 12
        adc $0E			;+ first item
        tax
        shorta0
        lda ROMCombineSpells,X
        sta TempSpell
        stz TempAttachedSpell
        stz TempSkipNaming
        lda #$01
        sta TempIsEffect
        jmp CastSpell

.endproc

; ---------------------------------------------------------------------------

;Command $21 (Pray/Recover)
.proc CommandTable20

_1169:
        lda #$21		;recover ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        lda #$21		;ability name
        jsr GFXCmdAttackNameA
        lda #$20		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeMultiTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $22 (Revive)
.proc CommandTable21

_118A:
        lda #$22		;revive ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        lda ProcSequence
        tax
        inc HitsInactive,X
        lda #$22		;ability name
        jsr GFXCmdAttackNameA
        lda #$21		;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeMultiTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers

.endproc

; ---------------------------------------------------------------------------

;Command $23 (Gaia/Terrain)
.proc CommandTable22

_11B2:
        tdc
        tax
        lda Level
        jsr Random_X_A 	;0..Level
        cmp #$0B
        bcs :+
        tdc          	;<11, 0
        bra Chosen
:	    CMP #$15
        bcs :+
        lda #$01     	;<21, 1
        bra Chosen
:	    CMP #$33
        bcs :+
        lda #$02     	;<50, 2
        bra Chosen
:	    LDA #$03     	;otherwise 3
Chosen:
        sta $0E		;terrain spell slot 0-3
        lda TerrainType
        jsr ShiftMultiply::_4
        clc
        adc $0E
        tax
        lda ROMTerrainSpells,X
        sta TempSpell
        lda TempSpell	;pointless load?
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        tdc
        tay
:	    LDA !ROMEffectInfo,X
        sta Temp,Y
        inx
        iny
        cpy #$0008	;copy 8 bytes spell data
        bne :-
        stz PartyTargets
        stz MonsterTargets
        lda Temp		;targetting byte
        bne Targetting
        lda AttackerIndex	;default to attacker
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets
        bra TargetSet
Targetting:
        and #$08	;target enemy by default
        bne TargetEnemy
        lda Temp
        and #$40	;target all
        bne TargetAllParty
        tdc
        tax
        lda #$03
        jsr Random_X_A 	;0..3 random party
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets
        bra TargetSet
TargetAllParty:
        lda #$F0
        sta PartyTargets
        bra TargetSet
TargetEnemy:
        lda Temp
        and #$40	;target all
        bne TargetAllEnemy
        tdc
        tax
        lda #$07	;0..7 random monster
        jsr Random_X_A
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
        bra TargetSet
TargetAllEnemy:
        lda #$FF
        sta MonsterTargets
TargetSet:
        stz TempAttachedSpell
        stz TempSkipNaming
        lda #$01
        sta TempIsEffect
        jmp CastSpell

.endproc

; ---------------------------------------------------------------------------

;Command $25 (Hide)
.proc CommandTable24

_125E:
        ldx AttackerOffset
        lda CharStruct::Status4,X
        ora #$01	;hidden
        sta CharStruct::Status4,X
        lda #$25	;hide ability name
        jsr GFXCmdAttackNameA
        lda #$24	;hide ability anim
        jsr GFXCmdAbilityAnim
        lda ProcSequence
        tax
        stz AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        inc UnknownReaction
        jsr FinishCommandNullTargets
        lda AttackerIndex
        sta $24
        lda #$14	;20, size of CharCommands struct
        sta $25
        jsr Multiply_8bit
        tdc
        tay
        ldx $26
FindHideCommands:
        lda CharCommands::ID,X
        cmp #$25	;hide command
        beq Found
        inx
        iny
        cpy #$0004	;4 command slots
        bne FindHideCommands
        beq Ret
Found:	LDA #$26	;show command
        sta CharCommands::ID,X
        lda #$08	;target enemy?
        sta CharCommands::Targetting,X
        inx
        iny
        bra FindHideCommands
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Command $26 (Show)
.proc CommandTable25

_12B3:
        ldx AttackerOffset
        lda CharStruct::Status4,X
        and #$FE	;clear hidden
        sta CharStruct::Status4,X
        lda #$26	;show ability name
        jsr GFXCmdAttackNameA
        lda #$25	;show ability anim
        jsr GFXCmdAbilityAnim
        lda ProcSequence
        tax
        stz AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        inc UnknownReaction
        jsr FinishCommandNullTargets
        lda AttackerIndex
        sta $24
        lda #$14	;20, size of CharCommands struct
        sta $25
        jsr Multiply_8bit
        tdc
        tay
        ldx $26
FindShowCommands:
        lda CharCommands::ID,X
        cmp #$26	;show command
        beq Found
        inx
        iny
        cpy #$0004	;4 command slots
        bne FindShowCommands
        beq Ret
Found:
        lda #$25
        sta CharCommands::ID,X
        stz CharCommands::Targetting,X
        inx
        iny
        bra FindShowCommands
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Command $29 (Flirt)
.proc CommandTable28

_1306:
        lda #$29	;flirt ability
        jsr CopyAbilityInfo
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        lda #$29	;ability name
        jsr GFXCmdAttackNameA
        lda #$28	;ability anim
        jsr GFXCmdAbilityAnim
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jsr GFXCmdDamageNumbers
        lda MessageBoxOffset
        tax
        lda #$27	;message
        sta MessageBoxes,X
        jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $2A (Dance)
.proc CommandTable29

_1333:
        stz ProcSequence	;reset command sequence (no procs)
        stz NextGFXQueueSlot
        tdc
        tax
        lda #$03
        jsr Random_X_A		;0..3 random dance
        sta TempDance
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$04		;sword dance up
        beq DanceCheck
        jsr Random_0_99
        lsr
        bcs NotSwordDance	;50% chance of sword dance
        lda #$03		;sword dance
        sta TempDance
        bra DanceCheck
NotSwordDance:
        jsr Random_0_99
        lsr
        stz TempDance
        rol TempDance		;50% chance of 0 or 1
DanceCheck:
        lda TempDance
        cmp #$03		;sword dance
        beq SwordDance
        jsr GetTargets
        jsr CheckRetarget
        jsr BuildTargetBitmask
        jsr CheckMultiTarget
        clc
        lda TempDance
        adc #$79		;offset to dance strings
        sta Temp+1		;string id / ability id
        stz Temp		;string table
        jsr GFXCmdAttackNameFromTemp
        lda #$29		;dance anim
        jsr GFXCmdAbilityAnim
        jsr SelectCurrentProcSequence
        lda Temp+1		;ability id
        jsr CopyROMMagicInfo
        jsr MagicAtkTypeSingleTarget
        jsr FinishCommand
        jmp GFXCmdDamageNumbers
SwordDance:
        lda #$7D		;sword dance ability
        sta Temp+1		;string id / ability id
        stz Temp		;string table
        jsr GFXCmdAttackNameFromTemp
        lda #$29		;dance anim
        jsr GFXCmdAbilityAnim
        lda ProcSequence
        tax
        lda #$7F		;do nothing
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommandNullTargets
        jsr GFXCmdDamageNumbers
        ldx AttackerOffset
        lda CharStruct::DamageMod,X
        ora #$D0		;auto hit, damage*2, M*2
        sta CharStruct::DamageMod,X
        jmp SimpleFight

.endproc

; ---------------------------------------------------------------------------

;Command $2B (Mimic)
.proc CommandTable2A

_13CE:
        tdc
        tay
        ldx AttackerOffset
:	    LDA !SavedAction,Y
        sta CharStruct::ActionFlag,X
        inx
        iny
        cpy #$000A	;copy 10 bytes action data
        bne :-
        ldx AttackerOffset
        lda CharStruct::ActionFlag,X
        and #$FE	;clear "costs mp" bit
        sta CharStruct::ActionFlag,X
        lda CharStruct::Command,X
        jmp DispatchCommand_CommandReady

.endproc

; ---------------------------------------------------------------------------

;Command $51
;Flirt Throbbing
;(null command with a message)
.proc CommandTable2F

_13EF:
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Cmd,X
        stz GFXQueue::Type,X
        stz GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        lda ProcSequence
        tax
        stz AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        jsr FinishCommandNullTargets
        lda MessageBoxOffset
        tax
        lda #$28	;message to display
        sta MessageBoxes,X
        jmp GFXCmdMessage

.endproc

; ---------------------------------------------------------------------------

;Command $52 
;Jump with a different name
.proc CommandTable30

_141D:
        lda #$52	;command name
        jsr GFXCmdAttackNameA
        jmp JumpCommand_Anim

.endproc

; ---------------------------------------------------------------------------

;Command $53
;Handles weapons that cast effect spells
;Wind Slash by default, but can be called mid-routine for other effects like Earthquake
.proc CommandTable31

_1425:
        lda #$4B	;wind slash spell effect
        sta TempEffect
WeaponEffectCommand:	;called here for other weapon effects
        stz $0E		;hand
        stz NextGFXQueueSlot
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        bne :+
        lda #$80	;left hand
        sta $0E
:	    JSR FindOpenGFXQueueSlot
        stz GFXQueue::Flag
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd
        lda #$01	;ability/command anim
        sta GFXQueue::Type
        lda #$04	;fight
        sta GFXQueue::Data1
        lda $0E		;hand (0 for RH, 80 for LH)
        sta GFXQueue::Data2
        lda #$7E	;always miss
        sta AtkType
        stz MultiTarget
        stz TargetType
        stz CommandTargetBitmask
        stz CommandTargetBitmask+1
        inc ProcSequence
        jsr GFXCmdDamageNumbers
        lda #$FF
        sta wMonsterTargets	;**optimize: wasted bytes
        stz wPartyTargets
        lda TempEffect
        sta TempSpell
        lda #$01
        sta TempIsEffect
        sta TempSkipNaming
        stz TempAttachedSpell
        jsr CastSpell
        ldx AttackerOffset
        lda CharStruct::Command,X
        cmp #$0C	;capture/mug
        bne Ret	;removes return address from stack for capture
        plx 		;likely unreachable since capture cancels procs
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Command $54
;Job-specific attack animation
;likely for credits demo?
.proc CommandTable32

_1490:
        ldx AttackerOffset
        clc
        lda CharStruct::Job,X
        adc #$30
        jsr GFXCmdAbilityAnim
        lda #$7F	;null attack
        sta AtkType
        stz MultiTarget
        stz TargetType
        lda #$08	;first monster
        sta CommandTargetBitmask
        sta TargetBitmask
        stz CommandTargetBitmask+1
        stz TargetBitmask+1
        inc UnknownReaction
        rts

.endproc

; ---------------------------------------------------------------------------

;Command $55
;for Double Lance, attacks twice per hand if hand's weapon has this command proc 
.proc CommandTable33

_14B8:
        stz ProcSequence	;cancels any other procs
        stz NextGFXQueueSlot
        ldx AttackerOffset
        lda CharStruct::MonsterTargets,X
        sta MonsterTargets
        lda CharStruct::PartyTargets,X
        sta PartyTargets
        jsr CheckRetarget
        ldx AttackerOffset
        lda PartyTargets
        sta CharStruct::PartyTargets,X
        lda MonsterTargets
        sta CharStruct::MonsterTargets,X
        pha
        and #$F0
        lsr
        lsr
        lsr
        lsr
        ora CharStruct::PartyTargets,X
        sta TempTargetBitmask
        pla
        and #$0F
        asl
        asl
        asl
        asl
        sta TempTargetBitmask+1
        lda AttackerIndex
        tax
        lda ROMTimes84,X	;size of one character's gear structs
        tax
        stx $0E			;GearStruct offset
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        bne RH
        jmp LH
RH:	    JSR SelectCurrentProcSequence
        sty $14
        stz $12
        ldx $0E
:      	LDA !RHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01		;ability/command animation
        sta GFXQueue::Type,X
        lda #$04		;fight
        sta GFXQueue::Data1,X
        stz GFXQueue::Data2,X	;right hand, no msword
        ldx $0E			;GearStruct offset
        lda RHWeapon::AtkType,X
        pha
        lda ProcSequence
        tax
        pla
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        inc ProcSequence
        jsr GFXCmdDamageNumbers
        jsr SelectCurrentProcSequence
        sty $14
        stz $12
        ldx $0E			;GearStruct offset
        lda RHWeapon::Properties,X
        and #$02		;command instead of attack
        beq LH
        lda RHWeapon::Param3,X
        cmp #$55		;this command
        bne LH
:	    LDA !RHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C		;copy 12 bytes data for 2nd attack
        bne :-
        lda #$80
        sta ActionAnimShift	;flag for later anim manipulation
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Cmd,X
        stz GFXQueue::Type,X
        stz GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        ldx $0E			;GearStruct offset
        lda RHWeapon::AtkType,X
        pha
        lda ProcSequence
        tax
        pla
        sta AtkType,X
        stz MultiTarget,X
        stz TargetType,X
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        inc ProcSequence
        jsr GFXCmdDamageNumbers
LH:	    LDX AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        jmp Ret
:       JSR SelectCurrentProcSequence
        sty $12
        stz $14
        ldx $0E			;GearStruct offset
:   	LDA !LHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $14
        lda $14
        cmp #$0C		;copy 12 bytes weapon data
        bne :-
        ldx $0E			;GearStruct offset
        lda ProcSequence
        tay
        lda LHWeapon::AtkType,X
        sta AtkType,Y
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC		;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01		;command/ability anim
        sta GFXQueue::Type,X
        lda #$04		;fight
        sta GFXQueue::Data1,X
        lda #$80		;left hand, no msword
        sta GFXQueue::Data2,X
        lda ProcSequence
        tax
        stz MultiTarget,X
        stz TargetType,X
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        inc ProcSequence
        jsr GFXCmdDamageNumbers
        jsr SelectCurrentProcSequence
        sty $12
        stz $14
        ldx $0E
        lda LHWeapon::Properties,X
        and #$02		;command instead of attack
        beq Ret
        lda LHWeapon::Param3,X
        cmp #$55		;this command
        bne Ret
:	    LDA !LHWeapon,X
        sta !AttackInfo,Y
        inx
        iny
        inc $14
        lda $14
        cmp #$0C		;copy 12 bytes weapon data for 2nd atk
        bne :-
        lda ActionAnimShift
        ora #$40
        sta ActionAnimShift
        ldx $0E
        lda ProcSequence
        tay
        lda LHWeapon::AtkType,X
        sta AtkType,Y
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Cmd,X
        stz GFXQueue::Type,X
        stz GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        lda ProcSequence
        tax
        stz MultiTarget,X
        stz TargetType,X
        lda ProcSequence
        asl
        tax
        lda TempTargetBitmask
        sta CommandTargetBitmask,X
        lda TempTargetBitmask+1
        sta CommandTargetBitmask+1,X
        inc ProcSequence
        jsr GFXCmdDamageNumbers
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Command $56
;Earthquake weapon effect
.proc CommandTable34

_16A2:
        lda #$4A	;earthquake
        sta TempEffect
        jmp WeaponEffectCommand

.endproc

; ===========================================================================

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

;Manage the ATB timer used for zombie/charm/berserk party members
;and set up their action when it is ready
;**bug: should probably check for death too (this is why berserkers always attack when they get up)
.proc HandleUncontrolledParty

_1DC4:
        tdc
        tax
        stx $3D		;char index, used in subroutines also
        stx $3F		;char offset
Loop:
        ldx $3D
        lda UncontrolledATB,X
        beq ActionReady
        ldx $3F
        lda CharStruct::Status3,X
        and #$10	;stop
        bne Next
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$60	;sleep/paralyze
        bne Next
        ldx $3D
        dec UncontrolledATB,X
        bra Next
ActionReady:
        ldx $3F		;char offset
        lda #$01
        sta CharStruct::CmdCancelled,X
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$02	;zombie
        beq :+
        jsr ZombieAction
        bra Next
:	LDA CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$10	;charm
        beq :+
        jsr CharmAction
        bra Next
:	LDA CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$08	;berserk
        beq Next
        jsr BerserkAction
Next:
        ldx $3F		;char offset
        jsr NextCharOffset
        stx $3F
        inc $003D	;char index
        lda $003D
        cmp #$04	;4 characters
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

;Param X = Char Offset, $3D = Char index
;sets up a fight command targetting a random party member
.proc ZombieAction

_1E2F:
        lda #$80
        sta CharStruct::ActionFlag,X
        lda #$05	;fight
        sta CharStruct::Command,X
        stz CharStruct::MonsterTargets,X
        stz CharStruct::SelectedItem,X
        stz CharStruct::SecondActionFlag,X
        stz CharStruct::SecondCommand,X
        stz CharStruct::SecondMonsterTargets,X
        stz CharStruct::SecondPartyTargets,X
        stz CharStruct::SecondSelectedItem,X
        phx
        tdc
        tax
        lda #$03
        jsr Random_X_A  ;0..3
        tax
        tdc
        jsr SetBit_X
        plx
        sta CharStruct::PartyTargets,X	;fight random party member
        jmp QueueUncontrolledAction

.endproc

; ---------------------------------------------------------------------------

;Param X = Char Offset, $3D = Char index, $3F = Char Offset
;50% chance: 	sets up a fight command targetting a random party member
;		or picks a random known white/black/time spell and casts with inverted targetting
.proc CharmAction

_1E62:
        lda CharStruct::EnableSpells,X
        and #$0F			;white magic
        ora CharStruct::EnableSpells+1,X	;black and time magic
        beq Fight
        jsr Random_0_99
        cmp #$32	;50% chance of spell
        bcc Magic
Fight:
        ldx $3F		;char offset
        lda #$80
        sta CharStruct::ActionFlag,X
        lda #$05	;fight
        sta CharStruct::Command,X
        stz CharStruct::MonsterTargets,X
        stz CharStruct::SelectedItem,X
        stz CharStruct::SecondActionFlag,X
        stz CharStruct::SecondCommand,X
        stz CharStruct::SecondMonsterTargets,X
        stz CharStruct::SecondPartyTargets,X
        stz CharStruct::SecondSelectedItem,X
        phx
        tdc
        tax
        lda #$03
        jsr Random_X_A    ;0..3
        tax
        tdc
        jsr SetBit_X
        plx
        sta CharStruct::PartyTargets,X	;fight random party member
        jmp QueueUncontrolledAction
Magic:
        lda $3D		;char index
        tax
        stx $2A
        ldx .sizeof(CharSpells)	;650, size of CharSpells struct
        stx $2C
        jsr Multiply_16bit    ;**optimize: use rom table instead
        ldx $2E
        stx SpellOffsetRandom
        stz $0E
FindAnySpell:		;checks if any spells are learned
        lda CharSpells::ID+18,X	;starts at first white spell
        cmp #$46		;Quick spell
        beq NextSpell
        cmp #$FF		;empty spell slot
        bne TryRandomSpell
NextSpell:
        inx
        inc $0E
        lda $0E
        cmp #$36
        bne FindAnySpell
        bra Fight		;no spells, hit something instead
TryRandomSpell:
        ldx #$0012		;first white spell
        lda #$47		;last time spell
        jsr Random_X_A  	;random white/black/time spell
        longa
        adc SpellOffsetRandom
        tax
        shorta0
        lda CharSpells::ID,X
        cmp #$FF		;empty spell slot
        beq TryRandomSpell	;keep trying until we hit a known spell
        cmp #$46		;quick spell
        beq TryRandomSpell	;is no good either
        pha 			;holds known random spell
        longa
        jsr ShiftMultiply::_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        sta TempTargetting	;temp area
        tdc
        tay
        sty $16			;target bits
        lda TempTargetting
        bne CheckTargetting
TargetSelf:
        longa
        lda $3F			;Char Offset
        jsr ShiftDivide::_128	;char index (could've just loaded that)
        tax
        shorta0
        jsr SetBit_X     	;target self if no targetting info
        sta $16
        bra TargetReady
CheckTargetting:
        and #$40		;hits all
        bne TargetsAll
        lda TempTargetting
        and #$08		;targets enemy by default
        bne TargetsEnemy
TargetsOther:			;assumed to normally target party, now targets monsters
        tdc
        tax
        lda #$07
        jsr Random_X_A	     	;random monster 0..7
        tax
        tdc
        jsr SetBit_X
        sta $17			;monster target
        bra TargetReady
TargetsEnemy:			;normally targets enemy, now targets party
        tdc
        tax
        lda #$03
        jsr Random_X_A    	;random party 0..3
        tax
        tdc
        jsr SetBit_X
        sta $16			;party target
        bra TargetReady
TargetsAll:			
        lda TempTargetting
        and #$08		;targets enemy by default
        bne :+
        lda #$FF
        sta $17
        bra TargetReady
:	LDA #$F0		;target all party members
        sta $16
TargetReady:
        ldx $3F			;char Offset
        pla 			;random known spell
        sta CharStruct::SelectedItem,X
        lda $16			;party targets
        sta CharStruct::PartyTargets,X
        lda $17			;monster targets
        sta CharStruct::MonsterTargets,X
        lda #$21		;magic + costs mp
        sta CharStruct::ActionFlag,X
        lda #$2C		;first magic command
        sta CharStruct::Command,X
        stz CharStruct::SecondActionFlag,X
        stz CharStruct::SecondCommand,X
        stz CharStruct::SecondMonsterTargets,X
        stz CharStruct::SecondPartyTargets,X
        stz CharStruct::SecondSelectedItem,X
QueueUncontrolledAction:
        jmp QueueUncontrolledAction

.endproc

; ---------------------------------------------------------------------------

;Param X = Char Offset, $3D = Char index
;sets up a fight command targetting a random party member
.proc BerserkAction

_1F80:
        lda #$80
        sta CharStruct::ActionFlag,X
        lda #$05	;fight
        sta CharStruct::Command,X
        stz CharStruct::PartyTargets,X
        stz CharStruct::SelectedItem,X
        stz CharStruct::SecondActionFlag,X
        stz CharStruct::SecondCommand,X
        stz CharStruct::SecondMonsterTargets,X
        stz CharStruct::SecondPartyTargets,X
        stz CharStruct::SecondSelectedItem,X
        phx
        tdc
        tax
        lda #$07
        jsr Random_X_A	;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        plx
        sta CharStruct::MonsterTargets,X
        jmp QueueUncontrolledAction

.endproc

; ---------------------------------------------------------------------------

;Param $3D = char index
;Sets character's queued action to fire on the next ATB tick, and reset their uncontrolled ATB for their next turn

.proc QueueUncontrolledAction

        lda $3D		;char index
        jsr ResetATB   	;also sets Y = timer offset
        lda $3D
        tax
        lda CurrentTimer::ATB,Y
        cmp #$7F
        bcc :+
        lda #$7F	;max ATB 127
:	STA UncontrolledATB,X
        lda #$01	;action on next ATB tick
        sta CurrentTimer::ATB,Y
        lda #$41    	;waiting for delayed action
        sta EnableTimer::ATB,Y
        rts

.endproc

; ---------------------------------------------------------------------------

;Randomizes a table of combatant numbers, also initializes global timers
.proc RandomizeOrder

_1FD2:
        lda CurrentlyReacting
        bne :+
        jsr GlobalTimers
:	TDC
        tax
        dec
        									;:					
:	STA RandomOrder,X
        inx
        cpx #$000C
        bne :-
        									;.					
        tdc
        tay 			;slot for writing
_RandomizeOrder:													
        tdc
        tax 			;slot for reading
        lda #$0B
        jsr Random_X_A		;0..11
        sta $0E
        ldx #$0000
        									;:					
CheckValueInUse:		;see if we've used this number yet
        lda $0E
        cmp RandomOrder,X
        beq _Next		;already used, try another
        inx
        cpx #$000C
        bne CheckValueInUse
        									;.					
        sta RandomOrder,Y	;if not, save it
        iny 			;and select next writing slot
        									;:					
 _Next:
        cpy #$000C		;12 combatant slots
        bne RandomizeOrder
        									;.					
        rts

.endproc

; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------

;Updates Status/ATB timers for all combatants
;may skip updates depending on stop/etc.
.proc UpdateTimers

_200B:
        jsr GlobalTimers
        tdc
        tax
        stx $0A     		;char index
Loop:	TDC
        tay
        sty $0C     		;timer index
        lda $0A
        jsr GetTimerOffset
        tyx 			;X = Timer Offset
        ldy $0A
        lda ActiveParticipants,Y
        beq NextChar
        lda PauseTimerChecks,Y
        bne NextChar
        lda CurrentlyReacting
        bne :+
        lda QuickTimeFrozen,Y
        bne NextChar
:	JSR UpdateTimer 	;first timer is stop
        lda $08			;check if stop active
        bne NextChar  		;don't process other timers if stopped
        ldy #$0008  		;process 8 more status timers
:	JSR UpdateTimer
        dey
        bne :-
        jsr UpdateTimer 	;one more status timer (paralyze)
        lda $08			;check if paralyze active
        bne NextChar
        jsr UpdateTimer  	;advance ATB timer if not paralyzed
NextChar:
        inc $0A     		;next char index
        lda $0A
        cmp #$0C		;12 combatants
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

;Advances a status/atb timer if that status is supposed to be checked this tick
;sets up for the next call to check the next timer
;Params: $0C = timer index, X = timer offset
;Outputs: $08 = timer triggered
.proc UpdateTimer 

_2055:							;
        stz $08    	;timer triggered flag
        phy
        ldy $0C		;timer index
        lda !ProcessTimer,Y	;should process this timer this tick?
        beq Finish
        cpy #$000A	;ProcessTimer::ATB
        beq :+
        lda CurrentlyReacting
        bne Finish
:	LDA !EnableTimer,X	;is it enabled?
        beq Finish
        bmi TimerActive  	;check the 80h timer flag
        lda !CurrentTimer,X
        beq FlagTimer
        dec !CurrentTimer,X
        lda !CurrentTimer,X
        bne TimerActive
FlagTimer:		;flag EnableTimer when CurrentTimer hits 0
        lda !EnableTimer,X
        ora #$81
        sta !EnableTimer,X
TimerActive:
        lda $0C
        bne :+		;doesn't branch anywhere regardless
:	INC $08    	;timer triggered flag
Finish:
        ply 		;restore original Y
        inx 		;next timer (in offset)
        inc $0C		;next timer index
        rts

.endproc
; ---------------------------------------------------------------------------

;Decreases global status timers, then flags and reset those that trigger
;sets ProcessTimer to indicate that status should be updated this tick
.proc GlobalTimers

_2090:
        tdc
        tax
DecTimer:														
        lda !GlobalTimer,X
        beq Triggered
        dec !GlobalTimer,X
        stz !ProcessTimer,X
        bra :+
Triggered:								
        lda #$01
        sta !ProcessTimer,X		;flag timer for processing
        lda !ROMGlobalTimer,X		;reset timer from rom
        sta !GlobalTimer,X
        							;:
:	INX
        cpx #$000B			;11 timers
        bne DecTimer				
        rts

.endproc

; ---------------------------------------------------------------------------

;Attempts to find one character for each timer for whom that timer has ended (EnableTimer bit 80h)
;Check is in a fixed random order that's set up at battle init
;but subsequent runs will continue after the last character checked for each timer so it's somewhat fair
.proc FindEndedTimers

_20B2:
        tdc
        tax
        stx $08			;timer index
        tay
:	STA !TimerEnded,Y
        iny
        cpy #$000B
        bne :-
TimerLoop:	;for each timer, loop finds the first character for whom that timer ended, checking in a "random" order
        tdc
        tax
        stx $0A			;char count
        ldx $08			;timer index
        lda !RandomOrderIndex,X
        pha 			;original RandomOrderIndex
CharLoop:	;searches characters in a "random" order
        ldx $08			;timer index
        lda !RandomOrderIndex,X
        tax
        lda RandomOrder,X
        sta $0C			;char index
        tax
        lda PauseTimerChecks,X
        bne NextChar
        lda CurrentlyReacting
        bne :+
        lda QuickTimeFrozen,X
        bne NextChar
:	LDA $0C			;char index
        jsr GetTimerOffset      ;Y = Timer Offset
        tya
        clc
        adc $08
        tax 			;timer offset + index
        lda !EnableTimer,X
        bpl NextChar		;80h must be set to contiue
        lda $0C
        tay
        lda ActiveParticipants,Y
        beq NextChar
        lda $08			;timer index
        cmp #$01		;poison
        beq PoisonCountRegen
        cmp #$03		;countdown
        beq PoisonCountRegen
        cmp #$07		;regen
        bne EndTimer
PoisonCountRegen:	;skips ending timer for these status if they're also erased/hidden/jumping
        phx 		;timer offset + index
        ldx $08
        lda !RandomOrderIndex,X
        tax
        lda RandomOrder,X
        longa
        jsr ShiftMultiply::_128
        tax
        shorta0
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne NextCharPLX
        lda CharStruct::CmdStatus,X
        and #$10	;jumping
        beq EndTimerPLX
NextCharPLX:
        plx
        bra NextChar
EndTimerPLX:
        plx 		;timer offset + index
EndTimer:		;sets flag that timer has ended, so effects can be applied later
        pla
        lda !EnableTimer,X
        and #$7E	;clear $81
        sta !EnableTimer,X
        ldx $08		;timer index
        phx
        lda #$01	;flag that we found someone timer ended for
        sta !TimerEnded,X
        lda !RandomOrderIndex,X
        tax
        lda RandomOrder,X
        plx 		;timer index
        sta !TimerReadyChar,X	;which character had their timer end
        bra NextTimer	;don't check any more characters for this timer
NextChar:	;this character's timer didn't end or isn't eligable, 
        	;keep looking until all have been checked or one is found
        ldx $08		;timer index
        inc !RandomOrderIndex,X
        lda !RandomOrderIndex,X
        cmp #$0C	;reset index at 12
        bne :+
        stz !RandomOrderIndex,X
:	INC $0A        	;char count
        lda $0A
        cmp #$0C	;12 chars
        beq :+
        jmp CharLoop
:	PLA 		;original RandomOrderIndex
        sta !RandomOrderIndex,X
NextTimer:	
        inc $08        	;next timer index
        lda $08
        cmp #$0B	;11 timers
        beq Ret
        jmp TimerLoop
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc ApplyTimerEffects

_2177:
        tdc
        tax
        stx ProcessingTimer
Loop:
        ldx ProcessingTimer
        lda !TimerEnded,X
        beq NextTimer
        inc !RandomOrderIndex,X
        lda !RandomOrderIndex,X
        cmp #$0C		;12 chars
        bne :+
        stz !RandomOrderIndex,X
:	LDA !TimerReadyChar,X
        jsr GetTimerOffset    	;sets Y to timer offset
        lda !TimerReadyChar,X
        jsr CalculateCharOffset
        lda ProcessingTimer
        beq TimerEffect    	;timer 0 is stop, skips below check
        lda !EnableTimer,Y	;bits 80h and 01 are cleared prev
        bne NextTimer    	;skip effect if any other bits set
TimerEffect:
        jsr DispatchTimerEffect
NextTimer:
        inc ProcessingTimer
        lda ProcessingTimer
        cmp #$0B		;11 timers
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

;uses a jump table to call an appropriate timer effect routine
;Params: Y = timer offset (used in the effect routines)
.proc DispatchTimerEffect

_21B5:
        lda ProcessingTimer
        asl
        tax
        lda TimerEffectJumpTable,X
        sta $08
        lda TimerEffectJumpTable+1,X
        sta $09
        lda #$c2 ; Load from bank C2
        sta $0A
        jml [$0008]

.endproc

; ---------------------------------------------------------------------------

TimerEffectJumpTable:
;.word TimerEffectStop
;.word TimerEffectPoison
;.word TimerEffectReflect
;.word TimerEffectCountdown
;.word TimerEffectMute
;.word TimerEffectHPLeak
;.word TimerEffectOld
;.word TimerEffectRegen
;.word TimerEffectSing
;.word TimerEffectParalyze
;.word TimerEffectATB
.word $21E3, $21EE, $222A, $2235, $224E, $2259, $2264, $22AD, $2319, $237C, $238F

; ---------------------------------------------------------------------------

.proc TimerEffectStop

_21E3:
        ldx AttackerOffset
        lda CharStruct::Status3,X
        and #$EF	;clear stop
        sta CharStruct::Status3,X
        rts

.endproc
        							;
; ---------------------------------------------------------------------------

.proc TimerEffectPoison

_21EE:
        lda #$01
        sta EnableTimer::Poison,Y
        lda InitialTimer::Poison,Y
        sta CurrentTimer::Poison,Y
        jsr WipeDisplayStructures
        longa
        ldx AttackerOffset
        lda CharStruct::MaxHP,X
        jsr ShiftDivide::_16
        bne :+
        inc 				;min 1 damage
:	STA $0E				;poison tick damage
        sec
        lda CharStruct::CurHP,X
        sbc $0E				;poison tick damage
        bcs :+
        tdc 				;min 0 hp
:	STA CharStruct::CurHP,X
        shorta0
        lda TimerReadyChar::Poison
        ldx $0E				;poison tick damage
        stx TempDisplayDamage
        jsr CopyDisplayDamage
        lda #$09	;C1 routine: display regen/poison damage
        jmp CallC1

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectReflect

_222A:
        ldx AttackerOffset
        lda CharStruct::Status3,X
        and #$7F	;clear reflect
        sta CharStruct::Status3,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectCountdown

_2235:
        ldx AttackerOffset
        lda CharStruct::Status1,X
        and #$02	;zombie
        bne Ret
        lda TimerReadyChar::Countdown
        jsr KillCharacter
        lda MonsterDead
        beq Ret
        lda #$07	;C1 routine: condemn death animation
        jsr CallC1
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectMute

_224E:
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$FB	;clear mute
        sta CharStruct::Status2,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectHPLeak

_2259:
        ldx AttackerOffset
        lda CharStruct::Status4,X
        and #$F7	;clear hp leak
        sta CharStruct::Status4,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectOld
_2264:
        lda #$01
        sta EnableTimer::Old,Y
        lda InitialTimer::Old,Y
        sta CurrentTimer::Old,Y
        ldx AttackerOffset
        stz $0E
StatsLoop:		;applies to all 4 main stats	
        lda CharStruct::BaseStr,X
        dec
        beq :+		;**bug: wraps 0 stats to 255
        sta CharStruct::BaseStr,X
:	LDA CharStruct::EquippedStr,X
        dec
        beq :+
        sta CharStruct::EquippedStr,X
:	inx
        inc $0E
        lda $0E
        cmp #$04	;4 stats
        bne StatsLoop
        ldx ProcessingTimer
        lda !TimerReadyChar,X
        cmp #$04	;monster check
        bcc Ret
        ldx AttackerOffset
        lda CharStruct::Level,X
        dec
        beq :+
        sta CharStruct::Level,X
:	LDA CharStruct::MonsterAttack,X
        dec
        bpl Ret	;bug? only decreases attack if above 128
        sta CharStruct::MonsterAttack,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectRegen

_22AD:
        lda #$01
        sta EnableTimer::Regen,Y
        lda InitialTimer::Regen,Y
        cmp #$1E
        bcs :+
        lda #$1E	;max 30 ticks if it was slower
        sta InitialTimer::Regen,Y
:	STA CurrentTimer::Regen,Y
        jsr WipeDisplayStructures
        ldx AttackerOffset
        jsr CopyStatsWithBonuses
        lda Level
        sta $24
        lda Vitality
        sta $25
        jsr Multiply_8bit
        ldx AttackerOffset
        lda CharStruct::Status1,X
        and #$02	;zombie
        bne Ret
        longa
        lda $26
        jsr ShiftDivide::_16
        tax
        bne :+
        inc 		;min 1
:	STA $0E
        ldx AttackerOffset
        clc
        adc CharStruct::CurHP,X
        bcs :+
        cmp CharStruct::MaxHP,X
        bcc :++
:	LDA CharStruct::MaxHP,X	;cap at maxhp
:	STA CharStruct::CurHP,X
        shorta0
        lda $0F
        ora #$80       		;flag to display as healing
        sta $0F
        lda TimerReadyChar::Regen
        ldx $0E
        stx TempDisplayDamage
        jsr CopyDisplayDamage
        lda #$09	;C1 routine: display regen/poison damage
        jsr CallC1
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectSing

_2319:
        lda #$01
        sta EnableTimer::Sing,Y
        lda InitialTimer::Sing,Y
        sta CurrentTimer::Sing,Y
        tdc
        tay
        ldx AttackerOffset
        lda CharStruct::Song,X
        beq Ret
FindSong:		;Y = song stat index
        asl
        bcs :+
        iny
        bra FindSong
:	STY $12		;song stat index
        tdc
        tax
        stx $0E		;target
        lda #$04
        sta $10		;after last target
        lda TimerReadyChar::Sing
        cmp #$04	;monster check? monsters can sing?
        bcc ApplySong
        lda #$04
        sta $0E		;target
        lda #$0C
        sta $10		;last target +1
        ldx #$0180	;**bug: should be $0200 for first monster
ApplySong:
        stx $14		;char offset
        longa
        txa
        clc
        adc $12		;adjust offset by song stat
        tax
        shorta0
CharLoop:
        ldy $0E		;target
        lda ActiveParticipants,Y
        beq Next
        clc
        lda CharStruct::BonusStr,X	;different stats depending on X
        inc
        cmp #$64	;don't apply changes at 100 and up
        bcs Next
        sta CharStruct::BonusStr,X
Next:
        jsr NextCharOffset
        stx $14		;char offset
        inc $0E		;next target
        lda $0E
        cmp $10		;last target +1
        bne CharLoop
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectParalyze

_237C:
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$DF	;clear paralyze
        sta CharStruct::Status2,X
        ldx ProcessingTimer
        lda !TimerReadyChar,X
        jmp ResetATB

.endproc

; ---------------------------------------------------------------------------

;character's atb timer finished, either add them to the menu queue or perform their action
.proc TimerEffectATB

_238F:
        jsr CheckBattleEnd
        lda BattleOver
        bne GoRet
        lda TimerReadyChar::ATB
        sta AttackerIndex
        jsr GetTimerOffset
        tyx
        lda EnableTimer::Paralyze,X
        bne GoRet
        lda EnableTimer::ATB,X
        beq :+
        jmp PerformAction      	;action is ready, do it
:	LDA TimerReadyChar::ATB
        cmp #$04	;monster check
        bcs Monster
        tdc
        tax
SearchTurnQueue:	;find character in turn queue
        lda ATBReadyQueue,X
        cmp TimerReadyChar::ATB
        beq GoRet	;character already in turn queue
        inx
        cpx #$0004
        bne SearchTurnQueue
        lda TimerReadyChar::ATB
        jsr CheckDisablingStatus
        bne GoRet
        ldx ATBReadyCount
        lda TimerReadyChar::ATB
        sta ATBReadyQueue,X
        inc ATBReadyCount
GoRet:	jmp Ret
Monster:
        jsr MonsterATB
Ret:	rts

.endproc

; ---------------------------------------------------------------------------




.proc MonsterATB
        nop
.endproc

.proc PerformAction
        nop
.endproc

.proc CheckBattleEnd
        nop
.endproc

.proc ResetATB
        nop
.endproc

.proc KillCharacter
        nop
.endproc

.proc HasteSlowMod
        nop
.endproc

.proc ApplyGear
        nop
.endproc

.proc GFXCmdAttackNameFromTemp
        nop
.endproc

.proc FindOpenGFXQueueSlot
        nop
.endproc

.proc SelectCurrentProcSequence
        nop
.endproc

.proc WeaponEffectCommand
        nop
.endproc

.proc GFXCmdDamageNumbers
        nop
.endproc

.proc ReplaceHands
        nop
.endproc

.proc CheckRetarget
        nop
.endproc

.proc CastSpell
        nop
.endproc

.proc JumpCommand_Anim
        nop
.endproc

.proc GFXCmdMessage
        nop
.endproc

.proc DispatchCommand_CommandReady
        nop
.endproc

.proc FightCommand
        nop
.endproc

.proc MSword
        nop
.endproc