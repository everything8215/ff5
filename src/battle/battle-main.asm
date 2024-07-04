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

.import ExecSound_ext, ExecBtlGfx_ext
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
        jsr     SetupRegisters
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
.a8
.i16
_0053:  lda #0
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
.a16
.i16
_006E:  inc wMusicChanged
        sta MusicData
        jsl ExecSound_ext
        stz wMusicChanged
        rts

.endproc

; ---------------------------------------------------------------------------

;generates a random number between X and A, inclusive
.proc Random_X_A
.a8
_007C:  shorti
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
        longi
        jsr Division
        shorti
        clc
        lda Remainder
        adc $3c
_Finish:
        pha
        lda RNGPointer
        tax
        inc $3a,X
        longi
        pla
        rts

.endproc

; ---------------------------------------------------------------------------

.proc Multiply_16bit
.i16
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
.a8
.i8
_00F1:  lda $24
        sta f:$004202
        lda $25
        sta f:$004203
        longa
        nop
        nop
        nop
        lda f:$004216
        sta $26
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Divide $7CB7 by $7CB9: result in $7CBB, remainder in $7CBD)
;16 bit

.proc Division
.i16
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
;.org $C20148
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
;.proc ShiftMultiply

_01B1:
ShiftMultiply_256:
        asl
ShiftMultiply_128:
        asl
ShiftMultiply_64:
        asl
ShiftMultiply_32:
        asl
ShiftMultiply_16:
        asl
ShiftMultiply_8:
        asl
ShiftMultiply_4:
        asl
ShiftMultiply_2:
        asl
        rts

; ---------------------------------------------------------------------------

;Division by 256, 128, 64, 32, 16, 8, 4 or 2 via shifts

_01BA:
ShiftDivide_256:
        lsr
ShiftDivide_128:
        lsr
ShiftDivide_64:
        lsr
ShiftDivide_32:
        lsr
ShiftDivide_16:
        lsr
ShiftDivide_8:
        lsr
ShiftDivide_4:
        lsr
ShiftDivide_2:
        lsr
        rts

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
        jsr ShiftMultiply_128
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
.a16
.i16
_0218:
        ldx #$0090
WipeAnimBlocks:		;wipes Anim structure and blocking information after it, $3BCC-$3C5B
        stz ActionAnim0::Flags,X
        dex
        bpl WipeAnimBlocks
        txa 		;A now $FF
        ldx #$037F
WipeGFXQueueDamage:	;wipes GFXQueue structure and DisplayDamage after it with $FF, $384C-$3BCB
        sta GFXQueue,X
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
.a8
.i16
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
.a8
.i16
_028A:
        stz a:wTargetIndex	;**optimize: wastes a byte
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
        jsr ShiftMultiply_8	;Size of Magic Data
        tax
        shorta0
        stz $3D
CopyFirst5:                                                                                                            
        lda ROMMagicInfo,X
        sta AttackInfo,Y
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
        lda ROMMagicInfo,X
        sta AttackInfo,Y
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
:	lda ROMItemEquippable,X
        sta TempEquippable,Y
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
        jsr ShiftDivide_32	;shift to 04h bit
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
Ret: 	rts

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
        lda MonsterAIScript,X
        sta GFXQueue,Y
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
        jsr ShiftMultiply_8
        tax
        shorta0
        ldy $0C		;ProcSequence*12
        stz $0A
:       lda ROMConsumables,X
        sta AttackInfo,Y
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
:	lda ROMConsumables,X
        sta AttackInfo,Y
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
        bpl _CheckRetarget
        lda ProcSequence
        tax
        inc HitsInactive,X	;can hit dead targets
        bra TargetOK
_CheckRetarget:
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
Single:	sta TargetType,X
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
        jsr ShiftMultiply_4
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
:	lda RHWeapon,X
        sta AttackInfo,Y
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
:	lda LHWeapon,X
        sta AttackInfo,Y
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
:	lda RHWeapon,X
        sta AttackInfo,Y
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
:	lda LHWeapon,X
        sta AttackInfo,Y
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
:	lda #$08	;otherwise unicorn
Chosen:
        sta TempSpell
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:	lda ROMEffectInfo,X
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
:	lda RHWeapon,X
        sta AttackInfo,Y
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
:	lda LHWeapon,X
        sta AttackInfo,Y
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
        jsr ShiftDivide_32
        tax 			;MagicBits spell
        lda MagicBits,Y
        jsr SelectBit_X
        beq PickRandomSummon	;don't know this one, try again
MagicLamp:			;Magic Lamp use jumps in here
        stz PartyTargets
        stz MonsterTargets
        lda TempSpell
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:	    lda ROMMagicInfo,X
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
Ret:	rts

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
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:  	    lda ROMMagicInfo,X
        sta TempMagicInfo,Y
        inx
        iny
        cpy #$0008		;8 bytes magic data
        bne :-
        jsr SelectCurrentProcSequence
        tdc
        tax
:	    lda TempMagicInfo,X
        sta AttackInfo,Y
        inx
        iny
        cpx #$0005		;copy first 5 bytes
        bne :-
        iny 			;increment dest pointer by 4
        iny
        iny
        iny
:	lda TempMagicInfo,X
        sta AttackInfo,Y
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
        bpl _CheckRetarget
        lda ProcSequence
        tax
        inc HitsInactive,X
        bra TargetOK
_CheckRetarget:
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
:	    sta TargetType,X
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
        jsr ShiftMultiply_32
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
:	    lda #$03     	;otherwise 3
Chosen:
        sta $0E		;terrain spell slot 0-3
        lda TerrainType
        jsr ShiftMultiply_4
        clc
        adc $0E
        tax
        lda ROMTerrainSpells,X
        sta TempSpell
        lda TempSpell	;pointless load?
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:	    lda ROMEffectInfo,X
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
Found:	lda #$26	;show command
        sta CharCommands::ID,X
        lda #$08	;target enemy?
        sta CharCommands::Targetting,X
        inx
        iny
        bra FindHideCommands
Ret:	rts

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
Ret:	rts

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
:	lda SavedAction,Y
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
        sta a:wMonsterTargets	;**optimize: wasted bytes
        stz a:wPartyTargets
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
Ret:	rts

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
:      	lda RHWeapon,X
        sta AttackInfo,Y
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
:	    lda RHWeapon,X
        sta AttackInfo,Y
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
:   	lda LHWeapon,X
        sta AttackInfo,Y
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
:	    lda LHWeapon,X
        sta AttackInfo,Y
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
Ret:	rts

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

.org $16AA
.proc CopyAbilityInfo
_16AA:
        pha
        jsr SelectCurrentProcSequence
        pla
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        stz $0A
:	lda ROMAbilityInfo,X
        sta AttackInfo,Y
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
:	lda ROMAbilityInfo,X
        sta AttackInfo,Y
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
:	lda DisplayInfo::CurrentChar
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
:	lda DisplayInfo::CurrentChar
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
Finish:	sta ControllingB
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
:	lda DisplayInfo::CurrentChar
        sta CurrentChar
        lda GearChanged
        beq :+
        stz GearChanged
        jsr ReplaceHands
        jsr ApplyGear
:	lda DisplayInfo::CurrentChar
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
:	lda DisplayInfo::CurrentChar
        cmp MenuData::CurrentChar
        beq :+
        lda EncounterInfo::IntroFX
        bmi :+		;branch if credits fight
        lda #$0D	;C1 Routine
        jsr CallC1
WaitForever:
        bra WaitForever	;infinite loop?
:	lda DisplayInfo::CurrentChar
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
:	lda MenuData::ActionFlag
        and #$40	;Item
        bne ItemDelay
        lda MenuData::ActionFlag
        and #$20	;Magic
        beq :+
        jmp MagicDelay
:	lda MenuData::ActionFlag
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
        lda LHWeapon,Y
        and #$03	;delay bits (delay/10)
        tax
        clc
        lda ROMTimes10,X
        adc $0E		;add other weapon's delay
        sta $0E
:	lda $0E		;attack delay
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
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMConsumables::Misc,X
        and #$08
        bne :+
        lda MenuData::SelectedItem
        jsr ConsumeItem
:	lda ROMConsumables::Targetting,X
        and #$03	;delay bits (delay/10)
        tax
        lda ROMTimes10,X
        bra Finish
MagicDelay:	
        stz $0E
        lda MenuData::SelectedItem
        longa
        jsr ShiftMultiply_8
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
        jsr ShiftMultiply_8
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
:	lda RHWeapon::ItemMagic,Y	;could be LHWeapon
        and #$7F	;weapon magic to cast
        beq Finish
        longa
        jsr ShiftMultiply_8
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
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;initializes some values when a battle during the credits happens
;this range is used by C1 graphics code but unsure what it does
.org $1C51
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
.org $1C74
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
Ret:	rts

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
:	lda DisplayInfo::CurrentChar
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
:	lda CharStruct::Status1,X
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
:	lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$10	;charm
        beq :+
        jsr CharmAction
        bra Next
:	lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$08	;berserk
        beq Next
        jsr BerserkAction
Next:
        ldx $3F		;char offset
        jsr NextCharOffset
        stx $3F
        inc a:$003D	;char index ; TODO: dont know why this is being done?
        lda a:$003D
        cmp #$04	;4 characters
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

;Param X = Char Offset, $3D = Char index
;sets up a fight command targetting a random party member
.org $1E2F
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
        jmp _QueueUncontrolledAction
Magic:
        lda $3D		;char index
        tax
        stx $2A
        ldx #$028A ; TODO: fixme .sizeof(CharSpells)	;650, size of CharSpells struct
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
        jsr ShiftMultiply_8
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
        jsr ShiftDivide_128	;char index (could've just loaded that)
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
:	lda #$F0		;target all party members
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
_QueueUncontrolledAction:
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
:	sta UncontrolledATB,X
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
:	sta RandomOrder,X
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
        bne _RandomizeOrder
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
        lda ProcessTimer,Y	;should process this timer this tick?
        beq Finish
        cpy #$000A	;ProcessTimer::ATB
        beq :+
        lda CurrentlyReacting
        bne Finish
:	lda EnableTimer,X	;is it enabled?
        beq Finish
        bmi TimerActive  	;check the 80h timer flag
        lda CurrentTimer,X
        beq FlagTimer
        dec CurrentTimer,X
        lda CurrentTimer,X
        bne TimerActive
FlagTimer:		;flag EnableTimer when CurrentTimer hits 0
        lda EnableTimer,X
        ora #$81
        sta EnableTimer,X
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
        lda GlobalTimer,X
        beq Triggered
        dec GlobalTimer,X
        stz ProcessTimer,X
        bra :+
Triggered:								
        lda #$01
        sta ProcessTimer,X		;flag timer for processing
        lda ROMGlobalTimer,X		;reset timer from rom
        sta GlobalTimer,X
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
:	sta TimerEnded,Y
        iny
        cpy #$000B
        bne :-
TimerLoop:	;for each timer, loop finds the first character for whom that timer ended, checking in a "random" order
        tdc
        tax
        stx $0A			;char count
        ldx $08			;timer index
        lda RandomOrderIndex,X
        pha 			;original RandomOrderIndex
CharLoop:	;searches characters in a "random" order
        ldx $08			;timer index
        lda RandomOrderIndex,X
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
:	lda $0C			;char index
        jsr GetTimerOffset      ;Y = Timer Offset
        tya
        clc
        adc $08
        tax 			;timer offset + index
        lda EnableTimer,X
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
        lda RandomOrderIndex,X
        tax
        lda RandomOrder,X
        longa
        jsr ShiftMultiply_128
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
        lda EnableTimer,X
        and #$7E	;clear $81
        sta EnableTimer,X
        ldx $08		;timer index
        phx
        lda #$01	;flag that we found someone timer ended for
        sta TimerEnded,X
        lda RandomOrderIndex,X
        tax
        lda RandomOrder,X
        plx 		;timer index
        sta TimerReadyChar,X	;which character had their timer end
        bra NextTimer	;don't check any more characters for this timer
NextChar:	;this character's timer didn't end or isn't eligable, 
        	;keep looking until all have been checked or one is found
        ldx $08		;timer index
        inc RandomOrderIndex,X
        lda RandomOrderIndex,X
        cmp #$0C	;reset index at 12
        bne :+
        stz RandomOrderIndex,X
:	INC $0A        	;char count
        lda $0A
        cmp #$0C	;12 chars
        beq :+
        jmp CharLoop
:	PLA 		;original RandomOrderIndex
        sta RandomOrderIndex,X
NextTimer:	
        inc $08        	;next timer index
        lda $08
        cmp #$0B	;11 timers
        beq Ret
        jmp TimerLoop
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc ApplyTimerEffects

_2177:
        tdc
        tax
        stx ProcessingTimer
Loop:
        ldx ProcessingTimer
        lda TimerEnded,X
        beq NextTimer
        inc RandomOrderIndex,X
        lda RandomOrderIndex,X
        cmp #$0C		;12 chars
        bne :+
        stz RandomOrderIndex,X
:	lda TimerReadyChar,X
        jsr GetTimerOffset    	;sets Y to timer offset
        lda TimerReadyChar,X
        jsr CalculateCharOffset
        lda ProcessingTimer
        beq TimerEffect    	;timer 0 is stop, skips below check
        lda EnableTimer,Y	;bits 80h and 01 are cleared prev
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
        lda f:TimerEffectJumpTable,X
        sta $08
        lda f:TimerEffectJumpTable+1,X
        sta $09
        lda #$c2 ; Load from bank C2
        sta $0A
        jml [$0008]

.endproc

; ---------------------------------------------------------------------------

.org $C221CD
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

.reloc
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
        jsr ShiftDivide_16
        bne :+
        inc 				;min 1 damage
:	sta $0E				;poison tick damage
        sec
        lda CharStruct::CurHP,X
        sbc $0E				;poison tick damage
        bcs :+
        tdc 				;min 0 hp
:	sta CharStruct::CurHP,X
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
Ret:	rts

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
:	lda CharStruct::EquippedStr,X
        dec
        beq :+
        sta CharStruct::EquippedStr,X
:	inx
        inc $0E
        lda $0E
        cmp #$04	;4 stats
        bne StatsLoop
        ldx ProcessingTimer
        lda TimerReadyChar,X
        cmp #$04	;monster check
        bcc Ret
        ldx AttackerOffset
        lda CharStruct::Level,X
        dec
        beq :+
        sta CharStruct::Level,X
:	lda CharStruct::MonsterAttack,X
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
:	sta CurrentTimer::Regen,Y
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
        jsr ShiftDivide_16
        tax
        bne :+
        inc 		;min 1
:	sta $0E
        ldx AttackerOffset
        clc
        adc CharStruct::CurHP,X
        bcs :+
        cmp CharStruct::MaxHP,X
        bcc :++
:	lda CharStruct::MaxHP,X	;cap at maxhp
:	sta CharStruct::CurHP,X
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
Ret:	rts

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
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc TimerEffectParalyze

_237C:
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$DF	;clear paralyze
        sta CharStruct::Status2,X
        ldx ProcessingTimer
        lda TimerReadyChar,X
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
:	lda TimerReadyChar::ATB
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

; ---------------------------------------------------------------------------

;called when character's turn is up, perform their queued action
.proc PerformAction

_23DF:
        jsr ProcessTurn
        lda DelayedFight
        bne Ret
        lda AttackerIndex
        cmp #$04	;monster check
        bcs _ResetATB
        ldx AttackerOffset
        lda CharStruct::CmdStatus,X
        and #$E0	;clear many flags (jump/flirt/others?)
        sta CharStruct::CmdStatus,X
        stz CharStruct::DamageMod,X
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$02	;zombie
        bne Uncontrolled
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$18	;charm/berserk
        beq _ResetATB
Uncontrolled:
        lda AttackerIndex
        jsr GetTimerOffset
        tdc
        sta EnableTimer::ATB,Y
        inc
        sta CurrentTimer::ATB,Y
        lda AttackerIndex
        tax
        lda UncontrolledATB,X
        and #$7F	;max 127
        sta UncontrolledATB,X
_ResetATB:
        inc CheckQuick
        lda AttackerIndex
        jsr ResetATB
        stz CheckQuick
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Waits when a character's turn arrives (amount depending on battle speed setting)
.proc ATBWait

_2432:
        lda ATBWaiting
        beq Ret
        lda ATBWaitLeft
        beq DoneWaiting
        dec
        sta ATBWaitLeft
        bne Ret
DoneWaiting:
        tdc
        sta ATBWaiting
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Updates ATB for all combatants and sets them active if present
.proc ResetATBAll

_2447:
        tdc
        tax
        tay
        stx $0E			;char index
ResetATBLoop:
        lda $0E
        jsr ResetATB
        lda $0E
        jsr CalculateCharOffset
        lda $0E
        cmp #$04		;monster check
        bcs Monster
        ldx AttackerOffset
        lda CharStruct::CharRow,X
        and #$40		;not present
        beq SetActive
        bne Next
Monster:
        sec
        lda $0E
        sbc #$04
        tax 			;monster index
        lda InitialMonsters,X
        beq Next
SetActive:
        ldx $0E
        lda #$01
        sta ActiveParticipants,X
Next:
        inc $0E			;char index
        lda $0E
        cmp #$0C		;12 participants
        bne ResetATBLoop
        									;.					
        rts

.endproc

; ---------------------------------------------------------------------------

;initialize ATB (A: character index 0-12)
.org $2482
.proc ResetATB

_2482:
        pha
        jsr GetTimerOffset	;Y and $36 = timer offset
        pla
        jsr CalculateCharOffset
        jsr CopyStatsWithBonuses
        lda CharStruct::EqWeight,X
        jsr ShiftDivide_8	;weight/8
        clc
        adc #$78     		;+120
        sec
        sbc Agility    	;-agi
        beq :+
        bcs :++
:	lda #$01     		;min 1
:	JSR HasteSlowMod
        sta CurrentTimer::ATB,Y
        lda EncounterInfo::IntroFX
        bpl NotCredits		;80h indicates a credits demo battle
        ldx AttackerOffset
        cpx #$0200		;monster
        bcs CreditsMonster
        lda #$01		;party member gets turn immediately
        bra CreditsParty
CreditsMonster:
        lda #$FF		;monster turn as late as possible
CreditsParty:								
        sta CurrentTimer::ATB,Y
NotCredits:								
        lda CheckQuick
        beq EnableATB
        lda QuickTurns
        beq EnableATB
        lda CurrentlyReacting
        bne EnableATB
        dec QuickTurns
        lda QuickTurns
        bne Quick
        phy
        jsr ClearQuick
        ply
        bra EnableATB
Quick:										;:					
        lda #$01
        sta CurrentTimer::ATB,Y
EnableATB:
        lda #$01
        sta EnableTimer::ATB,Y
        rts

.endproc

; ---------------------------------------------------------------------------

;Unfreezes time for everyone
.proc ClearQuick

_24E4:
        tdc
        tax
:	stz QuickTimeFrozen,X
        inx
        cpx #$000C		;12 combatants
        bne :-
        rts

.endproc

; ---------------------------------------------------------------------------

;Stop Timer (X: #timer; A: Target index 0-12)
.proc StopTimer

_24F0:
        phx
        jsr GetTimerOffset
        plx
        jsr AddTimerOffsetY
        tdc
        sta EnableTimer,Y
        rts

.endproc

; ---------------------------------------------------------------------------

;Start Timer (X: #timer; A: Participant index)
.proc StartTimer

_24FD:
        phx
        pha
        jsr GetTimerOffset
        pla
        jsr CalculateCharOffset
        jsr CopyStatsWithBonuses
        plx
        jsr GetTimerDuration	;also sets up Y
        ldx AttackerOffset	;not actually attacker, in this case
        jsr HasteSlowMod
        sta CurrentTimer,Y
        sta InitialTimer,Y
        lda #$01
        sta EnableTimer,Y
        stz StatusFixedDur
        rts

.endproc

; ---------------------------------------------------------------------------

;Get Timer Duration (X - #timer; $3ED7 - IsItem): A = return duration
;sets up and jumps to a jump table entry that sets the correct duration
;also sets up Y as the correct timer offset
.proc GetTimerDuration

_2521:
        jsr AddTimerOffsetY      ;Y = X + TimerOffset
        txa
        asl
        clc
        adc StatusFixedDur     ;uses alternate fixed status duration
        asl
        tax
        lda f:TimerDurationJumpTable,X
        sta $08
        lda f:TimerDurationJumpTable+1,X
        sta $09
        lda #$c2 ;.b #bank(TimerDurationJumpTable)
        sta $0A
        jmp [$0008]		;jump to table address

.endproc

; ---------------------------------------------------------------------------

;(X): Y = X + $36 Timer Offset)
.org $253F
.proc AddTimerOffsetY

_253F:
        txa
        longa
        clc
        adc TimerOffset
        tay
        shorta0
        rts

.endproc
; ---------------------------------------------------------------------------

;Jump table for timer durations, these all return with A = timer duration
.org $C2254A
.proc TimerDurationJumpTable

_254A:
; .word DurSpell
; .word Dur120a
; .word DurVit
; .word DurVit
; .word DurSpell
; .word Dur120b
; .word DurSpell
; .word Dur49
; .word DurSpell
; .word Dur180mod
; .word DurSpell
; .word Dur180
; .word Dur10
; .word Dur10
; .word Dur110mod
; .word Dur110mod
; .word Dur30
; .word Dur30
; .word DurSpellmod
; .word Dur120mod
.word $2572, $2576, $2579, $2579, $2572, $2584, $2572, $2587, $2572, $258A
.word $2572, $259A, $259D, $259D, $25A0, $25A0, $25AF, $25AF, $25B2, $25C3
.reloc
; ---------------------------------------------------------------------------

;Duration = Spell Duration
.proc DurSpell

_2572:
        lda StatusDuration
        rts

.endproc

; ---------------------------------------------------------------------------

						
;Duration = 120
.proc Dur120a

_2576:
        lda #$78	;120
        rts

.endproc

; ---------------------------------------------------------------------------

;Duration = Attacker's Vitality + 20
.proc DurVit

_2579:
        clc
        lda Vitality
        adc #$14	;+20
        bcc :+
        lda #$FF	;max 255
:	rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 120
.proc Dur120b

_2584:
        lda #$78	;120
        rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 49
.proc Dur49

_2587:
        lda #$31	;49
        rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 180 - Attacker's Magic Power / 2
.proc Dur180mod

_258A:
        lda MagicPower
        lsr
        sta $0E
        sec
        lda #$B4	;180
        sbc $0E
        bcs :+
        lda #$01	;min 1
:	rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 180 
.proc Dur180

_259A:
        lda #$B4	;180
        rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 10 
.proc Dur10

_259D:
        lda #$0A	;10
        rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 110 - Attacker's Magic Power, min 30
.proc Dur110mod

_25A0:
        sec
        lda #$6E	;110
        sbc MagicPower
        bcc :+
        cmp #$1E	;min 30
        bcs :++
:	lda #$1E	;min 30
:	rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 30
;**optimize: reuse code from Dur110mod 
.proc Dur30

_25AF:
        lda #$1E
        rts

.endproc

; ---------------------------------------------------------------------------

;Duration = Spell Duration - Attacker's Magic Power / 2
.proc DurSpellmod

_25B2:
        lda MagicPower
        lsr
        sta $0E
        sec
        lda StatusDuration
        sbc $0E
        bcs :+
        lda #$01	;min 1
:	rts

.endproc

; ---------------------------------------------------------------------------

;Duration = 120 - Attacker's Magic Power / 2
.proc Dur120mod

_25C3:
        lda MagicPower
        lsr
        sta $0E
        sec
        lda #$78	;120
        sbc $0E
        bcs :+
        lda #$01	;min 1
:	rts

.endproc

; ---------------------------------------------------------------------------

;Queues up a monster's action when their ATB is ready
.proc MonsterATB

_25D3:
        lda #$01
        sta AISkipDeadCheck
        sec
        lda AttackerIndex
        sbc #$04
        sta MonsterIndex
        jsr ShiftMultiply_16
        tax
        stx MonsterOffset16
        asl
        tax
        stx MonsterOffset32
        tdc
        tay
        sty TempCharm
        ldx MonsterOffset16
        lda #$FF
:	sta MonsterMagic,X
        inx
        iny
        cpy #$0010	;init 16 byte monster magic struct
        bne :-
        							;
        lda MonsterIndex
        asl
        tax
        lda ROMTimes100w,X
        sta $0E
        lda ROMTimes100w+1,X
        sta $0F
        tdc
        tay
        ldx $0E		;MonsterIndex *100
        lda #$FF
:	sta MonsterAIScript,X
        inx
        iny
        cpy #$0064	;init 100 bytes to $FF
        bne :-
        lda AttackerIndex
        jsr CalculateCharOffset
        ldx AttackerOffset
        lda #$2C       	;magic
        sta CharStruct::Command,X
        lda #$21	;magic + costs mp
        sta CharStruct::ActionFlag,X
        ldx AttackerOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$08	;berserk
        beq CheckCharm
        lda #$01
        sta CharStruct::CmdCancelled,X
        lda #$80	;monster fight
        sta AIBuffer
        lda #$FF	;end of list
        sta AIBuffer+1
        jsr DispatchAICommands
        jmp GoFinish
CheckCharm:
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$10	;charm
        beq CheckFlirt
TryRandomAction:
        ldx AttackerOffset
        lda #$01
        sta CharStruct::CmdCancelled,X
        tdc
        tax
        lda #$03
        jsr Random_X_A 	;0..3
        tax
        stx $0E
        lda MonsterIndex
        asl
        tax
        longa
        lda BattleMonsterID,X
        jsr ShiftMultiply_4
        clc
        adc $0E		;random number 0..3
        tax 		;offset into control actions table
        shorta0
        lda ROMControlActions,X
        cmp #$FF
        beq TryRandomAction	;no action in this slot, try again
        sta AIBuffer
        lda #$FF	;end of list
        sta AIBuffer+1
        inc TempCharm
        jsr DispatchAICommands
        bra GoFinish
CheckFlirt:								;
        lda CharStruct::CmdStatus,X
        and #$08	;flirt
        beq CheckControl
        lda #$51	;throbbing command
        sta CharStruct::Command,X
        lda #$80	;other
        sta CharStruct::ActionFlag,X
        bra GoFinish
CheckControl:
        lda CharStruct::Status4,X
        and #$20	;control
        bne Control
        lda CharStruct::Status2,X
        and #$40	;sleep
        bne Sleep
        bra Normal
Control:
        tdc
        tay
:	lda ControlTarget,Y
        cmp AttackerIndex
        beq FoundController
        iny
        bra :-
FoundController:
        lda ControlCommand,Y
        bne _ControlCommand
Sleep:	;or controlled without a command
        stz CharStruct::Command,X
        lda #$80	;action complete?
        sta CharStruct::ActionFlag,X
        bra GoFinish
_ControlCommand:
        tdc
        sta ControlCommand,Y
        lda MonsterIndex
        tax
        lda MonsterControlActions,X
        sta AIBuffer
        lda #$FF	;end of list
        sta AIBuffer+1
        jsr DispatchAICommands
GoFinish:
        jmp Finish
Normal:
        lda MonsterIndex
        tax
        lda AIActiveConditionSet,X
        sta AICurrentActiveCondSet
        lda MonsterIndex
        asl
        tax
        longa
        clc
        lda ROMTimes1620w,X	;*1620, size of MonsterAI struct
        adc #MonsterAI
        sta AIOffset
        shorta0
        stz AICurrentCheckedSet
CheckAIConditions:
        lda AICurrentCheckedSet
        tax
        lda ROMTimes17,X	;size of a MonsterAI condition
        tay
        sty AIConditionOffset
        stz AICheckIndex
CheckSingleCondition:
        ldy AIConditionOffset
        lda (AIOffset),Y
        beq AIActions		;0 always succeeds
        cmp #$FE		;indicates end of condition set
        beq AIActions
        jsr CheckAICondition
        lda AIConditionMet
        beq NextConditionSet
        longa
        clc
        lda AIConditionOffset
        adc #$0004		;next condition in set
        sta AIConditionOffset
        shorta0
        inc AICheckIndex
        bra CheckSingleCondition
NextConditionSet:	;failed a condition in this set, check next set of conditions
        inc AICurrentCheckedSet
        lda AICurrentCheckedSet
        cmp #$0A		;10 conditions max
        bne CheckAIConditions
AIActions:
        longa
        clc
        lda AIOffset
        adc #$00AA	;advances from Conditions to Actions
        sta AIOffset
        shorta0
        lda AICurrentActiveCondSet
        cmp AICurrentCheckedSet
        beq ConditionOK	;matches so don't need to change things
        lda MonsterIndex
        tax
        lda AICurrentCheckedSet
        sta AIActiveConditionSet,X	;checked cond is now current
        lda MonsterIndex
        asl
        tay
        lda AICurrentCheckedSet
        asl
        tax
        lda ROMTimes64w,X
        sta AICurrentOffset,Y
        lda ROMTimes64w+1,X
        sta AICurrentOffset+1,Y
ConditionOK:
        jsr ProcessAIScript
Finish:
        ldx MonsterOffset16
        lda MonsterMagic,X
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        and #$03       	;delay values
        tax
        lda ROMTimes10,X
        pha
        lda AttackerIndex
        jsr GetTimerOffset
        pla
        sta CurrentTimer::ATB,Y    ;**bug? doesn't adjust for haste/slow
        lda #$41	;pending action
        sta EnableTimer::ATB,Y
        lda MonsterIndex
        asl
        tax
        stz ForcedTarget::Party,X
        stz ForcedTarget::Monster,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CheckAICondition

_27BF:
        cmp #$13	;$12 is last valid condition
        bcc :+
        tdc 		;always succeed	(if invalid)
:	sta $0E		;condition to check
        asl
        tax
        lda f:AICondition,X
        sta $08
        lda f:AICondition+1,X
        sta $09
        lda #$C2    ;.b #bank(AICondition)
        sta $0A
        iny
        lda (AIOffset),Y
        sta AIParam1
        iny
        lda (AIOffset),Y
        sta AIParam2
        iny
        lda (AIOffset),Y
        sta AIParam3
        stz AIConditionMet
        lda AISkipDeadCheck
        bne Jump
        ldx AttackerOffset
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        beq Dead
        lda CharStruct::Status1,X
        and #$C0	;dead or stone
        beq NotDead
Dead:
        lda $0E
        cmp #$0F	;condition: dead
        beq Jump
        rts

NotDead:
        lda $0E
        cmp #$0F	;auto-fail condition: dead if not dead
        bne Jump
        rts

Jump:	JML [$0008]	;jump to AICondition table

.endproc

; ---------------------------------------------------------------------------

.org $C22814
.proc AICondition

_2814:
;%generatejumptable(AICondition,$12)
;vanilla values:
.word $283A, $283E, $289D, $28DB, $28EB, $291F, $2939, $29B1
.word $2A29, $2A63, $2A9D, $2AD2, $2B19, $2B2A, $2B6F, $2B87
.word $2B93, $2BC0, $2BFD

.endproc
.reloc

; ---------------------------------------------------------------------------

;AI Condition $00: Always Succeed
.proc AICondition00

        inc AIConditionMet
        rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 01: Check Status
;Param1: AITarget routine
;Param2: Status offset (0-3 for status 1-4)
;Param3: Status bits
;if checking for death status, also succeed if hp is 0 (though this behavior is bugged)
.proc AICondition01

        lda AIParam1
        jsr GetAITarget	;populates list of targets to check
        lda AIParam2
        tax
        stx $0E
        tdc
        tay
Loop:	longa
        lda AITargetOffsets,Y
        cmp #$FFFF	;end of list or no target found
        bne TargetFound
        shorta0
        bra Finish
TargetFound:
        sta $10		;target offset
        clc
        adc $0E		;status offset
        tax
        shorta0
        lda CharStruct::Status1,X	;could be status 1-4 depending
        ora CharStruct::AlwaysStatus1,X	;on status offset
        and AIParam3
        bne Match
        lda $0E
        bne Next
        lda AIParam3
        bpl Next
        ldx $10			;if asked to check death status
        lda CharStruct::CurHP,X	;also succeed if hp is 0
        ora CharStruct::CurHP,X	;**bug: should be high byte $2007
        bne Next
Match:	INC AIConditionMet
Next:	INY
        iny
        cpy #$0018	;12 characters * 2 bytes
        bne Loop
Finish:			;fail if any targets failed	
        lda AIMultiTarget
        beq Ret
        lda AITargetCount
        cmp AIConditionMet
        beq Ret
        stz AIConditionMet
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 02: HP less than value
;Param1: AITarget routine
;Param2: HP (low byte)
;Param3: HP (high byte)
.proc AICondition02

_289D:
        lda AIParam1
        jsr GetAITarget
        tdc
        tay
Loop:	longa
        lda AITargetOffsets,Y
        tax
        cmp #$FFFF	;end of list or no target found
        beq FinishMode
        lda CharStruct::CurHP,X
        cmp AIParam2
        bcs Next
        inc AIConditionMet
Next:	TDC
        shorta
        iny
        iny
        cpy #$0018	;12 characters * 2 bytes
        bne Loop
        bra Finish	;not needed (resetting mode is harmless)
FinishMode:		;need to fix A back to 8 bit
        shorta0
Finish:			;fail if any targets failed
        lda AIMultiTarget
        beq Ret
        lda AITargetCount
        cmp AIConditionMet
        beq Ret
        stz AIConditionMet
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 03: Check Variable
;Param2: Var to check (0-3)
;Param3: Value
.proc AICondition03

_28DB:
        lda AIParam2
        tax
        lda AIVars,X
        cmp AIParam3
        bne Fail
        inc AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 03: Alone 
;Param2: if 0, succeeds when completely alone
;	 if non-0, succeeds when all active monsters are the same
.proc AICondition04

_28EB:
        lda AIParam2
        bne CheckSame
        lda MonstersVisible
        jsr CountSetBits
        dex
        beq Met
        rts

CheckSame:
        lda MonsterIndex
        asl
        tax
        lda BattleMonsterID,X
        sta $0E
        tdc
        tay
Loop:	lda ActiveParticipants+4,Y
        beq Next
        tya
        asl
        tax
        lda BattleMonsterID,X
        cmp $0E
        bne Fail
Next:	INY
        cpy #$0008
        bne Loop
Met:
        inc AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 05: Compare Visible Monsters
;Param1: if 0, succeeds if visible monsters match provided value
;	 if non-0, succeeds if they do not match
;Param3: Monster Bits (1 bit per monster)
.proc AICondition05

_291F:
        lda AIParam1
        beq CheckMatch
        lda MonstersVisible
        cmp AIParam3
        bne Met
        rts

CheckMatch:
        lda MonstersVisible
        cmp AIParam3
        bne Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 06: Reaction to Command and/or Element
;Param1: if set, inverts test so a match fails the condition, and ignores element when checking commands
;Param2: Command (post-remap values)
;	 Command $07, normally BuildUp, is used as a flag to skip the command check and just check element
;Param3: Element (ignored if zero)
.proc AICondition06

_2939:
        ldx AttackerOffset
        lda ReactionFlags
        and #$01
        bne Reaction2	;Check 2nd set of reactions instead
        lda AIParam2	;command
        cmp #$07	;used as a flag to skip command check
        beq SkipCmdCheck1
        lda AIParam1	;invert checks
        beq CheckCmdMatch1
        lda AIParam2
        cmp CharStruct::Reaction1Command,X
        bne Met	;if param1 is >0, succeed when no cmd match
        rts

CheckCmdMatch1:
        lda AIParam2
        cmp CharStruct::Reaction1Command,X
        bne Fail	;if param1 is 0, fail when no cmd match
SkipCmdCheck1:		;command match or command $07 override 
        lda AIParam1
        beq CheckElemMatch1
        lda AIParam3	;element
        and CharStruct::Reaction1Element,X
        beq Met	;if param1 is >0, succeed when no elem match
        rts	;(only reachable via the $07 override)
CheckElemMatch1:
        lda AIParam3
        beq Met	;succeed when element is 0
        and CharStruct::Reaction1Element,X
        bne Met	;or when any element matches
        rts

Reaction2:	;same logic as above, but react to the second stored command
        lda AIParam2
        cmp #$07
        beq SkipCmdCheck2
        lda AIParam1
        beq CheckCmdMatch2
        lda AIParam2
        cmp CharStruct::Reaction2Command,X
        bne Met
        rts

CheckCmdMatch2:
        lda AIParam2
        cmp CharStruct::Reaction2Command,X
        bne Fail
SkipCmdCheck2:
        lda AIParam1
        beq CheckElemMatch2
        lda AIParam3
        and CharStruct::Reaction2Element,X
        beq Met
        rts

CheckElemMatch2:
        lda AIParam3
        beq Met
        and CharStruct::Reaction2Element,X
        beq Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $07: Reaction to Command and/or Category
;Param1: if set, inverts test so a match fails the condition, and ignores category when checking commands
;Param2: Command (post-remap values)
;	 Command $07, normally BuildUp, is used as a flag to skip the command check and just check category
;Param3: Category (ignored if zero)
.proc AICondition07

_29B1:
        ldx AttackerOffset
        lda ReactionFlags
        and #$01
        bne Reaction2	;Check 2nd set of reactions instead
        lda AIParam2	;command
        cmp #$07	;used as a flag to skip command check
        beq SkipCmdCheck1
        lda AIParam1	;invert checks if set
        beq CheckCmdMatch1
        lda AIParam2
        cmp CharStruct::Reaction1Command,X
        bne Met	;if param1 is >0, succeed when no cmd match
        rts

CheckCmdMatch1:
        lda AIParam2
        cmp CharStruct::Reaction1Command,X
        bne Fail	;if param1 is 0, fail when no cmd match
SkipCmdCheck1:		;command match or command $07 override
        lda AIParam1
        beq CheckCatMatch1
        lda AIParam3	;category
        and CharStruct::Reaction1Category,X
        beq Met	;if param1 >0, succeed when no category match
        rts

CheckCatMatch1:
        lda AIParam3
        beq Met	;succeed when category is 0
        and CharStruct::Reaction1Category,X
        bne Met	;or when any category matches
        rts

Reaction2:	;same logic as above, but react to the second stored command
        lda AIParam2
        cmp #$07	;used as a flag to skip command check
        beq SkipCmdCheck2
        lda AIParam1
        beq CheckCmdMatch2
        lda AIParam2
        cmp CharStruct::Reaction2Command,X
        bne Met
        rts

CheckCmdMatch2:
        lda AIParam2
        cmp CharStruct::Reaction2Command,X
        bne Fail
SkipCmdCheck2:
        lda AIParam1
        beq CheckCatMatch2
        lda AIParam3
        and CharStruct::Reaction2Category,X
        beq Met
        rts

CheckCatMatch2:
        lda AIParam3
        beq Met
        and CharStruct::Reaction2Category,X
        beq Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $08: Reaction to Magic
;Param1: if set, inverts test so a match fails the condition
;Param2: Spell
.proc AICondition08

_2A29:
        ldx AttackerOffset
        lda ReactionFlags
        and #$01	;check second set of reactions
        bne Reaction2
        lda AIParam1
        beq CheckMatch1
        lda CharStruct::Reaction1Magic,X
        cmp AIParam2
        bne Met
        rts

CheckMatch1:
        lda CharStruct::Reaction1Magic,X
        cmp AIParam2
        beq Met
        rts

Reaction2:
        lda AIParam1
        beq CheckMatch2
        lda CharStruct::Reaction2Magic,X
        cmp AIParam2
        bne Met
        rts

CheckMatch2:
        lda CharStruct::Reaction2Magic,X
        cmp AIParam2
        bne Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $09: Reaction to Item
;Param1: if set, inverts test so a match fails the condition
;Param2: Item
.proc AICondition09

_2A63:
        ldx AttackerOffset
        lda ReactionFlags
        and #$01	;check second set of reactions
        bne Reaction2
        lda AIParam1
        beq CheckMatch1
        lda CharStruct::Reaction1Item,X
        cmp AIParam2
        bne Met
        rts

CheckMatch1:
        lda CharStruct::Reaction1Item,X
        cmp AIParam2
        beq Met
        rts

Reaction2:
        lda AIParam1
        beq CheckMatch2
        lda CharStruct::Reaction2Item,X
        cmp AIParam2
        bne Met
        rts

CheckMatch2:
        lda CharStruct::Reaction2Item,X
        cmp AIParam2
        bne Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $0A: Reaction to Targetting
;Param3: if 0, succeeds when attack was single target
;	 if non-0, succeeds when attack was multi target
.proc AICondition0A

_2A9D:
        lda ReactionFlags
        and #$01	;check second set of reactions
        bne Reaction2
        ldx AttackerOffset
        lda CharStruct::Reaction1Targets,X
        jsr CountSetBits
        dex 		;targets -1
        bmi Fail	;fail for 0 targets
        jmp CheckInvert
Reaction2:
        ldx AttackerOffset
        lda CharStruct::Reaction2Targets,X
        jsr CountSetBits
        dex 		;targets -1
        bmi Fail	;fail for 0 targets
        jsr CheckInvert
Fail:	rts
CheckInvert:
        lda AIParam3	;inverts
        bne Invert
        txa
        bne Fail2	;fail for >1 targets
        beq Met	;succeed for exactly 1 target
Invert:
        txa
        beq Fail2	;fail for exactly 1 target
Met:	INC AIConditionMet
Fail2:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition 0B: Check CharStruct param
;Param1: AITarget routine
;Param2: Offset within CharStruct to check
;Param3: Value for success
.proc AICondition0B

_2AD2:
        lda AIParam1
        jsr GetAITarget	;populates list of targets to check
        lda AIParam2
        tax
        stx $0E		;Offset within CharStruct
        tdc
        tay
Loop:	longa
        lda AITargetOffsets,Y
        cmp #$FFFF	;end of list or no target found
        bne TargetFound
        shorta0
        bra Finish
TargetFound:
        clc
        adc $0E		;Offset within CharStruct
        tax
        shorta0
        lda CharStruct::CharRow,X	;check any single CharStruct byte
        cmp AIParam3		;compare with provided value
        bne :+
        inc AIConditionMet
:	INY
        iny
        cpy #$0018	;12 characters * 2 bytes
        bne Loop
Finish:			;fail if any targets failed
        lda AIMultiTarget
        beq Ret
        lda AITargetCount
        cmp AIConditionMet
        beq Ret
        stz AIConditionMet
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $0C: Compare with value at $A2?
;Param2/3: 16 bit Value to compare, succeeds if >= value at $A2
.proc AICondition0C
.reloc

_2B19:
        longa
        lda a:$00A2	;something from C1 bank	code, TODO: why must this be done?
        cmp AIParam2
        bcc :+
        inc AIConditionMet
:	TDC
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $0D: Check Event Flags
;Param2: Event Flag # (0-15)
;Param3: Value (bitmask)
;There's a special case for Event Flag 03
.proc AICondition0D

_2B2A:
        lda AIParam2
        tax
        cmp #$03		;special case
        bne CheckEventFlags	;otherwise just compares flag bits
        stz $0E
        tdc
        tax
        tay
PartyLoop:
        lda BattleData::EventFlags+3
        jsr SelectBit_X
        beq Next	;if flag bit not set, skip this party slot
        phx
        lda CharStruct::CharRow,Y
        and #$07	;character (butz, etc)
        tax
        lda $0E
        jsr SetBit_X 	;otherwise, set bit corresponding with char
        sta $0E
        plx
Next:
        longa
        tya
        clc
        adc #$0080	;next CharStruct Offset
        tay
        shorta0
        inx
        cpx #$0004	;4 party slots
        bne PartyLoop
        lda $0E		;bits set for party chars matching flag slots
        bra CheckMatch
CheckEventFlags:
        lda BattleData::EventFlags,X
CheckMatch:
        and AIParam3
        beq Ret
        inc AIConditionMet
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $0E: Reaction to Damage
.proc AICondition0E

_2B6F:
        lda ReactionFlags
        and #$01	;check second set of reactions
        bne Reaction2
        ldx AttackerOffset
        lda CharStruct::Reaction1Damage,X
        bne Met
        rts

.endproc
Reaction2:		;**bug: didn't load X for this path, but fortunately it's always? correct already
        lda CharStruct::Reaction2Damage,X
        beq Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $0F: Sets flag to skip dead monster checks (Always Succeeds)
.proc AICondition0F

_2B87:
        stz $4751
        lda #$01
        sta AISkipDeadCheck
        sta AIConditionMet
        rts

.endproc
; ---------------------------------------------------------------------------

;AI Condition $10: One party member alive
.proc AICondition10

_2B93:
        tdc
        tax
        tay
        sty $0E
PartyLoop:	;count number of active party members
        lda ActiveParticipants,Y
        beq Next
        lda CharStruct::Status1,X
        and #$C2	;Dead/Stone/Zombie
        bne Next
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        beq Next
        inc $0E    	;count of active characters
Next:	JSR NextCharOffset
        iny
        cpy #$0004	;4 party members
        bne PartyLoop
        lda $0E
        dec
        bne Fail  	;>1 party member active
        inc AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $11: Reaction to Summon
.proc AICondition11

_2BC0:
        lda ReactionFlags
        and #$01	;check second set of reactions
        bne Reaction2
        ldx AttackerOffset
        lda CharStruct::Reaction1Command,X
        cmp #$2B	;magic
        beq :+
        cmp #$17	;conjure
        bne Fail
:	lda CharStruct::Reaction1Magic,X
        cmp #$48	;first summon spell
        bcc Fail
        cmp #$57	;past last summon spell
        bcs Fail
        bra Met
Reaction2:
        ldx AttackerOffset
        lda CharStruct::Reaction2Command,X
        cmp #$2B	;magic
        beq :+
        cmp #$17	;conjure
        bne Fail
:	lda CharStruct::Reaction2Magic,X
        cmp #$48	;first summon spell
        bcc Fail
        cmp #$57	;past last summon spell
        bcs Fail
Met:	INC AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Condition $12: No Female targets available
.proc AICondition12

_2BFD:
        tdc
        tax
        tay
PartyLoop:
        lda CharStruct::CharRow,X
        and #$08	;gender
        beq Next
        lda ActiveParticipants,Y
        beq Next
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne Next
        lda CharStruct::CmdStatus,X
        and #$10	;jumping
        beq Fail  	;girl available, fail condition
Next:	JSR NextCharOffset
        iny
        cpy #$0004
        bne PartyLoop
        inc AIConditionMet
Fail:	rts

.endproc

; ---------------------------------------------------------------------------

.org $2C27
.proc GetAITarget
_2C27:
        asl
        tax
        lda f:AITarget,X
        sta $08
        lda f:AITarget+1,X
        sta $09
        lda #$C2    ;.b #bank(AITarget)
        sta $0A
        ldx #$0017	;18 bytes of AI target offsets to init
        lda #$FF
:	sta AITargetOffsets,X
        dex
        bpl :-
        stz AIMultiTarget
        stz AITargetCount
        jml [$0008]

.endproc

; ---------------------------------------------------------------------------

.org $C22C4D
.proc AITarget

_2C4D:
;%generatejumptable(AITarget,$32)
;original data
.word $2CB3, $2CDA, $2CE0, $2CE6, $2CEC, $2CF2, $2CFE, $2D0A ; $00
.word $2D16, $2D22, $2D2E, $2D3A, $2D46, $2D52, $2D79, $2DBD
.word $2DC6, $2DCD, $2DD3, $2E18, $2E25, $2E2D, $2E37, $2E44 ; $10
.word $2E4F, $2E59, $2E62, $2E65, $2EA6, $2F00, $2F0B, $2F19
.word $2F24, $2F27, $2F70, $2FAB, $2FB7, $2FC0, $2FC3, $3019 ; $20
.word $305D, $309B, $30A1, $30A7, $30AD, $30B3, $30C2, $30F1
.word $3115, $311B, $3121                                    ; $30

.endproc
.reloc

; ---------------------------------------------------------------------------

.proc AITarget00	;butz

        stz $0E
AITargetPerson:		;code reused for other party members
        tdc
        tay
        tax
Loop:
        lda CharStruct::CharRow,X
        and #$07	;just character bits
        cmp $0E
        bne Next
        lda ActiveParticipants,Y
        beq Ret
        jsr CheckTargetValid
        bne Ret
        stx AITargetOffsets
        bra Ret
Next:	JSR NextCharOffset
        iny
        cpy #$0004	;4 chars to check
        bne Loop
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget01	;lenna

        lda #$01
        sta $0E
        bra AITarget00::AITargetPerson

.endproc

; ---------------------------------------------------------------------------

.proc AITarget02 	;galuf
        lda #$02
        sta $0E
        bra AITarget00::AITargetPerson

.endproc

; ---------------------------------------------------------------------------


.proc AITarget03	;faris
        lda #$03
        sta $0E
        bra AITarget00::AITargetPerson

.endproc

; ---------------------------------------------------------------------------

.proc AITarget04	;krile
        lda #$04
        sta $0E
        bra AITarget00::AITargetPerson
;**optimize: would be very easy to combine these 8 monster target routines

.endproc

; ---------------------------------------------------------------------------

.proc AITarget05 	;monster 1
        lda ActiveParticipants+4
        beq Ret
        ldx #.sizeof(CharStruct)*4
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget06 	;monster 2
        lda ActiveParticipants+5
        beq Ret
        ldx #.sizeof(CharStruct)*5
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget07 	;monster 3
        lda ActiveParticipants+6
        beq Ret
        ldx #.sizeof(CharStruct)*6
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget08 	;monster 4
        lda ActiveParticipants+7
        beq Ret
        ldx #.sizeof(CharStruct)*7
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget09 	;monster 5
        lda ActiveParticipants+8
        beq Ret
        ldx #.sizeof(CharStruct)*8
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget0A 	;monster 6
        lda ActiveParticipants+9
        beq Ret
        ldx #.sizeof(CharStruct)*9
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget0B 	;monster 7
        lda ActiveParticipants+10
        beq Ret
        ldx #.sizeof(CharStruct)*10
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget0C 	;monster 8
        lda ActiveParticipants+11
        beq Ret
        ldx #.sizeof(CharStruct)*11
        stx AITargetOffsets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget0D	;self, unless forced
        lda ReactingIndexType
        beq Self
        dec
        beq ForcedMonster
        dec ReactingIndexType
        lda ReactingIndex
        bra Target
ForcedMonster:
        clc
        lda ReactingIndex
        adc #$04
        bra Target
Self:
        lda AttackerIndex
Target:
        longa
        jsr ShiftMultiply_128
        tax
        shorta0
        stx AITargetOffsets
        rts

.endproc
; ---------------------------------------------------------------------------

.proc AITarget0E	;all active monsters except attacker
        lda MonsterIndex
        sta $12		;excluded monster
        inc AIMultiTarget
AITargetMonstersExcept:
        tdc
        tax
        stx $0E		;first open AITargetOffsets slot
        stx $10		;currently checked monster
        ldx #.sizeof(CharStruct)*4	;first monster offset
Loop:
        ldy $10
        lda ActiveParticipants+4,Y
        beq Next
        lda $10		;currently checked monster
        cmp $12		;excluded monster
        beq Next
        jsr CheckTargetValid
        bne Next
        ldy $0E		;first open AITargetOffsets slot
        stx $08		;monster offset
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E		;next slot
        inc AITargetCount
Next:	JSR NextCharOffset
        inc $10
        lda $10		;monster counter
        cmp #$08	;8 monsters
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget0F	;all active monsters
        lda #$FF
        sta $12		;excluded monster
        inc AIMultiTarget
        bra AITarget0E::AITargetMonstersExcept

.endproc

; ---------------------------------------------------------------------------

.proc AITarget10	;random active monster except attacker
        lda MonsterIndex
        sta $12		;excluded monster
        bra AITarget0E::AITargetMonstersExcept

.endproc

; ---------------------------------------------------------------------------

.proc AITarget11	;random active monster
        lda #$FF
        sta $12		;excluded monster
        bra AITarget0E::AITargetMonstersExcept

.endproc

; ---------------------------------------------------------------------------

.proc AITarget12	;all front row active party 
        lda #$80
        sta $12		;check row bit
        stz $13		;front row
        inc AIMultiTarget
AITargetCharRow:
        tdc
        tax
        stx $0E		;first open AITargetOffsets slot
        stx $10
Loop:
        ldy $10		;currently checked character
        lda ActiveParticipants,Y
        beq Next
        jsr CheckTargetValid
        bne Next
        lda CharStruct::CharRow,X
        and $12		;bits to check
        cmp $13		;desired value
        bne Next
        ldy $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E		;next slot
        inc AITargetCount
Next:	JSR NextCharOffset
        inc $10
        lda $10
        cmp #$04	;4 party members to check
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget13	;all back row active party
        lda #$80
        sta $12		;check row bit
        lda #$80
        sta $13		;back row
        inc AIMultiTarget
        bra AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------


.proc AITarget14	;random front row active party							
        lda #$80
        sta $12		;check row bit
        stz $13		;front row
        bra AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget15	;random back row active party
        lda #$80
        sta $12		;check row bit
        lda #$80
        sta $13		;back row
        bra AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget16	;all female active party						
        lda #$08
        sta $12		;check gender bit
        lda #$08
        sta $13		;female
        inc AIMultiTarget
        bra AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget17	;all male active party
        lda #$08
        sta $12		;check gender bit
        stz $13		;male
        inc AIMultiTarget
        bra AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget18	;random female active party
        lda #$08
        sta $12		;check gender bit
        lda #$08
        sta $13		;female
        bra AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget19	;random male active party
        lda #$08
        sta $12		;check gender bit
        stz $13		;male
        jmp AITarget12::AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget1A	;all dead party
        inc AIMultiTarget
;continues to next routine

.endproc

; ---------------------------------------------------------------------------

.proc AITarget1B	;random dead party
        tdc
        tax
        stx $0E
        stx $10
Loop:
        lda CharStruct::Status1,X
        and #$40	;stone
        bne Next
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne Next
        lda CharStruct::CmdStatus,X
        and #$10	;jumping
        bne Next
        lda CharStruct::Status1,X
        bpl Next	;skip if alive
        ldy $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E
        inc AITargetCount
Next:	JSR NextCharOffset
        inc $10
        lda $10
        cmp #$04	;4 party members
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget1C	;all monsters with reflect
        lda #$1C	;status 3
        tax
        stx $14
        lda #$80	;reflect
        sta $12
        inc AIMultiTarget
AITargetMonsterStatus:
        tdc
        tax
        stx $0E
        stx $10
        ldx #.sizeof(CharStruct)*4
Loop:
        phx
        ldy $10
        lda ActiveParticipants+4,Y
        beq Next
        jsr CheckTargetValid
        bne Next
        longa
        clc
        txa
        adc $14		;status offset within CharStruct
        tax
        shorta0
        lda $2000,X	;selected status
        ora $2056,X	;always selected status
        and $12
        cmp $12		;status to check
        bne Next
        ldy $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E
        inc AITargetCount
Next:	PLX
        jsr NextCharOffset
        inc $10
        lda $10
        cmp #$08	;8 monsters
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget1D	;random monster with reflect
        lda #$1C	;status 3
        tax
        stx $14
        lda #$80	;reflect
        sta $12
        bra AITarget1C::AITargetMonsterStatus

.endproc

; ---------------------------------------------------------------------------

.proc AITarget1E	;all monsters with critical hp							
        lda #$1D	;status 4
        tax
        stx $14
        lda #$02	;critical hp
        sta $12
        inc AIMultiTarget
        bra AITarget1C::AITargetMonsterStatus
        
.endproc

; ---------------------------------------------------------------------------

.proc AITarget1F	;random monster with critical hp			
        lda #$1D	;status 4
        tax
        stx $14
        lda #$02	;critical hp
        sta $12
        bra AITarget1C::AITargetMonsterStatus
                
.endproc

; ---------------------------------------------------------------------------

.proc AITarget20	;all monsters with under half hp
        inc AIMultiTarget
;continues to next routine

.endproc

; ---------------------------------------------------------------------------

						
.proc AITarget21	;random monster with under half hp
        tdc
        tax
        stx $0E
        stx $10
        ldx #.sizeof(CharStruct)*4
Loop:
        ldy $10
        lda ActiveParticipants+4,Y
        beq Next
        jsr CheckTargetValid
        bne Next
        longa
        lda CharStruct::MaxHP,X
        lsr
        cmp CharStruct::CurHP,X
        bcs BelowHalf
        shorta0
        bra Next
BelowHalf:
        shorta0
        ldy $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E
        inc AITargetCount
Next:	JSR NextCharOffset
        inc $10
        lda $10
        cmp #$08	;8 monsters
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget22	;random party member with reflect
        tdc
        tax
        stx $0E
        stx $10
Loop:	LDY $10
        lda ActiveParticipants,Y
        beq Next
        jsr CheckTargetValid
        bne Next
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        bpl Next
        ldy $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E
        inc AITargetCount
Next:	JSR NextCharOffset
        inc $10
        lda $10
        cmp #$04	;4 party members
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget23	;all party members on the team?
        lda #$40
        sta $12		;on the team bit?
        stz $13
        inc AIMultiTarget
        jmp AITargetCharRow
                
.endproc

; ---------------------------------------------------------------------------

.proc AITarget24	;random party member on the team?
        lda #$40
        sta $12		;on the team bit?
        stz $13
        jmp AITargetCharRow

.endproc

; ---------------------------------------------------------------------------

.proc AITarget25	;all dead monsters
        inc AIMultiTarget
;continues to next routine
        
.endproc

; ---------------------------------------------------------------------------

.proc AITarget26	;random dead monster
        tdc
        tax
        stx $0E
        stx $10
        ldx #.sizeof(CharStruct)*4
Loop:	lda CharStruct::Status1,X
        and #$40	;stone
        bne Next
        cpx #.sizeof(CharStruct)*4
        bcc :+		;branch can never occur?
        lda SandwormBattle
        bne SkipChecks
:	lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne Next
        lda CharStruct::CmdStatus,X
        and #$10	;jumping
        bne Next
SkipChecks:
        lda CharStruct::Status1,X
        bmi Dead
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        bne Next
Dead:	LDY $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E
        inc AITargetCount
Next:	JSR NextCharOffset
        inc $10
        lda $10
        cmp #$08	;8 monsters
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget27

_3019:
;first monster target matching ai bits (??), excluding attacker or ReactingIndex if that's set
        	;not sure exactly what's going on here, also data it doesn't expect could easily cause an infinite loop 
        tdc
        tay
        sec
        lda AIScriptOffset
        sbc #$1B	;???
        tax
        lda MonsterAIScript,X
        beq Ret
        sta $12		;target bits from somewhere in ai script
        lda ReactingIndexType
        bne _ReactingIndex
        sec
        lda AttackerIndex
        sbc #$04
        bra FindTarget
_ReactingIndex:
        lda ReactingIndex
FindTarget:
        tax
        stx $10		;monster index to avoid
        lda $12		;bitfield of acceptable targets
BitLoop:
        asl
        bcs BitSet
        iny
        bra BitLoop
BitSet:
        cpy $10
        bne FoundTarget
        iny
        bra BitLoop
FoundTarget:
        tya
        clc
        adc #$04
        tax
        inc ActiveParticipants,X	;target forced active
        longa
        jsr ShiftMultiply_128
        sta AITargetOffsets
        shorta0
Ret:	rts
        
.endproc

; ---------------------------------------------------------------------------

.proc AITarget28 	;butz if jumping but not yet intercepted
        stz $0E		;butz
AITargetPersonJumping:
        tdc
        tay
        tax
Loop:	lda CharStruct::CharRow,X
        and #$07	;character bits
        cmp $0E
        bne Next
        lda ActiveParticipants,Y
        beq Ret
        lda CharStruct::Status1,X
        and #$C0	;stone or dead
        bne Ret
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne Ret
        lda CharStruct::CmdStatus,X
        and #$10	;jumping
        beq Ret	;aborts if NOT jumping
        lda CharStruct::Command,X
        cmp #$50	;jump intercepted
        beq Ret	;already intercepted
        stx AITargetOffsets
        bra Ret
Next:	JSR NextCharOffset
        iny
        cpy #$0004	;4 chars to check
        bne Loop
Ret:	rts
        
.endproc

; ---------------------------------------------------------------------------

.proc AITarget29	;lenna if jumping but not yet intercepted							
 
        lda #$01	;lenna
        sta $0E
        bra AITarget28::AITargetPersonJumping
.endproc 

; ---------------------------------------------------------------------------

.proc AITarget2A	;galuf if jumping but not yet intercepted

        lda #$02	;galuf
        sta $0E
        bra AITarget28::AITargetPersonJumping                

.endproc

; ---------------------------------------------------------------------------

.proc AITarget2B	;faris if jumping but not yet intercepted
        lda #$03	;faris
        sta $0E
        bra AITarget28::AITargetPersonJumping
                
.endproc

; ---------------------------------------------------------------------------

.proc AITarget2C	;krile if jumping but not yet intercepted
        lda #$04	;krile
        sta $0E
        bra AITarget28::AITargetPersonJumping

.endproc

; ---------------------------------------------------------------------------

.proc AITarget2D	;acted this tick
        lda ActedIndex
        longa
        jsr ShiftMultiply_128
        sta AITargetOffsets
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget2E	;all party matching a bitmask from event flags?
        inc AIMultiTarget
        tdc
        tax
        stx $0E
        stx $10
Loop:	LDX $0E
        lda BattleData::EventFlags+3	;???
        jsr SelectBit_X
        beq Next
        longa
        ldx $10
        lda $0E
        jsr ShiftMultiply_128
        sta AITargetOffsets,X
        inc $10
        inc $10
        shorta0
Next:	INC $0E
        lda $0E
        cmp #$04	;4 chars to check
        bne Loop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc AITarget2F	;butz if dead
        stz $0E		;butz
AITargetPersonDead:	;reused for other characters, note there is no krile version
        tdc
        tay
        tax
Loop:	lda CharStruct::CharRow,X
        and #$07	;character bits
        cmp $0E
        bne Next
        lda CharStruct::Status1,X
        and #$80	;dead
        beq Ret
        stx AITargetOffsets
        bra Ret
Next:	JSR NextCharOffset
        iny
        cpy #$0004	;4 chars to check
        bne Loop
Ret:	rts
        
.endproc

; ---------------------------------------------------------------------------

						
.proc AITarget30	;lenna if dead
        lda #$01	;lenna
        sta $0E
        bra AITarget2F::AITargetPersonDead
   
.endproc

; ---------------------------------------------------------------------------

.proc AITarget31	;galuf if dead							
        lda #$02	;galuf
        sta $0E
        bra AITarget2F::AITargetPersonDead

.endproc

; ---------------------------------------------------------------------------

;faris if dead
.proc AITarget32		;faris
        lda #$03
        sta $0E
        bra AITarget2F::AITargetPersonDead

.endproc

; ---------------------------------------------------------------------------


;used by AI routines, but seems like it could be useful elsewhere
;returns non-zero if target is dead, stone, erased, hidden or jumping
.proc CheckTargetValid

_3127:
        lda CharStruct::Status1,X
        and #$C0	;dead or stone
        bne Ret
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne Ret
        lda CharStruct::CmdStatus,X
        and #$10	;jumping
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Processes the current AI script into the AI command buffer, then executes it
.org $313B
.proc ProcessAIScript
_313B:
        ldx #$0040   	;init 64 byte buffer to $FF
        lda #$FF
:	sta AIBuffer,X
        dex
        bpl :-
        lda MonsterIndex
        asl
        tax
        lda AICurrentOffset,X
        sta $0E		;current ai offset
        lda AICurrentOffset+1,X
        sta $0F
        tdc
        tax
        ldy $0E		;current ai offset
ProcessScript:
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        cmp #$FE	;end of entry
        bne :+
        jmp EndSequence
:	CMP #$FF	;end of ai
        bne :+
        jmp EndSequence
:	CMP #$FD	;special command
        bne NotSpecial
;special commands (Flag byte $FD)
        lda (AIOffset),Y	;copy command byte
        sta AIBuffer,X
        iny
        inx
        cmp #$F7
        beq CopyExtraBytes
        cmp #$F0
        bcs OtherSpecial
        lda (AIOffset),Y	;copy 2 data bytes
        sta AIBuffer,X
        iny
        inx
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        lda (AIOffset),Y
        cmp #$FE		;check for end
        beq EndSequence
        cmp #$FF
        beq EndSequence
        bra UpdateOffset
CopyExtraBytes:	;F7 command, copies 2 bytes then extra bytes depending on the first (min 1)
        lda (AIOffset),Y	;number of extra data bytes to copy
        sta AIBuffer,X		;.. must be at least 1
        sta $10
        iny
        inx
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        stz $11
:	lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        inc $11
        lda $11
        cmp $10
        bne :-
        dex
        lda AIBuffer,X		;check the last byte copied
        cmp #$FE
        beq EndSequence
        cmp #$FF
        beq EndSequence
        bra UpdateOffset
OtherSpecial:	;>= F0, but not F7
        	;copies 2 more data bytes then continues processing
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        jmp ProcessScript
NotSpecial:	
        lda (AIOffset),Y	;check for end
        cmp #$FE
        beq EndSequence
        cmp #$FF
        beq EndSequence
UpdateOffset:
        sty $0E		;update current ai offset (position in script)
        lda MonsterIndex
        asl
        tay
        lda $0E
        sta AICurrentOffset,Y
        lda $0F
        sta AICurrentOffset+1,Y
        bra DispatchAICommands
EndSequence:	;FE or FF, resets offset to start of set
        lda MonsterIndex
        asl
        tay
        lda AICurrentCheckedSet
        asl
        tax
        lda ROMTimes64w,X
        sta AICurrentOffset,Y
        lda ROMTimes64w+1,X
        sta AICurrentOffset+1,Y
;continues into next routine

.endproc

; ---------------------------------------------------------------------------

;Sends AI commands to their proper processing routines
;	operates from the top of AIBuffer until it hits a $FE or $FF
;	$FD indicates a special command in the next byte(s)
;	anything else is a spell to cast
.org $3210
.proc DispatchAICommands

_3210:
        stz $0E
        ldx MonsterOffset32
InitMMTargets:
        stz MMTargets,X		;targets for monster magic
        inx
        inc $0E			;loop index
        lda $0E
        cmp #$20      		;32 byte structure
        bne InitMMTargets
        ldx MonsterOffset16
        stx MMOffset
        ldx MonsterOffset32
        stx MMTargetOffset
        lda MonsterIndex
        asl
        tax
        lda ROMTimes100w,X
        sta AIScriptOffset
        lda ROMTimes100w+1,X
        sta AIScriptOffset+1
        tdc
        tay
        sty AIBufferOffset
        sty AISpellCount
Loop:
        ldx AIScriptOffset
        ldy AIBufferOffset
        lda AIBuffer,Y
        sta MonsterAIScript::Flag,X
        cmp #$FE
        beq Ret
        cmp #$FF
        beq Ret
        cmp #$FD
        bcs Special
CastSpell:	;values < $FD are just a spell to cast
        jsr AICastNormalSpell
        bra GoLoop
Special:	;FD is a flag for special commands
        lda AIBuffer+1,Y
        cmp #$F0
        bcs :+
        jsr AIChoose1of3	;command < $F0 means a list of 3 spells
        bra CastSpell
:	CMP #$F2
        bne :+
        jsr AIShowMonster	;command $F2
        bra GoLoop
:	CMP #$F3
        bne :+
        jsr AISetTarget     	;command $F3
        bra GoLoop
:	CMP #$F4
        bne :+
        jsr AISetVar     	;command $F4
        bra GoLoop
:	CMP #$F9
        bne :+
        jsr AISetEventFlag     	;command $F9
        bra GoLoop
:	CMP #$FA
        bne :+
        jsr AISetStatsToggleStatus     	;command $FA
        bra GoLoop
:	JSR AICopyCommand     	;could be F0,F1,F5-F8,FB-FF
        			;.. just copies command as it is
GoLoop:
        bra Loop
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Spell is passed in A
;Includes things like "Monster Fight" which are all spells
.proc AICastNormalSpell

_329C:
        ldx MMOffset
        cmp #$F1
        bne QueueSpell
        ;$f1 is a special case command that aborts the action?
        ldx AIScriptOffset
        lda #$FF	;flag to be ignored by graphics routines?
        sta MonsterAIScript::Flag,X
        stz $16
        lda MMOffset
        lsr
        ror $16
        lsr
        ror $16
        lsr
        ror $16
        lsr
        ror $16		;low 4bits of MMOffset in high 4 of $16
        lda $16
        bne Dummy	;at least one action was queued before this?
        ;if nothing else has been queued consider the action complete?
        ldx AttackerOffset
        stz CharStruct::Command,X
        lda #$80	;physical/other
        sta CharStruct::ActionFlag,X
Dummy:
        lda #$78	;queue failure/dummy spell?
        ldx MMOffset
QueueSpell:								
        sta MonsterMagic,X
        sta $16		;spell to cast
        ldx AttackerOffset
        lda AISpellCount
        cmp #$02
        bcs NextMM	;if queued spells >= 2, don't bother with Char
        dec
        beq Add2ndSpell
        ;if 0, put first spell selection in CharStruct
        lda $16
        sta CharStruct::SelectedItem,X
        bra NextMM
Add2ndSpell:	;if 1, put in 2nd slot
        lda $16
        sta CharStruct::SecondSelectedItem,X
        lda #$21	;magic, costs mp
        sta CharStruct::SecondActionFlag,X
NextMM:	
        inc MMOffset
        inc AISpellCount
        ldx AIScriptOffset
        stz $17
WipeAIScript:		;wipes 20 bytes of MonsterAIScript 
        lda #$FF
        sta MonsterAIScript,X
        inx
        longa
        inc AIScriptOffset
        shorta
        inc $17		;loop index
        lda $17
        cmp #$14
        bne WipeAIScript
        longa
        dec AIScriptOffset   ;unsure why, incremented at end anyway
        shorta
        ldx MMTargetOffset
        lda MMTargets::Party,X
        ora MMTargets::Monster,X
        beq NoTarget
        inc MMTargetOffset
        inc MMTargetOffset
        bra Finish
NoTarget:	;our spell doesn't have a target yet, check if one was forced
        lda MonsterIndex
        asl
        tax
        lda ForcedTarget::Party,X
        ora ForcedTarget::Monster,X
        beq StillNoTarget
        ldy MMTargetOffset
        lda ForcedTarget::Party,X
        sta MMTargets::Party,Y
        lda ForcedTarget::Monster,X
        sta MMTargets::Monster,Y
        inc MMTargetOffset
        inc MMTargetOffset
        bra Finish
StillNoTarget:	;we still don't have a target, so call a routine to set it
        lda $16		;spell to cast
        jsr AISpellTarget
Finish:
        longa
        inc AIScriptOffset
        inc AIBufferOffset
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

;First spell in A when called
.proc AIChoose1of3

_334D:
        sta $2720		;scratch area
        lda AIBuffer+2,Y
        sta $2721
        lda AIBuffer+3,Y
        sta $2722
        inc AIBufferOffset	;incrememnt buffer to account
        inc AIBufferOffset	;.. for the extra 3 bytes
        inc AIBufferOffset
        tdc
        tax
        lda #$02
        jsr Random_X_A
        tax
        lda $2720,X
        rts

.endproc

; ---------------------------------------------------------------------------

;AI Command $F2, Show Monster
;Param1/Type:  	bitfield but only $40 (random) is supported
;Param2/Data1: 	bitfield of valid monsters to show
;		.. or if 0, picks a random monster other than self
.proc AIShowMonster

_336E:
        jsr AICopyCommand
        lda CurrentlyReacting
        beq Normal
        phx
        lda MonsterIndex
        tax
        lda FinalTarget2Bits	;remove self from any pending reactions
        jsr ClearBit_X
        sta FinalTarget2Bits
        plx
Normal:
        lda $0F     	;monsters to show
        bne ShowFromSet
        ;no monsters provided, pick one at random
        phx
TryRandomMonster:
        tdc
        tax
        lda #$07
        jsr Random_X_A
        sta $10
        cmp MonsterIndex
        beq TryRandomMonster  	;picked self, try again
        asl
        tax
        lda BattleMonsterID,X
        and BattleMonsterID+1,X
        cmp #$FF
        beq TryRandomMonster	;picked empty slot, try again
        lda MonsterIndex
        tax
        tdc
        jsr SetBit_X  	;current monster always shown
        pha
        lda $10
        tax
        pla
        jsr SetBit_X  	;randomly picked monster also shown
        plx
        sta MonsterAIScript::Data1,X
        rts
ShowFromSet:								
        lda $0E     	;command type bits
        and #$40	;but only $40 (random) is supported
        beq Ret
        phx
TryRandomSet:
        tdc
        tax
        lda #$07
        jsr Random_X_A
        sta $10
        asl
        tax
        lda BattleMonsterID,X
        and BattleMonsterID+1,X
        cmp #$FF
        beq TryRandomSet	;empty slot, try again
        lda $10
        tax
        tdc
        jsr SetBit_X
        and $0F			;monsters to show
        beq TryRandomSet	;not in set, try again
        lda $10
        tax
        tdc
        jsr SetBit_X   	;set the bit again for no reason
        plx
        sta MonsterAIScript::Data1,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Command $F3: Set Target
;Param1/Type: AITarget routine
.proc AISetTarget

_33EC:
        jsr AICopyCommand
        lda ReactingIndexType
        beq :+
        inc ReactingIndexType
:	lda $0E
        jsr GetAITarget
        lda AITargetOffsets
        and AITargetOffsets+1
        cmp #$FF	;no targets
        beq Ret
        tdc
        tax
        tay
        stx $0E
Loop:	;loop until we run out of targets
        longa
        lda AITargetOffsets,Y
        jsr ShiftDivide_128	;offset->index
        tax
        shorta0
        cpx #$000C	;>=12 means we're done
        bcs CheckMulti
        txa
        cmp #$04	;monster check
        bcs Monster
        lda $0E
        jsr SetBit_X
        sta $0E
        bra Next
Monster:
        sec
        sbc #$04
        tax
        lda $0F
        jsr  SetBit_X
        sta $0F
Next:	INY
        iny
        bra Loop
CheckMulti:
        lda AIMultiTarget
        bne Finish	;use as-is for multitarget, or select 1 random
TryRandom:
        tdc
        tax
        lda #$07
        jsr Random_X_A
        tax
        lda $0E		;party targets
        jsr SelectBit_X
        beq CheckMonster
        tdc
        jsr SetBit_X
        sta $0E
        bra Finish
CheckMonster:
        lda $0F		;monster targets
        jsr SelectBit_X
        beq TryRandom	;failed both, try again
        tdc
        jsr SetBit_X
        sta $0F
Finish:
        ldx MMTargetOffset
        lda $0E
        sta MMTargets::Party,X
        lda $0F
        sta MMTargets::Monster,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;AI Command $F4: Set Variable
;Param1: Variable #
;Param2: Value
.proc AISetVar

_346F:
        jsr AICopyCommand
        lda $0E
        tax
        lda $0F
        sta AIVars,X
        rts

.endproc

; ---------------------------------------------------------------------------

;AI Command $F9: Set Event Flag
;Param1: Event Flags Byte
;Param2: Bits to set
.proc AISetEventFlag

_347B:
        jsr AICopyCommand
        lda $0E
        tax
        lda BattleData::EventFlags,X
        ora $0F
        sta BattleData::EventFlags,X
        rts

.endproc

; ---------------------------------------------------------------------------

;AI Command $FA: Set Stats or Toggle Status 
;Param1: Offset within CharStruct
;Param2: Value to set, or Status to toggle (only highest status bit applies)
.proc AISetStatsToggleStatus

_348A:
        jsr AICopyCommand
        lda $0E
        longa
        clc
        adc AttackerOffset
        tax
        shorta0
        lda $0E
        cmp #$1A	;before Status1
        bcc SetDirect
        cmp #$1E	;before CmdStatus
        bcc ToggleStatus
        cmp #$70	;before AlwaysStatus
        bcc SetDirect
        cmp #$74	;after AlwaysStatus
        bcs SetDirect
ToggleStatus:		;also applies to always status
        tdc
        tay
        lda $0F
        beq Ret	;0 would cause infinite loop, so abort
FindFirstSetBit:
        asl
        bcs Found
        iny
        bra FindFirstSetBit
Found:		;now set this status if not set, or clear it if already set
        phx
        tyx
        lda ROMBitUnset,X
        sta $0E
        plx
        lda CharStruct,X
        and $0F
        eor $0F
        sta $10
        lda CharStruct,X
        and $0E
        ora $10
        sta CharStruct,X
Ret:	rts
SetDirect:	;for non-status offsets, just set it to the provided value
        lda $0F
        sta CharStruct,X
        rts

.endproc

; ---------------------------------------------------------------------------

;copies one 5 byte command from AIbuffer to MonsterAIScript
;first byte ($FD) has already been copied before this is called
;also puts the first 2 command params in $0E and $0F
.proc AICopyCommand

_34D9:
        sta MonsterAIScript::Cmd,X
        lda AIBuffer+2,Y
        sta MonsterAIScript::Type,X
        sta $0E
        lda AIBuffer+3,Y
        sta MonsterAIScript::Data1,X
        sta $0F
        sta MonsterAIScript::Data2,X
        longa
        clc
        lda AIScriptOffset
        adc #$0005		;advance past this command
        sta AIScriptOffset
        shorta0
        clc
        lda AIBufferOffset
        adc #$04		;advance mostly past this command
        sta AIBufferOffset	;.. last byte is in called routine
        rts

.endproc

; ---------------------------------------------------------------------------

;called when a queued MonsterMagic doesn't have a target
;A is the spell being cast
.proc AISpellTarget

_3504:
        pha
        pla
        bne :+
        stz $17
        jmp RandomParty
:	CMP #$EC	;a dummy spell
        bne SpellTargetting
        lda #$F0	;all party members
        sta $16		;party targets
        stz $17
        tdc
        tax
RemoveInactive:
        lda ActiveParticipants,X
        beq Next
        lda $16
        jsr ClearBit_X
        sta $16
Next:	INX
        cpx #$0004
        bne RemoveInactive
        jmp Finish
SpellTargetting:
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        sta TempTargetting
        tdc
        tay
        sty $16
        lda TempTargetting
        bne CheckRoulette
        ;no targetting info, so target self
        lda MonsterIndex
        tax
        tdc
        jsr SetBit_X
        sta $17
        bra Finish
CheckRoulette:
        and #$04	;roulette
        beq NormalTargetting
TryRandom:
        tdc
        tax
        lda #$0B
        jsr Random_X_A	;0..11, random target
        sta $16
        tay
        lda ActiveParticipants,Y
        beq TryRandom	;inactive target, try again
        lda $16
        cmp #$04	;monster check
        bcs Monster
        tax
        tdc
        jsr SetBit_X
        sta $16		;party target
        bra Finish
Monster:
        stz $16
        sec
        sbc #$04
        tax
        tdc
        jsr SetBit_X
        sta $17		;monster target
        bra Finish
NormalTargetting:
        lda TempTargetting
        and #$40	;hits all
        bne All
        lda TempTargetting
        and #$08	;targets enemy
        bne Enemy
        lda TempCharm	;flag for charm, inverts targetting
        bne RandomParty
RandomMonster:
        tdc
        tax
        lda #$07
        jsr Random_X_A
        tax
        tdc
        jsr SetBit_X
        sta $17		;monster target
        bra Finish
Enemy:
        lda TempCharm	;flag for charm, inverts targetting
        bne RandomMonster
RandomParty:
        tdc
        tax
        lda #$03
        jsr Random_X_A
        tax
        tdc
        jsr SetBit_X
        sta $16		;party target
        bra Finish
All:
        lda TempTargetting
        and #$08	;target enemy
        bne AllEnemy
        lda TempCharm
        bne AllParty
AllMonster:
        lda #$FF	;all monsters
        sta $17		;monster targets
        bra Finish
AllEnemy:
        lda TempCharm
        bne AllMonster
AllParty:
        lda #$F0	;all party members
        sta $16		;party targets
Finish:
        ldx MMTargetOffset
        lda $16
        sta MMTargets::Party,X
        lda $17
        sta MMTargets::Monster,X
        inc MMTargetOffset
        inc MMTargetOffset
        rts

.endproc

; ---------------------------------------------------------------------------

;Check for and launch any reactions to the attacks this tick
;Includes things like waking from sleep but also AI scripted reactions
.proc HandleReactions

_35E3:
        lda #$01
        sta ReactingIndexType
        lda CurrentlyReacting
        bne StartReactions
        lda TurnProcessed
        bne :+
        jmp Finish
:	lda AttackerIndex
        sta ActedIndex
        lda #$FF
        sta ReactingIndex
        stz ReactionFlags
        stz FinalTargetBits
        stz TargetWasParty
        stz FinalTarget2Bits
        stz Target2WasParty
        lda ActionAnim0::Flags
        and #$40	;target was a monster
        bne :+
        inc TargetWasParty
:	lda ActionAnim0::TargetBits
        sta FinalTargetBits
        bne TargetSet
        lda ActionAnim0::ReflecteeBits
        beq TargetSet
        sta FinalTargetBits
        lda TargetWasParty
        eor #$01
        sta TargetWasParty
TargetSet:
        lda #$80
        sta ReactionFlags
        lda Skip2ndReactionCheck	;may be unused?
        cmp #$FF
        beq StartReactions		;skips 2nd reaction setup
        lda ActionAnim1::Flags
        and #$40			;target was a monster
        bne :+
        inc Target2WasParty
:	lda ActionAnim1::TargetBits
        sta FinalTarget2Bits
        bne Target2Set
        lda ActionAnim1::ReflecteeBits
        beq Target2Set
        sta FinalTarget2Bits
        lda Target2WasParty
        eor #$01
        sta Target2WasParty
Target2Set:
        lda ReactionFlags
        ora #$40
        sta ReactionFlags
StartReactions:
        lda PendingReactions
        beq :+
        jmp Finish
:	lda ReactingIndex
        bmi :+
        jsr RestoreActionData
:	lda ReactionFlags
        and #$01	;should check second reaction instead
        beq CheckTargetsLoop
        jmp CheckTargets2Loop
CheckTargetsLoop:
        tdc
        tax
:	lda FinalTargetBits
        jsr SelectBit_X
        bne FoundTarget
        inx
        cpx #$0008
        bne :-
        inc ReactionFlags	;switch to second reaction
        jmp CheckTargets2Loop
FoundTarget:
        lda FinalTargetBits
        jsr ClearBit_X
        sta FinalTargetBits
        txa
        sta ReactingIndex
        lda TargetWasParty
        bne PartyTarget
        clc
        lda ReactingIndex
        adc #$04	;now character index
        bra MonsterTarget
PartyTarget:
        lda ReactingIndex
MonsterTarget:
        tax
        lda ActiveParticipants,X
        beq CheckTargetsLoop
        txa
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        and #$40	;stone
        bne CheckTargetsLoop
        lda CharStruct::Status3,X
        and #$10	;stop
        bne CheckTargetsLoop
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne CheckTargetsLoop
        lda TargetWasParty
        beq MonsterChecks
        jmp PartyChecks
MonsterChecks:
        lda CharStruct::Reaction1Magic,X
        cmp #$80	;monster fight
        beq Fight
        cmp #$81	;monster special
        beq Fight
        lda CharStruct::Reaction1Command,X
        cmp #$04	;fight
        beq Fight
        cmp #$0B	;capture/mug
        beq Fight
        cmp #$15	;aim
        beq Fight
        cmp #$16	;x-fight
        beq Fight
        cmp #$2C	;simple fight (no procs)
        beq Fight
        cmp #$2D	;jump landing
        beq Fight
        cmp #$33	;double lance
        bne CheckReactions	;**bug: missing swordslap
Fight:	;remove relevant statuses after getting hit
        lda CharStruct::Status4,X
        and #$DF	;clear controlled status
        sta CharStruct::Status4,X
        clc
        lda ReactingIndex
        adc #$04
        sta $0E		;char index
        tdc
        tay
:	lda ControlTarget,Y
        cmp $0E
        beq ClearControl
        iny
        cpy #$0004
        bne :-
        bra CheckSleep
ClearControl:
        tdc
        sta ControlTarget,Y
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $10		;stored status2
        lda ReactingIndex
        asl
        tax
        stz ForcedTarget::Party,X
        stz ForcedTarget::Monster,X
        bra WakeUp
CheckSleep:
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $10		;stored status2
        lda ReactingIndex
        jsr ShiftMultiply_4
        tay
        lda MonsterCombinedStatus::S2,Y	;8 is first monster
        and #$40	;sleep
        beq Awake
WakeUp:
        phx
        lda $0E		;char index
        jsr ResetATB
        plx
        lda CharStruct::Status2,X
        and #$BF	;clear sleep
        sta CharStruct::Status2,X
Awake:
        lda ReactingIndex
        jsr ShiftMultiply_4
        tay
        lda MonsterCombinedStatus::S2,Y	;8 is first monster
        and #$10	;charm
        beq :+
        lda CharStruct::Status2,X
        and #$EF	;clear charm
        sta CharStruct::Status2,X
:	lda $10		;stored status2
        and #$78	;sleep/para/charm/berserk
        beq CheckReactions
GoCheckTargetsLoop:
        jmp CheckTargetsLoop	;look for next target to check
CheckReactions:		
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$78	;sleep/para/charm/berserk
        bne GoCheckTargetsLoop
        lda CharStruct::Status4,X
        and #$20	;controlled
        bne GoCheckTargetsLoop
        lda CharStruct::Reaction1Command,X
        cmp #$1C	;catch
        beq GoCheckTargetsLoop
        jsr CheckReactionConditions
        lda AIConditionMet
        bne :+
        jmp CheckTargetsLoop
:	CLC
        lda ReactingIndex
        adc #$04	;shift to become Char Index
        sta ReactingIndex
        jsr SaveActionData
        jsr ProcessReaction
        jsr ReactionPauseTimerChecks
        jmp Finish
PartyChecks:	
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$04	;mute
        bne :+
        lda Void
        and #$40	;void
        beq CheckHP
:	lda CharStruct::Status4,X
        and #$04	;singing
        beq CheckHP
        lda CharStruct::Status4,X
        and #$FB	;clear singing
        sta CharStruct::Status4,X
        lda $55		;**bug?: should be $4755 ReactingIndex?
        		;..$55 is the high byte of defense, which is usually 0
        jsr GetTimerOffset
        tdc
        sta EnableTimer::Sing,Y
CheckHP:
        ldx AttackerOffset
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        beq Dead
        lda CharStruct::Status1,X
        and #$80	;dead
        bne Dead
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$20	;paralyze
        beq :+
Dead:	jmp GoCheckTargetsLoopB
:	lda CharStruct::Reaction1Magic,X
        cmp #$80	;monster fight
        beq PFight
        cmp #$81	;monster special
        beq PFight
        lda CharStruct::Reaction1Command,X
        cmp #$04	;fight
        beq PFight
        cmp #$0B	;capture/mug
        beq PFight
        cmp #$15	;aim
        beq PFight
        cmp #$16	;x-fight
        beq PFight
        cmp #$2C	;simple fight (no procs)
        beq PFight
        cmp #$2D	;jump landing
        beq PFight
        cmp #$33	;double lance
        bne CheckDisablingStatus	;**bug: missing swordslap
PFight:	lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $0E		;saved status2
        lda CharStruct::Status4,X
        sta $0F		;saved status4
        lda ReactingIndex
        jsr ShiftMultiply_4
        tay
        lda CombinedStatus::S2,Y
        and #$40	;sleep
        beq :+
        lda CharStruct::Status2,X
        and #$BF	;clear sleep
        sta CharStruct::Status2,X
:	lda CombinedStatus::S2,Y
        and #$10	;charm
        beq :+
        lda CharStruct::Status2,X
        and #$EF	;clear charm
        sta CharStruct::Status2,X
:	lda CharStruct::Status4,X
        and #$FB	;clear singing
        sta CharStruct::Status4,X
        lda CombinedStatus::S2,Y
        and $0E		;saved status2
        and #$50	;sleep/charm
        bne :+
        lda $0F		;saved status4
        and #$04	;singing
        beq CheckBarrier	;**bug: should be .CheckDisablingStatus
        			;..this allows barrier to be queued if para/berserk 
        			;..but only when reacting to physical attacks, not magic
        lda ReactingIndex
        jsr GetTimerOffset
        tdc
        sta EnableTimer::Sing,Y
:	lda ReactingIndex
        jsr ResetATB
        jmp GoCheckTargetsLoopB
CheckDisablingStatus:
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78	;sleep/para/charm/berserk
        bne :+
        lda CharStruct::Status4,X
        and #$04	;singing
        beq CheckBarrier
:	jmp GoCheckTargetsLoopB
CheckBarrier:
        lda CharStruct::Passives1,X
        and #$20	;Barrier
        beq CheckCounter
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        bmi CheckCounter	;no barrier if reflected
        lda AttackerIndex
        cmp #$04	;no barrier if attacker was also party
        bcc CheckCounter
        longa
        lda CharStruct::MaxHP,X
        jsr ShiftDivide_16
        cmp CharStruct::CurHP,X
        bcc CheckCounter16
        shorta0
        jsr SaveActionData
        ldy AttackerOffset
        lda #$20	;Magic
        sta CharStruct::ActionFlag,Y
        lda #$2C	;Magic
        sta CharStruct::Command,Y
        lda #$7C	;Magic Barrier spell
        sta CharStruct::SelectedItem,Y
        lda ReactingIndex
        tax
        tdc
        jsr SetBit_X
        sta CharStruct::PartyTargets,Y
        tdc
        sta CharStruct::MonsterTargets,Y
        jsr ProcessReaction_Party
        jsr ReactionPauseTimerChecks
        inc CurrentlyReacting
        jmp GoFinish
CheckCounter16:
        shorta0
CheckCounter:
        lda CharStruct::Passives1,X
        and #$80	;counter
        bpl GoCheckTargetsLoopB
        lda CharStruct::Reaction1Magic,X
        cmp #$80	;monster fight
        beq CounterAttempt
        cmp #$81	;monster special
        bne GoCheckTargetsLoopB
        lda ActedIndex
        longa
        jsr ShiftMultiply_128
        tay
        shorta0
        lda CharStruct::Specialty,Y
        and #$83	;auto hit ignore defense, hp leak, 1.5x damage
        beq GoCheckTargetsLoopB
CounterAttempt:		;50% chance to counter monster fight or damaging specialty
        jsr Random_0_99
        cmp #$32     	;50%
        bcs GoCheckTargetsLoopB
        jsr SaveActionData
        ldy AttackerOffset
        lda #$80	;Physical/Other
        sta CharStruct::ActionFlag,Y
        lda #$05     	;Fight
        sta CharStruct::Command,Y
        sec
        lda AttackerIndex
        sbc #$04	;to monster index
        tax
        tdc
        jsr SetBit_X
        sta CharStruct::MonsterTargets,Y
        tdc
        sta CharStruct::PartyTargets,Y
        sta CharStruct::SelectedItem,Y
        jsr ProcessReaction_Party
        jsr ReactionPauseTimerChecks
        inc CurrentlyReacting
        jmp GoFinish
GoCheckTargetsLoopB:
        jmp CheckTargetsLoop	;check next target for reactions
GoFinish:
        jmp Finish
CheckTargets2Loop:		;second reaction, same general code structure as above
        			;**optimize: could probably move a lot of the dupe code to reusable functions
        tdc
        tax
:	lda FinalTarget2Bits
        jsr SelectBit_X
        bne FoundTarget2
        inx
        cpx #$0008
        bne :-
        lda CurrentlyReacting
        beq GoFinish2
        jsr UnpauseTimerChecks
        stz CurrentlyReacting
GoFinish2:
        jmp Finish
FoundTarget2:
        lda FinalTarget2Bits
        jsr ClearBit_X
        sta FinalTarget2Bits
        txa
        sta ReactingIndex
        lda Target2WasParty
        bne PartyTarget2
        clc
        lda ReactingIndex
        adc #$04	;now char index
        bra MonsterTarget2
PartyTarget2:
        lda ReactingIndex
MonsterTarget2:
        tax
        lda ActiveParticipants,X
        beq CheckTargets2Loop
        txa
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        and #$40	;stone
        bne CheckTargets2Loop
        lda CharStruct::Status3,X
        and #$10	;stop
        bne CheckTargets2Loop
        lda CharStruct::Status4,X
        and #$81	;erased or hidden
        bne CheckTargets2Loop
        lda Target2WasParty
        beq MonsterChecks2
        jmp PartyChecks2
MonsterChecks2:
        lda CharStruct::Reaction2Magic,X
        cmp #$80	;monster fight
        beq Fight2
        cmp #$81	;monster special
        beq Fight2
        lda CharStruct::Reaction2Command,X
        cmp #$04	;fight
        beq Fight2
        cmp #$0B	;capture/mug
        beq Fight2
        cmp #$15	;aim
        beq Fight2
        cmp #$16	;x-fight
        beq Fight2
        cmp #$2C	;simple fight (no procs)
        beq Fight2
        cmp #$2D	;jump landing
        beq Fight2
        cmp #$33	;double lance
        bne CheckReactions2	;**bug: missing swordslap
Fight2:		;remove relevant statuses after getting hit
        lda CharStruct::Status4,X
        and #$DF	;clear controlled status
        sta CharStruct::Status4,X
        clc
        lda ReactingIndex
        adc #$04
        sta $0E		;char index
        tdc
        tay
:	lda ControlTarget,Y
        cmp $0E
        beq ClearControl2
        iny
        cpy #$0004
        bne :-
        bra CheckSleep2
ClearControl2:
        tdc
        sta ControlTarget,Y
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $10
        lda ReactingIndex
        asl
        tax
        stz ForcedTarget::Party,X
        stz ForcedTarget::Monster,X
        bra WakeUp2
CheckSleep2:
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $10
        lda ReactingIndex
        jsr ShiftMultiply_4
        tay
        lda MonsterCombinedStatus::S2,Y	;8 is first monster
        and #$40	;sleep
        beq Awake2
WakeUp2:                                                                                                     
        phx
        lda $0E		;char index
        jsr ResetATB
        plx
        lda CharStruct::Status2,X
        and #$BF	;clear sleep
        sta CharStruct::Status2,X
Awake2:                                                                                                      
        lda ReactingIndex
        jsr ShiftMultiply_4
        tay
        lda MonsterCombinedStatus::S2,Y
        and #$10	;charm
        beq :+
        lda CharStruct::Status2,X
        and #$EF	;clear charm
        sta CharStruct::Status2,X
:	lda $10		;stored status2
        and #$78	;sleep/para/charm/berserk
        beq CheckReactions2
GoCheckTargets2Loop:                                                                                         
        jmp CheckTargets2Loop
CheckReactions2:                                                                                             
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$78	;sleep/para/charm/berserk
        bne GoCheckTargets2Loop
        lda CharStruct::Status4,X
        and #$20	;controlled
        bne GoCheckTargets2Loop
        lda CharStruct::Reaction2Command,X
        cmp #$1C	;catch
        beq GoCheckTargets2Loop
        jsr CheckReactionConditions
        lda AIConditionMet
        bne :+
        jmp CheckTargets2Loop
:	CLC
        lda ReactingIndex
        adc #$04	;shift to become Char Index
        sta ReactingIndex
        jsr SaveActionData
        jsr ProcessReaction
        jsr ReactionPauseTimerChecks
        jmp Finish
PartyChecks2:	;differs from first set of reactions, is missing the (bugged) silence sing check             
        lda CurrentlyReacting
        beq :+
        jsr UnpauseTimerChecks
        stz CurrentlyReacting
:	LDX AttackerOffset
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        beq Dead2
        lda CharStruct::Status1,X
        and #$80	;dead
        bne Dead2
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$20	;paralyze
        beq :+
Dead2:	jmp GoCheckTargets2LoopB
:	lda CharStruct::Reaction2Magic,X
        cmp #$80	;monster fight
        beq PFight2
        cmp #$81	;monster special
        beq PFight2
        lda CharStruct::Reaction2Command,X
        cmp #$04	;fight
        beq PFight2
        cmp #$0B	;capture/mug
        beq PFight2
        cmp #$15	;aim
        beq PFight2
        cmp #$16	;x-fight
        beq PFight2
        cmp #$2C	;simple fight (no procs)
        beq PFight2
        cmp #$2D	;jump landing
        beq PFight2
        cmp #$33	;double lance
        bne CheckDisablingStatus2
PFight2:                                                                                                     
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $0E
        lda CharStruct::Status4,X
        sta $0F
        lda ReactingIndex
        jsr ShiftMultiply_4
        tay
        lda CombinedStatus::S2,Y
        and #$40	;sleep
        beq :+
        lda CharStruct::Status2,X
        and #$BF	;clear sleep
        sta CharStruct::Status2,X
:	lda CombinedStatus::S2,Y
        and #$10	;charm
        beq :+
        lda CharStruct::Status2,X
        and #$EF	;clear charm
        sta CharStruct::Status2,X
:	lda CharStruct::Status4,X
        and #$FB	;clear singing
        sta CharStruct::Status4,X
        lda CombinedStatus::S2,Y
        and $0E		;saved status2
        and #$50	;sleep/charm
        bne :+
        lda $0F		;saved status4
        and #$04	;singing
        beq CheckBarrier2	;**bug: should be .CheckDisablingStatus2
        lda ReactingIndex
        jsr GetTimerOffset
        tdc
        sta EnableTimer::Sing,Y
:	lda ReactingIndex
        jsr ResetATB
        jmp GoCheckTargetsLoopB	;**bug: this is the first reaction loop
        				;..but we're processing the second here
        				;..should be .GoCheckTargets2LoopB or just CheckTargets2Loop
CheckDisablingStatus2:
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78	;sleep/para/charm/berserk
        bne :+
        lda CharStruct::Status4,X
        and #$04	;singing
        beq CheckBarrier2
:	jmp GoCheckTargetsLoopB	;**bug: wrong reaction loop again
CheckBarrier2: 
        lda CharStruct::Passives1,X
        and #$20	;barrier
        beq CheckCounter2
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        bmi CheckCounter2	;no barrier if reflected
        lda ActedIndex
        cmp #$04	;no barrier if attacker was also party
        bcc CheckCounter2
        longa
        lda CharStruct::MaxHP,X
        jsr ShiftDivide_16
        cmp CharStruct::CurHP,X
        bcc CheckCounter2Mode
        shorta0
        jsr SaveActionData
        ldy AttackerOffset
        lda #$20	;Magic
        sta CharStruct::ActionFlag,Y
        lda #$2C	;Magic
        sta CharStruct::Command,Y
        lda #$7C	;Magic Barrier spell
        sta CharStruct::SelectedItem,Y
        lda ReactingIndex
        tax
        tdc
        jsr SetBit_X
        sta CharStruct::PartyTargets,Y
        tdc
        sta CharStruct::MonsterTargets,Y
        jsr ProcessReaction_Party
        jsr ReactionPauseTimerChecks
        inc CurrentlyReacting
        jmp Finish
CheckCounter2Mode:
        shorta0
CheckCounter2:
        lda CharStruct::Passives1,X
        and #$80	;counter
        bpl GoCheckTargets2LoopB
        lda CharStruct::Reaction2Magic,X
        cmp #$80	;monster fight
        beq CounterAttempt2
        cmp #$81	;monster special
        bne GoCheckTargets2LoopB
        lda ActedIndex
        longa
        jsr ShiftMultiply_128
        tay
        shorta0
        lda CharStruct::Specialty,Y
        and #$83	;auto hit ignore defense, hp leak, 1.5x damage
        beq GoCheckTargets2LoopB
CounterAttempt2:	;50% chance to counter monster fight or damaging specialty
        jsr Random_0_99
        cmp #$32     	;50%
        bcs GoCheckTargets2LoopB
        jsr SaveActionData
        ldy AttackerOffset
        lda #$80	;Physical/Other
        sta CharStruct::ActionFlag,Y
        lda #$05     	;Fight
        sta CharStruct::Command,Y
        sec
        lda ActedIndex	;first counter check used AttackerIndex
        sbc #$04	;to monster index
        tax
        tdc
        jsr SetBit_X
        sta CharStruct::MonsterTargets,Y
        tdc
        sta CharStruct::PartyTargets,Y
        sta CharStruct::SelectedItem,Y
        jsr ProcessReaction_Party
        jsr ReactionPauseTimerChecks
        inc CurrentlyReacting
        jmp Finish
GoCheckTargets2LoopB:
        jmp CheckTargets2Loop
Finish:
        stz ReactingIndexType
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CheckReactionConditions

_3C10:
        stz AISkipDeadCheck
        stz AIConditionMet
        lda ReactingIndex
        sta MonsterIndex
        tax
        lda MonsterReactions,X
        beq Ret
        lda MonsterEscaped,X
        bne Ret
        lda ReactingIndex
        asl
        tax
        longa
        clc
        lda ROMTimes1620w,X	;size of MonsterAI struct
        adc #MonsterAI::ReactConditions
        sta AIOffset
        shorta0
        stz AICurrentCheckedSet
CheckSets:		;10 sets of 4 conditions
        lda AICurrentCheckedSet
        tax
        lda ROMTimes17,X	;size of a condition set
        tay
        sty AIConditionOffset
        stz AICheckIndex
CheckEachCondition:
        ldy AIConditionOffset
        lda (AIOffset),Y
        beq Ret
        cmp #$FE	;end of condition set
        beq Ret	;returning here means all consitions succeeded
        jsr CheckAICondition
        lda AIConditionMet
        beq Finish
        longa
        clc
        lda AIConditionOffset
        adc #$0004	;next condition in set
        sta AIConditionOffset
        shorta0
        inc AICheckIndex
        bra CheckEachCondition
Finish:	INC AICurrentCheckedSet
        lda AICurrentCheckedSet
        cmp #$0A	;10 sets
        bne CheckSets
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc SaveActionData

_3C7F:
        tdc
        tay
        ldx AttackerOffset
:	LDA CharStruct::ActionFlag,X
        sta SavedAction2,Y
        inx
        iny
        cpy #$000A	;copy 10 bytes of action data
        bne :-
        lda ReactingIndex
        cmp #$04	;monster check
        bcc Finish
        sec
        sbc #$04	;now monster index
        sta $10
        asl
        tax
        lda ROMTimes100w,X	;size of MonsterAIScript
        sta $0E
        lda ROMTimes100w+1,X
        sta $0F
        tdc
        tay
        ldx $0E		;AI Script Offset
:	LDA MonsterAIScript,X
        sta SavedMonsterAIScript,Y
        inx
        iny
        cpy #$0064	;100 bytes copied
        bne :-
        lda $10		;monster index
        jsr ShiftMultiply_16
        tax
        stx $0E		;*16
        asl
        tax
        stx $10		;*32
        tdc
        tay
        ldx $0E
:	LDA MonsterMagic,X
        sta SavedMonsterMagic,Y
        inx
        iny
        cpy #$0010	;16 bytes copied
        bne :-
        tdc
        tay
        ldx $10
:	LDA MMTargets::Party,X
        sta SavedMMTargets,Y
        lda MMTargets::Monster,X
        sta SavedMMTargets+1,Y
        inx
        inx
        iny
        iny
        cpy #$0020	;32 bytes copied
        bne :-
Finish:		;party target, or finishing up after a monster target
        lda ReactingIndex
        asl
        tax
        lda ROMTimes11w,X	;**bug: 16 bit table accessed in 8 bit
        tax
        lda EnableTimer::ATB,X	;.. so this is for the wrong character
        sta SavedEnableATB	;.. mitigated by the restore being bugged
        lda CurrentTimer::ATB,X	;.. in the same way
        sta SavedCurrentATB
        rts

.endproc

; ---------------------------------------------------------------------------

;restores a bunch of action data which was saved before doing monster reactions
.proc RestoreActionData

_3D08:
        lda ReactingIndex
        jsr CalculateCharOffset
        tdc
        tay
        ldx AttackerOffset	;offset of forced target
:	LDA SavedAction2,Y
        sta CharStruct::ActionFlag,X
        inx
        iny
        cpy #$000A	;copy 10 bytes action data
        bne :-
        lda ReactingIndex
        cmp #$04	;monster check
        bcc Finish
        sec
        sbc #$04
        sta $10		;monster index
        asl
        tax
        lda ROMTimes100w,X
        sta $0E
        lda ROMTimes100w+1,X
        sta $0F
        tdc
        tay
        ldx $0E
:	LDA SavedMonsterAIScript,Y
        sta MonsterAIScript,X
        inx
        iny
        cpy #$0064	;100 bytes
        bne :-
        lda $10
        jsr ShiftMultiply_16
        tax
        stx $0E
        asl
        tax
        stx $10
        tdc
        tay
        ldx $0E
:	LDA SavedMonsterMagic,Y
        sta MonsterMagic,X
        inx
        iny
        cpy #$0010	;16 bytes
        bne :-
        tdc
        tay
        ldx $10
:	LDA SavedMMTargets,Y
        sta MMTargets::Party,X
        lda SavedMMTargets+1,Y
        sta MMTargets::Monster,X
        inx
        inx
        iny
        iny
        cpy #$0020	;32 bytes
        bne :-
Finish:		;either a party target, or after monster data is restored
        lda ReactingIndex
        asl
        tax
        lda ROMTimes11w,X	;**bug: this is a 16 bit table accessed 8-bit
        tax 			;.. so X is almost always wrong
        lda SavedEnableATB	;.. the save was also bugged the same way
        sta EnableTimer::ATB,X	;.. which mitigates the effects
        lda SavedCurrentATB
        sta CurrentTimer::ATB,X
        lda #$FF
        sta ReactingIndex
        rts

.endproc

; ---------------------------------------------------------------------------

.proc ReactionPauseTimerChecks

_3D9C:
        tdc
        tax
        stx $0E
        lda ReactingIndex
:	CMP $0E
        bne :+
        stz PauseTimerChecks,X
        bra :++
:	INC PauseTimerChecks,X
:	INC $0E
        inx
        cpx #$000C	;12 chars
        bne :---
        inc PendingReactions
        rts

.endproc

; ---------------------------------------------------------------------------

.proc UnpauseTimerChecks

_3DBB:
        tdc
        tax
:	STZ PauseTimerChecks,X
        inx
        cpx #$000C
        bne :-
        rts

.endproc

; ---------------------------------------------------------------------------

;Runs a Reaction AI Script 
;Also used for party reactions, called at a point near the end skipping the AI stuff
.proc ProcessReaction

_3DC7:
        sec
        lda ReactingIndex
        sbc #$04
        sta MonsterIndex
        jsr ShiftMultiply_16
        tax
        stx MonsterOffset16
        asl
        tax
        stx MonsterOffset32
        tdc
        tay
        ldx MonsterOffset16
        lda #$FF
:	STA MonsterMagic,X
        inx
        iny
        cpy #$0010	;init 16 bytes to $FF
        bne :-
        ldx AttackerOffset
        lda #$2C	;Magic (technically MSword L1)
        sta CharStruct::Command,X
        lda #$21	;Magic & Costs MP
        sta CharStruct::ActionFlag,X
        inc CurrentlyReacting	;indicates a reaction to other routines
        ldx #$0040		;init 64 byte AI buffer
        lda #$FF
:	STA AIBuffer,X
        dex
        bpl :-
        longa
        clc
        lda AIOffset
        adc #$00AA	;in MonsterAI, shift from Conditions to Actions
        sta AIOffset
        shorta0
        lda AICurrentCheckedSet
        asl
        tax
        lda ROMTimes64w,X	;size of one MonsterAI ReactActions entry
        sta $0E
        lda ROMTimes64w+1,X
        sta $0F
        tdc
        tax
        ldy $0E		;Current Action offset
ProcessScript:
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        cmp #$FE	;end of entry
        beq Finish
        cmp #$FF	;end of ai
        beq Finish
        cmp #$FD	;special command
        bne Finish
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        cmp #$F7
        beq CopyExtraBytes
        cmp #$F0
        bcs OtherSpecial
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        lda (AIOffset),Y
        sta AIBuffer,X
        bra Finish
CopyExtraBytes:		;command $F7
        lda (AIOffset),Y	;number of extra data bytes to copy
        sta AIBuffer,X		;.. must be at least 1
        sta $10
        iny
        inx
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        stz $11
:	LDA (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        inc $11			;bytes copied
        lda $11
        cmp $10			;bytes to copy
        bne :-
        bra Finish
OtherSpecial:		;>=$F0, but not $F7
        		;copies 2 more bytes then loops back to continue processing
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        lda (AIOffset),Y
        sta AIBuffer,X
        iny
        inx
        bra ProcessScript
Finish:
        stz TempCharm
        jsr DispatchAICommands
Party:
        lda ReactingIndex
        jsr GetTimerOffset
        lda #$01		;action for next tick
        sta CurrentTimer::ATB,Y
        lda #$41		;queued action
        sta EnableTimer::ATB,Y
        rts

.endproc

; ---------------------------------------------------------------------------

;Loads stats and status for all characters, including party equipment and monster AI
.proc LoadStatsEquipmentAI

_3EA2:
        tdc
        tax
        tay
        lda #$04		;4 characters
        sta $10
CopyCharStatsLoop:
        lda #$50		;80 bytes per character
        sta $0E
;Checks for fight $1F7, which is the Galuf Exdeath battle
        lda EncounterIndex
        cmp #$F7
        bne CopyOneChar
        lda EncounterIndex+1
        cmp #$01
        bne CopyOneChar
        lda #$08		;Set Always HP Leak
        sta CharStruct::AlwaysStatus4,Y
CopyOneChar:
        lda FieldChar,X
        sta CharStruct,Y
        inx
        iny
        dec $0E
        lda $0E
        bne CopyOneChar
        longa
        tya
        clc
        adc #$0030		;advance CharStruct index to next character
        tay
        shorta0
        dec $10			;next character
        lda $10
        bne CopyCharStatsLoop
        lda EncounterIndex+1	;1 for boss fights
        beq DoneLenna
        lda EncounterIndex
        cmp #$BA		;Forza/Magisa
        bne DoneLenna
        tdc
        tax
        tay
PoisonLenna:
        lda CharStruct::CharRow,X
        and #$07		;character bits
        cmp #$01		;Lenna
        bne Next
        lda CharStruct::Status1,X
        and #$C6		;if Dead/Stone/Zombie/Poison, abort
        bne DoneLenna
        lda CharStruct::Status1,X
        ora #$04		;set poison
        sta CharStruct::Status1,X
        bra DoneLenna
Next:	JSR NextCharOffset
        iny
        cpy #$0004
        bne PoisonLenna
DoneLenna:
        jsr StartPartyPoisonTimers
        jsr ApplyPartyGear
        tdc
        tax
        stx $0E			;MonsterStats offset
        stx $10			;Monster CharStruct offset
        stx $12			;current monster index
        lda #$D0
        sta $22
        sta $1E
        ldx #$9C00
        stx $20			;$D09C00, ROMAIScriptOffsets
        									;:					
LoadMonsterStatsAI:
        ldx #$0000
        lda #$02
        jsr Random_X_A
        cmp #$02
        bne :+
        lda #$FF		;-1
:	STA $16			;randomly 0, +1 or -1 ($FF)
        ldx $0E			;MonsterStats offset
        ldy $10			;Monster CharStruct offset
        clc
        lda MonsterStats::Speed,X
        adc $16			;randomly 0, +1 or -1
        sta CharStruct4::BaseAgi,Y	;CharStruct4 is the first monster
        sta CharStruct4::EquippedAgi,Y
        lda MonsterStats::AttackPower,X
        sta CharStruct4::MonsterAttack,Y
        lda MonsterStats::AttackMult,X
        sta CharStruct4::MonsterM,Y
        lda MonsterStats::Evade,X
        sta CharStruct4::Evade,Y
        lda MonsterStats::Defense,X
        sta CharStruct4::Defense,Y
        lda MonsterStats::MagicPower,X
        sta CharStruct4::BaseMag,Y
        sta CharStruct4::EquippedMag,Y
        lda MonsterStats::MDefense,X
        sta CharStruct4::MDefense,Y
        lda MonsterStats::MEvade,X
        sta CharStruct4::MEvade,Y
        longa
        lda MonsterStats::HP,X
        sta CharStruct4::CurHP,Y
        sta CharStruct4::MaxHP,Y
        lda MonsterStats::MP,X
        sta CharStruct4::CurMP,Y
        lda #$270F			;monsters always have 9999 Max MP
        sta CharStruct4::MaxMP,Y
        lda MonsterStats::Exp,X
        sta CharStruct4::RewardExp,Y
        lda MonsterStats::Gil,X
        sta CharStruct4::RewardGil,Y
        lda MonsterStats::StatusImmune1,X	;also copies StatusImmune2
        sta CharStruct4::StatusImmune1,Y
        shorta0
        sta CharStruct4::CharRow,Y
        lda MonsterStats::StatusImmune3,X
        sta CharStruct4::StatusImmune3,Y
        lda MonsterStats::AttackFX,X
        sta CharStruct4::RHWeapon,Y
        lda MonsterStats::EAbsorb,X
        sta CharStruct4::EAbsorb,Y
        lda MonsterStats::EImmune,X
        sta CharStruct4::EImmune,Y
        lda MonsterStats::CantEvade,X
        sta CharStruct4::CantEvade,Y
        lda MonsterStats::EWeak,X
        sta CharStruct4::EWeak,Y
        lda MonsterStats::CreatureType,X
        sta CharStruct4::CreatureType,Y
        lda MonsterStats::CmdImmunity,X
        sta CharStruct4::CmdImmunity,Y
        lda MonsterStats::Level,X
        sta CharStruct4::Level,Y
        sta CharStruct4::EquippedVit,Y		;vit = level for monsters
        phy
        lda MonsterStats::Status1,X
        bpl ApplyStatus
AlwaysStatus:										
        longa
        tya
        clc
        adc #$0056	;Shift offset so regular status points to always status
        tay
        shorta0
ApplyStatus:														
        lda MonsterStats::Status1,X
        and #$7F	;clear high bit since it also means death
        sta CharStruct4::Status1,Y
        lda MonsterStats::Status2,X
        sta CharStruct4::Status2,Y
        lda MonsterStats::Status3,X
        sta CharStruct4::Status3,Y
        lda MonsterStats::Status4,X
        sta CharStruct4::Status4,Y
        lda $12			;current monster index
        asl
        tay
        lda MonsterStats::EnemyNameID,X
        sta MonsterNameID,Y
        lda BattleMonsterID+1
        sta MonsterNameID+1,Y
        lda $12			;current monster index
        asl
        tax
        longa
        lda BattleMonsterID,X
        asl
        tax
        shorta0
        lda ROMSpecialtyData::Properties,X
        sta $1C
        lda ROMSpecialtyData::Name,X
        sta $1D
        ply
        lda $1C
        sta CharStruct4::Specialty,Y
        lda $1D
        sta CharStruct4::SpecialtyName,Y
        lda $12			;current monster index
        asl
        tax
        longa
        lda BattleMonsterID,X
        asl
        tay
        shorta0
        lda [$20],Y		;$D09C00, ROMAIScriptOffsets
        sta $1C
        iny
        lda [$20],Y
        sta $1D
        ldx #$0654
        stx $2A
        ldx $12			;current monster index
        stx $2C
        jsr Multiply_16bit	;current monster index * 1620
        ldx $2E			;which is MonsterAI offset
        stx $08			; there's a table for *1620 in the rom
        stx $0A			; not sure why it's not being used
        phx
        tdc
        tay
        sty $0C			;condition/action index
CopyAI:															
        ldx $08			;Current MonsterAI Condition offset
CopyAIConditions:													
        lda [$1C],Y		;AI script offset in ROM
        sta MonsterAI::Conditions,X
        inx
        iny
        cmp #$FE		;end of condition entry
        bne CopyAIConditions
        ldx $0A			;Current MonsterAI Action offset
CopyAIActions:														
        lda [$1C],Y		;AI script offset in ROM, Y is kept from above
        sta MonsterAI::Actions,X
        inx
        iny
        cmp #$FF		;end of AI script
        beq AICounters
        cmp #$FE		;end of action entry
        bne CopyAIActions
        longa
        clc
        lda $08			;MonsterAI Condition offset
        adc #$0011		;next condition
        sta $08
        clc
        lda $0A			;MonsterAI Action offset
        adc #$0040		;next action
        sta $0A
        shorta0
        inc $0C			;next condition/action index
        lda $0C
        cmp #$0A		;max 10 condition/action pairs
        bne CopyAI
AICounters:
        plx 			;MonsterAI offset
        stx $08
        stx $0A
        stx $0C
CopyAIReact:								
        ldx $08			;Current MonsterAI Condition offset
CopyAIReactConditions:													
        lda [$1C],Y		;AI script offset in ROM, Y is kept from above
        sta MonsterAI::ReactConditions,X
        inx
        iny
        cmp #$FF		;end of AI script
        beq NextMonster
        cmp #$FE         	;end of condition entry
        bne CopyAIReactConditions
        ldx $12			;current monster index
        inc MonsterReactions,X
        ldx $0A
CopyAIReactActions:													
        lda [$1C],Y		;AI script offset in ROM, Y is kept from above
        sta MonsterAI::ReactActions,X
        inx
        iny
        cmp #$FF		;end of AI script
        beq NextMonster
        cmp #$FE		;end of action entry
        bne CopyAIReactActions
        longa
        clc
        lda $08			;MonsterAI Condition offset
        adc #$0011       	;next react condition
        sta $08
        clc
        lda $0A          	;MonsterAI Action offset
        adc #$0040       	;next react action
        sta $0A
        shorta0
        inc $0C          	;next condition/action index
        lda $0C
        cmp #$0A         	;max 10 condition/action pairs
        bne CopyAIReact
NextMonster:
        ldx $10			;Monster CharStruct offset
        jsr NextCharOffset
        stx $10
        clc
        lda $0E			;MonsterStats offset
        adc #$20		;next monster
        sta $0E
        inc $12			;next monster index
        lda $12
        cmp #$08		;8 monsters
        beq MonsterStatusTimers
        jmp LoadMonsterStatsAI
MonsterStatusTimers:
        tdc
        tax
        lda #$04
        sta Temp
MonStatusLoop:														
        lda Temp
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        and #$04		;poison
        beq :+
        lda #$01		;poison timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status2,X
        and #$80		;old
        beq :+
        lda #$06		;old timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status2,X
        and #$20		;paralyze
        beq :+
        lda #$09		;paralyze timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status2,X
        and #$04		;mute
        beq :+
        lda #$04		;mute timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status3,X
        and #$80		;reflect
        beq :+
        lda #$02		;reflect timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status3,X
        and #$10		;stop
        beq :+
        lda #$00		;stop timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status3,X
        and #$01		;regen
        beq :+
        lda #$07		;regen timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status4,X
        and #$10		;countdown
        beq :+
        lda #$03		;countdown timer
        jsr StartTimerFromTemp
:	LDX AttackerOffset
        lda CharStruct::Status3,X	;**bug: should be Status4
        and #$08		;Haste, should be HP leak
        beq :+
        lda #$05		;HP leak timer
        jsr StartTimerFromTemp
:	INC Temp
        lda Temp
        cmp #$0C		;doing slots 4-11 for monsters, 12 is too far
        beq Ret
        jmp MonStatusLoop
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;(A: #timer, $2620: index) Starts a status timer for a character
.proc StartTimerFromTemp

_41A3:
        tax
        lda #$01
        sta StatusFixedDur
        lda Temp	;Party index
        jmp StartTimer

.endproc

; ---------------------------------------------------------------------------

;sets up inventory and magic lists
.proc SetupInventoryMagic

_41AF:
        tdc
        tax
        lda #$80
				
:       STA InventoryFlags,X
        inx
        cpx #$0100
        bne :-
				
        tdc
        tax
        stx $0E
			
InitSpells:
        tdc
        tay
        lda #$81
		
:	STA CharSpells::Flags,X
        sta CharSpells::Level,X
        inx
        iny
        cpy #$0082
        bne :-
				
        longa
        txa
        clc
        adc #$0208	;next CharSpells struct
        tax
        shorta0
        inc $0E		;next char index
        lda $0E
        cmp #$04	;4 chars
        bne InitSpells
				
        tdc
        tax
        stx $0E
        										;:					
InitCmdFlags:	
        tdc
        tay
        lda #$80
        										;:					
:	STA CharCommands::Flags,X
        inx
        iny
        cpy #$0004	;4 bytes of command data
        bne :-
				
        txa
        clc
        adc #$10	;next CharCommands Struct
        tax
        inc $0E		;next char index
        lda $0E
        cmp #$04	;4 chars
        bne InitCmdFlags
        tdc
        tax
        stx $0E
InitHandItemFlags:									;:				
        tdc
        tay
        lda #$80
:	STA HandItems::Flags,X
        inx
        iny
        cpy #$0002	;structure has room for 4 values but only 2 hands are used
        bne :-
        txa
        clc
        adc #$0A	;next charcter in HandItems
        tax
        inc $0E		;next char index
        lda $0E
        cmp #$04	;4 chars
        bne InitHandItemFlags
        tdc
        tay
        tax
        sty $0E
CopyEquipInfo:
        stz $0E
        phx

:	LDA CharStruct::EquipWeapons,X
        sta CharEquippable::Weapons,Y
        inx
        iny
        inc $0E		;next byte
        lda $0E
        cmp #$04	;4 bytes of weapon/armor equip data
        bne :-
				
        plx
        jsr NextCharOffset
        inc $0F		;next char index
        lda $0F
        cmp #$04	;4 chars
        bne CopyEquipInfo
				
        jsl $D0EF78	        ;wtf, code in the data bank
        tdc 			;it sets items with qty 0 to id 0
        tax 			;and sets items with id 0 to qty 0
        tay
        										;:					
:	LDA FieldItems,X
        sta InventoryItems,X
        inx
        cpx #$0200		;512, includes item quantities
        bne :-
				
:	LDA InventoryItems,Y
        jsr SetupInventoryInfo	;sets InventoryFlags and Temp (equipment type)
        iny
        cpy #$0100
        bne :-
				
        tdc
        tay
        sty $08
        										;:					
:	LDY $08
        jsr GetItemUsableY	;A now holds byte for InventoryUsable
        ldy $08
        sta Temp,Y		;**optimize: bypass temp and just save to proper place
        inc $08
        lda $08
        bne :-
				
        tdc
        tax
        										;: 					
:	LDA Temp,X
        sta InventoryUsable,X
        inx
        cpx #$0100
        bne :-
				
        tdc
        tax
        stx $0E		;MagicBits Index
        stx $10		;finished spell count
        stx $18		;blue magic flag
        inx
        stx $14		;current spell level 1-6
MagicBitsLoop:															
        lda $0E		;MagicBits Index
        cmp #$0C	;12 bytes of non-blue magic
        bcc :+
        sec
        lda $0E
        sbc #$0C	;-12	back to start
        clc
        adc #$10	;+16	start of blue magic
        tax
        bra :++
:	LDX $0E		;MagicBits Index
:	LDA MagicBits,X
        sta $12		;current byte of MagicBits
        ldy #$0008	;bit counter
ProcessMagicBit:
        ldx $10
        cpx #$0081
        bne CheckMagicBit
        jmp EnableSpells
CheckMagicBit:
        asl $12		;current byte of MagicBits
        bcs CheckBlue
        lda #$FF
        StoreOffsetX CharSpells, ID, 0
        StoreOffsetX CharSpells, ID, 1
        StoreOffsetX CharSpells, ID, 2
        StoreOffsetX CharSpells, ID, 3
        bra NextSpell
CheckBlue:									
        txa
        cmp #$5F	;end of non-blue magic
        bcc SetupSpell
        sec
        sbc #$5F	;remove offset from other spells so first blue is 0
        clc
        adc #$80	;first blue now $80
        pha
        lda #$FF	;clear the "original" blue magic position
        StoreOffsetX CharSpells, ID, 0
        StoreOffsetX CharSpells, ID, 1
        StoreOffsetX CharSpells, ID, 2
        StoreOffsetX CharSpells, ID, 3
        pla
        dex 		;blue spells offset the whole structure by -2
        dex		;because there's 2 empty bits in the first blue byte
SetupSpell:									
        stx $20		;temp index
        StoreOffsetX CharSpells, ID, 0
        StoreOffsetX CharSpells, ID, 1
        StoreOffsetX CharSpells, ID, 2
        StoreOffsetX CharSpells, ID, 3
        lda $14		;current spell level
        StoreOffsetX CharSpells, Level, 0
        StoreOffsetX CharSpells, Level, 1
        StoreOffsetX CharSpells, Level, 2
        StoreOffsetX CharSpells, Level, 3
        InstructionOffsetX lda, CharSpells, ID, 0
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        pha
        lda ROMMagicInfo::MPCost,X
        and #$7F	;just MP Cost
        ldx $20
        StoreOffsetX CharSpells, MP, 0
        StoreOffsetX CharSpells, MP, 1
        StoreOffsetX CharSpells, MP, 2
        StoreOffsetX CharSpells, MP, 3
        pla
        StoreOffsetX CharSpells, Targetting, 0
        StoreOffsetX CharSpells, Targetting, 1
        StoreOffsetX CharSpells, Targetting, 2
        StoreOffsetX CharSpells, Targetting, 3
NextSpell:	
        inc $15		;counter for spells in a spell Level
        lda $15
        cmp #$03	;max 3
        bne NextSpellCount
        stz $15
        inc $14		;next spell level
        lda $14
        cmp #$07	;only 6 spell levels
        bne NextSpellCount
        lda #$01	;reset to spell level 1
        sta $14
NextSpellCount:
        inc $10		;finished spell count
        lda $18		;blue magic flag
        bne NextSpellBit
        lda $10
        cmp #$5F	;last non-blue magic spell
        bcc NextSpellBit
        inc $18		;blue magic flag
        bra NextSpellByte
NextSpellBit:									
        dey 		;bit counter
        beq NextSpellByte
        jmp ProcessMagicBit
NextSpellByte:															
        inc $0E		;MagicBits Index (next)
        jmp MagicBitsLoop
EnableSpells:															
        tdc
        tay
        tax
UnpackEnableSpells:											;:			
        stz $0E
        phx
UnpackOne:											;:				
        lda CharStruct::EnableSpells,X
        pha
        jsr ShiftDivide_16
        inc
        sta Temp,Y	;high 4 bits to first temp byte
        pla
        and #$0F
        inc
        sta Temp+1,Y	;low 4 bits to second temp byte
        iny
        iny
        inx
        inc $0E
        lda $0E
        cmp #$03	;3 bytes for 6 magic types
        bne UnpackOne
				
        plx
        jsr NextCharOffset
        cpy #$0018	;6 bytes * 4 characters
        bne UnpackEnableSpells
				
        tdc
        tax
        tay
        sty $12		;character index
        sty HalfMP	;
        sty HalfMP+2
AllCharSpellConditions:											;:			
        stz $1A		;half mp for current character
        phx
        lda $12		;character index
        longa
        jsr ShiftMultiply_128
        tax
        shorta0
        lda CharStruct::ArmorProperties,X
        and #$08	;half mp cost
        beq :+
        lda $12		;character index
        tax
        inc HalfMP,X
        inc $1A		;half mp for current character
:	PLX
        stz $10			;spell type counter
        stz $14			;spell counter
CheckAllSpellConditions:											;:		
        stz $0E
CheckSpellCond:										;:					
        lda $14			;spell counter
        cmp #$57		;stop at 87 spells
        beq NextSpellType
        lda CharSpells::Level,X
        cmp Temp,Y		;magic level enabled for current Type
        bcs LevelFail
        lda EncounterInfo::Flags
        and #$04	;Always Void
        bne NextSpellCond
        stz CharSpells::Flags,X
        lda $1A		;half mp for current character
        beq NextSpellCond
        lsr CharSpells::MP,X
        bcc NextSpellCond
        inc CharSpells::MP,X	;min 1
        bra NextSpellCond
LevelFail:
        lda #$FF
        sta CharSpells::ID,X
        inc
        sta CharSpells::MP,X
NextSpellCond:
        inc $14			;spell counter
        inx
        inc $0E
        lda $0E
        cmp #$12		;18 spells per type
        bne CheckSpellCond
NextSpellType:									
        iny 			;index for magic level enabled table at Temp
        inc $10			;spell type counter
        lda $10
        cmp #$05		;checking first 5 types
        bne CheckAllSpellConditions
        longa
        txa
        clc
        adc #$0233		;$57 + $233 is the size of the CharSpells Struct
        tax 			;next character in CharSpells
        shorta0
        iny
        inc $12			;character index
        lda $12
        cmp #$04		;4 characters
        bne AllCharSpellConditions
				
        lda EncounterInfo::Flags
        and #$04		;always void
        bne DoneSongBlue
        stz $0E			;character index
        ldx #$0057		;first spell after summons (songs then blue)
        stx $12			;first song offset
        										;:					
AllSongBlue:
        lda $0E			;character index
        tax
        lda HalfMP,X
        sta $1A			;half mp for current character
        stz $10			;spell counter
        ldx $12			;first song offset
SongBlue:										;:					
        lda CharSpells::ID,X
        cmp #$FF
        beq NextSongBlue
        stz CharSpells::Flags,X
        lda $1A			;half mp for current character
        beq NextSongBlue
        lsr CharSpells::MP,X
        bcc NextSongBlue
        inc CharSpells::MP,X	;min 1
NextSongBlue:
        inx 			;charspell index
        inc $10			;spell counter
        lda $10
        cmp #$28		;40 spells (total of 128)
        bne SongBlue
        longa
        clc
        lda $12			;first song offset
        adc #$028A		;size of CharSpells struct
        sta $12			;first song offset for next character
        shorta0
        inc $0E			;character index
        lda $0E
        cmp #$04		;4 characters
        bne AllSongBlue
DoneSongBlue:															
        tdc
        tax
        tay
        sty $0E
        sty $10			;character counter
        										;:					
AllSetupCmds:
        stz $11			;command counter
        ldx $0E			;CharStruct offset
        										;:					
SetupCmds:
        phx
        lda CharStruct::BattleCommands,X
        beq NextCmd
        cmp #$50		;there are no commands $50 or higher
        bcs NextCmd
        cmp #$1D		;Catch Command
        bne OtherCmd
        ldx $0E
        lda CharStruct::CaughtMonster,X
        cmp #$FF		;no monster caught
        beq Catch
        lda #$1E		;Release Command
        bra OtherCmd
Catch:
        lda #$1D
OtherCmd:	
        sta CharCommands::ID,Y
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMAbilityInfo::Targetting,X
        sta CharCommands::Targetting,Y
NextCmd:
        plx 			;CharStruct offset
        iny 			;next command id slot
        inx 			;next command byte in CharStruct
        inc $11			;command counter
        lda $11
        cmp #$04		;4 commands
        bne SetupCmds
				
        ldx $0E
        jsr NextCharOffset
        stx $0E
        tya
        clc
        adc #$10		;next CharCommands character
        tay
        inc $10			;character counter
        lda $10
        cmp #$04		;4 characters
        bne AllSetupCmds
        tdc
        tax
        tay
        sty $0E
        sty $10			;character counter
AllSetupHandItems:
        stz $11			;hand counter
        ldx $0E			;charstruct offset
SetupHandItems:													
        phx
        lda CharStruct::RHWeapon,X
        bne :+
        lda CharStruct::RHShield,X
:	CMP #$01
        bne :+
        tdc 			;item 1 -> 0
:	STA HandItems::ID,Y
        pha
        cmp #$80
        bcc Weapon
        sec
        sbc #$80		;remove armor offset
        longa
        jsr ShiftMultiply_4
        sta $12
        asl
        clc
        adc $12
        tax 			;Armor ID *12
        shorta0
        lda #$5A
        sta HandItems::Flags,Y
        lda ROMArmor::EquipmentType,X
        bra SetHandUsable
Weapon:
        longa
        jsr ShiftMultiply_4
        sta $12
        asl
        clc
        adc $12
        tax              	;Weapon ID *12
        shorta0
        lda ROMWeapons::Targetting,X
        sta HandItems::Targetting,Y
        lda ROMWeapons::DoubleGrip,X
        and #$80		;double grip bit
        jsr ShiftDivide_32	;shift to 3rd bit
        sta HandItems::Flags,Y
        lda ROMWeapons::EquipmentType,X
        pha
        and #$C0			;usable? and throwable
        ora #$1A			;set some more bits ?
        ora HandItems::Flags,Y	;double grip bit from before
        sta HandItems::Flags,Y
        pla 			;equipment type
SetHandUsable:
        jsr GetItemUsableA
        sta HandItems::Usable,Y
        pla 			;current hand's item id
        beq NextHand
        tdc
        inc
        sta HandItems::Level,Y
NextHand:
        plx
        iny 			;structure offsets increase to left hand
        inx
        inc $11			;hand counter
        lda $11
        cmp #$02		;2 hands to process
        bne SetupHandItems
				
        ldx $0E			;charstruct offset
        jsr NextCharOffset
        stx $0E
        tya
        clc
        adc #$0A		;next HandItems character
        tay
        inc $10			;character counter
        lda $10
        cmp #$04		;4 characters
        beq Ret
        jmp AllSetupHandItems
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Calculate Item Usability
;(A: item data equipment type (byte 2/12), returns A = Useable byte, 2 bits per char)
.proc GetItemUsableA

_455E:
        phy
        and #$3F		;strip flags
        jsr ShiftMultiply_4
        tax
        tdc
        tay
        									;:					
:	LDA ROMItemEquippable,X
        sta TempEquippable,Y
        inx
        iny
        cpy #$0004		;2 weapon bytes, 2 armor bytes
        bne :-
        									;.					
        tdc
        tax
        tay
        lda #$AA		;usable for none
        sta $14			;in-progress usable byte
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
        jsr ShiftDivide_4
        bne Check1
        lda $14		;in-progress usable byte
        and #$7F	;clear first character bit
        sta $14
        bra NextChar
Check1:								
        cmp #$01
        bne Check2
        lda $14
        and #$DF	;clear second character bit
        sta $14
        bra NextChar
Check2:								
        cmp #$02
        bne Other
        lda $14
        and #$F7	;clear third character bit									
        sta $14
        bra NextChar
Other:								
        lda $14
        and #$FD	;clear fourth character bit
        sta $14
NextChar:
        inx
        inx
        inx
        inx
        cpx #$0010	;4 bytes * 4 characters
        bne DetermineEquippableLoop
        									;.					
        lda $14
        ply
        rts

.endproc

; ---------------------------------------------------------------------------

;Start party poison status timers
.proc StartPartyPoisonTimers

_45D5:
        tdc
        tax
        stx $22		;CharStruct Offset
        stx Temp	;Party index
StartTimersLoop:														
        ldx $22
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$04	;Poison
        beq Next
        lda #$01
        jsr StartTimerFromTemp
Next:	LDX $22
        jsr NextCharOffset
        stx $22
        inc Temp	;next party member
        lda Temp
        cmp #$04
        bne StartTimersLoop
        rts

.endproc

; ---------------------------------------------------------------------------

;Command $05 (Fight)
;**optimize: seems like there should be a better way than duplicating all the code for each hand
.org $45FF
.proc FightCommand

_45FF:
CommandTable04:
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
        sta TempTargetBitmask	;targets, CCCC MMMM
        pla
        and #$0F
        asl
        asl
        asl
        asl
        sta TempTargetBitmask+1	;targets 2nd byte, MMMM 0000
        lda AttackerIndex
        tax
        lda ROMTimes84,X		;multiplies by 84
        tax
        stx $0E				;offset into character equipment
        ldx AttackerOffset
        lda CharStruct::RHWeapon,X
        bne RightHand
        jmp LeftHand
RightHand:
        ldx $0E				;offset into character equipment
        lda RHWeapon::Properties,X
        sta $10				;RH Weapon Properties
        and #$02			;command instead of attack
        beq NormalAttack
        lda CurrentlyReacting
        bne NormalAttack
        ldx AttackerOffset
        lda CharStruct::Status1,X
        and #$02      			;Zombie
        bne NormalAttack
        lda CharStruct::Status2,X
        and #$10			;Charm
        bne NormalAttack
        jsr Random_0_99
        ldx $0E				;offset into character equipment
        cmp RHWeapon::Param2,X		;proc chance
        bcs NormalAttack
        lda RHWeapon::Param3,X		;proc command
        jmp DispatchCommand_CommandReady	;dispatch the new command
NormalAttack:								
        jsr SelectCurrentProcSequence	;Y and $0C = AttackInfo offsets
        sty $14				;AttackInfo offset
        stz $12
        ldx $0E				;offset into character equipment
CopyAttackInfo:										
        lda RHWeapon,X
        sta AttackInfo,Y
        inx
        iny
        inc $12
        lda $12
        cmp #$0C			;12 byte structure size
        bne CopyAttackInfo
        ldx $0E				;offset into character equipment
        lda RHWeapon::Properties,X
        and #$04			;Magic Sword OK
        bne MSword
        tdc
        bra :+
MSword:	LDX AttackerOffset
        lda CharStruct::MSwordAnim,X
        and #$7F			;Right Hand clears high bit
:	PHA
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC
        sta GFXQueue::Cmd,X
        lda #$01
        sta GFXQueue::Type,X
        lda #$04
        sta GFXQueue::Data1,X
        pla
        sta GFXQueue::Data2,X		;created command $00,FC,01,04,<MSword>
        ldx $0E				;offset into character equipment
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
        jsr GFXCmdDamageNumbers		;creates GFX cmd $00,FC,06,00,00
        lda $10				;RH Weapon Properties
        and #$01			;Wonder Rod
        beq NoWonder
        clc
        lda BattleData::WonderRod
        adc #$12			;+18
        sta $10				;Wonder Rod Spell + 18
        clc
        lda BattleData::WonderRod
        bne :+
        inc 				;min 1 (skips scan)
:	ADC #$01			;
        cmp #$24			;36
        bne :+
        tdc 				;clear wonder rod if it got too high
:	STA BattleData::WonderRod
        bra MagicProc
NoWonder:								
        lda $10				;RH Weapon Properties
        and #$08			;Magic on hit
        beq LeftHand
        jsr Random_0_99
        ldx $0E				;offset into character equipment
        cmp RHWeapon::Param2,X		;Proc Chance
        bcs LeftHand
        lda RHWeapon::Param3,X		;Proc Magic
        sta $10				;Proc Magic
MagicProc:
        ldy $14				;AttackInfo offset
        tya
        clc
        adc #$0C			;+12, size of AttackInfo
        tay
        sty $14				;next AttackInfo offset
        lda $10				;Proc or Wonder Magic
        pha
        jsr CopyROMMagicInfo
        lda ProcSequence
        tax
        ldy $14				;AttackInfo offset
        lda AttackInfo::MagicAtkType,Y
        and #$7F
        sta AtkType,X
        jsr FindOpenGFXQueueSlot
        pla 				;Proc or Wonder Magic
        sta GFXQueue::Data1,X
        stz GFXQueue::Flag,X
        stz GFXQueue::Data2,X
        lda #$FC
        sta GFXQueue::Cmd,X
        lda #$07
        sta GFXQueue::Type,X		;created command $00,FC,07,<Magic>,00
        lda ProcSequence
        tax
        stz MultiTarget,X
        lda #$10
        sta TargetType,X
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
        jsr GFXCmdDamageNumbers		;creates Action $00,FC,06,00,00
LeftHand:	;very similar to RH code, but does a bunch of stuff in a different order
        ldx AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        rts 			;no LH weapon, we're done
:	LDX $0E			;offset into character equipment
        lda LHWeapon::Properties,X
        sta $10			;LH Weapon Properties
        and #$02		;Command instead of attack
        beq NormalAttackLH
        lda CurrentlyReacting
        bne NormalAttackLH
        ldx AttackerOffset
        lda CharStruct::Status1,X
        and #$02		;Zombie
        bne NormalAttackLH
        lda CharStruct::Status2,X
        and #$10		;Charm
        bne NormalAttackLH
        jsr Random_0_99
        ldx $0E			;offset into character equipment
        cmp LHWeapon::Param2,X			;proc chance
        bcs NormalAttackLH
        lda LHWeapon::Param3,X			;proc command
        jmp DispatchCommand_CommandReady	;dispatch the new command
NormalAttackLH:
        jsr SelectCurrentProcSequence	;Y and $0C = AttackInfo offsets
        sty $12				;AttackInfo offset
        stz $14
        ldx $0E
CopyAttackInfoLH:
        lda LHWeapon,X
        sta AttackInfo,Y
        inx
        iny
        inc $14
        lda $14
        cmp #$0C			;12 byte structure size
        bne CopyAttackInfoLH
        ldx $0E			;offset into character equipment
        lda ProcSequence
        tay
        lda LHWeapon::AtkType,X
        sta AtkType,Y
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC
        sta GFXQueue::Cmd,X
        lda #$01
        sta GFXQueue::Type,X
        lda #$04
        sta GFXQueue::Data1,X
        stx $08			;command queue slot offset
        ldx $0E			;offset into character equipment
        lda LHWeapon::Properties,X
        and #$04		;Magic Sword OK
        bne MSwordLH
        lda #$80
        bra :+
MSwordLH:
        ldx AttackerOffset
        lda CharStruct::MSwordAnim,X
        ora #$80		;Left Hand sets high bit
:	LDX $08			;command queue slot offset
        sta GFXQueue::Data2,X	;created command $00,FC,01,04,<MSword/Hand>
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
        jsr GFXCmdDamageNumbers	;creates GFX command $00,FC,06,00,00
        lda $10			;LH Weapon Properties
        and #$01		;Wonder Rod
        beq NoWonderLH
        clc
        lda BattleData::WonderRod
        adc #$12		;+18
        sta $10			;Wonder Rod Spell + 18
        clc
        lda BattleData::WonderRod
        bne :+
        inc 			;skip scan
:	ADC #$01
        cmp #$24		;36
        bne :+
        tdc 			;clear wonder rod if it got too high
:	STA BattleData::WonderRod
        bra MagicProcLH
NoWonderLH:
        lda $10			;LH Weapon Properties
        and #$08		;magic on hit
        beq Ret
        jsr Random_0_99
        ldx $0E			;offset into character equipment
        cmp LHWeapon::Param2,X	;Proc chance
        bcs Ret
        lda LHWeapon::Param3,X	;Proc Magic
        sta $10			;Proc Magic
MagicProcLH:
        ldy $12			;AttackInfo offset
        tya
        clc
        adc #$0C		;+12, size of AttackInfo
        tay
        sty $12			;next AttackInfo offset
        ldx $0E			;offset into character equipment
        lda $10			;Proc or Wonder Magic
        pha
        jsr CopyROMMagicInfo
        ldy $12			;AttackInfo offset
        lda ProcSequence
        tax
        lda AttackInfo::MagicAtkType,Y
        and #$7F
        sta AtkType,X
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Data2,X
        lda #$FC
        sta GFXQueue::Cmd,X
        lda #$07
        sta GFXQueue::Type,X
        pla 			;Proc or Wonder Magic
        sta GFXQueue::Data1,X	;created command $00,FC,07,<Magic>,00
        lda ProcSequence
        tax
        stz MultiTarget,X
        lda #$10
        sta TargetType,X
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
        jsr GFXCmdDamageNumbers	;creates GFX cmd $00,FC,06,00,00
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.org $48BD
.proc ProcessTurn

_48BD:
        lda AttackerIndex
        jsr CalculateCharOffset
        ldx AttackerOffset
        lda CharStruct::CurHP,X
        sta CurrentHP
        lda CharStruct::CurHP+1,X
        sta CurrentHP+1
        ldx CurrentHP
        cpx #$270F	;9999
        bcc :+
        ldx #$270F	;cap at 9999
        stx CurrentHP
:	LDX AttackerOffset
        jsr CopyStatsWithBonuses
        jsr WipeDisplayStructures
        jsr WipeActionData
        jsr DispatchCommand
        jsr ProcessCommands
        jsr CheckLearnBlue
        lda WasMonsterReleased
        beq :+
        jsr RestoreStatsAfterRelease
:	LDA ActionAnimShift
        beq :+
        jsr ShiftActionAnims
:	LDA #$0A	;C1 Routine: execute graphics script
        jsr CallC1
        lda EncounterInfo::IntroFX
        bpl :+
        lda AttackerIndex      ;this section for credits demo
        cmp #$04		;monster check
        bcs :+
        lda #$20
        sta BattleOver		;battle ends after one action
:	LDA #$FF
        sta ReleasedMonsterID
        inc TurnProcessed
        stz PendingReactions
        rts

.endproc

; ---------------------------------------------------------------------------

.proc ShiftActionAnims
_4923:
        lda ActionAnimShift
        bpl Finish
        LoadOffset ActionAnim0, TargetBits, 0
        InstructionOffset ora, ActionAnim0, TargetBits, 1
        beq :+
        LoadOffset ActionAnim0, OrigTargetBits, 0
        InstructionOffset sta, ActionAnim0, TargetBits, 0
:	TDC
        tax
ShiftActionAnims:
        InstructionOffsetX lda, ActionAnim0, Flags, 2
        StoreOffsetX ActionAnim0, Flags, 1
        inx
        cpx #$0023	;35 bytes, which is 5 ActionAnim structures
        bne ShiftActionAnims
        lda ActionAnimShift
        and #$40
        beq Ret
        LoadOffset ActionAnim0, TargetBits, 1
        InstructionOffset ora, ActionAnim0, TargetBits, 2
        beq Finish
        LoadOffset ActionAnim0, OrigTargetBits, 1
        InstructionOffset sta, ActionAnim0, TargetBits, 1
Finish:
        tdc
        tax
ShiftActionAnims2:
        InstructionOffsetX lda, ActionAnim0, Flags, 3
        StoreOffsetX ActionAnim0, Flags, 2
        inx
        cpx #$0023	;35 bytes, which is 5 ActionAnim structures
        bne ShiftActionAnims2
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Dispatches Commands to their command routines
.proc DispatchCommand

_4968:
        ldx AttackerOffset
        lda CharStruct::Status1,X
        and #$02			;Zombie
        bne :+
        lda CharStruct::Status2,X
        and #$18      			;Charm or Berserk
        beq :++
:	LDA CharStruct::CmdCancelled,X
        bne :+				;already cancelled player command
        lda #$01
        sta CharStruct::CmdCancelled,X
        stz CharStruct::Command,X
        lda #$80
        sta CharStruct::ActionFlag,X
:	LDA CharStruct::Status2,X
        and #$40      			;Sleep
        beq :+
        stz CharStruct::Command,X
        lda #$80
        sta CharStruct::ActionFlag,X
:	CPX #$0200    			;AttackerOffset is a monster
        bcs ActionReady
        lda CharStruct::Command,X
        beq ActionReady
        cmp #$25			;Hide
        beq ActionReady
        cmp #$26			;Show
        beq ActionReady
        cmp #$2B			;Mimic
        beq ActionReady
        cmp #$4E			;Nothing 4E and above
        bcs ActionReady
        lda CharStruct::Status1,X
        and #$02			;Zombie
        bne ActionReady
        lda CharStruct::Status2,X
        and #$18			;Charm or Berserk
        bne ActionReady
        tdc
        tay
CopyActionInfo:		;copies 10 bytes of command and cursor selection info from CharStruct
        lda CharStruct::ActionFlag,X
        sta SavedAction,Y
        inx
        iny
        cpy #$000A
        bne CopyActionInfo
ActionReady:
        ldx AttackerOffset
        lda CharStruct::Command,X
CommandReady:		;routine can be called from here directly.  If so, A is a command (mimic or a weapon proc)
        pha
        longa
        jsr ShiftMultiply_8
        tax
        lda ROMAbilityInfo::CmdStatus,X	;holds command status and damage mod
        ldx AttackerOffset
        sta CharStruct::CmdStatus,X
        shorta0
        pla
        tax
        lda ROMCommandMap,X	;maps commands to command table..
        sta CommandOffset	;mostly just decrements by 1,
        asl 			;except for magic or internal commands after
        tax
        lda f:CommandTable,X
        sta $08
        lda f:CommandTable+1,X
        sta $09
        lda #$C2 ; Current bank
        sta $0A
        stz UnknownReaction
        stz DelayedFight
        stz SearchGFXQueue
        stz MPTaken
        stz MessageBoxOffset
        stz MessageBoxDataOffset
        stz WasMonsterReleased
        stz ActionAnimShift
        stz MissInactive
        stz HitsJumping
        tdc
        tax
:	STZ HitsInactive,X
        inx
        cpx #$0010
        bne :-
        jmp [$0008]		;jumps to command from command table

.endproc

; ---------------------------------------------------------------------------

.org $4A2F
.proc RestoreStatsAfterRelease

_4A2F:
        lda AttackerIndex
        jsr CalculateCharOffset
        ldx AttackerOffset
        lda SavedCharStats::Level
        sta CharStruct::Level,X
        lda SavedCharStats::MonsterAttack
        sta CharStruct::MonsterAttack,X
        lda SavedCharStats::MonsterM
        sta CharStruct::MonsterM,X
        lda SavedCharStats::EquippedMag
        sta CharStruct::EquippedMag,X
        lda SavedCharStats::CharRow
        sta CharStruct::CharRow,X
        lda CharStruct::Status1,X
        ora SavedCharStats::Status1
        and #$7F	;clear dead status
        sta CharStruct::Status1,X
        tdc
        tay
StatusLoop:	;Adds original Status1-4, CmdStatus, DamageMod, Passives1-2, ElementUp
        lda CharStruct::Status1,X
        ora SavedCharStats::Status1,Y
        sta CharStruct::Status1,X
        inx
        iny
        cpy #$0009
        bne StatusLoop
        tdc
        tay
        ldx AttackerOffset
MSwordLoop:
        lda SavedCharStats::MSwordElemental1,Y
        sta CharStruct::MSwordElemental1,X
        inx
        iny
        cpy #$0006
        bne MSwordLoop
        tdc
        tay
        ldx AttackerOffset
MiscLoop:	;copies Always Status, Bonus Stats, and Magic Sword Animation
        lda SavedCharStats::AlwaysStatus1,Y
        sta CharStruct::AlwaysStatus1,X
        inx
        iny
        cpy #$000B
        bne MiscLoop
        rts

.endproc

; ---------------------------------------------------------------------------

.org $C24A94
.proc CommandTable

_4A94:

.word $0511, $0570, $0791, $07A4, $45FF, $07A9, $07AE, $07CF
.word $090B, $0933, $094E, $096F, $0990, $0AA4, $0ADC, $0AFA
.word $0B0F, $0B6F, $0B7A, $0B9B, $0BBF, $0C67, $0C6F, $0DE4
.word $0DA2, $0DC3, $0EE0, $0EFE, $0F1F, $0F40, $1125, $0570
.word $1169, $118A, $11B2, $0511, $125E, $12B3, $5B9F, $5B9F
.word $1306, $1333, $13CE, $5B9F, $0814, $09DD, $0A7D, $13EF
.word $141D, $1425, $1490, $14B8, $16A2

.endproc
.reloc

; ---------------------------------------------------------------------------

;Retargets a single target attack if necessary (dead/hidden/etc)
;**optimize: this routine is extremely ineffecient in both speed and size
.org $4AFE
.proc CheckRetarget

_4AFE:
        ldx AttackerOffset
        phx
        stz $0E
        lda MonsterTargets
        beq NoMon
        inc $0E			;flag for monsters targetted
        jsr CountSetBits
        dex
        beq SingleTarget
        jmp Finish		;abort if multiple monster targets
NoMon:	LDA PartyTargets
        bne Party
        jmp Finish		;abort if no targets selected
Party:	JSR CountSetBits
        dex
        beq SingleTarget
        jmp Finish		;abort if multiple party targets
SingleTarget:
        lda MonsterTargets	;monsters 0-7
        pha
        and #$F0		;select monsters 0-3
        jsr ShiftDivide_16	;shift into slots 4-7
        ora PartyTargets	;party into slots 0-3
        sta $10			;first byte of target bitmask
        pla
        jsr ShiftMultiply_16	;shift monsters 4-7 into slots 8-11
        sta $11			;second byte of target bitmask
        tdc
        tax
FindTargetLoop:									
        asl $11
        rol $10
        bcs FoundTarget
        inx 			;due to earlier checks
        bra FindTargetLoop	;there should always be a target to escape loop
FoundTarget:								
        lda ActiveParticipants,X	;is target active?
        beq Retarget
        txa
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        and #$C0		;Dead or Stone	(but still active, somehow?)
        bne Retarget
        lda CharStruct::Status4,X
        and #$81		;Erased or Hidden
        bne Retarget
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        beq Finish		;target is fine, we're done
Retarget:									
        lda $0E			;flag for monsters targetted
        bne RetargetMonster
        stz $12
        lda #$04
        sta $13
        jsr CheckValidTargetsExist	;checks 0-3 for party
        bne Finish			;give up if it failed
        tdc
        tax
        lda #$03
        jsr Random_X_A
        bra TryNewTarget
        							;:skip3
RetargetMonster:
        lda #$04
        sta $12
        lda #$0C
        sta $13
        jsr CheckValidTargetsExist	;checks 4-11 for monsters
        bne Finish			;give up if it failed
        lda SandwormBattle
        beq :+
        ldx #$0004		;only retarget to holes for sandworm fight
        lda #$06
        bra :++
:	LDX #$0004		;otherwise retarget to any monster slot
        lda #$0B
:	JSR Random_X_A
TryNewTarget:
        sta $10
        tax
        lda ActiveParticipants,X
        beq Retarget		;if we chose an invalid target, start over
        lda $10
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        and #$C0		;Stone or Dead
        bne Retarget
        lda CharStruct::Status4,X
        and #$81		;Erased or Hidden
        bne Retarget
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        bne Retarget
        lda $0E			;flag for monsters targetted
        bne SetMonsterTargets
        lda $10
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets
        bra Finish
SetMonsterTargets:								
        sec
        lda $10
        sbc #$04
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
Finish:	PLX 			;original AttackerOffset
        stx AttackerOffset
        rts

.endproc

; ---------------------------------------------------------------------------

;Checks if any of the slots in a range are valid targets
;$12 is the first target to consider
;$13 is where to stop (one past the targets to check)
;result is in A and NewTargetFailed, and (as it's used) also the zero flag
;returns 0 if retargetting succeeded, 1 if not
;new target offset is in X
.proc CheckValidTargetsExist

_4BD7:
        lda $12
        tay
        lda ActiveParticipants,Y
        beq CheckNext
        lda $12
        longa
        jsr ShiftMultiply_128
        tax
        shorta0
        lda CharStruct::Status1,X
        and #$C0		;Dead or Stone
        bne CheckNext
        lda CharStruct::Status4,X
        and #$81		;Erased or Hidden
        bne CheckNext
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        bne CheckNext
        tdc
        bra Finish
CheckNext:								
        inc $12
        lda $12
        cmp $13
        bne CheckValidTargetsExist
        lda #$01
Finish:								
        sta NoValidTargets
        rts

.endproc

; ---------------------------------------------------------------------------


;Check if the last attack was learnable blue magic, 
;and queue it for learning if needed
.proc CheckLearnBlue

_4C10:
        lda AttackerIndex
        cmp #$04		;monster check
        bcs Monster
        rts 			;can only learn from monsters
Monster:
        sec
        sbc #$04		;now monster index
        jsr ShiftMultiply_16
        longa
        clc
        adc #MonsterMagic
        sta $3D			;address of monster blue magic table?
        shorta0
        tdc
        tay
        sty $3F			;spell table index
CheckBlueLoop:
        ldy $3F
        lda ($3D),Y
        sta $08			;monster blue magic (potentially)
        beq EarlyRet
        cmp #$FF
        beq EarlyRet
        cmp #$82		;first blue magic
        bcc NotBlue
        cmp #$A0		;after last blue magic
        bcc ValidBlue
NotBlue:
        jmp NextSpell
EarlyRet:
        rts
ValidBlue:
        stz $0E
        lda $3F			;spell table index
        tax
        lda ROMTimes7,X  	;*7, size of ActionAnim struct
        tax
        lda ActionAnim0::TargetBits,X
        sta $0F			;target bits
        lda ActionAnim0::Flags,X
        and #$40		;target was a monster
        beq :+
        lda ActionAnim0::ReflecteeBits,X
        sta $0E			;target after reflect
        stz $0F			;original target doesn't matter
:	LDA $0F
        ora $0E
        sta $0E			;final target bits
        beq NextSpell		;no targets
        tdc
        tax
        stx $10
TargetLoopLDX:
        ldx $10
TargetLoop:
        asl $0E
        inc $10
        bcs TargetHit
        inx
        cpx #$0008
        bne TargetLoop
        bra NextSpell
TargetHit:
        longa
        txa 		;target index
        jsr ShiftMultiply_128
        tay 		;target offset
        shorta0
        lda CharStruct::Passives1,Y
        and #$10	;learning
        beq TargetLoopLDX
        lda $08		;monster blue magic
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMMagicInfo::Misc,X
        and #$F0	;flags
        sta $10
        bmi NextSpell  ;monster bit (monster only?)
        and #$40	;learnable
        beq :+
        lda #$64       	;100%
        bra CheckLearn
:	LDA $10
        and #$20	;learnable (never used)
        beq :+
        lda #$32       	;50%
        bra CheckLearn
:	LDA $10
        and #$10	;learnable (never used)
        beq NextSpell
        lda #$0A       	;10%
CheckLearn:
        sta $0E
        jsr Random_0_99
        cmp $0E
        bcs NextTarget
        lda BlueLearnedCount
        cmp #$08       	;max 8
        beq Ret
        tax
        lda $08		;blue spell learned
        sta BlueLearned,X
        inc BlueLearnedCount
        bra NextSpell
NextTarget:
        bra TargetLoopLDX
NextSpell:	
        inc $3F			;spell table index
        jmp CheckBlueLoop
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc StartBattle
_4CE0:
        jsr InitBattle
        tdc
        tax
:	ADC a:$0000,X		;Sum first 1000 bytes of ram
        inx
        cpx #$03E8
        bne :-
        										;					
        adc RNGSeed		;add RNG seed
        sta RNGA
        										;					
:	ADC a:$0000,X		;Sum another 1000 bytes of ram
        inx
        cpx #$07D0
        bne :-
        										;					
        sta RNGB
        tdc
        tax
:	LDA ROMGlobalTimer,X		;copies table of constants for status timers
        sta GlobalTimer,X
        inx
        cpx #$000B
        bne :-
        										;					
        lda Config1
        and #$0F		;battle mode and speed
        tax
        lda ROMBattleSpeedTable,X
        sta ATBWaitTime
        longa
        lda EncounterIndex
        jsr ShiftMultiply_16
        tax
        shorta0
        tay
        										;					
:	LDA ROMEncounterInfo,X
        sta EncounterInfo,Y	;copy encounter info from rom
        iny
        inx
        cpy #$0010
        bne :-
        jsr CheckOneTimeEncounters
        tdc
        tax
        tay
CopyMonsterIDs:
        lda EncounterInfo::MonsterID,X
        sta BattleMonsterID,Y
        cmp #$FF
        bne :+
        sta BattleMonsterID+1,Y
        bra :++
:	LDA EncounterInfo::Flags
        and #$20		;boss battle
        beq :+
        lda #$01
        sta BattleMonsterID+1,Y
:	INX
        iny
        iny
        cpy #$0010		;8 monster slots *2
        bne CopyMonsterIDs
        tdc
        tay
        tax
        stx $10
        lda #$D0
        sta $14
        ldy #$0000
        sty $12			;$12 now holds $D00000 address
        ldy $10
        lda BattleMonsterID+1,Y
        beq CopyMonsterStats
        lda #$D0
        sta $14			;set this again, for some reason
        ldy #$2000		;shift to boss section of the table
        sty $12
CopyMonsterStats:
        ldy $10
        lda EncounterInfo::MonsterID,Y
        longa
        jsr ShiftMultiply_32
        tay
        shorta0
        stz $0E
        										;					
:	LDA [$12],Y		;ROMMonsterStats
        sta MonsterStats,X
        iny
        inx
        inc $0E
        lda $0E
        cmp #$20		;struct is 32 bytes long
        bne :-
        										;					
        inc $10
        lda $10
        cmp #$08		;8 monsters
        bne CopyMonsterStats
;set up visible and active monster tracking										;	
        tdc
        tax
        lda EncounterInfo::Visible
        sta MonstersVisibleUnused
        sta MonstersVisible
        sta MonsterKillTracker
        pha
        eor #$FF
        sta InactiveMonsters
        pla
        										;					
:	ASL
        bcc :+
        inc InitialMonsters,X
:	INX
        cpx #$0008
        bne :--
        										;					
        longa
        lda EncounterIndex
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
;set up monster X/Y coordinates on screen											
:	LDA ROMMonsterCoordinates,X
        sta MonsterCoordinates,Y
        inx
        iny
        cpy #$0008
        bne :-
        										;					
        tdc
        tax
;override monster ID $FF to be active
:	LDA EncounterInfo::MonsterID,X
        cmp #$FF
        bne :+
        lda MonsterKillTracker
        jsr SetBit_X
        sta MonsterKillTracker
:	INX
        cpx #$0008
        bne :--
        										;					
        jsr LoadStatsEquipmentAI
        jsr SetupInventoryMagic
        jsr RandomizeOrder
        jsr ResetATBAll
        jsr UpdateMonsterList
        jsr CopyHPMPStatus
        jsr SetupMusic
        jsr CheckAmbushes
        lda #$02		;C1 routine: init battle graphics
        jsr CallC1
        jsr AdvanceStartingATB
        lda MessageBoxes
        beq :+
        lda #$0A		;C1 routine: execute graphics script
        jsr CallC1
:	JMP MainBattleLoop

.endproc

; ---------------------------------------------------------------------------

;set up sandworm and one-time encounters
.proc CheckOneTimeEncounters

_4E25:
        lda EncounterInfo::Flags
        and #$10	;sandworm
        beq OneTimeEncounters
        inc SandwormBattle
        rts
OneTimeEncounters:
        tdc
        tax
        stx $10
CheckEncountersLoop:													
        ldx $10
        lda EncounterIndex
        cmp ROMOneTime::Encounter,X
        bne Next
        lda EncounterIndex+1
        cmp ROMOneTime::Encounter+1,X
        bne Next
        lda ROMOneTime::Replacement,X
        sta $12
        lda ROMOneTime::Replacement+1,X
        sta $13
        txa 			;always 0-28 and a multiple of 4
        jsr ShiftDivide_4	;0-7
;**optimize: this whole section is pointless, but maybe they had more than 8 of these at one point
        stz $0E
        lsr
        ror $0E
        lsr
        ror $0E
        lsr
        ror $0E
        tay 			;always 0
        lda $0E
        jsr ShiftDivide_32	;this recovers the original 0-7 number
        tax
        lda BattleData::EventFlags+1,Y	;Y is always 0 here
        jsr SelectBit_X			;selects event flag for this fight
        bne ChangeEncounter		;change the fight if we've done it
Next:	INC $10
        inc $10
        inc $10
        inc $10				;+4 for next encounter replacement
        lda $10
        cmp #$20			;8 total replacements in table
        bne CheckEncountersLoop
        rts
ChangeEncounter:
        longa
        lda $12			;replacement encounter
        sta EncounterIndex
        jsr ShiftMultiply_16
        tax
        shorta0
        tay
:	LDA ROMEncounterInfo,X
        sta EncounterInfo,Y
        iny
        inx
        cpy #$0010		;16 byte struct
        bne :-
        rts

.endproc

; ---------------------------------------------------------------------------

;Advances ATB so that the lowest ATB gets their turn immediately
;also applies Masamune's Initiative effect but it's buggy
.org $4E9F
.proc AdvanceStartingATB

_4E9F:
        lda #$FF
        sta $0E
        tdc
        tax
        tay
FindLowestATB:													
        lda ActiveParticipants,Y
        beq NextLowestATB
        lda CurrentTimer::ATB,X
        cmp $0E
        bcs NextLowestATB
        sta $0E			;current lowest
NextLowestATB:													
        txa
        clc
        adc #$0B	;next char timer offset
        tax
        iny
        cpy #$000C	;12 participants
        bne FindLowestATB
        sec
        lda $0E		;lowest atb
        sbc #$02	;-2
        bcs :+
        tdc		;min 0
:	STA $0E
        tdc
        tax
        tay
SubtractLowestATB:
        lda ActiveParticipants,Y
        beq NextSubtractLowest
        sec
        lda CurrentTimer::ATB,X
        sbc $0E		;lowest ATB -2
        sta CurrentTimer::ATB,X
NextSubtractLowest:
        txa
        clc
        adc #$0B	;next char timer offset
        tax
        iny
        cpy #$000C    	;12 participants
        bne SubtractLowestATB
        tdc
        tax
        tay
        ;**bug: they forgot to init $0E to zero here,
        ;so this writes 1 to random timer values after setting up initiative
        ;luckily, the range it can reach is all within CurrentTimer and InitialTimer
        ;and the 1s written there seem fairly harmless
CheckInitiative:	;this generally loops ~200 times due to the bug						   
        lda EncounterInfo::IntroFX
        bmi NextInitiative	;80h: credits demo battle
        lda CharStruct::WeaponProperties,X
        and #$20      		;initiative
        beq NextInitiative
        lda #$01
        sta CurrentTimer::ATB,Y
NextInitiative:													
        tya
        clc
        adc #$0B      	;next char timer offset
        tay
        jsr NextCharOffset
        inc $0E		;should be char index, but is likely >4
        lda $0E
        cmp #$04	;4 characters, will loop until the byte wraps around
        bne CheckInitiative
        rts

.endproc

; ---------------------------------------------------------------------------

.org $4F0A
.proc InitBattle

_4F0A:
        ldx #$0067
        tdc
:	STA $00,X	;Clears $00-67
        dex
        bpl :-
        ldx #$5CD7
        tdc
:	STA $2000,X	;Clears all of the ram used in battle, except ResetBattle
        dex
        bpl :-
        txa 		;A now $FF
        sta ATBReadyQueue
        sta ATBReadyQueue+1
        sta ATBReadyQueue+2
        sta ATBReadyQueue+3
        sta ATBReadyQueue+4
        sta DisplayInfo::CurrentChar
        sta ReleasedMonsterID
        ldx #$01FF
:	STA GFXQueue,X		;Init gfx queue to $FF, which is considered empty
        dex
        bpl :-
        tdc
        tax
:	LDA FieldData,X	;copy data like Escape count and battle event flags
        sta BattleData,X	;from field structure to battle structure
        inx
        cpx #$0020
        bne :-
        lda FieldTimerEnable
        sta BattleTimerEnable
        lda FieldTimer
        sta BattleTimer
        lda FieldTimer+1
        sta BattleTimer+1
        inc MonsterNextForm
        lda #$40			;set this bit for all monsters
        InstructionOffset sta, CharStruct, CharRow, 4
        InstructionOffset sta, CharStruct, CharRow, 5
        InstructionOffset sta, CharStruct, CharRow, 6
        InstructionOffset sta, CharStruct, CharRow, 7
        InstructionOffset sta, CharStruct, CharRow, 8
        InstructionOffset sta, CharStruct, CharRow, 9
        InstructionOffset sta, CharStruct, CharRow, 10
        InstructionOffset sta, CharStruct, CharRow, 11
        rts

.endproc

; ---------------------------------------------------------------------------

;(check for and set up preemptive and back attacks)
.proc CheckAmbushes

_4F7A:
        tdc
        tax
        tay
:	LDA CharStruct::CharRow,X
        sta SavedCharRow,Y
        jsr NextCharOffset
        iny
        cpy #$0004	;4 characters
        bne :-
        lda BattleMonsterID+1	;indicates boss	Monster
        bne CheckBackAttack
        InstructionOffset lda, CharStruct, Passives2, 0
        InstructionOffset ora, CharStruct, Passives2, 1
        InstructionOffset ora, CharStruct, Passives2, 2
        InstructionOffset ora, CharStruct, Passives2, 3
        sta $0E			;combined passives2
        and #$04      		;caution
        bne CheckPreemptive
CheckBackAttack:							
        lda EncounterInfo::Flags
        bmi _BackAttack		;80h: Always Back Attack
        lda BattleMonsterID+1
        bne Finish		;boss
        lda EncounterInfo::IntroFX
        bmi Finish		;80h: Credits Demo
        lda ResetBattle
        bne CheckPreemptive
        tdc
        tax
        lda #$FF
        jsr Random_X_A
        cmp #$10      		;16/256 chance, or 1 in 16
        bcs CheckPreemptive
_BackAttack:
        inc BackAttack
        jsr SetupBackAttack
        bra Finish
CheckPreemptive:													
        lda #$20	;32/256 by default, or 1 in 8
        sta $10		;preemptive chance
        lda $0E		;combined passives2
        and #$02      	;pre-emptive
        beq :+
        lda #$40	;64/256, or 1 in 4
        sta $10
:	TDC
        tax
        lda #$FF
        jsr Random_X_A
        cmp $10       	;preemptive chance
        bcs Finish
        jsr Preemptive
        						;:
Finish:	STZ ResetBattle
        rts

.endproc

; ---------------------------------------------------------------------------

;(sets up back attack)
.proc SetupBackAttack

_4FEA:
        lda #$1C	;back attack message
        sta MessageBoxes
        tdc
        tax 		;first character offset to adjust ATB
        tay 		;timer offset
        sty $0E		;first char index to adjust
        lda #$04
        sta $10		;char index to finish on
        stz $12		;0 to flip rows
        lda #$3C
        sta $16		;ATB adjustment
        jmp AdjustATB

.endproc

; ---------------------------------------------------------------------------

;(sets up pre-emptive strikes)
.proc Preemptive

_5001:
        lda #$1B	;pre-emptive strike message
        sta MessageBoxes
        ldx #$0200	;first character offset to adjust ATB
        ldy #$002C	;timer offset
        lda #$04
        sta $0E		;first char index to adjust
        lda #$0C
        sta $10		;char index to finish on
        sta $12		;don't flip rows
        lda #$5A
        sta $16		;ATB adjustment
        jmp AdjustATB

.endproc

; ---------------------------------------------------------------------------

;(finishes pre-emptive/back attack processing, but written such it could be used in different ways)
;X: first character offset to adjust ATB
;Y: timer offset
;$0E: first char index to adjust
;$10: char index to finish on
;$12: 0 to flip rows
;$16: ATB adjustment
.proc AdjustATB

_501D:
        lda CharStruct::WeaponProperties,X
        and #$20	;initiative
        bne SwapRow	;skips atb adjustment for back attack
        clc
        lda CurrentTimer::ATB,Y
        adc $16
        bcc :+
        lda #$FF	;cap at 255
:	STA CurrentTimer::ATB,Y
        lda $12
        bne Next
SwapRow:														
        lda CharStruct::CharRow,X
        and #$7F
        sta $14		;everything but row
        lda CharStruct::CharRow,X
        and #$80	;row
        eor #$80	;flip row
        ora $14		;recombine
        sta CharStruct::CharRow,X
Next:								
        jsr NextCharOffset
        tya
        clc
        adc #$0B	;next character timer offset
        tay
        inc $0E		;char index
        lda $0E
        cmp $10		;end index
        bne AdjustATB
        tdc
        jmp GFXCmdMessageClearAnim

.endproc

; ---------------------------------------------------------------------------

;set up combat music
.org $505C
.proc SetupMusic

_505C:
        lda EncounterInfo::Music
        bmi Ret
        lda EncounterInfo::Music
        jsr ShiftDivide_8
        tax
        lda ROMMusicTable,X
        jsr MusicChange
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc EndBattle

_5070:
        jsr WipeDisplayStructures
        ldx #$0007		;clear 8 bytes
:	STZ FieldItemsWon,X
        stz BattleItemsWon,X
        dex
        bpl :-
        lda ResetBattle
        beq CheckDead
        jmp Finish
CheckDead:
        lda BattleOver
        and #$40		;party dead
        beq CheckEscaped
        lda #$01
        jsr GFXCmdMessageClearAnim
        lda #$75    		;game over music
        jsr MusicChange
        lda #$01		;game over flag
        sta BattleData::EventFlags
        lda #$2A		;message
        sta MessageBoxes
        jmp MessageDisplay
CheckEscaped:
        lda BattleOver
        and #$01		;escaped
        beq CheckVictory
        lda #$02		;escaped flag
        sta BattleData::EventFlags
        lda #$80		;??
        sta ActionAnim0::TargetBits
        lda #$0C		;C1 routine
        jsr CallC1
        jsr ResetStats
        jsr ApplyPartyGear
        jsr UpdateFieldData
        lda #$2B		;message
        sta MessageBoxes
        tdc
        jsr GFXCmdMessageClearAnim
        clc
        lda BattleData::Escapes
        adc #$01
        bcc :+
        lda #$FF    		;255 cap
:	STA BattleData::Escapes
        bra MessageDisplay
CheckVictory:
        stz BattleData::EventFlags	;victory
        jsr ResetStats
        lda BattleOver
        bpl NoReward		;ended in a way other than enemies dying
        lda EncounterInfo::Flags
        and #$02		;no reward flag
        bne NoReward
        lda EncounterInfo::Music
        bmi :+			;no track change
        lda #$70    		;victory music
        jsr MusicChange
:	LDA #$0D		;C1 routine
        jsr CallC1
        lda #$01
        jsr GFXCmdMessageClearAnim
        lda #$29		;message
        sta MessageBoxes
        lda #$0A		;C1 routine: execute graphics script
        jsr CallC1
        jsr GetLootExp
NoReward:
        jsr ApplyPartyGear
        jsr UpdateFieldData
        bra CleanupItems
MessageDisplay:			;maybe also item drop window?
        lda #$0A		;C1 routine: execute graphics script
        jsr CallC1
CleanupItems:
        jsl $D0EF78 ;CleanupFieldItems_D0 ; very strange code, its code in the data bank
        jsr MergeItemDupes
        tdc
        tax
CopyBattleData:
        lda BattleData,X
        sta FieldData,X
        inx
        cpx #$0020		;32 bytes data copied
        bne CopyBattleData
        lda BattleTimer
        sta FieldTimer
        lda BattleTimer+1
        sta FieldTimer+1
        lda #$0E		;C1 routine
        jsr CallC1
Finish:
        tdc
        tax
:	ORA BattleItemsWon,X
        inx
        cpx #$0008
        bne :-
        pha 		;non-zero A means there was an item drop
        pla
        bne Ret
        lda EncounterInfo::Music
        bmi Ret	;no track change
        lda #$7F
        jsr MusicChange
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

.proc ResetStats

_515C:
        tdc
        tay
        tax
        stx $0E
CopyBaseStats:
        lda FieldChar::Level,Y
        sta CharStruct::Level,X
        lda FieldChar::BaseStr,Y
        sta CharStruct::BaseStr,X
        lda FieldChar::BaseAgi,Y
        sta CharStruct::BaseAgi,X
        lda FieldChar::BaseVit,Y
        sta CharStruct::BaseVit,X
        lda FieldChar::BaseMag,Y
        sta CharStruct::BaseMag,X
        jsr NextCharOffset
        longa
        tya
        clc
        adc #$0050		;size of FieldChar struct
        tay
        shorta0
        inc $0E
        lda $0E
        cmp #$04		;4 chars
        bne CopyBaseStats
        tdc
        tax
        stx $0E
        stx $10
ClearEffects:
        ldx $0E
        stz CharStruct::CmdStatus,X
        stz CharStruct::DamageMod,X
        stz CharStruct::ElementUp,X
        tdc
        tay
ClearResists:	;resists, status immunities, weapon/armor properties
        stz CharStruct::EAbsorb,X
        inx
        iny
        cpy #$000A		;10 bytes data cleared
        bne ClearResists
        ldx $0E
        jsr NextCharOffset
        stx $0E
        inc $10
        lda $10
        cmp #$04		;4 chars
        bne ClearEffects
        rts

.endproc

; ---------------------------------------------------------------------------

;Checks for any duplicate item ids, and merge them together if they match
.proc MergeItemDupes

_51C2:
        tdc
        tax
BaseLoop:
        txy
        iny
SearchLoop:
        lda FieldItems,X
        beq NextBaseItem
        cmp FieldItems,Y
        bne NextSearchItem
        clc
        lda FieldItemsQty,X	;match, merge quantities into base item
        adc FieldItemsQty,Y
        cmp #$63
        bcc :+
        lda #$63		;max 99
:	STA FieldItemsQty,X
        tdc 			;clear dupe's old slot
        sta FieldItems,Y
        sta FieldItemsQty,Y
NextSearchItem:
        iny
        cpy #$0100
        bne SearchLoop
NextBaseItem:
        inx
        cpx #$00FF
        bne BaseLoop
        rts

.endproc

; ---------------------------------------------------------------------------

;Restores original Max HP and resists
;Copies character stats and inventory from battle to field structures
;Updates play time frame count
.proc UpdateFieldData

_51F4:
        tdc
        tax
        tay
        sty $10
        stx $12
CopyCharLoop:
        stz $0E
        lda $10			;char index
        tax
        lda GiantDrink,X
        beq ResetStatus
        phy
        lda $10
        asl
        tay
        longa
        lda OriginalMaxHP,Y 	;resets giant drink hp increase
        ldx $12			;char offset
        sta CharStruct::MaxHP,X
        cmp CharStruct::CurHP,X
        bcs :+
        sta CharStruct::CurHP,X
:	TDC
        shorta
        ply
ResetStatus:			;keeps Status1 though
        ldx $12			;char offset
        stz CharStruct::Status2,X
        stz CharStruct::Status3,X
        stz CharStruct::Status4,X
        stz CharStruct::CmdStatus,X
        ldx $10			;char index
        lda SavedCharRow,X
        ldx $12
        sta CharStruct::CharRow,X
CopyStats:
        lda CharStruct,X
        sta FieldChar,Y
        inx
        iny
        inc $0E
        lda $0E
        cmp #$46		;70 bytes
        bne CopyStats
        longa
        tya
        clc
        adc #$000A		;10 bytes, adding up to size of FieldChar
        tay
        shorta0
        ldx $12
        jsr NextCharOffset
        stx $12
        inc $10
        lda $10
        cmp #$04		;4 chars
        bne CopyCharLoop
        ldx #$00FF
CopyInventory:
        lda InventoryItems,X
        sta FieldItems,X
        bne CopyQty
        tdc 			;Qty 0 for empty slot
        bra :+
CopyQty:
        lda InventoryQuantities,X
:	STA FieldItemsQty,X
        bne :+
        sta FieldItems,X		;Empty slot for Qty 0
:	DEX
        bpl CopyInventory
        ;update played time (32 bit frame count)
        longa
        clc
        lda FieldFrameCount
        adc a:BattleFrameCount
        sta FieldFrameCount
        lda FieldFrameCount+2
        adc a:BattleFrameCount+2
        sta FieldFrameCount+2
        bcc :+
        lda #$FFFF
        sta FieldFrameCount
        sta FieldFrameCount+2
:	TDC
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

;Adds Gil, Exp and AP from monsters
;queues up any item drops to be collected later
;applies level and job level ups
.org $52A2
.proc GetLootExp

_52A2:
        lda MonsterKillTracker
        eor #$FF
        sta MonsterKillTracker	;inverted, anything killed is now set
        jsr CountSetBits
        longa
        txa
        clc
        adc MonsterKillCount
        bcc :+
        lda #$FFFF		;max 65535 tracked kills
:	STA MonsterKillCount
        shorta0
        tdc
        tax
        stx VictoryGil
        stx VictoryGil+2	;also clears first byte of VictoryExp
        stx VictoryExp+1
        stx TempMonsterIndex
        ldy #$0200		;first monster offset
        sty TempMonsterOffset
TallyLoot:	;gil, exp and drops
        ldx TempMonsterIndex	;monster index
        lda MonsterKillTracker
        jsr SelectBit_X
        beq NextLoot
        clc
        lda VictoryGil
        adc CharStruct::RewardGil,Y
        sta VictoryGil
        lda VictoryGil+1
        adc CharStruct::RewardGil+1,Y
        sta VictoryGil+1
        lda VictoryGil+2
        adc #$00		;for carry
        sta VictoryGil+2
        clc
        lda VictoryExp
        adc CharStruct::RewardExp,Y
        sta VictoryExp
        lda VictoryExp+1
        adc CharStruct::RewardExp+1,Y
        sta VictoryExp+1
        lda VictoryExp+2
        adc #$00		;for carry
        sta VictoryExp+2
        jsr DropMonsterLoot
NextLoot:
        ldy TempMonsterOffset
        longa
        tya
        clc
        adc #$0080		;next monster offset
        tay
        shorta0
        sty TempMonsterOffset
        inc TempMonsterIndex	;monster index
        lda TempMonsterIndex
        cmp #$08		;8 monsters
        bne TallyLoot
        clc
        lda Gil
        adc VictoryGil
        sta Gil
        lda Gil+1
        adc VictoryGil+1
        sta Gil+1
        lda Gil+2
        adc VictoryGil+2
        sta Gil+2
        sec 			;cap gil at 9999999
        lda Gil
        sbc #$7F
        lda Gil+1
        sbc #$96
        lda Gil+2
        sbc #$98
        bcc AddAP
        lda #$7F
        sta Gil
        lda #$96
        sta Gil+1
        lda #$98
        sta Gil+2
AddAP:
        lda EncounterInfo::AP
        tax
        stx $0E			;encounter ap
        tdc
        tax
        stx $10
        stx $12
APLoop:
        lda $12			;char index
        tay
        lda ActiveParticipants,Y
        beq NextAP
        inc $10			;count of active chars
        longa
        clc
        lda CharStruct::AP,X
        adc $0E			;encounter ap
        bcc :+
        lda #$FFFF		;cap at 65535
:	STA CharStruct::AP,X
        shorta0
NextAP:
        jsr NextCharOffset
        inc $12			;char index
        lda $12
        cmp #$04		;4 chars
        bne APLoop
        lda VictoryExp
        ora VictoryExp+1
        ora VictoryExp+2
        beq :+
        jsr DivideExp
:	LDA MonsterKillTracker
        bne AddExp
        stz VictoryExp		;didn't kill anything, so no exp
        stz VictoryExp+1
        stz VictoryExp+2
AddExp:
        tdc
        tax
        stx $0E
ExpLoop:
        lda $0E				;char index
        tay
        lda ActiveParticipants,Y
        beq NextExp
        clc
        lda CharStruct::Exp,X
        adc VictoryExp
        sta CharStruct::Exp,X
        lda CharStruct::Exp+1,X
        adc VictoryExp+1
        sta CharStruct::Exp+1,X
        lda CharStruct::Exp+2,X
        adc VictoryExp+2
        sta CharStruct::Exp+2,X
        sec
        lda CharStruct::Exp,X		;cap exp at 9999999
        sbc #$7F
        lda CharStruct::Exp+1,X
        sbc #$96
        lda CharStruct::Exp+2,X
        sbc #$98
        bcc NextExp
        lda #$7F
        sta CharStruct::Exp,X
        lda #$96
        sta CharStruct::Exp+1,X
        lda #$98
        sta CharStruct::Exp+2,X
NextExp:
        jsr NextCharOffset
        inc $0E				;char index
        lda $0E
        cmp #$04
        bne ExpLoop			;4 members
;Display exp/gil/ap message boxes
        lda #$FF			;flag for unused message box
        sta MessageBoxes
        lda #$01
        jsr GFXCmdMessageClearAnim
        tdc
        tax
        lda VictoryGil
        sta MessageBoxData
        lda VictoryGil+1
        sta MessageBoxData::_1
        lda VictoryGil+2
        sta MessageBoxData+2
        lda VictoryGil
        ora VictoryGil+1
        ora VictoryGil+2
        beq :+
        lda #$2C			;gil message
        sta MessageBoxes,X
        inx 				;increment message box slot if used
:	LDA VictoryExp
        sta MessageBoxData1+0		;data slots are hardcoded tho
        lda VictoryExp+1		;..maybe they fix it later
        sta MessageBoxData1::_1
        lda VictoryExp+2
        sta MessageBoxData1+2
        lda VictoryExp
        ora VictoryExp+1
        ora VictoryExp+2
        beq :+
        lda #$2D			;exp message
        sta MessageBoxes,X
        inx
:	LDA EncounterInfo::AP
        beq :+
        sta MessageBoxData2
        stz MessageBoxData2::_1
        stz MessageBoxData2::_2
        jsr CheckHideAP
        lda $0E				;result, 1 to hide AP
        bne :+
        lda #$2E			;ap message
        sta MessageBoxes,X
:	LDA MessageBoxes
        cmp #$FF			;flag for unused message box
        beq Blue
        lda #$0A			;C1 routine: exec graphics script
        jsr CallC1
        lda #$FF
        sta MessageBoxes+1
Blue:
        lda BlueLearnedCount
        beq CheckLevelUp
        tdc
        tax
        stx $3D
BlueLoop:
        ldx $3D				;learned blue index
        lda BlueLearned,X
        beq CheckLevelUp
        cmp #$82			;first blue spell
        bcc CheckLevelUp
        cmp #$A0			;after blue spells
        bcs CheckLevelUp
        sta MessageBoxData+0
        stz MessageBoxData+1
        stz MessageBoxData+2
        stz $0E
        lsr
        ror $0E
        lsr
        ror $0E
        lsr
        ror $0E
        tay 				;MagicBits offset
        lda $0E
        jsr ShiftDivide_32
        tax 				;MagicBits spell
        lda MagicBits,Y
        jsr SelectBit_X
        bne NextBlue			;already know this one
        lda MagicBits,Y
        jsr SetBit_X
        sta MagicBits,Y
        lda #$01
        jsr GFXCmdMessageClearAnim
        lda #$32			;learned blue message
        sta MessageBoxes
        lda #$0A			;C1 routine: exec graphics script
        jsr CallC1
NextBlue:
        inc $3D				;learned blue index
        bra BlueLoop			;**bug?: no range check
        				;..relies on the next byte never being a valid blue spell if 8 blue spells were used
        				;..fortunately it seems to always be 0 or 1
CheckLevelUp:
        tdc
        tax
        stx $3D
        stx $3F
LevelLoop:		;will run multiple times for a single character if they gain multiple levels at once
        lda $3D				;char index
        tay
        lda ActiveParticipants,Y
        beq NextChar
        ldx $3F				;char offset
        lda CharStruct::Level,X
        cmp #$63			;skip check if already 99
        bcs NextChar
        tax
        stx $0E				;current level
        longa
        lda $0E
        asl
        sta $10				;level *2
        clc
        adc $0E				;level *3
        tax
        lda ROMLevelExp,X
        sta $0E				;required exp (low bytes)
        shorta0
        lda ROMLevelExp+2,X
        sta $12				;required exp (high byte)
        ldx $3F				;char offset
        sec 				;subtract requirement from current exp
        lda CharStruct::Exp,X
        sbc $0E
        lda CharStruct::Exp+1,X
        sbc $0F
        lda CharStruct::Exp+2,X
        sbc $12
        bcc NextChar			;not enough exp
        jsr LevelUp
        ldx $3F				;char offset
        inc CharStruct::Level,X
        ldx $3F				;still char offset
        lda CharStruct::CharRow,X
        and #$07			;character bits
        sta MessageBoxData+0
        stz MessageBoxData+1
        stz MessageBoxData+2
        lda #$2F			;level up message
        sta MessageBoxes
        lda #$0A			;C1 routine: exec graphics command
        jsr CallC1
        bra LevelLoop
NextChar:
        ldx $3F				;char offset
        jsr NextCharOffset
        stx $3F
        inc $3D				;char index
        lda $3D
        cmp #$04			;4 chars to check
        bne LevelLoop
        tdc
        tax
        stx $3D
        stx $3F
JobUpLoop:
        lda #$FF
        sta MessageBoxes+1
        sta MessageBoxes+2
        lda $3D
        tay
        lda ActiveParticipants,Y
        bne JobLevelLoop
        jmp NextJobCheck
JobLevelLoop:	;run multiple times in case we gained more than 1 job level
        ldx $3F				;char offset
        lda CharStruct::JobLevel,X
        sta $10
        lda CharStruct::Job,X
        cmp #$15			;freelancer/normal
        beq GoNextJobCheck
        tax
        asl
        sta $0E
        lda $10
        cmp ROMJobLevels,X
        bne :+
GoNextJobCheck: ;***
        jmp NextJobCheck
:	LDA $0E
        tax
        lda ROMJobPointers,X
        sta $12
        lda ROMJobPointers+1,X
        sta $13
        lda #$D1; Bank of ROMJobPointers
        sta $14
        lda $10			;job level
        asl
        clc
        adc $10			;job level*3
        tay
        lda [$12],Y
        sta $10			;ap cost (low)
        iny
        lda [$12],Y
        sta $11			;ap cost (hi)
        iny
        lda [$12],Y
        sta $12			;ability
        ldx $3F			;char offset
        sec
        lda CharStruct::AP,X
        sbc $10
        lda CharStruct::AP+1,X
        sbc $11
        bcs :+
        jmp NextJobCheck	;not enough ap
:	SEC
        lda CharStruct::AP,X
        sbc $10
        sta CharStruct::AP,X
        lda CharStruct::AP+1,X
        sbc $11
        sta CharStruct::AP+1,X
        inc CharStruct::JobLevel,X
        lda $3D			;char index
        asl
        tax
        lda ROMAbilityListPointers,X
        sta $0E			;now holds address of char's FieldAbilityList
        lda ROMAbilityListPointers+1,X
        sta $0F
        lda $12			;ability learned
        bpl :+
        and #$7F		;passive, this clears the passive flag bit
        clc
        adc #$4E      		;start at slot after the last active ability
:	ASL
        tax
        lda RomAbilityBitInfo,X
        tay 			;offset of byte containing ability in list
        lda RomAbilityBitInfo+1,X
        tax 			;bit number of ability
        lda ($0E),Y
        jsr SetBit_X
        sta ($0E),Y		;ability now in known list
        lda $3D			;char index
        tax
        inc FieldAbilityCount,X
        ldx $3F			;char offset
        lda CharStruct::CharRow,X
        and #$07		;character bits
        sta MessageBoxData+0
        stz MessageBoxData+1
        stz MessageBoxData+2
        lda #$30		;job level up message
        sta MessageBoxes
        lda #$0A		;c1 routine
        jsr CallC1
        lda #$31		;ability learned message
        sta MessageBoxes
        lda $12			;ability learned
        sta MessageBoxData
        cmp #$2C		;first normal magic command
        bcc :+
        cmp #$4C		;end of normal magic commands
        bcs :+
        sec
        sbc #$2C		;adjust first magic command to start at 0
        tax
        lda ROMJobMagicLevels,X	;look up table for magic level
        sta MessageBoxData1
        lda #$33		;alternate ability learned message
        sta MessageBoxes	;..which shows magic level
:	STZ MessageBoxData+1	;clear high bytes of message box data
        stz MessageBoxData+2
        stz MessageBoxData1::_1
        stz MessageBoxData1+2
        lda #$0A		;c1 routine
        jsr CallC1
        jmp JobLevelLoop
NextJobCheck:
        ldx $3F			;char offset
        jsr NextCharOffset
        stx $3F
        inc $3D			;next char index
        lda $3D
        cmp #$04		;4 chars to check
        beq Ret
        jmp JobUpLoop
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Sets a flag to hide AP display if all present/living characters are freelancers
.proc CheckHideAP

_5674:
        phx
        tdc
        tax
        tay
        stx $0E
Loop:	;searches for a character that's alive and not a freelancer
        lda CharStruct::CharRow,X
        and #$40		;not on the team
        bne Next
        lda CharStruct::Job,X
        cmp #$15		;normal/freelancer
        beq Next
        lda ActiveParticipants,Y
        bne Finish
Next:	JSR NextCharOffset
        iny
        cpy #$0004		;4 chars
        bne Loop
        inc $0E			;all chars dead or freelancer
Finish:	PLX
        rts

.endproc

; ---------------------------------------------------------------------------

.proc DropMonsterLoot

_569A:
        lda EncounterIndex+1
        beq Normal
        lda EncounterIndex
        cmp #$BF		;sandworm fight
        bne Normal
        rts 			;you get nothing
Normal:
        tdc
        tax
        lda #$FF
        jsr Random_X_A    	;0..255
        sta $0E			;random roll
        lda TempMonsterIndex	;monster index
        asl
        tax
        longa
        lda BattleMonsterID,X
        jsr ShiftMultiply_4
        tax
        shorta0
        ldy TempMonsterIndex
        lda ROMLoot::AlwaysDrop,X
        beq CheckRare
        cmp #$80
        beq CheckRare
        cmp #$FF
        beq CheckRare
        sta BattleItemsWon,Y
        rts
CheckRare:	;only checked if AlwaysDrop is $00, $80, or $FF (empty or Item FF)
        lda $0E			;random roll
        cmp #$10		;16/256 chance
        bcs Ret
        lda ROMLoot::RareDrop,X
        beq Ret
        cmp #$80
        beq Ret
        cmp #$FF
        beq Ret
        sta BattleItemsWon,Y
Ret:	RTS

.endproc

; ---------------------------------------------------------------------------

;Applies HP/MP and other changes for a character level up
;**bug: updates max hp without updating the value used for giant drink, which is retored after this
.proc LevelUp

_56EC:
        ldx $10		;old level*2
        lda ROMLevelHP,X
        sta $0E
        sta $2A
        lda ROMLevelHP+1,X
        sta $0F
        sta $2B
        ldx $3F		;char offset
        lda CharStruct::BaseVit,X
        tax
        stx $2C
        jsr Multiply_16bit	;Rom HP * Vit
        ldx #$0005
:	;loop divides by 32
        lsr $31
        ror $30
        ror $2F
        ror $2E
        dex
        bne :-
        ldx $3F		;char offset
        longa
        clc
        lda $0E		;rom hp
        adc $2E		;+ (rom hp * vit)/32
        cmp #$270F
        bcc :+
        lda #$270F	;max 9999
:	STA $08
        lda #$270F
        sta $0A		;save 9999 cap for later routine
        shorta0
        tdc
        tay
CheckHPCommands:	;looks for HP up passives in command slots
        lda CharStruct::BattleCommands,X
        cmp #$8E	;after HP +30%
        bcs NextHPCommandCheck
        cmp #$8B	;HP +10%
        bcc NextHPCommandCheck
        phx
        ldx $10		;old level*2
        phx
        jsr ApplyHPMPPassives
        plx
        stx $10
        plx
NextHPCommandCheck:
        inx
        iny
        cpy #$0004
        bne CheckHPCommands
        ldx $3F		;char offset
        lda $08		;new hp
        sta CharStruct::MaxHP,X
        lda $09
        sta CharStruct::MaxHP+1,X
        ldx $10		;old level*2
        lda ROMLevelMP,X
        sta $0E
        sta $2A
        lda ROMLevelMP+1,X
        sta $0F
        sta $2B
        ldx $3F		;char offset
        lda CharStruct::BaseMag,X
        tax
        stx $2C
        jsr Multiply_16bit	;Rom MP * Mag
        ldx #$0005
:	;loop divides by 32
        lsr $31
        ror $30
        ror $2F
        ror $2E
        dex
        bne :-
        ldx $3F		;char offset
        longa
        clc
        lda $0E		;rom mp
        adc $2E		;+ (rom mp * mag)/32
        cmp #$03E7	;cap at 999
        bcc :+
        lda #$03E7
:	STA $08
        lda #$03E7
        sta $0A		;save 999 cap for later routine
        shorta0
        tdc
        tay
CheckMPCommands:	
        lda CharStruct::BattleCommands,X
        cmp #$90	;after mp +30%
        bcs NextMPCommandCheck
        cmp #$8E	;mp +10%
        bcc NextMPCommandCheck
        jsr ApplyHPMPPassives
NextMPCommandCheck:
        inx
        iny
        cpy #$0004
        bne CheckMPCommands
        ldx $3F		;char offset
        lda $08		;new mp
        sta CharStruct::MaxMP,X
        lda $09
        sta CharStruct::MaxMP+1,X
        rts

.endproc

; ---------------------------------------------------------------------------

;============================ Stud Definitions for compilation purposes =====

.org $25D3
.proc MonsterATB
        rts
.endproc

.org $5AB4
.proc CheckBattleEnd
        rts
.endproc

.org $618A
.proc KillCharacter
        rts
.endproc

.org $6163
.proc HasteSlowMod
        rts
.endproc

.org $9A6F
.proc ApplyGear
        rts
.endproc

.org $992F
.proc GFXCmdAttackNameFromTemp
        rts
.endproc

.org $98FA
.proc FindOpenGFXQueueSlot
        rts
.endproc

.org $9923
.proc SelectCurrentProcSequence
        rts
.endproc

.org $142A
.proc WeaponEffectCommand
        rts
.endproc

.org $98E3
.proc GFXCmdDamageNumbers
        rts
.endproc

.org $9F3A
.proc ReplaceHands
        rts
.endproc

.org $5CE1
.proc CastSpell
        rts
.endproc

.org $0995
.proc JumpCommand_Anim
        rts
.endproc

.org $994C
.proc GFXCmdMessage
        rts
.endproc

.org $49D5
.proc DispatchCommand_CommandReady
        rts
.endproc

.org $30F3
.proc AITargetPersonDead
        rts
.endproc

.org $305F
.proc AITargetPersonJumping
        rts
.endproc

.org $2DDC 
.proc AITargetCharRow
        rts
.endproc

.org $2EB2
.proc AITargetMonsterStatus
        rts
.endproc

.org $2D81
.proc AITargetMonstersExcept
        rts
.endproc

.org $2CB5
.proc AITargetPerson
        rts
.endproc

.org $3E91
.proc ProcessReaction_Party
        rts
.endproc

.org $9A5E
.proc ApplyPartyGear
        rts
.endproc

.org $27BF
.proc CheckAICondition
        rts
.endproc

.org $9427
.proc ProcessCommands
        rts
.endproc

.org $5872
.proc MainBattleLoop
        rts
.endproc

.org $5921
.proc CopyHPMPStatus
        rts
.endproc

.org $5A41
.proc UpdateMonsterList
        rts
.endproc

.org $57C7
.proc ApplyHPMPPassives
        rts
.endproc

.org $5847
.proc GFXCmdMessageClearAnim
        rts
.endproc

.org $57E6
.proc DivideExp
        rts
.endproc