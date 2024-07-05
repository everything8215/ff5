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
Left:       inc TempHand
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
LH:       ldx AttackerOffset
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
Finish:       inc UnknownReaction
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
:       cmp #$05
        bcs :+
        lda #$01	;<5 squirrel
        bra Chosen
:       cmp #$0A
        bcs :+
        lda #$02	;<10 bee swarm
        bra Chosen
:       cmp #$14
        bcs :+
        lda #$03	;<20 nightingale
        bra Chosen
:       cmp #$1E
        bcs :+
        lda #$04	;<30 momonga
        bra Chosen
:       cmp #$28
        bcs :+
        lda #$05	;<40 falcon
        bra Chosen
:       cmp #$32
        bcs :+
        lda #$06	;<50 skunk
        bra Chosen
:       cmp #$3C
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
RH:       jsr SelectCurrentProcSequence
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
LH:       ldx AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        jmp Finish
:       jsr SelectCurrentProcSequence
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
        jmp CommandTable0C::Anim

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
:	    jsr FindOpenGFXQueueSlot
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
RH:	    jsr SelectCurrentProcSequence
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
LH:       ldx AttackerOffset
        lda CharStruct::LHWeapon,X
        bne :+
        jmp Ret
:       jsr SelectCurrentProcSequence
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
        jmp CommandTable31::WeaponEffectCommand

.endproc

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
:       ldx AttackerOffset
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
Finish:	tdc
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
:	tdc
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
Loop:	tdc
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
:       jsr UpdateTimer 	;first timer is stop
        lda $08			;check if stop active
        bne NextChar  		;don't process other timers if stopped
        ldy #$0008  		;process 8 more status timers
:       jsr UpdateTimer
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
:       inc $08    	;timer triggered flag
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
:       inc $0A        	;char count
        lda $0A
        cmp #$0C	;12 chars
        beq :+
        jmp CharLoop
:       pla 		;original RandomOrderIndex
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
.proc TimerEffectJumpTable
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
.endproc
.reloc

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
:       sty $12		;song stat index
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
:       jsr HasteSlowMod
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

.endproc
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

Jump:       jml [$0008]	;jump to AICondition table

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
Match:       inc AIConditionMet
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
Next:	tdc
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
Met:    inc AIConditionMet
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
Met:       inc AIConditionMet
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
Met:       inc AIConditionMet
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
Met:       inc AIConditionMet
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
Met:       inc AIConditionMet
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
Met:       inc AIConditionMet
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
:	tdc
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

Reaction2:		;**bug: didn't load X for this path, but fortunately it's always? correct already
        lda CharStruct::Reaction2Damage,X
        beq Fail
Met:    inc AIConditionMet
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
Next:       jsr NextCharOffset
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
Met:       inc AIConditionMet
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
Next:       jsr NextCharOffset
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
Next:       jsr NextCharOffset
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
Next:       jsr NextCharOffset
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
Next:       jsr NextCharOffset
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
Next:       jsr NextCharOffset
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
Next:       jsr NextCharOffset
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
Loop:       ldy $10
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
Next:       jsr NextCharOffset
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
        jmp AITarget12::AITargetCharRow
                
.endproc

; ---------------------------------------------------------------------------

.proc AITarget24	;random party member on the team?
        lda #$40
        sta $12		;on the team bit?
        stz $13
        jmp AITarget12::AITargetCharRow

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
Dead:       ldy $0E
        stx $08
        lda $08
        sta AITargetOffsets,Y
        lda $09
        sta AITargetOffsets+1,Y
        iny
        iny
        sty $0E
        inc AITargetCount
Next:       jsr NextCharOffset
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
Next:       jsr NextCharOffset
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
Loop:       ldx $0E
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
Next:       inc $0E
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
Next:       jsr NextCharOffset
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
:       cmp #$FF	;end of ai
        bne :+
        jmp EndSequence
:       cmp #$FD	;special command
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
:       cmp #$F2
        bne :+
        jsr AIShowMonster	;command $F2
        bra GoLoop
:       cmp #$F3
        bne :+
        jsr AISetTarget     	;command $F3
        bra GoLoop
:       cmp #$F4
        bne :+
        jsr AISetVar     	;command $F4
        bra GoLoop
:       cmp #$F9
        bne :+
        jsr AISetEventFlag     	;command $F9
        bra GoLoop
:       cmp #$FA
        bne :+
        jsr AISetStatsToggleStatus     	;command $FA
        bra GoLoop
:       jsr AICopyCommand     	;could be F0,F1,F5-F8,FB-FF
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
:       cmp #$EC	;a dummy spell
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
:       ldx AttackerOffset
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
Finish: inc AICurrentCheckedSet
        lda AICurrentCheckedSet
        cmp #$0A	;10 sets
        bne CheckSets
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc SaveActionData

_3C7F:
        tdc
        tay
        ldx AttackerOffset
:       lda CharStruct::ActionFlag,X
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
:       lda MonsterAIScript,X
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
:       lda MonsterMagic,X
        sta SavedMonsterMagic,Y
        inx
        iny
        cpy #$0010	;16 bytes copied
        bne :-
        tdc
        tay
        ldx $10
:       lda MMTargets::Party,X
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
:       lda SavedAction2,Y
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
:       lda SavedMonsterAIScript,Y
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
:       lda SavedMonsterMagic,Y
        sta MonsterMagic,X
        inx
        iny
        cpy #$0010	;16 bytes
        bne :-
        tdc
        tay
        ldx $10
:       lda SavedMMTargets,Y
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
:       cmp $0E
        bne :+
        stz PauseTimerChecks,X
        bra :++
:       inc PauseTimerChecks,X
:       inc $0E
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
:       stz PauseTimerChecks,X
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
:       sta MonsterMagic,X
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
:       sta AIBuffer,X
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
:       lda (AIOffset),Y
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

.endproc

.proc ProcessReaction_Party
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
Next:       jsr NextCharOffset
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
:       sta $16			;randomly 0, +1 or -1 ($FF)
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
:       ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$80		;old
        beq :+
        lda #$06		;old timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$20		;paralyze
        beq :+
        lda #$09		;paralyze timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$04		;mute
        beq :+
        lda #$04		;mute timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status3,X
        and #$80		;reflect
        beq :+
        lda #$02		;reflect timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status3,X
        and #$10		;stop
        beq :+
        lda #$00		;stop timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status3,X
        and #$01		;regen
        beq :+
        lda #$07		;regen timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status4,X
        and #$10		;countdown
        beq :+
        lda #$03		;countdown timer
        jsr StartTimerFromTemp
:       ldx AttackerOffset
        lda CharStruct::Status3,X	;**bug: should be Status4
        and #$08		;Haste, should be HP leak
        beq :+
        lda #$05		;HP leak timer
        jsr StartTimerFromTemp
:       inc Temp
        lda Temp
        cmp #$0C		;doing slots 4-11 for monsters, 12 is too far
        beq Ret
        jmp MonStatusLoop
Ret:	rts

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
				
:       sta InventoryFlags,X
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
		
:       sta CharSpells::Flags,X
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
:       sta CharCommands::Flags,X
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
:       sta HandItems::Flags,X
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

:       lda CharStruct::EquipWeapons,X
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
:       lda FieldItems,X
        sta InventoryItems,X
        inx
        cpx #$0200		;512, includes item quantities
        bne :-
				
:       lda InventoryItems,Y
        jsr SetupInventoryInfo	;sets InventoryFlags and Temp (equipment type)
        iny
        cpy #$0100
        bne :-
				
        tdc
        tay
        sty $08
        										;:					
:       ldy $08
        jsr GetItemUsableY	;A now holds byte for InventoryUsable
        ldy $08
        sta Temp,Y		;**optimize: bypass temp and just save to proper place
        inc $08
        lda $08
        bne :-
				
        tdc
        tax
        										;: 					
:       lda Temp,X
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
:       ldx $0E		;MagicBits Index
:       lda MagicBits,X
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
:       cmp #$01
        bne :+
        tdc 			;item 1 -> 0
:       sta HandItems::ID,Y
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
Ret:	rts

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
:       lda ROMItemEquippable,X
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
Next:       ldx $22
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
MSword:       ldx AttackerOffset
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
:       adc #$01			;
        cmp #$24			;36
        bne :+
        tdc 				;clear wonder rod if it got too high
:       sta BattleData::WonderRod
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
:       ldx $0E			;offset into character equipment
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
:       ldx $08			;command queue slot offset
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
:       adc #$01
        cmp #$24		;36
        bne :+
        tdc 			;clear wonder rod if it got too high
:       sta BattleData::WonderRod
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
Ret:	rts

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
:       ldx AttackerOffset
        jsr CopyStatsWithBonuses
        jsr WipeDisplayStructures
        jsr WipeActionData
        jsr DispatchCommand
        jsr ProcessCommands
        jsr CheckLearnBlue
        lda WasMonsterReleased
        beq :+
        jsr RestoreStatsAfterRelease
:       lda ActionAnimShift
        beq :+
        jsr ShiftActionAnims
:       lda #$0A	;C1 Routine: execute graphics script
        jsr CallC1
        lda EncounterInfo::IntroFX
        bpl :+
        lda AttackerIndex      ;this section for credits demo
        cmp #$04		;monster check
        bcs :+
        lda #$20
        sta BattleOver		;battle ends after one action
:       lda #$FF
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
:	tdc
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
Ret:	rts

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
:       lda CharStruct::CmdCancelled,X
        bne :+				;already cancelled player command
        lda #$01
        sta CharStruct::CmdCancelled,X
        stz CharStruct::Command,X
        lda #$80
        sta CharStruct::ActionFlag,X
:       lda CharStruct::Status2,X
        and #$40      			;Sleep
        beq :+
        stz CharStruct::Command,X
        lda #$80
        sta CharStruct::ActionFlag,X
:       cpx #$0200    			;AttackerOffset is a monster
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

.endproc

.proc DispatchCommand_CommandReady
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
:       stz HitsInactive,X
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
NoMon:       lda PartyTargets
        bne Party
        jmp Finish		;abort if no targets selected
Party:       jsr CountSetBits
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
:       ldx #$0004		;otherwise retarget to any monster slot
        lda #$0B
:       jsr Random_X_A
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
Finish:       plx 			;original AttackerOffset
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
:       lda $0F
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
:       lda $10
        and #$20	;learnable (never used)
        beq :+
        lda #$32       	;50%
        bra CheckLearn
:       lda $10
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
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc StartBattle
_4CE0:
        jsr InitBattle
        tdc
        tax
:       adc a:$0000,X		;Sum first 1000 bytes of ram
        inx
        cpx #$03E8
        bne :-
        										;					
        adc RNGSeed		;add RNG seed
        sta RNGA
        										;					
:       adc a:$0000,X		;Sum another 1000 bytes of ram
        inx
        cpx #$07D0
        bne :-
        										;					
        sta RNGB
        tdc
        tax
:       lda ROMGlobalTimer,X		;copies table of constants for status timers
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
:       lda ROMEncounterInfo,X
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
:       lda EncounterInfo::Flags
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
:       lda [$12],Y		;ROMMonsterStats
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
:       lda ROMMonsterCoordinates,X
        sta MonsterCoordinates,Y
        inx
        iny
        cpy #$0008
        bne :-
        										;					
        tdc
        tax
;override monster ID $FF to be active
:       lda EncounterInfo::MonsterID,X
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
:       jmp MainBattleLoop

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
Next:       inc $10
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
:       lda ROMEncounterInfo,X
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
:       sta $0E
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
:       sta $00,X	;Clears $00-67
        dex
        bpl :-
        ldx #$5CD7
        tdc
:       sta $2000,X	;Clears all of the ram used in battle, except ResetBattle
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
:       sta GFXQueue,X		;Init gfx queue to $FF, which is considered empty
        dex
        bpl :-
        tdc
        tax
:       lda FieldData,X	;copy data like Escape count and battle event flags
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
:       lda CharStruct::CharRow,X
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
:	tdc
        tax
        lda #$FF
        jsr Random_X_A
        cmp $10       	;preemptive chance
        bcs Finish
        jsr Preemptive
        						;:
Finish:       stz ResetBattle
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
:       sta CurrentTimer::ATB,Y
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
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc EndBattle

_5070:
        jsr WipeDisplayStructures
        ldx #$0007		;clear 8 bytes
:       stz FieldItemsWon,X
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
:       sta BattleData::Escapes
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
:       lda #$0D		;C1 routine
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
:       ora BattleItemsWon,X
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
Ret:	rts

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
:       sta FieldItemsQty,X
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
:	tdc
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
:       sta FieldItemsQty,X
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
:	tdc
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
:       sta MonsterKillCount
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
:       sta CharStruct::AP,X
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
:       lda MonsterKillTracker
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
:       lda VictoryExp
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
:       lda EncounterInfo::AP
        beq :+
        sta MessageBoxData2
        stz MessageBoxData2::_1
        stz MessageBoxData2::_2
        jsr CheckHideAP
        lda $0E				;result, 1 to hide AP
        bne :+
        lda #$2E			;ap message
        sta MessageBoxes,X
:       lda MessageBoxes
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
:       lda $0E
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
:       stz MessageBoxData+1	;clear high bytes of message box data
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
Ret:	rts

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
Next:       jsr NextCharOffset
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
Ret:	rts

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
:       sta $08
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
:       sta $08
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

.proc ApplyHPMPPassives

_57C7:
        phx
        phy
        sec
        sbc #$8B	;hp +10% command/passive
        tax
        lda f:HPMPMultTable,X
        tax
        stx $2A
        ldx $08		;new base hp
        stx $2C
        jsr Multiply_16bit	;base hp * 10, 20 or 30
        jsr ApplyPercentage
        ply
        plx
        rts

.endproc

; ---------------------------------------------------------------------------

.org $C257E1
.proc HPMPMultTable

_57E1:
.byte $0A,$14,$1E,$0A,$1E	;HP 10, 20, 30 then MP 10, 30

.endproc
.reloc

; ---------------------------------------------------------------------------

;Divides Exp for party members
;minimum 1 exp per char(this isn't called if total exp is 0)
.proc DivideExp

_57E6:
        ldx $10			;count of active chars
        phx
        ldx #$000F
:       stz $0E,X		;clear $0E-1D
        dex
        bpl :-
        plx
        stx $12			;count of active chars, divisor
        ldx VictoryExp
        stx $0E
        lda VictoryExp+2
        sta $10
;32 bit division routine
;Dividend: 	$0E-11
;Divisor: 	$12-15
;Quotient: 	$16-19
;Remainder: 	$1A-1C
;**optimize: make this a general purpose subroutine, duplicate at $04A9
        longa
        clc
        ldx #$0020
:       rol $0E
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
:       rol $16
        rol $18
        dex
        bne :--
        shorta0
;Division ends here
        lda $16			;quotient, exp to distribute
        ora $17
        ora $18
        bne :+
        inc $16			;min 1
:       ldx $16
        stx VictoryExp
        lda $18
        sta VictoryExp+2
        rts

.endproc

; ---------------------------------------------------------------------------

;puts a GFX command on the top of the queue to display a message
;then clear ActionAnim sturcture
;used for back and pre-emptive attacks, and attempting to flee a battle you can't run from
;00,FC,05,<A>,00
.proc GFXCmdMessageClearAnim

_5847:
        sta GFXQueue::Data1
        stz GFXQueue::Flag
        lda #$FC
        sta GFXQueue::Cmd
        lda #$05
        sta GFXQueue::Type
        stz GFXQueue::Data2
        lda #$20
        sta ActionAnim0::Flags
        stz ActionAnim0::OrigAttacker
        stz ActionAnim0::OrigTargetBits
        stz ActionAnim0::TargetBits
        stz ActionAnim0::ReflectorBits
        stz ActionAnim0::ReflecteeBits
        stz ActionAnim0::CoveredBits
        rts

.endproc

; ---------------------------------------------------------------------------

.org $5872
.proc MainBattleLoop

_5872:
        tdc
        tax
        									;:											
:       lda MenuDataC1,X	;Data from C1 bank graphics/menu routines
        sta MenuData,X
        inx
        cpx #$000E	;15 bytes copied
        bne :-
        									;;					
        lda ResetBattle
        bne _EndBattle
        inc BattleTickerA
        lda BattleTickerA
        cmp #$04
        bne :+
        stz BattleTickerA
        inc BattleTickerB	;only 1 when A is 0, 25% of the time
        bra :++
:       stz BattleTickerB
:       lda CurrentlyReacting
        bne SkipMonsterUpdate
        jsr CheckMonsterDeath
        jsr UpdateMonsterList
SkipMonsterUpdate:
        jsr CheckPartyDeath
        lda CurrentlyReacting
        beq :+
        lda ReactingIndex
        cmp #$04
        bcc :++
:       jsr HandleATBMenu
:       jsr CopyHPMPStatus
WaitLoop:								
        lda #$03	;c1 routine: wait one frame
        jsr CallC1
        jsr CheckBattleEnd
        lda BattleOver
        bne _EndBattle
        lda WaitModePause
        bne WaitLoop
        jsr UpdateMonsterRows
        lda CurrentlyReacting
        bne :+
        jsr ATBWait
        bne NextLoop
        jsr HandleUncontrolledParty
:       stz TurnProcessed
        jsr UpdateTimers
        jsr FindEndedTimers
        jsr ApplyTimerEffects
        jsr HandleReactions
NextLoop:								
        jsr Random_0_99		;consumes a random number
        jmp MainBattleLoop
_EndBattle:								
        lda #$03	;C1 routine: wait one frame
        jsr CallC1
        lda DisplayInfo::CurrentChar
        cmp #$FF	;no menu open
        beq MenuClosed
        sta MenuCurrentChar
        lda GearChanged
        beq :+
        jsr ReplaceHands
        jsr ApplyGear
:       lda #$01	;C1 routine: close menu
        jsr CallC1
MenuClosed:
        jsr EndBattle
        tdc
        tax
:       lda BattleItemsWon,X
        sta FieldItemsWon,X
        inx
        cpx #$0008
        bne :-
        rts

.endproc

; ---------------------------------------------------------------------------

;(Copy Current/Max HP/MP for players and merge status with always-status for players/monsters)
.org $5921
.proc CopyHPMPStatus

_5921:
        tdc
        tax
        tay
        stx $10
        longa
CopyAllVitals:
        stz $0E		;stat index
CopyVitals:		;copies current/max HP/MP									
        lda CharStruct::CurHP,X
        sta CharVitals::CurHP,Y
        inx
        inx
        iny
        iny
        inc $0E		;next stat
        lda $0E
        cmp #$0004	;4 stats
        bne CopyVitals
        									;.					
        clc
        txa
        adc #$0078	;next character offset
        tax
        cpy #$0020	;stop when 32 bytes have been copied (8 * 4 chars)
        bne CopyAllVitals
        									;.					
        shorta0
        lda EncounterInfo::IntroFX
        bpl :+			;80h: credits demo battle
        jmp MonsterStatus
        									;:
:	tdc
        tax
        tay
        stz $0E
CheckMergePartyStatus:
        phx
        phy
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$02	;zombie
        bne PartyStatus
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78	;sleep/para/charm/berserk
        bne PartyStatus
        lda CharStruct::Status4,X
        and #$04	;singing
        bne PartyStatus
        lda $0E
        tax
        jsr GetTimerOffset	;Y = Timer Offset
        lda EnableTimer::ATB,Y
        and #$40
        bne PartyStatus
        lda CurrentTimer::ATB,Y
        sta ATB,X
PartyStatus:														
        ply
        plx
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        sta CombinedStatus::S1,Y
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta $10
        and #$08	;berserk
        beq DoneBerserk
        lda CharStruct::Job,X
        cmp #$06	;berserker
        bne DoneBerserk
        lda $10
        and #$F7	;clear berserk bit
        sta $10
DoneBerserk:								
        lda CharStruct::Status4,X
        ora CharStruct::AlwaysStatus4,X
        sta CombinedStatus::S4,Y
        and #$80	;erased
        beq NotErased
        sta CombinedStatus::S4,Y
        tdc
        sta CombinedStatus::S1,Y
        sta CombinedStatus::S2,Y
        sta CombinedStatus::S3,Y
        bra NextParty
NotErased:														
        lda $10		;combined status2 without berserk
        sta CombinedStatus::S2,Y
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        sta CombinedStatus::S3,Y
NextParty:									;:
        inc $0E
        iny
        iny
        iny
        iny
        jsr NextCharOffset
        cpy #$0010	;4 * 4 chars
        beq MonsterStatus
        jmp CheckMergePartyStatus
MonsterStatus:
        ldy #$0004
        sty $0E
        tdc
        tay
MonsterStatusLoop:
        phy
        ldy $0E
        lda ActiveParticipants,Y
        bne ActiveMonster
        ply
        lda MonsterCombinedStatus::S1,Y	;8 is first monster entry
        and #$30	;keep toad/mini
        ora #$80	;set dead
        sta MonsterCombinedStatus::S1,Y
        tdc
        sta MonsterCombinedStatus::S2,Y
        sta MonsterCombinedStatus::S3,Y
        sta MonsterCombinedStatus::S4,Y
        bra NextMonster
ActiveMonster:														
        ply
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        sta MonsterCombinedStatus::S1,Y
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        sta MonsterCombinedStatus::S2,Y
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        sta MonsterCombinedStatus::S3,Y
        lda CharStruct::Status4,X
        ora CharStruct::AlwaysStatus4,X
        sta MonsterCombinedStatus::S4,Y
NextMonster:									;:
        inc $0E
        iny
        iny
        iny
        iny
        jsr NextCharOffset
        cpy #$0020	;4 * 8 monsters
        bne MonsterStatusLoop
        rts

.endproc


; ---------------------------------------------------------------------------

;Setup displayed monster slots (4 types and quantities)
.org $5A41
.proc UpdateMonsterList

_5A41:
        tdc
        tax
        tay
        stx MonsterSlots::ID
        stx MonsterSlots::Count
        stx MonsterSlots::ID + 4
        stx MonsterSlots::Count + 4
        stx MonsterSlots::ID + 8
        stx MonsterSlots::Count + 8
        stx MonsterSlots::ID + 12
        stx MonsterSlots::Count + 12
        stz MonstersVisible
        stx $10		;monster index
        dey 		;$FFFF, or -1
        sty $0E		;$FF, nothing or not displayed
        dey
        dey
        dey 		;FFFC, or -4	(so when it adds 4 we start at 0)
AddMonster:										
        ldx $10		;monster index
        lda ActiveParticipants+4,X	;monster part of table
        beq Next
        lda MonstersVisible
        jsr SetBit_X
        sta MonstersVisible
        txa
        clc
        adc #$04	;shift to char index
        jsr CalculateCharOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$30	;toad / mini
        bne Next
        lda $10		;monster index
        asl
        tax
        lda MonsterNameID,X
        cmp $0E		;previous monster name ID
        beq AddCount
        iny
        iny
        iny
        iny 		;next MonsterSlots entry
        sta MonsterSlots::ID,Y
        sta $0E		;saved monster name ID
        lda BattleMonsterID+1		;copy boss byte
        sta MonsterSlots::ID+1,Y
AddCount:
        clc
        lda MonsterSlots::Count,Y
        adc #$01
        sta MonsterSlots::Count,Y
Next:								
        inc $10		;next monster index
        lda $10
        cmp #$08	;8 monsters
        bne AddMonster
        rts

.endproc

; ---------------------------------------------------------------------------

;checks if the battle ended via timer, death, victory, or escape
.org $5AB4
.proc CheckBattleEnd

_5AB4:
        tdc
        tax
        tay
        lda BattleOver  	;check if it's already ending
        beq CheckTimer
        rts
CheckTimer:								
        lda BattleTimerEnable
        cmp #$02
        bne CheckParty
        lda BattleTimer
        ora BattleTimer+1
        bne CheckParty
        lda #$20		;end via timer
        sta BattleOver
        rts
CheckParty:									
        lda ActiveParticipants,Y
        beq NextParty
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$02	;zombies are active but don't count
        beq CheckHideFlee
NextParty:
        jsr NextCharOffset
        iny
        cpy #$0004
        bne CheckParty
        lda #$40	;end via party ko
        sta BattleOver
        rts
CheckHideFlee:		;X and Y start where they left off from the party checks
        lda ActiveParticipants,Y
        beq NextHide
        lda CharStruct::Status4,X
        and #$01	;hidden
        beq CheckMonsters
NextHide:
        jsr NextCharOffset
        iny
        cpy #$0004
        bne CheckHideFlee
        lda EncounterInfo::FleeChance
        bpl _FleeSuccess  ;80h = can't run
CheckMonsters:
        ldy #$0004 	;first monster index
        ldx #$0200 	;first monster offset
MonsterLoop:
        lda ActiveParticipants,Y
        beq NextMonster
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$02	;zombie monsters don't exist, but check anyway
        beq CheckFlee
NextMonster:
        jsr NextCharOffset
        iny
        cpy #$000C	;12 slots
        bne MonsterLoop
        lda #$80	;victory
        sta BattleOver
        rts
CheckFlee:									
        lda FleeTickerActive
        beq ResetFleeTicker
        lda FleeSuccess
        bmi _FleeSuccess	;80h: exit cast
        beq ResetFleeTicker
        lda EncounterInfo::FleeChance
        bpl AdvanceTicker
        jsr WipeDisplayStructures
        tdc
        jsr GFXCmdMessageClearAnim
        lda #$20   	;can't run message
        sta MessageBoxes
        lda #$0A	;C1 routine: execute graphics script
        jmp CallC1
AdvanceTicker:
        inc FleeTicker
        lda FleeTicker
        cmp #$14	;20 ticks before flee attempt
        bne Ret
        jsr Random_0_99
        cmp EncounterInfo::FleeChance
        bcs ResetFleeTicker
_FleeSuccess:
        lda #$01	;escaped
        sta BattleOver
        rts
ResetFleeTicker:
        stz FleeTicker
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc UpdateMonsterRows

_5B6C:
        lda #$0B	;C1 routine
        jsr CallC1
        tdc
        tax
        ldy #$0200	;first monster offset
Loop:
        lda CharStruct::CharRow,Y
        and #$7F	;clear row bit (front row)
        sta CharStruct::CharRow,Y
        lda C1Temp	;seems to have bits set for front row monsters
        jsr SelectBit_X
        bne Next
        lda CharStruct::CharRow,Y
        ora #$80	;set row bit (back row)
        sta CharStruct::CharRow,Y
Next:
        longa
        tya
        clc
        adc #$0080	;next monster offset
        tay
        shorta0
        inx
        cpx #$0008	;8 monsters
        bne Loop
        rts
;Command $27 (Dummy02)
;Command $28 (Sing)
;Command $2C-$4D (Magic)
MagicCommand:
CommandTable26:
CommandTable27:
CommandTable2B:		
        lda AttackerIndex
        cmp #$04	;monster check
        bcc Party
        jmp Monster
Party:       stz TempAttachedSpell
        stz TempSkipNaming
        ldx AttackerOffset
        lda CharStruct::MonsterTargets,X
        sta MonsterTargets
        lda CharStruct::PartyTargets,X
        sta PartyTargets
        lda CharStruct::SelectedItem,X
        sta TempSpell
        lda CharStruct::ActionFlag,X
        and #$08	;X-Magic
        beq CastSpell1
        lda PartyTargets
        beq CastSpell1
        lda CharStruct::PartyTargets,X
        cmp CharStruct::SecondPartyTargets,X
        bne CastSpell1	;if x-magic targetting the same party member
        lda TempSpell	;check spell
        cmp #$19	;Life
        beq CheckDeath
        cmp #$21	;Life2
        beq CheckDeath
        cmp #$54	;Phoenix
        beq CheckDeath
        cmp #$1D	;Heal/Esuna
        bne CastSpell1
Stone:       inc SpellCheckStone	;set variables to confirm they still
        bra CastSpell1		;have the relevant status
CheckDeath:	
        inc SpellCheckDeath
CastSpell1:
        stz TempIsEffect
        jsr CastSpell
        lda TempAttachedSpell	;attached spell for phoenix
        beq CastSpell2
        lda TempAttachedSpell
        sta TempSpell
        stz TempIsEffect
        lda TempMonsterTargets
        sta MonsterTargets
        lda TempPartyTargets
        sta PartyTargets
        inc TempSkipNaming
        jsr CastSpell
CastSpell2:
        ldx AttackerOffset
        lda CharStruct::ActionFlag,X
        and #$08	;X-Magic
        beq Finish
        stz TempAttachedSpell
        stz TempSkipNaming
        lda CharStruct::SecondMonsterTargets,X
        sta MonsterTargets
        lda CharStruct::SecondPartyTargets,X
        sta PartyTargets
        lda CharStruct::SecondSelectedItem,X
        sta TempSpell
        stz TempIsEffect
        jsr CastSpell
        lda TempAttachedSpell	;attached spell for phoenix fire
        beq Finish
        lda TempAttachedSpell
        sta TempSpell
        stz TempIsEffect
        lda TempMonsterTargets
        sta MonsterTargets
        lda TempPartyTargets
        sta PartyTargets
        inc TempSkipNaming
        jsr CastSpell
Finish:       stz SpellCheckDeath
        stz SpellCheckStone
        jmp Ret
Monster:
        sec
        lda AttackerIndex
        sbc #$04
        asl
        tax 		;monster index
        lda ROMTimes100w,X
        sta $0E		;monster index *100
        lda ROMTimes100w+1,X
        sta $0F
        tdc
        tay
        ldx $0E		;monster index *100
:       lda MonsterAIScript,X
        sta GFXQueue,Y
        inx
        iny
        cpy #$0064	;copy 100 bytes of commands
        bne :-
        inc SearchGFXQueue
        stz $1E
        sec
        lda AttackerIndex
        sbc #$04	;monster index
        jsr ShiftMultiply_16
        tax
        stx $0E		;monster index *16
        longa
        clc
        adc #MonsterMagic
        tax
        stx MMOffset		;MonsterMagic offset
        lda $0E
        asl 		;monster index *32
        clc
        adc #MMTargets
        tax
        stx MMTargetOffset	;MonsterMagic Targets offset
        shorta0
CastNextMonsterSpell:
        lda $1E
        tay
        lda (MMOffset),Y	;MonsterMagic
        cmp #$FF
        beq Ret
        sta TempSpell
        cmp #$E9	;banish
        bne :+
        inc UnknownReaction
:       lda TempSpell
        cmp #$A8	;interceptor rocket
        bne :+
        inc HitsJumping
:       lda TempSpell
        cmp #$83	;roulette
        bne :+
        inc MissInactive
:       stz TempAttachedSpell
        stz TempSkipNaming
        lda $1E
        asl
        tay
        lda (MMTargetOffset),Y	;MonsterMagic Party Targets
        sta PartyTargets
        iny
        lda (MMTargetOffset),Y	;MonsterMagic Monster Targets
        sta MonsterTargets
        stz TempIsEffect
        jsr CastSpell
        inc $1E
        lda $1E
        cmp #$10	;can cast up to 16 spells
        bne CastNextMonsterSpell
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;does everything needed to cast a spell or use a magic-like ability
;
;loads spell data into attack info structures and other variables
;fixes up targetting as needed
;displays needed messages and animations
.org $5CE1
.proc CastSpell

_5CE1:
        lda TempIsEffect
        bne LoadEffect
        lda TempSpell
        cmp #$F1	;Spell $F1 pulls data for $78 instead
        bne :+
        lda #$78
:       rep #$20
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:       lda ROMMagicInfo,X
        sta TempMagicInfo,Y
        inx
        iny
        cpy #$0008	;8 bytes in magic info struct
        bne :-
        bra DataLoaded
LoadEffect:
        lda TempSpell
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:       lda ROMEffectInfo,X
        sta TempMagicInfo,Y
        inx
        iny
        cpy #$0008	;8 bytes in magic info struct
        bne :-
DataLoaded:
        lda TempIsEffect
        bne CopyMagicInfo
        lda TempSpell
        cmp #$48	;first summon spell
        bcc CopyMagicInfo
        cmp #$57	;after last summon spell
        bcs CopyMagicInfo
        jsr PrepSummon
CopyMagicInfo:
        jsr SelectCurrentProcSequence	;Y = ProcSequence*12
        tdc
        tax
:       lda TempMagicInfo,X
        sta AttackInfo,Y
        inx
        iny
        cpx #$0005	;copy first 5 bytes to start of struct
        bne :-
        iny 		;skip 4 bytes in destination
        iny
        iny
        iny
:       lda TempMagicInfo,X
        sta AttackInfo,Y
        inx
        iny
        cpx #$0008
        bne :-		;copy remaining 3 bytes to end of struct
        lda TempMagicInfo::Misc
        and #$07	;number of hits -1
        beq SingleHit
        jsr CastMultiHitSpell
        rts
SingleHit:
        jsr CheckMultiTarget
        bne _MultiTarget
        lda TempMagicInfo::AtkType
        bpl CheckSpecialVars
_HitsInactive:
        lda ProcSequence
        tax
        lda #$01
        sta HitsInactive,X
        bra _BuildTargetBitmask
CheckSpecialVars:
        lda SpellCheckDeath
        bne CheckDeath
        lda SpellCheckStone
        beq CheckTarget
CheckStone:
        jsr GetPartyTargetOffset
        lda CharStruct::Status1,X
        bmi CheckTarget	;if dead
        and #$40
        bne _HitsInactive	;if stone
        bra CheckTarget
CheckDeath:
        jsr GetPartyTargetOffset
        lda CharStruct::Status1,X
        bmi _HitsInactive
CheckTarget:
        jsr CheckRetarget
        lda NoValidTargets
        beq _BuildTargetBitmask
NoTargets:
        lda #$F1
        sta TempSpell
        bra _BuildTargetBitmask
_MultiTarget:
        lda TempMagicInfo::AtkType
        bpl RemoveInactive
        lda ProcSequence
        tax
        lda #$01
        sta HitsInactive,X
        bra _BuildTargetBitmask
RemoveInactive:
        jsr RemoveInactiveTargets
        lda NoValidTargets
        bne NoTargets
        jsr CheckMultiTarget
_BuildTargetBitmask:
        jsr BuildTargetBitmask
        lda TempIsEffect
        bne CheckMagicAnimTable
        lda TempSpell
        cmp #$80	;monster fight
        beq Fight
        cmp #$DE	;strong fight?	vacuum wave?
        bne NotFight
Fight:       jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01	;ability/command anim
        sta GFXQueue::Type,X
        lda #$04
        sta GFXQueue::Data1,X
        tdc
        sta GFXQueue::Data2,X
        jmp TargettingStatus
NotFight:
        lda TempSpell
        cmp #$F1
        bne CheckMagicAnimTable
        jmp TargettingStatus
CheckMagicAnimTable:
        lda TempSkipNaming
        bne MagicAnim
        lda TempIsEffect
        bne CheckType
        lda TempSpell
        cmp #$82	;first blue spell
        bcc CheckType
        stz $0E
        sec
        sbc #$80	;high bit always 0 now
        lsr
        ror $0E
        lsr
        ror $0E
        lsr
        ror $0E
        tax 		;high 5 bits of spell, determines byte 0-15
        lda ROMMagicAnim,X
        pha
        lda $0E
        jsr ShiftDivide_32
        tax 		;low 3 bits of spell, determines which bit
        pla
        jsr SelectBit_X
        beq CheckType
MagicAnim:	;rom has this spell flagged for animation type 7 (most magic spells)
        lda TempIsEffect
        bne CheckType
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Data2,X
        lda #$FC	; exec graphics command
        sta GFXQueue::Cmd,X
        lda #$07	; magic animation
        sta GFXQueue::Type,X
        lda TempSpell
        sta GFXQueue::Data1,X
        bra TargettingStatus
CheckType:
        lda #$00
        sta $0E
        lda TempSpell
        sta Temp+1
        lda TempIsEffect
        bne AttackOrEffect
        lda TempSpell
        cmp #$81	;monster specialty
        bne AttackOrEffect
Specialty:
        lda #$03
        sta Temp
        ldx AttackerOffset
        lda CharStruct::SpecialtyName,X
        sta Temp+1
        jsr GFXCmdAttackNameFromTemp
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$01	;ability/command anim
        sta GFXQueue::Type,X
        lda #$04
        sta GFXQueue::Data1,X
        tdc
        sta GFXQueue::Data2,X
        bra TargettingStatus
AttackOrEffect:
        lda TempIsEffect
        beq :+
        lda #$02
        sta $0E
        lda #$02
:       sta Temp
        lda TempSkipNaming
        bne :+		;skips naming the attack a second time
        jsr GFXCmdAttackNameFromTemp
:       jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Data2,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda $0E		;attack or special ability effect, depending
        sta GFXQueue::Type,X
        lda Temp+1	;spell id
        sta GFXQueue::Data1,X
TargettingStatus:
        lda ProcSequence
        tax
        lda TempMagicInfo::AtkType
        and #$7F
        sta AtkType,X
        lda TempTargetting
        sta MultiTarget,X
        beq :+
        inc MultiTarget,X
        lda #$80
:       sta TargetType,X
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
        lda TempIsEffect
        bne DisplayDamage
        lda TempSpell
        cmp #$F1
        beq IsToadOK
        ldx AttackerOffset
        cmp #$82	;first blue magic
        bcs CheckToad
        cmp #$80	;monster fight or specialty
        bcs DisplayDamage
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$04	;mute
        bne Ineffective
        lda Void
        and #$40	;void
        bne Ineffective
CheckToad:
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$20	;toad
        beq DisplayDamage
IsToadOK:	;spell is F1, or (spell is >$82 AND we're a frog), or (spell is <$80, and not muted, and we're a frog)
        lda TempSpell
        stz $0E
        lsr
        ror $0E
        lsr
        ror $0E
        lsr
        ror $0E
        tax 		;magic byte offset 0-31
        lda $0E
        jsr ShiftDivide_32
        tay 		;magic bit
        phy
        lda ROMToadOK,X
        plx
        jsr SelectBit_X
        bne DisplayDamage
Ineffective:
        lda MessageBoxOffset
        tax
        lda #$1D        ;ineffective
        sta MessageBoxes,X
        lda ProcSequence
        dec 	;Procsequence already advanced, rolling back
        tax
        lda #$7E	;always miss attack type
        sta AtkType,X
        bra Finish
DisplayDamage:
        jsr GFXCmdDamageNumbers
        lda MessageBoxOffset
        tay
        longa
        lda TempSpell
        tax
        shorta0
        lda ROMBattleMessageOffsets,X
        sta MessageBoxes,Y
Finish:       jsr GFXCmdMessage
        rts

.endproc

; ---------------------------------------------------------------------------

;Casts multi-hit spells like Meteo.  Code suggests there's at least one like that in the effect magic
.proc CastMultiHitSpell

_5F75:
        sta TempNumHits	;number of hits	(-1?)
        lda TempIsEffect
        beq NormalMagic
        lda #$02	;Effect magic animation
        sta $0E
        sta Temp
        bra :+
NormalMagic:
        lda #$00	;normal magic animation
        sta $0E
        sta Temp
:
        lda TempSpell
        sta Temp+1	;spell to cast
        jsr GFXCmdAttackNameFromTemp
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        stz GFXQueue::Data2,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda $0E		;attack or special animation
        sta GFXQueue::Type,X
        lda Temp+1	;spell to cast
        sta GFXQueue::Data1,X
        lda TempNumHits
        sta $0E		;attacks remaining
CopyAttacks:		;one copy into AttackInfo table for each TempNumHits (which is actual hits -1)
        		;first copy was already made before this was called (and Y is kept from that)
        tdc
        tax
:       lda TempMagicInfo,X
        sta AttackInfo,Y
        inx
        iny
        cpx #$0005	;first 5 bytes to start of struct
        bne :-
        iny 		;advance 4 bytes in destination
        iny
        iny
        iny
:       lda TempMagicInfo,X
        sta AttackInfo,Y
        inx
        iny
        cpx #$0008	;remaining 3 bytes to end of struct
        bne :-
        dec $0E		;attacks remaining
        lda $0E
        bne CopyAttacks
        stz $1A		;flag for not enough mp
        lda AttackInfo::MPCost
        and #$7F
        tax
        stx $0E		;mp cost
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$08	;half mp
        beq :+
        lsr $0E		;halve
        bcc :+
        inc $0E		;round up
:       rep #$20
        sec
        lda CharStruct::CurMP,X
        sbc $0E		;mp cost
        bcc NoMP
        sta CharStruct::CurMP,X
        shorta0
        bra ProcessHits
NoMP:	tdc
        shorta
        inc $1A		;not enough mp
ProcessHits:		;sets up targetting and attack type for TempNumHits + 1 attacks
        lda MonsterTargets
        beq Party
        tdc
        tax
        lda #$07
        jsr Random_X_A	;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
        bra TargetSet
Party:
        tdc
        tax
        lda #$03
        jsr Random_X_A   ;0..3 random party member
        tax
        tdc
        jsr SetBit_X
        sta PartyTargets
TargetSet:
        jsr CheckRetarget
        lda NoValidTargets
        beq ValidTarget
        lda #$7E	;always miss attack type
        sta TempMagicInfo::AtkType
ValidTarget:
        jsr BuildTargetBitmask
        ldx AttackerOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$04	;mute
        bne Fail
        lda Void
        and #$40	;void
        bne Fail
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$20	;toad
        beq :+
Fail:       lda #$7E	;always miss attack type
        sta TempMagicInfo::AtkType
:       lda ProcSequence
        tax
        lda TempMagicInfo::AtkType
        and #$7F
        sta AtkType,X
        lda $1A		;not enough mp
        beq :+
        lda #$7E	;always miss attack type
        sta AtkType,X
:       stz MultiTarget,X
        stz TargetType,X
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
        lda TempIsEffect
        beq :+
        jsr GFXCmdDamageNumbers	;only need this for effect spells?
:       dec TempNumHits
        lda TempNumHits
        bmi Ret
        jmp ProcessHits
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Loads correct spell info for summon magic
.proc PrepSummon

_60A3:
        clc
        lda TempSpell
        adc #$17	;shifts id to the actual spell effect
        sta TempSpell	;Shiva -> Diamond Dust, for example
        cmp #$6B	;Phoenix (fire portion)
        beq Phoenix
        cmp #$6A	;Odin
        beq Odin
        cmp #$5F	;Chocobo
        beq Chocobo
        jmp Finish
Chocobo:
        jsr Random_0_99
        cmp #$08	;8% fat
        bcs Finish
        lda #$6E	;fat chocobo
        sta TempSpell
        lda #$FF	;all targets
        sta MonsterTargets
        bra Finish
Phoenix:
        lda MonsterTargets
        sta TempMonsterTargets
        lda PartyTargets
        sta TempPartyTargets
        lda #$FF	;all targets
        sta MonsterTargets
        stz PartyTargets
        lda #$70	;phoenix (life portion)
        sta TempAttachedSpell
        ldx AttackerOffset
        bra Finish
Odin:
        tdc
        tay
        tax
CheckMonsters:
        lda ActiveParticipants+4,Y
        beq NextMonster
        lda CharStruct4::CreatureType,X
        and #$20	;heavy
        bne Gungnir
NextMonster:
        jsr NextCharOffset
        iny
        cpy #$0008	;8 monsters
        bne CheckMonsters
        clc
        lda Level
        adc #$50	;level+80
        sec
        sbc CharStruct4::Level		;first monster's level
        bcs :+
        lda #$01	;min 1
:       cmp #$63
        bcc :+
        lda #$63	;max 99
:       sta $0E
        jsr Random_0_99
        cmp $0E
        bcs Gungnir
        lda #$FF	;all targets
        sta MonsterTargets
        bra Finish
Gungnir:
        lda #$6F	;gungnir
        sta TempSpell
        tdc
        tax
        lda #$07
        jsr Random_X_A  ;0..7 random monster
        tax
        tdc
        jsr SetBit_X
        sta MonsterTargets
Finish:
        lda TempSpell
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        tdc
        tay
:       lda ROMMagicInfo,X
        sta TempMagicInfo,Y
        inx
        iny
        cpy #$0008	;8 bytes magic data
        bne :-
        rts

.endproc

; ---------------------------------------------------------------------------

;Gets the CharStruct offset of the first party member target in PartyTargets ($66)
;Will infinite loop if there are no targets
.proc GetPartyTargetOffset

_614E:
        tdc
        tax
        lda PartyTargets
:	ASL
        bcs :+
        inx
        bra :-
:	TXA
        longa
        jsr ShiftMultiply_128
        tax
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Haste or Slow Modifier
;(A=A/2 if Haste, A=A*2 if Slow, Min 1, Max 255)
;**optimize: only load status once, only check min 1 at end
.org $6163
.proc HasteSlowMod

_6163:
        pha
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        and #$08   	;haste
        beq CheckSlow
        pla
        lsr        	;half duratiion
        bne :+
        inc        	;min 1
:	PHA
CheckSlow:
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        and #$04   	;slow
        beq :++
        pla
        asl        	;double duration
        bcc :+
        lda #$FF   	;max 255
:	PHA
:	PLA
        bne Ret
        inc 		;min 1, again
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Properly kills the target indexed in A	
;clears status, makes sure hp is 0 unless erased, handles monster control
;Params: A: char index 0-11
.proc KillCharacter

_618A:
        pha
        pha
        stz MonsterDead
        tax
        lda ActiveParticipants,X
        bne Active
        rts
Active:								;
        stz ActiveParticipants,X
        pla
        jsr ResetATB
        tdc
        sta EnableTimer::Stop,Y
        sta EnableTimer::Poison,Y
        sta EnableTimer::Reflect,Y
        sta EnableTimer::Countdown,Y
        sta EnableTimer::Mute,Y
        sta EnableTimer::HPLeak,Y
        sta EnableTimer::Old,Y
        sta EnableTimer::Regen,Y
        sta EnableTimer::Sing,Y
        sta EnableTimer::Paralyze,Y
        ldx AttackerOffset
        stz CharStruct::Status2,X
        stz CharStruct::Status3,X
        stz CharStruct::CmdStatus,X
        stz CharStruct::DamageMod,X
        lda CharStruct::Status4,X
        and #$FB	;clear singing
        sta CharStruct::Status4,X
        lda CharStruct::Status4,X
        and #$80	;erased
        bne :+
        stz CharStruct::CurHP,X
        stz CharStruct::CurHP+1,X
:	PLA
        pha
        cmp #$04	;monster check
        bcs Monster
        tay
        lda ControlTarget,Y
        pha
        tdc
        sta ControlTarget,Y
        sta ControlCommand,Y
        pla
        longa
        jsr ShiftMultiply_128	;offset of control target
        tay
        shorta0
        lda CharStruct::Status4,Y
        and #$DF		;clear controlled
        sta CharStruct::Status4,Y
        lda CharStruct::Status4,X
        and #$80		;erased
        beq Dead
        lda CharStruct::Status1,X
        and #$F3		;clear zombie/poison
        sta CharStruct::Status1,X
        bra Erased
Dead:	
        lda CharStruct::Status1,X
        and #$F3		;clear zombie/poison
        ora #$80		;set dead
        sta CharStruct::Status1,X
Erased:	
        lda CharStruct::Status4,X
        and #$80		;clear all but erased
        sta CharStruct::Status4,X
        pla
        rts
Monster:								;
        lda CharStruct::Status1,X
        and #$30		;keep toad/mini
        ora #$80		;set dead
        sta CharStruct::Status1,X
        stz CharStruct::Status4,X
        pla
        pha
        sec
        sbc #$04
        tax 			;monster index
        tdc
        jsr SetBit_X
        sta MonsterDead
        pla
        sta $0E
        tdc
        tax
FindController:		;monster is dead, clear anyone who was controlling them
        lda ControlTarget,X
        cmp $0E
        bne Next
        stz ControlTarget,X
        rts
Next:
        inx
        cpx #$0004
        bne FindController
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CheckPartyDeath

_6257:
        stz TempStartIndex
        lda #$04
        sta TempStopIndex
        stz TempIsMonster
        jmp CheckForDeath

.endproc

; ---------------------------------------------------------------------------

.proc CheckMonsterDeath

_6265:
        lda #$04		;first monster slot
        sta TempStartIndex
        lda #$0C		;12 total slots
        sta TempStopIndex
        sta TempIsMonster
        ;continues into next routine

.endproc

; ---------------------------------------------------------------------------

.proc CheckForDeath	;called here by another routine, for party instead of monsters

_6272:
        lda TempStartIndex
        tax
        stx $0E
        jsr CalculateCharOffset
Loop:
        lda QuickTurns
        beq CheckActive
        lda $0E
        cmp QuickCharIndex
        bne CheckActive
        lda CharStruct::Status1,X
        and #$C2	;dead/stone/zombie
        bne CancelQuick
        lda CharStruct::Status2,X
        and #$78	;sleep/paralyze/charm/berserk
        bne CancelQuick
        lda CharStruct::Status3,X
        and #$10	;stop
        bne CancelQuick
        lda CharStruct::Status4,X
        and #$84	;erased/singing
        bne CancelQuick
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        bne CheckActive
CancelQuick:									
        stz QuickTurns
        jsr ClearQuick
CheckActive:
        ldy $0E
        lda ActiveParticipants,Y
        bne :+
        jmp NextChar
:       lda TempIsMonster
        bne CheckHPLeak
CheckControl:		
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$C2	;dead/stone/zombie
        bne CancelControl
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78	;sleep/paralyze/charm/berserk
        bne CancelControl
        lda CharStruct::Status3,X
        and #$10	;stop
        bne CancelControl
        lda CharStruct::Status4,X
        and #$80	;erased
        beq CheckHPLeak
CancelControl:
        ldy $0E
        lda ControlTarget,Y
        pha
        tdc
        sta ControlTarget,Y
        sta ControlCommand,Y
        pla
        longa
        jsr ShiftMultiply_128
        tay
        shorta0
        lda CharStruct::Status4,Y	;of control target
        and #$DF	;clear controlled
        sta CharStruct::Status4,Y
CheckHPLeak:
        lda CharStruct::Status4,X
        ora CharStruct::AlwaysStatus4,X
        and #$08	;hp leak
        beq DoneLeak
        lda BattleTickerB	;1 every 4 ticks
        beq DoneLeak
        lda CharStruct::CmdStatus,X
        and #$10		;jumping
        bne DoneLeak
        longa
        lda CharStruct::CurHP,X
        beq :+
        dec
        sta CharStruct::CurHP,X
:	tdc
        shorta
        lda CharStruct::Status4,X
        and #$01		;hiding
        beq DoneLeak
        lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        bne DoneLeak
        inc CharStruct::CurHP,X	;**bug: this is only 8 bit math
DoneLeak:
        lda EncounterIndex
        cmp #$F7
        bne :+
        lda EncounterIndex+1
        cmp #$01
        beq CheckCriticalHP   	;encounter $1F7 is Galuf vs Exdeath
:       lda CharStruct::CurHP,X
        ora CharStruct::CurHP+1,X
        bne CheckCriticalHP
        jmp DeadZero
CheckCriticalHP:
        longa
        lda CharStruct::MaxHP,X
        jsr ShiftDivide_8
        cmp CharStruct::CurHP,X
        bcc CheckHP
        shorta0
        lda CharStruct::Status4,X
        ora #$02		;critical hp
        sta CharStruct::Status4,X
        bra CheckDeadStatus
CheckHP:
        lda CharStruct::CurHP,X
        cmp CharStruct::MaxHP,X
        bcc :+
        lda CharStruct::MaxHP,X
        sta CharStruct::CurHP,X	;cap at max hp
:	tdc
        shorta
        lda CharStruct::Status4,X
        and #$FD		;clear critical hp status
        sta CharStruct::Status4,X
CheckDeadStatus:	
        lda CharStruct::Status1,X
        and #$80		;dead
        bne Dead
        lda CharStruct::Status1,X
        and #$40		;stone
        bne StoneErased
        lda CharStruct::Status4,X
        bpl NextChar		;continue if erased
        lda SandwormBattle	;another sandworm erased special case
        bne NextChar
        phx
        lda $0E			;current char index
        jsr KillCharacter
        plx
StoneErased:
        ldy $0E
        tdc
        sta ActiveParticipants,Y
        lda CharStruct::Status1,X
        and #$74		;keep stone/toad/mini/poison
        sta CharStruct::Status1,X
        lda CharStruct::Status2,X
        and #$A4		;keep old/paralyze/mute
        sta CharStruct::Status2,X
        lda CharStruct::Status3,X
        and #$91		;keep reflect/stop/regen
        sta CharStruct::Status3,X
        lda CharStruct::Status4,X
        and #$9B		;clear false image/control/sing
        sta CharStruct::Status4,X
        bra NextChar
Dead:	
        tdc
        sta CharStruct::CurHP,X
        sta CharStruct::CurHP+1,X
DeadZero:
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$02		;Zombie
        bne NextChar
        phx
        lda $0E
        cmp #$04		;monster check
        bcc Party
        sec
        sbc #$04
        tax 			;monster index
        lda MonsterKillTracker
        jsr ClearBit_X
        ora InactiveMonsters
        sta MonsterKillTracker
Party:
        lda $0E
        jsr KillCharacter
        plx
NextChar:
        jsr NextCharOffset
        inc $0E
        lda $0E			;char index
        cmp TempStopIndex
        beq Ret
        jmp Loop
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc SetupAndLaunchAttack

_6408:
        jsr ClearAttackData
        jsr CopyAttackParams
        ldx AttackerOffset
        jsr CopyStatsWithBonuses
        stz SandwormHitFlag
        lda SandwormBattle
        beq Normal
        ;this section only applies to sandworm fight
        lda wTargetIndex
        cmp #$07		;first possible worm position
        bcc Normal
        cmp #$0A		;past last worm position
        bcs Normal
        sta SandwormHitIndex
        inc SandwormHitFlag
        lda #$0A		;apply attack to hardcoded "real" sandworm spot
        sta wTargetIndex
        ldx #$0500
        stx TargetOffset
Normal:
        lda wTargetIndex
        tax
        lda ActiveParticipants,X
        beq TargetNotActive
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$C0		;stone or dead
        bne TargetNotActive
        lda CharStruct::Status4,X
        and #$01		;hidden
        bne TargetNotActive
        lda CharStruct::CmdStatus,X
        and #$10		;jumping
        bne TargetNotActive
        lda SandwormBattle		;why does sandworm have a special case
        bne Continue			;for erased?
        lda CharStruct::Status4,X
        and #$80		;erased
        bne Miss		;
        bra Continue
TargetNotActive:
        ldx TargetOffset
        lda CharStruct::Status4,X
        and #$80		;erased
        bne Miss
        lda CharStruct::CmdStatus,X
        and #$10		;jumping
        beq :+
        lda HitsJumping
        bne Continue
        bra Miss
:
        lda MissInactive
        bne Miss
        lda MultiCommand
        tax
        lda HitsInactive,X
        bne Continue
        lda AtkType,X
        and #$7F
        cmp #$60		;hide/show monsters
        beq Continue
Miss:
        stz MissInactive
        inc AtkMissed
        bra GoApplyDamage
Continue:
        stz HitsJumping
        ldx TargetOffset
        lda CharStruct::CharRow,X
        and #$40		;not on the team?
        bne Miss
        lda FleeSuccess
        bmi Miss		;80h means Exit	was cast
        lda ResetBattle
        bne Miss		;Reset was cast
        jsr DispatchAttack
GoApplyDamage:
        jmp ApplyDamage

.endproc

; ---------------------------------------------------------------------------

;**cleanup: 	this is the routine that gets patched for the atk type bank relocation stuff
;		need to handle that if this code is merged with that
.proc DispatchAttack

_64B2:
        ldx TargetOffset
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        and #$02		;Invulnerable
        beq NotInvuln
        lda MultiCommand
        tax
        lda AtkType,X
        and #$7F
        cmp #$60		;hide/show monsters
        bne InvulnOrFake
        bra Finish
NotInvuln:
        ldx TargetOffset
        lda CharStruct::Status4,X
        ora CharStruct::AlwaysStatus4,X
        and #$40		;False Image
        beq NotFalseImage
InvulnOrFake:
        ldx AttackerOffset
        lda CharStruct::Command,X
        cmp #$1D		;Catch
        bne DoNothing
        lda MultiCommand
        tax
        lda AtkType,X
        and #$7F
        cmp #$60		;hide/show monster
        beq Finish
        lda #$7E		;this attack type always misses
        bra Finish
DoNothing:
        lda #$7F		;this attack type does nothing
        bra Finish
NotFalseImage:
        lda MultiCommand
        tax
        lda AtkType,X
Finish:
        and #$7F
        cmp #$7F
        bne :+
        lda #$75		;Convert type 7F to 75 (does nothing)
        bra :++
:       cmp #$7E
        bne :+
        lda #$74		;Convert type 7E to 74 (always miss)
:	ASL
        tax
        lda f:AtkTypeJumpTable,X
        sta $08
        lda f:AtkTypeJumpTable+1,X
        sta $09
        lda #$C2 ;.b #bank(AtkTypeJumpTable)
        sta $0A
        jmp [$0008]		;jump to routine from attack type table

.endproc

; ---------------------------------------------------------------------------

.proc ClearAttackData

_6523:
        ldx #$0017	;clears $4D through $64, vars used by attack types
:       stz $4D,X
        dex
        bpl :-
        txa 		;A is now $FF
        ldx #$0011
ClearDamageData:	;Wipes $7B69-$7B7A with $FF, used for final damage/healing amounts
        sta BaseDamage,X
        dex
        bpl ClearDamageData
        tdc
        sta StatusFixedDur
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CopyAttackParams

_653A:
        lda AttackerOffset2
        tax
        lda AttackInfo::Param1,X
        sta Param1
        lda AttackInfo::Param2,X
        sta Param2
        lda AttackInfo::Param3,X
        sta Param3
        rts

.endproc

; ---------------------------------------------------------------------------

;Applies damage to both displayed damage and actual character health
;also sets up display of the various types of evasion
;**optimize: could cut down a lot of space with some utility routines for the damage/healing->display section
.proc ApplyDamage

_654D:
        lda AtkMissed
        beq NotMiss
        lda wTargetIndex
        cmp #$04		;monster check
        bcs DoneBlocking
        lda ShieldBlock
        cmp #$07		;aegis shield block
        bne BlockNotAegis
        lda wTargetIndex
        tax
        tdc
        jsr SetBit_X
        pha
        lda MultiCommand
        tax
        pla
        sta AegisBlockTarget,X
        bra DoneBlocking
BlockNotAegis:
        lda MultiCommand
        tax
        lda SwordBlock
        ora KnifeBlock
        ora ElfCape
        ora ShieldBlock
        ora BladeGrasp
        sta BlockType,X
DoneBlocking:
        ldx #$0006
ClearCommandLoop:
        stz CurrentCommand,X
        dex
        bpl ClearCommandLoop
        lda AtkMissed
        bpl AfterMiss
        ldx #$4000
        stx DamageToTarget	;if AtkMissed bit 80h set, sets a flag
        bra AfterMiss
NotMiss:
        lda AtkElement
        sta CurrentCommand::Element
        lda wTargetIndex
        cmp #$04		;monster check
        bcc Party
        lda SandwormHitFlag
        beq :+
        lda SandwormHitIndex
        bra :++
:       lda wTargetIndex
:	SEC
        sbc #$04		;now monster index
Party:				;or party index
        tax
        tdc
        jsr SetBit_X
        sta TargetBitMaskSmall
AfterMiss:
        lda MultiDamage	;calculation for GetDamageDisplayOffset later
        sta $24
        lda #$18		;24
        sta $25
        jsr Multiply_8bit
CheckDamageAttacker:
        ldx DamageToAttacker
        bmi CheckDamageTarget		;if high bit set
        lda AttackerIndex
        jsr GetDamageDisplayOffset	;sets X
        lda DamageToAttacker
        sta DisplayDamage,X
        lda DamageToAttacker+1
        sta DisplayDamage+1,X
        and #$40
        bne CheckDamageTarget		;if 40h set
        longa
        ldx AttackerOffset
        sec
        lda CharStruct::CurHP,X
        sbc DamageToAttacker
        beq :+
        bcs :++
:       tdc 			;0 HP
:       sta CharStruct::CurHP,X	;new HP total
        shorta0
CheckDamageTarget:
        ldx DamageToTarget
        bpl :+
        jmp CheckHealAttacker
:       bne DamageTarget
        lda CurrentCommand::ID
        cmp #$2B		;Magic of any kind
        beq Magic
        tax
        lda ROMFightCommands,X		;per-command table, 0 or 1
        beq DamageTarget
        bra :+
Magic:
        lda CurrentCommand::Magic
        cmp #$81
        beq :+
        cmp #$80
        bne DamageTarget
:       inc FightFlag
DamageTarget:
        lda SandwormHitFlag
        beq :+
        lda SandwormHitIndex
        bra :++
:       lda wTargetIndex
:       jsr GetDamageDisplayOffset	;sets X
CheckReflectStacking:		;this section handles when a target takes damage multiple times in one cast due to reflect
        lda Reflected
        beq NoReflect
        longa
        lda DisplayDamage,X
        cmp #$FFFF			;indicates an uninitialized value here
        beq AbortReflect		;was reflected but no multi damage yet
        lda DisplayDamage,X
        and #$3FFF			;clear flags
        sta $0E				;existing damage without flags
        lda DisplayDamage,X
        and #$8000
        sta $10				;healing flag
        clc
        lda DamageToTarget
        and #$3FFF			;clear flag bits
        adc $0E				;add existing damage without flags
        cmp #$270F			;9999
        bcc :+
        sec
        sbc #$270F			;-9999
        sta $12				;damage overflowing 9999
        sec
        lda DamageToTarget		;damage including flags
        sbc $12				;-damage overflowing 9999
        sta DamageToTarget		;9999 cap preserving flags
        lda #$270F
:       ora $10				;healing flag
        sta DisplayDamage,X
        shorta0
        lda DisplayDamage+1,X
        bra CheckEarthWall
AbortReflect:
        shorta0
NoReflect:
        lda DamageToTarget
        sta DisplayDamage,X
        lda DamageToTarget+1
        sta DisplayDamage+1,X
CheckEarthWall:
        and #$40
        bne CheckHealAttacker
        lda wTargetIndex
        cmp #$04		;monster check
        bcs ApplyDamage
        lda CurrentCommand::Magic
        cmp #$80		;monster fight
        beq EarthWall
        cmp #$DE		;strong fight?	vacuum wave?
        beq EarthWall
        cmp #$81		;monster specialty
        bne ApplyDamage
EarthWall:
        lda EarthWallHP
        ora EarthWallHP+1
        beq ApplyDamage
        longa
        sec
        lda EarthWallHP
        sbc DamageToTarget
        bcs :+
        tdc
:       sta EarthWallHP
        shorta0
        lda MultiCommand
        tax
        lda #$04
        sta BlockType,X
        bra CheckHealAttacker

ApplyDamage:
        lda DamageToTarget
        ora DamageToTarget+1
        sta CurrentCommand::Damage
        lda wTargetIndex
        cmp #$04		;monster check
        bcc :+			;but it doesn't matter?
:	REP #$20
        ldx TargetOffset
        sec
        lda CharStruct::CurHP,X
        sbc DamageToTarget
        beq :+
        bcs :++
:	tdc 			;min 0
:	sta CharStruct::CurHP,X
        shorta0
CheckHealAttacker:
        ldx HealingToAttacker
        bmi CheckHealTarget
        lda AttackerIndex
        jsr GetDamageDisplayOffset	;sets X
        lda HealingToAttacker
        sta DisplayDamage,X
        lda HealingToAttacker+1
        ora #$80			;set heal flag
        sta DisplayDamage+1,X
        longa
        lda HealingToAttacker
        and #$3FFF			;clear flag bits
        sta $0E				;Healing without flags
        ldx AttackerOffset
        clc
        lda CharStruct::CurHP,X
        adc $0E				;Healing without flags
        bcs :+				;check overflow
        cmp CharStruct::MaxHP,X		;check max hp
        bcc :++
:	lda CharStruct::MaxHP,X		;cap at max hp
:	sta CharStruct::CurHP,X
        shorta0
CheckHealTarget:	
        ldx HealingToTarget
        bmi CheckHealAttackerMP
        lda SandwormHitFlag
        beq :+
        lda SandwormHitIndex
        bra :++
:	lda wTargetIndex
:	jsr GetDamageDisplayOffset
        lda HealingToTarget
        sta DisplayDamage,X
        lda HealingToTarget+1
        ora #$80			;set healing flag
        sta DisplayDamage+1,X
        longa
        lda HealingToTarget
        and #$3FFF			;clear flag bits
        sta $0E				;healing without flags
        ldx TargetOffset
        clc
        lda CharStruct::CurHP,X
        adc $0E				;healing without flags
        bcs :+				;check overflow
        cmp CharStruct::MaxHP,X		;check max hp
        bcc :++
:	lda CharStruct::MaxHP,X		;cap at max hp
:	sta CharStruct::CurHP,X
        shorta0
CheckHealAttackerMP:
        ldx HealingToAttackerMP
        bmi CheckHealTargetMP
        lda AttackerIndex
        jsr GetDamageDisplayOffset
        lda HealingToAttackerMP
        sta DisplayDamage,X
        lda HealingToAttackerMP+1
        ora #$80			;healing flag
        sta DisplayDamage+1,X
        longa
        lda HealingToAttackerMP
        and #$3FFF			;clear flag bits
        sta $0E				;healing without flags
        ldx AttackerOffset
        clc
        lda CharStruct::CurMP,X
        adc $0E				;healing without flags
        cmp CharStruct::MaxMP,X
        bcc :+
        lda CharStruct::MaxMP,X		;cap at max mp
:	sta CharStruct::CurMP,X
        shorta0
CheckHealTargetMP:	
        ldx HealingToTargetMP
        bmi CheckDamageAttackerMP
        lda SandwormHitFlag
        beq :+
        lda SandwormHitIndex
        bra :++
:	lda wTargetIndex
:	jsr GetDamageDisplayOffset
        lda HealingToTargetMP
        sta DisplayDamage,X
        lda HealingToTargetMP+1
        ora #$80			;healing flag
        sta DisplayDamage+1,X
        longa
        lda HealingToTargetMP
        and #$3FFF			;clear flag bits
        sta $0E				;healing without flags
        ldx TargetOffset
        clc
        lda CharStruct::CurMP,X
        adc $0E				;healing without flags
        cmp CharStruct::MaxMP,X
        bcc :+
        lda CharStruct::MaxMP,X		;cap at max mp
:	sta CharStruct::CurMP,X
        shorta0
CheckDamageAttackerMP:
        ldx DamageToAttackerMP
        bmi CheckDamageTargetMP
        lda AttackerIndex
        jsr GetDamageDisplayOffset
        lda DamageToAttackerMP
        sta DisplayDamage,X
        lda DamageToAttackerMP+1
        sta DisplayDamage+1,X
        longa
        lda DamageToAttackerMP
        and #$3FFF			;clear flag bits
        sta $0E				;damage without flags
        ldx AttackerOffset
        sec
        lda CharStruct::CurMP,X
        sbc $0E				;damage without flags
        bcs :+
        tdc 				;min 0
:	sta CharStruct::CurMP,X
        shorta0
CheckDamageTargetMP:
        ldx DamageToTargetMP
        bmi CheckZombie
        lda SandwormHitFlag
        beq :+
        lda SandwormHitIndex
        bra :++
:	lda wTargetIndex
:	jsr GetDamageDisplayOffset
        lda DamageToTargetMP
        sta DisplayDamage,X
        lda DamageToTargetMP+1
        sta DisplayDamage+1,X
        longa
        lda DamageToTargetMP
        and #$3FFF			;clear flag bits
        sta $0E				;damage without flags
        ldx TargetOffset
        sec
        lda CharStruct::CurMP,X
        sbc $0E				;damage without flags
        bcs :+
        tdc 				;min 0
:	sta CharStruct::CurMP,X
        shorta0
CheckZombie:
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$02			;zombie
        beq Ret
        stz CharStruct::CurHP,X		;zombies always have 0 HP
        stz CharStruct::CurHP+1,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;odd utility routine
;X = D0ED79[A]*2 + ($26)
;contents of that table are 8 9 10 11 0 1 2 3 4 5 6 7
;so effectively adjusts the index so party members are after monsters
;then doubles it to access a 16 bit structure
;then adjusts it by another offset cacluated earlier ($79FB) * 24
.proc GetDamageDisplayOffset

_685C:
        tax
        lda ROMCombatantReorder,X
        asl
        longa
        clc
        adc $26
        tax
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 00
;Not sure what this is for, doesn't do much
.proc Attack00

_686C:
        INC UnknownReaction
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 01 (Monster Fight)
.proc Attack01

_6870:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr MonsterDamage
        jsr BackRowMod
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr MagicSwordMod
        lda AtkMissed
        bne Miss
        jmp CalcFinalDamageMSword
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 02 (Monster Specialty)
;Param3: Status (special format)
.proc Attack02

_6898:
        ldx AttackerOffset
        lda CharStruct::Specialty,X
        bmi :+
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
:	JSR CheckTargetImage
        lda AtkMissed
        bne Miss
        jsr MonsterDamage
        jsr BackRowMod
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr SpecialtyMod
        jsr MagicSwordMod
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamageMSword
        lda wTargetIndex
        cmp #$04
        bcs :+
        lda EarthWallHP
        ora EarthWallHP+1
        bne Ret
:	JMP ApplySpecialtyEffects
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 03 (Magic Sword Level 1)
;Param1: Magic Sword Param
;Param2: Element
;Param3: Status
.proc Attack03

_68E2:
        jsr RemoveMagicSword
        ldx TargetOffset
        lda Param1
        sta CharStruct::MSwordAnim,X
        lda Param2
        sta CharStruct::MSwordElemental1,X
        lda Param3
        sta CharStruct::MSwordStatus1,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 04 (Magic Sword Level 2)
;Param1: Magic Sword Param
;Param2: Element
;Param3: Status
.proc Attack04

_68F7:
        jsr RemoveMagicSword
        ldx TargetOffset
        lda Param1
        sta CharStruct::MSwordAnim,X
        lda Param2
        sta CharStruct::MSwordElemental2,X
        lda Param3
        sta CharStruct::MSwordStatus2,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 05 (Magic Sword Level 3)
;Param1: Magic Sword Param
;Param2: Element
;Param3: Status
.proc Attack05

_690C:
        jsr RemoveMagicSword
        ldx TargetOffset
        lda Param1
        sta CharStruct::MSwordAnim,X
        lda Param2
        sta CharStruct::MSwordElemental3,X
        lda Param3
        sta CharStruct::MSwordStatusSpecial,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 06 (Magic Attack)
;Param2: Spell Power
;Param3: Element
.proc Attack06

_6921:
        jsr CheckAegis
        lda AtkMissed
        bne Miss
        lda Param3
        sta AtkElement
        jsr NormalMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamage
Miss:	LDA MagicNull
        beq Return
        stz AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 07 (Gravity)
;Param1: Hit%
;Param2: *Fraction/16
;Param3: Status 2
.proc Attack07

_694A:
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne Return
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20   			;heavy
        bne Miss
        jsr GravityDamage
Status:
        lda AtkMissed
        bne Return
        lda #$3C			;60
        sta Param2
        jsr CalcStatusDuration
        jsr ApplyStatus2
        stz AtkMissed
        rts
        							;
Miss:	INC AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 08 (Flare)
;Param2: Spell Power
;Param3: Element
;
;**optimize: merge with Attack0C
.proc Attack08

_6971:
        lda Param3
        sta AtkElement
        jsr FlareMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne :+
        jsr CalcFinalDamage
:	LDA MagicNull
        beq Return
        stz AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 09 (Random)
;Param2: M
;Param3: Element
.proc Attack09

_6993:
        lda Param3
        sta AtkElement
        jsr RandomMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne :+
        jsr CalcFinalDamage
:	LDA MagicNull
        beq Return
        stz AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 0A  (Physical Magic)
;Param1: Hit%
;Param2: Spell Power
;Param3: Element
.proc Attack0A

_69B5:
        jsr HitPhysicalMagic
        lda AtkMissed
        bne Miss
        lda Param3
        sta AtkElement
        jsr PhysicalMagicDamage
        jsr TargetStatusModPhys
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamage
Miss:	LDA MagicNull
        beq Return
        stz AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 0B (Level Based Damage)
;Param1: Element
;Param2: Type+Status%
;Param3: Status 1 or 2
.proc Attack0B

_69DB:
        jsr CheckAegis
        lda AtkMissed
        bne Miss
        jsr LevelDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        lda Param1
        sta AtkElement
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamage
        jsr ApplyConditionalStatus
Miss:	LDA MagicNull
        beq Ret
        stz AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 0C (Flare w/HP Leak)
;Param1: Element
;Param2: Spell Power
;Param3: HP Leak Duration
.proc Attack0C

_6A07:
        jsr CheckAegis
        lda AtkMissed
        bne Miss
        lda Param1
        sta AtkElement
        jsr FlareMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamage
        lda Param3
        sta StatusDuration
        lda #$08			;HP Leak
        sta Param3
        jsr ApplyStatus4
Miss:	LDA MagicNull
        beq Ret
        stz AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 0D (Drain)
;Param1: Hit%
;Param2: *Spell Power
.proc Attack0D

_6A3C:
        ldx AttackerOffset
        lda CharStruct::DamageMod,X
        bmi Hit		;check for autohit
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne Miss
Hit:	JSR NormalMagicDamage
        jsr TargetStatusModMag
        jmp DrainDamage
Miss:	;we failed hit check, but some commands need to report miss differently	
        lda AttackerIndex
        cmp #$04
        bcs Ret  		;check if monster
        ldx AttackerOffset
        lda CharStruct::ActionFlag,X
        bpl Ret
        lda #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 0E (Psyche)
;Param1: Hit%
;Param2: *Spell Power
.proc Attack0E

_6A65:
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne Ret
        jsr FlareMagicDamage
        jsr TargetStatusModMag
        jsr PsycheDamage
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 0F (Reduce HP to Critical)
;Param1: Hit%
;Param2: *conditional autohit flag
.proc Attack0F

_6A76:
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne :+
        jsr SetHPCritical
:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 10 (Heal)
;Param2: Spell Power
.proc Attack10

_6A81:
        jsr NormalMagicDamage
        tdc
        tax
        stx Defense
        jsr MultiTargetMod
        jmp CureTarget

.endproc

; ---------------------------------------------------------------------------

;Attack Type 11 (Full Heal)
;Param2: Spell Power
.proc Attack11

_6A8E:
        jsr NormalMagicDamage
        tdc
        tax
        stx Defense
        lda MultiCommand
        asl
        tax
        lda TargetBitmask,X
        and #$F0
        bne :+			;player targets in A
        lda TargetBitmask,X
        and #$0F
        jsr ShiftMultiply_16
        sta $0E
        lda TargetBitmask+1,X
        and #$F0
        jsr ShiftDivide_16
        ora $0E			;A now has monsters in order
:	JSR CountSetBits
        dex
        beq :+
        jsr MultiTargetMod
        jmp CureTarget
:	JMP FullCureTarget

.endproc

; ---------------------------------------------------------------------------

;Attack Type 12 (Status Effect 1)
;Param1: Hit%
;Param3: Status 1
.proc Attack12

_6AC4:
        jsr HitMagic
        lda AtkMissed
        bne :+
        jsr ApplyStatus1
:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 13 (Status Effect 2)
;Param1: Hit%
;Param2: Duration
;Param3: Status 2
.proc Attack13

_6ACF:
        jsr HitMagic
        lda AtkMissed
        bne :+
        jsr CalcStatusDuration
        jsr ApplyStatus2
:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 14 (Status Effect 3)
;Param1: Hit%
;Param2: Duration
;Param3: Status 3
.proc Attack14

_6ADD:
        jsr HitMagic
        lda AtkMissed
        bne :+
        jsr CalcStatusDuration
        jsr ApplyStatus3
:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 15 (Status Effect 1 Toggle)
;Param1: Hit%
;Param2: *conditional autohit flag
;Param3: Status 1
.proc Attack15

_6AEB:
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne :+
        jsr ToggleStatus1
:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 16 (Status Effect 3, Mutually exclusive statuses)
;Param1: Hit%
;Param2: Allowed Status 3
;Param3: Applied Status 3
.proc Attack16

_6AF6:
        jsr HitMagic
        lda AtkMissed
        bne :+
        jsr ApplyStatus3Exclusive
:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 17 (Status Effect 1, or Heals Undead to Max HP)
;Param1: Hit%
;Param2: *conditional autohit flag
;Param3: Status 1
.proc Attack17

_6B01:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01			;undead creature
        bne Undead
        lda CharStruct::ArmorProperties,X
        and #$02    			;undead armor
        beq NotUndead
Undead:
        longa
        lda CharStruct::MaxHP,X
        sta CharStruct::CurHP,X
        shorta0
        rts
NotUndead:							;
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne Ret
        lda wTargetIndex
        tay
        lda ActiveParticipants,Y
        beq Miss
        jmp ApplyStatus1
Miss:	INC AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 18 (Kill, Miss vs. Heavy)
;Param1: Hit%
;Param2: *conditional autohit flag
.proc Attack18

_6B32:
        jmp KillNonHeavy

.endproc

; ---------------------------------------------------------------------------

;Attack Type 19 (Remove Statuses)
;Param1: Status 1
;Param2: Status 2
;Param3: Status 3
.proc Attack19

_6B35:
        ldx AttackerOffset
        lda CharStruct::Status2,X
        and #$10   			;charm
        beq :+
        inc AtkMissed
        rts
:	JSR RemoveStatus3
        lda Param2
        sta Param3
        jsr RemoveStatus2
        lda Param1
        sta Param3
        jmp RemoveStatus1

.endproc

; ---------------------------------------------------------------------------

;Attack Type 1A (Revive)
;Revive target, kills non-heavy undead
;Param1: Hit% vs undead, high bit autohit
;Param2: High bit autohit unless monster vs party, also high bit restores to Max MP
;Param3: Fraction/4 of MaxHP to recover
.proc Attack1A

_6B52:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01				;undead	creature
        bne Kill
        lda CharStruct::ArmorProperties,X
        and #$02   				;undead armor
        beq CheckDead
Kill:	JSR KillNonHeavy
        rts
CheckDead:
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$80   				;dead
        bne Revive
        inc AtkMissed
        rts
Revive:       
        jsr ReviveTarget
        lda Param2
        bpl Ret
        jsr FullMPHeal
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 1B (Drain to party)
;Used by Sylph (and only Sylph?)
;Param2: Spell Power
.proc Attack1B

_6B7D:
        lda MultiDamage			;this calculates an offset
        sta $24				;to display healing later
        lda #$18			;24
        sta $25
        jsr Multiply_8bit
        ldx $26
        phx
        jsr NormalMagicDamage
        jsr TargetStatusModMag
        jsr CalcFinalDamage
        longa
        lda DamageToTarget
        jsr ShiftDivide_4
        sta $0E				;healing value in $0E
        shorta0
        plx
        stx $26
        tdc
        tay
        tax
        stx $10
PartyLoop:				;operates on all 4 party members
        lda ActiveParticipants,Y
        beq NextChar
        ldx $10
        lda CharStruct::Status1,X
        and #$C0    			;dead or stone
        bne NextChar
        lda CharStruct::Status4,X
        and #$81			;erased or hidden
        bne NextChar
        lda CharStruct::CmdStatus,X
        and #$10			;jumping
        bne NextChar
        longa
        clc
        lda CharStruct::CurHP,X
        adc $0E
        bcs :+				;cap overflow
        cmp CharStruct::MaxHP,X
        bcc :++
:	LDA CharStruct::MaxHP,X
:	STA CharStruct::CurHP,X
        shorta0
        tya
        jsr GetDamageDisplayOffset
        lda $0E
        sta DisplayDamage,X		;stores healing	for display
        lda $0F
        ora #$80			;flag for healing
        sta DisplayDamage+1,X
NextChar:
        ldx $10
        jsr NextCharOffset
        stx $10
        iny
        cpy #$0004
        bne PartyLoop
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 1C (Grant Elemental Absorption, Immunity, or Weakness)
;Param1: Absorb
;Param2: Immune
;Param3: Weak
.proc Attack1C

_6BFC:
        ldx TargetOffset
        lda CharStruct::EAbsorb,X
        ora Param1
        sta CharStruct::EAbsorb,X
        lda CharStruct::EImmune,X
        ora Param2
        sta CharStruct::EImmune,X
        lda CharStruct::EWeak,X
        ora Param3
        sta CharStruct::EWeak,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 1D (Scan Monster)
;Param1: 	80h miss vs bosses
;		40h scans level
;		20h scans current and max hp
;		08h scans element weakness (and status effects 1 and 2, due to a bug)
;		04h (should scan status effects 1 and 2, but does nothing due to a bug)
;**bug: 	status scan checks the wrong bit
.proc Attack1D

_6C17:
        jsr SetupMsgBoxIndexes	;prepares things in case of x-magic
        stx $0E			;$7B2C*24
        sty $10			;$7B2C*12, this saved var isn't used
        lda Param1
        bpl :+
        lda BattleMonsterID+1
        beq :+
        jmp Miss
:	LDA Param1
        and #$40
        beq :+
        lda #$11		;message to display
        sta MessageBoxes,X
        inc $0E
        ldx TargetOffset
        lda CharStruct::Level,X
        sta MessageBoxData+0,Y	;numbers used in message box
        tdc
        sta MessageBoxData+1,Y	;these are each 3 bytes long
        sta MessageBoxData+2,Y
:	LDA Param1
        and #$20
        beq :++
        ldx $0E
        lda #$12		;message to display
        sta MessageBoxes,X
        inc $0E
        ldx TargetOffset
        lda CharStruct::CmdImmunity,X
        bpl :+
        lda #$FF		;fill results with $FF if scan immune
        sta MessageBoxData1+0,Y
        sta MessageBoxData1+1,Y
        sta MessageBoxData1+2,Y
        sta MessageBoxData2+0,Y
        sta MessageBoxData2+1,Y
        sta MessageBoxData2+2,Y
        bra :++
:	LDA CharStruct::CurHP,X
        sta MessageBoxData1+0,Y
        lda CharStruct::CurHP+1,X
        sta MessageBoxData1+1,Y
        lda CharStruct::MaxHP,X
        sta MessageBoxData2+0,Y
        lda CharStruct::MaxHP+1,X
        sta MessageBoxData2+1,Y
        tdc
        sta MessageBoxData1+2,Y
        sta MessageBoxData2+2,Y
:	LDA Param1
        and #$08
        beq :++
        ldx TargetOffset
        lda CharStruct::EWeak,X
        sta $12
        ldx $0E
        tdc
        tay
        lda #$12		;message to display (incremented later)
EleLoop:				;loop through elements
        inc
        asl $12
        bcc :+
        sta MessageBoxes,X
        inx
        inc $0E
:	INY
        cpy #$0008
        bne EleLoop
:	LDA Param1
        and #$08		;this is a bug and should check for $04
        beq Ret
        ldx TargetOffset
        lda CharStruct::Status1,X
        sta $13
        lda CharStruct::Status2,X
        sta $12
        ldx $0E
        tdc
        tay
        lda #$00		;message to display (incremented later)
StatusLoop:			;loop through status effects (1 and 2)
        inc
        asl $12
        rol $13
        bcc :+
        sta MessageBoxes,X
        inx
        inc $0E
:	INY
        cpy #$0010
        bne StatusLoop
Ret:	rts
        								;
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------


;Attack Type 1E (Drag)
;Sets battle speed
.proc Attack1E

_6CE4:
        ldx #$0004
        lda ROMBattleSpeedTable,X
        sta ATBWaitTime
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 1F (Void)
;Param1: Success rate
.proc Attack1F

_6CEF:
        lda EncounterInfo::Flags
        and #$40   		;can't use void
        bne Miss
        jsr Random_0_99
        cmp Param1
        bcs Miss
        lda Void
        ora #$40
        sta Void
        tdc
        tax
        tay
Loop:	;X= CharStruct Offset, Y = Timer Offset, $0E loop index (hopefully initialized to 0 elsewhere?)
        lda CharStruct::Status4,X
        and #$FB		;clear singing status
        sta CharStruct::Status4,X
        tdc
        sta EnableTimer::Sing,Y
        sta CurrentTimer::Sing,Y
        sta InitialTimer::Sing,Y
        jsr NextCharOffset	;next CharStruct
        clc
        tya
        adc #$0B		;next char for timers
        tay
        inc $0E
        lda $0E
        cmp #$0C		;12
        bne Loop		;12 loops, one for each participant
        rts
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 20 (Exit)
;Param1: Success Rate
.proc Attack20

_6D2E:
        lda EncounterInfo::FleeChance
        bmi Miss		;80h = can't run
        jsr Random_0_99
        cmp Param1
        bcs Miss
        lda #$80
        sta FleeSuccess
        rts
Miss:	INC AtkMissed
        jsr SetupMsgBoxIndexes
        lda #$20		;message to display
        sta MessageBoxes,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 21 (Reset)
.proc Attack21

_6D4B:
        lda TerrainType
        cmp #$1F		;special case, changes terrain on reset
        bne :+			;.. likely neoexdeath back to tree
        lda #$1C
        sta TerrainType
:	INC ResetBattle
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 22 (Quick)
.proc Attack22

_6D5B:
        lda AttackerIndex
        cmp #$04   		;4+ is monster
        bcs Miss
        lda QuickTurns  	;miss if already Quick
        beq :+
Miss:	INC AtkMissed
        rts
        								;
:	TDC
        tax
Loop:				;Freezes time for everyone
        inc QuickTimeFrozen,X
        inx
        cpx #$000C
        bne Loop
        lda AttackerIndex
        sta QuickCharIndex
        tax
        stz QuickTimeFrozen,X	;unfreeze Quick guy
        lda #$03		;3 turns because this one counts
        sta QuickTurns
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 23 (Earth Wall)
;Param2: Spell Power
;
;**optimize: save a byte on AtkMissed, use 16 bit mode
.proc Attack23

_6D83:
        lda EarthWallHP
        ora EarthWallHP+1
        bne Miss
        lda Param2
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        clc
        lda $26			;add 1000 in 8 bit mode for some reason
        adc #$E8		;232
        sta EarthWallHP
        lda $27
        adc #$03		;768
        sta EarthWallHP+1
        rts
Miss:	INC a:AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 24 (Potions)
.proc Attack24

_6DAB:
        jsr ItemFormula
        jsr MedicineMod
        jmp CureTarget

.endproc

; ---------------------------------------------------------------------------

;Attack Type 25 (Ethers)
.proc Attack25

_6DB4:
        jsr ItemFormula
        jsr MedicineMod
        jmp MPHeal

.endproc

; ---------------------------------------------------------------------------

;Attack Type 26 (Full Restore)
;Param1:	80h Restore Full HP
;		40h Restore Full MP
;		20h Restore Half of max HP and MP
.proc Attack26

_6DBD:
        lda Param1
        bpl :+
        jsr FullCureTarget
:	LDA Param1
        and #$40
        beq :+
        jsr FullMPHeal
:	LDA Param1
        and #$20
        beq Ret
        jsr RestoreHalfMax	;restores hp and mp
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 27 (Status Effects ignoring Immunity)
;Param1: Status1
;Param2: Status2
;Param3: Status3
.proc Attack27

_6DD7:
        lda #$5A		;90
        sta StatusDuration
        jsr ApplyStatus3Bypass
        lda Param2
        sta Param3
        jsr ApplyStatus2Bypass
        lda Param1
        sta Param3
        jmp ApplyStatus1Bypass

.endproc

; ---------------------------------------------------------------------------

;Attack Type 28 (Direct Magic Damage)
;Param1: Hit%
;Damage = Param2 + Param3*256
;
;**optimize: just go to 16 bit mode and load param2
.proc Attack28

_6DED:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        lda Param2
        tax
        stx $0E
        lda Param3
        longa
        jsr ShiftMultiply_256
        clc
        adc $0E
        cmp #$270F		;cap at 9999
        bcc :+
        lda #$270F
:	STA DamageToTarget
        shorta0
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 29 (Apply Status 4, miss vs Heavy)
;Param1: Hit%
;Param2: Duration
;Param3: Status 4
.proc Attack29

_6E12:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20   			;heavy
        bne Miss
        jsr HitMagic
        lda AtkMissed
        bne Ret
        lda Param2
        sta StatusDuration
        jmp ApplyStatus4
Miss:	INC AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 2A (% of Max HP)
;Param1: Element
;Param2: Fraction/16
;Param3: HP Leak Duration
;
;**optimize: remove Second Param3 load
.proc Attack2A

_6E2D:
        lda Param1
        sta AtkElement
        jsr ElementDamageModMag2
        lda AtkMissed
        bne Ret
        jsr CalcDamageMaxHP
        lda Param3
        beq Ret
        lda Param3
        sta StatusDuration
        lda #$08		;HP Leak
        sta Param3
        jsr ApplyStatus4
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 2B (Damage % on Attacker Current HP)
;Param1: Element
;Param2: Fraction/16
;Param3: Status 1 (to attacker)
.proc Attack2B

_6E4C:
        lda Param1
        sta AtkElement
        jsr ElementDamageModMag2
        lda AtkMissed
        bne Ret
        jsr CalcDamageAttackerCurHP
        jsr ApplyStatus1AttackerBypass
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 2C (50/50 chance of Status 1 or Status 2)
;Param1: Status 1
;Param2: Status 2 Duration
;Param3: Status 2
.proc Attack2C

_6E5E:
        jsr Random_0_99
        cmp #$32		;50
        bcs S2
        lda Param1
        sta Param3
        jmp ApplyStatus1
S2:	JSR CalcStatusDuration
        jmp ApplyStatus2

.endproc

; ---------------------------------------------------------------------------

;Attack Type 2D (Ground Magic)
;Param2: Spell Power
;Param3: Element
.proc Attack2D

_6E72:
        jsr CheckFloat
        lda AtkMissed
        bne Miss
        lda Param3
        sta AtkElement
        jsr NormalMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamage
Miss:	LDA MagicNull
        beq Ret
        stz AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 2E (Phys Magic w/ Status 1) (Reaper's Sword)
;Param1: Hit%
;Param2: Spell Power
;Param3: Status 1
.proc Attack2E

_6E9B:
        jsr HitPhysicalMagic
        lda AtkMissed
        bne Ret
        jsr PhysicalMagicDamage
        jsr TargetStatusModPhys
        jsr CalcFinalDamage
        jsr ApplyStatus1
        stz AtkMissed
Ret:	rts
;

.endproc

; ---------------------------------------------------------------------------

;Attack Type 2F (Status 1 to Creature Type, unused)
;Param1: Creature Type
;Param3: Status 1
.proc Attack2F

_6EB1:
        jsr CheckCreatureType
        lda AtkMissed
        bne Ret
        jsr ApplyStatus1
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 30 (Fists)
;Param1: Crit%
.proc Attack30

_6EBC:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        beq Hit
        lda #$80
        sta AtkMissed
        rts
Hit:								;
        jsr FistDamage
        jsr BackRowMod
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr CheckCrit
        jsr CalcFinalDamage
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 31 (Swords)
;Param1: Element
;Param2/3: Proc% and Proc, not handled here
;if not(!_CombatTweaks)
.proc Attack31

_6EE1:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
Hit:	JSR SwordDamage
Row:	JSR BackRowMod
Cmd:	JSR CommandMod
        jsr DoubleGripMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
MSword:	JSR MagicSwordMod
Finish:	LDA TargetDead
        bne Ret
        lda AtkMissed
        bne Miss
        lda Param1
        sta AtkElement
        jsr ElementDamageModPhys
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamageMSword
        jmp ApplyMSwordStatus
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 32 (Knives)
;Param1: Element
;Param2/3: Proc% and Proc, not handled here
.proc Attack32

_6F1E:
        jsr SetHit100andHalfTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr KnifeDamage
        jsr BackRowMod
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr MagicSwordMod
        lda TargetDead
        bne Ret
        lda AtkMissed
        bne Miss
        lda Param1
        sta AtkElement
        jsr ElementDamageModPhys
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamageMSword
        jmp ApplyMSwordStatus
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 33 (Spears)
;Param1: Element
;Param2/3: Proc% and Proc, not handled here
;if not(!_CombatTweaks)
.proc Attack33

_6F58:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr SwordDamage
        jsr CommandMod
        jsr CheckJump
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        lda Param1
        sta AtkElement
        jsr ElementDamageModPhys
        lda AtkMissed
        bne Miss
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 34 (Axes)
;Param1: Hit%
;Param2/3: Proc% and Proc, not handled here
;if not(!_CombatTweaks)
.proc Attack34

_6F84:
        jsr SetHitParam1andTargetEvade_Dupe
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr AxeDamage
        jsr BackRowMod
        jsr CommandMod
        jsr DoubleGripMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 35 (Bows with Status)
;Param1: 	Hit%
;Param2:  	High Bit determines Status type 1 or 2
;		Lower 7 bits are Status Hit%
;Param3: 	Status (type 1 or 2 depending on bit above)
.proc Attack35

_6FA8:
        jsr SetHitParam1andHalfTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr KnifeDamage
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr CalcFinalDamage
        jmp ApplyConditionalStatus
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 36 (Bows with Element)
;Param1: 	Hit%
;Param2:  	Crit%
;Param3: 	Element
.proc Attack36

_6FC9:
        jsr SetHitParam1andHalfTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr KnifeDamage
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        lda Param3
        sta AtkElement
        jsr ElementDamageModPhys
        lda AtkMissed
        bne Miss
        lda Param2
        sta Param1		;move Crit% to Param1
        jsr CheckCrit		;because crit routine expects it there
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 37 (Katanas)
;Param1: Crit%
;Param2/3: Proc% and Proc, not handled here
;if not(!_CombatTweaks)
.proc Attack37

_6FF9:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr SwordDamage
        jsr BackRowMod
        jsr CommandMod
        jsr DoubleGripMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr CheckCrit
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 38 (Whips)
;Param1: Hit%
;Param2/3: Proc% and Proc, not handled here
.proc Attack38

_7020:
        jsr SetHitParam1andTargetEvade_Dupe
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr KnifeDamage
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 39 (Bells)
.proc Attack39

_703E:
        jsr CheckVoid
        lda AtkMissed
        bne Miss
        jsr BellDamage
        jsr TargetStatusModMag
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 3A (Long Reach Axes)
;Param1: Hit%
;Param2/3: Proc% and Proc, not handled here
;if not(!_CombatTweaks)
.proc Attack3A

_7053:
        jsr SetHitParam1andTargetEvade_Dupe
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr AxeDamage
        jsr CommandMod
        jsr DoubleGripMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 3B (Rods)
;Param1: Hit%
;Param3: Element
.proc Attack3B

_7074:
        jsr SetHitParam1andTargetMagicEvade
        jsr CheckAegis
        jsr TargetMHitMod
        jsr CheckForHit
        lda AtkMissed
        bne Miss
        jsr RodDamage
        jsr TargetStatusModMag
        lda Param3
        sta AtkElement
        jsr ElementDamageModPhys
        lda AtkMissed
        bne Miss
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 3C (Rune Weapons)
;Param1: Hit%
;Param2: Rune Damage Boost
;Param3: Rune MP Cost
;if not(!_CombatTweaks)
.proc Attack3C

_709D:
        jsr SetHitParam1andTargetEvade_Dupe
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr AxeDamage
        jsr RuneMod
        jsr BackRowMod
        jsr CommandMod
        jsr DoubleGripMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 3D (Death Claw)
;Param1: Hit%
;Param2: Status Duration
;Param3: Status 2
.proc Attack3D

_70C4:
        jsr HitMagic
        lda AtkMissed
        bne Miss
        jsr SetHPCritical
        lda AtkMissed
        bne Miss
        jsr CalcStatusDuration
        jsr ApplyStatus2
        stz AtkMissed
Miss:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 3E (Failure)
;Sets HP to critical or sets Status 4
;Param1: Chance for HP Critical
;Param2: Status Duration
;Param3: Status 4
.proc Attack3E

_70DB:
        jsr Random_0_99
        cmp Param1
        bcs :+
        jmp SetHPCritical
:	JSR CalcStatusDuration
        jsr ApplyStatus4
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 3F (Zombie Breath)
;Param2: Damage Multiplier (M)
.proc Attack3F

_70EC:
        jsr CheckAegis
        lda AtkMissed
        bne Ret
        jsr RandomMagicDamage  	;50..200 * M
        jsr TargetStatusModMag
        jsr CalcFinalDamage
        ldx TargetOffset
        lda CharStruct::CurHP,X
        sta $0E
        lda CharStruct::CurHP+1,X
        sta $0F
        ldx DamageToTarget
        cpx $0E			;check if damage was more than HP
        bcc Ret		;if so, apply zombie
        ldx TargetOffset
        lda #$02		;zombie
        sta Param3
        jsr ApplyStatus1
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 40 (Change Row)
;Param1: Hit%
;Param2: 	80h Switch Row (target)
;		40h Front Row (target)
;		Else Back Row (attacker)
;**optimize: Don't load Param2 twice
.proc Attack40

_7119:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        lda Param2
        bpl :+
        jmp TargetChangeRow		;80h set
:	LDA Param2
        and #$40
        beq :+
        jmp TargetFrontRow		;40h set
:	JMP AttackerBackRow		;anything else
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 41
;Param2: Attack Power
;Not sure what attack this is. Might be unused?  
.proc Attack41

_7134:
        jsr PhysicalParamDamage
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jmp CalcFinalDamage

.endproc

; ---------------------------------------------------------------------------

;Attack Type 42 (Heal HP & Status)
;Param1: 80h Status 1
;	 Else Status 2
;Param2: Spell Power
;Param3: Status 1 or 2
.proc Attack42

_7140:
        jsr NormalMagicDamage
        jsr CureTarget
        lda Param1
        bpl :+
        jmp RemoveStatus1
:	JMP RemoveStatus2

.endproc

; ---------------------------------------------------------------------------

;Attack Type 43 (Steal)
;Param1: Hit%
.proc Attack43

_7150:
        stz StealNoItems
        ldx TargetOffset
        lda CharStruct::StolenItem,X	;check if we already stole
        beq :+
        inc StealNoItems
        bra Miss
:	SEC
        lda wTargetIndex
        sbc #$04			;now monster 0-7
        asl
        tax
        longa
        lda BattleMonsterID,X
        jsr ShiftMultiply_4
        tax
        shorta0
        lda ROMLoot::RareSteal,X
        ora ROMLoot::CommonSteal,X
        bne :+
        inc StealNoItems		;no items to steal
        bra Miss
:	JSR HitCalcSteal
        jsr CheckForHit
        lda AtkMissed
        bne Miss
        jsr StealItem
        lda AtkMissed
        bne Miss
        lda #$21		;steal success message
        bra MsgBox
Miss:	INC AtkMissed
        lda StealNoItems
        beq :+
        lda #$4B		;nothing to steal message
        bra MsgBox
:	LDA #$22		;steal failed message
MsgBox:	PHA
        jsr SetupMsgBoxIndexes
        lda CurrentCommand::ID
        cmp #$0B		;steal
        beq :+
        cmp #$33		;?? command $33 is White Magic L2
        bne :++
:	TDC
        tax
:	PLA
        sta MessageBoxes,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 44 (Escape)
.proc Attack44

_71B9:
        lda AttackerIndex
        cmp #$04
        bcc Ret
        sec
        sbc #$04   		;now monster index 0-7
        tax
        inc MonsterEscaped,X
        lda InactiveMonsters
        jsr SetBit_X		;no longer revivable
        sta InactiveMonsters
        ldx AttackerOffset
        lda #$80			;dead status
        sta CharStruct::Status1,X	;escape is death
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 45 (Throw)
;Param1: Hit%
.proc Attack45

_71D7:
        lda Param1
        sta HitPercent
        tdc
        sta EvadePercent
        jsr HitPhysical
        lda AtkMissed
        bne Ret
        jsr ThrowDamage
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr CalcFinalDamage
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 46 (GilToss)
;Param1: Cost per Level
;Param2: M
.proc Attack46

_71F5:
        jsr GilTossDamage
        jmp CalcFinalDamage

.endproc

; ---------------------------------------------------------------------------

;Attack Type 47 (Tame)
;Param2: Status Duration
;Param3: Status 3
.proc Attack47

_71FB:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$04		;Creature/Beast
        beq Miss
        jsr CalcStatusDuration
        jmp ApplyStatus3
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 48 (Catch)
.proc Attack48

_720D:
        jsr SetupMsgBoxIndexes
        stx $14			;save msg box index for later
        ldx TargetOffset
        lda CharStruct::CmdImmunity,X
        and #$08		;catch
        beq :+
        lda #$4F		;message: immune to catch
        bra Miss
:	LDA CharStruct::Status4,X
        and #$02		;critical hp
        bne Success
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$01		;improved catch
        beq HighHP
        ldx TargetOffset
        longa
        lda CharStruct::MaxHP,X
        lsr
        cmp CharStruct::CurHP,X	;is max hp/2 >= current hp?
        bcs SuccessMode
        shorta0
        bra HighHP
SuccessMode:
        shorta0
        bra Success
HighHP:	LDA #$25		;message: hp too high
Miss:	LDX $14			;message box index
        sta MessageBoxes,X
        inc AtkMissed
        bra Ret
Success:
        jsr CatchMonster
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 49 (Flirt, Lilith Rod)
;Param1: Hit% 
.proc Attack49

_7255:
        jsr HitCalcLevelMEvade
        jsr DanceHitMod
        jsr CheckForHit
        lda AtkMissed
        bne Ret
        jsr ApplyFlirt
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 4A (unused, entry in jump table maps to type 4B)
.proc Attack4A

_7266:
;Attack Type 4B (L5 Doom)
;Param1: Level Mult
;Param3: Status 1
Attack4B:
        jsr CheckLevel
        lda AtkMissed
        bne Ret
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01		;undead	type
        bne Undead
        lda CharStruct::ArmorProperties,X
        and #$02   		;undead armor
        beq Die
Undead:	REP #$20
        lda CharStruct::MaxHP,X
        sta CharStruct::CurHP,X
        shorta0
        rts
        								;
Die:	JSR ApplyStatus1Bypass
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 4C (L2 Old)
;Param1: Level Mult
;Param2: Duration
;Param3: Status 2
.proc Attack4C

_728D:
        jsr CheckLevel
        lda AtkMissed
        bne Ret
        jsr CalcStatusDuration
        jsr ApplyStatus2Bypass
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 4D (L4 Qrter)
;Param1: Level Mult
;Param2: Fraction/16
;Param3: Status 2
.proc Attack4D

_729B:
        jsr CheckLevel
        lda AtkMissed
        bne Ret
        jsr GravityDamage
        lda #$3C		;status duration
        sta Param2
        jsr CalcStatusDuration
        jsr ApplyStatus2Bypass
        stz AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 4E (L3 Flare)
;Param1: Level Mult
;Param3: Element
.proc Attack4E

_72B2:
        jsr CheckLevel
        lda AtkMissed
        bne Miss
        lda Param3
        sta AtkElement
        jsr FlareMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr ElementUpMod
        jsr ElementDamageModMag
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamage
Miss:	LDA MagicNull
        beq Ret
        stz AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 4F (Spirit)
;Param1: Zombie%
;Param3: Fraction/4 HP Recovered
.proc Attack4F

_72DB:
        lda wTargetIndex
        tay
        lda ActiveParticipants,Y
        bne Miss
        jsr ReviveTarget
        lda AtkMissed
        bne Ret
        jsr Random_0_99
        cmp Param1
        bcc Zombie
        inc SpiritFlag
        bra Ret
Zombie:	LDA #$02		;zombie
        sta Param3
        jmp ApplyStatus1
Miss:	INC AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 50 (Goblin Punch)
;Param1: Hit%
;Param3: Status 1
.proc Attack50

_7300:
        jsr HitPhysicalMagic
        lda AtkMissed
        bne Ret
        jsr GoblinDamage
        jsr EqualLevelMod
        jsr TargetStatusModPhys
        jsr CalcFinalDamage
        jsr ApplyStatus1
        stz AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 51 (Modify Level or Defense)
;Param1: 	Hit%
;Param2: 	80h Halve Level
;		40h Halve Level (again)
;		20h Halve Def and MDef
;		10h Add Level
;		08h Subtract Level
;		04h Add Attack
;		02h Add Def and MDef
;Param3:	Stat Mod Amount
.proc Attack51

_7319:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        lda Param2
        bpl :+
        jsr HalveLevel

:	LDA Param2
        and #$40
        beq :+
        jsr HalveLevel
:	LDA Param2
        and #$20
        beq :+
        jsr HalveDefenses
:	LDA Param2
        and #$10
        beq :+
        jsr AddLevel
:	LDA Param2
        and #$08
        beq :+
        jsr SubtractLevel
:	LDA Param2
        and #$04
        beq :+
        jsr AddAttack
:	LDA Param2
        and #$02
        beq Ret
        jsr AddDefenses
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 52 (Mucus)
;Param1: Hit%
;Param2: Allowed Status 3
;Param3: Applied Status 3
.proc Attack52

_735E:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr ApplyStatus3Exclusive
        lda AtkMissed
        bne Ret
        lda #$FF
        sta StatusDuration
        lda #$08		;HP Leak
        sta Param3
        jsr ApplyStatus4
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 53 (Damage % MP)
;Param1: Hit%
;Param2: Fraction/16
.proc Attack53

_7379:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr CalcDamageTargetCurMP
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 54 (Damage = Max HP - Current HP)
;Param1: Hit%
.proc Attack54

_7384:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr CalcDamageAttackerDiffHP
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 55 (Fusion)
;Param3: Status 1
.proc Attack55

_738F:
        jsr FullCureTarget
        jsr FullMPHeal
        jmp ApplyStatus1AttackerBypass

.endproc

; ---------------------------------------------------------------------------

;Attack Type 56 (Unused?)
;Param1: Hit%
;Param3: Status 4
.proc Attack56

_7398:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr ToggleStatus4
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 57 (HP Leak + Status)
;Param1: Status 1
;Param2: Duration
;Param3: Status 2
.proc Attack57

_73A3:
        jsr CalcStatusDuration
        jsr ApplyStatus2
        stz AtkMissed
        lda Param1
        sta Param3
        jsr ApplyStatus1
        stz AtkMissed
        lda #$FF
        sta StatusDuration
        lda #$08		;HP Leak
        sta Param3
        jmp ApplyStatus4

.endproc

; ---------------------------------------------------------------------------

;Attack Type 58 (Mind Blast)
;Param1: Hit%
;Param2: Spell Power
;Param3: Status Duration
.proc Attack58

_73C0:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr FlareMagicDamage
        jsr MultiTargetMod
        jsr TargetStatusModMag
        jsr CalcFinalDamage
        lda Param3
        sta Param2
        jsr CalcStatusDuration
        lda #$20		;Paralyze
        sta Param3
        jsr ApplyStatus2
        stz AtkMissed
        lda #$FF
        sta StatusDuration
        lda #$08		;HP Leak
        sta Param3
        jsr ApplyStatus4
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 59 (Giant Drink)
.proc Attack59

_73F0:
        lda wTargetIndex
        cmp #$04		;<4 means party
        bcs Miss
        lda wTargetIndex
        tax
        lda GiantDrink,X
        bne Miss
        inc GiantDrink,X
        jmp DoubleMaxHP
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 5A (White Wind)
.proc Attack5A

_7407:
        ldx CurrentHP
        stx HealingToTarget
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 5B (Unused)
;Param3: Bits from this are set in CharStruct.2079 (unused) on Attacker
.proc Attack5B

_740E:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr AddSomethingUnused
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 5C (Hug)
;Param1: Hit%
;Param3: Status 1
.proc Attack5C

_7419:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr FullCureTarget
        jsr ApplyStatus1
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 5D (Dance of the Dead, Zombie Powder)
;Param1: Hit%
;Param2: Fraction/16 HP (only relevant if immune to Zombie)
.proc Attack5D

_7427:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        jsr ReviveTarget
        lda AtkMissed
        bne Ret
        lda #$02		;Zombie
        sta Param3
        jsr ApplyStatus1
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 5E (Song magic)
;Param3: Song
.proc Attack5E

_743D:
        jsr CheckVoid
        lda AtkMissed
        bne Ret
        ldx AttackerOffset
        lda CharStruct::Status4,X
        ora #$04		;Singing
        sta CharStruct::Status4,X
        lda Param3
        sta CharStruct::Song,X
        lda #$08
        tax
        lda wTargetIndex
        jsr StartTimer
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 5F (Requiem)
;Param1: Creature Type
;Param2: Spell Power
;Param3: HP Leak Duration
.proc Attack5F

_745C:
        jsr CheckCreatureType
        lda AtkMissed
        bne Ret
        jsr FlareMagicDamage
        jsr CalcFinalDamage
        lda Param3
        sta StatusDuration
        lda #$08		;HP Leak
        sta Param3
        jsr ApplyStatus4
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 60 (Hide and/or Show Monsters)
;Used by fight scripts to change battle formations
.proc Attack60

_7476:
        tdc
        tax
FindCommand:			;look for first $F2 flagged entry in table of queued commands
        lda GFXQueue::Cmd,X
        cmp #$F2
        beq Found
        inx 			;command structure is 5 bytes long
        inx
        inx
        inx
        inx
        bra FindCommand
Found:	
        lda GFXQueue::Type,X
        sta $11			;80h: copy attacker's hp to target
        lda GFXQueue::Data1,X
        sta $10			;bitmask of monsters to show or hide
        tdc
        tax
        stx $0E			;loop index (monsters)
MonsterLoop:	
        ldx $0E
        lda MonstersVisible
        jsr SelectBit_X
        beq NotVisible
        lda $10
        jsr SelectBit_X
        beq HideMonster
        jmp Next
HideMonster:	
        tdc
        jsr SetBit_X
        ora InactiveMonsters
        sta InactiveMonsters
        txa
        clc
        adc #$04		;+4 to account for party slots
        tax
        stz ActiveParticipants,X
        jmp Next
NotVisible:	
        lda $10
        jsr SelectBit_X
        bne ShowMonster
        jmp Next
ShowMonster:	
        tdc
        jsr SetBit_X
        ora MonstersVisible
        sta MonstersVisible
        lda InactiveMonsters
        jsr ClearBit_X
        sta InactiveMonsters
        txa
        clc
        adc #$04
        tax
        inc ActiveParticipants,X
        longa
        clc
        lda $0E
        adc #$0004
        jsr ShiftMultiply_128
        tax
        stx $14			;Target Monster CharStruct offset
        shorta0
        stz CharStruct::Status1,X	;clear all status
        stz CharStruct::Status2,X
        stz CharStruct::Status3,X
        stz CharStruct::Status4,X
        clc
        lda $0E
        adc #$04
        jsr GetTimerOffset	;Y = timer offset
        tdc 			;disable all timers
        sta EnableTimer+0,Y
        sta EnableTimer+1,Y
        sta EnableTimer+2,Y
        sta EnableTimer+3,Y
        sta EnableTimer+4,Y
        sta EnableTimer+5,Y
        sta EnableTimer+6,Y
        sta EnableTimer+7,Y
        sta EnableTimer+8,Y
        sta EnableTimer+9,Y
        lda $11
        bmi MaxHP
        longa
        ldx AttackerOffset
        lda CharStruct::CurHP,X	;copy attacker's HP
        ldx $14
        sta CharStruct::CurHP,X	;to target's HP
        cmp CharStruct::MaxHP,X
        bcc :+
        lda CharStruct::MaxHP,X
        sta CharStruct::CurHP,X	;cap at Max HP
:	TDC
        shorta
        bra Next
MaxHP:
        longa
        ldx $14			;Target Monster CharStruct offset
        lda CharStruct::MaxHP,X
        sta CharStruct::CurHP,X	;Full HP
        shorta0
Next:		
        inc $0E
        lda $0E
        cmp #$08		;8 monsters to check
        beq :+
        jmp MonsterLoop
:	INC UnknownReaction
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 61 (Stalker and Sandworm)
;Handles special fight gimmicks for Stalker and Sandworm, flagging which targets are valid to attack
.proc Attack61

_7562:
        lda SandwormBattle
        bne _SandwormBattle
        tdc
        tay
        ldx #$0200		;hardcoded CharStruct offset, first monster
MonsterLoop:			;set false image on all monsters	
        lda CharStruct::Status4,X
        ora #$40		;false image
        sta CharStruct::Status4,X
        jsr NextCharOffset
        iny
        cpy #$0008
        bne MonsterLoop
RandomMonster:			;randomly pick monster slots until we find one that exists
        ldx #$0004
        lda #$0B
        jsr Random_X_A  	;4..11
        tay
        lda ActiveParticipants,Y
        beq RandomMonster
        tya
        longa
        jsr ShiftMultiply_128	;convert to CharStruct Offset
        tax
        shorta0
        lda CharStruct::Status4,X
        and #$BF		;clear false image
        sta CharStruct::Status4,X
        tdc
        tay
        ldx #$0200		;hardcoded CharStruct offset, first monster
        longa
        lda CharStruct::CurHP,X
        sta $0E
FindHighHP:			;finds highest monster HP of first 4 monsters	
        lda CharStruct::CurHP,X
        cmp $0E
        bcs :+
        sta $0E
:	CLC
        txa
        adc #$0080		;next character
        tax
        iny
        cpy #$0004
        bne FindHighHP
        tdc
        tay
        ldx #$0200		;hardcoded CharStruct offset, first monster
SetHP:				;sets monster 0-3 HP to the highest found	
        lda $0E
        sta CharStruct::CurHP,X
        clc
        txa
        adc #$0080		;next character
        tax
        iny
        cpy #$0004
        bne SetHP
        shorta0
        rts
_SandwormBattle:									;
        tdc
        tax
FindCommand:
        lda GFXQueue::Cmd,X
        cmp #$F2
        beq Found
        inx 			;5 byte structure
        inx
        inx
        inx
        inx
        bra FindCommand
Found:	STX $10			;GFXQueue table index
        tdc
        tay
        ldx #$0200		;hardcoded CharStruct offset, first monster
MonsterLoop6:			;set false image on first 6 monsters
        lda CharStruct::Status4,X
        ora #$40		;false image
        sta CharStruct::Status4,X
        jsr NextCharOffset
        iny
        cpy #$0006
        bne MonsterLoop6
        ldx #$0007
        lda #$09
        jsr Random_X_A  	;Random 7..9
        sta $0E
        longa
        jsr ShiftMultiply_128	;convert to CharStruct Offset
        tax
        shorta0
        lda CharStruct::Status4,X
        and #$BF		;clear false image
        sta CharStruct::Status4,X
        stz ActiveParticipants+7	;worm spots all inactive
        stz ActiveParticipants+8
        stz ActiveParticipants+9
        inc ActiveParticipants+4	;holes all active
        inc ActiveParticipants+5
        inc ActiveParticipants+6
        lda $0E		;7..9
        tax
        inc ActiveParticipants,X	;worm's spot is active
        sec
        sbc #$07
        tax 		;0..2
        lda #$E0	;first 3 bits set
        jsr ClearBit_X
        sta $12		;2 bits set for active holes
        sec
        lda $0E		;7..9
        sbc #$03	;4..6
        tax
        stz ActiveParticipants,X	;worm's hole is inactive
        sec
        lda $0E		;7..9
        sbc #$04	;3..5
        tax
        lda $12		;2 bits set for active holes
        jsr SetBit_X	;sets bit for active worm
        ldx $10		;GFXQueue table index
        sta GFXQueue::Data1,X
        lda #$02	;2nd monster active
        sta MonsterKillTracker
        lda #$FD	;inverse of above
        sta InactiveMonsters
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 62 (Library Book Monster Swap)
;hides the user, unhides the next monster in the formation
;**bug: I think the new monster is set as revivable while also being alive
;	pretty harmless tho because the primary revive routine checks if the target is dead first
.proc Attack62

_7662:
        tdc
        tax
FindCommand:	
        lda GFXQueue::Cmd,X
        cmp #$F2
        beq Found
        inx
        inx
        inx
        inx
        inx
        bra FindCommand
Found:	SEC
        lda AttackerIndex
        sbc #$04		;convert to monster index
        inc 			;next monster
        sta $0E
        asl
        tay
        lda BattleMonsterID,Y
        and BattleMonsterID+1,Y
        cmp #$FF
        beq Miss		;nothing in this slot, abort
        phx
        lda $0E
        tax
        tdc
        jsr SetBit_X		;bit set for new monster
        plx
        sta GFXQueue::Data1,X
        lda $0E			;monster index for new monster
        tax
        dex 			;index for original monster
        tdc
        jsr SetBit_X		;bit set for original monster
        ora InactiveMonsters
        sta InactiveMonsters	;not revivable
        txa
        clc
        adc #$04
        tax
        stz ActiveParticipants,X	;no longer active
        lda $0E
        tax
        tdc
        jsr SetBit_X
        ora MonstersVisible
        sta MonstersVisible	;new monster visible
        lda InactiveMonsters
        jsr ClearBit_X		;also revivable? *bug?
        sta InactiveMonsters
        txa
        clc
        adc #$04
        tax
        inc ActiveParticipants,X	;new monster active
        bra :+
Miss:	INC AtkMissed
:	INC UnknownReaction
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 63 (Grand Cross)
;**optimize: could probably be made much smaller by jumping to a pointer (~27 bytes)
.proc Attack63

_76CE:
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$C2		;select for dead/stone/zombie
        beq :+
        rts
        								;
:	LDA #$01
        sta StatusFixedDur
        tdc
        tax
        lda #$11		;17
        jsr Random_X_A 		;0..17
        bne :+
        lda #$80		;0: Dead
        bra Status1
:	DEC
        bne :+
        lda #$40		;1: Stone
        bra Status1
:	DEC
        bne :+
        lda #$20		;2: Toad
        bra Status1
:	DEC
        bne :+
        lda #$10		;3: Mini
        bra Status1
:	DEC
        bne :+
        lda #$04		;4: Poison
        bra Status1
:	DEC
        bne :+
        lda #$02		;5: Zombie
        bra Status1
:	DEC
        bne :+
        lda #$01		;6: Blind
Status1:
        sta Param3
        jsr ApplyStatus1
        bra Finish
:	DEC
        bne :+
        lda #$80		;7: Old
        bra Status2
:	DEC
        bne :+
        lda #$40		;8: Sleep
        bra Status2
:	DEC
        bne :+
        lda #$20		;9: Paralyze
        bra Status2
:	DEC
        bne :+
        lda #$10		;10: Charm
        bra Status2
:	DEC
        bne :+
        lda #$08		;11: Berserk
        bra Status2
:	DEC
        bne :+
        lda #$04		;12: Mute
Status2:
        sta Param3
        jsr ApplyStatus2
        bra Finish
:	DEC
        bne :+
        lda #$04		;13: Slow
        bra Status3
:	DEC
        bne :+
        lda #$04		;14: Slow
Status3:
        sta Param3
        jsr ApplyStatus3
        bra Finish
:	DEC
        bne :+
        lda #$10		;15: Countdown
        bra Status4
:	DEC
        bne :+
        lda #$08		;16: HP Leak
Status4:
        sta Param3
        jsr ApplyStatus4
        bra Finish
:	JSR SetHPCritical	;17: HP Critical
Finish:	STZ AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 64 (Chicken Knife)
;Param2/3: Proc% and Proc, not handled here
.proc Attack64

_7774:
        jsr SetHit100andHalfTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr ChickenDamage
        jsr BackRowMod
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr MagicSwordMod
        lda TargetDead
        bne Ret
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamageMSword
        jmp ApplyMSwordStatus
Miss:	LDA #$80
        sta AtkMissed
        rts
Ret:	rts 			;**optimize, get rid of this

.endproc

; ---------------------------------------------------------------------------

;Attack Type 65 (Interceptor Rocket)
.proc Attack65

_77A4:
        ldx TargetOffset
        lda CharStruct::CmdStatus,X
        and #$10   		;jumping
        beq Miss
        lda #$50		;command $50 (forced landing)
        sta CharStruct::Command,X
        rts
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 66 (Targetting)
.proc Attack66

_77B6:
        sec
        lda AttackerIndex
        sbc #$04		;now monster index
        asl
        tay
        tdc
        sta ForcedTarget::Party,Y
        sta ForcedTarget::Monster,Y
        lda wTargetIndex
        cmp #$04
        bcs Monster
        tax
        tdc
        jsr SetBit_X
        sta ForcedTarget::Party,Y
        rts
;
Monster:
        sec
        sbc #$04
        tax
        tdc
        jsr SetBit_X
        sta ForcedTarget::Monster,Y
        rts
        							;
        rts 			;**optimize: not needed

.endproc

; ---------------------------------------------------------------------------

;Attack Type 67 (Pull) and Attack Type 68 (Terminate)
;Somewhat oddly, Attack 67 jumps into the middle of Attack 68
.proc Attack68

_77E0:
        lda #$20
        sta BattleOver

.endproc

; ---------------------------------------------------------------------------

.proc Attack67

_77E5:
        inc UnknownReaction
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 69 (Control)
;**optimize: could save some bytes rearranging this for branches to reach
.proc Attack69

_77E9:
        jsr SetupMsgBoxIndexes	;sets Y=MessageBoxData index
        stx $14			;MessageBoxes index
        ldx TargetOffset
        lda CharStruct::Status2,X
        and #$18     		;Charm/Berserk
        beq :+
        jmp Immune
:	LDA CharStruct::Status4,X
        and #$20     		;Controlled
        beq :+
        jmp Already
:	LDA CharStruct::CmdImmunity,X
        and #$10		;Control Immunity
        beq :+
        jmp Immune
:	JSR Random_0_99
        sta $0E			;0.99
        ldx AttackerOffset
        lda CharStruct::Headgear,X
        cmp #$CB    		;Hardcoded Coronet id
        bne :+
        lda $0E
        cmp #$4B    		;75% with coronet
        bcc Success
        bra MissJ
:	LDA $0E
        cmp #$28    		;40% without
        bcc Success
MissJ:	JMP Miss
Success:
        ldx TargetOffset
        lda #$80
        sta CharStruct::ActionFlag,X
        stz CharStruct::Command,X
        lda CharStruct::Status4,X
        ora #$20     		;Controlled
        sta CharStruct::Status4,X
        lda AttackerIndex
        tax
        stz ControlCommand,X
        lda wTargetIndex
        sta ControlTarget,X
        lda AttackerIndex
        tax
        lda ROMTimes20,X	;*20
        tay
        sty $10			;attacker index * structure size
        sec
        lda wTargetIndex
        sbc #$04		;now monster index
        asl
        tax
        longa
        lda BattleMonsterID,X
        jsr ShiftMultiply_4
        tax
        shorta0
        stz $0E			;loop index
CopyActionsLoop:
        lda ROMControlActions,X
        sta CharControl::Actions,Y
        cmp #$FF
        bne :+
        lda #$80
        bra :++
:	TDC
:	STA CharControl::Flags,Y
        inx
        iny
        inc $0E
        lda $0E
        cmp #$04
        bne CopyActionsLoop
        stz $0E			;loop index
        ldy $10			;attacker index * structure size
CopyTargettingLoop:
        lda CharControl::Actions,Y
        longa
        jsr ShiftMultiply_8
        tax
        shorta0
        lda ROMMagicInfo::Targetting,X
        sta CharControl::Targetting,Y
        iny
        inc $0E
        lda $0E
        cmp #$04
        bne CopyTargettingLoop
        ldx $14			;MessageBoxes index
        lda #$24		;success message
        sta MessageBoxes,X
        rts
Immune:									;
        lda #$4E		;can't control message
        bra :+
Already:
        lda #$4C		;already controlled message
:	LDX $14
        sta MessageBoxes,X
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 6A (Win Battle)
;**optimize: loop here could save space
.proc Attack6A

_78BC:
        stz ActiveParticipants+4	;these are the monster slots
        stz ActiveParticipants+5
        stz ActiveParticipants+6
        stz ActiveParticipants+7
        stz ActiveParticipants+8
        stz ActiveParticipants+9
        stz ActiveParticipants+10
        stz ActiveParticipants+11
        lda #$80			;Enemies Dead
        sta BattleOver
        inc UnknownReaction
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 6B (Grant Immunity or Creature Type)
;Param1: Creature Type
;Param2: Status 1 Immunity
;Param3: Element Up
.proc Attack6B

_78DD:
        jsr AddElementUp
        lda Param2
        sta Param3
        jsr AddStatus1Immunity
        lda Param1
        sta Param3
        jmp AddCreatureType

.endproc

; ---------------------------------------------------------------------------

;Attack Type 6C (Strong vs. Creature Type)
;Param2: Spell Power
;Param3: Creature Type
.proc Attack6C

_78EE:
        jsr NormalMagicDamage
        jsr CreatureMod
        jsr TargetStatusModMag
        jmp CalcFinalDamage

.endproc

; ---------------------------------------------------------------------------

;Attack Type 6D (Vampire)
;Param1: Hit%
.proc Attack6D

_78FA:
        jsr HitMagic
        lda AtkMissed
        bne Ret
        longa
        ldx AttackerOffset
        sec
        lda CharStruct::MaxHP,X
        sbc CharStruct::CurHP,X
        lsr
        sta Attack
        tdc
        sta Defense
        inc
        sta M
        shorta
        jsr DrainDamage
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 6E (Brave Blade)
.proc Attack6E

_791B:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr BraveDamage
        jsr BackRowMod
        jsr CommandMod
        jsr DoubleGripMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr MagicSwordMod
        lda TargetDead
        bne Ret
        lda AtkMissed
        bne Miss
        jsr CalcFinalDamageMSword
        jmp ApplyMSwordStatus
Miss:	LDA #$80
        sta AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 6F (Strong Fight)
.proc Attack6F

_794D:
        jsr StrongFightDamage
        jsr TargetStatusModPhys
        jmp CalcFinalDamage

.endproc

; ---------------------------------------------------------------------------

;Attack Type 70 (Wormhole)
;Param3: Status 4
.proc Attack70

_7956:
        lda wTargetIndex
        tax
        lda ActiveParticipants,X
        bne Miss			;miss on living members
        jmp ApplyStatus4
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 71 (Next Monster Form)
;Looks for 4 specific encounters, does nothing in other fights
;Actually, there's only one battle in that table and it's ArchaeAvis
;I'm also fairly sure this isn't used at all, even for that fight
;
;**optimize: 	Specialty data at the end doesn't need temp vars, could loop clearing timers, could trim from address setup
;		TempMStats loads are offset by X but X is always zero..
;		We could probably bypass the whole temp stat structure entirely by calculating the rom address directly
;		Also, this is probably completely unused, just delete it instead
.proc Attack71

_7964:
        longa
        tdc
        tax
        tay
CheckEncounterLoop:
        lda ROMMonsterFormData::Encounter,X
        cmp EncounterIndex
        beq SpecialEncounter
        clc
        txa
        adc #$0010
        tax
        iny
        cpy #$0004
        bne CheckEncounterLoop
        shorta0
        rts
SpecialEncounter:	;note we start in 16 bit mode here		;and I had to manually fix this disassembly
        .a16							;
        clc
        txa
        adc #$FFA0	;hardcoded address of ROMMonsterFormData
        sta $0E
        shorta0
        lda #$D0
        sta $10		;$0E now holds the 24 bit address $D0FFA0,X
LoadNewFormID:	
        lda MonsterNextForm
        asl
        tay
        lda [$0E],Y	;load new monster ID
        sta $12
        iny
        lda [$0E],Y
        sta $13
        lda $12
        ora $13
        bne HasData
NoData:			;data being all 0 indicates we've gone past the last form
        lda MonsterNextForm
        dec
        bne SetForm1	;if next form was any form but 1, set form 1
        rts 		;if next form was 1, abort now
        		;(because something has gone wrong, I think)	
SetForm1:
        lda #$01
        sta MonsterNextForm	;set next form to 1 then
        bra LoadNewFormID	;try loading new monster form again
HasData:	
        inc MonsterNextForm
        lda #$D0
        sta $10
        ldy #$0000
        sty $0E		;$D00000, normal monster stats
        lda $13
        beq NotBoss
        lda #$D0
        sta $10
        ldy #$2000
        sty $0E		;$D02000, boss monster stats
NotBoss:
        lda $12
        longa
        jsr ShiftMultiply_32
        tay
        tdc
        tax
        shorta
        stz $11
CopyMonsterStatsLoop:
        lda [$0E],Y
        sta TempMStats,X
        iny
        inx
        inc $11
        lda $11
        cmp #$20
        bne CopyMonsterStatsLoop
        lda AttackerIndex
        jsr GetTimerOffset		;puts timer offset in Y
        tdc
        sta EnableTimer+0,Y		;clear timers
        sta EnableTimer+1,Y
        sta EnableTimer+2,Y
        sta EnableTimer+3,Y
        sta EnableTimer+4,Y
        sta EnableTimer+5,Y
        sta EnableTimer+6,Y
        sta EnableTimer+7,Y
        sta EnableTimer+8,Y
        sta EnableTimer+9,Y
        ldy AttackerOffset
        tdc
        sta CharStruct::Status1,Y	;clear status
        sta CharStruct::Status2,Y
        sta CharStruct::Status3,Y
        sta CharStruct::Status4,Y
        tax
        lda TempMStats::Speed,X
        sta CharStruct::BaseAgi,Y
        sta CharStruct::EquippedAgi,Y
        lda TempMStats::AttackPower,X
        sta CharStruct::MonsterAttack,Y
        lda TempMStats::AttackMult,X
        sta CharStruct::MonsterM,Y
        lda TempMStats::Evade,X
        sta CharStruct::Evade,Y
        lda TempMStats::Defense,X
        sta CharStruct::Defense,Y
        lda TempMStats::MagicPower,X
        sta CharStruct::BaseMag,Y
        sta CharStruct::EquippedMag,Y
        lda TempMStats::MDefense,X
        sta CharStruct::MDefense,Y
        lda TempMStats::MEvade,X
        sta CharStruct::MEvade,Y
        longa
        lda TempMStats::HP,X
        sta CharStruct::CurHP,Y
        sta CharStruct::MaxHP,Y
        lda TempMStats::MP,X
        sta CharStruct::CurMP,Y
        lda #$270F			;monsters always have 9999 max mp
        sta CharStruct::MaxMP,Y
        lda TempMStats::Exp,X
        sta CharStruct::RewardExp,Y
        lda TempMStats::Gil,X
        sta CharStruct::RewardGil,Y
        lda TempMStats::StatusImmune1,X
        sta CharStruct::StatusImmune1,Y	;also copies StatusImmune2
        shorta0
        lda TempMStats::StatusImmune3,X
        sta CharStruct::StatusImmune3,Y
        lda TempMStats::AttackFX,X
        sta CharStruct::RHWeapon,Y
        lda TempMStats::EAbsorb,X
        sta CharStruct::EAbsorb,Y
        lda TempMStats::EImmune,X
        sta CharStruct::EImmune,Y
        lda TempMStats::CantEvade,X
        sta CharStruct::CantEvade,Y
        lda TempMStats::EWeak,X
        sta CharStruct::EWeak,Y
        lda TempMStats::CreatureType,X
        sta CharStruct::CreatureType,Y
        lda TempMStats::CmdImmunity,X
        sta CharStruct::CmdImmunity,Y
        lda TempMStats::Level,X
        sta CharStruct::Level,Y
        sta CharStruct::EquippedVit,Y
        phy
        longa
        lda $12
        asl
        tax
        shorta0
        lda ROMSpecialtyData::Properties,X
        sta $1C
        lda ROMSpecialtyData::Name,X
        sta $1D
        ply
        lda $1C
        sta CharStruct::Specialty,Y
        lda $1D
        sta CharStruct::SpecialtyName,Y
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 72 (Bows Strong vs. Creature)
;Param1: Creature Type
.proc Attack72

_7AD9:
        jsr SetHit100andHalfTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr KnifeDamage
        jsr CommandMod
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr CheckCreatureCrit
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 73 (Spears Strong vs. Creature)
;Param1: Creature Type
;if not(!_CombatTweaks)
.proc Attack73

_7AFA:
        jsr SetHit100andTargetEvade
        jsr HitPhysical
        lda AtkMissed
        bne Miss
        jsr SwordDamage
        jsr CommandMod
        jsr CheckJump
        jsr TargetStatusModPhys
        jsr AttackerStatusModPhys
        jsr CheckCreatureCrit
        jmp CalcFinalDamage
Miss:	LDA #$80
        sta AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Attack Type 74 (Miss), Attack Type 7E also mapped here
.proc Attack74

_7B1E:
        inc AtkMissed
;Attack type 75 (Do Nothing), Attack Type 7F also mapped here	
Attack75:
        rts

.endproc

; ---------------------------------------------------------------------------

.org $C27B21
.proc AtkTypeJumpTable

_7B21:
.word $686C, $6870, $6898, $68E2, $68F7, $690C, $6921, $694A, $6971, $6993, $69B5, $69DB, $6A07, $6A3C, $6A65
.word $6A76, $6A81, $6A8E, $6AC4, $6ACF, $6ADD, $6AEB, $6AF6, $6B01, $6B32, $6B35, $6B52, $6B7D, $6BFC, $6C17
.word $6CE4, $6CEF, $6D2E, $6D4B, $6D5B, $6D83, $6DAB, $6DB4, $6DBD, $6DD7, $6DED, $6E12, $6E2D, $6E4C, $6E5E
.word $6E72, $6E9B, $6EB1, $6EBC, $6EE1, $6F1E, $6F58, $6F84, $6FA8, $6FC9, $6FF9, $7020, $703E, $7053, $7074
.word $709D, $70C4, $70DB, $70EC, $7119, $7134, $7140, $7150, $71B9, $71D7, $71F5, $71FB, $720D, $7255, $7266
.word $7266, $728D, $729B, $72B2, $72DB, $7300, $7319, $735E, $7379, $7384, $738F, $7398, $73A3, $73C0, $73F0
.word $7407, $740E, $7419, $7427, $743D, $745C, $7476, $7562, $7662, $76CE, $7774, $77A4, $77B6, $77E5, $77E0
.word $77E9, $78BC, $78DD, $78EE, $78FA, $791B, $794D, $7956, $7964, $7AD9, $7AFA, $7B1E, $7B20

.endproc
.reloc

; ---------------------------------------------------------------------------

;(Hit = Attacker Level + Hit - Target Level, Evade = Magic Evade)
.org $7C0D
.proc HitCalcLevelMEvade

_7C0D:
        ldx AttackerOffset
        clc
        lda Param1
        adc Level
        sta $0E
        ldx TargetOffset
        clc
        lda CharStruct::Level,X
        adc CharStruct::BonusLevel,X
        sta $0F
        sec
        lda $0E
        sbc $0F
        bcs :+
        lda #$01
        bra :++
:	CMP #$63
        bcc :+
        lda #$63
:	STA HitPercent
        lda CharStruct::MEvade,X
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------

;Hit=Param1, Evade=Target Evade
.proc SetHitParam1andTargetEvade

_7C3B:
        lda Param1
        sta HitPercent
        ldx TargetOffset
        lda CharStruct::Evade,X
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------

;(Hit = 100, Evade = Evade%)
.org $7C47
.proc SetHit100andTargetEvade

_7C47:
        lda #$64
        sta HitPercent
        ldx TargetOffset
        lda CharStruct::Evade,X
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------


;Hit=100%, Evade=Target Evade/2
.proc SetHit100andHalfTargetEvade

_7C53:
        lda #$64	;100
        sta HitPercent
        ldx TargetOffset
        lda CharStruct::Evade,X
        lsr
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------

;Hit=Param1, Evade=Target Evade
;**optimize: this routine is identical to SetHitParam1andTargetEvade
.proc SetHitParam1andTargetEvade_Dupe

_7C60:
        lda Param1
        sta HitPercent
        ldx TargetOffset
        lda CharStruct::Evade,X
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------

;Hit=Param1
;Evade=Target Evade/2
.proc SetHitParam1andHalfTargetEvade

_7C6C:
        lda Param1
        sta HitPercent
        ldx TargetOffset
        lda CharStruct::Evade,X
        lsr
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------

;Hit=Param1
;Evade=Target Magic Evade
.proc SetHitParam1andTargetMagicEvade

_7C79:
        lda Param1
        sta HitPercent
        ldx TargetOffset
        lda CharStruct::MEvade,X
        sta EvadePercent
        rts

.endproc

; ---------------------------------------------------------------------------

;Steal Success Determination
.proc HitCalcSteal

_7C85:
        lda Param1
        sta HitPercent
        stz EvadePercent
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$10   		;improved steal
        beq :+
        asl HitPercent
:	rts

.endproc

; ---------------------------------------------------------------------------


;Aegis Shield Check
.proc CheckAegis

_7C97:
        ldx TargetOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$70   		;sleep/charm/paralyze
        bne Return
        lda CharStruct::Status3,X
        and #$10   		;stop
        bne Return
        lda CharStruct::ArmorProperties,X
        bpl Return
        jsr Random_0_99
        cmp #$21 		;33
        bcs Return
        lda AttackerIndex
        cmp wTargetIndex
        beq Return
        lda #$07
        sta ShieldBlock
        inc AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Multitargetting effect on Hit%
;
;**optimize: we don't need to sort the bits if we're just counting them
.proc MultiTargetHitPercent

_7CC3:
        lda AttackerOffset2
        tax
        lda AttackInfo::Targetting,X
        and #$40    		;hits all
        bne Return
        lda MultiCommand
        asl
        tax
        lda TargetBitmask,X
        and #$F0
        bne :+			;party targetted
        lda TargetBitmask,X
        and #$0F
        jsr ShiftMultiply_16
        sta $0E
        lda TargetBitmask+1,X
        and #$F0
        jsr ShiftDivide_16
        ora $0E			;A now contains monsters in order
:	JSR CountSetBits	;result in X
        dex
        beq Return		;if only one target
        lsr HitPercent
        lda HitPercent
        bne Return
        inc HitPercent
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Check for Evade, Weapon Block or Elf Cape
.proc CheckSpecialEvade

_7CFC:
        ldx TargetOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$70
        bne Return
        lda CharStruct::Status3,X
        and #$10
        bne Return
        lda CharStruct::Passives1,X
        and #$40
        beq :+
        jsr Random_0_99
        cmp #$19
        bcs :+
        lda AttackerIndex
        cmp wTargetIndex
        beq :+
        lda #$05
        sta BladeGrasp
        inc AtkMissed
        rts
:
        ldx TargetOffset
        lda CharStruct::WeaponProperties,X
        bpl :+
        jsr Random_0_99
        cmp #$19
        bcs :+
        lda AttackerIndex
        cmp wTargetIndex
        beq :+
        lda #$01
        sta SwordBlock
        inc AtkMissed
        rts
:							
        ldx TargetOffset
        lda CharStruct::WeaponProperties,X
        and #$40
        beq :+
        jsr Random_0_99
        cmp #$19
        bcs :+
        lda AttackerIndex
        cmp wTargetIndex
        beq :+
        lda #$02
        sta KnifeBlock
        inc AtkMissed
        rts
:									;Check for Elf Cape
        ldx TargetOffset
        lda CharStruct::ArmorProperties,X
        and #$40
        beq Return
        jsr Random_0_99
        cmp #$21
        bcs Return
        lda AttackerIndex
        cmp wTargetIndex
        beq Return
        lda #$03
        sta ElfCape
        inc AtkMissed
Return:
        rts

.endproc

; ---------------------------------------------------------------------------

;Dance Hit Modifier
.proc DanceHitMod

_7D7F:
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$04   		;dance up
        beq Ret
        asl HitPercent
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Target Status Effect Modifiers to Magical Hit%
.proc TargetMHitMod

_7D8B:
        ldx TargetOffset
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        and #$20    			;shell
        beq :+
        lsr HitPercent
        lda HitPercent
        bne :+
        inc HitPercent
:	LDA CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$20    			;toad
        beq Return
        stz EvadePercent
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Target Status Effect Modifiers to Physical Hit%
.proc TargetPHitMod

_7DAC:
        ldx TargetOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$20     			;toad
        beq :+
        stz EvadePercent
:
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$10     			;mini
        beq Return
        asl EvadePercent
        lda EvadePercent
        cmp #$63
        bcc Return
        lda #$63
        sta EvadePercent
Return:
        rts

.endproc

; ---------------------------------------------------------------------------


;Attacker Status Effect Modifiers to Physical Hit%
.proc AttackerStatusPHitMod

_7DD1:
        ldx AttackerOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$01
        beq Return
        lsr HitPercent
        lsr HitPercent
Return:
        rts

.endproc

; ---------------------------------------------------------------------------

;Check for target Image
.proc CheckTargetImage

_7DE2:
        ldx TargetOffset
        lda CharStruct::Status2,X
        and #$03
        beq Return
        jsr LoseOneImage
        inc AtkMissed
Return:	
        rts

.endproc

; ---------------------------------------------------------------------------

;Check for Void
.proc CheckVoid

_7DF1:
        lda Void
        and #$40    		;void
        beq Ret
        inc AtkMissed
        jsr SetupMsgBoxIndexes
        lda #$1F		;message to display
        sta MessageBoxes,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Check for Float
.proc CheckFloat

_7E03:
        ldx TargetOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$08   	;Float
        beq Ret
        inc AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Check for Hit
;Rolls checks against HitPercent($4E) and EvadePercent($4F)
.proc CheckForHit

_7E12:
        jsr Random_0_99
        cmp HitPercent
        bcs :+
        jsr Random_0_99
        cmp EvadePercent
        bcs Return
:	INC AtkMissed
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Check for Physical Hit
.proc CheckPHit

_7E23:
        jsr Random_0_99
        cmp HitPercent
        bcs Miss
        ldx TargetOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$70
        bne Return
        lda CharStruct::Status3,X
        and #$10
        bne Return
        jsr Random_0_99
        cmp EvadePercent
        bcs Return
        ldx TargetOffset
        lda CharStruct::RHShield,X
        ora CharStruct::LHShield,X
        beq Miss
        lda AttackerIndex
        cmp wTargetIndex
        beq Return
        lda #$06
        sta ShieldBlock
Miss:
        inc AtkMissed
Return:
        rts

.endproc

; ---------------------------------------------------------------------------


;Check for hit by Creature Type	
.proc CheckCreatureType

_7E5B:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and Param1
        bne Ret
        inc AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Hit if Target Level = Multiple of Parameter 1
.proc CheckLevel

_7E67:
        ldx TargetOffset
        lda CharStruct::Level,X
        tax
        stx Dividend
        lda Param1
        tax
        stx Divisor
        jsr Division
        ldx Remainder
        beq Ret
        inc AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Hit% determination for Magic, autohits in some cases
;Param1 is Hit%, high bit means autohit
;Param2 high bit means autohit unless it's a Monster attacking Party
.proc HitMagicConditionalAutohit

_7E81:
        lda Param1
        bmi Return
        lda Param2
        and #$80
        beq :++
        lda AttackerIndex
        cmp #$04
        bcs :+		;4 and up means monster	attacker
        lda wTargetIndex
        cmp #$04
        bcc Return	;autohits if attacker and target are both party
        ;attacker or target is monster
:	LDA wTargetIndex
        cmp #$04
        bcs Return 	;autohits if target is monster
        ;param2/$58 did not have high bit set
        ;or it's a monster attacking the party
:	LDA AttackerOffset2
        tax
        lda AttackInfo::Category,X
        ldx TargetOffset
        and CharStruct::CantEvade,X
        bne Return
        jsr HitCalcLevelMEvade
        jsr CheckAegis
        lda AtkMissed
        bne Return
        jsr MultiTargetHitPercent
        jsr TargetMHitMod
        jsr CheckForHit
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Hit% Determination for physical						
.proc HitPhysical

_7EBE:
        ldx TargetOffset
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$70
        bne :+
        ldx AttackerOffset
        lda CharStruct::DamageMod,X
        bmi :+
        lda AttackerOffset2
        tax
        lda AttackInfo::Category,X
        ldx TargetOffset
        and CharStruct::CantEvade,X
        bne :+
        jsr CheckSpecialEvade
        lda AtkMissed
        bne :++
        jsr TargetPHitMod
        jsr AttackerStatusPHitMod
        jsr CheckPHit
        lda AtkMissed
        bne :++
:
        jsr CheckTargetImage
:
        rts

.endproc

; ---------------------------------------------------------------------------

;Hit% Determination for Magic
.proc HitMagic

_7EF6:
        lda Param1
        bmi Ret
        lda AttackerOffset2
        tax
        lda AttackInfo::Category,X
        ldx TargetOffset
        and CharStruct::CantEvade,X
        bne Ret
        jsr HitCalcLevelMEvade
        jsr CheckAegis
        lda AtkMissed
        bne Ret
        jsr MultiTargetHitPercent
        jsr TargetMHitMod
        jsr CheckForHit
Ret:	rts

.endproc

; ---------------------------------------------------------------------------


;Hit% Determination for Physical Magic
.proc HitPhysicalMagic

_7F1B:
        lda Param1
        bmi Return
        jsr SetHitParam1andTargetEvade
        jsr CheckSpecialEvade
        lda AtkMissed
        bne Return
        jsr TargetPHitMod
        jsr CheckPHit
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Magic Damage Formula
;Attack = Spell Power + (0 .. Spell Power / 8)
;M = Level*Magic/256 +4
;Defense = Target Magic Defense
.proc NormalMagicDamage

_7F30:
        tdc
        tax
        lda Param2    		;spell power
        jsr ShiftDivide_8
        jsr Random_X_A
        clc
        adc Param2
        sta Attack
        tdc
        adc #$00
        sta Attack+1
        lda Level
        sta $24
        lda MagicPower
        sta $25
        jsr Multiply_8bit	;result in $26
        longa
        lda $26
        jsr ShiftDivide_256
        clc
        adc #$0004
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::MDefense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Flare Magic Damage Formula
;Spell Power in Param2
;Attack = Spell Power + (0 .. Spell Power / 32)
;M = Level*Magic/256 + 4
;Def = Target Magic Defense / 32
.proc FlareMagicDamage

_7F6A:
        tdc
        tax
        lda Param2
        jsr ShiftDivide_32
        jsr Random_X_A
        clc
        adc Param2
        sta Attack
        tdc
        adc #$00
        sta Attack+1
        lda Level
        sta $24
        lda MagicPower
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_256
        clc
        adc #$0004
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::MDefense,X
        jsr ShiftDivide_32
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Random Magic damage formula
;Attack = 50..200
;M = Param2
;Defense = Target Magic Defense
.proc RandomMagicDamage

_7FA7:
        tdc
        tax
        lda #$96		;150
        jsr Random_X_A
        clc
        adc #$32   		;50
        tax
        stx Attack
        lda Param2
        tax
        stx M
        ldx TargetOffset
        lda CharStruct::MDefense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Physical magic damage formula
;Spell Power in Param2
;Attack = Spell Power + (0..Spell Power/8)
;M = Level*Magic/256 + 4
;Defense = Target Defense
.proc PhysicalMagicDamage

_7FC2:
        tdc
        tax
        lda Param2
        jsr ShiftDivide_8
        jsr Random_X_A
        clc
        adc Param2
        sta Attack
        tdc
        adc #$00
        sta Attack+1
        lda Level
        sta $24
        lda MagicPower
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_256
        clc
        adc #$0004
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Swords damage formula
;
;Attack = Attack Power + 0..Attack Power/8
;M = Level*Strength/128 + 2
;Defense = Target Defense
;if not(!_CombatTweaks) ;this check affects all the damage types through rods
.proc SwordDamage

_7FFC:
        lda AttackerOffset2
        tax
        lda AttackInfo::AtkPower,X
        tax
        stx $0E			;Attack Power
        jsr ShiftDivide_8
        ldx #$0000
        jsr Random_X_A
        longa
        clc
        adc $0E    		;Attack + 0..Attack/8
        sta Attack
        shorta0
        lda Level
        sta $24
        lda Strength
        sta $25
        jsr Multiply_8bit
        longa
        lda $26			;Level*Strength
        jsr ShiftDivide_128
        clc
        adc #$0002
        sta M			;Level*Strength/128 + 2
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------


;Fists Damage formula
;
;With Brawl:
;	Attack = Attack Power + Level*2 + 0..Level/4 + (50 with Kaiser Knuckles)
;	M = Level*Strength/256 + 2
;Without Brawl:
;	Attack = Attack Power + 0..Level/4
;	M = 2
;Defense = Target Defense
;**optimize:	use 16 bit mode earlier, remove useless mode switches
;
.proc FistDamage

_803E:
        lda AttackerOffset2
        tax
        lda AttackInfo::AtkPower,X
        tax
        stx $0E			;Attack Power
        tdc
        tax
        lda Level
        jsr ShiftDivide_4
        jsr Random_X_A
        tax
        stx $10			;0..Level/4
        lda Level
        sta $24
        lda Strength
        sta $25
        jsr Multiply_8bit
        ldx $26
        stx $14			;Level*Strength
        ldx AttackerOffset
        lda CharStruct::Passives2,X
        and #$40    		;Brawl
        beq NoBrawl
        lda Level
        tax
        stx $12
        asl $12     		;Level *2
        rol $13
        clc
        lda $10			;0..Level/4
        adc $12
        sta $10			;Level*2 + 0..Level/4
        lda $11
        adc $13
        sta $11
        longa
        clc
        lda $0E			;Attack Power
        adc $10     		;+ Level*2 + 0..Level/4
        sta Attack
        shorta0
        longa
        lda $14			;Level*Strength
        jsr ShiftDivide_256
        clc
        adc #$0002
        sta M			;Level*Strength/256 + 2
        shorta0
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$20    		;Improved Brawl
        beq Def
        longa
        clc
        lda Attack
        adc #$0032		;+50
        sta Attack
        shorta0
        bra Def
NoBrawl:	
        longa
        clc
        lda $0E     		;Attack Power
        adc $10     		;+ 0..Level/4
        sta Attack
        shorta0
        ldx #$0002
        stx M			;M = 2
Def:	LDX TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Knives damage formula
;Attack = Attack Power + 0..3
;M = Level*Strength/128 + Level*Agility/128 + 2
;Defense = Target Defense
;
;**bug: knife agility calc only uses low byte, so effectively contributes only 0 or 1 to M
.proc KnifeDamage

_80D4:
        lda AttackerOffset2
        tax
        lda AttackInfo::AtkPower,X
        sta $50
        tdc
        tax
        lda #$03
        jsr Random_X_A
        clc
        adc $50
        tax
        stx $50
        lda Strength
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc #$0002
        sta $0E
        shorta0
        lda Agility
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        lda $26		;*bug: this should be in 16 bit mode
        longa
        jsr ShiftDivide_128
        clc
        adc $0E
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Axes Damage formula
;Attack = Attack Power / 2 + 0..Attack Power
;M = Level*Strength/128 + 2
;Defense = Target Defense / 4
.proc AxeDamage

_812B:
        lda AttackerOffset2
        tax
        lda AttackInfo::AtkPower,X
        tax
        stx $0E
        lsr $0E
        ldx #$0000
        jsr Random_X_A
        longa
        clc
        adc $0E
        sta Attack
        shorta0
        lda Strength
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc #$0002
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        jsr ShiftDivide_4
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Bells Damage formula
;Attack = Attack Power / 2 + 0..(Attack Power / 2)
;M = Level*Agility/128 + 2 + Level*MagicPower/128
;Defense = Target Magic Defense
.proc BellDamage

_816F:
        lda AttackerOffset2
        tax
        lda AttackInfo::AtkPower,X
        lsr
        tax
        stx $0E
        ldx #$0000
        jsr Random_X_A
        longa
        clc
        adc $0E
        sta Attack
        shorta0
        lda Agility
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc #$0002
        sta $0E
        shorta0
        lda MagicPower
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc $0E
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::MDefense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------


;Rods damage formula
;Attack = (0..Attack Power)*2
;M = Level*MagicPower/256 + 2
;Defense = Target Magic Defense
.proc RodDamage

_81CB:
        lda AttackerOffset2
        tax
        lda AttackInfo::AtkPower,X
        ldx #$0000
        jsr Random_X_A
        longa
        asl
        sta Attack
        shorta0
        lda MagicPower
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_256
        clc
        adc #$0002
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::MDefense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------


;Level based Damage formula
;Attack = 10..100
;M = Level/8 +2
;Defense = Target Magic Defense
.proc LevelDamage

_8205:
        tdc
        tax
        lda #$5A		;90
        jsr Random_X_A
        clc
        adc #$0A    		;10
        tax
        stx Attack
        lda Level
        jsr ShiftDivide_8
        clc
        adc #$02
        tax
        stx M
        ldx TargetOffset
        lda CharStruct::MDefense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Monster damage formula
;if not(!_CombatTweaks)
.proc MonsterDamage

_8227:
        ldx AttackerOffset
        lda CharStruct::MonsterAttack,X
        sta $0E
        jsr ShiftDivide_8
        ldx #$0000
        jsr Random_X_A
        clc
        adc $0E
        sta Attack
        tdc
        adc #$00
        sta Attack+1
        ldx AttackerOffset
        lda CharStruct::MonsterM,X
        tay
        sty M
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

.proc ItemFormula

_8252:
        lda Param1
        tax
        stx Attack
        lda Param2
        tax
        stx M
        tdc
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Physical attack but with attack power loaded like a spell
;Attack = Param2 + 0..Param2
;M = Strength*Level/128 + 1
;Defense = Target Defense
;
;**optimize: remove extra Attack load
.proc PhysicalParamDamage

_8261:
        tdc
        tax
        lda Param2
        jsr Random_X_A
        sta Attack	;0..Param2
        clc
        lda Attack
        adc Param2
        sta Attack	;Param2 + 0..Param2
        tdc
        adc #$00
        sta $51
        lda Level
        sta $24
        lda Strength
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        inc
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------


;Throw Damage formula
;Attack = Item Attack Power + (0..Item Attack/8)
;M = Level*Strength/128 + Level*Agility/128 + 2
;Defense = Target Defense
.proc ThrowDamage

_8299:
        ldx AttackerOffset
        lda CharStruct::SelectedItem,X
        sta ThrownItem
        longa
        jsr ShiftMultiply_4
        sta $0E			;ItemID*4
        asl          		;ItemID*8
        clc
        adc $0E			;ItemID*12
        tax
        shorta0
        lda ROMItems::AtkPower,X
        sta $0E       		;Item Attack
        jsr ShiftDivide_8     	;Item Attack / 8
        ldx #$0000
        jsr Random_X_A     	;0..Item Attack / 8
        clc
        adc $0E
        sta Attack    	;Item Attack + 0..Item Attack / 8
        tdc
        adc #$00
        sta Attack+1
        lda Strength
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        sta $0E			;Str*Level/128
        shorta0
        lda Agility
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128	;Agi*Level/128
        clc
        adc $0E
        adc #$0002    		;+2
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;GilToss Damage Formula
;Attack = Level + 10 (or =0, if poor)
;M = Param2
;Defense = Target Defense
;Gil Cost = Param1 * Level
;**optimize: uses long addressing to access gil for no reason
.proc GilTossDamage

_830A:
        clc
        lda Level
        adc #$0A   	;+10
        tax
        stx Attack
        lda Param1
        sta $24
        lda Level
        sta $25
        jsr Multiply_8bit
        lda f:Gil+2
        bne Rich	;>65535 Gil
        longa
        lda f:Gil
        cmp $26
        bcs GilOK
        tdc
        sta Attack	;Not enough Gil, Attack = 0
        shorta
        bra Finish
GilOK:	TDC
        shorta
Rich:	SEC 		;manual 24 bit subtraction
        lda f:Gil
        sbc $26
        sta f:Gil
        lda f:Gil+1
        sbc $27
        sta f:Gil+1
        lda f:Gil+2
        sbc #$00
        sta f:Gil+2
Finish:	LDA Param2
        tax
        stx M
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Multitargetting Modifications
;
;**optimize: we don't need to reorder the bits if we're just counting them
.proc MultiTargetMod

_8366:
        lda AttackerOffset2
        tax
        lda AttackInfo::Targetting,X
        and #$40   		;hits all targets
        bne Return
        lda MultiCommand
        asl
        tax
        lda TargetBitmask,X
        and #$F0
        bne :+			;party targetted
        ;shuffles bits to get monsters in the right order
        ;don't know why since we're just counting them
        lda TargetBitmask,X
        and #$0F
        jsr ShiftMultiply_16
        sta $0E
        lda TargetBitmask+1,X
        and #$F0
        jsr ShiftDivide_16
        ora $0E			;A now contains monsters in order
:	JSR CountSetBits	;result in X
        dex
        beq Return
        lsr Attack+1
        ror Attack
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Back Row Modifications
;**optimize: save some bytes by shifting in 8 bit mode to avoid mode switches
.proc BackRowMod

_839B:
        ldx AttackerOffset
        lda CharStruct::CmdStatus,X
        and #$10
        bne Return
        lda CharStruct::CharRow,X
        bpl :+
        longa
        lsr M
        shorta
:
        ldx TargetOffset
        lda CharStruct::CharRow,X
        bpl Return
        longa
        lsr M
        shorta
Return:
        rts

.endproc

; ---------------------------------------------------------------------------

;Command modifications to damage
;**optimize: save some bytes by shifting in 8 bit mode to avoid mode switches
.proc CommandMod

_83BD:
        ldx AttackerOffset
        lda CharStruct::DamageMod,X
        and #$40
        beq :+
        longa
        asl Attack
        shorta
:
        lda CharStruct::DamageMod,X
        and #$20
        beq :+
        longa
        lsr Attack
        shorta0
:
        lda CharStruct::DamageMod,X
        and #$10
        beq :+
        longa
        asl M
        shorta
:
        lda CharStruct::DamageMod,X
        and #$08
        beq :+
        longa
        lsr M
        shorta0
:
        lda CharStruct::DamageMod,X
        and #$04
        beq :+
        tdc
        tax
        stx Defense
:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        bpl :+
        ldx AttackerOffset
        lda CharStruct::DamageMod,X
        and #$01
        beq :+
        longa
        asl Attack
        shorta
:
        ldx TargetOffset
        lda CharStruct::CmdStatus,X
        bpl :+	  				;Defending
        longa
        lsr M
        shorta0
:
        lda CharStruct::CmdStatus,X
        and #$40				;Guarding
        beq :+
        tdc
        tax
        stx Attack
:
        rts

.endproc

; ---------------------------------------------------------------------------

;Double Grip Damage Multiplier Modifier
;**optimize: save some bytes by shifting in 8 bit mode to avoid mode switches
.proc DoubleGripMod

_8430:
        ldx AttackerOffset
        lda CharStruct::Passives2,X
        and #$20    				;Double Grip
        beq Ret
        lda CharStruct::RHShield,X
        ora CharStruct::LHShield,X
        bne Ret
        lda CharStruct::RHWeapon,X
        beq EmptyHand
        lda CharStruct::LHWeapon,X
        bne Ret
EmptyHand:
        longa
        asl M
        shorta
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Check for Jump
;**optimize: save some bytes by shifting in 8 bit mode to avoid mode switches
.proc CheckJump

_8452:
        ldx AttackerOffset
        lda CharStruct::CmdStatus,X
        and #$10				;Jumping
        beq NoJump
        longa
        asl M
        shorta0
        rts
        								;
NoJump:	JSR BackRowMod
        rts

.endproc

; ---------------------------------------------------------------------------

;Bonus to Attack, costing MP
.proc RuneMod

_8467:
        lda Param3	;MP Cost
        tax
        stx $12
        lda Param2	;Attack Boost
        tax
        stx $10
        longa
        ldx AttackerOffset
        lda CharStruct::CurMP,X
        cmp $12
        bcc Abort   	;not enough MP
        sec
        sbc $12
        sta CharStruct::CurMP,X
        clc
        lda Attack
        adc $10
        sta Attack
        shorta0
        lda Level
        sta $24
        lda MagicPower
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc M
        sta M
        shorta0
        inc Crit
        rts
        							;
Abort:	TDC
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

;Specialty Effect Modifier to Damage
.proc SpecialtyMod

_84AF:
        ldx AttackerOffset
        lda CharStruct::Specialty,X
        bpl :+ 		  			;Autohit or ignore defense
        stz Defense
        stz Defense+1
:
        lda CharStruct::Specialty,X
        and #$01    				;1.5x damage
        beq Return
        longa
        lda Attack
        lsr
        clc
        adc Attack
        sta Attack
        shorta0
Return:
        rts

.endproc

; ---------------------------------------------------------------------------

;Medicine modifier to damage
.proc MedicineMod

_84CF:
        ldx AttackerOffset
        lda CharStruct::Passives2,X
        and #$10   			;Medicine
        beq Ret
        asl M
        rol M+1
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Creature type modification to damage
.proc CreatureMod

_84DD:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and Param3
        beq Ret
        longa
        lda Attack
        jsr ShiftMultiply_8
        sta Attack
        shorta0
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Target Status Effect Modifiers to Magical Damage
.proc TargetStatusModMag

_84F3:
        ldx TargetOffset
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        and #$20   		;shell
        beq :+
        lsr M+1
        ror M
:	LDA CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$20   		;toad
        beq Return
        stz Defense
        stz Defense+1
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Target Status Effect Modifiers to Physical Damage
.proc TargetStatusModPhys

_8512:
        ldx TargetOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$30      				;Toad or Mini
        beq :+
        tdc
        tax
        stx Defense
:
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        and #$40      				;Armor/Protect
        beq :+
        longa
        lsr M
        shorta
:
        rts

.endproc

; ---------------------------------------------------------------------------

;Attacker Status Effect Modifiers to Physical Damage
.proc AttackerStatusModPhys

_8533:
        ldx AttackerOffset
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$30      				;Toad or Mini
        beq :+
        ldx #$0003
        stx Attack
:                                                                                      
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$08      				;Berserk
        beq :+
        longa
        lda Attack
        asl
        clc
        adc Attack
        lsr
        sta Attack
        shorta0
:
        rts

.endproc

; ---------------------------------------------------------------------------

;8x Bonus and Ignore Def if Equal Level
.proc EqualLevelMod

_855D:
        ldx TargetOffset
        lda CharStruct::Level,X
        ldx AttackerOffset
        cmp CharStruct::Level,X
        bne Ret
        longa
        asl Attack
        asl Attack
        asl Attack
        tdc
        sta Defense
        shorta
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Brave Blade Damage formula
;Attack = Attack Power - Escapes (min 0)
;M = Level*Strength/128 + 2
;Defense = Target Defense
;if not(!_CombatTweaks)	;applies through to chicken damage
.proc BraveDamage
        lda AttackerOffset2
        tax
        sec
        lda AttackInfo::AtkPower,X
        sbc BattleData::Escapes
        bcs :+
        tdc 			;min 0
:	TAX
        stx Attack
        lda Level
        sta $24
        lda Strength
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc #$0002
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Goblin Punch Damage formula
;Attack = RH + LH Goblin Attack Power
;M = Level*Strength/128 + 2, or Monster Attack Modifier
;Defense = Target Defense
.proc GoblinDamage

_85AD:
        ldx AttackerOffset
        clc
        lda CharStruct::MonsterAttack,X
        adc CharStruct::MonsterAttackLH,X
        sta Attack
        tdc
        adc #$00
        sta Attack+1
        lda AttackerIndex
        cmp #$04
        bcc Party  		;<4 means party
        lda CharStruct::MonsterM,X
        tax
        stx M
        bra Finish
Party:	LDA Level
        sta $24
        lda Strength
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        clc
        adc #$0002
        sta M
        shorta0
Finish:	LDX TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Strong Fight Damage formula
;Attack = Monster Attack * 8 + 0..Monster Attack / 8
;M = Monster M
;Defense = Target Defense
;**optimize: use 16 bit mode more to save some bytes
.proc StrongFightDamage

_85F1:
        ldx AttackerOffset
        clc
        lda CharStruct::MonsterAttack,X
        pha
        longa
        jsr ShiftMultiply_8
        sta Attack
        shorta0
        lda CharStruct::MonsterM,X
        tax
        stx M
        pla
        jsr ShiftDivide_8
        ldx #$0000
        jsr Random_X_A
        clc
        adc Attack
        sta Attack
        lda Attack+1
        adc #$00
        sta Attack+1
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts
;

.endproc

; ---------------------------------------------------------------------------

;Chicken Knife Damage Formula
;Attack = Escapes/2
;M = Level*Strength/128 + Level*Agility/128 + 2
;Defense = Target Defense
.proc ChickenDamage

_8626:
        lda BattleData::Escapes
        lsr
        tax
        stx Attack
        lda Level
        sta $24
        lda Strength
        sta $25
        jsr Multiply_8bit
        ldx $26
        stx $0E
        lda Level
        sta $24
        lda Agility
        sta $25
        jsr Multiply_8bit
        longa
        lda $26
        jsr ShiftDivide_128
        sta $10
        lda $0E
        jsr ShiftDivide_128
        clc
        adc $10
        adc #$0002
        sta M
        shorta0
        ldx TargetOffset
        lda CharStruct::Defense,X
        tax
        stx Defense
        rts

.endproc

; ---------------------------------------------------------------------------

;Magic Element Up Modifier
.proc ElementUpMod

_866D:
        ldx AttackerOffset
        lda CharStruct::ElementUp,X
        and AtkElement
        beq Return
        longa
        lda Attack
        lsr
        clc
        adc Attack
        sta Attack
        shorta0
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Magic Sword Modifiers to Physical
.proc MagicSwordMod

_8684:
        ldx AttackerOffset
        lda CharStruct::MSwordElemental1,X
        ora CharStruct::MSwordElemental2,X
        ora CharStruct::MSwordElemental3,X
        sta AtkElement
        ora CharStruct::MSwordStatus1,X
        ora CharStruct::MSwordStatus2,X
        ora CharStruct::MSwordStatusSpecial,X
        beq :+				;no MSword, could go to end but too far away
        				;so it'll just fail all the checks
        inc MagicSword
:	;Magic Sword Flare
        lda CharStruct::MSwordStatusSpecial,X
        bpl :+					;flare
        longa
        clc
        lda Attack
        adc #$0064  				;+100
        sta Attack
        lsr Defense
        lsr Defense
        shorta0
        rts
:	;;Elemental Magic Sword vs Absorb
        ldx TargetOffset
        lda CharStruct::EAbsorb,X
        and AtkElement
        beq :+
        inc AtkHealed
        stz Defense
        stz Defense+1
        rts
:	;;Elemental Magic Sword vs Immune		
        lda CharStruct::EImmune,X
        and AtkElement
        beq :+
        inc AtkMissed
        rts
:	;Elemental Magic Sword vs Resist		
        lda CharStruct::EHalf,X
        and AtkElement
        beq :+
        lsr M+1
        ror M
        rts
:	;;Elemental Magic Sword L3 vs Weakness
        lda CharStruct::EWeak,X
        sta $0E
        ldx AttackerOffset
        and CharStruct::MSwordElemental3,X
        beq :++
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20    				;heavy?
        beq :+
        longa
        asl Attack
        asl Attack
        stz Defense
        shorta
        rts
:	;;Elemental Magic Sword L3 Instant Death
        lda CharStruct::Status1,X		;X is still TargetOffset
        and #$02     				;zombie
        bne Return
        lda CharStruct::Status1,X
        ora #$80				;dead
        sta CharStruct::Status1,X
        inc TargetDead
        rts
:	;;Elemental Magic Sword L2 vs Weakness
        lda $0E					;CharStruct::EWeak for Target
        and CharStruct::MSwordElemental2,X  	;X is still TargetOffset
        beq :+
        longa
        lda Attack
        asl
        clc
        adc Attack
        sta Attack
        tdc
        sta Defense
        shorta
        rts
:	;;Elemental Magic Sword L1 vs Weakness
        lda $0E					;CharStruct::EWeak for Target
        and CharStruct::MSwordElemental1,X  	;X is still TargetOffset
        beq Return
        longa
        asl Attack
        stz Defense
        shorta
Return:
        rts

.endproc

; ---------------------------------------------------------------------------

;Elemental Modifiers for Physical
;
;**optimize: 	can probably merge elemental damage routines to save space
;		only difference is the magic one also checks for block, and sets a magic miss flag
.proc ElementDamageModPhys

_8734:
        lda MagicSword
        bne Ret		;Magic Sword Elements handled elsewhere
        ldx TargetOffset
        lda CharStruct::EAbsorb,X
        and AtkElement
        beq CheckImmune
        inc AtkHealed
        stz Defense
        stz Defense+1
        rts
CheckImmune:								;
        lda CharStruct::EImmune,X
        and AtkElement
        beq CheckHalf
        inc AtkMissed
        rts
CheckHalf:								;
        lda CharStruct::EHalf,X
        and AtkElement
        beq CheckWeak
        lsr Attack+1
        ror Attack
        rts
CheckWeak:								;
        lda CharStruct::EWeak,X
        and AtkElement
        beq Ret
        asl Attack
        rol Attack+1
        stz Defense
        stz Defense+1
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Elemental Damage Modifiers for Magic
;
;**optimize: save some space getting rid of magic sword check
.proc ElementDamageModMag

_876E:
        lda MagicSword		;shouldn't ever be set
        bne Return		;because it's not loaded for magic
        ldx TargetOffset
        lda CharStruct::EAbsorb,X
        and AtkElement
        beq :+
        ;absorbed
        inc AtkHealed
        stz Defense
        stz Defense+1
        rts
:							
        lda CharStruct::EBlock,X
        and AtkElement
        beq :+
        ;blocked (is this used?)
        inc AtkMissed
        rts
:						
        lda CharStruct::EImmune,X
        and AtkElement
        beq :+
        ;immune
        inc AtkMissed
        inc MagicNull
        rts
:							
        lda CharStruct::EHalf,X
        and AtkElement
        beq :+
        ;half
        lsr Attack+1
        ror Attack
        rts
:							
        lda CharStruct::EWeak,X
        and AtkElement
        beq Return
        asl Attack
        rol Attack+1
        stz Defense
        stz Defense+1
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Elemental Modifiers to Magic
;
;Very similar to ElementDamageModMag ($876E)
;Will note differences with *
;
;**optimize: consolidate with above routine if differences won't cause problems
.proc ElementDamageModMag2

_87B5:
        ldx TargetOffset
        ;check absorb
        lda CharStruct::EAbsorb,X
        and AtkElement
        beq :+
        inc AtkHealed
        rts 	;*does not 0 def on absorb like the other routine
        	;*also doesn't check for Element "block"							
:	;check immunity
        lda CharStruct::EImmune,X
        and AtkElement
        beq :+
        inc AtkMissed
        rts
:	;check half							
        lda CharStruct::EHalf,X
        and AtkElement
        beq :+
        lsr Param2    ;*halves param2 instead of Attack ($50)
        rts
:	;check weak								
        lda CharStruct::EWeak,X
        and AtkElement
        beq Ret
        asl Param2    	;*doubles param2 instead of Attack
        		;*also doesn't 0 def
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Check for Critical Hit
;Param1: Crit%
;**optimize: 	save 2 bytes by not switching modes, or by zeroing def while in 16 bit mode
.proc CheckCrit

_87DF:
        lda MagicSword	;no crits with magic sword,
        bne Ret	;in vanilla FF5 no weapons support both anyway
        jsr Random_0_99
        cmp Param1
        bcs Ret
        lda #$01
        sta Crit
        longa
        asl Attack
        shorta
        tdc
        tax
        stx Defense
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Check Creature Type for Critical Hit
.proc CheckCreatureCrit

_87F9:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and Param1
        beq Ret
        lda #$01
        sta Crit
        longa
        asl M
        shorta
        tdc
        tax
        stx Defense
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Calculate Final Damage w/Magic Sword
;Damage is returned in different addresses depending on what happens
;DamageToAttacker = $7B6B		
;DamageToTarget = $7B6D		
;HealingToAttacker = $7B6F		
;HealingToTarget = $7B71
;
;**optimize: get rid of pointless code		
.proc CalcFinalDamageMSword

_8811:
        jsr CalcBaseDamage
        ldx BaseDamage
        bne :+
        stx DamageToTarget
        rts
:											;
        lda AttackerOffset2
        tax
        lda AttackInfo::Category,X
        ldx TargetOffset
        and CharStruct::CantEvade,X
        beq :+
        longa
        asl M
        shorta
:
        ldx AttackerOffset
        lda CharStruct::MSwordStatusSpecial,X
        and #$40
        beq :+
        jmp DrainDamage
:
        lda CharStruct::MSwordStatusSpecial,X
        and #$20
        beq :+
        jmp MSwordPsyche
:
        lda AtkHealed
        beq :+
        ldx BaseDamage
        stx HealingToTarget
        rts
:	;Unsure what this section is for, attacker takes damage but no target healing	
        lda AttackerDamaged
        beq :++
        ldx AttackerOffset			;uses attacker's defense instead
        lda CharStruct::Defense,X
        tax
        stx Defense
        jsr CalcBaseDamage
        ldx BaseDamage
        bne :+					;*pointless code
        stx DamageToAttacker
        rts
:											;
        stx DamageToAttacker 			;*pointless code
        rts 					;*pointless code
:											;
        ldx BaseDamage
        stx DamageToTarget
        rts

.endproc

; ---------------------------------------------------------------------------

;Gravity Attack Damage, Type 07h
;Damage is Param2($58) * Current HP of target / 16 unless target is Heavy
.proc GravityDamage

_8874:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20    			;heavy
        beq NotHeavy
        tdc
        tax
        stx BaseDamage
        stx DamageToTarget
        rts
NotHeavy:									;
        lda CharStruct::CurHP,X
        sta $2A
        lda CharStruct::CurHP+1,X
        sta $2B
        lda Param2
        tax
        stx $2C
        jsr Multiply_16bit		;Multiply $2A by $2C and store in $2E
        longa
        lsr $30				;divide 32 bit result by 16
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        shorta
        ldx $30
        bne :+
        ldx $2E
        cpx #$270F
        bcc :++
:	LDX #$270F  			;9999 if result was greater
:	STX BaseDamage
        stx DamageToTarget
        rts

.endproc

; ---------------------------------------------------------------------------

;Cure, Damage if Undead
.proc CureTarget

_88C1:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01			;undead	creature
        bne :+
        lda CharStruct::ArmorProperties,X
        and #$02   			;undead armor
        beq :++
:	JMP CalcFinalDamageMSword
:	JSR CalcBaseDamage
        ldx BaseDamage
        stx HealingToTarget
        rts

.endproc

; ---------------------------------------------------------------------------


;Full Cure, Reduce HP to Critical if Undead
.proc FullCureTarget

_88DE:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01			;undead	creature
        bne :+
        lda CharStruct::ArmorProperties,X
        and #$02     			;undead armor
        beq :++
:	JMP SetHPCritical
:	REP #$20
        lda CharStruct::MaxHP,X
        sta CharStruct::CurHP,X
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Reduce HP to Critical
;Fails vs Heavy
.proc SetHPCritical

_88FD:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20   			;heavy
        beq NotHeavy
        inc AtkMissed
        rts
NotHeavy:								;
        tdc
        tax
        lda #$08
        jsr Random_X_A
        inc
        tax
        stx $0E
        longa
        ldx TargetOffset
        lda $0E
        cmp CharStruct::CurHP,X
        bcs :+
        sta CharStruct::CurHP,X
:	TDC
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

;Magic Sword Drain
.proc DrainDamage

_8926:
        jsr CalcBaseDamage
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01   				;undead	creature type
        bne Undead
        lda CharStruct::ArmorProperties,X
        and #$02   				;undead	armor
        beq NotUndead
Undead:	
        ldx BaseDamage
        stx DamageToAttacker
        stx HealingToTarget
        rts
NotUndead:
        ldx BaseDamage
        stx DamageToTarget
        stx HealingToAttacker
        rts

.endproc

; ---------------------------------------------------------------------------

;Magic Sword Psyche
.proc MSwordPsyche

_894D:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01				;undead	creature type
        bne Undead
        lda CharStruct::ArmorProperties,X
        and #$02   				;undead	armor
        beq NotUndead
Undead:
        ldx AttackerOffset
        longa
        lda CharStruct::CurMP,X
        jsr ShiftDivide_4
        sta HealingToTargetMP
        sta DamageToAttackerMP
        shorta0
        rts
NotUndead:					
        ldx TargetOffset
        longa
        lda CharStruct::CurMP,X
        jsr ShiftDivide_4
        sta HealingToAttackerMP
        sta DamageToTargetMP
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Psyche Damage
.proc PsycheDamage

_8985:
        longa
        sec
        lda Attack
        sbc Defense
        sta $2A
        lda M
        sta $2C
        shorta0
        jsr Multiply_16bit
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$01			;undead	creature
        bne Undead
        lda CharStruct::ArmorProperties,X
        and #$02   			;undead armor
        beq NotUndead
Undead:	REP #$20
        ldx AttackerOffset
        lda $2E
        cmp CharStruct::CurMP,X
        bcc :+
        lda CharStruct::CurMP,X
        sta $2E
:	LDA $2E
        sta HealingToTargetMP
        sta DamageToAttackerMP
        bra Finish
NotUndead:
        longa
        ldx TargetOffset
        lda $2E
        cmp CharStruct::CurMP,X
        bcc :+
        lda CharStruct::CurMP,X
        sta $2E
:	LDA $2E
        sta HealingToAttackerMP
        sta DamageToTargetMP
Finish:	TDC
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

;MP Heal
.proc MPHeal

_89DE:
        longa
        sec
        lda Attack
        sbc Defense
        sta $2A
        lda M
        sta $2C
        shorta0
        jsr Multiply_16bit
        ldx $2E
        stx HealingToTargetMP
        rts

.endproc

; ---------------------------------------------------------------------------

;Fully Heal MP
.proc FullMPHeal

_89F7:
        longa
        ldx TargetOffset
        lda CharStruct::MaxMP,X
        sta CharStruct::CurMP,X
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;(8A05)
;Calculate Final Damage 
;similar to $8811/CalcFinalDamageMSword but doesn't have Magic Sword checks
;DamageToAttacker = $7B6B		
;DamageToTarget = $7B6D		
;HealingToTarget = $7B71
;
;**bugfix: "can't evade" bit should also double damage
.org $8A05	
.proc CalcFinalDamage

_8A05:
        jsr CalcBaseDamage
        ldx BaseDamage
        beq Finish
        ldx M
        bne :+
        inc M
        ;likely bug, the "can't evade"	check here is only hit if M was 0
        lda AttackerOffset2
        tax
        lda AttackInfo::Category,X
        ldx TargetOffset
        and CharStruct::CantEvade,X
        beq :+
        longa
        asl M      		;would have doubled damage
        shorta
:	LDA AtkHealed
        beq :+
        ldx BaseDamage
        stx HealingToTarget
        rts
:	LDA AttackerDamaged	;attacker damaged instead of target
        beq Finish
        ldx AttackerOffset
        lda CharStruct::Defense,X  	;attacker's defense
        tax
        stx Defense
        jsr CalcBaseDamage
        ldx BaseDamage
        stx DamageToAttacker
        rts
Finish:	LDX BaseDamage
        stx DamageToTarget
        rts

.endproc

; ---------------------------------------------------------------------------

;Calculate Damage from % of Target Max HP
;**optimize: 	save some space by going to 16 bit mode earlier
;		and by removeing unnecessary high byte OR
.proc CalcDamageMaxHP

_8A4E:
        lda Param2
        tax
        stx $2C
        ldx TargetOffset
        lda CharStruct::MaxHP,X
        sta $2A
        lda CharStruct::MaxHP+1,X
        sta $2B
        jsr Multiply_16bit
        longa
        lsr $30		;divide result by 16 via shifts
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lda $30
        bne Cap
        lda $2E
        cmp #$270F	;9999
        bcc :+
Cap:	LDA #$270F	;cap at 9999
        sta $2E
:	LDA $2E
        ora $30		;*unnecessary
        bne :+
        inc $2E		;min 1
:	TDC
        shorta
        ldx $2E
        lda AtkHealed
        bne Heal
        stx DamageToTarget
        rts
        							;
Heal:	STX HealingToTarget
        rts

.endproc

; ---------------------------------------------------------------------------

;Calculate Damage from % of Attacker Current HP
;
;**optimize: 	save some space by going to 16 bit mode earlier
;		could also reuse more than half of the code from CalcDamageMaxHP
.proc CalcDamageAttackerCurHP

_8A9D:
        lda Param2
        tax
        stx $2C
        ldx AttackerOffset
        lda CharStruct::CurHP,X
        sta $2A
        lda CharStruct::CurHP+1,X
        sta $2B
        jsr Multiply_16bit
        longa
        lsr $30		;divide result by 16 via shifts
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lda $30
        bne Cap
        lda $2E
        cmp #$270F	;9999
        bcc :+
Cap:	LDA #$270F	;cap at 9999
        sta $2E
:	LDA $2E
        ora $30		;*unnecessary
        bne :+
        inc $2E		;min 1
:	TDC
        shorta
        ldx $2E
        lda AtkHealed
        bne Heal
        stx DamageToTarget
        rts
        						;
Heal:	STX HealingToTarget
        rts

.endproc

; ---------------------------------------------------------------------------

;Calculate Damage from % of Target Current MP
.proc CalcDamageTargetCurMP

_8AEC:
        lda Param2
        tax
        stx $2C
        ldx TargetOffset
        lda CharStruct::CurMP,X
        sta $2A
        lda CharStruct::CurMP+1,X
        sta $2B
        jsr Multiply_16bit
        longa
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lda $30
        bne Cap
        lda $2E
        cmp #$270F	;9999
        bcc :+
Cap:	LDA #$270F 	;cap at 9999
        sta $2E
:	LDA $2E
        ora $30		;*unnecessary
        bne :+
        inc $2E		;min 1
:	TDC
        shorta
        ldx $2E
        stx DamageToTargetMP
        rts

.endproc

; ---------------------------------------------------------------------------

;Calculate Damage = Attacker Max HP - Attacker Current HP
.proc CalcDamageAttackerDiffHP

_8B33:
        longa
        ldx AttackerOffset
        sec
        lda CharStruct::MaxHP,X
        sbc CharStruct::CurHP,X
        cmp #$270F		;9999
        bcc :+
        lda #$270F		;cap at 9999
:	STA DamageToTarget
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

				
;Heals HP and MP by half of their max
.proc RestoreHalfMax

_8B4D:
        longa
        ldx TargetOffset
        lda CharStruct::MaxHP,X
        lsr
        clc
        adc CharStruct::CurHP,X
        bcs Cap
        cmp CharStruct::MaxHP,X
        bcc :+
Cap:	LDA CharStruct::MaxHP,X
:	STA CharStruct::CurHP,X
        lda CharStruct::MaxMP,X
        lsr
        clc
        adc CharStruct::CurMP,X
        cmp CharStruct::MaxMP,X
        bcc :+
        lda CharStruct::MaxMP,X
:	STA CharStruct::CurMP,X
        shorta0
        rts

.endproc

; ---------------------------------------------------------------------------

;Status Duration Determination
;Param2($58) is base duration with a flag in the high bit
;Result in StatusDuration ($3ED8)
;	if flag is set result is (2*base + level/4)
;	if no flag set result is (2*base + level/4 - targetlvl/4) or 30 if target is heavy
;**optimize: this function does some strange things, could be worth optimizing for space
.proc CalcStatusDuration

_8B7D:
        stz $0F
        lda Param2
        and #$7F			;strips high bit
        asl        			;shift left, high bit (0) into carry
        rol $0F				;shifts a 0 bit into a 0? why?
        sta $0E
        lda Level
        jsr ShiftDivide_4
        clc
        adc $0E
        sta $0E
        lda $0F				;finish 16 bit result
        adc #$00
        beq :+
        lda #$FF			;but we cap at 255 if overflowed
        sta $0E    			;so why did we finish computing it
:	LDA Param2
        bmi Finish
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20   			;heavy
        beq :+
        lda #$1E			;duration is 30 if target heavy
        sta $0E
        bra Finish
:	CLC
        lda CharStruct::Level,X
        adc CharStruct::BonusLevel,X
        jsr ShiftDivide_4
        sta $0F				;target level /4
        sec
        lda $0E				;duration
        sbc $0F    			;target level /4
        beq :+
        bcs :++
:	LDA #$01   			;min duration 1
:	STA $0E
Finish:	LDA $0E
        sta StatusDuration
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Magic Sword Status Effects
.proc ApplyMSwordStatus

_8BCF:
        lda Param3
        pha 		;Save Param3 to restore later
        ldx AttackerOffset
        lda CharStruct::MSwordStatus1,X
        sta Param3
        lda #$01
        sta StatusFixedDur
        jsr ApplyStatus1
        ldx AttackerOffset
        lda CharStruct::MSwordStatus2,X
        sta Param3
        lda #$01
        sta StatusFixedDur
        jsr ApplyStatus2
        stz AtkMissed
        pla
        sta Param3	;Restore Original Param3
        stz StatusFixedDur
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Conditional Status Effects
;Status Hit% in Param2/$58
;High bit set means Status1, unset means Status2
;Status to apply in Param3/$59
;
;**optimize: Aborts if Magic Sword is initialized, but I don't think this is used anywhere that can happen
.proc ApplyConditionalStatus

_8BF9:
        lda MagicSword
        bne Miss
        lda Param2
        and #$7F
        sta HitPercent
        jsr Random_0_99
        cmp HitPercent
        bcs Miss
        lda Param2
        bpl :++			;check which status byte to mod
        lda #$01
        sta StatusFixedDur
        jsr ApplyStatus1
        ldx TargetOffset
        lda CharStruct::Status1,X
        bpl :+  		;check if target died
        inc TargetDead
:	BRA :++
:	LDA #$01
        sta StatusFixedDur
        jsr ApplyStatus2
:	STZ AtkMissed
Miss:	STZ StatusFixedDur
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Monster Specialty Effects
.proc ApplySpecialtyEffects

_8C2F:
        stz Param3		;used for status here
        ldx AttackerOffset
        lda CharStruct::Specialty,X
        and #$20  		 	;Poison	Specialty
        beq :+
        lda #$04			;Poison Status
        sta Param3
:
        lda CharStruct::Specialty,X
        and #$10   			;Blind Specialty
        beq :+
        lda Param3
        ora #$01			;Blind Status
        sta Param3
:
        lda #$01
        sta StatusFixedDur
        jsr ApplyStatus1
        stz Param3
        ldx AttackerOffset
        lda CharStruct::Specialty,X
        and #$40   			;Old Specialty
        beq :+
        lda #$80			;Old Status
        sta Param3
:
        lda CharStruct::Specialty,X
        and #$08   			;Paralyze Specialty
        beq :+
        lda Param3
        ora #$20			;Paralyze Status
        sta Param3
:	
        lda CharStruct::Specialty,X
        and #$04   			;Charm Specialty
        beq :+
        phx
        ldx TargetOffset
        lda CharStruct::Status4,X
        plx
        and #$20   			;Target is Controlled
        bne :+
        lda Param3
        ora #$10			;Charm Status
        sta Param3
:
        lda #$01
        sta StatusFixedDur
        jsr ApplyStatus2
        stz Param3
        ldx AttackerOffset
        lda CharStruct::Specialty,X
        and #$02   			;HP Leak Specialty
        beq :+
        lda #$08			;HP Leak Status
        sta Param3
        lda #$01
        sta StatusFixedDur
        jsr ApplyStatus4
:
        stz StatusFixedDur
        stz AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status Effect 1 after checking immunity
;Special cases for Zombie (players only) and Stone (kills monsters)
;Status in $59 / Param3
.proc ApplyStatus1

_8CAC:
        ldx TargetOffset
        lda CharStruct::StatusImmune1,X
        and Param3
        bne Miss			;Immune to a requested status
        lda wTargetIndex
        cmp #$04     			;Slots 4 and up are monsters
        bcs NotZombie			;zombie doesn't apply to monsters
        lda Param3
        and #$02     			;Zombie
        beq NotZombie
        lda CharStruct::Status1,X	;Target Status
        ora CharStruct::AlwaysStatus1,X
        and #$42     			;Stone or Zombie
        bne Miss    			;Miss if set
        ;now applying Zombie status
        ldx AttackerOffset
        phx
        lda wTargetIndex
        jsr ResetATB
        ldx AttackerOffset		;ResetATB actually puts target here
        lda #$80
        sta CharStruct::ActionFlag,X
        stz CharStruct::Command,X
        lda wTargetIndex
        tax
        lda CurrentTimer::ATB,Y		;Y is a timer offset from ResetATB
        cmp #$7F
        bcc :+
        lda #$7F			;Cap at 127
:
        sta UncontrolledATB,X
        tdc
        sta EnableTimer::ATB,Y			;??
        inc
        sta CurrentTimer::ATB,Y		;Reset ATB?
        ldx TargetOffset
        stz CharStruct::CurHP,X   	;HP = 0
        stz CharStruct::CurHP+1,X
        plx 				;Attacker Offset
        stx AttackerOffset		;Restore original Attacker
        bra :+
NotZombie:	
        lda wTargetIndex
        cmp #$04			;Slots 4 and up are monsters
        bcc :+
        lda Param3
        and #$40     			;Stone
        beq :+
        lda CharStruct::Status1,X	;X is Target here, and is a monster
        ora #$80     			;Set Wounded/Dead status
        sta CharStruct::Status1,X	;instead of stone for monsters
        bra :++
:
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$02			;Zombie
        bne Miss
        lda CharStruct::Status1,X
        ora Param3     		;Set any other status
        sta CharStruct::Status1,X
:
        jmp StartStatus1Timer
Miss:	
        inc AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status Effect 2
;Special case for charm/berserk (players only) and paralyze
;Status in $59 / Param3
.proc ApplyStatus2

_8D2E:
        ldx TargetOffset
        lda CharStruct::StatusImmune2,X
        and Param3
        beq :+
        jmp Miss		;Miss if Immune
:	LDA Param3
        and #$10		;Charm
        bne Charm
        lda Param3
        and #$08		;Berserk
        beq :+++
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$18		;Berserk or Charm
        bne Miss   		;Miss if have either
        lda CharStruct::Status4,X
        and #$20		;Controlled
        bne Miss		;Miss if have this status
        bra :+
Charm:	LDA CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$18		;Berserk or Charm
        bne Miss  		;Miss if have either
        lda CharStruct::Status4,X
        and #$20		;Controlled
        bne Miss		;Miss if have this status
:	LDA wTargetIndex
        cmp #$04		;04h and up are monsters
        bcs :++
        ;berserk or Charm on a player char	
        ldx AttackerOffset
        phx 			;save attacker since it gets overwritten
        lda wTargetIndex
        jsr ResetATB
        ldx AttackerOffset	;ResetATB actually puts target here
        lda #$80
        sta CharStruct::ActionFlag,X
        stz CharStruct::Command,X
        lda wTargetIndex
        tax
        lda CurrentTimer::ATB,Y	;Y is a timer offset from ResetATB
        cmp #$7F
        bcc :+
        lda #$7F		;cap at 127
:	STA UncontrolledATB,X
        tdc
        sta EnableTimer::ATB,Y
        inc
        sta CurrentTimer::ATB,Y
        plx
        stx AttackerOffset	;Restore attacker variable
        bra :++
:	;berserk or charm on monster, or other status
        lda Param3
        and #$20		;Paralyze
        beq :+
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$20
        bne Miss   		;Miss if already Paralyzed
        lda wTargetIndex
        jsr GetTimerOffset	;Y and $36 = Timer Offset from ROM
        tdc
        sta EnableTimer::ATB,Y
        inc
        sta CurrentTimer::ATB,Y
:	LDX TargetOffset
        lda CharStruct::Status2,X
        ora Param3
        sta CharStruct::Status2,X
        jmp StartStatus2Timer
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status Effect 3
;Status in $59 / Param3
.proc ApplyStatus3

_8DCB:
        ldx TargetOffset
        lda CharStruct::StatusImmune3,X
        and Param3
        bne Miss
        lda Param3
        and #$10			;Stop
        beq :+
        ;stop status
        lda CharStruct::Status2,X	;Checks Status2 instead of 3
        ora CharStruct::AlwaysStatus2,X	; likely a bug?
        and #$10			;Charm
        bne Miss			;Stop misses if target is charmed
        lda wTargetIndex
        jsr GetTimerOffset		;Y and $36 = Timer Offset from ROM
        lda EnableTimer::ATB,Y
        and #$40
        bne :+
        lda #$01
        sta EnableTimer::ATB,Y
:	LDX TargetOffset
        lda CharStruct::Status3,X
        ora Param3
        sta CharStruct::Status3,X
        jmp StartStatus3Timer
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status Effect 4
.proc ApplyStatus4

_8E05:
        ldx TargetOffset
        lda Param3		;Status
        and #$10		;Countdown
        beq :+
        lda CharStruct::Status4,X
        ora CharStruct::AlwaysStatus4,X
        and Param3
        bne Miss		;Miss if already have countdown
:	LDA CharStruct::Status4,X
        ora Param3
        sta CharStruct::Status4,X
        jmp Status4Timer
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Toggle Status 1
.proc ToggleStatus1

_8E25:
        ldx TargetOffset
        lda CharStruct::StatusImmune1,X
        and Param3
        bne Miss
        lda CharStruct::Status1,X
        eor Param3
        sta CharStruct::Status1,X
        rts
Miss:									;
        inc AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------


;Apply Flirt, miss if Heavy
;**optimize: remove extra X load
.proc ApplyFlirt

_8E3A:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20   		;heavy
        beq :+
        inc AtkMissed
        rts
        								;
:	LDX TargetOffset
        lda CharStruct::CmdStatus,X
        ora #$08		;flirted
        sta CharStruct::CmdStatus,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status Effect 3, Mutually exclusive statuses
;also adjusts status timers for haste and slow
.proc ApplyStatus3Exclusive

_8E51:
        ldx TargetOffset
        lda CharStruct::StatusImmune3,X
        and Param3
        bne Miss
        lda CharStruct::Status3,X
        sta $0E
        and Param2
        ora Param3
        sta CharStruct::Status3,X
        lda wTargetIndex
        jsr GetTimerOffset	;Y = $36 = timer offset
CheckSlow:	
        lda CharStruct::Status3,X
        and #$04		;slow
        beq CheckHaste
        lda $0E			;original Status3
        and #$04		;slow
        bne CheckHaste		;don't adjust timers if already	slow
        tdc
        tax
SlowTimerLoop:
        lda CurrentTimer+0,Y
        beq :++			;is timer active?
        asl 			;double timer
        bcc :+
        lda #$FF		;cap at 255
:	STA CurrentTimer+0,Y
:	INY
        inx
        cpx #$000B		;10 timers, 11 is too far
        bne SlowTimerLoop
        bra Ret
CheckHaste:	
        lda CharStruct::Status3,X
        and #$08		;haste
        beq Ret
        lda $0E			;original Status3
        and #$08		;haste
        bne Ret		;don't adjust timers if already	hasted
        tdc
        tax
HasteTimerLoop:
        lda CurrentTimer+0,Y
        beq :++			;is timer active?
        lsr 			;halve timer
        bne :+
        inc 			;min 1
:	STA CurrentTimer+0,Y
:	INY
        inx
        cpx #$000B		;10 timers, 11 is too far
        bne HasteTimerLoop
Ret:	rts
Miss:									;
        inc AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Remove Status 1
;Status in Param3, unset bits are to be cleared
.proc RemoveStatus1

_8EB6:
        lda Param3
        eor #$FF		;Invert, set bits are status to clear
        and #$02		;Zombie
        beq TestStone		;Clearing Zombie?
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$02
        beq TestStone		;Was Zombie set?
_ResetATB:			;need to update ATB if Zombie/Stone was set then cleared
        ldx AttackerOffset
        phx
        lda wTargetIndex
        jsr ResetATB		;changes AttackerOffset
        plx
        stx AttackerOffset	;restore original
        bra RemoveStatus
TestStone:
        lda Param3
        eor #$FF
        and #$40		;Stone
        beq RemoveStatus	;Clearing Stone?
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$40
        bne _ResetATB		;need ATB updated if clearing Stone
RemoveStatus:
        ldx TargetOffset
        lda CharStruct::Status1,X
        sta $0E			;original status
        and Param3
        sta CharStruct::Status1,X
        lda Param3
        sta $0F			;requested status clears
        eor #$FF		;Invert, set bits are status to clear
        sta Param3
        jsr StopStatus1Timer
        lda $0E			;original status
        and #$40		;Stone
        sta $0E			;Stone bit if target was Stone
        lda $0F			;unset bits are to be cleared
        eor #$FF		;Invert, set bits are status to clear
        and $0E
        beq Ret		;return if we didn't clear Stone
        lda wTargetIndex
        tax
        inc ActiveParticipants,X	;target is now active
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Remove Status 2
;Status in Param3, unset bits are to be cleared
.proc RemoveStatus2

_8F11:
        ldx TargetOffset
        lda Param3
        eor #$FF		;Invert, set bits are status to clear
        and #$10		;Charm
        beq :+
        lda CharStruct::Status2,X
        and #$10
        bne _ResetATB
:	LDA Param3
        eor #$FF		;Invert, set bits are status to clear
        and #$08		;Berserk
        beq :+
        lda CharStruct::Status2,X
        and #$18		;Charm or Berserk
        bne _ResetATB
:	LDA Param3
        eor #$FF		;Invert, set bits are status to clear
        and #$20		;Paralyze
        beq :+
        lda CharStruct::Status2,X
        and #$20
        bne _ResetATB
:	LDA Param3
        eor #$FF		;Invert, set bits are status to clear
        and #$40		;Sleep
        beq RemoveStatus
        lda CharStruct::Status2,X
        and #$40
        beq RemoveStatus
_ResetATB:	;if we're clearing Charm/Berserk/Poison/Sleep we need to update ATB
        ldx AttackerOffset
        phx
        lda wTargetIndex
        jsr ResetATB	;changes AttackerOffset
        plx
        stx AttackerOffset	;restore original
RemoveStatus:
        ldx TargetOffset
        lda CharStruct::Status2,X
        and Param3
        sta CharStruct::Status2,X
        lda Param3
        eor #$FF		;Invert, set bits are status to clear
        sta Param3
        jsr StopStatus2Timer
        rts

.endproc

; ---------------------------------------------------------------------------

;Remove Status 3
;Status in Param3, unset bits are to be cleared
.proc RemoveStatus3

_8F6E:
        ldx TargetOffset
        lda CharStruct::Status3,X
        and Param3
        sta CharStruct::Status3,X
        lda Param3
        eor #$FF			;Invert, set bits are status to clear
        sta Param3
        jsr StopStatus3Timer
        rts

.endproc

; ---------------------------------------------------------------------------

;Kill target, miss if Heavy
.proc KillNonHeavy

_8F82:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        and #$20    			;heavy
        beq NotHeavy
        inc AtkMissed
        rts
NotHeavy:								;
        jsr HitMagicConditionalAutohit
        lda AtkMissed
        bne Ret
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$02    			;zombie status
        bne Miss
        lda CharStruct::Status1,X
        ora #$80			;dead status
        sta CharStruct::Status1,X
        rts
        								;
Miss:	INC AtkMissed
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Revive Target
;Param3/$59 is fraction/4 of max hp to restore
.proc ReviveTarget

_8FAA:
        lda wTargetIndex
        cmp #$04
        bcc Revive			;<4 is party
        sec
        sbc #$04
        tax 				;now monster index(0-7)
        lda InactiveMonsters
        jsr SelectBit_X
        beq Revive			;check if monster is revivable
Miss:
        inc AtkMissed
        rts
Revive:									;
        ldx TargetOffset
        lda CharStruct::Status1,X
        and #$02			;zombie
        bne Miss
        lda CharStruct::Status1,X
        and #$7F			;clear dead
        sta CharStruct::Status1,X
        lda Param3
        tax
        stx $2C
        ldx TargetOffset
        lda CharStruct::MaxHP,X
        sta $2A
        lda CharStruct::MaxHP+1,X
        sta $2B
        jsr Multiply_16bit		;32 bit result in $2E
        longa
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E
        lsr $30
        ror $2E				;divide by 4 via shifts
        lda $30
        bne Cap			;cap at 9999 (high bytes)
        lda $2E
        cmp #$270F			;cap at 9999 (low bytes)
        bcc :+
Cap:	LDA #$270F
        sta $2E
:	LDA $2E
        bne :+
        inc $2E				;min 1
:	LDX TargetOffset
        clc
        lda CharStruct::CurHP,X
        adc $2E
        bcs :+				;check for overflow
        cmp CharStruct::MaxHP,X
        bcc :++
:	LDA CharStruct::MaxHP,X
:	STA CharStruct::CurHP,X		;set hp to max
        shorta0
        lda wTargetIndex
        tax
        inc ActiveParticipants,X
        rts

.endproc

; ---------------------------------------------------------------------------


;Apply Status 1 to target, bypass Status immunity
.proc ApplyStatus1Bypass

_902B:
        ldx TargetOffset
        lda CharStruct::Status1,X
        ora Param3
        sta CharStruct::Status1,X
        jsr StartStatus1Timer
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status 2 to Target, bypasses Status Immunity
;Still respects mutually exclusive status
.proc ApplyStatus2Bypass

_9039:
        ldx TargetOffset
        lda Param3
        and #$10   		;charm
        bne Charm
        lda Param3
        and #$08   		;berserk
        beq CheckPara
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$18		;charm or berserk
        bne Miss
        lda CharStruct::Status4,X
        and #$20   		;control
        bne Miss
        bra :+
Charm:	LDA CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$18   		;charm or berserk
        bne Miss
        lda CharStruct::Status4,X
        and #$20   		;control
        bne Miss
:	LDA wTargetIndex
        cmp #$04		;4+ is monster
        bcs CheckPara
        ldx AttackerOffset
        phx 			;store attacker for later
        lda wTargetIndex
        jsr ResetATB		;Y=timer offset
        ldx AttackerOffset	;is now target after ResetATB
        lda #$80
        sta CharStruct::ActionFlag,X
        stz CharStruct::Command,X
        lda wTargetIndex
        tax
        lda CurrentTimer::ATB,Y
        cmp #$7F		;caps at 127
        bcc :+
        lda #$7F
:	STA UncontrolledATB,X
        tdc
        sta EnableTimer::ATB,Y
        inc
        sta CurrentTimer::ATB,Y
        plx 			;restore original attacker
        stx AttackerOffset
        bra Finish
CheckPara:	
        lda Param3
        and #$20   		;paralyze
        beq Finish
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$20   		;paralyze
        bne Miss
        lda wTargetIndex
        jsr GetTimerOffset	;Y=timer offset
        tdc
        sta EnableTimer::ATB,Y
        inc
        sta CurrentTimer::ATB,Y
Finish:	LDX TargetOffset
        lda CharStruct::Status2,X
        ora Param3
        sta CharStruct::Status2,X
        jmp StartStatus2Timer
Miss:	INC a:AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status 3 to Target, bypasses Status Immunity
.proc ApplyStatus3Bypass

_90CD:
        ldx TargetOffset
        lda CharStruct::Status3,X
        ora Param3
        sta CharStruct::Status3,X
        jsr StartStatus3Timer
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply Status 1 to Attacker, bypasses Status Immunity
.proc ApplyStatus1AttackerBypass

_90DB:
        ldx AttackerOffset
        lda CharStruct::Status1,X
        ora Param3
        sta CharStruct::Status1,X
        jsr StartStatus1Timer
        rts

.endproc

; ---------------------------------------------------------------------------

;Remove Magic Sword Effects
.proc RemoveMagicSword

_90E9:
        ldx TargetOffset
        stz CharStruct::MSwordElemental1,X
        stz CharStruct::MSwordElemental2,X
        stz CharStruct::MSwordElemental3,X
        stz CharStruct::MSwordStatus1,X
        stz CharStruct::MSwordStatus2,X
        stz CharStruct::MSwordStatusSpecial,X
        stz CharStruct::MSwordAnim,X
        rts

.endproc

; ---------------------------------------------------------------------------


;Change Target Row
.proc TargetChangeRow

_9101:
        ldx TargetOffset
        lda CharStruct::CharRow,X
        and #$80
        eor #$80
        sta $0E
        lda CharStruct::CharRow,X
        and #$7F
        ora $0E
        sta CharStruct::CharRow,X
        rts
;       

.endproc

; ---------------------------------------------------------------------------

;Move Target to Front Row
.proc TargetFrontRow

_9117:
        ldx TargetOffset
        lda CharStruct::CharRow,X
        and #$7F
        sta CharStruct::CharRow,X
        rts
;       

.endproc

; ---------------------------------------------------------------------------

;Move Attacker to Back Row
.proc AttackerBackRow

_9122:
        ldx AttackerOffset
        lda CharStruct::CharRow,X
        ora #$80
        sta CharStruct::CharRow,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Defense & Magic Defense = Half
.proc HalveDefenses

_912D:
        ldx TargetOffset
        lsr CharStruct::Defense,X
        lsr CharStruct::MDefense,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Target Level = Half
.proc HalveLevel

_9136:
        ldx TargetOffset
        lda CharStruct::Level,X
        lsr
        bne :+
        inc 			;min 1
:	STA CharStruct::Level,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Add to Target Level
.proc AddLevel

_9143:
        ldx TargetOffset
        clc
        lda CharStruct::Level,X
        adc Param3
        bcc :+
        lda #$FF		;max 255
:	STA CharStruct::Level,X
        rts
;

.endproc

; ---------------------------------------------------------------------------

;Subtract from Target Level
.proc SubtractLevel

_9153:
        ldx TargetOffset
        sec
        lda CharStruct::Level,X
        sbc Param3
        bcs :+
        lda #$01		;min 1
:	STA CharStruct::Level,X
        rts
;

.endproc

; ---------------------------------------------------------------------------

;Double Max HP
;**optimize: party check is unnecessary here because the only place it is called from already checks
.proc DoubleMaxHP

_9163:
        lda wTargetIndex
        cmp #$04		;<4 means party
        bcc Go
        inc AtkMissed
        rts
Go:	ASL
        tay
        longa
        ldx TargetOffset
        lda CharStruct::MaxHP,X
        sta OriginalMaxHP,Y
        asl
        cmp #$270F 		;9999
        bcc :+
        lda #$270F		;cap at 9999
:	STA CharStruct::MaxHP,X
        lda CharStruct::CurHP,X
        asl
        cmp #$270F 		;9999
        bcc :+
        lda #$270F		;cap at 9999
:	STA CharStruct::CurHP,X
        shorta0
        rts
;

.endproc

; ---------------------------------------------------------------------------

;Increase Attack 
;**bug: Only increases Attack for monsters and Goblin Punch
.proc AddAttack

_9197:
        ldx TargetOffset
        clc
        lda CharStruct::MonsterAttack,X
        adc Param3
        bcc :+
        lda #$FF 		;max 255
:	STA CharStruct::MonsterAttack,X
        clc
        lda CharStruct::MonsterAttackLH,X
        adc Param3
        bcc :+
        lda #$FF		;max 255
:	STA CharStruct::MonsterAttackLH,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Increase Target Defense and Magic Defense
.proc AddDefenses

_91B4:
        ldx TargetOffset
        clc
        lda CharStruct::Defense,X
        adc Param3
        bcc :+
        lda #$FF   		;max 255
:	STA CharStruct::Defense,X
        clc
        lda CharStruct::MDefense,X
        adc Param3
        bcc :+
        lda #$FF   		;max 255
:	STA CharStruct::MDefense,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Toggle Status 4 on target
;(only called in unused attack type 56, so effectively unused)
.proc ToggleStatus4

_91D1:
        ldx TargetOffset
        lda CharStruct::Status4,X
        eor Param3
        sta CharStruct::Status4,X
        rts

.endproc

; ---------------------------------------------------------------------------

;(seems to be unused)
.proc AddSomethingUnused

_91DC:
        ldx AttackerOffset
        lda CharStruct::Unused3,X	;unused byte
        ora Param3
        sta CharStruct::Unused3,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Add Creature Type
.proc AddCreatureType

_91E7:
        ldx TargetOffset
        lda CharStruct::CreatureType,X
        ora Param3
        sta CharStruct::CreatureType,X
        rts

.endproc

; ---------------------------------------------------------------------------

					;
;Add Status 1 Immunity
.proc AddStatus1Immunity

_91F2:
        ldx TargetOffset
        lda CharStruct::StatusImmune1,X
        ora Param3
        sta CharStruct::StatusImmune1,X
        rts

.endproc

; ---------------------------------------------------------------------------

					;
;Add Magic Element Up
.proc AddElementUp

_91FD:
        ldx TargetOffset
        lda CharStruct::ElementUp,X
        ora Param3
        sta CharStruct::ElementUp,X
        rts

.endproc

; ---------------------------------------------------------------------------


;Steal Subroutine
.proc StealItem

_9208:
        lda wTargetIndex
        cmp #$04
        bcs :+
        jmp Miss
:	TDC
        tax
        lda #$FF
        jsr Random_X_A
        sta $0E			;0..255
        sec
        lda wTargetIndex
        sbc #$04		;now monster 0-7
        asl
        tax
        longa
        lda BattleMonsterID,X
        jsr ShiftMultiply_4
        tax 			;index into steal/drop table
        shorta0
        lda $0E
        cmp #$0A  		;10 out of 255
        bcc Rare
        inx 			;next item is common steal
Rare:	LDA ROMLoot::RareSteal,X
        beq Miss		;nothing in this steal slot
        sta $0E
        ldy #$00FF		;last item slot
SearchItemLoop:
        lda InventoryItems,Y
        cmp $0E
        beq Found		;found matching item
        dey
        bpl SearchItemLoop
        tdc
        tay
SearchEmptyLoop:
        lda InventoryItems,Y
        beq Found		;found empty slot
        iny
        cpy #$0100
        bne SearchEmptyLoop
        bra Miss		;no empty slots (impossible?)
Found:	LDA $0E
        sta InventoryItems,Y
        lda Temp,Y	;save this to restore later
        pha
        phy
        lda $0E
        jsr SetupInventoryInfo
        jsr GetItemUsableY
        ply
        sta InventoryUsable,Y
        pla
        sta Temp,Y	;restores value that setup overwrote
        clc
        lda InventoryQuantities,Y
        inc
        cmp #$64			;check for 100
        bcc :+
        lda #$63			;cap at 99
:	STA InventoryQuantities,Y
        ldx TargetOffset
        lda InventoryItems,Y
        sta CharStruct::StolenItem,X
        phy
        jsr SetupMsgBoxIndexes
        tyx
        ply
        lda CurrentCommand::ID
        cmp #$0B	;steal
        beq :+
        cmp #$33	;command $33 is White Magic L2, so ??
        bne :++
:	TDC
        tax
:	LDA InventoryItems,Y
        sta MessageBoxData,X
        rts
        							;
Miss:	INC AtkMissed
        rts

.endproc

; ---------------------------------------------------------------------------

;Status 1 Effect Timer
;Poison is the only Status 1 effect that has one
;Starts timer if Param3 poison bit is set
.proc StartStatus1Timer

_92A5:
        lda Param3		;Status1 here
        and #$04		;Poison
        beq Return
        lda #$01
        tax
        lda wTargetIndex
        jsr StartTimer
Return:
        rts

.endproc

; ---------------------------------------------------------------------------

;Stops timer if Param3 poison bit is set
.proc StopStatus1Timer

_92B4:
        lda Param3
        and #$04		;Poison
        beq Ret
        lda #$01		;Poison Timer
        tax
        lda wTargetIndex
        jsr StopTimer
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc StopStatus2Timer

_92C3:
        lda Param3
        and #$80		;Old
        beq :+
        lda #$06		;Old Timer
        tax
        lda wTargetIndex
        jsr StopTimer
:	LDA Param3
        and #$20		;Paralyze
        beq :+
        lda #$09		;Paralyze Timer
        tax
        lda wTargetIndex
        jsr StopTimer
:	LDA Param3
        and #$04		;Mute
        beq Ret
        lda #$04		;Mute Timer
        tax
        lda wTargetIndex
        jsr StopTimer
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

.proc StopStatus3Timer

_92EE:
        lda Param3
        and #$80		;Reflect
        beq :+
        lda #$02		;Reflect Timer
        tax
        lda wTargetIndex
        jsr StopTimer
:	LDA Param3		;Stop
        and #$10
        beq :+
        lda #$00		;Stop Timer
        tax
        lda wTargetIndex
        jsr StopTimer
:	LDA Param3
        and #$01		;Regen
        beq Ret
        lda #$07		;Regen Timer
        tax
        lda wTargetIndex
        jsr StopTimer
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;This routine is completely unused, as far as I can tell
;**optimize: delete it
.proc StopStatus4Timer

_9319:
        lda Param3
        and #$08	;HP Leak Status
        beq :+
        lda #$05	;HP Leak Timer
        tax
        lda wTargetIndex
        jsr StopTimer
:	rts

.endproc

; ---------------------------------------------------------------------------

;Status 2 Effect Timers (Old/Paralyze/Mute)
;Status in Param3/$59
.proc StartStatus2Timer

_9328:
        lda Param3
        and #$80		;Old
        beq :+
        lda #$06
        tax
        bra Finish
:	LDA Param3
        and #$20		;Paralyze
        beq :+
        lda #$09
        tax
        bra Finish
:	LDA Param3
        and #$04		;Mute
        beq Return
        lda #$04
        tax
Finish:	LDA wTargetIndex
        jsr StartTimer
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Status 3 Effect Timers
;Status in $59 / Param3
.proc StartStatus3Timer

_934D:
        lda Param3
        and #$80		;Reflect
        beq :+
        lda #$02
        tax
        bra Finish
:	LDA Param3
        and #$10		;Stop
        beq :+
        lda #$00
        tax
        bra Finish
:	LDA Param3
        and #$01		;Regen
        beq Return
        lda #$07
        tax
Finish:	LDA $48
        jsr StartTimer
Return:	rts

.endproc

; ---------------------------------------------------------------------------

;Status 4 Effect Timers (HP Leak and Countdown)
.proc Status4Timer

_9319:
        lda Param3
        and #$08		;HP Leak
        beq :+
        lda #$05
        tax
        lda wTargetIndex
        jmp StartTimer
:	LDA Param3
        and #$10		;Countdown
        beq Return
        lda #$03
        tax
        lda wTargetIndex
        jsr StartTimer
Return:	rts

.endproc

; ---------------------------------------------------------------------------


;Lose one Image
.proc LoseOneImage

_938F:
        ldx TargetOffset
        lda CharStruct::Status2,X
        pha
        and #$03   		;images
        sec
        sbc #$01
        sta $0E
        pla
        and #$FC
        ora $0E
        sta CharStruct::Status2,X
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CatchMonster

_93A5:
        sec
        lda wTargetIndex
        sbc #$04
        tax
        stx $0E			;monster index 0-7
        lda EncounterInfo::MonsterID,X
        ldx AttackerOffset
        sta CharStruct::CaughtMonster,X
        lda AttackerIndex
        sta $24
        lda #$14		;20, CharCommand structure size
        sta $25
        jsr Multiply_8bit	;Attacker Index * 20
        tdc
        tay
        ldx $26
:	LDA CharCommands::ID,X
        cmp #$1D		;Catch
        beq Found
        inx
        iny
        cpy #$0004
        bne :-
        beq Done
Found:	LDA #$1E		;Release
        sta CharCommands::ID,X
        lda #$08		;enemy by default
        sta CharCommands::Targetting,X
        inx
        iny
        bra :-
Done:   LDX TargetOffset
        lda CharStruct::Status1,X
        ora #$80		;"dead"
        sta CharStruct::Status1,X
        ldx $0E			;monster index 0-7
        lda InactiveMonsters
        jsr SetBit_X
        sta InactiveMonsters	;can't be revived
        rts

.endproc

; ---------------------------------------------------------------------------

;Subtract Defense from Damage and multiply by M
;Attack in $50
;M in $52
;Defense in $54
;result returned in $7B69
;
;**optimize: remove extra mode change
.proc CalcBaseDamage

_93F8:
        longa
        sec
        lda Attack
        sbc Defense
        beq :+
        bcs ApplyM
:
        tdc
        sta BaseDamage
        shorta
        rts
;Multiply Net Damage by M
ApplyM:
        sta $2A
        lda M
        bne :+
        inc
:
        sta $2C
        shorta0		;unnecessary, Multiply_16bit does this
        jsr Multiply_16bit 	;$2A*$2C into $2E
        ldx $2E			;Attack * M
        cpx #$270F		;9999
        bcc :+
        ldx #$270F
:
        stx BaseDamage
        rts

.endproc

; ---------------------------------------------------------------------------

.proc ProcessCommands

_9427:
        ldx #$0006
:	STZ CurrentCommand,X		;Clear 7 bytes
        dex
        bpl :-
        ldx #$001F
:	STZ BlockType,X			;Clear $20 (32) bytes
        dex 				;..includes AegisBlockTarget
        bpl :-
ProcessCommand:
        lda AttackerIndex
        jsr CalculateCharOffset
        lda MultiCommand
        tax
        lda ROMTimes12,X
        sta AttackerOffset2
        lda AtkType,X
        bpl ValidAtkType
        cmp #$FF
        bne TypeFF
        lda MultiCommand
        bne Multi
        lda AttackerIndex
        sta ActionAnim0::OrigAttacker
        stz ActionAnim0::Flags
        stz ActionAnim0::OrigTargetBits
        stz ActionAnim0::TargetBits
        stz ActionAnim0::ReflectorBits
        stz ActionAnim0::ReflecteeBits
        stz ActionAnim0::CoveredBits
Multi:	JMP Ret
TypeFF:
        inc MultiCommand
        jmp GoProcessCommand
ValidAtkType:
        lda TargetType,X
        and #$10		;weapon proc
        beq NoProc
        lda AtkMissed
        beq NoProc
        jsr DeleteCommand
        jmp ProcessCommand
NoProc:
        lda TargetType,X
        and #$EF		;bits other than weapon proc
        bne TargetTypeBits
        lda MultiCommand
        asl
        tax
        lda CommandTargetBitmask,X
        ora CommandTargetBitmask+1,X
        bne FindTarget		;no targets means target self
        lda AttackerIndex
        sta wTargetIndex
        sta AnotherTargetIndex
        bra CollectMP
FindTarget:	
        jsr FindFirstTargetPlus1
        dec
        sta wTargetIndex
        sta AnotherTargetIndex
CollectMP:
        jsr CollectMPCost
        bra Finish
TargetTypeBits:		;I think this is where a multi target attack is split up 
        		;.. into individual calls on an attack type
        		;.. but I can't understand how it works
        lda TargetAdjust
        bne :+
        lda #$FF		;-1
        sta wTargetIndex
:	JSR FindFirstTargetPlus1
        clc
        adc wTargetIndex		;dunno where it's set if not -1
        sta wTargetIndex
        sta AnotherTargetIndex
        jsr CollectMPCost
Finish:
        lda wTargetIndex
        longa
        jsr ShiftMultiply_128
        sta TargetOffset
        shorta0
        stz Crit
        stz FightFlag
        stz TargetBitMaskSmall
        stz ReflectorBitmask
        stz ReflecteeBitmask
        stz CoveredBitmask
        stz Reflected
        stz SpiritFlag
        stz MagicNull
        jsr CheckTargetRedirect
        jsr GetCharStructActionData
        jsr SetupAndLaunchAttack	;leads to attack type calcs
        jsr SetupReactionsAnims
GoProcessCommand:
        jmp ProcessCommand
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;This awkward routine finds the first set bit in CommandTargetBitmask
;then returns its index +1
;or the attacker index if none are set (or only the last monster is set, which might be a **bug?)
.proc FindFirstTargetPlus1

_94FF:
        lda MultiCommand
        asl
        tax
        tdc
        tay
        iny 		;starts at index 1
FindTargetLoop:
        asl CommandTargetBitmask+1,X
        rol CommandTargetBitmask,X
        bcs Found
        iny
        cpy #$000C	;only checks 11 bits due to starting at 1
        bne FindTargetLoop
        lda AttackerIndex 	;defaults to attacker
        tay
Found:	TYA
        rts

.endproc

; ---------------------------------------------------------------------------

.proc GetCharStructActionData

_951A:
        ldx AttackerOffset
        lda MultiCommand
        beq :+
        longa	;increment X by 5 if Multicommand non-zero
        txa 		;this uses second set of command data
        clc 		;in CharStruct later
        adc #$0005
        tax
        shorta0
:	LDA AttackerOffset2
        tay
        lda CommandOffset
        sta CurrentCommand::ID
        lda CharStruct::ActionFlag,X	;or .SecondActionFlag
        and #$20
        beq :+
        lda CharStruct::SelectedItem,X	;or .SecondSelectedItem
        sta CurrentCommand::Magic
        bra :++
:	LDA CharStruct::ActionFlag,X	;or .SecondActionFlag
        and #$40
        beq :+
        lda CharStruct::SelectedItem,X	;or .SecondSelectedItem
        sta CurrentCommand::Item
:	LDA AttackInfo::Category,Y
        sta CurrentCommand::Category
        lda CharStruct::MonsterTargets,X	; or .Second...
        ora CharStruct::PartyTargets,X
        sta CurrentCommand::Targets
        rts

.endproc

; ---------------------------------------------------------------------------

;Checks for Reflect and Cover and changes target if needed
.proc CheckTargetRedirect

_9561:
        lda AttackerOffset2
        tax
        lda AttackInfo::Category,X
        and #$0F		;Time/Black/White/Blue Magic
        bne Magic
        bra Bypass
Magic:	LDA AttackInfo::MPCost,X	;high bit is reflect
        bpl CheckReflect
        bra Bypass
CheckReflect:
        ldx TargetOffset
        lda CharStruct::Status3,X
        ora CharStruct::AlwaysStatus3,X
        bmi _Reflected
Bypass:	JMP DoneReflect
_Reflected:
        lda CharStruct::CharRow,X
        and #$40		;not on the team?
        bne Bypass
        lda CharStruct::Status4,X
        and #$C1		;Erased/False Image/Hidden
        bne Bypass
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        bne Bypass
        inc Reflected
        stz $0E			;0
        lda #$03
        sta $0F			;3 by default reflecting to party 0-3
        lda wTargetIndex
        sta $10			;Target Index
        cmp #$04		;monster check
        bcs :+
        lda #$04
        sta $0E			;4
        lda #$0B
        sta $0F			;11	reflecting to monsters 4-11
:	LDA $10			;Target Index
        cmp #$04
        bcc :+
        sec
        sbc #$04
:	TAX 			;now either party or monster index
        tdc
        jsr SetBit_X
        sta ReflectorBitmask
        jsr CheckValidTargetsExist2
        lda $11			;no valid targets if set
        beq PickRandomTarget
        tdc
        sta wTargetIndex
        tax
        stx TargetOffset	;give up and zap the first party member
        bra DoneReflect
PickRandomTarget:
        lda $0E			;min target index
        tax
        lda $0F			;max target index
        jsr Random_X_A
        sta $10			;new target index
        tay
        lda ActiveParticipants,Y
        beq PickRandomTarget
        lda $10
        longa
        jsr ShiftMultiply_128
        tax
        stx TargetOffset
        shorta0
        lda CharStruct::Status1,X
        and #$C0		;Stone or Dead
        bne PickRandomTarget
        lda CharStruct::Status4,X
        and #$81		;Erased or Hidden
        bne PickRandomTarget
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        bne PickRandomTarget
        lda $10
        sta wTargetIndex	;new target index
        cmp #$04		;monster check
        bcc :+
        sec
        sbc #$04		;now party or monster index
:	TAX
        tdc
        jsr SetBit_X
        sta ReflecteeBitmask
DoneReflect:
        lda AttackerIndex
        cmp #$04		;monster check
        bcs MonAttacker
        rts
        								;
MonAttacker:								
        lda wTargetIndex
        cmp #$04		;monster check
        bcc MonVsParty
        rts
MonVsParty:
        lda MultiCommand
        tax
        lda AtkType,X
        cmp #$01		;Monster Fight
        beq CheckCoverTarget
        cmp #$02		;Monster Specialty
        beq CheckCoverTarget
        rts
CheckCoverTarget:
        ldx TargetOffset
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        bne EarlyRet		;don't cover jumpers
        lda CharStruct::Status4,X
        and #$02		;Critical (return if not)
        beq EarlyRet		;only cover critical health
        lda CharStruct::Status2,X
        ora CharStruct::Status2,X	;**bug:should be .AlwaysStatus2
        and #$10		;Charm
        beq Cover		;don't cover charmed targets
EarlyRet:
        rts
Cover:
        lda EarthWallHP
        ora EarthWallHP+1
        bne EarlyRet		;don't cover if earth wall is doing it
        tdc
        tax
        stx $0E			;index for search loop
        stx $10			;number of found members that can cover
FindCoverLoop:	;searches for party members that can cover, stores their indexes at $2620
        ldy $0E
        lda ActiveParticipants,Y
        beq Next
        lda CharStruct::Passives2,X
        bpl Next  		;80h is Cover
        lda CharStruct::Status1,X
        ora CharStruct::AlwaysStatus1,X
        and #$42		;Stone or Zombie
        bne Next
        lda CharStruct::Status2,X
        ora CharStruct::AlwaysStatus2,X
        and #$78   		;Sleep/Paralyze/Charm/Berserk
        bne Next
        lda CharStruct::Status3,X
        and #$10   		;Stop
        bne Next
        lda CharStruct::Status4,X
        and #$81   		;Erased or Hidden
        bne Next
        lda CharStruct::CmdStatus,X
        and #$10   		;Jumping
        bne Next
        ldy $10
        lda $0E
        cmp wTargetIndex		;can't cover yourself
        beq Next
        sta Temp,Y		;temp area, store coverer's index
        inc $10
Next:	JSR NextCharOffset
        inc $0E
        lda $0E
        cmp #$04
        bne FindCoverLoop
        lda $10			;number of found members that can cover
        beq Ret
        ldx AttackerOffset
        phx
        tdc
        tax
        stx $0E			;loop index,
        stx $12			;offset into health storage
CopyCoverHP:
        ldx $0E
        lda Temp,X		;coverer's index
        jsr CalculateCharOffset
        ldy $12
        longa
        lda CharStruct::CurHP,X
        sta $262A,Y		;temp area, now holds current hp
        shorta0
        inc $12
        inc $12
        inc $0E
        lda $0E
        cmp $10			;number of members that can cover
        bne CopyCoverHP
        asl $10			;number of members that can cover * 2
        tdc
        tax
        stx $0E			;offset into temp health storage
        stx $12			;highest found hp
        stx $14			;index of coverer with highest found hp
FindHighestHP:	;finds the covering party member with the highest health, stores their index in $14
        longa
        ldx $0E
        lda $262A,X		;current hp
        cmp $12			;highest found hp
        bcc NextHP
        sta $12			;highest found hp
        shorta0
        lda $0E
        lsr
        tax
        lda Temp,X		;coverer's index
        sta $14			;index of coverer with highest found hp
        bra NextHP8b
NextHP:
        shorta0
NextHP8b:
        inc $0E
        inc $0E
        lda $0E
        cmp $10			;number of members that can cover * 2
        bne FindHighestHP
        lda wTargetIndex
        tax
        tdc
        jsr SetBit_X
        sta CoveredBitmask
        lda $14			;index of coverer with highest found hp
        sta wTargetIndex	;now the new target
        sta AnotherTargetIndex
        longa
        jsr ShiftMultiply_128
        tax
        stx TargetOffset
        shorta0
        plx 			;Restore attacker offset, though
        stx AttackerOffset	; not sure if it was ever changed?
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Makes sure there is a valid target available
;near duplicate of $4BD7 routine, just uses different memory locations to pass info
;**optimize: combine with the other routine, fixing up the memory stuff where it's used
.proc CheckValidTargetsExist2

_9728:
        lda $0F			;Ending Target Index
        tay
        iny
        sty $10			;Ending Target Index+1 (now 16-bit)
        stz $11
        lda $0E			;Starting Target Index
        tay
        longa
        jsr ShiftMultiply_128
        tax 			;Target Offset
        shorta0
FindValidTargetLoop:
        lda ActiveParticipants,Y
        beq Next
        lda CharStruct::Status1,X
        and #$C0		;Stone/Dead
        bne Next
        lda CharStruct::Status4,X
        and #$81		;Erased/Hidden
        bne Next
        lda CharStruct::CmdStatus,X
        and #$10		;Jumping
        beq Ret
Next:	JSR NextCharOffset	;X+128 for next character offset
        iny
        cpy $10
        bne FindValidTargetLoop
        inc $11
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;saves action data so reactions can be checked later
;also sets up animation flags, and anim targetting changes when redirected by cover or reflect
.proc SetupReactionsAnims

_9761:
        jsr CopyReactionInfo
        lda MultiCommand
        sta $24
        lda #$07
        sta $25
        jsr Multiply_8bit
        ldy $26				;MultiCommand * 7
        lda CurrentCommand::Magic
        cmp #$81			;Monster Specialty
        bne :+
        lda ActionAnim0::Flags,Y
        ora #$01
        sta ActionAnim0::Flags,Y
:	LDA UnknownReaction		;set by lots of things
        beq :+
        lda ActionAnim0::Flags,Y
        ora #$20
        sta ActionAnim0::Flags,Y
:	LDA SpiritFlag
        beq :+
        lda ActionAnim0::Flags,Y
        ora #$02
        sta ActionAnim0::Flags,Y
:	LDA AttackerIndex
        cmp #$04			;monster check
        bcc :+
        lda ActionAnim0::Flags,Y
        ora #$80
        sta ActionAnim0::Flags,Y
        sec
        lda AttackerIndex
        sbc #$04
:	STA ActionAnim0::OrigAttacker,Y	;party or monster index
        lda AnotherTargetIndex
        cmp #$04			;monster check
        bcc :+
        lda ActionAnim0::Flags,Y
        ora #$40
        sta ActionAnim0::Flags,Y
        sec
        lda AnotherTargetIndex
        sbc #$04
:	TAX
        lda ActionAnim0::OrigTargetBits,Y
        jsr SetBit_X
        sta ActionAnim0::OrigTargetBits,Y
        lda Crit
        ora TargetDead
        beq :+
        lda ActionAnim0::Flags,Y
        ora #$10
        sta ActionAnim0::Flags,Y
:	LDA FightFlag
        beq :+
        lda ActionAnim0::Flags,Y
        ora #$08
        sta ActionAnim0::Flags,Y
:	LDA Reflected
        bne Reflect
        lda ActionAnim0::TargetBits,Y
        ora TargetBitMaskSmall
        sta ActionAnim0::TargetBits,Y
        bra DoneReflect
Reflect:
        lda AnotherTargetIndex
        sta wTargetIndex
        lda ActionAnim0::ReflectorBits,Y
        ora ReflectorBitmask
        sta ActionAnim0::ReflectorBits,Y
        lda TargetBitMaskSmall
        beq DoneReflect
        lda MultiCommand
        jsr ShiftMultiply_8
        sta $0F			;MultiCommand*8
        lda wTargetIndex
        cmp #$04		;monster check
        bcc :+
        sec
        sbc #$04		;monster or party index
:	CLC
        adc $0F
        tax 			;Multicommand*8 + party/monster index
        lda ReflecteeBitmask
        sta CounterReflecteeTable,X
        lda ActionAnim0::ReflecteeBits,Y
        ora ReflecteeBitmask
        sta ActionAnim0::ReflecteeBits,Y
DoneReflect:
        lda CoveredBitmask
        sta ActionAnim0::CoveredBits,Y
        lda MultiCommand
        tax
        lda MultiTarget,X
        beq SingleTarget
        inc TargetAdjust
        cmp TargetAdjust
        bne Ret
SingleTarget:
        lda ActionAnim0::TargetBits,Y
        bne Finish
        ;this section is used when there's no target data
        ;i think it hides the message box with the ability name when there is no target
        ;but only for certain messages which are checked in a rom table
        jsr SetupMsgBoxIndexes
        stz $0E
        lda MessageBoxes,X
        lsr
        ror $0E
        lsr
        ror $0E
        lsr
        ror $0E			;low 3 bits of message box in high 3 of this
        tax 			;message box data/8 (selects byte in table)
        lda ROMHideMessages,X
        pha
        lda $0E
        jsr ShiftDivide_32	;5 LSRs, now bit selection for table byte
        tax
        pla
        jsr SelectBit_X
        bne Finish
        jsr SetupMsgBoxIndexes
        tdc
        sta MessageBoxes,X
Finish:	INC MultiCommand
        inc MultiDamage
        stz TargetAdjust
        stz MPTaken
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Copies information about the last attack to the character structure
;for use for reactions afterward
;there's only space for the first 2 actions to be countered
;**optimize: use loops, it's mostly contiguous (watch multi one tho)
.proc CopyReactionInfo

_9885:
        ldx TargetOffset
        lda MultiCommand
        bne Multi
        lda CurrentCommand::ID
        sta CharStruct::Reaction1Command,X
        lda CurrentCommand::Magic
        sta CharStruct::Reaction1Magic,X
        lda CurrentCommand::Item
        sta CharStruct::Reaction1Item,X
        lda CurrentCommand::Element
        sta CharStruct::Reaction1Element,X
        lda CurrentCommand::Category
        sta CharStruct::Reaction1Category,X
        lda CurrentCommand::Targets
        sta CharStruct::Reaction1Targets,X
        lda CurrentCommand::Damage
        sta CharStruct::Reaction1Damage,X
        bra Ret
Multi:	
        lda CurrentCommand::ID
        sta CharStruct::Reaction2Command,X
        lda CurrentCommand::Magic
        sta CharStruct::Reaction2Magic,X
        lda CurrentCommand::Item
        sta CharStruct::Reaction2Item,X
        lda CurrentCommand::Element
        sta CharStruct::Reaction2Element,X
        lda CurrentCommand::Category
        sta CharStruct::Reaction2Category,X
        lda CurrentCommand::Targets
        sta CharStruct::Reaction2Targets,X
        lda CurrentCommand::Damage
        sta CharStruct::Reaction2Damage,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;creates GFX command $00,FC,06,00,00
;(called at the end of a lot of commands)
.proc GFXCmdDamageNumbers

_98E3:
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC
        sta GFXQueue::Cmd,X
        lda #$06
        sta GFXQueue::Type,X
        stz GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        rts
;Either uses NextGFXQueueSlot, or searches for the first $FF entry, depending on SearchGFXQueue
;Returns slot offset in X

.endproc

; ---------------------------------------------------------------------------

.proc FindOpenGFXQueueSlot

_98FA:
        lda SearchGFXQueue
        bne SearchGFXSlots
        lda NextGFXQueueSlot
        asl
        tax
        longa
        lda ROMTimes5w,X
        tax
        shorta0
        inc NextGFXQueueSlot
        rts 			;return with X as the GFXQueue offset
;Finds first queued command with value $FF (empty slot?)
SearchGFXSlots:
        tdc
        tax
NextCommand:	
        lda GFXQueue::Flag,X
        cmp #$FF
        beq Ret
        inx 			;GFXQueue entries are 5 bytes long
        inx
        inx
        inx
        inx
        bra NextCommand
Ret:	rts			;return with X as the GFXQueue offset

.endproc

; ---------------------------------------------------------------------------

.proc SelectCurrentProcSequence

_9923:
        lda ProcSequence
        tax
        lda ROMTimes12,X
        tay
        sty $0C
        rts

.endproc

; ---------------------------------------------------------------------------

;Displays an attack's name
;Params: $2620: String Table
;	 $2621: String ID
;creates GFX command 00,FC,04,<$2620>,<$2621>
.proc GFXCmdAttackNameFromTemp

_992F:
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$04	;display attack name
        sta GFXQueue::Type,X
        lda Temp	;string table
        sta GFXQueue::Data1,X
        lda Temp+1	;string id
        sta GFXQueue::Data2,X
        rts

.endproc

; ---------------------------------------------------------------------------

;Displays a battle message
;also advances message box structure to the next set of message boxes
.proc GFXCmdMessage

_994C:
        jsr FindOpenGFXQueueSlot
        stz GFXQueue::Flag,X
        lda #$FC	;exec graphics command
        sta GFXQueue::Cmd,X
        lda #$05	;
        sta GFXQueue::Type,X
        stz GFXQueue::Data1,X
        stz GFXQueue::Data2,X
        jmp NextMessageBoxSet

.endproc

; ---------------------------------------------------------------------------

;(returns Y=$7B2C*12 and X=$7B2C*24)
;sets up the indexes into the table of messages to be displayed
;this offsets them into the correct table for each command of x-magic type multi-commands
.proc SetupMsgBoxIndexes

_9965:
        lda MultiCommand
        jsr ShiftMultiply_4
        sta $0E
        asl
        clc
        adc $0E
        tay
        lda MultiCommand
        jsr ShiftMultiply_8
        sta $0E
        asl
        clc
        adc $0E
        tax
        rts

.endproc

; ---------------------------------------------------------------------------

.proc DeleteCommand

_9980:
        lda MultiCommand
        tax
        lda ROMTimes12,X
        tay
        lda MultiCommand
        inc
        tax
        lda ROMTimes12,X
        tax
AttackInfoCopyLoop:		;shifts all of the AttackInfo Structs down, deleting the one indexed by Multicommand 						
        lda AttackInfo,X
        sta AttackInfo,Y
        inx
        iny
        cpx #$00C0		;192, 16 * 12 byte AttackInfo structs
        bne AttackInfoCopyLoop
        lda MultiCommand
        tay
        tyx
        inx
CopyTypeInfoLoop:		;shifts down, deleting the one indexed by Multicommand
        lda AtkType,X
        sta AtkType,Y
        lda MultiTarget,X
        sta MultiTarget,Y
        lda TargetType,X
        sta TargetType,Y
        inx
        iny
        cpx #$0010
        bne CopyTypeInfoLoop
        lda MultiCommand
        asl
        tay
        tyx
        inx
        inx
CopyTargetBitmaskLoop:		;shifts down, deleting the one indexed by Multicommand
        lda CommandTargetBitmask,X
        sta CommandTargetBitmask,Y
        lda CommandTargetBitmask+1,X
        sta CommandTargetBitmask+1,Y
        inx
        inx
        iny
        iny
        cpx #$0020
        bne CopyTargetBitmaskLoop
        			;wtf
        			;shifts the entire action queue down by 10 bytes (which is 2 actions)
        			;starting position depends on MultiCommand value but only seems valid for 1-3
        lda MultiCommand
        dec
        bne :+
        ldy #$000A		;10
        ldx #$0014		;20
        bra CopyActionLoop	;MultiCommand was 1
:	DEC
        bne :+
        ldy #$0014		;20
        ldx #$001E		;30
        bra CopyActionLoop	;MultiCommand was 2
:	LDY #$001E		;30
        ldx #$0028		;40
        ;multiCommand was something else, code seems to assume 3
CopyActionLoop:
        lda GFXQueue,X
        sta GFXQueue,Y
        inx
        iny
        cmp #$FF		;first byte of an empty command slot
        bne CopyActionLoop
        rts

.endproc

; ---------------------------------------------------------------------------

.proc CollectMPCost

_9A08:
        lda AttackerOffset2
        tax
        lda AttackInfo::Misc,X
        and #$07		;Meteo number of hits?
        bne Ret
        lda AttackInfo::MPCost,X
        and #$7F		;just MP cost
        tax
        stx $0E			;MP Cost
        ldx AttackerOffset
        lda CharStruct::ArmorProperties,X
        and #$08   		;Half MP
        beq :+
        lsr $0E    		;Halved MP cost
        bcc :+
        inc $0E    		;Min 1
:	LDA CharStruct::ActionFlag,X
        and #$01		;costs mp
        beq Ret
        lda MPTaken
        bne Ret
        inc MPTaken
        longa
        sec
        lda CharStruct::CurMP,X
        sbc $0E    		;Final MP cost
        bcc NotEnoughMP
        sta CharStruct::CurMP,X
        shorta0
        rts
NotEnoughMP:
        shorta0
        lda MultiCommand
        tax
        lda #$7E		;Attack Type Miss
        sta AtkType,X
        jsr SetupMsgBoxIndexes
        lda #$1E		;Not enough MP message
        sta MessageBoxes,X
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Apply Stats and Status from gear for all 4 characters
.proc ApplyPartyGear

_9A5E:
         stZ CurrentChar
:	 JSR ApplyGear
         inC CurrentChar
         ldA CurrentChar
         cmP #$04
         bnE :-
         rtS

.endproc

; ---------------------------------------------------------------------------

;Apply Stats and Status from gear ($7B7B: Character index 0-3)
.proc ApplyGear

_9A6F:
        lda CurrentChar
        jsr CalculateCharOffset
        longa
        txa
        clc
        adc #CharStruct::Headgear
        sta $0E			;pointer to equipment slot
        shorta0
        lda CurrentChar
        tax
        lda ROMTimes84,X	;*84,  7*12 byte equipment slots
        tax
        stx $10			;GearStats character offset
        stx $0A			;GearStats character offset
        tdc
        tay
        sty $14			;equipment slot index (0-6)
CopySevenItemsData:		;copy data for all 7 item slots from ROM
        ldy $14
        lda ($0E),Y		;current equipment slot
        longa
        jsr ShiftMultiply_4
        sta $16			;current equipment *4
        asl 			;current equipment *8
        clc
        adc $16
        tax 			;equipment *12, offset into ROMItems data
        shorta0
        stz $18
        ldy $10
CopyOneItemData:		;copy 12 bytes of data for current item							
        lda ROMItems,X
        sta GearStats,Y
        inx
        iny
        inc $18
        lda $18
        cmp #$0C		;12 bytes per item
        bne CopyOneItemData
        longa
        clc
        lda $10
        adc #$000C		;next item in GearStats
        sta $10
        shorta0
        inc $14			;next equipment slot index
        lda $14
        cmp #$07		;7 slots
        bne CopySevenItemsData
        tdc
        tax
        stx Temp		;scratch area
        stx TempStats
        stx TempStats+2
        stx TempStats+4
        stx TempStats+6
        stx $12
        tay
        ldx AttackerOffset
Add1000ToStats:		;copies stats to a 16-bit temp area and adds 1000 
        		;likely an attempt to handle negative numbers better, but they completely fail anyway
        lda CharStruct::BaseStr,X
        longa
        clc
        adc #$03E8		;+1000
        sta TempStats,Y
        shorta0
        inx
        iny
        iny
        inc $12
        lda $12
        cmp #$04		;4 stats, Str/Agi/Vit/Mag
        bne Add1000ToStats
        ldy $0A			;GearStats character offset
ApplyStatsElementUp:		;adds stat and element up bonuses for all 7 item slots
        lda GearStats::ElementOrStatsUp,Y
        bmi Stats
        ora Temp
        sta Temp
        longa
        bra NextItemStats
Stats:		
        .a8
        pha
        and #$07		;stat bonus bits
        asl
        tax
        lda ROMStatBonuses,X
        sta $14			;stat bonus 1
        lda ROMStatBonuses+1,X
        sta $16			;stat bonus 2
        pla
        asl
        sta $19			;stats to change
        tdc
        tax
        stz $15			;high byte of stat bonus 1
        stz $17		;**bug: these should be sign-extended if negative
        longa	;	or otherwise handled differently later
AddBonusStats:
        asl $18			;shift stat bit into carry
        bcs Bonus2
        lda $14			;bonus 1 if bit unset
        bra :+
Bonus2:	LDA $16			;bonus 2 if bit set
:	CLC
        adc TempStats,X		;adds Bonus
        sta TempStats,X		;will now be near 1255 if negative bonus
        inx
        inx
        cpx #$0008		;4 stats
        bne AddBonusStats
NextItemStats:
        tya
        clc
        adc #$000C		;next item slot in GearStats table
        tay
        shorta0
        inc $13			;item slot index
        lda $13
        cmp #$07		;7 item slots
        bne ApplyStatsElementUp
        tdc
        tax
        longa
Sub1000FromStats:															
        sec
        lda TempStats,X
        sbc #$03E8	;-1000
        bcs :+	;attempts detect and to 0 out negative numbers
        tdc 	;but due to the earlier bug this check always passes
:	STA TempStats,X
        inx
        inx
        cpx #$0008
        bne Sub1000FromStats
        tdc 		;copying just low bytes
        shorta	;this fixes negative bonuses unless they underflowed
        ldy $0A			;GearStats character offset
        ldx AttackerOffset
        lda Temp	;element up
        and #$7F	;high bit used as stats flag, can't have +water on gear
        sta CharStruct::ElementUp,X
        lda TempStats
        sta CharStruct::EquippedStr,X
        lda TempStats+2
        sta CharStruct::EquippedAgi,X
        lda TempStats+4
        sta CharStruct::EquippedVit,X
        lda TempStats+6
        sta CharStruct::EquippedMag,X
        stz CharStruct::MSwordElemental1,X
        stz CharStruct::MSwordElemental2,X
        stz CharStruct::MSwordElemental3,X
        stz CharStruct::MSwordStatus1,X
        stz CharStruct::MSwordStatus2,X
        stz CharStruct::MSwordStatusSpecial,X
        stz CharStruct::MSwordAnim,X
        lda RHWeapon::Category,Y
        sta CharStruct::RHCategory,X
        lda LHWeapon::Category,Y
        sta CharStruct::LHCategory,X
        lda RHWeapon::AtkPower,Y
        sta CharStruct::MonsterAttack,X
        lda LHWeapon::AtkPower,Y
        sta CharStruct::MonsterAttackLH,X
        lda RHWeapon::Properties,Y
        ora LHWeapon::Properties,Y
        sta CharStruct::WeaponProperties,X
        clc
        lda RHShield::ShieldEvade,Y
        adc LHShield::ShieldEvade,Y
        cmp #$63
        bcc :+
        lda #$63	;99 cap	for Evade
:	STA CharStruct::Evade,X
        clc
        lda Headgear::Defense,Y
        adc Bodywear::Defense,Y
        bcs :+
        adc Accessory::Defense,Y
        bcs :+
        adc RHShield::Defense,Y
        bcs :+
        adc LHShield::Defense,Y
        bcc :++
:	LDA #$FF	;255 cap for Defense
:	STA CharStruct::Defense,X
        clc
        lda Headgear::MEvade,Y
        adc Bodywear::MEvade,Y
        adc Accessory::MEvade,Y
        adc RHShield::MEvade,Y
        adc LHShield::MEvade,Y
        cmp #$63
        bcc :+
        lda #$63	;99 cap	for MEvade
:	STA CharStruct::MEvade,X
        clc
        lda Headgear::MDefense,Y
        adc Bodywear::MDefense,Y
        bcs :+
        adc Accessory::MDefense,Y
        bcs :+
        adc RHShield::MDefense,Y
        bcs :+
        adc LHShield::MDefense,Y
        bcc :++
:	LDA #$FF        ;255 cap for MDef
:	STA CharStruct::MDefense,X
        lda Headgear::Properties,Y
        ora Bodywear::Properties,Y
        ora Accessory::Properties,Y
        ora RHShield::Properties,Y
        ora LHShield::Properties,Y
        sta CharStruct::ArmorProperties,X
        tdc
        tax
        stx Temp
        stx Temp+2		;reusing memory that was TempStats
        stx Temp+4
        stx $0E			;armor slot index
        ldy $0A			;GearStats character offset
        sty $10
CopyArmorElementDef:													
        ldy $10			;item offset
        lda GearStats::ElementDef,Y
        longa
        sta $12
        jsr ShiftMultiply_4
        clc
        adc $12			;ROMElementDef offset (ElementDef*5)
        tax
        shorta0
        tay
CopyROMElementDef:													
        lda ROMElementDef,X
        ora Temp,Y
        sta Temp,Y
        inx
        iny
        cpy #$0005	;5 bytes absorb, evade, immunity, half, weakness
        bne CopyROMElementDef
        longa
        lda $10		;item offset
        clc
        adc #$000C	;next item
        sta $10
        shorta0
        inc $0E		;armor slot index, next slot
        lda $0E
        cmp #$05	;5 armor slots
        bne CopyArmorElementDef
        tdc
        tay
        ldx AttackerOffset
ApplyElementDef:													
        lda Temp,Y
        sta CharStruct::EAbsorb,X
        inx
        iny
        cpy #$0005	;5 bytes absorb, evade, immunity, half, weakness
        bne ApplyElementDef
        stz Temp
        stz Temp+1
        stz Temp+2
        stz $0E
        ldx $0A
        stx $10		;GearStats character offset
CopyROMStatusImmunities:												
        ldy $10
        lda GearStats::Status,Y
        sta $24
        lda #$07
        sta $25
        jsr Multiply_8bit
        tdc
        tay
        ldx $26		;ROMArmorStatus offset (GearStats::Status*7)
CopyROMImmunities:													
        lda ROMArmorStatus::Immune1,X
        ora Temp,Y
        sta Temp,Y
        inx
        iny
        cpy #$0003	;3 bytes of immunities
        bne CopyROMImmunities
        lda $0E		;armor slot index
        pha
        jsr ApplyEquipmentStatus
        pla
        sta $0E
        longa
        lda $10		;GearStats offset
        clc
        adc #$000C	;next item
        sta $10
        shorta0
        inc $0E		;armor slot index, next slot
        lda $0E
        cmp #$05	;5 armor slots
        bne CopyROMStatusImmunities
        tdc
        tay
        ldx AttackerOffset
ApplyImmunities:													
        lda CharStruct::StatusImmune1,X
        ora Temp,Y
        sta CharStruct::StatusImmune1,X
        inx
        iny
        cpy #$0003	;Status 1-3 immunity
        bne ApplyImmunities
        rts

.endproc

; ---------------------------------------------------------------------------

;Apply status from equipment 
;uses $26 as offset into ROMArmorStatus table
;due to design or possibly a **bug, only the last piece of a gear with always status will apply (for each type)
;zombie from equipment is also bugged, but doesn't exist in vanilla ff5
.proc ApplyEquipmentStatus

_9D01:
        stz $13		;set to 1 for always status, 0 for initial
        ldy AttackerOffset
        ldx $26		;ROMArmorStatus Offset
        lda ROMArmorStatus::Status1,X
        beq Status2
        sta $12		;Status 1 to apply
        bmi AlwaysS1
        and #$04	;poison
        beq ApplyS1	;only poison needs a timer
        lda $12
        and #$FB	;statuses except poison
        sta $12
        lda CharStruct::Status1,Y
        ora CharStruct::AlwaysStatus1,Y
        and #$04	;poison
        bne ApplyS1	;already poisoned, don't need a new timer
        lda CharStruct::Status1,Y
        ora #$04	;set poison
        sta CharStruct::Status1,Y
        lda #$01	;poison timer
        jsr StartTimerCurrentChar
        bra ApplyS1
AlwaysS1:		;high bit indicates always status instead of just initial
        inc $13		;always status instead of initial (for later statuses)
        lda $12		;Status 1 to apply
        and #$7F	;clear always bit because it also means dead
        sta CharStruct::AlwaysStatus1,Y
        bra Status2
ApplyS1:									;:
        ldy AttackerOffset
        lda CharStruct::Status1,Y
        ora $12
        sta CharStruct::Status1,Y
Status2:									
        ldy AttackerOffset
        ldx $26		;ROMArmorStatus Offset
        lda ROMArmorStatus::Status2,X
        sta $12		;Status 2 to apply
        lda CharStruct::Job,Y
        cmp #$06	;Berserker
        beq Berserker
        lda CharStruct::Status1,Y
        ora CharStruct::AlwaysStatus1,Y
        and #$02	;Zombie
        bne Zombie
        beq CheckStatus2
Berserker:	
        lda EncounterInfo::IntroFX
        bmi Zombie	;don't berserk during the credits demo battles
        lda #$08	;Berserk
        sta CharStruct::AlwaysStatus2,Y
Zombie:			;zombie from equipment is bugged, but doesn't exist anyway					
        lda CurrentChar
        tax
        lda #$3C
        sta UncontrolledATB,X
        lda CurrentChar
        asl
        tax
        lda ROMTimes11w,X	;**bug: 16 bit table accessed in 8 bit mode
        tax 			;char index*11 (wrong char due to bug)
        tdc
        sta EnableTimer::ATB,X	;disable normal ATB
        inc
        sta CurrentTimer::ATB,X	;normal ATB timer set at 1
CheckStatus2:								
        lda $12		;Status 2 to apply
        bne :+
        jmp Status3	;Nothing to apply
:	STA $12
        lda $13		;always status
        bne AlwaysS2
        lda $12		;Status 2 to apply
        and #$A4	;old, paralyze, mute
        beq ApplyS2	;only those need timers
        lda $12		;Status 2 to apply
        and #$80	;old
        beq CheckPara
        lda $12
        and #$7F	;-old
        sta $12		;Status 2 to apply, without old
        lda CharStruct::Status2,Y
        ora CharStruct::AlwaysStatus2,Y
        and #$80	;old
        bne CheckPara	;already old
        lda CharStruct::Status2,Y
        ora #$80	;+old
        sta CharStruct::Status2,Y
        lda #$06	;old timer
        jsr StartTimerCurrentChar
CheckPara:		;oddly inefficient compared to the other statuses
        lda $12
        and #$20	;paralyze
        beq CheckMute
        lda $12		;Status 2 to apply
        sta $14
        and #$DF	;without old
        sta $12		;Status 2 to apply without old
        ldy AttackerOffset
        lda CharStruct::Status2,Y
        ora CharStruct::AlwaysStatus2,Y
        and #$20	;paralyze
        bne CheckMute	;already paralyzed
        lda $14		;Status 2 to apply
        and #$20
        sta $14		;only paralyze
        lda CharStruct::Status2,Y
        ora $14		;+paralyze
        sta CharStruct::Status2,Y
        lda #$09	;paralyze timer
        jsr StartTimerCurrentChar
CheckMute:
        lda $12		;Status 2 to apply
        and #$04	;mute
        beq GoApplyS2
        lda $12
        and #$FB
        sta $12		;Status 2 to apply without mute
        ldy AttackerOffset
        lda CharStruct::Status2,Y
        ora CharStruct::AlwaysStatus2,Y
        and #$04	;mute
        bne GoApplyS2	;already mute
        lda CharStruct::Status2,Y
        ora #$04	;+mute
        sta CharStruct::Status2,Y
        lda #$04	;mute timer
        jsr StartTimerCurrentChar
GoApplyS2:								
        bra ApplyS2
AlwaysS2:
        lda $12		;Status 2 to apply
        sta CharStruct::AlwaysStatus2,Y
        bra Status3
ApplyS2:
        ldy AttackerOffset
        lda CharStruct::Status2,Y
        ora $12		;+Status 2
        sta CharStruct::Status2,Y
Status3:
        ldy AttackerOffset
        ldx $26		;ROMArmorStatus Offset
        lda ROMArmorStatus::Status3,X
        bne :+
        jmp Status4	;no status 3 to apply
:	STA $12		;status 3 to apply
        lda $13		;always status
        bne AlwaysS3
        lda $12		;status 3 to apply
        and #$91	;reflect/stop/regen
        beq ApplyS3	;only those need timers
        lda $12
        and #$80	;Reflect
        beq CheckStop
        lda $12
        and #$7F	;status 3 to apply without reflect
        sta $12
        lda CharStruct::Status3,Y
        ora CharStruct::AlwaysStatus3,Y
        and #$80	;reflect
        bne CheckStop	;already reflected
        lda CharStruct::Status3,Y
        ora #$80	;+reflect
        sta CharStruct::Status3,Y
        lda #$02	;reflect timer
        jsr StartTimerCurrentChar
CheckStop:					
        lda $12		;status 3 to apply
        and #$10	;Stop
        beq CheckRegen
        lda $12
        and #$EF	;status 3 to apply without stop
        sta $12
        ldy AttackerOffset
        lda CharStruct::Status3,Y	;always stop isn't a thing, apparently
        and #$10	;stop
        bne CheckRegen	;already stopped
        lda CharStruct::Status3,Y
        ora #$10	;+stop
        sta CharStruct::Status3,Y
        lda #$00	;timer for stop status
        jsr StartTimerCurrentChar
CheckRegen:								
        lda $12		;status 3 to apply
        and #$01	;regen
        beq GoApplyS3
        lda $12
        and #$FE	;status 3 to apply without regen
        sta $12
        ldy AttackerOffset
        lda CharStruct::Status3,Y
        ora CharStruct::AlwaysStatus3,Y
        and #$01	;regen
        bne GoApplyS3	;already regen
        lda CharStruct::Status3,Y
        ora #$01	;+regen
        sta CharStruct::Status3,Y
        lda #$07	;regen timer
        jsr StartTimerCurrentChar
GoApplyS3:
        bra ApplyS3
AlwaysS3:
        lda $12		;status 3 to apply
        sta CharStruct::AlwaysStatus3,Y
        bra Status4
ApplyS3:
        ldy AttackerOffset
        lda CharStruct::Status3,Y
        ora $12		;+status 3
        sta CharStruct::Status3,Y
Status4:
        ldy AttackerOffset
        ldx $26		;ROMArmorStatus Offse
        lda ROMArmorStatus::Status4,X
        beq Ret
        sta $12		;Status 4 to apply
        lda $13		;always status
        bne AlwaysS4
        lda $12
        and #$18	;Countdown and HP Leak
        beq ApplyS4	;only those need timers
        lda $12
        and #$10	;countdown
        beq CheckLeak
        lda $12
        and #$EF
        sta $12		;Status 4 to apply without countdown
        lda CharStruct::Status4,Y
        ora CharStruct::AlwaysStatus4,Y
        and #$10	;countdown
        bne CheckLeak	;already countdown
        lda CharStruct::Status4,Y
        ora #$10	;+countdown
        sta CharStruct::Status4,Y
        lda #$03	;timer for countdown status
        jsr StartTimerCurrentChar
CheckLeak:								
        lda $12		;Status 4 to apply
        and #$08	;HP Leak
        beq GoApplyS4
        lda $12
        and #$F7	;Status 4 to apply without HP Leak
        sta $12
        ldy AttackerOffset
        lda CharStruct::Status4,Y
        ora CharStruct::AlwaysStatus4,Y
        and #$08	;hp leak
        bne CheckLeak	;**bug: wrong target here, fortunately harmless
        lda CharStruct::Status4,Y
        ora #$08	;+hp leak
        sta CharStruct::Status4,Y
        lda #$05	;hp leak timer
        jsr StartTimerCurrentChar
GoApplyS4:								
        bra ApplyS4
AlwaysS4:
        lda $12		;Status 4 to apply
        sta CharStruct::AlwaysStatus4,Y
        bra Ret
ApplyS4:
        ldy AttackerOffset
        lda CharStruct::Status4,Y
        ora $12		;+Status 4
        sta CharStruct::Status4,Y
Ret:	rts

.endproc

; ---------------------------------------------------------------------------

;Start Timer for currently processed character (CurrentChar)
;(A: #timer, $7B7B: character index)
.proc StartTimerCurrentChar

_9F2E:
        tax
        lda #$01
        sta StatusFixedDur
        lda CurrentChar
        jmp StartTimer

.endproc

; ---------------------------------------------------------------------------

;Replaces the items in CharStruct hand fields with the items from HandItems
;presumably for when they're changed in the item menu
.org $9F3A
.proc ReplaceHands

_9F3A:
        stz $12
        lda DisplayInfo::CurrentChar
        jsr CalculateCharOffset
        lda DisplayInfo::CurrentChar
        jsr ShiftMultiply_4
        sta $0E
        asl
        clc
        adc $0E
        tay 		;current char*12
        lda HandItems::ID,Y
        sta $10		;first hand item id
        lda HandItems::ID+1,Y
        sta $11		;second hand item id
        cmp $10
        bcs HandsOrdered
        pha 		;swapping hands	so they're in order
        lda $10
        sta $11
        pla
        sta $10
        inc $12		;flag that hands swapped, to fix at end
HandsOrdered:
        lda $10
        ora $11
        bne NotEmpty
        tay 		;empty hands
        sty $16
        inc
        sta $14
        sta $15
        bra Finish
NotEmpty:
        lda $10
        bne FirstOccupied
        lda $11		;other hand is empty
        cmp #$80	;armor
        bcs OnlyShield
        stz $14		;only weapon
        lda $11
        sta $15
        stz $16
        stz $17
        bra Finish
OnlyShield:		;only shield
        tdc
        inc
        sta $14
        stz $15
        stz $16
        lda $11
        sta $17
        bra Finish
FirstOccupied:
        lda $10
        cmp #$80	;armor
        bcc Weapon
        stz $14		;two shields
        stz $15
        lda $10
        sta $16
        lda $11
        sta $17
        bra Finish
Weapon:			;weapon and something
        lda $11
        cmp #$80	;armor
        bcc TwoWeapons
        lda $10		;weapon and shield
        sta $14
        stz $15
        stz $16
        lda $11
        sta $17
        bra Finish
TwoWeapons:		;two weapons
        lda $10
        sta $14
        lda $11
        sta $15
        stz $16
        stz $17
Finish:
        jsr SwapHands
        lda $14
        sta CharStruct::RHWeapon,X
        lda $15
        sta CharStruct::LHWeapon,X
        lda $16
        sta CharStruct::RHShield,X
        lda $17
        sta CharStruct::LHShield,X
        rts

.endproc

; ---------------------------------------------------------------------------

;swaps some temp variables if $12 is Set
;utility code for ReplaceHands routine
;$14 <-> $15
;$16 <-> $17
.proc SwapHands

_9FE7:
        lda $12		;flag that hands were swapped
        beq Ret	;if not, don't do anything
        lda $14
        pha
        lda $15
        sta $14
        pla
        sta $15
        lda $16
        pha
        lda $17
        sta $16
        pla
        sta $17
Ret:	rts
;if !_CombatTweaks 
;        incsrc "damagetweaks.asm"
;endif
;if !_CombatTweaks
;        incsrc "attacktweaks.asm"
;endif
;print "C2 bank combat code ended one byte before ",pc
;print "Free space before start of menu code: ",$C2A000-pc," bytes"	;doesnt work, can't do math with pc here
;warnpc $C2A000	;make sure code hasn't grown beyond available space
;pad $C2A000	;$C2A000 is the start of the menu code
        	;fortunately it is programmed as if it is in its own bank, so does not call any code from the battle portion of C2

.endproc
; ---------------------------------------------------------------------------

;============================ Stud Definitions for compilation purposes =====