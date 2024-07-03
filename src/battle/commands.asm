.p816

; ===========================================================================

.include "../../include/macros.inc"
.include "../../include/hardware.inc"
.include "../../include/const.inc"
.include "battle_ram.inc"
.include "battle-main.asm"
.reloc


; ---------------------------------------------------------------------------

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
; ---------------------------------------------------------------------------

;Command $04 (Defend)
.proc CommandTable03

_07A4:
        lda #$03
        jmp NoActionAbility
; ---------------------------------------------------------------------------

;Command $06 (Guard)
.proc CommandTable05

_07A9:
        lda #$05
        jmp NoActionAbility
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
        bne MSword
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
        jsr ShiftMultiply_8
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

.endscope

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