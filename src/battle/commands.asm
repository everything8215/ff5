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
:	lda CharStruct::SelectedItem,X
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
:	lda !ROMConsumables,X
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
:	lda !ROMConsumables,X
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