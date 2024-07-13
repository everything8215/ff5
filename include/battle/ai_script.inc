; +----------------------------------------------------------------------------+
; |                                                                            |
; |                              FINAL FANTASY V                               |
; |                                                                            |
; +----------------------------------------------------------------------------+
; | file: ai_script.inc                                                        |
; |                                                                            |
; | description: macros for making ai coding easier to understand              |
; |                                                                            |
; |                                                                            |
; +----------------------------------------------------------------------------+

.ifndef AI_SCRIPT_INC
AI_SCRIPT_INC = 1

; === How to code AI in FF5 ===
;
; AI in FF5 consists of two sections: Conditions and Blocks, and they always 
; come in pairs. There may be up to 4 conditions sequentially, and in order for
; the block to execute all conditions must be true (AND operator is applied)
; A block always starts with $FE (begin_block macro) and ends with $FF for normal
; behaviour blocks and $FE for all other condition blocks
; Blocks consist of either actions that are taken sequentially, or can be broken
; up into lists with $FD which indicates either a random_choice of 3 actions, or
; if the first byte after $FD is greater than $F0 indicates a special command
; such as show_tex, show_monster, or any other macros defined below 

; -------------------------------AI Conditions---------------------------------

;AI Condition $00: Always Succeed
.mac normal_react
    .byte $00, $00, $00, $00
.endmac

;AI Condition 01: Check Status
;Param1: AITarget routine
;Param2: Status offset (0-3 for status 1-4)
;Param3: Status bits
;if checking for death status, also succeed if hp is 0 (though this behavior is bugged)
.mac check_status ai_target_routine, status_offset, status_bits
    .byte $01, ai_target_routine, status_offset, status_bits
.endmac

;AI Condition 02: HP less than valued
;Param1: AITarget routine
;Param2: HP
.mac if_hp_less ai_target_routine, hp
    .byte $02, ai_target_routine
    .word hp
.endmac

;AI Condition 03: Check Variable
;Param1: Var to check (0-3)
;Param2: Value
.mac if_var_eq var, val
    .byte $03, $00, var, val
.endmac

;AI Condition 04: Alone 
;succeeds when completely alone
.mac if_alone
    .byte $04, $00, $00, $00
.endmac

;AI Condition 04: Alone 
;succeeds when all active monsters are the same
.mac if_monsters_same
    .byte $04, $00, $01, $00 
.endmac

;AI Condition 05: Compare Visible Monsters
;Param1: if 0, succeeds if visible monsters match provided value
;     if non-0, succeeds if they do not match
;Param2: Monster Bits (1 bit per monster)
.mac if_monster_visible is_inverted, monster_bitmask
    .byte $05, is_inverted, $00, monster_bitmask
.endmac

;AI Condition 06: Reaction to Command and/or Element
;Param1: if set, inverts test so a match fails the condition, and ignores element when checking commands
;Param2: Command (post-remap values)
;     Command $07, normally BuildUp, is used as a flag to skip the command check and just check element
;Param3: Element (ignored if zero)
.mac react_to is_inverted, command, element
    .byte $06, is_inverted, command, element
.endmac

;AI Condition $07: Reaction to Command and/or Category
;Param1: if set, inverts test so a match fails the condition, and ignores category when checking commands
;Param2: Command (post-remap values)
;     Command $07, normally BuildUp, is used as a flag to skip the command check and just check category
;Param3: Category (ignored if zero)
.mac react_to_cmd is_inverted, command, category
    .byte $07, is_inverted, command, category
.endmac

;AI Condition $08: Reaction to Magic
;Param1: if set, inverts test so a match fails the condition
;Param2: Spell
.mac react_to_magic is_inverted, spell
    .byte $08, is_inverted, spell, $00
.endmac

;AI Condition $09: Reaction to Item
;Param1: if set, inverts test so a match fails the condition
;Param2: Item
.mac react_to_item is_inverted, item
    .byte $09, is_inverted, item, $00
.endmac

;AI Condition $0A: Reaction to Targetting
; succeeds when attack was single target
.mac if_single_target
    .byte $0A, $00, $00, $00
.endmac

;AI Condition $0A: Reaction to Targetting
; succeeds when attack was multi target
.mac if_multi_target
    .byte $0A, $00, $00, $01
.endmac

;AI Condition 0B: Check CharStruct param
;Param1: AITarget routine
;Param2: Offset within CharStruct to check
;Param3: Value for success
.mac check_char_param ai_target, struct_offset, value
    .byte $0B, ai_target, struct_offset, value
.endmac

;AI Condition $0C: Compare with value at $A2?
;Param2/3: 16 bit Value to compare, succeeds if >= value at $A2
.mac a2_check addr
    .byte $0C, $00
    .word addr
.endmac

;AI Condition $0D: Check Event Flags
;Param2: Event Flag # (0-15)
;Param3: Value (bitmask)
;There's a special case for Event Flag 03
.mac check_event_flag flag, bitmask
    .byte $0D, $0, flag, bitmask
.endmac
 
 ;AI Condition $0E: Reaction to Damage
.mac react_to_dmg
    .byte $0E, $00, $00, $00
.endmac

;AI Condition $0F: Sets flag to skip dead monster checks (Always Succeeds)
.mac if_dying
    .byte $0F, $00, $00, $00
.endmac

;AI Condition $10: One party member alive
.mac one_party_member_alive
    .byte $10, $00, $00, $00
.endmac

;AI Condition $11: Reaction to Summon
.mac react_to_summon
    .byte $11, $00, $00, $00
.endmac

;AI Condition $12: No Female targets available
.mac no_female
    .byte $12, $00, $00, $00
.endmac

; ---------------------------AI Special Commands-------------------------------

;AI Command $F2, Show Monster
;Param1/Type:      bitfield but only $40 (random) is supported
;Param2/Data1:     bitfield of valid monsters to show
;     .. or if 0, picks a random monster other than self
.mac show_monster param1, param2
    .byte $FD, $F2, param1, param2
.endmac

;AI Command $F3: Set Target
;Param1/Type: AITarget routine
.mac change_target target, attack
    .byte $FD, $F3, target, attack
.endmac

;AI Command $F4: Set Variable
;Param1: Variable #
;Param2: Value
.mac set_cond_var var, val
    .byte $FD, $F4, var, val
.endmac

;AI Command $F5: Show Text
;Param1: Text Offset
.mac show_text addr
    .byte $FD, $F6
    .word addr
.endmac

;AI Command $F7: No Interrupt
; Param1: addr of where no-interrupt stops. Everything inbetween will be 
.mac no_interrupt addr
    .local start
    .byte $FD, $F7, addr-start, $F0
    start := *
.endmac

.mac end_interrupt
    :
.endmac

;AI Command $F8: Change Music
; Param1: Changes music to specified offset
.mac change_music music
    .byte $FD, $F8
    .word music
.endmac

;AI Command $F9: Set Event Flag
;Param1: Event Flags Byte
;Param2: Bits to set
.mac set_event_flag flag, bitmask
    .byte $FD, $F9, flag, bitmask
.endmac

;AI Command $FA: Set Stats or Toggle Status 
;Param1: Offset within CharStruct
;Param2: Value to set, or Status to toggle (only highest status bit applies)
.mac set_char_param offset, value
    .byte $FD, $FA, offset, value
.endmac

; Begins a block
.mac begin_block
    .byte $FE
.endmac

; Required at the end of normal conditions, as well as if a normal condition is
; the very last block in the ai
.mac end_block
    .byte $FF
.endmac

; Required for the end of any non-normal condition
.mac end_cond_block
    .byte $FE
.endmac

.mac random_choice atk1, atk2, atk3
    .byte $FD, atk1, atk2, atk3
.endmac

.endif