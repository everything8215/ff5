; +----------------------------------------------------------------------------+
; |                                                                            |
; |                              FINAL FANTASY V                               |
; |                                                                            |
; +----------------------------------------------------------------------------+
; | file: ai_script.inc                                                         |
; |                                                                            |
; | description: global constant definitions                                   |
; |                                                                            |
; |                                                                            |
; +----------------------------------------------------------------------------+

.ifndef AI_SCRIPT_INC
AI_SCRIPT_INC = 1

.mac normal_react
    .byte $00, $00, $00, $00
.endmac

.mac check_status ai_target_routine, status_offset, status_bits
    .byte $01, ai_target_routine, status_offset, status_bits
.endmac

.mac if_hp_less hp
    .byte $02, $0D
    .word hp
.endmac

.mac if_var_eq var, val
    .byte $03, $00, var, val
.endmac

.mac if_alone are_monsters_same
    .byte $04, $00, are_monsters_same, $00
.endmac

.mac if_monster_visible is_inverted, monster_bitmask
    .byte $05, is_inverted, $00, monster_bitmask
.endmac

.mac react_to is_inverted, command, element
    .byte $06, is_inverted, command, element
.endmac

.mac react_to_cmd is_inverted, command, category
    .byte $07, is_inverted, command, category
.endmac

.mac react_to_magic is_inverted, spell
    .byte $08, is_inverted, spell, $00
.endmac

.mac react_to_item is_inverted, item
    .byte $09, is_inverted, item, $00
.endmac

.mac react_to_targetting is_multi_target
    .byte $0A, $00, $00, is_multi_target
.endmac

.mac check_char_param ai_target, struct_offset, value
    .byte $0B, ai_target, struct_offset, value
.endmac

.mac a2_check addr
    .byte $0C, $00
    .word addr
.endmac

.mac check_event_flag flag, bitmask
    .byte $0D, $0, flag, bitmask
.endmac
 
.mac react_to_dmg
    .byte $0E, $00, $00, $00
.endmac

.mac if_dying
    .byte $0F, $00, $00, $00
.endmac

.mac one_party_member_alive
    .byte $10, $00, $00, $00
.endmac

.mac react_to_summon
    .byte $11, $00, $00, $00
.endmac

.mac no_female
    .byte $12, $00, $00, $00
.endmac

; ======= Special Commands =======

.mac show_monster param1, param2
    .byte $FD, $F2, param1, param2
.endmac

.mac change_target target, attack
    .byte $FD, $F3, target, attack
.endmac

.mac set_cond_var var, val
    .byte $FD, $F4, var, val
.endmac

.mac show_text addr
    .byte $FD, $F6
    .word addr
.endmac

.mac no_interrupt length
    .byte $FD, $F7, length, $F0
.endmac

.mac change_music music
    .byte $FD, $F8
    .word music
.endmac

.mac set_event_flag flag, bitmask
    .byte $FD, $F9, flag, bitmask
.endmac

.mac set_char_param offset, value
    .byte $FD, $FA, offset, value
.endmac

.mac begin_block
    .byte $FE
.endmac

.mac end_special_block
    .byte $FE
.endmac

.mac end_block
    .byte $FF
.endmac

.mac random_choice atk1, atk2, atk3
    .byte $FD, atk1, atk2, atk3
.endmac

.endif