; ------------------------------------------------------------------------------

.segment "ai_script"
.include "battle/ai_script.inc"

; ------------------------------------------------------------------------------

AI_SCRIPT_ARRAY_LENGTH = 384

AIScriptPtr:
        make_ptr_tbl_abs AIScript, AI_SCRIPT_ARRAY_LENGTH

begin_fixed_block AIScript, $28FF

; ------------------------------------------------------------------------------


AIScript_00:
_109F00:
    normal_react
    begin_block
        random_choice $80, $80, $93
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_01:
_109F0B:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_02:
_109F16:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_03:
_109F21:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_04:
_109F2C:
    normal_react
    begin_block
        .byte $80
        random_choice $98, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_05:
_109F38:
    normal_react
    begin_block
        random_choice $80, $81, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_06:
_109F43:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_07:
_109F4B:
    normal_react
    begin_block
        random_choice $80, $80, $93
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_08:
_109F56:
    normal_react
    begin_block
        random_choice $80, $BC, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_09:
_109F61:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $81, $8F
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_0A:
_109F70:
    normal_react
    begin_block
        random_choice $81, $81, $80
        random_choice $81, $80, $80
        change_target $23, $F0
        random_choice $24, $25, $26
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_0B:
_109F87:
    normal_react
    begin_block
        show_text $E803
        .byte $DF
    end_block

    react_to_item $00, $E3
    begin_block
        show_text $E902
        random_choice $A2, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_0C:
_109FA0:
    no_female
    begin_block
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        change_target $18, $F0
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_0D:
_109FB6:
    no_female
    begin_block
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        change_target $18, $F0
        random_choice $80, $80, $81
        change_target $18, $F0
        random_choice $CE, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_0E:
_109FD4:
    normal_react
    begin_block
        .byte $80
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $81, $81, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_0F:
_109FE5:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $81
    end_block

    react_to $00, $2B, $01
    begin_block
        .byte $A2
    end_block

; ------------------------------------------------------------------------------


AIScript_10:
_109FF7:
    normal_react
    begin_block
        .byte $80
        random_choice $81, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_11:
_10A003:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_12:
_10A00E:
    normal_react
    begin_block
        .byte $AA, $AA, $AA, $A2
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_13:
_10A019:
    normal_react
    begin_block
        .byte $80
        random_choice $81, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_14:
_10A025:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_15:
_10A02D:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_16:
_10A042:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_17:
_10A04D:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_18:
_10A055:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_19:
_10A060:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $81, $B3
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_1A:
_10A06C:
    normal_react
    begin_block
        random_choice $80, $8B, $81
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_1B:
_10A081:
    if_alone
    begin_block
        random_choice $80, $89, $80
    end_cond_block

    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_1C:
_10A093:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $80, $25
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_1D:
_10A0A2:
    normal_react
    begin_block
        random_choice $28, $37, $3D
        random_choice $12, $2D, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_1E:
_10A0B1:
    if_alone
    begin_block
        random_choice $80, $DA, $DA
    end_cond_block

    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_1F:
_10A0C3:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_20:
_10A0CF:
    normal_react
    begin_block
        random_choice $D4, $99, $80
        random_choice $80, $1A, $AB
        random_choice $99, $80, $D4
        random_choice $D3, $AB, $80
    end_block

    if_dying
    react_to_magic $00, $4D
    begin_block
        no_interrupt
            show_text $0805
            show_text $0905
            show_text $3B05
            show_text $3C05
            .byte $AA
    end_cond_block
    end_interrupt

    if_dying
    begin_block
        no_interrupt
            show_text $3905
            show_text $3A05
            show_text $3B05
            show_text $3C05
            .byte $AA
    end_cond_block
    end_interrupt

    if_hp_less $0D, $0320
    react_to_dmg
    begin_block
        random_choice $2D, $2D, $AA
    end_cond_block

    react_to $01, $2B, $00
    begin_block
        random_choice $16, $AA, $AA
    end_cond_block

    react_to $00, $2B, $00
    begin_block
        random_choice $29, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_21:
_10A141:
    normal_react
    begin_block
        random_choice $80, $80, $81
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_22:
_10A14D:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_23:
_10A158:
    normal_react
    begin_block
        random_choice $80, $80, $9C
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_24:
_10A167:
    normal_react
    begin_block
        random_choice $80, $80, $81
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_25:
_10A173:
    normal_react
    begin_block
        random_choice $80, $C1, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_26:
_10A17E:
    if_alone
    begin_block
        random_choice $80, $80, $81
    end_cond_block

    if_hp_less $0D, $012C
    begin_block
        change_target $10, $F0
        .byte $96
    end_cond_block

    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_27:
_10A19B:
    normal_react
    begin_block
        random_choice $DE, $2C, $CE
        random_choice $2C, $8B, $DE
        random_choice $DE, $2C, $CE
        random_choice $2C, $DC, $35
    end_block

    if_dying
    begin_block
        show_text $6C03
        show_text $6D03
        show_text $6E03
        show_text $6F03
        show_text $7003
        show_text $7103
        show_text $7203
        set_event_flag $01, $80
        .byte $AA
    end_cond_block

    .byte $08, $00, $17, $F0
    ;react_to_magic $00, $17    ; TODO: figure out why this happens
    begin_block
        change_target $23, $F0
        .byte $17
    end_block

; ------------------------------------------------------------------------------


AIScript_28:
_10A1E3:
    if_alone
    begin_block
        random_choice $8B, $8B, $80
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_29:
_10A1F8:
    normal_react
    begin_block
        random_choice $80, $80, $81
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_2A:
_10A204:
    normal_react
    begin_block
        random_choice $80, $BF, $81
    end_block

    react_to $00, $07, $04
    begin_block
        .byte $9B
    end_block

; ------------------------------------------------------------------------------


AIScript_2B:
_10A215:
    normal_react
    begin_block
        .byte $80
    end_block

    if_dying
    if_alone
    begin_block
        show_monster $83, $0C
        .byte $EE
    end_block

; ------------------------------------------------------------------------------


AIScript_2C:
_10A22B:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_dmg
    begin_block
        change_target $23, $F0
        .byte $8D
    end_block

; ------------------------------------------------------------------------------


AIScript_2D:
_10A240:
    if_alone
    begin_block
        show_text $1401
        .byte $A2
    end_cond_block

    normal_react
    begin_block
        no_interrupt
            show_text $0B01
            change_target $0E, $F0
            .byte $AC
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_2E:
_10A25F:
    normal_react
    begin_block
        random_choice $1A, $80, $80
        random_choice $29, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_2F:
_10A26E:
    if_monsters_same
    begin_block
        .byte $A2
    end_cond_block

    normal_react
    begin_block
        .byte $80
    end_block

    react_to_magic $00, $AC
    begin_block
        .byte $81
    end_block

; ------------------------------------------------------------------------------


AIScript_30:
_10A283:
    normal_react
    begin_block
        random_choice $90, $80, $80
        random_choice $90, $80, $81
    end_block

    react_to_dmg
    begin_block
        change_target $23, $F0
        random_choice $8F, $8F, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_31:
_10A29F:
    normal_react
    begin_block
        random_choice $90, $80, $80
    end_block

    if_dying
    begin_block
        show_monster $87, $01
        .byte $A4
    end_block

; ------------------------------------------------------------------------------


AIScript_32:
_10A2B4:
    normal_react
    begin_block
        random_choice $85, $80, $80
    end_block

    if_dying
    begin_block
        show_monster $87, $01
        .byte $A4
    end_block

; ------------------------------------------------------------------------------


AIScript_33:
_10A2C9:
    normal_react
    begin_block
        random_choice $D5, $80, $80
    end_block

    if_dying
    begin_block
        show_monster $87, $01
        .byte $A4
    end_block

; ------------------------------------------------------------------------------


AIScript_34:
_10A2DE:
    normal_react
    begin_block
        .byte $80
        change_target $23, $F0
        .byte $8D
    end_block

    if_dying
    begin_block
        show_monster $87, $01
        .byte $A4
    end_block

; ------------------------------------------------------------------------------


AIScript_35:
_10A2F5:
    normal_react
    begin_block
        random_choice $DB, $2A, $2A
        random_choice $DB, $80, $81
    end_block

    if_dying
    begin_block
        show_text $0104
        show_text $0204
        show_text $0304
        .byte $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_36:
_10A316:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $81, $9B
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_37:
_10A322:
    normal_react
    begin_block
        .byte $80, $E7, $81, $E7
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_38:
_10A32D:
    normal_react
    begin_block
        .byte $80
    end_block

    if_alone
    react_to_dmg
    begin_block
        change_target $23, $F0
        .byte $2F
    end_block

; ------------------------------------------------------------------------------


AIScript_39:
_10A343:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_3A:
_10A34E:
    normal_react
    begin_block
        random_choice $94, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_3B:
_10A359:
    normal_react
    begin_block
        random_choice $80, $80, $81
        .byte $80
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_3C:
_10A36F:
    normal_react
    begin_block
        random_choice $CB, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_3D:
_10A37A:
    normal_react
    begin_block
        random_choice $9F, $C7, $B2
        random_choice $80, $81, $92
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_3E:
_10A389:
    normal_react
    begin_block
        random_choice $84, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_3F:
_10A394:
    normal_react
    begin_block
        .byte $80
        random_choice $B5, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_40:
_10A3A0:
    normal_react
    begin_block
        random_choice $A2, $80, $80
        random_choice $C9, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_41:
_10A3AF:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_42:
_10A3BE:
    normal_react
    begin_block
        change_target $23, $F0
        random_choice $2B, $2A, $AA
        change_target $23, $F0
        random_choice $2A, $2C, $AA
        change_target $23, $F0
        random_choice $2C, $2B, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_43:
_10A3DD:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $81, $AA
        .byte $80
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_44:
_10A3EE:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $81
        random_choice $80, $81, $AA
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_45:
_10A408:
    normal_react
    begin_block
        change_target $23, $F0
        random_choice $90, $AA, $AA
        random_choice $90, $80, $80
        change_target $0F, $F0
        .byte $9E
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_46:
_10A420:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        change_target $19, $F0
        .byte $BB
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_47:
_10A434:
    normal_react
    begin_block
        random_choice $80, $AA, $AA
        random_choice $89, $80, $AA
        .byte $80
        random_choice $89, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_48:
_10A448:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $DC, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_49:
_10A457:
    normal_react
    begin_block
        random_choice $B6, $80, $81
        random_choice $DC, $80, $80
    end_block

    if_dying
    begin_block
        .byte $86
    end_block

; ------------------------------------------------------------------------------


AIScript_4A:
_10A46C:
    normal_react
    begin_block
        random_choice $80, $C5, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_4B:
_10A477:
    normal_react
    begin_block
        random_choice $80, $80, $20
        random_choice $8E, $8E, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_4C:
_10A486:
    normal_react
    begin_block
        random_choice $80, $80, $94
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_4D:
_10A491:
    normal_react
    begin_block
        random_choice $80, $D4, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_4E:
_10A49C:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_4F:
_10A4A7:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $D3, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_50:
_10A4B3:
    if_alone
    begin_block
        random_choice $80, $80, $81
    end_cond_block

    if_hp_less $0D, $012C
    begin_block
        change_target $10, $F0
        .byte $96
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_51:
_10A4D3:
    check_status $1B, $00, $80
    begin_block
        change_target $1B, $F0
        .byte $BA
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_magic $00, $12
    begin_block
        change_target $23, $F0
        .byte $1E
    end_block

; ------------------------------------------------------------------------------


AIScript_52:
_10A4F3:
    normal_react
    begin_block
        random_choice $80, $D5, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_53:
_10A4FE:
    normal_react
    begin_block
        random_choice $80, $C5, $81
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_54:
_10A50A:
    normal_react
    begin_block
        .byte $9C
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_55:
_10A512:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to $00, $04, $00
    begin_block
        .byte $81
    end_block

; ------------------------------------------------------------------------------


AIScript_56:
_10A523:
    normal_react
    begin_block
        random_choice $C2, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_57:
_10A52E:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $80, $AA
        random_choice $80, $80, $B9
    end_block

    if_hp_less $0D, $2710
    react_to_dmg
    begin_block
        .byte $A2
    end_cond_block

    react_to $00, $04, $00
    begin_block
        .byte $81
    end_block

; ------------------------------------------------------------------------------


AIScript_58:
_10A552:
    normal_react
    begin_block
        random_choice $98, $98, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_59:
_10A55D:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_5A:
_10A568:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_5B:
_10A573:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $DA, $81
        random_choice $80, $80, $81
        random_choice $80, $81, $CD
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_5C:
_10A587:
    normal_react
    begin_block
        random_choice $80, $81, $A2
    end_block

    if_alone
    react_to $00, $2B, $00
    begin_block
        show_monster $07, $FC
        .byte $EE
    end_block

; ------------------------------------------------------------------------------


AIScript_5D:
_10A5A0:
    normal_react
    begin_block
        random_choice $80, $D5, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_5E:
_10A5AB:
    normal_react
    begin_block
        random_choice $80, $BC, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_5F:
_10A5B6:
    if_hp_less $0D, $012C
    begin_block
        .byte $A2
    end_cond_block

    normal_react
    begin_block
        .byte $80
        random_choice $80, $89, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_60:
_10A5C9:
    check_status $1B, $00, $80
    begin_block
        change_target $1B, $F0
        .byte $B7
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_61:
_10A5DF:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $D6, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_62:
_10A5EE:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $99, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_63:
_10A5FD:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_64:
_10A608:
    normal_react
    begin_block
        random_choice $80, $C3, $81
        random_choice $80, $80, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_65:
_10A617:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_66:
_10A622:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $80, $81
        random_choice $80, $B6, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_67:
_10A635:
    if_alone
    begin_block
        show_monster $82, $10
        .byte $EE
    end_cond_block

    normal_react
    begin_block
        show_text $0502
        .byte $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_68:
_10A64C:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to $01, $2B, $00
    begin_block
        random_choice $8B, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_69:
_10A660:
    if_alone
    begin_block
        .byte $80
    end_cond_block

    normal_react
    begin_block
        .byte $E6
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_6A:
_10A66F:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_6B:
_10A67A:
    normal_react
    begin_block
        random_choice $80, $8C, $81
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_6C:
_10A686:
    normal_react
    begin_block
        random_choice $80, $86, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_6D:
_10A691:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $3C, $81
        .byte $80
        random_choice $80, $42, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_6E:
_10A6A2:
    normal_react
    begin_block
        random_choice $80, $95, $AA
        random_choice $80, $EA, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_6F:
_10A6B1:
    normal_react
    begin_block
        random_choice $80, $80, $81
        change_target $0E, $F0
        .byte $18
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_70:
_10A6C1:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_71:
_10A6CC:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_72:
_10A6DB:
    normal_react
    begin_block
        random_choice $DE, $DE, $2D
    end_block

    if_dying
    begin_block
        set_event_flag $01, $40
        .byte $AA
    end_cond_block

    react_to_dmg
    begin_block
        change_target $2D, $00
        .byte $EB
    end_block

; ------------------------------------------------------------------------------


AIScript_73:
_10A6FB:
    normal_react
    begin_block
        random_choice $35, $AA, $AA
        random_choice $35, $8A, $AA
        .byte $8A
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_74:
_10A70B:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $BD, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_75:
_10A71A:
    normal_react
    begin_block
        random_choice $80, $80, $81
        change_target $10, $F0
        .byte $20
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_76:
_10A72A:
    normal_react
    begin_block
        random_choice $1A, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_77:
_10A735:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $DA, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_78:
_10A744:
    normal_react
    begin_block
        .byte $80, $E7, $81, $E7
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_79:
_10A74F:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_7A:
_10A75A:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    if_dying
    begin_block
        change_target $26, $F0
        .byte $21
    end_block

; ------------------------------------------------------------------------------


AIScript_7B:
_10A76F:
    normal_react
    begin_block
        change_target $1D, $F0
        random_choice $2A, $26, $AA
        change_target $1D, $F0
        random_choice $2B, $24, $AA
        change_target $1D, $F0
        random_choice $2C, $25, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_7C:
_10A78E:
    normal_react
    begin_block
        .byte $80
        random_choice $91, $80, $81
        .byte $80
        random_choice $95, $87, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_7D:
_10A79F:
    if_alone
    begin_block
        random_choice $34, $2E, $AA
        random_choice $2E, $47, $AA
    end_cond_block

    normal_react
    begin_block
        random_choice $1A, $2D, $AA
        random_choice $1A, $3D, $AA
        random_choice $2D, $40, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_7E:
_10A7C0:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_7F:
_10A7D5:
    if_alone
    begin_block
        .byte $B2, $80, $80
    end_cond_block

    normal_react
    begin_block
        .byte $80
        random_choice $80, $81, $80
        .byte $80
        random_choice $B2, $81, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_80:
_10A7EF:
    normal_react
    begin_block
        random_choice $80, $80, $AA
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $81, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_81:
_10A803:
    if_alone
    begin_block
        random_choice $80, $CC, $CC
        random_choice $80, $CC, $80
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $CD, $81
        random_choice $80, $CC, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_82:
_10A820:
    normal_react
    begin_block
        random_choice $80, $CA, $81
        random_choice $80, $CA, $80
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_83:
_10A833:
    normal_react
    begin_block
        random_choice $80, $DC, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_84:
_10A83E:
    if_alone
    begin_block
        random_choice $80, $80, $81
    end_cond_block

    normal_react
    begin_block
        random_choice $28, $28, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_85:
_10A853:
    normal_react
    begin_block
        random_choice $20, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_86:
_10A85E:
    normal_react
    begin_block
        .byte $80
    end_block

    react_to $00, $04, $00
    begin_block
        .byte $C1
    end_block

; ------------------------------------------------------------------------------


AIScript_87:
_10A86C:
    normal_react
    begin_block
        random_choice $80, $80, $AA
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $BC, $98, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_88:
_10A880:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_89:
_10A88F:
    normal_react
    begin_block
        random_choice $91, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_8A:
_10A89A:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_8B:
_10A8A2:
    normal_react
    begin_block
        .byte $AA
        random_choice $80, $80, $81
    end_block

    react_to_dmg
    begin_block
        random_choice $98, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_8C:
_10A8B7:
    normal_react
    begin_block
        .byte $AA
        no_interrupt
            show_text $6001
            show_monster $45, $70
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_8D:
_10A8CC:
    check_status $26, $00, $80
    begin_block
        no_interrupt
            show_monster $00, $E0
            .byte $EE, $A7
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_8E:
_10A8E4:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_8F:
_10A8F3:
    normal_react
    begin_block
        random_choice $80, $81, $AA
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_90:
_10A902:
    normal_react
    begin_block
        random_choice $DE, $DE, $81
        no_interrupt
            .byte $81, $81
    end_block
    end_interrupt

    if_dying
    begin_block
        show_text $4B03
        show_text $4C03
        set_event_flag $01, $08
        .byte $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_91:
_10A925:
    normal_react
    begin_block
        random_choice $80, $81, $AA
        change_target $00, $F0
        random_choice $BB, $BB, $AA
        random_choice $80, $9D, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_92:
_10A93C:
    normal_react
    begin_block
        random_choice $80, $81, $AA
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_93:
_10A948:
    normal_react
    begin_block
        random_choice $80, $95, $AA
        random_choice $94, $81, $AA
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_94:
_10A961:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $AA
        random_choice $B6, $80, $AA
        random_choice $80, $80, $AA
        random_choice $B9, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_95:
_10A979:
    if_alone
    begin_block
        .byte $AA
        no_interrupt
            show_text $2902
            show_monster $80, $00
            .byte $EE
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        change_target $10, $F0
        random_choice $18, $18, $1C
        change_target $10, $F0
        random_choice $18, $18, $16
        change_target $10, $F0
        random_choice $18, $18, $3A
        change_target $10, $F0
        random_choice $18, $18, $1B
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_96:
_10A9B4:
    normal_react
    begin_block
        random_choice $34, $80, $B0
        random_choice $47, $80, $B0
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_97:
_10A9C3:
    normal_react
    begin_block
        random_choice $D5, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_98:
_10A9CE:
    normal_react
    begin_block
        random_choice $80, $D4, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_99:
_10A9D9:
    normal_react
    begin_block
        change_target $23, $F0
        random_choice $2A, $8D, $8F
        random_choice $80, $81, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_9A:
_10A9EC:
    normal_react
    begin_block
        random_choice $85, $86, $87
        random_choice $88, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_9B:
_10A9FB:
    normal_react
    begin_block
        random_choice $1A, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_9C:
_10AA06:
    normal_react
    begin_block
        random_choice $80, $BE, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_9D:
_10AA11:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_9E:
_10AA1C:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_9F:
_10AA27:
    normal_react
    begin_block
        random_choice $17, $18, $AA
        random_choice $1A, $1C, $AA
        random_choice $13, $23, $AA
    end_block

    react_to_cmd $01, $2B, $00
    begin_block
        .byte $E2
    end_block

; ------------------------------------------------------------------------------


AIScript_A0:
_10AA40:
    normal_react
    begin_block
        random_choice $42, $37, $38
        random_choice $3B, $3C, $AA
        random_choice $3F, $40, $AA
        random_choice $44, $47, $43
    end_block

    react_to_cmd $01, $2B, $00
    begin_block
        .byte $45
    end_block

; ------------------------------------------------------------------------------


AIScript_A1:
_10AA5D:
    if_var_eq $00, $02
    begin_block
        random_choice $C3, $C3, $81
    end_cond_block

    .byte $05, $00, $F0, $40
    ;if_monster_visible $00, $40    ; TODO: figure out why this happens
    begin_block
        set_cond_var $00, $02
        .byte $E7
    end_cond_block

    .byte $05, $00, $F0, $60
    ;if_monster_visible $00, $60    ; TODO: figure out why this happens
    begin_block
        set_cond_var $00, $02
        .byte $E7
    end_cond_block

    if_var_eq $00, $01
    begin_block
        change_target $05, $F0
        random_choice $80, $80, $81
        change_target $05, $F0
        random_choice $80, $80, $81
        change_target $05, $F0
        random_choice $80, $C3, $81
    end_cond_block

    normal_react
    begin_block
        set_cond_var $00, $01
        .byte $E7
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_A2:
_10AAA7:
    normal_react
    begin_block
        random_choice $27, $29, $AA
        random_choice $2A, $2C, $AA
        random_choice $2B, $30, $AA
        random_choice $2F, $2E, $AA
        random_choice $31, $32, $AA
    end_block

    react_to_cmd $01, $2B, $00
    begin_block
        no_interrupt
            .byte $D7, $D7
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_A3:
_10AACD:
    normal_react
    begin_block
        .byte $80
    end_block

    react_to_cmd $01, $2B, $00
    begin_block
        .byte $81
    end_cond_block

    react_to_cmd $00, $2B, $00
    begin_block
        no_interrupt
            show_text $1502
            .byte $80, $80, $80, $80
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_A4:
_10AAED:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_A5:
_10AAF8:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_A6:
_10AB03:
    if_var_eq $01, $02
    begin_block
        random_choice $B6, $B6, $81
    end_cond_block

    .byte $05, $00, $F0, $20
    ;if_monster_visible $00, $20    ; TODO figure out why this happens
    begin_block
        set_cond_var $01, $02
        .byte $E7
    end_cond_block

    .byte $05, $00, $F0, $60
    ;if_monster_visible $00, $60    ; TODO figure out why this happens
    begin_block
        set_cond_var $01, $02
        .byte $E7
    end_cond_block

    if_var_eq $01, $01
    begin_block
        change_target $05, $F0
        random_choice $80, $80, $81
        change_target $05, $F0
        random_choice $80, $80, $81
        change_target $05, $F0
        random_choice $80, $98, $81
    end_cond_block

    normal_react
    begin_block
        set_cond_var $01, $01
        .byte $E7
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_A7:
_10AB4D:
    normal_react
    begin_block
        random_choice $D4, $D3, $D9
        random_choice $E6, $E2, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_A8:
_10AB5C:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_A9:
_10AB67:
    normal_react
    begin_block
        random_choice $80, $AA, $AA
        random_choice $80, $2E, $AA
        random_choice $80, $2E, $2E
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_AA:
_10AB84:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $81
    end_block

    react_to $00, $2B, $01
    begin_block
        .byte $A2
    end_block

; ------------------------------------------------------------------------------


AIScript_AB:
_10AB96:
    normal_react
    begin_block
        random_choice $80, $CB, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_AC:
_10ABA1:
    normal_react
    begin_block
        random_choice $80, $C9, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_AD:
_10ABAC:
    normal_react
    begin_block
        random_choice $D5, $80, $81
    end_block

    react_to $00, $04, $00
    begin_block
        no_interrupt
            .byte $E4, $A5
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_AE:
_10ABC2:
    normal_react
    begin_block
        random_choice $C5, $80, $81
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $D5, $BC, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_AF:
_10ABD6:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $E5
        random_choice $80, $80, $81
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $B5, $B5, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_B0:
_10ABEF:
    normal_react
    begin_block
        random_choice $82, $80, $81
    end_block

    react_to $00, $04, $00
    begin_block
        .byte $82
    end_block

; ------------------------------------------------------------------------------


AIScript_B1:
_10AC00:
    normal_react
    begin_block
        random_choice $80, $98, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_B2:
_10AC0B:
    normal_react
    begin_block
        random_choice $80, $B5, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_B3:
_10AC16:
    if_monster_visible $00, $08
    begin_block
        no_interrupt
            .byte $81, $81, $81
        end_interrupt
        show_monster $40, $F0
        .byte $EE
    end_cond_block

    if_monster_visible $00, $10
    begin_block
        show_monster $00, $08
        .byte $EE
    end_cond_block

    if_monster_visible $00, $20
    begin_block
        show_monster $00, $10
        .byte $EE
    end_cond_block

    if_monster_visible $00, $40
    begin_block
        show_monster $00, $20
        .byte $EE
    end_cond_block

    normal_react
    begin_block
        show_monster $00, $40
        .byte $EE
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_B4:
_10AC55:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_B5:
_10AC60:
    if_alone
    begin_block
        show_text $5C01
        .byte $AA, $D7
    end_cond_block

    normal_react
    begin_block
        .byte $80
    end_block

    react_to $00, $04, $00
    begin_block
        .byte $81
    end_block

; ------------------------------------------------------------------------------


AIScript_B6:
_10AC7A:
    if_alone
    begin_block
        .byte $34
    end_cond_block

    normal_react
    begin_block
        show_text $2C02
        .byte $AA
        show_text $2D02
        .byte $AA
        no_interrupt
            change_target $06, $F0
            .byte $A0, $AF
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_B7:
_10AC9C:
    check_status $0D, $00, $20
    begin_block
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $CA, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_B8:
_10ACAE:
    check_status $0D, $00, $20
    begin_block
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        random_choice $B6, $D1, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_B9:
_10ACC0:
    check_status $0D, $00, $20
    begin_block
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $C3, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_BA:
_10ACD2:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_cmd $00, $2B, $00
    begin_block
        .byte $9B
    end_block

; ------------------------------------------------------------------------------


AIScript_BB:
_10ACE3:
    normal_react
    begin_block
        random_choice $80, $80, $81
        change_target $1B, $F0
        .byte $B7
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_BC:
_10ACF7:
    normal_react
    begin_block
        random_choice $80, $91, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_BD:
_10AD02:
    normal_react
    begin_block
        .byte $83
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_BE:
_10AD0A:
    normal_react
    begin_block
        random_choice $80, $D6, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_BF:
_10AD15:
    if_alone
    begin_block
        random_choice $80, $C5, $AA
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_C0:
_10AD2A:
    normal_react
    begin_block
        random_choice $80, $C2, $81
        random_choice $C2, $80, $C2
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_C1:
_10AD39:
    normal_react
    begin_block
        random_choice $80, $B5, $81
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $B4, $89, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_C2:
_10AD4D:
    normal_react
    begin_block
        random_choice $80, $83, $81
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $E5, $BB, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_C3:
_10AD61:
    normal_react
    begin_block
        random_choice $80, $BE, $BE
        random_choice $80, $BE, $81
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $B7, $8A, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_C4:
_10AD79:
    normal_react
    begin_block
        random_choice $80, $AA, $AA
        random_choice $80, $80, $81
        random_choice $9E, $80, $AA
        random_choice $9E, $80, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_C5:
_10AD90:
    if_alone
    begin_block
        random_choice $80, $B5, $80
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $D4, $81
        .byte $80
    end_block

    react_to $00, $0D, $00
    begin_block
        random_choice $BD, $BD, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_C6:
_10ADAF:
    if_alone
    begin_block
        random_choice $80, $EB, $81
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_C7:
_10ADC4:
    normal_react
    begin_block
        random_choice $80, $80, $80
        random_choice $C6, $EA, $80
        random_choice $80, $C6, $80
        random_choice $C6, $EA, $80
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $81, $81, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_C8:
_10ADE4:
    normal_react
    begin_block
        random_choice $8E, $8E, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_C9:
_10ADEF:
    normal_react
    begin_block
        .byte $AA, $AA, $80, $AA
    end_block

    react_to_cmd $00, $2B, $00
    begin_block
        .byte $45
    end_cond_block

    react_to_cmd $01, $2B, $00
    begin_block
        random_choice $80, $80, $81
    end_block

; ------------------------------------------------------------------------------


AIScript_CA:
_10AE0A:
    normal_react
    begin_block
        .byte $AA
        random_choice $35, $89, $AA
        .byte $80
        random_choice $2D, $8A, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_CB:
_10AE1B:
    if_hp_less $0D, $0BB8
    begin_block
        random_choice $B8, $B8, $80
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $B8, $81
    end_block

    react_to_cmd $00, $2B, $00
    begin_block
        .byte $1C
    end_cond_block

    react_to $00, $04, $00
    begin_block
        .byte $16
    end_block

; ------------------------------------------------------------------------------


AIScript_CC:
_10AE3D:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $80, $80, $81
    end_block

    react_to $00, $04, $00
    begin_block
        random_choice $D9, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_CD:
_10AE55:
    normal_react
    begin_block
        random_choice $80, $DB, $81
        random_choice $80, $92, $81
    end_block

    react_to_cmd $01, $2B, $00
    begin_block
        random_choice $D6, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_CE:
_10AE6D:
    normal_react
    begin_block
        random_choice $80, $DA, $AA
        random_choice $80, $DA, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_CF:
_10AE7C:
    if_alone
    begin_block
        random_choice $CA, $CD, $CE
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_D0:
_10AE91:
    normal_react
    begin_block
        .byte $AD
        random_choice $85, $86, $87
        .byte $AD
        random_choice $86, $87, $88
        .byte $AD
        random_choice $87, $88, $85
        .byte $AD
        random_choice $88, $85, $87
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_D1:
_10AEAC:
    normal_react
    begin_block
        random_choice $80, $97, $AA
    end_block

    react_to_dmg
    begin_block
        random_choice $97, $97, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_D2:
_10AEC0:
    if_alone
    begin_block
        no_interrupt
            random_choice $30, $32, $31
            random_choice $30, $32, $31
        end_interrupt
        no_interrupt
            random_choice $15, $29, $17
            random_choice $3F, $40, $2F
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        no_interrupt
            random_choice $34, $20, $AA
            random_choice $3D, $2D, $AA
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_D3:
_10AEF1:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $E5, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_D4:
_10AF00:
    if_var_eq $00, $01
    begin_block
        random_choice $EF, $AA, $AA
        no_interrupt
            random_choice $E2, $E2, $80
            set_cond_var $00, $00
            show_monster $85, $E0
            .byte $EE
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        .byte $AA
        no_interrupt
            random_choice $A7, $A7, $80
            set_cond_var $00, $01
            show_monster $05, $1C
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_D5:
_10AF34:
    normal_react
    begin_block
        random_choice $DA, $DA, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_D6:
_10AF3F:
    normal_react
    begin_block
        random_choice $80, $80, $AA
    end_block

    react_to_dmg
    begin_block
        random_choice $EB, $D3, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_D7:
_10AF53:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_D8:
_10AF5E:
    if_hp_less $0D, $7530
    begin_block
        show_text $8504
        show_text $8603
        show_text $8707
        show_text $8804
        show_text $8905
        show_text $8A04
        show_text $8B06
        show_text $8C07
        show_text $8D05
        change_target $0D, $F0
        .byte $AC
    end_cond_block

    if_var_eq $00, $01
    begin_block
        no_interrupt
            show_text $7301
            random_choice $80, $80, $AA
            random_choice $80, $80, $AA
        end_interrupt
        no_interrupt
            show_text $7801
            random_choice $80, $80, $AA
            random_choice $80, $80, $AA
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        show_text $8402
        set_cond_var $00, $01
        .byte $80
    end_block

    react_to_magic $00, $AC
    begin_block
        show_text $DB06
        show_text $DC06
        show_text $DD06
        show_text $DE04
        show_text $DF04
        show_text $E006
        show_text $E106
        show_text $E201
        .byte $EF
    end_block

; ------------------------------------------------------------------------------


AIScript_D9:
_10AFE9:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_DA:
_10AFF4:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_DB:
_10AFFF:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_DC:
_10B00A:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $DA, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_DD:
_10B019:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_DE:
_10B024:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_DF:
_10B02F:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E0:
_10B03A:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E1:
_10B045:
    normal_react
    begin_block
        random_choice $DA, $80, $DA
        random_choice $DA, $80, $B9
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E2:
_10B054:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E3:
_10B05F:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E4:
_10B06A:
    normal_react
    begin_block
        no_interrupt
            .byte $80, $A2
    end_block
    end_interrupt

    if_dying
    begin_block
        .byte $A2
    end_block

; ------------------------------------------------------------------------------


AIScript_E5:
_10B07D:
    normal_react
    begin_block
        show_text $0603
        show_text $0703
        set_event_flag $01, $30
        .byte $A5
    end_cond_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E6:
_10B091:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_E7:
_10B09C:
    normal_react
    begin_block
        no_interrupt
            change_target $23, $F0
            .byte $2B
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_E8:
_10B0B5:
    normal_react
    begin_block
        no_interrupt
            .byte $DB
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_E9:
_10B0CA:
    normal_react
    begin_block
        no_interrupt
            .byte $DC
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_EA:
_10B0DF:
    normal_react
    begin_block
        no_interrupt
            .byte $81
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_EB:
_10B0F4:
    normal_react
    begin_block
        no_interrupt
            .byte $DA
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_EC:
_10B109:
    normal_react
    begin_block
        no_interrupt
            .byte $89
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_ED:
_10B11E:
    normal_react
    begin_block
        no_interrupt
            .byte $8B
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_EE:
_10B133:
    normal_react
    begin_block
        no_interrupt
            .byte $91
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_EF:
_10B148:
    normal_react
    begin_block
        no_interrupt
            .byte $B9
            show_text $6101
            show_monster $05, $80
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_F0:
_10B15D:
    normal_react
    begin_block
        random_choice $80, $84, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F1:
_10B168:
    normal_react
    begin_block
        random_choice $80, $81, $AA
    end_block

    react_to_item $00, $EC
    begin_block
        change_target $0D, $F0
        .byte $AF
    end_block

; ------------------------------------------------------------------------------


AIScript_F2:
_10B17D:
    normal_react
    begin_block
        random_choice $80, $95, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F3:
_10B188:
    normal_react
    begin_block
        random_choice $80, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F4:
_10B193:
    normal_react
    begin_block
        random_choice $2F, $80, $81
    end_block

    if_dying
    begin_block
        change_target $23, $F0
        .byte $2F
    end_block

; ------------------------------------------------------------------------------


AIScript_F5:
_10B1A8:
    normal_react
    begin_block
        random_choice $D3, $88, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F6:
_10B1B3:
    normal_react
    begin_block
        .byte $80
        random_choice $80, $80, $81
        .byte $80
        random_choice $80, $80, $B3
        random_choice $DA, $80, $DA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F7:
_10B1C8:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F8:
_10B1D3:
    normal_react
    begin_block
        random_choice $1A, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_F9:
_10B1DE:
    normal_react
    begin_block
        .byte $AA
        change_target $23, $F0
        .byte $1A
    end_block

    react_to_dmg
    begin_block
        .byte $1A
    end_block

; ------------------------------------------------------------------------------


AIScript_FA:
_10B1F1:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_dmg
    begin_block
        change_target $23, $F0
        .byte $8D
    end_block

; ------------------------------------------------------------------------------


AIScript_FB:
_10B206:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $80, $93
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_FC:
_10B215:
    normal_react
    begin_block
        .byte $80
    end_block

    if_dying
    begin_block
        .byte $D6
    end_cond_block

    react_to_dmg
    begin_block
        no_interrupt
            .byte $81, $81
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_FD:
_10B22F:
    normal_react
    begin_block
        random_choice $CA, $A7, $B2
        .byte $DD
        random_choice $BD, $92, $CA
        .byte $DD
        no_interrupt
            random_choice $A7, $B2, $DD
            random_choice $C2, $D6, $BD
        end_interrupt
        .byte $AD
        random_choice $BD, $92, $CA
        .byte $DD
    end_block

    react_to_dmg
    begin_block
        no_interrupt
            random_choice $C6, $C6, $C7
            random_choice $C6, $C6, $E2
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_FE:
_10B263:
    normal_react
    begin_block
        .byte $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_FF:
_10B26B:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_100:
_10B273:
    normal_react
    begin_block
        random_choice $DA, $80, $DA
        random_choice $DA, $80, $DA
        show_monster $05, $40
        .byte $EE
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_101:
_10B287:
    normal_react
    begin_block
        .byte $AA, $AA
        no_interrupt
            show_monster $05, $80
            .byte $EE, $DA
    end_block
    end_interrupt

    react_to_cmd $00, $04, $00
    if_var_eq $00, $01
    begin_block
        .byte $81
    end_cond_block

    react_to_cmd $00, $04, $00
    begin_block
        show_text $D005
        show_text $D105
        set_cond_var $00, $01
        .byte $81
    end_block

; ------------------------------------------------------------------------------


AIScript_102:
_10B2B7:
    normal_react
    begin_block
        random_choice $80, $81, $81
        random_choice $80, $C4, $81
        random_choice $80, $C4, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_103:
_10B2CA:
    normal_react
    begin_block
        change_music $F003
        show_text $5703
        .byte $AA, $E1
        show_monster $02, $80
        .byte $EE
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_104:
_10B2E0:
    normal_react
    begin_block
        random_choice $15, $37, $3A
        random_choice $12, $25, $13
        random_choice $16, $28, $26
        no_interrupt
            show_text $0402
            show_monster $05, $40
            .byte $EE
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_105:
_10B300:
    normal_react
    begin_block
        random_choice $80, $81, $80
        random_choice $80, $81, $80
        random_choice $80, $81, $81
        show_monster $05, $80
        .byte $EE
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_106:
_10B318:
    normal_react
    begin_block
        random_choice $80, $81, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_107:
_10B323:
    if_var_eq $00, $00
    if_hp_less $0D, $012C
    begin_block
        no_interrupt
            show_text $0A01
            show_monster $80, $C0
            .byte $EE
        end_interrupt
        no_interrupt
            change_target $10, $F0
            .byte $38
            set_cond_var $00, $01
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $24, $25, $26
        random_choice $8F, $81, $2D
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_108:
_10B356:
    check_status $0D, $00, $20
    begin_block
        change_target $0D, $F0
        .byte $29
    end_cond_block

    normal_react
    begin_block
        .byte $80
    end_block

    if_hp_less $0D, $0320
    react_to_dmg
    begin_block
        no_interrupt
            .byte $80
            random_choice $81, $80, $AA
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_109:
_10B37B:
    normal_react
    begin_block
        random_choice $80, $81, $DB
    end_block

    react_to_dmg
    begin_block
        no_interrupt
            .byte $DB
            show_monster $45, $60
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_10A:
_10B395:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to_dmg
    begin_block
        no_interrupt
            .byte $2A
            show_monster $45, $A0
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_10B:
_10B3AF:
    normal_react
    begin_block
        change_target $0D, $F0
        .byte $2A
    end_block

    react_to_dmg
    begin_block
        no_interrupt
            .byte $E6
            show_monster $45, $C0
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_10C:
_10B3CA:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_10D:
_10B3D5:
    normal_react
    begin_block
        random_choice $DE, $C9, $C9
    end_block

    if_hp_less $0B, $0001
    begin_block
        change_target $0D, $F0
        .byte $ED
    end_cond_block

    react_to_magic $00, $AC
    begin_block
        .byte $DE
    end_block

; ------------------------------------------------------------------------------


AIScript_10E:
_10B3F1:
    normal_react
    begin_block
        .byte $AA
    end_block

    react_to $01, $2B, $F0
    begin_block
        .byte $3C
    end_block

; ------------------------------------------------------------------------------


AIScript_10F:
_10B3FF:
    normal_react
    begin_block
        show_monster $08, $01
        .byte $A3
        show_monster $08, $01
        .byte $A3
        show_monster $08, $01
        .byte $A3
        change_target $0E, $F0
        .byte $AC
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_110:
_10B41A:
    normal_react
    begin_block
        no_interrupt
            .byte $80, $80
        end_interrupt
        random_choice $80, $80, $81
        no_interrupt
            .byte $81, $81
        end_interrupt
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_111:
_10B435:
    normal_react
    begin_block
        .byte $92
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_112:
_10B43D:
    normal_react
    begin_block
        random_choice $9F, $C6, $AA
        random_choice $9F, $9F, $C6
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_113:
_10B44C:
    if_hp_less $0D, $2710
    begin_block
        random_choice $45, $DE, $AA
    end_cond_block

    if_hp_less $0D, $7530
    begin_block
        random_choice $33, $22, $DE
        random_choice $DE, $DE, $C0
        random_choice $DE, $C0, $AA
        random_choice $DE, $82, $C0
    end_cond_block

    normal_react
    begin_block
        random_choice $DE, $C0, $AA
        random_choice $DE, $C0, $DE
    end_block

    if_dying
    begin_block
        no_interrupt
            change_music $3E00
            show_text $E307
            show_text $E407
            show_text $E507
            show_text $E607
            show_text $E707
            show_monster $8D, $DE
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_114:
_10B4A1:
    if_var_eq $00, $01
    begin_block
        show_text $1602
        .byte $AA
        show_text $1902
        .byte $AA
        show_text $1B02
        .byte $AA
        show_text $1C01
        .byte $DD
    end_cond_block

    normal_react
    begin_block
        show_text $1602
        .byte $AA
        show_text $1702
        .byte $AA
        show_text $1802
        .byte $AA
        show_text $1902
        .byte $AA
        show_text $1A02
        .byte $AA
        show_text $1B02
        .byte $AA
        show_text $1C01
        .byte $DD
        set_cond_var $00, $01
        .byte $AA
    end_block

    react_to_dmg
    if_hp_less $0D, $2710
    begin_block
        change_target $0F, $F0
        .byte $B1
    end_block

; ------------------------------------------------------------------------------


AIScript_115:
_10B4F8:
    normal_react
    begin_block
        random_choice $DE, $DA, $81
    end_block

    if_dying
    begin_block
        show_monster $82, $40
        .byte $EE
    end_block

; ------------------------------------------------------------------------------


AIScript_116:
_10B50D:
    normal_react
    begin_block
        random_choice $DE, $CD, $81
    end_block

    if_dying
    begin_block
        show_monster $82, $20
        .byte $EE
    end_block

; ------------------------------------------------------------------------------


AIScript_117:
_10B522:
    normal_react
    begin_block
        random_choice $DE, $DB, $81
    end_block

    if_dying
    begin_block
        show_monster $82, $10
        .byte $EE
    end_block

; ------------------------------------------------------------------------------


AIScript_118:
_10B537:
    normal_react
    begin_block
        random_choice $DE, $DC, $81
    end_block

    if_dying
    begin_block
        no_interrupt
            show_monster $80, $08
            .byte $EE
            show_text $D904
            show_text $D604
            show_text $D704
            show_text $D804
            .byte $AA
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_119:
_10B561:
    normal_react
    begin_block
        random_choice $DA, $C2, $81
        random_choice $BC, $80, $81
        random_choice $DB, $C2, $81
        random_choice $BC, $80, $81
        random_choice $DC, $C2, $81
        random_choice $BC, $80, $81
        random_choice $CD, $C2, $81
        random_choice $BC, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_11A:
_10B588:
    normal_react
    begin_block
        random_choice $84, $80, $81
        random_choice $84, $CD, $CD
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_11B:
_10B597:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $CF, $81
    end_block

    if_dying
    begin_block
        .byte $CF
    end_block

; ------------------------------------------------------------------------------


AIScript_11C:
_10B5AC:
    normal_react
    begin_block
        .byte $AA, $AA
        random_choice $80, $81, $9B
    end_block

    if_dying
    begin_block
        change_target $25, $F0
        .byte $21
    end_cond_block

    react_to_summon
    begin_block
        change_target $0D, $F0
        .byte $18
    end_block

; ------------------------------------------------------------------------------


AIScript_11D:
_10B5CE:
    normal_react
    begin_block
        random_choice $80, $EA, $80
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_11E:
_10B5DD:
    normal_react
    begin_block
        show_text $7301
        random_choice $80, $80, $81
        show_text $7801
        random_choice $80, $80, $81
    end_block

    if_hp_less $0D, $2710
    react_to_dmg
    begin_block
        show_text $7402
        show_text $7502
        show_text $7602
        show_text $7702
        .byte $A2
    end_block

; ------------------------------------------------------------------------------


AIScript_11F:
_10B60E:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_120:
_10B619:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_121:
_10B624:
    normal_react
    begin_block
        random_choice $80, $C7, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_122:
_10B62F:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_123:
_10B63A:
    if_var_eq $00, $01
    begin_block
        random_choice $DE, $E8, $CE
        random_choice $DE, $DE, $E8
    end_cond_block

    normal_react
    begin_block
        .byte $80
        random_choice $80, $93, $81
        random_choice $80, $90, $80
        random_choice $80, $AB, $81
    end_block

    if_dying
    if_var_eq $00, $01
    begin_block
        show_text $2202
        show_text $2302
        show_text $7902
        .byte $A2
    end_cond_block

    if_hp_less $0D, $09C4
    react_to_dmg
    if_var_eq $00, $00
    begin_block
        no_interrupt
            show_text $1E02
            .byte $3A
            show_text $1F02
            .byte $16
            show_text $2002
            .byte $1C
            show_text $2102
            .byte $E8
            set_cond_var $00, $01
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_124:
_10B69C:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    react_to $00, $2B, $F0
    begin_block
        .byte $B6
    end_cond_block

    react_to $01, $2B, $F0
    begin_block
        random_choice $80, $81, $9C
    end_block

; ------------------------------------------------------------------------------


AIScript_125:
_10B6B7:
    normal_react
    begin_block
        change_target $12, $F0
        .byte $2B
        change_target $13, $F0
        .byte $2B
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_126:
_10B6C8:
    normal_react
    begin_block
        random_choice $80, $EA, $81
        random_choice $80, $98, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_127:
_10B6D7:
    if_var_eq $00, $01
    if_alone
    begin_block
        show_monster $80, $FC
        .byte $EE
    end_cond_block

    if_var_eq $00, $01
    begin_block
        .byte $AA, $AA, $AA, $AA, $AA
        show_monster $80, $FC
        .byte $EE
    end_cond_block

    normal_react
    begin_block
        show_monster $80, $CC
        .byte $EE
        show_monster $80, $D8
        .byte $EE
        show_monster $80, $B4
        .byte $EE
        show_monster $80, $FC
        .byte $EE
        set_cond_var $00, $01
        .byte $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_128:
_10B716:
    normal_react
    begin_block
        random_choice $81, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_129:
_10B721:
    normal_react
    begin_block
        random_choice $81, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_12A:
_10B72C:
    normal_react
    begin_block
        random_choice $81, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_12B:
_10B737:
    normal_react
    begin_block
        random_choice $81, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_12C:
_10B742:
    normal_react
    begin_block
        random_choice $81, $81, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_12D:
_10B74D:
    if_var_eq $00, $01
    begin_block
        random_choice $80, $9F, $80
        random_choice $80, $80, $81
        random_choice $80, $8E, $80
    end_cond_block

    if_hp_less $0D, $1770
    begin_block
        no_interrupt
            show_monster $80, $C0
            .byte $EE
            show_text $2402
            show_text $2502
            show_text $2602
            show_text $2702
            change_target $27, $F0
            .byte $AC
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $9F, $80
        random_choice $80, $80, $81
        random_choice $80, $8E, $80
    end_block

    if_dying
    if_alone
    begin_block
        no_interrupt
            show_text $D203
            show_text $D304
            .byte $E7
            show_text $D405
            show_text $D503
            .byte $AA
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_12E:
_10B7B9:
    normal_react
    begin_block
        random_choice $80, $90, $81
        random_choice $98, $9F, $AB
        random_choice $80, $80, $81
        random_choice $98, $D4, $AB
        random_choice $80, $D3, $9E
    end_block

    react_to_magic $00, $AC
    begin_block
        set_cond_var $00, $01
        .byte $9E
    end_block

; ------------------------------------------------------------------------------


AIScript_12F:
_10B7DE:
    check_status $2F, $00, $80
    check_event_flag $03, $80
    begin_block
        change_target $2E, $00
        .byte $E3
    end_cond_block

    check_status $30, $00, $80
    check_event_flag $03, $40
    begin_block
        change_target $2E, $00
        .byte $E3
    end_cond_block

    check_status $31, $00, $80
    check_event_flag $03, $20
    begin_block
        change_target $2E, $00
        .byte $E3
    end_cond_block

    check_status $32, $00, $80
    check_event_flag $03, $10
    begin_block
        change_target $2E, $00
        .byte $E3
    end_cond_block

    check_status $1B, $00, $80
    begin_block
        random_choice $EC, $EC, $EC
        .byte $EC, $EC
        random_choice $EC, $EC, $40
        .byte $EC
        random_choice $EC, $EC, $3C
        .byte $EC
        random_choice $EC, $EC, $42
        .byte $EC
        random_choice $EC, $EC, $44
        .byte $EC
        random_choice $EC, $EC, $EC
        .byte $EC
        random_choice $EC, $EC, $3C
        .byte $EC
        random_choice $EC, $EC, $42
        .byte $EC
    end_cond_block

    normal_react
    begin_block
        no_interrupt
            random_choice $3F, $3F, $AA
            random_choice $3F, $3F, $AA
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_130:
_10B85C:
    if_hp_less $0D, $0BB8
    begin_block
        change_target $23, $F0
        .byte $30
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_131:
_10B872:
    if_hp_less $0D, $0BB8
    begin_block
        .byte $CF
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_132:
_10B884:
    if_hp_less $0D, $0BB8
    begin_block
        .byte $84
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_133:
_10B896:
    if_hp_less $0D, $0BB8
    begin_block
        change_target $23, $F0
        .byte $91
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_134:
_10B8AC:
    if_hp_less $0D, $1388
    begin_block
        change_target $0D, $F0
        random_choice $2E, $47, $34
        change_target $0D, $F0
        random_choice $2E, $47, $34
        change_target $0D, $F0
        random_choice $2E, $47, $34
        show_monster $02, $40
        .byte $EE
    end_cond_block

    if_hp_less $0D, $2710
    begin_block
        change_target $0D, $F0
        random_choice $2F, $3D, $1A
        change_target $0D, $F0
        random_choice $2F, $3D, $1A
        change_target $0D, $F0
        random_choice $2F, $3D, $1A
        show_monster $02, $40
        .byte $EE
    end_cond_block

    normal_react
    begin_block
        change_target $0D, $F0
        random_choice $2B, $2A, $2C
        change_target $0D, $F0
        random_choice $2B, $2A, $2C
        change_target $0D, $F0
        random_choice $2B, $2A, $2C
        show_monster $02, $40
        .byte $EE
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_135:
_10B916:
    normal_react
    begin_block
        random_choice $30, $80, $BB
        random_choice $32, $80, $81
        random_choice $31, $BB, $81
    end_block

    if_dying
    begin_block
        show_text $5D03
        .byte $A2
    end_block

; ------------------------------------------------------------------------------


AIScript_136:
_10B933:
    if_var_eq $00, $02
    begin_block
        show_text $B205
        show_text $B305
        show_text $B405
        show_text $B505
        show_text $B605
        .byte $E9
    end_cond_block

    normal_react
    begin_block
        no_interrupt
            show_text $B705
            show_text $B805
            .byte $80
        end_interrupt
        no_interrupt
            .byte $80
            show_text $B902
            .byte $AA
        end_interrupt
        no_interrupt
            .byte $80
            show_text $BA02
            .byte $AA
        end_interrupt
        no_interrupt
            .byte $80
            show_text $BB02
            .byte $AA
        end_interrupt
        change_music $2D00
        show_text $B005
        show_text $B105
        set_cond_var $00, $02
        .byte $AA
    end_block

    if_dying
    react_to_magic $00, $E9
    begin_block
        show_text $5803
        set_event_flag $01, $04
        .byte $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_137:
_10B9A3:
    if_hp_less $0D, $3E80
    begin_block
        no_interrupt
            random_choice $80, $81, $AA
            random_choice $30, $32, $80
        end_interrupt
        no_interrupt
            random_choice $30, $80, $81
            random_choice $31, $80, $AA
        end_interrupt
        no_interrupt
            random_choice $31, $80, $AA
            random_choice $32, $80, $81
    end_cond_block
    end_interrupt

    if_hp_less $0D, $1B58
    begin_block
        random_choice $45, $45, $80
        random_choice $30, $31, $32
        random_choice $80, $80, $81
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $82, $82
        random_choice $E5, $E5, $81
        random_choice $AE, $3C, $80
        random_choice $80, $80, $81
        random_choice $CF, $CF, $80
        random_choice $DB, $80, $81
        random_choice $EA, $EA, $80
        random_choice $B9, $B9, $81
        random_choice $80, $2F, $80
        random_choice $88, $88, $81
    end_block

    react_to $00, $04, $00
    begin_block
        change_target $24, $F0
        random_choice $23, $AA, $AA
    end_cond_block

    react_to_magic $00, $37
    begin_block
        .byte $3A
    end_cond_block

    react_to_magic $00, $40
    begin_block
        .byte $3A
    end_block

; ------------------------------------------------------------------------------


AIScript_138:
_10BA29:
    normal_react
    begin_block
        random_choice $80, $D3, $81
        random_choice $80, $D3, $81
        random_choice $80, $D3, $81
        random_choice $80, $D3, $81
        .byte $C5
    end_block

    if_dying
    begin_block
        .byte $A2
    end_block

; ------------------------------------------------------------------------------


AIScript_139:
_10BA47:
    if_alone
    begin_block
        random_choice $80, $B7, $81
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_13A:
_10BA5C:
    normal_react
    begin_block
        random_choice $80, $BC, $81
        random_choice $35, $BC, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_13B:
_10BA6B:
    check_char_param $28, $1E, $10
    begin_block
        change_target $28, $F0
        .byte $A8
    end_cond_block

    check_char_param $29, $1E, $10
    begin_block
        change_target $29, $F0
        .byte $A8
    end_cond_block

    check_char_param $2A, $1E, $10
    begin_block
        change_target $2A, $F0
        .byte $A8
    end_cond_block

    check_char_param $2B, $1E, $10
    begin_block
        change_target $2B, $F0
        .byte $A8
    end_cond_block

    check_char_param $2C, $1E, $10
    begin_block
        change_target $2C, $F0
        .byte $A8
    end_cond_block

    normal_react
    begin_block
        random_choice $9F, $92, $AA
        random_choice $C7, $DD, $AA
        random_choice $9F, $92, $AA
        random_choice $C7, $E5, $AA
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_13C:
_10BABA:
    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $32, $80, $AA
        random_choice $31, $80, $AA
        random_choice $80, $AC, $AA
        random_choice $32, $AC, $AA
        random_choice $31, $AC, $AA
        random_choice $80, $AC, $AC
        random_choice $32, $AC, $AC
        random_choice $31, $AC, $AC
    end_block

    react_to_magic $00, $AC
    begin_block
        no_interrupt
            change_target $23, $F0
            .byte $31, $A9
            show_monster $49, $F0
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_13D:
_10BAF9:
    normal_react
    begin_block
        random_choice $80, $AA, $AA
        random_choice $32, $AA, $AA
        random_choice $30, $AA, $AA
        random_choice $80, $AC, $AA
        random_choice $32, $AC, $AA
        random_choice $30, $AC, $AA
        random_choice $80, $AC, $AC
        random_choice $32, $AC, $AC
        random_choice $30, $AC, $AC
    end_block

    react_to_magic $00, $AC
    begin_block
        no_interrupt
            change_target $23, $F0
            .byte $32, $A9
            show_monster $4A, $F0
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_13E:
_10BB38:
    normal_react
    begin_block
        random_choice $80, $AA, $AA
        random_choice $31, $AA, $AA
        random_choice $30, $AA, $AA
        random_choice $80, $AC, $AA
        random_choice $31, $AC, $AA
        random_choice $30, $AC, $AA
        random_choice $80, $AC, $AC
        random_choice $31, $AC, $AC
        random_choice $30, $AC, $AC
    end_block

    react_to_magic $00, $AC
    begin_block
        no_interrupt
            change_target $23, $F0
            .byte $30, $A9
            show_monster $4B, $F0
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_13F:
_10BB77:
    normal_react
    begin_block
        random_choice $30, $AA, $AA
        random_choice $31, $AA, $AA
        random_choice $32, $AA, $AA
        random_choice $30, $AC, $AA
        random_choice $31, $AC, $AA
        random_choice $32, $AC, $AA
        random_choice $30, $AC, $AC
        random_choice $31, $AC, $AC
        random_choice $32, $AC, $AC
    end_block

    react_to_magic $00, $AC
    begin_block
        no_interrupt
            .byte $BB, $A9
            show_monster $4C, $F0
            .byte $EE
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_140:
_10BBB2:
    normal_react
    begin_block
        .byte $AA, $D0
        random_choice $80, $80, $81
    end_block

    if_dying
    begin_block
        show_text $4901
        .byte $EF
    end_block

; ------------------------------------------------------------------------------


AIScript_141:
_10BBC9:
    if_alone
    begin_block
        show_monster $80, $C0
        .byte $EE
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $80, $81
        random_choice $80, $80, $81
        random_choice $80, $80, $81
        random_choice $96, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_142:
_10BBEF:
    check_status $26, $00, $80
    begin_block
        no_interrupt
            show_monster $00, $E0
            .byte $EE, $A7
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $80, $80, $AA
        change_target $23, $F0
        random_choice $30, $30, $AA
        random_choice $80, $80, $92
        random_choice $80, $30, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_143:
_10BC1A:
    check_status $26, $00, $80
    begin_block
        no_interrupt
            show_monster $00, $E0
            .byte $EE, $A7
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $80, $80, $AA
        random_choice $CC, $CC, $AA
        random_choice $80, $80, $CD
        random_choice $80, $31, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_144:
_10BC41:
    check_status $26, $00, $80
    begin_block
        no_interrupt
            show_monster $00, $E0
            .byte $EE, $A7
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $80, $80, $AA
        change_target $23, $F0
        random_choice $2F, $2F, $AA
        random_choice $80, $80, $BD
        random_choice $80, $2F, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_145:
_10BC6C:
    if_hp_less $0D, $0FA0
    begin_block
        random_choice $32, $30, $31
    end_cond_block

    normal_react
    begin_block
        random_choice $12, $24, $37
        random_choice $13, $25, $38
        random_choice $15, $27, $3A
        random_choice $17, $3B, $AA
        random_choice $18, $29, $3C
        random_choice $1A, $2A, $3D
        random_choice $2C, $2B, $42
        random_choice $1F, $2D, $2F
    end_block

    if_dying
    begin_block
        .byte $33
    end_cond_block

    react_to $01, $2B, $00
    react_to $01, $0A, $00
    begin_block
        .byte $41
    end_block

; ------------------------------------------------------------------------------


AIScript_146:
_10BCAE:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    if_dying
    begin_block
        show_text $4D03
        show_text $4E03
        .byte $22
    end_cond_block

    react_to_item $00, $0B
    begin_block
        .byte $A5
    end_cond_block

    react_to $01, $2B, $00
    begin_block
        random_choice $81, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_147:
_10BCD8:
    normal_react
    begin_block
        random_choice $80, $80, $81
        random_choice $80, $80, $81
        no_interrupt
            random_choice $80, $81, $D1
            random_choice $80, $D1, $D1
        end_interrupt
        random_choice $80, $80, $81
        random_choice $80, $D1, $81
        random_choice $80, $84, $84
        no_interrupt
            random_choice $D1, $D1, $81
            random_choice $81, $80, $D1
        end_interrupt
        random_choice $80, $D1, $81
        random_choice $80, $80, $81
        no_interrupt
            random_choice $80, $81, $D1
            random_choice $80, $81, $D1
    end_block
    end_interrupt

    react_to_cmd $00, $2B, $00
    begin_block
        random_choice $D1, $AA, $AA
    end_cond_block

    react_to_cmd $01, $2B, $00
    begin_block
        random_choice $BC, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_148:
_10BD32:
    check_status $0F, $03, $40
    begin_block
        set_char_param $1D, $40
        .byte $A5
    end_cond_block

    normal_react
    begin_block
        random_choice $97, $AA, $AA
        random_choice $DE, $AA, $AA
        random_choice $EA, $AA, $AA
        random_choice $DE, $AA, $AA
        random_choice $1A, $AA, $AA
        random_choice $DE, $AA, $AA
        .byte $A3
    end_block

    if_multi_target
    check_status $0D, $03, $40
    react_to $00, $07, $00
    begin_block
        .byte $CD
    end_cond_block

    react_to_dmg
    begin_block
        .byte $A3
    end_block

; ------------------------------------------------------------------------------


AIScript_149:
_10BD72:
    if_var_eq $01, $02
    begin_block
        no_interrupt
            .byte $45, $45, $45
        end_interrupt
        no_interrupt
            .byte $C2, $CC, $84
        end_interrupt
        no_interrupt
            .byte $CD, $CC, $84
    end_cond_block
    end_interrupt

    if_var_eq $00, $02
    begin_block
        show_text $C005
        show_text $C105
        show_text $C205
        show_text $C305
        show_text $C405
        show_text $C505
        show_text $C605
        show_text $C705
        show_text $C805
        change_target $0D, $F0
        .byte $E9
    end_cond_block

    if_var_eq $00, $01
    begin_block
        .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
        set_cond_var $00, $02
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        show_text $2E04
        show_text $2F04
        show_text $3004
        show_text $3104
        show_text $3204
        show_text $3304
        show_text $3404
        show_text $3504
        show_text $3604
        show_text $3704
        set_cond_var $00, $01
        .byte $AA
    end_block

    if_hp_less $0D, $80E8
    react_to_dmg
    begin_block
        show_text $BC04
        show_text $BD04
        show_text $BE04
        change_music $0100
        show_text $BF04
        set_cond_var $01, $02
        .byte $45
    end_cond_block

    react_to_cmd $00, $2B, $00
    begin_block
        set_cond_var $01, $01
        random_choice $22, $33, $45
    end_cond_block

    react_to_cmd $01, $2B, $00
    begin_block
        set_cond_var $01, $01
        random_choice $80, $81, $D7
    end_cond_block

    react_to_dmg
    begin_block
        set_cond_var $01, $01
        .byte $DF
    end_block

; ------------------------------------------------------------------------------


AIScript_14A:
_10BE5C:
    if_hp_less $0D, $2710
    begin_block
        random_choice $D2, $D2, $80
    end_cond_block

    if_hp_less $0D, $3A98
    begin_block
        random_choice $B6, $B6, $80
        random_choice $B9, $B9, $80
    end_cond_block

    if_hp_less $0D, $4E20
    begin_block
        random_choice $C2, $C2, $80
        random_choice $CC, $CC, $80
    end_cond_block

    if_hp_less $0D, $61A8
    begin_block
        random_choice $84, $84, $80
        random_choice $DC, $DC, $80
    end_cond_block

    if_hp_less $0D, $7530
    begin_block
        random_choice $DB, $DB, $80
        random_choice $CF, $CF, $80
    end_cond_block

    if_hp_less $0D, $88B8
    begin_block
        random_choice $CA, $CA, $80
        random_choice $CD, $CD, $80
    end_cond_block

    normal_react
    begin_block
        .byte $AA, $AA, $D2
    end_block

    if_hp_less $0D, $2710
    react_to_magic $00, $D2
    begin_block
        .byte $D2
    end_block

; ------------------------------------------------------------------------------


AIScript_14B:
_10BEC0:
    normal_react
    begin_block
        random_choice $DA, $C2, $81
        random_choice $BC, $80, $81
        random_choice $DB, $C2, $81
        random_choice $BC, $80, $81
        random_choice $DC, $C2, $81
        random_choice $BC, $80, $81
        random_choice $CD, $C2, $81
        random_choice $BC, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_14C:
_10BEE7:
    if_var_eq $00, $01
    begin_block
        random_choice $80, $E5, $81
        random_choice $80, $80, $23
        random_choice $80, $E5, $81
        random_choice $80, $80, $3A
        random_choice $80, $E5, $81
        random_choice $80, $80, $1C
        set_cond_var $00, $00
        .byte $22
    end_cond_block

    normal_react
    begin_block
        set_cond_var $00, $01
        change_target $23, $F0
        .byte $A0
    end_block

    react_to_summon
    begin_block
        show_text $DA01
        change_music $F003
        .byte $D7
    end_block

; ------------------------------------------------------------------------------


AIScript_14D:
_10BF28:
    if_hp_less $0D, $0001
    begin_block
        .byte $EF
    end_cond_block

    if_var_eq $01, $01
    begin_block
        .byte $80
    end_cond_block

    if_hp_less $0D, $A7F8
    begin_block
        show_text $8101
        .byte $81
        no_interrupt
            show_text $8203
            show_text $8303
            .byte $33, $22, $45
        end_interrupt
        no_interrupt
            show_text $6202
            show_text $6302
            show_text $6402
            show_text $6502
            show_text $6602
            set_cond_var $00, $01
            set_cond_var $01, $01
            .byte $AA
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $31, $30, $32
        random_choice $DE, $DE, $42
        .byte $80
    end_block

    if_var_eq $00, $03
    react_to $00, $07, $00
    begin_block
        no_interrupt
            show_text $6B03
            set_event_flag $02, $80
            change_target $0D, $F0
            .byte $A5
    end_cond_block
    end_interrupt

    if_var_eq $00, $02
    react_to $00, $07, $00
    begin_block
        show_text $6903
        show_text $6A03
        set_cond_var $00, $03
        .byte $AA
    end_cond_block

    if_var_eq $00, $01
    react_to $00, $07, $00
    begin_block
        show_text $6702
        show_text $6802
        set_cond_var $00, $02
        .byte $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_14E:
_10BFC9:
    normal_react
    begin_block
        .byte $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_14F:
_10BFD1:
    normal_react
    begin_block
        .byte $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_150:
_10BFD9:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_151:
_10BFE1:
    normal_react
    begin_block
        random_choice $80, $8E, $81
        random_choice $8E, $8E, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_152:
_10BFF0:
    if_alone
    begin_block
        show_text $0C01
        show_text $0D01
        show_text $0E01
        show_text $0F01
        show_text $1001
        show_text $1101
        show_text $1201
        show_text $1301
        show_monster $01, $08
        .byte $EE
        .byte $FD, $F5, $F0, $22 ;Unknown command
    end_cond_block

    normal_react
    begin_block
        no_interrupt
            show_text $0B01
            change_target $0E, $F0
            .byte $AC
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_153:
_10C033:
    if_monsters_same
    begin_block
        .byte $A2
    end_cond_block

    normal_react
    begin_block
        .byte $AA
    end_block

    react_to_magic $00, $AC
    begin_block
        random_choice $80, $80, $81
    end_block

; ------------------------------------------------------------------------------


AIScript_154:
_10C04B:
    normal_react
    begin_block
        random_choice $C4, $C4, $80
        random_choice $C4, $C4, $80
        random_choice $C4, $D5, $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_155:
_10C05E:
    normal_react
    begin_block
        random_choice $80, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_156:
_10C069:
    check_status $24, $02, $80
    begin_block
        change_target $22, $F0
        random_choice $3A, $16, $38
        change_target $22, $F0
        random_choice $18, $1C, $1D
    end_cond_block

    check_status $0D, $02, $80
    begin_block
        change_target $0D, $F0
        random_choice $1F, $2F, $27
        change_target $0D, $F0
        random_choice $1F, $44, $3D
    end_cond_block

    normal_react
    begin_block
        random_choice $1F, $44, $3D
    end_block

    react_to $00, $04, $00
    begin_block
        .byte $2D
    end_block

; ------------------------------------------------------------------------------


AIScript_157:
_10C0A6:
    if_var_eq $00, $00
    check_status $0D, $00, $20
    begin_block
        change_target $0D, $F0
        .byte $29
    end_cond_block

    if_var_eq $00, $01
    begin_block
        random_choice $88, $88, $94
        random_choice $88, $88, $95
        random_choice $88, $88, $93
        set_cond_var $00, $00
        random_choice $88, $88, $97
    end_cond_block

    if_var_eq $00, $02
    begin_block
        random_choice $91, $91, $82
        random_choice $91, $91, $83
        random_choice $91, $91, $8F
        set_cond_var $00, $00
        random_choice $91, $91, $87
    end_cond_block

    if_var_eq $00, $03
    begin_block
        random_choice $9D, $9D, $85
        random_choice $9D, $9D, $86
        set_cond_var $00, $00
        random_choice $9D, $9D, $9A
    end_cond_block

    if_var_eq $00, $04
    begin_block
        random_choice $98, $98, $8A
        random_choice $98, $98, $8B
        set_cond_var $00, $00
        random_choice $98, $98, $9C
    end_cond_block

    if_var_eq $00, $05
    begin_block
        random_choice $84, $84, $90
        random_choice $84, $84, $92
        random_choice $84, $84, $89
        set_cond_var $00, $00
        random_choice $84, $84, $9F
    end_cond_block

    if_var_eq $00, $06
    begin_block
        random_choice $99, $99, $8C
        random_choice $99, $99, $8E
        set_cond_var $00, $00
        random_choice $99, $99, $9E
    end_cond_block

    if_var_eq $00, $07
    begin_block
        .byte $9B
    end_cond_block

    normal_react
    begin_block
        random_choice $94, $95, $97
        random_choice $82, $83, $87
        random_choice $85, $86, $9A
        random_choice $8A, $8B, $9C
        random_choice $90, $92, $9F
        random_choice $8C, $8E, $9E
    end_block

    react_to_magic $00, $88
    begin_block
        show_text $5103
        set_cond_var $00, $01
        .byte $AA
    end_cond_block

    react_to_magic $00, $91
    begin_block
        show_text $5003
        set_cond_var $00, $02
        .byte $AA
    end_cond_block

    react_to_magic $00, $9D
    begin_block
        show_text $5203
        set_cond_var $00, $03
        .byte $AA
    end_cond_block

    react_to_magic $00, $98
    begin_block
        show_text $5303
        set_cond_var $00, $04
        .byte $AA
    end_cond_block

    react_to_magic $00, $84
    begin_block
        show_text $5403
        set_cond_var $00, $05
        .byte $AA
    end_cond_block

    react_to_magic $00, $99
    begin_block
        show_text $5503
        set_cond_var $00, $06
        .byte $AA
    end_cond_block

    react_to_magic $00, $9B
    begin_block
        show_text $5603
        set_cond_var $00, $07
        .byte $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_158:
_10C1D3:
    check_status $24, $00, $08
    begin_block
        .byte $AE
    end_cond_block

    normal_react
    begin_block
        random_choice $CF, $CF, $EB
        random_choice $80, $CF, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_159:
_10C1E9:
    if_var_eq $02, $01
    begin_block
        no_interrupt
            set_char_param $1C, $02
            show_text $A902
            change_target $0A, $F0
            .byte $33
            change_target $0A, $F0
            .byte $AC
    end_cond_block
    end_interrupt

    if_var_eq $01, $01
    if_alone
    begin_block
        .byte $EF
    end_cond_block

    if_var_eq $01, $01
    begin_block
        change_target $0A, $F0
        random_choice $81, $30, $32
        change_target $0A, $F0
        random_choice $81, $31, $D7
    end_cond_block

    if_var_eq $00, $02
    begin_block
        no_interrupt
            random_choice $80, $80, $EA
            random_choice $80, $80, $81
        end_interrupt
        no_interrupt
            random_choice $80, $80, $81
            random_choice $80, $80, $81
        end_interrupt
        no_interrupt
            random_choice $80, $80, $EA
            random_choice $80, $80, $81
        end_interrupt
        no_interrupt
            random_choice $80, $80, $81
            random_choice $80, $80, $34
    end_cond_block
    end_interrupt

    if_alone
    begin_block
        show_text $7C07
        show_text $7D02
        show_text $7E02
        show_text $7F02
        set_char_param $1C, $02
        set_cond_var $00, $02
        .byte $8B
    end_cond_block

    if_var_eq $00, $01
    begin_block
        .byte $AA
    end_cond_block

    normal_react
    begin_block
        show_text $AC03
        show_text $AD03
        show_text $AE03
        set_cond_var $00, $01
        .byte $AA
    end_block

    if_dying
    if_alone
    begin_block
        show_text $AF03
        .byte $AA
    end_cond_block

    if_hp_less $0D, $270F
    if_var_eq $01, $00
    react_to_dmg
    check_event_flag $01, $04
    begin_block
        no_interrupt
            change_music $2200
            show_monster $80, $84
            .byte $EE
            show_text $8E07
            show_text $8007
            show_text $8F07
            show_text $9007
            show_text $9107
            show_text $9207
            show_text $9307
            show_text $9407
            set_char_param $1C, $02
            set_cond_var $01, $01
            .byte $AA
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_15A:
_10C2F0:
    normal_react
    begin_block
        random_choice $CA, $CC, $80
        random_choice $CA, $CC, $80
        change_target $23, $F0
        .byte $97, $AB, $AB
        show_monster $02, $40
        .byte $EE
    end_block

    react_to $01, $2B, $00
    begin_block
        random_choice $D1, $AA, $AA
    end_cond_block

    react_to $00, $2B, $00
    begin_block
        random_choice $D2, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_15B:
_10C31E:
    normal_react
    begin_block
        .byte $E0
    end_block

    react_to_dmg
    if_hp_less $0D, $2710
    begin_block
        change_target $0D, $F0
        .byte $B1
    end_block

; ------------------------------------------------------------------------------


AIScript_15C:
_10C334:
    normal_react
    begin_block
        .byte $E0
    end_block

    react_to_dmg
    if_hp_less $0D, $2710
    begin_block
        change_target $0D, $F0
        .byte $B1
    end_block

; ------------------------------------------------------------------------------


AIScript_15D:
_10C34A:
    if_var_eq $00, $01
    begin_block
        show_text $4003
        random_choice $EA, $80, $89
        show_text $4103
        show_text $4207
        show_text $4305
        show_text $4405
        random_choice $8C, $80, $81
        show_text $4503
        random_choice $D3, $80, $8A
        show_text $4603
        show_text $4703
        show_text $4803
        show_monster $87, $40
        .byte $EE
    end_cond_block

    normal_react
    begin_block
        random_choice $D3, $80, $8A
        random_choice $BE, $80, $81
        random_choice $8B, $80, $81
        random_choice $EA, $80, $89
        random_choice $8C, $80, $81
        random_choice $C6, $80, $89
    end_block

    if_hp_less $0D, $A410
    if_var_eq $00, $00
    react_to_dmg
    begin_block
        no_interrupt
            show_text $3D03
            show_text $3E03
            show_text $3F03
            set_cond_var $00, $01
            random_choice $8B, $80, $81
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_15E:
_10C3C9:
    normal_react
    begin_block
        .byte $18
        show_monster $02, $80
        .byte $EE
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_15F:
_10C3D6:
    normal_react
    begin_block
        random_choice $80, $BE, $81
        random_choice $80, $BE, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_160:
_10C3E5:
    if_var_eq $00, $01
    begin_block
        no_interrupt
            show_text $7B03
            change_target $0D, $F0
            .byte $D8, $E7
            set_cond_var $00, $00
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $D4, $99, $80
        random_choice $80, $1A, $AB
        random_choice $99, $80, $D4
        random_choice $D3, $AB, $80
    end_block

    react_to_magic $00, $4D
    begin_block
        no_interrupt
            .byte $E7
            show_text $7A03
            set_cond_var $00, $01
            .byte $AA
    end_cond_block
    end_interrupt

    if_var_eq $00, $00
    react_to $01, $2B, $00
    begin_block
        random_choice $16, $2D, $AA
    end_cond_block

    if_var_eq $00, $00
    react_to $00, $2B, $00
    begin_block
        random_choice $29, $2D, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_161:
_10C443:
    normal_react
    begin_block
        random_choice $80, $E2, $81
    end_block

    if_dying
    begin_block
        show_text $5903
        show_monster $85, $40
        .byte $EE
    end_cond_block

    react_to_item $00, $EC
    begin_block
        show_text $C903
        show_text $CA03
        .byte $DF
    end_block

; ------------------------------------------------------------------------------


AIScript_162:
_10C46B:
    normal_react
    begin_block
        random_choice $80, $AB, $AA
        random_choice $80, $DB, $81
    end_block

    if_dying
    if_alone
    if_var_eq $00, $01
    begin_block
        show_text $5B03
        .byte $AA
    end_cond_block

    if_dying
    if_var_eq $00, $01
    begin_block
        .byte $AA
    end_cond_block

    if_dying
    begin_block
        .byte $FD, $F7, $0F, $F0
        ;no_interrupt
            show_text $4F02
            set_cond_var $00, $01
            show_monster $85, $60
            .byte $EE
    end_block
    ;end_interrupt

; ------------------------------------------------------------------------------


AIScript_163:
_10C4AE:
    normal_react
    begin_block
        random_choice $90, $80, $81
        random_choice $EA, $80, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_164:
_10C4BD:
    normal_react
    begin_block
        random_choice $80, $B2, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_165:
_10C4C8:
    normal_react
    begin_block
        random_choice $2A, $DB, $81
        change_target $0D, $F0
        .byte $30
        random_choice $2A, $DB, $81
        random_choice $2A, $DB, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_166:
_10C4E0:
    normal_react
    begin_block
        random_choice $80, $9C, $81
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_167:
_10C4EB:
    normal_react
    begin_block
        random_choice $9F, $DD, $BF
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_168:
_10C4F6:
    check_status $0F, $03, $40
    begin_block
        set_char_param $1D, $40
        .byte $A2
    end_cond_block

    if_var_eq $00, $01
    begin_block
        random_choice $80, $81, $B2
    end_cond_block

    normal_react
    begin_block
        random_choice $80, $B2, $81
        no_interrupt
            show_text $4F02
            show_monster $00, $E0
            .byte $EE, $A3
            set_cond_var $00, $01
    end_block
    end_interrupt

    end_block
; ------------------------------------------------------------------------------


AIScript_169:
_10C528:
    if_var_eq $00, $01
    begin_block
        random_choice $C2, $80, $83
        random_choice $CC, $CA, $DC
        no_interrupt
            random_choice $C2, $80, $83
            random_choice $9A, $87, $88
        end_interrupt
        no_interrupt
            random_choice $80, $80, $EB
            random_choice $80, $80, $B6
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        .byte $AA
        no_interrupt
            set_cond_var $00, $01
            .byte $D1
    end_block
    end_interrupt

    react_to_magic $00, $AC
    begin_block
        set_cond_var $00, $00
        .byte $B9
    end_cond_block

    if_hp_less $0D, $4E20
    react_to_dmg
    begin_block
        random_choice $AC, $AA, $AA
    end_block

; ------------------------------------------------------------------------------


AIScript_16A:
_10C577:
    normal_react
    begin_block
        change_target $1D, $F0
        random_choice $33, $22, $33
        change_target $1C, $F0
        random_choice $30, $32, $31
        change_target $1D, $F0
        random_choice $22, $33, $22
        change_target $1C, $F0
        random_choice $30, $32, $31
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_16B:
_10C59E:
    if_monster_visible $00, $D0
    begin_block
        no_interrupt
            random_choice $45, $C8, $81
            random_choice $3F, $81, $81
        end_interrupt
        no_interrupt
            random_choice $45, $C8, $81
            random_choice $C2, $C8, $81
        end_interrupt
        no_interrupt
            random_choice $45, $80, $81
            random_choice $3F, $C8, $81
        end_interrupt
        .byte $A6
    end_cond_block

    if_var_eq $00, $01
    begin_block
        .byte $80
        random_choice $45, $80, $AA
        .byte $AA
    end_cond_block

    if_hp_less $0D, $3A98
    begin_block
        show_text $4A05
        change_music $030A
        .byte $AA
        change_music $830A
        .byte $A6
        set_cond_var $00, $01
        .byte $AA
    end_cond_block

    if_hp_less $0D, $7530
    begin_block
        show_text $4A05
        change_music $030A
        .byte $AA
        change_music $830A
        .byte $A6, $AA, $AA, $80, $23, $AA, $AA
    end_cond_block

    normal_react
    begin_block
        .byte $AA
        show_text $4A05
        change_music $030A
        .byte $AA
        change_music $830A
        .byte $A6, $AA, $AA, $23, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_16C:
_10C622:
    if_monster_visible $00, $C8
    begin_block
        no_interrupt
            random_choice $45, $C8, $81
            random_choice $3F, $81, $81
        end_interrupt
        no_interrupt
            random_choice $45, $C8, $81
            random_choice $C2, $C8, $81
        end_interrupt
        no_interrupt
            random_choice $45, $80, $81
            random_choice $3F, $C8, $81
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        change_music $0409
        .byte $AA
        change_music $8409
        .byte $C8, $AA, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_16D:
_10C65F:
    if_monster_visible $00, $C4
    begin_block
        no_interrupt
            random_choice $45, $45, $81
            random_choice $3F, $81, $81
        end_interrupt
        no_interrupt
            random_choice $45, $45, $81
            random_choice $C2, $C2, $81
        end_interrupt
        no_interrupt
            random_choice $45, $80, $81
            random_choice $3F, $C8, $81
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $80, $81, $AA
        random_choice $80, $80, $81
        random_choice $80, $81, $23
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_16E:
_10C69C:
    if_monster_visible $00, $C2
    begin_block
        no_interrupt
            random_choice $45, $45, $81
            random_choice $3F, $81, $81
        end_interrupt
        no_interrupt
            random_choice $45, $45, $81
            random_choice $C2, $C2, $81
        end_interrupt
        no_interrupt
            random_choice $45, $80, $81
            random_choice $3F, $C8, $81
    end_cond_block
    end_interrupt

    normal_react
    begin_block
        random_choice $91, $A7, $80
        random_choice $31, $80, $23
        random_choice $33, $30, $80
        random_choice $22, $32, $AA
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_16F:
_10C6DD:
    if_var_eq $02, $01
    begin_block
        .byte $AA
    end_cond_block

    if_var_eq $03, $01
    begin_block
        show_text $A007
        show_text $A107
        show_text $A207
        show_text $A307
        show_text $A407
        show_text $A507
        show_text $A607
        show_text $A707
        show_text $A807
        change_target $0D, $F0
        .byte $DF
        set_cond_var $02, $01
        change_target $0D, $F0
        .byte $DF
    end_cond_block

    normal_react
    begin_block
        show_text $9505
        show_text $9605
        show_text $9705
        .byte $E7
        show_text $9807
        show_text $9907
        show_text $9A07
        show_text $9B07
        show_text $9C07
        show_text $9D07
        show_text $9E07
        show_text $9F07
        set_cond_var $03, $01
        change_target $0D, $F0
        .byte $DF
    end_block

    react_to_magic $00, $AC
    begin_block
        no_interrupt
            show_text $AA03
            show_text $AB04
            change_target $05, $F0
            .byte $9B, $EF
    end_block
    end_interrupt

; ------------------------------------------------------------------------------


AIScript_170:
_10C770:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_171:
_10C778:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_172:
_10C780:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_173:
_10C788:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_174:
_10C790:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_175:
_10C798:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_176:
_10C7A0:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_177:
_10C7A8:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_178:
_10C7B0:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_179:
_10C7B8:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_17A:
_10C7C0:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_17B:
_10C7C8:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_17C:
_10C7D0:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block
; ------------------------------------------------------------------------------


AIScript_17D:
_10C7D8:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block

; ------------------------------------------------------------------------------

AIScript_17E:
_10C7E4:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block

; ------------------------------------------------------------------------------

AIScript_17F:
_10C7EC:
    normal_react
    begin_block
        .byte $80
    end_block

    end_block

; ------------------------------------------------------------------------------

end_fixed_block AIScript
