
; +----------------------------------------------------------------------------+
; |                                                                            |
; |                              FINAL FANTASY V                               |
; |                                                                            |
; +----------------------------------------------------------------------------+
; | file: src/text/text-main.asm                                               |
; |                                                                            |
; | description: game text                                                     |
; +----------------------------------------------------------------------------+

.p816

.include "macros.inc"
.include "const.inc"

; ------------------------------------------------------------------------------

.include "text/dlg_jp.inc"
.include "text/item_name_jp.inc"
.include "text/magic_name_jp.inc"
.include "text/map_title_jp.inc"

; ------------------------------------------------------------------------------

.segment "dlg_ptrs"

; c8/2220
DlgPtrs:
        ptr_tbl Dlg

; ------------------------------------------------------------------------------

.segment "dlg"

; ca/0000
Dlg:
        .incbin "dlg_jp.dat"

; ------------------------------------------------------------------------------

.segment "map_title"

; d0/7000
MapTitlePtrs:
        fixed_block $0200
        ptr_tbl MapTitle
        end_fixed_block

; d0/7200
MapTitle:
        fixed_block $0600
        .incbin "map_title_jp.dat"
        end_fixed_block

; ------------------------------------------------------------------------------

.segment "item_name"

; d1/1380
ItemName:
        .incbin "item_name_jp.dat"

; ------------------------------------------------------------------------------

.segment "magic_name"

; d1/1c80
MagicName:
        .incbin "magic_name_jp.dat"

; ------------------------------------------------------------------------------
