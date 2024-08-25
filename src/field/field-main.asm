; +-------------------------------------------------------------------------+
; |                                                                         |
; |                             FINAL FANTASY V                             |
; |                                                                         |
; +-------------------------------------------------------------------------+
; | file: field/field-main.asm                                              |
; |                                                                         |
; | description: field program                                              |
; +-------------------------------------------------------------------------+

.p816

; ===========================================================================

.include "macros.inc"
.include "hardware.inc"
.include "const.inc"

.include "text/dlg_jp.inc"
.include "text/item_name_jp.inc"
.include "text/magic_name_jp.inc"
.include "text/map_title_jp.inc"

.include "field/event_cond.inc"
.include "field/event_script.inc"
.include "field/event_trigger.inc"
.include "field/entrance_trigger.inc"
.include "field/npc_prop.inc"
.include "field/npc_script.inc"
.include "field/world_tilemap.inc"

.import _c10003, _c10006
.import ExecBattle_ext
.import ExecMenu_ext, _c2a008
.import ShowCutscene_ext, Decomp_ext
.import InitSound_ext, ExecSound_ext

.import WindowPal, WindowGfx, BigFontGfx, KanjiGfx, TimerFontGfx
.import MapBG3Gfx, MapBG3GfxPtrs, MapGfx, MapGfxPtrs
.import MapPal, MapAnimGfx, MapSpritePal, MapOverlayGfx
.import WorldPal, WorldTileAttr, WorldGfx, MinimapSpriteGfx

; ===========================================================================

.segment "field_code"

; ---------------------------------------------------------------------------

.proc Start

_0000:  sei
        clc
        xce
        longi
        shorta
        stz     hMEMSEL
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #$8f
        sta     hINIDISP
        lda     #0
        sta     hNMITIMEN
        lda     #0
        xba
        ldx     #$1fff
        txs
        lda     #0
        pha
        plb
        ldx     #$0b00
        phx
        pld
        ldx     #0
        stx     $06
        jsl     InitSound_ext
        lda     #$f1                    ; cutscene $f1 (title/credits)
        jsr     ShowCutscene
        jsr     InitHardware
        jsr     _c0490a
        lda     #3
        sta     $0134
        jsl     ExecMenu_ext            ; game load menu
        jsr     _c044e3
        jsr     InitHardware
        jsr     InitInterrupts
        lda     $0139
        beq     NewGame
        jsr     _c0491d                 ; reset ram $0b00-$1d00
        lda     $0af9
        sta     $0b60
        lsr
        sta     $0b5f
        lda     #1
        sta     $bd                     ; show party sprite
        sta     $bc                     ; enable walking animation
        inc
        sta     $b9
        lda     $0ad8                   ; x position
        sta     $1088
        lda     $0ad9                   ; y position
        sta     $1089
        jsr     LoadMapNoFade
        jmp     Main

NewGame:
        jsr     _c048fa                 ; init character data (new game)
        jsr     _c048ed                 ; init event flags
        jsr     _c048dd                 ; init npc flags
        jsr     _c04528                 ; init character names
        jsr     _c0450a                 ; init vehicles
        lda     #1
        sta     $bd                     ; show party sprite
        sta     $bc                     ; enable walking animation
        ldx     #$0010                  ; event $10 (intro)
        stx     $ce
        lda     #$01
        sta     $57
        lda     #$81
        sta     hNMITIMEN
        cli
        jsr     ExecEvent
        stz     $57
        stz     $58
        stz     $55
        jmp     Main

.endproc

; ---------------------------------------------------------------------------

; [ map main loop ]

.proc Main

_00ad:  jsr     WaitVBlank
        lda     $02
        and     #JOY_X
        beq     _00be
        lda     $53
        beq     _00c1
        lda     $5d
        beq     _00c1
_00be:  jmp     _0147
_00c1:  lda     $0b61
        and     #$1f
        bne     _0147                   ; branch if partially scrolled
        lda     $0b63                   ; bg1 y position
        and     #$1f
        bne     _0147                   ; branch if partially scrolled
        lda     #$00
        sta     $0135
        lda     $0adc
        bne     _00ef                   ; branch if in a vehicle ???
        ldx     $0ad6                   ; map index
        cpx     #$0005
        bcc     _00ea                   ; branch if not on a world map
        lda     #$fd
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        beq     _00ef
_00ea:  lda     #$80                    ; enable tent/cabin/save
        sta     $0135
_00ef:  stz     $08
        lda     $53
        beq     _00fc
        lda     $110f                   ; enable warp/teleport ???
        and     #$03
        sta     $08
_00fc:  lda     $0135
        ora     $08
        sta     $0135
        lda     #$00                    ; menu command $00 (main menu)
        sta     $0134
        jsr     OpenMenu
        lda     #$02
        sta     $55
        lda     $0ad8
        sta     $1088
        lda     $0ad9
        sta     $1089
        jsr     ReloadMap
        lda     $0139                   ; item used in menu
        cmp     #$f0
        bne     _012b                   ; branch if not tent
        ldx     #$0022
        bra     _013b
_012b:  cmp     #$f1
        bne     _0134                   ; branch if not cabin
        ldx     #$0024
        bra     _013b
_0134:  cmp     #$3e                    ; judgment staff ???
        bne     _013e
        ldx     #$0032
_013b:  jsr     ExecTriggerScript
_013e:  stz     $16aa
        jsr     PoisonMosaic                   ; pixelate screen (poison)
        jmp     _00ad
_0147:  jsr     CheckTriggers
        jsr     _c0a18b                   ; update timer
        lda     $58
        beq     _0156
        stz     $58
        jmp     _00ad
_0156:  lda     $6e
        beq     _0160
        jsr     LoadMap
        jmp     _00ad
_0160:  ldx     $0ad6                   ; map index
        cpx     #$0005
        jcs     _0262
        jsr     _c0073e
        lda     $58
        beq     _0177
        stz     $58
        jmp     _00ad
_0177:  lda     $61
        and     #$1f
        bne     _01fb
        lda     $63
        and     #$1f
        bne     _01fb
        lda     $0ad6                   ; map index
        cmp     #$01
        bne     _01a1
        lda     $0ad9
        cmp     #$a1
        bne     _01a1
        lda     $0ad8
        cmp     #$9f
        bcc     _01a1
        cmp     #$a2
        bcs     _01a1
        ldx     #$0012
        bra     _01ef
_01a1:  lda     $0ad6                   ; map index
        bne     _01c8
        lda     $0adc
        cmp     #$06
        bne     _01c8
        lda     $0ad8
        cmp     #$3d
        bcc     _01c8
        cmp     #$43
        bcs     _01c8
        lda     $0ad9
        cmp     #$9e
        bcc     _01c8
        cmp     #$a5
        bcs     _01c8
        ldx     #$0020
        bra     _01ef
_01c8:  lda     $0ad6                   ; map index
        cmp     #$02
        bne     _01fb
        lda     $0adc
        cmp     #$05
        bcc     _01fb
        lda     $0ad8
        cmp     #$b6
        bcc     _01fb
        cmp     #$ba
        bcs     _01fb
        lda     $0ad9
        cmp     #$87
        bcc     _01fb
        cmp     #$8b
        bcs     _01fb
        ldx     #$0018
_01ef:  jsr     ExecTriggerScript
        lda     $58
        beq     _01fb
        stz     $58
        jmp     _00ad
_01fb:  jsr     _c0cb11
        lda     $55
        beq     _0220
        lda     #$ff
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        bne     _0220
        jsr     RandomBattle
        lda     $0ad8
        sta     $1088
        lda     $0ad9
        sta     $1089
        jsr     ReloadMap
        jmp     _00ad
_0220:  stz     $55
        lda     $03
        and     #>JOY_Y
        beq     _0246
        lda     #$fb
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        beq     _0246
        jsr     ShowMinimap
        lda     $0ad8
        sta     $1088
        lda     $0ad9
        sta     $1089
        jsr     ReloadMap
        jmp     _00ad
_0246:  jsr     _c00f8c
        jsr     _c01a1d
        jsr     _c04c95                   ; clear sprite data
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01ec5
        jsr     _c01e64
        jsr     _c0420a
        jmp     _00ad
        rts                 ; unused rts
_0262:  jsr     _c00d3d
        lda     $58
        beq     _026e
        stz     $58
        jmp     _00ad
_026e:  lda     $10fb       ; tile properties byte 2
        cmp     #$00
        bne     _0281       ; branch if not a map exit
        jsr     LoadParentMap
        ldx     #$001c
        jsr     ExecTriggerScript
        jmp     _00ad
_0281:  jsr     _c0ca69       ;
        lda     $55
        beq     _029a
        lda     #$ff
        jsr     _c0ca3c       ; get event flag $01ff (disable random battles)
        cmp     #$00
        bne     _029a
        jsr     RandomBattle
        jsr     ReloadMap
        jmp     _00ad
_029a:  stz     $55
        jsr     _c032ab       ; update objects
        jsr     _c011c2
        lda     $6e
        beq     _02ac
        jsr     LoadMap
        jmp     _00ad
_02ac:  lda     $58
        beq     _02b5
        stz     $58
        jmp     _00ad
_02b5:  jsr     _c01ae4
        jsr     _c03bac
        jsr     _c04c95       ; clear sprite data
        jsr     _c04834
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        jsr     _c0420a
        jmp     _00ad
        rts                 ; unused rts

.endproc

; ---------------------------------------------------------------------------

.proc FieldNMI

_02d1:  php
        longa
        pha
        phx
        phy
        phb
        phd
        lda     #0
        shorta
        ldx     #$0b00
        phx
        pld
        lda     #0
        pha
        plb
        lda     hRDNMI
        lda     hRDNMI
        stz     hMDMAEN
        stz     $40
        lda     $47
        sta     hCGADSUB       ; addition/subtraction
        lda     $49
        sta     hTS       ; subscreen designation
        jsr     TfrPal
        jsr     TfrSprites
        jsr     _c04a7a
        jsr     _c04653       ; update screen pixelation
        jsr     _c049ff       ; update fixed color
        lda     $a3
        beq     _0314
        stz     $a3
        jsr     _c040d8       ; copy data to vram
_0314:  lda     $52
        jne     _041f
        jsr     _c04bc0       ; update scrolling registers
        lda     $53
        bne     _0360
        jsr     _c0637e
        jsr     _c09695
        jsr     _c0975f
        jsr     _c0964c
        jsr     _c09722
        lda     $9f
        beq     _0348
        stz     $9f
        lda     $ba
        beq     _0348
        and     #$01
        bne     _0345
        jsr     _c064bb
        jmp     _0348
_0345:  jsr     _c06465
_0348:  lda     $a1
        beq     _0354
        stz     $a1
        jsr     TfrPartyGfx
        jmp     _0408
_0354:  lda     $a2
        beq     _035d
        stz     $a2
        jsr     _c01e14
_035d:  jmp     _0408
_0360:  lda     $a0
        beq     _036c
        stz     $a0
        jsr     _c05e2b       ; copy tile layout to vram ???
        jmp     _0408
_036c:  lda     $a5
        beq     _0378
        stz     $a5
        jsr     _c08ba4
        jmp     _0408
_0378:  lda     $a6
        beq     _038b
        stz     $a6
        ldx     $06
        stx     $71
        jsr     _c06e7a
        jsr     _c08be4
        jmp     _03f9
_038b:  lda     $a7
        beq     _0392
        jsr     _c08e23
_0392:  jsr     _c08f78       ; draw yes/no indicator
        lda     $9f
        beq     _03ed
        stz     $9f
        lda     $70
        and     #$01
        bne     _03c7
        ldx     $06
        stx     $71
        jsr     _c06df5       ; horizontal scrolling (bg1)
        lda     $1121
        and     #$40
        bne     _03b7
        ldx     #$1000
        stx     $71
        jsr     _c06df5       ; horizontal scrolling (bg2)
_03b7:  lda     $1121
        bmi     _03c4
        ldx     #$2000
        stx     $71
        jsr     _c06df5       ; horizontal scrolling (bg3)
_03c4:  jmp     _0408
_03c7:  ldx     $06
        stx     $71
        jsr     _c06e7a       ; vertical scrolling (bg1)
        lda     $1121
        and     #$40
        bne     _03dd
        ldx     #$1000
        stx     $71
        jsr     _c06e7a       ; vertical scrolling (bg2)
_03dd:  lda     $1121
        bmi     _03ea
        ldx     #$2000
        stx     $71
        jsr     _c06e7a       ; vertical scrolling (bg3)
_03ea:  jmp     _0408
_03ed:  lda     $a1
        beq     _03f9
        stz     $a1
        jsr     TfrPartyGfx
        jmp     _0408
_03f9:  lda     $a2
        beq     _0405
        stz     $a2
        jsr     _c01e14
        jmp     _0408
_0405:  jsr     _c0996d       ; copy map animation graphics to vram
_0408:  jsr     _c05e8a
        jsr     _c05f3e
        lda     $5e         ; hdma enable
        sta     hHDMAEN
        jsr     _c09a00
        jsr     _c09799       ; update palette animation
        jsr     _c04931
        jsr     _c047aa
_041f:  jsr     _c04c90
        jsr     _c04d8e
        inc     $094a
        bne     _0437
        inc     $094b
        bne     _0437
        inc     $094c
        bne     _0437
        inc     $094d
_0437:  inc     $3f
        inc     $3e
        inc     $51         ; set vblank flag
        longa
        pld
        plb
        ply
        plx
        pla
        plp
        rti

.endproc

; ---------------------------------------------------------------------------

.proc FieldIRQ

_0446:  php
        longa
        pha
        phx
        phy
        phb
        phd
        lda     #0
        shorta
        ldx     #$0b00
        phx
        pld
        lda     #0
        pha
        plb
        lda     hTIMEUP
        inc     $40
        longa
        pld
        plb
        ply
        plx
        pla
        plp
        rti

.endproc

; ---------------------------------------------------------------------------

; [ execute trigger script ]

.proc ExecTriggerScript

_046a:  longa
        lda     f:EventCondPtrs,x
        sta     $23
        lda     f:EventCondPtrs+2,x
        sta     $26
        lda     $06
        shorta
_047c:  ldx     $23
        lda     f:EventCond::Start,x
        cmp     #$ff                    ; $ff: execute event
        jeq     _05fc
        cmp     #$fe                    ; $fe: if event flag $00xx is set
        bne     _049b
        lda     f:EventCond::Start+1,x
        jsr     _c0ca2f                   ; get event flag $00xx
        cmp     #$00
        bne     _04d1
        jmp     _05e5
_049b:  cmp     #$fd                    ; $fd: if event flag $00xx is clear
        bne     _04ad
        lda     f:EventCond::Start+1,x
        jsr     _c0ca2f                   ; get event flag $00xx
        cmp     #$00
        beq     _04d1
        jmp     _05e5
_04ad:  cmp     #$fc                    ; $fc: if event flag $01xx is set
        bne     _04bf
        lda     f:EventCond::Start+1,x
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        bne     _04d1
        jmp     _05e5
_04bf:  cmp     #$fb                    ; $fb: if event flag $01xx is clear
        bne     _04da
        lda     f:EventCond::Start+1,x
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        jne     _05e5
_04d1:  ldx     $23
        inx2
        stx     $23
        jmp     _047c
_04da:  cmp     #$fa                    ; $fa: compare ram (1-byte)
        bne     _051c
        longa
        lda     f:EventCond::Start+1,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     f:EventCond::Start+2,x
        and     #$c0
        bne     _0500
        lda     $0500,y
        cmp     f:EventCond::Start+3,x
        beq     _0568
        jmp     _05e5
_0500:  cmp     #$40
        bne     _0510
        lda     $0500,y
        cmp     f:EventCond::Start+3,x
        bcs     _0568
        jmp     _05e5
_0510:  lda     $0500,y
        cmp     f:EventCond::Start+3,x
        bcc     _0568
        jmp     _05e5
_051c:  cmp     #$f9                    ; $f9: compare ram (2-byte)
        bne     _0573
        longa
        lda     f:EventCond::Start+1,x
        and     #$3fff
        tay
        lda     f:EventCond::Start+1,x
        and     #$c000
        bne     _0543
        lda     $0500,y
        cmp     f:EventCond::Start+3,x
        beq     _0568
        lda     $06
        shorta
        jmp     _05e5
_0543:  cmp     #$00
        rti
        bne     _0558
        lda     $0500,y
        cmp     f:EventCond::Start+3,x
        bcs     _0568
        lda     $06
        shorta
        jmp     _05e5
_0558:  lda     $0500,y
        cmp     f:EventCond::Start+3,x
        bcc     _0568
        lda     $06
        shorta
        jmp     _05e5
_0568:  ldx     $23
        inx4
        stx     $23
        jmp     _047c
_0573:  cmp     #$f8                    ; $f8: boolean compare ram (1-byte)
        bne     _05a1
        longa
        lda     f:EventCond::Start+1,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y
        and     f:EventCond::Start+3,x
        bne     _0568
        jmp     _05e5
        lda     $06
        shorta
        ldx     $23
        inx5
        stx     $23
        jmp     _047c
_05a1:  cmp     #$f7                    ; $f7: if facing direction
        bne     _05b1
        lda     f:EventCond::Start+1,x
        cmp     $0adb                   ; facing direction
        bne     _05e5
        jmp     _04d1
_05b1:  cmp     #$f6                    ; $f6: if button pressed
        bne     _05cb
        lda     $02
        and     #JOY_A
        beq     _05e5
        lda     $10b8
        bne     _05e5
        inc     $10b8
        ldx     $23
        inx
        stx     $23
        jmp     _047c
_05cb:  longa                           ; $f5: boolean compare ram (2-byte)
        lda     f:EventCond::Start+1,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y
        and     f:EventCond::Start+3,x
        beq     _0568
        jmp     _05e5
_05e5:  ldx     $23
_05e7:  inx
        lda     f:EventCond::Start,x
        cmp     #$ff
        bne     _05e7
        inx3
        stx     $23
        cpx     $26
        beq     _0617
        jmp     _047c
_05fc:  lda     #$01
        sta     $57
        stz     $ba
        longa
        ldx     $23
        lda     f:EventCond::Start+1,x               ; event index
        sta     $ce
        lda     $06
        shorta
        ldx     $ce
        beq     _0617
        jsr     ExecEvent
_0617:  stz     $57
        rts

.endproc

; ---------------------------------------------------------------------------

; [ check triggers ]

.proc CheckTriggers

_061a:  lda     $53
        bne     _0633
        lda     $0adc
        beq     _0633
        cmp     #$01
        beq     _0633
        cmp     #$05
        beq     _0633
        ldx     $0ad6                   ; map index
        cpx     #$0003
        bcc     _063f
_0633:  lda     $61
        and     #$1f
        bne     _063f
        lda     $63
        and     #$1f
        beq     _0640
_063f:  rts
_0640:  stz     $5a
        longa
        lda     $0ad6                   ; map index
        asl
        tax
        lda     f:EventTriggerPtrs+2,x               ; event triggers
        sta     $23
        lda     f:EventTriggerPtrs,x
        tax
_0654:  cpx     $23
        beq     _0681
        lda     $0ad8
        cmp     f:EventTrigger::SrcPos,x
        beq     _066a
        txa
        clc
        adc     #EventTrigger::ITEM_SIZE
        tax
        jmp     _0654
_066a:  lda     $06
        shorta
        lda     #$01
        sta     $5a
        longa
        lda     f:EventTrigger::Event,x
        asl
        tax
        lda     $06
        shorta
        jsr     ExecTriggerScript
_0681:  longa
        lda     $0ad6                   ; map index
        cmp     #$0005
        bcc     _068e
        lda     $0ad4
_068e:  asl
        tax
        lda     f:EntranceTriggerPtrs+2,x
        sta     $23
        lda     f:EntranceTriggerPtrs,x
        tax
_069b:  cpx     $23
        jeq     _0739
        lda     $0ad8
        cmp     f:EntranceTrigger::SrcPos,x
        beq     _06b4
        txa
        clc
        adc     #EntranceTrigger::ITEM_SIZE
        tax
        jmp     _069b
_06b4:  lda     $59
        and     #$00ff
        jne     _0739
        stz     $16a0                   ; disable map title
        lda     $0ad6                   ; map index
        cmp     #$0005
        bcs     _06d2
        sta     $0af5                   ; set parent map
        lda     $0ad8
        sta     $0af7                   ; set parent map xy position
_06d2:  lda     f:EntranceTrigger::Map,x
        and     #$03ff
        sta     $0ad4
        sta     $0ad6                   ; map index
        cmp     #$0005
        bcs     _06f8
        lda     $06
        shorta
        lda     f:EntranceTrigger::DestX,x
        sta     $1088
        lda     f:EntranceTrigger::DestY,x
        sta     $1089
        bra     _0737
_06f8:  lda     $06
        shorta
        lda     f:EntranceTrigger::Flags,x
        and     #$08
        beq     _0707
        inc     $16a0                   ; enable map title
_0707:  lda     f:EntranceTrigger::Flags,x
        lsr4
        sta     $169e
        lda     f:EntranceTrigger::Dir,x
        and     #$c0
        lsr6
        sta     $b9
        inc
        sta     $ba
        sta     $bf
        lda     f:EntranceTrigger::DestX,x
        and     #$3f
        sta     $1088
        lda     f:EntranceTrigger::DestY,x
        and     #$3f
        sta     $1089
_0737:  inc     $6e
_0739:  lda     $06
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c0073e

_073e:  lda     $61
        and     #$1f
        bne     _074a
        lda     $63
        and     #$1f
        beq     _074b
_074a:  rts
_074b:  jsr     _c01733                   ; update local tile properties (world map)
        lda     $0adc
        dec
        asl2
        sta     $169c
        lda     $0adc
        jne     _07ee
        ldy     $06
        sty     $23
_0763:  ldy     $23
        lda     $0ade,y
        bmi     _07df
        lda     $0add,y
        and     #$e0
        lsr5
        cmp     $0ad6                   ; map index
        bne     _07df
        lda     $0adf,y
        cmp     $0ad8
        bne     _07df
        lda     $0ae0,y
        cmp     $0ad9
        bne     _07df
        lda     $0add,y
        and     #$1c
        cmp     #$10
        beq     _07a4
        cmp     #$14
        beq     _07a4
        lda     $02
        and     #JOY_A
        beq     _07ed
        lda     $10b8
        bne     _07ed
        inc     $10b8
_07a4:  sty     $169c
        tya
        lsr2
        inc
        sta     $0adc
        lda     #$80
        sta     $0ade,y
        lda     $0add,y
        and     #$1c
        lsr2
        tax
        lda     f:_c008b3,x
        sta     $c0
        lda     #$03
        sta     $0adb
        txa
        asl
        tax
        lda     f:BoardVehicleTbl,x
        sta     $26
        lda     f:BoardVehicleTbl+1,x
        sta     $27
        jsr     _c04583
        lda     #$01
        sta     $58
        jmp     ($0b26)
_07df:  lda     $23
        clc
        adc     #$04
        sta     $23
        cmp     #$18
        jne     _0763
_07ed:  rts
_07ee:  dec
        asl2
        tay
        lda     $0add,y                 ; vehicle index
        and     #$1c
        lsr2
        tax
        lda     $10fb                   ; world tile properties byte 2
        and     f:_c00890,x
        bne     _07ed                   ; branch if vehicle can't land
        tyx
        lda     $02
        and     #JOY_A
        beq     _07ed
        lda     $10b8
        bne     _07ed
        inc     $10b8
        ldy     $06
_0814:  tya
        lsr2
        inc
        cmp     $0adc
        beq     _0832
        lda     $0ade,y
        bmi     _0832
        lda     $0adf,y
        cmp     $0ad8
        bne     _0832
        lda     $0ae0,y
        cmp     $0ad9
        beq     _c0088f
_0832:  iny4
        cpy     #$0018
        bne     _0814
        txa
        lsr
        inc2
        tax
        lda     f:LandVehicleTbl,x
        sta     $26
        lda     f:LandVehicleTbl+1,x
        sta     $27
        lda     #$01
        sta     $58
        jmp     ($0b26)

.endproc

; ---------------------------------------------------------------------------

.proc _c00853

_0853:  ldy     $169c
        lda     $0ad8
        sta     $0adf,y
        lda     $0ad9
        sta     $0ae0,y
        lda     $0ade,y
        and     #$7f
        sta     $0ade,y
        lda     $0ad6                   ; map index
        asl5
        sta     $08
        lda     $0add,y
        and     #$1f
        ora     $08
        sta     $0add,y
        lda     #$02
        sta     $c0
        jsr     _c04583
        lda     $57
        bne     _088f
        ldx     #$0240
        jsr     ExecTriggerScript
_088f:  rts

.endproc

_c0088f := _c00853::_088f

; ---------------------------------------------------------------------------

_c00890:
_0890:  .byte $00,$10,$20,$40,$00,$00,$80

; board vehicle subroutines
BoardVehicleTbl:
        .addr 0
        .addr BoardChoco
        .addr BoardBlackChoco
        .addr BoardHiryuu
        .addr BoardSubmarine
        .addr BoardShip
        .addr BoardAirship

; land vehicle subroutines
LandVehicleTbl:
        .addr 0
        .addr LandChoco
        .addr LandBlackChoco
        .addr LandHiryuu
        .addr LandSubmarine
        .addr LandShip
        .addr LandAirship

_c008b3:
_08b3:  .byte $02,$04,$04,$04,$04,$04,$10

; ---------------------------------------------------------------------------

; [ get on chocobo ]

BoardChoco:
_08ba:  rts

; ---------------------------------------------------------------------------

; [ get off chocobo ]

LandChoco:
_08bb:  stz     $0adc
        jmp     _c00853

; ---------------------------------------------------------------------------

; [ get on black chocobo ]

BoardBlackChoco:
_08c1:  lda     #$00
        sta     $3d
_08c5:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y     ; vehicle height
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01ec5
        jsr     _c01e64
        inc     $3d
        lda     $3d
        cmp     #$40
        bne     _08c5
        rts

; ---------------------------------------------------------------------------

; [ get on hiryuu ]

BoardHiryuu:
_08ef:  lda     #$00
        sta     $3d
_08f3:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01ec5
        jsr     _c01e64
        inc     $3d
        lda     $3d
        cmp     #$40
        bne     _08f3
        rts

; ---------------------------------------------------------------------------

; [ land black chocobo ]

LandBlackChoco:
_091d:  lda     #$03
        sta     $0adb
        lda     #$3f
        sta     $3d
_0926:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01ec5
        jsr     _c01e64
        dec     $3d
        lda     $3d
        cmp     #$ff
        bne     _0926
        stz     $0adc
        jmp     _c00853

; ---------------------------------------------------------------------------

; [ land hiryuu ]

LandHiryuu:
_0955:  lda     #$03
        sta     $0adb
        lda     #$3f
        sta     $3d
_095e:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01ec5
        jsr     _c01e64
        dec     $3d
        lda     $3d
        cmp     #$ff
        bne     _095e
        stz     $0adc
        jmp     _c00853

; ---------------------------------------------------------------------------

; [ get on submarine ]

BoardSubmarine:
_098d:  rts

; ---------------------------------------------------------------------------

; [ get off submarine ]

LandSubmarine:
_098e:  lda     #$03
        sta     $0adb
        ldx     $0ad6       ; map index
        cpx     #$0003
        bcc     _09b9       ; branch if not underwater
        lda     $10fa       ; world tile properties byte 1
        bpl     _09b8       ; return if submarine can't surface
        jsr     _c009c6
        ldx     #$002a
        jsr     ExecTriggerScript
        jsr     _c04a68
        lda     #$01
        sta     $bd
        lda     #$28
        sta     $169b
        jsr     _c022fb
_09b8:  rts
_09b9:  jsr     _c009f7
        ldx     #$0028
        jsr     ExecTriggerScript
        jsr     _c00a11
        rts

; ---------------------------------------------------------------------------

; [  ]

.proc _c009c6

_09c6:  lda     #$80
        sta     $3d
        stz     $0d
_09cc:  jsr     WaitVBlank
        lda     $0d
        clc
        adc     $3d
        sta     $0d
        bcc     _09e4
        dec     $0289
        dec     $028d
        dec     $0291
        dec     $0295
_09e4:  lda     $3d
        cmp     #$c0
        bne     _09f2
        lda     #$84
        sta     $43
        lda     #$f0
        sta     $45
_09f2:  inc     $3d
        bne     _09cc
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c009f7
_09f7:  lda     #$01
        sta     $5b
        jsr     _c04798
        lda     #$88
        sta     $43
        lda     #$f0
        sta     $45
        lda     #$20
        sta     $169b
        jsr     _c022fb
        stz     $5b
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c00a11

_0a11:  lda     #$08
        sta     $43
        stz     $45
        jsr     _c02137
        stz     $0289
        stz     $028d
        lda     #$08
        sta     $0291
        sta     $0295
        lda     #$fd
        sta     $3d
        stz     $0d
_0a2e:  jsr     WaitVBlank
        lda     $0d
        sec
        sbc     $3d
        sta     $0d
        bcs     _0a46
        inc     $0289
        inc     $028d
        inc     $0291
        inc     $0295
_0a46:  dec     $3d
        lda     $3d
        bmi     _0a2e
        rts

.endproc

; ---------------------------------------------------------------------------

; [ board ship ]

BoardShip:
_0a4d:  ldx     #$01fc
        jsr     ExecTriggerScript
        rts

; ---------------------------------------------------------------------------

; [ get off ship ]

LandShip:
_0a54:  stz     $58
        rts

; ---------------------------------------------------------------------------

; [ board airship ]

BoardAirship:
_0a57:  jsr     _c04798
        lda     #$00
        sta     $3d
_0a5e:  jsr     WaitVBlank
        jsr     _c05bf8
        jsr     _c04c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        sta     $6f
        asl
        ora     #$80
        sta     $0ade,y
        lda     $6f
        beq     _0a8e
        cmp     #$0c
        bcs     _0a8e
        lda     #$70
        sta     $1879
        lda     #$7b
        sta     $187a
        jsr     _c04741
_0a8e:  jsr     _c02137
        jsr     _c0612b
        jsr     _c01e64
        jsr     _c047f7
        inc     $3d
        lda     $3d
        cmp     #$40
        bne     _0a5e
        rts

; ---------------------------------------------------------------------------

; [ land airship ]

LandAirship:
_0aa3:  lda     #$03
        sta     $0adb
        lda     $0af1
        and     #$03
        beq     _0afb
        dec
        jeq     _0bd1
        dec
        bne     _0ac5
        jsr     _c009f7
        ldx     #$002c
        jsr     ExecTriggerScript
        jsr     _c00a11
        rts
_0ac5:  lda     $10fa       ; world tile properties byte 1
        bpl     _0afa       ; branch if submarine can't surface
        jsr     _c009c6
        ldx     #$002e
        jsr     ExecTriggerScript
        jsr     _c04a68
        lda     #$01
        sta     $bd
        lda     #$28
        sta     $169b
        jsr     _c022fb
        lda     #$18
        sta     $169b
        jsr     _c022fb
        lda     $0af1
        and     #$e0
        ora     #$15
        sta     $0af1
        jsr     _c04583
        jsr     WaitVBlank
_0afa:  rts
_0afb:  lda     $57
        bne     _0b5c
        lda     #$fa
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        beq     _0b5c
        jsr     _c00c9f
        lda     $1697
        bmi     _0b5b
        bne     _0b5c
        lda     #$80
        sta     $3d
        stz     $da
        stz     $db
_0b1a:  jsr     WaitVBlank
        jsr     _c02137
        lda     $da
        clc
        adc     $3d
        sta     $da
        bcc     _0b2b
        inc     $db
_0b2b:  lda     $0289
        sec
        sbc     $db
        sta     $0289
        sta     $028d
        lda     $0291
        sec
        sbc     $db
        sta     $0291
        sta     $0295
        lda     $3d
        cmp     #$c0
        bne     _0b51
        lda     #$84
        sta     $43
        lda     #$f0
        sta     $45
_0b51:  inc     $3d
        bne     _0b1a
        ldx     #$001e
        jsr     ExecTriggerScript
_0b5b:  rts
_0b5c:  lda     #$2f
        sta     $3d
_0b60:  jsr     WaitVBlank
        jsr     _c05bf8
        jsr     _c04c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f+16,x
        sta     $6f
        asl
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01e64
        jsr     _c05bf8
        dec     $3d
        lda     $3d
        cmp     #$ff
        bne     _0b60
        lda     $10fa       ; world tile properties byte 1
        and     #$10
        beq     _0bc8
        lda     #$01
        sta     $5b
        jsr     _c046c4
        stz     $169b
        jsr     _c022fb
        stz     $5b
        ldy     $169c
        lda     $0add,y     ; set vehicle to ship
        and     #$e0
        ora     #$15
        sta     $0add,y
        lda     #$80
        sta     $0ade,y
        lda     #$04
        sta     $c0
        jsr     _c02137
        jsr     WaitVBlank
        jsr     _c04583
        jsr     WaitVBlank
        rts
_0bc8:  stz     $0adc
        jsr     WaitVBlank
        jmp     _c00853
_0bd1:  lda     $0adc
        cmp     #$06
        bne     _0c1e
        lda     $57
        bne     _0bef
        lda     #$f9
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        beq     _0bef
        jsr     _c00c9f
        lda     $1697
        bmi     _0c1e
        bne     _0c1f
_0bef:  lda     #$08
        sta     $169b
        lda     #$80
        sta     $0ade,y
        ldy     $169c
        lda     $0add,y     ; set vehicle to submarine
        and     #$e0
        ora     #$18
        sta     $0add,y
        lda     #$03
        sta     $0adb
        lda     #$10
        sta     $c0
        jsr     _c04583
        jsr     _c022fb
        lda     #$01
        sta     $5b
        jsr     BoardAirship
        stz     $5b
_0c1e:  rts
_0c1f:  lda     #$10
        sta     $169b
        lda     #$80
        sta     $0ade,y
        jsr     _c022fb
        ldy     $169c
        lda     $0add,y
        and     #$e0
        ora     #$10
        sta     $0add,y
        lda     #$03
        sta     $0adb
        lda     #$02
        sta     $c0
        jsr     _c009f7
        ldx     #$002c
        jsr     ExecTriggerScript
        jsr     _c00a11
        rts

; ---------------------------------------------------------------------------

_c00c4f:
        .byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte   0,1,1,1,1,2,2,2,2,3,3,3,4,4,4,5
        .byte   5,5,6,6,6,7,7,8,8,8,9,9,10,10,10,11
        .byte   11,11,12,12,12,13,13,13,14,14,14,14,15,15,15,15
        .byte   15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15

; ---------------------------------------------------------------------------

; [  ]

.proc _c00c9f

_0c9f:  stz     $1697
_0ca2:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        jsr     _c02137
        jsr     _c0612b
        jsr     _c01e64
        lda     $03
        and     #>JOY_B
        beq     _0cbd
        lda     #$80
        sta     $1697
        rts
_0cbd:  lda     $02
        and     #JOY_A
        beq     _0ccd
        lda     $10b8
        bne     _0ccd
        inc     $10b8
        bra     _0d3c
_0ccd:  lda     $03
        and     #>(JOY_UP | JOY_DOWN)
        beq     _0ce1
        cmp     #$08
        bne     _0cdc
        stz     $1697
        bra     _0ce1
_0cdc:  lda     #$01
        sta     $1697
_0ce1:  lda     $1697
        bne     _0cec
        lda     $3f
        and     #$08
        bne     _0d0d
_0cec:  ldy     $169c
        lda     $0ade,y
        and     #$7f
        sta     $08
        lda     #$64
        sec
        sbc     $08
        sta     $0201
        lda     #$74
        sta     $0200
        lda     #$48
        sta     $0202
        lda     #$36
        sta     $0203
_0d0d:  lda     $1697
        beq     _0d18
        lda     $3f
        and     #$08
        bne     _0d39
_0d18:  ldy     $169c
        lda     $0ade,y
        and     #$7f
        sta     $08
        lda     #$7e
        sec
        sbc     $08
        sta     $0205
        lda     #$74
        sta     $0204
        lda     #$48
        sta     $0206
        lda     #$b6
        sta     $0207
_0d39:  jmp     _0ca2
_0d3c:  rts

.endproc

; ---------------------------------------------------------------------------

.proc _c00d3d

_0d3d:  lda     $61
        and     #$1f
        bne     _0d73
        lda     $63
        and     #$1f
        bne     _0d73
        jsr     _c017e8       ; update local tile properties (normal map)
        lda     #$f1
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        bne     _0d63
        lda     $02
        and     #JOY_A
        beq     _0d73       ; branch if the a button is not down
        lda     $10b8
        bne     _0d73       ;
        inc     $10b8
_0d63:  jsr     CheckNPCEvents
        lda     $58
        bne     _0d73       ; branch if an event is running
        lda     $02
        and     #JOY_A
        beq     _0d73       ; branch if the a button is not down
        jsr     CheckTreasure
_0d73:  rts

.endproc

; ---------------------------------------------------------------------------

; [ check treasure chests ]

.proc CheckTreasure

_0d74:  longa
        lda     $0ad4
        tax
        lda     f:MapTreasures,x        ; first treasure on this map
        and     #$00ff
        asl2
        sta     $23
        lda     f:MapTreasures+1,x               ; first treasure on next map
        and     #$00ff
        asl2
        sta     $26
        lda     $06
        shorta
        lda     $0adb
        tax
        lda     $0ad8
        clc
        adc     f:_c00f84,x
        and     #$3f
        sta     $75
        lda     $0ad9
        clc
        adc     f:_c00f88,x
        and     #$3f
        sta     $76
_0db0:  ldx     $23
        cpx     $26
        beq     _0dd2
        bcs     _0dd2
        lda     f:TreasureProp,x               ; check x position
        cmp     $75
        bne     _0dca
        lda     f:TreasureProp+1,x               ; check y position
        cmp     $76
        bne     _0dca
        bra     _0dd3
_0dca:  inx4
        stx     $23
        bra     _0db0
_0dd2:  rts
_0dd3:  longa
        txa
        lsr2
        shorta
        xba
        lda     #$00
        xba
        sta     $16a1
        jsr     _c0ca16                   ; get treasure flag
        cmp     #$00
        bne     _0dd2
        lda     #$01
        sta     $58
        stz     $ba
        ldx     $23
        phx
        lda     f:TreasureProp+1,x
        longa
        asl6
        sta     $0d
        lda     f:TreasureProp,x
        and     #$00ff
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        lda     $7f0000,x
        cmp     #$02
        bne     _0e2e
        lda     #$8e
        jsr     PlaySfx
        ldx     $06
        stx     $73
        ldx     #$0101
        stx     $2c
        lda     #$12
        sta     $16b3
        jsr     _c06f08
        bra     _0e33
_0e2e:  lda     #$68
        jsr     PlaySfx
_0e33:  plx
        lda     f:TreasureProp+2,x
        sta     $11
        lda     f:TreasureProp+3,x
        sta     $12
        lda     $11
        jmi     _0ebc
        and     #$e0                    ; gp
        bne     _0e63
        jsr     CalcGil
        jsr     GiveGil
        jsr     _c04dd7
        ldx     #$0003
        stx     $af
        jsr     ShowDlg
        lda     $16a1
        jsr     _c0ca21
        rts
_0e63:  cmp     #$20                    ; spell
        bne     _0e7e
_0e67:  lda     $12
        sta     $16a3
        jsr     _c0c9a5                   ; give magic
        ldx     #$0004
        stx     $af
        jsr     ShowDlg
        lda     $16a1
        jsr     _c0ca21
        rts
_0e7e:  lda     $12                     ; item
        jsr     _c0bfdd
        cpy     #$0100
        beq     _0e98
        lda     $0740,y
        cmp     #$63
        beq     _0ead
        lda     $0740,y
        inc
        sta     $0740,y
        bra     _0ead
_0e98:  ldy     $06
_0e9a:  lda     $0640,y
        beq     _0ea2
        iny
        bra     _0e9a
_0ea2:  lda     $16a2
        sta     $0640,y
        lda     #$01
        sta     $0740,y
_0ead:  ldx     #$0002
        stx     $af
        jsr     ShowDlg
        lda     $16a1
        jsr     _c0ca21
        rts
_0ebc:  ldx     #$0005                  ; monster-in-a-box
        stx     $af
        jsr     ShowDlg
        lda     #$ff
        sta     $0be0
        lda     $11
        and     #$3f
        ora     #$40
        ldx     $11
        phx
        jsr     _c0bde6                   ; event battle
        lda     $09c4
        and     #$01
        beq     _0ee4                   ; branch if party was not defeated
        lda     #$f0                    ; don't reset spc
        sta     $1d00
        jmp     Start                   ; reset
_0ee4:  jsr     ReloadMap
        plx
        stx     $11
        lda     $09c4
        and     #$02
        bne     _0efd                   ; branch if party ran away
        lda     $11
        and     #$40
        jeq     _0e7e                   ; item
        jmp     _0e67                   ; spell
_0efd:  rts

.endproc

; ---------------------------------------------------------------------------

; [ give gil amount ]

; ++$37: gil

.proc GiveGil

_0efe:  lda     $0947                   ; current gp
        clc
        adc     $37
        sta     $0947
        lda     $0948
        adc     $38
        sta     $0948
        lda     $0949
        adc     $39
        sta     $0949
        cmp     #$98                    ; max 9999999
        beq     _0f1f
        bcc     _0f3c
        bcs     _0f31
_0f1f:  lda     $0948
        cmp     #$96
        beq     _0f2a
        bcc     _0f3c
        bcs     _0f31
_0f2a:  lda     $0947
        cmp     #$7f
        bcc     _0f3c
_0f31:  ldx     #$9896
        stx     $0948
        lda     #$7f
        sta     $0947
_0f3c:  rts

.endproc

; ---------------------------------------------------------------------------

; [ calculate gil amount (chest) ]

;   $11: multiplier (1, 10, 100, 1000, 10000, 34464, 256, 65280)
;   $12: value
; ++$37: value (out)

.proc CalcGil

_0f3d:  stz     $39
        lda     $11
        and     #$07
        asl
        tax
        lda     $12
        sta     hWRMPYA
        lda     f:_c00f78,x
        sta     hWRMPYB
        nop4
        ldy     hRDMPYL
        sty     $37
        lda     $12
        sta     hWRMPYA
        lda     f:_c00f78+1,x
        sta     hWRMPYB
        nop3
        longa
        lda     hRDMPYL
        clc
        adc     $38
        sta     $38
        lda     $06
        shorta
        rts

.endproc

_c00f78:
@0f78:  .addr   1,10,100,1000,10000,100000

_c00f84:
@0f84:  .word   $0100, $ff00

_c00f88:
@0f88:  .word   $00ff, $0001

; ---------------------------------------------------------------------------

; [  ]

.proc _c00f8c

_0f8c:  lda     $ba
        bne     _0f92
        stz     $41
_0f92:  lda     $61
        and     #$1f
        bne     _0f9e
        lda     $63
        and     #$1f
        beq     _0f9f
_0f9e:  rts
_0f9f:  lda     $57
        beq     _0fa7
        ldx     $3b
        stx     $02
_0fa7:  jsr     _c0104a
        lda     $03
        and     #$0f
        jeq     _1003
        lda     $03
        and     #>JOY_UP
        beq     _0fc7
        lda     #$01
        sta     $c4
        jsr     _c01055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fc7:  lda     $03
        and     #>JOY_RIGHT
        beq     _0fdb
        lda     #$02
        sta     $c4
        jsr     _c01055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fdb:  lda     $03
        and     #>JOY_DOWN
        beq     _0fef
        lda     #$03
        sta     $c4
        jsr     _c01055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fef:  lda     $03
        and     #>JOY_LEFT
        beq     _1006
        lda     #$04
        sta     $c4
        jsr     _c01055
        lda     $c4
        jne     _1006
_1003:  stz     $ba
        rts
_1006:  sta     $ba
        lda     $0ad9
        sta     $76
        jsr     _c069a1
        lda     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        jsr     _c06513
        inc     $9f
        stz     $59
        stz     $16aa
        lda     $0adc
        bne     _102c
        jsr     DoPoisonDmg
_102c:  jsr     _c0103a
        lda     #$fc
        jsr     _c0c796
        lda     #$fd
        jsr     _c0c796
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c0103a

_103a:  lda     $ca
        beq     _1049
        lda     $ba         ; movement direction
        tax
        lda     f:_11b8,x   ; pointer to tile in  movement direction
        tax
        jsr     _c0119a       ; get world tile transparent/underwater flags
_1049:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c0104a

_104a:  lda     $ca
        bne     _1054
        ldx     #$0008      ; tile at party location
        jsr     _c0119a       ; get world tile transparent/underwater flags
_1054:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c01055

_1055:  lda     $c4         ; movement direction
        dec
        sta     $0adb       ; party facing direction
        lda     $57
        bne     _10c3       ; return if an event is running
        jsr     _c0112d       ; check if there is a vehicle at the current position
        cmp     #$01
        beq     _10c3       ; return if there is a vehicle present
        lda     $c4
        tax
        lda     $0adc       ; current vehicle
        beq     _1087
        dec
        asl2
        tay
        lda     $0add,y     ; vehicle index
        and     #$1c
        lsr2
        cmp     #$01
        beq     _1087       ; branch if chocobo
        cmp     #$04
        beq     _1092       ; branch if submarine
        cmp     #$05
        beq     _1092       ; branch if ship
        bra     _109d
_1087:  lda     $10fb       ; world tile properties byte 2
        and     f:_11bd,x
        beq     _10c4       ; branch if not passable in that direction
        bra     _109d
_1092:  lda     $10fb       ; world tile properties byte 2
        eor     #$0f
        and     f:_11bd,x
        beq     _10c4       ; branch if ship/sub can't land there
_109d:  lda     $0adc
        beq     _10ad
        dec
        asl2
        tay
        lda     $0add,y     ; vehicle index
        and     #$1c
        lsr2
_10ad:  tax
        lda     f:_11b0,x   ; bit mask
        sta     $0d
        lda     $c4
        tax
        lda     f:_11b8,x   ; pointer to tile properties in facing direction
        tay
        lda     $10f2,y     ; world tile properties byte 1
        and     $0d
        beq     _10c4       ; branch if not passable
_10c3:  rts
_10c4:  jsr     _c010c8
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c010c8

_10c8:  ldx     $0ad6       ; map index
        cpx     #$0003
        bcs     _112a       ; branch if not a world map
        lda     $0adc
        beq     _112a       ; branch if not in a vehicle
        dec
        asl2
        tay
        lda     $0add,y
        and     #$1c
        lsr2
        cmp     #$04
        beq     _10e8
        cmp     #$05
        bne     _112a
_10e8:  lda     $c4
        tax
        lda     f:_11b8,x
        tax
        lda     $10f2,x     ; world tile properties byte 1
        and     #$01
        beq     _112a       ; branch if not passable on foot
        stz     $0adc
        lda     $0ad6       ; map index
        asl5
        sta     $08
        lda     $0add,y
        and     #$1f
        ora     $08
        sta     $0add,y
        lda     #$02
        sta     $c0
        lda     #$00
        sta     $0ade,y
        lda     $0ad8
        sta     $0adf,y
        lda     $0ad9
        sta     $0ae0,y
        jsr     _c04583
        jsr     WaitVBlank
        rts
_112a:  stz     $c4
        rts

.endproc

; ---------------------------------------------------------------------------

; [ check if there is a vehicle at the current position ]

; return 0 if not, 1 if there is a vehicle
; $23 is a pointer to vehicle data

.proc _c0112d

_112d:  lda     $0adc
        bne     _118f
        lda     $c4
        dec
        tax
        lda     $0ad8
        clc
        adc     f:_1192,x
        sta     $75
        lda     $0ad9
        clc
        adc     f:_1196,x
        sta     $76
        ldy     $06
        sty     $23         ; pointer to vehicle data (+$0add)
_114e:  ldy     $23
        lda     $0ade,y
        bmi     _1184       ; branch if vehicle is hidden
        lda     $0add,y
        and     #$1c
        lsr2
        cmp     #$04
        beq     _1164
        cmp     #$05
        bne     _1184
_1164:  lda     $0add,y     ; vehicle map
        and     #$e0
        lsr5
        cmp     $0ad6       ; map index
        bne     _1184       ; branch if vehicle is not on this map
        lda     $0adf,y
        cmp     $75
        bne     _1184       ; branch if xy position doesn't match
        lda     $0ae0,y
        cmp     $76
        bne     _1184
        lda     #$01
        rts
_1184:  lda     $23         ; next vehicle
        clc
        adc     #$04
        sta     $23
        cmp     #$18
        bne     _114e
_118f:  lda     #$00
        rts

_1192:  .byte   $00,$01,$00,$ff
_1196:  .byte   $ff,$00,$01,$00

.endproc

; ---------------------------------------------------------------------------

; [ get world tile transparent/underwater flags ]

.proc _c0119a

_119a:  lda     $10be,x     ; get tile index * 3
        longa
        asl
        clc
        adc     $10be,x
        tax
        lda     $1188,x     ; tile properties byte 3
        and     #$0060
        shorta
        sta     $ca
        rts

.endproc

; ---------------------------------------------------------------------------

_11b0:  .byte   $01,$02,$04,$08,$10,$20,$40,$80
_11b8:  .byte   $00,$02,$0a,$0e,$06
_11bd:  .byte   $00,$08,$01,$04,$02

; ---------------------------------------------------------------------------

; [  ]

.proc _c011c2

_11c2:  lda     $ba
        bne     _11c8       ; branch if party is moving
        stz     $41
_11c8:  lda     $61         ; bg1 x position
        and     #$1f
        bne     _11d4       ; return if partway between tiles
        lda     $63         ; bg1 y position
        and     #$1f
        beq     _11d5
_11d4:  rts
_11d5:  lda     $57
        beq     _11df       ; branch if an event is running
        ldx     $3b         ; override pressed buttons with ???
        stx     $02
        bra     _11fc
_11df:  jsr     _c0151d       ; update party sprite priority (current tile)
        lda     $10fb       ; map tile properties byte 2
        and     #$40
        beq     _11f6       ; branch if not forced facing direction
        lda     $10fb       ; map tile properties byte 2
        and     #$0f
        tax
        lda     f:_c0172a,x   ; forced facing direction
        dec
        bra     _11f9
_11f6:  lda     $bf
        dec
_11f9:  sta     $0adb       ; party facing direction
_11fc:  lda     $10fa       ; map tile properties byte 1
        and     #$04
        bne     _1212       ; branch if a bridge tile
        lda     $be
        bne     _1212       ; branch if jumping
        lda     $10fa       ; map tile properties byte 1
        and     #$03
        cmp     #$03
        beq     _1212       ; branch if stairs
        sta     $c3         ; set z-level
_1212:  lda     $10fa       ; map tile properties byte 1
        and     #$20
        beq     _122c       ; branch if no tile damage
        lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     _122a
        lda     $c3         ; z-level
        and     #$01
        beq     _122a       ; branch if not passable on upper z-level
        lda     #$00        ; use downstairs tile damage properties
        bra     _122c
_122a:  lda     #$01        ; use upstairs tile damage properties
_122c:  sta     $c9
        jsr     _c013a6       ; update party z-level (current tile)
        jsr     _c016b7
        lda     $10fb       ; map tile properties byte 2
        and     #$30
        lsr4
        tax
        lda     f:_c0171c,x   ; tile movement speed
        sta     $c0
        lda     $0afa       ; walking speed
        beq     _124f
        bpl     _1278
        lsr     $c0         ; half speed
        bra     _127a
_124f:  lda     $03
        and     #>JOY_B
        beq     _127a       ; branch if b button is not pressed
        ldx     $06
_1257:  lda     $0500,x     ; branch if character not present
        and     #$40
        bne     _1265
        lda     $0520,x     ; check for dash ability
        and     #$08
        bne     _1278
_1265:  longa               ; next character
        txa
        clc
        adc     #$0050
        tax
        lda     $06
        shorta
        cpx     #$0140
        bne     _1257
        bra     _127a
_1278:  asl     $c0         ; double speed
_127a:  lda     $1120
        jsr     CalcParallaxScroll
        sta     $1080
        stz     $1081
        lda     $1120
        lsr2
        jsr     CalcParallaxScroll
        sta     $1082
        stz     $1083
        lda     $1120
        lsr4
        jsr     CalcParallaxScroll
        sta     $1084
        stz     $1085
        lda     $1120
        lsr6
        jsr     CalcParallaxScroll
        sta     $1086
        stz     $1087
        lda     $10fb       ; map tile properties byte 2
        bpl     _12c5       ; branch if not an auto-move tile
        and     #$0f
        tax
        lda     f:_c0172a,x
        jmp     _1336
_12c5:  lda     $57
        bne     _12d8       ; branch if an event is running
        lda     $5d
        beq     _12d8       ; branch if party is already moving
        stz     $5d
        stz     $16aa
        jsr     DoTileDmg
        jsr     DoPoisonDmg
_12d8:  lda     $03
        and     #$0f
        jeq     _1331       ; no move
        lda     $03         ; check up button
        and     #>JOY_UP
        beq     _12f5
        lda     #$01        ; up
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_12f5:  lda     $03         ; check right button
        and     #>JOY_RIGHT
        beq     _1309
        lda     #$02        ; right
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_1309:  lda     $03         ; check down button
        and     #>JOY_DOWN
        beq     _131d
        lda     #$03        ; down
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_131d:  lda     $03         ; check left button
        and     #>JOY_LEFT
        beq     _1336
        lda     #$04        ; left
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        jne     _1336
_1331:  stz     $ba         ; no move
        jmp     _1367

; start move
_1336:  sta     $ba         ; set facing/moving direction
        sta     $bf
        lda     #$01
        sta     $5d
        stz     $59
        jsr     _c01372       ; update party z-level (destination tile)
        lda     $57
        bne     _134e       ; branch if an event is running
        lda     $10fb       ; map tile properties byte 2
        and     #$40
        bne     _1354       ; branch if forced facing direction
_134e:  lda     $ba
        dec
        sta     $0adb       ; set facing direction
_1354:  jsr     _c01538       ; update party sprite priority (destination tile)
        lda     #$fc
        jsr     _c0c796       ;
        lda     #$fd
        jsr     _c0c796       ;
        jsr     _c0155b
        jsr     _c015ac
_1367:  jsr     _c02973
        lda     $58
        bne     _1371
        jsr     _c016da
_1371:  rts

.endproc

; ---------------------------------------------------------------------------

; [ update party z-level (destination tile) ]

.proc _c01372

_1372:  lda     $ba         ; movement direction
        tax
        lda     f:_c01720,x   ; pointer to tile properties
        tax
        lda     $ca
        bne     _1385
        lda     $10f2,x     ; map tile properties byte 1
        and     #$0c
        sta     $ca
_1385:  lda     $cb         ; return if party sprite is not hidden
        beq     _13a5
        lda     $10f2,x     ; map tile properties byte 1
        and     #$04
        beq     _139a       ; branch if not a bridge tile
        lda     $c3         ; party z-level
        and     #$01
        beq     _139a       ; branch if not on upper z-level
        stz     $cb         ; show party sprite
        bra     _13a5
_139a:  lda     $10f2,x     ; map tile properties byte 1
        and     #$10
        lsr4
        sta     $cb         ; show/hide party sprite
_13a5:  rts

.endproc

; ---------------------------------------------------------------------------

; [ update party z-level (current tile) ]

.proc _c013a6

_13a6:  lda     $10fa       ; map tile properties byte 1
        and     #$03
        ora     #$04
        sta     $c2         ; set passable z-levels
        lda     $ca
        beq     _13ba       ;
        lda     $10fa       ; map tile properties byte 1
        and     #$0c
        sta     $ca
_13ba:  lda     $cb
        bne     _13d6       ; return if party sprite is already hidden
        lda     $10fa       ; map tile properties byte 1
        and     #$10
        beq     _13d6       ; return if tile doesn't hide party sprite
        lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     _13d2       ; hide party sprite if not a bridge tile
        lda     $c3
        and     #$01
        bne     _13d6       ; return if on upper z-level
_13d2:  lda     #$01        ; hide party sprite
        sta     $cb
_13d6:  rts

.endproc

; ---------------------------------------------------------------------------

; [ tile damage ]

.proc DoTileDmg

_13d7:  ldx     $06
_13d9:  lda     $0500,x
        and     #$40
        bne     _13ea       ; skip empty character slots
        lda     $0520,x
        and     #$04
        jne     _1474       ; no damage if any character has floor damage ability
_13ea:  longa
        txa                 ; next character
        clc
        adc     #$0050
        tax
        lda     $06
        shorta
        cpx     #$0140
        bne     _13d9
        ldy     $06
_13fd:  lda     $0500,y
        and     #$40
        bne     _1463       ; skip empty character slots
        lda     $051a,y
        and     #$c8
        bne     _1463       ; skip dead/petrified/float characters
        lda     $c9
        tax
        lda     $1112,x
        and     #$40
        beq     _141d
        lda     $051a,y     ; inflict poison
        ora     #$04
        sta     $051a,y
_141d:  lda     $c9
        tax
        lda     $1112,x     ; tile damage
        and     #$07
        asl
        tax
        beq     _144e
        longa
        lda     $0506,y
        beq     _143c
        sec
        sbc     f:TileDmgTbl,x   ; subtract hp
        beq     _1439
        bcs     _143c
_1439:  lda     #$0001
_143c:  sta     $0506,y
        lda     $06
        shorta
        lda     #$8f
        sta     $16aa
        ldx     #$1e00
        stx     $16ab
_144e:  lda     $c9
        tax
        lda     $1112,x     ; tile damage sound effect
        lsr3
        and     #$07
        beq     _1463
        tax
        lda     f:TileDmgSfxTbl-1,x
        jsr     PlaySfx
_1463:  longa
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        cpy     #$0140
        bne     _13fd
_1474:  rts

.endproc

; ---------------------------------------------------------------------------

; tile damage sound effect
TileDmgSfxTbl:
@1475:  .byte   $94, $94, $94, $94, $94, $94, $94, $94

; tile damage values (0, 50, 50, 100, 300, 400, 500, 1000)
TileDmgTbl:
@147d:  .word   0, 50, 50, 100, 300, 400, 500, 1000

; ---------------------------------------------------------------------------

; [ poison damage ]

.proc DoPoisonDmg

_148d:  lda     $57
        bne     _14e8       ; return if an event is running
        ldy     $06
_1493:  lda     $0500,y
        and     #$40
        bne     _14d7       ; branch if character slot is empty
        lda     $051a,y
        and     #$c0
        bne     _14d7       ; skip dead/petrified characters
        lda     $051a,y
        and     #$04
        beq     _14d7       ; skip characters that are not poisoned
        lda     #$8f
        sta     $16aa       ; pixelate screen
        longa
        lda     #$1e00
        sta     $16ab
        lda     $0508,y
        lsr6
        inc
        sta     $0d
        lda     $0506,y
        beq     _14d0
        sec
        sbc     $0d
        beq     _14cd
        bcs     _14d0
_14cd:  lda     #$0001
_14d0:  sta     $0506,y
        lda     $06
        shorta
_14d7:  longa
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        cpy     #$0140
        bne     _1493
_14e8:  rts

.endproc

; ---------------------------------------------------------------------------

; [ pixelate screen (poison) ]

.proc PoisonMosaic

_14e9:  ldy     $06
_14eb:  lda     $0500,y
        and     #$40
        bne     _150b
        lda     $051a,y
        and     #$c0
        bne     _150b
        lda     $051a,y
        and     #$04
        beq     _150b
        lda     #$8f        ;
        sta     $16aa
        ldx     #$1e00      ;
        stx     $16ab
_150b:  longa
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        cpy     #$0140
        bne     _14eb
        rts

.endproc

; ---------------------------------------------------------------------------

; [ update party sprite priority (current tile) ]

.proc _c0151d

_151d:  ldx     $c7         ; party sprite priority
        cpx     #$01e8
        bne     _1537       ; return if not currently low priority
        lda     $10fa       ; map tile properties byte 1
        and     #$40
        bne     _1537       ; return if a low priority tile
        lda     $10fa       ; map tile properties byte 1
        and     #$04
        bne     _1537       ; return if a bridge tile
        ldx     #$0110      ; change to normal priority
        stx     $c7
_1537:  rts

.endproc

; ---------------------------------------------------------------------------

; [ update party sprite priority (destination tile) ]

.proc _c01538

_1538:  lda     $ba         ; party moving direction
        tax
        lda     f:_c01720,x   ; pointer to tile properties
        tax
        lda     $10f2,x     ; map tile properties byte 1
        and     #$40
        bne     _1555       ; branch if a low priority tile
        ldy     $c7
        cpy     #$0110
        bne     _155a       ; return if currently normal priority
        lda     $10f2,x     ; map tile properties byte 1
        and     #$04
        beq     _155a       ; return if not a bridge tile
_1555:  ldx     #$01e8      ; change to low priority
        stx     $c7
_155a:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c0155b

_155b:  lda     $0adb
        lsr
        bcs     _15ab
        lsr
        bcs     _1575
        lda     $10c0
        cmp     #$13
        bne     _15ab
        lda     $0ad9
        sec
        sbc     #$02
        sta     $76
        bra     _1581
_1575:  lda     $10cc
        cmp     #$13
        bne     _15ab
        lda     $0ad9
        sta     $76
_1581:  ldx     $06
        stx     $73
        lda     $0ad8
        sta     $75
        ldx     #$0201
        stx     $2c
        lda     #$04
        sta     $16b3
        lda     #$14
        sta     $16b4
        jsr     _c06f08
        lda     #$8e
        jsr     PlaySfx
        lda     #$01
        sta     $58
        lda     $57
        bne     _15ab
        stz     $ba
_15ab:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c015ac

_15ac:  ldx     $06
_15ae:  lda     $0500,x
        and     #$40
        bne     _15bc
        lda     $0520,x
        and     #$02
        bne     _15cf
_15bc:  longa
        txa
        clc
        adc     #$0050
        tax
        lda     $06
        shorta
        cpx     #$0140
        bne     _15ae
        bra     _1612
_15cf:  lda     $0adb
        tax
        lda     f:_c0161b,x
        tay
        lda     $10be,y
        cmp     #$05
        bne     _1612
        lda     $0ad8
        clc
        adc     f:_c01617,x
        sta     $75
        lda     $0ad9
        clc
        adc     f:_c01613,x
        sta     $76
        ldx     $06
        stx     $73
        ldx     #$0101
        stx     $2c
        lda     #$15
        sta     $16b3
        jsr     _c06f08
        lda     #$01
        sta     $57
        ldx     #$000b      ; event $0b
        stx     $ce
        jsr     ExecEvent
        stz     $57
_1612:  rts

.endproc

; ---------------------------------------------------------------------------

_c01613:
@1613:  .byte   $ff,$00,$01,$00

_c01617:
@1617:  .byte   $00,$01,$00,$ff

_c0161b:
@161b:  .byte   $02,$0a,$0e,$06


; ---------------------------------------------------------------------------

; [ calculate parallax scroll ]

; 0 = return $c0 (1x)
; 1 = return $c0 >> 1 (0.5x)
; 2 = return $c0 << 1 (2x)
; 3 = return zero (no scroll)

.proc CalcParallaxScroll

_161f:  and     #%11
        bne     _1627
        lda     $c0
        bra     _1639
_1627:  dec
        bne     _162f
        lda     $c0
        lsr
        bra     _1639
_162f:  dec
        bne     _1637
        lda     $c0
        asl
        bra     _1639
_1637:  lda     $06
_1639:  rts

.endproc

; ---------------------------------------------------------------------------

; [ check if party can move ]

; $c4: direction (none, up, right, down, left)

.proc _c0163a

_163a:  lda     $57
        beq     _163f       ; return if an event is running
        rts
_163f:  lda     $10fb       ; map tile properties byte 2
        and     #$40
        bne     _164e       ; branch if forced facing direction
        lda     $c4
        sta     $bf
        dec
        sta     $0adb
_164e:  lda     $c4         ; moving direction
        tax
        lda     $10fb       ; map tile properties byte 2
        and     #$40
        bne     _1661       ; branch if forced facing direction
        lda     $10fb
        and     f:_c01725,x   ; facing direction mask
        beq     _16a9
_1661:  lda     $c4
        tax
        lda     f:_c01720,x   ; pointer to tile properties
        tax
        lda     $10d8,x     ; object at destination tile
        beq     _1692       ; branch if no object at destination
        lda     $10f2,x     ; map tile properties byte 1
        and     #$04
        beq     _167d       ; branch if not a bridge
        lda     $c3
        cmp     #$01
        bne     _1692
        bra     _168a
_167d:  lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     _168a
        lda     $c3
        cmp     #$02
        beq     _1692
_168a:  lda     $10d8,x
        sta     $e4
        jmp     _16a9
_1692:  lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     _16ac
        lda     $10f2,x     ; map tile properties byte 1
        and     #$04
        bne     _16b6
        lda     $10f2,x     ; map tile properties byte 1
        and     #$03
        and     $c3
        bne     _16b6
_16a9:  stz     $c4
        rts
_16ac:  lda     $10f2,x     ; map tile properties byte 1
        and     $c2
        bne     _16b6
        stz     $c4
        rts
_16b6:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c016b7

_16b7:  lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     _16c4
        lda     $c3
        cmp     #$01
        bne     _16d9
_16c4:  lda     $0ad8
        sta     $75
        lda     $0ad9
        sta     $76
        phx
        jsr     _c03cbb       ; get pointer to object layout
        lda     #$00
        sta     $7f3000,x
        plx
_16d9:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c016da

_16da:  lda     $ba
        tax
        lda     f:_c01720,x   ; pointer to tile properties
        tax
        lda     $10f2,x     ; map tile properties byte 1
        and     #$04
        beq     _16ef
        lda     $c3
        cmp     #$01
        bne     _1711
_16ef:  lda     $ba
        tax
        lda     $0ad8
        clc
        adc     f:_c01712,x
        sta     $75
        lda     $0ad9
        clc
        adc     f:_c01717,x
        sta     $76
        phx
        jsr     _c03cbb       ; get pointer to object layout
        lda     #$ff
        sta     $7f3000,x
        plx
_1711:  rts

.endproc

; ---------------------------------------------------------------------------

; none, up, right, down, left
_c01712:
@1712:  .byte $00,$00,$01,$00,$ff ; delta x

_c01717:
@1717:  .byte $00,$ff,$00,$01,$00 ; delta y

_c0171c:
@171c:  .byte $02,$01,$04,$08; tile movement speeds

_c01720:
@1720:  .byte $08,$02,$0a,$0e,$06 ; pointer to tile properties

_c01725:
@1725:  .byte $00,$08,$01,$04,$02

_c0172a:
@172a:  .byte $00,$02,$04,$00,$03,$00,$00,$00,$01


; ---------------------------------------------------------------------------

; [ update local tiles (world map) ]

.proc _c01733

_1733:  lda     $0ad8       ; x position
        sta     $0d
        stz     $0e
        lda     $0ad9       ; y position
        and     #$3f
        longa
        xba
        clc
        adc     $0d
        tay
        tya
        sec
        sbc     #$0100      ; tile above
        and     #$3fff
        tax
        lda     $7f0000,x   ; bg1 layout
        and     #$00ff
        sta     $10c0
        asl
        clc
        adc     $10c0
        tax
        lda     $1186,x     ; tile properties
        sta     $10f4
        tya
        and     #$3f00
        sta     $0d
        tya
        dec                 ; tile to the left
        and     #$00ff
        ora     $0d
        tax
        lda     $7f0000,x
        and     #$00ff
        sta     $10c4
        asl
        clc
        adc     $10c4
        tax
        lda     $1186,x
        sta     $10f8
        tyx
        lda     $7f0000,x   ; current tile
        and     #$00ff
        sta     $10c6
        asl
        clc
        adc     $10c6
        tax
        lda     $1186,x
        sta     $10fa       ; map tile properties byte 1
        tya
        and     #$3f00
        sta     $0d
        tya
        inc                 ; tile to the right
        and     #$00ff
        ora     $0d
        tax
        lda     $7f0000,x
        and     #$00ff
        sta     _c010c8
        asl
        clc
        adc     _c010c8
        tax
        lda     $1186,x
        sta     $10fc
        tya
        clc
        adc     #$0100      ; tile below
        and     #$3fff
        tax
        lda     $7f0000,x
        and     #$00ff
        sta     $10cc
        asl
        clc
        adc     $10cc
        tax
        lda     $1186,x
        sta     $1100
        lda     $06
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

; [ update local tiles (normal map) ]

.proc _c017e8

_17e8:  lda     $0ad8       ; x position
        and     #$3f
        sta     $0d
        stz     $0e
        lda     $0ad9       ; y position
        and     #$3f
        longa
        xba
        lsr
        lsr
        ora     $0d
        tay
        tya
        sec
        sbc     #$0080
        and     #$0fff
        sta     $0d
        tax
        lda     $7f3000,x
        sta     $10ea
        lda     $7f0000,x
        sta     $10d0
        and     #$00ff
        asl
        tax
        lda     $1186,x     ; tile properties
        sta     $1104
        tya
        sec
        sbc     #$0040
        and     #$0fc0
        sta     $0d
        tya
        dec
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10d8
        lda     $7f0000,x
        sta     $10be
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10f2       ; map tile properties byte 1
        tya
        sec
        sbc     #$0040
        and     #$0fff
        tax
        lda     $7f3000,x
        sta     $10da
        lda     $7f0000,x
        sta     $10c0
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10f4
        tya
        sec
        sbc     #$0040
        and     #$0fc0
        sta     $0d
        tya
        inc
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10dc
        lda     $7f0000,x
        sta     $10c2
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10f6
        tya
        and     #$0fc0
        sta     $0d
        tya
        dec
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10de
        lda     $7f0000,x
        sta     $10c4
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10f8
        tya
        and     #$0fc0
        sta     $0d
        tya
        dec
        dec
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10ec
        lda     $7f0000,x
        sta     $10d2
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10fc
        tyx                 ; current tile
        lda     $7f3000,x
        sta     $10e0
        lda     $7f0000,x
        sta     $10c6
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10fa       ; map tile properties byte 1
        tya
        and     #$0fc0
        sta     $0d
        tya
        inc                 ; to the right
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10e2
        lda     $7f0000,x
        sta     _c010c8
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10fc
        tya
        and     #$0fc0
        sta     $0d
        tya
        inc                 ; two tiles to the right
        inc
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x   ; object layout
        sta     $10ee
        lda     $7f0000,x
        sta     $10d4
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $1108
        tya
        clc
        adc     #$0040      ; tile below and to the left
        and     #$0fc0
        sta     $0d
        tya
        dec
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10e4
        lda     $7f0000,x
        sta     $10ca
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $10fe
        tya
        clc
        adc     #$0040      ; tile below
        and     #$0fff
        tax
        lda     $7f3000,x
        sta     $10e6
        lda     $7f0000,x
        sta     $10cc
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $1100
        tya
        clc
        adc     #$0040      ; tile below and to the right
        and     #$0fc0
        sta     $0d
        tya
        inc
        and     #$003f
        ora     $0d
        tax
        lda     $7f3000,x
        sta     $10e8
        lda     $7f0000,x
        sta     $10ce
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $1102
        tya
        clc
        adc     #$0080      ; two tiles below
        and     #$0fff
        tax
        lda     $7f3000,x
        sta     $10f0
        lda     $7f0000,x
        sta     $10d6
        and     #$00ff
        asl
        tax
        lda     $1186,x
        sta     $110a
        lda     $06
        shorta
        rts

.endproc

; ---------------------------------------------------------------------------

; [ get tile properties at current tile (unused ???) ]

.proc _c019f1

_19f1:  lda     $76         ; y position
        and     #$3f
        longa
        xba
        lsr
        lsr
        sta     $0d
        lda     $06
        shorta
        lda     $75         ; x position
        and     #$3f
        ora     $0d
        sta     $0d
        ldx     $0d
        lda     $7f0000,x   ; bg1 tile
        pha
        longa
        asl
        tax
        lda     $1186,x     ; tile properties
        tax
        lda     $06
        shorta
        pla
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c01a1d

_1a1d:  lda     $ba
        bne     _1a24
        inc     $41
        rts
_1a24:  lda     $0afa
        beq     _1a35
        bmi     _1a30
        lda     $c0
        asl
        bra     _1a37
_1a30:  lda     $c0
        lsr
        bra     _1a37
_1a35:  lda     $c0
_1a37:  sta     $0f
        stz     $10
        lda     $ba
        dec
        bne     _1a63
        lda     $63
        and     #$1f
        sec
        sbc     $0f
        bcs     _1a50
        lda     $0ad9
        dec
        sta     $0ad9
_1a50:  longa
        lda     $63
        sec
        sbc     $0f
        and     #$0fff
        sta     $63
        lda     $06
        shorta
        jmp     _1ad3
_1a63:  dec
        bne     _1a8b
        lda     $61
        and     #$1f
        clc
        adc     $0f
        and     #$e0
        beq     _1a78
        lda     $0ad8
        inc
        sta     $0ad8
_1a78:  longa
        lda     $61
        clc
        and     #$0fff
        adc     $0f
        sta     $61
        lda     $06
        shorta
        jmp     _1ad3
_1a8b:  dec
        bne     _1ab3
        lda     $63
        and     #$1f
        clc
        adc     $0f
        and     #$e0
        beq     _1aa0
        lda     $0ad9
        inc
        sta     $0ad9
_1aa0:  longa
        lda     $63
        clc
        and     #$0fff
        adc     $0f
        sta     $63
        lda     $06
        shorta
        jmp     _1ad3
_1ab3:  lda     $61
        and     #$1f
        sec
        sbc     $0f
        bcs     _1ac3
        lda     $0ad8
        dec
        sta     $0ad8
_1ac3:  longa
        lda     $61
        sec
        and     #$0fff
        sbc     $0f
        sta     $61
        lda     $06
        shorta
_1ad3:  lda     $61
        and     #$1f
        bne     _1ae1
        lda     $63
        and     #$1f
        bne     _1ae1
        inc     $56
_1ae1:  inc     $41
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c01ae4

_1ae4:  lda     $be
        beq     _1af5
        lda     $be
        sec
        sbc     $c0
        sta     $be
        and     #$7f
        bne     _1af5
        stz     $be
_1af5:  lda     $ba
        bne     _1afc
        inc     $41
        rts
_1afc:  lda     $41
        lsr
        bcs     _1b46
        lda     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        lda     $ba
        ldx     $06
        stx     $71
        jsr     _c0707d
        lda     $1121
        and     #$40
        bne     _1b2d
        lda     $78
        sta     $76
        lda     $77
        sta     $75
        lda     $ba
        ldx     #$1000
        stx     $71
        jsr     _c0707d
_1b2d:  lda     $1121
        bmi     _1b44
        lda     $7a
        sta     $76
        lda     $79
        sta     $75
        lda     $ba
        ldx     #$2000
        stx     $71
        jsr     _c0707d
_1b44:  inc     $9f
_1b46:  lda     $ba
        dec
        bne     _1ba6
        lda     $63
        and     #$1f
        sec
        sbc     $c0
        bcs     _1b5d
        lda     $0ad9
        dec
        and     #$3f
        sta     $0ad9
_1b5d:  lda     $67
        and     #$1f
        sec
        sbc     $1082
        bcs     _1b70
        lda     $0b78
        dec
        and     #$3f
        sta     $0b78
_1b70:  lda     $6b
        and     #$1f
        sec
        sbc     $1086
        bcs     _1b83
        lda     $0b7a
        dec
        and     #$3f
        sta     $0b7a
_1b83:  longa
        lda     $63
        sec
        sbc     $c0
        and     #$0fff
        sta     $63
        lda     $67
        sec
        sbc     $1082
        sta     $67
        lda     $6b
        sec
        sbc     $1086
        sta     $6b
        lda     $06
        shorta
        jmp     _1cc6
_1ba6:  dec
        bne     _1c0a
        lda     $61
        and     #$1f
        clc
        adc     $c0
        and     #$e0
        beq     _1bbd
        lda     $0ad8
        inc
        and     #$3f
        sta     $0ad8
_1bbd:  lda     $65
        and     #$1f
        clc
        adc     $1080
        and     #$e0
        beq     _1bd2
        lda     $0b77
        inc
        and     #$3f
        sta     $0b77
_1bd2:  lda     $69
        and     #$1f
        clc
        adc     $1084
        and     #$e0
        beq     _1be7
        lda     $0b79
        inc
        and     #$3f
        sta     $0b79
_1be7:  longa
        lda     $61
        clc
        and     #$0fff
        adc     $c0
        sta     $61
        lda     $65
        clc
        adc     $1080
        sta     $65
        lda     $69
        clc
        adc     $1084
        sta     $69
        lda     $06
        shorta
        jmp     _1cc6
_1c0a:  dec
        bne     _1c6e
        lda     $63
        and     #$1f
        clc
        adc     $c0
        and     #$e0
        beq     _1c21
        lda     $0ad9
        inc
        and     #$3f
        sta     $0ad9
_1c21:  lda     $67
        and     #$1f
        clc
        adc     $1082
        and     #$e0
        beq     _1c36
        lda     $0b78
        inc
        and     #$3f
        sta     $0b78
_1c36:  lda     $6b
        and     #$1f
        clc
        adc     $1086
        and     #$e0
        beq     _1c4b
        lda     $0b7a
        inc
        and     #$3f
        sta     $0b7a
_1c4b:  longa
        lda     $63
        clc
        and     #$0fff
        adc     $c0
        sta     $63
        lda     $67
        clc
        adc     $1082
        sta     $67
        lda     $6b
        clc
        adc     $1086
        sta     $6b
        lda     $06
        shorta
        jmp     _1cc6
_1c6e:  lda     $61
        and     #$1f
        sec
        sbc     $c0
        bcs     _1c80
        lda     $0ad8
        dec
        and     #$3f
        sta     $0ad8
_1c80:  lda     $65
        and     #$1f
        sec
        sbc     $1080
        bcs     _1c93
        lda     $0b77
        dec
        and     #$3f
        sta     $0b77
_1c93:  lda     $69
        and     #$1f
        sec
        sbc     $1084
        bcs     _1ca6
        lda     $0b79
        dec
        and     #$3f
        sta     $0b79
_1ca6:  longa
        lda     $61
        sec
        and     #$0fff
        sbc     $c0
        sta     $61
        lda     $65
        sec
        sbc     $1080
        sta     $65
        lda     $69
        sec
        sbc     $1084
        sta     $69
        lda     $06
        shorta
_1cc6:  lda     $61
        and     #$1f
        bne     _1cd4
        lda     $63
        and     #$1f
        bne     _1cd4
        inc     $56
_1cd4:  inc     $41
        rts

.endproc

; ---------------------------------------------------------------------------

; [ update showing character ]

.proc UpdateTopChar

_1cd7:  lda     $0ada
        cmp     #$07
        bcs     _1ce1       ; branch if a vehicle
        jsr     UpdatePlayerGfx
_1ce1:  rts

.endproc

; ---------------------------------------------------------------------------

; [ update party graphic ]

.proc UpdatePlayerGfx

        ldy     $06
_1ce4:  lda     $0500,y     ; find the front character
        and     #$40
        bne     _1d0f
        lda     $0500,y     ; character id
        and     #$07
        inc
        inc
        sta     $0ada       ; party graphic
        lda     $051a,y
        and     #$20
        beq     _1d03
        lda     #$01        ; frog graphic
        sta     $0ada
        bra     _1d1d
_1d03:  lda     $051a,y
        and     #$10
        beq     _1d1d
        stz     $0ada       ; tiny graphic
        bra     _1d1d
_1d0f:  longa        ; next character
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        bra     _1ce4
_1d1d:  rts

.endproc

; ---------------------------------------------------------------------------

; [ copy party sprite graphics to vram ]

.proc TfrPartyGfx

_1d1e:  lda     $0ada       ; party graphic
        cmp     #$09
        jcs     _1dc3       ; vehicle graphic

; normal graphic
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$01
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        lda     #$da
        sta     hDMA0::ADDR_B
        lda     $57
        bne     _1d7c
        lda     $0ada
        sec
        sbc     #$02
        bmi     _1d7c       ; branch if tiny or frog
        cmp     #$05
        bcs     _1d7c       ; branch if chocobo or moogle
        ldy     $06
_1d51:  lda     $0500,y
        and     #$40
        beq     _1d66
        longa
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        bra     _1d51
_1d66:  lda     $051a,y
        and     #$20
        beq     _1d71
        lda     #$01
        bra     _1d7f
_1d71:  lda     $051a,y
        and     #$10
        beq     _1d7c
        lda     #$00
        bra     _1d7f

; tiny, frog, chocobo, or moogle
_1d7c:  lda     $0ada
_1d7f:  asl
        tax
        longa
        lda     f:_c01e02,x
        sta     hDMA0::ADDR
        lda     #$6000
        sta     hVMADDL
        lda     $53
        bne     _1d99
        lda     #$0200
        bra     _1d9c
_1d99:  lda     #$0800
_1d9c:  sta     hDMA0::SIZE
        lda     $06
        shorta
        lda     #$01
        sta     hMDMAEN
        lda     $53
        bne     _1dc2
        ldx     #$6100
        stx     $2e
        ldx     #$0200
        stx     $2c
        ldx     #$6c00      ; da/6c00 (chocobo sprite graphics)
        stx     $23
        lda     #$da
        sta     $25
        jsr     _c04cbc       ; copy data to vram
_1dc2:  rts

; vehicle
_1dc3:  pha
        cmp     #$0a
        bcs     _1dda
        ldx     $06
_1dca:  lda     f:MapSpritePal+26*$20,x   ; copy color palette
        sta     $0d00,x
        sta     $0d80,x
        inx
        cpx     #$0010
        bne     _1dca
_1dda:  pla
        sec
        sbc     #$09
        asl
        tax
        longa
        lda     f:_c01dfa,x
        sta     $30
        lda     #$6000
        sta     $33
        lda     #$0018
        sta     $35
        lda     $06
        shorta
        jsr     _c04ad5       ; copy 3bpp graphics to vram
        rts

.endproc

; ---------------------------------------------------------------------------

; pointers to vehicle graphics (+$db0000)
_c01dfa:
@1dfa:  .word   $3b80,$4240,$3a00,$3a60

; pointers to party sprite graphics (+$da0000)
_c01e02:
@1e02:  .word   $c800,$d000,$d800,$e000,$e800,$f000,$f800,$6c00,$8400

; ---------------------------------------------------------------------------

; [ copy alt. sprite graphics to vram ]

_c01e14:
_1e14:  lda     $a4
        beq     _1e59
        dec
        asl
        tax
        longa
        lda     f:_c01e5a,x   ; pointer to graphics
        sta     hDMA0::ADDR
        lda     $06
        shorta
        lda     $53
        bne     _1e31
        ldx     #$7300      ; vram address = $7300 (normal map)
        bra     _1e34
_1e31:  ldx     #$6100      ; vram address = $6100 (world map)
_1e34:  stx     hVMADDL
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$01
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        ldx     #$0800
        stx     hDMA0::SIZE
        lda     #$db
        sta     hDMA0::ADDR_B
        lda     #$01
        sta     hMDMAEN
_1e59:  rts

; pointer to alt. party sprite graphics (+db0000)
_c01e5a:
@1e5a:  .word   $4d80,$5d80,$6580,$6180,$6180

; ---------------------------------------------------------------------------

_c01e64:
@1e64:  lda     $bd
        beq     _1eb4
        lda     $0adc
        beq     _1eb4
        dec
        asl
        asl
        tay
        lda     $0add,y
        and     #$1c
        lsr
        lsr
        cmp     #$02
        beq     _1e88
        cmp     #$03
        beq     _1e88
        cmp     #$04
        beq     _1e88
        cmp     #$06
        bne     _1eb4
_1e88:  lda     $0ade,y
        and     #$7c
        lsr
        tax
        longa
        lda     f:_c01eb5,x
        sta     $11
        lda     #$7570
        sta     $02f8
        lda     $11
        sta     $02fa
        lda     #$7578
        sta     $02fc
        lda     $11
        eor     #$4000
        sta     $02fe
        lda     $06
        shorta
_1eb4:  rts

_c01eb5:
@1eb5:  .word   $3044,$3044,$3045,$3045,$3045,$3046,$3046,$3046

; ---------------------------------------------------------------------------

; [  ]

.proc _c01ec5

_1ec5:  ldy     $06
        sty     $23
_1ec9:  ldy     $23
        lda     $6f
        jne     _1f48
        lda     $0ade,y
        jmi     _1f48
        lda     $0add,y
        lsr5
        cmp     $0ad6       ; map index
        jne     _1f48
        lda     $0adf,y
        clc
        adc     f:_c0205f
        sec
        sbc     $0ad8
        cmp     f:_c0205f+1
        bcs     _1f48
        jsr     _c0201f
        lda     $14
        lsr
        bcs     _1f48
        lda     $0ae0,y
        clc
        adc     f:_c0205f+2
        sec
        sbc     $0ad9
        cmp     f:_c0205f+3
        bcs     _1f48
        jsr     _c0203f
        ldy     $23
        lda     $0add,y
        and     #$03
        clc
        adc     $23
        clc
        adc     #$04
        asl
        tax
        longa
        lda     f:_c02063,x
        sta     $11
        lda     $06
        shorta
        lda     #$60
        tax
        lda     $0add,y
        and     #$1c
        cmp     #$08
        bcs     _1f45
        jsr     _c01fb4
        bra     _1f48
_1f45:  jsr     _c01f57
_1f48:  lda     $23
        clc
        adc     #$04
        sta     $23
        cmp     #$18
        jne     _1ec9
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c01f57

_1f57:  lda     $23
        asl2
        tay
        lda     #4
        sta     $09
_1f60:  lda     $13
        clc
        adc     f:_c020b7,x
        sec
        sbc     $10a0
        sta     $0298,y
        lda     $15
        clc
        adc     f:_c020b7+1,x
        sec
        sbc     $10a2
        sta     $0299,y
        longa
        lda     f:_c020b7+2,x
        clc
        adc     $11
        sta     $029a,y
        lda     $06
        shorta
        inx4
        iny4
        dec     $09
        bne     _1f60
        ldx     $23
        lda     $0add,x
        and     #$1c
        cmp     #$08
        bne     _1fb3
        lda     $0297,y
        and     #$0f
        sta     $0297,y
        lda     $0293,y
        and     #$0f
        sta     $0293,y
_1fb3:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc _c01fb4

_1fb4:  lda     $23
        asl2
        tay
        lda     #$04
        sta     $09
_1fbd:  lda     $13
        clc
        adc     f:_c02a95,x
        sec
        sbc     #$08
        sec
        sbc     $10a0
        sta     $0298,y
        lda     $15
        clc
        adc     f:_c02a95+1,x
        sec
        sbc     #$07
        sec
        sbc     $10a2
        sta     $0299,y
        longa
        lda     f:_c02a95+2,x
        clc
        adc     $11
        ora     #$3000
        sta     $029a,y
        lda     $06
        shorta
        inx4
        iny4
        dec     $09
        bne     _1fbd
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

_1fff:  lda     $13
        sta     $0284
        sec
        sbc     #$08
        sta     $0280
        lda     $15
        sec
        sbc     #$03
        sta     $0281
        sta     $0285
        ldx     #$0038
        stx     $0282
        stx     $0286
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0201f:
_201f:  longa
        asl4
        clc
        adc     #$0008
        sta     $0d
        lda     $61
        and     #$001f
        lsr
        sta     $0f
        lda     $0d
        sec
        sbc     $0f
        sta     $13
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0203f:
_203f:  longa
        asl4
        clc
        adc     #$0008
        sta     $0d
        lda     $63
        and     #$001f
        lsr
        sta     $0f
        lda     $0d
        sec
        sbc     $0f
        sta     $15
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

_c0205f:
@205f:  .byte   $07,$11,$07,$0f

_c02063:
@2063:  .word   $0000,$0000,$0000,$0000
        .word   $0010,$0000,$0000,$0000
        .word   $0020,$0000,$0000,$0000
        .word   $046c,$0000,$0000,$0000
        .word   $06cc,$06b4,$06cc,$06b4
        .word   $0654,$0684,$0000,$0000
        .word   $069c,$0654,$06cc,$06b4

_c0209b:
@209b:  .byte   $00,$00,$00,$00
        .byte   $00,$00,$00,$00
        .byte   $08,$00,$00,$00
        .byte   $08,$00,$00,$00
        .byte   $20,$00,$00,$00
        .byte   $20,$10,$00,$00
        .byte   $02,$20,$00,$00

; ---------------------------------------------------------------------------

_c020b7:
_20b7:  .byte   $f8,$f5,$08,$30
        .byte   $00,$f5,$09,$30
        .byte   $f8,$fd,$0a,$30
        .byte   $00,$fd,$0b,$30
_20c7:  .byte   $f8,$f5,$0c,$30
        .byte   $00,$f5,$0d,$30
        .byte   $f8,$fd,$0e,$30
        .byte   $00,$fd,$0f,$30
_20d7:  .byte   $f8,$f5,$11,$70
        .byte   $00,$f5,$10,$70
        .byte   $f8,$fd,$13,$70
        .byte   $00,$fd,$12,$70
_20e7:  .byte   $f8,$f5,$15,$70
        .byte   $00,$f5,$14,$70
        .byte   $f8,$fd,$17,$70
        .byte   $00,$fd,$16,$70
_20f7:  .byte   $f8,$f5,$00,$30
        .byte   $00,$f5,$01,$30
        .byte   $f8,$fd,$02,$30
        .byte   $00,$fd,$03,$30
_2107:  .byte   $f8,$f5,$04,$30
        .byte   $00,$f5,$05,$30
        .byte   $f8,$fd,$06,$30
        .byte   $00,$fd,$07,$30
_2117:  .byte   $f8,$f5,$10,$30
        .byte   $00,$f5,$11,$30
        .byte   $f8,$fd,$12,$30
        .byte   $00,$fd,$13,$30
_2127:  .byte   $f8,$f5,$14,$30
        .byte   $00,$f5,$15,$30
        .byte   $f8,$fd,$16,$30
        .byte   $00,$fd,$17,$30

 ; ---------------------------------------------------------------------------

 ; [  ]

_c02137:
_2137:  lda     $bd
        bne     _213c       ; branch if party sprite is visible
        rts
_213c:  lda     $be
        bpl     _2143
        and     #$7f
        lsr
_2143:  tax
        lda     f:_c022db,x
        sta     $0d
        lda     $0adc
        dec
        asl2
        tay
        lda     $61
        clc
        adc     $63
        lsr4
        and     #$01
        sta     $08
        lda     $0adb
        asl
        clc
        adc     $08
        sta     $08
        lda     $0ada
        cmp     #$07
        beq     _2172
        cmp     #$08
        bne     _2179

; chocobo or moogle

_2172:  lda     $08
        clc
        adc     #$40
        sta     $08
_2179:  lda     $08
        longa
        asl4
        tax
        lda     $06
        shorta
        lda     $0adc
        cmp     #$02
        jcs     _2246
        stz     $11
        cmp     #$00
        beq     _21aa
        lda     #$10
        sta     $11
        longa
        txa
        clc
        adc     #$0400
        tax
        lda     $06
        shorta
        lda     #$70
        bra     _21b5
_21aa:  lda     $61
        lsr4
        inc
        and     #$01
        ora     #$70
_21b5:  sta     $15
        lda     $0d
        bne     _21c1
        lda     $ca
        and     #$60
        bne     _21c9
_21c1:  lda     #$30
        sta     $08
        sta     $09
        bra     _21e1
_21c9:  lda     $ca
        and     #$20
        beq     _21d9
        lda     #$30
        sta     $08
        lda     #$00
        sta     $09
        bra     _21e1
_21d9:  lda     #$30
        sta     $08
        lda     #$38
        sta     $09
_21e1:  ldy     #$0088
        phx
        phy
        lda     #$04
        sta     $0a
_21ea:  lda     f:_c02a95,x
        clc
        adc     #$70
        sec
        sbc     $10a0
        sta     $0200,y
        lda     f:_c02a95+1,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0201,y
        lda     f:_c02a95+2,x
        clc
        adc     $11
        sta     $0202,y
        inx4
        iny4
        dec     $0a
        bne     _21ea
        ply
        plx
        lda     f:_c02a95+3,x
        ora     $08
        sta     $0203,y
        lda     f:_c02a95+7,x
        ora     $08
        sta     $0207,y
        lda     f:_c02a95+11,x
        ora     $09
        sta     $020b,y
        lda     f:_c02a95+15,x
        ora     $09
        sta     $020f,y
        rts
_2246:  lda     $0ade,y
        and     #$7f
        sta     $15
        lda     #$78
        sec
        sbc     $15
        sta     $15
        lda     $0adc
        asl2
        sta     $09
        lda     $0add,y
        and     #$03
        clc
        adc     $09
        asl
        tax
        longa
        lda     f:_c02063,x
        sta     $11
        lda     $06
        shorta
        lda     $0adc                   ; vehicle
        asl2
        sta     $09
        lda     $0add,y                 ; vehicle graphic
        and     #$03
        ora     $09
        tax
        lda     $3e
        and     f:_c0209b,x
        beq     _228a
        lda     #$10
_228a:  sta     $08
        lda     $0adb
        asl5
        clc
        adc     $08
        tax
        ldy     #$0088
        lda     #$04
        sta     $0a
_229f:  lda     f:_c020b7,x
        clc
        adc     #$78
        sec
        sbc     $10a0
        sta     $0200,y
        lda     f:_c020b7+1,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0201,y
        longa
        lda     f:_c020b7+2,x
        clc
        adc     $11
        sta     $0202,y
        lda     $06
        shorta
        inx4
        iny4
        dec     $0a
        bne     _229f
        rts

; ---------------------------------------------------------------------------

_c022db:
@22db:  .byte   $00,$01,$03,$04,$05,$06,$07,$08,$09,$0a,$0a,$0b,$0b,$0c,$0c,$0c
        .byte   $0c,$0c,$0c,$0b,$0b,$0a,$0a,$09,$08,$07,$06,$05,$04,$03,$01,$00

; ---------------------------------------------------------------------------

_c022fb:
        stz     $3e
_22fd:  jsr     WaitVBlank
        jsr     _c04c95
        jsr     _c047f7
        lda     $3e
        and     #$18
        lsr2
        clc
        adc     $169b
        tax
        longa
        lda     #$6d70
        sta     $0288
        lda     #$6d78
        sta     $028c
        lda     #$7570
        sta     $0290
        lda     #$7578
        sta     $0294
        lda     f:_c0234c,x
        sta     $028a
        inc
        sta     $028e
        inc
        sta     $0292
        inc
        sta     $0296
        lda     $06
        shorta
        jsr     _c01ec5
        lda     $3e
        cmp     #$1f
        bne     _22fd
        rts

; ---------------------------------------------------------------------------

_c0234c:
_234c:  .word   $36e4,$36e8,$36ec,$36f0,$36f0,$36ec,$36e8,$36e4
_235c:  .word   $36f4,$36f8,$36fc,$3700,$3700,$36fc,$36f8,$36f4
_236c:  .word   $3704,$3708,$370c,$3710,$3710,$370c,$3708,$3704

; ---------------------------------------------------------------------------

; [ update party sprite ]

UpdatePlayerSprite:
        lda     $bd                     ; return if party sprite is not shown
        bne     :+
        rts
:       lda     $cb                     ; return if party sprite is hidden (z-level)
        beq     :+
        rts
:       lda     $be                     ; jump counter
        bpl     :+
        and     #$7f
        lsr
:       tax
        lda     f:_c022db,x
        sta     $0d
        lda     $10fb                   ; map tile properties byte 2
        bpl     _23a3                   ; branch if not auto-move tile
        ldx     #$0240                  ; pointer to sprite frame data ???
        lda     $c6
        sta     $15
        jmp     _241d
_23a3:  lda     $bc
        beq     _23de
        lda     $57
        beq     _23af
        lda     $ba
        beq     _23de
_23af:  lda     $0ada
        cmp     #$0a
        bne     _23c6
        lda     $c6
        sta     $15
        lda     $3f
        and     #$02
        lsr
        clc
        adc     #$0a
        sta     $08
        bra     _2410
_23c6:  lda     $61
        clc
        adc     $63
        lsr4
        and     #$01
        sta     $08
        lda     $0adb
        asl
        clc
        adc     $08
        sta     $bb
        bra     _23e0
_23de:  lda     $bb
_23e0:  sta     $08
        lda     $0ada
        cmp     #$0d
        bcs     _2402
        cmp     #$07
        bcc     _2402
        lda     $08
        clc
        adc     #$40
        sta     $08
        lda     $0ada
        cmp     #$09
        bcc     _2402
        lda     $08
        clc
        adc     #$08
        sta     $08
_2402:  lda     $61
        lsr4
        inc
        and     #$01
        clc
        adc     $c6
        sta     $15
_2410:  lda     $08
        longa
        asl4
        tax
        lda     $06
        shorta
_241d:  lda     $0ada                   ; party sprite graphic
        cmp     #$0b
        bcc     _242c

; upper half = 3/0, lower half = 3/0
        lda     #$30
        sta     $08
        sta     $09
        bra     _2494

_242c:  lda     $0d
        bne     _246e                   ; branch if jumping
        lda     $c6
        cmp     #$70
        bne     _246e
        lda     $ca
        and     #$08
        bne     _244a                   ; branch if sprite gets fully hidden

; half hidden sprite
        lda     $ca
        and     #$04
        beq     _2476                   ; branch if not a bridge tile
        lda     $c3
        and     #$02
        beq     _246e                   ; branch if not on lower z-level
        bra     _2450                   ; branch if on lower z-level

; fully hidden sprite
_244a:  lda     $ca
        and     #$04
        bne     _2460                   ; branch if a bridge tile

; upper half = 2/p, lower half = 2/4
; (fully hidden sprite, bridge || half hidden sprite, bridge, lower z-level)
_2450:  lda     #$28                    ; priority 2, palette offset 2
        sta     $09                     ; lower half of sprite
        lda     $110f
        and     #$10
        lsr
        ora     #$20                    ; priority 2, palette offset 0 or 4
        sta     $08                     ; upper half of sprite
        bra     _2494

; upper half = 2/p, lower half = 2/p (fully hidden, bridge)
_2460:  lda     $110f
        and     #$10
        lsr
        ora     #$20
        sta     $08
        sta     $09
        bra     _2494

; upper half = 3/0, lower half = 3/0 (default)
_246e:  lda     #$30
        sta     $08
        sta     $09
        bra     _2494

; upper half = 3/p, lower half = 2/4 (half hidden, not bridge)
_2476:  lda     $110f
        and     #$10
        lsr
        ora     #$30
        sta     $08
        lda     #$28
        sta     $09
        bra     _2494

; upper half = 3/p, lower half = 2/0 (unused)
        lda     $110f
        and     #$10
        lsr
        ora     #$30
        sta     $08
        lda     #$20
        sta     $09
_2494:  phx
        lda     $0ada
        tax
        lda     $08
        ora     f:_c0256c,x
        sta     $08
        lda     $09
        ora     f:_c0256c,x
        sta     $09
        plx

; pointer to sprite data for upper half
; ($0110 for normal priority, $01e8 for low priority)
        ldy     $c7
        lda     f:_c02a95,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $0200,y                 ; x position
        lda     f:_c02a95+1,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0201,y                 ; y position
        lda     f:_c02a95+2,x
        sta     $0202,y                 ; tile index
        lda     f:_c02a95+3,x
        ora     $08
        sta     $0203,y                 ; vhoopppm
        lda     f:_c02a95+4,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $0204,y
        lda     f:_c02a95+5,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0205,y
        lda     f:_c02a95+6,x
        sta     $0206,y
        lda     f:_c02a95+7,x
        ora     $08
        sta     $0207,y
        ldy     #$01e8                  ; pointer to sprite data for lower half
        lda     f:_c02a95+8,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $0208,y
        lda     f:_c02a95+9,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0209,y
        lda     f:_c02a95+10,x
        sta     $020a,y
        lda     f:_c02a95+11,x
        ora     $09
        sta     $020b,y
        lda     f:_c02a95+12,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $020c,y
        lda     f:_c02a95+13,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $020d,y
        lda     f:_c02a95+14,x
        sta     $020e,y
        lda     f:_c02a95+15,x
        ora     $09
        sta     $020f,y
        rts

; ---------------------------------------------------------------------------

_c0256c:
@256c:  .res    16, 0

; ---------------------------------------------------------------------------

; [  ]

_c0257c:
_257c:  lda     #$80
        sta     hVMAINC
        lda     $1114
        longa
        asl4
        sta     $23
        lda     $06
        shorta
        ldx     $06
        stx     $11
_2594:  longa
        lda     #$7c00
        sta     $2e
        lda     $11
        cmp     #$0008
        bcc     _25a7
        lda     #$7d00
        sta     $2e
_25a7:  lda     $11
        asl5
        clc
        adc     $2e
        sta     hVMADDL
        clc
        adc     #$0100
        sta     $2e
        lda     $06
        shorta
        ldx     $23
        lda     f:_c02667,x
        longa
        asl3
        tax
        lda     $06
        shorta
        phx
        ldy     #$0008
_25d2:  lda     f:MapOverlayGfx,x
        stz     hVMDATAL
        sta     hVMDATAH
        inx
        dey
        bne     _25d2
        jsr     _c02636
        ldy     #$0008
_25e6:  lda     f:MapOverlayGfx,x
        stz     hVMDATAL
        sta     hVMDATAH
        inx
        dey
        bne     _25e6
        jsr     _c02636
        ldy     $2e
        sty     hVMADDL
        plx
        ldy     #$0008
_2600:  lda     f:MapOverlayGfx+128,x
        stz     hVMDATAL
        sta     hVMDATAH
        inx
        dey
        bne     _2600
        jsr     _c02636
        ldy     #$0008
_2614:  lda     f:MapOverlayGfx+128,x
        stz     hVMDATAL
        sta     hVMDATAH
        inx
        dey
        bne     _2614
        jsr     _c02636
        ldx     $23
        inx
        stx     $23
        inc     $11
        lda     $11
        cmp     #$10
        jne     _2594
        rts

; ---------------------------------------------------------------------------

; [  ]

_c02636:
.repeat 8
        stz     hVMDATAL
        stz     hVMDATAH
.endrep
        rts

; ---------------------------------------------------------------------------

_c02667:
        .byte   $00,$02,$8c,$8e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$a0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$a4,$84,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$66,$68,$6a,$a2,$a0,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$66,$68,$6a,$a2,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$2c,$2e,$40,$42,$44,$62,$64,$46,$48,$00,$00,$00,$00,$00
        .byte   $00,$02,$4a,$4c,$60,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$04,$06,$08,$0a,$0c,$0e,$20,$22,$24,$26,$28,$2a,$00,$00
        .byte   $00,$02,$04,$06,$08,$0a,$0c,$0e,$20,$22,$24,$26,$28,$2a,$a6,$a8
        .byte   $00,$02,$04,$06,$08,$0a,$0c,$0e,$20,$22,$24,$26,$28,$2a,$a6,$a8
        .byte   $00,$02,$88,$8a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$88,$8a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$82,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$80,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$86,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$6c,$6e,$4e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$8c,$8e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$2c,$2e,$40,$42,$44,$62,$64,$46,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; ---------------------------------------------------------------------------

; [  ]

_c02817:
_2817:  lda     $1114       ; tileset
        asl
        tax
        longa
        lda     f:_c0d980Ptrs,x
        clc
        adc     #near _c0d980Ptrs
        sta     $04f0
        lda     $06
        shorta
        lda     #^_c0d980Ptrs
        sta     $04f2
        ldx     #$1873
        stx     $04f3
        lda     #$00
        sta     $04f5
        jsl     Decomp_ext
        rts

; ---------------------------------------------------------------------------

; [  ]

_c02842:
@2842:  lda     $bd
        bne     @2847
        rts
@2847:  ldy     #$0100
        lda     $c0
        dec
        asl2
        sta     $0d
        lda     $ba
        beq     @2877
        dec
        clc
        adc     $0d
        asl
        tax
        longa
        lda     $108c
        clc
        adc     f:_c028f3,x
        sta     $108c
        lda     $108e
        clc
        adc     f:_c02933,x
        sta     $108e
        lda     $06
        shorta
@2877:  ldx     $06
@2879:  lda     $13c9,x
        and     #$80
        bne     @28ac
        lda     $13c6,x
        sec
        sbc     $108d
        sec
        sbc     $10a0
        sta     $0200,y
        lda     $13c7,x
        sec
        sbc     $108f
        sec
        sbc     $10a2
        sta     $0201,y
        lda     $13c8,x
        sta     $0202,y
        lda     $13c9,x
        and     #$7f
        sta     $0203,y
        bra     @28d6
@28ac:  lda     $13c6,x
        sec
        sbc     $108d
        sec
        sbc     $10a0
        sta     $02d8,y
        lda     $13c7,x
        sec
        sbc     $108f
        sec
        sbc     $10a2
        sta     $02d9,y
        lda     $13c8,x
        sta     $02da,y
        lda     $13c9,x
        and     #$7f
        sta     $02db,y
@28d6:  iny4
        inx4
        cpx     #$0010
        bne     @2879
        lda     #$aa
        sta     $0410
        lda     #$a0
        sta     $041d
        lda     #$0a
        sta     $041e
        rts

; ---------------------------------------------------------------------------

_c028f3:
@28f3:  .word   $0000,$0080,$0000,$ff80
        .word   $0000,$0100,$0000,$ff00
        .word   $0000,$0180,$0000,$fe80
        .word   $0000,$0200,$0000,$fe00
        .word   $0000,$0080,$0000,$ff80
        .word   $0000,$0100,$0000,$ff00
        .word   $0000,$0180,$0000,$fe80
        .word   $0000,$0400,$0000,$fc00

_c02933:
@2933:  .word   $ff80,$0000,$0080,$0000
        .word   $ff00,$0000,$0100,$0000
        .word   $fe80,$0000,$0180,$0000
        .word   $fe00,$0000,$0200,$0000
        .word   $ff80,$0000,$0080,$0000
        .word   $ff00,$0000,$0100,$0000
        .word   $fe80,$0000,$0180,$0000
        .word   $fc00,$0000,$0400,$0000

; ---------------------------------------------------------------------------

; [  ]

_c02973:
@2973:  stz     $108c
        stz     $108d
        stz     $108e
        stz     $108f
        lda     $ba
        beq     @2987
        dec
        asl3
@2987:  tax
        ldy     $06
@298a:  phy
        stz     $0f
        stz     $10
        lda     f:_c02a75,x
        sta     $13c6,y
        lda     f:_c02a75+1,x
        sta     $13c7,y
        lda     f:_c02a55,x
        tay
        cmp     #$12
        bne     @29be
        lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     @29e1
        lda     $c3
        cmp     #$02
        bne     @29e1
        lda     $1104
        and     #$03
        cmp     #$01
        bne     @2a0c
        bra     @29d8
@29be:  cmp     #$06
        bcs     @29e1
        lda     $10f8,y
        and     #$04
        beq     @29e1
        lda     $c3
        cmp     #$02
        bne     @2a0c
        lda     $10f2,y     ; map tile properties byte 1
        and     #$03
        cmp     #$01
        bne     @2a11
@29d8:  lda     #$80
        sta     $10
        lda     #$02
        jmp     @2a1c
@29e1:  lda     $10fa       ; map tile properties byte 1
        and     #$07
        cmp     #$05
        beq     @29f0
        cmp     #$01
        bne     @29ff
        bra     @29f6
@29f0:  lda     $c3
        cmp     #$02
        beq     @29ff
@29f6:  lda     $10f2,y     ; map tile properties byte 1
        and     #$03
        cmp     #$02
        beq     @2a0c
@29ff:  lda     $10f2,y     ; map tile properties byte 1
        and     #$04
        beq     @2a15
        lda     $c3
        cmp     #$01
        bne     @2a11
@2a0c:  lda     #$00
        jmp     @2a1c
@2a11:  lda     #$80
        sta     $10
@2a15:  lda     $10be,y
        tay
        lda     $1873,y
@2a1c:  ply
        longa
        clc
        adc     #$01c0
        ora     #$0400
        ora     $0f
        sta     $13c8,y
        and     #$0010
        beq     @2a39
        lda     $13c8,y
        ora     #$4000
        sta     $13c8,y
@2a39:  lda     $13c8,y
        and     #$ffef
        sta     $13c8,y
        lda     $06
        shorta
        inx2
        iny4
        cpy     #$0010
        jne     @298a
        rts

; ---------------------------------------------------------------------------

_c02a55:
@2a55:  .byte   $12,$00,$02,$00
        .byte   $08,$00,$08,$00
        .byte   $02,$00,$04,$00
        .byte   $08,$00,$0a,$00
        .byte   $02,$00,$08,$00
        .byte   $0e,$00,$0e,$00
        .byte   $00,$00,$02,$00
        .byte   $06,$00,$08,$00

_c02a75:
@2a75:  .byte   $70,$4f,$70,$5f
        .byte   $70,$6f,$70,$6f
        .byte   $70,$5f,$80,$5f
        .byte   $70,$6f,$80,$6f
        .byte   $70,$5f,$70,$6f
        .byte   $70,$7f,$70,$7f
        .byte   $60,$5f,$70,$5f
        .byte   $60,$6f,$70,$6f

; ---------------------------------------------------------------------------

; sprite frames (80 items, 16 bytes per frame)
_c02a95:
@2a95:  .byte   $00,$fc,$04,$00,$08,$fc,$05,$00,$00,$04,$06,$00,$08,$04,$07,$00
        .byte   $00,$fc,$04,$00,$08,$fc,$05,$00,$00,$04,$07,$40,$08,$04,$06,$40
        .byte   $00,$fc,$09,$40,$08,$fc,$08,$40,$00,$04,$0b,$40,$08,$04,$0a,$40
        .byte   $00,$fc,$0d,$40,$08,$fc,$0c,$40,$00,$04,$0f,$40,$08,$04,$0e,$40
        .byte   $00,$fc,$00,$00,$08,$fc,$01,$00,$00,$04,$02,$00,$08,$04,$03,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$01,$00,$00,$04,$03,$40,$08,$04,$02,$40
        .byte   $00,$fc,$08,$00,$08,$fc,$09,$00,$00,$04,$0a,$00,$08,$04,$0b,$00
        .byte   $00,$fc,$0c,$00,$08,$fc,$0d,$00,$00,$04,$0e,$00,$08,$04,$0f,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$01,$00,$00,$04,$02,$00,$08,$04,$03,$00
        .byte   $00,$fc,$04,$00,$08,$fc,$05,$00,$00,$04,$06,$00,$08,$04,$07,$00
        .byte   $00,$fc,$08,$00,$08,$fc,$09,$00,$00,$04,$0a,$00,$08,$04,$0b,$00
        .byte   $00,$fc,$0c,$00,$08,$fc,$0d,$00,$00,$04,$0e,$00,$08,$04,$0f,$00
        .byte   $00,$fc,$01,$40,$08,$fc,$00,$40,$00,$04,$03,$40,$08,$04,$02,$40
        .byte   $00,$fc,$05,$40,$08,$fc,$04,$40,$00,$04,$07,$40,$08,$04,$06,$40
        .byte   $00,$fc,$09,$40,$08,$fc,$08,$40,$00,$04,$0b,$40,$08,$04,$0a,$40
        .byte   $00,$fc,$0d,$40,$08,$fc,$0c,$40,$00,$04,$0f,$40,$08,$04,$0e,$40

@2b95:  .byte   $00,$fc,$19,$00,$08,$fc,$01,$00,$00,$04,$1a,$00,$08,$04,$03,$00
        .byte   $00,$fc,$18,$00,$08,$fc,$01,$00,$00,$04,$1a,$00,$08,$04,$03,$00
        .byte   $00,$fc,$1b,$00,$08,$fc,$01,$00,$00,$04,$1a,$00,$08,$04,$03,$00
        .byte   $00,$fc,$01,$40,$08,$fc,$19,$40,$00,$04,$03,$40,$08,$04,$1a,$40
        .byte   $00,$fc,$01,$40,$08,$fc,$18,$40,$00,$04,$03,$40,$08,$04,$1a,$40
        .byte   $00,$fc,$01,$40,$08,$fc,$1b,$40,$00,$04,$03,$40,$08,$04,$1a,$40
        .byte   $00,$fc,$04,$00,$08,$fc,$31,$00,$00,$04,$3e,$00,$08,$04,$28,$00
        .byte   $00,$fc,$04,$00,$08,$fc,$30,$00,$00,$04,$3e,$00,$08,$04,$28,$00
        .byte   $00,$fc,$31,$40,$08,$fc,$04,$40,$00,$04,$28,$40,$08,$04,$3e,$40
        .byte   $00,$fc,$30,$40,$08,$fc,$04,$40,$00,$04,$28,$40,$08,$04,$3e,$40
        .byte   $00,$fc,$08,$00,$08,$fc,$37,$00,$00,$04,$0a,$00,$08,$04,$38,$00
        .byte   $00,$fc,$08,$00,$08,$fc,$09,$00,$00,$04,$3c,$00,$08,$04,$3d,$00
        .byte   $00,$fc,$37,$40,$08,$fc,$08,$40,$00,$04,$38,$40,$08,$04,$0a,$40
        .byte   $00,$fc,$09,$40,$08,$fc,$08,$40,$00,$04,$3d,$40,$08,$04,$3c,$40
        .byte   $00,$fc,$10,$00,$08,$fc,$11,$00,$00,$04,$12,$00,$08,$04,$13,$00
        .byte   $00,$fc,$25,$00,$08,$fc,$26,$00,$00,$04,$06,$00,$08,$04,$07,$00

@2c95:  .byte   $00,$fc,$39,$00,$08,$fc,$3a,$00,$00,$04,$3b,$00,$08,$04,$0b,$00
        .byte   $00,$fc,$3a,$40,$08,$fc,$39,$40,$00,$04,$0b,$40,$08,$04,$3b,$40
        .byte   $00,$fc,$1c,$00,$08,$fc,$1d,$00,$00,$04,$1e,$00,$08,$04,$1f,$00
        .byte   $00,$fc,$1d,$40,$08,$fc,$1c,$40,$00,$04,$1f,$40,$08,$04,$1e,$40
        .byte   $00,$fc,$20,$00,$08,$fc,$21,$00,$00,$04,$22,$00,$08,$04,$2f,$00
        .byte   $00,$fc,$21,$40,$08,$fc,$20,$40,$00,$04,$2f,$40,$08,$04,$22,$40
        .byte   $00,$fc,$34,$00,$08,$fc,$35,$00,$00,$04,$36,$00,$08,$04,$36,$40
        .byte   $00,$fc,$32,$00,$08,$fc,$33,$00,$00,$04,$36,$00,$08,$04,$36,$40
        .byte   $00,$fc,$2d,$00,$08,$fc,$2e,$00,$00,$04,$2a,$00,$08,$04,$2a,$40
        .byte   $00,$fc,$2b,$00,$08,$fc,$2c,$00,$00,$04,$29,$00,$08,$04,$29,$40
        .byte   $00,$fc,$23,$00,$08,$fc,$24,$00,$00,$04,$27,$00,$08,$04,$27,$40
        .byte   $00,$fc,$04,$00,$08,$fc,$05,$00,$00,$04,$06,$00,$08,$04,$07,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$01,$00,$00,$04,$36,$00,$08,$04,$36,$40
        .byte   $00,$fc,$31,$40,$08,$fc,$31,$00,$00,$04,$28,$40,$08,$04,$28,$00
        .byte   $00,$fc,$30,$40,$08,$fc,$30,$00,$00,$04,$28,$40,$08,$04,$28,$00
        .byte   $00,$fc,$14,$00,$08,$fc,$15,$00,$00,$04,$16,$00,$08,$04,$17,$00

@2d95:  .byte   $00,$fc,$15,$40,$08,$fc,$14,$40,$00,$04,$17,$40,$08,$04,$16,$40
        .byte   $00,$fc,$04,$00,$08,$fc,$04,$40,$00,$04,$3e,$00,$08,$04,$3e,$40
        .byte   $00,$fc,$12,$00,$08,$fc,$13,$00,$00,$04,$06,$00,$08,$04,$07,$00
        .byte   $00,$fc,$15,$40,$08,$fc,$14,$40,$00,$04,$0b,$40,$08,$04,$16,$40
        .byte   $00,$fc,$10,$00,$08,$fc,$11,$00,$00,$04,$02,$00,$08,$04,$03,$00
        .byte   $00,$fc,$14,$00,$08,$fc,$15,$00,$00,$04,$16,$00,$08,$04,$0b,$00
        .byte   $00,$fc,$01,$40,$08,$fc,$17,$40,$00,$04,$03,$40,$08,$04,$1a,$40
        .byte   $00,$fc,$01,$40,$08,$fc,$18,$40,$00,$04,$03,$40,$08,$04,$1a,$40
        .byte   $00,$fc,$17,$00,$08,$fc,$01,$00,$00,$04,$1a,$00,$08,$04,$03,$00
        .byte   $00,$fc,$18,$00,$08,$fc,$01,$00,$00,$04,$1a,$00,$08,$04,$03,$00
        .byte   $00,$fc,$17,$00,$08,$fc,$19,$00,$00,$04,$1a,$00,$08,$04,$1b,$00
        .byte   $00,$fc,$1c,$00,$08,$fc,$1d,$00,$00,$04,$1e,$00,$08,$04,$1f,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$00,$00,$00,$04,$00,$00,$08,$04,$00,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$00,$00,$00,$04,$00,$00,$08,$04,$00,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$00,$00,$00,$04,$00,$00,$08,$04,$00,$00
        .byte   $00,$fc,$00,$00,$08,$fc,$00,$00,$00,$04,$00,$00,$08,$04,$00,$00

@2e95:  .byte   $00,$fc,$04,$00,$08,$fc,$05,$00,$00,$04,$06,$00,$08,$04,$07,$00
        .byte   $00,$fc,$05,$40,$08,$fc,$04,$40,$00,$04,$07,$40,$08,$04,$06,$40
        .byte   $00,$fc,$09,$40,$08,$fc,$08,$40,$00,$04,$0b,$40,$08,$04,$0a,$40
        .byte   $00,$fc,$0d,$40,$08,$fc,$0c,$40,$00,$04,$0f,$40,$08,$04,$0e,$40
        .byte   $00,$fc,$00,$00,$08,$fc,$01,$00,$00,$04,$02,$00,$08,$04,$03,$00
        .byte   $00,$fc,$01,$40,$08,$fc,$00,$40,$00,$04,$03,$40,$08,$04,$02,$40
        .byte   $00,$fc,$08,$00,$08,$fc,$09,$00,$00,$04,$0a,$00,$08,$04,$0b,$00
        .byte   $00,$fc,$0c,$00,$08,$fc,$0d,$00,$00,$04,$0e,$00,$08,$04,$0f,$00
        .byte   $00,$fc,$08,$00,$08,$fc,$09,$00,$00,$04,$0a,$00,$08,$04,$0b,$00
        .byte   $00,$fc,$0c,$00,$08,$fc,$0d,$00,$00,$04,$0e,$00,$08,$04,$0f,$00
        .byte   $00,$fc,$11,$40,$08,$fc,$10,$40,$00,$04,$13,$40,$08,$04,$12,$40
        .byte   $00,$fc,$15,$40,$08,$fc,$14,$40,$00,$04,$17,$40,$08,$04,$16,$40
        .byte   $00,$fc,$00,$00,$08,$fc,$01,$00,$00,$04,$02,$00,$08,$04,$03,$00
        .byte   $00,$fc,$04,$00,$08,$fc,$05,$00,$00,$04,$06,$00,$08,$04,$07,$00
        .byte   $00,$fc,$10,$00,$08,$fc,$11,$00,$00,$04,$12,$00,$08,$04,$13,$00
        .byte   $00,$fc,$14,$00,$08,$fc,$15,$00,$00,$04,$16,$00,$08,$04,$17,$00

; ---------------------------------------------------------------------------

; [ execute npc script ]

.proc ExecNPCScript

@2f95:  longa
        lda     $147f,x     ; npc script
        asl
        tax
        lda     f:NPCScriptPtrs,x
        sta     $23
        sta     $29
        lda     f:NPCScriptPtrs+2,x
        sta     $26
        lda     $06
        shorta
@2fae:  ldx     $23
        lda     f:NPCScript::Start,x
        cmp     #$f0        ; find the end of the script ($f0)
        beq     @2fcd
        sec
        sbc     #$f0
        tax
        lda     f:NPCCmdNumBytes,x
        clc
        adc     $23
        sta     $23
        lda     #$00
        adc     $24
        sta     $24
        bra     @2fae
@2fcd:  inx
        longa
        ldy     $06
@2fd2:  cpx     $26
        beq     @2fe3
        lda     f:NPCScript::Start,x   ; copy npc dialog
        sta     $13d6,y
        inx2
        iny2
        bra     @2fd2
@2fe3:  lda     $06
        shorta
@2fe7:  ldx     $29
        lda     f:NPCScript::Start,x   ; npc script command

; command $ff: execute event
@2fed:  cmp     #$ff
        jeq     @314d

; command $fe: if event flag $00xx set
        cmp     #$fe
        bne     @3006
        lda     f:NPCScript::Start+1,x
        jsr     _c0ca2f       ; get event flag $00xx
        cmp     #$00
        bne     @303c
        jmp     @3136

; command $fd: if event flag $00xx clear
@3006:  cmp     #$fd
        bne     @3018
        lda     f:NPCScript::Start+1,x
        jsr     _c0ca2f       ; get event flag $00xx
        cmp     #$00
        beq     @303c
        jmp     @3136

; command $fc: if event flag $01xx set
@3018:  cmp     #$fc
        bne     @302a
        lda     f:NPCScript::Start+1,x
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        bne     @303c
        jmp     @3136

; command $fb: if event flag $01xx clear
@302a:  cmp     #$fb
        bne     @3045
        lda     f:NPCScript::Start+1,x
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        jne     @3136
@303c:  ldx     $29         ; skip 2 bytes
        inx2
        stx     $29
        jmp     @2fe7

; command $fa: if character data xxxx <=> yy
@3045:  cmp     #$fa
        bne     @3092
        longa
        lda     f:NPCScript::Start+1,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     f:NPCScript::Start+2,x
        and     #$c0
        bne     @306b

; 0 (if equal)
@305f:  lda     $0500,y     ; character data
        cmp     f:NPCScript::Start+3,x
        beq     @3087
        jmp     @3136

; 1 (if greater)
@306b:  cmp     #$40
        bne     @307b
        lda     $0500,y
        cmp     f:NPCScript::Start+3,x
        bcs     @3087
        jmp     @3136

; 2 (if less)
@307b:  lda     $0500,y
        cmp     f:NPCScript::Start+3,x
        jcs     @3136

@3087:  ldx     $29         ; skip 4 bytes
        inx4
        stx     $29
        jmp     @2fe7

; command $f9: if character data xxxx <=> yyyy
@3092:  cmp     #$f9
        bne     @30de
        longa
        lda     f:NPCScript::Start+1,x
        and     #$3fff
        tay
        lda     f:NPCScript::Start+1,x
        and     #$c000
        bne     @30b9
        lda     $0500,y     ; character data
        cmp     f:NPCScript::Start+3,x
        beq     @3087
        lda     $06
        shorta
        jmp     @3136

@30b9:  .a16
        cmp     #$4000
        bne     @30ce
        lda     $0500,y
        cmp     f:NPCScript::Start+3,x
        bcs     @30fc
        lda     $06
        shorta
        jmp     @3136
@30ce:  lda     $0500,y
        cmp     f:NPCScript::Start+3,x
        bcc     @30fc
        lda     $06
        shorta
        jmp     @3136

; command $f8: if character data xxxx & yy
@30de:  cmp     #$f8
        bne     @310c
        longa
        lda     f:NPCScript::Start+1,x   ; pointer to character data
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y     ; character data
        and     f:NPCScript::Start+3,x   ; mask
        bne     @3087
        jmp     @3136

@30fc:  lda     $06         ; skip 5 bytes
        shorta
        ldx     $29
        inx5
        stx     $29
        jmp     @2fe7

; command $f7: if party is facing xx
@310c:  cmp     #$f7
        bne     @311c
        lda     f:NPCScript::Start+1,x
        cmp     $0adb       ; facing direction
        bne     @3136
        jmp     @303c

; command $f5: if character data xxxx & yyyy
@311c:  longa
        lda     f:NPCScript::Start+1,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y
        and     f:NPCScript::Start+3,x
        jeq     @3087

; condition not met, check next condition
@3136:  ldx     $29
@3138:  inx
        lda     f:NPCScript::Start,x
        cmp     #$ff
        bne     @3138
        inx3
        stx     $29
        cpx     $d0
        jne     @2fe7
@314d:  lda     #$01
        sta     $57
        stz     $ba
        longa
        ldx     $29
        lda     f:NPCScript::Start+1,x   ; event index
        sta     $ce
        lda     $06
        shorta
        ldx     $ce
        beq     @3168
        jsr     ExecEvent
@3168:  stz     $57
        rts

.endproc

; number of bytes for each npc script command ($f0-$ff)
NPCCmdNumBytes:
@316b:  .byte   0,0,0,0,0,4,1,2,4,5,4,2,2,2,2,3

; ---------------------------------------------------------------------------

; [ check npc events ]

CheckNPCEvents:
@317b:  lda     $0adb       ; facing direction
        asl2
        tax
        lda     f:_c0329b,x   ; pointer to forward tile
        tay
        lda     $10f3,y     ; tile properties byte 2
        cmp     #$ff
        bne     @31a0       ; branch if not a through-tile
        lda     f:_c0329b+3,x   ; forward x2 (interact through tile)
        tay
        jsr     _c0324b       ; get object at tile
        cmp     #$ff
        bne     @319a       ; return if no object
        rts
@319a:  jsr     _c03289       ; get pointer to object
        jmp     @3230
@31a0:  jsr     _c0324b       ; get object at tile
        cmp     #$ff
        beq     @31ad       ; branch if no object
        jsr     _c03289       ; get pointer to object
        jmp     @3230
@31ad:  lda     f:_c0329b+1,x
        tay
        jsr     _c0324b
        cmp     #$ff
        beq     @31d6
        jsr     _c03289
        lda     $147b,x
        bmi     @31d6
        lda     $0adb
        dec
        and     #$03
        sta     $08
        lda     $147c,x
        lsr
        and     #$03
        cmp     $08
        jeq     @3230
@31d6:  lda     $0adb
        asl2
        tax
        lda     f:_c0329b+2,x
        tay
        jsr     _c0324b
        cmp     #$ff
        beq     @3205
        jsr     _c03289
        lda     $147b,x
        bmi     @3205
        lda     $0adb
        inc
        and     #$03
        sta     $08
        lda     $147c,x
        lsr
        and     #$03
        cmp     $08
        jeq     @3230
@3205:  lda     $0adb
        asl2
        tax
        lda     f:_c0329b+3,x
        tay
        jsr     _c0324b
        cmp     #$ff
        beq     @322d
        jsr     _c03289
        lda     $147b,x
        bmi     @322d
        lda     $147c,x
        lsr
        and     #$03
        cmp     $0adb
        jeq     @3230
@322d:  jmp     @324a
@3230:  lda     $147e,x     ; check if object faces party when active
        bmi     @3240
        lda     $0adb       ; party facing direction
        inc2
        and     #$03
        asl
        sta     $147c,x     ; set graphic frame
@3240:  phx
        jsr     _c039b3       ; update object sprites
        plx
        jsr     ExecNPCScript
        stz     $e4         ;
@324a:  rts

; ---------------------------------------------------------------------------

; [ get object at tile ]

; y: pointer to tile properties

_c0324b:
@324b:  lda     $10fa       ; map tile properties byte 1
        and     #$04
        beq     @3261       ; branch if not a bridge tile

; party is on a bridge tile
        lda     $c3
        and     #$01
        beq     @3286       ; ignore object if party is below bridge
        lda     $10f2,y     ; map tile properties byte 1
        and     #$01
        beq     @3286       ; ignore if tile is not passable to upper z-level
        bra     @327c

; party is not on a bridge tile
@3261:  lda     $10f2,y     ; map tile properties byte 1
        and     #$04
        beq     @3271       ; branch if target tile is not a bridge tile
        lda     $c2
        and     #$01
        beq     @3286
        jmp     @327c
@3271:  lda     $10f2,y     ; map tile properties byte 1
        and     #$03
        beq     @327c
        and     $c2
        beq     @3286
@327c:  lda     $10d8,y
        beq     @3286
        cmp     #$ff
        beq     @3286
        rts
@3286:  lda     #$ff        ; no object
        rts

; ---------------------------------------------------------------------------

; [ get pointer to object data ]

_c03289:
@3289:  and     #$7f
        sta     hWRMPYA
        lda     #$14
        sta     hWRMPYB
        nop4
        ldx     hRDMPYL
        rts

; ---------------------------------------------------------------------------

; pointers to tiles in direction (forward, forward/left, forward/right, forward x2 )
_c0329b:
@329b:  .byte   $02,$00,$04,$12
@329f:  .byte   $0a,$04,$10,$16
@32a3:  .byte   $0e,$10,$0c,$18
@32a7:  .byte   $06,$0c,$00,$14

; ---------------------------------------------------------------------------

; [ update objects ]

_c032ab:
@32ab:  lda     $e6         ; number of objects
        bne     @32b0       ; return if there are no objects
        rts
@32b0:  stz     $e5         ; current object
        ldy     $06
        sty     $e9         ; pointer to current object
@32b6:  ldy     $e9
        lda     $147e,y
        and     #$04
        beq     @32ca       ; branch if object is not stationary
        lda     $147b,y
        ora     #$80
        sta     $147b,y     ; object does not move
        jmp     @348a
@32ca:  lda     $147e,y     ;
        and     #$08
        bne     @32d9
        lda     $147d,y
        jmi     @34b9
@32d9:  lda     $147d,y
        and     #$7f
        jne     @34b9
        lda     $1485,y
        and     #$fd
        sta     $1485,y
        lda     $e4
        beq     @32ff
        and     #$7f
        cmp     $e5
        bne     @32ff
        stz     $e4
        lda     $1485,y
        ora     #$02
        sta     $1485,y
@32ff:  longa
        lda     $1477,y
        and     #$3f00
        xba
        sta     $0d
        lda     $1479,y
        lsr
        lsr
        and     #$0fc0
        ora     $0d
        tax
        tay
        lda     $06
        shorta
        lda     #$7f
        pha
        plb
        longa
        lda     $3000,x     ; bg1 tile
        sta     $f5
        lda     a:$0000,x     ; object
        sta     $eb
        tya
        sec
        sbc     #$0040
        and     #$0fff
        tax
        lda     $3000,x
        sta     $f7
        lda     a:$0000,x
        sta     $ed
        tya
        and     #$0fc0
        sta     $0d
        tya
        dec
        and     #$003f
        ora     $0d
        tax
        lda     $3000,x
        sta     $fd
        lda     a:$0000,x
        sta     $f3
        tya
        and     #$0fc0
        sta     $0d
        tya
        inc
        and     #$003f
        ora     $0d
        tax
        lda     $3000,x
        sta     $f9
        lda     a:$0000,x
        sta     $ef
        tya
        clc
        adc     #$0040
        and     #$0fff
        tax
        lda     $3000,x
        sta     $fb
        lda     a:$0000,x
        sta     $f1
        lda     $06
        shorta
        lda     #$00
        pha
        plb
        lda     $eb
        longa
        and     #$00ff
        asl
        tax
        lda     $06
        shorta
        lda     $1186,x     ; tile properties byte 1
        and     #$83
        sta     $0c         ; $0c = z-level and npc passability
        lda     $1187,x     ; tile properties byte 2
        and     #$0f
        sta     $0b         ; $0b = direction passability
        ldy     $e9
        lda     $147b,y     ;
        and     #$7f
        sta     $147b,y
        lda     $1485,y
        bpl     @33c2       ; branch if random movement does not depend on party position
        and     #$40
        bne     @33bc
        jsr     _c0351c       ; move toward party
        jmp     @342c
@33bc:  jsr     _c03555       ; move away from party
        jmp     @342c
@33c2:  lda     $1485,y     ; movement type
        and     #$30
        bne     @342c       ; branch if object has custom movement
        lda     $147b,y
        and     #$7f
        bne     @33db       ; branch if already moving
        jsr     Rand
        and     #$03
        inc
        sta     $147b,y     ; random movement direction
        bra     @342c
@33db:  jsr     Rand
        cmp     #$20
        bcs     @342c
        lda     $147b,y
        lsr
        bcc     @340a
        jsr     Rand
        lsr
        bcs     @33fc
        ldx     #$0004
        jsr     _c034eb       ; check if npc can move to tile
        beq     @342c
        sta     $147b,y
        jmp     @348a
@33fc:  ldx     #$0008
        jsr     _c034eb       ; check if npc can move to tile
        beq     @342c
        sta     $147b,y
        jmp     @348a
@340a:  jsr     Rand
        lsr
        bcs     @341e
        ldx     #$0002
        jsr     _c034eb       ; check if npc can move to tile
        beq     @342c
        sta     $147b,y
        jmp     @348a
@341e:  ldx     #$0006
        jsr     _c034eb       ; check if npc can move to tile
        beq     @342c
        sta     $147b,y
        jmp     @348a
@342c:  lda     $147b,y     ; facing direction
        asl
        tax
        jsr     _c034eb       ; check if npc can move to tile
        bne     @348a
        lda     $1485,y     ; movement type
        and     #$30
        cmp     #$10
        beq     @346b       ; branch if object moves in a circle
        cmp     #$30
        beq     @3459       ; branch if object moves in a straight line

; move back and forth
        lda     $147b,y     ; facing direction
        dec
        clc
        adc     #$02        ; reverse direction
        and     #$03
        inc
        asl
        tax
        jsr     _c034eb       ; check if npc can move to tile
        beq     @347f
        sta     $147b,y
        bra     @348a

; move in a straight line
@3459:  lda     $147b,y     ; facing direction
        and     #$03
        inc
        asl
        tax
        jsr     _c034eb       ; check if npc can move to tile
        beq     @347f
        sta     $147b,y
        bra     @348a

; move in a circle
@346b:  lda     $147b,y     ; facing direction
        dec2
        and     #$03
        inc
        asl
        tax
        jsr     _c034eb       ; check if npc can move to tile
        beq     @347f
        sta     $147b,y
        bra     @348a
@347f:  lda     $147b,y
        ora     #$80
        sta     $147b,y
        jmp     @34b9
@348a:  lda     $1485,y
        lsr
        jcc     @34b9
        ldy     $e9
        jsr     _c03cf8       ; remove object from object layout
        lda     $147b,y
        bpl     @349f
        lda     #$00
@349f:  and     #$7f
        tax
        lda     $1478,y
        clc
        adc     f:_c034e1,x
        sta     $75
        lda     $147a,y
        clc
        adc     f:_c034e6,x
        sta     $76
        jsr     _c03ce0       ; add object to object layout
@34b9:  ldy     $e9
        lda     $147b,y
        and     #$7f
        beq     @34c7
        dec
        asl
        sta     $147c,y
@34c7:  longa       ; next object
        lda     $e9
        clc
        adc     #$0014
        sta     $e9
        lda     $06
        shorta
        inc     $e5
        lda     $e5
        cmp     $e6
        jne     @32b6
        rts

_c034e1:
@34e1:  .lobytes   0,0,+1,0,-1

_c034e6:
@34e6:  .lobytes   0,-1,0,+1,0

; ---------------------------------------------------------------------------

; [ check if npc can move to tile ]

; x: direction
; a: movement direction (out, zero if can't move)

_c034eb:
@34eb:  lda     $eb,x       ; bg1 tile
        phx
        longa
        and     #$00ff
        asl
        tax
        lda     $06
        shorta
        lda     $1186,x     ; tile properties
        plx
        and     #$83
        cmp     $0c         ; check z-level and npc passability
        bne     @3514
        lda     $f5,x       ; object at tile
        bne     @3514
        txa
        lsr
        tax
        lda     $0b         ; check direction passability
        and     f:_c03517,x
        beq     @3514
        txa                 ; can move (return movement direction)
        rts
@3514:  lda     #0              ; can't move
        rts

_c03517:
@3517:  .byte   $00,$08,$01,$04,$02

; ---------------------------------------------------------------------------

; [ move toward party ]

_c0351c:
@351c:  ldy     $e9
        lda     $1478,y
        cmp     $0ad8
        bcs     @3536
        lda     $147a,y
        cmp     $0ad9
        bcs     @3532
        lda     #$01
        bra     @3544
@3532:  lda     #$00
        bra     @3544
@3536:  lda     $147a,y
        cmp     $0ad9
        bcs     @3542
        lda     #$02
        bra     @3544
@3542:  lda     #$03
@3544:  sta     $08
        jsr     Rand
        and     #$01
        clc
        adc     $08
        and     #$03
        inc
        sta     $147b,y
        rts

; ---------------------------------------------------------------------------

; [ move away from party ]

_c03555:
@3555:  ldy     $e9
        lda     $1478,y     ; object x position
        cmp     $0ad8
        bcs     @356f       ; branch if greater than party x position
        lda     $147a,y
        cmp     $0ad9
        bcs     @356b
        lda     #$03
        bra     @357d
@356b:  lda     #$02
        bra     @357d
@356f:  lda     $147a,y
        cmp     $0ad9
        bcs     @357b
        lda     #$00
        bra     @357d
@357b:  lda     #$01
@357d:  sta     $08
        jsr     Rand
        and     #$01
        clc
        adc     $08
        and     #$03
        inc
        sta     $147b,y
        rts

; ---------------------------------------------------------------------------

; [ update hiryuu sprites ]

.proc DrawHiryuu

@358e:  lda     $169f
        beq     @359a       ; return if no hiryuu
        lda     $1485
        and     #$01
        bne     @359b
@359a:  rts
@359b:  lda     #$aa
        sta     $0400
        sta     $0401
        sta     $0402
        sta     $0403
        lda     #$a0
        sta     $041f
        lda     $147c
        lsr3
        sta     $0c
        lda     $147e
        and     #$04
        beq     @35c5
        lda     #$01
        sta     $0c
        lda     #$07
        bra     @35f5
@35c5:  lda     $1485
        and     #$08
        beq     @35dd
        lda     $147b
        beq     @35f5
        lda     $1479
        and     #$80
        clc
        rol2
        adc     #$08
        bra     @35f5
@35dd:  lda     $1485
        bpl     @35f0
        lda     $3e
        lsr3
        and     #$03
        tax
        lda     f:_c037bd,x
        bra     @35f5
@35f0:  lda     $147c       ; body frame
        and     #$07
@35f5:  longa
        asl4
        sta     $23
        asl
        clc
        adc     $23
        tax
        lda     $1477
        lsr3
        clc
        adc     #$0010
        sec
        sbc     $61
        sta     $13
        lda     $1479
        lsr3
        clc
        adc     #$0018
        sec
        sbc     $63
        sta     $15
        lda     $1488
        beq     @3652
        lda     $1485
        and     #$0004
        bne     @3636
        lda     $1488
        and     #$00ff
        lsr
        bra     @363e
@3636:  lda     $1488
        and     #$00ff
        lsr2
@363e:  phx
        tax
        lda     f:_c022db,x
        plx
        and     #$00ff
        asl
        sta     $0f
        lda     $15
        sec
        sbc     $0f
        sta     $15
@3652:  lda     $06
        tay
        shorta
@3657:  longa
        cpy     #$0000
        bne     @3671
        lda     $0c
        and     #$0007
        phx
        asl
        tax
        lda     f:_c037c9,x
        plx
        ora     f:_c037d3+2,x
        bra     @3675
@3671:  lda     f:_c037d3+2,x
@3675:  sta     $0202,y
        and     #$01ff
        beq     @36b6
        lda     f:_c037d3,x
        and     #$00ff
        cmp     #$0080
        bcc     @368c
        ora     #$ff00
@368c:  adc     $13
        and     #$07ff
        cmp     #$0200
        bcs     @36b6
        lsr
        sec
        sbc     #$0008
        sta     $0d
        lda     f:_c037d3+1,x
        and     #$00ff
        cmp     #$0080
        bcc     @36ac
        ora     #$ff00
@36ac:  adc     $15
        and     #$07ff
        cmp     #$01e0
        bcc     @36bd
@36b6:  lda     #$00f0
        sta     $0f
        bra     @36d5
@36bd:  lsr
        sec
        sbc     #$0010
        sta     $0f
        cpy     #$0000
        bne     @36d5
        lda     $0c
        and     #$0008
        lsr2
        clc
        adc     $0f
        sta     $0f
@36d5:  lda     $06
        shorta
        lda     $0d
        cmp     #$f8
        bcc     @36f9
        phx
        phy
        tya
        lsr2
        and     #$03
        tax
        tya
        lsr4
        tay
        lda     $0400,y
        ora     f:_c037b9,x
        sta     $0400,y
        ply
        plx
@36f9:  lda     $0d
        sta     $0200,y
        lda     $0f
        sta     $0201,y
        inx4
        iny4
        cpy     #$0030
        jne     @3657
        longa
        lda     $148b
        lsr3
        clc
        adc     #$0010
        sec
        sbc     $61
        sta     $13
        lda     $148d       ; poison damage
        lsr3
        clc
        adc     #$0018
        sec
        sbc     $63
        sta     $15
        lda     $06
        shorta
        ldx     $06
        ldy     $06
@373b:  longa
        lda     f:_c037c1+2,x
        sta     $03fa,y
        lda     f:_c037c1,x
        and     #$00ff
        cmp     #$0080
        bcc     @3753
        ora     #$ff00
@3753:  adc     $13
        and     #$07ff
        cmp     #$0200
        bcs     @3775
        lsr
        sec
        sbc     #$0008
        sta     $0d
        lda     f:_c037c1+1,x
        and     #$00ff
        adc     $15
        and     #$07ff
        cmp     #$01e0
        bcc     @377c
@3775:  lda     #$00f0
        sta     $0f
        bra     @3783
@377c:  lsr
        sec
        sbc     #$0010
        sta     $0f
@3783:  lda     $06
        shorta
        lda     $0d
        cmp     #$f8
        bcc     @37a1
        cpy     #$0000
        bne     @3799
        lda     $041f
        ora     #$10
        bra     @379e
@3799:  lda     $041f
        ora     #$40
@379e:  sta     $041f
@37a1:  lda     $0d
        sta     $03f8,y
        lda     $0f
        sta     $03f9,y
        iny4
        inx4
        cpy     #8
        bne     @373b
        rts

.endproc

_c037b9:
        .byte   $01,$04,$10,$40

_c037bd:
        .byte   $05,$04,$06,$06

_c037c1:
        .word   $0af0,$34be,$0a10,$74be

_c037c9:
        .word   $34b6,$34ba,$34bc,$74ba,$34b8

; hiryuu sprite data (8 frames, 12 sprites per frame, 4 bytes per sprite)
_c037d3:
        .byte   $00,$e8,$00,$00
        .byte   $f0,$e0,$50,$34
        .byte   $10,$e0,$50,$74
        .byte   $f0,$00,$52,$34
        .byte   $10,$00,$52,$74
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$e8,$00,$00
        .byte   $d0,$e0,$54,$34
        .byte   $f0,$e0,$58,$34
        .byte   $10,$e0,$58,$74
        .byte   $30,$e0,$54,$74
        .byte   $d0,$00,$56,$34
        .byte   $f0,$00,$5a,$34
        .byte   $10,$00,$5a,$74
        .byte   $30,$00,$56,$74
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$e8,$00,$00
        .byte   $f0,$c0,$70,$34
        .byte   $10,$c0,$70,$74
        .byte   $d0,$d0,$5c,$34
        .byte   $30,$d0,$5c,$74
        .byte   $f0,$e0,$72,$34
        .byte   $10,$e0,$72,$74
        .byte   $d0,$f0,$5e,$34
        .byte   $30,$f0,$5e,$74
        .byte   $f0,$00,$74,$34
        .byte   $10,$00,$74,$74
        .byte   $00,$00,$00,$34

        .byte   $00,$e0,$00,$00
        .byte   $f0,$d0,$7c,$34
        .byte   $10,$d0,$7c,$74
        .byte   $d0,$e0,$7a,$34
        .byte   $30,$e0,$7a,$74
        .byte   $f0,$f0,$7e,$34
        .byte   $10,$f0,$7e,$74
        .byte   $03,$00,$96,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$d0,$00,$00
        .byte   $f0,$bc,$92,$34
        .byte   $10,$bc,$92,$74
        .byte   $d0,$dc,$90,$34
        .byte   $f0,$dc,$94,$34
        .byte   $10,$dc,$94,$74
        .byte   $30,$dc,$90,$74
        .byte   $00,$fc,$96,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$cc,$00,$00
        .byte   $f0,$b8,$9a,$34
        .byte   $10,$b8,$9a,$74
        .byte   $d0,$d8,$98,$34
        .byte   $f0,$d8,$9c,$34
        .byte   $10,$d8,$9c,$74
        .byte   $30,$d8,$98,$74
        .byte   $00,$f8,$96,$74
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$d4,$00,$00
        .byte   $f0,$c0,$b0,$34
        .byte   $10,$c0,$b0,$74
        .byte   $d0,$d0,$9e,$34
        .byte   $30,$d0,$9e,$74
        .byte   $f0,$e0,$b2,$34
        .byte   $10,$e0,$b2,$74
        .byte   $00,$00,$96,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$00,$00,$00
        .byte   $00,$e0,$b4,$34
        .byte   $00,$00,$76,$34
        .byte   $f0,$e0,$50,$34
        .byte   $10,$e0,$50,$74
        .byte   $f0,$00,$52,$34
        .byte   $10,$00,$52,$74
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$e8,$00,$00
        .byte   $f0,$e0,$50,$34
        .byte   $10,$e0,$50,$74
        .byte   $f0,$00,$52,$34
        .byte   $10,$00,$78,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

        .byte   $00,$e8,$00,$00
        .byte   $f0,$e0,$50,$34
        .byte   $10,$e0,$50,$74
        .byte   $f0,$00,$78,$74
        .byte   $10,$00,$52,$74
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34
        .byte   $00,$00,$00,$34

; ---------------------------------------------------------------------------

; [ update object sprites ]

_c039b3:
@39b3:  lda     $e6
        bne     @39b8
        rts
@39b8:  ldy     $06
        sty     $e9
        sty     $23
        stz     $e5
        lda     $e6
@39c2:  pha
        ldy     $e9
        lda     $169f
        beq     @39d2       ; branch if no hiryuu
        cpy     #$0028
        jcc     @3b67
@39d2:  lda     $1485,y
        and     #$01
        jeq     @3b67
        lda     $147e,y     ; animation type
        and     #$70
        beq     @3a0b
        cmp     #$50
        beq     @3a0b

; special animation
        lsr2
        sta     $08
        lda     $147e,y     ; animation speed
        and     #$03
        tax
        lda     f:_c03b88,x
        tax
        lda     $3e
@39f8:  lsr
        dex
        bne     @39f8
        and     #$03
        ora     $08
        tax
        lda     f:_c03b8c,x
        clc
        adc     $1487,y     ; frame offset
        bra     @3a60

; walking animation
@3a0b:  lda     $147c,y
        and     #$7f
        cmp     #$10
        bcc     @3a18
        cmp     #$40
        bcc     @3a60
@3a18:  lda     $1488,y
        bne     @3a47
        lda     $1485,y
        and     #$08
        beq     @3a47
        stz     $08
        lda     $147e,y
        and     #$08
        bne     @3a32
        lda     $147d,y
        bmi     @3a3e
@3a32:  lda     $147d,y
        and     #$20
        clc
        rol4
        sta     $08
@3a3e:  lda     $147c,y
        and     #$7f
        ora     $08
        bra     @3a60
@3a47:  lda     $147c,y
        bmi     @3a50
        and     #$7f
        bra     @3a60
@3a50:  lda     $147d,y
        and     #$20
        lsr5
        clc
        adc     $147c,y
        and     #$7f
@3a60:  sta     $08
        cmp     #$08
        bcs     @3a76
        lda     $147e,y
        and     #$70
        cmp     #$50
        bne     @3a76
        lda     $08
        clc
        adc     #$40
        sta     $08
@3a76:  lda     $08
        longa
        asl4
        tax
        ldy     $e9
        lda     $1477,y
        lsr3
        clc
        adc     #$0010
        sec
        sbc     $61
        and     #$07ff
        cmp     #$0200
        jcs     @3b67
        lsr
        sta     $0d
        phx
        lda     $1485,y
        and     #$0004
        bne     @3aae
        lda     $1488,y
        and     #$00ff
        lsr
        bra     @3ab6
@3aae:  lda     $1488,y
        and     #$00ff
        lsr2
@3ab6:  tax
        lda     f:_c022db,x
        and     #$00ff
        asl
        sta     $0f
        plx
        lda     $1479,y
        lsr3
        clc
        adc     #$0010
        sec
        sbc     $63
        sec
        sbc     $0f
        and     #$07ff
        cmp     #$07f0
        bcs     @3ae2
        cmp     #$01e0
        jcs     @3b67
@3ae2:  lsr
        sta     $0f
        lda     $1477,y
        lsr6
        and     #$0001
        clc
        adc     $0f
        sta     $0f
        lda     $06
        shorta
        ldy     $23
        lda     $0d
        sec
        sbc     $10a0
        sta     $0244,y     ; x position
        sta     $031c,y
        sec
        sbc     #$08
        sta     $0240,y
        sta     $0318,y
        lda     $0f
        sec
        sbc     $10a2
        sec
        sbc     #$03
        sta     $0319,y     ; y position
        sta     $031d,y
        sec
        sbc     #$08
        sta     $0241,y
        sta     $0245,y
        longa
        ldy     $e9
        lda     f:_c02a95+2,x
        clc
        adc     $1481,y
        ldy     $23
        sta     $0242,y
        ldy     $e9
        lda     f:_c02a95+6,x
        clc
        adc     $1481,y
        ldy     $23
        sta     $0246,y
        ldy     $e9
        lda     f:_c02a95+10,x
        clc
        adc     $1483,y
        ldy     $23
        sta     $031a,y
        ldy     $e9
        lda     f:_c02a95+14,x
        clc
        adc     $1483,y
        ldy     $23
        sta     $031e,y
@3b67:  longa
        lda     $23
        clc
        adc     #$0008
        sta     $23
        lda     $e9
        clc
        adc     #$0014
        sta     $e9
        lda     $06
        shorta
        pla
        dec
        jne     @39c2
        jsr     DrawHiryuu
        rts

; animation speeds
_c03b88:
        .byte   3,4,5,2

_c03b8c:
        .byte   0,0,0,0                 ; 0: animation frames (unused)
        .byte   0,0,0,0                 ; 1: animation frames (1 frame)
        .byte   0,1,0,1                 ; 2: animation frames (2 frames)
        .byte   0,1,2,3                 ; 3: animation frames (4 frames)
        .byte   0,4,0,4                 ; 4: animation frames (h-flip)
        .byte   0,0,0,0                 ; 5: animation frames (unused)
        .byte   0,0,0,0                 ; 6: animation frames (unused)
        .byte   0,0,0,0                 ; 7: animation frames (unused)

; ---------------------------------------------------------------------------

; [ update object positions ]

_c03bac:
@3bac:  lda     $e6
        bne     @3bb1       ; return if there are no objects
        rts
@3bb1:  stz     $e5         ; current object
        ldy     $06
        sty     $e9         ; pointer to current object
@3bb7:  ldy     $e9
        lda     $57
        beq     @3bc5       ; branch if dialog window is not open
        lda     $1486,y
        bne     @3bde       ; branch if object is moving
        jmp     @3c74
@3bc5:  lda     $147b,y
        jmi     @3c48
        lda     $147e,y
        and     #$08
        bne     @3bde
        lda     $147d,y
        and     #$80
        jne     @3c48
@3bde:  lda     $147b,y
        and     #$7f
        jeq     @3c48
        ldy     $e9
        lda     $1485,y
        and     #$02
        bne     @3bf8
        lda     $147e,y     ; animation speed
        and     #$03
        bra     @3bfa
@3bf8:  lda     #$00
@3bfa:  asl
        tax
        lda     $147b,y
        dec
        beq     @3c0a
        dec
        beq     @3c19
        dec
        beq     @3c28
        bra     @3c37

; moving up
@3c0a:  longa
        lda     $1479,y
        sec
        sbc     f:_c03cab,x
        sta     $1479,y
        bra     @3c44

; moving right
@3c19:  longa
        lda     $1477,y
        clc
        adc     f:_c03cab,x
        sta     $1477,y
        bra     @3c44

; moving down
@3c28:  longa
        lda     $1479,y
        clc
        adc     f:_c03cab,x
        sta     $1479,y
        bra     @3c44

; moving left
@3c37:  longa
        lda     $1477,y
        sec
        sbc     f:_c03cab,x
        sta     $1477,y
@3c44:  lda     $06
        shorta
@3c48:  lda     $1485,y
        and     #$02
        bne     @3c56
        lda     $147e,y     ; animation speed
        and     #$03
        bra     @3c58
@3c56:  lda     #$00
@3c58:  tax
        lda     $147d,y     ;
        clc
        adc     f:_c03cb3,x
        sta     $147d,y
        lda     $1488,y
        beq     @3c74
        lda     $1488,y     ;
        sec
        sbc     f:_c03cb7,x
        sta     $1488,y
@3c74:  lda     $57
        beq     @3c91
        lda     $1486,y
        beq     @3c91
        lda     $147d,y
        and     #$7f
        bne     @3c91
        lda     $1488,y
        bne     @3c91
        lda     #$00        ; movement complete ???
        sta     $1486,y
        sta     $147b,y
@3c91:  longa                   ; next object
        lda     $e9
        clc
        adc     #$0014
        sta     $e9
        lda     $06
        shorta
        inc     $e5
        lda     $e5
        cmp     $e6
        jne     @3bb7
        rts

; movement speeds (normal, slowest, slow, fast)
_c03cab:
        .word   16,4,8,32

_c03cb3:
        .byte   $08,$02,$04,$10

_c03cb7:
        .byte   $04,$01,$02,$08

; ---------------------------------------------------------------------------

; [ get pointer to object layout ]

_c03cbb:
@3cbb:  lda     $76
        and     #$3f
        xba
        longa
        lsr
        lsr
        sta     $0d
        lda     $75
        and     #$003f
        ora     $0d
        tax
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ clear object layout ]

_c03cd3:
@3cd3:  ldx     #$1000
        lda     #$00
@3cd8:  sta     $7f2fff,x
        dex
        bne     @3cd8
        rts

; ---------------------------------------------------------------------------

; [ add object to object layout ]

_c03ce0:
@3ce0:  phx
        jsr     _c03cbb       ; get pointer to object layout
        longa
        txa
        sta     $1489,y
        lda     $06
        shorta
        lda     $e5
        ora     #$80
        sta     $7f3000,x
        plx
        rts

; ---------------------------------------------------------------------------

; [ remove object from object layout ]

_c03cf8:
        phx
        longa
        lda     $1489,y     ; pointer to object layout
        tax
        lda     $06
        shorta
        lda     #$00
        sta     $7f3000,x   ; clear location in object layout
        plx
        rts

; ---------------------------------------------------------------------------

; [ get object in object layout ]

_c03d0b:
        phx
        jsr     _c03cbb       ; get pointer to object layout
        lda     $7f3000,x
        plx
        rts

; ---------------------------------------------------------------------------

; [ get tile z-level (unused) ]

; a: tile index, z-level (out)

_c03d15:
        phx
        longa
        and     #$00ff
        asl
        tax
        lda     $06
        shorta
        lda     $1186,x     ; tile properties
        and     #$83
        plx
        rts

; ---------------------------------------------------------------------------

; [ load npc graphics ]

_c03d28:
@3d28:  lda     $e6
        bne     @3d2d
        rts
@3d2d:  jsr     _c03e98       ; get pointer to npc properties
        ldx     #$6500
        stx     $2e
        ldx     $06
        stx     $e9
        ldx     $e7
        stx     $29
        lda     #$00
@3d3f:  pha
        lda     #$da        ; graphics start in bank $da
        sta     $25
        ldx     $29
        pla
        pha
        beq     @3d6f
        lda     f:NPCProp::Start+2,x   ; npc graphics
        cmp     f:NPCProp::Start+2-7,x
        bne     @3d6f       ; branch if not the same as previous npc
        lda     $55
        bne     @3d6c
        longa
        ldx     $e9
        lda     $146d,x
        sta     $1481,x
        lda     $146f,x
        sta     $1483,x
        lda     $06
        shorta
@3d6c:  jmp     @3e78
@3d6f:  lda     f:NPCProp::Start+2,x
        cmp     #$ff
        beq     @3d95       ; branch if no graphics
        lda     $55
        bne     @3daa
        longa
        lda     $2e
        sec
        sbc     #$6000
        lsr4
        ldx     $e9
        sta     $1481,x
        sta     $1483,x
        lda     $06
        shorta
        bra     @3daa
@3d95:  lda     $55
        bne     @3da7
        ldx     $e9
        stz     $1481,x
        stz     $1482,x
        stz     $1483,x
        stz     $1484,x
@3da7:  jmp     @3e78
@3daa:  ldx     $29
        lda     f:NPCProp::Start+2,x

; $67-$68: hiryuu, 64 tiles each ($0800 bytes)
        cmp     #$67
        bcc     @3dd4       ; branch if not hiryuu
        sec
        sbc     #$67
        longa
        xba
        asl3
        clc
        adc     #$2a00
        sta     $23
        lda     $06
        shorta
        lda     #$04
        sta     $09
        inc     $25
        lda     #$01
        sta     $169f       ; set hiryuu flag
        bra     @3e50

; $52-$66: 16 tiles each ($0200 bytes)
@3dd4:  cmp     #$52
        bcc     @3ded
        sec
        sbc     #$52
        longa
        xba
        asl
        sta     $23
        lda     $06
        shorta
        lda     #$01
        sta     $09
        inc     $25
        bra     @3e50

; $4b-$51: main characters, 64 tiles each ($0800 bytes)
@3ded:  cmp     #$4b
        bcc     @3e18
        sec
        sbc     #$4b
        longa
        xba
        asl3
        clc
        adc     #$c800
        sta     $23
        lda     $06
        shorta
        ldx     $29
        lda     f:NPCProp::Start+1,x
        lsr6
        bne     @3e14
        lda     #$04
@3e14:  sta     $09
        bra     @3e50

; $32-$4a: 32 tiles each ($0400 bytes)
@3e18:  cmp     #$32
        bcc     @3e42
        sec
        sbc     #$32
        longa
        xba
        asl2
        clc
        adc     #$6400
        sta     $23
        lda     $06
        shorta
        ldx     $29
        lda     f:NPCProp::Start+1,x
        lsr6
        bne     @3e3e
        lda     #$02
@3e3e:  sta     $09
        bra     @3e50

; $00-$31: 16 tiles each ($0200 bytes)
@3e42:  longa
        xba
        asl
        sta     $23
        lda     $06
        shorta
        lda     #$01
        sta     $09
@3e50:  ldx     #$0200
        stx     $2c
@3e55:  jsr     _c04cbc       ; copy data to vram
        longa
        lda     $2e
        clc
        adc     #$0100
        sta     $2e
        lda     $23
        clc
        adc     #$0200
        sta     $23
        shorta
        xba
        bne     @3e71
        inc     $25
@3e71:  lda     #$00
        xba
        dec     $09
        bne     @3e55
@3e78:  longa
        lda     $e9
        clc
        adc     #$0014
        sta     $e9
        lda     $29
        clc
        adc     #$0007
        sta     $29
        lda     $06
        shorta
        pla
        inc
        cmp     $e6
        jne     @3d3f
        rts

; ---------------------------------------------------------------------------

; [ get pointer to npc properties ]

_c03e98:
@3e98:  longa
        lda     $110c       ; map index
        asl
        tax
        lda     f:NPCPropPtrs,x
        sta     $e7
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ load npcs ]

_c03eaa:
        jsr     _c03cd3       ; clear object layout
        longa
        lda     $110c
        asl
        tax
        lda     f:NPCPropPtrs,x
        sta     $e7
        lda     f:NPCPropPtrs+2,x
        sec
        sbc     f:NPCPropPtrs,x
        sta     hWRDIVL
        lda     $06
        shorta
        lda     #$07
        sta     hWRDIVB
        pha
        pla
        pha
        pla
        nop
        lda     hRDDIVL
        sta     $e6         ; number of objects
        bne     @3edc
        rts
@3edc:  jsr     _c03d28       ; load npc graphics
        ldy     $06
        sty     $e9
        stz     $e5
@3ee5:  ldy     $e9
        ldx     $e7
        lda     f:NPCProp::Start+6,x   ; palette
        and     #$07
        asl
        sta     $0f
        lda     f:NPCProp::Start+6,x   ; layer priority (top)
        and     #$08
        ora     #$10
        asl
        ora     $0f
        longa
        xba
        ora     $1481,y
        sta     $1481,y
        lda     $06
        shorta
        lda     f:NPCProp::Start+6,x   ; layer priority (bottom)
        and     #$10
        ora     #$20
        ora     $0f
        longa
        xba
        ora     $1483,y
        sta     $1483,y
        lda     $06
        shorta
        lda     f:NPCProp::Start+5,x   ; misc. flags
        sta     $147e,y
        and     #$70
        beq     @3f47       ; branch if walking animation
        cmp     #$50
        beq     @3f47       ; branch if animal animation
        phx
        lda     f:NPCProp::Start+6,x   ; action frame
        and     #$e0
        lsr5
        tax
        lda     f:_c04000,x
        sta     $1487,y
        plx
        bra     @3f63
@3f47:  lda     f:NPCProp::Start+6,x   ; action frame
        and     #$e0
        lsr5
        asl
        sta     $147c,y
        cmp     #$08
        bcc     @3f5e
        lda     #$00
        bra     @3f60
@3f5e:  lsr
        inc
@3f60:  sta     $147b,y     ; facing direction
@3f63:  longa
        lda     f:NPCProp::Start,x   ; npc script
        and     #$3fff
        sta     $147f,y
        lda     $06
        shorta
        lda     f:NPCProp::Start+3,x   ; x position
        and     #$3f
        longa
        xba
        sta     $1477,y
        lda     $06
        shorta
        lda     f:NPCProp::Start+4,x   ; y position
        and     #$3f
        longa
        xba
        sta     $1479,y
        lda     $06
        shorta
        lda     #$00
        sta     $147d,y     ; movement animation counter
        sta     $1486,y     ;
        sta     $1488,y     ; jump counter
        lda     f:NPCProp::Start+3,x
        and     #$c0
        sta     $1485,y
        lda     f:NPCProp::Start+4,x
        and     #$c0
        lsr2
        ora     $1485,y
        ora     #$08
        sta     $1485,y
        longa
        lda     $147f,y
        shorta
        phy
        jsr     _c0c9c1       ; get npc flag
        ply
        cmp     #$00
        beq     @3fde
        lda     $1478,y
        sta     $75
        lda     $147a,y
        sta     $76
        jsr     _c03ce0       ; add object to object layout
        ldy     $e9
        lda     $1485,y     ; make object visible
        ora     #$01
        sta     $1485,y
@3fde:  longa
        lda     $e9         ; next object
        clc
        adc     #$0014
        sta     $e9
        lda     $e7
        clc
        adc     #$0007
        sta     $e7
        lda     $06
        shorta
        inc     $e5
        lda     $e5
        cmp     $e6
        jne     @3ee5
        rts

; ---------------------------------------------------------------------------

_c04000:
        .byte   $08, $09, $0A, $0B, $0C, $22, $21, $0F

; ---------------------------------------------------------------------------

; [ convert color palettes to grayscale ]

_c04008:
        ldy     #$0008
@400b:  longa
        lda     $0c00,y
        and     #$001f
        sta     $0d
        lda     $0c00,y
        lsr5
        sta     $0f
        and     #$001f
        clc
        adc     $0d
        sta     $0d
        lda     $0f
        lsr5
        and     #$001f
        clc
        adc     $0d
        sta     hWRDIVL
        lda     $06
        shorta
        lda     #$03
        sta     hWRDIVB
        longa
        nop7
        lda     hRDDIVL
        asl5
        ora     hRDDIVL
        asl5
        ora     hRDDIVL
        sta     $0c00,y
        lda     $06
        shorta
        iny2
        cpy     #$0200
        bne     @400b
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0406b:
        sta     hWRMPYA
        ldy     $06
@4070:  longa
        lda     [$23],y
        sta     $0d
        lsr4
        and     #$003e
        sta     $0f
        lda     $0d
        xba
        lsr
        and     #$003e
        sta     $11
        lda     $0d
        and     #$001f
        asl
        sta     $0d
        lda     $06
        shorta
        lda     $0d
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        sta     $13
        lda     $11
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        asl
        asl
        and     #$7c
        sta     $14
        lda     $0f
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        longa
        xba
        lsr3
        ora     $13
        sta     $0c00,x
        lda     $06
        shorta
        inx2
        iny2
        cpy     $2c
        bne     @4070
        rts

; ---------------------------------------------------------------------------

; [ copy data to vram ]

_c040d8:
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$01
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        ldx     $16ad       ; vram destination
        stx     hVMADDL
        ldx     #$0800      ; size = $0800
        stx     hDMA0::SIZE
        ldx     #$7622      ; source = 7f/7622
        stx     hDMA0::ADDR
        lda     #$7f
        sta     hDMA0::ADDR_B
        lda     #$01
        sta     hMDMAEN
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04107:
@4107:  asl2
        tax
        lda     f:_c04171,x
        sta     $0d
        lda     f:_c04171+1,x
        sta     $0e
        lda     f:_c04171+2,x
        sta     $0f
        lda     f:_c04171+3,x
        sta     $10
        lda     #$7f
        pha
        plb
        ldy     $06
@4128:  lda     [$23],y
        and     $0d
        sta     $7622,y
        iny
        lda     [$23],y
        and     $0d
        sta     $7622,y
        iny
        lda     [$23],y
        and     $0e
        sta     $7622,y
        iny
        lda     [$23],y
        and     $0e
        sta     $7622,y
        iny
        lda     [$23],y
        and     $0f
        sta     $7622,y
        iny
        lda     [$23],y
        and     $0f
        sta     $7622,y
        iny
        lda     [$23],y
        and     $10
        sta     $7622,y
        iny
        lda     [$23],y
        and     $10
        sta     $7622,y
        iny
        cpy     $26
        bne     @4128
        lda     #$00
        pha
        plb
        rts

; ---------------------------------------------------------------------------

_c04171:
        .byte   $80,$00,$00,$00,$80,$00,$08,$00,$88,$00,$80,$00,$88,$00,$88,$00
        .byte   $a8,$00,$88,$00,$a8,$00,$8a,$00,$aa,$00,$8a,$00,$aa,$00,$aa,$00
        .byte   $aa,$40,$aa,$00,$aa,$44,$aa,$00,$aa,$44,$aa,$40,$aa,$44,$aa,$44
        .byte   $aa,$54,$aa,$44,$aa,$55,$aa,$44,$aa,$55,$aa,$54,$aa,$55,$aa,$55
        .byte   $ea,$55,$aa,$55,$ea,$55,$ae,$55,$ee,$55,$ae,$55,$ee,$55,$ee,$55
        .byte   $ee,$75,$ee,$55,$ee,$77,$ee,$55,$ee,$77,$ee,$57,$ee,$77,$ee,$77
        .byte   $fe,$77,$ee,$77,$ff,$77,$ee,$77,$ff,$77,$fe,$77,$ff,$77,$ff,$77
        .byte   $ff,$f7,$ff,$77,$ff,$ff,$ff,$77,$ff,$ff,$ff,$7f,$ff,$ff,$ff,$ff

; ---------------------------------------------------------------------------

; [  ]

_c041f1:
@41f1:  ldx     $06
@41f3:  lda     #$00
        sta     $1a58,x
        longa
        txa
        clc
        adc     #$0008
        tax
        lda     $06
        shorta
        cpx     #$0080
        bne     @41f3
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0420a:
        ldx     $06
        stx     $1a56
        lda     $3f
        lsr
        bcs     @4218
        ldy     $06
        bra     @421b
@4218:  ldy     #$0008
@421b:  sty     $23
@421d:  ldy     $23
        lda     $1a58,y
        bne     @4227
@4224:  jmp     @4440
@4227:  cmp     #$10
        bcs     @4224
        longa
        lda     $1a56
        lsr4
        tax
        lda     #$aaaa
        sta     $0400,x
        sta     $0402,x
        sta     $0408,x
        sta     $040a,x
        lda     $1a5a,y
        sta     $13
        lda     $1a5c,y
        sta     $15
        lda     $1a58,y
        and     #$000e
        asl2
        tax
        cpy     #$0040
        bcs     @4262
        tya
        and     #$0030
        bra     @4269
@4262:  tya
        and     #$0030
        ora     #$0080
@4269:  clc
        adc     $1a56
        tay
        lda     $06
        shorta
        lda     $53
        jeq     @43a8
        longa
        lda     $13
        sec
        sbc     #$0010
        sec
        sbc     $61
        and     #$07ff
        cmp     #$0200
        bcs     @42c6
        lsr
        sta     $0d
        lda     $15
        sec
        sbc     #$0010
        sec
        sbc     $63
        and     #$07ff
        cmp     #$07e0
        bcs     @42a5
        cmp     #$01e0
        bcs     @42c6
@42a5:  lsr
        sta     $0f
        lda     $06
        shorta
        lda     $0d
        sta     $0200,y
        lda     $0f
        sta     $0201,y
        lda     f:_c04488,x
        sta     $0202,y
        lda     f:_c04488+1,x
        sta     $0203,y
        longa
@42c6:  lda     $13
        clc
        adc     #$0010
        sec
        sbc     $61
        and     #$07ff
        cmp     #$0200
        bcs     @4311
        lsr
        sta     $0d
        lda     $15
        sec
        sbc     #$0010
        sec
        sbc     $63
        and     #$07ff
        cmp     #$07e0
        bcs     @42f0
        cmp     #$01e0
        bcs     @4311
@42f0:  lsr
        sta     $0f
        lda     $06
        shorta
        lda     $0d
        sta     $0204,y
        lda     $0f
        sta     $0205,y
        lda     f:_c04488+2,x
        sta     $0206,y
        lda     f:_c04488+3,x
        sta     $0207,y
        longa
@4311:  lda     $13
        sec
        sbc     #$0010
        sec
        sbc     $61
        and     #$07ff
        cmp     #$0200
        bcs     @435c
        lsr
        sta     $0d
        lda     $15
        clc
        adc     #$0010
        sec
        sbc     $63
        and     #$07ff
        cmp     #$07e0
        bcs     @433b
        cmp     #$01e0
        bcs     @435c
@433b:  lsr
        sta     $0f
        lda     $06
        shorta
        lda     $0d
        sta     $0208,y
        lda     $0f
        sta     $0209,y
        lda     f:_c04488+4,x
        sta     $020a,y
        lda     f:_c04488+5,x
        sta     $020b,y
        longa
@435c:  lda     $13
        clc
        adc     #$0010
        sec
        sbc     $61
        and     #$07ff
        cmp     #$0200
        bcs     @43a5
        lsr
        sta     $0d
        lda     $15
        clc
        adc     #$0010
        sec
        sbc     $63
        and     #$07ff
        cmp     #$07e0
        bcs     @4386
        cmp     #$01e0
        bcs     @43a5
@4386:  lsr
        sta     $0f
        lda     $06
        shorta
        lda     $0d
        sta     $020c,y
        lda     $0f
        sta     $020d,y
        lda     f:_c04488+6,x
        sta     $020e,y
        lda     f:_c04488+7,x
        sta     $020f,y
@43a5:  jmp     @443c
@43a8:  lsr     $14
        ror     $13
        lsr     $16
        ror     $15
        lda     $13
        sec
        sbc     #$08
        sta     $0200,y
        lda     $15
        sec
        sbc     #$08
        sta     $0201,y
        lda     f:_c04488,x
        clc
        adc     #$20
        sta     $0202,y
        lda     f:_c04488+1,x
        ora     #$01
        sta     $0203,y
        lda     $13
        clc
        adc     #$08
        sta     $0204,y
        lda     $15
        sec
        sbc     #$08
        sta     $0205,y
        lda     f:_c04488+2,x
        clc
        adc     #$20
        sta     $0206,y
        lda     f:_c04488+3,x
        ora     #$01
        sta     $0207,y
        lda     $13
        sec
        sbc     #$08
        sta     $0208,y
        lda     $15
        clc
        adc     #$08
        sta     $0209,y
        lda     f:_c04488+4,x
        clc
        adc     #$20
        sta     $020a,y
        lda     f:_c04488+5,x
        ora     #$01
        sta     $020b,y
        lda     $13
        clc
        adc     #$08
        sta     $020c,y
        lda     $15
        clc
        adc     #$08
        sta     $020d,y
        lda     f:_c04488+6,x
        clc
        adc     #$20
        sta     $020e,y
        lda     f:_c04488+7,x
        ora     #$01
        sta     $020f,y
@443c:  lda     $06
        shorta
@4440:  ldy     $23
        lda     $1a58,y
        beq     @444b
        dec
        sta     $1a58,y
@444b:  longa
        lda     $23
        clc
        adc     #$0010
        sta     $23
        tay
        lda     $06
        shorta
        cpy     #$0088
        beq     @4467
        cpy     #$0080
        jne     @421d
@4467:  rts

; ---------------------------------------------------------------------------

_c04468:
        .addr   0,+1,+2,+3,+4,+5,+6,+7
        .addr   0,-7,-6,-5,-4,-3,-2,-1

; ---------------------------------------------------------------------------

_c04488:
        .word   $341e,$f43e
        .word   $343e,$f41e
        .word   $341c,$f43c
        .word   $343c,$f41c
        .word   $341a,$f43a
        .word   $343a,$f41a
        .word   $3418,$f438
        .word   $3438,$f418
        .word   $3416,$f436
        .word   $3436,$f416
        .word   $3414,$f434
        .word   $3434,$f414
        .word   $3412,$f432
        .word   $3432,$f412
        .word   $3410,$f430
        .word   $3430,$f410

; ---------------------------------------------------------------------------

; [ show cutscene ]

; A: cutscene id

.proc ShowCutscene

        pha
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #0
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        pla
        jsl     ShowCutscene_ext
        jsr     _c044e3                       ; init map bank
        rts

.endproc

; ---------------------------------------------------------------------------

; [ init map bank ]

_c044e3:
@44e3:  longi
        shorta
        lda     #$00
        pha
        plb
        ldx     #$0b00
        phx
        pld
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #$8f
        sta     hINIDISP
        lda     #$00
        sta     hNMITIMEN
        lda     #$00
        xba
        jsr     InitInterrupts
        jsr     InitHardware
        rts

; ---------------------------------------------------------------------------

; [ init vehicles ]

_c0450a:
        stz     $0adc
        ldx     #$8000
        stx     $0add
        stx     $0ae1
        stx     $0ae5
        stx     $0ae9
        stx     $0aed
        stx     $0af1
        lda     #$02
        sta     $0ada
        rts

; ---------------------------------------------------------------------------

; [ init character names ]

_c04528:
        lda     #$ff
        ldy     $06
@452c:  sta     $0990,y
        iny
        cpy     #$001e
        bne     @452c
        lda     #$cb
        sta     $0990
        lda     #$ff
        sta     $0991
        sta     $0992
        sta     $0993
        sta     $0994
        sta     $0995
        rts

; ---------------------------------------------------------------------------

; [ open menu ]

OpenMenu:
        jsr     _c06081       ; fade out

OpenMenuNoFade:
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #$00
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     ExecMenu_ext
        jsr     _c044e3       ; init map bank
        jsr     UpdateTopChar
        rts

; ---------------------------------------------------------------------------

_c0456b:
        ldy     $06
@456d:  lda     $0500,y
        and     #$40
        beq     @4582
        longa
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        bra     @456d
@4582:  rts

; ---------------------------------------------------------------------------

; [  ]

_c04583:
        lda     #$fe
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        bne     @45e3
        jsr     _c04635                 ; stop sound
        lda     $0adc                   ; vehicles
        beq     @45cc
        cmp     #$06
        bne     @45b4
        lda     $0ad6                   ; map index
        cmp     #$02
        bne     @45b4
        lda     $0af1
        and     #$03
        bne     @45ab
        lda     #$69
        jsr     PlaySfx
@45ab:  lda     #$79
        jsr     _c0ca2f                   ; get event flag $00xx
        cmp     #$00
        bne     @45cc
@45b4:  lda     $0adc                   ; current vehicle
        asl2
        sta     $08
        tay
        lda     $0ad9,y
        and     #$03
        ora     $08
        tax
        lda     f:_c045ee,x
        beq     @45cc
        bra     @45e0
@45cc:  lda     #$7f
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        beq     @45d7
        lda     #$05
@45d7:  clc
        adc     $0ad6                   ; map index
        tax
        lda     f:_c045e4,x
@45e0:  jsr     PlaySong
@45e3:  rts

; ---------------------------------------------------------------------------

; default song for each vehicle
_c045e4:
        .byte   $23,$46,$23,$1e,$1e,$23,$27,$3f,$1e,$1e

; alt. songs for each vehicle (4 bytes each)
_c045ee:
        .byte   $00,$00,$00,$00
        .byte   $03,$00,$00,$00
        .byte   $1a,$00,$00,$00
        .byte   $1d,$00,$00,$00
        .byte   $1e,$1e,$1e,$1e
        .byte   $00,$18,$00,$00
        .byte   $28,$00,$1e,$00

; ---------------------------------------------------------------------------

.proc PlaySong

        sta     $08
        lda     $55
        cmp     #2
        beq     Done
        lda     $08
        sta     $1d01
        lda     #1
        sta     $1d00
        lda     #15
        sta     $1d03
        lda     $55
        cmp     #1
        beq     _462b
        lda     #$08
        bra     _462d
_462b:  lda     #$28
_462d:  sta     $1d02
        jsl     ExecSound_ext
Done:   rts

.endproc

; ---------------------------------------------------------------------------

; [ stop sound ]

_c04635:
        lda     #$f2        ; stop sound
        sta     $1d00
        bra     ExecSound_near

; ---------------------------------------------------------------------------

; [ play sound effect ]

; A: sound effect

.proc PlaySfx

        sta     $1d01
        lda     #$02        ; play sound effect
        sta     $1d00
        lda     #$0f        ; full volume, no envelope
        sta     $1d02
        lda     #$88
        sta     $1d03

ExecSound:
_464e:  jsl     ExecSound_ext
        rts

.endproc  ; PlaySfx

ExecSound_near := PlaySfx::ExecSound

; ---------------------------------------------------------------------------

; [ update screen pixelation (mosaic) ]

_c04653:
        lda     $16aa
        bmi     @468a
        beq     @4686       ; branch if no pixelation
        longa
        lda     $16aa
        inc
        and     #$00ff
        asl4
        sta     $19
        lda     $16ab
        sec
        sbc     $19
        sta     $16ab
        bpl     @467a
        stz     $16ab
        stz     $16aa
@467a:  lda     $06
        shorta
        lda     $16ac
        tax
        lda     f:EventMosaicTbl,x
@4686:  sta     hMOSAIC
@4689:  rts
@468a:  lda     $57
        bne     @4689
        lda     $ba
        beq     @4697
        lda     $41
        and     #$0e
        lsr
@4697:  tax
        lda     f:PoisonMosaicTbl,x
        bra     @4686

; ---------------------------------------------------------------------------

; screen pixelation data (poison)
PoisonMosaicTbl:
        .byte   $0f,$1f,$2f,$3f,$3f,$2f,$1f,$0f

; screen pixelation data (event)
EventMosaicTbl:
        .byte   $0f,$1f,$2f,$3f,$4f,$5f,$6f,$7f
        .byte   $8f,$9f,$af,$bf,$cf,$df,$ef,$ff
        .byte   $ef,$df,$cf,$bf,$af,$9f,$8f,$7f
        .byte   $6f,$5f,$4f,$3f,$2f,$1f

; ---------------------------------------------------------------------------

; [  ]

_c046c4:
        lda     #$70
        sta     $1879
        lda     #$7b
        sta     $187a
        ldy     $06
        stz     $09
@46d2:  lda     $09
        sta     $1880,y
        jsr     Rand
        and     #$01
        sta     $1882,y
        jsr     Rand
        and     #$07
        clc
        adc     #$18
        sta     $1883,y
        lda     #$00
        sta     $187f,y
        sta     $1881,y
        sta     $1884,y
        lda     $09
        asl
        tax
        longa
        lda     f:_c04721,x
        sta     $187b,y
        lda     $06
        shorta
        jsr     Rand
        and     #$30
        ora     #$c0
        sta     $187d,y
        lda     #$00
        sta     $187e,y
        inc     $09
        tya
        clc
        adc     #$0a
        tay
        cmp     #$a0
        bne     @46d2
        rts

; ---------------------------------------------------------------------------

_c04721:
        .addr   -128,-112,-96,-80,-64,-48,-32,-16
        .addr   +16,+32,+48,+64,+80,+96,+112,+128

; ---------------------------------------------------------------------------

_c04741:
        lda     $5b
        bne     @4746
        rts
@4746:  ldy     $06
@4748:  lda     $1883,y
        bne     @478e
        lda     $1884,y
        bne     @478e
        jsr     Rand
        and     #$0f
        sta     $1880,y
        jsr     Rand
        and     #$03
        clc
        adc     $6f
        adc     $6f
        sta     $1882,y
        jsr     Rand
        and     #$07
        clc
        adc     #$06
        sta     $1883,y
        longa
        lda     $06
        sta     $187b,y
        lda     #$0080
        sta     $187d,y
        lda     $06
        shorta
        lda     #$00
        sta     $187f,y
        sta     $1881,y
        sta     $1884,y
@478e:  tya
        clc
        adc     #$0a
        tay
        cmp     #$a0
        bne     @4748
        rts

; ---------------------------------------------------------------------------

_c04798:
        ldx     $06
@479a:  stz     $1883,x
        stz     $1884,x
        txa
        clc
        adc     #$0a
        tax
        cmp     #$a0
        bne     @479a
        rts

; ---------------------------------------------------------------------------

_c047aa:
        lda     $5b
        beq     @47f6
        ldy     $06
        sty     $17
        longa
@47b4:  lda     $17
        asl
        sta     $19
        asl2
        clc
        adc     $19
        tay
        ldx     $1883,y
        beq     @47e9
        lda     $187f,y
        clc
        adc     $187b,y
        sta     $187f,y
        lda     $1881,y
        clc
        adc     $187d,y
        sta     $1881,y
        lda     $1883,y
        dec
        sta     $1883,y
        lda     $187d,y
        sec
        sbc     #$0010
        sta     $187d,y
@47e9:  inc     $17
        lda     $17
        cmp     #$0010
        bne     @47b4
        lda     $06
        shorta
@47f6:  rts

; ---------------------------------------------------------------------------

_c047f7:
        ldx     $1873
        ldy     $06
@47fc:  lda     $1883,y
        bne     @4806
        lda     $1884,y
        beq     @4826
@4806:  lda     $1880,y
        clc
        adc     $1879
        sta     $0200,x
        lda     $1882,y
        eor     #$ff
        sec
        adc     $187a
        sta     $0201,x
        lda     #$47
        sta     $0202,x
        lda     #$30
        sta     $0203,x
@4826:  inx4
        tya
        clc
        adc     #$0a
        tay
        cmp     #$a0
        bne     @47fc
        rts

; ---------------------------------------------------------------------------

_c04834:
        lda     $0afb
        cmp     #$02
        beq     @483c
        rts
@483c:  lda     #$c8
        sta     $0201
        sta     $0205
        sta     $0209
        sta     $020d
        sta     $0211
        lda     #$c0
        sta     $0200
        lda     #$c8
        sta     $0204
        lda     #$d0
        sta     $0210
        lda     #$d8
        sta     $0208
        lda     #$e0
        sta     $020c
        lda     #$30
        sta     $0203
        sta     $0207
        sta     $020b
        sta     $020f
        sta     $0213
        ldx     $0afc
        stx     hWRDIVL
        lda     #$3c
        sta     hWRDIVB
        pha
        pla
        pha
        pla
        nop
        ldx     hRDDIVL
        stx     hWRDIVL
        lda     #$0a
        sta     hWRDIVB
        pha
        pla
        pha
        pla
        nop
        lda     hRDMPYL
        ora     #$40
        sta     $020e
        ldx     hRDDIVL
        stx     hWRDIVL
        lda     #$06
        sta     hWRDIVB
        pha
        pla
        pha
        pla
        nop
        lda     hRDMPYL
        ora     #$40
        sta     $020a
        ldx     hRDDIVL
        stx     hWRDIVL
        lda     #$0a
        sta     hWRDIVB
        pha
        pla
        pha
        pla
        nop
        lda     hRDMPYL
        ora     #$40
        sta     $0206
        lda     hRDDIVL
        ora     #$40
        sta     $0202
        lda     #$4f
        sta     $0212
        rts

; ---------------------------------------------------------------------------

; [ init npc flags ]

_c048dd:
        ldx     $06
@48df:  lda     f:InitNPCSwitch,x
        sta     $0a54,x
        inx
        cpx     #$0080
        bne     @48df
        rts

; ---------------------------------------------------------------------------

; [ init event flags ]

_c048ed:
        ldy     $06
        tya
@48f0:  sta     $0a14,y
        iny
        cpy     #$0040
        bne     @48f0
        rts

; ---------------------------------------------------------------------------

; [ init character data (new game) ]

_c048fa:
        ldx     $06
@48fc:  lda     f:CharProp,x
        sta     $0500,x
        inx
        cpx     #$0140
        bne     @48fc
        rts

; ---------------------------------------------------------------------------

; [ reset ram $0000-$1d00 ]

_c0490a:
        longa
        ldy     $06
        lda     $06
@4910:  sta     $0000,y
        iny2
        cpy     #$1d00
        bne     @4910
        shorta
        rts

; ---------------------------------------------------------------------------

; [ reset ram $0b00-$1d00 ]

_c0491d:
        longa
        ldy     #$0b00
        lda     $06
@4924:  sta     $0000,y
        iny2
        cpy     #$1d00
        bne     @4924
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04931:
        lda     $d3
        bne     @494c
        ldx     $06
        stx     $10a0
        stx     $10a2
        stx     $10a4
        stx     $10a6
        stx     $10a8
        stx     $10aa
        jmp     @49d2
@494c:  lda     $d3
        lsr6
        sta     $17
        lda     $3f
        and     $17
        bne     @49d2
        lda     $d3
        and     #$03
        tax
        lda     f:_c049d3,x
        sta     $17
        lda     $3f
        tax
        lda     f:RNGTbl,x
        and     $17
        sta     $10a0
        sta     $10a4
        sta     $10a8
        lda     $d3
        lsr2
        and     #$03
        tax
        lda     f:_c049d3,x
        sta     $17
        lda     $3f
        tax
        lda     f:RNGTbl+1,x
        and     $17
        sta     $10a2
        sta     $10a6
        sta     $10aa
        lda     $d3
        and     #$30
        lsr4
        beq     @49d2
        cmp     #$01
        bne     @49b4
        stz     $10a4
        stz     $10a6
        stz     $10a8
        stz     $10aa
        bra     @49d2
@49b4:  cmp     #$02
        bne     @49c6
        stz     $10a0
        stz     $10a2
        stz     $10a8
        stz     $10aa
        bra     @49d2
@49c6:  stz     $10a4
        stz     $10a6
        stz     $10a0
        stz     $10a2
@49d2:  rts

; ---------------------------------------------------------------------------

_c049d3:
        .byte   %0,%1,%11,%111

; ---------------------------------------------------------------------------

; [ init color addition ]

_c049d7:
        pha
        lda     $47         ; save previous values
        sta     $48
        lda     $49
        sta     $4a
        stz     $49
        lda     #$07        ; color addition
        sta     $47
        pla
        bra     _49f9

; ---------------------------------------------------------------------------

; [ init color subtraction ]

_c049e9:
        pha
        lda     $47         ; save previous values
        sta     $48
        lda     $49
        sta     $4a
        stz     $49
        lda     #$87        ; color subtraction
        sta     $47
        pla
_49f9:  jsr     _c04a44       ; set fixed color parameters
        stz     $46
        rts

; ---------------------------------------------------------------------------

; [ update fixed color ]

_c049ff:
        lda     $44         ; rate
        bpl     @4a1e       ; branch if decreasing
        and     #$7f
        clc
        adc     $46         ; increase intensity
        sta     $46
        lda     $4c
        and     #$1f
        asl3
        cmp     $46
        bcs     @4a33
        lda     $46
        and     #$f8
        sta     $46
        jmp     @4a33
@4a1e:  lda     $46
        beq     @4a29
        sec
        sbc     $44         ; decrease intensity
        sta     $46
        bra     @4a33
@4a29:  lda     $48         ; restore original fixed color values
        sta     $47
        lda     $4a
        sta     $49
        stz     $44
@4a33:  lda     $46
        lsr3
        sta     $17
        lda     $4c         ; color components
        and     #$e0
        ora     $17
        sta     hCOLDATA       ; fixed color
        rts

; ---------------------------------------------------------------------------

; [ set fixed color parameters ]

_c04a44:
@4a44:  pha
        pha
        and     #$e0        ; color components
        sta     $08
        pla
        and     #$07        ; intensity
        asl2
        clc
        adc     #$03
        ora     $08
        sta     $4c
        pla
        and     #$18        ; speed
        lsr3
        tax
        lda     f:_c04a64,x
        sta     $44         ; fixed color rate
        rts

; ---------------------------------------------------------------------------

_c04a64:
        .byte   $81,$82,$84,$84

; ---------------------------------------------------------------------------

; [  ]

_c04a68:
        lda     #$10
        sta     $43
        lda     #$10
        sta     $45
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04a71:
        lda     #$90
        sta     $43
        lda     #$f0
        sta     $45
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04a7a:
        lda     $43
        bmi     @4a92
        lda     $45
        and     #$f0
        cmp     #$f0
        beq     @4aa3
        lda     $43
        and     #$1f
        clc
        adc     $45
        sta     $45
        jmp     @4aa3
@4a92:  lda     $45
        beq     @4aa3
        lda     $43
        and     #$1f
        sta     $19
        lda     $45
        sec
        sbc     $19
        sta     $45
@4aa3:  lda     $45
        lsr4
        sta     hINIDISP
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04aad:
@4aad:  jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        lda     $02
        and     #$cf
        bne     @4aad
        lda     $03
        bne     @4aad
        rts

; ---------------------------------------------------------------------------

; [ wait for keypress ]

_c04ac1:
@4ac1:  jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        lda     $02
        and     #$cf
        bne     @4ad4
        lda     $03
        beq     @4ac1
@4ad4:  rts

; ---------------------------------------------------------------------------

; [ copy 3bpp graphics to vram ]

; +$30: source address (+$db0000)
; +$33: vram address
;  $35: tile count

_c04ad5:
        stz     hMDMAEN
        lda     #$80
        sta     hVMAINC
        lda     #$08
        sta     hDMA0::CTRL
        lda     #<hVMDATAH
        sta     hDMA0::HREG
        ldx     $33
        stx     hVMADDL
        ldx     #$0b06
        stx     hDMA0::ADDR
        stz     hDMA0::ADDR_B
        longa
        lda     $35
        asl3
        sta     $17
        asl
        clc
        adc     $17
        sta     hDMA0::SIZE
        lda     $06
        shorta
        lda     #$01
        sta     hMDMAEN
        stz     hMDMAEN
        ldx     $30
        stx     hDMA0::ADDR
        lda     #$db
        sta     hDMA0::ADDR_B
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        ldx     $33
        stx     hVMADDL
        ldy     $06
@4b27:  lda     #$80
        sta     hVMAINC
        lda     #$01
        sta     hDMA0::CTRL
        ldx     #$0010
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        stz     hMDMAEN
        stz     hVMAINC
        stz     hDMA0::CTRL
        ldx     #$0008
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        iny
        cpy     $35
        bne     @4b27
        rts

; ---------------------------------------------------------------------------

; [ load world map graphics ]

_c04b56:
        lda     #$80
        sta     hVMAINC
        ldx     $06
        stx     hVMADDL
        lda     $0ad6       ; map index
        tax
        lda     f:WorldTileAttrTbl,x
        sta     $24
        stz     $23
        ldx     $23
        ldy     $06
@4b70:  lda     f:WorldTileAttr,x
        sta     $1873,y
        inx
        iny
        cpy     #$0100
        bne     @4b70
        lda     $0ad6       ; map index
        tax
        lda     f:WorldTileAttrTbl,x
        asl5
        sta     $24
        stz     $23
        ldx     $23
        ldy     #$0000
@4b94:  lda     f:WorldGfx,x
        sta     $0a
        inx
        and     #$0f
        ora     $1873,y
        sta     hVMDATAH
        lda     $0a
        lsr4
        ora     $1873,y
        sta     hVMDATAH
        txa
        and     #$1f
        bne     @4b94
        iny
        cpy     #$0100
        bne     @4b94
        rts

; ---------------------------------------------------------------------------

; world map tilesets (identical to WorldTilesetTbl)
WorldTileAttrTbl:
        .byte   0,1,0,2,2

; ---------------------------------------------------------------------------

; [ update scrolling registers ]

_c04bc0:
        longa
        lda     $61
        lsr
        clc
        adc     $10a0
        sta     $17
        lda     $63
        lsr
        clc
        adc     $10a2
        sta     $19
        lda     $65
        sec
        sbc     $1098
        sta     $65
        lsr
        clc
        adc     $10a4
        sta     $1b
        lda     $67
        sec
        sbc     $109a
        sta     $67
        lsr
        clc
        adc     $10a6
        sta     $1d
        lda     $69
        sec
        sbc     $109c
        sta     $69
        lsr
        clc
        adc     $10a8
        sta     $1f
        lda     $0ad4
        cmp     #$be
        bne     @4c16
        lda     $3f
        lsr
        lsr
        bcs     @4c16
        lda     $1f
        eor     #$0100
        sta     $1f
@4c16:  lda     $6b
        sec
        sbc     $109e
        sta     $6b
        lsr
        clc
        adc     $10aa
        sta     $21
        lda     $06
        shorta
        lda     $17
        sta     hBG1HOFS
        lda     $18
        sta     hBG1HOFS
        lda     $19
        sta     hBG1VOFS
        lda     $1a
        sta     hBG1VOFS
        lda     $1b
        sta     hBG2HOFS
        lda     $1c
        sta     hBG2HOFS
        lda     $1d
        sta     hBG2VOFS
        lda     $1e
        sta     hBG2VOFS
        lda     $1f
        sta     hBG3HOFS
        lda     $20
        sta     hBG3HOFS
        lda     $21
        sta     hBG3VOFS
        lda     $22
        sta     hBG3VOFS
        longa
        lda     $17
        clc
        adc     #$0078
        sta     $17
        lda     $19
        clc
        adc     #$0078
        sta     $19
        lda     $06
        shorta
        lda     $17
        sta     hM7X
        lda     $18
        sta     hM7X
        lda     $19
        sta     hM7Y
        lda     $1a
        sta     hM7Y
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04c90:
        jsl     _c2a008     ; update joypad input
        rts

; ---------------------------------------------------------------------------

; [ clear sprite data ]

_c04c95:
        ldx     #$0200
        lda     #$f0
@4c9a:  sta     $01fd,x
        dex4
        bne     @4c9a
        ldx     #$0020
@4ca6:  stz     $03ff,x
        dex
        bne     @4ca6
        rts

; ---------------------------------------------------------------------------

; [ hide all sprites ]

_c04cad:
        ldx     #$0100
        lda     #$f0
@4cb2:  sta     $01fd,x
        dex4
        bne     @4cb2
        rts

; ---------------------------------------------------------------------------

; [ copy data to vram ]

; ++$23: source address
;  +$2c: size
;  +$2e: destination address (vram)

_c04cbc:
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$01
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        lda     $25
        sta     hDMA0::ADDR_B
        ldx     $2e
        stx     hVMADDL
        ldx     $23
        stx     hDMA0::ADDR
        ldx     $2c
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        rts

; ---------------------------------------------------------------------------

; [ disable interrupts ]

_c04ce8:
@4ce8:  stz     hMDMAEN
        stz     hHDMAEN
        lda     #$80
        sta     hINIDISP
        lda     #$00
        sta     hNMITIMEN
        sei
        rts

; ---------------------------------------------------------------------------

; [ enable interrupts ]

_c04cfa:
@4cfa:  lda     #$81
        sta     hNMITIMEN
        lda     #$00
        sta     hINIDISP
        cli
        rts

; ---------------------------------------------------------------------------

; [ clear vram ]

_c04d06:
        jsr     _c04ce8       ; disable interrupts
        stz     $0b6d
        jsr     FillVRAM
        jsr     _c04cfa       ; enable interrupts
        rts

; ---------------------------------------------------------------------------

; [ fill vram ]

; +$2c: size
; +$2e: start address
;  $6d: fill value

.proc FillVRAM
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$09
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        ldx     $2e
        stx     hVMADDL
        ldx     #$0b6d
        stx     hDMA0::ADDR
        stz     hDMA0::ADDR_B
        ldx     $2c
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        rts
.endproc

; ---------------------------------------------------------------------------

; [ copy sprite data to ppu ]

.proc TfrSprites
        lda     $5c
        bne     Done
        stz     hOAMADDL
        stz     hMDMAEN
        stz     hDMA0::CTRL
        lda     #<hOAMDATA
        sta     hDMA0::HREG
        ldx     #$0200
        stx     hDMA0::ADDR
        lda     #$00
        sta     hDMA0::ADDR_B
        ldx     #$0220
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
Done:   rts
.endproc

; ---------------------------------------------------------------------------

; [ copy color palettes to ppu ]

.proc TfrPal
        stz     hMDMAEN
        stz     hCGADD
        lda     #$02
        sta     hDMA0::CTRL
        lda     #<hCGDATA
        sta     hDMA0::HREG
        lda     #$00
        sta     hDMA0::ADDR_B
        ldx     #$0c00      ; source = 00/0c00
        stx     hDMA0::ADDR
        ldx     #$0200
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        rts
.endproc

; ---------------------------------------------------------------------------

; [  ]

_c04d8e:
        lda     $02
        and     #JOY_A
        bne     @4d97
        stz     $10b8
@4d97:  lda     $02
        and     #JOY_X
        bne     @4da0
        stz     $10b4
@4da0:  lda     $02
        and     #JOY_L
        bne     @4da9
        stz     $10b6
@4da9:  lda     $02
        and     #JOY_R
        bne     @4db2
        stz     $10b7
@4db2:  lda     $03
        and     #>JOY_B
        bne     @4dbb
        stz     $10b9
@4dbb:  lda     $03
        and     #>JOY_Y
        bne     @4dc4
        stz     $10b5
@4dc4:  lda     $03
        and     #>JOY_SELECT
        bne     @4dcd
        stz     $10ba
@4dcd:  lda     $03
        and     #>JOY_START
        bne     @4dd6
        stz     $10bb
@4dd6:  rts

; ---------------------------------------------------------------------------

; [  ]

_c04dd7:
        phx
        phy
        ldx     $06
@4ddb:  ldy     #$0000
        stz     $3a
@4de0:  longa
        lda     $37
        sec
        sbc     f:_c04e21,x
        sta     $37
        lda     $39
        sbc     f:_c04e31,x
        sta     $39
        bcc     @4df9
        iny
        jmp     @4de0
@4df9:  lda     $37
        clc
        adc     f:_c04e21,x
        sta     $37
        lda     $39
        adc     f:_c04e31,x
        sta     $39
        lda     $06
        shorta
        phx
        txa
        lsr
        tax
        tya
        sta     $10ac,x
        plx
        inx2
        cpx     #$0010
        bne     @4ddb
        ply
        plx
        rts

; ---------------------------------------------------------------------------

_c04e21:
        .word   .loword(10000000)
        .word   .loword(1000000)
        .word   .loword(100000)
        .word   .loword(10000)
        .word   .loword(1000)
        .word   .loword(100)
        .word   .loword(10)
        .word   .loword(1)

_c04e31:
        .word   .hiword(10000000)
        .word   .hiword(1000000)
        .word   .hiword(100000)
        .word   .hiword(10000)
        .word   .hiword(1000)
        .word   .hiword(100)
        .word   .hiword(10)
        .word   .hiword(1)

; ---------------------------------------------------------------------------

.proc WaitVBlank

_4e41:  stz     $51
:       lda     $51
        beq     :-
        stz     $51
        rts

.endproc

; ---------------------------------------------------------------------------

.proc InitInterrupts

_4e4a:  lda     #$5c
        sta     $1f00
        sta     $1f04
        ldx     #near FieldNMI
        stx     $1f01
        lda     #^FieldNMI
        sta     $1f03
        ldx     #near FieldIRQ
        stx     $1f05
        lda     #^FieldIRQ
        sta     $1f07
        rts

.endproc

; ---------------------------------------------------------------------------

.proc InitHardware

_4e69:  lda     #$80
        sta     hINIDISP
        lda     #0
        sta     hNMITIMEN
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #3
        sta     hOBJSEL
        stz     hOAMADDL
        stz     hOAMADDH
        lda     #$09
        sta     hBGMODE
        stz     hMOSAIC
        lda     #$00
        sta     hBG12NBA
        lda     #$04
        sta     hBG34NBA
        lda     #$80
        sta     hVMAINC
        stz     hM7SEL
        stz     hM7A
        lda     #$08
        sta     hM7A
        stz     hM7B
        stz     hM7B
        stz     hM7C
        stz     hM7C
        stz     hM7D
        lda     #$08
        sta     hM7D
        lda     #$80
        sta     hM7X
        sta     hM7X
        sta     hM7Y
        sta     hM7Y
        sta     hCGADD
        lda     #$bf
        sta     hW12SEL
        lda     #$0b
        sta     hW34SEL
        lda     #$bb
        sta     hWOBJSEL
        lda     #$08
        sta     hWH0
        lda     #$f7
        sta     hWH1
        lda     #$ff
        sta     hWH2
        lda     #$00
        sta     hWH3
        lda     #$01
        sta     hWBGLOG
        lda     #$00
        sta     hWOBJLOG
        lda     #$13
        sta     hTM
        lda     #$04
        sta     hTS
        lda     #$17
        sta     hTMW
        stz     hTSW
        lda     #$22
        sta     hCGSWSEL
        lda     #$e0
        sta     hCOLDATA
        lda     #$00
        sta     hSETINI
        lda     #$ff
        sta     hWRIO
        stz     hHTIMEL
        stz     hHTIMEH
        stz     hVTIMEL
        stz     hVTIMEH
        rts

.endproc

; ---------------------------------------------------------------------------

; [ generate random number ]

.proc Rand

@4f2b:  phx
        inc     $16a7
        bne     :+
        lda     $0af9       ; random number seed
        clc
        adc     #$07
        sta     $0af9
:       lda     $16a7
        tax
        lda     f:RNGTbl,x
        sec
        sbc     $0af9
        plx
        rts

.endproc  ; Rand

; ---------------------------------------------------------------------------

; [ update shattering crystal ]

_c04f48:
        lda     #$00
        sta     $25
        lda     $3f
        and     $1a55
        longa
        xba
        lsr
        clc
        adc     $16b1
        tax
        lda     $06
        shorta
@4f5e:  txa
        asl
        and     #$7f
        tay
        lda     f:_c0505a,x
        bmi     @4f8e
        sta     hWRMPYA
        lda     $1a53
        sta     hWRMPYB
        nop3
        stz     $38
        lda     hRDMPYH
        sta     $37
        lda     $1a54
        sta     hWRMPYB
        nop2
        longa
        lda     $37
        clc
        adc     hRDMPYL
        bra     @4fb8
@4f8e:  eor     #$1aff
        sta     hWRMPYA
        lda     $1a53
        sta     hWRMPYB
        nop3
        stz     $38
        lda     hRDMPYH
        sta     $37
        lda     $1a54
        sta     hWRMPYB
        nop2
        longa
        lda     $37
        clc
        adc     hRDMPYL
        eor     #$ffff
        inc
@4fb8:  clc
        adc     $13
        shorta
        xba
        jne     @503c
        xba
        sta     [$23],y
        iny
        lda     f:_c0505a+1,x
        bmi     @4ff2
        sta     hWRMPYA
        lda     $1a53
        sta     hWRMPYB
        nop3
        stz     $38
        lda     hRDMPYH
        sta     $37
        lda     $1a54
        sta     hWRMPYB
        nop2
        longa
        lda     $37
        clc
        adc     hRDMPYL
        bra     @501c
@4ff2:  eor     #$1aff
        sta     hWRMPYA
        lda     $1a53
        sta     hWRMPYB
        nop3
        stz     $38
        lda     hRDMPYH
        sta     $37
        lda     $1a54
        sta     hWRMPYB
        nop2
        longa
        lda     $37
        clc
        adc     hRDMPYL
        eor     #$ffff
        inc
@501c:  clc
        adc     $15
        shorta
        cmp     #$e0
        bcs     @503d
        xba
        jne     @503d
        xba
        sta     [$23],y
        iny
        lda     $16af
        sta     [$23],y
        iny
        lda     $16b0
        sta     [$23],y
        bra     @5044
@503c:  iny
@503d:  lda     #$00
        xba
        lda     #$f8
        sta     [$23],y
@5044:  inx2
        txa
        and     #$7f
        beq     @5059
        and     #$3f
        jne     @4f5e
        ldy     $26
        sty     $23
        jmp     @4f5e
@5059:  rts

; ---------------------------------------------------------------------------

_c0505a:
        .byte   $13,$a0,$10,$a3,$0b,$a6,$04,$a7,$fc,$a7,$f5,$a6,$f0,$a3,$ed,$a0
        .byte   $ed,$9c,$f0,$99,$f5,$96,$fc,$94,$04,$94,$0b,$96,$10,$99,$13,$9c
        .byte   $26,$a7,$20,$ae,$15,$b4,$07,$b6,$f9,$b6,$eb,$b4,$e0,$ae,$da,$a7
        .byte   $da,$a0,$e0,$99,$eb,$94,$f9,$91,$07,$91,$15,$94,$20,$99,$26,$a0
        .byte   $36,$b2,$2e,$bc,$1f,$c4,$0b,$c8,$f5,$c8,$e1,$c4,$d2,$bc,$ca,$b2
        .byte   $ca,$a7,$d2,$9d,$e1,$96,$f5,$92,$0b,$92,$1f,$96,$2e,$9d,$36,$a7
        .byte   $45,$c0,$3b,$cd,$27,$d7,$0e,$dc,$f2,$dc,$d9,$d7,$c5,$cd,$bb,$c0
        .byte   $bb,$b2,$c5,$a6,$d9,$9c,$f2,$97,$0e,$97,$27,$9c,$3b,$a6,$45,$b2

        .byte   $52,$d1,$45,$e0,$2e,$eb,$10,$f1,$f0,$f1,$d2,$eb,$bb,$e0,$ae,$d1
        .byte   $ae,$c0,$bb,$b1,$d2,$a6,$f0,$a0,$10,$a0,$2e,$a6,$45,$b1,$52,$c0
        .byte   $5b,$e3,$4d,$f3,$33,$00,$12,$07,$ee,$07,$cd,$00,$b3,$f3,$a5,$e3
        .byte   $a5,$d1,$b3,$c0,$cd,$b3,$ee,$ac,$12,$ac,$33,$b3,$4d,$c0,$5b,$d1
        .byte   $60,$f6,$52,$08,$36,$15,$13,$1d,$ed,$1d,$ca,$15,$ae,$08,$a0,$f6
        .byte   $a0,$e3,$ae,$d1,$ca,$c4,$ed,$bc,$13,$bc,$36,$c4,$52,$d1,$60,$e3
        .byte   $62,$0a,$53,$1c,$38,$2a,$14,$31,$ec,$31,$c8,$2a,$ad,$1c,$9e,$0a
        .byte   $9e,$f6,$ad,$e4,$c8,$d6,$ec,$cf,$14,$cf,$38,$d6,$53,$e4,$62,$f6

        .byte   $13,$a1,$0f,$a4,$09,$a7,$02,$a8,$fa,$a7,$f4,$a5,$ef,$a3,$ed,$9f
        .byte   $ed,$9b,$f1,$98,$f7,$95,$fe,$94,$06,$95,$0c,$96,$11,$99,$13,$9d
        .byte   $25,$a9,$1e,$b0,$12,$b4,$04,$b7,$f5,$b6,$e8,$b2,$de,$ad,$da,$a5
        .byte   $db,$9e,$e2,$97,$ee,$93,$fc,$91,$0b,$91,$18,$95,$22,$9b,$26,$a2
        .byte   $35,$b5,$2b,$be,$1a,$c5,$05,$c8,$f0,$c7,$dd,$c2,$cf,$ba,$c9,$b0
        .byte   $cb,$a5,$d5,$9b,$e6,$94,$fb,$91,$10,$92,$23,$97,$31,$a0,$37,$aa
        .byte   $44,$c4,$37,$d0,$21,$d8,$07,$dc,$eb,$db,$d3,$d5,$c2,$ca,$ba,$bd
        .byte   $bc,$af,$c9,$a3,$df,$9a,$f9,$96,$15,$97,$2d,$9e,$3e,$a9,$46,$b6

        .byte   $50,$d5,$40,$e3,$27,$ed,$08,$f2,$e8,$f0,$cb,$e9,$b7,$dc,$ad,$cd
        .byte   $b0,$bc,$c0,$ae,$d9,$a4,$f8,$9f,$18,$a1,$35,$a8,$49,$b5,$53,$c4
        .byte   $58,$e7,$47,$f7,$2c,$02,$09,$08,$e5,$06,$c5,$fd,$af,$f0,$a4,$de
        .byte   $a8,$cc,$b9,$bc,$d4,$b1,$f7,$ac,$1b,$ae,$3b,$b6,$51,$c4,$5c,$d5
        .byte   $5e,$fb,$4c,$0c,$2e,$18,$0a,$1d,$e4,$1b,$c2,$12,$aa,$04,$9e,$f1
        .byte   $a2,$de,$b4,$cd,$d2,$c1,$f6,$bc,$1c,$be,$3e,$c7,$56,$d5,$62,$e8
        .byte   $60,$0f,$4d,$20,$2f,$2c,$0a,$32,$e3,$30,$c1,$27,$a8,$18,$9c,$05
        .byte   $a0,$f1,$b3,$e0,$d1,$d4,$f6,$ce,$1d,$d0,$3f,$d9,$58,$e8,$64,$fb

        .byte   $14,$9e,$12,$a2,$0e,$a5,$07,$a7,$00,$a8,$f9,$a7,$f2,$a5,$ee,$a2
        .byte   $ec,$9e,$ee,$9a,$f2,$97,$f9,$95,$00,$94,$07,$95,$0e,$97,$12,$9a
        .byte   $26,$a4,$23,$ab,$1b,$b1,$0f,$b5,$00,$b7,$f1,$b5,$e5,$b1,$dd,$ab
        .byte   $da,$a4,$dd,$9c,$e5,$96,$f1,$92,$00,$90,$0f,$92,$1b,$96,$23,$9c
        .byte   $38,$ad,$33,$b7,$27,$c0,$15,$c7,$00,$c9,$eb,$c7,$d9,$c0,$cd,$b7
        .byte   $c8,$ad,$cd,$a2,$d9,$99,$eb,$93,$00,$91,$15,$93,$27,$99,$33,$a2
        .byte   $47,$b9,$41,$c7,$32,$d2,$1b,$da,$00,$dd,$e5,$da,$ce,$d2,$bf,$c7
        .byte   $b9,$b9,$bf,$ac,$ce,$a0,$e5,$99,$00,$96,$1b,$99,$32,$a0,$41,$ac

        .byte   $53,$c8,$4d,$d8,$3b,$e6,$20,$ef,$00,$f2,$e0,$ef,$c5,$e6,$b3,$d8
        .byte   $ad,$c8,$b3,$b9,$c5,$ab,$e0,$a2,$00,$9f,$20,$a2,$3b,$ab,$4d,$b9
        .byte   $5c,$da,$55,$eb,$41,$fa,$23,$04,$00,$08,$dd,$04,$bf,$fa,$ab,$eb
        .byte   $a4,$da,$ab,$c8,$bf,$b9,$dd,$af,$00,$ac,$23,$af,$41,$b9,$55,$c8
        .byte   $62,$ec,$5b,$ff,$45,$0f,$26,$1a,$00,$1e,$da,$1a,$bb,$0f,$a5,$ff
        .byte   $9e,$ec,$a5,$da,$bb,$ca,$da,$bf,$00,$bb,$26,$bf,$45,$ca,$5b,$da
        .byte   $64,$00,$5c,$13,$47,$23,$26,$2e,$00,$32,$da,$2e,$b9,$23,$a4,$13
        .byte   $9c,$00,$a4,$ed,$b9,$dd,$da,$d2,$00,$ce,$26,$d2,$47,$dd,$5c,$ed

        .byte   $13,$9f,$11,$a3,$0c,$a5,$06,$a7,$fe,$a8,$f7,$a7,$f1,$a4,$ed,$a1
        .byte   $ed,$9d,$ef,$99,$f4,$96,$fa,$95,$02,$94,$09,$95,$0f,$98,$13,$9b
        .byte   $26,$a5,$22,$ad,$18,$b2,$0b,$b6,$fc,$b7,$ee,$b4,$e2,$b0,$db,$a9
        .byte   $da,$a2,$de,$9b,$e8,$95,$f5,$91,$04,$91,$12,$93,$1e,$97,$25,$9e
        .byte   $37,$b0,$31,$ba,$23,$c2,$10,$c7,$fb,$c8,$e6,$c5,$d5,$be,$cb,$b5
        .byte   $c9,$aa,$cf,$a0,$dd,$97,$f0,$92,$05,$91,$1a,$94,$2b,$9b,$35,$a5
        .byte   $46,$bd,$3e,$ca,$2d,$d5,$15,$db,$f9,$dc,$df,$d8,$c9,$d0,$bc,$c4
        .byte   $ba,$b6,$c2,$a9,$d3,$9e,$eb,$97,$07,$96,$21,$9a,$37,$a3,$44,$af

        .byte   $53,$cd,$49,$dc,$35,$e9,$18,$f0,$f8,$f2,$d9,$ed,$c0,$e3,$b0,$d5
        .byte   $ad,$c4,$b7,$b5,$cb,$a8,$e8,$a1,$08,$9f,$27,$a4,$40,$ae,$50,$bc
        .byte   $5c,$de,$51,$f0,$3b,$fd,$1b,$06,$f7,$08,$d4,$02,$b9,$f7,$a8,$e7
        .byte   $a4,$d5,$af,$c4,$c5,$b6,$e5,$ae,$09,$ac,$2c,$b1,$47,$bc,$58,$cc
        .byte   $62,$f1,$56,$04,$3e,$12,$1c,$1b,$f6,$1d,$d2,$18,$b4,$0c,$a2,$fb
        .byte   $9e,$e8,$aa,$d5,$c2,$c7,$e4,$be,$0a,$bc,$2e,$c1,$4c,$cd,$5e,$de
        .byte   $64,$05,$58,$18,$3f,$27,$1d,$30,$f6,$32,$d1,$2c,$b3,$20,$a0,$0f
        .byte   $9c,$fb,$a8,$e8,$c1,$d9,$e3,$d0,$0a,$ce,$2f,$d4,$4d,$e0,$60,$f1

; ---------------------------------------------------------------------------

; [ load map ]

LoadMap:
@545a:  jsr     _c06081       ; fade out

LoadMapNoFade:
@545d:  ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @546c
        jsr     LoadWorldMap
        jsr     _c06100       ; fade in
        rts
@546c:  jsr     LoadSubMap
        jsr     _c06100       ; fade in
        jsr     _c09267       ; show map title
        rts

; ---------------------------------------------------------------------------

; [ reload map ]

ReloadMap:
@5476:  ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @5485
        jsr     _c05532       ; reload world map
        jsr     _c06100       ; fade in
        rts
@5485:  jsr     _c0577c       ; reload map
        jsr     _c06100       ; fade in
        jsr     _c09267       ; show map title
        rts

; ---------------------------------------------------------------------------

; [ load parent map ]

LoadParentMap:
@548f:  ldx     $0af5
        stx     $0ad6       ; map index
        lda     $0af7
        sta     $1088
        lda     $0af8
        sta     $1089
        inc     $6e
        jsr     LoadMap
        rts

; ---------------------------------------------------------------------------

; [  ]

_c054a7:
@54a7:  stz     $6e
        stz     $be
        stz     $5b
        stz     $b5
        stz     $a0
        stz     $a5
        stz     $9f
        stz     $a6
        stz     $a3
        stz     $56
        stz     $5a
        stz     $5c
        stz     $0a34
        stz     $0a35
        lda     #$01
        sta     $59
        sta     $c3         ; party z-level
        sta     $5d
        ldx     #$0110      ; party sprite priority (normal)
        stx     $c7
        lda     $1088
        sta     $0ad8
        lda     $1089
        sta     $0ad9
        ldx     $06
        stx     $16a8
        lda     #$f1
        jsr     _c0c796
        lda     #$fd
        jsr     _c0c796
        stz     $16a4
        stz     $16a5
        stz     $16a6

_c054f6:
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #$80
        sta     hINIDISP
        lda     #$00
        sta     hNMITIMEN
        sei
        stz     $5e
        jsr     InitHardware
        jsr     _c04c95       ; clear sprite data
        jsr     TfrSprites
        lda     #$70
        sta     $c5
        sta     $c6
        jsr     _c05ee5
        jsr     _c05f8d
        jsr     _c0990d       ; init palette animation
        jsr     _c041f1
        jsr     _c062bc
        rts

; ---------------------------------------------------------------------------

; [ load world map ]

LoadWorldMap:
@5528:  lda     $0adc
        beq     _c05532
        lda     #$03
        sta     $0adb

_c05532:
@5532:  stz     $53
        stz     $169a
        jsr     _c054a7
        lda     #$00
        sta     hBG1SC
        lda     #$10
        sta     $49
        sta     $4a
        ldx     $0ad6       ; map index
        cpx     #$0003
        bcc     @5551
        lda     #$c1
        bra     @5553
@5551:  lda     #$41
@5553:  sta     $48
        lda     $44
        bne     @555d
        lda     $48
        sta     $47
@555d:  jsr     _c04b56
        jsr     TfrPartyGfx
        jsr     _c01e14
        ldx     #$6100
        stx     $2e
        ldx     #$0200
        stx     $2c
        ldx     #$6c00
        stx     $23
        lda     #$da
        sta     $25
        jsr     _c04cbc       ; copy data to vram
        ldx     #$6200
        stx     $2e
        ldx     #$0400
        stx     $2c
        ldx     #$c000
        stx     $23
        lda     #$da        ; da/c000 (flying chocobo graphics)
        sta     $25
        jsr     _c04cbc       ; copy data to vram
        ldx     #$6440
        stx     $33
        ldx     #$000c
        stx     $35
        ldx     #$3ac0
        stx     $30
        jsr     _c04ad5       ; copy 3bpp graphics to vram
        ldx     #$6540
        stx     $33
        ldx     #$00f0
        stx     $35
        ldx     #$3b80
        stx     $30
        jsr     _c04ad5       ; copy 3bpp graphics to vram
        ldx     #$6400
        stx     $2e
        ldx     #$0080
        stx     $2c
        ldx     #$1f00
        stx     $23
        lda     #$da        ; da/1f00 (gradient graphics 1)
        sta     $25
        jsr     _c04cbc       ; copy data to vram
        ldx     #$6500
        stx     $2e
        ldx     #$0080
        stx     $2c
        ldx     #$1f80
        stx     $23
        lda     #$da        ; da/1f80 (gradient graphics 2)
        sta     $25
        jsr     _c04cbc       ; copy data to vram
        jsr     _c056f8
        ldx     #$0080
@55e8:  lda     f:MapSpritePal+$0100-1,x
        sta     $0cff,x
        dex
        bne     @55e8
        ldx     #$0040
@55f5:  lda     f:MapSpritePal-1,x
        sta     $0cff,x
        dex
        bne     @55f5
        ldx     $0ad6       ; map index
        cpx     #$0003
        bcc     @560c
        ldx     #$0020
        bra     @560e
@560c:  ldx     $06
@560e:  ldy     $06
@5610:  lda     f:WindowPal,x
        sta     $0d80,y
        inx
        iny
        cpy     #$0020
        bne     @5610
        jsr     TfrPal
        jsr     _c04c95       ; clear sprite data
        jsr     TfrSprites
        lda     $0ad6       ; map index
        tax
        lda     f:WorldTilesetTbl,x
        longa
        xba
        lsr2
        sta     $0d
        lda     $06
        shorta
        lda     $0ad6       ; map index
        tax
        lda     f:WorldTilesetTbl,x
        longa
        xba
        asl
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        ldy     $06
@5650:  lda     f:WorldTileProp,x
        sta     $1186,y
        inx
        iny
        cpy     #$0240
        bne     @5650
        lda     #$01
        sta     $ba
        lda     $0ad6       ; map index
        tax
        lda     f:WorldTilesetTbl,x
        asl
        clc
        adc     f:WorldTilesetTbl,x
        longa
        xba
        clc
        adc     #near WorldTileset
        tay
        lda     $06
        shorta
        lda     #^WorldTileset
        jsr     _c06b2d       ; load world tileset
        jsr     _c06c4a
        jsr     _c04583
        jsr     _c04798
        jsr     _c08c92
        jsr     _c08c2e       ; init hdma #1 (window 2 position)
        stz     $6f
        ldx     $0ad6       ; map index
        cpx     #$0003
        bcs     @56b1
        lda     $0adc
        beq     @56b5
        cmp     #$06
        bne     @56b5
        lda     $0af1
        and     #$03
        bne     @56b5
        lda     $0af2
        and     #$7f
        beq     @56b5
@56b1:  lda     #$0f
        sta     $6f
@56b5:  jsr     _c05bf8
        jsr     _c01733       ; update local tile properties (world map)
        jsr     _c0104a
        jsr     _c0612b
        jsr     _c02137
        jsr     _c01e64
        jsr     _c01ec5
        jsr     _c061d7
        lda     #$07
        sta     hBGMODE
        jsr     _c0630a
        stz     $ba
        lda     $0adc
        beq     @56e7
        dec
        asl2
        tay
        lda     $0add,y
        and     #$1c
        lsr2
@56e7:  asl
        tax
        longa
        lda     f:_c0573e,x
        sta     $c0
        lda     $06
        shorta
        stz     $55
        rts

; ---------------------------------------------------------------------------

; [ load world map palette ]

_c056f8:
@56f8:  lda     $0ad6       ; map index
        tax
        lda     f:WorldTilesetTbl,x
        longa
        xba
        tax
        ldy     $06
@5706:  lda     f:WorldPal,x
        sta     $0c00,y
        inx2
        iny2
        cpy     #$0100
        bne     @5706
        lda     $06
        shorta
        stz     $1123
        lda     $0ad6       ; map index
        beq     @5726
        cmp     #$02
        bne     @572e
@5726:  lda     #$0e
        sta     $1123
        jsr     _c0990d       ; init palette animation
@572e:  jsr     _c09618
        jsr     _c09704
        lda     #$66
        sta     $5e
        rts

; ---------------------------------------------------------------------------

; world map tilesets
WorldTilesetTbl:
@5739:  .byte   0,1,0,2,2

_c0573e:
        .word   2,4,4,4,4,4,16

; ---------------------------------------------------------------------------

; [ load normal map (sub-map) ]

LoadSubMap:
@574c:  lda     #$01
        sta     $53
        stz     $169f                   ; clear hiryuu flag
        lda     $b9
        sta     $0adb
        inc
        sta     $bf
        jsr     _c054a7
        jsr     _c05af6                   ; load map properties
        jsr     _c05adb
        jsr     _c03eaa                   ; load npcs
        jsr     _c06b44                   ; load tile properties and tileset
        jsr     _c05875                   ; load map layouts
        jsr     _c058db                   ; load map palette
        jsr     _c05cbd                   ; load treasure chests
        jsr     _c063d4
        jsr     _c0580e
        jsr     _c057f9

_c0577c:
        jsr     _c054f6
        ldx     $0971                   ; set window color
        stx     $0c02
        jsr     _c0591a                   ; load map graphics
        lda     $55
        beq     @5792                   ; branch if not a world map
        jsr     _c05adb
        jsr     _c03d28                   ; load npc graphics
@5792:  stz     $ba
        ldx     #$0002
        stx     $c0
        jsr     _c05b2d                   ; init map color math
        jsr     _c06b44                   ; load tile properties and tileset
        jsr     _c06d0c
        jsr     _c06c6a
        jsr     _c08c92
        jsr     _c08c2e                 ; init hdma #1 (window 2 position)
        jsr     _c08d0e
        jsr     _c08b53
        jsr     _c09a96                   ; init map animation
        jsr     _c05d30
        jsr     _c02817
        jsr     _c0928c                   ; init map title
        lda     #$fe
        jsr     _c0ca3c                   ; get event flag $01xx
        cmp     #$00
        bne     @57cc
        lda     $1125                   ; song
        jsr     PlaySong
@57cc:  jsr     _c017e8                 ; update local tile properties (normal map)
        jsr     _c01372                 ; update party z-level (destination tile)
        jsr     _c013a6                 ; update party z-level (current tile)
        jsr     UpdatePlayerSprite
        jsr     _c039b3                   ; update object sprites
        jsr     _c02973
        jsr     _c02842
        lda     $110f
        lsr5
        and     #$03
        tax
        lda     f:_c057f5,x
        sta     $5e
        stz     $55
        rts

; ---------------------------------------------------------------------------

_c057f5:
        .byte   $06,$46,$7e,$06

; ---------------------------------------------------------------------------

; [  ]

_c057f9:
@57f9:  lda     $110f
        and     #$0c
        beq     @580a
        and     #$08
        bne     @5808
        lda     #$10
        bra     @580a
@5808:  lda     #$3f
@580a:  sta     $169a
        rts

; ---------------------------------------------------------------------------

; [ init auto-scroll ]

_c0580e:
@580e:  ldx     #$0000
        stx     $1098
        stx     $109a
        stx     $109c
        stx     $109e
        lda     $1121
        bpl     @584c       ; branch if bg3 is not tiled
        and     #$07
        asl
        tax
        longa
        lda     f:ParallaxSpeedTbl,x   ; scroll speed
        sta     $109e
        lda     $06
        shorta
        lda     $1121
        lsr3
        and     #$07
        asl
        tax
        longa
        lda     f:ParallaxSpeedTbl,x
        sta     $109c
        lda     $06
        shorta
        bra     @5874
@584c:  and     #$07
        asl
        tax
        longa
        lda     f:ParallaxSpeedTbl,x
        sta     $109a
        lda     $06
        shorta
        lda     $1121
        lsr3
        and     #$07
        asl
        tax
        longa
        lda     f:ParallaxSpeedTbl,x
        sta     $1098
        lda     $06
        shorta
@5874:  rts

; ---------------------------------------------------------------------------

; [ load map layouts ]

_c05875:
@5875:  longa
        lda     $1118       ; bg1 map layout
        and     #$03ff
        dec
        tay
        lda     $06
        shorta
        ldx     $06
        cpy     #$ffff
        beq     @588f
        jsr     _c06aca       ; load map layout
        bra     @5894
@588f:  lda     #$01
        jsr     _c06b21
@5894:  longa
        lda     $1119       ; bg2 map layout
        and     #$0ffc
        lsr2
        dec
        tay
        lda     $06
        shorta
        ldx     #$1000
        cpy     #$ffff
        beq     @58b1
        jsr     _c06aca       ; load map layout
        bra     @58b6
@58b1:  lda     #$01
        jsr     _c06b21
@58b6:  longa
        lda     $111a       ; bg3 map layout
        and     #$3ff0
        lsr4
        dec
        tay
        lda     $06
        shorta
        ldx     #$2000
        cpy     #$ffff
        beq     @58d5
        jsr     _c06aca       ; load map layout
        bra     @58da
@58d5:  lda     #$01
        jsr     _c06b21
@58da:  rts

; ---------------------------------------------------------------------------

; [ load map palette ]

_c058db:
@58db:  lda     $1122       ; map palette index
        longa
        xba
        tax
        lda     $06
        tay
        shorta
@58e7:  lda     f:MapPal,x
        sta     $0c00,y
        inx
        iny
        cpy     #$0100
        bne     @58e7
        ldx     $06
        stx     $0c00
        ldx     #$318c
        stx     $0c04
        ldx     #$7fff
        stx     $0c06
        ldx     #$0080
@5909:  lda     f:MapSpritePal-1,x
        sta     $0cff,x
        sta     $0d7f,x
        dex
        bne     @5909
        jsr     TfrPal
        rts

; ---------------------------------------------------------------------------

; [ load map graphics ]

_c0591a:
@591a:  longa
        lda     $1114       ; map graphics 1
        and     #$3f00
        xba
        asl2
        tax
        lda     f:MapGfxPtrs,x
        clc
        adc     #.loword(MapGfx)
        sta     $23
        lda     #.hiword(MapGfx)
        adc     f:MapGfxPtrs+2,x
        and     #$00ff
        shorta
        sta     $25
        longa
        lda     $06
        sec
        sbc     $23
        cmp     #$2000
        bcc     @5955
        lda     #$2000
        sta     $2c
        lda     $06
        sta     $0d
        bra     @595f
@5955:  sta     $2c
        lda     $23
        clc
        adc     #$2000
        sta     $0d
@595f:  lda     $06
        shorta
        ldx     $06
        stx     $2e
        jsr     _c04cbc       ; copy data to vram
        longa
        lda     $2c
        lsr
        sta     $2e
        lda     $06
        shorta
        ldx     $0d
        beq     @5984
        stx     $2c
        ldx     $06
        stx     $23
        inc     $25
        jsr     _c04cbc       ; copy data to vram
@5984:  longa
        lda     $1115       ; map graphics 2
        and     #$0fc0
        asl2
        xba
        asl2
        tax
        lda     f:MapGfxPtrs,x
        clc
        adc     #.loword(MapGfx)
        sta     $23
        lda     #.hiword(MapGfx)
        adc     f:MapGfxPtrs+2,x
        and     #$00ff
        shorta
        sta     $25
        longa
        lda     $06
        sec
        sbc     $23
        cmp     #$2000
        bcc     @59c1
        lda     #$2000
        sta     $2c
        lda     $06
        sta     $0d
        bra     @59cb
@59c1:  sta     $2c
        lda     $23
        clc
        adc     #$2000
        sta     $0d
@59cb:  lda     $06
        shorta
        ldx     #$1000
        stx     $2e
        jsr     _c04cbc       ; copy data to vram
        longa
        lda     $2c
        lsr
        clc
        adc     #$1000
        sta     $2e
        lda     $06
        shorta
        ldx     $0d
        beq     @59f5
        stx     $2c
        ldx     $06
        stx     $23
        inc     $25
        jsr     _c04cbc       ; copy data to vram
@59f5:  longa
        lda     $1116       ; map graphics 3
        and     #$03f0
        lsr2
        tax
        lda     f:MapGfxPtrs,x   ; pointers to map graphics
        clc
        adc     #.loword(MapGfx)
        sta     $23
        lda     #.hiword(MapGfx)
        adc     f:MapGfxPtrs+2,x
        and     #$00ff
        shorta
        sta     $25
        longa
        lda     $06
        sec
        sbc     $23
        cmp     #$2000
        bcc     @5a2f
        lda     #$2000
        sta     $2c
        lda     $06
        sta     $0d
        bra     @5a39
@5a2f:  sta     $2c
        lda     $23
        clc
        adc     #$2000
        sta     $0d
@5a39:  lda     $06
        shorta
        ldx     #$2000
        stx     $2e
        jsr     _c04cbc       ; copy data to vram
        longa
        lda     $2c
        lsr
        clc
        adc     #$2000
        sta     $2e
        lda     $06
        shorta
        ldx     $0d
        beq     @5a63
        stx     $2c
        ldx     $06
        stx     $23
        inc     $25
        jsr     _c04cbc       ; copy data to vram
@5a63:  longa
        lda     $1116
        and     #$fc00
        lsr
        xba
        tax
        lda     f:MapBG3GfxPtrs,x
        clc
        adc     #near MapBG3Gfx
        sta     $23
        lda     $06
        shorta
        lda     #^MapBG3Gfx
        sta     $25
        ldx     #$4000
        stx     $2e
        ldx     #$1000
        stx     $2c
        jsr     _c04cbc       ; copy data to vram
        jsr     TfrPartyGfx
        jsr     _c01e14
        ldx     #$3d00
        stx     $2e
        ldx     #$0600
        stx     $2c
        ldx     #near WindowGfx
        stx     $23
        lda     #^WindowGfx
        sta     $25
        jsr     _c04cbc       ; copy data to vram
        jsr     _c0257c
        ldx     $06
@5aae:  lda     $0500,x
        and     #$40
        bne     @5abc
        lda     $0520,x
        and     #$01
        bne     @5ada
@5abc:  longa
        txa
        clc
        adc     #$0050
        tax
        lda     $06
        shorta
        cpx     #$0140
        bne     @5aae
        stz     $0b6d
        ldx     #$0020
        stx     $2e
        stx     $2c
        jsr     FillVRAM
@5ada:  rts

; ---------------------------------------------------------------------------

; [  ]

_c05adb:
@5adb:  lda     $a4
        bne     @5af5
        ldx     #$6400                  ; vram $6400
        stx     $2e
        ldx     #$0200                  ; size
        stx     $2c
        ldx     #near TimerFontGfx
        stx     $23
        lda     #^TimerFontGfx
        sta     $25
        jsr     _c04cbc                 ; copy data to vram
@5af5:  rts

; ---------------------------------------------------------------------------

; [ load map properties ]

_c05af6:
@5af6:  lda     $0ad4       ; map index
        sta     hM7A
        lda     $0ad5
        sta     hM7A
        lda     #$1a        ; 26 bytes each
        sta     hM7B
        sta     hM7B
        ldx     hMPYL
        ldy     $06
@5b0f:  lda     f:MapProp,x
        sta     $110c,y
        inx
        iny
        cpy     #$001a
        bne     @5b0f
        longa
        lda     $110c
        and     #$0fff
        sta     $0ad6       ; map index
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ init map color math settings ]

_c05b2d:
@5b2d:  lda     #$09
        sta     hBGMODE                 ; 8x8 tiles, high priority bg3, mode 1
        lda     #$49
        sta     hBG1SC                  ; 32x16 bg1 map at $4800
        ldx     $0ad6                   ; map index
        cpx     #$00dd
        beq     @5b4b                   ; branch if flying fortress
        lda     #$51
        sta     hBG2SC                  ; 32x16 bg2 map at $5000
        lda     #$59
        sta     hBG3SC                  ; 32x16 bg3 map at $5800
        bra     @5b55

; flying fortress
@5b4b:  lda     #$52
        sta     hBG2SC                  ; 16x32 bg2 map at $5000
        lda     #$5a
        sta     hBG3SC                  ; 16x32 bg3 map at $5800
@5b55:  ldx     $0ad4
        cpx     #$00be
        bne     @5b62                   ; branch if airship
        lda     #$23
        sta     hBG3SC                  ; 32x32 bg3 map at $2000
@5b62:  lda     $1110
        and     #$3f
        sta     $08
        asl
        clc
        adc     $08
        tax
        lda     f:_c05bb8,x
        sta     $48
        lda     $44
        bne     @5b7c
        lda     $48
        sta     $47
@5b7c:  lda     f:_c05bb8+2,x
        sta     $4a
        sta     hTS       ; sub-screen designation
        lda     f:_c05bb8+1,x
        and     #$01
        beq     @5b91
        lda     #$bf
        bra     @5b93
@5b91:  lda     #$bc
@5b93:  sta     hW12SEL       ; window mask settings bg1/bg2
        lda     f:_c05bb8+1,x
        ora     #$01        ; bg1 always on
        sta     hTM       ; main screen designation
        lda     $44
        bne     @5ba7
        lda     $4a
        sta     $49
@5ba7:  rts

; ---------------------------------------------------------------------------

; auto-scroll speeds
ParallaxSpeedTbl:
@5ba8:  .addr   0,-4,-2,-1,0,+1,+2,+4

; ---------------------------------------------------------------------------

; color math, main screen, sub-screen
_c05bb8:
        .byte   $00,$17,$00
        .byte   $51,$13,$04
        .byte   $42,$13,$04
        .byte   $42,$13,$10
        .byte   $06,$17,$11
        .byte   $d1,$13,$04
        .byte   $42,$06,$11
        .byte   $53,$13,$15
        .byte   $51,$11,$02
        .byte   $43,$12,$04
        .byte   $20,$15,$02
        .byte   $11,$13,$02
        .byte   $13,$13,$04
        .byte   $52,$13,$14
        .byte   $02,$17,$11
        .byte   $c2,$12,$04
        .byte   $50,$17,$01
        .byte   $44,$17,$11
        .byte   $51,$12,$04
        .byte   $44,$17,$13

; ---------------------------------------------------------------------------

; [  ]

_c05bf4:
        lda     #$10
        bra     _c05c01

_c05bf8:
        lda     $0ad6       ; map index
        cmp     #$03
        bcs     _5c2c       ; return if underwater
        lda     $6f

_c05c01:
        lsr
        asl4
        tax
        lda     $0ad6       ; map index
        and     #$01        ; 14 colors for world 0/2, 16 for world 1
        asl
        clc
        adc     #$0e
        sta     $23
        stz     $24
        ldy     $06
_5c16:  lda     f:_c05c2d,x
        sta     $0c40,y
        lda     f:_c05c2d+1,x
        sta     $0c41,y
        inx2
        iny2
        cpy     $23
        bne     _5c16
_5c2c:  rts

_c05c2d:
        .byte   $20,$0c,$60,$40,$ec,$76,$47,$72,$a5,$65,$e2,$58,$a1,$4c,$20,$0c
        .byte   $20,$18,$80,$44,$ab,$76,$06,$6e,$84,$65,$e2,$5c,$a1,$50,$20,$18
        .byte   $40,$24,$80,$48,$49,$72,$c5,$6d,$64,$65,$c2,$5c,$a1,$50,$40,$24
        .byte   $61,$30,$81,$50,$08,$6e,$a5,$69,$43,$65,$c2,$5c,$a1,$54,$61,$30
        .byte   $61,$3c,$81,$54,$a6,$69,$64,$69,$03,$61,$c1,$5c,$a1,$58,$61,$3c
        .byte   $81,$48,$a1,$58,$44,$69,$23,$65,$e2,$60,$c1,$60,$a1,$5c,$81,$48
        .byte   $81,$54,$a1,$5c,$03,$65,$e2,$64,$c2,$60,$a1,$60,$a1,$5c,$81,$54
        .byte   $81,$54,$a1,$5c,$03,$65,$e2,$64,$c2,$60,$a1,$60,$a1,$5c,$81,$54
        .byte   $a1,$60,$a1,$60,$a1,$60,$a1,$60,$a1,$60,$a1,$60,$a1,$60,$a1,$60

; ---------------------------------------------------------------------------

; [ load treasure chests ]

_c05cbd:
        longa
        lda     $0ad4
        tax
        lda     f:MapTreasures,x   ; pointer to treasure properties
        and     #$00ff
        asl2
        sta     $23
        lda     f:MapTreasures+1,x   ; pointer to next map's treasure properties
        and     #$00ff
        asl2
        sta     $26
        lda     $06
        shorta
@5cdd:  ldx     $23
        cpx     $26
        beq     @5d2f
        longa
        txa
        lsr2
        shorta
        xba
        lda     #$00
        xba
        sta     $16a1
        jsr     _c0ca16       ; get treasure flag
        cmp     #$00
        beq     @5d25       ; skip if treasure hasn't been obtained
        ldx     $23
        lda     f:TreasureProp+1,x
        longa
        asl6
        sta     $0d
        lda     f:TreasureProp,x
        and     #$00ff
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        lda     $7f0000,x   ; show open treasure chest
        cmp     #$02
        bne     @5d25
        lda     #$12
        sta     $7f0000,x
@5d25:  ldx     $23
        inx4
        stx     $23
        bra     @5cdd
@5d2f:  rts

; ---------------------------------------------------------------------------

; [  ]

_c05d30:
        jsr     _c05d87
        lda     #$7f
        pha
        plb
        ldx     #$00fe
        ldy     #$0100
        longa
@5d3f:  lda     $6b6c,x
        sta     $6b6c,y
        iny2
        dex2
        bpl     @5d3f
        lda     $06
        shorta
        lda     #$00
        pha
        plb
        rts

; ---------------------------------------------------------------------------

; [  ]

_c05d54:
        jsr     _c05d87
        lda     #$7f
        pha
        plb
        ldx     #$00fe
        ldy     #$0100
        longa
@5d63:  lda     $6b6c,x
        sta     $6b6c,y
        iny2
        dex4
        bpl     @5d63
        lda     #$00ff
@5d74:  sta     $6b6c,y
        iny2
        cpy     #$0200
        bne     @5d74
        lda     $06
        shorta
        lda     #$00
        pha
        plb
        rts

; ---------------------------------------------------------------------------

; [  ]

_c05d87:
@5d87:  lda     $169a
        and     #$7f
        bne     @5d8f
        rts
@5d8f:  sta     $0d
        stz     $0e
        longa
        ldx     $06
        txy
        lda     $0d
        pha
        sta     $0f
        asl     $0d
        lda     #$0003
        sec
        sbc     $0d
        sta     $11
@5da7:  lda     $0f
        sta     $0e00,y
        lda     $11
        bmi     @5dc3
        txa
        sec
        sbc     $0f
        asl2
        clc
        adc     #$000a
        clc
        adc     $11
        sta     $11
        dec     $0f
        bra     @5dcf
@5dc3:  txa
        asl2
        clc
        adc     #$0006
        clc
        adc     $11
        sta     $11
@5dcf:  inx
        iny2
        cpx     $0f
        bcc     @5da7
        tyx
        dex2
@5dd9:  lda     $0e00,x
        cmp     $0dfe,x
        beq     @5de8
        txa
        lsr
        sta     $0e00,y
        iny2
@5de8:  dex
        dex
        bne     @5dd9
        pla
        asl
        eor     #$ffff
        inc
        clc
        adc     #$0102
        sta     $0d
        tax
        beq     @5e0a
        ldx     $06
        lda     #$00ff
@5e00:  sta     $7f6b6c,x
        inx2
        cpx     $0d
        bne     @5e00
@5e0a:  lda     $06
        shorta
        dey2
@5e10:  lda     $0e00,y
        clc
        adc     #$78
        sta     $7f6b6d,x
        lda     #$78
        sec
        sbc     $0e00,y
        sta     $7f6b6c,x
        inx2
        dey2
        bpl     @5e10
        rts

; ---------------------------------------------------------------------------

; [ copy tile layout to vram ??? ]

; +$73: vram offset (+$4800)

_c05e2b:
        lda     #$80
        sta     hVMAINC
        lda     #$01
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        lda     #$7f
        sta     hDMA0::ADDR_B
        longa
        lda     #$7622      ; 7f/7622
        sta     hDMA0::ADDR
        lda     #$1000
        sta     hDMA0::SIZE
        lda     $73
        lsr
        clc
        adc     #$4800
        sta     hVMADDL
        lda     $06
        shorta
        lda     #$01
        sta     hMDMAEN
        rts

; ---------------------------------------------------------------------------

; [  ]

_c05e61:
        lda     $0ad9
        sec
        sbc     #$03
        and     #$3f
        xba
        longa
        lsr2
        sta     $23
        lda     $06
        shorta
        lda     $0ad8
        dec
        and     #$3f
        clc
        adc     $23
        sta     $23
        ldx     $23
        lda     $7f0000,x
        jsr     _c05e88

_c05e88:
        cmp     #$20                    ; does nothing ???

_c05e8a:
        lda     $53
        bne     @5e96
        ldx     $61
        stx     $69
        ldx     $63
        stx     $6b
@5e96:  ldy     $06
@5e98:  lda     $3f
        lsr2
        sta     $17
        tya
        clc
        adc     $6b
        clc
        adc     $17
        and     #$1e
        tax
        longa
        lda     $69
        lsr
        clc
        adc     f:_c05ec3,x
        tyx
        sta     $7f6de2,x
        lda     $06
        shorta
        iny2
        cpy     #$0020
        bne     @5e98
        rts

_c05ec3:
        .addr   0,0,+1,+1,+1,+1,+1,0
        .addr   0,0,-1,-1,-1,-1,-1,0,0

; ---------------------------------------------------------------------------

; [  ]

_c05ee5:
        ldx     $06
@5ee7:  lda     #$90
        sta     $7f6d8c,x
        lda     #$e2
        sta     $7f6d8d,x
        lda     #$6d
        sta     $7f6d8e,x
        inx3
        cpx     #$002a
        bne     @5ee7
        lda     #$00
        sta     $7f6d8c,x
        stz     hHDMAEN
        lda     #$42
        sta     hDMA3::CTRL
        sta     hDMA5::CTRL
        ldx     #$6d8c
        stx     hDMA3::ADDR
        stx     hDMA5::ADDR
        lda     #$7f
        sta     hDMA3::ADDR_B
        sta     hDMA5::ADDR_B
        sta     hDMA3::HDMA_B
        sta     hDMA5::HDMA_B
        lda     $53
        bne     @5f33
        lda     #<hBG1HOFS
        sta     hDMA3::HREG
        rts
@5f33:  lda     #<hBG2HOFS
        sta     hDMA3::HREG
        lda     #<hBG3HOFS
        sta     hDMA5::HREG
        rts

; ---------------------------------------------------------------------------

_c05f3e:
        ldy     $06
@5f40:  lda     $3f
        lsr2
        sta     $17
        tya
        clc
        adc     $6b
        clc
        adc     $17
        and     #$1e
        tax
        longa
        lda     $6b
        lsr
        clc
        adc     f:_c05f6b,x
        tyx
        sta     $7f6e02,x
        lda     $06
        shorta
        iny2
        cpy     #$0020
        bne     @5f40
        rts

_c05f6b:
        .addr   0,0,+1,+1,+1,+1,+1,0
        .addr   0,0,-1,-1,-1,-1,-1,0,0

; ---------------------------------------------------------------------------

_c05f8d:
        ldx     $06
@5f8f:  lda     #$90
        sta     $7f6db7,x
        lda     #$02
        sta     $7f6db8,x
        lda     #$6e
        sta     $7f6db9,x
        inx3
        cpx     #$002a
        bne     @5f8f
        lda     #$00
        sta     $7f6db7,x
        stz     hHDMAEN
        lda     #$42
        sta     hDMA4::CTRL
        sta     hDMA6::CTRL
        ldx     #$6db7
        stx     hDMA4::ADDR
        stx     hDMA6::ADDR
        lda     #$7f
        sta     hDMA4::ADDR_B
        sta     hDMA6::ADDR_B
        sta     hDMA4::HDMA_B
        sta     hDMA6::HDMA_B
        lda     $53
        bne     @5fdb
        lda     #<hBG1VOFS
        sta     hDMA4::HREG
        rts
@5fdb:  lda     #<hBG2VOFS
        sta     hDMA4::HREG
        lda     #<hBG3VOFS
        sta     hDMA6::HREG
        rts

; ---------------------------------------------------------------------------

; [  ]

_c05fe6:
@5fe6:  lda     $169e
        cmp     #$02
        jne     @6076
        lda     #$70
        sta     $0300
        lda     #$80
        sta     $0301
        lda     #$c2
        sta     $0302
        lda     #$01
        sta     $0303
        stz     $bc
        lda     $42
        bne     @6048
        lda     $0ad8
        sta     $75
        lda     $0ad9
        sta     $76
        lda     $76
        longa
        asl6
        sta     $0d
        lda     $75
        and     #$00ff
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        lda     $7f0000,x
        cmp     #$05
        bne     @6075
        ldx     #$0101
        stx     $2c
        ldx     $06
        stx     $73
        lda     #$15
        sta     $16b3
        jsr     _c06f08
        bra     @6075
@6048:  lda     $42
        lsr3
        lda     #$00
        adc     #$29
        sta     $bb
        lda     $42
        cmp     #$30
        bne     @6061
        jsr     _c04a71
        lda     #$85
        jsr     PlaySfx
@6061:  lda     $42
        cmp     #$30
        bcc     @6075
        lda     $c6
        inc2
        sta     $c6
        cmp     #$80
        bne     @6075
        lda     #$80
        sta     $42
@6075:  rts
@6076:  lda     $42
        bne     @6080
        jsr     _c04a71
        stz     $169e
@6080:  rts

; ---------------------------------------------------------------------------

; [ fade out ]

_c06081:
        lda     $169e
        bne     @6089
        jsr     _c04a71
@6089:  stz     $42
@608b:  jsr     WaitVBlank
        lda     $169e
        beq     @60a1
        lda     $42
        bmi     @60a1
        jsr     _c05fe6
        jsr     UpdatePlayerSprite
        inc     $42
        bra     @608b
@60a1:  inc     $42
        lda     $45
        bne     @608b
        rts

; ---------------------------------------------------------------------------

; [  ]

_c060a8:
@60a8:  lda     $169e
        cmp     #$02
        bne     @60ff
        lda     $42
        cmp     #$1e
        bcs     @60c0
        dec
        asl2
        sta     $c6
        lda     #$2a
        sta     $bb
        bra     @60ff
@60c0:  cmp     #$3e
        bcs     @60e7
        sec
        sbc     #$1e
        sta     $be
        lsr
        bcs     @60d7
        stz     $10a2
        stz     $10a6
        stz     $10a8
        bra     @60ff
@60d7:  eor     #$ff
        clc
        adc     #$10
        sta     $10a2
        sta     $10a6
        sta     $10aa
        bra     @60ff
@60e7:  cmp     #$56
        bcs     @60f3
        stz     $be
        lda     #$28
        sta     $bb
        bra     @60ff
@60f3:  stz     $169e
        lda     #$01
        sta     $bc
        lda     #$02
        sta     $0adb
@60ff:  rts

; ---------------------------------------------------------------------------

; [ fade in ]

_c06100:
        jsr     _c04a68
        stz     $42
        lda     #$81
        sta     hNMITIMEN
        lda     #$00
        sta     hINIDISP
        cli
@6110:  jsr     WaitVBlank
        lda     $169e
        cmp     #$02
        bne     @6124
        jsr     _c060a8
        jsr     UpdatePlayerSprite
        inc     $42
        bra     @6110
@6124:  lda     $45
        cmp     #$f0
        bne     @6110
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0612b:
        ldx     #$003c
        stx     $23
        ldx     $06
        bra     _613c

_c06134:
        ldx     #$0040
        stx     $23
        ldx     #$000f
_613c:  lda     $6f
        cmp     #$08
        bcc     @6198
        ldy     $06
@6144:  lda     f:_c06199,x
        sta     $0380,y
        sta     $03c0,y
        lda     $6f
        sec
        sbc     #$08
        asl2
        clc
        adc     f:_c061b8,x
        sta     $0381,y
        clc
        adc     #$10
        sta     $03c1,y
        lda     #$40
        sta     $0382,y
        inc2
        sta     $03c2,y
        lda     #$38
        sta     $0383,y
        sta     $03c3,y
        inx
        iny4
        cpy     $23
        bne     @6144
        lda     #$aa
        sta     $0418
        sta     $0419
        sta     $041a
        sta     $041b
        sta     $041c
        sta     $041d
        sta     $041e
        sta     $041f
@6198:  rts

_c06199:
        .byte   $08,$18,$28,$38,$48,$58,$68,$78,$88,$98,$a8,$b8,$c8,$d8,$e8
_c061a8:
        .byte   $00,$10,$20,$30,$40,$50,$60,$70,$80,$90,$a0,$b0,$c0,$d0,$e0,$f0

_c061b8:
        .byte   $e3,$e2,$e2,$e1,$e1,$e0,$e0,$e0,$e0,$e0,$e1,$e1,$e2,$e2,$e3
_c061c7:
        .byte   $e3,$e2,$e2,$e1,$e1,$e0,$e0,$e0,$e0,$e0,$e0,$e1,$e1,$e2,$e2,$e3

; ---------------------------------------------------------------------------

_c061d7:
        lda     #$00
        sta     hBGMODE
        ldx     $06
        stx     $26
        stz     $08
@61e2:  lda     $08
        lsr
        sta     $09
        lda     #$40
        sec
        sbc     $09
        tax
        stx     $23
        lda     $08
        longa
        xba
        lsr2
        sta     $26
        lda     $06
        shorta
        ldy     #$0100
@61ff:  ldx     $23
        lda     f:SineTbl-32,x
        inx
        stx     $23
        sta     hM7A
        stz     hM7A
        lda     $08
        sta     hM7B
        sta     hM7B
        lda     $0ad6       ; map index
        cmp     #$03
        bcs     @6234
        longa
        lda     hMPYL
        lsr5
        clc
        adc     #$0100
        ldx     $26
        sta     $7f4000,x
        jmp     @6249
@6234:  longa
        lda     hMPYL
        lsr6
        clc
        adc     #$0080
        ldx     $26
        sta     $7f4000,x
@6249:  lda     $06
        shorta
        inx2
        stx     $26
        dey
        bne     @61ff
        lda     $08
        clc
        adc     #$08
        sta     $08
        cmp     #$80
        bne     @61e2
        ldx     #$0008
        stx     $26
        ldx     $06
        stx     $23
@6268:  phx
        ldx     $23
        lda     f:_c0d240,x
        inx
        stx     $23
        plx
        sta     hM7A
        stz     hM7A
        lda     $26
        sta     hM7B
        sta     hM7B
        longa
        lda     hMPYL
        xba
        and     #$00ff
        shorta
        clc
        adc     #$e0
        sta     $7f6200,x
        inx
        txa
        bne     @6268
        ldy     #$0000
        sty     $23
        lda     $26
        clc
        adc     #$08
        sta     $26
        longa
        lda     $26
        sec
        sbc     #$0008
        asl5
        tax
        lda     $06
        shorta
        lda     $26
        cmp     #$48
        bne     @6268
        rts

; ---------------------------------------------------------------------------

_c062bc:
        ldx     $06
@62be:  lda     #$90
        sta     $7f6a18,x
        lda     #$49
        sta     $7f6a19,x
        lda     #$6a
        sta     $7f6a1a,x
        inx3
        cpx     #$002a
        bne     @62be
        lda     #$00
        sta     $7f6a42
        ldx     $06
        lda     #$00
@62e2:  sta     $7f6a49,x
        inx
        cpx     #$0010
        bne     @62e2
        stz     hHDMAEN
        lda     #$40
        sta     hDMA7::CTRL
        lda     #$00
        lda     #<hMOSAIC
        sta     hDMA7::HREG
        ldx     #$6a18
        stx     hDMA7::ADDR
        lda     #$7f
        sta     hDMA7::ADDR_B
        sta     hDMA7::HDMA_B
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0630a:
        stz     hM7B
        stz     hM7B
        stz     hM7C
        stz     hM7C
        lda     #$f0
        sta     $7f6a00
        sta     $7f6a03
        lda     #$00
        sta     $7f6a06
        stz     hHDMAEN
        lda     #$42
        sta     hDMA5::CTRL
        sta     hDMA6::CTRL
        lda     #<hM7A
        sta     hDMA5::HREG
        lda     #<hM7D
        sta     hDMA6::HREG
        ldx     #$6a00
        stx     hDMA5::ADDR
        stx     hDMA6::ADDR
        lda     #$7f
        sta     hDMA5::ADDR_B
        sta     hDMA6::ADDR_B
        sta     hDMA5::HDMA_B
        sta     hDMA6::HDMA_B
        lda     #$ff
        sta     $7f6a0a
        sta     $7f6a0d
        lda     #$00
        sta     $7f6a10
        stz     hHDMAEN
        lda     #$40
        sta     hDMA2::CTRL
        lda     #<hCOLDATA
        sta     hDMA2::HREG
        ldx     #$6a07
        stx     hDMA2::ADDR
        lda     #$7f
        sta     hDMA2::ADDR_B
        sta     hDMA2::HDMA_B
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0637e:
        lda     $6f
        asl
        clc
        adc     #$40
        sta     $7f6a02
        sta     $7f6a05
        lda     #$00
        sta     $7f6a01
        lda     #$e0
        sta     $7f6a04
        lda     $6f
        sec
        sbc     #$08
        beq     @63cd
        bcc     @63cd
        pha
        asl2
        ora     #$80
        sta     $7f6a07
        pla
        longa
        xba
        clc
        adc     #$6200
        sta     $7f6a08
        sta     $7f6a0b
        clc
        adc     #$0080
        sta     $7f6a0e
        lda     $06
        shorta
        lda     $5e
        ora     #$04
        sta     $5e
        rts
@63cd:  lda     $5e
        and     #$fb
        sta     $5e
        rts

; ---------------------------------------------------------------------------

; [  ]

_c063d4:
        lda     $1120
        and     #$03
        tax
        lda     f:_c06461,x
        tay
        lda     $0ad8
@63e2:  cpy     #$0000
        beq     @63eb
        asl
        dey
        bra     @63e2
@63eb:  lsr
        sec
        sbc     $111f
        and     #$3f
        sta     $77
        lda     $1120
        lsr2
        and     #$03
        tax
        lda     f:_c06461,x
        tay
        lda     $0ad9
@6404:  cpy     #$0000
        beq     @640d
        asl
        dey
        bra     @6404
@640d:  lsr
        sec
        sbc     $111e
        and     #$3f
        sta     $78
        lda     $1120
        lsr4
        and     #$03
        tax
        lda     f:_c06461,x
        tay
        lda     $0ad8
@6428:  cpy     #$0000
        beq     @6431
        asl
        dey
        bra     @6428
@6431:  lsr
        sec
        sbc     $111d
        and     #$3f
        sta     $79
        lda     $1120
        lsr6
        and     #$03
        tax
        lda     f:_c06461,x
        tay
        lda     $0ad9
@644e:  cpy     #$0000
        beq     @6457
        asl
        dey
        bra     @644e
@6457:  lsr
        sec
        sbc     $111c
        and     #$3f
        sta     $7a
        rts

; parallax multipliers
_c06461:
        .byte   1,0,2,1

; ---------------------------------------------------------------------------

; [  ]

_c06465:
        stz     hVMAINC
        jsr     _c06de9
        stz     hDMA0::CTRL
        ldx     $7f
        stx     hVMADDL
        ldx     #$16f3
        stx     hDMA0::ADDR
        ldx     $7b
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $81
        stx     hVMADDL
        ldx     $7d
        beq     @6494
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@6494:  ldx     $83
        stx     hVMADDL
        ldx     #$1773
        stx     hDMA0::ADDR
        ldx     $7b
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $85
        stx     hVMADDL
        ldx     $7d
        beq     @64ba
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@64ba:  rts

; ---------------------------------------------------------------------------

; [  ]

_c064bb:
        lda     #$03
        sta     hVMAINC
        jsr     _c06de9
        stz     hDMA0::CTRL
        ldx     $7f
        stx     hVMADDL
        ldx     #$16f3
        stx     hDMA0::ADDR
        ldx     $7b
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $81
        stx     hVMADDL
        ldx     $7d
        beq     @64ec
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@64ec:  ldx     $83
        stx     hVMADDL
        ldx     #$1773
        stx     hDMA0::ADDR
        ldx     $7b
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $85
        stx     hVMADDL
        ldx     $7d
        beq     @6512
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@6512:  rts

; ---------------------------------------------------------------------------

; [  ]

_c06513:
        lda     $ba
        bne     @6518
        rts
@6518:  and     #$01
        beq     @6520
        jml     _c065a3
@6520:  lda     $ba
        and     #$02
        beq     @652f
        lda     $75
        clc
        adc     #$21
        sta     $23
        bra     @6536
@652f:  lda     $75
        sec
        sbc     #$1f
        sta     $23
@6536:  lda     $76
        sec
        sbc     #$1f
        and     #$3f
        sta     $24
        ldx     $23
        stx     $26
        ldy     #$0000
@6546:  ldx     $23
        lda     $7f0000,x
        tax
        lda     $7f6e22,x
        sta     $16f3,y
        lda     $7f6fa2,x
        sta     $16f4,y
        lda     $7f6ee2,x
        sta     $1773,y
        lda     $7f7062,x
        sta     $1774,y
        lda     $24
        inc
        and     #$3f
        sta     $24
        iny2
        cpy     #$0080
        bne     @6546
        stz     $7e
        stz     $7c
        stz     $82
        stz     $86
        lda     $26
        asl
        and     #$7f
        sta     $7f
        sta     $81
        inc
        sta     $83
        sta     $85
        lda     $76
        sec
        sbc     #$1f
        and     #$3f
        sta     $80
        sta     $84
        asl
        sta     $7d
        lda     #$80
        sec
        sbc     $7d
        sta     $7b
        rts

; ---------------------------------------------------------------------------

; [  ]

_c065a3:
        lda     $ba
        and     #$02
        bne     @65b6
        lda     $76
        sec
        sbc     #$1f
        sta     $08
        and     #$3f
        sta     $24
        bra     @65c1
@65b6:  lda     $76
        clc
        adc     #$20
        sta     $08
        and     #$3f
        sta     $24
@65c1:  lda     $75
        sec
        sbc     #$1f
        sta     $23
        ldx     $23
        stx     $26
        ldy     #$0000
@65cf:  ldx     $23
        lda     $7f0000,x
        tax
        lda     $7f6e22,x
        sta     $16f3,y
        lda     $7f6ee2,x
        sta     $16f4,y
        lda     $7f6fa2,x
        sta     $1773,y
        lda     $7f7062,x
        sta     $1774,y
        iny2
        inc     $23
        cpy     #$0080
        bne     @65cf
        stz     $7d
        stz     $7e
        lda     #$80
        sta     $7b
        stz     $7c
        stz     $81
        lda     #$80
        sta     $85
        lda     $08
        and     #$3f
        sta     $80
        sta     $82
        sta     $84
        sta     $86
        lda     $75
        sec
        sbc     #$1f
        and     #$3f
        asl
        sta     $7f
        clc
        adc     #$80
        sta     $83
        lda     $7f
        sta     $7d
        lda     #$80
        sec
        sbc     $7d
        sta     $7b
        rts

; ---------------------------------------------------------------------------

; [ show mini-map ]

ShowMinimap:
@6632:  jsr     _c06081       ; fade out

ShowMinimapNoFade:
        jsr     _c04ce8       ; disable interrupts
        stz     $5e
        lda     #$07
        sta     hBGMODE
        lda     #$80
        sta     hM7SEL
        lda     #$00
        sta     hM7A
        lda     #$08
        sta     hM7A
        lda     #$00
        sta     hM7D
        lda     #$08
        sta     hM7D
        lda     #$c0
        sta     hBG1HOFS
        lda     #$ff
        sta     hBG1HOFS
        lda     #$d0
        sta     hBG1VOFS
        lda     #$ff
        sta     hBG1VOFS
        lda     #$01
        sta     hM7X
        lda     #$00
        sta     hM7X
        sta     hM7Y
        sta     hM7Y
        lda     #$11
        sta     hTM
        lda     #$e0
        sta     hCOLDATA
        stz     hVMAINC
        ldx     $06
        stx     hVMADDL
        stx     $0d
        lda     #$1f
        sta     $76
        stz     $ba
@6696:  jsr     _c069a1
        ldy     $06
@669b:  ldx     $26
        lda     $7f0000,x
        tax
        lda     $7f6fa2,x
        sta     hVMDATAL
        ldx     $26
        inx2
        stx     $26
        iny2
        cpy     #$0100
        bne     @669b
        longa
        lda     $26
        clc
        adc     #$0100
        and     #$3fff
        sta     $26
        lda     $06
        shorta
        inc     $76
        inc     $76
        lda     $76
        cmp     #$1f
        bne     @6696
        jsr     _c04c95       ; clear sprite data
        jsr     _c056f8
        jsr     _c06831
        lda     #^MinimapSpriteGfx
        sta     $25
        ldx     #near MinimapSpriteGfx
        stx     $23
        ldx     #$7000
        stx     $2e
        ldx     #$0400
        stx     $2c
        jsr     _c04cbc       ; copy data to vram
        lda     #$01
        sta     $52
        stz     $3f
        jsr     _c06100       ; fade in
@66f9:  jsr     WaitVBlank
        jsr     _c06731
        jsr     _c06755
        jsr     _c067ec
        jsr     _c0679c
        lda     $02
        and     #$cf
        bne     @66f9
        lda     $03
        bne     @66f9
@6712:  jsr     WaitVBlank
        jsr     _c06731
        jsr     _c06755
        jsr     _c067ec
        jsr     _c0679c
        lda     $02
        and     #$cf
        bne     @672b
        lda     $03
        beq     @6712
@672b:  jsr     _c06081       ; fade out
        stz     $52
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06731:
@6731:  lda     $0ad8
        lsr
        clc
        adc     #$3e
        sta     $0200
        lda     $0ad9
        lsr
        clc
        adc     #$2d
        sta     $0201
        lda     #$49
        sta     $0202
        lda     $3f
        and     #$10
        asl
        ora     #$06
        sta     $0203
        rts

; ---------------------------------------------------------------------------

_c06755:
@6755:  lda     #$f8
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        beq     @679b
        lda     $3f
        lsr
        and     #$0e
        tax
        ldy     $06
@6766:  lda     f:_c06961,x
        sta     $0204,y
        lda     f:_c06961+1,x
        sta     $0205,y
        lda     $3f
        lsr
        and     #$01
        clc
        adc     #$4a
        sta     $0206,y
        lda     #$36
        sta     $0207,y
        longa
        txa
        clc
        adc     #$0010
        tax
        tya
        clc
        adc     #$0004
        tay
        lda     $06
        shorta
        cpy     #$0010
        bne     @6766
@679b:  rts

; ---------------------------------------------------------------------------

_c0679c:
@679c:  ldy     $06
@679e:  lda     $0ade,y
        bmi     @67da
        lda     $0add,y
        lsr5
        cmp     $0ad6       ; map index
        bne     @67da
        lda     $0adf,y
        lsr
        clc
        adc     #$3c
        sta     $02f4,y
        lda     $0ae0,y
        lsr
        clc
        adc     #$2c
        sta     $02f5,y
        tya
        lsr2
        tax
        lda     f:_c067e9,x
        sta     $02f6,y
        lda     $3f
        and     #$10
        ora     #$05
        sta     $02f7,y
        bra     @67df
@67da:  lda     #$f8
        sta     $02f5,y
@67df:  iny4
        cpy     #$000c
        bne     @679e
        rts

; ---------------------------------------------------------------------------

_c067e9:
        .byte   $0e, $0f, $1e

; ---------------------------------------------------------------------------

_c067ec:
@67ec:  lda     $0ad6       ; map index
        asl
        tax
        longa
        lda     f:_c068e9+2,x
        sec
        sbc     f:_c068e9,x
        lsr
        sta     $2c
        lda     f:_c068e9,x
        tax
        lda     $06
        shorta
        ldy     $06
@680a:  lda     f:_c068f5,x
        sta     $0300,y
        lda     f:_c068f5+1,x
        sta     $0301,y
        lda     #$4a
        sta     $0302,y
        lda     $3f
        and     #$20
        ora     #$06
        sta     $0303,y
        iny4
        inx2
        dec     $2c
        bne     @680a
        rts

; ---------------------------------------------------------------------------

_c06831:
@6831:  lda     #$0a
        sta     $df
        lda     #$0b
        sta     $e0
        jsr     _c0baae       ; change color palette
        ldx     $06
@683e:  lda     f:_c06859,x
        sta     $0360,x
        inx
        cpx     #$0090
        bne     @683e
        ldx     $06
@684d:  lda     #$aa
        sta     $0416,x
        inx
        cpx     #$000a
        bne     @684d
        rts

; ---------------------------------------------------------------------------

_c06859:
        .byte   $31,$1f,$00,$35
        .byte   $41,$1f,$02,$35
        .byte   $51,$1f,$04,$35
        .byte   $61,$1f,$06,$35
        .byte   $71,$1f,$04,$35
        .byte   $81,$1f,$04,$75
        .byte   $91,$1f,$06,$75
        .byte   $a1,$1f,$04,$75
        .byte   $b1,$1f,$02,$75
        .byte   $c1,$1f,$00,$75
        .byte   $31,$2f,$08,$35
        .byte   $c1,$2f,$08,$75
        .byte   $31,$3f,$0a,$35
        .byte   $c1,$3f,$0a,$75
        .byte   $31,$4f,$0c,$35
        .byte   $c1,$4f,$0c,$75
        .byte   $31,$5f,$0a,$35
        .byte   $c1,$5f,$0a,$75
        .byte   $31,$6f,$0a,$b5
        .byte   $c1,$6f,$0a,$f5
        .byte   $31,$7f,$0c,$b5
        .byte   $c1,$7f,$0c,$f5
        .byte   $31,$8f,$0a,$b5
        .byte   $c1,$8f,$0a,$f5
        .byte   $31,$9f,$08,$b5
        .byte   $c1,$9f,$08,$f5
        .byte   $31,$af,$00,$b5
        .byte   $41,$af,$02,$b5
        .byte   $51,$af,$04,$b5
        .byte   $61,$af,$06,$b5
        .byte   $71,$af,$04,$b5
        .byte   $81,$af,$04,$f5
        .byte   $91,$af,$06,$f5
        .byte   $a1,$af,$04,$f5
        .byte   $b1,$af,$02,$f5
        .byte   $c1,$af,$00,$f5

; ---------------------------------------------------------------------------

_c068e9:
        .addr   _c068f5 - _c068f5
        .addr   _c06915 - _c068f5
        .addr   _c06927 - _c068f5
        .addr   _c06953 - _c068f5
        .addr   _c06959 - _c068f5
        .addr   _c06961 - _c068f5

_c068f5:
        .byte   $51,$43,$5d,$91,$60,$80,$67,$6a,$6a,$55,$81,$5b,$97,$46,$9b,$49
        .byte   $9b,$71,$aa,$69,$a2,$5b,$ad,$55,$a9,$46,$ad,$93,$92,$68,$b1,$42

_c06915:
        .byte   $66,$66,$4b,$68,$68,$72,$69,$78,$6a,$86,$90,$7c,$78,$61,$8a,$58
        .byte   $8f,$4a

_c06927:
        .byte   $5d,$91,$67,$6a,$6a,$55,$81,$5b,$9b,$49,$9b,$71,$ad,$55,$ad,$93
        .byte   $92,$68,$66,$66,$4b,$68,$68,$72,$69,$78,$6a,$86,$78,$61,$8a,$58
        .byte   $8f,$4a,$a7,$93,$82,$7c,$56,$6c,$9d,$5b,$b1,$42

_c06953:
        .byte   $5c,$4f,$78,$61,$4d,$68

_c06959:
        .byte   $5c,$4f,$a2,$5b,$b4,$82,$4d,$68

; ---------------------------------------------------------------------------

_c06961:
        .byte   $7c,$5c,$7e,$5d,$7f,$5f,$81,$60,$82,$62,$84,$63,$85,$65,$87,$66
        .byte   $97,$76,$95,$74,$93,$72,$92,$70,$90,$6f,$8e,$6d,$8c,$6b,$8a,$6a
        .byte   $5f,$7e,$64,$7c,$69,$79,$6e,$76,$74,$73,$79,$70,$7e,$6e,$83,$6b
        .byte   $a6,$58,$a2,$5a,$9e,$5c,$9b,$5e,$97,$60,$93,$62,$90,$64,$8c,$66

; ---------------------------------------------------------------------------

; [ load world map layout ]

.scope WorldMod
        Start = bank_start WorldMod
        PosY = Start
        PosX = Start + 1
        Size = Start + 2
        Switch = Start + 3
        TilePtr = Start + 4

        ARRAY_LENGTH = 5
.endscope

.scope WorldModTiles
        ARRAY_LENGTH = 299
.endscope

_c069a1:
        lda     #^WorldTilemap
        sta     $25
        lda     $ba
        and     #$02
        bne     @69b2
        lda     $76
        sec
        sbc     #$1f
        bra     @69b7
@69b2:  lda     $76
        clc
        adc     #$20
@69b7:  sta     $0f
        longa
        sta     $23
        asl     $23
        xba
        and     #$3f00
        sta     $26
        lda     $0ad6       ; map index
        xba
        asl
        clc
        adc     $23
        tax
        lda     f:WorldTilemapPtrs,x   ; pointer to world layout
        sta     $23

; check if we are in the 2nd bank - this code is very sloppy and might need
; to be modified if any major world map changes are made
        cpx     #$0800
        bcc     @69e4                   ; skip if slice id less than $0400
        lda     f:WorldTilemapPtrs,x
        cmp     #$8000                  ; skip if low pointer is > $8000
        bcs     @69e4
        inc     $25                     ; increment the bank

@69e4:  lda     $06
        shorta
        ldx     $06
        txy
@69eb:  lda     [$23],y
        sta     $7f7622,x
        iny
        inx
        cpx     #$0100
        bne     @69eb
        ldx     $06
        stx     $23
        ldy     $26
@69fe:  ldx     $23
        lda     $7f7622,x
        cmp     #$c0
        bcs     @6a36
        cmp     #$0c
        beq     @6a1f
        cmp     #$1c
        beq     @6a1f
        cmp     #$2c
        beq     @6a1f
        inx
        stx     $23
        tyx
        sta     $7f0000,x
        iny
        bra     @6a4e
@6a1f:  inx
        stx     $23
        tyx
        sta     $7f0000,x
        inc
        sta     $7f0001,x
        inc
        sta     $7f0002,x
        iny3
        bra     @6a4e
@6a36:  sec
        sbc     #$c0
        sta     $09
        lda     $7f7623,x
        inx2
        stx     $23
        tyx
@6a44:  sta     $7f0000,x
        inx
        dec     $09
        bpl     @6a44
        txy
@6a4e:  tya
        bne     @69fe
        longa
        lda     $0ad6       ; map index
        asl
        tax
        lda     f:WorldModPtrs+2,x
        sta     $0d
        lda     f:WorldModPtrs,x
        tax
        lda     $06
        shorta
@6a67:  cpx     $0d
        beq     @6abc
        lda     f:WorldMod::PosY,x   ; y
        cmp     $0f
        beq     @6a7b
@6a73:  inx6
        bra     @6a67
@6a7b:  lda     f:WorldMod::Switch,x
        clc
        adc     #$d0
        jsr     _c0ca3c       ; get event flag $01xx
        cmp     #$00
        beq     @6a73
        phx
        lda     f:WorldMod::Size,x
        sta     $2c
        lda     f:WorldMod::PosX,x
        longa
        clc
        adc     $26
        tay
        lda     f:WorldMod::TilePtr,x
        tax
        lda     $06
        shorta
        lda     #$7f
        pha
        plb
@6aa7:  lda     f:WorldMod::Start,x
        sta     0,y
        inx
        iny
        dec     $2c
        bne     @6aa7
        lda     #$00
        pha
        plb
        plx
        jmp     @6a73
@6abc:  rts

; pointers to world map modification data
WorldModPtrs:
        ptr_tbl WorldMod
        end_ptr WorldMod

; unused
@6ac8:  rts

; ---------------------------------------------------------------------------


; [ load map layout ]

; +x: destination address (+$7f0000)
; +y: map layout index

_c06aca:
        stx     $71
        lda     #^SubTilemap
        sta     $25
        longa
        ldx     $06
        cpy     #$0000                  ; this could be tya instead
        beq     @6af3
        inx2
        dey
        beq     @6af3
@6ade:  inx2
        lda     f:SubTilemapPtrs,x
        dex2
        cmp     f:SubTilemapPtrs,x
        bcs     @6aee
        inc     $25                     ; figure out which bank we're in
@6aee:  inx2
        dey
        bne     @6ade
@6af3:  lda     f:SubTilemapPtrs,x      ; pointer to map layout
        sta     $04f0
        sta     $23
        lda     $0b71
        sta     $04f3
        lda     $06
        shorta
        lda     $25
        sta     $04f2
        lda     #$7f
        sta     $04f5
        ldy     $06
        lda     [$23],y
        bne     @6b1b
        jsl     Decomp_ext
        rts
@6b1b:  ldx     $71
        jsr     _c06b21                 ; clear tile layout
        rts

; ---------------------------------------------------------------------------

; [ fill map tile layout ]

;  a: fill value
; +x: data offset

_c06b21:
@6b21:  ldy     #$1000
@6b24:  sta     $7f0000,x
        inx
        dey
        bne     @6b24
        rts

; ---------------------------------------------------------------------------

; [ load world tileset ]

; +y: source address
;  a: source bank

_c06b2d:
        pha
        plb
        ldx     $06
@6b31:  lda     $0000,y
        sta     $7f6e22,x
        iny
        inx
        cpx     #$0800
        bne     @6b31
        lda     $06
        pha
        plb
        rts

; ---------------------------------------------------------------------------

; [ load tile properties and tileset ]

_c06b44:
        lda     $1111       ; tile properties
        asl
        tax
        longa
        lda     f:MapTilePropPtrs,x
        clc
        adc     #near MapTilePropPtrs
        sta     $04f0
        lda     $06
        shorta
        lda     #^MapTilePropPtrs
        sta     $04f2
        ldx     #$1186      ; 00/1186
        stx     $04f3
        lda     #$00
        sta     $04f5
        jsl     Decomp_ext
        lda     $1114       ; tileset
        asl
        tax
        longa
        lda     f:MapTilesetPtrs,x   ; pointers to tilesets
        clc
        adc     #near MapTilesetPtrs
        sta     $04f0
        lda     $06
        shorta
        lda     #^MapTilesetPtrs
        sta     $04f2
        ldx     #$6e22      ; 7f/6e22
        stx     $04f3
        lda     #$7f
        sta     $04f5
        jsl     Decomp_ext
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06b99:
        ldx     #$1000
@6b9c:  sta     $7effff,x
        sta     $7f0fff,x
        sta     $7f1fff,x
        dex
        bne     @6b9c
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06bac:
@6bac:  longa
        lda     $0ad9
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        and     #$0fff
        sta     $63
        lda     $0ad8
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        and     #$0fff
        sta     $61
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ init bg scroll positions ]

_c06bdb:
@6bdb:  longa
        lda     $0ad9       ; y position ???
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        and     #$0fff
        sta     $63
        lda     $0ad8       ; x position ???
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        and     #$0fff
        sta     $61
        lda     $78         ; bg2 v-scroll
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        sta     $67
        lda     $77         ; bg2 h-scroll
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        sta     $65
        lda     $7a         ; bg3 v-scroll
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        sta     $6b
        lda     $79         ; bg3 h-scroll
        and     #$00ff
        sec
        sbc     #$0007
        asl5
        sta     $69
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06c4a:
        jsr     _c06bac
        lda     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        lda     #$40
@6c59:  pha
        jsr     _c069a1
        jsr     _c06513
        jsr     _c06465
        inc     $76
        pla
        dec
        bne     @6c59
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06c6a:
        ldx     $0ad4
        cpx     #$00be                  ; airship deck map
        bne     @6c9c
        stz     $0b6d
        ldx     #$2000
        stx     $2e
        stx     $2c
        jsr     FillVRAM
        lda     #$80
        sta     hVMAINC
        ldy     #$2702
        jsr     _c06c9d
        ldy     #$2d82
        jsr     _c06c9d
        ldy     #$2342
        jsr     _c06cd4
        ldy     #$29c2
        jsr     _c06cd4
@6c9c:  rts

; ---------------------------------------------------------------------------

; [  ]

_c06c9d:
@6c9d:  ldx     $06
@6c9f:  sty     hVMADDL
@6ca2:  lda     f:_c0cf00,x
        sta     hVMDATAL
        inx
        lda     f:_c0cf00,x
        sta     hVMDATAH
        inx
        txa
        and     #$1f
        bne     @6ca2
        longa
        tya
        clc
        adc     #$0020
        tay
        and     #$03e0
        bne     @6cca
        tya
        clc
        adc     #$0400
        tay
@6cca:  lda     $06
        shorta
        cpx     #$01e0
        bne     @6c9f
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06cd4:
@6cd4:  ldx     #$01e0
@6cd7:  sty     hVMADDL
@6cda:  lda     f:_c0cf00,x
        sta     hVMDATAL
        inx
        lda     f:_c0cf00,x
        sta     hVMDATAH
        inx
        txa
        and     #$1f
        bne     @6cda
        longa
        tya
        clc
        adc     #$0020
        tay
        and     #$03e0
        bne     @6d02
        tya
        clc
        adc     #$0400
        tay
@6d02:  lda     $06
        shorta
        cpx     #$0340
        bne     @6cd7
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06d0c:
        lda     #$10
        sta     $09
        jsr     _c06bdb       ; init bg scroll positions
        lda     $0ad9       ; y position
        sta     $0a
        lda     $78
        sta     $0b
        lda     $7a
        sta     $0c
@6d20:  lda     $0ad8       ; x position
        sta     $75
        lda     $0a
        sta     $76
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
        lda     $1121
        and     #$40
        bne     @6d53
        lda     $77
        sta     $75
        lda     $0b
        sta     $76
        lda     #$01
        ldx     #$1000
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
        bra     @6d89
@6d53:  lda     #$07
        sta     $75
        lda     $0b
        sec
        sbc     $78
        clc
        adc     #$08
        sta     $76
        lda     #$01
        ldx     #$1000
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
        lda     #$17
        sta     $75
        lda     $0b
        sec
        sbc     $78
        clc
        adc     #$08
        sta     $76
        lda     #$01
        ldx     #$1000
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
@6d89:  lda     $1121
        bmi     @6da5
        lda     $79
        sta     $75
        lda     $0c
        sta     $76
        lda     #$01
        ldx     #$2000
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
        bra     @6ddb
@6da5:  lda     #$07
        sta     $75
        lda     $0c
        sec
        sbc     $7a
        clc
        adc     #$08
        sta     $76
        lda     #$01
        ldx     #$2000
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
        lda     #$17
        sta     $75
        lda     $0c
        sec
        sbc     $7a
        clc
        adc     #$08
        sta     $76
        lda     #$01
        ldx     #$2000
        stx     $71
        jsr     _c0707d
        jsr     _c06e7a
@6ddb:  inc     $0a
        inc     $0b
        inc     $0c
        dec     $09
        jne     @6d20
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06de9:
@6de9:  stz     hMDMAEN
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        stz     hDMA0::ADDR_B
        rts

; ---------------------------------------------------------------------------

; [ horizontal scrolling ]

_c06df5:
        longa
        lda     $71
        xba
        lsr
        sta     $17
        lsr
        clc
        adc     $17
        tay
        lda     $06
        shorta
        lda     #$81        ; increment vertically
        sta     hVMAINC
        jsr     _c06de9
        lda     #$01
        sta     hDMA0::CTRL
        longa
        lda     $71
        xba
        asl3
        clc
        adc     #$16f3
        sta     hDMA0::ADDR
        lda     $06
        shorta
        ldx     $7f,y
        stx     hVMADDL
        ldx     $7b,y
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $81,y
        stx     hVMADDL
        ldx     $7d,y
        beq     @6e46
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@6e46:  longa
        lda     $71
        xba
        asl3
        clc
        adc     #$1733
        sta     hDMA0::ADDR
        lda     $06
        shorta
        ldx     $83,y
        stx     hVMADDL
        ldx     $7b,y
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $85,y
        stx     hVMADDL
        ldx     $7d,y
        beq     @6e79
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@6e79:  rts

; ---------------------------------------------------------------------------

; [ vertical scrolling ]

_c06e7a:
@6e7a:  longa
        lda     $71         ; pointer to map tile data ($7f0000)
        xba
        lsr
        sta     $17
        lsr
        clc
        adc     $17
        tay
        lda     $06
        shorta
        lda     #$80
        sta     hVMAINC
        jsr     _c06de9
        lda     #$01
        sta     hDMA0::CTRL
        ldx     $7f,y
        stx     hVMADDL
        longa
        lda     $71
        xba
        asl3
        clc
        adc     #$16f3      ; dma source
        sta     hDMA0::ADDR
        lda     $06
        shorta
        ldx     $7b,y
        stx     hDMA0::SIZE       ; dma size
        lda     #$01
        sta     hMDMAEN
        ldx     $81,y
        stx     hVMADDL
        stz     hMDMAEN
        ldx     $7d,y
        beq     @6ece
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@6ece:  ldx     $83,y
        stx     hVMADDL
        stz     hMDMAEN
        longa
        lda     $71
        xba
        asl3
        clc
        adc     #$1733
        sta     hDMA0::ADDR
        lda     $06
        shorta
        ldx     $7b,y
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        ldx     $85,y
        stx     hVMADDL
        stz     hMDMAEN
        ldx     $7d,y
        beq     @6f07
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
@6f07:  rts

; ---------------------------------------------------------------------------

; [  ]

_c06f08:
        lda     $76
        and     #$3f
        xba
        longa
        lsr2
        ora     $73
        sta     $0d
        lda     $75
        and     #$003f
        ora     $0d
        tax
        lda     $06
        shorta
        ldy     $06
        lda     $2d
        sta     $15
@6f27:  lda     $2c
        sta     $13
        phx
@6f2c:  lda     $16b3,y
        sta     $7f0000,x
        iny
        inx
        dec     $13
        bne     @6f2c
        longa
        pla
        clc
        adc     #$0040
        tax
        lda     $06
        shorta
        dec     $15
        bne     @6f27
        ldy     $73
        bne     @6f59
        lda     $0ad8
        sta     $75
        lda     $0ad9
        sta     $76
        bra     @6f70
@6f59:  cpy     #$1000
        bne     @6f68
        lda     $77
        sta     $75
        lda     $78
        sta     $76
        bra     @6f70
@6f68:  lda     $79
        sta     $75
        lda     $7a
        sta     $76
@6f70:  lda     $76
        sec
        sbc     #$08
        and     #$3f
        xba
        longa
        lsr2
        ora     $73
        sta     $23
        lda     $06
        shorta
        lda     $75
        sec
        sbc     #$07
        and     #$3f
        ora     $23
        sta     $23
        lda     $76
        sec
        sbc     #$08
        and     #$0f
        xba
        longa
        lsr
        sta     $26
        lda     $06
        shorta
        lda     $75
        sec
        sbc     #$07
        sta     $0d
        stz     $0e
        and     #$10
        bne     @6fb2
        ldy     #$0000
        bra     @6fb5
@6fb2:  ldy     #$0800
@6fb5:  lda     $0d
        and     #$0f
        asl2
        sta     $0d
        longa
        tya
        clc
        adc     $0d
        clc
        adc     $26
        sta     $26
        and     #$0f80
        eor     #$0800
        sta     $29
        lda     $06
        shorta
        lda     #$7f
        pha
        plb
        longa
        lda     #$0010
        sta     $11
@6fdf:  ldx     $23
        stx     $0d
        ldy     $26
        jsr     _c0703e
        ldy     $29
        jsr     _c0703e
        lda     $23
        clc
        adc     #$0040
        and     #$0fff
        ora     $73
        sta     $23
        lda     $26
        clc
        adc     #$0080
        sta     $26
        and     #$07ff
        cmp     #$0080
        bcs     @7012
        lda     $26
        sec
        sbc     #$0800
        sta     $26
@7012:  lda     $29
        clc
        adc     #$0080
        sta     $29
        and     #$07ff
        cmp     #$0080
        bcs     @702a
        lda     $29
        sec
        sbc     #$0800
        sta     $29
@702a:  dec     $11
        bne     @6fdf
        lda     $06
        shorta
        pha
        plb
        inc     $a0
@7036:  jsr     WaitVBlank
        lda     $a0
        bne     @7036
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0703e:
@703e:  .a16
        ldx     $0d
        lda     a:$0000,x
        and     #$00ff
        asl
        tax
        lda     $6e22,x
        sta     $7622,y
        lda     $7022,x
        sta     $7624,y
        lda     $7222,x
        sta     $7662,y
        lda     $7422,x
        sta     $7664,y
        lda     $0d
        inc
        and     #$003f
        sta     $0f
        lda     $0d
        and     #$ffc0
        ora     $0f
        sta     $0d
        tya
        clc
        adc     #$0004
        tay
        and     #$003f
        bne     @703e
        rts
        .a8

; ---------------------------------------------------------------------------

; [  ]

_c0707d:
@707d:  sta     $70
        and     #$01
        jne     @714b
        lda     $76
        sec
        sbc     #$07
        and     #$3f
        sta     $24
        stz     $23
        longa
        lda     $23
        lsr2
        clc
        adc     $71
        sta     $23
        lda     $06
        shorta
        lda     $70
        and     #$02
        beq     @70ae
        lda     $75
        clc
        adc     #$09
        jmp     @70b3
@70ae:  lda     $75
        sec
        sbc     #$08
@70b3:  and     #$3f
        clc
        adc     $23
        clc
        sta     $23
        ldx     $23
        stx     $26
        jsr     _c07221
@70c2:  ldx     $23
        lda     $7f0000,x
        longa
        asl
        tax
        lda     $7f6e22,x   ; top left tile
        sta     $16f3,y
        lda     $7f7222,x   ; top right tile
        sta     $16f5,y
        lda     $7f7022,x   ; bottom left tile
        sta     $1733,y
        lda     $7f7422,x   ; bottom right tile
        sta     $1735,y
        lda     $23
        clc
        adc     #$0040
        and     #$0fff
        sta     $23
        lda     $26
        and     #$f000
        clc
        adc     $23
        sta     $23
        iny4
        tya
        and     #$003f
        shorta
        bne     @70c2
        jsr     _c0722f
        jsr     _c07241
        longa
        lda     $71
        lsr
        clc
        adc     #$4800
        clc
        adc     $26
        sta     $7f,x
        and     #$03c0
        lsr4
        sta     $7d,x
        lda     #$0040
        sec
        sbc     $7d,x
        sta     $7b,x
        lda     $06
        shorta
        lda     $80,x
        sta     $84,x
        and     #$fc
        sta     $82,x
        sta     $86,x
        lda     $7f,x
        and     #$1f
        sta     $81,x
        inc
        sta     $85,x
        lda     $7f,x
        inc
        sta     $83,x
        rts
@714b:  lda     $70
        and     #$02
        bne     @7159
        lda     $76
        sec
        sbc     #$08
        jmp     @715e
@7159:  lda     $76
        clc
        adc     #$08
@715e:  and     #$3f
        sta     $24
        stz     $23
        longa
        lda     $23
        lsr2
        clc
        adc     $71
        sta     $23
        lda     $06
        shorta
        lda     $75
        sec
        sbc     #$07
        and     #$3f
        clc
        adc     $23
        sta     $23
        ldx     $23
        stx     $26
        jsr     _c07221
@7186:  ldx     $23
        lda     $7f0000,x
        longa
        asl
        tax
        lda     $7f6e22,x
        sta     $16f3,y
        lda     $7f7022,x
        sta     $16f5,y
        lda     $7f7222,x
        sta     $1733,y
        lda     $7f7422,x
        sta     $1735,y
        lda     $06
        shorta
        lda     $23
        inc
        and     #$3f
        sta     $29
        lda     $23
        and     #$c0
        ora     $29
        sta     $23
        iny4
        tya
        and     #$3f
        bne     @7186
        jsr     _c0722f
        jsr     _c07241
        lda     $27
        sta     $80,x
        lda     $26
        sta     $7f,x
        lda     $7f,x
        and     #$1f
        asl
        sta     $7d,x
        stz     $7e,x
        lda     #$40
        sec
        sbc     $7d,x
        sta     $7b,x
        stz     $7c,x
        lda     $7f,x
        and     #$e0
        sta     $81,x
        lda     $80,x
        clc
        adc     #$04
        and     #$07
        sta     $82,x
        longa
        lda     $71
        lsr
        clc
        adc     #$4800
        pha
        clc
        adc     $7f,x
        sta     $7f,x
        pla
        clc
        adc     $81,x
        sta     $81,x
        lda     $7f,x
        clc
        adc     #$0020
        sta     $83,x
        lda     $81,x
        clc
        adc     #$0020
        sta     $85,x
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c07221:
@7221:  longa
        lda     $71
        xba
        asl3
        tay
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0722f:
@722f:  longa
        lda     $71
        xba
        lsr
        sta     $0d
        lsr
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

_c07241:
@7241:  longa
        lda     $26
        and     #$03c0
        sta     $29
        lda     $26
        and     #$0010
        beq     @7259
        lda     $29
        clc
        adc     #$0400
        sta     $29
@7259:  lda     $26
        and     #$000f
        asl
        clc
        adc     $29
        and     #$07ff
        sta     $26
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

        .include "world-mod.asm"

; ---------------------------------------------------------------------------

; [ show dialog ]

ShowDlg:
@83ae:  jsr     GetDlgPtr
@83b1:  jsr     _c08f54
        ldx     $b1
        lda     f:Dlg,x
        beq     @83fa
        jsr     LoadDlgText
        ldy     $06
        sty     $ab
@83c3:  jsr     _c08d3b
        jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        ldy     $ab
        cpy     #$0040
        bne     @83c3
        lda     $b8
        bne     @83e6
        jsr     _c04aad
        jsr     _c04ac1       ; wait for keypress
        jsr     _c04aad
@83e6:  stz     $b8
        inc     $a5
        jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        lda     $b3
        beq     @83b1
@83fa:  jsr     _c094a8
        jsr     WaitVBlank
        rts

; ---------------------------------------------------------------------------

; [ get pointer to dialog ]

GetDlgPtr:
@8401:  stz     $b3         ;
        longa
        lda     $af         ; dialog index
        and     #$7fff
        asl
        tax
        lda     f:DlgPtrs,x   ; pointer to dialog
        sta     $b1
        lda     $06
        shorta
        lda     $b0
        bmi     @841f
        lda     #$01        ; dialog window at top of screen
        jmp     @8421
@841f:  lda     #$08        ; dialog window at bottom of screen
@8421:  sta     $b4
        jsr     _c09440
        rts

; ---------------------------------------------------------------------------

; [ load dialogue text ]

LoadDlgText:
@8427:  ldx     $06
        stx     $ab
_842b:  ldx     $b1
        lda     f:Dlg,x
        cmp     #$ff
        beq     _c08451
        cmp     #$cd
        jcs     _c08508
        cmp     #$20
        bcs     _c08451
        longa
        asl
        tax
        lda     f:DlgCmdTbl,x
        sta     $23
        lda     $06
        shorta
        jmp     ($0b23)

_c08451:
@8451:  ldy     $ab
        sta     $19d3,y
        iny
        sty     $ab

_c08459:
@8459:  ldy     $b1
        iny
        sty     $b1
        ldy     $ab
        cpy     #$0040
        bne     _842b
        rts

_c08466:
        ldx     $b1
        ldy     $ab
        lda     f:Dlg,x
        sta     $19d3,y
        iny
        sty     $ab
        jmp     _c08459

_c08477:
        ldx     $b1
        ldy     $ab
        lda     f:Dlg,x
        sta     $19d3,y
        inx
        iny
        lda     f:Dlg,x
        sta     $19d3,y
        stx     $b1
        iny
        sty     $ab
        jmp     _c08459

_c08493:
        ldy     $ab
        lda     #$ff
        sta     $19d3,y
        iny
        cpy     #$0040
        beq     @84a5
        lda     #$00
        sta     $19d3,y
@84a5:  ldy     #$0040
        sty     $ab
        inc     $b3
        jmp     _c08459

_c084af:
        longa
        lda     $ab
        and     #$fff0
        clc
        adc     #$0010
        sta     $23
        lda     $06
        shorta
        ldy     $ab
        cpy     $23
        beq     @84e4
        lda     #$ff
        sta     $19d3,y
        iny
        cpy     $23
        beq     @84e4
        lda     #$01
        sta     $19d3,y
        iny
        cpy     $23
        beq     @84e4
        lda     #$ff
@84dc:  sta     $19d3,y
        iny
        cpy     $23
        bne     @84dc
@84e4:  sty     $ab
        jmp     _c08459

_c084e9:
        ldx     $06
        ldy     $ab
@84ed:  lda     $0990,x
        cmp     #$ff
        beq     @8503
        sta     $19d3,y
        lda     #$00
        sta     $1a13,y
        iny
        inx
        cpx     #$0006
        bne     @84ed
@8503:  sty     $ab
        jmp     _c08459

_c08508:
@8508:  sec
        sbc     #$b2
        bra     _c08512

_c0850d:
        txa
        lsr
        sec
        sbc     #3

_c08512:
@8512:  tax
        lda     f:_c08541,x
        sta     $0d
        stz     $0e
        longa
        txa
        asl3
        tax
        lda     $06
        shorta
        ldy     $ab
@8528:  lda     f:_c0858f,x
        sta     $19d3,y
        lda     f:_c087f7,x
        sta     $1a13,y
        inx
        iny
        dec     $0d
        bne     @8528
        sty     $ab
        jmp     _c08459

; length of each MTE code

; $03-$1F (11 values)
_c08541:
        .byte         5,5,4,2,2,4,3,2,2,0,4,2,0
        .byte   0,0,0,2,3,3,2,0,5,3,4,5,4,6

; $CD-$FF
        .byte                             2,1,2
        .byte   1,1,1,1,1,4,1,1,1,2,1,1,1,2,6,2
        .byte   2,2,2,1,2,1,3,2,2,1,1,2,2,4,5,3
        .byte   3,2,3,3,4,2,2,2,2,4,3,2,2,8,6,1

; low bytes of MTE codes
_c0858f:
        .byte   $6e,$a8,$78,$7e,$aa,$00,$00,$00  ; $03: "クリスタル" (crystal)
        .byte   $7e,$8c,$6e,$c5,$b8,$00,$00,$00  ; $04: "タイクーン" (tycoon)
        .byte   $64,$c4,$64,$c4,$00,$00,$00,$00  ; $05: "ファファ"
        .byte   $37,$bf,$00,$00,$00,$00,$00,$00  ; $06: "じゃ"
        .byte   $8d,$ab,$00,$00,$00,$00,$00,$00  ; $07: "いる"
        .byte   $ff,$ff,$ff,$ff,$00,$00,$00,$00  ; $08: 4 spaces
        .byte   $ff,$ff,$ff,$00,$00,$00,$00,$00  ; $09: 3 spaces
        .byte   $ff,$ff,$00,$00,$00,$00,$00,$00  ; $0A: 2 spaces
        .byte   $12,$13,$00,$00,$00,$00,$00,$00  ; $0B: "魔物" (monster)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $0C: wait
        .byte   $24,$9b,$52,$57,$00,$00,$00,$00  ; $0D: "風の神殿" (wind temple)
        .byte   $04,$0a,$00,$00,$00,$00,$00,$00  ; $0E: "飛竜" (hiryuu)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $0F: new page
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $10: gil amount
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $11: item name
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $12: spell name
        .byte   $07,$0d,$00,$00,$00,$00,$00,$00  ; $13: "封印"
        .byte   $76,$46,$d0,$00,$00,$00,$00,$00  ; $14: "シド「" (cid)
        .byte   $9e,$46,$d0,$00,$00,$00,$00,$00  ; $15: "ミド「" (mid)
        .byte   $05,$06,$00,$00,$00,$00,$00,$00  ; $16: "世界" (world)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $17: wait (variable)
        .byte   $8e,$6e,$78,$44,$78,$00,$00,$00  ; $18: "エクスデス" (exdeath)
        .byte   $ac,$92,$d0,$00,$00,$00,$00,$00  ; $19: "レナ「" (lenna)
        .byte   $2a,$a6,$64,$d0,$00,$00,$00,$00  ; $1A: "ガラフ「" (galuf)
        .byte   $64,$c4,$a8,$78,$d0,$00,$00,$00  ; $1B: "ファリス「" (faris)
        .byte   $6e,$aa,$aa,$d0,$00,$00,$00,$00  ; $1C: "クルル「" (krile)
        .byte   $91,$37,$8d,$81,$bf,$b9,$00,$00  ; $1D: "おじいちゃん" (grandpa)
        .byte   $c9,$c9,$00,$00,$00,$00,$00,$00  ; $CD: "!!"
        .byte   $ce,$00,$00,$00,$00,$00,$00,$00  ; $CE: "／"
        .byte   $bd,$85,$00,$00,$00,$00,$00,$00  ; $CF: "って"
        .byte   $d0,$00,$00,$00,$00,$00,$00,$00  ; $D0: "「"
        .byte   $d1,$00,$00,$00,$00,$00,$00,$00  ; $D1: "」"
        .byte   $d2,$00,$00,$00,$00,$00,$00,$00  ; $D2: "。"
        .byte   $d3,$00,$00,$00,$00,$00,$00,$00  ; $D3: "A"
        .byte   $d4,$00,$00,$00,$00,$00,$00,$00  ; $D4: "B"
        .byte   $1b,$95,$08,$ad,$00,$00,$00,$00  ; $D5: "手に入れ"
        .byte   $d6,$00,$00,$00,$00,$00,$00,$00  ; $D6: "Y"
        .byte   $d7,$00,$00,$00,$00,$00,$00,$00  ; $D7: "L"
        .byte   $d8,$00,$00,$00,$00,$00,$00,$00  ; $D8: "R"
        .byte   $93,$8d,$00,$00,$00,$00,$00,$00  ; $D9: "ない"
        .byte   $da,$00,$00,$00,$00,$00,$00,$00  ; $DA: "H"
        .byte   $db,$00,$00,$00,$00,$00,$00,$00  ; $DB: "M"
        .byte   $dc,$00,$00,$00,$00,$00,$00,$00  ; $DC: "P"
        .byte   $c7,$c7,$00,$00,$00,$00,$00,$00  ; $DD: "……"
        .byte   $3f,$8d,$37,$c3,$89,$25,$00,$00  ; $DE: "だいじょうぶ"
        .byte   $61,$e3,$00,$00,$00,$00,$00,$00  ; $DF: "は、"
        .byte   $b9,$3f,$00,$00,$00,$00,$00,$00  ; $E0: "んだ"
        .byte   $85,$8d,$00,$00,$00,$00,$00,$00  ; $E1: "てい"
        .byte   $77,$7f,$00,$00,$00,$00,$00,$00  ; $E2: "した"
        .byte   $e3,$00,$00,$00,$00,$00,$00,$00  ; $E3: "、"
        .byte   $77,$85,$00,$00,$00,$00,$00,$00  ; $E4: "して"
        .byte   $e5,$00,$00,$00,$00,$00,$00,$00  ; $E5: "『"
        .byte   $91,$0f,$03,$00,$00,$00,$00,$00  ; $E6: "お父様" (father)
        .byte   $c9,$cb,$00,$00,$00,$00,$00,$00  ; $E7: "!?"
        .byte   $45,$79,$00,$00,$00,$00,$00,$00  ; $E8: "です"
        .byte   $e9,$00,$00,$00,$00,$00,$00,$00  ; $E9: "（"
        .byte   $ea,$00,$00,$00,$00,$00,$00,$00  ; $EA: "）"
        .byte   $73,$9b,$00,$00,$00,$00,$00,$00  ; $EB: "この"
        .byte   $9b,$02,$00,$00,$00,$00,$00,$00  ; $EC: "の力"
        .byte   $70,$aa,$2a,$c5,$00,$00,$00,$00  ; $ED: "ケルガー" (kelgar)
        .byte   $86,$d7,$87,$62,$a7,$00,$00,$00  ; $EE: "古代図書館" (ancient library)
        .byte   $1c,$bd,$85,$00,$00,$00,$00,$00  ; $EF: "言って"
        .byte   $2b,$0b,$d0,$00,$00,$00,$00,$00  ; $F0: "兵士「" (soldier)
        .byte   $6b,$a7,$00,$00,$00,$00,$00,$00  ; $F1: "から"
        .byte   $2c,$02,$0c,$00,$00,$00,$00,$00  ; $F2: "かりょくせん" (fire ship)
        .byte   $0e,$3d,$6f,$00,$00,$00,$00,$00  ; $F3: "海ぞく" (pirates)
        .byte   $8d,$37,$c3,$89,$00,$00,$00,$00  ; $F4: "いじょう"
        .byte   $2b,$e3,$00,$00,$00,$00,$00,$00  ; $F5: "が、"
        .byte   $7f,$81,$00,$00,$00,$00,$00,$00  ; $F6: "たち"
        .byte   $7f,$9b,$00,$00,$00,$00,$00,$00  ; $F7: "たの"
        .byte   $9d,$79,$00,$00,$00,$00,$00,$00  ; $F8: "ます"
        .byte   $6f,$3f,$75,$8d,$00,$00,$00,$00  ; $F9: "ください" (please)
        .byte   $6b,$bd,$7f,$00,$00,$00,$00,$00  ; $FA: "かった"
        .byte   $7f,$c9,$00,$00,$00,$00,$00,$00  ; $FB: "た!"
        .byte   $95,$e3,$00,$00,$00,$00,$00,$00  ; $FC: "に、"
        .byte   $8d,$93,$8d,$6b,$a7,$93,$b9,$3f  ; $FD: "いないからなんだ"
        .byte   $20,$38,$9b,$61,$35,$9d,$00,$00  ; $FE: "次元の狭間" (dimensional rift)

; high bytes of MTE codes
_c087f7:
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $03: "クリスタル" (crystal)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $04: "タイクーン" (tycoon)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $05: "ファファ"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $06: "じゃ"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $07: "いる"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $08: 4 spaces
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $09: 3 spaces
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $0A: 2 spaces
        .byte   $01,$01,$00,$00,$00,$00,$00,$00  ; $0B: "魔物" (monster)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $0C: wait
        .byte   $01,$00,$01,$01,$00,$00,$00,$00  ; $0D: "風の神殿" (wind temple)
        .byte   $01,$01,$00,$00,$00,$00,$00,$00  ; $0E: "飛竜" (hiryuu)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $0F: new page
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $10: gil amount
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $11: item name
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $12: spell name
        .byte   $01,$01,$00,$00,$00,$00,$00,$00  ; $13: "封印"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $14: "シド「" (cid)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $15: "ミド「" (mid)
        .byte   $01,$01,$00,$00,$00,$00,$00,$00  ; $16: "世界" (world)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $17: "\\wait["
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $18: "エクスデス" (exdeath)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $19: "レナ「" (lenna)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $1A: "ガラフ「" (galuf)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $1B: "ファリス「" (faris)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $1C: "クルル「" (krile)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $1D: "おじいちゃん" (grandpa)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $CD: "!!"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $CE: "／"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $CF: "って"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D0: "「"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D1: "」"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D2: "。"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D3: "A"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D4: "B"
        .byte   $01,$00,$01,$00,$00,$00,$00,$00  ; $D5: "手に入れ"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D6: "Y"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D7: "L"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D8: "R"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $D9: "ない"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $DA: "H"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $DB: "M"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $DC: "P"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $DD: "……"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $DE: "だいじょうぶ"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $DF: "は、"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E0: "んだ"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E1: "てい"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E2: "した"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E3: "、"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E4: "して"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E5: "『"
        .byte   $00,$01,$01,$00,$00,$00,$00,$00  ; $E6: "お父様" (father)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E7: "!?"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E8: "です"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $E9: "（"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $EA: "）"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $EB: "この"
        .byte   $00,$01,$00,$00,$00,$00,$00,$00  ; $EC: "の力"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $ED: "ケルガー" (kelgar)
        .byte   $01,$01,$01,$01,$01,$00,$00,$00  ; $EE: "古代図書館" (ancient library)
        .byte   $01,$00,$00,$00,$00,$00,$00,$00  ; $EF: "言って"
        .byte   $01,$01,$00,$00,$00,$00,$00,$00  ; $F0: "兵士「" (soldier)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F1: "から"
        .byte   $01,$01,$01,$00,$00,$00,$00,$00  ; $F2: "かりょくせん" (fire ship)
        .byte   $01,$00,$00,$00,$00,$00,$00,$00  ; $F3: "海ぞく" (pirates)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F4: "いじょう"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F5: "が、"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F6: "たち"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F7: "たの"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F8: "ます"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $F9: "ください" (please)
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $FA: "かった"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $FB: "た!"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $FC: "に、"
        .byte   $00,$00,$00,$00,$00,$00,$00,$00  ; $FD: "いないからなんだ"
        .byte   $02,$02,$00,$00,$00,$00,$00,$00  ; $FE: "次元の狭間" (dimensional rift)

; $1e: kanji (1st bank)
_c08a5f:
        ldx     $b1
        ldy     $ab
        lda     #$01
        sta     $1a13,y
        bra     _8a73

; $1f: kanji (2nd bank)
_c08a6a:
        ldx     $b1
        ldy     $ab
        lda     #$02
        sta     $1a13,y
_8a73:  lda     f:Dlg+1,x
        sta     $19d3,y
        iny
        sty     $ab
        ldy     $b1
        iny
        sty     $b1
        jmp     _c08459

; $12: spell name
_c08a85:
        lda     $16a3
        longa
        asl
        sta     $0f
        asl
        clc
        adc     $0f
        tax
        lda     $06
        shorta
        stz     $09
@8a98:  lda     f:MagicName+1,x
        cmp     #$ff
        beq     @8ab2
        sta     $19d3,y
        lda     #$00
        sta     $1a13,y
        iny
        inx
        inc     $09
        lda     $09
        cmp     #MagicName::ITEM_SIZE
        bne     @8a98
@8ab2:  sty     $ab
        jmp     _c08459

; $11: item name
_c08ab7:
        lda     $16a2
        longa
        sta     $0f                     ; multiply by 9
        asl3
        clc
        adc     $0f
        tax
        lda     $06
        shorta
        stz     $09
@8acb:  lda     f:ItemName+1,x
        cmp     #$ff
        beq     @8ae5
        sta     $19d3,y
        lda     #$00
        sta     $1a13,y
        iny
        inx
        inc     $09
        lda     $09
        cmp     #ItemName::ITEM_SIZE - 1
        bne     @8acb
@8ae5:  sty     $ab
        jmp     _c08459

; $10: gil amount
_c08aea:
        stz     $0c
        ldx     $06
@8aee:  lda     $0c
        bne     @8af9
        lda     $10ad,x
        beq     @8b08
        inc     $0c
@8af9:  lda     $10ad,x
        clc
        adc     #$53
        sta     $19d3,y
        lda     #$00
        sta     $1a13,y
        iny
@8b08:  inx
        cpx     #7
        bne     @8aee
        sty     $ab
        jmp     _c08459

; escape code jump table ($00-$1F), also includes MTE codes
DlgCmdTbl:
        .addr   _c08493,_c084af,_c084e9,_c0850d,_c0850d,_c0850d,_c0850d,_c0850d
        .addr   _c0850d,_c0850d,_c0850d,_c0850d,_c08466,_c0850d,_c0850d,_c08466
        .addr   _c08aea,_c08ab7,_c08a85,_c0850d,_c0850d,_c0850d,_c0850d,_c08477
        .addr   _c0850d,_c0850d,_c0850d,_c0850d,_c0850d,_c0850d,_c08a5f,_c08a6a

; ---------------------------------------------------------------------------

_c08b53:
        lda     #$80
        sta     hVMAINC
        ldx     #$3000
        stx     hVMADDL
        longa
        ldx     #$0000
@8b63:  lda     #$00ff
.repeat 8
        sta     hVMDATAL
.endrep
        lda     #$0000
.repeat 8
        sta     hVMDATAL
.endrep
        inx
        cpx     #$00d0
        bne     @8b63
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08ba4:
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$08
        sta     hDMA0::CTRL
        lda     #<hVMDATAH
        sta     hDMA0::HREG
        ldx     #$3000
        stx     hVMADDL
        stz     $6d
        ldx     #$0b6d
        stx     hDMA0::ADDR
        stz     hDMA0::ADDR_B
        ldx     #$0d00
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08bd3:
        ldx     $06
@8bd5:  lda     $7f6a8a,x
        sta     $7f6a59,x
        inx
        cpx     #$0031
        bne     @8bd5
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08be4:
        lda     $b4
        asl
        clc
        adc     $b4
        tax
        lda     $b5
        beq     @8c0a
        tay
        longa
@8bf2:  lda     #$6aec
        sta     $7f6a8b,x
        lda     #$6d6c
        sta     $7f6abc,x
        inx3
        dey
        bne     @8bf2
        lda     $06
        shorta
@8c0a:  lda     #$05
        sec
        sbc     $b5
        beq     @8c2d
        tay
        longa
@8c14:  lda     $7f6a5a,x
        sta     $7f6a8b,x
        lda     #$6d7c
        sta     $7f6abc,x
        inx3
        dey
        bne     @8c14
        lda     $06
        shorta
@8c2d:  rts

; ---------------------------------------------------------------------------

; [ init hdma #1 (window 2 position) ]

_c08c2e:
        lda     #$41
        sta     hDMA1::CTRL
        lda     #<hWH2
        sta     hDMA1::HREG
        ldx     #$6a8a
        stx     hDMA1::ADDR
        lda     #$7f
        sta     hDMA1::ADDR_B
        sta     hDMA1::HDMA_B
        ldx     #$6b8c
        stx     $23
        ldx     $06
@8c4d:  lda     #$90
        sta     $7f6a8a,x
        longa
        lda     $23
        sta     $7f6a8b,x
        clc
        adc     #$0020
        sta     $23
        lda     $06
        shorta
        inx3
        cpx     #$002a
        bne     @8c4d
        lda     #$8f
        sta     $7f6a8a
        lda     $55
        bne     @8c7a
        jsr     _c08c7b
@8c7a:  rts

; ---------------------------------------------------------------------------

; [  ]

_c08c7b:
@8c7b:  longa
        ldx     $06
        lda     #$00ff
@8c82:  sta     $7f6b6c,x
        inx2
        cpx     #$0200
        bne     @8c82
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08c92:
        ldx     $06
@8c94:  lda     #$90
        sta     $7f6a8a,x
        lda     #$0c
        sta     $7f6a8b,x
        lda     #$6b
        sta     $7f6a8c,x
        lda     #$90
        sta     $7f6abb,x
        lda     #$7c
        sta     $7f6abc,x
        lda     #$6d
        sta     $7f6abd,x
        inx3
        cpx     #$002d
        bne     @8c94
        lda     #$80
        sta     $7f6a8a,x
        sta     $7f6abb,x
        lda     #$8f
        sta     $7f6a8a
        sta     $7f6abb
        ldx     $06
@8cd6:  lda     #$ff
        sta     $7f6b0c,x
        lda     #$00
        sta     $7f6b0d,x
        lda     #$10
        sta     $7f6aec,x
        lda     #$ef
        sta     $7f6aed,x
        lda     #$90
        sta     $7f6b2c,x
        lda     #$f0
        sta     $7f6b2d,x
        lda     #$10
        sta     $7f6b4c,x
        lda     #$60
        sta     $7f6b4d,x
        inx2
        cpx     #$0020
        bne     @8cd6
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08d0e:
        ldx     $06
@8d10:  lda     #$bb
        sta     $7f6d6c,x
        lda     #$33
        sta     $7f6d7c,x
        inx
        cpx     #$0010
        bne     @8d10
        lda     #$40
        sta     hDMA2::CTRL
        lda     #<hWOBJSEL
        sta     hDMA2::HREG
        ldx     #$6abb
        stx     hDMA2::ADDR
        lda     #$7f
        sta     hDMA2::ADDR_B
        sta     hDMA2::HDMA_B
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08d3b:
        jsr     _c08e08
@8d3e:  ldy     $ab
        lda     $1a13,y
        jne     @8de1
        lda     $19d3,y
        cmp     #$0c
        bne     @8d54
        ldx     #$003c
        bra     @8d69
@8d54:  cmp     #$17
        bne     @8d83
        iny
        lda     $19d3,y
        longa
        asl6
        tax
        lda     $06
        shorta
@8d69:  stx     $ad
        iny
        sty     $ab
@8d6e:  jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        ldx     $ad
        dex
        stx     $ad
        bne     @8d6e
        bra     @8d3e
@8d83:  cmp     #$0f
        bne     @8d8e
        inc     $b8
        iny
        sty     $ab
        bra     @8d3e
@8d8e:  cmp     #$00
        bne     @8d99
        lda     #$40
        sta     $ab
        jmp     @8e07
@8d99:  cmp     #$01
        bne     @8de1
@8d9d:  lda     $a8
        beq     @8dba
        inc     $a7
        jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        lda     $a8
        clc
        adc     #$0d
        sta     $a8
        cmp     #$10
        bcc     @8d9d
@8dba:  stz     $a8
        ldy     $ab
        tya
        and     #$f0
        clc
        adc     #$10
        sta     $ab
        cmp     #$40
        beq     @8e07
        lsr3
        tax
        lda     f:_c08e1b,x
        sta     $0ba9
        lda     f:_c08e1b+1,x
        sta     $0baa
        bra     @8e07
        jmp     @8d3e

; kanji
@8de1:  lda     $1a13,y
        xba
        lda     $19d3,y
        iny
        sty     $ab
        jsr     _c08f01
        lda     $a8
        jsr     _c08ed3
        lda     $a8
        clc
        adc     #$0d
        sta     $a8
        cmp     #$10
        jcc     @8d3e
        and     #$0f
        sta     $a8
        inc     $a7
@8e07:  rts

; ---------------------------------------------------------------------------

; [  ]

_c08e08:
@8e08:  ldx     #$0000
@8e0b:  lda     $19bb,x
        sta     $19a3,x
        stz     $19bb,x
        inx
        cpx     #$0018
        bne     @8e0b
        rts

; ---------------------------------------------------------------------------

_c08e1b:
        .word   $3000,$3340,$3680,$39c0

; ---------------------------------------------------------------------------

; [  ]

_c08e23:
        stz     $a7
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$00
        sta     hDMA0::CTRL
        lda     #<hVMDATAH
        sta     hDMA0::HREG
        lda     #$00
        sta     hDMA0::ADDR_B
        longa
        lda     $a9
        clc
        adc     #$0002
        sta     $a9
        sta     hVMADDL
        lda     $06
        shorta
        ldx     #$19a3
        stx     hDMA0::ADDR
        ldx     #$0006
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        longa
        lda     $a9
        clc
        adc     #$000e
        sta     $a9
        sta     hVMADDL
        lda     $06
        shorta
        ldx     #$19a9
        stx     hDMA0::ADDR
        ldx     #$0006
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        longa
        lda     $a9
        clc
        adc     #$0012
        sta     $a9
        sta     hVMADDL
        lda     $06
        shorta
        ldx     #$19af
        stx     hDMA0::ADDR
        ldx     #$0006
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        longa
        lda     $a9
        clc
        adc     #$000e
        sta     $a9
        sta     hVMADDL
        lda     $06
        shorta
        ldx     #$19b5
        stx     hDMA0::ADDR
        ldx     #$0006
        stx     hDMA0::SIZE
        lda     #$01
        sta     hMDMAEN
        longa
        lda     $a9
        clc
        adc     #$0010
        sta     $a9
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08ed3:
@8ed3:  cmp     #$00
        beq     @8ef1
@8ed7:  pha
        ldx     #$0000
@8edb:  lsr     $1973,x
        ror     $197f,x
        ror     $198b,x
        ror     $1997,x
        inx
        cpx     #$000c
        bne     @8edb
        pla
        dec
        bne     @8ed7
@8ef1:  ldx     #$0030
@8ef4:  lda     $19a2,x
        ora     $1972,x
        sta     $19a2,x
        dex
        bne     @8ef4
        rts

; ---------------------------------------------------------------------------

; [ load font character graphics ]

_c08f01:
@8f01:  longa
        cmp     #$0100
        bcs     @8f2c

; katakana or hiragana
        asl3
        sta     $0d
        asl
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        ldy     $06
@8f18:  lda     f:BigFontGfx - (32 * 24),x
        sta     $1973,y
        lda     #0
        sta     $198b,y
        inx
        iny
        cpy     #24
        bne     @8f18
        rts
@8f2c:  .a16
        sec
        sbc     #$0100
        asl3
        sta     $0d
        asl
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        ldy     $06
@8f40:  lda     f:KanjiGfx,x
        sta     $1973,y
        lda     #0
        sta     $198b,y
        inx
        iny
        cpy     #24
        bne     @8f40
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08f54:
        stz     $a8
        ldx     #$3000
        stx     $a9
        ldx     #$0000
@8f5e:  stz     $19a3,x
        inx
        cpx     #$0030
        bne     @8f5e
        ldx     $06
@8f69:  lda     #$ff
        sta     $19d3,x
        stz     $1a13,x
        inx
        cpx     #$0040
        bne     @8f69
        rts

; ---------------------------------------------------------------------------

; [ draw yes/no indicator ]

_c08f78:
        lda     $1697
        bpl     @8fdc
        and     #$01
        asl3
        tax
        lda     #$00
        sta     hVMAINC
        lda     #$04
        sta     $19
        ldy     $1698
        sty     $1b
@8f91:  ldy     $1b
        sty     hVMADDL
        lda     f:_c08fdd,x
        sta     hVMDATAL
        inx
        iny
        tya
        longa
        and     #$001f
        bne     @8fb2
        tya
        sec
        sbc     #$0020
        eor     #$0400
        sta     hVMADDL
@8fb2:  lda     $06
        shorta
        lda     f:_c08fdd,x
        sta     hVMDATAL
        inx
        longa
        lda     $1b
        clc
        adc     #$0020
        and     #$03ff
        sta     $17
        lda     $1b
        and     #$fc00
        ora     $17
        sta     $1b
        lda     $06
        shorta
        dec     $19
        bne     @8f91
@8fdc:  rts

_c08fdd:
        .byte   $d9,$da,$db,$dc,$d0,$d0,$d0,$d0
        .byte   $d0,$d0,$d0,$d0,$d9,$da,$db,$dc

; ---------------------------------------------------------------------------

; [ show yes/no window ]

_c08fed:
        lda     $0ad9
        inc
        sta     $76
        lda     $0ad8
        sec
        sbc     #$06
        sta     $75
        longa
        lda     $75
        and     #$0010
        beq     @9007
        lda     #$0400
@9007:  sta     $23
        lda     $76
        and     #$000f
        xba
        lsr2
        clc
        adc     #$0020
        clc
        adc     $23
        sta     $23
        lda     $75
        and     #$000f
        asl
        sec
        adc     $23
        clc
        adc     #$4800
        sta     $1698
        lda     $06
        shorta
        lda     #$08
        sec
        adc     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        stz     $b7
@903d:  jsr     WaitVBlank
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        lda     $b7
        longa
        asl2
        sta     $0d
        asl2
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        ldy     #$0004
@905e:  lda     f:_c090f7,x
        sta     $16f3,y
        lda     #$03
        sta     $16f4,y
        lda     f:_c090f7+10,x
        sta     $1733,y
        lda     #$03
        sta     $1734,y
        inx
        iny2
        cpy     #$0018
        bne     @905e
        lda     $b7
        asl
        clc
        adc     $b7
        clc
        adc     #$18
        tax
        lda     #$4c
        sta     $7f6a8b,x
        lda     #$6b
        sta     $7f6a8c,x
        lda     #$6c
        sta     $7f6abc,x
        lda     #$6d
        sta     $7f6abd,x
        inc     $a6
        inc     $76
        inc     $b7
        lda     $b7
        cmp     #$03
        bne     @903d
        rts

; ---------------------------------------------------------------------------

_c090ad:
        lda     #$0a
        sec
        adc     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
@90ba:  jsr     WaitVBlank
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        inc     $a6
        lda     $b7
        asl
        clc
        adc     $b7
        clc
        adc     #$15
        tax
        lda     $7f6a5a,x
        sta     $7f6a8b,x
        lda     $7f6a5b,x
        sta     $7f6a8c,x
        lda     #$7c
        sta     $7f6abc,x
        lda     #$6d
        sta     $7f6abd,x
        dec     $76
        dec     $b7
        lda     $b7
        bne     @90ba
        rts

; tile data for yes/no window
_c090f7:
        .byte   $d1,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d2
        .byte   $d5,$d9,$da,$d0,$ea,$eb,$ec,$ed,$d0,$d6
        .byte   $d5,$db,$dc,$d0,$fa,$fb,$fc,$fd,$d0,$d6
        .byte   $d5,$d0,$d0,$ec,$ed,$ec,$ed,$ee,$ef,$d6
        .byte   $d5,$d0,$d0,$fc,$fd,$fc,$fd,$fe,$ff,$d6
        .byte   $d3,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d4

; ---------------------------------------------------------------------------

; [ show gp window ]

_c09133:
        lda     #$06
        sec
        adc     $0ad9       ; y position
        sta     $76
        lda     $0ad8       ; x position
        sta     $75
        stz     $b6
@9142:  jsr     WaitVBlank
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        lda     $b6
        longa
        asl3
        sta     $0d
        asl
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        ldy     #$0024
@9163:  lda     f:_c09237,x
        sta     $16f3,y
        lda     #$03
        sta     $16f4,y
        lda     f:_c09237+12,x
        sta     $1733,y
        lda     #$03
        sta     $1734,y
        inx
        iny2
        cpy     #$003c
        bne     @9163
        ldx     $06
        ldy     #$0028
        lda     $b6
        bne     @91a5
@918c:  lda     $10ad,x
        clc
        adc     #$e0
        sta     $1733,y
        lda     #$03
        sta     $1734,y
        inx
        iny2
        cpy     #$0036
        bne     @918c
        jmp     @91bb
@91a5:  lda     $10ad,x
        clc
        adc     #$f0
        sta     $16f3,y
        lda     #$03
        sta     $16f4,y
        inx
        iny2
        cpy     #$0036
        bne     @91a5
@91bb:  lda     $b6
        asl
        clc
        adc     $b6
        clc
        adc     #$12
        tax
        lda     #$2c
        sta     $7f6a8b,x
        lda     #$6b
        sta     $7f6a8c,x
        lda     #$6c
        sta     $7f6abc,x
        lda     #$6d
        sta     $7f6abd,x
        inc     $a6
        inc     $76
        inc     $b6
        lda     $b6
        cmp     #$02
        jne     @9142
        rts

; ---------------------------------------------------------------------------

; [  ]

_c091ed:
        lda     #$07
        sec
        adc     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
@91fa:  jsr     WaitVBlank
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        inc     $a6
        lda     $b6
        asl
        clc
        adc     $b6
        clc
        adc     #$0f
        tax
        lda     $7f6a5a,x
        sta     $7f6a8b,x
        lda     $7f6a5b,x
        sta     $7f6a8c,x
        lda     #$7c
        sta     $7f6abc,x
        lda     #$6d
        sta     $7f6abd,x
        dec     $76
        dec     $b6
        lda     $b6
        bne     @91fa
        rts

; ---------------------------------------------------------------------------

; tile data for gp window
_c09237:
        .byte   $d1,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d2
        .byte   $d5,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$dd,$d6
        .byte   $d5,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$de,$d6
        .byte   $d3,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d4

; ---------------------------------------------------------------------------

; [ show map title ]

_c09267:
        lda     $0b53       ;
        beq     @928b
        lda     $16a0
        beq     @927e       ; branch if map title is disabled
        lda     #$01        ; dialog window at top
        sta     $b4
        jsr     _c0933d       ; show dialog window (map title)
        jsr     _c04ac1       ; wait for keypress
        jsr     _c09419       ; hide dialog window (map title)
@927e:  stz     $16a0
        inc     $a5
        jsr     WaitVBlank
        lda     #$01
        sta     $10b8
@928b:  rts

; ---------------------------------------------------------------------------

; [ init map title ]

_c0928c:
        jsr     _c08f54
        lda     $57
        beq     @929d
        lda     $16a0
        beq     @92a2
        lda     $110e
        bra     @92a3
@929d:  lda     $110e
        bne     @92a3
@92a2:  rts
@92a3:  longa
        asl
        tax
        lda     f:MapTitlePtrs+2,x
        sec
        sbc     f:MapTitlePtrs,x
        sta     $2c
        lda     f:MapTitlePtrs,x
        tax
        lda     $06
        shorta
        ldy     $06
@92bd:  lda     f:MapTitle,x
        cmp     #$1e
        bne     @92cc
        lda     #$01
        sta     $1a13,y
        bra     @92d5
@92cc:  cmp     #$1f
        bne     @92e2
        lda     #$02
        sta     $1a13,y
@92d5:  inx
        dec     $2c
        lda     f:MapTitle,x
        sta     $19d3,y
        iny
        bra     @92e6
@92e2:  sta     $19d3,y
        iny
@92e6:  inx
        dec     $2c
        bne     @92bd
        tya
        sta     hWRMPYA
        lda     #$06
        sta     hWRMPYB
        nop4
        lda     #$60
        sec
        sbc     hRDMPYL
        sta     hWRDIVL
        stz     hWRDIVH
        lda     #$10
        sta     hWRDIVB
        nop8
        lda     hRDMPYL
        sta     $a8
        longa
        lda     hRDDIVL
        asl6
        clc
        adc     #$3000
        sta     $a9
        lda     $06
        shorta
        lda     #$10
        ldy     $06
        sty     $ab
@9331:  pha
        jsr     _c08d3b
        jsr     _c08e23
        pla
        dec
        bne     @9331
        rts

; ---------------------------------------------------------------------------

; [ show dialog window (map title) ]

_c0933d:
@933d:  jsr     _c08bd3
        lda     $b4
        sec
        adc     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        lda     #$00
        sta     $b5
@9351:  jsr     WaitVBlank
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        lda     $b5
        longa
        xba
        lsr2
        tax
        lda     $06
        shorta
        ldy     #$0004
@936c:  lda     f:_c09399,x
        sta     $16f3,y
        lda     #$03
        sta     $16f4,y
        lda     f:_c093b9,x
        sta     $1733,y
        lda     #$03
        sta     $1734,y
        inx
        iny2
        cpy     #$003c
        bne     @936c
        inc     $a6
        inc     $76
        inc     $b5
        lda     $b5
        cmp     #$02
        bne     @9351
        rts

; ---------------------------------------------------------------------------

; tile data for map window
_c09399:
        .byte   $d1,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7
        .byte   $d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d2,$00,$00,$00,$00

_c093b9:
        .byte   $d5,$00,$02,$04,$06,$08,$0a,$0c,$0e,$10,$12,$14,$16,$18,$1a,$1c
        .byte   $1e,$20,$22,$24,$26,$28,$2a,$2c,$2e,$30,$32,$d6,$00,$00,$00,$00

_c093d9:
        .byte   $d5,$01,$03,$05,$07,$09,$0b,$0d,$0f,$11,$13,$15,$17,$19,$1b,$1d
        .byte   $1f,$21,$23,$25,$27,$29,$2b,$2d,$2f,$31,$33,$d6,$00,$00,$00,$00

_c093f9:
        .byte   $d3,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8
        .byte   $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d4,$00,$00,$00,$00

; ---------------------------------------------------------------------------

; [ hide dialog window (map title) ]

_c09419:
@9419:  lda     $b4
        sec
        adc     $0ad9
        clc
        adc     #$01
        sta     $76
        lda     $0ad8
        sta     $75
@9429:  jsr     WaitVBlank
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        inc     $a6
        dec     $76
        dec     $b5
        lda     $b5
        bne     @9429
        rts

; ---------------------------------------------------------------------------

; [  ]

_c09440:
        jsr     _c08bd3
        lda     $b4
        sec
        adc     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        lda     #$00
        sta     $b5
@9454:  jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        lda     $b5
        longa
        xba
        lsr2
        tax
        lda     $06
        shorta
        ldy     #$0004
@9478:  lda     f:_c094d8,x
        sta     $16f3,y
        lda     #$03
        sta     $16f4,y
        lda     f:_c094f8,x
        sta     $1733,y
        lda     #$03
        sta     $1734,y
        inx
        iny2
        cpy     #$003c
        bne     @9478
        inc     $a6
        inc     $76
        inc     $b5
        lda     $b5
        cmp     #$05
        bne     @9454
        jsr     WaitVBlank
        rts

; ---------------------------------------------------------------------------

; [  ]

_c094a8:
        lda     $b4
        sec
        adc     $0ad9
        clc
        adc     #$04
        sta     $76
        lda     $0ad8
        sta     $75
@94b8:  jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        lda     #$01
        ldx     $06
        stx     $71
        jsr     _c0707d
        inc     $a6
        dec     $76
        dec     $b5
        lda     $b5
        bne     @94b8
        rts

; ---------------------------------------------------------------------------

; tile data for dialogue window
_c094d8:
        .byte   $d1,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7
        .byte   $d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d7,$d2,$00,$00,$00,$00

_c094f8:
        .byte   $d5,$00,$02,$04,$06,$08,$0a,$0c,$0e,$10,$12,$14,$16,$18,$1a,$1c
        .byte   $1e,$20,$22,$24,$26,$28,$2a,$2c,$2e,$30,$32,$d6,$00,$00,$00,$00

_c09518:
        .byte   $d5,$01,$03,$05,$07,$09,$0b,$0d,$0f,$11,$13,$15,$17,$19,$1b,$1d
        .byte   $1f,$21,$23,$25,$27,$29,$2b,$2d,$2f,$31,$33,$d6,$00,$00,$00,$00

_c09538:
        .byte   $d5,$34,$36,$38,$3a,$3c,$3e,$40,$42,$44,$46,$48,$4a,$4c,$4e,$50
        .byte   $52,$54,$56,$58,$5a,$5c,$5e,$60,$62,$64,$66,$d6,$00,$00,$00,$00

_c09558:
        .byte   $d5,$35,$37,$39,$3b,$3d,$3f,$41,$43,$45,$47,$49,$4b,$4d,$4f,$51
        .byte   $53,$55,$57,$59,$5b,$5d,$5f,$61,$63,$65,$67,$d6,$00,$00,$00,$00

_c09578:
        .byte   $d5,$68,$6a,$6c,$6e,$70,$72,$74,$76,$78,$7a,$7c,$7e,$80,$82,$84
        .byte   $86,$88,$8a,$8c,$8e,$90,$92,$94,$96,$98,$9a,$d6,$00,$00,$00,$00

_c09598:
        .byte   $d5,$69,$6b,$6d,$6f,$71,$73,$75,$77,$79,$7b,$7d,$7f,$81,$83,$85
        .byte   $87,$89,$8b,$8d,$8f,$91,$93,$95,$97,$99,$9b,$d6,$00,$00,$00,$00

_c095b8:
        .byte   $d5,$9c,$9e,$a0,$a2,$a4,$a6,$a8,$aa,$ac,$ae,$b0,$b2,$b4,$b6,$b8
        .byte   $ba,$bc,$be,$c0,$c2,$c4,$c6,$c8,$ca,$cc,$ce,$d6,$00,$00,$00,$00

_c095d8:
        .byte   $d5,$9d,$9f,$a1,$a3,$a5,$a7,$a9,$ab,$ad,$af,$b1,$b3,$b5,$b7,$b9
        .byte   $bb,$bd,$bf,$c1,$c3,$c5,$c7,$c9,$cb,$cd,$cf,$d6,$00,$00,$00,$00

_c095f8:
        .byte   $d3,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8
        .byte   $d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d8,$d4,$00,$00,$00,$00

; ---------------------------------------------------------------------------

; [  ]

_c09618:
        lda     #$80
        sta     hVMAINC
        ldy     #$1880
        sty     hVMADDL
        lda     hRVMDATAH               ; read from VRAM
        ldx     $06
@9628:  lda     hRVMDATAH
        sta     $7f8622,x
        inx
        cpx     #$0080
        bne     @9628
        ldy     #$1c80
        sty     hVMADDL
        lda     hRVMDATAH
@963e:  lda     hRVMDATAH
        sta     $7f8622,x
        inx
        cpx     #$0100
        bne     @963e
        rts

; ---------------------------------------------------------------------------

_c0964c:
        lda     $3e
        and     #$0f
        tax
        lda     $96f4,x
        ora     #$07
        tax
        lda     $7f8622,x
        sta     $17
        ldy     #$0007
@9660:  lda     $7f8621,x
        sta     $7f8622,x
        dex
        dey
        bne     @9660
        txa
        and     #$f8
        clc
        adc     #$47
        tax
        lda     $7f8622,x
        sta     $7f85db,x
        ldy     #$0007
@967e:  lda     $7f8621,x
        sta     $7f8622,x
        dex
        dey
        bne     @967e
        txa
        and     #$f8
        tax
        lda     $17
        sta     $7f8622,x
        rts

; ---------------------------------------------------------------------------

; [  ]

_c09695:
        lda     $0ad6       ; map index
        cmp     #$03
        bcs     @96f3
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$00
        sta     hDMA0::CTRL
        lda     #<hVMDATAH
        sta     hDMA0::HREG
        lda     #$7f
        sta     hDMA0::ADDR_B
        ldx     #$0080
        stx     hDMA0::SIZE
        ldx     #$8622
        stx     hDMA0::ADDR
        ldx     #$1880
        stx     hVMADDL
        lda     #$01
        sta     hMDMAEN
        stz     hMDMAEN
        lda     #$00
        sta     hDMA0::CTRL
        lda     #<hVMDATAH
        sta     hDMA0::HREG
        lda     #$7f
        sta     hDMA0::ADDR_B
        ldx     #$0080
        stx     hDMA0::SIZE
        ldx     #$86a2
        stx     hDMA0::ADDR
        ldx     #$1c80
        stx     hVMADDL
        lda     #$01
        sta     hMDMAEN
@96f3:  rts

; ---------------------------------------------------------------------------

_c096f4:
        .byte   $00,$88,$10,$98,$20,$a8,$30,$b8,$80,$08,$90,$18,$a0,$28,$b0,$38

; ---------------------------------------------------------------------------

; [  ]

_c09704:
        lda     #$80
        sta     hVMAINC
        ldy     #$21c0
        sty     hVMADDL
        lda     hRVMDATAH
        ldx     $06
@9714:  lda     hRVMDATAH
        sta     $7f8722,x
        inx
        cpx     #$0080
        bne     @9714
        rts

; ---------------------------------------------------------------------------

; [  ]

_c09722:
        lda     $3e
        and     #$0e
        lsr
        tax
        lda     f:_c09757,x
        and     #$07
        ora     #$70
        tax
        lda     $7f872a,x
        sta     $17
@9737:  lda     $7f8722,x
        sta     $7f872a,x
        txa
        sec
        sbc     #$08
        tax
        and     #$f8
        bne     @9737
        lda     $7f8722,x
        sta     $7f872a,x
        lda     $17
        sta     $7f8722,x
        rts

_c09757:
        .byte   0,4,2,6,1,5,3,7

; ---------------------------------------------------------------------------

; [  ]

_c0975f:
        lda     $0ad6       ; map index
        cmp     #$03
        bcs     @9798
        and     #$01
        bne     @9798
        lda     #$80
        sta     hVMAINC
        stz     hMDMAEN
        lda     #$00
        sta     hDMA0::CTRL
        lda     #<hVMDATAH
        sta     hDMA0::HREG
        lda     #$7f
        sta     hDMA0::ADDR_B
        ldx     #$0080
        stx     hDMA0::SIZE
        ldx     #$8722
        stx     hDMA0::ADDR
        ldx     #$21c0
        stx     hVMADDL
        lda     #$01
        sta     hMDMAEN
@9798:  rts

; ---------------------------------------------------------------------------

; [ update palette animation ]

_c09799:
        lda     $1123
        beq     @97e1
        ldy     $06
@97a0:  lda     $165a,y
        bmi     @97d5
        and     #$f0
        lsr4
        bne     @97ba
        jsr     _c098e2
        cmp     #$00
        bne     @97d5
        jsr     _c097e2
        jmp     @97d5
@97ba:  dec
        bne     @97ca
        jsr     _c098e2
        cmp     #$00
        bne     @97d5
        jsr     _c0980d
        jmp     @97d5
@97ca:  dec
        bne     @97d5
        jsr     _c098e2
        phy
        jsr     _c09838
        ply
@97d5:  tya
        clc
        adc     #$08
        tay
        cmp     #$20
        jne     @97a0
@97e1:  rts

; ---------------------------------------------------------------------------

; [  ]

_c097e2:
@97e2:  lda     $165c,y
        longa
        asl
        tax
        lda     $0c00,x
        sta     $17
        lda     $165b,y
        and     #$00ff
        asl
        sta     $19
@97f7:  lda     $0bfe,x
        sta     $0c00,x
        dex2
        cpx     $19
        bne     @97f7
        lda     $17
        sta     $0c00,x
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0980d:
@980d:  lda     $165b,y
        longa
        asl
        tax
        lda     $0c00,x
        sta     $17
        lda     $165c,y
        and     #$00ff
        asl
        sta     $19
@9822:  lda     $0c02,x
        sta     $0c00,x
        inx2
        cpx     $19
        bne     @9822
        lda     $17
        sta     $0c00,x
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [  ]

_c09838:
@9838:  longa
        lda     $165d,y
        sta     $30
        lda     $06
        shorta
        lda     #^MapPalAnim
        sta     $32
        lda     $165c,y
        sec
        sbc     $165b,y
        asl
        tax
        stx     $35
        lda     $1659,y
        tax
        lda     f:_c098d2,x
        pha
        lda     $165b,y
        asl
        tax
        pla
        jsr     _c09865
        rts

; ---------------------------------------------------------------------------

; [  ]

_c09865:
@9865:  sta     hWRMPYA
        ldy     $06
@986a:  longa
        lda     [$30],y
        sta     $17
        lsr4
        and     #$003e
        sta     $19
        lda     $17
        xba
        lsr
        and     #$003e
        sta     $1b
        lda     $17
        and     #$001f
        asl
        sta     $17
        lda     $06
        shorta
        lda     $17
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        sta     $1d
        lda     $1b
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        asl2
        and     #$7c
        sta     $1e
        lda     $19
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        longa
        xba
        lsr3
        ora     $1d
        sta     $0c00,x
        lda     $06
        shorta
        inx2
        iny2
        cpy     $35
        bne     @986a
        rts

; ---------------------------------------------------------------------------

_c098d2:
        .byte   $80,$78,$70,$68,$60,$58,$50,$48,$40,$48,$50,$58,$60,$68,$70,$78

; ---------------------------------------------------------------------------

; [  ]

_c098e2:
@98e2:  lda     $1657,y
        inc
        sta     $1657,y
        cmp     $1658,y
        bne     @990a
        lda     #$00
        sta     $1657,y
        lda     $1659,y
        inc
        sta     $1659,y
        lda     $165a,y
        and     #$0f
        cmp     $1659,y
        bne     @990a
        lda     #$00
        sta     $1659,y
        rts
@990a:  lda     #$01
        rts

; ---------------------------------------------------------------------------

; [ init palette animation ]

_c0990d:
        lda     $1123
        bne     @9913
        rts
@9913:  dec
        sta     hWRMPYA
        lda     #$18        ; 24 bytes each
        sta     hWRMPYB
        nop4
        ldx     hRDMPYL
        ldy     $06
@9925:  lda     f:MapPalAnim,x
        sta     $165a,y
        lda     f:MapPalAnim+1,x
        sta     $1658,y
        lda     f:MapPalAnim+2,x
        sta     $165b,y
        lda     f:MapPalAnim+3,x
        sta     $165c,y
        lda     f:MapPalAnim+4,x
        sta     $165d,y
        lda     f:MapPalAnim+5,x
        sta     $165e,y
        lda     #$00
        sta     $1657,y
        sta     $1659,y
        longa
        txa
        clc
        adc     #$0006
        tax
        lda     $06
        shorta
        tya
        clc
        adc     #$08
        tay
        cmp     #$20
        bne     @9925
        rts

; ---------------------------------------------------------------------------

; [ copy map animation graphics to vram ]

_c0996d:
        lda     $1110       ; return if map animation is disabled
        bpl     @9973
        rts
@9973:  lda     #$80
        sta     hVMAINC
        lda     #$01
        sta     hDMA0::CTRL
        lda     #<hVMDATAL
        sta     hDMA0::HREG
        ldy     $06
        ldx     #$2e00
@9987:  lda     $1417,y
        bmi     @9990
        lda     #^MapAnimGfx            ; load from rom (bank $df)
        bra     @9992
@9990:  lda     #$7f                    ; load from ram (bank $7f)
@9992:  sta     hDMA0::ADDR_B
        longa
        lda     $141b,y     ; source address
        sta     hDMA0::ADDR
        lda     #$0080
        sta     hDMA0::SIZE
        stx     hVMADDL
        txa
        clc
        adc     #$0040
        tax
        lda     $06
        shorta
        lda     #$01
        sta     hMDMAEN
        tya
        clc
        adc     #$08        ; 8 tiles
        tay
        cpy     #$0040
        bne     @9987
        lda     #^MapAnimGfx
        sta     hDMA0::ADDR_B
        ldx     #$4780      ; bg3 animation graphics
@99c7:  lda     $1417,y
        bmi     @99d0
        lda     #^MapAnimGfx
        bra     @99d2
@99d0:  lda     #$7f
@99d2:  sta     hDMA0::ADDR_B
        longa
        lda     $141b,y
        sta     hDMA0::ADDR
        lda     #$0040
        sta     hDMA0::SIZE
        stx     hVMADDL
        txa
        clc
        adc     #$0020
        tax
        lda     $06
        shorta
        lda     #$01
        sta     hMDMAEN
        tya
        clc
        adc     #$08        ; 4 tiles
        tay
        cpy     #$0060
        bne     @99c7
        rts

; ---------------------------------------------------------------------------

; [ update map animation ]

_c09a00:
        ldy     $06
@9a02:  lda     $1418,y
        lsr4
        xba
        longa
        lsr
        clc
        adc     $1419,y
        sta     $141b,y
        lda     $06
        shorta
        lda     $1417,y
        and     #$1c
        lsr2
        tax
        lda     $1418,y
        clc
        adc     f:_c09a8e,x
        sta     $1418,y
        lda     $1417,y
        and     #$03
        tax
        lda     $1418,y
        and     f:_c09a8a,x
        sta     $1418,y
        tya
        clc
        adc     #$08
        tay
        cpy     #$0040
        bne     @9a02
@9a45:  lda     $1418,y
        lsr4
        xba
        longa
        lsr2
        clc
        adc     $1419,y
        sta     $141b,y
        lda     $06
        shorta
        lda     $1417,y
        and     #$1c
        lsr2
        tax
        lda     $1418,y
        clc
        adc     f:_c09a8e,x
        sta     $1418,y
        lda     $1417,y
        and     #$03
        tax
        lda     $1418,y
        and     f:_c09a8a,x
        sta     $1418,y
        tya
        clc
        adc     #$08
        tay
        cpy     #$0060
        bne     @9a45
        rts

_c09a8a:
        .byte   $1f,$3f,$7f,$ff

_c09a8e:
        .byte   1,2,3,4,5,6,7,8

; ---------------------------------------------------------------------------

; [ init map animation ]

_c09a96:
        lda     $1114       ; tileset
        sta     hWRMPYA
        lda     #$24        ; animation properties are 36 bytes long
        sta     hWRMPYB
        nop4
        ldx     hRDMPYL
        stx     $13
        ldx     $06
        stx     $15
@9aae:  longa
        lda     $15         ; animated tile index
        asl
        clc
        adc     $13         ; pointer to animation properties
        tax
        lda     $15
        asl3
        tay
        lda     f:MapAnimProp+12,x
        sta     $1419,y     ; graphics offset
        sta     $2e
        lda     $15
        xba
        asl3
        sta     $23         ; $23 = tile index * #$0800
        lda     $15
        clc
        adc     $13
        tax
        lda     $06
        shorta
        lda     #$00
        sta     $1418,y     ; frame counter
        lda     f:MapAnimProp,x
        sta     $1417,y     ; animation flags
        bpl     @9af8
        longa
        lda     $23
        clc
        adc     #$8622      ; offset if loading from ram
        sta     $1419,y
        lda     $06
        shorta
        jsr     _c09b01       ; load animated tile graphics
@9af8:  inc     $15         ; next tile
        lda     $15
        cmp     #$0c
        bne     @9aae
        rts

; ---------------------------------------------------------------------------

; [ load animated tile graphics ]

_c09b01:
@9b01:  longa
        ldx     $2e
        ldy     $06
@9b07:  lda     f:MapAnimGfx,x   ; map animation graphics
        sta     $0f00,y     ; copy 4 tiles
        inx2
        iny2
        cpy     #$0080
        bne     @9b07
        lda     $06
        shorta
        lda     $15
        cmp     #$08
        bcs     @9b3a       ; branch if a bg3 tile
        ldy     $06
@9b23:  jsr     _c09c3d
        iny
        cpy     #$0020
        bne     @9b23
        ldy     #$0040
@9b2f:  jsr     _c09c3d
        iny
        cpy     #$0060
        bne     @9b2f
        bra     @9b51
@9b3a:  ldy     $06
@9b3c:  jsr     _c09cac
        iny
        cpy     #$0010
        bne     @9b3c
        ldy     #$0020
@9b48:  jsr     _c09cac
        iny
        cpy     #$0030
        bne     @9b48
@9b51:  longa
        lda     $23
        sta     $0f
        lda     $06
        sta     $0d
@9b5b:  lda     $06
        sta     $11
@9b5f:  lda     $15
        cmp     #$0008
        bcs     @9bb5
        ldx     $0d
        lda     f:_c09d1b+16,x
        clc
        adc     $11
        pha
        lda     f:_c09d1b,x
        clc
        adc     $11
        tay
        ldx     $0f
        lda     $0e00,y
        sta     $7f8622,x
        lda     $0e20,y
        sta     $7f8642,x
        lda     $0e10,y
        sta     $7f8632,x
        lda     $0e30,y
        sta     $7f8652,x
        ply
        lda     $0e40,y
        sta     $7f8662,x
        lda     $0e60,y
        sta     $7f8682,x
        lda     $0e50,y
        sta     $7f8672,x
        lda     $0e70,y
        sta     $7f8692,x
        bra     @9beb
@9bb5:  lda     $0d
        asl
        tax
        lda     f:_c09d1b+16,x
        clc
        adc     $11
        pha
        lda     f:_c09d1b,x
        clc
        adc     $11
        tay
        ldx     $0f
        lda     $0e00,y
        sta     $7f8622,x
        lda     $0e10,y
        sta     $7f8632,x
        ply
        lda     $0e20,y
        sta     $7f8642,x
        lda     $0e30,y
        sta     $7f8652,x
        bra     @9beb
        ply
@9beb:  lda     $0d
        inc2
        and     #$001f
        sta     $0d
        inc     $0f
        inc     $0f
        inc     $11
        inc     $11
        lda     $11
        cmp     #$0010
        jne     @9b5f
        lda     $0d
        clc
        adc     #$0012
        and     #$001f
        sta     $0d
        ldy     $15
        cpy     #$0008
        bcs     @9c28
        lda     $0f
        clc
        adc     #$0070
        sta     $0f
        and     #$07ff
        beq     @9c38
        jmp     @9b5b
@9c28:  lda     $0f
        clc
        adc     #$0030
        sta     $0f
        and     #$03ff
        jne     @9b5b
@9c38:  lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ ??? 4bpp ]

_c09c3d:
@9c3d:  lda     $1110
        and     #$40
        beq     @9c63
        lda     $0f00,y
        sta     $0f80,y
        sta     $1000,y
        sta     $0e80,y
        sta     $0e00,y
        lda     $0f20,y
        sta     $0fa0,y
        sta     $1020,y
        sta     $0ea0,y
        sta     $0e20,y
        rts
@9c63:  lda     $0f20,y
        lsr
        lda     $0f00,y
        ror
        sta     $0f80,y
        lda     $0f20,y
        ror
        sta     $0fa0,y
        lda     $0fa0,y
        lsr
        lda     $0f80,y
        ror
        sta     $1000,y
        lda     $0fa0,y
        ror
        sta     $1020,y
        lda     $0f00,y
        asl
        lda     $0f20,y
        rol
        sta     $0ea0,y
        lda     $0f00,y
        rol
        sta     $0e80,y
        lda     $0e80,y
        asl
        lda     $0ea0,y
        rol
        sta     $0e20,y
        lda     $0e80,y
        rol
        sta     $0e00,y
        rts

; ---------------------------------------------------------------------------

; [ ??? 2bpp ]

_c09cac:
@9cac:  lda     $1110
        and     #$40
        beq     @9cd2
        lda     $0f00,y
        sta     $0f80,y
        sta     $1000,y
        sta     $0e80,y
        sta     $0e00,y
        lda     $0f10,y
        sta     $0f90,y
        sta     $1010,y
        sta     $0e90,y
        sta     $0e10,y
        rts
@9cd2:  lda     $0f10,y
        lsr
        lda     $0f00,y
        ror
        sta     $0f80,y
        lda     $0f10,y
        ror
        sta     $0f90,y
        lda     $0f90,y
        lsr
        lda     $0f80,y
        ror
        sta     $1000,y
        lda     $0f90,y
        ror
        sta     $1010,y
        lda     $0f00,y
        asl
        lda     $0f10,y
        rol
        sta     $0e90,y
        lda     $0f00,y
        rol
        sta     $0e80,y
        lda     $0e80,y
        asl
        lda     $0e90,y
        rol
        sta     $0e10,y
        lda     $0e80,y
        rol
        sta     $0e00,y
        rts

; ---------------------------------------------------------------------------

_c09d1b:
        .word   $0100,$0180,$0180,$0200,$0200,$0200,$0180,$0180
        .word   $0100,$0080,$0080,$0000,$0000,$0000,$0080,$0080
        .word   $0100,$0180,$0180,$0200,$0200,$0200,$0180,$0180
        .word   $0100,$0080,$0080,$0000,$0000,$0000,$0080,$0080
        .word   $0100,$0100,$0180,$0180,$0180,$0180,$0100,$0100
        .word   $0100,$0100,$0080,$0080,$0080,$0080,$0100,$0100
        .word   $0100,$0100,$0180,$0180,$0180,$0180,$0100,$0100
        .word   $0100,$0100,$0080,$0080,$0080,$0080,$0100,$0100

; ---------------------------------------------------------------------------

; map animation properties (28 * 36 bytes)

MapAnimProp:
        .incbin "map_anim_prop.dat"

; ---------------------------------------------------------------------------

; [ update timer ]

_c0a18b:
        lda     $0afb       ; return if timer is disabled
        beq     @a1c0
        ldx     $0afc
        beq     @a1a3
        longa
        dec     $0afc       ; decrement timer counter
        lda     $06
        shorta
        jsr     _c0a1c1       ;
        bra     @a1c0
@a1a3:  lda     $61
        and     #$1f
        bne     @a1c0       ; return if scrolling
        lda     $63
        and     #$1f
        bne     @a1c0
        stz     $0afb       ; disable timer
        lda     #$01
        sta     $57
        ldx     $0afe       ; timer event
        stx     $ce
        jsr     ExecEvent
        stz     $57
@a1c0:  rts

; ---------------------------------------------------------------------------

; [ speed up music with timer ]

_c0a1c1:
@a1c1:  lda     $0afb
        cmp     #$02
        bne     @a216
        ldx     $0afc
        cpx     #$012c
        bne     @a1d4
        lda     #$07
        bra     @a208
@a1d4:  cpx     #$0258
        bne     @a1dd
        lda     #$06
        bra     @a208
@a1dd:  cpx     #$04b0
        bne     @a1e6
        lda     #$05
        bra     @a208
@a1e6:  cpx     #$0708
        bne     @a1ef
        lda     #$04
        bra     @a208
@a1ef:  cpx     #$0960
        bne     @a1f8
        lda     #$03
        bra     @a208
@a1f8:  cpx     #$0bb8
        bne     @a201
        lda     #$02
        bra     @a208
@a201:  cpx     #$0e10
        bne     @a216
        lda     #$01
@a208:  sta     $1d01       ; multiplier 1 to 7 depending on timer
        lda     #$86        ; set tempo multiplier (song & sfx)
        sta     $1d00
        jsl     ExecSound_ext
        bra     @a216
@a216:  rts

; ---------------------------------------------------------------------------

; [ execute event ]

.proc ExecEvent

        stz     $d2
        stz     $16aa
        longa
        ldy     $06
        sty     $d4
        lda     $ce         ; event index
        asl
        clc
        adc     $ce
        tax
        lda     f:EventScriptPtrs,x   ; pointer to event
        sta     $1126,y
        sta     $d6
        lda     f:EventScriptPtrs+2,x
        and     #$00ff
        sta     $1128,y
        sta     $d8
        lda     #$0001
        sta     $1166,y     ; event stack repeat count ???
        lda     $06
        shorta

ExecEventCmd:
        stz     $ba
        ldx     $d4
        ldy     #$0005
        lda     [$d6],y     ; event script
        sta     $e3
        dey
        lda     [$d6],y
        sta     $e2
        dey
        lda     [$d6],y
        sta     $e1
        dey
        lda     [$d6],y
        sta     $e0
        dey
        lda     [$d6],y
        sta     $df
        dey
        lda     [$d6],y
        sta     $de

; event command $c7
        cmp     #$c7
        bne     _a278
        inc     $d2
        jsr     _c0a40a
        jmp     NextEventCmd

; event command $cf
_a278:  cmp     #$cf
        bne     _a284
        inc     $d2
        jsr     _c0a3c9
        jmp     NextEventCmd

; event command $ce
_a284:  cmp     #$ce
        bne     _a28e
        jsr     _c0a3c9
        jmp     NextEventCmd

; event command $cd
_a28e:  cmp     #$cd
        bne     _a298
        jsr     _c0a380       ; jump to event
        jmp     NextEventCmd

; event command $ff: end of script
_a298:  cmp     #$ff
        bne     _a2a6
        ldy     $d4
        beq     TerminateEvent          ; end of event if not a nested event
        jsr     _c0a365                   ; return from subroutine
        jmp     NextEventCmd

_a2a6:  cmp     #$05
        bcc     _a2bc
        cmp     #$70
        bcc     _a2c8
        cmp     #$80
        bcc     _a2b9
        cmp     #$a0
        bcc     _a2bf
        sec
        sbc     #$20
_a2b9:  jmp     _c0a449       ; event command $70-$7f, $a0-$ff

; event command $00-$04: move object
_a2bc:  jmp     _c0a45f

; event command $80-$9f: object action
_a2bf:  lda     $df
        cmp     #$05
        bcc     _a2bc       ; branch if move command
        jmp     _c0c7f9       ; object action

; event command $05-$6f: party action
_a2c8:  jmp     _c0c932

; ---------------------------------------------------------------------------

; [ next command ]

NextEventCmd:
        longa
        ldx     $d4
        beq     _a315
        lda     $d6
        cmp     $1142,x
        bne     _a315
        lda     $d8
        cmp     $1144,x
        bne     _a315
        lda     $1162,x
        dec
        sta     $1162,x
        bne     _a30b
        dec     $d4
        dec     $d4
        dec     $d4
        dec     $d4
        ldx     $d4
        lda     $1146,x
        sta     $d6
        lda     $1148,x
        sta     $d8
        lda     $06
        shorta
        stz     $d2
        longa
        cpx     #$0000
        beq     _a315
        bra     NextEventCmd
_a30b:  lda     $1122,x
        sta     $d6
        lda     $1124,x
        sta     $d8
_a315:  lda     $06
        shorta
        jmp     ExecEventCmd

; ---------------------------------------------------------------------------

; [ end of event ]

TerminateEvent:
        lda     $53
        beq     _a358
        lda     $e6
        beq     _a358
        ldy     $06
        stz     $e5
_a328:  lda     $1485,y
        lsr
        bcc     _a344
        lda     $147b,y
        bmi     _a337
        and     #$7f
        bne     _a344
_a337:  lda     $1478,y
        sta     $75
        lda     $147a,y
        sta     $76
        jsr     _c03ce0       ; add object to object layout
_a344:  longa
        tya                 ; next object
        clc
        adc     #$0014
        tay
        lda     $06
        shorta
        inc     $e5
        lda     $e5
        cmp     $e6
        bne     _a328
_a358:  lda     $0adb
        inc
        sta     $bf
        lda     #$01
        sta     $58
        sta     $a1
        rts

.endproc  ; ExecEvent

NextEventCmd := ExecEvent::NextEventCmd
TerminateEvent := ExecEvent::TerminateEvent

; ---------------------------------------------------------------------------

; [ event command $ff: return from subroutine ]

_c0a365:
@a365:  longa
        dec     $d4
        dec     $d4
        dec     $d4
        dec     $d4
        ldx     $d4
        lda     $1146,x
        sta     $d6
        lda     $1148,x
        sta     $d8
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ event command $cd: jump to event ]

; +b1: event index

_c0a380:
@a380:  longa
        ldy     $d4         ; event stack pointer
        lda     #$0001
        sta     $1166,y     ; repeat once
        lda     $d6         ; add 3 to event script pointer
        clc
        adc     #$0003
        sta     $1146,y
        lda     $06
        adc     $d8
        sta     $1148,y
        lda     $df         ; event index
        asl
        clc
        adc     $df
        tax
        lda     f:EventScriptPtrs,x   ; pointer to event script
        sta     $1126,y
        sta     $d6
        lda     f:EventScriptPtrs+2,x
        and     #$00ff
        sta     $1128,y
        sta     $d8
        lda     #$0001
        sta     $1166,y     ; repeat once
        inc     $d4         ; increment stack pointer
        inc     $d4
        inc     $d4
        inc     $d4
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ event command $ce/$cf:  ]

; b1: repeat count
; b2: branch vector (forward)

_c0a3c9:
@a3c9:  longa
        ldx     $d4
        lda     $df         ; repeat count
        and     #$00ff
        sta     $1166,x
        lda     $d6         ; event pointer + 3
        clc
        adc     #$0003
        sta     $d6
        sta     $1126,x     ; event repeat stack
        lda     $06
        adc     $d8
        sta     $d8
        sta     $1128,x
        lda     $e0         ; branch vector
        and     #$00ff
        clc
        adc     $1126,x     ; add to repeat pointer
        sta     $1146,x
        lda     $06
        adc     $1128,x
        sta     $1148,x
        inc     $d4
        inc     $d4
        inc     $d4
        inc     $d4
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ event command $c7:  ]

; b1: branch vector (forward)

_c0a40a:
@a40a:  longa
        ldx     $d4
        lda     #$0001
        sta     $1166,x
        lda     $d6
        clc
        adc     #$0002
        sta     $d6
        sta     $1126,x
        lda     $06
        adc     $d8
        sta     $d8
        sta     $1128,x
        lda     $df         ;
        and     #$00ff
        clc
        adc     $1126,x
        sta     $1146,x
        lda     $06
        adc     $1128,x
        sta     $1148,x
        inc     $d4
        inc     $d4
        inc     $d4
        inc     $d4
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ event command $70-$7f, $a0-$ff ]

_c0a449:
@a449:  sec
        sbc     #$70
        longa
        asl
        tax
        lda     f:EventCmdTbl,x
        sta     $23
        lda     $06
        shorta
        lda     $df
        jmp     ($0b23)                 ; execute event command

; ---------------------------------------------------------------------------

; [ event command $00-$05: move object ]

_c0a45f:
@a45f:  ldy     $06
        lda     [$d6],y
        cmp     #$80
        bcs     @a46c       ; branch if moving an object (not camera)
        jsr     _c0c98a       ; init camera movement
        bra     @a46f
@a46c:  jsr     _c0c7c8       ; init object movement
@a46f:  lda     #$01
        jsr     AddEventPtr
        lda     $d2
        beq     @a488       ; branch if not executing in parallel ???
        ldy     $d4
        ldx     $1142,y
        cpx     $d6
        bne     @a45f
        lda     $1144,y
        cmp     $d8
        bne     @a45f
@a488:  lda     $53         ; branch if on a normal map ???
        bne     @a4a9
        jsr     _c01733       ; update local tile properties (world map)
        jsr     _c00f8c
        jsr     _c01a1d
        jsr     _c04c95       ; clear sprite data
        jsr     _c02137
        jsr     _c01ec5
        jsr     _c01e64
        jsr     _c0420a
        jsr     _c0612b
        bra     @a4c4
@a4a9:  jsr     _c017e8       ; update local tile properties (normal map)
        jsr     _c011c2
        jsr     _c01ae4
        jsr     _c03bac
        jsr     _c04c95       ; clear sprite data
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        jsr     _c0420a
@a4c4:  jsr     WaitVBlank
        lda     $be
        and     #$7f
        bne     @a488
        lda     $61
        and     #$1f
        bne     @a488       ; branch if scrolling
        lda     $63
        and     #$1f
        bne     @a488
        stz     $ba
        stz     $3b
        stz     $3c
        lda     $e6
        beq     @a505       ; branch if there are no objects
        stz     $09
        ldy     $06
@a4e7:  lda     $1488,y     ;
        bne     @a488
        lda     $1486,y     ; object is moving
        bne     @a488
        longa
        tya                 ; next object
        clc
        adc     #$0014
        tay
        lda     $06
        shorta
        inc     $09
        lda     $09
        cmp     $e6         ; number of objects
        bne     @a4e7
@a505:  jmp     NextEventCmd

; ---------------------------------------------------------------------------

; event command jump table ($70-$7f, $a0-$ff)
EventCmdTbl:
        .addr   EventCmd_70
        .addr   EventCmd_71
        .addr   EventCmd_72
        .addr   EventCmd_73
        .addr   EventCmd_74
        .addr   EventCmd_75
        .addr   EventCmd_76
        .addr   EventCmd_77
        .addr   EventCmd_78
        .addr   EventCmd_79
        .addr   EventCmd_7a
        .addr   EventCmd_7b
        .addr   EventCmd_7c
        .addr   EventCmd_7d
        .addr   EventCmd_7e
        .addr   EventCmd_7f

        .addr   EventCmd_a0
        .addr   EventCmd_a1
        .addr   EventCmd_a2
        .addr   EventCmd_a3
        .addr   EventCmd_a4
        .addr   EventCmd_a5
        .addr   EventCmd_a6
        .addr   EventCmd_a7
        .addr   EventCmd_a8
        .addr   EventCmd_a9
        .addr   EventCmd_aa
        .addr   EventCmd_ab
        .addr   EventCmd_ac
        .addr   EventCmd_ad
        .addr   EventCmd_ae
        .addr   EventCmd_af
        .addr   EventCmd_b0
        .addr   EventCmd_b1
        .addr   EventCmd_b2
        .addr   EventCmd_b3
        .addr   EventCmd_b4
        .addr   EventCmd_b5
        .addr   EventCmd_b6
        .addr   EventCmd_b7
        .addr   EventCmd_b8
        .addr   EventCmd_b9
        .addr   EventCmd_ba
        .addr   EventCmd_bb
        .addr   EventCmd_bc
        .addr   EventCmd_bd
        .addr   EventCmd_be
        .addr   EventCmd_bf
        .addr   EventCmd_c0
        .addr   EventCmd_c1
        .addr   EventCmd_c2
        .addr   EventCmd_c3
        .addr   EventCmd_c4
        .addr   EventCmd_c5
        .addr   EventCmd_c6
        .addr   0
        .addr   EventCmd_c8
        .addr   EventCmd_c9
        .addr   EventCmd_ca
        .addr   EventCmd_cb
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   EventCmd_d0
        .addr   EventCmd_d1
        .addr   EventCmd_d2
        .addr   EventCmd_d3
        .addr   EventCmd_d4
        .addr   EventCmd_d5
        .addr   EventCmd_d6
        .addr   EventCmd_d7
        .addr   EventCmd_d8
        .addr   EventCmd_d9
        .addr   EventCmd_da
        .addr   EventCmd_db
        .addr   EventCmd_dc
        .addr   EventCmd_dd
        .addr   0
        .addr   0
        .addr   EventCmd_e0
        .addr   EventCmd_e1
        .addr   EventCmd_e2
        .addr   EventCmd_e3
        .addr   EventCmd_e4
        .addr   EventCmd_e5
        .addr   EventCmd_e6
        .addr   EventCmd_e7
        .addr   EventCmd_e8
        .addr   EventCmd_e9
        .addr   EventCmd_ea
        .addr   EventCmd_eb
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   EventCmd_f0
        .addr   EventCmd_f1
        .addr   0
        .addr   EventCmd_f3
        .addr   EventCmd_f4
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0

; ---------------------------------------------------------------------------

; event command $bf subcommand jump table
_c0a5e8:
        .addr   EventCmd_bf00
        .addr   EventCmd_bf01
        .addr   EventCmd_bf02
        .addr   EventCmd_bf03
        .addr   EventCmd_bf04
        .addr   EventCmd_bf05
        .addr   EventCmd_bf06
        .addr   EventCmd_bf07
        .addr   EventCmd_bf08
        .addr   EventCmd_bf09
        .addr   EventCmd_bf0a
        .addr   EventCmd_bf0b
        .addr   EventCmd_bf0c
        .addr   EventCmd_bf0d
        .addr   EventCmd_bf0e
        .addr   EventCmd_bf0f
        .addr   EventCmd_bf10
        .addr   EventCmd_bf11
        .addr   EventCmd_bf12
        .addr   EventCmd_bf13
        .addr   EventCmd_bf14
        .addr   EventCmd_bf15
        .addr   EventCmd_bf16
        .addr   EventCmd_bf17
        .addr   EventCmd_bf18
        .addr   EventCmd_bf19
        .addr   EventCmd_bf1a
        .addr   0
        .addr   0
        .addr   0
        .addr   0
        .addr   0

; ---------------------------------------------------------------------------

IncEventPtr1:
        lda     #1                      ; 1-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

IncEventPtr2:
        lda     #2                      ; 2-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

IncEventPtr3:
        lda     #3                      ; 3-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

IncEventPtr4:
        lda     #4                      ; 4-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

IncEventPtr5:
        lda     #5                      ; 5-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

; unused
IncEventPtr6:
        lda     #$06                    ; 6-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

IncEventPtr7:
        lda     #7                      ; 7-byte command
        jsr     AddEventPtr
        jmp     NextEventCmd

AddEventPtr:
        longa                           ; add a to event pointer
        clc
        adc     $d6
        sta     $d6
        lda     $06
        adc     $d8
        sta     $d8
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ event command $bf:  ]

; b1: subcommand

EventCmd_bf:
@a672:  lda     $df
        longa
        asl
        tax
        lda     f:_c0a5e8,x   ; event command $bf jump table
        sta     $23
        lda     $06
        shorta
        jmp     ($0b23)

; ---------------------------------------------------------------------------

; [ event command $bf1a:  ]

EventCmd_bf1a:
@a685:  lda     #$fb
        sta     hWOBJSEL
        longa
        lda     #$6200
        sta     $7f6a08
        sta     $7f6a0b
        lda     #$6280
        sta     $7f6a0e
        lda     $06
        shorta
        lda     #$3f
        ldx     $06
@a6a6:  sta     $7f6200,x
        inx
        cpx     #$0080
        bne     @a6a6
        lda     #$01
        sta     $52
        longa
        ldx     $06
        lda     #$f708
@a6bb:  sta     $7f6b6c,x
        inx2
        cpx     #$00e0
        bne     @a6bb
        ldx     $06
@a6c8:  lda     f:_c0a726,x
        sta     $7f6c4c,x
        inx2
        cpx     #$0040
        bne     @a6c8
        lda     $06
        shorta
        stz     $3d
@a6dd:  jsr     WaitVBlank
        lda     $3d
        and     #$01
        beq     @a6ec
        lda     $5e
        and     #$fb
        bra     @a6f0
@a6ec:  lda     $5e
        ora     #$04
@a6f0:  sta     hHDMAEN
        inc     $3d
        lda     $3d
        cmp     #$08
        bne     @a6dd
        lda     #$e0
        ldx     $06
@a6ff:  sta     $7f6200,x
        inx
        cpx     #$0080
        bne     @a6ff
        jsr     WaitVBlank
        lda     $5e
        ora     #$04
        sta     hHDMAEN
        stz     $52
        lda     $5e
        and     #$fb
        sta     $5e
        lda     #$bb
        sta     hWOBJSEL
        jsr     _c08c7b
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0a726:
        .byte   $08,$f7
        .byte   $09,$f7
        .byte   $0a,$f6
        .byte   $0c,$f4
        .byte   $0e,$f2
        .byte   $10,$f0
        .byte   $12,$ee
        .byte   $14,$ec
        .byte   $16,$ea
        .byte   $18,$e8
        .byte   $1a,$e6
        .byte   $1c,$e4
        .byte   $1e,$e2
        .byte   $21,$df
        .byte   $23,$dd
        .byte   $26,$da
        .byte   $28,$d8
        .byte   $2b,$d5
        .byte   $2e,$d2
        .byte   $30,$d0
        .byte   $33,$cd
        .byte   $37,$c9
        .byte   $3a,$c6
        .byte   $3d,$c3
        .byte   $41,$bf
        .byte   $45,$bb
        .byte   $49,$b7
        .byte   $4e,$b2
        .byte   $53,$ad
        .byte   $59,$a7
        .byte   $60,$a0
        .byte   $69,$97

; ---------------------------------------------------------------------------

; [ event command $bf18:  ]

EventCmd_bf18:
@a766:  lda     #$0f
        sta     $6f
        jsr     _c0a7be
        jsr     _c05bf4
        jsr     _c04a68
        stz     $3d
@a775:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        lda     $3d
        and     #$01
        bne     @a787
        lda     $5e
        and     #$fd
        bra     @a78b
@a787:  lda     $5e
        ora     #$02
@a78b:  sta     $5e
        lda     #$1f
        sta     $0c00
        lda     #$03
        sta     hWBGLOG
        lda     #$00
        sta     hWOBJLOG
        jsr     _c06134
        lda     $3d
        lsr
        inc2
        sta     $169a
        jsr     _c05d54
        lda     $3d
        cmp     #$e0
        bne     @a7b3
        jsr     _c04a71
@a7b3:  inc     $3d
        lda     $3d
        cmp     #$f0
        bne     @a775
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0a7be:
@a7be:  lda     #$00
        sta     hWH0
        lda     #$ff
        sta     hWH1
        rts

; ---------------------------------------------------------------------------

; [ event command $bf19:  ]

EventCmd_bf19:
@a7c9:  lda     #$0f
        sta     $6f
        jsr     _c0a7be
        jsr     _c05bf4
        jsr     _c04a68
        lda     #$1f
        sta     $0c00
        lda     #$03
        sta     hWBGLOG
        lda     #$00
        sta     hWOBJLOG
        lda     #$40
        sta     $3d
@a7e9:  jsr     WaitVBlank
        jsr     _c06134
        dec     $3d
        bne     @a7e9
        lda     #$77
        sta     $169a
        jsr     _c05d54
        stz     $da
        stz     $3d
@a7ff:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        lda     $da
        clc
        adc     $3d
        sta     $da
        bcs     @a814
        lda     $5e
        and     #$fd
        bra     @a818
@a814:  lda     $5e
        ora     #$02
@a818:  sta     $5e
        jsr     _c06134
        inc     $3d
        lda     $3d
        cmp     #$80
        bne     @a7ff
        lda     #$e0
        sta     $3d
@a829:  jsr     WaitVBlank
        lda     $3d
        and     #$01
        bne     @a838
        lda     $5e
        and     #$fd
        bra     @a83c
@a838:  lda     $5e
        ora     #$02
@a83c:  sta     $5e
        jsr     _c06134
        lda     $3d
        lsr
        inc2
        sta     $169a
        jsr     _c05d54
        dec     $3d
        bne     @a829
        lda     $5e
        and     #$fd
        sta     $5e
        lda     #$40
        sta     $3d
@a85a:  jsr     WaitVBlank
        lda     $3d
        cmp     #$10
        bne     @a866
        jsr     _c04a71
@a866:  dec     $3d
        bne     @a85a
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf15:  ]

EventCmd_bf15:
@a86d:  jsr     WaitVBlank
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$5a80
        stx     $23
        ldx     #$0400
        stx     $26
        jsr     _c04107
        inc     $a3
        stz     $3d
@a88b:  jsr     WaitVBlank
        jsr     _c01ec5
        jsr     _c0a933
        lda     #$db
        sta     $25
        ldx     #$5a80
        stx     $23
        ldx     #$0400
        stx     $26
        lda     $3d
        lsr2
        jsr     _c04107
        inc     $a3
        inc     $3d
        lda     $3d
        bpl     @a88b
        stz     $3d
@a8b3:  jsr     WaitVBlank
        stz     $49
        jsr     _c04c95       ; clear sprite data
        jsr     _c0a933
        lda     #$a0
        sta     $040f
        lda     #$60
        sec
        sbc     $10a0
        sta     $02fc
        sta     $02f8
        lda     $3d
        lsr4
        clc
        adc     #$7c
        sec
        sbc     $10a2
        sta     $02fd
        lda     #$8c
        sec
        sbc     $10a2
        sta     $02f9
        lda     #$38
        sta     $02fe
        lda     #$30
        sta     $02fa
        lda     #$37
        sta     $02ff
        lda     #$07
        sta     $02fb
        inc     $3d
        bne     @a8b3
        lda     $0aee
        ora     #$80
        sta     $0aee
        lda     #$7f
        sta     $3d
@a90c:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        jsr     _c0a933
        lda     #$db
        sta     $25
        ldx     #$5a80
        stx     $23
        ldx     #$0400
        stx     $26
        lda     $3d
        lsr2
        jsr     _c04107
        inc     $a3
        dec     $3d
        bne     @a90c
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0a933:
@a933:  lda     $3f
        and     #$0c
        lsr
        clc
        adc     #$80
        sta     $0302
        inc
        sta     $0306
        clc
        adc     #$10
        sta     $0206
        dec
        sta     $0202
        lda     #$60
        sec
        sbc     $10a2
        sta     $0300
        sta     $0200
        lda     #$84
        sec
        sbc     $10a2
        sta     $0301
        sta     $0305
        lda     #$68
        sec
        sbc     $10a2
        sta     $0304
        sta     $0204
        lda     #$8c
        sec
        sbc     $10a2
        sta     $0201
        sta     $0205
        lda     #$37
        sta     $0203
        sta     $0207
        sta     $0303
        sta     $0307
        rts

; ---------------------------------------------------------------------------

; [ event command $bf14:  ]

EventCmd_bf14:
@a98b:  ldx     #$7200
        stx     $2e
        ldx     #$1000
        stx     $2c
        jsr     _c04d06       ; clear vram
        ldx     #$0800
        lda     #$00
@a99d:  sta     $7f7621,x
        dex
        bne     @a99d
        jsr     _c04a68
        lda     $5e
        ora     #$18
        sta     $5e
        jsr     _c04c95       ; clear sprite data
        jsr     _c0612b
@a9b3:  jsr     WaitVBlank
        lda     $3d
        and     #$e0
        lsr4
        sta     $08
        clc
        adc     #$72
        sta     $16ae
        stz     $16ad
        lda     $3d
        and     #$e0
        longa
        asl2
        clc
        adc     #$6980
        sta     $23
        lda     $06
        shorta
        lda     #$db
        sta     $25
        ldx     #$0400
        stx     $26
        lda     $3d
        and     #$1f
        jsr     _c04107
        inc     $a3
        jsr     _c0aa0e
        lda     $3f
        and     #$03
        bne     @a9b3
        lda     $3d
        cmp     #$70
        bne     @a9ff
        jsr     _c04a71
@a9ff:  inc     $3d
        lda     $3d
        bpl     @a9b3
        lda     $5e
        and     #$e7
        sta     $5e
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0aa0e:
@aa0e:  ldy     $06
        lda     $3f
        and     #$01
        tax
@aa15:  lda     f:_c0aa89,x
        beq     @aa74
        txa
        and     #$f0
        lsr5
        sta     $08
        lda     #$10
        sec
        sbc     $08
        sta     hWRMPYA
        txa
        and     #$0f
        sta     hWRMPYB
        txa
        clc
        adc     $3f
        phx
        tax
        lda     f:RNGTbl,x
        and     #$07
        plx
        adc     hRDMPYL
        sta     $0200,y
        txa
        and     #$f0
        lsr
        sta     $08
        lda     f:RNGTbl,x
        and     #$07
        clc
        adc     #$90
        sec
        sbc     $08
        sta     $0201,y
        txa
        clc
        adc     $3f
        and     #$08
        lsr2
        clc
        adc     f:_c0aa89,x
        sta     $0202,y
        lda     #$35
        sta     $0203,y
        iny4
@aa74:  inx
        inx
        cpx     #$00c0
        bcc     @aa15
        ldx     $06
@aa7d:  lda     #$aa
        sta     $0400,x
        inx
        cpx     #$0020
        bne     @aa7d
        rts

; ---------------------------------------------------------------------------

_c0aa89:
        .byte   $00,$00,$00,$00,$00,$20,$20,$20,$20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$20,$20,$20,$20,$20,$00,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$20,$20,$20,$20,$40,$20,$00,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$00,$40,$20,$40,$40,$20,$40,$20,$00,$00,$00,$00,$00
        .byte   $00,$00,$00,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$00,$00,$00
        .byte   $00,$00,$00,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$00,$00,$00
        .byte   $00,$00,$40,$60,$40,$60,$40,$40,$40,$60,$40,$60,$60,$00,$00,$00
        .byte   $00,$00,$60,$40,$60,$60,$60,$40,$60,$60,$60,$60,$60,$00,$00,$00
        .byte   $00,$00,$60,$60,$60,$80,$60,$60,$60,$60,$60,$60,$60,$00,$00,$00
        .byte   $00,$00,$60,$80,$80,$80,$80,$60,$80,$80,$60,$60,$80,$80,$00,$00
        .byte   $00,$00,$00,$80,$80,$80,$80,$00,$80,$80,$80,$80,$80,$80,$00,$00
        .byte   $00,$00,$00,$00,$80,$80,$00,$00,$80,$80,$00,$80,$80,$80,$00,$00

; ---------------------------------------------------------------------------

; [ event command $bf13:  ]

EventCmd_bf13:
@ab49:  jsr     _c0abb5
        jsr     _c04a68
        lda     #$7f
        sta     $3d
@ab53:  jsr     WaitVBlank
        jsr     _c0ab8e
        dec     $3d
        lda     $3d
        bpl     @ab53
        jsr     _c04c95       ; clear sprite data
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf12:  ]

EventCmd_bf12:
@ab65:  jsr     _c0abb5
        stz     $3d
@ab6a:  jsr     WaitVBlank
        jsr     _c0ab8e
        inc     $3d
        lda     $3d
        cmp     #$81
        bne     @ab6a
        jsr     _c04a71
        stz     $0b3d
@ab7e:  jsr     WaitVBlank
        stz     $49
        inc     $3d
        lda     $3d
        cmp     #$10
        bne     @ab7e
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0ab8e:
@ab8e:  stz     $49
        lda     $3d
        lsr3
        sta     $da
        lda     #$70
        clc
        adc     $da
        sta     $0225
        dec2
        sta     $0221
        lda     #$58
        clc
        adc     $da
        sta     $0218
        lda     #$88
        sec
        sbc     $da
        sta     $021c
        rts

; ---------------------------------------------------------------------------

_c0abb5:
@abb5:  stz     $49
        jsr     _c04c95       ; clear sprite data
        ldx     $06
@abbc:  lda     f:_c0abd5,x
        sta     $0200,x
        inx
        cpx     #$002c
        bne     @abbc
        lda     #$aa
        sta     $0400
        sta     $0401
        sta     $0402
        rts

; ---------------------------------------------------------------------------

_c0abd5:
        .byte   $50,$70,$30,$01
        .byte   $60,$70,$30,$01
        .byte   $80,$70,$30,$01
        .byte   $90,$70,$30,$01
        .byte   $70,$80,$30,$01
        .byte   $70,$90,$30,$01
        .byte   $58,$70,$34,$35
        .byte   $88,$70,$34,$35
        .byte   $70,$6f,$36,$35
        .byte   $70,$70,$30,$35
        .byte   $70,$70,$32,$35

; ---------------------------------------------------------------------------

; [ event command $bf0f: show epilogue cutscene ]

EventCmd_bf0f:
@ac01:  stz     hMDMAEN
        stz     hHDMAEN
        lda     #$00
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     _c10003     ; epilogue cutscene
        jsr     _c044e3       ; init map bank
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf10: show game stats (unknown cave psychic) ]

EventCmd_bf10:
@ac1c:  stz     hMDMAEN
        stz     hHDMAEN
        lda     #$00
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     _c10006     ; show game stats (unknown cave psychic)
        jsr     _c044e3       ; init map bank
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf07:  ]

EventCmd_bf07:
@ac37:  jsr     _c0ad28
        jsr     _c0ae0b
        jsr     _c0ad54
        lda     #$01        ; show party sprite
        sta     $bd
        jsr     _c0ac80
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf08:  ]

EventCmd_bf08:
@ac4a:  stz     $bd         ; hide party sprite
        jsr     _c04cad       ; hide all sprites
        jsr     _c0ada0
        jsr     _c0acdd
        jsr     _c0ae0b
        jsr     _c0acb1
        lda     #$01        ; show party sprite
        sta     $bd
        jsr     _c0ac80
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf09:  ]

EventCmd_bf09:
@ac65:  jsr     _c0ad01
        jsr     _c0add6
        jsr     _c0ac80
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf0a:  ]

EventCmd_bf0a:
@ac71:  jsr     _c0ada0
        jsr     _c0ae0b
        jsr     _c0ac8b
        jsr     _c0ac80
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0ac80:
@ac80:  lda     #$01
        sta     hWBGLOG
        lda     #$00
        sta     hWOBJLOG
        rts

; ---------------------------------------------------------------------------

_c0ac8b:
@ac8b:  lda     #$3f
        sta     $3d
@ac8f:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$01
        sta     hWOBJLOG
        lda     $3d
        inc2
        sta     $169a
        jsr     _c05d30
        jsr     _c0ad86
        dec     $3d
        bne     @ac8f
        jsr     _c08c7b
        rts

; ---------------------------------------------------------------------------

_c0acb1:
@acb1:  lda     #$3f
        sta     $3d
@acb5:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$01
        sta     hWOBJLOG
        lda     $3d
        inc2
        sta     $169a
        jsr     _c05d30
        jsr     _c0ad86
        jsr     _c0adb7
        dec     $3d
        bne     @acb5
        jsr     WaitVBlank
        jsr     _c08c7b
        rts

; ---------------------------------------------------------------------------

_c0acdd:
@acdd:  lda     #$70
        sta     $3d
@ace1:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$00
        sta     hWOBJLOG
        lda     $3d
        inc2
        sta     $169a
        jsr     _c05d30
        dec     $3d
        lda     $3d
        cmp     #$40
        bne     @ace1
        rts

; ---------------------------------------------------------------------------

_c0ad01:
@ad01:  stz     $3d
@ad03:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$01
        sta     hWOBJLOG
        lda     $3d
        inc2
        sta     $169a
        jsr     _c05d30
        jsr     _c0ad86
        inc     $3d
        lda     $3d
        cmp     #$40
        bne     @ad03
        stz     $bd         ; hide party sprite
        rts

; ---------------------------------------------------------------------------

_c0ad28:
@ad28:  stz     $3d
        stz     $da
@ad2c:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$01
        sta     hWOBJLOG
        lda     $3d
        inc2
        sta     $169a
        jsr     _c05d30
        jsr     _c0ad86
        jsr     _c0adb7
        inc     $3d
        lda     $3d
        cmp     #$40
        bne     @ad2c
        stz     $bd         ; hide party sprite
        rts

; ---------------------------------------------------------------------------

_c0ad54:
@ad54:  lda     #$40
        sta     $3d
@ad58:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$00
        sta     hWOBJLOG
        lda     $3d
        inc2
        sta     $169a
        jsr     _c05d30
        lda     $3d
        cmp     #$60
        bne     @ad7d
        lda     #$90
        sta     $43
        lda     #$f0
        sta     $45
@ad7d:  inc     $3d
        lda     $3d
        cmp     #$78
        bne     @ad58
        rts

; ---------------------------------------------------------------------------

_c0ad86:
@ad86:  lda     #^WindowPal
        sta     $25
        ldx     #near WindowPal
        stx     $23
        ldx     #$0020
        stx     $2c
        ldx     #$0180
        lda     #$7f
        sec
        sbc     $3d
        jsr     _c0406b
        rts

; ---------------------------------------------------------------------------

_c0ada0:
@ada0:  lda     #^WindowPal
        sta     $25
        ldx     #near WindowPal
        stx     $23
        ldx     #$0020
        stx     $2c
        ldx     #$0180
        lda     #$40
        jsr     _c0406b
        rts

; ---------------------------------------------------------------------------

_c0adb7:
@adb7:  lda     $da
        and     #$1f
        clc
        adc     $3d
        sta     $da
        and     #$60
        beq     @adc8
        lda     #$00
        bra     @adca
@adc8:  lda     #$01        ; show party sprite
@adca:  sta     $bd
        jsr     _c04c95       ; clear sprite data
        jsr     _c02137
        jsr     _c0612b
        rts

; ---------------------------------------------------------------------------

_c0add6:
@add6:  stz     $da
        stz     $db
        stz     $3d
@addc:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$01
        sta     hWOBJLOG
        jsr     _c0ae32
        lda     $3d
        and     #$01
        ora     #$3e
        sta     $169a
        jsr     _c05d30
        lda     $3d
        cmp     #$f0
        bne     @ae06
        lda     #$90
        sta     $43
        lda     #$f0
        sta     $45
@ae06:  inc     $3d
        bne     @addc
        rts

; ---------------------------------------------------------------------------

_c0ae0b:
@ae0b:  stz     $da
        stz     $db
        stz     $3d
@ae11:  jsr     WaitVBlank
        lda     #$03
        sta     hWBGLOG
        lda     #$01
        sta     hWOBJLOG
        jsr     _c0ae32
        lda     $3d
        and     #$01
        ora     #$3e
        sta     $169a
        jsr     _c05d30
        inc     $3d
        bne     @ae11
        rts

; ---------------------------------------------------------------------------

_c0ae32:
@ae32:  ldy     #$0070
        sty     $13
        ldy     #$0068
        sty     $15
        lda     #$aa
        sta     $0410
        sta     $0411
        sta     $0412
        sta     $0413
        jsr     Rand
        and     #$01
        bne     @ae6d
        jsr     Rand
        and     #$e0
        sta     $da
        jsr     Rand
        and     #$e0
        sta     $db
        jsr     Rand
        and     #$c0
        sta     $dc
        jsr     Rand
        and     #$c0
        sta     $dd
@ae6d:  lda     $da
        tax
        ldy     $06
@ae72:  lda     $3d
        lsr
        bcs     @aec7
        lda     f:_c0af44,x
        cmp     #$80
        beq     @aec7
        lda     $dc
        asl
        bmi     @ae8d
        lda     $13
        clc
        adc     f:_c0af44,x
        bra     @ae94
@ae8d:  lda     $13
        sec
        sbc     f:_c0af44,x
@ae94:  sta     $0300,y
        lda     $dc
        bmi     @aea4
        lda     $15
        clc
        adc     f:_c0af44+1,x
        bra     @aeab
@aea4:  lda     $15
        sec
        sbc     f:_c0af44+1,x
@aeab:  sta     $0301,y
        lda     $3d
        asl2
        and     #$08
        clc
        adc     f:_c0af44+2,x
        sta     $0302,y
        lda     f:_c0af44+3,x
        eor     $dc
        sta     $0303,y
        bra     @aecc
@aec7:  lda     #$f0
        sta     $0301,y
@aecc:  inx4
        iny4
        cpy     #$0020
        bne     @ae72
        lda     $db
        tax
@aedc:  lda     $3d
        lsr
        bcs     @af31
        lda     f:_c0af44,x
        cmp     #$80
        beq     @af31
        lda     $dd
        asl
        bmi     @aef7
        lda     $13
        clc
        adc     f:_c0af44,x
        bra     @aefe
@aef7:  lda     $13
        sec
        sbc     f:_c0af44,x
@aefe:  sta     $0300,y
        lda     $dd
        bmi     @af0e
        lda     f:_c0af44+1,x
        clc
        adc     $15
        bra     @af15
@af0e:  lda     $15
        sec
        sbc     f:_c0af44+1,x
@af15:  sta     $0301,y
        lda     $3d
        asl2
        and     #$08
        clc
        adc     f:_c0af44+2,x
        sta     $0302,y
        lda     f:_c0af44+3,x
        eor     $dd
        sta     $0303,y
        bra     @af36
@af31:  lda     #$f0
        sta     $0301,y
@af36:  inx4
        iny4
        cpy     #$0040
        bne     @aedc
        rts

; ---------------------------------------------------------------------------

_c0af44:
        .byte   $00,$b0,$30,$3b
        .byte   $10,$b0,$32,$3b
        .byte   $20,$b0,$36,$3b
        .byte   $00,$c0,$36,$bb
        .byte   $30,$c0,$34,$3b
        .byte   $30,$d0,$30,$bb
        .byte   $40,$d0,$36,$3b
        .byte   $40,$e0,$34,$3b
        .byte   $00,$e0,$36,$3b
        .byte   $00,$f0,$30,$bb
        .byte   $10,$f0,$36,$3b
        .byte   $10,$00,$34,$3b
        .byte   $00,$10,$30,$3b
        .byte   $10,$10,$36,$bb
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b
        .byte   $f0,$c0,$30,$3b
        .byte   $00,$c0,$32,$3b
        .byte   $10,$c0,$36,$3b
        .byte   $e0,$d0,$30,$3b
        .byte   $f0,$d0,$36,$bb
        .byte   $10,$d0,$34,$3b
        .byte   $e0,$e0,$36,$bb
        .byte   $10,$e0,$36,$bb
        .byte   $e0,$b0,$30,$3b
        .byte   $f0,$b0,$32,$3b
        .byte   $00,$b0,$36,$3b
        .byte   $e0,$c0,$34,$3b
        .byte   $00,$c0,$30,$bb
        .byte   $10,$c0,$36,$3b
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b
        .byte   $00,$d0,$30,$3b
        .byte   $10,$d0,$32,$3b
        .byte   $20,$d0,$36,$3b
        .byte   $20,$e0,$34,$3b
        .byte   $20,$f0,$36,$bb
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b
        .byte   $f0,$c0,$30,$3b
        .byte   $00,$c0,$36,$3b
        .byte   $e0,$d0,$30,$3b
        .byte   $f0,$d0,$36,$bb
        .byte   $00,$d0,$30,$bb
        .byte   $10,$d0,$32,$3b
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b
        .byte   $20,$d0,$30,$3b
        .byte   $30,$d0,$36,$3b
        .byte   $30,$e0,$30,$bb
        .byte   $40,$e0,$36,$3b
        .byte   $40,$f0,$34,$3b
        .byte   $30,$00,$30,$3b
        .byte   $40,$00,$36,$bb
        .byte   $80,$00,$30,$3b
        .byte   $00,$e0,$32,$3b
        .byte   $10,$e0,$36,$3b
        .byte   $10,$f0,$30,$bb
        .byte   $20,$f0,$36,$3b
        .byte   $20,$00,$34,$3b
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b
        .byte   $80,$00,$30,$3b

; ---------------------------------------------------------------------------

; [ event command $bf00: game over ]

EventCmd_bf00:
@b044:  lda     #$f0        ; don't reset spc
        sta     $1d00
        jmp     Start

; ---------------------------------------------------------------------------

; [ event command $bf01: load parent map ]

EventCmd_bf01:
@b04c:  jsr     LoadParentMap
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf16:  ]

EventCmd_bf16:
@b052:  lda     #$ff
        sta     $3d
        ldx     #$0410
        stx     $1a53
        lda     #$07
        sta     $1a55
@b061:  jsr     WaitVBlank
        lda     $3d
        lsr4
        tax
        lda     f:_c0b0fb,x
        sta     $16af
        lda     #$34
        sta     $16b0
        ldy     #$0280
        sty     $23
        ldy     #$0358
        sty     $26
        ldy     #$0074
        sty     $13
        sty     $15
        ldx     $06
        stx     $16b1
        jsr     _c04f48       ; update shattering crystal
        longa
        lda     $1a53
        sec
        sbc     #$0004
        sta     $1a53
        lda     $06
        shorta
        dec     $3d
        bne     @b061
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf02: crystal shatters ]

EventCmd_bf02:
@b0a7:  stz     $3d
        ldx     #$0020
        stx     $1a53
        lda     #$01
        sta     $1a55
@b0b4:  jsr     WaitVBlank
        lda     $3d
        lsr3
        tax
        lda     f:_c0b0fb,x
        sta     $16af
        lda     #$34
        sta     $16b0
        ldy     #$0280
        sty     $23
        ldy     #$0358
        sty     $26
        ldy     #$0074
        sty     $13
        sty     $15
        ldx     $06
        stx     $16b1
        jsr     _c04f48       ; update shattering crystal
        longa
        lda     $1a53
        clc
        adc     #$0010
        sta     $1a53
        lda     $06
        shorta
        inc     $3d
        lda     $3d
        bpl     @b0b4
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0b0fb:
        .byte   $10,$11,$12,$13,$14,$15,$16,$17,$17,$17,$17,$17,$17,$17,$17,$17

; ---------------------------------------------------------------------------

; [ event command $bf11:  ]

EventCmd_bf11:
@b10b:  lda     #$07
        sta     $6f
        jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        lda     #$01
        sta     $52
        jsr     _c05bf4
        jsr     _c04a68
        jsr     WaitVBlank
        ldx     #$7800
        stx     $2e
        ldx     #$0800
        stx     $2c
        lda     #$db
        sta     $25
        ldx     #$6d80
        stx     $23
        jsr     _c04cbc       ; copy data to vram
        jsr     WaitVBlank
        ldx     #$7c00
        stx     $2e
        ldx     #$0800
        stx     $2c
        lda     #$db
        sta     $25
        ldx     #$7580
        stx     $23
        jsr     _c04cbc       ; copy data to vram
        stz     $52
        ldx     #$ff80
        stx     $15
        stz     $3d
@b15a:  jsr     WaitVBlank
        lda     $3f
        lsr
        bcc     @b15a
        ldx     $63
        inx
        stx     $63
        lda     $3d
        cmp     #$b0
        bcs     @b172
        ldx     $15
        inx
        stx     $15
@b172:  jsr     _c0b240
        lda     $3d
        cmp     #$f0
        bne     @b17e
        jsr     _c04a71
@b17e:  inc     $3d
        bne     @b15a
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf05:  ]

EventCmd_bf05:
@b185:  jsr     _c04c95       ; clear sprite data
        jsr     _c05bf4
        lda     #$07
        sta     $6f
        ldx     #$7800
        stx     $2e
        ldx     #$1000
        stx     $2c
        jsr     _c04d06       ; clear vram
        ldx     #$0050
        stx     $15
        lda     #$00
        jsr     _c0b3cd
        jsr     _c04ce8       ; disable interrupts
        ldx     #$7c00
        stx     $2e
        ldx     #$0800
        stx     $2c
        lda     #$db
        sta     $25
        ldx     #$7580
        stx     $23
        jsr     _c04cbc       ; copy data to vram
        jsr     _c04cfa       ; enable interrupts
        jsr     _c04a68
        stz     $3d
@b1c7:  jsr     WaitVBlank
        lda     #^(MapSpritePal + $224)
        sta     $25
        ldx     #near (MapSpritePal + $224)
        stx     $23
        ldx     #$0018
        stx     $2c
        ldx     #$0104
        lda     $3d
        lsr
        inc
        jsr     _c0406b
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$6d80
        stx     $23
        ldx     #$0800
        stx     $26
        lda     $3d
        lsr3
        jsr     _c04107
        inc     $a3
        inc     $3d
        bne     @b1c7
@b204:  jsr     WaitVBlank
        lda     $3d
        lsr
        bcs     @b211
        ldx     $15
        dex
        stx     $15
@b211:  jsr     _c0b240
        inc     $3d
        lda     $3d
        cmp     #$60
        bne     @b204
        stz     $3d
@b21e:  jsr     WaitVBlank
        jsr     _c0b240
        lda     $3d
        lsr
        bcs     @b22e
        ldx     $63
        inx
        stx     $63
@b22e:  lda     $3d
        cmp     #$f0
        bne     @b237
        jsr     _c04a71
@b237:  inc     $3d
        bne     @b21e
        stz     $52
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0b240:
@b240:  lda     $3f
        lsr3
        and     #$07
        tax
        lda     f:_c0b250,x
        jsr     _c0b3cd
        rts

; ---------------------------------------------------------------------------

_c0b250:
        .byte   $00,$00,$00,$60,$C0,$C0,$C0,$C0,$60

; ---------------------------------------------------------------------------

; [ event command $bf04:  ]

EventCmd_bf04:
@b259:  jsr     _c0b371
        lda     #$38
        sta     $13
        lda     #$58
        sta     $15
        ldy     #$0080
        ldx     #$0050
        jsr     _c0b2a5
        stz     $3d
@b26f:  jsr     WaitVBlank
        jsr     _c04cad       ; hide all sprites
        jsr     _c0b35c
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$7a40
        stx     $23
        ldx     #$0800
        stx     $26
        lda     $3d
        lsr3
        jsr     _c04107
        inc     $a3
        jsr     _c0b5fd
        inc     $3d
        bne     @b26f
        jsr     _c0b594
        stz     $52
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0b2a5:
@b2a5:  sty     $0d
        ldy     $06
@b2a9:  tya
        and     #$e0
        lsr
        clc
        adc     $15
        sta     $0301,y
        tya
        and     #$1c
        asl2
        clc
        adc     $13
        sta     $0300,y
        lda     f:_c0b2ec,x
        and     #$0f
        ora     #$80
        sta     $0302,y
        lda     f:_c0b2ec,x
        and     #$c0
        ora     #$31
        sta     $0303,y
        inx
        iny4
        cpy     $0d
        bne     @b2a9
        ldy     #$0010
        lda     #$aa
@b2e2:  sta     $0400,y
        iny
        cpy     #$0020
        bne     @b2e2
        rts

; ---------------------------------------------------------------------------

_c0b2ec:
        .byte   $02,$02,$02,$02,$02,$02,$02,$84,$02,$02,$02,$02,$02,$02,$84,$00
        .byte   $02,$02,$02,$02,$02,$02,$04,$00,$02,$02,$02,$02,$02,$02,$02,$04
        .byte   $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02
        .byte   $06,$06,$06,$06,$06,$06,$06,$06,$02,$02,$02,$02,$02,$02,$02,$02
        .byte   $02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02

; ---------------------------------------------------------------------------

_c0b35c:
@b35c:  lda     #$00
        sta     hM7A
        lda     #$02
        sta     hM7A
        lda     #$00
        sta     hM7D
        lda     #$02
        sta     hM7D
        rts

; ---------------------------------------------------------------------------

_c0b371:
@b371:  lda     #$02
        sta     $5e
        ldx     #$7800
        stx     $2e
        ldx     #$1000
        stx     $2c
        jsr     _c04d06       ; clear vram
        lda     #$0a
        sta     $df
        lda     #$00
        sta     $e0
        jsr     _c0baae       ; change color palette
        lda     #$01
        sta     $a4
        inc     $a2
        jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        lda     #$01
        sta     $52
        jsr     _c041f1
        jsr     _c04a68
        stz     $3d
@b3a5:  jsr     WaitVBlank
        lda     $3d
        sta     hM7A
        lda     #$01
        sta     hM7A
        lda     $3d
        sta     hM7D
        lda     #$01
        sta     hM7D
        lda     $3d
        lsr4
        jsr     _c05c01
        inc     $3d
        bne     @b3a5
        jsr     _c05bf4
        rts

; ---------------------------------------------------------------------------

_c0b3cd:
@b3cd:  tax
        ldy     $06
@b3d0:  lda     f:_c0b428,x
        clc
        adc     #$2c
        sta     $0200,y
        lda     f:_c0b428+1,x
        longa
        cmp     #$00ff
        beq     @b3f7
        clc
        adc     $15
        cmp     #$fff0
        bcs     @b3f2
        cmp     #$00f0
        bcs     @b3f7
@b3f2:  and     #$00ff
        bra     @b3fa
@b3f7:  lda     #$00f0
@b3fa:  shorta
        sta     $0201,y
        lda     f:_c0b428+2,x
        sta     $0202,y
        lda     f:_c0b428+3,x
        sta     $0203,y
        inx4
        iny4
        cpy     #$0060
        bne     @b3d0
        ldy     $06
        lda     #$aa
@b41e:  sta     $0400,y
        iny
        cpy     #$0020
        bne     @b41e
        rts

; ---------------------------------------------------------------------------

_c0b428:
        .byte   $00,$00,$a6,$71
        .byte   $10,$00,$a4,$71
        .byte   $20,$00,$a2,$71
        .byte   $30,$00,$a0,$71
        .byte   $50,$00,$a0,$31
        .byte   $60,$00,$a2,$31
        .byte   $70,$00,$a4,$31
        .byte   $80,$00,$a6,$31
        .byte   $10,$10,$ac,$71
        .byte   $20,$10,$aa,$71
        .byte   $30,$10,$a8,$71
        .byte   $40,$10,$80,$31
        .byte   $50,$10,$a8,$31
        .byte   $60,$10,$aa,$31
        .byte   $70,$10,$ac,$31
        .byte   $30,$20,$82,$31
        .byte   $40,$20,$84,$31
        .byte   $50,$20,$86,$31
        .byte   $30,$30,$88,$31
        .byte   $40,$30,$8a,$31
        .byte   $50,$30,$8c,$31
        .byte   $40,$40,$8e,$31
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $00,$10,$c4,$71
        .byte   $10,$10,$c2,$71
        .byte   $20,$10,$c0,$71
        .byte   $30,$10,$ae,$71
        .byte   $40,$10,$e4,$31
        .byte   $50,$10,$ae,$31
        .byte   $60,$10,$c0,$31
        .byte   $70,$10,$c2,$31
        .byte   $80,$10,$c4,$31
        .byte   $30,$20,$82,$31
        .byte   $40,$20,$84,$31
        .byte   $50,$20,$86,$31
        .byte   $30,$30,$88,$31
        .byte   $40,$30,$8a,$31
        .byte   $50,$30,$8c,$31
        .byte   $40,$40,$8e,$31
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $30,$10,$c6,$71
        .byte   $40,$10,$e0,$31
        .byte   $50,$10,$c6,$31
        .byte   $20,$20,$ca,$71
        .byte   $30,$20,$e2,$31
        .byte   $40,$20,$84,$31
        .byte   $50,$20,$c8,$31
        .byte   $60,$20,$ca,$31
        .byte   $10,$30,$ce,$71
        .byte   $20,$30,$cc,$71
        .byte   $30,$30,$88,$31
        .byte   $40,$30,$8a,$31
        .byte   $50,$30,$8c,$31
        .byte   $60,$30,$cc,$31
        .byte   $70,$30,$ce,$31
        .byte   $40,$40,$8e,$31
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00
        .byte   $80,$ff,$00,$00

; ---------------------------------------------------------------------------

; [ event command $bf06:  ]

EventCmd_bf06:
@b548:  jsr     _c0b371
        lda     #$40
        sta     $13
        lda     #$40
        sta     $15
        ldy     #$00c0
        ldx     #$0000
        jsr     _c0b2a5
        stz     $3d
@b55e:  jsr     WaitVBlank
        jsr     _c04cad       ; hide all sprites
        jsr     _c0b35c
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$7a40
        stx     $23
        ldx     #$0400
        stx     $26
        lda     $3d
        lsr3
        jsr     _c04107
        inc     $a3
        jsr     _c0b5fd
        inc     $3d
        bne     @b55e
        jsr     _c0b594
        stz     $52
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0b594:
@b594:  lda     #$84
        sta     $43
        lda     #$f0
        sta     $45
@b59c:  jsr     WaitVBlank
        jsr     _c04cad       ; hide all sprites
        jsr     _c0b35c
        jsr     _c0420a
        inc     $3d
        lda     $3d
        cmp     #$40
        bne     @b59c
        rts

; ---------------------------------------------------------------------------

; [ event command $bf0b: tower of walse sinking ]

EventCmd_bf0b:
@b5b1:  jsr     _c0b371
        lda     #$22
        sta     $13
        lda     #$41
        sta     $15
        ldy     #$00e0
        ldx     #$0000
        jsr     _c0b2a5
        stz     $3d
@b5c7:  jsr     WaitVBlank
        jsr     _c04cad       ; hide all sprites
        jsr     _c0b35c
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$7a40
        stx     $23
        ldx     #$0800
        stx     $26
        lda     $3d
        lsr3
        jsr     _c04107
        inc     $a3
        jsr     _c0b5fd
        inc     $3d
        bne     @b5c7
        jsr     _c0b594
        stz     $52
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0b5fd:
@b5fd:  lda     #$c7
        sta     $df
        lda     #$1e
        sta     $e0
        sta     $e1
        jsr     _c0b9f5
        jsr     _c0420a
        rts

; ---------------------------------------------------------------------------

; [ event command $bf17:  ]

EventCmd_bf17:
@b60e:  jsr     _c04ce8       ; disable interrupts
        jsr     _c01e14
        jsr     _c04cfa       ; enable interrupts
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$5580
        stx     $23
        ldx     #$0700
        stx     $26
        lda     #$1f
        jsr     _c04107
        inc     $a3
        jsr     _c04a68
        jsr     _c0b750
        jsr     _c05bf4
        lda     #$0f
        sta     $6f
        lda     #$ff
        sta     $3d
@b643:  jsr     WaitVBlank
        lda     $3d
        lsr2
        sta     $da
        jsr     _c04c95       ; clear sprite data
        jsr     _c0b791
        jsr     _c0612b
        lda     #$87
        sta     $df
        lda     #$1e
        sta     $e0
        lda     $3d
        eor     #$ff
        inc
        lsr4
        clc
        adc     #$0e
        sta     $e1
        jsr     _c0b9f5
        jsr     _c0420a
        jsr     _c0bc9f
        dec     $3d
        lda     $3d
        bne     @b643
        stz     $3d
@b67c:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        jsr     _c0b791
        jsr     _c0612b
        lda     #$87
        sta     $df
        lda     #$1e
        sta     $e0
        sta     $e1
        jsr     _c0b9f5
        jsr     _c0420a
        jsr     _c0bc9f
        lda     $3d
        cmp     #$e0
        bne     @b6a9
        lda     #$88
        sta     $43
        lda     #$f0
        sta     $45
@b6a9:  inc     $3d
        bne     @b67c
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf03:  ]

EventCmd_bf03:
@b6b0:  ldx     #$7800
        stx     $2e
        ldx     #$1000
        stx     $2c
        jsr     _c04d06
        lda     #$0f
        sta     $6f
        jsr     _c0b750
        stz     $da
        jsr     _c0b791
        jsr     _c05bf4
        jsr     _c04a68
        stz     $3d
@b6d1:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        jsr     _c0b791
        jsr     _c0612b
        lda     $3d
        cmp     #$c0
        bcs     @b6f0
        lda     #$87
        sta     $df
        lda     #$1e
        sta     $e0
        sta     $e1
        jsr     _c0b9f5
@b6f0:  jsr     _c0420a
        inc     $3d
        bne     @b6d1
        stz     $d3
        stz     $3d
@b6fb:  jsr     WaitVBlank
        jsr     _c04c95       ; clear sprite data
        jsr     _c0b791
        jsr     _c0612b
        ldx     #$7800
        stx     $16ad
        lda     #$db
        sta     $25
        ldx     #$5580
        stx     $23
        ldx     #$0700
        stx     $26
        lda     $3d
        lsr2
        jsr     _c04107
        inc     $a3
        inc     $3d
        lda     $3d
        bpl     @b6fb
        stz     $3d
@b72c:  jsr     WaitVBlank
        lda     $3d
        lsr2
        sta     $da
        lda     $3d
        cmp     #$f0
        bne     @b73e
        jsr     _c04a71
@b73e:  jsr     _c04c95       ; clear sprite data
        jsr     _c0b791
        jsr     _c0612b
        inc     $3d
        lda     $3d
        bne     @b72c
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0b750:
@b750:  ldy     $06
        lda     #$00
@b754:  sta     $0da0,y
        iny
        cpy     #$0020
        bne     @b754
        rts

; ---------------------------------------------------------------------------

_c0b75e:
        lda     $3d
        longa
        asl
        clc
        adc     #$0200
        sta     $0d
        lda     $06
        shorta
        lda     $0d
        sta     hM7A
        lda     $0e
        sta     hM7A
        lda     $0d
        sta     hM7D
        lda     $0e
        sta     hM7D
        lda     $3d
        bpl     @b790
        and     #$7f
        lsr4
        ora     #$e0
        sta     hCOLDATA
@b790:  rts

; ---------------------------------------------------------------------------

_c0b791:
@b791:  ldx     $06
@b793:  lda     f:_c0b804,x
        sta     $0300,x
        lda     f:_c0b804+1,x
        sec
        sbc     $da
        sta     $0301,x
        lda     f:_c0b804+2,x
        sta     $0302,x
        lda     f:_c0b804+3,x
        ora     #$36
        sta     $0303,x
        inx4
        cpx     #$0030
        bne     @b793
        ldx     $06
@b7bf:  lda     f:_c0b804,x
        sta     $0330,x
        lda     f:_c0b804+1,x
        clc
        adc     #$04
        sta     $0331,x
        lda     f:_c0b804+2,x
        sta     $0332,x
        lda     f:_c0b804+3,x
        ora     #$0a
        sta     $0333,x
        inx4
        cpx     #$0030
        bne     @b7bf
        lda     #$aa
        sta     $0410
        sta     $0411
        sta     $0412
        sta     $0413
        sta     $0414
        sta     $0415
        sta     $0416
        sta     $0417
        rts

; ---------------------------------------------------------------------------

_c0b804:
        .byte   $68,$58,$80,$01
        .byte   $78,$58,$82,$01
        .byte   $88,$58,$84,$01
        .byte   $60,$68,$86,$01
        .byte   $70,$68,$88,$01
        .byte   $80,$68,$8a,$01
        .byte   $90,$68,$8c,$01
        .byte   $60,$78,$8e,$01
        .byte   $70,$78,$a0,$01
        .byte   $80,$78,$a2,$01
        .byte   $90,$78,$a4,$01
        .byte   $78,$88,$a6,$01

; ---------------------------------------------------------------------------

; [ event command $bf0c:  ]

EventCmd_bf0c:
@b834:  jsr     _c0b854
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf0e:  ]

EventCmd_bf0e:
@b83a:  lda     #$82
        sta     $43
        lda     #$f0
        sta     $45
        jsr     _c0b854
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $bf0d:  ]

EventCmd_bf0d:
@b848:  lda     #$02
        sta     $43
        stz     $45
        jsr     _c0b854
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0b854:
@b854:  jsr     WaitVBlank
        lda     #$80
        sta     $da
        lda     #$01
        sta     $52
@b85f:  jsr     WaitVBlank
        lda     $5e
        ora     #$80
        sta     hHDMAEN
        lda     $45
        eor     #$ff
        inc
        sta     hWRMPYA
        ldx     $06
@b873:  jsr     Rand
        and     #$f0
        sta     hWRMPYB
        nop4
        lda     hRDMPYH
        ora     #$0f
        sta     $7f6a49,x
        inx
        cpx     #$0010
        bne     @b873
        dec     $da
        bne     @b85f
        jsr     WaitVBlank
        stz     hHDMAEN
        stz     $52
        lda     #$00
        sta     hMOSAIC
        rts

; ---------------------------------------------------------------------------

; [ event command $ea: show map title ]

EventCmd_ea:
@b8a0:  lda     #$01        ; enable map title
        sta     $16a0
        jsr     _c04ce8       ; disable interrupts
        jsr     _c0928c       ; init map title
        jsr     _c04cfa       ; enable interrupts
        jsr     _c04a68
        jsr     _c09267       ; show map title
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $eb: transfer galuf's stats to krile ]

EventCmd_eb:
@b8b7:  lda     #$06        ; menu command $06 (transfer galuf's stats to krile)
        sta     $0134
        jsr     OpenMenuNoFade
        jsr     _c05532       ; load world map
        jsr     _c06100       ; fade in
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $e9: load character stats (ending) ]

EventCmd_e9:
@b8c8:  ldy     $06
        ldx     $06
@b8cc:  lda     $0500,y
        and     #$bf
        sta     $0500,y
        and     #$07
        cmp     $df
        beq     @b8e4
        lda     $0500,y
        ora     #$40
        sta     $0500,y
        bra     @b8e8
@b8e4:  sty     $23
        stx     $26
@b8e8:  inx
        jsr     _c0c19d
        cpy     #$0140
        bne     @b8cc
        lda     $26
        tax
        lda     f:_c0b98d,x   ; pointers to character abilities (+$08f7)
        tay
        ldx     $06
@b8fb:  lda     $08f7,y
        sta     $7f7622,x
        iny
        cpy     #$0050
        bne     @b90a
        ldy     $06
@b90a:  inx
        cpx     #$0050
        bne     @b8fb
        ldy     #$0014
        ldx     $06
@b915:  lda     $7f7622,x
        sta     $08f7,y     ; character abilities
        iny
        cpy     #$0050
        bne     @b924
        ldy     $06
@b924:  inx
        cpx     #$0050
        bne     @b915
        ldy     $26
        ldx     $06
@b92e:  lda     $08f3,y
        sta     $7f7622,x
        iny
        tya
        and     #$03
        tay
        inx
        cpx     #$0004
        bne     @b92e
        ldy     #$0001
        ldx     $06
@b945:  lda     $7f7622,x
        sta     $08f3,y
        iny
        tya
        and     #$03
        tay
        inx
        cpx     #$0004
        bne     @b945
        ldy     $23
        ldx     $06
@b95b:  lda     $0500,y
        sta     $7f7622,x
        iny
        cpy     #$0140
        bne     @b96a
        ldy     $06
@b96a:  inx
        cpx     #$0140
        bne     @b95b
        ldy     #$0050
        ldx     $06
@b975:  lda     $7f7622,x
        sta     $0500,y
        iny
        cpy     #$0140
        bne     @b984
        ldy     $06
@b984:  inx
        cpx     #$0140
        bne     @b975
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0b98d:
        .byte   $00,$14,$28,$3c

; ---------------------------------------------------------------------------

; [ event command $e7:  ]

EventCmd_e7:
@b991:  ldy     $06
        stz     $0d
@b995:  lda     $051a,y     ; character status
        and     #$02
        bne     @b9b6
        lda     $051a,y
        and     #$40
        bne     @b9b6
        lda     $051a,y
        bmi     @b9b6
        lda     $0500,y
        and     #$07
        tax
        lda     $0d
        ora     f:_c0b9ca,x
        sta     $0d
@b9b6:  jsr     _c0c19d
        cpy     #$0140
        bne     @b995
        lda     $0d
        dec
        asl2
        inc
        jsr     AddEventPtr
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

_c0b9ca:
        .byte   1,2,4,8,4

; ---------------------------------------------------------------------------

; [ event command $e6: convert color palettes to grayscale ]

EventCmd_e6:
@b9cf:  jsr     _c04008       ; convert color palettes to grayscale
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $e4: wait for spc-2 ]

EventCmd_e4:
@b9d5:  lda     hAPUIO2                 ; wait for spc-2
        bne     @b9d5
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $e5: wait for spc-3 ]

EventCmd_e5:
@b9dd:  lda     hAPUIO3                 ; wait for spc-3
        bne     @b9dd
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $dd: set character data ]

; +b1: pointer to character data
;  b2: data value

EventCmd_dd:
@b9e5:  ldy     $df
        lda     $e1
        sta     $0500,y     ; character data
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [ event command $da:  ]

EventCmd_da:
@b9ef:  jsr     _c0b9f5
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [  ]

_c0b9f5:
@b9f5:  ldy     $06
@b9f7:  lda     $1a58,y
        beq     @ba10
        longa
        tya
        clc
        adc     #$0008
        tay
        lda     $06
        shorta
        cpy     #$0080
        bne     @b9f7
        jmp     @ba85
@ba10:  longa
        lda     $df
        and     #$0038
        asl3
        sta     $1a56
        lda     $06
        shorta
        lda     $df
        and     #$c0
        lsr4
        tax
        jsr     Rand
        and     f:_c0ba86+2,x
        clc
        adc     #$10
        sta     $1a58,y
        lda     $df
        and     #$07
        asl
        tax
        jsr     Rand
        and     f:_c0ba86+2,x
        longa
        sec
        sbc     f:_c0ba86,x
        sta     $1a5a,y
        lda     $06
        shorta
        jsr     Rand
        and     f:_c0ba86+2,x
        longa
        sec
        sbc     f:_c0ba86,x
        sta     $1a5c,y
        lda     $e0
        and     #$00ff
        asl3
        clc
        adc     $1a5a,y
        sta     $1a5a,y
        lda     $e1
        and     #$00ff
        asl3
        clc
        adc     $1a5c,y
        sta     $1a5c,y
        lda     $06
        shorta
@ba85:  rts

; ---------------------------------------------------------------------------

_c0ba86:
        .word   %0
        .word   %0
        .word   %1
        .word   %11
        .word   %111
        .word   %1111
        .word   %11111
        .word   %111111
        .word   %1111111

; ---------------------------------------------------------------------------

; [ event command $79: show mini-map ]

EventCmd_79:
@ba98:  jsr     ShowMinimapNoFade
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $db: update party graphic ]

EventCmd_db:
@ba9e:  jsr     UpdatePlayerGfx
        lda     #$01
        sta     $a1
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $d9: change color palette ]

EventCmd_d9:
@baa8:  jsr     _c0baae       ; change color palette
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ change color palette ]

_c0baae:
@baae:  lda     $df
        longa
        asl5
        tay
        lda     $e0
        and     #$00ff
        asl5
        tax
        lda     $06
        shorta
        lda     #$20
        sta     $09
@bacb:  lda     f:MapSpritePal + $200,x ; color palette
        sta     $0c00,y
        inx
        iny
        dec     $09
        bne     @bacb
        rts

; ---------------------------------------------------------------------------

; [ event command $b6: show cutscene ]

; b1: cutscene id

EventCmd_b6:
@bad9:  lda     $df
        jsr     ShowCutscene
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $ae: pixelate the screen ]

; b1: speed

EventCmd_ae:
@bae1:  lda     $df
        sta     $16aa
        ldx     #$1e00
        stx     $16ab
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a6: set battle flag ]

EventCmd_a6:
@baef:  lda     $df
        jsr     _c0c9fa       ; set battle flag
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a7: clear battle flag ]

EventCmd_a7:
@baf7:  lda     $df
        jsr     _c0ca08       ; clear battle flag
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $7b:  ]

EventCmd_7b:
@baff:  lda     $0adc
        dec
        asl2
        tay
        sty     $169c
        lda     $0adc
        cmp     #$02
        bne     @bb15
        jsr     BoardBlackChoco
        bra     @bb21
@bb15:  cmp     #$03
        bne     @bb1e
        jsr     BoardHiryuu
        bra     @bb21
@bb1e:  jsr     BoardAirship
@bb21:  jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $7f:  ]

EventCmd_7f:
@bb24:  jsr     _c01733       ; update local tile properties (world map)
        lda     $0adc
        dec
        asl2
        tay
        sty     $169c
        lda     $0adc
        cmp     #$02
        bne     @bb3d
        jsr     LandBlackChoco
        bra     @bb49
@bb3d:  cmp     #$03
        bne     @bb46
        jsr     LandHiryuu
        bra     @bb49
@bb46:  jsr     LandAirship
@bb49:  jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $ad: inn ]

; b1: price

EventCmd_ad:
@bb4c:  jsr     CalcInnPrice
        jsr     _c04dd7       ;
        ldx     #$000d      ; dialog message $000d "welcome!  it is \gp gp per night ..."
        stx     $af
        jsr     GetDlgPtr
@bb5a:  jsr     _c08f54       ;
        ldx     $b1
        lda     f:Dlg,x   ; dialog
        beq     @bb8a
        jsr     LoadDlgText
        ldy     $06
        sty     $ab
@bb6c:  jsr     _c08d3b
        jsr     WaitVBlank
        ldy     $ab
        cpy     #$0040
        bne     @bb6c
        lda     $b3
        bne     @bb8a
        jsr     _c04aad
        jsr     _c04ac1       ; wait for keypress
        inc     $a5
        jsr     WaitVBlank
        bra     @bb5a
@bb8a:  lda     $0947
        sta     $37
        lda     $0948
        sta     $38
        lda     $0949
        sta     $39
        jsr     _c04dd7
        jsr     _c09133       ; show gp window
        jsr     _c08fed       ; show yes/no window
        lda     #$80
        sta     $1697
@bba7:  jsr     WaitVBlank
        lda     $03
        and     #>(JOY_UP | JOY_DOWN)
        beq     @bbbd
        cmp     #$08
        bne     @bbb8
        lda     #$80
        bra     @bbba
@bbb8:  lda     #$81
@bbba:  sta     $1697
@bbbd:  lda     $03
        bpl     @bbc8
        lda     #$01
        sta     $1697
        bra     @bbd6
@bbc8:  lda     $02
        and     #JOY_A
        beq     @bba7
        lda     $10b8
        bne     @bba7
        inc     $10b8
@bbd6:  lda     $1697
        and     #$01
        sta     $1697
        jsr     _c090ad
        jsr     WaitVBlank
        jsr     _c091ed
        jsr     WaitVBlank
        jsr     _c094a8
        jsr     WaitVBlank
        inc     $a5
        jsr     WaitVBlank
        lda     $1697
        bne     @bc3e
        jsr     CalcInnPrice
        lda     $0947
        sec
        sbc     $37
        sta     $08
        lda     $0948
        sbc     $38
        sta     $09
        lda     $0949
        sbc     $38
        sta     $0a
        bcs     @bc1f
        ldx     #$000f      ; dialog message $000f "please come again!"
        stx     $af
        jsr     ShowDlg
        bra     @bc3e
@bc1f:  lda     $08
        sta     $0947
        lda     $09
        sta     $0948
        lda     $0a
        sta     $0949
        ldx     #$000e      ; dialog message $000e "please rest well..."
        stx     $af
        jsr     ShowDlg
        lda     #$02
        jsr     AddEventPtr
        jmp     NextEventCmd
@bc3e:  jmp     TerminateEvent

; ---------------------------------------------------------------------------

; inn prices
_c0bc41:
        .word   10, 20, 30, 40, 50, 60, 70, 80

; ---------------------------------------------------------------------------

; [ get inn price ]

CalcInnPrice:
@bc51:  lda     $df
        asl
        tax
        lda     f:_c0bc41,x
        sta     $37
        lda     f:_c0bc41+1,x
        sta     $38
        stz     $39
        rts

; ---------------------------------------------------------------------------

; [ event command $ac:  ]

EventCmd_ac:
@bc64:  lda     $df
        jsr     _c0c9a5       ; give magic
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $c0:  ]

EventCmd_c0:
@bc6c:  lda     $df
        and     #$7f
        sta     $169a
        bne     @bc8a
        lda     #$bf
        sta     hW12SEL
        lda     #$0b
        sta     hW34SEL
        lda     #$01
        sta     hWBGLOG
        jsr     _c08c7b
        jmp     IncEventPtr2
@bc8a:  lda     $df
        bpl     @bc99
        lda     #$ff
        sta     hW12SEL
        sta     hW34SEL
        sta     hWBGLOG
@bc99:  jsr     _c05d30
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0bc9f:
        lda     $3f
        tax
        lda     f:RNGTbl,x
        cmp     #$10
        bcs     @bcb3
        lda     #$1a        ; system sound effect $1a
        sta     $1d00
        jsl     ExecSound_ext
@bcb3:  rts

; ---------------------------------------------------------------------------

; [ event command $d0: spc command ]

EventCmd_d0:
@bcb4:  lda     $df
        sta     $1d00
        lda     $e0
        sta     $1d01
        lda     #$01
        sta     $5c
        jsl     ExecSound_ext
        jsr     WaitVBlank
        stz     $5c
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $70-$75: wait ]

EventCmd_70:
EventCmd_71:
EventCmd_72:
EventCmd_73:
EventCmd_74:
EventCmd_75:
@bcce:  lda     $de
        sec
        sbc     #$70
        tax
        lda     f:_c0bcef,x   ; wait duration
        tax
@bcd9:  phx
        ldy     #$000f
@bcdd:  phy
        jsr     _c0c6de
        jsr     WaitVBlank
        ply
        dey
        bne     @bcdd
        plx
        dex
        bne     @bcd9
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; wait durations
_c0bcef:
        .byte   1,2,3,4,6,8

; ---------------------------------------------------------------------------

; [ event command $dc: tutorial menu ]

; 0-4: job, 5: row, 6: order

EventCmd_dc:
@bcf5:  lda     #$04        ; menu command $04 (tutorial)
        sta     $0134
        lda     $df
        sta     $0135       ; tutorial index
        jsr     OpenMenu
        inc     $55
        jsr     _c0577c       ;
        jsr     _c06100       ; fade in
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $7a: name change menu ]

EventCmd_7a:
@bd0d:  lda     #$05        ; menu command $05 (name change)
        sta     $0134
        jsr     OpenMenu
        jsr     _c0577c       ;
        jsr     _c06100       ; fade in
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $a1: shop ]

EventCmd_a1:
@bd1e:  lda     $df
        sta     $0135
        lda     #$02        ; menu command $02 (shop)
        sta     $0134
        jsr     OpenMenu
        inc     $55
        jsr     _c0577c
        jsr     _c06100       ; fade in
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $e8: character stats battle (ending) ]

EventCmd_e8:
@bd36:  ldx     $06
        stx     $16a8
        lda     $df
        and     #$7f
        longa
        asl2
        tax
        lda     $06
        shorta
        jsr     _c0cc52
        cmp     #$c0
        bcc     @bd51
        inx2
@bd51:  longa
        lda     f:EventBattleGrp,x
        sta     $04f0       ; battle index
        lda     $06
        shorta
        lda     $e0
        cmp     #$ff
        bne     @bd67
        lda     $1124       ; map default battle background
@bd67:  sta     $04f2
        inc     $55
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #$00
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     ExecBattle_ext
        jsr     _c044e3       ; init map bank
        lda     #$81
        sta     hNMITIMEN
        cli
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $bd: event battle ]

; b1: battle index (msb = manual fade in)
; b2: battle background ($ff = use map default)

EventCmd_bd:
@bd8d:  lda     $df
        and     #$7f
        jsr     _c0bde6       ; event battle
        lda     $09c4       ; battle return code
        and     #$01
        beq     @bda3       ; branch if not defeated
        lda     #$f0
        sta     $1d00       ; don't reset spc
        jmp     Start
@bda3:  jsr     _c0577c
        lda     #$f0
        sta     $43
        stz     $45
        lda     $df
        bmi     @bdb3
        jsr     _c06100       ; fade in
@bdb3:  lda     #$81
        sta     hNMITIMEN
        cli
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $e2: event battle (can lose) ]

EventCmd_e2:
@bdbc:  lda     $df
        and     #$7f
        jsr     _c0bde6       ; event battle
        jsr     _c0577c
        lda     #$f0
        sta     $43
        stz     $45
        lda     $df
        bmi     @bdd3
        jsr     _c06100       ; fade in
@bdd3:  lda     #$81
        sta     hNMITIMEN
        cli
        lda     $09c4
        and     #$01
        jne     IncEventPtr3
        jmp     IncEventPtr7

; ---------------------------------------------------------------------------

; [ event battle ]

; a: battle index

_c0bde6:
@bde6:  ldx     $06
        stx     $16a8
        longa
        asl2
        tax
        lda     $06
        shorta
        jsr     _c0cc52
        cmp     #$c0
        bcc     @bdfd
        inx2
@bdfd:  longa
        lda     f:EventBattleGrp,x   ; event battle group
        sta     $04f0
        lda     $06
        shorta
        lda     $e0
        cmp     #$ff
        bne     @be13
        lda     $1124
@be13:  sta     $04f2
        inc     $55
        lda     #$6f
        jsr     PlaySfx
        jsr     BattleBlur
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #$00
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     ExecBattle_ext
        jsr     _c044e3       ; init map bank
        lda     $09c4
        and     #$01
        bne     @be61
        lda     $013b
        ora     $013c
        ora     $013d
        ora     $013e
        ora     $013f
        ora     $0140
        ora     $0141
        ora     $0142
        beq     @be61
        lda     #$01        ; menu command $01 (collect items after battle)
        sta     $0134
        jsr     OpenMenuNoFade
@be61:  rts

; ---------------------------------------------------------------------------

; [ event command $b7: add/remove character ]

EventCmd_b7:
@be62:  lda     $df
        bmi     @beac

; add character
        and     #$07
        pha
        cmp     #$04        ; cara
        bne     @be7e
        lda     #$02        ; galuf
        jsr     _c0c164       ; get pointer to character data
        lda     $0500,y
        and     #$80
        ora     $df
        sta     $0500,y
        bra     @be89
@be7e:  jsr     _c0c164       ; get pointer to character data
        lda     $0500,y
        and     #$bf
        sta     $0500,y
@be89:  pla
        asl
        sta     $08
        asl
        clc
        adc     $08
        tay
        clc
        adc     $08
        tax
        beq     @bea9
        lda     #$06
        sta     $09
@be9c:  lda     f:_c0bec1,x
        sta     $0990,y     ; character name
        inx
        iny
        dec     $09
        bne     @be9c
@bea9:  jmp     IncEventPtr2

; remove character
@beac:  and     #$07
        jsr     _c0c164       ; get pointer to character data
        cpy     #$0140
        beq     @bebe
        lda     $0500,y
        ora     #$40        ; character is not present
        sta     $0500,y
@bebe:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; character names (8 bytes each, 6 bytes useable)
_c0bec1:
        .byte   $c7,$c7,$ff,$ff,$ff,$ff,$ff,$ff      ; ……
        .byte   $ac,$92,$ff,$ff,$ff,$ff,$ff,$ff      ; レナ
        .byte   $2a,$a6,$64,$ff,$ff,$ff,$ff,$ff      ; ガラフ
        .byte   $64,$c4,$a8,$78,$ff,$ff,$ff,$ff      ; ファリス
        .byte   $6e,$aa,$aa,$ff,$ff,$ff,$ff,$ff      ; クルル

; ---------------------------------------------------------------------------

; [ event command $c6: give job ]

; b1: job index

EventCmd_c6:
@bee9:  lda     $df
        longa
        tax
        lsr3
        tay
        lda     $06
        shorta
        txa
        and     #$07
        tax
        lda     $0840,y
        ora     f:BitOrTbl,x
        sta     $0840,y
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $af: give gp ]

EventCmd_af:
@bf07:  jsr     _c0bf3f       ; calculate gp (event)
        jsr     GiveGil
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $b0: take gp ]

EventCmd_b0:
@bf10:  jsr     _c0bf3f       ; calculate gp (event)
        lda     $0947       ; current gp
        sec
        sbc     $0b37
        sta     $0947
        lda     $0948
        sbc     $0b38
        sta     $0948
        lda     $0949
        sbc     $0b39
        sta     $0949
        cmp     #$9a
        bcc     @bf3c
        stz     $0947
        stz     $0948
        stz     $0949
@bf3c:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ calculate gp (event) ]

_c0bf3f:
@bf3f:  lda     $df
        bmi     @bf58
        and     #$7f        ; add gp
        longa
        asl
        sta     $37         ; multiply by 10
        asl2
        clc
        adc     $37
        sta     $37
        lda     $06
        shorta
        stz     $39
        rts
@bf58:  and     #$7f        ; subtract gp
        longa
        pha
        asl3
        sta     $0d
        asl
        clc
        adc     $0d
        sta     $0d
        pla
        xba
        sta     $37
        lda     $06
        shorta
        stz     $39
        asl     $37
        rol     $38
        rol     $39
        asl     $37
        rol     $38
        rol     $39
        lda     $37
        sec
        sbc     $0d
        sta     $37
        lda     $38
        sbc     $0e
        sta     $38
        lda     $39
        sbc     #$00
        sta     $39
        rts

; ---------------------------------------------------------------------------

; [ event command $aa: add item to inventory ]

EventCmd_aa:
@bf92:  lda     $df
        jsr     _c0bfdd       ; find item slot
        cpy     #$0100
        beq     @bfa9       ; branch if not found
        lda     $0740,y
        cmp     #$63
        beq     @bfbd       ; branch if 99
        inc
        sta     $0740,y     ; increment item quantity
        bra     @bfbd
@bfa9:  ldy     $06
@bfab:  lda     $0640,y     ; find the first empty slot
        beq     @bfb3
        iny
        bra     @bfab
@bfb3:  lda     $df
        sta     $0640,y     ; store item id
        lda     #$01
        sta     $0740,y     ; quantity 1
@bfbd:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $ab: remove item from inventory ]

EventCmd_ab:
@bfc0:  lda     $df
        jsr     _c0bfdd       ; find item slot
        cpy     #$0100
        beq     @bfda
        lda     $0740,y
        beq     @bfda
        dec
        sta     $0740,y
        bne     @bfda
        lda     #$00
        sta     $0640,y
@bfda:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ find item slot ]

_c0bfdd:
@bfdd:  sta     $16a2
        ldy     $06
@bfe2:  lda     $0640,y     ; item id
        cmp     $16a2
        beq     @bff0
        iny
        cpy     #$0100
        bne     @bfe2
@bff0:  rts

; ---------------------------------------------------------------------------

; [ event command $ba: remove status ]

EventCmd_ba:
@bff1:  lda     $df
        and     #$07
        jsr     _c0c164       ; get pointer to character data
        cpy     #$0140
        beq     @c00c
        lda     $0500,y
        and     #$40
        bne     @c00c
        lda     $051a,y
        and     $e0
        sta     $051a,y
@c00c:  jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $bb: set status ]

EventCmd_bb:
@c00f:  lda     $df
        and     #$07
        jsr     _c0c164       ; get pointer to character data
        cpy     #$0140
        beq     @c02a
        lda     $0500,y
        and     #$40
        bne     @c02a
        lda     $051a,y
        ora     $e0
        sta     $051a,y
@c02a:  jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $bc:  ]

EventCmd_bc:
@c02d:  lda     $df
        and     #$07
        jsr     _c0c164       ; get pointer to character data
        cpy     #$0140
        beq     @c048
        lda     $0500,y
        and     #$40
        bne     @c048
        lda     $051a,y
        eor     $e0
        sta     $051a,y
@c048:  jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $a8: modify hp ]

; b1: ccsvvvvv (c: character, s: subtract, v: hp value, #$1f = restore to max)

EventCmd_a8:
@c04b:  lda     $df
        and     #$c0
        lsr6
        jsr     _c0c164       ; get pointer to character data
        cpy     #$0140
        beq     @c0cd
        lda     $0500,y
        and     #$40
        bne     @c0cd
        lda     $df
        and     #$20
        bne     @c09d
        lda     $df
        and     #$1f
        cmp     #$1f
        bne     @c080
        longa
        lda     $0508,y
        sta     $0506,y
        lda     $06
        shorta
        bra     @c0cd
@c080:  asl
        tax
        longa
        lda     $0506,y
        clc
        adc     f:_c0c0d0,x
        cmp     $0508,y
        bcc     @c094
        lda     $0508,y
@c094:  sta     $0506,y
        lda     $06
        shorta
        bra     @c0cd
@c09d:  lda     $df
        and     #$1f
        cmp     #$1f
        bne     @c0b0
        longa
        lda     $06
        sta     $0506,y
        shorta
        bra     @c0cd
@c0b0:  asl
        tax
        longa
        lda     $0506,y
        sec
        sbc     f:_c0c0d0,x
        beq     @c0c3
        cmp     #$270f
        bcc     @c0c6
@c0c3:  lda     #$0001
@c0c6:  sta     $0506,y
        lda     $06
        shorta
@c0cd:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; hp constants
_c0c0d0:
        .word   10, 20, 50, 100, 200, 500, 1000, 2000

; ---------------------------------------------------------------------------

; [ event command $a9: modify mp ]

EventCmd_a9:
@c0e0:  lda     $df
        and     #$c0
        lsr6
        jsr     _c0c164       ; get pointer to character data
        cpy     #$0140
        beq     @c153
        lda     $0500,y
        and     #$40
        bne     @c153
        lda     $df
        and     #$20
        bne     @c132
        lda     $df
        and     #$1f
        cmp     #$1f
        bne     @c115
        longa
        lda     $050c,y
        sta     $050a,y
        lda     $06
        shorta
        bra     @c153
@c115:  asl
        tax
        longa
        lda     $050a,y
        clc
        adc     f:_c0c156,x
        cmp     $050c,y
        bcc     @c129
        lda     $050c,y
@c129:  sta     $050a,y
        lda     $06
        shorta
        bra     @c153
@c132:  lda     $df
        and     #$1f
        asl
        tax
        longa
        lda     $050a,y
        sec
        sbc     f:_c0c156,x
        beq     @c149
        cmp     #$270f
        bcc     @c14c
@c149:  lda     #$0001
@c14c:  sta     $050a,y
        lda     $06
        shorta
@c153:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; mp constants
_c0c156:
        .word   10, 20, 50, 100, 200, 500, 1000

; ---------------------------------------------------------------------------

; [ get pointer to character data ]

_c0c164:
@c164:  sta     $09
        cmp     #$02
        beq     @c183       ; branch if galuf
        cmp     #$04
        beq     @c183       ; branch if krile
        ldy     $06
@c170:  lda     $0500,y
        and     #$07
        cmp     $09
        beq     @c19c
        jsr     _c0c19d
        cpy     #$0140
        beq     @c19c
        bra     @c170
@c183:  ldy     $06
@c185:  lda     $0500,y
        and     #$07
        cmp     #$02
        beq     @c19c
        cmp     #$04
        beq     @c19c
        jsr     _c0c19d
        cpy     #$0140
        beq     @c19c
        bra     @c185
@c19c:  rts

; ---------------------------------------------------------------------------

; [  ]

_c0c19d:
@c19d:  longa
        tya
        clc
        adc     #$0050
        tay
        lda     $06
        shorta
        rts

; ---------------------------------------------------------------------------

; [ event command $c2: hide vehicle ]

; b1: vehicle index

EventCmd_c2:
@c1aa:  lda     $df
        dec
        asl2
        tay
        lda     $0ade,y     ; hide vehicle
        ora     #$80
        sta     $0ade,y
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $d2: show vehicle ]

; b1: map
; b2: x position
; b3: y position
; b4: vvvaaagg
;       v: vehicle index
;       a:
;       g: graphics

EventCmd_d2:
@c1bb:  lda     $e2
        and     #$e0        ; vehicle index
        lsr3
        sec
        sbc     #$04
        tay
        lda     $df         ; map
        asl5
        sta     $08
        lda     $e2
        and     #$1f
        ora     $08
        sta     $0add,y
        lda     $e0
        sta     $0adf,y     ; x position
        lda     $e1
        sta     $0ae0,y     ; y position
        lda     $0ade,y     ; show vehicle
        and     #$7f
        sta     $0ade,y
        jmp     IncEventPtr5

; ---------------------------------------------------------------------------

; [ event command $c9: resume suspended song ]

EventCmd_c9:
@c1ed:  lda     $df
        sta     $1d02
        lda     $e0
        sta     $1d03
        lda     $1d09       ; suspended song id
        sta     $1d01
        lda     #$01
        sta     $1d00
        lda     #$01
        sta     $5c
        jsl     ExecSound_ext
        jsr     WaitVBlank
        stz     $5c
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $b4: play song ]

EventCmd_b4:
@c212:  lda     $df
        sta     $1d01
        lda     #$01
        sta     $1d00
        lda     #$08        ; relative volume 100%
        sta     $1d02
        lda     #$0f        ; absolute volume full
        sta     $1d03
        lda     #$01
        sta     $5c
        jsl     ExecSound_ext
        jsr     WaitVBlank
        stz     $5c
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $d4: play song (specify volume) ]

EventCmd_d4:
        lda     $e0
        sta     $1d02
        lda     $e1
        sta     $1d03
        lda     $df
        sta     $1d01
        lda     #$01
        sta     $1d00
        lda     #$01
        sta     $5c
        jsl     ExecSound_ext
        jsr     WaitVBlank
        stz     $5c
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [ event command $b5: play sound effect ]

EventCmd_b5:
@c25a:  lda     $df
        jsr     PlaySfx
        jsl     ExecSound_ext
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $d5: play sound effect (specify volume) ]

EventCmd_d5:
@c266:  lda     $df
        jsr     PlaySfx
        lda     $e0
        sta     $1d02
        lda     $e1
        sta     $1d03
        jsl     ExecSound_ext
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [ event command $76: set scroll speed to normal ]

EventCmd_76:
@c27c:  stz     $0afa
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $78: set scroll speed to slow ]

EventCmd_78:
@c282:  lda     #$ff
        sta     $0afa
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $77: set scroll speed to fast ]

EventCmd_77:
@c28a:  lda     #$01
        sta     $0afa
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $f1: jump forward randomly ]

; b1: bitmask with number of options (4 bytes each)

EventCmd_f1:
@c292:  jsr     Rand
        and     $df
        longa
        asl2
        inc2
        clc
        adc     $d6
        sta     $d6
        lda     $06
        adc     $d8
        sta     $d8
        lda     $06
        shorta
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [ event command $f0: show dialog (yes/no) ]

EventCmd_f0:
@c2af:  longa
        lda     $df
        sta     $af
        lda     $06
        shorta
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcc     @c2cd
        jsr     _c04c95       ; clear sprite data
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
@c2cd:  jsr     GetDlgPtr
@c2d0:  jsr     _c08f54
        ldx     $b1
        lda     f:Dlg,x   ; dialog
        beq     @c300       ; branch if end of string
        jsr     LoadDlgText
        ldy     $06
        sty     $ab
@c2e2:  jsr     _c08d3b
        jsr     WaitVBlank
        ldy     $ab
        cpy     #$0040
        bne     @c2e2
        lda     $b3
        bne     @c300
        jsr     _c04aad
        jsr     _c04ac1       ; wait for keypress
        inc     $a5
        jsr     WaitVBlank
        bra     @c2d0
@c300:  jsr     _c08fed       ; show yes/no window
        lda     #$80
        sta     $1697
@c308:  jsr     WaitVBlank
        lda     $03
        and     #>(JOY_UP | JOY_DOWN)
        beq     @c31e
        cmp     #$08
        bne     @c319
        lda     #$80
        bra     @c31b
@c319:  lda     #$81
@c31b:  sta     $1697
@c31e:  lda     $03
        bpl     @c329
        lda     #$01
        sta     $1697
        bra     @c33f
@c329:  lda     $02
        and     #JOY_A
        beq     @c308
        lda     $10b8
        bne     @c308
        inc     $10b8
        lda     $1697
        and     #$01
        sta     $1697
@c33f:  jsr     _c090ad
        jsr     WaitVBlank
        jsr     _c094a8
        jsr     WaitVBlank
        inc     $a5
        jsr     WaitVBlank
        lda     #$03
        jsr     AddEventPtr
        lda     $1697
        beq     @c35f
        lda     #$04
        jsr     AddEventPtr
@c35f:  jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [ event command $d6: change vehicle ]

; +b1: --ammmmm mmmmmmmm
;        m: map id
;        a: something with height ???
;  b3: vvvaaabb
;        v: vehicle id
;        a: vehicle graphic
;        g: vehicle frame ???

EventCmd_d6:
@c362:  jsr     _c04ce8       ; disable interrupts
        stz     $169e
        lda     $0ad8
        sta     $1088
        lda     $0ad9
        sta     $1089
        longa
        lda     $df
        and     #$1fff
        sta     $0ad4
        sta     $0ad6       ; map index
        lda     $06
        shorta
        lda     $e1
        lsr5
        sta     $0adc       ; current vehicle
        beq     @c3bf       ; branch if no vehicle
        dec
        asl2
        tay
        lda     $e1
        and     #$1f
        sta     $0add,y
        lda     #$80
        sta     $0ade,y
        lda     $e0
        and     #$20
        beq     @c3bf
        lda     $0ade,y
        ora     #$0f
        sta     $0ade,y
        lda     $0add,y
        and     #$1c
        cmp     #$18
        bne     @c3bf
        lda     $0ade,y
        asl
        sta     $0ade,y
@c3bf:  jsr     LoadWorldMap
        lda     #$81
        sta     hNMITIMEN
        cli
        jsr     WaitVBlank
        lda     #$04
        jsr     AddEventPtr
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [ event command $e0: load map (fade out/in) ]

; +b0: ?pt---mm mmmmmmmm
;      p: set parent map
;      t: show map title
;      m: map index
;  b1: ffxxxxxx
;      f: facing direction (normal map only)
;      x: x position
;  b2: y position
;  b3: vvvhhhhh
;      v: vehicle
;      h: vehicle height ???

EventCmd_e0:
@c3d3:  jsr     _c06081       ; fade out
        jsr     _c0c43f
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @c3e6
        jsr     LoadWorldMap
        bra     @c3e9
@c3e6:  jsr     LoadSubMap
@c3e9:  jsr     _c06100       ; fade in
        jsr     _c09267       ; show map title
        lda     #$06
        jsr     AddEventPtr
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [ event command $e1: load map (no fade) ]

EventCmd_e1:
@c3f7:  jsr     _c0c43f
        ldx     $0ad6       ; map index
        cpx     #$0005
@c400:  bcs     @c407
        jsr     LoadWorldMap
        bra     @c40a
@c407:  jsr     LoadSubMap
@c40a:  jsr     _c04cfa       ; enable interrupts
        jsr     WaitVBlank
        lda     #$06
        jsr     AddEventPtr
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [ event command $e3: load map (fade out only) ]

EventCmd_e3:
@c418:  jsr     _c06081       ; fade out
        jsr     _c0c43f
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @c42b
        jsr     LoadWorldMap
        bra     @c42e
@c42b:  jsr     LoadSubMap
@c42e:  lda     #$81
        sta     hNMITIMEN
        cli
        jsr     WaitVBlank
        lda     #$06
        jsr     AddEventPtr
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [  ]

_c0c43f:
@c43f:  jsr     _c04ce8       ; disable interrupts
        stz     $16a0       ; disable map title
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @c45c
        lda     $e0
        and     #$40
        beq     @c45c
        stx     $0af5       ; set parent map
        ldx     $0ad8
        stx     $0af7       ; set parent xy position
@c45c:  stz     $169e
        lda     $e2         ; y position
        sta     $1089
        lda     $e3
        lsr5
        sta     $0adc       ; vehicle
        beq     @c49e
        dec
        asl2
        tay
        lda     $e3
        and     #$1f
        sta     $0add,y     ;
        lda     #$80
        sta     $0ade,y
        lda     $e0
        and     #$20
        beq     @c49e
        lda     $0ade,y
        ora     #$0f
        sta     $0ade,y
        lda     $0add,y
        and     #$1c
        cmp     #$18
        bne     @c49e
        lda     $0ade,y
        asl
        sta     $0ade,y
@c49e:  longa
        lda     $df
        and     #$03ff
        sta     $0ad4
        sta     $0ad6       ; map index
        tay
        lda     $06
        shorta
        cpy     #$0005
        bcs     @c4bd
        lda     $e1
        sta     $1088       ; x position (world map)
        jmp     @c4df
@c4bd:  lda     $e0
        bpl     @c4c4
        inc     $16a0       ; enable map title
@c4c4:  lda     $e1
        lsr6
        pha
        sta     $b9
        asl
        sta     $bb         ; party graphic action
        pla
        inc
        sta     $ba
        sta     $bf
        lda     $e1
        and     #$3f
        sta     $1088       ; x position (normal map)
@c4df:  rts

; ---------------------------------------------------------------------------

; [ event command $f4: change background (relative) ]

EventCmd_f4:
@c4e0:  lda     $df
        and     #$c0
        lsr2
        longa
        xba
        sta     $73
        lda     $06
        shorta
        lda     $df
        and     #$3f
        clc
        adc     $0ad8       ; party x position
        sta     $75
        lda     $e0
        clc
        adc     $0ad9       ; party y position
        sta     $76
        jmp     _c51d

; ---------------------------------------------------------------------------

; [ event command $f3: change background (absolute) ]

EventCmd_f3:
@c504:  lda     $df
        and     #$c0
        lsr2
        longa
        xba
        sta     $73
        lda     $06
        shorta
        lda     $df
        and     #$3f
        sta     $75
        lda     $e0
        sta     $76
_c51d:  lda     $e1
        and     #$0f
        inc
        sta     $2c
        sta     hWRMPYA
        lda     $e1
        lsr4
        inc
        sta     $2d
        sta     hWRMPYB
        nop4
        longa
        lda     hRDMPYL
        sta     $0d
        lda     $06
        shorta
        ldy     #$0004
        ldx     $06
@c547:  lda     [$d6],y
        sta     $16b3,x
        iny
        inx
        cpx     $0d
        bne     @c547
        longa
        lda     $0d
        clc
        adc     #$0004
        clc
        adc     $d6
        sta     $d6
        lda     $06
        adc     $d8
        sta     $d8
        lda     $06
        shorta
        jsr     _c06f08
        jmp     NextEventCmd

; ---------------------------------------------------------------------------

; [ event command $7c: disable timer ]

EventCmd_7c:
@c56f:  stz     $0afb       ; hide timer
        ldx     $06
        stx     $0afc       ; clear timer counter
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $d8: set object position (relative) ]

EventCmd_d8:
@c57a:  lda     $df
        and     #$1f
        jsr     _c0c5e4       ; remove current object from object layout
        lda     $df
        and     #$c0
        lsr5
        sta     $147c,y     ; graphic frame
        lda     $e0
        lsr4
        sta     $08
        and     #$08
        beq     @c59f
        lda     $08
        ora     #$f0
        sta     $08
@c59f:  lda     $08
        clc
        adc     $0ad8
        sta     $1478,y     ; x position
        lda     $e0
        and     #$0f
        sta     $08
        and     #$08
        beq     @c5b8
        lda     $08
        ora     #$f0
        sta     $08
@c5b8:  lda     $08
        clc
        adc     $0ad9
        sta     $147a,y     ; y position
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $d3: set object position (absolute) ]

; b1: object index
; b2: ddxxxxxx
;     d: facing direction
;     x: x position
; b3: y position

EventCmd_d3:
@c5c4:  lda     $df
        jsr     _c0c5e4       ; remove current object from object layout
        lda     $e0
        and     #$c0
        lsr5
        sta     $147c,y
        lda     $e0
        and     #$3f
        sta     $1478,y
        lda     $e1
        sta     $147a,y
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [ remove current object from object layout ]

_c0c5e4:
@c5e4:  and     #$7f
        sta     $e5         ; current object
        sta     hWRMPYA
        lda     #$14
        sta     hWRMPYB
        nop4
        ldy     hRDMPYL
        jsr     _c03cf8       ; remove object from object layout
        rts

; ---------------------------------------------------------------------------

; [ event command $d1: start timer ]

;  b1: timer duration (seconds)
; +b2: event

EventCmd_d1:
@c5fb:  lda     $df
        sta     hWRMPYA
        lda     #$3c
        sta     hWRMPYB
        nop4
        longa
        lda     hRDMPYL
        inc
        sta     $0afc       ; timer counter
        lda     $e0
        sta     $0afe       ; timer event
        lda     $06
        shorta
        lda     #$01        ; enable timer
        sta     $0afb
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [ event command $d7: start timer (speed up music) ]

;  b1: timer duration (seconds / 4)
; +b2: event

EventCmd_d7:
@c623:  lda     $df
        sta     hWRMPYA
        lda     #$f0
        sta     hWRMPYB
        nop4
        longa
        lda     hRDMPYL
        inc
        sta     $0afc
        lda     $e0
        sta     $0afe
        lda     $06
        shorta
        lda     #$02
        sta     $0afb
        jmp     IncEventPtr4

; ---------------------------------------------------------------------------

; [ event command $7d:  ]

EventCmd_7d:
@c64b:  stz     $46
        stz     $44
        lda     $48
        sta     $47
        lda     $4a
        sta     $49
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $c5: flash screen ]

; b1: bgr----- (color components)

EventCmd_c5:
@c65a:  lda     $46
        bne     @c668
        lda     $47
        sta     $48
        lda     $49
        sta     $4a
        stz     $49
@c668:  lda     #$07
        sta     $47         ; addition subtraction -> $2131 (hCGADSUB)
        lda     $df
        and     #$e0
        sta     $4c
        lda     #$f8
        sta     $46
        lda     #$08
        sta     $44
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $b8: color addition ]

; if color math is already active, reverse the rate using the same parameters

; b1: bgrssiii
;     b: affect blue
;     g: affect green
;     r: affect red
;     s: speed
;     i: intensity

EventCmd_b8:
@c67d:  lda     $46
        bne     @c689       ; if already active, reverse rate
        lda     $df
        jsr     _c049d7       ; init color addition
        jmp     IncEventPtr2
@c689:  lda     $44
        and     #$7f
        sta     $44
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $b9: color subtraction ]

; same parameters as color subtraction

EventCmd_b9:
@c692:  lda     $46
        bne     @c69e       ; if already active, reverse rate
        lda     $df
        jsr     _c049e9       ; init color subtraction
        jmp     IncEventPtr2
@c69e:  lda     $44
        and     #$7f
        sta     $44
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $7e: stop fade ]

EventCmd_7e:
@c6a7:  stz     $43
        jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ event command $c3: fade in ]

EventCmd_c3:
@c6ac:  lda     $df         ; fade speed
        sta     $43
        lda     #$10        ; screen brightness
        sta     $45
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $c4: fade out ]

EventCmd_c4:
@c6b7:  lda     $df         ;  fade speed
        ora     #$80
        sta     $43
        lda     #$f0        ; screen brightness
        sta     $45
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $be: shake screen ]

EventCmd_be:
@c6c4:  lda     $df
        sta     $d3
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $c1:  ]

EventCmd_c1:
@c6cb:  lda     $df
        sta     $a4
        inc     $a2
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $b1: set party sprite graphic ]

EventCmd_b1:
@c6d4:  lda     $df
        sta     $0ada
        inc     $a1
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [  ]

_c0c6de:
@c6de:  jsr     _c04c95       ; clear sprite data
        lda     $53
        bne     @c6f8
        jsr     _c01a1d
        jsr     _c02137
        jsr     _c01ec5
        jsr     _c01e64
        jsr     _c0420a
        jsr     _c0612b
        rts
@c6f8:  jsr     _c01ae4
        jsr     _c03bac
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        jsr     _c0420a
        rts

; ---------------------------------------------------------------------------

; [ event command $b2: wait xx frames ]

EventCmd_b2:
@c70b:  lda     $df
        tax
@c70e:  phx
        jsr     _c0c6de
        jsr     WaitVBlank
        plx
        dex
        bne     @c70e
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $b3: wait xx * 15 frames ]

EventCmd_b3:
@c71c:  lda     $df
        tax
@c71f:  phx
        ldy     #$000f
@c723:  phy
        jsr     _c0c6de
        jsr     WaitVBlank
        ply
        dey
        bne     @c723
        plx
        dex
        bne     @c71f
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a0: show npc dialog ]

; b1: npc dialog index

EventCmd_a0:
@c735:  lda     $df
        asl
        tax
        ldy     $13d6,x
        sty     $af
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcc     @c752
        jsr     _c04c95       ; clear sprite data
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
@c752:  jsr     ShowDlg
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a2: set event flag ]

; b1: event flag (0-255)

EventCmd_a2:
@c758:  lda     $df
        jsr     _c0ca49       ; get flag index
        lda     $0a14,y     ; event flags
        ora     f:BitOrTbl,x
        sta     $0a14,y
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a4: set event flag ]

; b1: event flag (256-511)

EventCmd_a4:
@c76a:  lda     $df
        jsr     _c0ca49       ; get flag index
        lda     $0a34,y
        ora     f:BitOrTbl,x
        sta     $0a34,y
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a3: clear event flag ]

; b1: event flag (0-255)

EventCmd_a3:
@c77c:  lda     $df
        jsr     _c0ca49       ; get flag index
        lda     $0a14,y
        and     f:BitAndTbl,x
        sta     $0a14,y
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $a5: clear event flag ]

; b1: event flag (256-511)

EventCmd_a5:
@c78e:  lda     $df
        jsr     _c0c796
        jmp     IncEventPtr2

; ---------------------------------------------------------------------------

_c0c796:
@c796:  jsr     _c0ca49       ; get flag index
        lda     $0a34,y
        and     f:BitAndTbl,x
        sta     $0a34,y
        rts

; ---------------------------------------------------------------------------

; [ event command $c8: show dialog ]

; +b1: dialog index

EventCmd_c8:
@c7a4:  longa
        lda     $df
        sta     $af         ; dialog index
        lda     $06
        shorta
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcc     @c7c2       ; branch if a world map
        jsr     _c04c95       ; clear sprite data
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
@c7c2:  jsr     ShowDlg
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ init object movement ]

_c0c7c8:
        jsr     _c0c5e4       ; remove current object from object layout
        lda     #$01
        jsr     AddEventPtr
        phy
        ldy     $06
        lda     [$d6],y     ; action
        ply
        cmp     #$05
        bcs     _c0c7f9       ; branch if not a move action
        sta     $147b,y     ; set facing direction
        lda     $1485,y
        and     #$08
        beq     @c7ee
        lda     $147b,y     ; facing direction
        beq     @c7ee
        dec
        asl
        sta     $147c,y     ; graphic frame
@c7ee:  lda     #$01
        sta     $1486,y
        lda     #$00
        sta     $147d,y     ; movement animation counter
        rts

; ---------------------------------------------------------------------------

; event command $80-$9f: object action (b0: object, b1: action)
_c0c7f9:
@c7f9:  lda     $de
        sec
        sbc     #$80
        sta     $e5         ; object
        sta     hWRMPYA
        lda     #$14
        sta     hWRMPYB
        nop4
        ldy     hRDMPYL       ; pointer to object data
        lda     $df

; object action $05: jump 1 tile
        cmp     #$05
        bne     @c825
        lda     #$40
        sta     $1488,y
        lda     $1485,y
        and     #$fb
        sta     $1485,y
        jmp     @c92f

; object action $06: jump 2 tiles
@c825:  cmp     #$06
        bne     @c83b
        lda     #$80
        sta     $1488,y
        lda     $1485,y
        and     #$fb
        ora     #$04
        sta     $1485,y
        jmp     @c92f

; object action $07: toggle sprite priority (top half)
@c83b:  cmp     #$07
        bne     @c84a
        lda     $1482,y
        eor     #$10
        sta     $1482,y
        jmp     @c92f

; object action $08: toggle sprite priority (bottom half)
@c84a:  cmp     #$08
        bne     @c859
        lda     $1484,y
        eor     #$10
        sta     $1484,y
        jmp     @c92f

; object action $09: show object
@c859:  cmp     #$09
        bne     @c86a
        lda     $1485,y
        and     #$fe
        ora     #$01
        sta     $1485,y
        jmp     @c92f

; object action $0a: hide object
@c86a:  cmp     #$0a
        bne     @c886
        lda     $1485,y
        and     #$fe
        sta     $1485,y
        jsr     _c03cf8       ; remove object from object layout
        jmp     @c92f

; unused ???
@c87c:
        .byte   $00,$00,$01,$00,$ff,$00,$ff,$00,$01,$00

; object action $0b: toggle walking animation
@c886:  cmp     #$0b
        bne     @c89a
        lda     $1485,y
        eor     #$08
        sta     $1485,y
        lda     #$00
        sta     $147d,y
        jmp     @c92f

; object action $0c
@c89a:  cmp     #$0c
        bne     @c8ae
        lda     $147e,y
        eor     #$04
        sta     $147e,y
        lda     #$00
        sta     $147d,y
        jmp     @c92f

; object action $0d
@c8ae:  cmp     #$0d
        bne     @c8bc
        lda     $1485,y
        and     #$3f
        sta     $1485,y
        bra     @c92f

; object action $0e
@c8bc:  cmp     #$0e
        bne     @c8cc
        lda     $1485,y
        and     #$3f
        ora     #$80
        sta     $1485,y
        bra     @c92f

; object action $0f
@c8cc:  cmp     #$0f
        bne     @c8dc
        lda     $1485,y
        and     #$3f
        ora     #$c0
        sta     $1485,y
        bra     @c92f

; object action $10-$13: set animation speed
@c8dc:  cmp     #$14
        bcs     @c8f1
        sec
        sbc     #$10
        sta     $08
        lda     $147e,y
        and     #$fc
        ora     $08
        sta     $147e,y
        bra     @c92f

; object action $14: toggle layer priority (top half)
@c8f1:  cmp     #$14
        bne     @c8ff
        lda     $1482,y
        eor     #$08
        sta     $1482,y
        bra     @c92f

; object action $15: toggle layer priority (bottom half)
@c8ff:  cmp     #$15
        bne     @c90d
        lda     $1484,y
        eor     #$08
        sta     $1484,y
        bra     @c92f

; object action $16
@c90d:  cmp     #$16
        bne     @c91b
        lda     $1487,y
        eor     #$01
        sta     $1487,y
        bra     @c92f

; object action $17
@c91b:  cmp     #$17
        bne     @c929
        lda     $1487,y
        eor     #$02
        sta     $1487,y
        bra     @c92f

; object action $20+: change graphic frame
@c929:  sec
        sbc     #$20
        sta     $147c,y
@c92f:  jmp     IncEventPtr2

; ---------------------------------------------------------------------------

; [ event command $05-$6f: party action ]

_c0c932:
@c932:  cmp     #$10
        bcs     @c96e

; $05: jump 1 tile
        cmp     #$05
        bne     @c940
        lda     #$20
        sta     $be
        bra     @c987

; $06: jump 2 tiles
@c940:  cmp     #$06
        bne     @c94a
        lda     #$c0
        sta     $be
        bra     @c987

; $09: show party sprite
@c94a:  cmp     #$09
        bne     @c954
        lda     #$01
        sta     $bd
        bra     @c987

; $0a: hide party sprite
@c954:  cmp     #$0a
        bne     @c95c
        stz     $bd
        bra     @c987

; $0b: enable party walking animation
@c95c:  cmp     #$0b
        bne     @c966
        lda     #$01
        sta     $bc
        bra     @c987

; $0c: disable party walking animation
@c966:  cmp     #$0c
        bne     @c987
        stz     $bc
        bra     @c987

; $10-$6f: graphic action
@c96e:  sec
        sbc     #$10
        sta     $bb
        cmp     #$08
        bcs     @c97b
        lsr
        sta     $0adb       ; facing direction
@c97b:  lda     $53
        bne     @c984       ; branch if a world map ???
        jsr     _c02137
        bra     @c987
@c984:  jsr     UpdatePlayerSprite
@c987:  jmp     IncEventPtr1

; ---------------------------------------------------------------------------

; [ init camera movement ]

_c0c98a:
        asl
        tax
        lda     f:_c0c99b,x
        sta     $3b
        lda     f:_c0c99b+1,x
        sta     f:$000b3c
        rts

_c0c99b:
        .word   $0000,$0800,$0100,$0400,$0200

; ---------------------------------------------------------------------------

; [ give spell ]

; a = spell index

_c0c9a5:
@c9a5:  pha
        lsr3
        tay
        pla
        and     #$07
        tax
        lda     $0950,y
        ora     f:_c0c9b9,x
        sta     $0950,y
        rts

; bit masks for spells
_c0c9b9:
        .byte   $80, $40, $20, $10, $08, $04, $02, $01

; ---------------------------------------------------------------------------

; [ get npc flag ]

_c0c9c1:
        phx
        jsr     _c0ca49       ; get flag index
        lda     $0a54,y
        and     f:BitOrTbl,x
        plx
        rts

; ---------------------------------------------------------------------------

; [ event command $ca: set npc flag ]

EventCmd_ca:
@c9ce:  longa
        lda     $df
        shorta
        jsr     _c0ca49       ; get flag index
        lda     $0a54,y
        ora     f:BitOrTbl,x
        sta     $0a54,y
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ event command $cb: clear npc flag ]

EventCmd_cb:
@c9e4:  longa
        lda     $df
        shorta
        jsr     _c0ca49       ; get flag index
        lda     $0a54,y
        and     f:BitAndTbl,x
        sta     $0a54,y
        jmp     IncEventPtr3

; ---------------------------------------------------------------------------

; [ set battle flag ]

_c0c9fa:
@c9fa:  jsr     _c0ca49       ; get flag index
        lda     $09b4,y
        ora     f:BitOrTbl,x
        sta     $09b4,y
        rts

; ---------------------------------------------------------------------------

; [ clear battle flag ]

_c0ca08:
@ca08:  jsr     _c0ca49       ; get flag index
        lda     $09b4,y
        and     f:BitAndTbl,x
        sta     $09b4,y
        rts

; ---------------------------------------------------------------------------

; [ get treasure flag ]

_c0ca16:
        jsr     _c0ca49       ; get flag index
        lda     $09d4,y
        and     f:BitOrTbl,x
        rts

; ---------------------------------------------------------------------------

; [ set treasure flag ]

_c0ca21:
        jsr     _c0ca49       ; get flag index
        lda     $09d4,y
        ora     f:BitOrTbl,x
        sta     $09d4,y
        rts

; ---------------------------------------------------------------------------

; [ get event flag $00xx ]

_c0ca2f:
        phx
        jsr     _c0ca49       ; get flag index
        lda     $0a14,y
        and     f:BitOrTbl,x
        plx
        rts

; ---------------------------------------------------------------------------

; [ get event flag $01xx ]

_c0ca3c:
        phx
        jsr     _c0ca49       ; get flag index
        lda     $0a34,y
        and     f:BitOrTbl,x
        plx
        rts

; ---------------------------------------------------------------------------

; [ get flag index ]

_c0ca49:
@ca49:  longa
        tax
        lsr3
        tay
        lda     $06
        shorta
        txa
        and     #$07
        tax
        rts

; ---------------------------------------------------------------------------


; flag masks
BitOrTbl:
        .byte   $01,$02,$04,$08,$10,$20,$40,$80

BitAndTbl:
        .byte   $fe,$fd,$fb,$f7,$ef,$df,$bf,$7f

; ---------------------------------------------------------------------------

; [ check random battles (normal map) ]

_c0ca69:
        lda     $5a
        bne     @ca7b
        lda     $56
        beq     @ca7b       ; branch if party didn't take a step
        lda     $c9
        tay
        lda     $1112,y
        and     #$80
        bne     @ca7c       ; branch if random battles are enabled
@ca7b:  rts
@ca7c:  stz     $56         ; reset step counter
        ldx     $16a5       ; increment random battle counter
        inx
        stx     $16a5
        lda     $1124       ; battle background
        sta     $04f2
        longa
        lda     $0ad6       ; map index
        and     #$0003
        tay
        lda     $0ad6
        lsr2
        tax
        lda     $06
        shorta
        lda     f:SubBattleRate,x
        cpy     #$0000
        beq     @caac
@caa7:  lsr2
        dey
        bne     @caa7
@caac:  and     #$03
        asl
        tax
        longa
        lda     $16a8       ; random battle counter
        clc
        adc     f:_c0cb09,x   ; add probability value
        bcc     @cabf
        lda     #$ff00      ; max $ff00
@cabf:  sta     $16a8
        lda     $06
        shorta
        jsr     _c0cc52       ; update random number for random battles
        cmp     $16a9
        bcs     @cb08       ; branch if no battle
        longa
        lda     $0ad4       ; map index
        asl
        tax
        lda     f:SubBattleGrp,x
        asl3
        tax
        lda     $06
        shorta
        jsr     _c0cc6d       ; update random number for battle group
        cmp     #$5a
        bcc     @caf6
        inx2
        cmp     #$b4
        bcc     @caf6
        inx2
        cmp     #$f0
        bcc     @caf6
        inx2
@caf6:  longa
        lda     f:RandBattleGrp,x
        sta     $04f0
        lda     $06
        shorta
        inc     $55         ; random battle flag
        inc     $16a4       ;
@cb08:  rts

; ---------------------------------------------------------------------------

; random battle probability values (normal, low, high, very high)
_c0cb09:
        .word   $00e0, $0040, $0180, $0200

; ---------------------------------------------------------------------------

; [ check random battles (world map) ]

_c0cb11:
        lda     $5a
        bne     @cb22
        lda     $0adc       ; current vehicle
        beq     @cb27
        cmp     #$06
        bne     @cb23
        lda     $6f
        beq     @cb27
@cb22:  rts
@cb23:  cmp     #$05
        bne     @cb2b
@cb27:  lda     $56         ; step counter
        bne     @cb2c
@cb2b:  rts
@cb2c:  stz     $56
        longa
        lda     $10c6       ; current bg1 tile
        asl
        clc
        adc     $10c6
        tax
        lda     $06
        shorta
        lda     $0ad6       ; map index
        and     #$01
        asl4
        sta     $08
        lda     $1188,x     ; tile properties byte 3
        and     #$0f
        ora     $08
        tax
        lda     f:_c0cc21,x   ; battle bg
        sta     $04f2
        txa
        and     #$0f
        tax
        lda     $0adc
        cmp     #$05
        bcc     @cb67
        lda     #$15        ; battle bg $15 (ship exterior)
        sta     $04f2
@cb67:  lda     f:_c0cc3a,x   ; probability offset
        sta     $11
        stz     $12
        lda     f:_c0cc42,x   ; battle group offset
        sta     $0f
        stz     $10
        lda     $0ad6       ; map index
        asl6
        sta     $0d
        lda     $0ad9
        lsr2
        and     #$38
        clc
        adc     $0d
        sta     $0d
        lda     $0ad8
        lsr5
        clc
        adc     $0d
        tax
        lda     f:WorldBattleRate,x
        ldy     $11
        beq     @cba7
@cba2:  lsr2
        dey
        bne     @cba2
@cba7:  and     #$03
        cmp     #$03
        beq     @cc20
        asl
        tax
        longa
        lda     $16a8
        clc
        adc     f:_c0cc4a,x
        bcc     @cbbe
        lda     #$ff00
@cbbe:  sta     $16a8
        lda     $06
        shorta
        jsr     _c0cc52
        cmp     $16a9
        bcs     @cc20
        lda     $0ad9
        and     #$e0
        longa
        asl
        sta     $0d
        lda     $0ad8
        lsr2
        and     #$0038
        clc
        adc     $0d
        sta     $23
        lda     $0ad6       ; map index
        xba
        asl
        clc
        adc     $23
        clc
        adc     $0f
        tax
        lda     f:WorldBattleGrp,x   ; world map battle groups
        asl3
        tax
        lda     $06
        shorta
        jsr     _c0cc6d
        cmp     #$5a
        bcc     @cc11
        inx2
        cmp     #$b4
        bcc     @cc11
        inx
        inx
        cmp     #$f0
        bcc     @cc11
        inx2
@cc11:  longa
        lda     f:RandBattleGrp,x
        sta     $04f0
        lda     $06
        shorta
        inc     $55
@cc20:  rts

; ---------------------------------------------------------------------------

; battle bg for world 1 and 3
_c0cc21:
        .byte   0,1,2,3,4,5,3,7,8,0,0,0,0,0,0,0

; battle bg for world 2
_c0cc31:
        .byte   33,1,2,3,4,5,3,7,8

; probability offset
_c0cc3a:
        .byte   3,2,1,0,3,2,1,0

; battle group offset
_c0cc42:
        .byte   0,2,4,6
        .byte   0,2,4,6

_c0cc4a:
        .word   $0100,$0010,$0180,$0000

; ---------------------------------------------------------------------------

; [ update random number for random battles ]

_c0cc52:
@cc52:  phx
        inc     $4f
        bne     @cc60
        lda     $0b60
        clc
        adc     #$11
        sta     $0b60
@cc60:  lda     $4f
        tax
        lda     f:RNGTbl,x
        clc
        adc     $0b60
        plx
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0cc6d:
@cc6d:  phx
        inc     $50
        bne     @cc7b
        lda     $0b5f
        clc
        adc     #$17
        sta     $0b5f
@cc7b:  lda     $50
        tax
        lda     f:RNGTbl,x
        clc
        adc     $0b5f
        plx
        rts

; ---------------------------------------------------------------------------

; [ battle blur (normal map) ]

.proc BattleBlurSub
        stz     $3f
Loop:   jsr     WaitVBlank
        lda     $3f
        cmp     #$10
        bcs     :+
        and     #%111
        bra     :++
:       and     #%1111
:       asl4
        ora     #$0f
        sta     hMOSAIC
        lda     $3f
        cmp     #$20
        bne     Loop
        rts
.endproc

; ---------------------------------------------------------------------------

; [ battle blur (world map) ]

.proc BattleBlurWorld
        stz     hMDMAEN
        stz     hHDMAEN
        stz     $3f
Loop:   jsr     WaitVBlank
        lda     $3f
        and     #$3f
        asl
        tax
        lda     f:_c0cd4a,x
        sta     hM7A
        lda     f:_c0cd4a+1,x
        sta     hM7A
        lda     f:_c0cd4a,x
        sta     hM7D
        lda     f:_c0cd4a+1,x
        sta     hM7D
        lda     $3f
        cmp     #$20
        bne     Loop
        rts
.endproc

; ---------------------------------------------------------------------------

; [ battle blur ]

.proc BattleBlur
        lda     #$01
        sta     $52
        lda     $53
        bne     @ccea
        jsr     BattleBlurWorld
        bra     @cced
@ccea:  jsr     BattleBlurSub
@cced:  stz     $52
        rts
.endproc

; ---------------------------------------------------------------------------

.proc RandomBattle

_ccf0:  ldx     $06
        stx     $16a8
        lda     #$6f
        jsr     PlaySfx
        jsr     BattleBlur
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #0
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     ExecBattle_ext
        jsr     _c044e3
        lda     $09c4
        and     #1
        beq     _cd24
        lda     #$f0
        sta     $1d00
        jmp     Start
_cd24:  lda     $013b
        ora     $013c
        ora     $013d
        ora     $013e
        ora     $013f
        ora     $0140
        ora     $0141
        ora     $0142
        beq     _cd46
        lda     #1
        sta     $0134
        jsr     OpenMenuNoFade
_cd46:  jsr     UpdateTopChar
        rts

.endproc

; ---------------------------------------------------------------------------

; zoom values for world map battle blur
_c0cd4a:
        .word   $02b0, $01d5, $0143, $00e2, $00a1, $0076, $0059, $0046
        .word   $0039, $0046, $0059, $0076, $00a1, $00e2, $0143, $01d5
        .word   $02b0, $01d5, $0143, $00e2, $00a1, $0076, $0059, $0046
        .word   $0039, $0031, $002b, $0027, $0025, $0023, $0022, $0021

; ===========================================================================

.segment "reset"

; ---------------------------------------------------------------------------

; [ reset ]

.proc Reset

_cec0:  fixed_block $20
        sei
        clc
        xce
        jml     Start
        end_fixed_block 0

.endproc

; ===========================================================================

.segment "nmi_irq"

        fixed_block $20

; ---------------------------------------------------------------------------

; [ nmi ]

.proc JumpNMI

_cee0:  jml     $001f00

.endproc

; ---------------------------------------------------------------------------

; [ irq ]

.proc JumpIRQ

_cee4:  jml     $001f04

.endproc

        end_fixed_block 0

; ===========================================================================

.export RNGTbl

.segment "rng_tbl"

; c0/fec0
RNGTbl:
        .incbin "rng_tbl.dat"

; ===========================================================================

.segment "snes_header"

; c0/ffc0
        .byte   "FINAL FANTASY 5      " ; rom title
        .byte   $21                     ; HiROM, SlowROM
        .byte   $02                     ; rom + ram + sram
        .byte   $0b                     ; rom size: 16 Mbit
        .byte   $03                     ; sram size: 64 kbit
        .byte   $00                     ; destination: japan
        .byte   $c3                     ; publisher: squaresoft
        .byte   $00                     ; revision: 1.0
        .word   0                       ; checksum (calculate later)
        .word   0                       ; inverse checksum

; ===========================================================================

.segment "vectors"

; c0/ffe0
        .res    10
        .addr   JumpNMI
        .res    2
        .addr   JumpIRQ
        .res    12
        .addr   Reset
        .res    2

; ===========================================================================

.segment "unknown_c0cf00"

; c0/cf00
_c0cf00:
        .incbin "unknown_c0cf00.dat"

; ===========================================================================

.segment "unknown_c0d240"

; c0/d240
_c0d240:
        .incbin "unknown_c0d240.dat"

; ===========================================================================

.scope _c0d980
        ARRAY_LENGTH = 28
        Start = _c0d980Ptrs
.endscope

.segment "unknown_c0d980"

; c0/d980
_c0d980Ptrs:
        ptr_tbl _c0d980

; c0/d9b8
_c0d980:
.repeat _c0d980::ARRAY_LENGTH, i
        array_item _c0d980, {i} := *
        .incbin .sprintf("unknown_c0d980/unknown_c0d980_%04x.dat.lz", i)
.endrep

; ===========================================================================

.segment "world_tilemap"

; c7/0000
WorldTilemap:
        .incbin "world_tilemap.dat"

; ===========================================================================

.segment "event_script"

; c8/3320
EventScriptPtrs:
        ptr_tbl_far EventScript

; c8/49dc
EventScript:
        .incbin "event_script.dat"

; ===========================================================================

.segment "sub_tilemap"

.scope SubTilemap
        ARRAY_LENGTH = 323
        Start = bank_start SubTilemap
.endscope

; cb/0000
SubTilemapPtrs:
        fixed_block $0290
        ptr_tbl SubTilemap
        end_ptr SubTilemap
        end_fixed_block 0

; cb/0290
SubTilemap:
        fixed_block $02f7b0
.repeat SubTilemap::ARRAY_LENGTH, i
        array_item SubTilemap, {i} := *
        .incbin .sprintf("sub_tilemap/sub_tilemap_%04x.dat.lz", i)
.endrep
SubTilemap::End:
        end_fixed_block 0

; ===========================================================================

.segment "map_pal_anim"

; cd/fa40
MapPalAnim:
        fixed_block $01c0
        .incbin "map_pal_anim.dat"

; cd/fba8
; palette animation colors
        .word   $017d,$029d,$4f7e,$009d,$0073,$273f,$129f,$0e1d
        .word   $0d9d,$0cfa,$0cf3,$0cac,$25af
        end_fixed_block 0

; ===========================================================================

.segment "sine_tbl"

; cd/fc00
SineTbl:
        .incbin "sine_tbl.dat"

; ===========================================================================

.segment "npc_script"

; ce/0000
NPCScriptPtrs:
        ptr_tbl NPCScript

; ce/0740
NPCScript:
        .incbin "npc_script.dat"

; ===========================================================================

.segment "event_trigger"

; ce/2400
EventTriggerPtrs:
        ptr_tbl EventTrigger

; ce/2800
EventTrigger:
        .incbin "trigger/event_trigger.dat"

; ===========================================================================

.segment "entrance_trigger"

; ce/36c0
EntranceTriggerPtrs:
        ptr_tbl EntranceTrigger

; ce/3ac0
EntranceTrigger:
        fixed_block $1f00
        .incbin "trigger/entrance_trigger.dat"
        end_fixed_block 0

; ===========================================================================

.segment "npc_prop"

; ce/59c0
NPCPropPtrs:
        ptr_tbl NPCProp
        end_ptr NPCProp

; ce/5dc2
NPCProp:
        .incbin "trigger/npc_prop.dat"
NPCProp::End:

; ===========================================================================

.segment "map_prop"

; ce/9c00
MapProp:
.repeat 512, i
        .incbin .sprintf("map_prop/map_prop_%04x.dat", i)
.endrep

; ===========================================================================

.scope MapTileset
        ARRAY_LENGTH = 28
        Start = MapTilesetPtrs
.endscope

.segment "map_tileset"

        fixed_block $c540

; cf/0000
MapTilesetPtrs:
        ptr_tbl MapTileset

; cf/0038
MapTileset:
.repeat MapTileset::ARRAY_LENGTH, i
        array_item MapTileset, {i} := *
        .incbin .sprintf("map_tileset/map_tileset_%04x.dat.lz", i)
.endrep

        end_fixed_block 0

; ===========================================================================

.scope MapTileProp
        ARRAY_LENGTH = 23
        Start = MapTilePropPtrs
.endscope

.segment "map_tile_prop"

; cf/c540
MapTilePropPtrs:
        ptr_tbl MapTileProp

; cf/c56e
MapTileProp:
.repeat MapTileProp::ARRAY_LENGTH, i
        array_item MapTileProp, {i} := *
        .incbin .sprintf("map_tile_prop/map_tile_prop_%04x.dat.lz", i)
.endrep
MapTileProp::End:

; ===========================================================================

.segment "world_tilemap_ptrs"

; cf/e000
WorldTilemapPtrs:
        ptr_tbl WorldTilemap

; ===========================================================================

.segment "world_tile_prop"

; cf/ea00
WorldTileProp:
        .incbin "world_tile_prop/bartz_world.dat"
        .incbin "world_tile_prop/galuf_world.dat"
        .incbin "world_tile_prop/underwater.dat"

; ===========================================================================

.segment "world_tileset"

; cf/f0c0
WorldTileset:
        .incbin "world_tileset/bartz_world.dat"
        .incbin "world_tileset/galuf_world.dat"
        .incbin "world_tileset/underwater.dat"

; ===========================================================================

.segment "rand_battle_grp"

; d0/6800
RandBattleGrp:
        .incbin "rand_battle_grp.dat"

; ===========================================================================

.segment "char_prop"

; d1/7000
CharProp:
        .incbin "char_prop.dat"

; ===========================================================================

.segment "event_battle_grp"

; d0/7800
EventBattleGrp:
        .incbin "event_battle_grp.dat"

; ===========================================================================

.segment "world_battle_grp"

; d0/7a00
WorldBattleGrp:
        .incbin "world_battle_grp.dat"

; ===========================================================================

.segment "sub_battle_grp"

; d0/8000
SubBattleGrp:
        .incbin "sub_battle_grp.dat"

; ===========================================================================

.segment "world_battle_rate"

; d0/8400
WorldBattleRate:
        .incbin "world_battle_rate.dat"

; ===========================================================================

.segment "sub_battle_rate"

; d0/84c0
SubBattleRate:
        .incbin "sub_battle_rate.dat"

; ===========================================================================

.segment "map_treasures"

; d1/3000
MapTreasures:
        .incbin "map_treasures.dat"

; ===========================================================================

.segment "treasure_prop"

; d1/3210
TreasureProp:
        .incbin "treasure_prop.dat"

; ===========================================================================

.segment "init_npc_switch"

; d8/e000
InitNPCSwitch:
        .incbin "init_npc_switch.dat"

; ===========================================================================

.segment "event_cond"

; d8/e080
EventCondPtrs:
        ptr_tbl EventCond

; d8/e600
EventCond:
        .incbin "event_cond.dat"

; ===========================================================================
