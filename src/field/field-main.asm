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

.import ExecBattle_ext
.import ExecMenu_ext
.import ShowCutscene_ext, Decomp_ext
.import InitSound_ext, ExecSound_ext

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
        jsr     $490a
        lda     #3
        sta     $0134
        jsl     ExecMenu_ext
        jsr     $44e3
        jsr     InitHardware
        jsr     InitInterrupts
        lda     $0139
        beq     _007d                   ; branch if there are no saved games
        jsr     $491d                   ; reset ram $0b00-$1d00
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
        jsr     $545d                   ; load map
        jmp     Main
_007d:  jsr     $48fa                   ; init character data (new game)
        jsr     $48ed                   ; init event flags
        jsr     $48dd                   ; init npc flags
        jsr     $4528                   ; init character names
        jsr     $450a                   ; init vehicles
        lda     #1
        sta     $bd                     ; show party sprite
        sta     $bc                     ; enable walking animation
        ldx     #$0010                  ; event $10 (intro)
        stx     $ce
        lda     #$01
        sta     $57
        lda     #$81
        sta     $4200
        cli
        jsr     $a217                   ; execute event
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
        jsr     $ca3c                   ; get event flag $01xx
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
        jsr     $454c                   ; open menu
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
        jsr     $a18b                   ; update timer
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
_01fb:  jsr     $cb11
        lda     $55
        beq     _0220
        lda     #$ff
        jsr     $ca3c                   ; get event flag $01xx
        cmp     #$00
        bne     _0220
        jsr     $ccf0                   ; random battle
        lda     $0ad8
        sta     $1088
        lda     $0ad9
        sta     $1089
        jsr     ReloadMap
        jmp     _00ad
_0220:  stz     $55
        lda     $03
        and     #$40
        beq     _0246
        lda     #$fb
        jsr     $ca3c                   ; get event flag $01xx
        cmp     #$00
        beq     _0246
        jsr     $6632                   ; show mini-map
        lda     $0ad8
        sta     $1088
        lda     $0ad9
        sta     $1089
        jsr     ReloadMap
        jmp     _00ad
_0246:  jsr     _c00f8c
        jsr     _c01a1d
        jsr     $4c95                   ; clear sprite data
        jsr     _c02137
        jsr     $612b
        jsr     _c01ec5
        jsr     _c01e64
        jsr     $420a
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
_0281:  jsr     $ca69       ;
        lda     $55
        beq     _029a
        lda     #$ff
        jsr     $ca3c       ; get event flag $01ff (disable random battles)
        cmp     #$00
        bne     _029a
        jsr     $ccf0       ; random battle
        jsr     ReloadMap
        jmp     _00ad
_029a:  stz     $55
        jsr     _c032ab       ; update objects
        jsr     c0_11c2
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
        jsr     $4c95       ; clear sprite data
        jsr     $4834
        jsr     UpdatePlayerSprite
        jsr     _c039b3       ; update object sprites
        jsr     _c02842
        jsr     $420a
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
        sta     $2131       ; addition/subtraction
        lda     $49
        sta     $212d       ; subscreen designation
        jsr     $4d67       ; copy color palettes to ppu
        jsr     $4d3e       ; copy sprite data to ppu
        jsr     $4a7a
        jsr     $4653       ; update screen pixelation
        jsr     $49ff       ; update fixed color
        lda     $a3
        beq     _0314
        stz     $a3
        jsr     $40d8       ; copy data to vram
_0314:  lda     $52
        jne     _041f
        jsr     $4bc0       ; update scrolling registers
        lda     $53
        bne     _0360
        jsr     $637e
        jsr     $9695
        jsr     $975f
        jsr     $964c
        jsr     $9722
        lda     $9f
        beq     _0348
        stz     $9f
        lda     $ba
        beq     _0348
        and     #$01
        bne     _0345
        jsr     $64bb
        jmp     _0348
_0345:  jsr     $6465
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
        jsr     $5e2b       ; copy tile layout to vram ???
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
        jsr     $6e7a
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
        jsr     $6df5       ; horizontal scrolling (bg1)
        lda     $1121
        and     #$40
        bne     _03b7
        ldx     #$1000
        stx     $71
        jsr     $6df5       ; horizontal scrolling (bg2)
_03b7:  lda     $1121
        bmi     _03c4
        ldx     #$2000
        stx     $71
        jsr     $6df5       ; horizontal scrolling (bg3)
_03c4:  jmp     _0408
_03c7:  ldx     $06
        stx     $71
        jsr     $6e7a       ; vertical scrolling (bg1)
        lda     $1121
        and     #$40
        bne     _03dd
        ldx     #$1000
        stx     $71
        jsr     $6e7a       ; vertical scrolling (bg2)
_03dd:  lda     $1121
        bmi     _03ea
        ldx     #$2000
        stx     $71
        jsr     $6e7a       ; vertical scrolling (bg3)
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
_0405:  jsr     $996d       ; copy map animation graphics to vram
_0408:  jsr     $5e8a
        jsr     $5f3e
        lda     $5e         ; hdma enable
        sta     $420c
        jsr     $9a00
        jsr     $9799       ; update palette animation
        jsr     $4931
        jsr     $47aa
_041f:  jsr     $4c90
        jsr     $4d8e
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
        lda     $d8e080,x               ; pointer to event condition
        sta     $23
        lda     $d8e082,x
        sta     $26
        lda     $06
        shorta
_047c:  ldx     $23
        lda     $d8e080,x
        cmp     #$ff                    ; $ff: execute event
        jeq     _05fc
        cmp     #$fe                    ; $fe: if event flag $00xx is set
        bne     _049b
        lda     $d8e081,x
        jsr     $ca2f                   ; get event flag $00xx
        cmp     #$00
        bne     _04d1
        jmp     _05e5
_049b:  cmp     #$fd                    ; $fd: if event flag $00xx is clear
        bne     _04ad
        lda     $d8e081,x
        jsr     $ca2f                   ; get event flag $00xx
        cmp     #$00
        beq     _04d1
        jmp     _05e5
_04ad:  cmp     #$fc                    ; $fc: if event flag $01xx is set
        bne     _04bf
        lda     $d8e081,x
        jsr     $ca3c                   ; get event flag $01xx
        cmp     #$00
        bne     _04d1
        jmp     _05e5
_04bf:  cmp     #$fb                    ; $fb: if event flag $01xx is clear
        bne     _04da
        lda     $d8e081,x
        jsr     $ca3c                   ; get event flag $01xx
        cmp     #$00
        jne     _05e5
_04d1:  ldx     $23
        inx2
        stx     $23
        jmp     _047c
_04da:  cmp     #$fa                    ; $fa: compare ram (1-byte)
        bne     _051c
        longa
        lda     $d8e081,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $d8e082,x
        and     #$c0
        bne     _0500
        lda     $0500,y
        cmp     $d8e083,x
        beq     _0568
        jmp     _05e5
_0500:  cmp     #$40
        bne     _0510
        lda     $0500,y
        cmp     $d8e083,x
        bcs     _0568
        jmp     _05e5
_0510:  lda     $0500,y
        cmp     $d8e083,x
        bcc     _0568
        jmp     _05e5
_051c:  cmp     #$f9                    ; $f9: compare ram (2-byte)
        bne     _0573
        longa
        lda     $d8e081,x
        and     #$3fff
        tay
        lda     $d8e081,x
        and     #$c000
        bne     _0543
        lda     $0500,y
        cmp     $d8e083,x
        beq     _0568
        lda     $06
        shorta
        jmp     _05e5
_0543:  cmp     #$00
        rti
        bne     _0558
        lda     $0500,y
        cmp     $d8e083,x
        bcs     _0568
        lda     $06
        shorta
        jmp     _05e5
_0558:  lda     $0500,y
        cmp     $d8e083,x
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
        lda     $d8e081,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y
        and     $d8e083,x
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
        lda     $d8e081,x
        cmp     $0adb                   ; facing direction
        bne     _05e5
        jmp     _04d1
_05b1:  cmp     #$f6                    ; $f6: if button pressed
        bne     _05cb
        lda     $02
        and     #$80
        beq     _05e5
        lda     $10b8
        bne     _05e5
        inc     $10b8
        ldx     $23
        inx
        stx     $23
        jmp     _047c
_05cb:  longa                           ; $f5: boolean compare ram (2-byte)
        lda     $d8e081,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y
        and     $d8e083,x
        beq     _0568
        jmp     _05e5
_05e5:  ldx     $23
_05e7:  inx
        lda     $d8e080,x
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
        lda     $d8e081,x               ; event index
        sta     $ce
        lda     $06
        shorta
        ldx     $ce
        beq     _0617
        jsr     $a217                   ; execute event
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
        lda     $ce2402,x               ; event triggers
        sta     $23
        lda     $ce2400,x
        tax
_0654:  cpx     $23
        beq     _0681
        lda     $0ad8
        cmp     $ce2400,x
        beq     _066a
        txa
        clc
        adc     #$0004
        tax
        jmp     _0654
_066a:  lda     $06
        shorta
        lda     #$01
        sta     $5a
        longa
        lda     $ce2402,x
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
        lda     $ce36c2,x               ; entrance triggers
        sta     $23
        lda     $ce36c0,x
        tax
_069b:  cpx     $23
        jeq     _0739
        lda     $0ad8
        cmp     $ce36c0,x
        beq     _06b4
        txa
        clc
        adc     #$0006
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
_06d2:  lda     $ce36c2,x
        and     #$03ff
        sta     $0ad4
        sta     $0ad6                   ; map index
        cmp     #$0005
        bcs     _06f8
        lda     $06
        shorta
        lda     $ce36c4,x
        sta     $1088
        lda     $ce36c5,x
        sta     $1089
        bra     _0737
_06f8:  lda     $06
        shorta
        lda     $ce36c3,x
        and     #$08
        beq     _0707
        inc     $16a0                   ; enable map title
_0707:  lda     $ce36c3,x
        lsr4
        sta     $169e
        lda     $ce36c4,x
        and     #$c0
        lsr6
        sta     $b9
        inc
        sta     $ba
        sta     $bf
        lda     $ce36c4,x
        and     #$3f
        sta     $1088
        lda     $ce36c5,x
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
        and     #$80
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
        lda     f:_c00897,x
        sta     $26
        lda     f:_c00897+1,x
        sta     $27
        jsr     $4583
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
        and     #$80
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
        lda     f:_c008a5,x
        sta     $26
        lda     f:_c008a5+1,x
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
        jsr     $4583
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

_c00897:
_0897:  .addr $0000,$08ba,$08c1,$08ef,$098d,$0a4d,$0a57

_c008a5:
_08a5:  .addr $0000,$08bb,$091d,$0955,$098e,$0a54,$0aa3

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
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y     ; vehicle height
        jsr     _c02137
        jsr     $612b
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
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     $612b
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
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     $612b
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
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     $612b
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
        jsr     $4a68
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
        jsr     $4798
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
_c30a57:
_0a57:  jsr     $4798
        lda     #$00
        sta     $3d
_0a5e:  jsr     WaitVBlank
        jsr     _c05bf8
        jsr     $4c95       ; clear sprite data
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
        jsr     $4741
_0a8e:  jsr     _c02137
        jsr     $612b
        jsr     _c01e64
        jsr     $47f7
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
        jsr     $4a68
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
        jsr     $4583
        jsr     WaitVBlank
_0afa:  rts
_0afb:  lda     $57
        bne     _0b5c
        lda     #$fa
        jsr     $ca3c       ; get event flag $01xx
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
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f+16,x
        sta     $6f
        asl
        ora     #$80
        sta     $0ade,y
        jsr     _c02137
        jsr     $612b
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
        jsr     $4583
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
        jsr     $ca3c       ; get event flag $01xx
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
        jsr     $4583
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
        jsr     $4c95       ; clear sprite data
        jsr     _c02137
        jsr     $612b
        jsr     _c01e64
        lda     $03
        and     #$80
        beq     _0cbd
        lda     #$80
        sta     $1697
        rts
_0cbd:  lda     $02
        and     #$80
        beq     _0ccd
        lda     $10b8
        bne     _0ccd
        inc     $10b8
        bra     _0d3c
_0ccd:  lda     $03
        and     #$0c
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
        jsr     $ca3c       ; get event flag $01xx
        cmp     #$00
        bne     _0d63
        lda     $02
        and     #$80
        beq     _0d73       ; branch if the a button is not down
        lda     $10b8
        bne     _0d73       ;
        inc     $10b8
_0d63:  jsr     CheckNPCEvents
        lda     $58
        bne     _0d73       ; branch if an event is running
        lda     $02
        and     #$80
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
        lda     $d13000,x               ; first treasure on this map
        and     #$00ff
        asl2
        sta     $23
        lda     $d13001,x               ; first treasure on next map
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
        lda     $d13210,x               ; check x position
        cmp     $75
        bne     _0dca
        lda     $d13211,x               ; check y position
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
        jsr     $ca16                   ; get treasure flag
        cmp     #$00
        bne     _0dd2
        lda     #$01
        sta     $58
        stz     $ba
        ldx     $23
        phx
        lda     $d13211,x
        longa
        asl6
        sta     $0d
        lda     $d13210,x
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
        jsr     $463c                   ; play sound effect
        ldx     $06
        stx     $73
        ldx     #$0101
        stx     $2c
        lda     #$12
        sta     $16b3
        jsr     $6f08
        bra     _0e33
_0e2e:  lda     #$68
        jsr     $463c                   ; play sound effect
_0e33:  plx
        lda     $d13212,x
        sta     $11
        lda     $d13213,x
        sta     $12
        lda     $11
        jmi     _0ebc
        and     #$e0                    ; gp
        bne     _0e63
        jsr     CalcGil
        jsr     GiveGil
        jsr     $4dd7
        ldx     #$0003
        stx     $af
        jsr     $83ae                   ; show dialog
        lda     $16a1
        jsr     $ca21
        rts
_0e63:  cmp     #$20                    ; spell
        bne     _0e7e
_0e67:  lda     $12
        sta     $16a3
        jsr     $c9a5                   ; give magic
        ldx     #$0004
        stx     $af
        jsr     $83ae                   ; show dialog
        lda     $16a1
        jsr     $ca21
        rts
_0e7e:  lda     $12                     ; item
        jsr     $bfdd
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
        jsr     $83ae                   ; show dialog
        lda     $16a1
        jsr     $ca21
        rts
_0ebc:  ldx     #$0005                  ; monster-in-a-box
        stx     $af
        jsr     $83ae                   ; show dialog
        lda     #$ff
        sta     $0be0
        lda     $11
        and     #$3f
        ora     #$40
        ldx     $11
        phx
        jsr     $bde6                   ; event battle
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
        sta     $4202
        lda     f:_c00f78,x
        sta     $4203
        nop4
        ldy     $4216
        sty     $37
        lda     $12
        sta     $4202
        lda     f:_c00f78+1,x
        sta     $4203
        nop3
        longa
        lda     $4216
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
        and     #$08
        beq     _0fc7
        lda     #$01
        sta     $c4
        jsr     _c01055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fc7:  lda     $03
        and     #$01
        beq     _0fdb
        lda     #$02
        sta     $c4
        jsr     _c01055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fdb:  lda     $03
        and     #$04
        beq     _0fef
        lda     #$03
        sta     $c4
        jsr     _c01055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fef:  lda     $03
        and     #$02
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
        jsr     $69a1
        lda     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        jsr     $6513
        inc     $9f
        stz     $59
        stz     $16aa
        lda     $0adc
        bne     _102c
        jsr     DoPoisonDmg
_102c:  jsr     _c0103a
        lda     #$fc
        jsr     $c796
        lda     #$fd
        jsr     $c796
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
        jsr     $4583
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

.proc c0_11c2

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
        and     #$80
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
        and     #$08
        beq     _12f5
        lda     #$01        ; up
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_12f5:  lda     $03         ; check right button
        and     #$01
        beq     _1309
        lda     #$02        ; right
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_1309:  lda     $03         ; check down button
        and     #$04
        beq     _131d
        lda     #$03        ; down
        sta     $c4
        jsr     _c0163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_131d:  lda     $03         ; check left button
        and     #$02
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
        jsr     $c796       ;
        lda     #$fd
        jsr     $c796       ;
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
        jsr     $463c       ; play sound effect
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
        jsr     $6f08
        lda     #$8e
        jsr     $463c       ; play sound effect
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
        jsr     $6f08
        lda     #$01
        sta     $57
        ldx     #$000b      ; event $0b
        stx     $ce
        jsr     $a217       ; execute event
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
        jsr     $707d
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
        jsr     $707d
_1b2d:  lda     $1121
        bmi     _1b44
        lda     $7a
        sta     $76
        lda     $79
        sta     $75
        lda     $ba
        ldx     #$2000
        stx     $71
        jsr     $707d
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

_1ce2:  ldy     $06
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
        sta     $2115
        stz     $420b
        lda     #$01
        sta     $4300
        lda     #$18
        sta     $4301
        lda     #$da
        sta     $4304
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
        sta     $4302
        lda     #$6000
        sta     hVMADDL
        lda     $53
        bne     _1d99
        lda     #$0200
        bra     _1d9c
_1d99:  lda     #$0800
_1d9c:  sta     $4305
        lda     $06
        shorta
        lda     #$01
        sta     $420b
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
        jsr     $4cbc       ; copy data to vram
_1dc2:  rts

; vehicle
_1dc3:  pha
        cmp     #$0a
        bcs     _1dda
        ldx     $06
_1dca:  lda     $dfff40,x   ; copy color palette
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
        jsr     $4ad5       ; copy 3bpp graphics to vram
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
        sta     $4302
        lda     $06
        shorta
        lda     $53
        bne     _1e31
        ldx     #$7300      ; vram address = $7300 (normal map)
        bra     _1e34
_1e31:  ldx     #$6100      ; vram address = $6100 (world map)
_1e34:  stx     hVMADDL
        lda     #$80
        sta     $2115
        stz     $420b
        lda     #$01
        sta     $4300
        lda     #$18
        sta     $4301
        ldx     #$0800
        stx     $4305
        lda     #$db
        sta     $4304
        lda     #$01
        sta     $420b
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
        jsr     $4c95
        jsr     $47f7
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
@237c:  lda     $bd                     ; return if party sprite is not shown
        bne     _2381
        rts
_2381:  lda     $cb                     ; return if party sprite is hidden (z-level)
        beq     _2386
        rts
_2386:  lda     $be                     ; jump counter
        bpl     _238d
        and     #$7f
        lsr
_238d:  tax
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
        sta     $2115
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
_25d2:  lda     f:$c0df00,x
        stz     hVMDATAL
        sta     hVMDATAH
        inx
        dey
        bne     _25d2
        jsr     _c02636
        ldy     #$0008
_25e6:  lda     f:$c0df00,x
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
_2600:  lda     f:$c0df80,x
        stz     hVMDATAL
        sta     hVMDATAH
        inx
        dey
        bne     _2600
        jsr     _c02636
        ldy     #$0008
_2614:  lda     f:$c0df80,x
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
        lda     f:$c0d980,x   ;
        clc
        adc     #$d980
        sta     $04f0
        lda     $06
        shorta
        lda     #$c0
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
        lda     $ce0000,x
        sta     $23
        sta     $29
        lda     $ce0002,x
        sta     $26
        lda     $06
        shorta
@2fae:  ldx     $23
        lda     $ce0000,x
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
        lda     $ce0000,x   ; copy npc dialog
        sta     $13d6,y
        inx2
        iny2
        bra     @2fd2
@2fe3:  lda     $06
        shorta
@2fe7:  ldx     $29
        lda     $ce0000,x   ; npc script command

; command $ff: execute event
@2fed:  cmp     #$ff
        jeq     @314d

; command $fe: if event flag $00xx set
        cmp     #$fe
        bne     @3006
        lda     $ce0001,x
        jsr     $ca2f       ; get event flag $00xx
        cmp     #$00
        bne     @303c
        jmp     @3136

; command $fd: if event flag $00xx clear
@3006:  cmp     #$fd
        bne     @3018
        lda     $ce0001,x
        jsr     $ca2f       ; get event flag $00xx
        cmp     #$00
        beq     @303c
        jmp     @3136

; command $fc: if event flag $01xx set
@3018:  cmp     #$fc
        bne     @302a
        lda     $ce0001,x
        jsr     $ca3c       ; get event flag $01xx
        cmp     #$00
        bne     @303c
        jmp     @3136

; command $fb: if event flag $01xx clear
@302a:  cmp     #$fb
        bne     @3045
        lda     $ce0001,x
        jsr     $ca3c       ; get event flag $01xx
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
        lda     $ce0001,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $ce0002,x
        and     #$c0
        bne     @306b

; 0 (if equal)
@305f:  lda     $0500,y     ; character data
        cmp     $ce0003,x
        beq     @3087
        jmp     @3136

; 1 (if greater)
@306b:  cmp     #$40
        bne     @307b
        lda     $0500,y
        cmp     $ce0003,x
        bcs     @3087
        jmp     @3136

; 2 (if less)
@307b:  lda     $0500,y
        cmp     $ce0003,x
        jcs     @3136

@3087:  ldx     $29         ; skip 4 bytes
        inx4
        stx     $29
        jmp     @2fe7

; command $f9: if character data xxxx <=> yyyy
@3092:  cmp     #$f9
        bne     @30de
        longa
        lda     $ce0001,x
        and     #$3fff
        tay
        lda     $ce0001,x
        and     #$c000
        bne     @30b9
        lda     $0500,y     ; character data
        cmp     $ce0003,x
        beq     @3087
        lda     $06
        shorta
        jmp     @3136

@30b9:  .a16
        cmp     #$4000
        bne     @30ce
        lda     $0500,y
        cmp     $ce0003,x
        bcs     @30fc
        lda     $06
        shorta
        jmp     @3136
@30ce:  lda     $0500,y
        cmp     $ce0003,x
        bcc     @30fc
        lda     $06
        shorta
        jmp     @3136

; command $f8: if character data xxxx & yy
@30de:  cmp     #$f8
        bne     @310c
        longa
        lda     $ce0001,x   ; pointer to character data
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y     ; character data
        and     $ce0003,x   ; mask
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
        lda     $ce0001,x
        cmp     $0adb       ; facing direction
        bne     @3136
        jmp     @303c

; command $f5: if character data xxxx & yyyy
@311c:  longa
        lda     $ce0001,x
        and     #$3fff
        tay
        lda     $06
        shorta
        lda     $0500,y
        and     $ce0003,x
        jeq     @3087

; condition not met, check next condition
@3136:  ldx     $29
@3138:  inx
        lda     $ce0000,x
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
        lda     $ce0001,x   ; event index
        sta     $ce
        lda     $06
        shorta
        ldx     $ce
        beq     @3168
        jsr     $a217       ; execute event
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
        lda     $c0329b,x   ; pointer to forward tile
        tay
        lda     $10f3,y     ; tile properties byte 2
        cmp     #$ff
        bne     @31a0       ; branch if not a through-tile
        lda     $c0329e,x   ; forward x2 (interact through tile)
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
@31ad:  lda     $c0329c,x
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
        lda     $c0329d,x
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
        lda     $c0329e,x
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
        sta     $4202
        lda     #$14
        sta     $4203
        nop4
        ldx     $4216
        rts

; ---------------------------------------------------------------------------

; pointers to tiles in direction (forward, forward/left, forward/right, forward x2 )
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
        adc     $c034e1,x
        sta     $75
        lda     $147a,y
        clc
        adc     $c034e6,x
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

@34e1:  .byte   $00,$00,$01,$00,$ff
@34e6:  .byte   $00,$ff,$00,$01,$00

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
        and     $c03517,x
        beq     @3514
        txa                 ; can move (return movement direction)
        rts
@3514:  lda     #$00        ; can't move
        rts

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
        lda     $c022db,x
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
        lda     $c037c3,x
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
        lda     $c037c2,x
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
        lda     $c022db,x
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
        lda     $c02a97,x
        clc
        adc     $1481,y
        ldy     $23
        sta     $0242,y
        ldy     $e9
        lda     $c02a9b,x
        clc
        adc     $1481,y
        ldy     $23
        sta     $0246,y
        ldy     $e9
        lda     $c02a9f,x
        clc
        adc     $1483,y
        ldy     $23
        sta     $031a,y
        ldy     $e9
        lda     $c02aa3,x
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
        lda     $ce59c2,x   ; npc graphics
        cmp     $ce59bb,x
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
@3d6f:  lda     $ce59c2,x
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
        lda     $ce59c2,x

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
        lda     $ce59c1,x
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
        lda     $ce59c1,x
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
@3e55:  jsr     $4cbc       ; copy data to vram
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
        lda     $ce59c0,x   ; pointer to npc properties
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
        lda     $ce59c0,x   ; npc data
        sta     $e7
        lda     $ce59c2,x
        sec
        sbc     $ce59c0,x
        sta     $4204
        lda     $06
        shorta
        lda     #$07
        sta     $4206
        pha
        pla
        pha
        pla
        nop
        lda     $4214
        sta     $e6         ; number of objects
        bne     @3edc
        rts
@3edc:  jsr     _c03d28       ; load npc graphics
        ldy     $06
        sty     $e9
        stz     $e5
@3ee5:  ldy     $e9
        ldx     $e7
        lda     $ce59c6,x   ; palette
        and     #$07
        asl
        sta     $0f
        lda     $ce59c6,x   ; layer priority (top)
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
        lda     $ce59c6,x   ; layer priority (bottom)
        and     #$10
        ora     #$20
        ora     $0f
        longa
        xba
        ora     $1483,y
        sta     $1483,y
        lda     $06
        shorta
        lda     $ce59c5,x   ; misc. flags
        sta     $147e,y
        and     #$70
        beq     @3f47       ; branch if walking animation
        cmp     #$50
        beq     @3f47       ; branch if animal animation
        phx
        lda     $ce59c6,x   ; action frame
        and     #$e0
        lsr5
        tax
        lda     f:_c04000,x
        sta     $1487,y
        plx
        bra     @3f63
@3f47:  lda     $ce59c6,x   ; action frame
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
        lda     $ce59c0,x   ; npc script
        and     #$3fff
        sta     $147f,y
        lda     $06
        shorta
        lda     $ce59c3,x   ; x position
        and     #$3f
        longa
        xba
        sta     $1477,y
        lda     $06
        shorta
        lda     $ce59c4,x   ; y position
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
        lda     $ce59c3,x
        and     #$c0
        sta     $1485,y
        lda     $ce59c4,x
        and     #$c0
        lsr2
        ora     $1485,y
        ora     #$08
        sta     $1485,y
        longa
        lda     $147f,y
        shorta
        phy
        jsr     $c9c1       ; get npc flag
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
        sta     $4204
        lda     $06
        shorta
        lda     #$03
        sta     $4206
        longa
        nop7
        lda     $4214
        asl5
        ora     $4214
        asl5
        ora     $4214
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
        sta     $4202
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
        sta     $4203
        nop4
        lda     $4217
        sta     $13
        lda     $11
        sta     $4203
        nop4
        lda     $4217
        asl
        asl
        and     #$7c
        sta     $14
        lda     $0f
        sta     $4203
        nop4
        lda     $4217
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
        sta     $2115
        stz     $420b
        lda     #$01
        sta     $4300
        lda     #$18
        sta     $4301
        ldx     $16ad       ; vram destination
        stx     $2116
        ldx     #$0800      ; size = $0800
        stx     $4305
        ldx     #$7622      ; source = 7f/7622
        stx     $4302
        lda     #$7f
        sta     $4304
        lda     #$01
        sta     $420b
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04017:
@4017:  asl2
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

_44c8:  pha
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #0
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        pla
        jsl     ShowCutscene_ext
        jsr     $44e3                       ; init map bank
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
        stz     $420b
        stz     $420c
        lda     #$8f
        sta     $2100
        lda     #$00
        sta     $4200
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

_c0454c:
        jsr     $6081       ; fade out
        stz     $420b
        stz     $420c
        lda     #$00
        sta     $4200
        lda     #$80
        sta     $2100
        sei
        jsl     $c2a000     ; execute menu command
        jsr     $44e3       ; init map bank
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
        jsr     $ca3c       ; get event flag $01xx
        cmp     #$00
        bne     @45e3
        jsr     $4635       ; stop sound
        lda     $0adc       ; vehicles
        beq     @45cc
        cmp     #$06
        bne     @45b4
        lda     $0ad6       ; map index
        cmp     #$02
        bne     @45b4
        lda     $0af1
        and     #$03
        bne     @45ab
        lda     #$69
        jsr     $463c       ; play sound effect
@45ab:  lda     #$79
        jsr     $ca2f       ; get event flag $00xx
        cmp     #$00
        bne     @45cc
@45b4:  lda     $0adc       ; current vehicle
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
        jsr     $ca3c       ; get event flag $01xx
        cmp     #$00
        beq     @45d7
        lda     #$05
@45d7:  clc
        adc     $0ad6       ; map index
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

_460a:  sta     $08
        lda     $55
        cmp     #2
        beq     _4634
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
_4634:  rts

.endproc

; ---------------------------------------------------------------------------

; [ stop sound ]

_c04635:
        lda     #$f2        ; stop sound
        sta     $1d00
        bra     _464e

; ---------------------------------------------------------------------------

; [ play sound effect ]

; A: sound effect

_c0463c:
        sta     $1d01
        lda     #$02        ; play sound effect
        sta     $1d00
        lda     #$0f        ; full volume, no envelope
        sta     $1d02
        lda     #$88
        sta     $1d03
_464e:  jsl     ExecSound_ext
        rts

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
@4686:  sta     $2106       ; pixelation register
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
        stx     $4204
        lda     #$3c
        sta     $4206
        pha
        pla
        pha
        pla
        nop
        ldx     $4214
        stx     $4204
        lda     #$0a
        sta     $4206
        pha
        pla
        pha
        pla
        nop
        lda     $4216
        ora     #$40
        sta     $020e
        ldx     $4214
        stx     $4204
        lda     #$06
        sta     $4206
        pha
        pla
        pha
        pla
        nop
        lda     $4216
        ora     #$40
        sta     $020a
        ldx     $4214
        stx     $4204
        lda     #$0a
        sta     $4206
        pha
        pla
        pha
        pla
        nop
        lda     $4216
        ora     #$40
        sta     $0206
        lda     $4214
        ora     #$40
        sta     $0202
        lda     #$4f
        sta     $0212
        rts

; ---------------------------------------------------------------------------

; [ init npc flags ]

_c048dd:
        ldx     $06
@48df:  lda     $d8e000,x
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
@48fc:  lda     $d17000,x
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
        lda     $c049d3,x
        sta     $17
        lda     $3f
        tax
        lda     $c0fec0,x   ; random number table
        and     $17
        sta     $10a0
        sta     $10a4
        sta     $10a8
        lda     $d3
        lsr2
        and     #$03
        tax
        lda     $c049d3,x
        sta     $17
        lda     $3f
        tax
        lda     $c0fec1,x
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
        sta     $2132       ; fixed color
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
        lda     $c04a64,x
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
        sta     $2100
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04aad:
@4aad:  jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     $39b3       ; update object sprites
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
        jsr     $39b3       ; update object sprites
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
        stz     $420b
        lda     #$80
        sta     $2115
        lda     #$08
        sta     $4300
        lda     #$19
        sta     $4301
        ldx     $33
        stx     $2116
        ldx     #$0b06
        stx     $4302
        stz     $4304
        longa
        lda     $35
        asl3
        sta     $17
        asl
        clc
        adc     $17
        sta     $4305
        lda     $06
        shorta
        lda     #$01
        sta     $420b
        stz     $420b
        ldx     $30
        stx     $4302
        lda     #$db
        sta     $4304
        lda     #$18
        sta     $4301
        ldx     $33
        stx     $2116
        ldy     $06
@4b27:  lda     #$80
        sta     $2115
        lda     #$01
        sta     $4300
        ldx     #$0010
        stx     $4305
        lda     #$01
        sta     $420b
        stz     $420b
        stz     $2115
        stz     $4300
        ldx     #$0008
        stx     $4305
        lda     #$01
        sta     $420b
        iny
        cpy     $35
        bne     @4b27
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04b56:
        lda     #$80
        sta     $2115
        ldx     $06
        stx     $2116
        lda     $0ad6       ; map index
        tax
        lda     $c04bbb,x
        sta     $24
        stz     $23
        ldx     $23
        ldy     $06
@4b70:  lda     $cff9c0,x
        sta     $1873,y
        inx
        iny
        cpy     #$0100
        bne     @4b70
        lda     $0ad6       ; map index
        tax
        lda     $c04bbb,x
        asl5
        sta     $24
        stz     $23
        ldx     $23
        ldy     #$0000
@4b94:  lda     $db8000,x
        sta     $0a
        inx
        and     #$0f
        ora     $1873,y
        sta     $2119
        lda     $0a
        lsr4
        ora     $1873,y
        sta     $2119
        txa
        and     #$1f
        bne     @4b94
        iny
        cpy     #$0100
        bne     @4b94
        rts

; ---------------------------------------------------------------------------

_c04bbb:
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
        sta     $210d
        lda     $18
        sta     $210d
        lda     $19
        sta     $210e
        lda     $1a
        sta     $210e
        lda     $1b
        sta     $210f
        lda     $1c
        sta     $210f
        lda     $1d
        sta     $2110
        lda     $1e
        sta     $2110
        lda     $1f
        sta     $2111
        lda     $20
        sta     $2111
        lda     $21
        sta     $2112
        lda     $22
        sta     $2112
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
        sta     $211f
        lda     $18
        sta     $211f
        lda     $19
        sta     $2120
        lda     $1a
        sta     $2120
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04c90:
        jsl     $c2a008     ; update joypad input
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
        sta     $2115
        stz     $420b
        lda     #$01
        sta     $4300
        lda     #$18
        sta     $4301
        lda     $25
        sta     $4304
        ldx     $2e
        stx     $2116
        ldx     $23
        stx     $4302
        ldx     $2c
        stx     $4305
        lda     #$01
        sta     $420b
        rts

; ---------------------------------------------------------------------------

; [ disable interrupts ]

_c04ce8:
@4ce8:  stz     $420b
        stz     $420c
        lda     #$80
        sta     $2100
        lda     #$00
        sta     $4200
        sei
        rts

; ---------------------------------------------------------------------------

; [ enable interrupts ]

_c04cfa:
@4cfa:  lda     #$81
        sta     $4200
        lda     #$00
        sta     $2100
        cli
        rts

; ---------------------------------------------------------------------------

; [ clear vram ]

_c04d06:
        jsr     $4ce8       ; disable interrupts
        stz     $0b6d
        jsr     $4d13       ; fill vram
        jsr     $4cfa       ; enable interrupts
        rts

; ---------------------------------------------------------------------------

; [ fill vram ]

; +$2c: size
; +$2e: start address
;  $6d: fill value

_c04d13:
@4d13:  lda     #$80
        sta     $2115
        stz     $420b
        lda     #$09
        sta     $4300
        lda     #$18
        sta     $4301
        ldx     $2e
        stx     $2116
        ldx     #$0b6d
        stx     $4302
        stz     $4304
        ldx     $2c
        stx     $4305
        lda     #$01
        sta     $420b
        rts

; ---------------------------------------------------------------------------

; [ copy sprite data to ppu ]

_c04d3e:
        lda     $5c
        bne     @4d66
        stz     $2102
        stz     $420b
        stz     $4300
        lda     #$04
        sta     $4301
        ldx     #$0200
        stx     $4302
        lda     #$00
        sta     $4304
        ldx     #$0220
        stx     $4305
        lda     #$01
        sta     $420b
@4d66:  rts

; ---------------------------------------------------------------------------

; [ copy color palettes to ppu ]

_c04d67:
        stz     $420b
        stz     $2121
        lda     #$02
        sta     $4300
        lda     #$22        ; -> $2122 (color palette address)
        sta     $4301
        lda     #$00
        sta     $4304
        ldx     #$0c00      ; source = 00/0c00
        stx     $4302
        ldx     #$0200
        stx     $4305
        lda     #$01
        sta     $420b
        rts

; ---------------------------------------------------------------------------

; [  ]

_c04d8e:
        lda     $02
        and     #$80
        bne     @4d97
        stz     $10b8
@4d97:  lda     $02
        and     #$40
        bne     @4da0
        stz     $10b4
@4da0:  lda     $02
        and     #$20
        bne     @4da9
        stz     $10b6
@4da9:  lda     $02
        and     #$10
        bne     @4db2
        stz     $10b7
@4db2:  lda     $03
        and     #$80
        bne     @4dbb
        stz     $10b9
@4dbb:  lda     $03
        and     #$40
        bne     @4dc4
        stz     $10b5
@4dc4:  lda     $03
        and     #$20
        bne     @4dcd
        stz     $10ba
@4dcd:  lda     $03
        and     #$10
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
        adc     $c04e21,x
        sta     $37
        lda     $39
        adc     $c04e31,x
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
        sta     $2105
        stz     $2106
        lda     #$00
        sta     $210b
        lda     #$04
        sta     $210c
        lda     #$80
        sta     $2115
        stz     $211a
        stz     $211b
        lda     #$08
        sta     $211b
        stz     $211c
        stz     $211c
        stz     $211d
        stz     $211d
        stz     $211e
        lda     #$08
        sta     $211e
        lda     #$80
        sta     $211f
        sta     $211f
        sta     $2120
        sta     $2120
        sta     $2121
        lda     #$bf
        sta     $2123
        lda     #$0b
        sta     $2124
        lda     #$bb
        sta     $2125
        lda     #$08
        sta     $2126
        lda     #$f7
        sta     $2127
        lda     #$ff
        sta     $2128
        lda     #$00
        sta     $2129
        lda     #$01
        sta     $212a
        lda     #$00
        sta     $212b
        lda     #$13
        sta     $212c
        lda     #$04
        sta     $212d
        lda     #$17
        sta     $212e
        stz     $212f
        lda     #$22
        sta     $2130
        lda     #$e0
        sta     $2132
        lda     #$00
        sta     $2133
        lda     #$ff
        sta     $4201
        stz     $4207
        stz     $4208
        stz     $4209
        stz     $420a
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
        lda     $c0fec0,x   ; random number table
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
        sta     $4202
        lda     $1a53
        sta     $4203
        nop3
        stz     $38
        lda     $4217
        sta     $37
        lda     $1a54
        sta     $4203
        nop2
        longa
        lda     $37
        clc
        adc     $4216
        bra     @4fb8
@4f8e:  eor     #$1aff
        sta     $4202
        lda     $1a53
        sta     $4203
        nop3
        stz     $38
        lda     $4217
        sta     $37
        lda     $1a54
        sta     $4203
        nop2
        longa
        lda     $37
        clc
        adc     $4216
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
        sta     $4202
        lda     $1a53
        sta     $4203
        nop3
        stz     $38
        lda     $4217
        sta     $37
        lda     $1a54
        sta     $4203
        nop2
        longa
        lda     $37
        clc
        adc     $4216
        bra     @501c
@4ff2:  eor     #$1aff
        sta     $4202
        lda     $1a53
        sta     $4203
        nop3
        stz     $38
        lda     $4217
        sta     $37
        lda     $1a54
        sta     $4203
        nop2
        longa
        lda     $37
        clc
        adc     $4216
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
@545a:  jsr     $6081       ; fade out
        ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @546c
        jsr     LoadWorldMap
        jsr     $6100       ; fade in
        rts
@546c:  jsr     LoadSubMap
        jsr     $6100       ; fade in
        jsr     $9267       ; show map title
        rts

; ---------------------------------------------------------------------------

; [ reload map ]

ReloadMap:
@5476:  ldx     $0ad6       ; map index
        cpx     #$0005
        bcs     @5485
        jsr     $5532       ; reload world map
        jsr     $6100       ; fade in
        rts
@5485:  jsr     $577c       ; reload map
        jsr     $6100       ; fade in
        jsr     $9267       ; show map title
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
        jsr     $c796
        lda     #$fd
        jsr     $c796
        stz     $16a4
        stz     $16a5
        stz     $16a6
        stz     $420b
        stz     $420c
        lda     #$80
        sta     $2100
        lda     #$00
        sta     $4200
        sei
        stz     $5e
        jsr     $4e69       ; init hardware registers
        jsr     $4c95       ; clear sprite data
        jsr     $4d3e       ; copy sprite data to ppu
        lda     #$70
        sta     $c5
        sta     $c6
        jsr     $5ee5
        jsr     $5f8d
        jsr     $990d       ; init palette animation
        jsr     $41f1
        jsr     $62bc
        rts

; ---------------------------------------------------------------------------

; [ load world map ]

LoadWorldMap:
@5528:  lda     $0adc
        beq     @5532
        lda     #$03
        sta     $0adb
@5532:  stz     $53
        stz     $169a
        jsr     $54a7
        lda     #$00
        sta     $2107
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
@555d:  jsr     $4b56
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
        jsr     $4cbc       ; copy data to vram
        ldx     #$6200
        stx     $2e
        ldx     #$0400
        stx     $2c
        ldx     #$c000
        stx     $23
        lda     #$da        ; da/c000 (flying chocobo graphics)
        sta     $25
        jsr     $4cbc       ; copy data to vram
        ldx     #$6440
        stx     $33
        ldx     #$000c
        stx     $35
        ldx     #$3ac0
        stx     $30
        jsr     $4ad5       ; copy 3bpp graphics to vram
        ldx     #$6540
        stx     $33
        ldx     #$00f0
        stx     $35
        ldx     #$3b80
        stx     $30
        jsr     $4ad5       ; copy 3bpp graphics to vram
        ldx     #$6400
        stx     $2e
        ldx     #$0080
        stx     $2c
        ldx     #$1f00
        stx     $23
        lda     #$da        ; da/1f00 (gradient graphics 1)
        sta     $25
        jsr     $4cbc       ; copy data to vram
        ldx     #$6500
        stx     $2e
        ldx     #$0080
        stx     $2c
        ldx     #$1f80
        stx     $23
        lda     #$da        ; da/1f80 (gradient graphics 2)
        sta     $25
        jsr     $4cbc       ; copy data to vram
        jsr     $56f8
        ldx     #$0080
@55e8:  lda     $dffcff,x
        sta     $0cff,x
        dex
        bne     @55e8
        ldx     #$0040
@55f5:  lda     $dffbff,x
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
@5610:  lda     $c0d340,x
        sta     $0d80,y
        inx
        iny
        cpy     #$0020
        bne     @5610
        jsr     $4d67       ; copy color palettes to ppu
        jsr     $4c95       ; clear sprite data
        jsr     $4d3e       ; copy sprite data to ppu
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
@5650:  lda     $cfea00,x   ; world map tile properties
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
        adc     #$f0c0
        tay
        lda     $06
        shorta
        lda     #$cf
        jsr     $6b2d       ; load world tileset
        jsr     $6c4a
        jsr     $4583
        jsr     $4798
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
        jsr     $612b
        jsr     $2137
        jsr     _c01e64
        jsr     _c01ec5
        jsr     $61d7
        lda     #$07
        sta     $2105
        jsr     $630a
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

; [  ]

_c056f8:
@56f8:  lda     $0ad6       ; map index
        tax
        lda     f:WorldTilesetTbl,x
        longa
        xba
        tax
        ldy     $06
@5706:  lda     $cffcc0,x
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
        jsr     $990d       ; init palette animation
@572e:  jsr     $9618
        jsr     $9704
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

; [ load normal map ]

LoadSubMap:
@574c:  lda     #$01
        sta     $53
        stz     $169f       ; clear hiryuu flag
        lda     $b9
        sta     $0adb
        inc
        sta     $bf
        jsr     $54a7
        jsr     $5af6       ; load map properties
        jsr     $5adb
        jsr     $3eaa       ; load npcs
        jsr     $6b44       ; load tile properties and tileset
        jsr     $5875       ; load map layouts
        jsr     $58db       ; load map palette
        jsr     $5cbd       ; load treasure chests
        jsr     $63d4
        jsr     $580e
        jsr     $57f9
        jsr     $54f6
        ldx     $0971
        stx     $0c02
        jsr     $591a       ; load map graphics
        lda     $55
        beq     @5792       ; branch if not a world map
        jsr     $5adb
        jsr     $3d28       ; load npc graphics
@5792:  stz     $ba
        ldx     #$0002
        stx     $c0
        jsr     $5b2d       ; init map color math
        jsr     $6b44       ; load tile properties and tileset
        jsr     $6d0c
        jsr     $6c6a
        jsr     _c08c92
        jsr     _c08c2e       ; init hdma #1 (window 2 position)
        jsr     _c08d0e
        jsr     $8b53
        jsr     $9a96       ; init map animation
        jsr     $5d30
        jsr     $2817
        jsr     $928c       ; init map title
        lda     #$fe
        jsr     $ca3c       ; get event flag $01xx
        cmp     #$00
        bne     @57cc
        lda     $1125       ; song
        jsr     $460a       ; play song
@57cc:  jsr     _c017e8       ; update local tile properties (normal map)
        jsr     _c01372       ; update party z-level (destination tile)
        jsr     _c013a6       ; update party z-level (current tile)
        jsr     UpdatePlayerSprite
        jsr     $39b3       ; update object sprites
        jsr     $2973
        jsr     $2842
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
        jsr     $6aca       ; load map layout
        bra     @5894
@588f:  lda     #$01
        jsr     $6b21
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
        jsr     $6aca       ; load map layout
        bra     @58b6
@58b1:  lda     #$01
        jsr     $6b21
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
        jsr     $6aca       ; load map layout
        bra     @58da
@58d5:  lda     #$01
        jsr     $6b21
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
@58e7:  lda     $c3bb00,x   ; map palette
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
@5909:  lda     $dffbff,x   ; sprite palettes
        sta     $0cff,x
        sta     $0d7f,x
        dex
        bne     @5909
        jsr     $4d67       ; copy color palettes to ppu
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
        lda     $dc2d84,x   ; pointers to map graphics
        clc
        adc     #$2e24
        sta     $23
        lda     #$00dc
        adc     $dc2d86,x
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
        jsr     $4cbc       ; copy data to vram
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
        jsr     $4cbc       ; copy data to vram
@5984:  longa
        lda     $1115       ; map graphics 2
        and     #$0fc0
        asl2
        xba
        asl2
        tax
        lda     $dc2d84,x   ; pointers to map graphics
        clc
        adc     #$2e24
        sta     $23
        lda     #$00dc
        adc     $dc2d86,x
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
        jsr     $4cbc       ; copy data to vram
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
        jsr     $4cbc       ; copy data to vram
@59f5:  longa
        lda     $1116       ; map graphics 3
        and     #$03f0
        lsr2
        tax
        lda     $dc2d84,x   ; pointers to map graphics
        clc
        adc     #$2e24
        sta     $23
        lda     #$00dc
        adc     $dc2d86,x
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
        jsr     $4cbc       ; copy data to vram
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
        jsr     $4cbc       ; copy data to vram
@5a63:  longa
        lda     $1116
        and     #$fc00
        lsr
        xba
        tax
        lda     $dc0000,x   ; map bg3 graphics
        clc
        adc     #$0024
        sta     $23
        lda     $06
        shorta
        lda     #$dc
        sta     $25
        ldx     #$4000
        stx     $2e
        ldx     #$1000
        stx     $2c
        jsr     $4cbc       ; copy data to vram
        jsr     TfrPartyGfx
        jsr     _c01e14
        ldx     #$3d00
        stx     $2e
        ldx     #$0600
        stx     $2c
        ldx     #$d380      ; c0/d380 (menu window graphics)
        stx     $23
        lda     #$c0
        sta     $25
        jsr     $4cbc       ; copy data to vram
        jsr     $257c
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
        jsr     $4d13       ; fill vram
@5ada:  rts

; ---------------------------------------------------------------------------

; [  ]

_c05adb:
@5adb:  lda     $a4
        bne     @5af5
        ldx     #$6400
        stx     $2e
        ldx     #$0200
        stx     $2c
        ldx     #$fe00      ; cdfe00
        stx     $23
        lda     #$cd
        sta     $25
        jsr     $4cbc       ; copy data to vram
@5af5:  rts

; ---------------------------------------------------------------------------

; [ load map properties ]

_c05af6:
@5af6:  lda     $0ad4       ; map index
        sta     $211b
        lda     $0ad5
        sta     $211b
        lda     #$1a        ; 26 bytes each
        sta     $211c
        sta     $211c
        ldx     $2134
        ldy     $06
@5b0f:  lda     $ce9c00,x   ; map properties
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
        sta     $2105       ; 8x8 tiles, high priority bg3, mode 1
        lda     #$49
        sta     $2107       ; 32x16 bg1 map at $4800
        ldx     $0ad6       ; map index
        cpx     #$00dd
        beq     @5b4b       ; branch if ???
        lda     #$51
        sta     $2108       ; 32x16 bg2 map at $5000
        lda     #$59
        sta     $2109       ; 32x16 bg3 map at $5800
        bra     @5b55
@5b4b:  lda     #$52
        sta     $2108       ; 16x32 bg2 map at $5000
        lda     #$5a
        sta     $2109       ; 16x32 bg3 map at $5800
@5b55:  ldx     $0ad4
        cpx     #$00be
        bne     @5b62       ; branch if ???
        lda     #$23
        sta     $2109       ; 32x32 bg3 map at $2000
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
@5b7c:  lda     $c05bba,x
        sta     $4a
        sta     $212d       ; sub-screen designation
        lda     $c05bb9,x
        and     #$01
        beq     @5b91
        lda     #$bf
        bra     @5b93
@5b91:  lda     #$bc
@5b93:  sta     $2123       ; window mask settings bg1/bg2
        lda     $c05bb9,x
        ora     #$01        ; bg1 always on
        sta     $212c       ; main screen designation
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
        bra     _5c01

_c05bf8:
        lda     $0ad6       ; map index
        cmp     #$03
        bcs     _5c2c       ; return if underwater
        lda     $6f
_5c01:  lsr
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
        lda     $d13000,x   ; pointer to treasure properties
        and     #$00ff
        asl2
        sta     $23
        lda     $d13001,x   ; pointer to next map's treasure properties
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
        jsr     $ca16       ; get treasure flag
        cmp     #$00
        beq     @5d25       ; skip if treasure hasn't been obtained
        ldx     $23
        lda     $d13211,x
        longa
        asl6
        sta     $0d
        lda     $d13210,x
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
        jsr     $5d87
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
        jsr     $5d87
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
        sta     $2115
        lda     #$01
        sta     $4300
        lda     #$18
        sta     $4301
        lda     #$7f
        sta     $4304
        longa
        lda     #$7622      ; 7f/7622
        sta     $4302
        lda     #$1000
        sta     $4305
        lda     $73
        lsr
        clc
        adc     #$4800
        sta     $2116
        lda     $06
        shorta
        lda     #$01
        sta     $420b
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
        jsr     $5e88
@5e88:  cmp     #$20
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
        stz     $420c
        lda     #$42
        sta     $4330
        sta     $4350
        ldx     #$6d8c
        stx     $4332
        stx     $4352
        lda     #$7f
        sta     $4334
        sta     $4354
        sta     $4337
        sta     $4357
        lda     $53
        bne     @5f33
        lda     #$0d
        sta     $4331
        rts
@5f33:  lda     #$0f
        sta     $4331
        lda     #$11
        sta     $4351
        rts
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
        stz     $420c
        lda     #$42
        sta     $4340
        sta     $4360
        ldx     #$6db7
        stx     $4342
        stx     $4362
        lda     #$7f
        sta     $4344
        sta     $4364
        sta     $4347
        sta     $4367
        lda     $53
        bne     @5fdb
        lda     #$0e
        sta     $4341
        rts
@5fdb:  lda     #$10
        sta     $4341
        lda     #$12
        sta     $4361
        rts

; ---------------------------------------------------------------------------

; [  ]

_c05fe6:
@5fe6:  lda     $169e
        cmp     #$02
        beq     @5ff0
        jmp     @6076
@5ff0:  lda     #$70
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
        jsr     $6f08
        bra     @6075
@6048:  lda     $42
        lsr3
        lda     #$00
        adc     #$29
        sta     $bb
        lda     $42
        cmp     #$30
        bne     @6061
        jsr     $4a71
        lda     #$85
        jsr     $463c       ; play sound effect
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
        jsr     $4a71
        stz     $169e
@6080:  rts

; ---------------------------------------------------------------------------

; [ fade out ]

_c06081:
        lda     $169e
        bne     @6089
        jsr     $4a71
@6089:  stz     $42
@608b:  jsr     WaitVBlank
        lda     $169e
        beq     @60a1
        lda     $42
        bmi     @60a1
        jsr     $5fe6
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
        jsr     $4a68
        stz     $42
        lda     #$81
        sta     $4200
        lda     #$00
        sta     $2100
        cli
@6110:  jsr     WaitVBlank
        lda     $169e
        cmp     #$02
        bne     @6124
        jsr     $60a8
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
        bra     @613c
        ldx     #$0040
        stx     $23
        ldx     #$000f
@613c:  lda     $6f
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
        sta     $2105
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
        lda     $cdfbe0,x
        inx
        stx     $23
        sta     $211b
        stz     $211b
        lda     $08
        sta     $211c
        sta     $211c
        lda     $0ad6       ; map index
        cmp     #$03
        bcs     @6234
        longa
        lda     $2134
        lsr5
        clc
        adc     #$0100
        ldx     $26
        sta     $7f4000,x
        jmp     @6249
@6234:  longa
        lda     $2134
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
        lda     $c0d240,x
        inx
        stx     $23
        plx
        sta     $211b
        stz     $211b
        lda     $26
        sta     $211c
        sta     $211c
        longa
        lda     $2134
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
        stz     $420c
        lda     #$40
        sta     $4370
        lda     #$00
        lda     #$06
        sta     $4371
        ldx     #$6a18
        stx     $4372
        lda     #$7f
        sta     $4374
        sta     $4377
        rts

; ---------------------------------------------------------------------------

; [  ]

_c0630a:
        stz     $211c
        stz     $211c
        stz     $211d
        stz     $211d
        lda     #$f0
        sta     $7f6a00
        sta     $7f6a03
        lda     #$00
        sta     $7f6a06
        stz     $420c
        lda     #$42
        sta     $4350
        sta     $4360
        lda     #$1b
        sta     $4351
        lda     #$1e
        sta     $4361
        ldx     #$6a00
        stx     $4352
        stx     $4362
        lda     #$7f
        sta     $4354
        sta     $4364
        sta     $4357
        sta     $4367
        lda     #$ff
        sta     $7f6a0a
        sta     $7f6a0d
        lda     #$00
        sta     $7f6a10
        stz     $420c
        lda     #$40
        sta     $4320
        lda     #$32
        sta     $4321
        ldx     #$6a07
        stx     $4322
        lda     #$7f
        sta     $4324
        sta     $4327
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
        lda     $c06461,x
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
        lda     $c06461,x
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
        lda     $c06461,x
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
        stz     $2115
        jsr     $6de9
        stz     $4300
        ldx     $7f
        stx     $2116
        ldx     #$16f3
        stx     $4302
        ldx     $7b
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $81
        stx     $2116
        ldx     $7d
        beq     @6494
        stx     $4305
        lda     #$01
        sta     $420b
@6494:  ldx     $83
        stx     $2116
        ldx     #$1773
        stx     $4302
        ldx     $7b
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $85
        stx     $2116
        ldx     $7d
        beq     @64ba
        stx     $4305
        lda     #$01
        sta     $420b
@64ba:  rts

; ---------------------------------------------------------------------------

; [  ]

_c064bb:
        lda     #$03
        sta     $2115
        jsr     $6de9
        stz     $4300
        ldx     $7f
        stx     $2116
        ldx     #$16f3
        stx     $4302
        ldx     $7b
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $81
        stx     $2116
        ldx     $7d
        beq     @64ec
        stx     $4305
        lda     #$01
        sta     $420b
@64ec:  ldx     $83
        stx     $2116
        ldx     #$1773
        stx     $4302
        ldx     $7b
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $85
        stx     $2116
        ldx     $7d
        beq     @6512
        stx     $4305
        lda     #$01
        sta     $420b
@6512:  rts

; ---------------------------------------------------------------------------

; [  ]

_c06513:
        lda     $ba
        bne     @6518
        rts
@6518:  and     #$01
        beq     @6520
        jml     $c065a3
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

showminimap:
@6632:
        jsr     $6081       ; fade out
        jsr     $4ce8       ; disable interrupts
        stz     $5e
        lda     #$07
        sta     $2105
        lda     #$80
        sta     $211a
        lda     #$00
        sta     $211b
        lda     #$08
        sta     $211b
        lda     #$00
        sta     $211e
        lda     #$08
        sta     $211e
        lda     #$c0
        sta     $210d
        lda     #$ff
        sta     $210d
        lda     #$d0
        sta     $210e
        lda     #$ff
        sta     $210e
        lda     #$01
        sta     $211f
        lda     #$00
        sta     $211f
        sta     $2120
        sta     $2120
        lda     #$11
        sta     $212c
        lda     #$e0
        sta     $2132
        stz     $2115
        ldx     $06
        stx     $2116
        stx     $0d
        lda     #$1f
        sta     $76
        stz     $ba
@6696:  jsr     $69a1
        ldy     $06
@669b:  ldx     $26
        lda     $7f0000,x
        tax
        lda     $7f6fa2,x
        sta     $2118
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
        jsr     $4c95       ; clear sprite data
        jsr     $56f8
        jsr     $6831
        lda     #$cf
        sta     $25
        ldx     #$d800
        stx     $23
        ldx     #$7000
        stx     $2e
        ldx     #$0400
        stx     $2c
        jsr     $4cbc       ; copy data to vram
        lda     #$01
        sta     $52
        stz     $3f
        jsr     $6100       ; fade in
@66f9:  jsr     WaitVBlank
        jsr     $6731
        jsr     $6755
        jsr     $67ec
        jsr     $679c
        lda     $02
        and     #$cf
        bne     @66f9
        lda     $03
        bne     @66f9
@6712:  jsr     WaitVBlank
        jsr     $6731
        jsr     $6755
        jsr     $67ec
        jsr     $679c
        lda     $02
        and     #$cf
        bne     @672b
        lda     $03
        beq     @6712
@672b:  jsr     $6081       ; fade out
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
@6755:  lda     #$f8
        jsr     $ca3c       ; get event flag $01xx
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
        lda     $c067e9,x
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
        asl     $1e0f
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
@680a:  lda     $c068f5,x
        sta     $0300,y
        lda     $c068f6,x
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
@6831:  lda     #$0a
        sta     $df
        lda     #$0b
        sta     $e0
        jsr     $baae       ; change color palette
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

_c069a1:
        lda     #$c7
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
        lda     $cfe000,x   ; pointer to world layout
        sta     $23
        cpx     #$0800
        bcc     @69e4
        lda     $cfe000,x
        cmp     #$8000
        bcs     @69e4
        inc     $25
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
        lda     $c06abf,x
        sta     $0d
        lda     $c06abd,x
        tax
        lda     $06
        shorta
@6a67:  cpx     $0d
        beq     @6abc
        lda     $c00000,x   ; y
        cmp     $0f
        beq     @6a7b
@6a73:  inx6
        bra     @6a67
@6a7b:  lda     $c00003,x   ; event bit index
        clc
        adc     #$d0
        jsr     $ca3c       ; get event flag $01xx
        cmp     #$00
        beq     @6a73
        phx
        lda     $c00002,x   ; width
        sta     $2c
        lda     $c00001,x   ; x
        longa
        clc
        adc     $26
        tay
        lda     $c00004,x   ; pointer to modified tilemap data
        tax
        lda     $06
        shorta
        lda     #$7f
        pha
        plb
@6aa7:  lda     $c00000,x   ; copy row of tiles
        sta     $0000,y
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
_c06abd:
        .addr   WorldMod1
        .addr   WorldMod2
        .addr   WorldMod3
        .addr   WorldMod4
        .addr   WorldMod5
        .addr   WorldModEnd
        ; .addr   $726c,$73da,$7518,$7794,$779a,$79e6

@6ac8:  rts

; ---------------------------------------------------------------------------


; [ load map layout ]

; +x: destination address (+$7f0000)
; +y: map layout index

_c06aca:
        stx     $71
        lda     #$cb
        sta     $25
        longa
        ldx     $06
        cpy     #$0000
        beq     @6af3
        inx2
        dey
        beq     @6af3
@6ade:  inx2
        lda     $cb0000,x
        dex2
        cmp     $cb0000,x
        bcs     @6aee
        inc     $25
@6aee:  inx2
        dey
        bne     @6ade
@6af3:  lda     $cb0000,x   ; pointer to map layout
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
        jsl     $c30002     ; decompress
        rts
@6b1b:  ldx     $71
        jsr     $6b21       ; clear tile layout
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
        lda     $cfc540,x   ; pointers to tile properties
        clc
        adc     #$c540      ; cf/c540
        sta     $04f0
        lda     $06
        shorta
        lda     #$cf
        sta     $04f2
        ldx     #$1186      ; 00/1186
        stx     $04f3
        lda     #$00
        sta     $04f5
        jsl     $c30002     ; decompress
        lda     $1114       ; tileset
        asl
        tax
        longa
        lda     $cf0000,x   ; pointers to tilesets
        clc
        adc     #$0000      ; cf/0000
        sta     $04f0
        lda     $06
        shorta
        lda     #$cf
        sta     $04f2
        ldx     #$6e22      ; 7f/6e22
        stx     $04f3
        lda     #$7f
        sta     $04f5
        jsl     $c30002     ; decompress
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
        jsr     $6bac
        lda     $0ad9
        sta     $76
        lda     $0ad8
        sta     $75
        lda     #$40
@6c59:  pha
        jsr     $69a1
        jsr     $6513
        jsr     $6465
        inc     $76
        pla
        dec
        bne     @6c59
        rts

; ---------------------------------------------------------------------------

; [  ]

_c06c6a:
        ldx     $0ad4
        cpx     #$00be
        bne     @6c9c
        stz     $0b6d
        ldx     #$2000
        stx     $2e
        stx     $2c
        jsr     $4d13       ; fill vram
        lda     #$80
        sta     $2115
        ldy     #$2702
        jsr     $6c9d
        ldy     #$2d82
        jsr     $6c9d
        ldy     #$2342
        jsr     $6cd4
        ldy     #$29c2
        jsr     $6cd4
@6c9c:  rts

; ---------------------------------------------------------------------------

; [  ]

_c06c9d:
@6c9d:  ldx     $06
@6c9f:  sty     $2116
@6ca2:  lda     $c0cf00,x
        sta     $2118
        inx
        lda     $c0cf00,x
        sta     $2119
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
@6cd7:  sty     $2116
@6cda:  lda     $c0cf00,x
        sta     $2118
        inx
        lda     $c0cf00,x
        sta     $2119
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
        jsr     $6bdb       ; init bg scroll positions
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
        jsr     $707d
        jsr     $6e7a
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
        jsr     $707d
        jsr     $6e7a
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
        jsr     $707d
        jsr     $6e7a
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
        jsr     $707d
        jsr     $6e7a
@6d89:  lda     $1121
        bmi     @6da5
        lda     $79
        sta     $75
        lda     $0c
        sta     $76
        lda     #$01
        ldx     #$2000
        stx     $71
        jsr     $707d
        jsr     $6e7a
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
        jsr     $707d
        jsr     $6e7a
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
        jsr     $707d
        jsr     $6e7a
@6ddb:  inc     $0a
        inc     $0b
        inc     $0c
        dec     $09
        beq     @6de8
        jmp     @6d20
@6de8:  rts

; ---------------------------------------------------------------------------

; [  ]

_c06de9:
@6de9:  stz     $420b
        lda     #$18
        sta     $4301
        stz     $4304
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
        sta     $2115
        jsr     $6de9
        lda     #$01
        sta     $4300
        longa
        lda     $71
        xba
        asl3
        clc
        adc     #$16f3
        sta     $4302
        lda     $06
        shorta
        ldx     $7f,y
        stx     $2116
        ldx     $7b,y
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $81,y
        stx     $2116
        ldx     $7d,y
        beq     @6e46
        stx     $4305
        lda     #$01
        sta     $420b
@6e46:  longa
        lda     $71
        xba
        asl3
        clc
        adc     #$1733
        sta     $4302
        lda     $06
        shorta
        ldx     $83,y
        stx     $2116
        ldx     $7b,y
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $85,y
        stx     $2116
        ldx     $7d,y
        beq     @6e79
        stx     $4305
        lda     #$01
        sta     $420b
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
        sta     $2115
        jsr     $6de9
        lda     #$01
        sta     $4300
        ldx     $7f,y
        stx     $2116
        longa
        lda     $71
        xba
        asl3
        clc
        adc     #$16f3      ; dma source
        sta     $4302
        lda     $06
        shorta
        ldx     $7b,y
        stx     $4305       ; dma size
        lda     #$01
        sta     $420b
        ldx     $81,y
        stx     $2116
        stz     $420b
        ldx     $7d,y
        beq     @6ece
        stx     $4305
        lda     #$01
        sta     $420b
@6ece:  ldx     $83,y
        stx     $2116
        stz     $420b
        longa
        lda     $71
        xba
        asl3
        clc
        adc     #$1733
        sta     $4302
        lda     $06
        shorta
        ldx     $7b,y
        stx     $4305
        lda     #$01
        sta     $420b
        ldx     $85,y
        stx     $2116
        stz     $420b
        ldx     $7d,y
        beq     @6f07
        stx     $4305
        lda     #$01
        sta     $420b
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
        jsr     $703e
        ldy     $29
        jsr     $703e
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
        beq     @7086
        jmp     @714b
@7086:  lda     $76
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
        jsr     $7221
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
        jsr     $722f
        jsr     $7241
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
        jsr     $7221
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
        jsr     $722f
        jsr     $7241
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
_c083ae:
        jsr     $8401       ; get pointer to dialog
@83b1:  jsr     _c08f54
        ldx     $b1
        lda     $ca0000,x
        beq     @83fa
        jsr     LoadDlgText
        ldy     $06
        sty     $ab
@83c3:  jsr     _c08d3b
        jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     $39b3       ; update object sprites
        jsr     $2842
        ldy     $ab
        cpy     #$0040
        bne     @83c3
        lda     $b8
        bne     @83e6
        jsr     $4aad
        jsr     $4ac1       ; wait for keypress
        jsr     $4aad
@83e6:  stz     $b8
        inc     $a5
        jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     $39b3       ; update object sprites
        jsr     $2842
        lda     $b3
        beq     @83b1
@83fa:  jsr     $94a8
        jsr     WaitVBlank
        rts

; ---------------------------------------------------------------------------

; [ get pointer to dialog ]

GetDlgPtr:
_c08401:
@8401:  stz     $b3         ;
        longa
        lda     $af         ; dialog index
        and     #$7fff
        asl
        tax
        lda     $c82220,x   ; pointer to dialog
        sta     $b1
        lda     $06
        shorta
        lda     $b0
        bmi     @841f
        lda     #$01        ; dialog window at top of screen
        jmp     @8421
@841f:  lda     #$08        ; dialog window at bottom of screen
@8421:  sta     $b4
        jsr     $9440       ;
        rts

; ---------------------------------------------------------------------------

; [ load dialogue text ]

LoadDlgText:
@8427:  ldx     $06
        stx     $ab
_842b:  ldx     $b1
        lda     $ca0000,x
        cmp     #$ff
        beq     _c08451
        cmp     #$cd
        bcc     @843c
        jmp     _c08508
@843c:  cmp     #$20
        bcs     _c08451
        longa
        asl
        tax
        lda     f:_c08b13,x
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
        lda     $ca0000,x
        sta     $19d3,y
        iny
        sty     $ab
        jmp     _c08459

_c08477:
        ldx     $b1
        ldy     $ab
        lda     $ca0000,x
        sta     $19d3,y
        inx
        iny
        lda     $ca0000,x
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
        sbc     #%11

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
_8a73:  lda     $ca0001,x
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
@8a98:  lda     $d11c81,x
        cmp     #$ff
        beq     @8ab2
        sta     $19d3,y
        lda     #$00
        sta     $1a13,y
        iny
        inx
        inc     $09
        lda     $09
        cmp     #$06
        bne     @8a98
@8ab2:  sty     $ab
        jmp     _c08459

; $11: item name
_c08ab7:
        lda     $16a2
        longa
        sta     $0f
        asl3
        clc
        adc     $0f
        tax
        lda     $06
        shorta
        stz     $09
@8acb:  lda     $d11381,x
        cmp     #$ff
        beq     @8ae5
        sta     $19d3,y
        lda     #$00
        sta     $1a13,y
        iny
        inx
        inc     $09
        lda     $09
        cmp     #$08
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
        cpx     #$0007
        bne     @8aee
        sty     $ab
        jmp     _c08459

; escape code jump table ($00-$1F), also includes MTE codes
_c08b13:
        .addr   _c08493,_c084af,_c084e9,_c0850d,_c0850d,_c0850d,_c0850d,_c0850d
        .addr   _c0850d,_c0850d,_c0850d,_c0850d,_c08466,_c0850d,_c0850d,_c08466
        .addr   _c08aea,_c08ab7,_c08a85,_c0850d,_c0850d,_c0850d,_c0850d,_c08477
        .addr   _c0850d,_c0850d,_c0850d,_c0850d,_c0850d,_c0850d,_c08a5f,_c08a6a

; ---------------------------------------------------------------------------

_c08b53:
        lda     #$80
        sta     $2115
        ldx     #$3000
        stx     $2116
        longa
        ldx     #$0000
@8b63:  lda     #$00ff
.repeat 8
        sta     $2118
.endrep
        lda     #$0000
.repeat 8
        sta     $2118
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
        sta     $2115
        stz     $420b
        lda     #$08
        sta     $4300
        lda     #$19
        sta     $4301
        ldx     #$3000
        stx     $2116
        stz     $6d
        ldx     #$0b6d
        stx     $4302
        stz     $4304
        ldx     #$0d00
        stx     $4305
        lda     #$01
        sta     $420b
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
        sta     $4310
        lda     #$28
        sta     $4311
        ldx     #$6a8a
        stx     $4312
        lda     #$7f
        sta     $4314
        sta     $4317
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
        sta     $4320
        lda     #$25
        sta     $4321
        ldx     #$6abb
        stx     $4322
        lda     #$7f
        sta     $4324
        sta     $4327
        rts

; ---------------------------------------------------------------------------

; [  ]

_c08d3b:
        jsr     _c08e08
@8d3e:  ldy     $ab
        lda     $1a13,y
        beq     @8d48
        jmp     $8de1
@8d48:  lda     $19d3,y
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
        jsr     $39b3       ; update object sprites
        jsr     $2842
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
        jmp     $8e07
@8d99:  cmp     #$01
        bne     @8de1
@8d9d:  lda     $a8
        beq     @8dba
        inc     $a7
        jsr     WaitVBlank
        jsr     UpdatePlayerSprite
        jsr     $39b3       ; update object sprites
        jsr     $2842
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
        jmp     $8d3e
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
        bcs     @8e01
        jmp     $8d3e
@8e01:  and     #$0f
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
        sta     $2115
        stz     $420b
        lda     #$00
        sta     $4300
        lda     #$19
        sta     $4301
        lda     #$00
        sta     $4304
        longa
        lda     $a9
        clc
        adc     #$0002
        sta     $a9
        sta     $2116
        lda     $06
        shorta
        ldx     #$19a3
        stx     $4302
        ldx     #$0006
        stx     $4305
        lda     #$01
        sta     $420b
        longa
        lda     $a9
        clc
        adc     #$000e
        sta     $a9
        sta     $2116
        lda     $06
        shorta
        ldx     #$19a9
        stx     $4302
        ldx     #$0006
        stx     $4305
        lda     #$01
        sta     $420b
        longa
        lda     $a9
        clc
        adc     #$0012
        sta     $a9
        sta     $2116
        lda     $06
        shorta
        ldx     #$19af
        stx     $4302
        ldx     #$0006
        stx     $4305
        lda     #$01
        sta     $420b
        longa
        lda     $a9
        clc
        adc     #$000e
        sta     $a9
        sta     $2116
        lda     $06
        shorta
        ldx     #$19b5
        stx     $4302
        ldx     #$0006
        stx     $4305
        lda     #$01
        sta     $420b
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

; [  ]

_c08f01:
@8f01:  longa
        cmp     #$0100
        bcs     @8f2c
        asl3
        sta     $0d
        asl
        clc
        adc     $0d
        tax
        lda     $06
        shorta
        ldy     $06
@8f18:  lda     $c3e800,x
        sta     $1973,y
        lda     #$00
        sta     $198b,y
        inx
        iny
        cpy     #$0018
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
@8f40:  lda     $dbd000,x
        sta     $1973,y
        lda     #$00
        sta     $198b,y
        inx
        iny
        cpy     #$0018
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
        sta     $2115
        lda     #$04
        sta     $19
        ldy     $1698
        sty     $1b
@8f91:  ldy     $1b
        sty     $2116
        lda     f:_c08fdd,x
        sta     $2118
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
        sta     $2116
@8fb2:  lda     $06
        shorta
        lda     f:_c08fdd,x
        sta     $2118
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
        jsr     $707d
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
        lda     $c09101,x
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
        jsr     $707d
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

.proc RandomBattle

_ccf0:  .a8
        .i16
        ldx     $06
        stx     $16a8
        lda     #$6f
        jsr     $463c
        jsr     $ccdd
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #0
        sta     hNMITIMEN
        lda     #$80
        sta     hINIDISP
        sei
        jsl     ExecBattle_ext
        jsr     $44e3
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
        jsr     $454f
_cd46:  jsr     UpdateTopChar
        rts

.endproc

; ===========================================================================

.segment "reset"

; ---------------------------------------------------------------------------

; [ reset ]

.proc Reset

_cec0:  sei
        clc
        xce
        jml     Start

.endproc

; ===========================================================================

.segment "nmi_irq"

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

; ===========================================================================

; c0/f8ef
SRAMSlotTbl:
        .addr   $6000
        .addr   $6700
        .addr   $6e00
        .addr   $7500

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
