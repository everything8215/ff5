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
        stx     $00
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
        jsr     $5476                   ; reload map
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
        jsr     $14e9                   ; pixelate screen (poison)
        jmp     _00ad
_0147:  jsr     CheckTriggers
        jsr     $a18b                   ; update timer
        lda     $58
        beq     _0156
        stz     $58
        jmp     _00ad
_0156:  lda     $6e
        beq     _0160
        jsr     $545a                   ; load map
        jmp     _00ad
_0160:  ldx     $0ad6                   ; map index
        cpx     #$0005
        bcc     _016b
        jmp     _0262
_016b:  jsr     _c0073e
        lda     $58
        beq     _0177
        stz     $58
        jmp     $00ad
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
        jmp     $00ad
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
        jsr     $5476                   ; reload map
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
        jsr     $5476                   ; reload map
        jmp     _00ad
_0246:  jsr     $0f8c
        jsr     $1a1d
        jsr     $4c95                   ; clear sprite data
        jsr     $2137
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
        jsr     $548f       ; load parent map
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
        jsr     $5476       ; reload map
        jmp     _00ad
_029a:  stz     $55
        jsr     $32ab       ; update objects
        jsr     c0_11c2
        lda     $6e
        beq     _02ac
        jsr     $545a       ; load map
        jmp     _00ad
_02ac:  lda     $58
        beq     _02b5
        stz     $58
        jmp     _00ad
_02b5:  jsr     $1ae4
        jsr     $3bac
        jsr     $4c95       ; clear sprite data
        jsr     $4834
        jsr     UpdatePlayerSprite
        jsr     $39b3       ; update object sprites
        jsr     $2842
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
        beq     _031b
        jmp     _041f
_031b:  jsr     $4bc0       ; update scrolling registers
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
        jsr     $1d1e       ; copy party sprite graphics to vram
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
        jsr     $8ba4
        jmp     _0408
_0378:  lda     $a6
        beq     _038b
        stz     $a6
        ldx     $00
        stx     $71
        jsr     $6e7a
        jsr     $8be4
        jmp     _03f9
_038b:  lda     $a7
        beq     _0392
        jsr     $8e23
_0392:  jsr     $8f78       ; draw yes/no indicator
        lda     $9f
        beq     _03ed
        stz     $9f
        lda     $70
        and     #$01
        bne     _03c7
        ldx     $00
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
_03c7:  ldx     $00
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
        jsr     $1d1e       ; copy party sprite graphics to vram
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
        lda     $00
        shorta
_047c:  ldx     $23
        lda     $d8e080,x
        cmp     #$ff                    ; $ff: execute event
        bne     _0489
        jmp     _05fc
_0489:  cmp     #$fe                    ; $fe: if event flag $00xx is set
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
        beq     _04d1
        jmp     _05e5
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
        lda     $00
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
        lda     $00
        shorta
        jmp     _05e5
_0543:  cmp     #$00
        rti
        bne     _0558
        lda     $0500,y
        cmp     $d8e083,x
        bcs     _0568
        lda     $00
        shorta
        jmp     _05e5
_0558:  lda     $0500,y
        cmp     $d8e083,x
        bcc     _0568
        lda     $00
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
        lda     $00
        shorta
        lda     $0500,y
        and     $d8e083,x
        bne     _0568
        jmp     _05e5
        lda     $00
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
        lda     $00
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
        lda     $00
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
_066a:  lda     $00
        shorta
        lda     #$01
        sta     $5a
        longa
        lda     $ce2402,x
        asl
        tax
        lda     $00
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
        bne     _06a2
        jmp     _0739
_06a2:  lda     $0ad8
        cmp     $ce36c0,x
        beq     _06b4
        txa
        clc
        adc     #$0006
        tax
        jmp     _069b
_06b4:  lda     $59
        and     #$00ff
        beq     _06be
        jmp     _0739
_06be:  stz     $16a0                   ; disable map title
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
        lda     $00
        shorta
        lda     $ce36c4,x
        sta     $1088
        lda     $ce36c5,x
        sta     $1089
        bra     _0737
_06f8:  lda     $00
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
_0739:  lda     $00
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
_074b:  jsr     $1733                   ; update local tile properties (world map)
        lda     $0adc
        dec
        asl2
        sta     $169c
        lda     $0adc
        beq     _075f
        jmp     _07ee
_075f:  ldy     $00
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
        beq     _07ed
        jmp     _0763
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
        ldy     $00
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
_08c5:  jsr     $4e41       ; wait for vblank
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y     ; vehicle height
        jsr     $2137
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
_08f3:  jsr     $4e41       ; wait for vblank
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     $2137
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
_0926:  jsr     $4e41       ; wait for vblank
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     $2137
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
_095e:  jsr     $4e41       ; wait for vblank
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f,x
        ora     #$80
        sta     $0ade,y
        jsr     $2137
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
_09b9:  jsr     $09f7
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
_09cc:  jsr     $4e41       ; wait for vblank
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
        jsr     $2137
        stz     $0289
        stz     $028d
        lda     #$08
        sta     $0291
        sta     $0295
        lda     #$fd
        sta     $3d
        stz     $0d
_0a2e:  jsr     $4e41       ; wait for vblank
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
        jsr     $046a       ; execute trigger script
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
_0a5e:  jsr     $4e41       ; wait for vblank
        jsr     $5bf8
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
_0a8e:  jsr     $2137
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
        bne     _0ab5
        jmp     _0bd1
_0ab5:  dec
        bne     _0ac5
        jsr     $09f7
        ldx     #$002c
        jsr     $046a       ; execute trigger script
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
        jsr     $4e41       ; wait for vblank
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
_0b1a:  jsr     $4e41       ; wait for vblank
        jsr     $2137
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
_0b60:  jsr     $4e41       ; wait for vblank
        jsr     $5bf8
        jsr     $4c95       ; clear sprite data
        ldy     $169c
        lda     $3d
        tax
        lda     f:_c00c4f+16,x
        sta     $6f
        asl
        ora     #$80
        sta     $0ade,y
        jsr     $2137
        jsr     $612b
        jsr     _c01e64
        jsr     $5bf8
        dec     $3d
        lda     $3d
        cmp     #$ff
        bne     _0b60
        lda     $10fa       ; world tile properties byte 1
        and     #$10
        beq     _0bc8
        lda     #$01
        sta     $5b
        jsr     $46c4
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
        jsr     $2137
        jsr     $4e41       ; wait for vblank
        jsr     $4583
        jsr     $4e41       ; wait for vblank
        rts
_0bc8:  stz     $0adc
        jsr     $4e41       ; wait for vblank
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
        jsr     $09f7
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
_0ca2:  jsr     $4e41
        jsr     $4c95       ; clear sprite data
        jsr     $2137
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
_0d39:  jmp     $0ca2
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
        jsr     $17e8       ; update local tile properties (normal map)
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
_0d63:  jsr     $317b       ; check npc events
        lda     $58
        bne     _0d73       ; branch if an event is running
        lda     $02
        and     #$80
        beq     _0d73       ; branch if the a button is not down
        jsr     $0d74       ; check treasure chests
_0d73:  rts

.endproc

; ---------------------------------------------------------------------------

; [ check treasure chests ]

.proc CheckTreasure

_0d72:  longa
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
        lda     $00
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
        lda     $00
        shorta
        lda     $7f0000,x
        cmp     #$02
        bne     _0e2e
        lda     #$8e
        jsr     $463c                   ; play sound effect
        ldx     $00
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
        bpl     _0e47
        jmp     _0ebc
_0e47:  and     #$e0                    ; gp
        bne     _0e63
        jsr     $0f3d                   ; calculate gp (chest)
        jsr     $0efe                   ; give gp
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
_0e98:  ldy     $00
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
_0ee4:  jsr     $5476                   ; reload map
        plx
        stx     $11
        lda     $09c4
        and     #$02
        bne     _0efd                   ; branch if party ran away
        lda     $11
        and     #$40
        bne     _0efa
        jmp     _0e7e                   ; item
_0efa:  jmp     $0e67                   ; spell
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
        lda     $00
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

.proc c0_0f8c

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
_0fa7:  jsr     $104a
        lda     $03
        and     #$0f
        bne     _0fb3
        jmp     _1003
_0fb3:  lda     $03
        and     #$08
        beq     _0fc7
        lda     #$01
        sta     $c4
        jsr     $1055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fc7:  lda     $03
        and     #$01
        beq     _0fdb
        lda     #$02
        sta     $c4
        jsr     $1055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fdb:  lda     $03
        and     #$04
        beq     _0fef
        lda     #$03
        sta     $c4
        jsr     $1055
        lda     $c4
        beq     _1003
        jmp     _1006
_0fef:  lda     $03
        and     #$02
        beq     _1006
        lda     #$04
        sta     $c4
        jsr     $1055
        lda     $c4
        beq     _1003
        jmp     _1006
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
        jsr     $148d       ; poison damage
_102c:  jsr     $103a
        lda     #$fc
        jsr     $c796
        lda     #$fd
        jsr     $c796
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc c0_103a

_103a:  lda     $ca
        beq     _1049
        lda     $ba         ; movement direction
        tax
        lda     f:_11b8,x   ; pointer to tile in  movement direction
        tax
        jsr     $119a       ; get world tile transparent/underwater flags
_1049:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc c0_104a

_104a:  lda     $ca
        bne     _1054
        ldx     #$0008      ; tile at party location
        jsr     $119a       ; get world tile transparent/underwater flags
_1054:  rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc c0_1055

_1055:  lda     $c4         ; movement direction
        dec
        sta     $0adb       ; party facing direction
        lda     $57
        bne     _10c3       ; return if an event is running
        jsr     $112d       ; check if there is a vehicle at the current position
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
_10c4:  jsr     $10c8
        rts

.endproc

; ---------------------------------------------------------------------------

; [  ]

.proc c0_10c8

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
        jsr     $4e41       ; wait for vblank
        rts
_112a:  stz     $c4
        rts

.endproc

; ---------------------------------------------------------------------------

; [ check if there is a vehicle at the current position ]

; return 0 if not, 1 if there is a vehicle
; $23 is a pointer to vehicle data

.proc c0_112d

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
        ldy     $00
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

.proc c0_119a

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
_11df:  jsr     $151d       ; update party sprite priority (current tile)
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
        jsr     $13a6       ; update party z-level (current tile)
        jsr     $16b7
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
        ldx     $00
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
        lda     $00
        shorta
        cpx     #$0140
        bne     _1257
        bra     _127a
_1278:  asl     $c0         ; double speed
_127a:  lda     $1120
        jsr     $161f       ; calculate parallax scroll
        sta     $1080
        stz     $1081
        lda     $1120
        lsr2
        jsr     $161f       ; calculate parallax scroll
        sta     $1082
        stz     $1083
        lda     $1120
        lsr4
        jsr     $161f       ; calculate parallax scroll
        sta     $1084
        stz     $1085
        lda     $1120
        lsr6
        jsr     $161f       ; calculate parallax scroll
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
        jsr     $13d7       ; tile damage
        jsr     $148d       ; poison damage
_12d8:  lda     $03
        and     #$0f
        bne     _12e1       ; branch if any direction buttons are pressed
        jmp     _1331       ; no move
_12e1:  lda     $03         ; check up button
        and     #$08
        beq     _12f5
        lda     #$01        ; up
        sta     $c4
        jsr     $163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_12f5:  lda     $03         ; check right button
        and     #$01
        beq     _1309
        lda     #$02        ; right
        sta     $c4
        jsr     $163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_1309:  lda     $03         ; check down button
        and     #$04
        beq     _131d
        lda     #$03        ; down
        sta     $c4
        jsr     $163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_131d:  lda     $03         ; check left button
        and     #$02
        beq     _1336
        lda     #$04        ; left
        sta     $c4
        jsr     $163a       ; check if party can move
        lda     $c4
        beq     _1331
        jmp     _1336
_1331:  stz     $ba         ; no move
        jmp     _1367

; start move
_1336:  sta     $ba         ; set facing/moving direction
        sta     $bf
        lda     #$01
        sta     $5d
        stz     $59
        jsr     $1372       ; update party z-level (destination tile)
        lda     $57
        bne     _134e       ; branch if an event is running
        lda     $10fb       ; map tile properties byte 2
        and     #$40
        bne     _1354       ; branch if forced facing direction
_134e:  lda     $ba
        dec
        sta     $0adb       ; set facing direction
_1354:  jsr     $1538       ; update party sprite priority (destination tile)
        lda     #$fc
        jsr     $c796       ;
        lda     #$fd
        jsr     $c796       ;
        jsr     $155b
        jsr     $15ac
_1367:  jsr     $2973
        lda     $58
        bne     _1371
        jsr     $16da
_1371:  rts

.endproc

; ---------------------------------------------------------------------------

; [ update party z-level (destination tile) ]

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

; ---------------------------------------------------------------------------

; [ update party z-level (current tile) ]

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

; ---------------------------------------------------------------------------

; [ tile damage ]

_13d7:  ldx     $00
_13d9:  lda     $0500,x
        and     #$40
        bne     _13ea       ; skip empty character slots
        lda     $0520,x
        and     #$04
        beq     _13ea
        jmp     _1474       ; no damage if any character has floor damage ability
_13ea:  longa
        txa                 ; next character
        clc
        adc     #$0050
        tax
        lda     $00
        shorta
        cpx     #$0140
        bne     _13d9
        ldy     $00
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
        sbc     f:_c0147d,x   ; subtract hp
        beq     _1439
        bcs     _143c
_1439:  lda     #$0001
_143c:  sta     $0506,y
        lda     $00
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
        lda     f:_c01475-1,x
        jsr     $463c       ; play sound effect
_1463:  longa
        tya
        clc
        adc     #$0050
        tay
        lda     $00
        shorta
        cpy     #$0140
        bne     _13fd
_1474:  rts

; ---------------------------------------------------------------------------

; tile damage sound effect
_c01475:
@1475:  .byte   $94, $94, $94, $94, $94, $94, $94, $94

; tile damage values (0, 50, 50, 100, 300, 400, 500, 1000)
_c0147d:
@147d:  .word   $0000, $0032, $0032, $0064, $012c, $0190, $01f4, $03e8

; ---------------------------------------------------------------------------

; [ poison damage ]

_148d:  lda     $57
        bne     _14e8       ; return if an event is running
        ldy     $00
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
        lda     $00
        shorta
_14d7:  longa
        tya
        clc
        adc     #$0050
        tay
        lda     $00
        shorta
        cpy     #$0140
        bne     _1493
_14e8:  rts

; ---------------------------------------------------------------------------

; [ pixelate screen (poison) ]

_14e9:  ldy     $00
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
        lda     $00
        shorta
        cpy     #$0140
        bne     _14eb
        rts

; ---------------------------------------------------------------------------

; [ update party sprite priority (current tile) ]

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

; ---------------------------------------------------------------------------

; [ update party sprite priority (destination tile) ]

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

; ---------------------------------------------------------------------------

; [  ]

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

; ---------------------------------------------------------------------------

; [  ]

_15ac:  ldx     $00
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
        lda     $00
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

_161f:  and     #$03
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

; ---------------------------------------------------------------------------

; [ check if party can move ]

; $c4: direction (none, up, right, down, left)

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

; ---------------------------------------------------------------------------

; [  ]

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
        jsr     $3cbb       ; get pointer to object layout
        lda     #$00
        sta     $7f3000,x
        plx
_16d9:  rts

; ---------------------------------------------------------------------------

; [  ]

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
        jsr     $3cbb       ; get pointer to object layout
        lda     #$ff
        sta     $7f3000,x
        plx
_1711:  rts

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
        sta     $10c8
        asl
        clc
        adc     $10c8
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

; ---------------------------------------------------------------------------

; [ update local tiles (normal map) ]

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
        sta     $10c8
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

; ---------------------------------------------------------------------------

; [ get tile properties at current tile (unused ???) ]

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

; ---------------------------------------------------------------------------

; [  ]

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

; ---------------------------------------------------------------------------

; [ update showing character ]

_1cd7:  lda     $0ada
        cmp     #$07
        bcs     _1ce1       ; branch if a vehicle
        jsr     $1ce2       ; update party graphic
_1ce1:  rts

; ---------------------------------------------------------------------------

; [ update party graphic ]

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

; ---------------------------------------------------------------------------

; [ copy party sprite graphics to vram ]

_1d1e:  lda     $0ada       ; party graphic
        cmp     #$09
        bcc     _1d28
        jmp     _1dc3       ; vehicle graphic

; normal graphic
_1d28:  lda     #$80
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

_c01ec5:
_1ec5:  ldy     $00
        sty     $23
_1ec9:  ldy     $23
        lda     $6f
        beq     _1ed2
        jmp     _1f48
_1ed2:  lda     $0ade,y
        bpl     _1eda
        jmp     _1f48
_1eda:  lda     $0add,y
        lsr5
        cmp     $0ad6       ; map index
        beq     _1eea
        jmp     _1f48
_1eea:  lda     $0adf,y
        clc
        adc     f:_c0205f
        sec
        sbc     $0ad8
        cmp     f:_c0205f+1
        bcs     _1f48
        jsr     $201f
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
        jsr     $1fb4
        bra     _1f48
_1f45:  jsr     $1f57
_1f48:  lda     $23
        clc
        adc     #$04
        sta     $23
        cmp     #$18
        beq     _1f56
        jmp     _1ec9
_1f56:  rts

; ---------------------------------------------------------------------------

; [  ]

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

; ---------------------------------------------------------------------------

; [  ]

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
        asl
        asl
        tay
        lda     $61
        clc
        adc     $63
        lsr
        lsr
        lsr
        lsr
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
        bcc     _2190
        jmp     _2246
_2190:  stz     $11
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
_22fd:  jsr     $4e41
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
        beq     _2635
        jmp     _2594
_2635:  rts

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
        jmp     $2a1c
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
        jmp     $2a1c
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
        beq     @2a54
        jmp     $298a
@2a54:  rts

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

.proc WaitVBlank

_4e41:  stz     $51
_4e43:  lda     $51
        beq     _4e43
        stz     $51
        rts

.endproc

; ---------------------------------------------------------------------------

.proc InitInterrupts

_4e4a:  .a8
        .i16
        lda     #$5c
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

_4e69:  .a8
        lda     #$80
        sta     hINIDISP
        lda     #0
        sta     hNMITIMEN
        stz     hMDMAEN
        stz     hHDMAEN
        lda     #3
        sta     hOBJSEL
        stz     hOAMADDL
        stz     hOAMADDH

        rts

.endproc

; ---------------------------------------------------------------------------

_world_mod_id .set 0

.mac make_world_mod_header i
        .byte .ident(.sprintf("WORLD_MOD_Y_%04x", i))
        .byte .ident(.sprintf("WORLD_MOD_X_%04x", i))
        .byte .ident(.sprintf("WORLD_MOD_SIZE_%04x", i))
        .byte .ident(.sprintf("WORLD_MOD_SWITCH_%04x", i))
        .addr .ident(.sprintf("WorldModTiles_%04x", i))
.endmac

.mac make_world_mod_xy xx, yy
        .ident(.sprintf("WORLD_MOD_X_%04x", _world_mod_id)) = xx
        .ident(.sprintf("WORLD_MOD_Y_%04x", _world_mod_id)) = yy
.endmac

.mac make_world_mod xy_pos, switch_id, tiles
        make_world_mod_xy xy_pos
        .ident(.sprintf("WORLD_MOD_SIZE_%04x", _world_mod_id)) = (.tcount(tiles) + 1) / 2
        .ident(.sprintf("WORLD_MOD_SWITCH_%04x", _world_mod_id)) = switch_id - $01d0
        .ident(.sprintf("WorldModTiles_%04x", _world_mod_id)) := *
        .byte tiles
        _world_mod_id .set _world_mod_id + 1
.endmac

WorldMod1:
.repeat $3c, i
        make_world_mod_header i
.endrep

WorldMod2:
.repeat $2e, i
        make_world_mod_header i + $3c
.endrep

; world 0 tiles
        make_world_mod {175, 142}, $01e0, {$9d}
        make_world_mod {175, 143}, $01e0, {$ad}
        make_world_mod {175, 144}, $01e0, {$bd}

        make_world_mod {164, 115}, $01e1,     {$27,$9f}
        make_world_mod {163, 116}, $01e1, {$18,$05,$05,$16}
        make_world_mod {163, 117}, $01e1, {$28,$05,$06}
        make_world_mod {163, 118}, $01e1, {$05,$05,$16}
        make_world_mod {163, 119}, $01e1, {$07,$07}

        make_world_mod {204,  84}, $01e2, {$9d}
        make_world_mod {204,  85}, $01e2, {$ad}
        make_world_mod {204,  86}, $01e2, {$ad}
        make_world_mod {204,  87}, $01e2, {$bd}

        make_world_mod {189,  81}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {189,  82}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$33,$54,$50,$33}
        make_world_mod {189,  83}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$05,$53,$51,$05}
        make_world_mod {189,  84}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$06,$07,$07,$08}
        make_world_mod {189,  85}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$06,$17,$17,$17,$18}
        make_world_mod {189,  86}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$70,$71,$26,$27,$27,$27,$28}
        make_world_mod {189,  87}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$70,$43,$43,$71,$05,$05}
        make_world_mod {189,  88}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$70,$43,$43,$71}
        make_world_mod {189,  89}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$60}
        make_world_mod {189,  90}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$70}
        make_world_mod {189,  91}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {189,  92}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {189,  93}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {189,  94}, $01e3, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}

        make_world_mod { 82,  76}, $01e4, {$05,$a1,$a0}
        make_world_mod { 82,  77}, $01e4, {$05,$a2,$05}

        make_world_mod {205, 202}, $01e5, {$81}

        make_world_mod { 63, 156}, $01e6,                         {$26}
        make_world_mod { 60, 157}, $01e6,             {$28,$05,$05,$05,$26,$27,$27,$27,$17,$17,$17}
        make_world_mod { 59, 158}, $01e6,         {$28,$05,$05,$a0,$79,$7d,$05,$a2,$05,$26,$17,$17}
        make_world_mod { 58, 159}, $01e6,     {$18,$05,$05,$79,$7b,$7a,$9c,$a1,$79,$7d,$05,$16,$17}
        make_world_mod { 57, 160}, $01e6, {$17,$18,$05,$a2,$89,$8a,$95,$7c,$7b,$7a,$9c,$05,$16}
        make_world_mod { 57, 161}, $01e6, {$17,$28,$05,$79,$7b,$7a,$95,$95,$95,$95,$7c,$7d,$26}
        make_world_mod { 57, 162}, $01e6, {$18,$05,$05,$89,$8a,$95,$95,$95,$95,$95,$8c,$8d,$05,$16}
        make_world_mod { 57, 163}, $01e6, {$18,$05,$05,$a0,$9b,$95,$95,$95,$95,$95,$7c,$7d,$05,$16,$17,$17}
        make_world_mod { 57, 164}, $01e6, {$17,$08,$05,$79,$7a,$95,$95,$95,$95,$8c,$8b,$8d,$05,$16,$17,$17}
        make_world_mod { 57, 165}, $01e6, {$17,$18,$05,$89,$8b,$8a,$8c,$8b,$8a,$9c,$a2,$05,$06,$17,$17,$17}
        make_world_mod { 57, 166}, $01e6, {$17,$17,$08,$05,$a1,$89,$8d,$a0,$89,$8d,$05,$05,$16}
        make_world_mod { 58, 167}, $01e6,     {$17,$17,$07,$07,$08,$05,$05,$05,$06,$07,$07,$17}
        make_world_mod { 58, 168}, $01e6,     {$17,$17,$17,$17,$17,$07,$07,$07,$17,$17,$17,$17,$17}
        make_world_mod { 58, 169}, $01e6,     {$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17}
        make_world_mod { 62, 170}, $01e6,                     {$17,$17,$17,$17,$17,$17,$17,$17,$17}

        make_world_mod { 60, 158}, $01e7, {$79,$7b,$7b,$7b}
        make_world_mod { 60, 159}, $01e7, {$9b,$8c,$8b,$8a}
        make_world_mod { 60, 160}, $01e7, {$9b,$9c,$9d,$9b}
        make_world_mod { 60, 161}, $01e7, {$9b,$7c,$7b,$7a}

        make_world_mod {146, 115}, $01ef, {$97}

        make_world_mod {147,  78}, $01db,     {$11}
        make_world_mod {146,  79}, $01db, {$11,$11,$11}
        make_world_mod {147,  80}, $01db,     {$11}

        make_world_mod {151,  78}, $01dc,     {$11}
        make_world_mod {150,  79}, $01dc, {$11,$11,$11}
        make_world_mod {151,  80}, $01dc,     {$11}

        make_world_mod {151,  82}, $01dd,     {$11}
        make_world_mod {150,  83}, $01dd, {$11,$11,$11}
        make_world_mod {151,  84}, $01dd,     {$11}

        make_world_mod {147,  82}, $01de,     {$11}
        make_world_mod {146,  83}, $01de, {$11,$11,$11}
        make_world_mod {147,  84}, $01de,     {$11}

; world 1 tiles
        make_world_mod {112,  95}, $01e8,                     {$87,$87}
        make_world_mod {110,  96}, $01e8,             {$87,$87,$87,$87,$87}
        make_world_mod {107,  97}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {107,  98}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {107,  99}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {107, 100}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {107, 101}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {107, 102}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {107, 103}, $01e8, {$87,$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {108, 104}, $01e8,     {$87,$87,$87,$87,$87,$87,$87,$87}
        make_world_mod {109, 105}, $01e8,         {$87,$87,$87,$87,$87,$87}
        make_world_mod {110, 106}, $01e8,             {$87,$87,$87}
        make_world_mod {111, 107}, $01e8,                 {$87,$87}

        make_world_mod {178, 160}, $01e9,         {$84}
        make_world_mod {176, 161}, $01e9, {$84,$87,$87,$87,$84}
        make_world_mod {177, 162}, $01e9,     {$84,$84,$87,$84}
        make_world_mod {177, 163}, $01e9,     {$85,$86,$84,$87,$84}
        make_world_mod {176, 164}, $01e9, {$84,$87,$84,$87,$84,$84}
        make_world_mod {177, 165}, $01e9,     {$84,$87,$84}

        make_world_mod {167, 160}, $01ea, {$05}
        make_world_mod {167, 161}, $01ea, {$05}

        make_world_mod {178, 160}, $01eb,         {$87}
        make_world_mod {176, 161}, $01eb, {$87,$87,$87,$87,$87}
        make_world_mod {177, 162}, $01eb,     {$87,$87,$87,$87}
        make_world_mod {177, 163}, $01eb,     {$87,$87,$87,$87,$87}
        make_world_mod {176, 164}, $01eb, {$87,$87,$87,$87,$87,$87}
        make_world_mod {177, 165}, $01eb,     {$87,$87,$87}

        make_world_mod { 47, 119}, $01ec,                                                 {$21,$21,$21,$21,$21,$21,$21,$21}
        make_world_mod { 45, 120}, $01ec,                                             {$21,$22,$79,$7b,$7b,$7b,$7b,$7b,$7b,$7d}
        make_world_mod { 41, 121}, $01ec,                             {$21,$21,$21,$22,$79,$7b,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$9c}
        make_world_mod { 40, 122}, $01ec,                         {$12,$79,$7b,$7b,$7b,$7a,$a5,$a5,$8c,$8b,$8b,$8b,$8b,$8b,$8d}
        make_world_mod { 40, 123}, $01ec,                         {$12,$9b,$a5,$a5,$6e,$a5,$a5,$a5,$8d,$00,$01,$01,$01,$01,$01}
        make_world_mod { 40, 124}, $01ec,                         {$12,$9b,$6e,$a5,$8c,$8b,$8b,$8d,$00}
        make_world_mod { 40, 125}, $01ec,                         {$22,$9b,$a5,$8c,$8d,$00,$01,$01}
        make_world_mod { 34, 126}, $01ec, {$20,$21,$21,$21,$21,$22,$79,$7a,$a5,$7c,$7d,$20,$21,$21,$21,$21,$21}
        make_world_mod { 34, 127}, $01ec, {$79,$7b,$7b,$7b,$7b,$7b,$7a,$a5,$a5,$a5,$7c,$7b,$7b,$7b,$7b,$7b,$7d,$20}
        make_world_mod { 34, 128}, $01ec, {$89,$8a,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$a5,$6e,$7c,$7d}
        make_world_mod { 35, 129}, $01ec,     {$89,$8b,$8b,$8a,$a5,$a5,$6e,$a5,$a5,$a5,$6e,$a5,$6e,$a5,$a5,$8c,$8d}
        make_world_mod { 38, 130}, $01ec,                 {$89,$8b,$8a,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$8c,$8d}
        make_world_mod { 40, 131}, $01ec,                         {$9b,$a5,$6e,$a5,$a5,$a5,$8c,$8b,$8b,$8d}
        make_world_mod { 40, 132}, $01ec,                         {$89,$8a,$a5,$a5,$8c,$8b,$8d}
        make_world_mod { 41, 133}, $01ec,                             {$89,$8b,$8b,$8d}
        make_world_mod { 46, 134}, $01ec,                                                 {$05,$09}
        make_world_mod { 45, 135}, $01ec,                                             {$0b,$05,$19}
        make_world_mod { 45, 136}, $01ec,                                             {$1b,$05,$29}

; Group 018 - Event Flag $1ED (493)
;                                                           x=213
; 106 $C074E8:  x=213  y= 36    9 at $C07CF7 to $C07D00 -> ($) 11 05 05 11 11 11 05 05 11
; 107 $C074EE:  x=213  y= 37    9 at $C07D00 to $C07D09 -> ($) 05 11 11 05 05 05 11 11 05
; 108 $C074F4:  x=213  y= 38    9 at $C07D09 to $C07D12 -> ($) 05 11 05 11 11 11 05 11 05
; 109 $C074FA:  x=213  y= 39    9 at $C07D12 to $C07D1B -> ($) 05 11 11 11 11 11 11 11 05
; 110 $C07500:  x=213  y= 40    9 at $C07D1B to $C07D24 -> ($) 05 11 11 05 11 05 11 11 05
; 111 $C07506:  x=213  y= 41    9 at $C07D24 to $C07D2D -> ($) 05 11 11 05 11 05 11 11 05
; 112 $C0750C:  x=213  y= 42    9 at $C07D2D to $C07D36 -> ($) 05 11 11 11 05 11 11 11 05
; 113 $C07512:  x=213  y= 43    9 at $C07D36 to $C07D3F -> ($) 11 05 11 11 11 11 11 05 11


; Map 2 replacements - from $C07518 to $C07794 (106.0 entries)

; Group 019 - Event Flag $1F0 (496)
; 114 $C07518:  x=161  y=150    1 at $C07D3F to $C07D40 -> ($) 2A

; Group 020 - Event Flag $1F2 (498)
;                                                           x=205
; 115 $C0751E:  x=205  y=198    3 at $C07D40 to $C07D43 -> ($) 05 05 05
; 116 $C07524:  x=205  y=199    3 at $C07D44 to $C07D47 -> ($) 73 43 71
; 117 $C0752A:  x=205  y=200    3 at $C07D47 to $C07D4A -> ($) 64 81 60
; 118 $C07530:  x=205  y=201    3 at $C07D4A to $C07D4D -> ($) 53 33 51

; Group 021 - Event Flag $1F3 (499)
;                                                           x=138
; 119 $C07536:  x=145  y=112    9 at $C07D57 to $C07D60 -> ($)                      73 74 87 87 87 87 87 87 87
; 120 $C0753C:  x=138  y=113   16 at $C07D60 to $C07D70 -> ($) 87 87 87 87 87 87 70 74 87 87 87 87 87 87 87 87
; 121 $C07542:  x=138  y=114   16 at $C07D70 to $C07D80 -> ($) 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87
; 122 $C07548:  x=138  y=115   16 at $C07D70 to $C07D80 -> ($) 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87
; 123 $C0754E:  x=138  y=116   16 at $C07D70 to $C07D80 -> ($) 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87
; 124 $C07554:  x=138  y=117   16 at $C07D70 to $C07D80 -> ($) 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87
; 125 $C0755A:  x=138  y=118   16 at $C07D70 to $C07D80 -> ($) 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87
; 126 $C07560:  x=138  y=119   16 at $C07D70 to $C07D80 -> ($) 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87 87
; 127 $C07566:  x=146  y=120    3 at $C07D80 to $C07D83 -> ($)                         87 87 87
;                                                           x=130
; 128 $C0756C:  x=132  y= 83    3 at $C07DBA to $C07DBD -> ($)       18 05 06
; 129 $C07572:  x=132  y= 84    3 at $C07DBD to $C07DC0 -> ($)       18 05 16
; 130 $C07578:  x=132  y= 85    3 at $C07DBD to $C07DC0 -> ($)       18 05 16
; 131 $C0757E:  x=131  y= 86    4 at $C07DC0 to $C07DC4 -> ($)    27 28 05 16
; 132 $C07584:  x=130  y= 87    3 at $C07DC4 to $C07DC7 -> ($) 28 05 05

; Group 023 - Event Flag $1F7 (503)
;                                                           x=101
; 133 $C0758A:  x=101  y= 97    2 at $C07DC7 to $C07DC9 -> ($) 26 27
; 134 $C07590:  x=101  y= 98    3 at $C07DC9 to $C07DCC -> ($) 05 05 26
; 135 $C07596:  x=103  y= 99    2 at $C07DCC to $C07DCE -> ($)       05 16

; Group 024 - Event Flag $1F5 (501)
;                                                           x=178
; 136 $C0759C:  x=182  y=130    7 at $C07DCE to $C07DD5 -> ($)             21 21 21 22 05 05 05
; 137 $C075A2:  x=180  y=131    9 at $C07DD5 to $C07DDE -> ($)       20 22 73 43 43 71 00 01 01
; 138 $C075A8:  x=180  y=132    7 at $C07DDE to $C07DE5 -> ($)       05 73 74 87 87 60 20
; 139 $C075AE:  x=178  y=133   11 at $C07DE5 to $C07DF0 -> ($) 05 05 73 74 87 87 87 70 71 20 21
; 140 $C075B4:  x=178  y=134   11 at $C07DF0 to $C07DFB -> ($) 05 05 64 87 82 83 83 84 70 71 05
; 141 $C075BA:  x=178  y=135   12 at $C07DFB to $C07E07 -> ($) 05 73 74 82 AE 88 88 AF 84 60 05 16
; 142 $C075C0:  x=178  y=136   12 at $C07E07 to $C07E13 -> ($) 05 64 87 85 88 88 88 88 86 70 71 16
; 143 $C075C6:  x=178  y=137   12 at $C07E13 to $C07E1F -> ($) 05 64 87 85 88 88 88 88 86 87 60 26
; 144 $C075CC:  x=178  y=138   13 at $C07E1F to $C07E2C -> ($) 05 53 54 90 BE 88 88 BF 92 87 70 71 26
; 145 $C075D2:  x=180  y=139   10 at $C07E2C to $C07E36 -> ($)       53 54 90 91 91 92 87 87 87 70
; 146 $C075D8:  x=180  y=140   11 at $C07E36 to $C07E41 -> ($)       05 53 54 50 33 33 54 87 87 87 87
; 147 $C075DE:  x=180  y=141    9 at $C07E41 to $C07E4A -> ($)       05 05 53 51 05 05 53 54 87
; 148 $C075E4:  x=183  y=142    3 at $C07E4A to $C07E4D -> ($)                05 05 05
; 149 $C075EA:  x=183  y=143    3 at $C07E4D to $C07E50 -> ($)                08 05 05
; 150 $C075F0:  x=183  y=144    2 at $C07E50 to $C07E52 -> ($)                18 05
; 151 $C075F6:  x=183  y=145    2 at $C07E50 to $C07E52 -> ($)                18 05
; 152 $C075FC:  x=183  y=146    2 at $C07E50 to $C07E52 -> ($)                18 05
; 153 $C07602:  x=183  y=147    2 at $C07E50 to $C07E52 -> ($)                18 05

; Group 025 - Event Flag $1F6 (502)
;                                                           x=73
; 154 $C07608:  x= 76  y=114    5 at $C07EDB to $C07EE0 -> ($)          05 05 05 05 05
; 155 $C0760E:  x= 73  y=115   15 at $C07EE0 to $C07EEF -> ($) 05 05 05 05 05 73 71 05 73 43 71 69 6B 6D 69
; 156 $C07614:  x= 73  y=116   12 at $C07EEF to $C07EFB -> ($) 05 05 05 73 43 74 70 43 74 87 70 71
; 157 $C0761A:  x= 73  y=117   12 at $C07EFB to $C07F07 -> ($) 05 05 73 74 87 82 83 83 84 87 87 60
; 158 $C07620:  x= 73  y=118   14 at $C07F07 to $C07F15 -> ($) 05 05 64 87 82 AE 88 88 AF 84 87 60 05 69
; 159 $C07626:  x= 74  y=119   13 at $C07F15 to $C07F22 -> ($)    05 53 54 85 88 88 88 88 86 87 70 71 05
; 160 $C0762C:  x= 74  y=120   13 at $C07F22 to $C07F2F -> ($)    05 05 64 85 88 88 88 88 86 87 50 51 05
; 161 $C07632:  x= 74  y=121   11 at $C07F2F to $C07F3A -> ($)    08 05 64 90 BE 88 88 BF 92 50 51
; 162 $C07638:  x= 74  y=122   11 at $C07F3A to $C07F45 -> ($)    18 05 53 54 90 91 91 92 50 51 06
; 163 $C0763E:  x= 75  y=123    9 at $C07F45 to $C07F4E -> ($)       08 05 53 54 50 33 33 51 06
; 164 $C07644:  x= 76  y=124    7 at $C07F4E to $C07F55 -> ($)          07 08 53 51 05 06 07
; 165 $C0764A:  x= 78  y=125    4 at $C07F55 to $C07F59 -> ($)                08 05 05 26

; Group 026 - Event Flag $1F7 (503)
;                                                           x=213
; 166 $C07650:  x=218  y= 42    4 at $C07FD7 to $C07FDB -> ($)                27 27 27 27
; 167 $C07656:  x=215  y= 43    8 at $C07FDB to $C07FE3 -> ($)       27 27 28 73 43 43 71 26
; 168 $C0765C:  x=214  y= 44   10 at $C07FE3 to $C07FED -> ($)    28 05 73 43 74 87 87 60 05 16
; 169 $C07662:  x=213  y= 45   11 at $C07FED to $C07FF8 -> ($) 18 73 43 74 82 83 83 84 70 71 26
; 170 $C07668:  x=213  y= 46   12 at $C07FF8 to $C08004 -> ($) 18 53 54 82 AE 88 88 AF 84 70 71 16
; 171 $C0766E:  x=214  y= 47   11 at $C08004 to $C0800F -> ($)    08 64 85 88 88 88 88 86 87 60 16
; 172 $C07674:  x=214  y= 48   11 at $C0800F to $C0801A -> ($)    18 64 85 88 88 88 88 86 50 51 16
; 173 $C0767A:  x=214  y= 49   10 at $C0801A to $C08024 -> ($)    18 64 90 BE 88 88 BF 92 60 06
; 174 $C07680:  x=214  y= 50   10 at $C08024 to $C0802E -> ($)    18 53 54 90 91 91 92 50 51 16
; 175 $C07686:  x=215  y= 51    8 at $C0802E to $C08036 -> ($)       08 53 33 54 50 33 51 06
; 176 $C0768C:  x=216  y= 52    6 at $C08036 to $C0803C -> ($)          07 08 53 51 06 07
; 177 $C07692:  x=218  y= 53    2 at $C0803C to $C0803E -> ($)                07 07
;                                                           x=167
; 178 $C07698:  x=171  y= 40    8 at $C080B7 to $C080BF -> ($)             54 87 87 87 87 87 87 87
; 179 $C0769E:  x=171  y= 41    8 at $C080BF to $C080C7 -> ($)             53 54 87 87 87 87 50 33
; 180 $C076A4:  x=170  y= 42    9 at $C080C7 to $C080D0 -> ($)          05 05 64 87 87 87 87 60 06
; 181 $C076AA:  x=169  y= 43   10 at $C080D0 to $C080DA -> ($)       05 05 73 74 87 87 87 50 51 16
; 182 $C076B0:  x=169  y= 44   10 at $C080DA to $C080E4 -> ($)       05 73 74 82 83 83 84 70 71 26
; 183 $C076B6:  x=168  y= 45   11 at $C080E4 to $C080EF -> ($)    05 05 64 82 AE 88 88 AF 84 60 05
; 184 $C076BC:  x=168  y= 46   11 at $C080EF to $C080FA -> ($)    05 73 74 85 88 88 88 88 86 70 71
; 185 $C076C2:  x=168  y= 47   12 at $C080FA to $C08106 -> ($)    05 64 87 85 88 88 88 88 86 87 60 26
; 186 $C076C8:  x=167  y= 48   13 at $C08106 to $C08113 -> ($) 02 05 53 54 90 BE 88 88 BF 92 87 60 06
; 187 $C076CE:  x=168  y= 49   12 at $C08113 to $C0811F -> ($)    02 05 53 54 90 91 91 92 87 50 51 16
; 188 $C076D4:  x=169  y= 50   11 at $C0811F to $C0812A -> ($)       02 05 64 87 87 87 50 33 51 05 26
; 189 $C076DA:  x=170  y= 51    9 at $C0812A to $C08133 -> ($)          02 53 33 54 50 51 05 05 05
; 190 $C076E0:  x=172  y= 52    6 at $C08133 to $C08139 -> ($)                05 53 51 05 05 05
; 191 $C076E6:  x=174  y= 53    4 at $C08139 to $C0813D -> ($)                      05 05 05 05
; 192 $C076EC:  x=176  y= 54    2 at $C0813D to $C0813F -> ($)                            05 05
;                                                           x=29
; 193 $C076F2:  x= 37  y= 35    2 at $C081B9 to $C081BB -> ($)                         05 05
; 194 $C076F8:  x= 33  y= 36    6 at $C081BB to $C081C1 -> ($)             05 05 05 05 05 05
; 195 $C076FE:  x= 31  y= 37   11 at $C081C1 to $C081CC -> ($)       05 05 05 73 43 71 05 73 75 71 05
; 196 $C07704:  x= 31  y= 38   11 at $C081CC to $C081D7 -> ($)       05 73 43 74 87 70 43 74 87 60 05
; 197 $C0770A:  x= 29  y= 39   13 at $C081D7 to $C081E4 -> ($) 05 05 05 64 87 82 83 83 84 87 87 60 00
; 198 $C07710:  x= 29  y= 40   13 at $C081E4 to $C081F1 -> ($) 05 05 73 74 82 AE 88 88 AF 84 87 60 10
; 199 $C07716:  x= 30  y= 41   12 at $C081F1 to $C081FD -> ($)    05 53 54 85 88 88 88 88 86 50 51 10
; 200 $C0771C:  x= 30  y= 42   11 at $C081FD to $C08208 -> ($)    05 05 64 85 88 88 88 88 86 60 00
; 201 $C07722:  x= 31  y= 43    9 at $C08208 to $C08211 -> ($)       05 64 90 BE 88 88 BF 92 60
; 202 $C07728:  x= 31  y= 44    9 at $C08211 to $C0821A -> ($)       05 53 54 90 91 91 92 87 70
; 203 $C0772E:  x= 32  y= 45    8 at $C0821A to $C08222 -> ($)          05 53 54 87 87 87 87 87
; 204 $C07734:  x= 32  y= 46    4 at $C08222 to $C08226 -> ($)          05 05 53 54
;                                                           x=206
; 205 $C0773A:  x=208  y=111    5 at $C082BD to $C082C2 -> ($)       87 87 87 70 71
; 206 $C07740:  x=206  y=112   10 at $C082C2 to $C082CC -> ($) 87 87 87 87 87 87 70 43 43 71
; 207 $C07746:  x=206  y=113   11 at $C082CC to $C082D7 -> ($) 33 54 87 87 87 82 83 83 84 70 71
; 208 $C0774C:  x=206  y=114   11 at $C082D7 to $C082E2 -> ($) 05 53 54 87 82 AE 88 88 AF 84 60
; 209 $C07752:  x=206  y=115   13 at $C082E2 to $C082EF -> ($) 5B 5D 64 87 85 88 88 88 88 86 70 71 69
; 210 $C07758:  x=207  y=116   12 at $C082EF to $C082FB -> ($)    9A 64 87 85 88 88 88 88 86 87 60 05
; 211 $C0775E:  x=207  y=117   13 at $C082FB to $C08308 -> ($)    6D 53 54 90 BE 88 88 BF 92 87 60 59 5D
; 212 $C07764:  x=209  y=118    9 at $C08308 to $C08311 -> ($)          64 87 90 91 91 92 50 33 51
; 213 $C0776A:  x=208  y=119    8 at $C08311 to $C08319 -> ($)       08 53 54 50 33 33 33 51
; 214 $C07770:  x=209  y=120    7 at $C08319 to $C08320 -> ($)          08 53 51 06 07 07 07
; 215 $C07776:  x=209  y=121    3 at $C08320 to $C08323 -> ($)          28 05 06
; 216 $C0777C:  x=210  y=122    2 at $C08323 to $C08325 -> ($)             05 16
;                                                           x=76
; 217 $C07782:  x= 76  y=124    2 at $C083A8 to $C083AA -> ($) 08 05
; 218 $C07788:  x= 77  y=125    2 at $C083AA to $C083AC -> ($)    08 05
; 219 $C0778E:  x= 77  y=126    2 at $C083AC to $C083AE -> ($)    28 05


; Map 3 replacements - from $C07794 to $C0779A (1.0 entries)

; Group 031 - Event Flag $1EA (490)
; 220 $C07794:  x=169  y=164    1 at $C07C1F to $C07C20 -> ($) 42


; Map 4 replacements - from $C0779A to $C079E6 (98.0 entries)

; Group 032 - Event Flag $1F2 (498)
;                                                           x=205
; 221 $C0779A:  x=206  y=199    1 at $C07D4D to $C07D4E -> ($)    04
; 222 $C077A0:  x=205  y=200    3 at $C07D4E to $C07D51 -> ($) 04 19 04
; 223 $C077A6:  x=206  y=201    1 at $C07D51 to $C07D52 -> ($)    04

; Group 033 - Event Flag $1F3 (499)
;                                                           x=139
; 224 $C077AC:  x=145  y=113    8 at $C07D83 to $C07D8B -> ($)                   04 04 04 04 04 04 04 04
; 225 $C077B2:  x=139  y=114   14 at $C07D8B to $C07D99 -> ($) 04 04 04 04 04 04 04 04 04 04 04 04 04 04
; 226 $C077B8:  x=139  y=115   13 at $C07D99 to $C07DA6 -> ($) 04 04 04 04 04 04 04 04 04 04 04 04 04
; 227 $C077BE:  x=140  y=116   10 at $C07DA6 to $C07DB0 -> ($)    04 04 04 04 04 04 04 04 04 04
; 228 $C077C4:  x=140  y=117    7 at $C07DB0 to $C07DB7 -> ($)    04 04 04 04 04 04 04
; 229 $C077CA:  x=141  y=118    7 at $C07DB0 to $C07DB7 -> ($)       04 04 04 04 04 04 04
; 230 $C077D0:  x=146  y=119    3 at $C07DB7 to $C07DBA -> ($)                      04 04 04

; Group 034 - Event Flag $1F5 (501)
;                                                           x=178
; 231 $C077D6:  x=182  y=128    4 at $C07E52 to $C07E56 -> ($)             33 42 42 31
; 232 $C077DC:  x=181  y=129    6 at $C07E56 to $C07E5C -> ($)          33 43 52 52 41 21
; 233 $C077E2:  x=180  y=130    7 at $C07E5C to $C07E63 -> ($)       33 43 53 62 62 51 31
; 234 $C077E8:  x=179  y=131    9 at $C07E63 to $C07E6C -> ($)    23 43 53 63 04 04 61 41 31
; 235 $C077EE:  x=179  y=132   10 at $C07E6C to $C07E76 -> ($)    33 53 63 04 04 04 04 51 41 21
; 236 $C077F4:  x=178  y=133   11 at $C07E76 to $C07E81 -> ($) 23 43 63 04 04 04 04 04 61 51 31
; 237 $C077FA:  x=178  y=134   12 at $C07E81 to $C07E8D -> ($) 23 53 04 04 22 22 22 22 04 61 41 21
; 238 $C07800:  x=178  y=135   12 at $C07E8D to $C07E99 -> ($) 23 63 04 22 22 22 22 22 22 04 51 31
; 239 $C07806:  x=178  y=136   12 at $C07E99 to $C07EA5 -> ($) 23 04 04 22 22 22 22 22 22 04 61 41
; 240 $C0780C:  x=178  y=137   12 at $C07EA5 to $C07EB1 -> ($) 23 04 04 22 22 22 22 22 22 04 04 51
; 241 $C07812:  x=178  y=138   12 at $C07EB1 to $C07EBD -> ($) 14 13 04 22 22 22 22 22 22 04 04 61
; 242 $C07818:  x=179  y=139   11 at $C07EBD to $C07EC8 -> ($)    14 13 04 22 22 22 22 04 04 04 04
; 243 $C0781E:  x=180  y=140    8 at $C07EC8 to $C07ED0 -> ($)       14 13 04 04 04 04 04 04
; 244 $C07824:  x=181  y=141    6 at $C07ED0 to $C07ED6 -> ($)          14 12 12 13 19 19
; 245 $C0782A:  x=184  y=142    3 at $C07ED6 to $C07ED9 -> ($)                   23 19 19
; 246 $C07830:  x=184  y=143    2 at $C07ED9 to $C07EDB -> ($)                   23 19
; 247 $C07836:  x=184  y=144    2 at $C07ED9 to $C07EDB -> ($)                   23 19

; Group 035 - Event Flag $1F6 (502)
;                                                           x=74
; 248 $C0783C:  x= 81  y=112    3 at $C07F59 to $C07F5C -> ($)                      33 42 31
; 249 $C07842:  x= 76  y=113    9 at $C07F5C to $C07F65 -> ($)       33 42 42 42 42 43 52 41 31
; 250 $C07848:  x= 75  y=114   11 at $C07F65 to $C07F70 -> ($)    33 43 52 52 52 52 53 62 51 41 21
; 251 $C0784E:  x= 74  y=115   12 at $C07F70 to $C07F7C -> ($) 23 43 53 62 62 62 62 63 04 61 51 21
; 252 $C07854:  x= 74  y=116   12 at $C07F7C to $C07F88 -> ($) 23 53 63 04 04 04 04 04 04 04 61 21
; 253 $C0785A:  x= 74  y=117   12 at $C07F88 to $C07F94 -> ($) 23 63 04 04 22 22 22 22 04 04 04 21
; 254 $C07860:  x= 74  y=118   12 at $C07F94 to $C07FA0 -> ($) 23 04 04 22 22 22 22 22 22 04 04 21
; 255 $C07866:  x= 74  y=119   12 at $C07FA0 to $C07FAC -> ($) 14 13 04 22 22 22 22 22 22 04 04 21
; 256 $C0786C:  x= 75  y=120   11 at $C07FAC to $C07FB7 -> ($)    23 04 22 22 22 22 22 22 04 04 21
; 257 $C07872:  x= 75  y=121   11 at $C07FB7 to $C07FC2 -> ($)    23 04 22 22 22 22 22 22 04 11 10
; 258 $C07878:  x= 75  y=122    9 at $C07FC2 to $C07FCB -> ($)    14 13 04 22 22 22 22 04 11
; 259 $C0787E:  x= 77  y=123    6 at $C07FCB to $C07FD1 -> ($)          13 04 04 04 04 11
; 260 $C07884:  x= 77  y=124    6 at $C07FD1 to $C07FD7 -> ($)          14 12 12 12 12 10

; Group 036 - Event Flag $1F7 (503)
;                                                           x=207
; 261 $C0788A:  x=212  y=108    1 at $C08325 to $C08326 -> ($)                31
; 262 $C07890:  x=212  y=109    4 at $C08326 to $C0832A -> ($)                41 42 42 31
; 263 $C07896:  x=212  y=110    5 at $C0832A to $C0832F -> ($)                51 52 52 41 31
; 264 $C0789C:  x=211  y=111    7 at $C0832F to $C08336 -> ($)             19 61 62 62 51 41 21
; 265 $C078A2:  x=208  y=112   10 at $C08336 to $C08340 -> ($)    04 04 04 04 04 04 04 61 51 31
; 266 $C078A8:  x=207  y=113   12 at $C08340 to $C0834C -> ($) 13 04 04 04 22 22 22 22 04 61 41 21
; 267 $C078AE:  x=207  y=114   12 at $C0834C to $C08358 -> ($) 23 04 04 22 22 22 22 22 22 04 51 21
; 268 $C078B4:  x=207  y=115   12 at $C08358 to $C08364 -> ($) 23 04 04 22 22 22 22 22 22 04 61 21
; 269 $C078BA:  x=207  y=116   12 at $C08364 to $C08370 -> ($) 23 04 04 22 22 22 22 22 22 04 04 21
; 270 $C078C0:  x=207  y=117   12 at $C08370 to $C0837C -> ($) 14 13 04 22 22 22 22 22 22 04 04 21
; 271 $C078C6:  x=208  y=118   11 at $C0837C to $C08387 -> ($)    33 04 04 22 22 22 22 04 04 11 10
; 272 $C078CC:  x=208  y=119   10 at $C08387 to $C08391 -> ($)    43 19 04 04 04 04 04 11 12 10
; 273 $C078D2:  x=208  y=120    8 at $C08391 to $C08399 -> ($)    53 19 11 12 13 11 12 10
; 274 $C078D8:  x=208  y=121    6 at $C08399 to $C0839F -> ($)    63 19 31 03 33 31
; 275 $C078DE:  x=209  y=122    5 at $C0839F to $C083A4 -> ($)       19 41 42 43 41
; 276 $C078E4:  x=212  y=123    2 at $C083A4 to $C083A6 -> ($)                53 51
; 277 $C078EA:  x=212  y=124    2 at $C083A6 to $C083A8 -> ($)                63 61
;                                                           x=31
; 278 $C078F0:  x= 34  y= 34    7 at $C08226 to $C0822D -> ($)          33 42 31 03 33 42 31
; 279 $C078F6:  x= 32  y= 35   10 at $C0822D to $C08237 -> ($)    33 42 43 52 41 42 43 52 41 21
; 280 $C078FC:  x= 31  y= 36   11 at $C08237 to $C08242 -> ($) 23 43 52 53 62 51 52 53 62 51 21
; 281 $C07902:  x= 31  y= 37   11 at $C08242 to $C0824D -> ($) 23 53 62 63 04 61 62 63 04 61 21
; 282 $C07908:  x= 31  y= 38   11 at $C0824D to $C08258 -> ($) 23 63 04 04 04 04 04 04 04 04 21
; 283 $C0790E:  x= 31  y= 39   11 at $C08258 to $C08263 -> ($) 23 04 04 22 22 22 22 04 04 04 21
; 284 $C07914:  x= 31  y= 40   11 at $C08263 to $C0826E -> ($) 23 04 22 22 22 22 22 22 04 04 21
; 285 $C0791A:  x= 31  y= 41   11 at $C0826E to $C08279 -> ($) 33 04 22 22 22 22 22 22 04 04 31
; 286 $C07920:  x= 31  y= 42   12 at $C08279 to $C08285 -> ($) 43 04 22 22 22 22 22 22 04 04 41 21
; 287 $C07926:  x= 31  y= 43   12 at $C08285 to $C08291 -> ($) 53 04 22 22 22 22 22 22 04 04 51 21
; 288 $C0792C:  x= 31  y= 44   12 at $C08291 to $C0829D -> ($) 63 19 04 22 22 22 22 04 04 04 61 21
; 289 $C07932:  x= 31  y= 45   12 at $C0829D to $C082A9 -> ($) 19 11 13 04 04 04 04 04 04 04 04 21
; 290 $C07938:  x= 31  y= 46   11 at $C082A9 to $C082B4 -> ($) 19 21 23 19 04 04 04 04 04 19 19
; 291 $C0793E:  x= 34  y= 47    8 at $C082B4 to $C082BC -> ($)          19 04 04 18 04 04 19 19
; 292 $C07944:  x= 41  y= 48    1 at $C082BC to $C082BD -> ($)                               19
;                                                           x=168
; 293 $C0794A:  x=170  y= 41    9 at $C0813F to $C08148 -> ($)       19 19 04 04 04 04 04 04 19
; 294 $C07950:  x=169  y= 42   10 at $C08148 to $C08152 -> ($)    19 19 19 04 04 04 04 04 04 11
; 295 $C07956:  x=169  y= 43   10 at $C08152 to $C0815C -> ($)    19 19 19 04 04 04 04 04 19 31
; 296 $C0795C:  x=168  y= 44   12 at $C0815C to $C08168 -> ($) 19 19 19 04 22 22 22 22 04 19 41 21
; 297 $C07962:  x=168  y= 45   12 at $C08168 to $C08174 -> ($) 13 19 04 22 22 22 22 22 22 04 51 21
; 298 $C07968:  x=168  y= 46   12 at $C08174 to $C08180 -> ($) 23 19 04 22 22 22 22 22 22 04 61 21
; 299 $C0796E:  x=168  y= 47   12 at $C08180 to $C0818C -> ($) 23 04 04 22 22 22 22 22 22 04 04 21
; 300 $C07974:  x=168  y= 48   12 at $C0818C to $C08198 -> ($) 14 13 04 22 22 22 22 22 22 04 04 21
; 301 $C0797A:  x=169  y= 49   11 at $C08198 to $C081A3 -> ($)    14 13 04 22 22 22 22 04 04 11 10
; 302 $C07980:  x=170  y= 50    9 at $C081A3 to $C081AC -> ($)       23 04 04 04 04 04 04 11 10
; 303 $C07986:  x=170  y= 51    8 at $C081AC to $C081B4 -> ($)       14 13 04 04 04 11 12 10
; 304 $C0798C:  x=171  y= 52    5 at $C081B4 to $C081B9 -> ($)          14 12 12 12 10
;                                                           x=214
; 305 $C07992:  x=218  y= 40    4 at $C0803E to $C08042 -> ($)             33 42 42 31
; 306 $C07998:  x=216  y= 41    7 at $C08042 to $C08049 -> ($)       33 42 43 52 52 41 21
; 307 $C0799E:  x=214  y= 42    9 at $C08049 to $C08052 -> ($) 23 42 43 52 53 62 62 51 31
; 308 $C079A4:  x=214  y= 43   10 at $C08052 to $C0805C -> ($) 23 52 53 62 63 04 04 61 41 31
; 309 $C079AA:  x=214  y= 44   11 at $C0805C to $C08067 -> ($) 23 62 63 04 04 04 04 04 51 41 21
; 310 $C079B0:  x=214  y= 45   11 at $C08067 to $C08072 -> ($) 23 04 04 22 22 22 22 04 61 51 21
; 311 $C079B6:  x=214  y= 46   11 at $C08072 to $C0807D -> ($) 23 04 22 22 22 22 22 22 04 61 21
; 312 $C079BC:  x=214  y= 47   11 at $C0807D to $C08088 -> ($) 23 04 22 22 22 22 22 22 04 04 21
; 313 $C079C2:  x=214  y= 48   11 at $C08088 to $C08093 -> ($) 23 04 22 22 22 22 22 22 04 11 10
; 314 $C079C8:  x=214  y= 49   10 at $C08093 to $C0809D -> ($) 23 04 22 22 22 22 22 22 04 21
; 315 $C079CE:  x=214  y= 50   10 at $C0809D to $C080A7 -> ($) 14 13 04 22 22 22 22 04 11 10
; 316 $C079D4:  x=215  y= 51    8 at $C080A7 to $C080AF -> ($)    14 13 04 04 04 04 11 10
; 317 $C079DA:  x=216  y= 52    6 at $C080AF to $C080B5 -> ($)       14 12 13 11 12 10
; 318 $C079E0:  x=218  y= 53    2 at $C080B5 to $C080B7 -> ($)             14 10

; ---------------------------------------------------------------------------

.proc RandomBattle

_ccf0:  .a8
        .i16
        ldx     $00
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
_cd46:  jsr     $1cd7
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
