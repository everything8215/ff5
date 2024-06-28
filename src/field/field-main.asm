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

.import ExecSound_ext, ExecBattle_ext, ShowCutscene_ext, ExecMenu_ext

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
        jsl     $c40000
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
        jsr     $1ec5
        jsr     $1e64
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
        jsr     $1e14
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
        jsr     $1e14
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
        lda     $c008b3,x
        sta     $c0
        lda     #$03
        sta     $0adb
        txa
        asl
        tax
        lda     $c00897,x
        sta     $26
        lda     $c00898,x
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
        and     $c00890,x
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
        lda     $c008a5,x
        sta     $26
        lda     $c008a6,x
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

_0890:  .byte $00,$10,$20,$40,$00,$00,$80
_0897:  .addr $0000,$08ba,$08c1,$08ef,$098d,$0a4d,$0a57
_08a5:  .addr $0000,$08bb,$091d,$0955,$098e,$0a54,$0aa3
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
        jsr     $1ec5
        jsr     $1e64
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
        jsr     $1ec5
        jsr     $1e64
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
        jsr     $1ec5
        jsr     $1e64
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
        jsr     $1ec5
        jsr     $1e64
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
        jsr     $1e64
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
        lda     $c00c5f,x
        sta     $6f
        asl
        ora     #$80
        sta     $0ade,y
        jsr     $2137
        jsr     $612b
        jsr     $1e64
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
        jsr     $1e64
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
        adc     $c00f84,x
        and     #$3f
        sta     $75
        lda     $0ad9
        clc
        adc     $c00f88,x
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
        lda     f:_0f78,x
        sta     $4203
        nop4
        ldy     $4216
        sty     $37
        lda     $12
        sta     $4202
        lda     f:_0f78+1,x
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

_0f78:  .addr   1,10,100,1000,10000,100000

        .word   $0100, $ff00, $00ff, $0001

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
        lda     $c0172a,x   ; forced facing direction
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
        lda     $c0171c,x   ; tile movement speed
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
        lda     $c0172a,x
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
        lda     $c01720,x   ; pointer to tile properties
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
        sbc     $c0147d,x   ; subtract hp
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
        lda     $c01474,x
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
_1475:  .byte   $94, $94, $94, $94, $94, $94, $94, $94

; tile damage values (0, 50, 50, 100, 300, 400, 500, 1000)
_147d:  .word   $0000, $0032, $0032, $0064, $012c, $0190, $01f4, $03e8

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
        lda     $c01720,x   ; pointer to tile properties
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
        lda     $c0161b,x
        tay
        lda     $10be,y
        cmp     #$05
        bne     _1612
        lda     $0ad8
        clc
        adc     $c01617,x
        sta     $75
        lda     $0ad9
        clc
        adc     $c01613,x
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

_1613:  .byte   $ff,$00,$01,$00
_1617:  .byte   $00,$01,$00,$ff
_161b:  .byte   $02,$0a,$0e,$06

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
        and     $c01725,x   ; facing direction mask
        beq     _16a9
_1661:  lda     $c4
        tax
        lda     $c01720,x   ; pointer to tile properties
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
        lda     $c01720,x   ; pointer to tile properties
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
        adc     $c01712,x
        sta     $75
        lda     $0ad9
        clc
        adc     $c01717,x
        sta     $76
        phx
        jsr     $3cbb       ; get pointer to object layout
        lda     #$ff
        sta     $7f3000,x
        plx
_1711:  rts

; ---------------------------------------------------------------------------

; none, up, right, down, left
_1712:  .byte $00,$00,$01,$00,$ff ; delta x
_1717:  .byte $00,$ff,$00,$01,$00 ; delta y
_171c:  .byte $02,$01,$04,$08; tile movement speeds
_1720:  .byte $08,$02,$0a,$0e,$06 ; pointer to tile properties
_1725:  .byte $00,$08,$01,$04,$02
_172a:  .byte $00,$02,$04,$00,$03,$00,$00,$00,$01

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
        lda     $c01e02,x
        sta     $4302
        lda     #$6000
        sta     $2116
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
        lda     $c01dfa,x
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
_1dfa:  .word   $3b80,$4240,$3a00,$3a60

; pointers to party sprite graphics (+$da0000)
_1e02:  .word   $c800,$d000,$d800,$e000,$e800,$f000,$f800,$6c00,$8400

; ---------------------------------------------------------------------------

; [ copy alt. sprite graphics to vram ]

_1e14:  lda     $a4
        beq     _1e59
        dec
        asl
        tax
        longa
        lda     $c01e5a,x   ; pointer to graphics
        sta     $4302
        lda     $06
        shorta
        lda     $53
        bne     _1e31
        ldx     #$7300      ; vram address = $7300 (normal map)
        bra     _1e34
_1e31:  ldx     #$6100      ; vram address = $6100 (world map)
_1e34:  stx     $2116
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
_1e5a:  .word   $4d80,$5d80,$6580,$6180,$6180

; ---------------------------------------------------------------------------

        lda     $bd
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
        lda     $c01eb5,x
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

_1eb5:  .word   $3044,$3044,$3045,$3045,$3045,$3046,$3046,$3046

; ---------------------------------------------------------------------------

; [  ]

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
        adc     $c0205f
        sec
        sbc     $0ad8
        cmp     $c02060
        bcs     _1f48
        jsr     $201f
        lda     $14
        lsr
        bcs     _1f48
        lda     $0ae0,y
        clc
        adc     $c02061
        sec
        sbc     $0ad9
        cmp     $c02062
        bcs     _1f48
        jsr     $203f
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
        lda     $c02063,x
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
        adc     $c020b7,x
        sec
        sbc     $10a0
        sta     $0298,y
        lda     $15
        clc
        adc     $c020b8,x
        sec
        sbc     $10a2
        sta     $0299,y
        longa
        lda     $c020b9,x
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
        adc     $c02a95,x
        sec
        sbc     #$08
        sec
        sbc     $10a0
        sta     $0298,y
        lda     $15
        clc
        adc     $c02a96,x
        sec
        sbc     #$07
        sec
        sbc     $10a2
        sta     $0299,y
        longa
        lda     $c02a97,x
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

_205f:  .byte   $07,$11,$07,$0f

_2063:  .word   $0000,$0000,$0000,$0000,$0010,$0000,$0000,$0000
_2073:  .word   $0020,$0000,$0000,$0000,$046c,$0000,$0000,$0000
_2083:  .word   $06cc,$06b4,$06cc,$06b4,$0654,$0684,$0000,$0000
_2093:  .word   $069c,$0654,$06cc,$06b4,$0000,$0000,$0000,$0000
_20a3:  .word   $0008,$0000,$0008,$0000,$0020,$0000,$1020,$0000
_20b3:  .word   $2002,$0000

; ---------------------------------------------------------------------------

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
        lda     $c022db,x
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
_21ea:  lda     $c02a95,x
        clc
        adc     #$70
        sec
        sbc     $10a0
        sta     $0200,y
        lda     $c02a96,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0201,y
        lda     $c02a97,x
        clc
        adc     $11
        sta     $0202,y
        inx4
        iny4
        dec     $0a
        bne     _21ea
        ply
        plx
        lda     $c02a98,x
        ora     $08
        sta     $0203,y
        lda     $c02a9c,x
        ora     $08
        sta     $0207,y
        lda     $c02aa0,x
        ora     $09
        sta     $020b,y
        lda     $c02aa4,x
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
        lda     $c02063,x
        sta     $11
        lda     $06
        shorta
        lda     $0adc
        asl2
        sta     $09
        lda     $0add,y
        and     #$03
        ora     $09
        tax
        lda     $3e
        and     $c0209b,x
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
_229f:  lda     $c020b7,x
        clc
        adc     #$78
        sec
        sbc     $10a0
        sta     $0200,y
        lda     $c020b8,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0201,y
        longa
        lda     $c020b9,x
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

_22db:  .byte   $00,$01,$03,$04,$05,$06,$07,$08,$09,$0a,$0a,$0b,$0b,$0c,$0c,$0c
_22eb:  .byte   $0c,$0c,$0c,$0b,$0b,$0a,$0a,$09,$08,$07,$06,$05,$04,$03,$01,$00

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
        lda     $c0234c,x
        sta     $028a
        inc
        sta     $028e
        inc
        sta     $0292
        inc
        sta     $0296
        lda     $06
        shorta
        jsr     $1ec5
        lda     $3e
        cmp     #$1f
        bne     _22fd
        rts

; ---------------------------------------------------------------------------

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
        lda     $c022db,x
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
        ora     $c0256c,x
        sta     $08
        lda     $09
        ora     $c0256c,x
        sta     $09
        plx

; pointer to sprite data for upper half
; ($0110 for normal priority, $01e8 for low priority)
        ldy     $c7
        lda     $c02a95,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $0200,y                 ; x position
        lda     $c02a96,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0201,y                 ; y position
        lda     $c02a97,x
        sta     $0202,y                 ; tile index
        lda     $c02a98,x
        ora     $08
        sta     $0203,y                 ; vhoopppm
        lda     $c02a99,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $0204,y
        lda     $c02a9a,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0205,y
        lda     $c02a9b,x
        sta     $0206,y
        lda     $c02a9c,x
        ora     $08
        sta     $0207,y
        ldy     #$01e8                  ; pointer to sprite data for lower half
        lda     $c02a9d,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $0208,y
        lda     $c02a9e,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $0209,y
        lda     $c02a9f,x
        sta     $020a,y
        lda     $c02aa0,x
        ora     $09
        sta     $020b,y
        lda     $c02aa1,x
        clc
        adc     $c5
        sec
        sbc     $10a0
        sta     $020c,y
        lda     $c02aa2,x
        clc
        adc     $15
        sec
        sbc     $10a2
        sec
        sbc     $0d
        sta     $020d,y
        lda     $c02aa3,x
        sta     $020e,y
        lda     $c02aa4,x
        ora     $09
        sta     $020f,y
        rts

; ---------------------------------------------------------------------------

_256c:  .res    16, 0

; ---------------------------------------------------------------------------

; [  ]

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
        sta     $2116
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
_25d2:  lda     $c0df00,x
        stz     $2118
        sta     $2119
        inx
        dey
        bne     _25d2
        jsr     $2636
        ldy     #$0008
_25e6:  lda     $c0df00,x
        stz     $2118
        sta     $2119
        inx
        dey
        bne     _25e6
        jsr     $2636
        ldy     $2e
        sty     $2116
        plx
        ldy     #$0008
_2600:  lda     $c0df80,x
        stz     $2118
        sta     $2119
        inx
        dey
        bne     _2600
        jsr     $2636
        ldy     #$0008
_2614:  lda     $c0df80,x
        stz     $2118
        sta     $2119
        inx
        dey
        bne     _2614
        jsr     $2636
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

; [ show cutscene ]

; a: cutscene id

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

.proc random_battle

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
