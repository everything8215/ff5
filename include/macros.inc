; +-------------------------------------------------------------------------+
; |                                                                         |
; |                             FINAL FANTASY V                             |
; |                                                                         |
; +-------------------------------------------------------------------------+
; | file: macro.inc                                                         |
; |                                                                         |
; | description: macro definitions                                          |
; +-------------------------------------------------------------------------+

.list off
.ifndef MACROS_INC
MACROS_INC = 1

; ------------------------------------------------------------------------------

; [ clr_a/x/y ]

; clear accumulator, x, y registers
; note: don't use if dp register is nonzero

.macro clr_a
        tdc
.endmacro

.macro clr_ax
        tdc
        tax
.endmacro

.macro clr_ay
        tdc
        tay
.endmacro

.macro clr_axy
        tdc
        tax
        tay
.endmacro

.macro clr_ayx
        tdc
        tay
        tax
.endmacro

; ------------------------------------------------------------------------------

; [ set the size of accumulator/index registers ]

PSW_A = %00100000
PSW_I = %00010000
PSW_C = %00000001

.macro longa
        .a16
        rep #PSW_A
.endmacro

.macro shorta
        .a8
        sep #PSW_A
.endmacro

.macro shorta0                          ; battle code almost always clears
        clr_a                           ; accumulator before a shorta
        shorta
.endmacro

.macro longi
        .i16
        rep #PSW_I
.endmacro

.macro shorti
        .i8
        sep #PSW_I
.endmacro

.macro longai
        .a16
        .i16
        rep #PSW_A|PSW_I
.endmacro

.macro shortai
        .a8
        .i8
        sep #PSW_A|PSW_I
.endmacro

; ------------------------------------------------------------------------------

; [ set the size of accumulator/index registers and carry ]

.macro longa_clc
        .a16
        rep #PSW_A|PSW_C
.endmacro

.macro shorta_sec
        .a8
        sep #PSW_A|PSW_C
.endmacro

.macro longi_clc
        .i16
        rep #PSW_I|PSW_C
.endmacro

.macro shorti_sec
        .i8
        sep #PSW_I|PSW_C
.endmacro

.macro longai_clc
        .a16
        .i16
        rep #PSW_A|PSW_I|PSW_C
.endmacro

.macro shortai_sec
        .a8
        .i8
        sep #PSW_A|PSW_I|PSW_C
.endmacro

; ------------------------------------------------------------------------------

; [ repeated no operation ]

.macro nop_n n
.repeat n
        nop
.endrep
.endmacro

.define nop2 nop_n 2
.define nop3 nop_n 3
.define nop4 nop_n 4
.define nop5 nop_n 5
.define nop6 nop_n 6
.define nop7 nop_n 7
.define nop8 nop_n 8

; ------------------------------------------------------------------------------

; [ repeated shift left ]

.macro asl_n n
.repeat n
        asl
.endrep
.endmacro

.define asl2 asl_n 2
.define asl3 asl_n 3
.define asl4 asl_n 4
.define asl5 asl_n 5
.define asl6 asl_n 6
.define asl7 asl_n 7
.define asl8 asl_n 8

; ------------------------------------------------------------------------------

; [ repeated shift right ]

.macro lsr_n n
.repeat n
        lsr
.endrep
.endmacro

.define lsr2 lsr_n 2
.define lsr3 lsr_n 3
.define lsr4 lsr_n 4
.define lsr5 lsr_n 5
.define lsr6 lsr_n 6
.define lsr7 lsr_n 7
.define lsr8 lsr_n 8

; ------------------------------------------------------------------------------

; [ repeated rotate left ]

.macro rol_n n
.repeat n
        rol
.endrep
.endmacro

.define rol2 rol_n 2
.define rol3 rol_n 3
.define rol4 rol_n 4
.define rol5 rol_n 5
.define rol6 rol_n 6
.define rol7 rol_n 7
.define rol8 rol_n 8

; ------------------------------------------------------------------------------

; [ repeated rotate right ]

.macro ror_n n
.repeat n
        ror
.endrep
.endmacro

.define ror2 ror_n 2
.define ror3 ror_n 3
.define ror4 ror_n 4
.define ror5 ror_n 5
.define ror6 ror_n 6
.define ror7 ror_n 7
.define ror8 ror_n 8

; ------------------------------------------------------------------------------

; [ repeated increment ]

.macro inc_n n
.repeat n
        inc
.endrep
.endmacro

.define inc2 inc_n 2
.define inc3 inc_n 3
.define inc4 inc_n 4
.define inc5 inc_n 5
.define inc6 inc_n 6
.define inc7 inc_n 7
.define inc8 inc_n 8

; ------------------------------------------------------------------------------

; [ repeated decrement ]

.macro dec_n n
.repeat n
        dec
.endrep
.endmacro

.define dec2 dec_n 2
.define dec3 dec_n 3
.define dec4 dec_n 4
.define dec5 dec_n 5
.define dec6 dec_n 6
.define dec7 dec_n 7
.define dec8 dec_n 8

; ------------------------------------------------------------------------------

; [ repeated increment x ]

.macro inx_n n
.repeat n
        inx
.endrep
.endmacro

.define inx2 inx_n 2
.define inx3 inx_n 3
.define inx4 inx_n 4
.define inx5 inx_n 5
.define inx6 inx_n 6
.define inx7 inx_n 7
.define inx8 inx_n 8

; ------------------------------------------------------------------------------

; [ repeated decrement x ]

.macro dex_n n
.repeat n
        dex
.endrep
.endmacro

.define dex2 dex_n 2
.define dex3 dex_n 3
.define dex4 dex_n 4
.define dex5 dex_n 5
.define dex6 dex_n 6
.define dex7 dex_n 7
.define dex8 dex_n 8

; ------------------------------------------------------------------------------

; [ repeated increment y ]

.macro iny_n n
.repeat n
        iny
.endrep
.endmacro

.define iny2 iny_n 2
.define iny3 iny_n 3
.define iny4 iny_n 4
.define iny5 iny_n 5
.define iny6 iny_n 6
.define iny7 iny_n 7
.define iny8 iny_n 8

; ------------------------------------------------------------------------------

; [ repeated decrement y ]

.macro dey_n n
.repeat n
        dey
.endrep
.endmacro

.define dey2 dey_n 2
.define dey3 dey_n 3
.define dey4 dey_n 4
.define dey5 dey_n 5
.define dey6 dey_n 6
.define dey7 dey_n 7
.define dey8 dey_n 8

; ------------------------------------------------------------------------------

; [ invert and negate the A register ]

; bitwise inverse, i.e. logical not (1's complement)
.mac not_a
        .if .asize = 8
                eor #$ff
        .else
                eor #$ffff
        .endif
.endmac

; negate (2's complement)
.mac neg_a
        not_a
        inc
.endmac

; ------------------------------------------------------------------------------

; [ long branch instructions ]

.macro  jeq     addr
        bne     *+5
        jmp     addr
.endmacro

.macro  jne     addr
        beq     *+5
        jmp     addr
.endmacro

.macro  jmi     addr
        bpl     *+5
        jmp     addr
.endmacro

.macro  jpl     addr
        bmi     *+5
        jmp     addr
.endmacro

.macro  jcs     addr
        bcc     *+5
        jmp     addr
.endmacro

.macro  jcc     addr
        bcs     *+5
        jmp     addr
.endmacro

.macro  jvs     addr
        bvc     *+5
        jmp     addr
.endmacro

.macro  jvc     addr
        bvs     *+5
        jmp     addr
.endmacro

; ------------------------------------------------------------------------------

; [ define near address (16-bit) ]

.define near(label) .loword(label)

; ------------------------------------------------------------------------------

; [ define bank start address ]

.define bank_start(label) .bankbyte(label) << 16

; ------------------------------------------------------------------------------

; [ make pointer table (near) ]

.macro ptr_tbl label
        .local offset
        .ifdef label::Start
                offset = label::Start
        .else
                offset = 0
        .endif
        .repeat label::ARRAY_LENGTH, i
                .addr label::.ident(.sprintf("_%d", i)) - offset
        .endrep
.endmacro

; ------------------------------------------------------------------------------

; [ make pointer table (far) ]

.macro ptr_tbl_far label
        .local offset
        .ifdef label::Start
                offset = label::Start
        .else
                offset = 0
        .endif
        .repeat label::ARRAY_LENGTH, i
                .faraddr label::.ident(.sprintf("_%d", i)) - offset
        .endrep
.endmacro

; ------------------------------------------------------------------------------

; [ make pointer table (dword) ]

.macro ptr_tbl_dword label
        .local offset
        .ifdef label::Start
                offset = label::Start
        .else
                offset = 0
        .endif
        .repeat label::ARRAY_LENGTH, i
                .dword label::.ident(.sprintf("_%d", i)) - offset
        .endrep
.endmacro

; ------------------------------------------------------------------------------

; [ make end pointer ]

.mac end_ptr label
        .ifdef label::Start
                .addr label::End - label::Start
        .else
                .addr label::End
        .endif
.endmac

.mac end_ptr_far label
        .ifdef label::Start
                .faraddr label::End - label::Start
        .else
                .faraddr label::End
        .endif
.endmac

.mac end_ptr_dword label
        .ifdef label::Start
                .dword label::End - label::Start
        .else
                .dword label::End
        .endif
.endmac

; ------------------------------------------------------------------------------

; [ begin/end fixed length block ]

::_fixed_block_size .set 0
::_fixed_block_seq .set 0

.mac fixed_block size
        .assert ::_fixed_block_size = 0, error, "Fixed blocks can't be nested"
        .assert size > 0, error, "Invalid fixed block size"
        ::_fixed_block_size .set size
        .ident(.sprintf("_fixed_block_%d", ::_fixed_block_seq)) := *
.endmac

.mac end_fixed_block pad_val
        .assert ::_fixed_block_size <> 0, error, "Invalid fixed block"
        .local pad_size, start
        start := .ident(.sprintf("_fixed_block_%d", ::_fixed_block_seq))
        pad_size = (start - *) + ::_fixed_block_size
        .if pad_size < 0
                .error .sprintf("Fixed block too large by %d bytes", -pad_size)
        .elseif pad_size > 0
                .ifnblank pad_val
                        .res pad_size, pad_val
                .else
                        .res pad_size
                .endif
        .endif
        ::_fixed_block_size .set 0
        ::_fixed_block_seq .set ::_fixed_block_seq + 1
.endmac

; ------------------------------------------------------------------------------

; [ calculate the size of assembled bytecode ]

; place this macro at the end of a block to define a symbol "sizeof_<labal>"
; equal to the size of the block (i.e. the distance from the label to the
; label to the current program counter).

.mac calc_size label
        .ident(.concat("sizeof_", .string(label))) = * - label
.endmac

; ------------------------------------------------------------------------------

.define array_item(array, index) array::.ident(.sprintf("_%d", index))
.define array_member(array, index, member) array::.ident(.sprintf("_%d", index))::member

; ------------------------------------------------------------------------------

.endif
.list on
