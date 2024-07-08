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

; [ macros for reserving RAM variables ]

.enum RAM_ADDR_SIZE
        ZP = 1
        NEAR = 2
        FAR = 3
.endenum

::_ram_offset .set 0
::_ram_verbose .set 0
::_ram_addr_size .set RAM_ADDR_SIZE::FAR
::_ram_scope_start .set 0

.define ram_org(offset) ::_ram_offset .set offset
.define ram_verbose(value) ::_ram_verbose .set value
.define ram_addr_size(size) ::_ram_addr_size .set RAM_ADDR_SIZE::size

.mac ram_label label, namespace
        .ifblank namespace
                .ident(.sprintf("%s_zp", .string(label))) := <::_ram_offset
                .ident(.sprintf("%s_near", .string(label))) := near ::_ram_offset
                .ident(.sprintf("%s_far", .string(label))) := ::_ram_offset
                .if ::_ram_addr_size = RAM_ADDR_SIZE::ZP
                        label := <::_ram_offset
                .elseif ::_ram_addr_size = RAM_ADDR_SIZE::NEAR
                        label := near ::_ram_offset
                .elseif ::_ram_addr_size = RAM_ADDR_SIZE::FAR
                        label := ::_ram_offset
                .endif
        .else
                namespace::.ident(.sprintf("%s_zp", .string(label))) := <::_ram_offset
                namespace::.ident(.sprintf("%s_near", .string(label))) := near ::_ram_offset
                namespace::.ident(.sprintf("%s_far", .string(label))) := ::_ram_offset
                .if ::_ram_addr_size = RAM_ADDR_SIZE::ZP
                        namespace::label := <::_ram_offset
                .elseif ::_ram_addr_size = RAM_ADDR_SIZE::NEAR
                        namespace::label := near ::_ram_offset
                .elseif ::_ram_addr_size = RAM_ADDR_SIZE::FAR
                        namespace::label := ::_ram_offset
                .endif
        .endif
.endmac

.mac ram_res label, size
        ; define the label
        .ifnblank label
                ram_label label

                ; define labels for array items
                .if .defined(NUM_ITEMS) && .defined(ITEM_SIZE)
                        .local old_offset
                        old_offset = ::_ram_offset
                        .repeat NUM_ITEMS, i
                                ram_label label, .ident(.sprintf("_%d", i))
                                ::_ram_offset .set ::_ram_offset + ITEM_SIZE
                        .endrep
                        ::_ram_offset .set old_offset
                .endif

                ; print the label and address
                .if ::_ram_verbose
                        .if label <= $ff
                                .out .sprintf("$%02x %s", label, .string(label))
                        .elseif label <= $ffff
                                .out .sprintf("$%04x %s", label, .string(label))
                        .else
                                .out .sprintf("$%06x %s", label, .string(label))
                        .endif
                .endif

                ; define the size
                .if size <> 0
                        .scope label
                        .endscope
                        label::SIZE = size
                .endif
        .endif

        ; increment the ram offset
        ::_ram_offset .set ::_ram_offset + size
.endmac

.mac ram_byte label
        ram_res label, 1
.endmac

.mac ram_word label
        ram_res label, 2
.endmac

.mac ram_faraddr label
        ram_res label, 3
.endmac

.mac ram_dword label
        ram_res label, 4
.endmac

.mac ram_scope label
        ram_res label, 0
        .scope label
        START = ::_ram_offset
.endmac

.mac end_ram_scope
; define the size of this scope and end the scope
        .ifndef SIZE
                SIZE = ::_ram_offset - START
        .endif
        .endscope
.endmac

.mac ram_array label, num_items, item_size, num_blocks
        ram_scope label
        ITEM_SIZE = item_size
        NUM_ITEMS = num_items
        SIZE = ITEM_SIZE * NUM_ITEMS
        .ifnblank num_blocks
                ; array with more than 1 block
                .assert item_size .mod num_blocks = 0, error, "Item size must be divisible by the number of blocks"
                BLOCK_SIZE = item_size / num_blocks
                NUM_BLOCKS = num_blocks
                block_start .set START
        .endif

        ; create namespaces for each array item
        .repeat NUM_ITEMS, i
                .scope .ident(.sprintf("_%d", i))
                .endscope
                ::_ram_offset .set START + i * ITEM_SIZE
                ram_label .ident(.sprintf("_%d", i)), label
        .endrep
        ::_ram_offset .set START
.endmac

.mac end_ram_array
        ; make sure we didn't overflow
        .local bytes_over
        .ifdef BLOCK_SIZE
                bytes_over = ::_ram_offset - block_start - BLOCK_SIZE
                .assert bytes_over <= 0, error, .sprintf("Array block too large by %d bytes", bytes_over)
        .else
                bytes_over = ::_ram_offset - START - ITEM_SIZE
                .assert bytes_over <= 0, error, .sprintf("Array item too large by %d bytes", bytes_over)
        .endif

        ; increment the RAM offset
        ::_ram_offset .set START + SIZE
        end_ram_scope
.endmac

.mac end_ram_block
        ; check if the block overflowed
        .local bytes_over
        bytes_over = ::_ram_offset - block_start - BLOCK_SIZE
        .assert bytes_over <= 0, error, .sprintf("Array block too large by %d bytes", bytes_over)

        ; go to the next block
        ::_ram_offset .set block_start + BLOCK_SIZE * NUM_ITEMS
        block_start .set ::_ram_offset
.endmac

.define array_item(array, index) array::.ident(.sprintf("_%d", index))
.define array_member(array, index, member) array::.ident(.sprintf("_%d", index))::member

; ------------------------------------------------------------------------------

.endif
.list on
