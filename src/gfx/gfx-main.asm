; ---------------------------------------------------------------------------

.segment "big_font_gfx"

.export BigFontGfx

; c3/eb00
BigFontGfx:
        .incbin "big_font.1bpp"

; ---------------------------------------------------------------------------

.segment "small_font_gfx"

.export SmallFontGfx

; d1/f000
SmallFontGfx:
        .incbin "small_font.2bpp"

; ---------------------------------------------------------------------------

.segment "kanji_gfx"

.export KanjiGfx

; db/d000
KanjiGfx:
        .incbin "kanji.1bpp"

; ---------------------------------------------------------------------------
