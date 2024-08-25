; ---------------------------------------------------------------------------

.include "macros.inc"

; ---------------------------------------------------------------------------

.segment "window_gfx"

.export WindowPal, WindowGfx

; c0/d340
WindowPal:
        .incbin "window.pal"

; c0/d380
WindowGfx:
        .incbin "window.4bpp"

; ---------------------------------------------------------------------------

.segment "map_overlay"

.export MapOverlayGfx

; c0/df00
MapOverlayGfx:
        .incbin "map_overlay.1bpp"

; ---------------------------------------------------------------------------

.segment "map_pal"

.export MapPal

; c3/bb00
MapPal:
.repeat 44, i
        .incbin .sprintf("map_pal/map_pal_%04x.pal", i)
.endrep

; ---------------------------------------------------------------------------

.segment "big_font_gfx"

.export BigFontGfx

; c3/eb00
BigFontGfx:
        .incbin "big_font.1bpp"

; ---------------------------------------------------------------------------

.segment "timer_font_gfx"

.export TimerFontGfx

; cd/fe00
TimerFontGfx:
        .incbin "timer_font.4bpp"

; ---------------------------------------------------------------------------

.segment "minimap_sprite_gfx"

.export MinimapSpriteGfx

; cf/d800
MinimapSpriteGfx:
        .incbin "minimap_sprite.4bpp"

; ---------------------------------------------------------------------------

.export WorldTileAttr

.segment "world_tile_attr"

; cf/f9c0
WorldTileAttr:
        .incbin "world_tile_attr/bartz_world.dat"
        .incbin "world_tile_attr/galuf_world.dat"
        .incbin "world_tile_attr/underwater.dat"

; ---------------------------------------------------------------------------

.export WorldPal

.segment "world_pal"

; cf/fcc0
WorldPal:
        .incbin "world_pal/bartz_world.pal"
        .incbin "world_pal/galuf_world.pal"
        .incbin "world_pal/underwater.pal"

; ---------------------------------------------------------------------------

.segment "small_font_gfx"

.export SmallFontGfx

; d1/f000
SmallFontGfx:
        .incbin "small_font.2bpp"

; ---------------------------------------------------------------------------

.segment "world_gfx"

.export WorldGfx

; db/8000
WorldGfx:
        .incbin "world_gfx/bartz_world.4bpp"
        .incbin "world_gfx/galuf_world.4bpp"
        .incbin "world_gfx/underwater.4bpp"

; ---------------------------------------------------------------------------

.segment "map_sprite_gfx"

.export MapSpriteGfx

; da/0000
MapSpriteGfx:
        .incbin "map_sprite.4bpp"

; ---------------------------------------------------------------------------

.segment "kanji_gfx"

.export KanjiGfx

; db/d000
KanjiGfx:
        .incbin "kanji.1bpp"

; ---------------------------------------------------------------------------

.segment "map_gfx"

.export MapBG3Gfx, MapBG3GfxPtrs, MapGfx, MapGfxPtrs

.enum MapBG3Gfx
        ARRAY_LENGTH = 17
        Start = MapBG3Gfx
.endenum

; dc/0000
MapBG3GfxPtrs:
        ptr_tbl MapBG3Gfx
        end_ptr MapBG3Gfx

; dc/0024
MapBG3Gfx:

.repeat MapBG3Gfx::ARRAY_LENGTH, i
        array_item MapBG3Gfx, {i} := *
        .incbin .sprintf("map_bg3_gfx/map_bg3_gfx_%04x.2bpp", i)
.endrep
MapBG3Gfx::End:

; ---------------------------------------------------------------------------

.enum MapGfx
        ARRAY_LENGTH = 39
        Start = MapGfx
.endenum

; dc/2d84
MapGfxPtrs:
        ptr_tbl_dword MapGfx
        end_ptr_dword MapGfx

; dc/2e24
MapGfx:
.repeat MapGfx::ARRAY_LENGTH, i
        array_item MapGfx, {i} := *
        .incbin .sprintf("map_gfx/map_gfx_%04x.4bpp", i)
.endrep
MapGfx::End:

; ---------------------------------------------------------------------------

.segment "map_anim_gfx"

.export MapAnimGfx

; df/9b00
MapAnimGfx:
        .incbin "map_anim.4bpp"

; ---------------------------------------------------------------------------

.segment "map_sprite_pal"

.export MapSpritePal

; df/fc00
MapSpritePal:
.repeat 32, i
        .incbin .sprintf("map_sprite_pal/map_sprite_%04x.pal", i)
.endrep

; ---------------------------------------------------------------------------
