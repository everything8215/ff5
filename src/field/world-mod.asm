; ---------------------------------------------------------------------------

.mac world_mod_xy xx, yy
        .byte yy, xx
.endmac

.mac world_mod xy_pos, size, switch, tile_ptr
        world_mod_xy xy_pos
        .byte size
        .byte switch - $01d0
        .addr tile_ptr
.endmac

; ---------------------------------------------------------------------------

WorldMod:

; ---------------------------------------------------------------------------

WorldMod::_0:

world_mod {175, 142},  1, $01e0, array_item WorldModTiles, 0
world_mod {175, 143},  1, $01e0, array_item WorldModTiles, 1
world_mod {175, 144},  1, $01e0, array_item WorldModTiles, 2

world_mod {164, 115},  2, $01e1, array_item WorldModTiles, 3
world_mod {163, 116},  4, $01e1, array_item WorldModTiles, 4
world_mod {163, 117},  3, $01e1, array_item WorldModTiles, 5
world_mod {163, 118},  3, $01e1, array_item WorldModTiles, 6
world_mod {163, 119},  2, $01e1, array_item WorldModTiles, 7

world_mod {204,  84},  1, $01e2, array_item WorldModTiles, 8
world_mod {204,  85},  1, $01e2, array_item WorldModTiles, 9
world_mod {204,  86},  1, $01e2, array_item WorldModTiles, 10
world_mod {204,  87},  1, $01e2, array_item WorldModTiles, 11

world_mod {189,  81}, 16, $01e3, array_item WorldModTiles, 12
world_mod {189,  82}, 16, $01e3, array_item WorldModTiles, 13
world_mod {189,  83}, 16, $01e3, array_item WorldModTiles, 14
world_mod {189,  84}, 15, $01e3, array_item WorldModTiles, 15
world_mod {189,  85}, 15, $01e3, array_item WorldModTiles, 16
world_mod {189,  86}, 15, $01e3, array_item WorldModTiles, 17
world_mod {189,  87}, 15, $01e3, array_item WorldModTiles, 18
world_mod {189,  88}, 16, $01e3, array_item WorldModTiles, 19
world_mod {189,  89}, 16, $01e3, array_item WorldModTiles, 20
world_mod {189,  90}, 16, $01e3, array_item WorldModTiles, 21
world_mod {189,  91}, 16, $01e3, array_item WorldModTiles, 22
world_mod {189,  92}, 16, $01e3, array_item WorldModTiles, 23
world_mod {189,  93}, 16, $01e3, array_item WorldModTiles, 24
world_mod {189,  94}, 16, $01e3, array_item WorldModTiles, 25

world_mod { 82,  76},  3, $01e4, array_item WorldModTiles, 26
world_mod { 82,  77},  3, $01e4, array_item WorldModTiles, 27

world_mod {205, 202},  1, $01e5, array_item WorldModTiles, 28

world_mod { 63, 156},  1, $01e6, array_item WorldModTiles, 43
world_mod { 60, 157}, 11, $01e6, array_item WorldModTiles, 29
world_mod { 59, 158}, 12, $01e6, array_item WorldModTiles, 30
world_mod { 58, 159}, 13, $01e6, array_item WorldModTiles, 31
world_mod { 57, 160}, 13, $01e6, array_item WorldModTiles, 32
world_mod { 57, 161}, 13, $01e6, array_item WorldModTiles, 33
world_mod { 57, 162}, 14, $01e6, array_item WorldModTiles, 34
world_mod { 57, 163}, 16, $01e6, array_item WorldModTiles, 35
world_mod { 57, 164}, 16, $01e6, array_item WorldModTiles, 36
world_mod { 57, 165}, 16, $01e6, array_item WorldModTiles, 37
world_mod { 57, 166}, 13, $01e6, array_item WorldModTiles, 38
world_mod { 58, 167}, 12, $01e6, array_item WorldModTiles, 39
world_mod { 58, 168}, 13, $01e6, array_item WorldModTiles, 40
world_mod { 58, 169}, 13, $01e6, array_item WorldModTiles, 41
world_mod { 62, 170},  9, $01e6, array_item WorldModTiles, 42

world_mod { 60, 158},  4, $01e7, array_item WorldModTiles, 44
world_mod { 60, 159},  4, $01e7, array_item WorldModTiles, 45
world_mod { 60, 160},  4, $01e7, array_item WorldModTiles, 46
world_mod { 60, 161},  4, $01e7, array_item WorldModTiles, 47

world_mod {146, 115},  1, $01ef, array_item WorldModTiles, 48

world_mod {147,  78},  1, $01db, array_item WorldModTiles, 111
world_mod {146,  79},  3, $01db, array_item WorldModTiles, 112
world_mod {147,  80},  1, $01db, array_item WorldModTiles, 113

world_mod {151,  78},  1, $01dc, array_item WorldModTiles, 111
world_mod {150,  79},  3, $01dc, array_item WorldModTiles, 112
world_mod {151,  80},  1, $01dc, array_item WorldModTiles, 113

world_mod {151,  82},  1, $01dd, array_item WorldModTiles, 111
world_mod {150,  83},  3, $01dd, array_item WorldModTiles, 112
world_mod {151,  84},  1, $01dd, array_item WorldModTiles, 113

world_mod {147,  82},  1, $01de, array_item WorldModTiles, 111
world_mod {146,  83},  3, $01de, array_item WorldModTiles, 112
world_mod {147,  84},  1, $01de, array_item WorldModTiles, 113

; ---------------------------------------------------------------------------

WorldMod::_1:

world_mod {112,  95},  2, $01e8, array_item WorldModTiles, 49
world_mod {110,  96},  5, $01e8, array_item WorldModTiles, 50
world_mod {107,  97}, 10, $01e8, array_item WorldModTiles, 51
world_mod {107,  98}, 10, $01e8, array_item WorldModTiles, 52
world_mod {107,  99}, 10, $01e8, array_item WorldModTiles, 53
world_mod {107, 100}, 10, $01e8, array_item WorldModTiles, 54
world_mod {107, 101},  9, $01e8, array_item WorldModTiles, 55
world_mod {107, 102},  9, $01e8, array_item WorldModTiles, 56
world_mod {107, 103},  9, $01e8, array_item WorldModTiles, 57
world_mod {108, 104},  8, $01e8, array_item WorldModTiles, 58
world_mod {109, 105},  6, $01e8, array_item WorldModTiles, 59
world_mod {110, 106},  3, $01e8, array_item WorldModTiles, 60
world_mod {111, 107},  2, $01e8, array_item WorldModTiles, 61

world_mod {178, 160},  1, $01e9, array_item WorldModTiles, 62
world_mod {176, 161},  5, $01e9, array_item WorldModTiles, 63
world_mod {177, 162},  4, $01e9, array_item WorldModTiles, 64
world_mod {177, 163},  5, $01e9, array_item WorldModTiles, 65
world_mod {176, 164},  6, $01e9, array_item WorldModTiles, 66
world_mod {177, 165},  3, $01e9, array_item WorldModTiles, 67

world_mod {167, 160},  1, $01ea, array_item WorldModTiles, 68
world_mod {167, 161},  1, $01ea, array_item WorldModTiles, 69

world_mod {178, 160},  1, $01eb, array_item WorldModTiles, 71
world_mod {176, 161},  5, $01eb, array_item WorldModTiles, 72
world_mod {177, 162},  4, $01eb, array_item WorldModTiles, 73
world_mod {177, 163},  5, $01eb, array_item WorldModTiles, 74
world_mod {176, 164},  6, $01eb, array_item WorldModTiles, 75
world_mod {177, 165},  3, $01eb, array_item WorldModTiles, 76

world_mod { 47, 119},  8, $01ec, array_item WorldModTiles, 77
world_mod { 45, 120}, 10, $01ec, array_item WorldModTiles, 78
world_mod { 41, 121}, 14, $01ec, array_item WorldModTiles, 79
world_mod { 40, 122}, 15, $01ec, array_item WorldModTiles, 80
world_mod { 40, 123}, 15, $01ec, array_item WorldModTiles, 81
world_mod { 40, 124},  9, $01ec, array_item WorldModTiles, 82
world_mod { 40, 125},  8, $01ec, array_item WorldModTiles, 83
world_mod { 34, 126}, 17, $01ec, array_item WorldModTiles, 84
world_mod { 34, 127}, 18, $01ec, array_item WorldModTiles, 85
world_mod { 34, 128}, 18, $01ec, array_item WorldModTiles, 86
world_mod { 35, 129}, 17, $01ec, array_item WorldModTiles, 87
world_mod { 38, 130}, 13, $01ec, array_item WorldModTiles, 88
world_mod { 40, 131}, 10, $01ec, array_item WorldModTiles, 89
world_mod { 40, 132},  7, $01ec, array_item WorldModTiles, 90
world_mod { 41, 133},  4, $01ec, array_item WorldModTiles, 91
world_mod { 46, 134},  2, $01ec, array_item WorldModTiles, 92
world_mod { 45, 135},  3, $01ec, array_item WorldModTiles, 93
world_mod { 45, 136},  3, $01ec, array_item WorldModTiles, 94

world_mod {213,  36},  9, $01ed, array_item WorldModTiles, 95
world_mod {213,  37},  9, $01ed, array_item WorldModTiles, 96
world_mod {213,  38},  9, $01ed, array_item WorldModTiles, 97
world_mod {213,  39},  9, $01ed, array_item WorldModTiles, 98
world_mod {213,  40},  9, $01ed, array_item WorldModTiles, 99
world_mod {213,  41},  9, $01ed, array_item WorldModTiles, 100
world_mod {213,  42},  9, $01ed, array_item WorldModTiles, 101
world_mod {213,  43},  9, $01ed, array_item WorldModTiles, 102

; ---------------------------------------------------------------------------

WorldMod::_2:

world_mod {161, 150},  1, $01f0, array_item WorldModTiles, 103

world_mod {205, 198},  3, $01f2, array_item WorldModTiles, 104
world_mod {205, 199},  3, $01f2, array_item WorldModTiles, 105
world_mod {205, 200},  3, $01f2, array_item WorldModTiles, 106
world_mod {205, 201},  3, $01f2, array_item WorldModTiles, 107

world_mod {145, 112},  9, $01f3, array_item WorldModTiles, 114
world_mod {138, 113}, 16, $01f3, array_item WorldModTiles, 115
world_mod {138, 114}, 16, $01f3, array_item WorldModTiles, 116
world_mod {138, 115}, 16, $01f3, array_item WorldModTiles, 116
world_mod {138, 116}, 16, $01f3, array_item WorldModTiles, 116
world_mod {138, 117}, 16, $01f3, array_item WorldModTiles, 116
world_mod {138, 118}, 16, $01f3, array_item WorldModTiles, 116
world_mod {138, 119}, 16, $01f3, array_item WorldModTiles, 116
world_mod {146, 120},  3, $01f3, array_item WorldModTiles, 117
world_mod {132,  83},  3, $01f3, array_item WorldModTiles, 124
world_mod {132,  84},  3, $01f3, array_item WorldModTiles, 125
world_mod {132,  85},  3, $01f3, array_item WorldModTiles, 125
world_mod {131,  86},  4, $01f3, array_item WorldModTiles, 126
world_mod {130,  87},  3, $01f3, array_item WorldModTiles, 127

world_mod {101,  97},  2, $01f7, array_item WorldModTiles, 128
world_mod {101,  98},  3, $01f7, array_item WorldModTiles, 129
world_mod {103,  99},  2, $01f7, array_item WorldModTiles, 130

world_mod {182, 130},  7, $01f5, array_item WorldModTiles, 131
world_mod {180, 131},  9, $01f5, array_item WorldModTiles, 132
world_mod {180, 132},  7, $01f5, array_item WorldModTiles, 133
world_mod {178, 133}, 11, $01f5, array_item WorldModTiles, 134
world_mod {178, 134}, 11, $01f5, array_item WorldModTiles, 135
world_mod {178, 135}, 12, $01f5, array_item WorldModTiles, 136
world_mod {178, 136}, 12, $01f5, array_item WorldModTiles, 137
world_mod {178, 137}, 12, $01f5, array_item WorldModTiles, 138
world_mod {178, 138}, 13, $01f5, array_item WorldModTiles, 139
world_mod {180, 139}, 10, $01f5, array_item WorldModTiles, 140
world_mod {180, 140}, 11, $01f5, array_item WorldModTiles, 141
world_mod {180, 141},  9, $01f5, array_item WorldModTiles, 142
world_mod {183, 142},  3, $01f5, array_item WorldModTiles, 143
world_mod {183, 143},  3, $01f5, array_item WorldModTiles, 144
world_mod {183, 144},  2, $01f5, array_item WorldModTiles, 145
world_mod {183, 145},  2, $01f5, array_item WorldModTiles, 145
world_mod {183, 146},  2, $01f5, array_item WorldModTiles, 145
world_mod {183, 147},  2, $01f5, array_item WorldModTiles, 145

world_mod { 76, 114},  5, $01f6, array_item WorldModTiles, 162
world_mod { 73, 115}, 15, $01f6, array_item WorldModTiles, 163
world_mod { 73, 116}, 12, $01f6, array_item WorldModTiles, 164
world_mod { 73, 117}, 12, $01f6, array_item WorldModTiles, 165
world_mod { 73, 118}, 14, $01f6, array_item WorldModTiles, 166
world_mod { 74, 119}, 13, $01f6, array_item WorldModTiles, 167
world_mod { 74, 120}, 13, $01f6, array_item WorldModTiles, 168
world_mod { 74, 121}, 11, $01f6, array_item WorldModTiles, 169
world_mod { 74, 122}, 11, $01f6, array_item WorldModTiles, 170
world_mod { 75, 123},  9, $01f6, array_item WorldModTiles, 171
world_mod { 76, 124},  7, $01f6, array_item WorldModTiles, 172
world_mod { 78, 125},  4, $01f6, array_item WorldModTiles, 173

world_mod {218,  42},  4, $01f7, array_item WorldModTiles, 187
world_mod {215,  43},  8, $01f7, array_item WorldModTiles, 188
world_mod {214,  44}, 10, $01f7, array_item WorldModTiles, 189
world_mod {213,  45}, 11, $01f7, array_item WorldModTiles, 190
world_mod {213,  46}, 12, $01f7, array_item WorldModTiles, 191
world_mod {214,  47}, 11, $01f7, array_item WorldModTiles, 192
world_mod {214,  48}, 11, $01f7, array_item WorldModTiles, 193
world_mod {214,  49}, 10, $01f7, array_item WorldModTiles, 194
world_mod {214,  50}, 10, $01f7, array_item WorldModTiles, 195
world_mod {215,  51},  8, $01f7, array_item WorldModTiles, 196
world_mod {216,  52},  6, $01f7, array_item WorldModTiles, 197
world_mod {218,  53},  2, $01f7, array_item WorldModTiles, 198

world_mod {171,  40},  8, $01f7, array_item WorldModTiles, 213
world_mod {171,  41},  8, $01f7, array_item WorldModTiles, 214
world_mod {170,  42},  9, $01f7, array_item WorldModTiles, 215
world_mod {169,  43}, 10, $01f7, array_item WorldModTiles, 216
world_mod {169,  44}, 10, $01f7, array_item WorldModTiles, 217
world_mod {168,  45}, 11, $01f7, array_item WorldModTiles, 218
world_mod {168,  46}, 11, $01f7, array_item WorldModTiles, 219
world_mod {168,  47}, 12, $01f7, array_item WorldModTiles, 220
world_mod {167,  48}, 13, $01f7, array_item WorldModTiles, 221
world_mod {168,  49}, 12, $01f7, array_item WorldModTiles, 222
world_mod {169,  50}, 11, $01f7, array_item WorldModTiles, 223
world_mod {170,  51},  9, $01f7, array_item WorldModTiles, 224
world_mod {172,  52},  6, $01f7, array_item WorldModTiles, 225
world_mod {174,  53},  4, $01f7, array_item WorldModTiles, 226
world_mod {176,  54},  2, $01f7, array_item WorldModTiles, 227

world_mod { 37,  35},  2, $01f7, array_item WorldModTiles, 240
world_mod { 33,  36},  6, $01f7, array_item WorldModTiles, 241
world_mod { 31,  37}, 11, $01f7, array_item WorldModTiles, 242
world_mod { 31,  38}, 11, $01f7, array_item WorldModTiles, 243
world_mod { 29,  39}, 13, $01f7, array_item WorldModTiles, 244
world_mod { 29,  40}, 13, $01f7, array_item WorldModTiles, 245
world_mod { 30,  41}, 12, $01f7, array_item WorldModTiles, 246
world_mod { 30,  42}, 11, $01f7, array_item WorldModTiles, 247
world_mod { 31,  43},  9, $01f7, array_item WorldModTiles, 248
world_mod { 31,  44},  9, $01f7, array_item WorldModTiles, 249
world_mod { 32,  45},  8, $01f7, array_item WorldModTiles, 250
world_mod { 32,  46},  4, $01f7, array_item WorldModTiles, 251

world_mod {208, 111},  5, $01f7, array_item WorldModTiles, 267
world_mod {206, 112}, 10, $01f7, array_item WorldModTiles, 268
world_mod {206, 113}, 11, $01f7, array_item WorldModTiles, 269
world_mod {206, 114}, 11, $01f7, array_item WorldModTiles, 270
world_mod {206, 115}, 13, $01f7, array_item WorldModTiles, 271
world_mod {207, 116}, 12, $01f7, array_item WorldModTiles, 272
world_mod {207, 117}, 13, $01f7, array_item WorldModTiles, 273
world_mod {209, 118},  9, $01f7, array_item WorldModTiles, 274
world_mod {208, 119},  8, $01f7, array_item WorldModTiles, 275
world_mod {209, 120},  7, $01f7, array_item WorldModTiles, 276
world_mod {209, 121},  3, $01f7, array_item WorldModTiles, 277
world_mod {210, 122},  2, $01f7, array_item WorldModTiles, 278

world_mod { 76, 124},  2, $01f7, array_item WorldModTiles, 296
world_mod { 77, 125},  2, $01f7, array_item WorldModTiles, 297
world_mod { 77, 126},  2, $01f7, array_item WorldModTiles, 298

; ---------------------------------------------------------------------------

WorldMod::_3:

world_mod {169, 164},  1, $01ea, array_item WorldModTiles, 70

; ---------------------------------------------------------------------------

WorldMod::_4:

world_mod {206, 199},  1, $01f2, array_item WorldModTiles, 108
world_mod {205, 200},  3, $01f2, array_item WorldModTiles, 109
world_mod {206, 201},  1, $01f2, array_item WorldModTiles, 110

world_mod {145, 113},  8, $01f3, array_item WorldModTiles, 118
world_mod {139, 114}, 14, $01f3, array_item WorldModTiles, 119
world_mod {139, 115}, 13, $01f3, array_item WorldModTiles, 120
world_mod {140, 116}, 10, $01f3, array_item WorldModTiles, 121
world_mod {140, 117},  7, $01f3, array_item WorldModTiles, 122
world_mod {141, 118},  7, $01f3, array_item WorldModTiles, 122
world_mod {146, 119},  3, $01f3, array_item WorldModTiles, 123

world_mod {182, 128},  4, $01f5, array_item WorldModTiles, 146
world_mod {181, 129},  6, $01f5, array_item WorldModTiles, 147
world_mod {180, 130},  7, $01f5, array_item WorldModTiles, 148
world_mod {179, 131},  9, $01f5, array_item WorldModTiles, 149
world_mod {179, 132}, 10, $01f5, array_item WorldModTiles, 150
world_mod {178, 133}, 11, $01f5, array_item WorldModTiles, 151
world_mod {178, 134}, 12, $01f5, array_item WorldModTiles, 152
world_mod {178, 135}, 12, $01f5, array_item WorldModTiles, 153
world_mod {178, 136}, 12, $01f5, array_item WorldModTiles, 154
world_mod {178, 137}, 12, $01f5, array_item WorldModTiles, 155
world_mod {178, 138}, 12, $01f5, array_item WorldModTiles, 156
world_mod {179, 139}, 11, $01f5, array_item WorldModTiles, 157
world_mod {180, 140},  8, $01f5, array_item WorldModTiles, 158
world_mod {181, 141},  6, $01f5, array_item WorldModTiles, 159
world_mod {184, 142},  3, $01f5, array_item WorldModTiles, 160
world_mod {184, 143},  2, $01f5, array_item WorldModTiles, 161
world_mod {184, 144},  2, $01f5, array_item WorldModTiles, 161

world_mod { 81, 112},  3, $01f6, array_item WorldModTiles, 174
world_mod { 76, 113},  9, $01f6, array_item WorldModTiles, 175
world_mod { 75, 114}, 11, $01f6, array_item WorldModTiles, 176
world_mod { 74, 115}, 12, $01f6, array_item WorldModTiles, 177
world_mod { 74, 116}, 12, $01f6, array_item WorldModTiles, 178
world_mod { 74, 117}, 12, $01f6, array_item WorldModTiles, 179
world_mod { 74, 118}, 12, $01f6, array_item WorldModTiles, 180
world_mod { 74, 119}, 12, $01f6, array_item WorldModTiles, 181
world_mod { 75, 120}, 11, $01f6, array_item WorldModTiles, 182
world_mod { 75, 121}, 11, $01f6, array_item WorldModTiles, 183
world_mod { 75, 122},  9, $01f6, array_item WorldModTiles, 184
world_mod { 77, 123},  6, $01f6, array_item WorldModTiles, 185
world_mod { 77, 124},  6, $01f6, array_item WorldModTiles, 186

world_mod {212, 108},  1, $01f7, array_item WorldModTiles, 279
world_mod {212, 109},  4, $01f7, array_item WorldModTiles, 280
world_mod {212, 110},  5, $01f7, array_item WorldModTiles, 281
world_mod {211, 111},  7, $01f7, array_item WorldModTiles, 282
world_mod {208, 112}, 10, $01f7, array_item WorldModTiles, 283
world_mod {207, 113}, 12, $01f7, array_item WorldModTiles, 284
world_mod {207, 114}, 12, $01f7, array_item WorldModTiles, 285
world_mod {207, 115}, 12, $01f7, array_item WorldModTiles, 286
world_mod {207, 116}, 12, $01f7, array_item WorldModTiles, 287
world_mod {207, 117}, 12, $01f7, array_item WorldModTiles, 288
world_mod {208, 118}, 11, $01f7, array_item WorldModTiles, 289
world_mod {208, 119}, 10, $01f7, array_item WorldModTiles, 290
world_mod {208, 120},  8, $01f7, array_item WorldModTiles, 291
world_mod {208, 121},  6, $01f7, array_item WorldModTiles, 292
world_mod {209, 122},  5, $01f7, array_item WorldModTiles, 293
world_mod {212, 123},  2, $01f7, array_item WorldModTiles, 294
world_mod {212, 124},  2, $01f7, array_item WorldModTiles, 295

world_mod { 34,  34},  7, $01f7, array_item WorldModTiles, 252
world_mod { 32,  35}, 10, $01f7, array_item WorldModTiles, 253
world_mod { 31,  36}, 11, $01f7, array_item WorldModTiles, 254
world_mod { 31,  37}, 11, $01f7, array_item WorldModTiles, 255
world_mod { 31,  38}, 11, $01f7, array_item WorldModTiles, 256
world_mod { 31,  39}, 11, $01f7, array_item WorldModTiles, 257
world_mod { 31,  40}, 11, $01f7, array_item WorldModTiles, 258
world_mod { 31,  41}, 11, $01f7, array_item WorldModTiles, 259
world_mod { 31,  42}, 12, $01f7, array_item WorldModTiles, 260
world_mod { 31,  43}, 12, $01f7, array_item WorldModTiles, 261
world_mod { 31,  44}, 12, $01f7, array_item WorldModTiles, 262
world_mod { 31,  45}, 12, $01f7, array_item WorldModTiles, 263
world_mod { 31,  46}, 11, $01f7, array_item WorldModTiles, 264
world_mod { 34,  47},  8, $01f7, array_item WorldModTiles, 265
world_mod { 41,  48},  1, $01f7, array_item WorldModTiles, 266

world_mod {170,  41},  9, $01f7, array_item WorldModTiles, 228
world_mod {169,  42}, 10, $01f7, array_item WorldModTiles, 229
world_mod {169,  43}, 10, $01f7, array_item WorldModTiles, 230
world_mod {168,  44}, 12, $01f7, array_item WorldModTiles, 231
world_mod {168,  45}, 12, $01f7, array_item WorldModTiles, 232
world_mod {168,  46}, 12, $01f7, array_item WorldModTiles, 233
world_mod {168,  47}, 12, $01f7, array_item WorldModTiles, 234
world_mod {168,  48}, 12, $01f7, array_item WorldModTiles, 235
world_mod {169,  49}, 11, $01f7, array_item WorldModTiles, 236
world_mod {170,  50},  9, $01f7, array_item WorldModTiles, 237
world_mod {170,  51},  8, $01f7, array_item WorldModTiles, 238
world_mod {171,  52},  5, $01f7, array_item WorldModTiles, 239

world_mod {218,  40},  4, $01f7, array_item WorldModTiles, 199
world_mod {216,  41},  7, $01f7, array_item WorldModTiles, 200
world_mod {214,  42},  9, $01f7, array_item WorldModTiles, 201
world_mod {214,  43}, 10, $01f7, array_item WorldModTiles, 202
world_mod {214,  44}, 11, $01f7, array_item WorldModTiles, 203
world_mod {214,  45}, 11, $01f7, array_item WorldModTiles, 204
world_mod {214,  46}, 11, $01f7, array_item WorldModTiles, 205
world_mod {214,  47}, 11, $01f7, array_item WorldModTiles, 206
world_mod {214,  48}, 11, $01f7, array_item WorldModTiles, 207
world_mod {214,  49}, 10, $01f7, array_item WorldModTiles, 208
world_mod {214,  50}, 10, $01f7, array_item WorldModTiles, 209
world_mod {215,  51},  8, $01f7, array_item WorldModTiles, 210
world_mod {216,  52},  6, $01f7, array_item WorldModTiles, 211
world_mod {218,  53},  2, $01f7, array_item WorldModTiles, 212

WorldMod::End := *

; ---------------------------------------------------------------------------

WorldModTiles::_0: .byte   $9d
WorldModTiles::_1: .byte   $ad
WorldModTiles::_2: .byte   $bd

WorldModTiles::_3: .byte   $27,$9f
WorldModTiles::_4: .byte   $18,$05,$05,$16
WorldModTiles::_5: .byte   $28,$05,$06
WorldModTiles::_6: .byte   $05,$05,$16
WorldModTiles::_7: .byte   $07,$07

WorldModTiles::_8: .byte   $9d
WorldModTiles::_9: .byte   $ad
WorldModTiles::_10: .byte   $ad
WorldModTiles::_11: .byte   $bd

WorldModTiles::_12: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_13: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$33,$54,$50,$33
WorldModTiles::_14: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$05,$53,$51,$05
WorldModTiles::_15: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$06,$07,$07,$08
WorldModTiles::_16: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$06,$17,$17,$17,$18
WorldModTiles::_17: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$70,$71,$26,$27,$27,$27,$28
WorldModTiles::_18: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$70,$43,$43,$71,$05,$05
WorldModTiles::_19: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$70,$43,$43,$71
WorldModTiles::_20: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$60
WorldModTiles::_21: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$70
WorldModTiles::_22: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_23: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_24: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_25: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87

WorldModTiles::_26: .byte   $05,$a1,$a0
WorldModTiles::_27: .byte   $05,$a2,$05

WorldModTiles::_28: .byte   $81
WorldModTiles::_29: .byte   $28,$05,$05,$05,$26,$27,$27,$27,$17,$17,$17
WorldModTiles::_30: .byte   $28,$05,$05,$a0,$79,$7d,$05,$a2,$05,$26,$17,$17
WorldModTiles::_31: .byte   $18,$05,$05,$79,$7b,$7a,$9c,$a1,$79,$7d,$05,$16,$17
WorldModTiles::_32: .byte   $17,$18,$05,$a2,$89,$8a,$95,$7c,$7b,$7a,$9c,$05,$16
WorldModTiles::_33: .byte   $17,$28,$05,$79,$7b,$7a,$95,$95,$95,$95,$7c,$7d,$26
WorldModTiles::_34: .byte   $18,$05,$05,$89,$8a,$95,$95,$95,$95,$95,$8c,$8d,$05,$16
WorldModTiles::_35: .byte   $18,$05,$05,$a0,$9b,$95,$95,$95,$95,$95,$7c,$7d,$05,$16,$17,$17
WorldModTiles::_36: .byte   $17,$08,$05,$79,$7a,$95,$95,$95,$95,$8c,$8b,$8d,$05,$16,$17,$17
WorldModTiles::_37: .byte   $17,$18,$05,$89,$8b,$8a,$8c,$8b,$8a,$9c,$a2,$05,$06,$17,$17,$17
WorldModTiles::_38: .byte   $17,$17,$08,$05,$a1,$89,$8d,$a0,$89,$8d,$05,$05,$16
WorldModTiles::_39: .byte   $17,$17,$07,$07,$08,$05,$05,$05,$06,$07,$07,$17
WorldModTiles::_40: .byte   $17,$17,$17,$17,$17,$07,$07,$07,$17,$17,$17,$17,$17
WorldModTiles::_41: .byte   $17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17
WorldModTiles::_42: .byte   $17,$17,$17,$17,$17,$17,$17,$17,$17

WorldModTiles::_43: .byte   $26

WorldModTiles::_44: .byte   $79,$7b,$7b,$7b
WorldModTiles::_45: .byte   $9b,$8c,$8b,$8a
WorldModTiles::_46: .byte   $9b,$9c,$9d,$9b
WorldModTiles::_47: .byte   $9b,$7c,$7b,$7a

WorldModTiles::_48: .byte   $97

WorldModTiles::_49: .byte   $87,$87
WorldModTiles::_50: .byte   $87,$87,$87,$87,$87
WorldModTiles::_51: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_52: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_53: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_54: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_55: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_56: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_57: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_58: .byte   $87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_59: .byte   $87,$87,$87,$87,$87,$87
WorldModTiles::_60: .byte   $87,$87,$87
WorldModTiles::_61: .byte   $87,$87

WorldModTiles::_62: .byte   $84
WorldModTiles::_63: .byte   $84,$87,$87,$87,$84
WorldModTiles::_64: .byte   $84,$84,$87,$84
WorldModTiles::_65: .byte   $85,$86,$84,$87,$84
WorldModTiles::_66: .byte   $84,$87,$84,$87,$84,$84
WorldModTiles::_67: .byte   $84,$87,$84

WorldModTiles::_68: .byte   $05
WorldModTiles::_69: .byte   $05

WorldModTiles::_70: .byte   $42

WorldModTiles::_71: .byte   $87
WorldModTiles::_72: .byte   $87,$87,$87,$87,$87
WorldModTiles::_73: .byte   $87,$87,$87,$87
WorldModTiles::_74: .byte   $87,$87,$87,$87,$87
WorldModTiles::_75: .byte   $87,$87,$87,$87,$87,$87
WorldModTiles::_76: .byte   $87,$87,$87

WorldModTiles::_77: .byte   $21,$21,$21,$21,$21,$21,$21,$21
WorldModTiles::_78: .byte   $21,$22,$79,$7b,$7b,$7b,$7b,$7b,$7b,$7d
WorldModTiles::_79: .byte   $21,$21,$21,$22,$79,$7b,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$9c
WorldModTiles::_80: .byte   $12,$79,$7b,$7b,$7b,$7a,$a5,$a5,$8c,$8b,$8b,$8b,$8b,$8b,$8d
WorldModTiles::_81: .byte   $12,$9b,$a5,$a5,$6e,$a5,$a5,$a5,$8d,$00,$01,$01,$01,$01,$01
WorldModTiles::_82: .byte   $12,$9b,$6e,$a5,$8c,$8b,$8b,$8d,$00
WorldModTiles::_83: .byte   $22,$9b,$a5,$8c,$8d,$00,$01,$01
WorldModTiles::_84: .byte   $20,$21,$21,$21,$21,$22,$79,$7a,$a5,$7c,$7d,$20,$21,$21,$21,$21,$21
WorldModTiles::_85: .byte   $79,$7b,$7b,$7b,$7b,$7b,$7a,$a5,$a5,$a5,$7c,$7b,$7b,$7b,$7b,$7b,$7d,$20
WorldModTiles::_86: .byte   $89,$8a,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$a5,$6e,$7c,$7d
WorldModTiles::_87: .byte   $89,$8b,$8b,$8a,$a5,$a5,$6e,$a5,$a5,$a5,$6e,$a5,$6e,$a5,$a5,$8c,$8d
WorldModTiles::_88: .byte   $89,$8b,$8a,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$8c,$8d
WorldModTiles::_89: .byte   $9b,$a5,$6e,$a5,$a5,$a5,$8c,$8b,$8b,$8d
WorldModTiles::_90: .byte   $89,$8a,$a5,$a5,$8c,$8b,$8d
WorldModTiles::_91: .byte   $89,$8b,$8b,$8d
WorldModTiles::_92: .byte   $05,$09
WorldModTiles::_93: .byte   $0b,$05,$19
WorldModTiles::_94: .byte   $1b,$05,$29

WorldModTiles::_95: .byte   $11,$05,$05,$11,$11,$11,$05,$05,$11
WorldModTiles::_96: .byte   $05,$11,$11,$05,$05,$05,$11,$11,$05
WorldModTiles::_97: .byte   $05,$11,$05,$11,$11,$11,$05,$11,$05
WorldModTiles::_98: .byte   $05,$11,$11,$11,$11,$11,$11,$11,$05
WorldModTiles::_99: .byte   $05,$11,$11,$05,$11,$05,$11,$11,$05
WorldModTiles::_100: .byte   $05,$11,$11,$05,$11,$05,$11,$11,$05
WorldModTiles::_101: .byte   $05,$11,$11,$11,$05,$11,$11,$11,$05
WorldModTiles::_102: .byte   $11,$05,$11,$11,$11,$11,$11,$05,$11

WorldModTiles::_103: .byte   $2a

WorldModTiles::_104: .byte   $05,$05,$05,$05
WorldModTiles::_105: .byte   $73,$43,$71
WorldModTiles::_106: .byte   $64,$81,$60
WorldModTiles::_107: .byte   $53,$33,$51

WorldModTiles::_108: .byte   $04
WorldModTiles::_109: .byte   $04,$19,$04
WorldModTiles::_110: .byte   $04

WorldModTiles::_111: .byte   $11
WorldModTiles::_112: .byte   $11,$11,$11
WorldModTiles::_113: .byte   $11

WorldModTiles::_114: .byte   $73,$74,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_115: .byte   $87,$87,$87,$87,$87,$87,$70,$74,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_116: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_117: .byte   $87,$87,$87

WorldModTiles::_118: .byte   $04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles::_119: .byte   $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles::_120: .byte   $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles::_121: .byte   $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles::_122: .byte   $04,$04,$04,$04,$04,$04,$04
WorldModTiles::_123: .byte   $04,$04,$04
WorldModTiles::_124: .byte   $18,$05,$06
WorldModTiles::_125: .byte   $18,$05,$16
WorldModTiles::_126: .byte   $27,$28,$05,$16
WorldModTiles::_127: .byte   $28,$05,$05

WorldModTiles::_128: .byte   $26,$27
WorldModTiles::_129: .byte   $05,$05,$26
WorldModTiles::_130: .byte   $05,$16

WorldModTiles::_131: .byte   $21,$21,$21,$22,$05,$05,$05
WorldModTiles::_132: .byte   $20,$22,$73,$43,$43,$71,$00,$01,$01
WorldModTiles::_133: .byte   $05,$73,$74,$87,$87,$60,$20
WorldModTiles::_134: .byte   $05,$05,$73,$74,$87,$87,$87,$70,$71,$20,$21
WorldModTiles::_135: .byte   $05,$05,$64,$87,$82,$83,$83,$84,$70,$71,$05
WorldModTiles::_136: .byte   $05,$73,$74,$82,$ae,$88,$88,$af,$84,$60,$05,$16
WorldModTiles::_137: .byte   $05,$64,$87,$85,$88,$88,$88,$88,$86,$70,$71,$16
WorldModTiles::_138: .byte   $05,$64,$87,$85,$88,$88,$88,$88,$86,$87,$60,$26
WorldModTiles::_139: .byte   $05,$53,$54,$90,$be,$88,$88,$bf,$92,$87,$70,$71,$26
WorldModTiles::_140: .byte   $53,$54,$90,$91,$91,$92,$87,$87,$87,$70
WorldModTiles::_141: .byte   $05,$53,$54,$50,$33,$33,$54,$87,$87,$87,$87
WorldModTiles::_142: .byte   $05,$05,$53,$51,$05,$05,$53,$54,$87
WorldModTiles::_143: .byte   $05,$05,$05
WorldModTiles::_144: .byte   $08,$05,$05
WorldModTiles::_145: .byte   $18,$05

WorldModTiles::_146: .byte   $33,$42,$42,$31
WorldModTiles::_147: .byte   $33,$43,$52,$52,$41,$21
WorldModTiles::_148: .byte   $33,$43,$53,$62,$62,$51,$31
WorldModTiles::_149: .byte   $23,$43,$53,$63,$04,$04,$61,$41,$31
WorldModTiles::_150: .byte   $33,$53,$63,$04,$04,$04,$04,$51,$41,$21
WorldModTiles::_151: .byte   $23,$43,$63,$04,$04,$04,$04,$04,$61,$51,$31
WorldModTiles::_152: .byte   $23,$53,$04,$04,$22,$22,$22,$22,$04,$61,$41,$21
WorldModTiles::_153: .byte   $23,$63,$04,$22,$22,$22,$22,$22,$22,$04,$51,$31
WorldModTiles::_154: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$61,$41
WorldModTiles::_155: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$51
WorldModTiles::_156: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$61
WorldModTiles::_157: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$04,$04,$04
WorldModTiles::_158: .byte   $14,$13,$04,$04,$04,$04,$04,$04
WorldModTiles::_159: .byte   $14,$12,$12,$13,$19,$19
WorldModTiles::_160: .byte   $23,$19,$19
WorldModTiles::_161: .byte   $23,$19

WorldModTiles::_162: .byte   $05,$05,$05,$05,$05
WorldModTiles::_163: .byte   $05,$05,$05,$05,$05,$73,$71,$05,$73,$43,$71,$69,$6b,$6d,$69
WorldModTiles::_164: .byte   $05,$05,$05,$73,$43,$74,$70,$43,$74,$87,$70,$71
WorldModTiles::_165: .byte   $05,$05,$73,$74,$87,$82,$83,$83,$84,$87,$87,$60
WorldModTiles::_166: .byte   $05,$05,$64,$87,$82,$ae,$88,$88,$af,$84,$87,$60,$05,$69
WorldModTiles::_167: .byte   $05,$53,$54,$85,$88,$88,$88,$88,$86,$87,$70,$71,$05
WorldModTiles::_168: .byte   $05,$05,$64,$85,$88,$88,$88,$88,$86,$87,$50,$51,$05
WorldModTiles::_169: .byte   $08,$05,$64,$90,$be,$88,$88,$bf,$92,$50,$51
WorldModTiles::_170: .byte   $18,$05,$53,$54,$90,$91,$91,$92,$50,$51,$06
WorldModTiles::_171: .byte   $08,$05,$53,$54,$50,$33,$33,$51,$06
WorldModTiles::_172: .byte   $07,$08,$53,$51,$05,$06,$07
WorldModTiles::_173: .byte   $08,$05,$05,$26

WorldModTiles::_174: .byte   $33,$42,$31
WorldModTiles::_175: .byte   $33,$42,$42,$42,$42,$43,$52,$41,$31
WorldModTiles::_176: .byte   $33,$43,$52,$52,$52,$52,$53,$62,$51,$41,$21
WorldModTiles::_177: .byte   $23,$43,$53,$62,$62,$62,$62,$63,$04,$61,$51,$21
WorldModTiles::_178: .byte   $23,$53,$63,$04,$04,$04,$04,$04,$04,$04,$61,$21
WorldModTiles::_179: .byte   $23,$63,$04,$04,$22,$22,$22,$22,$04,$04,$04,$21
WorldModTiles::_180: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_181: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_182: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_183: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$11,$10
WorldModTiles::_184: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$11
WorldModTiles::_185: .byte   $13,$04,$04,$04,$04,$11
WorldModTiles::_186: .byte   $14,$12,$12,$12,$12,$10

WorldModTiles::_187: .byte   $27,$27,$27,$27
WorldModTiles::_188: .byte   $27,$27,$28,$73,$43,$43,$71,$26
WorldModTiles::_189: .byte   $28,$05,$73,$43,$74,$87,$87,$60,$05,$16
WorldModTiles::_190: .byte   $18,$73,$43,$74,$82,$83,$83,$84,$70,$71,$26
WorldModTiles::_191: .byte   $18,$53,$54,$82,$ae,$88,$88,$af,$84,$70,$71,$16
WorldModTiles::_192: .byte   $08,$64,$85,$88,$88,$88,$88,$86,$87,$60,$16
WorldModTiles::_193: .byte   $18,$64,$85,$88,$88,$88,$88,$86,$50,$51,$16
WorldModTiles::_194: .byte   $18,$64,$90,$be,$88,$88,$bf,$92,$60,$06
WorldModTiles::_195: .byte   $18,$53,$54,$90,$91,$91,$92,$50,$51,$16
WorldModTiles::_196: .byte   $08,$53,$33,$54,$50,$33,$51,$06
WorldModTiles::_197: .byte   $07,$08,$53,$51,$06,$07
WorldModTiles::_198: .byte   $07,$07
WorldModTiles::_199: .byte   $33,$42,$42,$31
WorldModTiles::_200: .byte   $33,$42,$43,$52,$52,$41,$21
WorldModTiles::_201: .byte   $23,$42,$43,$52,$53,$62,$62,$51,$31
WorldModTiles::_202: .byte   $23,$52,$53,$62,$63,$04,$04,$61,$41,$31
WorldModTiles::_203: .byte   $23,$62,$63,$04,$04,$04,$04,$04,$51,$41,$21
WorldModTiles::_204: .byte   $23,$04,$04,$22,$22,$22,$22,$04,$61,$51,$21
WorldModTiles::_205: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$61,$21
WorldModTiles::_206: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_207: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$11,$10
WorldModTiles::_208: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$21
WorldModTiles::_209: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$11,$10
WorldModTiles::_210: .byte   $14,$13,$04,$04,$04,$04,$11,$10
WorldModTiles::_211: .byte   $14,$12,$13,$11,$12,$10
WorldModTiles::_212: .byte   $14,$10
WorldModTiles::_213: .byte   $54,$87,$87,$87,$87,$87,$87,$87
WorldModTiles::_214: .byte   $53,$54,$87,$87,$87,$87,$50,$33
WorldModTiles::_215: .byte   $05,$05,$64,$87,$87,$87,$87,$60,$06
WorldModTiles::_216: .byte   $05,$05,$73,$74,$87,$87,$87,$50,$51,$16
WorldModTiles::_217: .byte   $05,$73,$74,$82,$83,$83,$84,$70,$71,$26
WorldModTiles::_218: .byte   $05,$05,$64,$82,$ae,$88,$88,$af,$84,$60,$05
WorldModTiles::_219: .byte   $05,$73,$74,$85,$88,$88,$88,$88,$86,$70,$71
WorldModTiles::_220: .byte   $05,$64,$87,$85,$88,$88,$88,$88,$86,$87,$60,$26
WorldModTiles::_221: .byte   $02,$05,$53,$54,$90,$be,$88,$88,$bf,$92,$87,$60,$06
WorldModTiles::_222: .byte   $02,$05,$53,$54,$90,$91,$91,$92,$87,$50,$51,$16
WorldModTiles::_223: .byte   $02,$05,$64,$87,$87,$87,$50,$33,$51,$05,$26
WorldModTiles::_224: .byte   $02,$53,$33,$54,$50,$51,$05,$05,$05
WorldModTiles::_225: .byte   $05,$53,$51,$05,$05,$05
WorldModTiles::_226: .byte   $05,$05,$05,$05
WorldModTiles::_227: .byte   $05,$05
WorldModTiles::_228: .byte   $19,$19,$04,$04,$04,$04,$04,$04,$19
WorldModTiles::_229: .byte   $19,$19,$19,$04,$04,$04,$04,$04,$04,$11
WorldModTiles::_230: .byte   $19,$19,$19,$04,$04,$04,$04,$04,$19,$31
WorldModTiles::_231: .byte   $19,$19,$19,$04,$22,$22,$22,$22,$04,$19,$41,$21
WorldModTiles::_232: .byte   $13,$19,$04,$22,$22,$22,$22,$22,$22,$04,$51,$21
WorldModTiles::_233: .byte   $23,$19,$04,$22,$22,$22,$22,$22,$22,$04,$61,$21
WorldModTiles::_234: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_235: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_236: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$04,$11,$10
WorldModTiles::_237: .byte   $23,$04,$04,$04,$04,$04,$04,$11,$10
WorldModTiles::_238: .byte   $14,$13,$04,$04,$04,$11,$12,$10
WorldModTiles::_239: .byte   $14,$12,$12,$12,$10
WorldModTiles::_240: .byte   $05,$05
WorldModTiles::_241: .byte   $05,$05,$05,$05,$05,$05
WorldModTiles::_242: .byte   $05,$05,$05,$73,$43,$71,$05,$73,$75,$71,$05
WorldModTiles::_243: .byte   $05,$73,$43,$74,$87,$70,$43,$74,$87,$60,$05
WorldModTiles::_244: .byte   $05,$05,$05,$64,$87,$82,$83,$83,$84,$87,$87,$60,$00
WorldModTiles::_245: .byte   $05,$05,$73,$74,$82,$ae,$88,$88,$af,$84,$87,$60,$10
WorldModTiles::_246: .byte   $05,$53,$54,$85,$88,$88,$88,$88,$86,$50,$51,$10
WorldModTiles::_247: .byte   $05,$05,$64,$85,$88,$88,$88,$88,$86,$60,$00
WorldModTiles::_248: .byte   $05,$64,$90,$be,$88,$88,$bf,$92,$60
WorldModTiles::_249: .byte   $05,$53,$54,$90,$91,$91,$92,$87,$70
WorldModTiles::_250: .byte   $05,$53,$54,$87,$87,$87,$87,$87
WorldModTiles::_251: .byte   $05,$05,$53,$54
WorldModTiles::_252: .byte   $33,$42,$31,$03,$33,$42,$31
WorldModTiles::_253: .byte   $33,$42,$43,$52,$41,$42,$43,$52,$41,$21
WorldModTiles::_254: .byte   $23,$43,$52,$53,$62,$51,$52,$53,$62,$51,$21
WorldModTiles::_255: .byte   $23,$53,$62,$63,$04,$61,$62,$63,$04,$61,$21
WorldModTiles::_256: .byte   $23,$63,$04,$04,$04,$04,$04,$04,$04,$04,$21
WorldModTiles::_257: .byte   $23,$04,$04,$22,$22,$22,$22,$04,$04,$04,$21
WorldModTiles::_258: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_259: .byte   $33,$04,$22,$22,$22,$22,$22,$22,$04,$04,$31
WorldModTiles::_260: .byte   $43,$04,$22,$22,$22,$22,$22,$22,$04,$04,$41,$21
WorldModTiles::_261: .byte   $53,$04,$22,$22,$22,$22,$22,$22,$04,$04,$51,$21
WorldModTiles::_262: .byte   $63,$19,$04,$22,$22,$22,$22,$04,$04,$04,$61,$21
WorldModTiles::_263: .byte   $19,$11,$13,$04,$04,$04,$04,$04,$04,$04,$04,$21
WorldModTiles::_264: .byte   $19,$21,$23,$19,$04,$04,$04,$04,$04,$19,$19
WorldModTiles::_265: .byte   $19,$04,$04,$18,$04,$04,$19,$19
WorldModTiles::_266: .byte   $19
WorldModTiles::_267: .byte   $87,$87,$87,$70,$71
WorldModTiles::_268: .byte   $87,$87,$87,$87,$87,$87,$70,$43,$43,$71
WorldModTiles::_269: .byte   $33,$54,$87,$87,$87,$82,$83,$83,$84,$70,$71
WorldModTiles::_270: .byte   $05,$53,$54,$87,$82,$ae,$88,$88,$af,$84,$60
WorldModTiles::_271: .byte   $5b,$5d,$64,$87,$85,$88,$88,$88,$88,$86,$70,$71,$69
WorldModTiles::_272: .byte   $9a,$64,$87,$85,$88,$88,$88,$88,$86,$87,$60,$05
WorldModTiles::_273: .byte   $6d,$53,$54,$90,$be,$88,$88,$bf,$92,$87,$60,$59,$5d
WorldModTiles::_274: .byte   $64,$87,$90,$91,$91,$92,$50,$33,$51
WorldModTiles::_275: .byte   $08,$53,$54,$50,$33,$33,$33,$51
WorldModTiles::_276: .byte   $08,$53,$51,$06,$07,$07,$07
WorldModTiles::_277: .byte   $28,$05,$06
WorldModTiles::_278: .byte   $05,$16

WorldModTiles::_279: .byte   $31
WorldModTiles::_280: .byte   $41,$42,$42,$31
WorldModTiles::_281: .byte   $51,$52,$52,$41,$31
WorldModTiles::_282: .byte   $19,$61,$62,$62,$51,$41,$21
WorldModTiles::_283: .byte   $04,$04,$04,$04,$04,$04,$04,$61,$51,$31
WorldModTiles::_284: .byte   $13,$04,$04,$04,$22,$22,$22,$22,$04,$61,$41,$21
WorldModTiles::_285: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$51,$21
WorldModTiles::_286: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$61,$21
WorldModTiles::_287: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_288: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles::_289: .byte   $33,$04,$04,$22,$22,$22,$22,$04,$04,$11,$10
WorldModTiles::_290: .byte   $43,$19,$04,$04,$04,$04,$04,$11,$12,$10
WorldModTiles::_291: .byte   $53,$19,$11,$12,$13,$11,$12,$10
WorldModTiles::_292: .byte   $63,$19,$31,$03,$33,$31
WorldModTiles::_293: .byte   $19,$41,$42,$43,$41
WorldModTiles::_294: .byte   $53,$51
WorldModTiles::_295: .byte   $63,$61
WorldModTiles::_296: .byte   $08,$05
WorldModTiles::_297: .byte   $08,$05
WorldModTiles::_298: .byte   $28,$05

; ---------------------------------------------------------------------------
