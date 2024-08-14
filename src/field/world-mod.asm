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

WorldMod1:

world_mod {175, 142},  1, $01E0, WorldModTiles0
world_mod {175, 143},  1, $01E0, WorldModTiles1
world_mod {175, 144},  1, $01E0, WorldModTiles2

world_mod {164, 115},  2, $01E1, WorldModTiles3
world_mod {163, 116},  4, $01E1, WorldModTiles4
world_mod {163, 117},  3, $01E1, WorldModTiles5
world_mod {163, 118},  3, $01E1, WorldModTiles6
world_mod {163, 119},  2, $01E1, WorldModTiles7

world_mod {204,  84},  1, $01E2, WorldModTiles8
world_mod {204,  85},  1, $01E2, WorldModTiles9
world_mod {204,  86},  1, $01E2, WorldModTiles10
world_mod {204,  87},  1, $01E2, WorldModTiles11

world_mod {189,  81}, 16, $01E3, WorldModTiles12
world_mod {189,  82}, 16, $01E3, WorldModTiles13
world_mod {189,  83}, 16, $01E3, WorldModTiles14
world_mod {189,  84}, 15, $01E3, WorldModTiles15
world_mod {189,  85}, 15, $01E3, WorldModTiles16
world_mod {189,  86}, 15, $01E3, WorldModTiles17
world_mod {189,  87}, 15, $01E3, WorldModTiles18
world_mod {189,  88}, 16, $01E3, WorldModTiles19
world_mod {189,  89}, 16, $01E3, WorldModTiles20
world_mod {189,  90}, 16, $01E3, WorldModTiles21
world_mod {189,  91}, 16, $01E3, WorldModTiles22
world_mod {189,  92}, 16, $01E3, WorldModTiles23
world_mod {189,  93}, 16, $01E3, WorldModTiles24
world_mod {189,  94}, 16, $01E3, WorldModTiles25

world_mod { 82,  76},  3, $01E4, WorldModTiles26
world_mod { 82,  77},  3, $01E4, WorldModTiles27

world_mod {205, 202},  1, $01E5, WorldModTiles28

world_mod { 63, 156},  1, $01E6, WorldModTiles43
world_mod { 60, 157}, 11, $01E6, WorldModTiles29
world_mod { 59, 158}, 12, $01E6, WorldModTiles30
world_mod { 58, 159}, 13, $01E6, WorldModTiles31
world_mod { 57, 160}, 13, $01E6, WorldModTiles32
world_mod { 57, 161}, 13, $01E6, WorldModTiles33
world_mod { 57, 162}, 14, $01E6, WorldModTiles34
world_mod { 57, 163}, 16, $01E6, WorldModTiles35
world_mod { 57, 164}, 16, $01E6, WorldModTiles36
world_mod { 57, 165}, 16, $01E6, WorldModTiles37
world_mod { 57, 166}, 13, $01E6, WorldModTiles38
world_mod { 58, 167}, 12, $01E6, WorldModTiles39
world_mod { 58, 168}, 13, $01E6, WorldModTiles40
world_mod { 58, 169}, 13, $01E6, WorldModTiles41
world_mod { 62, 170},  9, $01E6, WorldModTiles42

world_mod { 60, 158},  4, $01E7, WorldModTiles44
world_mod { 60, 159},  4, $01E7, WorldModTiles45
world_mod { 60, 160},  4, $01E7, WorldModTiles46
world_mod { 60, 161},  4, $01E7, WorldModTiles47

world_mod {146, 115},  1, $01EF, WorldModTiles48

world_mod {147,  78},  1, $01DB, WorldModTiles111
world_mod {146,  79},  3, $01DB, WorldModTiles112
world_mod {147,  80},  1, $01DB, WorldModTiles113

world_mod {151,  78},  1, $01DC, WorldModTiles111
world_mod {150,  79},  3, $01DC, WorldModTiles112
world_mod {151,  80},  1, $01DC, WorldModTiles113

world_mod {151,  82},  1, $01DD, WorldModTiles111
world_mod {150,  83},  3, $01DD, WorldModTiles112
world_mod {151,  84},  1, $01DD, WorldModTiles113

world_mod {147,  82},  1, $01DE, WorldModTiles111
world_mod {146,  83},  3, $01DE, WorldModTiles112
world_mod {147,  84},  1, $01DE, WorldModTiles113

; ---------------------------------------------------------------------------

WorldMod2:

world_mod {112,  95},  2, $01E8, WorldModTiles49
world_mod {110,  96},  5, $01E8, WorldModTiles50
world_mod {107,  97}, 10, $01E8, WorldModTiles51
world_mod {107,  98}, 10, $01E8, WorldModTiles52
world_mod {107,  99}, 10, $01E8, WorldModTiles53
world_mod {107, 100}, 10, $01E8, WorldModTiles54
world_mod {107, 101},  9, $01E8, WorldModTiles55
world_mod {107, 102},  9, $01E8, WorldModTiles56
world_mod {107, 103},  9, $01E8, WorldModTiles57
world_mod {108, 104},  8, $01E8, WorldModTiles58
world_mod {109, 105},  6, $01E8, WorldModTiles59
world_mod {110, 106},  3, $01E8, WorldModTiles60
world_mod {111, 107},  2, $01E8, WorldModTiles61

world_mod {178, 160},  1, $01E9, WorldModTiles62
world_mod {176, 161},  5, $01E9, WorldModTiles63
world_mod {177, 162},  4, $01E9, WorldModTiles64
world_mod {177, 163},  5, $01E9, WorldModTiles65
world_mod {176, 164},  6, $01E9, WorldModTiles66
world_mod {177, 165},  3, $01E9, WorldModTiles67

world_mod {167, 160},  1, $01EA, WorldModTiles68
world_mod {167, 161},  1, $01EA, WorldModTiles69

world_mod {178, 160},  1, $01EB, WorldModTiles71
world_mod {176, 161},  5, $01EB, WorldModTiles72
world_mod {177, 162},  4, $01EB, WorldModTiles73
world_mod {177, 163},  5, $01EB, WorldModTiles74
world_mod {176, 164},  6, $01EB, WorldModTiles75
world_mod {177, 165},  3, $01EB, WorldModTiles76

world_mod { 47, 119},  8, $01EC, WorldModTiles77
world_mod { 45, 120}, 10, $01EC, WorldModTiles78
world_mod { 41, 121}, 14, $01EC, WorldModTiles79
world_mod { 40, 122}, 15, $01EC, WorldModTiles80
world_mod { 40, 123}, 15, $01EC, WorldModTiles81
world_mod { 40, 124},  9, $01EC, WorldModTiles82
world_mod { 40, 125},  8, $01EC, WorldModTiles83
world_mod { 34, 126}, 17, $01EC, WorldModTiles84
world_mod { 34, 127}, 18, $01EC, WorldModTiles85
world_mod { 34, 128}, 18, $01EC, WorldModTiles86
world_mod { 35, 129}, 17, $01EC, WorldModTiles87
world_mod { 38, 130}, 13, $01EC, WorldModTiles88
world_mod { 40, 131}, 10, $01EC, WorldModTiles89
world_mod { 40, 132},  7, $01EC, WorldModTiles90
world_mod { 41, 133},  4, $01EC, WorldModTiles91
world_mod { 46, 134},  2, $01EC, WorldModTiles92
world_mod { 45, 135},  3, $01EC, WorldModTiles93
world_mod { 45, 136},  3, $01EC, WorldModTiles94

world_mod {213,  36},  9, $01ED, WorldModTiles95
world_mod {213,  37},  9, $01ED, WorldModTiles96
world_mod {213,  38},  9, $01ED, WorldModTiles97
world_mod {213,  39},  9, $01ED, WorldModTiles98
world_mod {213,  40},  9, $01ED, WorldModTiles99
world_mod {213,  41},  9, $01ED, WorldModTiles100
world_mod {213,  42},  9, $01ED, WorldModTiles101
world_mod {213,  43},  9, $01ED, WorldModTiles102

; ---------------------------------------------------------------------------

WorldMod3:

world_mod {161, 150},  1, $01F0, WorldModTiles103

world_mod {205, 198},  3, $01F2, WorldModTiles104
world_mod {205, 199},  3, $01F2, WorldModTiles105
world_mod {205, 200},  3, $01F2, WorldModTiles106
world_mod {205, 201},  3, $01F2, WorldModTiles107

world_mod {145, 112},  9, $01F3, WorldModTiles114
world_mod {138, 113}, 16, $01F3, WorldModTiles115
world_mod {138, 114}, 16, $01F3, WorldModTiles116
world_mod {138, 115}, 16, $01F3, WorldModTiles116
world_mod {138, 116}, 16, $01F3, WorldModTiles116
world_mod {138, 117}, 16, $01F3, WorldModTiles116
world_mod {138, 118}, 16, $01F3, WorldModTiles116
world_mod {138, 119}, 16, $01F3, WorldModTiles116
world_mod {146, 120},  3, $01F3, WorldModTiles117
world_mod {132,  83},  3, $01F3, WorldModTiles124
world_mod {132,  84},  3, $01F3, WorldModTiles125
world_mod {132,  85},  3, $01F3, WorldModTiles125
world_mod {131,  86},  4, $01F3, WorldModTiles126
world_mod {130,  87},  3, $01F3, WorldModTiles127

world_mod {101,  97},  2, $01F7, WorldModTiles128
world_mod {101,  98},  3, $01F7, WorldModTiles129
world_mod {103,  99},  2, $01F7, WorldModTiles130

world_mod {182, 130},  7, $01F5, WorldModTiles131
world_mod {180, 131},  9, $01F5, WorldModTiles132
world_mod {180, 132},  7, $01F5, WorldModTiles133
world_mod {178, 133}, 11, $01F5, WorldModTiles134
world_mod {178, 134}, 11, $01F5, WorldModTiles135
world_mod {178, 135}, 12, $01F5, WorldModTiles136
world_mod {178, 136}, 12, $01F5, WorldModTiles137
world_mod {178, 137}, 12, $01F5, WorldModTiles138
world_mod {178, 138}, 13, $01F5, WorldModTiles139
world_mod {180, 139}, 10, $01F5, WorldModTiles140
world_mod {180, 140}, 11, $01F5, WorldModTiles141
world_mod {180, 141},  9, $01F5, WorldModTiles142
world_mod {183, 142},  3, $01F5, WorldModTiles143
world_mod {183, 143},  3, $01F5, WorldModTiles144
world_mod {183, 144},  2, $01F5, WorldModTiles145
world_mod {183, 145},  2, $01F5, WorldModTiles145
world_mod {183, 146},  2, $01F5, WorldModTiles145
world_mod {183, 147},  2, $01F5, WorldModTiles145

world_mod { 76, 114},  5, $01F6, WorldModTiles162
world_mod { 73, 115}, 15, $01F6, WorldModTiles163
world_mod { 73, 116}, 12, $01F6, WorldModTiles164
world_mod { 73, 117}, 12, $01F6, WorldModTiles165
world_mod { 73, 118}, 14, $01F6, WorldModTiles166
world_mod { 74, 119}, 13, $01F6, WorldModTiles167
world_mod { 74, 120}, 13, $01F6, WorldModTiles168
world_mod { 74, 121}, 11, $01F6, WorldModTiles169
world_mod { 74, 122}, 11, $01F6, WorldModTiles170
world_mod { 75, 123},  9, $01F6, WorldModTiles171
world_mod { 76, 124},  7, $01F6, WorldModTiles172
world_mod { 78, 125},  4, $01F6, WorldModTiles173

world_mod {218,  42},  4, $01F7, WorldModTiles187
world_mod {215,  43},  8, $01F7, WorldModTiles188
world_mod {214,  44}, 10, $01F7, WorldModTiles189
world_mod {213,  45}, 11, $01F7, WorldModTiles190
world_mod {213,  46}, 12, $01F7, WorldModTiles191
world_mod {214,  47}, 11, $01F7, WorldModTiles192
world_mod {214,  48}, 11, $01F7, WorldModTiles193
world_mod {214,  49}, 10, $01F7, WorldModTiles194
world_mod {214,  50}, 10, $01F7, WorldModTiles195
world_mod {215,  51},  8, $01F7, WorldModTiles196
world_mod {216,  52},  6, $01F7, WorldModTiles197
world_mod {218,  53},  2, $01F7, WorldModTiles198

world_mod {171,  40},  8, $01F7, WorldModTiles213
world_mod {171,  41},  8, $01F7, WorldModTiles214
world_mod {170,  42},  9, $01F7, WorldModTiles215
world_mod {169,  43}, 10, $01F7, WorldModTiles216
world_mod {169,  44}, 10, $01F7, WorldModTiles217
world_mod {168,  45}, 11, $01F7, WorldModTiles218
world_mod {168,  46}, 11, $01F7, WorldModTiles219
world_mod {168,  47}, 12, $01F7, WorldModTiles220
world_mod {167,  48}, 13, $01F7, WorldModTiles221
world_mod {168,  49}, 12, $01F7, WorldModTiles222
world_mod {169,  50}, 11, $01F7, WorldModTiles223
world_mod {170,  51},  9, $01F7, WorldModTiles224
world_mod {172,  52},  6, $01F7, WorldModTiles225
world_mod {174,  53},  4, $01F7, WorldModTiles226
world_mod {176,  54},  2, $01F7, WorldModTiles227

world_mod { 37,  35},  2, $01F7, WorldModTiles240
world_mod { 33,  36},  6, $01F7, WorldModTiles241
world_mod { 31,  37}, 11, $01F7, WorldModTiles242
world_mod { 31,  38}, 11, $01F7, WorldModTiles243
world_mod { 29,  39}, 13, $01F7, WorldModTiles244
world_mod { 29,  40}, 13, $01F7, WorldModTiles245
world_mod { 30,  41}, 12, $01F7, WorldModTiles246
world_mod { 30,  42}, 11, $01F7, WorldModTiles247
world_mod { 31,  43},  9, $01F7, WorldModTiles248
world_mod { 31,  44},  9, $01F7, WorldModTiles249
world_mod { 32,  45},  8, $01F7, WorldModTiles250
world_mod { 32,  46},  4, $01F7, WorldModTiles251

world_mod {208, 111},  5, $01F7, WorldModTiles267
world_mod {206, 112}, 10, $01F7, WorldModTiles268
world_mod {206, 113}, 11, $01F7, WorldModTiles269
world_mod {206, 114}, 11, $01F7, WorldModTiles270
world_mod {206, 115}, 13, $01F7, WorldModTiles271
world_mod {207, 116}, 12, $01F7, WorldModTiles272
world_mod {207, 117}, 13, $01F7, WorldModTiles273
world_mod {209, 118},  9, $01F7, WorldModTiles274
world_mod {208, 119},  8, $01F7, WorldModTiles275
world_mod {209, 120},  7, $01F7, WorldModTiles276
world_mod {209, 121},  3, $01F7, WorldModTiles277
world_mod {210, 122},  2, $01F7, WorldModTiles278

world_mod { 76, 124},  2, $01F7, WorldModTiles296
world_mod { 77, 125},  2, $01F7, WorldModTiles297
world_mod { 77, 126},  2, $01F7, WorldModTiles298

; ---------------------------------------------------------------------------

WorldMod4:

world_mod {169, 164},  1, $01EA, WorldModTiles70

; ---------------------------------------------------------------------------

WorldMod5:

world_mod {206, 199},  1, $01F2, WorldModTiles108
world_mod {205, 200},  3, $01F2, WorldModTiles109
world_mod {206, 201},  1, $01F2, WorldModTiles110

world_mod {145, 113},  8, $01F3, WorldModTiles118
world_mod {139, 114}, 14, $01F3, WorldModTiles119
world_mod {139, 115}, 13, $01F3, WorldModTiles120
world_mod {140, 116}, 10, $01F3, WorldModTiles121
world_mod {140, 117},  7, $01F3, WorldModTiles122
world_mod {141, 118},  7, $01F3, WorldModTiles122
world_mod {146, 119},  3, $01F3, WorldModTiles123

world_mod {182, 128},  4, $01F5, WorldModTiles146
world_mod {181, 129},  6, $01F5, WorldModTiles147
world_mod {180, 130},  7, $01F5, WorldModTiles148
world_mod {179, 131},  9, $01F5, WorldModTiles149
world_mod {179, 132}, 10, $01F5, WorldModTiles150
world_mod {178, 133}, 11, $01F5, WorldModTiles151
world_mod {178, 134}, 12, $01F5, WorldModTiles152
world_mod {178, 135}, 12, $01F5, WorldModTiles153
world_mod {178, 136}, 12, $01F5, WorldModTiles154
world_mod {178, 137}, 12, $01F5, WorldModTiles155
world_mod {178, 138}, 12, $01F5, WorldModTiles156
world_mod {179, 139}, 11, $01F5, WorldModTiles157
world_mod {180, 140},  8, $01F5, WorldModTiles158
world_mod {181, 141},  6, $01F5, WorldModTiles159
world_mod {184, 142},  3, $01F5, WorldModTiles160
world_mod {184, 143},  2, $01F5, WorldModTiles161
world_mod {184, 144},  2, $01F5, WorldModTiles161

world_mod { 81, 112},  3, $01F6, WorldModTiles174
world_mod { 76, 113},  9, $01F6, WorldModTiles175
world_mod { 75, 114}, 11, $01F6, WorldModTiles176
world_mod { 74, 115}, 12, $01F6, WorldModTiles177
world_mod { 74, 116}, 12, $01F6, WorldModTiles178
world_mod { 74, 117}, 12, $01F6, WorldModTiles179
world_mod { 74, 118}, 12, $01F6, WorldModTiles180
world_mod { 74, 119}, 12, $01F6, WorldModTiles181
world_mod { 75, 120}, 11, $01F6, WorldModTiles182
world_mod { 75, 121}, 11, $01F6, WorldModTiles183
world_mod { 75, 122},  9, $01F6, WorldModTiles184
world_mod { 77, 123},  6, $01F6, WorldModTiles185
world_mod { 77, 124},  6, $01F6, WorldModTiles186

world_mod {212, 108},  1, $01F7, WorldModTiles279
world_mod {212, 109},  4, $01F7, WorldModTiles280
world_mod {212, 110},  5, $01F7, WorldModTiles281
world_mod {211, 111},  7, $01F7, WorldModTiles282
world_mod {208, 112}, 10, $01F7, WorldModTiles283
world_mod {207, 113}, 12, $01F7, WorldModTiles284
world_mod {207, 114}, 12, $01F7, WorldModTiles285
world_mod {207, 115}, 12, $01F7, WorldModTiles286
world_mod {207, 116}, 12, $01F7, WorldModTiles287
world_mod {207, 117}, 12, $01F7, WorldModTiles288
world_mod {208, 118}, 11, $01F7, WorldModTiles289
world_mod {208, 119}, 10, $01F7, WorldModTiles290
world_mod {208, 120},  8, $01F7, WorldModTiles291
world_mod {208, 121},  6, $01F7, WorldModTiles292
world_mod {209, 122},  5, $01F7, WorldModTiles293
world_mod {212, 123},  2, $01F7, WorldModTiles294
world_mod {212, 124},  2, $01F7, WorldModTiles295

world_mod { 34,  34},  7, $01F7, WorldModTiles252
world_mod { 32,  35}, 10, $01F7, WorldModTiles253
world_mod { 31,  36}, 11, $01F7, WorldModTiles254
world_mod { 31,  37}, 11, $01F7, WorldModTiles255
world_mod { 31,  38}, 11, $01F7, WorldModTiles256
world_mod { 31,  39}, 11, $01F7, WorldModTiles257
world_mod { 31,  40}, 11, $01F7, WorldModTiles258
world_mod { 31,  41}, 11, $01F7, WorldModTiles259
world_mod { 31,  42}, 12, $01F7, WorldModTiles260
world_mod { 31,  43}, 12, $01F7, WorldModTiles261
world_mod { 31,  44}, 12, $01F7, WorldModTiles262
world_mod { 31,  45}, 12, $01F7, WorldModTiles263
world_mod { 31,  46}, 11, $01F7, WorldModTiles264
world_mod { 34,  47},  8, $01F7, WorldModTiles265
world_mod { 41,  48},  1, $01F7, WorldModTiles266

world_mod {170,  41},  9, $01F7, WorldModTiles228
world_mod {169,  42}, 10, $01F7, WorldModTiles229
world_mod {169,  43}, 10, $01F7, WorldModTiles230
world_mod {168,  44}, 12, $01F7, WorldModTiles231
world_mod {168,  45}, 12, $01F7, WorldModTiles232
world_mod {168,  46}, 12, $01F7, WorldModTiles233
world_mod {168,  47}, 12, $01F7, WorldModTiles234
world_mod {168,  48}, 12, $01F7, WorldModTiles235
world_mod {169,  49}, 11, $01F7, WorldModTiles236
world_mod {170,  50},  9, $01F7, WorldModTiles237
world_mod {170,  51},  8, $01F7, WorldModTiles238
world_mod {171,  52},  5, $01F7, WorldModTiles239

world_mod {218,  40},  4, $01F7, WorldModTiles199
world_mod {216,  41},  7, $01F7, WorldModTiles200
world_mod {214,  42},  9, $01F7, WorldModTiles201
world_mod {214,  43}, 10, $01F7, WorldModTiles202
world_mod {214,  44}, 11, $01F7, WorldModTiles203
world_mod {214,  45}, 11, $01F7, WorldModTiles204
world_mod {214,  46}, 11, $01F7, WorldModTiles205
world_mod {214,  47}, 11, $01F7, WorldModTiles206
world_mod {214,  48}, 11, $01F7, WorldModTiles207
world_mod {214,  49}, 10, $01F7, WorldModTiles208
world_mod {214,  50}, 10, $01F7, WorldModTiles209
world_mod {215,  51},  8, $01F7, WorldModTiles210
world_mod {216,  52},  6, $01F7, WorldModTiles211
world_mod {218,  53},  2, $01F7, WorldModTiles212

WorldModEnd := *

; ---------------------------------------------------------------------------

WorldModTiles0: .byte   $9d
WorldModTiles1: .byte   $ad
WorldModTiles2: .byte   $bd

WorldModTiles3: .byte   $27,$9f
WorldModTiles4: .byte   $18,$05,$05,$16
WorldModTiles5: .byte   $28,$05,$06
WorldModTiles6: .byte   $05,$05,$16
WorldModTiles7: .byte   $07,$07

WorldModTiles8: .byte   $9d
WorldModTiles9: .byte   $ad
WorldModTiles10: .byte   $ad
WorldModTiles11: .byte   $bd

WorldModTiles12: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles13: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$33,$54,$50,$33
WorldModTiles14: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$05,$53,$51,$05
WorldModTiles15: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$06,$07,$07,$08
WorldModTiles16: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$50,$51,$06,$17,$17,$17,$18
WorldModTiles17: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$70,$71,$26,$27,$27,$27,$28
WorldModTiles18: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$70,$43,$43,$71,$05,$05
WorldModTiles19: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$70,$43,$43,$71
WorldModTiles20: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$60
WorldModTiles21: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$70
WorldModTiles22: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles23: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles24: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles25: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87

WorldModTiles26: .byte   $05,$a1,$a0
WorldModTiles27: .byte   $05,$a2,$05

WorldModTiles28: .byte   $81
WorldModTiles29: .byte   $28,$05,$05,$05,$26,$27,$27,$27,$17,$17,$17
WorldModTiles30: .byte   $28,$05,$05,$a0,$79,$7d,$05,$a2,$05,$26,$17,$17
WorldModTiles31: .byte   $18,$05,$05,$79,$7b,$7a,$9c,$a1,$79,$7d,$05,$16,$17
WorldModTiles32: .byte   $17,$18,$05,$a2,$89,$8a,$95,$7c,$7b,$7a,$9c,$05,$16
WorldModTiles33: .byte   $17,$28,$05,$79,$7b,$7a,$95,$95,$95,$95,$7c,$7d,$26
WorldModTiles34: .byte   $18,$05,$05,$89,$8a,$95,$95,$95,$95,$95,$8c,$8d,$05,$16
WorldModTiles35: .byte   $18,$05,$05,$a0,$9b,$95,$95,$95,$95,$95,$7c,$7d,$05,$16,$17,$17
WorldModTiles36: .byte   $17,$08,$05,$79,$7a,$95,$95,$95,$95,$8c,$8b,$8d,$05,$16,$17,$17
WorldModTiles37: .byte   $17,$18,$05,$89,$8b,$8a,$8c,$8b,$8a,$9c,$a2,$05,$06,$17,$17,$17
WorldModTiles38: .byte   $17,$17,$08,$05,$a1,$89,$8d,$a0,$89,$8d,$05,$05,$16
WorldModTiles39: .byte   $17,$17,$07,$07,$08,$05,$05,$05,$06,$07,$07,$17
WorldModTiles40: .byte   $17,$17,$17,$17,$17,$07,$07,$07,$17,$17,$17,$17,$17
WorldModTiles41: .byte   $17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17,$17
WorldModTiles42: .byte   $17,$17,$17,$17,$17,$17,$17,$17,$17

WorldModTiles43: .byte   $26

WorldModTiles44: .byte   $79,$7b,$7b,$7b
WorldModTiles45: .byte   $9b,$8c,$8b,$8a
WorldModTiles46: .byte   $9b,$9c,$9d,$9b
WorldModTiles47: .byte   $9b,$7c,$7b,$7a

WorldModTiles48: .byte   $97

WorldModTiles49: .byte   $87,$87
WorldModTiles50: .byte   $87,$87,$87,$87,$87
WorldModTiles51: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles52: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles53: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles54: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles55: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles56: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles57: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles58: .byte   $87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles59: .byte   $87,$87,$87,$87,$87,$87
WorldModTiles60: .byte   $87,$87,$87
WorldModTiles61: .byte   $87,$87

WorldModTiles62: .byte   $84
WorldModTiles63: .byte   $84,$87,$87,$87,$84
WorldModTiles64: .byte   $84,$84,$87,$84
WorldModTiles65: .byte   $85,$86,$84,$87,$84
WorldModTiles66: .byte   $84,$87,$84,$87,$84,$84
WorldModTiles67: .byte   $84,$87,$84

WorldModTiles68: .byte   $05
WorldModTiles69: .byte   $05

WorldModTiles70: .byte   $42

WorldModTiles71: .byte   $87
WorldModTiles72: .byte   $87,$87,$87,$87,$87
WorldModTiles73: .byte   $87,$87,$87,$87
WorldModTiles74: .byte   $87,$87,$87,$87,$87
WorldModTiles75: .byte   $87,$87,$87,$87,$87,$87
WorldModTiles76: .byte   $87,$87,$87

WorldModTiles77: .byte   $21,$21,$21,$21,$21,$21,$21,$21
WorldModTiles78: .byte   $21,$22,$79,$7b,$7b,$7b,$7b,$7b,$7b,$7d
WorldModTiles79: .byte   $21,$21,$21,$22,$79,$7b,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$9c
WorldModTiles80: .byte   $12,$79,$7b,$7b,$7b,$7a,$a5,$a5,$8c,$8b,$8b,$8b,$8b,$8b,$8d
WorldModTiles81: .byte   $12,$9b,$a5,$a5,$6e,$a5,$a5,$a5,$8d,$00,$01,$01,$01,$01,$01
WorldModTiles82: .byte   $12,$9b,$6e,$a5,$8c,$8b,$8b,$8d,$00
WorldModTiles83: .byte   $22,$9b,$a5,$8c,$8d,$00,$01,$01
WorldModTiles84: .byte   $20,$21,$21,$21,$21,$22,$79,$7a,$a5,$7c,$7d,$20,$21,$21,$21,$21,$21
WorldModTiles85: .byte   $79,$7b,$7b,$7b,$7b,$7b,$7a,$a5,$a5,$a5,$7c,$7b,$7b,$7b,$7b,$7b,$7d,$20
WorldModTiles86: .byte   $89,$8a,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$a5,$6e,$7c,$7d
WorldModTiles87: .byte   $89,$8b,$8b,$8a,$a5,$a5,$6e,$a5,$a5,$a5,$6e,$a5,$6e,$a5,$a5,$8c,$8d
WorldModTiles88: .byte   $89,$8b,$8a,$a5,$a5,$a5,$6e,$a5,$a5,$a5,$a5,$8c,$8d
WorldModTiles89: .byte   $9b,$a5,$6e,$a5,$a5,$a5,$8c,$8b,$8b,$8d
WorldModTiles90: .byte   $89,$8a,$a5,$a5,$8c,$8b,$8d
WorldModTiles91: .byte   $89,$8b,$8b,$8d
WorldModTiles92: .byte   $05,$09
WorldModTiles93: .byte   $0b,$05,$19
WorldModTiles94: .byte   $1b,$05,$29

WorldModTiles95: .byte   $11,$05,$05,$11,$11,$11,$05,$05,$11
WorldModTiles96: .byte   $05,$11,$11,$05,$05,$05,$11,$11,$05
WorldModTiles97: .byte   $05,$11,$05,$11,$11,$11,$05,$11,$05
WorldModTiles98: .byte   $05,$11,$11,$11,$11,$11,$11,$11,$05
WorldModTiles99: .byte   $05,$11,$11,$05,$11,$05,$11,$11,$05
WorldModTiles100: .byte   $05,$11,$11,$05,$11,$05,$11,$11,$05
WorldModTiles101: .byte   $05,$11,$11,$11,$05,$11,$11,$11,$05
WorldModTiles102: .byte   $11,$05,$11,$11,$11,$11,$11,$05,$11

WorldModTiles103: .byte   $2a

WorldModTiles104: .byte   $05,$05,$05,$05
WorldModTiles105: .byte   $73,$43,$71
WorldModTiles106: .byte   $64,$81,$60
WorldModTiles107: .byte   $53,$33,$51

WorldModTiles108: .byte   $04
WorldModTiles109: .byte   $04,$19,$04
WorldModTiles110: .byte   $04

WorldModTiles111: .byte   $11
WorldModTiles112: .byte   $11,$11,$11
WorldModTiles113: .byte   $11

WorldModTiles114: .byte   $73,$74,$87,$87,$87,$87,$87,$87,$87
WorldModTiles115: .byte   $87,$87,$87,$87,$87,$87,$70,$74,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles116: .byte   $87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87,$87
WorldModTiles117: .byte   $87,$87,$87

WorldModTiles118: .byte   $04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles119: .byte   $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles120: .byte   $04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles121: .byte   $04,$04,$04,$04,$04,$04,$04,$04,$04,$04
WorldModTiles122: .byte   $04,$04,$04,$04,$04,$04,$04
WorldModTiles123: .byte   $04,$04,$04
WorldModTiles124: .byte   $18,$05,$06
WorldModTiles125: .byte   $18,$05,$16
WorldModTiles126: .byte   $27,$28,$05,$16
WorldModTiles127: .byte   $28,$05,$05

WorldModTiles128: .byte   $26,$27
WorldModTiles129: .byte   $05,$05,$26
WorldModTiles130: .byte   $05,$16

WorldModTiles131: .byte   $21,$21,$21,$22,$05,$05,$05
WorldModTiles132: .byte   $20,$22,$73,$43,$43,$71,$00,$01,$01
WorldModTiles133: .byte   $05,$73,$74,$87,$87,$60,$20
WorldModTiles134: .byte   $05,$05,$73,$74,$87,$87,$87,$70,$71,$20,$21
WorldModTiles135: .byte   $05,$05,$64,$87,$82,$83,$83,$84,$70,$71,$05
WorldModTiles136: .byte   $05,$73,$74,$82,$ae,$88,$88,$af,$84,$60,$05,$16
WorldModTiles137: .byte   $05,$64,$87,$85,$88,$88,$88,$88,$86,$70,$71,$16
WorldModTiles138: .byte   $05,$64,$87,$85,$88,$88,$88,$88,$86,$87,$60,$26
WorldModTiles139: .byte   $05,$53,$54,$90,$be,$88,$88,$bf,$92,$87,$70,$71,$26
WorldModTiles140: .byte   $53,$54,$90,$91,$91,$92,$87,$87,$87,$70
WorldModTiles141: .byte   $05,$53,$54,$50,$33,$33,$54,$87,$87,$87,$87
WorldModTiles142: .byte   $05,$05,$53,$51,$05,$05,$53,$54,$87
WorldModTiles143: .byte   $05,$05,$05
WorldModTiles144: .byte   $08,$05,$05
WorldModTiles145: .byte   $18,$05

WorldModTiles146: .byte   $33,$42,$42,$31
WorldModTiles147: .byte   $33,$43,$52,$52,$41,$21
WorldModTiles148: .byte   $33,$43,$53,$62,$62,$51,$31
WorldModTiles149: .byte   $23,$43,$53,$63,$04,$04,$61,$41,$31
WorldModTiles150: .byte   $33,$53,$63,$04,$04,$04,$04,$51,$41,$21
WorldModTiles151: .byte   $23,$43,$63,$04,$04,$04,$04,$04,$61,$51,$31
WorldModTiles152: .byte   $23,$53,$04,$04,$22,$22,$22,$22,$04,$61,$41,$21
WorldModTiles153: .byte   $23,$63,$04,$22,$22,$22,$22,$22,$22,$04,$51,$31
WorldModTiles154: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$61,$41
WorldModTiles155: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$51
WorldModTiles156: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$61
WorldModTiles157: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$04,$04,$04
WorldModTiles158: .byte   $14,$13,$04,$04,$04,$04,$04,$04
WorldModTiles159: .byte   $14,$12,$12,$13,$19,$19
WorldModTiles160: .byte   $23,$19,$19
WorldModTiles161: .byte   $23,$19

WorldModTiles162: .byte   $05,$05,$05,$05,$05
WorldModTiles163: .byte   $05,$05,$05,$05,$05,$73,$71,$05,$73,$43,$71,$69,$6b,$6d,$69
WorldModTiles164: .byte   $05,$05,$05,$73,$43,$74,$70,$43,$74,$87,$70,$71
WorldModTiles165: .byte   $05,$05,$73,$74,$87,$82,$83,$83,$84,$87,$87,$60
WorldModTiles166: .byte   $05,$05,$64,$87,$82,$ae,$88,$88,$af,$84,$87,$60,$05,$69
WorldModTiles167: .byte   $05,$53,$54,$85,$88,$88,$88,$88,$86,$87,$70,$71,$05
WorldModTiles168: .byte   $05,$05,$64,$85,$88,$88,$88,$88,$86,$87,$50,$51,$05
WorldModTiles169: .byte   $08,$05,$64,$90,$be,$88,$88,$bf,$92,$50,$51
WorldModTiles170: .byte   $18,$05,$53,$54,$90,$91,$91,$92,$50,$51,$06
WorldModTiles171: .byte   $08,$05,$53,$54,$50,$33,$33,$51,$06
WorldModTiles172: .byte   $07,$08,$53,$51,$05,$06,$07
WorldModTiles173: .byte   $08,$05,$05,$26

WorldModTiles174: .byte   $33,$42,$31
WorldModTiles175: .byte   $33,$42,$42,$42,$42,$43,$52,$41,$31
WorldModTiles176: .byte   $33,$43,$52,$52,$52,$52,$53,$62,$51,$41,$21
WorldModTiles177: .byte   $23,$43,$53,$62,$62,$62,$62,$63,$04,$61,$51,$21
WorldModTiles178: .byte   $23,$53,$63,$04,$04,$04,$04,$04,$04,$04,$61,$21
WorldModTiles179: .byte   $23,$63,$04,$04,$22,$22,$22,$22,$04,$04,$04,$21
WorldModTiles180: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles181: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles182: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles183: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$11,$10
WorldModTiles184: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$11
WorldModTiles185: .byte   $13,$04,$04,$04,$04,$11
WorldModTiles186: .byte   $14,$12,$12,$12,$12,$10

WorldModTiles187: .byte   $27,$27,$27,$27
WorldModTiles188: .byte   $27,$27,$28,$73,$43,$43,$71,$26
WorldModTiles189: .byte   $28,$05,$73,$43,$74,$87,$87,$60,$05,$16
WorldModTiles190: .byte   $18,$73,$43,$74,$82,$83,$83,$84,$70,$71,$26
WorldModTiles191: .byte   $18,$53,$54,$82,$ae,$88,$88,$af,$84,$70,$71,$16
WorldModTiles192: .byte   $08,$64,$85,$88,$88,$88,$88,$86,$87,$60,$16
WorldModTiles193: .byte   $18,$64,$85,$88,$88,$88,$88,$86,$50,$51,$16
WorldModTiles194: .byte   $18,$64,$90,$be,$88,$88,$bf,$92,$60,$06
WorldModTiles195: .byte   $18,$53,$54,$90,$91,$91,$92,$50,$51,$16
WorldModTiles196: .byte   $08,$53,$33,$54,$50,$33,$51,$06
WorldModTiles197: .byte   $07,$08,$53,$51,$06,$07
WorldModTiles198: .byte   $07,$07
WorldModTiles199: .byte   $33,$42,$42,$31
WorldModTiles200: .byte   $33,$42,$43,$52,$52,$41,$21
WorldModTiles201: .byte   $23,$42,$43,$52,$53,$62,$62,$51,$31
WorldModTiles202: .byte   $23,$52,$53,$62,$63,$04,$04,$61,$41,$31
WorldModTiles203: .byte   $23,$62,$63,$04,$04,$04,$04,$04,$51,$41,$21
WorldModTiles204: .byte   $23,$04,$04,$22,$22,$22,$22,$04,$61,$51,$21
WorldModTiles205: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$61,$21
WorldModTiles206: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles207: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$11,$10
WorldModTiles208: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$21
WorldModTiles209: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$11,$10
WorldModTiles210: .byte   $14,$13,$04,$04,$04,$04,$11,$10
WorldModTiles211: .byte   $14,$12,$13,$11,$12,$10
WorldModTiles212: .byte   $14,$10
WorldModTiles213: .byte   $54,$87,$87,$87,$87,$87,$87,$87
WorldModTiles214: .byte   $53,$54,$87,$87,$87,$87,$50,$33
WorldModTiles215: .byte   $05,$05,$64,$87,$87,$87,$87,$60,$06
WorldModTiles216: .byte   $05,$05,$73,$74,$87,$87,$87,$50,$51,$16
WorldModTiles217: .byte   $05,$73,$74,$82,$83,$83,$84,$70,$71,$26
WorldModTiles218: .byte   $05,$05,$64,$82,$ae,$88,$88,$af,$84,$60,$05
WorldModTiles219: .byte   $05,$73,$74,$85,$88,$88,$88,$88,$86,$70,$71
WorldModTiles220: .byte   $05,$64,$87,$85,$88,$88,$88,$88,$86,$87,$60,$26
WorldModTiles221: .byte   $02,$05,$53,$54,$90,$be,$88,$88,$bf,$92,$87,$60,$06
WorldModTiles222: .byte   $02,$05,$53,$54,$90,$91,$91,$92,$87,$50,$51,$16
WorldModTiles223: .byte   $02,$05,$64,$87,$87,$87,$50,$33,$51,$05,$26
WorldModTiles224: .byte   $02,$53,$33,$54,$50,$51,$05,$05,$05
WorldModTiles225: .byte   $05,$53,$51,$05,$05,$05
WorldModTiles226: .byte   $05,$05,$05,$05
WorldModTiles227: .byte   $05,$05
WorldModTiles228: .byte   $19,$19,$04,$04,$04,$04,$04,$04,$19
WorldModTiles229: .byte   $19,$19,$19,$04,$04,$04,$04,$04,$04,$11
WorldModTiles230: .byte   $19,$19,$19,$04,$04,$04,$04,$04,$19,$31
WorldModTiles231: .byte   $19,$19,$19,$04,$22,$22,$22,$22,$04,$19,$41,$21
WorldModTiles232: .byte   $13,$19,$04,$22,$22,$22,$22,$22,$22,$04,$51,$21
WorldModTiles233: .byte   $23,$19,$04,$22,$22,$22,$22,$22,$22,$04,$61,$21
WorldModTiles234: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles235: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles236: .byte   $14,$13,$04,$22,$22,$22,$22,$04,$04,$11,$10
WorldModTiles237: .byte   $23,$04,$04,$04,$04,$04,$04,$11,$10
WorldModTiles238: .byte   $14,$13,$04,$04,$04,$11,$12,$10
WorldModTiles239: .byte   $14,$12,$12,$12,$10
WorldModTiles240: .byte   $05,$05
WorldModTiles241: .byte   $05,$05,$05,$05,$05,$05
WorldModTiles242: .byte   $05,$05,$05,$73,$43,$71,$05,$73,$75,$71,$05
WorldModTiles243: .byte   $05,$73,$43,$74,$87,$70,$43,$74,$87,$60,$05
WorldModTiles244: .byte   $05,$05,$05,$64,$87,$82,$83,$83,$84,$87,$87,$60,$00
WorldModTiles245: .byte   $05,$05,$73,$74,$82,$ae,$88,$88,$af,$84,$87,$60,$10
WorldModTiles246: .byte   $05,$53,$54,$85,$88,$88,$88,$88,$86,$50,$51,$10
WorldModTiles247: .byte   $05,$05,$64,$85,$88,$88,$88,$88,$86,$60,$00
WorldModTiles248: .byte   $05,$64,$90,$be,$88,$88,$bf,$92,$60
WorldModTiles249: .byte   $05,$53,$54,$90,$91,$91,$92,$87,$70
WorldModTiles250: .byte   $05,$53,$54,$87,$87,$87,$87,$87
WorldModTiles251: .byte   $05,$05,$53,$54
WorldModTiles252: .byte   $33,$42,$31,$03,$33,$42,$31
WorldModTiles253: .byte   $33,$42,$43,$52,$41,$42,$43,$52,$41,$21
WorldModTiles254: .byte   $23,$43,$52,$53,$62,$51,$52,$53,$62,$51,$21
WorldModTiles255: .byte   $23,$53,$62,$63,$04,$61,$62,$63,$04,$61,$21
WorldModTiles256: .byte   $23,$63,$04,$04,$04,$04,$04,$04,$04,$04,$21
WorldModTiles257: .byte   $23,$04,$04,$22,$22,$22,$22,$04,$04,$04,$21
WorldModTiles258: .byte   $23,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles259: .byte   $33,$04,$22,$22,$22,$22,$22,$22,$04,$04,$31
WorldModTiles260: .byte   $43,$04,$22,$22,$22,$22,$22,$22,$04,$04,$41,$21
WorldModTiles261: .byte   $53,$04,$22,$22,$22,$22,$22,$22,$04,$04,$51,$21
WorldModTiles262: .byte   $63,$19,$04,$22,$22,$22,$22,$04,$04,$04,$61,$21
WorldModTiles263: .byte   $19,$11,$13,$04,$04,$04,$04,$04,$04,$04,$04,$21
WorldModTiles264: .byte   $19,$21,$23,$19,$04,$04,$04,$04,$04,$19,$19
WorldModTiles265: .byte   $19,$04,$04,$18,$04,$04,$19,$19
WorldModTiles266: .byte   $19
WorldModTiles267: .byte   $87,$87,$87,$70,$71
WorldModTiles268: .byte   $87,$87,$87,$87,$87,$87,$70,$43,$43,$71
WorldModTiles269: .byte   $33,$54,$87,$87,$87,$82,$83,$83,$84,$70,$71
WorldModTiles270: .byte   $05,$53,$54,$87,$82,$ae,$88,$88,$af,$84,$60
WorldModTiles271: .byte   $5b,$5d,$64,$87,$85,$88,$88,$88,$88,$86,$70,$71,$69
WorldModTiles272: .byte   $9a,$64,$87,$85,$88,$88,$88,$88,$86,$87,$60,$05
WorldModTiles273: .byte   $6d,$53,$54,$90,$be,$88,$88,$bf,$92,$87,$60,$59,$5d
WorldModTiles274: .byte   $64,$87,$90,$91,$91,$92,$50,$33,$51
WorldModTiles275: .byte   $08,$53,$54,$50,$33,$33,$33,$51
WorldModTiles276: .byte   $08,$53,$51,$06,$07,$07,$07
WorldModTiles277: .byte   $28,$05,$06
WorldModTiles278: .byte   $05,$16

WorldModTiles279: .byte   $31
WorldModTiles280: .byte   $41,$42,$42,$31
WorldModTiles281: .byte   $51,$52,$52,$41,$31
WorldModTiles282: .byte   $19,$61,$62,$62,$51,$41,$21
WorldModTiles283: .byte   $04,$04,$04,$04,$04,$04,$04,$61,$51,$31
WorldModTiles284: .byte   $13,$04,$04,$04,$22,$22,$22,$22,$04,$61,$41,$21
WorldModTiles285: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$51,$21
WorldModTiles286: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$61,$21
WorldModTiles287: .byte   $23,$04,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles288: .byte   $14,$13,$04,$22,$22,$22,$22,$22,$22,$04,$04,$21
WorldModTiles289: .byte   $33,$04,$04,$22,$22,$22,$22,$04,$04,$11,$10
WorldModTiles290: .byte   $43,$19,$04,$04,$04,$04,$04,$11,$12,$10
WorldModTiles291: .byte   $53,$19,$11,$12,$13,$11,$12,$10
WorldModTiles292: .byte   $63,$19,$31,$03,$33,$31
WorldModTiles293: .byte   $19,$41,$42,$43,$41
WorldModTiles294: .byte   $53,$51
WorldModTiles295: .byte   $63,$61
WorldModTiles296: .byte   $08,$05
WorldModTiles297: .byte   $08,$05
WorldModTiles298: .byte   $28,$05

; ---------------------------------------------------------------------------
