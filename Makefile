
# the assembler
ASM = ca65
ASMFLAGS = -g -I include
VERSION_EXT =

# the linker
LINK = ld65
LINKFLAGS =

# list of ROM versions
VERSIONS = ff5-jp ff5-en
OBJ_DIR = obj
ROM_DIR = rom
ROMS = $(foreach V, $(VERSIONS), $(ROM_DIR)/$(V).sfc)

# the SPC program
SPC_PRG = src/sound/ff5-spc.dat

.PHONY: all rip clean spc distclean rng lz cmp text world_tilemap \
	battle_bg_tiles battle_bg_flip $(VERSIONS)

# disable default suffix rules
.SUFFIXES:

# make all versions
all: $(VERSIONS)

PYTHON := python3
export PYTHONPATH := tools/romtools:$(PYTHONPATH)

# install submodules
setup:
	git submodule update --init --recursive --remote --force

# rip data from ROMs
rip: setup
	$(PYTHON) tools/extract_assets.py

# shuffle the RNG table
rng:
	$(PYTHON) tools/shuffle_rng.py src/field/rng_tbl.dat

spc: $(SPC_PRG)

# remove all intermediate files
clean:
	$(RM) -r $(ROM_DIR) $(OBJ_DIR)
	find ./src -name "*.cmp" -type f -delete
	find ./src -name "*.lz" -type f -delete
	find ./src -name "*.bgt" -type f -delete
	find ./src -name "*.bgf" -type f -delete
	find ./src/text -name "*.dat" -type f -delete
	$(RM) ./src/field/world_tilemap.dat
	$(RM) $(SPC_PRG)

# remove all ripped assets
distclean: clean
	$(PYTHON) tools/clean_assets.py tools/rip_list_jp.json
	$(PYTHON) tools/clean_assets.py tools/rip_list_en.json
	$(RM) -rf ./src/field/world_tilemap

# ROM filenames
FF5_JP_PATH = $(ROM_DIR)/ff5-jp.sfc
FF5_EN_PATH = $(ROM_DIR)/ff5-en.sfc

ff5-jp: $(FF5_JP_PATH)
ff5-en: $(FF5_EN_PATH)

# set up target-specific variables
ff5-jp: VERSION_EXT = jp
ff5-en: VERSION_EXT = en

ff5-jp: ASMFLAGS += -D ROM_VERSION=0
ff5-en: ASMFLAGS += -D LANG_EN=1 -D ROM_VERSION=0

%.lz: %
	$(PYTHON) tools/ff5_compress.py $< lzss

%.cmp: %
	$(PYTHON) tools/ff5_compress.py $< multi

# list of all include files
INC_FILES = $(wildcard include/*.inc) $(wildcard include/*/*.inc)

# target-specific object filenames
OBJ_FILES_JP = $(foreach M,$(MODULES),$(OBJ_DIR)/$(M)-jp.o)
OBJ_FILES_EN = $(foreach M,$(MODULES),$(OBJ_DIR)/$(M)-en.o)

LZ_LIST = $(wildcard src/field/overlay_prop/overlay_prop_*.dat) \
$(wildcard src/field/sub_tilemap/*.dat) \
$(wildcard src/field/map_tileset/*.dat) \
$(wildcard src/field/map_tile_prop/*.dat) \
$(wildcard src/gfx/battle_bg_gfx/*.4bpp) \
src/gfx/the_end.4bpp \
src/cutscene/unknown_c34469.dat \
src/cutscene/unknown_c34e34.dat \
src/cutscene/unknown_c350d4.dat \
src/cutscene/unknown_c351ef.dat


CMP_LIST = src/cutscene/unknown_c30200.dat \
src/cutscene/unknown_c30200.dat \
src/cutscene/unknown_c302b3.dat \
src/cutscene/unknown_c30368.dat \
src/cutscene/unknown_c305f9.dat \
src/cutscene/unknown_c305fd.dat \
src/cutscene/unknown_c30663.dat \
src/cutscene/unknown_c3067c.dat \
src/cutscene/unknown_c306f5.dat \
src/cutscene/unknown_c309d2.dat \
src/cutscene/unknown_c30d89.dat \
src/cutscene/unknown_c30da3.dat \
src/cutscene/unknown_c30e1b.dat \
src/cutscene/unknown_c30e70.dat \
src/cutscene/unknown_c30f4a.dat \
src/cutscene/unknown_c30f70.dat \
src/cutscene/unknown_c31177.dat \
src/cutscene/unknown_c31243.dat \
src/cutscene/unknown_c31c65.dat \
src/cutscene/unknown_c31c69.dat \
src/cutscene/unknown_c31d25.dat \
src/cutscene/unknown_c31d48.dat \
src/cutscene/unknown_c31dfe.dat \
src/cutscene/unknown_c31e83.dat \
src/cutscene/unknown_c32d22.dat \
src/cutscene/unknown_c3331f.dat \
src/cutscene/unknown_c33342.dat \
src/cutscene/unknown_c333f6.dat \
src/cutscene/unknown_c3362b.dat \
src/cutscene/unknown_c3383c.dat \
src/cutscene/unknown_c33909.dat \
src/cutscene/unknown_c34452.dat \
src/cutscene/unknown_c34df8.dat \
src/cutscene/unknown_c35b1e.dat \
src/cutscene/unknown_c366d8.dat \
src/cutscene/unknown_c372e7.dat \
src/cutscene/unknown_c37445.dat \
src/cutscene/unknown_c3750d.dat \
src/cutscene/unknown_c3759a.dat \
src/cutscene/unknown_c3765c.dat \
src/cutscene/unknown_c377ae.dat \
src/cutscene/unknown_c37891.dat \
src/cutscene/unknown_c37a07.dat \
src/cutscene/unknown_c37a69.dat \
src/cutscene/unknown_c37b92.dat

LZ_FILES = $(addsuffix .lz, $(LZ_LIST))
CMP_FILES = $(addsuffix .cmp, $(CMP_LIST))

lz: $(LZ_FILES)
cmp: $(CMP_FILES)

# list of modules
MODULES = field btlgfx battle menu cutscene sound text gfx

# generate rules for making each module
define MAKE_MODULE
$1_SRC := $(wildcard src/$1/*) $(wildcard src/$1/*/*)
$$(OBJ_DIR)/$1-%.o: $$($1_SRC) $$(INC_FILES)
	@mkdir -p $$(OBJ_DIR)
	$$(ASM) $$(ASMFLAGS) -l $$(@:o=lst) src/$1/$1-main.asm -o $$@
endef

$(foreach M, $(MODULES), $(eval $(call MAKE_MODULE,$(M))))

# temporary compressed cutscene program file
LZ_DIR = temp_lz
CUTSCENE_LZ = $(LZ_DIR)/cutscene.lz
CUTSCENE_LZ_ASM = $(LZ_DIR)/cutscene_lz.asm

$(OBJ_DIR)/ff5-spc.o: src/sound/ff5-spc.asm $(INC_FILES)
	@mkdir -p $(OBJ_DIR)
	$(ASM) $(ASMFLAGS) -l $(@:o=lst) $< -o $@

$(SPC_PRG): cfg/ff5-spc.cfg $(OBJ_DIR)/ff5-spc.o
	$(LINK) $(LINKFLAGS) -o $@ -C $< $(OBJ_DIR)/ff5-spc.o

# list of all text files
TEXT_JSON_JP = $(wildcard src/text/*jp.json)
TEXT_JSON_EN = $(wildcard src/text/*en.json) \
src/text/dlg_jp.json \
src/text/monster_special_name_jp.json \
src/text/map_title_jp.json \
src/text/char_name_jp.json
TEXT_DAT_JP = $(TEXT_JSON_JP:json=dat)
TEXT_DAT_EN = $(TEXT_JSON_EN:json=dat)

text_jp: $(TEXT_DAT_JP)
text_en: $(TEXT_DAT_EN)
text: text_jp text_en

src/text/%.dat: src/text/%.json
	$(PYTHON) tools/encode_text.py $<

src/field/world_tilemap.dat: $(wildcard src/field/world_tilemap/world_tilemap_*.dat)
	$(PYTHON) tools/encode_world_tilemap.py

world_tilemap: src/field/world_tilemap.dat

battle_bg_tiles: $(addsuffix .bgt, $(wildcard src/gfx/battle_bg_tiles/*.scr))

%.bgt: %
	$(PYTHON) tools/encode_battle_bg_tiles.py $<

battle_bg_flip: $(addsuffix .bgf, $(wildcard src/gfx/battle_bg_flip/*.dat))

%.bgf: %
	$(PYTHON) tools/encode_battle_bg_flip.py $<

# rules for making ROM files
# run linker twice: 1st for the cutscene program, 2nd for the ROM itself
$(FF5_JP_PATH): cfg/ff5-jp.cfg spc text_jp cmp lz world_tilemap battle_bg_tiles battle_bg_flip $(OBJ_FILES_JP)
	@mkdir -p $(LZ_DIR) $(ROM_DIR)
	$(LINK) $(LINKFLAGS) -o "" -C $< $(OBJ_FILES_JP)
	$(PYTHON) tools/encode_cutscene.py $(CUTSCENE_LZ:lz=bin) $(CUTSCENE_LZ)
	@printf '.segment "cutscene_code_lz"\n.incbin "cutscene.lz"' > $(CUTSCENE_LZ_ASM)
	$(ASM) --bin-include-dir $(LZ_DIR) $(CUTSCENE_LZ_ASM) -o $(CUTSCENE_LZ).o
	$(LINK) $(LINKFLAGS) --dbgfile $(@:sfc=dbg) -m $(@:sfc=map) -o $@ -C $< $(OBJ_FILES_JP) $(CUTSCENE_LZ).o
	@$(RM) -r $(LZ_DIR)
	$(PYTHON) tools/fix_checksum.py $@

$(FF5_EN_PATH): cfg/ff5-en.cfg spc text_en cmp lz world_tilemap battle_bg_tiles battle_bg_flip $(OBJ_FILES_EN)
	@mkdir -p $(LZ_DIR) $(ROM_DIR)
	$(LINK) $(LINKFLAGS) -o "" -C $< $(OBJ_FILES_EN)
	$(PYTHON) tools/encode_cutscene.py $(CUTSCENE_LZ:lz=bin) $(CUTSCENE_LZ)
	@printf '.segment "cutscene_code_lz"\n.incbin "cutscene.lz"' > $(CUTSCENE_LZ_ASM)
	$(ASM) --bin-include-dir $(LZ_DIR) $(CUTSCENE_LZ_ASM) -o $(CUTSCENE_LZ).o
	$(LINK) $(LINKFLAGS) --dbgfile $(@:sfc=dbg) -m $(@:sfc=map) -o $@ -C $< $(OBJ_FILES_EN) $(CUTSCENE_LZ).o
	@$(RM) -r $(LZ_DIR)
	$(PYTHON) tools/fix_checksum.py $@
