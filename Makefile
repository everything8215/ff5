
# the assembler
ASM = ca65
ASMFLAGS = -g -I include
VERSION_EXT =

# the linker
LINK = ld65
LINKFLAGS =

# script to fix the SNES checksum
FIX_CHECKSUM = python3 tools/fix_checksum.py

# list of ROM versions
VERSIONS = ff5-jp ff5-en
OBJ_DIR = obj
ROM_DIR = rom
ROMS = $(foreach V, $(VERSIONS), $(ROM_DIR)/$(V).sfc)

# the SPC program
# SPC_PRG = src/sound/ff5-spc.dat

.PHONY: all rip clean distclean \
	$(VERSIONS) $(MODULES)

# disable default suffix rules
.SUFFIXES:

# make all versions
all: $(VERSIONS)

# rip data from ROMs
rip:
	python3 tools/extract_assets.py

clean:
	$(RM) -rf $(ROM_DIR) $(OBJ_DIR)
	$(RM) -f src/text/*.dat

distclean: clean
	python3 tools/clean_assets.py

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

# list of all include files
INC_FILES = $(wildcard include/*.inc) $(wildcard include/*/*.inc)

# target-specific object filenames
OBJ_FILES_JP = $(foreach M,$(MODULES),$(OBJ_DIR)/$(M)-jp.o)
OBJ_FILES_EN = $(foreach M,$(MODULES),$(OBJ_DIR)/$(M)-en.o)

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

# list of all text files
TEXT_JSON_JP = $(wildcard src/text/*jp.json)
TEXT_JSON_EN = $(wildcard src/text/*en.json)
TEXT_DAT_JP = $(TEXT_JSON_JP:json=dat)
TEXT_DAT_EN = $(TEXT_JSON_EN:json=dat)

text_jp: $(TEXT_DAT_JP)
text_en: $(TEXT_DAT_EN)
text: text_jp text_en

src/text/%.dat: src/text/%.json
	python3 tools/encode_text.py $<

# rules for making ROM files
$(FF5_JP_PATH): cfg/ff5-jp.cfg text_jp $(OBJ_FILES_JP)
	@mkdir -p $(ROM_DIR)
	$(LINK) $(LINKFLAGS) --dbgfile $(@:sfc=dbg) -m $(@:sfc=map) -o $@ -C $< $(OBJ_FILES_JP)
	@$(RM) -rf $(LZ_DIR)
	$(FIX_CHECKSUM) $@

$(FF5_EN_PATH): cfg/ff5-en.cfg text_en $(OBJ_FILES_EN)
	@mkdir -p $(ROM_DIR)
	$(LINK) $(LINKFLAGS) --dbgfile $(@:sfc=dbg) -m $(@:sfc=map) -o $@ -C $< $(OBJ_FILES_EN)
	@$(RM) -rf $(LZ_DIR)
	$(FIX_CHECKSUM) $@
