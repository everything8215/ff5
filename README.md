# Final Fantasy V Disassembly

This is a disassembly of Final Fantasy V for the Super Famicom/SNES. It is a
work in progress which aims to build the following ROMs:

- Final Fantasy V 1.0 (J), CRC32: `0xC1BC267D`
- Final Fantasy V 1.10 (RPGe), CRC32: `0x17444605`

I'm happy to give access to this repo to anyone interested in contributing.

## Dependencies

Below is a minimal list of dependencies needed to build this project.
Adding tools or functionality which require additional dependencies should
be avoided if possible.

- GNU Make: <https://www.gnu.org/software/make/>
- cc65: <https://cc65.github.io>
- Python 3: <https://www.python.org/>

Here is a very nice tutorial explaining how to set up Cygwin and cc65 on
a Windows machine: <https://github.com/SlithyMatt/x16-hello-cc65>

## Project roadmap

This repo currently contains a barebones set of files which build a mostly empty
ROM file. The project will go through the stages below in order to
ultimately produce a ROM which matches the original game byte-for-byte.

IMPORTANT: Fair use privilege allows assembly files to be added to a public
repo even if they produce copyrighted material when assembled. However, raw
copyrighted material should never be added to the repo. All such copyrighted
assets must be extracted from an original ROM. This includes raw graphics,
text, and other game data files.

### Stage 1

All 65c816 and SPC-700 machine code is converted into assembly files that can be assembled back
into the original machine code. It is not necessary to add descriptive labels or for the code to be
relocateable at this stage.

The work can be split up into program "modules" that can be worked on
individually:

- Field/world module (bank `C0`)
- Battle graphics module (bank `C1` and `D9`)
- Battle module (`C2/0000-C2/9FFF`)
- Menu module (`C2/A000-C2/FFFF` and bank `D0`)
- Cutscene module (bank `C3`)
- Sound module (bank `C4`), includes SPC code and music/sfx scripts
- RPGe translation code (bank `E0`)

### Stage 2

Add labels to assembly code to allow for code insertions/deletions and
relocation.

Start to add descriptive names for labels and constant values (e.g. subroutines like `DrawNPC` or constants like `STATUS::POISON`).

### Stage 3

Add tools to extract all game data/assets from an original ROM and include everything into the assembly. Most assets will be included in the appropriate code modules
above, but a few additional modules will be added for more complex game data:

- Text module
- Graphics module (includes all graphics, palettes, and tilemaps)
- Event script module (banks `C8 and C9`)

When this stage is complete the project will produce
a fully matching ROM.

### Stage 4

Add RAM definitions for each module so that variables can be inserted and
deleted.

Continue cleaning up code and data for improved readability.

Potentially create editor tools to modify the more complex game data like maps/triggers and battle animations.

## Conventions

The conventions below are not required, especially in cases where existing
code is being added to the project, but should be followed as much as
possible to keep the project organized and easy to follow.

The Apple II DeskTop project has some nice ca65
coding style conventions here (<https://github.com/a2stuff/a2d/blob/main/docs/Coding_Style.md>)
although I'm not sure how strictly I would want to follow them.

### File Formats, Names, and Extensions

Assembly files have the extension `.asm`. This should include all files that
output bytes when assembled. In most cases, assembly files should only be
assembled once. The only exception is when the ROM contains multiple
identical copies of the same subroutine or data.

Include files have the extension `.inc`. These files can be
included multiple times and should not output any bytes to the assembler or
reserve any memory addresses. Examples include macro definitions, hardware
address definitions, and definitions of constant expressions. Include files
should begin with an include guard to ensure that they are not included more
than once.

Native graphics files should have an extension which describes the graphics
format such as `.4bpp` for 4 bits per pixel SNES graphics. SNES Palette files
(BGR555 format) should have the extension `.pal`. Screen tilemap files should
have the extension `.scr`.

Other binary data files can have any reasonable extension. I usually use `.dat`
if I can't think of anything better.

Compressed data files should have the same filename and extension as their
uncompressed counterpart with an appropriate extension added at the end (e.g.
`image.cgx.lz` is the compressed version of `image.cgx` in the same directory).

### File Organization

The `src` directory contains all of the assembly code and game data. The
`include` directory contains all of the include files.

Each of the modules described above is in a separate subdirectory within the
`src` directory. This mimics my best guess as to how the original source code
was organized based on e.g. the Playstation releases where each module has a
directory named after the main programmer for that module ('NARITA', 'YOSHII',
etc.). Each module directory contains all of the source code and data for that
module. The root directory of the repo contains a single Makefile to assemble each
module and link all of the object files together to create the ROM.

The `tools` directory contains a set of python scripts which are used to
extract and modify game data.

The `notes` directory contains my notes from reverse engineering the
assembly code. Much more hacking information is also available at <https://www.ff6hacking.com/ff5wiki/index.php/Main_Page>.

The `cfg` directory contains linker configuration files which ensure that the
assembled game data is placed in the ROM to match the original versions. I
don't know what assembler and linker were used to build the game by the
original development team, but it worked somewhat differently than ca65 and
ld65. Because of this, I had to use a lot more modules than would typically
be used in order to build a program of this size and complexity.

### Naming Conventions

As a naming convention for symbols, I've chosen to follow the example of the
Pokémon reverse engineering team (<https://github.com/pret>). Subroutine names
and labels for data in the ROM use PascalCase. Acronyms like RAM appear in all
capitals (i.e. `InitRAM`). This differs from conventional camel-case
capitalization rules.

External subroutines (which get imported/exported and can be called by other modules) get the special
suffix `_ext`. Also, dummy subroutines called by another bank (typically a
`jsr` followed by `rtl` or `jsl` followed by `rts` on the 65c816) get the
special suffix `_far` or `_near`, respectively.

Descriptive labels have been added for some of the subroutines in the code. Labels
for unknown or unlabeled subroutines are the 6-digit ROM address of the
subroutine (including the bank) preceded by an underscore, e.g. `_c28566`.

Local labels inside subroutines use lowercase with underscores separating words. Most local labels
are unnamed, and instead use the 4-digit ROM address (excluding the bank). I
also typically include a local label at the start of each subroutine so that
it can be compared to the original ROM file, but these are just for convenience
and can be removed eventually.

WRAM and SRAM labels begin with a lowercase `w` or `s` followed by a
descriptive name in PascalCase case, e.g. `wSpriteData`. Similarly, direct- or zero-page
labels begin with a lowercase `z`.

Hardware registers are a slight exception to the rule. I chose to use the
official register names from the SNES development manual in all caps,
prepended with a lowercase 'h' (i.e. `$2100` is `hINIDISP`).

Instruction mnemonics and macro names are in all lowercase. Macro names can
include underscores to improve readability.

Constant expressions are in all
uppercase with underscores between words. Enums should be used to group
constant values of a similar type (i.e. `STATUS::POISON` and `STATUS::HASTE`).
This is useful for error-checking and when several flag bits need to be OR'd
together.

To shorten subroutine and label names, the following are examples of shortened words that can be
used:

- anim: animation
- btm: bottom
- char: character
- cmd: command
- ctrl: control or controller
- dec: decrement
- div: divide
- dlg: dialogue
- dur: duration
- elem: element
- exec: execute
- gfx: graphics
- grp: group
- inc: increment
- init: initialize
- mod: modify or modifier
- msg: message
- mult: multiply or multiplier
- obj: object
- qty: quantity
- pal: palette
- prop: properties
- ptr: pointer
- rand: random
- reg: register
- sfx: sound effect
- tbl: table

### Tabs, Spaces, and Comments

.asm and .inc files should not have lines longer than 80 characters.
Never use tabs. Always use spaces. In assembly files, labels begin in column 1,
instructions and macros begin in column 9, operands begin in column 17, and
comments can begin in column 41. Long comments that would extend beyond the
80-character limit should be placed on their own line before the assembly
code that they describe.

An exception to these rules is for scripts made with macros instead of
instruction mnemonics because the macros names are often longer than 7
characters. In this case, operands begin in column 25.
