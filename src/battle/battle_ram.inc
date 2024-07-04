;Defines and structures to label addresses used by combat routines

;Generally in order of their appearance in the game's address space
.org $7E0000

;$08-$23 are used as scratch variables
;not normally labeling these, but I do label some when info is passed between routines using them

;these are set for magic routines
TempAttachedSpell = $20		;spell cast routines will cast this spell after the normally queued spell (phoenix revive)
TempSkipNaming = $21		;spell will be cast without a name box
TempPartyTargets = $22		;targets for attached spell above
TempMonsterTargets = $23	;targets for attached spell above

;for monster ai
TempCharm = $22			;for monster ai, indicates a charmed action

;$24-27 are used for FF5's 8 bit multiply routine, before copying to SNES hardware multiplier
;$2A-31 are used for FF5's 16 bit multiply routine

AttackerOffset = $32	;16 bit offset into Character Structures (should be multiple of $80)
			;also used as a "current" character offset when processesing non-attack things
	;* Target/Attacker Offset = index * 80h
	;     0 - Character1
	;    80 - Character2
	;   100 - Character3
	;   180 - Character4
	;   200 - Monster1
	;   280 - Monster2
	;   ...
	;   580 - Monster8

SpellOffset = $34	;16 bit offset into CharSpells struct, char index * 650

TimerOffset = $36 	;16 bit offset into timer structures, pulled from rom for a given target
 
AttackerOffset2 = $39	;Offset into combat tables for multi-commands? 
			;equal to MultiCommand*12 

RNGA = $3A	
RNGB = $3B

;$3D - 46 are used for various things (temp area)
;for loot
TempMonsterIndex = $3D
TempMonsterOffset = $3F

;for ai
MMOffset = $3D			;init to Monster index of acting monster * 16 during AI script
MMTargetOffset = $3F		;init to Monster index of acting monster * 32 during AI script

AIScriptOffset = $41		;monster index *100 	
AIBufferOffset = $43		

AISpellCount = $45		;number of spells already in Spell Queue (MonsterMagic)

;for magic
HalfMP = $3D		;1 indicates half mp is active, 1 byte * 4 characters

SpellOffsetRandom = $41	;16 bit offset into CharSpells struct, char index * 650 
				;used when randomly determining a magic spell to cast

TempNumHits = $41		;meteor style multihit

TempSpellOffset = $45		;16 bit offset into CharSpells struct, char index * 650 

;for items
TempHand = $45			;0 for RH, 1 for LH
TempItemMagic = $46		


AttackerIndex = $47
wTargetIndex = $48
	;* Target/Attacker index:
	;   0 - Character1
	;   1 - Character2
	;   2 - Character3
	;   3 - Character4
	;   4 - Monster1 
	;   5 - Monster2 
	;   ... 
	;   B - Monster8

TargetOffset = $49	;16 bit offset into Character Structures (should be multiple of $80)

AIOffset = $4B

AtkElement = $4D
HitPercent = $4E
EvadePercent = $4F

Attack = $50		;Attack Damage during calcuations (16 bit)

M = $52		;Attack Multiplier (16 bit)

Defense = $54		;Defense value used during calculations (16 bit)
			;could be physical or magical depending on attack type
			
AtkMissed = $56		;Greater than 0 means attack has missed
				;80h is something specific (physical miss?)

;These are used for different things depending on attack type 
Param1 = $57	
Param2 = $58
Param3 = $59
			;Battle Params			$57		$58		$59
				
			;Weapons:	
			
			;30 Fists			Crit%		-		-
			;31 Swords: 			Element 	Proc% 		Proc
			;32 Knives: 			Element 	Proc% 		Proc
			;33 Spears: 			Element 	Proc% 		Proc
			;34 Axes: 			Hit%		Proc% 		Proc
			;35 Bows(Status):		Hit%		Type+Status%	Status 1/2
			;36 Bows(Element):		Hit%		Crit%		Element
			;37 Katanas:			Crit%		Proc%		Proc
			;38 Whips:			Hit%		Proc%		Proc
			;39 Bells:			-		-		-					
			;3A Long Axes:			Hit%		Proc%		Proc
			;3B Rods:			Hit%		-		Element
			;3C Rune:			Hit%		Dmg Boost	MP Cost
			;64 Chicken Knife		-		Proc%		Proc
			;6E Brave Blade			-		-		-
			;72 Bows(Creature):		Creature Type	-		-		
			;73 Spears(Creature):		Creature Type	-		-
				
			;Other:	
				
			;01 Monster Fight		-		-		-
			;02 Monster Specialty		-		-		Status (special format)
			;03-05 Magic Sword		Animation	Element		Status 
			;06 Magic			-		Spell Power	Element
			;07 Gravity			Hit%		*Fraction/16	Status 2
			;08 Flare			-		Spell Power	Element
			;09 Random			-		M		Element
			;0A Physical Magic		Hit%		Spell Power	Element
			;0B Level Based			Element		Type+Status%	Status 1/2
			;0C Flare w/HP Leak		Element		Spell Power	Duration
			;0D Drain			Hit%		*Spell Power	-
			;0E Psych			Hit%		*Spell Power	-
			;0F HP Critical			Hit%		*		-
			;10 Heal			-		Spell Power	-
			;11 Full Heal			-		Spell Power	-
			;12 Status 1			Hit%		Duration	Status 1
			;13 Status 2			Hit%		Duration	Status 2
			;14 Status 3			Hit%		Duration	Status 3
			;15 Toggle Status 1		Hit%		*		Status 1
			;16 Status 3 Exclusive		Hit%		Allowed Status3	Applied Status 3
			;17 Status 1 or Heal Undead	Hit%		*		Status1
			;18 Kill non-Heavy		Hit%		*		-
			;19 Remove Status		Status 1	Status 2	Status 3
			;1A Revive			Hit% (undead)	*Max MP		Fraction/4
			;1B Sylph			-		Spell Power	-
			;1C Elemental Defenses		Absorb		Immune		Weak
			;1D Scan			Scan Type	-		-
			;1E Drag			-		-		-
			;1F Void			Success%	-		-
			;20 Exit			Success%	-		-
			;21 Reset			-		-		-
			;22 Quick			-		-		-
			;23 Earth Wall			-		Spell Power	-
			;24 Potions			Base Heal	M		-
			;25 Ethers			Base Heal	M		-
			;26 Full Restore		HP/MP Bitmask	-		-
			;27 Status ignoring immunity	Status 1	Status 2	Status 3
			;28 Direct Magic Damage		Hit%		Damage		Damage (high byte)
			;29 Status 4			Hit%		Duration	Status 4
			;2A Damage by % Target Max HP 	Element		Fraction/16	HP Leak Duration
			;2B Damage by % Attacker HP	Element		Fraction/16	Status 1 (to attacker)
			;2C 50/50 Status1/2		Status 1	S2 Duration	Status 2
			;2D Ground Magic		-		Spell Power	Element
			;2E Phys Magic w/ Status 1	Hit%		Spell Power	Status 1
			;2F Status1 to Type (unused)	Creature Type	-		Status 1
			;30-3C Weapons, see above table
			;3D Death Claw			Hit%		Duration	Status 2
			;3E Failure (Mix)		Effect%		Duration	Status 4
			;3F Zombie Breath		-		M		-
			;40 Change Row			Hit%		Row Effect	-
			;41 Physical Attack (?)		-		Attack Power	-
			;42 Heal and Remove Status	Status Type	Spell Power	Status 1/2
			;43 Steal			Hit%		-		-
			;44 Escape			-		-		-
			;45 Throw			Hit%		-		-
			;46 Gil Toss			Cost per Level	M		-
			;47 Tame/Calm			-		Duration	Status 3
			;48 Catch			-		-		-
			;49 Flirt			Hit%		-		-
			;4A L5 Doom (unused)		Level Mult	-		Status 1
			;4B L5 Doom			Level Mult	-		Status 1
			;4C L2 Old			Level Mult	Duration	Status 2
			;4D L4 Quarter			Level Mult	Fraction/16	Status 2
			;4E L3 Flare			Level Mult	-		Element
			;4F Spirit			Zombie%		-		Fraction/4 HP
			;50 Goblin Punch		Hit%		-		Status 1
			;51 Modify Level or Defense	Hit%		Operation Bits	Amount
			;52 Mucus			Hit%		Allowed Status3 Applied Status 3
			;53 Damage % MP			Hit%		Fraction/16	-
			;54 MaxHP - CurHP Damage	Hit%		-		-
			;55 Fusion			-		-		Status 1 (to attacker)
			;56 Toggle Status 4 (unused)	Hit%		-		Status 4
			;57 HP Leak + Status		Status 1	Duration	Status 2
			;58 Mind Blast			Hit%		Spell Power	Paralyze Duration
			;59 Giant Drink			-		-		-
			;5A White Wind			-		-		-
			;5B Set Unused Byte (unused)	Hit%		-		Bits to set
			;5C Hug				Hit%		-		Status 1
			;5D Zombie Powder		Hit%		Fraction/16 HP	-
			;5E Persistent Songs		-		-		Song
			;5F Requiem			Creature Type	Spell Power	HP Leak Duration
			;60 Hide and/or Show Monsters	-		-		-
			;61 Stalker and Sandworm	-		-		-
			;62 Library Book Monster Swap	-		-		-
			;63 Grand Cross			-		-		-
			;64 Chicken Knife		-		Proc%		Proc
			;65 Interceptor Rocket		-		-		-
			;66 Targetting			-		-		-
			;67 Pull? 			-		-		-
			;68 Terminate Battle		-		-		-
			;69 Control			-		-		-
			;6A Win Battle			-		-		-
			;6B Add Type/Immunity/ElementUp Creature Type	Status 1	Element
			;6C Magic Strong vs Type	-		Spell Power	Creature Type
			;6D Vampire			Hit%		-		-
			;6E Brave Blade			-		-		-
			;6F Strong Fight		-		-		-
			;70 Wormhole			-		-		Status 4
			;71 Next Monster Form		-		-		-
			;72-73 Creature Bows/Spears, see above table
			;74 Miss			-		-		-
			;75 Do Nothing			-		-		-
			;7E Maps to 74			-		-		-
			;7F Maps to 75			-		-		-
			
			
			;  *	high bit here causes autohit except when it's monster vs party
			; 	likely unintended in some cases

;It's odd that these are separate bytes when they're also set to specific values
;these are likely used to determine what animation to play
SwordBlock	= $5A	;Set to 1 when attack is evaded by Hardened/Defender
KnifeBlock	= $5B	;Set to 2 when attack is evaded by Guardian
ElfCape = $5C		;Set to 3 when attack is evaded by Elf Cape
ShieldBlock = $5D  	;Set to 6 when attack is evaded by Shield
			;Set to 7 when attack is evaded by Aegis Shield vs Magic
BladeGrasp = $5E	;Set to 5 when attack is evaded by Blade Grasp passive skill

Crit = $5F		;Set to 1 when an attack crits (screen flash?)

MagicSword = $60 	;set when Magic Sword is applied
			;bypasses element calculations, some other effects

TargetDead = $61	;Set to 1 when Target is instantly killed (by MSword)
		
AtkHealed = $62

AttackerDamaged = $63	;makes attacker take damage from attack instead of target
			;no target healing though so isn't undead reversing drain
			;not sure where this is used?  

MonsterTargets = $65
wMonsterTargets = $65	;1 bit per monster

wPartyTargets = $66	;1 bit per party member then 0000
PartyTargets = $66

;$70 seems to be a temp var, used for passing data to/from C1 bank
MonsterDead = $70	
MenuCurrentChar = $70	
C1Temp = $70
		
;$00A2			;written to in C1 code, checked in AICondition0C

FieldItemsWon = $013B	;8 bytes, BattleItemsWon copied here at end

EncounterIndex = $04F0	;16 bit, index into $D03000 ROM combat tables

TerrainType = $04F2

.macro CreateFieldCharStruct name, addr
.struct name				;see battle version at CharStruct for details
.org addr
	CharRow 	 	.res 1     	;00
	Job 			.res 1		;01
	Level	 		.res 1		;02
	Exp			.res 3		;03-5
	CurHP			.res 2		;06-7
	MaxHP			.res 2		;08-9
	CurMP			.res 2		;0A-B
	MaxMP			.res 2		;0C-D
	Headgear		.res 1		;0E
	Bodywear              .res 1		;0F
	Accessory             .res 1		;10
	RHShield              .res 1		;11
	LHShield              .res 1		;12
	RHWeapon              .res 1		;13
	LHWeapon              .res 1		;14
	CaughtMonster         .res 1		;15
	BattleCommands        .res 4		;16-19
	Status1		.res 1		;1A
	Status2	        .res 1		;1B
	Status3       	.res 1		;1C 
	Status4		.res 1		;1D
	CmdStatus		.res 1		;1E
	DamageMod		.res 1		;1F
	Passives1             .res 1		;20
	Passives2             .res 1		;21
	ElementUp             .res 1		;22
	EqWeight              .res 1		;23
	BaseStr               .res 1		;24
	BaseAgi               .res 1		;25
	BaseVit               .res 1		;26
	BaseMag               .res 1		;27
	EquippedStr		.res 1		;28
	EquippedAgi           .res 1		;29
	EquippedVit           .res 1		;2A
	EquippedMag           .res 1		;2B
	Evade                 .res 1		;2C
	Defense               .res 1		;2D
	MEvade                .res 1		;2E
	MDefense              .res 1		;2F
	EAbsorb               .res 1		;30
	EBlock                .res 1		;31		
	EImmune               .res 1		;32
	EHalf                 .res 1		;33
	EWeak                 .res 1		;34
	StatusImmune1         .res 1		;35
	StatusImmune2		.res 1		;36
	StatusImmune3         .res 1		;37
	WeaponProperties      .res 1		;38
	ArmorProperties       .res 1		;39
	JobLevel              .res 1		;3A
	AP                    .res 2		;3B-C
	EnableSpells     	.res 3		;3D   		Sword/White	4 bits high/low unpacked into separate bytes later
						;3E		Black/Time
						;3F		Summon/Misc	(Misc would be Songs/Blue but seems unused)
	EquipWeapons		.res 2		;40		bitmask for equippable weapons
	EquipArmor		.res 2		;42		bitmask for equippable armor
	MonsterAttack		.res 1		;44		Only used for Goblin Punch and Monsters
	MonsterAttackLH       .res 1		;45		Only used for Goblin Punch 
	;rest differs from the battle CharStruct
	;could use Asar struct extensions but syntax would be annoying for the often-used CharStruct
	Unused			.res 4		;46-2049
	MasteryStr		.res 1		;4A		;mastery entries are for freelancer
	MasteryAgi             .res 1          ;4B
	MasteryVit             .res 1          ;4C
	MasteryMag             .res 1          ;4D
	MasteryPassives1       .res 1          ;4E
	MasteryPassives2       .res 1          ;4F
.endstruct
name := name::CharRow
.endmacro

CreateFieldCharStruct FieldChar, $0500
;FieldCharStats = $0500

FieldItems = $0640	;256 item ids
FieldItemsQty = $0740	;256 item qtys

FieldAbilityCount = $08F3	;1 byte per character
FieldAbilityList = $08F7	;20 bytes per character in order butz,lenna,galuf,faris

Gil = $000947		;3 bytes, also accessed via 3 byte address for some reason

FieldFrameCount = $094A	;4 bytes, time played (in frames)

MonsterKillCount = $094E	;2 bytes

MagicBits = $0950	;Learned magic
			;12 bytes, 1 bit per spell
			;4 bytes unused before blue magic (hardcoded to .res them though)

BlueMagicBits = $0960	;4 bytes (hardcoded to .res first 2 bits of first byte)
			;may be 12 more bytes free?

Config1 = $0970		;80h: 		command style
			;10h-40h: 	message speed
			;08h: 		battle mode
			;01h-04h: 	battle speed

.macro CreateBattleDataStruct name, addr
.struct name
.org addr
	MagicLamp 	.res 1		;09B4		;7C74
	Escapes	.res 1		;09B5		;7C75
	WonderRod	.res 1		;09B6		;7C76
		.res 9		;09B7-09BF	;7C77-7C8F	;unknown/unused?
	Battles	.res 2		;09C0		;7C80
	Saves		.res 2		;09C2		;7C82
	EventFlags	.res 16		;09C4		;7C84 - 00: Victory; 01: Game Over; 02 - Escaped
							;7C85 - 7C93 other flags?
							;7C87 some kind of party bitmask for slots to check (AICondition0D)
.endstruct
name := name::MagicLamp
.endmacro

CreateBattleDataStruct FieldData, $09B4
CreateBattleDataStruct BattleData, $7C74

RNGSeed = $0AF9

FieldTimerEnable = $0AFB
FieldTimer = $0AFC		;frame count, 16 bit
FieldTimerEnd = $0AFE		;16 bit

MusicData = $1D00

.struct CharStruct
.org $2000
	CharRow 	 	.res 1     	;$2000
						;For Characters: first 3 bits indicate who it is(Butz, etc.) as a list
						;then, 	08h	Gender (Male=0 / Female=1)
						;	40h	Not in the team	?
						;	80h	Back Row
						;For Monsters:
						;	40h	Always set?
						;	80h 	Back Row
	Job 			.res 1		;$2001
	Level	 		.res 1		;$2002
	Exp			.res 3		;$2003-5
	CurHP			.res 2		;$2006-7
	MaxHP			.res 2		;$2008-9
	CurMP			.res 2		;$200A-B
	MaxMP			.res 2		;$200C-D
	Headgear		.res 1		;200E
	Bodywear              .res 1		;200F
	Accessory             .res 1		;2010
	RHShield              .res 1		;2011
	LHShield              .res 1		;2012
	RHWeapon              .res 1		;2013
	LHWeapon              .res 1		;2014
	CaughtMonster         .res 1		;2015
	BattleCommands        .res 4		;2016
						;2017
						;2018
						;2019
	Status1		.res 1		;201A
				;80h = Wounded/Dead
				;40h = Stone
				;20h = Toad
				;10h = Mini
				;08h = Float
				;04h = Poison
				;02h = Zombie
				;01h = Blind
				;00h = Normal
	Status2	        .res 1		;201B
				;80h Aging
				;40h Sleep
				;20h Paralyze
				;10h Charm
				;08h Berserk
				;04h Mute
				;02h Image x2
				;01h Image x1
	Status3       	.res 1		;201C
				;80h Reflect
				;40h Armor
				;20h Shell
				;10h Stop
				;08h Haste
				;04h Slow
				;02h Invulnerable
				;01h Regen
	Status4		.res 1		;201D
				;80h Erased
				;40h False Image
				;20h Controlled
				;10h Countdown
				;08h HP Leak
				;04h Singing
				;02h Critical
				;01h Hidden
	CmdStatus		.res 1		;201E
				;08h: If Flirted
				;10h: If Jumping
				;40h: If Guarding
				;80h: If Defending
	DamageMod		.res 1		;201F
				;80h: Auto hit
				;40h: Damage = Damage * 2
				;20h: Damage = Damage / 2
				;10h: M = M * 2
				; 8h: M = M / 2
				; 4h: Defense = 0
				; 1h: Damage * 2 vs Humans (80h Creature Type)
	Passives1             .res 1		;2020
				;80h:Counter
				;40h:Evade
				;20h:Barrier
				;10h:Learning
				;08h:Dash
				;04h:DamageFloor
				;02h:Pitfalls
				;01h:Passages
	Passives2             .res 1		;2021
				;80h:Cover
				;40h:Brawl
				;20h:Double Grip
				;10h:Medicine
				;08h:Berserk
				;04h:Caution
				;02h:Pre-Emptive
				;01h:2-Handed
	ElementUp              .res 1		;2022
				;80h Water 	note: 80h can't come from equipment
				;40h Wind
				;20h Earth
				;10h Holy
				;08h Poison
				;04h Lightning
				;02h Ice
				;01h Fire
	EqWeight              .res 1		;2023
	BaseStr               .res 1		;2024
	BaseAgi               .res 1		;2025
	BaseVit               .res 1		;2026
	BaseMag               .res 1		;2027
	EquippedStr		.res 1		;2028
	EquippedAgi           .res 1		;2029
	EquippedVit           .res 1		;202A
	EquippedMag           .res 1		;202B
	Evade                 .res 1		;202C
	Defense               .res 1		;202D
	MEvade                .res 1		;202E
	MDefense              .res 1		;202F
	EAbsorb               .res 1		;2030
	EBlock                .res 1		;2031		likely unused but seems functional?
	EImmune               .res 1		;2032
	EHalf                 .res 1		;2033
	EWeak                 .res 1		;2034
	StatusImmune1         .res 1		;2035
	StatusImmune2		.res 1		;2036
	StatusImmune3         .res 1		;2037
	WeaponProperties      .res 1		;2038
				;80h: Sword parry (Hardened, Defender)
				;40h: Knife parry (Guardian)
				;20h: Initiative
				;08h: Magic on hit
				;04h: Magic Sword OK
				;02h: Command instead of attack
				;01h: Wonder Rod
	ArmorProperties       .res 1		;2039
				;80h Magic block (Aegis shield)
				;40h Elf Cape dodge
				;20h Improved Brawl
				;10h Improved Steal
				;08h Half MP cost
				;04h Sword Dance / Flirt up
				;02h Become undead
				;01h Improved Catch
	JobLevel              .res 1		;203A
	AP                    .res 2		;203B-C
	EnableSpells     	.res 3		;203D   	Sword/White	4 bits high/low unpacked into separate bytes later
						;203E		Black/Time
						;203F		Summon/Misc	(Misc would be Songs/Blue but seems unused)
	EquipWeapons		.res 2		;2040		bitmask for equippable weapons
						;		low byte: 80h	Katana		high: 80h
						;                         40h	Hammer                40h	Bell
						;                         20h	Axe                   20h	Whip
						;                         10h	Spear                 10h	Harp
						;                         08h	Knight Sword          08h	Bow
						;                         04h	Sword                 04h	Flail
						;                         02h	Ninja Knife           02h	Staff
						;                         01h	Knife                 01h	Rod
	EquipArmor		.res 2		;2042		bitmask for equippable armor
						;		low byte: 80h	Common Gear	high: 80h
						;                         40h	Mage Robe             40h
						;                         20h	Light Armor           20h
						;                         10h	Heavy Armor           10h	Mage Hat
						;                         08h	Dancer Gear           08h	Chemist Gear
						;                         04h	Light Helmet          04h	Thief Gear
						;                         02h	Heavy Helmet          02h	Light Accessory
						;                         01h	Shield                01h	Heavy Accessory
	MonsterAttack		.res 1		;2044		Only used for Goblin Punch and Monsters
	MonsterAttackLH       .res 1		;2045		Only used for Goblin Punch
	Reaction1Command	.res 1		;2046		;post-remap command table values, unlike .Command
	Reaction1Magic	.res 1		;2047
	Reaction1Item		.res 1		;2048
	Reaction1Element	.res 1		;2049
	Reaction1Category	.res 1		;204A
	Reaction1Targets	.res 1		;204B
	Reaction1Damage	.res 1		;204C
	Reaction2Command	.res 1		;204D
	Reaction2Magic	.res 1		;204E
	Reaction2Item		.res 1		;204F		;remaining 2nd counter attacks are at $207B
	MSwordElemental1	.res 1		;2050
	MSwordElemental2      .res 1		;2051
	MSwordElemental3      .res 1		;2052
	MSwordStatus1         .res 1		;2053
	MSwordStatus2         .res 1		;2054
	MSwordStatusSpecial   .res 1		;2055
				;80h Power-up (Flare sword)
				;40h HP drain (Drain sword)
				;20h MP drain (Psych sword)
	ActionFlag		.res 1		;2056
				;80h Physical/Other (physical attacks but also aborted commands)
				;40h Item
				;20h Magic
				;10h Weapon used as Item
				;08h X-Magic
				;01h Costs MP
	Command		.res 1		;2057		;queued command
								;maps to CommandTable as follows:
								;$00	-> $00
								;$01-2B	-> $00-2A	(character menu commands)
								;$2C-4D	-> $2B 		(all magic commands)
								;$4E-56 -> $2C-34	(commands used internally)
	MonsterTargets	.res 1		;2058		Monster Target
	PartyTargets          .res 1		;2059		Character Target
	SelectedItem          .res 1		;205A		Chosen Item (for throw, etc), or spell (for magic)
								;initially this is a menu selection index then later replaced by the item/spell id
	SecondActionFlag	.res 1		;205B
	SecondCommand   	.res 1		;205C
	SecondMonsterTargets	.res 1		;205D
	SecondPartyTargets	.res 1		;205E
	SecondSelectedItem	.res 1		;205F
	Unused1		.res 1		;2060		Unused?
	CmdCancelled		.res 1		;2061		set to 1 when zombie/charm/berserk cancels a player command
	MonsterM		.res 1		;2062
	Unused2		.res 1		;2063		Unused?
	CantEvade             .res 1		;2064
	CreatureType          .res 1		;2065
				;80h Human
				;40h Desert
				;20h Heavy
				;10h Dragon
				;08h Aevis
				;04h Creature/Beast
				;02h Archae? Frog? (only set on ArchaeToad)
				;01h Undead
	CmdImmunity           .res 1		;2066		Command Immunity
						;		80h Scan
						;		10h Control
						;		08h Catch
	RewardExp		.res 2		;2067
	RewardGil		.res 2		;2069
	StolenItem		.res 1		;206B		When an item is stolen from a monster the item id is recorded here
	RHCategory      	.res 1		;206C		;these are initialized but never used
	LHCategory     	.res 1		;206D
	Specialty             .res 1		;206E
				;80h Auto-hit and ignore defense
				;40h Old
				;20h Poison
				;10h Blind
				;08h Paralyze
				;04h Charm
				;02h HP Leak
				;01h 1.5x damage
	Song                  .res 1		;206F
				;80h Strength
				;40h Agility
				;20h Vitality (unused)
				;10h Magic
				;08h Level
	AlwaysStatus1		.res 1		;2070
	AlwaysStatus2	     	.res 1		;2071
	AlwaysStatus3	     	.res 1		;2072
	AlwaysStatus4	     	.res 1		;2073
	BonusStr		.res 1		;2074		For Songs
	BonusAgi		.res 1		;2075
	BonusVit		.res 1		;2076		No song affects this, likely unused
	BonusMag		.res 1		;2077
	BonusLevel		.res 1		;2078
	Unused3		.res 1		;2079		Only used in Atk Type 5B which is itself unused
						;		Optional power drink fix uses this byte
	MSwordAnim		.res 1		;207A		Used for Magic Sword Animations, high bit is hand
	Reaction2Element	.res 1		;207B
	Reaction2Category	.res 1		;207C
	Reaction2Targets	.res 1		;207D
	Reaction2Damage	.res 1		;207E
	SpecialtyName		.res 1		;207F
.endstruct
CharStruct = CharStruct::CharRow



.struct SavedCharStats	;saved before releasing a monster (which overwrites stats), then later restored
.org $2600
	Level			.res 1	;2600
	MonsterAttack		.res 1	;2601
	MonsterM		.res 1	;2602
	EquippedMag		.res 1	;2603
	CharRow		.res 1	;2604
	Status1               .res 1	;2605
	Status2               .res 1	;2606
	Status3               .res 1	;2607
	Status4               .res 1	;2608
	CmdStatus             .res 1	;2609
	DamageMod             .res 1	;260A
	Passives1             .res 1	;260B
	Passives2             .res 1	;260C
	ElementUp             .res 1	;260D
	MSwordElemental1	.res 1	;260E
	MSwordElemental2      .res 1	;260F
	MSwordElemental3      .res 1	;2610
	MSwordStatus1         .res 1	;2611
	MSwordStatus2         .res 1	;2612
	MSwordStatusSpecial   .res 1	;2613
	AlwaysStatus1		.res 1	;2614
	AlwaysStatus2	     	.res 1	;2615
	AlwaysStatus3	     	.res 1	;2616
	AlwaysStatus4	     	.res 1	;2617
	BonusStr		.res 1	;2618
	BonusAgi		.res 1	;2619
	BonusVit		.res 1	;261A
	BonusMag		.res 1	;261B
	BonusLevel		.res 1	;261C
	Unused3		.res 1	;261D	;this byte is saved/restored even though it is never used
	MSwordAnim		.res 1	;261E
	UnusedSave		.res 1	;261F	;there's space for a 20th byte to be saved, but it is never used
.endstruct

;note, $2620 area is used as a temp area for a lot of different stuff in different routines
Temp = $2620
TempStats = $2622

;for monster AI routines
AITargetOffsets = $2620	;2 bytes * 12 characters, Char.struct offsets of targets, terminated early by

;%CreateMagicInfoStruct(TempMagicInfo,$7E262A)

AIBuffer = $2640		;AI buffer for monsters, max 64 bytes, ends with $FF

TempTargetting = $26A0		;sometimes is number of targets minus 1
				;sometimes is ability targetting information (a bitfield)

.macro CreateMonsterStatsStruct name, addr
.struct name
.org addr
	Speed		.res 1		;26F0	;3EFF	;D00000	;D02000
	AttackPower		.res 1          ;26F1	;3F00	;D00001
	AttackMult		.res 1          ;26F2	;3F01	;D00002
	Evade			.res 1          ;26F3	;3F02	;D00003
	Defense		.res 1          ;26F4	;3F03	;D00004
	MagicPower		.res 1          ;26F5	;3F04	;D00005
	MDefense		.res 1          ;26F6	;3F05	;D00006
	MEvade			.res 1          ;26F7	;3F06	;D00007
	HP			.res 2          ;26F8	;3F07	;D00008		;sets max and current HP
	MP			.res 2          ;26FA	;3F09	;D0000A		;monsters always have 9999 max mp, this sets current MP
	Exp			.res 2          ;26FC	;3F0B	;D0000C
	Gil			.res 2          ;26FE	;3F0D	;D0000E
	AttackFX		.res 1          ;2700	;3F0F	;D00010
	EImmune		.res 1          ;2701	;3F10	;D00011
	StatusImmune1		.res 1          ;2702	;3F11	;D00012
	StatusImmune2		.res 1          ;2703	;3F12	;D00013
	StatusImmune3		.res 1          ;2704	;3F13	;D00014
	EAbsorb		.res 1          ;2705	;3F14	;D00015
	CantEvade		.res 1          ;2706	;3F15	;D00016
	EWeak			.res 1          ;2707	;3F16	;D00017
	CreatureType		.res 1          ;2708	;3F17	;D00018
	CmdImmunity		.res 1          ;2709	;3F18	;D00019
	Status1		.res 1          ;270A	;3F19	;D0001A		;high bit here means all status are always instead of initial
	Status2		.res 1          ;270B	;3F1A	;D0001B
	Status3		.res 1          ;270C	;3F1B	;D0001C
	Status4		.res 1          ;270D	;3F1C	;D0001D
	EnemyNameID		.res 1          ;270E	;3F1D	;D0001E
	Level			.res 1          ;270F	;3F1E	;D0001F
.endstruct
name := name::Speed
.endmacro

CreateMonsterStatsStruct TempMStats, $26F0

;$2720 region is still temp area, reused for various things

;for many commands
TempTargetBitmask = $2720	;16-bit target bitmask
				;CCCCMMMM in first byte
				;MMMM.... in second byte

;for GetItemUsableY and GetItemUsableA
;%CreateEquippableStruct(TempEquippable,$7E2720)		;4 bytes

;for routines CheckMonsterDeath and CheckPartyDeath
TempStartIndex = $2720
TempStopIndex = $2721
TempIsMonster = $2722

;for monster AI
AIParam1 = $2721
AIParam2 = $2722
AIParam3 = $2723

AIMultiTarget = $2724	;if 0, a random target in AITargetOffsets should be used
			;if 1, all targets in AITargetOffsets should be used
AITargetCount = $2725

;for casting spells (including item and combine)
TempSpell = $2722
TempIsEffect = $2723	;determines whether spell is in the regular spell data or effect data

;for dance and weapon effects
TempDance = $2733
TempEffect = $2733

InventoryItems = $2734
InventoryQuantities = $2834

InventoryTargetting = $2A34	;only set for weapons and consumables

InventoryFlags	= $2B34		;256 bytes initialized to $80
				;Armor or item 00 = $5A
				;80 and 40h are from EquipmentType for Weapons
				;entire byte from EquipmentType for consumables

				;80h: not usable (from EquipmentType)
				;40h: Not Throwable (from EquipmentType), always set for armor
				;20h: not usable (for other/consumables)
				;10h: set for weapons and armor
				;08h: set for weapons and armor
				;04h: Double Grip Only
				;02h: set for weapons and armor
				;01h

InventoryUsable = $2C34	;2 bits per character (256 items)
				;10 for not usable, 00 for usable
				;$AA means not usable for anyone
				;unsure what the 2nd bit is for (nothing maybe)

.struct CharSpells	;650 bytes * 4 characters
.org $2D34
	ID		.res 130	;2D34
	Level		.res 130	;2DB6		;init to $81
	MP		.res 130	;2E38
	Targetting	.res 130	;2EBA
	Flags		.res 130	;2F3C		;init to $81, 0 when void
							;80h: spell disabled
							;01h: skips mp/status checks
.endstruct

.struct CharCommands		;20 bytes * 4 characters
.org $375C
	ID 		.res 4		;375C 		;command ids (such as Fight, Mug, Item, etc)
	Level		.res 4		;3760		;probably unused
	MP		.res 4		;3764		;probably unused
	Targetting	.res 4		;3768
	Flags		.res 4		;376C 		;init to $80, disabled
.endstruct

.struct HandItems		;12 bytes * 4 characters	;similar but doesn't match the others!
.org $37AC
	ID		.res 2		;
	Level		.res 2		;37AE	;probably unused?, init to 1 when there's something in your hand tho
	MP		.res 2		;37B0	;probably unused
	Targetting	.res 2		;37B2
	Flags		.res 2		;37B4	init to $80
	Usable		.res 2		;37B6	same style as InventoryUsable, 2 bits per char
.endstruct

.struct CharControl	;command info when controlling monsters, 20 bytes
				;one structure for each character
.org $37DC
	Actions	.res 4		;$37DC	 monster commands
	Level		.res 4		;$37E0	;probably unused
	MP		.res 4		;$37E4	;probably unused
	Targetting 	.res 4		;$37E8	 monster command targetting info
	Flags 		.res 4		;$37EC	 monster command flags
						;Action $FF 	-> sets this to $80
						;Others 	-> sets this to $00
.endstruct

.struct CharVitals	;8 bytes * 4 characters
	CurHP		.res 2
	MaxHP		.res 2
	CurMP		.res 2
	MaxMP		.res 2
.endstruct


.macro CreateGFXAIQueue name, addr
.struct name   	;5 byte structure for queued animations, 102 entries (512/5)
.org addr		;fairly sure that putting $FF in any byte but the first one will break things

				;structure also used for monster ai?
				;Flag can be FE for end of a block or FD for monster commands

				;GFXQueue						;MonsterAIScript
	Flag	.res 1	;$384C	;00 for valid actions, $FF for empty		$4367
	Cmd	.res 1	;$384D	;Command Value, usually $FC for graphics        $4368
	Type	.res 1	;$384E	;Command Type (for Command $FC)                 $4369
	Data1	.res 1	;$384F	;use varies by command type                     $436A
	Data2	.res 1	;$3850	;use varies by command type                     $436B
.endstruct
name = name::Flag
.endmacro

CreateGFXAIQueue GFXQueue, $384C

;gonna dump some command stuff here to try to figure out what's going on with GFXQueue commands
;Character Command 01 Other and 24 Dummy01
	;for monsters copies 100 bytes from MonsterAIScript into GFXQueue

;from c1 dissasembly:
; graphics command jump table
;   0: attack animation
;   1: ability/command animation
;   2: special ability animation (animals/conjure/combine/terrain)
;   3: monster special attack animation ???
;   4: show attack name
;   5: display battle messages
;   6: show damage numerals
;   7: magic
;   8: no effect
;   9: Item animation?
;   A: Weapon-as-Item animation?

;C1/8B7D: B677 8D5F 8CF1 8CF1 8BE4 8C37 01FE B68C
;C1/8B8D: 8BA9 8BAA 8B93

;command FC (graphics commands)
;01: ability/command animation
	;Data1: command table id
	;Data2: 2nd param varies
	;example: $04 (Fight), Magic Sword animation

;04: show attack name
;	;Data1: string table
	;Data2: string id
	;example: $04 (Items), Item ID

;05: show system message (can't run, etc)
	;Data1: message

;06: show damage numerals
	;Data params both always 0?

;07: magic
	;Data1: spell

;09/0A: Item animations?
	;Data1: selected item

;the following are Monster AI related and should? all have FD as their flag byte
;they also use bytes .2 and .3 for their params, and .4 is a copy of .3
;<F0 is a normal spell (picked 1 of 3), probably doesn't ever make it to GFXQueue before being processed/deleted
;F1: no action (also unlikely to make it to GFXQueue)
;F2: show monsters
	;Param1:	type bits (only having $40 set does anything)
	;Param2:	input:
	;		set of potential monsters to show (bitfield),
	;		. or a completely random one (ignoring param1) if 0
	;		output (Data1):
	;		selected monster to show (bitfield)
;F3: set target
	;Param1: 	AITarget routine
;F4: set variable
	;Param1: 	Var (0-3)
	;Param2:	Value
;F7: copies extra bytes to AI buffer (maybe for no-interrupt multiple commands?)
	;Param1:	# extra bytes to copy (min 1)
;F9: set event flag
	;Param1:	Event Flag byte (0-15)
	;Param2:	Bits to set
;FA: set stats or toggle status
	;Param1:	Offset within CharStruct
	;Param2:	Value to set (if not status or alwaysstatus bytes)
	;		. or Status to toggle (only highest bit applies)

DisplayDamage = $3A4C		;a displayed damage number (16 bit)
				;high bit set indicates healing, another flag with 40h
				;table of these is oddly structured:  monsters then party
				;with another offset $79FB * 24 on top of that so there's more than one table of them

;7 byte structure here (indexed by multicommand*7)
;saves data after an action to allow it to be animated by C1 bank code
.macro CreateActionAnim name, addr
.struct name
.org addr
	Flags		.res 1	;$3BCC	;3BD3	;3BDA	;3BE1
							;80h Attacker was a monster
							;40h Target was a monster
							;20h UnknownReaction $7BFB (changes party member position/stance?)
							;10h Crit or instant killed by MSword
							;08h FightFlag (basic fight-ish actions)
							;02h SpiritFlag
							;01h Monster Specialty (Magic $81)

	OrigAttacker 		.res 1	;$3BCD	;3BD4	;party or monster index

	OrigTargetBits 	.res 1	;$3BCE	;3BD5
	TargetBits 		.res 1	;$3BCF	;3BD6	;3BDD
	ReflectorBits		.res 1 	;$3BD0	;3BD7
	ReflecteeBits		.res 1	;$3BD1	;3BD8
	CoveredBits 		.res 1	;$3BD2	;3BD9
.endstruct
.endmacro

;.org $7E3BCC
CreateActionAnim ActionAnim0, $3BCC
CreateActionAnim ActionAnim1, $3BD3
CreateActionAnim ActionAnim2, $3BDA
CreateActionAnim ActionAnim3, $3BE1


BlockType = $3C3C		;type of block
					;Set to 1 for Hardened/Defender
					;Set to 2 for Guardian
					;Set to 3 for Elf Cape
					;Set to 4 for Golem
					;Set to 5 for Blade Grasp passive skill
					;Set to 6 for Shield
					;Aegis would be 7, but isn't set here

				;indexed by Multicommand, 32 bytes cleared here when attacks are set up

AegisBlockTarget = $3C4C	;bit set for aegis-blocking target
				;indexed by Multicommand

TempDisplayDamage = $3C5D	;temporarily holds damage amount for poison/regen (16 bit)

MessageBoxes = $3C5F	;24 bytes of messages to display, uses single byte constants for each message
			;actually there's two 24 byte arrays here, one for each command of x-magic
			;might be four?

MessageBoxData = $3CBF		;12 bytes of numbers to use in message boxes, 4x 3 byte numbers
				;use varies by which message is set in MessageBoxes

.struct MessageBoxData	;basic 3-byte struct to make things look a bit nicer
 .res 1
 .res 1
 .res 1
.endstruct


MessageBoxOffset = $3CEF
MessageBoxDataOffset = $3CF0

.macro CreateTimerStruct name, addr
.struct name		;11 bytes, 10 for status timers (1 = enabled) and one for ATB (which may not be used in all of them)
.org addr			;there's an array of 12 of these structs each time it's used, one for each combatant
					;Enable		;Current	;Initial	EndedChar	;Global		;Process
	Stop	.res 1		;$3CF1		;$3D75		;$3DF9 		;3EB7		;$3ED9 		;$3EE4
	Poison	.res 1		;$3CF2  	;$3D76  	;$3DFA		;3EB8
	Reflect   .res 1		;$3CF3  	;$3D77  	;$3DFB          ;3EB9
	Countdown .res 1          ;$3CF4  	;$3D78  	;$3DFC          ;3EBA
	Mute      .res 1          ;$3CF5  	;$3D79  	;$3DFD          ;3EBB
	HPLeak  	.res 1          ;$3CF6  	;$3D7A  	;$3DFE          ;3EBC
	Old       .res 1          ;$3CF7  	;$3D7B  	;$3DFF          ;3EBD
	Regen     .res 1          ;$3CF8  	;$3D7C  	;$3E00          ;3EBE
	Sing	.res 1		;$3CF9		;$3D7D		;$3E01          ;3EBF
	Paralyze  .res 1          ;$3CFA  	;$3D7E  	;$3E02          ;3EC0
	ATB		.res 1		;$3CFB		;$3D7F		;$3E03	;tends to be used differently than status timers

.endstruct
name = name::Stop	;makes a label so we don't have to keep specifying a property when accessing the structure as a whole
.endmacro


CreateTimerStruct EnableTimer, $3CF1		;usually 1 here means enabled, but has bitflags
						;for any:	80h bit set when timer ends to flag it to be processesd later
						;for ATB: 	0 takes no action when timer expires (opens command menu if party and not disabled)
						;		1 when waiting for turn
						;		41h waiting for delayed action (such as spell cast or queued berserker attack)

CreateTimerStruct CurrentTimer, $3D75

CreateTimerStruct InitialTimer, $3DF9	;ATB is not used here

CreateTimerStruct RandomOrderIndex, $3E7D	;holds current index in RandomOrder for each timer

CreateTimerStruct TimerEnded, $3E88		;1 when a timer ends and needs its effect applied

RandomOrder	= $3E93		;12 bytes, randomized list of unique numbers 0-11
				;(combatant processing order for timers)

PauseTimerChecks = $3E9F 	;12 bytes, cancels timer checking for char when set

QuickTimeFrozen = $3EAB		;12 bytes, one for each participant
					;>0 means time frozen (so someone else can be Quick)

CreateTimerStruct TimerReadyChar, $3EB7	;char index of char that just had a timer end

ActiveParticipants = $3EC2
			;12 byte table for battle participants
			;death spells miss participant if 0, may be list of active (not dead/stoned) participants?
			;for determining game over or victory, maybe?

ProcessingTimer = $3ECE	;id of the timer which is in the process of being triggered (16 bti)

ATB = $3ED0		;1 byte * 4 chars
			;copied from CurrentTimer.ATB

ATBWaiting = $3ED4	;set to 1 when ATB is paused due to a character's turn

ATBWaitLeft = $3ED5	;number of ticks left on ATBWaiting delay, starts at ATBDelay

ATBWaitTime = $3ED6	;ATB Delay when a character's turn comes up, from battle speed and Drag spell

StatusFixedDur = $3ED7	;When set, status effects with durations use an alternate formula
			;this is usually a fixed duration instead of a spell-based duration

StatusDuration = $3ED8

CreateTimerStruct GlobalTimer, $3ED9		;init from rom $D12976
CreateTimerStruct ProcessTimer, $3EE4	;set to 1 when GlobalTimer runs out


.macro CreateEncounterInfoStruct name, addr
.struct name 	;EncounterInfo from rom encounter info table at, 16 bytes
.org addr
	IntroFX		.res 1		;$3EEF		;3 Low bits are monster intro animation (0-7)
									;0	Normal
									;1	Drop
									;2	Scroll Up
									;3	Fade		Not Used
									;4	Cinematic	Demo Battles
									;5	Scroll Right
									;6	?
									;7	?
								;High bits are a bitfield for special effects
									;08	?		Not used
									;10	Wall Barrier	Used in Phoenix Tower
									;20	Invalid Target	Used in Final Battle
									;40	Underwater	Used in Sunken Worus Tower
									;80	Credits
	FleeChance		.res 1		;$3EF0		;Flee Chance = 80h if Can't Run
	AP			.res 1		;$3EF1
	Visible		.res 1		;$3EF2		;1 bit per monster
	MonsterID		.res 8		;$3EF3-$7EFA	;one byte for each monster
	Palettes		.res 2		;$3EFB-7EFC	;2 bits per monster
	Music			.res 1		;$3EFD		;index into $D0EEDF table
	Flags			.res 1		;$3EFE		;80h Always Back Attack
								;40h Can't use Void
								;20h Boss Battle (changes if MonsterID is in regular monsters or bosses)
								;10h Sandworm
								;08h Minotauros
								;04h Always Void
								;02h No Reward
								;01h Alternate Death
.endstruct
name = name::IntroFX
.endmacro

CreateEncounterInfoStruct EncounterInfo, $3EEF

CreateMonsterStatsStruct MonsterStats, $3EFF	;32 bytes * 8 monsters

MonstersVisibleUnused = $3FFF		;1 bit per monster, copied from EncounterInfo.Visible and doesn't seem to ever get used

MonsterCoordinates = $4000

MonsterNameID = $4008			;2 bytes per monster, >255 indicates boss

InitialMonsters = $4018		;1 byte per monster, 1 if monster existed at the start of the fight

BattleMonsterID = $4020		;2 bytes per monster, >255 indicates boss

MonsterReactions = $4030		;1 byte per monster, number of scripted AI reactions

.struct MonsterSlots		;4 bytes * 4 posible monster types
	ID		.res 2
	Count		.res 2		;max 8, despite the 2 bytes
.endstruct

MonstersVisible = $4048		;1 bit per monster, copied from EncounterInfo.Visible

;%CreateAttackInfoStruct(GearStats,$7E4049)	;created for real after attackinfo struct

;%CreateAttackInfoStruct(Headgear,$7E4049)	;broken out into types for better labeling, only the weapons are used tho
;%CreateAttackInfoStruct(Bodywear,$7E4055)
;%CreateAttackInfoStruct(Accessory,$7E4061)
;%CreateAttackInfoStruct(RHShield,$7E406D)
;%CreateAttackInfoStruct(LHShield,$7E4079)
;%CreateAttackInfoStruct(RHWeapon,$7E4085)
;%CreateAttackInfoStruct(LHWeapon,$7E4091)

.macro CreateEquippableStruct name, addr
.struct name				;CharEquippable, ROMItemEquippable, TempEquippable
.org addr
	Weapons 	.res 2	;4199		from CharStruct.EquipWeapons
	Armor		.res 2	;419B		from CharStruct.EquipArmor

.endstruct
name := name::Weapons
.endmacro

CreateEquippableStruct TempEquippable, $2720		;4 bytes

CreateEquippableStruct CharEquippable, $4199				;4 bytes * 4 characters

ATBReadyQueue = $41A9		;seems to be a queue of character indexes that have their atb ready
				;5 bytes
				;$FF is empty and also the queue terminator

ATBReadyCount = $41AE		;count of above queue


.macro CreateMenuDataStruct name, addr
.struct name			;MenuData and Copy
.org addr
	MenuOpen			.res 1	;41B0	41BE
	ActionFlag		.res 1	;41B1	41BF
	Command			.res 1	;41B2	41C0
	CurrentChar		.res 1	;41B3	41C1
	MonsterTargets		.res 1	;41B4	41C2
	PartyTargets		.res 1	;41B5	41C3
	SelectedItem		.res 1	;41B6	41C4	;for weapon-use effects, 0 indicates right hand, otherwise left
				.res 1	;41B7	41C5
	SecondActionFlag		.res 1	;41B8	41C6
	SecondCommand		.res 1	;41B9	41C7
     				.res 1	;41BA	41C8
	SecondMonsterTargets	.res 1	;41BB	41C9
	SecondPartyTargets	.res 1	;41BC	41CA
	SecondSelectedItem	.res 1	;41BD	41CB
.endstruct
name := name::MenuOpen
.endmacro

CreateMenuDataStruct MenuDataC1, $41B0
CreateMenuDataStruct MenuData, $41BE

.struct DisplayInfo		;used for routines which handle C1 bank graphics calls to display menus in battle
.org $41CC
	CurrentChar	.res 1		;41CC	init to FF, but holds index of character that currently has a menu open
	CD		.res 1		;41CD	likely unused
	Status1	.res 1		;41CE	;these status are processed 2 at a time using 16 bits
	Status2	.res 1		;41CF
	Status3	.res 1		;41D0
	Status4	.res 1		;41D1
	CurMP		.res 2		;41D2
.endStruct

.macro CreateSavedActionStruct name, addr
.struct name		;chunk of CharStruct relating to actions is copied here when a command is issued
.org addr			;used for mimic (and nothing else?)
	ActionFlag		.res 1		;41D4		;20h magic, 40h item
	Command		.res 1		;41D5
	MonsterTargets	.res 1		;41D6		Monster Target
	PartyTargets          .res 1		;41D7		Character Target
	SelectedItem          .res 1		;41D8		Chosen Item (for throw, etc)
	SecondActionFlag	.res 1		;41D9
	SecondCommand   	.res 1		;41DA
	SecondMonsterTargets	.res 1		;41DB
	SecondPartyTargets	.res 1		;41DC
	SecondSelectedItem	.res 1		;41DD
.endstruct
name := name::ActionFlag
.endmacro

CreateSavedActionStruct SavedAction, $41D4	;used for mimic
CreateSavedActionStruct SavedAction2, $473A	;used for reactions

MonsterMagic = $41DE		;spells available to monsters
				;1 byte * 16 slots (per monster)

.struct MMTargets	;targets of MonsterMagic above, 16 slots per monster
.org $425E
	Party		.res 1	;425E
	Monster	.res 1	;425F
.endstruct
MMTargets = MMTargets::Party

MonsterOffset16 = $435E		;monster index *16, used in monster atb/ai
MonsterOffset32 = $4360		;monster index *32, used in monster atb/ai

AIVars = $4363			;variables used by AI condition checks
				;unsure how many, possibly 4 (0-3)

CreateGFXAIQueue MonsterAIScript, $4367
				;100 bytes per monster, init to $FF
				;copied to GFXQueue and has the same structure
				;up to 5 blocks of 4 commands

;conditions are in 10 sets of 4 conditions
;condition sets are 4x 4 byte conditions followed by $FE
AIActiveConditionSet = $4687	;1 byte per monster, currently active AI condition set for each monster
				;when this changes, it has to reset progress along the script (AICurrentOffset)

AICurrentActiveCondSet = $468F	;Current AI Condition Set for this monster being processed (copied from above)

AICurrentCheckedSet = $4690	;set of conditions currently being checked

AICheckIndex = $4691		;this is which of the 4 in a set is currently being checked

AIConditionOffset = $4692	;2 bytes, pointer within MonsterAI.Conditions to current 4 byte condition being checked

AIConditionMet = $4694		;non-zero if a the last checked condition is met

AICurrentOffset = $4696

;SavedAction2 = $473A		;SavedAction struct, 10 bytes
				;used for monster reactions

;copies of 	MonsterAiScript post-action (100 bytes)
;		MonsterMagic post-action (16 bytes)
;		MMTargets post-action (32 bytes)
;saved and later restored during monster reactions
SavedMonsterAIScript = $46D6
SavedMonsterMagic = $46A6
SavedMMTargets = $46B6

;EnableTimer.ATB and CurrentTimer.ATB are saved to these then later restored
;the offset is bugged though, so it saves/restores the wrong character
;which is usually harmless
SavedEnableATB = $4744
SavedCurrentATB = $4745



.struct CurrentCommand
	ID       		.res 1	;4746	;index into command jump table (Steal, Jump, etc), post remap
	Magic        	  	.res 1	;4747
	Item	       	 	.res 1	;4748
	Element		.res 1  ;4749
	Category		.res 1	;474A	;Song/Blue/etc
	Targets		.res 1  ;474B	;one byte, either party or monster targets
	Damage		.res 1	;474C	;both bytes of damage OR'd together
.endstruct
;CurrentCommand = CurrentCommand.ID

CommandOffset = $474D			;Current Command, but adjusted to match the order of the command table
					;mostly just subtracts 1 vs the menu commands,
					;but magic commands are all mapped to $2B, and internal commands after that are shifted down accordingly

CurrentlyReacting = $474E		;indicates that a monster reaction is being processed,
					;which tends to .res things:

					;for weapons that usually use a command instead of an attack, .res it and make a normal attack
					;also skips advancing status timers
					;and checking for monster Death

;targets of the attacks in the last turn, for reactions
FinalTargetBits = $474F
TargetWasParty = $4750
FinalTarget2Bits = $4751
Target2WasParty = $4752

ReactionFlags = $4753			;low bit determines whether condition checks should use reaction1 or reaction2 fields
					;80h: set during tick end processing if a turn was proessed
					;40h: set during tick end processing if a second action happened?

TurnProcessed = $4754			;keeps track of whether a turn was processed in the current tick

ReactingIndex = $4755	;monster index or char index of the target of the last action
			;and is now reacting to it
			;either 0-7 or 0-11 depending on ReactingIndexType $7C56
			;80h is a flag

PendingReactions = $4756		;indicates PauseTimerChecks had some values set
					;zero'd after action execution finishes

.struct MonsterAI	;1620 bytes * 8 monsters
.org $4759
	Conditions		.res 170	;4759, 10*17 bytes.   4*4byte args then $FE
	Actions		.res 640	;4803, 10*64 byte entries
	ReactConditions	.res 170	;4A83, 10*17 bytes.   4*4byte args then $FE
	ReactActions          .res 640        ;4B2D, 10*64 byte entries
.endstruct
MonsterAI = MonsterAI::Conditions

NextGFXQueueSlot = $79F9		;Max 101, though there can be more queued GFXs than that
					;game works around it by just searching if so

ProcSequence = $79FA			;might be which attack out of a multi command sequence that we're on?
					;for weapon procs?
					;probably needs a better name
					;used as an index (*12) into AttackInfo array of structures
					;same kind of index as MultiCommand $7B 2C

MultiDamage = $79FB			;Used *24 when calculating the offset for displayed damage numbers
					;$79FB incremented along with multicommand at end of counter routine


.macro CreateAttackInfoStruct name, addr
.struct name			;AttackInfo
.org addr				;12-bytes, multiple structures indexed by AttackerOffset2
					;the attacking weapon data is loaded into AttackInfo
					;or 8 byte magic/Command structure into first 5 and last 3 bytes
					;same structure at other locations is also used for all equipment data
	Targetting  .res 1	;0	;79FC	4049	4085	4091
							;80 Multi-target optional
							;40 Hits all targets
							;20 Target selectable
							;10 Side selectable
							;08 Target enemy (by default)
							;04 Roulette
							;02 Delay+ (together these bits hold delay/10)
							;01 Delay

	Category	.res 1	;1	;79FD	404A	4086	4092
							;80 Physical
							;40 Aerial (weapons)
							;20 Song
							;10 Summon
							;08 Dimen
							;04 Black
							;02 White
							;01 Blue
					

	;EquipmentType		;2	;79FE	404B	4087	4093
						;For Weapons
							;40 NOT throwable
							;20-01 Table of "sets" of classes that can use it
	;Misc					;For Spells
							;80 Monster bit (monster only?)
							;40 Learnable 100%
							;20 Learnable 50% (unused?)
							;10 Learnable 10% (unused?)
							;08
							;04-01 Number of hits (minus 1?) (Meteo)
						;For Items
							;08 Does not consume when used
	CmdStatus		.res 1	;For Abilities
							;80h Defending
							;40h Guarding
							;10h Jumping
							;08h Throbbing (from Flirt)
					

	;ElementOrStatsUp	;3	;79FF	404C	4088	4094
						;For Weapons
							;80 Stats up ->
							;40 Wind         Strength
							;20 Earth        Speed
							;10 Holy         Vitality
							;08 Poison       Magic power
							;04 Lightning    \
							;02 Ice           Stat bonuses
							;01 Fire         /
									;Stat bonuses  **cleanup: get actual rom data here
										;0 +1
										;1 +2
										;2 +3
										;3 +1/-1/+1/-1 (not used)
										;4 -1 (use inverted stat selection) (not used)
										;5 +5/-5/+5/-5 (Giant Gloves)
										;6 -5 (use inverted stat selection)
										;7 +5
	;MPCost				;For Magic
							;80 Not reflectable
							;40-01 MP cost
	DamageMod		.res 1	;For Abilities
							;80h: Auto hit
							;40h: Damage = Damage * 2
							;20h: Damage = Damage / 2
							;10h: M = M * 2
							; 8h: M = M / 2
							; 4h: Defense = 0
							; 2h: SwdSlap?
							; 1h: Damage * 2 vs Humans (80h Creature Type)
					


	;DoubleGrip 		;4 	;7A00	404D	4089	4095
	;Description			;Double Grip + Description
							;80 Double Grip only
							;40 Double Grip allowed
							;20-01 Table of which "equip notes" description to display, such as "Magic Sword OK"
	MagicAtkType	.res 1		;For some reason (whyyy) they put attack type here for magic/items, rather than the same byte weapons use
					
	;Entries 5-8 are weapon only
	Properties		.res 1;5	;7A01	404E	408A	4096
						;Special properties
							;80h: Sword parry (Hardened, Defender)    ;80h Magic block (Aegis shield)
							;40h: Knife parry (Guardian)              ;40h Elf Cape dodge
							;20h: Initiative                          ;20h Improved Brawl
							;10h                                      ;10h Improved Steal
							;08h: Magic on hit                        ;08h Half MP cost
							;04h: Magic Sword OK                      ;04h Sword Dance / Flirt up
							;02h: Command instead of attack           ;02h Become undead
							;01h: Wonder Rod                          ;01h Improved Catch
					

	;ItemMagic		;6	;7A02	404F	408B	4097
	ShieldEvade		.res 1	;Byte also reused for shield evade values
					;Item magic:
							;80: Breaks after Use
							;40-01 Magic cast
					

	;AtkPower		;7 	;7A03	4050	408C	4098
	Defense			.res 1;Attack power or Defense, depending if weapon/magic or armor
					

	;AtkType		;8	;7A04	4051	408D	4099
	MEvade		.res 1;Magic Evade for armor
					;Flag + Attack formula for weapons
							;80 Don't Retarget (can hit inactive/dead)
							;40-01 [Attack formula]  Attack formula
					

	;Param1		;9	;7A05	4052	408E	409A	;Parameter 1
	MDefense			.res 1	 			;or MDefense for armor

	;Param2		;A	;7A06	4053	408F	409B	;Parameter 2  	(For most atk types this is Proc chance)
	ElementDef			.res 1	 			;or ROMElementDef index for armor

	;Param3		;B	;7A07	4054	4090	409C	;Parameter 3  	(For most atk types this is Proc Command/Magic)
									;		(When command, this is an offset into $C24A94 command table)
	Status			.res 1				;or ROMArmorStatus index for armor
.endstruct
name::EquipmentType := name::CmdStatus
name::Misc := name::CmdStatus
name::ElementOrStatsUp := name::DamageMod
name::MPCost := name::DamageMod
name::DoubleGrip := name::MagicAtkType
name::Description := name::MagicAtkType
name::ItemMagic := name::ShieldEvade
name::AtkPower := name::Defense
name::AtkType := name::MEvade
name::Param1 := name::MDefense
name::Param2 := name::ElementDef
name::Param3 := name::Status
name := name::Targetting		;makes a define so we don't have to keep specifying a property when accessing the structure as a whole
.endmacro

CreateAttackInfoStruct GearStats, $4049	;table with gear stats, 7*12 byte AttackInfo structs, then *4 for party members, total size 84 bytes per character
CreateAttackInfoStruct Headgear, $4049	;broken out into types for better labeling
CreateAttackInfoStruct Bodywear, $4055
CreateAttackInfoStruct Accessory, $4061
CreateAttackInfoStruct RHShield, $406D
CreateAttackInfoStruct LHShield, $4049
CreateAttackInfoStruct RHWeapon, $4085
CreateAttackInfoStruct LHWeapon, $4091
CreateAttackInfoStruct AttackInfo, $79FC	;16 of these, 12*16 = 192 bytes

TargetAdjust = $7ABC			;causes some kind of adjustment to target index
					;believe it's used to dispatch individual attacks from a multitarget ability
					;but the code is hard to follow

TargetType = $7ACC			;set to $10 for a weapon proc
					;0 for single target item
					;80h for multi target item or spell
					;uses ProcSequence/Multicommand table index


CommandTargetBitmask = $7ADC		;16-bit target bitmask just like the one below
					;uses ProcSequence/Multicommand table index





TargetBitmask = $7AFC			;16-bit target bitmask
					;CCCCMMMM in first byte
					;MMMM.... in second byte
					;C=Characters, M=Monsters, .=unused?
					;uses ProcSequence/Multicommand table index
					;for spells only? not always set

;$7B0E					;address of special counter data in D0 Bank

MultiTarget = $7B1C			;number of targets, except single target = 0
					;uses ProcSequence/Multicommand table index


MultiCommand = $7B2C			;Name here is a guess but seems to fit
					;offset into combat tables such as target bitmask
					;seems like it could be valid 0-15,
					; but there's at least one place where I'm not sure what will happen if it's more than 3
					;seems like it is which attack we're currently processing for any double attack type effects?

AtkType = $7B2D			;table of attack types indexed by MultiCommand above
					;High bit is used as a flag that clears some data?
					;$FF is also a special case that points to the next MultiCommand

Skip2ndReactionCheck = $7B2E		;checked for FF to .res setting up the second reaction check
					;but never written to that I can see

FightFlag = $7B40			;set to 1 during damage application when command is:
					;Fight,Capture,Jump,Aim,X-Fight
					;or any type of magic when selected spell is monster attack/specialty
					;seems to be basic fight-style actions (but not everything that's considered physical)

TargetBitMaskSmall = $7B41		;single byte, bits indicate target (can be party or monsters)

ReflectorBitmask = $7B42		;single byte, bits indicate who is reflecting a spell (can be party or monsters)
ReflecteeBitmask = $7B43		;single byte, bits indicate who getting hit by a reflected spell (can be party or monsters)

CoveredBitmask = $7B44			;bit indicating who is covered for an attack

AnotherTargetIndex = $7B45		;index indicating who is covering for an attack (strangely, not a bitmask)
					;isn't just used for cover, not sure exactly what it's for yet

Reflected = $7B46			;either a flag indicating reflect occuring or a count of reflected spells

CounterReflecteeTable = $7B49		;8 bytes * Multicommand
					;doesn't seem to ever be used anywhere after being set up

BaseDamage = $7B69			;Damage after defense and M

DamageToAttacker = $7B6B		;Final Damage to Attacker
DamageToTarget = $7B6D			;Target Final Damage
HealingToAttacker = $7B6F		;Final Healing to Attacker
HealingToTarget = $7B71			;Target Final Healing

HealingToAttackerMP = $7B73
HealingToTargetMP = $7B75
DamageToAttackerMP = $7B77
DamageToTargetMP = $7B79

CurrentChar = $7B7B			;used in some routines to keep track of the current character being processed

GearChanged = $7B7D			;>0 means equipment was changed in the battle item menu

.macro CreateCombinedStatus name, addr
.struct name		;4 bytes * 16 participant slots, combines StatusX and AlwaysStatusX
.org addr			;4 party, 4 unused?, 8 monsters
	S1		.res 1	;7B7E 	;7B9E
	S2		.res 1	;7B7F	;7B9F
	S3		.res 1	;7B80	;7BA0
	S4		.res 1	;7B81	;7BA1
.endstruct
.endmacro

; Address of first party combined status
CreateCombinedStatus CombinedStatus, $7B7E

; Address of first monster combined status
CreateCombinedStatus MonsterCombinedStatus, $7B9E

BattleOver = $7BDE			;Flag for ending battle
					;80h: Enemies dead
					;40h: Party dead
					;20h: Timer Ended or Terminate action (Attack 68)
					;01h: Escaped

;Stats after all bonuses
Strength = $7BE1
Agility = $7BE2
Vitality = $7BE3
MagicPower = $7BE4
Level = $7BE5

Void = $7BE6				;40h void battle state (no magic)
					;seems to be set up to allow other states in other bits,
					;but those aren't used as far as I can tell

FleeSuccess = $7BE8			;80h on Exit cast
					;0 seems to reset FleeTicker for normal running, so might be another value while holding L+R?

HitsInactive = $7BEB			;causes attacks to still be able to hit most inactive targets (dead, hiding, etc)
					;still fails if erased, jumping or $7C9D is set
					;indexed by Multicommand


UnknownReaction = $7BFB		;bunch of commands set this to 1, unsure what it does
					;set to 1 for Attack Types
					;00 (and is the only thing 00 does)
					;60 Hide/Show monster
					;62 library book monster swap
					;67 "Pull", does only this
					;68 Terminate Battle
					;6A Win Battle
					;commands themselves also set it though: row, escape, defend/guard, jump, slash,  "other"
					;seems to be related to triggering counter attacks (or other events?)
					;causes actionanim flag 20h to be set, which is likely animation related
					;..nothing reads that flag in battle logic code

DelayedFight = $7BFC			;set when Build Up or Jump commands execute
SwordSlap = $7BFD			;set when Sword Slap command executes, then is never checked again

;$7C03 is used in various places for different things
SearchGFXQueue = $7C03		;indicates that the routine that finds the next open GFX Queue slot should
				; manually search instead of using NextGFXQueueSlot

MonsterIndex = $7C03		;for monster atb routine and subsequent AI script routines


UncontrolledATB = $7C04		;Party member ATB when Zombie/Berserk/Charm
					;used instead of CurrentTimer.ATB during those status
					;caps at 127
					;4 of these for party members

MPTaken = $7C08		;Flag set when MP is taken for a spell
				;I think this is so Meteo doesn't subtract cost for the repeated casts?
				;

MonsterKillTracker = $7C09	;bitfield of monsters used to track kills
				;at battle start, bits are set for visible monsters and empty slots (but not hidden monsters)
				;bits are cleared when monsters die, but all empty slots are kept set (from below var)
				;during battle end processing, this in inverted then bits are counted to determine number of kills

InactiveMonsters = $7C0A	;bitfield, bits set for slots that weren't active/visible at battle start

VictoryGil = $7C0B		;3 byte total of Gil from monster kills (only tallied at end of battle)
VictoryExp = $7C0E		;3 byte total of Exp from monster kills (only tallied at end of battle)

OriginalMaxHP = $7C11				;saved before Giant Drink doubles MHP
						;8 bytes, 2 per char

BackAttack = $7C19

EarthWallHP = $7C1E

SavedCharRow = $7C1A		;1 byte * 4 characters, same format as CharStruct.CharRow

BlueLearnedCount = $7C20	;number of blue spells learned this fight, max 8
BlueLearned = $7C21		;ids of blue spells learned, 8 bytes

NoValidTargets = $7C29		;set to 1 when retargetting fails to find a valid new target

.struct ForcedTarget 	;2 bytes * 8 entries in table, ends at
.org $7C2A
	Party			.res 1		;$7C2A
	Monster		.res 1		;$7C2B
.endstruct

ControlTarget = $7C3A			;holds target index for control, 4 bytes to handle party
ControlCommand = $7C3E			;likely chosen command for control (unconfirmed), 4 bytes to handle party
					;set to 0 upon successful control

ControllingA = $7C42			;related to control? 1 when controlling

MonsterControlActions = $7C43		;7C43-7C4A
					;8 bytes for monsters, Control Action from CharControl.Actions

ReleasedMonsterID = $7C4B		;monster ID when you release a monster?

SandwormBattle = $7C4C			;changes what attack type 61 does and some other adjustments
					;seems to be used for the sandworm fight

ControllingB = $7C4D			;related to control? 1 when controlling, unsure how it differs from A, they're set at the same time

FleeTickerActive = $7C4E		;FleeTicker doesn't advance unless this is set
					;set to 1 on first character atb

WasMonsterReleased = $7C4F

AISkipDeadCheck = $7C50			;set to 1 at start of monster turn
					;also set to 1 if passing a condition:dead check
					;set to 0 in $3C10 routine

QuickTurns = $7C51			;Set to 3 when Quick cast

WaitModePause = $7C52			;1 when battle is paused due to wait mode with menu open

BattleTickerA = $7C53			;loops through 0-3 every run of the main battle loop
BattleTickerB = $7C54			;1 when BattleTickerA is 0, 0 otherwise (so 25% of the time)
					;may rename these once I figure out what they're used for (hp leak, but anything else?)

RNGPointer = $7C55			;0 or 1 to determine which RNG to use

ReactingIndexType = $7C56		;1 for monster target in ReactingIndex $4755 (0-7)
					;>1 for character target in ReactingIndex $4755 (0-11)


GiantDrink = $7C59			;>0 means hp has already been doubled
					;4 bytes here, 1 for each char

SpiritFlag = $7C5D			;0 if inflicting Zombie, 1 if not
					;set by Attack 4F (Spirit)
					;think this is used to show a graphical effect later

FleeTicker = $7C5F			;counts up toward 20 (when holding L+R, presumably?)
					;if it makes it there a flee attempt roll is made

SandwormHitIndex = $7C60		;slot index where worm was hit
SandwormHitFlag = $7C61			;1 when sandworm was hit

SelectedItem = $7C62			;Item selected when weapon is used (from item menu)

ThrownItem = $7C63			;from CharStruct.SelectedItem

CurrentHP = $7C64			;Attacker's Current HP, capped at 9999.   Used by White Wind

BattleItemsWon = $7C66 			;8 bytes

MonsterNextForm = $7C72			;Monster's next form when switching with Attack Type 71
					;used to load new stats

ActedIndex = $7C73			;index of character that acted this battle tick
					;checked in targetting routine 2D
					;holds a target index

;%CreateBattleDataStruct(BattleData,$7E7C74)
;.Escapes = $7C75			;Number of times escaped.  Used by Chicken Knife and Brave Blade
;.WonderRod = $7C76			;number of times Wonder Rod has been used
					;jumps from 0 to 2, resets to 0 when it hits 36

;copies from FieldTimer $0AFB
BattleTimerEnable = $7C94		;$02 means end battle when timer is up
BattleTimer = $7C95			;frame count, 16 bit


MagicNull = $7C97			;Set when magic attacks are nullified (element immunity)
					;generally used to properly set AtkMissed

;flags that make the CastSpell hit the target even if they are inactive, if they have this status
SpellCheckStone = $7C98
SpellCheckDeath = $7C99

StealNoItems = $7C9A			;Set when there are no items left to steal

ActionAnimShift = $7C9B		;messes with animations, might be a better name but don't understand it yet
				;used by double lance (Command $55): 80h for RH, 40h for LH
					;80h: ActionAnims[3] and 4 more structures moved down to [2]
					;otherwise, if any bits were set,
					;mess with target bits of ActionAnim[0], then move ActionAnims[2] and 4 more structures down to [1]
					;then, if 40h, mess with target bits of the new ActionAnim[1]

HitsJumping = $7C9C			;nonzero means ok to hit jumping target (inteceptor rocket)

MissInactive = $7C9D			;causes attacks that would otherwise hit inactive targets to not do so

MonsterEscaped = $7CAE			;Set when monster escapes (8 bytes, 1 for each monster)

QuickCharIndex = $7CB6			;Index (0-12) of character that is currently Quick

; $7CB7-7CBE used by FF5's Division routine
;	Divides $7CB7 by $7CB9: result in $7CBB, remainder in $7CBD, all 16-bit
Dividend = $7CB7
Divisor = $7CB9
Quotient = $7CBB
Remainder = $7CBD

CheckQuick = $7CC7			;0 causes ATB update to .res the check for Quick status

ResetBattle = $7CD8			;Set when Reset is used

BattleFrameCount = $DB6E		;4 bytes, time elapsed in battle (in frames)

wMusicChanged = $DBB3			;1 while music change routine is being called

;Rom Structures
ROMRNG = $C0FEC0

CreateMonsterStatsStruct ROMMonsterStats, $D00000	;32 bytes * 256 regular monsters
;ROMBossStats = ROMMonsterStats[$2000]			;then another 128 boss monsters

CreateEncounterInfoStruct ROMEncounterInfo, $D03000

.struct ROMLoot		;4 bytes, 384 entries, ends at
.org $D05000
	RareSteal		.res 1
	CommonSteal		.res 1
	RareDrop		.res 1
	AlwaysDrop		.res 1
.endstruct

ROMControlActions = $D05600 		;monster actions for control, 4 bytes * 384 monsters

ROMMonsterReleaseActions = $D08600	;monster actions for release

ROMMonsterCoordinates = $D08900

.struct ROMSpecialtyData		;monster specialty attacks, 2 bytes * 384 monsters
	Properties		.res 1
	Name			.res 1
.endstruct

ROMAIScriptOffsets = $D09C00

RomAbilityBitInfo = $D0EC00		;2 bytes per ability
					;first byte is byte offset in FieldAbilityList
					;second is which bit corresponds with the ability

;rom data tables for multiplication and other calculation saving lookups
ROMBitUnset = $D0ECDE			;0111 1111 bit selection table, 0 moves toward low bits
ROMBitSet = $D0ECE6			;1000 0000 bit selection table, 1 moves toward low bits
ROMTimes10 = $D0ECEE			;*10 valid for 0-3
ROMBattleSpeedTable = $D0ECF2		;0 then 15 then doubles until 240 then repeats
					;2nd copy is for active/wait

ROMCommandMap = $D0ED02		;converts commands into Command table offsets
					;0 then counts from 0 to 42, then has 43 repeated 34 times, then counts to 52
					;the repeated item is for Magic commands, they're all Command $2B

ROMTimes650w = $D0ED59			;*650 valid for 0-3 (16 bit)
ROMTimes11w = $D0ED61			;*11 valid for 0-11 (16 bit)
ROMCombatantReorder = $D0ED79		;8 9 10 11 0 1 2 3 4 5 6 7 to rearrange party members after monsters instead of before
ROMTimes84 = $D0ED85			;*84 valid for 0-3
ROMTimes5w = $D0ED89			;*5 valid for 0-101 (16 bit) (yes, the table is that big)
ROMTimes12 = $D0EE55			;*12 valid for 0-15
ROMTimes24w = $D0EE65			;*24 valid for 0-15 (16 bit)
ROMTimes7 = $D0EE85			;*7 valid for 0-15
ROMTimes100w = $D0EE95			;*100 valid for 0-7 (16 bit)
ROMTimes1620w = $D0EEA5			;*1620 valid for 0-7 (16 bit)
ROMTimes64w = $D0EEB5			;*64 valid for 0-9 (16 bit)
ROMTimes17 = $D0EEC9			;*17 valid for 0-9
ROMTimes20 = $D0EEDB			;*20 valid for 0-3


ROMAbilityListPointers = $D0EED3	;pointers to character ability inventories (in ram)
;Points to FieldAbilityList:
;Butz 		$08F7
;Lenna 		$090B
;Galuf/Krile1F 	$091F
;Faris		$0933

ROMMusicTable = $D0EEDF			;maps the numbers from EncounterInfo.Music to values that the Music Change routine in C4 bank wants

ROMMagicLamp = $D0EEE7			;list of spells to use from magic lamp casts


ROMHideMessages = $D0EEF6		;bitfield, bits are set for messages that are hidden
					;when there's no target data for the attack animation
					;$00 00 00 07 A4 00 00 00 00 1B 80 00 00 00 00 00

ROMJobMagicLevels = $D0EF06	;used to map magic commands to their magic level to display a job level up
				;(such as white or black level 1-6)

ROMFightCommands = $D0EF26	;has 1 for commands which are standard "fight" type commands, by some criteria?

ROMToadOK = $D0EF58		;1 bit per spell, determines whether magic can be used as a toad

;$D0EF78 is code 	CleanupFieldItems_D0	;set quantities to 0 for id 0 items, set ids to 0 for qty 0 items

.struct ROMMonsterFormData	;16 bytes * 4 entries
					;there's only one actual entry in the vanilla rom, the rest are blank
					;even the one with data there is likely usused
	Encounter		.res 2
	FormData		.res 14
.endstruct

.struct ROMOneTime	;4 bytes * 8 entries for one-time encounters
	Encounter		.res 2
	Replacement		.res 2
.endstruct

CreateAttackInfoStruct ROMItems, $D10000		;12 bytes, 256 entries, ends at $D10BFF
							;identical to AttackInfo .struct at, see that for details
ROMWeapons := ROMItems
ROMArmor := ROMItems + $600	;$D10600
;consumable items use the 8 byte magic/ability structure instead

.macro CreateMagicInfoStruct name, addr
.Struct name		;8 bytes
.org addr			;same format as AttackInfo Struct's first 4 and last 4 elements
				;see that for details
				;awkwardly, they copy .AtkType to the 5th position in attackinfo, instead of 9th to match weapons, so it's more like a 5/3 split
	Targetting		.res 1		;0
	Category		.res 1		;1
	Misc			.res 1		;2 Also used as CmdStatus
	MPCost 			.res 1		;3 Also used for DamageMod
	AtkType			.res 1		;4	
	Param1			.res 1	 	;5
	Param2			.res 1		;6
	Param3			.res 1		;7
.endstruct
name::CmdStatus := name::Misc
name::DamageMod := name::MPCost
name := name::Targetting	;makes a define so we don't have to keep specifying a property when accessing the structure as a whole
.endmacro


CreateMagicInfoStruct TempMagicInfo, $262A
CreateMagicInfoStruct ROMConsumables, $D10A80		;8 bytes * 32 consumables
CreateMagicInfoStruct ROMMagicInfo, $D10B80		;8 bytes * 256 spells

CreateEquippableStruct ROMItemEquippable, $D12480	;4 bytes * 64 entries

.struct ROMElementDef	
	Absorb 	.res 1
	Evade		.res 1
	Immune	.res 1
	Half		.res 1
	Weak		.res 1
.endstruct
;ROMElementDef = ROMElementDef.0

.struct ROMArmorStatus
	Status0	.res 1		;high bit here indicates always status, for all other status below as well
	Status2       	.res 1		;only the last item per type with an always status will apply
	Status3       	.res 1		
	Status4       	.res 1		
	Immune1        .res 1
	Immune2        .res 1
	Immune3        .res 1
.endstruct

ROMStatBonuses = $D12880	;16 bytes, 8 stat mod pairs

CreateTimerStruct ROMGlobalTimer, $D12976	;11 bytes

ROMMagicAnim = $D12981		;16 byte table of bitfields mapping to spells $80+
				;1 indicates a given spell uses animation type 7 (magic)

ROMBattleMessageOffsets = $D13840

ROMLevelExp = $D15000		;3 bytes * 99 entries
ROMLevelHP = $D15129		;2 bytes * 99
ROMLevelMP = $D151EF		;2 bytes * 99

ROMJobPointers = $D152C0	;2 bytes * 21, pointer to first ability for each job (no freelancer)
ROMJobLevels = $D152EA		;1 byte * 22, number of levels for each job (also ability count)
ROMJobAbilities = $D15300	;3 bytes, 2 byte ap cost followed by 1 byte id, accessed via ROMJobPointers

CreateMagicInfoStruct ROMAbilityInfo, $D159E0		;8 bytes * 96 abilities

ROMStatusDisableCommands = $D15CE0	;16 bit entries, index is command ID
					;holds Status1 and Status2 that should disable that command

ROMCommandDelay = $D15DA0	;1 byte per command, atb delay when used
				;80h set -> use weapon or spell delay instead

CreateMagicInfoStruct ROMEffectInfo, $D16AB1	;Spell effects for Animals, Mix, etc. 
						;8 bytes * 105? abilities
ROMTerrainSpells = $D16DF9	;4 bytes per terrain type
							
ROMCombineSpells = $D16EF9	;144 bytes?
				;Effect spell ids for combine/mix
				;12*item2 + item1
