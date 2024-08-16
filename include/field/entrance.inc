.list off
.ifndef ENTRANCE_INC
ENTRANCE_INC = 1

.global Entrance, EntrancePtrs

.scope Entrance

        ITEM_SIZE = 6
        Start := EntrancePtrs

        SrcPos := Start
        SrcX := Start
        SrcY := Start + 1
        Map := Start + 2
        Flags := Start + 3
        DestPos := Start + 4
        DestX := Start + 4
        Dir := Start + 4
        DestY := Start + 5

; ##############################################################################
; ###          AUTOMATICALLY GENERATED CODE, DO NOT MODIFY MANUALLY          ###
; ##############################################################################


; ##############################################################################

.endscope

.endif
.list on
