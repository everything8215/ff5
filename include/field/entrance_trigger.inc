.list off
.ifndef ENTRANCE_TRIGGER_INC
ENTRANCE_TRIGGER_INC = 1

.global EntranceTrigger, EntranceTriggerPtrs

.scope EntranceTrigger

        ITEM_SIZE = 6
        Start := EntranceTriggerPtrs

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
