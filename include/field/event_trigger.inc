.list off
.ifndef EVENT_TRIGGER_INC
EVENT_TRIGGER_INC = 1

.global EventTrigger, EventTriggerPtrs

.scope EventTrigger

        ITEM_SIZE = 4
        Start := EventTriggerPtrs

        SrcPos := Start
        SrcX := Start
        SrcY := Start + 1
        Event := Start + 2

; ##############################################################################
; ###          AUTOMATICALLY GENERATED CODE, DO NOT MODIFY MANUALLY          ###
; ##############################################################################


; ##############################################################################

.endscope

.endif
.list on
