.org $C10000
Dispatch_C1:
;calls a routine in C1 bank
;index of routine to run is passed in A

; ---------------------------------------------------------------------------

.org $C40004
MusicChange_C4:
;passes music byte at $1D00

; ---------------------------------------------------------------------------

.org $D0EF78
;code in the D0 data bank, cleans up battle inventory
;sets items with id 0 to qty 0, and vice versa
;likely a last minute bug fix
;commenting out code because I want to keep this file to just labels (for now?)
CleanupFieldItems_D0:
;	TDC				;7B 
;	TAX				;AA 
;-	LDA FieldItems,X 		;BD 40 06  (field items)
;	BNE +				;D0 03
;	STZ FieldItemsQty,X 		;9E 40 07  (field item qty)
;+	LDA FieldItemsQty,X		;BD 40 07 
;	BNE +				;D0 03
;	STZ FieldItems,X 		;9E 40 06 
;+	INX				;E8 
;	CPX #$0100			;E0 00 01  (256 items)
;	BNE -				;D0 EA 
;	RTL				;6B


