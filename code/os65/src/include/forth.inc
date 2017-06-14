
		.extern	_DO_LITERAL	; (LITERAL)
		.extern	_DO_USER	; (USER)
		
		.extern	_FETCH		; @
		.extern	_C_FETCH	; C@
		.extern	_DECIMAL	; DECIMAL
		.extern	_DUP		; DUP
		.extern	_DROP		; DROP
		.extern	_HEX		; HEX


DSTK		.equ	$0000
RSTK		.equ	$0100

;===============================================================================
;-------------------------------------------------------------------------------

DO_LITERAL	.macro	VALUE
		lda	#<VALUE
		ldy	#>VALUE
		jsr	_DO_LITERAL
		.endm
		
;===============================================================================
;-------------------------------------------------------------------------------

FETCH		.macro
		jsr	_FETCH
		.endm

C_FETCH		.macro
		jsr	_C_FETCH
		.endm

;===============================================================================
;-------------------------------------------------------------------------------

DECIMAL		.macro
		jsr	_DECIMAL
		.endm
		
HEX		.macro
		jsr	_HEX
		.endm

;===============================================================================
;-------------------------------------------------------------------------------

DROP		.macro
		inx
		inx
		.endm

DUP		.macro
		jsr	_DUP
		.endm
		
;===============================================================================
;-------------------------------------------------------------------------------

TO_R		.macro
		lda	DSTK+2,x
		pha
		lda	DSTK+1,x
		pha
		inx
		inx
		.endm
		
R_FROM		.macro
		dex
		dex
		pla
		sta	DSTK+1,x
		pla
		sta	DTSK+2,x
		.endm
		
		