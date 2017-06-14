
		.code

DSTK		.equ	$0000
RSTK		.equ	$0100

USER_AREA	.equ	$00F8

BASE_OFFSET	.equ	0

;===============================================================================
;-------------------------------------------------------------------------------

		.global	_DO_LITERAL
_DO_LITERAL:
		dex
		dex
		sta	1,x
		sty	2,x
		rts
		
		.global	_TRUE
_TRUE:
		lda	#$ff
		tay
		bne	_DO_LITERAL
	
		.global	_FALSE
_FALSE:
		lda	#$00
		tay
		beq	_DO_LITERAL

;===============================================================================
; User Variables
;-------------------------------------------------------------------------------

_DO_USER:

		.global	_BASE
_BASE:
		lda	#<(USER_AREA + BASE_OFFSET)
		ldy	#>(USER_AREA + BASE_OFFSET)
		jmp	_DO_LITERAL

;===============================================================================
;-------------------------------------------------------------------------------

		.global	_FETCH
_FETCH:
		lda	(DSTK+1,x)
		tay
		inc	DSTK+1,x
		if eq
		 inc	DSTK+2,x
		endif
		lda	(DSTK+1,x)
		sta	DSTK+2,x
		sty	DSTK+1,x
		rts
		
		.global	_STORE
_STORE:
		rts

		.global	_C_FETCH
_C_FETCH:
		lda	(DSTK+1,x)
		sta	DSTK+1,x
		.if	__65C02__
		stz	DSTK+2,x
		.else
		lda	#0
		sta	DSTK+2,x
		.endif
		
		rts

;===============================================================================
;-------------------------------------------------------------------------------

		.global	_DECIMAL
_DECIMAL:	
		lda	#<10
		ldy	#>10
		jsr	_DO_LITERAL
		jsr	_BASE
		jmp	_STORE

		.global	_HEX
_HEX:	
		lda	#<16
		ldy	#>16
		jsr	_DO_LITERAL
		jsr	_BASE
		jmp	_STORE

;===============================================================================
;-------------------------------------------------------------------------------

		.global	_TWO_DROP
_TWO_DROP:
		inx
		inx
		inx
		inx
		rts


		.global	_DUP
_DUP:
		dex
		dex
		lda	DSTK+3,x
		sta	DSTK+1,x
		lda	DSTK+4,x
		sta	DSTK+2,x
		rts

;===============================================================================
; Return Stack Operations
;-------------------------------------------------------------------------------



;===============================================================================
;-------------------------------------------------------------------------------

		.global	_AND
_AND:
		lda	DSTK+1,x
		and	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		and	DSTK+4,x
		sta	DSTK+4,x
		inx
		inx
		rts

		.global	_INVERT
_INVERT:
		lda	#$ff
		eor	DSTK+1,x
		sta	DSTK+1,x
		lda	#$ff
		eor	DSTK+2,x
		sta	DSTK+2,x
		rts
		
		.global	_OR
_OR:
		lda	DSTK+1,x
		and	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		and	DSTK+4,x
		sta	DSTK+4,x
		inx
		inx
		rts

		
		.global	_XOR
_XOR:
		lda	DSTK+1,x
		eor	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		eor	DSTK+4,x
		sta	DSTK+4,x
		inx
		inx
		rts

		

		.end