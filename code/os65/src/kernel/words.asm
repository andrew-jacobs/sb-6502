
		.include "../include/forth.inc"
		
		.code

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
; Memory Access
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
; Number Bases
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
; Stack Operations
;-------------------------------------------------------------------------------

		.global	_QUERY_DUP
_QUERY_DUP:
		lda	DSTK+1,x	; Is the top value non-zero?
		ora	DSTK+2,x
		bne	_DUP		; Yes, duplicate it
		rts			; No, done


		.global	_TWO_DROP
_TWO_DROP:
		inx			; 
		inx
		inx
		inx
		rts			; Done


		.global	_DUP
_DUP:
		dex			; Make room on the stack
		dex
		lda	DSTK+3,x	; Then copy the top value
		sta	DSTK+1,x
		lda	DSTK+4,x
		sta	DSTK+2,x
		rts			; Done

;===============================================================================
; Return Stack Operations
;-------------------------------------------------------------------------------


;===============================================================================
; Single Precision Arithemtic
;-------------------------------------------------------------------------------

		.global	_PLUS
_PLUS:
		clc
		lda	DSTK+1,x	; Add the top two values
		adc	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		adc	DSTK+4,x
		sta	DSTK+4,x
		inx			; Drop the old top
		inx
		rts			; Done

		.global	_MINUS
_MINUS:
		sec
		lda	DSTK+1,x	; Subtract the top two values
		sbc	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		sbc	DSTK+4,x
		sta	DSTK+4,x
		inx			; Drop the old top
		inx
		rts			; Done

		.global	_ONE_PLUS
_ONE_PLUS:
		inc	DSTK+1,x	; Increment the top value
		if eq
		 inc 	DSTK+2,x
		endif
		rts			; Done
	
		.global	_ONE_MINUS
_ONE_MINUS:
		lda	DSTK+1,x
		if eq
		 dec	DSTK+2,x
		endif
		dec	DSTK+1,x
		rts

		.global	_TWO_STAR
_TWO_STAR:
		asl	DSTK+1,x
		rol	DSTK+2,x
		rts			; Done

		
		.global	_TWO_SLASH
_TWO_SLASH:
		lda	DSTK+2,x	; Copy sign bit into carry
		asl	a
		ror	DSTK+2,x	; Then shift all bits right
		ror	DSTK+1,x
		rts
		
		
;===============================================================================
; Logical Operations
;-------------------------------------------------------------------------------

		.global	_AND
_AND:
		lda	DSTK+1,x	; AND top value with next on stack
		and	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		and	DSTK+4,x
		sta	DSTK+4,x
		inx			; Drop the old top value
		inx
		rts			; Done

		.global	_INVERT
_INVERT:
		lda	#$ff		; Invert the top value
		eor	DSTK+1,x
		sta	DSTK+1,x
		lda	#$ff
		eor	DSTK+2,x
		sta	DSTK+2,x
		rts			; Done
		
		.global	_OR
_OR:
		lda	DSTK+1,x	; AND top value with next on stack
		and	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		and	DSTK+4,x
		sta	DSTK+4,x
		inx			; Drop the old top value
		inx
		rts			; Done

		
		.global	_XOR
_XOR:
		lda	DSTK+1,x	; XOR top value with next on stack
		eor	DSTK+3,x
		sta	DSTK+3,x
		lda	DSTK+2,x
		eor	DSTK+4,x
		sta	DSTK+4,x
		inx			; Drop the old top value
		inx
		rts			; Done

;===============================================================================
; Comparison Operators
;-------------------------------------------------------------------------------

		.global	_ZERO_EQUAL
_ZERO_EQUAL:
		ldy	#0		; Assume result is false
		lda	DSTK+1,x	; Test the top of stack value
		ora	DSTK+2,x
		if eq
		 dey			; If all bits clear make result true
		endif
		sty	DSTK+1,x	; Save the result flag
		sty	DSTK+2,x
		rts			; Done
		
;===============================================================================
; I/O
;-------------------------------------------------------------------------------

		.global	_DO_QUOTE
_DO_QUOTE:
		R_FROM
		ONE_PLUS
		lda	(DSTK+1,x)
		tay
		while ne
		 ONE_PLUS
		 C_FETCH
		 JSR	_EMIT
		 dey
		endw
		TO_R
		rts
		
		


		.global	_EMIT
_EMIT:
		inx
		inx
		rts
	
		.global	_KEY
_KEY:
		dex
		dex
		rts

		.end