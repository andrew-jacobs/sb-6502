;

;

		.6502

		.include "../sb-6502.inc"

;===============================================================================
; ASCII Control Characters
;-------------------------------------------------------------------------------

NUL		.equ	$00
BEL		.equ	$07
BS		.equ	$08
HT		.equ	$09
CR		.equ	$0d
LF		.equ	$0a
DC1		.equ	$11			; XON
DC3		.equ	$13			; XOFF
ESC		.equ	$1b
DEL		.equ	$7f

;===============================================================================
;-------------------------------------------------------------------------------

RX_SIZE		.equ	64
TX_SIZE		.equ	64

DSTACK		.equ	$0000
RSTACK		.equ	$0100


;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.page0
		.org	$00d0

RSP		.space	1

;-------------------------------------------------------------------------------

		.org	$00f0

; Communications buffer offsets

RX_HEAD		.space	1		; UART receive buffer offsets
RX_TAIL		.space	1
TX_HEAD		.space	1		; UART transmit buffer offsets
TX_TAIL		.space	1


;-------------------------------------------------------------------------------

		.org	$00ff

IO_TEMP		.space	1		;

;-------------------------------------------------------------------------------


;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.bss

		.org	$e000


;-------------------------------------------------------------------------------
; UART Buffers

		.org	$ef00

RX_BUFF		.space	RX_SIZE		; UART receive buffer
TX_BUFF		.space	TX_SIZE		; UART transmit buffer

;===============================================================================
; Macros
;-------------------------------------------------------------------------------

; The LINK macro deposits the link section of a word header automatically
; linking the new word to the last.

LASTWORD	.set	0			; Word counter

LINK		.macro	TYPE
		.word	LASTWORD		; Link
		.byte	TYPE			; Type
LASTWORD	.set	$
		.endm

; Deposits a word header containing the name which is linked back to the
; previous word.
;
; The WDC assembler does not handle string parameters to macros very well,
; stopping at the first comma or space in them, so some headers must be
; manually constructed.

NORMAL		.equ	$00
IMMEDIATE	.equ	$80
INLINE		.equ	$40

HEADER		.macro	LEN,NAME,TYPE
		LINK	TYPE
		.byte	LEN,NAME
		.endm

;===============================================================================
; Reset Handler
;-------------------------------------------------------------------------------

		.code
		.org	$F000

RESET:
		sei			; Ensure interrupts disabled
		cld			; Ensure binary mode
		ldx	#$ff		; Reset the stack
		txs

		inx
		stx	RX_HEAD		; Clear buffer offsets
		stx	RX_TAIL
		stx	TX_HEAD
		stx	TX_TAIL

		lda	#%00011111	; 8 bits, 1 stop bit, 19200 baud
		sta	ACIA_CTRL
		lda	#%11001001	; No parity, no interrupt
		sta	ACIA_CMND
		lda	ACIA_DATA	; Clear receive buffer

;===============================================================================
; System/User Variables
;-------------------------------------------------------------------------------

;===============================================================================
; Constants
;-------------------------------------------------------------------------------

; 0 ( -- 0 )
;
; Push the constant value zero on the stack

		HEADER	1,"0",NORMAL
ZERO:
		dex			; Make space on the stack
		dex
		lda	#$00		; And create a zero value
		sta	DSTACK+1,x
		sta	DSTACK+2,x
		rts			; Done

; BL ( -- char )
;
; char is the character value for a space.

		HEADER	2,"BL",NORMAL
BL:
		dex			; Make space on the stack
		dex
		lda	#' '		; And save a space value
		sta	DSTACK+1,x
		lda	#0
		sta	DSTACK+2,x
		rts			; Done

; FALSE ( -- false )
;
; Return a false flag.

		HEADER	5,"FALSE",NORMAL
FALSE:
		dex			; Make space on the stack
		dex
		lda	#$00		; And create a false value
		sta	DSTACK+1,x
		sta	DSTACK+2,x
		rts			; Done

; TRUE ( -- true )
;
; Return a true flag, a single-cell value with all bits set.

		HEADER	4,"TRUE",NORMAL
TRUE:
		dex			; Make space on the stack
		dex
		lda	#$ff		; And create a true value
		sta	DSTACK+1,x
		sta	DSTACK+2,x
		rts			; Done

;===============================================================================
; Radix
;-------------------------------------------------------------------------------

; DECIMAL ( -- )
;
; Set the numeric conversion radix to ten (decimal).

		HEADER	7,"DECIMAL",NORMAL
DECIMAL:
		jsr	DO_LITERAL
		.word	10
		jsr	BASE
		jmp	STORE

; HEX ( -- )
;
; Set contents of BASE to sixteen.

		HEADER	3,"HEX",NORMAL
HEX:
		jsr	DO_LITERAL
		.word	16
		jsr	BASE
		jmp	STORE

;===============================================================================
; Memory Operations
;-------------------------------------------------------------------------------

; ! ( x a-addr -- )
;
; Store x at a-addr.

		HEADER	1,"!",NORMAL
STORE:
		lda	DSTACK+3,x	; Fetch data value
		sta	(DSTACK+1,x)	; .. and store
		inc	DSTACK+1,x
		if eq
		 inc	DSTACK+2,x
		endif
		lda	DSTACK+4,x
		sta	(DSTACK+1,x)
		inx			; Clean up data stack
		inx
		inx
		inx
		rts			; Done

; +! ( n|u a-addr -- )
;
; Add n|u to the single-cell number at a-addr.

		HEADER	2,"+!",NORMAL
PLUS_STORE:
		clc
		lda	DSTACK+3,x	; Fetch data value
		adc	(DSTACK+1,x)
		sta	(DSTACK+1,x)
		inc	DSTACK+1,x
		if eq
		 inc	DSTACK+2,x
		endif
		lda	DSTACK+4,x
		adc	(DSTACK+1,x)
		sta	(DSTACK+1,x)
		inx			; Clean up data stack
		inx
		inx
		inx
		rts			; Done

; , ( x -- )
;
; Reserve one cell of data space and store x in the cell. If the data-space
; pointer is aligned when , begins execution, it will remain aligned when ,
; finishes execution. An ambiguous condition exists if the data-space pointer
; is not aligned prior to execution of ,.
;
; In this implementation is its defined as:
;
;   HERE ! 1 CELLS ALLOT

		LINK	NORMAL
		.byte	1,","
COMMA:
		jsr	HERE
		jsr	STORE
		jsr	DO_LITERAL
		.word	1
		jsr	CELLS
		jmp	ALLOT

; 2! ( x1 x2 a-addr -- )
;
; Store the cell pair x1 x2 at a-addr, with x2 at a-addr and x1 at the next
; consecutive cell.
;
; In this implementation is its defined as:
;
;   SWAP OVER ! CELL+ !.

		HEADER	2,"2!",NORMAL
TWO_STORE:
		jsr	SWAP
		jsr	OVER
		jsr	STORE
		jsr	CELL_PLUS
		jmp	STORE

; 2@ ( a-addr -- x1 x2 )
;
; Fetch the cell pair x1 x2 stored at a-addr. x2 is stored at a-addr and x1 at
; the next consecutive cell.
;
; In this implementation is its defined as:
;
;   DUP CELL+ @ SWAP @

		HEADER	2,"2@",NORMAL
TWO_FETCH:
		jsr	DUP
		jsr	CELL_PLUS
		jsr	FETCH
		jsr	SWAP
		jmp	FETCH

; @ ( a-addr -- x )
;
; x is the value stored at a-addr.

		HEADER	1,"@",NORMAL
FETCH:
		lda	(DSTACK+1,x)	; Fetch from memory
		tay
		inc	DSTACK+1,x
		if eq
		 inc	DSTACK+2,x
		endif
		lda	(DSTACK+1,x)
		sta	DSTACK+2,x	; .. and replace top value
		sty	DSTACK+1,x		    
		rts			; Done

; ALLOT ( n -- )
;
; If n is greater than zero, reserve n address units of data space. If n is
; less than zero, release |n| address units of data space. If n is zero, leave
; the data-space pointer unchanged.
;
; In this implementation its is defined as:
;
;   DP +!

		HEADER	5,"ALLOT",NORMAL
ALLOT:
		jsr	DP
		jmp	PLUS_STORE

; C! ( char c-addr -- )
;
; Store char at c-addr. When character size is smaller than cell size, only the
; number of low-order bits corresponding to character size are transferred.

		HEADER	2,"C!",NORMAL
C_STORE:
		lda	DSTACK+3,x	; Fetch the data value
		sta	(DSTACK+1,x)	; And store it
		inx			; Clean up the stack
		inx
		inx
		inx
		rts			; Done

; C, ( char -- )
;
; Reserve space for one character in the data space and store char in the
; space. If the data-space pointer is character aligned when C, begins
; execution, it will remain character aligned when C, finishes execution.
; An ambiguous condition exists if the data-space pointer is not character-
; aligned prior to execution of C,
;
;   HERE C! 1 CHARS ALLOT

		LINK	NORMAL
		.byte	2,"C,"
C_COMMA:
		jsr	HERE
		jsr	C_STORE
		jsr	DO_LITERAL
		.word	1
		jsr	CHARS
		jmp	ALLOT

; C@ ( c-addr -- char )
;
; Fetch the character stored at c-addr. When the cell size is greater than
; character size, the unused high-order bits are all zeroes.

		HEADER	2,"C@",NORMAL
C_FETCH:
		lda	(DSTACK+1,x)	; Fetch the data byte
		sta	DSTACK+1,x	; .. and replace stack value
		lda	#0
		sta	DSTACK+2,x
		rts			; Done

; HERE ( -- addr )
;
; addr is the data-space pointer.

		HEADER	4,"HERE",NORMAL
HERE:
		jsr	DP
		jmp	FETCH

;===============================================================================
; Alignment
;-------------------------------------------------------------------------------

; ALIGN ( -- )
;
; If the data-space pointer is not aligned, reserve enough space to align it.

		HEADER	5,"ALIGN",NORMAL
ALIGN:
		rts			; Done

; ALIGNED ( addr -- a-addr )
;
; a-addr is the first aligned address greater than or equal to addr.

		HEADER	7,"ALIGNED",NORMAL
ALIGNED:
		rts			; Done

; CELL+ ( a-addr1 -- a-addr2 )
;
; Add the size in address units of a cell to a-addr1, giving a-addr2.

		HEADER	5,"CELL+",NORMAL
CELL_PLUS:
					; Bump the address by two
		lda	#2
		adc	DSTACK+1,x
		sta	DSTACK+1.x
		if cs
		 inc	DSTACK+2,x
		endif
		rts			; Done

; CELLS ( n1 -- n2 )
;
; n2 is the size in address units of n1 cells.

		HEADER	5,"CELLS",NORMAL
CELLS:
		asl	DSTACK+1,x	; Two bytes per cell
		rol	DSTACK+2,x
		rts			; Done

; CHAR+ ( c-addr1 -- c-addr2 )
;
; Add the size in address units of a character to c-addr1, giving c-addr2.

		HEADER	5,"CHAR+",NORMAL
CHAR_PLUS:
		inc	DSTACK+1,x	; Bump the address by one
		if eq
		 inc	DSTACK+2,x
		endif
		rts			; Done

; CHAR- ( c-addr1 -- c-addr2 )
;
; Subtract the size in address units of a character to c-addr1, giving c-addr2.

		HEADER	5,"CHAR-",NORMAL
CHAR_MINUS:
		lda	DSTACK+1.x
		if eq
		 dec	DSTACK+2,x
		endif
		dec	DSTACK+1,x
		rts			; Done

; CHARS ( n1 -- n2 )
;
; n2 is the size in address units of n1 characters.

		HEADER	5,"CHARS",NORMAL
CHARS:
		rts			; Done
		
;===============================================================================
; Stack Operations
;-------------------------------------------------------------------------------

; 2DROP ( x1 x2 -- )
;
; Drop cell pair x1 x2 from the stack.

		HEADER	5,"2DROP",NORMAL
TWO_DROP:
		inx			; Removed two words from stack
		inx
		inx
		inx
		rts			; Done

; 2DUP ( x1 x2 -- x1 x2 x1 x2 )
;
; Duplicate cell pair x1 x2.

		HEADER	4,"2DUP",NORMAL
TWO_DUP:
		dex			; Make space for new value
		dex
		dex
		dex
		lda	DSTACK+5,x	; Copy top two values
		sta	DSTACK+1,x
		lda	DSTACK+6,x
		sta	DSTACK+2,x
		lda	DSTACK+7,x
		sta	DSTACK+3,x
		lda	DSTACK+8,x
		sta	DSTACK+4,x
		rts			; Done

; 2OVER ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
;
; Copy cell pair x1 x2 to the top of the stack.

		HEADER	5,"2OVER",NORMAL
TWO_OVER:
		dex			; Make space for new value
		dex
		dex
		dex
		lda	DSTACK+9,x	; Copy over two values
		sta	DSTACK+1,x
		lda	DSTACK+10,x
		sta	DSTACK+2,x
		lda	DSTACK+11,x
		sta	DSTACK+3,x
		lda	DSTACK+12,x
		sta	DSTACK+4,x
		rts			; Done

; 2ROT ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
;
; Rotate the top three cell pairs on the stack bringing cell pair x1 x2 to
; the top of the stack.

	.if	0
		HEADER	4,"2ROT",NORMAL
TWO_ROT:	jsr	DO_COLON
		lda	<11			; Save x1
		pha
		lda	<9			; Save x2
		pha
		lda	<7			; Move x3
		sta	<11
		lda	<5			; Move x4
		sta	<9
		lda	<3			; Move x5
		sta	<7
		lda	<1			; Move x6
		sta	<5
		pla				; Restore x2
		sta	<1
		pla				; Restore x1
		sta	<3
		rts			; Done
	.endif
	
; 2SWAP ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
;
; Exchange the top two cell pairs.

	.if	0
		HEADER	5,"2SWAP",NORMAL
TWO_SWAP:
		lda	<3			; Save x3
		pha
		lda	<1			; Save x4
		pha
		lda	<7			; Move x1
		sta	<3
		lda	<5			; Move x2
		sta	<1
		pla				; Move x4
		sta	<5
		pla				; Move x3
		sta	<7
		rts			; Done
	.endif
	
; ?DUP ( x -- 0 | x x )
;
; Duplicate x if it is non-zero.

		HEADER	4,"?DUP",NORMAL
QUERY_DUP:
		lda	DSTACK+1,x	; Fetch top value
		ora	DSTACK+2,x
		bne	DUP		; Non-zero value?
		rts			; Done

; DEPTH ( -- +n )
;
; +n is the number of single-cell values contained in the data stack before +n
; was placed on the stack.

	.if 0
		HEADER	5,"DEPTH",NORMAL
DEPTH:		jsr	DO_COLON
		jsr	AT_DP
		jsr	DO_LITERAL,DSTACK_END-1
		jsr	SWAP
		jsr	MINUS
		jsr	TWO_SLASH
		jsr	EXIT
	.endif

; DROP ( x -- )
;
; Remove x from the stack.

		HEADER	4,"DROP",NORMAL|INLINE
DROP:
		inx			; Drop the top value
		inx
		rts			; Done

; DUP ( x -- x x )
;
; Duplicate x.

		HEADER	3,"DUP",NORMAL
DUP:
		dex
		dex
		lda	DSTACK+3,x	; Copy the top value
		sta	DSTACK+1,x
		lda	DSTACK+4,x
		sta	DSTACK+2,x
		rts			; Done

; NIP ( x1 x2 -- x2 )
;
; Drop the first item below the top of stack.

		HEADER	3,"NIP",NORMAL
NIP:
		lda	DSTACK+1,x	; Copy x2 over x1
		sta	DSTACK+3,x
		lda	DSTACK+2,x
		sta	DSTACK+4,x
		jmp	DROP

; OVER ( x1 x2 -- x1 x2 x1 )
;
; Place a copy of x1 on top of the stack.

		HEADER	4,"OVER",NORMAL
OVER:
		dex
		dex
		lda	DSTACK+3,x	; Fetch second value
		sta	DSTACK+1,x	; And make a copy
		lda	DSTACK+4,x
		sta	DSTACK+2,x
		rts			; Done

; PICK ( xu ... x1 x0 u -- xu ... x1 x0 xu )
;
; Remove u. Copy the xu to the top of the stack. An ambiguous condition exists
; if there are less than u+2 items on the stack before PICK is executed.

	.if	0
		HEADER	4,"PICK",NORMAL
PICK:
		lda	<1			; Fetch the index
		asl	a
		tax
		lda	<3,x			; Load the target value
		sta	<1			; .. and save
		rts			; Done
	.endif
	
; ROLL ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
;
; Remove u. Rotate u+1 items on the top of the stack. An ambiguous condition
; exists if there are less than u+2 items on the stack before ROLL is executed.

	.if 0
		HEADER	4,"ROLL",NORMAL
ROLL:
		asl	<1			; Convert count to index
		ldx	<1
		beq	ROLL_2			; Zero? Nothing to do
		lda	<3,x			; Save the final value
		pha
ROLL_1:		lda	<1,x			; Move x-1 to x
		sta	<3,x
		dex				; And repeat
		dex
		bne	ROLL_1
		pla				; Recover the new top value
		sta	<3
ROLL_2:		jmp	DROP			; Drop the count
	.endif
	
; ROT ( x1 x2 x3 -- x2 x3 x1 )
;
; Rotate the top three stack entries.

	.if 0
		HEADER	3,"ROT",NORMAL
ROT:
		ldx	<5			; Save x1
		lda	<3			; Move x2
		sta	<5
		lda	<1			; Move x3
		sta	<3
		stx	<1			; Restore x1
		rts
	.endif
	
; SWAP ( x1 x2 -- x2 x1 )
;
; Exchange the top two stack items.

		HEADER	4,"SWAP",NORMAL
SWAP:
		lda	DSTACK+1.x	; Switch top two words
		ldy	DSTACK+3,x
		sta	DSTACK+3,x
		sty	DSTACK+1,x
		lda	DSTACK+2,x
		ldy	DSTACK+4,x
		sta	DSTACK+4,x
		sty	DSTACK+2,x
		rts			; Done

; TUCK ( x1 x2 -- x2 x1 x2 )
;
; Copy the first (top) stack item below the second stack item.

		HEADER	4,"TUCK",NORMAL
TUCK:
		jsr	SWAP
		jmp	OVER

;===============================================================================
; Return Stack Operations
;-------------------------------------------------------------------------------

; 2>R ( x1 x2 -- ) ( R: -- x1 x2 )
;
; Transfer cell pair x1 x2 to the return stack. Semantically equivalent to
; SWAP >R >R.

		HEADER	3,"2>R",NORMAL
TWO_TO_R:
		jsr	SWAP
		jsr	TO_R
		jmp	TO_R

; 2R> ( -- x1 x2 ) ( R: x1 x2 -- )
;
; Transfer cell pair x1 x2 from the return stack. Semantically equivalent to R>
; R> SWAP.

		HEADER	3,"2R>",NORMAL
TWO_R_FROM:
		jsr	R_FROM
		jsr	R_FROM
		jmp	SWAP

; 2R@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
;
; Copy cell pair x1 x2 from the return stack. Semantically equivalent to R> R>
; 2DUP >R >R SWAP.

		HEADER	3,"2R@",NORMAL
TWO_R_FETCH:
		dex			; Make space for values
		dex
		dex
		dex
		ldy	RSP
		lda	RSTACK+1,y
		sta	DSTACK+1,x
		lda	RSTACK+2,y
		sta	DSTACK+2,x
		lda	RSTACK+3,y
		sta	DSTACK+3,x
		lda	RSTACK+4,y
		sta	DSTACK+4,x
		rts			; Done

; >R ( x -- ) ( R: -- x )
;
; Move x to the return stack.

		HEADER	2,">R",NORMAL
TO_R:
		dec	RSP
		dec	RSP
		ldy	RSP
		lda	DSTACK+1,x	; Transfer top value
		sta	RSTACK+1,y	; .. to return stack
		lda	DSTACK+2,x
		sta	RSTACK+2,y
		inx
		inx
		rts			; Done

; I ( -- n|u ) ( R: loop-sys -- loop-sys )
;
; n|u is a copy of the current (innermost) loop index. An ambiguous condition
; exists if the loop control parameters are unavailable.

		HEADER	1,"I",NORMAL
I:
		dex
		dex
		ldy	RSP
		lda	RSTACK+1,y
		sta	DSTACK+1,x
		lda	RSTACK+2,y
		sta	DSTACK+2,x
		rts

; J ( -- n|u ) ( R: loop-sys1 loop-sys2 -- loop-sys1 loop-sys2 )
;
; n|u is a copy of the next-outer loop index. An ambiguous condition exists if
; the loop control parameters of the next-outer loop, loop-sys1, are
; unavailable.

		HEADER	1,"J",NORMAL
J:
		dex
		dex
		ldy	RSP
		lda	RSTACK+5,y
		sta	DSTACK+1,x
		lda	RSTACK+6,y
		sta	DSTACK+2,x
		rts			; Done

; R> ( -- x ) ( R: x -- )
;
; Move x from the return stack to the data stack.

		HEADER	2,"R>",NORMAL
R_FROM:
		dex
		dex
		ldy	RSP
		lda	RSTACK+1,y
		sta	DSTACK+1,x
		lda	RSTACK+2,y
		sta	DSTACK+2,x
		inc	RSP
		inc	RSP
		rts			; Done

; R@ ( -- x ) ( R: x -- x )
;
; Copy x from the return stack to the data stack.

		HEADER	2,"R@",NORMAL
R_FETCH:
		dex
		dex
		ldy	RSP
		lda	RSTACK+1,y
		sta	DSTACK+1,x
		lda	RSTACK+2,y
		sta	DSTACK+2,x
		rts

;===============================================================================
; Single Precision Arithmetic
;-------------------------------------------------------------------------------

; * ( n1|u1 n2|u2 -- n3|u3 )
;
; Multiply n1|u1 by n2|u2 giving the product n3|u3.
;
; In this implementation it is defined as:
;
;   M* DROP

		HEADER	1,"*",NORMAL
STAR:
		jsr	M_STAR
		jmp	DROP

; */ ( n1 n2 n3 -- n4 )
;
; Multiply n1 by n2 producing the intermediate double-cell result d. Divide d
; by n3 giving the single-cell quotient n4. An ambiguous condition exists if
; n3 is zero or if the quotient n4 lies outside the range of a signed number.
; If d and n3 differ in sign, the implementation-defined result returned will
; be the same as that returned by either the phrase >R M* R> FM/MOD SWAP DROP
; or the phrase >R M* R> SM/REM SWAP DROP.
;
; In this implementation it is defined as:
;
;   >R M* R> FM/MOD SWAP DROP

		HEADER	2,"*/",NORMAL
STAR_SLASH:
		jsr	TO_R
		jsr	M_STAR
		jsr	R_FROM
		jsr	FM_SLASH_MOD
		jsr	SWAP
		jmp	DROP

; */MOD ( n1 n2 n3 -- n4 n5 )
;
; Multiply n1 by n2 producing the intermediate double-cell result d. Divide d
; by n3 producing the single-cell remainder n4 and the single-cell quotient n5.
; An ambiguous condition exists if n3 is zero, or if the quotient n5 lies
; outside the range of a single-cell signed integer. If d and n3 differ in
; sign, the implementation-defined result returned will be the same as that
; returned by either the phrase >R M* R> FM/MOD or the phrase >R M* R> SM/REM.
;
; In this implementation it is defined as:
;
;   >R M* R> FM/MOD

		HEADER	5,"*/MOD",NORMAL
STAR_SLASH_MOD:
		jsr	TO_R
		jsr	M_STAR
		jsr	R_FROM
		jmp	FM_SLASH_MOD

; + ( n1|u1 n2|u2 -- n3|u3 )
;
; Add n2|u2 to n1|u1, giving the sum n3|u3.

		HEADER	1,"+",NORMAL
PLUS:
		clc			; Add top two values
		lda	DSTACK+3,x
		adc	DSTACK+1,x
		sta	DSTACK+3,x
		lda	DSTACK+4,x
		adc	DSTACK+2,x
		sta	DSTACK+4,x
		inx			; Clean up data stack
		inx
		rts			; Done

; - ( n1|u1 n2|u2 -- n3|u3 )
;
; Subtract n2|u2 from n1|u1, giving the difference n3|u3.

		HEADER	1,"-",NORMAL
MINUS:
		sbc			; Subtract top two values
		lda	DSTACK+3,x
		sbc	DSTACK+1,x
		sta	DSTACK+3,x
		lda	DSTACK+4,x
		sbc	DSTACK+2,x
		sta	DSTACK+4,x
		inx			; Clean up data stack
		inx
		rts			; Done

; / ( n1 n2 -- n3 )
;
; Divide n1 by n2, giving the single-cell quotient n3. An ambiguous condition
; exists if n2 is zero. If n1 and n2 differ in sign, the implementation-defined
; result returned will be the same as that returned by either the phrase >R S>D
; R> FM/MOD SWAP DROP or the phrase >R S>D R> SM/REM SWAP DROP.
;
; In this implementation it is defined as:
;
;   >R S>D R> FM/MOD SWAP DROP

		HEADER	1,"/",NORMAL
SLASH:
		jsr	TO_R
		jsr	S_TO_D
		jsr	R_FROM
		jsr	FM_SLASH_MOD
		jsr	SWAP
		jmp	DROP

; /MOD ( n1 n2 -- n3 n4 )
;
; Divide n1 by n2, giving the single-cell remainder n3 and the single-cell
; quotient n4. An ambiguous condition exists if n2 is zero. If n1 and n2 differ
; in sign, the implementation-defined result returned will be the same as that
; returned by either the phrase >R S>D R> FM/MOD or the phrase >R S>D R> SM/REM.
;
; In this implementation it is defined as:
;
;   >R S>D R> FM/MOD

		HEADER	4,"/MOD",NORMAL
SLASH_MOD:
		jsr	TO_R
		jsr	S_TO_D
		jsr	R_FROM
		jmp	FM_SLASH_MOD

; 1+ ( n1|u1 -- n2|u2 )
;
; Add one (1) to n1|u1 giving the sum n2|u2.

		HEADER	2,"1+",NORMAL
ONE_PLUS:
		inc	DSTACK+1,x	; Increment top of stack
		if eq
		 inc	DSTACK+2,x
		endif			
		rts			; Done

; 1- ( n1|u1 -- n2|u2 )
;
; Subtract one (1) from n1|u1 giving the difference n2|u2.

		HEADER	2,"1-",NORMAL
ONE_MINUS:
		lda	DSTACK+1,x	; Decrement top of stack
		if eq
		 dec	DSTACK+2,x
		endif
		dec	DSTACK+1,x		       
		rts			; Done

; 2* ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

		HEADER	2,"2*",NORMAL
TWO_STAR:
		asl	DSTACK+1,x	; Multiply top value by two
		rol	DSTACK+2,x
		rts			; Done

; 2/ ( x1 -- x2 )
;
; x2 is the result of shifting x1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

		HEADER	2,"2/",NORMAL
TWO_SLASH:
		lda	DSTACK+2,x	; Load the top value
		cmp	#$80		; Extract the top bit
		ror	DSTACK+2,x	; And shift back into value
		ror	DSTACK+1,x
		rts

; ?NEGATE ( x sign -- x/-x)
;
; If the sign value is negative then negate the value of x to match.
;
; In this implementation it is defined as:
;
;   0< IF NEGATE THEN

QUERY_NEGATE:
		jsr	ZERO_LESS
		jsr	QUERY_BRANCH
		.word	QUERY_NEGATE_1
		jsr	NEGATE
QUERY_NEGATE_1: rts

; ABS ( n -- u )
;
; u is the absolute value of n.

		HEADER	3,"ABS",NORMAL
ABS:
		lda	DSTACK+2,x
		if mi
		 jmp	NEGATE
		endif
		rts			; Done

; FM/MOD ( n1 n2 -- n3 n4 )
;
; Divide n1 by n2, giving the single-cell remainder n3 and the single-cell
; quotient n4. An ambiguous condition exists if n2 is zero. If n1 and n2 differ
; in sign, the implementation-defined result returned will be the same as that
; returned by either the phrase >R S>D R> FM/MOD or the phrase >R S>D R> SM/REM.
;
; In this implementation it is defined as:
;
;   DUP >R			divisor
;   2DUP XOR >R			sign of quotient
;   >R				divisor
;   DABS R@ ABS UM/MOD
;   SWAP R> ?NEGATE SWAP	apply sign to remainder
;   R> 0< IF			if quotient negative,
;	NEGATE
;	OVER IF			if remainder nonzero,
;	R@ ROT - SWAP 1-	adjust rem,quot
;	THEN
;   THEN  R> DROP ;

		HEADER	6,"FM/MOD",NORMAL
FM_SLASH_MOD:
		jsr	DUP
		jsr	TO_R
		jsr	TWO_DUP
		jsr	XOR
		jsr	TO_R
		jsr	TO_R
		jsr	DABS
		jsr	R_FETCH
		jsr	ABS
		jsr	UM_SLASH_MOD
		jsr	SWAP
		jsr	R_FROM
		jsr	QUERY_NEGATE
		jsr	SWAP
		jsr	R_FROM
		jsr	ZERO_LESS
		jsr	QUERY_BRANCH
		.word	FM_SLASH_MOD_1
		jsr	NEGATE
		jsr	OVER
		jsr	QUERY_BRANCH
		.word	FM_SLASH_MOD_1
		jsr	R_FETCH
		jsr	ROT
		jsr	MINUS
		jsr	SWAP
		jsr	ONE_MINUS
FM_SLASH_MOD_1: jsr	R_FROM
		jmp	DROP

; MAX ( n1 n2 -- n3 )
;
; n3 is the greater of n1 and n2.

		HEADER	3,"MAX",NORMAL
MAX:		jsr	DO_COLON
		jsr	TWO_DUP
		jsr	LESS
		jsr	QUERY_BRANCH,MAX_1
		jsr	SWAP
MAX_1:		jsr	DROP
		jsr	EXIT

; MIN ( n1 n2 -- n3 )
;
; n3 is the lesser of n1 and n2.

		HEADER	3,"MIN",NORMAL
MIN:		jsr	DO_COLON
		jsr	TWO_DUP
		jsr	GREATER
		jsr	QUERY_BRANCH,MIN_1
		jsr	SWAP
MIN_1:		jsr	DROP
		jsr	EXIT

; MOD ( n1 n2 -- n3 )
;
; Divide n1 by n2, giving the single-cell remainder n3. An ambiguous condition
; exists if n2 is zero. If n1 and n2 differ in sign, the implementation-defined
; result returned will be the same as that returned by either the phrase >R S>D
; R> FM/MOD DROP or the phrase >R S>D R> SM/REM DROP.
;
; In this implementation it is defined as:
;
;   >R S>D R> FM/MOD DROP

		HEADER	3,"MOD",NORMAL
MOD:		jsr	DO_COLON
		jsr	TO_R
		jsr	S_TO_D
		jsr	R_FROM
		jsr	FM_SLASH_MOD
		jsr	DROP
		jsr	EXIT

; NEGATE ( n1 -- n2 )
;
; Negate n1, giving its arithmetic inverse n2.

		HEADER	6,"NEGATE",NORMAL
NEGATE:
		sec				; Negate the top of stack
		lda	#0
		sbc	<1
		sta	<1
		rts			   ; Done

; UMAX ( x1 x2 -- x3 )
;
; x3 is the greater of x1 and x2.

		HEADER	4,"UMAX",NORMAL
UMAX:
		lda	<1			; Compare the top values
		cmp	<3
		bcs	UMAX_EXIT		; Is x2 biggest?
		jmp	DROP			; No, x1 is
UMAX_EXIT:	jmp	NIP

; UMIN ( x1 x2 -- x3 )
;
; x3 is the lesser of x1 and x2.

		HEADER	4,"UMIN",NORMAL
UMIN:
		lda	<1			; Compare the top values
		cmp	<3
		bcc	UMIN_EXIT		; Is x2 smallest?
		jmp	DROP			; No, x1 is
UMIN_EXIT:	jmp	NIP

;===============================================================================
; Double Precision Arithmetic
;-------------------------------------------------------------------------------

; ?DNEGATE ( d1 sign -- d1/-d1 )
;
; If sign is less than zero than negate d1 otherwise leave it unchanged.

QUERY_DNEGATE:	jsr	DO_COLON
		jsr	ZERO_LESS
		jsr	QUERY_BRANCH,QUERY_DNEG_1
		jsr	DNEGATE
QUERY_DNEG_1:	jsr	EXIT

; D+ ( d1|ud1 d2|ud2 -- d3|ud3 )
;
; Add d2|ud2 to d1|ud1, giving the sum d3|ud3.

		HEADER	2,"D+",NORMAL
D_PLUS:
		clc
		lda	<7			; Add low words
		adc	<3
		sta	<7
		lda	<5			; Then the high words
		adc	<1
		sta	<5
		tdc				; Drop top double
		inc	a
		inc	a
		inc	a
		inc	a
		tcd
		rts			   ; Done

; D- ( d1|ud1 d2|ud2 -- d3|ud3 )
;
; Subtract d2|ud2 from d1|ud1, giving the difference d3|ud3.

		HEADER	2,"D-",NORMAL
D_MINUS:
		sec
		lda	<7			; Subtract low words
		sbc	<3
		sta	<7
		lda	<5			; Then the high words
		sbc	<1
		sta	<5
		tdc				; Drop top double
		inc	a
		inc	a
		inc	a
		inc	a
		tcd
		rts			   ; Done

; D0< ( d -- flag )
;
; flag is true if and only if d is less than zero.

		HEADER	3,"D0<",NORMAL
D_ZERO_LESS:
		ldx	<1			; Fetch sign
		tdc				; Drop a word
		inc	a
		inc	a
		tcd
		stz	<1			; Assume false
		txa
		bpl	D_ZERO_LESS_1
		dec	<1
D_ZERO_LESS_1:	rts

; D0= ( d -- flag )
;
; flag is true if and only if d is equal to zero.

		HEADER	3,"D0=",NORMAL
D_ZERO_EQUAL:
		ldx	<1			; Fetch sign
		tdc				; Drop a word
		inc	a
		inc	a
		tcd
		stz	<1			; Assume false
		txa
		bne	D_ZERO_EQUAL_1
		dec	<1
D_ZERO_EQUAL_1: rts

; D2* ( xd1 -- xd2 )
;
; xd2 is the result of shifting xd1 one bit toward the most-significant bit,
; filling the vacated least-significant bit with zero.

		HEADER	3,"D2*",NORMAL
D_TWO_STAR:
		asl	<3
		rol	<1
		rts

; D2/ ( xd1 -- xd2 )
;
; xd2 is the result of shifting xd1 one bit toward the least-significant bit,
; leaving the most-significant bit unchanged.

		HEADER	3,"D2/",NORMAL
D_TWO_SLASH:
		lda	<1
		rol	a
		ror	<1
		ror	<3
		rts

; D< ( d1 d2 -- flag )
;
; flag is true if and only if d1 is less than d2.

		HEADER	2,"D<",NORMAL
D_LESS:		jsr	DO_COLON
		jsr	D_MINUS
		jsr	D_ZERO_LESS
		jsr	EXIT

; D= ( d1 d2 -- flag )
;
; flag is true if and only if d1 is bit-for-bit the same as d2.

		HEADER	2,"D=",NORMAL
D_EQUAL:	jsr	DO_COLON
		jsr	D_MINUS
		jsr	D_ZERO_EQUAL
		jsr	EXIT

; DABS ( d -- ud )
;
; ud is the absolute value of d.

		HEADER	4,"DABS",NORMAL
DABS:
		lda	<1
		bpl	DABS_1
		jmp	DNEGATE
DABS_1:		rts

; DMAX ( d1 d2 -- d3 )
;
; d3 is the greater of d1 and d2.

		HEADER	4,"DMAX",NORMAL
DMAX:		jsr	DO_COLON
		jsr	TWO_OVER
		jsr	TWO_OVER
		jsr	D_LESS
		jsr	QUERY_BRANCH,DMAX_1
		jsr	TWO_SWAP
DMAX_1:		jsr	TWO_DROP
		jsr	EXIT

; DMIN ( d1 d2 -- d3 )
;
; d3 is the lesser of d1 and d2.

		HEADER	4,"DMIN",NORMAL
DMIN:		jsr	DO_COLON
		jsr	TWO_OVER
		jsr	TWO_OVER
		jsr	D_LESS
		jsr	INVERT
		jsr	QUERY_BRANCH,DMIN_1
		jsr	TWO_SWAP
DMIN_1:		jsr	TWO_DROP
		jsr	EXIT

; DNEGATE ( d1 -- d2 )
;
; d2 is the negation of d1.

		HEADER	7,"DNEGATE",NORMAL
DNEGATE:
		sec
		lda	#0			; Subtract low word from zero
		sbc	<3
		sta	<3
		lda	#0			; Then the high word
		sbc	<1
		sta	<1
		rts			   ; Done

;===============================================================================
; Mixed Arithmetic
;-------------------------------------------------------------------------------

; D>S ( d -- n )
;
; n is the equivalent of d. An ambiguous condition exists if d lies outside the
; range of a signed single-cell number.

		HEADER	3,"D>S",NORMAL
D_TO_S:
		tdc
		inc	a			; Drop the high word
		inc	a
		tcd
		rts

; M* ( n1 n2 -- d )
;
; d is the signed product of n1 times n2.
;
; In this implementation it is defined as:
;
;   2DUP XOR >R			carries sign of the result
;   SWAP ABS SWAP ABS UM*
;   R> ?DNEGATE

		HEADER	2,"M*",NORMAL
M_STAR:		jsr	DO_COLON
		jsr	TWO_DUP
		jsr	XOR
		jsr	TO_R
		jsr	SWAP
		jsr	ABS
		jsr	SWAP
		jsr	ABS
		jsr	UM_STAR
		jsr	R_FROM
		jsr	QUERY_DNEGATE
		jsr	EXIT

; M*/ ( d1 n1 +n2 -- d2 )
;
; Multiply d1 by n1 producing the triple-cell intermediate result t. Divide t
; by +n2 giving the double-cell quotient d2. An ambiguous condition exists if
; +n2 is zero or negative, or the quotient lies outside of the range of a
; double-precision signed integer.



; M+ ( d1|ud1 n -- d2|ud2 )
;
; Add n to d1|ud1, giving the sum d2|ud2.

		HEADER	2,"M+",NORMAL
M_PLUS:
		clc
		lda	<1
		adc	<5
		sta	<5
		bcc	$+4
		inc	<3
		tdc
		inc	a
		inc	a
		tcd
		rts

; S>D ( n -- d )
;
; Convert the number n to the double-cell number d with the same numerical
; value.

		HEADER	3,"S>D",NORMAL
S_TO_D:
		tdc
		dec	a			; Assume n is positive
		dec	a
		tcd
		stz	<1			; .. push a zero value
		lda	<3			; Test the number
		bpl	S_TO_D_1
		dec	<1			; Make top -1 if negative
S_TO_D_1	rts			   ; Done

; SM/REM ( d1 n1 -- n2 n3 )
;
; Divide d1 by n1, giving the symmetric quotient n3 and the remainder n2.
; Input and output stack arguments are signed. An ambiguous condition exists if
; n1 is zero or if the quotient lies outside the range of a single-cell signed
; integer.
;
; In this implementation it is defined as:
;
;   2DUP XOR >R			sign of quotient
;   OVER >R			sign of remainder
;   ABS >R DABS R> UM/MOD
;   SWAP R> ?NEGATE
;   SWAP R> ?NEGATE ;

		HEADER	6,"SM/REM",NORMAL
SM_SLASH_REM:	jsr	DO_COLON
		jsr	TWO_DUP
		jsr	XOR
		jsr	TO_R
		jsr	OVER
		jsr	TO_R
		jsr	ABS
		jsr	TO_R
		jsr	DABS
		jsr	R_FROM
		jsr	UM_SLASH_MOD
		jsr	SWAP
		jsr	R_FROM
		jsr	QUERY_NEGATE
		jsr	SWAP
		jsr	R_FROM
		jsr	QUERY_NEGATE
		jsr	EXIT

; UD* ( ud1 d2 -- ud3)
;
; 32*16->32 multiply
;
;   DUP >R UM* DROP  SWAP R> UM* ROT + ;

		HEADER	3,"UD*",NORMAL
UD_STAR:	jsr	DO_COLON
		jsr	DUP
		jsr	TO_R
		jsr	UM_STAR
		jsr	DROP
		jsr	SWAP
		jsr	R_FROM
		jsr	UM_STAR
		jsr	ROT
		jsr	PLUS
		jsr	EXIT

; UM* ( u1 u2 -- ud )
;
; Multiply u1 by u2, giving the unsigned double-cell product ud. All values and
; arithmetic are unsigned.

		HEADER	3,"UM*",NORMAL
UM_STAR:
		lda	<1			; Fetch multiplier
		pha
		stz	<1			; Clear the result
		ldx	#16
UM_STAR_1:	lda	<3			; Shift multiplier one bit
		lsr	a
		bcc	UM_STAR_2		; Not set, no add
		lda	1,s			; Fetch multiplicand
		clc
		adc	<1
		sta	<1
UM_STAR_2:	ror	<1			; Rotate high word down
		ror	<3
		dex
		bne	UM_STAR_1
		pla
		rts			   ; Done

; UM/MOD ( ud u1 -- u2 u3 )
;
; Divide ud by u1, giving the quotient u3 and the remainder u2. All values and
; arithmetic are unsigned. An ambiguous condition exists if u1 is zero or if the
; quotient lies outside the range of a single-cell unsigned integer.

		HEADER	6,"UM/MOD",NORMAL
UM_SLASH_MOD:
		sec				; Check for overflow
		lda	<3
		sbc	<1
		bcs	UM_SLASH_MOD_3

		ldx	#17
UM_SLASH_MOD_1: rol	<5			; Rotate dividend lo
		dex
		beq	UM_SLASH_MOD_4
		rol	<3
		bcs	UM_SLASH_MOD_2		; Carry set dividend > divisor

		lda	<3			; Is dividend < divisor?
		cmp	<1
		bcc	UM_SLASH_MOD_1		; Yes, shift in 0

UM_SLASH_MOD_2: lda	<3			; Reduce dividend
		sbc	<1
		sta	<3
		bra	UM_SLASH_MOD_1		; Shift in 1

UM_SLASH_MOD_3: lda	#$ffff			; Overflowed set results
		sta	<3
		sta	<5
UM_SLASH_MOD_4: tdc				; Drop top word
		inc	a
		inc	a
		tcd
		jmp	SWAP			; Swap quotient and remainder

;===============================================================================
; Comparisons
;-------------------------------------------------------------------------------

; 0< ( n -- flag )
;
; flag is true if and only if n is less than zero.

		HEADER	2,"0<",NORMAL
ZERO_LESS:
		lda	<1			; Test top of stack
		stz	<1			; Assume false result
		bpl	ZERO_LT_1		; Was the value negative?
		dec	<1			; Yes, make true result
ZERO_LT_1:	rts			   ; Done

; 0<> ( x -- flag )
;
; flag is true if and only if x is not equal to zero.

		HEADER	3,"0<>",NORMAL
ZERO_NOT_EQUAL:
		lda	<1			; Test top of stack
		stz	<1			; Assume false result
		beq	ZERO_NE_1		; Was the value non-zero?
		dec	<1			; Yes, make true result
ZERO_NE_1:	rts			   ; Done

; 0= ( x -- flag )
;
; flag is true if and only if x is equal to zero.

		HEADER	2,"0=",NORMAL
ZERO_EQUAL:
		lda	<1			; Test top of stack
		stz	<1			; Assume false result
		bne	ZERO_EQ_1		; Was the value zero?
		dec	<1			; Yes, make true result
ZERO_EQ_1:	rts			   ; Done

; 0> ( n -- flag )
;
; flag is true if and only if n is greater than zero.

		HEADER	2,"0>",NORMAL
ZERO_GREATER:
		lda	<1			; Test top of stack
		stz	<1			; Assume false result
		bmi	ZERO_GT_EXIT		; Was the value positive?
		beq	ZERO_GT_EXIT		; .. but not zero
		dec	<1			; Yes, make true result
ZERO_GT_EXIT:	rts			   ; Done

; < ( n1 n2 -- flag )
;
; flag is true if and only if n1 is less than n2.

		HEADER	1,"<",NORMAL
LESS:
		jsr	SWAP
		jmp	GREATER

; <> ( x1 x2 -- flag )
;
; flag is true if and only if x1 is not bit-for-bit the same as x2.

		HEADER	2,"<>",NORMAL
NOT_EQUAL:
		ldx	<1			; Pull x2 from stack
		tdc
		inc	a
		inc	a
		tcd
		cpx	<1			; Compare with x1
		stz	<1			; Assume equal
		beq	NE_EXIT			; Test flags
		dec	<1			; Make result true
NE_EXIT:	rts			   ; Done

; = ( x1 x2 -- flag )
;
; flag is true if and only if x1 is bit-for-bit the same as x2.

		HEADER	1,"=",NORMAL
EQUAL:
		ldx	<1			; Pull x2 from stack
		tdc
		inc	a
		inc	a
		tcd
		cpx	<1			; Compare with x1
		stz	<1			; Assume not equal
		bne	EQ_EXIT			; Test the flags
		dec	<1			; Make result true
EQ_EXIT:	rts			   ; Done

; > ( n1 n2 -- flag )
;
; flag is true if and only if n1 is greater than n2.

		HEADER	1,">",NORMAL
GREATER:
		ldx	<1			; Pull x2 from stack
		tdc
		inc	a
		inc	a
		tcd
		txa
		sec				; Compare with x1
		sbc	<1
		stz	<1			; Assume false result
		bvc	GREATER_1
		eor	#$8000
GREATER_1:	bpl	GREATER_2		; V == 1 && N == 1
		dec	<1
GREATER_2:	rts

; U< ( u1 u2 -- flag )
;
; flag is true if and only if u1 is less than u2.

		HEADER	2,"U<",NORMAL
U_LESS:
		ldx	<1			; Pull x2
		tdc				; Drop from stack
		inc	a
		inc	a
		tcd
		cpx	<1			; Compare with x1
		stz	<1			; Assume false
		beq	U_LESS_1		; Equal
		bcc	U_LESS_1		; Less
		dec	<1
U_LESS_1:	rts

; U> ( u1 u2 -- flag )
;
; flag is true if and only if u1 is greater than u2.

		HEADER	2,"U>",NORMAL
U_GREATER:
		jsr	SWAP
		jmp	U_LESS

;===============================================================================
; Logical Operations
;-------------------------------------------------------------------------------

; AND ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit logical “and” of x1 with x2.

		HEADER	3,"AND",NORMAL
AND:
		lda	<1
		and	<3
		sta	<3
		tdc
		inc	a
		inc	a
		tcd
		rts

; INVERT ( x1 -- x2 )
;
; Invert all bits of x1, giving its logical inverse x2.

		HEADER	6,"INVERT",NORMAL
INVERT:
		lda	<1			; Fetch top value
		eor	#$ffff			; Invert all the bits
		sta	<1			; .. and write back
		rts			   ; Done

; LSHIFT ( x1 u -- x2 )
;
; Perform a logical left shift of u bit-places on x1, giving x2. Put zeroes
; into the least significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

		HEADER	6,"LSHIFT",NORMAL
LSHIFT:
		ldx	<1			; Pull bit count
		php
		tdc
		inc	a			; .. from the stack
		inc	a
		tcd
		plp
		beq	LSHIFT_0		; Zero shift?
		cpx	#16			; Shifting by 16+ bits
		bcs	LSHIFT_2		; Yes, result will be zero
LSHIFT_1	asl	<1			; Shift one bit left
		dex				; Update count
		bne	LSHIFT_1		; .. and repeat as needed
LSHIFT_0	rts			   ; Done
LSHIFT_2	stz	<1			; Clear top value
		rts			   ; Done

; OR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit inclusive-or of x1 with x2.

		HEADER	2,"OR",NORMAL
OR:
		lda	<1
		ora	<3
		sta	<3
		tdc
		inc	a
		inc	a
		tcd
		rts

; RSHIFT ( x1 u -- x2 )
;
; Perform a logical right shift of u bit-places on x1, giving x2. Put zeroes
; into the most significant bits vacated by the shift. An ambiguous condition
; exists if u is greater than or equal to the number of bits in a cell.

		HEADER	6,"RSHIFT",NORMAL
RSHIFT:
		ldx	<1			; Pull bit count
		php
		tdc
		inc	a			; .. from the stack
		inc	a
		tcd
		plp
		beq	RSHIFT_0		; Zero shift?
		cpx	#16			; Shifting by 16+ bits
		bcs	RSHIFT_2		; Yes, result will be zero
RSHIFT_1	lsr	<1			; Shift one bit left
		dex				; Update count
		bne	RSHIFT_1		; .. and repeat as needed
RSHIFT_0	rts			   ; Done
RSHIFT_2	stz	<1			; Clear top value
		rts			   ; Done

; XOR ( x1 x2 -- x3 )
;
; x3 is the bit-by-bit exclusive-or of x1 with x2.

		HEADER	3,"XOR",NORMAL
XOR:
		lda	<1
		eor	<3
		sta	<3
		tdc
		inc	a
		inc	a
		tcd
		rts

;===============================================================================
; Control Words
;-------------------------------------------------------------------------------

; ?ABORT
;
;   ROT IF TYPE ABORT THEN 2DROP ;

QUERY_ABORT:
		jsr	ROT
		jsr	QUERY_BRANCH
		.word	QUERY_ABORT_1
		jsr	TYPE
		jsr	ABORT
QUERY_ABORT_1:	jsr	TWO_DROP
		rts

; ABORT ( i*x -- ) ( R: j*x -- )
;
; Empty the data stack and perform the function of QUIT, which includes
; emptying the return stack, without displaying a message.

		HEADER	5,"ABORT",NORMAL
ABORT:
		jsr	DO_ABORT
		jsr	QUIT

DO_ABORT:
		lda	#DSTACK_END-1
		tcd
		rts

; (BUILD) ( dtc-addr -- )
;
; Adds a jump the to exection function for the new word.

;		HEADER	7,"(BUILD)",NORMAL
BUILD:
		jsr	DO_LITERAL
		.word	$20
		jsr	C_COMMA
		jmp	COMMA

; CREATE ( -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below. If the data-
; space pointer is not aligned, reserve enough data space to align it. The new
; data-space pointer defines name’s data field. CREATE does not allocate data
; space in name’s data field.

		HEADER	6,"CREATE",NORMAL
CREATE:
		jsr	LATEST
		jsr	FETCH
		jsr	COMMA
		jsr	ZERO
		jsr	C_COMMA
		jsr	HERE
		jsr	LATEST
		jsr	STORE
		jsr	BL
		jsr	WORD
		jsr	C_FETCH
		jsr	ONE_PLUS
		jmp	ALLOT

; EXECUTE ( i*x xt -- j*x )
;
; Remove xt from the stack and perform the semantics identified by it. Other
; stack effects are due to the word EXECUTEd.

		HEADER	7,"EXECUTE",NORMAL
EXECUTE:
		ldx	<1
		tdc
		inc	a
		inc	a
		tcd
		dex
		phx
		rts

; EXIT ( -- ) ( R: nest-sys -- )
;
; Return control to the calling definition specified by nest-sys. Before
; executing EXIT within a do-loop, a program shall discard the loop-control
; parameters by executing UNLOOP.

		HEADER	4,"EXIT",NORMAL
EXIT:
		pla
		pla
		rts

; QUIT ( -- ) ( R: i*x -- )
;
; Empty the return stack, store zero in SOURCE-ID if it is present, make the
; user input device the input source, and enter interpretation state. Do not
; display a message. Repeat the following:
; – Accept a line from the input source into the input buffer, set >IN to zero,
;   and interpret.
; – Display the implementation-defined system prompt if in interpretation state,
;   all processing has been completed, and no ambiguous condition exists.
;
; In this implementation it is defined as:
;
;   DO_QUIT 0 STATE !
;   0 (SOURCE-ID) !
;   BEGIN
;     REFILL
;     WHILE SOURCE EVALUATE
;     STATE @ 0= IF S" Ok" CR TYPE THEN
;   AGAIN ;

		HEADER	4,"QUIT",NORMAL
QUIT:
		jsr	DO_QUIT
		jsr	ZERO
		jsr	STATE
		jsr	STORE
		jsr	ZERO
		jsr	SOURCEID
		jsr	STORE
QUIT_1:		jsr	REFILL
		jsr	QUERY_BRANCH
		.word	QUIT_2
		jsr	INTERPRET
QUIT_2:		jsr	STATE
		jsr	FETCH
		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	QUIT_3
		jsr	DO_S_QUOTE
		db	2,"Ok"
		jsr	TYPE
		jsr	CR
QUIT_3:		jsr	BRANCH
		.word	QUIT_1

DO_QUIT:
		lda	#RSTACK_END-1		; Reset the return stack
		tcs
		rts			   ; Done

;===============================================================================
; Parser & Interpreter
;-------------------------------------------------------------------------------

; ?NUMBER
;
;   DUP	 0 0 ROT COUNT	    -- ca ud adr n
;   ?SIGN >R  >NUMBER	    -- ca ud adr' n'
;   IF	 R> 2DROP 2DROP 0   -- ca 0   (error)
;   ELSE 2DROP NIP R>
;	IF NEGATE THEN	-1  -- n -1   (ok)
;   THEN ;

		HEADER	7,"?NUMBER",NORMAL
QUERY_NUMBER:
		jsr	DUP
		jsr	ZERO
		jsr	ZERO
		jsr	ROT
		jsr	COUNT
		jsr	QUERY_SIGN
		jsr	TO_R
		jsr	TO_NUMBER
		jsr	QUERY_BRANCH
		.word	QNUM_1
		jsr	R_FROM
		jsr	TWO_DROP
		jsr	TWO_DROP
		jsr	ZERO
		jsr	BRANCH
		.word	QNUM_3
QNUM_1:		jsr	TWO_DROP
		jsr	NIP
		jsr	R_FROM
		jsr	QUERY_BRANCH
		.word	QNUM_2
		jsr	NEGATE
QNUM_2:		jsr	DO_LITERAL
		.word	-1
QNUM_3:		rts

; ?SIGN ( c-addr n -- adr' n' f )
;
;   OVER C@		    -- adr n c
;   2C - DUP ABS 1 = AND    -- +=-1, -=+1, else 0
;   DUP IF 1+		    -- +=0, -=+2
;	>R 1 /STRING R>	    -- adr' n' f
;   THEN ;

		HEADER	5,"?SIGN",NORMAL
QUERY_SIGN:
		jsr	OVER
		jsr	C_FETCH
		jsr	DO_LITERAL
		.word	','
		jsr	MINUS
		jsr	DUP
		jsr	ABS
		jsr	DO_LITERAL
		.word	1
		jsr	EQUAL
		jsr	AND
		jsr	DUP
		jsr	QUERY_BRANCH
		.word	QSIGN_1
		jsr	ONE_PLUS
		jsr	TO_R
		jsr	DO_LITERAL,1
		jsr	SLASH_STRING
		jsr	R_FROM
QSIGN_1:	rts

; >COUNTED ( c-addr n -- )
;
;   2DUP C! CHAR+ SWAP CMOVE

TO_COUNTED:
		jsr	TWO_DUP
		jsr	C_STORE
		jsr	CHAR_PLUS
		jsr	SWAP
		jmp	CMOVE

; >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
;
; ud2 is the unsigned result of converting the characters within the string
; specified by c-addr1 u1 into digits, using the number in BASE, and adding
; each into ud1 after multiplying ud1 by the number in BASE. Conversion
; continues left-to-right until a character that is not convertible, including
; any “+” or “-”, is encountered or the string is entirely converted. c-addr2
; is the location of the first unconverted character or the first character
; past the end of the string if the string was entirely converted. u2 is the
; number of unconverted characters in the string. An ambiguous condition exists
; if ud2 overflows during the conversion.
;
; In this implementation its is defined as:
;
;   BEGIN
;   DUP WHILE
;	OVER C@ DIGIT?
;	0= IF DROP EXIT THEN
;	>R 2SWAP BASE @ UD*
;	R> M+ 2SWAP
;	1 /STRING
;   REPEAT ;

		HEADER	7,">NUMBER",NORMAL
TO_NUMBER:
TO_NUM_1:	jsr	DUP
		jsr	QUERY_BRANCH
		.word	TO_NUM_3
		jsr	OVER
		jsr	C_FETCH
		jsr	DIGIT_QUERY
		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	TO_NUM_2
		jsr	DROP
		jsr	EXIT
TO_NUM_2:	jsr	TO_R
		jsr	TWO_SWAP
		jsr	BASE
		jsr	FETCH
		jsr	UD_STAR
		jsr	R_FROM
		jsr	M_PLUS
		jsr	TWO_SWAP
		jsr	DO_LITERAL
		.word	1
		jsr	SLASH_STRING
		jsr	BRANCH
		.word	TO_NUM_1
TO_NUM_3:	rts

; ACCEPT ( c-addr +n1 -- +n2 )
;
; Receive a string of at most +n1 characters. An ambiguous condition exists if
; +n1 is zero or greater than 32,767. Display graphic characters as they are
; received. A program that depends on the presence or absence of non-graphic
; characters in the string has an environmental dependency. The editing
; functions, if any, that the system performs in order to construct the string
; are implementation-defined.
;
; Input terminates when an implementation-defined line terminator is received.
; When input terminates, nothing is appended to the string, and the display is
; maintained in an implementation-defined way.
;
; +n2 is the length of the string stored at c-addr.
;
; In this implementation it is defined as:
;
;   OVER + 1- OVER	-- sa ea a
;   BEGIN KEY		-- sa ea a c
;   DUP 0D <> WHILE
;     DUP 8 = OVER 127 = OR IF
;	DROP 1-
;	>R OVER R> UMAX
;	8 EMIT SPACE 8 EMIT
;     ELSE
;	DUP EMIT	-- sa ea a c
;	OVER C! 1+ OVER UMIN
;     THEN		-- sa ea a
;   REPEAT		-- sa ea a c
;   DROP NIP SWAP - ;

		HEADER	6,"ACCEPT",NORMAL
ACCEPT:
		jsr	OVER
		jsr	PLUS
		jsr	ONE_MINUS
		jsr	OVER
ACCEPT_1:	jsr	KEY
		jsr	DUP
		jsr	DO_LITERAL
		.word	$0D
		jsr	NOT_EQUAL
		jsr	QUERY_BRANCH
		.word	ACCEPT_4
		jsr	DUP
		jsr	DO_LITERAL
		.word	$08
		jsr	EQUAL
		jsr	OVER
		jsr	DO_LITERAL
		.word	$7f
		jsr	EQUAL
		jsr	OR
		jsr	QUERY_BRANCH
		.word	ACCEPT_2
		jsr	DROP
		jsr	ONE_MINUS
		jsr	TO_R
		jsr	OVER
		jsr	R_FROM
		jsr	UMAX
		jsr	DO_LITERAL
		.word	8
		jsr	EMIT
		jsr	SPACE
		jsr	DO_LITERAL
		.word	8
		jsr	EMIT
		jsr	BRANCH
		.word	ACCEPT_3
ACCEPT_2:	jsr	DUP
		jsr	EMIT
		jsr	OVER
		jsr	C_STORE
		jsr	ONE_PLUS
		jsr	OVER
		jsr	UMIN
ACCEPT_3:	jsr	BRANCH
		.word	ACCEPT_1
ACCEPT_4:	jsr	DROP
		jsr	NIP
		jsr	SWAP
		jmp	MINUS

; DIGIT?
;
;   [ HEX ] DUP 39 > 100 AND +	   silly looking
;   DUP 140 > 107 AND -	  30 -	   but it works!
;   DUP BASE @ U< ;

		HEADER	6,"DIGIT?",NORMAL
DIGIT_QUERY:
		jsr	DUP
		jsr	DO_LITERAL
		.word	'9'
		jsr	GREATER
		jsr	DO_LITERAL
		.word	$100
		jsr	AND
		jsr	PLUS
		jsr	DUP
		jsr	DO_LITERAL
		.word	$140
		jsr	GREATER
		jsr	DO_LITERAL
		.word	$107
		jsr	AND
		jsr	MINUS
		jsr	DO_LITERAL
		.word	'0'
		jsr	MINUS
		jsr	DUP
		jsr	BASE
		jsr	FETCH
		jmp	U_LESS

; EVALUATE ( i*x c-addr u -- j*x )
;
; Save the current input source specification. Store minus-one (-1) in
; SOURCE-ID if it is present. Make the string described by c-addr and u both
; the input source and input buffer, set >IN to zero, and interpret. When the
; parse area is empty, restore the prior input source specification. Other
; stack effects are due to the words EVALUATEd.
;
;   >R >R SAVE-INPUT
;   -1 (SOURCE-ID) !
;   0 >IN ! (LENGTH) ! (BUFFER) !
;   INTERPRET
;   RESTORE-INPUT DROP

		HEADER	8,"EVALUATE",NORMAL
EVALUATE:
		jsr	TO_R
		jsr	TO_R
		jsr	SAVE_INPUT
		jsr	R_FROM
		jsr	R_FROM
		jsr	TRUE
		jsr	SOURCEID
		jsr	STORE
		jsr	ZERO
		jsr	TO_IN
		jsr	STORE
		jsr	LENGTH
		jsr	STORE
		jsr	BUFFER
		jsr	STORE
		jsr	INTERPRET
		jsr	RESTORE_INPUT
		jmp	DROP

; INTERPRET ( -- )
;
;
;   BEGIN
;   BL WORD DUP C@ WHILE	-- textadr
;	FIND			-- a 0/1/-1
;	?DUP IF			-- xt 1/-1
;	    1+ STATE @ 0= OR	immed or interp?
;	    IF EXECUTE ELSE , THEN
;	ELSE			-- textadr
;	    ?NUMBER
;	    IF STATE @
;		IF POSTPONE LITERAL THEN     converted ok
;	    ELSE COUNT TYPE 3F EMIT CR ABORT  err
;	    THEN
;	THEN
;   REPEAT DROP ;

		HEADER	9,"INTERPRET",NORMAL
INTERPRET:
INTERPRET_1:	jsr	BL
		jsr	WORD
		jsr	DUP
		jsr	C_FETCH
		jsr	QUERY_BRANCH
		.word	INTERPRET_7
		jsr	FIND
		jsr	QUERY_DUP
		jsr	QUERY_BRANCH
		.word	INTERPRET_4
		jsr	ONE_PLUS
		jsr	STATE
		jsr	FETCH
		jsr	ZERO_EQUAL
		jsr	OR
		jsr	QUERY_BRANCH
		.word	INTERPRET_2
		jsr	EXECUTE
		jsr	BRANCH
		.word	INTERPRET_3
INTERPRET_2:	jsr	COMMA
INTERPRET_3:	jsr	BRANCH
		.word	INTERPRET_6
INTERPRET_4:	jsr	QUERY_NUMBER
		jsr	QUERY_BRANCH
		.word	INTERPRET_5
		jsr	STATE
		jsr	FETCH
		jsr	QUERY_BRANCH
		.word	INTERPRET_6
		jsr	LITERAL
		jsr	BRANCH
		.word	INTERPRET_6
INTERPRET_5:	jsr	COUNT
		jsr	TYPE
		jsr	DO_LITERAL
		.word	$3f
		jsr	EMIT
		jsr	CR
		jsr	ABORT
INTERPRET_6	jsr	BRANCH
		.word	INTERPRET_1
INTERPRET_7:	jmp	DROP

; FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
;
; Find the definition named in the counted string at c-addr. If the definition
; is not found, return c-addr and zero. If the definition is found, return its
; execution token xt. If the definition is immediate, also return one (1),
; otherwise also return minus-one (-1). For a given string, the values returned
; by FIND while compiling may differ from those returned while not compiling.
;
; In this implementation it is defined as:
;
;   LATEST @ BEGIN	       -- a nfa
;	2DUP OVER C@ CHAR+     -- a nfa a nfa n+1
;	S=		       -- a nfa f
;	DUP IF
;	    DROP
;	    NFA>LFA @ DUP      -- a link link
;	THEN
;   0= UNTIL		       -- a nfa	 OR  a 0
;   DUP IF
;	NIP DUP NFA>CFA	       -- nfa xt
;	SWAP IMMED?	       -- xt iflag
;	0= 1 OR		       -- xt 1/-1
;   THEN ;

		HEADER	4,"FIND",NORMAL
FIND:
		jsr	LATEST
		jsr	FETCH
FIND1:		jsr	TWO_DUP
		jsr	OVER
		jsr	C_FETCH
		jsr	CHAR_PLUS
		jsr	S_EQUAL
		jsr	DUP
		jsr	QUERY_BRANCH
		.word	FIND2
		jsr	DROP
		jsr	NFA_TO_LFA
		jsr	FETCH
		jsr	DUP
FIND2:		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	FIND1
		jsr	DUP
		jsr	QUERY_BRANCH
		.word	FIND3
		jsr	NIP
		jsr	DUP
		jsr	NFA_TO_CFA
		jsr	SWAP
		jsr	IMMED_QUERY
		jsr	ZERO_EQUAL
		jsr	DO_LITERAL
		.word	1
		jsr	OR
FIND3:		rts

; IMMED? ( nfa -- f )

IMMED_QUERY:
		jsr	ONE_MINUS
		jmp	C_FETCH

; NFA>CFA ( nfa -- cfa )

NFA_TO_CFA:
		jsr	COUNT
		jmp	PLUS

; NFA>LFA ( nfa -- lfa )

NFA_TO_LFA:
		jsr	DO_LITERAL
		.word	3
		jmp	MINUS

; REFILL ( -- flag )
;
; Attempt to fill the input buffer from the input source, returning a true flag
; if successful.
;
; When the input source is the user input device, attempt to receive input into
; the terminal input buffer. If successful, make the result the input buffer,
; set >IN to zero, and return true. Receipt of a line containing no characters
; is considered successful. If there is no input available from the current
; input source, return false.
;
; When the input source is a string from EVALUATE, return false and perform no
; other action.
;
; In this implementation it is defined as:
;
;   SOURCE-ID 0= IF
;     TIB DUP #TIB @ ACCEPT SPACE
;     LENGTH ! BUFFER !
;     0 >IN ! TRUE EXIT
;   THEN
;   FALSE

		HEADER	6,"REFILL",NORMAL
REFILL:
		jsr	SOURCE_ID
		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	REFILL_1
		jsr	TIB
		jsr	DUP
		jsr	HASH_TIB
		jsr	FETCH
		jsr	ACCEPT
		jsr	SPACE
		jsr	LENGTH
		jsr	STORE
		jsr	BUFFER
		jsr	STORE
		jsr	ZERO
		jsr	TO_IN
		jsr	STORE
		jsr	TRUE
		jsr	EXIT
REFILL_1:	jmp	FALSE

; RESTORE-INPUT ( xn ... x1 n -- flag )
;
; Attempt to restore the input source specification to the state described by
; x1 through xn. flag is true if the input source specification cannot be so
; restored.
;
; An ambiguous condition exists if the input source represented by the
; arguments is not the same as the current input source.
;
; In this implementation it is defined as:
;
;   >IN ! (LENGTH) ! BUFFER !
;   SOURCEID !
;   TRUE

		HEADER	13,"RESTORE-INPUT",NORMAL
RESTORE_INPUT
		jsr	TO_IN
		jsr	STORE
		jsr	LENGTH
		jsr	STORE
		jsr	BUFFER
		jsr	STORE
		jsr	SOURCEID
		jsr	STORE
		jmp	TRUE

; S= ( c-addr1 caddr2 u -- n)
;
; Misnamed, more like C's strncmp. Note that counted length bytes are compared!

S_EQUAL:
		phy
		ldx	<1			; Fetch maximum length
		beq	S_EQUAL_3
		ldy	#0
		short_a
S_EQUAL_1:
		lda	(5),y			; Compare bytes
		cmp	(3),y
		bne	S_EQUAL_2
		iny
		dex				; End of strings?
		bne	S_EQUAL_1		; No
		bra	S_EQUAL_3		; Yes. must be the same
S_EQUAL_2:
		ldx	#$ffff			; Difference found
S_EQUAL_3:
		long_a
		tdc				; Clean up the stack
		inc	a
		inc	a
		inc	a
		inc	a
		tcd
		stx	<1			; Save the flag
		ply
		rts

; SAVE-INPUT ( -- xn ... x1 n )
;
; x1 through xn describe the current state of the input source specification
; for later use by RESTORE-INPUT.

		HEADER	10,"SAVE-INPUT",NORMAL
SAVE_INPUT:
		jsr	SOURCEID
		jsr	FETCH
		jsr	BUFFER
		jsr	FETCH
		jsr	LENGTH
		jsr	FETCH
		jsr	TO_IN
		jsr	FETCH
		jsr	EXIT

; SCAN ( c-addr n c == c-addr' n' )

SCAN:
SCAN_1:
		lda	<3			; Any data left to scan?
		beq	SCAN_2			; No.
		lda	<1			; Fetch and compare with scan
		short_a
		cmp	(5)
		long_a
		beq	SCAN_2
		inc	<5
		dec	<3
		bra	SCAN_1
SCAN_2:
		jmp	DROP			; Drop the character

; SKIP ( c-addr n c == c-addr' n' )

SKIP:
SKIP_1:		lda	<3			; Any data left to skip over?
		beq	SKIP_2			; No.
		lda	<1			; Fetch and compare with skip
		short_a
		cmp	(5)
		long_a
		bne	SKIP_2			; Cannot be skipped
		inc	<5			; Bump data address
		dec	<3			; and update length
		bra	SKIP_1			; And repeat
SKIP_2:
		jmp	DROP			; Drop the character

; SOURCE ( -- c-addr u )
;
; c-addr is the address of, and u is the number of characters in, the input
; buffer.
;
; In this implementation it is defined as
;
;   BUFFER @ LENGTH @

		HEADER	6,"SOURCE",NORMAL
SOURCE:
		jsr	BUFFER
		jsr	FETCH
		jsr	LENGTH
		jmp	FETCH

; SOURCE-ID ( -- 0 | -1 )
;
; Identifies the input source: -1 if string (via EVALUATE), 0 if user input
; device.

		HEADER	9,"SOURCE-ID",NORMAL
SOURCE_ID:
		jsr	SOURCEID
		jmp	FETCH

; WORD ( char “<chars>ccc<char>” -- c-addr )
;
; Skip leading delimiters. Parse characters ccc delimited by char. An
; ambiguous condition exists if the length of the parsed string is greater
; than the implementation-defined length of a counted string.
;
; c-addr is the address of a transient region containing the parsed word as
; a counted string. If the parse area was empty or contained no characters
; other than the delimiter, the resulting string has a zero length. A space,
; not included in the length, follows the string. A program may replace
; characters within the string.
;
; In this implementation it is defined as:
;
;   DUP	 SOURCE >IN @ /STRING	-- c c adr n
;   DUP >R   ROT SKIP		-- c adr' n'
;   OVER >R  ROT SCAN		-- adr" n"
;   DUP IF CHAR- THEN	     skip trailing delim.
;   R> R> ROT -	  >IN +!	update >IN offset
;   TUCK -			-- adr' N
;   HERE >counted		--
;   HERE			-- a
;   BL OVER COUNT + C! ;    append trailing blank

		HEADER	4,"WORD",NORMAL
WORD:
		jsr	DUP
		jsr	SOURCE
		jsr	TO_IN
		jsr	FETCH
		jsr	SLASH_STRING
		jsr	DUP
		jsr	TO_R
		jsr	ROT
		jsr	SKIP
		jsr	OVER
		jsr	TO_R
		jsr	ROT
		jsr	SCAN
		jsr	DUP
		jsr	QUERY_BRANCH
		.word	WORD_1
		jsr	CHAR_MINUS
WORD_1:		jsr	R_FROM
		jsr	R_FROM
		jsr	ROT
		jsr	MINUS
		jsr	TO_IN
		jsr	PLUS_STORE
		jsr	TUCK
		jsr	MINUS
		jsr	HERE
		jsr	TO_COUNTED
		jsr	HERE
		jsr	BL
		jsr	OVER
		jsr	COUNT
		jsr	PLUS
		jmp	C_STORE

;===============================================================================
; String Words
;-------------------------------------------------------------------------------

; -TRAILING ( c-addr u1 -- c-addr u2 )
;
; If u1 is greater than zero, u2 is equal to u1 less the number of spaces at
; the end of the character string specified by c-addr u1. If u1 is zero or the
; entire string consists of spaces, u2 is zero.

		HEADER	9,"-TRAILING",NORMAL
DASH_TRAILING:
		phy				; Save IP
		ldy	<1			; Is u1 > 0?
		beq	DASH_TRAIL_3		; No
		short_a
		dey				; Convert to offset
DASH_TRAIL_1:	lda	(3),y			; Space character at end?
		cmp	#' '
		bne	DASH_TRAIL_2		; No
		dey				; More characters to check?
		bpl	DASH_TRAIL_1		; Yes
DASH_TRAIL_2:	long_a
		iny				; Convert to length
DASH_TRAIL_3:	sty	<1			; Update
		ply				; Restore IP
		rts			   ; Done

; /STRING ( c-addr1 u1 n -- c-addr2 u2 )
;
; Adjust the character string at c-addr1 by n characters. The resulting
; character string, specified by c-addr2 u2, begins at c-addr1 plus n;
; characters and is u1 minus n characters long.
;
; In this implementation it is defined as:
;
;   ROT OVER + ROT ROT -

		HEADER	7,"/STRING",NORMAL
SLASH_STRING:
		jsr	ROT
		jsr	OVER
		jsr	PLUS
		jsr	ROT
		jsr	ROT
		jmp	MINUS

; BLANK ( c-addr u -- )
;
; If u is greater than zero, store the character value for space in u
; consecutive character positions beginning at c-addr.
;
; In this implementation it is defined as
;
;   ?DUP IF OVER + SWAP DO BL I C! LOOP ELSE DROP THEN

		HEADER	5,"BLANK",NORMAL
BLANK:
		jsr	QUERY_DUP
		jsr	QUERY_BRANCH
		.word	BLANK_2
		jsr	OVER
		jsr	PLUS
		jsr	SWAP
		jsr	DO_DO
BLANK_1:	jsr	BL
		jsr	I
		jsr	C_STORE
		jsr	DO_LOOP
		.word	BLANK_1
		jsr	EXIT
BLANK_2:	jmp	DROP

; CMOVE ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from lower addresses to higher addresses.

		HEADER	5,"CMOVE",NORMAL
CMOVE:
		phy
		ldx	<1			; Any characters to move?
		beq	CMOVE_2			; No
		ldy	#0
		short_a
CMOVE_1:					; Transfer a byte
		lda	(5),y
		sta	(3),y
		iny
		dex				; Decrement count
		bne	CMOVE_1			; .. and repeat until done
		long_a
CMOVE_2:
		tdc				; Clean up the stack
		clc
		adc	#6
		tcd
		ply
		rts			   ; Done

; CMOVE> ( c-addr1 c-addr2 u -- )
;
; If u is greater than zero, copy u consecutive characters from the data space
; starting at c-addr1 to that starting at c-addr2, proceeding character-by-
; character from higher addresses to lower addresses.

		HEADER	6,"CMOVE>",NORMAL
CMOVE_GREATER:
		phy
		ldx	<1			; Any characters to move?
		beq	CMOVE_GT_2		; No.
		ldy	<1
		short_a
CMOVE_GT_1:
		dey				; Transfer a byte
		lda	(5),y
		sta	(3),y
		dex				; Decrement length
		bne	CMOVE_GT_1		; .. and repeat until done
		long_a
CMOVE_GT_2:
		tdc				; Clean up the stack
		clc
		adc	#6
		tcd
		ply
		rts			   ; Done

; COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
;
; Compare the string specified by c-addr1 u1 to the string specified by c-addr2
; u2. The strings are compared, beginning at the given addresses, character by
; character, up to the length of the shorter string or until a difference is
; found. If the two strings are identical, n is zero. If the two strings are
; identical up to the length of the shorter string, n is minus-one (-1) if u1
; is less than u2 and one (1) otherwise. If the two strings are not identical
; up to the length of the shorter string, n is minus-one (-1) if the first
; non-matching character in the string specified by c-addr1 u1 has a lesser
; numeric value than the corresponding character in the string specified by
; c-addr2 u2 and one (1) otherwise.

		HEADER	7,"COMPARE",NORMAL
COMPARE:
		lda	<1			; Both string lengths zero?
		ora	<5
		beq	COMPARE_X		; Yes, must be equal

		lda	<1			; Second string length zero?
		beq	COMPARE_P		; Yes, must be shorter
		lda	<5			; First string length zero?
		beq	COMPARE_N		; Yes, must be shorter
		short_a
		lda	(7)			; Compare next characters
		cmp	(3)
		long_a
		bcc	COMPARE_N
		bne	COMPARE_P

		inc	<3			; Bump string pointers
		inc	<7
		dec	<1			; And reduce lengths
		dec	<5
		bra	COMPARE

COMPARE_P:	lda	#1
		bra	COMPARE_X
COMPARE_N:	lda	#-1

COMPARE_X:	sta	<7			; Save the result
		tdc
		clc
		adc	#6
		tcd
		rts			   ; Done

; COUNT ( c-addr1 -- c-addr2 u )
;
; Return the character string specification for the counted string stored at
; c-addr1. c-addr2 is the address of the first character after c-addr1. u is
; the contents of the character at c-addr1, which is the length in characters
; of the string at c-addr2.
;
; In this implementation it is defined as
;
;   DUP CHAR+ SWAP C@

		HEADER	5,"COUNT",NORMAL
COUNT:
		jsr	DUP
		jsr	CHAR_PLUS
		jsr	SWAP
		jmp	C_FETCH

; SEARCH ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
;
; Search the string specified by c-addr1 u1 for the string specified by c-addr2
; u2. If flag is true, a match was found at c-addr3 with u3 characters
; remaining. If flag is false there was no match and c-addr3 is c-addr1 and u3
; is u1.

		HEADER	6,"SEARCH",NORMAL
SEARCH:		jsr	DO_COLON
; TODO
		rts

;===============================================================================
; Compiling Words
;-------------------------------------------------------------------------------

; ( ( -- )
;
; Parse ccc delimited by ) (right parenthesis). ( is an immediate word.
;
; The number of characters in ccc may be zero to the number of characters in the
; parse area.
;
; In this implementation it is defined as:
;
;  [ HEX ] 29 WORD DROP ; IMMEDIATE

		HEADER	1,"(",IMMEDIATE
		jsr	DO_LITERAL
		.word	')'
		jsr	WORD
		jmp	DROP

; .( ( “ccc<paren>” -- )
;
; Parse and display ccc delimited by ) (right parenthesis). .( is an immediate
; word.

		HEADER	2,".(",IMMEDIATE
DOT_PAREN:
		jsr	DO_LITERAL
		.word	')'
		jsr	WORD
		jsr	COUNT
		jmp	TYPE

; ." ( “ccc<quote>” -- )
;
; Parse ccc delimited by " (double-quote). Append the run-time semantics given
; below to the current definition.

		LINK	IMMEDIATE
		db	2,".",'"'
DOT_QUOTE:
		jsr	S_QUOTE
		jsr	DO_LITERAL
		.word	TYPE
		jmp	COMMA

; +LOOP ( -- )

		HEADER	5,"+LOOP",IMMEDIATE
PLUS_LOOP:
		jsr	DO_LITERAL
		.word	DO_PLUS_LOOP
		jsr	COMMA
		jsr	COMMA
		jsr	QUERY_DUP
		jsr	QUERY_BRANCH
		.word	PLUS_LOOP_1
		jsr	HERE
		jsr	SWAP
		jsr	STORE
PLUS_LOOP_1:	rts

DO_PLUS_LOOP:
		ldx	<1			; Fetch increment
		tdc				; And drop
		inc	a
		inc	a
		tcd
		clc				; Add to loop counter
		txa
		adc	1,s
		sta	1,s
		cmp	3,s			; Reached limit?
		bcs	DO_PLOOP_END		; Yes
		lda	!0,y			; No, branch back to start
		tay
		rts			   ; Done

DO_PLOOP_END:	iny				; Skip over address
		iny
		pla				; Drop loop variables
		pla
		rts			   ; Done

; ' ( -- xt )
;
; Skip leading space delimiters. Parse name delimited by a space. Find name and
; return xt, the execution token for name. An ambiguous condition exists if name
; is not found.
;
; In this implementation it is defined as:
;
;   BL WORD FIND 0= IF ." ?" ABORT THEN

		HEADER	1,"'",NORMAL
TICK:
		jsr	BL
		jsr	WORD
		jsr	FIND
		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	TICK_1
		jsr	DO_S_QUOTE
		.byte	1,"?"
		jsr	ABORT
TICK_1:		jsr	EXIT

; : ( -- )

		HEADER	1,":",NORMAL
COLON:
		jsr	CREATE
		jsr	DO_LITERAL
		.word	DO_COLON
		jsr	BUILD
		jmp	RIGHT_BRACKET

DO_COLON:
		plx				; Pull new word IP-1
		phy				; Save the old IP
		inx				; Work out new IP
		txy
		rts			   ; Done

; :NONAME ( -- xt )

		HEADER	7,":NONAME",NORMAL
NONAME:
		jsr	HERE
		jsr	DO_LITERAL
		.word	DO_COLON
		jsr	BUILD
		jmp	RIGHT_BRACKET

; ; ( -- )

		LINK	IMMEDIATE
		db	1,";"
SEMICOLON:
		jsr	DO_LITERAL
		.word	EXIT
		jsr	COMMA
		jmp	LEFT_BRACKET

; ?DO ( -- jump orig )

		HEADER	3,"?DO",IMMEDIATE
QUERY_DO:
		jsr	DO_LITERAL
		.word	QUERY_DO_DO
		jsr	COMMA
		jsr	HERE
		jsr	ZERO
		jsr	COMMA
		jmp	HERE

QUERY_DO_DO:
		lda	<1			; Are the start and limit
		eor	<3			; .. the same?
		beq	QUERY_DO_DO_1
		iny				; No, Skip over jump address
		iny
		jmp	DO_DO			; And start a normal loop

QUERY_DO_DO_1:	tdc				; Drop the loop parameters
		inc	a
		inc	a
		inc	a
		inc	a
		tcd
		jmp	BRANCH			; And skip over loop

; 2CONSTANT ( x “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below.

		HEADER	9,"2CONSTANT",NORMAL
TWO_CONSTANT:
		jsr	CREATE
		jsr	DO_LITERAL
		.word	DO_TWO_CONSTANT
		jsr	BUILD
		jsr	COMMA
		jmp	COMMA

DO_TWO_CONSTANT:
		plx				; Get return address
		tdc				; Create space on stack
		dec	a
		dec	a
		dec	a
		dec	a
		tcd
		lda	!1,x			; Transfer the value
		sta	<1
		lda	!3,x
		sta	<3
		rts			   ; Done

; 2LITERAL

		HEADER	8,"2LITERAL",IMMEDIATE
TWO_LITERAL:
		jsr	DO_LITERAL
		.word	DO_TWO_LITERAL
		jsr	COMMA
		jsr	COMMA
		jmp	COMMA

DO_TWO_LITERAL:
		tdc				; Make room on stack
		dec	a
		dec	a
		dec	a
		dec	a
		tcd
		lda	!0,y			; Fetch constant from IP
		sta	<1
		lda	!2,y
		sta	<3
		iny				; Bump IP
		iny
		iny
		iny
		rts			   ; Done

; 2VARIABLE

		HEADER	9,"2VARIABLE",IMMEDIATE
TWO_VARIABLE:
		jsr	CREATE
		jsr	DO_LITERAL
		.word	DO_VARIABLE
		jsr	BUILD
		jsr	DO_LITERAL
		.word	2
		jsr	CELLS
		jmp	ALLOT

; ABORT" ( -- )

		LINK	IMMEDIATE
		db	6,"ABORT",'"'
ABORT_QUOTE:
		jsr	S_QUOTE
		jsr	DO_LITERAL
		.word	QUERY_ABORT
		jmp	COMMA

; AGAIN ( orig -- )
		HEADER	5,"AGAIN",IMMEDIATE
AGAIN:
		jsr	DO_LITERAL,BRANCH
		jsr	COMMA
		jmp	COMMA

; BEGIN ( -- orig )

		HEADER	5,"BEGIN",IMMEDIATE
BEGIN:
		jmp	HERE

; CHAR ( -- char )
;
;   BL WORD 1+ C@

		HEADER	4,"CHAR",NORMAL
CHAR:
		jsr	BL
		jsr	WORD
		jsr	ONE_PLUS
		jmp	C_FETCH

; CONSTANT ( x “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below.

		HEADER	8,"CONSTANT",NORMAL
CONSTANT:
		jsr	CREATE
		jsr	DO_LITERAL
		.word	DO_CONSTANT
		jsr	BUILD
		jmp	COMMA

DO_CONSTANT:
		plx				; Get return address
		tdc				; Create space on stack
		dec	a
		dec	a
		tcd
		lda	!1,x			; Transfer the value
		sta	<1
		rts			   ; Done

; DO ( -- 0 orig )

		HEADER	2,"DO",IMMEDIATE
DO:
		jsr	DO_LITERAL
		.word	DO_DO
		jsr	COMMA
		jsr	ZERO
		jmp	HERE

DO_DO:
		lda	<3
		pha
		lda	<1
		pha
		tdc
		inc	a
		inc	a
		inc	a
		inc	a
		tcd
		rts

; ELSE ( jump -- jump' )

		HEADER	4,"ELSE",IMMEDIATE
ELSE:
		jsr	DO_LITERAL
		.word	BRANCH
		jsr	COMMA
		jsr	HERE
		jsr	ZERO
		jsr	COMMA
		jsr	HERE
		jsr	SWAP
		jmp	STORE

BRANCH:
		lda	!0,y			; Load branch address into IP
		tay
		rts			   ; Done

; IF ( -- jump )

		HEADER	2,"IF",IMMEDIATE
IF:
		jsr	DO_LITERAL
		.word	QUERY_BRANCH
		jsr	COMMA
		jsr	HERE
		jsr	ZERO
		jsr	COMMA
		jsr	EXIT

QUERY_BRANCH:
		ldx	<1			; Pull the top of stack value
		tdc
		inc	a			; Drop top item
		inc	a
		tcd
		txa
		beq	BRANCH			; Branch if top was zero
		iny				; Otherwise skip address
		iny
		rts			   ; Done

; IMMEDIATE ( -- )

		HEADER	9,"IMMEDIATE",IMMEDIATE
		jsr	DO_LITERAL
		.word	IMMEDIATE
		jsr	LATEST
		jsr	FETCH
		jsr	ONE_MINUS
		jsr	C_STORE
		jsr	EXIT

; LITERAL ( x -- )
;
; Append the run-time semantics given below to the current definition.

		HEADER	7,"LITERAL",IMMEDIATE
LITERAL:
		jsr	DO_LITERAL
		.word	DO_LITERAL
		jsr	COMMA
		jsr	COMMA
		jsr	EXIT

DO_LITERAL:
		tdc				; Make room on stack
		dec	a
		dec	a
		tcd
		lda	!0,y			; Fetch constant from IP
		sta	<1
		iny
		iny
		rts			   ; Done

; LOOP ( jump orig -- )

		HEADER	4,"LOOP",IMMEDIATE
LOOP:
		jsr	DO_LITERAL
		.word	DO_LOOP
		jsr	COMMA
		jsr	COMMA
		jsr	QUERY_DUP
		jsr	QUERY_BRANCH
		.word	LOOP_1
		jsr	HERE
		jsr	SWAP
		jsr	STORE
LOOP_1:		rts

; (LOOP)

DO_LOOP
		lda	1,s			; Add one to loop counter
		inc	a
		sta	1,s
		cmp	3,s			; Reached limit?
		bcs	DO_LOOP_END		; Yes
		lda	!0,y			; No, branch back to start
		tay
		rts			   ; Done

DO_LOOP_END:	iny				; Skip over address
		iny
		pla				; Drop loop variables
		pla
		rts			   ; Done

; POSTPONE

;   BL WORD FIND
;   DUP 0= ABORT" ?"
;   0< IF   -- xt	non immed: add code to current
;			def'n to compile xt later.
;	['] LIT ,XT  ,	add "LIT,xt,COMMAXT"
;	['] ,XT ,XT	to current definition
;   ELSE  ,XT	   immed: compile into cur. def'n
;   THEN ; IMMEDIATE

		HEADER	8,"POSTPONE",IMMEDIATE
POSTPONE:
		jsr	BL
		jsr	WORD
		jsr	FIND
		jsr	DUP
		jsr	ZERO_EQUAL
		jsr	DO_S_QUOTE
		.byte	1,"?"
		jsr	QUERY_ABORT
		jsr	ZERO_LESS
		jsr	QUERY_BRANCH
		.word	POSTPONE_1
		jsr	DO_LITERAL
		.word	DO_LITERAL
		jsr	COMMA
		jsr	COMMA
		jsr	BRANCH
		.word	POSTPONE_2
POSTPONE_1:	jsr	COMMA
POSTPONE_2:	rts

; RECURSE ( -- )

		HEADER	7,"RECURSE",IMMEDIATE
RECURSE:
		jsr	LATEST
		jsr	FETCH
		jsr	NFA_TO_CFA
		jmp	COMMA

; REPEAT ( orig jump -- )

		HEADER	6,"REPEAT",IMMEDIATE
REPEAT:
		jsr	SWAP
		jsr	DO_LITERAL
		.word	BRANCH
		jsr	COMMA
		jsr	COMMA
		jsr	HERE
		jsr	SWAP
		jmp	STORE

; S"

		LINK	IMMEDIATE
		db	2,"S",'"'
S_QUOTE:
		jsr	DO_LITERAL
		.word	DO_S_QUOTE
		jsr	COMMA
		jsr	DO_LITERAL
		.word	'"'
		jsr	WORD
		jsr	C_FETCH
		jsr	ONE_PLUS
		jsr	ALIGNED
		jmp	ALLOT

; (S") ( -- c-addr u )

DO_S_QUOTE:
		jsr	R_FROM
		jsr	COUNT
		jsr	TWO_DUP
		jsr	PLUS
		jsr	ALIGNED
		jmp	TO_R

; THEN ( orig -- )

		HEADER	4,"THEN",IMMEDIATE
THEN:
		jsr	HERE
		jsr	SWAP
		jmp	STORE

; UNTIL ( orig -- )

		HEADER	5,"UNTIL",IMMEDIATE
UNTIL:
		jsr	DO_LITERAL
		.word	QUERY_BRANCH
		jsr	COMMA
		jmp	COMMA

; USER

		HEADER	4,"USER",NORMAL
USER:
		jsr	CREATE
		jsr	DO_LITERAL
		.word	DO_USER
		jsr	BUILD
		jsr	COMMA
		jsr	EXIT

DO_USER:
		tdc
		dec	a			; Push on data stack
		dec	a
		tcd
		plx
		clc
		lda	!1,x
		adc	#USER_AREA
		sta	<1
		rts			   ; Done

; VARIABLE ( “<spaces>name” -- )
;
; Skip leading space delimiters. Parse name delimited by a space. Create a
; definition for name with the execution semantics defined below. Reserve one
; cell of data space at an aligned address.

		LINK	NORMAL
		db	8,"VARIABLE"
VARIABLE:
		jsr	CREATE
		jsr	DO_LITERAL
		.word	DO_VARIABLE
		jsr	BUILD
		jsr	DO_LITERAL
		.word	1
		jsr	CELLS
		jmp	ALLOT

DO_VARIABLE:
		tdc
		dec	a
		dec	a
		tcd
		pla
		inc	a
		sta	<1
		rts

; WHILE ( orig -- orig jump )

		HEADER	5,"WHILE",IMMEDIATE
WHILE:
		jsr	DO_LITERAL
		.word	QUERY_BRANCH
		jsr	COMMA
		jsr	HERE
		jsr	ZERO
		jmp	COMMA

; WORDS ( -- )
;
;   LATEST @ BEGIN
;	DUP COUNT TYPE SPACE
;	NFA>LFA @
;   DUP 0= UNTIL
;   DROP ;

		HEADER	5,"WORDS",NORMAL
		jsr	LATEST
		jsr	FETCH
WORDS_1:	jsr	DUP
		jsr	COUNT
		jsr	TYPE
		jsr	SPACE
		jsr	NFA_TO_LFA
		jsr	FETCH
		jsr	DUP
		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	WORDS_1
		jmp	DROP

; [
;
; In this implementation it is defined as
;
;   0 STATE !

		HEADER	1,"[",IMMEDIATE
LEFT_BRACKET:
		jsr	ZERO
		jsr	STATE
		jmp	STORE

; [']

		HEADER	3,"[']",IMMEDIATE
		jsr	TICK
		jsr	DO_LITERAL
		.word	DO_LITERAL
		jsr	COMMA
		jmp	COMMA

; [CHAR]
;
;   CHAR ['] LIT ,XT  , ; IMMEDIATE

		HEADER	6,"[CHAR]",IMMEDIATE
		jsr	CHAR
		jsr	DO_LITERAL
		.word	DO_LITERAL
		jsr	COMMA
		jmp	COMMA

; \ ( -- )
;
; Parse and discard the remainder of the parse area. \ is an immediate word.
;
; In this implementation it is defined as
;
;   1 WORD DROP

		HEADER	1,"\",IMMEDIATE
BACKSLASH:
		jsr	DO_LITERAL
		.word	1
		jsr	WORD
		jmp	DROP

; ]
;
; In this implementation it is defined as
;
;   -1 STATE !

		HEADER	1,"]",NORMAL
RIGHT_BRACKET:
		jsr	DO_LITERAL
		.word	-1
		jsr	STATE
		jmp	STORE

;===============================================================================
; I/O Operations
;-------------------------------------------------------------------------------

; CR ( -- )
;
; Cause subsequent output to appear at the beginning of the next line.
;
; In this implementation it is defined as
;
;   13 EMIT 10 EMIT

		HEADER	2,"CR",NORMAL
CR:
		jsr	DO_LITERAL
		.word	13
		jsr	EMIT
		jsr	DO_LITERAL
		.word	10
		jmp	EMIT

; EMIT ( x -- )
;
; If x is a graphic character in the implementation-defined character set,
; display x. The effect of EMIT for all other values of x is implementation
; -defined.

		HEADER	4,"EMIT",NORMAL
		extern	UartTx
EMIT:
		lda	DSTACK+1,x	; Fetch character from stack
		jsr	UartTx		; .. and transmit
		inx			; Drop the character
		inx
		rts			; Done

; KEY ( -- char )
;
; Receive one character char, a member of the implementation-defined character
; set. Keyboard events that do not correspond to such characters are discarded
; until a valid character is received, and those events are subsequently
; unavailable.
;
; All standard characters can be received. Characters received by KEY are not
; displayed.

		HEADER	3,"KEY",NORMAL
		extern	UartRx
KEY:
		jsr	UartRx		; Receive a character
		dex
		dex
		sta	DSTACK+1,x
		lda	#0
		sta	DSTACK+2,x
		rts			; Done

; KEY? ( -- flag )
;
; If a character is available, return true. Otherwise, return false. If
; non-character keyboard events are available before the first valid character,
; they are discarded and are subsequently unavailable. The character shall be
; returned by the next execution of KEY.

		HEADER	4,"KEY?",NORMAL
		extern	UartRxTest
KEY_QUERY:
		jsr	UartRxTest		; Determine if any data is
		ldx	#0			; .. available
		bcc	$+3
		dex
		tdc				; And push to stack
		dec	a
		dec	a
		tcd
		stx	<1
		rts			   ; Done

; SPACE ( -- )
;
; Display one space.
;
; In this implementation it is defined as
;
;   BL EMIT

		HEADER	5,"SPACE",NORMAL
SPACE:
		jsr	BL
		jmp	EMIT

; SPACES ( n -- )
;
; If n is greater than zero, display n spaces.
;
; In this implementation it is defined as
;
;   BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP

		HEADER	6,"SPACES",NORMAL
SPACES:
SPACES_1:	jsr	DUP
		jsr	ZERO_GREATER
		jsr	QUERY_BRANCH
		.word	SPACES_2
		jsr	SPACE
		jsr	ONE_MINUS
		jsr	BRANCH
		.word	SPACES_1
SPACES_2:	jmp	DROP

; TYPE ( c-addr u -- )
;
; If u is greater than zero, display the character string specified by c-addr
; and u.
;
; In this implementation it is defined as
;
;   ?DUP IF
;     OVER + SWAP DO I C@ EMIT LOOP
;   ELSE DROP THEN

		HEADER	4,"TYPE",NORMAL
TYPE:
		jsr	QUERY_DUP
		jsr	QUERY_BRANCH
		.word	TYPE_2
		jsr	OVER
		jsr	PLUS
		jsr	SWAP
		jsr	DO_DO
TYPE_1:		jsr	I
		jsr	C_FETCH
		jsr	EMIT
		jsr	DO_LOOP
		.word	TYPE_1
		jsr	BRANCH
		.word	TYPE_3
TYPE_2		jsr	DROP
TYPE_3		rts

;===============================================================================
; Formatted Output
;-------------------------------------------------------------------------------

; # ( ud1 -- ud2 )
;
; Divide ud1 by the number in BASE giving the quotient ud2 and the remainder n.
; (n is the least-significant digit of ud1.) Convert n to external form and add
; the resulting character to the beginning of the pictured numeric output string.
; An ambiguous condition exists if # executes outside of a <# #> delimited
; number conversion.
;
;	BASE @ >R 0 R@ UM/MOD ROT ROT R> UM/MOD ROT ROT DUP 9 > 7 AND + 30 + HOLD

		HEADER	1,"#",NORMAL
HASH:
		jsr	BASE
		jsr	FETCH
		jsr	TO_R
		jsr	ZERO
		jsr	R_FETCH
		jsr	UM_SLASH_MOD
		jsr	ROT
		jsr	ROT
		jsr	R_FROM
		jsr	UM_SLASH_MOD
		jsr	ROT
		jsr	ROT
		jsr	DUP
		jsr	DO_LITERAL
		.word	9
		jsr	GREATER
		jsr	DO_LITERAL
		.word	7
		jsr	AND
		jsr	PLUS
		jsr	DO_LITERAL
		.word	'0'
		jsr	PLUS
		jmp	HOLD

; #> ( xd -- c-addr u )
;
; Drop xd. Make the pictured numeric output string available as a character
; string. c-addr and u specify the resulting character string. A program may
; replace characters within the string.
;
;	2DROP HP @ PAD OVER -

		HEADER	2,"#>",NORMAL
HASH_GREATER:
		jsr	TWO_DROP
		jsr	HP
		jsr	FETCH
		jsr	PAD
		jsr	OVER
		jmp	MINUS

; #S ( ud1 -- ud2 )
;
; Convert one digit of ud1 according to the rule for #. Continue conversion
; until the quotient is zero. ud2 is zero. An ambiguous condition exists if #S
; executes outside of a <# #> delimited number conversion.
;
;	BEGIN # 2DUP OR 0= UNTIL

		HEADER	2,"#S",NORMAL
HASH_S:
HASH_S_1:	jsr	HASH
		jsr	TWO_DUP
		jsr	OR
		jsr	ZERO_EQUAL
		jsr	QUERY_BRANCH
		.word	HASH_S_1
		rts
; . ( n -- )
;
; Display n in free field format.
;
;	<# DUP ABS 0 #S ROT SIGN #> TYPE SPACE

		HEADER	1,".",NORMAL
DOT:
		jsr	LESS_HASH
		jsr	DUP
		jsr	ABS
		jsr	ZERO
		jsr	HASH_S
		jsr	ROT
		jsr	SIGN
		jsr	HASH_GREATER
		jsr	TYPE
		jmp	SPACE

; <# ( -- )
;
; Initialize the pictured numeric output conversion process.
;
;	PAD HP !

		HEADER	2,"<#",NORMAL
LESS_HASH:	jsr	DO_COLON
		jsr	PAD
		jsr	HP
		jmp	STORE

; HOLD ( char -- )

; Add char to the beginning of the pictured numeric output string. An
; ambiguous condition exists if HOLD executes outside of a <# #> delimited
; number conversion.
;
;	-1 HP +!  HP @ C!

		HEADER	4,"HOLD",NORMAL
HOLD:
		jsr	FALSE
		jsr	HP
		jsr	PLUS_STORE
		jsr	HP
		jsr	FETCH
		jmp	C_STORE

; PAD ( -- c-addr )
;
; c-addr is the address of a transient region that can be used to hold data
; for intermediate processing.

		HEADER	3,"PAD",NORMAL
PAD:		
		jsr	DO_LITERAL
		.word	PAD_END
		rts

; SIGN ( n -- )
;
; If n is negative, add a minus sign to the beginning of the pictured numeric
; output string. An ambiguous condition exists if SIGN executes outside of a
; <# #> delimited number conversion.
;
;	[ HEX ] 0< IF 2D HOLD THEN

		HEADER	4,"SIGN",NORMAL
SIGN:
		jsr	ZERO_LESS
		jsr	QUERY_BRANCH
		.word	SIGN_1
		jsr	DO_LITERAL
		.word'-'
		jsr	HOLD
SIGN_1:		rts

; U. ( u -- )
;
; Display u in free field format.
;
;  <# 0 #S #> TYPE SPACE

		HEADER	2,"U.",NORMAL
U_DOT:
		jsr	LESS_HASH
		jsr	ZERO
		jsr	HASH_S
		jsr	HASH_GREATER
		jsr	TYPE
		jmp	SPACE

;===============================================================================
; Programming Tools
;-------------------------------------------------------------------------------

; .NYBBLE ( n -- )
;
; Print the least significant nybble of the top value on the stack in hex.

;		HEADER	7,".NYBBLE",NORMAL
DOT_NYBBLE:
		lda	DSTACK+1,x
		and	#$0f
		ora	#$30
		cmp	#$3a
		if	cs
		 adc	#$06
		endif
		jsr	UartTx
		jmp	DROP

; .BYTE ( n -- )
;
; Print least significant byte of top value on the stack in hex followed by
; a space.

		HEADER	5,".BYTE",NORMAL
DOT_BYTE:
		jsr	DUP
		jsr	DO_LITERAL
		.word	4
		jsr	RSHIFT
		jsr	DOT_NYBBLE
		jsr	DOT_NYBBLE
		jmp	SPACE

; .WORD ( n -- )
;
; Print the top value on the stack in hex followed by a space.

		HEADER	5,".WORD",NORMAL
DOT_WORD:
		jsr	DUP
		jsr	DO_LITERAL
		.word	12
		jsr	RSHIFT
		jsr	DOT_NYBBLE
		jsr	DUP
		jsr	DO_LITERAL
		.word	8
		jsr	RSHIFT
		jsr	DOT_NYBBLE
		jsr	DUP
		jsr	DO_LITERAL
		.word	4
		jsr	RSHIFT
		jsr	DOT_NYBBLE
		jsr	DOT_NYBBLE
		jmp	SPACE

; .DP

		HEADER	3,".DP",NORMAL
		jsr	AT_DP
		jmp	DOT_WORD

		HEADER	3,".RP",NORMAL
		jsr	AT_RP
		jmp	DOT_WORD

; .S ( -- )
;
; Copy and display the values currently on the data stack. The format of the
; display is implementation-dependent.

		HEADER	2,".S",NORMAL
		jsr	DO_LITERAL
		.word	'{'
		jsr	EMIT
		jsr	SPACE
		jsr	AT_DP
		jsr	ONE_PLUS
		jsr	DO_LITERAL
		.word	DSTACK_END
		jsr	SWAP
		jsr	QUERY_DO_DO
		.word	DOT_S_2
DOT_S_1:	jsr	I
		jsr	FETCH
		jsr	DOT_WORD
		jsr	DO_LITERAL
		.word	2
		jsr	DO_PLUS_LOOP
		jsr	DOT_S_1
DOT_S_2:	jsr	DO_LITERAL
		.word	'}'
		jsr	EMIT
		jmp	SPACE

; ? ( a-addr -- )
;
; Display the value stored at a-addr.

		HEADER	1,"?",NORMAL
		jsr	FETCH
		jmp	DOT_WORD

		HEADER	3,"@DP",NORMAL
AT_DP:
		txa
		dex
		dex
		sta	DSTACK+1,x
		lda	#0
		sta	DSTACK+2,x
		rts

		HEADER	3,"@RP",NORMAL
AT_RP:
		lda	RSP
		dex
		dex
		sta	DSTACK+1,x
		lda	#0
		sta	DSTACK+2,x
		rts

;===============================================================================
; I/O Page
;-------------------------------------------------------------------------------

		.org	$fe00
		.space	256

;===============================================================================
; UART Interface
;-------------------------------------------------------------------------------

; Inserts the byte in A into the transmit buffer. If the buffer is full then
; wait until some space is available. Registers A is preserved.

UartTx:
		pha
		
		ldy	TX_TAIL		; Save the data byte at the tail
		sta	TX_BUFF,y
		jsr	BumpTx		; Work out the next offset
		repeat			; And wait until save to store
		 cpy	TX_HEAD
		until	ne
		sty	TX_TAIL
		lda	#$05		; Ensure TX interrupt enabled
		sta	ACIA_CMND

		pla
		rts			; Done

;
;

UartRx:
		ldy	RX_HEAD		; Wait until there is some data
		repeat
		 cpy	RX_TAIL
		until	ne
		lda	RX_BUFF,y	; Then extract the head byte
		jsr	BumpRx		; Update the offset
		sty	RX_HEAD

		rts			; Done


;===============================================================================
; IRQ Handler
;-------------------------------------------------------------------------------

; Handle interrupts, currently just UART transmit buffer empty and receive
; buffer full.

IRQ:
		pha			; Save users registers
		txa
		pha
		tya
		pha
		
		tsx			; Check for BRK
		lda	STACK+4,x
		and	#$10
		if	ne
		 jmp	(BRKV)		; Redirect thru pseudo vector
		endif

;-------------------------------------------------------------------------------

		lda	ACIA_STAT	; ACIA is the source?
		if	mi

		 pha
		 and	#$10		; TX Buffer empty?
		 if	ne
		  ldy	TX_HEAD		; Any data to send?
		  cpy	TX_TAIL
		  if	ne
		   lda	TX_BUFF,y	; Yes, extract and send it
		   sta	ACIA_DATA
		   jsr	BumpTx
		   sty	TX_HEAD
		  else
		   lda	#$01		; No, disable TX interrupt
		   sta	ACIA_CMND
		  endif
		 endif

		 pla
		 and	#$08		; RX Buffer full?
		 if	ne
		  lda	ACIA_DATA	; Yes, fetch the character
		  ldy	RX_TAIL		; .. and save it
		  sta	RX_BUFF,y
		  jsr	BumpRx
		  cpy	RX_HEAD		; Is buffer completely full?
		  if	ne
		   sty	RX_TAIL		; No, update tail offset
		  endif
		 endif
		endif

;-------------------------------------------------------------------------------

		pla			; Restore user registers
		tay
		pla
		tax
		pla
NMI:
		rti			; Done

;-------------------------------------------------------------------------------

; Bump and wrap a receive buffer index value.

BumpRx:
		.if	RX_SIZE != TX_SIZE
		iny			; Increase the index
		cpy	#RX_SIZE	; Reached the limit?
		if	eq
		 ldy	#0		; Yes, wrap around
		endif
		rts			; Done
		.endif

; Bump and wrap a transmit buffer index value.

BumpTx:
		iny			; Increase the index
		cpy	#TX_SIZE	; Reached the limit?
		if	eq
		 ldy	#0		; Yes, wrap around
		endif
		rts			; Done

;===============================================================================
; Vector Locations
;-------------------------------------------------------------------------------

		.org	$FFF8

BRKV:		.word	NMI		; BRK
		.word	NMI		; NMI
		.word	RESET		; RESET
		.word	IRQ		; IRQ/BRK

		.end