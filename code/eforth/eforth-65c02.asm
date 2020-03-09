;===============================================================================
;       _____          _   _        __  ____   ____ ___ ____  
;   ___|  ___|__  _ __| |_| |__    / /_| ___| / ___/ _ \___ \ 
;  / _ \ |_ / _ \| '__| __| '_ \  | '_ \___ \| |  | | | |__) |
; |  __/  _| (_) | |  | |_| | | | | (_) |__) | |__| |_| / __/ 
;  \___|_|  \___/|_|   \__|_| |_|  \___/____/ \____\___/_____|
;                                                                                              
; eForth for the 65C02
;-------------------------------------------------------------------------------
; Copyright (C),2020 Andrew Jacobs
; All rights reserved.
;
; This work is made available under the terms of the Creative Commons
; Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
; following URL to see the details.
;
; http://creativecommons.org/licenses/by-nc-sa/4.0/
;
;===============================================================================
; Notes:
;
; This is a slightly extended version of Dr C H Ting's eForth ported to the
; 65(S)C02 microprocessor.
;
; In this implementation the Forth instruction pointer is incremented before it
; is accessed rather than afterwards as this better suits the 65(C)02 and word
; addresses are stored -1 as they invoked with an RTS instruction.
;-------------------------------------------------------------------------------

		.include "../sb-6502.inc"
		
		.65C02

DSTACK		.equ	$0000
DSTACK_TOP	.equ	$00df	;fd

X_TEMP		.equ	$00fe		; Storage for X in critcal sections

RSTACK		.equ	$0100
RSTACK_TOP	.equ	$01ff

;===============================================================================
; Macros
;-------------------------------------------------------------------------------

PREV		.set	0

NORMAL		.equ	$00
IMMEDIATE	.equ	$80

HEADER		.macro	LEN,NAME,TYPE
THIS		.set	$
		.word	PREV
		.byte	LEN,NAME
		.byte	TYPE
PREV		.set	THIS
		.endm
		
;-------------------------------------------------------------------------------
		
COLON		.macro
		jmp	doLIST
		.endm
		
VARIABLE	.macro
		jsr	doVAR
		.endm
		
VOCAB		.macro
		jsr	doVOC
		.endm
		
USER		.macro
		jsr	doUSER
		.endm
		
LITERAL		.macro	VALUE
		.word	doLIT-1
		.word	VALUE
		.endm
		
;-------------------------------------------------------------------------------
		
LINK		.macro	WORD
		.word	WORD-1
		.endm
		
JZERO		.macro	ADDR
		.word	QBRANCH-1
		.word	ADDR-2
		.endm
		
JUMP		.macro	ADDR
		.word	BRANCH-1
		.word	ADDR-2
		.endm
		
NEXT		.macro	ADDR
		.word	doNEXT-1
		.word	ADDR-2
		.endm

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.page0
		.org	$00

IP		.space	2		; Forth instruction pointer -2 

		.bss
		.org	$0300

_UP		.space	2		; User Area pointer

USER_AREA	.space	32		; User variables


;===============================================================================
; Startup
;-------------------------------------------------------------------------------

		.code
		.org	$c000
		
		lda	#<TEST+1 ;COLD+1
		sta	IP+0
		lda	#>TEST+1 ;COLD+1
		sta	IP+1
		ldx	#<DSTACK_TOP
		jmp	INNER

;===============================================================================
; 2.1 System Interface
;-------------------------------------------------------------------------------

; BYE ( -- )
;
; Restart the CPU and enter the monitor

		HEADER	3,"bye",NORMAL
BYE:
		jmp	($fffc)		; Simulate a RESET

; ?RX ( -- c T | F )
;
; Return input character and true, or a false if no input.

		HEADER	3,"?rx", NORMAL
RX:
		jsr	UARTRXCOUNT	; Any data in the RX buffer?
		if ne
		 jsr	UARTRX		; Yes, read and push it
		 dex
		 dex
		 sta	DSTACK+1,x
		 stz	DSTACK+2,x
		 lda	#$ff		; Set flag true
		endif
		dex			; Push flag
		dex
		sta	DSTACK+1,x
		sta	DSTACK+2,x
		bra	INNER		; Done

; TX! ( c -- )
;
; Send character c to output device.

		HEADER	3,"tx!",NORMAL
TX:
		lda	DSTACK+1,x	; Transmit TOS characater
		jsr	UARTTX
		inx			; .. then drop from stack
		inx
		bra	INNER
		
; !IO ( -- )
;
; Initialize the serial I/O devices.

		HEADER 	3,"!io",NORMAL
IO:
		bra	INNER		; Nothing to do in this implemenation

;===============================================================================
; 2.2 Inner Interpreter
;-------------------------------------------------------------------------------

; doList ( -- a )
;
; Run address list in a colon word.
	
doLIST:
		lda	IP+1		; Save the old IP
		pha
		lda	IP+0
		pha
		
		ldy	#1		; Fetch target again
		lda	(IP),y
		tay
		clc
		lda	(IP)
		adc	#2		; And convert to new IP-2
		if cs
		 iny
		endif
		sta	IP+0		; And save it
		sty	IP+1

; Execute the next word in a word list

INNER:
		clc			; Increment the instruction pointer
		lda	#2
		adc	IP+0
		sta	IP+0
		if cs
		 inc	IP+1
		endif
		ldy	#1		; Fetch the target word address - 1
		lda	(IP),y
		pha
		lda	(IP)
		pha
		rts 			; Then invoke the word

; EXIT R: ( a -- )
;
; Terminate a colon definition.

		HEADER	4,"EXIT",NORMAL
EXIT:
		pla			; Pull the old IP from
		sta	IP+0
		pla
		sta	IP+1
		bra	INNER		; And continue
		
; EXECUTE ( ca -- )
;
; Execute the word at ca.

		HEADER	6,"EXECUTE",NORMAL
EXECUTE:
		ldy	DSTACK+2,x	; Prepare target address
		lda	DSTACK+1,x	
		if eq
		 dey
		endif
		dec	a
		phy			; .. to be used by an RTS
		pha
		inx			; Clean up data stack
		inx
		rts			; Enter the target code
		
; doLIT ( -- w )
;
; Push inline literal on data stack.

doLIT:
		dex			; Make space on data stack
		dex
		ldy	#3		; Copy literal to stack
		lda	(IP),y
		sta	DSTACK+2,x
		dey
		lda	(IP),y
		sta	DSTACK+1,x
		clc			; Adjust IP to skip literal
		tya
		adc	IP+0
		sta	IP+0
		if cs
		 inc	IP+1
		endif
		bra	INNER		; And continue
		
;===============================================================================
; 2.3 Loops and Branches
;-------------------------------------------------------------------------------

; doNEXT ( -- )
;
; Decrement index and exit loop

doNEXT:
		pla			; Fetch the index from return stack
		ply
		cmp	#0		; Was the last index zero?
		if eq
		 cpy	#0
		 bra	SKIP		; Yes, skip over target address
		 dey
		endif
		dec	a
		phy			; Replace with new value
		pha
		bra	BRANCH		; And branch back to start of loop
		
; ?branch ( f -- )
		
QBRANCH:
		lda	DSTACK+1,x	; Check if TOS is zero
		ora	DSTACK+2,x
		inx
		inx
		tay			; .. and branch if it is
		beq	BRANCH
SKIP:
		clc			; Otherwise skip over address
		lda	#2
		adc	IP+0
		sta	IP+0
		if cs
		 inc	IP+1
		endif
		bra	INNER		; And continue
		
; branch ( -- )

BRANCH:
		ldy	#3		; Fetch the target IP-2
		lda	(IP),Y
		pha
		dey
		lda	(IP),Y
		sta	IP+0		; And place in IP
		pla
		sta	IP+1
		jmp	INNER
		
;===============================================================================
; 2.4 Memory Access
;-------------------------------------------------------------------------------

; ! ( w a -- )
;
; Pop the data stack to memory.

		HEADER	1,"!",NORMAL
STORE:
		lda	DSTACK+3,x	; Save LSB
		sta	(DSTACK+1,x)
		inc	DSTACK+1,x	; Bump memory address
		if eq
		 inc	DSTACK+2,x
		endif
		lda	DSTACK+4,x	; Save MSB
		sta	(DSTACK+1,x)
		inx			; Drop address and data
		inx
		inx
		inx
		jmp	INNER
		
; @ ( a -- w )
;
; Push memory location to data stack.
		
		HEADER 	1,"@",NORMAL
FETCH:
		lda	(DSTACK+1,x)	; Load LSB
		tay
		inc	DSTACK+1,x	; Bump memory address
		if eq
		 inc	DSTACK+2,x
		endif
		lda	(DSTACK+1,x)	; Load MSB
		sty	DSTACK+1,x	; Replace address with data
		sta	DSTACK+2,x
		jmp	INNER
		
; C! ( c a -- )
;
; Pop data stack to byte memory.

		HEADER 	2,"c!",NORMAL
C_STORE:
		lda	DSTACK+3,x
		sta	(DSTACK+1,x)
		inx
		inx
		inx
		inx
		jmp	INNER
		
; C@ ( a -- c )
;
; Push byte memory content on data stack.

		HEADER 	2,"c@",NORMAL
C_FETCH:
		lda	(DSTACK+1,x)
		sta	DSTACK+1,x
		stz	DSTACK+2,x
		jmp	INNER
		
;===============================================================================
; 2.5 Return Stack
;-------------------------------------------------------------------------------

; RP@ ( -- a )
;
; Push current RP to data stack.

		HEADER	3,"rp@",NORMAL
RP_FETCH:
		php			; Start critical section
		sei
		stx	X_TEMP		; Save data stack pointer
		tsx			; .. And fetch the return stack pointer
		inx			; Adjust for flags saved earlier
		txa
		ldx	X_TEMP		; Reload data stack pointer
		plp
		ldy	#$01		; MSB is always page one
		dex			; Push return stack pointer
		dex
		sta	DSTACK+1,x		
		sty	DSTACK+2,x
		jmp	INNER		; Continue
		
; RP! ( w -- )
;
; Set the return stack pointer.

		HEADER	3,"rp!",NORMAL
RP_STORE:
		php			; Start critical section
		sei
		stx	X_TEMP		; Save data stack pointer
		lda	DSTACK+1,x	; Get LSB of new return stack 
		tax			; .. and install in SP
		txs
		lda	X_TEMP		; Reload data stack pointer
		plp
		inx			; Clear up data stack
		inx
		jmp	INNER		; Continue
	
; r> ( -- w )
;
; Pop return stack to data stack.
	
		HEADER	2,"r>",NORMAL
R_FROM:
		pla
		ply
		dex
		dex
		sta	DSTACK+1,x
		sty	DSTACK+2,x
		jmp	INNER

; R@ ( -- w )
;
; Copy top of return stack to data stack.

		HEADER	2,"r@",NORMAL
R_FETCH:
		pla			; Fetch a copy of return TOS
		ply
		phy
		pha
		dex			; Then push a copy to data stack
		dex
		sta	DSTACK+1,x
		sty	DSTACK+2,x
		jmp	INNER		; Done.
		
; >R ( w -- )
;
; Push data stack to return stack.

		HEADER	2,">r",NORMAL
TO_R:
		lda	DSTACK+1,x	; Fetch the data stack TOS
		ldy	DSTACK+2,x
		phy			; And push to return stack
		pha
		inx
		inx
		jmp	INNER
		
;===============================================================================
; 2.6 Data Stack
;-------------------------------------------------------------------------------

; DROP ( w -- )
;
; Discard top stack item.

		HEADER	4,"drop",NORMAL
DROP:
		inx
		inx
		jmp	INNER

; DUP ( w -- w w )
;
; Duplicate the top stack item.

		HEADER	3,"dup",NORMAL
DUP:
		dex
		dex
		lda	DSTACK+3,x
		sta	DSTACK+1,x
		lda	DSTACK+4,x
		sta	DSTACK+2,x
		jmp	INNER

; SWAP ( w1 w2 -- w2 w1 )
;
; Exchange top two stack items.

		HEADER	4,"swap",NORMAL
SWAP:
		lda	DSTACK+1,x
		ldy	DSTACK+3,x
		sty	DSTACK+1,x
		sta	DSTACK+3,x
		lda	DSTACK+2,x
		ldy	DSTACK+4,x
		sty	DSTACK+2,x
		sta	DSTACK+4,x
		jmp	INNER
	
; OVER ( w1 w2 -- w1 w2 w1 )
;
; Copy second stack item to top.

		HEADER	4,"over",NORMAL
OVER:
		dex
		dex
		lda	DSTACK+3,x
		ldy	DSTACK+4,x
		sta	DSTACK+1,x
		sty	DSTACK+2,x
		jmp	INNER

; SP@ ( -- a )
;
; Push the current data stack pointer.

		HEADER	3,"sp@",NORMAL
SP_FETCH:
		txa
		ldy	#$01
		dex
		dex
		sta	DSTACK+1,x
		sty	DSTACK+2,x
		jmp	INNER
		
; SP! ( a -- )
;
; Set the data stack pointer.
		
		HEADER	3,"sp!",NORMAL
SP_STORE:
		lda	DSTACK+1,x
		tax
		jmp	INNER
		
;===============================================================================
; 2.7 Logical Words
;-------------------------------------------------------------------------------

; 0< ( w -- f)
;
; Return true if n is negative.

		HEADER	2,"0<",NORMAL
ZERO_LESS:
		ldy	#0		; Assume TOS is positive
		lda	DSTACK+2,x
		if mi
		 dey			; .. and correct if not
		endif
		sty	DSTACK+1,x	; Overwrite TOS with result
		sty	DSTACK+2,x
		jmp	INNER		; Done

; AND ( w w -- w )
;
; Bitwise AND.
	
		HEADER	3,"and",NORMAL
AND:
		lda	DSTACK+1,x
		and	DSTACK+3,x
		sta	DSTACK+3,x
		lda	DSTACK+2,x
		and	DSTACK+4,x
		sta	DSTACK+4,x
		inx
		inx
		jmp	INNER

; OR ( w w -- w )
;
; Bitwise inclusive OR.
	
		HEADER	2,"or",NORMAL
OR:
		lda	DSTACK+1,x
		ora	DSTACK+3,x
		sta	DSTACK+3,x
		lda	DSTACK+2,x
		ora	DSTACK+4,x
		sta	DSTACK+4,x
		inx
		inx
		jmp	INNER

; XOR ( w w -- w )
;
; Bitwise exclusive OR.
	
		HEADER	3,"xor",NORMAL
XOR:
		lda	DSTACK+1,x
		eor	DSTACK+3,x
		sta	DSTACK+3,x
		lda	DSTACK+2,x
		eor	DSTACK+4,x
		sta	DSTACK+4,x
		inx
		inx
		jmp	INNER
		
;===============================================================================
; 2.8 Primitive Arithmetic
;-------------------------------------------------------------------------------

; UM+ ( w w -- w c )
;
; Add two numbers, return the sum and carry flag.

		HEADER	3,"um+",NORMAL
UM_PLUS:
		clc
		lda	DSTACK+3,x	; Add LSBs
		adc	DSTACK+1,x
		sta	DSTACK+3,x
		lda	DSTACK+4,x	; Add MSBs
		adc	DSTACK+2,x
		sta	DSTACK+4,x
		lda	#0		; Create carry
		sta	DSTACK+2,x
		adc	#0
		sta	DSTACK+1,x
		jmp	INNER

; 1+ ( w -- w+1 )
;
; Adds one to the top value on the data stack, Not in eForth but very useful in
; this implementation.

		HEADER	2,"1+",NORMAL
ONE_PLUS:
		inc	DSTACK+1,x
		if eq
		 inc	DSTACK+2,x
		endif
		jmp	INNER
		
; 1- (w -- w-1 )
;
; Subtracts one from the top value on the data stack. Not in eForth but very
; useful in this implementation.

		HEADER	2,"1-",NORMAL
ONE_MINUS:
		lda	DSTACK+1,x
		if eq
		 dec	DSTACK+2,x
		endif
		dec	DSTACK+1,x
		jmp	INNER
		
; == ONLY HIGH LEVEL WORDS FROM THIS POINT ON ==
		
;===============================================================================
; 3.1 Variables and User Variables
;-------------------------------------------------------------------------------
; doVar, doUSER and doVOC are invoked with a JSR which puts the return address
; minus one on the return stack so an additional 1+ is needed in this version.

		HEADER	2,"up",NORMAL
UP:		VARIABLE
		.word	0

; doVAR ( -- a )

doVAR:		COLON
		LINK	R_FROM
		LINK	ONE_PLUS
		LINK	EXIT
		
; doUSER ( -- a )
		
doUSER:		COLON
		LINK	R_FROM
		LINK	ONE_PLUS
		LINK	FETCH
		LINK	UP
		LINK	FETCH
		LINK	PLUS
		LINK 	EXIT

; doVOC ( -- )

doVOC:		COLON
		LINK	R_FROM
		LINK	ONE_PLUS
		LINK	CONTEXT
		LINK	STORE
		LINK	EXIT
		
; FORTH ( -- )

FORTH:		VOCAB
		.word	0
		.word	0
		
;-------------------------------------------------------------------------------

; SP0 ( -- a )		
		HEADER	3,"sp0",NORMAL
SP0:		USER
		.word	0
		
; RP0 ( -- a )

		HEADER	3,"rp0",NORMAL
RP0:		USER
		.word	2
		
; '?KEY ( -- a )

		HEADER	5,"'?key",NORMAL
TICK_QKEY:	USER
		.word	4
		
; 'EMIT ( -- a )

		HEADER	5, "'emit",NORMAL
TICK_EMIT:	USER
		.word	6
		
;'expect
		.word	8
;'tap
		.word	10
;'echo
		.word	12
;'prompt
		.word	14

; BASE ( -- a )

		HEADER	4,"base",NORMAL
BASE:		USER
		.word	16

; tmp
		.word	18
; SPAN
		.word	20
		
; >IN
		.word	22
		
; #TIB ( -- a )

		HEADER	4,"#tib",NORMAL
HASH_TIB:	USER
		.word	24
		
; CSP ( -- a )

		HEADER	3,"csp",NORMAL
CSP:		USER
		.word	26
		
; 'EVAL
		.word	28
; 'NUMBER
		.word	30
; HLD
		.word	32
; HANDLER
		.word	34
	
; CONTEXT ( -- a )

		HEADER	7,"context",NORMAL
CONTEXT:	USER
		.word	36

; CURRENT ( -- a )

		HEADER	7,"current",NORMAL
CURRENT:	USER
		.word	56
; CP ( -- a )

		HEADER	2,"cp",NORMAL
CP:		USER
		.word	60
		
; NP ( -- a )

		HEADER	2,"np",NORMAL
NP:		USER
		.word	62

; LAST ( -- a )

		HEADER	4,"last",NORMAL
LAST:		USER
		.word	64

;===============================================================================
; Common Functions
;-------------------------------------------------------------------------------

		HEADER	5,"false",NORMAL
FALSE:		COLON
		LITERAL	0
		LINK	EXIT
		
		HEADER	4,"true",NORMAL
TRUE:		COLON
		LITERAL	-1
		LINK	EXIT


; ?DUP ( w -- w w )

		HEADER	4,"?dup",NORMAL
QDUP:		COLON
		LINK	DUP		; DUP
		JZERO	.Skip		; IF
		LINK	DUP		; DUP
.Skip		LINK 	EXIT		; THEN ;

		HEADER 	3,"rot",NORMAL
ROT:		COLON
		LINK	TO_R
		LINK	SWAP
		LINK	R_FROM
		LINK	SWAP
		LINK	EXIT

		HEADER	5,"2drop",NORMAL
TWO_DROP:	COLON
		LINK	DROP
		LINK	DROP
		LINK	EXIT
		
		HEADER	4,"2dup",NORMAL
TWO_DUP:	COLON
		LINK	OVER
		LINK	OVER
		LINK	EXIT
		
		HEADER	1,"+",NORMAL
PLUS:		COLON
		LINK	UM_PLUS
		LINK	DROP
		LINK	EXIT
		
		HEADER	3,"not",NORMAL
NOT:		COLON
		LINK	TRUE
		LINK	XOR
		LINK	EXIT
	
		HEADER	6,"negate",NORMAL
NEGATE:		COLON
		LINK	NOT
		LINK	ONE_PLUS
		LINK	EXIT
	
		HEADER	7,"dnegate",NORMAL
DNEGATE:	COLON
		LINK	NOT
		LINK	TO_R
		LINK	NOT
		LITERAL	1
		LINK	UM_PLUS
		LINK	R_FROM
		LINK	PLUS
		LINK	EXIT
		
		HEADER	2,"d+",NORMAL
D_PLUS:		COLON
		LINK	TO_R
		LINK	SWAP
		LINK	TO_R
		LINK	UM_PLUS
		LINK	R_FROM
		LINK	R_FROM
		LINK	PLUS
		LINK	PLUS
		LINK 	EXIT
	
		HEADER	1,"-",NORMAL
MINUS:		COLON
		LINK	NEGATE
		LINK	PLUS
		LINK	EXIT
	
		HEADER	3,"abs",NORMAL
ABS:		COLON
		LINK	DUP
		LINK	ZERO_LESS
		JZERO	.Skip
		LINK	NEGATE
.Skip:		LINK	EXIT

;===============================================================================
; Comparison
;-------------------------------------------------------------------------------

		HEADER	1,"=",NORMAL
EQUAL:		COLON
		LINK	XOR
		JZERO	.Then
		LINK	FALSE
		LINK	EXIT
.Then:		LINK	TRUE
		LINK 	EXIT
		
		HEADER	2,"u<",NORMAL
U_LESS:		COLON
		LINK	TWO_DUP
		LINK	XOR
		LINK	ZERO_LESS
		JZERO	.Then
		LINK	SWAP
		LINK	DROP
		LINK	ZERO_LESS
		LINK	EXIT
.Then:		LINK	MINUS
		LINK	ZERO_LESS
		LINK 	EXIT
		
; < ( w w -- f )
		
		HEADER	1,"<",NORMAL
LESS:		COLON
		LINK	TWO_DUP
		LINK	XOR
		LINK	ZERO_LESS
		JZERO	.Then
		LINK	DROP
		LINK	ZERO_LESS
		LINK	EXIT
.Then:		LINK	MINUS
		LINK	ZERO_LESS
		LINK 	EXIT
		
; MAX ( w w -- w )
		
		HEADER	3,"max",NORMAL
MAX:		COLON
		LINK	TWO_DUP
		LINK	LESS
		JZERO	.Then
		LINK	SWAP
.Then		LINK	DROP
		LINK	EXIT
		
; MIN ( w w -- w )
		
		HEADER	3,"min",NORMAL
MIN:		COLON
		LINK	TWO_DUP
		LINK	SWAP
		LINK	LESS
		JZERO	.Then
		LINK	SWAP
.Then		LINK	DROP
		LINK	EXIT
		
; WITHIN 
		
		HEADER	6,"within",NORMAL
WITHIN:		COLON
		LINK	OVER
		LINK	MINUS
		LINK	TO_R
		LINK	MINUS
		LINK	R_FROM
		LINK	U_LESS
		LINK	EXIT

;===============================================================================
; Divide
;-------------------------------------------------------------------------------
		
; UM/MOD
; M/MOD
; /MOD
; MOD
; /
		
;===============================================================================
; Multiply
;-------------------------------------------------------------------------------

		HEADER	3,"um*",NORMAL
UM_TIMES:	COLON	


		LINK	EXIT
		
	
		HEADER	1,"*",NORMAL
TIMES:		COLON
		LINK	UM_TIMES
		LINK	DROP
		LINK	EXIT

; M*
; */MOD
; */

	
;===============================================================================
; Memory Allocation
;-------------------------------------------------------------------------------

		HEADER	5,"cell-",NORMAL
CELL_MINUS:	COLON
		LITERAL -2
		LINK	PLUS
		LINK	EXIT
		
		HEADER	5,"cell+",NORMAL
CELL_PLUS:	COLON
		LITERAL 2
		LINK	PLUS
		LINK	EXIT

		HEADER	5,"cells",NORMAL
CELLS:		COLON
		LITERAL 2
		LINK	TIMES
		LINK	EXIT
	
; 6502 family devices are not alignment sensitive.

		HEADER	7,"aligned",NORMAL
ALIGNED:	COLON
		LINK	EXIT

		HEADER	2,"bl",NORMAL
BL:		COLON
		LITERAL	' '
		LINK	EXIT

; >CHAR
; DEPTH
; PICK

;===============================================================================
; Memory Access
;-------------------------------------------------------------------------------

; +! ( n a -- )
		HEADER	2,"+!",NORMAL
PLUS_STORE:	COLON
		LINK	SWAP		; SWAP
		LINK	OVER		; OVER
		LINK	FETCH		; @
		LINK	PLUS		; +
		LINK	SWAP		; SWAP
		LINK 	STORE		; !
		LINK	EXIT		; ;
		
; 2! ( d a -- )

		HEADER	2,"2!",NORMAL
TWO_STORE:	COLON
		LINK	SWAP		; SWAP
		LINK	OVER		; OVER
		LINK	STORE		; !
		LINK	CELL_PLUS	; CELL+
		LINK	STORE		; !
		LINK	EXIT		; ;
		
; 2@ ( a -- d )

		HEADER	2,"2@",NORMAL
TWO_FETCH:	COLON
		LINK	DUP		; DUP
		LINK	CELL_PLUS	; CELL+
		LINK	FETCH		; @
		LINK	SWAP		; SWAP
		LINK	FETCH		; @
		LINK	EXIT		; ;
		
; COUNT ( b -- b +n )

		HEADER	5,"count",NORMAL
COUNT:		COLON
		LINK	DUP		; DUP
		LINK	ONE_PLUS	; 1 +
		LINK	SWAP		; SWAP
		LINK	C_FETCH		; C@
		LINK	EXIT		; ;

; HERE ( -- a )

		HEADER	4,"here",NORMAL
HERE:		COLON
		LINK	CP		; CP
		LINK	FETCH		; @
		LINK	EXIT		; ;

; PAD ( -- a )

		HEADER	3,"pad",NORMAL
PAD:		COLON
		LINK	HERE		; HERE
		LITERAL	80		; 80
		LINK	PLUS		; +
		LINK	EXIT		; ;

; TIB ( -- a )

		HEADER	3,"tib",NORMAL
TIB:		COLON
		LINK	HASH_TIB	; #TIB
		LINK	CELL_PLUS	; CELL+
		LINK 	FETCH		; @
		LINK	EXIT		; ;

; @EXECUTE ( a -- )

		HEADER	8,"@execute",NORMAL
AT_EXECUTE:	COLON
		LINK	FETCH		; @
		LINK	QDUP		; ?DUP
		JZERO	.Then		; IF
		LINK	EXECUTE		; EXECUTE THEN
.Then:		LINK	EXIT		; ;

; CMOVE ( b b u -- )

		HEADER	5,"cmove",NORMAL
CMOVE:		COLON
		LINK	TO_R		; FOR
		JUMP	.Then		; AFT
.Loop:		LINK	TO_R		; >R
		LINK	DUP		; DUP
		LINK	C_FETCH		; C@
		LINK	R_FETCH		; R@
		LINK	C_STORE		; C!
		LINK	ONE_PLUS	; 1 + THEN
.Then		NEXT	.Loop		; NEXT
		LINK	TWO_DROP	; 2DROP
		LINK	EXIT		; ;

; FILL ( b u c -- )

		HEADER	4,"fill",NORMAL
FILL:		COLON
		LINK	SWAP		; SWAP
		LINK	TO_R		; FOR
		LINK	SWAP		; SWAP
		JUMP	.Then		; AFT
.Loop:		LINK	TWO_DUP		; 2DUP
		LINK	C_STORE		; C!
		LINK	ONE_PLUS	; 1 + THEN
.Then:		NEXT	.Loop		; NEXT
		LINK	TWO_DROP	; 2DROP
		LINK	EXIT		; ;
		
; -TRAILING
; PACK$

;===============================================================================
; 4.1 Number Formatting
;-------------------------------------------------------------------------------

; DIGIT ( u -- c )
		HEADER	5,"digit",NORMAL
DIGIT:		COLON
		LITERAL	9		; 9
		LINK	OVER		; OVER
		LINK	LESS		; <
		LITERAL	7		; 7
		LINK	AND		; AND
		LINK	PLUS		; +
		LITERAL	'0'		; [CHAR] 0
		LINK	PLUS		; +
		LINK	EXIT		; ;
		
; EXTRACT ( n base -- b c )

		HEADER	7,"extract",NORMAL
EXTRACT:	COLON
		LINK	FALSE		; 0
		LINK	SWAP		; SWAP
		LINK	UM_MOD		; UM/MOD
		LINK	SWAP		; SWAP
		LINK	DIGIT		; DIGIT
		LINK	EXIT
		
		HEADER	2,"<#",NORMAL
LESS_HASH:	COLON
		LINK	PAD		; PAD
		LINK	HLD		; HLD
		LINK	STORE		; !
		LINK	EXIT		; ;
		
		HEADER	4,"hold",NORMAL
HOLD:		COLON
		LINK	HLD		; HLD
		LINK	FETCH		; @
		LINK	ONE_MINUS	; 1 -
		LINK	DUP		; DUP
		LINK	HLD		; HLD
		LINK	STORE		; !
		LINK	C_STORE		; C!
		LINK	EXIT		; ;
	
		HEADER	1,"#",NORMAL
HASH:		COLON
		LINK	BASE		; BASE
		LINK	FETCH		; @
		LINK	EXTRACT		; EXTRACT
		LINK	HOLD		; HOLD
		LINK	EXIT		; ;

		HEADER	2,"#s",NORMAL
HASH_S:		COLON
.Begin:		LINK	HASH		; BEGIN #
		LINK	DUP		; DUP
		JZERO	.Done		; WHILE
		JUMP	.Begin		; REPEAT	???
.Done		LINK	EXIT		; ;

		HEADER	4,"sign",NORMAL
SIGN:		COLON
		LINK	ZERO_LESS	; 0<
		JZERO	.Then		; IF
		LITERAL	'-'		; [CHAR] -
		LINK	HOLD		; HOLD
.Then:		LINK	EXIT		; THEN ;

		HEADER	3,"#>",NORMAL
HASH_GREATER:	COLON
		LINK	DROP		; DROP
		LINK	HLD		; HLD
		LINK	FETCH		; @
		LINK	PAD		; PAD
		LINK	OVER		; OVER
		LINK	MINUS		; -
		LINK	EXIT		; ;
		
;===============================================================================
; 4.2 Numeric Output
;-------------------------------------------------------------------------------
		
; str ( n -- b u )
;
; Convert a signed integer to a numeric string.

		HEADER	3,"str",NORMAL
STR:		COLON
		LINK	DUP		; DUP
		LINK	TO_R		; >R
		LINK	ABS		; ABS
		LINK	LESS_HASH	; <#
		LINK	HASH_S		; #S
		LINK	R_FROM		; R>
		LINK	SIGN		; SIGN
		LINK	HASH_GREATER	; #>
		LINK	EXIT		; ;

; HEX ( -- )
;
; Use radix 16 as base for numeric conversions.

		HEADER	3,"hex",NORMAL
HEX:		COLON
		LITERAL	16		; 16
		LINK	BASE		; BASE
		LINK	STORE		; !
		LINK	EXIT		; ;
		
; DECIMAL ( -- )
;
; Use radix 10 as base for numeric conversions.

		HEADER	7,"decimal",NORMAL
DECIMAL:	COLON
		LITERAL	10		; 10
		LINK	BASE		; BASE
		LINK	STORE		; !
		LINK	EXIT		; ;

; .R ( n +n -- )
;
; Use radix 16 as base for numeric conversions.

		HEADER	2,".r",NORMAL
DOT_R:		COLON
		LINK	TO_R		; >R
		LINK	STR		; STR
		LINK	R_FROM		; R>
		LINK	OVER		; OVER
		LINK	MINUS		; -
		LINK	SPACES		; SPACES
		LINK	TYPE		; TYPE
		LINK	EXIT		; ;
		
; U.R
; U.
; .
; ?

;===============================================================================
; Numeric Input
;-------------------------------------------------------------------------------

; DIGIT?
; NUMBER?

;===============================================================================
; Basic I/O
;-------------------------------------------------------------------------------

; ?KEY ( -- c T | F )
		HEADER	4,"?key",NORMAL
QKEY:		COLON
		LINK	TICK_QKEY	; ?QKEY
		LINK	AT_EXECUTE	; @EXECUTE
		LINK	EXIT		; ;
; KEY ( -- c )

		HEADER	3,"key",NORMAL
KEY:		COLON
.Loop:		LINK	QKEY		; BEGIN ?KEY
		JZERO	.Loop		; UNTIL
		LINK	EXIT		; ;

; EMIT ( c -- )

		HEADER	4,"emit",NORMAL
EMIT:		COLON
		LINK	TICK_EMIT	; 'EMIT
		LINK	AT_EXECUTE	; @EXECUTE
		LINK	EXIT		; ;

; NUF? ( -- f )
;
; The definition in Dr Ting's document looks wrong. 2DROP KEY => DROP 

		HEADER	4,"nuf?",NORMAL
NUF:		COLON
		LINK	QKEY		; ?KEY
		LINK	DUP		; DUP
		JZERO	.Then		; IF
		LINK	DROP		; DROP
		LITERAL	13		; 13
		LINK	EQUAL		; =
.Then:		LINK	EXIT		; THEN ;

; PACE ??

; SPACE ( -- )

		HEADER	5,"space",NORMAL
SPACE:		COLON
		LINK	BL		; BL
		LINK	EMIT		; EMIT
		LINK	EXIT		; ;

; CHARS ( +n c -- )

		HEADER	5,"chars",NORMAL
CHARS:		COLON
		LINK	SWAP		; SWAP
		LITERAL	0		; 0
		LINK	MAX		; MAX
		LINK	TO_R		; FOR
		JUMP	.Then		; AFT
.Aft:		LINK	DUP		; DUP
		LINK	C_FETCH		; C@
		LINK	EMIT		; EMIT
		LINK	ONE_PLUS	; 1 +
.Then:		NEXT	.Aft		; THEN NEXT
		LINK	DROP		; DROP
		LINK	EXIT		; ;

; SPACES ( +n -- )

		HEADER	6,"spaces",NORMAL
SPACES:		COLON
		LINK	BL		; BL
		LINK	CHARS		; CHARS
		LINK	EXIT		; ;
		
; TYPE ( b u -- )

		HEADER	4,"type",NORMAL
TYPE:		COLON
		LINK	TO_R		; FOR
		JUMP	.Then		; AFT
.Aft:		LINK	DUP		; DUP
		LINK	C_FETCH		; C@
		LINK	EMIT		; EMIT
		LINK	ONE_PLUS	; 1 +
.Then:		NEXT	.Aft		; THEN NEXT
		LINK	DROP		; DROP
		LINK	EXIT		; ;
		
; CR ( -- )

		HEADER	2,"cr",NORMAL
CR:		COLON
		LITERAL	13		; 13
		LINK	EMIT		; EMIT
		LITERAL	10		; 10
		LINK	EMIT		; EMIT
		LINK	EXIT		; ;

;===============================================================================
; Parsing
;-------------------------------------------------------------------------------

; parse

;===============================================================================
; Dictionary Search
;-------------------------------------------------------------------------------


;===============================================================================
; Terminal
;-------------------------------------------------------------------------------


;===============================================================================
; Error Handling
;-------------------------------------------------------------------------------

; CATCH
; THROW

;===============================================================================
; Text Interpreter
;-------------------------------------------------------------------------------


;===============================================================================
; Shell
;-------------------------------------------------------------------------------

		HEADER	6,"preset",NORMAL
PRESET:		COLON
		LINK	SP0		; SP0
		LINK	FETCH		; @
		LINK	SP_STORE	; SP!
		LINK	TIB		; TIB
		LINK	HASH_TIB	; #TIB
		LINK	CELL_PLUS	; CELL+
		LINK	STORE		; !
		LINK	EXIT		; ;
		
		HEADER	4,"quit",NORMAL
QUIT:		COLON
;; TODO
		LINK	EXIT

;===============================================================================
; Compiler
;-------------------------------------------------------------------------------

		HEADER	5,"overt",NORMAL
OVERT:		COLON
		LINK	LAST		; LAST
		LINK	FETCH		; @
		LINK	CURRENT		; CURRENT
		LINK	FETCH		; @
		LINK	STORE		; !
		LINK	EXIT		; ;

;===============================================================================
; Structures
;-------------------------------------------------------------------------------


;===============================================================================
; 5.5 Defining Words
;-------------------------------------------------------------------------------

; USER
; CREATE
; VARIABLE


;===============================================================================
; Tools
;-------------------------------------------------------------------------------


;===============================================================================
; 6.4 Startup
;-------------------------------------------------------------------------------

; VER ( -- w )
;
; Returns the version number of this eForth system.
 
		HEADER	3,"ver",NORMAL
VER:		COLON
		LITERAL	$0101
		LINK	EXIT
		
; HI ( -- )
;
; Initialises I/O and displays the eForth version number. The version in the
; eForth saves BASE on the start but does not restore it at exit.

		HEADER	2,"hi",NORMAL
HI:		COLON
		LINK	IO		; !IO
		LINK	BASE		; BASE
		LINK	FETCH		; @
		LINK	HEX		; HEX
		LINK	CR		; CR
		
		LINK	VER		; VER
		LINK	HASH_LESS	; <#
		LINK	HASH		; #
		LINK	HASH		; #
		LITERAL	46		; 46
		LINK	HOLD		; HOLD
		LINK	HASH		; #
		LINK	HASH_GREATER	; #>
		LINK	TYPE		; TYPE
		LINK	CR		; CR
		LINK	BASE		; BASE
		LINK	STORE		; !
		LINK	EXIT		; ;

; 'BOOT ( -- a )
;
; A variable containing the boot function address

TICK_BOOT:	VARIABLE
		.word	HI
		
; COLD ( -- )
;
; Cold start entry point.

		HEADER	4,"cold",NORMAL
COLD:		COLON
.Begin:		LINK	U0		; BEGIN U0
		LINK	UP		; UP
		LITERAL	74		; 74
		LINK	CMOVE		; CMOVE
		LINK	PRESET		; PRESET
		LINK	TICK_BOOT	; 'BOOT
		LINK	AT_EXECUTE	; @EXECUTE
		LINK	FORTH		; FORTH
		LINK	CONTEXT		; CONTEXT
		LINK	FETCH		; @
		LINK	DUP		; DUP
		LINK	CURRENT		; CURRENT
		LINK	TWO_STORE	; 2!
		LINK	OVERT		; OVERT
		LINK	QUIT		; QUIT
		JUMP	.Begin		; AGAIN

; U0 ( -- a )
;
; Push the address if the USER area initial values table.

U0:		COLON
		LITERAL	UZERO
		LINK	EXIT

UZERO:
		.word	DSTACK_TOP	; + 0 = SP0
		.word	RSTACK_TOP	; + 2 = RP0
		.word	RX		; + 4 = '?KEY
		.word	TX		; + 6 = 'EMIT
		.word	0		; + 8
		.word	0		; +10
		.word	0		; +12
		.word	0		; +14
		.word	10		; +16 = BASE
		.word	0		; +18 = tmp
		.word	0		; +20 = SPAN
		.word	0		; +22 = >IN
		.word	0		; +24 = #TIB
		.word	0		; +26 = TIB
		.word	0		; +28 = CSP
		.word	0		; +30 = 'EVAL
		.word	0		; +32 = 'NUMBER
		.word	0		; +34 = HLD
		.word	0		; +36 = HANDER
		.word	0		; +38 = CONTEXT
		.word	0,0,0,0		; +40 = VOCAB STACK
		.word	0,0,0,0
		.word	0,0		; +56 = CURRENT
		.word	0		; +60 = CP
		.word	0		; +62 = NP
		.word	0		; +64 = LAST
		
		.end