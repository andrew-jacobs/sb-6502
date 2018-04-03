;===============================================================================
; Macros
;-------------------------------------------------------------------------------

LAST		.set	$0000

; The HEADER macro creates a work header containing a link to the last word
; the word name, including its length and its IMMEDIATE flag if set.

NORMAL		.equ	$00
IMMED		.equ	$80

HEADER		.macro	LEN,NAME,FLAGS
THIS		.set	$
		.word	LAST
		.byte	FLAGS,LEN,NAME
LAST		.set	THIS
		.endm
		
; Creates a link another defined word. As this value is executed using RTS
; it must be one less than the actual address.
		
LINK		.macro	ADDR
		.word	ADDR-1
		.endm
		
; Creates the code sequence to load a literal value.

LIT		.macro	VALUE
		.word	doLITERAL-1
		.word	VALUE
		.endm
		
; Creates the code for an unconditional branch to an indicated point in a
; compiled word definition.
		
JUMP		.macro	ADDR
		.word	BRANCH-1
		.word	ADDR
		.endm
		
; Creates the code for a conditional branch to an indicated point in a compiled
; word definition.
		
JZERO		.macro	ADDR
		.word	QBRANCH-1
		.word	ADDR
		.endm
		
;===============================================================================
; Variables
;-------------------------------------------------------------------------------

SP_ZERO:

CP:

HASH_TIB:

;===============================================================================
; Common Functions
;-------------------------------------------------------------------------------

; ?DUP ( w -- w w | 0 )
;
; If the value on the top of the data stack is non-zero then duplicate it
; otherwise leave a single zero value.

		HEADER	4,"?dup",NORMAL
QDUP:		jsr	doLIST
		LINK	DUP
		JZERO	.Skip
		LINK	DUP
.Skip		LINK	EXIT

; ROT ( w1 w2 w3 -- w2 w3 w1 )
;
; Rotates the top three values on the data stack.

		HEADER	3,"rot",NORMAL
ROT:		jsr	doLIST
		LINK	TO_R
		LINK	SWAP
		LINK	R_FROM
		LINK	SWAP
		LINK	EXIT

; 2DROP ( w w -- )
;
; Drop two value from the top of the data stack.		

		HEADER	5,"2drop",NORMAL
TWO_DROP:	jsr	doLIST
		LINK	DROP
		LINK	DROP
		LINK	EXIT
		
; 2DUP ( w1 w2 -- w1 w2 w1 w2 )
;
; Duplicates the two topmost cells on the data stack.

		HEADER	4,"2dup",NORMAL
TWO_DUP:	jsr	doLIST
		LINK	DUP
		LINK	DUP
		LINK	EXIT
		
; + ( w w -- w )
;
; Adds the two topmost cells on the data stack and leaves the sum.

		HEADER	1,"+",NORMAL
PLUS:		jsr	doLIST
		LINK	UM_PLUS
		LINK	DROP
		LINK	EXIT
		
; NOT ( w -- w )

		HEADER	3,"not",NORMAL
NOT:		jsr	doLIST
		LIT	-1
		LINK	XOR
		LINK	EXIT

; NEGATE ( n -- -n )

		HEADER	6,"negate",NORMAL
NEGATE:		jsr	doLIST
		LINK	NOT
		LIT	1
		LINK	PLUS
		LINK	EXIT

; DNEGATE ( d -- -d )

		HEADER	7,"dnegate",NORMAL
DNEGATE:	jsr	doLIST
		LINK	NOT
		LINK	TO_R
		LINK	NOT
		LIT	1
		LINK	UM_PLUS
		LINK	R_FROM
		LINK	PLUS
		LINK	EXIT
		
; D+ ( d d -- d )

		HEADER	2,"d+",NORMAL
D_PLUS:		jsr	doLIST
		LINK	TO_R
		LINK	SWAP
		LINK	TO_R
		LINK	UM_PLUS
		LINK	R_FROM
		LINK	R_FROM
		LINK	PLUS
		LINK	PLUS
		LINK	EXIT
		
; - ( w w -- w )
		
		HEADER	1,"-",NORMAL
MINUS:		jsr	doLIST
		LINK	NEGATE
		LINK	PLUS
		LINK	EXIT
		
; ABS ( n -- +n )

		HEADER	3,"abs",NORMAL
ABS:		jsr	doLIST
		LINK	DUP
		LINK	ZERO_LESS
		JZERO	.Skip
		LINK	NEGATE
.Skip		LINK	EXIT

;===============================================================================
; Comparison
;-------------------------------------------------------------------------------

; = ( w w -- t )
	
		HEADER	1,"=",NORMAL
EQUAL:		jsr	doLIST
		LINK	XOR
		JZERO	.Skip
		LIT	0
		LINK	EXIT
.Skip:		LIT	-1
		LINK	EXIT

; U< ( u u -- t )

		HEADER	2,"u<",NORMAL
U_LESS:		jsr	doLIST
		LINK	TWO_DUP
		LINK	XOR
		LINK	ZERO_LESS
		JZERO	.Skip
		LINK	SWAP
		LINK	DROP
		LINK	ZERO_LESS
		LINK	EXIT
.Skip:		LINK	MINUS
		LINK	ZERO_LESS
		LINK	EXIT
		
; < ( n n -- t )

		HEADER	1,"<",NORMAL
LESS:		jsr	doLIST
		LINK	TWO_DUP
		LINK	XOR
		LINK	ZERO_LESS
		JZERO	.Skip
		LINK	DROP
		LINK	ZERO_LESS
		LINK	EXIT
.Skip:		LINK	MINUS
		LINK	ZERO_LESS
		LINK	EXIT
		
; MAX ( n n -- n )

		HEADER	3,"max",NORMAL
MAX:		jsr	doLIST
		LINK	TWO_DUP
		LINK	LESS
		JZERO	.Skip
		LINK	SWAP
.Skip:		LINK	DROP
		LINK	EXIT
		
; MIN ( n n -- n )

		HEADER	3,"min",NORMAL
MIN:		jsr	doLIST
		LINK	TWO_DUP
		LINK	SWAP
		LINK	LESS
		JZERO	.Skip
		LINK	SWAP
.Skip:		LINK	DROP
		LINK	EXIT

; WITHIN ( u ul uh -- t )

		HEADER	6,"within",NORMAL
WITHIN:		jsr	doLIST
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

UM_SLASH_MOD:

M_SLASH_MOD:

; /MOD ( n n -- r q )

		HEADER	4,"/mod",NORMAL
SLASH_MOD:	jsr	doLIST
		LINK	OVER
		LINK	ZERO_LESS
		LINK	SWAP
		LINK	M_SLASH_MOD
		LINK	EXIT

; MOD ( n n -- r )

		HEADER	3,"mod",NORMAL
MOD:		jsr	doLIST
		LINK	SLASH_MOD
		LINK	DROP
		LINK	EXIT

; / ( n n -- q )

		HEADER	1,"/",NORMAL
SLASH:		jsr	doLIST
		LINK	SLASH_MOD
		LINK	SWAP
		LINK	DROP
		LINK	EXIT

;===============================================================================
; Multiply
;-------------------------------------------------------------------------------

UM_STAR:

; * ( n n -- n )

		HEADER	1,"*",NORMAL
STAR:		jsr	doLIST
		LINK	UM_STAR
		LINK	DROP
		LINK	EXIT

; M* ( n n -- d )

		HEADER	2,"m*",NORMAL
M_STAR:		jsr	doLIST
		LINK	TWO_DUP
		LINK	XOR
		LINK	ZERO_LESS
		LINK	TO_R
		LINK	ABS
		LINK	SWAP
		LINK	UM_STAR
		LINK	R_FROM
		JZERO	.Skip
		LINK	DNEGATE
.Skip:		LINK	EXIT

; */MOD ( n n n -- r q )

		HEADER	5,"*/mod",NORMAL
STAR_SLASH_MOD:	jsr	doLIST
		LINK	TO_R
		LINK	M_STAR
		LINK	R_FROM
		LINK	M_SLASH_MOD
		LINK	EXIT

; */ ( n n n -- q )

		HEADER	2,"*/",NORMAL
STAR_SLASH:	jsr	doLIST
		LINK	STAR_SLASH_MOD
		LINK	SWAP
		LINK	DROP
		LINK	EXIT

;===============================================================================
; Memory Alignment
;-------------------------------------------------------------------------------

; CELL- ( a -- a )

		HEADER	5,"cell-",NORMAL
CELL_MINUS:	jsr	doLIST
		LIT	-2
		LINK	PLUS
		LINK	EXIT
		
; CELL+ ( a -- a )

		HEADER	5,"cell+",NORMAL
CELL_PLUS:	jsr	doLIST
		LIT	2
		LINK	PLUS
		LINK	EXIT

; CELLS ( n -- n )

		HEADER	5,"cells",NORMAL
CELLS:		jsr	doLIST
		LIT	2
		LINK	STAR
		LINK	EXIT		

; ALIGNED ( b -- a )

		HEADER	7,"aligned",NORMAL
ALIGNED:	jsr	doLIST
		LINK	EXIT
		
; BL ( -- 32 )

		HEADER	2,"bl",NORMAL
BL:		jsr	doLIST
		LIT	' '
		LINK	EXIT
		
; >CHAR ( c -- c )

		HEADER	5,">char",NORMAL
LESS_CHAR:	jsr	doLIST
		LIT	$7f
		LINK	AND
		LINK	DUP
		LIT	127
		LINK	BL
		LINK	WITHIN
		JZERO	.Skip
		LINK	DROP
		LIT	95
.Skip:		LINK	EXIT

; DEPTH ( -- n )

		HEADER	5,"depth",NORMAL
DEPTH:		jsr	doLIST
		LINK	SP_FETCH
		LINK	SP_ZERO
		LINK	FETCH
		LINK	SWAP
		LINK	MINUS
		LIT	2
		LINK	SLASH
		LINK	EXIT

; PICK ( +n -- w )

		HEADER	4,"pick",NORMAL
PICK:		jsr	doLIST
		LIT	1
		LINK	PLUS
		LINK	CELLS
		LINK	SP_FETCH
		LINK	PLUS
		LINK	FETCH
		LINK	EXIT

;===============================================================================
; Memory Access
;-------------------------------------------------------------------------------

; +! ( n a -- )

		HEADER	2,"+!",NORMAL
PLUS_STORE:	jsr	doLIST
		LINK	SWAP
		LINK	OVER
		LINK	FETCH
		LINK	PLUS
		LINK	SWAP
		LINK	STORE
		LINK	EXIT

; 2! ( d a -- )

		HEADER	2,"2!",NORMAL
TWO_STORE:	jsr	doLIST
		LINK	SWAP
		LINK	OVER
		LINK	STORE
		LINK	CELL_PLUS
		LINK	STORE
		LINK	EXIT

; 2@ ( a -- d )

		HEADER	2,"2@",NORMAL
TWO_FETCH:	jsr	doLIST
		LINK	DUP
		LINK	CELL_PLUS
		LINK	FETCH
		LINK	SWAP
		LINK	FETCH
		LINK	EXIT

; COUNT ( b -- b + n )

		HEADER	5,"count",NORMAL
COUNT:		jsr	doLIST
		LINK	DUP
		LIT	1
		LINK	PLUS
		LINK	SWAP
		LINK	C_FETCH
		LINK	EXIT

; HERE ( -- a )

		HEADER	4,"here",NORMAL
HERE:		jsr	doLIST
		LINK	CP
		LINK	FETCH
		LINK	EXIT
		
; PAD ( -- a )

		HEADER	3,"pad",NORMAL
PAD:		jsr	doLIST
		LINK	HERE
		LIT	80
		LINK	PLUS
		LINK	EXIT
		
; TIB ( -- a )

		HEADER	3,"tib",NORMAL
TIB:		jsr	doLIST
		LINK	HASH_TIB
		LINK	CELL_PLUS
		LINK	FETCH
		LINK	EXIT
		
; @EXECUTE ( a -- )

		HEADER	8,"@execute",NORMAL
AT_EXECUTE:	jsr	doLIST
		LINK 	FETCH
		LINK	QDUP
		JZERO	.Skip
		LINK	EXECUTE
.Skip:		LINK	EXIT

CMOVE:

FILL:

MINUS_TRAILING:

PACK_DOLLAR:

;===============================================================================
; Numeric Output
;-------------------------------------------------------------------------------



	
;===============================================================================
;-------------------------------------------------------------------------------

		HEADER	4,"cold",NORMAL
COLD:		jsr	doLIST

