;===============================================================================
;
;  ____	 ____	      __  ____	 ____ ___   ___ ____	    
; / ___|| __ )	     / /_| ___| / ___( _ ) / _ \___ \	    
; \___ \|  _ \ _____| '_ \___ \| |   / _ \| | | |__) |	    
;  ___) | |_) |_____| (_) |__) | |__| (_) | |_| / __/	    
; |____/|____/	     \___/____/ \____\___/ \___/_____|	    
; | __ )  ___	___ | |_|  \/  | ___  _ __ (_) |_ ___  _ __ 
; |  _ \ / _ \ / _ \| __| |\/| |/ _ \| '_ \| | __/ _ \| '__|
; | |_) | (_) | (_) | |_| |  | | (_) | | | | | || (_) | |   
; |____/ \___/ \___/ \__|_|  |_|\___/|_| |_|_|\__\___/|_|   
;							    
;-------------------------------------------------------------------------------
; Copyright (C)2017-2018 Andrew John Jacobs.
; All rights reserved.
;
; This work is made available under the terms of the Creative Commons
; Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
; following URL to see the details.
;
; http://creativecommons.org/licenses/by-nc-sa/4.0/
;===============================================================================
;
; Notes:
;
; When the monitor is running the processor is switched to native and configured
; with A as 8-bits and X/Y as 16-bits. User programs can be in either native or
; emulation mode.
;
; Interrupt handling is performed in both emulation and native mode and the code
; is written to use 8-bit accumulator and index registers.
; 
; The Uart I/O routines will work in any configuration. They save the callers
; register configuration on entry and restore it on exit.
;
;-------------------------------------------------------------------------------

		.65816
		
MONITOR		.equ	1

		.include "../sb-6502.inc"
		
;===============================================================================
; ASCII Character Codes
;-------------------------------------------------------------------------------

SOH		.equ	$01
EOT		.equ	$04
ACK		.equ	$06
BEL		.equ	$07
BS		.equ	$08
LF		.equ	$0a
CR		.equ	$0d
DC1		.equ	$11		; XON
DC3		.equ	$13		; XOFF
NAK		.equ	$15
CAN		.equ	$18
ESC		.equ	$1b
DEL		.equ	$7f

;===============================================================================
; Instruction and Mode Constants
;-------------------------------------------------------------------------------
		
OP_ERR		.equ	0<<1
OP_ADC		.equ	1<<1
OP_AND		.equ	2<<1
OP_ASL		.equ	3<<1
OP_BCC		.equ	4<<1
OP_BCS		.equ	5<<1
OP_BEQ		.equ	6<<1
OP_BIT		.equ	7<<1
OP_BMI		.equ	8<<1
OP_BNE		.equ	9<<1
OP_BPL		.equ	10<<1
OP_BRA		.equ	11<<1
OP_BRK		.equ	12<<1
OP_BRL		.equ	13<<1
OP_BVC		.equ	14<<1
OP_BVS		.equ	15<<1
OP_CLC		.equ	16<<1
OP_CLD		.equ	17<<1
OP_CLI		.equ	18<<1
OP_CLV		.equ	19<<1
OP_CMP		.equ	20<<1
OP_COP		.equ	21<<1
OP_CPX		.equ	22<<1
OP_CPY		.equ	23<<1
OP_DEC		.equ	24<<1
OP_DEX		.equ	25<<1
OP_DEY		.equ	26<<1
OP_EOR		.equ	27<<1
OP_INC		.equ	28<<1
OP_INX		.equ	29<<1
OP_INY		.equ	30<<1
OP_JML		.equ	31<<1
OP_JMP		.equ	32<<1
OP_JSL		.equ	33<<1
OP_JSR		.equ	34<<1
OP_LDA		.equ	35<<1
OP_LDX		.equ	36<<1
OP_LDY		.equ	37<<1
OP_LSR		.equ	38<<1
OP_MVN		.equ	39<<1
OP_MVP		.equ	40<<1
OP_NOP		.equ	41<<1
OP_ORA		.equ	42<<1
OP_PEA		.equ	43<<1
OP_PEI		.equ	44<<1
OP_PER		.equ	45<<1
OP_PHA		.equ	46<<1
OP_PHB		.equ	47<<1
OP_PHD		.equ	48<<1
OP_PHK		.equ	49<<1
OP_PHP		.equ	50<<1
OP_PHX		.equ	51<<1
OP_PHY		.equ	52<<1
OP_PLA		.equ	53<<1
OP_PLB		.equ	54<<1
OP_PLD		.equ	55<<1
OP_PLP		.equ	56<<1
OP_PLX		.equ	57<<1
OP_PLY		.equ	58<<1
OP_REP		.equ	59<<1
OP_ROL		.equ	60<<1
OP_ROR		.equ	61<<1
OP_RTI		.equ	62<<1
OP_RTL		.equ	63<<1
OP_RTS		.equ	64<<1
OP_SBC		.equ	65<<1
OP_SEC		.equ	66<<1
OP_SED		.equ	67<<1
OP_SEI		.equ	68<<1
OP_SEP		.equ	69<<1
OP_STA		.equ	70<<1
OP_STP		.equ	71<<1
OP_STX		.equ	72<<1
OP_STY		.equ	73<<1
OP_STZ		.equ	74<<1
OP_TAX		.equ	75<<1
OP_TAY		.equ	76<<1
OP_TCD		.equ	77<<1
OP_TCS		.equ	78<<1
OP_TDC		.equ	79<<1
OP_TRB		.equ	80<<1
OP_TSB		.equ	81<<1
OP_TSC		.equ	82<<1
OP_TSX		.equ	83<<1
OP_TXA		.equ	84<<1
OP_TXS		.equ	85<<1
OP_TXY		.equ	86<<1
OP_TYA		.equ	87<<1
OP_TYX		.equ	88<<1
OP_WAI		.equ	89<<1
OP_WDM		.equ	90<<1
OP_XBA		.equ	91<<1
OP_XCE		.equ	92<<1

;-------------------------------------------------------------------------------

MO_ABS		.equ	0<<1			; a
MO_ACC		.equ	1<<1			; A
MO_ABX		.equ	2<<1			; a,x
MO_ABY		.equ	3<<1			; a,y
MO_ALG		.equ	4<<1			; al
MO_ALX		.equ	5<<1			; al,x
MO_AIN		.equ	6<<1			; (a)
MO_AIX		.equ	7<<1			; (a,x)
MO_DPG		.equ	8<<1			; d
MO_STK		.equ	9<<1			; d,s
MO_DPX		.equ	10<<1			; d,x
MO_DPY		.equ	11<<1			; d,x
MO_DIN		.equ	12<<1			; (d)
MO_DLI		.equ	13<<1			; [d]
MO_SKY		.equ	14<<1			; (d,s),y
MO_DIX		.equ	15<<1			; (d,x)
MO_DIY		.equ	16<<1			; (d),y
MO_DLY		.equ	17<<1			; [d],y
MO_IMP		.equ	18<<1			;
MO_REL		.equ	19<<1			; r
MO_RLG		.equ	20<<1			; rl
MO_MOV		.equ	21<<1			; xyc
MO_IMM		.equ	22<<1			; # (A or M)
MO_INT		.equ	23<<1			; # (BRK/COP/WDM)
MO_IMX		.equ	24<<1			; # (X or Y)

;===============================================================================
; Macros
;-------------------------------------------------------------------------------

; The MNEM macro compresses three characters into a 16-bit value.

MNEM		.macro	CH1,CH2,CH3
		.word	((((CH3 & $1f) << 5)|(CH2 & $1f)) << 5)|(CH1 & $1f)
		.endm

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.page0
		.org	$00e0
		
E_BIT		.space	1			; E in MSB
P_REG		.space	1
C_REG		.space	2
X_REG		.space	2
Y_REG		.space	2
DP_REG		.space	2
SP_REG		.space	2
PC_REG		.space	2
B_REG		.space	1
K_REG		.space	1

CMD_LEN		.space	1			; Command buffer length
ADDR_S		.space	2
ADDR_E		.space	2

TEMP		.space	2
COUNT		.space	1
	
;===============================================================================
; UART Buffers
;-------------------------------------------------------------------------------
		
BUF_SIZE	.equ	58

		.bss
		.org	$0200
		
		.space	8		; Vectors

RX_HEAD		.space	1		; UART recieve buffer offsets
RX_TAIL		.space	1
TX_HEAD		.space	1		; UART transmit buffer offsets
TX_TAIL		.space	1

RX_BUFFER	.space	BUF_SIZE	; UART recieve buffer
TX_BUFFER	.space	BUF_SIZE	; UART transmit buffer

CMD_SIZE	.equ	128

COMMAND		.space	CMD_SIZE

;===============================================================================
; Compressed Mnemonics
;-------------------------------------------------------------------------------
		
		.code
		.org	$f000
		
MNEMONICS:
		MNEM    '?','?','?'
		MNEM	'A','D','C'
		MNEM	'A','N','D'
		MNEM	'A','S','L'
		MNEM	'B','C','C'
		MNEM	'B','C','S'
		MNEM	'B','E','Q'
		MNEM	'B','I','T'
		MNEM	'B','M','I'
		MNEM	'B','N','E'
		MNEM	'B','P','L'
		MNEM	'B','R','A'
		MNEM	'B','R','K'
		MNEM	'B','R','L'
		MNEM	'B','V','C'
		MNEM	'B','V','S'
		MNEM	'C','L','C'
		MNEM	'C','L','D'
		MNEM	'C','L','I'
		MNEM	'C','L','V'
		MNEM	'C','M','P'
		MNEM	'C','O','P'
		MNEM	'C','P','X'
		MNEM	'C','P','Y'
		MNEM	'D','E','C'
		MNEM	'D','E','X'
		MNEM	'D','E','Y'
		MNEM	'E','O','R'
		MNEM	'I','N','C'
		MNEM	'I','N','X'
		MNEM	'I','N','Y'
		MNEM	'J','M','L'
		MNEM	'J','M','P'
		MNEM	'J','S','L'
		MNEM	'J','S','R'
		MNEM	'L','D','A'
		MNEM	'L','D','X'
		MNEM	'L','D','Y'
		MNEM	'L','S','R'
		MNEM	'M','V','N'
		MNEM	'M','V','P'
		MNEM	'N','O','P'
		MNEM	'O','R','A'
		MNEM	'P','E','A'
		MNEM	'P','E','I'
		MNEM	'P','E','R'
		MNEM	'P','H','A'
		MNEM	'P','H','B'
		MNEM	'P','H','D'
		MNEM	'P','H','K'
		MNEM	'P','H','P'
		MNEM	'P','H','X'
		MNEM	'P','H','Y'
		MNEM	'P','L','A'
		MNEM	'P','L','B'
		MNEM	'P','L','D'
		MNEM	'P','L','P'
		MNEM	'P','L','X'
		MNEM	'P','L','Y'
		MNEM	'R','E','P'
		MNEM	'R','O','L'
		MNEM	'R','O','R'
		MNEM	'R','T','I'
		MNEM	'R','T','L'
		MNEM	'R','T','S'
		MNEM	'S','B','C'
		MNEM	'S','E','C'
		MNEM	'S','E','D'
		MNEM	'S','E','I'
		MNEM	'S','E','P'
		MNEM	'S','T','A'
		MNEM	'S','T','P'
		MNEM	'S','T','X'
		MNEM	'S','T','Y'
		MNEM	'S','T','Z'
		MNEM	'T','A','X'
		MNEM	'T','A','Y'
		MNEM	'T','C','D'
		MNEM	'T','C','S'
		MNEM	'T','D','C'
		MNEM	'T','R','B'
		MNEM	'T','S','B'
		MNEM	'T','S','C'
		MNEM	'T','S','X'
		MNEM	'T','X','A'
		MNEM	'T','X','S'
		MNEM	'T','X','Y'
		MNEM	'T','Y','A'
		MNEM	'T','Y','X'
		MNEM	'W','A','I'
		MNEM	'W','D','M'
		MNEM	'X','B','A'
		MNEM	'X','C','E'

;===============================================================================
; Power On Reset
;-------------------------------------------------------------------------------

		.longa	off		; Assume 8-bits at reset
		.longi	off
RESET:
		sei			; Disable interrupts
		cld			
		sec			; Ensure emulation mode
		xce
		
		ldx	#$ff		; Initialise the stack
		txs
		
		inx
		stx	TX_HEAD		; Clear buffer indexes
		stx	TX_TAIL
		stx	RX_HEAD
		stx	RX_TAIL
		
		repeat
		 lda	VECTORS,x	; Setup the vectors
		 sta	IRQV,x
		 inx
		 cpx	#8
		until eq
		
                lda     #%00011111	; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTRL
                lda     #%11001001	; No parity, no interrupt
                sta     ACIA_CMND
                lda     ACIA_DATA	; Clear receive buffer
		
		cli			; Allow interrupts
		
		jsr	NewLine		; Show title string
		ldx	#TTL_STR
		jsr	ShowString
		
		repeat
		 brk	#0		; And enter monitor via BRK
		forever

;-------------------------------------------------------------------------------

VECTORS:	.word	IRQE
		.word	NMIE
		.word	IRQN
		.word	NMIN
		
;===============================================================================
; Break Handler
;-------------------------------------------------------------------------------

		.longa 	off
		.longi	off
BRKE:
BRKN:
		clc			; Ensure native mode
		xce
		ror	E_BIT		; .. but save the original
		
		LONG_AI			; Make all registers 16-bit
		sta	C_REG		; Save A/B
		stx	X_REG		; .. X
		sty	Y_REG		; .. Y
		tdc			; Save DP
		sta	DP_REG
		
		SHORT_A			; Make A register 8-bits
		bit	E_BIT		; 
		if pl
	; Banks
	
		endif
		
		pla			; Save P
		sta	P_REG
		plx			; Save PC
		dex
		dex
		stx	PC_REG
		tsx			; Save SP
		stx	SP_REG
		
		cli			; Allow interrupts
		
;===============================================================================
; Show Registers
;-------------------------------------------------------------------------------

		.longa	off
		.longi	on
ShowRegisters:
		jsr	NewLine
		ldx	#PC_STR		; Display the PC
		jsr	ShowString
		lda	PC_REG+1
		xba
		lda	PC_REG+0
		jsr	ShowHex4
		
		ldx	#E_STR		; Display the E bit
		jsr	ShowString
		lda	#'0'
		bit	E_BIT
		if	mi
		 inc	a
		endif
		jsr	UartTx
		
		ldx	#C_STR		; Display A/B
		jsr	ShowString
		lda	C_REG+1
		xba
		lda	C_REG+0
		jsr	ShowAcc
		
		ldx	#X_STR		; Display X
		jsr	ShowString
		lda	X_REG+1
		xba
		lda	X_REG+0
		jsr	ShowReg
		
		ldx	#Y_STR		; Display Y
		jsr	ShowString
		lda	Y_REG+1
		xba
		lda	Y_REG+0
		jsr	ShowReg
		
		ldx	#P_STR		; Display P
		jsr	ShowString
		ldx	#7
		repeat
		 ldy	#'.'
		 lda	BITS,x
		 bit	P_REG
		 if	NE
		  ldy	FLAG,x
		 endif
		 tya
		 jsr	UartTx
		 dex
		until mi

; TODO PBR:DBR		

		ldx	#DP_STR		; Display DP
		jsr	ShowString
		lda	DP_REG+1
		xba
		lda	DP_REG+0
		jsr	ShowHex4
		
		ldx	#SP_STR		; Display SP
		jsr	ShowString
		lda	SP_REG+1
		xba
		lda	SP_REG+0
		jsr	ShowHex4
		
;===============================================================================
; Command Line
;-------------------------------------------------------------------------------

NewCommand:
		stz	CMD_LEN
RptCommand:
		SHORT_AI
		jsr	NewLine		; Move cursor to next line
		lda	#'.'		; And output the prompt
		jsr	UartTx
		
		ldx	#0		; Output prepared command
		repeat
		 cpx	CMD_LEN		; Any saved characters to display?
		 break	cs		; No
		 lda	COMMAND,X	; Yes, print from the buffer
		 jsr	UartTx
		 inx
		forever
		
		lda	#DC1		; Send XON
		jsr	UartTx
		repeat
		 jsr	UartRx		; Read a character
		 sta	COMMAND,x	; .. and save
		 
		 cmp	#CR		; End of input?
		 break	eq
		 
		 cmp 	#ESC		; Cancel input?
		 if	eq
		  beq	NewCommand	; Yes
		 endif

		 cmp	#DEL		; Turn a delete
		 if 	eq
		  lda	#BS		; .. into a backspace
		 endif
		 
		 cmp	#BS		; Handle backspace
		 if 	eq
		  cpx   #0
		  if 	ne
		   pha
		   jsr	UartTx
		   jsr	Space
		   pla
		   jsr	UartTx
		   dex
		  endif
		  continue
		 endif
		 
		 cmp	#' '		; Beep if non-printable
		 if 	cc
		  lda	#BEL
		  jsr	UartTx
		  continue
		 endif
		 
		 jsr	UartTx		; Otherwise echo to screen
		 inx			; And bump counter
		forever
		
		stx	CMD_LEN		; Save command length
		lda	#DC3		; Send XOFF
		jsr	UartTx

		ldx	#0		; Set character offset to start
		jsr	SkipSpaces	; And get first character
		bcs	NewCommand
		
		cmp	#CR		; Empty line?
		beq	NewCommand	; Yes

;===============================================================================
; 'D' - Disassemble Memory
;-------------------------------------------------------------------------------

		cmp	#'D'
		if 	eq
		endif
	
;===============================================================================
; 'F' - Fill
;-------------------------------------------------------------------------------

		cmp	#'F'
		if	eq
		endif
		
;===============================================================================
; 'G' - Go
;-------------------------------------------------------------------------------

		cmp	#'G'
		if 	eq
		endif
		
;===============================================================================
; 'M' - Show Memory
;-------------------------------------------------------------------------------

		cmp	#'M'
		if	eq
		endif
		
;===============================================================================
; 'R' - Show Registers
;-------------------------------------------------------------------------------

		cmp	#'R'
		if	eq
		 jmp	ShowRegisters
		endif
		
;===============================================================================
; 'S' - S19 Record Loader
;-------------------------------------------------------------------------------

		cmp	#'S'
		if eq
		endif
		
;===============================================================================
; 'W' - Write memory
;-------------------------------------------------------------------------------

		cmp	#'W'
		if	eq
		 jsr	GetWord	;	 Get the target address
		 if	cc
		  jsr	SetStartAddr	; Copy to start address
		  jsr	GetByte		; Get the value
		  if	cc
		   ldy	#0		; Write to memory
		   lda	TEMP+0
		   sta	(ADDR_S),Y
		   lda	#1		; Increment address
		   jsr	BumpAddr
		   lda	#'W'		; Create prompt for next byte
		   jmp	SetPrompt
		  endif
		 endif
		 jmp	Error		; Handle syntax errors
		endif
		
;===============================================================================
; '?' - Display Help
;-------------------------------------------------------------------------------

		cmp 	#'?'
		if eq
		 ldx	#HLP_STR
		else
Error:		 ldx	#ERR_STR
		endif
		jsr	ShowString
		jmp	NewCommand
		
;===============================================================================
;-------------------------------------------------------------------------------		
		
		.longa	off
SetStartAddr:
		lda	TEMP+0
		sta	ADDR_S+0
		lda	TEMP+1
		sta	ADDR_S+1
		rts
		
		.longa	off
SetEndAddr:
		lda	TEMP+0
		sta	ADDR_E+0
		lda	TEMP+1
		sta	ADDR_E+1
		rts
		
		.longa	off
BumpAddr:
		inc	ADDR_S+0
		if	eq
		 inc	ADDR_S+1
		endif
		rts
	
		.longa	off
CheckEnd:
		lda	ADDR_S+1
		cmp	ADDR_E+1
		if 	cs
		 if 	eq
		  lda	ADDR_S+0
		  cmp	ADDR_E+0
		 endif
		endif
		rts
		
; Create a prompt string in the command buffer for the command in A using the
; current value of the starting address.

		.longa	off
		.longi	off
SetPrompt:
		ldx	#0		; Clear buffer and add command letter
		jsr	AppendChar
		lda	#' '		; Then a space
		jsr	AppendChar
		
		lda	ADDR_S+1	; Followed by the address
		jsr	AppendHex2
		lda	ADDR_S+0
		jsr	AppendHex2
		lda	#' '		; And another space
		jsr	AppendChar
		jmp	RptCommand	; Then output it

; Convert the byte in A into hexadecimal digits and append to the command buffer.		
		
		.longa	off
		.longi	off
AppendHex2:
		pha
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		jsr	AppendHex
		pla
AppendHex:
		jsr	ToHex

; Append the character in A to the command buffer to create the next prompt
; string.

		.longa	off
		.longi	off
AppendChar:
		sta	COMMAND,x
		inx
		stx	CMD_LEN
		rts
	
;===============================================================================
; Parsing Utilities
;-------------------------------------------------------------------------------

; Get the next characater from the command buffer indicated by the X register
; and convert it to UPPER case. If the carry is set then the end of the buffer
; has been reached.

		.longa	off
		.longi	off
NextChar:
		cpx	CMD_LEN		; Reached end of buffer>
		if	CS
		 rts			; Yes, return with C=1
		endif
		lda	COMMAND,X	; No, fetch a character and C=0
		inx


; Convert the character in A to upper case.

		.longa	off
ToUpper:
		cmp	#'a'		; Between 'a'
		if cs
		 cmp	#'z'+1		; .. and 'z'?
		 if cc
		  and	#$5f		; Yes, convert 	
		 endif
		endif
		clc			; Ensure C=0
		rts

; Fetch the next characters from the command buffer ignoring spaces.

		.longa	off
		.longi	off
SkipSpaces:
		repeat
		 jsr	NextChar	; Fetch a character?
		 break cs		; Reached the end?
		 cmp	#' '		; A space to ignore?
		 clc
		until ne
		rts			; Done

; Parse a word from the command buffer and store it at 0,Y. Return if the
; carry set if there is a syntax error.

		.longa	off
		.longi	off
GetWord:
		ldy	#4		; Set maximim number of nybbles
		bne	GetByte+2

; Parse a word from the command buffer and store it at 0,Y. Return if the
; carry set if there is a syntax error.

		.longa	off
		.longi	off
GetByte:
		ldy	#2		; Set maximum number of nybble
		sty	COUNT
		
		stz	TEMP+0		; Clear conversion area
		stz	TEMP+1

		jsr	SkipSpaces	; Fetch first character
		jsr	GetNybble	; And try to convert
		if	cs
		 rts			; Syntax error
		endif
		repeat
		 asl	TEMP+0		; Fold into the result
		 rol	TEMP+1
		 asl	TEMP+0
		 rol	TEMP+1
		 asl	TEMP+0
		 rol	TEMP+1
		 asl	TEMP+0
		 rol	TEMP+1
		 ora	TEMP+0
		 sta	TEMP+0
		 
		 dec	COUNT		; Reach maximum length?
		 break	eq
		 
		 jsr	NextChar	; Try for another nybble
		 jsr	GetNybble
		until 	cs		
		clc			; Conversion sucessfull
		rts

; Try to parse a nybble from the command line. If not a valid hex digit then
; return with the carry set.

		.longa	off
		.longi	off
GetNybble:
		jsr	IsHex		; Got a hex digit?
		if	cs
		 cmp	#'A'		; Handle letters
		 if	cs
		  sbc	#7
		 endif
		 and	#$0f		; Strip out nybble
		 clc			; Done
		 rts
		endif
		sec			; Set carry -- not hex
		rts

; Return with the carry set of the character in A is a digit or 'A' thru 'F'.

IsHex:
		cmp	#'9'+1
		if 	cc
		 cmp	#'0'
		 rts
		endif
		cmp	#'F'+1
		if	cc
		 cmp	#'A'
		 rts
		endif
		clc
		rts
		
IsPrintable:
		cmp	#' '
		if	cs
		 cmp	#DEL
		 if	CC
		  sec
		  rts
		 endif
		endif
		clc
		rts

;===============================================================================
; Disassembly
;-------------------------------------------------------------------------------

Disassemble:
		rts



;===============================================================================
; Display Utilities
;-------------------------------------------------------------------------------

; Show the value of the accumulator with brackets inserted to show its current
; size based on the M bit.

		.longa	off
ShowAcc:
		pha
		bit	E_BIT		; In emulation mode?
		bmi	ShowShort
		lda	#$20
		bit	P_REG		; No, M bit set?
		bne	ShowShort	; Yes, short
		
ShowLong:
		jsr	OpenBracket	; Output [xxxx]
		xba
		jsr	ShowHex2
		pla
		jsr	ShowHex2
		jmp	CloseBracket
		
; Show the value of the accumulator with brackets inserted to show its current
; size based on the X bit.

		.longa	off
ShowReg:
		pha
		bit	E_BIT		; In emulation mode?
		bmi	ShowShort
		lda	#$10
		bit	P_REG		; No, X bit set?
		beq	ShowLong	; No, long

ShowShort:
		xba			; Output xx[xx]
		jsr	ShowHex2
		jsr	OpenBracket
		pla
		jsr	ShowHex2
		jmp	CloseBracket

;-------------------------------------------------------------------------------

; Display the byte in A as four hexadecimal digits. The values in A & Y are
; destroyed.

		.longa	off
ShowHex4:
		xba			; Recover the MS byte
		jsr	ShowHex2	; Print it
		xba			; Recover the LS byte
		
; Display the byte in A as two hexadecimal digits. The values in A & Y are
; destroyed.

		.longa	off
ShowHex2:
		pha			; Save the data byte
		lsr	a		; Shift down hi nybble
		lsr	a
		lsr	a
		lsr	a
		jsr	ShowHex 	; Display it
		pla			; Recover the lo nybble

; Display the lo nybble of A as a hexadecimal digit. The values in A & Y are
; destroyed.
		.longa	off
ShowHex:
		jsr	ToHex
		jmp	UartTx
		
; Convert the lo nybble of A to a hexadecimal digit.
		
ToHex:
		and	#$0f		; Extract the lo nybble
		sed			; Convert to ASCII using BCD
		clc
		adc	#$90
		adc	#$40
		cld
		rts			; Done

;-------------------------------------------------------------------------------		

		.longa	off
Space:
		lda	#' '
		jmp	UartTx
		
		.longa	off
OpenBracket:
		lda	#'['
		jmp	UartTx
		
		.longa	off
CloseBracket:
		lda	#']'
		jmp	UartTx
	
;===============================================================================
; Strings
;-------------------------------------------------------------------------------

; Output the string in the string table starting at the offset in X until a
; null byte is reached.

		.longa	off		; A must be 8-bit. X can be either
ShowString:
		repeat
		 lda	STRINGS,x	; Fetch the next character
		 break	eq		; Reached the end?
		 jsr	UartTx		; No, display it
		 inx			; Bump the index
		forever		
		rts			; Done.
		
STRINGS:
TTL_STR		.equ	$-STRINGS
		.byte	"Boot 65C802 [18.05]"
		.byte	0
PC_STR		.equ	$-STRINGS
		.byte	"PC=",0
E_STR		.equ	$-STRINGS
		.byte	" E=",0
C_STR		.equ	$-STRINGS
		.byte	" C=",0
X_STR		.equ	$-STRINGS
		.byte	" X=",0
Y_STR		.equ	$-STRINGS
		.byte	" Y=",0
P_STR		.equ	$-STRINGS
		.byte	" P=",0
DP_STR		.equ	$-STRINGS
		.byte	" DP=",0
SP_STR		.equ	$-STRINGS
		.byte	" SP=",0
ERR_STR		.equ	$-STRINGS
		.byte	CR,LF,"?",0
HLP_STR		.equ	$-STRINGS
		.byte 	CR,LF,"D xxxx yyyy\t\tDisassemble"
		.byte	CR,LF,"F xxxx yyyy bb\t\tFill Memory"
		.byte	CR,LF,"G [xxxx]\t\tGoto"
		.byte	CR,LF,"M xxxx yyyy\t\tDisplay Memory"
		.byte	CR,LF,"R\t\t\tDisplay Registers"
		.byte	CR,LF,"S...\t\t\tS19 Load"
		.byte	CR,LF,"W xxxx yy\t\tWrite Memory"
		.byte	0
		
FLAG		.byte	"CZIDXMVN"
BITS		.byte	$01,$02,$04,$08,$10,$20,$40,$80
		
;===============================================================================
; Instruction Lookup Tables
;-------------------------------------------------------------------------------

		.org	$fc00

OPCODES:
		.byte	OP_BRK,OP_ORA,OP_COP,OP_ORA,OP_TSB,OP_ORA,OP_ASL,OP_ORA	; 00
		.byte	OP_PHP,OP_ORA,OP_ASL,OP_PHD,OP_TSB,OP_ORA,OP_ASL,OP_ORA
		.byte	OP_BPL,OP_ORA,OP_ORA,OP_ORA,OP_TRB,OP_ORA,OP_ASL,OP_ORA	; 10
		.byte	OP_CLC,OP_ORA,OP_INC,OP_TCS,OP_TRB,OP_ORA,OP_ASL,OP_ORA
		.byte	OP_JSR,OP_AND,OP_JSL,OP_AND,OP_BIT,OP_AND,OP_ROL,OP_AND	; 20
		.byte	OP_PLP,OP_AND,OP_ROL,OP_PLD,OP_BIT,OP_AND,OP_ROL,OP_AND
		.byte	OP_BMI,OP_AND,OP_AND,OP_AND,OP_BIT,OP_AND,OP_ROL,OP_AND	; 30
		.byte	OP_SEC,OP_AND,OP_DEC,OP_TSC,OP_BIT,OP_AND,OP_ROL,OP_AND
		.byte	OP_RTI,OP_EOR,OP_WDM,OP_EOR,OP_MVP,OP_EOR,OP_LSR,OP_EOR	; 40
		.byte	OP_PHA,OP_EOR,OP_LSR,OP_PHK,OP_JMP,OP_EOR,OP_LSR,OP_EOR
		.byte	OP_BVC,OP_EOR,OP_EOR,OP_EOR,OP_MVN,OP_EOR,OP_LSR,OP_EOR	; 50
		.byte	OP_CLI,OP_EOR,OP_PHY,OP_TCD,OP_JMP,OP_EOR,OP_LSR,OP_EOR
		.byte	OP_RTS,OP_ADC,OP_PER,OP_ADC,OP_STZ,OP_ADC,OP_ROR,OP_ADC	; 60
		.byte	OP_PLA,OP_ADC,OP_ROR,OP_RTL,OP_JMP,OP_ADC,OP_ROR,OP_ADC
		.byte	OP_BVS,OP_ADC,OP_ADC,OP_ADC,OP_STZ,OP_ADC,OP_ROR,OP_ADC	; 70
		.byte	OP_SEI,OP_ADC,OP_PLY,OP_TDC,OP_JMP,OP_ADC,OP_ROR,OP_ADC
		.byte	OP_BRA,OP_STA,OP_BRL,OP_STA,OP_STY,OP_STA,OP_STX,OP_STA	; 80
		.byte	OP_DEY,OP_BIT,OP_TXA,OP_PHB,OP_STY,OP_STA,OP_STX,OP_STA
		.byte	OP_BCC,OP_STA,OP_STA,OP_STA,OP_STY,OP_STA,OP_STX,OP_STA	; 90
		.byte	OP_TYA,OP_STA,OP_TXS,OP_TXY,OP_STZ,OP_STA,OP_STZ,OP_STA
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_LDA,OP_LDY,OP_LDA,OP_LDX,OP_LDA	; A0
		.byte	OP_TAY,OP_LDA,OP_TAX,OP_PLB,OP_LDY,OP_LDA,OP_LDX,OP_LDA
		.byte	OP_BCS,OP_LDA,OP_LDA,OP_LDY,OP_LDA,OP_LDY,OP_LDX,OP_LDA	; B0
		.byte	OP_CLV,OP_LDA,OP_TSX,OP_TYX,OP_LDY,OP_LDA,OP_LDX,OP_LDA
		.byte	OP_CPY,OP_CMP,OP_REP,OP_CMP,OP_CPY,OP_CMP,OP_DEC,OP_CMP	; C0
		.byte	OP_INY,OP_CMP,OP_DEX,OP_WAI,OP_CPY,OP_CMP,OP_DEC,OP_CMP
		.byte	OP_BNE,OP_CMP,OP_CMP,OP_CMP,OP_PEI,OP_CMP,OP_DEC,OP_CMP	; D0
		.byte	OP_CLD,OP_CMP,OP_PHX,OP_STP,OP_JML,OP_CMP,OP_DEC,OP_CMP
		.byte	OP_CPX,OP_SBC,OP_SEP,OP_SBC,OP_CPX,OP_SBC,OP_INC,OP_SBC	; E0
		.byte	OP_INX,OP_SBC,OP_NOP,OP_XBA,OP_CPX,OP_SBC,OP_INC,OP_SBC
		.byte	OP_BEQ,OP_SBC,OP_SBC,OP_SBC,OP_PEA,OP_SBC,OP_INC,OP_SBC	; F0
		.byte	OP_SED,OP_SBC,OP_PLX,OP_XCE,OP_JSR,OP_SBC,OP_INC,OP_SBC	

MODES:
		.byte	MO_INT,MO_DIX,MO_INT,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; 00
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG	
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_DPG,MO_DPX,MO_DPX,MO_DLY	; 10
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABS,MO_ABX,MO_ABX,MO_ALX	
		.byte	MO_ABS,MO_DIX,MO_ALG,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; 20
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG	
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_DPX,MO_DPX,MO_DPX,MO_DLY	; 30	
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABX,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMP,MO_DIX,MO_INT,MO_STK,MO_MOV,MO_DPG,MO_DPG,MO_DLI	; 40
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_MOV,MO_DPX,MO_DPX,MO_DLY	; 50
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ALG,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMP,MO_DIX,MO_IMP,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; 60
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_AIN,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_DPX,MO_DPX,MO_DPX,MO_DLY	; 70
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_AIX,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_REL,MO_DIX,MO_RLG,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; 80
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_DPX,MO_DPX,MO_DPY,MO_DLY	; 90
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABS,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMX,MO_DIX,MO_IMX,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; A0
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_DPX,MO_DPX,MO_DPY,MO_DLY	; B0
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_ABY,MO_ALX
		.byte	MO_IMX,MO_DIX,MO_INT,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; C0
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_IMP,MO_DPX,MO_DPX,MO_DLY	; D0
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_AIN,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMX,MO_DIX,MO_INT,MO_STK,MO_DPG,MO_DPG,MO_DPG,MO_DLI	; E0
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY,MO_IMP,MO_DPX,MO_DPX,MO_DLY	; F0
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_AIX,MO_ABX,MO_ABX,MO_ALX

;===============================================================================
; Virtual Hardware Area
;-------------------------------------------------------------------------------

		.org	$fe00
		.space	64		; Skip over virtual hardware
		
;===============================================================================
; I/O API
;-------------------------------------------------------------------------------

		.longa	off
		jmp	UartTx
		
		.longa	off
		jmp	UartRx
		
		.longa	off
NewLine:	lda	#CR
		jsr	UartTx
		lda	#LF
		jmp	UartTx
		
;===============================================================================
; IRQ Handlers
;-------------------------------------------------------------------------------

; In emulation mode we have to differentiate between BRK and a real interrupt.

		.longa	off
		.longi	off
IRQE:
		pha			; Save users A
		lda	2,s		; Fetch the flags from stack
		bit	#$10		; Test for BRK
		if	ne
		 pla
		 jmp	BRKE		; Enter monitor in emulation mode
		endif
		
		phy			; Save users Y
		jsr	UartHandler	; Service the UART
		ply			; Restore users registers
		pla
		rti			; Continue

;-------------------------------------------------------------------------------

IRQN:
		pha			; Save users registers
		phx
		phy
		php			; Save register sizes
		SHORT_AI		; Ensure 8-bit
		jsr	UartHandler	; Service the UART
		plp			; Restore register sizes
		ply			; Restore users registers
		plx
		pla
		rti			; Continue

;===============================================================================
; Uart I/O
;-------------------------------------------------------------------------------

		.longa	off		; Assume 8-bit A
		.longi	off		; Assume 8-bit X/Y
UartHandler:
		lda	ACIA_STAT	; Any ACIA interrupts to process?
		if mi
		 pha			; Yes
		 and	#$10		; TX buffer empty?
		 if ne
		  ldy	TX_HEAD		; Any data to send?
		  cpy	TX_TAIL
		  if ne
		   lda	TX_BUFFER,y	; Yes, extract and send it
		   sta	ACIA_DATA
		   jsr	BumpIdx
		   sty	TX_HEAD
		  else
		   lda	#$01		; No, disable TX interrupt
		   sta	ACIA_CMND
		  endif
		 endif
		 
		 pla
		 and 	#$08		; RX buffer full?
		 if ne
		  lda	ACIA_DATA	; Yes, fetch the character
		  ldy 	RX_TAIL		; .. and save it
		  sta	RX_BUFFER,y
		  jsr	BumpIdx
		  cpy	RX_HEAD		; Is buffer completely full?
		  if ne
		   sty	RX_TAIL		; No, update tail offset
		  endif
		 endif
		endif
		rts			; Done

;-------------------------------------------------------------------------------

; Inserts a character at the end of the TX buffer. If the buffer is completely
; full then wait for a space before updating the indexes.

UartTx:
		pha			; Save callers registers
		phx
		phy
		php			; .. and register sizes
		SHORT_AI		; Make registers 8-bits
		ldy	TX_TAIL		; Save character at tail
		sta	TX_BUFFER,y
		jsr	BumpIdx		; Bump the index value
		repeat
		 cpy	TX_HEAD		; Wait if buffer is full
		until ne		
		sty	TX_TAIL		; Then save the new index
		lda	#$05		; Ensure TX interrupt enabled
		sta	ACIA_CMND
		plp			; Restore register sizes
		ply			; .. and callers values
		plx
		pla
		rts			; And continue

; Extracts the next character from the head of the RX buffer. If the buffer is
; empty then wait for some data to be placed in it by the interrupt handler.

UartRx:
		phx
		phy			; Save callers registers
		php			; .. and register sizes
		SHORT_AI		; Make registers 8-bits
		ldy	RX_HEAD		; Wait if buffer is empty
		repeat
		 cpy	RX_TAIL
		until ne
		lda	RX_BUFFER,y	; Fetch a character
		jsr	BumpIdx		; Bump and same new index
		sty	RX_HEAD		
		plp			; Restore register sizes
		ply			; .. and callers values
		plx
		rts			; And continue

; Increments an index value wrapping it back to zero if it exceeds the buffer
; size.

		.longi	off		; Assume 8-bit X/Y
BumpIdx:
		iny			; Bump index
		cpy	#BUF_SIZE	; Reached end of buffer?
		if cs
		 ldy	#0		; Yes, wrap around
		endif
		rts			; Done

;===============================================================================
; Vectors
;-------------------------------------------------------------------------------

DO_IRQ:		jmp	(IRQV)
DO_NMI:		jmp	(NMIV)
DO_IRQN:	jmp	(IRQNV)
DO_NMIN:	jmp	(NMINV)

; Native Mode Vectors
		
		.org	$ffe4
		
		.word	COPN
		.word	BRKN
		.word	ABORTN
		.word	DO_NMIN
		.word	0
		.word	DO_IRQN

NMIN:		
COPN:
ABORTN:
		rti

NMIE:
COPE:
ABORTE:
		rti

; Emulation Mode Vectors

		.org	$fff4
		
		.word	COPE
		.word	0
		.word	ABORTE
		.word	DO_NMI
		.word	RESET
		.word	DO_IRQ

		.end