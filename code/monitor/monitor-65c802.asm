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
NAK		.equ	$15
CAN		.equ	$18
ESC		.equ	$1b
DEL		.equ	$7f

;===============================================================================
; Instruction and Mode Constants
;-------------------------------------------------------------------------------
		
OP_ADC		.equ	0<<1
OP_AND		.equ	1<<1
OP_ASL		.equ	2<<1
OP_BCC		.equ	3<<1
OP_BCS		.equ	4<<1
OP_BEQ		.equ	5<<1
OP_BIT		.equ	6<<1
OP_BMI		.equ	7<<1
OP_BNE		.equ	8<<1
OP_BPL		.equ	9<<1
OP_BRA		.equ	10<<1
OP_BRK		.equ	11<<1
OP_BRL		.equ	12<<1
OP_BVC		.equ	13<<1
OP_BVS		.equ	14<<1
OP_CLC		.equ	15<<1
OP_CLD		.equ	16<<1
OP_CLI		.equ	17<<1
OP_CLV		.equ	18<<1
OP_CMP		.equ	19<<1
OP_COP		.equ	20<<1
OP_CPX		.equ	21<<1
OP_CPY		.equ	22<<1
OP_DEC		.equ	23<<1
OP_DEX		.equ	24<<1
OP_DEY		.equ	25<<1
OP_EOR		.equ	26<<1
OP_INC		.equ	27<<1
OP_INX		.equ	28<<1
OP_INY		.equ	29<<1
OP_JML		.equ	30<<1
OP_JMP		.equ	31<<1
OP_JSL		.equ	32<<1
OP_JSR		.equ	33<<1
OP_LDA		.equ	34<<1
OP_LDX		.equ	35<<1
OP_LDY		.equ	36<<1
OP_LSR		.equ	37<<1
OP_MVN		.equ	38<<1
OP_MVP		.equ	39<<1
OP_NOP		.equ	40<<1
OP_ORA		.equ	41<<1
OP_PEA		.equ	42<<1
OP_PEI		.equ	43<<1
OP_PER		.equ	44<<1
OP_PHA		.equ	45<<1
OP_PHB		.equ	46<<1
OP_PHD		.equ	47<<1
OP_PHK		.equ	48<<1
OP_PHP		.equ	49<<1
OP_PHX		.equ	50<<1
OP_PHY		.equ	51<<1
OP_PLA		.equ	52<<1
OP_PLB		.equ	53<<1
OP_PLD		.equ	54<<1
OP_PLP		.equ	55<<1
OP_PLX		.equ	56<<1
OP_PLY		.equ	57<<1
OP_REP		.equ	58<<1
OP_ROL		.equ	59<<1
OP_ROR		.equ	60<<1
OP_RTI		.equ	61<<1
OP_RTL		.equ	62<<1
OP_RTS		.equ	63<<1
OP_SBC		.equ	64<<1
OP_SEC		.equ	65<<1
OP_SED		.equ	66<<1
OP_SEI		.equ	67<<1
OP_SEP		.equ	68<<1
OP_STA		.equ	69<<1
OP_STP		.equ	70<<1
OP_STX		.equ	71<<1
OP_STY		.equ	72<<1
OP_STZ		.equ	73<<1
OP_TAX		.equ	74<<1
OP_TAY		.equ	75<<1
OP_TCD		.equ	76<<1
OP_TCS		.equ	77<<1
OP_TDC		.equ	78<<1
OP_TRB		.equ	79<<1
OP_TSB		.equ	80<<1
OP_TSC		.equ	81<<1
OP_TSX		.equ	82<<1
OP_TXA		.equ	83<<1
OP_TXS		.equ	84<<1
OP_TXY		.equ	85<<1
OP_TYA		.equ	86<<1
OP_TYX		.equ	87<<1
OP_WAI		.equ	88<<1
OP_WDM		.equ	89<<1
OP_XBA		.equ	90<<1
OP_XCE		.equ	91<<1

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


;-------------------------------------------------------------------------------

; The MNEM macro compresses three characters into a 16-bit value.

MNEM		.macro	CH1,CH2,CH3
		.word	((((CH3 & $1F) << 5)|(CH2 & $1F)) << 5)|(CH1 & $1F)
		.endm

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.page0
		.org	$00f0
		
E_BIT		.space	1			; E in MSB
REG_P		.space	1
REG_C		.space	2
REG_X		.space	2
REG_Y		.space	2
REG_DP		.space	2
REG_SP		.space	2
REG_PC		.space	2
REG_B		.space	1
REG_K		.space	1

CMD_LEN		.space	1			; Command buffer length
ADDR_S		.space	2
ADDR_E		.space	2

;TEMP		.SPACE	2
;COUNT		.SPACE	1

		
		
		.bss
		.org	$0200

BUF_SIZE	.equ	62
CMD_SIZE	.equ	128

TX_HEAD		.space	1
TX_TAIL		.space	1
RX_HEAD		.space	1
RX_TAIL		.space	1

TX_BUFFER	.space	BUF_SIZE
RX_BUFFER	.space	BUF_SIZE

COMMAND		.space	CMD_SIZE


;===============================================================================
; Entry Points
;-------------------------------------------------------------------------------

		.code
		.org	$f000
		
UARTTX:		.word	_UartTx
UARTRX:		.word	_UartRx
		
;===============================================================================
; Compressed Mnemonics
;-------------------------------------------------------------------------------
		
MNEMONICS:
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
		stz	TX_HEAD		; Clear buffer indexes
		stz	TX_TAIL
		stz	RX_HEAD
		stz	RX_TAIL
		
                lda     #%00011111	; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTRL
                lda     #%11001001	; No parity, no interrupt
                sta     ACIA_CMND
                lda     ACIA_DATA	; Clear receive buffer
		
		cli			; Allow interrupts
		
		ldx	#CRLF		; Show title string
		jsr	ShowString
		ldx	#TITLE
		jsr	ShowString
		
		repeat
		 brk	#0		; And enter monitor via BRK
		forever

;===============================================================================
;-------------------------------------------------------------------------------

		.longa	off
		.longi	on
ShowRegisters:
		jsr	NewLine
		ldx	#STR_PC		; Display the PC
		jsr	ShowString
		lda	REG_PC+1
		xba
		lda	REG_PC+0
		jsr	ShowHex4
		
		ldx	#STR_E		; Display the E bit
		jsr	ShowString
		lda	#'0'
		bit	E_BIT
		if	mi
		 inc	a
		endif
		jsr	UartTx
		
		ldx	#STR_C		; Display A/B
		jsr	ShowString
		lda	REG_C+1
		xba
		lda	REG_C+0
		jsr	ShowAcc
		
		ldx	#STR_X		; Display X
		jsr	ShowString
		lda	REG_X+1
		xba
		lda	REG_X+0
		jsr	ShowReg
		
		ldx	#STR_Y		; Display Y
		jsr	ShowString
		lda	REG_Y+1
		xba
		lda	REG_Y+0
		jsr	ShowReg
		
		ldx	#STR_DP		; Display DP
		jsr	ShowString
		lda	REG_DP+1
		xba
		lda	REG_DP+0
		jsr	ShowHex4
		
		ldx	#STR_SP		; Display SP
		jsr	ShowString
		lda	REG_SP+1
		xba
		lda	REG_SP+0
		jsr	ShowHex4
		
;===============================================================================
; Command Line
;-------------------------------------------------------------------------------

NewCommand:
		stz	CMD_LEN
RptCommand:
		SHORT_AI
		jsr	NewLine
		lda	#'.'
		jsr	UartTx
		
		ldx	#0		; Output prepared command
		repeat
		 cpx	CMD_LEN
		 break cs
		 lda	COMMAND,x
		 jsr	UartTx
		 inx
		forever
		
		repeat
		 jsr	UartRx		; Read a character
		 sta	COMMAND,x	; .. and save
		 
		 cmp	#CR		; End of input?
		 break	eq
		 
		 cmp	#BS		; Turn a backspace
		 if eq
		  lda	#DEL		; .. into a delete
		 endif
		 cmp	#DEL		; Handle delete
		 if eq
		  cpx   #0
		  if ne
		   lda	#BS
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
		 if cc
		  lda	#BEL
		  jsr	UartTx
		  continue
		 endif
		 
		 jsr	UartTx		; Otherwise echo to screen
		 inx			; And bump counter
		forever
		
		ldx	#0		; Prepare to parse command
		jsr	SkipSpaces
		
		cmp	#CR		; Empty line?
		beq	NewCommand	; Yes

;===============================================================================
; Assemble
;-------------------------------------------------------------------------------

		cmp	#'A'
		if eq
		endif

;===============================================================================
; Display Memory
;-------------------------------------------------------------------------------

		cmp	#'M'
		if eq
		endif
		
		
;===============================================================================
; Go
;-------------------------------------------------------------------------------

		cmp	#'G'
		if eq
		endif
		
;===============================================================================
; S19 Record Loader
;-------------------------------------------------------------------------------

		cmp	#'S'
		if eq
		endif
		
;===============================================================================
; Display Help
;-------------------------------------------------------------------------------

		cmp 	#'?'
		if eq
		endif
		
;===============================================================================
; Illegal Command
;-------------------------------------------------------------------------------

		jsr	NewLine
		ldx	#STR_BADCMD
		jsr	ShowString
		jmp	NewCommand
		
;===============================================================================
;-------------------------------------------------------------------------------

; Fetch the next characters from the command buffer ignoring spaces.

		.longa	off
		.longi	off
SkipSpaces:
		repeat
		 lda	COMMAND,x	; Fetch the next character
		 inx
		 cmp 	#' '		; Until not a space
		until ne

;

		.longa	off
ToUpper:
		cmp	#'a'		; Is A lower case letter?
		if cs
		 cmp	#'z'+1
		 if cc
		  and	#$df		; Yes, convert to upper case
		 endif
		endif
		rts			; Done

;

		.longa	off
ShowAcc:
		pha
		bit	E_BIT		; In emulation mode?
		bmi	ShowShort
		lda	#$20
		bit	REG_P		; No, M bit set?
		bne	ShowShort	; Yes, short
		
ShowLong:
		jsr	OpenBracket	; Output [xxxx]
		xba
		jsr	ShowHex2
		pla
		jsr	ShowHex2
		jmp	CloseBracket
		
		.longa	off
ShowReg:
		pha
		bit	E_BIT		; In emulation mode?
		bmi	ShowShort
		lda	#$10
		bit	REG_P		; No, X bit set?
		beq	ShowLong	; No, long

ShowShort:
		xba			; Output xx[xx]
		jsr	ShowHex2
		jsr	OpenBracket
		pla
		jsr	ShowHex2
		jmp	CloseBracket

;-------------------------------------------------------------------------------

		.longa	off
ShowHex4:
		xba			; Recover the MS byte
		jsr	ShowHex2	; Print it
		xba			; Recover the LS byte
		
		.longa	off
ShowHex2:
		pha			; Save the data byte
		lsr	a		; Shift down hi nybble
		lsr	a
		lsr	a
		lsr	a
		jsr	ShowHex1	; Display it
		pla			; Recover the lo nybble

		.longa	off
ShowHex1:
		and	#$0f		; Extract the lo nybble
		ora	#'0'		; And convert to ASCII
		cmp	#'9'+1		; Not a digit?
		if cs
		 adc	#6		; Yes, make it a letter
		endif
		bra	UartTx		; Then print it.

;-------------------------------------------------------------------------------		

		.longa	off
Space:
		lda	#' '
		bra	UartTx
		
		.longa	off
OpenBracket:
		lda	#'['
		bra	UartTx
		
		.longa	off
CloseBracket:
		lda	#']'
		bra	UartTx

		.longa	off
NewLine:
		lda	#CR
		jsr	UartTx
		lda	#LF

UartTx:		jmp	(UARTTX)		

UartRx		jmp	(UARTRX)
	
;===============================================================================
; Strings
;-------------------------------------------------------------------------------




		.longa	off		; A must be 8-bit. X can be either
ShowString:
		repeat
		 lda	STRINGS,x	; Fetch the next character
		 break	eq		; Reached the end?
		 jsr	UartTx		; No, display it
		 inx			; Bump the index
		forever		
		rts			; Done.
		
		
;-------------------------------------------------------------------------------

STRINGS		.equ	$
TITLE		.equ	$-STRINGS
		.byte	"Boot 65C802 [18.02]"
CRLF		.equ	$-STRINGS
		.byte	CR,LF,0
STR_PC		.equ	$-STRINGS
		.byte	"PC=",0
STR_E		.equ	$-STRINGS
		.byte	" E=",0
STR_C		.equ	$-STRINGS
		.byte	" C=",0
STR_X		.equ	$-STRINGS
		.byte	" X=",0
STR_Y		.equ	$-STRINGS
		.byte	" Y=",0
STR_DP		.equ	$-STRINGS
		.byte	" DP=",0
STR_SP		.equ	$-STRINGS
		.byte	" SP=",0
STR_BADCMD	.equ	$-STRINGS
		.byte	"Bad Command",0
		
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

_UartTx:
		pha			; Save callers registers
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
		pla
		rts			; And continue

; Extracts the next character from the head of the RX buffer. If the buffer is
; empty then wait for some data to be placed in it by the interrupt handler.

_UartRx:
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
		rts			; And continue

; Increments an index value wrapping it back to zero if it exceeds the buffer
; size.

		.longi	off		; Assume 8-bit X/Y
BumpIdx:
		iny
		cpy	#BUF_SIZE
		if cs
		 ldy	#0
		endif
		rts

;===============================================================================
; Instruction Lookup Tables
;-------------------------------------------------------------------------------

		.org	$fd00

OPCODES:
		.byte	OP_BRK,OP_ORA,OP_COP,OP_ORA	; 00
		.byte	OP_TSB,OP_ORA,OP_ASL,OP_ORA
		.byte	OP_PHP,OP_ORA,OP_ASL,OP_PHD
		.byte	OP_TSB,OP_ORA,OP_ASL,OP_ORA
		.byte	OP_BPL,OP_ORA,OP_ORA,OP_ORA	; 10
		.byte	OP_TRB,OP_ORA,OP_ASL,OP_ORA
		.byte	OP_CLC,OP_ORA,OP_INC,OP_TCS
		.byte	OP_TRB,OP_ORA,OP_ASL,OP_ORA
		.byte	OP_JSR,OP_AND,OP_JSL,OP_AND	; 20
		.byte	OP_BIT,OP_AND,OP_ROL,OP_AND
		.byte	OP_PLP,OP_AND,OP_ROL,OP_PLD
		.byte	OP_BIT,OP_AND,OP_ROL,OP_AND
		.byte	OP_BMI,OP_AND,OP_AND,OP_AND	; 30
		.byte	OP_BIT,OP_AND,OP_ROL,OP_AND
		.byte	OP_SEC,OP_AND,OP_DEC,OP_TSC
		.byte	OP_BIT,OP_AND,OP_ROL,OP_AND
		.byte	OP_RTI,OP_EOR,OP_WDM,OP_EOR	; 40
		.byte	OP_MVP,OP_EOR,OP_LSR,OP_EOR
		.byte	OP_PHA,OP_EOR,OP_LSR,OP_PHK
		.byte	OP_JMP,OP_EOR,OP_LSR,OP_EOR
		.byte	OP_BVC,OP_EOR,OP_EOR,OP_EOR	; 50
		.byte	OP_MVN,OP_EOR,OP_LSR,OP_EOR
		.byte	OP_CLI,OP_EOR,OP_PHY,OP_TCD
		.byte	OP_JMP,OP_EOR,OP_LSR,OP_EOR
		.byte	OP_RTS,OP_ADC,OP_PER,OP_ADC	; 60
		.byte	OP_STZ,OP_ADC,OP_ROR,OP_ADC
		.byte	OP_PLA,OP_ADC,OP_ROR,OP_RTL
		.byte	OP_JMP,OP_ADC,OP_ROR,OP_ADC
		.byte	OP_BVS,OP_ADC,OP_ADC,OP_ADC	; 70
		.byte	OP_STZ,OP_ADC,OP_ROR,OP_ADC
		.byte	OP_SEI,OP_ADC,OP_PLY,OP_TDC
		.byte	OP_JMP,OP_ADC,OP_ROR,OP_ADC
		.byte	OP_BRA,OP_STA,OP_BRL,OP_STA	; 80
		.byte	OP_STY,OP_STA,OP_STX,OP_STA
		.byte	OP_DEY,OP_BIT,OP_TXA,OP_PHB
		.byte	OP_STY,OP_STA,OP_STX,OP_STA
		.byte	OP_BCC,OP_STA,OP_STA,OP_STA	; 90
		.byte	OP_STY,OP_STA,OP_STX,OP_STA
		.byte	OP_TYA,OP_STA,OP_TXS,OP_TXY
		.byte	OP_STZ,OP_STA,OP_STZ,OP_STA
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_LDA	; A0
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_LDA
		.byte	OP_TAY,OP_LDA,OP_TAX,OP_PLB
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_LDA
		.byte	OP_BCS,OP_LDA,OP_LDA,OP_LDY	; B0
		.byte	OP_LDA,OP_LDY,OP_LDX,OP_LDA
		.byte	OP_CLV,OP_LDA,OP_TSX,OP_TYX
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_LDA
		.byte	OP_CPY,OP_CMP,OP_REP,OP_CMP	; C0
		.byte	OP_CPY,OP_CMP,OP_DEC,OP_CMP
		.byte	OP_INY,OP_CMP,OP_DEX,OP_WAI
		.byte	OP_CPY,OP_CMP,OP_DEC,OP_CMP
		.byte	OP_BNE,OP_CMP,OP_CMP,OP_CMP	; D0
		.byte	OP_PEI,OP_CMP,OP_DEC,OP_CMP
		.byte	OP_CLD,OP_CMP,OP_PHX,OP_STP
		.byte	OP_JML,OP_CMP,OP_DEC,OP_CMP
		.byte	OP_CPX,OP_SBC,OP_SEP,OP_SBC	; E0
		.byte	OP_CPX,OP_SBC,OP_INC,OP_SBC
		.byte	OP_INX,OP_SBC,OP_NOP,OP_XBA
		.byte	OP_CPX,OP_SBC,OP_INC,OP_SBC
		.byte	OP_BEQ,OP_SBC,OP_SBC,OP_SBC	; F0
		.byte	OP_PEA,OP_SBC,OP_INC,OP_SBC
		.byte	OP_SED,OP_SBC,OP_PLX,OP_XCE
		.byte	OP_JSR,OP_SBC,OP_INC,OP_SBC

MODES:
		.byte	MO_INT,MO_DIX,MO_INT,MO_STK	; 00
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; 10
		.byte	MO_DPG,MO_DPX,MO_DPX,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP
		.byte	MO_ABS,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_ABS,MO_DIX,MO_ALG,MO_STK	; 20
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; 30
		.byte	MO_DPX,MO_DPX,MO_DPX,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP
		.byte	MO_ABX,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMP,MO_DIX,MO_INT,MO_STK	; 40
		.byte	MO_MOV,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; 50
		.byte	MO_MOV,MO_DPX,MO_DPX,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP
		.byte	MO_ALG,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMP,MO_DIX,MO_IMP,MO_STK	; 60
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP
		.byte	MO_AIN,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; 70
		.byte	MO_DPX,MO_DPX,MO_DPX,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP
		.byte	MO_AIX,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_REL,MO_DIX,MO_RLG,MO_STK	; 80
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; 90
		.byte	MO_DPX,MO_DPX,MO_DPY,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP
		.byte	MO_ABS,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMX,MO_DIX,MO_IMX,MO_STK	; A0
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; B0
		.byte	MO_DPX,MO_DPX,MO_DPY,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP
		.byte	MO_ABX,MO_ABX,MO_ABY,MO_ALX
		.byte	MO_IMX,MO_DIX,MO_INT,MO_STK	; C0
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; D0
		.byte	MO_IMP,MO_DPX,MO_DPX,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP
		.byte	MO_AIN,MO_ABX,MO_ABX,MO_ALX
		.byte	MO_IMX,MO_DIX,MO_INT,MO_STK	; E0
		.byte	MO_DPG,MO_DPG,MO_DPG,MO_DLI
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP
		.byte	MO_ABS,MO_ABS,MO_ABS,MO_ALG
		.byte	MO_REL,MO_DIY,MO_DIN,MO_SKY	; F0
		.byte	MO_IMP,MO_DPX,MO_DPX,MO_DLY
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP
		.byte	MO_AIX,MO_ABX,MO_ABX,MO_ALX

;===============================================================================
; I/O Page
;-------------------------------------------------------------------------------

		.org	$ff00
		.space	64
		
;===============================================================================
; Break Handler
;-------------------------------------------------------------------------------

BRKE:
BRKN:
		clc			; Ensure native mode
		xce
		ror	E_BIT		; .. but save the original
		
		LONG_AI			; Make all registers 16-bit
		sta	REG_C		; Save A/B
		stx	REG_X		; .. X
		sty	REG_Y		; .. Y
		tdc			; Save DP
		sta	REG_DP
		
		SHORT_A			; Make A register 8-bits
		bit	E_BIT		; 
		if pl
	; Banks
	
		endif
		
		pla			; Save P
		sta	REG_P
		plx			; Save PC
		dex
		dex
		stx	REG_PC
		tsx			; Save SP
		stx	REG_SP
		
		cli

		jmp	ShowRegisters
		
;===============================================================================
; Interrupt Handlers
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
		phy
		php			; Save register sizes
		SHORT_AI		; Ensure 8-bit
		jsr	UartHandler	; Service the UART
		plp			; Restore register sizes
		ply			; Restore users registers
		pla
		rti			; Continue

;===============================================================================
; Vectors
;-------------------------------------------------------------------------------

; Native Mode Vectors
		
		.org	$ffe4
		
		.word	COPN
		.word	BRKN
		.word	ABORTN
		.word	NMIN
		.word	0
		.word	IRQN

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
		.word	NMIE
		.word	RESET
		.word	IRQE

		.end