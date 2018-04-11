
;===============================================================================
; ASCII Control Characters
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

OP_ERR		.equ	$00
OP_ADC		.equ	$02
OP_AND		.equ	$04
OP_ASL		.equ	$06
		.if	__65C02__
OP_BBR		.equ	$08
OP_BBS		.equ	$0A
		.endif
OP_BCC		.equ	$0C
OP_BCS		.equ	$0E
OP_BEQ		.equ	$10
OP_BIT		.equ	$12
OP_BNE		.equ	$14
OP_BMI		.equ	$16
OP_BPL		.equ	$18
		.if	__65C02__
OP_BRA		.equ	$1A
		.endif
OP_BRK		.equ	$1C
OP_BVC		.equ	$1E
OP_BVS		.equ	$20
OP_CLC		.equ	$22
OP_CLD		.equ	$24
OP_CLI		.equ	$26
OP_CLV		.equ	$28
OP_CMP		.equ	$2A
OP_CPX		.equ	$2C
OP_CPY		.equ	$2E
OP_DEC		.equ	$30
OP_DEX		.equ	$32
OP_DEY		.equ	$34
OP_EOR		.equ	$36
OP_INC		.equ	$38
OP_INX		.equ	$3A
OP_INY		.equ	$3C
OP_jmp		.equ	$3E
OP_JSR		.equ	$40
OP_LDA		.equ	$42
OP_LDX		.equ	$44
OP_LDY		.equ	$46
OP_LSR		.equ	$48
OP_NOP		.equ	$4A
OP_ORA		.equ	$4C
OP_pha		.equ	$4E
OP_PHP		.equ	$50
		.if	__65C02__
OP_PHX		.equ	$52
OP_PHY		.equ	$54
		.endif
OP_PLA		.equ	$56
OP_PLP		.equ	$58
		.if	__65C02__
OP_PLX		.equ	$5A
OP_PLY		.equ	$5C
OP_RMB		.equ	$5E
		.endif
OP_ROL		.equ	$60
OP_ROR		.equ	$62
OP_RTI		.equ	$64
OP_RTS		.equ	$66
OP_SBC		.equ	$68
OP_SEC		.equ	$6A
OP_SED		.equ	$6C
OP_SEI		.equ	$6E
		.if	__65C02__
OP_SMB		.equ	$70
		.endif
OP_STA		.equ	$72
		.if	__65C02__
OP_STP		.equ	$74
		.endif
OP_STX		.equ	$76
OP_STY		.equ	$78
		.if	__65C02__
OP_STZ		.equ	$7A
		.endif
OP_TAX		.equ	$7C
OP_TAY		.equ	$7E
		.if	__65C02__
OP_TRB		.equ	$80
OP_TSB		.equ	$82
		.endif
OP_TSX		.equ	$84
OP_TXA		.equ	$86
OP_TXS		.equ	$88
OP_TYA		.equ	$8A
		.if	__65C02__
OP_WAI		.equ	$8C
		.endif

; Bit patterns for addressing modes

MB_IND		.equ	%10000000
MB_REL		.equ	%01000000
MB_bit		.equ	%00100000

MB_IMP		.equ	%00000000
MB_ZPG		.equ	%00000001
MB_IMM		.equ	%00000010
MB_ABS		.equ	%00000011

MB_ACC		.equ	%00010000
MB_XRG		.equ	%00001000
MB_YRG		.equ	%00000100

; Addressing modes

		.if	__65C02__
MO_BIT		.equ	MB_bit	     |MB_ZPG
MO_BRL		.equ	MB_bit|MB_REL|MB_ZPG
		.endif
MO_ACC		.equ	       MB_ACC|MB_IMP
MO_IMP		.equ		      MB_IMP
MO_IMM		.equ		      MB_IMM
MO_REL		.equ		      MB_REL
MO_ZPG		.equ		      MB_ZPG
MO_ZPX		.equ	       MB_YRG|MB_ZPG
MO_ZPY		.equ	       MB_YRG|MB_ZPG
		.if	__65C02__
MO_IZP		.equ	MB_IND	     |MB_ZPG
		.endif
MO_IZX		.equ	MB_IND|MB_XRG|MB_ZPG
MO_IZY		.equ	MB_IND|MB_YRG|MB_ZPG
MO_ABS		.equ		      MB_ABS
MO_ABX		.equ	       MB_XRG|MB_ABS
MO_ABY		.equ	       MB_YRG|MB_ABS
MO_IAB		.equ	MB_IND	     |MB_ABS
		.if	__65C02__
MO_IAX		.equ	MB_IND|MB_XRG|MB_ABS
		.endif

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
		.org	$00f0

A_REG		.space	1
X_REG		.space	1
Y_REG		.space	1
P_REG		.space	1
PC_REG		.space	2

CMD_LEN		.space	1			; Command buffer length
ADDR_S		.space	2
ADDR_E		.space	2

TEMP		.space	2
COUNT		.space	1

;-------------------------------------------------------------------------------

		.org	$00fe

IO_TEMP		.space	1

;===============================================================================
; UART Buffers
;-------------------------------------------------------------------------------

BUF_SIZE	.equ	62
		.bss
		.org	$0200

		.space	8		; Vectors

; Communications buffer offsets

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
		MNEM    'A','D','C'
		MNEM    'A','N','D'
		MNEM    'A','S','L'
		MNEM    'B','B','R'
		MNEM    'B','B','S'
		MNEM    'B','C','C'
		MNEM    'B','C','S'
		MNEM    'B','E','Q'
		MNEM    'B','I','T'
		MNEM    'B','N','E'
		MNEM    'B','M','I'
		MNEM    'B','P','L'
		MNEM    'B','R','A'
		MNEM    'B','R','K'
		MNEM    'B','V','C'
		MNEM    'B','V','S'
		MNEM    'C','L','C'
		MNEM    'C','L','D'
		MNEM    'C','L','I'
		MNEM    'C','L','V'
		MNEM    'C','M','P'
		MNEM    'C','P','X'
		MNEM    'C','P','Y'
		MNEM    'D','E','C'
		MNEM    'D','E','X'
		MNEM    'D','E','Y'
		MNEM    'E','O','R'
		MNEM    'I','N','C'
		MNEM    'I','N','X'
		MNEM    'I','N','Y'
		MNEM    'J','M','P'
		MNEM    'J','S','R'
		MNEM    'L','D','A'
		MNEM    'L','D','X'
		MNEM    'L','D','Y'
		MNEM    'L','S','R'
		MNEM    'N','O','P'
		MNEM    'O','R','A'
		MNEM    'P','H','A'
		MNEM    'P','H','P'
		MNEM    'P','H','X'
		MNEM    'P','H','Y'
		MNEM    'P','L','A'
		MNEM    'P','L','P'
		MNEM    'P','L','X'
		MNEM    'P','L','Y'
		MNEM    'R','M','B'
		MNEM    'R','O','L'
		MNEM    'R','O','R'
		MNEM    'R','T','I'
		MNEM    'R','T','S'
		MNEM    'S','B','C'
		MNEM    'S','E','C'
		MNEM    'S','E','D'
		MNEM    'S','E','I'
		MNEM    'S','M','B'
		MNEM    'S','T','A'
		MNEM    'S','T','P'
		MNEM    'S','T','X'
		MNEM    'S','T','Y'
		MNEM    'S','T','Z'
		MNEM    'T','A','X'
		MNEM    'T','A','Y'
		MNEM    'T','R','B'
		MNEM    'T','S','B'
		MNEM    'T','S','X'
		MNEM    'T','X','A'
		MNEM    'T','X','S'
		MNEM    'T','Y','A'
		MNEM    'W','A','I'
		
;===============================================================================
; Power On Reset
;-------------------------------------------------------------------------------

RESET:
		sei
		cld			; Ensure binary mode
		
		ldx	#$FF		; Reset the stack
		txs

		inx			; Clear buffer offsets
		stx	RX_HEAD
		stx	RX_TAIL
		stx	TX_HEAD
		stx	TX_TAIL

                lda     #%00011111	; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTRL
                lda     #%11001001	; No parity, no interrupt
                sta     ACIA_CMND
                lda     ACIA_DATA	; Clear receive buffer

		cli			; Allow interrupts
		
		jsr	NewLine
		ldx	#TTL_STR
		jsr	ShowString
		
		repeat
		 brk	#0		; And enter monitor
		forever
		
;===============================================================================
;
;-------------------------------------------------------------------------------

BRK:
		pla			; Pull off saved registers
		sta	Y_REG
		pla
		sta	X_REG
		pla
		sta	A_REG
		pla
		sta	P_REG
		sec			; Move PC back to BRK
		pla
		sbc	#2
		sta	PC_REG+0
		pla
		sbc	#0
		sta	PC_REG+1
		
		cli			; Allow interrupts

;===============================================================================
; Show Registers
;-------------------------------------------------------------------------------

ShowRegisters:
		jsr	NewLine
		ldx	#PC_STR		; Display the PC
		jsr	ShowString
		lda	PC_REG+1
		jsr	ShowHex2
		lda	PC_REG+0
		jsr	ShowHex2

		ldx	#A_STR		; Display A
		jsr	ShowString
		lda	A_REG
		jsr	ShowHex2

		ldx	#X_STR		; Display X
		jsr	ShowString
		lda	X_REG
		jsr	ShowHex2

		ldx	#Y_STR		; Display Y
		jsr	ShowString
		lda	Y_REG
		jsr	ShowHex2
		
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
		
		ldx	#SP_STR		; Display SP
		jsr	ShowString
		tsx
		txa
		jsr	ShowHex2
		
;===============================================================================
;-------------------------------------------------------------------------------

NewCommand:
		.if	__65C02__
		stz	CMD_LEN		; Mark the buffer as empty
		.else
		lda	#0		; Mark the buffer as empty
		sta	CMD_LEN
		.endif

RptCommand:
		jsr	NewLine		; Move cursor to next line
		lda	#'.'		; And output the prompt
		jsr	UartTx

		ldx	#0		; Output prepared command
		repeat
		 cpx	CMD_LEN		; Any saved characters to display?
		 break	cs		; No
		 lda	COMMAND,x	; Yes, print from the buffer
		 jsr	UartTx
		 inx
		forever
		
		lda	#DC1		; Send XON
		jsr	UartTx
		repeat
		 jsr	UartRx		; Wait for some user input
		 sta	COMMAND,x
		 
		 cmp	#CR		; End of input?
		 break eq
		 
		 cmp 	#ESC		; Cancel input?
		 if eq
		  beq	NewCommand	; Yes
		 endif

		 cmp	#DEL		; Turn a delete
		 if eq
		  lda	#BS		; .. into a backspace
		 endif
		 
		 cmp	#BS		; Handle backspace
		 if eq
		  cpx   #0
		  if ne
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
		
		stx	CMD_LEN		; Save the command length
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
		if	EQ
		 jsr	GET_WORD
		 if	CC
		  jsr	SET_ADDR_S
		  jsr	SET_ADDR_E
		  jsr	GET_WORD
		  if	CC
		   jsr	SET_ADDR_E
		  else
		   inc	ADDR_E+1
		  endif
		  
		  repeat
		   jsr	NewLine		; Print the memory address
		   lda	ADDR_S+1
		   jsr	ShowHex2
		   lda	ADDR_S+0
		   jsr	ShowHex2
		   
		   jsr	DISASSEMBLE
		   jsr	BUMP_ADDR		   
		   jsr	CHECK_END
		  until	PL
		  jmp	NewCommand
		 endif
		 jmp	Error		
		endif
		
;===============================================================================
; 'G' - Go
;-------------------------------------------------------------------------------

		cmp	#'G'
		if	EQ

		 lda	PC_REG+1	; Push the target address
		 pha
		 lda	PC_REG+0
		 pha
		 lda	P_REG		; And status flags
		 pha
		 lda	A_REG		; Reload A, X and Y
		 ldx	X_REG
		 ldy	Y_REG
		 rti			; Then go to code
		endif

;===============================================================================
; 'M' - Show Memory
;-------------------------------------------------------------------------------		

		cmp	#'M'
		if	EQ
		 jsr	GET_WORD
		 if	CC
		  jsr	SET_ADDR_S
		  jsr	SET_ADDR_E
		  jsr	GET_WORD
		  if	CC
		   jsr	SET_ADDR_E
		  else
		   inc	ADDR_E+1
		  endif
		  
		  repeat
		   jsr	NewLine		; Print the memory address
		   lda	ADDR_S+1
		   jsr	ShowHex2
		   lda	ADDR_S+0
		   jsr	ShowHex2
		   
		   ldy	#0		; Dump 16 bytes of data
		   repeat
		    jsr	Space
		    lda	(ADDR_S),Y
		    iny
		    jsr	ShowHex2
		    cpy #16
		   until EQ
		   
		   jsr	Space		; Then show as characters
		   jsr	Bar
		   ldy	#0
		   repeat
		    lda	(ADDR_S),Y
		    iny
		    jsr	IS_PRINTABLE
		    if CC
		     lda #'.'
		    endif
		    jsr	UartTx
		    cpy	#16
		   until EQ
		   jsr	Bar
		   
		   tya
		   jsr	BUMP_ADDR
		   jsr	CHECK_END
		  until	PL
		  jmp	NewCommand
		 endif
		 jmp	Error
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
		if	eq
		endif
		
;===============================================================================
; 'W' - Write Memory
;-------------------------------------------------------------------------------

		cmp	#'W'
		if	EQ
		 jsr	GET_WORD	; Get the target address
		 if	CC
		  jsr	SET_ADDR_S	; Copy to start address
		  jsr	GET_BYTE	; Get the value
		  if	CC
		   ldy	#0		; Write to  memory
		   lda	TEMP+0
		   sta	(ADDR_S),Y
		   lda	#1		; Increment address
		   jsr	BUMP_ADDR
		   lda	#'W'		; Create prompt for next byte
		   jmp	SET_PROMPT
		  endif
		 endif
		 jmp	Error		; Handle syntax errors
		endif
		
;===============================================================================
; '?' - Display Help
;-------------------------------------------------------------------------------

		cmp	#'?'
		if	EQ
		 ldx	#HLP_STR
		else
Error:		 ldx	#ERR_STR
		endif
		jsr	ShowString
		jmp	NewCommand

;===============================================================================
;-------------------------------------------------------------------------------

SET_ADDR_S:
		lda	TEMP+0
		sta	ADDR_S+0
		lda	TEMP+1
		sta	ADDR_S+1
		rts

SET_ADDR_E:
		lda	TEMP+0
		sta	ADDR_E+0
		lda	TEMP+1
		sta	ADDR_E+1
		rts

BUMP_ADDR:
		clc
		adc	ADDR_S+0
		sta	ADDR_S+0
		if	CS
		 inc	ADDR_S+1
		endif
		rts
		
CHECK_END:
		sec
		lda	ADDR_S+0
		sbc	ADDR_E+0
		lda	ADDR_S+1
		sbc	ADDR_E+1
		rts

; Create a prompt string in the command buffer for the command in A using the
; current value of the starting address.

SET_PROMPT:
		ldx	#0		; Clear buffer and add command letter
		jsr	APPEND_CHAR
		lda	#' '		; Then a space
		jsr	APPEND_CHAR
		
		lda	ADDR_S+1	; Followed by the address
		jsr	APPEND_HEX2
		lda	ADDR_S+0
		jsr	APPEND_HEX2
		lda	#' '		; And another space
		jsr	APPEND_CHAR
		jmp	RptCommand	; Then output it

; Convert the byte in A into hexadecimal digits and append to the command buffer.		
		
APPEND_HEX2:
		pha
		lsr	A
		lsr	A
		lsr	A
		lsr	A
		jsr	APPEND_HEX
		pla
APPEND_HEX:
		jsr	TO_HEX

; Append the character in A to the command buffer to create the next prompt
; string.

APPEND_CHAR:
		sta	COMMAND,X
		inx
		stx	CMD_LEN
		rts
				
;===============================================================================
; Parsing Utilities
;-------------------------------------------------------------------------------

; Get the next characater from the command buffer indicated by the X register
; and convert it to UPPER case. If the carry is set then the end of the buffer
; has been reached.

NextChar:
		cpx	CMD_LEN		; Reached end of buffer>
		if cs
		 rts			; Yes, return with C=1
		endif
		lda	COMMAND,X	; No, fetch a character
		inx

; Convert the character in A to upper case.

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

GET_WORD:
		ldy	#4		; Set maximim number of nybbles
		bne	GET_BYTE+2


; Parse a word from the command buffer and store it at 0,Y. Return if the
; carry set if there is a syntax error.

GET_BYTE:
		ldy	#2		; Set maximum number of nybble
		sty	COUNT
		
		.if	__65C02__
		stz	TEMP+0		; Clear conversion area
		stz	TEMP+1
		.else
		ldy	#0
		sty	TEMP+0		; Clear conversion area
		sty	TEMP+1
		.endif

		jsr	SkipSpaces	; Fetch first character
		jsr	GET_NYBBLE	; And try to convert
		if	CS
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
		 break	EQ
		 
		 jsr	NextChar	; Try for another nybble
		 jsr	GET_NYBBLE
		until CS		
		clc			; Conversion sucessfull
		rts

;
;

GET_NYBBLE:
		jsr	IS_HEX		; Got a hex digit?
		if	CS
		 cmp	#'A'		; Handle letters
		 if	CS
		  sbc	#7
		 endif
		 and	#$0F		; Skip out nybble
		 clc			; Done
		 rts
		endif
		sec			; Set carry -- not hex
		rts

; Return with the carry set of the character in A is a digit or 'A' thru 'F'.

IS_HEX:
		cmp	#'9'+1
		if 	CC
		 cmp	#'0'
		 rts
		endif
		cmp	#'F'+1
		if	CC
		 cmp	#'A'
		 rts
		endif
		clc
		rts
		
IS_PRINTABLE:
		cmp	#' '
		if	CS
		 cmp	#DEL
		 if	CC
		  sec
		  rts
		 endif
		endif
		clc
		rts
		
;===============================================================================
;-------------------------------------------------------------------------------

DISASSEMBLE:
		jsr	Space
		ldy	#0		; Fetch the opcode
		lda	(ADDR_S),Y
		tax
		jsr	ShowHex2		; And display it
		
		jsr	Space
		lda	MODES,X
		pha
		pha
		and	#MB_REL|MB_ABS
		if	NE
		 ldy	#1
		 lda	(ADDR_S),Y
		 jsr	ShowHex2
		else
		 jsr	Space2
		endif
		
		jsr	Space
		pla
		tay
		and	#MB_REL|MB_ZPG
		cmp	#MB_REL|MB_ZPG
		if	NE
		 tya
		 and	#MB_ABS
		 cmp	#MB_ABS
		endif
		if	EQ
		 ldy	#2
		 lda	(ADDR_S),Y
		 jsr	ShowHex2
		else
		 jsr	Space2
		endif
		
		iny			; Save the byte count
		tya
		pha

		jsr	Space
		ldy	#0		; Fetch the opcode
		lda	(ADDR_S),Y
		tax
		lda	OPCODES,X
		tax
		lda	MNEMONICS+1,X
		sta	TEMP
		lda	MNEMONICS+0,X
		jsr	EXTRACT_LETTER
		jsr	EXTRACT_LETTER
		jsr	EXTRACT_LETTER
		jsr	Space
		
		pla
		pha
		and	#MB_bit
		if	NE
		 ldy	#0
		 lda	(ADDR_S),Y
		 and	#7
		 ora	#'0'
		 jsr	UartTx
		 lda	#','
		 jsr	UartTx
		endif
		
		pla
		pha
		if	MI
		 lda	#'('
		 jsr	UartTx
		endif
		
		pla
		pha
		and	#MB_ABS
		if	NE
		 pha
		 cmp	#MB_IMM
		 if	EQ
		  lda	#'#'
		  jsr	UartTx
		 endif
		 lda	#'$'
		 jsr	UartTx
		 pla
		 cmp	#MB_ABS
		 if	EQ
		  ldy	#2
		  lda	(ADDR_S),Y
		  jsr	ShowHex2
		 endif
		 ldy	#1
		 lda	(ADDR_S),Y
		 jsr	ShowHex2
		endif
		
		pla
		pha
		and	#MB_bit|MB_REL
		cmp	#MB_bit|MB_REL
		if	EQ
		 lda	#','
		 jsr	UartTx
		endif
		
		pla
		pha
		tay
		and	#MB_REL
		if	NE
		 lda	#'$'
		 jsr	UartTx
		 tya
		 ldy	#1
		 and	#MB_bit
		 if	NE
		  iny
		 endif
		 
		 lda	#'r'
		 jsr	UartTx
		endif
		
		pla
		pha
		and	#MB_ACC
		if 	NE
		 lda	#'A'
		 jsr	UartTx
		endif
		
		pla
		pha
		and	#MB_XRG
		if 	NE
		 lda	#','
		 jsr	UartTx
		 lda	#'X'
		 jsr	UartTx
		endif
		
		pla
		pha
		if	MI
		 lda	#')'
		 jsr	UartTx
		endif
		
		pla
		and	#MB_YRG
		if 	NE
		 lda	#','
		 jsr	UartTx
		 lda	#'Y'
		 jsr	UartTx
		endif
		
		pla			; Return the number of bytes
		rts
		
EXTRACT_LETTER:
		pha
		and	#$1F
		ora	#'@'
		jsr	UartTx
		pla
		lsr	TEMP
		ror	A
		lsr	TEMP
		ror	A
		lsr	TEMP
		ror	A
		lsr	TEMP
		ror	A
		lsr	TEMP
		ror	A
		rts
		
		
;===============================================================================
; Display Utilities
;-------------------------------------------------------------------------------

; Display the byte in A as two hexadecimal digits. The values in A & Y are
; destroyed.

ShowHex2:
		pha			; Save a copy of the value
		lsr	A		; Shift down the hi nybble
		lsr	A
		lsr	A
		lsr	A
		jsr	HEX		; Convert and display
		pla			; Pull back value and ...

; Display the lo nybble of A as a hexadecimal digit. The values in A & Y are
; destroyed.

HEX		jsr	TO_HEX		; Convert to printable character
		jmp	UartTx		; And display.
		
;
		
TO_HEX		and	#$0F		; Isolate the lo nybble
		sed			; Converted to ASCII
		clc
		adc	#$90
		adc	#$40
		cld
		rts

; Output two spaces.

Space2:
		jsr	Space
		
; Output a single space. The values in A & Y are destroyed.

Space:
		lda	#' '
		jmp	UartTx
		
Bar:
		lda	#'|'
		jmp	UartTx

;-------------------------------------------------------------------------------


ShowString:
		repeat
		 lda	STRINGS,X
		 if	EQ
		  rts
		 endif
		 jsr	UartTx
		 inx
		forever

STRINGS:
TTL_STR		.equ	.-STRINGS
		.if	__6502__
		.byte	"Boot 6502 [18.04]"
		.endif
		.if	__65C02__
		.byte	"Boot 65C02 [18.04]"
		.endif
		.byte	0
PC_STR		.equ	.-STRINGS
		.byte	"PC=",0
SP_STR		.equ	.-STRINGS
		.byte	" SP=",0
P_STR		.equ	.-STRINGS
		.byte	" P=",0
A_STR		.equ	.-STRINGS
		.byte	" A=",0
X_STR		.equ	.-STRINGS
		.byte	" X=",0
Y_STR		.equ	.-STRINGS
		.byte	" Y=",0
ERR_STR		.equ	.-STRINGS
		.byte	CR,LF,"?",0
HLP_STR		.equ	.-STRINGS
		.byte 	CR,LF,"D xxxx yyyy\t\tDisassemble"
		.byte	CR,LF,"G [xxxx]\t\tGoto"
		.byte	CR,LF,"M xxxx yyyy\t\tDisplay Memory"
		.byte	CR,LF,"R\t\t\tDisplay Registers"
		.byte	CR,LF,"S...\t\t\tS19 Load"
		.byte	CR,LF,"W xxxx yy\t\tWrite Memory"
		.byte 	0
		
FLAG		.byte	"CZID11VN"
BITS		.byte	$01,$02,$04,$08,$10,$20,$40,$80

;===============================================================================
; Instruction Lookup Tables
;-------------------------------------------------------------------------------

		.org	$fc00
		
		.if	__6502__
OPCODES:
		.byte	OP_BRK,OP_ORA,OP_ERR,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR ; 0
		.byte	OP_PHP,OP_ORA,OP_ASL,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR
		.byte	OP_BPL,OP_ORA,OP_ERR,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR ; 1
		.byte	OP_CLC,OP_ORA,OP_INC,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR
		.byte	OP_JSR,OP_AND,OP_ERR,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR ; 2
		.byte	OP_PLP,OP_AND,OP_ROL,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR
		.byte	OP_BMI,OP_AND,OP_ERR,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR ; 3
		.byte	OP_SEC,OP_AND,OP_DEC,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR
		.byte	OP_RTI,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_ERR ; 4
		.byte	OP_pha,OP_EOR,OP_LSR,OP_ERR,OP_jmp,OP_EOR,OP_LSR,OP_ERR
		.byte	OP_BVC,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_ERR ; 5
		.byte	OP_CLI,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_ERR
		.byte	OP_RTS,OP_ADC,OP_ERR,OP_ERR,OP_ERR,OP_ADC,OP_ROR,OP_ERR ; 6
		.byte	OP_PLA,OP_ADC,OP_ROR,OP_ERR,OP_jmp,OP_ADC,OP_ROR,OP_ERR
		.byte	OP_BVS,OP_ADC,OP_ERR,OP_ERR,OP_ERR,OP_ADC,OP_ROR,OP_ERR ; 7
		.byte	OP_SEI,OP_ADC,OP_ERR,OP_ERR,OP_ERR,OP_ADC,OP_ROR,OP_ERR
		.byte	OP_ERR,OP_STA,OP_ERR,OP_ERR,OP_STY,OP_STA,OP_STX,OP_ERR ; 8
		.byte	OP_DEY,OP_BIT,OP_TXA,OP_ERR,OP_STY,OP_STA,OP_STX,OP_ERR
		.byte	OP_BCC,OP_STA,OP_ERR,OP_ERR,OP_STY,OP_STA,OP_STX,OP_ERR ; 9
		.byte	OP_TYA,OP_STA,OP_TXS,OP_ERR,OP_ERR,OP_STA,OP_ERR,OP_ERR
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR ; A
		.byte	OP_TAY,OP_LDA,OP_TAX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR
		.byte	OP_BCS,OP_LDA,OP_ERR,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR ; B
		.byte	OP_CLV,OP_LDA,OP_TSX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR
		.byte	OP_CPY,OP_CMP,OP_ERR,OP_ERR,OP_CPY,OP_CMP,OP_DEC,OP_ERR ; C
		.byte	OP_INY,OP_CMP,OP_DEX,OP_ERR,OP_CPY,OP_CMP,OP_DEC,OP_ERR
		.byte	OP_BNE,OP_CMP,OP_ERR,OP_ERR,OP_ERR,OP_CMP,OP_DEC,OP_ERR ; D
		.byte	OP_CLD,OP_CMP,OP_ERR,OP_ERR,OP_ERR,OP_CMP,OP_DEC,OP_ERR
		.byte	OP_CPX,OP_SBC,OP_ERR,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_ERR ; E
		.byte	OP_INX,OP_SBC,OP_NOP,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_ERR
		.byte	OP_BEQ,OP_SBC,OP_ERR,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_ERR ; F
		.byte	OP_SED,OP_SBC,OP_ERR,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_ERR

MODES:
		.byte	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_IMP ; 0
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; 1
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.byte	MO_ABS,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; 2
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPX,MO_IMP ; 3
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABX,MO_ABX,MO_ABX,MO_IMP
		.byte	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_IMP ; 4
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; 5
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.byte	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_IMP ; 6
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_IAB,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; 7
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.byte	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; 8
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_IMP ; 9
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_IMP,MO_IMP
		.byte	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; A
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_IMP ; B
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_ABY,MO_IMP
		.byte	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; C
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; D
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.byte	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; E
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.byte	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; F
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.endif

;-------------------------------------------------------------------------------

		.if	__65C02__
OPCODES:
		.byte	OP_BRK,OP_ORA,OP_ERR,OP_ERR,OP_TSB,OP_ORA,OP_ASL,OP_RMB ; 0
		.byte	OP_PHP,OP_ORA,OP_ASL,OP_ERR,OP_TSB,OP_ORA,OP_ASL,OP_BBR
		.byte	OP_BPL,OP_ORA,OP_ORA,OP_ERR,OP_TRB,OP_ORA,OP_ASL,OP_RMB ; 1
		.byte	OP_CLC,OP_ORA,OP_INC,OP_ERR,OP_TRB,OP_ORA,OP_ASL,OP_BBR
		.byte	OP_JSR,OP_AND,OP_ERR,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_RMB ; 2
		.byte	OP_PLP,OP_AND,OP_ROL,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_BBR
		.byte	OP_BMI,OP_AND,OP_AND,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_RMB ; 3
		.byte	OP_SEC,OP_AND,OP_DEC,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_BBR
		.byte	OP_RTI,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_RMB ; 4
		.byte	OP_pha,OP_EOR,OP_LSR,OP_ERR,OP_jmp,OP_EOR,OP_LSR,OP_BBR
		.byte	OP_BVC,OP_EOR,OP_EOR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_RMB ; 5
		.byte	OP_CLI,OP_EOR,OP_PHY,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_BBR
		.byte	OP_RTS,OP_ADC,OP_ERR,OP_ERR,OP_STZ,OP_ADC,OP_ROR,OP_RMB ; 6
		.byte	OP_PLA,OP_ADC,OP_ROR,OP_ERR,OP_jmp,OP_ADC,OP_ROR,OP_BBR
		.byte	OP_BVS,OP_ADC,OP_ADC,OP_ERR,OP_STZ,OP_ADC,OP_ROR,OP_RMB ; 7
		.byte	OP_SEI,OP_ADC,OP_PLY,OP_ERR,OP_jmp,OP_ADC,OP_ROR,OP_BBR
		.byte	OP_BRA,OP_STA,OP_ERR,OP_ERR,OP_STY,OP_STA,OP_STX,OP_SMB ; 8
		.byte	OP_DEY,OP_BIT,OP_TXA,OP_ERR,OP_STY,OP_STA,OP_STX,OP_BBS
		.byte	OP_BCC,OP_STA,OP_STA,OP_ERR,OP_STY,OP_STA,OP_STX,OP_SMB ; 9
		.byte	OP_TYA,OP_STA,OP_TXS,OP_ERR,OP_STZ,OP_STA,OP_STZ,OP_BBS
		.byte	OP_LDY,OP_LDA,OP_LDX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_SMB ; A
		.byte	OP_TAY,OP_LDA,OP_TAX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_BBS
		.byte	OP_BCS,OP_LDA,OP_LDA,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_SMB ; B
		.byte	OP_CLV,OP_LDA,OP_TSX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_BBS
		.byte	OP_CPY,OP_CMP,OP_ERR,OP_ERR,OP_CPY,OP_CMP,OP_DEC,OP_SMB ; C
		.byte	OP_INY,OP_CMP,OP_DEX,OP_WAI,OP_CPY,OP_CMP,OP_DEC,OP_BBS
		.byte	OP_BNE,OP_CMP,OP_CMP,OP_ERR,OP_ERR,OP_CMP,OP_DEC,OP_SMB ; D
		.byte	OP_CLD,OP_CMP,OP_PHX,OP_STP,OP_ERR,OP_CMP,OP_DEC,OP_BBS
		.byte	OP_CPX,OP_SBC,OP_ERR,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_SMB ; E
		.byte	OP_INX,OP_SBC,OP_NOP,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_BBS
		.byte	OP_BEQ,OP_SBC,OP_SBC,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_SMB ; F
		.byte	OP_SED,OP_SBC,OP_PLX,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_BBS

MODES:
		.byte	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 0
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPG,MO_ZPX,MO_ZPX,MO_ZPG ; 1
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABS,MO_ABX,MO_ABX,MO_BRL
		.byte	MO_ABS,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 2
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPX,MO_ZPG ; 3
		.byte	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABX,MO_ABX,MO_ABX,MO_BRL
		.byte	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG ; 4
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPG ; 5
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_BRL
		.byte	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 6
		.byte	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_IAB,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPX,MO_ZPG ; 7
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IAX,MO_ABX,MO_ABX,MO_BRL
		.byte	MO_REL,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 8
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_ZPG ; 9
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABS,MO_ABX,MO_ABX,MO_BRL
		.byte	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; A
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_ZPG ; B
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_ABY,MO_BRL
		.byte	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; C
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPG ; D
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_BRL
		.byte	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; E
		.byte	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.byte	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPG ; F
		.byte	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_BRL
		.endif
		
;==============================================================================
; I/O Area
;-------------------------------------------------------------------------------

		.org	$fe00
		.space	64		; Skip over virtual hardware
		
;===============================================================================
; API Vectors
;-------------------------------------------------------------------------------

		jmp	UartTx
		
		jmp	UartRx
		
NewLine:	lda	#CR
		jsr	UartTx
		lda	#LF
		jmp	UartTx
		
;===============================================================================
; IRQ Handler
;-------------------------------------------------------------------------------

; Handle interrupts, currently just UART transmit buffer empty and receive
; buffer full.

IRQ:
		pha			; Save users registers
		.if	__65C02__
		phx
		phy
		.else
		txa
		pha
		tya
		pha
		cld
		.endif

		tsx			; Check for BRK
		lda	STACK+4,X
		and	#$10
		if ne
		 jmp	BRK		; Enter monitor with registers on stack
		endif
		
;-------------------------------------------------------------------------------

		lda	ACIA_STAT	; ACIA is the source?
		if mi
		 pha
		 and	#$10		; TX Buffer empty?
		 if ne
		  ldy	TX_HEAD		; Any data to send?
		  cpy	TX_TAIL
		  if ne
		   lda	TX_BUFFER,Y	; Yes, extract and send it
		   sta	ACIA_DATA
		   jsr	BumpIdx
		   sty	TX_HEAD
		  else
		   lda	#$01		; No, disable TX interrupt
		   sta	ACIA_CMND
		  endif
		 endif

		 pla
		 and	#$08		; RX Buffer full?
		 if ne
		  lda	ACIA_DATA	; Yes, fetch the character
		  ldy	RX_TAIL		; .. and save it
		  sta	RX_BUFFER,Y
		  jsr	BumpIdx
		  cpy	RX_HEAD		; Is buffer completely full?
		  if	ne
		   sty	RX_TAIL		; No, update tail offset
		  endif
		 endif
		endif

;-------------------------------------------------------------------------------

		.if	__65C02__
		ply			; Restore user registers
		plx
		.else
		pla			; Restore user registers
		tay
		pla
		tax
		.endif
		pla
NMI:		rti			; Done

;===============================================================================
; UART I/O
;-------------------------------------------------------------------------------

; Inserts the byte in A into the transmit buffer. If the buffer is full then
; wait until some space is available. Registers are preserved.

UartTx:
		pha
		sty	IO_TEMP
		
		ldy	TX_TAIL		; Save the data byte at the tail
		sta	TX_BUFFER,Y
		jsr	BumpIdx		; Work out the next offset
		repeat			; And wait until save to store
		 cpy	TX_HEAD
		until	NE
		sty	TX_TAIL
		lda	#$05		; Ensure TX interrupt enabled
		sta	ACIA_CMND
		
		ldy	IO_TEMP
		pla
		rts			; Done

; Extracts the next character from the head of the RX buffer. If the buffer is
; empty then wait for some data to be placed in it by the interrupt handler.

UartRx:
		sty	IO_TEMP
		ldy	RX_HEAD		; Wait until there is some data
		repeat
		 cpy	RX_TAIL
		until	NE
		lda	RX_BUFFER,y	; Then extract the head byte
		jsr	BumpIdx		; Update the offset
		sty	RX_HEAD
		ldy	IO_TEMP
		rts			; Done
		
; Increments an index value wrapping it back to zero if it exceeds the buffer
; size.

BumpIdx:
		iny			; Bump index
		cpy	#BUF_SIZE	; Reached end of buffer?
		if cs
		 ldy	#0		; Yes, wrap around
		endif
		rts			; Done
		
;===============================================================================
; Vector Locations
;-------------------------------------------------------------------------------

		.org	$FFFA

		.word	NMI		; NMI
		.word	RESET		; RESET
		.word	IRQ		; IRQ/BRK

		.END