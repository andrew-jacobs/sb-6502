;
; $0000	+-------------------+ ----- $00	+-------------------+
;       | Zero Page         |		|                   |
; $0100 +-------------------+		| Application	    |
;       | Stack             | 		| Variables	    |
; $0200 +-------------------+ \		|		    |
;	|		    |  \	|		    |
;	|		    |   \   $e0 |-------------------|
;	| Applications	    |	 \	| Disk Variables    |
;	|		    |	  \ $f0 |-------------------|
;	|		    |	   \	| O/S Variables	    |
; $e000 +-------------------+	    \	+-------------------+
;	|		    |
;	| Disk Buffers	    |
;	|		    |
; $ef00 +-------------------+
;	| UART Buffers      |
; $f000 +-------------------+
;	| O/S Jump Table    |
;	|-------------------|
;	|		    |
;	| O/S Code	    |
;	|		    |
; $fe00 +-------------------+
;       | I/O Devices       |
; $ff00 +-------------------+
;	| O/S / Vectors     |
;	+-------------------+
;
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

CMD_SIZE	.equ	128

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.page0
		.org	$00f0

; Communications buffer offsets

RX_HEAD		.space	1		; UART receive buffer offsets
RX_TAIL		.space	1
TX_HEAD		.space	1		; UART transmit buffer offsets
TX_TAIL		.space	1

TM_TK		.space	1
TM_SC		.space	1
TM_MN		.space	1
TM_HR		.space	1
TM_DY		.space	1
TM_MO		.space	1
TM_YR		.space	1

;-------------------------------------------------------------------------------

		.org	$00ff

IO_TEMP		.space	1		;

;-------------------------------------------------------------------------------


;===============================================================================
; UART Buffers
;-------------------------------------------------------------------------------

		.bss
		.org	$ef00

BUFFER		.space	CMD_SIZE

RX_BUFF		.space	RX_SIZE		; UART receive buffer
TX_BUFF		.space	TX_SIZE		; UART transmit buffer

;===============================================================================
;-------------------------------------------------------------------------------

		.code
		.org	$F000



;===============================================================================
; O/S Entry Points
;-------------------------------------------------------------------------------

		jmp	0
		jmp	0
		jmp	0
		jmp	0

;===============================================================================
; Reset Handler
;-------------------------------------------------------------------------------

RESET:
		sei			; Ensure interrupts disabled
		cld			; Ensure binary mode
		ldx	#$FF		; Reset the stack
		txs

		.if	__65C02__
		stz	RX_HEAD		; Clear buffer offsets
		stz	RX_TAIL
		stz	TX_HEAD
		stz	TX_TAIL
		.else
		inx			; Clear buffer offsets
		stx	RX_HEAD
		stx	RX_TAIL
		stx	TX_HEAD
		stx	TX_TAIL
		.endif

                lda     #%00011111	; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTRL
                lda     #%11001001	; No parity, no interrupt
                sta     ACIA_CMND
                lda     ACIA_DATA	; Clear receive buffer

		lda	#%00000001	; Configure SPI
		sta	SPI_CTRL
		jsr	SpiCSHi		; And /CS Hi

		cli			; Allow interrupts

		jsr	UartLn
		ldx	#BOOT_STR
		jsr	UartStr

;===============================================================================
; SD Card Initialisation
;-------------------------------------------------------------------------------

		jsr	SpiSlow		; Set SPI to slow speed
		ldx	#20
		repeat
		 jsr	SpiIdle		; Send clock pulses
		 dex
		until	eq
		
;-------------------------------------------------------------------------------

.SendCmd0
		lda	#3		; Load retry counter
		repeat
		 pha
		 ldx	#SD_CMD0	; Send CMD0
		 jsr	SpiCommand
		 ldy	#16		; Set byte count
		 repeat
		  jsr	SpiIdle		; Send idle data
	pha
	jsr	Hex2
	pla
		  cmp	#$01		; Received a reply?
		  beq	.DoneCmd0	; Send next command
		  dey
		 until 	eq		; Out of bytes?
		 jsr	SpiCSHi		; Yes, set CS idle
		 pla			; Any retries left?
		.if	__65C02__
		 dec	a
		.else
		 sec
		 sbc 	#1
		.endif
		until	eq		
		jmp	NoCard		; No

.DoneCmd0:
		pla
		jsr	SpiCSHi
		jsr	SpiIdle

;-------------------------------------------------------------------------------

.SendCmd8:
	lda	#'*'
	jsr	UartTx
		lda	#3
		repeat
		 pha
		 ldx	#SD_CMD8	; Send CMD8
		 jsr	SpiCommand
		 ldy	#255
		 repeat
		  jsr	SpiIdle		; Send idle data
	pha
	jsr	Hex2
	pla
		  cmp	#$01		; Received a reply?
		  beq	.DoneCmd8	; Yes
		  dey
		 until 	eq		; Out of bytes?
		 jsr	SpiCSHi		; Yes, set CS idle
		 pla			; Any retries left?
		.if	__65C02__
		 dec	a
		.else
		 sec
		 sbc	#1
		.endif
		until	eq
		jmp	NoCard		; No

.DoneCmd8:
		pla			; Drop retry count
		tsx
		jsr	SpiIdle		; Read the result
		pha
	pha
	jsr	Hex2
	pla
		jsr	SpiIdle
		pha
	pha
	jsr	Hex2
	pla
		jsr	SpiIdle
		pha
	pha
	jsr	Hex2
	pla
		jsr	SpiIdle
		pha
	pha
	jsr	Hex2
	pla
		jsr	SpiCSHi
		jsr	SpiIdle
		pla
		cmp	#$aa
		if	eq
		 pla
		 cmp	#$01
		 if 	eq
		  pla
		  if 	eq
		   pla
		   beq	.SendACmd41A
		  endif
		 endif
		endif
		txs

;-------------------------------------------------------------------------------

.SendACmd41A
	lda	#'+'
	jsr	UartTx
		lda	#3		; Load retry counter
		repeat
		 pha
		 ldx	#SD_CMD55	; Send CMD55
		 jsr	SpiCommand
		 jsr	SpiIdle
		 jsr	SpiIdle
		 jsr	SpiIdle
		 jsr	SpiIdle
		 ldx	#SD_ACMD41A	; Send CMD41
		 jsr	SpiCommand
		 ldy	#16
		 repeat
		  jsr	SpiIdle
	pha
	jsr	Hex2
	pla
		  cmp	#$00
		  beq	.DoneACmd41A
		  cmp	#$01
		  break	eq
		  dey
		 until	eq
		 jsr	SpiCSHi		; Yes, set CS idle
		 pla			; Any retries left?
		.if	__65C02__
		 dec	a
		.else
		 sec
		 sbc	#1
		.endif
		until	eq
		jmp	.SendACmd41B	; No
		
		   
.DoneACmd41A:
		pla
		jsr	SpiCSHi
		jsr	SpiIdle
		jmp	.SendCmd58

;-------------------------------------------------------------------------------

.SendACmd41B:
	lda	#'-'
	jsr	UartTx
		lda	#3		; Load retry counter
		repeat
		 pha
		 ldx	#SD_CMD55	; Send CMD55
		 jsr	SpiCommand
		 jsr	SpiIdle
		 jsr	SpiIdle
		 jsr	SpiIdle
		 jsr	SpiIdle
		 ldx	#SD_ACMD41B	; Send CMD41
		 jsr	SpiCommand
		 ldy	#16
		 repeat
		  jsr	SpiIdle
	pha
	jsr	Hex2
	pla
		  cmp	#$00
		  beq	.DoneACmd41B
		  cmp	#$01
		  break	eq
		  dey
		 until	eq
		 jsr	SpiCSHi		; Yes, set CS idle
		 pla			; Any retries left?
		.if	__65C02__
		 dec	a
		.else
		 sec
		 sbc	#1
		.endif
		until	eq
		jmp	.SendCmd1	; No
		
.DoneACmd41B:
		pla
		jsr	SpiCSHi
		jsr	SpiIdle

;-------------------------------------------------------------------------------

.SendCmd1:
	lda	#'='
	jsr	UartTx
		lda	#3		; Load retry counter
		repeat
		 pha
		 ldx	#SD_CMD1	; Send CMD1
		 jsr	SpiCommand
		 ldy	#16
		 repeat
		  jsr	SpiIdle		; Send idle data
	pha
	jsr	Hex2
	pla
		  cmp	#$00		; Received a reply?
		  beq	.DoneCmd1	; Yes
		  cmp	#$01
		  break	eq
		  dey
		 until 	eq
		 jsr SpiCSHi
		 pla
		.if	__65C02__
		 dec	a
		.else
		 sec
		 sbc	#1
		.endif
		until	eq
		jmp	NoCard

.DoneCmd1:
		pla
		jsr	SpiCSHi
		jsr	SpiIdle
		jmp	.SendCmd16

;-------------------------------------------------------------------------------

.SendCmd58:
	lda	#'^'
	jsr	UartTx
		ldx	#SD_CMD58	; Send CMD58
		jsr	SpiCommand
		ldy	#16
		repeat
		 jsr	SpiIdle		; Send idle data
	pha
	jsr	Hex2
	pla
		 cmp	#$00		; Received a reply?
		 beq	.DoneCmd58	; Yes
		 dey
		until 	eq
		jsr	SpiCSHi
		jmp	NoCard
		
.DoneCmd58:
		jsr	SpiIdle
		tax			; Save CCS bit
	pha
	jsr	Hex2
	pla
		jsr	SpiIdle
	pha
	jsr	Hex2
	pla
		jsr	SpiIdle
	pha
	jsr	Hex2
	pla
		jsr	SpiIdle
	pha
	jsr	Hex2
	pla
		jsr	SpiCSHi
		jsr	SpiIdle
		
		txa			; Test CCS bit in OCR
		and	#$40
		bne	.DoneCmd16

;-------------------------------------------------------------------------------

.SendCmd16:
	lda	#'#'
	jsr	UartTx
		ldx	#SD_CMD16	; Send CMD16
		jsr	SpiCommand
		ldy	#0		; Load retry counter
		repeat
		 jsr	SpiIdle		; Send idle data
	pha
	jsr	Hex2
	pla
		 cmp	#$00		; Received a reply?
		 beq	.DoneCmd16
		 dey
		until 	eq
		jsr	SpiCSHi
		jmp	NoCard

.DoneCmd16:
		jsr	SpiCSHi		; Yes
		jsr	SpiIdle

;-------------------------------------------------------------------------------

		jmp	$

NoCard:
		ldx	#NO_SDCARD_STR
		jsr	UartStr
		jmp	$

		
SpiCommand:
		jsr	SpiCSLo
		ldy	#6
		repeat
		 lda	SD_CMDS,X
		 inx
		 jsr	SpiSend
		 dey
		until	eq
		rts

		
SD_CMDS:
SD_CMD0		.equ	.-SD_CMDS
		.byte	$40| 0,$00,$00,$00,$00,$95
SD_CMD1		.equ	.-SD_CMDS
		.byte	$40| 1,$00,$00,$00,$00,$ff
SD_CMD8		.equ	.-SD_CMDS
		.byte	$40| 8,$00,$00,$01,$aa,$87
SD_CMD16	.equ	.-SD_CMDS
		.byte	$40|16,$00,$00,$02,$00,$ff
SD_CMD55	.equ	.-SD_CMDS
		.byte	$40|55,$00,$00,$00,$00,$ff
SD_CMD58	.equ	.-SD_CMDS
		.byte	$40|58,$00,$00,$00,$00,$ff
SD_ACMD41A	.equ	.-SD_CMDS
		.byte	$40|41,$40,$00,$00,$00,$ff
SD_ACMD41B	.equ	.-SD_CMDS
		.byte	$40|41,$00,$00,$00,$00,$ff



;===============================================================================
;-------------------------------------------------------------------------------




;===============================================================================
;-------------------------------------------------------------------------------

Hex2:		pha
		lsr	a
		lsr	a
		lsr	a
		lsr	a
		jsr	Hex
		pla

Hex:		and	#$0f
		sed
		clc
		adc	#$90
		adc	#$40
		cld
		jmp	UartTx


UartLn:
		ldx	#CRLF_STR
UartStr:
		repeat
		 lda	STRINGS,X
		 if 	eq
		  rts
		 endif
		 jsr	UartTx
		 inx
		forever

;===============================================================================
; String table
;-------------------------------------------------------------------------------

STRINGS:
BOOT_STR	.equ	$-STRINGS
		.if	__6502__
		.byte	CR,LF,"OS/6502 [17.02]"
		.endif
		.if	__65C02__
		.byte	CR,LF,"OS/65C02 [17.02]"
		.endif

CRLF_STR	.equ	$-STRINGS
		.byte	CR,LF,0

NO_SDCARD_STR	.equ	$-STRINGS
		.byte	"No SD card found",0

TIME_LIMIT:	.byte	100,60,60,24
MONTH_LIMIT:	.byte	31,28,31,30, 31,30,31,31, 30,31,30,31

;==============================================================================
; I/O Page
;-------------------------------------------------------------------------------

		.org	$fe00
		.space	256

;===============================================================================
; UART Interface
;-------------------------------------------------------------------------------

; Inserts the byte in A into the transmit buffer. If the buffer is full then
; wait until some space is available. Registers are preserved.

UartTx:
		pha
		sty	IO_TEMP

		ldy	TX_TAIL		; Save the data byte at the tail
		sta	TX_BUFF,Y
		jsr	BumpTx		; Work out the next offset
		repeat			; And wait until save to store
		 cpy	TX_HEAD
		until	ne
		sty	TX_TAIL
		lda	#$05		; Ensure TX interrupt enabled
		sta	ACIA_CMND

		ldy	IO_TEMP
		pla
		rts			; Done

;
;

UartRx:
		sty	IO_TEMP
		ldy	RX_HEAD		; Wait until there is some data
		repeat
		 cpy	RX_TAIL
		until	ne
		lda	RX_BUFF,Y	; Then extract the head byte
		jsr	BumpRx		; Update the offset
		sty	RX_HEAD
		ldy	IO_TEMP
		rts			; Done

;===============================================================================
; SPI Handler
;-------------------------------------------------------------------------------

; Set the SPI divisor for high or low speed transfer.

SpiFast:
		pha
		lda	#1		; Set SPI clock to 8MHz
		bne	SpiSetSpeed

SpiSlow:
		pha
		lda	#63		; Set SPI clock to 200KHz

SpiSetSpeed:
		sta	SPI_DVSR
		pla
		rts

; Set the chip select line to the make the SD card busy or idle.

SpiCSLo:
		pha
		lda	#0<<2		; Set /CS lo
		beq	SpiSelect

SpiCSHi:
		pha
		lda	#1<<2		; Set /CS hi

SpiSelect:
		sta	SPI_SLCT
		pla
		rts

SpiIdle:
		lda	#$ff

; Send a byte of data to the SPI slave and return the byte of data received in
; reply. Update the CRC while waiting.

SpiSend:
		sta	SPI_DATA	; Transmit the byte in A
		repeat
		 lda	SPI_STAT	; Wait until transfer is complete
		until	mi
		lda	SPI_DATA	; Read the incoming data
		rts

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

		ldx	#0
		repeat
		 inc	TM_TK,x		; Bump time component
		 lda	TM_TK,x
		 cmp	TIME_LIMIT,x	; Reached limit?
		 bne	.Done		; No.
		.if	__65C02__
		 stz	TM_TK,x
		.else
		 lda	#0
		 sta	TM_TK,x		; Yes, reset
		.endif
		 inx			; And move to next
		 cpx	#4
		until	eq

		ldx	TM_MO		; February?
		cpx	#2
		clc
		if	eq
		 lda	TM_YR		; Leap year?
		 and	#3
		 if	eq
		  sec			; Yes set carry
		 endif
		endif
		lda	MONTH_LIMIT-1,x	; get day count
		adc	#0

		inc	TM_DY
		cmp	TM_DY
		if 	cc
		 lda	#1
		 sta	TM_DY
		 inc	TM_MO
		 lda	TM_MO
		 cmp	#13
		 if	cs
		  lda	 #1
		  sta	TM_MO
		  inc	TM_YR
		 endif
		endif
.Done:

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