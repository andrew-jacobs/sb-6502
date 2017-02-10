
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

IO_TEMP		.space	1

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

		lda	#0		; Configure SPI
		sta	SPI_CTRL
		jsr	SpiIdle		; And /CS Hi
		
		cli			; Allow interrupts
		
;-------------------------------------------------------------------------------

		jsr	UartLn
		ldx	#BOOT_STR
		jsr	UartStr
		
		jsr	SpiSlow		; Set SPI to 400KHz
		ldx	#10
		repeat
		 lda	#$13		; Send 80 clock pulses
		 jsr	SpiSend
		 dex
		until	eq


;-------------------------------------------------------------------------------

		jmp	$


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

SpiFast:
		lda	#1		; Set SPI clock to 8MHz
		.byte	$2c
SpiSlow:
		lda	#39		; Set SPI clock to 400KHz
		sta	SPI_DVSR
		rts

SpiBusy:
		lda	#0		; Set /CS lo
		.byte	$2c
SpiIdle:
		lda	#1<<2		; Set /CS hi
		sta	SPI_SLCT
		rts

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
		   lda	TX_BUFF,Y	; Yes, extract and send it
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
		  sta	RX_BUFF,Y
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
		 inc	TM_TK,X		; Bump time component
		 lda	TM_TK,X
		 cmp	TIME_LIMIT,X	; Reached limit?
		 bne	.Done		; No. 
		 lda	#0
		 sta	TM_TK,X		; Yes, reset
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
		lda	MONTH_LIMIT-1,X	; get day count
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