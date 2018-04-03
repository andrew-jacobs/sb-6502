;===============================================================================
;       _____          _   _           
;   ___|  ___|__  _ __| |_| |__    _   
;  / _ \ |_ / _ \| '__| __| '_ \ _| |_ 
; |  __/  _| (_) | |  | |_| | | |_   _|
;  \___|_|__\___/|_|___\__|_| |_|_|_|  
;  / /_| ___| / ___( _ ) / _ \___ \    
; | '_ \___ \| |   / _ \| | | |__) |   
; | (_) |__) | |__| (_) | |_| / __/    
;  \___/____/ \____\___/ \___/_____|   
;
; An Extended 65C802 eForth for the SB-6502       
;-------------------------------------------------------------------------------                               
; Copyright (C),2018 Andrew John Jacobs
; All rights reserved.
;
; This work is made available under the terms of the Creative Commons
; Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
; following URL to see the details.
;
; http://creativecommons.org/licenses/by-nc-sa/4.0/
;===============================================================================
; Notes:
;
; This is an implementation of C.H Ting's eForth model for the WDC 65C802
; microprocessor. The eForth model is based on small number of machine coded
; primitives with the majority of the standard Forth words written in Forth
; itself. This combination allows an implementation to be created quickly.
;
; To make the standard model more efficient on the 65C802 I have made a few
; changes, in particular the pointer values used in compiled words are one less
; than normal as the RTS opcode used to execute them adds one to the return
; address when pulled.
;
; Some additional words have been added to access hardware and additional words
; will be translated into native code to improve performance in later releases.
;-------------------------------------------------------------------------------

		.65816
		.include "../sb-6502.inc"
		
;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

DSTACK		.equ	$0000		; Data stack base
RSTACK		.equ	$0100		; Return stack base

		.page0
		.org	$0000
		
IP		.space	2		; Forth instruction pointer
TMP		.space	2

;-------------------------------------------------------------------------------
; USER Variables

		.bss
		.org	$0200

;-------------------------------------------------------------------------------
; I/O
		
DATA_SIZE	.equ	62

TXHEAD		.space	1		; Transmit buffer indices
TXTAIL		.space	1
RXHEAD		.space	1		; Receive buffer indices
RXTAIL		.space	1

TXDATA		.space	DATA_SIZE	; Buffers
RXDATA		.space	DATA_SIZE
		.code
		
;===============================================================================
; eForth Definitions
;-------------------------------------------------------------------------------

		.code
		.org	$e000
		
		.include "common.inc"
		
;===============================================================================
; Native Words
;-------------------------------------------------------------------------------

		HEADER	4,"exit",NORMAL
EXIT:
		pla			; Pull last IP
		sta	<IP	

NEXT:
		lda	(IP)		; Fetch next word address
		inc	<IP		; Bump instruction pointer
		inc	<IP
		pha			; Execute target word
		rts
	
doLIST:
		ply
		iny
		lda	<IP
		pha
		sty	<IP
		bra 	NEXT
		

doLITERAL:
		dex
		dex
		lda	(IP)
		inc	<IP
		inc	<IP
		sta	<DSTACK+1,x
		bra	NEXT	


		HEADER	7,"execute",NORMAL
EXECUTE:
		lda	<DSTACK+1,x
		inx
		inx
		dec	a
		pha
		rts
		
;-------------------------------------------------------------------------------

QBRANCH:
		lda	<DSTACK+1,x
		inx
		inx
		cmp	#0
		if ne
		 inc	<IP
		 inc	<IP
		 bra	NEXT
		endif

BRANCH:		
		lda	(IP)
		sta	<IP
		bra	NEXT
		
;-------------------------------------------------------------------------------

		HEADER	1,"!",NORMAL
STORE:
		lda	<DSTACK+3,x
		sta	(DSTACK+1,x)
		inx
		inx
		inx
		inx
		jmp	NEXT

		HEADER	1,"@",NORMAL
FETCH:
		lda	(DSTACK+1,x)
		sta	<DSTACK+1,x
		jmp	NEXT

		HEADER	2,"c!",NORMAL
C_STORE:
		SHORT_A
		lda	<DSTACK+3,x
		sta	(DSTACK+1,x)
		LONG_A
		inx
		inx
		inx
		inx
		jmp	NEXT
		
		HEADER	2,"c@",NORMAL
C_FETCH:
		SHORT_A
		lda	(DSTACK+1,x)
		LONG_A
		and	#$ff
		sta	<DSTACK+1,x
		jmp	NEXT

;-------------------------------------------------------------------------------

		HEADER	3,"rp!",NORMAL
RP_STORE:
		txa
		tay
		lda	<DSTACK+1,x
		tax
		txs
		tya
		tax
		inx
		inx
		jmp	NEXT

		HEADER	3,"rp@",NORMAL
RP_FETCH:
		dex
		dex
		txa
		tay
		tsx
		stx	<DSTACK+1,y
		ldx	#$01
		stx	<DSTACK+2,y
		tya
		tax
		jmp	NEXT
		
		HEADER	2,"r>",NORMAL
R_FROM:
		dex
		dex
		pla
		sta	<DSTACK+1,x
		pla
		sta	<DSTACK+2,x
		jmp	NEXT
		
		HEADER	2,"r@",NORMAL
R_FETCH:
		dex
		dex
		pla
		tay
		sta	<DSTACK+1,x
		pla
		sta	<DSTACK+2,x
		pha
		tya
		pha
		jmp	NEXT
		
		HEADER	2,">r",NORMAL
TO_R:
		lda	<DSTACK+2,x
		pha
		lda	<DSTACK+1,x
		pha
		inx
		inx
		jmp	NEXT
	
;-------------------------------------------------------------------------------

		HEADER	4,"drop",NORMAL
DROP:
		inx
		inx
		jmp	NEXT
		
		HEADER	3,"dup",NORMAL
DUP:
		dex
		dex
		lda	<DSTACK+3,x
		sta	<DSTACK+1,x
		jmp	NEXT
		
		HEADER	4,"swap",NORMAL
SWAP
		lda	<DSTACK+1,x
		ldy	<DSTACK+3,x
		sty	<DSTACK+1,x
		sta	<DSTACK+3,x
		jmp	NEXT
		
		HEADER	4,"over",NORMAL
OVER:
		dex
		dex
		lda	<DSTACK+5,x
		sta	<DSTACK+1,x
		jmp	NEXT
		
		HEADER	3,"sp!",NORMAL
SP_STORE:
		lda	<DSTACK+1,x
		tax
		jmp	NEXT
		
		HEADER	3,"sp@",NORMAL
SP_FETCH:
		txa
		dex
		dex
		sta	<DSTACK+1,x
		lda	#0
		sta	<DSTACK+2,x
		jmp	NEXT
		
;-------------------------------------------------------------------------------

		HEADER	2,"0<",NORMAL
ZERO_LESS:
		lda	<2,x
		if mi
		 dey
		endif
		sty	<DSTACK+1,x
		sty	<DSTACK+2,x
		jmp	NEXT

		HEADER	3,"and",NORMAL
AND:
		lda	<DSTACK+1,x
		and	<DSTACK+3,x
		sta	<DSTACK+3,x
		lda	<DSTACK+2,x
		and	<DSTACK+4,x
		sta	<DSTACK+4,x
		inx
		inx
		jmp	NEXT

		HEADER	2,"or",NORMAL
OR:
		lda	<DSTACK+1,x
		and	<DSTACK+3,x
		sta	<DSTACK+3,x
		lda	<DSTACK+2,x
		and	<DSTACK+4,x
		sta	<DSTACK+4,x
		inx
		inx
		jmp	NEXT

		HEADER	3,"xor",NORMAL
XOR:
		lda	<DSTACK+1,x
		eor	<DSTACK+3,x
		sta	<DSTACK+3,x
		lda	<DSTACK+2,x
		eor	<DSTACK+4,x
		sta	<DSTACK+4,x
		inx
		inx
		jmp	NEXT

;-------------------------------------------------------------------------------

		HEADER	3,"um+",NORMAL
UM_PLUS:
		clc
		lda	<DSTACK+1,x
		adc	<DSTACK+3,x
		sta	<DSTACK+3,x
		lda	<DSTACK+2,x
		adc	<DSTACK+4,x
		sta	<DSTACK+4,x
		lda	#0
		sta	<DSTACK+1,x
		sta	<DSTACK+2,x
		if cs
		 inc	<DSTACK+1,x
		endif
		jmp	NEXT
		
;===============================================================================
; I/O Area
;-------------------------------------------------------------------------------

		.org	$ff00
		.space	64
		
;===============================================================================
; Power On Reset
;-------------------------------------------------------------------------------
		.org	$ff40

RESET:
		sei			; Ensure CPU state is reset
		cld
		ldx	#$ff		; Initialise data and return stacks
		txs
		
                lda     #%00011111	; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTRL
                lda     #%11001001	; No parity, no interrupt
                sta     ACIA_CMND
                lda     ACIA_DATA	; Clear receive buffer

		lda	#0		; Empty the UART buffers
		sta	TXHEAD
		sta	TXTAIL
		sta	RXHEAD
		sta	RXTAIL		
		cli			; Start interrupt handling
		
		lda	#<0		; Set IP for initial word
		sta	<IP+0
		lda	#>0
		sta	<IP+1
		jmp	NEXT		; and start Forth
		
;===============================================================================
; Interrupt Handler
;-------------------------------------------------------------------------------

IRQ:
		pha			; Save user task registers
		txa
		pha
		
		lda	#$08		; Has the UART received data?
		bit	ACIA_STAT
		if ne
		 ldx	RXTAIL
		 lda	ACIA_DATA
		 sta	RXDATA,x
		 jsr	BumpIndex
		 cpx	RXHEAD
		 if ne
		  stx	RXTAIL
		 endif
		endif
		
		lda	#$10		; Is the UART ready to transmit?
		bit	ACIA_STAT
		if ne
		 ldx	TXHEAD		; Any data left to send?
		 cpx	TXTAIL
		 if ne
		  lda	TXDATA,x	; Yes, send the next character
		  sta	ACIA_DATA
		  jsr	BumpIndex
		  stx	TXHEAD
		 else
		  lda	#$01		; No, disable the interrupt
		  sta	ACIA_CMND
		 endif
		endif

		pla			; Restore user task registers
		tax
		pla
NMI:
		rti			; And continue

;-------------------------------------------------------------------------------
		
BumpIndex:
		inx			; Increment the index
		cpx	#DATA_SIZE	; .. and check for wrap around
		if eq
		 ldx	#0		; Reset if has occurred.
		endif
		rts			; Done.

;===============================================================================
; Vectors
;-------------------------------------------------------------------------------

		.org	$fffa
		
		.word	NMI
		.word	RESET
		.word	IRQ
		
		.end