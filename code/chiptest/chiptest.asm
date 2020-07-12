;===============================================================================
;   ____ _     _     _____         _   
;  / ___| |__ (_)_ _|_   _|__  ___| |_ 
; | |   | '_ \| | '_ \| |/ _ \/ __| __|
; | |___| | | | | |_) | |  __/\__ \ |_ 
;  \____|_| |_|_| .__/|_|\___||___/\__|
;               |_|                    
;
; A Simple 65xx Chip Detector
;-------------------------------------------------------------------------------
; Copyright (C)2020 Andrew John Jacobs.
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
; This program runs a short code sequence that executes slightly differently on
; 6502, 65C02, 65SC02 and 65C816/802 devices. It displays the result in a human
; readable form.
;
; As the SB-6502 board can't take a 65C816 this code reports it as a 65C802.
;
;-------------------------------------------------------------------------------

		.65C02
		
		.include "../sb-6502.inc"
		.include "../ascii.inc"

		.code
		.org	$0300
		
		lda	#0		; Chromatix's cunning chip detector
		sta	$84
		sta	$85
		lda	#$1d
		sta	$83
		lda	#$6b
		sta	$1d
		lda	#'N'
		rmb4 	$83
		eor	$83
		
		.6502
		
		pha			; Save the result
		ldx	#0		; Print leading text
		jsr	Print
		ldx	#C802-STRINGS	; Assume its an 802
		pla
		cmp	#'8'
		if ne
		 cmp	#'N'		; Is it an NMOS?
		 if ne
		  cmp 	#'S'		; No, handle SC/C
		  if eq
		   jsr	UARTTX
		  endif
		  lda	#'C'
		  jsr	UARTTX
		 endif
		 ldx	#N02-STRINGS	; Prepare 02
		endif
		jsr	Print		; Display the tail

		brk			; Return to monitor

; Print the string starting at offset X until a NUL byte is reached.

Print:
		repeat
		 lda	STRINGS,x	; Fetch next character
		 break	eq		; Done?
		 jsr	UARTTX		; No, display,
		 inx			; .. bump and repeat
		forever
		rts

; All the strings we will ever need

STRINGS		.byte	CR,LF,"Found a 65",0
N02		.byte	"02"
		.byte	CR,LF,0
C802		.byte	"C802"
		.byte	CR,LF,0

		.end