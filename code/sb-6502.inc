;===============================================================================
;  ____  ____         __  ____   ___ ____  
; / ___|| __ )       / /_| ___| / _ \___ \ 
; \___ \|  _ \ _____| '_ \___ \| | | |__) |
;  ___) | |_) |_____| (_) |__) | |_| / __/ 
; |____/|____/       \___/____/ \___/_____|
;                                          
; A Minimal 65(C)02/65C802 Single Board Computer
;-------------------------------------------------------------------------------
; Copyright (C)2014-2017 Andrew John Jacobs
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
;          65(C)02                 65C802
; +------+-----------------------+-----------------------+
; | 0000 | Page Zero             | RAM                   |
; +------+-----------------------+                       |
; | 0100 | Stack                 |                       |
; +------+-----------------------+                       |
; | 0200 | RAM                   |                       |
; |      |                       |                       |
; |      |                       |                       |
; |      |                       |                       |
; | FEFF |                       |                       |
; +------+-----------------------+-----------------------+
; | F000 | Boot ROM Image        | Boot ROM Image        |
; +------+-----------------------+-----------------------+
; | FF00 | ACIA                  | ACIA                  |
; | FF10 | 65SPI                 | 65SPI                 |
; | FF20 | DS1318                | DS1318                |
; | FF30 | Empty                 | Empty                 |
; +------+-----------------------+-----------------------+
; | FF40 | Boot ROM Image        | Boot ROM Image        |
; | FFE8 |                       | 65C802 Vectors        |
; | FFFA | 65(C)02 Vectors       |                       |
; +------+-----------------------+-----------------------+
;
; The Host PIC18F is too fast for the UART to be set to 50 or 75 baud. The other
; speeds work correctly.
;-------------------------------------------------------------------------------

;===============================================================================
; Hardware Definitions
;-------------------------------------------------------------------------------

STACK		.equ	$0100

; Emulated 6551 ACIA

ACIA_DATA	.equ	$ff00		; R/W
ACIA_STAT	.equ	$ff01		; R/W
ACIA_CMND	.equ	$ff02		; R/W
ACIA_CTRL	.equ	$ff03		; R/W

; Emulated 65SPI

SPI_DATA	.equ	$ff10		; R/W
SPI_STAT	.equ	$ff11		; R/O
SPI_CTRL	.equ	$ff11		; W/O
SPI_DVSR	.equ	$ff12		; R/W
SPI_SLCT	.equ	$ff13		; R/W

; Emulated DS1318 RTCC

RTC_SUB0	.equ	$ff20		; R/W
RTC_SUB1	.equ	$ff21		; R/W
RTC_SEC0	.equ	$ff22		; R/W
RTC_SEC1	.equ	$ff23		; R/W
RTC_SEC2	.equ	$ff24		; R/W
RTC_SEC3	.equ	$ff25		; R/W
RTC_ALM0	.equ	$ff26		; R/W
RTC_ALM1	.equ	$ff27		; R/W
RTC_ALM2	.equ	$ff27		; R/W
RTC_ALM3	.equ	$ff28		; R/W
RTC_CTLA	.equ	$ff2a		; R/W
RTC_CTLB	.equ	$ff2b		; R/W
RTC_STAT	.equ	$ff2c		; R/W

;===============================================================================
; Macros
;-------------------------------------------------------------------------------

		.if	__65816__

SHORT_A		.macro
		.longa	off
		sep	#$20
		.endm
		
SHORT_I		.macro
		.longi	off
		sep	#$10
		.endm
		
SHORT_AI	.macro
		.longa	off
		.longi	off
		sep	#$30
		.endm

LONG_A		.macro
		.longa	on
		rep	#$20
		.endm
		
LONG_I		.macro
		.longi	on
		rep	#$10
		.endm
		
LONG_AI		.macro
		.longa	on
		.longi	on
		rep	#$30
		.endm
		
		.endif