;===============================================================================
;  ____  ____         __  ____   ___ ____  
; / ___|| __ )       / /_| ___| / _ \___ \ 
; \___ \|  _ \ _____| '_ \___ \| | | |__) |
;  ___) | |_) |_____| (_) |__) | |_| / __/ 
; |____/|____/       \___/____/ \___/_____|
;                                          
; A Minimal 65(C)02/65C802 Single Board Computer
;-------------------------------------------------------------------------------
; Copyright (C)2014-2018 Andrew John Jacobs
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
; | 0000 | Page Zero             | Default Page Zero     |
; | 00F0 | Monitor (Overwritable)| Monitor (Overwritable)|
; | 00FE | Interrupt Temp        |                       |
; +------+-----------------------+-----------------------+
; | 0100 | Stack                 | Default Stack         |
; +------+-----------------------+-----------------------+
; | 0200 | Vectors               | Vectors               |
; | 0208 | I/O RAM               | I/O RAM               |
; | 0280 | Monitor (Overwritable)| Monitor (Overwritable)|
; +------+-----------------------+-----------------------+
; | 0300 | RAM                   | RAM                   |
; |      |                       |                       |
; | BFFF |                       |                       |
; +------+-----------------------+-----------------------+
; | C000 | 'ROM' Image           | 'ROM' Image           |
; |      | (Overwritable)        | (Overwritable)        |
; | EFFF |                       |                       |
; +------+-----------------------+-----------------------+
; | F000 | Boot 'ROM' Image      | Boot 'ROM' Image      |
; |      | (Overwritable)        | (Overwritable)        |
; | FDFF |                       |                       |
; +------+-----------------------+-----------------------+
; | FE00 | ACIA                  | ACIA                  |
; | FE10 | 65SPI                 | 65SPI                 |
; | FE20 | DS1318                | DS1318                |
; | FE30 | Flash Access          | Flash Access          |
; +------+-----------------------+-----------------------+
; | FE40 | I/O API & Drivers     | I/O API & Drivers     |
; | FFE8 |                       | 65C802 Vectors        |
; | FFFA | 65(C)02 Vectors       |                       |
; +------+-----------------------+-----------------------+
;
; If the monitor is not being used then its zero page ($F0-$FD) and buffer area
; ($0280-$02FF) can be overwriten by the user program.
;
; The Host PIC18F is too fast for the UART to be set to 50 or 75 baud. The other
; speeds work correctly.
;-------------------------------------------------------------------------------

;===============================================================================
; Hardware Definitions
;-------------------------------------------------------------------------------

STACK		.equ	$0100

; Emulated 6551 ACIA

ACIA_DATA	.equ	$fe00		; R/W
ACIA_STAT	.equ	$fe01		; R/W
ACIA_CMND	.equ	$fe02		; R/W
ACIA_CTRL	.equ	$fe03		; R/W

; Emulated 65SPI

SPI_DATA	.equ	$fe10		; R/W
SPI_STAT	.equ	$fe11		; R/O
SPI_CTRL	.equ	$fe11		; W/O
SPI_DVSR	.equ	$fe12		; R/W
SPI_SLCT	.equ	$fe13		; R/W

; Emulated DS1318 RTCC

RTC_SUB0	.equ	$fe20		; R/W
RTC_SUB1	.equ	$fe21		; R/W
RTC_SEC0	.equ	$fe22		; R/W
RTC_SEC1	.equ	$fe23		; R/W
RTC_SEC2	.equ	$fe24		; R/W
RTC_SEC3	.equ	$fe25		; R/W
RTC_ALM0	.equ	$fe26		; R/W
RTC_ALM1	.equ	$fe27		; R/W
RTC_ALM2	.equ	$fe27		; R/W
RTC_ALM3	.equ	$fe28		; R/W
RTC_CTLA	.equ	$fe2a		; R/W
RTC_CTLB	.equ	$fe2b		; R/W
RTC_STAT	.equ	$fe2c		; R/W

; Flash ROM Interface

ROM_OFFL	.equ	$fe30		; R/W
ROM_OFFH	.equ	$fe31		; R/W
ROM_DATA	.equ	$fe32		; R/W
ROM_LOCK	.equ	$fe33		; R/W

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

;===============================================================================
; Vectors
;-------------------------------------------------------------------------------

IRQV		.equ	$0200		; 65(C)02 Vectors
NMIV		.equ	$0202
IRQNV		.equ	$0204		; 65C802 Vectors (Native Mode)
NMINV		.equ	$0206
		
;===============================================================================
; I/O API
;-------------------------------------------------------------------------------

UARTTX		.equ	$fe40
UARTRX		.equ	$fe43
UARTTXCOUNT	.equ	$fe46
UARTRXCOUNT	.equ	$fe49

SPISENDIDLE	.equ	$fe4c
SPISENDDATA	.equ	$fe4f
