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
