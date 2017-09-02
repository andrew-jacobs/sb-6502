;===============================================================================
;  ____  ____         __  ____   ___ ____  
; / ___|| __ )       / /_| ___| / _ \___ \ 
; \___ \|  _ \ _____| '_ \___ \| | | |__) |
;  ___) | |_) |_____| (_) |__) | |_| / __/ 
; |____/|____/       \___/____/ \___/_____|
;                                         
; A Firmware for a Three Chip 6502/65C02 Single Board Computer
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
; This firmware programs a Microchip PIC18F46K22 microcontroller to act as
; the glue logic between a Rockwell/Synertec 6502 or WDC 65C02 and 64K of a
; 128K SRAM memory chip while partially emulating a 6551 ACIA, a 65SPI interface
; and a DS1813 RTC.
;
; At start up the firmware determines which microprocessor is installed by
; executing a JMP ($00FF) instruction and checking which memory locations the
; indirect address is read from (e.g. $00FF/$0000 = 6502, $00FF/$0100 = 65C02).
; a different boot ROM image is loaded accordingly.
;
;
; FOSC is too fast for the UART to be set to 50 or 75 baud. The other speeds
; should work.
;-------------------------------------------------------------------------------

#define M(X)    (.1 << (X))

;===============================================================================
; Fuse Settings
;-------------------------------------------------------------------------------

                ifdef __18F46K22
                include "P18F46K22.inc"

                config  FOSC=INTIO67,PLLCFG=ON,PRICLKEN=ON,FCMEN=OFF,IESO=OFF 
                config  BOREN=OFF,WDTPS=1024,STVREN=ON,LVP=OFF,XINST=OFF
                config  CCP2MX=PORTC1,PBADEN=OFF,HFOFST=ON,MCLRE=EXTMCLR
                ifdef   __DEBUG
                config  WDTEN=OFF,DEBUG=ON,PWRTEN=OFF
                else
                config  WDTEN=OFF,DEBUG=OFF,PWRTEN=ON
                endif

                config  CP0=OFF,CP1=OFF,CP2=OFF,CP3=OFF,CPB=OFF,CPD=OFF
                config  WRT0=OFF,WRT1=OFF,WRT2=OFF,WRT3=OFF,WRTC=OFF,WRTB=OFF,WRTD=OFF
                config  EBTR0=OFF,EBTR1=OFF,EBTR2=OFF,EBTR3=OFF,EBTRB=OFF

                endif
                
;===============================================================================
; Constants
;-------------------------------------------------------------------------------

BOOT_ADDR       equ     h'1000'                 ; Dummy reset address
ROM_BASE        equ     h'f000'                 ; Base address of ROM image

ADRH_MASK       equ     h'3f'                   ; 3f when debugging ff otherwise

; ASCII Control Characters
       
LF              equ     .10
CR              equ     .13

;===============================================================================
; Device Configuration
;-------------------------------------------------------------------------------

; Internal Oscillator
       
OSC             equ     .16000000
PLL             equ     .4

FOSC            equ     OSC * PLL

; 6502/65C02 Control Pins

ADRL_TRIS       equ     TRISA
ADRL_PORT       equ     PORTA
ADRL_LAT        equ     LATA

ADRH_TRIS       equ     TRISB
ADRH_PORT       equ     PORTB
ADRH_LAT        equ     LATB

DATA_TRIS       equ     TRISD
DATA_PORT       equ     PORTD
DATA_LAT        equ     LATD

NRAM_TRIS       equ     TRISC
NRAM_LAT        equ     LATC
NRAM_PIN        equ     .0

NIRQ_TRIS       equ     TRISC
NIRQ_LAT        equ     LATC
NIRQ_PIN        equ     .1

NRES_TRIS       equ     TRISE
NRES_LAT        equ     LATE
NRES_PIN        equ     .0

PHI0_TRIS       equ     TRISE
PHI0_LAT        equ     LATE
PHI0_PIN        equ     .1

RW_TRIS         equ     TRISE
RW_PORT         equ     PORTE
RW_PIN          equ     .2

; UART Signal Pins

TXD_TRIS        equ     TRISC
TXD_PIN         equ     .6
RXD_TRIS        equ     TRISC
RXD_PIN         equ     .7
         
#define UART_BRG(X)      (FOSC / (.16 * (X)) - .1)
        
; SPI Signal Pins
      
SEL_TRIS        equ     TRISC
SEL_PIN         equ     .2
SCK_TRIS        equ     TRISC
SCK_PIN         equ     .3
SDI_TRIS        equ     TRISC
SDI_PIN         equ     .4
SDO_TRIS        equ     TRISC
SDO_PIN         equ     .5
         
#define SPI_BRG(X)      (FOSC / (.4 * (X)) - .1)
         
; Timer 2
         
TMR2_HZ         equ     .4096
TMR2_PRE        equ     .4
TMR2_POST       equ     .8
TMR2_PR         equ     FOSC / (.4 * TMR2_HZ * TMR2_PRE * TMR2_POST) - .1
         
                if      TMR2_PR & h'ffffff00'
                error   "Timer2 period does not fit in 8-bits"
                endif
        
;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

                udata_acs

DEVICE          res     .1                      ; h'00' if 6502, h'ff' if 65C02

ADDRL           res     .1                      ; Address next ROM byte to force
ADDRH           res     .1                      ; .. load

SCRATCH         res     .1                      ; Scratch area

;-------------------------------------------------------------------------------

INT_FLAG        res     .1                      ; Software interrupt flags
INT_MASK        res     .1                      ; Interrupt mask
        
INT_HW_RXD      equ     RC1IF
INT_HW_TXD      equ     TX1IF
INT_HW_SPI      equ     SSP1IF
INT_SW_TMR      equ     TMR2IF

ACIA_CMD        res     .1                      ; ACIA Command Register
ACIA_CTL        res     .1                      ; ACIA Control Register
        
SPI_CTL         res     .1                      ; SPI Command Register
SPI_DIV         res     .1                      ; SPI Divisor Register
SPI_SEL         res     .1                      ; SPI Select Register
         
RTC_SUB0R       res     .1                      ; RTC Realtime registers
RTC_SUB1R       res     .1
RTC_SEC0R       res     .1      
RTC_SEC1R       res     .1
RTC_SEC2R       res     .1
RTC_SEC3R       res     .1
RTC_SUB0        res     .1                      ; RTC Buffered registers
RTC_SUB1        res     .1
RTC_SEC0        res     .1
RTC_SEC1        res     .1
RTC_SEC2        res     .1
RTC_SEC3        res     .1
RTC_ALM0        res     .1
RTC_ALM1        res     .1
RTC_ALM2        res     .1
RTC_ALM3        res     .1
RTC_CTLA        res     .1
RTC_CTLB        res     .1
RTC_STAT        res     .1

;===============================================================================
; Reset Vector
;-------------------------------------------------------------------------------
        
.ResetVector    code    h'0000'

                goto    PowerOnReset

;===============================================================================
; Interrupt Handler
;-------------------------------------------------------------------------------
         
.Interrupt      code    h'0008'
      
                bcf     PIR1,TMR2IF     ; Clear the interrupt flag

                movlw   h'10'           ; Bump the time value
                addwf   RTC_SUB0R,W
                movwf   RTC_SUB0R
                btfsc   RTC_CTLA,.7
                movwf   RTC_SUB0
                
                movlw   .0
                addwfc  RTC_SUB1R,F
                movwf   RTC_SUB1R
                btfsc   RTC_CTLA,.7
                movwf   RTC_SUB1
                
                movlw   .0
                addwfc  RTC_SEC0,F
                movwf   RTC_SEC0R
                btfsc   RTC_CTLA,.7
                movwf   RTC_SEC0
                
                movlw   .0
                addwfc  RTC_SEC1,F
                movwf   RTC_SEC1R
                btfsc   RTC_CTLA,.7
                movwf   RTC_SEC1
                
                movlw   .0
                addwfc  RTC_SEC2,F
                movwf   RTC_SEC2R
                btfsc   RTC_CTLA,.7
                movwf   RTC_SEC2
                
                movlw   .0
                addwfc  RTC_SEC3,F
                movwf   RTC_SEC3R
                btfsc   RTC_CTLA,.7
                movwf   RTC_SEC3
      
                retfie  FAST
         
;===============================================================================
; Power On Reset
;-------------------------------------------------------------------------------

PowerOnReset:
                banksel ANSELA
                clrf    ANSELA                  ; Turn analog off
                clrf    ANSELB
                clrf    ANSELC
                clrf    ANSELD
                clrf    ANSELE

                clrf    LATC                    ; Clear output latches
                clrf    LATE
             
                bcf     NRES_TRIS,NRES_PIN      ; Pull /RES low to reset 6502
                bcf     NRES_LAT,NRES_PIN
                bsf     NIRQ_TRIS,NIRQ_PIN      ; Ensure /IRQ is high
                bcf     NIRQ_LAT,NIRQ_PIN

                bcf     PHI0_TRIS,PHI0_PIN      ; Make PHI0 high output
                bsf     PHI0_LAT,PHI0_PIN
                bcf     NRAM_TRIS,NRAM_PIN      ; Make /RAM high output
                bsf     NRAM_LAT,NRAM_PIN

;-------------------------------------------------------------------------------

                movlw   b'01110000'             ; Set oscillator for 16 MIPs
                movwf   OSCCON
                bsf     OSCTUNE,PLLEN

                ifndef  __DEBUG
WaitTillStable:
                btfss   OSCCON,HFIOFS           ; And wait until stable
                bra     WaitTillStable
                endif

;-------------------------------------------------------------------------------

                bcf     TXD_TRIS,TXD_PIN        ; Make TXD an output    
                bsf     RXD_TRIS,RXD_PIN        ; .. and RXD an input
                                
                movlw   M(BRG16)                ; Configure UART
                movwf   BAUDCON1
                movlw   M(TXEN)
                movwf   TXSTA1 
                movlw   M(SPEN)|M(CREN)
                movwf   RCSTA1

                movf    RCREG1,W                ; Clear input buffer
                movf    RCREG1,W

;-------------------------------------------------------------------------------
                
                bcf     SCK_TRIS,SCK_PIN        ; Configure SPI pins
                bcf     SDO_TRIS,SDO_PIN
                bsf     SDI_TRIS,SDI_PIN
                bcf     SEL_TRIS,SEL_PIN
                
                movlw   M(SSPEN)|b'1010'        ; Configure SPI
                movwf   SSP1CON1
                
                movf    SSP1BUF,W
                movf    SSP1BUF,W
                
;-------------------------------------------------------------------------------

                movlw   TMR2_PR                 ; Configure Timer2
                movwf   PR2
                movlw   M(TMR2ON)|h'03'|((TMR2_POST-.1) << .3)
                clrf    TMR2
                movwf   T2CON
                
                bcf     PIR1,TMR2IF
                
;-------------------------------------------------------------------------------

                clrf    WREG
                clrf    DEVICE                  ; Assume a 6502
                bsf     INTCON,GIE

;===============================================================================
; Start Regular Clock Pulses
;-------------------------------------------------------------------------------

                movlw   .64
PulseClock:
                bcf     PHI0_LAT,PHI0_PIN       ; Make PHI0 low
                nop
                nop
                nop
                nop
                nop
                nop

                bsf     PHI0_LAT,PHI0_PIN       ; Make PHI0 high
                nop
                nop
                nop
                nop
                decfsz  WREG,F                  ; Reduce pulse count
                bra     PulseClock

                bsf     NRES_TRIS,NRES_PIN      ; Release /RES line
                nop
                nop
                nop
                nop

;===============================================================================
; Allow device to reset
;-------------------------------------------------------------------------------

WaitForReset:
                rcall   LoPhase

                bsf     PHI0_LAT,PHI0_PIN       ; Make PHI0 high
                nop

                movf    ADRL_PORT,W             ; Address is $FFFC?
                xorlw   h'fc'
                bnz     WaitForReset            ; No
                movf    ADRH_PORT,W
                xorlw   h'ff' & ADRH_MASK
                bnz     WaitForReset            ; No

                btfss   RW_PORT,RW_PIN          ; CPU is reading?
                bra     WaitForReset            ; No

                clrf    DATA_TRIS
                movlw   low BOOT_ADDR           ; Force boot addr low
                movwf   DATA_LAT
                nop
                nop

                rcall   LoPhase
                bsf     PHI0_LAT,PHI0_PIN       ; Make PHI0 high
                nop               

                movf    ADRL_PORT,W             ; Address is $FFFD?
                xorlw   h'fd'
                bnz     WaitForReset            ; No
                movf    ADRH_PORT,W
                xorlw   h'ff' & ADRH_MASK
                bnz     WaitForReset            ; No

                btfss   RW_PORT,RW_PIN          ; CPU is reading?
                bra     WaitForReset            ; No

                clrf    DATA_TRIS
                movlw   high BOOT_ADDR          ; Force boot addr high
                movwf   DATA_LAT
                nop
                nop

;-------------------------------------------------------------------------------

                rcall   LoPhase                 ; Execute a JMP ($00FF)
                movlw   h'6c'
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   h'ff'
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   h'00'
                rcall   HiPhaseLoad

                rcall   LoPhase                 ; Is next read from $00FF?   
                comf    ADRL_PORT,W             
                btfsc   STATUS,Z                ; No, 65C02
                bra     ForceAddress            ; Yes, 6502

                decf    DEVICE,F                ; Change device type
                movlw   h'00'                   ; Repeat address MSB
                rcall   HiPhaseLoad
                rcall   LoPhase

ForceAddress:
                movlw   low BOOT_ADDR           ; Force boot address
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   high BOOT_ADDR
                rcall   HiPhaseLoad

;-------------------------------------------------------------------------------

                btfsc   DEVICE,.7               ; Determine device
                bra     WDC65C02

R6502:
                movlw   low ROM6502             ; Point table pointer at ROM
                movwf   TBLPTRL
                movlw   high ROM6502
                movwf   TBLPTRH
                clrf    TBLPTRU
                bra     StartLoad

WDC65C02:
                movlw   low ROM65C02            ; Point table pointer at ROM
                movwf   TBLPTRL
                movlw   high ROM65C02
                movwf   TBLPTRH
                clrf    TBLPTRU

;-------------------------------------------------------------------------------

StartLoad:
                clrf    ADDRL                   ; Reset ROM load location
                movlw   high(ROM_BASE)
                movwf   ADDRH

LoadImage:
                tblrd   *+
                rcall   LoPhase                 ; Force LDA #data
                movlw   h'a9'
                rcall   HiPhaseLoad
                rcall   LoPhase
                movf    TABLAT,W
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   h'8d'                   ; Force STA abs
                rcall   HiPhaseLoad
                rcall   LoPhase
                movf    ADDRL,W
                rcall   HiPhaseLoad
                rcall   LoPhase
                movf    ADDRH,W
                rcall   HiPhaseLoad
                rcall   LoPhase
                rcall   HiPhaseWrite

                incf    ADDRL,F                 ; Next address
                bnz     LoadImage

                rcall   LoPhase                 ; Jump back to boot addr
                movlw   h'4c'                   ; .. every page
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   low BOOT_ADDR
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   high BOOT_ADDR
                rcall   HiPhaseLoad

                incf    ADDRH,F                 ; Until complete
                bnz     LoadImage

                rcall   LoPhase                 ; Restart using real ROM
                movlw   h'6c'                   ; .. image
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   h'fc'
                rcall   HiPhaseLoad
                rcall   LoPhase
                movlw   h'ff'
                rcall   HiPhaseLoad
                btfss   DEVICE,.7               ; Extra read for 65C02?
                bra     NoExtraRead             ; No, 6502
                rcall   LoPhase                 ; Yes
                movlw   h'ff'
                rcall   HiPhaseLoad
NoExtraRead:
                rcall   LoPhase
                rcall   HiPhaseRead
                rcall   LoPhase
                rcall   HiPhaseRead

                bra     Execute                 ; And start real execution

;-------------------------------------------------------------------------------

LoPhase:
                bcf     PHI0_LAT,PHI0_PIN       ; Make PHI0 low

                setf    DATA_TRIS               ; Tristate data bus
                bsf     NRAM_LAT,NRAM_PIN       ; Disable RAM
                nop
                nop
                return

;-------------------------------------------------------------------------------

HiPhaseLoad:
                bsf     PHI0_LAT,PHI0_PIN

                btfsc   RW_PORT,RW_PIN          ; Make DATA port an output
                clrf    DATA_TRIS               ; .. if CPU reading
                movwf   DATA_LAT                ; Save data to be read
                nop
                nop
                return

;-------------------------------------------------------------------------------

HiPhaseRead:
                bsf     PHI0_LAT,PHI0_PIN

                btfsc   RW_PORT,RW_PIN          ; Make RAM accessible
                bcf     NRAM_LAT,NRAM_PIN       ; .. if CPU reading
                nop
                nop
                return

;-------------------------------------------------------------------------------

HiPhaseWrite:
                bsf     PHI0_LAT,PHI0_PIN

                btfss   RW_PORT,RW_PIN          ; Make RAM accessible
                bcf     NRAM_LAT,NRAM_PIN       ; .. if CPU writing
                nop
                nop
                return

;===============================================================================
; Normal Execution
;-------------------------------------------------------------------------------

Execute:
                clrf    INT_FLAG
                clrf    INT_MASK                ; No interrupts enabled

                clrf    ACIA_CMD                ; Clear emulated registers
                clrf    ACIA_CTL
                
                clrf    SPI_CTL
                clrf    SPI_DIV
                clrf    SPI_SEL
                
                clrf    RTC_SUB0R
                clrf    RTC_SUB1R
                clrf    RTC_SEC0R
                clrf    RTC_SEC1R
                clrf    RTC_SEC2R
                clrf    RTC_SEC3R
                
                clrf    RTC_SUB0
                clrf    RTC_SUB1
                clrf    RTC_SEC0
                clrf    RTC_SEC1
                clrf    RTC_SEC2
                clrf    RTC_SEC3
                clrf    RTC_ALM0
                clrf    RTC_ALM1
                clrf    RTC_ALM2
                clrf    RTC_ALM3
                clrf    RTC_CTLA
                clrf    RTC_CTLB
                clrf    RTC_STAT
                
                bsf     INTCON,GIE
                
;-------------------------------------------------------------------------------

NormalLo:
                bcf     PHI0_LAT,PHI0_PIN       ; Make PHI0 low

                setf    DATA_TRIS               ; Tristate data bus
                bsf     NRAM_LAT,NRAM_PIN       ; Disable RAM

                movf    PIR1,W                  ; Any hardware interrupts
                andlw   M(INT_HW_RXD)|M(INT_HW_TXD)|M(INT_HW_SPI)
                iorwf   INT_FLAG,W              ; .. or software interrupts
                andwf   INT_MASK,W              ; .. to service?
                btfss   STATUS,Z
                bcf     NIRQ_TRIS,NIRQ_PIN      ; Yes
                btfsc   STATUS,Z
                bsf     NIRQ_TRIS,NIRQ_PIN      ; No

NormalHi:
                bsf     PHI0_LAT,PHI0_PIN       ; Make PHI0 high
                nop
                
                movf    ADRH_PORT,W             ; Read high byte of address
                xorlw   h'fe' & ADRH_MASK       ; I/O page?
                bz      IOAccess
                bcf     NRAM_LAT,NRAM_PIN       ; No, RAM access
                bra     NormalLo
                
IOAccess:
                movf    ADRL_PORT,W             ; Read low part of address
                andlw   h'3f'
                btfss   RW_PORT,RW_PIN          ; Read access?
                iorlw   h'40'                   ; Yes, add 64
                
                mullw   .2                      ; Work out jump offset
                rcall   ComputedJump            ; And go there
                
;-------------------------------------------------------------------------------
                
                bra     AciaRdData              ; ACIA  $FE00 RD
                bra     AciaRdStat
                bra     AciaRdCmnd
                bra     AciaRdCtrl
                bra     AciaRdData
                bra     AciaRdStat
                bra     AciaRdCmnd
                bra     AciaRdCtrl
                bra     AciaRdData
                bra     AciaRdStat
                bra     AciaRdCmnd
                bra     AciaRdCtrl
                bra     AciaRdData
                bra     AciaRdStat
                bra     AciaRdCmnd
                bra     AciaRdCtrl
             
                bra     SpiRdData               ; SPI65 $FE10 RD
                bra     SpiRdStat
                bra     SpiRdDvsr
                bra     SpiRdSlct
                bra     SpiRdData
                bra     SpiRdStat
                bra     SpiRdDvsr
                bra     SpiRdSlct
                bra     SpiRdData
                bra     SpiRdStat
                bra     SpiRdDvsr
                bra     SpiRdSlct
                bra     SpiRdData
                bra     SpiRdStat
                bra     SpiRdDvsr
                bra     SpiRdSlct
                
                bra     RtcRdSub0               ; RTC   $FE20 RD
                bra     RtcRdSub1
                bra     RtcRdSec0
                bra     RtcRdSec1
                bra     RtcRdSec2
                bra     RtcRdSec3
                bra     RtcRdAlm0
                bra     RtcRdAlm1
                bra     RtcRdAlm2
                bra     RtcRdAlm3
                bra     RtcRdCtlA
                bra     RtcRdCtlB
                bra     RtcRdStat
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                
                bra     BlankRd                 ; Empty $FE30 RD
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                
;-------------------------------------------------------------------------------

                bra     AciaWrData              ; ACIA  $FE00 WR
                bra     AciaWrStat
                bra     AciaWrCmnd
                bra     AciaWrCtrl
                bra     AciaWrData
                bra     AciaWrStat
                bra     AciaWrCmnd
                bra     AciaWrCtrl
                bra     AciaWrData
                bra     AciaWrStat
                bra     AciaWrCmnd
                bra     AciaWrCtrl
                bra     AciaWrData
                bra     AciaWrStat
                bra     AciaWrCmnd
                bra     AciaWrCtrl
                
                bra     SpiWrData               ; SPI65 $FE10 WR
                bra     SpiWrCtrl
                bra     SpiWrDvsr
                bra     SpiWrSlct
                bra     SpiWrData
                bra     SpiWrCtrl
                bra     SpiWrDvsr
                bra     SpiWrSlct
                bra     SpiWrData
                bra     SpiWrCtrl
                bra     SpiWrDvsr
                bra     SpiWrSlct
                bra     SpiWrData
                bra     SpiWrCtrl
                bra     SpiWrDvsr
                bra     SpiWrSlct
                
                bra     RtcWrSub0               ; RTC   $FE20 WR
                bra     RtcWrSub1
                bra     RtcWrSec0
                bra     RtcWrSec1
                bra     RtcWrSec2
                bra     RtcWrSec3
                bra     RtcWrAlm0
                bra     RtcWrAlm1
                bra     RtcWrAlm2
                bra     RtcWrAlm3
                bra     RtcWrCtlA
                bra     RtcWrCtlB
                bra     RtcWrStat
                bra     BlankRd
                bra     BlankRd
                bra     BlankRd
                
                bra     BlankWr                 ; Empty $FE30 WR
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                bra     BlankWr
                
;===============================================================================
; Blank Memory
;-------------------------------------------------------------------------------
                
BlankRd:
                movlw   h'ff'
                clrf    DATA_TRIS
                movwf   DATA_LAT
BlankWr:
                nop
                nop
                bra     NormalLo
                
;===============================================================================
; ACIA Emulation
;-------------------------------------------------------------------------------

AciaRdData:
                movf    RCREG1,W                ; Read UART
                clrf    DATA_TRIS               ; Place on data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo         

AciaWrData:
                movf    DATA_PORT,W             ; Read the data bus
                movwf   TXREG1                  ; And load UART
                nop
                nop
                bra    NormalLo

AciaRdStat:
                clrf    WREG                    ; Build status byte
                btfsc   PIR1,RC1IF              ; Test for receive data full
                iorlw   M(.7)|M(.3)
                btfsc   PIR1,TX1IF              ; Test for transmit data empty
                iorlw   M(.7)|M(.4)
                btfsc   RC1STA,OERR             ; Test for overrun error
                iorlw   M(.2)
                btfsc   RC1STA,FERR             ; Test for framing error
                iorlw   M(.1)
                clrf    DATA_TRIS               ; Place on data bus
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo                ; Continue
        
AciaWrStat:
                clrf    ACIA_CMD                ; Reset register
                clrf    ACIA_CTL
                bcf     INT_MASK,INT_HW_RXD     ; All interrupts
                bcf     INT_MASK,INT_HW_TXD     ; .. turned off
                nop
                nop
                bra     NormalLo                ; Continue

AciaRdCmnd:
                movf    ACIA_CMD                ; Fetch command bits
                clrf    DATA_TRIS               ; Place on data bus
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo                ; Continue

AciaWrCmnd:
                movf    DATA_PORT,W             ; Save native value
                movwf   ACIA_CMD
                bcf     INT_MASK,INT_HW_RXD     ; Assume all interrupts
                bcf     INT_MASK,INT_HW_TXD     ; .. turned off
                btfss   ACIA_CMD,.0             ; DTR clear?
                bra     NormalLo                ; Yes, all off
                btfss   ACIA_CMD,.1             ; IRD enabled?
                bsf     INT_MASK,INT_HW_RXD     ; Yes
                andlw   h'0c'                   ; Extract TIC
                xorlw   h'04'                   ; TX enabled?
                btfsc   STATUS,Z
                bsf     INT_MASK,INT_HW_TXD     ; Yes               
                bra     NormalLo                ; Continue

AciaRdCtrl:
                movf    ACIA_CTL                ; Fetch control bits
                clrf    DATA_TRIS               ; Place on data bus
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo                ; Continue

AciaWrCtrl:
                movf    DATA_PORT,W             ; Save native value
                movwf   ACIA_CTL
                
                andlw   h'0f'                   ; Set the baud rate
                mullw   .10
                rcall   ComputedJump
            
                movlw   low UART_BRG(.115200)   ; 0 = 115K
                movwf   SPBRG1
                movlw   high UART_BRG(.115200)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.50)       ; 1 = 50
                movwf   SPBRG1
                movlw   high UART_BRG(.50)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.75)       ; 2 = 75
                movwf   SPBRG1
                movlw   high UART_BRG(.75)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.110)      ; 3 = 110
                movwf   SPBRG1
                movlw   high UART_BRG(.110)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.134)      ; 4 = 134
                movwf   SPBRG1
                movlw   high UART_BRG(.134)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.150)      ; 5 = 150
                movwf   SPBRG1
                movlw   high UART_BRG(.150)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.300)      ; 6 = 300
                movwf   SPBRG1
                movlw   high UART_BRG(.300)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.600)      ; 7 = 600
                movwf   SPBRG1
                movlw   high UART_BRG(.600)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.1200)     ; 8 = 1200
                movwf   SPBRG1
                movlw   high UART_BRG(.1200)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.1800)     ; 9 = 1800
                movwf   SPBRG1
                movlw   high UART_BRG(.1800)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.2400)     ; A = 2400
                movwf   SPBRG1
                movlw   high UART_BRG(.2400)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.3600)     ; B = 3600
                movwf   SPBRG1
                movlw   high UART_BRG(.3600)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.4800)     ; C = 4800
                movwf   SPBRG1
                movlw   high UART_BRG(.4800)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.7200)     ; D = 7200
                movwf   SPBRG1
                movlw   high UART_BRG(.7200)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.9600)     ; E = 9600
                movwf   SPBRG1
                movlw   high UART_BRG(.9600)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

                movlw   low UART_BRG(.19200)    ; F = 19200
                movwf   SPBRG1
                movlw   high UART_BRG(.19200)
                movwf   SPBRGH1
                bra     NormalLo                ; Continue

;===============================================================================
; SPI65 Emulation
;-------------------------------------------------------------------------------

SpiRdData:
                movf    SSP1BUF,W
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo

SpiWrData:
                movf    DATA_PORT,W
                movwf   SSP1BUF
                nop
                nop
                bra     NormalLo 

SpiRdStat:
                movf    SPI_CTL,W               ; Combine control bits
                btfsc   SSP1STAT,BF             ; .. with buffer full
                iorlw   M(.7)
                btfsc   SSP1STAT,R_NOT_W        ; .. and busy flag
                iorlw   M(.5)
                clrf    DATA_TRIS               ; Then set as output
                movwf   DATA_LAT
                nop
                nop
                nop
                bra     NormalLo
                
SpiWrCtrl:
                movf    DATA_PORT,W
                andlw   h'5f'
                movwf   SPI_CTL
                bcf     SSP1CON1,SSPEN
                bcf     SSP1STAT,CKE            ; Map CPOL
                btfsc   WREG,.1
                bsf     SSP1STAT,CKE
                bcf     SSP1STAT,SMP            ; Map CPHA
                btfsc   WREG,.0
                bsf     SSP1STAT,SMP
                bsf     SSP1CON1,SSPEN
                nop
                nop
                nop
                bra     NormalLo 

SpiRdDvsr:
                movf    SPI_DIV,W
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo 

SpiWrDvsr:
                movf    DATA_PORT,W
                andlw   h'3f'
                movwf   SPI_DIV
                movwf   SSP1ADD
                nop
                bra     NormalLo                

SpiRdSlct:
                movf    SPI_SEL
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo
 
SpiWrSlct:
                movf    DATA_PORT,W             ; Save native value
                movwf   SPI_SEL
                xorwf   LATC,W                  ; And set RC2 to match
                andlw   M(SEL_PIN)
                xorwf   LATC,F
                nop
                bra     NormalLo
                
;===============================================================================
; RTC Emulation
;-------------------------------------------------------------------------------

RtcRdSub0:       
                movf    RTC_SUB0,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo 
                
RtcWrSub0: 
                movf    DATA_PORT,W             ; Copy from data bus
                andlw   h'f1'
                movwf   RTC_SUB0R               ; .. to registers
                movwf   RTC_SUB0
        ; Handle SQWS
                nop
                nop
                bra     NormalLo
        
RtcRdSub1:     
                movf    RTC_SUB1,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo
                
RtcWrSub1:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_SUB1R               ; .. to registers
                movwf   RTC_SUB1
                nop
                nop
                bra     NormalLo
        
RtcRdSec0:
                movf    RTC_SEC0,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo 

RtcWrSec0:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_SEC0R               ; .. to registers
                movwf   RTC_SEC0
                nop
                nop
                bra     NormalLo
                
RtcRdSec1:       
                movf    RTC_SEC1,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo
                
RtcWrSec1:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_SEC1R               ; .. to registers
                movwf   RTC_SEC1
                nop
                nop
                bra     NormalLo
                
RtcRdSec2:      
                movf    RTC_SEC2,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo
                
RtcWrSec2:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_SEC2R               ; .. to registers
                movwf   RTC_SEC2
                nop
                nop
                bra     NormalLo
                
RtcRdSec3:       
                movf    RTC_SEC3,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo
                
RtcWrSec3:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_SEC3R               ; .. to registers
                movwf   RTC_SEC3
                nop
                nop
                bra     NormalLo
                
RtcRdAlm0:
                movf    RTC_ALM0,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo 
                
RtcWrAlm0:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_ALM0                ; .. to register
                nop
                nop
                bra     NormalLo
                
RtcRdAlm1:
                movf    RTC_ALM1,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo 
                
RtcWrAlm1:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_ALM1                ; .. to register
                nop
                nop
                bra     NormalLo
                
RtcRdAlm2:
                movf    RTC_ALM2,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo 
                
RtcWrAlm2:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_ALM2                ; .. to register
                nop
                nop
                bra     NormalLo
                
RtcRdAlm3:
                movf    RTC_ALM3,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo 
                
RtcWrAlm3:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_ALM3                ; .. to register
                nop
                nop
                bra     NormalLo
                
RtcRdCtlA:
                movf    RTC_CTLA,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo
        
RtcWrCtlA:
                movf    DATA_PORT,W             ; Copy from data bus
                andlw   h'cf'
                movwf   RTC_CTLA                ; .. to register
                nop
                nop
                bra     NormalLo
                
RtcRdCtlB:
                movf    RTC_CTLB,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo
        
RtcWrCtlB:
                movf    DATA_PORT,W             ; Copy from data bus
                movwf   RTC_CTLB                ; .. to register
                nop
                nop
                bra     NormalLo
                
RtcRdStat:
                movf    RTC_STAT,W              ; Copy from register
                clrf    DATA_TRIS               ; .. to the data bus
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo
        
RtcWrStat:
                clrf    RTC_STAT                ; Any write to STAT clears
                nop                             ; .. all the flags
                nop
                bra     NormalLo
        
;===============================================================================
; Computed Jump
;-------------------------------------------------------------------------------
                
ComputedJump:
                movf    PRODL,W                 ; Add PROD to return address
                addwf   TOSL,F
                movf    PRODH,W
                addwfc  TOSH,F
                return                          ; And go there
                
;===============================================================================
; Uart Interface
;-------------------------------------------------------------------------------

UartTx:
                btfss   PIR1,TX1IF              ; Wait until able to TX
                bra     UartTx
                movwf   TXREG1                  ; Then send the character
                return

;===============================================================================
; Boot ROM Images
;-------------------------------------------------------------------------------

.BootROM        code_pack

ROM6502:
                include "boot-6502.asm"

ROM65C02:
                include "boot-65c02.asm"

                end