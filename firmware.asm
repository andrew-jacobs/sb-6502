;===============================================================================
;  ____  ____         __  ____   ___ ____  
; / ___|| __ )       / /_| ___| / _ \___ \ 
; \___ \|  _ \ _____| '_ \___ \| | | |__) |
;  ___) | |_) |_____| (_) |__) | |_| / __/ 
; |____/|____/       \___/____/ \___/_____|
;                                         
; A Firmware for a Three Chip 6502/65C02 Single Board Computer
;-------------------------------------------------------------------------------
; Copyright (C),2014-2016 HandCoded Software Ltd.
; All rights reserved.
;
;===============================================================================
;
; Notes:
;
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

UART_BAUD       equ     .57600

UART_BRG        equ     FOSC / (.16 * UART_BAUD) - .1
        
; SPI Signal Pins
        



;===============================================================================
;-------------------------------------------------------------------------------

                udata_acs

DEVICE          res     .1                      ; h'00' if 6502, h'ff' if 65C02

ADDRL           res     .1                      ; Address next ROM byte to force
ADDRH           res     .1                      ; .. load

SCRATCH         res     .1                      ; Scratch area

;-------------------------------------------------------------------------------

INT_MASK        res     .1                      ; Hardware interrupt mask

ACIA_CMD        res     .1
ACIA_CTL        res     .1

SPI_DIV         res     .1
SPI_SEL         res     .1

;===============================================================================
; Power On Reset
;-------------------------------------------------------------------------------

.ResetVector    code    h'0000'

                goto    PowerOnReset

;-------------------------------------------------------------------------------

                code
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
                btfss   OSCCON,HFIOFS
                bra     WaitTillStable
                endif

;-------------------------------------------------------------------------------

                bcf     TRISC,TXD_PIN           ; Make TXD an output    

                movlw   low UART_BRG            ; Configure UART
                movwf   SPBRG1
                movlw   high UART_BRG
                movwf   SPBRGH1
                movlw   M(BRG16)
                movwf   BAUDCON1
                movlw   M(TXEN)
                movwf   TXSTA1 
                movlw   M(SPEN)|M(CREN)
                movwf   RCSTA1

                movf    RCREG1,W                ; Clear input buffer
                movf    RCREG1,W

;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------

                clrf    WREG
                rcall   UartTx
                rcall   UartTx
                rcall   NewLine
                rcall   NewLine

                clrf    DEVICE                  ; Assume a 6502

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
                rcall   StringOut
                db      "SB-6502 [16.05]",CR,LF,.0

                movlw   low ROM6502             ; Point table pointer at ROM
                movwf   TBLPTRL
                movlw   high ROM6502
                movwf   TBLPTRH
                clrf    TBLPTRU
                bra     StartLoad

WDC65C02:
                rcall   StringOut
                db      "SB-65C02 [16.05]",CR,LF,.0

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
                clrf    INT_MASK                ; No interrupts enabled

        ; Setup ACIA
        ; Setup SPI65

NormalLo:
                bcf     PHI0_LAT,PHI0_PIN       ; Make PHI0 low

                setf    DATA_TRIS               ; Tristate data bus
                bsf     NRAM_LAT,NRAM_PIN       ; Disable RAM

                movf    PIR1,W                  ; Any hardware interrupts
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
                bnz     RamAccess

                if      0
                rcall   NewLine

                movlw   'R'
                btfss   RW_PORT,RW_PIN
                movlw   'W'
                rcall   UartTx

                movf    ADRH_PORT,W
                rcall   DumpByte
                movf    ADRL_PORT,W
                rcall   DumpByte

                movlw   ' '
                rcall   UartTx
                endif
                
                movlw   high(DeviceJump)
                movwf   PCLATH
                movf    ADRL_PORT,W             ; Read low part of address
                andlw   h'07'                   
                btfss   RW_PORT,RW_PIN          ; Read access?
                iorlw   h'08'
                
                rlncf   WREG,F                  ; Work out jump address
                addlw   low(DeviceJump)
                btfsc   STATUS,C
                incf    PCLATH,F
                movwf   PCL

DeviceJump:
                bra     AciaRdData              ; ACIA read
                bra     AciaRdStat
                bra     AciaRdCmnd
                bra     AciaRdCtrl
                bra     SpiRdData               ; SPI65 read
                bra     SpiRdStat
                bra     SpiRdDvsr
                bra     SpiRdSlct

                bra     AciaWrData              ; ACIA write
                bra     AciaWrStat
                bra     AciaWrCmnd
                bra     AciaWrCtrl
                bra     SpiWrData               ; SPI65 write
                bra     SpiWrCtrl
                bra     SpiWrDvsr
                bra     SpiWrSlct

RamAccess:
                bcf     NRAM_LAT,NRAM_PIN       ; No, RAM access
                bra     NormalLo

;===============================================================================
; ACIA Emulation
;-------------------------------------------------------------------------------

AciaRdData:
                movf    RCREG1,W                ; Read UART
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo

AciaWrData:
                movf    DATA_PORT,W             ; Write UART
                movwf   TXREG1
                nop
                nop
                bra    NormalLo

AciaRdStat:
                clrf    WREG                    ; Build status byte
                btfsc   PIR1,RC1IF
                iorlw   M(.7)|M(.3)
                btfsc   PIR1,TX1IF
                iorlw   M(.7)|M(.4)
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra     NormalLo
        
AciaWrStat:
                nop                             ; Ignore for now
                nop
                nop
                bra    NormalLo

AciaRdCmnd:
                movf    ACIA_CMD
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo

AciaWrCmnd:
                movf    DATA_PORT,W             ; Save native value
                movwf   ACIA_CMD
                bcf     INT_MASK,RC1IF          ; Assume all interrupts
                bcf     INT_MASK,TX1IF          ; .. turned off
                btfss   ACIA_CMD,.0             ; DTR clear?
                bra     NormalLo                ; Yes, all off
                btfss   ACIA_CMD,.1             ; IRD enabled?
                bsf     INT_MASK,RC1IF          ; Yes
                andlw   h'0c'                   ; Extract TIC
                xorlw   h'04'                   ; TX enabled?
                btfsc   STATUS,Z
                bsf     INT_MASK,TX1IF          ; Yes               
                bra     NormalLo

AciaRdCtrl:
                movf    ACIA_CTL
                clrf    DATA_TRIS
                movwf   DATA_LAT
                nop
                nop
                bra    NormalLo

AciaWrCtrl:
                movf    DATA_PORT,W             ; Save native value
                movwf   ACIA_CTL
                nop                             ; Translate to PIC
                nop
                bra    NormalLo

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
                nop
                nop
                nop
                bra     NormalLo
                
SpiWrCtrl:
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
                movwf   SPI_DIV
                nop
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
                andlw   M(.2)
                xorwf   LATC,F
                nop
                bra     NormalLo

;===============================================================================
; Uart Interface
;-------------------------------------------------------------------------------

UartTx:
                btfss   PIR1,TX1IF              ; Wait until able to TX
                bra     UartTx
                movwf   TXREG1                  ; Then send the character
                return

;-------------------------------------------------------------------------------

NewLine:
                movlw   CR
                rcall   UartTx
                movlw   LF
                bra     UartTx


StringOut:
                movf    TOSL,W                  ; Use return address as
                movwf   TBLPTRL                 ; .. string pointer
                movf    TOSH,W
                movwf   TBLPTRH
                movf    TOSU,W
                movwf   TBLPTRU
StrLoop:
                tblrd   *+                      ; Fetch character from string
                movf    TABLAT,W
                bz      StrDone
                rcall   UartTx
                bra     StrLoop
StrDone:
                btfsc   TBLPTRL,.0              ; Finished on an odd address
                tblrd   *+                      ; Yes, skip to even address

                movf    TBLPTRL,W               ; Replace the return address
                movwf   TOSL
                movf    TBLPTRH,W
                movwf   TOSH
                movf    TBLPTRU,W
                movwf   TOSU
                return                          ; Done

;===============================================================================
; Boot ROM Image
;-------------------------------------------------------------------------------

.BootROM        code_pack h'e000'

ROM6502:
                include "boot-6502.asm"

ROM65C02:
                include "boot-65c02.asm"

                end