# SB-6502

This repository contains the firmware for a simple single board computer that uses only three chips, namely:

- A MOS 6502, WDC 65C02 or WDC 65C802 microprocessor
- A 128K SRAM memory chip (of which 64K is used)
- A 16F45K22 PIC micro-controller

The complete system, including a USB serial adapter and Micro SD card interface module, looks like this when assembled.

![SB-6502 Version 1](images/sb-6502.jpg)

In order to make a working system the PIC must act as the source of the microprocessor's clock signal and decode it's control signals to either enable the memory or emulate a peripheral chip at the appropriate time.

## Booting a ROM-less System

Normally a microprocessor needs a ROM to hold the initial firmware that gets the system up and running. In this design the microprocessor does not have a ROM so we have to get the microprocessor to create one by feeding it fake instructions from the PIC.

The PIC generates a series of clock pluses while the microprocessor is released from reset. The microprocessor performs an interrupt sequence pushing a random PC value on the stack followed by the status flags. Then it reads the reset vector from $FFFC/D. The PIC provides data values back to the  microprocessor as if it was a ROM

### Telling the 6502, 65C02 and 65C802 Apart

The board is designed to take either a MOS 6502 or a WDC 65C02. A different socket is provided for each as some of the control signal pins differ between the two chips. A WDC 65C802 can also be used as it is pin compatible with the original 6502.

The PIC uses the fact that the 6502, 65C02 and 65C802 treat JMP ($00FF) instructions differently. All will read the low byte of the target address from $00FF but as 6502 reads the high byte from $0000 while the 65C02 and 65C802 increment the address correctly and read from $0100 but take a different number of cycles to do so.

There are other ways you could tell the processors apart but this technique is easy to implement. The PIC feeds the microprocessor the jump indirect instruction when it resets and examines address bus values to see which memory address is accessed in each clock cycle. It then knows which boot ROM image to download.

### Loading the ROM

Once the PIC knows what type of microprocessor is installed it can generate the instructions needed to create an appropriate ROM image in the memory. The ROM is transferred by generating a pair of byte load and store (e.g. LDA #$xx STA $yyyy) instructions for each byte. The PIC enables the SRAM chip during the cycle when the microprocessor writes the byte value to store it in the memory.

Every 256 bytes a JMP $1000 instruction is generated to reset the program counter to a lower value. When all of the image has been transferred a JMP ($FFFC) instruction is generated to restart the microprocessor through the ROM's reset vector and the PIC firmware changes to normal operation mode.

## Normal Operation

In normal operation the PIC becomes subservient to the microprocessor. It continues to generate the clock pulse but now it examines the control signals and address bus value to determine what data the microprocessor is trying to access. Most of the time the microprocessor will be accessing the SRAM memory chip but if the address is in the $FE00-$FEFF range then address is interpreted as a virtual peripheral access.

The PIC code implements three virtual peripherals, an 6551 ACIA, a DS1318 RTS and a 65SPI (a SPI controller implemented in a CPLD designed by members of the 6502.org web forum). The features of these two chips are mapped to the PICs hardware.

In program code the following addresses should be used to access the peripheral registers.

    ; Emulated 6551 ACIA

    ACIA_DATA       .equ    $ff00           ; R/W
    ACIA_STAT       .equ    $ff01           ; R/W
    ACIA_CMND       .equ    $ff02           ; R/W
    ACIA_CTRL       .equ    $ff03           ; R/W

    ; Emulated 65SPI

    SPI_DATA        .equ    $ff10           ; R/W
    SPI_STAT        .equ    $ff11           ; R/O
    SPI_CTRL        .equ    $ff11           ; W/O
    SPI_DVSR        .equ    $ff12           ; R/W
    SPI_SLCT        .equ    $ff13           ; R/W

    ; Emulated DS1318 RTC

    RTC_SUB0        .equ    $ff20           ; R/W
    RTC_SUB1        .equ    $ff21           ; R/W
    RTC_SEC0        .equ    $ff22           ; R/W
    RTC_SEC1        .equ    $ff23           ; R/W
    RTC_SEC2        .equ    $ff24           ; R/W
    RTC_SEC3        .equ    $ff25           ; R/W
    RTC_ALM0        .equ    $ff26           ; R/W
    RTC_ALM1        .equ    $ff27           ; R/W
    RTC_ALM2        .equ    $ff27           ; R/W
    RTC_ALM3        .equ    $ff28           ; R/W
    RTC_CTLA        .equ    $ff2a           ; R/W
    RTC_CTLB        .equ    $ff2b           ; R/W
    RTC_STAT        .equ    $ff2c           ; R/W

Accessing some of these registers takes an extended period of time during which the microprocessor experiences clock stretching.

## Firmware

The PIC contains three 8K ROM images, one for each supported processor type. 

## Notes

You could use this design with other 40 pin PIC 18F chips such as the 18F4680 with a few changes. The advantage of the 18F46K22 is that it executes a little faster on its internal oscillator than the older 18F chips (i.e. 16 MIPS vs 10 MIPS) which in turn means that the microprocessor is clocked a higher rate.