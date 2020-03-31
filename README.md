# SB-6502

This repository contains the firmware for a simple single board computer that uses only three chips, namely:

- A MOS 6502, WDC 65C02, R65SC02 or WDC 65C802 microprocessor
- A 128K SRAM memory chip (of which 64K is used)
- A 18F45K22 or 18F47K40 PIC micro-controller

The complete system, including a USB serial adapter and Micro SD card interface module, looks like this when assembled.

![SB-6502 Version 1](images/sb-6502.jpg)

The board in the photo is a version 1 PCB and lacks a pull up on the /SO pin (not that it seems to matter to any of the processors). The PIC is also installed on a development daughter board which allows in circuit programming, normally its plugged straight into the PCB. The latest version of the board is slightly smaller than 10cm x 10cm, includes an extra resistor (for /SO) and has black solder mask.

The board is designed to take either a MOS 6502 or a WDC 65C02. A different socket is provided for each as some of the control signal pins differ between the two chips but the layout of the board overlaps the two sockets.

Both Rockwell and WDC made chips that where pin compatible with the original MOS 6502, namely:

- R65C02
- R65SC02
- W65C802

I have tested the board with Synertek (MOS) 6502, California Micro Devices G65SC02, WDC 65C802, GTE G65SC802 and WDC 65C02 devices.

On the prototype board I cut two 40 pin DIP sockets up and soldered them in so the same board can be used for any type of chip. On my other boards I usually just install a single 40 pin socket.

In order to make a working system the PIC must act as the source of the microprocessor's clock signal and decode it's control signals to either enable the memory or emulate a peripheral chip at the appropriate time.

## Booting a ROM-less System

Normally a microprocessor needs a ROM to hold the initial firmware that gets the system up and running. In this design the microprocessor does not have a ROM so we have to get the microprocessor to create one by feeding it fake instructions from the PIC.

The PIC generates a series of clock pluses while the microprocessor is released from reset. The microprocessor performs an interrupt sequence pushing a random PC value on the stack followed by the status flags. Then it reads the reset vector from $FFFC/D. The PIC provides data values back to the microprocessor as if it was a ROM

### Telling the 6502, 65C02 and 65C802 Apart

The PIC cannot tell directly which socket is populated or what type of device it is. The PIC uses the fact that the 6502, 65C02 and 65C802 treat JMP ($FFFF) instructions differently. All will read the low byte of the target address from $FFFF but as 6502 reads the high byte from $FF00 while the 65C02 (/65SC02) and 65C802 increment the address correctly and read from $0000 but take a different number of cycles to do so.

There are other ways you could tell the processors apart but this technique is easy to implement. The PIC feeds the microprocessor the jump indirect instruction when it resets and examines address bus values to see which memory address is accessed in each clock cycle.

### 65C02 vs 65SC02

The latest revision of the firmware can differentiate between the 65C02 and 65SC02. The 65SC02 is largely the same as the 65C02 but lacks the bit instructions RMB, SMB, BBR and BBR. On a 65SC02 these opcodes function as one byte NOP instructions.

Detection is accomplished by executing an additional SMB0 instruction and checking to see if zero page is accessed to read a target byte (e.g. a 65C02) or not (e.g. a 65SC02).

### Loading the ROM

Once the PIC knows what type of microprocessor is installed it can generate the instructions needed to create an appropriate ROM image in the memory. 

The 18F45K22 PIC only has room for 3 ROM images (e.g. 6502, 65C02 and 65C802) while the 18F47K40 can hold four (e.g. 6502, 65C02, 65SC02 and 65C802).

The ROM is transferred by generating a pairs of byte load and store instructions (e.g. LDA #data STA $addr) for each byte in the ROM image. These instructions appear to have come from memory but infact the RAM is unchanged by them except when the PIC enables the SRAM chip during the cycle when the microprocessor writes the byte value to store it in the ROM memory area ($C000-$FFFF).

Every 256 bytes a JMP $1000 instruction is generated to reset the program counter to a lower value.

When all of the image has been transferred a reset signal is generated to restart the microprocessor through the ROM's reset vector and the PIC firmware changes to normal operation mode.

### Normal Operation

In normal operation the PIC becomes subservient to the microprocessor. It continues to generate the clock pulse but now it examines the control signals and address bus value to determine what data the microprocessor is trying to access. Most of the time the microprocessor will be accessing the static RAM memory chip but if the address is in the $FE00-$FE3F range then the address is interpreted as a virtual peripheral access.

## Memory

The memory map of the microprocessor is mapped almost entirely to 64K of RAM. Only 64 bytes between $FE00 and $FE3F are used to map virtual devices into the address space.

Start | End   | Description
----- | ----- | ------------------
$0000 | $00FF | Zero Page
$0100 | $01FF | Stack
$0200 | $BF00 | RAM
$C000 | $EFFF | ROM Image (in RAM)
$F000 | $FDFF | Monitor (in RAM)
$FE00 | $FE0F | Virtual 6551 ACIA
$FE10 | $FE1F | Virtual 65SPI
$FE20 | $FE2F | Virtual DS1318 RTC
$FE30 | $FE3F | Virtual Flash Interface
$FE40 | $FFFF | Core I/O Functions + Vectors

The boot code copies a 16K ROM image into the memory area $C000 to $FFFF but this can be overwritten by a user program.

## Virtual Devices

The PIC code implements four virtual peripherals, an 6551 ACIA, a DS1318 RTS, a 65SPI and a flash ROM interface. The features of these four chips are mapped to the PIC's hardware.

In program code the following addresses should be used to access the peripheral registers.

    ; Emulated 6551 ACIA

    ACIA_DATA       .equ    $fe00           ; R/W
    ACIA_STAT       .equ    $fe01           ; R/W
    ACIA_CMND       .equ    $fe02           ; R/W
    ACIA_CTRL       .equ    $fe03           ; R/W

    ; Emulated 65SPI

    SPI_DATA        .equ    $fe10           ; R/W
    SPI_STAT        .equ    $fe11           ; R/O
    SPI_CTRL        .equ    $fe11           ; W/O
    SPI_DVSR        .equ    $fe12           ; R/W
    SPI_SLCT        .equ    $fe13           ; R/W

    ; Emulated DS1318 RTC

    RTC_SUB0        .equ    $fe20           ; R/W
    RTC_SUB1        .equ    $fe21           ; R/W
    RTC_SEC0        .equ    $fe22           ; R/W
    RTC_SEC1        .equ    $fe23           ; R/W
    RTC_SEC2        .equ    $fe24           ; R/W
    RTC_SEC3        .equ    $fe25           ; R/W
    RTC_ALM0        .equ    $fe26           ; R/W
    RTC_ALM1        .equ    $fe27           ; R/W
    RTC_ALM2        .equ    $fe27           ; R/W
    RTC_ALM3        .equ    $fe28           ; R/W
    RTC_CTLA        .equ    $fe2a           ; R/W
    RTC_CTLB        .equ    $fe2b           ; R/W
    RTC_STAT        .equ    $fe2c           ; R/W

    ; Flash ROM Interface

    ROM_OFFL        .equ    $fe30           ; R/W
    ROM_OFFH        .equ    $fe31           ; R/W
    ROM_DATA        .equ    $fe32           ; R/W
    ROM_LOCK        .equ    $fe33           ; R/W

Accessing some of these registers takes an extended period of time during which the microprocessor experiences clock stretching while the operation is completed.

### ACIA

The PIC partially implements 6551 'Asynchronous Communication Interface Adapter' (ACIA). It omits the flow control features of the real chip.

Addr  | Register
----- | --------
$FE00 | Data
$FE01 | Status
$FE02 | Command
$FE03 | Control

### SPI65

The SPI65 is a simple SPI controller implemented in a CPLD designed by members of the 6502.org web forum. 

Addr  | Register
----- | --------
$FE10 | Data
$FE11 | Status
$FE12 | Control
$FE13 | Divisor
$FE14 | Select

Only one chip select pin is available at it is controlled by bit 2 of the select register.

### DS1318

The DS1318 is a relatively simple real time clock chip that counts elapsed time and can generate a periodic interrupt at a configurable rate. Once enabled the sub-second counter in the DS1318 is incremented 4096 times per second. If the seconds portion of this value is the same as the that held in the alarm then an interrupt can be raised.

Addr  | Register
----- | --------
$FE20 | Sub-seconds LO
$FE21 | Sub-seconds HI
$FE22 | Seconds LSW/LSB
$FE23 | Seconds LSW/MSB
$FE24 | Seconds MSW/LSB
$FE25 | Seconds MSW/MSB
$FE26 | Alarm LSW/LSB
$FE27 | Alarm LSW/MSB
$FE28 | Alarm MSW/LSB
$FE29 | Alarm MSW/MSB
$FE2A | Control A
$FE2B | Control B
$FE2C | Status

A real DS1318 chip has the ability to generate a square wave output at different frequencies. This feature has not been emulated (as there are no spare pins to output it through).

## Flash ROM

The ROM image for detected microprocessor can be 'self-written' to allow ROM and monitor updates without reprogramming the PIC

The Flash ROM is accessed through four registers

Addr  | Register
----- | --------
$FE30 | Offset LSB
$FE31 | Offset MSB
$FE32 | Data
$FE33 | Lock

To read from flash set the offset and then access the data register to read successive bytes from the image in flash memory. For example to read 256 bytes into a RAM buffer:
```
        ldx     #0
        stx     ROM_OFFL
        stx     ROM_OFFH
        repeat
         lda    ROM_DATA
         sta    MEMORY,X
         inx
        until eq
```
To write flash you must set the offset and perform an unlock sequence before changing the image. For example:
```
        ldx     #0
        stx     ROM_OFFL
        stx     ROM_OFFH
        lda     #$55
        sta     ROM_LOCK
        lda     #$aa
        sta     ROM_LOCK
        repeat
         lda    MEMORY,X
         sta    ROM_DATA
         inx
        until eq
```
Writes must start on a 16 byte aligned offset ($xxx0) to correctly erase the old content.

## Firmware

The PIC contains three (18F45K22) or four (18F47K40) 16K ROM images, one for each supported processor type, containing a simple boot monitor that allows you to examine and change the memory, download S19 records and execute code.

>Later releases will add an eForth interpreter that can be started from the monitor.

### Memory Usage

The monitor resides in the top 4K of memory from $F000 to $FFFF. The code needed to provide interrupt driven serial I/O is held in the area above the I/O registers (e.g. $FE40-$FFFF) and provides a set of entry points for user programs to call.

Address | Name | Description
------|----------|---------
$FE40 | UARTTX | Adds a character to the UART TX buffer    
$FE43 | UARTRX | Fetches a character from the UART RX buffer
$FE46 | UARTTXCOUNT | Returns the number of characters in the TX buffer
$FE49 | UARTRXCOUNT | Returns the number of characters in the RX buffer
$FE4C | SPISENDIDLE | Sends $FF through MOSI and returns the received byte from MISO
$FE4F | SPISENDDATA | Sends a data byte through MOSI and returns the received byte from MISO

The memory area from $0200 to $027F is used to hold some interrupt vectors, the UART buffers and their head/tail indexes. The Monitor uses $0280 to $02FF as a command buffer.

Address | Name | Description
------|--------|------------
$0200 | IRQV | 65(C)02 Interrupt Vector
$0200 | NMIV | 65(C)02 Non-Maskable Interrupt Vector
$0204 | IRQNV | 65C802 Native Interrupt Vector
$0206 | NMINV | 65C802 Native Non-Maskable Vector

The vectors are initialised to point at a default handlers. The IRQNV and NMINV vectors are only used by the WDC 65C802 version of the monitor.

If you write your own program then you can use the zero page locations $00 to $DF and the main RAM area between $0300 to $EFFF without corrupting the monitor.

As the monitor image is entirely held in RAM it can be completely overwritten (provided you disable interrupts while doing so) but it is probably more practical to preserve the interrupt handler code at $FE40-$FFFF, the associated vectors/buffers area at $0200 to $027F and the zero page temporary area at $FE-$FF (used by the 6502 and 65C02 code). Everything else is up for grabs.

### Monitor Commands

When the firmware boots it configures the UART to work at 19200 baud, 8-bits with no parity and XON/XOFF flow control then prints a message showing the type of device detected and the version of the firmware. It then executes a BRK instruction to enter the monitor which displays the registers and prompts for user input.

    SB-6502 [20.03]
    PC=F0C6 A=00 X=17 Y=00 P=..11..ZC SP=FF
    .

The monitor supports a small set of commands, shown in the following table, sufficient to load more elaborate applications into memory and perform basic memory examination and changes.

Command | Description
------------|-----------
D xxxx [yyyy] | Disassmble the memory between addresses xxxx and yyyy
M xxxx yyyy | Display the memory between addresses xxxx and yyyy in bytes
F xxxx yyyy bb | File the memory between xxxx and yyyy with the byte bb
G [xxxx] | Start program execution at xxxx or the last break location if not specified.
R | Displays the values of all the registers
S... | Interprets an S19 record and load it into memory
T [xxxx [yyyy]] | Trace yyyy (default 1) instructions starting at xxxx (defaults to PC)
W xxxx bb | Set memory address xxxx to byte bb. Automatically prompts for the next address
? | Print a summary of all the commands

The easiest way to load a new application is to assemble your program code and produce an S19 output file. Then use the file transfer capabilities of your serial terminal application to send the S19 file to the SB-6502. If possible set an inter-line delay to give the SB-6502 time to process the line before the next is sent.

## Notes

You could use this design with other 40 pin PIC 18F chips such as the 18F4680 with a few changes. The advantage of the 18F46K22 is that it executes a little faster on its internal oscillator than the older 18F chips (i.e. 16 MIPS vs 10 MIPS) which in turn means that the microprocessor is clocked a higher rate.

The WDC 65C802 is quite a rare chip and unfortunately I have made it rarer by buying up a number of them in recent years. The 16-bit registers available in native mode make programming it quite a different experience from the regular 65(C)02.

## Bugs/Features

1. The PIC runs too quickly to support the 50 and 75 baud rate settings provided by a real 6551 ACIA.
2. The SB-65C802 monitor does not have a trace command.