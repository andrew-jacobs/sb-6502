
;===============================================================================
; Virtual Peripherals
;-------------------------------------------------------------------------------

ACIA_DATA	.EQU	$FE00
ACIA_STAT	.EQU	$FE01
ACIA_CMND	.EQU	$FE02
ACIA_CTRL	.EQU	$FE03

SPI_DATA	.EQU	$FE04
SPI_STAT	.EQU	$FE05
SPI_CTRL	.EQU	$FE05
SPI_DVSR	.EQU	$FE06
SPI_SLCT	.EQU	$FE07

;===============================================================================
; ASCII Control Characters
;-------------------------------------------------------------------------------

NUL		.EQU	$00
BEL		.EQU	$07
BS		.EQU	$08
HT		.EQU	$09
CR		.EQU	$0D
LF		.EQU	$0A
DC1		.EQU	$11			; XON
DC3		.EQU	$13			; XOFF
ESC		.EQU	$1B
DEL		.EQU	$7F

;===============================================================================
;-------------------------------------------------------------------------------

OP_ERR		.EQU	$00
OP_ADC		.EQU	$02
OP_AND		.EQU	$04
OP_ASL		.EQU	$06
		.IF	__65C02__
OP_BBR		.EQU	$08
OP_BBS		.EQU	$0A
		.ENDIF
OP_BCC		.EQU	$0C
OP_BCS		.EQU	$0E
OP_BEQ		.EQU	$10
OP_BIT		.EQU	$12
OP_BNE		.EQU	$14
OP_BMI		.EQU	$16
OP_BPL		.EQU	$18
		.IF	__65C02__
OP_BRA		.EQU	$1A
		.ENDIF
OP_BRK		.EQU	$1C
OP_BVC		.EQU	$1E
OP_BVS		.EQU	$20
OP_CLC		.EQU	$22
OP_CLD		.EQU	$24
OP_CLI		.EQU	$26
OP_CLV		.EQU	$28
OP_CMP		.EQU	$2A
OP_CPX		.EQU	$2C
OP_CPY		.EQU	$2E
OP_DEC		.EQU	$30
OP_DEX		.EQU	$32
OP_DEY		.EQU	$34
OP_EOR		.EQU	$36
OP_INC		.EQU	$38
OP_INX		.EQU	$3A
OP_INY		.EQU	$3C
OP_JMP		.EQU	$3E
OP_JSR		.EQU	$40
OP_LDA		.EQU	$42
OP_LDX		.EQU	$44
OP_LDY		.EQU	$46
OP_LSR		.EQU	$48
OP_NOP		.EQU	$4A
OP_ORA		.EQU	$4C
OP_PHA		.EQU	$4E
OP_PHP		.EQU	$50
		.IF	__65C02__
OP_PHX		.EQU	$52
OP_PHY		.EQU	$54
		.ENDIF
OP_PLA		.EQU	$56
OP_PLP		.EQU	$58
		.IF	__65C02__
OP_PLX		.EQU	$5A
OP_PLY		.EQU	$5C
OP_RMB		.EQU	$5E
		.ENDIF
OP_ROL		.EQU	$60
OP_ROR		.EQU	$62
OP_RTI		.EQU	$64
OP_RTS		.EQU	$66
OP_SBC		.EQU	$68
OP_SEC		.EQU	$6A
OP_SED		.EQU	$6C
OP_SEI		.EQU	$6E
		.IF	__65C02__
OP_SMB		.EQU	$70
		.ENDIF
OP_STA		.EQU	$72
		.IF	__65C02__
OP_STP		.EQU	$74
		.ENDIF
OP_STX		.EQU	$76
OP_STY		.EQU	$78
		.IF	__65C02__
OP_STZ		.EQU	$7A
		.ENDIF
OP_TAX		.EQU	$7C
OP_TAY		.EQU	$7E
		.IF	__65C02__
OP_TRB		.EQU	$80
OP_TSB		.EQU	$82
		.ENDIF
OP_TSX		.EQU	$84
OP_TXA		.EQU	$86
OP_TXS		.EQU	$88
OP_TYA		.EQU	$8A
		.IF	__65C02__
OP_WAI		.EQU	$8C
		.ENDIF

; Bit patterns for addressing modes

MB_IND		.EQU	%10000000
MB_REL		.EQU	%01000000
MB_BIT		.EQU	%00100000

MB_IMP		.EQU	%00000000
MB_ZPG		.EQU	%00000001
MB_IMM		.EQU	%00000010
MB_ABS		.EQU	%00000011

MB_ACC		.EQU	%00010000
MB_XRG		.EQU	%00001000
MB_YRG		.EQU	%00000100

; Addressing modes

		.IF	__65C02__
MO_BIT		.EQU	MB_BIT	     |MB_ZPG
MO_BRL		.EQU	MB_BIT|MB_REL|MB_ZPG
		.ENDIF
MO_ACC		.EQU	       MB_ACC|MB_IMP
MO_IMP		.EQU		      MB_IMP
MO_IMM		.EQU		      MB_IMM
MO_REL		.EQU		      MB_REL
MO_ZPG		.EQU		      MB_ZPG
MO_ZPX		.EQU	       MB_YRG|MB_ZPG
MO_ZPY		.EQU	       MB_YRG|MB_ZPG
		.IF	__65C02__
MO_IZP		.EQU	MB_IND	     |MB_ZPG
		.ENDIF
MO_IZX		.EQU	MB_IND|MB_XRG|MB_ZPG
MO_IZY		.EQU	MB_IND|MB_YRG|MB_ZPG
MO_ABS		.EQU		      MB_ABS
MO_ABX		.EQU	       MB_XRG|MB_ABS
MO_ABY		.EQU	       MB_YRG|MB_ABS
MO_IAB		.EQU	MB_IND	     |MB_ABS
		.IF	__65C02__
MO_IAX		.EQU	MB_IND|MB_XRG|MB_ABS
		.ENDIF

;===============================================================================
; Data Areas
;-------------------------------------------------------------------------------

		.PAGE0
		.ORG	$00F0

A_REG		.SPACE	1
X_REG		.SPACE	1
Y_REG		.SPACE	1
P_REG		.SPACE	1
PC_REG		.SPACE	2

CMD_LEN		.SPACE	1			; Command buffer length
ADDR_S		.SPACE	2
ADDR_E		.SPACE	2

TEMP		.SPACE	2
COUNT		.SPACE	1

;-------------------------------------------------------------------------------

		.ORG	$00FE

IO_TEMP		.SPACE	1

FLAGS		.SPACE	1		; I/O Flags for XON/XOFF

FLAG_STOPPED	.EQU	$80
FLAG_STOP	.EQU	$40

;-------------------------------------------------------------------------------

		.ORG	$0100

STACK		.SPACE	256

;===============================================================================
; UART Buffers
;-------------------------------------------------------------------------------

RX_SIZE		.EQU	62
TX_SIZE		.EQU	62

CMD_SIZE	.EQU	128

		.BSS
		.ORG	$0200

; Communications buffer offsets

RX_HEAD		.SPACE	1		; UART recieve buffer offsets
RX_TAIL		.SPACE	1
TX_HEAD		.SPACE	1		; UART transmit buffer offsets
TX_TAIL		.SPACE	1

RX_BUFF		.SPACE	RX_SIZE		; UART recieve buffer
TX_BUFF		.SPACE	TX_SIZE		; UART transmit buffer

BUFFER		.SPACE	CMD_SIZE



		.CODE
		.ORG	$F000

;===============================================================================
;-------------------------------------------------------------------------------

		.IF	__6502__
OPCODES:
		.BYTE	OP_BRK,OP_ORA,OP_ERR,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR ; 0
		.BYTE	OP_PHP,OP_ORA,OP_ASL,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR
		.BYTE	OP_BPL,OP_ORA,OP_ERR,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR ; 1
		.BYTE	OP_CLC,OP_ORA,OP_INC,OP_ERR,OP_ERR,OP_ORA,OP_ASL,OP_ERR
		.BYTE	OP_JSR,OP_AND,OP_ERR,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR ; 2
		.BYTE	OP_PLP,OP_AND,OP_ROL,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR
		.BYTE	OP_BMI,OP_AND,OP_ERR,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR ; 3
		.BYTE	OP_SEC,OP_AND,OP_DEC,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_ERR
		.BYTE	OP_RTI,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_ERR ; 4
		.BYTE	OP_PHA,OP_EOR,OP_LSR,OP_ERR,OP_JMP,OP_EOR,OP_LSR,OP_ERR
		.BYTE	OP_BVC,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_ERR ; 5
		.BYTE	OP_CLI,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_ERR
		.BYTE	OP_RTS,OP_ADC,OP_ERR,OP_ERR,OP_ERR,OP_ADC,OP_ROR,OP_ERR ; 6
		.BYTE	OP_PLA,OP_ADC,OP_ROR,OP_ERR,OP_JMP,OP_ADC,OP_ROR,OP_ERR
		.BYTE	OP_BVS,OP_ADC,OP_ERR,OP_ERR,OP_ERR,OP_ADC,OP_ROR,OP_ERR ; 7
		.BYTE	OP_SEI,OP_ADC,OP_ERR,OP_ERR,OP_ERR,OP_ADC,OP_ROR,OP_ERR
		.BYTE	OP_ERR,OP_STA,OP_ERR,OP_ERR,OP_STY,OP_STA,OP_STX,OP_ERR ; 8
		.BYTE	OP_DEY,OP_BIT,OP_TXA,OP_ERR,OP_STY,OP_STA,OP_STX,OP_ERR
		.BYTE	OP_BCC,OP_STA,OP_ERR,OP_ERR,OP_STY,OP_STA,OP_STX,OP_ERR ; 9
		.BYTE	OP_TYA,OP_STA,OP_TXS,OP_ERR,OP_ERR,OP_STA,OP_ERR,OP_ERR
		.BYTE	OP_LDY,OP_LDA,OP_LDX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR ; A
		.BYTE	OP_TAY,OP_LDA,OP_TAX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR
		.BYTE	OP_BCS,OP_LDA,OP_ERR,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR ; B
		.BYTE	OP_CLV,OP_LDA,OP_TSX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_ERR
		.BYTE	OP_CPY,OP_CMP,OP_ERR,OP_ERR,OP_CPY,OP_CMP,OP_DEC,OP_ERR ; C
		.BYTE	OP_INY,OP_CMP,OP_DEX,OP_ERR,OP_CPY,OP_CMP,OP_DEC,OP_ERR
		.BYTE	OP_BNE,OP_CMP,OP_ERR,OP_ERR,OP_ERR,OP_CMP,OP_DEC,OP_ERR ; D
		.BYTE	OP_CLD,OP_CMP,OP_ERR,OP_ERR,OP_ERR,OP_CMP,OP_DEC,OP_ERR
		.BYTE	OP_CPX,OP_SBC,OP_ERR,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_ERR ; E
		.BYTE	OP_INX,OP_SBC,OP_NOP,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_ERR
		.BYTE	OP_BEQ,OP_SBC,OP_ERR,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_ERR ; F
		.BYTE	OP_SED,OP_SBC,OP_ERR,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_ERR

MODES:
		.BYTE	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_IMP ; 0
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; 1
		.BYTE	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.BYTE	MO_ABS,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; 2
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPX,MO_IMP ; 3
		.BYTE	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABX,MO_ABX,MO_ABX,MO_IMP
		.BYTE	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_IMP ; 4
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; 5
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.BYTE	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_IMP ; 6
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_IAB,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; 7
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.BYTE	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; 8
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_IMP ; 9
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_IMP,MO_IMP
		.BYTE	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; A
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_IMP ; B
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_ABY,MO_IMP
		.BYTE	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; C
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; D
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.BYTE	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_IMP ; E
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_IMP
		.BYTE	MO_REL,MO_IZY,MO_IMP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_IMP ; F
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_IMP
		.ENDIF

;-------------------------------------------------------------------------------

		.IF	__65C02__
OPCODES:
		.BYTE	OP_BRK,OP_ORA,OP_ERR,OP_ERR,OP_TSB,OP_ORA,OP_ASL,OP_RMB ; 0
		.BYTE	OP_PHP,OP_ORA,OP_ASL,OP_ERR,OP_TSB,OP_ORA,OP_ASL,OP_BBR
		.BYTE	OP_BPL,OP_ORA,OP_ORA,OP_ERR,OP_TRB,OP_ORA,OP_ASL,OP_RMB ; 1
		.BYTE	OP_CLC,OP_ORA,OP_INC,OP_ERR,OP_TRB,OP_ORA,OP_ASL,OP_BBR
		.BYTE	OP_JSR,OP_AND,OP_ERR,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_RMB ; 2
		.BYTE	OP_PLP,OP_AND,OP_ROL,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_BBR
		.BYTE	OP_BMI,OP_AND,OP_AND,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_RMB ; 3
		.BYTE	OP_SEC,OP_AND,OP_DEC,OP_ERR,OP_BIT,OP_AND,OP_ROL,OP_BBR
		.BYTE	OP_RTI,OP_EOR,OP_ERR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_RMB ; 4
		.BYTE	OP_PHA,OP_EOR,OP_LSR,OP_ERR,OP_JMP,OP_EOR,OP_LSR,OP_BBR
		.BYTE	OP_BVC,OP_EOR,OP_EOR,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_RMB ; 5
		.BYTE	OP_CLI,OP_EOR,OP_PHY,OP_ERR,OP_ERR,OP_EOR,OP_LSR,OP_BBR
		.BYTE	OP_RTS,OP_ADC,OP_ERR,OP_ERR,OP_STZ,OP_ADC,OP_ROR,OP_RMB ; 6
		.BYTE	OP_PLA,OP_ADC,OP_ROR,OP_ERR,OP_JMP,OP_ADC,OP_ROR,OP_BBR
		.BYTE	OP_BVS,OP_ADC,OP_ADC,OP_ERR,OP_STZ,OP_ADC,OP_ROR,OP_RMB ; 7
		.BYTE	OP_SEI,OP_ADC,OP_PLY,OP_ERR,OP_JMP,OP_ADC,OP_ROR,OP_BBR
		.BYTE	OP_BRA,OP_STA,OP_ERR,OP_ERR,OP_STY,OP_STA,OP_STX,OP_SMB ; 8
		.BYTE	OP_DEY,OP_BIT,OP_TXA,OP_ERR,OP_STY,OP_STA,OP_STX,OP_BBS
		.BYTE	OP_BCC,OP_STA,OP_STA,OP_ERR,OP_STY,OP_STA,OP_STX,OP_SMB ; 9
		.BYTE	OP_TYA,OP_STA,OP_TXS,OP_ERR,OP_STZ,OP_STA,OP_STZ,OP_BBS
		.BYTE	OP_LDY,OP_LDA,OP_LDX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_SMB ; A
		.BYTE	OP_TAY,OP_LDA,OP_TAX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_BBS
		.BYTE	OP_BCS,OP_LDA,OP_LDA,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_SMB ; B
		.BYTE	OP_CLV,OP_LDA,OP_TSX,OP_ERR,OP_LDY,OP_LDA,OP_LDX,OP_BBS
		.BYTE	OP_CPY,OP_CMP,OP_ERR,OP_ERR,OP_CPY,OP_CMP,OP_DEC,OP_SMB ; C
		.BYTE	OP_INY,OP_CMP,OP_DEX,OP_WAI,OP_CPY,OP_CMP,OP_DEC,OP_BBS
		.BYTE	OP_BNE,OP_CMP,OP_CMP,OP_ERR,OP_ERR,OP_CMP,OP_DEC,OP_SMB ; D
		.BYTE	OP_CLD,OP_CMP,OP_PHX,OP_STP,OP_ERR,OP_CMP,OP_DEC,OP_BBS
		.BYTE	OP_CPX,OP_SBC,OP_ERR,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_SMB ; E
		.BYTE	OP_INX,OP_SBC,OP_NOP,OP_ERR,OP_CPX,OP_SBC,OP_INC,OP_BBS
		.BYTE	OP_BEQ,OP_SBC,OP_SBC,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_SMB ; F
		.BYTE	OP_SED,OP_SBC,OP_PLX,OP_ERR,OP_ERR,OP_SBC,OP_INC,OP_BBS

MODES:
		.BYTE	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 0
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPG,MO_ZPX,MO_ZPX,MO_ZPG ; 1
		.BYTE	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABS,MO_ABX,MO_ABX,MO_BRL
		.BYTE	MO_ABS,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 2
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPX,MO_ZPG ; 3
		.BYTE	MO_IMP,MO_ABY,MO_ACC,MO_IMP,MO_ABX,MO_ABX,MO_ABX,MO_BRL
		.BYTE	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG ; 4
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPG ; 5
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_BRL
		.BYTE	MO_IMP,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 6
		.BYTE	MO_IMP,MO_IMM,MO_ACC,MO_IMP,MO_IAB,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPX,MO_ZPG ; 7
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IAX,MO_ABX,MO_ABX,MO_BRL
		.BYTE	MO_REL,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; 8
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_ZPG ; 9
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABS,MO_ABX,MO_ABX,MO_BRL
		.BYTE	MO_IMM,MO_IZX,MO_IMM,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; A
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPY,MO_ZPG ; B
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_ABY,MO_BRL
		.BYTE	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; C
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPG ; D
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_BRL
		.BYTE	MO_IMM,MO_IZX,MO_IMP,MO_IMP,MO_ZPG,MO_ZPG,MO_ZPG,MO_ZPG ; E
		.BYTE	MO_IMP,MO_IMM,MO_IMP,MO_IMP,MO_ABS,MO_ABS,MO_ABS,MO_BRL
		.BYTE	MO_REL,MO_IZY,MO_IZP,MO_IMP,MO_IMP,MO_ZPX,MO_ZPX,MO_ZPG ; F
		.BYTE	MO_IMP,MO_ABY,MO_IMP,MO_IMP,MO_IMP,MO_ABX,MO_ABX,MO_BRL
		.ENDIF

;-------------------------------------------------------------------------------

SQUEEZE		.MACRO	CH1,CH2,CH3
		.WORD	((((CH3 & $1F) << 5)|(CH2 & $1F)) << 5)|(CH1 & $1F)
		.ENDM

MNEMONICS:
		SQUEEZE '?','?','?'
		SQUEEZE 'A','D','C'
		SQUEEZE 'A','N','D'
		SQUEEZE 'A','S','L'
		SQUEEZE 'B','B','R'
		SQUEEZE 'B','B','S'
		SQUEEZE 'B','C','C'
		SQUEEZE 'B','C','S'
		SQUEEZE 'B','E','Q'
		SQUEEZE 'B','I','T'
		SQUEEZE 'B','N','E'
		SQUEEZE 'B','M','I'
		SQUEEZE 'B','P','L'
		SQUEEZE 'B','R','A'
		SQUEEZE 'B','R','K'
		SQUEEZE 'B','V','C'
		SQUEEZE 'B','V','S'
		SQUEEZE 'C','L','C'
		SQUEEZE 'C','L','D'
		SQUEEZE 'C','L','I'
		SQUEEZE 'C','L','V'
		SQUEEZE 'C','M','P'
		SQUEEZE 'C','P','X'
		SQUEEZE 'C','P','Y'
		SQUEEZE 'D','E','C'
		SQUEEZE 'D','E','X'
		SQUEEZE 'D','E','Y'
		SQUEEZE 'E','O','R'
		SQUEEZE 'I','N','C'
		SQUEEZE 'I','N','X'
		SQUEEZE 'I','N','Y'
		SQUEEZE 'J','M','P'
		SQUEEZE 'J','S','R'
		SQUEEZE 'L','D','A'
		SQUEEZE 'L','D','X'
		SQUEEZE 'L','D','Y'
		SQUEEZE 'L','S','R'
		SQUEEZE 'N','O','P'
		SQUEEZE 'O','R','A'
		SQUEEZE 'P','H','A'
		SQUEEZE 'P','H','P'
		SQUEEZE 'P','H','X'
		SQUEEZE 'P','H','Y'
		SQUEEZE 'P','L','A'
		SQUEEZE 'P','L','P'
		SQUEEZE 'P','L','X'
		SQUEEZE 'P','L','Y'
		SQUEEZE 'R','M','B'
		SQUEEZE 'R','O','L'
		SQUEEZE 'R','O','R'
		SQUEEZE 'R','T','I'
		SQUEEZE 'R','T','S'
		SQUEEZE 'S','B','C'
		SQUEEZE 'S','E','C'
		SQUEEZE 'S','E','D'
		SQUEEZE 'S','E','I'
		SQUEEZE 'S','M','B'
		SQUEEZE 'S','T','A'
		SQUEEZE 'S','T','P'
		SQUEEZE 'S','T','X'
		SQUEEZE 'S','T','Y'
		SQUEEZE 'S','T','Z'
		SQUEEZE 'T','A','X'
		SQUEEZE 'T','A','Y'
		SQUEEZE 'T','R','B'
		SQUEEZE 'T','S','B'
		SQUEEZE 'T','S','X'
		SQUEEZE 'T','X','A'
		SQUEEZE 'T','X','S'
		SQUEEZE 'T','Y','A'
		SQUEEZE 'W','A','I'

;===============================================================================
;
;-------------------------------------------------------------------------------

BREAK:
		CLI			; Allow interrupts
		PLA
		STA	Y_REG
		PLA
		STA	X_REG
		PLA
		STA	A_REG
		PLA
		STA	P_REG
		SEC
		PLA
		SBC	#2
		STA	PC_REG+0
		PLA
		SBC	#0
		STA	PC_REG+1

REGISTERS:
		JSR	CRLF
		LDX	#PC_STR
		JSR	SHOW_STR
		LDA	PC_REG+1
		JSR	HEX2
		LDA	PC_REG+0
		JSR	HEX2

		LDX	#A_STR
		JSR	SHOW_STR
		LDA	A_REG
		JSR	HEX2

		LDX	#X_STR
		JSR	SHOW_STR
		LDA	X_REG
		JSR	HEX2

		LDX	#Y_STR
		JSR	SHOW_STR
		LDA	Y_REG
		JSR	HEX2
		
		LDX	#P_STR
		JSR	SHOW_STR
		LDX	#7
		REPEAT
		 LDY	#'.'
		 LDA	BITS,X
		 BIT	P_REG
		 IF	NE
		  LDY	FLAG,X
		 ENDIF
		 TYA
		 JSR	UART_TX
		 DEX
		UNTIL MI		  
		
		LDX	#SP_STR
		JSR	SHOW_STR
		TSX
		TXA
		JSR	HEX2

COMMAND:
		.IF	__65C02__
		STZ	CMD_LEN		; Mark the buffer as empty
		.ELSE
		LDA	#0		; Mark the buffer as empty
		STA	CMD_LEN
		.ENDIF

PROMPT:
		JSR	CRLF		; Move cursor to next line
		LDA	#'.'		; And output the prompt
		JSR	UART_TX

		LDX	#0
		REPEAT
		 CPX	CMD_LEN		; Any saved characters to display?
		 BREAK	EQ		; No
		 LDA	BUFFER,X	; Yes, print from the buffer
		 JSR	UART_TX
		 INX
		FOREVER
		
		JSR	XON
		REPEAT
		 JSR	UART_RX		; Wait for some user input
		 
		 CMP 	#ESC		; Cancel input?
		 IF	EQ
		  BEQ	COMMAND		; Yes
		 ENDIF

		 CMP	#BS		; Backspace?
		 IF	EQ
BACKSPACE:	  CPX	#0		; Anything in the buffer?
		  IF	NE
		   PHA			; Erase the last character
		   JSR	UART_TX
		   JSR	SPACE
		   PLA
		   JSR	UART_TX
		   DEX
		  ENDIF
		  CONTINUE
		 ENDIF

		 CMP	#CR		; End of command entry?
		 BREAK	EQ		; Yes

		 CMP	#DEL		; Convert DEL into BS
		 IF	EQ
		  LDA	#BS
		  BNE	BACKSPACE
		 ENDIF
		 IF	CS		; In the range $7F-$FF?
SQUAWK:		  LDA	#BEL		; Yes, squawk!
		  JSR	UART_TX
		  CONTINUE
		 ENDIF

		 CMP	#' '		; In the range $00-$1F?
		 BCC	SQUAWK		; Yes, squawk!

		 CPX	#CMD_SIZE-1	; Command buffer full?
		 BCS	SQUAWK		; Yes, squawk!

		 STA	BUFFER,X	; Save the character
		 INX			; Bump the count
		 JSR	UART_TX		; And echo to terminal
		FOREVER
		JSR	XOFF

		STX	CMD_LEN		; Save the command length
		LDX	#0		; Set character offset to start
		JSR	SKIP_CHAR	; And get first character
		BCS	COMMAND

;===============================================================================
; 'A' - Assemble
;-------------------------------------------------------------------------------

		CMP	#'A'
		IF	EQ
		ENDIF

;===============================================================================
; 'D' - Disassemble Memory
;-------------------------------------------------------------------------------

		CMP	#'D'
		IF	EQ
		 JSR	GET_WORD
		 IF	CC
		  JSR	SET_ADDR_S
		  JSR	SET_ADDR_E
		  JSR	GET_WORD
		  IF	CC
		   JSR	SET_ADDR_E
		  ELSE
		   INC	ADDR_E+1
		  ENDIF
		  
		  REPEAT
		   JSR	CRLF		; Print the memory address
		   LDA	ADDR_S+1
		   JSR	HEX2
		   LDA	ADDR_S+0
		   JSR	HEX2
		   
		   JSR	DISASSEMBLE
		   JSR	BUMP_ADDR		   
		   JSR	CHECK_END
		  UNTIL	PL
		  JMP	COMMAND
		 ENDIF
		 JMP	ERROR		
		ENDIF
		
;===============================================================================
; 'G' - Go
;-------------------------------------------------------------------------------

		CMP	#'G'
		IF	EQ

		 LDA	PC_REG+1	; Push the target address
		 PHA
		 LDA	PC_REG+0
		 PHA
		 LDA	P_REG		; And status flags
		 PHA
		 LDA	A_REG		; Reload A, X and Y
		 LDX	X_REG
		 LDY	Y_REG
		 RTI			; Then go to code
		ENDIF

;===============================================================================
; 'M' - Show Memory
;-------------------------------------------------------------------------------		

		CMP	#'M'
		IF	EQ
		 JSR	GET_WORD
		 IF	CC
		  JSR	SET_ADDR_S
		  JSR	SET_ADDR_E
		  JSR	GET_WORD
		  IF	CC
		   JSR	SET_ADDR_E
		  ELSE
		   INC	ADDR_E+1
		  ENDIF
		  
		  REPEAT
		   JSR	CRLF		; Print the memory address
		   LDA	ADDR_S+1
		   JSR	HEX2
		   LDA	ADDR_S+0
		   JSR	HEX2
		   
		   LDY	#0		; Dump 16 bytes of data
		   REPEAT
		    JSR	SPACE
		    LDA	(ADDR_S),Y
		    INY
		    JSR	HEX2
		    CPY #16
		   UNTIL EQ
		   
		   JSR	SPACE		; Then show as characters
		   JSR	BAR
		   LDY	#0
		   REPEAT
		    LDA	(ADDR_S),Y
		    INY
		    JSR	IS_PRINTABLE
		    IF CC
		     LDA #'.'
		    ENDIF
		    JSR	UART_TX
		    CPY	#16
		   UNTIL EQ
		   JSR	BAR
		   
		   TYA
		   JSR	BUMP_ADDR
		   JSR	CHECK_END
		  UNTIL	PL
		  JMP	COMMAND
		 ENDIF
		 JMP	ERROR
		ENDIF

;===============================================================================
; 'R' - Show Registers
;-------------------------------------------------------------------------------

		CMP	#'R'
		IF	EQ
		 JMP	REGISTERS
		ENDIF
		
;===============================================================================
; 'S' - Load SREC
;-------------------------------------------------------------------------------

		CMP	#'S'
		IF	EQ
		ENDIF
		
;===============================================================================
; 'T' - Trace
;-------------------------------------------------------------------------------

		CMP	#'T'
		IF	EQ
		ENDIF
		
;===============================================================================
; 'W' - Write Memory
;-------------------------------------------------------------------------------

		CMP	#'W'
		IF	EQ
		 JSR	GET_WORD	; Get the target address
		 IF	CC
		  JSR	SET_ADDR_S	; Copy to start address
		  JSR	GET_BYTE	; Get the value
		  IF	CC
		   LDY	#0		; Write to  memory
		   LDA	TEMP+0
		   STA	(ADDR_S),Y
		   LDA	#1		; Increment address
		   JSR	BUMP_ADDR
		   LDA	#'W'		; Create prompt for next byte
		   JMP	SET_PROMPT
		  ENDIF
		 ENDIF
		 JMP	ERROR		; Handle syntax errors
		ENDIF
		
;===============================================================================
; '?' - Display Help
;-------------------------------------------------------------------------------

		CMP	#'?'
		IF	EQ
		 LDX	#HLP_STR
		ELSE
ERROR:		 LDX	#ERR_STR
		ENDIF
		JSR	SHOW_STR
		JMP	COMMAND

;===============================================================================
;-------------------------------------------------------------------------------

SET_ADDR_S:
		LDA	TEMP+0
		STA	ADDR_S+0
		LDA	TEMP+1
		STA	ADDR_S+1
		RTS

SET_ADDR_E:
		LDA	TEMP+0
		STA	ADDR_E+0
		LDA	TEMP+1
		STA	ADDR_E+1
		RTS

BUMP_ADDR:
		CLC
		ADC	ADDR_S+0
		STA	ADDR_S+0
		IF	CS
		 INC	ADDR_S+1
		ENDIF
		RTS
		
CHECK_END:
		SEC
		LDA	ADDR_S+0
		SBC	ADDR_E+0
		LDA	ADDR_S+1
		SBC	ADDR_E+1
		RTS

; Create a prompt string in the command buffer for the command in A using the
; current value of the starting address.

SET_PROMPT:
		LDX	#0		; Clear buffer and add command letter
		JSR	APPEND_CHAR
		LDA	#' '		; Then a space
		JSR	APPEND_CHAR
		
		LDA	ADDR_S+1	; Followed by the address
		JSR	APPEND_HEX2
		LDA	ADDR_S+0
		JSR	APPEND_HEX2
		LDA	#' '		; And another space
		JSR	APPEND_CHAR
		JMP	PROMPT		; Then output it

; Convert the byte in A into hexadecimal digits and append to the command buffer.		
		
APPEND_HEX2:
		PHA
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		JSR	APPEND_HEX
		PLA
APPEND_HEX:
		JSR	TO_HEX

; Append the character in A to the command buffer to create the next prompt
; string.

APPEND_CHAR:
		STA	BUFFER,X
		INX
		STX	CMD_LEN
		RTS
				
;===============================================================================
; Parsing Utilities
;-------------------------------------------------------------------------------

; Get the next characater from the command buffer indicated by the X register
; and convert it to UPPER case. If the carry is set then the end of the buffer
; has been reached.

NEXT_CHAR:
		CPX	CMD_LEN		; Reached end of buffer
		IF	CS
		 RTS
		ENDIF
		LDA	BUFFER,X
		INX

; Convert the character in A to upper case.

TO_UPPER:
		CMP	#'a'
		IF	CS
		 CMP	#'z'+1
		 IF	CC
		  AND	#$5F
		 ENDIF
		ENDIF
		CLC
		RTS
		
SKIP_CHAR:
		REPEAT
		 JSR	NEXT_CHAR
		 IF 	CS
		  RTS
		 ENDIF
		 CMP	#' '
		UNTIL	NE
		CLC
		RTS

; Parse a word from the command buffer and store it at 0,Y. Return if the
; carry set if there is a syntax error.

GET_WORD:
		LDY	#4		; Set maximim number of nybbles
		BNE	GET_BYTE+2


; Parse a word from the command buffer and store it at 0,Y. Return if the
; carry set if there is a syntax error.

GET_BYTE:
		LDY	#2		; Set maximum number of nybble
		STY	COUNT
		
		.IF	__65C02__
		STZ	TEMP+0		; Clear conversion area
		STZ	TEMP+1
		.ELSE
		LDY	#0
		STY	TEMP+0		; Clear conversion area
		STY	TEMP+1
		.ENDIF

		JSR	SKIP_CHAR	; Fetch first character
		JSR	GET_NYBBLE	; And try to convert
		IF	CS
		 RTS			; Syntax error
		ENDIF
		REPEAT
		 ASL	TEMP+0		; Fold into the result
		 ROL	TEMP+1
		 ASL	TEMP+0
		 ROL	TEMP+1
		 ASL	TEMP+0
		 ROL	TEMP+1
		 ASL	TEMP+0
		 ROL	TEMP+1
		 ORA	TEMP+0
		 STA	TEMP+0
		 
		 DEC	COUNT		; Reach maximum length?
		 BREAK	EQ
		 
		 JSR	NEXT_CHAR	; Try for another nybble
		 JSR	GET_NYBBLE
		UNTIL CS		
		CLC			; Conversion sucessfull
		RTS

;
;

GET_NYBBLE:
		JSR	IS_HEX		; Got a hex digit?
		IF	CS
		 CMP	#'A'		; Handle letters
		 IF	CS
		  SBC	#7
		 ENDIF
		 AND	#$0F		; Skip out nybble
		 CLC			; Done
		 RTS
		ENDIF
		SEC			; Set carry -- not hex
		RTS

; Return with the carry set of the character in A is a digit or 'A' thru 'F'.

IS_HEX:
		CMP	#'9'+1
		IF 	CC
		 CMP	#'0'
		 RTS
		ENDIF
		CMP	#'F'+1
		IF	CC
		 CMP	#'A'
		 RTS
		ENDIF
		CLC
		RTS
		
IS_PRINTABLE:
		CMP	#' '
		IF	CS
		 CMP	#DEL
		 IF	CC
		  SEC
		  RTS
		 ENDIF
		ENDIF
		CLC
		RTS
		
;===============================================================================
;-------------------------------------------------------------------------------

DISASSEMBLE:
		JSR	SPACE
		LDY	#0		; Fetch the opcode
		LDA	(ADDR_S),Y
		TAX
		JSR	HEX2		; And display it
		
		JSR	SPACE
		LDA	MODES,X
		PHA
		PHA
		AND	#MB_REL|MB_ABS
		IF	NE
		 LDY	#1
		 LDA	(ADDR_S),Y
		 JSR	HEX2
		ELSE
		 JSR	SPACE2
		ENDIF
		
		JSR	SPACE
		PLA
		TAY
		AND	#MB_REL|MB_ZPG
		CMP	#MB_REL|MB_ZPG
		IF	NE
		 TYA
		 AND	#MB_ABS
		 CMP	#MB_ABS
		ENDIF
		IF	EQ
		 LDY	#2
		 LDA	(ADDR_S),Y
		 JSR	HEX2
		ELSE
		 JSR	SPACE2
		ENDIF
		
		INY			; Save the byte count
		TYA
		PHA

		JSR	SPACE
		LDY	#0		; Fetch the opcode
		LDA	(ADDR_S),Y
		TAX
		LDA	OPCODES,X
		TAX
		LDA	MNEMONICS+1,X
		STA	TEMP
		LDA	MNEMONICS+0,X
		JSR	EXTRACT_LETTER
		JSR	EXTRACT_LETTER
		JSR	EXTRACT_LETTER
		JSR	SPACE
		
		PLA
		PHA
		AND	#MB_BIT
		IF	NE
		 LDY	#0
		 LDA	(ADDR_S),Y
		 AND	#7
		 ORA	#'0'
		 JSR	UART_TX
		 LDA	#','
		 JSR	UART_TX
		ENDIF
		
		PLA
		PHA
		IF	MI
		 LDA	#'('
		 JSR	UART_TX
		ENDIF
		
		PLA
		PHA
		AND	#MB_ABS
		IF	NE
		 PHA
		 CMP	#MB_IMM
		 IF	EQ
		  LDA	#'#'
		  JSR	UART_TX
		 ENDIF
		 LDA	#'$'
		 JSR	UART_TX
		 PLA
		 CMP	#MB_ABS
		 IF	EQ
		  LDY	#2
		  LDA	(ADDR_S),Y
		  JSR	HEX2
		 ENDIF
		 LDY	#1
		 LDA	(ADDR_S),Y
		 JSR	HEX2
		ENDIF
		
		PLA
		PHA
		AND	#MB_BIT|MB_REL
		CMP	#MB_BIT|MB_REL
		IF	EQ
		 LDA	#','
		 JSR	UART_TX
		ENDIF
		
		PLA
		PHA
		TAY
		AND	#MB_REL
		IF	NE
		 LDA	#'$'
		 JSR	UART_TX
		 TYA
		 LDY	#1
		 AND	#MB_BIT
		 IF	NE
		  INY
		 ENDIF
		 
		 LDA	#'r'
		 JSR	UART_RX
		ENDIF
		
		PLA
		PHA
		AND	#MB_ACC
		IF 	NE
		 LDA	#'A'
		 JSR	UART_TX
		ENDIF
		
		PLA
		PHA
		AND	#MB_XRG
		IF 	NE
		 LDA	#','
		 JSR	UART_TX
		 LDA	#'X'
		 JSR	UART_TX
		ENDIF
		
		PLA
		PHA
		IF	MI
		 LDA	#')'
		 JSR	UART_TX
		ENDIF
		
		PLA
		AND	#MB_YRG
		IF 	NE
		 LDA	#','
		 JSR	UART_TX
		 LDA	#'Y'
		 JSR	UART_TX
		ENDIF
		
		PLA			; Return the number of bytes
		RTS
		
EXTRACT_LETTER:
		PHA
		AND	#$1F
		ORA	#'@'
		JSR	UART_TX
		PLA
		LSR	TEMP
		ROR	A
		LSR	TEMP
		ROR	A
		LSR	TEMP
		ROR	A
		LSR	TEMP
		ROR	A
		LSR	TEMP
		ROR	A
		RTS
		
		
;===============================================================================
; Display Utilities
;-------------------------------------------------------------------------------

; Display the byte in A as two hexadecimal digits. The values in A & Y are
; destroyed.

HEX2:
		PHA			; Save a copy of the value
		LSR	A		; Shift down the hi nybble
		LSR	A
		LSR	A
		LSR	A
		JSR	HEX		; Convert and display
		PLA			; Pull back value and ...

; Display the lo nybble of A as a hexadecimal digit. The values in A & Y are
; destroyed.

HEX		JSR	TO_HEX		; Convert to printable character
		JMP	UART_TX		; And display.
		
;
		
TO_HEX		AND	#$0F		; Isolate the lo nybble
		SED			; Converted to ASCII
		CLC
		ADC	#$90
		ADC	#$40
		CLD
		RTS

; Output two spaces.

SPACE2:
		JSR	SPACE
		
; Output a single space. The values in A & Y are destroyed.

SPACE:
		LDA	#' '
		JMP	UART_TX
		
BAR:
		LDA	#'|'
		JMP	UART_TX

; Output a CR/LF control sequence to move the display cursor to the start of
; the next line. A & Y are destroyed.

CRLF:
		LDA	#CR		; Output a carriage return
		JSR	UART_TX
		LDA	#LF		; .. followed by a new line
		JMP	UART_TX

;-------------------------------------------------------------------------------


SHOW_STR:
		REPEAT
		 LDA	STRINGS,X
		 IF	EQ
		  RTS
		 ENDIF
		 JSR	UART_TX
		 INX
		FOREVER

STRINGS:
PC_STR		.EQU	.-STRINGS
		.BYTE	"PC=",0
SP_STR		.EQU	.-STRINGS
		.BYTE	" SP=",0
P_STR		.EQU	.-STRINGS
		.BYTE	" P=",0
A_STR		.EQU	.-STRINGS
		.BYTE	" A=",0
X_STR		.EQU	.-STRINGS
		.BYTE	" X=",0
Y_STR		.EQU	.-STRINGS
		.BYTE	" Y=",0
ERR_STR		.EQU	.-STRINGS
		.BYTE	CR,LF,"?",0
HLP_STR		.EQU	.-STRINGS
		.BYTE	CR,LF,"A xxxx opcode [args]\tAssemble"
		.BYTE 	CR,LF,"D xxxx yyyy\t\tDisassemble"
		.BYTE	CR,LF,"G [xxxx]\t\tGoto"
		.BYTE	CR,LF,"M xxxx yyyy\t\tDisplay Memory"
		.BYTE	CR,LF,"R\t\t\tDisplay Registers"
		.BYTE	CR,LF,"S...\t\t\tS19 Load"
		.BYTE	CR,LF,"T [xxxx]\t\tTrace"
		.BYTE	CR,LF,"W xxxx yy\t\tWrite Memory"
		.BYTE 	0
		
FLAG		.BYTE	"CZID11VN"
BITS		.BYTE	$01,$02,$04,$08,$10,$20,$40,$80
		
;==============================================================================
; I/O Page
;-------------------------------------------------------------------------------

		.ORG	$FE00
		.SPACE	256

;===============================================================================
; Reset Handler
;-------------------------------------------------------------------------------

		.ORG	$FF00

RESET:
		CLD			; Ensure binary mode
		LDX	#$FF		; Reset the stack
		TXS

		INX			; Clear buffer offsets
		STX	RX_HEAD
		STX	RX_TAIL
		STX	TX_HEAD
		STX	TX_TAIL
;		STX	FLAGS		; And flow control flags

                lda     #%00011111	; 8 bits, 1 stop bit, 19200 baud
                sta     ACIA_CTRL
                lda     #%11001001	; No parity, no interrupt
                sta     ACIA_CMND
                lda     ACIA_DATA	; Clear receive buffer

		CLI			; Allow interrupts
		BRK

;===============================================================================
; UART Interface
;-------------------------------------------------------------------------------

XON:
		LDA	#DC1
		BNE	UART_TX

XOFF:
		LDA	#DC3

; Inserts the byte in A into the transmit buffer. If the buffer is full then
; wait until some space is available. Registers are preserved.

UART_TX:
		PHA
		STY	IO_TEMP
		
		LDY	TX_TAIL		; Save the data byte at the tail
		STA	TX_BUFF,Y
		JSR	BUMP_TX		; Work out the next offset
		REPEAT			; And wait until save to store
		 CPY	TX_HEAD
		UNTIL	NE
		STY	TX_TAIL
		LDA	#$05		; Ensure TX interrupt enabled
		STA	ACIA_CMND
		
		LDY	IO_TEMP
		PLA
		RTS			; Done

;
;

UART_RX:
;		BIT	FLAGS		; Terminal stopped from sending?
;		IF	MI
;		 JSR	RX_COUNT	; Yes, check RX buffer count
;		 CMP	#RX_SIZE*1/10	; Restart when only 10% full
;		 IF	CC
;		  LDA	#DC1		; Send XON
;		  JSR	UART_TX
;		.IF	__65C02__
;		  STZ	FLAGS		; Mark as no longer stopped
;		.ELSE
;		  LDA	#0		; Mark as no longer stopped
;		  STA	FLAGS
;		.ENDIF
;		 ENDIF
;		ENDIF

		STY	IO_TEMP
		LDY	RX_HEAD		; Wait until there is some data
		REPEAT
		 CPY	RX_TAIL
		UNTIL	NE
		LDA	RX_BUFF,Y	; Then extract the head byte
		JSR	BUMP_RX		; Update the offset
		STY	RX_HEAD
		LDY	IO_TEMP
		RTS			; Done

;
;

		.IF	0
RX_COUNT:
		SEC
		LDA	RX_TAIL		; Subtract the two offsets
		SBC	RX_HEAD
		IF	CC
		 ADC	#RX_SIZE	; And correct if negative
		ENDIF
		RTS			; Done
		.ENDIF
		
;===============================================================================
; NMI Handler
;-------------------------------------------------------------------------------

NMI:
		JMP	BREAK

;===============================================================================
; IRQ Handler
;-------------------------------------------------------------------------------

; Handle interrupts, currently just UART transmit buffer empty and receive
; buffer full.

IRQ:
		PHA			; Save users registers
		.IF	__65C02__
		PHX
		PHY
		.ELSE
		TXA
		PHA
		TYA
		PHA
		CLD
		.ENDIF

		TSX			; Check for BRK
		LDA	STACK+4,X
		AND	#$10
		BNE	NMI		; Enter via NMI handler

;-------------------------------------------------------------------------------

		LDA	ACIA_STAT	; ACIA is the source?
		BPL	NOT_ACIA	; No.

		PHA
		AND	#$10		; TX Buffer empty?
		IF	NE
;		 BIT	FLAGS		; Do we need to send XOFF?
;		 IF	VS
;		  LDA	#DC3		; Yes, send XOFF to terminal
;		  STA	ACIA_DATA
;		  ASL	FLAGS		; And mark as sent
;		  BNE	NOT_ACIA
;		 ENDIF

		 LDY	TX_HEAD		; Any data to send?
		 CPY	TX_TAIL
		 IF	NE
		  LDA	TX_BUFF,Y	; Yes, extract and send it
		  STA	ACIA_DATA
		  JSR	BUMP_TX
		  STY	TX_HEAD
		 ELSE
		  LDA	#$01		; No, disable TX interrupt
		  STA	ACIA_CMND
		 ENDIF
		ENDIF

		PLA
		AND	#$08		; RX Buffer full?
		IF	NE
		 LDA	ACIA_DATA	; Yes, fetch the character
		 LDY	RX_TAIL		; .. and save it
		 STA	RX_BUFF,Y
		 JSR	BUMP_RX
		 CPY	RX_HEAD		; Is buffer completely full?
		 IF	NE
		  STY	RX_TAIL		; No, update tail offset
		 ENDIF

;		 BIT	FLAGS		; Already stopped or stopping?
;		 IF	PL
;		  IF	VC
;		   JSR	RX_COUNT	; Fetch FX buffer count
;		   CMP	#RX_SIZE*9/10	; More than 90%
;		   IF	CS
;		    LDA #FLAG_STOP	; Yes, set flags to send XOFF
;		    STA FLAGS
;		    LDA #$05		; Ensure transmit interrupt enabled
;		    STA ACIA_CMND
;		   ENDIF
;		  ENDIF
;		 ENDIF
		ENDIF
NOT_ACIA:

;-------------------------------------------------------------------------------

		.IF	__65C02__
		PLY			; Restore user registers
		PLX
		.ELSE
		PLA			; Restore user registers
		TAY
		PLA
		TAX
		.ENDIF
		PLA
		RTI			; Done

; Bump and wrap a recieve buffer index value.

BUMP_RX:
		.IF	RX_SIZE != TX_SIZE
		INY
		CPY	#RX_SIZE
		IF	EQ
		 LDY	#0
		ENDIF
		RTS
		.ENDIF

; Bump and wrap a transmit buffer index value.

BUMP_TX:
		INY
		CPY	#TX_SIZE
		IF	EQ
		 LDY	#0
		ENDIF
		RTS

;===============================================================================
; Vector Locations
;-------------------------------------------------------------------------------

		.ORG	$FFFA

		.WORD	NMI		; NMI
		.WORD	RESET		; RESET
		.WORD	IRQ		; IRQ/BRK

		.END