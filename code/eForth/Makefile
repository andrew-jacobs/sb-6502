#===============================================================================
# eForth Build Definitions
#-------------------------------------------------------------------------------
# Copyright (C),2017-2018 HandCoded Software Ltd.
# All rights reserved.
#
# This work is made available under the terms of the Creative Commons
# Attribution-NonCommercial 2.0 license. Open the following URL to see the
# details.
#
# http://creativecommons.org/licenses/by-nc/2.0/
#-------------------------------------------------------------------------------
#
# Notes:
#
#
#
#-------------------------------------------------------------------------------

DEV65_DIR = ..

include	../MakeDefns

#===============================================================================
# Targets
#-------------------------------------------------------------------------------

TARGETS	= \
	eforth-6502.bin eforth-6502.hex ../../boot-6502.asm \
	eforth-65c02.bin eforth-65c02.hex ../../boot-65c02.asm \
	eforth-65c802.bin eforth-65c802.hex ../../boot-65c802.asm

LK65_FLAGS = \
	-bss $$0200-$$EFFF -code $$E000-$$FFFF

all:	$(TARGETS)

eforth-6502.bin: eforth-6502.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ eforth-6502.obj

eforth-65c02.bin: eforth-65c02.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ eforth-65c02.obj

eforth-65c802.bin: eforth-65c802.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ eforth-65c802.obj

eforth-6502.hex: eforth-6502.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ eforth-6502.obj

eforth-65c02.hex: eforth-65c02.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ eforth-65c02.obj

eforth-65c802.hex: eforth-65c802.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ eforth-65c802.obj

../../boot-6502.asm: eforth-6502.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < eforth-6502.hex > ../../boot-6502.asm

../../boot-65c02.asm: eforth-65c02.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < eforth-65c02.hex > ../../boot-65c02.asm

../../boot-65c802.asm: eforth-65c802.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < eforth-65c802.hex > ../../boot-65c802.asm

clean:
	$(RM) *.obj
	$(RM) *.hex
	$(RM) *.bin
	$(RM) *.lst
	$(RM) *.map

#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

eforth-6502.obj: \
	eforth-6502.asm common.inc ..\sb-6502.inc

eforth-65c02.obj: \
	eforth-65c02.asm common.inc ..\sb-6502.inc

eforth-65c802.obj: \
	eforth-65c802.asm common.inc ..\sb-6502.inc
