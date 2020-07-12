#===============================================================================
# Monitor Build Definitions
#-------------------------------------------------------------------------------
# Copyright (C)2016-2020 HandCoded Software Ltd.
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
#===============================================================================
# Revision History:
#
# 2014-06-07 AJ Initial version
#-------------------------------------------------------------------------------

DEV65_DIR = ..

include	../MakeDefns

#===============================================================================
# Targets
#-------------------------------------------------------------------------------

TARGETS	= \
	monitor-6502.bin monitor-6502.hex ../../boot-6502.asm \
	monitor-65c02.bin monitor-65c02.hex ../../boot-65c02.asm \
	monitor-65sc02.bin monitor-65sc02.hex ../../boot-65sc02.asm \
	monitor-65c802.bin monitor-65c802.hex ../../boot-65c802.asm

LK65_FLAGS = \
	-bss 0200-EFFF -code F000-FFFF

all:	$(TARGETS)

monitor-6502.bin: monitor-6502.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ monitor-6502.obj

monitor-65c02.bin: monitor-65c02.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ monitor-65c02.obj

monitor-65sc02.bin: monitor-65sc02.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ monitor-65sc02.obj

monitor-65c802.bin: monitor-65c802.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ monitor-65c802.obj

monitor-6502.hex: monitor-6502.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ monitor-6502.obj

monitor-65c02.hex: monitor-65c02.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ monitor-65c02.obj

monitor-65sc02.hex: monitor-65sc02.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ monitor-65sc02.obj

monitor-65c802.hex: monitor-65c802.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ monitor-65c802.obj

../../boot-6502.asm: monitor-6502.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < monitor-6502.hex > ../../boot-6502.asm

../../boot-65c02.asm: monitor-65c02.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < monitor-65c02.hex > ../../boot-65c02.asm

../../boot-65sc02.asm: monitor-65sc02.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < monitor-65sc02.hex > ../../boot-65sc02.asm

../../boot-65c802.asm: monitor-65c802.hex
	java -cp $(DEV65_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < monitor-65c802.hex > ../../boot-65c802.asm

clean:
	$(RM) *.obj
	$(RM) *.hex
	$(RM) *.bin
	$(RM) *.lst
	$(RM) *.map

#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

monitor-6502.obj: \
	monitor-6502.asm monitor.asm ..\sb-6502.inc

monitor-65c02.obj: \
	monitor-65c02.asm monitor.asm ..\sb-6502.inc

monitor-65sc02.obj: \
	monitor-65c02.asm monitor.asm ..\sb-6502.inc

monitor-65c802.obj: \
	monitor-65c802.asm ..\sb-6502.inc
