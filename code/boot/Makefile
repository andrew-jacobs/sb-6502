#===============================================================================
# Boot ROM Build Definitions
#-------------------------------------------------------------------------------
# Copyright (C)2017 HandCoded Software Ltd.
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
# 2017-02-04 AJ Initial version
#-------------------------------------------------------------------------------

include	../MakeDefns

#===============================================================================
# Targets
#-------------------------------------------------------------------------------
	
TARGETS	= \
	boot-6502.bin boot-6502.hex ../../boot-6502.asm \
	boot-65c02.bin boot-65c02.hex ../../boot-65c02.asm

LK65_FLAGS = \
	-bss $$0200-$$EFFF -code $$F000-$$FFFF
	
all:	$(TARGETS)

boot-6502.bin: boot-6502.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ boot-6502.obj
	
boot-65c02.bin: boot-65c02.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ boot-65c02.obj
	
boot-6502.hex: boot-6502.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ boot-6502.obj
	
boot-65c02.hex: boot-65c02.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ boot-65c02.obj
	
../../boot-6502.asm: boot-6502.hex
	java -cp $(6502_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < boot-6502.hex > ../../boot-6502.asm

../../boot-65c02.asm: boot-65c02.hex
	java -cp $(6502_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < boot-65c02.hex > ../../boot-65c02.asm

clean:
	$(RM) *.obj
	$(RM) *.hex
	$(RM) *.bin
	$(RM) *.lst
	$(RM) *.map
	$(RM) ../../boot-65*.asm
	
#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

boot-6502.obj: \
	boot-6502.asm boot.asm

boot-65c02.obj: \
	boot-65c02.asm boot.asm
