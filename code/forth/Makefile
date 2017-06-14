#===============================================================================
# Forth ROM Build Definitions
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
	forth.bin forth.hex ../../boot-6502.asm

LK65_FLAGS = \
	-bss $$0200-$$EFFF -code $$F000-$$FFFF
	
all:	$(TARGETS)

forth.bin: forth.obj
	$(LK65) $(LK65_FLAGS) -bin -output $@ forth.obj
	
forth.hex: forth.obj
	$(LK65) $(LK65_FLAGS) -hex -output $@ forth.obj
	
../../boot-6502.asm: forth.hex
	java -cp $(6502_JAR) uk.co.demon.obelisk.util.Hex2Mpasm < forth.hex > ../../boot-6502.asm

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
forth.obj: \
	forth.asm
