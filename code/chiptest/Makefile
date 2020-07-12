#===============================================================================
# Chip Test Build Definitions
#-------------------------------------------------------------------------------
# Copyright (C)2020 HandCoded Software Ltd.
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
#-------------------------------------------------------------------------------

DEV65_DIR = ..

include	../MakeDefns

#===============================================================================
# Targets
#-------------------------------------------------------------------------------

TARGETS	= \
	chiptest.s19

LK65_FLAGS = \
	-bss $$0280-$$BFFF -code $$0300-$$0380

all:	$(TARGETS)

chiptest.s19: chiptest.obj
	$(LK65) $(LK65_FLAGS) -s19 -output $@ chiptest.obj

clean:
	$(RM) *.obj
	$(RM) *.s19
	$(RM) *.bin
	$(RM) *.lst
	$(RM) *.map

#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

chiptest.obj: \
	chiptest.asm ..\sb-6502.inc