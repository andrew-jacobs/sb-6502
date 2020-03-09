#===============================================================================
# eForth Build Definitions
#-------------------------------------------------------------------------------
# Copyright (C)2018-2020 HandCoded Software Ltd.
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
	eforth-65c02.s19

LK65_FLAGS = \
	-bss $$0300-$$BFFF -code $$C000-$$EFFF

all:	$(TARGETS)

eforth-65c02.s19: eforth-65c02.obj
	$(LK65) $(LK65_FLAGS) -s19 -output $@ eforth-65c02.obj

clean:
	$(RM) *.obj
	$(RM) *.s19
	$(RM) *.bin
	$(RM) *.lst
	$(RM) *.map

#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

eforth-65c02.obj: \
	eforth-65c02.asm ..\sb-6502.inc