
DEV65_DIR	=	../../..

include ../../../MakeDefns

SRCS		=	kernel.asm words.asm

OBJS		=	$(SRCS:.asm=.obj)

TARGETS		=	kernel.bin

LK65_FLAGS = \
	-bss $$0200-$$DFFF -code $$E000-$$FFFF

#===============================================================================
# Targets
#-------------------------------------------------------------------------------

all:	$(TARGETS)

clean:
	$(RM) $(OBJS)
	$(RM) *.lst
	$(RM) *.bin
	$(RM) *.map

kernel.bin:	$(OBJS)
	$(LK65) $(LK65_FLAGS) -bin -output $@ $(OBJS)

#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

kernel.obj:	kernel.asm ../include/forth.inc