#===============================================================================
# OS/65 Build Definitions
#-------------------------------------------------------------------------------
# Copyright (C)2016-2017 HandCoded Software Ltd.
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
# 2016-03-06 AJ Initial version
#-------------------------------------------------------------------------------

include	../../../MakeDefns

#===============================================================================
# Targets
#-------------------------------------------------------------------------------

TARGET	=	os65.lib

SRCS	=	heap.c file.c

OBJS	=	$(SRCS:.c=.obj)

$(TARGET):	$(OBJS)
		$(RM) $(TARGET)
		$(AR) -A $(TARGET) $(OBJS)

#-------------------------------------------------------------------------------

install:	$(TARGET)
		$(CP) $(TARGET) ..\..\..\lib

clean:
		$(RM) $(TARGET)
		$(RM) $(OBJS)
		$(RM) *.lst
		$(RM) *.map

#===============================================================================
# Dependencies
#-------------------------------------------------------------------------------

heap.obj:	\
			heap.c

file.obj:	\
			file.c
