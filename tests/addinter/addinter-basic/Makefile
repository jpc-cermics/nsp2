#/* -*- Mode: Makefile -*- */
# Copyright (C) 2005-2010 Jean-Philippe Chancelier Enpc/Cermics
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License.
#
# basic example with a complete interface file  interf.c 
# 

SCIDIR=../../../
LIBRARY=libtutorial
SOURCES=interf.c
OBJS=$(SOURCES:.c=.o)

include $(SCIDIR)/Makefile.incl

all	:: $(LIBRARY).a 

$(LIBRARY).a : $(SOURCES)
	@$(SCIDIR)/bin/nsplibtool OBJS=$(OBJS) LIBRARY=$(LIBRARY) 

clean	::
	@$(SCIDIR)/bin/nsplibtool OBJS=$(OBJS) LIBRARY=$(LIBRARY) clean 

distclean::
	@$(SCIDIR)/bin/nsplibtool OBJS=$(OBJS) LIBRARY=$(LIBRARY) distclean

tests	:: all
	@$(SCIDIR)/bin/nsplibtool OBJS=$(OBJS) LIBRARY=$(LIBRARY) tests

