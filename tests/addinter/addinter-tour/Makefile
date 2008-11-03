#/* -*- Mode: Makefile -*- */
# Copyright (C) 2005 Jean-Philippe Chancelier Enpc/Cermics
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License.


SCIDIR=../../../

include $(SCIDIR)/Makefile.incl

all	:: Makelib job 
	make -f Makelib

Makelib : builder.sce
	@echo running builder 
	@$(SCIDIR)/bin/nsp -nw -e "nsptest('builder.sce');quit" 

job	: 
	make -f Makelib

tests	: all
	make -f Makelib tests 

clean	::
	@echo Cleaning 
	@$(RM) -f -r *.o *.lo *.la *.so *.dll *.sl *.a *.dia .job.sce so_locations
	@$(RM) -f -r .libs libmex.c 

distclean:: clean 
	@$(RM) -f *.dia  *_gateway.c 
	@$(RM) -f Makelib 

message:
	@echo "------------------------------------------";
	@echo "At Scilab prompt, enter:";
	@echo "-->exec loader.sce";
	@echo "------------------------------------------";



