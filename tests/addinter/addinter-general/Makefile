#/* -*- Mode: Makefile -*- */
# Copyright (C) 2007 Jean-Philippe Chancelier Enpc/Cermics
# This file is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License.


SCIDIR=../../../

include $(SCIDIR)/Makefile.incl

all	:: Makelib job 
	@cd lib ; make all
	@make -f Makelib

Makelib : builder.sce
	@echo running builder 
	@$(SCIDIR)/bin/scilab -nw -e "nsptest('builder.sce');quit" 

job	: 
	@make -f Makelib

tests	: all
	@echo "------------------------------------------";
	@make -f Makelib tests 
	@echo "------------------------------------------";

clean	::
	@echo Cleaning 
	@$(RM) -f -r *.o *.lo *.la *.so *.dll *.sl *.a *.dia .job.sce so_locations
	@$(RM) -f -r .libs 
	@cd lib ; make clean

distclean:: clean 
	@$(RM) -f *.dia  *_gateway.c libmex.c
	@$(RM) -f Makelib 
	@cd lib ; make distclean 

message:
	@echo "------------------------------------------";
	@echo "At Scilab prompt, enter:";
	@echo "-->exec loader.sce";
	@echo "------------------------------------------";



