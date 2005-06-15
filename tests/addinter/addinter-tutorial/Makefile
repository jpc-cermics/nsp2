SCIDIR=/usr/local/nsp2

include $(SCIDIR)/Makefile.incl

all	:: Makelib job message

job	:
	make -f Makelib

Makelib : builder.sce
	@echo running builder 
	@$(SCIDIR)/bin/scilab -nw -e "exec('builder.sce');quit;"

tests	: all
	make -f Makelib tests 

clean	::
	@echo Cleaning 
	@$(RM) -f -r *.o *.lo *.la *.so *.dll *.a *.dia .job.sce so_locations
	@$(RM) -f -r .libs 

distclean:: clean 
	@$(RM) -f *.dia  
	@$(RM) -f Makelib 

message:
	@echo "------------------------------------------";
	@echo "At prompt, enter:";
	@echo "-->exec loader.sce";
	@echo "------------------------------------------";

