.SUFFIXES: .dia .tst 

TESTS=grand.dia 

all : $(TESTS) 


.tst.dia :
	@echo "-----------$*.dia----------------"
	@../bin/scilex -f ../macros/system/nsptest.sci -e "nsptest('$*.tst');quit" 

distclean	:
	@echo cleaning tests
	@$(RM) $(TESTS) 

clean:	distclean
