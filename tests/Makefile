.SUFFIXES: .dia .tst 

TESTS=grand.dia 

all : $(TESTS) 


.tst.dia :
	@echo "-----------$*.dia----------------"
	@../bin/scilex -nw -e "nsptest('$*.tst');quit" 

distclean	:
	@echo cleaning tests
	@$(RM) $(TESTS) 

clean:	distclean
