.SUFFIXES: .dia .tst 

TESTS= \
	grand.dia \
	delete_op-nsp.dia \
	class.dia \
	gsort-nsp.dia \
	env.dia \
	hash.dia \
	setro.dia \
	matrix_io.dia \
	test_minmax.dia \
	testsbdiv.dia \
	fft.dia \
	extract.dia 

AFINIR= \
	matelm-nsp.dia \
	matops-nsp.dia \
	sparse-nsp.dia \
	strelm-nsp.dia \
	algebra.dia

all : $(TESTS) 

tests: all

.tst.dia :
	@echo -n "testing $*: "
	@../bin/scilab -nw -e "nsptest('$*.tst');quit" 

distclean	:
	@echo cleaning tests
	@$(RM) $(TESTS) 

clean:	distclean
