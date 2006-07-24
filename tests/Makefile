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
	extract.dia \
	addinter.dia \
	dsearch.dia \
	qr.dia \
	function.dia \
	smat_concat_down.dia \
	smat_concat_right.dia \
	regexp.dia \
	serial.dia

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
	@$(RM) $(TESTS) libaddinter.*
	@../bin/nsplibtool OBJS=addinter.o LIBRARY=libaddinter distclean

clean:	distclean
