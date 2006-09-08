.SUFFIXES: .dia .tst .vl

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
	serial.dia \
	sp.dia \
	spmult.dia \
	concat.dia \
	cells.dia 

ALG=algebra-balanc.dia \
	algebra-bdiag.dia \
	algebra-bslash.dia \
	algebra-chol.dia \
	algebra-det.dia \
	algebra-gschur.dia \
	algebra-gspec.dia \
	algebra-hess.dia \
	algebra-inv.dia \
	algebra-lsq.dia \
	algebra-lu.dia \
	algebra-qr.dia \
	algebra-rcond.dia \
	algebra-schur.dia \
	algebra-slash.dia \
	algebra-spec.dia \
	algebra-svd.dia 

AFINIR= \
	color-strings.tst \
	gsort.tst \
	hash-vl.tst \
	insert.tst \
	lists.tst \
	lu.tst \
	matelm-nsp.tst \
	matops-nsp.tst \
	matops.tst \
	pvm.tst \
	sorting-lex.tst \
	sorting.tst \
	sparse-nsp.tst \
	sparse.tst \
	sprow.tst \
	sp-sci.tst \
	strelm-nsp.tst \
	strelm.tst \
	svd.tst \
	trycatch.tst


VL=$(ALG:.dia=.vl) $(TESTS:.dia=.vl) 

all : algebra basic 

vl  : $(VL)

algebra : $(ALG) 

basic : $(TESTS) 

tests: all

.tst.dia :
	@echo -n "testing $*: "
	@../bin/scilab -nw -e "nsptest('$*.tst');quit" 

.tst.vl :
	@echo -n "valgrind $*: "
	@valgrind --leak-check=full  ../bin/scilex -nw -e "exec('$*.tst');quit" 





distclean	:
	@echo cleaning tests
	@$(RM) $(TESTS) $(ALG) libaddinter.*
	@../bin/nsplibtool OBJS=addinter.o LIBRARY=libaddinter distclean

clean:	distclean


TESTS= \
	addinter.dia \
	class.dia \
	concat.dia \
	delete_op-nsp.dia \
	dsearch.dia \
	env.dia \
	extract.dia \
	fft.dia \
	function.dia \
	grand.dia \
	gsort-nsp.dia \
	hash.dia \
	matrix_io.dia \
	qr.dia \
	regexp.dia \
	serial.dia \
	setro.dia \
	smat_concat_down.dia \
	smat_concat_right.dia \
	sp.dia \
	spmult.dia \
	test_minmax.dia \
	testsbdiv.dia
