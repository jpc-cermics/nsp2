.SUFFIXES: .dia .tst .vl

TESTS= \
	addinter.dia \
	cells.dia \
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
	gsort.dia \
	hash.dia \
	insert.dia \
	lists.dia \
	lu.dia \
	matrix_io.dia \
	matelm-nsp.dia \
	qr.dia \
	regexp.dia \
	serial.dia \
	setdiff.dia \
	setro.dia \
	smat_concat_down.dia \
	smat_concat_right.dia \
	sorting-lex.dia \
	sorting.dia \
	sp.dia \
	spmult.dia \
	sprow.dia \
	strings.dia \
	svd.dia \
	test_minmax.dia \
	testsbdiv.dia \
	trycatch.dia \
	umfpack.dia \
	unique.dia \
	createbyset.dia \
	sparse-mixed.dia \
	extract_empty.dia \
	sci_save_load.dia \
	global.dia

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
	hash-vl.tst \
	matops-nsp.tst \
	matops.tst \
	pvm.tst \
	sparse-nsp.tst \
	sparse.tst \
	sp-sci.tst \



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
	@valgrind --leak-check=full  ../bin/scilex -nw -e "exec('$*.tst');quit" >& $*.vl
	@echo  " $*.vl"

distclean	:
	@echo cleaning tests
	@$(RM) $(TESTS) $(ALG) $(VL) libaddinter.*
	@../bin/nsplibtool OBJS=addinter.o LIBRARY=libaddinter distclean

clean:	distclean


