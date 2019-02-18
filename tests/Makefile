.SUFFIXES: .dia .tst .vl

SHELL=/bin/csh


TESTS= \
	addinter.dia \
	cells.dia \
	class.dia \
	classref.dia \
	concat.dia \
	delete_op-nsp.dia \
	bsearch.dia \
	env.dia \
	extract.dia \
	fft.dia \
	fftnew.dia \
	function.dia \
	grand.dia \
	gsort-nsp.dia \
	gsort.dia \
	hash.dia \
	insert.dia \
	lists.dia \
	lu.dia \
	matrix_io.dia \
	io.dia \
	sscanf.dia \
	matelm-nsp.dia \
	minmax.dia \
	qr.dia \
	regexp.dia \
	serial.dia \
	setdiff.dia \
	setro.dia \
	smat_concat_down.dia \
	smat_concat_right.dia \
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
	setfunctions.dia \
	ndind2ind.dia \
	createbyset.dia \
	extract_empty.dia \
	sci_save_load.dia \
	global.dia \
	cholmod.dia \
	bool.dia  \
	dollar.dia \
	intg.dia \
	int2d.dia \
	int3d.dia \
	intg_splin.dia \
	sparse_ij_val.dia \
	sparse-mixed.dia \
	solve.dia \
	legendre.dia \
	scalexp.dia \
	fsolve1.dia \
	fsolve.dia \
	ode_1.dia \
	ode_2.dia \
	ode_3.dia \
	find.dia \
	global.dia \
	matops.dia \
	matops-int.dia \
	min_max_minmax_int.dia \
	statfuncs.dia \
	smio.dia \
	select.dia \
	markup.dia \
	polynoms.dia \
	errcatch.dia \
	execf.dia \
	cdfxxx.dia \
	spmf.dia \
	linprog.dia \
	scale_rc.dia \
	percenta.dia \
	ast.dia \
	persistent.dia \
	stochdec.dia \
	linmeq.dia \
	qhull.dia \
	loadmatfile.dia \
	rmatrix.dia \
	spqr-more.dia \

ALG=algebra-balanc.dia \
	algebra-bdiag.dia \
	algebra-bslash.dia \
	algebra-chol.dia \
	algebra-det.dia \
	algebra-qz.dia \
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
	ode_4.dia \
	color-strings.tst \
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

# on windows USE ../bin/nsp-nowin.exe 

.tst.dia :
	@echo -n "testing $*: "
	@../bin/nsp -ns -nw -e "nsptest('$*.tst');quit" 

#export G_DEBUG=gc-friendly;export G_SLICE=always-malloc;valgrind --suppressions=tests/valgrind.supp --leak-check=full  bin/scilex -nw 

.tst.vl :
	@echo -n "valgrind $*: "
	@setenv G_DEBUG gc-friendly; setenv G_SLICE always-malloc; \
		valgrind --suppressions=valgrind.supp --leak-check=full  \
			../bin/scilex -nw -e "exec('$*.tst');quit" >& $*.vl
	@echo  " $*.vl"

distclean	::
	@echo cleaning tests
	@$(RM) $(TESTS) $(ALG) $(VL) 
	@../bin/nsplibtool OBJS=addinter.o LIBRARY=libaddinter distclean

clean:	distclean


# Tests generated from manual examples
#--------------------------------------

MANTESTS= \
	man-signal.dia \
	man-linalg.dia \
	man-robust.dia \
	man-control.dia \
	man-arrays.dia \
	man-arrays.dia \
	man-basicnumarrays.dia \
	man-datatypes.dia \
	man-graphics.dia \
	man-gui.dia \
	man-misc.dia \
	man-numbers.dia \
	man-numerics.dia \
	man-polynomial.dia \
	man-spmf.dia \
	man-statistics.dia \
	man-strings.dia \
	man-system.dia \

test-manuals: $(MANTESTS)

man-signal.tst: ../man/src/signal/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-robust.tst: ../man/src/robust/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-control.tst: ../man/src/control/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-linalg.tst : ../man/src/linalg/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

distclean::
	@$(RM) -f $(MANTESTS)


# To be added

man-arrays.tst : ../man/src/arrays/*.tex
	@echo "generate $*.tst from man"
	../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-basicnumarrays.tst : ../man/src/basicnumarrays/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-datatypes.tst : ../man/src/datatypes/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-fsqp.tst : ../man/src/fsqp/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-graphics.tst : ../man/src/graphics/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-gui.tst : ../man/src/gui/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-list.tst : ../man/src/list/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-misc.tst : ../man/src/misc/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-model.tst : ../man/src/model/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-numbers.tst : ../man/src/numbers/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-numerics.tst : ../man/src/numerics/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-polynomial.tst : ../man/src/polynomial/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-sorting.tst : ../man/src/sorting/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-sounds.tst : ../man/src/sounds/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-spmf.tst : ../man/src/spmf/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-statistics.tst : ../man/src/statistics/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-strings.tst : ../man/src/strings/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"

man-system.tst : ../man/src/system/*.tex
	@echo "generate $*.tst from man"
	@../bin/nsp -nw -e "exec('extractman.sci');extractman('$*');quit"
