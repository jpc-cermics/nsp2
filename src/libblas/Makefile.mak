#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = blas.lib

OBJSC = dcopyC.obj

DBLAS1 = idamax.obj dasum.obj daxpy.obj dcopy.obj ddot.obj dnrm2.obj \
	drot.obj drotg.obj dscal.obj dswap.obj

ZBLAS1 = dcabs1.obj dzasum.obj dznrm2.obj izamax.obj zaxpy.obj zcopy.obj \
	zdotc.obj zdotu.obj zdscal.obj zrotg.obj zscal.obj zswap.obj

#ZB1AUX = idamax.obj dasum.obj daxpy.obj dcopy.obj dnrm2.obj dscal.obj

# stored in lapack 
# ALLBLAS  = lsame.obj xerbla.obj

DBLAS2 = dgemv.obj dgbmv.obj dsymv.obj dsbmv.obj dspmv.obj \
	dtrmv.obj dtbmv.obj dtpmv.obj dtrsv.obj dtbsv.obj dtpsv.obj \
	dger.obj dsyr.obj dspr.obj dsyr2.obj dspr2.obj

ZBLAS2 = zgemv.obj zgbmv.obj zhemv.obj zhbmv.obj zhpmv.obj \
	ztrmv.obj ztbmv.obj ztpmv.obj ztrsv.obj ztbsv.obj ztpsv.obj \
	zgerc.obj zgeru.obj zher.obj zhpr.obj zher2.obj zhpr2.obj

DBLAS3 = dgemm.obj dsymm.obj dsyrk.obj dsyr2k.obj dtrmm.obj dtrsm.obj

ZBLAS3 = zgemm.obj zsymm.obj zsyrk.obj zsyr2k.obj ztrmm.obj ztrsm.obj \
	zhemm.obj zherk.obj zher2k.obj

OBJSF = $(ZBLAS1) $(ZB1AUX) $(ZBLAS2) $(ZBLAS3) $(ALLBLAS) \
	$(DBLAS1) $(DBLAS2) $(DBLAS3) 

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile



