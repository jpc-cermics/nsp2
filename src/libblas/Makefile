SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = blas.a

OBJSC = dcopyC.o

DBLAS1 = idamax.o dasum.o daxpy.o dcopy.o ddot.o dnrm2.o \
	drot.o drotg.o dscal.o dswap.o

ZBLAS1 = dcabs1.o dzasum.o dznrm2.o izamax.o zaxpy.o zcopy.o \
	zdotc.o zdotu.o zdscal.o zrotg.o zscal.o zswap.o

#ZB1AUX = idamax.o dasum.o daxpy.o dcopy.o dnrm2.o dscal.o

# stored in lapack 
# ALLBLAS  = lsame.o xerbla.o

DBLAS2 = dgemv.o dgbmv.o dsymv.o dsbmv.o dspmv.o \
	dtrmv.o dtbmv.o dtpmv.o dtrsv.o dtbsv.o dtpsv.o \
	dger.o dsyr.o dspr.o dsyr2.o dspr2.o

ZBLAS2 = zgemv.o zgbmv.o zhemv.o zhbmv.o zhpmv.o \
	ztrmv.o ztbmv.o ztpmv.o ztrsv.o ztbsv.o ztpsv.o \
	zgerc.o zgeru.o zher.o zhpr.o zher2.o zhpr2.o

DBLAS3 = dgemm.o dsymm.o dsyrk.o dsyr2k.o dtrmm.o dtrsm.o

ZBLAS3 = zgemm.o zsymm.o zsyrk.o zsyr2k.o ztrmm.o ztrsm.o \
	zhemm.o zherk.o zher2k.o

OBJSF = $(ZBLAS1) $(ZB1AUX) $(ZBLAS2) $(ZBLAS3) $(ALLBLAS) \
	$(DBLAS1) $(DBLAS2) $(DBLAS3) 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile



