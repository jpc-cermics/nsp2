SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = integ.a

OBJSC = int3dnsp.o dopri5nsp.o integ-IN.o

OBJSF = ainvg.o bnorm.o cfode.o  ewset.o fnorm.o intdy.o lsoda.o lsode.o \
	lsodi.o prepj.o prepji.o prja.o rscma1.o rscom1.o solsy.o stoda.o \
	stode.o stodi.o svcma1.o svcom1.o vmnorm.o vnorm.o \
	fromquadpack.o \
	rgk4.o lsodar.o rscar1.o svcar1.o rchek.o roots.o \
	colnew.o  rkf45.o rksimp.o twodq.o  ddaskr.o \
	lsodar2.o rchek2.o roots2.o xerrwv.o \
	dgesl_linpack.o dgefa_linpack.o	dgbsl.o dgbfa.o daux.o util.o

# from linpack:	dgesl.o dgefa.o	dgbsl.o dgbfa.o

TOBEDONE= lsdisc.o ddasrt.o ddassl.o ddaskr.o

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak Makefile.amk


all:: Makefile.mak Makefile.amk

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.amk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile

colnew.o: colnew.c
	@echo compiling $<
	@$(CC) $(CFLAGS) -Wno-array-bounds -c $< -o $@
