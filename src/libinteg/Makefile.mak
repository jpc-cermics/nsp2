#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = integ.lib

OBJSC = int3dnsp.obj dopri5nsp.obj integ-IN.obj

OBJSF = ainvg.obj bnorm.obj cfode.obj  ewset.obj fnorm.obj intdy.obj lsoda.obj lsode.obj \
	lsodi.obj prepj.obj prepji.obj prja.obj rscma1.obj rscom1.obj solsy.obj stoda.obj \
	stode.obj stodi.obj svcma1.obj svcom1.obj vmnorm.obj vnorm.obj \
	fromquadpack.obj \
	rgk4.obj lsodar.obj rscar1.obj svcar1.obj rchek.obj roots.obj \
	colnew.obj  rkf45.obj rksimp.obj twodq.obj  ddaskr.obj \
	lsodar2.obj rchek2.obj roots2.obj xerrwv.obj \
	dgesl.obj dgefa.obj	dgbsl.obj dgbfa.obj daux.obj

# from linpack:	dgesl.obj dgefa.obj	dgbsl.obj dgbfa.obj

TOBEDONE= lsdisc.obj ddasrt.obj ddassl.obj ddaskr.obj

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak

 Makefile.libmk


 Makefile.libmk

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.libmk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile

