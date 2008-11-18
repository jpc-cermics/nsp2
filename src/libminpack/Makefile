SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC= intminpack.o chkder.o dogleg.o dpmpar.o enorm.o	\
       fdjac1.o fdjac2.o hybrd1.o hybrd.o hybrd2.o hybrj1.o \
       hybrj.o lmder1.o lmder.o lmdif1.o lmdif.o lmpar.o \
       lmstr1.o lmstr.o qform.o qrfac.o qrsolv.o r1mpyq.o r1updt.o \
       rwupdt.o hybrd1-test.o lmdif2.o

OBJSF= 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile









