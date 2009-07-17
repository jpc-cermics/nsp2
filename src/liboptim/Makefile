SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = optim-IN.o \
	ctcab.o \
	ctonb.o \
	dcube.o \
	fcube.o \
	fuclid.o \
	gcbd.o \
	icscof.o \
	icse0.o \
	icse.o \
	icsec2.o \
	icsei.o \
	majour.o \
	n1fc1.o \
	n1qn1.o \
	n1qn3.o \
	nlis0.o \
	proj.o \
	qnbd.o \
	rlbd.o 

OBJSF = 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

