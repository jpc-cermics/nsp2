SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = maxplus.a

OBJSC = maxplus-IN.o howard.o fordbellman.o karp.o display2.o in_span.o
OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak Makefile.amk

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.amk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile

