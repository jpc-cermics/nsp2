SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = zcalelm.a

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = matutil.o 
OBJSF = dsort.o dlamch.o 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile



