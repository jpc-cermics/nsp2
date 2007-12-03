SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = spmf.a

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC=spmf.o spmf-IN.o

OBJSF=dxlegf.o 


include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile



