SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

OBJSC = module.o modulelt.o module-lib.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) -pg
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


lmo-IN.c : lmo-IN.nam 
