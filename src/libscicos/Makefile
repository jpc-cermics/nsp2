SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY=nsp.a

OBJSF=
OBJSC=  blocks_new.o evaluate_expr.o import.o intcos.o \
	sciblk2.o scicos.o scicos_malloc.o  \
	dmmul.o  blocks.o trees.o simul.o \

# blocks.o


BLOCKS=$(BLOCKSF) $(BLOCKSC)

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.amk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile



