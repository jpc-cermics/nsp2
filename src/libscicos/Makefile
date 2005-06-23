SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY=nsp.a

OBJSF=
OBJSC=  blocks_new.o coselm.o evaluate_expr.o ftree2.o \
	ftree3.o ftree4.o import.o intcos.o intcscicos.o \
	intrealtime.o readf.o realtime.o sciblk2.o scicos.o scicosclip.o \
	scicos_free.o scicos_malloc.o scifunc.o dmmul.o sctree.o blocks.o

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



