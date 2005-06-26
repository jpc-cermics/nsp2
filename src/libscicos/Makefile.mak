#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY=nsp.lib

OBJSF=
OBJSC=  blocks_new.obj coselm.obj evaluate_expr.obj import.obj intcos.obj intcscicos.obj \
	intrealtime.obj readf.obj realtime.obj sciblk2.obj scicos.obj scicosclip.obj \
	scicos_free.obj scicos_malloc.obj scifunc.obj dmmul.obj  blocks.obj trees.obj simul.obj

# blocks.obj


BLOCKS=$(BLOCKSF) $(BLOCKSC)

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.libmk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile



