#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY=nsp.lib

OBJSF=
OBJSC=  blocks_new.obj evaluate_expr.obj import.obj intcos.obj \
	sciblk2.obj scicos.obj \
	dmmul.obj  blocks.obj trees.obj simul.obj \

# blocks.obj


BLOCKS=$(BLOCKSF) $(BLOCKSC)

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) -MMD

FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.libmk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile

#----------

blocks.c: ../include/nsp/machine.h
blocks.c: ../include/nsp/math.h 
blocks.c: ../include/nsp/graphics-old/Graphics.h 
blocks.c: ../include/nsp/object.h 
blocks.c: ../include/nsp/blas.h 
blocks.c: ../include/nsp/matutil.h 
blocks.c: ../librand/grand.h 
blocks.c: ../system/files.h 
blocks.c: ../include/scicos/scicos.h
blocks.c: ../include/scicos/blocks.h
blocks.c: ../interp/LibsTab.h
blocks.c: ../include/nsp/gtk/gobject.h

blocks_new.c: ../include/nsp/machine.h
blocks_new.c: ../include/nsp/graphics-old/Graphics.h 
blocks_new.c: ../include/nsp/object.h 
blocks_new.c: ../include/nsp/blas.h 
blocks_new.c: ../include/nsp/matutil.h 
blocks_new.c: ../include/scicos/scicos.h
blocks_new.c: ../include/scicos/blocks.h

dmmul.c: ../include/nsp/machine.h
dmmul.c: ../include/nsp/object.h
dmmul.c: ../include/nsp/blas.h

evaluate_expr.c: ../include/scicos/scicos.h

import.c: ../include/nsp/math.h
import.c: ../include/nsp/machine.h
import.c: ../include/nsp/object.h
import.c: ../include/scicos/scicos.h

intcos.c: ../include/nsp/machine.h
intcos.c: ../include/nsp/matrix-in.h
intcos.c: ../include/nsp/bmatrix-in.h
intcos.c: ../include/scicos/scicos.h

sciblk2.c: ../include/nsp/machine.h
sciblk2.c: ../include/nsp/object.h
sciblk2.c: ../include/scicos/scicos.h

scicos.c: ../include/nsp/machine.h
scicos.c: ../functions/linking.h
scicos.c: ../include/nsp/graphics-old/Graphics.h 
scicos.c: ../include/nsp/object.h 
scicos.c: ../include/nsp/blas.h 
scicos.c: ../include/nsp/matutil.h 
scicos.c: ../include/nsp/system.h 
scicos.c: ../include/scicos/scicos.h
scicos.c: ../include/scicos/blocks.h

simul.c: ../include/nsp/machine.h
simul.c: ../include/nsp/matrix-in.h
simul.c: ../include/nsp/bmatrix-in.h
simul.c: ../include/scicos/scicos.h
simul.c: ../include/scicos/simul.h

trees.c: ../include/nsp/machine.h
trees.c: ../include/nsp/math.h
trees.c: ../include/nsp/gsort-p.h
