#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = matutil.obj matnum.obj qsort.obj qsort1.obj qsort2.obj \
	qsort-stable.obj merge-sort.obj

# gsort.obj

OBJSF = dsort.obj 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

matnum.obj: matnum.c
	@echo "compiling matnum.c"
	@$(CC) $(CC_OPTIONS1)  -c matnum.c 



