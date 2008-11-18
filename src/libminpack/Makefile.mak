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

OBJSC= intminpack.obj chkder.obj dogleg.obj dpmpar.obj enorm.obj	\
       fdjac1.obj fdjac2.obj hybrd1.obj hybrd.obj hybrd2.obj hybrj1.obj \
       hybrj.obj lmder1.obj lmder.obj lmdif1.obj lmdif.obj lmpar.obj \
       lmstr1.obj lmstr.obj qform.obj qrfac.obj qrsolv.obj r1mpyq.obj r1updt.obj \
       rwupdt.obj hybrd1-test.obj lmdif2.obj

OBJSF= 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile









