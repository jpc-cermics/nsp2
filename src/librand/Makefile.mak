#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = rand.lib

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC=Rand.obj rand_mt.obj rand_kiss.obj rand_clcg2.obj rand_clcg4.obj rand_urand.obj rand_fsultra.obj \
	genbet.obj genchi.obj genexp.obj genf.obj gengam.obj genmn.obj genmul.obj gennch.obj gennf.obj gennor.obj genprm.obj \
	igngeom.obj ignbin.obj ignnbn.obj ignpoi.obj \
	phrtsd.obj sdot.obj setgmn.obj sexpo.obj sgamma.obj snorm.obj spofa.obj

OBJSF= 


include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile



