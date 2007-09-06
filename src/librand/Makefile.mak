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

OBJSC=  Random.obj Random-IN.obj rand_mt.obj rand_kiss.obj rand_clcg2.obj rand_clcg4.obj rand_fsultra.obj rand_well1024a.obj \
	genbet.obj genchi.obj genexp.obj genf.obj gengam.obj genmn.obj genmul.obj gennch.obj gennf.obj gennor.obj genprm.obj \
	igngeom.obj ignbin.obj ignnbn.obj ignpoi.obj \
	phrtsd.obj sdot.obj setgmn.obj sexpo.obj sgamma.obj snorm.obj rejection_exp_128.obj \
	rejection_nor_128.obj rand_discrete.obj bin_trd.obj poi_trd.obj gamma_new.obj ndgauss.obj

OBJSF= 


include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile



