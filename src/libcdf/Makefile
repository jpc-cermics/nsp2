SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = dcd.a

OBJSC = DcdInterf.o dlamch.o algdiv.o alngam.o \
	bcorr.o betaln.o bratio.o brcmp1.o brcomp.o \
	cdfbet.o cdfbin.o cdfchi.o cdfchn.o cdff.o cdffnc.o cdfgam.o cdfnbn.o cdfnor.o \
	cdfpoi.o cdft.o cdftnc.o cumbet.o cumbin.o cumchi.o cumf.o cumfnc_new.o cumgam.o \
	cumnbn.o cumnor.o cumpoi.o cumt.o cumtnc.o cumtnc_new.o \
	dbetrm.o devlpl.o dinvnr_new.o dlanor.o dln1mx.o \
	dlnbet.o dlngam.o dt1.o erf.o exparg.o \
	gam1.o gaminv.o gamln.o gamln1.o grat1.o gratio.o gsumln.o \
	ipmpar.o rcomp.o rlog1.o spmpar.o stvaln.o cumchn_new.o zsearch.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib



all:: Makefile.mak Makefile.amk

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.amk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile

