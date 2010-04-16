SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = dcd.a

OBJSC = DcdInterf.o dlamch.o algdiv.o alngam.o \
	bcorr.o betaln.o bratio.o brcmp1.o brcomp.o \
	cdfbet.o cdfbin.o cdfchi.o cdfchn.o cdff.o cdffnc.o cdfgam.o cdfnbn.o cdfnor.o \
	cdfpoi.o cdft.o cdftnc.o cumbet.o cumbin.o cumchi.o cumchn.o cumf.o cumfnc.o cumgam.o \
	cumnbn.o cumnor.o cumpoi.o cumt.o cumtnc.o \
	dbetrm.o devlpl.o dexpm1.o dinvnr.o dinvr.o dlanor.o dln1mx.o dln1px.o \
	dlnbet.o dlngam.o dstrem.o dt1.o dzror.o \
	erf.o exparg.o \
	gam1.o gaminv.o gamln.o gamln1.o gamma.o grat1.o gratio.o gsumln.o \
	ipmpar.o psi.o rcomp.o rexp.o rlog.o rlog1.o spmpar.o stvaln.o cumchn_new.o

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

