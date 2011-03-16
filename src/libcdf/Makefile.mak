#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = dcd.lib

OBJSC = DcdInterf.obj dlamch.obj algdiv.obj alngam.obj \
	bcorr.obj betaln.obj bratio.obj brcmp1.obj brcomp.obj \
	cdfbet.obj cdfbin.obj cdfchi.obj cdfchn.obj cdff.obj cdffnc.obj cdfgam.obj cdfnbn.obj cdfnor.obj \
	cdfpoi.obj cdft.obj cdftnc.obj cumbet.obj cumbin.obj cumchi.obj cumf.obj cumfnc_new.obj cumgam.obj \
	cumnbn.obj cumnor.obj cumpoi.obj cumt.obj cumtnc.obj cumtnc_new.obj \
	dbetrm.obj devlpl.obj dinvnr_new.obj dlanor.obj dln1mx.obj \
	dlnbet.obj dlngam.obj dt1.obj erf.obj exparg.obj \
	gam1.obj gaminv.obj gamln.obj gamln1.obj grat1.obj gratio.obj gsumln.obj \
	ipmpar.obj rcomp.obj rlog1.obj spmpar.obj stvaln.obj cumchn_new.obj zsearch.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



 Makefile.libmk

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

Makefile.libmk	: Makefile
	$(SCIDIR)/scripts/Mak2ABSMak Makefile

