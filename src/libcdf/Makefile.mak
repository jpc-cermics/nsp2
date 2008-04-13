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
	cdfpoi.obj cdft.obj cdftnc.obj cumbet.obj cumbin.obj cumchi.obj cumchn.obj cumf.obj cumfnc.obj cumgam.obj \
	cumnbn.obj cumnor.obj cumpoi.obj cumt.obj cumtnc.obj \
	dbetrm.obj devlpl.obj dexpm1.obj dinvnr.obj dinvr.obj dlanor.obj dln1mx.obj dln1px.obj \
	dlnbet.obj dlngam.obj dstrem.obj dt1.obj dzror.obj \
	erf.obj exparg.obj \
	gam1.obj gaminv.obj gamln.obj gamln1.obj gamma.obj grat1.obj gratio.obj gsumln.obj \
	ipmpar.obj psi.obj rcomp.obj rexp.obj rlog.obj rlog1.obj spmpar.obj stvaln.obj 

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

