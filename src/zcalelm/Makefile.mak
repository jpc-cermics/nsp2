#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = zcalelm.lib

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = zfunctions.obj dfunctions.obj ifunctions.obj convert.obj franck.obj dsort.obj dlamch.obj hilbert.obj
OBJSF = hilber.obj magic.obj urand.obj

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile



