#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = maxplus.lib

OBJSC = maxplus-IN.obj howard.obj fordbellman.obj karp.obj display2.obj in_span.obj
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

