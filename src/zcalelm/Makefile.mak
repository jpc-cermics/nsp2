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

OBJSC = matutil.obj 
OBJSF = dsort.obj dlamch.obj 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile



