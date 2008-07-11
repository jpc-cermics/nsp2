#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

CFLAGS = $(CC_OPTIONS) -DWITH_PLAY 
FFLAGS = $(FC_OPTIONS)

#sndfile-play.obj

OBJSC = sndfileobj.obj sndfile-play.obj
OBJSF = 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


