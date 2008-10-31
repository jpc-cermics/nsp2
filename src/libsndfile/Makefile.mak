#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

# -DWITH_PLAY : this is to be added to CFLAGS if 
# sndfile-play.c is added too. But it has only 
# be checked on Linux with alsa 

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

#sndfile-play.obj

OBJSC = sndfileobj.obj 

OBJSF = 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


