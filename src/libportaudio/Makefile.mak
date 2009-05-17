#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

CFLAGS = $(CC_OPTIONS) `pkg-config portaudio-2.0 --cflags `
FFLAGS = $(FC_OPTIONS)

OBJSC = play_file.obj play_data.obj play2.obj paobj.obj record_data.obj
OBJSF = 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


