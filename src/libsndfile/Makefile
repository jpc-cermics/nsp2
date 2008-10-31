SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

# -DWITH_PLAY : this is to be added to CFLAGS if 
# sndfile-play.c is added too. But it has only 
# be checked on Linux with alsa 

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

#sndfile-play.o

OBJSC = sndfileobj.o 

OBJSF = 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


