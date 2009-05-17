SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS) `pkg-config portaudio-2.0 --cflags `
FFLAGS = $(FC_OPTIONS)

OBJSC = play_file.o play_data.o play2.o paobj.o record_data.o
OBJSF = 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


