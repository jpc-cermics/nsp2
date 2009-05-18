SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS) `pkg-config portaudio-2.0 --cflags `
FFLAGS = $(FC_OPTIONS)

OBJSC = play_file.o play_data.o play2.o paobj.o record_data.o play_util.o
OBJSF = 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# no optimization for this one 

play_util.o: play_util.c
	@echo "compiling play_util.c"
	@$(CC) $(CC_OPTIONS1)  -c play_util.c 
