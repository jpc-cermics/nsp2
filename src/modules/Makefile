SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

OBJSC = 

#mod.o modObj.o mod-IN.o meObj.o lmoObj.o lmo-IN.o lmo.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) -pg
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile


lmo-IN.c : lmo-IN.nam 
