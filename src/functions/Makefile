SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

OBJSC = callfunc.o FunTab.o linking.o addinter.o Functions-IN.o \
	mexlib.o mex-IN.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile

#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================
