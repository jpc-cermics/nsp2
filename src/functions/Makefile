SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

OBJSC = callfunc.o linking.o addinter.o Functions-IN.o \
	mexlib.o mex-IN.o accelerated_tab.o \
	swiglib.o swigglobalvar.o swigvarlink.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================

mexlib.c : ../include/mex/mex.h 
linking.o: link_SYSV.c link_W95.c 

include Makefile.deps 
