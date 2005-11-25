SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

OBJSC = callfunc.o FunTab.o linking.o addinter.o Functions-IN.o \
	mexlib.o mex-IN.o umfpackmex.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


umfpack.o : umfpack.c 
	$(CC) $(CFLAGS) -DmexFunction=mex_umfpack -c umfpackmex.c



#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================
