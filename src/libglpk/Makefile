SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS) $(GLPK_CFLAGS)
CXXFLAGS = $(CC_OPTIONS) 
WIP= coinmp_cpp.o clp_cpp.o clp-IN.o
OBJSC  = linprog-IN.o $(WIP)


include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

#--------------dependencies
linprog-IN.o: linprog-IN.c
