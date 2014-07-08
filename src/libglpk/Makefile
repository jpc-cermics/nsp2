SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS) $(GLPK_CFLAGS) -I/opt/ibm/ILOG/CPLEX_Studio126/cplex/include 
CXXFLAGS = $(CC_OPTIONS) 
WIP= coinmp_cpp.o clp_cpp.o clp-IN.o nspcplex.o
OBJSC  = linprog-IN.o 



include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

#--------------dependencies
linprog-IN.o: linprog-IN.c
