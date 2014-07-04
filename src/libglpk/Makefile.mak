#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

CFLAGS = $(CC_OPTIONS) $(GLPK_CFLAGS)
CXXFLAGS = $(CC_OPTIONS) 
WIP= coinmp_cpp.obj clp_cpp.obj clp-IN.obj
OBJSC  = linprog-IN.obj $(WIP)


include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

#--------------dependencies
linprog-IN.obj: linprog-IN.c
