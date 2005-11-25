#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

OBJSC = callfunc.obj FunTab.obj linking.obj addinter.obj Functions-IN.obj \
	mexlib.obj mex-IN.obj umfpackmex.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


umfpack.obj : umfpack.c 
	$(CC) $(CFLAGS) -DmexFunction=mex_umfpack -c umfpackmex.c



#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================
