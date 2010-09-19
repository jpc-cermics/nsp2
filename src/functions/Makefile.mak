#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

OBJSC = callfunc.obj FunTab.obj linking.obj addinter.obj Functions-IN.obj \
	mexlib.obj mex-IN.obj accelerated_tab.obj swiglib.obj swigglobalvar.obj swigvarlink.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================

mexlib.c : ../include/mex/mex.h 

linking.obj: link_SYSV.c link_W95.c 


