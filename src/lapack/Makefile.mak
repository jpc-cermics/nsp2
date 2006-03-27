#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib 

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = lapack.obj lapack-IN.obj

# dhgeqz.f : patched version 

OBJSF = dspadm.obj dgpadm.obj zhpadm.obj zgpadm.obj dhgeqz.obj

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# a special rule for dlamch which must be compiler 
# without optimization 

dlamch.obj: dlamch.c 
	$(FC) -c dlamch.f -o dlamch.obj

