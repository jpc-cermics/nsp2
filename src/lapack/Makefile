SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = lapack.o lapack-IN.o lapack_all.o

# dhgeqz.f : patched version 

OBJSF = dspadm.o dgpadm.o zhpadm.o zgpadm.o dhgeqz.o

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# a special rule for dlamch which must be compiler 
# without optimization 

dlamch.o: dlamch.c 
	$(FC) -c dlamch.f -o dlamch.o

