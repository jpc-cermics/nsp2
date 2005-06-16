SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = matutil.o matnum.o qsort.o qsort1.o qsort2.o \
	qsort-stable.o merge-sort.o

# gsort.o

OBJSF = dsort.o 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

matnum.o: matnum.c
	@echo "compiling matnum.c"
	@$(CC) $(CC_OPTIONS1)  -c matnum.c 



