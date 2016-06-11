SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = $(patsubst %.c,%.o,$(wildcard *.c))
OBJSC = \
	dad.o \
	dadd.o	 \
	ddif.o   \
	dmcopy.o \
	dmmul.o	 \
	gdcp2i.o \
	iwamax.o \
	pythag.o \
	wasum.o	 \
	waxpy.o	 \
	wcopy.o	 \
	wdiv.o	 \
	wdotci.o \
	wdotcr.o \
	wmmul.o	 \
	wmul.o	 \
	wnrm2.o	 \
	wrscal.o \
	wscal.o	 \
	wsign.o	 \
	wsqrt.o	 \
	wswap.o	 \
	wwdiv.o

OBJSF =

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

SRC = $(patsubst %.o,%.c,$(OBJSC))

job	:
	mv $(SRC) src/


