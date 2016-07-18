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

OBJSC = $(patsubst %.c,%.obj,$(wildcard *.c))
OBJSC = \
	dad.obj \
	dadd.obj	 \
	ddif.obj   \
	dmcopy.obj \
	dmmul.obj	 \
	gdcp2i.obj \
	iwamax.obj \
	pythag.obj \
	wasum.obj	 \
	waxpy.obj	 \
	wcopy.obj	 \
	wdiv.obj	 \
	wdotci.obj \
	wdotcr.obj \
	wmmul.obj	 \
	wmul.obj	 \
	wnrm2.obj	 \
	wrscal.obj \
	wscal.obj	 \
	wsign.obj	 \
	wsqrt.obj	 \
	wswap.obj	 \
	wwdiv.obj  \
	coshin.obj \
	dprxc.obj \
	wprxc.obj \
	rat.obj \

OBJSF =

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

SRC = $(patsubst %.obj,%.c,$(OBJSC))

job	:
	mv $(SRC) src/


