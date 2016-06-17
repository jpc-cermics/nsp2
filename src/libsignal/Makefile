SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = signal-IN.o \
	amell.o \
	cmpse3.o \
	compel.o \
	conv2.o \
	deli11.o \
	deli2.o \
	delip.o \
	dellk.o \
	desi00.o \
	desi01.o \
	desi11.o \
	desi12.o \
	desi14.o \
	desi21.o \
	desi22.o \
	desi24.o \
	desia.o \
	desib.o \
	dpsimp.o \
	fft842.o \
	remez.o \
	romeg.o \
	syredi.o \
	transn.o \
	tscccf.o \
	sfact.o \
	dsn2.o \

PLUS=\
	cheby.o \
	cmpse2.o \
	coeft.o \
	deli1.o \
	dfft2.o \
	dfftbi.o \
	dfftmx.o \
	filbut.o \
	freque.o \
	hammin.o \
	ino.o \
	nstabl.o \
	ouch.o \
	poles.o \
	rpem.o \
	sn.o \
	snell.o \
	tg02ad.o \


OBJSF=

include ../Make.lib

all:: Makefile.mak

SRC = $(patsubst %.o,%.c,$(PLUS))

job	:
	mv $(SRC) src/

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# according to compiler, do not optimize the following files

