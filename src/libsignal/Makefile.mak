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

OBJSC = signal-IN.obj \
	amell.obj \
	cmpse3.obj \
	compel.obj \
	conv2.obj \
	deli11.obj \
	deli2.obj \
	delip.obj \
	dellk.obj \
	desi00.obj \
	desi01.obj \
	desi11.obj \
	desi12.obj \
	desi14.obj \
	desi21.obj \
	desi22.obj \
	desi24.obj \
	desia.obj \
	desib.obj \
	dpsimp.obj \
	fft842.obj \
	remez.obj \
	romeg.obj \
	syredi.obj \
	transn.obj \
	tscccf.obj \
	sfact.obj \
	dsn2.obj \

PLUS=\
	cheby.obj \
	cmpse2.obj \
	coeft.obj \
	deli1.obj \
	dfft2.obj \
	dfftbi.obj \
	dfftmx.obj \
	filbut.obj \
	freque.obj \
	hammin.obj \
	ino.obj \
	nstabl.obj \
	ouch.obj \
	poles.obj \
	rpem.obj \
	sn.obj \
	snell.obj \
	tg02ad.obj \


OBJSF=

include ../Make.lib.mak



SRC = $(patsubst %.obj,%.c,$(PLUS))

job	:
	mv $(SRC) src/

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# according to compiler, do not optimize the following files

