SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

CFLAGS = $(CC_OPTIONS)
FFLAGS = $(FC_OPTIONS)

OBJSC = control-IN.o \
	arl2_all.o \
	balanc.o \
	balbak.o \
	bdiag.o \
	calcsc.o \
	cbal.o \
	comqr3.o \
	corth.o \
	cortr.o \
	dfrmg.o \
	dgeco.o \
	dgefa.o \
	dgesl.o \
	dhetr.o \
	dqrdc.o \
	dqrsl.o \
	dqrsm.o \
	ereduc.o \
	exch.o \
	expan.o \
	fstair.o \
	fxshfr.o \
	giv.o \
	hhdml.o \
	hqror2.o \
	newest.o \
	nextk.o \
	orthes.o \
	ortran.o \
	polmc.o \
	qhesz.o \
	qitz.o \
	quad.o \
	quadit.o \
	quadsd.o \
	qvalz.o \
	realit.o \
	residu.o \
	riccpack.o \
	rpoly.o \
	rtitr.o \
	shrslv.o \
	split.o \
	ssxmc.o \
	sszer.o \
	wbdiag.o \
	wdegre.o \
	wesidu.o \
	wexchn.o \
	wgeco.o \
	wgefa.o \
	wgesl.o \
	wshrsl.o \
	dgelsy1.o \
	wexpm1.o \
	wpade.o \
	wclmat.o \
	wcerr.o \
	coef.o \
	dexpm1.o \
	pade.o \
	dclmat.o \
	cerr.o \



OBJSF=

include ../Make.lib

all:: Makefile.mak

SRC = $(patsubst %.o,%.c,$(OBJSC))

job	:
	mv $(SRC) src/

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# according to compiler, do not optimize the following files

hqror2.o: hqror2.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

comqr3.o: comqr3.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

tql2.o: tql2.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

imtql3.o: imtql3.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

dsvdc.o: dsvdc.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

wsvdc.o: wsvdc.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

pade.o: pade.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

