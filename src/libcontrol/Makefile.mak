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

OBJSC = control-IN.obj \
	arl2_all.obj \
	balanc.obj \
	balbak.obj \
	bdiag.obj \
	calcsc.obj \
	cbal.obj \
	comqr3.obj \
	corth.obj \
	cortr.obj \
	dfrmg.obj \
	dgeco.obj \
	dgefa.obj \
	dgesl.obj \
	dhetr.obj \
	dqrdc.obj \
	dqrsl.obj \
	dqrsm.obj \
	ereduc.obj \
	exch.obj \
	expan.obj \
	fstair.obj \
	fxshfr.obj \
	giv.obj \
	hhdml.obj \
	hqror2.obj \
	newest.obj \
	nextk.obj \
	orthes.obj \
	ortran.obj \
	polmc.obj \
	qhesz.obj \
	qitz.obj \
	quad.obj \
	quadit.obj \
	quadsd.obj \
	qvalz.obj \
	realit.obj \
	residu.obj \
	riccpack.obj \
	rpoly.obj \
	rtitr.obj \
	shrslv.obj \
	split.obj \
	ssxmc.obj \
	sszer.obj \
	wbdiag.obj \
	wdegre.obj \
	wesidu.obj \
	wexchn.obj \
	wgeco.obj \
	wgefa.obj \
	wgesl.obj \
	wshrsl.obj \
	dgelsy1.obj \
	wexpm1.obj \
	wpade.obj \
	wclmat.obj \
	wcerr.obj \
	coef.obj \
	dexpm1.obj \
	pade.obj \
	dclmat.obj \
	cerr.obj \



OBJSF=

include ../Make.lib.mak



SRC = $(patsubst %.obj,%.c,$(OBJSC))

job	:
	mv $(SRC) src/

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

# according to compiler, do not optimize the following files

hqror2.obj: hqror2.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

comqr3.obj: comqr3.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

tql2.obj: tql2.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

imtql3.obj: imtql3.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

dsvdc.obj: dsvdc.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

wsvdc.obj: wsvdc.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

pade.obj: pade.c
	@echo "compiling $<  Wall "
	@$(CC)  $(CC_OPTIONS1) -c $< -o $@

