#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib 

OBJSC = mod.obj modObj.obj mod-IN.obj meObj.obj lmoObj.obj lmo-IN.obj lmo.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) -pg
FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile


lmo-IN.c : lmo-IN.nam 
