SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

OBJSC = agraph.o callbacks.o  gvdevice_gtk.o  gvplugin_gtk.o \
	interface.o  support.o


OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) -I./ -I/usr/include/graphviz
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile


src::	
	cd ../types-test/codegen; make 


