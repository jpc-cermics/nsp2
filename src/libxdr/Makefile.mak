#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = xdr.lib
CFLAGS = $(CC_OPTIONS) -I.
FFLAGS = $(FC_OPTIONS)

OBJSC =	xdr.obj xdr_array.obj xdr_float.obj xdr_mem.obj \
	xdr_rec.obj xdr_reference.obj xdr_stdio.obj

OBJSF = 

include ../../Makefile.incl.mak
include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile


xdr.obj: rpc/types.h rpc/xdr.h
xdr_array.obj: rpc/types.h rpc/xdr.h
xdr_float.obj: rpc/types.h rpc/xdr.h
xdr_mem.obj: rpc/types.h rpc/xdr.h
xdr_rec.obj: rpc/types.h rpc/xdr.h
xdr_reference.obj: rpc/types.h rpc/xdr.h
xdr_stdio.obj: rpc/types.h rpc/xdr.h
