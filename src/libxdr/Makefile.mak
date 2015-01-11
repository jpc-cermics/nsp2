#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = xdr.lib

OBJSC =	xdr.obj xdr_array.obj xdr_float.obj xdr_mem.obj \
	xdr_rec.obj xdr_reference.obj xdr_stdio.obj xdr_intXX_t.obj

OBJSF = 

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) -I.

FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

xdr.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_array.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_float.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_mem.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_rec.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_reference.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_stdio.obj: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
