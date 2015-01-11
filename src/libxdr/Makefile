SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = xdr.a

OBJSC =	xdr.o xdr_array.o xdr_float.o xdr_mem.o \
	xdr_rec.o xdr_reference.o xdr_stdio.o xdr_intXX_t.o

OBJSF = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) -I.

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

xdr.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_array.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_float.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_mem.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_rec.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_reference.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
xdr_stdio.o: ../include/nsp/rpc/types.h ../include/nsp/rpc/xdr.h
