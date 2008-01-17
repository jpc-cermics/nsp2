SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

OBJSC = classa.o classb.o classc.o classaref.o classbref.o
# example_wrap.o example.o 

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

%.X : %.c 
	protoize -k -c -I../include $*.c 
	egrep -v "/usr/|nsp/" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' -e "s/ extern/extern/" > $*.X 
	rm -f $*.c.X

example.o	: example.cxx 
	g++ -g -c example.cxx 

example_wrap.o	: example_wrap.cpp
	g++ -I../include -g -c example_wrap.cpp


ajour	:
	cd /usr/local/src/nsp2-jpc/keep/test;make 
	cp /usr/local/src/nsp2-jpc/keep/test/classaref.c .
	cp /usr/local/src/nsp2-jpc/keep/test/classaref.h ../include/nsp 
	cp /usr/local/src/nsp2-jpc/keep/test/classbref.c .
	cp /usr/local/src/nsp2-jpc/keep/test/classbref.h ../include/nsp 
