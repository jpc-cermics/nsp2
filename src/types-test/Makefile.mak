#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib 

OBJSC = classa.obj classb.obj classc.obj 
# kiko.obj mpz.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

%.X : %.c 
	protoize -k -c -I../include $*.c 
	egrep -v "/usr/|nsp/" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' -e "s/ extern/extern/" > $*.X 
	rm -f $*.c.X

