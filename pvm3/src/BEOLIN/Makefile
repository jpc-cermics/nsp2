#
# $Id$
#

PVMDIR		=	../..
SDIR		=	$(PVMDIR)/src

PVM_ARCH	=	BEOLIN
CONFFILE	=	$(PVMDIR)/conf/$(PVM_ARCH).def
LIBDIR		=	$(PVMDIR)/lib/$(PVM_ARCH)


all:
	$(MAKE) -f $(CONFFILE) -f $(SDIR)/Makefile.aimk all-beo 

install:	
	$(MAKE) -f $(CONFFILE) -f $(SDIR)/Makefile.aimk install-beo

clean:
	$(MAKE) -f $(CONFFILE) -f $(SDIR)/Makefile.aimk clean

