#
# $Id$
#

PVMDIR		=	../..
SDIR		=	$(PVMDIR)/src

PVM_ARCH	=	OS2
CONFFILE	=	$(PVMDIR)/conf/$(PVM_ARCH).def
LIBDIR		=	$(PVMDIR)/lib/$(PVM_ARCH)


all:
	make LIBPREFIX="" -f $(CONFFILE) -f $(SDIR)/Makefile.aimk all-os2

install:
	make LIBPREFIX="" \
		-f $(CONFFILE) -f $(SDIR)/Makefile.aimk install-os2

clean:
	make LIBPREFIX="" -f $(CONFFILE) -f $(SDIR)/Makefile.aimk clean

