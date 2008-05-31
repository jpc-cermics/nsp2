#
# Id: Makefile,v 1.1.1.1 2004/04/26 15:36:59 stochopt Exp $
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

