#
# $Id$
#

PVMDIR		=	../..
SDIR		=	$(PVMDIR)/console

PVM_ARCH	=	OS2
CONFFILE	=	$(PVMDIR)/conf/$(PVM_ARCH).def
LIBDIR		=	$(PVMDIR)/lib/$(PVM_ARCH)

install:	default $(LIBDIR)

default:
	make LIBPREFIX="" -f $(CONFFILE) -f $(SDIR)/Makefile.aimk install

clean:
	make -f $(CONFFILE) -f $(SDIR)/Makefile.aimk clean

$(LIBDIR):
	mkdir $(LIBDIR)

