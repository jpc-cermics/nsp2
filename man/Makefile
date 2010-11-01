SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

all::

distclean::
	cd pdf; make distclean
	cd html; make distclean

clean:: distclean

DESTDIR=$(prefix)/lib/nsp

install::
	@$(TOP)/config/install-sh -d $(DESTDIR)/man/html/generated/
	@$(TOP)/config/install-sh html/generated/*  $(DESTDIR)/man/html/generated/
