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

xxinstall::
	@$(TOP)/config/install-sh -d $(DESTDIR)/man/html/generated/
	@$(TOP)/config/install-sh html/generated/*  $(DESTDIR)/man/html/generated/

install::
	@echo "installing man pages"
	@$(TOP)/config/install-sh -d $(DESTDIR)/man/html/generated/
	@tar   cf - --exclude CVS html/generated  | ( cd $(DESTDIR)/man ; tar xfp - )
	@$(TOP)/config/install-sh scicoslab.4dx  $(DESTDIR)/man/


man::
