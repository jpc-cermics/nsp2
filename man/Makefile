SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

all::

distclean::
	cd pdf; make distclean
	cd html; make distclean

clean:: distclean

install::
	@$(TOP)/config/install-sh -d $(prefix)/lib/nsp/man/html/generated/
	@$(TOP)/config/install-sh html/generated/*  $(prefix)/lib/nsp/man/html/generated/
