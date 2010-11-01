SHELL = /bin/sh
TOP=../..

include $(TOP)/Makefile.incl

all::

distclean::

clean::

DESTDIR=$(prefix)/lib/nsp

install::
	@echo "installing nsp header files"
	@find . -name '*.h' -exec $(TOP)/config/install-sh \
		 $(opt_DATA) {} $(DESTDIR)/include/{} \;


