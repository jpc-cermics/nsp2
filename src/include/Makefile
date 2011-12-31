SHELL = /bin/sh
TOP=../..

include $(TOP)/Makefile.incl

all::

distclean::

clean::

DESTDIR=$(prefix)/lib/nsp

xxinstall::
	@echo "installing nsp header files"
	@find . -name '*.h' -exec $(TOP)/config/install-sh \
		 $(opt_DATA) {} $(DESTDIR)/include/{} \;

install::
	@echo "installing nsp header files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/include
	@tar   cf - --exclude CVS --exclude Makefile .  | ( cd $(DESTDIR)/include ; tar xfp - )


