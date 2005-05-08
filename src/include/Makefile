SHELL = /bin/sh
TOP=../..

include $(TOP)/Makefile.incl

all::

distclean::

clean::

install::
	@echo "installing nsp header files"
	find . -name '*.h' -exec $(TOP)/config/install-sh \
		 $(opt_DATA) {} $(prefix)/lib/nsp/include/{} \;


