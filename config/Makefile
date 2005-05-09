SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 

SUBDIRS = blocks graphics scicos system

all:: 

distclean::

clean::


SCRIPTS=findpath install-sh  ltmain.sh config.guess config.sub

install::
	@echo installing config files 
	@for i in $(SCRIPTS) ;\
	do ( \
	  $(TOP)/config/install-sh  $(opt_SCRIPT) $$i $(prefix)/lib/nsp/config/$$i ); \
	done
	@for i in Make* ;\
	do ( \
	  $(TOP)/config/install-sh  $(opt_DATA) $$i $(prefix)/lib/nsp/config/$$i ); \
	done


