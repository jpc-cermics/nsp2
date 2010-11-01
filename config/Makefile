SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 

SUBDIRS = blocks graphics scicos system

all:: 

distclean::

clean::

DESTDIR=$(prefix)/lib/nsp

SCRIPTS=findpath install-sh  ltmain.sh config.guess config.sub

install::
	@echo installing config files 
	@for i in $(SCRIPTS) ;\
	do ( \
	  $(TOP)/config/install-sh  $(opt_SCRIPT) $$i $(DESTDIR)/config/$$i ); \
	done
	@for i in Make* *.desktop *.png ;\
	do ( \
	  $(TOP)/config/install-sh  $(opt_DATA) $$i $(DESTDIR)/config/$$i ); \
	done
	@sed -e "s+src/include+include+g" $(DESTDIR)/config/Makeso.incl > $(DESTDIR)/config/Makeso1 
	@mv $(DESTDIR)/config/Makeso1 $(DESTDIR)/config/Makeso.incl
	@sed -e "s+src/include+include+g" $(DESTDIR)/config/Makemex > $(DESTDIR)/config/Makemex1 
	@mv $(DESTDIR)/config/Makemex1 $(DESTDIR)/config/Makemex



