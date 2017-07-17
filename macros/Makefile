SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

SILENT=$(findstring -s,$(MFLAGS))

SUBDIRS = \
	graphics miscellaneous system \
	statistics linalg calpol \
	ast rmatrix signal \
	linearsys control robust qhull \
	plotlib egraphics

DIR=

all clean distclean pp ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i && if test "x$(SILENT)" != "x-s"; then echo "making $@ in $(DIR)$$i ";fi && \
		$(MAKE) $(MFLAGS) DIR=$(DIR)$$i/ $@ ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "make $@ in sub directory $$i failed"; \
	       	   case '${MFLAGS}' in *[k]*) echo "carrying on compilation (-k used)";; *) exit $$IER;;esac;\
	   	esac;\
	done

DESTDIR=$(prefix)/lib/nsp

install::
	@echo "installing macros files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/macros
	@tar   cf - --exclude CVS . | ( cd $(DESTDIR)/macros ; tar xfp - )

