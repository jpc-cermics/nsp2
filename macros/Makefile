SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

SILENT=$(findstring s,$(MFLAGS))

SUBDIRS = \
	graphics miscellaneous system \
	statistics linalg calpol \
	ast rmatrix signal \
	linearsys control robust \

all clean distclean ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i && if test "x$(SILENT)" != "xs"; then echo "making $@ in macros/$$i ";fi && \
		$(MAKE) $(MFLAGS) -s $@ ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "Cannot make $@ in sub directory $$d";\
	       	  exit $$IER;;\
	   	esac;\
	done

DESTDIR=$(prefix)/lib/nsp

install::
	@echo "installing macros files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/macros
	@tar   cf - --exclude CVS . | ( cd $(DESTDIR)/macros ; tar xfp - )


