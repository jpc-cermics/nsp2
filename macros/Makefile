SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 

SUBDIRS =  graphics miscellaneous system statistics linalg calpol

all clean distclean ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i && echo "making $@ in demos/$$i " && \
		$(MAKE) $(MFLAGS) -s $@ ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "Cannot make $@ in sub directory $$d";\
	       	  exit $$IER;;\
	   	esac;\
	done

DESTDIR=$(prefix)/lib/nsp

xxinstall::
	@echo "installing function files"
	@find . \( \( -name '*.sci' -o -name '*.bin' -o -name 'Mak*' -o -name '*.cos' -o -name '*.png' \) -a -type f  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(DESTDIR)/macros/{} \; \
		-exec echo -n "." \;
	@echo ""

install::
	@echo "installing macros files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/macros
	@tar   cf - --exclude CVS . | ( cd $(DESTDIR)/macros ; tar xfp - )






