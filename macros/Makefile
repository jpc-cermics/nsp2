SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 

WIP=	auto-sci \
	control-sci \
	elementary-sci \
	linalg-sci \
	optim-sci \
	robust-sci \
	lss-sci \

SUBDIRS =  graphics miscellaneous system statistics linalg calpol ast $(WIP)

all clean distclean ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i && echo "making $@ in macros/$$i " && \
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


up	:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(WIP) ;\
	do \
		(cd $$i && echo "making $@ in macros/$$i " && \
		cvs up ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "Cannot make $@ in sub directory $$d";\
	       	  exit $$IER;;\
	   	esac;\
	done

commit	:
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(WIP) ;\
	do \
		(cd $$i && echo "making $@ in macros/$$i " && \
		cvs commit -m updates ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "Cannot make $@ in sub directory $$d";\
	       	  exit $$IER;;\
	   	esac;\
	done


