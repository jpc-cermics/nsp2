SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

SUBDIRS = graphics gtk2 
DIR=
SILENT=$(findstring -s,$(MFLAGS))

all clean distclean ::
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

xxinstall::
	@echo "installing demos files"
	@find . \( -type f  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(DESTDIR)/demos/{} \; \
		-exec echo -n "." \;
	@echo ""

install::
	@echo "installing demos files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/demos
	@tar   cf - --exclude CVS .  | ( cd $(DESTDIR)/demos ; tar xfp - )




