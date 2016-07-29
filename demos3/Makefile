SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS)

SUBDIRS = graphics gtk3

all clean distclean ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i && echo "making $@ in demos3/$$i " && \
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
	@echo "installing demos3 files"
	@find . \( -type f  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(DESTDIR)/demos3/{} \; \
		-exec echo -n "." \;
	@echo ""

install::
	@echo "installing demos3 files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/demos3
	@tar   cf - --exclude CVS .  | ( cd $(DESTDIR)/demos3 ; tar xfp - )
