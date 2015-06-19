SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 

SUBDIRS = graphics gtk2 


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
	@echo "installing demos files"
	@find . \( -type f  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(DESTDIR)/demos/{} \; \
		-exec echo -n "." \;
	@echo ""

install::
	@echo "installing demos files"
	@$(TOP)/config/install-sh -d $(DESTDIR)/demos
	@tar   cf - --exclude CVS .  | ( cd $(DESTDIR)/demos ; tar xfp - )




