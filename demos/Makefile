SHELL = /bin/sh
TOP=..
include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 

SUBDIRS = graphics gtk2 

all:: 
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making all in macros/scicos_blocks/$$i..."; \
		$(MAKE) $(MFLAGS) all); \
	done

distclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making distclean in macros/scicos_blocks/$$i..."; \
		$(MAKE) $(MFLAGS)  distclean); \
	done

clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making clean in macros/scicos_blocks/$$i..."; \
		$(MAKE) $(MFLAGS) clean); \
	done

install::
	@echo "installing demos files"
	@find . \( -type f  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(prefix)/lib/nsp/demos/{} \; \
		-exec echo -n "." \;
	@echo ""


