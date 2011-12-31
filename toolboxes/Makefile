SHELL = /bin/sh
TOP=..

include $(TOP)/Path.incl
include $(TOP)/Makefile.incl

SUBDIRS =  scicos-2.7
SUBDIRS_CLEAN =scicos-2.7


all:: 
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making all in macros/$$i..."; \
		$(MAKE) $(MFLAGS) all); \
	done

distclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS_CLEAN) ;\
	do \
		(cd $$i ; echo "making distclean in macros/$$i..."; \
		$(MAKE) $(MFLAGS)  distclean); \
	done

clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS_CLEAN) ;\
	do \
		(cd $$i ; echo "making clean in macros/$$i..."; \
		$(MAKE) $(MFLAGS) clean); \
	done

DESTDIR=$(prefix)/lib/nsp

install::
	@echo "installing toolboxes"
	@$(TOP)/config/install-sh -d $(DESTDIR)/toolboxes
	@tar   cf - --exclude CVS .  | ( cd $(DESTDIR)/toolboxes ; tar xfp - )

