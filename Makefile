SHELL = /bin/sh
TOP=.

include Path.incl
include Makefile.incl

all:: bin/scilex  

# can be used to update path.incl 
# note that thist is done by ./configure

path.incl: 
	./config/findpath;

# directives for linking 

include config/Makefile.linux

distclean::
	$(RM) bin/scilex

scilex-lib::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	cd src; echo "making all in src..."; \
		$(MAKE) $(MFLAGS) all;

scilex-lib::
	@cd pvm3; echo "making all in pvm3..."; $(MAKE) $(MFLAGS) "CC=$(CC)";

SUBDIRS = scripts macros demos 

all::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making all in $$i..."; \
			$(MAKE) $(MFLAGS) all); \
	done

distclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in src $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making distclean in $$i..."; \
			$(MAKE) $(MFLAGS)  distclean); \
	done

distclean::
	@cd pvm3; echo "making distclean in pvm3..."; \
	$(MAKE) $(MFLAGS) distclean;

clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in src $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making clean in $$i..."; \
			$(MAKE) $(MFLAGS)  clean); \
	done

man::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in man;\
	do \
		(cd $$i ; echo "making man in $$i..."; \
			$(MAKE) $(MFLAGS) man); \
	done

manclean:	
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in man;\
	do \
		(cd $$i ; echo "making manclean in $$i..."; \
			$(MAKE) $(MFLAGS) manclean); \
	done

tests:
	@echo "Type \"make tests\" in $(SCIDIR)/tests directory "
	@echo "  to test the  distribution"

distclean::
	$(RM) config.cache config.log config.status .binary foo.f foo.o \
	conftest conftest.c so_locations

tarbindist:
	@echo tarbindist not supported by nsp 
	@echo use ./configure --prefix=path
	@echo to get a binary version at path location

# this are the files that really need to be installed 
# from pvm 
# 	pvm3/lib/pvm \
# 	pvm3/lib/pvmd \
# 	pvm3/lib/pvmtmparch \
# 	pvm3/lib/pvmgetarch \
# 	pvm3/lib/LINUX/pvmd3 \
# 	pvm3/lib/LINUX/pvmgs \
# 	pvm3/lib/LINUX/pvm \
# 	pvm3/bin/LINUX/*

install:
	@echo "installing bin files"
	find bin \( -name '*'  \) \
		-exec $(TOP)/config/install-sh  $(opt_PROG) {} $(prefix)/lib/nsp/{} \;
	@echo "installing lib files"
	find libs \( -name '*'  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(prefix)/lib/nsp/{} \;
	find pvm3/lib \( -name '*'  \) \
		-exec $(TOP)/config/install-sh  $(opt_PROG) {} $(prefix)/lib/nsp/{} \;
	find pvm3/lib \( -name '*.a'  \) \
		-exec $(TOP)/config/install-sh  $(opt_DATA) {} $(prefix)/lib/nsp/{} \;
	find pvm3/bin \( -name '*'  \) \
		-exec $(TOP)/config/install-sh  $(opt_PROG) {} $(prefix)/lib/nsp/{} \;
	$(TOP)/config/install-sh $(opt_PROG) scripts/scilab-inst $(prefix)/lib/nsp/bin/scilab
	$(TOP)/config/install-sh $(opt_PROG) scripts/scilab-inst $(prefix)/bin/scilab
	cd demos ; make install;
	cd macros ; make install;
	cd src/include ; make install;

cvsclean::

setversion : 
	echo "NSPVERSION=nsp-cvs-`date --iso`" > Version.incl
