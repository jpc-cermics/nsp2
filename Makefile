SHELL = /bin/sh

binary:
	@if test -f .binary; then \
		echo "Humm... this is a binary version"; \
		config/findpath; \
		(cd scripts; make); \
		echo "Installation done"; \
	else \
		echo "**********************************"; \
		echo "Humm... this is a source version."; \
		echo "Please read the README file first."; \
		echo "**********************************"; \
	fi

#SCIDIR=.
include Path.incl
include Makefile.incl

all:: bin/scilex  

# Add the object files that are used to compile Scilex
# include Makefile.OBJ

include config/Makefile.linux

distclean::
	$(RM) bin/scilex

SUBDIRS = scripts

scilex-lib::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	cd src; echo "making all in src..."; \
		$(MAKE) $(MFLAGS) all;

scilex-lib::
	@cd pvm3; echo "making all in pvm3..."; $(MAKE) $(MFLAGS) "CC=$(CC)";

all::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making all in $$i..."; \
			$(MAKE) $(MFLAGS) all); \
	done

distclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in src $(SUBDIRS) man;\
	do \
		(cd $$i ; echo "making distclean in $$i..."; \
			$(MAKE) $(MFLAGS)  distclean); \
	done

distclean::
	@cd pvm3; echo "making distclean in pvm3..."; \
	$(MAKE) $(MFLAGS) distclean;


clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in src $(SUBDIRS) man;\
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

# SCIBASE for scilab binaries generation
SCIBASE = scilab-2.6

PVMBINDISTFILES = \
	$(SCIBASE)/pvm3/lib/pvm \
	$(SCIBASE)/pvm3/lib/pvmd \
	$(SCIBASE)/pvm3/lib/pvmtmparch \
	$(SCIBASE)/pvm3/lib/pvmgetarch \
	$(SCIBASE)/pvm3/lib/LINUX/pvmd3 \
	$(SCIBASE)/pvm3/lib/LINUX/pvmgs \
	$(SCIBASE)/pvm3/lib/LINUX/pvm \
	$(SCIBASE)/pvm3/bin/LINUX/*

BINDISTFILES = \
	$(SCIBASE)/.binary \
	$(SCIBASE)/.pvmd.conf \
	$(SCIBASE)/ACKNOWLEDGEMENTS \
	$(SCIBASE)/CHANGES \
	$(SCIBASE)/Makefile \
        $(SCIBASE)/Makefile.OBJ \
	$(SCIBASE)/Makefile.incl \
	$(SCIBASE)/Makemex \
        $(SCIBASE)/Path.incl \
	$(SCIBASE)/README \
	$(SCIBASE)/Version.incl \
	$(SCIBASE)/configure \
	$(SCIBASE)/libtool \
	$(SCIBASE)/license.txt \
	$(SCIBASE)/licence.txt \
	$(SCIBASE)/scilab.quit \
	$(SCIBASE)/scilab.star \
	$(SCIBASE)/X11_defaults \
	$(SCIBASE)/bin \
	$(SCIBASE)/config \
	$(SCIBASE)/contrib \
	$(SCIBASE)/demos \
	$(SCIBASE)/examples \
	$(SCIBASE)/imp/NperiPos.ps \
	$(SCIBASE)/imp/giffonts \
	$(SCIBASE)/macros \
	$(SCIBASE)/man/eng/*.htm \
	$(SCIBASE)/man/eng/*/*.htm \
	$(SCIBASE)/man/fr/*/*.htm \
	$(SCIBASE)/man/fr/*.htm \
	$(SCIBASE)/man/*.dtd \
	$(SCIBASE)/man/*/*.xsl \
	$(SCIBASE)/src/*.h \
	$(SCIBASE)/src/*/*.h \
	$(SCIBASE)/src/Make.lib \
	$(SCIBASE)/src/default/FCreate \
	$(SCIBASE)/src/default/Flist \
	$(SCIBASE)/src/default/README \
	$(SCIBASE)/src/default/fundef \
	$(SCIBASE)/src/default/*.c \
	$(SCIBASE)/src/default/*.f \
	$(SCIBASE)/scripts \
	$(SCIBASE)/tcl \
	$(SCIBASE)/tests \
	$(SCIBASE)/util

tarbindist:
	touch .binary
	strip bin/scilex
	cd tests; make distclean
	cd examples; make distclean
	cd .. ; tar cvf $(SCIBASE)/$(SCIBASE)-bin.tar $(BINDISTFILES) $(PVMBINDISTFILES)
	$(RM) .binary

LIBPREFIX = /usr

install:
	@if test `pwd` != ${LIBPREFIX}/$(SCIBASE); then \
		touch .binary; \
		strip $(SCIDIR)/bin/scilex; \
		(cd tests; make distclean); \
		(cd examples; make distclean); \
		(cd .. ; tar cf - $(BINDISTFILES) $(PVMBINDISTFILES) $(PVMBINDISTFILES1) | (cd ${LIBPREFIX}; tar xf -)); \
		(cd ${LIBPREFIX}/$(SCIBASE); make); \
		$(RM) .binary; \
	fi
	$(RM) /usr/bin/scilab
	ln -fs ${LIBPREFIX}/$(SCIBASE)/bin/scilab /usr/bin/scilab
	$(RM) /usr/bin/intersci
	ln -fs ${LIBPREFIX}/$(SCIBASE)/bin/intersci /usr/bin/intersci
	$(RM) /usr/bin/intersci-n
	ln -fs ${LIBPREFIX}/$(SCIBASE)/bin/intersci-n /usr/bin/intersci-n


uninstall:
	$(RM) -r ${LIBPREFIX}/$(SCIBASE)
	$(RM) /usr/bin/scilab
	$(RM) /usr/bin/intersci
	$(RM) /usr/bin/intersci-n


cvsclean::
	@$(RM) -f -r geci xless wless xmetanet src/comm src/libcomm
	@cd man; $(RM) -f -r arma comm control dcd elementary fileio functions graphics gui identification linear metanet nonlinear polynomials programming pvm robust scicos signal sound strings tdcs time-date tksci translation utilities

setversion : 
	echo "SCIVERSION=scilab-2.7-CVS-`date --iso`" > Version.incl
