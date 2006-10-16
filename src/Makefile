SHELL = /bin/sh

include ../Makefile.incl

SCIDIRS = zcalelm objects interp functions gtk2 \
	system graphics menus gtksci gzvt2 modules gobjects types-test \
	librand libcdf  libfft libinteg libapprox lapack \
	libscicos    libsndfile libumfpack

ALLSCIDIRS = zcalelm objects interp functions gtk2 \
	system graphics menus gtksci gzvt2 modules gobjects types-test \
	librand libcdf libmaxplus libfft libinteg libapprox lapack \
	libblas liblapack libscicos libxdr libsndfile libumfpack

SUBDIRS = $(DLD_SUBDIR) $(SCIDIRS) 
ALLSUBDIRS = $(DLD_SUBDIR) $(ALLSCIDIRS) 


all::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		(cd $$i ; echo "making all in src/$$i..."; \
		$(MAKE) $(MFLAGS) all); \
	done

distclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(ALLSCIDIRS) ;\
	do \
		(cd $$i ; echo "making distclean in src/$$i..."; \
		$(MAKE) $(MFLAGS)  distclean); \
	done

clean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(ALLSUBDIRS) ;\
	do \
		(cd $$i ; echo "making clean in src/$$i..."; \
		$(MAKE) $(MFLAGS) clean); \
	done

f2cclean::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(ALLSUBDIRS) ;\
	do \
		(cd $$i ; echo "making f2cclean in src/$$i..."; \
		$(MAKE) $(MFLAGS) f2cclean); \
	done
