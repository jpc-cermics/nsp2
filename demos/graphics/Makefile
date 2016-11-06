SHELL = /bin/sh

include ../../Path.incl
include ../../Makefile.incl

SUBDIRS = libdemo
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

