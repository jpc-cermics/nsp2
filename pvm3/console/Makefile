SHELL=/bin/sh

include ../../Makefile.incl

SILENT=$(findstring s,$(MFLAGS))

PVM_ARCH= `../lib/pvmgetarch`
SUBDIRS =$(PVM_ARCH)

FLAGS=-f ../../conf/`../../lib/pvmgetarch`.def -f ../Makefile.aimk PVM_ARCH=`../../lib/pvmgetarch` \
	RANLIB=$(RANLIB) CC=$(CC) FC=$(FC)

install all clean distclean ::
	@case '${MFLAGS}' in *[ik]*) set +e;; esac; \
	for i in $(SUBDIRS) ;\
	do \
		if test -d $$i; then ok=1; else mkdir $$i; fi; \
		(cd $$i && if test "x$(SILENT)" != "xs"; then echo "making $@ in $(DIR)$$i ";fi && \
		$(MAKE) $(MFLAGS) $(FLAGS) DIR=$(DIR)$$i/ $@ ); \
	   	IER=$$? &&\
	   	case $$IER in\
	    	0) ;;\
	    	*) echo "Cannot make $@ in sub directory $$d";\
	       	  exit $$IER;;\
	   	esac;\
	done
