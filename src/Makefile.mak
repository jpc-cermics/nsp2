SHELL = /bin/sh

include ../Makefile.incl.mak

SCIDIRS = Functions Newstack Parse System System1 f2c mexlib xdr zblas zcalelm 

SUBDIRS = $(XAW_LOCAL_SUBDIR) $(DLD_SUBDIR) $(SCIDIRS)

all::
	copy include/machine-h.vc include/machine.h
	Makesubdirs.bat all

!IF "$(DTK)" == "-DWITH_TK"
all::
	Makesubdirs.bat tksci
!ENDIF

!IF "$(DPVM)" == "-DWITH_PVM"
all::
	Makesubdirs.bat pvm
!ENDIF

clean::
	Makesubdirs.bat clean

distclean::
	Makesubdirs.bat distclean 


	
