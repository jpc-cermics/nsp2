
SCIDIR=.
include Makefile.incl.mak 

all ::  src-pvm  bin/scilex.exe macros wless imp intersci

!IF "$(DLPVM)" == "YES"
src-pvm :: src pvm 
!ELSE 
src-pvm :: src 
!ENDIF

pvm	:
	Makesubdirs.bat pvm

src:: 
	Makesubdirs.bat dumpexts
	Makesubdirs.bat src
	Makesubdirs.bat def

macros::
	Makesubdirs.bat macros

wless::
	Makesubdirs.bat wless

imp::
	Makesubdirs.bat imp

intersci::
	Makesubdirs.bat intersci

clean::
	Makesubdirs.bat src-clean
	Makesubdirs.bat macros-clean

distclean::
	Makesubdirs.bat src-distclean 
	Makesubdirs.bat macros-distclean 

# win32 
LIBRSCI = libs/parse.lib libs/func.lib libs/mexlib.lib \
	libs/System.lib libs/System1.lib libs/nsp.lib \
	libs/xdr.lib libs/zcalelm.lib libs/zblas.lib libs/libf2c.lib

LIBR = $(XAW_LOCAL_LIB) $(LIBRSCI) $(DLDLIB)

DEFAULTS = src/Parse/lsci.obj 

FFLAGS = $(FC_OPTIONS)
CFLAGS = $(CC_OPTIONS)
#RESOURCES= src/wsci/Rscilab.res 

bin/LibScilab.dll: $(LIBRSCI)
	@echo Creation of $*.dll and import lib $*.lib
	@$(LINKER) $(LINKER_FLAGS) $(RESOURCES)  $(LIBR) $(XLIBS) \
	 /dll /out:"$*.dll" /implib:"$*.lib" /def:"$*.def" 

bin/scilex.exe : bin/LibScilab.dll
	@$(LINKER) $(LINKER_FLAGS) -OUT:"$*.exe"  $(RESOURCES) \
	$(DEFAULTS) bin/LibScilab.lib $(XLIBS) 



