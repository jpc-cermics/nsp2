
include ../Makefile.incl.mak 

all :: ..\bin\LibScilab.def 

FILES = AAbase.def + System.def + System1.def + func.def + libf2c.def + nsp.def \
	+ parse.def + xdr.def + zblas.def + zcalelm.def \
	+ mexlib.def 

!IF "$(DTK)" == "-DWITH_TK"
FILES1 = $(FILES) + tksci.def 
!ELSE 
FILES1 = $(FILES)
!ENDIF

!IF "$(DPVM)" == "-DWITH_PVM"
FILES2 = $(FILES1)  + pvm.def 
!ELSE 
FILES2 = $(FILES1)
!ENDIF


..\bin\LibScilab.def :  *.def 
	@echo "Creation of $*.def"
	@copy $(FILES1)  $*.def  > null

