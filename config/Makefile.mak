FFLAGS = $(FC_OPTIONS)
CFLAGS = $(CC_OPTIONS)
RESOURCES= src/wsci/Rscilab.res 


bin/LibScilab.dll: $(DEFAULTS) $(LIBRSCI)
	@echo Creation of $*.dll and import lib $*.lib
	@$(LINKER) $(LINKER_FLAGS) $(RESOURCES) $(DEFAULTS) $(LIBR) $(XLIBS) \
	 /dll /out:"$*.dll" /implib:"$*.lib" /def:"$*.def" 

bin/scilex.exe : bin/LibScilab.dll
	@$(LINKER) $(LINKER_FLAGS) -OUT:"$*.exe"  $(RESOURCES) \
	src/f2c/libf2c/main.obj bin/LibScilab.lib $(XLIBS) 

