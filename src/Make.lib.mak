# CFLAGS IS CHANGED LOCALLY FOR EACH SUBDIR 
# CFLAGS = $(CC_OPTIONS) 
# or 
# CFLAGS = $(CC_OPTIONS) $(XFLAGS)

FFLAGS = $(FC_OPTIONS)

OBJS = $(OBJSC) $(OBJSF)

world: all

all:: $(SCIDIR1)\libs\$(LIBRARY)

# standard library 

$(SCIDIR1)\libs\$(LIBRARY): $(OBJS)
	@echo Creation of $@
	@$(LINKER) /lib /out:"$@"  $(OBJS)
	@echo Done

# A partial def file (without headers) for the library 
# to build a scilex.def at the end 

DEF=$(LIBRARY:.lib=.def)

all:: $(SCIDIR1)\libs\$(DEF)

$(SCIDIR1)\libs\$(DEF) : $(OBJS) 
	@echo Creation of $@
	@$(SCIDIR1)\bin\dumpexts.exe -o $@ -n scilex.dll $(OBJS)

# Not used up to now 

DLL=$(LIBRARY:.lib=.dll)

dll:: $(SCIDIR1)\libs\$(DLL)

$(SCIDIR1)\libs\$(DLL) : $(OBJS) 
	@echo Creation of dll $@ and import lib 
	$(LINKER) $(OBJS) $(DLL_LIBS) $(GUILIBS) /dll /out:"$@" /implib:"$*.ilib" $(LINKER_FLAGS) /MAP:"$*.map"  /def:"$*.def"

# /PDB:NONE

clean:: 
	@del *.obj 

cleanC :
	$(RM) $(OBJSC)

cleanF :
	$(RM) $(OBJSCF)

distclean::
	@del *.obj 
	@del $(LIBRARY)


