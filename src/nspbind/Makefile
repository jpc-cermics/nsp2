#######################################################################
#                                                                     #
#                               Simport                               #
#                                                                     #
#                   Pierre Weis, INRIA Rocquencourt                   #
#                                                                     #
#  Copyright 2010-2014,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  All rights reserved.                                               #
#                                                                     #
#  This file is distributed under the terms of the BSD License.       #
#                                                                     #
#######################################################################

# $Id$

# The root of our source hierarchy: ScicosLab sources should reside in
# $(SRC_ROOT_DIR)/scicoslab.
SRC_ROOT_DIR=..

# Defining the minmum to access generic include Makefiles.
MAIN_DIR=.
CONFIG_DIR=$(MAIN_DIR)/config

include $(CONFIG_DIR)/Config.mk

# Inherited from $(CONFIG_DIR)/Config.mk
#BIN_DIR=$(MAIN_DIR)/bin
#LIB_DIR=$(MAIN_DIR)/lib
#SRC_DIR=$(MAIN_DIR)/src
#AUTO_DOC_DIR=$(MAIN_DIR)/autodoc/html
#DOC_SRC_DIR=$(MAIN_DIR)/doc_src

# Inherited from $(SRC_DIR)/Objs.mk
#SIMPORT_INTERNAL_LIBS_DIR=$(SRC_DIR)/internal_libs/lib/ocaml
#OCAML_XML_LIB_DIR=$(SIMPORT_INTERNAL_LIBS_DIR)/ocaml-xml
#OCAML_XML_DIR=$(SLX_FILE_FORMAT_PARSING_DIR)/ocaml-xml
#CAMLZIP_LIB_DIR=$(SIMPORT_INTERNAL_LIBS_DIR)/camlzip
#CAMLZIP_DIR=$(SLX_FILE_FORMAT_PARSING_DIR)/camlzip
#EXTERNAL_LIBRARIES_BYT_OBJS=unix.cma
#EXTERNAL_LIBRARIES_BIN_OBJS=$(SIMPORT_EXTERNAL_LIBRARIES_BYT_OBJS:.cma=.cmxa)
#SIMPORT_INTERNAL_LIBRARIES=$(SIMPORT_INTERNAL_LIBRARIES_BYT_OBJS) $(SIMPORT_INTERNAL_LIBRARIES_BIN_OBJS)
#SIMPORT_BYT_OBJS=...

SCRIPTS_DIR=$(SRC_DIR)/scripts

SRC_MDL_DIR=$(SRC_DIR)/mdl
SRC_MDL_FILE_FORMAT_DIR=$(SRC_DIR)/mdl_file_format
SRC_SLX_FILE_FORMAT_DIR=$(SRC_DIR)/slx_file_format

# The default target.
default: simport

# The list of object files to build.
include $(SRC_DIR)/Objs.mk

AUTO_DOC_OBJS=$(AUTO_DOC_DIR)/index.html

# The list of directories where Identity indication can be written
IDENTITY_DIRS=\
 $(MAIN_DIR)/lib\
 $(MAIN_DIR)/share\
 $(MAIN_DIR)/config\
 $(SRC_DIR)/configuration\
 $(SRC_DIR)/simulink_embedded\
 $(SRC_DIR)/matlab\
 $(SRC_MDL_DIR)/ast\
 $(SRC_MDL_FILE_FORMAT_DIR)/parsing\
 $(SRC_MDL_DIR)/dast\
 $(SRC_MDL_DIR)/inheritance\
 $(SRC_MDL_DIR)/split\
 $(SRC_MDL_DIR)/translation\
 $(SRC_MDL_DIR)/genr_block/common\
 $(SRC_MDL_DIR/genr_block/genr_scicos_block\
 $(SRC_MDL_DIR)/genr_block/genr_scicos_pro_block\
 $(SRC_SLX_FILE_FORMAT_DIR)/parsing\
 $(SRC_SLX_FILE_FORMAT_DIR)/dast\
 $(SRC_SLX_FILE_FORMAT_DIR)/translating\
 $(SRC_DIR)/compiler\
 $(SRC_DIR)/driver\

CAML_FILES=$(SIMPORT_CAML_FILES)

CAML_INCLUDES=$(SIMPORT_CAML_INCLUDES)

CAML_GENERATED_FILES=$(SIMPORT_CAML_GENERATED_FILES)

# Simport pervasives file

SIMPORT_COMMAND=$(BIN_DIR)/simport
SIMPORT_PRO_COMMAND=$(BIN_DIR)/simport_pro

# Simport linking object files
SIMPORT_LINK_BYT_OBJS=\
 $(SIMPORT_EXTERNAL_LIBRARIES_BYT_OBJS)\
 $(SIMPORT_INTERNAL_LIBRARIES_BYT_OBJS)\
 $(SIMPORT_BYT_OBJS)\

SIMPORT_BIN_OBJS=$(SIMPORT_BYT_OBJS:.cmo=.cmx)\

SIMPORT_LINK_BIN_OBJS=\
 $(SIMPORT_EXTERNAL_LIBRARIES_BIN_OBJS)\
 $(SIMPORT_INTERNAL_LIBRARIES_BIN_OBJS)\
 $(SIMPORT_BIN_OBJS)\

# Simport Pro linking object files
SIMPORT_PRO_LINK_BYT_OBJS=\
 $(SIMPORT_EXTERNAL_LIBRARIES_BYT_OBJS)\
 $(SIMPORT_INTERNAL_LIBRARIES_BYT_OBJS)\
 $(SIMPORT_PRO_BYT_OBJS)\

SIMPORT_PRO_BIN_OBJS=$(SIMPORT_PRO_BYT_OBJS:.cmo=.cmx)

SIMPORT_PRO_LINK_BIN_OBJS=\
 $(SIMPORT_EXTERNAL_LIBRARIES_BIN_OBJS)\
 $(SIMPORT_INTERNAL_LIBRARIES_BIN_OBJS)\
 $(SIMPORT_PRO_BIN_OBJS)\

# Executables to build
EXE=$(BIN_DIR)/simport
EXE_PRO=$(BIN_DIR)/simport_pro

.PHONY: simport exe_byt exe_bin info identity ignore
.PHONY: bannerize bannerize-local bannerize-matlab bannerize-slx
.PHONY: test test-no-warnings test-nw
.PHONY: test_pro test_pro-no-warnings test_pro-nw
.PHONY: autodoc showdoc showman update
.PHONY: simport_pro exe_pro_byt exe_pro_bin

# The regular version
simport: $(SIMPORT_INTERNAL_LIBRARIES)
	$(MAKE)\
	  CAML_INCLUDES="$(SIMPORT_CAML_INCLUDES)"\
	  SIMPORT=$(SIMPORT_COMMAND)\
	  byt bin

byt: $(SIMPORT_CAML_FILES) exe_byt 

bin: $(SIMPORT_CAML_FILES) exe_bin 

exe_byt: $(EXE).byt
	$(CPC) $(EXE).byt $(EXE)

exe_bin: $(EXE).bin
	$(CPC) $(EXE).bin $(EXE)

$(EXE).byt: $(SIMPORT_BYT_OBJS)
	@echo "Byte linking program $(EXE).byt" && \
	$(CAML_BYT) -o $(EXE).byt $(SIMPORT_LINK_BYT_OBJS)

$(EXE).bin: $(SIMPORT_BIN_OBJS)
	@echo "Binary linking program $(EXE).bin" && \
	$(CAML_BIN) -o $(EXE).bin $(SIMPORT_LINK_BIN_OBJS)

# The pro version
simport_pro: $(SIMPORT_INTERNAL_LIBRARIES)
	$(MAKE)\
	  CAML_INCLUDES="$(SIMPORT_PRO_CAML_INCLUDES)"\
	  SIMPORT=$(SIMPORT_PRO_COMMAND)\
	  byt_pro bin_pro

byt_pro: $(SIMPORT_PRO_CAML_FILES) exe_pro_byt $(SIMPORT_PRO_OWN_LIBRARY_OBJS)

bin_pro: $(SIMPORT_PRO_CAML_FILES) exe_pro_bin $(SIMPORT_PRO_OWN_LIBRARY_OBJS)

exe_pro_byt: $(EXE_PRO).byt
	$(CPC) $(EXE_PRO).byt $(EXE_PRO)

exe_pro_bin: $(EXE_PRO).bin
	$(CPC) $(EXE_PRO).bin $(EXE_PRO)

$(EXE_PRO).byt: $(SIMPORT_PRO_BYT_OBJS)
	@echo "Byte linking program $(EXE_PRO).byt" && \
	$(CAML_BYT) -o $(EXE_PRO).byt $(SIMPORT_PRO_LINK_BYT_OBJS)

$(EXE_PRO).bin: $(SIMPORT_PRO_BIN_OBJS)
	@echo "Binary linking program $(EXE_PRO).bin" && \
	$(CAML_BIN) -o $(EXE_PRO).bin $(SIMPORT_PRO_LINK_BIN_OBJS)

test:
	(cd test_data && $(MAKE) all)

test_pro:
	(cd test_data && $(MAKE) all_pro)

test-no-warnings:
	(cd test_data && $(MAKE) all SIMPORT_FLAGS=-no-warnings)

test_pro-no-warnings:
	(cd test_data && $(MAKE) all_pro SIMPORT_PRO_FLAGS=-no-warnings)

test-nw: test-no-warnings

test_pro-nw: test_pro-no-warnings

test-nsp:
	(cd test_data && $(MAKE) compile-nsp)

clean-test:
	@(cd test_data && $(MAKE) clean-all)

clean-all::
	@$(RM) $(SIMPORT_INTERNAL_LIBS_DIR)/*/* &&\
	(cd $(CAMLZIP_DIR) && $(MAKE) clean-all) &&\
	(cd $(OCAML_XML_DIR) && $(MAKE) clean-all) &&\
	$(MAKE) clean-test

distclean: clean-all
	$(RM) $(CONFIG_DIR)/banner $(MATLAB_DIR)/banner $(SLX_DIR)/banner

clean::
	@$(RM) $(EXE) $(EXE).b* &&\
	$(RM) $(SIMPORT_OWN_LIBRARY_OBJS) $(SIMPORT_PRO_OWN_LIBRARY_OBJS)

# We need internal libraries to be compiled to compute dependencies
before-depend: $(SIMPORT_INTERNAL_LIBRARIES)

local-depend:
	@echo "Computing additional dependencies" &&\
	$(CAML_DEP) $(SIMPORT_PRO_CAML_INCLUDES)\
	  $(MDL_GENR_SCICOS_PRO_BLOCK_CAML_FILES) >> .depend

depend:: local-depend
	@echo "Computing internal libraries dependencies" 

info:
	@echo &&\
	echo "CAML_FILES are" &&\
	echo $(CAML_FILES) &&\
	echo &&\
	echo "SIMPORT_BYT_OBJS are" &&\
	echo $(SIMPORT_BYT_OBJS) &&\
	echo "SIMPORT_LINK_BYT_OBJS are" &&\
	echo $(SIMPORT_LINK_BYT_OBJS) &&\
	echo &&\
	echo "SIMPORT_BIN_OBJS are" &&\
	echo $(SIMPORT_BIN_OBJS) &&\
	echo "SIMPORT_LINK_BIN_OBJS are" &&\
	echo $(SIMPORT_LINK_BIN_OBJS) &&\
	echo &&\
	echo "CAML_INCLUDES are" &&\
	echo $(CAML_INCLUDES) &&\
	echo "CAML_PRO_INCLUDES are" &&\
	echo $(CAML_PRO_INCLUDES) &&\
	echo "CAML_BYT_INCLUDES are" &&\
	echo $(CAML_BYT_INCLUDES) &&\
	echo &&\
	echo "CAML_COMP_FLAGS are" &&\
	echo $(CAML_COMP_FLAGS) &&\
	echo "CAML_BYT_FLAGS are" &&\
	echo $(CAML_BYT_FLAGS) &&\
	echo "CAML_BIN_FLAGS are" &&\
	echo $(CAML_BIN_FLAGS) &&\
	echo &&\
	echo "CAML_BYT is" &&\
	echo $(CAML_BYT) &&\
	echo "CAML_BIN is" &&\
	echo $(CAML_BIN) &&\
	echo &&\
	echo "CAML_GENERATED_FILES are" &&\
	echo $(CAML_GENERATED_FILES) &&\
	echo &&\
	echo "SIMPORT command is" &&\
	echo $(SIMPORT) &&\
	echo "SIMPORT_OWN_LIBRARY_OBJS are" &&\
	echo $(SIMPORT_OWN_LIBRARY_OBJS) &&\
	echo "SIMPORT_PRO_OWN_LIBRARY_OBJS are" &&\
	echo $(SIMPORT_PRO_OWN_LIBRARY_OBJS) &&\
	echo

autodoc: $(AUTO_DOC_OBJS)

$(AUTO_DOC_OBJS): $(CAML_FILES) $(SIMPORT_OWN_LIBRARY_OBJS)
	HTMLDIR=$(AUTO_DOC_DIR) $(SCRIPTS_DIR)/generate_htmldoc.sh

showdoc: $(AUTO_DOC_OBJS)
	$(BROWSER) $(AUTO_DOC_OBJS) &

showman: $(DOC_SRC_DIR)/simport.1
	$(MAN) $(DOC_SRC_DIR)/simport.1 | $(MORE)

identity: clean-generated
	FILES=`find $(IDENTITY_DIRS) -type f -a -exec grep -l '$$Id:' "{}" \; | grep -v .svn` &&\
	for file in $$FILES; do\
	  svn propset svn:keywords "Id" "$$file";\
	  IER=$$? &&\
	  case $$IER in\
	    0) ;;\
	    *) echo "Cannot make $@ for file \"$$file\"" &&\
	       exit $$IER;;\
	  esac;\
	done

ignore:
	DIRS=`find . -type d | grep -v .svn` &&\
	for dir in $$DIRS; do\
	  echo "Looking for ignore file in directory $$dir" &&\
	  if test -r $$dir/.cvsignore; then\
	    (cd $$dir &&\
	     echo "Setting ignore property in dir $$dir" &&\
	     svn propset svn:ignore -F ./.cvsignore -R .);\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	    0) ;;\
	    *) echo "Cannot make $@ in directory $$dir" &&\
	       exit $$IER;;\
	  esac;\
	done

# From Objs.mk
# LOCAL_FILES_TO_BANNERIZE=\
#  $(MAIN_DIR)/Makefile $(SRC_DIR)/Objs.mk\
#  $(SIMPORT_FILES_TO_BANNERIZE)\
#  $(CONFIG_FILES_TO_BANNERIZE)\
# LOCAL_SCRIPTS_TO_BANNERIZE=\
#  $(MAIN_DIR)/configure\
#  $(CONFIG_SCRIPTS_TO_BANNERIZE)\
#
# MATLAB_DIR=$(SRC_DIR)/matlab
# MATLAB_FILES_TO_BANNERIZE=\
#  $(MATLAB_CAML_FILES)\
# MATLAB_SCRIPTS_TO_BANNERIZE=\
#
# SLX_DIR=$(SRC_DIR)/slx_file_format
# SLX_FILES_TO_BANNERIZE=\
#  $(SLX_CAML_FILES)\
# SLX_SCRIPTS_TO_BANNERIZE=\

bannerize-local: $(CONFIG_DIR)/banner $(LOCAL_FILES_TO_BANNERIZE)
	for file in $(LOCAL_FILES_TO_BANNERIZE); do\
	  if test -w $$file; then\
	    $(BH) -bf $(CONFIG_DIR)/banner -bft caml $$file;\
	  else echo "Warning: $$file is not writable.";\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	   0) ;;\
	   *) exit $$IER;;\
	  esac;\
	done;\
	for file in $(LOCAL_SCRIPTS_TO_BANNERIZE); do\
	  if test -w $$file; then\
	    $(BH) -bf $(CONFIG_DIR)/banner -bft caml -slf 1 $$file;\
	  else echo "Warning: $$file is not writable.";\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	   0) ;;\
	   *) exit $$IER;;\
	  esac;\
	done

bannerize: bannerize-local bannerize-matlab bannerize-slx

bannerize-matlab: $(MATLAB_DIR)/banner $(MATLAB_FILES_TO_BANNERIZE)
	for file in $(MATLAB_FILES_TO_BANNERIZE); do\
	  if test -w $$file; then\
	    $(BH) -bf $(MATLAB_DIR)/banner -bft caml $$file;\
	  else echo "Warning: $$file is not writable.";\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	   0) ;;\
	   *) exit $$IER;;\
	  esac;\
	done;\
	for file in $(MATLAB_SCRIPTS_TO_BANNERIZE); do\
	  if test -w $$file; then\
	    $(BH) -bf $(CONFIG_DIR)/banner -bft caml -slf 1 $$file;\
	  else echo "Warning: $$file is not writable.";\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	   0) ;;\
	   *) exit $$IER;;\
	  esac;\
	done

bannerize-slx: $(SLX_DIR)/banner $(SLX_FILES_TO_BANNERIZE)
	for file in $(SLX_FILES_TO_BANNERIZE); do\
	  if test -w $$file; then\
	    $(BH) -bf $(SLX_DIR)/banner -bft caml $$file;\
	  else echo "Warning: $$file is not writable.";\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	   0) ;;\
	   *) exit $$IER;;\
	  esac;\
	done;\
	for file in $(SLX_SCRIPTS_TO_BANNERIZE); do\
	  if test -w $$file; then\
	    $(BH) -bf $(CONFIG_DIR)/banner -bft caml -slf 1 $$file;\
	  else echo "Warning: $$file is not writable.";\
	  fi;\
	  IER=$$? &&\
	  case $$IER in\
	   0) ;;\
	   *) exit $$IER;;\
	  esac;\
	done

include $(CONFIG_DIR)/Caml.mk
include $(CONFIG_DIR)/Simport.mk
include $(CONFIG_DIR)/Doc.mk

MLI=src/matlab/printing/stringarg.ml

mli	:
	ocamlc -I src/basics -I src/configuration -I src/matlab/printing -I src/matlab/parsing \
		-i $(MLI) > $(MLI)i

etags:
	etags  `find . -name '*.ml*' `
