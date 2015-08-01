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
#NSPBIND_EXTERNAL_LIBRARIES_BYT_OBJS=unix.cma
#NSPBIND_EXTERNAL_LIBRARIES_BIN_OBJS=$(NSPBIND_EXTERNAL_LIBRARIES_BYT_OBJS:.cma=.cmxa)
#NSPBIND_INTERNAL_LIBS_DIR=libdir
#NSPBIND_INTERNAL_LIBRARIES_BYT_OBJS (in fact empty)
#NSPBIND_INTERNAL_LIBRARIES_BIN_OBJS (in fact empty)
#NSPBIND_BYT_OBJS=...

SCRIPTS_DIR=$(SRC_DIR)/scripts

# The default target.
default: nspbind

# The list of object files to build.
include $(SRC_DIR)/Objs.mk

AUTO_DOC_OBJS=$(AUTO_DOC_DIR)/index.html

# The list of directories where Identity indication can be written
IDENTITY_DIRS=\
 $(MAIN_DIR)/lib\
 $(MAIN_DIR)/share\
 $(MAIN_DIR)/config\
 $(SRC_DIR)/configuration\
 $(SRC_DIR)/compiler\
 $(SRC_DIR)/driver\

CAML_FILES=$(NSPBIND_CAML_FILES)

CAML_INCLUDES=$(NSPBIND_CAML_INCLUDES)

CAML_GENERATED_FILES=$(NSPBIND_CAML_GENERATED_FILES)

# Simport pervasives file

NSPBIND_COMMAND=$(BIN_DIR)/nspbind

# Simport linking object files
NSPBIND_LINK_BYT_OBJS=\
 $(NSPBIND_EXTERNAL_LIBRARIES_BYT_OBJS)\
 $(NSPBIND_INTERNAL_LIBRARIES_BYT_OBJS)\
 $(NSPBIND_BYT_OBJS)\

NSPBIND_BIN_OBJS=$(NSPBIND_BYT_OBJS:.cmo=.cmx)\

NSPBIND_LINK_BIN_OBJS=\
 $(NSPBIND_EXTERNAL_LIBRARIES_BIN_OBJS)\
 $(NSPBIND_INTERNAL_LIBRARIES_BIN_OBJS)\
 $(NSPBIND_BIN_OBJS)\

.PHONY: nspbind exe_byt exe_bin info identity ignore
.PHONY: bannerize bannerize-local bannerize-matlab bannerize-slx
.PHONY: test test-no-warnings test-nw
.PHONY: autodoc showdoc showman update

# The regular version

nspbind: $(NSPBIND_COMMAND)

$(NSPBIND_COMMAND): $(NSPBIND_INTERNAL_LIBRARIES) $(NSPBIND_CAML_FILES) $(NSPBIND_BYT_OBJS) $(NSPBIND_BIN_OBJS)
	@echo "Binary linking program $(NSPBIND_COMMAND)" && \
	$(CAML_BIN) -o $(NSPBIND_COMMAND) $(NSPBIND_LINK_BIN_OBJS)

test:
	(cd test_data && $(MAKE) all)

test-no-warnings:
	(cd test_data && $(MAKE) all NSPBIND_FLAGS=-no-warnings)

test-nw: test-no-warnings

distclean: clean

clean::
	@$(RM) $(EXE) $(EXE).b* &&\
	$(RM) $(NSPBIND_OWN_LIBRARY_OBJS)

# We need internal libraries to be compiled to compute dependencies
before-depend: $(NSPBIND_INTERNAL_LIBRARIES)

local-depend:
	@echo "Computing additional dependencies" &&\
	$(CAML_DEP) $(MDL_GENR_SCICOS_PRO_BLOCK_CAML_FILES) >> .depend

depend:: local-depend
	@echo "Computing internal libraries dependencies"

info:
	@echo &&\
	echo "CAML_FILES are" &&\
	echo $(CAML_FILES) &&\
	echo &&\
	echo "NSPBIND_BYT_OBJS are" &&\
	echo $(NSPBIND_BYT_OBJS) &&\
	echo "NSPBIND_LINK_BYT_OBJS are" &&\
	echo $(NSPBIND_LINK_BYT_OBJS) &&\
	echo &&\
	echo "NSPBIND_BIN_OBJS are" &&\
	echo $(NSPBIND_BIN_OBJS) &&\
	echo "NSPBIND_LINK_BIN_OBJS are" &&\
	echo $(NSPBIND_LINK_BIN_OBJS) &&\
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
	echo "NSPBIND command is" &&\
	echo $(NSPBIND) &&\
	echo "NSPBIND_OWN_LIBRARY_OBJS are" &&\
	echo $(NSPBIND_OWN_LIBRARY_OBJS) &&\
	echo "NSPBIND_PRO_OWN_LIBRARY_OBJS are" &&\
	echo $(NSPBIND_PRO_OWN_LIBRARY_OBJS) &&\
	echo

autodoc: $(AUTO_DOC_OBJS)

$(AUTO_DOC_OBJS): $(CAML_FILES) $(NSPBIND_OWN_LIBRARY_OBJS)
	HTMLDIR=$(AUTO_DOC_DIR) $(SCRIPTS_DIR)/generate_htmldoc.sh

showdoc: $(AUTO_DOC_OBJS)
	$(BROWSER) $(AUTO_DOC_OBJS) &

showman: $(DOC_SRC_DIR)/nspbind.1
	$(MAN) $(DOC_SRC_DIR)/nspbind.1 | $(MORE)

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
#  $(NSPBIND_FILES_TO_BANNERIZE)\
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
	ocamlc -I src/basics -I src/configuration \
	-I src/matlab/printing -I src/matlab/parsing \
	-I src/printing \
	-i $(MLI) > $(MLI)i

etags:
	etags  `find . -name '*.ml*' `
