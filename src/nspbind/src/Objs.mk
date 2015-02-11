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

# The list of all the Caml compiled objects to make in this project.
# Each source directory has its specific make variable associated
# with the relevant list of object files.
#
# This file is shared and supposed to be included by
# each make file that needs to link and build a Caml executable.
# See below for a template of make file.

# Inherited from Main_DIR/Makefile
#CONFIG_DIR=$(MAIN_DIR)/config

SIMPORT_INTERNAL_LIBS_DIR=$(SRC_DIR)/internal_libs/lib/ocaml
OCAML_XML_LIB_DIR=$(SIMPORT_INTERNAL_LIBS_DIR)/ocaml-xml
CAMLZIP_LIB_DIR=$(SIMPORT_INTERNAL_LIBS_DIR)/camlzip
BASICS_DIR=$(SRC_DIR)/basics
CONFIGURATION_DIR=$(SRC_DIR)/configuration

CODEGEN_PARSING_DIR=$(SRC_DIR)/parsing
PRINTING_DIR=$(SRC_DIR)/printing

COMPILER_DIR=$(SRC_DIR)/compiler
DRIVER_DIR=$(SRC_DIR)/driver

SIMPORT_SUB_DIRS=\
 $(BASICS_DIR)\
 $(CONFIGURATION_DIR)\
 $(EMBEDDED_DIR)\
 $(CODEGEN_PARSING_DIR)\
 $(PRINTING_DIR)\
 $(COMPILER_DIR)\
 $(DRIVER_DIR)\

SIMPORT_COMMON_CAML_INCLUDES=\
 -I $(BASICS_DIR)\
 -I $(CONFIGURATION_DIR)\
 -I $(CODEGEN_PARSING_DIR)\
 -I $(PRINTING_DIR)\

SIMPORT_TRAILING_CAML_INCLUDES=\
 -I $(COMPILER_DIR)\
 -I $(DRIVER_DIR)\

SIMPORT_CAML_INCLUDES=\
 $(SIMPORT_COMMON_CAML_INCLUDES)\
 $(SIMPORT_TRAILING_CAML_INCLUDES)\

SIMPORT_PRO_CAML_INCLUDES=\
 $(SIMPORT_COMMON_CAML_INCLUDES)\
 $(SIMPORT_TRAILING_CAML_INCLUDES)\

# External libraries

# EXTERNAL_LIBRARIES=unix
SIMPORT_EXTERNAL_LIBRARIES_BYT_OBJS=str.cma unix.cma
SIMPORT_EXTERNAL_LIBRARIES_BIN_OBJS=$(SIMPORT_EXTERNAL_LIBRARIES_BYT_OBJS:.cma=.cmxa)

# Internal libraries

# Directory src/basics

BASICS_ML_ONLY_FILES=\
 $(BASICS_DIR)/ident_print.ml\
 $(BASICS_DIR)/path_print.ml\

BASICS_ML_FILES=\
 $(BASICS_DIR)/lib_print.ml\
 $(BASICS_DIR)/location.ml\
 $(BASICS_DIR)/location_print.ml\
 $(BASICS_DIR)/path.ml\
 $(BASICS_ML_ONLY_FILES)\

BASICS_BYT_OBJS=\
 $(BASICS_ML_FILES:.ml=.cmo)\

BASICS_MLI_FILES=\
 $(BASICS_DIR)/lib_print.mli\
 $(BASICS_DIR)/location.mli\
 $(BASICS_DIR)/location_print.mli\
 $(BASICS_DIR)/ident.mli\
 $(BASICS_DIR)/path.mli\

BASICS_CAML_FILES=\
 $(BASICS_MLI_FILES)\
 $(BASICS_ML_FILES)\

BASICS_CAML_GENERATED_FILES=\
 $(BASICS_DIR)/ident_print.ml\
 $(BASICS_DIR)/path_print.ml\

BASICS_BYT_FILES_TO_INSTALL=\
 $(BASICS_MLI_FILES)\
 $(BASICS_MLI_FILES:.mli=.cmi)\
 $(BASICS_ML_ONLY_FILES:.ml=.cmi)\

BASICS_BIN_FILES_TO_INSTALL=\
 $(BASICS_ML_FILES:.ml=.cmx)\

$(BASICS_DIR)/ident_print.ml: $(BASICS_DIR)/ident.mli
	$(CAML_GEN) $(BASICS_DIR)/ident.mli

$(BASICS_DIR)/path_print.ml: $(BASICS_DIR)/path.mli
	$(CAML_GEN) $(BASICS_DIR)/path.mli

# Directory src/configuration

CONFIGURATION_BYT_OBJS=\
 $(CONFIGURATION_DIR)/configuration.cmo\
 $(CONFIGURATION_DIR)/codegen_configuration_print.cmo\
 $(CONFIGURATION_DIR)/say.cmo\
 $(CONFIGURATION_DIR)/arguments.cmo\

CONFIGURATION_CAML_FILES=\
 $(CONFIGURATION_DIR)/format_printer.mli\
 $(CONFIGURATION_DIR)/configuration.mli\
 $(CONFIGURATION_DIR)/codegen_configuration.mli\
 $(CONFIGURATION_DIR)/say.mli\
 $(CONFIGURATION_DIR)/arguments.mli\
 $(CONFIGURATION_BYT_OBJS:.cmo=.ml)\

CONFIGURATION_CAML_GENERATED_FILES=\
 $(CONFIGURATION_DIR)/configuration.ml\
 $(CONFIGURATION_DIR)/codegen_configuration_print.ml\

$(CONFIGURATION_DIR)/configuration.ml:\
 $(CONFIG_DIR)/simport.env\

# Refrain from using $< in this rule: this is a non portable gnu-ism.
$(CONFIGURATION_DIR)/codegen_configuration_print.ml:\
 $(CONFIGURATION_DIR)/codegen_configuration.mli
	$(CAML_GEN) $(CONFIGURATION_DIR)/codegen_configuration.mli

# Directory src/matlab

# Directory src/parsing

MATLAB_LEX_BYT_OBJS=\
 $(CODEGEN_PARSING_DIR)/mtlb_location.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_lexer.cmo\
 $(CODEGEN_PARSING_DIR)/lisp_lexer.cmo\

MATLAB_LEX_CAML_FILES=\
 $(CODEGEN_PARSING_DIR)/mtlb_location.mli\
 $(CODEGEN_PARSING_DIR)/mtlb_lexer.mll\
 $(CODEGEN_PARSING_DIR)/lisp_lexer.mll\
 $(MATLAB_LEX_BYT_OBJS:.cmo=.ml)\

MATLAB_LEX_CAML_GENERATED_FILES=\
 $(CODEGEN_PARSING_DIR)/mtlb_lexer.ml\
 $(CODEGEN_PARSING_DIR)/lisp_lexer.ml\

MATLAB_PARSE_BYT_OBJS=\
 $(CODEGEN_PARSING_DIR)/lisp_ast.cmo\
 $(CODEGEN_PARSING_DIR)/ast_node_utils.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_syntaxerr.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_ast_funs.cmo\
 $(CODEGEN_PARSING_DIR)/lexing_print.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_location_print.cmo\
 $(CODEGEN_PARSING_DIR)/ast_node_print.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_ast_print.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_parser.cmo\
 $(CODEGEN_PARSING_DIR)/lisp_parser.cmo\
 $(CODEGEN_PARSING_DIR)/mtlb_to_ast.cmo\
 $(CODEGEN_PARSING_DIR)/lisp_to_ast.cmo\

MATLAB_PARSE_CAML_FILES=\
 $(CODEGEN_PARSING_DIR)/ast_node.mli\
 $(CODEGEN_PARSING_DIR)/ast_node_utils.mli\
 $(CODEGEN_PARSING_DIR)/mtlb_ast.mli\
 $(CODEGEN_PARSING_DIR)/mtlb_syntaxerr.mli\
 $(CODEGEN_PARSING_DIR)/mtlb_parser.mly\
 $(CODEGEN_PARSING_DIR)/mtlb_parser.mli\
 $(CODEGEN_PARSING_DIR)/lisp_parser.mly\
 $(CODEGEN_PARSING_DIR)/lisp_parser.mli\
 $(CODEGEN_PARSING_DIR)/mtlb_to_ast.mli\
 $(CODEGEN_PARSING_DIR)/lisp_to_ast.mli\
 $(MATLAB_PARSE_BYT_OBJS:.cmo=.ml)\

MATLAB_PARSE_CAML_GENERATED_FILES=\
 $(CODEGEN_PARSING_DIR)/mtlb_location_print.ml\
 $(CODEGEN_PARSING_DIR)/ast_node_print.ml\
 $(CODEGEN_PARSING_DIR)/mtlb_ast_print.ml\
 $(CODEGEN_PARSING_DIR)/mtlb_parser.mli\
 $(CODEGEN_PARSING_DIR)/mtlb_parser.ml\
 $(CODEGEN_PARSING_DIR)/lisp_parser.mli\
 $(CODEGEN_PARSING_DIR)/lisp_parser.ml\

# Refrain from using $< in this rule: this is a non portable gnu-ism.
$(CODEGEN_PARSING_DIR)/mtlb_location_print.ml:\
  $(CODEGEN_PARSING_DIR)/mtlb_location.mli
	$(CAML_GEN) $(CODEGEN_PARSING_DIR)/mtlb_location.mli

# Refrain from using $< in this rule: this is a non portable gnu-ism.
$(CODEGEN_PARSING_DIR)/ast_node_print.ml: $(CODEGEN_PARSING_DIR)/ast_node.mli
	$(CAML_GEN) $(CODEGEN_PARSING_DIR)/ast_node.mli

# Refrain from using $< in this rule: this is a non portable gnu-ism.
$(CODEGEN_PARSING_DIR)/mtlb_ast_print.ml: $(CODEGEN_PARSING_DIR)/mtlb_ast.mli
	$(CAML_GEN) $(CODEGEN_PARSING_DIR)/mtlb_ast.mli

# Directory src/matlab/printing

PRINTING_BYT_OBJS=\
 $(PRINTING_DIR)/mtlb_ppf.cmo\
 $(PRINTING_DIR)/overrides.cmo\
 $(PRINTING_DIR)/file.cmo\
 $(PRINTING_DIR)/stringarg.cmo\
 $(PRINTING_DIR)/definitions.cmo\
 $(PRINTING_DIR)/build.cmo\
 $(PRINTING_DIR)/genheaders.cmo\
 $(PRINTING_DIR)/genfunctions.cmo\
 $(PRINTING_DIR)/genmethods.cmo\
 $(PRINTING_DIR)/gengetset.cmo\
 $(PRINTING_DIR)/codegen.cmo\
 $(PRINTING_DIR)/override_print.cmo\

# $(PRINTING_DIR)/codegen.cmo\
#

PRINTING_CAML_FILES=\
 $(PRINTING_DIR)/genheaders.mli\
 $(PRINTING_DIR)/mtlb_ppf.mli\
 $(PRINTING_DIR)/file.mli\
 $(PRINTING_DIR)/stringarg.mli\
 $(PRINTING_DIR)/definitions.mli\
 $(PRINTING_DIR)/genfunctions.mli\
 $(PRINTING_DIR)/genmethods.mli\
 $(PRINTING_BYT_OBJS:.cmo=.ml)\

PRINTING_CAML_GENERATED_FILES=\

MATLAB_BYT_OBJS=\
 $(MATLAB_LEX_BYT_OBJS)\
 $(MATLAB_PARSE_BYT_OBJS)\
 $(PRINTING_BYT_OBJS)\

MATLAB_CAML_FILES=\
 $(MATLAB_LEX_CAML_FILES)\
 $(MATLAB_PARSE_CAML_FILES)\
 $(PRINTING_CAML_FILES)\

MATLAB_CAML_GENERATED_FILES=\
 $(MATLAB_LEX_CAML_GENERATED_FILES)\
 $(MATLAB_PARSE_CAML_GENERATED_FILES)\
 $(PRINTING_CAML_GENERATED_FILES)\

# Directory src/compiler

COMPILER_BYT_OBJS=\
 $(COMPILER_DIR)/main_gen.cmo\
 $(COMPILER_DIR)/defs_compile.cmo\

COMPILER_CAML_FILES=\
 $(COMPILER_DIR)/main_gen.mli\
 $(COMPILER_DIR)/defs_compile.mli\
 $(COMPILER_BYT_OBJS:.cmo=.ml)\

COMPILER_CAML_GENERATED_FILES=\

# Directory src/driver

DRIVER_BYT_OBJS=\
 $(DRIVER_DIR)/driver.cmo\

DRIVER_CAML_FILES=\
 $(DRIVER_BYT_OBJS:.cmo=.ml)\

DRIVER_CAML_GENERATED_FILES=\

# Directory src/

SIMPORT_COMMON_BYT_OBJS=\
 $(BASICS_BYT_OBJS)\
 $(CONFIGURATION_BYT_OBJS)\
 $(MATLAB_BYT_OBJS)\

SIMPORT_TRAILING_BYT_OBJS=\
 $(COMPILER_BYT_OBJS)\
 $(DRIVER_BYT_OBJS)\

SIMPORT_BYT_OBJS=\
 $(SIMPORT_COMMON_BYT_OBJS)\
 $(SIMPORT_TRAILING_BYT_OBJS)\

SIMPORT_PRO_BYT_OBJS=\
 $(SIMPORT_COMMON_BYT_OBJS)\
 $(SIMPORT_TRAILING_BYT_OBJS)\

SIMPORT_COMMON_CAML_FILES=\
 $(BASICS_CAML_FILES)\
 $(CONFIGURATION_CAML_FILES)\
 $(MATLAB_CAML_FILES)\

SIMPORT_TRAILING_CAML_FILES=\
 $(COMPILER_CAML_FILES)\
 $(DRIVER_CAML_FILES)\

SIMPORT_CAML_FILES=\
 $(SIMPORT_COMMON_CAML_FILES)\
 $(SIMPORT_TRAILING_CAML_FILES)\

SIMPORT_CAML_GENERATED_FILES=\
 $(BASICS_CAML_GENERATED_FILES)\
 $(CONFIGURATION_CAML_GENERATED_FILES)\
 $(MATLAB_CAML_GENERATED_FILES)\
 $(COMPILER_CAML_GENERATED_FILES)\
 $(DRIVER_CAML_GENERATED_FILES)\

SIMPORT_PRO_CAML_FILES=\
 $(SIMPORT_COMMON_CAML_FILES)\
 $(SIMPORT_TRAILING_CAML_FILES)\

# List of files to bannerize

SIMPORT_FILES_TO_BANNERIZE=\
 $(BASICS_CAML_FILES)\
 $(CONFIGURATION_CAML_FILES)\
 $(COMPILER_CAML_FILES)\
 $(DRIVER_CAML_FILES)\

CONFIG_FILES_TO_BANNERIZE=\
 $(CONFIG_DIR)/Commands.mk\
 $(CONFIG_DIR)/Caml.mk\
 $(CONFIG_DIR)/Simport.mk\
 $(CONFIG_DIR)/Config.mk.in\
 $(CONFIG_DIR)/Doc.mk\
 $(CONFIG_DIR)/Params.mk\
 $(CONFIG_DIR)/simport.env\

CONFIG_SCRIPTS_TO_BANNERIZE=\
 $(CONFIG_DIR)/configure\

# List of .mlin .mliin, etc. to enter in the list of files to bannerize
CAML_GENERATING_FILES=\

LOCAL_FILES_TO_BANNERIZE=\
 $(CONFIG_FILES_TO_BANNERIZE)\
 $(MAIN_DIR)/Makefile\
 $(SRC_DIR)/Objs.mk\
 $(CAML_GENERATING_FILES)\
 $(SIMPORT_FILES_TO_BANNERIZE)\

LOCAL_SCRIPTS_TO_BANNERIZE=\
 $(MAIN_DIR)/configure\
 $(CONFIG_SCRIPTS_TO_BANNERIZE)\

MATLAB_FILES_TO_BANNERIZE=\
 $(MATLAB_CAML_FILES)\

MATLAB_SCRIPTS_TO_BANNERIZE=\

# Refrain from using $< in this rule: this is a non portable gnu-ism.
$(CONFIG_DIR)/banner: $(CONFIG_DIR)/banner.in
	$(HTMLC) -f $(CONFIG_DIR)/banner.in -o $@

# Refrain from using $< in this rule: this is a non portable gnu-ism.
$(MATLAB_DIR)/banner: $(MATLAB_DIR)/banner.in
	$(HTMLC) -f $(MATLAB_DIR)/banner.in -o $@

