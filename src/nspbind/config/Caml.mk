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

# This Makefile defines generic rules to compile various kinds of Caml source
# files and to automatically handle their dependencies.

# This Makefile also defines two entries:
# - clean (to clean up the compiled files)
# - depend (to recompute the dependency order).

# This Makefile should be included at the end of the Makefile that handles a
# set of Caml files (to build a library or an application).
# Simpy write at the end of your Makefile:
# include path_to_Caml.mk/Caml.mk

.PHONY: default all bin byt
.PHONY: clean clean-all clean-spurious clean-generated
.PHONY: depend before-depend common-depend local-depend
.PHONY: test

# Main targets
all: byt bin

# Generic clean up
EXT_TO_CLEAN=.cm* .o .a .annot .output .obj .lib

clean::
	@for EXT in $(EXT_TO_CLEAN); do\
	  find . -name "*$$EXT" -exec $(RM) "{}" \; ;\
	done

SPURIOUS_EXT_TO_CLEAN=*~ .*~ a.out .\#*

clean-spurious:
	@for EXT in $(SPURIOUS_EXT_TO_CLEAN); do\
	  find . -name "$$EXT" -exec $(RM) "{}" \; ;\
	done

clean-all::
	@$(MAKE) clean-spurious &&\
	$(MAKE) clean-generated &&\
	$(MAKE) clean &&\
	$(MAKE) depend

clean-generated:
	@$(RM) $(CAML_GENERATED_FILES)

# Rebuilding dependencies
before-depend:

common-depend:
	@echo "Computing dependencies" &&\
	$(CAML_DEP) $(CAML_INCLUDES) $(CAML_FILES) > .depend

local-depend:

depend:: before-depend $(CAML_FILES) common-depend local-depend

# Compilation rules
.SUFFIXES:
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mll .mly .mlin .mliin

.ml.cmo:
	@echo "Byte compiling $<" && \
	$(CAML_BYT) -c $<

.mli.cmi:
	@echo "Compiling $<" && \
	$(CAML_BYT) -c $<

.ml.cmx:
	@echo "Binary compiling $<" && \
	$(CAML_BIN) -c $<

.mll.ml:
	@echo "Compiling lexer $<" && \
	$(CAML_LEX) $<

.mly.ml:
	@echo "Compiling parser $<" && \
	$(CAML_YAC) $<

.mly.mli:
	@echo "Compiling parser $<" && \
	$(CAML_YAC) $<

.mlin.ml:
	@echo "Expanding $<" && \
	$(RM) $@ && \
	$(HTMLC) -i $< -o $@

.mliin.mli:
	@echo "Expanding $<" && \
	$(RM) $@ && \
	$(HTMLC) -i $< -o $@

banner: banner.in
	@echo "Expanding banner.in" && \
	$(RM) banner && \
	$(HTMLC) -i banner.in -o banner

include .depend
