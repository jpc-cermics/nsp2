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

# $Id$ #

# This Makefile should be included at the end of a Makefile that handles a
# set of Simulink files (to translate them to nsp or scicoslab).
# Simply write at the end of your Makefile:
# include <path_to_Simport.mk>/Simport.mk

.SUFFIXES: .mdl .mdlo .mdli .mdlio .mtlb .m .oct .sci .nsp .hml .slx
.SUFFIXES: .sime .cose

.mdl.mdlo:
	$(SIMPORT) $<

.mdli.mdlio:
	$(SIMPORT) $<

.mdl.sci:
	$(SIMPORT) -tl scicoslab $<

.mdl.nsp:
	$(SIMPORT) -tl nsp $<

.mdl.hml:
	$(SIMPORT_PRO) -tl hypermath $<

.slx.mdlo:
	$(SIMPORT) $<

.slx.sci:
	$(SIMPORT) -tl scicoslab $<

.slx.nsp:
	$(SIMPORT) -tl nsp $<

.slx.hml:
	$(SIMPORT_PRO) -tl hypermath $<

.mtlb.sci:
	$(SIMPORT) -target-language scicoslab $<

.mtlb.nsp:
	$(SIMPORT) -target-language nsp $<

.mtlb.oct:
	$(SIMPORT) -target-language octave $<

.mtlb.m:
	$(SIMPORT) -target-language matlab $<

.mtlb.hml:
	$(SIMPORT_PRO) -target-language hypermath $<

.sime.cose:
	$(SIMPORT) -target-language e-scicos $<
