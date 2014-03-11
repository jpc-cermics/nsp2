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

.SUFFIXES: .htm .html .shtml .data .1 .man

.data.html:
	$(RM) $@
	$(HTMLC) -i $< -o $@

.shtml.html:
	$(RM) $@
	$(HTMLC) -i $< -o $@

.html.htm:
	$(RM) $@
	$(HTMLC) -i $< -o $@

.man.1:
	$(RM) $@
	$(HTMLC) -f $< -t $@

cleandir::
	$(RM) *.htm *.1
