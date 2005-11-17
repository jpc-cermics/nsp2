#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
.SUFFIXES: .defs .c $(SUFFIXES)
SHELL = /bin/sh

SCIDIR=../../..
SCIDIR1=..\..\..

LIBRARY = nsp-cla.lib 

OBJSC = gtk.obj gdk.obj atk.obj pango.obj 
OBJSF=

include ../../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

# include ../../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

%.X : %.c 
	protoize -k -c -I../include $*.c 
	egrep -v "/usr/|nsp/" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' -e "s/ extern/extern/" > $*.X 
	rm -f $*.c.X

# on peut rajouter --prefix py$* 

PYTHON=python 
ALL=  gdk.c gtk.c pango.c atk.c gtk24.c 

all:: $(ALL)

clean :: 
	$(RM) $(ALL)

.defs.c:
	($(PYTHON) codegen/codegen.py \
	    --override $*.objverride \
	    --register ./pango-types.defs \
	    --register ./atk-types.defs \
	    --register ./gdk-types.defs \
	    --register ./gtk-types.defs \
	    --prefix $* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c

gtk24.c : gtk24.defs 
	($(PYTHON) codegen/codegen.py \
	    --override $*.objverride \
	    --register ./pango-types.defs \
	    --register ./atk-types.defs \
	    --register ./gdk-types.defs \
	    --register ./gtk-types.defs \
	    --prefix gtk $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c


gdk.c : gdk.defs  gdk.objverride gdk-types.defs 
gtk.c : gtk.defs  gtk.objverride gtk-types.defs gtk-extrafuncs.defs
gtk24.c : gtk24.defs  gtk24.objverride gtk24-types.defs gtk-extrafuncs.defs
gtkgl.c : gtkgl.defs  gtkgl.objverride 
libglade.c : libglade.defs  libglade.objverride 
atk.c : atk.defs  atk.objverride atk-types.defs 
pango.c : pango.defs  pango.objverride pango-types.defs 

