.SUFFIXES: .defs .c $(SUFFIXES)
SHELL = /bin/sh

SCIDIR=../../..
SCIDIR1=..\..\..

LIBRARY = nsp-cla.a 

OBJSC = gtk.o gdk.o atk.o pango.o 
OBJSF=

include ../../../Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

# include ../../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

%.X : %.c 
	protoize -k -c -I../include $*.c 
	egrep -v "/usr/|nsp/" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' -e "s/ extern/extern/" > $*.X 
	rm -f $*.c.X

# on peut rajouter --prefix py$* 

PYTHON=python 
ALL=  gdk.c gtk.c gtkgl.c libglade.c pango.c atk.c 

all:: $(ALL)

clean :: 
	$(RM) $(ALL)

.defs.c:
	($(PYTHON) codegen/codegen.py \
	    --override $*.override \
	    --register ./pango-types.defs \
	    --register ./atk-types.defs \
	    --register ./gdk-types.defs \
	    --register ./gtk-types.defs \
	    --prefix $* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c

gtk24.c: gtk24.defs gtk.override
	($(PYTHON) codegen/codegen.py \
	    --override gtk.override \
	    --register ./pango-types.defs \
	    --register ./atk-types.defs \
	    --register ./gdk-types.defs \
	    --register ./gtk-types.defs \
	    --prefix $* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c

gdk.c : gdk.defs  gdk.override gdk-types.defs 
gtk.c : gtk.defs  gtk.override gtk-types.defs gtk-extrafuncs.defs
gtkgl.c : gtkgl.defs  gtkgl.override 
libglade.c : libglade.defs  libglade.override 
atk.c : atk.defs  atk.override atk-types.defs 
pango.c : pango.defs  pango.override pango-types.defs 

