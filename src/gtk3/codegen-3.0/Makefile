.SUFFIXES: .defs .diff .idiff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  pango.c atk.c gdk.c gtk.c webkit.c gvalue.c girepository.c gio.c cairo.c glib.c gparamspec.c gtksourceview.c

DIFF=$(patsubst %.c,%.diff,$(ALL))

PROG=../../../bin/nspbind -nw

all:	$(ALL)
	cd ../; make gio.o

pipo:
	cd ../; make -j

%.c: %.defs
	@echo "generating $*.c"
	@$(PROG) -cp "codegen-3.0/" -p $* -o $*.c $*
	@mv *.h ../../include/nsp/gtk/
	@mv $*.c ../

clean:
	rm -f *.c *.h

pango.c: pango.defs pango.override pango-types.defs
atk.c: atk.defs atk.override atk-types.defs
gdk.c: gdk.defs gdk.override gdk-types.defs
gtk.c: gtk.defs gtk.override gtk-types.defs
webkit.c: webkit.defs webkit.override
gvalue.c: gvalue.defs gvalue.override
gparamspec.c: gparamspec.defs gparamspec.override
cairo.c: cairo.defs cairo.override cairo-types.defs
glib.c: glib.defs glib.override glib-types.defs
gtksourceview.c: gtksourceview-types.defs gtksourceview.defs gtksourceview.override


glib.c: glib.defs
	@echo "generating $*.c"
	@$(PROG) -cp "codegen-3.0/" -p $* -o $*.c $*
	@mv gerror.h ../../include/nsp/gtk/
	@mv *.h ../../include/nsp/
	@mv $*.c ../

gparamspec.c: gparamspec.defs
	@echo "generating $*.c"
	@$(PROG) -cp "codegen-3.0/" -p $* -o $*.c $*
	@mv *.h ../../include/nsp/
	@mv $*.c ../
