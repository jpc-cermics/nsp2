SUFFIXES=.job

H2DEF=./codegen/h2def.py

all:	gtk.defs gdk.defs pango.defs gio.defs gdkpixbuf.defs cairo.defs glib.defs gtksourceview.defs

gtk.defs :
	$(H2DEF) -s gtk /usr/local/src/mingw32-build/mingw-cross-apps-jpc/gtk+3.22/gtk+-3.22.4/gtk/gtk*.h

gdk.defs :
	$(H2DEF) -s gdk ~/Downloads/gtk+-3.16.0/gdk/*.h

pango.defs: Makefile
	$(H2DEF) -s pango /usr/include/pango-1.0/pango/pango*.h

gio.defs: Makefile
	$(H2DEF) -s gio /usr/local/src/mingw64-build/mingw-cross-apps-jpc/gtk+-3.22/glib-2.50.2/gio/*.h

glib.defs: Makefile
	$(H2DEF) -s glib /usr/include/glib-2.0/glib/*.h

gdkpixbuf.defs: Makefile
	$(H2DEF) -s gdkpixbuf /usr/include/gdk-pixbuf-2.0/gdk-pixbuf/*.h

CAIRO_H= /usr/include/cairo/cairo-gobject.h \
	/usr/include/cairo/cairo.h \
	/usr/include/cairo/cairo-pdf.h \
	/usr/include/cairo/cairo-ps.h \
	/usr/include/cairo/cairo-script.h \
	/usr/include/cairo/cairo-script-interpreter.h \
	/usr/include/cairo/cairo-svg.h \
	/usr/include/cairo/cairo-version.h

cairo.defs: Makefile
	$(H2DEF) -s cairo $(CAIRO_H)

gtksourceview.defs: Makefile
	$(H2DEF) -s gtksourceview /usr/include/gtksourceview-3.0/gtksourceview/*.h

test	:
	cat $(CAIRO_H) > cairo.h
