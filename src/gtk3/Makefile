SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

# gobject.o

OBJSC = gtk.o gdk.o gboxed.o gobject.o  gdkatom.o  atk.o \
	pango.o pangoattribute.o to_be_done.o \
	to_be_done1.o gpointer.o strings.o \
	gmarkup.o gdate.o gdatetime.o \
	girepository.o gvalue.o

OBJSF =

include ../../Makefile.incl
SCIDIR=../..

CFLAGS = $(CC_OPTIONS) -I/usr/include/libglade-2.0/

GTKFLAGS= -DG_DISABLE_DEPRECATED  \
	-DGDK_PIXBUF_DISABLE_DEPRECATED \
	-DGDK_DISABLE_DEPRECATED \
	-DGTK_DISABLE_DEPRECATED \
	-DPANGO_DISABLE_DEPRECATED

CFLAGS = $(CC_OPTIONS) -I/usr/include/libglade-2.0/

include ../Make.lib

gtksci.o : gtksci-generated.c

atk.o : atk.c  codegen/atk.c
gtk.o : gtk.c  codegen/gtk.c
gdk.o : gdk.c  codegen/gdk.c
pango.o : pango.c codegen/pango.c
gtk.o : gtk.c codegen/gtk.c
webkit.o : webkit.c codegen/webkit.c

girepository.o 	: codegen/girepository.c girepository.c
gvalue.o 	: codegen/gvalue.c gvalue.c
