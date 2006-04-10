SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

# gobject.o

OBJSC = gboxed.o gobject.o  gdkatom.o gtk.o atk.o pango.o gdk.o pangoattribute.o to_be_done.o to_be_done1.o gpointer.o strings.o graphics.o

OBJSF = 

include ../../Makefile.incl
SCIDIR=../..

CFLAGS = $(CC_OPTIONS) 

include ../Make.lib

gtksci.o : gtksci-generated.c 

atk.o : atk.c  codegen/atk.c 
gtk.o : gtk.c  codegen/gtk.c 
gdk.o : gdk.c  codegen/gdk.c 
pango.o : pango.c codegen/pango.c
gtk.o : gtk.c codegen/gtk.c codegen/gtk24.c


