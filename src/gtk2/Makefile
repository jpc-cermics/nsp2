SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

# gobject.o

OBJSC = gboxed.o gobject.o  gdkatom.o gtk.o atk.o pango.o gdk.o pangoattribute.o to_be_done.o to_be_done1.o gpointer.o

OBJSF = 

include ../../Makefile.incl
SCIDIR=../..

CFLAGS = $(CC_OPTIONS) 

include ../Make.lib

gtksci.o : gtksci-generated.c 

atk.c : 
	@ln -s codegen/atk.c .
	@ln -s codegen/atk.override .

gdk.c : 
	@ln -s codegen/gdk.c .	
	@ln -s codegen/gdk.override .	

gtk.c : 
	@ln -s codegen/gtk.c .	
	@ln -s codegen/gtk.override .	

pango.c : 
	@ln -s codegen/pango.c .	
	@ln -s codegen/pango.override .	

clean :: 
	$(RM) atk.c gdk.c gtk.c pango.c
	$(RM) atk.override gdk.override gtk.override pango.override


