SHELL = /bin/sh

SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

# gobject.o

OBJSC = gboxed.o gobject.o  gdkatom.o gtk.o atk.o pango.o gdk.o pangoattribute.o to_be_done.o to_be_done1.o gpointer.o strings.o

OBJSF = 

include ../../Makefile.incl
SCIDIR=../..

CFLAGS = $(CC_OPTIONS) 

include ../Make.lib

gtksci.o : gtksci-generated.c 

atk.c : 
	@ln -s codegen/atk.c .

gdk.c : 
	@ln -s codegen/gdk.c .	

# gtk.c is not a link 

pango.c : 
	@ln -s codegen/pango.c .	

clean :: 
	$(RM) atk.c gdk.c  pango.c




