# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

SCIDIR=../..
include ../../Version.incl

LIBRARY = gtksci.a

OBJSC =	click.o command.o  io.o \
	menus.o x_VTPrsTbl.o x_main.o \
	zzledt-rl.o about.o queue.o helpbrowser.o 

OBJSF  = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(TK_INC_PATH) $(TCL_INC_PATH)
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

# need gnome includes 

helpbrowser.o : helpbrowser.c 
	$(CC) $(CC_OPTIONS) `gnome-config --cflags gnome` -c helpbrowser.c 

helpbrowser:  helpbrowser.c queue.o
	$(CC) -DALONE $(CC_OPTIONS) `gnome-config --cflags gnome` \
		-o helpbrowser helpbrowser.c queue.o \
		`gnome-config --cflags gnome` $(XLIBS)

OBJTEST = x_test_loop.o

clean::
	$(RM) $(OBJTEST)
distclean::
	$(RM) $(OBJTEST) test


# generated with gcc -MMD 
#--------------------------


