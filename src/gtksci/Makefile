# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

SCIDIR=../..
include ../../Version.incl

LIBRARY = gtksci.a

OBJSHELP=helpbrowser.o uri.o


# gtkmenu.c, gtkmarshalers.h and gtkintl.h
# are only usefull for gtk version < 2.3.5
# to correct a gtk bug (see configure.in)

OBJSC =	click.o command.o  io.o \
	menus.o  x_main.o \
	about.o queue.o $(OBJSHELP) gtkmenu.o


OBJSF  = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(TK_INC_PATH) $(TCL_INC_PATH) $(HTMFLAGS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

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


