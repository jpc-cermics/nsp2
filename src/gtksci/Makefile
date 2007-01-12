# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

SCIDIR=../..
include ../../Version.incl

LIBRARY = nsp.a

OBJSHELP=helpbrowser.o uri.o 

# gtkmenu.c, gtkmarshalers.h and gtkintl.h
# are only usefull for gtk version < 2.3.5
# to correct a gtk bug (see configure.in)

OBJSC =	click.o command.o  io.o \
	menus.o  x_main.o \
	about.o queue.o $(OBJSHELP)  \
	testtext1.o testtext.o

OBJSF  = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(TK_INC_PATH) $(TCL_INC_PATH) $(HTMFLAGS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

helpbrowser:  testhelpbrowser.c queue.o uri.o
	$(CC) -g -DALONE -o helpbrowser  testhelpbrowser.c queue.o uri.o \
		 -I../include `pkg-config gtk+-2.0 libgtkhtml-2.0 --libs --cflags `

OBJTEST = x_test_loop.o

clean::
	$(RM) $(OBJTEST)
distclean::
	$(RM) $(OBJTEST) test


# generated with gcc -MMD 
#--------------------------


