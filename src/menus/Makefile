SHELL = /bin/sh
SCIDIR=../..

LIBRARY = nsp.a 

OBJSC = gxmen_choice.o gxmen_choose.o gxmen_dialog.o gxmen_getfile.o \
	gxmen_mdial.o gxmen_message.o gxmen_print.o gxmen_madial.o \
	men_combo_color.o gxmen_utils.o men-IN.o 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(XFLAGS)

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

#==============dependencies 

$(OBJSC) : ../include/nsp/menus.h ../include/nsp/machine.h

