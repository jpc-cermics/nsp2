SHELL = /bin/sh
SCIDIR=../..
include ../../Version.incl

LIBRARY = graphics.a

# periGif.o 

OBJSC = Graphics-IN.o periGtk.o periGL.o periPos.o periFig.o \
	Contour.o Plo3d.o Math.o Axes.o Champ.o Plo2d.o \
	Plo2d1.o Plo2d2.o Plo2d3.o Plo2d4.o Plo2dEch.o Rec.o Gray.o \
	Alloc.o FeC.o RecLoad.o RecSave.o Tests.o Actions.o \
	nues1.o Format.o windows.o Xcall1.o clip.o dsearch.o

OBJSF = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(XFLAGS) 

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

# with gcc compiler, do not inline "floor" function in "Format.c"

Format.o: Format.c
	$(CC) $(CFLAGS) -D__NO_MATH_INLINES -c $< -o $@

#--- dependencies generated with gcc ----
