# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..
include ../../Version.incl

LIBRARY = $(SCIDIR)/libs/graphics.lib

OBJSC = periWin.obj periPos.obj periFig.obj periGif.obj Xcall.obj Xcall1.obj \
	Contour.obj Plo3d.obj Math.obj Axes.obj Champ.obj Plo2d.obj \
	Plo2d1.obj Plo2d2.obj Plo2d3.obj Plo2d4.obj Plo2dEch.obj Rec.obj Gray.obj \
	Alloc.obj FeC.obj RecLoad.obj RecSave.obj Tests.obj Actions.obj \
	gsort.obj qsort.obj nues1.obj  Format.obj 

OBJSF = 

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) $(XFLAGS) 

include ../Make.lib.mak

#--- dependencies generated with gcc ----
Actions.o: Actions.c Math.h ../machine.h Graphics.h
Alloc.o: Alloc.c Math.h ../machine.h Graphics.h
Axes.o: Axes.c Math.h ../machine.h Graphics.h PloEch.h
Champ.o: Champ.c Math.h ../machine.h Graphics.h PloEch.h
Contour.o: Contour.c Math.h ../machine.h Graphics.h PloEch.h
FeC.o: FeC.c Math.h ../machine.h Graphics.h
Format.o: Format.c Math.h ../machine.h Graphics.h
Gray.o: Gray.c Math.h ../machine.h Graphics.h PloEch.h
gsort.o: gsort.c Math.h ../machine.h Graphics.h ../sun/men_Sutils.h \
 gsort-int.h gsort-double.h gsort-string.h
Math.o: Math.c Math.h ../machine.h Graphics.h ../sparse/spConfig.h
nues1.o: nues1.c ../machine.h Math.h Graphics.h
periFig.o: periFig.c Math.h ../machine.h Graphics.h periFig.h color.h
periGif.o: periGif.c Math.h ../machine.h Graphics.h periGif.h color.h \
 ../gd/gd.h ../gd/../machine.h
periPos.o: periPos.c Math.h ../machine.h Graphics.h periPos.h color.h
periX11.o: periX11.c Math.h ../machine.h Graphics.h periX11.h \
 periX11-bcg.h ../version.h color.h ../intersci/cerro.h \
 ../intersci/../machine.h
Plo2d1.o: Plo2d1.c Math.h ../machine.h Graphics.h PloEch.h
Plo2d2.o: Plo2d2.c Math.h ../machine.h Graphics.h PloEch.h
Plo2d3.o: Plo2d3.c Math.h ../machine.h Graphics.h PloEch.h
Plo2d4.o: Plo2d4.c Math.h ../machine.h Graphics.h PloEch.h
Plo2d.o: Plo2d.c Math.h ../machine.h Graphics.h PloEch.h
Plo2dEch.o: Plo2dEch.c Math.h ../machine.h Graphics.h PloEch.h
Plo3d.o: Plo3d.c Math.h ../machine.h Graphics.h PloEch.h
qsort.o: qsort.c
Rec.o: Rec.c Math.h ../machine.h Graphics.h Rec.h PloEch.h
RecLoad.o: RecLoad.c Math.h ../machine.h Graphics.h Rec.h
RecSave.o: RecSave.c Math.h ../machine.h Graphics.h Rec.h
Tests.o: Tests.c Math.h ../machine.h Graphics.h
Xcall1.o: Xcall1.c Math.h ../machine.h Graphics.h PloEch.h
Xcall.o: Xcall.c Math.h ../machine.h Graphics.h periX11.h periPos.h \
 periFig.h periGif.h

