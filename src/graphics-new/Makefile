encies generatedSHELL = /bin/sh
SCIDIR=../..
include ../../Version.incl

LIBRARY = graphics.a

# periGif.o 

OBJSC = Graphics-IN.o periGtk.o periPos.o periFig.o \
	Contour.o Plo3d.o Math.o Axes.o Champ.o Plo2d.o \
	Plo2d1.o Plo2d2.o Plo2d3.o Plo2d4.o Plo2dEch.o Rec.o Gray.o \
	Alloc.o FeC.o RecLoad.o RecSave.o Tests.o Actions.o \
	nues1.o Format.o windows.o Xcall1.o

OBJSF = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(XFLAGS) 

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

# with gcc compiler, do not inline "floor" function in "Format.c"

Format.o: Format.c
	$(CC) $(CFLAGS) -D__NO_MATH_INLINES -c $< -o $@

#--- dependencies generated with gcc ----
Graphics-IN.o: Graphics-IN.c ../../src/include/nsp/matrix-in.h \
  ../../src/include/nsp/interf.h ../../src/include/nsp/object.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/plisttoken.h ../../src/include/nsp/file.h \
  ../../src/include/nsp/sciio.h ../../src/include/nsp/matrix.h \
  ../../src/include/nsp/bmatrix.h ../../src/include/nsp/smatrix.h \
  ../../src/include/nsp/spmatrix.h ../../src/include/nsp/pmatrix.h \
  ../../src/include/nsp/plist.h ../../src/include/nsp/list.h \
  ../../src/include/nsp/hash.h ../../src/include/nsp/hobj.h \
  ../../src/include/nsp/function.h ../../src/include/nsp/ivect.h \
  ../../src/include/nsp/mod.h ../../src/include/nsp/me.h \
  ../../src/include/nsp/lmo.h ../../src/include/nsp/rect.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h ../../src/include/nsp/block.h \
  ../../src/include/nsp/gframe.h ../../src/include/nsp/grint.h \
  ../../src/include/nsp/connector.h ../../src/include/nsp/link.h \
  ../../src/include/nsp/none.h ../../src/include/nsp/type.h \
  ../../src/include/nsp/bmatrix-in.h ../Parse/Eval.h \
  ../../src/include/nsp/datas.h ../../src/include/nsp/gsort-p.h
periGtk.o: periGtk.c /usr/include/gtk-2.0/gtk/gtk.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/periGtk.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h \
  ../../src/include/nsp/graphics/color.h
periPos.o: periPos.c ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/periPos.h /usr/X11R6/include/X11/Xlib.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h \
  ../../src/include/nsp/graphics/color.h
periFig.o: periFig.c ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/periFig.h /usr/X11R6/include/X11/Xlib.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h \
  ../../src/include/nsp/graphics/color.h
Contour.o: Contour.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo3d.o: Plo3d.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Math.o: Math.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../sparse/spConfig.h
Axes.o: Axes.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Champ.o: Champ.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d.o: Plo2d.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../sparse/spConfig.h
Plo2d1.o: Plo2d1.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d2.o: Plo2d2.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d3.o: Plo2d3.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d4.o: Plo2d4.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2dEch.o: Plo2dEch.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Rec.o: Rec.c ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h \
  ../../src/include/nsp/graphics/Rec_private.h \
  ../../src/include/nsp/graphics/Rec.h
Gray.o: Gray.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Alloc.o: Alloc.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
FeC.o: FeC.c ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
RecLoad.o: RecLoad.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../../src/include/nsp/graphics/Rec.h
RecSave.o: RecSave.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../../src/include/nsp/graphics/Rec.h
Tests.o: Tests.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Actions.o: Actions.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
nues1.o: nues1.c ../../src/include/nsp/machine.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/sciio.h ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Format.o: Format.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
windows.o: windows.c ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/machine.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Xcall1.o: Xcall1.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Graphics-IN.o: Graphics-IN.c ../../src/include/nsp/matrix-in.h \
  ../../src/include/nsp/interf.h ../../src/include/nsp/object.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/plisttoken.h ../../src/include/nsp/file.h \
  ../../src/include/nsp/sciio.h ../../src/include/nsp/matrix.h \
  ../../src/include/nsp/bmatrix.h ../../src/include/nsp/smatrix.h \
  ../../src/include/nsp/spmatrix.h ../../src/include/nsp/pmatrix.h \
  ../../src/include/nsp/plist.h ../../src/include/nsp/list.h \
  ../../src/include/nsp/hash.h ../../src/include/nsp/hobj.h \
  ../../src/include/nsp/function.h ../../src/include/nsp/ivect.h \
  ../../src/include/nsp/mod.h ../../src/include/nsp/me.h \
  ../../src/include/nsp/lmo.h ../../src/include/nsp/rect.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h ../../src/include/nsp/block.h \
  ../../src/include/nsp/gframe.h ../../src/include/nsp/grint.h \
  ../../src/include/nsp/connector.h ../../src/include/nsp/link.h \
  ../../src/include/nsp/none.h ../../src/include/nsp/type.h \
  ../../src/include/nsp/bmatrix-in.h ../Parse/Eval.h \
  ../../src/include/nsp/datas.h ../../src/include/nsp/gsort-p.h
periGtk.o: periGtk.c /usr/include/gtk-2.0/gtk/gtk.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/periGtk.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h  \
  ../../src/include/nsp/graphics/color.h
periPos.o: periPos.c ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/periPos.h /usr/X11R6/include/X11/Xlib.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h  \
  ../../src/include/nsp/graphics/color.h
periFig.o: periFig.c ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/periFig.h /usr/X11R6/include/X11/Xlib.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h   \
  ../../src/include/nsp/graphics/color.h
Contour.o: Contour.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo3d.o: Plo3d.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Math.o: Math.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../sparse/spConfig.h
Axes.o: Axes.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Champ.o: Champ.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d.o: Plo2d.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../sparse/spConfig.h
Plo2d1.o: Plo2d1.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d2.o: Plo2d2.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d3.o: Plo2d3.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2d4.o: Plo2d4.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Plo2dEch.o: Plo2dEch.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Rec.o: Rec.c ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h \
  ../../src/include/nsp/graphics/Rec_private.h \
  ../../src/include/nsp/graphics/Rec.h
Gray.o: Gray.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Alloc.o: Alloc.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
FeC.o: FeC.c ../../src/include/nsp/math.h ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
RecLoad.o: RecLoad.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../../src/include/nsp/graphics/Rec.h
RecSave.o: RecSave.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h ../../src/include/nsp/graphics/Rec.h
Tests.o: Tests.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Actions.o: Actions.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
nues1.o: nues1.c ../../src/include/nsp/machine.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/sciio.h ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Format.o: Format.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h \
  ../../src/include/nsp/graphics/Graphics.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
windows.o: windows.c ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/machine.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/math.h ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
Xcall1.o: Xcall1.c ../../src/include/nsp/math.h \
  ../../src/include/nsp/machine.h ../../src/include/nsp/sciio.h \
  ../../src/include/nsp/graphics/Graphics.h \
  ../../src/include/nsp/graphics/perigen.h \
  ../../src/include/nsp/plisttoken.h
