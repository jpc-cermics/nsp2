SHELL = /bin/sh
SCIDIR=../..
include ../../Version.incl

LIBRARY = graphics.a

# periGif.o 

OBJSC = Graphics-IN.o periGtk.o periGL.o periPos.o periFig.o \
	perigen.o Contour.o Plo3d.o Math.o Axes.o Champ.o Plo2d.o \
	Plo2d1.o Plo2d2.o Plo2d3.o Plo2d4.o Plo2dEch.o Rec.o Gray.o \
	Alloc.o RecLoad.o RecSave.o Tests.o Actions.o \
	nues1.o Format.o windows.o Xcall1.o clip.o dsearch.o \
	Plo3dObj.o

OBJSF = 

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) $(XFLAGS) 

FFLAGS = $(FC_OPTIONS)

include ../Make.lib

# with gcc compiler, do not inline "floor" function in "Format.c"

Format.o: Format.c
	@echo compiling $<
	@$(CC) $(CFLAGS) -D__NO_MATH_INLINES -c $< -o $@

periGtk.o : periGtkstd.c periGLGtk.c 
periGL.o : periGLstd.c periGLGtk.c 

#--- dependencies generated with gcc ----
Actions.o: Actions.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/command.h
Alloc.o: Alloc.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Axes.o: Axes.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Champ.o: Champ.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
clip.o: clip.c ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Contour.o: Contour.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
dsearch.o: dsearch.c ../include/nsp/matrix-in.h \
  ../include/nsp/interf.h ../include/nsp/object.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/object-inlined.h ../include/nsp/string.h \
  ../include/nsp/hobj.h ../include/nsp/sciio.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h ../include/nsp/file.h \
  ../include/nsp/xdr.h ../include/nsp/matrix.h \
  ../include/nsp/bmatrix.h ../include/nsp/smatrix.h \
  ../include/nsp/spmatrix.h ../include/nsp/pmatrix.h \
  ../include/nsp/plist.h ../include/nsp/list.h \
  ../include/nsp/hash.h ../include/nsp/function.h \
  ../include/nsp/ivect.h ../include/nsp/mod.h \
  ../include/nsp/me.h ../include/nsp/lmo.h \
  ../include/nsp/rect.h ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/block.h \
  ../include/nsp/gframe.h ../include/nsp/grint.h \
  ../include/nsp/connector.h ../include/nsp/link.h \
  ../include/nsp/none.h ../include/nsp/type.h \
  ../include/nsp/module.h ../include/nsp/modulelt.h \
  ../include/nsp/classa.h ../include/nsp/classb.h \
  ../include/nsp/cells.h ../include/nsp/matint.h \
  ../include/nsp/smatrix-in.h ../include/nsp/bmatrix-in.h \
  ../interp/Eval.h ../include/nsp/interf.h \
  ../include/nsp/datas.h ../include/nsp/graphics/Graphics.h \
  ../include/nsp/gsort-p.h
Format.o: Format.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Graphics-IN.o: Graphics-IN.c ../include/nsp/matrix-in.h \
  ../include/nsp/interf.h ../include/nsp/object.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/object-inlined.h ../include/nsp/string.h \
  ../include/nsp/hobj.h ../include/nsp/sciio.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h ../include/nsp/file.h \
  ../include/nsp/xdr.h ../include/nsp/matrix.h \
  ../include/nsp/bmatrix.h ../include/nsp/smatrix.h \
  ../include/nsp/spmatrix.h ../include/nsp/pmatrix.h \
  ../include/nsp/plist.h ../include/nsp/list.h \
  ../include/nsp/hash.h ../include/nsp/function.h \
  ../include/nsp/ivect.h ../include/nsp/mod.h \
  ../include/nsp/me.h ../include/nsp/lmo.h \
  ../include/nsp/rect.h ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/block.h \
  ../include/nsp/gframe.h ../include/nsp/grint.h \
  ../include/nsp/connector.h ../include/nsp/link.h \
  ../include/nsp/none.h ../include/nsp/type.h \
  ../include/nsp/module.h ../include/nsp/modulelt.h \
  ../include/nsp/classa.h ../include/nsp/classb.h \
  ../include/nsp/cells.h ../include/nsp/matint.h \
  ../include/nsp/bmatrix-in.h ../include/nsp/parse.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/gsort-p.h ../include/nsp/gtk/gobject.h \
  ../include/nsp/sciio.h ../include/nsp/object.h \
  ../include/nsp/gtk/gdkimage.h \
  ../include/nsp/gtk/gobject.h \
  ../include/nsp/gtk/gdkpixbuf.h
Gray.o: Gray.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Math.o: Math.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
nues1.o: nues1.c ../include/nsp/machine.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
periFig.o: periFig.c ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h ../include/nsp/math.h \
  ../include/nsp/graphics/periFig.h /usr/X11R6/include/X11/Xlib.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/version.h \
  ../include/nsp/graphics/color.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h ../include/nsp/math.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/version.h \
  ../include/nsp/graphics/color.h ../include/nsp/command.h \
  ../include/nsp/sciio.h ../include/nsp/math.h \
  ../include/nsp/machine.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/math.h ../include/nsp/graphics/periGL.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/version.h \
  ../include/nsp/graphics/color.h ../include/nsp/command.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h ../include/nsp/math.h \
  ../include/nsp/graphics/periGtk.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/version.h \
  ../include/nsp/graphics/color.h ../include/nsp/command.h
periPos.o: periPos.c ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h ../include/nsp/math.h \
  ../include/nsp/graphics/periPos.h /usr/X11R6/include/X11/Xlib.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/version.h \
  ../include/nsp/graphics/color.h ../system/files.h \
  ../include/nsp/math.h ../include/nsp/machine.h
Plo2d1.o: Plo2d1.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Plo2d2.o: Plo2d2.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Plo2d3.o: Plo2d3.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Plo2d4.o: Plo2d4.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Plo2d.o: Plo2d.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Plo2dEch.o: Plo2dEch.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Plo3d.o: Plo3d.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/gsort-p.h \
  ../include/nsp/string.h ../include/nsp/object.h \
  ../include/nsp/object-inlined.h ../include/nsp/hobj.h \
  ../include/nsp/sciio.h ../include/nsp/file.h \
  ../include/nsp/xdr.h ../include/nsp/matrix.h \
  ../include/nsp/bmatrix.h ../include/nsp/smatrix.h \
  ../include/nsp/spmatrix.h ../include/nsp/pmatrix.h \
  ../include/nsp/plist.h ../include/nsp/list.h \
  ../include/nsp/hash.h ../include/nsp/function.h \
  ../include/nsp/ivect.h ../include/nsp/mod.h \
  ../include/nsp/me.h ../include/nsp/lmo.h \
  ../include/nsp/rect.h ../include/nsp/graphics/Graphics.h \
  ../include/nsp/block.h ../include/nsp/gframe.h \
  ../include/nsp/grint.h ../include/nsp/connector.h \
  ../include/nsp/link.h ../include/nsp/none.h \
  ../include/nsp/type.h ../include/nsp/module.h \
  ../include/nsp/modulelt.h ../include/nsp/classa.h \
  ../include/nsp/classb.h ../include/nsp/cells.h \
  ../include/nsp/matint.h ../include/nsp/graphics/periGL.h \
  /usr/lib/glib-2.0/include/glibconfig.h \
  /usr/lib/gtk-2.0/include/gdkconfig.h \
  /usr/lib/gtkglext-1.0/include/gdkglext-config.h \
  ../include/nsp/graphics/Graphics.h
Plo3dObj.o: Plo3dObj.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/interf.h \
  ../include/nsp/object.h ../include/nsp/object-inlined.h \
  ../include/nsp/string.h ../include/nsp/hobj.h \
  ../include/nsp/sciio.h ../include/nsp/file.h \
  ../include/nsp/xdr.h ../include/nsp/matrix.h \
  ../include/nsp/bmatrix.h ../include/nsp/smatrix.h \
  ../include/nsp/spmatrix.h ../include/nsp/pmatrix.h \
  ../include/nsp/plist.h ../include/nsp/list.h \
  ../include/nsp/hash.h ../include/nsp/function.h \
  ../include/nsp/ivect.h ../include/nsp/mod.h \
  ../include/nsp/me.h ../include/nsp/lmo.h \
  ../include/nsp/rect.h ../include/nsp/graphics/Graphics.h \
  ../include/nsp/block.h ../include/nsp/gframe.h \
  ../include/nsp/grint.h ../include/nsp/connector.h \
  ../include/nsp/link.h ../include/nsp/none.h \
  ../include/nsp/type.h ../include/nsp/module.h \
  ../include/nsp/modulelt.h ../include/nsp/classa.h \
  ../include/nsp/classb.h ../include/nsp/cells.h \
  ../include/nsp/matint.h Plo3dObj.h
Rec.o: Rec.c ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/Rec_private.h \
  ../include/nsp/graphics/Rec.h ../include/nsp/object.h \
  ../include/nsp/object-inlined.h ../include/nsp/string.h \
  ../include/nsp/hobj.h ../include/nsp/sciio.h \
  ../include/nsp/object.h ../include/nsp/file.h \
  ../include/nsp/xdr.h ../include/nsp/matrix.h \
  ../include/nsp/bmatrix.h ../include/nsp/smatrix.h \
  ../include/nsp/spmatrix.h ../include/nsp/pmatrix.h \
  ../include/nsp/plist.h ../include/nsp/list.h \
  ../include/nsp/hash.h ../include/nsp/function.h \
  ../include/nsp/ivect.h ../include/nsp/mod.h \
  ../include/nsp/me.h ../include/nsp/lmo.h \
  ../include/nsp/rect.h ../include/nsp/graphics/Graphics.h \
  ../include/nsp/block.h ../include/nsp/gframe.h \
  ../include/nsp/grint.h ../include/nsp/connector.h \
  ../include/nsp/link.h ../include/nsp/none.h \
  ../include/nsp/type.h ../include/nsp/module.h \
  ../include/nsp/modulelt.h ../include/nsp/classa.h \
  ../include/nsp/classb.h ../include/nsp/cells.h \
  ../include/nsp/matint.h
RecLoad.o: RecLoad.c ../include/nsp/math.h \
  ../include/nsp/machine.h ../include/nsp/xdr.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/graphics/Rec.h \
  ../include/nsp/object.h ../include/nsp/object-inlined.h \
  ../include/nsp/string.h ../include/nsp/hobj.h \
  ../include/nsp/sciio.h ../include/nsp/object.h \
  ../include/nsp/file.h ../include/nsp/xdr.h \
  ../include/nsp/matrix.h ../include/nsp/bmatrix.h \
  ../include/nsp/smatrix.h ../include/nsp/spmatrix.h \
  ../include/nsp/pmatrix.h ../include/nsp/plist.h \
  ../include/nsp/list.h ../include/nsp/hash.h \
  ../include/nsp/function.h ../include/nsp/ivect.h \
  ../include/nsp/mod.h ../include/nsp/me.h \
  ../include/nsp/lmo.h ../include/nsp/rect.h \
  ../include/nsp/graphics/Graphics.h ../include/nsp/block.h \
  ../include/nsp/gframe.h ../include/nsp/grint.h \
  ../include/nsp/connector.h ../include/nsp/link.h \
  ../include/nsp/none.h ../include/nsp/type.h \
  ../include/nsp/module.h ../include/nsp/modulelt.h \
  ../include/nsp/classa.h ../include/nsp/classb.h \
  ../include/nsp/cells.h ../include/nsp/matint.h
RecSave.o: RecSave.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h ../include/nsp/graphics/Rec.h \
  ../include/nsp/object.h ../include/nsp/object-inlined.h \
  ../include/nsp/string.h ../include/nsp/hobj.h \
  ../include/nsp/sciio.h ../include/nsp/object.h \
  ../include/nsp/file.h ../include/nsp/xdr.h \
  ../include/nsp/matrix.h ../include/nsp/bmatrix.h \
  ../include/nsp/smatrix.h ../include/nsp/spmatrix.h \
  ../include/nsp/pmatrix.h ../include/nsp/plist.h \
  ../include/nsp/list.h ../include/nsp/hash.h \
  ../include/nsp/function.h ../include/nsp/ivect.h \
  ../include/nsp/mod.h ../include/nsp/me.h \
  ../include/nsp/lmo.h ../include/nsp/rect.h \
  ../include/nsp/graphics/Graphics.h ../include/nsp/block.h \
  ../include/nsp/gframe.h ../include/nsp/grint.h \
  ../include/nsp/connector.h ../include/nsp/link.h \
  ../include/nsp/none.h ../include/nsp/type.h \
  ../include/nsp/module.h ../include/nsp/modulelt.h \
  ../include/nsp/classa.h ../include/nsp/classb.h \
  ../include/nsp/cells.h ../include/nsp/matint.h \
  ../include/nsp/xdr.h
Tests.o: Tests.c ../include/nsp/math.h \
  ../include/nsp/machine.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
windows.o: windows.c ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/machine.h \
  ../include/nsp/user-prefs.h ../include/nsp/tokenizer.h \
  ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
Xcall1.o: Xcall1.c ../include/nsp/math.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/math.h ../include/nsp/user-prefs.h \
  ../include/nsp/tokenizer.h ../include/nsp/plisttoken.h \
  ../include/nsp/graphics/Graphics.h \
  ../include/nsp/machine.h ../include/nsp/sciio.h \
  ../include/nsp/graphics/perigen.h \
  ../include/nsp/plisttoken.h
