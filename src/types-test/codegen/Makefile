.SUFFIXES: .defs .c $(SUFFIXES)
SHELL = /bin/sh

SCIDIR=../../..
SCIDIR1=..\..\..

LIBRARY = nsp-cla.a 

OBJSC = classa.o classb.o 
OBJSF=

include $(SCIDIR)/Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

PYTHON=python 
ALL=  ../classa.c ../classb.c ../classaref.c ../classbref.c \
	../polyline.c ../graphic.c ../axes.c ../figure.c ../curve.c \
	../groot.c ../gmatrix.c ../vfield.c ../contour.c ../grstring.c \
	../grrect.c ../grarc.c ../arrows.c ../segments.c ../surf.c \
	../box3d.c ../objs3d.c ../points3d.c ../polyhedron.c ../polyline3d.c \
	../spolyhedron.c ../string3d.c  ../compound.c ../gmatrix1.c \
	../fec.c 

all:: $(ALL)

clean :: 
	$(RM) $(ALL)

.defs.c:
	($(PYTHON) codegen/codegen.py \
	    --override $*.override \
	    --prefix $* $*.defs) > gen-$*.c \
	&& cp gen-$*.c $*.c \
	&& rm -f gen-$*.c

../classa.c : classa-types.defs classa.defs classa.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classa.override \
	    --prefix ClassA classa.defs) > $*.c \
	&& cp classa.h ../../include/nsp 

../classb.c : classb-types.defs classb.defs  classb.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classb.override \
	    --prefix ClassB classb.defs) > $*.c \
	&& cp classb.h ../../include/nsp 

../classaref.c : classaref-types.defs classaref.defs  classaref.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classaref.override \
	    --prefix ClassARef classaref.defs) > $*.c \
	&& cp classaref.h ../../include/nsp 

../classbref.c : classbref-types.defs classbref.defs classbref.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override classbref.override \
	    --prefix ClassBRef classbref.defs) > $*.c \
	&& cp classbref.h ../../include/nsp 

../polyline.c : polyline-types.defs polyline.defs  polyline.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override polyline.override \
	    --prefix Polyline polyline.defs) > $*.c \
	&& cp polyline.h ../../include/nsp 

../graphic.c : graphic-types.defs graphic.defs graphic.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override graphic.override \
	    --prefix Graphic graphic.defs) > $*.c \
	&& cp graphic.h ../../include/nsp 

../axes.c : axes-types.defs axes.defs axes.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override axes.override \
	    --prefix Axes axes.defs) > $*.c \
	&& cp axes.h ../../include/nsp 

../curve.c : curve-types.defs curve.defs curve.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override curve.override \
	    --prefix Curve curve.defs) > $*.c \
	&& cp curve.h ../../include/nsp 

../figure.c :  figure.defs  figure.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
		--register ./axes-types.defs \
		--register ./graphic-types.defs \
	    --override figure.override \
	    --prefix Figure figure.defs) > $*.c \
	&& cp figure.h ../../include/nsp \
	&& cp figuredata.h ../../include/nsp

../groot.c : groot.defs  groot.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override groot.override \
	    --prefix GRoot groot.defs) > $*.c \
	&& cp groot.h ../../include/nsp 

../gmatrix.c :  gmatrix.defs gmatrix.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override gmatrix.override \
	    --prefix GMatrix gmatrix.defs) > $*.c \
	&& cp gmatrix.h ../../include/nsp 

../gmatrix1.c :  gmatrix1.defs gmatrix1.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override gmatrix1.override \
	    --prefix GMatrix1 gmatrix1.defs) > $*.c \
	&& cp gmatrix1.h ../../include/nsp 

../fec.c :  fec.defs fec.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override fec.override \
	    --prefix Fec fec.defs) > $*.c \
	&& cp fec.h ../../include/nsp 

../vfield.c : vfield.defs vfield.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override vfield.override \
	    --prefix VField vfield.defs) > $*.c \
	&& cp vfield.h ../../include/nsp 

../contour.c :  contour.defs contour.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override contour.override \
	    --prefix Contour contour.defs) > $*.c \
	&& cp contour.h ../../include/nsp 

../grstring.c : grstring-types.defs grstring.defs  grstring.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override grstring.override \
	    --prefix Grstring grstring.defs) > $*.c \
	&& cp grstring.h ../../include/nsp 

../grrect.c : grrect-types.defs grrect.defs  grrect.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override grrect.override \
	    --prefix GrRect grrect.defs) > $*.c \
	&& cp grrect.h ../../include/nsp 

../grarc.c : grarc-types.defs grarc.defs grarc.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override grarc.override \
	    --prefix GrArc grarc.defs) > $*.c \
	&& cp grarc.h ../../include/nsp 

../arrows.c : arrows.defs arrows-types.defs arrows.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override arrows.override \
	    --prefix Arrows arrows.defs) > $*.c \
	&& cp arrows.h ../../include/nsp 

../segments.c : segments.defs segments-types.defs segments.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override segments.override \
	    --prefix Segments segments.defs) > $*.c \
	&& cp segments.h ../../include/nsp 

../surf.c : surf-types.defs surf.defs  surf.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override surf.override \
	    --prefix Surf surf.defs) > $*.c \
	&& cp surf.h ../../include/nsp 

../string3d.c : string3d-types.defs string3d.defs string3d.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override string3d.override \
	    --prefix String3d string3d.defs) > $*.c \
	&& cp string3d.h ../../include/nsp 

../spolyhedron.c : spolyhedron-types.defs spolyhedron.defs  spolyhedron.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override spolyhedron.override \
	    --prefix SPolyhedron spolyhedron.defs) > $*.c \
	&& cp spolyhedron.h ../../include/nsp 

../polyhedron.c : polyhedron-types.defs polyhedron.defs polyhedron.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override polyhedron.override \
	    --prefix Polyhedron polyhedron.defs) > $*.c \
	&& cp polyhedron.h ../../include/nsp 

../points3d.c : points3d-types.defs points3d.defs  points3d.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override points3d.override \
	    --prefix Points3d points3d.defs) > $*.c \
	&& cp points3d.h ../../include/nsp 

../objs3d.c : objs3d-types.defs objs3d.defs  objs3d.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override objs3d.override \
	    --prefix Objs3d objs3d.defs) > $*.c \
	&& cp objs3d.h ../../include/nsp 

../box3d.c : box3d-types.defs box3d.defs box3d.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override box3d.override \
	    --prefix Box3d box3d.defs) > $*.c \
	&& cp box3d.h ../../include/nsp 

../polyline3d.c : polyline3d-types.defs polyline3d.defs  polyline3d.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override polyline3d.override \
	    --prefix Polyline3d polyline3d.defs) > $*.c \
	&& cp polyline3d.h ../../include/nsp 

../compound.c : compound-types.defs compound.defs  compound.override codegen/*.py
	($(PYTHON) codegen/codegen.py \
	    --override compound.override \
	    --prefix Compound compound.defs) > $*.c \
	&& cp compound.h ../../include/nsp 


