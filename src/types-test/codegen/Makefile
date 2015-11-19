.SUFFIXES: .defs .c $(SUFFIXES)
SHELL = /bin/sh

include ../../../Makefile.incl

ALL=  classa.defs classb.defs classaref.defs classbref.defs \
	polyline.defs graphic.defs axes.defs figure.defs curve.defs \
	qcurve.defs groot.defs gmatrix.defs vfield.defs contour.defs \
	grstring.defs grrect.defs grarc.defs arrows.defs segments.defs \
	surf.defs box3d.defs objs3d.defs points3d.defs polyhedron.defs \
	polyline3d.defs spolyhedron.defs string3d.defs compound.defs \
	gmatrix1.defs fec.defs contour3d.defs block.defs link.defs \
	connector.defs diagram.defs grimage.defs epoints.defs sharedlib.defs \
	hm.defs ast.defs astv.defs bvar.defs gpixbuf.defs \
	stochdec.defs bson.defs classc.defs

PROG=../../../bin/nspbind

SRC=../
ISRC=../../include/nsp/

TARGET=$(patsubst %.defs,../%.c,$(ALL)) ../../libgraphviz/agraph.c

all::	$(TARGET)
	cd ../; make 

../%.c: %.defs
	@echo "generate $*.c and include files"
	@$(PROG) -o $*.c $*
	@$(RM) ../$*.c ; mv $*.c ../
	@$(RM) -f $(ISRC)/$*.h
	@mv $*.h $(ISRC)/$*.h

../../libgraphviz/agraph.c: agraph.defs
	@echo "generate agraph.c and include files"
	@$(PROG) -o agraph.c agraph
	@$(RM) ../../libgraphviz/agraph.c ;
	@mv agraph.c ../../libgraphviz
	@$(RM) $(ISRC)/agraph.h
	@mv agraph.h $(ISRC)/agraph.h

clean:
	@$(RM) *.c *.h

# dependencies

../classa.c : classa-types.defs classa.defs classa.override
../classb.c : classb-types.defs classb.defs  classb.override
../classc.c : classc.defs  classc.override
../classaref.c : classaref-types.defs classaref.defs  classaref.override
../classbref.c : classbref-types.defs classbref.defs classbref.override
../polyline.c : polyline-types.defs polyline.defs  polyline.override
../graphic.c : graphic-types.defs graphic.defs graphic.override
../axes.c : axes-types.defs axes.defs axes.override
../curve.c : curve-types.defs curve.defs curve.override
../qcurve.c : qcurve-types.defs qcurve.defs qcurve.override
../figure.c :  figure.defs  figure.override
../groot.c : groot.defs  groot.override
../gmatrix.c :  gmatrix.defs gmatrix.override
../gmatrix1.c :  gmatrix1.defs gmatrix1.override
../fec.c :  fec.defs fec.override
../vfield.c : vfield.defs vfield.override
../contour.c :  contour.defs contour.override
../grstring.c : grstring-types.defs grstring.defs  grstring.override
../grrect.c : grrect-types.defs grrect.defs  grrect.override
../grarc.c : grarc-types.defs grarc.defs grarc.override
../arrows.c : arrows.defs arrows-types.defs arrows.override
../segments.c : segments.defs segments-types.defs segments.override
../surf.c : surf-types.defs surf.defs  surf.override
../string3d.c : string3d-types.defs string3d.defs string3d.override
../spolyhedron.c : spolyhedron-types.defs spolyhedron.defs  spolyhedron.override
../contour3d.c : contour3d-types.defs contour3d.defs  contour3d.override
../polyhedron.c : polyhedron-types.defs polyhedron.defs polyhedron.override
../points3d.c : points3d-types.defs points3d.defs  points3d.override
../objs3d.c : objs3d-types.defs objs3d.defs  objs3d.override
../box3d.c : box3d-types.defs box3d.defs box3d.override
../polyline3d.c : polyline3d-types.defs polyline3d.defs  polyline3d.override
../compound.c : compound-types.defs compound.defs  compound.override
../block.c : block.defs  block.override
../link.c : link.defs  link.override
../diagram.c : diagram.defs  diagram.override
../connector.c : connector.defs  connector.override
../grimage.c : grimage.defs grimage-types.defs grimage.override
../interface.c : interface.defs
../sharedlib.c : sharedlib.defs sharedlib-types.defs sharedlib.override
../hm.c : hm.defs hm-types.defs hm.override
../ast.c : ast.defs ast-types.defs ast.override
../astv.c : astv.defs astv-types.defs astv.override
../bvar.c : bvar.defs bvar-types.defs bvar.override
../stochdec.c : stochdec.defs stochdec-types.defs stochdec.override
../bson.c : bson.defs bson-types.defs bson.override

../../libgraphviz/agraph.c: agraph.defs agraph-types.defs agraph.override
