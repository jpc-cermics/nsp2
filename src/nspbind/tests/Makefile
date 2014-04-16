.SUFFIXES: .defs .diff *.idiff .c $(SUFFIXES)
SHELL = /bin/sh

ALL=  classa.c classb.c classaref.c classbref.c \
	polyline.c graphic.c axes.c figure.c curve.c \
	qcurve.c \
	groot.c gmatrix.c vfield.c contour.c grstring.c \
	grrect.c grarc.c arrows.c segments.c surf.c \
	box3d.c objs3d.c points3d.c polyhedron.c polyline3d.c \
	spolyhedron.c string3d.c  compound.c gmatrix1.c \
	fec.c  contour3d.c block.c link.c connector.c \
	diagram.c grimage.c epoints.c sharedlib.c \
	hm.c agraph.c ast.c astv.c bvar.c \
	stochdec.c bson.c 

DIFF=$(patsubst %.c,%.diff,$(ALL))
IDIFF=$(patsubst %.c,%.idiff,$(ALL))

PROG=../bin/simport 
SRC=/usr/local/src/nsp2-ref/src/types-test/
ISRC=/usr/local/src/nsp2-ref/src/include/nsp/

all:	$(ALL)

agraph.c: agraph.defs 
	$(PROG) -o /tmp/$*.c $* 
	@sed -e "s+codegen/+../types-test/codegen/+g" \
	    -e "s+/tmp/agraph.c+agraph.c+g" /tmp/$*.c > $*.c
	@rm -f /tmp/$*.c

test.c: test.defs
	$(PROG) -p $* -o $*.c $* 

test1.c: test1.defs
	$(PROG) -p $* -o $*.c $* 

agraph.diff : agraph.c 
	@echo "-------- diff $*.c -------------------"
	@diff -rw $*.c $(SRC)/../libgraphviz/; $$status 

%.diff: %.c	
	@echo "-------- diff $*.c -------------------"
	@diff -rw $*.c $(SRC); $$status 

%.idiff: %.c	
	@echo "-------- diff $*.h -------------------"
	@diff -rw $*.h $(ISRC); $$status 

%.c: %.defs
	$(PROG) -o $*.c $*

diff: $(DIFF)

idiff: $(IDIFF)

clean:
	rm -f *.c *.h

classa.c : classa-types.defs classa.defs classa.override 

classb.c : classb-types.defs classb.defs  classb.override 

classaref.c : classaref-types.defs classaref.defs  classaref.override 

classbref.c : classbref-types.defs classbref.defs classbref.override 

polyline.c : polyline-types.defs polyline.defs  polyline.override 

graphic.c : graphic-types.defs graphic.defs graphic.override 

axes.c : axes-types.defs axes.defs axes.override 

curve.c : curve-types.defs curve.defs curve.override 

qcurve.c : qcurve-types.defs qcurve.defs qcurve.override 

figure.c :  figure.defs  figure.override 

groot.c : groot.defs  groot.override 

gmatrix.c :  gmatrix.defs gmatrix.override 

gmatrix1.c :  gmatrix1.defs gmatrix1.override 

fec.c :  fec.defs fec.override 

vfield.c : vfield.defs vfield.override 

contour.c :  contour.defs contour.override 

grstring.c : grstring-types.defs grstring.defs  grstring.override 

grrect.c : grrect-types.defs grrect.defs  grrect.override 

grarc.c : grarc-types.defs grarc.defs grarc.override 

arrows.c : arrows.defs arrows-types.defs arrows.override 

segments.c : segments.defs segments-types.defs segments.override 

surf.c : surf-types.defs surf.defs  surf.override 

string3d.c : string3d-types.defs string3d.defs string3d.override 

spolyhedron.c : spolyhedron-types.defs spolyhedron.defs  spolyhedron.override 

contour3d.c : contour3d-types.defs contour3d.defs  contour3d.override 

polyhedron.c : polyhedron-types.defs polyhedron.defs polyhedron.override 

points3d.c : points3d-types.defs points3d.defs  points3d.override 

objs3d.c : objs3d-types.defs objs3d.defs  objs3d.override 

box3d.c : box3d-types.defs box3d.defs box3d.override 

polyline3d.c : polyline3d-types.defs polyline3d.defs  polyline3d.override 

compound.c : compound-types.defs compound.defs  compound.override 

block.c : block.defs  block.override 

link.c : link.defs  link.override 

diagram.c : diagram.defs  diagram.override 

connector.c : connector.defs  connector.override 

grimage.c : grimage.defs grimage-types.defs grimage.override 

interface.c : interface.defs 

sharedlib.c : sharedlib.defs sharedlib-types.defs sharedlib.override 

hm.c : hm.defs hm-types.defs hm.override 

ast.c : ast.defs ast-types.defs ast.override 

astv.c : astv.defs astv-types.defs astv.override 

bvar.c : bvar.defs bvar-types.defs bvar.override 

stochdec.c : stochdec.defs stochdec-types.defs stochdec.override 

bson.c : bson.defs bson-types.defs bson.override 
























