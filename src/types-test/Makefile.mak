#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib 

GRAPHICS= figure.obj axes.obj polyline.obj curve.obj graphic.obj groot.obj gmatrix.obj \
	vfield.obj contour.obj grarc.obj grrect.obj arrows.obj segments.obj surf.obj \
	box3d.obj objs3d.obj points3d.obj polyhedron.obj polyline3d.obj spolyhedron.obj \
	string3d.obj grstring.obj compound.obj gmatrix1.obj fec.obj qcurve.obj contour3d.obj \
	block.obj link.obj connector.obj diagram.obj grimage.obj gpixbuf.obj gridblock.obj

OBJSC = classa.obj classb.obj classc.obj classaref.obj classbref.obj \
	clip.obj Math.obj gr_alloc.obj Format.obj dsearch.obj \
	switch.obj $(GRAPHICS) sharedlib.obj epoints.obj hm.obj ast.obj astv.obj \
	bvar.obj bson.obj stochdec.obj linearsys.obj ast-print.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

%.X : %.c 
	protoize -k -c -I../include $*.c 
	egrep -v "/usr/|nsp/" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' -e "s/ extern/extern/" > $*.X 
	rm -f $*.c.X

example.obj	: example.cxx 
	g++ -g -c example.cxx 

example_wrap.obj	: example_wrap.cpp
	g++ -I../include -g -c example_wrap.cpp


force	:
	cd codegen ;   make 

