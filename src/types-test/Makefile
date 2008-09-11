SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 

OBJSC = classa.o classb.o classc.o classaref.o classbref.o \
	figure.o axes.o polyline.o curve.o graphic.o groot.o gmatrix.o \
	vfield.o contour.o grarc.o grrect.o arrows.o segments.o surf.o \
	box3d.o objs3d.o points3d.o polyhedron.o polyline3d.o spolyhedron.o \
	string3d.o grstring.o



OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

%.X : %.c 
	protoize -k -c -I../include $*.c 
	egrep -v "/usr/|nsp/" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' -e "s/ extern/extern/" > $*.X 
	rm -f $*.c.X

example.o	: example.cxx 
	g++ -g -c example.cxx 

example_wrap.o	: example_wrap.cpp
	g++ -I../include -g -c example_wrap.cpp


force	:
	cd codegen ;   make 

