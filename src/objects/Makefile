SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a 




OBJSC = Matrix.o MatOps.o MatObj.o  \
	BMatrix.o BMatObj.o \
	SMatrix.o SMatObj.o \
	PMatrix.o PMatObj.o \
	SpMatrix.o SpMatObj.o \
	List.o ListObj.o \
	Hash.o HashObj.o \
	object.o objectlib.o \
	typeobj.o \
	Hobj.o \
	function.o \
	IVect.o  IVectObj.o \
	Interf.o Interf-IN.o \
	P_PList.o  P_PListObj.o \
	Datas.o Datas-IN.o Stack.o \
	Cnumeric.o \
	Perm.o \
	File.o FileObj.o \
	Alloc.o \
	none.o \
	matint.o \
	MaxpObj.o MaxpMatrix.o \
	cells.o cellsobj.o Sptriplet.o \
	pr-output.o bhash.o \
	serial.o \
	SpColMatrix.o SpColMatObj.o \
	eframe.o mtlb.o \
	IMatrix.o IMatObj.o IMatOps.o


OLD=	gsort.o qsort.o	


OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

main :: 
	cc -g -Wall -Dlinux -fwritable-strings -I../../src/include main.c -o main ../../libs/nsp.a ../../libs/zcalelm.a \
	../../libs/zblas.a -lm -ldl -lg2c -ltermcap

%.X : %.c 
	protoize -k -c -I../ $*.c 
	egrep -v "/usr/|nsp_include" $*.c.X | grep -v "static " | sed -e 's+/\*[^/]*/++g' > $*.X 
	rm -f $*.c.X

