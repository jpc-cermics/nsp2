#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib 

OBJSC = Matrix.obj MatOps.obj MatObj.obj  \
	BMatrix.obj BMatObj.obj \
	SMatrix.obj SMatObj.obj \
	PMatrix.obj PMatObj.obj \
	SpMatrix.obj SpMatObj.obj \
	List.obj ListObj.obj \
	Hash.obj HashObj.obj \
	object.obj objectlib.obj \
	typeobj.obj Hobj.obj function.obj \
	IVect.obj  IVectObj.obj \
	Interf.obj Interf-IN.obj \
	P_PList.obj  P_PListObj.obj \
	Datas.obj Datas-IN.obj Stack.obj \
	Cnumeric.obj Perm.obj \
	File.obj FileObj.obj Alloc.obj none.obj \
	matint.obj MaxpObj.obj MaxpMatrix.obj \
	cells.obj cellsobj.obj Sptriplet.obj \
	pr-output.obj bhash.obj serial.obj \
	SpColMatrix.obj SpColMatObj.obj \
	eframe.obj mtlb.obj \
	IMatrix.obj IMatObj.obj IMatOps.obj SMio.obj SMioObj.obj \
	SpMaxpColMatrix.obj SpMaxpColMatObj.obj \
	RMatrix.obj RMatObj.obj

OLD=	gsort.obj qsort.obj	

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS) 
FFLAGS = $(FC_OPTIONS)

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

#=====================================================
#dependencies generated with make depend 
#=====================================================

include Makefile.deps 
