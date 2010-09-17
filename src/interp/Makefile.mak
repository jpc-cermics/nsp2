#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

OBJSC = Parse_exprs.obj Parse.obj Parse_check.obj \
	Tokenizer.obj Eval.obj FuncEval.obj Parse-IN.obj \
	LibsTab-new.obj PList.obj PListBase.obj reader_rl.obj nsp_io.obj astnode.obj \
	scalexp.obj

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

all :: lsci.obj 

#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================
