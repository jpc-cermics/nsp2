SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

OBJSC = Parse_exprs.o Parse.o Parse_check.o \
	Tokenizer.o Eval.o FuncEval.o Parse-IN.o \
	LibsTab.o PList.o PListBase.o reader_rl.o nsp_io.o astnode.o

OBJSF=

include ../../Makefile.incl

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

all :: lsci.o 

#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================
