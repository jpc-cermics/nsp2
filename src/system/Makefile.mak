#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

OBJSC = files.obj System-IN.obj timer.obj \
	tcl/unix/regexp.obj\
	tcl/unix/tcl2interf.obj\
	tcl/unix/tclCmdAZ.obj\
	tcl/unix/tclEnv.obj\
	tcl/unix/tclFCmd.obj\
	tcl/unix/tclFileName.obj\
	tcl/unix/tclIOUtil.obj\
	tcl/unix/tclPosixStr.obj\
	tcl/unix/tclUnixFCmd.obj\
	tcl/unix/tclUnixFile.obj\
	tcl/unix/tclUnixInit.obj\
	tcl/unix/tclUtil.obj\
	tcl/unix/Tobedone.obj 

OBJSF=

include ../../Makefile.incl.mak

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD
FFLAGS = $(FC_OPTIONS) 

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/util/Mak2VCMak Makefile

#=====================================================
#dependencies generated with gcc -MMD 
#=====================================================
