SHELL = /bin/sh
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

OBJSC =  System-IN.o  \
	timer.o files.o \
	tcl/unix/tclCmdAZ.o \
	tcl/unix/regexp.o \
	tcl/unix/tclEnv.o \
	tcl/unix/tclFCmd.o \
	tcl/unix/tclFileName.o \
	tcl/unix/tclIOUtil.o \
	tcl/unix/tclPosixStr.o \
	tcl/unix/tclUnixFCmd.o \
	tcl/unix/tclUnixFile.o \
	tcl/unix/tclUnixInit.o \
	tcl/unix/Tobedone.o \
	tcl/unix/tcl2interf.o \
	tcl/unix/tclUtil.o

OBJSF =


all     ::
	cd tcl/unix ; make all

include ../../Makefile.incl
SCIDIR=../..

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD

include ../Make.lib

FUNDEF= System1-IN.fun

timer.o: timer.c ../include/nsp/machine.h

clean	::
	cd tcl/unix ; make clean 

distclean :: clean 


