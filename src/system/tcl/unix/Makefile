# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

SCIDIR=../../../../
SCIDIR1=..\..\..\..\

include ../../../../Makefile.incl

CFLAGS = $(CC_OPTIONS)  -DSTANDALONE -MMD

# To disable ANSI-C procedure prototypes reverse the comment characters
# on the following lines:
PROTO_FLAGS =
#PROTO_FLAGS = -DNO_PROTOTYPE

#----------------------------------------------------------------
# The information below is modified by the configure script when
# Makefile is generated from Makefile.in.  You shouldn't normally
# modify any of this stuff by hand.
#----------------------------------------------------------------

COMPAT_OBJS =		
AC_FLAGS =		 -DHAVE_GETCWD=1 -DHAVE_UNISTD_H=1 -DUSE_TERMIOS=1 -DHAVE_SYS_TIME_H=1 -DTIME_WITH_SYS_TIME=1 -DHAVE_TM_ZONE=1 -DHAVE_TM_GMTOFF=1 -DHAVE_TIMEZONE_VAR=1 -DSTDC_HEADERS=1 -DNEED_MATHERR=1 -DHAVE_SIGNED_CHAR=1 -DHAVE_SYS_IOCTL_H=1 
RANLIB =		ranlib
SRC_DIR =		.
TOP_DIR =		./..
GENERIC_DIR = 		$(TOP_DIR)/generic
COMPAT_DIR =		$(TOP_DIR)/compat
TOOL_DIR =		$(TOP_DIR)/tools
DLTEST_DIR =		./dltest
UNIX_DIR = 		.

#----------------------------------------------------------------
# The information below should be usable as is.  The configure
# script won't modify it and you shouldn't need to modify it
# either.
#----------------------------------------------------------------

CC_SWITCHES =	${CFLAGS} ${TCL_SHLIB_CFLAGS} -I${GENERIC_DIR} -I${SRC_DIR} \
${AC_FLAGS} ${MATH_FLAGS} ${GENERIC_FLAGS} ${PROTO_FLAGS} ${MEM_DEBUG_FLAGS} \
${COMPILE_DEBUG_FLAGS} ${ENV_FLAGS} -DTCL_SHLIB_EXT=\"${SHLIB_SUFFIX}\"

LIBS =		-ldl  $(MATH_LIBS) -lc

DEPEND_SWITCHES = ${CFLAGS} -I${GENERIC_DIR} -I${SRC_DIR} \
${AC_FLAGS} ${MATH_FLAGS} \
${GENERIC_FLAGS} ${PROTO_FLAGS} ${MEM_DEBUG_FLAGS} \
-DTCL_SHLIB_EXT=\"${SHLIB_SUFFIX}\"

UNIX_OBJS = tclUnixFCmd.o tclUnixFile.o 	tclUnixInit.o

GENERIC_OBJS =  regexp.o tclCmdAZ.o \
	tclFCmd.o tclFileName.o  tclIOUtil.o tclPosixStr.o \
	tclUtil.o Tobedone.o tclEnv.o  tcl2interf.o 

OBJS = ${GENERIC_OBJS} ${UNIX_OBJS} ${NOTIFY_OBJS} ${COMPAT_OBJS} 

GENERIC_HDRS = \
	$(GENERIC_DIR)/tclRegexp.h \
	$(GENERIC_DIR)/tcl.h \
	$(GENERIC_DIR)/tclInt.h \
	$(GENERIC_DIR)/tclPort.h \
	$(GENERIC_DIR)/tclPatch.h

GENERIC_SRCS = \
	$(GENERIC_DIR)/regexp.c \
	$(GENERIC_DIR)/tclAsync.c \
	$(GENERIC_DIR)/tclBasic.c \
	$(GENERIC_DIR)/tclBinary.c \
	$(GENERIC_DIR)/tclCkalloc.c \
	$(GENERIC_DIR)/tclClock.c \
	$(GENERIC_DIR)/tclCmdAZ.c \
	$(GENERIC_DIR)/tclCompExpr.c \
	$(GENERIC_DIR)/tclCompile.c \
	$(GENERIC_DIR)/tclDate.c \
	$(GENERIC_DIR)/tclEnv.c \
	$(GENERIC_DIR)/tclEvent.c \
	$(GENERIC_DIR)/tclExecute.c \
	$(GENERIC_DIR)/tclFCmd.c \
	$(GENERIC_DIR)/tclFileName.c \
	$(GENERIC_DIR)/Tobedone.c \
	$(GENERIC_DIR)/tclGet.c \
	$(GENERIC_DIR)/tclHash.c \
	$(GENERIC_DIR)/tclHistory.c \
	$(GENERIC_DIR)/tclIndexObj.c \
	$(GENERIC_DIR)/tclInterp.c \
	$(GENERIC_DIR)/tclIO.c \
	$(GENERIC_DIR)/tclIOCmd.c \
	$(GENERIC_DIR)/tcl2interf.c \
	$(GENERIC_DIR)/tclIOSock.c \
	$(GENERIC_DIR)/tclIOUtil.c \
	$(GENERIC_DIR)/tclLink.c \
	$(GENERIC_DIR)/tclListObj.c \
	$(GENERIC_DIR)/tclMain.c \
	$(GENERIC_DIR)/tclNamesp.c \
	$(GENERIC_DIR)/tclNotify.c \
	$(GENERIC_DIR)/tclObj.c \
        $(GENERIC_DIR)/tclParse.c \
	$(GENERIC_DIR)/tclPipe.c \
	$(GENERIC_DIR)/tclPkg.c \
	$(GENERIC_DIR)/tclPosixStr.c \
	$(GENERIC_DIR)/tclPreserve.c \
	$(GENERIC_DIR)/tclProc.c \
	$(GENERIC_DIR)/tclStringObj.c \
	$(GENERIC_DIR)/tclTest.c \
	$(GENERIC_DIR)/tclTestObj.c \
	$(GENERIC_DIR)/tclTimer.c \
	$(GENERIC_DIR)/tclUtil.c \
	$(GENERIC_DIR)/tclVar.c

UNIX_HDRS = \
	$(UNIX_DIR)/tclUnixPort.h

UNIX_SRCS = \
	$(UNIX_DIR)/tclAppInit.c \
	$(UNIX_DIR)/tclUnixChan.c \
	$(UNIX_DIR)/tclUnixEvent.c \
	$(UNIX_DIR)/tclUnixFCmd.c \
	$(UNIX_DIR)/tclUnixFile.c \
	$(UNIX_DIR)/tclUnixNotfy.c \
	$(UNIX_DIR)/tclUnixPipe.c \
	$(UNIX_DIR)/tclUnixSock.c \
	$(UNIX_DIR)/tclUnixTest.c \
	$(UNIX_DIR)/tclUnixInit.c

DL_SRCS = 

# Note: don't include DL_SRCS in SRCS:  most of those files won't
# compile on the current machine, and they will cause problems for
# things like "make depend".

SRCS = $(GENERIC_SRCS) $(UNIX_SRCS)

all :: ${OBJS}

clean::
	rm -f *.a *.o libtcl* core errs *~ \#* TAGS *.E a.out \
		errors tclsh tcltest lib.exp
	if test -f dltest/Makefile; then cd dltest; $(MAKE) clean; fi

distclean:: clean
	rm -rf Makefile config.status config.cache config.log tclConfig.sh \
		SUNWtcl.* prototype
	if test -f dltest/Makefile; then cd dltest; $(MAKE) distclean; fi

depend:
	makedepend -- $(DEPEND_SWITCHES) -- $(SRCS)

# Object files used on all Unix systems:

panic.o: $(GENERIC_DIR)/panic.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/panic.c

regexp.o: $(GENERIC_DIR)/regexp.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/regexp.c

tclAppInit.o: $(UNIX_DIR)/tclAppInit.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclAppInit.c

tclAsync.o: $(GENERIC_DIR)/tclAsync.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclAsync.c

tclBasic.o: $(GENERIC_DIR)/tclBasic.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclBasic.c

tclBinary.o: $(GENERIC_DIR)/tclBinary.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclBinary.c

tclCkalloc.o: $(GENERIC_DIR)/tclCkalloc.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclCkalloc.c

tclClock.o: $(GENERIC_DIR)/tclClock.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclClock.c

tclCmdAZ.o: $(GENERIC_DIR)/tclCmdAZ.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclCmdAZ.c

tclCmdIL.o: $(GENERIC_DIR)/tclCmdIL.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclCmdIL.c

tclCmdMZ.o: $(GENERIC_DIR)/tclCmdMZ.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclCmdMZ.c

tclDate.o: $(GENERIC_DIR)/tclDate.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclDate.c

tclCompExpr.o: $(GENERIC_DIR)/tclCompExpr.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclCompExpr.c

tclCompile.o: $(GENERIC_DIR)/tclCompile.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclCompile.c

tclEnv.o: $(GENERIC_DIR)/tclEnv.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclEnv.c

Tobedone.o: $(GENERIC_DIR)/Tobedone.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/Tobedone.c

tcl2interf.o: $(GENERIC_DIR)/tcl2interf.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tcl2interf.c

tclEvent.o: $(GENERIC_DIR)/tclEvent.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclEvent.c

tclExecute.o: $(GENERIC_DIR)/tclExecute.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclExecute.c

tclFCmd.o: $(GENERIC_DIR)/tclFCmd.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclFCmd.c

tclFileName.o: $(GENERIC_DIR)/tclFileName.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclFileName.c

tclGet.o: $(GENERIC_DIR)/tclGet.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclGet.c

tclHash.o: $(GENERIC_DIR)/tclHash.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclHash.c

tclHistory.o: $(GENERIC_DIR)/tclHistory.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclHistory.c

tclIndexObj.o: $(GENERIC_DIR)/tclIndexObj.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclIndexObj.c

tclInterp.o: $(GENERIC_DIR)/tclInterp.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclInterp.c

tclIO.o: $(GENERIC_DIR)/tclIO.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclIO.c

tclIOCmd.o: $(GENERIC_DIR)/tclIOCmd.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclIOCmd.c

tclIOSock.o: $(GENERIC_DIR)/tclIOSock.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclIOSock.c

tclIOUtil.o: $(GENERIC_DIR)/tclIOUtil.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclIOUtil.c

tclLink.o: $(GENERIC_DIR)/tclLink.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclLink.c

tclListObj.o: $(GENERIC_DIR)/tclListObj.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclListObj.c

tclObj.o: $(GENERIC_DIR)/tclObj.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclObj.c

tclMain.o: $(GENERIC_DIR)/tclMain.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclMain.c

tclNamesp.o: $(GENERIC_DIR)/tclNamesp.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclNamesp.c

tclNotify.o: $(GENERIC_DIR)/tclNotify.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclNotify.c

tclParse.o: $(GENERIC_DIR)/tclParse.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclParse.c

tclPipe.o: $(GENERIC_DIR)/tclPipe.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclPipe.c

tclPkg.o: $(GENERIC_DIR)/tclPkg.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclPkg.c

tclPosixStr.o: $(GENERIC_DIR)/tclPosixStr.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclPosixStr.c

tclPreserve.o: $(GENERIC_DIR)/tclPreserve.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclPreserve.c

tclProc.o: $(GENERIC_DIR)/tclProc.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclProc.c

tclStringObj.o: $(GENERIC_DIR)/tclStringObj.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclStringObj.c

tclUtil.o: $(GENERIC_DIR)/tclUtil.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclUtil.c

tclVar.o: $(GENERIC_DIR)/tclVar.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclVar.c

tclTest.o: $(GENERIC_DIR)/tclTest.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclTest.c

tclTestObj.o: $(GENERIC_DIR)/tclTestObj.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclTestObj.c

tclTimer.o: $(GENERIC_DIR)/tclTimer.c
	$(CC) -c $(CC_SWITCHES) $(GENERIC_DIR)/tclTimer.c

tclUnixChan.o: $(UNIX_DIR)/tclUnixChan.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixChan.c

tclUnixEvent.o: $(UNIX_DIR)/tclUnixEvent.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixEvent.c

tclUnixFCmd.o: $(UNIX_DIR)/tclUnixFCmd.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixFCmd.c

tclUnixFile.o: $(UNIX_DIR)/tclUnixFile.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixFile.c

tclUnixNotfy.o: $(UNIX_DIR)/tclUnixNotfy.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixNotfy.c

tclUnixPipe.o: $(UNIX_DIR)/tclUnixPipe.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixPipe.c

tclUnixSock.o: $(UNIX_DIR)/tclUnixSock.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixSock.c

tclUnixTest.o: $(UNIX_DIR)/tclUnixTest.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixTest.c

tclUnixTime.o: $(UNIX_DIR)/tclUnixTime.c
	$(CC) -c $(CC_SWITCHES) $(UNIX_DIR)/tclUnixTime.c

tclUnixInit.o: $(UNIX_DIR)/tclUnixInit.c tclConfig.sh
	$(CC) -c $(CC_SWITCHES) -DTCL_LIBRARY=\"${TCL_LIBRARY}\" \
		-DTCL_PACKAGE_PATH="\"${TCL_PACKAGE_PATH}\"" \
		$(UNIX_DIR)/tclUnixInit.c

# compat binaries

fixstrtod.o: $(COMPAT_DIR)/fixstrtod.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/fixstrtod.c

getcwd.o: $(COMPAT_DIR)/getcwd.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/getcwd.c

opendir.o: $(COMPAT_DIR)/opendir.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/opendir.c

strncasecmp.o: $(COMPAT_DIR)/strncasecmp.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/strncasecmp.c

strstr.o: $(COMPAT_DIR)/strstr.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/strstr.c

strtod.o: $(COMPAT_DIR)/strtod.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/strtod.c

strtol.o: $(COMPAT_DIR)/strtol.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/strtol.c

strtoul.o: $(COMPAT_DIR)/strtoul.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/strtoul.c

tmpnam.o: $(COMPAT_DIR)/tmpnam.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/tmpnam.c

waitpid.o: $(COMPAT_DIR)/waitpid.c
	$(CC) -c $(CC_SWITCHES) $(COMPAT_DIR)/waitpid.c

.c.o:
	$(CC) -c $(CC_SWITCHES) $<

