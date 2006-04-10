# Generated automatically from Makefile.in by configure.
SHELL = /bin/sh

SCIDIR=../../../../
SCIDIR1=..\..\..\..\

include ../../../../Makefile.incl

CFLAGS_I = $(CC_OPTIONS) 

#-DSTANDALONE -MMD

CFLAGS =${CFLAGS_I} -I../generic 

UNIX_OBJS = tclUnixFCmd.o tclUnixFile.o 	tclUnixInit.o
WIN32_OBJS = tclWinFCmd.o tclWinFile.o 	tclWinError.o tclWinInit.o

GENERIC_OBJS =  tclCmdAZ.o tclfiles.o tclIOUtil.o tclUtil.o Tobedone.o tclEnv.o 

OBJS = ${GENERIC_OBJS} ${WIN32_OBJS}

all :: ${OBJS}

clean::
	rm -f *.a *.o libtcl* core errs *~ \#* TAGS *.E a.out errors tclsh tcltest lib.exp

distclean:: clean


GENERIC_DIR=../generic

# Object files used on all Unix systems:

regexp.o: $(GENERIC_DIR)/regexp.c
	@echo "compiling regexp.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/regexp.c

tclCmdAZ.o: $(GENERIC_DIR)/tclCmdAZ.c
	@echo "compiling tclCmdAZ.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclCmdAZ.c

tclfiles.o: $(GENERIC_DIR)/tclfiles.c
	@echo "compiling tclfiles.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclfiles.c

tclEnv.o: $(GENERIC_DIR)/tclEnv.c
	@echo "compiling tclEnv.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclEnv.c

Tobedone.o: $(GENERIC_DIR)/Tobedone.c
	@echo "compiling Tobedone.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/Tobedone.c

tclIOUtil.o: $(GENERIC_DIR)/tclIOUtil.c
	@echo "compiling tclIOUtil.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclIOUtil.c

tclMain.o: $(GENERIC_DIR)/tclMain.c
	@echo "compiling tclMain.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclMain.c

tclPosixStr.o: $(GENERIC_DIR)/tclPosixStr.c
	@echo "compiling tclPosixStr.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclPosixStr.c

tclUtil.o: $(GENERIC_DIR)/tclUtil.c
	@echo "compiling tclUtil.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/tclUtil.c

dstring.o: $(GENERIC_DIR)/dstring.c
	@echo "compiling dstring.c"
	@$(CC) -c $(CFLAGS) $(GENERIC_DIR)/dstring.c

tclWinFCmd.o: tclWinFCmd.c
	@echo "compiling tclWinFCmd.c"
	@$(CC) -c $(CFLAGS) tclWinFCmd.c

tclWinFile.o: tclWinFile.c
	@echo "compiling tclWinFile.c"
	@$(CC) -c $(CFLAGS) tclWinFile.c

tclWinError.o: tclWinError.c
	@echo "compiling tclWinError.c"
	@$(CC) -c $(CFLAGS) tclWinError.c

tclWinInit.o: tclWinInit.c 
	@echo "compiling tclWinInit.c"
	@$(CC) -c $(CFLAGS) tclWinInit.c

