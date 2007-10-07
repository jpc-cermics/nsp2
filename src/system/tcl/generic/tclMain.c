/* 
 * tclMain.c --
 *
 * Main program for Tcl shells and other Tcl-based applications.
 *
 * Copyright (c) 1988-1994 The Regents of the University of California.
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tclInt.h"
#include "tcl.h"

/*
 * Declarations for various library procedures and variables (don't want
 * to include tclPort.h here, because people might copy this file out of
 * the Tcl source directory to make their own modified versions).
 * Note:  "exit" should really be declared here, but there's no way to
 * declare it without causing conflicts with other definitions elsewher
 * on some systems, so it's better just to leave it out.
 */

extern int		isatty (int fd);
extern char *		strcpy (char *dst, const char *src);

#ifdef TCL_MEM_DEBUG
static char dumpFile[100];	/* Records where to dump memory allocation
				 * information. */
static int quitFlag = 0;	/* 1 means "checkmem" command was called,
				 * so the application should quit and dump
				 * memory allocation information. */
#endif



/*
 *----------------------------------------------------------------------
 *
 * Tcl_Main --
 *
 *	Main program for tclsh and most other Tcl-based applications.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done.
 *
 * Side effects:
 *	This procedure initializes the Tk world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_Main(argc, argv, appInitProc)
    int argc;			/* Number of arguments. */
    char **argv;		/* Array of argument strings. */
    Tcl_AppInitProc *appInitProc;
				/* Application-specific initialization
				 * procedure to call after most
				 * initialization but before starting to
				 * execute commands. */
{
    Tcl_Obj *prompt1NamePtr = NULL;
    Tcl_Obj *prompt2NamePtr = NULL;
    Tcl_Obj *resultPtr;
    Tcl_Obj *commandPtr = NULL;
    char buffer[1000], *args, *fileName, *bytes;
    int code, gotPartial, tty, length;
    int exitCode = 0;
    Tcl_Channel inChannel, outChannel, errChannel;

    nsp_find_executable(argv[0]);

    /*
     * Make command-line arguments available in the Tcl variables "argc"
     * and "argv".  If the first argument doesn't start with a "-" then
     * strip it off and use it as the name of a script file to process.
     */

    /*
     * Process commands from stdin until there's an end-of-file.  Note
     * that we need to fetch the standard channels again after every
     * eval, since they may have been changed.
     */
    nsp_file_rename_cmd( argc, argv,0);
    TestchmodCmd(0, argc, argv);
    TestfileCmd(0,  argc, argv);
    Tcl_GlobCmd(0,  argc, argv);
}

/*
 *---------------------------------------------------------------------------
 *
 * TestchmodCmd --
 *
 *	Implements the "testchmod" cmd.  Used when testing "file"
 *	command.  The only attribute used by the Mac and Windows platforms
 *	is the user write flag; if this is not set, the file is
 *	made read-only.  Otehrwise, the file is made read-write.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Changes permissions of specified files.
 *
 *---------------------------------------------------------------------------
 */
 
int TestchmodCmd(dummy, argc, argv)
    ClientData dummy;			/* Not used. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int i, mode;
    char *rest;

    if (argc < 2) 
      {
      usage:
	Tcl_AppendResult( "wrong # args: should be \"", argv[0],
		" mode file ?file ...?", (char *) NULL);
	return TCL_ERROR;
    }

    mode = (int) strtol(argv[1], &rest, 8);
    if (*rest != '\0') {
	goto usage;
    }

    for (i = 2; i < argc; i++) {
        nsp_tcldstring buffer;
        
        argv[i] = nsp_translate_file_name( argv[i], &buffer);
        if (argv[i] == NULL) {
            return TCL_ERROR;
        }
	if (chmod(argv[i], (unsigned) mode) != 0) {
	    Tcl_AppendResult( argv[i], ": ", nsp_posix_error(),
		    (char *) NULL);
	    return TCL_ERROR;
	}
        nsp_tcldstring_free(&buffer);
    }
    return TCL_OK;
}

int TestfileCmd(dummy, argc, argv)
    ClientData dummy;			/* Not used. */
    int argc;				/* Number of arguments. */
    char **argv;			/* Argument strings. */
{
    int force, i, j, result;
    nsp_tcldstring error, name[2];
    
    if (argc < 3) {
	return TCL_ERROR;
    }

    force = 0;
    i = 2;
    if (strcmp(argv[2], "-force") == 0) {
        force = 1;
	i = 3;
    }

    nsp_tcldstring_init(&name[0]);
    nsp_tcldstring_init(&name[1]);
    nsp_tcldstring_init(&error);

    if (argc - i > 2) {
	return TCL_ERROR;
    }

    for (j = i; j < argc; j++) {
        argv[j] = nsp_translate_file_name( argv[j], &name[j - i]);
	if (argv[j] == NULL) {
	    return TCL_ERROR;
	}
    }

    if (strcmp(argv[1], "mv") == 0) {
	result = nsp_rename_file(argv[i], argv[i + 1]);
    } else if (strcmp(argv[1], "cp") == 0) {
        result = nsp_copy_file(argv[i], argv[i + 1]);
    } else if (strcmp(argv[1], "rm") == 0) {
        result = nsp_delete_file(argv[i]);
    } else if (strcmp(argv[1], "mkdir") == 0) {
        result = nsp_create_directory(argv[i]);
    } else if (strcmp(argv[1], "cpdir") == 0) {
        result = nsp_copy_directory(argv[i], argv[i + 1], &error);
    } else if (strcmp(argv[1], "rmdir") == 0) {
        result = nsp_remove_directory(argv[i], force, &error);
    } else {
        result = TCL_ERROR;
	goto end;
    }
	
    if (result != TCL_OK) {
	if (nsp_tcldstring_value(&error)[0] != '\0') {
	    Tcl_AppendResult( nsp_tcldstring_value(&error), " ", NULL);
	}
	Tcl_AppendResult( nsp_errno_id(), (char *) NULL);
    }

    end:
    nsp_tcldstring_free(&error);
    nsp_tcldstring_free(&name[0]);
    nsp_tcldstring_free(&name[1]);

    return result;
}




