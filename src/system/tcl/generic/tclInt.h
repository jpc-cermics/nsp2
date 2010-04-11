/*
 * tclInt.h --
 *
 *	Declarations of things used internally by the Tcl interpreter.
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1993-1997 Lucent Technologies.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *SCCS: @(#) tclInt.h 1.293 97/08/12 17:07:02
 */

#ifndef _TCLINT
#define _TCLINT

#include "nsp/object.h"
#include "nsp/stack.h"
#include "nsp/interf.h"
#include "../../files.h"

/*
 * Common include files needed by most of the Tcl source files are
 * included here, so that system-dependent personalizations for the
 * include files only have to be made in once place.  This results
 * in a few extra includes, but greater modularity.  The order of
 * the three groups of #includes is important.  For example, stdio.h
 * is needed by tcl.h, and the _ANSI_ARGS_ declaration in tcl.h is
 * needed by stdlib.h in some configurations.
 */

#include <stdio.h>

#include "tcl.h"

#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <stdlib.h>

#if defined(__STDC__) || defined(HAS_STDARG)
#   include <stdarg.h>
#else
#   include <varargs.h>
#endif

/*
 * The following macros are used to specify the runtime platform
 * setting of the tclPlatform variable.
 */

typedef enum {
    TCL_PLATFORM_UNIX,		/* Any Unix-like OS. */
    TCL_PLATFORM_MAC,		/* MacOS. */
    TCL_PLATFORM_WINDOWS	/* Any Microsoft Windows OS. */
} TclPlatformType;

/*
 * The following types are used for getting and storing platform-specific
 * file attributes in tclFCmd.c and the various platform-versions of
 * that file. This is done to have as much common code as possible
 * in the file attributes code. For more information about the callbacks,
 * see TclFileAttrsCmd in tclFCmd.c.
 */

typedef int (TclGetFileAttrProc) (int objIndex, char *fileName, 
				  NspObject **attrObjPtrPtr);
typedef int (TclSetFileAttrProc) (int objIndex, char *fileName,
				  NspObject *val );

typedef struct TclFileAttrProcs {
    TclGetFileAttrProc *getProc; 	/* The procedure for getting attrs. */
    TclSetFileAttrProc *setProc;	/* The procedure for setting attrs. */
} TclFileAttrProcs;
    
/*
 *----------------------------------------------------------------
 * Variables shared among Tcl modules but not used by the outside world.
 *----------------------------------------------------------------
 */

extern char *			tclExecutableName;
extern TclPlatformType		tclPlatform;
extern char *			tclpFileAttrStrings[];
extern const TclFileAttrProcs   tclpFileAttrProcs[];

/*
 *----------------------------------------------------------------
 * Procedures shared among Tcl modules but not used by the outside
 * world:
 *----------------------------------------------------------------
 */

extern int		nsp_chdir (char *dirName);
extern int		nsp_do_glob (char *separators, nsp_tcldstring *headPtr,
				   char *tail,NspSMatrix *S);

extern int		nsp_file_copy_cmd (int argc, char **argv,int force ) ;
extern int 		nsp_file_delete_cmd (int argc, char **argv,int force );
extern int		nsp_file_make_dirs_cmd ( int argc, char **argv) ;
extern int		nsp_file_rename_cmd ( int argc, char **argv,int force);
extern nsp_string       nsp_absolute_file_name( char *fname);

extern void		nsp_finalize_environment (void);

extern char *		nsp_get_cwd (void);
extern char *		nsp_getenv (const char *name);
extern const char *	nsp_get_extension (const char *name);

extern char *		nsp_get_user_home (char *name, nsp_tcldstring *bufferPtr);

extern int		nsp_copy_file (char *source, char *dest);
extern int              nsp_copy_directory (char *source,char *dest, nsp_tcldstring *errorPtr);
extern int              nsp_create_directory (char *path);
extern int              nsp_delete_file (char *path);
extern int		nsp_list_volumes (Stack stack,int n);
extern int              nsp_remove_directory (char *path, int recursive, nsp_tcldstring *errorPtr);
extern int              nsp_rename_file (char *source, char *dest);
extern char *		TclpSetEnv (const char *name, const char *value);

extern void		TclPlatformExit (int status);
extern void		nsp_tclplatform_init (void);
extern char *		TclWordEnd (char *start, char *lastChar, int nested, int *semiPtr);

extern void             nsp_create_exit_handler();
extern int              nsp_match_files(char *separators,  nsp_tcldstring *dirPtr, char *pattern, char *tail, NspSMatrix *S);

extern nsp_string nsp_tail(char *fileName);
extern nsp_string nsp_dirname (char *fileName);
extern void update_exec_dir(char *filename,char *exec_dir,char *filename_exec,unsigned int length);
extern void update_exec_dir_from_dir(char *dirname,char *exec_dir,unsigned int length);



/*
 * The macro below is used to modify a "char" value (e.g. by casting
 * it to an unsigned character) so that it can be used safely with
 * macros such as isspace.
 */

#define UCHAR(c) ((unsigned char) (c))

#endif /* _TCLINT */

