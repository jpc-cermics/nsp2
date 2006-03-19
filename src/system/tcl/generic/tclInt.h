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

/** Attention chez nous VOID veut dire autre chose !!! XXXXXX **/
#undef VOID  

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
#include "tclRegexp.h"
#include "dstring.h"

#include <ctype.h>
#ifdef NO_LIMITS_H
#   include "../compat/limits.h"
#else
#   include <limits.h>
#endif
#ifdef NO_STDLIB_H
#   include "../compat/stdlib.h"
#else
#   include <stdlib.h>
#endif
#ifdef NO_STRING_H
#include "../compat/string.h"
#else
#include <string.h>
#endif
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
extern CONST TclFileAttrProcs   tclpFileAttrProcs[];

/*
 *----------------------------------------------------------------
 * Procedures shared among Tcl modules but not used by the outside
 * world:
 *----------------------------------------------------------------
 */

extern int		TclChdir (char *dirName);
extern int		TclDoGlob (char *separators, Tcl_DString *headPtr,
				   char *tail,NspSMatrix *S);

extern int		TclFileCopyCmd (int argc, char **argv,int force ) ;
extern int 		TclFileDeleteCmd (int argc, char **argv,int force );
extern int		TclFileMakeDirsCmd ( int argc, char **argv) ;
extern int		TclFileRenameCmd ( int argc, char **argv,int force) ;
extern void		TclFinalizeEnvironment (void);

extern char *		TclGetCwd (void);
extern char *		TclGetEnv (CONST char *name);
extern char *		TclGetExtension (char *name);

extern char *		TclGetUserHome (char *name, Tcl_DString *bufferPtr);

extern int		TclpCopyFile (char *source, char *dest);
extern int              TclpCopyDirectory (char *source,char *dest, Tcl_DString *errorPtr);
extern int              TclpCreateDirectory (char *path);
extern int              TclpDeleteFile (char *path);
extern unsigned long	TclpGetClicks (void);
extern unsigned long	TclpGetSeconds (void);
extern int		TclpGetTimeZone (unsigned long time);
extern char *		TclpGetTZName (void);
extern int		TclpListVolumes (Stack stack,int n);
extern int              TclpRemoveDirectory (char *path, int recursive, Tcl_DString *errorPtr);
extern int              TclpRenameFile (char *source, char *dest);
extern char *		TclpSetEnv (CONST char *name, CONST char *value);

extern void		TclPlatformExit (int status);
extern void		TclPlatformInit (void);
extern char *		TclWordEnd (char *start, char *lastChar, int nested, int *semiPtr);

extern void             Tcl_CreateExitHandler();

extern int              TclMatchFiles(char *separators,  Tcl_DString *dirPtr, char *pattern, char *tail, NspSMatrix *S);

/*
 * The macro below is used to modify a "char" value (e.g. by casting
 * it to an unsigned character) so that it can be used safely with
 * macros such as isspace.
 */

#define UCHAR(c) ((unsigned char) (c))

#endif /* _TCLINT */

