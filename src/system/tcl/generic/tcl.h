/*
 * tcl.h --
 *
 *	This header file describes the externally-visible facilities
 *	of the Tcl interpreter.
 *
 * Copyright (c) 1987-1994 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1993-1996 Lucent Technologies.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tcl.h 1.324 97/08/07 10:26:49
 */

#ifndef _TCL
#define _TCL

#include "../../dstring.h"
/*
 * When version numbers change here, must also go into the following files
 * and update the version numbers:
 *
 * library/init.tcl
 * unix/configure.in
 * unix/pkginfo
 * win/makefile.bc
 * win/makefile.vc
 *
 * The release level should be  0 for alpha, 1 for beta, and 2 for
 * final/patch.  The release serial value is the number that follows the
 * "a", "b", or "p" in the patch level; for example, if the patch level
 * is 7.6b2, TCL_RELEASE_SERIAL is 2.  It restarts at 1 whenever the
 * release level is changed, except for the final release which is 0
 * (the first patch will start at 1).
 */

/*
 * The following definitions set up the proper options for Windows
 * compilers.  We use this method because there is no autoconf equivalent.
 */

#ifndef __WIN32__
#   if defined(_WIN32) || defined(WIN32)
#	define __WIN32__
#   endif
#endif

#ifdef __WIN32__
#   ifndef STRICT
#	define STRICT
#   endif
#   ifndef USE_PROTOTYPE
#	define USE_PROTOTYPE 1
#   endif
#   ifndef HAS_STDARG
#	define HAS_STDARG 1
#   endif
#   ifndef USE_PROTOTYPE
#	define USE_PROTOTYPE 1
#   endif
#   ifndef USE_TCLALLOC
#	define USE_TCLALLOC 1
#   endif
#   ifndef STRINGIFY
#	define STRINGIFY(x)	    STRINGIFY1(x)
#	define STRINGIFY1(x)	    #x
#   endif
#endif /* __WIN32__ */

/*
 * The following definitions set up the proper options for Macintosh
 * compilers.  We use this method because there is no autoconf equivalent.
 */

#ifdef MAC_TCL
#   ifndef HAS_STDARG
#	define HAS_STDARG 1
#   endif
#   ifndef USE_TCLALLOC
#	define USE_TCLALLOC 1
#   endif
#   ifndef NO_STRERROR
#	define NO_STRERROR 1
#   endif
#endif

/* 
 * A special definition used to allow this header file to be included 
 * in resource files so that they can get obtain version information from
 * this file.  Resource compilers don't like all the C stuff, like typedefs
 * and procedure declarations, that occur below.
 */

#ifndef RESOURCE_INCLUDED

#ifndef BUFSIZ
#include <stdio.h>
#endif

/*
 * Definitions that allow Tcl functions with variable numbers of
 * arguments to be used with either varargs.h or stdarg.h.  TCL_VARARGS
 * is used in procedure prototypes.  TCL_VARARGS_DEF is used to declare
 * the arguments in a function definiton: it takes the type and name of
 * the first argument and supplies the appropriate argument declaration
 * string for use in the function definition.  TCL_VARARGS_START
 * initializes the va_list data structure and returns the first argument.
 */

#if defined(__STDC__) || defined(HAS_STDARG)
#   define TCL_VARARGS(type, name) (type name, ...)
#   define TCL_VARARGS_DEF(type, name) (type name, ...)
#   define TCL_VARARGS_START(type, name, list) (va_start(list, name), name)
#else
#   ifdef __cplusplus
#	define TCL_VARARGS(type, name) (type name, ...)
#	define TCL_VARARGS_DEF(type, name) (type va_alist, ...)
#   else
#	define TCL_VARARGS(type, name) ()
#	define TCL_VARARGS_DEF(type, name) (va_alist)
#   endif
#   define TCL_VARARGS_START(type, name, list) \
	(va_start(list), va_arg(list, type))
#endif

/*
 * Definitions that allow this header file to be used either with or
 * without ANSI C features like function prototypes.
 */

#undef _ANSI_ARGS_
#undef CONST

#if ((defined(__STDC__) || defined(SABER)) && !defined(NO_PROTOTYPE)) || defined(__cplusplus) || defined(USE_PROTOTYPE)
#   define _USING_PROTOTYPES_ 1
#   define _ANSI_ARGS_(x)	x
#   define CONST const
#else
#   define _ANSI_ARGS_(x)	()
#   define CONST
#endif

#ifdef __cplusplus
#   define extern extern "C"
#else
#   define extern extern
#endif

/*
 * Macro to use instead of "void" for arguments that must have
 * type "void *" in ANSI C;  maps them to type "char *" in
 * non-ANSI systems.
 */
#ifndef __WIN32__
#ifndef VOID
#   ifdef __STDC__
#       define VOID void
#   else
#       define VOID char
#   endif
#endif
#else /* __WIN32__ */
/*
 * The following code is copied from winnt.h
 */
#ifndef VOID
#define VOID void
typedef char CHAR;
typedef short SHORT;
typedef long LONG;
#endif
#endif /* __WIN32__ */

/*
 * Miscellaneous declarations.
 */

#ifndef NULL
#define NULL 0
#endif

#ifndef _CLIENTDATA
#   if defined(__STDC__) || defined(__cplusplus)
    typedef void *ClientData;
#   else
    typedef int *ClientData;
#   endif /* __STDC__ */
#define _CLIENTDATA
#endif


typedef struct Tcl_AsyncHandler_ *Tcl_AsyncHandler;
typedef struct Tcl_Pid_ *Tcl_Pid;

/*
 * When a TCL command returns, the interpreter contains a result from the
 * command. Programmers are strongly encouraged to use one of the
 * procedures Tcl_GetObjResult() or Tcl_GetStringResult() to read the
 * interpreter's result. See the SetResult man page for details. Besides
 * this result, the command procedure returns an integer code, which is 
 * one of the following:
 *
 * TCL_OK		Command completed normally; the interpreter's
 *			result contains	the command's result.
 * TCL_ERROR		The command couldn't be completed successfully;
 *			the interpreter's result describes what went wrong.
 * TCL_RETURN		The command requests that the current procedure
 *			return; the interpreter's result contains the
 *			procedure's return value.
 * TCL_BREAK		The command requests that the innermost loop
 *			be exited; the interpreter's result is meaningless.
 * TCL_CONTINUE		Go on to the next iteration of the current loop;
 *			the interpreter's result is meaningless.
 */

#define TCL_OK		0
#define TCL_ERROR	1
#define TCL_RETURN	2
#define TCL_BREAK	3
#define TCL_CONTINUE	4

/*
 * Procedure types defined by Tcl:
 */

typedef void (Tcl_FreeProc) (char *blockPtr);

/*
 * The following declarations either map ckalloc and ckfree to
 * malloc and free, or they map them to procedures with all sorts
 * of debugging hooks defined in tclCkalloc.c.
 */

extern char *		Tcl_Alloc (unsigned int size);
extern void		Tcl_Free (char *ptr);
extern char *		Tcl_Realloc (char *ptr, unsigned int size);
#define ckalloc(x) malloc(x)
#define ckfree(x)  free(x)
#define ckrealloc(x,y) realloc(x,y)

/*
 * Enum for different types of file paths.
 */

typedef enum Tcl_PathType {
    TCL_PATH_ABSOLUTE,
    TCL_PATH_RELATIVE,
    TCL_PATH_VOLUME_RELATIVE
} Tcl_PathType;

/*
 * Exported Tcl procedures:
 */

extern void		Tcl_AppendElement ( char *string);
extern void		Tcl_AppendResult  TCL_VARARGS(char *,arg1);
extern char		Tcl_Backslash (CONST char *src, int *readPtr);

extern int		Tcl_CommandComplete (char *cmd);

extern int		Tcl_ConvertCountedElement (CONST char *src, int length, char *dst, int flags);
extern int		Tcl_ConvertElement (CONST char *src, char *dst, int flags);
extern char *		Tcl_DbCkalloc (unsigned int size, char *file, int line);
extern int		Tcl_DbCkfree (char *ptr,  char *file, int line);
extern char *		Tcl_DbCkrealloc (char *ptr,  unsigned int size, char *file, int line);
extern void		Tcl_DeleteFileHandler (int fd);
extern void		Tcl_DetachPids (int numPids, Tcl_Pid *pidPtr);
extern char *		nsp_errno_id (void);
extern char *		nsp_error_msg (int err);
extern void		Tcl_Exit (int status);
extern void		Tcl_Finalize (void);
extern void		nsp_find_executable (char *argv0);

extern char *		Tcl_GetCwd (char *buf, int len);
extern int		nsp_get_errno (void);
extern char *		Tcl_GetHostName (void);
extern Tcl_PathType	nsp_get_path_type (char *path);
extern int		Tcl_GetServiceMode (void);
extern char *		nsp_join_path (int argc, char **argv, nsp_tcldstring *resultPtr);
extern char *		nsp_posix_error ();
extern void		Tcl_Preserve (ClientData data);
extern int		nsp_putenv (CONST char *string);
extern void		Tcl_ReapDetachedProcs (void);

extern void		Tcl_Release (ClientData clientData);
extern void		Tcl_RestartIdleTimer (void);

#define Tcl_Return Tcl_SetResult
extern int		Tcl_ScanCountedElement (CONST char *string, int length, int *flagPtr);
extern int		Tcl_ScanElement (CONST char *string, int *flagPtr);

extern int		Tcl_ServiceAll (void);
extern int		Tcl_ServiceEvent (int flags);
extern int		Tcl_SetBooleanObj (Stack stack,int n,int ival);
extern int		Tcl_SetDoubleObj (Stack stack , int i, double doubleValue);
extern void		nsp_set_errno (int err);
extern void		Tcl_SetPanicProc (void (*proc)  TCL_VARARGS(char *, format));
extern void		Tcl_SetResult (char *string, Tcl_FreeProc *freeProc);
extern int		Tcl_SetServiceMode (int mode);
extern int 		Tcl_SetStringObj (Stack stack , int i,  char *bytes, int length);
extern char *		nsp_signal_id (int sig);
extern char *		nsp_signal_msg (int sig);
extern void		Tcl_Sleep (int ms);
extern void		nsp_split_path (char *path, int *argcPtr, char ***argvPtr);
extern int		nsp_string_match (char *string, char *pattern);
#define Tcl_TildeSubst nsp_translate_file_name

extern char *		nsp_translate_file_name ( char *name, nsp_tcldstring *bufferPtr);

extern Tcl_Pid		Tcl_WaitPid (Tcl_Pid pid, int *statPtr,  int options);

extern NspObject* Tcl_NewStringObj (char *bytes, int length); 

#endif /* RESOURCE_INCLUDED */
#endif /* _TCL */
