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
#   define EXTERN extern "C"
#else
#   define EXTERN extern
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
typedef struct Tcl_RegExp_ *Tcl_RegExp;

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

typedef void (Tcl_FreeProc) _ANSI_ARGS_((char *blockPtr));

/*
 * The structure defined below is used to hold dynamic strings.  The only
 * field that clients should use is the string field, and they should
 * never modify it.
 */

#define TCL_DSTRING_STATIC_SIZE 200
typedef struct Tcl_DString {
    char *string;		/* Points to beginning of string:  either
				 * staticSpace below or a malloced array. */
    int length;			/* Number of non-NULL characters in the
				 * string. */
    int spaceAvl;		/* Total number of bytes available for the
				 * string and its terminating NULL char. */
    char staticSpace[TCL_DSTRING_STATIC_SIZE];
				/* Space to use in common case where string
				 * is small. */
} Tcl_DString;

#define Tcl_DStringLength(dsPtr) ((dsPtr)->length)
#define Tcl_DStringValue(dsPtr) ((dsPtr)->string)
#define Tcl_DStringTrunc Tcl_DStringSetLength

/*
 * The following declarations either map ckalloc and ckfree to
 * malloc and free, or they map them to procedures with all sorts
 * of debugging hooks defined in tclCkalloc.c.
 */

EXTERN char *		Tcl_Alloc _ANSI_ARGS_((unsigned int size));
EXTERN void		Tcl_Free _ANSI_ARGS_((char *ptr));
EXTERN char *		Tcl_Realloc _ANSI_ARGS_((char *ptr,
			    unsigned int size));
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

EXTERN void		Tcl_AppendElement _ANSI_ARGS_((
			    char *string));
EXTERN void		Tcl_AppendResult _ANSI_ARGS_(
			    TCL_VARARGS(char *,arg1));
EXTERN char		Tcl_Backslash _ANSI_ARGS_((CONST char *src,
			    int *readPtr));

EXTERN int		Tcl_CommandComplete _ANSI_ARGS_((char *cmd));

EXTERN int		Tcl_ConvertCountedElement _ANSI_ARGS_((CONST char *src,
			    int length, char *dst, int flags));
EXTERN int		Tcl_ConvertElement _ANSI_ARGS_((CONST char *src,
			    char *dst, int flags));
EXTERN char *		Tcl_DbCkalloc _ANSI_ARGS_((unsigned int size,
			    char *file, int line));
EXTERN int		Tcl_DbCkfree _ANSI_ARGS_((char *ptr,
			    char *file, int line));
EXTERN char *		Tcl_DbCkrealloc _ANSI_ARGS_((char *ptr,
			    unsigned int size, char *file, int line));
EXTERN void		Tcl_DeleteFileHandler _ANSI_ARGS_((int fd));
EXTERN void		Tcl_DetachPids _ANSI_ARGS_((int numPids, Tcl_Pid *pidPtr));
EXTERN char *		Tcl_DStringAppend _ANSI_ARGS_((Tcl_DString *dsPtr,
			    CONST char *string, int length));
EXTERN char *		Tcl_DStringAppendElement _ANSI_ARGS_((
			    Tcl_DString *dsPtr, CONST char *string));
EXTERN void		Tcl_DStringEndSublist _ANSI_ARGS_((Tcl_DString *dsPtr));
EXTERN void		Tcl_DStringFree _ANSI_ARGS_((Tcl_DString *dsPtr));
EXTERN void		Tcl_DStringInit _ANSI_ARGS_((Tcl_DString *dsPtr));
EXTERN void		Tcl_DStringSetLength _ANSI_ARGS_((Tcl_DString *dsPtr,
			    int length));
EXTERN void		Tcl_DStringStartSublist _ANSI_ARGS_((
			    Tcl_DString *dsPtr));
EXTERN char *		Tcl_ErrnoId _ANSI_ARGS_((void));
EXTERN char *		Tcl_ErrnoMsg _ANSI_ARGS_((int err));
EXTERN void		Tcl_Exit _ANSI_ARGS_((int status));
EXTERN void		Tcl_Finalize _ANSI_ARGS_((void));
EXTERN void		Tcl_FindExecutable _ANSI_ARGS_((char *argv0));

EXTERN char *		Tcl_GetCwd _ANSI_ARGS_((char *buf, int len));
EXTERN int		Tcl_GetErrno _ANSI_ARGS_((void));
EXTERN char *		Tcl_GetHostName _ANSI_ARGS_((void));
EXTERN Tcl_PathType	Tcl_GetPathType _ANSI_ARGS_((char *path));
EXTERN int		Tcl_GetServiceMode _ANSI_ARGS_((void));
EXTERN char *		Tcl_JoinPath _ANSI_ARGS_((int argc, char **argv,
			    Tcl_DString *resultPtr));
EXTERN char *		Tcl_PosixError _ANSI_ARGS_(());
EXTERN void		Tcl_Preserve _ANSI_ARGS_((ClientData data));
EXTERN int		Tcl_PutEnv _ANSI_ARGS_((CONST char *string));
EXTERN void		Tcl_ReapDetachedProcs _ANSI_ARGS_((void));
EXTERN Tcl_RegExp	Tcl_RegExpCompile _ANSI_ARGS_((
			    char *string));
EXTERN int		Tcl_RegExpExec _ANSI_ARGS_((
			    Tcl_RegExp regexp, char *string, char *start));
EXTERN int		Tcl_RegExpMatch _ANSI_ARGS_((
			    char *string, char *pattern));
EXTERN void		Tcl_RegExpRange _ANSI_ARGS_((Tcl_RegExp regexp,
			    int index, char **startPtr, char **endPtr));
EXTERN void		Tcl_Release _ANSI_ARGS_((ClientData clientData));
EXTERN void		Tcl_RestartIdleTimer _ANSI_ARGS_((void));

#define Tcl_Return Tcl_SetResult
EXTERN int		Tcl_ScanCountedElement _ANSI_ARGS_((CONST char *string,
			    int length, int *flagPtr));
EXTERN int		Tcl_ScanElement _ANSI_ARGS_((CONST char *string,
			    int *flagPtr));

EXTERN int		Tcl_ServiceAll _ANSI_ARGS_((void));
EXTERN int		Tcl_ServiceEvent _ANSI_ARGS_((int flags));
EXTERN int		Tcl_SetBooleanObj _ANSI_ARGS_((Stack stack,int n,int ival));
EXTERN int		Tcl_SetDoubleObj _ANSI_ARGS_((Stack stack , int i,
			    double doubleValue));
EXTERN void		Tcl_SetErrno _ANSI_ARGS_((int err));
EXTERN void		Tcl_SetPanicProc _ANSI_ARGS_((void (*proc)
			    _ANSI_ARGS_(TCL_VARARGS(char *, format))));
EXTERN void		Tcl_SetResult _ANSI_ARGS_((
			    char *string, Tcl_FreeProc *freeProc));
EXTERN int		Tcl_SetServiceMode _ANSI_ARGS_((int mode));
EXTERN int 		Tcl_SetStringObj _ANSI_ARGS_((Stack stack , int i,
			    char *bytes, int length));
EXTERN char *		Tcl_SignalId _ANSI_ARGS_((int sig));
EXTERN char *		Tcl_SignalMsg _ANSI_ARGS_((int sig));
EXTERN void		Tcl_Sleep _ANSI_ARGS_((int ms));
EXTERN void		Tcl_SplitPath _ANSI_ARGS_((char *path,
			    int *argcPtr, char ***argvPtr));
EXTERN int		Tcl_StringMatch _ANSI_ARGS_((char *string,
			    char *pattern));
#define Tcl_TildeSubst Tcl_TranslateFileName

EXTERN char *		Tcl_TranslateFileName _ANSI_ARGS_((
			    char *name, Tcl_DString *bufferPtr));

EXTERN Tcl_Pid		Tcl_WaitPid _ANSI_ARGS_((Tcl_Pid pid, int *statPtr, 
			    int options));

EXTERN NspObject* Tcl_NewStringObj _ANSI_ARGS_((char *bytes, int length)); 

#endif /* RESOURCE_INCLUDED */
#endif /* _TCL */
