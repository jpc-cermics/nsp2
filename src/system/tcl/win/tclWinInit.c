/* 
 * tclUnixInit.c --
 *
 *	Contains the Unix-specific interpreter initialization functions.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclUnixInit.c 1.26 97/08/05 20:09:25
 */

#include "tclInt.h"
#include "tclPort.h"
#if defined(__FreeBSD__)
#   include <floatingpoint.h>
#endif
#if defined(__bsdi__)
#   include <sys/param.h>
#   if _BSDI_VERSION > 199501
#	include <dlfcn.h>
#   endif
#endif

/*
 *----------------------------------------------------------------------
 *
 * TclPlatformInit --
 *
 *	Performs Unix-specific interpreter initialization related to the
 *      tcl_library and tcl_platform variables, and other platform-
 *	specific things.
 *
 * Results:
 *	None.
 *
 * XXXX : a changer : cette routine fixe tclPlatform qui doit etre 
 *        accessible aussi a partir de Scilab 
 *        le mieux serait que TclPlatFormInit renvoit une SMatrix 
 *----------------------------------------------------------------------
 */

void nsp_tclplatform_init(void)
{
  tclPlatform = TCL_PLATFORM_WINDOWS;
}



/* 
 * tclWinInit.c --
 *
 *	Contains the Windows-specific interpreter initialization functions.
 *
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 * Copyright (c) 1998-1999 by Scriptics Corporation.
 * All rights reserved.
 *
 */

#include "tclWinInt.h"
#include <winnt.h>
#include <winbase.h>
#include <lmcons.h>

/*
 * The following declaration is a workaround for some Microsoft brain damage.
 * The SYSTEM_INFO structure is different in various releases, even though the
 * layout is the same.  So we overlay our own structure on top of it so we
 * can access the interesting slots in a uniform way.
 */

typedef struct {
    WORD wProcessorArchitecture;
    WORD wReserved;
} OemId;

/*
 * The following macros are missing from some versions of winnt.h.
 */

#ifndef PROCESSOR_ARCHITECTURE_INTEL
#define PROCESSOR_ARCHITECTURE_INTEL 0
#endif
#ifndef PROCESSOR_ARCHITECTURE_MIPS
#define PROCESSOR_ARCHITECTURE_MIPS  1
#endif
#ifndef PROCESSOR_ARCHITECTURE_ALPHA
#define PROCESSOR_ARCHITECTURE_ALPHA 2
#endif
#ifndef PROCESSOR_ARCHITECTURE_PPC
#define PROCESSOR_ARCHITECTURE_PPC   3
#endif
#ifndef PROCESSOR_ARCHITECTURE_SHX  
#define PROCESSOR_ARCHITECTURE_SHX   4
#endif
#ifndef PROCESSOR_ARCHITECTURE_ARM
#define PROCESSOR_ARCHITECTURE_ARM   5
#endif
#ifndef PROCESSOR_ARCHITECTURE_IA64
#define PROCESSOR_ARCHITECTURE_IA64  6
#endif
#ifndef PROCESSOR_ARCHITECTURE_ALPHA64
#define PROCESSOR_ARCHITECTURE_ALPHA64 7
#endif
#ifndef PROCESSOR_ARCHITECTURE_MSIL
#define PROCESSOR_ARCHITECTURE_MSIL  8
#endif
#ifndef PROCESSOR_ARCHITECTURE_AMD64
#define PROCESSOR_ARCHITECTURE_AMD64 9
#endif
#ifndef PROCESSOR_ARCHITECTURE_IA32_ON_WIN64
#define PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 10
#endif
#ifndef PROCESSOR_ARCHITECTURE_UNKNOWN
#define PROCESSOR_ARCHITECTURE_UNKNOWN 0xFFFF
#endif

/*
 * The following arrays contain the human readable strings for the Windows
 * platform and processor values.
 */



/*
 * The Init script (common to Windows and Unix platforms) is
 * defined in tkInitScript.h
 */

/*
 *---------------------------------------------------------------------------
 *
 * TclpInitPlatform --
 *
 *	Initialize all the platform-dependant things like signals and
 *	floating-point error handling.
 *
 *	Called at process initialization time.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */

void
TclpInitPlatform()
{
    tclPlatform = TCL_PLATFORM_WINDOWS;

    /*
     * The following code stops Windows 3.X and Windows NT 3.51 from 
     * automatically putting up Sharing Violation dialogs, e.g, when 
     * someone tries to access a file that is locked or a drive with no 
     * disk in it.  Tcl already returns the appropriate error to the 
     * caller, and they can decide to put up their own dialog in response 
     * to that failure.  
     *
     * Under 95 and NT 4.0, this is a NOOP because the system doesn't 
     * automatically put up dialogs when the above operations fail.
     */

    SetErrorMode(SetErrorMode(0) | SEM_FAILCRITICALERRORS);

}


/*
 *---------------------------------------------------------------------------
 *
 * ToUtf --
 *
 *	Convert a char string to a UTF string.  
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */
/*
static int ToUtf(const WCHAR *wSrc, char *dst)
{
    char *start;

    start = dst;
    while (*wSrc != '\0') {
	dst += Tcl_UniCharToUtf(*wSrc, dst);
	wSrc++;
    }
    *dst = '\0';
    return (int) (dst - start);
}
*/


/* 
 * The following variables keep track of information about this DLL
 * on a per-instance basis.  Each time this DLL is loaded, it gets its own 
 * new data segment with its own copy of all static and global information.
 */

static HINSTANCE hInstance;	/* HINSTANCE of this DLL. */
static int platformId;		/* Running under NT, or 95/98? */


/*
 *----------------------------------------------------------------------
 *
 * TclWinInit --
 *
 *	This function initializes the internal state of the tcl library.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Initializes the tclPlatformId variable.
 *
 *----------------------------------------------------------------------
 */

void
TclWinInit(hInst)
    HINSTANCE hInst;		/* Library instance handle. */
{
    OSVERSIONINFO os;
    hInstance = hInst;
    os.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&os);
    platformId = os.dwPlatformId;
}


/*
 *----------------------------------------------------------------------
 *
 * TclWinGetPlatformId --
 *
 *	Determines whether running under NT, 95, or Win32s, to allow 
 *	runtime conditional code.
 *
 * Results:
 *	The return value is one of:
 *	    VER_PLATFORM_WIN32s		Win32s on Windows 3.1. (not supported)
 *	    VER_PLATFORM_WIN32_WINDOWS	Win32 on Windows 95.
 *	    VER_PLATFORM_WIN32_NT	Win32 on Windows NT
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int		
TclWinGetPlatformId()
{
  return     VER_PLATFORM_WIN32_NT ;
}






