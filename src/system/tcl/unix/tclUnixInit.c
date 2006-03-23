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
 * nsp_tclplatform_init --
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

void nsp_tclplatform_init()
{
#ifndef NO_UNAME
    struct utsname name;
#endif
    int unameOK;
    tclPlatform = TCL_PLATFORM_UNIX;
    unameOK = 0;
#ifndef NO_UNAME
    if (uname(&name) >= 0) {
      unameOK = 1;
      /*** Tcl_SetVar2(interp, "tcl_platform", "os", name.sysname,
	   TCL_GLOBAL_ONLY); 
      ***/
      /*
       * The following code is a special hack to handle differences in
       * the way version information is returned by uname.  On most
       * systems the full version number is available in name.release.
       * However, under AIX the major version number is in
       * name.version and the minor version number is in name.release.
       */
      if ((strchr(name.release, '.') != NULL) || !isdigit(name.version[0])) {
	/* 
	   Tcl_SetVar2(interp, "tcl_platform", "osVersion", name.release,
	   TCL_GLOBAL_ONLY);
	*/
      } else {
	/* 
	   Tcl_SetVar2(interp, "tcl_platform", "osVersion", name.version,
	   TCL_GLOBAL_ONLY);
	   Tcl_SetVar2(interp, "tcl_platform", "osVersion", ".",
	   TCL_GLOBAL_ONLY|TCL_APPEND_VALUE);
	   Tcl_SetVar2(interp, "tcl_platform", "osVersion", name.release,
	   TCL_GLOBAL_ONLY|TCL_APPEND_VALUE);
	*/
      }
      /* 
	 Tcl_SetVar2(interp, "tcl_platform", "machine", name.machine,
	 TCL_GLOBAL_ONLY);
      */
		
    }
#endif
    if (!unameOK) {
      /* 
	Tcl_SetVar2(interp, "tcl_platform", "os", "", TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp, "tcl_platform", "osVersion", "", TCL_GLOBAL_ONLY);
	Tcl_SetVar2(interp, "tcl_platform", "machine", "", TCL_GLOBAL_ONLY);
      */
    }
}












