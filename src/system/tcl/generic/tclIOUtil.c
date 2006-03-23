/* 
 * tclIOUtil.c --
 *
 *	This file contains a collection of utility procedures that
 *	are shared by the platform specific IO drivers.
 *
 *	Parts of this file are based on code contributed by Karl
 *	Lehenbauer, Mark Diekhans and Peter da Silva.
 *
 * Copyright (c) 1991-1994 The Regents of the University of California.
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tclInt.h"
#include "tclPort.h"

/*
 *----------------------------------------------------------------------
 *
 * nsp_get_errno --
 *
 *	Gets the current value of the Tcl error code variable. This is
 *	currently the global variable "errno" but could in the future
 *	change to something else.
 *
 * Results:
 *	The value of the Tcl error code variable.
 *
 * Side effects:
 *	None. Note that the value of the Tcl error code variable is
 *	UNDEFINED if a call to nsp_set_errno did not precede this call.
 *
 *----------------------------------------------------------------------
 */

int
nsp_get_errno()
{
    return errno;
}

/*
 *----------------------------------------------------------------------
 *
 * nsp_set_errno --
 *
 *	Sets the Tcl error code variable to the supplied value.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Modifies the value of the Tcl error code variable.
 *
 *----------------------------------------------------------------------
 */

void
nsp_set_errno(err)
    int err;			/* The new value. */
{
    errno = err;
}

/*
 *----------------------------------------------------------------------
 *
 * nsp_posix_error --
 *
 *	This procedure is typically called after UNIX kernel calls
 *	return errors.  It stores machine-readable information about
 *	the error in $errorCode returns an information string for
 *	the caller's use.
 *
 * Results:
 *	The return value is a human-readable string describing the
 *	error.
 *
 * Side effects:
 *	The global variable $errorCode is reset.
 *
 *----------------------------------------------------------------------
 */

char *nsp_posix_error(void)
{
  char *id, *msg;
  msg = nsp_error_msg(errno);
  id = nsp_errno_id();
  /* XXXX Tcl_SetErrorCode( "POSIX", id, msg, (char *) NULL); **/
  return msg;
}
