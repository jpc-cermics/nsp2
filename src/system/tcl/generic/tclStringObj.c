/* 
 * tclStringObj.c --
 *
 *	This file contains procedures that implement string operations
 *	on Tcl objects.  To do this efficiently (i.e. to allow many
 *	appends to be done to an object without constantly reallocating
 *	the space for the string representation) we overallocate the
 *	space for the string and use the internal representation to keep
 *	track of the extra space.  Objects with this internal
 *	representation are called "expandable string objects".
 *
 * Copyright (c) 1995-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclStringObj.c 1.30 97/07/24 18:53:30
 */

#include "tclInt.h"

/*
 * Prototypes for procedures defined later in this file:
 */

