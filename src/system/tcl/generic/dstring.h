#ifndef TCL_DSTRING 
#define TCL_DSTRING 

/*
 * The structure defined below is used to hold dynamic strings.  The only
 * field that clients should use is the string field, and they should
 * never modify it.
 * 
 * extracted form tcl:
 * 
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#define TCL_DSTRING_STATIC_SIZE 200

typedef struct nsp_tcldstring_  nsp_tcldstring ;

struct nsp_tcldstring_ {
    char *string;		/* Points to beginning of string:  either
				 * staticSpace below or a malloced array. */
    int length;			/* Number of non-NULL characters in the
				 * string. */
    int spaceAvl;		/* Total number of bytes available for the
				 * string and its terminating NULL char. */
    char staticSpace[TCL_DSTRING_STATIC_SIZE];
				/* Space to use in common case where string
				 * is small. */
};

#define nsp_tcldstring_length(dsPtr) ((dsPtr)->length)
#define nsp_tcldstring_value(dsPtr) ((dsPtr)->string)

extern char *nsp_tcldstring_append (nsp_tcldstring *dsPtr, const char *string, int length);
extern char *nsp_tcldstring_appendElement ( nsp_tcldstring *dsPtr, const char *string);
extern void nsp_tcldstring_free (nsp_tcldstring *dsPtr);
extern void nsp_tcldstring_init (nsp_tcldstring *dsPtr);
extern void nsp_tcldstring_set_length (nsp_tcldstring *dsPtr, int length);

#endif
