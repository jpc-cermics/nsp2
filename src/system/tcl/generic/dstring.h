#ifndef TCL_DSTRING 
#define TCL_DSTRING 

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


extern char *Tcl_DStringAppend (Tcl_DString *dsPtr, const char *string, int length);
extern char *Tcl_DStringAppendElement ( Tcl_DString *dsPtr, const char *string);
extern void Tcl_DStringEndSublist (Tcl_DString *dsPtr);
extern void Tcl_DStringFree (Tcl_DString *dsPtr);
extern void Tcl_DStringInit (Tcl_DString *dsPtr);
extern void Tcl_DStringSetLength (Tcl_DString *dsPtr, int length);
extern void Tcl_DStringStartSublist ( Tcl_DString *dsPtr);

#endif
