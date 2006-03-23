/* 
 * nsp_tcldstring built from Tcl_Dstring from tcl/tk 
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * XXX: could be replaced by similar structure from the Glib library.
 */

#include "dstring.h"

#define ckalloc(x) malloc(x)
#define ckfree(x)  free(x)

/**
 * nsp_tcldstring_init:
 * @dsPtr: Pointer to structure for dynamic string.
 * 
 *	initializes a dynamic string, discarding any previous contents
 *	of the string (@nsp_tcldstring_free should have been called already
 *	if the dynamic string was previously in use). The dynamic string is 
 *      initialized to be empty
 * 
 **/

void nsp_tcldstring_init(nsp_tcldstring *dsPtr)
{
  dsPtr->string = dsPtr->staticSpace;
  dsPtr->length = 0;
  dsPtr->spaceAvl = TCL_DSTRING_STATIC_SIZE;
  dsPtr->staticSpace[0] = 0;
}


/**
 * nsp_tcldstring_append:
 * @dsPtr: Structure describing dynamic string.
 * @string:  String to append.  If length is -1 then this must be null-terminated.
 * @length: Number of characters from string to append. If < 0, then append all of string, 
 * up to null at end.
 * 
 * Appends more characters to the current value of a dynamic string.
 * Length bytes from @string (or all of @string if @length is less
 * than zero) are added to the current value of the string. Memory
 * gets reallocated if needed to accomodate the string's new size.
 * 
 * Return value: The return value is a pointer to the dynamic string's new value.
 **/

char *
nsp_tcldstring_append(nsp_tcldstring *dsPtr,const char * string,int  length)
{
  int newSize;
  char *newString, *dst;
  const char *end;

  if (length < 0) {
    length = strlen(string);
  }
  newSize = length + dsPtr->length;

  /*
   * Allocate a larger buffer for the string if the current one isn't
   * large enough. Allocate extra space in the new buffer so that there
   * will be room to grow before we have to allocate again.
   */

  if (newSize >= dsPtr->spaceAvl) {
    dsPtr->spaceAvl = newSize*2;
    newString = (char *) ckalloc((unsigned) dsPtr->spaceAvl);
    memcpy((void *) newString, (void *) dsPtr->string,
	   (size_t) dsPtr->length);
    if (dsPtr->string != dsPtr->staticSpace) {
      ckfree(dsPtr->string);
    }
    dsPtr->string = newString;
  }

  /*
   * Copy the new string into the buffer at the end of the old
   * one.
   */

  for (dst = dsPtr->string + dsPtr->length, end = string+length;
       string < end; string++, dst++) {
    *dst = *string;
  }
  *dst = '\0';
  dsPtr->length += length;
  return dsPtr->string;
}


/**
 * nsp_tcldstring_set_length:
 * @dsPtr: Structure describing dynamic string.
 * @length: New length for dynamic string.
 * 
 * changes the length of a dynamic string.  This can cause the
 * string to either grow or shrink, depending on the value of
 * length. The length of dsPtr is changed to length and a null byte is
 * stored at that position in the string. 
 * 
 **/

void
nsp_tcldstring_set_length(nsp_tcldstring *dsPtr, int  length)
{
  if (length < 0) {
    length = 0;
  }
  if (length >= dsPtr->spaceAvl) {
    char *newString;

    dsPtr->spaceAvl = length+1;
    newString = (char *) ckalloc((unsigned) dsPtr->spaceAvl);

    /*
     * SPECIAL NOTE: must use memcpy, not strcpy, to copy the string
     * to a larger buffer, since there may be embedded NULLs in the
     * string in some cases.
     */

    memcpy((void *) newString, (void *) dsPtr->string,
	   (size_t) dsPtr->length);
    if (dsPtr->string != dsPtr->staticSpace) {
      ckfree(dsPtr->string);
    }
    dsPtr->string = newString;
  }
  dsPtr->length = length;
  dsPtr->string[length] = 0;
}


/**
 * nsp_tcldstring_free:
 * @dsPtr: Structure describing dynamic string.
 * 
 * frees up any memory allocated for the dynamic string and
 * reinitializes the string to an empty state.
 * The previous contents of the dynamic string are lost, and
 * the new value is an empty string.
 **/

void nsp_tcldstring_free(nsp_tcldstring *dsPtr) 
{
  if (dsPtr->string != dsPtr->staticSpace) {
    ckfree(dsPtr->string);
  }
  dsPtr->string = dsPtr->staticSpace;
  dsPtr->length = 0;
  dsPtr->spaceAvl = TCL_DSTRING_STATIC_SIZE;
  dsPtr->staticSpace[0] = 0;
}
