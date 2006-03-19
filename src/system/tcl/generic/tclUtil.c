/* 
 * tclUtil.c --
 *
 *	This file contains utility procedures that are used by many Tcl
 *	commands.
 *
 * Copyright (c) 1987-1993 The Regents of the University of California.
 * Copyright (c) 1994-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclUtil.c 1.161 97/08/12 17:07:18
 */

#include "tclInt.h"
#include "tclPort.h"

/*
 *----------------------------------------------------------------------
 *
 * Tcl_StringMatch --
 *
 *	See if a particular string matches a particular pattern.
 *
 * Results:
 *	The return value is 1 if string matches pattern, and
 *	0 otherwise.  The matching operation permits the following
 *	special characters in the pattern: *?\[] (see the manual
 *	entry for details on what these mean).
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_StringMatch(char *string,char * pattern)
     /*     char *string;		/\* String. *\/ 
      *     char *pattern;		/\* Pattern, which may contain special 
      * 				 * characters. *\/ 
      */
{
  char c2;

  while (1) {
    /* See if we're at the end of both the pattern and the string.
     * If so, we succeeded.  If we're at the end of the pattern
     * but not at the end of the string, we failed.
     */
	
    if (*pattern == 0) {
      if (*string == 0) {
	return 1;
      } else {
	return 0;
      }
    }
    if ((*string == 0) && (*pattern != '*')) {
      return 0;
    }

    /* Check for a "*" as the next pattern character.  It matches
     * any substring.  We handle this by calling ourselves
     * recursively for each postfix of string, until either we
     * match or we reach the end of the string.
     */
	
    if (*pattern == '*') {
      pattern += 1;
      if (*pattern == 0) {
	return 1;
      }
      while (1) {
	if (Tcl_StringMatch(string, pattern)) {
	  return 1;
	}
	if (*string == 0) {
	  return 0;
	}
	string += 1;
      }
    }
    
    /* Check for a "?" as the next pattern character.  It matches
     * any single character.
     */

    if (*pattern == '?') {
      goto thisCharOK;
    }

    /* Check for a "[" as the next pattern character.  It is followed
     * by a list of characters that are acceptable, or by a range
     * (two characters separated by "-").
     */
	
    if (*pattern == '[') {
      pattern += 1;
      while (1) {
	if ((*pattern == ']') || (*pattern == 0)) {
	  return 0;
	}
	if (*pattern == *string) {
	  break;
	}
	if (pattern[1] == '-') {
	  c2 = pattern[2];
	  if (c2 == 0) {
	    return 0;
	  }
	  if ((*pattern <= *string) && (c2 >= *string)) {
	    break;
	  }
	  if ((*pattern >= *string) && (c2 <= *string)) {
	    break;
	  }
	  pattern += 2;
	}
	pattern += 1;
      }
      while (*pattern != ']') {
	if (*pattern == 0) {
	  pattern--;
	  break;
	}
	pattern += 1;
      }
      goto thisCharOK;
    }
    
    /* If the next pattern character is '/', just strip off the '/'
     * so we do exact matching on the character that follows.
     */
	
    if (*pattern == '\\') {
      pattern += 1;
      if (*pattern == 0) {
	return 0;
      }
    }

    /* There's no special character.  Just make sure that the next
     * characters of each string match.
     */
	
    if (*pattern != *string) {
      return 0;
    }

  thisCharOK: pattern += 1;
    string += 1;
  }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppendResult --
 *
 *	Display a variable number of strings with Scierror
 *
 * Results:
 *	None.
 *
 * interp is not used 
 * XXXXX : emule pour Scilab 
 *----------------------------------------------------------------------
 */

void
Tcl_AppendResult TCL_VARARGS_DEF(char *,arg1)
{
  va_list argList;
  char *string;
  /* Now go through all the argument and print them 
   */

  TCL_VARARGS_START(char *,arg1,argList);
  if (arg1 != NULL) fprintf(stdout,arg1);
  while (1) {
    string = va_arg(argList, char *);
    if (string == NULL) {
      break;
    }
    /** On peut pas enchainer des fonctions a arguments optionels **/
    /** Scierror(string); FIXME XXXXXXXX an attendant mieux **/
    fprintf(stdout,string);
  }
  fprintf(stdout,"\n");
  va_end(argList);
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringInit --
 *
 *	Initializes a dynamic string, discarding any previous contents
 *	of the string (Tcl_DStringFree should have been called already
 *	if the dynamic string was previously in use).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The dynamic string is initialized to be empty.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DStringInit(Tcl_DString *dsPtr)
     /* Tcl_DString *dsPtr;		 Pointer to structure for dynamic string. */
{
  dsPtr->string = dsPtr->staticSpace;
  dsPtr->length = 0;
  dsPtr->spaceAvl = TCL_DSTRING_STATIC_SIZE;
  dsPtr->staticSpace[0] = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringAppend --
 *
 *	Append more characters to the current value of a dynamic string.
 *
 * Results:
 *	The return value is a pointer to the dynamic string's new value.
 *
 * Side effects:
 *	Length bytes from string (or all of string if length is less
 *	than zero) are added to the current value of the string. Memory
 *	gets reallocated if needed to accomodate the string's new size.
 *
 *----------------------------------------------------------------------
 */

char *
Tcl_DStringAppend(Tcl_DString *dsPtr,CONST char * string,int  length)
     /* Tcl_DString *dsPtr;		Structure describing dynamic string. */
     /* CONST char *string;	        String to append.  If length is -1 then
      *                                 this must be null-terminated. */
     /* int length;			Number of characters from string to
      *                                 append.  If < 0, then append all of string,
      *                                 up to null at end. */
{
  int newSize;
  char *newString, *dst;
  CONST char *end;

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
    memcpy((VOID *) newString, (VOID *) dsPtr->string,
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

/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringSetLength --
 *
 *	Change the length of a dynamic string.  This can cause the
 *	string to either grow or shrink, depending on the value of
 *	length.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The length of dsPtr is changed to length and a null byte is
 *	stored at that position in the string.  If length is larger
 *	than the space allocated for dsPtr, then a panic occurs.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DStringSetLength(Tcl_DString *dsPtr, int  length)
{
  /* Tcl_DString *dsPtr;		 Structure describing dynamic string. */
  /* int length;			 New length for dynamic string. */
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

    memcpy((VOID *) newString, (VOID *) dsPtr->string,
	   (size_t) dsPtr->length);
    if (dsPtr->string != dsPtr->staticSpace) {
      ckfree(dsPtr->string);
    }
    dsPtr->string = newString;
  }
  dsPtr->length = length;
  dsPtr->string[length] = 0;
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_DStringFree --
 *
 *	Frees up any memory allocated for the dynamic string and
 *	reinitializes the string to an empty state.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The previous contents of the dynamic string are lost, and
 *	the new value is an empty string.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_DStringFree(Tcl_DString *dsPtr /* Structure describing dynamic string. */)
{
  if (dsPtr->string != dsPtr->staticSpace) {
    ckfree(dsPtr->string);
  }
  dsPtr->string = dsPtr->staticSpace;
  dsPtr->length = 0;
  dsPtr->spaceAvl = TCL_DSTRING_STATIC_SIZE;
  dsPtr->staticSpace[0] = 0;
}
