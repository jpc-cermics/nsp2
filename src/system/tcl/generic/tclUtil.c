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
 * Tcl_RegExpCompile --
 *
 *	Compile a regular expression into a form suitable for fast
 *	matching.  This procedure retains a small cache of pre-compiled
 *	regular expressions in the interpreter, in order to avoid
 *	compilation costs as much as possible.
 *
 * Results:
 *	The return value is a pointer to the compiled form of string,
 *	suitable for passing to Tcl_RegExpExec.  This compiled form
 *	is only valid up until the next call to this procedure, so
 *	don't keep these around for a long time!  If an error occurred
 *	while compiling the pattern, then NULL is returned and an error
 *	message is left in interp->result.
 *
 * Side effects:
 *	The cache of compiled regexp's in interp will be modified to
 *	hold information for string, if such information isn't already
 *	present in the cache.
 *
 *----------------------------------------------------------------------
 */

Tcl_RegExp
Tcl_RegExpCompile(char * string)
     /* char *string;			 String for which to produce
      *                                  compiled regular expression. 
      */
{
  int length;
  regexp *result;

  length = strlen(string);

  /*** Remettre un cache en place FIXME 
       for (i = 0; i < NUM_REGEXPS; i++) {
       if ((length == iPtr->patLengths[i])
       && (strcmp(string, iPtr->patterns[i]) == 0)) {
       / *
       * Move the matched pattern to the first slot in the
       * cache and shift the other patterns down one position.
       * /

       if (i != 0) {
       int j;
       char *cachedString;
	
       Cachedstring = iPtr->patterns[i];
       result = iPtr->regexps[i];
       for (j = i-1; j >= 0; j--) {
       iPtr->patterns[j+1] = iPtr->patterns[j];
       iPtr->patLengths[j+1] = iPtr->patLengths[j];
       iPtr->regexps[j+1] = iPtr->regexps[j];
       }
       iPtr->patterns[0] = cachedString;
       iPtr->patLengths[0] = length;
       iPtr->regexps[0] = result;
       }
       return (Tcl_RegExp) iPtr->regexps[0];
       }
       }
  ***/
  /*
   * No match in the cache.  Compile the string and add it to the
   * cache.
   */

  TclRegError((char *) NULL);
  result = TclRegComp(string);
  if (TclGetRegError() != NULL) {
    Tcl_AppendResult(
		     "couldn't compile regular expression pattern: ",
		     TclGetRegError(), (char *) NULL);
    return NULL;
  }

  /** Idem XXXX
      if (iPtr->patterns[NUM_REGEXPS-1] != NULL) {
      ckfree(iPtr->patterns[NUM_REGEXPS-1]);
      ckfree((char *) iPtr->regexps[NUM_REGEXPS-1]);
      }
      for (i = NUM_REGEXPS - 2; i >= 0; i--) {
      iPtr->patterns[i+1] = iPtr->patterns[i];
      iPtr->patLengths[i+1] = iPtr->patLengths[i];
      iPtr->regexps[i+1] = iPtr->regexps[i];
      }
      iPtr->patterns[0] = (char *) ckalloc((unsigned) (length+1));
      strcpy(iPtr->patterns[0], string);
      iPtr->patLengths[0] = length;
      iPtr->regexps[0] = result;
  **/ 
  return (Tcl_RegExp) result;
}


/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegExpExec --
 *
 *	Execute the regular expression matcher using a compiled form
 *	of a regular expression and save information about any match
 *	that is found.
 *
 * Results:
 *	If an error occurs during the matching operation then -1
 *	is returned and interp->result contains an error message.
 *	Otherwise the return value is 1 if a matching range is
 *	found and 0 if there is no matching range.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_RegExpExec(Tcl_RegExp  re,char * string,char * start)
     /*     Tcl_RegExp re;		/\* Compiled regular expression;  must have 
      * 				 * been returned by previous call to 
      * 				 * Tcl_RegExpCompile. *\/ 
      *     char *string;		/\* String against which to match re. *\/ 
      *     char *start;		/\* If string is part of a larger string, 
      * 				 * this identifies beginning of larger 
      * 				 * string, so that "^" won't match. *\/ 
      */
{
  int match;

  regexp *regexpPtr = (regexp *) re;
  TclRegError((char *) NULL);
  match = TclRegExec(regexpPtr, string, start);
  if (TclGetRegError() != NULL) {
    Tcl_AppendResult( "error while matching regular expression: ",
		      TclGetRegError(), (char *) NULL);
    return -1;
  }
  return match;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegExpRange --
 *
 *	Returns pointers describing the range of a regular expression match,
 *	or one of the subranges within the match.
 *
 * Results:
 *	The variables at *startPtr and *endPtr are modified to hold the
 *	addresses of the endpoints of the range given by index.  If the
 *	specified range doesn't exist then NULLs are returned.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_RegExpRange(Tcl_RegExp re,int index,char ** startPtr,char ** endPtr)
     /*     Tcl_RegExp re;		* Compiled regular expression that has 
      * 				 * been passed to Tcl_RegExpExec. *\/ 
      *     int index;			/\* 0 means give the range of the entire 
      * 				 * match, > 0 means give the range of 
      * 				 * a matching subrange.  Must be no greater 
      * 				 * than NSUBEXP. *\/ 
      *     char **startPtr;		/\* Store address of first character in 
      * 				 * (sub-) range here. *\/ 
      *     char **endPtr;		/\* Store address of character just after last 
      * 				 * in (sub-) range here. *\/ 
      */
{
  regexp *regexpPtr = (regexp *) re;

  if (index >= NSUBEXP) {
    *startPtr = *endPtr = NULL;
  } else {
    *startPtr = regexpPtr->startp[index];
    *endPtr = regexpPtr->endp[index];
  }
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_RegExpMatch --
 *
 *	See if a string matches a regular expression.
 *
 * Results:
 *	If an error occurs during the matching operation then -1
 *	is returned and interp->result contains an error message.
 *	Otherwise the return value is 1 if "string" matches "pattern"
 *	and 0 otherwise.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_RegExpMatch(char * string,char * pattern)
     /* char *string;		 String. */
     /* char *pattern;		 Regular expression to match against
      *                          string. */
{
  Tcl_RegExp re;

  re = Tcl_RegExpCompile( pattern);
  if (re == NULL) {
    return -1;
  }
  return Tcl_RegExpExec( re, string, string);
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
