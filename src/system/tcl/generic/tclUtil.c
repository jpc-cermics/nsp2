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


