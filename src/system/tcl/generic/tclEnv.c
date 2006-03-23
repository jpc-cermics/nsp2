/* 
 * tclEnv.c --
 *
 *	Tcl support for environment variables, including a setenv
 *	procedure.  This file contains the generic portion of the
 *	environment module.  It is primarily responsible for keeping
 *	the "env" arrays in sync with the system environment variables.
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

static int cacheSize = 0;	/* Number of env strings in environCache. */
static char **environCache = NULL;
				/* Array containing all of the environment
				 * strings that Tcl has allocated. */

#ifndef USE_PUTENV
static int environSize = 0;	/* Non-zero means that the environ array was
				 * malloced and has this many total entries
				 * allocated to it (not all may be in use at
				 * once).  Zero means that the environment
				 * array is in its original static state. */
#endif

/*
 * Declarations for local procedures defined in this file:
 */

static int		FindVariable (CONST char *name,  int *lengthPtr);
static void		ReplaceString (CONST char *oldStr,  char *newStr);
void			nsp_setenv (CONST char *name, CONST char *value);
void			nsp_unsetenv (CONST char *name);


/**
 * nsp_setenv:
 * @name: Name of variable whose value is to be set.
 * @value:  New value for variable.
 * 
 *	Set an environment variable, replacing an existing value
 *	or creating a new variable if there doesn't exist a variable
 *	by the given @name.  This procedure is intended to be a
 *	stand-in for the  UNIX "setenv" procedure so that applications
 *	using that procedure will interface properly.  
 **/

void nsp_setenv(const char *name,const char *value)
{
    int index, length, nameLength;
    char *p, *oldValue;
#ifdef MAC_TCL
    if (environ == NULL) {
	environSize = TclMacCreateEnv();
    }
#endif

    /*
     * Figure out where the entry is going to go.  If the name doesn't
     * already exist, enlarge the array if necessary to make room.  If
     * the name exists, free its old entry.
     */

    index = FindVariable(name, &length);
    if (index == -1) {
#ifndef USE_PUTENV
	if ((length+2) > environSize) {
	    char **newEnviron;

	    newEnviron = (char **) ckalloc((unsigned)
		    ((length+5) * sizeof(char *)));
	    memcpy((VOID *) newEnviron, (VOID *) environ,
		    length*sizeof(char *));
	    if (environSize != 0) {
		ckfree((char *) environ);
	    }
	    environ = newEnviron;
	    environSize = length+5;
	}
	index = length;
	environ[index+1] = NULL;
#endif
	oldValue = NULL;
	nameLength = strlen(name);
    } else {
	/*
	 * Compare the new value to the existing value.  If they're
	 * the same then quit immediately (e.g. don't rewrite the
	 * value or propagate it to other interpreters).  Otherwise,
	 * when there are N interpreters there will be N! propagations
	 * of the same value among the interpreters.
	 */

	if (strcmp(value, environ[index]+length+1) == 0) {
	    return;
	}
	oldValue = environ[index];
	nameLength = length;
    }
	
    /*
     * Create a new entry.
     */

    p = (char *) ckalloc((unsigned) (nameLength + strlen(value) + 2));
    strcpy(p, name);
    p[nameLength] = '=';
    strcpy(p+nameLength+1, value);

    /*
     * Update the system environment.
     */

#ifdef USE_PUTENV
    putenv(p);
#else
    environ[index] = p;
#endif

    /*
     * Replace the old value with the new value in the cache.
     */

    ReplaceString(oldValue, p);
}

/**
 * nsp_putenv:
 * @string: Info about environment variable in the form NAME=value.
 * 
 *
 *	Sets an environment variable.  Similar to setenv except that
 *	the information is passed in a single string of the form
 *	NAME=value, rather than as separate name strings.  This procedure
 *	is intended to be a stand-in for the  UNIX "putenv" procedure.
 *	The environ array gets updated, 
 * 
 * Return value: unused.
 **/

int nsp_putenv(const char *string)
{
    int nameLength;
    char *name, *value;

    if (string == NULL) {
	return 0;
    }

    /*
     * Separate the string into name and value parts, then call
     * nsp_setenv to do all of the real work.
     */

    value = strchr(string, '=');
    if (value == NULL) {
	return 0;
    }
    nameLength = value - string;
    if (nameLength == 0) {
	return 0;
    }
    name = (char *) ckalloc((unsigned) nameLength+1);
    memcpy((VOID *) name, (VOID *) string, (size_t) nameLength);
    name[nameLength] = 0;
    nsp_setenv(name, value+1);
    ckfree(name);
    return 0;
}

/**
 * nsp_unsetenv:
 * @name: Name of variable to remove.
 * 
 *	Remove an environment variable, updating the "env" arrays
 *	This function is intended to replace the UNIX "unsetenv" function.
 **/

void nsp_unsetenv(const char *name)
	
{
  char *oldValue;
  int length, index;
#ifdef USE_PUTENV
    char *string;
#else
    char **envPtr;
#endif

#ifdef MAC_TCL
    if (environ == NULL) {
	environSize = TclMacCreateEnv();
    }
#endif

    index = FindVariable(name, &length);

    /*
     * First make sure that the environment variable exists to avoid
     * doing needless work and to avoid recursion on the unset.
     */
    
    if (index == -1) {
	return;
    }
    /*
     * Remember the old value so we can free it if Tcl created the string.
     */

    oldValue = environ[index];

    /*
     * Update the system environment.  This must be done before we 
     * update the interpreters or we will recurse.
     */

#ifdef USE_PUTENV
    string = ckalloc(length+2);
    memcpy((VOID *) string, (VOID *) name, (size_t) length);
    string[length] = '=';
    string[length+1] = '\0';
    putenv(string);
    ckfree(string);
#else
    for (envPtr = environ+index+1; ; envPtr++) {
	envPtr[-1] = *envPtr;
	if (*envPtr == NULL) {
	    break;
	}
    }
#endif

    /*
     * Replace the old value in the cache.
     */

    ReplaceString(oldValue, NULL);

}


/**
 * nsp_getenv:
 * @name:  Name of variable to find.
 *
 *	Retrieve the value of an environment variable.
 *
 * Return value: a pointer to a static string in the environment,
 *	or NULL if the value was not found.
 **/

char * nsp_getenv(const char *name)
{
    int length, index;

#ifdef MAC_TCL
    if (environ == NULL) {
	environSize = TclMacCreateEnv();
    }
#endif

    index = FindVariable(name, &length);
    if ((index != -1) &&  (*(environ[index]+length) == '=')) {
	return environ[index]+length+1;
    } else {
	return NULL;
    }
}


/**
 * ReplaceString:
 * @oldStr:  Old environment string
 * @newStr: New environment string
 * 
 *	Replace one string with another in the environment variable
 *	cache.  The cache keeps track of all of the environment
 *	variables which were modified so they can be freed later.
 *      May free the old string.
 * 
 **/

static void ReplaceString(const char *oldStr, char *newStr)
{
    int i;
    char **newCache;

    /*
     * Check to see if the old value was allocated by Tcl.  If so,
     * it needs to be deallocated to avoid memory leaks.  Note that this
     * algorithm is O(n), not O(1).  This will result in n-squared behavior
     * if lots of environment changes are being made.
     */

    for (i = 0; i < cacheSize; i++) {
	if ((environCache[i] == oldStr) || (environCache[i] == NULL)) {
	    break;
	}
    }
    if (i < cacheSize) {
	/*
	 * Replace or delete the old value.
	 */

	if (environCache[i]) {
	    ckfree(environCache[i]);
	}
	    
	if (newStr) {
	    environCache[i] = newStr;
	} else {
	    for (; i < cacheSize-1; i++) {
		environCache[i] = environCache[i+1];
	    }
	    environCache[cacheSize-1] = NULL;
	}
    } else {	
	/*
	 * We need to grow the cache in order to hold the new string.
	 */

	newCache = (char **) ckalloc((cacheSize + 5) * sizeof(char *));
	if (environCache) {
	    memcpy((VOID *) newCache, (VOID *) environCache,
		    (size_t) (cacheSize * sizeof(char*)));
	    ckfree((char *) environCache);
	}
	environCache = newCache;
	environCache[cacheSize] = (char *) newStr;
	environCache[cacheSize+1] = NULL;
	cacheSize += 5;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * FindVariable --
 *
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */


/**
 * FindVariable:
 * @name: Name of desired environment variable.
 * @searches:  Used to return length of name (for
 * successful searches) or number of non-NULL
 * entries in environ (for unsuccessful
 * 
 * Locate the entry in environ for a given name.
 *
 * Return value: The return value is the index in environ of an entry with the
 *	name "name", or -1 if there is no such entry.   The integer at
 *	*lengthPtr is filled in with the length of name (if a matching
 *	entry is found) or the length of the environ array (if no matching
 *	entry is found).
 * 
 **/

static int FindVariable(const char *name,int * lengthPtr)
{
    int i;
    register const char *p1, *p2;

    for (i = 0, p1 = environ[i]; p1 != NULL; i++, p1 = environ[i]) {
	for (p2 = name; *p2 == *p1; p1++, p2++) {
	    /* NULL loop body. */
	}
	if ((*p1 == '=') && (*p2 == '\0')) {
	    *lengthPtr = p2-name;
	    return i;
	}
    }
    *lengthPtr = i;
    return -1;
}

/**
 * nsp_finalize_environment:
 * @void: 
 * 
 *	This function releases any storage allocated by this module
 *	that isn't still in use by the global environment.  Any
 *	strings that are still in the environment will be leaked.
 * 
 **/

void nsp_finalize_environment(void)
{
  /*
   * For now we just deallocate the cache array and none of the environment
   * strings.  This may leak more memory that strictly necessary, since some
   * of the strings may no longer be in the environment.  However,
   * determining which ones are ok to delete is n-squared, and is pretty
   * unlikely, so we don't bother.
   */

  if (environCache) {
    ckfree((char *) environCache);
    environCache = NULL;
  }
}






