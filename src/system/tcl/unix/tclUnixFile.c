/* 
 * tclUnixFile.c --
 *
 *      This file contains wrappers around UNIX file handling functions.
 *      These wrappers mask differences between Windows and UNIX.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclUnixFile.c 1.48 97/07/07 16:38:11
 */

#include "tclInt.h"
#include "tclPort.h"

/*
 * The variable below caches the name of the current working directory
 * in order to avoid repeated calls to getcwd.  The string is malloc-ed.
 * NULL means the cache needs to be refreshed.
 */

static char *currentDir =  NULL;
static int currentDirExitHandlerSet = 0;

/*
 * The variable below is set if the exit routine for deleting the string
 * containing the executable name has been registered.
 */

static int executableNameExitHandlerSet = 0;

extern pid_t waitpid (pid_t pid, int *stat_loc, int options);

/*
 * Static routines for this file:
 */

static void	FreeCurrentDir (ClientData clientData);
static void	FreeExecutableName (ClientData clientData);

/*
 *----------------------------------------------------------------------
 *
 * FreeCurrentDir --
 *
 *	Frees the string stored in the currentDir variable. This routine
 *	is registered as an exit handler and will be called during shutdown.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Frees the memory occuppied by the currentDir value.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static void
FreeCurrentDir(clientData)
     ClientData clientData;	/* Not used. */
{
  if (currentDir != (char *) NULL) {
    ckfree(currentDir);
    currentDir = (char *) NULL;
    currentDirExitHandlerSet = 0;
  }
}


/*
 *----------------------------------------------------------------------
 *
 * FreeExecutableName --
 *
 *	Frees the string stored in the tclExecutableName variable. This
 *	routine is registered as an exit handler and will be called
 *	during shutdown.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Frees the memory occuppied by the tclExecutableName value.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
static void
FreeExecutableName(clientData)
     ClientData clientData;	/* Not used. */
{
  if (tclExecutableName != (char *) NULL) {
    ckfree(tclExecutableName);
    tclExecutableName = (char *) NULL;
  }
}


/*
 *----------------------------------------------------------------------
 *
 * nsp_chdir --
 *
 *	Change the current working directory.
 *
 * Results:
 *	The result is a standard Tcl result.  If an error occurs and 
 *	interp isn't NULL, an error message is left in interp->result.
 *
 * Side effects:
 *	The working directory for this application is changed.  Also
 *	the cache maintained used by nsp_get_cwd is deallocated and
 *	set to NULL.
 *
 *----------------------------------------------------------------------
 */

int nsp_chdir(char *dirName) /* Path to new working directory. */
{
  if (currentDir != NULL) {
    ckfree(currentDir);
    currentDir = NULL;
  }
  if (chdir(dirName) != 0) {
    nsp_posix_error();
    Scierror("Error: couldn't change working directory to \"%s\"\n",  dirName);
    return TCL_ERROR;
  }
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * nsp_get_cwd --
 *
 *	Return the path name of the current working directory.
 *
 * Results:
 *	The result is the full path name of the current working
 *	directory, or NULL if an error occurred while figuring it out.
 *	The returned string is owned by the nsp_get_cwd routine and must
 *	not be freed by the caller.  If an error occurs and interp
 *	isn't NULL, an error message is left in interp->result.
 *
 * Side effects:
 *	The path name is cached to avoid having to recompute it
 *	on future calls;  if it is already cached, the cached
 *	value is returned.
 *
 *----------------------------------------------------------------------
 */

char *nsp_get_cwd(void)
{
    char buffer[MAXPATHLEN+1];
    if (currentDir == NULL) {
      if (!currentDirExitHandlerSet) {
	currentDirExitHandlerSet = 1;
	nsp_create_exit_handler(FreeCurrentDir, (ClientData) NULL);
      }
#ifdef USEGETWD
      if ((int)getwd(buffer) == (int)NULL) {
	Scierror("Error: getting working directory name: %s\n", buffer);
	return NULL;
      }
#else
      if (getcwd(buffer, MAXPATHLEN+1) == NULL) 
	{
	  if (errno == ERANGE) {
	    Scierror("working directory name is too long");
	  } else {
	    nsp_posix_error();
	    Scierror("Error: getting working directory name\n");
	  }
	  return NULL;
	}
#endif
      currentDir = (char *) ckalloc((unsigned) (strlen(buffer) + 1));
      strcpy(currentDir, buffer);
    }
    return currentDir;
}

/*
 *----------------------------------------------------------------------
 *
 * nsp_find_executable --
 *
 *	This procedure computes the absolute path name of the current
 *	application, given its argv[0] value.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The variable tclExecutableName gets filled in with the file
 *	name for the application, if we figured it out.  If we couldn't
 *	figure it out, nsp_find_executable is set to NULL.
 *
 *----------------------------------------------------------------------
 */

void
nsp_find_executable(argv0)
    char *argv0;		/* The value of the application's argv[0]. */
{
    char *name, *p, *cwd;
    nsp_tcldstring buffer;
    int length;
    struct stat statBuf;

    fprintf(stderr,"Executable : argv %s \n",argv0); /* XXXXX */

    nsp_tcldstring_init(&buffer);
    if (tclExecutableName != NULL) {
	ckfree(tclExecutableName);
	tclExecutableName = NULL;
    }

    name = argv0;
    for (p = name; *p != 0; p++) {
	if (*p == '/') {
	    /*
	     * The name contains a slash, so use the name directly
	     * without doing a path search.
	     */

	    goto gotName;
	}
    }

    p = getenv("PATH");
    if (p == NULL) {
	/*
	 * There's no PATH environment variable; use the default that
	 * is used by sh.
	 */

	p = ":/bin:/usr/bin";
    }

    /*
     * Search through all the directories named in the PATH variable
     * to see if argv[0] is in one of them.  If so, use that file
     * name.
     */

    while (*p != 0) {
	while (isspace(UCHAR(*p))) {
	    p++;
	}
	name = p;
	while ((*p != ':') && (*p != 0)) {
	    p++;
	}
	nsp_tcldstring_set_length(&buffer, 0);
	if (p != name) {
	    nsp_tcldstring_append(&buffer, name, p-name);
	    if (p[-1] != '/') {
		nsp_tcldstring_append(&buffer, "/", 1);
	    }
	}
	nsp_tcldstring_append(&buffer, argv0, -1);
	if ((access(nsp_tcldstring_value(&buffer), X_OK) == 0)
		&& (stat(nsp_tcldstring_value(&buffer), &statBuf) == 0)
		&& S_ISREG(statBuf.st_mode)) {
	    name = nsp_tcldstring_value(&buffer);
	    goto gotName;
	}
	if (*p == 0) {
	    break;
	}
	p++;
    }
    goto done;

    /*
     * If the name starts with "/" then just copy it to tclExecutableName.
     */

    gotName:
    if (name[0] == '/')  {
	tclExecutableName = (char *) ckalloc((unsigned) (strlen(name) + 1));
	strcpy(tclExecutableName, name);
	goto done;
    }

    /*
     * The name is relative to the current working directory.  First
     * strip off a leading "./", if any, then add the full path name of
     * the current working directory.
     */

    if ((name[0] == '.') && (name[1] == '/')) {
	name += 2;
    }
    cwd = nsp_get_cwd();
    if (cwd == NULL) {
	tclExecutableName = NULL;
	goto done;
    }
    length = strlen(cwd);
    tclExecutableName = (char *) ckalloc((unsigned)
	    (length + strlen(name) + 2));
    strcpy(tclExecutableName, cwd);
    tclExecutableName[length] = '/';
    strcpy(tclExecutableName + length + 1, name);

    done:
    nsp_tcldstring_free(&buffer);
    fprintf(stderr,"Executable : %s \n",tclExecutableName); /* XXXXX */
    if (!executableNameExitHandlerSet) {
        executableNameExitHandlerSet = 1;
        nsp_create_exit_handler(FreeExecutableName, (ClientData) NULL);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * nsp_get_user_home --
 *
 *	This function takes the passed in user name and finds the
 *	corresponding home directory specified in the password file.
 *
 * Results:
 *	The result is a pointer to a static string containing
 *	the new name.  If there was an error in processing the
 *	user name then the return value is NULL.  Otherwise the
 *	result is stored in bufferPtr, and the caller must call
 *	nsp_tcldstring_free(bufferPtr) to free the result.
 *
 * Side effects:
 *	Information may be left in bufferPtr.
 *
 *----------------------------------------------------------------------
 */

char *
nsp_get_user_home(name, bufferPtr)
    char *name;			/* User name to use to find home directory. */
    nsp_tcldstring *bufferPtr;	/* May be used to hold result.  Must not hold
				 * anything at the time of the call, and need
				 * not even be initialized. */
{
    struct passwd *pwPtr;

    pwPtr = getpwnam(name);
    if (pwPtr == NULL) {
	endpwent();
	return NULL;
    }
    nsp_tcldstring_init(bufferPtr);
    nsp_tcldstring_append(bufferPtr, pwPtr->pw_dir, -1);
    endpwent();
    return bufferPtr->string;
}



/*
 *----------------------------------------------------------------------
 *
 * nsp_match_files --
 *
 *	This routine is used by the globbing code to search a
 *	directory for all files which match a given pattern.
 *
 * Results: 
 *	If the tail argument is NULL, then the matching files are
 *	added to the interp->result.  Otherwise, nsp_do_glob is called
 *	recursively for each matching subdirectory.  The return value
 *	is a standard Tcl result indicating whether an error occurred
 *	in globbing.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
nsp_match_files( separators, dirPtr, pattern, tail,S)
    char *separators;		/* Path separators to pass to nsp_do_glob. */
    nsp_tcldstring *dirPtr;	/* Contains path to directory to search. */
    char *pattern;		/* Pattern to match against. */
    char *tail;			/* Pointer to end of pattern. */
    NspSMatrix *S;
{
    char *dirName, *patternEnd = tail;
    char savedChar = 0;		/* Initialization needed only to prevent
				 * compiler warning from gcc. */
    DIR *d;
    struct stat statBuf;
    struct dirent *entryPtr;
    int matchHidden;
    int result = TCL_OK;
    int baseLength = nsp_tcldstring_length(dirPtr);

    /*
     * Make sure that the directory part of the name really is a
     * directory.  If the directory name is "", use the name "."
     * instead, because some UNIX systems don't treat "" like "."
     * automatically.  Keep the "" for use in generating file names,
     * otherwise "glob foo.c" would return "./foo.c".
     */

    if (dirPtr->string[0] == '\0') {
	dirName = ".";
    } else {
	dirName = dirPtr->string;
    }
    if ((stat(dirName, &statBuf) != 0) || !S_ISDIR(statBuf.st_mode)) {
	return TCL_OK;
    }

    /*
     * Check to see if the pattern needs to compare with hidden files.
     */

    if ((pattern[0] == '.')
	    || ((pattern[0] == '\\') && (pattern[1] == '.'))) {
	matchHidden = 1;
    } else {
	matchHidden = 0;
    }

    /*
     * Now open the directory for reading and iterate over the contents.
     */

    d = opendir(dirName);
    if (d == NULL) {

      /*
	 * Strip off a trailing '/' if necessary, before reporting the error.
	 */
      
	if (baseLength > 0) {
	    savedChar = dirPtr->string[baseLength-1];
	    if (savedChar == '/') {
		dirPtr->string[baseLength-1] = '\0';
	    }
	}
	nsp_posix_error();
	Scierror("Error: couldn't read directory \"%s\"\n",dirPtr->string);
	if (baseLength > 0) {
	    dirPtr->string[baseLength-1] = savedChar;
	}
	return TCL_ERROR;
    }

    /*
     * Clean up the end of the pattern and the tail pointer.  Leave
     * the tail pointing to the first character after the path separator
     * following the pattern, or NULL.  Also, ensure that the pattern
     * is null-terminated.
     */

    if (*tail == '\\') {
	tail++;
    }
    if (*tail == '\0') {
	tail = NULL;
    } else {
	tail++;
    }
    savedChar = *patternEnd;
    *patternEnd = '\0';

    while (1) {
	entryPtr = readdir(d);
	if (entryPtr == NULL) {
	    break;
	}

	/*
	 * Don't match names starting with "." unless the "." is
	 * present in the pattern.
	 */

	if (!matchHidden && (*entryPtr->d_name == '.')) {
	  continue;
	}

	/*
	 * Now check to see if the file matches.  If there are more
	 * characters to be processed, then ensure matching files are
	 * directories before calling nsp_do_glob. Otherwise, just add
	 * the file to the result.
	 */

	if (nsp_string_match(entryPtr->d_name, pattern)) {
	  nsp_tcldstring_set_length(dirPtr, baseLength);
	  nsp_tcldstring_append(dirPtr, entryPtr->d_name, -1);
	  if (tail == NULL) {
	    if (nsp_row_smatrix_append_string(S, dirPtr->string)== FAIL) 
	      return FAIL;
	  } else if ((stat(dirPtr->string, &statBuf) == 0)
		     && S_ISDIR(statBuf.st_mode)) {
	    nsp_tcldstring_append(dirPtr, "/", 1);
	    result = nsp_do_glob( separators, dirPtr, tail,S);
	    if (result != TCL_OK) {
	      break;
	    }
	  }
	}
    }
    *patternEnd = savedChar;
    
    closedir(d);
    return result;
}


