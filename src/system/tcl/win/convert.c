/*
 * tclWinFCmd.c
 *
 *      This file implements the Windows specific portion of file manipulation 
 *      subcommands of the "file" command. 
 *
 * Copyright (c) 1996-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclWinFCmd.c 1.19 97/08/05 15:23:47
 */

#include "tclWinInt.h"

/*
 * The following constants specify the type of callback when
 * TraverseWinTree() calls the traverseProc()
 */

#define DOTREE_PRED   1     /* pre-order directory  */
#define DOTREE_POSTD  2     /* post-order directory */
#define DOTREE_F      3     /* regular file */

/*
 * Callbacks for file attributes code.
 */

static int		GetWinFileAttributes _ANSI_ARGS_((
			    int objIndex, char *fileName,
			    NspObject **attributePtrPtr));
static int		GetWinFileLongName _ANSI_ARGS_((
			    int objIndex, char *fileName,
			    NspObject **attributePtrPtr));
static int		GetWinFileShortName _ANSI_ARGS_((
			    int objIndex, char *fileName,
			    NspObject **attributePtrPtr));
static int		SetWinFileAttributes _ANSI_ARGS_((
			    int objIndex, char *fileName,
			    NspObject *attributePtr));
static int		CannotSetAttribute _ANSI_ARGS_((
			    int objIndex, char *fileName,
			    NspObject *attributePtr));

/*
 *----------------------------------------------------------------------
 *
 * ConvertFileNameFormat --
 *
 *      Returns a Tcl_Obj containing either the long or short version of the 
 *	file name.
 *
 * Results:
 *      Standard Tcl result and a Tcl_Obj in attributePtrPtr. The object
 *	will have ref count 0. If the return value is not TCL_OK,
 *	attributePtrPtr is not touched.
 *
 * Side effects:
 *      A new object is allocated if the file is valid.
 *
 *----------------------------------------------------------------------
 */

static int
ConvertFileNameFormat(
    int objIndex,		    /* The index of the attribute. */
    char *fileName,		    /* The name of the file. */
    int longShort,		    /* 0 to short name, 1 to long name. */
    NspObject **attributePtrPtr)	    /* A pointer to return the object with. */
{
  HANDLE findHandle;
  WIN32_FIND_DATA findData;
  int pathArgc, i;
  char **pathArgv, **newPathArgv;
  char *currentElement, *resultStr;
  nsp_tcldstring resultDString;
  int result = TCL_OK;
  
  Tcl_SplitPath(fileName, &pathArgc, &pathArgv);
  newPathArgv = (char **) ckalloc(pathArgc * sizeof(char *));

  for (i = 0; i < pathArgc; i++) {
    if (strcmp(pathArgv[i], ".") == 0) {
      currentElement = ckalloc(strlen(".") + 1);
      strcpy(currentElement, ".");
    } else if (strcmp(pathArgv[i], "..") == 0) {
      currentElement = ckalloc(strlen("..") + 1);
      strcpy(currentElement, "..");
    } else if ((i == 0) && (pathArgv[i][1] == ':') 
	       && (strlen(pathArgv[i]) == 3)) {
      currentElement = ckalloc(4);
      strcpy(currentElement, pathArgv[i]);
    } else if ((i == 0) && (pathArgv[i][0] == '/')
	       && (pathArgv[i][1] == '/')) {
      currentElement = ckalloc(strlen(pathArgv[i]) + 1);
      strcpy(currentElement, pathArgv[i]);
    } else {
      int useLong;

      nsp_tcldstring_init(&resultDString);
      resultStr = Tcl_JoinPath(i + 1, pathArgv, &resultDString);
      findHandle = FindFirstFile(resultStr, &findData);
      if (findHandle == INVALID_HANDLE_VALUE) {
	pathArgc = i - 1;
	AttributesPosixError( objIndex, fileName, 0);
	result = TCL_ERROR;
	nsp_tcldstring_free(&resultDString);
	goto cleanup;
      }
      if (longShort) {
	if (findData.cFileName[0] != '\0') {
	  useLong = 1;
	} else {
	  useLong = 0;
	}
      } else {
	if (findData.cAlternateFileName[0] == '\0') {
	  useLong = 1;
	} else {
	  useLong = 0;
	}
      }
      if (useLong) {
	currentElement = ckalloc(strlen(findData.cFileName) + 1);
	strcpy(currentElement, findData.cFileName);
      } else {
	currentElement = ckalloc(strlen(findData.cAlternateFileName) 
				 + 1);
	strcpy(currentElement, findData.cAlternateFileName);
      }
      nsp_tcldstring_free(&resultDString);
      FindClose(findHandle);
    }
    newPathArgv[i] = currentElement;
  }

  nsp_tcldstring_init(&resultDString);
  resultStr = Tcl_JoinPath(pathArgc, newPathArgv, &resultDString);
  *attributePtrPtr = nsp_new_string_obj(NVOID,resultStr, nsp_tcldstring_length(&resultDString));
  nsp_tcldstring_free(&resultDString);

 cleanup:
  for (i = 0; i < pathArgc; i++) {
    ckfree(newPathArgv[i]);
  }
  ckfree((char *) newPathArgv);
  return result;
}

/*
 *----------------------------------------------------------------------
 *
 * GetWinFileLongName --
 *
 *      Returns a Tcl_Obj containing the short version of the file
 *	name.
 *
 * Results:
 *      Standard Tcl result and a Tcl_Obj in attributePtrPtr. The object
 *	will have ref count 0. If the return value is not TCL_OK,
 *	attributePtrPtr is not touched.
 *
 * Side effects:
 *      A new object is allocated if the file is valid.
 *
 *----------------------------------------------------------------------
 */

static int
GetWinFileLongName(objIndex, fileName, attributePtrPtr)
     int objIndex;		/* The index of the attribute. */
     char *fileName;		/* The name of the file. */
     NspObject **attributePtrPtr;	/* A pointer to return the object with. */
{
    return ConvertFileNameFormat( objIndex, fileName, 1, attributePtrPtr);
}


/*
 *----------------------------------------------------------------------
 *
 * GetWinFileShortName --
 *
 *      Returns a Tcl_Obj containing the short version of the file
 *	name.
 *
 * Results:
 *      Standard Tcl result and a Tcl_Obj in attributePtrPtr. The object
 *	will have ref count 0. If the return value is not TCL_OK,
 *	attributePtrPtr is not touched.
 *
 * Side effects:
 *      A new object is allocated if the file is valid.
 *
 *----------------------------------------------------------------------
 */

static int
GetWinFileShortName(objIndex, fileName, attributePtrPtr)
     int objIndex;		/* The index of the attribute. */
     char *fileName;		/* The name of the file. */
     NspObject **attributePtrPtr;	/* A pointer to return the object with. */
{
    return ConvertFileNameFormat( objIndex, fileName, 0, attributePtrPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_SplitPath --
 *
 *	Split a path into a list of path components.  The first element
 *	of the list will have the same path type as the original path.
 *
 * Results:
 *	Returns a standard Tcl result.  The interpreter result contains
 *	a list of path components.
 *	*argvPtr will be filled in with the address of an array
 *	whose elements point to the elements of path, in order.
 *	*argcPtr will get filled in with the number of valid elements
 *	in the array.  A single block of memory is dynamically allocated
 *	to hold both the argv array and a copy of the path elements.
 *	The caller must eventually free this memory by calling ckfree()
 *	on *argvPtr.  Note:  *argvPtr and *argcPtr are only modified
 *	if the procedure returns normally.
 *
 * Side effects:
 *	Allocates memory.
 *
 *----------------------------------------------------------------------
 */

void
Tcl_SplitPath(path, argcPtr, argvPtr)
     char *path;			/* Pointer to string containing a path. */
     int *argcPtr;		/* Pointer to location to fill in with
				 * the number of elements in the path. */
     char ***argvPtr;		/* Pointer to place to store pointer to array
				 * of pointers to path elements. */
{
  int i, size;
  char *p;
  nsp_tcldstring buffer;
  nsp_tcldstring_init(&buffer);

  /*
   * Perform platform specific splitting.  These routines will leave the
   * result in the specified buffer.  Individual elements are terminated
   * with a null character.
   */

  p = NULL;			/* Needed only to prevent gcc warnings. */
  switch (tclPlatform) {
  case TCL_PLATFORM_UNIX:
    p = SplitUnixPath(path, &buffer);
    break;

  case TCL_PLATFORM_WINDOWS:
    p = SplitWinPath(path, &buffer);
    break;
	    
  case TCL_PLATFORM_MAC:
    p = SplitMacPath(path, &buffer);
    break;
  }

  /*
   * Compute the number of elements in the result.
   */

  size = nsp_tcldstring_length(&buffer);
  *argcPtr = 0;
  for (i = 0; i < size; i++) {
    if (p[i] == '\0') {
      (*argcPtr)++;
    }
  }
    
  /*
   * Allocate a buffer large enough to hold the contents of the
   * DString plus the argv pointers and the terminating NULL pointer.
   */

  *argvPtr = (char **) ckalloc((unsigned)
			       ((((*argcPtr) + 1) * sizeof(char *)) + size));

  /*
   * Position p after the last argv pointer and copy the contents of
   * the DString.
   */

  p = (char *) &(*argvPtr)[(*argcPtr) + 1];
  memcpy((VOID *) p, (VOID *) nsp_tcldstring_value(&buffer), (size_t) size);

  /*
   * Now set up the argv pointers.
   */

  for (i = 0; i < *argcPtr; i++) {
    (*argvPtr)[i] = p;
    while ((*p++) != '\0') {}
  }
  (*argvPtr)[i] = NULL;

  nsp_tcldstring_free(&buffer);
}

/*
 *----------------------------------------------------------------------
 *
 * SplitUnixPath --
 *
 *	This routine is used by Tcl_SplitPath to handle splitting
 *	Unix paths.
 *
 * Results:
 *	Stores a null separated array of strings in the specified
 *	Tcl_DString.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static char *
SplitUnixPath(path, bufPtr)
     char *path;			/* Pointer to string containing a path. */
     nsp_tcldstring *bufPtr;	/* Pointer to DString to use for the result. */
{
  int length;
  char *p, *elementStart;

  /*
   * Deal with the root directory as a special case.
   */

  if (path[0] == '/') {
    nsp_tcldstring_append(bufPtr, "/", 2);
    p = path+1;
  } else {
    p = path;
  }

  /*
   * Split on slashes.  Embedded elements that start with tilde will be
   * prefixed with "./" so they are not affected by tilde substitution.
   */

  for (;;) {
    elementStart = p;
    while ((*p != '\0') && (*p != '/')) {
      p++;
    }
    length = p - elementStart;
    if (length > 0) {
      if ((elementStart[0] == '~') && (elementStart != path)) {
	nsp_tcldstring_append(bufPtr, "./", 2);
      }
      nsp_tcldstring_append(bufPtr, elementStart, length);
      nsp_tcldstring_append(bufPtr, "", 1);
    }
    if (*p++ == '\0') {
      break;
    }
  }
  return nsp_tcldstring_value(bufPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * SplitWinPath --
 *
 *	This routine is used by Tcl_SplitPath to handle splitting
 *	Windows paths.
 *
 * Results:
 *	Stores a null separated array of strings in the specified
 *	Tcl_DString.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static char *
SplitWinPath(path, bufPtr)
     char *path;			/* Pointer to string containing a path. */
     nsp_tcldstring *bufPtr;	/* Pointer to DString to use for the result. */
{
  int length;
  char *p, *elementStart;

  p = ExtractWinRoot(path, bufPtr, 0);

  /*
   * Terminate the root portion, if we matched something.
   */

  if (p != path) {
    nsp_tcldstring_append(bufPtr, "", 1);
  }

  /*
   * Split on slashes.  Embedded elements that start with tilde will be
   * prefixed with "./" so they are not affected by tilde substitution.
   */

  do {
    elementStart = p;
    while ((*p != '\0') && (*p != '/') && (*p != '\\')) {
      p++;
    }
    length = p - elementStart;
    if (length > 0) {
      if ((elementStart[0] == '~') && (elementStart != path)) {
	nsp_tcldstring_append(bufPtr, "./", 2);
      }
      nsp_tcldstring_append(bufPtr, elementStart, length);
      nsp_tcldstring_append(bufPtr, "", 1);
    }
  } while (*p++ != '\0');

  return nsp_tcldstring_value(bufPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_JoinPath --
 *
 *	Combine a list of paths in a platform specific manner.
 *
 * Results:
 *	Appends the joined path to the end of the specified
 *	returning a pointer to the resulting string.  Note that
 *	the nsp_tcldstring must already be initialized.
 *
 * Side effects:
 *	Modifies the Tcl_DString.
 *
 *----------------------------------------------------------------------
 */

char *
Tcl_JoinPath(argc, argv, resultPtr)
     int argc;
     char **argv;
     nsp_tcldstring *resultPtr;	/* Pointer to previously initialized DString. */
{
  int oldLength, length, i, needsSep;
  nsp_tcldstring buffer;
  char *p, c, *dest;

  nsp_tcldstring_init(&buffer);
  oldLength = nsp_tcldstring_length(resultPtr);

  switch (tclPlatform) {
  case TCL_PLATFORM_UNIX:
    for (i = 0; i < argc; i++) {
      p = argv[i];
      /*
       * If the path is absolute, reset the result buffer.
       * Consume any duplicate leading slashes or a ./ in
       * front of a tilde prefixed path that isn't at the
       * beginning of the path.
       */

      if (*p == '/') {
	nsp_tcldstring_set_length(resultPtr, oldLength);
	nsp_tcldstring_append(resultPtr, "/", 1);
	while (*p == '/') {
	  p++;
	}
      } else if (*p == '~') {
	nsp_tcldstring_set_length(resultPtr, oldLength);
      } else if ((nsp_tcldstring_length(resultPtr) != oldLength)
		 && (p[0] == '.') && (p[1] == '/')
		 && (p[2] == '~')) {
	p += 2;
      }

      if (*p == '\0') {
	continue;
      }

      /*
       * Append a separator if needed.
       */

      length = nsp_tcldstring_length(resultPtr);
      if ((length != oldLength)
	  && (nsp_tcldstring_value(resultPtr)[length-1] != '/')) {
	nsp_tcldstring_append(resultPtr, "/", 1);
	length++;
      }

      /*
       * Append the element, eliminating duplicate and trailing
       * slashes.
       */

      nsp_tcldstring_set_length(resultPtr, (int) (length + strlen(p)));
      dest = nsp_tcldstring_value(resultPtr) + length;
      for (; *p != '\0'; p++) {
	if (*p == '/') {
	  while (p[1] == '/') {
	    p++;
	  }
	  if (p[1] != '\0') {
	    *dest++ = '/';
	  }
	} else {
	  *dest++ = *p;
	}
      }
      length = dest - nsp_tcldstring_value(resultPtr);
      nsp_tcldstring_set_length(resultPtr, length);
    }
    break;

  case TCL_PLATFORM_WINDOWS:
    /*
     * Iterate over all of the components.  If a component is
     * absolute, then reset the result and start building the
     * path from the current component on.
     */

    for (i = 0; i < argc; i++) {
      p = ExtractWinRoot(argv[i], resultPtr, oldLength);
      length = nsp_tcldstring_length(resultPtr);
		
      /*
       * If the pointer didn't move, then this is a relative path
       * or a tilde prefixed path.
       */

      if (p == argv[i]) {
	/*
	 * Remove the ./ from tilde prefixed elements unless
	 * it is the first component.
	 */

	if ((length != oldLength)
	    && (p[0] == '.')
	    && ((p[1] == '/') || (p[1] == '\\'))
	    && (p[2] == '~')) {
	  p += 2;
	} else if (*p == '~') {
	  nsp_tcldstring_set_length(resultPtr, oldLength);
	  length = oldLength;
	}
      }

      if (*p != '\0') {
	/*
	 * Check to see if we need to append a separator.
	 */

		    
	if (length != oldLength) {
	  c = nsp_tcldstring_value(resultPtr)[length-1];
	  if ((c != '/') && (c != ':')) {
	    nsp_tcldstring_append(resultPtr, "/", 1);
	  }
	}

	/*
	 * Append the element, eliminating duplicate and
	 * trailing slashes.
	 */

	length = nsp_tcldstring_length(resultPtr);
	nsp_tcldstring_set_length(resultPtr, (int) (length + strlen(p)));
	dest = nsp_tcldstring_value(resultPtr) + length;
	for (; *p != '\0'; p++) {
	  if ((*p == '/') || (*p == '\\')) {
	    while ((p[1] == '/') || (p[1] == '\\')) {
	      p++;
	    }
	    if (p[1] != '\0') {
	      *dest++ = '/';
	    }
	  } else {
	    *dest++ = *p;
	  }
	}
	length = dest - nsp_tcldstring_value(resultPtr);
	nsp_tcldstring_set_length(resultPtr, length);
      }
    }
    break;

  case TCL_PLATFORM_MAC:
    needsSep = 1;
    for (i = 0; i < argc; i++) {
      nsp_tcldstring_set_length(&buffer, 0);
      p = SplitMacPath(argv[i], &buffer);
      if ((*p != ':') && (*p != '\0')
	  && (strchr(p, ':') != NULL)) {
	nsp_tcldstring_set_length(resultPtr, oldLength);
	length = strlen(p);
	nsp_tcldstring_append(resultPtr, p, length);
	needsSep = 0;
	p += length+1;
      }

      /*
       * Now append the rest of the path elements, skipping
       * : unless it is the first element of the path, and
       * watching out for :: et al. so we don't end up with
       * too many colons in the result.
       */

      for (; *p != '\0'; p += length+1) {
	if (p[0] == ':' && p[1] == '\0') {
	  if (nsp_tcldstring_length(resultPtr) != oldLength) {
	    p++;
	  } else {
	    needsSep = 0;
	  }
	} else {
	  c = p[1];
	  if (*p == ':') {
	    if (!needsSep) {
	      p++;
	    }
	  } else {
	    if (needsSep) {
	      nsp_tcldstring_append(resultPtr, ":", 1);
	    }
	  }
	  needsSep = (c == ':') ? 0 : 1;
	}
	length = strlen(p);
	nsp_tcldstring_append(resultPtr, p, length);
      }
    }
    break;
			       
  }
  nsp_tcldstring_free(&buffer);
  return nsp_tcldstring_value(resultPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_TranslateFileName --
 *
 *	Converts a file name into a form usable by the native system
 *	interfaces.  If the name starts with a tilde, it will produce
 *	a name where the tilde and following characters have been
 *	replaced by the home directory location for the named user.
 *
 * Results:
 *	The result is a pointer to a static string containing
 *	the new name.  If there was an error in processing the
 *	name, then an error message is left in interp->result
 *	and the return value is NULL.  The result will be stored
 *	in bufferPtr; the caller must call nsp_tcldstring_free(bufferPtr)
 *	to free the name if the return value was not NULL.
 *
 * Side effects:
 *	Information may be left in bufferPtr.
 *
 *----------------------------------------------------------------------
 */

char *
Tcl_TranslateFileName( name, bufferPtr)
     char *name;			/* File name, which may begin with "~"
				 * (to indicate current user's home directory)
				 * or "~<user>" (to indicate any user's
				 * home directory). */
     nsp_tcldstring *bufferPtr;	/* May be used to hold result.  Must not hold
				 * anything at the time of the call, and need
				 * not even be initialized. */
{
  register char *p;

  /*
   * Handle tilde substitutions, if needed.
   */

  if (name[0] == '~') {
    int argc, length;
    char **argv;
    nsp_tcldstring temp;

    Tcl_SplitPath(name, &argc, &argv);
	
    /*
     * Strip the trailing ':' off of a Mac path
     * before passing the user name to DoTildeSubst.
     */

    if (tclPlatform == TCL_PLATFORM_MAC) {
      length = strlen(argv[0]);
      argv[0][length-1] = '\0';
    }
	
    nsp_tcldstring_init(&temp);
    argv[0] = DoTildeSubst( argv[0]+1, &temp);
    if (argv[0] == NULL) {
      nsp_tcldstring_free(&temp);
      ckfree((char *)argv);
      return NULL;
    }
    nsp_tcldstring_init(bufferPtr);
    Tcl_JoinPath(argc, argv, bufferPtr);
    nsp_tcldstring_free(&temp);
    ckfree((char*)argv);
  } else {
    nsp_tcldstring_init(bufferPtr);
    Tcl_JoinPath(1, &name, bufferPtr);
  }

  /*
   * Convert forward slashes to backslashes in Windows paths because
   * some system interfaces don't accept forward slashes.
   */

  if (tclPlatform == TCL_PLATFORM_WINDOWS) {
    for (p = nsp_tcldstring_value(bufferPtr); *p != '\0'; p++) {
      if (*p == '/') {
	*p = '\\';
      }
    }
  }
  return nsp_tcldstring_value(bufferPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * TclGetExtension --
 *
 *	This function returns a pointer to the beginning of the
 *	extension part of a file name.
 *
 * Results:
 *	Returns a pointer into name which indicates where the extension
 *	starts.  If there is no extension, returns NULL.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

char *
TclGetExtension(name)
     char *name;			/* File name to parse. */
{
  char *p, *lastSep;

  /*
   * First find the last directory separator.
   */

  lastSep = NULL;		/* Needed only to prevent gcc warnings. */
  switch (tclPlatform) {
  case TCL_PLATFORM_UNIX:
    lastSep = strrchr(name, '/');
    break;

  case TCL_PLATFORM_MAC:
    if (strchr(name, ':') == NULL) {
      lastSep = strrchr(name, '/');
    } else {
      lastSep = strrchr(name, ':');
    }
    break;

  case TCL_PLATFORM_WINDOWS:
    lastSep = NULL;
    for (p = name; *p != '\0'; p++) {
      if (strchr("/\\:", *p) != NULL) {
	lastSep = p;
      }
    }
    break;
  }
  p = strrchr(name, '.');
  if ((p != NULL) && (lastSep != NULL)
      && (lastSep > p)) {
    p = NULL;
  }

  /*
   * Back up to the first period in a series of contiguous dots.
   * This is needed so foo..o will be split on the first dot.
   */

  if (p != NULL) {
    while ((p > name) && *(p-1) == '.') {
      p--;
    }
  }
  return p;
}

/*
 *----------------------------------------------------------------------
 *
 * DoTildeSubst --
 *
 *	Given a string following a tilde, this routine returns the
 *	corresponding home directory.
 *
 * Results:
 *	The result is a pointer to a static string containing the home
 *	directory in native format.  If there was an error in processing
 *	the substitution, then an error message is left in interp->result
 *	and the return value is NULL.  On success, the results are appended
 * 	to resultPtr, and the contents of resultPtr are returned.
 *
 * Side effects:
 *	Information may be left in resultPtr.
 *
 *----------------------------------------------------------------------
 */

static char *
DoTildeSubst( user, resultPtr)
     char *user;			/* Name of user whose home directory should be
				 * substituted, or "" for current user. */
     nsp_tcldstring *resultPtr;	/* May be used to hold result.  Must not hold
				 * anything at the time of the call, and need
				 * not even be initialized. */
{
  char *dir;

  if (*user == '\0') {
    dir = TclGetEnv("HOME");
    if (dir == NULL) {
      Tcl_AppendResult("couldn't find HOME environment ",
		       "variable to expand path", (char *) NULL);
      return NULL;
    }
    Tcl_JoinPath(1, &dir, resultPtr);
  } else {
	
    /* lint, TclGetuserHome() always NULL under windows. */
    if (TclGetUserHome(user, resultPtr) == NULL) {	
      Tcl_AppendResult( "user \"", user, "\" doesn't exist",
			(char *) NULL);
      return NULL;
    }
  }
  return resultPtr->string;
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_GlobCmd --
 *
 *	This procedure is invoked to process the "glob" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

int int_glob (stack,rhs,opt,lhs) 
     Stack stack;int rhs,opt,lhs;
{
  NspObject *O;
  NspSMatrix *S;
  int i, noComplain=0, firstArg;
  char c;
  int result = TCL_OK;
  nsp_tcldstring buffer;
  char *separators, *head, *tail,*str;
    
  nsp_tcldstring_init(&buffer);
  separators = NULL;		/* Needed only to prevent gcc warnings. */

  if ((S=nsp_smatrix_create("",0,0,".",0))== NULLSMAT) 
    return RET_BUG;
  for (i = 1 ; i <= rhs ; i++) 
    {
      if ((str = GetString(stack,i)) == (char*)0) return RET_BUG;
      
      switch (tclPlatform) {
      case TCL_PLATFORM_UNIX:
	separators = "/";
	break;
      case TCL_PLATFORM_WINDOWS:
	separators = "/\\:";
	break;
      case TCL_PLATFORM_MAC:
	separators = (strchr(str, ':') == NULL) ? "/" : ":";
	break;
      }
      
    nsp_tcldstring_set_length(&buffer, 0);

    /*
     * Perform tilde substitution, if needed.
     */
    if (str[0] == '~') 
      {
	char *p;
	
	/*
	 * Find the first path separator after the tilde.
	 */

	for (tail = str; *tail != '\0'; tail++) {
	  if (*tail == '\\') {
	    if (strchr(separators, tail[1]) != NULL) {
	      break;
	    }
	  } else if (strchr(separators, *tail) != NULL) {
	    break;
	  }
	}
	
	/*
	 * Determine the home directory for the specified user.  Note that
	 * we don't allow special characters in the user name.
	 */
	
	c = *tail;
	*tail = '\0';
	p = strpbrk(str+1, "\\[]*?{}");
	if (p == NULL) {
	  head = DoTildeSubst( str+1, &buffer);
	} else {
	  if (!noComplain) {
	    Tcl_AppendResult("globbing characters not ",
			     "supported in user names", (char *) NULL);
	  }
	  head = NULL;
	}
	*tail = c;
	if (head == NULL) {
	  if (noComplain) {
	    continue;
	  } else {
	    result = TCL_ERROR;
	    goto done;
	  }
	}
	if (head != nsp_tcldstring_value(&buffer)) {
	  nsp_tcldstring_append(&buffer, head, -1);
	}
      } else {
	tail = str;
      }
    result = TclDoGlob( separators, &buffer, tail,S);
    if (result != TCL_OK) {
      if (noComplain) {
	continue;
      } else {
	goto done;
      }
    }
  }
 done:
  nsp_tcldstring_free(&buffer);
  if ( result == TCL_OK ) 
    {
      if ( rhs >= 1) 
	MoveObj(stack,1,(NspObject*)S);
      else 
	NthObj(1) = (NspObject*)S;
      return 1;
    }
  else
    {
      return RET_BUG;
    }
}


/*
 *----------------------------------------------------------------------
 *
 * SkipToChar --
 *
 *	This function traverses a glob pattern looking for the next
 *	unquoted occurance of the specified character at the same braces
 *	nesting level.
 *
 * Results:
 *	Updates stringPtr to point to the matching character, or to
 *	the end of the string if nothing matched.  The return value
 *	is 1 if a match was found at the top level, otherwise it is 0.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
SkipToChar(stringPtr, match)
     char **stringPtr;			/* Pointer string to check. */
     char *match;			/* Pointer to character to find. */
{
  int quoted, level;
  register char *p;

  quoted = 0;
  level = 0;

  for (p = *stringPtr; *p != '\0'; p++) {
    if (quoted) {
      quoted = 0;
      continue;
    }
    if ((level == 0) && (*p == *match)) {
      *stringPtr = p;
      return 1;
    }
    if (*p == '{') {
      level++;
    } else if (*p == '}') {
      level--;
    } else if (*p == '\\') {
      quoted = 1;
    }
  }
  *stringPtr = p;
  return 0;
}

/*
 *----------------------------------------------------------------------
 *
 * TclDoGlob --
 *
 *	This recursive procedure forms the heart of the globbing
 *	code.  It performs a depth-first traversal of the tree
 *	given by the path name to be globbed.  The directory and
 *	remainder are assumed to be native format paths.
 *
 * Results:
 *	The return value is a standard Tcl result indicating whether
 *	an error occurred in globbing.  After a normal return the
 *	result in interp will be set to hold all of the file names
 *	given by the dir and rem arguments.  After an error the
 *	result in interp will hold an error message.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
TclDoGlob( separators, headPtr, tail, S)
     char *separators;		/* String containing separator characters
				 * that should be used to identify globbing
				 * boundaries. */
     nsp_tcldstring *headPtr;	/* Completely expanded prefix. */
     char *tail;		/* The unexpanded remainder of the path. */
     NspSMatrix *S;                /* String Matrix for appending results 
				   the Matrix can be an empty Matrix 
				*/
{
  int baseLength, quoted, count;
  int result = TCL_OK;
  char *p, *openBrace, *closeBrace, *name, *firstSpecialChar, savedChar;
  char lastChar = 0;
  int length = nsp_tcldstring_length(headPtr);
  
  if (length > 0) {
    lastChar = nsp_tcldstring_value(headPtr)[length-1];
  }

  /*
   * Consume any leading directory separators, leaving tail pointing
   * just past the last initial separator.
   */

  count = 0;
  name = tail;
  for (; *tail != '\0'; tail++) {
    if ((*tail == '\\') && (strchr(separators, tail[1]) != NULL)) {
      tail++;
    } else if (strchr(separators, *tail) == NULL) {
      break;
    }
    count++;
  }

  /*
   * Deal with path separators.  On the Mac, we have to watch out
   * for multiple separators, since they are special in Mac-style
   * paths.
   */
  
  switch (tclPlatform) {
  case TCL_PLATFORM_MAC:
    if (*separators == '/') {
      if (((length == 0) && (count == 0))
	  || ((length > 0) && (lastChar != ':'))) {
	nsp_tcldstring_append(headPtr, ":", 1);
      }
    } else {
      if (count == 0) {
	if ((length > 0) && (lastChar != ':')) {
	  nsp_tcldstring_append(headPtr, ":", 1);
	}
      } else {
	if (lastChar == ':') {
	  count--;
	}
	while (count-- > 0) {
	  nsp_tcldstring_append(headPtr, ":", 1);
	}
      }
    }
    break;
  case TCL_PLATFORM_WINDOWS:
    /*
     * If this is a drive relative path, add the colon and the
     * trailing slash if needed.  Otherwise add the slash if
     * this is the first absolute element, or a later relative
     * element.  Add an extra slash if this is a UNC path.
     */

    if (*name == ':') {
      nsp_tcldstring_append(headPtr, ":", 1);
      if (count > 1) {
	nsp_tcldstring_append(headPtr, "/", 1);
      }
    } else if ((*tail != '\0')
	       && (((length > 0)
		    && (strchr(separators, lastChar) == NULL))
		   || ((length == 0) && (count > 0)))) {
      nsp_tcldstring_append(headPtr, "/", 1);
      if ((length == 0) && (count > 1)) {
	nsp_tcldstring_append(headPtr, "/", 1);
      }
    }
	    
    break;
  case TCL_PLATFORM_UNIX:
    /*
     * Add a separator if this is the first absolute element, or
     * a later relative element.
     */

    if ((*tail != '\0')
	&& (((length > 0)
	     && (strchr(separators, lastChar) == NULL))
	    || ((length == 0) && (count > 0)))) {
      nsp_tcldstring_append(headPtr, "/", 1);
    }
    break;
  }

  /*
   * Look for the first matching pair of braces or the first
   * directory separator that is not inside a pair of braces.
   */

  openBrace = closeBrace = NULL;
  quoted = 0;
  for (p = tail; *p != '\0'; p++) {
    if (quoted) {
      quoted = 0;
    } else if (*p == '\\') {
      quoted = 1;
      if (strchr(separators, p[1]) != NULL) {
	break;			/* Quoted directory separator. */
      }
    } else if (strchr(separators, *p) != NULL) {
      break;			/* Unquoted directory separator. */
    } else if (*p == '{') {
      openBrace = p;
      p++;
      if (SkipToChar(&p, "}")) {
	closeBrace = p;		/* Balanced braces. */
	break;
      }
      Scierror("unmatched open-brace in file name");
      return TCL_ERROR;
    } else if (*p == '}') {
      Scierror("unmatched close-brace in file name");
      return TCL_ERROR;
    }
  }

  /*
   * Substitute the alternate patterns from the braces and recurse.
   */

  if (openBrace != NULL) {
    char *element;
    nsp_tcldstring newName;
    nsp_tcldstring_init(&newName);

    /*
     * For each element within in the outermost pair of braces,
     * append the element and the remainder to the fixed portion
     * before the first brace and recursively call TclDoGlob.
     */

    nsp_tcldstring_append(&newName, tail, openBrace-tail);
    baseLength = nsp_tcldstring_length(&newName);
    length = nsp_tcldstring_length(headPtr);
    *closeBrace = '\0';
    for (p = openBrace; p != closeBrace; ) {
      p++;
      element = p;
      SkipToChar(&p, ",");
      nsp_tcldstring_set_length(headPtr, length);
      nsp_tcldstring_set_length(&newName, baseLength);
      nsp_tcldstring_append(&newName, element, p-element);
      nsp_tcldstring_append(&newName, closeBrace+1, -1);
      result = TclDoGlob( separators,
			 headPtr, nsp_tcldstring_value(&newName),S);
      if (result != TCL_OK) {
	break;
      }
    }
    *closeBrace = '}';
    nsp_tcldstring_free(&newName);
    return result;
  }

  /*
   * At this point, there are no more brace substitutions to perform on
   * this path component.  The variable p is pointing at a quoted or
   * unquoted directory separator or the end of the string.  So we need
   * to check for special globbing characters in the current pattern.
   * We avoid modifying tail if p is pointing at the end of the string.
   */

  if (*p != '\0') {
    savedChar = *p;
    *p = '\0';
    firstSpecialChar = strpbrk(tail, "*[]?\\");
    *p = savedChar;
  } else {
    firstSpecialChar = strpbrk(tail, "*[]?\\");
  }

  if (firstSpecialChar != NULL) {
    /*
     * Look for matching files in the current directory.  The
     * implementation of this function is platform specific, but may
     * recursively call TclDoGlob.  For each file that matches, it will
     * add the match onto the interp->result, or call TclDoGlob if there
     * are more characters to be processed.
     */

    return TclMatchFiles( separators, headPtr, tail, p,S);
  }
  nsp_tcldstring_append(headPtr, tail, p-tail);
  if (*p != '\0') {
    return TclDoGlob( separators, headPtr, p,S);
  }

  /*
   * There are no more wildcards in the pattern and no more unprocessed
   * characters in the tail, so now we can construct the path and verify
   * the existence of the file.
   */

  switch (tclPlatform) {
  case TCL_PLATFORM_MAC:
    if (strchr(nsp_tcldstring_value(headPtr), ':') == NULL) {
      nsp_tcldstring_append(headPtr, ":", 1);
    }
    name = nsp_tcldstring_value(headPtr);
    if (access(name, F_OK) == 0) {
      if ((name[1] != '\0') && (strchr(name+1, ':') == NULL)) {
      if (nsp_row_smatrix_append_string(S,name+1 )== FAIL) 
	return FAIL;
      } else {
	if (nsp_row_smatrix_append_string(S,name )== FAIL) 
	  return FAIL;
      }
    }
    break;
  case TCL_PLATFORM_WINDOWS: {
    int exists;
    /*
     * We need to convert slashes to backslashes before checking
     * for the existence of the file.  Once we are done, we need
     * to convert the slashes back.
     */

    if (nsp_tcldstring_length(headPtr) == 0) {
      if (((*name == '\\') && (name[1] == '/' || name[1] == '\\'))
	  || (*name == '/')) {
	nsp_tcldstring_append(headPtr, "\\", 1);
      } else {
	nsp_tcldstring_append(headPtr, ".", 1);
      }
    } else {
      for (p = nsp_tcldstring_value(headPtr); *p != '\0'; p++) {
	if (*p == '/') {
	  *p = '\\';
	}
      }
    }
    name = nsp_tcldstring_value(headPtr);
    exists = (access(name, F_OK) == 0);
    for (p = name; *p != '\0'; p++) {
      if (*p == '\\') {
	*p = '/';
      }
    }
    if (exists) {
      if (nsp_row_smatrix_append_string(S,name )== FAIL) 
	return FAIL;
    }
    break;
  }
  case TCL_PLATFORM_UNIX:
    if (nsp_tcldstring_length(headPtr) == 0) {
      if ((*name == '\\' && name[1] == '/') || (*name == '/')) {
	nsp_tcldstring_append(headPtr, "/", 1);
      } else {
	nsp_tcldstring_append(headPtr, ".", 1);
      }
    }
    name = nsp_tcldstring_value(headPtr);
    if (access(name, F_OK) == 0) {
      if (nsp_row_smatrix_append_string(S,name )== FAIL) 
	return FAIL;
    }
    break;
  }
  return TCL_OK;
}



