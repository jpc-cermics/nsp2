/*
 * from tcl: file related functions 
 * 
 * Copyright (c) 1996-1997 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "tclInt.h"
#include "tclPort.h"

/*
 * Declarations for local procedures defined in this file:
 */

static int CopyRenameOneFile( char *source, char *dest, int copyFlag,int force);
static char *FileBasename( char *path, nsp_tcldstring *bufferPtr);
static int   FileCopyRename( int argc, char **argv, int copyFlag,int forceFlag);

/**
 * nsp_absolute_file_name:
 * @fname: a string 
 * 
 * returns the absolute path name corresponding to @fname.
 * 
 * Returns: a new string 
 **/

nsp_string nsp_absolute_file_name(char *fname)
{
  nsp_string S;
  nsp_tcldstring buffer;
  int pargc=2;
  char *pargv[] ={NULL, fname};
  if (( pargv[0] = nsp_get_cwd() ) == NULL) return NULL;
  if ( nsp_get_path_type(fname)== TCL_PATH_RELATIVE )
    {
      nsp_tcldstring_init (&buffer);
      nsp_join_path (pargc, pargv, &buffer);
      S= nsp_new_string(nsp_tcldstring_value(&buffer),-1);
      nsp_tcldstring_set_length (&buffer, 0);
      nsp_tcldstring_free (&buffer);
    }
  else 
    {
      /* fname is already an absolute path_name */
      return nsp_new_string(fname,-1);
    }
  return S;
}


/**
 * nsp_file_rename_cmd:
 * @argc: Number of arguments;
 * @argv: Argument strings for rename Command 
 * @force: force option %TRUE or %FALSE 
 *
 *	This procedure implements the "rename" subcommand of the "file"
 *      command.  Filename arguments need to be translated to native
 *	format before being passed to platform-specific code that
 *	implements rename functionality.
 * 
 * Return value: %OK or %FAIL ?
 **/

int nsp_file_rename_cmd(int  argc,char ** argv,int force)
{
  return FileCopyRename( argc, argv, 0,force);
}

/**
 * nsp_file_copy_cmd:
 * @argc: Number of arguments.
 * @argv: Argument strings passed to Tcl_FileCmd.
 * @force: force option %TRUE or %FALSE 
 *
 *	This procedure implements the "copy" subcommand of the "file"
 *	command.  Filename arguments need to be translated to native
 *	format before being passed to platform-specific code that
 *	implements copy functionality.
 * 
 * Return value:  %OK or %FAIL ?
 **/

int nsp_file_copy_cmd(int argc,	char **argv, int force)
{
  return FileCopyRename( argc, argv, 1,force);
}

/**
 * FileCopyRename:
 * @argc: Number of arguments.
 * @argv: Argument strings passed to Tcl_FileCmd.
 * @copyFlag:  If non-zero, copy source(s).  Otherwise, rename them.
 * @forceFlags: force flag for copy or rename.
 *
 *	Performs the work of nsp_file_rename_cmd() and nsp_file_copy_cmd().
 *	See comments for those procedures.
 *
 * Return value:  %OK or %FAIL ?
 **/

static int FileCopyRename(int argc, char **argv, int copyFlag,int forceFlag)
{
    int i, result;
    struct stat statBuf; 
    nsp_tcldstring targetBuffer;
    char *target;

    /*
     * If target doesn't exist or isn't a directory, try the copy/rename.
     * More than 2 arguments is only valid if the target is an existing
     * directory.
     */

    target = nsp_translate_file_name(argv[argc - 1], &targetBuffer);
    if (target == NULL) {
	return TCL_ERROR;
    }

    result = TCL_OK;

    /*
     * Call stat() so that if target is a symlink that points to a directory
     * we will put the sources in that directory instead of overwriting the
     * symlink.
     */

    if ((stat(target, &statBuf) != 0) || !S_ISDIR(statBuf.st_mode)) {
      if ( argc > 2) {
	errno = ENOTDIR;
	nsp_posix_error();
	Scierror("Error: %s, target \"%s\" is not a directory\n",
		 ((copyFlag) ? "copying" : "renaming"),
		  argv[argc - 1]);
	result = TCL_ERROR;
      } else {
	/* target is not a directory but we only have two arguments 
	 * we test a Rename one file 
	 * Even though already have target == translated(argv[1]),
	 * pass the original argument down, so if there's an error, the
	 * error message will reflect the original arguments.
	 */
	result = CopyRenameOneFile( argv[0], argv[1], copyFlag,
				   forceFlag);
      }
      nsp_tcldstring_free(&targetBuffer);
      return result;
    }
    
    /*
     * Move each source file into target directory.  Extract the basename
     * from each source, and append it to the end of the target path.
     */
    
    for (i=0 ; i < argc - 1; i++) {
	char *jargv[2];
	char *source, *newFileName;
	nsp_tcldstring sourceBuffer, newFileNameBuffer;

	source = FileBasename( argv[i], &sourceBuffer);
	if (source == NULL) {
	    result = TCL_ERROR;
	    break;
	}
	jargv[0] = argv[argc - 1];
	jargv[1] = source;
	nsp_tcldstring_init(&newFileNameBuffer);
	newFileName = nsp_join_path(2, jargv, &newFileNameBuffer);
	result = CopyRenameOneFile( argv[i], newFileName, copyFlag,
		forceFlag);
	nsp_tcldstring_free(&sourceBuffer);
	nsp_tcldstring_free(&newFileNameBuffer);

	if (result == TCL_ERROR) {
	    break;
	}
    }
    nsp_tcldstring_free(&targetBuffer);
    return result;
}


/**
 * nsp_file_make_dirs_cmd:
 * @argc:  Number of arguments.
 * @argv:  Argument strings giving the dirs to create.
 *
 *	This procedure implements the "mkdir" subcommand of the "file"
 *      command.  Filename arguments need to be translated to native
 *	format before being passed to platform-specific code that
 *	implements mkdir functionality.
 * 
 * Return value:  %OK or %FAIL ?
 **/

int nsp_file_make_dirs_cmd(int argc, char **argv)
{
    nsp_tcldstring nameBuffer, targetBuffer;
    char *errfile;
    int result, i, j, pargc;
    char **pargv;
    struct stat statBuf;

    pargv = NULL;
    errfile = NULL;
    nsp_tcldstring_init(&nameBuffer);
    nsp_tcldstring_init(&targetBuffer);

    result = TCL_OK;
    for (i = 0; i < argc; i++) {
	char *name = nsp_translate_file_name(argv[i], &nameBuffer);
	if (name == NULL) {
	    result = TCL_ERROR;
	    break;
	}

	nsp_split_path(name, &pargc, &pargv);
	if (pargc == 0) {
	    errno = ENOENT;
	    errfile = argv[i];
	    break;
	}
	for (j = 0; j < pargc; j++) {
	    char *target = nsp_join_path(j + 1, pargv, &targetBuffer);

	    /*
	     * Call stat() so that if target is a symlink that points to a
	     * directory we will create subdirectories in that directory.
	     */

	    if (stat(target, &statBuf) == 0) {
		if (!S_ISDIR(statBuf.st_mode)) {
		    errno = EEXIST;
		    errfile = target;
		    goto done;
		}
	    } else if ((errno != ENOENT)
		    || (nsp_create_directory(target) != TCL_OK)) {
		errfile = target;
		goto done;
	    }
	    nsp_tcldstring_free(&targetBuffer);
	}
	ckfree((char *) pargv);
	pargv = NULL;
	nsp_tcldstring_free(&nameBuffer);
    }
	
    done:
    if (errfile != NULL) 
      {
	nsp_posix_error();
	Scierror("Error: can't create directory \"%s\"\n",errfile);
	result = TCL_ERROR;
      }

    nsp_tcldstring_free(&nameBuffer);
    nsp_tcldstring_free(&targetBuffer);
    if (pargv != NULL) {
	ckfree((char *) pargv);
    }
    return result;
}


/**
 * nsp_file_delete_cmd:
 * @argc: Number of arguments 
 * @argv: Argument strings passed to Tcl_FileCmd
 * @forceFlag: force Argument
 *
 *	This procedure implements the "delete" subcommand of the "file"
 *      command.
 * 
 * Return value:  %OK or %FAIL ?
 **/

int nsp_file_delete_cmd(int argc,char **argv,int forceFlag)
{
    nsp_tcldstring nameBuffer, errorBuffer;
    int i, force=0, result;
    char *errfile;

    errfile = NULL;
    result = TCL_OK;
    nsp_tcldstring_init(&errorBuffer);
    nsp_tcldstring_init(&nameBuffer);

    for (i=0 ; i < argc; i++) {
	struct stat statBuf;
	char *name;

	errfile = argv[i];
	nsp_tcldstring_set_length(&nameBuffer, 0);
	name = nsp_translate_file_name( argv[i], &nameBuffer);
	if (name == NULL) {
	    result = TCL_ERROR;
	    goto done;
	}

	/*
	 * Call lstat() to get info so can delete symbolic link itself.
	 */

	if (lstat(name, &statBuf) != 0) {
	    /*
	     * Trying to delete a file that does not exist is not
	     * considered an error, just a no-op
	     */

	    if (errno != ENOENT) {
		result = TCL_ERROR;
	    }
	} else if (S_ISDIR(statBuf.st_mode)) {
	    result = nsp_remove_directory(name, force, &errorBuffer);
	    if (result != TCL_OK) {
		if ((force == 0) && (errno == EEXIST)) 
		  {
		    nsp_posix_error();
		    Scierror("Error: \"%s\" directory is not empty\n", argv[i]);
		    goto done;
		}

		/* 
		 * If possible, use the untranslated name for the file.
		 */
		 
		errfile = nsp_tcldstring_value(&errorBuffer);
		if (strcmp(name, errfile) == 0) {
		    errfile = argv[i];
		}
	    }
	} else {
	    result = nsp_delete_file(name);
	}
	
	if (result == TCL_ERROR) {
	    break;
	}
    }
    if (result != TCL_OK) 
      {
	nsp_posix_error();
	Scierror("Error while deleting \"%s\"\n", errfile);
    } 
    done:
    nsp_tcldstring_free(&errorBuffer);
    nsp_tcldstring_free(&nameBuffer);
    return result;
}

/**
 * CopyRenameOneFile:
 * @source: Pathname of file to copy.  May need to be translated.
 * @target: Pathname of file to create/overwrite. May need to be translated.
 * @copyFlag: f non-zero, copy files.  Otherwise rename them.
 * @force: If non-zero, overwrite target file if it exists.
 *         Otherwise, error if target already exists.
 *
 *	Copies or renames specified source file or directory hierarchy
 *	to the specified target.  
 *	Target is overwritten if the force flag is set.  Attempting to
 *	copy/rename a file onto a directory or a directory onto a file
 *	will always result in an error.  
 * 
 * Return value: %OK or %FAIL ? 
 **/

static int CopyRenameOneFile( char *source, char *target, int copyFlag, int force)
{
    int result;
    nsp_tcldstring sourcePath, targetPath, errorBuffer;
    char *targetName, *sourceName, *errfile;
    struct stat sourceStatBuf, targetStatBuf;
	
    sourceName = nsp_translate_file_name( source, &sourcePath);
    if (sourceName == NULL) {
	return TCL_ERROR;
    }
    targetName = nsp_translate_file_name( target, &targetPath);
    if (targetName == NULL) {
	nsp_tcldstring_free(&sourcePath);
	return TCL_ERROR;
    }
    
    errfile = NULL;
    result = TCL_ERROR;
    nsp_tcldstring_init(&errorBuffer);
    
    /*
     * We want to copy/rename links and not the files they point to, so we
     * use lstat(). If target is a link, we also want to replace the 
     * link and not the file it points to, so we also use lstat() on the
     * target.
     */

    if (lstat(sourceName, &sourceStatBuf) != 0) {
	errfile = source;
	goto done;
    }
    if (lstat(targetName, &targetStatBuf) != 0) {
	if (errno != ENOENT) {
	    errfile = target;
	    goto done;
	}
    } else {
	if (force == 0) {
	    errno = EEXIST;
	    errfile = target;
	    goto done;
	}

        /* 
         * Prevent copying or renaming a file onto itself.  Under Windows, 
         * stat always returns 0 for st_ino.  However, the Windows-specific 
         * code knows how to deal with copying or renaming a file on top of
         * itself.  It might be a good idea to write a stat that worked.
         */
     
        if ((sourceStatBuf.st_ino != 0) && (targetStatBuf.st_ino != 0)) {
            if ((sourceStatBuf.st_ino == targetStatBuf.st_ino) &&
            	    (sourceStatBuf.st_dev == targetStatBuf.st_dev)) {
            	result = TCL_OK;
            	goto done;
            }
        }

	/*
	 * Prevent copying/renaming a file onto a directory and
	 * vice-versa.  This is a policy decision based on the fact that
	 * existing implementations of copy and rename on all platforms
	 * also prevent this.
	 */

	if (S_ISDIR(sourceStatBuf.st_mode)
                && !S_ISDIR(targetStatBuf.st_mode)) {
	    errno = EISDIR;
	    Scierror("Error: can't overwrite file \"%s\" with directory \"%s\"\n", target,source);
	    goto done;
	}
	if (!S_ISDIR(sourceStatBuf.st_mode)
	        && S_ISDIR(targetStatBuf.st_mode)) {
	    errno = EISDIR;
	    Scierror("Error: can't overwrite directory \"%s\" with file \"%s\"\n", target,source);
	    goto done;
	}
    }

    if (copyFlag == 0) {
	result = nsp_rename_file(sourceName, targetName);
	if (result == TCL_OK) {
	    goto done;
	}
	    
	if (errno == EINVAL) 
	  {
	    Scierror("Error: renaming \"%s\" to \"%s\" trying to rename a volume or move a directory into itself\n",
		     source,target);
	    goto done;
	  } 
	else if (errno != EXDEV) {
	  errfile = target;
	  goto done;
	}
	
	/*
	 * The rename failed because the move was across file systems.
	 * Fall through to copy file and then remove original.  Note that
	 * the low-level nsp_rename_file is allowed to implement
	 * cross-filesystem moves itself.
	 */
    }

    if (S_ISDIR(sourceStatBuf.st_mode)) {
	result = nsp_copy_directory(sourceName, targetName, &errorBuffer);
	if (result != TCL_OK) {
	    errfile = nsp_tcldstring_value(&errorBuffer);
	    if (strcmp(errfile, sourceName) == 0) {
		errfile = source;
	    } else if (strcmp(errfile, targetName) == 0) {
		errfile = target;
	    }
	}
    } else {
	result = nsp_copy_file(sourceName, targetName);
	if (result != TCL_OK) {
	    /*
	     * Well, there really shouldn't be a problem with source,
	     * because up there we checked to see if it was ok to copy it.
	     */

	    errfile = target;
	}
    }
    if ((copyFlag == 0) && (result == TCL_OK)) {
	if (S_ISDIR(sourceStatBuf.st_mode)) {
	    result = nsp_remove_directory(sourceName, 1, &errorBuffer);
	    if (result != TCL_OK) {
		errfile = nsp_tcldstring_value(&errorBuffer);
		if (strcmp(errfile, sourceName) == 0) {
		    errfile = source;
		}
	    }
	} else {
	    result = nsp_delete_file(sourceName);
	    if (result != TCL_OK) {
		errfile = source;
	    }
	}
	if (result != TCL_OK) 
	  {
	    nsp_posix_error();
	    Scierror("Error: can't unlink \"%s\"\n",errfile);
	    errfile = NULL;
	}
    }
    
    done:
    if (errfile != NULL) 
      {
	nsp_posix_error();
	Scierror("Error: %s \"%s\"",((copyFlag) ? "copying" : "renaming"),source);
	if (errfile != source) 
	  {
	    Scierror(" to \"%s\"", target);
	    if (errfile != target) 
	      {
		Scierror(": \"%s\"\n", errfile);
	      }
	  }
      }
    nsp_tcldstring_free(&errorBuffer);
    nsp_tcldstring_free(&sourcePath);
    nsp_tcldstring_free(&targetPath);
    return result;
}

/**
 * FileBasename:
 * @path: Path whose basename to extract.
 * @bufferPtr: Initialized DString that receives basename.
 *	Given a path in either tcl format (with / separators), or in the
 *	platform-specific format for the current platform, return all the
 *	characters in the path after the last directory separator.  But,
 *	if path is the root directory, returns no characters.
 *	Appends the string that represents the basename to the end of
 *	the specified initialized DString, returning a pointer to the
 *	resulting string.  If there is an error, an error message is left
 *	in interp, NULL is returned, and the nsp_tcldstring is unmodified.
 * 
 * 
 * Return value: a pointer to the file base name in @bufferPtr.
 **/

static char *FileBasename(char *path, nsp_tcldstring *bufferPtr)
{
  int argc;
  char **argv;
    
  nsp_split_path(path, &argc, &argv);
  if (argc == 0) 
    {
      nsp_tcldstring_init(bufferPtr);
    } 
  else 
    {
      if ((argc == 1) && (*path == '~')) 
	{
	  nsp_tcldstring buffer;
	  
	  ckfree((char *) argv);
	  path = nsp_translate_file_name( path, &buffer);
	  if (path == NULL) {
	    return NULL;
	  }
	  nsp_split_path(path, &argc, &argv);
	  nsp_tcldstring_free(&buffer);
	}
      nsp_tcldstring_init(bufferPtr);

      /*
       * Return the last component, unless it is the only component, and it
       * is the root of an absolute path.
       */
      
      if (argc > 0) {
	if ((argc > 1)
	    || (nsp_get_path_type(argv[0]) == TCL_PATH_RELATIVE)) {
	  nsp_tcldstring_append(bufferPtr, argv[argc - 1], -1);
	}
      }
    }
  ckfree((char *) argv);
  return nsp_tcldstring_value(bufferPtr);
}


/*
 * This variable indicates whether the cleanup procedure has been
 * registered for this file yet.
 */

static int initialized = 0;

/*
 * The following regular expression matches the root portion of a Windows
 * absolute or volume relative path.  It will match both UNC and drive relative
 * paths.
 */

#define WIN_ROOT_PATTERN "^(([a-zA-Z]:)|[/\\][/\\]+([^/\\]+)[/\\]+([^/\\]+)|([/\\]))([/\\])*"

/*
 * The following regular expression matches the root portion of a Macintosh
 * absolute path.  It will match degenerate Unix-style paths, tilde paths,
 * Unix-style paths, and Mac paths.
 */

#define MAC_ROOT_PATTERN "^((/+([.][.]?/+)*([.][.]?)?)|(~[^:/]*)(/[^:]*)?|(~[^:]*)(:.*)?|/+([.][.]?/+)*([^:/]+)(/[^:]*)?|([^:]+):.*)$"

/*
 * The following variables are used to hold precompiled regular expressions
 * for use in filename matching.
 */

static regexp *winRootPatternPtr = NULL;
static regexp *macRootPatternPtr = NULL;

/*
 * The following variable is set in the nsp_tclplatform_init call to one
 * of: TCL_PLATFORM_UNIX, TCL_PLATFORM_MAC, or TCL_PLATFORM_WINDOWS.
 * NOTE that  nsp_tclplatform_init is not used up to now 
 */

#ifdef WIN32
TclPlatformType tclPlatform = TCL_PLATFORM_WINDOWS;
#else 
TclPlatformType tclPlatform = TCL_PLATFORM_UNIX;
#endif 

/*
 * Prototypes for local procedures defined in this file:
 */

static char *		DoTildeSubst ( char *user, nsp_tcldstring *resultPtr);
static char *		ExtractWinRoot (char *path,  nsp_tcldstring *resultPtr, int offset);
static void		FileNameCleanup (ClientData clientData);
static int		SkipToChar (char **stringPtr,char *match);
static char *		SplitMacPath (char *path, nsp_tcldstring *bufPtr);
static char *		SplitWinPath (char *path, nsp_tcldstring *bufPtr);
static char *		SplitUnixPath (const char *path, nsp_tcldstring *bufPtr);


/**
 * nsp_update_exec_dir:
 * @filename: a file name given on entry 
 * @exec_dir: the current value of exec directory in a string buffer of size @length.
 * @filename_exec: a string buffer of size @length.
 * @length:  length of @exec_dir and @filename_exec
 * 
 * 
 * This function is used to update the @exec_dir value during the 
 * execution of script file @filename. Thus, if @filename is a relative filename
 * @exec_dir is updated to @execdir/dirname(filename) and @filename_exec is 
 * updated to @execdir/dirname(filename)/tail(filename). 
 * 
 **/

void nsp_update_exec_dir(char *filename,char *exec_dir,char *filename_exec,unsigned int length)
{
  int path_type = nsp_get_path_type(filename);
  nsp_string dirname = nsp_dirname (filename);
  if ( dirname == NULL) return ;
  if ( exec_dir == NULL ) return ;
  if ( exec_dir[0] == '\0' || path_type == TCL_PATH_ABSOLUTE 
       ||  path_type ==      TCL_PATH_VOLUME_RELATIVE)
    {
      /* if exec_dir is not set or the path in filename is absolute */
      if ( strcmp(dirname,".") != 0) 
	{
	  strncpy(exec_dir,dirname,length);
	}
      strncpy(filename_exec,filename,length);
      nsp_string_destroy(&dirname);
      return;
    }
  else
    {
      /* set exec_dir = exec_dir/dirname */
      nsp_string tail ;
      nsp_tcldstring buffer;
      int pargc=2;
      char *pargv[] ={ exec_dir, dirname};
      nsp_tcldstring_init (&buffer);
      if ( strcmp(dirname,".") != 0)
	{
	  nsp_join_path (pargc, pargv, &buffer);
	  strncpy(exec_dir,nsp_tcldstring_value (&buffer),Min(buffer.length,length));
	  nsp_tcldstring_set_length (&buffer, 0);
	}
      /* also update filename_exec to  exec_dir/dirname/tail */
      tail = nsp_tail(filename);
      pargv[1]= tail;
      nsp_join_path (pargc, pargv, &buffer);
      strncpy(filename_exec,nsp_tcldstring_value (&buffer),Min(buffer.length+1,length));
      nsp_tcldstring_free (&buffer);
      nsp_string_destroy(&dirname);
      nsp_string_destroy(&tail);
    }
}

/**
 * nsp_update_exec_dir_from_dir:
 * @dirname: a directory name given on entry 
 * @exec_dir: the current value of exec directory in a string buffer of size @length.
 * @length:  length of @exec_dir and @filename_exec
 * 
 * This function is used to update the @exec_dir value using the value of @dirname.
 * If @dirname is a relative filename @exec_dir is updated to @execdir/dirname 
 * 
 **/

void nsp_update_exec_dir_from_dir(char *dirname,char *exec_dir,unsigned int length)
{
  int path_type = nsp_get_path_type(dirname);
  if ( dirname == NULL) return ;
  if ( exec_dir == NULL ) return ;
  if ( exec_dir[0] == '\0' || path_type == TCL_PATH_ABSOLUTE 
       ||  path_type ==      TCL_PATH_VOLUME_RELATIVE)
    {
      /* if exec_dir is not set or the path in filename is absolute */
      if ( strcmp(dirname,".") != 0) 
	{
	  strncpy(exec_dir,dirname,length);
	}
      return;
    }
  else
    {
      /* set exec_dir = exec_dir/dirname */
      nsp_tcldstring buffer;
      int pargc=2;
      char *pargv[] ={ exec_dir, dirname};
      nsp_tcldstring_init (&buffer);
      if ( strcmp(dirname,".") != 0)
	{
	  nsp_join_path (pargc, pargv, &buffer);
	  strncpy(exec_dir,nsp_tcldstring_value (&buffer),Min(buffer.length,length));
	  nsp_tcldstring_set_length (&buffer, 0);
	}
      nsp_tcldstring_free (&buffer);
    }
}

/**
 * nsp_dirname:
 * @fileName: 
 * 
 * 
 * 
 * Return value: 
 **/

nsp_string nsp_dirname (char *fileName)
{
  nsp_string result = NULL;
  nsp_tcldstring buffer;
  int pargc;
  char **pargv = NULL;
  nsp_tcldstring_init (&buffer);
  nsp_split_path (fileName, &pargc, &pargv);
  if ((pargc == 1) && (*fileName == '~'))
    {
      /*
       * If there is only one element, and it starts with a tilde,
       * perform tilde substitution and resplit the path.
       */
      ckfree ((char *) pargv);pargv=NULL;
      fileName = nsp_translate_file_name (fileName, &buffer);
      if (fileName == NULL)
	{
	  goto done;
	}
      nsp_split_path (fileName, &pargc, &pargv);
      nsp_tcldstring_set_length (&buffer, 0);
    }
  /*
   * Return all but the last component.  If there is only one
   * component, return it if the path was non-relative, otherwise
   * return the current directory.
   */
  if (pargc > 1)
    {
      nsp_join_path (pargc - 1, pargv, &buffer);
      result = nsp_new_string(nsp_tcldstring_value (&buffer), buffer.length);
    }
  else if ((pargc == 0) || (nsp_get_path_type (pargv[0]) == TCL_PATH_RELATIVE))
    {
      result = nsp_new_string((tclPlatform == TCL_PLATFORM_MAC) ? ":" : ".", 1);
    }
  else
    {
      result = nsp_new_string(pargv[0], -1);
    }
done:
  if (pargv != NULL) ckfree ((char *) pargv);
  nsp_tcldstring_free (&buffer);
  return result;
}


/**
 * nsp_tail:
 * @fileName: 
 * 
 * 
 * 
 * Return value: 
 **/

nsp_string nsp_tail(char *fileName)
{
  nsp_tcldstring buffer;
  nsp_string result = NULL;
  int pargc;
  char **pargv;
  /*
   * If there is only one element, and it starts with a tilde,
   * perform tilde substitution and resplit the path.
   */

  nsp_split_path(fileName, &pargc, &pargv);
  if ((pargc == 1) && (*fileName == '~')) 
    {
      ckfree((char*) pargv);pargv=NULL;
      fileName = nsp_translate_file_name( fileName, &buffer);
      if (fileName == NULL) 
	{
	  goto done;
	}
    nsp_split_path(fileName, &pargc, &pargv);
    nsp_tcldstring_set_length(&buffer, 0);
    }
  /*
   * Return the last component, unless it is the only component,
   * and it is the root of an absolute path.
   */
  if (pargc > 0) 
    {
      if ((pargc > 1)
	  || (nsp_get_path_type(pargv[0]) == TCL_PATH_RELATIVE)) 
	{
	  result = nsp_new_string(pargv[pargc - 1], -1);
	}
    }
 done :
  if (pargv != NULL) ckfree ((char *) pargv);
  return result;
}

/**
 * FileNameCleanup:
 * @clientData: unused
 * 
 *	This procedure is a Tcl_ExitProc used to clean up the static
 *	data structures used in this file.
 *	Deallocates storage used by the procedures in this file.
 * 
 **/

static void FileNameCleanup(ClientData clientData)
{
  if (winRootPatternPtr != NULL) {
    ckfree((char *)winRootPatternPtr);
    winRootPatternPtr = (regexp *) NULL;
  }
  if (macRootPatternPtr != NULL) {
    ckfree((char *)macRootPatternPtr);
    macRootPatternPtr = (regexp *) NULL;
  }
  initialized = 0;
}


/**
 * ExtractWinRoot:
 * @path: Path to parse.
 * @resultPtr: Buffer to hold result.
 * @offset: Offset in buffer where result should be stored.
 * 
 *	Matches the root portion of a Windows path and appends it
 *	to the specified nsp_tcldstring.
 *	Modifies the specified nsp_tcldstring.
 * 
 * Return value: the position in the path immediately after the root
 *   including any trailing slashes. Appends a cleaned up version of 
 *   the root to the nsp_tcldstring at the specified offest.
 *
 **/

static char *
ExtractWinRoot( char *path,nsp_tcldstring * resultPtr,int  offset)

{
  int length;

  /*
   * Initialize the path name parser for Windows path names.
   */

  if (winRootPatternPtr == NULL) {
    winRootPatternPtr = tcl_reg_comp(WIN_ROOT_PATTERN);
    if (!initialized) {
      nsp_create_exit_handler(FileNameCleanup, NULL);
      initialized = 1;
    }
  }

  /*
   * Match the root portion of a Windows path name.
   */

  if (!tcl_reg_exec(winRootPatternPtr, path, path)) {
    return path;
  }

  nsp_tcldstring_set_length(resultPtr, offset);

  if (winRootPatternPtr->startp[2] != NULL) {
    nsp_tcldstring_append(resultPtr, winRootPatternPtr->startp[2], 2);
    if (winRootPatternPtr->startp[6] != NULL) {
      nsp_tcldstring_append(resultPtr, "/", 1);
    }
  } else if (winRootPatternPtr->startp[4] != NULL) {
    nsp_tcldstring_append(resultPtr, "//", 2);
    length = winRootPatternPtr->endp[3]
      - winRootPatternPtr->startp[3];
    nsp_tcldstring_append(resultPtr, winRootPatternPtr->startp[3], length);
    nsp_tcldstring_append(resultPtr, "/", 1);
    length = winRootPatternPtr->endp[4]
      - winRootPatternPtr->startp[4];
    nsp_tcldstring_append(resultPtr, winRootPatternPtr->startp[4], length);
  } else {
    nsp_tcldstring_append(resultPtr, "/", 1);
  }
  return winRootPatternPtr->endp[0];
}



/**
 * nsp_get_path_type:
 * @path: 
 * 
 *	Determines whether a given path is relative to the current
 *	directory, relative to the current volume, or absolute.
 * 
 * Return value: Returns one of TCL_PATH_ABSOLUTE, TCL_PATH_RELATIVE, or
 * TCL_PATH_VOLUME_RELATIVE.
 **/

Tcl_PathType nsp_get_path_type( char *path)
{
  Tcl_PathType type = TCL_PATH_ABSOLUTE;

  switch (tclPlatform) {
  case TCL_PLATFORM_UNIX:
    /*
     * Paths that begin with / or ~ are absolute.
     */

    if ((path[0] != '/') && (path[0] != '~')) {
      type = TCL_PATH_RELATIVE;
    }
    break;

  case TCL_PLATFORM_MAC:
    if (path[0] == ':') {
      type = TCL_PATH_RELATIVE;
    } else if (path[0] != '~') {

      /*
       * Since we have eliminated the easy cases, use the
       * root pattern to look for the other types.
       */

      if (!macRootPatternPtr) {
	macRootPatternPtr = tcl_reg_comp(MAC_ROOT_PATTERN);
	if (!initialized) {
	  nsp_create_exit_handler(FileNameCleanup, NULL);
	  initialized = 1;
	}
      }
      if (!tcl_reg_exec(macRootPatternPtr, path, path)
	  || (macRootPatternPtr->startp[2] != NULL)) {
	type = TCL_PATH_RELATIVE;
      }
    }
    break;
	
  case TCL_PLATFORM_WINDOWS:

    if (path[0] != '~') {
      /* Sciprintf("testing %s\n",path); */
      /*
       * Since we have eliminated the easy cases, check for
       * drive relative paths using the regular expression.
       */

      if (!winRootPatternPtr) {
	winRootPatternPtr = tcl_reg_comp(WIN_ROOT_PATTERN);
	if (!initialized) {
	  nsp_create_exit_handler(FileNameCleanup, NULL);
	  initialized = 1;
	}
      }
      if (tcl_reg_exec(winRootPatternPtr, path, path)) {
	if (winRootPatternPtr->startp[5]
	    || (winRootPatternPtr->startp[2]
		&& !(winRootPatternPtr->startp[6]))) {
	  type = TCL_PATH_VOLUME_RELATIVE;
	}
      } else {
	type = TCL_PATH_RELATIVE;
      }
    }
    break;
  }
  return type;
}



/**
 * nsp_split_path:
 * @path: Pointer to string containing a path.
 * @argcPtr: Pointer to location to fill in with the number of elements in the path.
 * @argvPtr: Pointer to place to store pointer to array of pointers to path elements.
 * 
 *	Split a path into a list of path components.  The first element
 *	of the list will have the same path type as the original path.
 *
 *	*argvPtr will be filled in with the address of an array
 *	whose elements point to the elements of path, in order.
 *	*argcPtr will get filled in with the number of valid elements
 *	in the array.  A single block of memory is dynamically allocated
 *	to hold both the argv array and a copy of the path elements.
 *	The caller must eventually free this memory by calling ckfree()
 *	on *argvPtr.  Note:  *argvPtr and *argcPtr are only modified
 *	if the procedure returns normally.
 *
 **/

void nsp_split_path(char *path,int *argcPtr, char ***argvPtr)
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
  memcpy((void *) p, (void *) nsp_tcldstring_value(&buffer), (size_t) size);

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

/**
 * SplitUnixPath:
 * @path: Pointer to string containing a path.
 * @bufPtr: Pointer to DString to use for the result.
 * 
 * used by nsp_split_path to handle splitting
 * unix paths. Stores a null separated array of strings in the specified
 * nsp_tcldstring.
 * 
 * Return value: Stores a null separated array of strings in the specified 
 *  nsp_tcldstring which value is returned.
 **/

static char *SplitUnixPath(const char *path, nsp_tcldstring *bufPtr)
{
  int length;
  const char *p;
  const char *elementStart;

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


/**
 * SplitWinPath:
 * @path: Pointer to string containing a path.
 * @bufPtr:  Pointer to DString to use for the result.
 * 
 *	This routine is used by nsp_split_path to handle splitting
 *	Windows paths.
 *	Stores a null separated array of strings in the specified
 *	nsp_tcldstring.
 *
 * Return value: the value of @bufPtr
 **/

static char *SplitWinPath(char *path, nsp_tcldstring *bufPtr)
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


/**
 * SplitMacPath:
 * @path: Pointer to string containing a path.
 * @bufPtr: Pointer to DString to use for the result.
 *
 *	This routine is used by nsp_split_path to handle splitting
 *	Macintosh paths.
 *	Returns a newly allocated argv array.
 * 
 * Return value: the value of @bufPtr
 **/

static char *SplitMacPath( char *path, nsp_tcldstring *bufPtr)
{
  int isMac = 0;		/* 1 if is Mac-style, 0 if Unix-style path. */
  int i, length;
  const char *p, *elementStart;

  /*
   * Initialize the path name parser for Macintosh path names.
   */

  if (macRootPatternPtr == NULL) {
    macRootPatternPtr = tcl_reg_comp(MAC_ROOT_PATTERN);
    if (!initialized) {
      nsp_create_exit_handler(FileNameCleanup, NULL);
      initialized = 1;
    }
  }

  /*
   * Match the root portion of a Mac path name.
   */

  i = 0;			/* Needed only to prevent gcc warnings. */
  if (tcl_reg_exec(macRootPatternPtr, path, path) == 1) {
    /*
     * Treat degenerate absolute paths like / and /../.. as
     * Mac relative file names for lack of anything else to do.
     */

    if (macRootPatternPtr->startp[2] != NULL) {
      nsp_tcldstring_append(bufPtr, ":", 1);
      nsp_tcldstring_append(bufPtr, path, macRootPatternPtr->endp[0]
			- macRootPatternPtr->startp[0] + 1);
      return nsp_tcldstring_value(bufPtr);
    }

    if (macRootPatternPtr->startp[5] != NULL) {

      /*
       * Unix-style tilde prefixed paths.
       */

      isMac = 0;
      i = 5;
    } else if (macRootPatternPtr->startp[7] != NULL) {

      /*
       * Mac-style tilde prefixed paths.
       */

      isMac = 1;
      i = 7;
    } else if (macRootPatternPtr->startp[10] != NULL) {

      /*
       * Normal Unix style paths.
       */

      isMac = 0;
      i = 10;
    } else if (macRootPatternPtr->startp[12] != NULL) {

      /*
       * Normal Mac style paths.
       */

      isMac = 1;
      i = 12;
    }

    length = macRootPatternPtr->endp[i]
      - macRootPatternPtr->startp[i];

    /*
     * Append the element and terminate it with a : and a null.  Note that
     * we are forcing the DString to contain an extra null at the end.
     */

    nsp_tcldstring_append(bufPtr, macRootPatternPtr->startp[i], length);
    nsp_tcldstring_append(bufPtr, ":", 2);
    p = macRootPatternPtr->endp[i];
  } else {
    isMac = (strchr(path, ':') != NULL);
    p = path;
  }
    
  if (isMac) {

    /*
     * p is pointing at the first colon in the path.  There
     * will always be one, since this is a Mac-style path.
     */

    elementStart = p++;
    while ((p = strchr(p, ':')) != NULL) {
      length = p - elementStart;
      if (length == 1) {
	while (*p == ':') {
	  nsp_tcldstring_append(bufPtr, "::", 3);
	  elementStart = p++;
	}
      } else {
	/*
	 * If this is a simple component, drop the leading colon.
	 */

	if ((elementStart[1] != '~')
	    && (strchr(elementStart+1, '/') == NULL)) {
	  elementStart++;
	  length--;
	}
	nsp_tcldstring_append(bufPtr, elementStart, length);
	nsp_tcldstring_append(bufPtr, "", 1);
	elementStart = p++;
      }
    }
    if (elementStart[1] != '\0' || elementStart == path) {
      if ((elementStart[1] != '~') && (elementStart[1] != '\0')
	  && (strchr(elementStart+1, '/') == NULL)) {
	elementStart++;
      }
      nsp_tcldstring_append(bufPtr, elementStart, -1);
      nsp_tcldstring_append(bufPtr, "", 1);
    }
  } else {

    /*
     * Split on slashes, suppress extra /'s, and convert .. to ::. 
     */

    for (;;) {
      elementStart = p;
      while ((*p != '\0') && (*p != '/')) {
	p++;
      }
      length = p - elementStart;
      if (length > 0) {
	if ((length == 1) && (elementStart[0] == '.')) {
	  nsp_tcldstring_append(bufPtr, ":", 2);
	} else if ((length == 2) && (elementStart[0] == '.')
		   && (elementStart[1] == '.')) {
	  nsp_tcldstring_append(bufPtr, "::", 3);
	} else {
	  if (*elementStart == '~') {
	    nsp_tcldstring_append(bufPtr, ":", 1);
	  }
	  nsp_tcldstring_append(bufPtr, elementStart, length);
	  nsp_tcldstring_append(bufPtr, "", 1);
	}
      }
      if (*p++ == '\0') {
	break;
      }
    }
  }
  return nsp_tcldstring_value(bufPtr);
}


/**
 * nsp_join_path:
 * @argc: 
 * @argv: 
 * @resultPtr:  Pointer to previously initialized DString.
 *
 * Combines a list of paths in a platform specific manner.
 *
 * Return value: Appends the joined path to the end of the specified
 *	returning a pointer to the resulting string.  Note that
 *	the nsp_tcldstring must already be initialized.
 **/

char *nsp_join_path( int argc, char **argv,  nsp_tcldstring *resultPtr)
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


/**
 * nsp_translate_file_name:
 * @name: File name, which may begin with tilde or tilde user
 * @bufferPtr: May be used to hold result. Must not hold anything 
 *   at the time of the call, and need not even be initialized.
 * 
 *	Converts a file name into a form usable by the native system
 *	interfaces.  If the name starts with a tilde, it will produce
 *	a name where the tilde and following characters have been
 *	replaced by the home directory location for the named user.
 * 
 * 
 * Return value:  The result is a pointer to a static string containing
 *	the new name.  If there was an error in processing the
 *	name, then an error message is left
 *	and the return value is %NULL.  The result will be stored
 *	in bufferPtr; the caller must call nsp_tcldstring_free(bufferPtr)
 *	to free the name if the return value was not %NULL.
 **/

char *nsp_translate_file_name(char *name,nsp_tcldstring *bufferPtr)
{
  register char *p;

  /*
   * Handle tilde substitutions, if needed.
   */

  if (name[0] == '~') {
    int argc, length;
    char **argv;
    nsp_tcldstring temp;

    nsp_split_path(name, &argc, &argv);
	
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
    nsp_join_path(argc, argv, bufferPtr);
    nsp_tcldstring_free(&temp);
    ckfree((char*)argv);
  } else {
    nsp_tcldstring_init(bufferPtr);
    nsp_join_path(1, &name, bufferPtr);
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

/**
 * nsp_get_extension:
 * @name: File name to parse. 
 * 
 *	This function returns a pointer to the beginning of the
 *	extension part of a file name.
 * 
 * Return value: 	Returns a pointer into name which indicates where the extension
 * starts.  If there is no extension, returns %NULL.
 **/

char *nsp_get_extension(char *name)
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


/**
 * DoTildeSubst:
 * @user: Name of user whose home directory should be substituted.
 * @resultPtr: May be used to hold result. Must not hold anything 
 *      at the time of the call, and need not even be initialized.
 *
 *	Given a string following a tilde, this routine returns the
 *	corresponding home directory.
 *
 * Return value: The result is a pointer to a static string containing the home
 *	directory in native format.  If there was an error in processing
 *	the substitution, then an error message is left
 *	and the return value is %NULL. On success, the results are appended
 * 	to resultPtr, and the contents of resultPtr are returned.
 **/

static char *DoTildeSubst(char *user, nsp_tcldstring *resultPtr)
{
  char *dir;

  if (*user == '\0') {
    dir = nsp_getenv("HOME");
    if (dir == NULL) {
      Scierror("Error: couldn't find HOME environment variable to expand path\n");
      return NULL;
    }
    nsp_join_path(1, &dir, resultPtr);
  } else {
	
    /* lint, TclGetuserHome() always NULL under windows. */
    if (nsp_get_user_home(user, resultPtr) == NULL) {	
      Scierror("Error: user \"%s\" doesn't exist\n",user);
      return NULL;
    }
  }
  return resultPtr->string;
}

/* list files from a pattern  printing result
 */

int nsp_glob(const char *pattern)
{
  NspSMatrix *S;
  int noComplain=0;
  char c;
  int result = TCL_OK;
  nsp_tcldstring buffer;
  char *separators, *head, *tail,*str;
  char pattern1[FSIZE+1];
  nsp_path_expand(pattern,pattern1,FSIZE);
  str = nsp_new_string(pattern1,-1);
  nsp_tcldstring_init(&buffer);
  separators = NULL;		/* Needed only to prevent gcc warnings. */
  
  if ((S=nsp_smatrix_create(NVOID,0,0,".",0))== NULLSMAT) 
    return FAIL;
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
	if (!noComplain) 
	  {
	    Scierror("Error: globbing characters not supported in user names\n");
	  }
	head = NULL;
      }
      *tail = c;
      if (head == NULL) {
	if (noComplain) {
	  goto done;
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
  result = nsp_do_glob( separators, &buffer, tail,S);
  if (result != TCL_OK) {
    goto done;
  }
 done:
  nsp_tcldstring_free(&buffer);
  if ( result == TCL_OK ) 
    {
      nsp_smatrix_print_multicols(S,0,"ls",0);
      nsp_smatrix_destroy(S);
      nsp_string_destroy(&str);
      return OK;
    }
  else
    {
      nsp_string_destroy(&str);
      return FAIL;
    }
}


/*
 * int_glob: interface to the glob command 
 *
 */

int int_glob (Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *S;
  int i, noComplain=0;
  char c;
  int result = TCL_OK;
  nsp_tcldstring buffer;
  char *separators, *head, *tail,*str1;
  
  nsp_tcldstring_init(&buffer);
  separators = NULL;		/* Needed only to prevent gcc warnings. */
  
  if ((S=nsp_smatrix_create(NVOID,0,0,".",0))== NULLSMAT) 
    return RET_BUG;
  
  for (i = 1 ; i <= rhs ; i++) 
    {
      char str[FSIZE+1];
      if ((str1 = GetString(stack,i)) == (char*)0) return RET_BUG;
      /* expand file */
      nsp_path_expand(str1,str,FSIZE);
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
	    if (!noComplain) 
	      {
		Scierror("Error: globbing characters not supported in user names\n");
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
      result = nsp_do_glob( separators, &buffer, tail,S);
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
      MoveObj(stack,1, NSP_OBJECT(S));
      return 1;
    }
  else
    {
      return RET_BUG;
    }
}



/**
 * SkipToChar:
 * @stringPtr:  Pointer string to check.
 * @match: Pointer to character to find.
 * 
 *	This function traverses a glob pattern looking for the next
 *	unquoted occurance of the specified character at the same braces
 *	nesting level.
 *
 * Return value: Updates @stringPtr to point to the matching character, or to
 *	the end of the string if nothing matched.  The return value
 *	is 1 if a match was found at the top level, otherwise it is 0.
 **/

static int SkipToChar( char **stringPtr,char *match)
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


/**
 * nsp_do_glob:
 * @separators: String containing separator characters that should be used to identify globbing 
 *  boundaries.
 * @headPtr: Completely expanded prefix.
 * @tail: The unexpanded remainder of the path.
 * @S:  String Matrix for appending results. the Matrix can be an empty Matrix.
 *
 * This recursive procedure forms the heart of the globbing
 * code.  It performs a depth-first traversal of the tree
 * given by the path name to be globbed.  The directory and
 * remainder are assumed to be native format paths.
 *
 * Return value: The return value is a standard Tcl result indicating whether
 *	an error occurred in globbing.
 **/

int nsp_do_glob(char *separators, nsp_tcldstring *headPtr, char *tail, NspSMatrix *S)
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
     * before the first brace and recursively call nsp_do_glob.
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
      result = nsp_do_glob( separators,
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
     * recursively call nsp_do_glob.  For each file that matches, it will
     * add the match onto the interp->result, or call nsp_do_glob if there
     * are more characters to be processed.
     */

    return nsp_match_files( separators, headPtr, tail, p,S);
  }
  nsp_tcldstring_append(headPtr, tail, p-tail);
  if (*p != '\0') {
    return nsp_do_glob( separators, headPtr, p,S);
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


