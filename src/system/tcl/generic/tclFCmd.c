/*
 * tclFCmd.c
 *
 *      This file implements the generic portion of file manipulation 
 *      subcommands of the "file" command. 
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

/*
 *---------------------------------------------------------------------------
 *
 * nsp_file_rename_cmd
 *
 *	This procedure implements the "rename" subcommand of the "file"
 *      command.  Filename arguments need to be translated to native
 *	format before being passed to platform-specific code that
 *	implements rename functionality.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *   int argc;			 Number of arguments. 
 *   char **argv;		 Argument strings for rename Command 
 *   int force ;                 force option 
*
 *---------------------------------------------------------------------------
 */

int
nsp_file_rename_cmd(int  argc,char ** argv,int force)
{
    return FileCopyRename( argc, argv, 0,force);
}

/*
 *---------------------------------------------------------------------------
 *
 * nsp_file_copy_cmd
 *
 *	This procedure implements the "copy" subcommand of the "file"
 *	command.  Filename arguments need to be translated to native
 *	format before being passed to platform-specific code that
 *	implements copy functionality.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *---------------------------------------------------------------------------
 */

int nsp_file_copy_cmd(int argc,			/* Number of arguments. */
		   char **argv,		/* Argument strings passed to Tcl_FileCmd. */
		   int force)
{
    return FileCopyRename( argc, argv, 1,force);
}

/*
 *---------------------------------------------------------------------------
 *
 * FileCopyRename --
 *
 *	Performs the work of nsp_file_rename_cmd and nsp_file_copy_cmd.
 *	See comments for those procedures.
 *
 * Results:
 *	See above.
 *
 * Side effects:
 *	See above.
 *
 *---------------------------------------------------------------------------
 */

static int
FileCopyRename(
	       int argc,			/* Number of arguments. */
	       char **argv,		/* Argument strings passed to Tcl_FileCmd. */
	       int copyFlag,		/* If non-zero, copy source(s).  Otherwise,
				 * rename them. */
	       int forceFlag              /* force flag for copy or rename */)
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
	Tcl_AppendResult( "error ",
			 ((copyFlag) ? "copying" : "renaming"), ": target \"",
			 argv[argc - 1], "\" is not a directory", (char *) NULL);
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

/*
 *---------------------------------------------------------------------------
 *
 * nsp_file_make_dirs_cmd
 *
 *	This procedure implements the "mkdir" subcommand of the "file"
 *      command.  Filename arguments need to be translated to native
 *	format before being passed to platform-specific code that
 *	implements mkdir functionality.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */
int nsp_file_make_dirs_cmd(
		       int argc,			/* Number of arguments */
		       char **argv		/* Argument strings giving the dirs to create  */)
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
    if (errfile != NULL) {
	Tcl_AppendResult( "can't create directory \"",
		errfile, "\": ", nsp_posix_error(), (char *) NULL);
	result = TCL_ERROR;
    }

    nsp_tcldstring_free(&nameBuffer);
    nsp_tcldstring_free(&targetBuffer);
    if (pargv != NULL) {
	ckfree((char *) pargv);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * nsp_file_delete_cmd
 *
 *	This procedure implements the "delete" subcommand of the "file"
 *      command.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

int
nsp_file_delete_cmd(int argc,			/* Number of arguments */
		 char **argv,		/* Argument strings passed to Tcl_FileCmd. */
		 int forceFlag              /* force Argument */)
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
		if ((force == 0) && (errno == EEXIST)) {
		    Tcl_AppendResult( "error deleting \"", argv[i],
			    "\": directory not empty", (char *) NULL);
		    nsp_posix_error();
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
    if (result != TCL_OK) {
	Tcl_AppendResult("error deleting \"", errfile,
		"\": ", nsp_posix_error(), (char *) NULL);
    } 
    done:
    nsp_tcldstring_free(&errorBuffer);
    nsp_tcldstring_free(&nameBuffer);
    return result;
}

/*
 *---------------------------------------------------------------------------
 *
 * CopyRenameOneFile
 *
 *	Copies or renames specified source file or directory hierarchy
 *	to the specified target.  
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Target is overwritten if the force flag is set.  Attempting to
 *	copy/rename a file onto a directory or a directory onto a file
 *	will always result in an error.  
 *
 *----------------------------------------------------------------------
 */

static int
CopyRenameOneFile(
		  char *source,		/* Pathname of file to copy.  May need to
				 * be translated. */
		  char *target,		/* Pathname of file to create/overwrite.
				 * May need to be translated. */
		  int copyFlag,		/* If non-zero, copy files.  Otherwise,
				 * rename them. */
		  int force			/* If non-zero, overwrite target file if it
				 * exists.  Otherwise, error if target already
				 * exists. */)
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
	    Tcl_AppendResult( "can't overwrite file \"", target,
		    "\" with directory \"", source, "\"", (char *) NULL);
	    goto done;
	}
	if (!S_ISDIR(sourceStatBuf.st_mode)
	        && S_ISDIR(targetStatBuf.st_mode)) {
	    errno = EISDIR;
	    Tcl_AppendResult( "can't overwrite directory \"", target, 
	            "\" with file \"", source, "\"", (char *) NULL);
	    goto done;
	}
    }

    if (copyFlag == 0) {
	result = nsp_rename_file(sourceName, targetName);
	if (result == TCL_OK) {
	    goto done;
	}
	    
	if (errno == EINVAL) {
	    Tcl_AppendResult( "error renaming \"", source, "\" to \"",
		    target, "\": trying to rename a volume or ",
		    "move a directory into itself", (char *) NULL);
	    goto done;
	} else if (errno != EXDEV) {
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
	if (result != TCL_OK) {
	    Tcl_AppendResult( "can't unlink \"", errfile, "\": ",
		    nsp_posix_error(), (char *) NULL);
	    errfile = NULL;
	}
    }
    
    done:
    if (errfile != NULL) {
	Tcl_AppendResult(
		((copyFlag) ? "error copying \"" : "error renaming \""),
		source, (char *) NULL);
	if (errfile != source) {
	    Tcl_AppendResult( "\" to \"", target, (char *) NULL);
	    if (errfile != target) {
		Tcl_AppendResult("\": \"", errfile, (char *) NULL);
	    }
	}
	Tcl_AppendResult( "\": ", nsp_posix_error(),
		(char *) NULL);
    }
    nsp_tcldstring_free(&errorBuffer);
    nsp_tcldstring_free(&sourcePath);
    nsp_tcldstring_free(&targetPath);
    return result;
}

/*
 *---------------------------------------------------------------------------
 *
 * FileBasename --
 *
 *	Given a path in either tcl format (with / separators), or in the
 *	platform-specific format for the current platform, return all the
 *	characters in the path after the last directory separator.  But,
 *	if path is the root directory, returns no characters.
 *
 * Results:
 *	Appends the string that represents the basename to the end of
 *	the specified initialized DString, returning a pointer to the
 *	resulting string.  If there is an error, an error message is left
 *	in interp, NULL is returned, and the nsp_tcldstring is unmodified.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */

static char *FileBasename(
			  char *path,			/* Path whose basename to extract. */
			  nsp_tcldstring *bufferPtr	/* Initialized DString that receives
							 * basename. */)
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
