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

static int		GetWinFileAttributes (
			    int objIndex, char *fileName,
			    NspObject **attributePtrPtr);
static int		GetWinFileLongName (
			    int objIndex, char *fileName,
			    NspObject **attributePtrPtr);
static int		GetWinFileShortName (
			    int objIndex, char *fileName,
			    NspObject **attributePtrPtr);
static int		SetWinFileAttributes (
			    int objIndex, char *fileName,
			    NspObject *attributePtr);
static int		CannotSetAttribute (
			    int objIndex, char *fileName,
			    NspObject *attributePtr);

/*
 * Constants and variables necessary for file attributes subcommand.
 */

enum {
    WIN_ARCHIVE_ATTRIBUTE,
    WIN_HIDDEN_ATTRIBUTE,
    WIN_LONGNAME_ATTRIBUTE,
    WIN_READONLY_ATTRIBUTE,
    WIN_SHORTNAME_ATTRIBUTE,
    WIN_SYSTEM_ATTRIBUTE
};

static int attributeArray[] = {FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_HIDDEN,
	0, FILE_ATTRIBUTE_READONLY, 0, FILE_ATTRIBUTE_SYSTEM};


char *tclpFileAttrStrings[] = {"-archive", "-hidden", "-longname", "-readonly",
	"-shortname", "-system", (char *) NULL};
const TclFileAttrProcs tclpFileAttrProcs[] = {
	{GetWinFileAttributes, SetWinFileAttributes},
	{GetWinFileAttributes, SetWinFileAttributes},
	{GetWinFileLongName, CannotSetAttribute},
	{GetWinFileAttributes, SetWinFileAttributes},
	{GetWinFileShortName, CannotSetAttribute},
	{GetWinFileAttributes, SetWinFileAttributes}};

/*
 * Prototype for the TraverseWinTree callback function.
 */

typedef int (TraversalProc)(char *src, char *dst, DWORD attr, int type, 
	nsp_tcldstring *errorPtr);

/*
 * Declarations for local procedures defined in this file:
 */

static void		AttributesPosixError (
			    int objIndex, char *fileName, int getOrSet);
static int		ConvertFileNameFormat (
			    int objIndex, char *fileName, int longShort,
			    NspObject **attributePtrPtr);
static int		TraversalCopy(char *src, char *dst, DWORD attr, 
				int type, nsp_tcldstring *errorPtr);
static int		TraversalDelete(char *src, char *dst, DWORD attr,
				int type, nsp_tcldstring *errorPtr);
static int		TraverseWinTree(TraversalProc *traverseProc,
			    nsp_tcldstring *sourcePtr, nsp_tcldstring *destPtr,
			    nsp_tcldstring *errorPtr);


/*
 *---------------------------------------------------------------------------
 *
 * TclpRenameFile --
 *
 *      Changes the name of an existing file or directory, from src to dst.
 *	If src and dst refer to the same file or directory, does nothing
 *	and returns success.  Otherwise if dst already exists, it will be
 *	deleted and replaced by src subject to the following conditions:
 *	    If src is a directory, dst may be an empty directory.
 *	    If src is a file, dst may be a file.
 *	In any other situation where dst already exists, the rename will
 *	fail.  
 *
 * Results:
 *	If the directory was successfully created, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR and errno is set to
 *	indicate the error.  Some possible values for errno are:
 *
 *	EACCES:     src or dst parent directory can't be read and/or written.
 *	EEXIST:	    dst is a non-empty directory.
 *	EINVAL:	    src is a root directory or dst is a subdirectory of src.
 *	EISDIR:	    dst is a directory, but src is not.
 *	ENOENT:	    src doesn't exist.  src or dst is "".
 *	ENOTDIR:    src is a directory, but dst is not.  
 *	EXDEV:	    src and dst are on different filesystems.
 *
 *	EACCES:     exists an open file already referring to src or dst.
 *	EACCES:     src or dst specify the current working directory (NT).
 *	EACCES:	    src specifies a char device (nul:, com1:, etc.) 
 *	EEXIST:	    dst specifies a char device (nul:, com1:, etc.) (NT)
 *	EACCES:	    dst specifies a char device (nul:, com1:, etc.) (95)
 *	
 * Side effects:
 *	The implementation supports cross-filesystem renames of files,
 *	but the caller should be prepared to emulate cross-filesystem
 *	renames of directories if errno is EXDEV.
 *
 *---------------------------------------------------------------------------
 */

int
nsp_rename_file(
    char *src,			/* Pathname of file or dir to be renamed. */ 
    char *dst)			/* New pathname for file or directory. */
{
    DWORD srcAttr, dstAttr;
    
    /*
     * Would throw an exception under NT if one of the arguments is a 
     * char block device.
     */
    /*  XXX
    try {
	if (MoveFile(src, dst) != FALSE) {
	    return TCL_OK;
	}
    } except (-1) {}
    */

    if (MoveFile(src, dst) != FALSE) {
      return TCL_OK;
    }


    TclWinConvertError(GetLastError());

    srcAttr = GetFileAttributes(src);
    dstAttr = GetFileAttributes(dst);
    if (srcAttr == (DWORD) -1) {
	srcAttr = 0;
    }
    if (dstAttr == (DWORD) -1) {
	dstAttr = 0;
    }

    if (errno == EBADF) {
	errno = EACCES;
	return TCL_ERROR;
    }
    if ((errno == EACCES) && (TclWinGetPlatformId() == VER_PLATFORM_WIN32s)) {
	if ((srcAttr != 0) && (dstAttr != 0)) {
	    /*
	     * Win32s reports trying to overwrite an existing file or directory
	     * as EACCES.
	     */

	    errno = EEXIST;
	}
    }
    if (errno == EACCES) {
	decode:
	if (srcAttr & FILE_ATTRIBUTE_DIRECTORY) {
	    char srcPath[MAXPATHLEN], dstPath[MAXPATHLEN];
	    int srcArgc, dstArgc;
	    char **srcArgv, **dstArgv;
	    char *srcRest, *dstRest;
	    int size;

	    size = GetFullPathName(src, sizeof(srcPath), srcPath, &srcRest);
	    if ((size == 0) || (size > sizeof(srcPath))) {
		return TCL_ERROR;
	    }
	    size = GetFullPathName(dst, sizeof(dstPath), dstPath, &dstRest);
	    if ((size == 0) || (size > sizeof(dstPath))) {
		return TCL_ERROR;
	    }
	    if (srcRest == NULL) {
		srcRest = srcPath + strlen(srcPath);
	    }
	    if (strnicmp(srcPath, dstPath, srcRest - srcPath) == 0) {
		/*
		 * Trying to move a directory into itself.
		 */

		errno = EINVAL;
		return TCL_ERROR;
	    }
	    nsp_split_path(srcPath, &srcArgc, &srcArgv);
	    nsp_split_path(dstPath, &dstArgc, &dstArgv);
	    if (srcArgc == 1) {
		/*
		 * They are trying to move a root directory.  Whether
		 * or not it is across filesystems, this cannot be
		 * done.
		 */

		errno = EINVAL;
	    } else if ((srcArgc > 0) && (dstArgc > 0) &&
		    (stricmp(srcArgv[0], dstArgv[0]) != 0)) {
		/*
		 * If src is a directory and dst filesystem != src
		 * filesystem, errno should be EXDEV.  It is very
		 * important to get this behavior, so that the caller
		 * can respond to a cross filesystem rename by
		 * simulating it with copy and delete.  The MoveFile
		 * system call already handles the case of moving a
		 * file between filesystems.
		 */

		errno = EXDEV;
	    }

	    ckfree((char *) srcArgv);
	    ckfree((char *) dstArgv);
	}

	/*
	 * Other types of access failure is that dst is a read-only
	 * filesystem, that an open file referred to src or dest, or that
	 * src or dest specified the current working directory on the
	 * current filesystem.  EACCES is returned for those cases.
	 */

    } else if (errno == EEXIST) {
	/*
	 * Reports EEXIST any time the target already exists.  If it makes
	 * sense, remove the old file and try renaming again.
	 */

	if (srcAttr & FILE_ATTRIBUTE_DIRECTORY) {
	    if (dstAttr & FILE_ATTRIBUTE_DIRECTORY) {
		/*
		 * Overwrite empty dst directory with src directory.  The
		 * following call will remove an empty directory.  If it
		 * fails, it's because it wasn't empty.
		 */

		if (nsp_remove_directory(dst, 0, NULL) == TCL_OK) {
		    /*
		     * Now that that empty directory is gone, we can try
		     * renaming again.  If that fails, we'll put this empty
		     * directory back, for completeness.
		     */

		    if (MoveFile(src, dst) != FALSE) {
			return TCL_OK;
		    }

		    /*
		     * Some new error has occurred.  Don't know what it
		     * could be, but report this one.
		     */

		    TclWinConvertError(GetLastError());
		    CreateDirectory(dst, NULL);
		    SetFileAttributes(dst, dstAttr);
		    if (errno == EACCES) {
			/*
			 * Decode the EACCES to a more meaningful error.
			 */

			goto decode;
		    }
		}
	    } else {	/* (dstAttr & FILE_ATTRIBUTE_DIRECTORY) == 0 */
		errno = ENOTDIR;
	    }
	} else {    /* (srcAttr & FILE_ATTRIBUTE_DIRECTORY) == 0 */
	    if (dstAttr & FILE_ATTRIBUTE_DIRECTORY) {
		errno = EISDIR;
	    } else {
		/*
		 * Overwrite existing file by:
		 * 
		 * 1. Rename existing file to temp name.
		 * 2. Rename old file to new name.
		 * 3. If success, delete temp file.  If failure,
		 *    put temp file back to old name.
		 */

		char tempName[MAXPATHLEN];
		int result, size;
		char *rest;
		
		size = GetFullPathName(dst, sizeof(tempName), tempName, &rest);
		if ((size == 0) || (size > sizeof(tempName)) || (rest == NULL)) {
		    return TCL_ERROR;
		}
		*rest = '\0';
		result = TCL_ERROR;
		if (GetTempFileName(tempName, "tclr", 0, tempName) != 0) {
		    /*
		     * Strictly speaking, need the following DeleteFile and
		     * MoveFile to be joined as an atomic operation so no
		     * other app comes along in the meantime and creates the
		     * same temp file.
		     */
		     
		    DeleteFile(tempName);
		    if (MoveFile(dst, tempName) != FALSE) {
			if (MoveFile(src, dst) != FALSE) {
			    SetFileAttributes(tempName, FILE_ATTRIBUTE_NORMAL);
			    DeleteFile(tempName);
			    return TCL_OK;
			} else {
			    DeleteFile(dst);
			    MoveFile(tempName, dst);
			}
		    } 

		    /*
		     * Can't backup dst file or move src file.  Return that
		     * error.  Could happen if an open file refers to dst.
		     */

		    TclWinConvertError(GetLastError());
		    if (errno == EACCES) {
			/*
			 * Decode the EACCES to a more meaningful error.
			 */

			goto decode;
		    }
		}
		return result;
	    }
	}
    }
    return TCL_ERROR;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpCopyFile --
 *
 *      Copy a single file (not a directory).  If dst already exists and
 *	is not a directory, it is removed.
 *
 * Results:
 *	If the file was successfully copied, returns TCL_OK.  Otherwise
 *	the return value is TCL_ERROR and errno is set to indicate the
 *	error.  Some possible values for errno are:
 *
 *	EACCES:     src or dst parent directory can't be read and/or written.
 *	EISDIR:	    src or dst is a directory.
 *	ENOENT:	    src doesn't exist.  src or dst is "".
 *
 *	EACCES:     exists an open file already referring to dst (95).
 *	EACCES:	    src specifies a char device (nul:, com1:, etc.) (NT)
 *	ENOENT:	    src specifies a char device (nul:, com1:, etc.) (95)
 *
 * Side effects:
 *	It is not an error to copy to a char device.
 *
 *---------------------------------------------------------------------------
 */

int 
nsp_copy_file(
    char *src,			/* Pathname of file to be copied. */
    char *dst)			/* Pathname of file to copy to. */
{
    /*
     * Would throw an exception under NT if one of the arguments is a char
     * block device.
     */
  /* XXX 
    try {
	if (CopyFile(src, dst, 0) != FALSE) {
	    return TCL_OK;
	}
    } except (-1) {}
  */
  if (CopyFile(src, dst, 0) != FALSE) {
    return TCL_OK;
  }
  TclWinConvertError(GetLastError());
    if (errno == EBADF) {
	errno = EACCES;
	return TCL_ERROR;
    }
    if (errno == EACCES) {
	DWORD srcAttr, dstAttr;

	srcAttr = GetFileAttributes(src);
	dstAttr = GetFileAttributes(dst);
	if (srcAttr != (DWORD) -1) {
	    if (dstAttr == (DWORD) -1) {
		dstAttr = 0;
	    }
	    if ((srcAttr & FILE_ATTRIBUTE_DIRECTORY) ||
		    (dstAttr & FILE_ATTRIBUTE_DIRECTORY)) {
		errno = EISDIR;
	    }
	    if (dstAttr & FILE_ATTRIBUTE_READONLY) {
		SetFileAttributes(dst, dstAttr & ~FILE_ATTRIBUTE_READONLY);
		if (CopyFile(src, dst, 0) != FALSE) {
		    return TCL_OK;
		}
		/*
		 * Still can't copy onto dst.  Return that error, and
		 * restore attributes of dst.
		 */

		TclWinConvertError(GetLastError());
		SetFileAttributes(dst, dstAttr);
	    }
	}
    }
    return TCL_ERROR;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpDeleteFile --
 *
 *      Removes a single file (not a directory).
 *
 * Results:
 *	If the file was successfully deleted, returns TCL_OK.  Otherwise
 *	the return value is TCL_ERROR and errno is set to indicate the
 *	error.  Some possible values for errno are:
 *
 *	EACCES:     a parent directory can't be read and/or written.
 *	EISDIR:	    path is a directory.
 *	ENOENT:	    path doesn't exist or is "".
 *
 *	EACCES:     exists an open file already referring to path.
 *	EACCES:	    path is a char device (nul:, com1:, etc.)
 *
 * Side effects:
 *      The file is deleted, even if it is read-only.
 *
 *---------------------------------------------------------------------------
 */

int
nsp_delete_file(
    char *path)			/* Pathname of file to be removed. */
{
    DWORD attr;

    if (DeleteFile(path) != FALSE) {
	return TCL_OK;
    }
    TclWinConvertError(GetLastError());
    if (path[0] == '\0') {
	/*
	 * Win32s thinks that "" is the same as "." and then reports EISDIR
	 * instead of ENOENT.
	 */

	errno = ENOENT;
    } else if (errno == EACCES) {
        attr = GetFileAttributes(path);
	if (attr != (DWORD) -1) {
	    if (attr & FILE_ATTRIBUTE_DIRECTORY) {
		/*
		 * Windows NT reports removing a directory as EACCES instead
		 * of EISDIR.
		 */

		errno = EISDIR;
	    } else if (attr & FILE_ATTRIBUTE_READONLY) {
		SetFileAttributes(path, attr & ~FILE_ATTRIBUTE_READONLY);
		if (DeleteFile(path) != FALSE) {
		    return TCL_OK;
		}
		TclWinConvertError(GetLastError());
		SetFileAttributes(path, attr);
	    }
	}
    } else if (errno == ENOENT) {
        attr = GetFileAttributes(path);
	if (attr != (DWORD) -1) {
	    if (attr & FILE_ATTRIBUTE_DIRECTORY) {
	    	/*
		 * Windows 95 reports removing a directory as ENOENT instead 
		 * of EISDIR. 
		 */

		errno = EISDIR;
	    }
	}
    } else if (errno == EINVAL) {
	/*
	 * Windows NT reports removing a char device as EINVAL instead of
	 * EACCES.
	 */

	errno = EACCES;
    }

    return TCL_ERROR;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpCreateDirectory --
 *
 *      Creates the specified directory.  All parent directories of the
 *	specified directory must already exist.  The directory is
 *	automatically created with permissions so that user can access
 *	the new directory and create new files or subdirectories in it.
 *
 * Results:
 *	If the directory was successfully created, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR and errno is set to
 *	indicate the error.  Some possible values for errno are:
 *
 *	EACCES:     a parent directory can't be read and/or written.
 *	EEXIST:	    path already exists.
 *	ENOENT:	    a parent directory doesn't exist.
 *
 * Side effects:
 *      A directory is created.
 *
 *---------------------------------------------------------------------------
 */

int
nsp_create_directory(
    char *path)			/* Pathname of directory to create */
{
    int error;

    if (CreateDirectory(path, NULL) == 0) {
	error = GetLastError();
	if (TclWinGetPlatformId() == VER_PLATFORM_WIN32s) {
	    if ((error == ERROR_ACCESS_DENIED) 
		    && (GetFileAttributes(path) != (DWORD) -1)) {
		error = ERROR_FILE_EXISTS;
	    }
	}
	TclWinConvertError(error);
	return TCL_ERROR;
    }   
    return TCL_OK;
}

/*
 *---------------------------------------------------------------------------
 *
 * TclpCopyDirectory --
 *
 *      Recursively copies a directory.  The target directory dst must
 *	not already exist.  Note that this function does not merge two
 *	directory hierarchies, even if the target directory is an an
 *	empty directory.
 *
 * Results:
 *	If the directory was successfully copied, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR, errno is set to indicate
 *	the error, and the pathname of the file that caused the error
 *	is stored in errorPtr.  See TclpCreateDirectory and TclpCopyFile
 *	for a description of possible values for errno.
 *
 * Side effects:
 *      An exact copy of the directory hierarchy src will be created
 *	with the name dst.  If an error occurs, the error will
 *      be returned immediately, and remaining files will not be
 *	processed.
 *
 *---------------------------------------------------------------------------
 */

int
nsp_copy_directory(
    char *src,			/* Pathname of directory to be copied. */
    char *dst,			/* Pathname of target directory. */
    nsp_tcldstring *errorPtr)	/* If non-NULL, initialized DString for
				 * error reporting. */
{
    int result;
    nsp_tcldstring srcBuffer;
    nsp_tcldstring dstBuffer;

    nsp_tcldstring_init(&srcBuffer);
    nsp_tcldstring_init(&dstBuffer);
    nsp_tcldstring_append(&srcBuffer, src, -1);
    nsp_tcldstring_append(&dstBuffer, dst, -1);
    result = TraverseWinTree(TraversalCopy, &srcBuffer, &dstBuffer, 
	    errorPtr);
    nsp_tcldstring_free(&srcBuffer);
    nsp_tcldstring_free(&dstBuffer);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TclpRemoveDirectory -- 
 *
 *	Removes directory (and its contents, if the recursive flag is set).
 *
 * Results:
 *	If the directory was successfully removed, returns TCL_OK.
 *	Otherwise the return value is TCL_ERROR, errno is set to indicate
 *	the error, and the pathname of the file that caused the error
 *	is stored in errorPtr.  Some possible values for errno are:
 *
 *	EACCES:     path directory can't be read and/or written.
 *	EEXIST:	    path is a non-empty directory.
 *	EINVAL:	    path is root directory or current directory.
 *	ENOENT:	    path doesn't exist or is "".
 * 	ENOTDIR:    path is not a directory.
 *
 *	EACCES:	    path is a char device (nul:, com1:, etc.) (95)
 *	EINVAL:	    path is a char device (nul:, com1:, etc.) (NT)
 *
 * Side effects:
 *	Directory removed.  If an error occurs, the error will be returned
 *	immediately, and remaining files will not be deleted.
 *
 *----------------------------------------------------------------------
 */

int
nsp_remove_directory(
    char *path,			/* Pathname of directory to be removed. */
    int recursive,		/* If non-zero, removes directories that
				 * are nonempty.  Otherwise, will only remove
				 * empty directories. */
    nsp_tcldstring *errorPtr)	/* If non-NULL, initialized DString for
				 * error reporting. */
{
    int result;
    nsp_tcldstring buffer;
    DWORD attr;

    if (RemoveDirectory(path) != FALSE) {
	return TCL_OK;
    }
    TclWinConvertError(GetLastError());
    if (path[0] == '\0') {
	/*
	 * Win32s thinks that "" is the same as "." and then reports EACCES
	 * instead of ENOENT.
	 */

	errno = ENOENT;
    }
    if (errno == EACCES) {
	attr = GetFileAttributes(path);
	if (attr != (DWORD) -1) {
	    if ((attr & FILE_ATTRIBUTE_DIRECTORY) == 0) {
		/* 
		 * Windows 95 reports calling RemoveDirectory on a file as an 
		 * EACCES, not an ENOTDIR.
		 */
		
		errno = ENOTDIR;
		goto end;
	    }

	    if (attr & FILE_ATTRIBUTE_READONLY) {
		attr &= ~FILE_ATTRIBUTE_READONLY;
		if (SetFileAttributes(path, attr) == FALSE) {
		    goto end;
		}
		if (RemoveDirectory(path) != FALSE) {
		    return TCL_OK;
		}
		TclWinConvertError(GetLastError());
		SetFileAttributes(path, attr | FILE_ATTRIBUTE_READONLY);
	    }

	    /* 
	     * Windows 95 and Win32s report removing a non-empty directory 
	     * as EACCES, not EEXIST.  If the directory is not empty,
	     * change errno so caller knows what's going on.
	     */

	    if (TclWinGetPlatformId() != VER_PLATFORM_WIN32_NT) {
		HANDLE handle;
		WIN32_FIND_DATA data;
		nsp_tcldstring buffer;
		char *find;
		int len;

		nsp_tcldstring_init(&buffer);
		find = nsp_tcldstring_append(&buffer, path, -1);
		len = nsp_tcldstring_length(&buffer);
		if ((len > 0) && (find[len - 1] != '\\')) {
		    nsp_tcldstring_append(&buffer, "\\", 1);
		}
		find = nsp_tcldstring_append(&buffer, "*.*", 3);
		handle = FindFirstFile(find, &data);
		if (handle != INVALID_HANDLE_VALUE) {
		    while (1) {
			if ((strcmp(data.cFileName, ".") != 0)
				&& (strcmp(data.cFileName, "..") != 0)) {
			    /*
			     * Found something in this directory.
			     */

			    errno = EEXIST;
			    break;
			}
			if (FindNextFile(handle, &data) == FALSE) {
			    break;
			}
		    }
		    FindClose(handle);
		}
		nsp_tcldstring_free(&buffer);
	    }
	}
    }
    if (errno == ENOTEMPTY) {
	/* 
	 * The caller depends on EEXIST to signify that the directory is
	 * not empty, not ENOTEMPTY. 
	 */

	errno = EEXIST;
    }
    if ((recursive != 0) && (errno == EEXIST)) {
	/*
	 * The directory is nonempty, but the recursive flag has been
	 * specified, so we recursively remove all the files in the directory.
	 */

	nsp_tcldstring_init(&buffer);
	nsp_tcldstring_append(&buffer, path, -1);
	result = TraverseWinTree(TraversalDelete, &buffer, NULL, errorPtr);
	nsp_tcldstring_free(&buffer);
	return result;
    }

    end:
    if (errorPtr != NULL) {
        nsp_tcldstring_append(errorPtr, path, -1);
    }
    return TCL_ERROR;
}

/*
 *---------------------------------------------------------------------------
 *
 * TraverseWinTree --
 *
 *      Traverse directory tree specified by sourcePtr, calling the function 
 *	traverseProc for each file and directory encountered.  If destPtr 
 *	is non-null, each of name in the sourcePtr directory is appended to 
 *	the directory specified by destPtr and passed as the second argument 
 *	to traverseProc() .
 *
 * Results:
 *      Standard Tcl result.
 *
 * Side effects:
 *      None caused by TraverseWinTree, however the user specified 
 *	traverseProc() may change state.  If an error occurs, the error will
 *      be returned immediately, and remaining files will not be processed.
 *
 *---------------------------------------------------------------------------
 */

static int 
TraverseWinTree(
    TraversalProc *traverseProc,/* Function to call for every file and
				 * directory in source hierarchy. */
    nsp_tcldstring *sourcePtr,	/* Pathname of source directory to be
				 * traversed. */
    nsp_tcldstring *targetPtr,	/* Pathname of directory to traverse in
				 * parallel with source directory. */
    nsp_tcldstring *errorPtr)	/* If non-NULL, an initialized DString for
				 * error reporting. */
{
    DWORD sourceAttr;
    char *source, *target, *errfile;
    int result, sourceLen, targetLen=0, sourceLenOriginal, targetLenOriginal;
    HANDLE handle;
    WIN32_FIND_DATA data;

    result = TCL_OK;
    source = nsp_tcldstring_value(sourcePtr);
    sourceLenOriginal = nsp_tcldstring_length(sourcePtr);
    if (targetPtr != NULL) {
	target = nsp_tcldstring_value(targetPtr);
	targetLenOriginal = nsp_tcldstring_length(targetPtr);
    } else {
	target = NULL;
	targetLenOriginal = 0;
    }

    errfile = NULL;

    sourceAttr = GetFileAttributes(source);
    if (sourceAttr == (DWORD) -1) {
	errfile = source;
	goto end;
    }
    if ((sourceAttr & FILE_ATTRIBUTE_DIRECTORY) == 0) {
	/*
	 * Process the regular file
	 */

	return (*traverseProc)(source, target, sourceAttr, DOTREE_F, errorPtr);
    }

    /*
     * When given the pathname of the form "c:\" (one that already ends
     * with a backslash), must make sure not to add another "\" to the end
     * otherwise it will try to access a network drive.  
     */

    sourceLen = sourceLenOriginal;
    if ((sourceLen > 0) && (source[sourceLen - 1] != '\\')) {
	nsp_tcldstring_append(sourcePtr, "\\", 1);
	sourceLen++;
    }
    source = nsp_tcldstring_append(sourcePtr, "*.*", 3); 
    handle = FindFirstFile(source, &data);
    nsp_tcldstring_set_length(sourcePtr, sourceLen);
    if (handle == INVALID_HANDLE_VALUE) {
	/* 
	 * Can't read directory
	 */

	TclWinConvertError(GetLastError());
	errfile = source;
	goto end;
    }

    result = (*traverseProc)(source, target, sourceAttr, DOTREE_PRED, errorPtr);
    if (result != TCL_OK) {
	FindClose(handle);
	return result;
    }

    if (targetPtr != NULL) {
	targetLen = targetLenOriginal;
	if ((targetLen > 0) && (target[targetLen - 1] != '\\')) {
	    target = nsp_tcldstring_append(targetPtr, "\\", 1);
	    targetLen++;
	}
    }

    while (1) {
	if ((strcmp(data.cFileName, ".") != 0)
	        && (strcmp(data.cFileName, "..") != 0)) {
	    /* 
	     * _append name after slash, and recurse on the file. 
	     */

	    nsp_tcldstring_append(sourcePtr, data.cFileName, -1);
	    if (targetPtr != NULL) {
		nsp_tcldstring_append(targetPtr, data.cFileName, -1);
	    }
	    result = TraverseWinTree(traverseProc, sourcePtr, targetPtr, 
		    errorPtr);
	    if (result != TCL_OK) {
		break;
	    }

	    /*
	     * Remove name after slash.
	     */

	    nsp_tcldstring_set_length(sourcePtr, sourceLen);
	    if (targetPtr != NULL) {
		nsp_tcldstring_set_length(targetPtr, targetLen);
	    }
	}
	if (FindNextFile(handle, &data) == FALSE) {
	    break;
	}
    }
    FindClose(handle);

    /*
     * Strip off the trailing slash we added
     */

    nsp_tcldstring_set_length(sourcePtr, sourceLenOriginal);
    source = nsp_tcldstring_value(sourcePtr);
    if (targetPtr != NULL) {
	nsp_tcldstring_set_length (targetPtr, targetLenOriginal);
	target = nsp_tcldstring_value (targetPtr);
    }

    if (result == TCL_OK) {
	/*
	 * Call traverseProc() on a directory after visiting all the
	 * files in that directory.
	 */

	result = (*traverseProc)(source, target, sourceAttr, 
		DOTREE_POSTD, errorPtr);
    }
    end:
    if (errfile != NULL) {
	TclWinConvertError(GetLastError());
	if (errorPtr != NULL) {
	    nsp_tcldstring_append(errorPtr, errfile, -1);
	}
	result = TCL_ERROR;
    }
	    
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TraversalCopy
 *
 *      Called from TraverseUnixTree in order to execute a recursive
 *      copy of a directory.
 *
 * Results:
 *      Standard Tcl result.
 *
 * Side effects:
 *      Depending on the value of type, src may be copied to dst.
 *      
 *----------------------------------------------------------------------
 */

static int 
TraversalCopy(
    char *src,			/* Source pathname to copy. */
    char *dst,			/* Destination pathname of copy. */
    DWORD srcAttr,		/* File attributes for src. */
    int type,			/* Reason for call - see TraverseWinTree() */
    nsp_tcldstring *errorPtr)	/* If non-NULL, initialized DString for
				 * error return. */
{
    switch (type) {
	case DOTREE_F:
	    if (nsp_copy_file(src, dst) == TCL_OK) {
		return TCL_OK;
	    }
	    break;

	case DOTREE_PRED:
	    if (nsp_create_directory(dst) == TCL_OK) {
		if (SetFileAttributes(dst, srcAttr) != FALSE) {
		    return TCL_OK;
		}
		TclWinConvertError(GetLastError());
	    }
	    break;

        case DOTREE_POSTD:
	    return TCL_OK;

    }

    /*
     * There shouldn't be a problem with src, because we already
     * checked it to get here.
     */

    if (errorPtr != NULL) {
	nsp_tcldstring_append(errorPtr, dst, -1);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * TraversalDelete --
 *
 *      Called by procedure TraverseWinTree for every file and
 *      directory that it encounters in a directory hierarchy. This
 *      procedure unlinks files, and removes directories after all the
 *      containing files have been processed.
 *
 * Results:
 *      Standard Tcl result.
 *
 * Side effects:
 *      Files or directory specified by src will be deleted. If an
 *      error occurs, the windows error is converted to a Posix error
 *      and errno is set accordingly.
 *
 *----------------------------------------------------------------------
 */

static int
TraversalDelete( 
    char *src,			/* Source pathname. */
    char *ignore,		/* Destination pathname (not used). */
    DWORD srcAttr,		/* File attributes for src (not used). */
    int type,			/* Reason for call - see TraverseWinTree(). */
    nsp_tcldstring *errorPtr)	/* If non-NULL, initialized DString for
				 * error return. */
{
    switch (type) {
	case DOTREE_F:
	    if (nsp_delete_file(src) == TCL_OK) {
		return TCL_OK;
	    }
	    break;

	case DOTREE_PRED:
	    return TCL_OK;

	case DOTREE_POSTD:
	    if (nsp_remove_directory(src, 0, NULL) == TCL_OK) {
		return TCL_OK;
	    }
	    break;

    }

    if (errorPtr != NULL) {
	nsp_tcldstring_append(errorPtr, src, -1);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * AttributesPosixError --
 *
 *	Sets the object result with the appropriate error.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      The interp's object result is set with an error message
 *	based on the objIndex, fileName and errno.
 *
 *----------------------------------------------------------------------
 */

static void
AttributesPosixError(
    int objIndex,		/* The attribute which caused the problem. */
    char *fileName,		/* The name of the file which caused the 
				 * error. */
    int getOrSet)		/* 0 for get; 1 for set */
{
    TclWinConvertError(GetLastError());
    Scierror("cannot %s attribute \"%s\" for file \"%s\": %s\n ",
	     getOrSet ? "set" : "get", tclpFileAttrStrings[objIndex],
	     fileName, nsp_posix_error());
}

/*
 *----------------------------------------------------------------------
 *
 * GetWinFileAttributes --
 *
 *      Returns a Tcl_Obj containing the value of a file attribute.
 *	This routine gets the -hidden, -readonly or -system attribute.
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
GetWinFileAttributes(objIndex, fileName, attributePtrPtr)
     int objIndex;		/* The index of the attribute. */
     char *fileName;		/* The name of the file. */
     NspObject **attributePtrPtr;	/* A pointer to return the object with. */
{
  NspObject *O;
  DWORD result = GetFileAttributes(fileName);

  if (result == 0xFFFFFFFF) {
    AttributesPosixError( objIndex, fileName, 0);
    return TCL_ERROR;
  }

  if ((O =nsp_create_true_object("void"))==NULLOBJ)   return TCL_ERROR;
  ((NspBMatrix *) O)->B[0] = result & attributeArray[objIndex] ; 
  *attributePtrPtr = O;
  return TCL_OK;
}


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
  char **pathArgv;
  char **newPathArgv;
  char *currentElement, *resultStr;
  nsp_tcldstring resultDString;
  int result = TCL_OK;
  
  nsp_split_path(fileName, &pathArgc, &pathArgv);
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
      resultStr = nsp_join_path(i + 1,(const char **) pathArgv, &resultDString);
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
  resultStr = nsp_join_path(pathArgc,(const char **) newPathArgv, &resultDString);
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
 * SetWinFileAttributes --
 *
 *	Set the file attributes to the value given by attributePtr.
 *	This routine sets the -hidden, -readonly, or -system attributes.
 *
 * Results:
 *      Standard TCL error.
 *
 * Side effects:
 *      The file's attribute is set.
 *
 *----------------------------------------------------------------------
 */


static int
SetWinFileAttributes(objIndex, fileName, attributePtr)
    int objIndex;		    /* The index of the attribute. */
    char *fileName;		    /* The name of the file. */
    NspObject *attributePtr;	            /* The attribute to set. */
{
    DWORD fileAttributes = GetFileAttributes(fileName);
    int yesNo;

    if (fileAttributes == 0xFFFFFFFF) {
	AttributesPosixError( objIndex, fileName, 1);
	return TCL_ERROR;
    }

    if ( BoolScalar(attributePtr, &yesNo)== FAIL) 	return TCL_ERROR;

    if (yesNo) {
	fileAttributes |= (attributeArray[objIndex]);
    } else {
	fileAttributes &= ~(attributeArray[objIndex]);
    }

    if (!SetFileAttributes(fileName, fileAttributes)) {
	AttributesPosixError( objIndex, fileName, 1);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * SetWinFileLongName --
 *
 *	The attribute in question is a readonly attribute and cannot
 *	be set.
 *
 * Results:
 *      TCL_ERROR
 *
 * Side effects:
 *      The object result is set to a pertinant error message.
 *
 *----------------------------------------------------------------------
 */

static int
CannotSetAttribute(objIndex, fileName, attributePtr)
    int objIndex;		    /* The index of the attribute. */
    char *fileName;		    /* The name of the file. */
    NspObject *attributePtr;	            /* The attribute to set. */
{
  Scierror("cannot set attribute \"%s\" for file \"%s\" : attribute is readonly\n" ,
	   tclpFileAttrStrings[objIndex], fileName);
  return TCL_ERROR;
}


/*
 *---------------------------------------------------------------------------
 *
 * TclpListVolumes --
 *
 *	Lists the currently mounted volumes
 *
 * Results:
 *	A standard Tcl result.  Will always be TCL_OK, since there is no way
 *	that this command can fail.  Also, the interpreter's result is set to 
 *	the list of volumes.
 *
 * Side effects:
 *	None
 *
 *---------------------------------------------------------------------------
 */

int  nsp_list_volumes(Stack stack,int n)
{
  NspSMatrix *S;
  char buf[4];
  int i;
  
  buf[1] = ':';
  buf[2] = '/';
  buf[3] = '\0';

  /*
   * On Win32s: 
   * GetLogicalDriveStrings() isn't implemented.
   * GetLogicalDrives() returns incorrect information.
   */
  if ((S=nsp_smatrix_create("",0,0,".",0))== NULLSMAT) 
    return RET_BUG;

  for (i = 0; i < 26; i++) {
    buf[0] = (char) ('a' + i);
    if (GetVolumeInformation(buf, NULL, 0, NULL, NULL, NULL, NULL, 0)  
	|| (GetLastError() == ERROR_NOT_READY)) {
      if (nsp_row_smatrix_append_string(S, buf )== FAIL) 
	return RET_BUG;
    }
  }
  MoveObj(stack,n,(NspObject*) S);
  return 1;	
}
