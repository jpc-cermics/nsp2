/* 
 * Nsp
 * Copyright (C) 2004-2006 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * A set of interfaces which use Tcl functions 
 * Some of the following functions should be replaced by glib ones 
 *
 *--------------------------------------------------------------------------
 */

#include "tclInt.h"
#include "tclPort.h"
#include "nsp/stack.h" 
#include "../../files.h"
/*
 * Prototypes for local procedures defined in this file:
 */

static char *		GetTypeFromMode (int mode);
static NspSMatrix *	StoreStatData(struct stat *statPtr);
static int              StoreStat(NspSMatrix *S,int i,char *str1,char *str2);
static int              TclFileAttrsCmd(Stack stack,int,int,int);

/*
 * The following variable holds the full path name of the binary
 * from which this application was executed, or NULL if it isn't
 * know.  The value of the variable is set by the procedure
 * nsp_find_executable.  The storage space is dynamically allocated.
 */

char *tclExecutableName = NULL;

/*
 * int_syscd 
 *	This procedure is invoked to process the "cd" command.
 */

int int_syscd(Stack stack,int rhs,int opt,int lhs) 
{
  char old[FSIZE+1];
  char str_expanded[FSIZE+1];
  int result ;
  nsp_tcldstring buffer;
  char *dirName,*str;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* change current exec dir using str 
   */
  nsp_expand_dir_and_update_exec_dir(&stack,old,str,str_expanded);
  /* since expansion has been performed what follows 
   * is certainly not that important 
   */
  nsp_tcldstring_init(&buffer);
  dirName = nsp_translate_file_name( str_expanded, &buffer);
  if (dirName == NULL)  
    {
      nsp_reset_exec_dir(&stack,old);
      return RET_BUG;
    }
  result = nsp_chdir(dirName);
  nsp_tcldstring_free(&buffer);
  if (result ==  TCL_ERROR) 
    {
      nsp_reset_exec_dir(&stack,old);
      return RET_BUG;
    }
  else 
    {
      /* we must be sure that current exec dir is absolute */
      if (nsp_get_path_type(str_expanded) != TCL_PATH_ABSOLUTE) 
	{
	  strncpy(stack.val->current_exec_dir,nsp_get_cwd(),FSIZE);
	}
      /* we set current exec dir to an absolute value */
      return 0;
    }
}

/*
 *
 * int_sysfile
 *	This procedure is invoked to process the "file"  command.
 */

int int_sysfile(Stack stack,int rhs,int opt,int lhs) 
{
  char *fileName, *extension, *errorString;
  int statOp = 0;		/* Init. to avoid compiler warning. */
  int mode = 0;		/* Init. to avoid compiler warning. */
  struct stat statBuf;
  nsp_tcldstring buffer;
  int index, result=0;
  NspSMatrix *S;
  
  /*
   * This list of constants should match the fileOption string array below.
   */

  enum {FILE_ATIME, FILE_ATTRIBUTES, FILE_COPY, FILE_DELETE, FILE_DIRNAME,
	FILE_EXECUTABLE, FILE_EXISTS, FILE_EXTENSION, FILE_ISDIRECTORY,
	FILE_ISFILE, FILE_JOIN, FILE_LSTAT, FILE_MTIME, FILE_MKDIR,
	FILE_NATIVENAME, FILE_OWNED, FILE_PATHTYPE, FILE_READABLE,
	FILE_READLINK, FILE_RENAME, FILE_ROOTNAME, FILE_SIZE, FILE_SPLIT,
	FILE_STAT, FILE_TAIL, FILE_TYPE, FILE_VOLUMES, FILE_WRITABLE};
    
  static char *fileOptions[] = 
    {"atime", "attributes", "copy", "delete", 
     "dirname", "executable", "exists", "extension", "isdirectory", 
     "isfile", "join", "lstat", "mtime", "mkdir", "nativename", 
     "owned", "pathtype", "readable", "readlink", "rename",
     "rootname", "size", "split", "stat", "tail", "type", "volumes", 
     "writable", (char *) NULL
    };
    
  CheckRhs(1,10000); /* XXXX **/
  CheckLhs(0,1);

  /** First argument must be a string in fileOptions **/
  if ((index = GetStringInArray(stack,1,fileOptions,0)) == -1 ) return RET_BUG;
  
  /* 
   * First, do the volumes command, since it is the only one that
   * has rhs == 1.
   */
    
  if ( index == FILE_VOLUMES) {
    if ( rhs != 1 ) {
      Scierror("Error: function %s, wrong number of arguments for volume option\n",
	       NspFname(stack));
      return RET_BUG;
    }
    return nsp_list_volumes(stack,1);
  }
    
  if ( rhs < 2 ) {
    Scierror("Error: function %s, Not enough arguments \n",NspFname(stack));
    return RET_BUG;
  }
    
  nsp_tcldstring_init(&buffer);
    
  /*
   * Handle operations on the file name.
   */
    
  switch (index) 
    {
    case FILE_ATTRIBUTES: /* OK */
      result = TclFileAttrsCmd(stack,rhs,opt,lhs) ;
      goto done;
    case FILE_DIRNAME:	/* OK */
      {
	nsp_string dirname;
	if (rhs != 2) 
	  {
	    errorString = "dirname name";
	    goto not3Args;
	  }
	if ((fileName = GetString(stack,2)) == (char*)0) 
	  {
	    result = RET_BUG;
	    goto done;
	  }
	if ((dirname = nsp_dirname (fileName))== NULL)
	  {
	    result = RET_BUG;
	    goto done;
	  }
	result = nsp_move_string(stack,1, dirname, -1);
	nsp_string_destroy(&dirname);
	result = ( result == FAIL) ? RET_BUG: 1;
	goto done;
      }
    case FILE_TAIL: /* OK */ 
      {
	nsp_string tail;
	if (rhs != 2 ) {
	  errorString = "tail name";
	  goto not3Args;
	}

	if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;
	
	if ((tail = nsp_tail(fileName))==NULL)
	  {
	    result = RET_BUG;
	    goto done;
	  }
	result = nsp_move_string(stack,1,tail, -1);
	nsp_string_destroy(&tail);
	result = ( result == FAIL) ? RET_BUG: 1;
	goto done;
      }
    case FILE_ROOTNAME:  /* OK */
      {
	char *fileName;
	
	if (rhs != 2) {
	  errorString = "rootname name";
	  goto not3Args;
	}
	if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;
	extension = nsp_get_extension(fileName);
	if (extension == NULL) {
	  NthObj(2)->ret_pos = 1;
	  result = 1;
	} else {
	  result = nsp_move_string(stack,1, fileName, (int) (strlen(fileName) - strlen(extension)));
	  result = ( result == FAIL) ? RET_BUG: 1;
	}
	goto done; 
      }
    case FILE_EXTENSION: /* OK */
      if (rhs != 2) {
	errorString = "extension name";
	goto not3Args;
      }
      if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;
	
      extension = nsp_get_extension(fileName);
      result = nsp_move_string(stack,1,(extension != NULL)? extension:"", -1);	
      result = ( result == FAIL) ? RET_BUG: 1;
      goto done;
    case FILE_PATHTYPE:  /* OK */
      if (rhs != 2) {
	errorString = "pathtype name";
	goto not3Args;
      }
      if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;
      switch (nsp_get_path_type(fileName)) 
	{
	case TCL_PATH_ABSOLUTE:
	  result = nsp_move_string(stack,1, "absolute", -1);
	  result = ( result == FAIL) ? RET_BUG: 1;
	  break;
	case TCL_PATH_RELATIVE:
	  result =  nsp_move_string(stack,1, "relative", -1);
	  result = ( result == FAIL) ? RET_BUG: 1;
	  break;
	case TCL_PATH_VOLUME_RELATIVE:
	  result = nsp_move_string(stack,1, "volumerelative", -1);
	  result = ( result == FAIL) ? RET_BUG: 1;
	  break;
	}
      goto done;
    case FILE_SPLIT:  /* OK */
      { 
	NspSMatrix *S;
	int pargc;
	char **pargvList;
	if (rhs != 2) {
	  errorString = "split name";
	  goto not3Args;
	}
	  
	if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;
		
	nsp_split_path(fileName, &pargc, &pargvList);
	
	if ((S =nsp_smatrix_create_from_array(NVOID,pargc,(const char **)pargvList)) == NULLSMAT ) return RET_BUG;
	MoveObj(stack,1,(NspObject*) S);
	result = 1;
	goto done;
      }
    case FILE_JOIN: /* OK */
      {
	NspSMatrix *S;
	if (rhs != 2) {
	  errorString = "join,String Matrix";
	  goto not3Args;
	}
	if (( S = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
	nsp_join_path(S->mn,S->S, &buffer);
	result = nsp_move_string(stack,1, nsp_tcldstring_value(&buffer),  buffer.length);
	result = ( result == FAIL) ? RET_BUG: 1;
	goto done;
      }
    case FILE_RENAME: /* OK */
      {
	int force=1; /*** XXXXXX A gerer ***/
	NspSMatrix *S;
	if (rhs != 2) {
	  errorString = "rename,String Matrix";
	  goto not3Args;
	}
	if (( S = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
	result = nsp_file_rename_cmd(S->mn,S->S,force);
	if ( result == TCL_ERROR) result = RET_BUG;
	goto done;
      }
    case FILE_MKDIR: /* OK */
      {
	NspSMatrix *S;
	if (rhs != 2) {
	  errorString = "mkdir,String Matrix";
	  goto not3Args;
	}
	if (( S = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
	result = nsp_file_make_dirs_cmd(S->mn,S->S);
	if ( result == TCL_ERROR) result = RET_BUG;
	goto done;
      }
    case FILE_DELETE:  /* OK */
      {
	int force = 1;
	/** Rajouter le force XXXX **/
	NspSMatrix *S;
	if (rhs != 2) {
	  errorString = "delete,String Matrix";
	  goto not3Args;
	}
	if (( S = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
	result = nsp_file_delete_cmd( S->mn,S->S,force);
	if ( result == TCL_ERROR) result = RET_BUG;
	goto done;
      }
    case FILE_COPY: /* OK */
      {
	int force =1 ;
	/** Rajouter le force XXXX **/
	NspSMatrix *S;
	if (rhs != 2) {
	  errorString = "copy,String Matrix";
	  goto not3Args;
	}
	if (( S = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
	result = nsp_file_copy_cmd( S->mn,S->S,force);
	if ( result == TCL_ERROR) result = RET_BUG;
	goto done;
      }
    case FILE_NATIVENAME: /* OK */
      {
	char *fileName;
	if (rhs != 2) {
	  errorString = "nativename name";
	  goto not3Args;
	}
	if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;	
	fileName = nsp_translate_file_name(fileName,&buffer);
	if (fileName == NULL) {
	  result = RET_BUG ;
	} else {
	  result = nsp_move_string(stack,1 ,fileName, -1);
	  result = ( result == FAIL) ? RET_BUG: 1;
	}
	goto done;
      }
    }
    
  /*
   * Next, handle operations that can be satisfied with the "access"
   * kernel call.
   */
    
  if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;	
    
  switch (index) {
  case FILE_READABLE:/* OK */
    if (rhs != 2) {
      errorString = "readable,name";
      goto not3Args;
    }
    mode = R_OK;
  checkAccess:
    /*
     * The result might have been set within nsp_translate_file_name
     * (like no such user "blah" for file exists ~blah)
     * but we don't want to flag an error in that case.
     */
    if (fileName == NULL) 
      {
	result = nsp_move_boolean(stack,1,0);
	result = (result == FAIL) ? RET_BUG: 1;
      } 
    else 
      {
	result = nsp_move_boolean(stack,1, (access(fileName, mode) != -1));
	result = (result == FAIL) ? RET_BUG: 1;
      }
    goto done;
  case FILE_WRITABLE: /* OK */
    if (rhs != 2) {
      errorString = "writable name";
      goto not3Args;
    }
    mode = W_OK;
    goto checkAccess;
  case FILE_EXECUTABLE: /* OK */
    if (rhs != 2) {
      errorString = "executable name";
      goto not3Args;
    }
    mode = X_OK;
    goto checkAccess;
  case FILE_EXISTS: /* OK */
    if (rhs != 2) {
      errorString = "exists name";
      goto not3Args;
    }
    mode = F_OK;
    goto checkAccess;
  }
    
	
  /*
   * Lastly, check stuff that requires the file to be stat-ed.
   */
    
  if (fileName == NULL) {
    result = RET_BUG;
    goto done;
  }
    
  switch (index) 
    {
    case FILE_ATIME:/* OK */
      if (rhs != 2) {
	errorString = "atime name";
	goto not3Args;
      }
	
      if (stat(fileName, &statBuf) == -1) {
	goto badStat;
      }
      if (nsp_move_double(stack,1, (double) statBuf.st_atime) == FAIL) 
	result = RET_BUG;
      else 
	{
	  result= 1 ;
	}
      
      goto done;
    case FILE_ISDIRECTORY:/* OK */
      if (rhs != 2) {
	errorString = "isdirectory name";
	goto not3Args;
      }
      statOp = 2;
      break;
    case FILE_ISFILE:/* OK */
      if (rhs != 2) {
	errorString = "isfile name";
	goto not3Args;
      }
      statOp = 1;
      break;
    case FILE_LSTAT: /* OK */
      if (rhs != 2) {
	errorString = "lstat name";
	goto not3Args;
      }
    	    
      if (lstat(fileName, &statBuf) == -1) {
	Scierror("Error: couldn't lstat \"%s\": %s\n",fileName,
		 nsp_posix_error());
	result = RET_BUG;
	goto done;
      }
      /** REnvoyer stat XXXXX **/
      if (( S = StoreStatData(&statBuf)) == NULLSMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject*)S);
      result = 1;
      goto done;
    case FILE_MTIME: /* OK */
      if (rhs != 2) {
	errorString = "mtime name";
	goto not3Args;
      }
      if (stat(fileName, &statBuf) == -1) {
	goto badStat;
      }
      if ( nsp_move_double(stack,1, (double) statBuf.st_mtime)==FAIL)
	result=RET_BUG;
      else 
	result=1;
      goto done;
    case FILE_OWNED: /* OK */
      if (rhs != 2) {
	errorString = "owned name";
	goto not3Args;
      }	        	    
      statOp = 0;
      break;
    case FILE_READLINK: /* OK */
      {
	char linkValue[MAXPATHLEN + 1];
	int linkLength;
	
	if (rhs != 2) {
	  errorString = "readlink name";
	  goto not3Args;
	}
	/*
	 * If S_IFLNK isn't defined it means that the machine doesn't
	 * support symbolic links, so the file can't possibly be a
	 * symbolic link.  Generate an EINVAL error, which is what
	 * happens on machines that do support symbolic links when
	 * you invoke readlink on a file that isn't a symbolic link.
	 */

#ifndef S_IFLNK
	linkLength = -1;
	errno = EINVAL;
#else
#ifdef  WIN32 
	/* XXX this is to be done with TclWinResolveShortcut*/
	Scierror("Error: readlink on win32 not implemented \"%s\": %s\n",fileName,nsp_posix_error());
	result = RET_BUG;
	goto done;
#else 
	linkLength = readlink(fileName, linkValue, sizeof(linkValue) - 1);
#endif
#endif /* S_IFLNK */
	if (linkLength == -1) {
	  Scierror("Error: couldn't readlink \"%s\": %s\n",fileName,nsp_posix_error());
	  result = RET_BUG;
	  goto done;
	}
	linkValue[linkLength] = 0;
	result = nsp_move_string(stack,1, linkValue, linkLength);
	  result = ( result == FAIL) ? RET_BUG: 1;
	goto done;
      }
    case FILE_SIZE: /* OK */
      if (rhs != 2) {
	errorString = "size name";
	goto not3Args;
      }
      if (stat(fileName, &statBuf) == -1) {
	goto badStat;
      }
      if (nsp_move_double(stack,1, (double) statBuf.st_size)==FAIL) 
	result = RET_BUG;
      else
	result =1;
      goto done;
    case FILE_STAT:/* OK */
      if (rhs != 2) {
	errorString = "stat name";
	result = RET_BUG;
	goto done;
      }

      if (stat(fileName, &statBuf) == -1) {
      badStat:
	Scierror("Error: couldn't stat \"%s\": %s\n",fileName,nsp_posix_error());
	result = RET_BUG;
	goto done;
      }
      if ((S = StoreStatData(&statBuf)) == NULLSMAT ) return RET_BUG;
      MoveObj(stack,1,(NspObject*) S);
      result = 1;
      goto done;
    case FILE_TYPE: /* OK */
      if (rhs != 2) {
	errorString = "type name";
	goto not3Args;
      }
      if (lstat(fileName, &statBuf) == -1) {
	goto badStat;
      }
      errorString = GetTypeFromMode((int) statBuf.st_mode);
      result = nsp_move_string(stack,1, errorString, -1);
      result = ( result == FAIL) ? RET_BUG: 1;
      goto done;
    }

  if (stat(fileName, &statBuf) == -1) {
    result = nsp_move_boolean(stack,1, 0);
    result = (result == FAIL) ? RET_BUG: 1;
    goto done;
  }
  switch (statOp) {
  case 0:
    /*
     * For Windows and Macintosh, there are no user ids 
     * associated with a file, so we always return 1.
     */

#if (defined(__WIN32__) || defined(MAC_TCL))
    mode = 1;
#else
    mode = (geteuid() == statBuf.st_uid);
#endif
    break;
  case 1:
    mode = S_ISREG(statBuf.st_mode);
    break;
  case 2:
    mode = S_ISDIR(statBuf.st_mode);
    break;
  }
  result = nsp_move_boolean(stack,1, mode);
  result = (result == FAIL) ? RET_BUG: 1;
 done:
  nsp_tcldstring_free(&buffer);
  return result;

 not3Args:
  Scierror("Error: function %s, wrong number of argument for option (%s)\n",
	   NspFname(stack),errorString);
  result = RET_BUG;
  goto done;
}

/* scilab compatibility 
 *
 */ 


int int_basename(Stack stack,int rhs,int opt,int lhs) 
{
  char *fileName;
  nsp_string tail;
  CheckRhs(1,1);
  CheckLhs(0,1);
  /* FILE_TAIL */
  if ((fileName = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((tail = nsp_tail(fileName))==NULL) return RET_BUG; 
  if (  nsp_move_string(stack,1,tail, -1)==FAIL) return RET_BUG; 
  nsp_string_destroy(&tail);
  return 1;
}

int int_dirname(Stack stack,int rhs,int opt,int lhs) 
{
  char *fileName;
  nsp_string dirname;
  CheckRhs(1,1);
  CheckLhs(0,1);
  /* FILE_DIRNAME: */
  if ((fileName = GetString(stack,1)) == (char*)0)  return RET_BUG;
  if ((dirname = nsp_dirname (fileName))== NULL)return RET_BUG;
  if (nsp_move_string(stack,1, dirname, -1)== FAIL) return RET_BUG;
  nsp_string_destroy(&dirname);
  return 1;
}

/*
 *
 * StoreStatData --
 *
 *	This is a utility procedure that breaks out the fields of a
 *	"stat" structure and stores them in textual form into the
 *	elements of a string Matrix 
 *
 * Results:
 *	Returns a string Matrix 
 *
 */

static NspSMatrix *StoreStatData(struct stat *statPtr) 
     /* Pointer to buffer containing
      * stat data to store in varName. */
{
  char string[30];
  NspSMatrix *S;
  if (( S =nsp_smatrix_create(NVOID,11,2,"v",0)) ==  NULLSMAT ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_dev);
  if ( StoreStat(S,0,"dev",string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_ino);
  if ( StoreStat(S,1, "ino", string ) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_mode);
  if ( StoreStat(S,2, "mode", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_nlink);
  if ( StoreStat(S,3, "nlink", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_uid);
  if ( StoreStat(S,4, "uid", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_gid);
  if ( StoreStat(S,5, "gid", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%lu", (unsigned long) statPtr->st_size);
  if ( StoreStat(S,6, "size", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_atime);
  if ( StoreStat(S,7, "atime", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_mtime);
  if ( StoreStat(S,8, "mtime", string) == FAIL ) return NULLSMAT;
  sprintf(string, "%ld", (long) statPtr->st_ctime);
  if ( StoreStat(S,9, "ctime", string) == FAIL ) return NULLSMAT;
  if ( StoreStat(S,10, "type",
		 GetTypeFromMode((int) statPtr->st_mode)) == FAIL ) return NULLSMAT; 
  return S;
}

static int StoreStat( NspSMatrix *S, int i, char *str1,char *str2)
{
  char *loc ;
  if (( loc =new_nsp_string(str1)) == (nsp_string) 0)  return FAIL;
  nsp_string_destroy(&S->S[i]);
  S->S[i] = loc ;
  if (( loc =new_nsp_string(str2)) == (nsp_string) 0)  return FAIL;
  nsp_string_destroy(&S->S[i+11]);
  S->S[i+11] = loc ;
  return OK;
}


/*
 *
 * GetTypeFromMode --
 *
 *	Given a mode word, returns a string identifying the type of a
 *	file.
 *
 * Results:
 *	A static text string giving the file type from mode.
 *
 * Side effects:
 *	None.
 *
 */

static char *GetTypeFromMode(int mode)
{
  if (S_ISREG(mode)) {
    return "file";
  } else if (S_ISDIR(mode)) {
    return "directory";
  } else if (S_ISCHR(mode)) {
    return "characterSpecial";
  } else if (S_ISBLK(mode)) {
    return "blockSpecial";
  } else if (S_ISFIFO(mode)) {
    return "fifo";
#ifdef S_ISLNK
  } else if (S_ISLNK(mode)) {
    return "link";
#endif
#ifdef S_ISSOCK
  } else if (S_ISSOCK(mode)) {
    return "socket";
#endif
  }
  return "unknown";
}

/*
 * TclFileAttrsCmd --
 *
 * Called by the previous interface when first argument if attributes
 * file('attributes',<name>,....)
 */

static int TclFileAttrsCmd(Stack stack,int rhs,int opt,int lhs) 
{
  int result = RET_BUG;
  char *fileName;
  int  index;
  NspObject *elementObjPtr;
  nsp_tcldstring buffer;
  if ((rhs > 3 ) && ((rhs % 2) !=  0)) {
    Scierror("Error: wrong # args: must be file(attributes,name,[option,value,option,value])\n");
    return TCL_ERROR;
  }
  /** first argument is "attributes" second is file name **/
  if ( rhs < 2 ) 
    {
      Scierror("Error: must have at least 2 arguments \n");
      return TCL_ERROR;
    }
  if ((fileName = GetString(stack,2)) == (char*)0) return RET_BUG;
  nsp_tcldstring_init(&buffer);
  if (nsp_translate_file_name( fileName, &buffer) == NULL) 
    {
      goto done;
    }
  
  if (rhs  ==  2 ) 
    {
      /** Get the attributes and their values : we use a NspSMatrix **/
      NspSMatrix *S;
      int count;
      for (count = 0; tclpFileAttrStrings[count] != NULL; count++) {};
      if (( S =nsp_smatrix_create_with_length(NVOID,count,2,-1)) ==  NULLSMAT ) goto done;
      for (index = 0; tclpFileAttrStrings[index] != NULL; index++) 
	{
	  char *loc ;
	  if (( loc =new_nsp_string(tclpFileAttrStrings[index])) == (nsp_string) 0)  goto done;
	  S->S[index] = loc ;
	  if ((*tclpFileAttrProcs[index].getProc)( index, fileName,&elementObjPtr) != TCL_OK) {
	    goto done;
	  }
	  if (( loc =new_nsp_string(((NspSMatrix *) elementObjPtr)->S[0])) == (nsp_string) 0)  
	    goto done;
	  S->S[index+count] = loc ;
	}
      MoveObj(stack,1,(NspObject*) S);
      result = 1;
    } 
  else if (rhs == 3) 
    {
      /* get one specified attribute  */
      NspObject *O;
      if ((index = GetStringInArray(stack,3,tclpFileAttrStrings,0)) == -1 ) 
	goto done;
      if ((*tclpFileAttrProcs[index].getProc)( index, fileName, &O) != TCL_OK) {
	goto done;
      }
      MoveObj(stack,1,O);
      result = 1;
    } 
  else 
    {
      /* set attributes */
      int i;
      for (i = 3 ; i <= rhs  ; i += 2) {
	if ((index = GetStringInArray(stack,i,tclpFileAttrStrings,0)) == -1 ) 
	  goto done;
	if ((*tclpFileAttrProcs[index].setProc)( index, fileName,NthObj(i+1)) 
	    != TCL_OK) 
	  {
	    goto done ;
	  }
      }
      result = 0;
    }
  done :
    nsp_tcldstring_free(&buffer);
  return result ;
}


/*
 * InfoHostnameCmd --
 *
 *      Called to implement the "hostname" command that returns the
 *      host name. 
 */

int int_sys_hostname(Stack stack,int rhs,int opt,int lhs) 
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  /**  A revoir XXXXXX
       return nsp_move_string(stack,1, Tcl_GetHostName(), -1);
  */
  return nsp_move_string(stack,1, "foomachine", -1) == FAIL ? RET_BUG : 1;
}

/*
 * int_pwd 
 *	This procedure is invoked to process the "pwd" command.
 */

int int_pwd(Stack stack,int rhs,int opt,int lhs) 
{
  NspSMatrix *S;
  char *dirName;
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ((dirName = nsp_get_cwd() ) == NULL) return RET_BUG;
  if (( S =nsp_smatrix_create(NVOID,1,1,dirName,1) ) == NULLSMAT ) 
    return RET_BUG;
  NthObj(1)= (NspObject*) S;
  NSP_OBJECT(S)->ret_pos = 1;
  return 1;
}


/*
 *
 */

int int_get_current_exec_dir(Stack stack,int rhs,int opt,int lhs)
{
  NspSMatrix *S;
  int absolute=FALSE;
  nsp_option opts[] ={
    { "absolute",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  }; 
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  if ( get_optional_args(stack,rhs,opt,opts,&absolute) == FAIL) return RET_BUG;
  if ( absolute == FALSE) 
    S=nsp_smatrix_create(NVOID,1,1,stack.val->current_exec_dir,1);
  else
    {
      nsp_string str = nsp_absolute_file_name(stack.val->current_exec_dir);
      if ( str == NULL) return RET_BUG;
      if ((S =nsp_smatrix_create_with_length(NVOID,1,1,-1)) == NULLSMAT)
	return RET_BUG;
      S->S[0]= str;
    }
  if ( S == NULLSMAT ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(S));
  return 1;
}

