/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 * interface for system  functions.
 * the tcl subdirectory was made from tcl code 
 * to get a portable set of system functions 
 * This code is unfinished. 
 *--------------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "nsp/interf.h"
#include "nsp/system.h"
#include "nsp/nsptcl.h"
#include "regexp.h"
#include "files.h" /* FSIZE */

/* define in spawn  */

extern function int_spawn_create;
extern function int_g_spawn_sync;
extern function int_g_spawn_async;

/* defined in the tcl subdir 
 */

extern function int_syscd;
extern function int_sysfile;
extern function int_dirname;
extern function int_basename;
extern function int_pwd;
extern function int_glob;
extern function int_get_current_exec_dir;

/*
 * Interface for timer 
 */

static int int_timer(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( (OM=nsp_create_object_from_double(NVOID,nsp_timer())) == NULLOBJ) 
    return RET_BUG;
  MoveObj(stack,1,OM);
  return 1;
}

/*
 * Interface for cputime
 */

static int int_cputime(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ( (OM=nsp_create_object_from_double(NVOID,nsp_cputime())) == NULLOBJ) 
    return RET_BUG;
  MoveObj(stack,1,OM);
  return 1;
}


/*
 * Interface for tic toc 
 */

static int int_tic(Stack stack,int rhs,int opt,int lhs) 
{
  CheckRhs(0,0);
  CheckLhs(0,0);
  nsp_tictoc(NULL);
  return 0;
}

static int int_toc(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  double etime;
  CheckRhs(0,0);
  CheckLhs(0,1);

  if ( nsp_tictoc(&etime) == FAIL )
    {
      Scierror("Error: tic() must be called before toc()\n");
      return RET_BUG;
    }

  if ( (OM=nsp_create_object_from_double(NVOID,etime)) == NULLOBJ ) 
    return RET_BUG;
  MoveObj(stack,1,OM);
  return 1;
}

/*
 * Interface for system(str)
 */

static int int_system(Stack stack,int rhs,int opt,int lhs) 
{
  NspObject *OM;
  double rep;
  char *command;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((command = GetString(stack,1)) == (char*)0) return RET_BUG;
  rep = (double) system(command);
  if ( (OM=nsp_create_object_from_double(NVOID,rep)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1, OM);
  return 1;
}

/*
 * Interface for getenv(A [,def ] ) 
 */

static int int_getenv(Stack stack,int rhs,int opt,int lhs) 
{
  NspSMatrix *S;
  char *envname,*env,*def;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ( rhs == 2 ) 
    if ((def = GetString(stack,2)) == (char*)0) return RET_BUG;
  env = nsp_getenv(envname);
  if ( env != NULL ) 
    {
      if ((S=nsp_smatrix_create(NVOID,1,1,env,(integer)1)) == NULLSMAT ) 
	return RET_BUG;
      MoveObj(stack,1,(NspObject*) S);
      return 1;
    }
  else 
    {
      if ( rhs == 2) 
	{
	  NthObj(2)->ret_pos = 1;
	  return 1;
	}
      else
	{
	  Scierror("Error: environment variable %s is not defined\r\n",envname);
	  return RET_BUG;
	}
    }
}

/*
 * Interface for setenv(A,value)
 */

static int int_setenv(Stack stack,int rhs,int opt,int lhs) 
{
  char *envname,*val;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((val = GetString(stack,2)) == (char*)0) return RET_BUG;
  nsp_setenv(envname, val);
  nsp_tclplatform_init() ; 
  /* XXXXXX setenv does not exists on all Ops 
  if ( setenv(envname,val,1) == -1 ) 
    {
      Scierror("Error: setenv failed, there was insufficient space in the environment\n");
      return RET_BUG;
    }
  */
  return 0;
}

/*
 * Interface for unsetenv(A)
 */

static int int_unsetenv(Stack stack,int rhs,int opt,int lhs) 
{
  char *envname;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((envname = GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_unsetenv(envname);
  return 0;
}

/*
 *
 */

int int_realtime_init(Stack stack, int rhs, int opt, int lhs) 
{
 double zer=0.0,rtv;
 CheckRhs(1,1);
 CheckLhs(1,1);
 /*  checking variable scale */
 if (GetScalarDouble(stack,1,&rtv) == FAIL) return RET_BUG;
 /* cross variable size checking */
 nsp_realtime_init(&zer,&rtv);
 return 0;
}

/*
 *
 */
 
int int_realtime(Stack stack, int rhs, int opt, int lhs) 
{
  double rtv;
  CheckRhs(1,1);
  CheckLhs(1,1);
  /*  checking variable t */
  if (GetScalarDouble(stack,1,&rtv) == FAIL) return RET_BUG;
  /* cross variable size checking */
  nsp_realtime(&rtv);
  return 0;
}               



/*
 * int_regsub 
 */

static int int_regsub(Stack stack,int rhs,int opt,int lhs) 
{
  int nmatch,i,code=RET_BUG;
  NspSMatrix *S,*Res=NULLSMAT;
  NspMatrix *M=NULLMAT;
  int noCase = FALSE, all = TRUE;
  char *pattern, *subSpec, *p;

  Tcl_RegExp regExpr;
  nsp_tcldstring  resultDString;
  CheckRhs(3,5);
  CheckLhs(1,2);

  nsp_option opts[] ={
    { "all",s_bool,NULLOBJ,-1},
    { "nocase",s_bool,NULLOBJ,-1},
    { NULL,t_end,NULLOBJ,-1}
  }; 
  CheckStdRhs(3,3);
  CheckLhs(1,2);

  if ((S = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((pattern = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ((subSpec  = GetString(stack,3)) == (char*)0) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&all,&noCase) == FAIL) return RET_BUG;

  /*
   * Convert the string and pattern to lower case, if desired, and
   * perform the matching operation.
   */

  if ( noCase == TRUE)
    {
      /* Copy S and pattern: 
       * Convert the strings and pattern to lower case before 
       * performing the matching operation.
       */
      if ((S = nsp_smatrix_copy(S)) == NULLSMAT) goto done;
      if ((pattern =nsp_string_copy(pattern)) == NULLSTRING) goto done;
      for (p = pattern; *p != 0; p++) {
	if (isupper(UCHAR(*p))) {
	  *p = (char)tolower(UCHAR(*p));
	}
      }
      for ( i = 0 ; i < S->mn; i++) 
	for (p = S->S[i]; *p != 0; p++) {
	  if (isupper(UCHAR(*p))) {
	    *p = (char)tolower(UCHAR(*p));
	  }
	}
    }
  /* prepare regexp */
  if (( regExpr = nsp_tclregexp_compile( pattern)) == NULL) goto done;

  if (( Res =nsp_smatrix_create_with_length(NVOID,S->m,S->n,-1)) == NULLSMAT )
    goto done;
  if ( lhs == 2) 
    {
      if (( M =nsp_matrix_create(NVOID,'r',S->m,S->n)) == NULLMAT )
	goto done;
    }
  
  for ( i = 0 ; i < S->mn ; i++) 
    {
      if ((nsp_tcl_regsub(S->S[i],regExpr,subSpec,&resultDString,&nmatch,all))==FAIL)
	goto done;
      if (( Res->S[i] = nsp_string_copy(nsp_tcldstring_value(&resultDString))) == (nsp_string) 0)
	{
	  nsp_tcldstring_free(&resultDString);	  
	  goto done;
	}
      if ( lhs == 2) M->R[i] = (double) nmatch;
    }

  MoveObj(stack,1,NSP_OBJECT(Res));
  if ( lhs == 2 )   MoveObj(stack,2,NSP_OBJECT(M));
  code = Max(lhs,1);
 done:
  if ( noCase == TRUE)
    {
      /* copies were done we have to free */
      nsp_smatrix_destroy(S);
      nsp_string_destroy(&pattern);
    }
  return code;
}

/*
 * int_regexp 
 *
 */

static int int_regexp(Stack stack,int rhs,int opt,int lhs) 
{
  int noCase = FALSE;
  Tcl_RegExp regExpr;
  char  *string, *pattern, *start, *end,*p;
  int match = 0;			/* Initialization needed only to
					 * prevent compiler warning. */
  int i;
  nsp_option opts[] ={{ "nocase",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(2,2);
  CheckLhs(1,2);

  if ((string  = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((pattern = GetString(stack,2)) == (char*)0) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&noCase) == FAIL) return RET_BUG;
  if ( noCase == TRUE)
    {
      if ((string =nsp_string_copy(pattern)) == NULLSTRING)  return  RET_BUG;
      if ((pattern =nsp_string_copy(pattern)) == NULLSTRING)  return  RET_BUG;
      /*
       * Convert the string and pattern to lower case, if desired, and
       * perform the matching operation.
       */
      for (p = pattern; *p != 0; p++) {
	if (isupper(UCHAR(*p))) {
	  *p = (char)tolower(UCHAR(*p));
	}
      }
      for (p = string; *p != 0; p++) {
	if (isupper(UCHAR(*p))) {
	  *p = (char)tolower(UCHAR(*p));
	}
      }
    }

  if ((regExpr = nsp_tclregexp_compile( pattern))==  NULL) return RET_BUG;
  if ((match = nsp_tclregexp_exec(regExpr, string, string)) < 0) 
    return RET_BUG;
  
  if ( nsp_move_boolean(stack,1,match) == FAIL ) return RET_BUG;
  
  if (lhs == 2)
    {
      NspObject *OM;
      if ( match == 0) 
	{
	  if ( (OM=nsp_create_empty_matrix_object(NVOID)) == NULLOBJ) return RET_BUG;   
	  MoveObj(stack,2,OM);
	}
      else 
	{
	  NspMatrix *A;
	  int count=0; 
	  /* Note that only the first occurence will be returned 
	   *
	   */
	  for (i = 0; i < NSUBEXP ; i++) 
	    {
	      nsp_tclregexp_range(regExpr, i, &start, &end);
	      if ( start != NULL) count++;
	    }
	  if ((A= nsp_matrix_create(NVOID,'r',count,2))== NULLMAT) 
	    return RET_BUG;   
	  count=0;
	  for (i = 0; i < NSUBEXP ; i++) 
	    {
	      nsp_tclregexp_range(regExpr, i, &start, &end);
	      if ( start != NULL) 
		{
		  A->R[count]= (double)(start - string +1);
		  A->R[count+A->m]= (double)(end - string);
		  count++;
		}
	      /* fprintf(stderr,"Range : %d %d \n",(int)(start - string),
		 (int)(end - string - 1)); */
	    }
	  MoveObj(stack,2,NSP_OBJECT(A));
	}
    }
  return Max(lhs,1);
}

/* edit the contents of a file 
 *
 */

extern void nsp_edit(char *filename,int read_only);

static int int_editfile(Stack stack, int rhs, int opt, int lhs)
{
  char Fname_expanded[FSIZE+1];
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "read_only",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  char *Fname;
  int read_only = FALSE;
  if ( GetArgs(stack,rhs,opt,T,&Fname,&opts,&read_only) == FAIL) return RET_BUG;
  /* expand keys in path name result in buf */
  nsp_expand_file_with_exec_dir(&stack,Fname,Fname_expanded);
  nsp_edit(Fname_expanded,read_only);
  return 0;
}

#if 0 
static int int_mktemp(Stack stack, int rhs, int opt, int lhs)
{
  char *name, Fname_expanded[FSIZE+1];
  int_types T[] = {string, t_end} ;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
  /* expand keys in path name result in buf */
  nsp_expand_file_with_exec_dir(&stack,name,Fname_expanded);
  name= mktemp(Fname_expanded);
  if ( name == NULL || name[0]=='\0')
    {
      Scierror("Error: in %s, could not generate a unique name\n",NspFname(stack));
    }
  if ( nsp_move_string(stack,1,name,-1) == FAIL) return RET_BUG; 
  return 1;
},
#endif 



/*
 * Interface for localtime
 * returns a Hash whose keys are given by TM_NAME. 
 * using C localtime function
 * J�r�me Lelong. 
 */

static int int_localtime(Stack stack,int rhs,int opt,int lhs)
{
  int i;
  int *Timeptr;
  time_t now;
  NspHash *H;
  const char *TM_NAME[] = {"sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "isdst"};
  now = time (NULL);

  CheckRhs(0,0);
  CheckLhs(1,1);

  Timeptr = (int *) localtime (&now);
  if(( H = nsp_hash_create(NVOID,4)) == NULLHASH) return RET_BUG;
  for ( i = 0 ; i < 9 ; i++ )
    {
      NspObject *obj;
      if ((obj= nsp_create_object_from_double(TM_NAME[i],Timeptr[i])) == NULL) goto err;
      if (nsp_hash_enter(H,obj ) == FAIL) goto err;
    }
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
 err:
  nsp_hash_destroy(H);
  return RET_BUG;
}



/*
 * The Interface for system functions 
 */ 

#ifdef WIN32 
extern function  int_nsp_query_registry;
#else 

int int_nsp_query_registry(Stack stack,int rhs,int opt,int lhs)
{
  Scierror("Error: %s is only implemented on windows platforms\n",NspFname(stack));
  return RET_BUG;
}
#endif 

static OpTab System_func[]={
  {"registry", int_nsp_query_registry },
  {"localtime", int_localtime},
  {"chdir", int_syscd},
  {"getcwd", int_pwd},
  {"file", int_sysfile},
  {"basename", int_basename}, /* scilab compat */
  {"dirname", int_dirname},   /* scilab compat */
  {"regexp", int_regexp},
  {"regsub", int_regsub},
  {"glob",int_glob},
  {"getenv",int_getenv},
  {"setenv",int_setenv},
  {"unsetenv",int_unsetenv},
  {"timer", int_timer},
  {"cputime", int_cputime},
  {"tic", int_tic},
  {"toc", int_toc},
  {"system",int_system},
  {"realtime",int_realtime},
  {"realtimeinit",int_realtime_init},
  {"get_current_exec_dir", int_get_current_exec_dir},
  {"spawn", int_spawn_create},
  {"spawn_sync", int_g_spawn_sync},
  {"spawn_async", int_g_spawn_async},
  {"editfile", int_editfile},
#if 0 
  {"mktemp", int_mktemp},
#endif 

  {(char *) 0, NULL}
};

int System_Interf(int i,Stack stack,int rhs,int opt,int lhs) 
{
  return (*(System_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 * (for adding or removing functions) 
 */

void System_Interf_Info(int i, char **fname, function **f)
{
  *fname = System_func[i].name;
  *f = System_func[i].fonc;
}





