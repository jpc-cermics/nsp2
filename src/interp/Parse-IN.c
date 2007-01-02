/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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
 * 
 * a set of system functions which can be called from nsp interpreter 
 *------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "nsp/interf.h"
#include "LibsTab.h"
#include "../system/files.h"
#include "nsp/datas.h"
#include "nsp/parse.h"
#include "Eval.h"

static function  int_parseevalfile;
static function  int_add_lib;
static function  int_remove_lib;
static function  int_find_macro;
static function  int_execf;
static function  int_lasterror;
static function  int_error;

/*
 * exec(...)
 * Interface to use parse eval functions at Scilab level
 * interfaced as exec function 
 * [ok,H]=exec(...)
 */

void update_exec_dir(char *filename,char *exec_dir,char *filename_exec,unsigned int length);

static int int_parseevalfile(Stack stack, int rhs, int opt, int lhs)
{
#ifdef UPDATE_EXEC_DIR
  static char dir[FSIZE+1]={0};
  char old[FSIZE+1];
#endif
  char buf[FSIZE+1];
  NspObject *Ob;
  NspHash *H=NULL,*E=NULL;
  char *fname= NULL;
  int display=FALSE,echo =FALSE,errcatch=FALSE,rep,pausecatch=FALSE;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "display",s_bool,NULLOBJ,-1},
		      { "echo",s_bool,NULLOBJ,-1},
		      { "env", obj_check,NULLOBJ,-1},
		      { "errcatch",s_bool,NULLOBJ,-1},
		      { "pausecatch",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( rhs >=1 && IsNspPListObj(stack,1) ) 
    return int_execf(stack,rhs,opt,lhs);
  if ( GetArgs(stack,rhs,opt,T,&fname,&opts,&display,&echo,&nsp_type_hash,&E,&errcatch,&pausecatch) == FAIL) 
    return RET_BUG;
  nsp_path_expand(fname,buf,FSIZE);
#ifdef UPDATE_EXEC_DIR
  Sciprintf("Initial (%s,%s) ",dir,buf);
  strncpy(old,dir,FSIZE);
  update_exec_dir(fname,dir,buf,FSIZE);
  Sciprintf("Updated to %s and file = %s\n",dir,buf);
#endif
 
  if ( lhs == 2 ||  E != NULL )
    {
      if ( nsp_new_frame() == FAIL) return RET_BUG;
      if ( E != NULL) 
	{
	  if ( nsp_frame_insert_hash_contents(E) == FAIL) 
	    {
	      Scierror("Error: inserting values in environement failed\n");
	      nsp_frame_delete();
	      return RET_BUG; 
	    }
	}
      rep =nsp_parse_eval_file(buf,display,echo,errcatch,(pausecatch == TRUE) ? FALSE: TRUE);
      if ( rep < 0 ) 
	{
	  nsp_frame_delete();
	  if ( errcatch == FALSE ) return RET_BUG; 
	}
      if ( lhs == 2 )
	{
	  if ((H=nsp_current_frame_to_hash())==NULLHASH) 
	    {
	      nsp_frame_delete();
	      return RET_BUG;
	    }
	}
    }
  else 
    {
      rep =nsp_parse_eval_file(buf,display,echo,errcatch,(pausecatch == TRUE) ? FALSE: TRUE);
    }

#ifdef UPDATE_EXEC_DIR
  strncpy(dir,old,FSIZE);
#endif
  if ( rep < 0 && errcatch == FALSE ) return RET_BUG;
  if (( Ob =nsp_create_boolean_object(NVOID,(rep < 0) ? FALSE: TRUE)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,Ob);
  if ( lhs == 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(H));
    }
  return Max(1,lhs);
}

/*
 * execstr(...) 
 * we must copy the argument since it can be 
 * changed by execstr as in 
 * x=['x=90';'z=80']
 * execstr(x);
 * XXXX a revoir pour les pbs de recursions 
 * dans ParseEvalLoop
 */

static int int_execstr(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H=NULL,*E=NULL;
  NspObject *Ob;
  NspSMatrix *S= NULL;
  int display=FALSE,echo =FALSE,errcatch=FALSE,rep,pausecatch=FALSE;;
  int_types T[] = {smat,new_opts, t_end} ;
  nsp_option opts[] ={{ "display",s_bool,NULLOBJ,-1},
		      { "echo",s_bool,NULLOBJ,-1},
		      { "env", obj_check,NULLOBJ,-1},
		      { "errcatch",s_bool,NULLOBJ,-1},
		      { "pausecatch",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&S,&opts,&display,&echo,&nsp_type_hash,&E,&errcatch,&pausecatch) == FAIL) 
    return RET_BUG;

  if ( lhs == 2 ||  E != NULL )
    {
      /* evaluate the string in a new frame and returns the 
       * frame as an hash table 
       */
      if ( nsp_new_frame() == FAIL) return RET_BUG;
      if ( E != NULL) 
	{
	  if ( nsp_frame_insert_hash_contents(E) == FAIL) 
	    {
	      Scierror("Error: inserting values in environement failed\n");
	      nsp_frame_delete();
	      return RET_BUG; 
	    }
	}
      rep =nsp_parse_eval_from_smat(S,display,echo,errcatch,(pausecatch == TRUE) ? FALSE: TRUE );
      if ( rep < 0 ) 
	{
	  nsp_frame_delete();
	  if ( errcatch == FALSE ) return RET_BUG; 
	}
      if ( lhs == 2 )
	{
	  if ((H=nsp_current_frame_to_hash())==NULLHASH) 
	    {
	      nsp_frame_delete();
	      return RET_BUG;
	    }
	}
    }
  else 
    {
      rep =nsp_parse_eval_from_smat(S,display,echo,errcatch,(pausecatch == TRUE) ? FALSE: TRUE );
    }
  if ( rep < 0 && errcatch == FALSE ) return RET_BUG;
  if (( Ob =nsp_create_boolean_object(NVOID,(rep < 0) ? FALSE: TRUE)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,Ob);
  if ( lhs == 2 )
    {
      MoveObj(stack,2,NSP_OBJECT(H));
    }
  return Max(1,lhs);
}

/*
 * lasterror
 */

/* FIXME */

NspSMatrix *nsp_lasterror_get() ;
void nsp_lasterror_clear() ;

static int int_lasterror(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *rep = nsp_lasterror_get();
  NspObject *Ob;
  CheckRhs(0,0);
  CheckLhs(1,1);
  if ( rep != NULL ) 
    {
      if ( ( Ob = nsp_object_copy(NSP_OBJECT(rep)))== NULLOBJ) return RET_BUG;
      nsp_lasterror_clear();
    }
  else
    {
      if ( ( Ob = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0)) == NULLOBJ) return RET_BUG;
    }
  MoveObj(stack,1,Ob);
  return 1;
}

/*
 * execf(..)
 * execute a macros in the current frame 
 */

static int int_execf(Stack stack, int rhs, int opt, int lhs)
{
  int ans = OK;
  NspPList *PL;
#ifdef  WITH_SYMB_TABLE
  NspPList *PL1;
#endif 
  NspObject *Ob;
  int display=FALSE,echo =FALSE,errcatch=FALSE,rep;
  int_types T[] = {obj ,new_opts, t_end} ;
  nsp_option opts[] ={{ "display",s_bool,NULLOBJ,-1},
		      { "echo",s_bool,NULLOBJ,-1},
		      { "errcatch",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&PL,&opts,&display,&echo,&errcatch) == FAIL) return RET_BUG;
  if ( IsNspPList(NSP_OBJECT(PL)) == FALSE ) return RET_BUG;
  NthObj(rhs+1)=NthObj(1);
  NspFname(stack) = ((NspObject *) PL)->name;
  NspFileName(stack) = ((NspPList *) PL)->file_name;
#ifdef  WITH_SYMB_TABLE
  /* we cannot execute macro code in a context were its 
   * symbol table is not present 
   * thus we must copy the code and untag local variables
   */
  if ((PL1= NspPListCopy_no_local_vars(PL))== NULL) return RET_BUG;
  rep=nsp_eval_macro_body((NspObject *) PL1,stack,stack.first+rhs+1,0,0,0);
  NspPListDestroy(PL1);
#else
  rep=nsp_eval_macro_body((NspObject *) PL,stack,stack.first+rhs+1,0,0,0);
#endif 
  if ( rep == RET_CTRLC ) 
    {
      Scierror("Error:\tExecution of function %s interupted\n",NSP_OBJECT(PL)->name);
      ans= FAIL;
    }	  
  else if ( rep < 0 ) 
    {
      Scierror("Error:\tBug detected during evaluation of function  %s\n",
	       NSP_OBJECT(PL)->name);
      ans = FAIL;
    }
  if ( ans == FAIL && errcatch == FALSE )
    {
      nsp_error_message_show();
      return RET_BUG;
    }
  if ( errcatch == TRUE )  nsp_error_message_to_lasterror();
  if (( Ob =nsp_create_boolean_object(NVOID,(ans == FAIL) ? FALSE: TRUE)) == NULLOBJ )
    return RET_BUG;
  MoveObj(stack,1,Ob);
  return 1;
}


/* FIXME: juste here for testing 
 * Interface for library names specification 
 * add_lib('directory-name')
 */

static int int_add_lib(Stack stack, int rhs, int opt, int lhs)
{
  const char *dirname = NULL;
  int recursive=TRUE,compile=FALSE;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "compile",s_bool,NULLOBJ,-1},
		      { "recursive",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&dirname,&opts,&compile,&recursive) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  if ((dirname = GetString(stack,1)) == (char*)0) return RET_BUG;
  /* macros expansions is performed in EnterMacros */
  if (nsp_enter_macros(dirname,recursive,compile) == FAIL ) return RET_BUG;
  return 0;
}

/* FIXME: juste here for testing 
 * Interface for library names specification 
 * remove_lib('directory-name')
 */

static int int_remove_lib(Stack stack, int rhs, int opt, int lhs)
{
  char *Dir=0;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((Dir = GetString(stack,1)) == (char*)0) return RET_BUG;
  if (nsp_delete_macros(Dir) == FAIL ) return RET_BUG;
  return 0;
}

/* FIXME: juste here for testing 
 * Interface for library names specification 
 */

static int int_find_macro(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  double xrep;
  char *Name=0;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Name = GetString(stack,1)) == (char*)0) return RET_BUG;
  if ((O=nsp_find_macro(Name)) == NULLOBJ )
    xrep =0;
  else 
    xrep= 1;
  if (( O =nsp_create_object_from_double(NVOID,xrep)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
}

/* FIXME: juste here for testing 
 * 
 */

extern double nsp_timer(void);

static int int_find_macro_symbol(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int i;
  char *Name=0;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((Name = GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_timer();
  for (i=0; i < 1000000; i++)
    nsp_find_macro(Name);
  if (( O =nsp_create_object_from_double(NVOID,nsp_timer())) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
}

static int int_find_frames_symbol(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int i;
  char *Name=0;
  CheckRhs(1,1);
  CheckLhs(1,2);
  if ((Name = GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_timer();
  for (i=0; i < 1000000; i++)
    nsp_frames_search_object(Name);
  if (( O =nsp_create_object_from_double(NVOID,nsp_timer())) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
}

static int int_find_frame_symbol(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *O;
  int i;
  char *Name=0;
  CheckRhs(1,1);
  CheckLhs(1,1);
  if ((Name = GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_timer();
  for (i=0; i < 1000000; i++)
    nsp_frame_search_object(Name);
  if (( O =nsp_create_object_from_double(NVOID,nsp_timer())) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,O);
  return 1;
}


/* argn()
 * Interface for argn 
 * (Obsolete)
 */

static int int_argn(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,1);
  Sciprintf("Warning: argn is obsolete, as in matlab you can use nargin,nargout\n");
  if ( lhs >= 1 ) 
    {
      NspObject *Ob=nsp_frame_search_object("nargout");
      if ( Ob == NULLOBJ) 
	{
	  Scierror("nargin not found \n");
	  return RET_BUG;
	}
      MoveObj(stack,1,Ob);
    }
  if ( lhs >= 2 ) 
    {
      NspObject *Ob=nsp_frame_search_object("nargin");
      if ( Ob == NULLOBJ) 
	{
	  Scierror("nargin not found \n");
	  return RET_BUG;
	}
      MoveObj(stack,2,Ob);
    }
  return Max(lhs,0);
}

/* error(...)
 * Interface for error
 * FIXME: change str to string matrix (vector)
 */

static int int_error(Stack stack, int rhs, int opt, int lhs)
{
  char *str ;
  CheckRhs(0,1);
  if ( rhs == 1) 
    {
      if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
      Scierror("%s\n",str);
    }
  else 
    {
      Scierror("Error: error raised\n");
    }
  return RET_ERROR_RAISED;
}

extern function int_astnode_create;



/*
 * The Interface for basic parse operations 
 */

static OpTab Parse_func[]={
  {"parse_eval" , int_parseevalfile },
  {"exec" , int_parseevalfile },
  {"remove_lib",int_remove_lib},
  {"add_lib",int_add_lib},
  {"find_macro",int_find_macro},
  {"find_frame_symbol",int_find_frame_symbol},
  {"find_frames_symbol",int_find_frames_symbol},
  {"find_macro_symbol",int_find_macro_symbol},
  {"execstr",int_execstr},
  {"execf",int_execf},
  {"argn",int_argn},
  {"lasterror", int_lasterror},
  {"error", int_error},
  {"astnode_create", int_astnode_create},
  {(char *) 0, NULL}
};

int Parse_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Parse_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void Parse_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Parse_func[i].name;
  *f = Parse_func[i].fonc;
}






