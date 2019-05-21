/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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

#include <nsp/nsp.h> 
#include <nsp/objects.h> 
#include <nsp/interf.h>
#include <nsp/system.h>
#include <nsp/plist.h> 
#include <nsp/datas.h>
#include <nsp/libstab.h>
#include <nsp/eval.h>
#include <nsp/seval.h>
#include <nsp/parse.h> 
#include <nsp/nsptcl.h> 
#include <nsp/ast.h> 
#include <nsp/astv.h> 

static function  int_parseevalfile;
static function  int_add_lib;
static function  int_remove_lib;
static function  int_find_macro;
static function  int_execf;
static function  int_lasterror;
static function  int_error;

/**
 * nsp_expand_file_and_update_exec_dir:
 * @stack: 
 * @old:  a pointer to a string of size %FSIZE+1 used to store current exec directory 
 * @filename: filename to be used to change the default exec directory 
 * @filename_exec: expanded version of filename using current exec directory 
 * 
 * expand @filename in @filename_exec using macros and current exec directory. 
 * The current exec directory is also updated using the dirname of the expanded 
 * filename.
 * 
 **/

void nsp_expand_file_and_update_exec_dir(Stack *stack,char *old,char *filename,char *filename_exec)
{
#ifdef UPDATE_EXEC_DIR
  char buf1[FSIZE+1];
  /* expand filename in buf1 */
  nsp_path_expand(filename,buf1,FSIZE);
  /* protect current_exec_dir in old */
  strncpy(old,stack->val->current_exec_dir,FSIZE);
  /* update current_exec_dir */
  nsp_update_exec_dir(buf1,stack->val->current_exec_dir,filename_exec,FSIZE);
  /* Sciprintf("exec(%s)->[%s] dir=%s\n",filename,filename_exec,stack->val->current_exec_dir); */
#else 
  nsp_path_expand(filename,filename_exec,FSIZE);
#endif
}
 
/**
 * nsp_expand_dir_and_update_exec_dir:
 * @stack: #Stack object 
 * @old:  a pointer to a string of size %FSIZE+1 used to store current exec directory 
 * @dirname: filename to be used to change the default exec directory 
 * @dirname_exec: expanded version of filename using current exec directory 
 * 
 * expand @dirname in @dirname_exec using macros and current exec directory. 
 * The current exec directory is also updated using the expanded dirname.
 * 
 **/

void nsp_expand_dir_and_update_exec_dir(Stack *stack,char *old,char *dirname,char *dirname_exec)
{
#ifdef UPDATE_EXEC_DIR
  char buf1[FSIZE+1];
  /* expand dirname in buf1 */
  nsp_path_expand(dirname,buf1,FSIZE);
  /* protect current_exec_dir in old */
  strncpy(old,stack->val->current_exec_dir,FSIZE);
  /* update current_exec_dir */
  nsp_update_exec_dir_from_dir(buf1,stack->val->current_exec_dir,FSIZE);
  strncpy(dirname_exec,stack->val->current_exec_dir,FSIZE);
#else 
  nsp_path_expand(dirname,dirname_exec,FSIZE);
#endif
}
 

/**
 * nsp_expand_file_with_exec_dir:
 * @stack: 
 * @filename: filename to be used to change the default exec directory 
 * @filename_exec: expanded version of filename using current exec directory 
 * 
 * expand @filename in @filename_exec using macros and current exec directory.
 *
 **/

void nsp_expand_file_with_exec_dir(Stack *stack,char *filename,char *filename_exec)
{
#ifdef UPDATE_EXEC_DIR
  char buf1[FSIZE+1], old[FSIZE+1];
  /* expand filename in buf1 */
  nsp_path_expand(filename,buf1,FSIZE);
  strncpy(old,stack->val->current_exec_dir,FSIZE);
  /* update current_exec_dir */
  nsp_update_exec_dir(buf1,old,filename_exec,FSIZE);
  /* Sciprintf("exec(%s)->[%s] dir=%s\n",filename,filename_exec,stack->val->current_exec_dir); */
#else 
  nsp_path_expand(filename,filename_exec,FSIZE);
#endif
}

/**
 * nsp_reset_exec_dir:
 * @stack: 
 * @old: a pointer to a string of size %FSIZE+1 containing a directory name.
 * 
 * reset the default exec directory using @old a pointer to a string of size %FSIZE+1.
 **/

void nsp_reset_exec_dir(Stack *stack,char *old)
{
#ifdef UPDATE_EXEC_DIR
  strncpy(stack->val->current_exec_dir,old,FSIZE+1);
#endif
}

/*
 * exec(...)
 * Interface to use parse eval functions at Scilab level
 * interfaced as exec function 
 * [ok,H]=exec(...)
 */

static int int_parseevalfile_gen(Stack stack, int rhs, int opt, int lhs,int mtlb)
{
  char old[FSIZE+1], fname_expanded[FSIZE+1];
  NspObject *Ob;
  NspHash *H=NULL,*E=NULL;
  char *fname= NULL;
  int errcatch_all, pausecatch_all;
  int display=FALSE,echo =FALSE,errcatch=FALSE,rep,pausecatch=FALSE,ret=RET_BUG;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "display",s_bool,NULLOBJ,-1},
		      { "echo",s_bool,NULLOBJ,-1},
		      { "env", obj_check,NULLOBJ,-1},
		      { "errcatch",s_bool,NULLOBJ,-1},
		      { "pausecatch",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( rhs >=1 && IsNspPListObj(stack,1) )  return int_execf(stack,rhs,opt,lhs);
  CheckLhs(0,2);
  if ( GetArgs(stack,rhs,opt,T,&fname,&opts,&display,&echo,&nsp_type_hash,&E,&errcatch,&pausecatch) == FAIL) 
    return RET_BUG;

  /* update the current exec dir in stack, old value is returned in old 
   * fname_expanded contains the file name after expansion using current_exec_dir
   */
  nsp_expand_file_and_update_exec_dir(&stack,old,fname,fname_expanded);

  /* take care that this execstr can be enclosed in a errcatch protected evaluation */
  errcatch_all =( stack.val->errcatch == TRUE ) ? TRUE :  errcatch;
  pausecatch_all = ( stack.val->pause == FALSE ) ? TRUE: pausecatch;

  if ( lhs == 2 ||  E != NULL )
    {
      /* evaluate the string in a new frame and returns the 
       * frame as a hash table : take care that frame must be deleted 
       * at the end. 
       */
      if ( nsp_new_frame("datas") == FAIL) goto err;
      /* insert the contente of E in new frame */
      if ( E != NULL) 
	{
	  if ( nsp_frame_insert_hash_contents(E) == FAIL) 
	    {
	      Scierror("Error: inserting values in environement failed\n");
	      nsp_frame_delete();
	      goto err;
	    }
	}
      rep =nsp_parse_eval_file(fname_expanded,display,echo,errcatch_all,
			       (pausecatch_all == TRUE) ? FALSE: TRUE,mtlb);
      if ( rep >= 0 && lhs == 2 ) H=nsp_current_frame_to_hash(); 
      nsp_frame_delete();
    }
  else 
    {
      rep =nsp_parse_eval_file(fname_expanded,display,echo,errcatch_all,
			       (pausecatch_all == TRUE) ? FALSE: TRUE,mtlb);
    }
  if ( rep < 0 )
    {
      if ( errcatch == FALSE ) goto err;
      if ( lhs == 2 ) H = nsp_hcreate(NVOID,1);
    }
  if (( Ob =nsp_create_boolean_object(NVOID,(rep < 0) ? FALSE: TRUE)) == NULLOBJ ) goto err;
  MoveObj(stack,1,Ob);
  if ( lhs == 2) 
    {
      if ( H == NULLHASH ) goto err;
      MoveObj(stack,2,NSP_OBJECT(H));
    }
  ret = Max(1,lhs);
 err: 
  nsp_reset_exec_dir(&stack,old);
  return ret;
}


static int int_parseevalfile(Stack stack, int rhs, int opt, int lhs)
{
  return int_parseevalfile_gen(stack,rhs,opt,lhs,FALSE);
}

/* use a tokenize for which % is used for comments 
 * so as to parse most of matlab syntax
 * 
 */

static int int_parseevalfile_mtlb(Stack stack, int rhs, int opt, int lhs)
{
  return int_parseevalfile_gen(stack,rhs,opt,lhs,TRUE);
}


/*
 * interface for execstr.
 */

static int int_execstr(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H=NULL,*E=NULL;
  NspObject *Ob;
  NspSMatrix *S= NULL;
  int display=FALSE,echo =FALSE,errcatch=FALSE,rep,pausecatch=FALSE;;
  int errcatch_all, pausecatch_all;
  nsp_option opts[] ={{ "display",s_bool,NULLOBJ,-1},
		      { "echo",s_bool,NULLOBJ,-1},
		      { "env", obj_check,NULLOBJ,-1},
		      { "errcatch",s_bool,NULLOBJ,-1},
		      { "pausecatch",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  /*
   * we must take care to copy the argument here (i.e the 
   * GetSMatCopy is not enough) in order 
   * to remove a named variable from the stack, just in 
   * case the variable value is changed by the parse eval 
   * below as for example in  x=['x=90';'z=80']; execstr(x);
   */
  CheckStdRhs(1,1);
  CheckLhs(0,2);
  if ((S=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&display,&echo,&nsp_type_hash,&E,&errcatch,&pausecatch) == FAIL) 
    return RET_BUG;
  if ((S = nsp_smatrix_copy(S)) == NULLSMAT ) return RET_BUG ;

  /* take care that this execstr can be enclosed in a errcatch protected evaluation */
  errcatch_all =( stack.val->errcatch == TRUE ) ? TRUE :  errcatch;
  pausecatch_all = ( stack.val->pause == FALSE ) ? TRUE: pausecatch;

  if ( lhs == 2 ||  E != NULL )
    {
      /* evaluate the string in a new frame and returns the 
       * frame as a hash table : take care that frame must be deleted 
       * at the end. 
       */
      if ( nsp_new_frame("datas") == FAIL) goto fail;
      /* insert the contente of E in new frame */
      if ( E != NULL) 
	{
	  if ( nsp_frame_insert_hash_contents(E) == FAIL) 
	    {
	      Scierror("Error: inserting values in environement failed\n");
	      nsp_frame_delete();
	      goto fail;
	    }
	}
      rep =nsp_parse_eval_from_smat(S,display,echo,errcatch_all,(pausecatch_all == TRUE) ? FALSE: TRUE );
      if ( rep >= 0 && lhs == 2 ) H=nsp_current_frame_to_hash(); 
      nsp_frame_delete();
    }
  else 
    {
      rep =nsp_parse_eval_from_smat(S,display,echo,errcatch_all,(pausecatch_all == TRUE) ? FALSE: TRUE );
    }
  if ( rep < 0 )
    {
      if ( errcatch == FALSE ) goto fail;
      if ( lhs == 2 ) H = nsp_hcreate(NVOID,1);
    }
  if (( Ob =nsp_create_boolean_object(NVOID,(rep < 0) ? FALSE: TRUE)) == NULLOBJ ) goto fail;
  MoveObj(stack,1,Ob);
  if ( lhs == 2) 
    {
      if ( H == NULLHASH ) goto fail;
      MoveObj(stack,2,NSP_OBJECT(H));
    }
  nsp_smatrix_destroy(S);
  return Max(1,lhs);
 fail:
  nsp_smatrix_destroy(S);
  return RET_BUG;
}


/*
 * input('message',gui=%f,eval=%t,env=..)
 */

static int _int_input(Stack stack,char *prompt,int eval,NspHash *E, int accept_empty, int retval)
{
  NspObject *Res;
  Tokenizer To;
  char buf[1024];
  int buf_size=1024, len_line, eof;
  int display=FALSE,echo =FALSE,errcatch=FALSE,rep,pausecatch=FALSE;;
 again:
  nsp_tokenizer_init(&To);
  To.token_readline(&To,prompt,buf, &buf_size, &len_line, &eof);
  if ( accept_empty == TRUE && buf[0]=='\0') 
    {
      if ( retval == TRUE ) 
	{
	  nsp_move_string(stack,1,"",-1);
	  return 1;
	}
      else 
	return 0;
    }
  if ( eval == FALSE ) 
    {
      if ( retval == TRUE ) 
	{
	  nsp_move_string(stack,1,buf,-1);
	  return 1;
	}
      else 
	return 0;
    }
  /* evaluate the string in a new frame and returns the 
   * frame as a hash table : take care that frame must be deleted 
   * at the end. 
   */
  if ( nsp_new_frame("datas") == FAIL) return RET_BUG;
  /* insert the contents of E in new frame */
  if ( E != NULL) 
    {
      if ( nsp_frame_insert_hash_contents(E) == FAIL) 
	{
	  Scierror("Error: inserting values in environement failed\n");
	  nsp_frame_delete();
	  return RET_BUG; 
	}
    }
  rep =nsp_parse_eval_from_string(buf,display,echo,errcatch,(pausecatch == TRUE) ? FALSE: TRUE );
  if ( rep >= 0 ) 
    {
      /* get a copy of ans in the current frame and return it */
      if ((Res = nsp_frame_search_object("ans"))== NULLOBJ) 
	{
	  Sciprintf("Error: evaluation of expression '%s' should give a value to ans\n",buf);
	  rep = -1;
	}
      else 
	{
	  if ((Res = nsp_object_copy(Res))== NULLOBJ) 
	    {
	      nsp_frame_delete();
	      return RET_BUG;
	    }
	}
    }
  nsp_frame_delete();
  if ( rep < 0 ) goto again;
  
  if ( retval == TRUE ) 
    {
      MoveObj(stack,1,NSP_OBJECT(Res));
      return 1;
    }
  return 0;
}


static int int_input(Stack stack, int rhs, int opt, int lhs)
{
  char *prompt;
  NspHash *E=NULL;
  int accept_empty = FALSE, eval =FALSE,rep ;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "eval",s_bool,NULLOBJ,-1},
		      { "env", obj_check,NULLOBJ,-1},
		      { "accept_empty", s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&prompt,&opts,&eval,&nsp_type_hash,&E,&accept_empty) == FAIL) 
    return RET_BUG;
  rep = _int_input(stack,prompt,eval,E,accept_empty,TRUE);
  if ( rep == RET_BUG) return rep;
  return Max(rep,lhs);
}

static int int_halt(Stack stack, int rhs, int opt, int lhs)
{
  int rep ;
  CheckLhs(0,1);
  CheckRhs(0,0);
  rep = _int_input(stack,"Enter any key to continue:",FALSE,NULL,TRUE,FALSE);
  if ( rep == RET_BUG) return rep;
  return Max(rep,lhs);
}


/*
 * interface for lasterror.
 * This function returns in a string matrix all pending 
 * error messages and clears the pending messages.
 * Thus, typically this function should be always called in 
 * ok = execstr(..., errcatch=%t) 
 * if ~ok then 
 *   // always clear the error message by calling lasterror() 
 *   // if needed use the returned matrix to display an error 
 *   x_message('Error:\n\n'+catenate(lasterror()));
 * end 
 */

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

static int nsp_execf(Stack stack,int rhs, int opt, int lhs, 
		     NspPList *PL, int display, int echo,int errcatch, int pause)
{
  int rep;
#ifdef  WITH_SYMB_TABLE
  NspPList *PL1;
#endif 
  int cur_echo= nsp_set_echo_input_line(echo);
  int errcatch_cur = stack.val->errcatch;
  int pause_cur = stack.val->pause;

  NspFname(stack) = ((NspObject *) PL)->name;
  NspFileName(stack) = ((NspPList *) PL)->file_name;

  /* must be reset at the end */
  stack.val->errcatch = errcatch; 
  stack.val->pause = pause; 
#ifdef  WITH_SYMB_TABLE
  /* we cannot execute macro code in a context were its 
   * symbol table is not present 
   * thus we must copy the code and untag local variables
   */
  if ((PL1= NspPListCopy_no_local_vars(PL))== NULL) return RET_BUG;
  rep=nsp_eval_macro_body((NspObject *) PL1,stack,stack.first+rhs+1,0,0,0,display);
  NspPListDestroy(PL1);
#else
  rep=nsp_eval_macro_body((NspObject *) PL,stack,stack.first+rhs+1,0,0,0,display);
#endif 
  nsp_set_echo_input_line(cur_echo);
  stack.val->errcatch= errcatch_cur; 
  stack.val->pause= pause_cur;
  return rep;
}

static int int_execf(Stack stack, int rhs, int opt, int lhs)
{
  NspHash *H=NULL,*E=NULL;
  NspPList *PL;
  NspObject *Ob;
  int errcatch_all, pausecatch_all;
  int display=FALSE,echo =FALSE,errcatch=FALSE,pausecatch=FALSE,rep, rep1, ans = OK ;
  int_types T[] = {obj ,new_opts, t_end} ;
  nsp_option opts[] ={{ "display",s_bool,NULLOBJ,-1},
		      { "echo",s_bool,NULLOBJ,-1},
		      { "env", obj_check,NULLOBJ,-1},
		      { "errcatch",s_bool,NULLOBJ,-1},
		      { "pausecatch",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,2);
  if ( GetArgs(stack,rhs,opt,T,&PL,&opts,&display,&echo,&nsp_type_hash,&E,
	       &errcatch,&pausecatch) == FAIL) return RET_BUG;
  if ( IsNspPList(NSP_OBJECT(PL)) == FALSE ) return RET_BUG;
  NthObj(rhs+1)=NthObj(1);
  
  /* take care that this execstr can be enclosed in a errcatch protected evaluation */
  errcatch_all =( stack.val->errcatch == TRUE ) ? TRUE :  errcatch;
  pausecatch_all = ( stack.val->pause == FALSE ) ? TRUE: pausecatch;

  if ( lhs == 2 ||  E != NULL )
    {
      /* evaluate the function in a new frame and returns the 
       * frame as a hash table : take care that frame must be deleted 
       * at the end. 
       */
      if ( nsp_new_frame("datas") == FAIL) return RET_BUG;
      /* insert the contente of E in new frame */
      if ( E != NULL) 
	{
	  if ( nsp_frame_insert_hash_contents(E) == FAIL) 
	    {
	      Scierror("Error: inserting values in environement failed\n");
	      nsp_frame_delete();
	      return RET_BUG;
	    }
	}
      rep = rep1 = nsp_execf(stack,rhs,opt, lhs, PL, echo,display,errcatch_all,
		      (pausecatch_all == TRUE) ? FALSE: TRUE);
      if ( rep1 == RET_RETURN) rep = 0;
      if ( rep >= 0 && lhs == 2 ) H=nsp_current_frame_to_hash(); 
      nsp_frame_delete();
    }
  else 
    {
      rep = rep1 = nsp_execf(stack,rhs,opt, lhs, PL, echo,display,errcatch_all,
		      (pausecatch_all == TRUE) ? FALSE: TRUE);
      if ( rep1 == RET_RETURN) rep = 0;
    }
  
  if ( rep == RET_CTRLC ) 
    {
      Scierror("Error:\tExecution of function %s interupted\n",NSP_OBJECT(PL)->name);
      ans= FAIL;
    }	  
  else if ( lhs <= 0 && errcatch == FALSE && ( rep1 == RET_RETURN || rep1 == RET_BREAK || rep1 == RET_CONTINUE ))
    {
      // Sciprintf("propagate a return,break,continue\n");
      return rep1;
    }
  else if ( rep < 0 ) 
    {
      Scierror("Error:\tBug detected during evaluation of function  %s\n",
	       NSP_OBJECT(PL)->name);
      ans = FAIL;
    }
  if ( ans == FAIL && errcatch == FALSE )
    {
      /* nsp_error_message_show(); */
      return RET_BUG;
    }
  if ( errcatch == TRUE ) 
    {
      nsp_error_message_to_lasterror();
    }
  if (( Ob =nsp_create_boolean_object(NVOID,(ans == FAIL) ? FALSE: TRUE)) == NULLOBJ )
    return RET_BUG;
  MoveObj(stack,1,Ob);
  if ( lhs == 2) 
    {
      if ( H == NULL && (H = nsp_hcreate(NVOID,1))==NULL) return RET_BUG;
      MoveObj(stack,2,NSP_OBJECT(H));
    }
  return  Max(1,lhs);
}


/**
 * int_add_lib:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * Interface for add_lib(path, compile=%t|%f, recursive=%t|%f)
 * 
 * Returns: 
 **/

static int int_add_lib(Stack stack, int rhs, int opt, int lhs)
{
  int rep = 0;
  nsp_string dir;
  char dirname_expanded[FSIZE+1];
  char *dirname = NULL;
  int recursive=TRUE,compile=FALSE;
  int_types T[] = {string,new_opts, t_end} ;
  nsp_option opts[] ={{ "compile",s_bool,NULLOBJ,-1},
		      { "recursive",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&dirname,&opts,&compile,&recursive) == FAIL) return RET_BUG;
  CheckLhs(0,1);
  nsp_expand_file_with_exec_dir(&stack,dirname,dirname_expanded);
  dir = nsp_absolute_file_name(dirname_expanded);
  /* macros expansions is performed in EnterMacros */
  if (nsp_enter_macros(dir,recursive,compile) == FAIL ) rep=RET_BUG;
  nsp_string_destroy(&dir);
  return rep;
}

/**
 * int_remove_lib:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for remove_lib(path)
 * 
 * Returns: 
 **/

static int int_remove_lib(Stack stack, int rhs, int opt, int lhs)
{
  int rep= 0;
  nsp_string dir;
  char dirname_expanded[FSIZE+1];
  char *dirname=0;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((dirname= GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,dirname,dirname_expanded);
  dir = nsp_absolute_file_name(dirname_expanded);
  if (nsp_delete_macros(dir) == FAIL ) rep=RET_BUG;
  nsp_string_destroy(&dir);
  return rep;
}

/**
 * int_absolute_file_name:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * interface for absolute_file_name(path) 
 * which returns the absolute filename associated to 
 * path but using the current exec dir value if path 
 * is relative. 
 * 
 * Returns: 
 **/

static int int_absolute_file_name(Stack stack, int rhs, int opt, int lhs)
{
  int rep = RET_BUG;
  nsp_string apath=NULL;
  char path_expanded[FSIZE+1];
  char *path=0;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((path= GetString(stack,1)) == (char*)0) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,path,path_expanded);
  if ( (apath = nsp_absolute_file_name(path_expanded))==NULL) goto end;
  if ( nsp_move_string(stack,1,apath,-1) == FAIL) goto end;
  rep = 1;
 end:
  if ( apath != NULL) nsp_string_destroy(&apath);
  return rep;
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


static int int_restart(Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(0,1);
  /* reinitialize the function table */
  nsp_init_function_table();
  /* insert the macros libraries */
  nsp_enter_macros("SCI/macros",TRUE,FALSE);
  /* reset the linked interfaces and functions */
  return 0;
}

/*
 * ast=parse_file(...)
 * 
 */

static int int_parse_file_gen(Stack stack, int rhs, int opt, int lhs,int mtlb)
{
  NspAst *ast;
  char fname_expanded[FSIZE+1];
  char *fname= NULL;
  int_types T[] = {string, t_end} ;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&fname) == FAIL) return RET_BUG;
  nsp_expand_file_with_exec_dir(&stack,fname,fname_expanded);
  ast =nsp_parse_file(fname_expanded);
  if ( ast == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ast));
  return 1;
}

static int int_parse_file(Stack stack, int rhs, int opt, int lhs)
{
  return int_parse_file_gen(stack,rhs,opt,lhs,FALSE);
}

static int int_parse(Stack stack, int rhs, int opt, int lhs)
{
  NspAst *ast;
  NspSMatrix *S;
  CheckStdRhs(1,1);
  CheckLhs(0,1);
  if ((S=GetSMatCopy(stack,1))== NULLSMAT) return RET_BUG;
  ast =nsp_parse_from_smat(S);
  if ( ast == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ast));
  return 1;
}

static int int_ast_create_1(Stack stack, int rhs, int opt, int lhs)
{
  NspList *args=NULL;
  NspAst *ast;
  char *str=NULL;
  int code,arity=0;
  int_types T[] = {s_int,new_opts, t_end} ;
  nsp_option opts[] ={{ "str",string,NULLOBJ,-1},
		      { "args",list,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&code,&opts,&str,&args) == FAIL)   return RET_BUG;
  if ( args != NULL) 
    {
      if ( nsp_ast_check_args(args) == FAIL)
	{
	  Scierror("Error: args should be a list of ast objects\n");
	  return RET_BUG;
	}
      if ((args = (NspList *) nsp_object_copy_and_name("args",(NspObject*) args)) == NULL) 
	return RET_BUG;
      /* now we need to set the arity accordingly */
      arity =  nsp_list_length((NspList *)args);
    }
  if ((ast=nsp_ast_create(NVOID,code,arity,NULL,NULL,args,NULL,-1,NULL))==NULL)
    return RET_BUG;
  if ( str != NULL) 
    {
      if ( nsp_ast_set_str(ast,str) == FAIL) 
	return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(ast));
  return 1;
}


static int int_astv_create_1(Stack stack, int rhs, int opt, int lhs)
{
  NspAstv *astv;
  NspObject *Obj=NULL;
  int value=FALSE;
  int_types T[] = {obj ,new_opts, t_end} ;
  nsp_option opts[] ={{ "value",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&Obj,&opts,&value) == FAIL)   return RET_BUG;
  if (( astv = nsp_astv(Obj,value))== NULL)
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(astv));
  return 1;
}

/* assert */

static int int_assert(Stack stack, int rhs, int opt, int lhs)
{
  int value = TRUE;
  int_types T[] = {s_bool , t_end} ;
  CheckLhs(0,1);
  if ( GetArgs(stack,rhs,opt,T,&value) == FAIL)   return RET_BUG;
  if ( value == FALSE )
    {
      Scierror("Error: assertion failed");
      return RET_BUG;
    }
  return 0;
}

/*
 * The Interface for basic parse operations 
 */

static OpTab Parse_func[]={
  {"parse_s" , int_parse }, /* for ast */
  {"parse_eval" , int_parseevalfile },
  {"exec" , int_parseevalfile },
  {"exec_mtlb" , int_parseevalfile_mtlb },
  {"remove_lib",int_remove_lib},
  {"add_lib",int_add_lib},
  {"absolute_file_name", int_absolute_file_name},
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
  {"input", int_input},
  {"halt", int_halt},
  {"restart",int_restart},
  {"parse_file",int_parse_file}, /* for ast */
  {"ast_create",int_ast_create_1}, /* for ast */
  {"astv_create",int_astv_create_1}, /* for astv */
  {"assert",int_assert}, /* for astv */
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






