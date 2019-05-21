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
 * Caling functions or extractions for variables 
 *--------------------------------------------------------------------------*/

#include <stdio.h>
#include <string.h>

#include <nsp/nsp.h> 
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/bhash.h> 
#include <nsp/cells.h> 
#include <nsp/smatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/list.h> 
#include <nsp/file.h> 
#include <nsp/hobj.h> 
#include <nsp/function.h> 
#include <nsp/imatrix.h> 
#include <nsp/hash.h> 
#include <nsp/ivect.h> 
#include <nsp/plisttoken.h> /*for name_maxl **/
#include <nsp/stack.h> 
#include <nsp/parse.h> 
#include <nsp/matint.h> 
#include <nsp/accelerated_tab.h>
#include <nsp/frame.h> 
#include <nsp/seval.h> 
#include <nsp/libstab.h> 
#include <nsp/funtab.h>
#include <nsp/nspdatas.h>
#include <nsp/nspthreads.h>
#include <nsp/gtk/gobject.h>
#include <nsp/system.h>

static int nsp_check_named(PList Loc,int i,int j, Stack stack, int first, int nargs);
static void FuncEvalErrorMess(const char *str,Stack *stack,int first,int msuffix);
static int SearchInOPt(char *str, Stack stack, int first, int nargs,int *wrong_pos);
static int frame_insert_var(int rhs,int opt,int lhs);
static int extract_varargout(Stack stack,NspObject *O,int *j,int Lhs);
static int  MacroEval_Base(NspObject *OF, Stack stack, int first, int rhs, int opt, int lhs,int display);
static void nsp_build_funcname_tag(const char *str, Stack *stack, int first, int rhs, char *name,char **tag);

/*
 * FIXME: 
 * reste des choses mal faites. comment gerer les macros qui 
 * sont ds les env locaux. Seule les macros des librairies 
 * sont correctement g�r�e pour les noms compos�s 
 * par ex: 
 * function y=f_m(x);y=sin(x);endfunction
 * f(5) 
 * ne marchera pas car pour la recherche des variables seul f est cherch�e 
 * donc ds FuncEval O sera non null si on appelle f_m(5) et null sinon. 
 *
 * Un pb different est f=sin; f(5) qui apparait ds extract 
 * et est peut-etre a regler plus haut pour qu'on arrive ds FuncEval 
 * Noter en plus que f=sin pose un pb car sin existe pas seule sin_m existe 
 * i.e faut peut-etre aussi tjrs definir la fonction la moins sp�cialis�e. 
 * 
 * Il y a un autre pb c'est si on appelle sin_m(5) directement 
 * on veut pas de name_mangling il faut donc trouver un prefixe 
 * qui annulle le name mangling: c'est pour l'instant __sin_m 
 * 
 */

/**
 *nsp_eval_func:
 * @O: 
 * @str: 
 * @msuffix:
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * 
 * Evaluation of FEVAL expressions i.e f(...) 
 * but where f=@str is a function name or a variable name.
 * @str arguments are stored in the stack at position [@first,@rhs+@first[;
 * within the @rhs arguments the last @opt argument are optional 
 * (i.e specified as name=<expr>). 
 * Here we are in the case where a variable with name @str is 
 * an object (@O != NULL) found in the calling stacks which is a macro 
 * or @str was not found and has to be searched in functions and macros
 * hash tables.
 * The evaluation must return the requested @lhs arguments 
 * on the stack at position [@first,@first + @lhs[  
 * Each interface function must also clean the stack 
 * 
 * 
 * Return value: #RET_BUG or the number of returned arguments
 **/

/* #define LIBSTAB_NEW */

int nsp_eval_func(NspObject *O,const char *str, int msuffix, Stack stack, int first, int rhs, int opt, int lhs)
{
  NspObject *M;
  char *tag[]={NULL,NULL,NULL};
  char name[NAME_MAXL];
  stack.first = first;
  if ( O != NULLOBJ && IsNspPList(O)) 
    {
      /* f(...) where f was found on the calling stacks 
       * and is a macro. The case f is a function should be 
       * also performed here ( f=sin;f(56);
       */
      NspPList *Pl= NspPListObj(O);
      /* Calling a function given by Pl */
      /* if (debug) Sciprintf("==>%s exists\n",str); */
      /* nsp coded function evaluation */
      return nsp_eval_macro(NSP_OBJECT(Pl),stack,first,rhs,opt,lhs);
    }
  else 
    {
      /* Build a new name according to arguments type not using optional arguments 
       * we first build the most specialized name (i.e based on Min(rhs-op,2)) 
       * then loop up to less specialized 
       */ 
      int nb_suffix = Min(msuffix, rhs-opt),sm=0;
      nsp_build_funcname_tag(str,&stack,first,nb_suffix,name,tag);
      while (1) 
	{
#ifdef LIBSTAB_NEW 
	  int type ;
	  if ((M = nsp_find_macro_or_func(name,&type)) != NULLOBJ)
	    {
	      if ( type == 1) 
		{
		  /* Call a primitive with name depending on argument types */
		  NspFname(stack) = name ;
		  return nsp_interfaces(((NspFunction *) M)->iface,
					((NspFunction *) M)->pos,stack,rhs,opt,lhs);
		}
	      else 
		{
		  return(nsp_eval_macro(M,stack,first,rhs,opt,lhs));
		}
	    }
#else 
	  int Int,Num;
	  if ( nsp_find_function(name,&Int,&Num) == OK) 
	    {
	      /* Call a primitive with name depending on argument types */
	      NspFname(stack) = name ;
	      return(nsp_interfaces(Int,Num,stack,rhs,opt,lhs));
	    }
	  if ((M=nsp_find_macro(name)) != NULLOBJ) 
	    {
	      return(nsp_eval_macro(M,stack,first,rhs,opt,lhs));
	    }
#endif 
	  if ( tag[sm] == NULL) break;
	  /* truncate name to previous suffix */
	  tag[sm++][0]='\0';
	}
      /* A revoir XXXXX */
      FuncEvalErrorMess(str,&stack,first,nb_suffix);
      /*clean the stack */
      nsp_reorder_stack(stack,0);
      return RET_BUG;
    }
}


/**
 *nsp_eval_dotplus:
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * try to fast eval a .+ : THIS IS JUST A TEST FUNCTION.
 * 
 * Return value: #RET_BUG or the number of returned arguments
 **/

extern function int_mxdadd;

int nsp_eval_dotplus(Stack stack, int first, int rhs, int opt, int lhs)
{
  NspObject *o1,*o2;
  int ret;
  /* I assume here that the two given arguments are Matrices */
  if ((o1 =nsp_get_object(stack,1))== NULLOBJ) return RET_BUG; 
  if ((o2 =nsp_get_object(stack,2))== NULLOBJ) return RET_BUG; 
  if ( o1->basetype->id == nsp_type_matrix_id 
       && o1->basetype == o2->basetype) 
    {
      stack.first = first;
#define TEST2
#ifdef TEST1
      return nsp_interfaces(0,130,stack,rhs,opt,lhs);
#endif 
#ifdef TEST2
      ret = int_mxdadd(stack,rhs,opt,lhs); 
      return nsp_reorder_stack(stack,ret);
#endif
    }
  else
    {
      const char *name =nsp_astcode_to_nickname(DOTPLUS);
      /* should use the macro in Eval.c 
       * Nsp_frames_search_op_object
       */
      o1 =nsp_frames_search_object(name);
      return nsp_eval_func(o1,name,2,stack,first,rhs,opt,lhs);
    }
  return ret;
}



/**
 * nsp_eval_method:
 * @str: a method name 
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 *  invoke method @str on object stored at position @first.
 *  Arguments including the object are stored at position [@first,@rhs+@first[;
 *  The evaluation must return the requested @lhs arguments 
 *  on the stack at position [first,first + lhs[. 
 * 
 * Return value: a number of argument stored on the calling stack or a negative 
 *   value in case of error.
 **/

int nsp_eval_method(char *str, Stack stack, int first, int rhs, int opt, int lhs)
{
  int ret;
  NspObject *ob = NthObj(1);
#if 1 
  NspObject *ob1 = ob;
#endif 
  stack.first = first+1; 
  rhs--;
  NspFname(stack) = str;
  /* XXX To be done: next step would be to accept soft coded methods: */
  HOBJ_GET_OBJECT(ob,RET_BUG);
  ret =  nsp_exec_method_util(ob,ob->basetype,str,stack,rhs,opt,lhs);
  if ( ret == RET_BUG ) 
    {
      /* clean the stack before returning */
      NspObject**O = stack.val->S + stack.first; 
      while ( *O != NULL) 
	{ 
	  (*O)->ret_pos= -1;
	  O++;
	}
      nsp_reorder_stack(stack,0);
      stack.val->S[stack.first-1]= NULLOBJ; 
      return RET_BUG;
    }
  
  ret = nsp_reorder_stack(stack,ret);
  
  if ( ret == RET_BUG ) 
    {
      /* XXXX */
      return RET_BUG;
    }
  else 
    {
      int i;
#if 1
      int free=TRUE;
#endif 
      stack.first--; 
      /* here we check if we can clean ob if ob is unnamed and not in the returned objects */
      for (i = 0 ; i < ret ; i++) 
	{
	  stack.val->S[stack.first+i]=stack.val->S[stack.first+i+1];
#if 1	  
	  if (stack.val->S[stack.first+i] == ob1 ) free=FALSE;
#endif 
	}
#if 1
      /* if activated it causes a crash in polynoms */
      if ( ret > 0 && Ocheckname(ob1,NVOID) && free == TRUE)
	{
	  nsp_object_destroy(&ob1);
	}
#endif 
      stack.val->S[stack.first+ret] = NULLOBJ;
    }
  return ret;
}


/**
 * nsp_eval_extract:
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * Extractions operations evaluation. Typically 
 * x(:),x(:,<>), x(<>,:), x(<>,<>),x(<>,<>,<>)
 * object on which to perform extraction , indices are stored on @stack 
 * at positions [@first,@first+@rhs [.
 * 
 * Return value: a number of argument stored on the calling stack or a negative 
 *   value in case of error.
 **/

int nsp_eval_extract(Stack stack, int first, int rhs, int opt, int lhs)
{
  int n;
  stack.first = first;
  if ( rhs - opt == 2 )
    {
      if ( IsIVect(stack.val->S[stack.first+1] ))       /* x(:) */
	{
	  nsp_void_seq_object_destroy(stack,stack.first+1,stack.first+2);
	  n = nsp_eval_maybe_accelerated_op("resize2vect", 1, resize2vect_tab, 
					    stack,stack.first, rhs-1, opt , 1);
	}
      else                                              /* x(i) */           
	{
	  n = nsp_eval_maybe_accelerated_op("extractelts", 1, extractelts_tab,
					    stack,stack.first, rhs,  opt, 1);
	}
    }
  else if ( rhs-opt == 3 )
    {
      if (IsIVect(stack.val->S[stack.first+1]) )
	{
	  if (IsIVect(stack.val->S[stack.first+2]) )    /* x(:,:) --> keep x unchanged */
	    {
	      nsp_void_seq_object_destroy(stack,stack.first+1,stack.first+3);
	      n = 1;
	    }
	  else 		                                /* x(:,j) */
	    {
	      nsp_void_seq_object_destroy(stack,stack.first+1,stack.first+2);
	      stack.val->S[stack.first+1] = stack.val->S[stack.first+2];
	      stack.val->S[stack.first+2] = NULLOBJ;
	      n = nsp_eval_maybe_accelerated_op("extractcols", 1, extractcols_tab,
						stack,stack.first, rhs-1, opt, 1);
	    }
	}
      else 
	{
	  if (IsIVect(stack.val->S[stack.first+2]) )    /* x(i,:) */
	    {
	      nsp_void_seq_object_destroy(stack,stack.first+2,stack.first+3);
	      n = nsp_eval_maybe_accelerated_op("extractrows", 1, extractrows_tab,
						stack,stack.first, rhs-1, opt, 1);
	    }
	  else                                          /* x(i,j) */
	    {
	      n = nsp_eval_maybe_accelerated_op("extract", 1, extract_tab,
						stack,stack.first, rhs, opt, 1);
	    }
	}
    }
  else  /* rhs -opt > 3  currently not implemented */
    {
      Scierror("Error: multi-dimensionnal arrays not currently supported\n");
      /* voir pour le message d'erreur */
      n = -1;
    }
  if ( n < 0 )  return RET_BUG ; else return n;
}


/**
 * nsp_eval_extract_cells:
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * Extractions operations for {} 
 * x{:},x{:,<>}, x{<>,:}, x{<>,<>},x{<>,<>,<>}
 * object on which to perform extraction , indices are stored on @stack 
 * at positions [@first,@first+@rhs [.
 * Mostly a copy of nsp_eval_extract() but the called functions are prefixed 
 * with string ce. Note also that cells extraction can return a sequence 
 * of values.
 * 
 * Return value: a number of argument stored on the calling stack or a negative 
 *   value in case of error.
 **/

int nsp_eval_extract_cells(Stack stack, int first, int rhs, int opt, int lhs)
{
  int nret;
  stack.first = first;
  if ((nret =nsp_eval_extract(stack,first,rhs,opt,lhs)) < 0) 
    {
      nsp_void_seq_object_destroy(stack,first,first+rhs);
      return nret;
    }
  if ( nret != 1 ) 
    {
      Scierror("Error: extraction cannot be performed\n");
      Scierror("\tExpecting one object on the stack and %d found\n",nret);
      nsp_void_seq_object_destroy(stack,first,first+nret);
      return RET_BUG;
    }
  /* now perform a convertion to sequence of objects */
  return nsp_eval_func(NULLOBJ,"object2seq",1,stack,stack.first,1,0,1);
}


/**
 * nsp_build_funcname:
 * @str: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @name: 
 * 
 * Build a name which depends on argument types 
 * here rhs must be rhs or a smaller number 
 * the mangling name strategy is limited to 2 arguments 
 * f() -> f
 * f(x1) -> f_<type_x1>
 * f(x1,x2) -> f_<type_x1>_<type_x2>
 * f(x1,x2,...) -> f_<type_x1>_<type_x2>
 * 
 **/

void nsp_build_funcname(const char *str, Stack *stack, int first, int rhs, char *name)
{
  char *s1;
  rhs = Min(rhs,2);
  switch (rhs) 
    {
    case 2: 
      /*Build a name which depends on argument type **/
      /*Faster than a sprintf **/
      while ( *str != '\0' ) *name++ = *str++ ;
      *name++ = '_';
      s1=nsp_object_type_short(stack->val->S[first]);
      while ( *s1 != '\0')  *name++ = *s1++ ;
      *name++ = '_';
      s1=nsp_object_type_short(stack->val->S[first+1]);
      while ( *s1 != 0) *name++ = *s1++;
      *name = '\0';
      break;
    case 1:
      /*Build a name which depends on argument type **/
      while ( *str != '\0' ) *name++ = *str++ ;
      *name++ = '_';
      s1=nsp_object_type_short(stack->val->S[first]);
      while ( *s1 != '\0')  *name++ = *s1++ ;
      *name = '\0';
      break;
    default: 
      while ( *str != '\0' ) *name++ = *str++ ;
      *name = '\0';
      break;
    }
}


void nsp_build_funcname_tag(const char *str, Stack *stack, int first, int rhs, char *name,char **tag)
{
  char *s1;
  rhs = Min(rhs,2);
  switch (rhs) 
    {
    case 2: 
      /*Build a name which depends on argument type **/
      /*Faster than a sprintf **/
      while ( *str != '\0' ) *name++ = *str++ ;
      tag[1]=name;
      *name++ = '_';
      s1=nsp_object_type_short(stack->val->S[first]);
      while ( *s1 != '\0')  *name++ = *s1++ ;
      tag[0]=name;
      *name++ = '_';
      s1=nsp_object_type_short(stack->val->S[first+1]);
      while ( *s1 != 0) *name++ = *s1++;
      *name = '\0';
      break;
    case 1:
      /*Build a name which depends on argument type **/
      while ( *str != '\0' ) *name++ = *str++ ;
      tag[0]=name;
      *name++ = '_';
      s1=nsp_object_type_short(stack->val->S[first]);
      while ( *s1 != '\0')  *name++ = *s1++ ;
      *name = '\0';
      break;
    default: 
      while ( *str != '\0' ) *name++ = *str++ ;
      *name = '\0';
      break;
    }
}


/**
 * nsp_build_funcnameij:
 * @str: 
 * @stack: 
 * @first: 
 * @i: 
 * @j: 
 * @name: 
 *
 * Build a name which depends on argument i and argument j
 * 
 **/

void nsp_build_funcnameij(const char *str, Stack *stack, int first, int i, int j, char *name)
{
  char *s1,*s2;
  /*Build a name which depends on argument type **/
  /*Faster than a sprintf **/
  while ( *str != '\0' ) *name++ = *str++ ;
  *name++ = '_';
  s1=nsp_object_type_short(stack->val->S[first+i]);
  while ( *s1 != '\0')  *name++ = *s1++ ;
  *name++ = '_';
  s2=nsp_object_type_short(stack->val->S[first+j]);
  while ( *s2 != 0) *name++ = *s2++;
  *name = '\0';
}


/**
 *nsp_build_funcname_i:
 * @str: 
 * @stack: 
 * @first: 
 * @i: 
 * @name: 
 * 
 * Build a name which depends on argument i 
 * 
 **/

void nsp_build_funcname_i(const char *str, Stack *stack, int first, int i, char *name)
{
  /*Build a name which depends on argument type */
  while ( *str != '\0' ) *name++ = *str++ ;
  *name++ = '_';
  str=nsp_object_type_short(stack->val->S[first+i]);
  while ( *str != '\0')  *name++ = *str++ ;
  *name = '\0';
}


/**
 * FuncEvalErrorMess:
 * @str: 
 * @rhs: 
 * @opt: 
 * @name: 
 * @name1: 
 * 
 * raise an error message for function evaluation.
 * 
 **/

static void FuncEvalErrorMess(const char *str,Stack *stack,int first,int msuffix)
{
  switch (msuffix)
    {
    case 0: 
      Scierror("Error:\tUnknown function %s \n",str);
      break;
    case 1:
      Scierror("Error:\tUnknown function %s_%s or %s\n",
	       str,nsp_object_type_short(stack->val->S[first]),
	       str);
      break;
    default: 
      Scierror("Error:\tUnknown functions %s_%s_%s, %s_%s or %s\n",
	       str,nsp_object_type_short(stack->val->S[first]),
	       nsp_object_type_short(stack->val->S[first+1]),
	       str,nsp_object_type_short(stack->val->S[first]),
	       str);
      break;
    }
}

/**
 * nsp_eval_macro:
 * @OF: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * evalution of the nsp coded function stored in @OF
 * arguments are in the stack at position [@first,@first+@rhs[
 *
 * Return value: a number of argument stored on the calling stack or a negative 
 *   value in case of error.
 **/

/* #define NSP_PROFILING */

int nsp_eval_macro(NspObject *OF, Stack stack, int first, int rhs, int opt, int lhs)
{
  const char *name_def="datas",*name= NSP_OBJECT(OF)->name;
  int rep;
  /* new data frame for function evaluation */
#ifdef NSP_PROFILING
  double cpu = nsp_cputime();
  ((NspPList *) OF)->counter++;
#endif 
#ifdef WITH_SYMB_TABLE
  if ( strcmp(name,NVOID)==0 ) name=name_def;
  nsp_new_frame_with_local_vars(name,((NspPList *) OF)->D->next->next->next->O);
#else 
  nsp_new_frame(name); 
#endif 
  stack.first = first;
  /* Sciprintf("<-- %s\n",name); */
  if ((rep= MacroEval_Base(OF,stack,first,rhs,opt,lhs,0) ) == RET_BUG) 
    {
      /*clean the stack */
      nsp_reorder_stack(stack,0);
    }
  /*Closing the frame **/
  nsp_frame_delete();
  /* Sciprintf("--> %s\n",name);*/
#ifdef NSP_PROFILING
  ((NspPList *) OF)->cpu +=  nsp_cputime() -cpu;
#endif
  return rep;
}

static int  MacroEval_Base(NspObject *OF, Stack stack, int first, int rhs, int opt, int lhs, int display)
{
  nsp_datas *data = nsp_get_datas();
  int count_ret, nargs,j,js=0,i,n,posi,nret,body_ret, varargin_case=0,option_case =0,
    less_args_case = 0 ;
  NspObject *O;
  PList Lhs,Feval,Body,Loc,Loc1;
  PList P = ((NspPList *) OF)->D;
  Lhs  = (PList) ((PList) P->next->O)->next->O;
  Feval= (PList) ((PList) P->next->O)->next->next->O;
  Body = (PList) P->next->next->O;
  /*Test on Lhs **/
  stack.first = first;
  NspFname(stack) = ((NspObject *) OF)->name;
  NspFileName(stack) = ((NspPList *) OF)->file_name;
  /* stack.val->symbols =(NspObject *) symb_table; */
  Loc = Lhs;
  nret = Loc->arity ; 

  if ( frame_insert_var(rhs,opt,lhs) == FAIL) 
    {
      return RET_BUG;
    }

  if ( lhs >=0 && lhs > nret ) 
    {
      /* We ignore this case since the function can 
       * return more than nret values if one argument is 
       * a varargout variable 
       */
      /*
	Scierror("Error:\t%d arguments required and %s can only return up to %d values \n",
	lhs,Feval->next->O,nret);
	return RET_BUG;
      */
    }
  if (debug) 
    {
      Sciprintf("Lhs \n");
      nsp_plist_print_internal(Lhs);
      Sciprintf("Feval \n");
      nsp_plist_print_internal(Feval);
      Sciprintf("Body\n");
      nsp_plist_print_internal(Body) ;
    }
  /* Exploring Feval to insert arguments in the new frame */
  Loc = Feval;
  nargs = Loc->arity - 1 ;
  Loc = Feval->next;
  /* nargs give the number of arguments given in the 
   * macro definition
   */
  for ( j = 0 ; j < nargs ; j++)
    {
      Loc = Loc->next ;
      if ( Loc->type  != NAME ) 
	{
	  /* js is the arg list position at which we are positioned 
	   */
	  js = j;
	  option_case = 1;
	  break;
	}
      if ( strcmp(Loc->O,"varargin")==0) 
	{
	  js = j;
	  varargin_case = 1;
	  break;
	}
      else if ( strcmp(Loc->O,"varargopt")==0) 
	{
	  /* js is the stack position at which we have to start 
	   * searching options 
	   */
	  js = j;
	  option_case = 2;
	  break;
	}
      if ( rhs < j+1 )
	{
	  /* this can be checked with nargin in the macro body 
	   */
	  less_args_case = 1;
	  break; 
	  /* 
	   * Scierror("Error:\tCall to macro %s : missing argument %s \n",Feval->next->O,Loc->O);
	   * return RET_BUG;
	   */
	}
      
      /* here we have to insert stack.val->S[first+j] in the 
       * local frame but note that this could be accelerated 
       * since we know that variables are in the function var table.
       */
      
      if( IsHobj(stack.val->S[first+j]) )
	{
	  /* pointer cases */

	  if ( IsHopt(stack.val->S[first+j]))
	    {
	      /* argument is given as name=expr while expecting an expr */
	      Sciprintf("Error: argument %d of function %s given as an optional named argument %s=.. \n",
			j+1,Feval->next->O,nsp_object_get_name(stack.val->S[first+j]));
	      return RET_BUG;
	    }
	  else 
	    {
	      /*  Argument is a pointer : create a new local pointer 
	       *  which points to the same object 
	       */
	      NspHobj *H = (NspHobj *) stack.val->S[first+j];
	      NspObject *H1= ((NspObject *) H);
	      HOBJ_GET_OBJECT(H1,RET_BUG);
	      if ((H = HobjCreate(Loc->O,H1)) == NULLHOBJ) return RET_BUG;
	      if ( nsp_frame_replace_object((NspObject *)H,Loc->arity )==FAIL)
		{
		  nsp_hobj_destroy(H);
		  return RET_BUG;
		}
	    }
	}
      else 
	{
	  if ( ! Ocheckname(stack.val->S[first+j],NVOID)) 
	    {
	      /*  Object given on the calling sequence is given by its name
	       *  we create a pointer to this object inserted in local frame
	       */
	      NspHobj *H;
	      if ((H = HobjCreate(Loc->O,stack.val->S[first+j])) == NULLHOBJ) return RET_BUG;
	      if ( nsp_frame_replace_object((NspObject *)H,Loc->arity)== FAIL) 
		{
		  nsp_hobj_destroy(H);
		  return RET_BUG;
		}
	    }
	  else 
	    {
	      /* we can use the transmited value directly */
	      if (nsp_object_set_name(stack.val->S[first+j],(char *) Loc->O)== FAIL) return RET_BUG;
	      if ( nsp_frame_replace_object(stack.val->S[first+j],Loc->arity)== FAIL) 
		{
		  return RET_BUG;
		}
	    }
	}
    }


  if ( less_args_case == 1 ) 
    {
      /* less arguments than expected. 
       * we have to check if optional arguments or varargin exist
       * in the argument list 
       */ 
      for ( i = j+1 ; i < nargs ; i++)
	{
	  Loc = Loc->next ;
	  if ( Loc->type  != NAME ) 
	    {
	      js=i;
	      option_case = 1;
	      break;
	    }
	  else if ( strcmp(Loc->O,"varargopt")==0) 
	    {
	      js=i;
	      option_case = 2;
	      break;
	    }
	  else if ( strcmp(Loc->O,"varargin")==0) 
	    {
	      js=i;
	      varargin_case = 1;
	      break;
	    }
	}
    }
  
  /* the varargin case 
   * we collect all the remaining variables up to the 
   * named optional arguments in a list called varargin.
   */ 
  
  if (varargin_case == 1) 
    {
      
      NspList *L;
      /* Sciprintf("varargin case "); */
      if ((L =nsp_list_create(NVOID))==NULLLIST) return RET_BUG;
      for ( i = j ; i < rhs-opt ; i++ )
	{
	  if( IsHopt(stack.val->S[first+i]) ) 
	    {
	      Scierror("Error:%s named optional argument found (%s) while collecting last arguments in varargin\n",
		       Feval->next->O,stack.val->S[first+i]->name);
	      return RET_BUG;
	    }
	  if ( MaybeObjCopy(&stack.val->S[first+i]) == NULL)  return RET_BUG;
	  if (nsp_object_set_name(stack.val->S[first+i],"lel") == FAIL) return RET_BUG;
	  if (nsp_list_end_insert(L,stack.val->S[first+i]) == FAIL ) return RET_BUG;
	}
      if (nsp_object_set_name((NspObject *)L,(char *) Loc->O)== FAIL) return RET_BUG;
      if (nsp_frame_replace_object((NspObject *) L,-1)==FAIL) 
	{
	  nsp_list_destroy(L);
	  return RET_BUG;
	}
      /* varargin can be followed by optional args */
      if ( js+1 < nargs  ) 
	{
	  Loc = Loc->next ;
	  if ( Loc->type  != NAME ) 
	    {
	      option_case = 1;
	    }
	  else if ( strcmp(Loc->O,"varargopt")==0) 
	    {
	      option_case = 2;
	    }
	  j = rhs-opt; /* position of first named optional in the call  */
	  js++; /* position of first named optional in function def */
	}
    }
  
  /*Optional arguments **/  

  if ( option_case == 1 ) 
    {
      if ( nsp_check_named(Loc,js,nargs,stack,first+j,rhs-j)==FAIL) 
	return RET_BUG;

      /* search optional arguments in stack from first+j to end of stack  */
      int wrong_pos=js+1; 
      if (debug) Sciprintf("%d named optional arguments:",nargs -js);
      for ( i= js ; i < nargs ; i++ ) 
	{
	  Loc1 = ((PList) Loc->O)->next;
	  if ( debug ) Sciprintf("<%s> ",(char *) Loc1->O);
	  posi=SearchInOPt((char *) Loc1->O,stack,first+j,rhs-j,&wrong_pos);
	  if ( posi == -2 ) 
	    {
	      Scierror("Error:%s optional argument expected and expression found at position %d\n",
		       Feval->next->O,wrong_pos);
	      return RET_BUG;
	    }
	  if ( posi >= 0 ) 
	    {
	      /*argument is given at position posi **/
	      NspHobj *H;
	      H = (NspHobj*) stack.val->S[posi];
	      if ( Ocheckname(H->O,NVOID) ) 
		{
		  /*we can use the transmited value directly */
		  if (nsp_object_set_name(H->O,(char *) Loc1->O)== FAIL) return RET_BUG;
		  if ( nsp_frame_replace_object(H->O,-1) == FAIL) 
		    {
		      return RET_BUG;
		    }
		}
	      else 
		{
		  if ((H = HobjCreate((char *) Loc1->O,H->O)) == NULLHOBJ) return RET_BUG;
		  if ( nsp_frame_replace_object((NspObject *)H,-1) == FAIL) 
		    {
		      nsp_hobj_destroy(H);
		      return RET_BUG;
		    }
		}
	    }
	  else
	    {
	      /* an optional named argument with name (char *) Loc1->O 
	       * was not found in the calling sequence. We have to evaluate the 
	       * default value and use it in the stack.
	       */
	      n =nsp_eval_arg(Loc1->next,&stack,first+rhs,1,1,0);
	      if ( n > 1 ) 
		{
		  int i ; 
		  /* to many argument returned we ignore the last ones */
		  for ( i = 2 ; i <= n ; i++)nsp_void_object_destroy(&stack.val->S[first+i]);
		  n=1;
		}
	      if ( n == 1 ) 
		{
		  int rep =nsp_store_result((char *) Loc1->O,stack,first+rhs); 
		  stack.val->S[first+rhs]= NULLOBJ ;
		  if ( rep < 0 ) return RET_BUG;
		}
	      else 
		{
		  stack.val->S[first+rhs]= NULLOBJ ;
		  return n;
		}
	    }
	  Loc = Loc->next;
	}
      if (debug) Sciprintf("\n",nargs -j);
    }
  
  else if ( option_case == 2 ) 
    {
      /* Objects from j+1 to rhs are optional arguments 
       * which are to be inserted in hash table varargopt 
       */
      NspHash *H;
      NspObject *O;
      /* Only optional arguments are given */ 
      if ( js+1 < nargs) 
	{
	  Scierror("Error: in function %s, varargopt should be the last argument\n",
		   NspFname(stack));
	  return RET_BUG;
	}
      if(( H = nsp_hash_create("varargopt",rhs-js)) == NULLHASH) return RET_BUG;
      for ( i = j+1 ; i <= rhs ; i++) 
	{
	  if ( IsHopt(NthObj(i))== FALSE )
	    {
	      Scierror("Error: %s of function %s is wrong, expecting optional argument (name = val) for varargopt\n",
		       ArgPosition(i),NspFname(stack));
	      return RET_BUG;
	    }
	  /* A copy of object is added in the hash table **/
	  /* GetObj takes care of Hobj pointers **/
	  if (( O =nsp_object_copy(nsp_get_object(stack,i))) == NULLOBJ ) return RET_BUG;
	  if (nsp_object_set_name(O,nsp_object_get_name(NthObj(i))) == FAIL) return RET_BUG;
	  if (nsp_hash_enter(H,O) == FAIL) return RET_BUG;
	}
      if ( nsp_frame_replace_object((NspObject *)H,-1) == FAIL) return RET_BUG;
    }
  
  /* we can now clean the stack since arguments are now on the local frame 
   * We only have to take care of Hopt which are to be destroyed 
   */
  
  for ( j = 1 ; j <= rhs  ; j++) 
    {
      /* delete Hopt objects (but of course not the object they point to) */
      if ( IsHopt(NthObj(j)) ) nsp_object_destroy(&NthObj(j));
      NthObj(j)=NULLOBJ;
    }
  
  /* Body Evaluation */
  body_ret =nsp_eval(Body,stack,first,0,0,display);
  if ( body_ret < 0 ) 
    {
      char *filename =( (NspPList *) OF)->file_name ; 
      switch (body_ret) 
	{
	case RET_RETURN: break;
	case RET_ABORT : 
	  Scierror("Aborting evaluation of function %s\n",Feval->next->O);
	  return body_ret;
	  break;
	case RET_ERROR_RAISED: 
	  /* error was raised at nsp level through the nsp error function */
	  if ( stack.val->errcatch == FALSE ) 
	    {
	      nsp_error_message_show();
	    }
	  return RET_BUG ; /* we want here the next error message */
	default: 
	  Scierror("Error in body evaluation of function %s",Feval->next->O);
	  if ( filename != NULL) 
	    Scierror(" (file '%s')\n",filename);
	  else 
	    Scierror("\n");
	  if ( stack.val->errcatch == FALSE ) 
	    {
	      int stop;
	      nsp_error_message_show();
	      Sciprintf("\tEntering a pause in function %s\n",Feval->next->O);
	      inc_pause_prompt();
	      stop= nsp_parse_eval_from_std(1);
	      dec_pause_prompt();
	      nsp_readline_clear_line();
	      /* 
	       * here we must know if nsp_parse_eval_from_std was stopped 
	       * by a quit or a abort.
	       */
	      if ( stop == RET_ABORT ) 
		{
		  Sciprintf("\tAborting %s execution\n",Feval->next->O);
		  return RET_ABORT;
		}
	      else 
		{
		  Sciprintf("\treturning from %s with error\n",Feval->next->O);
		}
	    }
	  return RET_BUG ; /* we want here the next error message */
	}
    }

  /* Return computed values and close Framedestroy */
  Loc = Lhs->next;
  
  count_ret = 0;
  for ( j = 0 ; j < nret  ; j++) 
    {
      int lhs_1=Max(lhs,1);
      if ( count_ret >= lhs_1 ) break;
      /* XXXXX
       * Scilab does not exactly work the same way 
       * on peut retourner par une fonction une valeur non calculee 
       * par la fonction mais qui existe ds un frame + haut 
       * ca me parait bof 
       */
      if ( Loc->arity != -1 ) 
	{
	  int tag = VAR_IS_PERSISTENT(Loc->arity) ? 2 : 1;
	  /* search return value in local variables */
	  O =((NspCells*) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(Loc->arity)];
	  ((NspCells*) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(Loc->arity)]= NULL;
	}
      else 
	{
	  O=nsp_frame_search_and_remove_object(Loc->O);
	}
      if ( O ==  NULLOBJ) 
	{
	  /* If a return value was not computed we consider that it's a  bug */
	  Scierror("Error:\tNo %s value computed inside function %s \n",
		   Loc->O,Feval->next->O);
	  return RET_BUG;
	}
      if ( strcmp((char *)Loc->O,"varargout") == 0 )
	{
	  if ( IsList(O) == FALSE ) 
	    {
	      Scierror("Error: varargout computed inside function %s is not a list\n",
		       Feval->next->O);
	      return RET_BUG;
	    }
	  extract_varargout(stack,O,&count_ret,lhs_1);
	}
      else 
	{
	  stack.val->S[first+count_ret] = O;
	  /*
	   * XXXXX : Removing names of objects since they were extracted 
	   *         from a deleted frame i.e they can be used without copying them
	   */
	  if (nsp_object_set_name(stack.val->S[first+count_ret],NVOID)== FAIL) return RET_BUG;
	  if( IsHobj(stack.val->S[first+count_ret]) )
	    {
	      /* If an object is a pointer we delete it and return the 
	       * object it pointed to. The pointer was created when entering 
	       * this function.
	       */
	      NspHobj *H =(NspHobj*) stack.val->S[first+count_ret];
	      NspObject *H1= (NspObject *) H;
	      HOBJ_GET_OBJECT(H1,RET_BUG);
	      stack.val->S[first+count_ret] = H1;
	      H->O= NULLOBJ;
	      nsp_hobj_destroy(H);
	    }
	  count_ret++;
	}
      
      Loc = Loc->next;
    }

  if ( lhs >=0 && lhs > count_ret ) 
    {
      Scierror("Error:\t%d arguments required and %s only returned %d values \n",
	       lhs,Feval->next->O,count_ret);
      return RET_BUG;
    }

  return count_ret ;
}


static int extract_varargout(Stack stack,NspObject *O,int *j,int Lhs)
{
  
  NspList *L = (NspList *) O;
  Cell *C= L->first;
  while ( C != NULLCELL) 
    {
      if ( *j >= Lhs ) break;
      if ( C->O != NULLOBJ )
	{
	  stack.val->S[stack.first+(*j)] = C->O;
	  C->O = NULLOBJ;
	  if (nsp_object_set_name(stack.val->S[stack.first+(*j)],NVOID)== FAIL) return FAIL;
	  (*j)++;
	}
      C = C->next ;
    }
  /* XXX here we need to destroy the list */
  nsp_list_destroy(L);
  return OK;
}


/**
 * nsp_eval_macro_body:
 * @OF: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * Evaluation of the body of a nsp coded function 
 * in the current Frame.  Arguments are supposed to be in the current Frame 
 * used in exec(f) or execf(f).
 * 
 * 
 * Return value: a number of argument stored on the calling stack or a negative 
 * value in case of error. Note that a return in the macro body will return a 
 * RET_RETURN which is negative (and not an error).
 **/

int nsp_eval_macro_body(NspObject *OF, Stack stack, int first, int rhs, int opt, int lhs,int display)
{
  /* int body_ret ; */
  PList P = ((NspPList *) OF)->D;
  /* PList Lhs  = (PList) ((PList) P->next->O)->next->O; */
  /* PList Feval= (PList) ((PList) P->next->O)->next->next->O; */
  PList Body = (PList) P->next->next->O;
  /*Test on Lhs */
  /* int nret = Lhs->arity ; */
  stack.first = first;
  return nsp_eval(Body,stack,first,0,0,display);
  /* 
  body_ret= nsp_eval(Body,stack,first,0,0,display);
  if ( body_ret < 0 && body_ret != RET_RETURN ) return body_ret ;
  return 0 ;
  */
}


/**
 * SearchInOPt:
 * @str: 
 * @stack: 
 * @first: 
 * @nargs: 
 * @wrong_pos: 
 * 
 * Used to collect named optional arguments in macro calls.
 * Arguments in the stack at position stack.val->S[first+i] 
 * for i=0 to nargs are searched for a named argument with name @str.
 * If such an argument is found the its position i.e @firts+i is returned. 
 * If no such argument is found then -1 is returned. -2 is returned if 
 * a non optional argument is found on the stack.
 * 
 * Return value: a positive integer in case of sucess and -1 or -2 in case of error.
 **/

static int SearchInOPt(char *str, Stack stack, int first, int nargs,int *wrong_pos)
{
  int i;
  for ( i = 0 ; i < nargs ; i++)
    {
      if ( ! IsHopt(stack.val->S[first+i])) 
	{
	  *wrong_pos += i;
	  return -2 ;
	};
      if ( Ocheckname(stack.val->S[first+i],str)) return first+i;
    }
  return -1;
}

/**
 * 
 * check if used optional named are in the list of expected values 
 *
 **/

static int nsp_check_named(PList Loc,int js,int nargs1, Stack stack, int first, int nargs)
{
  int i,j;
  for ( i = 0 ; i < nargs ; i++)
    {
      if ( IsHopt(stack.val->S[first+i])) 
	{
	  const char *str = NSP_OBJECT(stack.val->S[first+i])->name;
	  PList Loc1,Loc2=Loc;
	  int ok = FALSE;
	  /* Sciprintf("given option: <%s>\n",str);*/
	  for ( j = js ; j  < nargs1 ; j++ ) 
	    {
	      Loc1 = ((PList) Loc2->O)->next;
	      /* Sciprintf("possible option: <%s>\n",(char *) Loc1->O); */
	      if ( strcmp((char *) Loc1->O,str)==0) 
		{
		  ok = TRUE; break;
		}
	      Loc2 = Loc2->next;
	    }
	  if ( ok == FALSE) 
	    {
	      Scierror("Error: named option %s is not recognized\n", str);
	      Loc2=Loc;
	      Scierror("\tpossible options are: ");
	      for ( j = js ; j  < nargs1 ; j++ ) 
		{
		  Loc1 = ((PList) Loc2->O)->next;
		  Scierror("%s",(char *) Loc1->O);
		  Loc2 = Loc2->next;
		  if ( j == nargs1 -1 ) 
		    Scierror(".\n");
		  else
		    Scierror(", ");
		}
	      return FAIL;
	    }
	}
    }
  return OK;
}

/**
 * frame_insert_var:
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * insert nargin, nargout and norgopt in the function local frame 
 * 
 * Return value: #OK or #FAIL.
 **/

static int frame_insert_var(int rhs,int opt,int lhs)
{
  NspObject *O;
#ifdef WITH_SYMB_TABLE
#ifndef SMAT_SYMB_TABLE 
  int nargin=2,nargout=3,nargopt=4;
#else 
  int nargin=-1, nargout=-1, nargopt=-1;
#endif
#else 
  int nargin=-1, nargout=-1, nargopt=-1;
#endif 
  if (( O =nsp_create_object_from_int("nargin",rhs))== NULLOBJ) return FAIL;
  if ( nsp_frame_replace_object(O,nargin) == FAIL) return FAIL;
  if (( O =nsp_create_object_from_int("nargout",lhs))== NULLOBJ) return FAIL;
  if ( nsp_frame_replace_object(O,nargout) == FAIL) return FAIL;
  if (( O =nsp_create_object_from_int("nargopt",opt))== NULLOBJ) return FAIL;
  if ( nsp_frame_replace_object(O,nargopt) == FAIL) return FAIL;
  return OK;
}
