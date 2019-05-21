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
 * evaluation of nsp expression
 *--------------------------------------------------------------------------*/

#include <nsp/nsp.h> 
#include <nsp/plist.h> 
#include <nsp/plistc.h> 
#include <nsp/bhash.h> 
#include <nsp/cells.h> 
#include <nsp/smatrix.h> 
#include <nsp/list.h> 
#include <nsp/file.h> 
#include <nsp/ivect.h> 
#include <nsp/smatrix.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/hobj.h> 
#include <nsp/function.h> 
#include <nsp/imatrix.h> 
#include <nsp/hash.h> 
#include <nsp/plisttoken.h>
#include <nsp/stack.h>
#include <nsp/parse.h>
#include <nsp/system.h> 
#include <nsp/libstab.h>
#include <nsp/seval.h>
#include <nsp/nsptcl.h>
#include <nsp/frame.h> 
#include <nsp/gtksci.h> 
#include <nsp/nspthreads.h> 
#include <nsp/nspdatas.h> 

/* #define USE_GETRUSAGE  */
#ifdef  USE_GETRUSAGE 
#include <sys/time.h>
#include <sys/resource.h>
#endif 

static int EvalEqual (PList L1,Stack stack,int first);
static int EvalOpt (PList L1,Stack stack,int first);
static int EvalFor (PList L1,Stack stack,int first);
static int EvalEqual1 (const char *name,Stack stack,int first,int fargs,int *flag);
static int EvalEqual2 (const char *name,Stack stack,int first,int largs,int fargs,int dot_flag);
static int nsp_eval_object_is_true(Stack *S, NspObject *O, int *err);


typedef struct _obj_check_field obj_check_field ;
struct _obj_check_field {
  NspObject *f;
  NspObject *s;
};

static int EvalLhsList(PList L, int arity, Stack stack, int *ipos, int *r_args_1, int *mlhs_r, int *mlhs_flag,
		       obj_check_field *objs, int *objs_count);

static int EvalRhsList (PList L,Stack, int first, int rhs,int lhs,int display);
static int EvalRhsCall (PList L,Stack, int first, int rhs,int lhs);
static int show_eval_bug(Stack s,int n, PList L) ;

static int nsp_store_result_in_symb_table(int position, const char *str, Stack stack, int first);
static void nsp_set_dollar(Stack *stack,NspObject *O, int j);

#define SHOWBUG(stack,n,L) return show_eval_bug(stack,n,L)

/*
 * This flag is used to prevent search of operators 
 * in calling frames operators overloading are to 
 * be only defined in libraries 
 */

#define INHIBIT_FRAMES_OPERATOR

#ifdef INHIBIT_FRAMES_OPERATOR 
#define nsp_frames_search_op_object(x) NULLOBJ
#else 
#define nsp_frames_search_op_object nsp_frames_search_object 
#endif 

/**
 *nsp_eval:
 * @L1: Expression to be evaluated 
 * @stack: 
 * @first: is the stack indice which can be used 
 *       to store the first argument for L1 evaluation 
 *       ( stack.val->S[first]) 
 * @rhs: gives the number of arguments already present on the 
 *       stack
 * @lhs: gives the number of return arguments expected 
 *       (a value of -1 means that any number of arguments can be returned) 
 * @display: a flag for result display 
 * 
 * Return value: an integer 
 **/

int nsp_eval(PList L1, Stack stack, int first, int rhs, int lhs, int display)
{
  int err;
  int nargs=-1,n=0;
  PList L,loc;
  NspObject *O,*O1;
  PList FC;
  NspPList *F;
  NspIVect *IV;
  const char *s;
  char *fname ; 
  int j,rep;

#define EVENTS 
#ifdef EVENTS 
  static int ecount = 0;
  if ( 0 &&  ecount++ == 1000) 
    {
      // printf("check events\n");
      nsp_check_gtk_events ();
      ecount = 0;
    }
#endif 

  stack.first = first;

#ifdef  USE_GETRUSAGE 
  { struct rusage usage;
  struct rlimit limit;
  getrusage(RUSAGE_SELF, &usage);
  getrlimit (RLIMIT_STACK, &limit);
  printf("Entering nsp_eval with first=%d, maximum resident set size used=%ld, stack limit= %ld and %ld max\n",
    first,usage.ru_maxrss, limit.rlim_cur, limit.rlim_max);
}
#endif 

  /* nsp_check_stack(stack,rhs,0,lhs,"Something wrong with Eval",NULL); */
  L = L1; /* operator */
  L1= L->next ; /* first arg */
  if ( L->type > 0  ) 
    {
      const char *opcode ;
      /* Evaluation of operators */
      switch ( L->arity ) 
	{
	case 0:
	  /* supposed to be only : */
	  if ( L->type != COLON_OP ) 
	    {
	      Scierror("Error: unknown 0-ary operator\n");
	      return RET_BUG;
	    }
	  /* we put an IVect on the stack  */ 
	  if (( IV =nsp_ivect_create(NVOID,0,0,0,1)) == NULLIVECT) return RET_BUG;
	  stack.val->S[first] = (NspObject *) IV;
	  return 1;
	case 1:
	  if ( L->type == RETURN_OP || L->type == SEMICOLON_OP || L->type == COMMA_OP
	       || L->type == SEMICOLON_RET_OP || L->type == COMMA_RET_OP
	       )
	    {
	      if (( nargs =nsp_eval_arg(L1,&stack,first,1,-1,display)) < 0) return nargs;
	      if ( nargs == 0 ) return 0;
	      /* XXXX : Warning, here we can have more that one returned value */
	      /* ex (10,20), */
	      if ( Ocheckname(stack.val->S[first],NVOID))
		{
		  nsp_object_set_name(stack.val->S[first],"ans");
		  if ( nsp_frame_replace_object(stack.val->S[first],-1)==FAIL) 
		    {
		      nsp_object_destroy(&stack.val->S[first]);
		    }
		  
		}
	      if ( display == 1 && L->type != SEMICOLON_OP && L->type != SEMICOLON_RET_OP)
		{
		  opcode =nsp_astcode_to_nickname(L->type);
		  O1=nsp_frames_search_op_object(opcode);
		  if ((n=nsp_eval_func(O1,opcode,2,stack,first,nargs,0,lhs))<0) return n;
		}
	      /* clean the stack : XXXX maybe useless now */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      return 0;
	    }
	  else
	    {
	      if (( nargs  =nsp_eval_arg(L1,&stack,first,1,1,display)) < 0) 
		SHOWBUG(stack,nargs,L1);
	      if ( nargs != 1) 
		{
		  /* too many arguments returned */
		  /* ex:A=1:5; -(A{1:3})*/
		  if ( nargs < 1 ) 
		    Scierror("Error: not enough value (%d) returned as a first argument of unary operator %s\n",
			     nargs,nsp_astcode_to_name(L->type));
		  else 
		    Scierror("Error: too many values (%d) returned as a first argument of unary operator %s\n",
			     nargs,nsp_astcode_to_name(L->type));
		  /* clean the stack */
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,RET_BUG,L1);
		  return RET_BUG;
		}
	      opcode =nsp_astcode_to_nickname(L->type);
	      O1=nsp_frames_search_op_object(opcode);
	      if ((n=nsp_eval_func(O1,opcode,2,stack,first,nargs,0,lhs))<0) 
		SHOWBUG(stack,n,L1);
	      return n;
	    }
	  break;
	case 2:
	  opcode =nsp_astcode_to_nickname(L->type);
	  /*checking eye and ones */
	  if ( L->type == SEQAND || L->type == SEQOR ) 
	    {
	      O1=nsp_frames_search_op_object(opcode);
	      if (( n  =nsp_eval_arg(L1,&stack,first,1,1,display)) < 0 ) SHOWBUG(stack,n,L1);
	      nargs = n;
	      if ( nargs != 1 ) 
		{
		  /* too many arguments returned */
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  /* ex: A=1:5;A{1:3} && 9 */
		  if ( nargs < 1 ) 
		    Scierror("Error: not enough value (%d) returned as a first argument of binary operator %s\n",
			     nargs,nsp_astcode_to_name(L->type));
		  else 
		    Scierror("Error: too many values (%d) returned as a first argument of binary operator %s\n",
			     nargs,nsp_astcode_to_name(L->type));
		  SHOWBUG(stack,RET_BUG,L1);
		  return RET_BUG;
		}
	      if ( IsBMat(stack.val->S[first]) == TRUE )
		{
		  NspBMatrix *B=BMatObj(stack.val->S[first]);
		  if ( B->mn == 1 ) 
		    {
		      if ( L->type == SEQAND && B->B[0] == FALSE ) 
			return nargs;
		      if ( L->type == SEQOR && B->B[0] == TRUE ) 
			return nargs;
		    }
		}
	      /* continue with next argument */
	      if (( n  =nsp_eval_arg(L1->next,&stack,first+nargs,1,1,display)) < 0) 
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L1);
		}
	      if ( n != 1 ) 
		{
		  /* to many argument returned ex:  A=1:5; 9 && A{1:3} */

		  nsp_void_seq_object_destroy(stack,first,first+nargs+n);
		  if ( n < 1 ) 
		    Scierror("Error: not enough value (%d) returned as second argument of binary operator %s\n",
			     n,nsp_astcode_to_name(L->type));
		  else 
		    Scierror("Error: too many values (%d) returned as second argument of binary operator %s\n",
			     n,nsp_astcode_to_name(L->type));
		  SHOWBUG(stack,RET_BUG,L1);
		  return RET_BUG;
		}
	      nargs +=n;
	      /*XXXXX Pas forcement astucieux pour un operateur ? **/
	      if ((n=nsp_eval_func(O1,opcode,2,stack,first,nargs,0,lhs))<0) 
		SHOWBUG(stack,n,L1);
	      return n;
	    }
	  else if ( L->type == DOTPLUS )
	    {
	      /* testing a new mode for operators */
	      if (( n  =nsp_eval_arg(L1,&stack,first,1,1,display)) < 0 ) SHOWBUG(stack,n,L1);
	      nargs = n;
	      if (( n  =nsp_eval_arg(L1->next,&stack,first+nargs,1,1,display)) < 0)
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L1);
		}
	      nargs +=n;
	      if ((n=nsp_eval_dotplus(stack,first,nargs,0,lhs))<0)
		SHOWBUG(stack,n,L1);
	      return n;

	    }
	  else 
	    {
	      /* standard 2 ary operators */
	      if (( n  =nsp_eval_arg(L1,&stack,first,1,1,display)) < 0 ) 
		SHOWBUG(stack,n,L1);
	      nargs = n;
	      if ( nargs != 1 ) 
		{
		  /* too many arguments returned */
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  /* ex: A=1:5;A{1:3} op 9 */
		  Scierror("Error: too many values (%d) returned as a first argument of binary operator %s\n",nargs,nsp_astcode_to_name(L->type));
		  SHOWBUG(stack,RET_BUG,L1);
		  return RET_BUG;
		}
	      if (( n  =nsp_eval_arg(L1->next,&stack,first+nargs,1,1,display)) < 0) 
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L1->next);
		}
	      if ( n != 1 ) 
		{
		  /* to many argument returned ex:  A=1:5; 9 && A{1:3} */
		  nsp_void_seq_object_destroy(stack,first,first+nargs+n);
		  Scierror("Error: too many values (%d) returned as second argument of binary operator %s\n",n,nsp_astcode_to_name(L->type));
		  SHOWBUG(stack,RET_BUG,L1);
		  return RET_BUG;
		}
	      nargs +=n;
	      if ((n=nsp_eval_maybe_accelerated_binop(opcode, L->type, stack, first, nargs, 0, lhs))<0)
		SHOWBUG(stack,n,L);
	      return n;

	    }
	  break;
	default :
	  opcode =nsp_astcode_to_nickname(L->type);
	  loc = L1;
	  O1=nsp_frames_search_op_object(opcode);
	  nargs=0;
	  for ( j = L->arity ; j > 0  ; j--)
	    {
	      if ((n =nsp_eval_arg(loc,&stack,first+nargs,1,1,display)) < 0 )
		{
		  /* cleaning */
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L1);
		}
	      nargs += n;
	      loc = loc->next ;
	    }
	  if ((n=nsp_eval_func(O1,opcode,2,stack,first,nargs,0,lhs))<0) SHOWBUG(stack,n,L1);
	  return n;
	  break;
	}
    }
  else 
    {
      switch ( L->type ) 
	{
	case OPT :
	  if (( n = EvalOpt(L1,stack,first)) < 0) SHOWBUG(stack,n,L1);
	  return n;
	  break;
	case EQUAL_OP:
	  if (( n = EvalEqual(L1,stack,first)) < 0) SHOWBUG(stack,n,L1);
	  return n;
	  break;
	case MLHS  :
	  /*we never get there MLHS is evaluated elsewhere **/
	  Scierror("Error: Something Strange ...\n");
	  return RET_BUG;
	  break;
	case FEVAL :
	  /*we never get there FEVAL is evaluated elsewhere **/
	  Scierror("Error: Something Strange <FEVAL>...\n");
	  return RET_BUG;
	  break;
	case ARGS :
	case METARGS :
	case DOTARGS :
	case CELLARGS :
	  /*Evaluation of a set of Args XXXX : could be performed elsewhere **/
	  /* Scierror("Le cas DOTARGS XXXXX  %d\n",DOTARGS== L->type); */
	  loc = L1;
	  nargs=0;
	  /*used to follow a path for list extraction or insertion **/
	  for ( j = 1 ; j <= L->arity ;  j++ )
	    {
	      if ((n =nsp_eval_arg(loc,&stack,first+nargs,1,1,display)) < 0)  
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,loc);
		}

	      loc = loc->next ;
	      nargs += n;
	    }
	  return nargs;
	  break;
	case CALLEVAL : 
	  if (( n = EvalRhsCall(L,stack,first,rhs,lhs))< 0) 
	    {
	      SHOWBUG(stack,n,L);
	    }
	  return n;
	  break;
	case LISTEVAL :
	  if (( n = EvalRhsList(L,stack,first,rhs,lhs,display))< 0) 
	    {
	      SHOWBUG(stack,n,L);
	    }
	  return n;
	  break;
	  
	case PLIST :
	  if (L->next == NULLPLIST )
	    {
	      if ((nargs=nsp_eval_arg(L,&stack,first,1,1,display)) < 0) 
		SHOWBUG(stack,nargs,L1);
	      return nargs;
	    }
	  return 0;
	  break;
	case COMMENT : 
	  return 0;
	  break;
	case NAME :
	case OPNAME :
	case NUMBER:
	case INUMBER32 :
	case INUMBER64 :
	case UNUMBER32 :
	case UNUMBER64 :
	case STRING:
	case EMPTYMAT:
	case EMPTYCELL:
	  nargs=nsp_eval_arg(L,&stack,first,1,1,display);
	  break;
	case P_MATRIX :
	case P_CELL :
	  return (nargs=nsp_eval_arg(L1,&stack,first,1,1,display));
	  break;
	case CELLDIAGCONCAT:
	case CELLROWCONCAT:
	case CELLCOLCONCAT:
	  /* 
	   * a cell is always (P_CELL (CELLDIAGCONCAT arg1 ... argn ))
	   *                   argi = (CELLROWCONCAT carg1 .... cargn))
	   *                   cargi =(CELLCOLCONCAT exp1 .... expn) 
	   * Thus CELLROWCONCAT and CELLDIAGCONCAT could be accelerated 
	   * when they are of arity one since they do nothing in that case. 
	   */
	  loc = L1;
	  fname = ( L->type == CELLCOLCONCAT) 
	    ? "col_cells_create": (( L->type == CELLROWCONCAT) 
				   ? "row_cells_create": "diag_cells_create"); 
	  O1=nsp_frames_search_op_object(fname);
	  nargs = 0;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      /* evaluate arguments 
	       * we accept n to be != 1 
	       */
	      if ((n =nsp_eval_arg(loc,&stack,first+nargs,1,1,display)) <0 ) 
		{
		  /* clean and return */
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L1);
		}
	      nargs += n;
	      loc= loc->next;
	    }
	  /* now we have nargs arguments : we must create a cell */
	  if (( n =nsp_eval_func(O1,fname,2,stack,first,nargs,0,lhs)) < 0) SHOWBUG(stack,n,L);
	  return n;
	  break;
#ifdef NSP_PARSE_MATRIX_AS_CELLS 
	  /* XXXX en test si le parsing des matrices est remis comme celui des cells */
	case DIAGCONCAT:
	case ROWCONCAT:
	case COLCONCAT:
	  /* when matrix are parsed like cells */
	  /* 
	   * a matrix is always (P_MATRIX (DIAGCONCAT arg1 ... argn ))
	   *                   argi = (ROWCONCAT carg1 .... cargn))
	   *                   cargi =(COLCONCAT exp1 .... expn) 
	   * Thus ROWCONCAT and DIAGCONCAT could be accelerated 
	   * when they are of arity one since they do nothing in that case. 
	   */
	  loc = L1;
	  nargs = 0;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ((n =nsp_eval_arg(loc,&stack,first+nargs,1,1,display)) <0 ) 
		{
		  /* clean and return */
		  nsp_void_seq_object_destroy(stack,first,first+nargs+n);
		  SHOWBUG(stack,n,L1);
		}
	      if ( n > 1)
		{
		  /* we accept n== 0 since some elements may be just comments */
		  Scierror("Error: evaluation of argument %d in a matrix concatenation ``%s'' returned %d arguments while expecting one\n",
			   j+1,nsp_astcode_to_name(L->type),n);
		  /* clean and return */
		  nsp_void_seq_object_destroy(stack,first,first+nargs+n);
		  SHOWBUG(stack,RET_BUG,L1);
		}
	      nargs += n;
	      if ( nargs == 2 ) 
		{
		  switch ( L->type ) 
		    {
		    case ROWCONCAT:
		      if ( (n =nsp_eval_maybe_accelerated_op("concatd",2,concatd_tab, stack,first,nargs,0,lhs)) < 0 ) 
			{
			  SHOWBUG(stack,n,L1);
			}
		      break;
		    case COLCONCAT:
		      if ( (n =nsp_eval_maybe_accelerated_op("concatr",2,concatr_tab, stack,first,nargs,0,lhs)) < 0 ) 
			{
			  SHOWBUG(stack,n,L1);
			}
		      break;
		    case DIAGCONCAT:
		      O1=nsp_frames_search_op_object("concatdiag");
		      if ((n=nsp_eval_func(O1,"concatdiag",2,stack,first,nargs,0,lhs)) < 0) 
			{
			  SHOWBUG(stack,n,L);
			}
		    }
		  nargs = 1;
		}
	      loc= loc->next;
	    }
	  return nargs;
	  break;
#else 
	case ROWCONCAT:
	  if ((nargs =nsp_eval_arg(L1,&stack,first,1,1,display)) <0 ) SHOWBUG(stack,nargs,L1);
	  if ( nargs > 1 ) 
	    {
	      Scierror("Error: too many values (%d) returned as a first argument of row concatenation %s\n",
		       nargs,nsp_astcode_to_name(L->type));
	      /* clean the stack */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      SHOWBUG(stack,RET_BUG,L1);
	      return RET_BUG;
	    }
	  if ((n =nsp_eval_arg(L1->next,&stack,first+nargs,1,1,display)) < 0) 
	    {
	      /* clean first part */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      SHOWBUG(stack,n,L1);
	    }
	  if ( n > 1 ) 
	    {
	      Scierror("Error: too many values (%d) returned as a second argument of row concatenation %s\n",
		       n,nsp_astcode_to_name(L->type));
	      /* clean the stack */
	      nsp_void_seq_object_destroy(stack,first,first+nargs+n);
	      SHOWBUG(stack,RET_BUG,L1);
	      return RET_BUG;
	    }
	  nargs += n;
	  /* Note here that nargs can be equal to one if one of the argument was a comment */
	  if ( nargs < 2 ) return nargs;
	  if ( (n =nsp_eval_maybe_accelerated_op("concatd",2,concatd_tab, stack,first,nargs,0,lhs)) < 0 ) 
	    SHOWBUG(stack,n,L);
	  return n;
	  break;
	case COLCONCAT:
	  if ((nargs =nsp_eval_arg(L1,&stack,first,1,1,display)) <0 )  SHOWBUG(stack,nargs,L1);
	  if ( nargs > 1 ) 
	    {
	      Scierror("Error: too many values (%d) returned as a first argument of column concatenation %s\n",
		       nargs,nsp_astcode_to_name(L->type));
	      /* clean the stack */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      SHOWBUG(stack,RET_BUG,L1);
	      return RET_BUG;
	    }
	  if ((n =nsp_eval_arg(L1->next,&stack,first+nargs,1,1,display)) < 0)  
	    {
	      /* clean first part */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      SHOWBUG(stack,n,L1);
	    }
	  if ( n > 1 ) 
	    {
	      Scierror("Error: too many values (%d) returned as a second argument of column concatenation %s\n",
		       n,nsp_astcode_to_name(L->type));
	      /* clean the stack */
	      nsp_void_seq_object_destroy(stack,first,first+nargs+n);
	      SHOWBUG(stack,RET_BUG,L1);
	      return RET_BUG;
	    }
	  nargs += n;
	  /* Note here that nargs can be equal to one if one of the argument was a comment */
	  if ( nargs < 2 ) return nargs;
	  if ( (n =nsp_eval_maybe_accelerated_op("concatr",2,concatr_tab, stack,first,nargs,0,lhs)) < 0 ) 
	    SHOWBUG(stack,n,L);
	  return n;
	  break;

	case DIAGCONCAT:
	  O1=nsp_frames_search_op_object("concatdiag");
	  if ((nargs =nsp_eval_arg(L1,&stack,first,1,1,display)) <0 ) SHOWBUG(stack,nargs,L1);
	  if ( nargs > 1 ) 
	    {
	      Scierror("Error: too many values (%d) returned as a first argument of diag concatenation %s\n",
		       nargs,nsp_astcode_to_name(L->type));
	      /* clean the stack */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      SHOWBUG(stack,RET_BUG,L1);
	      return RET_BUG;
	    }
	  if ((n =nsp_eval_arg(L1->next,&stack,first+nargs,1,1,display)) < 0) 
	    {
	      /* clean first part */
	      nsp_void_seq_object_destroy(stack,first,first+nargs);
	      SHOWBUG(stack,n,L1);
	    }
	  if ( n > 1 ) 
	    {
	      Scierror("Error: too many values (%d) returned as a second argument of diag concatenation %s\n",
		       n,nsp_astcode_to_name(L->type));
	      /* clean the stack */
	      nsp_void_seq_object_destroy(stack,first,first+nargs+n);
	      SHOWBUG(stack,RET_BUG,L1);
	      return RET_BUG;
	    }
	  nargs += n;
	  /* Note here that nargs can be equal to one if one of the argument was a comment */
	  if ( nargs < 2 ) return nargs;
	  if ((n=nsp_eval_func(O1,"concatdiag",2,stack,first,nargs,0,lhs)) < 0) SHOWBUG(stack,n,L);
	  return n;
	  break;
#endif
	case WHILE:
	  while (1) 
	    {
	      int rep;
	      /* swallow potential comments */
	      /* while (L1->type == COMMENT ) L1 = L1->next; */
	      /* if (L1->type == COMMENT ) Scierror("L1 is a comment \n"); */
	      nargs=nsp_eval_arg(L1,&stack,first,1,1,display);
	      if ( nargs <= 0 ) 
		{
		  if ( nargs == 0 ) 
		    {
		      Scierror("while statement with a test which returns no value\n");
		      nargs = RET_BUG;
		    }
		  
		  SHOWBUG(stack,nargs,L1);
		}
	      rep =nsp_eval_object_is_true(&stack,stack.val->S[first],&err);
	      nsp_void_object_destroy(&stack.val->S[first]);
	      stack.val->S[first]= NULLOBJ;
	      if ( err == TRUE) SHOWBUG(stack,RET_BUG,(PList) L1->O); /* just print the condition */
	      if ( rep == FALSE ) break;
	      nargs=nsp_eval_arg(L1->next,&stack,first,1,1,display);
	      if ( nargs < 0 ) 
		{
		  if ( nargs == RET_BREAK ) break;
		  else if ( nargs == RET_CONTINUE ) continue;
		  else SHOWBUG(stack,nargs,L1);
		}
	      else if ( nargs > 0)  
		{
		  Scierror("Strange: a while body evaluation returns %d variables\n",nargs);
		}
	    }
	  return 0;
	  break;
	case FUNCTION:
	  /* We store the parsed function on the stack 
	   * L1 is copied since it is destroyed after evaluation 
	   */
	  if ((FC=nsp_plist_copy(L)) == NULLPLIST ) return RET_BUG;
	  if (( F = NspPListCreate(nsp_function_name(FC),FC,NspFileName(stack))) == NULLP_PLIST) 
	    return RET_BUG;
	  O = (NspObject *) F;
	  stack.val->S[first] = O;
	  if ( nsp_frame_replace_object(O,-1)==FAIL) 
	    {
	      nsp_object_destroy(&stack.val->S[first]);
	      SHOWBUG(stack,RET_BUG,L1);
	    }
	  return 1;
	  break;
	case FOR:
	  return EvalFor(L1,stack,first);
	case IF :
	  /*a sequence of if elseif etc.... */
	  for ( j = 0 ; j < L->arity  ; j += 2 )
	    {
	      if ( j == L->arity-1 ) 
		{
		  /*we have reached the last else **/
		  if ((nargs=nsp_eval_arg(L1,&stack,first,1,1,display))  < 0) SHOWBUG(stack,nargs,L1);
		  if ( nargs > 0)  
		    {
		      fprintf(stderr,"XXXStrange: last if evaluation returns  %d \n",nargs);
		    }
		  return 0;
		}
	      else 
		{ 
		  int iftest;
		  /* swallow potential comments */
		  /* while (L1->type == COMMENT ) L1 = L1->next; */
		  nargs=nsp_eval_arg(L1,&stack,first,0,1,display);
		  if ( nargs != 1 ) 
		    {
		      if ( nargs > 1 ) 
			{
			  /* to many argument returned */
			  nsp_void_seq_object_destroy(stack,first,first+nargs);
			  Scierror("Error: too many values (%d) returned as an if condition\n",nargs);
			  SHOWBUG(stack,nargs,L1);
			  return RET_BUG;
			}
		      else if ( nargs <= 0 ) 
			{
			  if ( nargs == 0 ) 
			    {
			      nargs = RET_BUG;
			      Scierror("if statement with a test which returns no value\n");
			    }
			  SHOWBUG(stack,nargs,L1);
			}
		    }
		  iftest =nsp_eval_object_is_true(&stack,stack.val->S[first],&err);
		  nsp_void_object_destroy(&stack.val->S[first]);
		  stack.val->S[first]= NULLOBJ;
		  if ( err == TRUE) SHOWBUG(stack,RET_BUG,(PList) L1->O); /* just print the condition */
		  if ( iftest == TRUE )
		    {
		      if ((nargs=nsp_eval_arg(L1->next,&stack,first,1,1,display))  < 0) SHOWBUG(stack,nargs,L1);
		      if ( nargs > 0)  
			{
			  fprintf(stderr,"XXXStrange: if branch returns %d \n",nargs);
			}
		      return 0;
		    }
		  L1 = L1->next->next;
		}
	    }
	  /* cleaning if necessary */
	  /* XXX nsp_void_object_destroy(&stack.val->S[first]);
	     stack.val->S[first]=NULLOBJ;
	  */
	  /* 	  nsp_check_stack(stack,0,0,lhs,"Something wrong end of If ",NULL); */
	  return 0;
	  break;
	case TRYCATCH :
	  {
	    int return_ = 0;
	    /* try/catch/finally */
	    /* evaluates the try */
	    int errcatch_cur = stack.val->errcatch;
	    int pause_cur = stack.val->pause;
	    stack.val->errcatch = TRUE;
	    stack.val->pause = TRUE;
	    nargs=nsp_eval_arg(L1,&stack,first,0,1,display);
	    stack.val->errcatch = errcatch_cur;
	    stack.val->pause = pause_cur;
	    L1 = L1->next ;
	    if ( nargs > 0)  
	      {
		fprintf(stderr,"strange: try evaluation returns %d\n",nargs);
	      }
	    if ( nargs < 0 ) 
	      {
		if ( nargs == RET_RETURN )
		  {
		    return_ = RET_RETURN;
		  }
		else 
		  {
		    /* push the error message */
		    nsp_error_message_to_lasterror();
		    /* evaluates the catch */
		    nargs=nsp_eval_arg(L1,&stack,first,0,1,display);
		    if ( nargs == RET_RETURN || nargs == RET_ERROR_RAISED)
		      {
			return_ = nargs ;
		      }
		    else if ( nargs < 0 ) 
		      {
			SHOWBUG(stack,nargs,L1);
			return nargs;
		      }
		  }
	      }
	    if ( L->arity == 3 ) 
	      {
		/* evaluates the finally even 
		 * if try/catch stopped on return
		 */
		L1 = L1->next ;
		nargs=nsp_eval_arg(L1,&stack,first,0,1,display);
		if ( nargs < 0 ) 
		  {
		    if ( nargs != RET_RETURN && nargs != RET_ERROR_RAISED) 
		      SHOWBUG(stack,nargs,L1);
		    return nargs ;
		  }
	      }
	    return return_; /* try/catch with return or error_raised */
	  }
	  break;
	case SELECT :
	  /*arity N. 
	   *  First argument is the test other arguments are 
	   *  the cases
	   **/
	  /* we first evaluate the test SHOWBUG ? */ 
	  nargs =nsp_eval_arg(L1,&stack,first,0,1,display);
	  if ( nargs != 1 ) 
	    {
	      if ( nargs > 1 ) 
		{
		  /* to many argument returned */
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  Scierror("Error: too many values (%d) returned as a select condition\n",nargs);
		  SHOWBUG(stack,RET_BUG,L1);
		}
	      else if ( nargs == 0 ) 
		{
		  Scierror("select expression which evaluation returns no value\n");
		  SHOWBUG(stack,RET_BUG,L1);
		}
	      else
		{
		  /* there was an error in the select evaluation */
		  SHOWBUG(stack,nargs,L1);
		}
	    }
	  /* we need here to copy the object since it can be 
	   * changed in select evaluations 
	   */
	  if ( Ocheckname(stack.val->S[first],NVOID))
	    {
	      nsp_object_set_name(stack.val->S[first],"#");
	    }
	  else 
	    {
	      if ((O =nsp_object_copy_and_name("#",stack.val->S[first])) == NULLOBJ )
		{
		  SHOWBUG(stack,RET_BUG,L1);
		}
	      stack.val->S[first] = O;    
	    }
	  L1 = L1->next;
	  /* now we evaluate all the cases the test is kept at position first */
	  for ( j = 1 ; j < L->arity ; j++)
	    {
	      int rep;
	      /* evaluate a case, the case test is returned 
	       * in stack.val->S[first+1]
	       */
	      nargs=nsp_eval_arg(L1,&stack,first+1,1,1,display); 
	      if ( nargs <= 0 ) 
		{
		  /* error occured */
		  nsp_void_object_destroy(&stack.val->S[first+1]);
		  stack.val->S[first+1]=NULLOBJ;
		  break;  
		}
	      rep = nsp_eval_object_is_true(&stack,stack.val->S[first+1],&err); 
	      /* clean the select == case test */ 
	      nsp_void_object_destroy(&stack.val->S[first+1]);
	      stack.val->S[first+1]=NULLOBJ;
	      if ( err == TRUE) SHOWBUG(stack,RET_BUG,(PList) L1->O); /* just print the condition */
	      /* stop if case was true */
	      if ( rep == TRUE ) break;
	      /* go on */
	      L1 = L1->next; 
	    }
	  /* clean select statement and the select==case value  */
	  if (stack.val->S[first]!= NULLOBJ )
	    {
	      if (Ocheckname(stack.val->S[first],"#"))
		{
		  nsp_object_destroy(&stack.val->S[first]);
		  stack.val->S[first]=NULLOBJ;
		}
	      else 
		{
		  nsp_void_object_destroy(&stack.val->S[first]);
		  stack.val->S[first]=NULLOBJ;
		}
	    }
	  if ( nargs < 0 ) return nargs ; 
	  nargs = 0;
	  break;
	case STATEMENTS :
	case STATEMENTS1 :
	  /*ici lhs n'est pas utilise XXX **/
	  nargs = 0;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( (nargs=nsp_eval_arg(L1,&stack,first,1,-1,display)) < 0) 
		{
		  return nargs;
		}
	      L1 = L1->next;
	    }
	  break;
	  /*On pourrait ici controler l'adequation entre lhs et nargs ? XXXX */
	  return 0;
	case PARENTH :
	  nargs=0;
	  for ( j = 0 ; j < L->arity ; j++)
	    {
	      if ( (n=nsp_eval_arg(L1,&stack,first+nargs,1,-1,display)) < 0) 
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  return n;
		}
	      nargs += n;
	      L1 = L1->next;
	    }
	  /*On pourrait ici controler l'adequation entre lhs et nargs ? XXXX */
	  return nargs;
	  break;
	case CASE :
	  {
	    /* adapted to compare objects of different types (bruno, march,2,2009) */
	    /* the select expression */ 
	    NspObject *C = NULLOBJ, *SelectObj = stack.val->S[first-1];
	    HOBJ_GET_OBJECT(SelectObj,RET_BUG);
	    /* first evaluate the case expression */ 
	    if (L1->type == COMMENT ) 
	      {
		/* a case expression which is just a comment */
		nsp_void_object_destroy(&stack.val->S[first]);
		/* create a false object and places at first pos in stack */
		if ( (stack.val->S[first] = nsp_create_false_object(NVOID)) == NULLOBJ )
		  {
		    SHOWBUG(stack,nargs,L1);   /* a voir... */
		  }
		return 1; 
	      }
	    nargs=nsp_eval_arg(L1,&stack,first,1,1,display);
	    if ( nargs != 1 ) 
	      {
		if ( nargs > 1 ) 
		  {
		    /* to many argument returned */
		    nsp_void_seq_object_destroy(stack,first,first+nargs);
		    Scierror("Error: too many values (%d) returned as a case condition evaluation\n",nargs);
		    SHOWBUG(stack,RET_BUG,L1);
		  }
		else if ( nargs == 0 ) 
		  {
		    Scierror("Error: case expression which evaluation returns no value\n");
		    SHOWBUG(stack,RET_BUG,L1);
		  }
		else
		  SHOWBUG(stack,nargs,L1);
	      }
	    C = stack.val->S[first];
	    HOBJ_GET_OBJECT(C,RET_BUG);
	    
	    /* now the value to be compared to case is at position first-1 (and pointed by SelectObj) and 
	     * the case expression is at position first (and pointed by C)
	     */
	    
	    if (IsCells(C))
	      {
		/* when case expression is an array of cells  {val1,....,valn} */
		int ic,ok =FALSE;
		/* the test is to be done looping on elements of the array of cells */
		for ( ic = 0 ; ic < ((NspCells *) C)->mn; ic++)
		  {
		    NspObject *Current = ((NspCells *) C)->objs[ic];

		    if ( Current != NULLOBJ  &&  SelectObj->type->eq(SelectObj, Current) )
		      {
			/* destroy the case value if needed */
			nsp_void_object_destroy(&stack.val->S[first]);
			/* create a true object and places at first pos in stack */
			if ( (stack.val->S[first] = nsp_create_true_object(NVOID)) == NULLOBJ )
			  {
			    SHOWBUG(stack,nargs,L1);   /* a voir... */
			  }
			ok = TRUE;
			break;
		      }
		  }
		/* all the comparisons have led to false : we return */
		if ( ok == FALSE ) 
		  {
		    /* destroy the case value if needed */
		    nsp_void_object_destroy(&stack.val->S[first]);
		    /* create a false object and places at first pos in stack */
		    if ( (stack.val->S[first] = nsp_create_false_object(NVOID)) == NULLOBJ )
		      {
			SHOWBUG(stack,nargs,L1);   /* a voir... */
		      }
		    return 1; 
		  }
	      }
	    else 
	      {
		/* when case expression is not an array of cells */
		if ( SelectObj->type->eq(SelectObj, C) )
		  {
		    /* destroy the case value if needed */
		    nsp_void_object_destroy(&stack.val->S[first]);
		    /* create a true object and places at first pos in stack */
		    if ( (stack.val->S[first] = nsp_create_true_object(NVOID)) == NULLOBJ )
		      {
			SHOWBUG(stack,nargs,L1);   /* a voir... */
		      }
		  }
		else
		  {
		    /* destroy the case value if needed */
		    nsp_void_object_destroy(&stack.val->S[first]);
		    /* create a false object and places at first pos in stack */
		    if ( (stack.val->S[first] = nsp_create_false_object(NVOID)) == NULLOBJ )
		      {
			SHOWBUG(stack,nargs,L1);   /* a voir... */
		      }
		    return 1; 
		  }
	      }
	  }
	  /* evaluate the then part if OK  */
	  nargs=nsp_eval_arg(L1->next,&stack,first+1,1,1,display);
	  if ( nargs >= 0 ) nargs +=1;
	  return nargs;
	  break;
	case LASTCASE :
	  nargs=nsp_eval_arg(L1,&stack,first,1,1,display);
	  if ( nargs > 0)  
	    {
	      fprintf(stderr,"XXXStrange: the last select branch returns %d \n",nargs);
	    }
	  return nargs;
	  break;
	case PAUSE:  
	  /* 1-ary pause 
	   */
	  if ( stack.val->pause == TRUE ) 
	    {
	      Sciprintf("pause [%s]",(char *) L1->O);
	      if ( NspFileName(stack) != NULL) 
		{
		  int line= nsp_parser_get_line(L1);
		  if ( line != -1 ) 
		    Sciprintf(" at line %d of file %s", line,NspFileName(stack));
		}
	      Sciprintf("\n");
	      /* We enter a new scilab evaluation loop  */
	      inc_pause_prompt();
	      rep =nsp_parse_eval_from_std(1);
	      dec_pause_prompt();
	      return rep;
	    }
	  else 
	    {
	      /* explicit pause considered as RET_BUG 
	       * in this case we are are in a pausecatch=%t
	       */
	      Scierror("Error: Catching a pause\n");
	      return RET_BUG;
	    }
	  break;
	case CLEAR:  
	  /* n-ary clear n>= 1 */
	  loc = L1;
	  for ( j = 1 ; j <= L->arity ;  j++ )
	    {
	      nsp_frame_remove_object((char *) loc->O);
	      loc = loc->next ;
	    }
	  return 0;
	  break;
	case CLEARGLOBAL:  
	  /* n-ary clearglobal n>= 1 */
	  loc = L1;
	  for ( j = 1 ; j <= L->arity ;  j++ )
	    {
	      nsp_global_frame_remove_object((char *) loc->O);
	      loc = loc->next ;
	    }
	  return 0;
	  break;
	case HELP  : 
	  /* 1-ary help */
	  nsp_help_browser(NULL,NULL,(char *) L1->O);
	  return 0;
	  break;
	case NSP_EXIT :
	  /* 1-ary exit */
	  {
	    char *endptr;
	    const char *str = (char *) L1->O;
	    gint64 val=0;
	    val = g_ascii_strtoll(str, &endptr, 10 );
	    /* Check for various possible errors */
	    if ((val == G_MAXINT64 || val == G_MININT64 )
		|| (val == 0 && endptr == str)  /* no digits in input */
		)
	      {
		Scierror("Error: exit called with wrong argument");
		val = 1;
	      }
	    sci_clear_and_exit(val);
	  }
	  return 0;
	  break;
	case WHO :
	  /* 1-ary who */
	  nsp_who(&stack,(const char *) L1->O,FALSE,TRUE,&rep);
	  if (rep == TRUE ) return RET_BUG;
	  return 0;
	  break;
	case CD_COMMAND :
	  /* 1-ary CD */
	  if ( nsp_chdir_and_update_exec_dir(&stack,(char *) L1->O) == FAIL) 
	    return RET_BUG;
	  return 0;
	  break;
	case LS_COMMAND :
	  /* 1-ary LS */
	  nsp_glob((char *) L1->O);
	  return 0;
	  break;
	case PWD_COMMAND :
	  /* 1-ary PWD */
	  Scierror("Error: pwd should be used without arguments\n");
	  return RET_BUG;
	  break;
	case GLOBAL:
	  /* n-ary global */
	  loc = L1;
	  for ( j = 1 ; j <= L->arity ;  j++ )
	    {
	      if (nsp_declare_global((char *) loc->O,loc->arity, NULLOBJ)== FAIL) return RET_BUG;
	      loc = loc->next ;
	    }
	  return 0;
	  break;
	case EXEC: 
	  {
	    int rep;
	    char old[FSIZE+1], fname_expanded[FSIZE+1];
	    nsp_expand_file_and_update_exec_dir(&stack,old,(char *) L1->O,fname_expanded);
	    /* 1-ary exec  */
	    rep =nsp_parse_eval_file(fname_expanded,FALSE,FALSE,FALSE,stack.val->pause,FALSE);
	    nsp_reset_exec_dir(&stack,old);
	    return rep;
	  }
	  break;
	case APROPOS:
	  /* 1-ary apropos */
	  Sciprintf("apropos [%s]\n",(char *) L1->O);
	  return 0;
	  break;
	default:
	  Sciprintf("Oooops  in nsp_eval: Please send a bug report\n");
	  s=nsp_astcode_to_name(L->type);
	  if ( s != (char *) 0) Sciprintf(" %s ",s);
	  return RET_BUG;
	}
    }
  /* nsp_check_stack(stack,nargs,0,-1,"Something wrong end of Eval",NULL); */
  return nargs ;
}



/**
 *nsp_eval_arg:
 * @L: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @lhs: 
 * @display: 
 * 
 * Evaluates arguments in a PList 
 *   Plist = ( arg1 .... argn [op arity]) 
 * 
 * Return value: 
 **/

 
int nsp_eval_arg(PList L, Stack *stack, int first, int rhs, int lhs, int display)
{
  nsp_datas *data = nsp_get_datas();
  int Int,Num,rep;
  if ( L == NULLPLIST ) 
    {
      Scierror("Error: Something Strange: () found\n");
      return RET_BUG;
    }
  switch (L->type) 
    {
    case COMMENT :
      return 0;
      break;
    case NAME :
    case OPNAME :
      /* if ( debug ) Sciprintf("==>%s\n",(char *) L->O); */
      
      if ( strcmp((char *) L->O,"$")==0)
	{
	  NspMatrix *dollar;
	  if ( stack->dollar == -1 ) 
	    {
	      Scierror("Error: trying to use $ in a context where $ do not exists\n");
	      return RET_BUG;
	    }
	  if (( dollar = nsp_matrix_create(NVOID,'r',1,1) ) == NULLMAT )
	    {
	      return RET_BUG;
	    }
	  dollar->R[0] = stack->dollar;
	  stack->val->S[first] = NSP_OBJECT(dollar); 
	  return 1;
	}

      if ( L->arity != -1 ) 
	{
	  int tag = VAR_IS_PERSISTENT(L->arity) ? 2 : 1;
	  if ( data->L == NULLLIST ) 
	    {
	      Scierror("Error: Can't find local variable in empty Data frame\n");
	      return RET_BUG;
	    }
	  /* get current frame local variable table */
	  stack->val->S[first] = ((NspCells*) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(L->arity)];
	  if (stack->val->S[first]== NULL) 
	    {
	      /* maybe the local variable has a value in calling stacks */
	      stack->val->S[first]=nsp_frames_search_local_in_calling((char *) L->O, FALSE );
	    }
	  if (stack->val->S[first]== NULL) 
	    {
	      Scierror("Warning: local variable %s id=%d not found \n",(char *) L->O ,L->arity);
	      return RET_BUG;
	    }
	  return 1;
	}

      if ((stack->val->S[first] =nsp_frames_search_object((char *) L->O )) != NULLOBJ) 
	{
	  /* FIXME: should be limited just to current frame 
	   * a symbol in frames 
	   */
	  /* Extra warning  */
	  /* 	  if ( nsp_find_function((char *) L->O,&Int,&Num) == OK || nsp_find_macro((char *) L->O) != NULLOBJ) */
	  /* 	    { */
	  /* 	      Sciprintf("Warning: frame variable %s is hiding a function\n",(char *) L->O ); */
	  /* 	    } */
	  return 1;
	}
      else if ( nsp_find_function((char *) L->O,&Int,&Num) == OK) 
	{
	  /* check if name is a function **/
	  if ((stack->val->S[first]= (NspObject *) function_create(NVOID,(char *) L->O,Int,Num,-1,NULL))==  NULLOBJ) 
	    return RET_BUG;
	  return 1;
	}
      else if ( (stack->val->S[first]=nsp_find_macro((char *) L->O)) != NULLOBJ) 
	{
	  /* check for a macro */
	  return 1;
	}
      else
	{
	  Scierror("Error: unknown variable %s\n",(char*)L->O);
	  return RET_BUG;
	}
    case NUMBER:
      /* if (debug) Sciprintf("==>%s\n",((parse_double *) L->O)->str); */
      if ((stack->val->S[first] = nsp_create_object_from_double(NVOID,((parse_double *) L->O)->val)) == NULLOBJ) 
	return RET_BUG;
      return 1;
    case INUMBER32 :
      if ((stack->val->S[first] = nsp_create_object_from_tint(NVOID,(nsp_int_union) ((parse_int *) L->O)->Gint32,
							      nsp_gint32)) == NULLOBJ) return RET_BUG;
      return 1;
    case INUMBER64 :
      if ((stack->val->S[first] = nsp_create_object_from_tint(NVOID,(nsp_int_union)((parse_int *) L->O)->Gint64,
							      nsp_gint64)) == NULLOBJ) return RET_BUG;
      return 1;
    case UNUMBER32 :
      if ((stack->val->S[first] = nsp_create_object_from_tint(NVOID,(nsp_int_union)((parse_int *) L->O)->Guint32,
							      nsp_guint32)) == NULLOBJ)return RET_BUG;
      return 1;
    case UNUMBER64 :
      if ((stack->val->S[first] = nsp_create_object_from_tint(NVOID,(nsp_int_union)((parse_int *) L->O)->Guint64,
							      nsp_guint64)) == NULLOBJ)return RET_BUG;
      return 1;
      break;
    case STRING:
      /*       if (debug) Sciprintf("==>\"%s\"\n",(char *) L->O); */
      if ((stack->val->S[first]=nsp_create_object_from_str(NVOID,(char *) L->O)) == NULLOBJ) return RET_BUG;
      return 1;
      break;
    case EMPTYMAT:
      /* if (debug) Sciprintf("==> {}");*/
      if ( (stack->val->S[first]=nsp_create_empty_matrix_object(NVOID)) == NULLOBJ) return RET_BUG;
      return 1;
    case EMPTYCELL:
      /* if (debug) Sciprintf("==> []"); */
      if ( (stack->val->S[first]=((NspObject *) nsp_cells_create(NVOID,0,0)))  == NULLOBJ) return RET_BUG;
      return 1;
      break;
    case BREAK:
      /* if (debug) Sciprintf("==>send a  break\n");  */
      return RET_BREAK;
      break;
    case PLIST :
      return (nsp_eval((PList) L->O,*stack,first,rhs,lhs,display));
      break;
    case PRETURN: 
      /* if (debug) Sciprintf("==>send a return \n"); */
      return RET_RETURN;
      break;
    case QUIT: 
      return RET_QUIT;
      break;
    case PAUSE :
      /* 0-ary pause */
      /* We enter a new scilab evaluation loop 
       * Need to deal with abort FIXME 
       */
      if ( stack->val->pause == TRUE ) 
	{
	  if ( stack->file_name  != NULL) 
	    {
	      int line= nsp_parser_get_line(L);
	      if ( line != -1 ) 
		Sciprintf("pause at line %d of file %s\n", line,stack->file_name);
	    }
	  inc_pause_prompt();
	  rep =nsp_parse_eval_from_std(1);
	  dec_pause_prompt();
	  return rep;
	}
      else 
	{
	  /* explicit pause considered as RET_BUG 
	   * in this case we are are in a pausecatch=%t
	   */
	  Scierror("Error: Catching a pause\n");
	  return RET_BUG;
	}
    case CLEAR:  
      /* Sciprintf("clear without arguments\n"); */
      nsp_frame_remove_all_objects(); 
      return 0;
    case CLEARGLOBAL:  
      /*       Sciprintf("clearglobal without arguments\n"); */
      nsp_global_frame_remove_all_objects(); 
      return 0;
    case HELP:
      /* help without arguments */
      nsp_help_browser(NULL,NULL,NULL);
      return 0;
    case CD_COMMAND:
      if ( nsp_chdir_and_update_exec_dir(stack,NULL) == FAIL)
	return RET_BUG;
      return 0;
    case LS_COMMAND:
      nsp_glob("*");
      return 0;
    case PWD_COMMAND:
      Sciprintf("%s\n",nsp_get_cwd());
      return 0;
    case WHAT:
      Sciprintf("command without arguments\n");
      return 0;
    case WHO:
      if ( data->L == NULLLIST ) return RET_BUG;
      nsp_who(stack,"local",FALSE,TRUE,&rep);
      return 0;
    case NSP_EXIT:
      /* Sciprintf("exit without arguments\n"); */
      sci_clear_and_exit(0);
      return 0;
    case ABORT:
      return RET_ABORT;
    case CONTINUE : 
      return RET_CONTINUE;
    default:
      Scierror("Something Strange L->type ....\n");
      return RET_BUG;
    }
}

/**
 * EvalFor:
 * @L1: 
 * @stack: 
 * @first: 
 * 
 * Evaluation of a For loop 
 * 
 * Return value: 
 **/

static int EvalFor(PList L1, Stack stack, int first)
{
  nsp_datas *data = nsp_get_datas();
  NspObject *Val;
  int iloop=0,nargs,rep;
  NspObject *O = NULLOBJ,*O_kp = NULLOBJ;
  stack.first = first;
  /* if (debug) Sciprintf("Evaluation of for \n") ; */
  /*first element of L1 is a name : for loop argument name  **/
  /*Evaluation of the loop argument stored at first **/
  nargs=nsp_eval_arg(L1->next,&stack,first,1,1,0);
  if ( nargs != 1 ) 
    {
      if ( nargs > 1 ) 
	{
	  /* XX we could decide here to loop on args ? */
	  Scierror("Error: incorrect loop value assignment (%d value returned)\n",nargs);
	  nsp_void_seq_object_destroy(stack,first,first+nargs);
	  SHOWBUG(stack,RET_BUG,L1);
	}
      else
	{
	  SHOWBUG(stack,nargs,L1);
	}
    }
  /* copy object from which we loop */
  if ( MaybeObjCopy(&stack.val->S[first]) == NULL)  return RET_BUG;
  Val = stack.val->S[first];
  /* if (nsp_object_set_name(stack.val->S[first],"loop")== FAIL) return RET_BUG;  */
  while (1) 
    {
      iloop++;
      /* On first call O is a nullobject 
       * must be sure that the object cannot be changed while looping 
       * maybe need to make a copy of it ? 
       * LoopExtract must check that O alway has the right size and type 
       */
      if ((O=nsp_object_loop_extract((char *) L1->O,O,Val,iloop,&rep)) == NULLOBJ ) 
	{
	  if ( rep == RET_ENDFOR ) break;
	  rep = RET_BUG;
	  break;
	}
      if ( O_kp != O )  /*if ( iloop == 1 || O_kp != O )*/
	{
	  /* Store O in the data frame only at first iteration or if its memory location 
	   * has changed 
	   */
	  O_kp = O;
	  if (nsp_frame_replace_object(O,-1)==FAIL) 
	    {
	      rep = RET_BUG;
	      break;
	    }
	}
      /*Evaluation of the loop **/
      if (( nargs=nsp_eval_arg(L1->next->next,&stack,first+1,0,0,0))< 0) 
	{
	  if ( nargs != RET_CONTINUE ) 
	    {
	      rep=nargs ; 
	      break;
	    }
	}
    }
  nsp_void_object_destroy(&stack.val->S[first]);
  stack.val->S[first]=NULLOBJ;
  switch (rep) 
    {
      /*XXXX : reste a faire ici : netoyer + break 	  le gerer **/
    case RET_BREAK : 
      /* if (debug ) Sciprintf("catched a break \n");  */
      return 0;
      break;
    case RET_ENDFOR : 
      /* if (debug) Sciprintf("end for \n");  */
      /* we must destroy the loop object */
      if (0 ) 
	{
	  if ( L1->arity != -1 ) 
	    {
	      int tag = VAR_IS_PERSISTENT(L1->arity) ? 2 : 1;
	      /* object is a local variable */
	      O = ((NspCells *) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(L1->arity)];
	      ((NspCells *) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(L1->arity)] = NULL;
	      nsp_object_destroy(&O);
	    }
	  else
	    {
	      O=nsp_frame_search_and_remove_object((char *) L1->O);
	      nsp_object_destroy(&O);
	    }
	}
      return 0; 
      break;
    default:
      return rep;
    }
  return 0;
}


#define MAXLHS 128

/**
 * EvalEqual:
 * @L1: 
 * @stack: 
 * @first: 
 * 
 * Evaluation of the '=' operator 
 *  x0 = Rexp | [x1,....,xn]=Rexp
 *  x0,x1,...,xn  can be a <name> or f(....) or f(..)(..)(..)
 *  and Rexp is a right expression which can return multiple values 
 * 
 * A first pass is performed on the left hand side expresion to estimate 
 * the numbre of values requested. Then Rexp is evaluated 
 * and affectation is performed from left to right 
 * 
 * Note : [x,y(x)] =g (...) 
 * when evaluation of y(x) is performed x takes its new value.
 * 
 * Examples : x=10 , x(1) = 20  , x(1:3)=[1,2,3]
 *            [x,l(3:4)]=(a,b,c)  ( l is a list )  

 * 
 *  A=rand(3,3)
 *  [A(:,4),B]=(rand(2,2),rand(4,5)) ==>  when the first EvalEqual1 fails 
 *  then the stck clean is not corect 
 *  XXXXXX a terminer 
 *  Quand le premier clean de EvalEqual1 est fait il 
 *  y a alors des trous dans le stack avec des non nul obj 
 *  apres des nullobj; Il faut ici faire un clean particulier 
 * 
 *** XXXXXX : check that MAXLHS is never reached 
 * 
 * Return value: 
 **/


static int EvalEqual(PList L1, Stack stack, int first)
{
  int itag;
  int mlhs[MAXLHS]; /*Position of the lhs argument in the stack */
  int mlhs_symb_tab[MAXLHS]; /*Position of the lhs argument in the symbol table */
  int mlhs_dot_flag[MAXLHS]; /* lhs is a set_attribute (DOTVAL) i.e .val or a elts extraction (ARGS, CELLARGS) **/
  char *mlhs_name[MAXLHS]; /*name of lhs argument **/
  int mlhs_r[MAXLHS]; /*number of values requested by lhs argument **/
  obj_check_field objs_check[MAXLHS];
  int r_args_1,nargs,rhs_count,freepos,ret_args, objs_count=0;
  PList loc,loc1,loc2;
  int i,r_args,ipos;
  stack.first = first;
  nsp_datas *data = nsp_get_datas();
  /* L1 starts at MLHS */
  /* Check Lhs It must be a MLHS */
  if ( L1->type != PLIST ) 
    {
      Scierror("Error: Not a valid Lhs\n");
      return RET_BUG ;
    }
  loc = (PList) L1->O;
  if ( loc->type != MLHS )  
    {
      Scierror("Error: Not a valid Lhs \n");
      return RET_BUG ;
    }
  /*r_args expressions in the mlhs expression **/
  r_args = loc->arity ;
  /*
   * Preprocessing of the Mlhs 
   */
  loc  = loc->next;
  r_args_1=0; /*counts how many returned arguments are needed */
  ipos=first;
  for ( i = 0; i < r_args ; i++) 
    {
      if ( loc->type == NAME ) 
	{
	  /*[...... name ....] = f(...) **/
	  mlhs_name[i]= loc->O;
	  mlhs_symb_tab[i]= loc->arity;
	  mlhs[i] = ipos;
	  stack.val->S[ipos++]= data->Reserved; 
	  r_args_1 += mlhs_r[i] = 1;
	}
      else 
	{
	  /*[...... x(..)(..)(..) ....] = f(...) **/
	  int n;
	  loc2 = (PList) loc->O ;
	  loc1 = loc2->next;
	  mlhs[i] = ipos;
	  if ((n= EvalLhsList(loc1,loc2->arity,stack,&ipos,&r_args_1,&mlhs_r[i],&mlhs_dot_flag[i],objs_check,&objs_count))<0) 
	    {
	      nsp_void_seq_object_destroy(stack,first,ipos);
	      return n;
	    }
	}
      loc=loc->next ;
    }
  /* mlhs[i] gives the position of each lhs arguments 
   * an argument will have on reserved position if it's just a name 
   * or more than on argument position if its is a x(..)(..)(..)
   * 
   * mlhs[r_args] gives the first position used by rhs 
   *
   */
  mlhs[r_args]=ipos;/* first free position */

  /*
   * Evaluation of the Rhs 
   * we are requesting r_args_1 returned arguments 
   * Rhs arguments will be stored starting at position ipos.
   */

  if (( nargs =nsp_eval_arg(L1->next,&stack,ipos,1,r_args_1,0)) < 0) 
    {
      nsp_void_seq_object_destroy(stack,first,ipos);
      return nargs ;
    }
  
  if ( nargs < r_args_1 ) 
    {
      /* FIXME: maybe we should here consider to return a 
       * Scierror 
       */ 
      Scierror("Error: Expecting %d value%s and only %d returned\n", r_args_1, (r_args_1 > 1) ? "s":"", nargs);
      return RET_BUG;
    }

  /*
   * if too many arguments are returned then we ignore extra 
   * arguments ex [a,b,c]=(1,2,3,4,5)
   */
  
  if ( nargs > r_args_1 )
    {
      nsp_void_seq_object_destroy(stack,ipos+r_args_1,ipos+nargs);
      /* we update nargs since we have destroyed arguments */
      nargs = r_args_1;
    }

  /* the first free position */
  freepos = ipos+nargs;

  /* 
   * There's one difficulty here : the following situation can occur  
   * [a,b]=(b,a)  
   * if we perform a=b first then the a on the stack (the rhs one)
   * will be damaged and next b=a will result in a BUG  
   * Thus if a rhs object has a name and its name is present in  
   * the MLhs list we copy the Rhs Object (with just the exeption of 
   * the trivial case x = x ). 
   * 
   * Thus if rhs has more than one argument (i.e nargs > 1) 
   * then rhs arguments with name are copied if they are named.
   * 
   * FIXME: Il faut ensuite interdire ds Mlhs  
   *        des noms redondants i.e [a,b(a)]=  
   *        [l,l(1)(1)]= etc.... 
   *        ===> ce qui doit etre fait ds le parseur (this is to be done)
   */

  /*if nargs > 1, then copy the rhs arguments if they have names **/ 
  
  for ( i = 1 ; i < nargs ; i++) 
    {
      if (! Ocheckname(stack.val->S[ipos+i],NVOID) ) 
	{
	  stack.val->S[ipos+i]=nsp_object_copy(stack.val->S[ipos+i]);
	  if ( stack.val->S[ipos+i] == NULLOBJ) 
	    return RET_BUG;
	}
    }

  /*
   * Store Rhs result(s) in Lhs 
   */

  rhs_count = 0;
  ret_args = 0;
  for ( i = 0; i < Min(nargs,r_args) ; i++) 
    {
      /* rhs position */
      int rhs_pos = mlhs[r_args]+rhs_count;
      if ( stack.val->S[mlhs[i]] == data->Reserved ) 
	{
	  /* [...... <name>....] = f(...) 
	   * name = stack.val->S[rhs_pos] is performed 
	   */
	  if ( mlhs_symb_tab[i] != -1 ) 
	    {
	      if ( nsp_store_result_in_symb_table(mlhs_symb_tab[i],mlhs_name[i],stack,rhs_pos) < 0) 
		{
		  nsp_void_object_destroy(&stack.val->S[rhs_pos]);
		  stack.val->S[rhs_pos]=NULLOBJ;
		  /* now a general clean before returning */
		  nsp_void_seq_object_destroy(stack,first,freepos);
		  return RET_BUG;
		}
	    }
	  else 
	    {
	      if (nsp_store_result(mlhs_name[i],stack,rhs_pos) < 0) 
		{
		  nsp_void_object_destroy(&stack.val->S[rhs_pos]);
		  stack.val->S[rhs_pos]=NULLOBJ;
		  /* now a general clean before returning */
		  nsp_void_seq_object_destroy(stack,first,freepos);
		  return RET_BUG;
		}
	    }
	  if ( rhs_count >= nargs )
	    {
	      ret_args = i;
	      break;
	    }
	  /* 
	   */ 
	  stack.val->S[first+i]=stack.val->S[rhs_pos];
	  stack.val->S[rhs_pos]=data->Reserved;/* fill the cell with something */
	  rhs_count++;
	}
      else 
	{
	  const char *name;
	  int n;
	  int j = mlhs[i+1]-mlhs[i],k;
	  name =nsp_object_get_name(stack.val->S[mlhs[i]]);
	  /*Requested Rhs arguments **/
	  rhs_count += mlhs_r[i];
	  if ( rhs_count -1 >= nargs )
	    {
	      /* here we break since we have less than expected 
	       * arguments 
	       */
	      ret_args = i;
	      break;
	    }
	  for ( k=0 ; k < j ; k++ ) 
	    {
	      stack.val->S[freepos+k]= stack.val->S[mlhs[i]+k];
	      stack.val->S[mlhs[i]+k]= data->Reserved;
	    }
	  /*Associated rhs arguments **/
	  for ( k=0 ; k < mlhs_r[i] ; k++)
	    {
	      stack.val->S[freepos+j+k]=stack.val->S[rhs_pos+k];
	      stack.val->S[rhs_pos+k]= data->Reserved;
	    }
	  /* we have to check here cells deletion 
	   * since cells can behave like list or matrix depending on operations 
	   * the test stack.val->S[freepos+1]->type->path_extract is not enough.
	   */
	  /* XXXX: There's a bug here in the check for objects 
	   *       indices are wrong for j+mlhs_r[i]-1 
	   *       if should depend on first. 
	   *       see bug-list 
	   */
	  itag =  check_cast(stack.val->S[freepos+1],nsp_type_cells_id) == TRUE 
	    && mlhs_dot_flag[i] == ARGS 
	    && check_cast(stack.val->S[j+mlhs_r[i]-1],nsp_type_matrix_id) == TRUE 
	    && nsp_object_get_size( stack.val->S[j+mlhs_r[i]-1],0) == 0 ;
	  if ( stack.val->S[freepos+1]->type->path_extract != NULL && itag == FALSE ) 
	    {
	      /* x(exp)= rexp or x()()()()(exp)= rexp 
	       * when x or x()()()() is of <<list>> type 
	       */
	      n=EvalEqual2(name,stack,freepos+1,j-1,j+mlhs_r[i]-1,mlhs_dot_flag[i]);
	      if ( n < 0 ) 
		{ 
		  /* now a general clean before returning */
		  stack.val->S[freepos] = NULLOBJ;
		  /* now a general clean before returning */
		  nsp_void_seq_object_destroy(stack,first,freepos);
		  return n;
		}
	      stack.val->S[freepos+1] = NULLOBJ;
	    }
	  else
	    {
	      int flag = FALSE;
	      /*x(exp)= rexp or x()()()()(exp)= rexp 
	       *  when x or x()()()() is of <<matrix>> type 
	       **/
	      n=EvalEqual1(name,stack,freepos+1,j+mlhs_r[i]-3,&flag);
	      if ( n < 0 ) 
		{ 
		  /* now a general clean before returning */
		  stack.val->S[freepos] = NULLOBJ;
		  /* now a general clean before returning */
		  nsp_void_seq_object_destroy(stack,first,freepos);
		  return n;
		}
	      if ( flag == TRUE )
		{
		  /* We arrive here if EvalEqual1 used a macro instead 
		   * of a standard interface in order to perform the 
		   * computations. Since in EvalEqual1 the interface 
		   * used are special in the sense that they change their 
		   * given argument we have to mimic the same behaviour when 
		   * operations are executed by a macro. In that case 
		   * stack.val->S[freepos] is the old value 
		   * stack.val->S[freepos+1] is the value that the variable 
		   * should have after insertion.
		   */
		  stack.val->S[freepos]= stack.val->S[freepos+1];
		  if ( Ocheckname(stack.val->S[freepos],NVOID) == FALSE)
		    {
		      if ( nsp_frame_replace_object(stack.val->S[freepos],-1)==FAIL) 
			{
			  /* now a general clean before returning */
			  stack.val->S[freepos] = NULLOBJ;
			  /* now a general clean before returning */
			  nsp_void_seq_object_destroy(stack,first,freepos);
			  return RET_BUG;
			}
		    }
		}
	      stack.val->S[freepos+1] = NULLOBJ;
	    }
	  stack.val->S[first+i]= stack.val->S[freepos];
	  stack.val->S[freepos] = NULLOBJ;
	}
    }
  
  if ( ret_args != 0) 
    {
      ret_args  =  Min(ret_args,Min(nargs,r_args));
    }
  else
    {
      ret_args =  Min(nargs,r_args);
    }
  nsp_void_seq_object_destroy(stack,first+ret_args,freepos);
  /*
   * last pass in case we have changed some fields to 
   * check if changes were admissible 
   */
  for ( i = objs_count -1 ; i >= 0 ; i--)
    {
      int j;
      if ( nsp_set_attribute_object(objs_check[i].f,objs_check[i].f->basetype,objs_check[i].s) == FAIL) 
	{
	  /*XXXX in case of problem we have to free the remaining elements */
	  Scierror("Error: affectation would produce an invalid value for field %s\n",  
		   nsp_object_get_name(objs_check[i].s));
	  for ( j=0; j <= i ; j++)  nsp_object_destroy(&objs_check[j].s);
	  return RET_BUG;
	}
    }
  return ret_args; 
}


/**
 * EvalEqual1:
 * @name: 
 * @stack: 
 * @first: 
 * @fargs: 
 * @flag: 
 * 
 * Insertion or Deletion for objects similar to matrix 
 * we want to perform x(z....t) = w 
 * and x,z,...t,w are stored on the stack at location 
 * [first,first+fargs+1[
 * here fargs is the z....t number 
 * 
 * Return value: 
 **/

int EvalEqual1(const char *name, Stack stack, int first, int fargs, int *flag)
{
  NspObject *Obj;
  int k,rep;
  stack.first = first;
  /* check if w=[] : Not perfect since list() will also return 0 
   * in fact deletion should be detected at Parsing XXXXX
   */

  if (nsp_object_get_size( stack.val->S[first+1+fargs],0) == 0) 
    { 
      /* Removing columns or lines or elements 
       * when we get here reamaining IVect are of the type (:) 
       */
      switch ( fargs ) 
	{
	case  1:
	  Obj = stack.val->S[first+1];
	  HOBJ_GET_OBJECT(Obj,RET_BUG);
	  if ( IsIVect(Obj) && ((NspIVect *) Obj)->flag == 1 )   /* x(:)=[] ==> x=[] */
	    {
	      /* delete first+1,first+2 */
	      nsp_void_seq_object_destroy(stack,first+1,first+3);
	      Obj = stack.val->S[first];
	      rep= nsp_eval_maybe_accelerated_op("tozero", 1, tozero_tab, stack,first,1,0,1); 
	      goto end ;
	    }
	  else	                                                  /* x(elts)=[] ==> removing elements */
	    {
	      nsp_void_object_destroy(&stack.val->S[first+2]);
	      stack.val->S[first+2]=NULLOBJ;
	      Obj = stack.val->S[first];
	      rep= nsp_eval_maybe_accelerated_op("deleteelts", 2, deleteelts_tab, stack,first,2,0,1); 
	      goto end;
	    }
	  break;
	case 2: 
	  Obj = stack.val->S[first+1];
	  HOBJ_GET_OBJECT(Obj,RET_BUG);
	  if (IsIVect(Obj) && ((NspIVect *) Obj)->flag == 1 )
	    {
	      Obj = stack.val->S[first+2];
	      HOBJ_GET_OBJECT(Obj,RET_BUG);
	      if (IsIVect(Obj) && ((NspIVect *) Obj)->flag == 1 ) 
		{
		  /* delete first+1,first+2,first+3 */
		  nsp_void_seq_object_destroy(stack,first+1,first+4);
		  Obj = stack.val->S[first];
		  rep= nsp_eval_maybe_accelerated_op("tozero",1, tozero_tab, stack,first,1,0,1); 
		  goto end;
		}
	      else                                                /* x(:,<expr>)=[] */
		{
		  nsp_void_object_destroy(&stack.val->S[first+1]);
		  stack.val->S[first+1]=stack.val->S[first+2];
		  stack.val->S[first+2]=NULLOBJ;
		  nsp_void_object_destroy(&stack.val->S[first+3]);
		  stack.val->S[first+3]=NULLOBJ;
		  /* now we have [x,expr] on the stack */
		  Obj = stack.val->S[first];
		  rep= nsp_eval_maybe_accelerated_op("deletecols",2, deletecols_tab, stack,first,2,0,1);
		  goto end;
		}
	    }
	  else 
	    {
	      Obj = stack.val->S[first+2];
	      HOBJ_GET_OBJECT(Obj,RET_BUG);
	      if (IsIVect(Obj) && ((NspIVect *) Obj)->flag == 1 ) 
		{
		  /* we have [x,expr] on the stack : delete first+2,first+3 */
		  nsp_void_seq_object_destroy(stack,first+2,first+4);
		  Obj = stack.val->S[first];
		  rep = nsp_eval_maybe_accelerated_op("deleterows",2, deleterows_tab,stack,first,2,0,1);
		  goto end;
		}
	      else                                                /* x(<expr>,<expr>)=[] */
		{
		  nsp_void_object_destroy(&stack.val->S[first+3]);
		  stack.val->S[first+3]=NULLOBJ;
		  Obj = stack.val->S[first];
		  rep = nsp_eval_maybe_accelerated_op("deleteelts",2, deleteelts_tab, stack,first,3,0,1);
		  goto end;
		}
	    }
	  break;
	default :       /* when multi dim arrays will be supported we will have to verify in fact
                           if fargs > nb dim of the array */
	  Sciprintf("Error: multi-dimensionnal arrays not currently supported\n");
	  return RET_BUG;
	  break;
	}
    }
  /*General case **/
  /*Expand implicit `:' vectors **/
  for ( k = 1 ; k <= fargs ; k++)
    {
      NspObject *Obj =stack.val->S[first+k]; 
      HOBJ_GET_OBJECT(Obj,RET_BUG);
      if ( IsIVect(Obj) )
	{
	  NspIVect *IV = (NspIVect *) Obj;
	  if ( IV->flag == 1 ) 
	    {
	      IV->flag = 0; IV->first = 1;IV->step=1;
	      IV->last=nsp_object_get_size(stack.val->S[first],fargs == 1 ? 0 : k);
	    }
#ifdef EXPAND_IVECT 
	  /* copy at end of stack before evaluation */
	  stack.val->S[first+fargs+2]= stack.val->S[first+k];
	  if ((n=nsp_eval_func(NULLOBJ,"iv2mat", 2, stack,first+fargs+2,1,0,1)) < 0) return n;
	  /* we do not need to clean stack.val->S[first+k] since the corresponding 
	   * object is cleaned by iv2mat 
	   */
	  stack.val->S[first+k]=stack.val->S[first+fargs+2];
	  stack.val->S[first+fargs+2]=NULLOBJ;
#endif 
	}
    }
  Obj = stack.val->S[first]; /* object to be changed */
  rep = nsp_eval_maybe_accelerated_op("setrowscols",1, setrowscols_tab, stack, first,fargs+2,0,1);
 end:
  if ( rep >= 1 && Obj != stack.val->S[first])
    {
      /* we arrive here if setrowscols was not a primitive but a nsp function
       * stack.val->S[first] in that case contains the new value of the 
       * variable which was changed by setrowscols 
       * XXX Attention: cela se produit pour la plupart des fonctions plus haut !
       */
      nsp_object_set_name(stack.val->S[first],nsp_object_get_name(Obj));
      *flag = TRUE;
    }
  return rep;
}

/**
 * EvalEqual2:
 * @name: 
 * @stack: 
 * @first: 
 * @largs:
 * @fargs: 
 * @dotflag: 
 * 
 * Insertion or Deletion for list objects
 * or Hash table etc.... 
 * 
 * We want to perform here L(z) = (w1,...,wp) 
 * z can be a matrix of size p (scalar or string) 
 * L,z,w1,...,wp are stored in the stack in the 
 * range [first,first+fargs[
 * (deletion is performed when wi = null() )
 * Example :
 * L=list(1,2,3) ; L(4:6)= (7,8,9)
 * 
 * Return value: 
 **/

int EvalEqual2(const char *name, Stack stack, int first,int largs, int fargs, int dotflag)
{
  int n;
  NspObject *O1;
  stack.first = first;
  if ( dotflag == DOTARGS ) 
    {
      /* set_attribute */
      NspFname(stack) = "set"; 
      if (call_interf((function *)stack.val->S[first]->basetype->set_attrs,stack,fargs,0,1) < 0) return RET_BUG;
      nsp_void_seq_object_destroy(stack,first,first+fargs);
      return 1;
    }
  else if ( dotflag == ARGS )
    {
      int i;
      NspMatrix *M=NULL;
      /* here we fill the implicit vectors with proper dimensions
       * and expand them 
       * Expand implicit `:' vectors L(:) = (7,8,9) 
       */
      for ( i=1; i < fargs ; i++) 
	{
	  if (IsIVect(stack.val->S[first+i]) )
	    {
	      NspIVect *IV = nsp_ivect_object(stack.val->S[first+i]);
	      if ( IV->flag == 1)
		{
		  IV->flag = 0; IV->first = 1;IV->step=1;
		  IV->last=nsp_object_get_size(stack.val->S[first], 0);
		}
	      /* directly call the function not to perturb the stack
	       * if ((n=nsp_eval_func(NULLOBJ,"iv2mat",2,stack,first+1,1,0,1)) < 0) return n; 
	       */
	      if ((M=nsp_ivect_2_mat(IV)) == NULLMAT) return RET_BUG;
	      nsp_void_object_destroy(&stack.val->S[first+i]);
	      stack.val->S[first+i]=(NspObject *) M;
	    }
	}
      /* we build a function name depending only on the object L */
      O1=nsp_frames_search_op_object(fname);
      if (nsp_eval_func(O1,"setrowscols",1,stack,first,fargs,0,1) < 0) return RET_BUG;
      nsp_void_seq_object_destroy(stack,first,first+fargs);
      return 1;
    }
  else 
    {
      /* Push largs -1 as last elements it gives the number of indices */
      NspObject *Ob = nsp_create_object_from_double(NVOID,largs-1);
      if ( Ob == NULLOBJ) return RET_BUG;
      stack.val->S[first+fargs]=Ob;
      /* here it should be CELLARGS */
      /* Expand implicit `:' vectors L{:} = (7,8,9) 
       * Expansion should be performed earlier 
       * Note that we can have more than two indices on the 
       * left 
       * L{8,9}=(.....)
       */
      if (IsIVect(stack.val->S[first+1]) )
	{
	  NspIVect *IV = nsp_ivect_object(stack.val->S[first+1]);
	  if ( IV->flag == 1)
	    {
	      IV->flag = 0; IV->first = 1;IV->step=1;
	      IV->last=nsp_object_get_size(stack.val->S[first], 0);
	    }
	  /*WARNING: must be sure that int_iv2mat only changes first+1 **/
	  if ((n=nsp_eval_func(NULLOBJ,"iv2mat",2,stack,first+1,1,0,1)) < 0) return n;
	}
      O1=nsp_frames_search_op_object(fname);
      if (nsp_eval_func(O1,"cells_setrowscols",1,stack,first,fargs+1,0,1) < 0) return RET_BUG;
      nsp_void_seq_object_destroy(stack,first,first+fargs+1);
      return 1;
    }
}

/**
 * EvalLhsList:
 * @L: 
 * @arity: 
 * @stack: 
 * @ipos: 
 * @r_args_1: 
 * @mlhs_r: 
 * @mlhs_flag: 
 * 
 * 
 * Evaluation of L(exp1)(exp2)...(expn) when it appears in a lhs 
 * 
 * L is first evaluated and stored at position *ipos on the stack 
 * Then we follow the path L(exp1)(exp2)....(expn-1) and recursively 
 * evaluate expressions to reach the object  L(exp1)(exp2)....(expn-1)
 * which is stored at location *ipos+1. 
 * Each step of the recursive evaluation is performed by an object 
 * function.
 * 
 * expn is then evaluated and returned arguments are store at position 
 *       *ipos+2,....,
 * 
 * mlhs_r : will return how many arguments of the rhs are to be stored 
 *          in  L(exp1)(exp2)....(expn-1) 
 * r_args_1 += mlhs_r.
 * 
 * Return value: 
 **/

static int EvalLhsList(PList L, int arity, Stack stack, int *ipos, int *r_args_1, int *mlhs_r, int *mlhs_flag, obj_check_field *objs, int *objs_count)
{
  int copy_tag,tag ;
  PList L1;
  char *name;
  int j,fargs,arity1;
  NspObject *O;
  nsp_datas *data = nsp_get_datas();
  /*Object which is changed **/
  if ( L->type != NAME ) 
    {
      Scierror("Error: expecting an expression starting with a name in the lhs of an equation\n");
      SHOWBUG(stack,RET_BUG,L);
    }
  name = L->O;
  tag = VAR_IS_PERSISTENT(L->arity) ? 2 : 1;
  /* Object L which is changed
   */
  if (data->L != NULLLIST 
      && ( ( L->arity != -1 && (O=((NspCells *) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(L->arity)]) != NULLOBJ)
	   || (O= nsp_eframe_search_object((NspFrame *) data->L->first->O,name,FALSE)) != NULLOBJ))
    {
      /* Object is in the current frame or as a variable in the local table which has a value 
       * or as a frame variable. 
       */
      stack.val->S[*ipos] = O;
      if ( stack.val->S[*ipos]->basetype == NSP_TYPE_BASE(nsp_type_hobj) ) 
	{
	  /* object is a pointer to an other object 
	   */
	  if ( ((NspHobj *) stack.val->S[*ipos])->htype != 'g' )
	    {
	      /* 
	       * if we are not dealing with a global 
	       * variable we must perform a copy 
	       * to implement the fact that arguments are transmited by value 
	       */
	      if ((O =nsp_object_copy_and_name(O->name,((NspHobj*)stack.val->S[*ipos])->O)) == NULLOBJ ) 
		{
		  stack.val->S[*ipos] = NULLOBJ;
		  SHOWBUG(stack,RET_BUG,L);
		}
	      /* the copy is stored on the local frame */
	      stack.val->S[*ipos] = O;
	      if ( nsp_frame_replace_object(stack.val->S[*ipos],-1)== FAIL) 
		{
		  nsp_object_destroy(&stack.val->S[*ipos]);
		  SHOWBUG(stack,RET_BUG,L);
		}
	    }
	  else
	    {
	      /* we use a pointer to a global value the pointed Object 
	       * is stored on the stack
	       */
	      /* be sure that pointer is not corrupted ((NspHobj *) O)->O;*/
	      if ((O = nsp_global_frame_search_object(NSP_OBJECT(O)->name)) == NULLOBJ)
		{
		  Scierror("Pointer to a global non existant variablen");
		}
       	      stack.val->S[*ipos] = O;
	    }
	}
    }
  else if ( 0 &&  ( O =nsp_frames_search_object(name)) != NULLOBJ)
    {
      /* XXXX : unused since it is not similar to scilab semi-global philosophy.
       * Object was found in an other frame, we perform a copy 
       * the copy is inserted in the local frame 
       */
      if ((O =nsp_object_copy_with_name(O)) == NULLOBJ ) 
	{
	  stack.val->S[*ipos] = NULLOBJ;
	  SHOWBUG(stack,RET_BUG,L);
	}
      stack.val->S[*ipos] = O;
      if (nsp_frame_replace_object(stack.val->S[*ipos],-1)==FAIL) 
	{
	  nsp_object_destroy(&stack.val->S[*ipos]);
	  SHOWBUG(stack,RET_BUG,L);
	}
    }
  else
    {
      /*
       * We create an empty object. 
       * Note that here we know the type of the rhs 
       * we could use it to create an empty object of the same type as Rhs 
       * note also that the next argument might be a string matrix 
       * as in a.foo or a('foo') in that case we should create a hash table !
       * FIXME 
       */
      /* 
      if ((stack.val->S[*ipos] =nsp_create_empty_matrix_object(name)) == NULLOBJ ) 
	SHOWBUG(stack,RET_BUG,L);
      if (nsp_frame_replace_object(stack.val->S[*ipos],-1)==FAIL) 
	{
	  nsp_object_destroy(&stack.val->S[*ipos]);
	  SHOWBUG(stack,RET_BUG,L);
	}
      */
      stack.val->S[*ipos] = data->Reserved;
    }
  /* duplicate */
  stack.val->S[*ipos+1]=stack.val->S[*ipos];
  /*Following the path **/
  for ( j = 1 ; j <= arity - 2 ; j++ )
    {
      int n;
      L=L->next;
      nsp_set_dollar(&stack,stack.val->S[*ipos+1],0);
      if ((n =nsp_eval_arg(L,&stack,*ipos+2,1,1,0)) < 0) 
	{
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  SHOWBUG(stack,n,L);
	}
      if ( stack.val->S[*ipos+1] == data->Reserved )
	{
	  /* we must create missing object */
	  /* 
	  switch (((PList) L->O)->type)
	    {
	    case ARGS :
	      Sciprintf("Using args\n");break;
	    case METARGS :
	      Sciprintf("Using methargs\n");break;
	    case DOTARGS :
	      Sciprintf("Using dotargs\n");break;
	    case CELLARGS :
	      Sciprintf("Using cellargs\n");break;
	    default: 
	      Sciprintf("Oooops \n");
	    }
	  */
	  /* XXX we create an empty matrix 
	   */
	  if ((stack.val->S[*ipos+1] =nsp_create_empty_matrix_object(name)) == NULLOBJ ) 
	    {
	      stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	      nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+n);
	      SHOWBUG(stack,RET_BUG,L);
	    }
	  if (nsp_frame_replace_object(stack.val->S[*ipos+1],-1)==FAIL) 
	    {
	      nsp_object_destroy(&stack.val->S[*ipos+1]);
	      stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	      nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+n);
	      SHOWBUG(stack,RET_BUG,L);
	    }
	  if ( stack.val->S[*ipos] == data->Reserved ) stack.val->S[*ipos]= stack.val->S[*ipos+1];
	}
      /* let's go for component extraction */
      if ( stack.val->S[*ipos+1]->type->path_extract == NULL ) 
	{
	  Scierror("Error: path extraction cannot be performed (step %d)\n",j);
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+n);
	  SHOWBUG(stack,RET_BUG,L);
	}
      /* XXXX : Pb here because path-extract can have more than one 
       * argument for example A{1,2}... for cells 
       */
      O= stack.val->S[*ipos+1]->type->path_extract(stack.val->S[*ipos+1],n,&stack.val->S[*ipos+2],&copy_tag);
      if ( O == NULLOBJ ) 
	{
	  Scierror("Error: path extraction cannot be performed (step %d)\n",j);
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+n);
	  SHOWBUG(stack,RET_BUG,L);
	}
      if ( copy_tag == TRUE ) 
	{
	  /* we record the fields which are extracted for future check at the end of 
	   * the affectation. If copy_tag is true it means that we have to work on a copy 
	   * and at the end the resulting object will be checked before accepting the change.
	   */
	  objs[*objs_count].f = stack.val->S[*ipos+1];
	  if ((O =nsp_object_copy_with_name(O)) == NULLOBJ )
	    {
	      SHOWBUG(stack,RET_BUG,L);
	    }
	  objs[*objs_count].s = O;
	  (*objs_count)++;
	}
      /* store the extracted object on the stack */
      stack.val->S[*ipos+1]= O;
      nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+n);
    }

  /* Now stack.val->S[*ipos+1] contains the element inside the followed object 
   * which is to be changed. Now we evaluate the last expr which gives
   * the extraction or deletion indices 
   * Note that stack.val->S[*ipos+1] and stack.val->S[*ipos] 
   */
  L=L->next;
  L= (PList) L->O;
  L1= L;
  arity1 = L1->arity;
  L=L->next;
  /* 
   * 
   */
  *mlhs_flag = L1->type; /*  == DOTARGS; */

  if ( stack.val->S[*ipos] == data->Reserved )
    {
      /* first object does not exists we have to create it 
       */
      switch (L1->type)
	{
	case ARGS :
	  /* Sciprintf("Using args\n"); */
	  stack.val->S[*ipos] = nsp_create_empty_matrix_object(name);
	  break;
	case DOTARGS :
	  /* Sciprintf("Using dotargs\n");*/
	  stack.val->S[*ipos] =(NspObject *) nsp_hcreate(name, 5);
	  break;
	case CELLARGS :
	  /* Sciprintf("Using cellargs\n");*/
	  stack.val->S[*ipos] =(NspObject *)  nsp_cells_create(name,0,0);
	  break; 
	default: 
	  stack.val->S[*ipos]= NULLOBJ;
	}
      if (stack.val->S[*ipos] == NULLOBJ ) 
	{
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  SHOWBUG(stack,RET_BUG,L);
	}
      if (nsp_frame_replace_object(stack.val->S[*ipos],-1)==FAIL) 
	{
	  nsp_object_destroy(&stack.val->S[*ipos]);
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  SHOWBUG(stack,RET_BUG,L);
	}
      stack.val->S[*ipos+1]= stack.val->S[*ipos];
    }

  if (0 && arity1 > 2 ) 
    {
      /* FIXME : to be relaxed in the futur 
       */
      stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
      Scierror("y(.)= or y(.,.)= only \n");
      return RET_BUG;
    }
  /* arguments are stored at *ipos+2,...., */
  fargs=0;
  for ( j = 1 ; j <= arity1 ; j++ )
    {
      int n;
      nsp_set_dollar(&stack,stack.val->S[*ipos+1],arity1 == 1 ? 0 : j );
      if ((n =nsp_eval_arg(L,&stack,*ipos+2+fargs,1,1,0)) < 0) 
	{
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+fargs);
	  SHOWBUG(stack,n,L);
	}
      L=L->next;
      fargs += n;
    }
  if ( fargs == 1 && (IsList(stack.val->S[*ipos+2]) || IsHash(stack.val->S[*ipos+2]) ))
    {
      int n;
      /* L(L1)= val : perform a path extraction using L1 */
      stack.first += 1;
      n= ListFollowExtract(stack,2,0,3); 
      stack.first -=1; 
      if ( n < 0 ) 
	{
	  return RET_BUG;
	}
      stack.val->S[*ipos+1]= stack.val->S[*ipos+2]; 
      stack.val->S[*ipos+2]= stack.val->S[*ipos+3]; 
      stack.val->S[*ipos+3]= NULLOBJ; 
    }
  /*
   * Next code should be changed to be object oriented 
   * and avoid to have to check types of arguments
   * 
   * we need to evaluate how many rhs are to be stored in L(exp1)(exp2)....(expn-1) 
   */ 
  if ( IsList(stack.val->S[*ipos+1]) )
    {
      /*  special case when we have a list the number of rhs which 
       *  are to be stored in the list are given by the size of 
       *  stack.val->S[ipos+2] 
       *  L(X)= (....) compute size(X)
       */
      if ( arity1 >= 1 && IsIVect(stack.val->S[*ipos+2]) )
	{
	  /* we have an argument if it is an implicit vector 
	   * we expand it.
	   */
	  NspObject *O;
	  NspIVect *V = nsp_ivect_object(stack.val->S[*ipos+2]);
	  if ( V->flag == 1)
	    {
	      V->first = 1;
	      V->last =nsp_object_get_size(stack.val->S[*ipos+1],0);
	      V->step = 1;
	      V->flag = 0;
	    }
	  if ((O= (NspObject *)nsp_ivect_2_mat(V)) == NULLOBJ ) 
	    {
	      stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	      nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+fargs);
	      return RET_BUG;
	    }
	  nsp_void_object_destroy(&stack.val->S[*ipos+2]); /* : */
	  stack.val->S[*ipos+2] = O;
	}
      if ( fargs != 1) 
	{
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+fargs);
	  Scierror("Error: too many arguments (%d) for list element extraction\n",fargs);
	  return RET_BUG;
	}
      *r_args_1 += (*mlhs_r =nsp_object_get_size(stack.val->S[*ipos+2],0));
    }
  else if (IsCells(stack.val->S[*ipos+1]) && L1->type == CELLARGS ) 
    {
      int i,nelts=1;
      /*  special case for cells when the extraction operator is {} 
       *  C{arg1,....,argn} = (....) 
       *  the requested number of arguments are given by size(arg1)*size(arg2) etc..
       *  arity1: is the number of expressions in C(arg1,...,argn) i.e n 
       *  fargs : is the number of objects created by evaluation of arg1,....,argn 
       */
      for ( i = 0 ; i < fargs ; i++) 
	{
	  if ( IsIVect(stack.val->S[*ipos+2+i]) )
	    {
	      /* we have an argument if it is an implicit vector we expand it.
	       * This could be delegated in interfaces for space optimization.
	       */
	      NspObject *O;
	      NspIVect *V =nsp_ivect_object(stack.val->S[*ipos+2+i]);
	      if ( V->flag == 1)
		{
		  V->first = 1;
		  V->last =nsp_object_get_size(stack.val->S[*ipos+1],(fargs == 1) ? 0: i+1);
		  V->step = 1;
		  V->flag = 0;
		}
	      if ((O= (NspObject *)nsp_ivect_2_mat(V)) == NULLOBJ ) 
		{
		  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
		  nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+fargs);
		  return RET_BUG;
		}
	      nsp_void_object_destroy(&stack.val->S[*ipos+2+i]); /* : */
	      stack.val->S[*ipos+2+i] = O;
	    }
	  nelts *= nsp_object_get_size(stack.val->S[*ipos+2+i],0);
	}
      if ( fargs > 2 ) 
	{
	  stack.val->S[*ipos] =  stack.val->S[*ipos+1] = NULLOBJ;
	  nsp_void_seq_object_destroy(stack,*ipos+2,*ipos+2+fargs);
	  Scierror("Error: too many arguments (%d) for cell elements insertion\n",fargs);
	  return RET_BUG;
	}
      *r_args_1 += (*mlhs_r = nelts);
    }
  else 
    {
      /* we expect just one rhs argument */
      *r_args_1 += *mlhs_r = 1;
    }
  *ipos += fargs+2;
  return 1;
}


/**
 * EvalRhsList:
 * @L: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @lhs: 
 * 
 * evaluation of <name><exp1><exp2>...<expn> when found at a  rhs position 
 *     where <name> := <varname>
 *           <expi> := ( <expr1>,<expr2>,..<exprn>) |
 *	               { <expr1>,<expr2>,..<exprn>} |
 *	               [ <expr1>,<expr2>,..<exprn>] |
 *	               .name 
 *    
 *     --> (varname exp1 exp2 .... expn  LISTEVAL) 
 *         expi   --> ( expr1 expr2 ... exprn  ARGS|METHARGS|CELLARGS) 
 *         expi    or ( "name" DOTARGS)
 *         
 * { exp , stack } 
 *  
 *  { ( f <ARGS> .... <ARGS> LISTEVAL) , [] } 
 * 
 *  { ( f <ARGS> LISTEVAL) , [] }  --> {  ,  extract( ev(f),ev(<ARGS>)) } 
 *  { ( f <ARGS1> .... <ARGSN> LISTEVAL ) , [] } 
 * 	--> { ( <ARGS1>,....,<ARGSN> LISTEVAL ) , [ O=ev(f)] } 
 *  { ( <ARGS1>,....,<ARGSN> LISTEVAL ) , [ O_1,...,O_K] } 
 *     if <ARGS1> == <ARGS> and k=2 call a method given by O_2 
 *                          --> { {....} , meth_[O_2]( O_1,O_2,ev(ARGS1))}
 *     if <ARGS1> == <DOTARG> and k=1 call dot  --> { {....} , dot(O_1,ev(ARGS1)}
 *		--> {  , extract( O1,....,OK, ev(<ARGS1>) } 
 *     if <ARGS1> == <ARGS> and k=1 call funceval 
 *              --> { {....}, funceval(O1,ev(ARGS))} 
 *     else error 
 * 
 * 
 *      win.set_title[foo]   --> method 
 * 	win.title	     --> dot 
 * 	win(title,10)	     --> eval 
 * 
 * Return value: 
 **/

int EvalRhsList(PList L, Stack stack, int first, int rhs, int lhs, int display)
{
  const char *name=NULL;
  int j,n,arity,nargs,lhs1=1,opt, copy_tag;
  int arg_is_macro = FALSE, arg_is_fun = FALSE;
  NspObject *O1;
  PList L2;
  stack.first = first;
  /*list extraction when following a path first */
  arity = L->arity ;
  /* Get first argument to fix $ value if necessary */
  if ( L->next->type == PLIST ) 
    {
      /* first argument is given by a PLIST we need to evaluate to obtain its value 
       */
      /* nsp_plist_print_internal(L->next->O); */
      if (( n=nsp_eval((PList) L->next->O,stack,first,0,1,display))< 0)
	{
	  nsp_void_seq_object_destroy(stack,first,first+1);
	  return n;
	}
      L= L->next;
    }
  else 
    {
      /* first argument is given by it's name: we first search in frames */
      name = (char *) L->next->O;
      /* */
      L= L->next;
      stack.val->S[first]=nsp_frames_search_object(name);
      if (stack.val->S[first] == NULLOBJ ) 
	{
	  int Int,Num;
	  /* then search in functions  */
	  if ( nsp_find_function((char *) L->O,&Int,&Num) == OK) 
	    {
	      /* check if name is a function **/
	      if ((stack.val->S[first]= (NspObject *) function_create(NVOID,(char *) L->O,Int,Num,-1,NULL))==  NULLOBJ) 
		return RET_BUG;
	      arg_is_fun = TRUE;
	    }
	  else 
	    {
	      /* then search for macros */
	      if ((stack.val->S[first]=nsp_find_macro((char *) L->O)) != NULLOBJ ) arg_is_macro = TRUE;
	    }
	}
    }
  /*Following the evaluation  L(exp1)(exp2)...(expn) **/
  nargs=1;
  for ( j = 1 ; j < arity ; j++ )
    {
      /* sequential evaluation of (.. <ARGS>|<DOTARGS>|<METHARGS>|<CELLARGS>) 
       * we assume here that L was properly build 
       */
      /* char fname[NAME_MAXL]; */
      int nret ,k ;
      PList Larg ; 
      /* we expect 1 returned argument except for last evaluation */
      if ( j == arity -1 ) lhs1 = lhs;  
      L=L->next;
      Larg = (PList) L->O;
      if ( Larg->type == DOTARGS )
	{
	  /*
	   * x.name or x.name[...] ? 
	   */
	  if ( j < arity-1 && ((PList) L->next->O)->type == METARGS ) 
	    {
	      /*
	       * method invocation x.name[...]
	       */
	      char *meth_name = (char *) (Larg->next)->O ;
	      /* we expect 1 returned argument except for last evaluation */
	      if ( j == arity - 2 ) lhs1 = lhs;  
	      if ( nargs != 1 ) 
		{
		  Scierror("Error: calling method %s cannot be performed (step %d)\n",
			   meth_name,j);
		  Scierror("\t Expecting one object on the stack and %d found\n",nargs);
		  if ( nargs > 1 ) 
		    {
		      nsp_void_seq_object_destroy(stack,first,first+nargs);
		    }
		  SHOWBUG(stack,RET_BUG,L);
		}
	      if ( stack.val->S[first] == NULLOBJ ) 
		{
		  /* f.name[....] where f is not found 
		   * FIXME: we could check if f can be a function 
		   * and associate methods to functions 
		   */
		  Scierror("Error: variable %s does not exists\n",name); 
		  SHOWBUG(stack,RET_BUG,L);
		}
	      /* name mangling for methods ? 
	       *nsp_build_funcname(meth_name,stack,first,1,fname+1); 
	       */
	      L=L->next; 
	      j++;  
	      /*
	       * evaluation of method arguments 
	       */
	      if ((n =nsp_eval_arg(L,&stack,first+nargs,1,1,0)) < 0)  
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L);
		}
	      nargs += n;
	      /*
	       * counting method optional arguments 
	       */
	      opt=0; for ( k = 0 ; k < nargs ; k++ ) if ( IsHopt(stack.val->S[first+k]) ) opt++;
	      /* since a method can change the object it is applied to 
	       * we have to check if object is a pointer and copy the object if 
	       * necessary. 
	       */
	      if ( stack.val->S[first]->basetype == NSP_TYPE_BASE(nsp_type_hobj) ) 
		{
		  /* object is a pointer to an other object 
		   * and it is not a global variable.
		   */
		  if ( ((NspHobj *) stack.val->S[first])->htype != 'g' )
		    {
		      NspObject *O;
		      /* 
		       * if we are not dealing with a global 
		       * variable we must perform a copy 
		       * to implement the fact that arguments are transmited by value 
		       */
		      if ((O =nsp_object_copy_and_name(stack.val->S[first]->name,
						       ((NspHobj*)stack.val->S[first])->O)) == NULLOBJ ) 
			{
			  nsp_void_seq_object_destroy(stack,first,first+nargs);
			  return RET_BUG;
			}
		      /* the copy is stored on the local frame and replaces the pointer */
		      stack.val->S[first] = O;
		      if ( nsp_frame_replace_object(stack.val->S[first],-1)== FAIL) 
			{
			  nsp_void_seq_object_destroy(stack,first,first+nargs);
			  return RET_BUG;
			}
		    }
		}
	      /*
	       * call the method and return 
	       */
	      if ((nret =nsp_eval_method(meth_name,stack,first,nargs,opt,lhs1)) < 0) 
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  return RET_BUG;
		}
	      nargs = nret;
	      if ( nargs > 0 ) name = stack.val->S[first]->name;
	    }
	  else 
	    {
	      /*
	       * find attribute x.name
	       */
	      function *get_attrs;
	      if ( nargs != 1 ) 
		{
		  Scierror("Error: ``.'' extraction cannot be performed (step %d)\n",j);
		  Scierror("Expecting one argument on the stack and %d found\n",nargs);
		  if ( nargs > 1 ) 
		    {
		      nsp_void_seq_object_destroy(stack,first,first+nargs);
		    }
		  SHOWBUG(stack,RET_BUG,L);
		}
	      /* DOT operator */
	      if ( stack.val->S[first] == NULLOBJ ) 
		{
		  /* f.name[....] where f is not found 
		   * FIXME: we could check if f can be a function 
		   * and associate methods to functions 
		   */
		  Scierror("Error: unknown variable %s\n",name); 
		  SHOWBUG(stack,RET_BUG,L);
		}
	      /* evaluation of ARGS */
	      if ((n =nsp_eval_arg(L,&stack,first+nargs,1,1,0)) < 0) 
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		  SHOWBUG(stack,n,L);
		}
	      if ( stack.val->S[first] == NULLOBJ ) 
		{
		  /* FIXME: we could check if f can be a function 
		   * and associate methods to functions 
		   */
		  /* clean the evaluated args */
		  nsp_void_seq_object_destroy(stack,first+1,first+1+nargs);
		  Scierror("Error: unknown variable %s\n",name); 
		  SHOWBUG(stack,RET_BUG,L);
		}
	      /*  get attributes */
	      NspFname(stack) = "get"; 
	      /*counting optional arguments **/
	      opt=0; for ( k = 0 ; k < nargs+n ; k++ ) if ( IsHopt(stack.val->S[first+k]) ) opt++;
	      /* we want to call here the get_attribute function    */
	      if ( IsHobj(stack.val->S[first]) )
		{
		  NspObject *Ob = stack.val->S[first];
		  HOBJ_GET_OBJECT(Ob,RET_BUG);
		  get_attrs = (function *) Ob->basetype->get_attrs; 
		}
	      else
		get_attrs = (function *)stack.val->S[first]->basetype->get_attrs; 
	      if ((nret = call_interf(get_attrs,stack,nargs+n,opt,lhs1)) < 0) 
		{ 
		  nsp_void_seq_object_destroy(stack,first,first+nargs+n );
  		  return RET_BUG;
		}
	      nargs = nret;
	      if ( nargs > 0 ) name = stack.val->S[first]->name;
	    }
	}
      else 
	{
	  /*
	   * here we evaluate A(...) or A{}
	   * thus A must have resulted in just one object on the stack
	   * Note that A can be a function or macro
	   */
	  int count,k;
	  if ( nargs != 1 ) /* && j != arity-1)  */
	    {
	      Scierror("Error: extraction or function evaluation cannot be performed (step %d)\n",j);
	      Scierror("\tExpecting one argument on the stack and %d found\n",nargs);
	      if ( nargs > 1 ) 
		{
		  nsp_void_seq_object_destroy(stack,first,first+nargs);
		}
	      SHOWBUG(stack,RET_BUG,L);
	    }
	  /*
	   * A is a pointer replace it by the object it points to 
	   */
	  if ( stack.val->S[first] != NULLOBJ && stack.val->S[first]->basetype ==NSP_TYPE_BASE(nsp_type_hobj)) 
	    {
	      /* a pointer to a nsp coded function */
	      NspObject *O1 =  stack.val->S[first];
	      HOBJ_GET_OBJECT(O1,RET_BUG);
	      if ( O1->basetype == NSP_TYPE_BASE(nsp_type_plist))   stack.val->S[first] = O1;
	    }
	  if ( stack.val->S[first] != NULLOBJ && stack.val->S[first]->basetype != NSP_TYPE_BASE(nsp_type_plist) 
	       && arg_is_macro == FALSE &&  arg_is_fun == FALSE)
	    { 
	      /* Object is a variable, evaluate arguments and call extract
	       */
	      count = nargs;
	      L2= L->O;
	      L2= L2->next;
	      for ( k= 1; k <= Larg->arity ; k++) 
		{
		  /* set '$' before evaluation of arguments */
		  nsp_set_dollar(&stack,stack.val->S[first],Larg->arity == 1 ? 0 : k );
		  if ((n =nsp_eval_arg(L2,&stack,first+count,1,1,0)) < 0)  
		    {
		      nsp_void_seq_object_destroy(stack,first,first+count);
		      SHOWBUG(stack,n,L2);
		    }
		  count += n;
		  L2 = L2->next;
		}
	      /* counting optional arguments */
	      opt=0; for ( k = 0 ; k < count ; k++ ) if ( IsHopt(stack.val->S[first+k]) ) opt++;
	      if ( Larg->type == CELLARGS )
		{
		  /* Special case when we are performing a {} extraction */
		  NspObject *O1 =  stack.val->S[first];
		  /* XXXX If we keep a pointer here next operations are wrong 
		   * but changing the pointer is dangerous since object can 
		   * then be changed by methods. 
		   * this is dangerous because methods can then change the object ? 
		   */
		  HOBJ_GET_OBJECT(O1,RET_BUG);
		  stack.val->S[first] = O1;
		  
		  if ( j == arity -1 )
		    {
		      if ((nret = nsp_eval_extract_cells(stack,first,count,opt,-1)) < 0)
			return nret;
		    }
		  else if ( stack.val->S[first]->type->path_extract != NULL ) 
		    {
		      /* for a cell here we must perform a path_extract */
		      NspObject *Ob= stack.val->S[first]->type->path_extract(stack.val->S[first],count-1,&stack.val->S[first+1],&copy_tag);
		      if ( Ob == NULLOBJ ) 
			{
			  Scierror("Error: extraction cannot be performed (step %d)\n",j);
			  nsp_void_seq_object_destroy(stack,first,first+count);
			  SHOWBUG(stack,RET_BUG,L);
			}
		      if (copy_tag == FALSE)
			{
			  /* copy was not performed in path_extract but it is not necessary in a EvalRhsList 
			   *  If we make a copy here (without name) then the object will be freed latter and 
			   *  maybe we will also extract a part of this object in next steps 
			   *  we have the risk to free it twice. Thus not copying seams a better idea 
			   * Ob = nsp_object_copy(Ob);
			   */
			  if ( Ocheckname(Ob,NVOID))
			    {
			      Scierror("Internal Error: extraction from cells of an object having no name\n");
			      return RET_BUG;
			    }
			}
		      if ( Ob == NULLOBJ ) 
			{
			  Scierror("Error: failed to copy an object in extraction (step %d)\n",j);
			  SHOWBUG(stack,RET_BUG,L);
			}		      
		      /* path_extract do not perform freeing */
		      nsp_void_seq_object_destroy(stack,first+1,first+count);
		      stack.val->S[first] = Ob;
		      nret=1;
		    }
		  else
		    {
		      if ((nret =nsp_eval_extract(stack,first,count,opt,-1)) < 0) 
			{
			  nsp_void_seq_object_destroy(stack,first,first+count);
			  return nret;
			}
		      if ( nret != 1 ) 
			{
			  Scierror("Error: path extraction in step (step %d) returned too many arguments\n",
				   j,nret);
			  nsp_void_seq_object_destroy(stack,first,first+count);
			  SHOWBUG(stack,RET_BUG,L);
			}
		    }
		}
	      else 
		{
		  if ((nret =nsp_eval_extract(stack,first,count,opt,-1)) < 0) 
		    {
		      nsp_void_seq_object_destroy(stack,first,first+count);
		      return nret;
		    }
		}
	      /* If the last extraction is done here,
	       * we must take care that for list or cells the 
	       * previous call to nsp_eval_extract and nsp_eval_extract_cells 
	       * have not performed copies. We must do it here 
	       */
	      nargs = nret;
	      if ( j == arity -1 ) 
		{
		  int i1; 
		  for ( i1 = 0 ; i1 < nargs ; i1++)
		    {
		      NspObject *O;
		      if ( Ocheckname(stack.val->S[first+i1],NVOID) == FALSE ) 
			{
			  if ((O = nsp_object_copy(stack.val->S[first+i1]))== NULLOBJ) return RET_BUG;
			  stack.val->S[first+i1]=O;
			}
		    }
		}
	    }
	  else 
	    {
	      /* 
	       * function or macro evaluation 
	       */
	      count = 0;
	      if ( arg_is_macro == TRUE || arg_is_fun == TRUE )
		{
		  /* because of name mangling: if the first argument was a function or 
		   * macro we must forget it's name here 
		   */
		  O1= NULL;
		  if (arg_is_fun == TRUE ) nsp_object_destroy(&stack.val->S[first]);
		  /* forget this for further iterations */
		  arg_is_macro =  arg_is_fun = FALSE;
		}
	      else
		{
		  O1 = stack.val->S[first];
		}
	      /* evaluation of ARGS */
	      stack.val->S[first]=NULLOBJ;
	      if ((n =nsp_eval_arg(L,&stack,first+count,1,1,0)) < 0)  
		{
		  nsp_void_seq_object_destroy(stack,first,first+count);
		  SHOWBUG(stack,n,L);
		}
	      count += n;
	      /*counting optional arguments **/
	      opt=0; for ( k = 0 ; k < count ; k++ ) if ( IsHopt(stack.val->S[first+k]) ) opt++;
	      /*function evaluation with name mangling */
	      if ((nret=nsp_eval_func(O1,name,2,stack,first,count,opt,lhs1))<0) 
		return nret;
	      nargs = nret;
	    }
	}
    }
  return nargs;
}


/**
 * EvalRhsCall:
 * @L: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @lhs: 
 * 
 * A simplified version of EvalRhsList which is used to evaluate
 * (CALLEVAL f (ARGS ...))
 * i.e a simple function call or an extract. 
 *
 * Return value: 
 **/
 
int EvalRhsCall(PList L, Stack stack, int first, int rhs, int lhs)
{
  nsp_datas *data = nsp_get_datas();
  int count,k,n,opt,nret,i1,nargs;
  char *name;
  NspObject *O1;
  PList Largs,Lf;
  stack.first = first;
  /* we know here that arity is 2 when entering Eval RhsCall 
   * L == (CALLEVAL fname (ARGS ....)) 
   */
  Lf = L->next;
  name = Lf->O;
  Largs = Lf->next->O;
  /* Get first argument to fix $ value if necessary 
   * O1 can be NULL 
   */
  if ( ! ( name[0] == '_' && name[1] == '_' ) ) 
    {
      /* should turn what follows in a function */
      if ( Lf->arity == -1 ) 
	{
	  /* search object in frames */
	  stack.val->S[first]=nsp_frames_search_object(name);
	}
      else
	{
	  int tag = VAR_IS_PERSISTENT(Lf->arity) ? 2 : 1;
	  /* direct acces to object through table of local variables */
	  stack.val->S[first] = ((NspCells*) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(Lf->arity)];
	  /* maybe the local variable has a value in calling stacks */
	  if ( stack.val->S[first] == NULLOBJ ) 
	    stack.val->S[first]=nsp_frames_search_local_in_calling(name,FALSE);
	}
    }
  else 
    {
      stack.val->S[first]= NULLOBJ;
    }
  /*
   * A is a pointer replace it by the object it points to 
   */
  if ( stack.val->S[first] != NULLOBJ && stack.val->S[first]->basetype ==NSP_TYPE_BASE(nsp_type_hobj)) 
    {
      /* a pointer to a nsp coded function */
      NspObject *O1 =  stack.val->S[first];
      HOBJ_GET_OBJECT(O1,RET_BUG);
      if ( O1->basetype == NSP_TYPE_BASE(nsp_type_plist))   stack.val->S[first] = O1;
    }
  
  if ( stack.val->S[first] != NULLOBJ && stack.val->S[first]->basetype != NSP_TYPE_BASE(nsp_type_plist) 
       && stack.val->S[first]->basetype != NSP_TYPE_BASE(nsp_type_function))
    { 
      /* Object is a variable, evaluate arguments and call extract
       */
      count = 1;
      nargs = Largs->arity;
      Largs = Largs->next;/* point to first element of ARGS */
      for ( k= 1; k <= nargs ; k++) 
	{
	  /* set '$' before evaluation of arguments */
	  nsp_set_dollar(&stack,stack.val->S[first],nargs == 1 ? 0 : k );
	  if ((n =nsp_eval_arg(Largs,&stack,first+count,1,1,0)) < 0)  
	    {
	      nsp_void_seq_object_destroy(stack,first,first+count);
	      SHOWBUG(stack,n,Largs);
	    }
	  count += n;
	  Largs = Largs->next;
	}
      /* counting optional arguments */
      opt=0; for ( k = 0 ; k < count ; k++ ) if ( IsHopt(stack.val->S[first+k]) ) opt++;
      
      if ((nret =nsp_eval_extract(stack,first,count,opt,lhs)) < 0) 
	{
	  nsp_void_seq_object_destroy(stack,first,first+count);
	  return nret;
	}
      /*
       * we must take care that for list, previous call to nsp_eval_extract
       * have not performed copies. We must do it here 
       */
      for ( i1 = 0 ; i1 < nret ; i1++)
	{
	  NspObject *O;
	  /* we do not copy hopt object (returned by H(:) when H is a 
	   * hash table or object which have no names 
	   */
	  if ( (!IsHopt(stack.val->S[first+i1])) 
	       &&  Ocheckname(stack.val->S[first+i1],NVOID) == FALSE ) 
	    {
	      if ((O = nsp_object_copy(stack.val->S[first+i1]))== NULLOBJ) return RET_BUG;
	      stack.val->S[first+i1]=O;
	    }
	}
      
    }
  else 
    {
      /* 
       * function or macro evaluation 
       */
      /* evaluation of ARGS **/
      O1= stack.val->S[first];
      if ( O1 != NULL && stack.val->S[first]->basetype == NSP_TYPE_BASE(nsp_type_function))
	{
	  /* O1 is a function not a macro */
	  name = ((NspFunction *) stack.val->S[first])->fname;
	  O1= NULL;
	}
      stack.val->S[first]=NULLOBJ;
      /*
       * sequential evaluation of function arguments 
       * we explicitely walk on args not calling nsp_eval_arg
       */
      count = 0;
      nargs = Largs->arity;
      Largs = Largs->next;/* point to first element of ARGS */
      for ( k= 1; k <= nargs ; k++) 
	{
	  if ((n =nsp_eval_arg(Largs,&stack,first+count,1,1,0)) < 0)  
	    {
	      nsp_void_seq_object_destroy(stack,first,first+count);
	      SHOWBUG(stack,n,Largs);
	    }
	  count += n;
	  Largs = Largs->next;
	}
      /*counting optional arguments **/
      opt=0; for ( k = 0 ; k < count ; k++ ) if ( IsHopt(stack.val->S[first+k]) ) opt++;

      if ( name[0]== '_' && name[1] == '_' ) 
	{
	  /* Undocumented Feature: 
	   * Note here that if a function name starts with __ 
	   * we search the interface to be used without considering name mangling 
	   * and store the interface to be called in arity field.
	   */
	  int Int,Num;
	  stack.first = first;
	  NspFname(stack) = name ;
	  if ( Lf->arity == -1 ) 
	    {
	      if ( nsp_find_function(name+2,&Int,&Num) == OK) 
		Lf->arity = (Int << 12 )  + Num;
	      else 
		{
		  nsp_reorder_stack(stack,0);
		  return RET_BUG;
		}
	    }
	  else 
	    {
	      Num = Lf->arity & 0xfff;
	      Int = (Lf->arity & 0xfff000 ) >> 12 ;
	    }
	  if (( nret = nsp_interfaces(Int,Num,stack,count,opt,lhs)) < 0)
	    {
	      return nret;
	    }
	}
      else 
	{
	  nret=nsp_eval_func(O1,name,2,stack,first,count,opt,lhs);
	}
    }
  return nret;
}


/**
 * EvalOpt:
 * @L1: 
 * @stack: 
 * @first: 
 * 
 * 
 * Evaluation of the OPT operator 
 *    ( i.e  <name> = Rhs inside a FEVAL)
 * 
 * Return value: 
 **/

static int EvalOpt(PList L1, Stack stack, int first)
{
  NspHobj *H;
  int nargs =0;
  stack.first = first;
  /*Check Lhs It must be a MLHS **/
  if ( ! ( L1->type == NAME ||  L1->type == OPNAME ) )
    {
      Scierror("Not a valid optional argument specification x=Rhs, x is not a name \n");
      return RET_BUG ;
    }
  /*expecting 1 returned values **/
  /*Evaluation of the Rhs **/
  if (( nargs =nsp_eval_arg(L1->next,&stack,first,1,1,0)) < 0) return nargs ;
  if ( nargs < 1 ) 
    {
      Sciprintf("Warning: %d returned value \n",nargs);
      return RET_BUG;
    }
  /*We put a Hopt on the stack  **/
  if ( stack.val->S[first]->basetype == NSP_TYPE_BASE(nsp_type_hobj))
    {
      NspObject *O1 = stack.val->S[first];
      HOBJ_GET_OBJECT(O1,RET_BUG);
      /*Argument is a pointer : Hopt must point to the object  */
      if ((H = HoptCreate((char *) L1->O,O1)) == NULLHOPT)  return RET_BUG;
    }
  else 
    {
      if ((H = HoptCreate((char *) L1->O,stack.val->S[first])) == NULLHOPT)  return RET_BUG;
    }
  stack.val->S[first]= (NspObject *) H;
  return 1;
}


/**
 *nsp_store_result:
 * @str: 
 * @stack: 
 * @first: 
 * 
 * Object at position first in the stack is stored
 *       in the current frame  (or in the global frame)
 *       under the name str 
 *       if Object has a name it is copied first
 *       if Object is a pointer the objects it points 
 *                 to is stored 
 *       if Object name == str 
 *          if str exists in current frame and == Object 
 *          then do nothing else do as above 
 *
 * If a new object is created it is stored at position first 
 * 
 * Return value: 
 **/

int nsp_store_result(char *str, Stack stack, int first)
{
  /* if ( debug ) Sciprintf("=Storing=>%s\n",str); */
  if ( stack.val->S[first] != NULLOBJ ) 
    {
      NspObject *Ob = stack.val->S[first], *O1=nsp_frame_search_object(str);
      if ( Ocheckname(Ob,str) ) 
	{
	  /* Ob->name == str 
	   * if Object exists in the current frame and 
	   * is the same as Ob then do nothing 
	   * a = a ==> no Copy 
	   * a = H.a ==> copy since it's not the same a
	   */ 
	  if ( O1 != NULL && O1 == Ob ) return 1;
	}
      /* If the Object on the stack is a pointer 
	 The object it points to is considered */
      if ( Ob->basetype ==  NSP_TYPE_BASE(nsp_type_hobj))
	{
	  HOBJ_GET_OBJECT(Ob,RET_BUG);
	}
      if ( MaybeObjCopy(&Ob) != NULL ) 
	{
	  nsp_object_set_name(Ob,str);
	  /* XXX : Attention les deux fonction qui suivent peuvent renvoyer une valeur */
	  if ( O1 != NULL && IsGlobal(O1) )
	    {
	      if ( nsp_global_frame_replace_object(Ob) == FAIL) 
		{
		  if ( Ob == stack.val->S[first] ) 
		    {
		      /* the object was unnamed and we have set its name here */
		      nsp_object_destroy(&stack.val->S[first]);
		      stack.val->S[first]=NULLOBJ;
		    }
		  else 
		    {
		      /* if a copy was made we need to clean */
		      nsp_object_destroy(&Ob);
		    }
		  return RET_BUG;
		}
	    }
	  else 
	    {
	      if (nsp_frame_replace_object(Ob,-1)==FAIL) 
		{
		  if ( Ob == stack.val->S[first] ) 
		    {
		      /* the object was unnamed and we have set its name here */
		      nsp_object_destroy(&stack.val->S[first]);
		      stack.val->S[first]=NULLOBJ;
		    }
		  else 
		    {
		      /* if a copy was made we need to clean */
		      nsp_object_destroy(&Ob);
		    }
		  return RET_BUG;
		}
	    }
	  /* note that the object which as at position first 
	   * need not be destroyed since it is a named object 
	   * or it was a void object which was renamed in Ob 
	   **/
	  stack.val->S[first]=Ob;
	  return 1;
	}
      else 
	{
	  Scierror("Error: No more Space \n");
	  return RET_BUG;
	}
    }
  else 
    {
      /*we should never get there **/
      Scierror("Bug Report: internal error in StoreResult ....\n");
      return RET_BUG;
    }
}

/* store an object in the curent frame 
 * taking care of global frame. We assume 
 * here that the object has a name.
 */

int nsp_store_object(NspObject *Ob) 
{
  const char *name = nsp_object_get_name(Ob);
  NspObject *O1; 
  if ( name == NULL) return FAIL;
  O1 = nsp_frame_search_object(name);
  /* If the Object is a pointer 
   * then the object it points to is considered 
   */
  if ( Ob->basetype ==  NSP_TYPE_BASE(nsp_type_hobj))
    {
      HOBJ_GET_OBJECT(Ob,FAIL);
    }
  if ( O1 != NULL && IsGlobal(O1) )
    {
      if ( nsp_global_frame_replace_object(Ob) == FAIL) 
	{
	  Scierror("Error: cannot insert %s in global frame\n",
		   name);
	  return FAIL;
	}
    }
  else 
    {
      if (nsp_frame_replace_object(Ob,-1)==FAIL) 
	{
	  Scierror("Error: cannot insert %s in local frame\n",
		   name);
	  return FAIL;
	}
    }
  return OK;
}



/**
 *nsp_store_result_in_symb_table:
 * @position: id of variable in the local table.
 * @str: 
 * @stack: 
 * @first: 
 * 
 * Object at position first in the stack is stored
 * in the symbol table at position @position.
 * 
 *       if Object has a name it is copied first
 *       if Object is a pointer the objects it points 
 *                 to is stored 
 *       if Object name == str 
 *          if str exists in current frame and == Object 
 *          then do nothing else do as above 
 *
 * If a new object is created it is stored at position first 
 * 
 * Return value: 1 or %RET_BUG;
 **/

static int nsp_store_result_in_symb_table(int position,const char *str, Stack stack, int first)
{
  nsp_datas *data = nsp_get_datas();
  /* if ( debug ) Sciprintf("=Storing=>%s\n",str); */
  if ( stack.val->S[first] != NULLOBJ ) 
    {
      int tag;
      NspObject *Ob = stack.val->S[first], *O1;
      if (  data->L == NULLLIST ) 
	{
	  Scierror("Error: Can't insert obj in empty Data frame\n");
	  return RET_BUG; 
	}
      /* get current frame local variable table */
      tag = VAR_IS_PERSISTENT(position) ? 2 : 1;
      O1 = ((NspCells *) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(position)];
      if ( Ocheckname(Ob,str) ) 
	{
	  /* Ob->name == str 
	   * if Object exists in the current frame and 
	   * is the same as Ob then do nothing 
	   * a = a ==> no Copy 
	   * a = H.a ==> copy since it's not the same a
	   */ 
	  if ( O1 != NULL && O1 == Ob ) return 1;
	}
      /* If the Object on the stack is a pointer 
       * The object it points to is considered 
       */
      if ( Ob->basetype ==  NSP_TYPE_BASE(nsp_type_hobj))
	{
	  HOBJ_GET_OBJECT(Ob,RET_BUG);
	}
      if ( MaybeObjCopy(&Ob) != NULL ) 
	{
	  nsp_object_set_name(Ob,str);

	  if ( O1 != NULL && IsGlobal(O1) )
	    {
	      if ( nsp_global_frame_replace_object(Ob) == FAIL) 
		{
		  if ( Ob == stack.val->S[first] ) 
		    {
		      /* the object was unnamed and we have set its name here */
		      nsp_object_destroy(&stack.val->S[first]);
		      stack.val->S[first]=NULLOBJ;
		    }
		  else 
		    {
		      /* if a copy was made we need to clean */
		      nsp_object_destroy(&Ob);
		    }
		  return RET_BUG;
		}
	      /* now O1 must points to the new global value */
	      ((NspHobj *) O1)->O = Ob;
	    }
	  else 
	    {
	      tag = VAR_IS_PERSISTENT(position) ? 2 : 1;
	      if ( O1 != NULL )  nsp_object_destroy(&O1);
	      ((NspCells *) ((NspFrame *) data->L->first->O)->locals->objs[tag])->objs[VAR_ID(position)]= Ob;
	    }
	  /* note that the object which as at position first 
	   * need not be destroyed since it is a named object 
	   * or it was a void object which was renamed in Ob 
	   **/
	  stack.val->S[first]=Ob;
	  return 1;
	}
      else 
	{
	  Scierror("Error: No more Space \n");
	  return RET_BUG;
	}
    }
  else 
    {
      /*we should never get there **/
      Scierror("Bug Report: internal error in StoreResult ....\n");
      return RET_BUG;
    }
}

/**
 *nsp_set_dollar:
 * @stack: pointer to a stack object 
 * @O: an object 
 * @j: indice to use 
 * 
 * 
 * Fix the value of scilab $ variable 
 * $ is fixed to object dimensions. 
 * The current $ value is stored in the Stack @stack.
 * 
 **/

static void nsp_set_dollar(Stack *stack,NspObject *O, int j)
{
  if ( O != NULLOBJ ) 
    {
      stack->dollar = nsp_object_get_size(O,j);
    }
}


/*
 * Used to display the list in which an evaluation bug was detected 
 * -2 can be used to fix the recursive level for message display 
 */

static int show_eval_bug(Stack stack,int n, PList L) 
{
  IOVFun def ;
  MoreFun mf; 
  if (  n >= RET_BUG-2 && n <= RET_BUG ) 
    { 
      NspSMatrix *res;
      Scierror("\t-error->");
      /* changes io in order to write in a string matrix */
      def = SetScilabIO(Sciprint2string);
      mf =nsp_set_nsp_more(scimore_void);
      nsp_plist_print(L,0);
      res = (NspSMatrix *) Sciprint2string_reset(); 
      SetScilabIO(def);
      nsp_set_nsp_more(mf);
      if ( res != NULL) 
	{
	  int i;
	  /* push res at the end of error */
	  for ( i = 0 ; i < res->mn ; i++) 
	    Scierror("%s\n",res->S[i]);
	  nsp_smatrix_destroy(res);
	}
      if ( NspFileName(stack) != NULL) 
	{
	  int line= nsp_parser_get_line(L);
	  if ( line != -1 ) 
	    Scierror("\tline %d of file %s\n",line,NspFileName(stack));
	}
      else if ( 0 && NspFname(stack) != NULL) 
	{
	  /* we just get there if function was defined interactively 
	   * and in that case line refers to interactive lines 
	   * thus it is not worth giving this information
	   */
       	  int line= nsp_parser_get_line(L);
	  if ( line != -1 ) 
	    Scierror("\tline %d of function %s\n",line,NspFname(stack));
	}
      /* to get rid of next potential messages */
      return n-5;
    }
  else if ( n == RET_ERROR_RAISED )
    {
      /* we get here after a call to the nsp function error("...") */
      if ( 0 && NspFileName(stack) != NULL) 
	{
	  int line= nsp_parser_get_line(L);
	  if ( line != -1 ) 
	    Scierror("\tline %d of file %s\n",line,NspFileName(stack));
	}
      return n;
    }
  else 
    {
      return n;
    }
}



/**
 * nsp_void_seq_object_destroy:
 * @stack: objects stack
 * @from: first object to be removed 
 * @to: last object to be remove is @to-1
 * 
 * deallocate a sequence of objects from position @from to position @to-1
 * using nsp_void_object_destroy and remove them from @stack.
 * 
 **/

void nsp_void_seq_object_destroy(Stack stack,int from, int to)
{
  int i;
  for ( i= from ; i < to ; i++)
    {
      nsp_void_object_destroy(&stack.val->S[i]);
      stack.val->S[i]=NULLOBJ;
    }
}



/**
 * nsp_eval_maybe_accelerated_op:
 * @opname: 
 * @msuffix: 
 * @tab: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_eval_maybe_accelerated_op(char *opname, int msuffix,accelerated_ops tab_id,
				  Stack stack, int first, int rhs, int opt, int lhs)
{
  const AcceleratedTab *tab = nsp_get_accelerated_tab(tab_id);
  int id1, id2;
  function *the_func = NULL;

  HOBJ_GET_OBJECT(stack.val->S[stack.first], RET_BUG);
  id1 = nsp_get_id_from_object(stack.val->S[stack.first]);
  
  if ( tab->arity == 1 )
    the_func = nsp_get_fast_function(tab, id1);
  else
    {
      HOBJ_GET_OBJECT(stack.val->S[stack.first+1], RET_BUG);
      id2 = nsp_get_id_from_object(stack.val->S[stack.first+1]);
      if ( id1 == id2 )
	the_func = nsp_get_fast_function(tab, id1);
    }

  if ( the_func )      /* acceleration is supported */
    {
      NspFname(stack) = opname;
      return call_interf(the_func, stack, rhs, opt, lhs);
    }
  else                 /* acceleration not supported */
    {
      NspObject *O1= nsp_frames_search_op_object(opname);
      return nsp_eval_func(O1, opname, msuffix, stack, first, rhs, opt, lhs);
    }
}


/**
 * nsp_eval_maybe_accelerated_binop:
 * @opname: 
 * @opcode: 
 * @stack: 
 * @first: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_eval_maybe_accelerated_binop(const char *opname, int opcode,
				     Stack stack, int first, int rhs, int opt, int lhs)
{
  int id1, id2;
  NspObject *O1=NULLOBJ;
  const AcceleratedTab *OpTab;
  function *the_func = NULL;

  if (  NOTCODE_OP < opcode && opcode < LASTCODE_OP  
	&&  ((OpTab = nsp_get_accelerated_tab(opcode - NOTCODE_OP + setrowscols_tab)) != NULL )
	&&  OpTab->length != 0 )
    {
      /* acceleration supported for this operator (at least on one type) */
      HOBJ_GET_OBJECT(stack.val->S[stack.first], RET_BUG);
      id1 = nsp_get_id_from_object(stack.val->S[stack.first]);

      HOBJ_GET_OBJECT(stack.val->S[stack.first+1], RET_BUG);
      id2 = nsp_get_id_from_object(stack.val->S[stack.first+1]);
      if ( id1 == id2 )
	{
	  the_func = nsp_get_fast_function(OpTab, id1);
	  if ( the_func )      /* acceleration is supported */
	    {
	      NspFname(stack) = opname;
	      return call_interf(the_func, stack, rhs, opt, lhs);
	    }
	}
    }

  /* acceleration not supported */
  O1 = nsp_frames_search_op_object(opname);
  return nsp_eval_func(O1, opname, 2, stack, first, rhs, opt, lhs);
}



/**
 *nsp_eval_object_is_true:
 * @O: a #NspObject pointer 
 * 
 * Check if object @O can be considered as a boolean a TRUE Object 
 * in a logical statement. Try also to detect if this evaluation 
 * raised an error.
 * It should be better to add a parameter to the is_true method in 
 * order to detect error raised by is_true method but we don't for 
 * backward compatibility and thus try to detect errors differently.
 * 
 * Return value: %TRUE or %FALSE
 **/

static int nsp_eval_object_is_true(Stack *S, NspObject *O, int *err)
{
  int rep, n1,n2;
  n1=nsp_error_count_lines(S);
  rep = O->type->is_true(O);
  n2=nsp_error_count_lines(S);
  *err = (n2 > n1 ) ? TRUE : FALSE;
  return rep;
}
