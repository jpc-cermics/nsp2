/* Nsp
 * Copyright (C) 2004-2019 Jean-Philippe Chancelier Enpc/Cermics
 * 
 * This library emulates Matlab' API functions.
 * It is a fully rewriten version of Scilab mexlib.c file 
 * since Scilab and nsp object are totally different 
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
 * mexlib library for Nsp.
 * rewrite for nsp of the scilab mexlib library (Chancelier, Delebecque).
 *
 */


static void nsp_mex_errjump(void);
#include <stdio.h>
#include <setjmp.h>
#include <nsp/nsp.h>
#include <nsp/object.h>
#include <nsp/hash.h>
#include <nsp/matrix.h>
#include <nsp/bmatrix.h>
#include <nsp/smatrix.h>
#include <nsp/imatrix.h>
#include <nsp/matint.h>
#include <nsp/interf.h>
#include <nsp/plist.h>
#include <nsp/cells.h>
#include <nsp/spcolmatrix.h>
#include <nsp/hobj.h>

#include "nsp/datas.h"
#include "nsp/parse.h"
#include "nsp/gtk/gobject.h" /* FIXME: nsp_gtk_eval_function */

/* some nsp function are to be updated for const */

#define MEXLIB 
#include "mex/mex.h"

/* these two variables are 
 * to be set up corectly to enable reentry in mex 
 */

static int onlyone =0;
static jmp_buf MexEnv;


static void nsp_initmex(const char *name,int *lfirst,int lhs,mxArray *plhs[], 
			int rhs, mxArray *prhs[])
{
  Stack *stack = nsp_get_stack();
  int k=0;
  if ( onlyone != 0) 
    {
      Scierror("Error: mex recursive calls are not allowed\n");
      nsp_mex_errjump();      
    }
  else 
    {
      onlyone = 1;
    }
  stack->first = *lfirst;
  NspFnameH(stack) = name;
  for (k = 1; k <= rhs ; ++k) 
    {
      NspObject *Obj;
      if (( Obj=nsp_get_object(*stack,k)) == NULL) 
	{
	  Scierror("Error: something corrupter in a nsp_initmex call\n");
	  nsp_mex_errjump();      
	}
      else 
	prhs[k-1]= Obj;
    }
  for (k = 0; k < Max(lhs,1) ; ++k) plhs[k]=NULL;
} 

static int nsp_endmex_backconvert(NspObject *Obj)
{
  if (IsSpColMat(Obj))
    {
      /* back convert sparses */
      NspSpColMatrix *Sp =(NspSpColMatrix *) Obj;
      if ( nsp_spcol_update_from_triplet(Sp)==FAIL) 
	return FAIL;
    }
  return OK;
}

static void nsp_endmex_backfree(NspObject *Obj)
{
  mxFreeSparseMtlbTriplet((mxArray *) Obj);
}

static void nsp_endmex(Stack stack,int lhs,mxArray *plhs[],int rhs,mxArray *prhs[])
{
  int i;
  /* first pass to set the return position */
  for ( i= 1 ; i <= Max(lhs,1) ; i++) 
    {
      NspObject *Obj = plhs[i-1];
      if ( Obj != NULL ) 
	{
	  Obj->ret_pos = i;
	  if ( nsp_endmex_backconvert(Obj)== FAIL) goto bug;
	}
    }
  /* clean the input array for non-returned arguments 
   * here we have to back convert input arguments in fact 
   * just to clean allocated parts since a user is not supposed 
   * to have changed an input argument.
   */
  for ( i= 1 ; i <= rhs ; i++) 
    {
      NspObject *obj = prhs[i-1];
      if ( obj != NULLOBJ ) 
	{
	  nsp_endmex_backfree(obj) ;
	  if  (IsHobj(obj) ) 
	    {
	      /* Hopt was created when entering the interface 
	       * other pointers are not to be destroyed.
	       */
	      if ( IsHopt(obj) )
		{
		  /* O is of type pointer */
		  NspObject *obj1= ((NspHobj *) obj)->O;
		  nsp_object_destroy(&obj);
		  /* if Hopt was pointing to a non named object and is not 
		   * a return value 
		   */
		  if ( obj1->ret_pos == -1 )
		    nsp_void_object_destroy(&obj1);
		}
	    }
	  else if ( obj->ret_pos == -1 ) 
	    {
	      nsp_void_object_destroy(&obj);
	    }
	}
      NthObj(i)=NULLOBJ;
    }
  /* put lhs on stack */
  for ( i= 1 ; i <= Max(lhs,1) ; i++) 
    {
      NthObj(i) =  plhs[i-1];
    }
  onlyone =0;
  return;
 bug:
  onlyone =0;
  nsp_mex_errjump();
}

/*  used when performing a longjmp **/

static void nsp_clearmex(void)
{
  onlyone =0;
}

/**
 * nsp_mex_wrapper:
 * @stack: calling stack 
 * @rhs: an integer 
 * @opt: an integer 
 * @lhs: an integer 
 * @mexFunction: a #mexfun to be called 
 * 
 * performs a mexFunction call. 
 * 
 * Return value: an integer
 **/

/* XXX maybe we have here to block Ctrl-C 
 * to be sure that nsp_endmaex is always performed 
 */

typedef const mxArray **c_prhs;

int nsp_mex_wrapper(Stack stack, int rhs, int opt, int lhs,mexfun *mexFunction)
{
  mxArray  *plhs[INTERSIZ];
  mxArray *prhs[INTERSIZ];
  int rfl;
#ifdef WIN32 
  if (( rfl = setjmp(MexEnv)) != 0 )
    {
      return RET_BUG;
    }
#else 
  if (( rfl = sigsetjmp(MexEnv,1)) != 0 )
    {
      return RET_BUG;
    }
#endif 
  nsp_initmex(NspFname(stack),&stack.first,lhs, plhs, rhs, prhs);
  mexFunction(lhs, plhs, rhs,(c_prhs)  prhs);
  if ( lhs <= 0 && plhs[0] != NULL ) lhs = 1;
  nsp_endmex(stack,lhs, plhs, rhs,prhs);
  return Max(0,lhs);
}


static void nsp_mex_errjump(void)
{
  nsp_clearmex();
#ifdef WIN32 
  longjmp(MexEnv,-1);
#else 
  siglongjmp(MexEnv,-1);
#endif 
}


/**
 * mxGetPr:
 * @ptr: an #mxArray 
 * 
 * Gets a pointer to the real part of an #mxArray. works
 * for #NspMatrix and #NspSpColMatrix. 
 * 
 * Return value: a pointer to an array of double or %NULL.
 **/

double *mxGetPr(const mxArray *ptr)
{
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  ptr;
      /* be sure that matrix is matlab converted */
      A = Mat2mtlb_cplx (A);
      return A->R;
    }
  else if ( IsSpColMat(ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      return A->triplet.Pr;
    }
  else if ( IsBMat(ptr) )
    {
      Sciprintf("Warning: mxGetPr should be replaced by mxGetData for booleans\n");
      return (double *) ((NspBMatrix *) ptr)->B;
    }
  else if ( IsString(ptr) )
    {
      return (double *)  ((NspSMatrix *) ptr)->S[0];
    }
  Scierror("Error in %s: mxGetPr failed\n","mex");
  nsp_mex_errjump();
  return NULL;
}

/**
 * mxGetPi:
 * @ptr: an #mxArray 
 * 
 * Gets a pointer to the imaginary part of an #mxArray. works
 * for #NspMatrix and #NspSpColMatrix. 
 * 
 * Return value: a pointer to an array of double or %NULL.
 **/


double *mxGetPi(const mxArray *ptr)
{  
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *) ptr ;
      if ( A->rc_type == 'r' )
	{
	  return NULL;
	}
      /* be sure that matrix is matlab converted */
      A = Mat2mtlb_cplx (A);
      return A->R+ A->mn;
    }
  else if ( IsSpColMat(ptr)) 
    {
      NspSpColMatrix *A = (NspSpColMatrix *) ptr ;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      return A->rc_type == 'c' ? A->triplet.Pi : NULL;
    }
  else
    {
      Scierror("Error in %s: mxGetPr failed\n","mex");
      nsp_mex_errjump();
    }
  return NULL;
}

/**
 * mxGetM:
 * @ptr: an #mxArray 
 * 
 * returns the number of rows of a matrix or more generally 
 * the first dimension of an object. 
 * 
 * Return value: an integer
 **/

int mxGetM(const mxArray *ptr)
{
  if ( IsHash(ptr)) 
    {
      return 1;
    }
  return nsp_object_get_size(ptr,1);
}


/**
 * mxGetJc:
 * @ptr:  an #mxArray 
 * 
 * Jc=mxGetJc returns a n+1 integer array such that 
 * Jc(j) is the indice in Ir and Pr arrays of the firt non null element of column j 
 * of the sparse matrix @ptr.
 * Jc(n) is the number of non-null elements of the sparse matrix.
 * 
 * Return value: a pointer to an array of integer.
 **/

int *mxGetJc(const mxArray *ptr)
{
  NspSpColMatrix *A = (NspSpColMatrix *) ptr;
  if ( ! IsSpColMat(ptr)) nsp_mex_errjump();
  if (A->convert != 't' ) 
    {
      if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
    }
  return A->triplet.Jc;
}

/**
 * mxGetIr:
 * @ptr:  an #mxArray 
 * 
 * Ir=mxGetIr returns an integer array with the row indices of the non-nul elements of 
 * sparse matrix @ptr (See also mxGetJc()).
 * 
 * Return value: a pointer to an array of integer.
 **/

int *mxGetIr(const mxArray *ptr)
{
  NspSpColMatrix *A = (NspSpColMatrix *) ptr;
  if ( ! IsSpColMat(ptr)) nsp_mex_errjump();
  if (A->convert != 't' ) 
    {
      if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
    }
  return A->triplet.Ir;
}

/**
 * mxGetN:
 * @ptr:  an #mxArray 
 * 
 * returns the number of rows of mxArray @ptr or more generally 
 * the dimension 2 of an object. For string matrices it returns 
 * the max of the length of strings storedin the matrix.
 * 
 * Return value: an integer.
 **/

int mxGetN(const mxArray *ptr)
{
  if ( IsSMat(ptr)) 
    {
      int n=0,i;
      if ( ((NspSMatrix*) ptr)->mn == 0 ) return 0;
      n = strlen(((NspSMatrix*) ptr)->S[0]);
      for ( i=1 ; i < ((NspSMatrix*) ptr)->mn ; i++)
	n =Max(n, strlen(((NspSMatrix*) ptr)->S[i]));
      return n;
    }
  else if ( IsHash(ptr)) 
    {
      return 1;
    }
  return nsp_object_get_size(ptr,2);
}

/**
 * mxIsString:
 * @ptr:  an #mxArray 
 * 
 * returns 1 if @ptr is a string matrix of size 1x1.
 * 
 * Return value: 1 if @ptr is a string else 0
 **/

int mxIsString(const mxArray *ptr)
{
  return (IsSMat(ptr) && ((NspSMatrix *) ptr)->mn == 1);
} 

/**
 * mxIsNumeric:
 * @ptr: an #mxArray 
 * 
 * checks if @ptr is a Matrix (#NspMatrix) or a sparse Matrix 
 *(#NspSpColMatrix).
 * 
 * Return value: 1 if @ptr is a #NspMatrix or #NspSpMatrix else 0.
 **/

int mxIsNumeric(const mxArray *ptr)
{
  return ( IsMat(ptr) ||  IsSpColMat(ptr) );
}

/**
 * mxIsFull:
 * @ptr: an #mxArray 
 *
 * checks if @ptr is a full Matrix (#NspMatrix).
 * 
 * Return value: 1 if @ptr is a #NspMatrix else 0.
 **/

int mxIsFull(const mxArray *ptr)
{
  return IsMat(ptr);
}

/**
 * mxIsSparse:
 * @ptr: an #mxArray 
 * 
 * checks if @ptr is a sparse Matrix. 
 * 
 * Return value: 1 if @ptr is a #NspSpMatrix else 0.
 **/

int mxIsSparse(const mxArray *ptr)
{
  return IsSpColMat(ptr) ;
}

/**
 * mxIsComplex:
 * @ptr: an #mxArray 
 * 
 * checks if @ptr is a complex Matrix (#NspMatrix or #NspSpColMatrix).
 * 
 * Return value: 1 if @ptr is a complex #NspMatrix or complex #NspSpMatrix else 0.
 **/

int mxIsComplex(const mxArray *ptr)
{
  if ( IsMat(ptr) )
    {
      return ((NspMatrix *) ptr)->rc_type == 'c' ;
    }
  else if ( IsSpColMat(ptr) )
    {
      return ((NspSpColMatrix *) ptr)->rc_type == 'c';
    }
  else
    return 0;
}

/**
 * mxGetScalar:
 * @ptr: an #mxArray 
 * 
 * get scalar value from a 1x1 mxArray. 
 * 
 * Return value: a double if @ptr is a scalar else an error jump.
 **/

double mxGetScalar(const mxArray *ptr)
{ 
  if ( IsMat(ptr) && ((NspMatrix *) ptr)->mn >= 1
       && ((NspMatrix *) ptr)->rc_type == 'r')
    {
      return ((NspMatrix *) ptr)->R[0];
    }
  else if ( IsBMat(ptr)  && ((NspBMatrix *) ptr)->mn >= 1)
    {
      return (double) ((NspBMatrix *) ptr)->B[0];
    }
  else {
    Scierror("Expecting a scalar\n");
  }
  nsp_mex_errjump();
  return 0.0;
}

/**
 * mexErrMsgTxt:
 * @error_msg: a string to be used as error message 
 * 
 * uses Scierror() to display @error_message, the mex evaluation 
 * is stopped and control returns at the interpreter level with error mechanism.
 * 
 **/

void mexErrMsgTxt(const char *error_msg)
{
  Scierror("Error in %s: ","mex");
  Scierror(error_msg);
  Scierror("\n");
  nsp_mex_errjump();
}

/**
 * mexErrMsgIdAndTxt:
 * @error_msg: a string to be used as error message 
 * 
 * uses Scierror() to display @error_message, the mex evaluation 
 * is stopped and control returns at the interpreter level with error mechanism.
 * 
 **/

void mexErrMsgIdAndTxt(const char *id,const char *error_msg)
{
  /* Scierror("Error in %s: ","mex"); */
  Scierror("Error: %s",error_msg);
  Scierror("\n");
  nsp_mex_errjump();
}

/**
 * mexWarnMsgIdAndTxt:
 * @error_msg: a string to be used as error message 
 * 
 * uses Scierror() to display @error_message, the mex evaluation 
 * is stopped and control returns at the interpreter level with error mechanism.
 * 
 **/

void mexWarnMsgIdAndTxt(const char *id,const char *error_msg)
{
  /* Scierror("Error in %s: ","mex"); */
  Sciprintf("Error: %s\n",error_msg);
}


/**
 * mxCreateDoubleMatrix:
 * @m: number of rows 
 * @n: number of columns 
 * @it: #mxReal or #mxComplex.
 * 
 * creates a new #NspMatrix of size (mxn). The array 
 * elements are initialized to zero.
 * 
 * Return value: a new #mxArray or error jump
 **/

mxArray *mxCreateDoubleMatrix(int m, int n,  mxComplexity it)
{
  int i;
  NspMatrix *A;
  if ( it == mxREAL ) 
    {
      if ((A = nsp_matrix_create(NVOID,'r',m,n) ) == NULLMAT) nsp_mex_errjump();
      /* memset(A->R,0,A->mn*sizeof(double)); */
      for (i=0; i < A->mn ; i++) A->R[i]=0.0;
    }
  else
    {
      if ((A = nsp_matrix_create(NVOID,'c',m,n) ) == NULLMAT) nsp_mex_errjump();
      A->convert = 'c'; /* matab complex style */
      /* memset(A->R,0,2*A->mn*sizeof(double)); */
      for (i=0; i < 2*A->mn ; i++) A->R[i] = 0.0;
    }
  return NSP_OBJECT(A);
}


/**
 * mxCreateScalarDouble:
 * @value: a double
 * 
 * creates a new #NspMatrix of size (1x1) initialized with @d.
 * 
 * Return value: a new #mxArray or error jump
 **/

mxArray *mxCreateScalarDouble(double value)
{
  NspMatrix *A;
  if ((A = nsp_matrix_create_from_array(NVOID,1,1,&value,NULL))  == NULLMAT) 
    nsp_mex_errjump();
  return NSP_OBJECT(A);
}

/**
 * mxCreateDoubleScalar:
 * @value: a double
 * 
 * creates a new #NspMatrix of size (1x1) initialized with @d.
 * 
 * Return value: a new #mxArray or error jump
 **/

mxArray *mxCreateDoubleScalar(double value)
{
  NspMatrix *A;
  if ((A = nsp_matrix_create_from_array(NVOID,1,1,&value,NULL))  == NULLMAT) 
    nsp_mex_errjump();
  return NSP_OBJECT(A);
}


/**
 * mxCreateFull:
 * @m: number of rows 
 * @n: number of columns 
 * @it: #mxReal or #mxComplex.
 * 
 * creates a new #NspMatrix of size (mxn). The array 
 * elements are not initialized.
 * 
 * Return value: a new #mxArray or error jump
 **/

mxArray *mxCreateFull(int m, int n, int it)
{
  int i;
  NspMatrix *A;
  if ( it == 0) 
    {
      if ((A = nsp_matrix_create(NVOID,'r',m,n) ) == NULLMAT) nsp_mex_errjump();
      for (i=0; i < A->mn ; i++) A->R[i]=0.0;
    }
  else
    {
      if ((A = nsp_matrix_create(NVOID,'c',m,n) ) == NULLMAT) nsp_mex_errjump();
      A->convert = 'c'; /* matab complex style */
      for (i=0; i < 2*A->mn ; i++) A->R[i] = 0.0;
    }
  return NSP_OBJECT(A);
}

/**
 * mxCalloc:
 * @n: number of array element to allocate 
 * @size: size of array element
 * 
 * allocates an array of size @n x @size*sizeof(char). 
 * Note that in Nsp the array is not freed when quiting the mex.
 * 
 * Return value: the new allocated array 
 **/

void *mxCalloc(size_t n, size_t size)
{
  return calloc(n,size*sizeof(char));
}

/**
 * mxGetString:
 * @ptr: an #mxArray 
 * @str: a char buffer of size at least @strl
 * @strl: maximum number of characters which can be writen in @str.
 * 
 * 
 * Return in @str at most @strl -1 characters from the string matrix 
 * pointed by @ptr (If @ptr is a mxn String NspMatrix, the str will contain 
 * the concatenation of the strings contained in @ptr).
 * The string @str is always #NULL terminated. 
 * 
 * Return value: 0 on success and 1 in case of error. The possible reasons for error are: 
 *  @ptr is not a string mxArray.
 *  @strl is less than the number of characters needed to store the entire mxArray 
 *  pointed to by @str. If this is the case, 1 is returned and the string is truncated.
 **/

int mxGetString(const mxArray *ptr, char *str, int strl)
{
  int rep;
  nsp_string message;
  const NspSMatrix *A = (const NspSMatrix *) ptr;
  if ( ! IsSMat(ptr) ) {
    str[0]='\0';    return 1;
  }
  message =nsp_smatrix_elts_concat(A,"",1,"",1);
  if ( message == NULL) 
    {
      nsp_mex_errjump();
    }
  strncpy(str,message,strl-1);
  str[strl-1]='\0';
  rep = (strlen(message) > strl-1) ? 1: 0;
  nsp_string_destroy(&message);
  return rep;
}

/**
 * mxArrayToString:
 * @ptr: an #mxArray
 * 
 * Get all the strings of @ptr (which is checked to be an #NspSMatrix)
 * in one buffer, the string is allocated and should be freed by the user.
 * with mxFree().
 * 
 * Return value: a newly allocated array of chars.
 **/

char *mxArrayToString(const mxArray *ptr)
{
  nsp_string message;
  const NspSMatrix *A = (const NspSMatrix *) ptr;
  if ( ! IsSMat(ptr) ) nsp_mex_errjump();
  message =nsp_smatrix_elts_concat(A,"",1,"",1);
  if ( message == NULL) 
    {
      nsp_mex_errjump();
    }
  return message;
}
      
/**
 * mxFreeMatrix:
 * @ptr: a pointer 
 * 
 *
 * 
 **/

void mxFreeMatrix (mxArray *ptr)
{
  nsp_object_destroy(&ptr);
  return ;
}


/**
 * mxFree:
 * @ptr: a pointer 
 * 
 * frees previously allocated memmory ( mxCalloc() or mxMalloc() )
 **/

void mxFree(void *ptr)
{
  if (ptr != NULL)  free(ptr);
}

/**
 * mexAtExit:
 * @ExitFcn: 
 * 
 * Do nothing.
 * 
 * Return value: 0.
 **/

int mexAtExit(void (*ExitFcn)(void))
{
  return 0;
}


/**
 * mxCreateSparse:
 * @m: number of rows 
 * @n: number of columns 
 * @nzmax: number of non null elements 
 * @ComplexFlag:  mxREAL, mxCOMPLEX. 
 * 
 * creates a new @mx@n sparse matrix and initialize arrays 
 * using the @nzmax value. 
 * 
 * Return value: a new #NspSpColMatrix or NULLSPCOL
 **/

mxArray *mxCreateSparse(int m, int n, int nzmax, 
			mxComplexity ComplexFlag)
{
  NspSpColMatrix *A;
  if ( ComplexFlag == mxREAL ) 
    {
      if ((A = nsp_spcolmatrix_create(NVOID,'r',m,n) ) == NULLSPCOL) nsp_mex_errjump();
    }
  else
    {
      if ((A = nsp_spcolmatrix_create(NVOID,'c',m,n) ) == NULLSPCOL) nsp_mex_errjump();
      A->convert = 'c'; /* matab complex style */
    }
  /* just allocate triplet */
  if ( nsp_spcol_alloc_col_triplet(A,nzmax) == FAIL)  nsp_mex_errjump();
  /* set the triplet to zero */
#if 1 
  if ( A->convert == 't' ) 
    {
      int i;
      for ( i=0; i < A->n; i++) A->triplet.Jc[i]=0;
      for ( i=0; i < nzmax; i++) A->triplet.Ir[i]=0;
      for ( i=0; i < nzmax; i++) A->triplet.Pr[i]=0;
      if ( A->rc_type=='c' )
	for ( i=0; i < nzmax; i++) A->triplet.Pi[i]=0;
    }
#endif 
  return NSP_OBJECT(A);
}


/**
 * mxCreateString:
 * @string: a string 
 * 
 * returns a new #NspSMatrix containing a copy of @string and 
 * returned as a #mxArray
 * 
 * Return value: a new #mxArray 
 **/

mxArray *mxCreateString(const char *string)
{
  NspSMatrix *S;
  if ((S= nsp_smatrix_create(NVOID,1,1,string,(integer)1)) == NULLSMAT ) nsp_mex_errjump();
  return NSP_OBJECT(S);
}


/**
 * mxGetField:
 * @pa: a #mxArray 
 * @i:  an integer 
 * @fieldname: a string 
 * 
 * here @pa is supposed to be a Hash Table 
 * the index is not used i.e we only accept 
 * i==0; The object associated to name @fieldname 
 * is searched and returned from the @pa Hash Table without copy.
 * 
 * 
 * Return value: a #NspObject or %NULL
 **/

mxArray *mxGetField (const mxArray *pa, int i,const char *fieldname)
{
  NspObject *Obj;
  NspHash *H = (NspHash *) pa;
  if ( i != 0 )
    {
      Scierror("Struct just have a zero index \n");
      nsp_mex_errjump();
    }
  if ( ! IsHash(pa) ) nsp_mex_errjump();
  if ( nsp_hash_find(H,fieldname,&Obj) == FAIL) 
    {
      return NULL;
      /* 
	 Scierror("Error: cannot find field %s\n",fieldname);
	 nsp_mex_errjump();
      */
    }
  return Obj;
}

/**
 * mxCreateStructMatrix:
 * @m: number of rows 
 * @n: number of columns 
 * @nfields: number of fields 
 * @field_names: a table giving the field names
 * 
 * Only 1x1 strcut matrice are handled and they are stored 
 * in nsp hash table objets. 
 * 
 * Return value: a new #NspHash object or %NULLHASH
 **/

mxArray *mxCreateStructMatrix(int m, int n, int nfields, const char **field_names)
{
  NspHash *H;
  if ( m != 1 || n != 1) 
    {
      Scierror("Struct are just 1x1 \n");
      nsp_mex_errjump();
    }
  if (( H = nsp_hcreate(NVOID,nfields)) == NULLHASH)
    {
      nsp_mex_errjump();
    }
  return NSP_OBJECT(H);
}

/**
 * mxSetField:
 * @pa: a #mxArray object 
 * @i: an integer 
 * @fieldname: a string 
 * @value: an nsp object. 
 *
 * The name of object @value is set to @fieldname and @value
 * is inserted in hash table @pa without copy. @i is ignored 
 * since only 1x1 struct are supported. 
 * 
 * (changed April 2007)
 * 
 **/

void mxSetField (mxArray *pa, int i, const char *fieldname, mxArray *value)
{
  NspHash *H = (NspHash *) pa;
  if ( i != 0 )
    {
      Scierror("Struct just have a zero index \n");
      nsp_mex_errjump();
    }
  if ( ! IsHash(pa) ) nsp_mex_errjump();
  /* 
  if ((Obj =nsp_object_copy_and_name(fieldname,value))== NULLOBJ)
    nsp_mex_errjump();
  */

  if ( nsp_object_set_name((NspObject *) value,fieldname)== FAIL)
    nsp_mex_errjump();

  if (nsp_hash_enter(H,NSP_OBJECT(value))==FAIL)
    nsp_mex_errjump();
}

/**
 * mxGetNumberOfDimensions:
 * @ptr: an mxArray 
 * 
 * returns the number of dimensions of given array @ptr.
 * 
 * Return value: an integer 
 **/
int mxGetNumberOfDimensions (const mxArray *ptr)
{
  return 2;
}

/**
 * mxGetNumberOfFields:
 * @ptr: an mxArray 
 * 
 * returns the number of fields of given array @ptr.
 * 
 * Return value: an integer 
 **/

int mxGetNumberOfFields (const mxArray *ptr)
{
  if ( ! IsHash(ptr) ) return 0;
  return ((NspHash *) ptr)->filled;
}


/**
 * mxIsChar:
 * @ptr: a #mxArray 
 * 
 * checks if @ptr is a matrix of strings 
 * 
 * Return value: %TRUE or %FALSE 
 **/

bool mxIsChar(const mxArray *ptr)
{
  return  IsSMat(ptr) ;
}

/**
 * mexWarnMsgTxt:
 * @error_msg: a string 
 * 
 * printf the wraning message in the current output stream
 * 
 **/

void mexWarnMsgTxt(const char *error_msg)
{
  Sciprintf(error_msg);
}

/**
 * mxGetInf:
 * 
 * returns <literal>%inf</literal>. 
 * 
 * Return value: ieee Infinity as a double
 **/

double mxGetInf(void)
{
  double d=0;d=1/d;

/**
 * mxGetNaN:
 * 
 * returns <literal>%nan</literal>. 
 * 
 * Return value: ieee Nan as a double 
 **/
  return d;
}
double mxGetNaN(void)
{
  double d=0;d=d/d;  
  return d;
}

/**
 * mxGetEps:
 * 
 * returns <literal>%eps</literal>. 
 * 
 * Return value: returns <literal>nsp_dlamch("e")</literal>
 **/

double mxGetEps(void)
{
  return nsp_dlamch("e");
}

/**
 * mxIsInf:
 * @x: a double 
 * 
 * checks if @x is plus or minus infinity 
 * 
 * Return value: %TRUE or %FALSE 
 **/

bool mxIsInf(double x)
{
  return isinf(x);
}

/**
 * mxIsFinite:
 * @x: a double 
 * 
 * checks if @x is finite. 
 * 
 * Return value: %TRUE or %FALSE 
 **/

bool mxIsFinite(double x)
{
  return finite(x);
}

/**
 * mxIsNaN:
 * @x: a double 
 * 
 * checks if @x is finite by calling finite()
 * 
 * Return value: %TRUE or %FALSE 
 **/

bool mxIsNaN(double x)
{
  return isnan(x);
}

/**
 * mxGetNumberOfElements:
 * @ptr: a #mxArrsy 
 * 
 * returns the number of elements contained in a matrix. 
 * Except for string matrices for which the total number of 
 * characters contained in the strings is returned.
 * 
 * Return value: an integer 
 **/

int mxGetNumberOfElements(const mxArray *ptr)
{
  if ( IsSMat(ptr) )
    {
      int n=0,i;
      if ( ((NspSMatrix*) ptr)->mn == 0 ) return 0;
      for ( i=0 ; i < ((NspSMatrix*) ptr)->mn ; i++)
	n += strlen(((NspSMatrix*) ptr)->S[i]);
      return n;
    }
  else if ( IsHash(ptr) )
    {
      /* hash table are seen as 1x1 struct */
      return 1;
    }
  return nsp_object_get_size(ptr,0);
}


/**
 * mxIsStruct:
 * @ptr: a #mxArray 
 * 
 * Checks if @ptr is a #Nsphash !
 * 
 * Return value: %TRUE or %FALSE 
 **/

bool mxIsStruct(const mxArray *ptr)
{
  return IsHash(ptr);
}

/**
 * mxIsCell:
 * @ptr: a #mxArray 
 * 
 * checks if @ptr is a #NspCells
 * 
 * Return value: %TRUE or %FALSE 
 **/

bool mxIsCell(const mxArray *ptr)
{
  return IsCells(ptr);
}


/**
 * mxGetCell:
 * @ptr: a #mxArray 
 * @index: an integer 
 *
 * checks that @ptr is a #NspCells and returns a 
 * pointer to the @index-element stored in @ptr. 
 *
 * Return value: a #mxArray 
 **/

mxArray *mxGetCell(const mxArray *ptr, int index)
{
  NspCells *C = (NspCells *) ptr;
  if ( ! IsCells(ptr) ) nsp_mex_errjump();
  if ( ! (index >= 0 && index < C->mn )) nsp_mex_errjump();
  return C->objs[index];
}

/**
 * mxSetCell:
 * @ptr: a #mxArray 
 * @index: an integer 
 * @value: a #mxAarray
 * 
 * checks that @ptr is a #NspCells and stores object @value 
 * at indice @index.
 * 
 **/

void mxSetCell(mxArray *ptr, int index, mxArray *value)
{
  NspCells *C = (NspCells *) ptr;
  if ( ! IsCells(ptr) ) nsp_mex_errjump();
  if ( nsp_cells_set_element(C, index,value) == FAIL) 
    {
      nsp_mex_errjump();
    }
}


/**
 * mxCreateCellMatrix:
 * @m: an integer 
 * @n: an integer 
 * 
 * Creates and returns a new #NspCells of size mxn.
 * 
 * Return value: a new #mxArray 
 **/

mxArray *mxCreateCellMatrix(int m, int n)
{
  NspCells *A;
  if ((A = nsp_cells_create(NVOID,m,n) ) == NULLCELLS) nsp_mex_errjump();
  return NSP_OBJECT(A);
}

/**
 * mxCreateCellArray:
 * @ndim: an integer 
 * @dims: an array of size @ndim 
 * 
 * 
 * Creates and returns a new #NspCells of size given by @dims. 
 * Note that it only works for two dimensionnal cells.
 * 
 * Return value:  a new #mxArray 
 **/

mxArray *mxCreateCellArray(int ndim, const int *dims)
{
  NspCells *A=NULL;
  if ( ndim > 2 )
    {
      Scierror("Error: Nsp cells are restricted to mxn cells\n");
      nsp_mex_errjump();
    }
  switch ( ndim ) 
    {
    case 0: 
      if ((A = nsp_cells_create(NVOID,0,0)) == NULLCELLS) nsp_mex_errjump();
      break;
    case 1: 
      if ((A = nsp_cells_create(NVOID,1,dims[0]) ) == NULLCELLS) nsp_mex_errjump();
      break;
    case 2: 
      if ((A = nsp_cells_create(NVOID,dims[0],dims[1]) ) == NULLCELLS) nsp_mex_errjump();
      break;
    }
  return NSP_OBJECT(A);
}


/**
 * mxCalcSingleSubscript:
 * @ptr: an #mxArray
 * @nsubs: an integer 
 * @subs: an int array of size @nsubs
 * 
 * Computes a unique indice from a set of @nsubs indices given in 
 * array @subs. The unique indice can be used instead of the @subs array to 
 * access the same element of array @ptr.
 * 
 * Return value: an indice as an integer 
 **/

int mxCalcSingleSubscript(const mxArray *ptr, int nsubs, const int *subs)
{
  int k, retval=0, coeff=1;
  int *dims = mxGetDimensions(ptr);
  for ( k = 0 ; k < nsubs ; k++) 
    {
      retval += subs[k]*coeff;
      coeff *= dims[k];
    }
  FREE(dims)
  return retval;
}

/* temp : XXXXXXX  dims must be freed by caller !
 *
 */

/**
 * mxGetDimensions:
 * @ptr: an #mxArray
 * 
 * returns a pointer to an allocated array of int filled 
 * with the dimensions of array @ptr. The returned array 
 * should be freed by the caller. 
 * 
 * Return value: an int pointer 
 **/
int *mxGetDimensions(const mxArray *ptr)
{
  int  number_of_dimensions = mxGetNumberOfDimensions( ptr );
  int *dims = mxCalloc(number_of_dimensions, sizeof(int));
  if ( dims == NULL) return NULL;
  dims[0] = mxGetM( ptr );
  dims[1] = mxGetN( ptr );
  return dims;
}

/**
 * mxGetClassID:
 * @ptr: an #mxArray
 * 
 * returns the id of the array @ptr 
 * 
 * Return value: an #mxClassID 
 **/

mxClassID mxGetClassID(const mxArray *ptr) 
{
  int id=((NspTypeBase *) ptr->basetype)->id;
  if ( ptr == NULL )  nsp_mex_errjump();
  if (id == nsp_type_cells_id) return mxCELL_CLASS;
  else if ( id ==     nsp_type_hash_id ) return mxSTRUCT_CLASS;
  else if ( id ==     nsp_type_smatrix_id) return mxCHAR_CLASS;
  else if ( id ==     nsp_type_bmatrix_id) return mxLOGICAL_CLASS;
  else if ( id ==     nsp_type_matrix_id) return mxDOUBLE_CLASS;
  else if ( id ==     nsp_type_object_id) return mxOBJECT_CLASS;
  else if ( id ==     nsp_type_spcolmatrix_id) return mxSPARSE_CLASS;
  else if ( id ==     nsp_type_imatrix_id) 
    {
      switch(((NspIMatrix *)ptr)->itype)
	{	
	case nsp_gint: return mxINT32_CLASS;  break;			
	case nsp_guint: return mxUINT32_CLASS;  break;			
	case nsp_gshort: return mxINT16_CLASS;  break;			
	case nsp_gushort:return mxUINT16_CLASS;  break;			
	case nsp_glong: return mxINT32_CLASS ; break;			
	case nsp_gulong:  return mxUINT32_CLASS; break;			
	case nsp_gint8: return mxINT8_CLASS; break;			
	case nsp_guint8:  return mxUINT8_CLASS;break;			
	case nsp_gint16:return mxINT16_CLASS;  break;			
	case nsp_guint16:  return mxUINT16_CLASS; break;			
	case nsp_gint32: return mxINT32_CLASS;  break;			
	case nsp_guint32:  return mxUINT32_CLASS; break;			
	case nsp_gint64: return mxINT64_CLASS;  break;			
	case nsp_guint64: return mxUINT64_CLASS;  break;
	}
      return mxUNKNOWN_CLASS;
    }
  else if ( id ==     nsp_type_plist_id ) return mxFUNCTION_CLASS;
  else  return mxUNKNOWN_CLASS;
}

/**
 * mxMalloc:
 * @n: size of array to allocate 
 * 
 * Return value: a newly allocated array of char
 **/
void *mxMalloc(size_t n)
{
  return malloc(sizeof(char)*n);
}

/**
 * mxDestroyArray:
 * @ptr: an #mxArray
 * 
 * destroy the object @ptr.
 * Note that objects are destroyed if they have no names. 
 * Thus we preserve array which are stored in a struct 
 * via mxSetField(). 
 **/

void mxDestroyArray(mxArray *ptr)
{
  nsp_void_object_destroy(&ptr);
}

/**
 * mxDestroyNamedArray:
 * @ptr: an #mxArray
 * 
 * destroy the object @ptr even if object has a name. 
 * see #mxDestroyArray for the named case.
 **/

void mxDestroyNamedArray(mxArray *ptr)
{
  nsp_object_destroy(&ptr);
}

/**
 * mxGetName:
 * @ptr: an #mxArray
 * 
 * returns the name of the array @ptr
 * 
 * Return value: a pointer to constant string
 **/

const char * mxGetName(const mxArray *ptr) 
{
  return nsp_object_get_name(ptr);
}

/**
 * mexPutVariable:
 * @workspace: "global", "base" or "caller"
 * @var_name: name to give to the #mxArray.
 * @array_ptr: Pointer to an #mxArray.
 * 
 * Puts the given array in the given workspace with the 
 * given name.
 * 
 * Return value: %OK or %FAIL
 **/

int mexPutVariable(const char *workspace, const char *var_name, mxArray *array_ptr)
{
  if (nsp_object_set_name(array_ptr,var_name) == FAIL) return FAIL;
  if ( nsp_endmex_backconvert(array_ptr)== FAIL) return FAIL;
  return  mexPutArray(array_ptr,workspace);
}

/**
 * mxCreateCharMatrixFromStrings:
 * @m: an integer 
 * @str: an array of strings 
 * 
 * returns an array of strings of size @mx1 filled with 
 * strings (copy) from the array @str.
 * 
 * Return value: a new #NspSMatrix or %NULLSMAT
 **/
mxArray *mxCreateCharMatrixFromStrings(int m, const char **str)
{
  NspSMatrix *A;
  if ((A = nsp_smatrix_create_from_array(NVOID,m,str))== NULL) 
    nsp_mex_errjump();
  return NSP_OBJECT(A);
}

/**
 * mxDuplicateArray:
 * @in: an #mxArray 
 * 
 * returns a copy of @in 
 * 
 * Return value: a new #mxArray or %NULL 
 **/
mxArray *mxDuplicateArray(const mxArray *in)
{
  return ( in != NULL ) ? nsp_object_copy(in) : NULL;
}

/**
 * mxSetName:
 * @array_ptr: Pointer to an #mxArray.
 * @var_name: a string 
 * 
 * changes the name of object @array_ptr which is set to @var_name
 **/

void mxSetName(mxArray *array_ptr,const char *var_name)
{
  nsp_object_set_name(array_ptr,var_name);
}

/**
 * mexPutArray:
 * @array_ptr: Pointer to an #mxArray.
 * @workspace: "global", "base" or "caller"
 * 
 * Puts the given array in the given workspace. 
 * Note that the array must have a name.
 * 
 * Return value: %OK or %FAIL.
 **/

int mexPutArray( mxArray *array_ptr,const char *workspace)
{
  if ( Ocheckname(array_ptr,NVOID) ) return FAIL;
  if ( strcmp(workspace,"global")==0) 
    {
      return  nsp_global_frame_replace_object(array_ptr);
    }
  else if ( strcmp(workspace,"base")==0) 
    {
      return  nsp_toplevel_frame_replace_object(array_ptr);
    }
  else if ( strcmp(workspace,"caller")==0) 
    {
      return  nsp_frame_replace_object(array_ptr,-1);
    }
  else 
    {
      Scierror("Error: mexPutVariable unknown workspace id %s\n",workspace);
      Scierror("       allowed workspaces are global base or caller\n");
      nsp_mex_errjump();      
    }
  return OK;
}


/**
 * mexEvalString:
 * @command: a string 
 * 
 * evaluates the nsp expression given by @command and returns 0 in case of success
 * and -1 in case of failure.
 * 
 * Return value: -1 or 0 
 **/

int mexEvalString(char *command)
{
  int display=FALSE,echo =FALSE,errcatch=TRUE,pausecatch=TRUE;
  if ( nsp_parse_eval_from_string(command,display,echo,errcatch,pausecatch) < 0) 
    return -1;
  else 
    return 0;
}

/**
 * mxGetNzmax:
 * @array_ptr: Pointer to an #mxArray.
 * 
 * Get the size of arrays allocated to keep the 
 * non null values of sparse matrix pointed by @array_ptr
 * 
 * Return value: an integer 
 **/

int mxGetNzmax(const mxArray *array_ptr)
{
  NspSpColMatrix *A=(NspSpColMatrix *) array_ptr;
  if ( array_ptr == NULL)
    {
      Scierror("Error: mxGetNzmax on a non allocated ptr\n");
      nsp_mex_errjump(); 
    }
  if ( A->convert != 't' ) 
    {
      if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
    }
  /* get nnz on the triplet */
  return A->triplet.Aisize;
}


/**
 * mxSetNzmax:
 * @array_ptr: Pointer to an #mxArray.
 * @n: an integer 
 * 
 * 
 * 
 * Return value: 
 **/

int mxSetNzmax(mxArray *array_ptr,int n)
{
  if (IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A=(NspSpColMatrix *) array_ptr;
      if ( array_ptr == NULL)
	{
	  Scierror("Error: mxGetNzmax on a non allocated ptr\n");
	  nsp_mex_errjump(); 
	}
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.Aisize = n ;
      /* nsp_spcol_realloc_col_triplet(A,n);*/
      return OK;
    }
  else
    {
      Scierror("Error in %s: mxGetPr failed\n","mex");
      nsp_mex_errjump();
    }
  return FAIL;
}


/**
 * mexCallMATLAB:
 * @nlhs: Number of desired output arguments.
 * @plhs: Pointer to an array of mxArray (NspObject).
 * @nrhs: Number of input arguments.
 * @prhs: Pointer to an array of input arguments.
 * @command_name: string containing the name of the function to be executed.
 *  
 * performs a function call, the function is given by its name.
 * In case of failure a longjump returns control to Nsp.
 *
 * Note: In Matlab, command_name can contain an operator name this is to be done here (XXX).
 * Note: the returned arguments in plhs can be safely destroyed if necessary, Note that 
 *       they are not automatically destroyed by Nsp if they are not stored in the mex 
 *       plhs array.
 *
 * Return value: 0 if successful, and a nonzero value if unsuccessful.
 **/

/**
 * mexCallNsp:
 * @nlhs: Number of desired output arguments.
 * @plhs: Pointer to an array of mxArray (NspObject).
 * @nrhs: Number of input arguments.
 * @prhs: Pointer to an array of input arguments.
 * @command_name: string containing the name of the function to be executed.
 *  
 * performs a function call,  the function is given by its name.
 * In case of failure a longjump returns control to Nsp.
 *
 * Note: In Matlab, command_name can contain an operator name this is to be done here (XXX).
 * Note: the returned arguments in plhs can be safely destroyed if necessary, Note that 
 *       they are not automatically destroyed by Nsp if they are not stored in the mex 
 *       plhs array.
 *
 * Return value: 0 if successful, and a nonzero value if unsuccessful.
 **/

/**
 * mexCallScilab:
 * @nlhs: Number of desired output arguments.
 * @plhs: Pointer to an array of mxArray (NspObject).
 * @nrhs: Number of input arguments.
 * @prhs: Pointer to an array of input arguments.
 * @command_name: string containing the name of the function to be executed.
 *  
 *
 * performs a function call, the function is given by its name.
 * In case of failure a longjump returns control to Nsp.
 *
 * Note: In Matlab, command_name can contain an operator name this is to be done here (XXX).
 * Note: the returned arguments in plhs can be safely destroyed if necessary, Note that 
 *       they are not automatically destroyed by Nsp if they are not stored in the mex 
 *       plhs array.
 *
 * Return value: 0 if successful, and a nonzero value if unsuccessful.
 **/

int mexCallMATLAB(int nlhs, mxArray *plhs[], int nrhs,
		  mxArray *prhs[],const char *command_name)
{
  int i;
  for ( i= 0 ; i < nrhs ; i++) 
    {
      /* be sure that rhs arguments have names to protect them */
      if ( Ocheckname(prhs[i],NVOID) )
	{
	  nsp_object_set_name(prhs[i],"@mex");
	}
    }
  if ( nsp_gtk_eval_function_by_name(command_name,prhs,nrhs,plhs,&nlhs)== FAIL) 
    {
      /* this could be traped if requested by user */
      nsp_mex_errjump();
      return 1;
    }
  return 0;
}




/**
 * mxIsEmpty:
 * @array_ptr: Pointer to an #mxArray.
 * 
 * checks if given mxArray has empty dimension or not.
 * 
 * Return value: Logical 1 (true) if the mxArray is empty, 
 * and logical 0 (false) otherwise.
 **/

bool mxIsEmpty(const mxArray *array_ptr)
{
  if (array_ptr == NULL
      || nsp_object_get_size(array_ptr,1) == 0 
      || nsp_object_get_size(array_ptr,2) == 0 )
    return TRUE;
  else 
    return FALSE;
}


/**
 * mexMakeArrayPersistent:
 * @array_ptr: an #mxArray 
 * 
 * 
 * Make mxArray persist after MEX-file completes where 
 * @array_ptr is and #mxArray created by a mxCreate* routine.
 * 
 * Note that by default with Nsp array create by mxCreate* routines 
 * are persistent and if you create a persistent mxArray which is not 
 * used as an input or output value, you are responsible for 
 * destroying when it is no more used. 
 * 
 * In Nsp the array created by mxCreate* are not automatically 
 * freed when quiting a mex invocation. Nsp takes in charge frees 
 * and copies just for variables stored in plhr and prhs.
 *
 */

void mexMakeArrayPersistent(mxArray *array_ptr)
{
  /* just add a name to be sure that the array is really 
   * persistent i.e not freed if transmited to 
   * mexCallMATLAB
   */
  if ( array_ptr == NULL ) return;
  if ( Ocheckname(array_ptr,NVOID) )
    {
      nsp_object_set_name(array_ptr,"@mex");
    }
}


/**
 * mxCreateLogicalScalar:
 * @value: 1 or 0
 * 
 * Create scalar NspBMat
 * value
 *  The desired logical value, logical 1 (true) or logical 0 (false), 
 *  to which you want to initialize the array.
 * 
 * 
 * Return value: a newly allocated  NspBMat 
 **/

mxArray *mxCreateLogicalScalar(mxLogical value)
{
  NspBMatrix *A;
  if ((A=nsp_bmatrix_create(NVOID,1,1))==NULLBMAT) 
    nsp_mex_errjump();
  A->B[0]=value;
  return NSP_OBJECT(A);
}

/**
 * mxIsLogicalScalarTrue:
 * @array_ptr: Pointer to an #mxArray.
 *
 * Determine whether scalar mxArray of class mxLogical is true
 * 
 * Return value:  Logical 1 (true) if the value of the mxArray's logical,
 *  scalar element is true, and logical 0 (false) otherwise.
 **/

bool mxIsLogicalScalarTrue(const mxArray *array_ptr)
{
  if ( IsBMat(array_ptr) && ((NspBMatrix *) array_ptr)->mn == 1
       && ((NspBMatrix *) array_ptr)->B[0] == TRUE)
    {
      return TRUE;
    }
  else if ( IsMat(array_ptr) && ((NspMatrix *) array_ptr)->mn == 1 
	 && ((NspMatrix *) array_ptr)->rc_type == 'r' 
	 && ((NspMatrix *) array_ptr)->R[0] != 0 )
    {
      return TRUE;
    }
  return FALSE;
}


/**
 * mxIsLogicalScalar:
 * @array_ptr: Pointer to an #mxArray.
 * 
 * Determine whether scalar #mxArray is of class mxLogical
 * 
 * Return value:  Logical 1 (true) if the #mxArray is of class mxLogical 
 * and has 1-by-1 dimensions, 
 * and logical 0 (false) otherwise.
 **/

bool mxIsLogicalScalar(const mxArray *array_ptr)
{
  if ( IsBMat(array_ptr) && ((NspBMatrix *) array_ptr)->mn == 1 ) 
    {
      return TRUE;
    }
  else if ( IsMat(array_ptr) && ((NspMatrix *) array_ptr)->mn == 1 
	    && ((NspMatrix *) array_ptr)->rc_type == 'r' )
    {
      return TRUE;
    }
  return FALSE;
}

/* mxIsLogical: 
 * bool mxIsLogical(const #mxArray *array_ptr);
 * array_ptr: Pointer to an #mxArray.
 * Description

 */

/**
 * mxIsLogical:
 * @array_ptr: Pointer to an #mxArray.
 * 
 * Use mxIsLogical to determine whether data is  treated as Boolean (logical).
 * In Nsp it is true if array_ptr is a BMat 
 * 
 * Return value: 
 * Logical 1 (true) if array_ptr points to a logical mxArray, and 
 * logical 0 (false) otherwise.
 **/

bool mxIsLogical(const mxArray *array_ptr)
{
  return IsBMat(array_ptr);
}


/**
 * mexMakeMemoryPersistent:
 * @ptr: a void pointer
 * 
 * Make allocated memory persistant after MEX-function completes
 * Note that by default with Nsp memory allocated by mxMalloc() or mxCalloc() 
 * is persistent. you are responsible for freeing memory when it is no more used. 
 * 
 * 
 * By default, memory allocated by Nsp is not freed automatically when 
 * the MEX-file finishes. 
 *
 */

void mexMakeMemoryPersistent(void *ptr)
{
}


/**
 * mexLock:
 *
 * set the lock status of MEX-file to TRUE. 
 * To unlock a MEX-file, call mexUnlock.
 * This feature is not used in Nsp.
 */

void mexLock(void)
{
}

/**
 * mexUnlock:
 *
 * Unsets the lock flag of a MEX-file. No effects in Nsp.
 */

void mexUnlock(void)
{
}

/**
 * mexIsLocked:
 *
 * returns the MEX-file lock status, 1 (true) if the MEX-file is locked, logical 0 (false) if 
 * the file is unlocked. Since in nsp lock mechanism is not implemented this function always 
 * returns: FALSE. 
 *
 */

bool mexIsLocked(void)
{
  return FALSE;
}


/**
 * mxGetData:
 * @array_ptr: an #mxArray 
 * 
 * returns a pointer to the array which contains values of an #mxArray 
 * object as 
 *
 * Return value: a void pointer 
 **/

void *mxGetData(const mxArray *array_ptr)
{
  if ( IsMat(array_ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  array_ptr;
      /* be sure that matrix is matlab converted */
      A = Mat2mtlb_cplx (A);
      return A->R;
    }
  if ( IsBMat(array_ptr)) 
    {
      /* must be used with mxLogical */
      return ((NspBMatrix *)  array_ptr)->B;
    }
  else if (IsString(array_ptr))
    {
      return ((NspSMatrix *) array_ptr)->S[0];
    }
	    
  else if ( IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  array_ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      return A->triplet.Pr;
    }
  else if ( IsIMat(array_ptr))
    {
      return ((NspIMatrix *)  array_ptr)->Iv;
    }
  Scierror("Error in %s: mxGetPr failed\n","mex");
  nsp_mex_errjump();
  return NULL;
}

/**
 * mxIsSharedArray:
 * @array_ptr: a #mxArray 
 * 
 * ?
 * 
 * Returns: %FALSE 
 **/

bool mxIsSharedArray(const mxArray *array_ptr)
{
  return false;
}

/**
 * mxUnshareArray:
 * @array_ptr: a #mxArray 
 * 
 * ?
 * 
 **/

void mxUnshareArray(const mxArray *array_ptr)
{
  
}

/**
 * mxCreateSparseLogicalMatrix:
 * @m: an integer 
 * @n: an integer 
 * @nzmax: an integer 
 * 
 * Since sparse boolean matrices are not implemented in 
 * Nsp. This function just returns a new real sparse matrix 
 * of size @mx@n.
 * 
 * 
 * Returns: a new #mxArray
 **/

mxArray *mxCreateSparseLogicalMatrix(int m, int n, int nzmax)
{
  mexWarnMsgTxt("Warning: mxCreateSparseLogicalMatrix creates a standard sparse\n");
  return mxCreateSparse(m,n,nzmax,mxREAL);
}



/**
 * mxGetFieldByNumber:
 * @array_ptr: a new #mxArray 
 * @index: an integer 
 * @field_number: an integer 
 * 
 * Not yet implemented 
 * 
 * Returns: a new #mxArray 
 **/

mxArray *mxGetFieldByNumber(const mxArray *array_ptr, int index, 
			    int field_number)
{
  int i, count=-1;
  NspHash *H = (NspHash *) array_ptr;
  if ( index != 0 )
    {
      Scierror("Struct just have a zero index \n");
      nsp_mex_errjump();
    }
  if ( ! IsHash(array_ptr) ) nsp_mex_errjump();
  for ( i =0 ; i <= H->hsize  ; i ++) 
    {
      Hash_Entry *loc = ((Hash_Entry *) H->htable) + i;
      if ( loc->used && loc->data != NULLOBJ) 
	{
	  count++;
	  if ( count == field_number ) 
	    return    loc->data;
	}
    }
  return NULL;
}

/**
 * mxGetChars:
 * @array_ptr: a #mxArray 
 * 
 * check that @array_ptr is a string matrix of size 1x1 
 * and returns a pointer to the string 
 * 
 * Returns: a pointer to a char array 
 **/

mxChar *mxGetChars(const mxArray *array_ptr)
{
  if ( ! IsSMat(array_ptr) ) nsp_mex_errjump();
  if ( ((NspSMatrix *) array_ptr)->mn == 1)  
    {
      return ((NspSMatrix *) array_ptr)->S[0];
    }
  else 
    {
      Scierror("Error: mxGetChars not implemented for string arrays\n");
      nsp_mex_errjump();      
    }
  return NULL;
}


/**
 * mxGetElementSize:
 * @array_ptr: Pointer to an #mxArray.
 * 
 * computes the number of bytes required to store one element of 
 * the specified mxArray, if successful. Returns 0 on failure. 
 * The primary reason for failure is that array_ptr points to an m
 * xArray having an unrecognized class. If array_ptr points to a a 
 * matrix of pointers (SMat, BMat, cells, etc...)
 * then mxGetElementSize returns the size of a pointer
 * 
 * Return value: the number of bytes or 0 on failure.
 **/


int mxGetElementSize(const mxArray *array_ptr)
{
  unsigned int elt_size; /* size in number of bytes */
  NspTypeBase *type;
  if ( IsSMat(array_ptr)) 
    {
      return sizeof(mxChar);
    }
  else if (( type = check_implements(array_ptr,nsp_type_matint_id)) == NULL )
    {
      return 0;
    }
  elt_size = MAT_INT(type)->elt_size(array_ptr); 
  return elt_size;
}


/**
 * mxCreateNumericArray:
 * @ndim: 
 * @dims: 
 * @class: 
 * @ComplexFlag: 
 * 
 * Only works for  mxDOUBLE_CLASS
 * 
 * Returns: 
 **/

mxArray *mxCreateNumericArray(int ndim, const int *dims, 
			      mxClassID class, mxComplexity ComplexFlag)
{
  if ( ndim == 2 ) 
    {
      if ( class == mxDOUBLE_CLASS )
	{
	  return mxCreateDoubleMatrix(dims[0],dims[1],ComplexFlag);
	}
      if ( class == mxINT8_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_gint8);
	}
      if ( class == mxUINT8_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_guint8);
	}
      if ( class == mxINT16_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_gint16);
	}
      if ( class == mxUINT16_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_guint16);
	}
      if ( class == mxINT32_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_gint32);
	}
      if ( class == mxUINT32_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_guint32);
	}
      if ( class == mxINT64_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_gint64);
	}
      if ( class == mxUINT64_CLASS )
	{
	  return (mxArray *) nsp_imatrix_create(NVOID,dims[0],dims[1], nsp_guint64);
	}
      else if ( class == mxCHAR_CLASS ) 
	{
	  NspSMatrix *S;
	  if ( dims[0] != 1) 
	    {
	      Scierror("Error: mxCreateNumericArray with mxchar only works with dims[0]==1\n");
	    }
	  /* dims[0] strings, each string is of length (strlen) dims[1] */
	  S = nsp_smatrix_create_with_length(NVOID,dims[0],1,dims[1]);
	  /* be sure that string is not null terminated */
	  S->S[0][dims[1]] = '\0';
	  return (mxArray *) S;
	}
    }
  else if ( ndim == 0) 
    {
      if ( class == mxDOUBLE_CLASS )
	{
	  return mxCreateDoubleMatrix(0,0,ComplexFlag);
	}
      else if ( class == mxCHAR_CLASS ) 
	{
	  NspSMatrix *S;
	  /* dims[0] strings, each string is of length (strlen) dims[1] */
	  S = nsp_smatrix_create_with_length(NVOID,0,0,0);
	  /* be sure that string is not null terminated */
	  return (mxArray *) S;
	}
    }
  Scierror("Error: mxCreateNumericArray only works for doubles/char and 2 dims\n");
  nsp_mex_errjump();   
  return NULL;
}

/**
 * mxGetLogicals:
 * @array_ptr: a new #mxArray 
 * 
 * checks that @array_ptr is a boolean matrix 
 * and returns a pointer to the array of int in 
 * which the boolean values are stored.
 * 
 * Returns: a pointer to an int array 
 **/

mxLogical *mxGetLogicals(const mxArray *array_ptr)
{
  if ( IsBMat(array_ptr) && ((NspBMatrix *) array_ptr)->mn != 0 )
    return ((NspBMatrix *) array_ptr)->B;
  else 
    return NULL;
}


/**
 * mxCreateCharArray:
 * @ndim: number of dimensions (Note that the string counts for one dimension)
 * @dims: dimensions (the last one gives the length to be reserved for strings)
 * 
 * Only works for @ndim==2 and creates a #NspSMatrix of size @dims[0]x1 
 * filled with strings of lenght @dims[1].
 * Note that some functions will then only work if dims[0]==1 (for example 
 * mxgetPr() ) since the Matlab and Nsp coding of string matrices differ.
 * 
 * Note also that  mxGetNumberOfElements() only returns correct answer
 * if the string array if filled with non zero values since string length 
 * are computed with strlen
 * 
 * Return value: #NULLOBJ or the newly created #NspSMatrix
 **/

mxArray *mxCreateCharArray(int ndim, const int *dims)
{
  NspSMatrix *A;
  if ( ndim != 2 )
    {
      Scierror("Error: mxCreateCharArray only works for ndim==2 char arrays\n");
      nsp_mex_errjump();      
    }
  if (( A= nsp_smatrix_create_with_length(NVOID,dims[0],1,dims[1]+1))==NULLSMAT) 
    {
      nsp_mex_errjump();       
    }
  else 
    {
      int i,n=dims[1];
      for ( i= 0; i < A->mn ; i++)
	{
	  memset(A->S[i],' ',n);A->S[i][n]='\0';
	}
    }
  if ( dims[0] != 1 ) 
    {
      Sciprintf("Warning: mxCreateCharArray with dims[0] != 1, some functions will not work\n");
    }
  return NSP_OBJECT(A);
}

/**
 * mxCreateStructArray:
 * @ndim: an integer 
 * @dims: an array of int 
 * @nfields: number of fields 
 * @field_names: an array of strings 
 * 
 * just implemented for 1x1 struct. 
 * 
 * 
 * Return value: a new #mxArray 
 **/

mxArray *mxCreateStructArray(int ndim, const int *dims, int nfields,
			     const char **field_names)
{
  switch (ndim) 
    {
    case 1: 
      return mxCreateStructMatrix(1,dims[0],nfields,field_names);
    case 2: 
      return mxCreateStructMatrix(dims[0],dims[1],nfields,field_names);
    default:
      Scierror("Error: mxCreateCharArray only works for dims <= 2\n");
      nsp_mex_errjump();      
    }
  return NULL;
}

/**
 * mxGetClassName:
 * @array_ptr: a new #mxArray 
 * 
 * returns the type of the array @array_ptr as a string pointer
 * 
 * Returns: a string 
 **/
const char *mxGetClassName(const mxArray *array_ptr)
{
  return array_ptr->type->s_type();
}

/**
 * mxSetFieldByNumber:
 * @array_ptr: a new #mxArray 
 * @index: an integer 
 * @field_number: an integer 
 * @value: a #mxArray 
 * 
 * Not implemented. 
 * 
 **/

void mxSetFieldByNumber(mxArray *array_ptr, int index,  
			int field_number, mxArray *value)
{
  Scierror("Error: mxSetFieldByNumber is not implemented\n");
  nsp_mex_errjump();      
}

/**
 * mxGetFieldNameByNumber:
 * @array_ptr: a new #mxArray 
 * @field_number: an integer 
 * 
 * 
 * 
 * Returns: a pointer to a string
 **/

const char *mxGetFieldNameByNumber(const mxArray *array_ptr, 
				   int field_number)
{
  int i, count=-1;
  NspHash *H = (NspHash *) array_ptr;
  if ( ! IsHash(array_ptr) ) nsp_mex_errjump();
  for ( i =0 ; i <= H->hsize  ; i ++) 
    {
      Hash_Entry *loc = ((Hash_Entry *) H->htable) + i;
      if ( loc->used && loc->data != NULLOBJ) 
	{
	  count++;
	  if ( count == field_number ) 
	    return nsp_object_get_name(loc->data);
	}
    }
  return NULL;
}

/* mxCreateLogicalArray to create an N-dimensional mxArray of logical 1 (true) and logical 0 (false) 
 * elements. After creating the mxArray, mxCreateLogicalArray initializes all its elements to 
 * logical 0. mxCreateLogicalArray differs from mxCreateLogicalMatrix in that the latter 
 * can create two-dimensional arrays only.
 */

/**
 * mxCreateLogicalMatrix:
 * @m: an integer 
 * @n: an integer 
 * 
 * returns a new boolean matrix (#NspBMat).
 * 
 * 
 * Returns: a new #mxArray 
 **/

mxArray *mxCreateLogicalMatrix(int m, int n)
{
  int i;
  NspBMatrix *A;
  if ((A = nsp_bmatrix_create(NVOID,m,n)) == NULLBMAT) nsp_mex_errjump();
  for (i=0; i < A->mn ; i++) A->B[i]= 0;
  return NSP_OBJECT(A);
}


/**
 * mxCreateLogicalArray:
 * @ndim:  an integer
 * @dims:  an array of int
 * 
 * returns a new boolean matrix (#NspBMat) of size given by 
 * @dims 
 * 
 * Returns: a new #mxArray 
 **/

mxArray *mxCreateLogicalArray(int ndim, const int *dims)
{
  switch (ndim) 
    {
    case 0 :
      return mxCreateLogicalMatrix(0,0);
    case 1 :  
      return mxCreateLogicalMatrix(1,dims[0]);
    case 2: 
      return mxCreateLogicalMatrix(dims[0],dims[1]);
    default: 
      Scierror("Error: mxCreateLogicalArray only works for ndim <= 2\n");
      nsp_mex_errjump();
    }
  return NULL;
}


/**
 * mxIsDouble:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/
bool mxIsDouble(const mxArray *array_ptr)
{
  if ( IsMat(array_ptr) )
    return true; 
  if ( IsSpColMat(array_ptr) )
    return true;
  return false;
}

/**
 * mxIsInt8:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsInt8(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_gint8)
    return true; 
  return false;
}

/**
 * mxIsInt16:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsInt16(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_gint16)
    return true; 
  return false;
}

/**
 * mxIsInt32:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsInt32(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_gint32)
    return true; 
  return false;
}

/**
 * mxIsInt64:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsInt64(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_gint64)
    return true; 
  return false;
}


/**
 * mxIsUint8:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsUint8(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_guint8)
    return true; 
  return false;
}

/**
 * mxIsUint16:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsUint16(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_guint16)
    return true; 
  return false;
}

/**
 * mxIsUint32:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsUint32(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_guint32)
    return true; 
  return false;
}

/**
 * mxIsUint64:
 * @array_ptr:Pointer to an #mxArray. 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/

bool mxIsUint64(const mxArray *array_ptr)
{
  if ( IsIMat(array_ptr) && ((NspIMatrix *)array_ptr)->itype == nsp_guint64)
    return true; 
  return false;
}






/**
 * mexGetVariable:
 * @workspace: "base","caller" or "global"
 * @var_name:  name of variable to be searched
 * 
 * get a variable in a workspace. A copy of the variable 
 * is returned on success. 
 * 
 * Return value: a new #mxArray or %NULL
 **/

mxArray *mexGetVariable(const char *workspace, const char *var_name)
{
  mxArray *Obj=NULLOBJ;
  if (strcmp(workspace,"caller")==0) 
    {
      Obj = nsp_frame_search_object(var_name);
    }
  else if ( strcmp(workspace,"global")==0) 
    {
      Obj = nsp_global_frame_search_object(var_name);
    }
  else if ( strcmp(workspace,"base")== 0)
    {
      /* this should be changed */
      Obj = nsp_global_frame_search_object(var_name);
    }
  else
    {
      /* 
       * Scierror("Error: mexGetVariable workspace %s is not known\n",workspace);
       * nsp_mex_errjump();
       */
      return NULL;
    }

  if ( Obj != NULL && check_cast (Obj, nsp_type_hobj_id) == TRUE)			
    {									
      if (((NspHobj *)Obj)->htype != 'g') Obj = ((NspHobj *) Obj)->O;
      else 
	{
	  if ((Obj= nsp_global_frame_search_object(NSP_OBJECT(Obj)->name)) == NULLOBJ)
	    {
	      Scierror("Pointer to a global non existant variablen");
	      nsp_mex_errjump();
	    }
	}
    }
  /* 
  if ( Obj != NULL && Obj->basetype == NSP_TYPE_BASE(nsp_type_hobj))
    Obj = ((NspHobj *) Obj)->O;
  */

  if ( Obj != NULL ) Obj=nsp_object_copy(Obj);
  return Obj;
}

/**
 * mexGetVariablePtr:
 * @workspace: "base","caller" or "global"
 * @var_name:  name of variable to be searched
 * 
 * get a variable in a workspace. The variable 
 * is not copied and should only be used for reading.
 * 
 * Return value: a new #mxArray or %NULL
 **/

mxArray *mexGetVariablePtr(const char *workspace, const char *var_name)
{
  mxArray *Obj=NULLOBJ;
  if (strcmp(workspace,"caller")==0) 
    {
      Obj = nsp_frame_search_object(var_name);
    }
  else if ( strcmp(workspace,"global")==0) 
    {
      Obj = nsp_global_frame_search_object(var_name);
    }
  else if ( strcmp(workspace,"base")== 0)
    {
      /* this should be changed */
      Obj = nsp_global_frame_search_object(var_name);
    }
  else
    {
      return NULL;
      /* Scierror("Error: mexGetVariable workspace %s is not known\n",workspace);
       * nsp_mex_errjump();
       */
    }

  if (Obj != NULL && check_cast (Obj, nsp_type_hobj_id) == TRUE)			
    {									
      if (((NspHobj *)Obj)->htype != 'g') Obj = ((NspHobj *) Obj)->O;
      else 
	{
	  if ((Obj= nsp_global_frame_search_object(NSP_OBJECT(Obj)->name)) == NULLOBJ)
	    {
	      Scierror("Pointer to a global non existant variablen");
	      nsp_mex_errjump();
	    }
	}
    }
  /* 
  if ( Obj != NULL &&  Obj->basetype == NSP_TYPE_BASE(nsp_type_hobj))
    Obj = ((NspHobj *) Obj)->O;
  */
  return Obj;
}


/**
 * mexGetArray:
 * @name: name of variable to be searched
 * @workspace: "base","caller" or "global"
 * 
 * deprecated, use mexGetVariable instead. 
 * 
 * Return value: a #mxArray or %NULL
 **/

mxArray *mexGetArray(const char *name, const char *workspace)
{
  return mexGetVariable(workspace,name);
}



/**
 * mxSetLogical:
 * @array_ptr: Pointer to an #mxArray.
 * 
 * cast a #NspMatrix to a #NspBMatrix and converts data.
 * 
 **/

void mxSetLogical(mxArray *array_ptr)
{
  if ( IsMat(array_ptr) && ((NspMatrix *) array_ptr)->rc_type == 'r') 
    {
      int i;
      /* we cast the #NspMatrix to a #NspBMatrix which is always 
       * possible 
       */
      NspBMatrix *M = (NspBMatrix *) array_ptr;      
      M->type = new_type_bmatrix(T_BASE);
      NSP_OBJECT(M)->type =(NspTypeObject *) M->type->surtype;
      NSP_OBJECT(M)->basetype =(NspTypeBase *) M->type;
      for ( i = 0 ; i < M->mn ; i++ ) 
	M->B[i] = (((NspMatrix *) array_ptr)->R[i] != 0.0) ? TRUE : FALSE ;
      return;
    }
  else if ( IsBMat(array_ptr) )
    {
      /* nothing to do */
      return;
    }
  Scierror("Error: mxSetLogical failed, argument cannot be converted to logical\n");
  nsp_mex_errjump();
}


/**
 * _mxAssert:
 * @mess: 
 * @line: 
 * @file: 
 * 
 * 
 **/

void _mxAssert(char *mess, int line, const char *file)
{
  Scierror("Assertion failed at line %d of file %s (%s)\n",line,file,mess); 
  nsp_mex_errjump();
}


/**
 * mxSetPr:
 * @array_ptr: Pointer to an #mxArray.
 * @pr: pointer to an array of double 
 * 
 * Sets the array value of array @array_ptr to the 
 * pointer given by @pr. @pr must points to an 
 * allocated array and must be compatible with the array size.
 * 
 * 
 **/

void mxSetPr(mxArray *array_ptr, double *pr)
{
  if ( IsMat(array_ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  array_ptr;
      /* be sure that matrix is matlab converted */
      A->R = pr ;
    }
  else if ( IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  array_ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.Pr = pr;
    }
  else if ( IsBMat(array_ptr) )
    {
      Sciprintf("Warning: mxGetPr should be replaced by mxGetData for booleans\n");
      ((NspBMatrix *) array_ptr)->B = (int *) pr;
    }
  else 
    {
      Scierror("Error in %s: mxSetPr failed\n","mex");
      nsp_mex_errjump();
    }
}

/**
 * mxSetData:
 * @array_ptr: Pointer to an #mxArray.
 * @pr: a void pointer
 * 
 * Sets the array value of array @array_ptr to the 
 * pointer given by @pr. @pr must points to an 
 * allocated array and must be compatible with the array size and type.
 * 
 * 
 **/

void mxSetData(mxArray *array_ptr, void *pr)
{
  if ( IsMat(array_ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  array_ptr;
      /* be sure that matrix is matlab converted */
      A->R = pr ;
    }
  else if ( IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  array_ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.Pr = pr;
    }
  else if ( IsBMat(array_ptr) )
    {
      Sciprintf("Warning: mxGetPr should be replaced by mxGetData for booleans\n");
      ((NspBMatrix *) array_ptr)->B =  pr;
    }
  else 
    {
      Scierror("Error in %s: mxSetPr failed\n","mex");
      nsp_mex_errjump();
    }
}

/**
 * mxSetPi:
 * @array_ptr: Pointer to an #mxArray.
 * @pi: pointer to an array of double 
 * 
 * Sets the complex array value of array @array_ptr to the 
 * pointer given by @pi. @pi must points to an 
 * allocated array and must be compatible with the array size.
 * It only works for sparse matrices. 
 * 
 **/

void mxSetPi(mxArray *array_ptr, double *pi)
{
  if ( IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  array_ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.Pi = pi;
      return;
    }
  Scierror("Error in %s: mxSetPi failed, use mxSetPr for real+complex values\n","mex");
  nsp_mex_errjump();
}

/**
 * mxSetJc:
 * @array_ptr: Pointer to an #mxArray.
 * @jc: an array of integers 
 * 
 * Sets the Jc array of a sparse matrix @array_ptr to the 
 * pointer given by @jc. @jc must points to an 
 * allocated array and must be compatible with the array size.
 * It only works for sparse matrices. 
 * 
 **/

void mxSetJc(mxArray *array_ptr, int *jc)
{
  if ( IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  array_ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.Jc = jc;
    }
  else 
    {
      Scierror("Error in %s: mxSetJc failed\n","mex");
      nsp_mex_errjump();
    }
  
}

/**
 * mxSetIr:
 * @array_ptr: Pointer to an #mxArray.
 * @ir: an array of integers 
 * 
 * Sets the Ir array of a sparse matrix @array_ptr to the 
 * pointer given by @ir. @ir must points to an 
 * allocated array and must be compatible with the array size.
 * It only works for sparse matrices. 
 * 
 **/

void mxSetIr(mxArray *array_ptr,int *ir)
{
  if ( IsSpColMat(array_ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  array_ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.Ir = ir;
    }
  else 
    {
      Scierror("Error in %s: mxSetIr failed\n","mex");
      nsp_mex_errjump();
    }
}

/**
 * mxRealloc:
 * @ptr: a void pointer 
 * @size: a size 
 * 
 * reallocates the pointer given by @ptr.
 * 
 * Return value: pointer to reallocated zone.
 **/

void *mxRealloc(void *ptr, size_t size)
{
  return realloc(ptr,sizeof(char)*size);
}

/**
 * mxSetN:
 * @ptr: a #mxArray 
 * @n: an integer 
 * 
 * sets the number of rows of a matrix to @n whitout changing 
 * or allocating values. Coherence must be kept by the user. 
 *
 **/

void mxSetN(mxArray *ptr, mwSize n)
{
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  ptr;
      /* be sure that matrix is matlab converted */
      A->n = n;
      A->mn = A->m*A->n;
    }
  else if ( IsSpColMat(ptr) )
    {
      NspSpColMatrix *A = (NspSpColMatrix *) ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.n = n;
    }
  else 
    {
      Scierror("Error in %s: mxSetN failed\n","mex");
      nsp_mex_errjump();
    }
}

/**
 * mxSetM:
 * @ptr: a #mxArray 
 * @m: an integer 
 * 
 * sets the number of columns of a matrix to @n whitout changing 
 * or allocating values. Coherence must be kept by the user. 
 * 
 **/
void mxSetM(mxArray *ptr, mwSize m)
{
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  ptr;
      /* be sure that matrix is matlab converted */
      A->m = m;
      A->mn = A->m*A->n;
    }
  else if ( IsSpColMat(ptr) )
    {
      NspSpColMatrix *A = (NspSpColMatrix *) ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_spcol_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      A->triplet.m = m;
    }
  else 
    {
      Scierror("Error in %s: mxSetM failed\n","mex");
      nsp_mex_errjump();
    }
}

/**
 * mxFreeSparseMtlbTriplet:
 * @ptr: a #mxArray 
 * 
 * utility to free memory used in a Matlab triplet 
 * since this memory is not freed when the sparse matrix 
 * is not directly in rhs or lhs but for example in 
 * a field of a struct or in a cell
 **/

void mxFreeSparseMtlbTriplet(const mxArray *ptr) 
{
  if ( IsSpColMat(ptr))
    {
      NspSpColMatrix *A = (NspSpColMatrix *)  ptr;
      if (A->convert == 't' ) 
	{
	  A->convert = 'v';
	  FREE(A->triplet.Jc);
	  FREE(A->triplet.Ir);
	  FREE(A->triplet.Pr);
	  FREE(A->triplet.Pi);
	}
    }
}


/**
 * mxSparseMtlbTripletTonsp:
 * @ptr: a #mxArray 
 *
 * utility to explicitely back convert a sparse from 
 * its matlab triplet representation to nsp internal.
 * This is usefull when the sparse matrix is in a cell
 * or a struct for which back convertion is not automatic 
 * when quitting mexfile.
 * 
 **/

void mxSparseMtlbTripletTonsp(const mxArray *ptr) 
{
  if ( nsp_spcol_update_from_triplet((NspSpColMatrix *) ptr)==FAIL) 
    {
      nsp_mex_errjump();
    }
}


void mexSetTrapFlag(int trapflag)
{

}


int mexCheck(const char *str,int nbvars) 
{ 
  if ( nbvars != -1 ) 
    fprintf(stderr,"%s %d\r\n",str,nbvars);
  return 0 ;
}

void *mat2fort(const mxArray *ptr, int m, int n)
{ 
  if ( IsMat(ptr)) 
    {
      double *d= nsp_alloc_doubles(2*m*n);
      NspMatrix *M = (NspMatrix *) ptr; 
      if ( d==NULL)
	{
	  nsp_mex_errjump();      
	}
      memcpy(d,M->R,M->mn*sizeof(doubleC));
      return d;
    }
  Scierror("Error in %s: mat2fort failed\n","mex");
  nsp_mex_errjump();
  return NULL;
}

mxArray *fort2mat(void *data,int lda, int m, int n)
{
  NspMatrix *M;
  if ((M = nsp_matrix_create (NVOID, 'c', m, n)) == NULLMAT)
    {
      nsp_mex_errjump();      
    }
  memcpy(M->R,data,m*n*sizeof(doubleC));
  return (mxArray *) M;
}
