/*------------------------------------------------------------------------
 * mexlib  library
 * Copyright (C) 2004-2006 Jean-Philippe Chancelier Enpc/Cermics
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
 * The next comments are just here for information since 
 * mtlb_sparse cells and struct are not yet operational in nsp 
 * 
 *--------------------------------------------------------------------------*/

static void nsp_mex_errjump(void);

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <setjmp.h>

#include "nsp/matrix-in.h"
#include "nsp/smatrix-in.h"
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
  Stack stack;
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
  stack = SciStack;
  stack.first = *lfirst;
  NspFname(stack) = name;
  for (k = 1; k <= rhs ; ++k) 
    {
      NspObject *Obj;
      if (( Obj=nsp_get_object(stack,k)) == NULL) 
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
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * @mexFunction: 
 * 
 * performs a mexFunction call. 
 * 
 * Return value: 
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
  if (( rfl = sigsetjmp(MexEnv,1)) != 0 )
    {
      return RET_BUG;
    }
  nsp_initmex(NspFname(stack),&stack.first,lhs, plhs, rhs, prhs);
  mexFunction(lhs, plhs, rhs,(c_prhs)  prhs);
  if ( lhs <= 0 && plhs[0] != NULL ) lhs = 1;
  nsp_endmex(stack,lhs, plhs, rhs,prhs);
  return Max(0,lhs);
}


static void nsp_mex_errjump(void)
{
  nsp_clearmex();
  siglongjmp(MexEnv,-1);
}


/**
 * mxGetPr:
 * @ptr: a #mxArray 
 * 
 * 
 * 
 * Return value: 
 **/
/*  Get real part of matrix **/

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
  Scierror("Error in %s: mxGetPr failed\n","mex");
  nsp_mex_errjump();
  return NULL;
}

/**
 * mxGetPi:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
/* Get imaginary part of matrix */

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
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
/* Get m dimension of matrix **/

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
 * @ptr: 
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
 * @ptr: 
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
 * @ptr: 
 * 
 * returns the number of rows of mxArray @ptr.
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
 * @ptr: 
 * 
 * 
 * Return value: 1 if @ptr is a string else 0
 **/

int mxIsString(const mxArray *ptr)
{
  return (IsSMat(ptr) && ((NspSMatrix *) ptr)->mn == 1);
} 

/**
 * mxIsNumeric:
 * @ptr: 
 * 
 * Return value: 1 if @ptr is a #NspMatrix or #NspSpMatrix else 0.
 **/

int mxIsNumeric(const mxArray *ptr)
{
  return ( IsMat(ptr) ||  IsSpColMat(ptr) );
}

/**
 * mxIsFull:
 * @ptr: 
 * 
 * Return value: 1 if @ptr is a #NspMatrix else 0.
 **/

int mxIsFull(const mxArray *ptr)
{
  return IsMat(ptr);
}

/**
 * mxIsSparse:
 * @ptr: 
 * 
 * Return value: 1 if @ptr is a #NspSpMatrix else 0.
 **/

int mxIsSparse(const mxArray *ptr)
{
  return IsSpColMat(ptr) ;
}

/**
 * mxIsComplex:
 * @ptr: 
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
 * @ptr: 
 * 
 * 
 * 
 * Return value: a double if @ptr is a scalar else an error jump.
 **/

double mxGetScalar(const mxArray *ptr)
{ 
  if ( IsMat(ptr) && ((NspMatrix *) ptr)->mn == 1
       && ((NspMatrix *) ptr)->rc_type == 'r')
    {
      return ((NspMatrix *) ptr)->R[0];
    }
  else
    {
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

void mexErrMsgTxt(char *error_msg)
{
  Scierror("Error in %s: ","mex");
  Scierror(error_msg);
  Scierror("\n");
  nsp_mex_errjump();
}

/**
 * mxCreateDoubleMatrix:
 * @m: number of rows 
 * @n: number of columns 
 * @it: #mxReal or #mxComplex.
 * 
 * creates a new #NspMatrix of size (mxn). The array 
 * elements are not initialized.
 * 
 * Return value: a new #mxArray or error jump
 **/

mxArray *mxCreateDoubleMatrix(int m, int n,  mxComplexity it)
{
  NspMatrix *A;
  if ( it == mxREAL ) 
    {
      if ((A = nsp_matrix_create(NVOID,'r',m,n) ) == NULLMAT) nsp_mex_errjump();
    }
  else
    {
      if ((A = nsp_matrix_create(NVOID,'c',m,n) ) == NULLMAT) nsp_mex_errjump();
      A->convert = 'c'; /* matab complex style */
    }
  return NSP_OBJECT(A);
}


/**
 * mxCreateScalarDouble:
 * @d: a double
 * 
 * creates a new #NspMatrix of size (1x1) initialized with @d.
 * 
 * Return value: a new #mxArray or error jump
 **/

mxArray *mxCreateScalarDouble(double d)
{
  NspMatrix *A;
  if ((A = nsp_matrix_create_from_array(NVOID,1,1,&d,NULL))  == NULLMAT) 
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
  NspMatrix *A;
  if ( it == 0) 
    {
      if ((A = nsp_matrix_create(NVOID,'r',m,n) ) == NULLMAT) nsp_mex_errjump();
    }
  else
    {
      if ((A = nsp_matrix_create(NVOID,'c',m,n) ) == NULLMAT) nsp_mex_errjump();
      A->convert = 'c'; /* matab complex style */
    }
  return NSP_OBJECT(A);
}

/**
 * mxCalloc:
 * @n: 
 * @size: 
 * 
 * allocates an array of size @n x @size*sizeof(char). 
 * Note that in Nsp the array is not freed when quiting the mex.
 * 
 * Return value: the new allocated array 
 **/

void *mxCalloc(unsigned int n, unsigned int size)
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
 * Return in @str at most @strl characters from first element of 
 * string NspMatrix pointed by @ptr (@ptr is assumed to be a String NspMatrix ).
 * The returned string can be non #NULL terminated.
 * 
 * Return value: 0 on success and error jump in case of error.
 **/

int mxGetString(const mxArray *ptr, char *str, int strl)
{
  nsp_string message;
  const NspSMatrix *A = (const NspSMatrix *) ptr;
  if ( ! IsSMat(ptr) ) nsp_mex_errjump();
  message =nsp_smatrix_elts_concat(A,"",1,"",1);
  if ( message == NULL) 
    {
      nsp_mex_errjump();
    }
  strncpy(str,message,strl);
  nsp_string_destroy(&message);
  return 0;
}

/**
 * mxArrayToString:
 * @ptr: 
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
 * @ptr: 
 * 
 * XXXX : to be done 
 * 
 **/

void mxFreeMatrix (mxArray *ptr)
{
  return ;
}


/**
 * mxFree:
 * @ptr: 
 * 
 * free previously allocated memmory ( mxCalloc() or mxMalloc() )
 **/

void mxFree(void *ptr)
{
  free(ptr);
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
 * @m: 
 * @n: 
 * @nzmax: 
 * @ComplexFlag: 
 * 
 * 
 * Return value: 
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
  return NSP_OBJECT(A);
}

/**
 * mxCreateString:
 * @string: 
 * 
 * 
 * 
 * Return value: 
 **/

mxArray *mxCreateString(const char *string)
{
  NspSMatrix *S;
  if ((S= nsp_smatrix_create(NVOID,1,1,string,(integer)1)) == NULLSMAT ) nsp_mex_errjump();
  return NSP_OBJECT(S);
}


/**
 * mxGetField:
 * @pa: 
 * @i: 
 * @fieldname: 
 * 
 * here pas is supposed to be a Hash Table 
 * the index is not used i.e we only accept 
 * i==0;
 * fieldname ->  const char *fieldname
 * 
 * 
 * Return value: 
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
 * @m: 
 * @n: 
 * @nfields: 
 * @field_names: 
 * 
 * 
 * 
 * Return value: 
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
 * @pa: 
 * @i: 
 * @fieldname: 
 * @value: 
 *
 * A copy of object @value is inserted in @pa.
 * 
 * 
 **/

void mxSetField (mxArray *pa, int i, const char *fieldname, mxArray *value)
{
  NspObject *Obj;
  NspHash *H = (NspHash *) pa;
  if ( i != 0 )
    {
      Scierror("Struct just have a zero index \n");
      nsp_mex_errjump();
    }
  if ( ! IsHash(pa) ) nsp_mex_errjump();
  if ((Obj =nsp_object_copy_and_name(fieldname,value))== NULLOBJ)
    nsp_mex_errjump();
  if (nsp_hash_enter(H,Obj)==FAIL)
    nsp_mex_errjump();
}

/**
 * mxGetNumberOfDimensions:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
int mxGetNumberOfDimensions (const mxArray *ptr)
{
  return 2;
}

/**
 * mxGetNumberOfFields:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/

int mxGetNumberOfFields (const mxArray *ptr)
{
  if ( ! IsHash(ptr) ) return 0;
  return ((NspHash *) ptr)->filled;
}


/**
 * mxIsChar:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/

bool mxIsChar(const mxArray *ptr)
{
  return  IsSMat(ptr) ;
}

/**
 * mexWarnMsgTxt:
 * @error_msg: 
 * 
 * 
 **/

void mexWarnMsgTxt(const char *error_msg)
{
  Sciprintf(error_msg);
}

/**
 * mxGetInf:
 * 
 * Return value: ieee Infinity as a double
 **/

double mxGetInf(void)
{
  double d=0;d=1/d;

/**
 * mxGetNaN:
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
 * Return value: returns <literal>nsp_dlamch("e")</literal>
 **/

double mxGetEps(void)
{
  return nsp_dlamch("e");
}

/**
 * mxIsInf:
 * @x: 
 * 
 * 
 * 
 * Return value: 
 **/

bool mxIsInf(double x)
{
  return isinf(x);
}

/**
 * mxIsFinite:
 * @x: 
 * 
 * 
 * 
 * Return value: 
 **/

bool mxIsFinite(double x)
{
  return finite(x);
}

/**
 * mxIsNaN:
 * @x: 
 * 
 * 
 * 
 * Return value: 
 **/

bool mxIsNaN(double x)
{
  return isnan(x);
}

/**
 * mxGetNumberOfElements:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
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
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/

bool mxIsStruct(const mxArray *ptr)
{
  return IsHash(ptr);
}

/**
 * mxIsCell:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/

bool mxIsCell(const mxArray *ptr)
{
  return IsCells(ptr);
}


/**
 * mxGetCell:
 * @ptr: 
 * @index: 
 * 
 * Return value: 
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
 * @ptr: 
 * @index: 
 * @value: 
 * 
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
 * @m: 
 * @n: 
 * 
 * 
 * 
 * Return value: 
 **/

mxArray *mxCreateCellMatrix(int m, int n)
{
  NspCells *A;
  if ((A = nsp_cells_create(NVOID,m,n) ) == NULLCELLS) nsp_mex_errjump();
  return NSP_OBJECT(A);
}

/**
 * mxCreateCellArray:
 * @ndim: 
 * @dims: 
 * 
 * 
 * 
 * Return value: 
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
 * @ptr: 
 * @nsubs: 
 * @subs: 
 * 
 * 
 * 
 * Return value: 
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
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
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
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
mxClassID mxGetClassID(const mxArray *ptr) 
{
  if ( ptr == NULL )  nsp_mex_errjump();
  /* just return the base type that's enough for mexlib */
  return ((NspTypeBase *) ptr->basetype)->id;
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
 * @ptr: 
 * 
 * 
 **/
void mxDestroyArray(mxArray *ptr)
{
  nsp_object_destroy(&ptr);
}

/**
 * mxGetName:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/

const char * mxGetName(const mxArray *ptr) 
{
  return nsp_object_get_name(ptr);
}

/**
 * mexPutVariable:
 * @workspace: 
 * @var_name: 
 * @array_ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
int mexPutVariable(const char *workspace, const char *var_name, mxArray *array_ptr)
{
  if (nsp_object_set_name(array_ptr,var_name) == FAIL) return FAIL;
  if ( nsp_endmex_backconvert(array_ptr)== FAIL) return FAIL;
  return  mexPutArray(array_ptr,workspace);
}

/**
 * mxCreateCharMatrixFromStrings:
 * @m: 
 * @str: 
 * 
 * 
 * 
 * Return value: 
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
 * @in: 
 * 
 * 
 * 
 * Return value: 
 **/
mxArray *mxDuplicateArray(const mxArray *in)
{
  return ( in != NULL ) ? nsp_object_copy(in) : NULL;
}

/**
 * mxSetName:
 * @array_ptr: 
 * @var_name: 
 * 
 * 
 **/
void mxSetName(mxArray *array_ptr,const char *var_name)
{
  nsp_object_set_name(array_ptr,var_name);
}

/**
 * mexPutArray:
 * @array_ptr: 
 * @workspace: 
 * 
 * 
 * 
 * Return value: 
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
      return  nsp_global_frame_replace_object(array_ptr);
    }
  else if ( strcmp(workspace,"caller")==0) 
    {
      return  nsp_frame_replace_object(array_ptr,-1);
    }
  else 
    {
      Scierror("Error: mexPutVariable unknown workspace id %d\n",workspace);
      nsp_mex_errjump();      
    }
  return OK;
}


/**
 * mexEvalString:
 * @command: 
 * 
 * 
 * 
 * Return value: 
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
 * @array_ptr: 
 * 
 * 
 * 
 * Return value: 
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
 * @array_ptr: 
 * @n: 
 * 
 * 
 * 
 * Return value: 
 **/

int mxSetNzmax(mxArray *array_ptr,int n)
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


/**
 * mexCallMATLAB:
 * @nlhs: Number of desired output arguments.
 * @plhs: Pointer to an array of mxArray (NspObject).
 * @nrhs: Number of input arguments.
 * @prhs: Pointer to an array of input arguments.
 * @command_name: string containing the name of the function to be executed.
 *  
 *
 * Call mexCallMATLAB evaluate a function call the function name given by its name.
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
		  mxArray *prhs[],char *command_name)
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
 * @array_ptr: 
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



/* mexMakeArrayPersistent:
 * 
 * Make mxArray persist after MEX-file completes
 *
 * array_ptr
 *  Pointer to an mxArray created by an mxCreate* routine.
 * 
 * By default, mxArrays allocated by mxCreate* routines are not persistent 
 * in MATLAB. The MATLAB memory management facility automatically frees 
 * nonpersistent mxArrays when the MEX-function finishes. 
 * If you want the mxArray to persist through multiple invocations of the 
 * MEX-function, you must call mexMakeArrayPersistent.
 * 
 * Note If you create a persistent mxArray, you are responsible for 
 * destroying it when the MEX-file is cleared. If you do not destroy a 
 * persistent mxArray, MATLAB will leak memory. See mexAtExit to see how 
 * to register a function that gets called when the MEX-file is cleared. 
 * See mexLock to see how to lock your MEX-file so that it is never cleared. 
 * 
 * In Nsp the array created by mxCreate* are not automatically 
 * freed when quiting a mex invocation. Nsp take in charge frees 
 * and copies just for variables stored in plhr and prhs.
 *
 */

/**
 * mexMakeArrayPersistent:
 * @array_ptr: 
 * 
 * 
 **/

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
 * @value: 
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
 * @array_ptr: 
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
 * @array_ptr: 
 * 
 * Determine whether scalar mxArray is of class mxLogical
 * 
 * Return value:  Logical 1 (true) if the mxArray is of class mxLogical 
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
 * bool mxIsLogical(const mxArray *array_ptr);
 * array_ptr: Pointer to an mxArray.
 * Description

 */

/**
 * mxIsLogical:
 * @array_ptr: 
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


/* mexMakeMemoryPersistent:
 * 
 * Make allocated memory MATLAB persist after MEX-function completes
 * void mexMakeMemoryPersistent(void *ptr);
 * ptr
 *  Pointer to the beginning of memory allocated by one of the MATLAB memory allocation routines.
 * Description
 * 
 * By default, memory allocated by MATLAB is nonpersistent, so it is freed automatically when 
 * the MEX-file finishes. If you want the memory to persist, you must call mexMakeMemoryPersistent.
 * Note    If you create persistent memory, you are responsible for freeing it when the MEX-function 
 * is cleared. If you do not free the memory, MATLAB will leak memory. To free memory, use mxFree. 
 * See mexAtExit to see how to register a function that gets called when the MEX-function is cleared. 
 * See mexLock to see how to lock your MEX-function so that it is never cleared. 
 * 
 */

void mexMakeMemoryPersistent(void *ptr)
{
}


/* mexLock:
 * Prevent MEX-file from being cleared from memory
 * By default, MEX-files are unlocked, meaning that a user can clear them at any time. 
 * Call mexLock to prohibit a MEX-file from being cleared.
 *
 * To unlock a MEX-file, call mexUnlock.
 *
 * mexLock increments a lock count. If you call mexLock n times, you must call mexUnlock n times to unlock your MEX-file.
 */

void mexLock(void)
{
}

/*mexUnlock:
 *
 * Allow MEX-file to be cleared from memory
 * Description
 * By default, MEX-files are unlocked, meaning that a user can clear them at any time. 
 * Calling mexLock locks a MEX-file so that it cannot be cleared. Calling mexUnlock removes 
 * the lock so that the MEX-file can be cleared.
 * 
 * mexLock increments a lock count. If you called mexLock n times, you must call mexUnlock n times to unlock your MEX-file.
 */

void mexUnlock(void)
{
}

/* mexIsLocked:
 * Determine if MEX-file is locked
 * bool mexIsLocked(void);
 * Returns Logical 1 (true) if the MEX-file is locked; logical 0 (false) if 
 * the file is unlocked.
 * 
 * Call mexIsLocked to determine if the MEX-file is locked. By default, 
 * MEX-files are unlocked, meaning that users can clear the MEX-file at any time.
 * 
 */

bool mexIsLocked(void)
{
  return FALSE;
}


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
  Scierror("Error in %s: mxGetPr failed\n","mex");
  nsp_mex_errjump();
  return NULL;
}

bool mxIsSharedArray(const mxArray *array_ptr)
{
  return false;
}

extern void mxUnshareArray(const mxArray *array_ptr)
{
  
}

mxArray *mxCreateSparseLogicalMatrix(int m, int n, int nzmax)
{
  mexWarnMsgTxt("Warning: mxCreateSparseLogicalMatrix only creates a sparse_n");
  return mxCreateSparse(m,n,nzmax,mxREAL);
}



mxArray *mxGetFieldByNumber(const mxArray *array_ptr, int index, 
			    int field_number)
{
  Scierror("Error: mxGetFieldByNumber not implemented\n");
  nsp_mex_errjump();      
  return NULL;
}

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
 * @array_ptr: 
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


mxArray *mxCreateNumericArray(int ndim, const int *dims, 
			      mxClassID class, mxComplexity ComplexFlag)
{
  if ( ndim != 2 && class != mxDOUBLE_CLASS )
    {
      Scierror("Error: mxCreateNumericArray only works for doubles and 2dims\n");
      nsp_mex_errjump();      
    }
  return mxCreateDoubleMatrix(dims[0],dims[1],ComplexFlag);
}

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
 * @ndim: 
 * @dims: 
 * @nfields: 
 * @field_names: 
 * 
 * 
 * 
 * Return value: 
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

const char *mxGetClassName(const mxArray *array_ptr)
{
  return array_ptr->type->s_type();
}

void mxSetFieldByNumber(mxArray *array_ptr, int index,  
			int field_number, mxArray *value)
{
  Scierror("Error: mxSetFieldByNumber is not implemented\n");
  nsp_mex_errjump();      
}



const char *mxGetFieldNameByNumber(const mxArray *array_ptr, 
				   int field_number)
{
  Scierror("Error: mxGetFieldNameByNumber is not implemented\n");
  nsp_mex_errjump();      
  return NULL;
}

/* mxCreateLogicalArray to create an N-dimensional mxArray of logical 1 (true) and logical 0 (false) 
 * elements. After creating the mxArray, mxCreateLogicalArray initializes all its elements to 
 * logical 0. mxCreateLogicalArray differs from mxCreateLogicalMatrix in that the latter 
 * can create two-dimensional arrays only.
 */

mxArray *mxCreateLogicalMatrix(int m, int n)
{
  int i;
  NspBMatrix *A;
  if ((A = nsp_bmatrix_create(NVOID,m,n)) == NULLBMAT) nsp_mex_errjump();
  for (i=0; i < A->mn ; i++) A->B[i]= 0;
  return NSP_OBJECT(A);
}

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
 * @array_ptr: 
 * 
 * Logical 1 (true) if the mxArray stores its data as double-precision, 
 * floating-point numbers, and logical 0 (false) otherwise.
 * 
 * Return value: true or flase;
 **/
bool mxIsDouble(const mxArray *array_ptr)
{
  return  IsMat(array_ptr) ? true: false;
}





/**
 * mexGetVariable:
 * @workspace: "base","caller" or "global"
 * @var_name: 
 * 
 * get a variable in a workspace. A copy of the variable 
 * is returned on success. 
 * 
 * Return value: 
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
      Scierror("Error: mexGetVariable workspace %s is not known\n",workspace);
      nsp_mex_errjump();
    }
  if ( Obj != NULL ) Obj=nsp_object_copy(Obj);
  return Obj;
}


/**
 * mexGetArray:
 * @name: 
 * @workspace: 
 * 
 * deprecated, use mexGetVariable instead. 
 * 
 * Return value: 
 **/

mxArray *mexGetArray(const char *name, const char *workspace)
{
  return mexGetVariable(workspace,name);
}



/**
 * mxSetLogical:
 * @array_ptr: 
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
 * @array_ptr: 
 * @pr: 
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
 * mxSetPi:
 * @array_ptr: 
 * @pi: 
 * 
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
 * @array_ptr: 
 * @jc: 
 * 
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
 * @array_ptr: 
 * @ir: 
 * 
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
 * @ptr: 
 * @size: 
 * 
 * 
 **/

void *mxRealloc(void *ptr, size_t size)
{
  return realloc(ptr,sizeof(char)*size);
}

void mxSetN(mxArray *ptr, mwSize n)
{
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  ptr;
      /* be sure that matrix is matlab converted */
      A->n = n;
      A->mn = A->m*A->n;
    }
  else 
    {
      Scierror("Error in %s: mxSetN failed\n","mex");
      nsp_mex_errjump();
    }
}

void mxSetM(mxArray *ptr, mwSize m)
{
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  ptr;
      /* be sure that matrix is matlab converted */
      A->m = m;
      A->mn = A->m*A->n;
    }
  else 
    {
      Scierror("Error in %s: mxSetM failed\n","mex");
      nsp_mex_errjump();
    }
}
