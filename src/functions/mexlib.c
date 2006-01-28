/*------------------------------------------------------------------------
 * mexlib  library
 * Copyright (C) 2004 Jean-Philippe Chancelier Enpc/Cermics
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

static void nsp_mex_errjump();

#include <string.h>
#include <math.h>
#include <stdio.h>
#include <setjmp.h>

#include "nsp/matrix-in.h"
#include "nsp/smatrix-in.h"
#include "nsp/datas.h"
#include "nsp/parse.h"

/* some nsp function are to be updated for const */

#define MEXLIB 
#include "nsp/mex.h"

/* these two variables are 
 * to be set up corectly to enable reentry in mex 
 */

static int onlyone =0;
static jmp_buf MexEnv;


static void nsp_initmex(char *name,int *lfirst,int lhs,mxArray *plhs[], int rhs,const mxArray *prhs[])
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

static void nsp_endmex(Stack stack,int lhs,mxArray *plhs[],int rhs,const mxArray *prhs[])
{
  int i;
  for ( i= 1 ; i <= Max(lhs,1) ; i++) 
    {
      NspObject *Obj = plhs[i-1];
      if ( Obj != NULL ) 
	{
	  NthObj(i) = Obj;
	  NthObj(i)->ret_pos = i;
	  /* back convert sparses */
	  if (IsSpMat(NthObj(i)))
	    {
	      NspSpMatrix *Sp =(NspSpMatrix *) NthObj(i);
	      if ( nsp_sparse_update_from_triplet(Sp)==FAIL) 
		goto bug;
	    }
	}
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
 * 
 * 
 * Return value: 
 **/

int nsp_mex_wrapper(Stack stack, int rhs, int opt, int lhs,mexfun *mexFunction)
{
  mxArray  *plhs[INTERSIZ];
  const mxArray *prhs[INTERSIZ];
  int rfl;
  if (( rfl = sigsetjmp(MexEnv,1)) != 0 )
    {
      return RET_BUG;
    }
  nsp_initmex(NspFname(stack),&stack.first,lhs, plhs, rhs, prhs);
  mexFunction(lhs, plhs, rhs, prhs);
  if ( lhs <= 0 && plhs[0] != NULL ) lhs = 1;
  nsp_endmex(stack,lhs, plhs, rhs, prhs);
  return Max(0,lhs);
}

static  jmp_buf MexEnv;

static void nsp_mex_errjump()
{
  nsp_clearmex();
  siglongjmp(MexEnv,-1);
}


/**
 * mxGetPr:
 * @ptr: 
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
  else if ( IsSpMat(ptr))
    {
      NspSpMatrix *A = (NspSpMatrix *)  ptr;
      if (A->convert != 't' ) 
	{
	  if ( nsp_sparse_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      return A->triplet.Ax;
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
  else if ( IsSpMat(ptr)) 
    {
      NspSpMatrix *A = (NspSpMatrix *) ptr ;
      if (A->convert != 't' ) 
	{
	  if ( nsp_sparse_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
	}
      return A->triplet.Ax + A->triplet.Aisize;
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

/* int array de taille n+1 tel que 
 *  Jc(j) = indice ds Ir et Pr du premier elets non nul de la j ieme colonne 
 *  Jc(n) est le monbre d'elments non nuls de la matrice 
 **/

/**
 * mxGetJc:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
int *mxGetJc(const mxArray *ptr)
{
  NspSpMatrix *A = (NspSpMatrix *) ptr;
  if ( ! IsSpMat(ptr)) nsp_mex_errjump();
  if (A->convert != 't' ) 
    {
      if ( nsp_sparse_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
    }
  return A->triplet.Ap;
}

/* Get Ir tableaux d'entier de meme taille que # des elts non nuls 
 * contient indice de ligne de chaque element */

/**
 * mxGetIr:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
int *mxGetIr(const mxArray *ptr)
{
  NspSpMatrix *A = (NspSpMatrix *) ptr;
  if ( ! IsSpMat(ptr)) nsp_mex_errjump();
  if (A->convert != 't' ) 
    {
      if ( nsp_sparse_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
    }
  return A->triplet.Ai;
}

/**
 * mxGetN:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
/* Get n dimension of matrix **/

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
 * 
 * Return value: 
 **/
/* Check that object is a String **/

int mxIsString(const mxArray *ptr)
{
  return (IsSMat(ptr) && ((NspSMatrix *) ptr)->mn == 1);
} 

/**
 * mxIsNumeric:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
/*  Numeric : i.e sparse or matrix **/

int mxIsNumeric(const mxArray *ptr)
{
  return ( IsMat(ptr) ||  IsSpMat(ptr) );
}

/**
 * mxIsFull:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
/*  Full : NspMatrix **/

int mxIsFull(const mxArray *ptr)
{
  return IsMat(ptr);
}

/**
 * mxIsSparse:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
/*  Full : NspMatrix **/

int mxIsSparse(const mxArray *ptr)
{
  return IsSpMat(ptr) ;
}

/*  Complex : NspMatrix or Sparse  **/


/**
 * mxIsComplex:
 * @ptr: 
 * 
 * 
 * 
 * Return value: 
 **/
int mxIsComplex(const mxArray *ptr)
{
  if ( IsMat(ptr) )
    {
      return ((NspMatrix *) ptr)->rc_type == 'c' ;
    }
  else if ( IsSpMat(ptr) )
    {
      return ((NspSpMatrix *) ptr)->rc_type == 'c';
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
 * Return value: 
 **/
/*  a Scalar **/

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
 * @m: 
 * @n: 
 * @it: 
 * 
 * 
 * 
 * Return value: 
 **/
/*  New matrix **/

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
 * mxCreateFull:
 * @m: 
 * @n: 
 * @it: 
 * 
 * 
 * 
 * Return value: 
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
 * 
 * 
 * Return value: 
 **/
/* Allocation on the stack */

void *mxCalloc(unsigned int n, unsigned int size)
{
  return malloc(n*size);
}


/**
 * mxGetString:
 * @ptr: 
 * @str: 
 * @strl: 
 * 
 * 
 * Return in str at most strl characters from first element of 
 * string NspMatrix pointed by ptr ( ptr is assumed to be a String NspMatrix )
 * 
 * Return value: 
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
 * Get all the strings of Matrix SMatrix 
 * in one buffer, the string is allocated 
 * and should be freed by the user 
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
 * 
 **/
void mxFree(void *ptr)
{
  free(ptr);
}

/**
 * mexAtExit:
 * @ExitFcn: 
 * 
 * 
 * 
 * Return value: 
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
 * 
 * Return value: 
 **/
mxArray *mxCreateSparse(int m, int n, int nzmax, 
			mxComplexity ComplexFlag)
{
  NspSpMatrix *A;
  if ( ComplexFlag == mxREAL ) 
    {
      if ((A = nsp_spmatrix_create(NVOID,'r',m,n) ) == NULLSP) nsp_mex_errjump();
    }
  else
    {
      if ((A = nsp_spmatrix_create(NVOID,'c',m,n) ) == NULLSP) nsp_mex_errjump();
      A->convert = 'c'; /* matab complex style */
    }
  /* just allocate triplet */
  if ( nsp_sparse_alloc_col_triplet(A,nzmax) == FAIL)  nsp_mex_errjump();
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
mxArray *mxCreateString(char *string)
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

mxArray *mxGetField (const mxArray *pa, int i, char *fieldname)
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
  if (nsp_object_set_name(value,fieldname) == FAIL) return ;
  if (  nsp_hash_enter_copy(H,value)==FAIL)
    {
      nsp_mex_errjump();
    }
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
void mexWarnMsgTxt(char *error_msg)
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

/* temp : dims must be freed by caller !
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
      return  nsp_frame_replace_object(array_ptr);
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
int mxGetNzmax( mxArray *array_ptr)
{
  NspSpMatrix *A=(NspSpMatrix *) array_ptr;
  if ( array_ptr == NULL)
    {
      Scierror("Error: mxGetNzmax on a non allocated ptr\n");
      nsp_mex_errjump(); 
    }
  if ( A->convert != 't' ) 
    {
      if ( nsp_sparse_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
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
  NspSpMatrix *A=(NspSpMatrix *) array_ptr;
  if ( array_ptr == NULL)
    {
      Scierror("Error: mxGetNzmax on a non allocated ptr\n");
      nsp_mex_errjump(); 
    }
  if (A->convert != 't' ) 
    {
      if ( nsp_sparse_set_triplet_from_m(A,TRUE)==FAIL) nsp_mex_errjump();
    }
  return  nsp_sparse_realloc_col_triplet(A,n);
}
