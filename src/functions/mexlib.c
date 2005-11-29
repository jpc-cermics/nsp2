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

/****************************************************
 * C function for C mexfunctions 
 ****************************************************/

/*  A set of constant **/

static int onlyone =0;
static Stack stack;
static jmp_buf MexEnv;

/*
 * FIXME:
 *   we would like this function to be reentrant 
 *   which has to be improved since it is based on 
 *   static variables ....
 *   Il faudrait faire deux choses 
 *      une librairie degradé non réentrante 
 *      si on a besoin de charger un mex binaire 
 *      provenant de matlab 
 *      une librairie améliorée réentrante pour les 
 *      besoin de nsp
 *   je sais pas si la premiere version est vraiment utile !!
 */

static void nsp_initmex(char *name,int *lfirst,int lhs,mxArray *plhs[], int rhs,const mxArray *prhs[])
{
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
  stack.fname = name;
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

static void nsp_endmex(int lhs,mxArray *plhs[],int rhs,const mxArray *prhs[])
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

int nsp_mex_wrapper(Stack stack, int rhs, int opt, int lhs,mexfun *mexFunction)
{
  mxArray  *plhs[INTERSIZ];
  const mxArray *prhs[INTERSIZ];
  int rfl;
  if (( rfl = sigsetjmp(MexEnv,1)) != 0 )
    {
      return RET_BUG;
    }
  nsp_initmex(stack.fname,&stack.first,lhs, plhs, rhs, prhs);
  mexFunction(lhs, plhs, rhs, prhs);
  if ( lhs <= 0 && plhs[0] != NULL ) lhs = 1;
  nsp_endmex(lhs, plhs, rhs, prhs);
  return Max(0,lhs);
}

static  jmp_buf MexEnv;

static void nsp_mex_errjump()
{
  nsp_clearmex();
  siglongjmp(MexEnv,-1);
}


/*  Get real part of matrix **/

double *mxGetPr(const mxArray *ptr)
{
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *)  ptr;
      /* A revoir 
	 if (( A=GetMtlbMat(stack,i)) == NULLMAT)   
	   {
	     nsp_mex_errjump();
	   }
      */
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
  Scierror("Error in %s: mxGetPr failed\n",stack.fname);
  nsp_mex_errjump();
  return NULL;
}

/* Get imaginary part of matrix */

double *mxGetPi(const mxArray *ptr)
{  
  if ( IsMat(ptr)) 
    {
      NspMatrix *A = (NspMatrix *) ptr ;
      /* 
      if ((A=GetMtlbMat(stack,i)) == NULLMAT) 
      nsp_mex_errjump();
      */
      if ( A->rc_type == 'r' )
	{
	  return NULL;
	}
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
      Scierror("Error in %s: mxGetPr failed\n",stack.fname);
      nsp_mex_errjump();
    }
  return NULL;
}

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

/* Check that object is a String **/

int mxIsString(const mxArray *ptr)
{
  return (IsSMat(ptr) && ((NspSMatrix *) ptr)->mn == 1);
} 

/*  Numeric : i.e sparse or matrix **/

int mxIsNumeric(const mxArray *ptr)
{
  return ( IsMat(ptr) ||  IsSpMat(ptr) );
}

/*  Full : NspMatrix **/

int mxIsFull(const mxArray *ptr)
{
  return IsMat(ptr);
}

/*  Full : NspMatrix **/

int mxIsSparse(const mxArray *ptr)
{
  return IsSpMat(ptr) ;
}

/*  Complex : NspMatrix or Sparse  **/


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

/*  Error + Jump  **/

void mexErrMsgTxt(char *error_msg)
{
  Scierror("Error in %s: ",stack.fname);
  Scierror(error_msg);
  Scierror("\n");
  nsp_mex_errjump();
}

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

/* Allocation on the stack */

void *mxCalloc(unsigned int n, unsigned int size)
{
  return malloc(n*size);
}


/**************************************************************
 * Return in str at most strl characters from first element of 
 * string NspMatrix pointed by ptr ( ptr is assumed to be a String NspMatrix )
 **************************************************************/

/* Get all the strings of Matrix SMatrix 
 * in one buffer 
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

/* Get all the strings of Matrix SMatrix 
 * in one buffer, the string is allocated 
 * and should be freed by the user 
 * XXXX mxFree does not work 
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

      
/* libere le CreateFull **/

void mxFreeMatrix (mxArray *ptr)
{
  return ;
}

/* libere le Calloc **/

void mxFree(void *ptr)
{
  free(ptr);
}

/* exit function : mexAtExit : **/

int mexAtExit(void (*ExitFcn)(void))
{
  return 0;
}


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


/**************************************************************
 * Create on Scilab Stack a 1x1 string matrix filled with string
 **************************************************************/

mxArray *mxCreateString(char *string)
{
  NspSMatrix *S;
  if ((S= nsp_smatrix_create(NVOID,1,1,string,(integer)1)) == NULLSMAT ) nsp_mex_errjump();
  return NSP_OBJECT(S);
}

/* here pas is supposed to be a Hash Table 
 * the index is not used i.e we only accept 
 * i==0;
 * fieldname ->  const char *fieldname
 */


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

/*
 *
 */

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

int mxGetNumberOfDimensions (const mxArray *ptr)
{
  return 2;
}


int mxGetNumberOfFields (const mxArray *ptr)
{
  if ( ! IsHash(ptr) ) return 0;
  return ((NspHash *) ptr)->filled;
}


bool mxIsChar(const mxArray *ptr)
{
  return  IsSMat(ptr) ;
}

void mexWarnMsgTxt(char *error_msg)
{
  Sciprintf(error_msg);
}


double mxGetInf(void)
{
  double d=0;d=1/d;
  return d;
}
double mxGetNaN(void)
{
  double d=0;d=d/d;  
  return d;
}

double mxGetEps(void)
{
  return nsp_dlamch("e");
}

bool mxIsInf(double x)
{
  return isinf(x);
}

bool mxIsFinite(double x)
{
  return finite(x);
}

bool mxIsNaN(double x)
{
  return isnan(x);
}

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


bool mxIsStruct(const mxArray *ptr)
{
  return IsHash(ptr);
}

bool mxIsCell(const mxArray *ptr)
{
  return IsCells(ptr);
}


mxArray *mxGetCell(const mxArray *ptr, int index)
{
  NspCells *C = (NspCells *) ptr;
  if ( ! IsCells(ptr) ) nsp_mex_errjump();
  if ( ! (index >= 0 && index < C->mn )) nsp_mex_errjump();
  return C->objs[index];
}

void mxSetCell(mxArray *ptr, int index, mxArray *value)
{
  NspCells *C = (NspCells *) ptr;
  if ( ! IsCells(ptr) ) nsp_mex_errjump();
  if ( nsp_cells_set_element(C, index,value) == FAIL) 
    {
      nsp_mex_errjump();
    }
}


mxArray *mxCreateCellMatrix(int m, int n)
{
  NspCells *A;
  if ((A = nsp_cells_create(NVOID,m,n) ) == NULLCELLS) nsp_mex_errjump();
  return NSP_OBJECT(A);
}

mxArray *mxCreateCellArray(int ndim, const int *dims)
{
  NspCells *A;
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

int *mxGetDimensions(const mxArray *ptr)
{
  int  number_of_dimensions = mxGetNumberOfDimensions( ptr );
  int *dims = mxCalloc(number_of_dimensions, sizeof(int));
  if ( dims == NULL) return NULL;
  dims[0] = mxGetM( ptr );
  dims[1] = mxGetN( ptr );
  return dims;
}

mxClassID mxGetClassID(const mxArray *ptr) 
{
  if ( ptr == NULL )  nsp_mex_errjump();
  /* just return the base type that's enough for mexlib */
  return ((NspTypeBase *) ptr->basetype)->id;
}

void *mxMalloc(size_t n)
{
  return malloc(sizeof(char)*n);
}

void mxDestroyArray(mxArray *ptr)
{
  nsp_object_destroy(&ptr);
}

const char * mxGetName(const mxArray *ptr) 
{
  return nsp_object_get_name(ptr);
}

int mexPutVariable(const char *workspace, const char *var_name, mxArray *array_ptr)
{
  if (nsp_object_set_name(array_ptr,var_name) == FAIL) return FAIL;
  return  mexPutArray(array_ptr,workspace);
}

mxArray *mxCreateCharMatrixFromStrings(int m, const char **str)
{
  NspSMatrix *A;
  if ((A = nsp_smatrix_create_from_array(NVOID,m,str))== NULL) 
    nsp_mex_errjump();
  return NSP_OBJECT(A);
}

mxArray *mxDuplicateArray(const mxArray *in)
{
  return ( in != NULL ) ? nsp_object_copy(in) : NULL;
}

void mxSetName(mxArray *array_ptr,const char *var_name)
{
  nsp_object_set_name(array_ptr,var_name);
}

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


int mexEvalString(char *command)
{
  int display=FALSE,echo =FALSE,errcatch=TRUE,pausecatch=TRUE;
  if ( nsp_parse_eval_from_string(command,display,echo,errcatch,pausecatch) < 0) 
    return -1;
  else 
    return 0;
}

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
