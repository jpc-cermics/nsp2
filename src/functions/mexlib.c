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
#include <stdio.h>
#include <setjmp.h>

#include "nsp/matrix-in.h"
#include "nsp/smatrix-in.h"

#define MEXLIB 
#include "nsp/mex.h"

/****************************************************
 * C function for C mexfunctions 
 ****************************************************/

/*  A set of constant **/

static int rhs ;
static int newmat=0;
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
 *      besoin de Scilab 
 *   je sais pas si la premiere version est vraiment utile !!
 */

static void nsp_initmex(char *name,int *lfirst,int lhs,int  *plhs[], int rhs,const int  *prhs[])
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
  newmat=0;
  stack = SciStack;
  stack.first = *lfirst;
  stack.fname = name;
  for (k = 0; k < rhs ; ++k) prhs[k]= NSP_INT_TO_POINTER(k+1);
} 

static void nsp_endmex(int lhs,int  *plhs[],int rhs,const int  *prhs[])
{
  int i;
  for ( i= 0 ; i < lhs ; i++) 
    {
      int j= NSP_POINTER_TO_INT(plhs[i]);
      NthObj(j)->ret_pos = i+1;
    }
  onlyone =0;
}

/*  used when performing a longjmp **/

static void nsp_clearmex(void)
{
  onlyone =0;
}

int nsp_mex_wrapper(Stack stack, int rhs, int opt, int lhs,mexfun *mexFunction)
{
  int *plhs[INTERSIZ];
  const int *prhs[INTERSIZ];
  int rfl;
  if (( rfl = sigsetjmp(MexEnv,1)) != 0 )
    {
      return RET_BUG;
    }
  nsp_initmex(stack.fname,&stack.first,lhs, plhs, rhs, prhs);
  mexFunction(lhs, plhs, rhs, prhs);
  lhs=Max(lhs,0);
  nsp_endmex(lhs, plhs, rhs, prhs);
  return lhs;
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
  int i= NSP_POINTER_TO_INT( ptr);
  NspMatrix *A;
  if ((A=GetRealMat(stack,i)) == NULLMAT)   
    {
      nsp_mex_errjump();
    }
  return A->R;
}

/* Get imaginary part of matrix */
/* we need to make a copy since Matlab complex matrix are 
 *  not implemented as complex arrays 
 *  XXXX : pas bon a revoir 
 **/

double *mxGetPi(const mxArray *ptr)
{  
  int i= NSP_POINTER_TO_INT(ptr);
  NspMatrix *A;
  if ((A=GetRealMat(stack,i)) == NULLMAT) 
    nsp_mex_errjump();
  if ( A->rc_type == 'r' )
    {
      Scierror("Error in %s: mxGetPi argument ",stack.fname);
      if ( ! Ocheckname(NthObj(i),NVOID) ) 
	Scierror("%s",nsp_object_get_name((NthObj(i))));
      Scierror("is a real matrix\n");
      nsp_mex_errjump();
    }
  else
    {
      if ( (A=GetMatCopy(stack,i))== NULLMAT) nsp_mex_errjump();
      if (nsp_mat_get_imag(A) !=  OK ) nsp_mex_errjump(); ;
    }
  return A->R;
}

/* Get m dimension of matrix **/

int mxGetM(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT( ptr);
  if ( check_cast(NthObj(i),nsp_type_matrix_id) ) 
    return ((NspMatrix *) NthObj(i))->m; 
  else if ( check_cast(NthObj(i),nsp_type_smatrix_id) ) 
    return i;
  else
    {
      Scierror("Error in %s: mxGetM size not implement for type xxx \n",stack.fname);
      if (! Ocheckname(NthObj(i),NVOID)) Scierror("%s",nsp_object_get_name((NthObj(i))));
    }
  return 0;
}

/* int array de taille n+1 tel que 
    Jc(j) = indice ds Ir et Pr du premier elets non nul de la j ieme colonne 
    Jc(n) est le monbre d'elments non nuls de la matrice 
**/


int *mxGetJc(const mxArray *ptr)
{
  return NULL;
}

/* Get Ir tableaux d'entier de meme taille que # des elts non nuls 
    contient indice de ligne de chaque element */

int *mxGetIr(const mxArray *ptr)
{
  return NULL;
}

/* Get n dimension of matrix **/

int mxGetN(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT(ptr);
  char *Str;
  if ( check_cast(NthObj(i),nsp_type_matrix_id) ) 
    return ((NspMatrix *) NthObj(i))->n;
  else if ( check_cast(NthObj(i),nsp_type_smatrix_id) ) 
    {
      Str =  ((NspSMatrix*) NthObj(i))->S[0];
      return strlen(Str);
    }
  else 
    {
      Scierror("Error in %s: mxGetM size not implement for type xxx \n",stack.fname);
      if ( ! Ocheckname(NthObj(i),NVOID) ) Scierror("%s",nsp_object_get_name((NthObj(i))));
    }
  return 0;
}

/* Check that object is a String **/

int mxIsString(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT( ptr);
  if (IsSMatObj(stack,i)  && 
      ((NspSMatrix *)NthObj(i))->mn == 1)
    return 1;
  else
    return 0;
} 

/*  Numeric : i.e sparse or matrix **/

int mxIsNumeric(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT( ptr);
  if ( IsMatObj(stack,i)  || 
       IsSpMatObj(stack,i)  )
    return 1;
  else
    return 0;
}

/*  Full : NspMatrix **/

int mxIsFull(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT( ptr);
  if ( IsMatObj(stack,i) ) 
    return 1;
  else
    return 0;
}

/*  Full : NspMatrix **/

int mxIsSparse(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT( ptr);
  if ( IsSpMatObj(stack,i)  )
    return 1;
  else
    return 0;
}

/*  Complex : NspMatrix or Sparse  **/


int mxIsComplex(const mxArray *ptr)
{
  int i= NSP_POINTER_TO_INT(ptr);
  if ( IsMatObj(stack,i) )
    {
      if (((NspMatrix *) NthObj(i))->rc_type == 'c' ) 
	return 1;
      else 
	return 0;
    }
  else if ( IsSpMatObj(stack,i)  )
    {
      if (((NspSpMatrix *) NthObj(i))->rc_type == 'c' ) 
	return 1;
      else 
	return 0;
    }
  else
    return 0;
}

/*  a Scalar **/

double mxGetScalar(const mxArray *ptr)
{ 
  int i= NSP_POINTER_TO_INT( ptr);
  double dval;
  if (GetScalarDouble(stack,i,&dval) == FAIL) 
    nsp_mex_errjump();
  return dval;
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

int *mxCreateFull(int m, int n, int it)
{
  NspMatrix *A;
  if ( it == 0) 
    {
      if ((A = nsp_matrix_create(NVOID,'r',m,n) ) == NULLMAT) nsp_mex_errjump();
    }
  else
    {
      if ((A = nsp_matrix_create(NVOID,'c',m,n) ) == NULLMAT) nsp_mex_errjump();
    }
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) A;
  return (int*) (rhs+newmat);
}

/* Allocation on the stack **/

void *mxCalloc(unsigned int n, unsigned int size)
{
  int *ptr,m;
  m = (n * size) /sizeof(double) + 1;
  ptr = mxCreateFull(m,1,0);
  return (void *) mxGetPr(ptr);
}

/**************************************************************
 * Return in str at most strl characters from first element of 
 * string NspMatrix pointed by ptr ( ptr is assumed to be a String NspMatrix )
 **************************************************************/

/* Get an SMatrix 1x1 **/

int mxGetString(const mxArray *ptr, char *str, int strl)
{
  int i= NSP_POINTER_TO_INT( ptr);
  char *Str;
  if ((Str = GetString(stack,i)) == (char*)0) nsp_mex_errjump();
  strncpy(str,Str,strl);
  str[strl]='\0';
  return 0;
}
      
/* libere le CreateFull **/

void mxFreeMatrix(int *ptr)
{
  return ;
}

/* libere le Calloc **/

void mxFree(int *ptr)
{
  return ;
}

/* exit function : mexAtExit : **/

int mexAtExit(int *ptr)
{
  return 0;
}

void mxCreateSparse(int *ptr)
{
  return ;
}

/**************************************************************
 * Create on Scilab Stack a 1x1 string matrix filled with string
 **************************************************************/

int *mxCreateString(char *string)
{
  NspSMatrix *S;
  if ((S= nsp_smatrix_create(NVOID,1,1,string,(integer)1)) == NULLSMAT ) nsp_mex_errjump();
  newmat++;
  NthObj(rhs+newmat)= (NspObject *) S;
  return (int*) (rhs+newmat);
}




