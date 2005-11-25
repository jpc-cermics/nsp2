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
 *      une librairie degrad� non r�entrante 
 *      si on a besoin de charger un mex binaire 
 *      provenant de matlab 
 *      une librairie am�lior�e r�entrante pour les 
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
  for (k = 0; k < Max(lhs,1) ; ++k) plhs[k]=NSP_INT_TO_POINTER(-1);
} 

static void nsp_endmex(int lhs,int  *plhs[],int rhs,const int  *prhs[])
{
  int i;
  for ( i= 0 ; i < Max(lhs,1) ; i++) 
    {
      int j= NSP_POINTER_TO_INT(plhs[i]);
      if ( j != -1 ) NthObj(j)->ret_pos = i+1;
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
  if ( lhs <= 0 && NSP_POINTER_TO_INT(plhs[0]) != -1 ) lhs = 1;
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
  int i= NSP_POINTER_TO_INT( ptr);
  NspMatrix *A;
  if (( A=GetMtlbMat(stack,i)) == NULLMAT)   
    {
      nsp_mex_errjump();
    }
  return A->R;
}

/* Get imaginary part of matrix */

double *mxGetPi(const mxArray *ptr)
{  
  int i= NSP_POINTER_TO_INT(ptr);
  NspMatrix *A;
  if ((A=GetMtlbMat(stack,i)) == NULLMAT) 
    nsp_mex_errjump();
  if ( A->rc_type == 'r' )
    {
      Scierror("Error in %s: mxGetPi argument ",stack.fname);
      if ( ! Ocheckname(NthObj(i),NVOID) ) 
	Scierror("%s",nsp_object_get_name((NthObj(i))));
      Scierror("is a real matrix\n");
      nsp_mex_errjump();
    }
  return A->R+ A->mn;
}

/* Get m dimension of matrix **/

int mxGetM(const mxArray *ptr)
{
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( ptr);
  if (( Obj=nsp_get_object(stack,ih)) == NULL)
    {
      nsp_mex_errjump();
    }
  return nsp_object_get_size(Obj,1);
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
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( ptr);
  if (( Obj=nsp_get_object(stack,ih)) == NULL)
    {
      nsp_mex_errjump();
    }
  if ( IsSMat(Obj)) 
    {
      int n=0,i;
      if ( ((NspSMatrix*) Obj)->mn == 0 ) return 0;
      n = strlen(((NspSMatrix*) Obj)->S[0]);
      for ( i=1 ; i < ((NspSMatrix*) Obj)->mn ; i++)
	n =Max(n, strlen(((NspSMatrix*) Obj)->S[i]));
      return n;
    }
  return nsp_object_get_size(Obj,2);
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

int *mxCreateDoubleMatrix(int m, int n,  mxComplexity it)
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
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) A;
  return NSP_INT_TO_POINTER(rhs+newmat);
}

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
      A->convert = 'c'; /* matab complex style */
    }
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) A;
  return NSP_INT_TO_POINTER(rhs+newmat);
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

/* Get all the strings of Matrix SMatrix 
 * in one buffer 
 **/

int mxGetString(const mxArray *ptr, char *str, int strl)
{
  nsp_string message;
  int i= NSP_POINTER_TO_INT( ptr);
  NspSMatrix *A;
  if (( A=GetSMat(stack,i)) == NULLSMAT)   
    {
      nsp_mex_errjump();
    }
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

char *mxArrayToString(const mxArray *array_ptr)
{
  nsp_string message;
  int i= NSP_POINTER_TO_INT(array_ptr);
  NspSMatrix *A;
  if (( A=GetSMat(stack,i)) == NULLSMAT)   
    {
      nsp_mex_errjump();
    }
  message =nsp_smatrix_elts_concat(A,"",1,"",1);
  if ( message == NULL) 
    {
      nsp_mex_errjump();
    }
  return message;
}
      
/* libere le CreateFull **/

void mxFreeMatrix(int *ptr)
{
  return ;
}

/* libere le Calloc **/

void mxFree(void *ptr)
{
  return ;
}

/* exit function : mexAtExit : **/

int mexAtExit(void (*ExitFcn)(void))
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
  return NSP_INT_TO_POINTER(rhs+newmat);
}


/* here pas is supposed to be a Hash Table 
 * the index is not used i.e we only accept 
 * i==0;
 * fieldname ->  const char *fieldname
 */


mxArray *mxGetField (const mxArray *pa, int i, char *fieldname)
{
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( pa);
  NspHash *H;
  if ( i != 0 )
    {
      Scierror("Struct just have a zero index \n");
      nsp_mex_errjump();
    }
  if (( H=GetHash(stack,ih)) == NULLHASH)   
    {
      nsp_mex_errjump();
    }
  if ( nsp_hash_find(H,fieldname,&Obj) == FAIL) 
    {
      return NULL;
      /* 
	 Scierror("Error: cannot find field %s\n",fieldname);
	 nsp_mex_errjump();
      */
    }
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) Obj;
  return NSP_INT_TO_POINTER(rhs+newmat);
}

/*
 *
 */

extern mxArray *mxCreateStructMatrix(int m, int n, int nfields, const char **field_names)
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
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) H;
  return NSP_INT_TO_POINTER(rhs+newmat);
}


extern void mxSetField (mxArray *pa, int i, const char *fieldname, mxArray *value)
{
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( pa);
  int ivalue = NSP_POINTER_TO_INT(value);
  NspHash *H;
  if ( i != 0 )
    {
      Scierror("Struct just have a zero index \n");
      nsp_mex_errjump();
    }
  if (( H=GetHash(stack,ih)) == NULLHASH)   
    {
      nsp_mex_errjump();
    }
  if (( Obj=nsp_get_object(stack,ivalue)) == NULL)
    {
      nsp_mex_errjump();
    }
  if (  nsp_hash_enter_copy(H,Obj)==FAIL)
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
  int ih= NSP_POINTER_TO_INT( ptr);
  NspHash *H;
  if (( H=GetHash(stack,ih)) == NULLHASH)   
    {
      nsp_mex_errjump();
    }
  return H->filled;
}



bool mxIsChar(const mxArray *ptr)
{
  int ih= NSP_POINTER_TO_INT( ptr);
  if (! IsSMatObj(stack,ih) ) return FALSE;
  /* XXXX if (((NspSMatrix *)NthObj(ih))->n != 1) return FALSE; */
  return TRUE;
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
  return ISNAN(x);
}

int mxGetNumberOfElements(const mxArray *ptr)
{
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( ptr);
  if (( Obj=nsp_get_object(stack,ih)) == NULL)
    {
      nsp_mex_errjump();
    }
  if ( IsSMat(Obj) )
    {
      int n=0,i;
      if ( ((NspSMatrix*) Obj)->mn == 0 ) return 0;
      for ( i=0 ; i < ((NspSMatrix*) Obj)->mn ; i++)
	n += strlen(((NspSMatrix*) Obj)->S[i]);
      return n;
    }
  return nsp_object_get_size(Obj,0);
}


bool mxIsStruct(const mxArray *ptr)
{
  int ih= NSP_POINTER_TO_INT( ptr);
  return IsHashObj(stack,ih);
}


bool mxIsCell(const mxArray *ptr)
{
  int ih= NSP_POINTER_TO_INT( ptr);
  return IsCellsObj(stack,ih);
}



mxArray *mxGetCell(const mxArray *ptr, int index)
{
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( ptr);
  NspCells *C;
  if (( C=GetCells(stack,ih)) == NULL)   
    {
      nsp_mex_errjump();
    }
  if ( index >= 0 && index < C->mn )
    {
      Obj= C->objs[index];
    }
  else 
    {
      nsp_mex_errjump();
    }
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) Obj;
  return NSP_INT_TO_POINTER(rhs+newmat);
}

void mxSetCell(mxArray *array_ptr, int index, mxArray *value)
{
  NspObject *Obj;
  int ih= NSP_POINTER_TO_INT( array_ptr);
  NspCells *C;
  int iv= NSP_POINTER_TO_INT( value);
  if (( Obj=nsp_get_object(stack,iv)) == NULL)
    {
      nsp_mex_errjump();
    }
  if (( C=GetCells(stack,ih)) == NULL)   
    {
      nsp_mex_errjump();
    }
  if ( nsp_cells_set_element(C, index,Obj) == FAIL) 
    {
      nsp_mex_errjump();
    }
}


mxArray *mxCreateCellMatrix(int m, int n)
{
  NspCells *A;
  if ((A = nsp_cells_create(NVOID,m,n) ) == NULLCELLS) 
    nsp_mex_errjump();
  newmat++;
  NthObj(rhs+newmat)= (NspObject*) A;
  return NSP_INT_TO_POINTER(rhs+newmat);
}
