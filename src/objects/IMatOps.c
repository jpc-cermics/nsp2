/* Nsp
 * Copyright (C) 2009-2019 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2009-2019 Bruno Pincon Esial/Iecn
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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <nsp/object.h> 
#include <nsp/matrix.h> 
#include <nsp/bmatrix.h> 
#include <nsp/imatrix.h> 
/* #include <nsp/matint.h>  */

#include "nsp/cnumeric.h"
#include "nsp/matutil.h"
#include "nsp/gsort-p.h"
#include "nsp/nsp_lapack.h"
#include "nsp/lapack-c.h"
#include <nsp/blas.h>
#include <nsp/matutil.h>

static int nsp_iarray_maxi(int n,NspIMatrix *A, int off1,NspIMatrix *amax,int off2,int incr);
static int nsp_iarray_mini(int n,NspIMatrix *A, int off1,NspIMatrix *amin,int off2,int incr);

static void IKronecker (NspIMatrix *A,NspIMatrix *B,NspIMatrix *PK);
typedef int (*AdSu) (const int,const void *,void *);
typedef int (*AdSuZ) (int*,doubleC *,int*,doubleC *,int*);

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/*
 * A = A op B  
 *   rules : 
 *     A=[] , flag ==1 then A unchanged else A=B and F2(A)
 *     B=[] , flag ==1 then A=[] else A unchanged 
 *     B scalar, F1(A,B) 
 *     A scalar, A=A(1,1)*onesB) et F3(A,B) 
 */

typedef int (*MPM) (NspIMatrix *,NspIMatrix*);
typedef int (*PM) (NspIMatrix*);

static int MatNoOp(NspIMatrix *A)
{
  return OK;
}

static int MatOp(NspIMatrix *A, NspIMatrix *B, MPM F1, PM F2, MPM F3, int flag)
{

  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  if ( A->mn == 0) 
    {
      /* A = [] */ 
      if ( flag == 1) return OK ; 
      else
	{
	  int i;
	  /* flag != 1 ==> [] op A  returns F4(A) **/
	  /* resize A to B */ 
	  if ( nsp_imatrix_resize(A, B->m, B->n) == FAIL) return FAIL;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < A->mn ; i++) A->name[i] = B->name[i];break;
	  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
	  return (*F2)(A); 
	}
    }
  if ( B->mn == 0) 
    {
      /* B = [] */ 
      if ( flag == 1) 
	/* flag == 1 ==> A op [] returns [] **/
	return  nsp_imatrix_resize(A,0,0); 
      else
	return OK; 
    }
  if ( B->mn == 1) 
    {
      /* B is scalar */ 
      return (*F1)(A,B);
    }
  else if ( A->mn == 1 ) 
    {
      if ( nsp_imatrix_scalar_to_mn(A,B->m,B->n) == FAIL) return FAIL; 
    }
  /* general case */
  return (*F3)(A,B); 
}



/**
 * nsp_imatrix_mult:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * @flag: an int
 * 
 * computes A*B (flag=0) or A' * B  (flag=1) or  A * B' (flag=2)
 *  or A'*B' (flag=3).
 * @A and @B are not modified by this function.
 *
 * Returns: a #NspIMatrix or %NULLIMAT.
 */

NspIMatrix *nsp_imatrix_mult(NspIMatrix *A, NspIMatrix *B, int flag)
{ 
  NspIMatrix *Loc;
  int m, n, left, right,i,j,k;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return NULL;
    }
  switch ( flag ) 
    {  
    default: 
      Sciprintf("Invalid flag '%d' assuming 0\n", flag);
    case 0: 
      m = A->m; left = A->n; right = B->m; n = B->n; 
      break;
    case 1: 
      m = A->n; left = A->m; right = B->m; n = B->n; 
      break;
    case 2: 
      m = A->m; left = A->n; right = B->n; n = B->m; 
      break;
    case 3: 
      m = A->n; left = A->m; right = B->n; n = B->m;
      break;
    }
		 
  if ( left != right ) 
    {
      Scierror("Error:\tIncompatible dimensions\n");
      return NULLIMAT;
    }

  if ( left == 0 )
    {
      if ( (Loc = nsp_imatrix_create(NVOID,m,n,A->itype)) == NULLIMAT ) goto err;
      return Loc;
    }

  if ( (Loc =nsp_imatrix_create(NVOID,m,n,A->itype)) == NULLIMAT ) goto err;


  switch ( flag ) 
    {  
    default: 
      Sciprintf("Invalid flag '%d' assuming 0\n", flag);
    case 0: 
      /* A*B */
#define IMAT_AC(name,type,arg)						\
      for ( i=0 ; i < Loc->m ; i++)					\
	for ( j=0 ; j < Loc->n ; j++)					\
	  {								\
	    Loc->name[i+(Loc->m)*j]=0;					\
	    for ( k=0 ; k < left ; k++)					\
	      Loc->name[i+(Loc->m)*j] += A->name[i+k*A->m]*B->name[k+B->m*j]; \
	  }								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 1: 
      /* A'*B */
#define IMAT_AC(name,type,arg)						\
      for ( i=0 ; i < Loc->m ; i++)					\
	for ( j=0 ; j < Loc->n ; j++)					\
	  {								\
	    Loc->name[i+(Loc->m)*j]=0;					\
	    for ( k=0 ; k < left ; k++)					\
	      Loc->name[i+(Loc->m)*j] += A->name[k+i*A->m]*B->name[k+B->m*j]; \
	  }								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC

      break;
    case 2: 
      /*  A * B' */
#define IMAT_AC(name,type,arg)						\
      for ( i=0 ; i < Loc->m ; i++)					\
	for ( j=0 ; j < Loc->n ; j++)					\
	  {								\
	    Loc->name[i+(Loc->m)*j]=0;					\
	    for ( k=0 ; k < left ; k++)					\
	      Loc->name[i+(Loc->m)*j] += A->name[i+k*A->m]*B->name[j+B->m*k]; \
	  }								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC

      break;
    case 3: 
      /* A' *B ' */
#define IMAT_AC(name,type,arg)						\
      for ( i=0 ; i < Loc->m ; i++)					\
	for ( j=0 ; j < Loc->n ; j++)					\
	  {								\
	    Loc->name[i+(Loc->m)*j]=0;					\
	    for ( k=0 ; k < left ; k++)					\
	      Loc->name[i+(Loc->m)*j] += A->name[k+i*A->m]*B->name[j+B->m*k]; \
	  }								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC

      break;
    }

  return Loc;

 err:
  return NULLIMAT;
}

/*
 * term to term addition : the general case 
 * A = A+B (covers the scalar and [] cases ) 
 */

/**
 * nsp_imatrix_add:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * computes in @A the sum @A + @B (i.e @A= @A+@B) taking care 
 * of the limit cases, when on of the matrix is sero sized or scalar.
 * 
 * Return value:   %OK or %FAIL.
 **/

int nsp_imatrix_add(NspIMatrix *A, NspIMatrix *B) 
{
  return MatOp(A,B,nsp_imatrix_add_scalar,MatNoOp,nsp_imatrix_dadd,0);
}

/**
 * nsp_imatrix_dadd:
 * @Mat1: a #NspIMatrix 
 * @Mat2: a #NspIMatrix 
 * 
 * computes in @Mat1 the sum @Mat1 + @Mat2 (i.e @Mat1= @Mat1+@Mat2) when 
 * @Mat1 and @Mat2 have the same sizes.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_dadd(NspIMatrix *Mat1, NspIMatrix *Mat2)
{
  int i;
  if ( Mat1->itype != Mat2->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }

  if ( ! SameDim(Mat1,Mat2) )
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
#define IMAT_AC(name,type,arg) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] += Mat2->name[i];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}

/**
 * nsp_imatrix_add_mat:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix  
 * 
 * computes in @Mat1 the sum @Mat1 + @Mat2 (i.e @Mat1= @Mat1+@Mat2) when 
 * @Mat1 and @Mat2 have the same sizes.
 * 
 * Return value: %OK or %FAIL
 **/

/* supposed to do the same that previous function 
 *
 */

int nsp_imatrix_add_mat(NspIMatrix *A, NspIMatrix *B)
{
  return nsp_imatrix_dadd(A,B);
}

/**
 * nsp_imatrix_add_scalar:
 * @Mat1: a #NspIMatrix 
 * @Mat2: a #NspIMatrix of size 1x1 
 * 
 * computes in @A the sum @A + @B (i.e @A= @A+@B) when @B 
 * is a scalar matrix. 
 * 
 * Return value:  %OK or %FAIL.
 **/

int nsp_imatrix_add_scalar(NspIMatrix *Mat1, NspIMatrix *Mat2)
{
  int i;
  if ( Mat1->mn == 0 )  return OK;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] += Mat2->name[0];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}

/**
 * nsp_imatrix_add_scalar_bis:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix of size 1x1 (that is a scalar) 
 * 
 * Do the operation A <- A + B  when B is a scalar
 * and when nsp is compiled in MTLB_MODE (matlab
 * 's behavior for empty matrix)
 *
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_add_scalar_bis(NspIMatrix *A, NspIMatrix *B) 
{
  int i;
  if ( A->mn == 0 )  return OK;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < A->mn ; i++) A->name[i] += B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}


/**
 * nsp_imatrix_sub:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * @A = @A-@B, covering the case of scalar or empty matrices.
 * 
 * Return value:  %OK or %FAIL.
 **/

int nsp_imatrix_sub(NspIMatrix *A, NspIMatrix *B) 
{
  return MatOp(A,B,nsp_imatrix_sub_scalar,nsp_imatrix_minus,nsp_imatrix_dsub,0);
}

/**
 * nsp_imatrix_dsub:
 * @Mat1: a #NspIMatrix 
 * @Mat2: a #NspIMatrix 
 * 
 * 
 * @Mat1= @Mat1-@Mat2 when the two matrices have the same size.
 * 
 * Return value:  %OK or %FAIL.
 **/

int nsp_imatrix_dsub(NspIMatrix *Mat1, NspIMatrix *Mat2)
{
  int i;
  if ( Mat1->itype != Mat2->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }

  if ( ! SameDim(Mat1,Mat2) )
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
#define IMAT_AC(name,type,arg) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] -= Mat2->name[i];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}

/**
 * nsp_imatrix_sub_scalar:
 * @Mat1: a #NspIMatrix 
 * @Mat2: a #NspIMatrix 
 * 
 * 
 * @Mat1= @Mat1-@Mat2 when @Mat2 is a 1x1 matrix 
 * 
 * Return value:   %OK or %FAIL.
 **/

int nsp_imatrix_sub_scalar(NspIMatrix *Mat1, NspIMatrix *Mat2)
{
  int i;
  if ( Mat1->mn == 0 )  return OK;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] -= Mat2->name[0];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}

/**
 * nsp_imatrix_sub_scalar_bis:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix of size 1x1 (that is a scalar) 
 * 
 * Do the operation A <- A - B  when B is a scalar
 * and when nsp is compiled in MTLB_MODE (matlab
 * 's behavior for empty matrix)
 *
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_sub_scalar_bis(NspIMatrix *A, NspIMatrix *B) 
{
  int i;
  if ( A->mn == 0 )  return OK;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < A->mn ; i++) A->name[i] -= B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}


/**
 * nsp_imatrix_subs_calarm:
 * @Mat1: a #NspIMatrix 
 * @Mat2: a #NspIMatrix 
 * 
 * @Mat1 = - @Mat1 + @Mat2 and @Mat2 is a 1x1 matrix.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_subs_calarm(NspIMatrix *Mat1, NspIMatrix *Mat2)
{
  int i;
  if ( Mat1->mn == 0 )  return OK;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] = -Mat1->name[i]+ Mat2->name[0];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}



/**
 * nsp_imatrix_maxitt1:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * @Ind: a #NspIMatrix or %NULL
 * @j: integer 
 * @flag: integer if flag==0 then Ind is unused.
 * 
 *  Utility function used when computing the max of a set of matrices. 
 *  Computes A(k,l) = Maxi(A(k,l),B(k,l)). 
 *  Ind(k,l) is set to j if B(k,l) realize the max and flag ==1 
 *  if flag == 0 Ind is unused and can be null. 
 *  @A and @B must have same size or be scalars. @A is enlarged 
 *  if necessary. If @Ind is used it must have 
 *  On entry A and Ind if Ind is used must have the same sizes.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_maxitt1(NspIMatrix *A, NspIMatrix *B, NspMatrix *Ind,int j,int flag)
{
  int i,indval=0;

  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  if (  SameDim(A,B) )
    {
#define IMAT_MAX(name,type,arg) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] <  B->name[i] )				\
	  {								\
	    A->name[i] = B->name[i] ;					\
	    if ( flag == 1 ) Ind->R[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
    }
  else if ( B->mn == 1) 
    {
#define IMAT_MAX(name,type,arg) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] <  B->name[0] )				\
	  {								\
	    A->name[i] = B->name[0] ;					\
	    if ( flag == 1 ) Ind->R[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
    }
  else if ( A->mn == 1) 
    {
      /* A is scalar and B is not: A must be enlarged 
       * and Ind also if flag == 1 
       */
      nsp_int_union xx; 
#define IMAT_MAX(name,type,arg) xx.name = A->name[0];	\
      if ( flag == 1) indval = (int) Ind->R[0]; break 
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
      if ( nsp_imatrix_resize(A,B->m,B->n) == FAIL) return FAIL;
      if ( flag == 1)
	{
	  if ( nsp_matrix_resize(Ind,B->m,B->n) == FAIL) return FAIL;
	  nsp_mat_set_rval(Ind,indval);
	}
#define IMAT_MAX(name,type,arg)			\
      for ( i = 0; i < A->mn ; i++ )		\
	if  (  xx.name < B->name[i] )		\
	  {					\
	A->name[i] = B->name[i] ;		\
	if ( flag == 1 ) Ind->R[i] = j;	\
	  }					\
	else					\
	  { A->name[i] = xx.name;}		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
  return(OK);
}


/**
 * nsp_imatrix_minitt1:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * @Ind: a #NspIMatrix or %NULL
 * @j: integer 
 * @flag: integer if flag==0 then Ind is unused.
 * 
 *  Utility function used when computing the min of a set of matrices. 
 *  Computes A(k,l) = Min(A(k,l),B(k,l)). 
 *  Ind(k,l) is set to j if B(k,l) realize the min and flag ==1 
 *  if flag == 0 Ind is unused and can be null. 
 *  @A and @B must have same size or be scalars. @A is enlarged 
 *  if necessary. If @Ind is used it must have 
 *  On entry A and Ind if Ind is used must have the same sizes.
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_minitt1(NspIMatrix *A, NspIMatrix *B, NspMatrix *Ind, int j, int flag)
{

  int i,indval=0;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  if (  SameDim(A,B) )
    {
#define IMAT_MAX(name,type,arg) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] >  B->name[i] )				\
	  {								\
	    A->name[i] = B->name[i] ;					\
	    if ( flag == 1 ) Ind->R[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
    }
  else if ( B->mn == 1) 
    {
#define IMAT_MAX(name,type,arg) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] >  B->name[0] )				\
	  {								\
	    A->name[i] = B->name[0] ;					\
	    if ( flag == 1 ) Ind->R[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
    }
  else if ( A->mn == 1) 
    {
      /* A is scalar and B is not: A must be enlarged 
       * and Ind also if flag == 1 
       */
      nsp_int_union xx; 
#define IMAT_MAX(name,type,arg) xx.name = A->name[0];	\
      if ( flag == 1) indval = (int) Ind->R[0]; break 
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
      if ( nsp_imatrix_resize(A,B->m,B->n) == FAIL) return FAIL;
      if ( flag == 1)
	{
	  if ( nsp_matrix_resize(Ind,B->m,B->n) == FAIL) return FAIL;
	  nsp_mat_set_rval(Ind,indval);
	}
#define IMAT_MAX(name,type,arg)				\
      for ( i = 0; i < A->mn ; i++ )		\
	if  (  xx.name > B->name[i] )		\
	  {					\
	A->name[i] = B->name[i] ;		\
	if ( flag == 1 ) Ind->R[i] = j;	\
	  }					\
	else					\
	  { A->name[i] = xx.name;}		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
  return(OK);
}



/**
 * nsp_imatrix_kron:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * returns in a new #NspIMatrix the Kroeneker product of 
 * @A and @B.
 * 
 * Return value: a new #NspIMatrix or %NULLIMAT.
 **/
NspIMatrix *nsp_imatrix_kron(NspIMatrix *A, NspIMatrix *B)
{
  NspIMatrix *Loc;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return NULLIMAT;
    }
  if ((Loc = nsp_imatrix_create(NVOID,B->m*A->m,B->n*A->n,A->itype)) == NULLIMAT) 
    return(NULLIMAT);
  if ( Loc->mn == 0 ) return(Loc);
  IKronecker(A,B,Loc);
  return(Loc);
}


/**
 * nsp_imatrix_sum:  computes various sums of @A
 * @A: a #NspIMatrix
 * @dim: an integer 
 * 
 * for dim=0 the sum of all elements of @A is computed, a scalar is returned
 * for dim=1 the sum over the row indices is computed, a row vector is returned. 
 * for dim=2 the sum over the column indices is computed, a column vector is returned. 
 * else dim=0 is forced.
 * 
 * Return value: a  #NspIMatrix (a scalar, row or comumn vector)
 **/


NspIMatrix *nsp_imatrix_sum(NspIMatrix *A, int dim)
{
  NspIMatrix *Sum;
  int j,i;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
      if ((Sum = nsp_imatrix_create(NVOID,1,1,A->itype)) == NULLIMAT) 
	return(NULLIMAT);
#define IMAT_AC(name,type,arg) for ( i=0 ; i < A->mn ; i++) Sum->name[0] +=A->name[i] ;break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 1:
      if ((Sum = nsp_imatrix_create(NVOID,1,A->n,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( j=0 ; j < A->n ; j++)				\
	{							\
	  Sum->name[j] =0;					\
	  for ( i=0 ; i < A->m ; i++)				\
	    Sum->name[j] += A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 2:
      if ((Sum = nsp_imatrix_create(NVOID,A->m,1,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( i=0 ; i < A->m ; i++)				\
	{							\
	  Sum->name[i] =0;					\
	  for ( j=0 ; j < A->n ; j++)				\
	    Sum->name[i] += A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    }
  return Sum;
}


/**
 * nsp_imatrix_prod:  computes various products of elements of @A
 * @A: a #NspIMatrix
 * @dim: an integer 
 * 
 *  for dim=0 the product of all elements is computed, a scalar is returned.
 *  for dim=1 the product over the row indices is computed, a row vector is returned. 
 *  for dim=2 the product over the column indices is computed, a column vector is returned. 
 *  else dim=0 is forced.
 * 
 * Return value: a  #NspIMatrix (a scalar, row or comumn vector)
 **/

NspIMatrix *nsp_imatrix_prod(NspIMatrix *A, int dim)
{
  NspIMatrix *Prod;
  int i,j;
  /* int inc=1; */
  
  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0:
      if ((Prod = nsp_imatrix_create(NVOID,1,1,A->itype)) == NULLIMAT) 
	return(NULLIMAT);
#define IMAT_AC(name,type,arg)						\
      Prod->name[0]=1;							\
      for ( i=0 ; i < A->mn ; i++) Prod->name[0] *=A->name[i] ;break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 1:
      if ((Prod = nsp_imatrix_create(NVOID,1,A->n,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( j=0 ; j < A->n ; j++)				\
	{							\
	  Prod->name[j] =1;					\
	  for ( i=0 ; i < A->m ; i++)				\
	    Prod->name[j] *= A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 2:
      if ((Prod = nsp_imatrix_create(NVOID,A->m,1,A->itype)) == NULLIMAT) 
	return NULLIMAT;
      /* inc = A->m;*/
#define IMAT_AC(name,type,arg)					\
      for ( i=0 ; i < A->m ; i++)				\
	{							\
	  Prod->name[i] =1;					\
	  for ( j=0 ; j < A->n ; j++)				\
	    Prod->name[i] *= A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    }
  return Prod;
}



/**
 * nsp_imatrix_cum_prod:  cumulative products of elements of @A
 * @A: a #NspIMatrix
 * @dim: and integer 
 * 
 * for dim=0 the cumulative product over all elements is computed (in column major order).
 * for dim=1 the cumulative product over the row indices is computed.
 * for dim=2 the cumulative product over the column indices is computed.
 * else dim=0 is forced.
 * 
 * Return value: a #NspIMatrix of same dim than @A
 **/


NspIMatrix *nsp_imatrix_cum_prod(NspIMatrix *A, int dim)
{
  nsp_int_union cuprod; 
  NspIMatrix *Prod;
  int i,j, k, kp;

  if ( A->mn == 0) return nsp_imatrix_create(NVOID,A->m,A->n,A->itype);

  if ((Prod = nsp_imatrix_create(NVOID,A->m,A->n,A->itype)) == NULLIMAT) 
    return NULLIMAT;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
#define IMAT_AC(name,type,arg)			\
      cuprod.name=1;				\
      for ( i=0 ; i < A->mn ; i++)		\
	Prod->name[i] = (cuprod.name *= A->name[i]);	\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
      break;
    case 1:
#define IMAT_AC(name,type,arg)						\
      for ( j= 0 ; j < A->n ; j++)					\
	{								\
	  cuprod.name=1;						\
	  for ( i=0 ; i < A->m ; i++)					\
	    Prod->name[i+(A->m)*j] = (cuprod.name *= A->name[i+(A->m)*j]); \
	}								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;

    case 2:
#define IMAT_AC(name,type,arg)					\
      memcpy(Prod->name, A->name, A->mn*sizeof(type));		\
      for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )		\
	Prod->name[k] *= Prod->name[kp];			\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    }
  return Prod;
}


/**
 * nsp_imatrix_cum_sum:  cumulative sums of elements of @A
 * @A: a #NspIMatrix
 * @dim: an integer 
 *
 * for dim=0 the cumulative sum over all elements is computed (in column major order).
 * for dim=1 the cumulative sum over the row indices is computed.
 * for dim=2 the cumulative sum over the column indices is computed.
 * else dim=0 is forced.
 * 
 * Return value: a #NspIMatrix of same dim than @A
 **/

NspIMatrix *nsp_imatrix_cum_sum(NspIMatrix *A, int dim)
{
  nsp_int_union cusum;
  NspIMatrix *Sum;
  int i,j, k, kp;
  
  if ( A->mn == 0) 
    return  nsp_imatrix_create(NVOID,A->m,A->n,A->itype);

  if ((Sum = nsp_imatrix_create(NVOID,A->m,A->n,A->itype)) == NULLIMAT) 
    return NULLIMAT;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
#define IMAT_AC(name,type,arg)				\
      cusum.name=0;					\
      for ( i=0 ; i < A->mn ; i++)			\
	Sum->name[i] = (cusum.name += A->name[i]);	\
      break;
    NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;

    case 1:
#define IMAT_AC(name,type,arg)						\
      for ( j= 0 ; j < A->n ; j++)					\
	{								\
	  cusum.name=0;							\
	  for ( i=0 ; i < A->m ; i++)					\
	    Sum->name[i+(A->m)*j] = (cusum.name += A->name[i+(A->m)*j]); \
	}								\
      break;
    NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;

    case 2:
#define IMAT_AC(name,type,arg)				\
      memcpy(Sum->name, A->name, A->mn*sizeof(type));	\
      for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )	\
	Sum->name[k] += Sum->name[kp];			\
      break;						
    NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");		
#undef IMAT_AC
    break;
    }
  return Sum;
}


/**
 * nsp_imatrix_diff:  diff of elements of @A
 * @A: a #NspIMatrix
 * @order: diff level
 * @dim: an integer 
 * 
 *  for dim=0 the diff over all elements is computed (in column major order).
 *  for dim=1 the diff over the row indices is computed.
 *  for dim=2 the diff sum over the column indices is computed.
 *  else dim=0 is forced.
 * 
 * Return value: a #NspIMatrix
 **/


NspIMatrix *nsp_imatrix_diff(NspIMatrix *A, int order, int dim)
{
  NspIMatrix *Diff;
  int i, j, k, l;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0: 
      if ( A->mn - order <= 0 )
	return nsp_imatrix_create(NVOID,0,0,A->itype);
      if ( (Diff = nsp_imatrix_copy(A)) == NULLIMAT ) 
	return NULLIMAT;
#define IMAT_DIFF(name,type,arg)						\
      for ( k = 1 ; k <= order ; k++ )				\
	for ( i = 0 ; i < A->mn - k ; i++ )			\
	  Diff->name[i] = Diff->name[i+1] - Diff->name[i];	\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIFF,"");
#undef IMAT_DIFF
      if ( A->m > 1 )
	nsp_imatrix_resize(Diff, A->mn-order, 1);
      else
	nsp_imatrix_resize(Diff, 1, A->mn-order);
      break;
    case 1:
      if ( A->m - order <= 0 )
	return nsp_imatrix_create(NVOID,0,A->n,A->itype);

      if ( (Diff = nsp_imatrix_copy(A)) == NULLIMAT ) 
	return NULLIMAT;
#define IMAT_DIFF(name,type,arg)						\
      for ( k = 0 ; k < order ; k++ )				\
	for ( j = 0 ; j < A->n ; j++ )				\
	  for ( i = j*(A->m-k) ; i < (j+1)*(A->m-k) - k ; i++ )	\
	    Diff->name[i-j] = Diff->name[i+1] - Diff->name[i];		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIFF,"");
#undef IMAT_DIFF
      nsp_imatrix_resize(Diff, A->m-order, A->n);
      break;
    case 2:
      if ( A->n - order <= 0 )
	return nsp_imatrix_create(NVOID,A->m,0,A->itype);
      if ( (Diff = nsp_imatrix_copy(A)) == NULLIMAT ) 
	return NULLIMAT;
#define IMAT_DIFF(name,type,arg)						\
      for ( k = 1 ; k <= order ; k++ )				\
	for ( i = 0 ; i < A->m ; i++ )				\
	  for ( j = 0, l = i ; j < A->n-k ; j++, l+=A->m )	\
	    Diff->name[l] = Diff->name[l+A->m] - Diff->name[l] ;		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIFF,"");
#undef IMAT_DIFF
      nsp_imatrix_resize(Diff, A->m, A->n-order);
      break;
    }

  return Diff;
}


typedef int (*MaMi) (int,NspIMatrix *,int ,NspIMatrix *,int ,int );

/*
 *  MatMaxiMini(A, dim_flag, Imax, lhs, func)
 *     A is unchanged 
 *     dim_flag = 0 min or max for the whole matrix (got a scalar)
 *     dim_flag = 1 min or max of each column (got a row vector)
 *     dim_flag = 2 min or max of each row (got a column vector)
 *     Imax is created if lhs == 2 
 */

static NspIMatrix *MatMaxiMini(NspIMatrix *A, int dim_flag, NspMatrix **Imax, int lhs, MaMi F)
{
  NspIMatrix *M;
  int j;
  int inc=1,imax=0;

  switch (dim_flag) 
    {
    default :
      Sciprintf("\nInvalid dim flag '%d' assuming dim=0\n", dim_flag);

    case 0: 
      if ((M = nsp_imatrix_create(NVOID,Min(A->m,1),Min(A->n,1),A->itype)) == NULLIMAT) 
	return(NULLIMAT);
      if (M->mn == 1) 
	imax = (*F)(A->mn,A,0,M,0,1);
      if ( lhs == 2 ) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',Min(A->m,1),Min(A->n,1))) == NULLMAT)
	    return NULLIMAT; 
	  if (M->mn == 1) (*Imax)->R[0] = imax;
	}
      break;

    case 1:
      if ((M = nsp_imatrix_create(NVOID,Min(A->m,1),A->n,A->itype)) == NULLIMAT) 
	return NULLIMAT;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',Min(A->m,1),A->n)) == NULLMAT) 
	    return NULLIMAT; 
	  if ( M->mn > 0 )
	    for ( j= 0 ; j < A->n ; j++) 
	      {
		(*Imax)->R[j]=(*F)(A->m,A,(A->m)*j,M,j,1); 
	      }
	}
      else
	if ( M->mn > 0 )
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      (*F)(A->m,A,(A->m)*j,M,j,1); 
	    }
      break ;

    case 2:
      if ((M = nsp_imatrix_create(NVOID,A->m,Min(A->n,1),A->itype)) == NULLIMAT) 
	return NULLIMAT;
      inc = A->m;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',A->m,Min(A->n,1))) == NULLMAT) 
	    return NULLIMAT; 
	  if ( M->mn > 0 )
	    for ( j= 0 ; j < A->m ; j++) 
	      (*Imax)->R[j] = (*F)(A->n,A,j,M,j,inc);
	}
      else
	if ( M->mn > 0 )
	  for ( j= 0 ; j < A->m ; j++) (*F)(A->n,A,j,M,j,inc);
      break;
    }
  return M;
}


int nsp_iarray_maxi(int n, NspIMatrix *A, int off1, NspIMatrix  *amax, int off2, int incr)
{
  nsp_int_union xx; 
  int imax=1,i, i1;
#define IMAT_MAX(name,type,arg) xx.name = A->name[off1];	\
  for ( i=2, i1=off1+incr ; i <= n ; i++, i1+=incr)		\
    if ( A->name[i1] > xx.name )				\
      {								\
	xx.name =A->name[i1] ;  				\
	imax = i;						\
      }								\
  amax->name[off2]=xx.name;					\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
  return imax;
}


/**
 * nsp_imatrix_maxi:
 * @A: 
 * @flag: 
 * @Imax: 
 * @lhs: 
 * 
 * 
 * 
 * Return value: 
 **/

NspIMatrix *nsp_imatrix_maxi(NspIMatrix *A, int dim_flag, NspMatrix **Imax, int lhs)
{
  return MatMaxiMini(A,dim_flag,Imax,lhs,nsp_iarray_maxi);
}


/*
 *nsp_imatrix_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */


int nsp_iarray_mini(int n, NspIMatrix *A, int off1, NspIMatrix *amin, int off2, int incr)
{
  nsp_int_union xx; 
  int imax=1,i, i1;
#define IMAT_MAX(name,type,arg) xx.name = A->name[off1];	\
  for ( i=2, i1=off1+incr ; i <= n ; i++, i1+=incr)	       	\
    if ( A->name[i1] < xx.name )		\
      {						\
	xx.name = A->name[i1];   		\
	imax = i;				\
      }						\
  amin->name[off2]=xx.name;			\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX
  return imax;
}


/**
 * nsp_imatrix_mini:
 * @A: 
 * @flag: 
 * @Imax: 
 * @lhs: 
 * 
 * 
 * 
 * Return value: 
 **/
NspIMatrix *nsp_imatrix_mini(NspIMatrix *A, int dim_flag, NspMatrix **Imax, int lhs)
{
  return MatMaxiMini(A,dim_flag,Imax,lhs,nsp_iarray_mini);
}


/*
 *  compute the min and max of a vector
 */
#define ALGO_BASIC   /* to choose among the basic algo and the 3n/2 algo  */ 
                     /* but basic is faster than 3n/2... */

#ifdef ALGO_BASIC

static void VIMiniMaxi(int n, NspIMatrix *A, int aof, int incr,NspIMatrix *Amin, int aminof,
		       NspIMatrix *Amax, int amaxof, int *Imin, int *Imax)
{
  int i, i1, imin, imax;
  imin = imax = 1;
#define IMAT_MAX(name,type,arg)						\
  {                                                                     \
    type vmin, vmax;                    				\
    vmin = vmax = A->name[aof];		                                \
    for ( i=2, i1=aof+incr ; i <= n ; i++, i1+= incr)			\
      {									\
	if ( A->name[i1] < vmin )                       		\
	  {								\
	    vmin = A->name[i1]; imin = i;				\
	  }								\
	else if ( A->name[i1] > vmax )					\
	  {								\
	    vmax = A->name[i1]; imax = i;				\
	  }								\
      }	                                                                \
    Amin->name[aminof] = vmin; Amax->name[amaxof] = vmax;		\
  }                                                                     \
  break;					
  NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX

  *Imax = imax; *Imin = imin;
}

#else

/* 
 *  compute the min and max of a vector with the 3n/2 algorithm
 */

static void VIMiniMaxi(int n, NspIMatrix *A, int aof, int incr,NspIMatrix *Amin, int aminof,
		       NspIMatrix *Amax, int amaxof, int *Imin, int *Imax)
{
  int i, i1, is, i1s, imin, imax;
  imin = imax = 1;
  is = (n % 2 == 0) ? 1 : 2;
  i1s = (n % 2 == 0) ? aof : aof+incr;
#define IMAT_MAX(name,type,arg)						\
  {                                                                     \
    type vmin, vmax, first, second;      				\
    vmin = vmax = A->name[aof];		                                \
    for ( i=is, i1=i1s ; i <= n ; i+=2, i1+= 2*incr)			\
      {									\
	if ( (first=A->name[i1]) < (second=A->name[i1+incr]) )		\
	  {								\
	    if ( first < vmin )						\
	      {								\
		vmin = first; imin = i;   				\
	      }								\
	    if ( second > vmax )					\
	      {								\
		vmax = second; imax = i+1;        			\
	      }								\
	  }								\
	else								\
	  {								\
	    if ( first > vmax )						\
	      {								\
		vmax = first; imax = i;   				\
	      }								\
	    if ( second < vmin )					\
	      {								\
		vmin = second; imin = i+1;        			\
	      }								\
	  }								\
      }	                                                                \
    Amin->name[aminof] = vmin; Amax->name[amaxof] = vmax;		\
  }                                                                     \
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_MAX,"");
#undef IMAT_MAX

  *Imax = imax; *Imin = imin;
}
#endif

/**
 * nsp_imatrix_minmax:
 * @A: a #NspIMatrix 
 * @dim: 
 * @Amin: a #NspIMatrix handler 
 * @Imin: a #NspIMatrix handler 
 * @Amax: a #NspIMatrix handler 
 * @Imax: a #NspIMatrix handler 
 * @lhs: an integer 
 * 
 * 
 * 
 * Returns: %OK or %FAIL
 **/
int nsp_imatrix_minmax(NspIMatrix *A, int dim, NspIMatrix **Amin, NspMatrix **Imin,
		   NspIMatrix **Amax, NspMatrix **Imax, int lhs)
{
  NspIMatrix *amin=NULLIMAT, *amax=NULLIMAT;
  NspMatrix *imin=NULLMAT, *imax=NULLMAT;
  int k, m, n, indmin, indmax;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
      m = Min(A->m,1); n = Min(1,A->n); break;
    case 1:
      m = Min(A->m,1); n = A->n; break;
    case 2:
      m = A->m; n = Min(1,A->n); break;
    }

  if ( (amin = nsp_imatrix_create(NVOID,m,n,A->itype)) == NULLIMAT
       || (amax = nsp_imatrix_create(NVOID,m,n,A->itype)) == NULLIMAT ) goto err;
  if ( lhs > 2 )
    {
      if ((imin = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT
	  || (imax = nsp_matrix_create(NVOID,'r',m,n)) == NULLMAT ) goto err;
    }
  
  if ( A->mn == 0 )  /* special case : quick return */
    { 
      *Amin = amin; *Amax = amax; *Imin = imin; *Imax = imax; 
      return OK; 
    }

  if ( dim == 0 )
    {
      VIMiniMaxi(A->mn, A,0, 1, amin,0, amax,0, &indmin, &indmax);
      if (lhs > 2) {imin->R[0] = (double) indmin; imax->R[0] = (double) indmax;}
    }
  else if ( dim == 1 )
    for ( k = 0 ; k < A->n ; k++ )
      {
	VIMiniMaxi(A->m, A,k*A->m, 1, amin, k, amax,k, &indmin, &indmax);
	if (lhs > 2) {imin->R[k] = (double) indmin; imax->R[k] = (double) indmax;}
      }
  else
    for ( k = 0 ; k < A->m ; k++ )
      {
	VIMiniMaxi(A->n, A,k, A->m, amin,k, amax,k, &indmin, &indmax);
	if (lhs > 2) {imin->R[k] = (double) indmin; imax->R[k] = (double) indmax;}
      }

  *Amin = amin; *Amax = amax; *Imin = imin; *Imax = imax; 
  return OK;

 err:
  nsp_imatrix_destroy(amin); nsp_imatrix_destroy(amax); 
  nsp_matrix_destroy(imin); nsp_matrix_destroy(imax);
  return FAIL;
}

/**
 * nsp_imatrix_createinit:
 * @name: 
 * @type: 
 * @m: 
 * @n: 
 * @func: 
 * 
 * 
 * creates a new #NspIMatrix using @func to initialize the entries.
 * R=func(i,j) or R=func(i,j,Imag) 
 * 
 * Return value: a new #NspIMatrix or %NULLIMAT.
 **/

NspIMatrix *nsp_imatrix_createinit(char *name,nsp_itype itype, int m, int n,int (*func) (int,int))
{
  NspIMatrix *Loc;
  int i1,i2;
  if (( Loc= nsp_imatrix_create(name,m,n,itype)) == NULLIMAT) return(NULLIMAT);
#define IMAT_CI(name,type,arg)				\
  for (i2 = 0 ; i2 < n ; i2++)			\
    for (i1 = 0 ; i1 < m ; i1++)		\
      {						\
	int mi2=m*i2;				\
	Loc->name[i1+mi2] = (*func)(i1,i2);	\
      }						\
  break; 
  NSP_ITYPE_SWITCH(Loc->itype,IMAT_CI,"");
#undef IMAT_CI
  return(Loc);
}


/**
 * nsp_imatrix_triu:
 * @A: a #NspIMatrix 
 * @k: an integer 
 * 
 * A = triu(A,k). 
 **/
void nsp_imatrix_triu(NspIMatrix *A, int k)
{
  int i,j;
  nsp_int_union_ptr Aj;
#define IMAT_TRIU(name,type,arg)							\
  for ( j = 0, Aj.Iv = A->name ; j < Min(A->m+k-1,A->n) ; j++, Aj.name += A->m ) \
    for ( i = Max(0,j+1-k) ; i < A->m ; i++)				\
      Aj.name[i] = 0;							\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_TRIU,"");
#undef IMAT_TRIU
}

/**
 * nsp_imatrix_tril:
 * @A: a #NspIMatrix 
 * @k:  an integer
 * 
 * A=Tril(A)
 **/
void nsp_imatrix_tril(NspIMatrix *A, int k)
{
  int i,j;
  nsp_int_union_ptr Aj;
  int j0 = Max(0,k+1);

#define IMAT_TRIL(name,type,arg)							\
  Aj.Iv = &A->name[j0*A->m];						\
  for ( j = j0; j < A->n ; j++, Aj.name += A->m )			\
    for ( i = 0 ; i < Min(A->m,j-k) ; i++)				\
      Aj.name[i] = 0;							\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_TRIL,"");
#undef IMAT_TRIL
}


/**
 * nsp_imatrix_eye:
 * @m: number of rows
 * @n: number of columns
 * @itype: a #nsp_itype
 * 
 * returns the identity matrix of size @m x @n (eyes(m,n))
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

NspIMatrix *nsp_imatrix_eye(int m, int n,nsp_itype itype)
{
  NspIMatrix *Loc;
  int i;
  if (( Loc= nsp_imatrix_create(NVOID,m,n,itype)) == NULLIMAT) return(NULLIMAT);
#define IMAT_EYE(name,type,arg)							\
  for ( i=0 ; i < Min(m,n) ; i++) Loc->name[i+m*i]= 1;			\
  break;
  NSP_ITYPE_SWITCH(Loc->itype,IMAT_EYE,"");
#undef IMAT_EYE
  return(Loc);
}


/**
 * nsp_imatrix_ones:
 * @m: number of rows
 * @n: number of columns
 * @itype: a #nsp_itype
 * 
 * returns a  @m x @n matrix filled with 1.0
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

NspIMatrix *nsp_imatrix_ones(int m, int n,nsp_itype itype)
{
  int i;
  NspIMatrix *Loc;
  if ((Loc= nsp_imatrix_create(NVOID,m,n,itype))  == NULLIMAT) return(NULLIMAT);
#define IMAT_ONES(name,type,arg)							\
  for ( i=0 ; i < Loc->mn ; i++) Loc->name[i]= 1;			\
  break;
  NSP_ITYPE_SWITCH(Loc->itype,IMAT_ONES,"");
#undef IMAT_ONES
  return Loc;
}

/**
 * nsp_imatrix_zeros:
 * @m: number of rows
 * @n: number of columns
 * @itype: a #nsp_itype
 * 
 * returns a  @m x @n matrix filled with 0.0
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

NspIMatrix *nsp_imatrix_zeros(int m, int n, nsp_itype itype)
{
  NspIMatrix *Loc;
  if ((Loc= nsp_imatrix_create(NVOID,m,n,itype))  == NULLIMAT) return(NULLIMAT);
  return(Loc);
}


/**
 * nsp_imatrix_pow_matscalar:
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix with only one element (that is a scalar !) 
 * 
 * This routine computes @A^@B where @B must be a scalar (this must
 * be verified by the calling routine (*)). The matrix @A is modified 
 * and hold the final result (if OK is returned).  @A must be
 * square (if not FAIL is returned). This routine works only if @B is a
 * positive integer. 
 * 
 * (*) The operation A^B is done with the generic interface int_mx_mopscal
 *     which branches to one of the 3 routines:
 *        1/ nsp_imatrix_pow_matscalar(A,B) if B is a scalar 
 *        2/ nsp_imatrix_pow_matmat(A,B), if neither A and B are scalar
 *        3/ nsp_imatrix_pow_scalarmat(B,A), if A is a scalar
 *
 * Return value: %OK or %FAIL
 **/

static void nsp_imatrix_mult1(NspIMatrix *A, NspIMatrix *B, NspIMatrix *C);
static void nsp_imatrix_copy_contents(NspIMatrix *A, NspIMatrix *B);

int nsp_imatrix_pow_matscalar(NspIMatrix *A, NspIMatrix *B) 
{
  guint32 p=0;
  int i, oddflag=0;
  int A_is_square = A->m==A->n;

  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }

  if ( ! A_is_square )
    {
      Scierror("Error:\t matrix must be square\n");
      return FAIL;
    }

  if ( A->m == 1 )
    {
      /* A is a scalar */
      return nsp_imatrix_pow_scalar(A,B);
    }

#define IMAT_I(name,type,arg) p= B->name[0]; break;
  NSP_ITYPE_SWITCH(B->itype,IMAT_I,"");
#undef IMAT_I

  if ( p == 0 )
    {
      /* return identity matrix  */
#define IMAT_I(name,type,arg)					\
      for ( i = 0 ; i < A->mn ; i++ ) A->name[i] = 0.0;		\
      for ( i = 0 ; i < A->mn ; i+=A->m+1 ) A->name[i] = 1.0;	\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_I,"");
#undef IMAT_I
      return OK;
    }

  if ( p < 0 )
    {
      Scierror("Error: exponent should be a positive integer\n");
      return FAIL;
    }

  if ( p == 1 ) return OK;
  
  /* now use the power algorithm */
  {
    NspIMatrix *temp, *oddmat;
    if ( (temp = nsp_imatrix_create(NVOID,A->m,A->n,A->itype)) == NULLIMAT ) 
      return FAIL;
    while ( p > 1 )
      {
	if ( p % 2 == 1 )
	  {
	    if ( ! oddflag )
	      {
		if ( (oddmat = nsp_imatrix_create(NVOID,A->m,A->n,A->itype)) == NULLIMAT ) 
		  {
		    nsp_imatrix_destroy(temp);return FAIL; 
		  }
		oddflag = 1;
		nsp_imatrix_copy_contents(A,oddmat);
	      }
	    else
	      {
		/* temp = A*oddmat */
		nsp_imatrix_mult1(A,oddmat,temp);
		/* copy temp dans oddmat */
		nsp_imatrix_copy_contents(temp,oddmat);
	      }
	  }
	/* temp = A*A */
	nsp_imatrix_mult1(A,A,temp);
	/* copy temp dans A */
	nsp_imatrix_copy_contents(temp,A);
	p = p/2;
      }
    if ( oddflag )
      {
	/* temp = A*oddmat */
	nsp_imatrix_mult1(A,oddmat,temp);
	/* copy temp dans A */
	nsp_imatrix_copy_contents(temp,A);
	nsp_imatrix_destroy(oddmat);
      }
    nsp_imatrix_destroy(temp);
  }
  return OK;
}

static void nsp_imatrix_copy_contents(NspIMatrix *A, NspIMatrix *B)
{
#define IMAT_CP(name,type,arg)  memcpy(B->name,A->name, A->mn*sizeof(type));break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_CP,"");
#undef IMAT_CP
}

/* C = A*B */

static void nsp_imatrix_mult1(NspIMatrix *A, NspIMatrix *B, NspIMatrix *C)
{ 
  int i,j,k;
  /* A*B */
#define IMAT_AC(name,type,arg)						\
  for ( i=0 ; i < C->m ; i++)						\
    for ( j=0 ; j < C->n ; j++)						\
      {									\
	C->name[i+(C->m)*j]=0;						\
	for ( k=0 ; k < A->n ; k++)					\
	  C->name[i+(C->m)*j] += A->name[i+k*A->m]*B->name[k+B->m*j];	\
      }									\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
}

/**
 * nsp_imatrix_pow_matmat:
 * @A: a #NspIMatrix which is not a scalar
 * @B: a #NspIMatrix which is not a scalar
 * 
 * The operation @A^@B is done with the generic interface int_mx_mopscal
 * which branches to one of the 3 routines:
 *        1/ nsp_imatrix_pow_matscalar(@A,@B) if @B is a scalar 
 *        2/ nsp_imatrix_pow_matmat(@A,@B), if neither @A and @B are scalar
 *        3/ nsp_imatrix_pow_scalarmat(@B,@A), if @A is a scalar
 *
 * Here this routine is made for the case 2 but as it is not a defined
 * operation it displays only an error message.  
 *
 * Return value: %FAIL
 **/

int nsp_imatrix_pow_matmat(NspIMatrix *A, NspIMatrix *B) 
{
  Scierror("Error: at least one operand of power operator must be a scalar\n");
  return FAIL;
}

/**
 * nsp_imatrix_pow_scalarmat:
 * @B: a #NspIMatrix which must be square 
 * @A: a #NspIMatrix which must be a scalar 
 *  
 * The operation @A^@B is done with the generic interface int_mx_mopscal
 * which branches to one of the 3 routines:
 *        1/ nsp_imatrix_pow_matscalar(@A,@B) if @B is a scalar 
 *        2/ nsp_imatrix_pow_matmat(@A,@B), if neither A and B are scalar
 *        3/ nsp_imatrix_pow_scalarmat(@B,@A), if @A is a scalar
 * Note that the result is returned in @B which is overwritten.
 *
 * algorithm: @A^@B = e^(ln(@A)*@B), so we use expm( ln(@A)*@B )
 * 
 * Return value: %OK or %FAIL 
 **/

int nsp_imatrix_pow_scalarmat(NspIMatrix *B, NspIMatrix *A) 
{
  Scierror("Error: scalar^A not implemented for int matrices\n");
  return FAIL;
}


/*
 *  A Set of term to term function on Matrices (complex or real)
 */

/**
 * nsp_imatrix_pow_tt:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * @A = @A .^ @B (covers the scalar and [] cases )
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_pow_tt(NspIMatrix *A, NspIMatrix *B) 
{
  return MatOp(A,B,nsp_imatrix_pow_scalar,MatNoOp,nsp_imatrix_pow_el,1);
}


/**
 * nsp_imatrix_pow_el:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * @A = @A .^ @B when @A and @B have same dimensions.
 * 
 * Return value: %OK or %FAIL
 **/

#define IMAT_POWS(name,type,arg)					\
  for ( i = 0 ; i < A->mn ; i++ )					\
    {									\
      nsp_int_union z,p,x;						\
      z.name = 1;							\
      p.name = B->name[arg];						\
      x.name = A->name[i];						\
      if (  B->name[arg] < 0 ) { goto err;}				\
      else								\
	while ( p.name > 1 )						\
	  {								\
	    if ( p.name % 2 == 1 )					\
	      z.name *= x.name;						\
	    x.name *= x.name;						\
	    p.name = p.name/2;						\
	  }								\
      A->name[i]= z.name*x.name;					\
    } break;

int nsp_imatrix_pow_el(NspIMatrix *A, NspIMatrix *B)
{
  int i;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  if (SameDim(A,B))
    {
      NSP_ITYPE_SWITCH(A->itype,IMAT_POWS,i);
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
 err: 
  Scierror("Error: exponent must be positive for int matrices\n");
  return FAIL;
}

/**
 * nsp_imatrix_pow_scalar:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * @A = @A .^ @B when @B is a 1x1 matrix.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_pow_scalar(NspIMatrix *A, NspIMatrix *B)
{
  int i;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  NSP_ITYPE_SWITCH(A->itype,IMAT_POWS,0);
  return OK;
 err: 
  Scierror("Error: exponent must be positive for int matrices\n");
  return FAIL;
}

#undef IMAT_POWS

/**
 * nsp_imatrix_pow_scalarm:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * @A = @B .^ @A when @B is a 1x1 matrix.
 * 
 * Return value: %OK or %FAIL.
 **/

#define IMAT_POWS(name,type,arg)					\
  for ( i = 0 ; i < A->mn ; i++ )					\
    {									\
      nsp_int_union z,p,x;						\
      z.name = 1;							\
      p.name = A->name[i];						\
      x.name = B->name[0];						\
      if (  A->name[i] < 0 ) { goto err;}				\
      else								\
	while ( p.name > 1 )						\
	  {								\
	    if ( p.name % 2 == 1 )					\
	      z.name *= x.name;						\
	    x.name *= x.name;						\
	    p.name = p.name/2;						\
	  }								\
      A->name[i]= z.name*x.name;					\
    } break;

int nsp_imatrix_pow_scalarm(NspIMatrix *A, NspIMatrix *B)
{
  int i;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  NSP_ITYPE_SWITCH(A->itype,IMAT_POWS,"");
  return OK;
 err: 
  Scierror("Error: exponent must be positive for int matrices\n");
  return FAIL;
}

#undef IMAT_POWS




/**
 * nsp_imatrix_div_tt:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * @A = @A ./ @B (covers the scalar and [] cases ) 
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_div_tt(NspIMatrix *A, NspIMatrix *B) 
{
  return MatOp(A,B,nsp_imatrix_div_scalar,MatNoOp,nsp_imatrix_div_el,1);
}

/**
 * nsp_imatrix_div_el:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * @A = @A ./ @B, when matrices have the same sizes.
 * 
 * Return value: %OK or %FAIL.
 **/
/* deals with the case dim A == dim B **/

int nsp_imatrix_div_el(NspIMatrix *A, NspIMatrix *B)
{
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }

  if (SameDim(A,B))
    {
      int i;
#define IMAT_DIVEL(name,type,arg)					\
      for ( i = 0 ; i < A->mn ; i++ )					\
	{								\
	  if ( B->name[i]== (type) 0) goto err;				\
	  A->name[i] /= B->name[i];					\
	}								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIVEL,"");
#undef IMAT_DIVEL
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
 err:
  Scierror("Error:\tDivision by zero in integer matrices\n");
  return(FAIL);
}

/**
 * nsp_imatrix_div_scalar:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * @A = @A ./ @B when @B is a 1x1 matrix.
 * 
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_div_scalar(NspIMatrix *A, NspIMatrix *B)
{
  int i;
#define IMAT_DIVEL(name,type,arg)				\
  if ( B->name[0]== (type) 0)					\
    {								\
      Scierror("Error:\tDivision by zero in integer matrices\n");	\
      return(FAIL);							\
    }									\
  for ( i = 0 ; i < A->mn ; i++ ) A->name[i] /= B->name[0];		\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_DIVEL,"");
#undef IMAT_DIVEL
  return OK;
}

/**
 * nsp_imatrix_bdiv_tt:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * @A = @A .\ @B (covers the scalar and [] cases ) 
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_bdiv_tt(NspIMatrix *A, NspIMatrix *B) 
{
  return MatOp(A,B,nsp_imatrix_bdiv_scalar,MatNoOp,nsp_imatrix_bdiv_el,1);
}

/**
 * nsp_imatrix_bdiv_el:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 *
 * @A = @A .\ @B, when the matrices have the same dimensions.
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_bdiv_el(NspIMatrix *A, NspIMatrix *B)
{
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  if (SameDim(A,B))
    {
      int i;
#define IMAT_BDIVEL(name,type,arg)					\
      for ( i = 0 ; i < A->mn ; i++ )					\
	{								\
	  if ( A->name[i]== (type) 0) goto err;				\
	  A->name[i] = B->name[i]/A->name[i] ;				\
	}								\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_BDIVEL,"");
#undef IMAT_BDIVEL
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
 err:
  Scierror("Error:\tArguments must have the same size\n");
  return(FAIL);
}

/**
 * nsp_imatrix_bdiv_scalar:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * @A = @A .\ @B, when @B is a 1x1 matrix.
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_bdiv_scalar(NspIMatrix *A, NspIMatrix *B)
{
  int i;
#define IMAT_DIVSC(name,type,arg)					\
  for ( i = 0 ; i < A->mn ; i++ ) A->name[i] = B->name[0]/A->name[i];	\
      break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_DIVSC,"");
#undef IMAT_DIVSC
  return(OK);
}


/**
 * nsp_imatrix_mult_tt:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 *
 * @A = @A .* @B (covers the scalar and [] cases ) 
 * 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_mult_tt(NspIMatrix *A, NspIMatrix *B) 
{
  return MatOp(A,B,nsp_imatrix_mult_scalar,MatNoOp,nsp_imatrix_mult_el,1);
}


/**
 * nsp_imatrix_mult_el:
 * @A: a #NspIMatrix 
 * @B: 
 * 
 *  @A = @A .* @B, when the matrices have the same dimensions.
 *  plus the special following cases which add the possibility 
 *  to compute efficiently with a diagonal matrix stored as a vector 
 *  (Bruno, sept 8 2005) by adding 2 new rules for the .* operator :
 *      1/ In place of diag(A)*B we do A.*B (if B is m x n, A must be m x 1)
 *      2/ In place of A*diag(B) we do A.*B (if A is m x n, B must be 1 x n)
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_mult_el(NspIMatrix *A, NspIMatrix *B)
{
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }

  if (SameDim(A,B))
    {
      int i;
#define IMAT_MULTEL(name,type,arg)						\
      for ( i = 0 ; i < A->mn ; i++ ) A->name[i] *= B->name[i];		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MULTEL,"");
#undef IMAT_MULTEL
      return OK;
    }
  else if ( A->m == B->m  &&  A->n == 1 )    /* diag(A)*B */
    {
      /* result must be in A so copy A first in 
       * coef then resize A to the sizes of B 
       */
      NspIMatrix *coef;
      int i,j, k=0;
      if ( (coef=nsp_imatrix_copy(A)) == NULLIMAT ) return FAIL;
      if ( nsp_imatrix_resize(A,B->m,B->n) == FAIL ) 
	{nsp_imatrix_destroy(coef); return FAIL;}
#define IMAT_MULTEL(name,type,arg)						\
      for ( j = 0 ; j < A->n ; j++)					\
	for ( i = 0 ; i < A->m ; i++, k++ )				\
	  A->name[k] = B->name[k]*coef->name[i];			\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MULTEL,"");
#undef IMAT_MULTEL
      nsp_imatrix_destroy(coef);
      return OK;
    }
  else if ( A->n == B->n  &&  B->m == 1 )    /* A*diag(B) */
    {
      int i,j, k=0;
#define IMAT_MULTEL(name,type,arg)						\
      for ( j = 0 ; j < A->n ; j++)					\
	for ( i = 0 ; i < A->m ; i++, k++ ) A->name[k] *= B->name[j];	\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MULTEL,"");
#undef IMAT_MULTEL
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
}

/**
 * nsp_imatrix_mult_scalar:
 * @Mat1: a #NspIMatrix
 * @Mat2: a #NspIMatrix
 * 
 * @Mat1 = @Mat1 .* @Mat2, when @Mat2 is a 1x1 matrix.
 * 
 * Return value: %OK or %FAIL.
 **/
 
int nsp_imatrix_mult_scalar(NspIMatrix *Mat1, NspIMatrix *Mat2)
{
  int i;
  if ( Mat1->itype != Mat2->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
#define IMAT_MULTEL(name,type,arg)						\
  for ( i = 0 ; i < Mat1->mn ; i++ ) Mat1->name[i] *= Mat2->name[0];	\
  break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_MULTEL,"");
#undef IMAT_MULTEL
  return OK;
}

/**
 * nsp_imatrix_mult_scalar_bis:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix of size 1x1 (that is a scalar) 
 * 
 * Do the operation A <- A*B  when B is a scalar
 * and when nsp is compiled in MTLB_MODE (matlab
 * 's behavior for empty matrix)
 *
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_mult_scalar_bis(NspIMatrix *A, NspIMatrix *B) 
{
  int i;
  if ( A->mn == 0 )    return OK;
#define IMAT_MULTEL(name,type,arg)					\
  for ( i = 0 ; i < A->mn ; i++ )					\
    A->name[i] *= B->name[0];						\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_MULTEL,"");
#undef IMAT_MULTEL
  return OK;
}



/**
 * nsp_imatrix_modulo:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * A= A mod B
 * @B can be sacalar or can have the same size as @A;
 * @B and @A should have the same int type.
 **/

void nsp_imatrix_modulo(NspIMatrix *A,NspIMatrix *B)
{
  int i ;
  if ( A->mn == B->mn) 
    {
#define IMAT_MOD(name,type,arg)						\
      for ( i=0 ; i < A->mn ; i++)					\
	A->name[i]= A->name[i] % B->name[i] ;				\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MOD,"");
#undef IMAT_MOD
    }
  else
    {
#define IMAT_MOD(name,type,arg)						\
      for ( i=0 ; i < A->mn ; i++)					\
	A->name[i]= A->name[i] % B->name[0] ;				\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MOD,"");
#undef IMAT_MOD
    }
}

/**
 * nsp_imatrix_mod:
 * @x: a #NspIMatrix 
 * @y: a #NspIMatrix 
 * 
 * computes  @x (or @y) <- @x - @y .* floor (@x ./ @y)
 *    @x and @y must be both real and of equal length or
 *    one can be a scalar
 *    generally @x is modified in place but when @x is scalar
 *    with @y->mn > 1,  @y must hold the result. 
 *    When @y[i] = 0 the result is @x[i]
 * 
 **/

void nsp_imatrix_mod(NspIMatrix *x, NspIMatrix *y)
{
  int i;
  if ( x->itype != y->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return ;
    }
#define IMAT_IDIV(name,type,arg)				\
  if ( x->mn == 1 && y->mn > 1 )				\
    for ( i = 0 ; i < y->mn ; i++ )				\
      y->name[i] = x->name[0] % y->name[i];			\
  else if ( y->mn == 1 )					\
    for ( i = 0 ; i < x->mn ; i++ )					\
      x->name[i] = x->name[i] % y->name[0];				\
  else								\
    for ( i = 0 ; i < x->mn ; i++ )				\
      x->name[i] = x->name[i] % y->name[i];				\
  break;
  NSP_ITYPE_SWITCH(x->itype,IMAT_IDIV,"");
#undef IMAT_IDIV

}

/**
 * nsp_imatrix_idiv:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix of size 1x1
 * 
 * 
 * A is changed to A / B(1) : 
 **/

void nsp_imatrix_idiv(NspIMatrix *A, NspIMatrix *B)
{
  int i ;
#define IMAT_IDIV(name,type,arg)					\
  for ( i=0 ; i < A->mn ; i++)						\
    A->name[i] /= B->name[0] ;						\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_IDIV,"");
#undef IMAT_IDIV
}


/**
 * nsp_imatrix_sign:
 * @A: a #NspIMatrix 
 * 
 * A=Sign(A)
 *
 * Return value: %OK.
 **/

int nsp_imatrix_sign(NspIMatrix *A)
{
  int i ;
#define IMAT_SIGN(name,type,arg)					\
  for ( i=0 ; i < A->mn ; i++)				\
    if ( A->name[i] > 0.0)				\
      A->name[i]=1;					\
    else if ( A->name[i] < 0.0 )			\
      A->name[i]=-1;					\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_SIGN,"");
#undef IMAT_SIGN
  return(OK);
}


/**
 * nsp_imatrix_abs:
 * @A: a #NspIMatrix 
 * 
 * A=Abs(A), absolue value or module of each element 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_abs(NspIMatrix *A)
{
  int i ;
#define IMAT_ABS(name,type,arg)					\
  for ( i=0 ; i < A->mn ; i++)				\
    if ( A->name[i] < 0.0)				\
      A->name[i]= - A->name[i];				\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_ABS,"");
#undef IMAT_ABS
  return OK;
}

/*
 * A= A & B logical int &  
 */

/**
 * nsp_imatrix_iand:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * A= A & B. Logical and of matrices entries (casted with aint()).
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_iand(NspIMatrix *A, NspIMatrix *B)
{
  int i ;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
#define IMAT_IAND(name,type,arg)				\
  for ( i = 0 ; i < A->mn ; i++)				\
    A->name[i]&= B->name[i];					\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_IAND,"");
#undef IMAT_IAND
  return(OK);
}

/**
 * nsp_imatrix_iandu:
 * @A: a #NspIMatrix 
 * @res: an integer pointer to store the result.
 * 
 * logical and of all the entries of A 
 * casted to int 
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_iandu(NspIMatrix *A, nsp_int_union *res)
{
  int i ;
#define IMAT_IAND(name,type,arg)					\
  (*res).name = A->name[0];						\
  for ( i =1  ; i < A->mn ; i++)					\
    (*res).name &=  A->name[i];						\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_IAND,"");
#undef IMAT_IAND
  return(OK);
}


/**
 * nsp_imatrix_ior:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * 
 * A= A | B. Logical  or of matrices entries (casted with aint()).
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_ior(NspIMatrix *A, NspIMatrix *B)
{
  int i ;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
#define IMAT_IOR(name,type,arg)					\
  for ( i = 0 ; i < A->mn ; i++)				\
    A->name[i] |= B->name[i];					\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_IOR,"");
#undef IMAT_IOR
  return(OK);
}




/**
 * nsp_imatrix_ishift:
 * @A: a #NspIMatrix 
 * @shift: number of bits to shift.
 * @dir: 'r' for shift right, else it is a shift left 
 * 
 * A= (A << i) or A= (A >> i).
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_ishift(NspIMatrix *A,int shift,char dir)
{
  int i ;
#define IMAT_ISHIFT(name,type,arg)					\
  if ( dir == 'r' )							\
    for ( i = 0 ; i < A->mn ; i++)					\
      A->name[i] >>= shift;						\
  else									\
    for ( i = 0 ; i < A->mn ; i++)					\
      A->name[i] <<= shift;						\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_ISHIFT,"");				
#undef IMAT_ISHIFT
  return(OK);
}

/**
 * nsp_imatrix_ior_unary
 * @A: a #NspIMatrix 
 * @dim: a direction 
 * 
 * logical or of the entries of @A along 
 * @dim dimension
 * 
 * Return value: a new #NspIMatrix 
 **/

NspIMatrix *nsp_imatrix_ior_unary(NspIMatrix *A, int dim)
{
  NspIMatrix *Ior;
  int j,i;
  if ( A->mn == 0)
    {
      if ( dim == 0 )
	{
	  Ior = nsp_imatrix_create(NVOID,1,1,A->itype);
	  return Ior;
	}
      else
	{
	  return  nsp_imatrix_create(NVOID,0,0,A->itype);
	}
    }
  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
      if ((Ior = nsp_imatrix_create(NVOID,1,1,A->itype)) == NULLIMAT) 
	return(NULLIMAT);
#define IMAT_AC(name,type,arg)						\
      Ior->name[0]= A->name[0];						\
      for ( i=1 ; i < A->mn ; i++) Ior->name[0] |= A->name[i] ;break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 1:
      if ((Ior = nsp_imatrix_create(NVOID,1,A->n,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( j=0 ; j < A->n ; j++)				\
	{							\
	  Ior->name[j] = 0;					\
	  for ( i=0 ; i < A->m ; i++)				\
	    Ior->name[j] |= A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 2:
      if ((Ior = nsp_imatrix_create(NVOID,A->m,1,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( i=0 ; i < A->m ; i++)				\
	{							\
	  Ior->name[i] =0;					\
	  for ( j=0 ; j < A->n ; j++)				\
	    Ior->name[i] |= A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    }
  return Ior;
}

/**
 * nsp_imatrix_ior_unary
 * @A: a #NspIMatrix 
 * @dim: a direction 
 * 
 * logical or of the entries of @A along 
 * @dim dimension
 * 
 * Return value: a new #NspIMatrix 
 **/

NspIMatrix *nsp_imatrix_iand_unary(NspIMatrix *A, int dim)
{
  NspIMatrix *Iand;
  int j,i;
  if ( A->mn == 0)
    {
      if ( dim == 0 )
	{
	  Iand = nsp_imatrix_create(NVOID,1,1,A->itype);
	  return Iand;
	}
      else
	{
	  return  nsp_imatrix_create(NVOID,0,0,A->itype);
	}
    }
  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
      if ((Iand = nsp_imatrix_create(NVOID,1,1,A->itype)) == NULLIMAT) 
	return(NULLIMAT);
#define IMAT_AC(name,type,arg)						\
      Iand->name[0]= A->name[0];						\
      for ( i=1 ; i < A->mn ; i++) Iand->name[0] &= A->name[i] ;break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 1:
      if ((Iand = nsp_imatrix_create(NVOID,1,A->n,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( j=0 ; j < A->n ; j++)				\
	{							\
	  Iand->name[j] =A->name[(A->m)*j] ;			\
	  for ( i=1 ; i < A->m ; i++)				\
	    Iand->name[j] &= A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    case 2:
      if ((Iand = nsp_imatrix_create(NVOID,A->m,1,A->itype)) == NULLIMAT) 
	return NULLIMAT;
#define IMAT_AC(name,type,arg)					\
      for ( i=0 ; i < A->m ; i++)				\
	{							\
	  Iand->name[i] =A->name[i];				\
	  for ( j=1 ; j < A->n ; j++)				\
	    Iand->name[i] &= A->name[i+(A->m)*j];		\
	}							\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      break;
    }
  return Iand;
}




/**
 * nsp_imatrix_minus:
 * @A: a #NspIMatrix 
 * 
 * A= -A 
 * 
 * Return value: %OK.
 **/

int nsp_imatrix_minus(NspIMatrix *A)
{
  int i ;
#define IMAT_MINUS(name,type,arg)						\
  for ( i = 0 ; i < A->mn ; i++) A->name[i]= - A->name[i];		\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_MINUS,"");
#undef IMAT_MINUS
  return(OK);
}

/**
 * IKronecker:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * @PK: a #NspIMatrix 
 * 
 * Kronecker product of two Matrices 
 * PK is the result it must be created 
 * before calling this function size (AmxBm,AnxBn)
 * The rule to compute PK is the following 
 * PK[ i + j*B->m + k*(B->m*A->m) + p*(B->m*A->m*B->n)] = a(j,p)*b(i,k)
 * 
 **/

static void IKronecker(NspIMatrix *A, NspIMatrix *B, NspIMatrix *PK)
{
  int iA, jA, jB, iB, k;
#define IMAT_IK(name,type,arg)						\
  {									\
    type *colA, *colB;							\
    for ( jA = 0, k = 0, colA = A->name ; jA < A->n ; jA++, colA += A->m )	\
      for ( jB = 0, colB = B->name ; jB < B->n ; jB++, colB += B->m )	\
	for ( iA = 0 ; iA < A->m ; iA++)				\
	  for ( iB = 0 ; iB < B->m ; iB++, k++ )			\
	    PK->name[k] = colA[iA] * colB[iB];				\
  } break;
  
  NSP_ITYPE_SWITCH(A->itype,IMAT_IK,"");
#undef IMAT_IK
}


/*
 * Comparison operators
 */

/* Operations **/

static int Lt(nsp_itype itype,nsp_int_union a,nsp_int_union b) 
{
#define IMAT_COP(name,type,arg) return a.name < b.name 
  NSP_ITYPE_SWITCH(itype,IMAT_COP,"");
#undef IMAT_COP
  return 0;
}

static int Le(nsp_itype itype,nsp_int_union a, nsp_int_union b) 
{
#define IMAT_COP(name,type,arg) return a.name <= b.name 
  NSP_ITYPE_SWITCH(itype,IMAT_COP,"");
#undef IMAT_COP
  return 0;
}

static int Eq(nsp_itype itype,nsp_int_union a, nsp_int_union b)
{
#define IMAT_COP(name,type,arg) return a.name == b.name 
  NSP_ITYPE_SWITCH(itype,IMAT_COP,"");
#undef IMAT_COP
  return 0;
}

static int NEq(nsp_itype itype,nsp_int_union a, nsp_int_union b)
{
#define IMAT_COP(name,type,arg) return a.name != b.name 
  NSP_ITYPE_SWITCH(itype,IMAT_COP,"");
#undef IMAT_COP
  return 0;
}

static int Gt(nsp_itype itype,nsp_int_union a, nsp_int_union b) 
{
#define IMAT_COP(name,type,arg) return a.name > b.name 
  NSP_ITYPE_SWITCH(itype,IMAT_COP,"");
#undef IMAT_COP
  return 0;
}

static int Ge(nsp_itype itype,nsp_int_union a, nsp_int_union b) 
{
#define IMAT_COP(name,type,arg) return a.name >= b.name 
  NSP_ITYPE_SWITCH(itype,IMAT_COP,"");
#undef IMAT_COP
  return 0;
}

typedef int (CompOp) (nsp_itype itype,nsp_int_union a, nsp_int_union b);

typedef struct cpt {
  char *name;
  CompOp *fonc,*foncop;
} CompTab;

/* Warning : sorted tab **/ 

static CompTab comptab[] = {
  {"<",Lt  , Ge},
  {"<=",Le  ,Gt},
  {"<>",NEq ,Eq},
  {"==",Eq  ,NEq},
  {">",Gt  , Le},
  {">=",Ge  , Lt},
  {(char *) NULL, 0,0}
};

static int SearchComp(const char *op, CompOp (**realop))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *realop = comptab[i].fonc;
	  return(OK);
	}
      else
	{ 
	  if ( j <= 0)
	    {
	      Sciprintf("\nUnknow comp operator <%s>\n",op);
	      return(FAIL);
	    }
	  else i++;
	}
    }
  Sciprintf("\n Unknow comp operator <%s>\n",op);
  return(FAIL);
}


/**
 * nsp_imatrix_comp:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * @op: comparison operator as a string
 * 
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j). A and B must be size compatible with 
 * the standard promotion of scalars i.e 1x1 matrices. 
 * A and B are unchanged : Res is created 
 * 
 * Return value: a new #NspBMatrix or %NULLBMAT
 **/

NspBMatrix  *nsp_imatrix_comp(const NspIMatrix *A,const NspIMatrix *B,const char *op)
{
  int i, iA, iB, inc_A, inc_B, m, n;
  NspBMatrix *Loc ;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return NULL;
    }

  if ( !( A->m == B->m  &&  A->n == B->n ) )
    {
      if ( B->mn == 1 ) 
	{
	  m = A->m; n = A->n; inc_A = 1; inc_B = 0;
	}
      else if ( A->mn == 1 ) 
	{
	  m = B->m; n = B->n; inc_A = 0; inc_B = 1;
	}
      else   
	{
	  /* Incompatible dimensions */
	  if ( (Loc =nsp_bmatrix_create(NVOID,1,1)) == NULLBMAT ) return NULLBMAT;
	  if ( strcmp(op,"<>") == 0 ) 
	    Loc->B[0] = TRUE;
	  else
	    Loc->B[0] = FALSE;
	  return Loc;
	}
    }
  else    
    {
      /* A and B are of same dimensions */ 
      if ( A->mn == 0) 
	{
	  if ( (Loc =nsp_bmatrix_create(NVOID,A->m,A->n)) == NULLBMAT ) return NULLBMAT;
	  return Loc;
	}
      else
	{
	  m = A->m; n = A->n; inc_A = 1; inc_B = 1;
	}
    }

  if ( (Loc =nsp_bmatrix_create(NVOID,m,n)) == NULLBMAT ) return NULLBMAT;

#define MAKE_COMP(name,type,op)						\
  for ( i = 0, iA = 0, iB = 0 ; i < m*n ; i++, iA+=inc_A, iB+=inc_B )	\
    Loc->B[i] = A->name[iA] op B->name[iB];				\
  break;

  if ( strcmp(op,"==") == 0 )
    { NSP_ITYPE_SWITCH(A->itype, MAKE_COMP,==); }
  else if ( strcmp(op,"<>") == 0 )
    { NSP_ITYPE_SWITCH(A->itype,MAKE_COMP, !=); }
  else if ( strcmp(op,"<=") == 0 )
    { NSP_ITYPE_SWITCH(A->itype,MAKE_COMP, <=); }
  else if ( strcmp(op,">=") == 0 )
    { NSP_ITYPE_SWITCH(A->itype, MAKE_COMP, >=); }
  else if ( strcmp(op,"<") == 0 )
    { NSP_ITYPE_SWITCH(A->itype,MAKE_COMP, <); }
  else if ( strcmp(op,">") == 0 )
    { NSP_ITYPE_SWITCH(A->itype,MAKE_COMP, >); }
  return Loc;
#undef MAKE_COMP
}


/* 
 * returns and(nsp_imatrix_comp(A,B,op))
 * err is set to TRUE if an  allocation error is raised or if 
 * op is not found 
 */ 


/**
 * nsp_imatrix_fullcomp:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * @op: 
 * @err: 
 * 
 * 
 * 
 * Return value: 
 **/

int nsp_imatrix_fullcomp(const NspIMatrix *A,const NspIMatrix *B,const char *op,int *err)
{
  nsp_int_union a, b;
  CompOp *realop;
  int i, Loc=TRUE ;
  *err=FALSE;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      *err=TRUE;
      return FALSE;
    }

  if ( SearchComp(op,&realop) == FAIL) { *err=TRUE; return FALSE;}
  if ( !( A->m == B->m && A->n == B->n ) )
    {
      if ( B->mn == 1 && A->mn != 0 ) 
	{
	  /* Special case B is a constant, Loc created  */
#define MAKE_COMP(name,type,arg)							\
	  for ( i = 0 ; i < A->mn ; i++ )				\
	    if ( (*realop)(A->itype,(a.name= A->name[i],a),(b.name=B->name[0],b))==FALSE ) return FALSE; \
	  break;
	  NSP_ITYPE_SWITCH(A->itype, MAKE_COMP,"");
#undef MAKE_COMP
	  return Loc;
	}
      if ( A->mn == 1 && B->mn != 0 ) 
	{
	  /* Special case A is a constant */
#define MAKE_COMP(name,type,arg)							\
	  for ( i = 0 ; i < A->mn ; i++ )				\
	    if ( (*realop)(A->itype,(a.name= A->name[0],a),(b.name=B->name[i],b))==FALSE ) return FALSE; \
	  break;
	  NSP_ITYPE_SWITCH(A->itype, MAKE_COMP,"");
#undef MAKE_COMP
	  return(Loc);
	}
      /* Incompatible dimensions */
      if ( strcmp(op,"==") == 0) 
	{
	  return FALSE;
	}
      else if ( strcmp(op,"<>") == 0) 
	{
	  return TRUE;
	}
      else 
	{
	  return FALSE;
	}
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  if ( (*realop)(nsp_gint,(a.Gint=1,a),(b.Gint=1,b)) ==FALSE ) return  FALSE;
	}
      else
	{
#define MAKE_COMP(name,type,arg)							\
	  for ( i = 0 ; i < A->mn ; i++ )				\
	    if ( (*realop)(A->itype,(a.name= A->name[i],a),(b.name=B->name[i],b))==FALSE ) return FALSE; \
	  break;
	  NSP_ITYPE_SWITCH(A->itype, MAKE_COMP,"");
#undef MAKE_COMP
	}
    }
  return TRUE;
}


/**
 * nsp_imatrix_find:
 * @A: a #NspIMatrix 
 * @lhs: 1 or 2 
 * @Res1: a pointer to a #NspIMatrix
 * @Res2: a pointer to a #NspIMatrix
 * 
 * returns in one or two #NspIMatrix the indices for which the 
 * Matrix @A has non zero entries. @A is left unchanged
 * according to the value of @lhs one or two arguments are returned 
 * When @A is a non null matrix and @lhs is one, the return value is a 1xn row vector. 
 * But when n is equal to zero and @A is scalar then a 0x0 is returned. 
 *
 * In Matlab the rules are different: 
 * when lhs == 1 
 *   A 1x1 => result 1x1 or 0x0 (like us) 
 *   A mx1 => result px1        (diff)
 *   A 1xn => result 1xp        (like us)
 *   A mxn => result px min(1,max(m,n)) 
 *     (this rule is valid for all m,n cases except the ones covered above)
 * 
 *
 * Return value: %OK or %FAIL  
 **/


/**
 * nsp_imatrix_find:
 * @A: a #NspIMatrix. 
 * 
 * returns in a #NspMatrix the indices for which the 
 * #NspIMatrix @A is non null considering @A as o one dimensional array.
 * 
 * Return value:  a new #NspMatrix or %NULLMAT
 */

int nsp_imatrix_find(NspIMatrix *A, int lhs, NspObject **Res1, NspObject **Res2, char ind_type)
{
  int j,i,count=0; 
  int nrow = ( A->mn == 0) ? 0: 1;
  int *indr=NULL, *indc=NULL;

  /* first pass for counting */
#define IMAT_FIND1(name,type,arg) for ( i=0 ; i < A->mn ; i++) \
    {if ( A->name[i] ) count++;}break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_FIND1,"");
#undef FIND1

  /* special rule for scalars */
  if ( A-> m == 1 && count ==0) nrow =0;
  if ( lhs == 1) 
    {
      if ( (*Res1 = nsp_alloc_mat_or_imat(nrow, count, ind_type, &indr)) == NULLOBJ )
	return FAIL;
      count=0;
#define IMAT_FIND2(name,type,arg) for ( i=0 ; i < A->mn ; i++) \
	if ( A->name[i] ) indr[count++] = i+1; break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_FIND2,"");
#undef FIND2
    }
  else 
    {
      int k;
      if ( (*Res1 = nsp_alloc_mat_or_imat(nrow, count, ind_type, &indr)) == NULLOBJ )
	return FAIL;
      if ( (*Res2 = nsp_alloc_mat_or_imat(nrow, count, ind_type, &indc)) == NULLOBJ )
	{
	  nsp_object_destroy(Res1);
	  return FAIL;
	}
      count=0;
#define IMAT_FIND3(name,type,arg)		\
      for ( j = 0, k = 0 ; j < A->n ; j++ )	\
	for ( i = 0 ; i < A->m ; i++, k++ )	\
	  if (  A->name[k] != 0.0 )		\
	    {					\
	      indr[count] = i+1;		\
	      indc[count++] = j+1;		\
	    }					\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_FIND3,"");
#undef FIND2
    }
  return OK;
}

#if 0
static CompOp *SearchCompBis(const char *op)
{
  int i = 0;
  while ( comptab[i].name != (char *) NULL )
    {
      if ( strcmp(op,comptab[i].name) == 0 )
	return comptab[i].fonc;
      i++;
    }
  return NULL;
}
#endif 

/**
 * nsp_imatrix_mfind:
 * @x: a #NspIMatrix 
 * @m: number of tests
 * @ops: string array given the operator of each of the m tests 
 * @scalars: scalars associated with the tests 
 * @Ind: the result (a matrix of (m+1) indices vectors)
 * 
 * Return value: %OK or %FAIL
 *
 * multiple find  [ind1,...,indm, indm+1] = mfind( x, ops1, sc1, ..., opsm, scm )
 * 
 * opsk is the operator for the k th test (<,<=, >, ...) and sck is the scalar
 * for the k th test. 
 *
 * ind1 is the vector of indices i1 corresponding to components
 *      of x satisfying: x(i1) ops1 sc1  (x(i1) < 0.0 if ops1 is < and sc1 is 0.0) 
 * ind2 is the vector of indices i2 corresponding to components 
 *      of x which don't satisfy test 1 but satisfy test 2:  x(i2) ops2 sc2
 * .....
 * indm+1 is the vector of indices which don't satisfy any of the m tests 
 *
 **/
#if 0
int nsp_imatrix_mfind(const NspIMatrix *x, int m,const char **ops,const double *scalars, NspMatrix **Ind)
{
  CompOp **func = NULL;
  int *length = NULL;
  Boolean found;
  int i, j;

  if ( (length = malloc((m+1)*sizeof(int))) == NULL )
    goto err;
  for ( i = 0 ; i <= m ; i++ ) length[i] = 0;

  if ( (func = malloc(m*sizeof(CompOp *))) == NULL )
    goto err;
  for ( i = 0 ; i < m ; i++ )
    if ( (func[i] = SearchCompBis(ops[i])) == NULL )
      {
	Sciprintf("\nUnknow comp operator <%s>\n",ops[i]);
	goto err;
      }

  for ( i = 0 ; i <= m ; i++ )
    {
      if ( (Ind[i] = nsp_imatrix_create(NVOID, 'r', 1, x->mn)) == NULLIMAT ) 
	goto err;
    }

  for ( j = 0 ; j < x->mn ; j++ )
    {
      found = FALSE;
      for ( i = 0 ; i < m ; i++ )
	if ( func[i](x->R[j], scalars[i]) )
	  {
	    Ind[i]->R[length[i]++] = (double) j+1;
	    found = TRUE;
	    break;
	  }
      if ( ! found )
	Ind[m]->R[length[m]++] = (double) j+1;
    }

  for ( i = 0 ; i <= m ; i++ )
    nsp_imatrix_resize(Ind[i], 1, length[i]);

  free(length);
  free(func);
  return OK;

err:
  free(length);
  free(func);
  for ( i = 0 ; i <= m ; i++ )
    nsp_imatrix_destroy(Ind[i]);
  return FAIL;
}
#endif

/**
 * nsp_imatrix_ndind2ind:
 * @dims: (input) int vector with successive dimension lengths (of a supposed n-dimensional matrix)
 * @nd: (input) size of dims, number of dimensions of the supposed n-dimensional matrix)
 * @ndind: (input) vector pointer onto nd #NspIMatrix each one having the role of an index vector
 * @Ind: (output) a #NspIMatrix having the role of an index vector equivalent to the
 *       multiple indexing (the supposed n-dimensional matrix having the fortran indexing scheme) 
 * 
 * [ind] = ndind2ind(dims, I_1, I_2, ..., I_nd)
 *
 * dims = [n_1, n_2, ..., n_nd]  each k \in I_k must be 1 <= k <= n_k
 *
 * the supposed "submatrix" being adressed is I_1 x I_2 x ... x I_nd
 *
 * let (i_1, i_2, ..., i_nd) be such a multi_index in I_1 x I_2 x ... x I_nd
 * this routine computes for all these multi-indices, their equivalent one-way indices
 * with the following formula:
 *
 *    i_equ = i_1 + n_1*( (i_2 - 1) + n_2*( (i_3 - 1) + .... + n_{nd-1}*(i_nd - 1)))
 *
 * Return value: %OK or %FAIL
 *
 **/
#if 0
int nsp_imatrix_ndind2ind(int *dims, int nd, NspIMatrix **ndind, NspIMatrix **Ind)
{
  NspIMatrix *ind;
  int *j, i, k, p, ni, ip, K, ntot=1;

  for ( i = 0 ; i < nd ; i++ )
    ntot *= ndind[i]->mn;

  if ( (ind = nsp_imatrix_create(NVOID,'r',1,ntot)) == NULLIMAT) return FAIL;
  j = (int *) ind->R;

  K = ndind[nd-1]->mn;
  for ( k = 0 ; k < K ; k++ )
    {
      j[k] = ((int) ndind[nd-1]->R[k]) - 1;
      if ( j[k]  < 0  ||  j[k] >= dims[nd-1] )
	{
	  Scierror("Error: %d th index out of bounds in the last index vector\n", k+1);
	  goto err;
	}
    }

  for ( i = nd-2 ; i >= 0 ; i-- )
    {
      ni = ndind[i]->mn;
      for ( k = 0 ; k < K ; k++ )
	j[k] *= dims[i];

      for ( p = ni-1 ; p >= 0 ; p-- )
	{
	  ip = ((int) ndind[i]->R[p]) - 1;
	  if ( ip  < 0  ||  ip >= dims[i] )
	    {
	      Scierror("Error: %d th index out of bounds in the %d th index vector\n", p+1, i+1);
	      goto err;
	    }
	  if ( p > 0 ) memcpy(&j[K*p],j,K*sizeof(int));
	  for ( k = K*p ; k < K*p+K ; k++ )
	    j[k] += ip;
	}
      K *= ni;
    }

  for ( i = 0 ; i < ntot ; i++ ) j[i]++;

  ind->convert = 'i';
  ind = Mat2double(ind);
  *Ind = ind;
  return OK;

 err:
  nsp_imatrix_destroy(ind);
  return FAIL;
}
#endif 
  
/**
 * nsp_imatrix_sub2ind:
 * @dims: (input) int vector with successive dimension lengths (of a supposed n-dimensional matrix)
 * @nd: (input) size of dims, number of dimensions of the supposed n-dimensional matrix)
 * @ndind: (input) vector pointer onto nd #NspIMatrix each one having the role of an index vector
 *         all the index vectors have the same number of components (@nb_ind)
 * @nb_ind: (input)  number of components of the index vectors
 * @Ind: (output) a #NspIMatrix having the role of a one-way index vector equivalent to the nb_ind
 *       multiple indices Ind_equ(k) <-> (i_1(k),i_2(k),....,i_nd(k))
 *
 * (the supposed n-dimensional matrix having the fortran indexing scheme) 
 * 
 * [ind] = sub2ind(dims, I_1, I_2, ..., I_nd)
 *
 * dims = [n_1, n_2, ..., n_nd]  each k \in I_k must be 1 <= k <= n_k
 *
 * We compute for each k = 1,...,nb_ind 
 *
 *    Ind_equ(k) = i_1(k) + n_1*( (i_2(k) - 1) + n_2*( (i_3(k) - 1) + .... + n_{nd-1}*(i_nd(k) - 1)))
 *
 * Return value: %OK or %FAIL
 *
 **/
#if 0
int nsp_imatrix_sub2ind(int *dims, int nd, NspIMatrix **ndind, int nb_ind, NspIMatrix **Ind)
{
  NspIMatrix *ind;
  int *j, i, k, p;

  if ( (ind = nsp_imatrix_create(NVOID,'r',1,nb_ind)) == NULLIMAT) return FAIL;
  j = (int *) ind->R;

  for ( k = 0 ; k < nb_ind ; k++ )
    {
      p = (int) ndind[nd-1]->R[k];

      if ( p  < 1  ||  p > dims[nd-1] )
	{
	  Scierror("Error: component %d of vector index %d is out of bounds\n", k+1,nd);
	  goto err;
	}
      j[k] = p-1;
    }
  
  for ( i = nd-2 ; i >= 0 ; i-- )
    {
      for ( k = 0 ; k < nb_ind ; k++ )
	{
	  p = (int) ndind[i]->R[k];
	  if ( p  < 1  ||  p > dims[i] )
	    {
	      Scierror("Error: component %d of vector index %d is out of bounds\n", k+1,i+1);
	      goto err;
	    }
	  j[k] = p-1 + dims[i]*j[k];
	}
    }

  for ( k = 0 ; k < nb_ind ; k++ ) j[k]++; /* 1-based indices... and not 0-based */

  ind->convert = 'i';
  ind = Mat2double(ind);
  *Ind = ind;
  return OK;

 err:
  nsp_imatrix_destroy(ind);
  return FAIL;
}
#endif 



/**
 * nsp_scalar_sub_imatrix_bis:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix of size 1x1 (that is a scalar) 
 * 
 * Do the operation A <- B - A  when B is a scalar
 * and when nsp is compiled in MTLB_IDIVMODE (matlab
 * 's behavior for empty matrix)
 *
 * Return value: %FAIL or %OK
 **/

int nsp_scalar_sub_imatrix_bis(NspIMatrix *A, NspIMatrix *B) 
{
  int i;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
  if ( A->mn == 0 )  return OK;
#define IMAT_AC(name,type,arg) for ( i=0 ; i < A->mn ; i++) A->name[i] = B->name[0] - A->name[i];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
  return OK;
}

/**
 * nsp_imatrix_sub_mat:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix  
 * 
 * Do the operation A <- A - B when nsp is compiled 
 * in MTLB_MODE (matlab 's behavior for empty matrix)
 * 
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_sub_mat(NspIMatrix *A, NspIMatrix *B)
{
  int i;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }

  if (SameDim(A,B))
    {
#define IMAT_AC(name,type,arg) for ( i=0 ; i < A->mn ; i++) A->name[i] -= B->name[i];break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
}


/**
 * nsp_imatrix_scale_rows:
 * @A: a #NspIMatrix of size m x n
 * @x: a #NspIMatrix must be a vector of size m (1 x m or m x 1)
 * @op: a char '*' to multiply or '/' to divide
 *  for (i from 0 to m-1)  
 *      multiplie or divide row i of A by x[i]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_scale_rows(NspIMatrix *A, NspIMatrix *x, char op)
{
  int i,j, k;

  if ( A->itype != x->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
#define IMAT_AC(name,type,arg)				\
  for ( j = 0, k=0 ; j < A->n ; j++)		\
    for ( i = 0 ; i < A->m ; i++, k++ )		\
      A->name[k] *= x->name[i];			\
  break;
#define IMAT_AD(name,type,arg)				\
  for ( j = 0, k=0 ; j < A->n ; j++)		\
    for ( i = 0 ; i < A->m ; i++, k++ )		\
      A->name[k] /= x->name[i];			\
  break;

  if ( op == '*' )
    {
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
    }
  else
    {
      NSP_ITYPE_SWITCH(A->itype,IMAT_AD,"");
    }

#undef IMAT_AC
#undef IMAT_AD

  return OK;
}

/**
 * nsp_imatrix_scale_cols:
 * @A: a #NspIMatrix of size m x n
 * @x: a #NspIMatrix must be a vector of size n (1 x n or n x 1)
 * @op: a char '*' to multiply or '/' to divide
 *  for (j from 0 to n-1)  
 *      multiplie or divide column j of A by x[j]
  * 
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_scale_cols(NspIMatrix *A, NspIMatrix *x, char op)
{

  int i,j, k;
  if ( A->itype != x->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return FAIL;
    }
#define IMAT_AC(name,type,arg)				\
  for ( j = 0, k=0 ; j < A->n ; j++)		\
    for ( i = 0 ; i < A->m ; i++, k++ )		\
      A->name[k] *= x->name[j];			\
  break;
#define IMAT_AD(name,type,arg)				\
  for ( j = 0, k=0 ; j < A->n ; j++)		\
    for ( i = 0 ; i < A->m ; i++, k++ )		\
      A->name[k] /= x->name[j];			\
  break;

  if ( op == '*' )
    {
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
    }
  else
    {
      NSP_ITYPE_SWITCH(A->itype,IMAT_AD,"");
    }

#undef IMAT_AC
#undef IMAT_AD

  return OK;
}

/**
 * nsp_imatrix_nnz:
 * @A: a #NspIMatrix of size m x n
 * 
 * counts non null elements. 
 * 
 * Return value: an integer 
 **/

int nsp_imatrix_nnz(NspIMatrix *A)
{
  int i, count =0;
#define IMAT_AC(name,type,arg)				\
  for ( i = 0;  i < A->mn ; i++ )		\
    if ( A->name[i] ) count++;			\
  break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");
#undef IMAT_AC
  return count;
}


/**
 * nsp_imatrix_unique:
 * @x: (input-output) real #NspIMatrix (considered as a vector)
 * @Ind: (output) if NOT NULL a real #NspIMatrix (see after)
 * @Occ: (output) if NOT NULL a real #NspIMatrix (see after)
 * @first_ind: (input) used in case Ind is not NULL true if Ind[i]
 *             should be the first index of x[i] in the original @x
 *
 * Removes from @x multiple occurences of identical entries. Thus after 
 * calling this function @x will contain strictly different values sorted 
 * in increasing order. If @Ind is non null, it will be set to 
 * a new NspIMatrix filled with the indice in the original array of each 
 * output value in @x. Thus @Ind[i] will contain the original indice of 
 * @x[i] in the input array @x. Note that indices in @Ind start at 1 !
 * If @Occ is non null, then @Occ[i] will contain the number of occurences 
 * in the input matrix @x of the output value @x[i]. 
 *
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_unique(NspIMatrix *x, NspObject **Ind, NspMatrix **Occ, Boolean first_ind, char ind_type)
{
  int i0=0, i, i_old, *index;
  NspMatrix *occ=NULLMAT;
  nsp_int_union val;


  if ( Ind == NULL )
    {
      i0=0;
      if ( x->mn <= 1 ) return OK;
#define IMAT_AC(name,type,arg)				\
      nsp_qsort_##type( x->Iv, NULL, 0, x->mn, 'i');	\
      i0 = 0; val.name = x->name[0];			\
      for ( i = 1 ; i < x->mn ; i++ )			\
	if ( x->name[i] != val.name )			\
	  {						\
	    i0++;					\
	    val.name = x->name[i0] = x->name[i];	\
	  }						\
      break;
      NSP_ITYPE_SWITCH(x->itype,IMAT_AC,"");
#undef IMAT_AC
      if ( x->m == 1 )
	nsp_imatrix_resize(x, 1, i0+1);
      else
	nsp_imatrix_resize(x, i0+1, 1);
      return OK;
    }

  else
    {
      if ( (*Ind = nsp_alloc_mat_or_imat(x->m, x->n, ind_type, &index)) == NULLOBJ )
	return FAIL;

      if ( Occ != NULL )
	if ( (occ = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	  {
	    nsp_object_destroy(Ind); return FAIL;
	  }
      
      if ( x->mn > 0 )
	{
	  i0 = 0;
#define IMAT_AC(name,type,arg)					\
	  if ( first_ind )					\
	  nsp_sqsort_bp_##type( x->Iv, x->mn, index, 'i');	\
	  else							\
	    nsp_qsort_##type( x->Iv, index, 1, x->mn, 'i');	\
	  i0 = 0; val.name = x->name[0]; i_old = 0;			\
	  for ( i = 1 ; i < x->mn ; i++ )			\
	    {							\
	    if ( x->name[i] != val.name )				\
	      {								\
	      if (Occ != NULL) { occ->R[i0] = i - i_old; i_old = i; }	\
	      i0++;							\
	      val.name = x->name[i0] = x->name[i];					\
	      index[i0] = index[i];					\
	      }								\
	    }								\
	  if (Occ != NULL) occ->R[i0] = x->mn - i_old;			\
	  break;
	  NSP_ITYPE_SWITCH(x->itype,IMAT_AC,"");
#undef IMAT_AC
	  
	  if ( x->m == 1 )
	    {
	      nsp_imatrix_resize(x, 1, i0+1);
	      if ( ind_type == 'd' )
		nsp_matrix_resize((NspMatrix *) *Ind, 1, i0+1);
	      else
		nsp_imatrix_resize((NspIMatrix *) *Ind, 1, i0+1);
	      if ( Occ != NULL ) nsp_matrix_resize(occ, 1, i0+1);
	    }
	  else
	    {
	      nsp_imatrix_resize(x, i0+1, 1);
	      if ( ind_type == 'd' )
		nsp_matrix_resize((NspMatrix *) *Ind, i0+1, 1);
	      else
		nsp_imatrix_resize((NspIMatrix *) *Ind, i0+1, 1);
	      if ( Occ != NULL ) nsp_matrix_resize(occ, i0+1, 1);
	    }
	  if ( ind_type == 'd' )
	    *Ind = (NspObject *) Mat2double((NspMatrix *) *Ind);
	}

      if ( Occ != NULL ) *Occ = occ;
      return OK;
    }
}

/**
 * nsp_imatrix_unique_rows:
 * @x: (input-output) #NspIMatrix
 * @Ind: (output) if NOT NULL a vector with indices
 * @Occ: (output) if NOT NULL a real #NspMatrix (see after)
 * @ind_type: (input) should be 'i' or 'd'. This gives the type of the
 *            vector which stores the indices.
 * Removes from @x multiple occurences of identical rows
 *
 * Return value: %OK or %FAIL
 **/
int nsp_imatrix_unique_rows(NspIMatrix *x, NspObject **Ind, NspMatrix **Occ, char ind_type)
{
  int i0=0, i, j, i_old, *index;
  NspMatrix *occ=NULLMAT;
  Boolean equal;

  if ( Ind == NULL )
    {
      if ( x->m <= 1 ) return OK;
#define IMAT_AC(name,type,arg)				           \
      { type *pt;                                                  \
      nsp_qsort_gen_lexirow_##type( x->Iv, NULL, 0, x->m, x->n,'i');\
      i0 = 0;                                                      \
      for ( i = 1 ; i < x->m ; i++ )                               \
	{                                                          \
	  for ( j = 0, equal = TRUE ; equal && j < x->n ; j++ )    \
	    equal = x->name[i0 + j*x->m] ==  x->name[i + j*x->m];  \
	  if ( ! equal )                                           \
	    {                                                      \
	      i0++;                                                \
	      for ( j= 0 ; j < x->n ; j++ )                        \
		x->name[i0 + j*x->m] = x->name[i + j*x->m];        \
	    }                                                      \
	}                                                          \
      for ( j = 1, pt = x->name+i0+1 ; j < x->n ; j++ , pt+=i0+1 ) \
	memmove(pt, x->name+j*x->m, (i0+1)*sizeof(type) );         \
      }                                                            \
      break;
      NSP_ITYPE_SWITCH(x->itype,IMAT_AC,"");
#undef IMAT_AC
      nsp_imatrix_resize(x, i0+1, x->n);
      return OK;
    }
  else
    {
      if ( (*Ind = nsp_alloc_mat_or_imat(x->m, 1, ind_type, &index)) == NULLOBJ )
	return FAIL;

      if ( Occ != NULL )
	if ( (occ = nsp_matrix_create(NVOID,'r',x->m,1)) == NULLMAT )
	  {
	    nsp_object_destroy(Ind); return FAIL;
	  }
      
      if ( x->m > 0 )
	{
#define IMAT_AC(name,type,arg)				                  \
	  { type *pt;							\
	    nsp_qsort_gen_lexirow_##type( x->Iv, index, 1, x->m, x->n,'i'); \
	  i0 = 0; i_old = 0;                                              \
	  for ( i = 1 ; i < x->m ; i++ )                                  \
	    {                                                             \
	      for ( j = 0, equal = TRUE ; equal && j < x->n ; j++ )       \
		equal = x->name[i0 + j*x->m] ==  x->name[i + j*x->m];     \
	      if ( ! equal )                                              \
		{                                                         \
		  if (Occ != NULL) { occ->R[i0] = i - i_old; i_old = i; } \
		  i0++;                                                   \
		  for ( j= 0 ; j < x->n ; j++ )                           \
		    x->name[i0 + j*x->m] = x->name[i + j*x->m];           \
		  index[i0] = index[i];                                   \
		}                                                         \
	      else                                                        \
		if ( index[i] < index[i0] )  index[i0] = index[i];        \
	    }                                                             \
	  if (Occ != NULL) occ->R[i0] = x->m - i_old;                     \
	  for ( j = 1, pt = x->name+i0+1 ; j < x->n ; j++ , pt+=i0+1 )    \
	    memmove(pt, x->name+j*x->m, (i0+1)*sizeof(type) );            \
       }                                                              \
	  break;
	  NSP_ITYPE_SWITCH(x->itype,IMAT_AC,"");
#undef IMAT_AC
	  nsp_imatrix_resize(x, i0+1, x->n);

	  if ( ind_type == 'd' )
	    nsp_matrix_resize((NspMatrix *) *Ind, i0+1, 1);
	  else
	    nsp_imatrix_resize((NspIMatrix *) *Ind, i0+1, 1);
	  if ( Occ != NULL ) nsp_matrix_resize(occ, i0+1, 1);

	  if ( ind_type == 'd' )
	    *Ind = (NspObject *) Mat2double((NspMatrix *) *Ind);
	}

      if ( Occ != NULL ) *Occ = occ;
      return OK;
    }
}

/**
 * nsp_imat_unique_columns:
 * @x: (input-output) #NspIMatrix
 * @Ind: (output) if NOT NULL a vector with indices
 * @Occ: (output) if NOT NULL a real #NspMatrix (see after)
 * @ind_type: (input) should be 'i' or 'd'. This gives the type of the
 *            vector which stores the indices.
 * Removes from @x multiple occurences of identical columns
 *
 * Return value: %OK or %FAIL
 **/
int nsp_imatrix_unique_columns(NspIMatrix *x, NspObject **Ind, NspMatrix **Occ, char ind_type)
{
  int j0=0, i, j, j_old=0, *index, pj0, pj;
  NspMatrix *occ=NULLMAT;
  Boolean equal;

  if ( Ind == NULL )
    {
      if ( x->n <= 1 ) return OK;
#define IMAT_AC(name,type,arg)					       \
      nsp_qsort_gen_lexicol_##type( x->Iv, NULL, 0, x->m, x->n, 'i');  \
	j0 = 0;							       \
	for ( j = 1 ; j < x->n ; j++ )				       \
	  {								\
	    for ( i = 0, equal = TRUE, pj0 = j0*x->m, pj = j*x->m ; equal && i < x->m ; i++, pj0++, pj++ ) \
	      equal = x->name[pj0] ==  x->name[pj];			\
	    if ( ! equal )						\
	      {								\
		j0++;							\
		if ( j0 != j) memcpy(x->name + j0*x->m, x->name + j*x->m, x->m*sizeof(type)); \
	      }								\
	  }								\
	break;
      NSP_ITYPE_SWITCH(x->itype,IMAT_AC,"");
#undef IMAT_AC
      nsp_imatrix_resize(x, x->m, j0+1);
      return OK;
    }
  else
    {
      if ( (*Ind = nsp_alloc_mat_or_imat(1, x->n, ind_type, &index)) == NULLOBJ )
	return FAIL;

      if ( Occ != NULL )
	if ( (occ = nsp_matrix_create(NVOID,'r', 1, x->n)) == NULLMAT )
	  {
	    nsp_object_destroy(Ind); return FAIL;
	  }
      
      if ( x->n > 0 )
	{
#define IMAT_AC(name,type,arg)						\
	  nsp_qsort_gen_lexicol_##type( x->Iv, index, 1, x->m, x->n, 'i'); \
	  j0 = 0; j_old = 0;						\
	  for ( j = 1 ; j < x->n ; j++ )				\
	    {								\
	      for ( i = 0, equal = TRUE, pj0 = j0*x->m, pj = j*x->m ; equal && i < x->m ; i++, pj0++, pj++ ) \
		equal = x->name[pj0] ==  x->name[pj];			\
	      if ( ! equal )						\
		{							\
		  if (Occ != NULL) { occ->R[j0] = j - j_old; j_old = j; } \
		  j0++;							\
		  if ( j0 != j) memcpy(x->name + j0*x->m, x->name + j*x->m, x->m*sizeof(type)); \
		  index[j0] = index[j];					\
		}							\
	      else							\
		if ( index[j] < index[j0] )  index[j0] = index[j];	\
	    }								\
	  break;
	  NSP_ITYPE_SWITCH(x->itype,IMAT_AC,"");
#undef IMAT_AC

	  if (Occ != NULL) occ->R[j0] = x->n - j_old;                        

	  nsp_imatrix_resize(x, x->m, j0+1);

	  if ( ind_type == 'd' )
	    nsp_matrix_resize((NspMatrix *) *Ind, 1, j0+1);
	  else
	    nsp_imatrix_resize((NspIMatrix *) *Ind, 1, j0+1);
	  if ( Occ != NULL ) nsp_matrix_resize(occ, 1, j0+1);

	  if ( ind_type == 'd' )
	    *Ind = (NspObject *) Mat2double((NspMatrix *) *Ind);
	}

      if ( Occ != NULL ) *Occ = occ;
      return OK;
    }
}

/**
 * nsp_imatrix_dot:
 * @A: a #NspIMatrix.
 * @B: a #NspIMatrix.
 * @dim_flag: (input) dim parameter (0, 1 or 2)
 *
 * computes scalar products between @A and @B:
 *
 *  @dim_flag = 0: sum_i_j conj(A_i_j)* B_i_j
 *  @dim_flag = 1: sum_i   conj(A_i_j)* B_i_j  for each j (got a row vector)
 *  @dim_flag = 2: sum_j   conj(A_i_j)* B_i_j  for each i (got a column vector)
 *
 * this is easily writing in nsp with sum(A.*B, dim_flag) (real case)
 *                            or with sum(conj(A).*B, dim_flag) (complex case)
 * but using various dot function from BLAS and from matutil.c (which completes
 * the BLAS) is much faster.
 *
 * Return value: an #NspIMatrix storing the result
 **/
#if 0
NspIMatrix *nsp_imatrix_dot(NspIMatrix *A, NspIMatrix *B, int dim_flag)
{
  NspIMatrix *C;
  int j, one=1;
  
  switch (dim_flag) 
    {
    default :
      Sciprintf("\nInvalid dim flag '%d' assuming dim=0\n", dim_flag);

    case 0: 
      if ((C = nsp_imatrix_create(NVOID,type,Min(A->m,1),Min(A->n,1))) == NULLIMAT) 
	return NULLIMAT;
      if (C->mn == 1)
	{
	  C->R[0] = C2F(ddot)(&(A->mn), A->R, &one, B->R, &one);
	}
      break;
    case 1:
      if ((C = nsp_imatrix_create(NVOID,type,1,A->n)) == NULLIMAT) 
	return NULLIMAT;
      if ( A->n >= 1 )
	{
	  for ( j = 0 ; j < A->n ; j++ ) 
	    C->R[j] = C2F(ddot)(&(A->m), A->R+j*A->m, &one, B->R+j*A->m, &one);
	}
      break ;
    case 2:
      if ((C = nsp_imatrix_create(NVOID,type,A->m,1)) == NULLIMAT) 
	return NULLIMAT;
      if ( A->m >= 1 )
	{
	  nsp_rowdddot(A->R, B->R, C->R, A->m, A->n);
	}
      break;
    }
  return C;
}
#endif 

/**
 * nsp_imatrix_cross:
 * @X: #NspIMatrix  (should be 3 x n or m x 3)
 * @Y: #NspIMatrix  (should be 3 x n or m x 3) 
 * @dim: (input) dim parameter (1 or 2) (if dim = 1 matrices should be 3 x n
 *       and if dim = 2 matrces should be m x 3)
 *
 * computes vectors products between respectives rows or columns vectors of @X and @Y:
 *
 * dim = 1  then Z(:,j) = X(:,j) x Y(:,j)  for each j
 * dim = 2  then Z(i,:) = X(i,:) x Y(i,:)  for each i
 *
 * Return value: an #NspIMatrix storing the result
 **/

#if 0
NspIMatrix *nsp_imatrix_cross(NspIMatrix *X, NspIMatrix *Y, int dim)
{
  NspIMatrix *Z;
  int p;

  if ( X->itype != Y->itype ) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return FAIL;
    }

  /* these checks are also done by the nsp interface but this is useful in case of internal use */
  if ( (X->m != Y->m) || (X->n != Y->n) ) return NULLIMAT;

  if ( dim == 1 )
    {
      if ( X->m != 3 ) return NULLIMAT;
    }
  else if ( dim == 2 )
    {
      if ( X->n != 3 ) return NULLIMAT;
    }
  else
    return NULLIMAT;

  if ( (Z = nsp_imatrix_create(NVOID, X->m, X->n,X->itype)) == NULLIMAT )
    return NULLIMAT;

  /* number of vectors pairs */
  p = dim == 1 ? X->n : X->m;
  nsp_dcross(X->R, Y->R, Z->R, p, dim);
  return Z;
}
#endif 

/* this routine can test if:
 *   1/ a whoole array is ordered (length=A->mn, start=0, inc=1)
 *   2/ a given column, says column j, of A is ordered (length=A->m, start=j*A->m, inc=1)
 *   3/ a given row, says row i, of A is ordered (length=A->n, start=i, inc=A->m)
 */
static Boolean array_is_sorted(NspIMatrix *A, int length, int start, int inc, Boolean strict_order)
{
  Boolean bool = TRUE;
  int i, j;

  if ( length < 1 ) return bool;

  if ( strict_order )
    {
      if ( inc == 1 )
	{
#define IMAT_AC(name,type,arg)					\
	  for ( i = start+1 ; i < start+length && bool ; i++ )	\
	    bool = A->name[i-1] < A->name[i];				\
	  break;
	  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
	}
      else
	{
#define IMAT_AC(name,type,arg)					\
	  for ( i = 1, j=start+inc ; i < length && bool ; i++, j+=inc )	\
	    bool = A->name[j-inc] < A->name[j];				\
	  break;
	  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
	}
    }

  else
    {
      if ( inc == 1 )
	{
#define IMAT_AC(name,type,arg)					\
	  for ( i = start+1 ; i < start+length && bool ; i++ )	\
	    bool = A->name[i-1] <= A->name[i];				\
	  break;
	  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
	}
      else
	{
#define IMAT_AC(name,type,arg)					\
	  for ( i = 1, j=start+inc ; i < length && bool ; i++, j+=inc )	\
	    bool = A->name[j-inc] <= A->name[j];				\
	  break;
	  NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
	}
    }

  return bool;
} 

/* compare 2 columns or 2 rows of A in the lexicographic meaning: 
 *     to compare columns j1 and j2 use : n=A->m, startx= j1*A->m, starty=j2*A->m and inc=1
 *     to compare rows i1 and i2 use : n=A->n, startx= i1, starty=i2 and inc=A->m
 */
static Boolean compare_vect(NspIMatrix *A, int n, int startx, int starty, int inc, Boolean strict_order)
{
  int i, kx, ky;

  if ( n < 1 ) return TRUE;

  if ( strict_order )
    {
#define IMAT_AC(name,type,arg)	                \
      for ( i = 0, kx = startx, ky = starty ; i < n  ; i++, kx+=inc, ky+=inc ) \
	{					\
	  if ( A->name[kx] < A->name[ky] )	\
	    return TRUE;			\
	  else if ( A->name[kx] > A->name[ky] ) \
	    return FALSE;			\
	}					\
      return FALSE;				\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
    }
  else
    {
#define IMAT_AC(name,type,arg)	                \
      for ( i = 0, kx = startx, ky = starty ; i < n  ; i++, kx+=inc, ky+=inc ) \
	{					\
	  if ( A->name[kx] < A->name[ky] )	\
	    return TRUE;			\
	  else if ( A->name[kx] > A->name[ky] ) \
	    return FALSE;			\
	}					\
      return TRUE;				\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_AC,"");	
#undef IMAT_AC
    }

  return TRUE;   /* to avoid a gcc warning */
}

/**
 * nsp_imatrix_issorted:
 * @A: (input) #NspIMatrix
 * @flag: (input) 0 = test_sort_g to test if the whoole array is sorted
 *                1 = test_sort_c to test if each row is sorted (got a column boolean vector)
 *                2 = test_sort_r to test if each column is sorted (got a row boolean vector)
 *                3 = test_sort_lc to test if the columns are lexically ordered
 *                4 = test_sort_lr to test if the rows are lexically ordered
 * @strict_order: (input) true for "<" and false for  "=<"
 *
 * Return value: TRUE or FALSE
 **/
NspBMatrix *nsp_imatrix_issorted(NspIMatrix *A, int flag, Boolean strict_order)
{
  NspBMatrix *C;
  Boolean bool = TRUE;
  int i, j;

  if ( flag == test_sort_c )
    {
      if ( (C = nsp_bmatrix_create(NVOID,A->m,Min(A->n,1))) == NULLBMAT ) 
	return NULLBMAT;
    }
  else if ( flag == test_sort_r )
    {
      if ( (C = nsp_bmatrix_create(NVOID,Min(A->m,1),A->n)) == NULLBMAT ) 
	return NULLBMAT;
    }
  else
    {
      if ( (C = nsp_bmatrix_create(NVOID,Min(A->m,1),Min(A->n,1))) == NULLBMAT ) 
	return NULLBMAT;
    }

  if ( C->mn == 0 )
    return C;


  switch ( flag ) 
    {
    case test_sort_g:
      C->B[0] =  array_is_sorted(A, A->mn, 0, 1, strict_order);
      break;
      
    case test_sort_r:    /* test if each column is sorted */
      for ( j = 0 ; j < A->n ; j++ )
	C->B[j] =  array_is_sorted(A, A->m, j*A->m, 1, strict_order);
      break;

    case test_sort_c:    /* test if each row is sorted */
      for ( i = 0 ; i < A->m ; i++ )
	C->B[i] =  array_is_sorted(A, A->n, i, A->m, strict_order);
      break;

    case test_sort_lr:  /* are rows sorted in the lexicographic meaning ? */
      for ( i = 1 ; i < A->m && bool ; i++ )
	bool = compare_vect(A,  A->n, i-1, i, A->m, strict_order);
      C->B[0] = bool;
      break;

    case test_sort_lc: /* are columns sorted in the lexicographic meaning ? */
      for ( j = 1 ; j < A->n && bool ; j++ )
	bool = compare_vect(A, A->m, (j-1)*A->m, j*A->m, 1, strict_order);
      C->B[0] = bool;
      break;
    }
  
  return C;
}


/**
 * nsp_imatrix_has:
 * @A: (input) #NspIMatrix
 * @x: (input) #NspIMatrix
 * @lhs: (input) dim parameter: for lhs=2 ind must be computed, for lhs=3, ind and ind2 must be computed
 * @ind: (optional output) 
 * @ind2: (optional output) 
 *
 * looks for each component of @x if it is in @A or not with additional first 1-index (if lhs=2) or 2-index.
 * (if lhs=3)
 * Return value: a NspBMatrix
 **/

NspBMatrix *nsp_imatrix_has(NspIMatrix *A, NspIMatrix *x, int lhs, NspMatrix **ind, NspMatrix **ind2)
{
  NspBMatrix *B=NULLBMAT;
  NspMatrix *Ind=NULLMAT, *Ind2=NULLMAT;
  int i, k;
  if ( A->itype != x->itype ) 
    {
      Scierror("Error: arguments must have the same integer type\n");
      return NULL;
    }

  if ( (B = nsp_bmatrix_create(NVOID,x->m,x->n)) == NULLBMAT )
    return NULLBMAT;

  for ( k = 0 ; k < x->mn ; k++ ) B->B[k] = FALSE;

  if ( lhs >= 2 )
    {
      if ( (Ind = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	goto err;
      for ( k = 0 ; k < x->mn ; k++ ) Ind->R[k] = 0.0;
      
      if ( lhs == 3 )
	{
	  if ( (Ind2 = nsp_matrix_create(NVOID,'r',x->m,x->n)) == NULLMAT )
	    goto err;
	  for ( k = 0 ; k < x->mn ; k++ ) Ind2->R[k] = 0.0;
	}
    }

#define IMAT_HAS(name,type,arg)							\
  for ( i = 0 ; i < A->mn ; i++ )					\
    if ( A->name[i] == x->name[k] )					\
      {									\
	B->B[k] = TRUE;							\
    if ( lhs == 2 )							\
      {									\
	Ind->R[k] = i+1;						\
      }									\
    else if ( lhs == 3 )						\
      {									\
	Ind->R[k] = (i % A->m) + 1; Ind2->R[k] = i / A->m + 1;		\
      }									\
    break;								\
      }									\
  break;

  for ( k = 0 ; k < x->mn ; k++ )
    {
      NSP_ITYPE_SWITCH(A->itype,IMAT_HAS,"");
    }
#undef IMAT_HAS

  if ( lhs >= 2 )
    {
      *ind = Ind;
      if ( lhs == 3 )
	*ind2 = Ind2;
    }
  return B;

 err:
  nsp_bmatrix_destroy(B);
  nsp_matrix_destroy(Ind);
  nsp_matrix_destroy(Ind2);
  return NULLBMAT;
}
