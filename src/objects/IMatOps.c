/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 * Copyright (C) 2005-2008 Bruno Pincon Esial/Iecn
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

#include "nsp/object.h"
#include "nsp/cnumeric.h"
#include "nsp/matutil.h"
#include "nsp/gsort-p.h"
#include "nsp/nsp_lapack.h"
#include "nsp/lapack-c.h"

#include <nsp/blas.h>
#include <nsp/matutil.h>


static void Kronecker (NspIMatrix *A,NspIMatrix *B,NspIMatrix *PK);
typedef int (*AdSu) (const int,const void *,void *);
typedef int (*AdSuZ) (int*,doubleC *,int*,doubleC *,int*);
static int MatOpScalar (NspIMatrix *Mat1,NspIMatrix *Mat2,AdSu F1);
static int nsp_imatrix_readline(FILE *fd, char *Info);
static int nsp_numtokens(char *string);
static void nsp_ciset(const int *n,const double *z, doubleC *tab, const int *inc);

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
  /* A = [] */ 
  if ( A->mn == 0) 
    {
      if ( flag == 1) return OK ; 
      else
	{
	  /* flag != 1 ==> [] op A  returns F4(A) **/
	  /* resize A to B */ 
	  if ( nsp_imatrix_fill_with(A,B) == FAIL) return FAIL;
	  return (*F2)(A); 
	}
    }
  /* B = [] */ 
  if ( B->mn == 0) 
    {
      if ( flag == 1) 
	/* flag == 1 ==> A op [] returns [] **/
	return  nsp_imatrix_resize(A,0,0); 
      else
	return OK; 
    }
  /* B is scalar */ 
  if ( B->mn == 1) 
    return (*F1)(A,B);
  else if ( A->mn == 1 ) 
    {
      if ( nsp_imatrix_scalar_to_mn(A,B->m,B->n) == FAIL) return FAIL; 
    }
  /* general case */
  return (*F3)(A,B); 
}


/* a generic function for Mat op scalar 
 *   Mat1 is a Matrix Mat2 is a scalar 
 *   F1 is used if both argument are real
 *   F2 is used if both are complex 
 **/

static int MatOpScalar(NspIMatrix *Mat1, NspIMatrix *Mat2, AdSu F1)
{
  if ( Mat1->itype != Mat2->itype ) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return FAIL;
    }
  (*F1)((Mat1->mn),Mat2->Iv,Mat1->Iv);
  return OK;
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
 * Returns a #NspIMatrix or %NULLIMAT.
 */

NspIMatrix *nsp_imatrix_mult(NspIMatrix *A, NspIMatrix *B, int flag)
{ 
  /* 
  doubleC zalpha={1.00,0.00},zbeta={0.00,0.00};
  double alpha=1.00,beta=0.00;
  */
  NspIMatrix *Loc;
  int m, n, left, right;
  char *flagA, *flagB;

  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return NULL;
    }
  switch ( flag ) 
    {  
    default: 
      Sciprintf("Invalid flag '%d' assuming 0\n", flag);
    case 0: 
      m = A->m; left = A->n; right = B->m; n = B->n; 
      flagA = "N"; flagB = "N";      
      break;
    case 1: 
      m = A->n; left = A->m; right = B->m; n = B->n; 
      flagA = "C"; flagB = "N";
      break;
    case 2: 
      m = A->m; left = A->n; right = B->n; n = B->m; 
      flagA = "N"; flagB = "C"; 
      break;
    case 3: 
      m = A->n; left = A->m; right = B->n; n = B->m;
      flagA = "C"; flagB = "C"; 
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
      nsp_imatrix_set_rval(Loc, 0.0); 
      return Loc;
    }

  if ( (Loc =nsp_imatrix_create(NVOID,m,n,A->itype)) == NULLIMAT ) goto err;

  if ( Loc->m != 0 && Loc->n != 0) 
    {
      /* we have checked empty case to avoid a Scierror raised by zgemm or dgemm */
      /*
	if ( Loc->rc_type == 'c' ) 
	C2F(zgemm)(flagA,flagB,&m,&n,&left,&zalpha,A->C,&A->m,B->C,&B->m,
	&zbeta,Loc->C,&Loc->m,1,1);
	else 
	C2F(dgemm)(flagA,flagB,&m,&n,&left,&alpha,A->R,&A->m,B->R,&B->m,
	&beta,Loc->R,&Loc->m,1,1); 
      */
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
      Scierror("Error: arguments must have the same integre type\n");
      return FAIL;
    }

  if (SameDim(Mat1,Mat2)== FAIL)
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
#define IMAT_AC(name) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] += Mat2->name[i];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC);
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
#define IMAT_AC(name) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] += Mat2->name[0];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC);
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
#define IMAT_AC(name) for ( i=0 ; i < A->mn ; i++) A->name[i] += B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC);
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
      Scierror("Error: arguments must have the same integre type\n");
      return FAIL;
    }

  if (SameDim(Mat1,Mat2)== FAIL)
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
#define IMAT_AC(name) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] -= Mat2->name[i];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC);
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
#define IMAT_AC(name) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] -= Mat2->name[0];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC);
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
#define IMAT_AC(name) for ( i=0 ; i < A->mn ; i++) A->name[i] -= B->name[0];break;
  NSP_ITYPE_SWITCH(A->itype,IMAT_AC);
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
#define IMAT_AC(name) for ( i=0 ; i < Mat1->mn ; i++) Mat1->name[i] = -Mat1->name[i]+ Mat2->name[0];break;
  NSP_ITYPE_SWITCH(Mat1->itype,IMAT_AC);
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

int nsp_imatrix_maxitt1(NspIMatrix *A, NspIMatrix *B, NspIMatrix *Ind,int j,int flag)
{
  int i,indval;

  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return FAIL;
    }
  if (  SameDim(A,B) )
    {
#define IMAT_MAX(name) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] <  B->name[i] )				\
	  {								\
	    A->name[i] = B->name[i] ;					\
	    if ( flag == 1 ) Ind->name[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
#undef IMAT_MAX
    }
  else if ( B->mn == 1) 
    {
#define IMAT_MAX(name) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] <  B->name[0] )				\
	  {								\
	    A->name[i] = B->name[0] ;					\
	    if ( flag == 1 ) Ind->name[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
#undef IMAT_MAX
    }
  else if ( A->mn == 1) 
    {
      /* A is scalar and B is not: A must be enlarged 
       * and Ind also if flag == 1 
       */
      nsp_int_union xx; 
#define IMAT_MAX(name) xx.name = A->name[0];	\
      if ( flag == 1) indval = (int) Ind->name[0]; break\
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
#undef IMAT_MAX
      if ( nsp_imatrix_resize(A,B->m,B->n) == FAIL) return FAIL;
      if ( flag == 1)
	{
	  if ( nsp_imatrix_resize(Ind,B->m,B->n) == FAIL) return FAIL;
	  nsp_imatrix_set_rval(Ind,indval);
	}
#define IMAT_MAX(name)				\
      for ( i = 0; i < A->mn ; i++ )		\
	if  (  xx.name < B->name[i] )		\
	  {					\
	A->name[i] = B->name[i] ;		\
	if ( flag == 1 ) Ind->name[i] = j;	\
	  }					\
	else					\
	  { A->name[i] = xx.name;}		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
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

int nsp_imatrix_minitt1(NspIMatrix *A, NspIMatrix *B, NspIMatrix *Ind, int j, int flag)
{

  int i,indval;
  if ( A->itype != B->itype ) 
    {
      Scierror("Error: arguments must have the same integre type\n");
      return FAIL;
    }
  if (  SameDim(A,B) )
    {
#define IMAT_MAX(name) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] >  B->name[i] )				\
	  {								\
	    A->name[i] = B->name[i] ;					\
	    if ( flag == 1 ) Ind->name[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
#undef IMAT_MAX
    }
  else if ( B->mn == 1) 
    {
#define IMAT_MAX(name) for ( i=0 ; i < A->mn ; i++)			\
	if  (  A->name[i] >  B->name[0] )				\
	  {								\
	    A->name[i] = B->name[0] ;					\
	    if ( flag == 1 ) Ind->name[i] = j;				\
	  } break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
#undef IMAT_MAX
    }
  else if ( A->mn == 1) 
    {
      /* A is scalar and B is not: A must be enlarged 
       * and Ind also if flag == 1 
       */
      nsp_int_union xx; 
#define IMAT_MAX(name) xx.name = A->name[0];	\
      if ( flag == 1) indval = (int) Ind->name[0]; break\
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
#undef IMAT_MAX
      if ( nsp_imatrix_resize(A,B->m,B->n) == FAIL) return FAIL;
      if ( flag == 1)
	{
	  if ( nsp_imatrix_resize(Ind,B->m,B->n) == FAIL) return FAIL;
	  nsp_imatrix_set_rval(Ind,indval);
	}
#define IMAT_MAX(name)				\
      for ( i = 0; i < A->mn ; i++ )		\
	if  (  xx.name > B->name[i] )		\
	  {					\
	A->name[i] = B->name[i] ;		\
	if ( flag == 1 ) Ind->name[i] = j;	\
	  }					\
	else					\
	  { A->name[i] = xx.name;}		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_MAX);
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
      Scierror("Error: arguments must have the same integre type\n");
      return NULLIMAT;
    }
  if ((Loc = nsp_imatrix_create(NVOID,B->m*A->m,B->n*A->n,A->itype)) == NULLIMAT) 
    return(NULLIMAT);
  if ( Loc->mn == 0 ) return(Loc);
  IKronecker(A,B,Loc);
  return(Loc);
}

/*
 * nsp_imatrix_sort: Index=Sort(A)
 * A is changed, Index created with the indexes 
 * return NULLIMAT on error 
 * WARNING : A must be real but the test is not done here 
 * ======
 * to be updated XXX
 */

/**
 * nsp_imatrix_sort:
 * @A: a #NspIMatrix 
 * @flag: 
 * @str1: 
 * @str2: 
 * 
 * 
 * 
 * Return value: a new #NspIMatrix or %NULLIMAT.
 **/

#if 0 
NspIMatrix *nsp_imatrix_sort(NspIMatrix *A, int flag, char *str1, char *str2)
{
  int iflag=0,inc=-1,*iloc=NULL,Locm=A->m,Locn=A->n;
  NspIMatrix *Loc=NULL;
  if ( flag == 2 )
    {
      if ( str1[0] == 'l' ) 
	{
	  if ( str1[1] == 'r' ) Locn=Min(Locn,1);
	  else Locm=Min(Locm,1);
	}
      if ((Loc= nsp_imatrix_create(NVOID,'r',Locm,Locn))  == NULLIMAT) return(NULLIMAT);
      iflag = 1;
      iloc =(int *) Loc->R;  
    }
  if ( str1[0] == 'l' ) 
    {
      /* Change A to int **/
      nsp_double2int(&A->mn,A->R,(int *) A->R);
    }
  nsp_gsort((int *) A->R,A->R,iloc,&iflag,&A->m,&A->n,str1,str2);
  if ( flag == 2) 
    {
      /* Loc contains ints, NOTE inc = -1 **/
      nsp_int2double(&Loc->mn,iloc,&inc,Loc->R,&inc);
    }
  if ( str1[0] == 'l' ) 
    {
      /* Back to double NOTE inc = -1 **/
      nsp_int2double(&A->mn,(int *)A->R,&inc,A->R,&inc);
    }
  return Loc;
}
#endif 

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

#if 0 
NspIMatrix *nsp_imatrix_sum(NspIMatrix *A, int dim)
{
  NspIMatrix *Sum;
  int j;
  int inc=1;


  if ( A->mn == 0)
    {
      if ( dim == 0 )
	{
	  Sum = nsp_imatrix_create(NVOID,1,1,A->itype);
	  if ( Sum != NULLIMAT) Sum->R[0]=0;
	  return Sum;
	}
      else
	return  nsp_imatrix_create(NVOID,0,0,A->itype);
    }

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);
    case 0: 
      if ((Sum = nsp_imatrix_create(NVOID,A->rc_type,1,1)) == NULLIMAT) 
	return(NULLIMAT);
      if ( A->rc_type == 'r' ) 
	Sum->R[0] =nsp_dsum(&A->mn,A->R,&inc);
      else
	nsp_zsum(&Sum->C[0],&A->mn,A->C,&inc);
      break;

    case 1:
      if ((Sum = nsp_imatrix_create(NVOID,A->rc_type,1,A->n)) == NULLIMAT) 
	return NULLIMAT;
      if ( A->rc_type == 'r' ) 
	for ( j= 0 ; j < A->n ; j++) 
	  Sum->R[j] =nsp_dsum(&A->m,A->R+(A->m)*j,&inc); 
      else
	for ( j= 0 ; j < A->n ; j++) 
	  nsp_zsum(&Sum->C[j],&A->m,A->C+(A->m)*j,&inc);
      break;

    case 2:
      if ((Sum = nsp_imatrix_create(NVOID,A->rc_type,A->m,1)) == NULLIMAT) 
	return NULLIMAT;
      if ( A->rc_type == 'r' )
	nsp_dsumrows(A->R, Sum->R, A->m, A->n);
      else
	nsp_zsumrows(A->C, Sum->C, A->m, A->n);
      break;
    }
  return Sum;
}
#endif 

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
#if 0 
NspIMatrix *nsp_imatrix_prod(NspIMatrix *A, int dim)
{
  NspIMatrix *Prod;
  int j;
  int inc=1,zero=0;

  if ( A->mn == 0) 
    {
      if ( dim == 0 )
	{
	  Prod = nsp_imatrix_create(NVOID,'r',1,1);
	  if ( Prod != NULLIMAT) Prod->R[0]= 1.0;
	  return Prod;
	}
      else 
	return  nsp_imatrix_create(NVOID,'r',0,0);
    }

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0:
      if ((Prod = nsp_imatrix_create(NVOID,A->rc_type,1,1)) == NULLIMAT) 
	return(NULLIMAT);
      if ( A->rc_type == 'r' ) 
	Prod->R[0] = nsp_dprod(A->R, A->mn, 1);
      else
	{
	  Prod->C[0].r =1.00 ; Prod->C[0].i = 0.00;
	  nsp_zvmul(&A->mn,A->C,&inc,Prod->C,&zero);
	}
      break;

    case 1:
      if ((Prod = nsp_imatrix_create(NVOID,A->rc_type,1,A->n)) == NULLIMAT) 
	return NULLIMAT;
      if ( A->rc_type == 'r' ) 
	for ( j= 0 ; j < A->n ; j++) 
	  Prod->R[j] = nsp_dprod(A->R+(A->m)*j, A->m, 1);
      else
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    Prod->C[j].r =1.00 ; Prod->C[j].i = 0.00;
	    nsp_zvmul(&A->m,A->C+(A->m)*j,&inc,Prod->C+j,&zero);
	  }
      break;

    case 2:
      if ((Prod = nsp_imatrix_create(NVOID,A->rc_type,A->m,1)) == NULLIMAT) 
	return NULLIMAT;
      inc = A->m;
      if ( A->rc_type == 'r' ) 
	nsp_dprodrows(A->R, Prod->R, A->m, A->n);
      else
	for ( j= 0 ; j < A->m ; j++) 
	  {
	    Prod->C[j].r =1.00 ; Prod->C[j].i = 0.00;
	    nsp_zvmul(&A->n,A->C+j,&inc,Prod->C+j,&zero);
	  }
      break;

    }

  return Prod;
}
#endif 


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


#if 0
NspIMatrix *nsp_imatrix_cum_prod(NspIMatrix *A, int dim)
{
  double cuprod;
  doubleC C_cuprod;
  NspIMatrix *Prod;
  int i,j, k, kp;

  if ( A->mn == 0) return nsp_imatrix_create(NVOID,'r',A->m,A->n);

  if ((Prod = nsp_imatrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLIMAT) 
    return NULLIMAT;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0: 
      if ( A->rc_type == 'r' ) 
	{
	  cuprod=1.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    Prod->R[i] = (cuprod *= A->R[i]);
	}
      else
	{
	  C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    { 
	      nsp_prod_c(&C_cuprod,&A->C[i]);
	      Prod->C[i] = C_cuprod;
	    }
	}
      break;

    case 1:
      if ( A->rc_type == 'r' ) 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    cuprod=1.00;
	    for ( i=0 ; i < A->m ; i++) 
	      Prod->R[i+(A->m)*j] = (cuprod *= A->R[i+(A->m)*j]);
	  }
      else
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    C_cuprod.r  = 1.00 ; C_cuprod.i = 0.00;
	    for ( i=0 ; i < A->m ; i++) 
	      { 
		nsp_prod_c(&C_cuprod,&A->C[i+j*A->m]);
		Prod->C[i+j*A->m] = C_cuprod;
	      }
	  }
      break;

    case 2:
      if ( A->rc_type == 'r' ) 
	{
	  memcpy(Prod->R, A->R, A->mn*sizeof(double));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    Prod->R[k] *= Prod->R[kp];
	}
      else
	{
	  memcpy(Prod->C, A->C, A->mn*sizeof(doubleC));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    nsp_prod_c(&Prod->C[k], &Prod->C[kp]);
	}
      break;
    }

  return Prod;
}
#endif 

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

#if 0
NspIMatrix *nsp_imatrix_cum_sum(NspIMatrix *A, int dim)
{
  double cusum;
  doubleC C_cusum;
  NspIMatrix *Sum;
  int i,j, k, kp;

  if ( A->mn == 0) 
    return  nsp_imatrix_create(NVOID,'r',A->m,A->n);

  if ((Sum = nsp_imatrix_create(NVOID,A->rc_type,A->m,A->n)) == NULLIMAT) 
    return NULLIMAT;

  switch (dim) 
    {
    default : 
      Sciprintf("Invalid dim flag '%d' assuming 0\n",dim);

    case 0: 
      if ( A->rc_type == 'r' ) 
	{
	  cusum=0.00;
	  for ( i=0 ; i < A->mn ; i++)
	    Sum->R[i] = (cusum += A->R[i]);
	}
      else
	{
	  C_cusum.r  = 0.00 ; C_cusum.i = 0.00;
	  for ( i=0 ; i < A->mn ; i++) 
	    { 
	      Sum->C[i].r = ( C_cusum.r += A->C[i].r);
	      Sum->C[i].i = ( C_cusum.i += A->C[i].i);
	    }
	}
      break;

    case 1:
      if ( A->rc_type == 'r' ) 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    cusum=0.00;
	    for ( i=0 ; i < A->m ; i++) 
	      Sum->R[i+(A->m)*j] = (cusum += A->R[i+(A->m)*j]);
	  }
      else
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    C_cusum.r  = 0.00 ; C_cusum.i = 0.00;
	    for ( i=0 ; i < A->m ; i++) 
	      { 
		Sum->C[i+j*A->m].r = ( C_cusum.r +=A->C[i+j*A->m].r);
		Sum->C[i+j*A->m].i = ( C_cusum.i +=A->C[i+j*A->m].i);
	      }
	  }
      break;

    case 2:
      if ( A->rc_type == 'r' ) 
	{
	  memcpy(Sum->R, A->R, A->mn*sizeof(double));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    Sum->R[k] += Sum->R[kp];
	}
      else
	{
	  memcpy(Sum->C, A->C, A->mn*sizeof(doubleC));
	  for ( k = A->m, kp = 0 ; k < A->mn ; k++, kp++ )
	    {
	      Sum->C[k].r += Sum->C[kp].r;
	      Sum->C[k].i += Sum->C[kp].i;
	    }
	}
      break;
    }

  return Sum;
}
#endif

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
#define IMAT_DIFF(name)						\
      for ( k = 1 ; k <= order ; k++ )				\
	for ( i = 0 ; i < A->mn - k ; i++ )			\
	  Diff->name[i] = Diff->name[i+1] - Diff->name[i];	\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIFF);
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
#define IMAT_DIFF(name)						\
      for ( k = 0 ; k < order ; k++ )				\
	for ( j = 0 ; j < A->n ; j++ )				\
	  for ( i = j*(A->m-k) ; i < (j+1)*(A->m-k) - k ; i++ )	\
	    Diff->name[i-j] = Diff->name[i+1] - Diff->name[i];		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIFF);
#undef IMAT_DIFF
      nsp_imatrix_resize(Diff, A->m-order, A->n);
      break;
    case 2:
      if ( A->n - order <= 0 )
	return nsp_imatrix_create(NVOID,A->m,0,A->itype);
      if ( (Diff = nsp_imatrix_copy(A)) == NULLIMAT ) 
	return NULLIMAT;
#define IMAT_DIFF(name)						\
      for ( k = 1 ; k <= order ; k++ )				\
	for ( i = 0 ; i < A->m ; i++ )				\
	  for ( j = 0, l = i ; j < A->n-k ; j++, l+=A->m )	\
	    Diff->name[l] = Diff->name[l+A->m] - Diff->name[l] ;		\
      break;
      NSP_ITYPE_SWITCH(A->itype,IMAT_DIFF);
#undef IMAT_DIFF
      nsp_imatrix_resize(Diff, A->m, A->n-order);
      break;
    }

  return Diff;
}


typedef int (*MaMi) (int,const double *,int,double *);

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
	imax = (*F)(A->mn,A->Iv,1,M->Iv);
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
		(*Imax)->R[j]=(*F)(A->m,A->R+(A->m)*j,1,&M->R[j]); 
	      }
	}
      else
	if ( M->mn > 0 )
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      (*F)(A->m,A->R+(A->m)*j,1,&M->R[j]); 
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
	      (*Imax)->R[j] = (*F)(A->mn,A->R+j,inc,&M->R[j]);
	}
      else
	if ( M->mn > 0 )
	  for ( j= 0 ; j < A->m ; j++) (*F)(A->mn,A->R+j,inc,&M->R[j]);
      break;
    }
  return M;
}

int nsp_array_maxi(int n,const double *A, int incr, double *amax)
{
  int imax=1,i,i1=1;
  if ( n <= 0 ) return 0;

  *amax = A[0];
  /* look for the first non Nan component */
  i = 0; i1 = 0;
  while ( i < n && ISNAN(A[i]) )
    {
      i += incr; i1++;
    }

  if ( i < n )  /* this vector is not only with Nan... */
    {
      /* So init with the first non Nan component then do the usual loop */
      *amax = A[i]; imax = i1+1;
      for (  ; i < n ; i += incr )
	{
	  if ( A[i] > *amax ) 
	    {
	      *amax = A[i];
	      imax = i1+1;
	    }
	  i1++;
	}
    }
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

NspIMatrix *nsp_imatrix_maxi(NspIMatrix *A, int dim_flag, NspIMatrix **Imax, int lhs)
{
  return MatMaxiMini(A,dim_flag,Imax,lhs,nsp_array_maxi);
}


/*
 *nsp_imatrix_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */


int nsp_array_mini(int n, const double *A, int incr, double *amin)
{
  int imin=1,i,i1=0;
  if ( n <= 0 ) return 0;

  *amin= A[0];
  /* look for the first non Nan component */
  i = 0; i1 = 0;
  while ( i < n && ISNAN(A[i]) )
    {
      i += incr; i1++;
    }

  if ( i < n )  /* this vector is not only with Nan... */
    {
      /* So init with the first non Nan component then do the usual loop */
      *amin = A[i]; imin = i1+1;
      for ( ; i < n ; i += incr )
	{
	  if ( A[i] < *amin ) 
	    {
	      *amin = A[i];
	      imin = i1+1;
	    }
	  i1++;
	}
    }
  return imin;
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
NspIMatrix *nsp_imatrix_mini(NspIMatrix *A, int dim_flag, NspIMatrix **Imax, int lhs)
{
  return MatMaxiMini(A,dim_flag,Imax,lhs,nsp_array_mini);
}


/* 
 *  compute the min and max of a vector taking care of Nan.
 *  Note : it is difficult to use the the minmax algorithm 
 *  (which compares first A[i] and A[i+1] before comparing 
 *  one to the current minimum and the other to the current 
 *  maximum) because it mail fails if there are Nan components.
 */
static void VMiniMaxi(int n, double *A, int incr, double *Amin, double *Amax,
		      int *Imin, int *Imax)
{
  int i,i1, imin, imax;
  double amin, amax;
  imin = imax = 1; amin = amax = A[0];

  /* look for the first non Nan component */
  i = 0; i1 = 1;
  while ( i1 <= n && ISNAN(A[i]) ) { i += incr; i1++; }

  if ( i1 <= n )
    {
      /* init with the first non Nan component then do the usual loop */
      amin = amax = A[i]; imin = imax = i1;

      i1++; i+=incr;
      for (  ; i1 <= n ; i += incr, i1++ )
	{
	  if ( A[i] < amin )
	    {
	      amin = A[i]; imin = i1;
	    }
	  else if ( A[i] > amax )
	    {
	      amax = A[i];
	      imax = i1;
	    }
	}
    }

  *Amax = amax; *Amin = amin; *Imax = imax; *Imin = imin;
  return;
}

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
int nsp_imatrix_minmax(NspIMatrix *A, int dim, NspIMatrix **Amin, NspIMatrix **Imin,
		   NspIMatrix **Amax, NspIMatrix **Imax, int lhs)
{
  NspIMatrix *amin=NULLIMAT, *imin=NULLIMAT, *amax=NULLIMAT, *imax=NULLIMAT;
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

  if (    (amin = nsp_imatrix_create(NVOID,'r',m,n)) == NULLIMAT
       || (amax = nsp_imatrix_create(NVOID,'r',m,n)) == NULLIMAT ) goto err;
  if ( lhs > 2 )
    if (    (imin = nsp_imatrix_create(NVOID,'r',m,n)) == NULLIMAT
         || (imax = nsp_imatrix_create(NVOID,'r',m,n)) == NULLIMAT ) goto err;

  
  if ( A->mn == 0 )  /* special case : quick return */
    { 
      *Amin = amin; *Amax = amax; *Imin = imin; *Imax = imax; 
      return OK; 
    }

  if ( dim == 0 )
    {
      VMiniMaxi(A->mn, A->R, 1, amin->R, amax->R, &indmin, &indmax);
      if (lhs > 2) {imin->R[0] = (double) indmin; imax->R[0] = (double) indmax;}
    }
  else if ( dim == 1 )
    for ( k = 0 ; k < A->n ; k++ )
      {
	VMiniMaxi(A->m, &(A->R[k*A->m]), 1, &(amin->R[k]), &(amax->R[k]), &indmin, &indmax);
	if (lhs > 2) {imin->R[k] = (double) indmin; imax->R[k] = (double) indmax;}
      }
  else
    for ( k = 0 ; k < A->m ; k++ )
      {
	VMiniMaxi(A->n, &(A->R[k]), A->m, &(amin->R[k]), &(amax->R[k]), &indmin, &indmax);
	if (lhs > 2) {imin->R[k] = (double) indmin; imax->R[k] = (double) indmax;}
      }

  *Amin = amin; *Amax = amax; *Imin = imin; *Imax = imax; 
  return OK;

 err:
  nsp_imatrix_destroy(amin); nsp_imatrix_destroy(amax); 
  nsp_imatrix_destroy(imin); nsp_imatrix_destroy(imax);
  return FAIL;
}


/*
 */

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

NspIMatrix *nsp_imatrix_createinit(char *name, char type, int m, int n, double (*func) (/* ??? */))
{
  NspIMatrix *Loc;
  int i1,i2;
  if (( Loc= nsp_imatrix_create(name,type,m,n)) == NULLIMAT) return(NULLIMAT);
  for (i2 = 0 ; i2 < n ; i2++)
    for (i1 = 0 ; i1 < m ; i1++)
      { 
	int mi2=m*i2;
	Loc->R[i1+mi2] = (*func)(i1,i2);
      }
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
  double *Aj;
  for ( j = 0, Aj = A->R ; j < Min(A->m+k-1,A->n) ; j++, Aj += A->m )
    for ( i = Max(0,j+1-k) ; i < A->m ; i++)
      Aj[i] = 0.0;
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
  int j0 = Max(0,k+1);
  double *Aj= &A->R[j0*A->m];
  for ( j = j0; j < A->n ; j++, Aj += A->m )
    for ( i = 0 ; i < Min(A->m,j-k) ; i++)
      Aj[i] = 0.0;
}


/**
 * nsp_imatrix_eye:
 * @m: number of rows
 * @n: number of columns
 * 
 * returns the identity matrix of size @m x @n (eyes(m,n))
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

NspIMatrix *nsp_imatrix_eye(int m, int n)
{
  NspIMatrix *Loc;
  int i;
  if (( Loc= nsp_imatrix_create(NVOID,'r',m,n)) == NULLIMAT) return(NULLIMAT);
  nsp_imatrix_set_rval(Loc,(double) 0.00);
  for ( i=0 ; i < Min(m,n) ; i++) Loc->R[i+m*i]= (double) 1.00;
  return(Loc);
}


/**
 * nsp_imatrix_ones:
 * @m: number of rows
 * @n: number of columns
 * 
 * returns a  @m x @n matrix filled with 1.0
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

NspIMatrix *nsp_imatrix_ones(int m, int n)
{
  NspIMatrix *Loc;
  if ((Loc= nsp_imatrix_create(NVOID,'r',m,n))  == NULLIMAT) return(NULLIMAT);
  nsp_imatrix_set_rval(Loc,(double) 1.00);
  return(Loc);
}


/**
 * nsp_imatrix_zeros:
 * @m: number of rows
 * @n: number of columns
 * 
 * returns a  @m x @n matrix filled with 0.0
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

NspIMatrix *nsp_imatrix_zeros(int m, int n)
{
  NspIMatrix *Loc;
  if ((Loc= nsp_imatrix_create(NVOID,'r',m,n))  == NULLIMAT) return(NULLIMAT);
  nsp_imatrix_set_rval(Loc,(double) 0.00);
  return(Loc);
}

/*
 *nsp_imatrix_rand: A=rand(m,n)
 * A is changed  
 */

/**
 * nsp_imatrix_rand:
 * @m: number of rows
 * @n: number of columns
 * 
 * returns a  @m x @n matrix filled with random samples of normal or 
 * uniform law.
 * 
 * Return value: a new #NspIMatrix or %NULL.
 **/

static int rand_data[] = {1,0};

NspIMatrix *nsp_imatrix_rand(int m, int n)
{
  NspIMatrix *Loc;
  int i;
  /* test2DD();*/ /* XXXXX*/

  if (( Loc= nsp_imatrix_create(NVOID,'r',m,n))  == NULLIMAT) return(NULLIMAT);
  for ( i = 0 ; i < Loc->mn ; i++ ) 
    {
      Loc->R[i]= nsp_urand( rand_data);
      if (rand_data[1] == 1) 
	{
	  double sr,si,t=2.0;
	  while ( t > 1.00) 
	    {
	      sr=2.0*nsp_urand(rand_data)-1.0;
	      si=2.0*nsp_urand(rand_data)-1.0;
	      t = sr*sr+si*si;
	    }
	  Loc->R[i] = sr*sqrt(-2.0*log(t)/t);
	}
    }
  return(Loc);
}

/**
 * nsp_set_urandseed:
 * @m: integer 
 * 
 * sets the seed of the default random generator.
 **/

void nsp_set_urandseed(int m)
{
  rand_data[0] = Max(m,0);
}

/**
 * nsp_get_urandseed:
 * 
 * gets the seed of the default random generator.
 * 
 * Return value: the seed value as an integer.
 **/

int nsp_get_urandseed(void)
{
  return rand_data[0];
}

/**
 * nsp_set_urandtype:
 * @m: integer
 * 
 * sets the default random generator to normal (@m=1) or uniform (@m=0).
 **/

void nsp_set_urandtype(int m)
{
  rand_data[1]=m;
}

/**
 * nsp_get_urandtype:
 * 
 * gets the default genetor law (1 for normal law and 0 for uniform law)
 * 
 * Return value: 1 or 0.
 **/

int nsp_get_urandtype(void)
{
  return rand_data[1];
}

/**
 * nsp_imatrix_pow_matscalar:
 * @A: a #NspIMatrix
 * @B: a #NspIMatrix with only one element (that is a scalar !) 
 * 
 * This routine computes @A^@B where @B must be a scalar (this must
 * be verified by the calling routine (*)). The matrix @A is modified 
 * and hold the final result (if OK is returned).  @A must be
 * square (if not FAIL is returned). When @A is not a scalar
 * this routine works only if @B is an integer (and when @B is
 * a negative integer, @A must be numerically invertible).
 * 
 * (*) The operation A^B is done with the generic interface int_mx_mopscal
 *     which branches to one of the 3 routines:
 *        1/ nsp_imatrix_pow_matscalar(A,B) if B is a scalar 
 *        2/ nsp_imatrix_pow_matmat(A,B), if neither A and B are scalar
 *        3/ nsp_imatrix_pow_scalarmat(B,A), if A is a scalar
 *
 * Return value: %OK or %FAIL
 **/

int nsp_imatrix_pow_matscalar(NspIMatrix *A, NspIMatrix *B) 
{
  int p, i, oddflag=0;
  int A_is_square = A->m==A->n;

  if ( ! A_is_square )
    {
      Scierror("Error:\t matrix must be square\n");
      return FAIL;
    }

  if ( A->m == 1 ) /* A is a scalar */
    return nsp_imatrix_pow_scalar(A,B);

  else if ( B->rc_type == 'c' || B->R[0] != floor(B->R[0]) )
    {
      Scierror("Error:\t ^ operator is not currently implemented for non integer power\n");
      return FAIL;
    }
  
  p = B->R[0];

  if ( p == 0 ) /* return identity matrix */  
    /* FIXME : must we do something if A has Nan ? */
    /* (Matlab : nan^0 => nan but [nan nan;nan nan]^0 => Identity */
    {
      if ( A->rc_type == 'r' )
	{
	  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = 0.0;
	  for ( i = 0 ; i < A->mn ; i+=A->m+1 ) A->R[i] = 1.0;
	}
      else
	{
	  for ( i = 0 ; i < A->mn ; i++ ) { A->C[i].r = 0.0 ; A->C[i].i = 0.0; }  
	  for ( i = 0 ; i < A->mn ; i+=A->m+1 ) A->C[i].r = 1.0;
	}
      return OK;
    }


  if ( p < 0 )
    {
      if ( nsp_inv(A) == FAIL ) return FAIL;
      p = -p;
    }

  if ( p == 1 ) return OK;

  /* now use the power algorithm */
  if ( A->rc_type == 'r' )
    {
      double alpha=1.0,beta=0.0;
      double *temp = NULL, *oddmat = NULL;
      if ( (temp = nsp_alloc_doubles(A->mn)) == NULL ) return FAIL;
      while ( p > 1 )
	{
	  if ( p % 2 == 1 )
	    {
	      if ( ! oddflag )
		{
		  if ( (oddmat = nsp_alloc_doubles(A->mn)) == NULL ) { FREE(temp); return FAIL; }
		  oddflag = 1;
		  memcpy(oddmat, A->R, A->mn*sizeof(double));
		}
	      else
		{
		  C2F(dgemm)("N","N",&A->m,&A->m,&A->m,&alpha,A->R,&A->m,oddmat,&A->m,
			     &beta,temp,&A->m,1,1); 
		  memcpy(oddmat, temp, A->mn*sizeof(double));
		}
	    }
	  C2F(dgemm)("N","N",&A->m,&A->m,&A->m,&alpha,A->R,&A->m,A->R,&A->m,
		     &beta,temp,&A->m,1,1); 
	  memcpy(A->R, temp, A->mn*sizeof(double));
	  p = p/2;
	}
      if ( oddflag )
	{
	  C2F(dgemm)("N","N",&A->m,&A->m,&A->m,&alpha,A->R,&A->m,oddmat,&A->m,
		     &beta,temp,&A->m,1,1); 
	  memcpy(A->R, temp, A->mn*sizeof(double));
	  FREE(oddmat);
	}
      FREE(temp);
    }
  else  /* A is complex */
    {
      doubleC alpha={1.0,0.0}, beta={0.0,0.0};
      doubleC *temp = NULL, *oddmat = NULL;
      if ( (temp = nsp_alloc_doubleC(A->mn)) == NULL ) return FAIL;
      while ( p > 1 )
	{
	  if ( p % 2 == 1 )
	    {
	      if ( ! oddflag )
		{
		  if ( (oddmat = nsp_alloc_doubleC(A->mn)) == NULL ) { FREE(temp); return FAIL; }
		  oddflag = 1;
		  memcpy(oddmat, A->C, A->mn*sizeof(doubleC));
		}
	      else
		{
		  C2F(zgemm)("N","N",&A->m,&A->m,&A->m,&alpha,A->C,&A->m,oddmat,&A->m,
			     &beta,temp,&A->m,1,1); 
		  memcpy(oddmat, temp, A->mn*sizeof(doubleC));
		}
	    }
	  C2F(zgemm)("N","N",&A->m,&A->m,&A->m,&alpha,A->C,&A->m,A->C,&A->m,
		     &beta,temp,&A->m,1,1); 
	  memcpy(A->C, temp, A->mn*sizeof(doubleC));
	  p = p/2;
	}
      if ( oddflag )
	{
	  C2F(zgemm)("N","N",&A->m,&A->m,&A->m,&alpha,A->C,&A->m,oddmat,&A->m,
		     &beta,temp,&A->m,1,1); 
	  memcpy(A->C, temp, A->mn*sizeof(doubleC));
	  FREE(oddmat);
	}
      FREE(temp);
    }

  return OK;
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
  Scierror("Error:\t for ^ operator at least one operand must be a scalar\n");
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
  double a=0.0;
  doubleC ac, acc;
  char rc_flag;
  int i;
  NspIMatrix *C; /* used because A must not be modified : C will contain the scalar ln(a) */

  if ( B->m != B->n )
    {
      Scierror("Error:\t in scalar^M, the exponent M must be a square matrix\n");
      return FAIL;
    }

  if ( A->rc_type == 'r' )
    { a = A->R[0]; rc_flag = 'r';}
  else
    { ac = A->C[0]; rc_flag = 'c';}

  if ( (rc_flag == 'r' && a == 0.0) || (rc_flag == 'c' && ac.r == 0.0 && ac.i == 0.0) )  /* return nul matrix */
    {
      if ( B->rc_type == 'r' )
	for ( i = 0 ; i < B->mn ; i++ ) B->R[i] = 0.0;
      else
	for ( i = 0 ; i < B->mn ; i++ ) { B->C[i].r = 0.0; B->C[i].i = 0.0; }  /* a voir */
      return OK;
    }

  if ( rc_flag == 'r'  && a < 0.0 ) {ac.r = a; ac.i = 0.0; rc_flag = 'c';}

  if ( (C = nsp_imatrix_create(NVOID,rc_flag,1,1)) == NULLIMAT ) return FAIL;

  if ( rc_flag == 'c' )
    {
      acc = ac;
      nsp_log_c(&acc, &ac);
      C->C[0] = ac;
    }
  else
    {
      a = log(a);
      C->R[0] = a;
    }

  if ( nsp_imatrix_mult_scalar(B, C) == FAIL ) goto err;

  if ( nsp_expm(B) == FAIL ) goto err;

  nsp_imatrix_destroy(C); 
  return OK;

 err:
  nsp_imatrix_destroy(C); 
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

int nsp_imatrix_pow_el(NspIMatrix *A, NspIMatrix *B)
{
  if (SameDim(A,B))
    {
      int i;
      Boolean rflag = TRUE;
      for ( i = 0 ; i < A->mn ; i++ ) 
	{
	  if ( rflag )
	    if ( A->R[i] >= 0.0 )
	      A->R[i] = pow(A->R[i],B->R[i]);
	    else if ( floor(B->R[i]) == B->R[i] ) 
	      /* exposant is integer => result is still real */
	      A->R[i] = pow(A->R[i],B->R[i]);
	    else
	      {
		if (nsp_imatrix_complexify(A,0.00) == FAIL ) return FAIL;
		nsp_pow_cd(&A->C[i],B->R[i],&A->C[i]);
		rflag = FALSE;
	      }
	  else
	    nsp_pow_cd_or_ci(&A->C[i],B->R[i],&A->C[i]);
	}
      return OK;
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return FAIL;
    }
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
  if(A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  if ( B->R[0] == 2.0 )
	    for ( i = 0 ; i < A->mn ; i++ ) A->R[i] *= A->R[i];
	  else if ( B->R[0] == 3.0 )
	    for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = A->R[i]*A->R[i]*A->R[i];
	  else if ( B->R[0] == floor(B->R[0]) )  /* integer exponent */
	    {
	      if ( fabs(B->R[0]) <= 65536.0 ) 
		/* use power algorithm (2^16 = 65536 so less than 16 multiplications, */
		/* so the relative error is bounded by 16 epsm)                       */
		for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = nsp_pow_di(A->R[i], (int) B->R[0]);
	      else
		for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = pow(A->R[i], B->R[0]);
	    }
	  else   /* A.^p  with p real or p a too big integer */
	    {
	      Boolean rflag = TRUE;
	      for ( i = 0 ; i < A->mn ; i++ ) 
		{
		  if ( rflag )
		    if ( A->R[i] >= 0.0 )
		      A->R[i] = pow(A->R[i],B->R[0]);
		    else
		      {
			if (nsp_imatrix_complexify(A,0.00) == FAIL ) return FAIL;
			nsp_pow_cd(&A->C[i],B->R[0],&A->C[i]);
			rflag = FALSE;
		      }
		  else
		    nsp_pow_cd(&A->C[i],B->R[0],&A->C[i]);
		}
	    }
	}
    }
  return OK;
}

/**
 * nsp_imatrix_pow_scalarm:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix
 * 
 * @A = @B .^ @A when @B is a 1x1 matrix.
 * 
 * Return value: %OK or %FAIL.
 **/

int nsp_imatrix_pow_scalarm(NspIMatrix *A, NspIMatrix *B)
{
  int i;
  if(A->rc_type == 'r' ) 
    {
      if ( B->rc_type == 'r') 
	{
	  if ( B->R[0] >= 0.0 )
	    for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = pow(B->R[0],A->R[i]);
	  else
	    {
	      Boolean rflag = TRUE;
	      doubleC B_complexified = {1.0, 0.0};
	      for ( i = 0 ; i < A->mn ; i++ ) 
		{
		  if ( rflag )
		    if ( floor(A->R[i]) == A->R[i] ) /* exposant is integer => result is still real */
			A->R[i] = pow(B->R[0],A->R[i]);
		    else
		      {
			if (nsp_imatrix_complexify(A,0.00) == FAIL ) return FAIL;
			B_complexified.r = B->R[0];
			nsp_pow_cd(&B_complexified,A->C[i].r,&A->C[i]);
			rflag = FALSE;
		      }
		  else
		    nsp_pow_cd_or_ci(&B_complexified,A->C[i].r,&A->C[i]);
		}
	    }
	}
  return OK;
}

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
  if (SameDim(A,B))
    {
      int i;
      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] /= B->R[i];
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
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
  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] /= B->R[0];
  return(OK);
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
  if (SameDim(A,B))
    {
      int i;
      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = B->R[i]/A->R[i];
      return(OK);
    }
  else 
    {
      Scierror("Error:\tArguments must have the same size\n");
      return(FAIL);
    }
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
  for ( i = 0 ; i < A->mn ; i++ ) A->R[i] = B->R[0]/A->R[i];
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
  if (SameDim(A,B))
    {
      int i;
      for ( i = 0 ; i < A->mn ; i++ ) A->R[i] *= B->R[i];
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

      for ( j = 0 ; j < A->n ; j++)
	for ( i = 0 ; i < A->m ; i++, k++ ) 
	  A->R[k] = B->R[k]*coef->R[i];
      nsp_imatrix_destroy(coef);
      return OK;
    }
  else if ( A->n == B->n  &&  B->m == 1 )    /* A*diag(B) */
    {
      int i,j, k=0;
      for ( j = 0 ; j < A->n ; j++)
	for ( i = 0 ; i < A->m ; i++, k++ ) A->R[k] *= B->R[j];
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
  return MatOpScalar(Mat1,Mat2,nsp_dvmul,nsp_zvmul);
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

  if ( A->mn == 0 )
    return OK;
  for ( i = 0 ; i < A->mn ; i++ )
    A->R[i] *= B->R[0];
  return OK;
}



/**
 * nsp_imatrix_modulo:
 * @A: a #NspIMatrix 
 * @n: integer 
 * 
 * A= A mod(n) 
 **/

void nsp_imatrix_modulo(NspIMatrix *A, int n)
{
  int i ;
  nsp_imatrix_int(A);
  switch ( A->rc_type ) 
    {
    case 'r' : 
      for ( i=0 ; i < A->mn ; i++) 
	{
	  A->R[i]= ((int) aint(A->R[i])) % n ;
	}
      break ;
    case 'c' :
      for ( i=0 ; i < A->mn ; i++) 
	{
	  A->C[i].r = ((int) aint(A->C[i].r)) % n ;
	  A->C[i].i = ((int) aint(A->C[i].i)) % n ;
	}
    }
}


/**
 * nsp_imatrix_idiv:
 * @A: a #NspIMatrix 
 * @n: an integer 
 * 
 * 
 * A is changed to A / n :  quotient in int division
 **/

void nsp_imatrix_idiv(NspIMatrix *A, int n)
{
  int i ;
  nsp_imatrix_int(A);
  switch ( A->rc_type ) 
    {
    case 'r' : 
      for ( i=0 ; i < A->mn ; i++) 
	{
	  A->R[i]= ((int) aint(A->R[i])) / n ;
	}
      break ;
    case 'c' :
      for ( i=0 ; i < A->mn ; i++) 
	{
	  A->C[i].r = ((int) aint(A->C[i].r)) / n ;
	  A->C[i].i = ((int) aint(A->C[i].i)) / n ;
	}
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

  if ( x->mn == 1 && y->mn > 1 )
    for ( i = 0 ; i < y->mn ; i++ )
      y->R[i] = (y->R[i] != 0.0) ? x->R[0] - y->R[i]*(floor(x->R[0]/y->R[i])) : x->R[0];
  else if ( y->mn == 1 )
    {
      if ( y->R[0] != 0 )
	for ( i = 0 ; i < x->mn ; i++ )
	  x->R[i] -= y->R[0]*(floor(x->R[i]/y->R[0]));
    }
  else
    for ( i = 0 ; i < x->mn ; i++ )
      if ( y->R[i] != 0.0 )
	x->R[i] -= y->R[i]*(floor(x->R[i]/y->R[i]));
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
  switch ( A->rc_type ) 
    {
    case 'r' :
      for ( i=0 ; i < A->mn ; i++) 
	{
	  if ( A->R[i] > 0.0) 
	    A->R[i]=1.00;
	  else if ( A->R[i] < 0.0 )
	    A->R[i]=-1.00;
	}
      break;
    case  'c':
      for ( i=0 ; i < A->mn ; i++)nsp_signum_c(&A->C[i],&A->C[i]);
      break;
    }
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
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->mn ; i++) A->R[i]= fabs(A->R[i]);
    }
  else
    {
      for ( i = 0 ; i < A->mn ; i++) A->C[i].r=nsp_abs_c(&A->C[i]);
      if (nsp_imatrix_get_real(A) == FAIL) return(FAIL);
    }
  return(OK);
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
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) 
    {
      Scierror("iand: arguments should be real\n");
      return(FAIL);
    }
  for ( i = 0 ; i < A->mn ; i++) 
    A->R[i] = ((int) aint(A->R[i])) & ((int) aint(B->R[i]));
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

int nsp_imatrix_iandu(NspIMatrix *A, unsigned int *res)
{
  int i ;
  if ( A->rc_type == 'c') 
    {
      Scierror("iand: argument should be real\n");
      return(FAIL);
    }
  *res = (unsigned int) A->R[0];
  for ( i =1  ; i < A->mn ; i++) 
    *res = (*res) & ((unsigned int) A->R[i]);
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
  if ( A->rc_type == 'c' || B->rc_type == 'c' ) 
    {
      Scierror("ior: arguments should be real\n");
      return(FAIL);
    }
  for ( i = 0 ; i < A->mn ; i++) 
    A->R[i] = ((int) aint(A->R[i])) | ((int) aint(B->R[i]));
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
  if ( A->rc_type == 'c' )
    {
      Scierror("ishit: argument should be real\n");
      return(FAIL);
    }
  if ( dir == 'r' )
    for ( i = 0 ; i < A->mn ; i++) 
      A->R[i] = ((int) aint(A->R[i])) >> shift; 
  else 
    for ( i = 0 ; i < A->mn ; i++) 
      A->R[i] = ((int) aint(A->R[i])) << shift; 
  return(OK);
}

/**
 * nsp_imatrix_ioru:
 * @A: a #NspIMatrix 
 * @res: 
 * 
 * logical or of the entries of @A 
 * casted to int 
 * 
 * Return value: 
 **/

int nsp_imatrix_ioru(NspIMatrix *A, unsigned int *res)
{
  int i ;
  if ( A->rc_type == 'c') 
    {
      Scierror("ior: argument should be real\n");
      return(FAIL);
    }
  *res = (unsigned int) A->R[0];
  for ( i = 1 ; i < A->mn ; i++) 
    *res = (*res) | ((unsigned int) A->R[i]);
  return(OK);
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
  for ( i = 0 ; i < A->mn ; i++) A->R[i]= - A->R[i];
  return(OK);
}


/**
 * Kronecker:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * @PK: a #NspIMatrix 
 * 
 * Kronecker product of two Matrices 
 * PK is the result it must be created 
 * before calling this function size (AmxBm,AnxBn)
 * The rule to compute PK is the following 
 * PK[ i + j*B->m + k*(B->m*A->m) + p*(B->m*A->m*B->n)] = a(j,p)*b(i,k)
 * The i-loop leads to dcopy calls 
 * 
 **/

static void Kronecker(NspIMatrix *A, NspIMatrix *B, NspIMatrix *PK)
{
  static int c__1 = 1;
  double d0 = 0.00;
  int p,k,j,k0,k1,k2,k3;
  for ( p = 0 ; p < A->n ; p++)
    {
      k0 = p*(B->m*A->m*B->n);
      k3 = p*A->m;
      for ( k = 0 ; k < B->n ; k++) 
	{
	  k1= k*(B->m*A->m) + k0;
	  k2= k*(B->m);
	  for ( j = 0 ; j < A->m ; j++)
	    {
	      double *DR;
	      doubleC *DI;
	      if ( PK->rc_type == 'c' ) 
		{
		  DI = PK->C + j*B->m + k1;
		  if ( B->rc_type == 'c' ) 
		    {
		      /* C2F(zcopy)(&B->m, &B->C[k2], &c__1,DI, &c__1); **/
		      memcpy(DI, &B->C[k2],B->m*sizeof(doubleC));
		    }
		  else 
		    {
		      nsp_dzset(&B->m, &d0, DI, &c__1);
		    }
		  if ( A->rc_type == 'c' ) 
		    {
		      C2F(zscal)(&B->m,&A->C[j+k3],DI,&c__1);
		    }
		  else 
		    {
		      nsp_dzscal(&B->m,&A->R[j+k3],DI,&c__1);
		    }
		}
	      else
		{
		  /* if PK is real then A and B are real */
		  DR= PK->R + j*B->m + k1;
		  /* C2F(dcopy)(&B->m, &B->R[k2], &c__1,DR, &c__1); */
		  memcpy(DR, &B->R[k2],B->m*sizeof(double));
		  C2F(dscal)(&B->m, &A->R[j+k3],DR  , &c__1);
		}
	    }
	}
    }
}



/*
 * Comparison operators
 */

/* Operations **/

static int Lt(double a, double b) {  return(a<b);}
static int Le(double a, double b) {  return(a<=b);}
static int Eq(double a, double b) {  return(a==b);}
static int NEq(double a, double b) {  return(a!=b);}
static int Gt(double a, double b) {  return(a>b);}
static int Ge(double a, double b) {  return(a>=b);}

static int Eq_nan(double a, double b) { if (isnan(a) && isnan(b)) return TRUE; return(a==b);}
static int NEq_nan(double a, double b){ if (isnan(a) && isnan(b)) return FALSE;  return(a!=b);}

static int C_Lt(const doubleC *a,const  doubleC *b) {  return(nsp_abs_c(a)< nsp_abs_c(b));}
static int C_Le(const doubleC *a,const  doubleC *b) {  return(nsp_abs_c(a)<=nsp_abs_c(b));}
static int C_Eq(const doubleC *a,const  doubleC *b) {  return(a->r == b->r && a->i == b->i );}
static int C_NEq(const doubleC *a,const  doubleC *b) {  return(a->r != b->r || a->i != b->i );}
static int C_Gt(const doubleC *a,const  doubleC *b) {  return(nsp_abs_c(a)> nsp_abs_c(b));}
static int C_Ge(const doubleC *a,const  doubleC *b) {  return(nsp_abs_c(a)>=nsp_abs_c(b));}

static int C_Eq_nan(const doubleC *a,const  doubleC *b) {  return Eq_nan(a->r,b->r) && Eq_nan(a->i,b->i );};
static int C_NEq_nan(const doubleC *a,const  doubleC *b) { return NEq_nan(a->r,b->r) || NEq_nan(a->i,b->i );};

typedef int (CompOp) (double,double);
typedef int (C_CompOp) (const doubleC *,const  doubleC *);

typedef struct cpt {
  char *name;
  CompOp *fonc,*foncop;
  C_CompOp *C_fonc,*C_foncop;
} CompTab;

/* Warning : sorted tab **/ 

static CompTab comptab[] = {
  {"<",Lt  , Ge,C_Lt  , C_Ge},
  {"<=",Le  ,Gt,C_Le  ,C_Gt},
  {"<>",NEq ,Eq,C_NEq ,C_Eq},
  {"==",Eq  ,NEq,C_Eq  ,C_NEq},
  {"=nan=",Eq_nan ,NEq_nan , C_Eq_nan  ,C_NEq_nan },
  {">",Gt  , Le,C_Gt  , C_Le},
  {">=",Ge  , Lt,C_Ge  , C_Lt},
  {(char *) NULL, 0,0,0,0}
};

static int SearchComp(const char *op, CompOp (**realop), C_CompOp (**C_realop))
{
  int i=0;
  while ( comptab[i].name != (char *) NULL)
    {
      int j;
      j = strcmp(op,comptab[i].name);
      if ( j == 0 )
	{
	  *realop = comptab[i].fonc;
	  *C_realop = comptab[i].C_fonc;
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
 * @op: the code for the comparison as a string
 * 
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j). A and B must be size compatible with 
 * the standard promotion of scalars i.e 1x1 matrices. 
 * A and B are unchanged : Res is created 
 * 
 * Return value: a new #NspBMatrix
 **/

NspBMatrix  *nsp_imatrix_comp(NspIMatrix *A, NspIMatrix *B,const char *op)
{
  CompOp *realop;
  C_CompOp *C_realop;
  int i;
  NspBMatrix *Loc ;

  if ( A->rc_type == 'r'  &&  B->rc_type == 'r' )
    return nsp_imatrix_comp_real(A, B, op);

  if ( SearchComp(op,&realop,&C_realop) == FAIL) return(NULLBMAT);
  if ( !( A->m == B->m && A->n == B->n ) )
    {
      /* dimensions are not the same */
      if ( B->mn == 1 ) 
	{
	  /* Special case B is a 1x1 constant, size of result is controled by A 
	   * even the 0xn and nx0 cases 
	   */
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) { return(NULLBMAT);   }
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->R[i],B->R[0])==FALSE  ) Loc->B[i] = FALSE;
	  
	  return(Loc);
	}
      if ( A->mn == 1 )
	{
	  /* Special case A is a constant */
	  Loc =nsp_bmatrix_create(NVOID,B->m,B->n);
	  if ( Loc == NULLBMAT)     { return(NULLBMAT);  }
	  for ( i = 0 ; i < B->mn ; i++ )  
	    if ( (*realop)(A->R[0],B->R[i])==FALSE  ) Loc->B[i] = FALSE;
	  
	  return(Loc);
	}
      /* Incompatible dimensions: we return a boolean scalar as in Scilab 
       * this is not the matlab way.
       */
      if ( strcmp(op,"==") == 0) 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = FALSE;
	  return Loc;
	}
      else if ( strcmp(op,"<>") == 0) 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = TRUE ;
	  return Loc;
	}
      else 
	{
	  if ((Loc =nsp_bmatrix_create(NVOID,1,1))== NULLBMAT)return(NULLBMAT);
	  Loc->B[0] = FALSE;
	  return Loc;
	}
    }
  else 
    {
      /* A and B are of same dimensions */
      if ( A->mn == 0) 
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	}
      else
	{
	  Loc =nsp_bmatrix_create(NVOID,A->m,A->n);
	  if ( Loc == NULLBMAT) return(NULLBMAT);
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->R[i],B->R[i])==FALSE ) Loc->B[i] = FALSE;
	}
    }
  return(Loc);
}


#define MAKE_REAL_COMP(op)						\
  for ( i = 0, iA = 0, iB = 0 ; i < m*n ; i++, iA+=inc_A, iB+=inc_B )	\
    Loc->B[i] = A->R[iA] op B->R[iB] 

/**
 * nsp_imatrix_comp_real:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix 
 * @op: comparison operator as a string
 * 
 * returns in a #NspBMatrix the result of the comparison 
 * operation @A @op @B. op can be chosen among the following values 
 * "==", "<>", "<=", ">=", "<", ">". 
 * 
 * Return value: a new #NspBMatrix or %NULLBMAT
 **/

NspBMatrix  *nsp_imatrix_comp_real(NspIMatrix *A, NspIMatrix *B,const char *op)
{
  /* comparizon for both A and B of type real */
  int i, iA, iB, inc_A, inc_B, m, n;
  NspBMatrix *Loc ;
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
  else    /* A and B are of same dimensions */ 
    {
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
  
  if ( strcmp(op,"==") == 0 )
    MAKE_REAL_COMP(==);
  else if ( strcmp(op,"<>") == 0 )
    MAKE_REAL_COMP(!=);
  else if ( strcmp(op,"<=") == 0 )
    MAKE_REAL_COMP(<=);
  else if ( strcmp(op,">=") == 0 )
    MAKE_REAL_COMP(>=);
  else if ( strcmp(op,"<") == 0 )
    MAKE_REAL_COMP(<);
  else if ( strcmp(op,">") == 0 )
    MAKE_REAL_COMP(>);
  
  return Loc;
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

int nsp_imatrix_fullcomp(NspIMatrix *A, NspIMatrix *B, char *op,int *err)
{
  CompOp *realop;
  C_CompOp *C_realop;
  int i;
  int Loc=TRUE ;
  *err=FALSE;
  if ( SearchComp(op,&realop,&C_realop) == FAIL) { *err=TRUE; return FALSE;}
  if ( !( A->m == B->m && A->n == B->n ) )
    {
      if ( B->mn == 1 && A->mn != 0 ) 
	{
	  /* Special case B is a constant, Loc created with true */
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->R[i],B->R[0])==FALSE ) return FALSE;
	  return Loc;
	}
      if ( A->mn == 1 && B->mn != 0 ) 
	{
	  /* Special case A is a constant */
	  for ( i = 0 ; i < B->mn ; i++ )  
	    if ( (*realop)(A->R[0],B->R[i]) ==FALSE ) return FALSE;
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
	  if ( (*realop)(1.0,1.0) ==FALSE ) return  FALSE;
	}
      else
	{
	  for ( i = 0 ; i < A->mn ; i++ )  
	    if ( (*realop)(A->R[i],B->R[i]) ==FALSE ) return FALSE;
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

int nsp_imatrix_find(NspIMatrix *A, int lhs, NspIMatrix **Res1, NspIMatrix **Res2)
{
  int j,i,count=0; 
  int nrow = ( A->mn == 0) ? 0: 1;
  /* first pass for counting */
  if ( A->rc_type == 'r' ) 
    {
      for ( i=0 ; i < A->mn ; i++) 
	if ( A->R[i] != 0.0 ) count++;
    }
  else 
    {
      for ( i=0 ; i < A->mn ; i++) 
	if (A->C[i].r != 0.0 || A->C[i].i != 0.0) count++;
    }
  /* special rule for scalars */
  if ( A-> m == 1 && count ==0) nrow =0;
  if ( lhs == 1) 
    {
      *Res1 = nsp_imatrix_create(NVOID,'r',nrow,(int) count);
      if ( *Res1 == NULLIMAT) return FAIL;
      count=0;
      if ( A->rc_type == 'r' ) 
	{
	  for ( i = 0 ; i < A->mn ; i++ )
	    if ( A->R[i] != 0.0 ) (*Res1)->R[count++] = i+1;
	}
      else 
	{
	  for ( i = 0 ; i < A->mn ; i++ )
	    if ((A->C[i].r != 0.0 || A->C[i].i != 0.0))
	      (*Res1)->R[count++] = i+1;
	}
    }
  else 
    {
      int k;
      *Res1 = nsp_imatrix_create(NVOID,'r',nrow,(int) count);
      if ( *Res1 == NULLIMAT) return FAIL;
      *Res2 = nsp_imatrix_create(NVOID,'r',nrow,(int) count);
      if ( *Res2 == NULLIMAT) return FAIL;
      count=0;
      if ( A->rc_type == 'r' ) 
	{
	  for ( j = 0, k = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < A->m ; i++, k++ )
	      if (  A->R[k] != 0.0 )
		{
		  (*Res1)->R[count] = i+1;
		  (*Res2)->R[count++] = j+1;
		}
	}
      else 
	{
	  for ( j = 0, k = 0 ; j < A->n ; j++ )
	    for ( i = 0 ; i < A->m ; i++, k++ )
	      if (  A->C[k].r != 0.0 || A->C[k].i != 0.0 )
		{
		  (*Res1)->R[count] = i+1;
		  (*Res2)->R[count++] = j+1;
		}
	}
    }
  return OK;
}


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

int nsp_imatrix_mfind(const NspIMatrix *x, int m,const char **ops,const double *scalars, NspIMatrix **Ind)
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



/**
 * nsp_scalar_sub_imatrix_bis:
 * @A: a #NspIMatrix 
 * @B: a #NspIMatrix of size 1x1 (that is a scalar) 
 * 
 * Do the operation A <- B - A  when B is a scalar
 * and when nsp is compiled in MTLB_MODE (matlab
 * 's behavior for empty matrix)
 *
 * Return value: %FAIL or %OK
 **/

int nsp_scalar_sub_imatrix_bis(NspIMatrix *A, NspIMatrix *B) 
{
  int i;
  if ( A->mn == 0 )
    return OK;
  for ( i = 0 ; i < A->mn ; i++ ) 
    A->R[i] =  B->R[0] - A->R[i];
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
  if (SameDim(A,B))
    {
      for ( i = 0 ; i < A->mn ; i++ ) 
	A->R[i] -= B->R[i];
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
 * 
 *  for (i from 0 to m-1)  
 *      multiplie row i of A by x[i]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_scale_rows(NspIMatrix *A, NspIMatrix *x)
{
  int i,j, k;
  
  for ( j = 0, k=0 ; j < A->n ; j++)
    for ( i = 0 ; i < A->m ; i++, k++ ) 
      A->R[k] *= x->R[i];
  return OK;
}

/**
 * nsp_imatrix_scale_cols:
 * @A: a #NspIMatrix of size m x n
 * @x: a #NspIMatrix must be a vector of size n (1 x n or n x 1)
 * 
 *  for (j from 0 to n-1)  
 *      multiplie column j of A by x[j]
 * 
 * Return value: %FAIL or %OK
 **/

int nsp_imatrix_scale_cols(NspIMatrix *A, NspIMatrix *x)
{
  int i,j, k;
  
  for ( j = 0, k=0 ; j < A->n ; j++)
    for ( i = 0 ; i < A->m ; i++, k++ ) 
      A->R[k] *= x->R[j];
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
  for ( i = 0 ; i < A->mn ; i++ ) 
    if ( A->R[i] != 0.0) count++;
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

int nsp_imatrix_unique(NspIMatrix *x, NspIMatrix **Ind, NspIMatrix **Occ, Boolean first_ind)
{
  int i0, i, i_old, *index;
  NspIMatrix *ind=NULLIMAT, *occ=NULLIMAT;
  double val;

  if ( Ind == NULL )
    {
      if ( x->mn <= 1 ) return OK;
      nsp_qsort_double( x->R, NULL, 0, x->mn, 'i');
      i0 = 0; val = x->R[0];
      for ( i = 1 ; i < x->mn ; i++ )
	if ( x->R[i] != val )
	  {
	    i0++;
	    val = x->R[i0] = x->R[i];
	  }
      if ( x->m == 1 )
	nsp_imatrix_resize(x, 1, i0+1);
      else
	nsp_imatrix_resize(x, i0+1, 1);
      return OK;
    }

  else
    {
      if ( (ind = nsp_imatrix_create(NVOID,'r',x->m,x->n)) == NULLIMAT )
	return FAIL;
      index = (int *) ind->R;

      if ( Occ != NULL )
	if ( (occ = nsp_imatrix_create(NVOID,'r',x->m,x->n)) == NULLIMAT )
	  {
	    nsp_imatrix_destroy(ind); return FAIL;
	  }
      
      if ( x->mn > 0 )
	{
	  if ( first_ind )
	    nsp_sqsort_bp_double( x->R, x->mn, index, 'i');
	  else
	    nsp_qsort_double( x->R, index, 1, x->mn, 'i');
      
	  i0 = 0; val = x->R[0]; i_old = 0;
	  for ( i = 1 ; i < x->mn ; i++ )
	    {
	      if ( x->R[i] != val )
		{
		  if (Occ != NULL) { occ->R[i0] = i - i_old; i_old = i; }
		  i0++;
		  val = x->R[i0] = x->R[i];
		  index[i0] = index[i];
		}
	    }
	  if (Occ != NULL) occ->R[i0] = x->mn - i_old;
	  
	  if ( x->m == 1 )
	    {
	      nsp_imatrix_resize(x, 1, i0+1);
	      nsp_imatrix_resize(ind, 1, i0+1);
	      if ( Occ != NULL ) nsp_imatrix_resize(occ, 1, i0+1);
	    }
	  else
	    {
	      nsp_imatrix_resize(x, i0+1, 1);
	      nsp_imatrix_resize(ind, i0+1, 1);
	      if ( Occ != NULL ) nsp_imatrix_resize(occ, i0+1, 1);
	    }
	  ind->convert = 'i';
	  ind = Mat2double(ind);
	}

      *Ind = ind;
      if ( Occ != NULL ) *Occ = occ;
      return OK;
    }
}


/**
 * nsp_imatrix_dot:
 * @A, @B: (input) #NspIMatrix with same dimensions
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


/**
 * nsp_imatrix_cross:
 * @X, @Y: (input) #NspIMatrix with same dimensions (should be 3 x n or m x 3)
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

NspIMatrix *nsp_imatrix_cross(NspIMatrix *X, NspIMatrix *Y, int dim)
{
  NspIMatrix *Z;
  char type;
  int p;

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


  /* type (real or complexe) of the resulting matrix */
  type =  (X->rc_type == 'r' && Y->rc_type == 'r') ? 'r' : 'c'; 
  if ( (Z = nsp_imatrix_create(NVOID, type, X->m, X->n)) == NULLIMAT )
    return NULLIMAT;

  /* number of vectors pairs */
  p = dim == 1 ? X->n : X->m;

  if ( X->rc_type == 'r' )
    {
      if ( Y->rc_type == 'r' )
	nsp_dcross(X->R, Y->R, Z->R, p, dim);
      else
	{
	  int k;
	  doubleC *Xc;
	  if ( (Xc = nsp_alloc_work_doubleC(3*p) ) == NULL ) return NULLIMAT;
	  for ( k = 0 ; k < 3*p ; k++ ) { Xc[k].r = X->R[k]; Xc[k].i = 0.0; }
	  nsp_zcross(Xc, Y->C, Z->C, p, dim);
	  FREE(Xc);
	}
    }
  else
    {
      if ( Y->rc_type == 'r' )
	{
	  int k;
	  doubleC *Yc;
	  if ( (Yc = nsp_alloc_work_doubleC(3*p) ) == NULL ) return NULLIMAT;
	  for ( k = 0 ; k < 3*p ; k++ ) { Yc[k].r = Y->R[k]; Yc[k].i = 0.0; }
	  nsp_zcross(X->C, Yc, Z->C, p, dim);
	  FREE(Yc);
	}
      else
	nsp_zcross(X->C, Y->C, Z->C, p, dim);
    }

  return Z;
}

/**
 * nsp_imatrix_issorted:
 * @A: (input) #NspIMatrix
 * @dim_flag: (input) (0, 1 or 2)
 * @strict_order: (input) true for "<" and false for  "=<"
 *
 * Return value: TRUE or FALSE
 **/

NspBMatrix *nsp_imatrix_issorted(NspIMatrix *A, int dim_flag, Boolean strict_order)
{
  NspBMatrix *C;
  Boolean bool = TRUE;
  int i, j, k, c0, c1;

  if ((C = nsp_bmatrix_create(NVOID,Min(A->m,1),Min(A->n,1))) == NULLBMAT) 
    return NULLBMAT;
  
  if ( C->mn == 0 )
    return C;

  switch (dim_flag) 
    {
    default :
      Sciprintf("\nInvalid dim flag '%d' assuming dim=0\n", dim_flag);
      
    case 0: 
      if ( strict_order )
	{
	  for ( i = 1 ; i < A->mn && bool ; i++ )
	    bool = A->R[i-1] < A->R[i];
	}
      else
	{
	  for ( i = 1 ; i < A->mn && bool ; i++ )
	    bool = (A->R[i-1] <= A->R[i]);
	}
      C->B[0] = bool;
      break;

    case 1:  /* are rows sorted ? (in the lexicographic meaning) */
      for ( i = 1 ; i < A->m && bool ; i++ )
	for ( j = 0, k = i ; j < A->n ; j++, k+=A->m )
	  {
	    if ( A->R[k-1] < A->R[k] )
	      { bool = TRUE; break; }
	    else if ( A->R[k-1] == A->R[k] )
	      bool = TRUE;
	    else if ( A->R[k-1] > A->R[k] )
	      { bool = FALSE; break; }
	  }
      C->B[0] = bool;
      break;

    case 2: /* are columns sorted ? (in the lexicographic meaning) */
      for ( j = 1, c0 = 0, c1 = A->m ; j < A->n && bool ; j++, c0+=A->m, c1+=A->m )
	for ( i = 0  ; i < A->m ; i++)
	  {
	    if ( A->R[i+c0] < A->R[i+c1] )
	      { bool = TRUE; break; }
	    else if ( A->R[i+c0] == A->R[i+c1] )
	      bool = TRUE;
	    else if ( A->R[i+c0] > A->R[i+c1] )
	      { bool = FALSE; break; }
	  }
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
      Scierror("Error: arguments must have the same integre type\n");
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

#define IMAT_HAS(name)							\
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
      NSP_ITYPE_SWITCH(A->itype,IMAT_HAS);
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
