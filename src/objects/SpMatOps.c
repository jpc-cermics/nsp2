/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

/*
 * Sparse matrix continued 
 *
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "nsp/object.h"
#include "nsp/cnumeric.h"

#include <nsp/blas.h>
#include <nsp/matutil.h>

#define SameDim(Mat1,Mat2) ( Mat1->m == Mat2->m && Mat1->n == Mat2->n  )

/*
 * A = Matclean(a) clean A according to epsa and epsr 
 * epsa is used if rhs >= 1 
 * epsr is used if rhs >= 2
 * A is changed, 
 */

int nsp_spmatrix_clean(NspSpMatrix *A, int rhs, double epsa, double epsr)
{
  int j,i,n;
  double d_epsr=DBL_EPSILON;
  double d_epsa=DBL_EPSILON;
  double norm,eps;
  int inc=1;
  if ( A->rc_type == 'r') 
    {
      norm=0.0;
      for ( i = 0 ; i < A->m ; i++) 
	if ( A->D[i]->size != 0) 
	  norm += C2F(dasum)(&A->D[i]->size,A->D[i]->R,&inc);
    }
  else
    {
      norm=0.0;
      for ( i = 0 ; i < A->m ; i++) 
	if ( A->D[i]->size != 0) 
	  norm +=nsp_zasum(&A->D[i]->size,A->D[i]->C,&inc);
    }
  if ( rhs >= 1 ) d_epsa = epsa;
  if ( rhs >= 2 ) d_epsr = epsr;
  eps= Max(epsa,epsr*norm);
  for ( i = 0 ; i < A->m  ; i++ ) 
    {
      n =0;
      for ( j = 0 ; j < A->D[i]->size ; j++) 
	{
	  switch ( A->rc_type ) 
	    {
	    case 'r' : if ( Abs(A->D[i]->R[j])   < eps) A->D[i]->J[j] = -1;n=1; break ;
	    case 'c' : if (nsp_abs_c(&A->D[i]->C[j]) < eps) A->D[i]->J[j] = -1;n=1; break ;
	    }
	}
      /* remove null elements and resize rows **/
      if ( n != 0 ) 
	{
	  int ndel =nsp_spmatrix_compress_row(A,i);
	  if (nsp_spmatrix_resize_row(A,i, A->D[i]->size-ndel ) == FAIL) return(FAIL) ;
	}
    }
  return OK;
}

/*
 *  Res = Maxi(A,B) 
 *  term to term max  A(i;j) = Max(A(i,j),B(i,j)
 *  Res(i,j) = 1 or 2  
 *  A changed, B unchanged, 
 *  Res Created if flag == 1
 */

/*  minmaxflag = 1 for max -1 for min  */

NspMatrix *nsp_spmatrix_maximinitt_g(NspSpMatrix *A, NspSpMatrix *B, int flag, int minmaxflag, int *err)
{
  /* Same philosophy as in BinaryOp **/
  int i,count,k1,k2,k;
  NspSpMatrix *Loc;
  NspMatrix *Indi=NULL;
  char type = 'r';
  if ( SameDim(A,B) ) 
    {
      if ( A->rc_type == 'c' || B->rc_type == 'c' ) 
	{
	  Scierror("Error: Arguments must be real matrices\n");
	  return NULLMAT;
	}
      /* storing indices **/
      if ( flag == 1) 
	{
	  if (( Indi = nsp_matrix_create(NVOID,'r',A->m,A->n)) == NULLMAT ) 
	    return NULLMAT;
	  nsp_mat_set_rval(Indi,1.0);
	}
      /* Buffer **/
      if ((Loc =nsp_spmatrix_create(NVOID,type,1,A->n)) == NULLSP ) return(NULLMAT);
      if (nsp_spmatrix_resize_row(Loc,1,A->n ) == FAIL) return(NULLMAT) ;
      for ( i = 0 ; i < A->m ; i++ ) 
	{
	  SpRow *Ai = A->D[i];
	  SpRow *Bi = B->D[i];
	  /* We explore the ith line of A and B in increasing order of column 
	     and want to merge the columns found ( in increasing order ) 
	     when a same column number appear in both A and B we call the 
	     2-ary operator op 
	     This is very near to a merge sort of two sorted arrays 
	  **/ 
	  k1 = 0 ; k2 = 0 ; 
	  count = 0 ; 
	  /* merge part **/
	  while ( k1 < Ai->size && k2 <  Bi->size) 
	    { 
	      int j1,j2;
	      j1 = Ai->J[k1] ;
	      j2 = Bi->J[k2] ;
	      if ( j1 < j2 ) 
		{ 
		  /* A != 0 and B == 0 **/
		  if ( minmaxflag*Ai->R[k1] > 0 ) 
		    {
		      Loc->D[0]->J[count] = j1;
		      Loc->D[0]->R[count] = Ai->R[k1];
		      count++;
		    }
		  else 
		    {
		      if ( flag == 1) Indi->R[i+Indi->m*j1]=2;
		    }
		  k1++; 
		}
	      else if ( j1 == j2 ) 
		{ 
		  /* A != 0 and B != 0 **/
		  Loc->D[0]->J[count] = j1;
		  if (  minmaxflag*Ai->R[k1] >   minmaxflag*Bi->R[k2] )
		    {
		      Loc->D[0]->R[count]=  Ai->R[k1];
		    }
		  else
		    {
		      Loc->D[0]->R[count]=  Bi->R[k2];
		      if ( flag == 1) Indi->R[i+Indi->m*j1]=2;
		    }
		  count++;
		  k1++; k2 ++; 

		}
	      else 
		{ 
		  /* A == 0 and B != 0 **/
		  if (minmaxflag* Bi->R[k2] > 0 ) 
		    {
		      Loc->D[0]->J[count] = j2;
		      Loc->D[0]->R[count] = Bi->R[k2];
		      if ( flag == 1) Indi->R[i+Indi->m*j2]=2;
		      count++;
		    }
		  k2++;
		}
	    }
	  /* Keep inserting remaining arguments for A **/
	  for ( k = k1 ; k < Ai->size ; k++) 
	    { 
	      if ( minmaxflag*Ai->R[k] > 0 ) 
		{
		  Loc->D[0]->J[count] = Ai->J[k];
		  Loc->D[0]->R[count] = Ai->R[k];
		  count++;
		}
	    }
	  /* Keep inserting remaining arguments for B **/
	  for ( k = k2 ; k < Bi->size ; k++) 
	    { 
	      if ( minmaxflag* Bi->R[k] > 0 ) 
		{
		  Loc->D[0]->J[count] = Bi->J[k];
		  Loc->D[0]->R[count] = Bi->R[k];
		  if ( flag == 1) Indi->R[i+Indi->m*k]=2;
		  count++;
		}
	    }
	  /* count is not set to the proper ith row dimension  **/
	  /* we resize A(i,:) and store Loc  **/
	  if (nsp_spmatrix_resize_row(A,i,count)  == FAIL) return(NULLMAT) ;
	  /* use icopy and dcopy XXXX **/
	  for ( k =0 ; k < A->D[i]->size ; k++) 
	    {
	      A->D[i]->J[k] = Loc->D[0]->J[k];
	      A->D[i]->R[k] = Loc->D[0]->R[k];
	    }
						  
	}
      return(Indi);
    }
  else 
    {
      Scierror("Mat1 & Mat2 don't have same size \n");
      return(NULLMAT);
    }
}


NspMatrix *nsp_spmatrix_maxitt(NspSpMatrix *A, NspSpMatrix *B, int flag, int *err)
{
  return nsp_spmatrix_maximinitt_g(A,B,flag,1,err);
}

/*
 *  A(i,j) = Maxi(A(i,j),B(i,j)) 
 *  Ind(i,j) set to j if B(i,j) realize the max and flag ==1 
 *  B unchanged A,B are changed 
 */


/*
 *  Res = Mini(A,B) 
 *  term to term max  A(i;j) = Max(A(i,j),B(i,j)
 *  Res(i,j) = 1 or 2  
 *  A changed, B unchanged, 
 *  Res Created if flag == 1
 */

NspMatrix *nsp_spmatrix_minitt(NspSpMatrix *A, NspSpMatrix *B, int flag, int *err)
{
  return nsp_spmatrix_maximinitt_g(A,B,flag,-1,err);
}


/*
 *  A(i,j) = Mini(A(i,j),B(i,j)) 
 *  Ind(i,j) set to j if B(i,j) realize the max and flag ==1 
 *  B unchanged A,B are changed 
 */

/*
 * Return the Real part of Matrix A 
 * In a Real Matrix A
 * A is changed if a is not real 
 * A = real(A) 
 */

int nsp_spmatrix_realpart(NspSpMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r' )  return(OK);
  for ( i=0 ; i < A->m ; i++)
    {
      int count =0; 
      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	{
	  if ( A->D[i]->C[k].r != 0.0) count++;
	}
      if ( count != 0) 
	{
	  if ((A->D[i]->R =nsp_alloc_doubles((int) count)) == (double *) 0 ) 
	    {
	      Scierror("Error:\tRunning out of memory\n");
	      return(FAIL);
	    }
	  count =0;
	  for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	    {
	      if ( A->D[i]->C[k].r != 0.0) 
		{ 
		  A->D[i]->R[count] =A->D[i]->C[k].r; count++;
		}
	    }
	  FREE ( A->D[i]->C ) ;
	}
    }
  A->rc_type = 'r';
  return(OK);
}

/*
 * Return the Imaginary part of Matrix A 
 * In a Real Matrix A. A= Imag(A) 
 * A is changed  
 */

int nsp_spmatrix_imagpart(NspSpMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      for ( i=0 ; i < A->m ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	      FREE( A->D[i]->C);
	    }
	  A->D[i]->size =0;
	}
    }
  else
    {
      for ( i=0 ; i < A->m ; i++)
	{
	  int count =0; 
	  for ( k = 0 ; k < A->D[i]->size ; k++ ) 
	    {
	      if ( A->D[i]->C[k].i != 0.0) count++;
	    }
	  if ( count != 0) 
	    {
	      if ((A->D[i]->R =nsp_alloc_doubles((int) count)) == (double *) 0 ) 
		{
		  Scierror("Error:\tRunning out of memory\n");
		  return(FAIL);
		}
	      count =0;
	      for ( k = 0 ; k < A->D[i]->size ; k++ ) 
		{
		  if ( A->D[i]->C[k].i != 0.0) 
		    { 
		      A->D[i]->R[count] =A->D[i]->C[k].i; count++;
		    }
		}
	      FREE ( A->D[i]->C ) ;
	    }
	}
      A->rc_type = 'r';
    }
  return OK;
}

/*
 *nsp_mat_inv_el: a(i,j)=1/a(i,j) A est changee
 */

/*
 *nsp_mat_kron: produit de Kroeneker
 * A et B sont inchanges 
 */

/*
 *nsp_mat_sort: Index=Sort(A)
 * A is changed, Index created with the indexes 
 * return NULLMAT on error 
 * WARNING : A must be real but the test is not done here 
 * ======
 */

/*
 * Sum =nsp_mat_sum(A ,B])
 *     A is unchanged 
 * if B= 'c' the sum for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the sum for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the full sum is computed 
 */

NspSpMatrix *nsp_spmatrix_sum(NspSpMatrix *A, char *flag)
{
  double S;
  doubleC SC,C;
  NspSpMatrix *Sum=NULL;
  int i,k,count;
  int inc=1;
  if ( A->mn == 0) 
    {
      if ( flag[0] == 'F' || flag[0]=='f' )
	{
	  if ((Sum =nsp_spmatrix_create(NVOID,'r',1,1)) == NULLSP) return NULLSP;
	  if (nsp_spmatrix_resize_row(Sum,0,1)== FAIL) return NULLSP;
	  Sum->D[0]->J[0] =0;
	  Sum->D[0]->R[0] =0.00;
	  return Sum;
	}
      else 
	return nsp_spmatrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    case 'f': 
    case 'F':
  
      if ((Sum =nsp_spmatrix_create(NVOID,A->rc_type,1,1)) == NULLSP) return(NULLSP);
      switch ( A->rc_type) 
	{
	case 'r' : 
	  S=0;
	  for ( i= 0 ; i < A->m ; i++ ) 
	    S +=nsp_dsum(&A->D[i]->size,A->D[i]->R,&inc);
	  if ( S != 0) 
	    {
	      if (nsp_spmatrix_resize_row(Sum,0,1)== FAIL) return NULLSP;
	      Sum->D[0]->R[0] = S;
	      Sum->D[0]->J[0] = 0;
	    }
	  break;
	case 'c' :  
	  SC.r = SC.i = 0.0;
	  for ( i= 0 ; i < A->m ; i++ ) 
	    { 
	      nsp_zsum(&C,&A->D[i]->size,A->D[i]->C,&inc); 
	      SC.r += C.r;SC.i += C.i;
	    }
	  if ( SC.r  != 0.0 ||  SC.i != 0.0) 
	    {
	      if (nsp_spmatrix_resize_row(Sum,0,1)== FAIL) return NULLSP;
	      Sum->D[0]->C[0] = SC;
	      Sum->D[0]->J[0] = 0;
	    }
	  break;
	}
      break;
    case 'r':
    case 'R':
      
      if ((Sum =nsp_spmatrix_create(NVOID,A->rc_type,1,A->n)) == NULLSP) return NULLSP;
      if (nsp_spmatrix_resize_row(Sum,0,A->n)== FAIL) return NULLSP;
      for ( k=0 ; k < Sum->D[0]->size ; k++) 
	{
	  Sum->D[0]->J[k]=k;
	  switch ( A->rc_type ) 
	    {
	    case 'r' :  Sum->D[0]->R[k]=0.0;break;
	    case 'c' :  Sum->D[0]->C[k].r = Sum->D[0]->C[k].i =0.0;break;
	    }
	}
      for ( i = 0 ; i < A->m ; i++) 
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      switch ( A->rc_type ) 
		{
		case 'r' :  Sum->D[0]->R[A->D[i]->J[k]] += A->D[i]->R[k];break;
		case 'c' :  Sum->D[0]->C[A->D[i]->J[k]].r += A->D[i]->C[k].r ;
		  Sum->D[0]->C[A->D[i]->J[k]].i += A->D[i]->C[k].i ;break;
		}
	    }
	}
      count =0;
      switch ( A->rc_type ) 
	{
	case 'r' : 
	  for ( k=0 ; k < Sum->D[0]->size ; k++) 
	    {
	      if ( Sum->D[0]->R[k] == 0.0 ) { count=1; Sum->D[0]->J[k]=-1;}
	    }
	  break;
	case 'c' : 
	  for ( k=0 ; k < Sum->D[0]->size ; k++) 
	    {
	      if ( Sum->D[0]->C[k].r == 0.0 && Sum->D[0]->C[k].i == 0.0 ) 
		{ count=1; Sum->D[0]->J[k]=-1;}
	    }
	  break;
	}
      if ( count != 0 ) 
	{
	  int ndel =nsp_spmatrix_compress_row(Sum,0);
	  if (nsp_spmatrix_resize_row(Sum,0,A->D[0]->size-ndel ) == FAIL) return NULLSP;
	}
      break;
    case 'c':
    case 'C':
  
      if ((Sum =nsp_spmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSP) return NULLSP;
      switch ( A->rc_type) 
	{
	case 'r' : 
	  for ( i = 0 ; i < A->m ; i++) 
	    {
	      double S;
	      S =nsp_dsum(&A->D[i]->size,A->D[i]->R,&inc); 
	      if ( S != 0.0 ) 
		{
		  if (nsp_spmatrix_resize_row(Sum,i,1)== FAIL) return NULLSP;
		  Sum->D[i]->R[0] = S;
		  Sum->D[i]->J[0] = 0;
		}
	    }
	  break ;
	case 'c' :  
	  for ( i = 0 ; i < A->m ; i++) 
	    {
	      nsp_zsum(&C,&A->D[i]->size,A->D[i]->C,&inc); 
	      if ( C.r  != 0.0 || C.i != 0.0 ) 
		{
		  if (nsp_spmatrix_resize_row(Sum,i,1)== FAIL) return NULLSP;
		  Sum->D[i]->C[0] = C;
		  Sum->D[i]->J[0] = 0;
		}
	    }
	  break;
	}
      break;
    }
  return Sum;
}

/*
 * Prod =nsp_mat_prod(A ,B])
 *     A is unchanged 
 * if B= 'c' the prod for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the prod for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the full prod is computed 
 */

/*
 *nsp_mat_cum_prod: Cumulative Product of all elements of A
 * A is unchanged 
 */

/*
 *nsp_mat_cum_sum: Cumulative Sum of all elements of A
 * A is unchanged 
 */

/*
 * Max =nsp_mat_maxi(A,B,Imax,lhs)
 *     A is unchanged 
 * if B= 'c' the max for the column indices is computed 
 *       and a column vector is returned. 
 * if B= 'r' the max for the row indices is computed 
 *       and a Row vector is returned.
 * if B= 'f' the maximum 
 * Imax is created if lhs == 2 
 *    Note that Imax is a full matrix;
 */

typedef int (*SpMaMi1) (NspSpMatrix *A,NspSpMatrix *M);
typedef int (*SpMaMi2) (NspSpMatrix *A,int j,NspSpMatrix *M,int *count);
typedef int (*SpMaMi3) (NspSpMatrix *A,int j,NspSpMatrix *M);

static NspSpMatrix *SpMaxiMini(NspSpMatrix *A, char *flag, NspMatrix **Imax, int lhs, SpMaMi1 F1, SpMaMi2 F2, SpMaMi3 F3)
{
  NspSpMatrix *M=NULL;
  int j;
  int inc=1,imax,count;
  if ( A->mn == 0 ) 
    {
      if ( lhs == 2) *Imax = nsp_matrix_create(NVOID,'r',0,0);
      return nsp_spmatrix_create(NVOID,'r',0,0);
    }
  switch (flag[0]) 
    {
    case 'f': 
    case 'F':
      if ((M =nsp_spmatrix_create(NVOID,A->rc_type,1,1)) == NULLSP) return(NULLSP);
      imax = (*F1)(A,M);
      /* Check if M was properly resized **/
      if ( imax == 0 )  return NULLSP;
      
      if ( lhs == 2 ) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,1)) == NULLMAT)
	    return NULLSP; 
	  (*Imax)->R[0] = imax;
	}
      break;
    case 'r':
    case 'R':
      if ((M =nsp_spmatrix_create(NVOID,A->rc_type,1,A->n)) == NULLSP)
	return NULLSP;
      if (nsp_spmatrix_resize_row(M,0,A->n) == FAIL) return NULLSP;
      count =0;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',1,A->n)) == NULLMAT) 
	    return NULLSP;
	  for ( j= 0 ; j < A->n ; j++) 
	    {
	      (*Imax)->R[j]=(*F2)(A,j,M,&count); 
	    }
	}
      else 
	for ( j= 0 ; j < A->n ; j++) 
	  {
	    (*F2)(A,j,M,&count); 
	  }
      if (nsp_spmatrix_resize_row(M,0,count) == FAIL) return NULLSP;
      break ;
    case 'c':
    case 'C':
      if ((M =nsp_spmatrix_create(NVOID,A->rc_type,A->m,1)) == NULLSP) 
	return NULLSP;
      inc = A->m;
      if ( lhs == 2) 
	{
	  if ((*Imax = nsp_matrix_create(NVOID,'r',A->m,1)) == NULLMAT) 
	    return NULLSP; 
	  for ( j= 0 ; j < A->m ; j++) 
	    {
	      int imax =  (*F3)(A,j,M);
	      if ( imax == 0) return NULLSP;
	      (*Imax)->R[j] = imax;
	    }
	}
      else
	for ( j= 0 ; j < A->m ; j++) 
	  {
	    int imax =  (*F3)(A,j,M);
	    if ( imax == 0) return NULLSP;
	  }
      break;
    }
  return M;
}

/*M(1) = Maxi(A) **/

static int SpMaxi1(NspSpMatrix *A, NspSpMatrix *M)
{
  int imax = 0,i,k;
  double amax=0.0; imax=1;
  /* find a first value **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      if ( A->D[i]->size !=0 ) 
	{ amax = A->D[i]->R[0];imax = A->D[i]->J[0]+1; break;}
    }
  /* find the max  **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->R[k] > amax ) 
	    {
	      amax = A->D[i]->R[k];
	      imax =  A->D[i]->J[k]+1;
	    }
	}
    }
  if ( amax != 0.0 )
    {
      if (nsp_spmatrix_resize_row(M,0,1) == FAIL) return 0;
      M->D[0]->J[0]=0;
      M->D[0]->R[0]= amax;
    }
  return imax;
}

/*M(j)=Max A(:,j) **/

static int SpMaxi2(NspSpMatrix *A, int j, NspSpMatrix *M, int *count)
{
  int imax = 0,i,k;
  double amax=0.0; imax=1;
  /* find a first value **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) 
	    { amax = A->D[i]->R[k] ; imax = i+1; break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( amax != 0.0 ) break;
    }
  /* find the max **/
  for ( i = 0 ; i < A->m ; i++ ) 
    {
      int ok=-1;
      for ( k = 0 ; k < A->D[i]->size ; k++) 
	{
	  if ( A->D[i]->J[k] == j ) { ok=k;break;}
	  if ( j < A->D[i]->J[k] ) break;
	}
      if ( ok != -1 )
	{
	  if ( A->D[i]->R[ok] > amax ) 
	    {
	      amax = A->D[i]->R[k];
	      imax = i+1;
	    }
	}
    }
  if ( amax != 0.0 )
    {
      M->D[0]->J[*count]= j;
      M->D[0]->R[*count]= amax; (*count)++;
    }
  return imax;
}

/*M(j)=Max A(j,:) **/

static int SpMaxi3(NspSpMatrix *A, int j, NspSpMatrix *M)
{
  int imax = 0,k;
  double amax=0.0; imax=1;
  /* find a first value **/
  if ( A->D[j]->size != 0 ) { amax = A->D[j]->R[0] ; imax = A->D[j]->J[0]+1;}
  /* find the max **/
  for ( k = 0 ; k < A->D[j]->size ; k++) 
    {
      if ( A->D[j]->R[k]> amax ) 
	{
	  amax = A->D[j]->R[k];
	  imax = A->D[j]->J[k]+1;
	}
    }
  if ( amax != 0.0 )
    {
      if (nsp_spmatrix_resize_row(M,j,1) == FAIL) return 0;
      M->D[j]->J[0]= 0;
      M->D[j]->R[0]= amax;
    }
  return imax;
}


NspSpMatrix *nsp_spmatrix_maxi(NspSpMatrix *A, char *flag, NspMatrix **Imax, int lhs)
{
  return SpMaxiMini(A,flag,Imax,lhs,SpMaxi1,SpMaxi2,SpMaxi3);
}


/*
 *nsp_mat_mini: Mini(A)
 * A is unchanged 
 * rs and ri are set to the result 
 */

/*
 * Creates a Matrix and initialize it with the 
 * function func 
 * R=func(i,j) or R=func(i,j,&Imag) 
 */

/*
 *nsp_mat_triu: A=Triu(A)
 * A is changed  
 */

/*
 *nsp_mat_tril: A=Tril(A)
 * A is changed  
 */

/*
 *nsp_mat_eye: A=Eye(m,n)
 */

NspSpMatrix *nsp_spmatrix_eye(int m, int n)
{
  NspSpMatrix *Loc;
  int i;
  if (( Loc=nsp_spmatrix_create(NVOID,'r',m,n)) == NULLSP) return(NULLSP);
  for ( i = 0 ; i < Min(Loc->m,Loc->n) ; i++ ) 
    {
      if (nsp_spmatrix_resize_row(Loc,i,1)== FAIL) return NULLSP;
      Loc->D[i]->J[0]= i;
      Loc->D[i]->R[0]= 1.0;
    }
  return(Loc);
}

/*
 *nsp_mat_ones: A=ones(m,n)
 * A is changed  
 */

NspSpMatrix *nsp_spmatrix_ones(int m, int n)
{
  NspSpMatrix *Loc;
  int i,k;
  if (( Loc=nsp_spmatrix_create(NVOID,'r',m,n)) == NULLSP) return(NULLSP);
  for ( i = 0 ; i < Loc->m ; i++ ) 
    {
      if (nsp_spmatrix_resize_row(Loc,i,Loc->n)== FAIL) return NULLSP;
      for ( k = 0 ; k < Loc->n ; k++) 
	{
	  Loc->D[i]->J[k]= k;
	  Loc->D[i]->R[k]= 1.0;
	}
    }
  return(Loc);
}

/*
 *nsp_mat_ones: A=zeros(m,n)
 * A is changed  
 */

NspSpMatrix *nsp_spmatrix_zeros(int m, int n)
{
  NspSpMatrix *Loc;
  if (( Loc=nsp_spmatrix_create(NVOID,'r',m,n)) == NULLSP) return(NULLSP);
  return(Loc);
}

/*
 *nsp_mat_rand: A=rand(m,n)
 * A is changed  a 'ecrire ? 
 */

/*
  A Set of term to term function on Matrices (complex or real)
*/

/*
 *nsp_mat_pow_el(A,B) a(i,i)**b(i,i) 
 * A is changed  since 0.^0 --> 1 the returned matrix is full
 */

/*
 *nsp_mat_pow_scalar(A,B) a(i,i)**b
 * A is changed 
 */

/*
 * MatPowScalarMat(A,B) a(i,j)=b**a(i,j)
 * A is changed 
 */

/*
 *nsp_mat_div_el(A,B) a(i,i)/b(i,i) 
 * A is changed 
 */

/*
 *nsp_mat_div_scalar(A,B) a(i,i)/b
 * A is changed 
 */

/*
 *nsp_mat_bdiv_el(A,B) a(i,j) = a(i,j) \ b(i,j) 
 * A is changed 
 */

/*
 *nsp_mat_bdiv_scalar(A,B) a(i,j)= a(i,j) \ b
 * A is changed 
 */

/*
 * A=nsp_mat_mult_el(A,B) a(i,i).*b(i,i) 
 * A is changed 
 */

/*
 * A=Acos(A), 
 * A is changed 
 */

typedef double (*Func1) (double);
typedef void   (*Func2) (const doubleC *, doubleC *);


static NspMatrix* SpUnary2Full(NspSpMatrix *A, Func1 F1, Func2 F2)
{
  double val ;
  doubleC Cval,Czero={0.0,0.0};
  int i,j,k;
  NspMatrix *Loc;
  if ((Loc = nsp_matrix_create(NVOID,A->rc_type,A->m,A->n))== NULLMAT) return(NULLMAT);
  switch ( A->rc_type )
    {
    case 'r' : val = (*F1)(0.00); nsp_mat_set_rval(Loc,val); break;
    case 'c' : (*F2)(&Czero,&Cval);
      nsp_mat_set_rval(Loc,Cval.r);
      nsp_mat_set_ival(Loc,Cval.i);
      break;
    }
  if ( A->rc_type == 'r' )
    {
      for ( i = 0 ; i < A->m ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    Loc->R[i+Loc->m*j] = (*F1)( A->D[i]->R[k]);
	  }
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++ ) 
	for ( k = 0 ; k < A->D[i]->size ; k++) 
	  {
	    j= A->D[i]->J[k];
	    (*F2)(&A->D[i]->C[k],&Loc->C[i+Loc->m*j]);
	  }
    }
  return Loc;
}

NspMatrix *nsp_spmatrix_acos(NspSpMatrix *A)
{
  return SpUnary2Full(A,acosh,nsp_acosh_c);
}

/*
 * A=Acosh(A), 
 * A is changed 
 */

NspMatrix *nsp_spmatrix_acosh(NspSpMatrix *A)
{
  return SpUnary2Full(A,acos,nsp_acos_c);
}


/*
 * Generic Function for Sparse unary operators 
 * computes A=f1(A) or A=f2(A) assuming fi(0)=0
 */

static void  SpUnary(NspSpMatrix *A, Func1 F1, Func2 F2)
{
  int i,k,compress,ndel;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	{
	  compress=0;
	  for ( k=0; k < A->D[i]->size ; k++ ) 
	    {
	      A->D[i]->R[k] = (*F1)(A->D[i]->R[k]);
	      if ( A->D[i]->R[k] == 0.0) 
		{
		  compress=1; A->D[i]->J[k]=-1;
		}
	    }
	  if ( compress == 1) 
	    {
	      ndel =nsp_spmatrix_compress_row(A,i);
	      nsp_spmatrix_resize_row(A,i,A->D[i]->size-ndel);
	    }
	}
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	{
	  compress=0;
	  for ( k=0; k < A->D[i]->size ; k++ ) 
	    {
	      (*F2)(&A->D[i]->C[k],&A->D[i]->C[k]);
	      if ( A->D[i]->C[k].r == 0.0 &&  A->D[i]->C[k].i == 0.0 )
		{
		  compress=1;
		  A->D[i]->J[k]=-1;
		}
	    }
	  if ( compress == 1) 
	    {
	      ndel =nsp_spmatrix_compress_row(A,i);
	      nsp_spmatrix_resize_row(A,i,A->D[i]->size-ndel);
	    }
	}
    }
}


/*
 * A=Asin(A), 
 * A is changed 
 */


void nsp_spmatrix_asin(NspSpMatrix *A)
{
  SpUnary(A,asin,nsp_asin_c);
}

/*
 * A=Asinh(A),
 * A is changed 
 */

void nsp_spmatrix_asinh(NspSpMatrix *A)
{
  SpUnary(A,asinh,nsp_asinh_c);
}

/*
 * A=Atan(A),  * A is changed 
 */

void nsp_spmatrix_atan(NspSpMatrix *A)
{
  SpUnary(A,atan,nsp_atan_c);
}

/*
 * A=Atanh(A),
 * A is changed 
 */

void nsp_spmatrix_atanh(NspSpMatrix *A)
{
  SpUnary(A,atanh,nsp_atanh_c);
}


/*
 *nsp_mat_ceil: A=Ceil(A)
 * A is changed  
 */

void nsp_spmatrix_ceil(NspSpMatrix *A)
{
  SpUnary(A,ceil,nsp_ceil_c);
}

/*
 *nsp_mat_int: A=Int(A)
 * A is changed  
 */

static double R_aint(double x) { return aint(x);} 

void nsp_spmatrix_int(NspSpMatrix *A)
{
  SpUnary(A,R_aint,nsp_aint_c);
}

/*
 *nsp_mat_floor: A=Floor(A)
 * A is changed  
 */

void nsp_spmatrix_floor(NspSpMatrix *A)
{
  SpUnary(A,floor,nsp_floor_c);
}

/*
 *nsp_mat_round: A=Round(A)
 * A is changed  
 */

static double R_anint(double x) { return anint(x);} 

void nsp_spmatrix_round(NspSpMatrix *A)
{
  SpUnary(A,R_anint,nsp_round_c);
}

/*
 *nsp_mat_sign: A=Sign(A)
 * A is changed  
 * return 0 if error 
 */

int nsp_spmatrix_sign(NspSpMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  {
	    if (A->D[i]->R[k] > 0.0) 
	      A->D[i]->R[k] = 1.00;
	    else if (A->D[i]->R[k] < 0.0) 
	      A->D[i]->R[k] = -1.00;
	  }
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  nsp_signum_c(&A->D[i]->C[k],&A->D[i]->C[k]);
    }
  return(OK);
}

/*
 * A=Tan(A), absolue value or module of each element 
 * A is changed 
 */

void nsp_spmatrix_tan(NspSpMatrix *A)
{
  SpUnary(A,tan,nsp_tan_c);
}

/*
 * A=Tanh(A), absolue value or module of each element 
 * A is changed 
 */

void nsp_spmatrix_tanh(NspSpMatrix *A)
{
  SpUnary(A,tanh,nsp_tanh_c);
}

/*
 * A=Abs(A), absolue value or module of each element 
 * A is changed 
 */

int nsp_spmatrix_abs(NspSpMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] = fabs(A->D[i]->R[k]);
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_abs_c(&A->D[i]->C[k]);
      if (nsp_spmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}

/*
 * A=Erf(A), Erf function 
 */

int nsp_spmatrix_erf(NspSpMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->R[k] = erf(A->D[i]->R[k]);
    }
  else
    {
      Scierror("Error:\t erf function argument must be real\n");
      return(FAIL);
    }
  return(OK);
}

/*
 * A=Erfc(A), Erf function 
 */

/*
  int SpErfc(A)
  NspSpMatrix *A;
  {
  int i ;
  if ( A->rc_type == 'r') 
  {
  for ( i = 0 ; i < A->mn ; i++) A->R[i]= erfc(A->R[i]);
  }
  else
  {
  Scierror("Erf function argument must be real\n");
  return(FAIL);
  }
  return(OK);
  }
**/

/*
 * A=Arg(A),
 * Argument or Phase 
 */

int nsp_spmatrix_arg(NspSpMatrix *A)
{
  int i,k ;
  if ( A->rc_type == 'r') 
    {
      /* change a to  [] sparse **/
      for ( i=0 ; i < A->m ; i++ ) 
	{
	  if ( A->D[i]->size != 0 ) 
	    {
	      FREE( A->D[i]->J);
	      FREE( A->D[i]->R);
	      FREE( A->D[i]->C);
	    }
	  A->D[i]->size =0;
	}
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k=0; k < A->D[i]->size ; k++ ) 
	  A->D[i]->C[k].r =nsp_arg_c(&A->D[i]->C[k]);
      if (nsp_spmatrix_realpart(A) == FAIL) return FAIL;
    }
  return(OK);
}

/*
 * A=Polar(A,B),
 * A=A(cos(B)+%i*sin(B);
 */

/*
 *nsp_mat_conj: A=real(A)-i*Imag(A)
 * A is changed  only if imaginary 
 */

void nsp_spmatrix_conj(NspSpMatrix *A)
{
  int i,k;
  switch ( A->rc_type ) 
    {
    case 'r' : break;
    case 'c' : 
      for ( i = 0 ; i < A->m ; i++)
	{
	  for ( k= 0 ; k < A->D[i]->size; k++ ) 
	    A->D[i]->C[k].i = - A->D[i]->C[k].i;
	}
      break;
    }
}

/*
 *nsp_mat_cos: A=Cos(A)
 * A is changed  
 * return 0 if error 
 */


NspMatrix *nsp_spmatrix_cos(NspSpMatrix *A)
{
  return SpUnary2Full(A,cos,nsp_cos_c);
}

/*
 *nsp_mat_cosh: A=Cosh(A)
 * A is changed  
 * return 0 if error 
 */

NspMatrix *nsp_spmatrix_cosh(NspSpMatrix *A)
{
  return SpUnary2Full(A,cosh,nsp_cosh_c);
}

/*
 * MatExpl : Exponentiation term to term 
 * A is changed 
 */

NspMatrix *nsp_spmatrix_expel(NspSpMatrix *A)
{
  return SpUnary2Full(A,exp,nsp_exp_c);
}

/*
 *nsp_mat_logel: A=LogEl(A)  log term to term 
 * A is changed  
 * The real case is special since the result can be complex
 */

int nsp_spmatrix_logel(NspSpMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->m ; i++) 
	for ( k=0 ; k < A->D[i]->size ; k++) 
	  if ( A->D[i]->R[k] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
      if ( itr == 0) 
	{
	  /* real case sqrt(A) is real  */
	  SpUnary(A,log,nsp_log_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_spmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpUnary(A,log,nsp_log_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpUnary(A,log,nsp_log_c);
  return OK;
}

/*
 *nsp_mat_sin: A=Sin(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_spmatrix_sin(NspSpMatrix *A)
{
  SpUnary(A,sin,nsp_sin_c);
}


/*
 *nsp_mat_sinh: A=Sinh(A)
 * A is changed  
 * return 0 if error 
 */

void nsp_spmatrix_sinh(NspSpMatrix *A)
{
  SpUnary(A,sinh,nsp_sinh_c);
}

/*
 *nsp_mat_sqrtel: A=SqrtEl(A)  term to term square root
 * A is changed  
 * return 0 if error 
 * The real case is special since the result can be complex
 */

int nsp_spmatrix_sqrtel(NspSpMatrix *A)
{
  int i,k;
  if ( A->rc_type == 'r')
    {
      /* Check if really real or imaginary case */
      int itr = 0;
      for ( i=0 ; i < A->m ; i++) 
	for ( k=0 ; k < A->D[i]->size ; k++) 
	  if ( A->D[i]->R[k] < 0.00 ) 
	    {
	      itr = 1; break;
	    }
      if ( itr == 0) 
	{
	  /* real case sqrt(A) is real  */
	  SpUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
      else 
	{
	  /* result is complex  */
	  if (nsp_spmatrix_seti(A,0.00) == FAIL ) return FAIL;
	  SpUnary(A,sqrt,nsp_sqrt_c);
	  return OK;
	}
    }
  /* A is complex and sqrt(A) too **/
  SpUnary(A,sqrt,nsp_sqrt_c);
  return OK;
}

/*
 *nsp_mat_minus(A),  A= -A 
 * A is changed 
 */

int nsp_spmatrix_minus(NspSpMatrix *A)
{
  int i,k ;
  if ( A->rc_type  == 'r') 
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k = 0 ; k < A->D[i]->size ; k++)
	  {
	    A->D[i]->R[k] = - A->D[i]->R[k];
	  }
    }
  else
    {
      for ( i = 0 ; i < A->m ; i++)
	for ( k = 0 ; k < A->D[i]->size ; k++)
	  {
	    A->D[i]->C[k].r = - A->D[i]->C[k].r;
	    A->D[i]->C[k].i = - A->D[i]->C[k].i;
	  }
    }
  return(OK);
}


/*
 * Kronecker product of two Matrices 
 * PK is the result it must be created 
 * before calling this function size (AmxBm,AnxBn)
 * The rule to compute PK is the following 
 * PK[ i + j*B->m + k*(B->m*A->m) + p*(B->m*A->m*B->n)] = a(j,p)*b(i,k)
 * The i-loop leads to dcopy calls 
 */

/*
 *nsp_mat_magic: A=Magic(n)
 */

/*
 *nsp_mat_franck: A=Franck(n)
 */

/*
 *nsp_mat_hilbert: A=Hilbert(n)
 */

/*
 * Comparison operators
 */

/*
 * Operation on Matrices leading to Boolean Matrices results 
 * Res = A(i,j) op B(i;j) 
 * with the special case : 
 *      A(i;j)op B(0,0) or A(0,0) op B(i,j) if A or B are of size 1x1
 *      
 * A and B are unchanged : Res is created 
 */

/*
 * returns in a Matrix the indices for which the 
 * Matrix A has non zero entries 
 * A is left unchanged
 * according to lhs one or two arguments are returned 
 */

int nsp_spmatrix_find(NspSpMatrix *A, int lhs, NspMatrix **Res1, NspMatrix **Res2)
{
  int k,i,count=0;
  /* first pass for counting **/
  for ( i=0 ; i < A->m ; i++) 
    {
      count += A->D[i]->size ;
    }
  if ( lhs == 1) 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      count=0;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    (*Res1)->R[count++] = A->D[i]->J[k]*A->m + i + 1;
	}
    }
  else 
    {
      *Res1 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res1 == NULLMAT) return FAIL;
      *Res2 = nsp_matrix_create(NVOID,'r',(int) 1,(int) count);
      if ( *Res2 == NULLMAT) return FAIL;
      count=0;
      for ( i = 0 ; i < A->m ; i++ )
	{
	  for ( k = 0 ; k < A->D[i]->size ; k++) 
	    {
	      (*Res1)->R[count]   = i + 1;
	      (*Res2)->R[count++] = A->D[i]->J[k] + 1;
	    }
	}
    }
  return OK;
}


